//// A pure-Gleam YAML 1.2 parser. Passes 351/351 of the official YAML test
//// suite. Works on both Erlang and JavaScript targets; an optional native
//// backend (`taffy/native`, Erlang only) wraps `fast_yaml` for ~3-7×
//// speedup on large documents.
////
//// ## Quick start
////
//// ```gleam
//// import taffy
////
//// pub fn main() {
////   let assert Ok(value) =
////     taffy.parse("name: John\nage: 30\ntags:\n  - gleam\n  - erlang")
////
////   let assert Ok(name) = taffy.get(value, "name")
////   let json = taffy.to_json_string(value)
//// }
//// ```
////
//// ## Output
////
//// - `to_json` / `to_json_string` — convert to `gleam_json` values
//// - `to_yaml` — emit block-style YAML, round-trips through `parse`
//// - `validate_unique_keys` — opt-in YAML 1.2 strict duplicate-key check

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import taffy/lexer
import taffy/parser
import taffy/parser/types.{type ParseError, ParseError}
import taffy/value.{type YamlValue}

pub type Value =
  YamlValue

pub type Error =
  ParseError

/// Per-parse safety knobs. Construct via `default_options()` and override
/// fields, e.g. `Options(..taffy.default_options(), max_depth: 64)`.
///
/// - `alias_budget` — node-count ceiling for alias expansion. Defends
///   against billion-laughs / alias-bomb constructions. Default 10M.
/// - `max_depth` — maximum recursive value-parsing depth. Defends against
///   deeply nested block input. Default 1024.
pub type Options {
  Options(alias_budget: Int, max_depth: Int)
}

pub fn default_options() -> Options {
  Options(
    alias_budget: types.default_alias_budget,
    max_depth: types.default_max_depth,
  )
}

/// Compute `#(line, column)` (both 1-indexed) for the byte position
/// `pos` inside `input`. Out-of-range positions clamp to the last line/column.
/// Useful for turning `error.pos` into a user-facing location:
///
/// ```gleam
/// case taffy.parse(input) {
///   Ok(_) -> ...
///   Error(err) -> {
///     let #(line, col) = taffy.error_location(input, err.pos)
///     io.println(err.message <> " at " <> int.to_string(line) <>
///       ":" <> int.to_string(col))
///   }
/// }
/// ```
pub fn error_location(input: String, pos: Int) -> #(Int, Int) {
  error_location_loop(string.to_graphemes(input), pos, 1, 1)
}

fn error_location_loop(
  graphemes: List(String),
  remaining: Int,
  line: Int,
  col: Int,
) -> #(Int, Int) {
  case graphemes, remaining {
    _, n if n <= 0 -> #(line, col)
    [], _ -> #(line, col)
    ["\n", ..rest], _ -> error_location_loop(rest, remaining - 1, line + 1, 1)
    [_, ..rest], _ -> error_location_loop(rest, remaining - 1, line, col + 1)
  }
}

/// Parse a single YAML document. For multi-document streams use `parse_all`.
/// On error the position is the byte offset where parsing gave up; feed it
/// through `error_location` to recover `#(line, column)`.
/// `<<` merge keys are resolved automatically. Duplicate mapping keys are
/// allowed (with first-wins access) — the YAML 1.2 spec mandates uniqueness
/// but the official test suite expects parsers to accept them; use
/// `validate_unique_keys` to enforce strict uniqueness yourself.
pub fn parse(input: String) -> Result(Value, Error) {
  case lexer.tokenize(input) {
    Error(#(msg, pos)) -> Error(ParseError(msg, pos))
    Ok(tokens) -> parser.parse(tokens) |> result.map(value.resolve_merges)
  }
}

/// Parse a YAML stream as a list of documents. Empty input yields `[]`.
/// Same merge-key + duplicate semantics as `parse`.
pub fn parse_all(input: String) -> Result(List(Value), Error) {
  case lexer.tokenize(input) {
    Error(#(msg, pos)) -> Error(ParseError(msg, pos))
    Ok(tokens) ->
      parser.parse_all(tokens) |> result.map(list.map(_, value.resolve_merges))
  }
}

/// Like `parse` but with custom safety limits. Useful when accepting YAML
/// from untrusted callers and the defaults are too generous (or too tight).
pub fn parse_with_options(
  input: String,
  options: Options,
) -> Result(Value, Error) {
  case lexer.tokenize(input) {
    Error(#(msg, pos)) -> Error(ParseError(msg, pos))
    Ok(tokens) ->
      parser.parse_with(tokens, options.alias_budget, options.max_depth)
      |> result.map(value.resolve_merges)
  }
}

/// Like `parse_all` but with custom safety limits.
pub fn parse_all_with_options(
  input: String,
  options: Options,
) -> Result(List(Value), Error) {
  case lexer.tokenize(input) {
    Error(#(msg, pos)) -> Error(ParseError(msg, pos))
    Ok(tokens) ->
      parser.parse_all_with(tokens, options.alias_budget, options.max_depth)
      |> result.map(list.map(_, value.resolve_merges))
  }
}

/// Reject any mapping (recursively) that contains duplicate keys. The error
/// message names the first offending key. Run after `parse` if your
/// application requires YAML 1.2's key-uniqueness invariant.
pub fn validate_unique_keys(val: Value) -> Result(Value, Error) {
  case value.check_no_duplicates(val) {
    Ok(Nil) -> Ok(val)
    Error(key) -> Error(ParseError("Duplicate mapping key: " <> key, 0))
  }
}

/// Look up a key in a mapping. Returns `Error(Nil)` when `val` is not a
/// mapping or the key is absent.
pub fn get(val: Value, key: String) -> Result(Value, Nil) {
  value.get(val, key) |> option.to_result(Nil)
}

/// Like `get`, but returns `default` instead of an error when the key is
/// missing or `val` is not a mapping.
pub fn get_or(val: Value, key: String, default: Value) -> Value {
  case value.get(val, key) {
    option.Some(v) -> v
    option.None -> default
  }
}

/// Walk a chain of keys through nested mappings. An empty path returns
/// `val` unchanged; any missing key short-circuits to `Error(Nil)`.
pub fn get_path(val: Value, path: List(String)) -> Result(Value, Nil) {
  case path {
    [] -> Ok(val)
    [key, ..rest] -> {
      case value.get(val, key) {
        option.Some(v) -> get_path(v, rest)
        option.None -> Error(Nil)
      }
    }
  }
}

/// Get the `idx`-th item of a sequence (0-indexed). Returns `Error(Nil)` if
/// `val` is not a sequence or `idx` is out of range.
pub fn index(val: Value, idx: Int) -> Result(Value, Nil) {
  value.index(val, idx) |> option.to_result(Nil)
}

/// Pull out a string scalar; `None` for any other variant.
pub fn as_string(val: Value) -> Option(String) {
  value.as_string(val)
}

/// Pull out an int scalar; `None` for any other variant (no float coercion).
pub fn as_int(val: Value) -> Option(Int) {
  value.as_int(val)
}

/// Pull out a float scalar. Coerces `Int(n)` to `Float(n)` so numeric callers
/// don't have to branch on the parse-time tag.
pub fn as_float(val: Value) -> Option(Float) {
  value.as_float(val)
}

/// Pull out a bool scalar; `None` for any other variant.
pub fn as_bool(val: Value) -> Option(Bool) {
  value.as_bool(val)
}

/// Pull out a sequence as a list. Order is preserved.
pub fn as_list(val: Value) -> Option(List(Value)) {
  value.as_list(val)
}

/// Pull out a mapping as a `Dict`. Note: this loses YAML's insertion order;
/// use `as_pairs` if order matters.
pub fn as_dict(val: Value) -> Option(Dict(String, Value)) {
  value.as_dict(val)
}

/// Pull out a mapping as ordered `#(key, value)` pairs, preserving YAML
/// insertion order.
pub fn as_pairs(val: Value) -> Option(List(#(String, Value))) {
  value.as_pairs(val)
}

/// True only for `Null`. Bool/Int/zero values all return False.
pub fn is_null(val: Value) -> Bool {
  value.is_null(val)
}

/// Render a value as a JSON-shaped debug string. Useful for snapshot tests;
/// for round-trippable YAML output use `to_yaml`.
pub fn to_string(val: Value) -> String {
  value.to_string(val)
}

/// Emit a value as block-style YAML with a trailing newline. The output
/// round-trips through `parse` for all values that are themselves
/// round-trippable (taffy is lossy on tag, anchor, and comment metadata).
pub fn to_yaml(val: Value) -> String {
  value.to_yaml(val)
}

/// Convert a parsed YAML value into a `gleam_json` `Json`. Whole-valued
/// floats serialize as ints to round-trip nicely with the YAML test suite.
pub fn to_json(val: Value) -> Json {
  case val {
    value.Null -> json.null()
    value.Bool(b) -> json.bool(b)
    value.Int(i) -> json.int(i)
    value.Float(f) -> {
      let truncated = float.truncate(f)
      case f == int.to_float(truncated) {
        True -> json.int(truncated)
        False -> json.float(f)
      }
    }
    value.String(s) -> json.string(s)
    value.Sequence(items) -> json.array(items, to_json)
    value.Mapping(pairs) ->
      json.object(
        pairs
        |> list.map(fn(pair) { #(pair.0, to_json(pair.1)) }),
      )
  }
}

/// Convenience wrapper around `to_json` + `json.to_string`.
pub fn to_json_string(val: Value) -> String {
  to_json(val) |> json.to_string
}
