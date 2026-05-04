//// Native YAML parsing backend wrapping the `fast_yaml` C NIF (libyaml).
//// Erlang target only — requires `fast_yaml` in your dependencies.
//// Output is the same `YamlValue` shape as the pure-Gleam parser but
//// ~3-7× faster on large documents.
////
//// ```gleam
//// import taffy/native
////
//// let assert Ok(value) = native.parse("name: John\nage: 30")
//// ```
////
//// One known divergence from the pure parser: `fast_yaml` represents both
//// `{}` and `[]` as the empty Erlang list at the term level, so the
//// distinction can't be recovered from the decoded value alone. Taffy
//// resolves the ambiguity in favour of `Mapping([])` since `{}` is the
//// more common author intent; users who need `Sequence([])` for `[]`
//// should use the pure parser.

import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import taffy/parser/scalar
import taffy/parser/types.{type ParseError, ParseError}
import taffy/value.{type YamlValue}

/// Parse a single document via `fast_yaml`. The error type matches the
/// pure parser's so callers can swap backends without changing their
/// error-handling. `pos` is always 0 here — `fast_yaml` doesn't expose a
/// position on its error path.
pub fn parse(input: String) -> Result(YamlValue, ParseError) {
  ensure_all_started(binary_to_atom("fast_yaml"))
  case fast_yaml_decode(input) {
    Ok(docs) -> {
      case decode_documents(docs) {
        [first, ..] -> Ok(first)
        [] -> Ok(value.Null)
      }
    }
    Error(err) -> Error(format_error(err))
  }
}

/// Parse a multi-document stream via `fast_yaml`. Same error semantics
/// as `parse`.
pub fn parse_all(input: String) -> Result(List(YamlValue), ParseError) {
  ensure_all_started(binary_to_atom("fast_yaml"))
  case fast_yaml_decode(input) {
    Ok(docs) -> Ok(decode_documents(docs))
    Error(err) -> Error(format_error(err))
  }
}

@external(erlang, "fast_yaml", "decode")
fn fast_yaml_decode(input: String) -> Result(Dynamic, Dynamic)

@external(erlang, "application", "ensure_all_started")
fn ensure_all_started(app: Dynamic) -> Dynamic

@external(erlang, "erlang", "binary_to_atom")
fn binary_to_atom(name: String) -> Dynamic

fn format_error(err: Dynamic) -> ParseError {
  ParseError("fast_yaml decode error: " <> string.inspect(err), 0)
}

fn decode_documents(docs: Dynamic) -> List(YamlValue) {
  case dynamic_to_list(docs) {
    Ok(items) -> list.map(items, convert_value)
    Error(_) -> [convert_value(docs)]
  }
}

fn convert_value(val: Dynamic) -> YamlValue {
  // Try decoders in priority order; first match wins, otherwise Null.
  try_as_int(val)
  |> option.lazy_or(fn() { try_as_float(val) })
  |> option.lazy_or(fn() { try_as_mapping(val) })
  |> option.lazy_or(fn() { try_as_sequence(val) })
  |> option.lazy_or(fn() { try_as_string(val) })
  |> option.unwrap(value.Null)
}

fn try_as_int(val: Dynamic) -> Option(YamlValue) {
  dynamic_to_int(val) |> result.map(value.Int) |> option.from_result
}

fn try_as_float(val: Dynamic) -> Option(YamlValue) {
  dynamic_to_float(val) |> result.map(value.Float) |> option.from_result
}

fn try_as_mapping(val: Dynamic) -> Option(YamlValue) {
  try_proplist(val) |> option.map(value.Mapping)
}

fn try_as_sequence(val: Dynamic) -> Option(YamlValue) {
  dynamic_to_list(val)
  |> result.map(fn(items) { value.Sequence(list.map(items, convert_value)) })
  |> option.from_result
}

fn try_as_string(val: Dynamic) -> Option(YamlValue) {
  dynamic_to_string(val)
  |> result.map(scalar.parse_scalar)
  |> option.from_result
}

fn try_proplist(val: Dynamic) -> Option(List(#(String, YamlValue))) {
  use items <- option.then(dynamic_to_list(val) |> option.from_result)
  case items {
    // fast_yaml encodes `{}` and `[]` identically as `[]`, so an empty
    // result is fundamentally ambiguous. Prefer Mapping since `{}` is the
    // more common author intent; users wanting Sequence([]) for `[]` should
    // use the pure parser.
    [] -> Some([])
    _ ->
      case list.all(items, is_tuple2) {
        True -> Some(list.filter_map(items, decode_pair))
        False -> None
      }
  }
}

fn decode_pair(item: Dynamic) -> Result(#(String, YamlValue), Nil) {
  use #(key, raw_val) <- result.try(decode_tuple2(item))
  use k <- result.try(dynamic_to_string(key))
  Ok(#(k, convert_value(raw_val)))
}

@external(erlang, "taffy_ffi", "to_list")
fn dynamic_to_list(val: Dynamic) -> Result(List(Dynamic), Nil)

@external(erlang, "taffy_ffi", "to_string")
fn dynamic_to_string(val: Dynamic) -> Result(String, Nil)

@external(erlang, "taffy_ffi", "to_int")
fn dynamic_to_int(val: Dynamic) -> Result(Int, Nil)

@external(erlang, "taffy_ffi", "to_float")
fn dynamic_to_float(val: Dynamic) -> Result(Float, Nil)

@external(erlang, "taffy_ffi", "is_tuple2")
fn is_tuple2(val: Dynamic) -> Bool

@external(erlang, "taffy_ffi", "decode_tuple2")
fn decode_tuple2(val: Dynamic) -> Result(#(Dynamic, Dynamic), Nil)
