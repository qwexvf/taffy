//// YAML parser for Gleam.
////
//// A pure Gleam YAML 1.2 parser supporting:
//// - Scalars: strings, numbers, booleans, null
//// - Block collections: sequences and mappings
//// - Flow collections: [a, b] and {key: value}
//// - Multi-line strings: | (literal) and > (folded)
//// - Anchors and aliases: &anchor and *alias
//// - Comments: # comment
////
//// ## Quick Start
////
//// ```gleam
//// import yaml
////
//// pub fn main() {
////   let input = "
//// name: John
//// age: 30
//// active: true
//// "
////   let assert Ok(value) = yaml.parse(input)
////   let assert Ok(name) = yaml.get(value, "name")
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import yaml/lexer
import yaml/parser
import yaml/parser/types.{type ParseError, ParseError}
import yaml/value.{type YamlValue}

// Re-export types
pub type Value =
  YamlValue

pub type Error =
  ParseError

// Re-export constructors
pub const null = value.Null

pub const bool = value.Bool

pub const int = value.Int

pub const float = value.Float

pub const string = value.String

pub const sequence = value.Sequence

pub const mapping = value.Mapping

/// Parses a YAML string into a Value.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(val) = yaml.parse("name: John")
/// ```
pub fn parse(input: String) -> Result(Value, Error) {
  case lexer.tokenize(input) {
    Error(msg) -> Error(ParseError(msg, 0))
    Ok(tokens) -> parser.parse(tokens)
  }
}

/// Parses a YAML string, returning Null on error.
pub fn parse_or_null(input: String) -> Value {
  case parse(input) {
    Ok(v) -> v
    Error(_) -> value.Null
  }
}

/// Gets a field from a mapping.
pub fn get(val: Value, key: String) -> Result(Value, Nil) {
  value.get(val, key) |> option.to_result(Nil)
}

/// Gets a field, returning a default if not found.
pub fn get_or(val: Value, key: String, default: Value) -> Value {
  case value.get(val, key) {
    option.Some(v) -> v
    option.None -> default
  }
}

/// Gets a nested field using a path of keys.
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

/// Gets an index from a sequence.
pub fn index(val: Value, idx: Int) -> Result(Value, Nil) {
  value.index(val, idx) |> option.to_result(Nil)
}

/// Gets a value as a string.
pub fn as_string(val: Value) -> Option(String) {
  value.as_string(val)
}

/// Gets a value as an int.
pub fn as_int(val: Value) -> Option(Int) {
  value.as_int(val)
}

/// Gets a value as a float.
pub fn as_float(val: Value) -> Option(Float) {
  value.as_float(val)
}

/// Gets a value as a bool.
pub fn as_bool(val: Value) -> Option(Bool) {
  value.as_bool(val)
}

/// Gets a value as a list.
pub fn as_list(val: Value) -> Option(List(Value)) {
  value.as_list(val)
}

/// Gets a value as a dict.
pub fn as_dict(val: Value) -> Option(Dict(String, Value)) {
  value.as_dict(val)
}

/// Checks if a value is null.
pub fn is_null(val: Value) -> Bool {
  value.is_null(val)
}

/// Converts a Value to a string representation.
pub fn to_string(val: Value) -> String {
  value.to_string(val)
}

/// Converts a Value to Json.
pub fn to_json(val: Value) -> Json {
  case val {
    value.Null -> json.null()
    value.Bool(b) -> json.bool(b)
    value.Int(i) -> json.int(i)
    value.Float(f) -> json.float(f)
    value.String(s) -> json.string(s)
    value.Sequence(items) -> json.array(items, to_json)
    value.Mapping(pairs) ->
      json.object(
        pairs
        |> dict.to_list
        |> list.map(fn(pair) { #(pair.0, to_json(pair.1)) }),
      )
  }
}

/// Converts a Value to a JSON string.
pub fn to_json_string(val: Value) -> String {
  to_json(val) |> json.to_string
}

/// Gets error message from ParseError.
pub fn error_message(err: Error) -> String {
  err.message
}

/// Gets error position from ParseError.
pub fn error_position(err: Error) -> Int {
  err.pos
}
