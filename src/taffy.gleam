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
//// import taffy
////
//// pub fn main() {
////   let input = "
//// name: John
//// age: 30
//// active: true
//// "
////   let assert Ok(value) = taffy.parse(input)
////   let assert Ok(name) = taffy.get(value, "name")
//// }
//// ```

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import taffy/lexer
import taffy/parser
import taffy/parser/types.{type ParseError, ParseError}
import taffy/value.{type YamlValue}

pub type Value =
  YamlValue

pub type Error =
  ParseError

pub const null = value.Null

pub const bool = value.Bool

pub const int = value.Int

pub const float = value.Float

pub const string = value.String

pub const sequence = value.Sequence

pub const mapping = value.Mapping

pub fn parse(input: String) -> Result(Value, Error) {
  case lexer.tokenize(input) {
    Error(msg) -> Error(ParseError(msg, 0))
    Ok(tokens) -> parser.parse(tokens)
  }
}

pub fn parse_all(input: String) -> Result(List(Value), Error) {
  case lexer.tokenize(input) {
    Error(msg) -> Error(ParseError(msg, 0))
    Ok(tokens) -> parser.parse_all(tokens)
  }
}

pub fn get(val: Value, key: String) -> Result(Value, Nil) {
  value.get(val, key) |> option.to_result(Nil)
}

pub fn get_or(val: Value, key: String, default: Value) -> Value {
  case value.get(val, key) {
    option.Some(v) -> v
    option.None -> default
  }
}

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

pub fn index(val: Value, idx: Int) -> Result(Value, Nil) {
  value.index(val, idx) |> option.to_result(Nil)
}

pub fn as_string(val: Value) -> Option(String) {
  value.as_string(val)
}

pub fn as_int(val: Value) -> Option(Int) {
  value.as_int(val)
}

pub fn as_float(val: Value) -> Option(Float) {
  value.as_float(val)
}

pub fn as_bool(val: Value) -> Option(Bool) {
  value.as_bool(val)
}

pub fn as_list(val: Value) -> Option(List(Value)) {
  value.as_list(val)
}

pub fn as_dict(val: Value) -> Option(Dict(String, Value)) {
  value.as_dict(val)
}

pub fn is_null(val: Value) -> Bool {
  value.is_null(val)
}

pub fn to_string(val: Value) -> String {
  value.to_string(val)
}

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

pub fn to_json_string(val: Value) -> String {
  to_json(val) |> json.to_string
}
