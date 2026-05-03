//// Scalar parsing utilities.

import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import taffy/value.{type YamlValue}

pub fn parse_scalar(s: String) -> YamlValue {
  let trimmed = string.trim(s)

  case string.lowercase(trimmed) {
    "null" | "~" | "" -> value.Null
    "true" | "yes" | "on" -> value.Bool(True)
    "false" | "no" | "off" -> value.Bool(False)
    _ -> parse_numeric_or_string(trimmed)
  }
}

fn parse_numeric_or_string(s: String) -> YamlValue {
  let int_result =
    try_parse_hex(s)
    |> result.lazy_or(fn() { try_parse_octal(s) })
    |> result.lazy_or(fn() { int.parse(s) })

  case int_result {
    Ok(i) -> value.Int(i)
    Error(_) ->
      case float.parse(s) {
        Ok(f) -> value.Float(f)
        Error(_) ->
          case parse_special_float(s) {
            Ok(f) -> value.Float(f)
            Error(_) -> value.String(s)
          }
      }
  }
}

fn try_parse_hex(s: String) -> Result(Int, Nil) {
  case string.starts_with(string.lowercase(s), "0x") {
    True -> int.base_parse(string.drop_start(s, 2), 16)
    False -> Error(Nil)
  }
}

fn try_parse_octal(s: String) -> Result(Int, Nil) {
  case string.starts_with(string.lowercase(s), "0o") {
    True -> int.base_parse(string.drop_start(s, 2), 8)
    False -> Error(Nil)
  }
}

fn parse_special_float(s: String) -> Result(Float, Nil) {
  case s {
    ".inf" | ".Inf" | ".INF" -> Ok(1.0 /. 0.0)
    "-.inf" | "-.Inf" | "-.INF" -> Ok(-1.0 /. 0.0)
    ".nan" | ".NaN" | ".NAN" -> Ok(0.0 /. 0.0)
    _ -> Error(Nil)
  }
}

pub fn value_to_key_string(val: YamlValue) -> String {
  case val {
    value.String(s) -> s
    value.Int(i) -> int.to_string(i)
    value.Float(f) -> float.to_string(f)
    value.Bool(True) -> "true"
    value.Bool(False) -> "false"
    value.Null -> ""
    value.Sequence(items) -> {
      let item_strs = list.map(items, value_to_key_string)
      "[" <> string.join(item_strs, ", ") <> "]"
    }
    value.Mapping(pairs) -> {
      let pair_strs =
        pairs
        |> list.map(fn(pair) {
          let #(k, v) = pair
          k <> ": " <> value_to_key_string(v)
        })
      "{" <> string.join(pair_strs, ", ") <> "}"
    }
  }
}
