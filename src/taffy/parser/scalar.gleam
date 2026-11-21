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
      case parse_float(s) {
        Ok(f) -> value.Float(f)
        Error(_) ->
          case parse_special_float(s) {
            Ok(f) -> value.Float(f)
            Error(_) -> value.String(s)
          }
      }
  }
}

/// A float in YAML 1.2's syntax,
/// `[-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?`. `float.parse` alone
/// is not enough: on Erlang it reads `521621,621373` as `521621.621373`, and
/// Asana's spec has an example of two ids written that way.
fn parse_float(s: String) -> Result(Float, Nil) {
  let unsigned = case s {
    "-" <> rest | "+" <> rest -> rest
    _ -> s
  }
  let #(mantissa, exponent) = case string.split_once(unsigned, "e") {
    Ok(pair) -> #(pair.0, Ok(pair.1))
    Error(_) ->
      case string.split_once(unsigned, "E") {
        Ok(pair) -> #(pair.0, Ok(pair.1))
        Error(_) -> #(unsigned, Error(Nil))
      }
  }
  let mantissa_ok = case string.split_once(mantissa, ".") {
    Ok(#("", fraction)) -> all_digits(fraction)
    Ok(#(whole, "")) -> all_digits(whole)
    Ok(#(whole, fraction)) -> all_digits(whole) && all_digits(fraction)
    Error(_) -> all_digits(mantissa)
  }
  let exponent_ok = case exponent {
    Error(_) -> True
    Ok("-" <> digits) | Ok("+" <> digits) | Ok(digits) -> all_digits(digits)
  }
  case mantissa_ok && exponent_ok {
    True -> float.parse(s)
    False -> Error(Nil)
  }
}

fn all_digits(s: String) -> Bool {
  s != ""
  && list.all(string.to_graphemes(s), fn(c) {
    case c {
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
      _ -> False
    }
  })
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
