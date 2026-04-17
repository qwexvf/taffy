//// Scalar parsing utilities.

import gleam/float
import gleam/int
import gleam/list
import gleam/string
import taffy/value.{type YamlValue}

pub fn parse_scalar(s: String) -> YamlValue {
  let trimmed = string.trim(s)

  case string.lowercase(trimmed) {
    "null" | "~" | "" -> value.Null
    "true" | "yes" | "on" -> value.Bool(True)
    "false" | "no" | "off" -> value.Bool(False)
    _ -> {
      case try_parse_hex(trimmed) {
        Ok(i) -> value.Int(i)
        Error(_) ->
          case try_parse_octal(trimmed) {
            Ok(i) -> value.Int(i)
            Error(_) ->
              case int.parse(trimmed) {
                Ok(i) -> value.Int(i)
                Error(_) -> {
                  case float.parse(trimmed) {
                    Ok(f) -> value.Float(f)
                    Error(_) -> {
                      case trimmed {
                        ".inf" | ".Inf" | ".INF" -> value.Float(1.0 /. 0.0)
                        "-.inf" | "-.Inf" | "-.INF" -> value.Float(-1.0 /. 0.0)
                        ".nan" | ".NaN" | ".NAN" -> value.Float(0.0 /. 0.0)
                        _ -> value.String(trimmed)
                      }
                    }
                  }
                }
              }
          }
      }
    }
  }
}

fn try_parse_hex(s: String) -> Result(Int, Nil) {
  case string.starts_with(string.lowercase(s), "0x") {
    True -> {
      let hex_part = string.drop_start(s, 2)
      int.base_parse(hex_part, 16)
    }
    False -> Error(Nil)
  }
}

fn try_parse_octal(s: String) -> Result(Int, Nil) {
  case string.starts_with(string.lowercase(s), "0o") {
    True -> {
      let oct_part = string.drop_start(s, 2)
      int.base_parse(oct_part, 8)
    }
    False -> Error(Nil)
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
