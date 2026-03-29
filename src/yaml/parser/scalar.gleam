//// Scalar parsing utilities.

import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import yaml/value.{type YamlValue}

/// Parse a plain scalar into typed value.
pub fn parse_scalar(s: String) -> YamlValue {
  let trimmed = string.trim(s)

  case string.lowercase(trimmed) {
    // Null
    "null" | "~" | "" -> value.Null
    // Boolean
    "true" | "yes" | "on" -> value.Bool(True)
    "false" | "no" | "off" -> value.Bool(False)
    // Try numeric
    _ -> {
      case int.parse(trimmed) {
        Ok(i) -> value.Int(i)
        Error(_) -> {
          case float.parse(trimmed) {
            Ok(f) -> value.Float(f)
            Error(_) -> {
              // Special floats
              case trimmed {
                ".inf" | ".Inf" | ".INF" -> value.Float(1.0 /. 0.0)
                "-.inf" | "-.Inf" | "-.INF" -> value.Float(-1.0 /. 0.0)
                ".nan" | ".NaN" | ".NAN" -> value.Float(0.0 /. 0.0)
                _ -> value.String(s)
              }
            }
          }
        }
      }
    }
  }
}

/// Convert a value to a string (for use as mapping key from alias).
pub fn value_to_string(val: YamlValue) -> String {
  case val {
    value.String(s) -> s
    value.Int(i) -> int.to_string(i)
    value.Float(f) -> float.to_string(f)
    value.Bool(True) -> "true"
    value.Bool(False) -> "false"
    value.Null -> ""
    _ -> ""
  }
}

/// Convert a value to a string suitable for use as a mapping key.
/// This handles complex values like sequences and mappings.
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
        dict.to_list(pairs)
        |> list.map(fn(pair) {
          let #(k, v) = pair
          k <> ": " <> value_to_key_string(v)
        })
      "{" <> string.join(pair_strs, ", ") <> "}"
    }
  }
}
