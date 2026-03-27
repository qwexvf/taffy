//// YAML value types.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// A YAML value.
pub type YamlValue {
  /// Null value (null, ~, or empty)
  Null
  /// Boolean value
  Bool(Bool)
  /// Integer value
  Int(Int)
  /// Floating point value
  Float(Float)
  /// String value
  String(String)
  /// Sequence (list/array)
  Sequence(List(YamlValue))
  /// Mapping (dictionary/object)
  Mapping(Dict(String, YamlValue))
}

/// Converts a YamlValue to a string representation.
pub fn to_string(value: YamlValue) -> String {
  case value {
    Null -> "null"
    Bool(True) -> "true"
    Bool(False) -> "false"
    Int(i) -> int.to_string(i)
    Float(f) -> float.to_string(f)
    String(s) -> "\"" <> escape_string(s) <> "\""
    Sequence(items) -> {
      let inner =
        items
        |> list.map(to_string)
        |> string.join(", ")
      "[" <> inner <> "]"
    }
    Mapping(pairs) -> {
      let inner =
        pairs
        |> dict.to_list
        |> list.map(fn(pair) { pair.0 <> ": " <> to_string(pair.1) })
        |> string.join(", ")
      "{" <> inner <> "}"
    }
  }
}

fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

/// Gets a value as a string.
pub fn as_string(value: YamlValue) -> Option(String) {
  case value {
    String(s) -> Some(s)
    _ -> None
  }
}

/// Gets a value as an int.
pub fn as_int(value: YamlValue) -> Option(Int) {
  case value {
    Int(i) -> Some(i)
    _ -> None
  }
}

/// Gets a value as a float.
pub fn as_float(value: YamlValue) -> Option(Float) {
  case value {
    Float(f) -> Some(f)
    Int(i) -> Some(int.to_float(i))
    _ -> None
  }
}

/// Gets a value as a bool.
pub fn as_bool(value: YamlValue) -> Option(Bool) {
  case value {
    Bool(b) -> Some(b)
    _ -> None
  }
}

/// Gets a value as a list.
pub fn as_list(value: YamlValue) -> Option(List(YamlValue)) {
  case value {
    Sequence(items) -> Some(items)
    _ -> None
  }
}

/// Gets a value as a dict.
pub fn as_dict(value: YamlValue) -> Option(Dict(String, YamlValue)) {
  case value {
    Mapping(pairs) -> Some(pairs)
    _ -> None
  }
}

/// Gets a field from a mapping.
pub fn get(value: YamlValue, key: String) -> Option(YamlValue) {
  case value {
    Mapping(pairs) -> dict.get(pairs, key) |> option.from_result
    _ -> None
  }
}

/// Gets an index from a sequence.
pub fn index(value: YamlValue, idx: Int) -> Option(YamlValue) {
  case value {
    Sequence(items) -> list_at(items, idx)
    _ -> None
  }
}

fn list_at(items: List(a), idx: Int) -> Option(a) {
  case items, idx {
    [], _ -> None
    [first, ..], 0 -> Some(first)
    [_, ..rest], n if n > 0 -> list_at(rest, n - 1)
    _, _ -> None
  }
}

/// Checks if a value is null.
pub fn is_null(value: YamlValue) -> Bool {
  case value {
    Null -> True
    _ -> False
  }
}
