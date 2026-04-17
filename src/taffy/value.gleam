//// YAML value types.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type YamlValue {
  Null
  Bool(Bool)
  Int(Int)
  Float(Float)
  String(String)
  Sequence(List(YamlValue))
  Mapping(List(#(String, YamlValue)))
}

pub fn ordered_insert(
  pairs: List(#(String, YamlValue)),
  key: String,
  val: YamlValue,
) -> List(#(String, YamlValue)) {
  let exists = list.any(pairs, fn(p) { p.0 == key })
  case exists {
    True ->
      list.map(pairs, fn(p) {
        case p.0 == key {
          True -> #(key, val)
          False -> p
        }
      })
    False -> list.append(pairs, [#(key, val)])
  }
}

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

pub fn as_string(value: YamlValue) -> Option(String) {
  case value {
    String(s) -> Some(s)
    _ -> None
  }
}

pub fn as_int(value: YamlValue) -> Option(Int) {
  case value {
    Int(i) -> Some(i)
    _ -> None
  }
}

pub fn as_float(value: YamlValue) -> Option(Float) {
  case value {
    Float(f) -> Some(f)
    Int(i) -> Some(int.to_float(i))
    _ -> None
  }
}

pub fn as_bool(value: YamlValue) -> Option(Bool) {
  case value {
    Bool(b) -> Some(b)
    _ -> None
  }
}

pub fn as_list(value: YamlValue) -> Option(List(YamlValue)) {
  case value {
    Sequence(items) -> Some(items)
    _ -> None
  }
}

pub fn as_dict(value: YamlValue) -> Option(Dict(String, YamlValue)) {
  case value {
    Mapping(pairs) -> Some(dict.from_list(pairs))
    _ -> None
  }
}

pub fn as_pairs(value: YamlValue) -> Option(List(#(String, YamlValue))) {
  case value {
    Mapping(pairs) -> Some(pairs)
    _ -> None
  }
}

pub fn get(value: YamlValue, key: String) -> Option(YamlValue) {
  case value {
    Mapping(pairs) -> list.key_find(pairs, key) |> option.from_result
    _ -> None
  }
}

pub fn index(value: YamlValue, idx: Int) -> Option(YamlValue) {
  case value {
    Sequence(items) -> list.drop(items, idx) |> list.first |> option.from_result
    _ -> None
  }
}

pub fn is_null(value: YamlValue) -> Bool {
  case value {
    Null -> True
    _ -> False
  }
}
