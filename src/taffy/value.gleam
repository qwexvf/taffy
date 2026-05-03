//// The `YamlValue` type and direct accessors. Most users want the
//// re-exports on the top-level `taffy` module; this module is exposed for
//// pattern-matching (`value.Null`, `value.Mapping(_)`) and for direct
//// `YamlValue` construction in tests.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Note on `Mapping`: pairs are stored in YAML insertion order. Taffy does
/// not currently reject duplicate keys (YAML 1.2 says it should — this is a
/// known gap). When duplicates exist, `get` / `get_path` return the first
/// match while `as_dict` collapses to last-wins.
pub type YamlValue {
  Null
  Bool(Bool)
  Int(Int)
  Float(Float)
  String(String)
  Sequence(List(YamlValue))
  Mapping(List(#(String, YamlValue)))
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

/// Walk the value tree and return `Error(key)` for the first mapping that
/// contains a duplicate key. YAML 1.2 mandates rejection; merge-key
/// resolution should run before this so synthesized duplicates from `<<`
/// don't fire it.
@internal
pub fn check_no_duplicates(value: YamlValue) -> Result(Nil, String) {
  case value {
    Mapping(pairs) -> {
      case find_duplicate_key(pairs, []) {
        Some(key) -> Error(key)
        None -> {
          list.try_each(pairs, fn(p) { check_no_duplicates(p.1) })
        }
      }
    }
    Sequence(items) -> list.try_each(items, check_no_duplicates)
    _ -> Ok(Nil)
  }
}

fn find_duplicate_key(
  pairs: List(#(String, YamlValue)),
  seen: List(String),
) -> Option(String) {
  case pairs {
    [] -> None
    [#(k, _), ..rest] ->
      case list.contains(seen, k) {
        True -> Some(k)
        False -> find_duplicate_key(rest, [k, ..seen])
      }
  }
}

/// Count the total number of nodes in a value tree (including the root).
/// Used by the parser to budget alias expansion against billion-laughs
/// attacks; exposed so callers can apply their own size policies.
pub fn size(value: YamlValue) -> Int {
  case value {
    Sequence(items) -> list.fold(items, 1, fn(acc, item) { acc + size(item) })
    Mapping(pairs) -> list.fold(pairs, 1, fn(acc, p) { acc + size(p.1) })
    _ -> 1
  }
}

pub fn is_null(value: YamlValue) -> Bool {
  case value {
    Null -> True
    _ -> False
  }
}

/// Emit a value as block-style YAML. The output ends with a trailing
/// newline. Strings are quoted only when they would otherwise be parsed as
/// a different scalar type (numbers, booleans, null) or contain characters
/// that aren't safe to render plain.
pub fn to_yaml(value: YamlValue) -> String {
  emit(value, 0) <> "\n"
}

fn emit(value: YamlValue, indent: Int) -> String {
  case value {
    Null -> "null"
    Bool(True) -> "true"
    Bool(False) -> "false"
    Int(i) -> int.to_string(i)
    Float(f) -> float.to_string(f)
    String(s) -> emit_string(s)
    Sequence([]) -> "[]"
    Sequence(items) -> emit_sequence(items, indent)
    Mapping([]) -> "{}"
    Mapping(pairs) -> emit_mapping(pairs, indent)
  }
}

fn emit_sequence(items: List(YamlValue), indent: Int) -> String {
  let pad = string.repeat(" ", indent)
  items
  |> list.map(fn(item) { pad <> "- " <> emit_after_dash(item, indent + 2) })
  |> string.join("\n")
}

fn emit_mapping(pairs: List(#(String, YamlValue)), indent: Int) -> String {
  let pad = string.repeat(" ", indent)
  pairs
  |> list.map(fn(pair) {
    let #(k, v) = pair
    pad <> emit_string(k) <> ":" <> emit_value_part(v, indent + 2)
  })
  |> string.join("\n")
}

fn emit_after_dash(value: YamlValue, indent: Int) -> String {
  case value {
    Mapping([]) -> "{}"
    Sequence([]) -> "[]"
    Mapping(pairs) ->
      // Hang the first pair off the dash; subsequent pairs sit at indent.
      case pairs {
        [first, ..rest] -> {
          let #(k, v) = first
          let head = emit_string(k) <> ":" <> emit_value_part(v, indent + 2)
          case rest {
            [] -> head
            _ -> head <> "\n" <> emit_mapping(rest, indent)
          }
        }
        [] -> "{}"
      }
    Sequence(_) -> "\n" <> emit(value, indent)
    _ -> emit(value, indent)
  }
}

fn emit_value_part(value: YamlValue, indent: Int) -> String {
  case value {
    Mapping([]) -> " {}"
    Sequence([]) -> " []"
    Mapping(_) | Sequence(_) -> "\n" <> emit(value, indent)
    _ -> " " <> emit(value, indent)
  }
}

fn emit_string(s: String) -> String {
  case is_plain_safe(s) {
    True -> s
    False -> "\"" <> escape_for_double_quote(s) <> "\""
  }
}

fn escape_for_double_quote(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\t", "\\t")
}

fn is_plain_safe(s: String) -> Bool {
  case s {
    "" -> False
    _ -> {
      let first = string.first(s) |> result.unwrap("")
      let bad_starts = [
        "-", "?", ":", "&", "*", "!", "|", ">", "%", "@", "`", ",", "[", "]",
        "{", "}", "#", "'", "\"", " ", "\t",
      ]
      let bad_substrings = [": ", " #", "\n", "\t", "'", "\""]
      let ambiguous_scalars = [
        "true", "True", "TRUE", "false", "False", "FALSE", "null", "Null",
        "NULL", "~", "yes", "Yes", "YES", "no", "No", "NO", "on", "On", "ON",
        "off", "Off", "OFF",
      ]
      let looks_numeric = case int.parse(s), float.parse(s) {
        Error(_), Error(_) -> False
        _, _ -> True
      }
      !list.contains(bad_starts, first)
      && !list.any(bad_substrings, string.contains(s, _))
      && !list.contains(ambiguous_scalars, s)
      && !looks_numeric
    }
  }
}

/// Resolve YAML 1.1 merge keys (`<<`) recursively. The merging mapping's
/// own keys take precedence over merged keys, and earlier merge sources
/// take precedence over later ones. A `<<` whose value isn't a mapping or
/// sequence-of-mappings is left as a plain key.
@internal
pub fn resolve_merges(value: YamlValue) -> YamlValue {
  case value {
    Mapping(pairs) -> {
      let pairs = list.map(pairs, fn(p) { #(p.0, resolve_merges(p.1)) })
      Mapping(merge_pairs(pairs))
    }
    Sequence(items) -> Sequence(list.map(items, resolve_merges))
    _ -> value
  }
}

fn merge_pairs(pairs: List(#(String, YamlValue))) -> List(#(String, YamlValue)) {
  let #(own, merged_sources) =
    list.fold(pairs, #([], []), fn(acc, p) {
      let #(own, sources) = acc
      case p.0 == "<<" {
        True -> #(own, [p.1, ..sources])
        False -> #([p, ..own], sources)
      }
    })
  let own = list.reverse(own)
  let merged_sources = list.reverse(merged_sources)
  case merged_sources {
    [] -> own
    _ -> {
      let merged = list.flat_map(merged_sources, extract_merge_pairs)
      let own_keys =
        own
        |> list.map(fn(p) { p.0 })
      append_unique(own, merged, own_keys)
    }
  }
}

fn extract_merge_pairs(value: YamlValue) -> List(#(String, YamlValue)) {
  case value {
    Mapping(pairs) -> pairs
    Sequence(items) -> list.flat_map(items, extract_merge_pairs)
    _ -> []
  }
}

fn append_unique(
  acc: List(#(String, YamlValue)),
  to_add: List(#(String, YamlValue)),
  seen: List(String),
) -> List(#(String, YamlValue)) {
  case to_add {
    [] -> acc
    [#(k, v), ..rest] ->
      case list.contains(seen, k) {
        True -> append_unique(acc, rest, seen)
        False -> append_unique(list.append(acc, [#(k, v)]), rest, [k, ..seen])
      }
  }
}
