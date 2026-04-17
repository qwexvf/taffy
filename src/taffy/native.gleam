//// Native YAML parsing backend using fast_yaml (C NIF / libyaml).
////
//// Provides the same `YamlValue` output as the pure Gleam parser but
//// ~16-250x faster. Requires `fast_yaml` as a dependency.
////
//// ```gleam
//// import taffy/native
////
//// let assert Ok(value) = native.parse("name: John\nage: 30")
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import taffy/value.{type YamlValue}

pub fn parse(input: String) -> Result(YamlValue, String) {
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

pub fn parse_all(input: String) -> Result(List(YamlValue), String) {
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

fn format_error(err: Dynamic) -> String {
  let str = string.inspect(err)
  "fast_yaml decode error: " <> str
}

fn decode_documents(docs: Dynamic) -> List(YamlValue) {
  case dynamic_to_list(docs) {
    Ok(items) -> list.map(items, convert_value)
    Error(_) -> [convert_value(docs)]
  }
}

fn convert_value(val: Dynamic) -> YamlValue {
  case dynamic_to_int(val) |> option.from_result {
    Some(i) -> value.Int(i)
    None ->
      case dynamic_to_float(val) |> option.from_result {
        Some(f) -> value.Float(f)
        None ->
          case try_proplist(val) {
            Some(pairs) -> value.Mapping(pairs)
            None ->
              case dynamic_to_list(val) |> option.from_result {
                Some(items) -> value.Sequence(list.map(items, convert_value))
                None ->
                  case dynamic_to_string(val) |> option.from_result {
                    Some(s) -> parse_scalar(s)
                    None -> value.Null
                  }
              }
          }
      }
  }
}

fn parse_scalar(s: String) -> YamlValue {
  case s {
    "true" | "True" | "TRUE" | "yes" | "Yes" | "YES" | "on" | "On" | "ON" ->
      value.Bool(True)
    "false" | "False" | "FALSE" | "no" | "No" | "NO" | "off" | "Off" | "OFF" ->
      value.Bool(False)
    "null" | "Null" | "NULL" | "~" | "" -> value.Null
    _ -> {
      case int.parse(s) {
        Ok(i) -> value.Int(i)
        Error(_) ->
          case float.parse(s) {
            Ok(f) -> value.Float(f)
            Error(_) -> value.String(s)
          }
      }
    }
  }
}

fn try_proplist(val: Dynamic) -> option.Option(List(#(String, YamlValue))) {
  case dynamic_to_list(val) {
    Ok(items) -> {
      case list.all(items, is_tuple2) {
        True -> {
          let pairs =
            list.filter_map(items, fn(item) {
              case decode_tuple2(item) {
                Ok(#(key, raw_val)) -> {
                  case dynamic_to_string(key) |> option.from_result {
                    Some(k) -> Ok(#(k, convert_value(raw_val)))
                    None -> Error(Nil)
                  }
                }
                Error(_) -> Error(Nil)
              }
            })
          case pairs {
            [] -> None
            _ -> Some(pairs)
          }
        }
        False -> None
      }
    }
    Error(_) -> None
  }
}

@external(erlang, "native_ffi", "to_list")
fn dynamic_to_list(val: Dynamic) -> Result(List(Dynamic), Nil)

@external(erlang, "native_ffi", "to_string")
fn dynamic_to_string(val: Dynamic) -> Result(String, Nil)

@external(erlang, "native_ffi", "to_int")
fn dynamic_to_int(val: Dynamic) -> Result(Int, Nil)

@external(erlang, "native_ffi", "to_float")
fn dynamic_to_float(val: Dynamic) -> Result(Float, Nil)

@external(erlang, "native_ffi", "is_tuple2")
fn is_tuple2(val: Dynamic) -> Bool

@external(erlang, "native_ffi", "decode_tuple2")
fn decode_tuple2(val: Dynamic) -> Result(#(Dynamic, Dynamic), Nil)
