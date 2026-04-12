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

/// Parse a YAML string using the fast_yaml C NIF backend.
///
/// Returns the same `YamlValue` type as `taffy.parse()` but significantly faster.
/// Requires the `fast_yaml` package and only works on the Erlang target.
pub fn parse(input: String) -> Result(YamlValue, String) {
  ensure_started()
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

/// Parse all YAML documents in a stream using the native backend.
pub fn parse_all(input: String) -> Result(List(YamlValue), String) {
  ensure_started()
  case fast_yaml_decode(input) {
    Ok(docs) -> Ok(decode_documents(docs))
    Error(err) -> Error(format_error(err))
  }
}

// ---------------------------------------------------------------------------
// FFI bindings
// ---------------------------------------------------------------------------

@external(erlang, "fast_yaml", "decode")
fn fast_yaml_decode(input: String) -> Result(Dynamic, Dynamic)

@external(erlang, "application", "ensure_all_started")
fn ensure_all_started(app: Dynamic) -> Dynamic

@external(erlang, "erlang", "binary_to_atom")
fn binary_to_atom(name: String) -> Dynamic

fn ensure_started() -> Nil {
  ensure_all_started(binary_to_atom("fast_yaml"))
  Nil
}

fn format_error(err: Dynamic) -> String {
  let str = string.inspect(err)
  "fast_yaml decode error: " <> str
}

// ---------------------------------------------------------------------------
// Convert fast_yaml output to YamlValue
// ---------------------------------------------------------------------------

fn decode_documents(docs: Dynamic) -> List(YamlValue) {
  case dynamic_to_list(docs) {
    Ok(items) -> list.map(items, convert_value)
    Error(_) -> [convert_value(docs)]
  }
}

fn convert_value(val: Dynamic) -> YamlValue {
  // Try each type in order: integer, float, proplist (mapping), list (sequence), string
  case try_int(val) {
    Some(i) -> value.Int(i)
    None ->
      case try_float(val) {
        Some(f) -> value.Float(f)
        None ->
          case try_proplist(val) {
            Some(pairs) -> value.Mapping(pairs)
            None ->
              case try_list(val) {
                Some(items) -> value.Sequence(items)
                None ->
                  case try_string(val) {
                    Some(s) -> parse_scalar(s)
                    None -> value.Null
                  }
              }
          }
      }
  }
}

/// fast_yaml keeps booleans and null as strings — we parse them here.
fn parse_scalar(s: String) -> YamlValue {
  case s {
    "true" | "True" | "TRUE" | "yes" | "Yes" | "YES" | "on" | "On" | "ON" ->
      value.Bool(True)
    "false" | "False" | "FALSE" | "no" | "No" | "NO" | "off" | "Off" | "OFF" ->
      value.Bool(False)
    "null" | "Null" | "NULL" | "~" | "" -> value.Null
    _ -> {
      // Try parsing as number (fast_yaml already handles most, but some edge cases)
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
      // Check if this is a proplist (list of 2-tuples) or a plain list
      case list.all(items, is_tuple2) {
        True -> {
          let pairs =
            list.filter_map(items, fn(item) {
              case decode_tuple2(item) {
                Ok(#(key, raw_val)) -> {
                  case try_string(key) {
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

fn try_list(val: Dynamic) -> option.Option(List(YamlValue)) {
  case dynamic_to_list(val) {
    Ok(items) -> Some(list.map(items, convert_value))
    Error(_) -> None
  }
}

fn try_string(val: Dynamic) -> option.Option(String) {
  case dynamic_to_string(val) {
    Ok(s) -> Some(s)
    Error(_) -> None
  }
}

fn try_int(val: Dynamic) -> option.Option(Int) {
  case dynamic_to_int(val) {
    Ok(i) -> Some(i)
    Error(_) -> None
  }
}

fn try_float(val: Dynamic) -> option.Option(Float) {
  case dynamic_to_float(val) {
    Ok(f) -> Some(f)
    Error(_) -> None
  }
}

// ---------------------------------------------------------------------------
// Dynamic helpers (Erlang FFI)
// ---------------------------------------------------------------------------

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
