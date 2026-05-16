// JS shim for taffy_ffi. The native backend wraps the Erlang `fast_yaml`
// C NIF and is intentionally unavailable on the JavaScript target — these
// stubs let `taffy/native` compile on JS so the pure parser stays
// portable; any call to `native.parse` / `native.parse_all` will return
// the documented "fast_yaml not available on JavaScript target" error.

import { Ok, Error } from "./gleam.mjs";

export function to_list(_) {
  return new Error(undefined);
}

export function to_string(_) {
  return new Error(undefined);
}

export function to_int(_) {
  return new Error(undefined);
}

export function to_float(_) {
  return new Error(undefined);
}

export function is_tuple2(_) {
  return false;
}

export function decode_tuple2(_) {
  return new Error(undefined);
}

export function fast_yaml_decode(_) {
  return new Error("fast_yaml not available on JavaScript target");
}

export function ensure_all_started(_) {
  return null;
}

export function binary_to_atom(name) {
  return name;
}
