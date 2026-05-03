# Changelog

All notable changes to taffy will be documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/) and the project adheres to
[Semantic Versioning](https://semver.org/) (with the caveat that pre-1.0
versions may break compatibility on minor bumps).

## [0.1.0] — 2026-05-04

Initial Hex release.

### Compliance
- Passes 351/351 cases of the official YAML test suite (one case parses
  correctly but emits keys in a different order from the reference output).

### Features
- `parse(input) -> Result(Value, Error)` for single documents
- `parse_all(input) -> Result(List(Value), Error)` for streams
- `to_yaml(value) -> String` block-style emitter
- `to_json` / `to_json_string` via `gleam_json`
- `validate_unique_keys` for opt-in YAML 1.2 key-uniqueness enforcement
- `error_location(input, pos) -> #(line, column)` for user-facing diagnostics
- `as_string` / `as_int` / `as_float` / `as_bool` / `as_list` / `as_dict`
  (unordered) / `as_pairs` (order-preserving) / `is_null` accessors
- `get` / `get_or` / `get_path` / `index` for navigation
- Merge keys (`<<: *anchor`, `<<: [*a, *b]`) resolved automatically
- Optional native backend (`taffy/native`) wrapping `fast_yaml` C NIF on
  Erlang for ~3-7× speedup on large documents

### Security
- Alias-expansion budget (default 10M nodes) defends against
  billion-laughs / alias-bomb constructions
- Recursive value-parsing depth cap (default 1024) defends against
  deeply-nested block input

### Known limits
- Pure-flow `[[[...]]]` nesting depth is not currently capped; if you accept
  flow-only YAML from untrusted sources, validate input size yourself.
- Tags, anchors, and comments are dropped on `to_yaml` round-trip.
- The native backend cannot distinguish empty `{}` from empty `[]` (both
  are encoded identically by `fast_yaml`); taffy resolves the ambiguity in
  favour of `Mapping([])`. Use the pure parser if `Sequence([])` matters.
