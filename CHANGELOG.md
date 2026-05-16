# Changelog

All notable changes to taffy will be documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/) and the project adheres to
[Semantic Versioning](https://semver.org/): breaking changes to the public
API of `taffy`, `taffy/value`, or `taffy/native` will bump the major
version.

## [1.1.0] — 2026-05-16

### Added
- `taffy.Options`, `default_options()`, `parse_with_options/2`, and
  `parse_all_with_options/2` for per-call `alias_budget` / `max_depth`
  overrides. Defaults unchanged (10M / 1024).
- JavaScript-target shim for `taffy/native` (`src/taffy_ffi.mjs`). The
  pure parser was already JS-compatible; `taffy/native` now also compiles
  on JS, with calls returning an error since `fast_yaml` is Erlang-only.

### Changed
- `as_dict` is now first-wins on duplicate keys, matching `get` /
  `get_path`. Previously collapsed last-wins, which contradicted the
  other accessors.
- `to_yaml` escapes C0 control characters (`\0 \a \b \v \f \r \e` and
  `\x..` for the rest) and `DEL` (`\x7f`) inside double-quoted strings,
  fixing round-tripping for values that contain them.

### Performance
- `value.check_no_duplicates` and merge-key `append_unique` now use
  `gleam/set` for membership instead of `list.contains`, dropping the
  per-mapping O(n²) scan. The merge-append no longer rebuilds the
  accumulator with `list.append/2` per pair.
- `escape_for_double_quote` is one codepoint-fold instead of four full
  `string.replace` passes.

## [1.0.2] — 2026-05-06

### Performance
- Rewrite the pure-Gleam lexer on a `BitArray` with byte-prefix pattern
  matching, replacing the upfront `string.to_graphemes` walk and the
  per-step `List(String)` cons. Hot scanners and the plain scalar reader
  now slice the input once at the boundary instead of cons-per-byte.
  Block scalar accumulators move from `String` concat (O(n²)) to
  list-cons + single join.
- Roughly 2× faster on medium and large inputs (e.g. ~330 IPS on the
  ~2000-line OpenAPI sample, up from ~157). Pure Gleam is now ~67% the
  speed of the libyaml C NIF on large input. No public API changes;
  351/351 conformance suite still passes.

## [1.0.1] — 2026-05-05

### Docs
- Drop the "Supported YAML Features" subset list from README and module
  docs — it read like a disclaimer of incomplete support next to the
  YAML 1.2 / 351-of-351 compliance claim.
- Split the README features bullet so `to_json_string` (one-way JSON
  output) is no longer grouped with `to_yaml` round-tripping.

## [1.0.0] — 2026-05-04

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
  Erlang for ~3-7× speedup on large documents. Returns the same `ParseError`
  type as the pure parser so callers can swap backends without changing
  error handling.

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
