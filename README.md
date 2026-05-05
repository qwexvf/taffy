# taffy

A YAML 1.2 parser for Gleam with an optional native C NIF backend for high performance.

## Features

- **Pure Gleam** — works on Erlang and JavaScript targets, no FFI required
- **Native backend** — optional C NIF via [fast_yaml](https://github.com/processone/fast_yaml) for ~3-7x faster parsing
- **YAML 1.2 compliant** — passes 351/351 cases of the official YAML test suite (one case parses correctly but emits keys in a different order from the reference output)
- **Merge keys** — `<<: *anchor` and `<<: [*a, *b]` resolved automatically per YAML 1.1
- **JSON output** — `to_json` / `to_json_string` for handing parsed values to `gleam_json`
- **YAML emit** — `to_yaml` round-trips through `parse` (lossy on tags, anchors, comments)

## Installation

```sh
gleam add taffy
```

For the native C NIF backend (optional, Erlang target only):

```sh
gleam add fast_yaml
```

## Usage

### Pure Gleam (default)

```gleam
import taffy

pub fn main() {
  let assert Ok(value) = taffy.parse("
name: John
age: 30
active: true
tags:
  - gleam
  - erlang
")

  // Access fields
  let assert Ok(name) = taffy.get(value, "name")
  let assert Ok(age) = taffy.get(value, "age")

  // Nested access
  let assert Ok(city) = taffy.get_path(value, ["address", "city"])

  // Convert to JSON
  let json = taffy.to_json_string(value)
  // {"name":"John","age":30,"active":true,"tags":["gleam","erlang"]}
}
```

### Native backend (fast, Erlang only)

Same API, same `YamlValue` output — just faster. Requires `fast_yaml` in your deps.

```gleam
import taffy/native

pub fn main() {
  let assert Ok(value) = native.parse("
name: John
age: 30
")

  // Works with all the same taffy functions
  let json = taffy.to_json_string(value)
}
```

### When to use which

| | `taffy.parse` | `taffy/native.parse` |
|---|---|---|
| **Target** | Erlang + JavaScript | Erlang only |
| **Dependencies** | None | `fast_yaml` (C NIF) |
| **Speed (small)** | ~10,000 ops/s | ~37,000 ops/s |
| **Speed (medium)** | ~550 ops/s | ~3,700 ops/s |
| **Speed (large)** | ~150 ops/s | ~385 ops/s |
| **Use case** | JS target, simple configs | Large specs, performance-critical |

### Type accessors

```gleam
import taffy
import gleam/option.{Some, None}

let assert Ok(value) = taffy.parse("count: 42")
let assert Ok(count) = taffy.get(value, "count")

case taffy.as_int(count) {
  Some(n) -> io.println("Count: " <> int.to_string(n))
  None -> io.println("Not an integer")
}

// Available: as_string, as_int, as_float, as_bool, as_list, as_dict,
//            as_pairs (order-preserving), is_null
```

### Multiple documents

```gleam
import taffy

let assert Ok(docs) = taffy.parse_all("
---
name: first
---
name: second
")
// docs is List(YamlValue)
```

### Merge keys

```gleam
let assert Ok(value) = taffy.parse("
defaults: &d
  retries: 3
  timeout: 30
prod:
  <<: *d
  timeout: 60
")
// prod is { retries: 3, timeout: 60 } — own keys win over merged
```

### Strict duplicate-key validation

`parse` is lenient by default (the official YAML test suite expects parsers
to accept duplicates). Apps that want YAML 1.2's spec-strict uniqueness can
opt in:

```gleam
let assert Ok(val) = taffy.parse(input)
case taffy.validate_unique_keys(val) {
  Ok(_) -> ...
  Error(key) -> // first duplicate key
}
```

### Emitting YAML

```gleam
let yaml = taffy.to_yaml(value)
// Block-style output, safe-quoted strings, ends with a trailing newline.
```

`to_yaml` round-trips through `parse` for any value that doesn't carry
metadata taffy doesn't preserve (tags, anchors, comments).

### Error reporting

```gleam
case taffy.parse(input) {
  Ok(value) -> // ...
  Error(err) -> {
    let #(line, col) = taffy.error_location(input, err.pos)
    io.println(
      "parse error at " <> int.to_string(line) <> ":" <> int.to_string(col)
        <> " — " <> err.message,
    )
  }
}
```

### Security guards

`parse` rejects two classes of malicious input by default:

- **Alias bombs** — chained `&a → *a` references whose expansion would
  exceed 10M nodes (configurable per-parser internally).
- **Deep block nesting** — block sequences/mappings nested past 1024 levels.

These let you accept YAML from untrusted sources (config uploads, API
payloads) without booby-trapping memory. See `CHANGELOG.md` for known
limits — pure-flow `[[[...]]]` nesting is not currently capped.

## Development

```sh
gleam test                # Run all tests (unit + YAML 1.2 test suite)
gleam run -m benchmark    # Run parser benchmarks (requires dev deps)
```

## License

Apache 2.0
