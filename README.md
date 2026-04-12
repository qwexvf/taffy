# taffy

A YAML 1.2 parser for Gleam with an optional native C NIF backend for high performance.

## Features

- **Pure Gleam** — works on Erlang and JavaScript targets, no FFI required
- **Native backend** — optional C NIF via [fast_yaml](https://github.com/processone/fast_yaml) for 10-56x faster parsing
- **YAML 1.2 compliant** — passes 350/351 official YAML test suite cases
- **JSON compatible** — convert parsed YAML to JSON with `to_json_string`

### Supported YAML Features

- Scalars: strings, numbers, booleans, null
- Block collections: sequences (`-`) and mappings (`:`)
- Flow collections: `[a, b]` and `{key: value}`
- Multi-line strings: `|` (literal) and `>` (folded)
- Anchors and aliases: `&anchor` and `*alias`
- Comments, document markers (`---`, `...`), tag directives

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
  // 10-56x faster than pure Gleam, same result type
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
| **Speed (small)** | 17,725 ops/s | 88,835 ops/s |
| **Speed (medium)** | 1,141 ops/s | 10,576 ops/s |
| **Speed (large)** | 4.5 ops/s | 253 ops/s |
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

// Available: as_string, as_int, as_float, as_bool, as_list, as_dict, is_null
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

## Development

```sh
gleam test                # Run all tests (unit + YAML 1.2 test suite)
gleam run -m benchmark    # Run parser benchmarks (requires dev deps)
```
