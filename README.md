# taffy

YAML 1.2 parser for Gleam. Optional native `fast_yaml` C NIF backend on Erlang.

## Install

```sh
gleam add taffy
gleam add fast_yaml   # optional, native backend (Erlang only)
```

## Usage

```gleam
import taffy

pub fn main() {
  let assert Ok(value) = taffy.parse("name: John\nage: 30")
  let assert Ok(name) = taffy.get(value, "name")
  let json = taffy.to_json_string(value)
}
```

Native backend — same API, faster:

```gleam
import taffy/native

let assert Ok(value) = native.parse(input)
```

| | `taffy.parse` | `taffy/native.parse` |
|---|---|---|
| Target | Erlang + JS | Erlang only |
| Speed (small / medium / large) | 10k / 550 / 150 ops/s | 37k / 3.7k / 385 ops/s |

## Reference

- `parse`, `parse_all`, `parse_with_options`, `parse_all_with_options`
- `get`, `get_or`, `get_path`, `index`
- `as_string`, `as_int`, `as_float`, `as_bool`, `as_list`, `as_dict`, `as_pairs`, `is_null`
- `to_json`, `to_json_string`, `to_yaml`
- `validate_unique_keys`, `error_location`

`parse` rejects alias bombs (>10M nodes) and block nesting >1024 levels.
Override via `parse_with_options(input, Options(..taffy.default_options(), max_depth: 64))`.

## Development

```sh
gleam test
gleam run -m benchmark
```

## License

Apache 2.0
