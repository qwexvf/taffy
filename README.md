# yaml

[![Package Version](https://img.shields.io/hexpm/v/yaml)](https://hex.pm/packages/yaml)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/yaml/)

A pure Gleam YAML 1.2 parser.

## Features

- **Pure Gleam** - No FFI, works on all targets (Erlang, JavaScript)
- **YAML 1.2** - Supports most common YAML features
- **JSON compatible** - Convert YAML values to JSON

### Supported YAML Features

- Scalars: strings, numbers, booleans, null
- Block collections: sequences and mappings
- Flow collections: `[a, b]` and `{key: value}`
- Multi-line strings: `|` (literal) and `>` (folded)
- Anchors and aliases: `&anchor` and `*alias`
- Comments: `# comment`
- Document markers: `---` and `...`

## Installation

```sh
gleam add yaml@1
```

## Usage

### Parsing

```gleam
import yaml

pub fn main() {
  let input = "
name: John
age: 30
active: true
"
  let assert Ok(value) = yaml.parse(input)

  // Access fields
  let assert Ok(name) = yaml.get(value, "name")
  let assert Ok(age) = yaml.get(value, "age")
}
```

### Nested Access

```gleam
import yaml

pub fn main() {
  let input = "
person:
  name: John
  address:
    city: New York
"
  let assert Ok(value) = yaml.parse(input)

  // Access nested fields
  let assert Ok(city) = yaml.get_path(value, ["person", "address", "city"])
}
```

### Converting to JSON

```gleam
import yaml

pub fn main() {
  let assert Ok(value) = yaml.parse("name: John")
  let json_string = yaml.to_json_string(value)
  // {"name": "John"}
}
```

### Type Accessors

```gleam
import yaml
import gleam/option.{Some, None}

pub fn main() {
  let assert Ok(value) = yaml.parse("count: 42")
  let assert Ok(count) = yaml.get(value, "count")

  case yaml.as_int(count) {
    Some(n) -> io.println("Count is: " <> int.to_string(n))
    None -> io.println("Not an integer")
  }
}
```

## Development

```sh
gleam test  # Run the tests
gleam check # Type check
```
