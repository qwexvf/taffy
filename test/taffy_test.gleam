import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import taffy
import taffy/value

pub fn main() -> Nil {
  gleeunit.main()
}

// Scalar tests

pub fn parse_null_test() {
  taffy.parse("null")
  |> should.be_ok
  |> should.equal(value.Null)

  taffy.parse("~")
  |> should.be_ok
  |> should.equal(value.Null)
}

pub fn parse_bool_test() {
  taffy.parse("true")
  |> should.be_ok
  |> should.equal(value.Bool(True))

  taffy.parse("false")
  |> should.be_ok
  |> should.equal(value.Bool(False))

  taffy.parse("yes")
  |> should.be_ok
  |> should.equal(value.Bool(True))

  taffy.parse("no")
  |> should.be_ok
  |> should.equal(value.Bool(False))
}

pub fn parse_int_test() {
  taffy.parse("42")
  |> should.be_ok
  |> should.equal(value.Int(42))

  taffy.parse("-17")
  |> should.be_ok
  |> should.equal(value.Int(-17))

  taffy.parse("0")
  |> should.be_ok
  |> should.equal(value.Int(0))
}

pub fn parse_float_test() {
  taffy.parse("3.14")
  |> should.be_ok
  |> should.equal(value.Float(3.14))

  taffy.parse("-2.5")
  |> should.be_ok
  |> should.equal(value.Float(-2.5))
}

pub fn parse_string_test() {
  taffy.parse("hello")
  |> should.be_ok
  |> should.equal(value.String("hello"))

  taffy.parse("\"quoted string\"")
  |> should.be_ok
  |> should.equal(value.String("quoted string"))

  taffy.parse("'single quoted'")
  |> should.be_ok
  |> should.equal(value.String("single quoted"))
}

pub fn parse_escaped_string_test() {
  taffy.parse("\"hello\\nworld\"")
  |> should.be_ok
  |> should.equal(value.String("hello\nworld"))

  taffy.parse("\"tab\\there\"")
  |> should.be_ok
  |> should.equal(value.String("tab\there"))
}

// Flow collection tests

pub fn parse_flow_sequence_test() {
  taffy.parse("[1, 2, 3]")
  |> should.be_ok
  |> should.equal(value.Sequence([value.Int(1), value.Int(2), value.Int(3)]))
}

pub fn parse_empty_flow_sequence_test() {
  taffy.parse("[]")
  |> should.be_ok
  |> should.equal(value.Sequence([]))
}

pub fn parse_nested_flow_sequence_test() {
  taffy.parse("[[1, 2], [3, 4]]")
  |> should.be_ok
  |> should.equal(
    value.Sequence([
      value.Sequence([value.Int(1), value.Int(2)]),
      value.Sequence([value.Int(3), value.Int(4)]),
    ]),
  )
}

pub fn parse_flow_mapping_test() {
  let result = taffy.parse("{name: John, age: 30}")
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get(val, "name") |> should.equal(Ok(value.String("John")))
  taffy.get(val, "age") |> should.equal(Ok(value.Int(30)))
}

pub fn parse_empty_flow_mapping_test() {
  taffy.parse("{}")
  |> should.be_ok
  |> should.equal(value.Mapping([]))
}

// Block collection tests

pub fn parse_block_sequence_test() {
  let input =
    "- one
- two
- three"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.index(val, 0) |> should.equal(Ok(value.String("one")))
  taffy.index(val, 1) |> should.equal(Ok(value.String("two")))
  taffy.index(val, 2) |> should.equal(Ok(value.String("three")))
}

pub fn parse_block_mapping_test() {
  let input =
    "name: John
age: 30
active: true"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get(val, "name") |> should.equal(Ok(value.String("John")))
  taffy.get(val, "age") |> should.equal(Ok(value.Int(30)))
  taffy.get(val, "active") |> should.equal(Ok(value.Bool(True)))
}

pub fn parse_nested_mapping_test() {
  let input =
    "person:
  name: John
  age: 30"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get_path(val, ["person", "name"])
  |> should.equal(Ok(value.String("John")))
  taffy.get_path(val, ["person", "age"])
  |> should.equal(Ok(value.Int(30)))
}

pub fn parse_sequence_of_mappings_test() {
  let input =
    "- name: Alice
  age: 25
- name: Bob
  age: 30"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  let assert Ok(first) = taffy.index(val, 0)
  taffy.get(first, "name") |> should.equal(Ok(value.String("Alice")))
}

// Comment tests

pub fn parse_with_comments_test() {
  let input =
    "# This is a comment
name: John  # inline comment
age: 30"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get(val, "name") |> should.equal(Ok(value.String("John")))
  taffy.get(val, "age") |> should.equal(Ok(value.Int(30)))
}

// Document markers tests

pub fn parse_with_document_start_test() {
  let input =
    "---
name: John"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get(val, "name") |> should.equal(Ok(value.String("John")))
}

// Anchor and alias tests

pub fn parse_anchor_alias_test() {
  let simple_input =
    "name: &myname John
greeting: *myname"

  let result = taffy.parse(simple_input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get(val, "name") |> should.equal(Ok(value.String("John")))
  taffy.get(val, "greeting") |> should.equal(Ok(value.String("John")))
}

pub fn parse_anchor_nested_mapping_test() {
  let input =
    "bill-to: &id001
  given: Chris
ship-to: *id001"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get_path(val, ["bill-to", "given"])
  |> should.equal(Ok(value.String("Chris")))
  taffy.get_path(val, ["ship-to", "given"])
  |> should.equal(Ok(value.String("Chris")))
}

pub fn parse_anchor_with_literal_test() {
  // Test with 2-space indent and nested mapping (just address, no given)
  let input =
    "bill-to: &id001
  address:
    city: Royal Oak
ship-to: *id001"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get_path(val, ["ship-to", "address"])
  |> should.be_ok
}

// Accessor tests

pub fn as_string_test() {
  let val = value.String("hello")
  taffy.as_string(val) |> should.equal(Some("hello"))

  let val = value.Int(42)
  taffy.as_string(val) |> should.equal(None)
}

pub fn as_int_test() {
  let val = value.Int(42)
  taffy.as_int(val) |> should.equal(Some(42))

  let val = value.String("hello")
  taffy.as_int(val) |> should.equal(None)
}

pub fn as_bool_test() {
  let val = value.Bool(True)
  taffy.as_bool(val) |> should.equal(Some(True))

  let val = value.String("hello")
  taffy.as_bool(val) |> should.equal(None)
}

pub fn as_float_test() {
  taffy.as_float(value.Float(3.14)) |> should.equal(Some(3.14))

  // Int coerces to Float
  taffy.as_float(value.Int(7)) |> should.equal(Some(7.0))

  taffy.as_float(value.String("hello")) |> should.equal(None)
  taffy.as_float(value.Null) |> should.equal(None)
}

pub fn as_list_test() {
  let items = [value.Int(1), value.Int(2), value.Int(3)]
  taffy.as_list(value.Sequence(items)) |> should.equal(Some(items))

  taffy.as_list(value.Sequence([])) |> should.equal(Some([]))
  taffy.as_list(value.String("hello")) |> should.equal(None)
  taffy.as_list(value.Mapping([#("a", value.Int(1))])) |> should.equal(None)
}

pub fn as_pairs_test() {
  let pairs = [#("b", value.Int(2)), #("a", value.Int(1))]
  taffy.as_pairs(value.Mapping(pairs)) |> should.equal(Some(pairs))

  taffy.as_pairs(value.Sequence([])) |> should.equal(None)
  taffy.as_pairs(value.Null) |> should.equal(None)
}

pub fn as_dict_test() {
  let pairs = [#("a", value.Int(1)), #("b", value.Int(2))]
  let assert Some(d) = taffy.as_dict(value.Mapping(pairs))
  d |> dict.size |> should.equal(2)
  d |> dict.get("a") |> should.equal(Ok(value.Int(1)))
  d |> dict.get("b") |> should.equal(Ok(value.Int(2)))

  taffy.as_dict(value.Sequence([])) |> should.equal(None)
  taffy.as_dict(value.Null) |> should.equal(None)
}

pub fn is_null_test() {
  taffy.is_null(value.Null) |> should.be_true
  taffy.is_null(value.String("hello")) |> should.be_false
  taffy.is_null(value.Bool(False)) |> should.be_false
  taffy.is_null(value.Int(0)) |> should.be_false
}

// Accessor edge cases

pub fn get_or_test() {
  let val = value.Mapping([#("name", value.String("John"))])

  taffy.get_or(val, "name", value.Null) |> should.equal(value.String("John"))
  taffy.get_or(val, "missing", value.Int(42)) |> should.equal(value.Int(42))

  // get_or on non-mapping returns default
  taffy.get_or(value.Int(5), "any", value.Null) |> should.equal(value.Null)
}

pub fn get_on_non_mapping_test() {
  taffy.get(value.Int(5), "key") |> should.equal(Error(Nil))
  taffy.get(value.String("hi"), "key") |> should.equal(Error(Nil))
  taffy.get(value.Sequence([]), "key") |> should.equal(Error(Nil))
  taffy.get(value.Null, "key") |> should.equal(Error(Nil))
}

pub fn get_missing_key_test() {
  let val = value.Mapping([#("a", value.Int(1))])
  taffy.get(val, "missing") |> should.equal(Error(Nil))
}

pub fn index_edge_cases_test() {
  let seq = value.Sequence([value.Int(1), value.Int(2)])

  // Out of range
  taffy.index(seq, 2) |> should.equal(Error(Nil))
  taffy.index(seq, 99) |> should.equal(Error(Nil))

  // Non-sequence
  taffy.index(value.Mapping([]), 0) |> should.equal(Error(Nil))
  taffy.index(value.String("abc"), 0) |> should.equal(Error(Nil))
  taffy.index(value.Null, 0) |> should.equal(Error(Nil))
}

pub fn get_path_edge_cases_test() {
  let val =
    value.Mapping([
      #("a", value.Mapping([#("b", value.Int(1))])),
    ])

  // Empty path returns the value itself
  taffy.get_path(val, []) |> should.equal(Ok(val))

  // Missing intermediate key
  taffy.get_path(val, ["missing", "b"]) |> should.equal(Error(Nil))

  // Missing leaf
  taffy.get_path(val, ["a", "missing"]) |> should.equal(Error(Nil))

  // Path through non-mapping
  taffy.get_path(value.Int(5), ["a"]) |> should.equal(Error(Nil))
}

// Error-path tests for parse / parse_all

pub fn parse_unclosed_flow_sequence_test() {
  taffy.parse("[1, 2") |> should.be_error
}

pub fn parse_unclosed_flow_mapping_test() {
  taffy.parse("{a: 1") |> should.be_error
}

pub fn parse_misindented_top_level_dash_test() {
  // Top-level sequence with a misindented sibling dash: the first dash sits
  // at column 0 holding a mapping, the second at column 1. The sequence
  // parser locks seq_col on the first dash and rejects the sibling per
  // YAML 1.2 §8.2.1 (YAML test suite ZVH3). Pinned here so the fix survives
  // a test-suite submodule rebaseline.
  taffy.parse("- key: value\n - item1\n") |> should.be_error
}

pub fn parse_misindented_dash_during_scalar_test() {
  // Counter-test: a misindented dash *during* a still-active multi-line
  // plain scalar is folded into the scalar (YAML 1.2 W4TN), not treated as
  // a new sequence item. Pinned so the sequence-column fix doesn't
  // accidentally tighten this path.
  let assert Ok(val) = taffy.parse("- single multiline\n - sequence entry\n")
  val
  |> should.equal(
    value.Sequence([value.String("single multiline - sequence entry")]),
  )
}

pub fn parse_undefined_alias_test() {
  taffy.parse("*nope") |> should.be_error
}

// Security guards

pub fn parse_alias_bomb_rejected_test() {
  // Classic billion-laughs construction: each level multiplies the previous
  // anchor by 9. Without alias-budget, this would expand to 9^N nodes and
  // OOM. The parser must error before exhausting memory.
  let input =
    "a: &a [\"x\",\"x\",\"x\",\"x\",\"x\",\"x\",\"x\",\"x\",\"x\"]
b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]
c: &c [*b,*b,*b,*b,*b,*b,*b,*b,*b]
d: &d [*c,*c,*c,*c,*c,*c,*c,*c,*c]
e: &e [*d,*d,*d,*d,*d,*d,*d,*d,*d]
f: &f [*e,*e,*e,*e,*e,*e,*e,*e,*e]
g: &g [*f,*f,*f,*f,*f,*f,*f,*f,*f]
h: [*g,*g,*g,*g,*g,*g,*g,*g,*g]"

  let assert Error(err) = taffy.parse(input)
  err.message
  |> should.equal("Alias expansion budget exceeded (possible alias-bomb)")
}

pub fn parse_deep_nesting_rejected_test() {
  // 2000 levels of nested block sequences. Each level enters parse_value,
  // which charges the depth budget; 1024 is the default cap so this errors
  // before exhausting stack/memory. (Pure-flow `[[...]]` nesting bypasses
  // parse_value and isn't capped — that's a separate gap; documented in
  // the changelog as a known limit.)
  let depth = 2000
  let input = string.repeat("- ", depth) <> "leaf"
  let assert Error(err) = taffy.parse(input)
  err.message
  |> should.equal("Maximum recursion depth exceeded (possible nesting bomb)")
}

pub fn error_location_test() {
  let input = "name: John\nage: not-a-number-but-valid\n!badtag oops"
  case taffy.parse(input) {
    Error(err) -> {
      let #(line, col) = taffy.error_location(input, err.pos)
      // Just verify we get sensible 1-indexed values; exact line varies
      // by error path.
      should.be_true(line >= 1)
      should.be_true(col >= 1)
    }
    Ok(_) -> Nil
  }
}

pub fn error_location_basic_test() {
  // pos 0 is line 1, col 1.
  taffy.error_location("hello\nworld", 0) |> should.equal(#(1, 1))
  // After "hello\n", we're at line 2 col 1.
  taffy.error_location("hello\nworld", 6) |> should.equal(#(2, 1))
  // Out of range clamps to the last position rather than crashing.
  taffy.error_location("hi", 100) |> should.equal(#(1, 3))
}

pub fn parse_all_empty_test() {
  let assert Ok(docs) = taffy.parse_all("")
  docs |> should.equal([])
}

pub fn parse_all_multiple_documents_test() {
  let input =
    "---
name: first
---
name: second
---
name: third"

  let assert Ok(docs) = taffy.parse_all(input)
  docs
  |> should.equal([
    value.Mapping([#("name", value.String("first"))]),
    value.Mapping([#("name", value.String("second"))]),
    value.Mapping([#("name", value.String("third"))]),
  ])
}

// YAML emitter tests

pub fn to_yaml_scalars_test() {
  taffy.to_yaml(value.Null) |> should.equal("null\n")
  taffy.to_yaml(value.Bool(True)) |> should.equal("true\n")
  taffy.to_yaml(value.Int(42)) |> should.equal("42\n")
  taffy.to_yaml(value.String("hello")) |> should.equal("hello\n")
}

pub fn to_yaml_quotes_ambiguous_strings_test() {
  // Strings that would round-trip as a different scalar must be quoted.
  taffy.to_yaml(value.String("true")) |> should.equal("\"true\"\n")
  taffy.to_yaml(value.String("42")) |> should.equal("\"42\"\n")
  taffy.to_yaml(value.String("null")) |> should.equal("\"null\"\n")
  taffy.to_yaml(value.String("")) |> should.equal("\"\"\n")
}

pub fn to_yaml_quotes_special_chars_test() {
  // Colon-space and hash-space need quoting to avoid being mis-parsed.
  taffy.to_yaml(value.String("a: b")) |> should.equal("\"a: b\"\n")
}

pub fn to_yaml_block_mapping_test() {
  let val =
    value.Mapping([#("name", value.String("John")), #("age", value.Int(30))])
  taffy.to_yaml(val) |> should.equal("name: John\nage: 30\n")
}

pub fn to_yaml_block_sequence_test() {
  let val = value.Sequence([value.String("one"), value.String("two")])
  taffy.to_yaml(val) |> should.equal("- one\n- two\n")
}

pub fn to_yaml_nested_test() {
  let val =
    value.Mapping([
      #(
        "person",
        value.Mapping([
          #("name", value.String("Alice")),
          #("age", value.Int(25)),
        ]),
      ),
    ])
  taffy.to_yaml(val)
  |> should.equal("person:\n  name: Alice\n  age: 25\n")
}

pub fn to_yaml_sequence_of_mappings_test() {
  let val =
    value.Sequence([
      value.Mapping([#("name", value.String("Alice"))]),
      value.Mapping([#("name", value.String("Bob"))]),
    ])
  taffy.to_yaml(val)
  |> should.equal("- name: Alice\n- name: Bob\n")
}

pub fn to_yaml_round_trip_test() {
  let input = "name: John\nage: 30\ntags:\n  - gleam\n  - erlang\n"
  let assert Ok(val) = taffy.parse(input)
  let emitted = taffy.to_yaml(val)
  let assert Ok(round) = taffy.parse(emitted)
  round |> should.equal(val)
}

pub fn to_yaml_empty_collections_test() {
  taffy.to_yaml(value.Sequence([])) |> should.equal("[]\n")
  taffy.to_yaml(value.Mapping([])) |> should.equal("{}\n")
}

// Strict duplicate-key validation

pub fn validate_unique_keys_pass_test() {
  let assert Ok(val) = taffy.parse("a: 1\nb: 2")
  taffy.validate_unique_keys(val) |> should.be_ok
}

pub fn validate_unique_keys_fail_test() {
  let assert Ok(val) = taffy.parse("a: 1\na: 2")
  let assert Error(err) = taffy.validate_unique_keys(val)
  err.message |> should.equal("Duplicate mapping key: a")
}

pub fn validate_unique_keys_nested_test() {
  let assert Ok(val) = taffy.parse("outer:\n  k: 1\n  k: 2")
  let assert Error(err) = taffy.validate_unique_keys(val)
  err.message |> should.equal("Duplicate mapping key: k")
}

// Merge key tests

pub fn parse_merge_key_single_test() {
  let input =
    "defaults: &d
  a: 1
  b: 2
config:
  <<: *d
  c: 3"

  let assert Ok(val) = taffy.parse(input)
  taffy.get_path(val, ["config", "a"]) |> should.equal(Ok(value.Int(1)))
  taffy.get_path(val, ["config", "b"]) |> should.equal(Ok(value.Int(2)))
  taffy.get_path(val, ["config", "c"]) |> should.equal(Ok(value.Int(3)))
  taffy.get_path(val, ["config", "<<"]) |> should.equal(Error(Nil))
}

pub fn parse_merge_key_overrides_test() {
  // Own keys take precedence over merged keys.
  let input =
    "defaults: &d
  a: 1
  b: 2
config:
  <<: *d
  a: 99"

  let assert Ok(val) = taffy.parse(input)
  taffy.get_path(val, ["config", "a"]) |> should.equal(Ok(value.Int(99)))
  taffy.get_path(val, ["config", "b"]) |> should.equal(Ok(value.Int(2)))
}

pub fn parse_merge_key_sequence_test() {
  // Multiple sources; earlier wins between them.
  let input =
    "first: &f
  a: 1
second: &s
  a: 2
  b: 2
config:
  <<: [*f, *s]"

  let assert Ok(val) = taffy.parse(input)
  taffy.get_path(val, ["config", "a"]) |> should.equal(Ok(value.Int(1)))
  taffy.get_path(val, ["config", "b"]) |> should.equal(Ok(value.Int(2)))
}

// Tag directive tests

pub fn parse_yaml_directive_test() {
  // Directives only attach to documents, so we route through parse_all.
  let input =
    "%YAML 1.2
---
name: John"

  let assert Ok([val]) = taffy.parse_all(input)
  taffy.get(val, "name") |> should.equal(Ok(value.String("John")))
}

pub fn parse_tag_directive_test() {
  let input =
    "%TAG !e! tag:example.com,2000:app/
---
name: John"

  let assert Ok([val]) = taffy.parse_all(input)
  taffy.get(val, "name") |> should.equal(Ok(value.String("John")))
}

pub fn parse_declared_tag_handle_works_test() {
  // After a %TAG directive declares !e!, an !e!suffix tag is recognised.
  let input =
    "%TAG !e! tag:example.com,2000:app/
---
greet: !e!hello world"

  let assert Ok([val]) = taffy.parse_all(input)
  taffy.get(val, "greet") |> should.equal(Ok(value.String("world")))
}

// Block scalar tests (literal | and folded >)

pub fn parse_literal_block_scalar_test() {
  let input =
    "msg: |
  line one
  line two"

  let assert Ok(val) = taffy.parse(input)
  taffy.get(val, "msg")
  |> should.equal(Ok(value.String("line one\nline two\n")))
}

pub fn parse_folded_block_scalar_test() {
  let input =
    "msg: >
  line one
  line two"

  let assert Ok(val) = taffy.parse(input)
  // Folded joins consecutive non-empty lines with a space.
  taffy.get(val, "msg")
  |> should.equal(Ok(value.String("line one line two\n")))
}

// JSON conversion tests

pub fn to_json_string_basic_test() {
  let val =
    value.Mapping([#("name", value.String("John")), #("age", value.Int(30))])

  taffy.to_json_string(val)
  |> should.equal("{\"name\":\"John\",\"age\":30}")
}

pub fn to_json_string_null_in_collection_test() {
  let val =
    value.Mapping([
      #("a", value.Null),
      #("b", value.Sequence([value.Null, value.Int(1)])),
    ])

  taffy.to_json_string(val)
  |> should.equal("{\"a\":null,\"b\":[null,1]}")
}

pub fn to_json_string_float_as_int_test() {
  // Whole-valued floats serialize as ints (preserves to_json branch).
  taffy.to_json_string(value.Float(2.0))
  |> should.equal("2")
}

pub fn to_json_string_float_test() {
  // Non-whole floats serialize as floats.
  taffy.to_json_string(value.Float(2.5))
  |> should.equal("2.5")
}

pub fn to_json_string_empty_collections_test() {
  taffy.to_json_string(value.Sequence([])) |> should.equal("[]")
  taffy.to_json_string(value.Mapping([])) |> should.equal("{}")
}

pub fn to_json_string_round_trip_test() {
  let input = "name: John\nage: 30\ntags:\n  - gleam\n  - erlang"
  let assert Ok(val) = taffy.parse(input)
  taffy.to_json_string(val)
  |> should.equal(
    "{\"name\":\"John\",\"age\":30,\"tags\":[\"gleam\",\"erlang\"]}",
  )
}

// Complex document test

pub fn parse_complex_document_test() {
  let input =
    "openapi: \"3.1.0\"
info:
  title: My API
  version: \"1.0.0\"
servers:
  - url: https://api.example.com
    description: Production
paths:
  /users:
    get:
      operationId: listUsers
      responses:
        \"200\":
          description: Success"

  let result = taffy.parse(input)
  result |> should.be_ok

  let assert Ok(val) = result
  taffy.get_path(val, ["info", "title"])
  |> should.equal(Ok(value.String("My API")))
  taffy.get_path(val, ["openapi"])
  |> should.equal(Ok(value.String("3.1.0")))
}
