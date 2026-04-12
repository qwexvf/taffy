import gleam/option.{None, Some}
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
  // Note: This basic parser doesn't support merge keys (<<)
  // but anchors/aliases for simple values should work
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

pub fn is_null_test() {
  taffy.is_null(value.Null) |> should.be_true
  taffy.is_null(value.String("hello")) |> should.be_false
}

// JSON conversion tests

pub fn to_json_string_test() {
  let val =
    value.Mapping([#("name", value.String("John")), #("age", value.Int(30))])

  let json_str = taffy.to_json_string(val)
  // Should be valid JSON
  json_str |> should.not_equal("")
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
