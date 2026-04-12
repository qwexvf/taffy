import gleam/option.{Some}
import gleeunit/should
import taffy
import taffy/native
import taffy/value

pub fn parse_simple_mapping_test() {
  let assert Ok(result) = native.parse("name: John\nage: 30")
  let assert Some(name_val) = value.get(result, "name")
  name_val |> should.equal(value.String("John"))
  let assert Some(age_val) = value.get(result, "age")
  age_val |> should.equal(value.Int(30))
}

pub fn parse_booleans_test() {
  let assert Ok(result) = native.parse("a: true\nb: false\nc: yes\nd: no")
  let assert Some(a) = value.get(result, "a")
  a |> should.equal(value.Bool(True))
  let assert Some(b) = value.get(result, "b")
  b |> should.equal(value.Bool(False))
  let assert Some(c) = value.get(result, "c")
  c |> should.equal(value.Bool(True))
  let assert Some(d) = value.get(result, "d")
  d |> should.equal(value.Bool(False))
}

pub fn parse_null_test() {
  let assert Ok(result) = native.parse("a: null\nb: ~")
  let assert Some(a) = value.get(result, "a")
  a |> should.equal(value.Null)
  let assert Some(b) = value.get(result, "b")
  b |> should.equal(value.Null)
}

pub fn parse_sequence_test() {
  let assert Ok(result) = native.parse("items:\n  - one\n  - two\n  - three")
  let assert Some(items) = value.get(result, "items")
  case items {
    value.Sequence(list) ->
      list
      |> should.equal([
        value.String("one"),
        value.String("two"),
        value.String("three"),
      ])
    _ -> should.fail()
  }
}

pub fn parse_nested_test() {
  let assert Ok(result) = native.parse("outer:\n  inner:\n    key: value")
  let assert Some(outer) = value.get(result, "outer")
  let assert Some(inner) = value.get(outer, "inner")
  let assert Some(key) = value.get(inner, "key")
  key |> should.equal(value.String("value"))
}

pub fn parse_float_test() {
  let assert Ok(result) = native.parse("pi: 3.14")
  let assert Some(pi) = value.get(result, "pi")
  pi |> should.equal(value.Float(3.14))
}

pub fn parse_matches_pure_gleam_test() {
  let yaml = "openapi: '3.1.0'\ninfo:\n  title: My API\n  version: '1.0.0'"
  let assert Ok(native_result) = native.parse(yaml)
  let assert Ok(gleam_result) = taffy.parse(yaml)

  let native_json = taffy.to_json_string(native_result)
  let gleam_json = taffy.to_json_string(gleam_result)
  native_json |> should.equal(gleam_json)
}
