//// YAML Parser Benchmark
////
//// Compares taffy (pure Gleam) vs fast_yaml (C NIF / libyaml) vs yamerl (pure Erlang).
//// Run with: gleam run -m benchmark

import gleam/io
import gleam/string
import gleamy/bench
import simplifile
import taffy
import taffy/native as taffy_native

// FFI bindings for fast_yaml
@external(erlang, "fast_yaml", "decode")
fn fast_yaml_decode(input: String) -> Result(a, b)

// FFI bindings for yamerl
@external(erlang, "yamerl_constr", "string")
fn yamerl_string(input: String) -> a

pub fn main() {
  io.println("=== YAML Parser Benchmark ===")
  io.println("")

  // Ensure fast_yaml application is started
  start_fast_yaml()

  // Small YAML
  let small_yaml =
    "name: John
age: 30
active: true
tags:
  - gleam
  - erlang"

  // Medium YAML — nested objects and arrays
  let medium_yaml =
    "openapi: '3.1.0'
info:
  title: Petstore API
  version: '1.0.0'
  description: A simple pet store API
servers:
  - url: https://api.petstore.example.com/v1
paths:
  /pets:
    get:
      operationId: listPets
      summary: List all pets
      tags:
        - pets
      parameters:
        - name: limit
          in: query
          required: false
          schema:
            type: integer
      responses:
        '200':
          description: A list of pets
          content:
            application/json:
              schema:
                type: array
                items:
                  type: object
                  properties:
                    id:
                      type: integer
                    name:
                      type: string
                    tag:
                      type: string
  /pets/{petId}:
    get:
      operationId: showPetById
      parameters:
        - name: petId
          in: path
          required: true
          schema:
            type: string
      responses:
        '200':
          description: A pet
components:
  schemas:
    Pet:
      type: object
      required:
        - id
        - name
      properties:
        id:
          type: integer
        name:
          type: string
        tag:
          type: string
    Error:
      type: object
      required:
        - code
        - message
      properties:
        code:
          type: integer
        message:
          type: string"

  // Large YAML — read from file if available
  let large_yaml = case simplifile.read("examples/ometama.yaml") {
    Ok(content) -> content
    Error(_) -> string.repeat(medium_yaml <> "\n---\n", 5)
  }

  io.println("--- Small YAML (5 keys, flat) ---")
  let small_result =
    bench.run(
      [bench.Input("small", small_yaml)],
      [
        bench.Function("taffy (pure Gleam)", fn(yaml) {
          let _ = taffy.parse(yaml)
          Nil
        }),
        bench.Function("taffy/native (C NIF)", fn(yaml) {
          let _ = taffy_native.parse(yaml)
          Nil
        }),
        bench.Function("fast_yaml (C NIF raw)", fn(yaml) {
          let _ = fast_yaml_decode(yaml)
          Nil
        }),
        bench.Function("yamerl (pure Erlang)", fn(yaml) {
          let _ = yamerl_string(yaml)
          Nil
        }),
      ],
      [bench.Duration(3000)],
    )
  io.println(
    bench.table(small_result, [bench.IPS, bench.Min, bench.Mean, bench.P(99)]),
  )

  io.println("")
  io.println("--- Medium YAML (~80 lines, OpenAPI spec) ---")
  let medium_result =
    bench.run(
      [bench.Input("medium", medium_yaml)],
      [
        bench.Function("taffy (pure Gleam)", fn(yaml) {
          let _ = taffy.parse(yaml)
          Nil
        }),
        bench.Function("taffy/native (C NIF)", fn(yaml) {
          let _ = taffy_native.parse(yaml)
          Nil
        }),
        bench.Function("fast_yaml (C NIF raw)", fn(yaml) {
          let _ = fast_yaml_decode(yaml)
          Nil
        }),
        bench.Function("yamerl (pure Erlang)", fn(yaml) {
          let _ = yamerl_string(yaml)
          Nil
        }),
      ],
      [bench.Duration(3000)],
    )
  io.println(
    bench.table(medium_result, [
      bench.IPS, bench.Min, bench.Mean, bench.P(99),
    ]),
  )

  io.println("")
  io.println(
    "--- Large YAML (~2000 lines, real-world spec) ---",
  )
  let large_result =
    bench.run(
      [bench.Input("large", large_yaml)],
      [
        bench.Function("taffy (pure Gleam)", fn(yaml) {
          let _ = taffy.parse(yaml)
          Nil
        }),
        bench.Function("taffy/native (C NIF)", fn(yaml) {
          let _ = taffy_native.parse(yaml)
          Nil
        }),
        bench.Function("fast_yaml (C NIF raw)", fn(yaml) {
          let _ = fast_yaml_decode(yaml)
          Nil
        }),
        bench.Function("yamerl (pure Erlang)", fn(yaml) {
          let _ = yamerl_string(yaml)
          Nil
        }),
      ],
      [bench.Duration(3000)],
    )
  io.println(
    bench.table(large_result, [bench.IPS, bench.Min, bench.Mean, bench.P(99)]),
  )

  io.println("")
  io.println("Done.")
}

@external(erlang, "application", "ensure_all_started")
fn ensure_started(app: a) -> b

fn start_fast_yaml() -> Nil {
  ensure_started(fast_yaml_atom())
  Nil
}

@external(erlang, "erlang", "binary_to_atom")
fn binary_to_atom(name: String) -> a

fn fast_yaml_atom() -> a {
  binary_to_atom("fast_yaml")
}
