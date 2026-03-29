//// Parser types and state.

import gleam/dict.{type Dict}
import yaml/value.{type YamlValue}
import yaml/lexer.{type Token}

/// Parser state.
pub type Parser {
  Parser(
    tokens: List(Token),
    pos: Int,
    anchors: Dict(String, YamlValue),
    indent_stack: List(Int),
  )
}

/// Parse error.
pub type ParseError {
  ParseError(message: String, pos: Int)
}

/// Creates a new parser from tokens.
pub fn new(tokens: List(Token)) -> Parser {
  Parser(tokens: tokens, pos: 0, anchors: dict.new(), indent_stack: [0])
}
