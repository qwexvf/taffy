import gleam/dict.{type Dict}
import gleam/option.{type Option, None}
import taffy/lexer.{type Token}
import taffy/value.{type YamlValue}

pub type Parser {
  Parser(
    tokens: List(Token),
    pos: Int,
    anchors: Dict(String, YamlValue),
    seq_entry_indent: Option(Int),
    in_inline_value: Bool,
    flow_min_indent: Int,
    flow_multiline: Bool,
    tag_handles: List(String),
  )
}

pub type ParseError {
  ParseError(message: String, pos: Int)
}

pub fn new(tokens: List(Token)) -> Parser {
  Parser(
    tokens: tokens,
    pos: 0,
    anchors: dict.new(),
    seq_entry_indent: None,
    in_inline_value: False,
    flow_min_indent: 0,
    flow_multiline: False,
    tag_handles: [],
  )
}
