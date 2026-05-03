import gleam/dict.{type Dict}
import gleam/option.{type Option, None}
import taffy/lexer.{type Token}
import taffy/value.{type YamlValue}

/// `tokens` is the *remaining* token stream. `consumed` is the LIFO of tokens
/// already advanced past (most-recent first), used by `helpers.backtrack` to
/// rewind a single step in O(1) without reindexing.
pub type Parser {
  Parser(
    tokens: List(Token),
    consumed: List(Token),
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
    consumed: [],
    pos: 0,
    anchors: dict.new(),
    seq_entry_indent: None,
    in_inline_value: False,
    flow_min_indent: 0,
    flow_multiline: False,
    tag_handles: [],
  )
}

pub fn register_anchor(parser: Parser, name: String, val: YamlValue) -> Parser {
  Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
}
