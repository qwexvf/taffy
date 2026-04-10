//// Parser types and state.

import gleam/dict.{type Dict}
import gleam/option.{type Option, None}
import yaml/lexer.{type Token}
import yaml/value.{type YamlValue}

/// Parser state.
pub type Parser {
  Parser(
    tokens: List(Token),
    pos: Int,
    anchors: Dict(String, YamlValue),
    indent_stack: List(Int),
    /// Tracks the indent level where the containing block sequence's entries
    /// are expected. Used by scalar continuation to distinguish between a new
    /// sequence entry (Dash at seq level) and plain text (Dash between seq
    /// level and content level).
    seq_entry_indent: Option(Int),
    /// True when parsing an inline mapping value (same line as key).
    /// Used to reject nested implicit block mappings on the same line.
    in_inline_value: Bool,
    /// Minimum indent level for flow content on continuation lines.
    /// Set when entering a flow collection from block context.
    flow_min_indent: Int,
    /// True if the current flow collection crossed a line boundary.
    flow_multiline: Bool,
    /// Tag handles defined by %TAG directives for the current document.
    /// Contains handles like "!prefix!" that map to tag prefixes.
    tag_handles: List(String),
  )
}

/// Parse error.
pub type ParseError {
  ParseError(message: String, pos: Int)
}

/// Creates a new parser from tokens.
pub fn new(tokens: List(Token)) -> Parser {
  Parser(
    tokens: tokens,
    pos: 0,
    anchors: dict.new(),
    indent_stack: [0],
    seq_entry_indent: None,
    in_inline_value: False,
    flow_min_indent: 0,
    flow_multiline: False,
    tag_handles: [],
  )
}
