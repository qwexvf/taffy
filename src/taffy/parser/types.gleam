import gleam/dict.{type Dict}
import gleam/option.{type Option, None}
import taffy/lexer.{type Token}
import taffy/value.{type YamlValue}

/// `tokens` is the *remaining* token stream. `consumed` is the LIFO of tokens
/// already advanced past (most-recent first), used by `helpers.backtrack` to
/// rewind a single step in O(1) without reindexing.
///
/// `alias_budget` and `max_depth` cap pathological inputs:
/// - Each alias resolution charges the budget by the resolved subtree's
///   node count, defending against billion-laughs / alias-bomb attacks.
/// - `max_depth` caps recursive value parsing depth, defending against
///   deeply nested `[[[[...]]]]` payloads that would otherwise OOM.
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
    alias_budget: Int,
    depth: Int,
    max_depth: Int,
  )
}

pub type ParseError {
  ParseError(message: String, pos: Int)
}

/// Default node-budget for alias expansion. Enough for any realistic
/// document; trips on alias-bomb constructions like `*c <- *b <- *a`
/// where each level multiplies.
pub const default_alias_budget: Int = 10_000_000

/// Default maximum recursive value-parsing depth.
pub const default_max_depth: Int = 1024

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
    alias_budget: default_alias_budget,
    depth: 0,
    max_depth: default_max_depth,
  )
}

pub fn register_anchor(parser: Parser, name: String, val: YamlValue) -> Parser {
  Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
}

/// Look up an alias and charge the parser's expansion budget by the size
/// of the resolved value. Returns `Error("budget exceeded")` if the budget
/// would go negative (alias-bomb defense), `Error("unknown")` if the name
/// isn't bound, otherwise `Ok(#(value, parser_with_budget_charged))`.
pub fn resolve_alias(
  parser: Parser,
  name: String,
) -> Result(#(YamlValue, Parser), String) {
  case dict.get(parser.anchors, name) {
    Error(_) -> Error("unknown")
    Ok(val) -> {
      let cost = value.size(val)
      let remaining = parser.alias_budget - cost
      case remaining < 0 {
        True -> Error("budget exceeded")
        False -> Ok(#(val, Parser(..parser, alias_budget: remaining)))
      }
    }
  }
}

/// Step into one level of recursive value parsing. Returns
/// `Error(Nil)` if `max_depth` is exceeded. Pair every call with
/// `exit_depth` once the recursive body returns.
pub fn enter_depth(parser: Parser) -> Result(Parser, Nil) {
  case parser.depth + 1 > parser.max_depth {
    True -> Error(Nil)
    False -> Ok(Parser(..parser, depth: parser.depth + 1))
  }
}

pub fn exit_depth(parser: Parser) -> Parser {
  Parser(..parser, depth: parser.depth - 1)
}
