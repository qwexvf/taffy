//// Parser helper functions.

import gleam/int
import gleam/option.{type Option, None, Some}
import taffy/lexer
import taffy/parser/types.{type Parser, Parser}

pub fn current(parser: Parser) -> Option(lexer.Token) {
  case parser.tokens {
    [first, ..] -> Some(first)
    [] -> None
  }
}

pub fn advance(parser: Parser) -> Parser {
  case parser.tokens {
    [first, ..rest] ->
      Parser(
        ..parser,
        tokens: rest,
        consumed: [first, ..parser.consumed],
        pos: parser.pos + 1,
      )
    [] -> parser
  }
}

/// Undo one `advance` step in O(1). Returns the parser unchanged if nothing
/// has been consumed.
pub fn backtrack(parser: Parser) -> Parser {
  case parser.consumed {
    [last, ..rest] ->
      Parser(
        ..parser,
        tokens: [last, ..parser.tokens],
        consumed: rest,
        pos: parser.pos - 1,
      )
    [] -> parser
  }
}

pub fn skip_whitespace(parser: Parser) -> Parser {
  case current(parser) {
    Some(lexer.Newline) -> skip_whitespace(advance(parser))
    Some(lexer.Indent(_)) -> skip_whitespace(advance(parser))
    Some(lexer.Comment(_)) -> skip_whitespace(advance(parser))
    _ -> parser
  }
}

pub fn flow_whitespace_has_comment(parser: Parser) -> Bool {
  case current(parser) {
    Some(lexer.Comment(_)) -> True
    Some(lexer.Newline) -> flow_whitespace_has_comment(advance(parser))
    Some(lexer.Indent(_)) -> flow_whitespace_has_comment(advance(parser))
    _ -> False
  }
}

pub fn skip_flow_whitespace(parser: Parser) -> Result(Parser, types.ParseError) {
  case current(parser) {
    Some(lexer.Newline) -> {
      let parser = Parser(..parser, flow_multiline: True)
      case parser.flow_min_indent > 0 {
        True -> {
          let after = advance(parser)
          case current(after) {
            Some(lexer.Newline) -> skip_flow_whitespace(after)
            Some(lexer.Indent(_)) -> skip_flow_whitespace(after)
            Some(lexer.Comment(_)) -> skip_flow_whitespace(after)
            Some(lexer.Eof) | None -> Ok(after)
            Some(lexer.BracketClose)
            | Some(lexer.BraceClose)
            | Some(lexer.Comma) -> Ok(after)
            _ ->
              Error(types.ParseError(
                "Flow content must be indented",
                parser.pos,
              ))
          }
        }
        False -> skip_flow_whitespace(advance(parser))
      }
    }
    Some(lexer.Indent(n)) -> {
      let parser = Parser(..parser, flow_multiline: True)
      case n >= parser.flow_min_indent {
        True -> skip_flow_whitespace(advance(parser))
        False -> {
          let after = advance(parser)
          case current(after) {
            Some(lexer.BracketClose)
            | Some(lexer.BraceClose)
            | Some(lexer.Comma) -> Ok(after)
            _ ->
              Error(types.ParseError(
                "Flow content must be indented",
                parser.pos,
              ))
          }
        }
      }
    }
    Some(lexer.Comment(_)) -> skip_flow_whitespace(advance(parser))
    _ -> Ok(parser)
  }
}

pub fn skip_newlines_and_comments(parser: Parser) -> Parser {
  case current(parser) {
    Some(lexer.Newline) -> skip_newlines_and_comments(advance(parser))
    Some(lexer.Comment(_)) -> skip_newlines_and_comments(advance(parser))
    Some(lexer.Indent(_)) -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        Some(lexer.Comment(_)) ->
          skip_newlines_and_comments(advance(after_indent))
        Some(lexer.Newline) -> skip_newlines_and_comments(advance(after_indent))
        _ -> parser
      }
    }
    _ -> parser
  }
}

pub fn skip_newlines_and_comments_tracking(parser: Parser) -> #(Bool, Parser) {
  skip_newlines_and_comments_tracking_loop(parser, False)
}

fn skip_newlines_and_comments_tracking_loop(
  parser: Parser,
  skipped_newline: Bool,
) -> #(Bool, Parser) {
  case current(parser) {
    Some(lexer.Newline) ->
      skip_newlines_and_comments_tracking_loop(advance(parser), True)
    Some(lexer.Comment(_)) ->
      skip_newlines_and_comments_tracking_loop(advance(parser), skipped_newline)
    Some(lexer.Indent(_)) -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        Some(lexer.Comment(_)) ->
          skip_newlines_and_comments_tracking_loop(
            advance(after_indent),
            skipped_newline,
          )
        Some(lexer.Newline) ->
          skip_newlines_and_comments_tracking_loop(advance(after_indent), True)
        _ -> #(skipped_newline, parser)
      }
    }
    _ -> #(skipped_newline, parser)
  }
}

/// True if `token` ends the current line/expression: a newline, a structural
/// indent, a comment, EOF, or absence of a token. Used to recognise places
/// where YAML implicitly closes a value.
pub fn is_terminator(token: Option(lexer.Token)) -> Bool {
  case token {
    Some(lexer.Newline)
    | Some(lexer.Indent(_))
    | Some(lexer.Comment(_))
    | Some(lexer.Eof)
    | None -> True
    _ -> False
  }
}

/// Debug formatter — preserves token kind so test output and traces are unambiguous.
pub fn token_to_string(token: lexer.Token) -> String {
  case token {
    lexer.DocStart -> "---"
    lexer.DocEnd -> "..."
    lexer.Colon -> ":"
    lexer.Question -> "?"
    lexer.Dash -> "-"
    lexer.BracketOpen -> "["
    lexer.BracketClose -> "]"
    lexer.BraceOpen -> "{"
    lexer.BraceClose -> "}"
    lexer.Comma -> ","
    lexer.Anchor(n) -> "&" <> n
    lexer.Alias(n) -> "*" <> n
    lexer.Tag(t) -> t
    lexer.Literal(_) -> "|"
    lexer.Folded(_) -> ">"
    lexer.Plain(s) -> "plain(" <> s <> ")"
    lexer.SingleQuoted(s) -> "'" <> s <> "'"
    lexer.DoubleQuoted(s) -> "\"" <> s <> "\""
    lexer.Comment(c) -> "#" <> c
    lexer.Newline -> "\\n"
    lexer.Indent(n) -> "indent(" <> int.to_string(n) <> ")"
    lexer.Directive(d) -> "%" <> d
    lexer.Eof -> "EOF"
  }
}

/// User-facing formatter — renders a token the way it appears in source.
/// Used in `ParseError.message` so error text reads naturally.
pub fn token_for_error(token: lexer.Token) -> String {
  case token {
    lexer.Plain(s) -> s
    lexer.Newline -> "newline"
    lexer.Indent(_) -> "indentation"
    lexer.Comment(_) -> "comment"
    lexer.Eof -> "end of input"
    other -> token_to_string(other)
  }
}
