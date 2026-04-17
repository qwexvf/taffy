//// Parser helper functions.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import taffy/lexer
import taffy/parser/types.{type Parser, Parser}

pub fn current(parser: Parser) -> Option(lexer.Token) {
  list.drop(parser.tokens, parser.pos)
  |> list.first
  |> option.from_result
}

pub fn advance(parser: Parser) -> Parser {
  Parser(..parser, pos: parser.pos + 1)
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
