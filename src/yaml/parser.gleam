//// YAML parser - parses tokens into YAML values.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import yaml/lexer.{type Token}
import yaml/value.{type YamlValue}

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

/// Parses tokens into a YAML value.
pub fn parse(tokens: List(Token)) -> Result(YamlValue, ParseError) {
  let parser = new(tokens)
  let parser = skip_newlines_and_comments(parser)

  // Skip document start if present
  let parser = case current(parser) {
    Some(lexer.DocStart) -> advance(parser) |> skip_newlines_and_comments
    _ -> parser
  }

  case parse_value(parser, 0) {
    Ok(#(value, _)) -> Ok(value)
    Error(e) -> Error(e)
  }
}

/// Parses a single value at the given indentation level.
fn parse_value(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)

  case current(parser) {
    None -> Ok(#(value.Null, parser))
    Some(lexer.Eof) -> Ok(#(value.Null, parser))
    Some(lexer.DocEnd) -> Ok(#(value.Null, parser))

    // Anchor
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_spaces
      case parse_value(parser, min_indent) {
        Ok(#(val, parser)) -> {
          let parser =
            Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
          Ok(#(val, parser))
        }
        Error(e) -> Error(e)
      }
    }

    // Alias
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(val, parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }

    // Skip tags for now
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser) |> skip_spaces
      parse_value(parser, min_indent)
    }

    // Flow sequence
    Some(lexer.BracketOpen) -> parse_flow_sequence(advance(parser))

    // Flow mapping
    Some(lexer.BraceOpen) -> parse_flow_mapping(advance(parser))

    // Block sequence
    Some(lexer.Dash) -> parse_block_sequence(parser, min_indent)

    // Literal block scalar
    Some(lexer.Literal) -> parse_literal_block(advance(parser))

    // Folded block scalar
    Some(lexer.Folded) -> parse_folded_block(advance(parser))

    // Scalars
    Some(lexer.Plain(s)) -> {
      let parser = advance(parser)
      // Check if this is a mapping key
      let parser = skip_spaces(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          // This is a mapping
          parse_block_mapping_from_key(s, advance(parser), min_indent)
        }
        _ -> Ok(#(parse_scalar(s), parser))
      }
    }

    Some(lexer.SingleQuoted(s)) -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) ->
          parse_block_mapping_from_key(s, advance(parser), min_indent)
        _ -> Ok(#(value.String(s), parser))
      }
    }

    Some(lexer.DoubleQuoted(s)) -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) ->
          parse_block_mapping_from_key(s, advance(parser), min_indent)
        _ -> Ok(#(value.String(s), parser))
      }
    }

    Some(lexer.Indent(n)) if n >= min_indent -> {
      let parser = advance(parser)
      parse_value(parser, min_indent)
    }

    Some(lexer.Indent(_)) -> Ok(#(value.Null, parser))

    Some(token) ->
      Error(ParseError(
        "Unexpected token: " <> token_to_string(token),
        parser.pos,
      ))
  }
}

fn parse_flow_sequence(parser: Parser) -> Result(#(YamlValue, Parser), ParseError) {
  parse_flow_sequence_items(skip_whitespace(parser), [])
}

fn parse_flow_sequence_items(
  parser: Parser,
  acc: List(YamlValue),
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_whitespace(parser)

  case current(parser) {
    Some(lexer.BracketClose) ->
      Ok(#(value.Sequence(list.reverse(acc)), advance(parser)))
    Some(lexer.Eof) -> Error(ParseError("Unterminated flow sequence", parser.pos))
    None -> Error(ParseError("Unterminated flow sequence", parser.pos))
    _ -> {
      case parse_flow_value(parser) {
        Ok(#(val, parser)) -> {
          let parser = skip_whitespace(parser)
          case current(parser) {
            Some(lexer.Comma) ->
              parse_flow_sequence_items(advance(parser), [val, ..acc])
            Some(lexer.BracketClose) ->
              Ok(#(value.Sequence(list.reverse([val, ..acc])), advance(parser)))
            _ -> Error(ParseError("Expected ',' or ']'", parser.pos))
          }
        }
        Error(e) -> Error(e)
      }
    }
  }
}

fn parse_flow_mapping(parser: Parser) -> Result(#(YamlValue, Parser), ParseError) {
  parse_flow_mapping_pairs(skip_whitespace(parser), dict.new())
}

fn parse_flow_mapping_pairs(
  parser: Parser,
  acc: Dict(String, YamlValue),
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_whitespace(parser)

  case current(parser) {
    Some(lexer.BraceClose) -> Ok(#(value.Mapping(acc), advance(parser)))
    Some(lexer.Eof) -> Error(ParseError("Unterminated flow mapping", parser.pos))
    None -> Error(ParseError("Unterminated flow mapping", parser.pos))
    _ -> {
      case parse_flow_key(parser) {
        Ok(#(key, parser)) -> {
          let parser = skip_whitespace(parser)
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = skip_whitespace(advance(parser))
              case parse_flow_value(parser) {
                Ok(#(val, parser)) -> {
                  let acc = dict.insert(acc, key, val)
                  let parser = skip_whitespace(parser)
                  case current(parser) {
                    Some(lexer.Comma) ->
                      parse_flow_mapping_pairs(advance(parser), acc)
                    Some(lexer.BraceClose) ->
                      Ok(#(value.Mapping(acc), advance(parser)))
                    _ -> Error(ParseError("Expected ',' or '}'", parser.pos))
                  }
                }
                Error(e) -> Error(e)
              }
            }
            _ -> Error(ParseError("Expected ':'", parser.pos))
          }
        }
        Error(e) -> Error(e)
      }
    }
  }
}

fn parse_flow_key(parser: Parser) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.SingleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(s, advance(parser)))
    _ -> Error(ParseError("Expected mapping key", parser.pos))
  }
}

fn parse_flow_value(parser: Parser) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_whitespace(parser)

  case current(parser) {
    Some(lexer.BracketOpen) -> parse_flow_sequence(advance(parser))
    Some(lexer.BraceOpen) -> parse_flow_mapping(advance(parser))
    Some(lexer.Plain(s)) -> Ok(#(parse_scalar(s), advance(parser)))
    Some(lexer.SingleQuoted(s)) -> Ok(#(value.String(s), advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(value.String(s), advance(parser)))
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_whitespace
      case parse_flow_value(parser) {
        Ok(#(val, parser)) -> {
          let parser =
            Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
          Ok(#(val, parser))
        }
        Error(e) -> Error(e)
      }
    }
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(val, parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    _ -> Ok(#(value.Null, parser))
  }
}

fn parse_block_sequence(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_block_sequence_items(parser, min_indent, [])
}

fn parse_block_sequence_items(
  parser: Parser,
  min_indent: Int,
  acc: List(YamlValue),
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)

  case current(parser) {
    Some(lexer.Dash) -> {
      let parser = advance(parser) |> skip_spaces
      case parse_value(parser, min_indent + 1) {
        Ok(#(val, parser)) -> {
          parse_block_sequence_items(parser, min_indent, [val, ..acc])
        }
        Error(e) -> Error(e)
      }
    }
    Some(lexer.Indent(n)) if n >= min_indent -> {
      let parser = advance(parser)
      case current(parser) {
        Some(lexer.Dash) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_value(parser, n + 1) {
            Ok(#(val, parser)) -> {
              parse_block_sequence_items(parser, min_indent, [val, ..acc])
            }
            Error(e) -> Error(e)
          }
        }
        _ -> Ok(#(value.Sequence(list.reverse(acc)), parser))
      }
    }
    _ -> Ok(#(value.Sequence(list.reverse(acc)), parser))
  }
}

fn parse_block_mapping_from_key(
  first_key: String,
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_spaces(parser)

  // Parse the first value
  case parse_value(parser, min_indent + 1) {
    Ok(#(first_val, parser)) -> {
      let initial = dict.from_list([#(first_key, first_val)])
      parse_block_mapping_pairs(parser, min_indent, initial)
    }
    Error(e) -> Error(e)
  }
}

fn parse_block_mapping_pairs(
  parser: Parser,
  min_indent: Int,
  acc: Dict(String, YamlValue),
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)

  case current(parser) {
    Some(lexer.Indent(n)) if n >= min_indent -> {
      let parser = advance(parser)
      case parse_mapping_key(parser) {
        Ok(#(key, parser)) -> {
          let parser = skip_spaces(parser)
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = advance(parser) |> skip_spaces
              case parse_value(parser, n + 1) {
                Ok(#(val, parser)) -> {
                  let acc = dict.insert(acc, key, val)
                  parse_block_mapping_pairs(parser, min_indent, acc)
                }
                Error(e) -> Error(e)
              }
            }
            _ -> Ok(#(value.Mapping(acc), parser))
          }
        }
        Error(_) -> Ok(#(value.Mapping(acc), parser))
      }
    }
    Some(lexer.Plain(s)) -> {
      // Same-line continuation
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_value(parser, min_indent + 1) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, s, val)
              parse_block_mapping_pairs(parser, min_indent, acc)
            }
            Error(e) -> Error(e)
          }
        }
        _ -> Ok(#(value.Mapping(acc), parser))
      }
    }
    _ -> Ok(#(value.Mapping(acc), parser))
  }
}

fn parse_mapping_key(parser: Parser) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.SingleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(s, advance(parser)))
    _ -> Error(ParseError("Expected mapping key", parser.pos))
  }
}

fn parse_literal_block(parser: Parser) -> Result(#(YamlValue, Parser), ParseError) {
  // Skip to newline
  let parser = skip_to_newline(parser)
  let parser = skip_newline(parser)

  // Determine block indent
  case current(parser) {
    Some(lexer.Indent(block_indent)) -> {
      let parser = advance(parser)
      parse_literal_lines(parser, block_indent, "")
    }
    _ -> Ok(#(value.String(""), parser))
  }
}

fn parse_literal_lines(
  parser: Parser,
  block_indent: Int,
  acc: String,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      let line = case acc {
        "" -> s
        _ -> acc <> "\n" <> s
      }
      let parser = advance(parser)
      parse_literal_after_line(parser, block_indent, line)
    }
    Some(lexer.Newline) -> {
      let line = case acc {
        "" -> ""
        _ -> acc <> "\n"
      }
      parse_literal_after_line(advance(parser), block_indent, line)
    }
    _ -> Ok(#(value.String(acc), parser))
  }
}

fn parse_literal_after_line(
  parser: Parser,
  block_indent: Int,
  acc: String,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Newline) -> {
      parse_literal_lines(advance(parser), block_indent, acc <> "\n")
    }
    Some(lexer.Indent(n)) if n >= block_indent -> {
      let parser = advance(parser)
      parse_literal_lines(parser, block_indent, acc)
    }
    _ -> Ok(#(value.String(acc), parser))
  }
}

fn parse_folded_block(parser: Parser) -> Result(#(YamlValue, Parser), ParseError) {
  // Skip to newline
  let parser = skip_to_newline(parser)
  let parser = skip_newline(parser)

  // Determine block indent
  case current(parser) {
    Some(lexer.Indent(block_indent)) -> {
      let parser = advance(parser)
      parse_folded_lines(parser, block_indent, "", False)
    }
    _ -> Ok(#(value.String(""), parser))
  }
}

fn parse_folded_lines(
  parser: Parser,
  block_indent: Int,
  acc: String,
  had_blank: Bool,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      let line = case acc, had_blank {
        "", _ -> s
        _, True -> acc <> "\n\n" <> s
        _, False -> acc <> " " <> s
      }
      let parser = advance(parser)
      parse_folded_after_line(parser, block_indent, line)
    }
    Some(lexer.Newline) -> {
      parse_folded_after_line(advance(parser), block_indent, acc)
    }
    _ -> Ok(#(value.String(acc), parser))
  }
}

fn parse_folded_after_line(
  parser: Parser,
  block_indent: Int,
  acc: String,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Newline) -> {
      parse_folded_lines(advance(parser), block_indent, acc, True)
    }
    Some(lexer.Indent(n)) if n >= block_indent -> {
      let parser = advance(parser)
      parse_folded_lines(parser, block_indent, acc, False)
    }
    _ -> Ok(#(value.String(acc), parser))
  }
}

/// Parse a plain scalar into typed value.
fn parse_scalar(s: String) -> YamlValue {
  let trimmed = string.trim(s)

  case string.lowercase(trimmed) {
    // Null
    "null" | "~" | "" -> value.Null
    // Boolean
    "true" | "yes" | "on" -> value.Bool(True)
    "false" | "no" | "off" -> value.Bool(False)
    // Try numeric
    _ -> {
      case int.parse(trimmed) {
        Ok(i) -> value.Int(i)
        Error(_) -> {
          case float.parse(trimmed) {
            Ok(f) -> value.Float(f)
            Error(_) -> {
              // Special floats
              case trimmed {
                ".inf" | ".Inf" | ".INF" -> value.Float(1.0 /. 0.0)
                "-.inf" | "-.Inf" | "-.INF" -> value.Float(-1.0 /. 0.0)
                ".nan" | ".NaN" | ".NAN" -> value.Float(0.0 /. 0.0)
                _ -> value.String(s)
              }
            }
          }
        }
      }
    }
  }
}

// Helper functions

fn current(parser: Parser) -> Option(Token) {
  list.drop(parser.tokens, parser.pos)
  |> list.first
  |> option.from_result
}

fn advance(parser: Parser) -> Parser {
  Parser(..parser, pos: parser.pos + 1)
}

fn skip_spaces(parser: Parser) -> Parser {
  // Spaces are already consumed by lexer, this is for inline whitespace
  parser
}

fn skip_whitespace(parser: Parser) -> Parser {
  case current(parser) {
    Some(lexer.Newline) -> skip_whitespace(advance(parser))
    Some(lexer.Indent(_)) -> skip_whitespace(advance(parser))
    Some(lexer.Comment(_)) -> skip_whitespace(advance(parser))
    _ -> parser
  }
}

fn skip_newlines_and_comments(parser: Parser) -> Parser {
  case current(parser) {
    Some(lexer.Newline) -> skip_newlines_and_comments(advance(parser))
    Some(lexer.Comment(_)) -> skip_newlines_and_comments(advance(parser))
    _ -> parser
  }
}

fn skip_to_newline(parser: Parser) -> Parser {
  case current(parser) {
    Some(lexer.Newline) -> parser
    Some(lexer.Eof) -> parser
    None -> parser
    _ -> skip_to_newline(advance(parser))
  }
}

fn skip_newline(parser: Parser) -> Parser {
  case current(parser) {
    Some(lexer.Newline) -> advance(parser)
    _ -> parser
  }
}

fn token_to_string(token: Token) -> String {
  case token {
    lexer.DocStart -> "---"
    lexer.DocEnd -> "..."
    lexer.Colon -> ":"
    lexer.Dash -> "-"
    lexer.BracketOpen -> "["
    lexer.BracketClose -> "]"
    lexer.BraceOpen -> "{"
    lexer.BraceClose -> "}"
    lexer.Comma -> ","
    lexer.Anchor(n) -> "&" <> n
    lexer.Alias(n) -> "*" <> n
    lexer.Tag(t) -> t
    lexer.Literal -> "|"
    lexer.Folded -> ">"
    lexer.Plain(s) -> "plain(" <> s <> ")"
    lexer.SingleQuoted(s) -> "'" <> s <> "'"
    lexer.DoubleQuoted(s) -> "\"" <> s <> "\""
    lexer.Comment(c) -> "#" <> c
    lexer.Newline -> "\\n"
    lexer.Indent(n) -> "indent(" <> int.to_string(n) <> ")"
    lexer.Eof -> "EOF"
  }
}
