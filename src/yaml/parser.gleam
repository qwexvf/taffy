//// YAML parser - parses tokens into YAML values.

import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import yaml/lexer.{type Token}
import yaml/parser/block
import yaml/parser/explicit
import yaml/parser/flow
import yaml/parser/helpers.{
  advance, current, skip_newlines_and_comments, skip_spaces, token_to_string,
}
import yaml/parser/scalar.{parse_scalar}
import yaml/parser/types.{type ParseError, type Parser, ParseError, Parser}
import yaml/value.{type YamlValue}

/// Creates a new parser from tokens.
pub fn new(tokens: List(Token)) -> Parser {
  types.new(tokens)
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
    Ok(#(val, parser)) -> {
      // Check for trailing content after the value
      let parser = skip_newlines_and_comments(parser)
      case current(parser) {
        None | Some(lexer.Eof) -> Ok(val)
        // Multiple documents - we only parse the first one
        Some(lexer.DocStart) -> Ok(val)
        Some(lexer.DocEnd) -> {
          // Skip document end - content after ... is a new document
          // We only return the first document, so ignore the rest
          Ok(val)
        }
        Some(token) ->
          Error(ParseError(
            "Unexpected trailing content: " <> token_to_string(token),
            parser.pos,
          ))
      }
    }
    Error(e) -> Error(e)
  }
}

/// Parses a single value at the given indentation level.
pub fn parse_value(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  // Handle newlines and indentation carefully
  case current(parser) {
    // Skip comments
    Some(lexer.Comment(_)) -> parse_value(advance(parser), min_indent)

    // Newline means next content is at indent 0
    // If min_indent > 0, there's no value at the required indent
    Some(lexer.Newline) -> {
      let parser = advance(parser)
      case min_indent > 0 {
        True -> Ok(#(value.Null, parser))
        False -> parse_value(parser, min_indent)
      }
    }

    None -> Ok(#(value.Null, parser))
    Some(lexer.Eof) -> Ok(#(value.Null, parser))
    Some(lexer.DocEnd) -> Ok(#(value.Null, parser))
    // Another document starting - current document is empty
    Some(lexer.DocStart) -> Ok(#(value.Null, parser))

    // Anchor
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_spaces
      // Check if this anchor is on a mapping key (scalar followed by colon)
      case current(parser) {
        Some(lexer.Plain(s)) -> {
          let after_plain = advance(parser) |> skip_spaces
          case current(after_plain) {
            Some(lexer.Colon) -> {
              // Anchor is on the key, not the mapping
              // Register the anchor with the key value, then parse the mapping
              let parser =
                Parser(
                  ..after_plain,
                  anchors: dict.insert(parser.anchors, name, value.String(s)),
                )
              block.parse_block_mapping_from_key(
                s,
                advance(parser),
                min_indent,
                parse_value,
              )
            }
            _ -> {
              // Not a mapping key, anchor is on the scalar value
              let parser =
                Parser(
                  ..advance(parser),
                  anchors: dict.insert(parser.anchors, name, parse_scalar(s)),
                )
              Ok(#(parse_scalar(s), parser))
            }
          }
        }
        // Handle quoted strings as mapping keys with anchors
        Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) -> {
          let after_quoted = advance(parser) |> skip_spaces
          case current(after_quoted) {
            Some(lexer.Colon) -> {
              // Anchor is on the key
              let parser =
                Parser(
                  ..after_quoted,
                  anchors: dict.insert(parser.anchors, name, value.String(s)),
                )
              block.parse_block_mapping_from_key(
                s,
                advance(parser),
                min_indent,
                parse_value,
              )
            }
            _ -> {
              // Not a mapping key, anchor is on the string value
              let parser =
                Parser(
                  ..advance(parser),
                  anchors: dict.insert(parser.anchors, name, value.String(s)),
                )
              Ok(#(value.String(s), parser))
            }
          }
        }
        _ -> {
          // Not a scalar, parse normally
          case parse_value(parser, min_indent) {
            Ok(#(val, parser)) -> {
              let parser =
                Parser(
                  ..parser,
                  anchors: dict.insert(parser.anchors, name, val),
                )
              Ok(#(val, parser))
            }
            Error(e) -> Error(e)
          }
        }
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

    // Flow sequence - might be a mapping key
    Some(lexer.BracketOpen) -> {
      case flow.parse_flow_sequence(advance(parser)) {
        Ok(#(seq_val, parser)) -> {
          let parser = skip_spaces(parser)
          case current(parser) {
            // Flow sequence used as mapping key
            Some(lexer.Colon) -> {
              let key = scalar.value_to_key_string(seq_val)
              block.parse_block_mapping_from_key(
                key,
                advance(parser),
                min_indent,
                parse_value,
              )
            }
            _ -> Ok(#(seq_val, parser))
          }
        }
        Error(e) -> Error(e)
      }
    }

    // Flow mapping - might be a mapping key
    Some(lexer.BraceOpen) -> {
      case flow.parse_flow_mapping(advance(parser)) {
        Ok(#(map_val, parser)) -> {
          let parser = skip_spaces(parser)
          case current(parser) {
            // Flow mapping used as mapping key
            Some(lexer.Colon) -> {
              let key = scalar.value_to_key_string(map_val)
              block.parse_block_mapping_from_key(
                key,
                advance(parser),
                min_indent,
                parse_value,
              )
            }
            _ -> Ok(#(map_val, parser))
          }
        }
        Error(e) -> Error(e)
      }
    }

    // Block sequence
    Some(lexer.Dash) ->
      block.parse_block_sequence(parser, min_indent, parse_value)

    // Explicit mapping key (?)
    Some(lexer.Question) ->
      explicit.parse_explicit_mapping(parser, min_indent, parse_value)

    // Empty key mapping (: with no key) OR plain scalar starting with colon
    Some(lexer.Colon) -> {
      let after_colon = advance(parser)
      case current(after_colon) {
        // Space or newline after colon - it's an empty key mapping
        Some(lexer.Plain(_))
        | Some(lexer.SingleQuoted(_))
        | Some(lexer.DoubleQuoted(_))
        | Some(lexer.Anchor(_))
        | Some(lexer.Alias(_))
        | Some(lexer.Dash)
        | Some(lexer.BracketOpen)
        | Some(lexer.BraceOpen)
        | Some(lexer.Newline)
        | Some(lexer.Indent(_))
        | Some(lexer.Comment(_))
        | Some(lexer.Eof)
        | None ->
          block.parse_block_mapping_from_key(
            "",
            after_colon,
            min_indent,
            parse_value,
          )
        // Non-whitespace after colon - treat as plain scalar starting with :
        Some(lexer.Colon)
        | Some(lexer.Comma)
        | Some(lexer.BracketClose)
        | Some(lexer.BraceClose) -> {
          // Collect the rest of the plain scalar
          let #(rest, parser) = collect_plain_scalar_from_colon(after_colon)
          Ok(#(parse_scalar(":" <> rest), parser))
        }
        // Tags after colon - still a mapping value
        Some(lexer.Tag(_))
        | Some(lexer.Question)
        | Some(lexer.Literal(_))
        | Some(lexer.Folded(_))
        | Some(lexer.DocStart)
        | Some(lexer.DocEnd) ->
          block.parse_block_mapping_from_key(
            "",
            after_colon,
            min_indent,
            parse_value,
          )
      }
    }

    // Literal block scalar
    Some(lexer.Literal(content)) ->
      Ok(#(value.String(content), advance(parser)))

    // Folded block scalar
    Some(lexer.Folded(content)) -> Ok(#(value.String(content), advance(parser)))

    // Scalars
    Some(lexer.Plain(s)) -> {
      // Collect the full key (including flow indicators in block context)
      let #(full_key, parser) = collect_block_plain_key(advance(parser), s)
      // Check if this is a mapping key
      let parser = skip_spaces(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          // Check if colon is followed by whitespace (key separator)
          let after_colon = advance(parser)
          case current(after_colon) {
            // Colon followed by these = key separator
            Some(lexer.Plain(_))
            | Some(lexer.SingleQuoted(_))
            | Some(lexer.DoubleQuoted(_))
            | Some(lexer.Newline)
            | Some(lexer.Indent(_))
            | Some(lexer.Eof)
            | Some(lexer.Comment(_))
            | Some(lexer.Anchor(_))
            | Some(lexer.Alias(_))
            | Some(lexer.Tag(_))
            | Some(lexer.Dash)
            | Some(lexer.Question)
            | Some(lexer.Literal(_))
            | Some(lexer.Folded(_))
            | Some(lexer.BracketOpen)
            | Some(lexer.BraceOpen)
            | None ->
              block.parse_block_mapping_from_key(
                full_key,
                after_colon,
                min_indent,
                parse_value,
              )
            // Colon followed by non-whitespace = continue collecting
            _ -> {
              let #(rest, parser) =
                collect_block_plain_value(
                  after_colon,
                  full_key <> ":",
                  min_indent,
                )
              Ok(#(parse_scalar(rest), parser))
            }
          }
        }
        // Not a mapping key - check for multiline scalar continuation
        Some(lexer.Newline) -> {
          let #(full_value, parser) =
            check_multiline_continuation(advance(parser), full_key, min_indent)
          Ok(#(parse_scalar(full_value), parser))
        }
        Some(lexer.Indent(n)) if n >= min_indent -> {
          let #(full_value, parser) =
            collect_block_plain_value(parser, full_key, min_indent)
          Ok(#(parse_scalar(full_value), parser))
        }
        _ -> Ok(#(parse_scalar(full_key), parser))
      }
    }

    Some(lexer.SingleQuoted(s)) -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) ->
          block.parse_block_mapping_from_key(
            s,
            advance(parser),
            min_indent,
            parse_value,
          )
        _ -> Ok(#(value.String(s), parser))
      }
    }

    Some(lexer.DoubleQuoted(s)) -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) ->
          block.parse_block_mapping_from_key(
            s,
            advance(parser),
            min_indent,
            parse_value,
          )
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

/// Collect the rest of a plain scalar that starts with a colon.
fn collect_plain_scalar_from_colon(parser: Parser) -> #(String, Parser) {
  case current(parser) {
    Some(lexer.Colon) -> {
      let #(rest, parser) = collect_plain_scalar_from_colon(advance(parser))
      #(":" <> rest, parser)
    }
    Some(lexer.Comma) -> {
      let #(rest, parser) = collect_plain_scalar_from_colon(advance(parser))
      #("," <> rest, parser)
    }
    Some(lexer.Plain(s)) -> {
      let #(rest, parser) = collect_plain_scalar_from_colon(advance(parser))
      #(s <> rest, parser)
    }
    // Stop at whitespace, newlines, or end
    _ -> #("", parser)
  }
}

/// Collect a block plain key (including flow indicators which are plain text in block context).
/// Stops when we see a colon (potential key separator) or whitespace.
fn collect_block_plain_key(parser: Parser, acc: String) -> #(String, Parser) {
  case current(parser) {
    // Flow indicators in block context are part of the key
    Some(lexer.Comma) -> collect_block_plain_key(advance(parser), acc <> ",")
    Some(lexer.BracketOpen) ->
      collect_block_plain_key(advance(parser), acc <> "[")
    Some(lexer.BracketClose) ->
      collect_block_plain_key(advance(parser), acc <> "]")
    Some(lexer.BraceOpen) ->
      collect_block_plain_key(advance(parser), acc <> "{")
    Some(lexer.BraceClose) ->
      collect_block_plain_key(advance(parser), acc <> "}")
    // Colon might be part of the key or the separator - return to caller to decide
    Some(lexer.Colon) -> {
      // Look ahead: if followed by closing flow indicators or another colon immediately, it's part of the key
      let after_colon = advance(parser)
      case current(after_colon) {
        // Colon followed by closing indicators or another colon - part of the key
        Some(lexer.BracketClose)
        | Some(lexer.BraceClose)
        | Some(lexer.Colon)
        | Some(lexer.Comma) -> collect_block_plain_key(after_colon, acc <> ":")
        // Otherwise, the colon is the key separator
        // This includes colon followed by space, opening brackets (values), etc.
        _ -> #(acc, parser)
      }
    }
    // Plain scalar continuation (adjacent to previous token)
    Some(lexer.Plain(s)) -> collect_block_plain_key(advance(parser), acc <> s)
    // Key is complete
    _ -> #(acc, parser)
  }
}

/// Collect a block plain value (including flow indicators which are plain text in block context).
/// Supports multiline plain scalars where continuation lines are indented >= min_indent.
fn collect_block_plain_value(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  case current(parser) {
    // Flow indicators in block context are part of the value
    Some(lexer.Comma) ->
      collect_block_plain_value(advance(parser), acc <> ",", min_indent)
    Some(lexer.BracketOpen) ->
      collect_block_plain_value(advance(parser), acc <> "[", min_indent)
    Some(lexer.BracketClose) ->
      collect_block_plain_value(advance(parser), acc <> "]", min_indent)
    Some(lexer.BraceOpen) ->
      collect_block_plain_value(advance(parser), acc <> "{", min_indent)
    Some(lexer.BraceClose) ->
      collect_block_plain_value(advance(parser), acc <> "}", min_indent)
    Some(lexer.Colon) ->
      collect_block_plain_value(advance(parser), acc <> ":", min_indent)
    // Plain scalar continuation on same line
    Some(lexer.Plain(s)) ->
      collect_block_plain_value(advance(parser), acc <> " " <> s, min_indent)
    // Tag in plain scalar context - treat as literal text
    Some(lexer.Tag(s)) ->
      collect_block_plain_value(advance(parser), acc <> " " <> s, min_indent)
    // Anchor in plain scalar context - treat as literal text
    Some(lexer.Anchor(s)) ->
      collect_block_plain_value(advance(parser), acc <> " &" <> s, min_indent)
    // Newline - check for multiline continuation
    // If followed by Indent, it's a blank line (add \n)
    // If followed by Plain (at indent 0), it's a normal line break
    Some(lexer.Newline) -> {
      let after_newline = advance(parser)
      case current(after_newline) {
        // Newline followed by Indent = blank line
        Some(lexer.Indent(_)) ->
          check_multiline_continuation(after_newline, acc <> "\n", min_indent)
        // Otherwise, normal line break
        _ -> check_multiline_continuation(after_newline, acc, min_indent)
      }
    }
    // Indented line - check if it's a continuation
    Some(lexer.Indent(n)) if n >= min_indent -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        // Stop at indicators that start new structures
        Some(lexer.Dash) | Some(lexer.Question) | Some(lexer.Colon) -> #(
          acc,
          parser,
        )
        // Comment after indent ends the scalar
        Some(lexer.Comment(_)) -> #(acc, parser)
        // Another Indent = blank line with leading spaces, add \n and continue
        Some(lexer.Indent(m)) if m >= min_indent ->
          collect_block_plain_value(after_indent, acc <> "\n", min_indent)
        // Plain text - check if it's a mapping key
        Some(lexer.Plain(s)) -> {
          case is_mapping_key(after_indent) {
            True -> #(acc, parser)
            False -> {
              // After blank line (acc ends with \n), don't add space
              let sep = case string.ends_with(acc, "\n") {
                True -> ""
                False -> " "
              }
              collect_block_plain_value(
                advance(after_indent),
                acc <> sep <> s,
                min_indent,
              )
            }
          }
        }
        // Tag at start of continuation line - treat as plain text
        Some(lexer.Tag(s)) -> {
          let sep = case string.ends_with(acc, "\n") {
            True -> ""
            False -> " "
          }
          collect_block_plain_value(
            advance(after_indent),
            acc <> sep <> s,
            min_indent,
          )
        }
        // Anchor at start of continuation line - treat as plain text
        Some(lexer.Anchor(s)) -> {
          let sep = case string.ends_with(acc, "\n") {
            True -> ""
            False -> " "
          }
          collect_block_plain_value(
            advance(after_indent),
            acc <> sep <> "&" <> s,
            min_indent,
          )
        }
        // Other tokens end the scalar
        _ -> #(acc, parser)
      }
    }
    // Value is complete
    _ -> #(acc, parser)
  }
}

/// Check for multiline continuation after a newline (at indent 0).
fn check_multiline_continuation(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  case current(parser) {
    // Another newline means a blank line - preserve as literal newline
    Some(lexer.Newline) -> {
      check_multiline_continuation(advance(parser), acc <> "\n", min_indent)
    }
    // Indent token after newline
    Some(lexer.Indent(n)) if n >= min_indent -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        // Stop at indicators that start new structures
        Some(lexer.Dash) | Some(lexer.Question) | Some(lexer.Colon) -> #(
          acc,
          Parser(..parser, pos: parser.pos - 1),
        )
        // Comment ends the scalar
        Some(lexer.Comment(_)) -> #(acc, Parser(..parser, pos: parser.pos - 1))
        // Another newline - blank line with indent (treat indent as part of blank line)
        Some(lexer.Newline) ->
          check_multiline_continuation(
            advance(after_indent),
            acc <> "\n",
            min_indent,
          )
        // Plain text - check if it's a mapping key (followed by colon)
        Some(lexer.Plain(s)) -> {
          // Look ahead to see if this plain is followed by colon (making it a mapping key)
          case is_mapping_key(after_indent) {
            True -> #(acc, Parser(..parser, pos: parser.pos - 1))
            False -> {
              // After a blank line (acc ends with \n), don't add extra space
              let sep = case string.ends_with(acc, "\n") {
                True -> ""
                False -> " "
              }
              collect_block_plain_value(
                advance(after_indent),
                acc <> sep <> s,
                min_indent,
              )
            }
          }
        }
        // Tag at start of continuation line - treat as plain text (not a real tag)
        Some(lexer.Tag(s)) -> {
          let sep = case string.ends_with(acc, "\n") {
            True -> ""
            False -> " "
          }
          collect_block_plain_value(
            advance(after_indent),
            acc <> sep <> s,
            min_indent,
          )
        }
        // Anchor at start of continuation line - treat as plain text (not a real anchor)
        Some(lexer.Anchor(s)) -> {
          let sep = case string.ends_with(acc, "\n") {
            True -> ""
            False -> " "
          }
          collect_block_plain_value(
            advance(after_indent),
            acc <> sep <> "&" <> s,
            min_indent,
          )
        }
        // Quoted strings followed by colon are mapping keys
        Some(lexer.SingleQuoted(_)) | Some(lexer.DoubleQuoted(_)) -> {
          case is_mapping_key(after_indent) {
            True -> #(acc, Parser(..parser, pos: parser.pos - 1))
            False -> #(acc, Parser(..parser, pos: parser.pos - 1))
          }
        }
        // Other tokens end the scalar
        _ -> #(acc, Parser(..parser, pos: parser.pos - 1))
      }
    }
    // Content at indent 0 (no Indent token) - only continue if min_indent == 0
    Some(lexer.Plain(s)) if min_indent == 0 -> {
      // Check if it's a mapping key
      case is_mapping_key(parser) {
        True -> #(acc, Parser(..parser, pos: parser.pos - 1))
        False -> {
          let sep = case string.ends_with(acc, "\n") {
            True -> ""
            False -> " "
          }
          collect_block_plain_value(
            advance(parser),
            acc <> sep <> s,
            min_indent,
          )
        }
      }
    }
    // Insufficient indent or no indent - scalar ends
    _ -> #(acc, Parser(..parser, pos: parser.pos - 1))
  }
}

/// Check if the current position starts a mapping key (scalar followed by colon).
fn is_mapping_key(parser: Parser) -> Bool {
  case current(parser) {
    Some(lexer.Plain(_))
    | Some(lexer.SingleQuoted(_))
    | Some(lexer.DoubleQuoted(_)) -> {
      // Advance past the scalar and check for colon
      let after_scalar = advance(parser)
      case current(after_scalar) {
        Some(lexer.Colon) -> True
        _ -> False
      }
    }
    _ -> False
  }
}
