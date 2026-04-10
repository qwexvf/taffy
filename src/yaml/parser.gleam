//// YAML parser - parses tokens into YAML values.

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import yaml/lexer.{type Token}
import yaml/parser/block
import yaml/parser/explicit
import yaml/parser/flow
import yaml/parser/helpers.{
  advance, current, skip_newlines_and_comments, skip_spaces, token_to_string,
}
import yaml/parser/scalar
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

/// Parses all documents in a YAML stream.
pub fn parse_all(tokens: List(Token)) -> Result(List(YamlValue), ParseError) {
  let parser = new(tokens)
  // Consume and validate directives at the start
  use parser <- result.try(consume_directives(parser))
  parse_documents(parser, [])
}

/// Validate a directive's content.
fn validate_directive(
  content: String,
  is_yaml: Bool,
  pos: Int,
) -> Result(Nil, ParseError) {
  case is_yaml {
    True -> {
      // Split at first valid comment (space + #)
      let before_comment = case string.split_once(content, " #") {
        Ok(#(before, _)) -> string.trim(before)
        Error(_) -> string.trim(content)
      }
      // Check for # without preceding space (invalid comment)
      let has_invalid_hash = string.contains(before_comment, "#")
      // After version should be just "YAML X.Y" (2 words max)
      let parts =
        string.split(before_comment, " ")
        |> list.filter(fn(p) { p != "" })
      let too_many_parts = list.length(parts) > 2
      case has_invalid_hash || too_many_parts {
        True -> Error(ParseError("Invalid %YAML directive: %" <> content, pos))
        False -> Ok(Nil)
      }
    }
    False -> Ok(Nil)
  }
}

/// Consume YAML directives (%YAML, %TAG) before a document.
/// Validates that directives are followed by a document start marker.
/// Also records TAG handles for the current document.
fn consume_directives(parser: Parser) -> Result(Parser, ParseError) {
  // Reset tag handles at the start of each document's directives
  let parser = Parser(..parser, tag_handles: [])
  consume_directives_loop(parser)
}

fn consume_directives_loop(parser: Parser) -> Result(Parser, ParseError) {
  let parser = skip_newlines_and_comments(parser)
  case current(parser) {
    Some(lexer.Directive(d)) -> {
      let is_yaml_dir = string.starts_with(d, "YAML")
      let is_tag_dir = string.starts_with(d, "TAG")
      // Validate %YAML directive format
      use _ <- result.try(validate_directive(d, is_yaml_dir, parser.pos))
      // Record TAG handle
      let parser = case is_tag_dir {
        True -> {
          let handle = extract_tag_handle(d)
          Parser(..parser, tag_handles: [handle, ..parser.tag_handles])
        }
        False -> parser
      }
      let parser = advance(parser)
      let parser = skip_newlines_and_comments(parser)
      // After directives, must have --- or more directives
      case current(parser) {
        Some(lexer.DocStart) -> Ok(parser)
        Some(lexer.Directive(d2)) -> {
          case is_yaml_dir && string.starts_with(d2, "YAML") {
            True -> Error(ParseError("Duplicate %YAML directive", parser.pos))
            False -> consume_directives_loop(parser)
          }
        }
        Some(lexer.DocEnd) ->
          Error(ParseError(
            "Directive must be followed by document start '---'",
            parser.pos,
          ))
        None | Some(lexer.Eof) ->
          Error(ParseError(
            "Directive must be followed by document start '---'",
            parser.pos,
          ))
        _ -> Ok(parser)
      }
    }
    _ -> Ok(parser)
  }
}

/// Extract the tag handle from a TAG directive content.
/// e.g., "TAG !prefix! tag:example.com,2011:" → "!prefix!"
fn extract_tag_handle(directive_content: String) -> String {
  // Skip "TAG " prefix
  let after_tag = string.drop_start(directive_content, 4)
  let trimmed = string.trim_start(after_tag)
  // Handle is everything up to the next space
  case string.split_once(trimmed, " ") {
    Ok(#(handle, _)) -> handle
    Error(_) -> trimmed
  }
}

/// Validate that a tag's handle (e.g., !prefix! in !prefix!A) is defined.
/// Default handles ! and !! are always valid. Custom handles need %TAG directives.
fn validate_tag_handle(tag: String, parser: Parser) -> Result(Nil, ParseError) {
  // Verbatim tags (!<...>) are always valid
  case string.starts_with(tag, "!<") {
    True -> Ok(Nil)
    False -> {
      // Non-specific tag ! is always valid
      case tag == "!" {
        True -> Ok(Nil)
        False -> {
          // !! (secondary handle) is always valid
          case string.starts_with(tag, "!!") {
            True -> Ok(Nil)
            False -> {
              // Check for custom handle pattern: !handle!suffix
              // A custom handle starts with ! and contains another !
              case extract_custom_handle(tag) {
                option.None -> Ok(Nil)
                option.Some(handle) -> {
                  case list.contains(parser.tag_handles, handle) {
                    True -> Ok(Nil)
                    False ->
                      Error(ParseError(
                        "Undefined tag handle: " <> handle,
                        parser.pos,
                      ))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Extract a custom tag handle from a tag string.
/// e.g., "!prefix!A" → Some("!prefix!"), "!foo" → None, "!!str" → None
fn extract_custom_handle(tag: String) -> option.Option(String) {
  // Must start with ! and have another ! somewhere after
  case string.starts_with(tag, "!") {
    False -> option.None
    True -> {
      // Find the second ! (after the first character)
      let rest = string.drop_start(tag, 1)
      case string.split_once(rest, "!") {
        Ok(#(handle_body, _suffix)) -> option.Some("!" <> handle_body <> "!")
        Error(_) -> option.None
      }
    }
  }
}

/// Check for invalid anchor + mapping key on same line as `---`.
/// `--- &anchor key: value` is ambiguous and should be rejected.
fn check_doc_start_anchor(parser: Parser) -> Result(Nil, ParseError) {
  case current(parser) {
    // Anchor right after --- on same line
    Some(lexer.Anchor(_)) -> {
      let after_anchor = advance(parser) |> skip_spaces
      case current(after_anchor) {
        // Anchor followed by scalar that might be a mapping key
        Some(lexer.Plain(_)) -> {
          let after_plain = advance(after_anchor) |> skip_spaces
          case current(after_plain) {
            Some(lexer.Colon) ->
              Error(ParseError(
                "Anchor before mapping on document start line is ambiguous",
                parser.pos,
              ))
            _ -> Ok(Nil)
          }
        }
        _ -> Ok(Nil)
      }
    }
    _ -> Ok(Nil)
  }
}

/// Check that there's no content on the same line as `...`.
fn check_after_doc_end(parser: Parser) -> Result(Parser, ParseError) {
  case current(parser) {
    Some(lexer.Newline)
    | Some(lexer.Indent(_))
    | Some(lexer.Comment(_))
    | Some(lexer.Eof)
    | None -> Ok(parser)
    Some(_) ->
      Error(ParseError(
        "Invalid content on same line as document end marker '...'",
        parser.pos,
      ))
  }
}

fn parse_documents(
  parser: Parser,
  acc: List(YamlValue),
) -> Result(List(YamlValue), ParseError) {
  let parser = skip_newlines_and_comments(parser)
  case current(parser) {
    None | Some(lexer.Eof) -> Ok(list.reverse(acc))
    Some(lexer.DocStart) -> {
      let after_doc_start = advance(parser)
      // Check for invalid anchor + mapping on same line as ---
      use _ <- result.try(check_doc_start_anchor(after_doc_start))
      let parser = skip_newlines_and_comments(after_doc_start)
      case current(parser) {
        // Empty document followed by another doc start or doc end
        Some(lexer.DocStart) ->
          parse_documents(Parser(..parser, tag_handles: []), [value.Null, ..acc])
        Some(lexer.DocEnd) -> {
          let parser = advance(parser)
          use parser <- result.try(check_after_doc_end(parser))
          let parser = skip_newlines_and_comments(parser)
          use parser <- result.try(consume_directives(parser))
          parse_documents(parser, [value.Null, ..acc])
        }
        None | Some(lexer.Eof) -> Ok(list.reverse([value.Null, ..acc]))
        _ -> {
          use #(val, parser) <- result.try(parse_value(parser, 0))
          let parser = skip_newlines_and_comments(parser)
          case current(parser) {
            Some(lexer.DocEnd) -> {
              let parser = advance(parser)
              use parser <- result.try(check_after_doc_end(parser))
              let parser = skip_newlines_and_comments(parser)
              use parser <- result.try(consume_directives(parser))
              parse_documents(parser, [val, ..acc])
            }
            Some(lexer.DocStart) ->
              parse_documents(Parser(..parser, tag_handles: []), [val, ..acc])
            None | Some(lexer.Eof) -> parse_documents(parser, [val, ..acc])
            Some(token) ->
              Error(ParseError(
                "Unexpected trailing content: " <> token_to_string(token),
                parser.pos,
              ))
          }
        }
      }
    }
    Some(lexer.DocEnd) -> {
      let parser = advance(parser)
      use parser <- result.try(check_after_doc_end(parser))
      let parser = skip_newlines_and_comments(parser)
      use parser <- result.try(consume_directives(parser))
      parse_documents(parser, acc)
    }
    // Directive between documents - consume and continue
    Some(lexer.Directive(_)) -> {
      use parser <- result.try(consume_directives(parser))
      parse_documents(parser, acc)
    }
    _ -> {
      // Bare document (no --- prefix)
      use #(val, parser) <- result.try(parse_value(parser, 0))
      let parser = skip_newlines_and_comments(parser)
      case current(parser) {
        Some(lexer.DocEnd) -> {
          let parser = advance(parser)
          use parser <- result.try(check_after_doc_end(parser))
          let parser = skip_newlines_and_comments(parser)
          use parser <- result.try(consume_directives(parser))
          parse_documents(parser, [val, ..acc])
        }
        Some(lexer.DocStart) ->
          parse_documents(Parser(..parser, tag_handles: []), [val, ..acc])
        None | Some(lexer.Eof) -> Ok(list.reverse([val, ..acc]))
        Some(token) ->
          Error(ParseError(
            "Unexpected trailing content: " <> token_to_string(token),
            parser.pos,
          ))
      }
    }
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
      let parser = Parser(..advance(parser), in_inline_value: False)
      case min_indent > 0 {
        True -> Ok(#(value.Null, parser))
        False -> parse_value(parser, min_indent)
      }
    }

    None -> Ok(#(value.Null, parser))
    Some(lexer.Eof) -> Ok(#(value.Null, parser))
    Some(lexer.DocEnd) -> Ok(#(value.Null, parser))
    // Directive - treat as document boundary (stop parsing current value)
    Some(lexer.Directive(_)) -> Ok(#(value.Null, parser))
    // Another document starting - current document is empty
    Some(lexer.DocStart) -> Ok(#(value.Null, parser))

    // Anchor
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_spaces
      // Check if this anchor is on a mapping key (scalar followed by colon)
      case current(parser) {
        // Reject two anchors on the same node
        Some(lexer.Anchor(_)) ->
          Error(ParseError("A node can only have one anchor", parser.pos))
        // Reject anchor on an alias (can't anchor an alias reference)
        Some(lexer.Alias(_)) ->
          Error(ParseError("Cannot place an anchor on an alias", parser.pos))
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
                  anchors: dict.insert(
                    parser.anchors,
                    name,
                    scalar.parse_scalar(s),
                  ),
                )
              Ok(#(scalar.parse_scalar(s), parser))
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
        // Block sequence indicator right after anchor on same line is invalid
        Some(lexer.Dash) ->
          Error(ParseError(
            "Block sequence not allowed on same line as anchor",
            parser.pos,
          ))
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

    // Alias - might be a value or a mapping key
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser) |> skip_spaces
      case dict.get(parser.anchors, name) {
        Ok(alias_val) -> {
          // Check if this alias is used as a mapping key
          case current(parser) {
            Some(lexer.Colon) -> {
              // Alias is a mapping key
              let key = scalar.value_to_key_string(alias_val)
              block.parse_block_mapping_from_key(
                key,
                advance(parser),
                min_indent,
                parse_value,
              )
            }
            _ -> Ok(#(alias_val, parser))
          }
        }
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }

    // Handle tags - check if it's a string tag with truly no value
    Some(lexer.Tag(tag)) -> {
      // Validate tag handle is defined
      use _ <- result.try(validate_tag_handle(tag, parser))
      let parser = advance(parser) |> skip_spaces
      let is_str_tag =
        tag == "!!str" || string.contains(tag, "tag:yaml.org,2002:str")
      // Non-specific tag `!` means "don't do type resolution" (keep as string)
      let is_non_specific = tag == "!"
      case current(parser), is_str_tag, is_non_specific {
        // String tag at end of input = empty string
        Some(lexer.Eof), True, _ | None, True, _ ->
          Ok(#(value.String(""), parser))
        // String tag followed by newline, check if value continues
        Some(lexer.Newline), True, _ -> {
          let after_newline = advance(parser)
          case current(after_newline) {
            // End of input or document marker after newline = empty string
            Some(lexer.Eof) | None | Some(lexer.DocStart) | Some(lexer.DocEnd) ->
              Ok(#(value.String(""), parser))
            // Dash at indent 0 means new sequence item, so this value is empty
            Some(lexer.Dash) -> Ok(#(value.String(""), parser))
            // Content continues on next line, parse normally
            _ -> parse_value(parser, min_indent)
          }
        }
        // Non-specific tag followed by plain scalar - keep as string
        Some(lexer.Plain(s)), _, True -> {
          Ok(#(value.String(string.trim(s)), advance(parser)))
        }
        // Otherwise, parse the value normally
        _, _, _ -> parse_value(parser, min_indent)
      }
    }

    // Flow sequence - might be a mapping key
    Some(lexer.BracketOpen) -> {
      let parser =
        Parser(
          ..advance(parser),
          flow_min_indent: min_indent,
          flow_multiline: False,
        )
      case flow.parse_flow_sequence(parser) {
        Ok(#(seq_val, parser)) -> {
          let was_multiline = parser.flow_multiline
          let parser = skip_spaces(parser)
          case current(parser) {
            // Flow sequence used as mapping key - reject if multiline
            Some(lexer.Colon) -> {
              case was_multiline {
                True ->
                  Error(ParseError(
                    "Multiline flow sequence cannot be used as implicit mapping key",
                    parser.pos,
                  ))
                False -> {
                  let key = scalar.value_to_key_string(seq_val)
                  block.parse_block_mapping_from_key(
                    key,
                    advance(parser),
                    min_indent,
                    parse_value,
                  )
                }
              }
            }
            // After flow collection in block context, reject inline scalars
            Some(lexer.Plain(_))
            | Some(lexer.SingleQuoted(_))
            | Some(lexer.DoubleQuoted(_))
            | Some(lexer.Dash) ->
              Error(ParseError(
                "Invalid content after flow sequence",
                parser.pos,
              ))
            _ -> Ok(#(seq_val, parser))
          }
        }
        Error(e) -> Error(e)
      }
    }

    // Flow mapping - might be a mapping key
    Some(lexer.BraceOpen) -> {
      let parser = Parser(..advance(parser), flow_min_indent: min_indent)
      case flow.parse_flow_mapping(parser) {
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
            // After flow collection in block context, reject inline scalars
            Some(lexer.Plain(_))
            | Some(lexer.SingleQuoted(_))
            | Some(lexer.DoubleQuoted(_))
            | Some(lexer.Dash) ->
              Error(ParseError("Invalid content after flow mapping", parser.pos))
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
          Ok(#(scalar.parse_scalar(":" <> rest), parser))
        }
        // Tags after colon - still a mapping value
        Some(lexer.Tag(_))
        | Some(lexer.Question)
        | Some(lexer.Literal(_))
        | Some(lexer.Folded(_))
        | Some(lexer.DocStart)
        | Some(lexer.DocEnd)
        | Some(lexer.Directive(_)) ->
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
            | None -> {
              // Reject nested implicit mappings on same line as parent value
              case
                parser.in_inline_value
                && is_inline_value_token(current(after_colon))
              {
                True ->
                  Error(ParseError(
                    "Implicit mapping not allowed as inline mapping value",
                    parser.pos,
                  ))
                False ->
                  block.parse_block_mapping_from_key(
                    full_key,
                    after_colon,
                    min_indent,
                    parse_value,
                  )
              }
            }
            // Colon followed by non-whitespace = continue collecting
            _ -> {
              let #(rest, parser) =
                collect_block_plain_value(
                  after_colon,
                  full_key <> ":",
                  min_indent,
                )
              Ok(#(scalar.parse_scalar(rest), parser))
            }
          }
        }
        // Not a mapping key - check for multiline scalar continuation
        Some(lexer.Newline) -> {
          // A Newline token followed by Indent means there was a blank line
          // (the Newline came from a line with only whitespace)
          // We need to preserve this as \n in the scalar
          let after_newline = advance(parser)
          let acc = case current(after_newline) {
            // Blank line before indented content - preserve newline
            Some(lexer.Indent(_)) -> full_key <> "\n"
            // Regular line break - will be folded to space
            _ -> full_key
          }
          let #(full_value, parser) =
            check_multiline_continuation(after_newline, acc, min_indent)
          Ok(#(scalar.parse_scalar(full_value), parser))
        }
        Some(lexer.Indent(n)) if n >= min_indent -> {
          let #(full_value, parser) =
            collect_block_plain_value(parser, full_key, min_indent)
          Ok(#(scalar.parse_scalar(full_value), parser))
        }
        _ -> Ok(#(scalar.parse_scalar(full_key), parser))
      }
    }

    Some(lexer.SingleQuoted(s)) -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) -> {
          let after_colon = advance(parser)
          case
            parser.in_inline_value
            && is_inline_value_token(current(after_colon))
          {
            True ->
              Error(ParseError(
                "Implicit mapping not allowed as inline mapping value",
                parser.pos,
              ))
            False ->
              block.parse_block_mapping_from_key(
                s,
                after_colon,
                min_indent,
                parse_value,
              )
          }
        }
        Some(lexer.Plain(_)) ->
          Error(ParseError("Invalid content after quoted value", parser.pos))
        _ -> Ok(#(value.String(s), parser))
      }
    }

    Some(lexer.DoubleQuoted(s)) -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) -> {
          let after_colon = advance(parser)
          case
            parser.in_inline_value
            && is_inline_value_token(current(after_colon))
          {
            True ->
              Error(ParseError(
                "Implicit mapping not allowed as inline mapping value",
                parser.pos,
              ))
            False ->
              block.parse_block_mapping_from_key(
                s,
                after_colon,
                min_indent,
                parse_value,
              )
          }
        }
        Some(lexer.Plain(_)) ->
          Error(ParseError("Invalid content after quoted value", parser.pos))
        _ -> Ok(#(value.String(s), parser))
      }
    }

    Some(lexer.Indent(n)) if n >= min_indent -> {
      let parser = Parser(..advance(parser), in_inline_value: False)
      case current(parser) {
        Some(lexer.Dash) ->
          case is_sequence_dash(parser, n) {
            True -> block.parse_block_sequence(parser, n, parse_value)
            False -> parse_value(parser, min_indent)
          }
        Some(lexer.Plain(_))
        | Some(lexer.SingleQuoted(_))
        | Some(lexer.DoubleQuoted(_)) ->
          parse_value_after_indent(parser, n, min_indent)
        _ -> parse_value(parser, min_indent)
      }
    }

    Some(lexer.Indent(_)) -> Ok(#(value.Null, parser))

    Some(token) ->
      Error(ParseError(
        "Unexpected token: " <> token_to_string(token),
        parser.pos,
      ))
  }
}

/// Check whether a Dash at indent n should be treated as a sequence entry
/// or as plain text. Returns True when the dash is a valid sequence entry.
fn is_sequence_dash(parser: Parser, n: Int) -> Bool {
  case parser.seq_entry_indent {
    // If we know the parent sequence's entry indent, the dash must match it
    option.Some(sei) if n > sei && n == 0 -> True
    option.Some(sei) if n == sei -> True
    option.Some(_) -> False
    // No parent sequence context - always treat as sequence
    option.None -> True
  }
}

/// Check whether a Dash at indent n in scalar continuation should be absorbed
/// as plain text rather than terminating the scalar.
fn should_absorb_dash_in_scalar(
  seq_entry_indent: option.Option(Int),
  n: Int,
  min_indent: Int,
) -> Bool {
  case seq_entry_indent {
    option.Some(sei) -> n > sei && n == min_indent
    option.None -> False
  }
}

/// Parse value after seeing an Indent(n) token.
/// For mapping keys at this indent, uses n as min_indent so continuation
/// pairs must be at the same indent level.
fn parse_value_after_indent(
  parser: Parser,
  n: Int,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      let #(full_key, kparser) = collect_block_plain_key(advance(parser), s)
      let kparser = skip_spaces(kparser)
      case current(kparser) {
        Some(lexer.Colon) -> {
          let after_colon = advance(kparser)
          // Verify this is a key separator (colon followed by value/whitespace)
          case current(after_colon) {
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
              block.parse_block_mapping_from_key_col(
                full_key,
                after_colon,
                n,
                Some(n),
                parse_value,
              )
            _ -> parse_value(parser, min_indent)
          }
        }
        _ -> parse_value(parser, min_indent)
      }
    }
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) -> {
      let kp = advance(parser) |> skip_spaces
      case current(kp) {
        Some(lexer.Colon) ->
          block.parse_block_mapping_from_key_col(
            s,
            advance(kp),
            n,
            Some(n),
            parse_value,
          )
        _ -> parse_value(parser, min_indent)
      }
    }
    _ -> parse_value(parser, min_indent)
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
        // Dash: check if it should be absorbed as plain text or terminate
        Some(lexer.Dash) ->
          case
            should_absorb_dash_in_scalar(parser.seq_entry_indent, n, min_indent)
          {
            True -> {
              let sep = case string.ends_with(acc, "\n") {
                True -> ""
                False -> " "
              }
              collect_block_plain_value(
                advance(after_indent),
                acc <> sep <> "-",
                min_indent,
              )
            }
            False -> #(acc, parser)
          }
        // ? and : at indent level start new structures - stop collecting
        Some(lexer.Question) | Some(lexer.Colon) -> #(acc, parser)
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

/// Check if a token represents an inline value (not newline/indent/comment/eof).
fn is_inline_value_token(token: option.Option(lexer.Token)) -> Bool {
  case token {
    Some(lexer.Newline)
    | Some(lexer.Indent(_))
    | Some(lexer.Comment(_))
    | Some(lexer.Eof)
    | None -> False
    _ -> True
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
