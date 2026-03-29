//// Explicit mapping parsing (using ? for keys).

import gleam/dict.{type Dict}
import gleam/option.{Some}
import yaml/lexer
import yaml/parser/block
import yaml/parser/flow
import yaml/parser/helpers.{
  advance, current, skip_newlines_and_comments, skip_spaces, token_to_string,
}
import yaml/parser/scalar.{value_to_key_string, value_to_string}
import yaml/parser/types.{type ParseError, type Parser, ParseError, Parser}
import yaml/value.{type YamlValue}

/// Type alias for parse_value function to avoid circular imports.
pub type ParseValueFn =
  fn(Parser, Int) -> Result(#(YamlValue, Parser), ParseError)

/// Parse an explicit mapping (using ? for keys).
pub fn parse_explicit_mapping(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_explicit_mapping_items(parser, min_indent, dict.new(), parse_value_fn)
}

fn parse_explicit_mapping_items(
  parser: Parser,
  min_indent: Int,
  acc: Dict(String, YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)

  case current(parser) {
    Some(lexer.Question) -> {
      let parser = advance(parser) |> skip_spaces
      // Parse the key (can be complex value)
      case parse_explicit_key_value(parser, min_indent, parse_value_fn) {
        Ok(#(key, parser)) -> {
          // Look for : after the key
          let parser = skip_newlines_and_comments(parser)
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = advance(parser) |> skip_spaces
              // For explicit keys at indent 0, the value can also be at indent 0
              case parse_explicit_value(parser, min_indent, parse_value_fn) {
                Ok(#(val, parser)) -> {
                  let acc = dict.insert(acc, key, val)
                  parse_explicit_mapping_items(
                    parser,
                    min_indent,
                    acc,
                    parse_value_fn,
                  )
                }
                Error(e) -> Error(e)
              }
            }
            Some(lexer.Indent(n)) -> {
              let parser = advance(parser)
              case current(parser) {
                Some(lexer.Colon) -> {
                  let parser = advance(parser) |> skip_spaces
                  case parse_value_fn(parser, n + 1) {
                    Ok(#(val, parser)) -> {
                      let acc = dict.insert(acc, key, val)
                      parse_explicit_mapping_items(
                        parser,
                        min_indent,
                        acc,
                        parse_value_fn,
                      )
                    }
                    Error(e) -> Error(e)
                  }
                }
                // Check for another ? or implicit key at this indent
                Some(lexer.Question) -> {
                  let acc = dict.insert(acc, key, value.Null)
                  parse_explicit_mapping_items_at_indent(
                    parser,
                    min_indent,
                    n,
                    acc,
                    parse_value_fn,
                  )
                }
                Some(lexer.Plain(s)) -> {
                  // Could be next implicit key
                  let acc = dict.insert(acc, key, value.Null)
                  parse_mixed_mapping_from_plain(
                    parser,
                    s,
                    min_indent,
                    n,
                    acc,
                    parse_value_fn,
                  )
                }
                _ -> {
                  // No colon, key maps to null
                  let acc = dict.insert(acc, key, value.Null)
                  parse_explicit_mapping_items(
                    parser,
                    min_indent,
                    acc,
                    parse_value_fn,
                  )
                }
              }
            }
            // Check for ? at same indent level (another explicit key with no value)
            Some(lexer.Question) -> {
              let acc = dict.insert(acc, key, value.Null)
              parse_explicit_mapping_items(
                parser,
                min_indent,
                acc,
                parse_value_fn,
              )
            }
            _ -> {
              // No colon, key maps to null
              let acc = dict.insert(acc, key, value.Null)
              parse_explicit_mapping_items(
                parser,
                min_indent,
                acc,
                parse_value_fn,
              )
            }
          }
        }
        Error(e) -> Error(e)
      }
    }
    Some(lexer.Indent(n)) if n >= min_indent -> {
      parse_explicit_mapping_items_at_indent(
        parser,
        min_indent,
        n,
        acc,
        parse_value_fn,
      )
    }
    // Handle implicit keys at indent 0 (when min_indent == 0)
    Some(lexer.Plain(s)) if min_indent == 0 -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_value_fn(parser, 1) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, s, val)
              parse_explicit_mapping_items(
                parser,
                min_indent,
                acc,
                parse_value_fn,
              )
            }
            Error(e) -> Error(e)
          }
        }
        _ -> Ok(#(value.Mapping(acc), parser))
      }
    }
    Some(lexer.SingleQuoted(s)) if min_indent == 0 -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_value_fn(parser, 1) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, s, val)
              parse_explicit_mapping_items(
                parser,
                min_indent,
                acc,
                parse_value_fn,
              )
            }
            Error(e) -> Error(e)
          }
        }
        _ -> Ok(#(value.Mapping(acc), parser))
      }
    }
    Some(lexer.DoubleQuoted(s)) if min_indent == 0 -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_value_fn(parser, 1) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, s, val)
              parse_explicit_mapping_items(
                parser,
                min_indent,
                acc,
                parse_value_fn,
              )
            }
            Error(e) -> Error(e)
          }
        }
        _ -> Ok(#(value.Mapping(acc), parser))
      }
    }
    // Empty key entry (: value) at indent 0
    Some(lexer.Colon) if min_indent == 0 -> {
      let parser = advance(parser) |> skip_spaces
      case parse_value_fn(parser, 1) {
        Ok(#(val, parser)) -> {
          let acc = dict.insert(acc, "", val)
          parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
        }
        Error(e) -> Error(e)
      }
    }
    _ -> Ok(#(value.Mapping(acc), parser))
  }
}

fn parse_explicit_mapping_items_at_indent(
  parser: Parser,
  min_indent: Int,
  n: Int,
  acc: Dict(String, YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = advance(parser)
  case current(parser) {
    Some(lexer.Question) -> {
      let parser = advance(parser) |> skip_spaces
      case parse_explicit_key_value(parser, n, parse_value_fn) {
        Ok(#(key, parser)) -> {
          let parser = skip_newlines_and_comments(parser)
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = advance(parser) |> skip_spaces
              case parse_value_fn(parser, n + 1) {
                Ok(#(val, parser)) -> {
                  let acc = dict.insert(acc, key, val)
                  parse_explicit_mapping_items(
                    parser,
                    min_indent,
                    acc,
                    parse_value_fn,
                  )
                }
                Error(e) -> Error(e)
              }
            }
            Some(lexer.Indent(i)) -> {
              let parser = advance(parser)
              case current(parser) {
                Some(lexer.Colon) -> {
                  let parser = advance(parser) |> skip_spaces
                  case parse_value_fn(parser, i + 1) {
                    Ok(#(val, parser)) -> {
                      let acc = dict.insert(acc, key, val)
                      parse_explicit_mapping_items(
                        parser,
                        min_indent,
                        acc,
                        parse_value_fn,
                      )
                    }
                    Error(e) -> Error(e)
                  }
                }
                _ -> {
                  let acc = dict.insert(acc, key, value.Null)
                  parse_explicit_mapping_items(
                    parser,
                    min_indent,
                    acc,
                    parse_value_fn,
                  )
                }
              }
            }
            _ -> {
              let acc = dict.insert(acc, key, value.Null)
              parse_explicit_mapping_items(
                parser,
                min_indent,
                acc,
                parse_value_fn,
              )
            }
          }
        }
        Error(e) -> Error(e)
      }
    }
    // Handle implicit key (plain scalar followed by colon)
    Some(lexer.Plain(s)) -> {
      parse_mixed_mapping_from_plain(
        parser,
        s,
        min_indent,
        n,
        acc,
        parse_value_fn,
      )
    }
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) -> {
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_value_fn(parser, n + 1) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, s, val)
              parse_explicit_mapping_items(
                parser,
                min_indent,
                acc,
                parse_value_fn,
              )
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

fn parse_mixed_mapping_from_plain(
  parser: Parser,
  s: String,
  min_indent: Int,
  n: Int,
  acc: Dict(String, YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = advance(parser) |> skip_spaces
  case current(parser) {
    Some(lexer.Colon) -> {
      let parser = advance(parser) |> skip_spaces
      case parse_value_fn(parser, n + 1) {
        Ok(#(val, parser)) -> {
          let acc = dict.insert(acc, s, val)
          parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
        }
        Error(e) -> Error(e)
      }
    }
    _ -> Ok(#(value.Mapping(acc), parser))
  }
}

/// Collect multiline text for an explicit key.
/// Stops at `:` (value separator), `?` (another key), or when indent decreases.
fn collect_explicit_key_multiline(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  case current(parser) {
    // Plain text continuation on same line
    Some(lexer.Plain(s)) ->
      collect_explicit_key_multiline(
        advance(parser),
        acc <> " " <> s,
        min_indent,
      )
    // Newline - check for continuation
    Some(lexer.Newline) -> {
      let after_newline = advance(parser)
      case current(after_newline) {
        Some(lexer.Indent(n)) if n > min_indent -> {
          let after_indent = advance(after_newline)
          case current(after_indent) {
            // Colon or question means end of key
            Some(lexer.Colon) | Some(lexer.Question) -> #(acc, after_newline)
            // Plain text continuation
            Some(lexer.Plain(s)) ->
              collect_explicit_key_multiline(
                advance(after_indent),
                acc <> " " <> s,
                min_indent,
              )
            // Other - end of key
            _ -> #(acc, after_newline)
          }
        }
        // Indent at or below min_indent, or colon - end of key
        _ -> #(acc, parser)
      }
    }
    // Indent token (already past newline)
    Some(lexer.Indent(n)) if n > min_indent -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        // Colon or question means end of key
        Some(lexer.Colon) | Some(lexer.Question) -> #(acc, parser)
        // Plain text continuation
        Some(lexer.Plain(s)) ->
          collect_explicit_key_multiline(
            advance(after_indent),
            acc <> " " <> s,
            min_indent,
          )
        // Other - end of key
        _ -> #(acc, parser)
      }
    }
    // Colon, question, or comment ends the key
    Some(lexer.Colon) | Some(lexer.Question) | Some(lexer.Comment(_)) -> #(
      acc,
      parser,
    )
    // End of key
    _ -> #(acc, parser)
  }
}

/// Parse an explicit key value.
pub fn parse_explicit_key_value(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      let parser = advance(parser)
      // Check if this is an inline mapping (key: value format)
      case current(parser) {
        Some(lexer.Colon) -> {
          // This could be an inline mapping as the key (e.g., "earth: blue" in "? earth: blue")
          let parser = advance(parser) |> skip_spaces
          case current(parser) {
            // If there's a value after the colon on the same line, it's an inline mapping
            Some(lexer.Plain(v)) -> {
              // Parse as inline mapping: {s: v}
              let parser = advance(parser)
              // Check for more content (multiline or nested)
              let parser = skip_newlines_and_comments(parser)
              case current(parser) {
                // If followed by an indent and colon, the mapping is complete
                Some(lexer.Indent(_)) -> {
                  let key_str = "{" <> s <> ": " <> v <> "}"
                  Ok(#(key_str, parser))
                }
                // If followed by colon at same level, also complete
                Some(lexer.Colon) -> {
                  let key_str = "{" <> s <> ": " <> v <> "}"
                  Ok(#(key_str, parser))
                }
                _ -> {
                  let key_str = "{" <> s <> ": " <> v <> "}"
                  Ok(#(key_str, parser))
                }
              }
            }
            Some(lexer.SingleQuoted(v)) | Some(lexer.DoubleQuoted(v)) -> {
              let parser = advance(parser)
              let key_str = "{" <> s <> ": " <> v <> "}"
              Ok(#(key_str, parser))
            }
            // Empty value or value on next line - treat as simple key
            _ -> {
              // Not an inline mapping, backtrack - this is just the key with separator
              let #(full_key, parser) =
                collect_explicit_key_multiline(parser, s, min_indent)
              Ok(#(full_key, parser))
            }
          }
        }
        _ -> {
          // Collect potential multiline continuation
          let #(full_key, parser) =
            collect_explicit_key_multiline(parser, s, min_indent)
          Ok(#(full_key, parser))
        }
      }
    }
    Some(lexer.SingleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(s, advance(parser)))
    // Block scalar as key
    Some(lexer.Literal(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.Folded(s)) -> Ok(#(s, advance(parser)))
    // Block sequence as key
    Some(lexer.Dash) -> {
      case block.parse_block_sequence(parser, min_indent, parse_value_fn) {
        Ok(#(seq_val, parser)) -> Ok(#(value_to_key_string(seq_val), parser))
        Error(e) -> Error(e)
      }
    }
    // Flow sequence as key
    Some(lexer.BracketOpen) -> {
      case flow.parse_flow_sequence(advance(parser)) {
        Ok(#(seq_val, parser)) -> Ok(#(value_to_key_string(seq_val), parser))
        Error(e) -> Error(e)
      }
    }
    // Flow mapping as key
    Some(lexer.BraceOpen) -> {
      case flow.parse_flow_mapping(advance(parser)) {
        Ok(#(map_val, parser)) -> Ok(#(value_to_key_string(map_val), parser))
        Error(e) -> Error(e)
      }
    }
    // Anchor before key
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_spaces
      case parse_explicit_key_value(parser, min_indent, parse_value_fn) {
        Ok(#(key, parser)) -> {
          let parser =
            Parser(
              ..parser,
              anchors: dict.insert(parser.anchors, name, value.String(key)),
            )
          Ok(#(key, parser))
        }
        Error(e) -> Error(e)
      }
    }
    // Alias as key
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(value_to_string(val), parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    // Tag before key (skip it)
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser) |> skip_spaces
      parse_explicit_key_value(parser, min_indent, parse_value_fn)
    }
    // Newline after ? - look for content on next line
    Some(lexer.Newline) -> {
      let parser = advance(parser)
      case current(parser) {
        // Sequence at indent 0 as the key
        Some(lexer.Dash) -> {
          case block.parse_block_sequence(parser, 0, parse_value_fn) {
            Ok(#(seq_val, parser)) ->
              Ok(#(value_to_key_string(seq_val), parser))
            Error(e) -> Error(e)
          }
        }
        // Indented content
        Some(lexer.Indent(n)) -> {
          let after_indent = advance(parser)
          case current(after_indent) {
            Some(lexer.Dash) -> {
              let parser = Parser(..after_indent, pos: after_indent.pos - 1)
              case block.parse_block_sequence(parser, n, parse_value_fn) {
                Ok(#(seq_val, parser)) ->
                  Ok(#(value_to_key_string(seq_val), parser))
                Error(e) -> Error(e)
              }
            }
            // Other indented content - parse as key value
            _ ->
              parse_explicit_key_value(after_indent, min_indent, parse_value_fn)
          }
        }
        // Colon immediately after newline - empty key
        Some(lexer.Colon) -> Ok(#("", parser))
        // Other - empty key
        _ -> Ok(#("", parser))
      }
    }
    // Indent token (already past newline)
    Some(lexer.Indent(n)) -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        Some(lexer.Dash) -> {
          let parser = Parser(..after_indent, pos: after_indent.pos - 1)
          case block.parse_block_sequence(parser, n, parse_value_fn) {
            Ok(#(seq_val, parser)) ->
              Ok(#(value_to_key_string(seq_val), parser))
            Error(e) -> Error(e)
          }
        }
        Some(lexer.Colon) -> Ok(#("", parser))
        _ -> parse_explicit_key_value(after_indent, min_indent, parse_value_fn)
      }
    }
    Some(lexer.Colon) -> Ok(#("", parser))
    option.None -> Ok(#("", parser))
    Some(lexer.Eof) -> Ok(#("", parser))
    Some(lexer.Comment(_)) -> {
      let parser = advance(parser)
      parse_explicit_key_value(parser, min_indent, parse_value_fn)
    }
    Some(token) ->
      Error(ParseError(
        "Expected explicit key value, got " <> token_to_string(token),
        parser.pos,
      ))
  }
}

/// Parse the value after : in an explicit mapping.
/// Unlike regular mapping values, explicit mapping values can be at the same indent.
fn parse_explicit_value(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    // Newline after : - look for value on next line
    Some(lexer.Newline) -> {
      let parser = advance(parser)
      case current(parser) {
        // Sequence at indent 0 as the value
        Some(lexer.Dash) -> {
          block.parse_block_sequence(parser, 0, parse_value_fn)
        }
        // Indented content
        Some(lexer.Indent(n)) -> {
          let after_indent = advance(parser)
          case current(after_indent) {
            Some(lexer.Dash) -> {
              let parser = Parser(..after_indent, pos: after_indent.pos - 1)
              block.parse_block_sequence(parser, n, parse_value_fn)
            }
            _ -> {
              let parser = Parser(..after_indent, pos: after_indent.pos - 1)
              parse_value_fn(parser, n)
            }
          }
        }
        // Other - null value
        _ -> Ok(#(value.Null, parser))
      }
    }
    // Direct content after : (same line)
    _ -> parse_value_fn(parser, min_indent + 1)
  }
}
