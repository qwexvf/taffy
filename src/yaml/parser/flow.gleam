//// Flow collection parsing (sequences and mappings in [] and {} syntax).

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{Some}
import gleam/string
import yaml/lexer
import yaml/parser/helpers.{advance, current, skip_whitespace}
import yaml/parser/scalar.{parse_scalar, value_to_key_string}
import yaml/parser/types.{type ParseError, type Parser, ParseError, Parser}
import yaml/value.{type YamlValue}

/// Parse a flow sequence.
pub fn parse_flow_sequence(
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
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
    Some(lexer.Eof) ->
      Error(ParseError("Unterminated flow sequence", parser.pos))
    option.None -> Error(ParseError("Unterminated flow sequence", parser.pos))
    // Explicit key in flow sequence starts a single-pair mapping
    Some(lexer.Question) -> {
      let parser = skip_whitespace(advance(parser))
      case parse_flow_sequence_single_pair(parser, True) {
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
    _ -> {
      case parse_flow_sequence_entry(parser) {
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

/// Parse a flow sequence entry, which might be a single-pair implicit mapping.
fn parse_flow_sequence_entry(
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    // Nested sequence - check if it's used as a key
    Some(lexer.BracketOpen) -> {
      case parse_flow_sequence(advance(parser)) {
        Ok(#(seq_val, parser)) -> {
          // Check if this sequence is a key in a single-pair mapping
          let parser = skip_whitespace(parser)
          case current(parser) {
            Some(lexer.Colon) -> {
              // This is a single-pair mapping with sequence as key
              let key = value_to_key_string(seq_val)
              let parser = skip_whitespace(advance(parser))
              case parse_flow_value(parser) {
                Ok(#(val, parser)) -> {
                  let mapping = dict.from_list([#(key, val)])
                  Ok(#(value.Mapping(mapping), parser))
                }
                Error(e) -> Error(e)
              }
            }
            // Adjacent colon: [key]:value
            Some(lexer.Plain(p)) -> {
              case string.starts_with(p, ":") {
                True -> {
                  let key = value_to_key_string(seq_val)
                  let val_str = string.drop_start(p, 1)
                  let val = case val_str {
                    "" -> value.Null
                    _ -> parse_scalar(val_str)
                  }
                  let mapping = dict.from_list([#(key, val)])
                  Ok(#(value.Mapping(mapping), advance(parser)))
                }
                False -> Ok(#(seq_val, parser))
              }
            }
            _ -> Ok(#(seq_val, parser))
          }
        }
        Error(e) -> Error(e)
      }
    }
    // Nested mapping - check if it's used as a key
    Some(lexer.BraceOpen) -> {
      case parse_flow_mapping(advance(parser)) {
        Ok(#(map_val, parser)) -> {
          // Check if this mapping is a key in a single-pair mapping
          let parser = skip_whitespace(parser)
          case current(parser) {
            Some(lexer.Colon) -> {
              // This is a single-pair mapping with mapping as key
              let key = value_to_key_string(map_val)
              let parser = skip_whitespace(advance(parser))
              case parse_flow_value(parser) {
                Ok(#(val, parser)) -> {
                  let mapping = dict.from_list([#(key, val)])
                  Ok(#(value.Mapping(mapping), parser))
                }
                Error(e) -> Error(e)
              }
            }
            // Adjacent colon: {key: val}:value
            Some(lexer.Plain(p)) -> {
              case string.starts_with(p, ":") {
                True -> {
                  let key = value_to_key_string(map_val)
                  let val_str = string.drop_start(p, 1)
                  let val = case val_str {
                    "" -> value.Null
                    _ -> parse_scalar(val_str)
                  }
                  let mapping = dict.from_list([#(key, val)])
                  Ok(#(value.Mapping(mapping), advance(parser)))
                }
                False -> Ok(#(map_val, parser))
              }
            }
            _ -> Ok(#(map_val, parser))
          }
        }
        Error(e) -> Error(e)
      }
    }
    // Check for implicit single-pair mapping (key: value) or multiline plain scalar
    Some(lexer.Plain(s)) -> {
      let parser = advance(parser)
      // Collect any multiline continuation parts
      case collect_flow_plain_scalar_parts(parser, s) {
        #(full_scalar, parser) -> {
          let parser = skip_whitespace(parser)
          case current(parser) {
            Some(lexer.Colon) -> {
              // This is a single-pair implicit mapping
              let parser = skip_whitespace(advance(parser))
              case parse_flow_value(parser) {
                Ok(#(val, parser)) -> {
                  let mapping = dict.from_list([#(full_scalar, val)])
                  Ok(#(value.Mapping(mapping), parser))
                }
                Error(e) -> Error(e)
              }
            }
            _ -> Ok(#(parse_scalar(full_scalar), parser))
          }
        }
      }
    }
    Some(lexer.SingleQuoted(s)) -> {
      let parser = advance(parser)
      let parser = skip_whitespace(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = skip_whitespace(advance(parser))
          case parse_flow_value(parser) {
            Ok(#(val, parser)) -> {
              let mapping = dict.from_list([#(s, val)])
              Ok(#(value.Mapping(mapping), parser))
            }
            Error(e) -> Error(e)
          }
        }
        // Adjacent colon: 'key':value
        Some(lexer.Plain(p)) -> {
          case string.starts_with(p, ":") {
            True -> {
              let val_str = string.drop_start(p, 1)
              let val = case val_str {
                "" -> value.Null
                _ -> parse_scalar(val_str)
              }
              let mapping = dict.from_list([#(s, val)])
              Ok(#(value.Mapping(mapping), advance(parser)))
            }
            False -> Ok(#(value.String(s), parser))
          }
        }
        _ -> Ok(#(value.String(s), parser))
      }
    }
    Some(lexer.DoubleQuoted(s)) -> {
      let parser = advance(parser)
      let parser = skip_whitespace(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = skip_whitespace(advance(parser))
          case parse_flow_value(parser) {
            Ok(#(val, parser)) -> {
              let mapping = dict.from_list([#(s, val)])
              Ok(#(value.Mapping(mapping), parser))
            }
            Error(e) -> Error(e)
          }
        }
        // Adjacent colon: "key":value
        Some(lexer.Plain(p)) -> {
          case string.starts_with(p, ":") {
            True -> {
              let val_str = string.drop_start(p, 1)
              let val = case val_str {
                "" -> value.Null
                _ -> parse_scalar(val_str)
              }
              let mapping = dict.from_list([#(s, val)])
              Ok(#(value.Mapping(mapping), advance(parser)))
            }
            False -> Ok(#(value.String(s), parser))
          }
        }
        _ -> Ok(#(value.String(s), parser))
      }
    }
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_whitespace
      case parse_flow_sequence_entry(parser) {
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
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser) |> skip_whitespace
      parse_flow_sequence_entry(parser)
    }
    // Empty key followed by value - single-pair implicit mapping like [: b]
    Some(lexer.Colon) -> {
      let parser = skip_whitespace(advance(parser))
      case parse_flow_value(parser) {
        Ok(#(val, parser)) -> {
          let mapping = dict.from_list([#("", val)])
          Ok(#(value.Mapping(mapping), parser))
        }
        Error(e) -> Error(e)
      }
    }
    // Empty entry (null)
    Some(lexer.Comma) -> Ok(#(value.Null, parser))
    Some(lexer.BracketClose) -> Ok(#(value.Null, parser))
    _ -> Ok(#(value.Null, parser))
  }
}

/// Parse a single-pair mapping in a flow sequence (after seeing ?).
fn parse_flow_sequence_single_pair(
  parser: Parser,
  _saw_question: Bool,
) -> Result(#(YamlValue, Parser), ParseError) {
  // Parse the key (may be multiline in flow context)
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      let parser = advance(parser)
      // Collect multiline continuation parts
      let #(full_key, parser) = collect_flow_plain_scalar_parts(parser, s)
      let parser = skip_whitespace(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = skip_whitespace(advance(parser))
          case parse_flow_value(parser) {
            Ok(#(val, parser)) -> {
              let mapping = dict.from_list([#(full_key, val)])
              Ok(#(value.Mapping(mapping), parser))
            }
            Error(e) -> Error(e)
          }
        }
        _ -> {
          // Key with no value
          let mapping = dict.from_list([#(full_key, value.Null)])
          Ok(#(value.Mapping(mapping), parser))
        }
      }
    }
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) -> {
      let parser = skip_whitespace(advance(parser))
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = skip_whitespace(advance(parser))
          case parse_flow_value(parser) {
            Ok(#(val, parser)) -> {
              let mapping = dict.from_list([#(s, val)])
              Ok(#(value.Mapping(mapping), parser))
            }
            Error(e) -> Error(e)
          }
        }
        _ -> {
          let mapping = dict.from_list([#(s, value.Null)])
          Ok(#(value.Mapping(mapping), parser))
        }
      }
    }
    // Colon immediately (empty key)
    Some(lexer.Colon) -> {
      let parser = skip_whitespace(advance(parser))
      case parse_flow_value(parser) {
        Ok(#(val, parser)) -> {
          let mapping = dict.from_list([#("", val)])
          Ok(#(value.Mapping(mapping), parser))
        }
        Error(e) -> Error(e)
      }
    }
    // No key or colon - empty mapping with empty key
    _ -> {
      let mapping = dict.from_list([#("", value.Null)])
      Ok(#(value.Mapping(mapping), parser))
    }
  }
}

/// Parse a flow mapping.
pub fn parse_flow_mapping(
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_flow_mapping_pairs(skip_whitespace(parser), dict.new())
}

fn parse_flow_mapping_pairs(
  parser: Parser,
  acc: Dict(String, YamlValue),
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_whitespace(parser)

  case current(parser) {
    Some(lexer.BraceClose) -> Ok(#(value.Mapping(acc), advance(parser)))
    Some(lexer.Eof) ->
      Error(ParseError("Unterminated flow mapping", parser.pos))
    option.None -> Error(ParseError("Unterminated flow mapping", parser.pos))
    // Explicit key in flow mapping
    Some(lexer.Question) -> {
      let parser = skip_whitespace(advance(parser))
      // Check for empty explicit key followed immediately by end or comma
      case current(parser) {
        Some(lexer.BraceClose) -> {
          // ?} - empty key with null value
          let acc = dict.insert(acc, "", value.Null)
          Ok(#(value.Mapping(acc), advance(parser)))
        }
        Some(lexer.Comma) -> {
          // ?, - empty key with null value
          let acc = dict.insert(acc, "", value.Null)
          parse_flow_mapping_pairs(advance(parser), acc)
        }
        Some(lexer.Colon) -> {
          // ?: - empty explicit key, parse the value
          let parser = skip_whitespace(advance(parser))
          case parse_flow_value(parser) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, "", val)
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
        _ -> {
          case parse_flow_key_with_colon(parser) {
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
                // Key with no value
                Some(lexer.Comma) -> {
                  let acc = dict.insert(acc, key, value.Null)
                  parse_flow_mapping_pairs(advance(parser), acc)
                }
                Some(lexer.BraceClose) -> {
                  let acc = dict.insert(acc, key, value.Null)
                  Ok(#(value.Mapping(acc), advance(parser)))
                }
                _ -> Error(ParseError("Expected ':', ',' or '}'", parser.pos))
              }
            }
            Error(e) -> Error(e)
          }
        }
      }
    }
    _ -> {
      case parse_flow_key_with_colon(parser) {
        Ok(#(key, parser)) -> {
          let parser = skip_whitespace(parser)
          case current(parser) {
            // Handle adjacent colon: "key":value becomes key="key", and
            // we have Plain(":value") as the next token
            Some(lexer.Plain(s)) -> {
              case string.starts_with(s, ":") {
                True -> {
                  // Extract value after the colon
                  let val_str = string.drop_start(s, 1)
                  let val = case val_str {
                    "" -> value.Null
                    _ -> parse_scalar(val_str)
                  }
                  let acc = dict.insert(acc, key, val)
                  let parser = skip_whitespace(advance(parser))
                  case current(parser) {
                    Some(lexer.Comma) ->
                      parse_flow_mapping_pairs(advance(parser), acc)
                    Some(lexer.BraceClose) ->
                      Ok(#(value.Mapping(acc), advance(parser)))
                    _ -> Error(ParseError("Expected ',' or '}'", parser.pos))
                  }
                }
                False -> Error(ParseError("Expected ':', ',' or '}'", parser.pos))
              }
            }
            Some(lexer.Colon) -> {
              let parser = skip_whitespace(advance(parser))
              // Check for empty value (comma or close brace immediately after colon)
              case current(parser) {
                Some(lexer.Comma) -> {
                  let acc = dict.insert(acc, key, value.Null)
                  parse_flow_mapping_pairs(advance(parser), acc)
                }
                Some(lexer.BraceClose) -> {
                  let acc = dict.insert(acc, key, value.Null)
                  Ok(#(value.Mapping(acc), advance(parser)))
                }
                _ -> {
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
              }
            }
            // Key with no colon (implicit null value) - like "http://foo.com,"
            Some(lexer.Comma) -> {
              let acc = dict.insert(acc, key, value.Null)
              parse_flow_mapping_pairs(advance(parser), acc)
            }
            Some(lexer.BraceClose) -> {
              let acc = dict.insert(acc, key, value.Null)
              Ok(#(value.Mapping(acc), advance(parser)))
            }
            _ -> Error(ParseError("Expected ':', ',' or '}'", parser.pos))
          }
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Parse a flow key that might contain colons (like URLs) or be complex (like [a,b]).
pub fn parse_flow_key_with_colon(
  parser: Parser,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      // Check if there's a colon followed by space/newline (end of key)
      // or if the colon is part of the key (like in URLs)
      let parser = advance(parser)
      collect_flow_key_parts(parser, s)
    }
    Some(lexer.SingleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(s, advance(parser)))
    // Complex key: flow sequence as key like [d, e]: f
    Some(lexer.BracketOpen) -> {
      case parse_flow_sequence(advance(parser)) {
        Ok(#(val, parser)) -> Ok(#(value_to_key_string(val), parser))
        Error(e) -> Error(e)
      }
    }
    // Complex key: flow mapping as key like {a: b}: c
    Some(lexer.BraceOpen) -> {
      case parse_flow_mapping(advance(parser)) {
        Ok(#(val, parser)) -> Ok(#(value_to_key_string(val), parser))
        Error(e) -> Error(e)
      }
    }
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_whitespace
      // Handle anchor before complex key like &a [b, c]
      case current(parser) {
        Some(lexer.BracketOpen) -> {
          case parse_flow_sequence(advance(parser)) {
            Ok(#(val, parser)) -> {
              let parser =
                Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
              Ok(#(value_to_key_string(val), parser))
            }
            Error(e) -> Error(e)
          }
        }
        Some(lexer.BraceOpen) -> {
          case parse_flow_mapping(advance(parser)) {
            Ok(#(val, parser)) -> {
              let parser =
                Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
              Ok(#(value_to_key_string(val), parser))
            }
            Error(e) -> Error(e)
          }
        }
        _ -> {
          case parse_flow_key_with_colon(parser) {
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
      }
    }
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(value_to_key_string(val), parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser) |> skip_whitespace
      parse_flow_key_with_colon(parser)
    }
    // Empty key - Colon indicates the key is empty (null)
    Some(lexer.Colon) -> Ok(#("", parser))
    _ -> Error(ParseError("Expected mapping key", parser.pos))
  }
}

/// Collect parts of a flow key that might span colons (like http://foo.com)
/// or multiple lines (multiline plain scalars in flow context).
fn collect_flow_key_parts(
  parser: Parser,
  acc: String,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    // Colon that's part of the key (like in URLs)
    Some(lexer.Colon) -> {
      // Look ahead to see if this colon is followed by something that
      // indicates it's a mapping separator vs part of the key
      let after_colon = advance(parser)
      case current(after_colon) {
        // Space after colon means this is a mapping separator
        Some(lexer.Indent(_)) -> Ok(#(string.trim_end(acc), parser))
        // For flow context, we need to check if there's a space
        // A colon directly followed by a value is a separator
        Some(lexer.Plain(_))
        | Some(lexer.SingleQuoted(_))
        | Some(lexer.DoubleQuoted(_))
        | Some(lexer.BracketOpen)
        | Some(lexer.BraceOpen) -> {
          // This looks like a separator - stop here
          Ok(#(string.trim_end(acc), parser))
        }
        // Otherwise, colon is part of the key
        Some(lexer.Newline) | Some(lexer.Comma) | Some(lexer.BraceClose) -> {
          Ok(#(string.trim_end(acc), parser))
        }
        _ -> Ok(#(string.trim_end(acc), parser))
      }
    }
    // Plain text continuation
    Some(lexer.Plain(s)) -> {
      let parser = advance(parser)
      collect_flow_key_parts(parser, acc <> s)
    }
    // Multiline continuation: whitespace (newline/indent) followed by more plain text
    // In flow context, newlines are folded to a single space
    Some(lexer.Indent(_)) | Some(lexer.Newline) -> {
      // Look ahead to see if there's a Plain token after the whitespace
      let parser_after_ws = skip_whitespace(parser)
      case current(parser_after_ws) {
        Some(lexer.Plain(s)) -> {
          // Continue with a space between the parts
          let parser = advance(parser_after_ws)
          collect_flow_key_parts(parser, acc <> " " <> s)
        }
        // If not followed by plain text, stop here
        _ -> Ok(#(string.trim_end(acc), parser))
      }
    }
    _ -> Ok(#(string.trim_end(acc), parser))
  }
}

/// Collect parts of a plain scalar in a flow sequence that might span multiple lines.
/// This is similar to collect_flow_key_parts but for values, not keys.
fn collect_flow_plain_scalar_parts(parser: Parser, acc: String) -> #(String, Parser) {
  case current(parser) {
    // Stop at flow indicators
    Some(lexer.Comma)
    | Some(lexer.BracketClose)
    | Some(lexer.BraceClose)
    | Some(lexer.Colon) -> #(string.trim_end(acc), parser)
    // Plain text continuation on same line
    Some(lexer.Plain(s)) -> {
      let parser = advance(parser)
      collect_flow_plain_scalar_parts(parser, acc <> s)
    }
    // Multiline continuation: whitespace (newline/indent) followed by more plain text
    // In flow context, newlines are folded to a single space
    Some(lexer.Indent(_)) | Some(lexer.Newline) -> {
      // Look ahead to see if there's a Plain token after the whitespace
      let parser_after_ws = skip_whitespace(parser)
      case current(parser_after_ws) {
        Some(lexer.Plain(s)) -> {
          // Continue with a space between the parts
          let parser = advance(parser_after_ws)
          collect_flow_plain_scalar_parts(parser, acc <> " " <> s)
        }
        // If not followed by plain text, stop here
        _ -> #(string.trim_end(acc), parser_after_ws)
      }
    }
    _ -> #(string.trim_end(acc), parser)
  }
}

/// Parse a flow value.
pub fn parse_flow_value(parser: Parser) -> Result(#(YamlValue, Parser), ParseError) {
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
    // Skip tags and parse the value
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser) |> skip_whitespace
      parse_flow_value(parser)
    }
    _ -> Ok(#(value.Null, parser))
  }
}
