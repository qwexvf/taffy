//// Block collection parsing (sequences and mappings in block style).

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import yaml/lexer
import yaml/parser/helpers.{
  advance, current, skip_newlines_and_comments, skip_spaces,
}
import yaml/parser/scalar.{value_to_string}
import yaml/parser/types.{type ParseError, type Parser, ParseError, Parser}
import yaml/value.{type YamlValue}

/// Type alias for parse_value function to avoid circular imports.
pub type ParseValueFn =
  fn(Parser, Int) -> Result(#(YamlValue, Parser), ParseError)

/// Parse a block sequence.
pub fn parse_block_sequence(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_block_sequence_items(parser, min_indent, [], parse_value_fn)
}

fn parse_block_sequence_items(
  parser: Parser,
  min_indent: Int,
  acc: List(YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)
  let is_first_item = list.is_empty(acc)

  case current(parser) {
    // Raw Dash (no preceding Indent) means it's at indent 0
    // Match if: this is the first item OR min_indent allows indent 0
    // The first item is always valid because parse_value directed us here
    Some(lexer.Dash) if is_first_item -> {
      let parser = advance(parser) |> skip_spaces
      case parse_value_fn(parser, min_indent + 1) {
        Ok(#(val, parser)) -> {
          parse_block_sequence_items(parser, min_indent, [val, ..acc], parse_value_fn)
        }
        Error(e) -> Error(e)
      }
    }
    Some(lexer.Dash) if min_indent == 0 -> {
      let parser = advance(parser) |> skip_spaces
      case parse_value_fn(parser, min_indent + 1) {
        Ok(#(val, parser)) -> {
          parse_block_sequence_items(parser, min_indent, [val, ..acc], parse_value_fn)
        }
        Error(e) -> Error(e)
      }
    }
    Some(lexer.Indent(n)) if n >= min_indent -> {
      let parser = advance(parser)
      case current(parser) {
        Some(lexer.Dash) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_value_fn(parser, n + 1) {
            Ok(#(val, parser)) -> {
              parse_block_sequence_items(parser, min_indent, [val, ..acc], parse_value_fn)
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

/// Parse a block mapping starting from a key that was already parsed.
pub fn parse_block_mapping_from_key(
  first_key: String,
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_spaces(parser)

  // Use parse_mapping_value to handle all the complex cases (anchors, sequences, etc.)
  case parse_mapping_value(parser, min_indent, parse_value_fn) {
    Ok(#(first_val, parser)) -> {
      let initial = dict.from_list([#(first_key, first_val)])
      parse_block_mapping_pairs(parser, min_indent, initial, parse_value_fn)
    }
    Error(e) -> Error(e)
  }
}

/// Parse a mapping value that might be on the next line as a sequence.
pub fn parse_mapping_value(
  parser: Parser,
  key_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_mapping_value_with_anchor(parser, key_indent, None, parse_value_fn)
}

fn parse_mapping_value_with_anchor(
  parser: Parser,
  key_indent: Int,
  anchor: Option(String),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Newline) -> {
      let parser = advance(parser)
      parse_mapping_value_after_newline(parser, key_indent, anchor, parse_value_fn)
    }
    // Tag on value - skip it and continue (handles `key: !!tag\n  value`)
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser) |> skip_spaces
      parse_mapping_value_with_anchor(parser, key_indent, anchor, parse_value_fn)
    }
    // Indent(n) means we're on a new line with indent n (lexer consumed the newline)
    Some(lexer.Indent(n)) -> {
      let parser = advance(parser)
      case current(parser) {
        Some(lexer.Dash) if n >= key_indent -> {
          let parser = Parser(..parser, pos: parser.pos - 1)
          case parse_block_sequence(parser, n, parse_value_fn) {
            Ok(#(val, parser)) -> wrap_with_anchor(val, anchor, parser)
            Error(e) -> Error(e)
          }
        }
        // Anchor on its own line before the value
        Some(lexer.Anchor(name)) -> {
          let parser = advance(parser) |> skip_spaces
          parse_mapping_value_after_anchor(parser, key_indent, name, parse_value_fn)
        }
        _ -> {
          let parser = Parser(..parser, pos: parser.pos - 1)
          case parse_value_fn(parser, key_indent + 1) {
            Ok(#(val, parser)) -> wrap_with_anchor(val, anchor, parser)
            Error(e) -> Error(e)
          }
        }
      }
    }
    // Anchor on same line
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_spaces
      parse_mapping_value_after_anchor(parser, key_indent, name, parse_value_fn)
    }
    _ -> {
      case parse_value_fn(parser, key_indent + 1) {
        Ok(#(val, parser)) -> wrap_with_anchor(val, anchor, parser)
        Error(e) -> Error(e)
      }
    }
  }
}

/// After a newline (at indent 0), look for the value.
fn parse_mapping_value_after_newline(
  parser: Parser,
  key_indent: Int,
  anchor: Option(String),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Indent(n)) -> {
      let parser = advance(parser)
      case current(parser) {
        Some(lexer.Dash) if n >= key_indent -> {
          let parser = Parser(..parser, pos: parser.pos - 1)
          case parse_block_sequence(parser, n, parse_value_fn) {
            Ok(#(val, parser)) -> wrap_with_anchor(val, anchor, parser)
            Error(e) -> Error(e)
          }
        }
        // Anchor on its own line before the value
        Some(lexer.Anchor(name)) -> {
          let parser = advance(parser) |> skip_spaces
          parse_mapping_value_after_anchor(parser, key_indent, name, parse_value_fn)
        }
        _ -> {
          let parser = Parser(..parser, pos: parser.pos - 1)
          case parse_value_fn(parser, key_indent + 1) {
            Ok(#(val, parser)) -> wrap_with_anchor(val, anchor, parser)
            Error(e) -> Error(e)
          }
        }
      }
    }
    Some(lexer.Dash) if key_indent == 0 -> {
      case parse_block_sequence(parser, 0, parse_value_fn) {
        Ok(#(val, parser)) -> wrap_with_anchor(val, anchor, parser)
        Error(e) -> Error(e)
      }
    }
    // Anchor directly after newline (no indent)
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_spaces
      parse_mapping_value_after_anchor(parser, key_indent, name, parse_value_fn)
    }
    _ -> Ok(#(value.Null, parser))
  }
}

/// After seeing an anchor, parse the actual value which might be on a following line.
fn parse_mapping_value_after_anchor(
  parser: Parser,
  key_indent: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Newline) -> {
      let parser = advance(parser)
      case current(parser) {
        // Sequence at indent 0 (anchor was on previous line)
        Some(lexer.Dash) if key_indent == 0 -> {
          case parse_block_sequence(parser, 0, parse_value_fn) {
            Ok(#(val, parser)) -> {
              let parser =
                Parser(
                  ..parser,
                  anchors: dict.insert(parser.anchors, anchor_name, val),
                )
              Ok(#(val, parser))
            }
            Error(e) -> Error(e)
          }
        }
        Some(lexer.Indent(n)) -> {
          let parser = advance(parser)
          case current(parser) {
            // Sequence at >= key_indent
            Some(lexer.Dash) if n >= key_indent -> {
              let parser = Parser(..parser, pos: parser.pos - 1)
              case parse_block_sequence(parser, n, parse_value_fn) {
                Ok(#(val, parser)) -> {
                  let parser =
                    Parser(
                      ..parser,
                      anchors: dict.insert(parser.anchors, anchor_name, val),
                    )
                  Ok(#(val, parser))
                }
                Error(e) -> Error(e)
              }
            }
            _ -> {
              // Check if indent indicates a continuation value (greater than key_indent)
              case n > key_indent {
                True -> {
                  let parser = Parser(..parser, pos: parser.pos - 1)
                  case parse_value_fn(parser, key_indent + 1) {
                    Ok(#(val, parser)) -> {
                      let parser =
                        Parser(
                          ..parser,
                          anchors: dict.insert(parser.anchors, anchor_name, val),
                        )
                      Ok(#(val, parser))
                    }
                    Error(e) -> Error(e)
                  }
                }
                // Same or lower indent - anchor is for empty/null value
                False -> {
                  let parser = Parser(..parser, pos: parser.pos - 1)
                  let parser =
                    Parser(
                      ..parser,
                      anchors: dict.insert(parser.anchors, anchor_name, value.Null),
                    )
                  Ok(#(value.Null, parser))
                }
              }
            }
          }
        }
        // No indent after newline - anchor is for empty/null value
        _ -> {
          let parser =
            Parser(
              ..parser,
              anchors: dict.insert(parser.anchors, anchor_name, value.Null),
            )
          Ok(#(value.Null, parser))
        }
      }
    }
    // Value immediately after anchor (Indent token means newline+indent was combined)
    _ -> {
      case parse_value_fn(parser, key_indent + 1) {
        Ok(#(val, parser)) -> {
          let parser =
            Parser(
              ..parser,
              anchors: dict.insert(parser.anchors, anchor_name, val),
            )
          Ok(#(val, parser))
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Wrap a value with an optional anchor.
fn wrap_with_anchor(
  val: YamlValue,
  anchor: Option(String),
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case anchor {
    Some(name) -> {
      let parser =
        Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
      Ok(#(val, parser))
    }
    None -> Ok(#(val, parser))
  }
}

fn parse_block_mapping_pairs(
  parser: Parser,
  min_indent: Int,
  acc: Dict(String, YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)

  case current(parser) {
    Some(lexer.Indent(n)) if n >= min_indent -> {
      let parser = advance(parser)
      case current(parser) {
        // Empty key at this indent (: with no key)
        Some(lexer.Colon) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_mapping_value(parser, n, parse_value_fn) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, "", val)
              parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
            }
            Error(e) -> Error(e)
          }
        }
        // Explicit key (?)
        Some(lexer.Question) -> {
          parse_explicit_key_in_mapping(parser, min_indent, n, acc, parse_value_fn)
        }
        _ -> {
          case parse_mapping_key(parser) {
            Ok(#(key, parser)) -> {
              let parser = skip_spaces(parser)
              case current(parser) {
                Some(lexer.Colon) -> {
                  let parser = advance(parser) |> skip_spaces
                  case parse_mapping_value(parser, n, parse_value_fn) {
                    Ok(#(val, parser)) -> {
                      let acc = dict.insert(acc, key, val)
                      parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
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
      }
    }
    // Empty key at indent 0 (: with no key and no preceding indent)
    Some(lexer.Colon) -> {
      case min_indent != 0 {
        True -> Ok(#(value.Mapping(acc), parser))
        False -> {
          let parser = advance(parser) |> skip_spaces
          case parse_mapping_value(parser, min_indent, parse_value_fn) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, "", val)
              parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
            }
            Error(e) -> Error(e)
          }
        }
      }
    }
    // Explicit key (?) at indent 0
    Some(lexer.Question) -> {
      case min_indent != 0 {
        True -> Ok(#(value.Mapping(acc), parser))
        False -> parse_explicit_key_in_mapping(parser, min_indent, 0, acc, parse_value_fn)
      }
    }
    // Keys directly visible (no Indent token) - these are at indent 0
    // Only accept them if min_indent == 0
    Some(lexer.Plain(s)) -> {
      case min_indent != 0 {
        True -> Ok(#(value.Mapping(acc), parser))
        False -> {
          let parser = advance(parser) |> skip_spaces
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = advance(parser) |> skip_spaces
              case parse_mapping_value(parser, min_indent, parse_value_fn) {
                Ok(#(val, parser)) -> {
                  let acc = dict.insert(acc, s, val)
                  parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
                }
                Error(e) -> Error(e)
              }
            }
            _ -> Ok(#(value.Mapping(acc), parser))
          }
        }
      }
    }
    Some(lexer.SingleQuoted(s)) -> {
      case min_indent != 0 {
        True -> Ok(#(value.Mapping(acc), parser))
        False -> {
          let parser = advance(parser) |> skip_spaces
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = advance(parser) |> skip_spaces
              case parse_mapping_value(parser, min_indent, parse_value_fn) {
                Ok(#(val, parser)) -> {
                  let acc = dict.insert(acc, s, val)
                  parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
                }
                Error(e) -> Error(e)
              }
            }
            _ -> Ok(#(value.Mapping(acc), parser))
          }
        }
      }
    }
    Some(lexer.DoubleQuoted(s)) -> {
      case min_indent != 0 {
        True -> Ok(#(value.Mapping(acc), parser))
        False -> {
          let parser = advance(parser) |> skip_spaces
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = advance(parser) |> skip_spaces
              case parse_mapping_value(parser, min_indent, parse_value_fn) {
                Ok(#(val, parser)) -> {
                  let acc = dict.insert(acc, s, val)
                  parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
                }
                Error(e) -> Error(e)
              }
            }
            _ -> Ok(#(value.Mapping(acc), parser))
          }
        }
      }
    }
    // Alias as mapping key
    Some(lexer.Alias(name)) -> {
      case min_indent != 0 {
        True -> Ok(#(value.Mapping(acc), parser))
        False -> {
          let parser = advance(parser) |> skip_spaces
          case dict.get(parser.anchors, name) {
            Ok(key_val) -> {
              let key = value_to_string(key_val)
              case current(parser) {
                Some(lexer.Colon) -> {
                  let parser = advance(parser) |> skip_spaces
                  case parse_mapping_value(parser, min_indent, parse_value_fn) {
                    Ok(#(val, parser)) -> {
                      let acc = dict.insert(acc, key, val)
                      parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
                    }
                    Error(e) -> Error(e)
                  }
                }
                _ -> Ok(#(value.Mapping(acc), parser))
              }
            }
            Error(_) ->
              Error(ParseError("Unknown anchor: " <> name, parser.pos))
          }
        }
      }
    }
    // Anchor before mapping key
    Some(lexer.Anchor(name)) -> {
      case min_indent != 0 {
        True -> Ok(#(value.Mapping(acc), parser))
        False -> {
          let parser = advance(parser) |> skip_spaces
          case parse_mapping_key(parser) {
            Ok(#(key, parser)) -> {
              let parser = skip_spaces(parser)
              case current(parser) {
                Some(lexer.Colon) -> {
                  let parser = advance(parser) |> skip_spaces
                  case parse_mapping_value(parser, min_indent, parse_value_fn) {
                    Ok(#(val, parser)) -> {
                      let parser =
                        Parser(
                          ..parser,
                          anchors: dict.insert(
                            parser.anchors,
                            name,
                            value.String(key),
                          ),
                        )
                      let acc = dict.insert(acc, key, val)
                      parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
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
      }
    }
    // Tag before mapping key (skip the tag and parse the key)
    Some(lexer.Tag(_)) -> {
      case min_indent != 0 {
        True -> Ok(#(value.Mapping(acc), parser))
        False -> {
          let parser = advance(parser) |> skip_spaces
          case parse_mapping_key(parser) {
            Ok(#(key, parser)) -> {
              let parser = skip_spaces(parser)
              case current(parser) {
                Some(lexer.Colon) -> {
                  let parser = advance(parser) |> skip_spaces
                  case parse_mapping_value(parser, min_indent, parse_value_fn) {
                    Ok(#(val, parser)) -> {
                      let acc = dict.insert(acc, key, val)
                      parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
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
      }
    }
    _ -> Ok(#(value.Mapping(acc), parser))
  }
}

/// Parse a mapping key.
/// In block context, flow indicators like `,`, `[`, `]`, `{`, `}` are plain characters.
pub fn parse_mapping_key(parser: Parser) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      // Collect additional tokens that are part of the key in block context
      collect_block_key(advance(parser), s)
    }
    Some(lexer.SingleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(s, advance(parser)))
    // Alias as key - resolve and use its string value
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(value_to_string(val), parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    // Anchor before key
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_spaces
      case parse_mapping_key(parser) {
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
    // Tag before key (skip the tag, parse the key)
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser) |> skip_spaces
      parse_mapping_key(parser)
    }
    _ -> Error(ParseError("Expected mapping key", parser.pos))
  }
}

/// Collect additional tokens that form part of a block key.
/// In block context, flow indicators (`,`, `[`, `]`, `{`, `}`) are plain characters.
/// The key ends when we see a colon followed by whitespace.
fn collect_block_key(parser: Parser, acc: String) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    // Flow indicators in block context are part of the key
    Some(lexer.Comma) -> collect_block_key(advance(parser), acc <> ",")
    Some(lexer.BracketOpen) -> collect_block_key(advance(parser), acc <> "[")
    Some(lexer.BracketClose) -> collect_block_key(advance(parser), acc <> "]")
    Some(lexer.BraceOpen) -> collect_block_key(advance(parser), acc <> "{")
    Some(lexer.BraceClose) -> collect_block_key(advance(parser), acc <> "}")
    // Colon might be part of the key or the separator
    Some(lexer.Colon) -> {
      // Check what follows the colon
      let after_colon = advance(parser)
      case current(after_colon) {
        // Colon followed by whitespace/newline/eof = key separator
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
        | None -> Ok(#(acc, parser))
        // Colon followed by non-whitespace = part of the key
        _ -> collect_block_key(after_colon, acc <> ":")
      }
    }
    // Plain scalar continuation
    Some(lexer.Plain(s)) -> collect_block_key(advance(parser), acc <> s)
    // Key is complete
    _ -> Ok(#(acc, parser))
  }
}

/// Parse an explicit key (?) within a block mapping.
fn parse_explicit_key_in_mapping(
  parser: Parser,
  min_indent: Int,
  key_indent: Int,
  acc: Dict(String, YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  // Skip the ? token
  let parser = advance(parser) |> skip_spaces

  // Parse the key (can be a simple scalar or complex value)
  case parse_explicit_key(parser, parse_value_fn) {
    Ok(#(key, parser)) -> {
      // Look for : after the key
      let parser = skip_newlines_and_comments(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = advance(parser) |> skip_spaces
          case parse_value_fn(parser, key_indent + 1) {
            Ok(#(val, parser)) -> {
              let acc = dict.insert(acc, key, val)
              parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
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
                  parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
                }
                Error(e) -> Error(e)
              }
            }
            // Another ? means previous key has null value
            Some(lexer.Question) -> {
              let acc = dict.insert(acc, key, value.Null)
              parse_explicit_key_in_mapping(parser, min_indent, n, acc, parse_value_fn)
            }
            // Plain scalar means implicit key (mixed mapping)
            Some(lexer.Plain(s)) -> {
              let acc = dict.insert(acc, key, value.Null)
              let parser = advance(parser) |> skip_spaces
              case current(parser) {
                Some(lexer.Colon) -> {
                  let parser = advance(parser) |> skip_spaces
                  case parse_value_fn(parser, n + 1) {
                    Ok(#(val, parser)) -> {
                      let acc = dict.insert(acc, s, val)
                      parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
                    }
                    Error(e) -> Error(e)
                  }
                }
                _ -> Ok(#(value.Mapping(acc), parser))
              }
            }
            _ -> {
              // No colon, key maps to null
              let acc = dict.insert(acc, key, value.Null)
              parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
            }
          }
        }
        // Another ? at same level means previous key has null value
        Some(lexer.Question) -> {
          let acc = dict.insert(acc, key, value.Null)
          parse_explicit_key_in_mapping(parser, min_indent, key_indent, acc, parse_value_fn)
        }
        _ -> {
          // No colon, key maps to null
          let acc = dict.insert(acc, key, value.Null)
          parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
        }
      }
    }
    Error(e) -> Error(e)
  }
}

/// Collect a multiline explicit key value.
/// Stops at `:` (value separator) or when indent decreases.
fn collect_explicit_key_value(parser: Parser, acc: String) -> #(String, Parser) {
  case current(parser) {
    // Plain text continuation
    Some(lexer.Plain(s)) -> collect_explicit_key_value(advance(parser), acc <> " " <> s)
    // Newline - check for continuation
    Some(lexer.Newline) -> {
      let after_newline = advance(parser)
      case current(after_newline) {
        Some(lexer.Indent(n)) if n > 0 -> {
          let after_indent = advance(after_newline)
          case current(after_indent) {
            // Colon means end of key
            Some(lexer.Colon) -> #(acc, after_newline)
            // Plain text continuation
            Some(lexer.Plain(s)) -> collect_explicit_key_value(advance(after_indent), acc <> " " <> s)
            // Other - end of key
            _ -> #(acc, after_newline)
          }
        }
        // No indent or colon at indent 0 - end of key
        _ -> #(acc, parser)
      }
    }
    // Indent token (already past newline)
    Some(lexer.Indent(n)) if n > 0 -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        // Colon means end of key
        Some(lexer.Colon) -> #(acc, parser)
        // Plain text continuation
        Some(lexer.Plain(s)) -> collect_explicit_key_value(advance(after_indent), acc <> " " <> s)
        // Other - end of key
        _ -> #(acc, parser)
      }
    }
    // Colon at indent 0 - end of key
    Some(lexer.Colon) -> #(acc, parser)
    // Comment ends the key
    Some(lexer.Comment(_)) -> #(acc, parser)
    // End of key
    _ -> #(acc, parser)
  }
}

/// Parse the key part of an explicit key.
fn parse_explicit_key(parser: Parser, parse_value_fn: ParseValueFn) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      // Collect potential multiline continuation
      let #(full_key, parser) = collect_explicit_key_value(advance(parser), s)
      Ok(#(full_key, parser))
    }
    Some(lexer.SingleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.Literal(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.Folded(s)) -> Ok(#(s, advance(parser)))
    // Alias as key
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(value_to_string(val), parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    // Anchor before key
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_spaces
      case parse_explicit_key(parser, parse_value_fn) {
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
    // Tag before key (skip it)
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser) |> skip_spaces
      parse_explicit_key(parser, parse_value_fn)
    }
    // Newline - check for content on next line
    Some(lexer.Newline) -> {
      let after_newline = advance(parser)
      case current(after_newline) {
        Some(lexer.Dash) -> {
          case parse_block_sequence(after_newline, 0, parse_value_fn) {
            Ok(#(seq_val, parser)) -> Ok(#(value_to_string(seq_val), parser))
            Error(e) -> Error(e)
          }
        }
        Some(lexer.Indent(n)) -> {
          let after_indent = advance(after_newline)
          case current(after_indent) {
            Some(lexer.Dash) -> {
              let parser = Parser(..after_indent, pos: after_indent.pos - 1)
              case parse_block_sequence(parser, n, parse_value_fn) {
                Ok(#(seq_val, parser)) -> Ok(#(value_to_string(seq_val), parser))
                Error(e) -> Error(e)
              }
            }
            Some(lexer.Colon) -> Ok(#("", after_newline))
            _ -> Ok(#("", after_newline))
          }
        }
        Some(lexer.Colon) -> Ok(#("", after_newline))
        _ -> Ok(#("", parser))
      }
    }
    // Indent - check for content
    Some(lexer.Indent(n)) -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        Some(lexer.Dash) -> {
          let parser = Parser(..after_indent, pos: after_indent.pos - 1)
          case parse_block_sequence(parser, n, parse_value_fn) {
            Ok(#(seq_val, parser)) -> Ok(#(value_to_string(seq_val), parser))
            Error(e) -> Error(e)
          }
        }
        Some(lexer.Colon) -> Ok(#("", parser))
        _ -> Ok(#("", parser))
      }
    }
    // Empty key
    Some(lexer.Colon) -> Ok(#("", parser))
    None | Some(lexer.Eof) -> Ok(#("", parser))
    Some(lexer.Comment(_)) -> {
      let parser = advance(parser)
      parse_explicit_key(parser, parse_value_fn)
    }
    _ -> Ok(#("", parser))
  }
}
