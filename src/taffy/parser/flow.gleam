//// Flow collection parsing (sequences and mappings in [] and {} syntax).

import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import taffy/lexer
import taffy/parser/helpers.{
  advance, current, flow_whitespace_has_comment, skip_flow_whitespace,
  skip_whitespace,
}
import taffy/parser/scalar.{parse_scalar, value_to_key_string}
import taffy/parser/types.{type ParseError, type Parser, ParseError, Parser}
import taffy/value.{type YamlValue}

/// Parse a flow sequence.
pub fn parse_flow_sequence(
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  use parser <- result.try(skip_flow_whitespace(parser))
  parse_flow_sequence_items(parser, [])
}

fn parse_flow_sequence_items(
  parser: Parser,
  acc: List(YamlValue),
) -> Result(#(YamlValue, Parser), ParseError) {
  use parser <- result.try(skip_flow_whitespace(parser))

  case current(parser) {
    Some(lexer.BracketClose) ->
      Ok(#(value.Sequence(list.reverse(acc)), advance(parser)))

    Some(lexer.Eof) | option.None ->
      Error(ParseError("Unterminated flow sequence", parser.pos))

    // Leading comma or double comma is invalid
    Some(lexer.Comma) -> {
      case acc {
        [] ->
          Error(ParseError("Invalid leading comma in flow sequence", parser.pos))
        _ ->
          Error(ParseError("Invalid extra comma in flow sequence", parser.pos))
      }
    }

    Some(lexer.Question) -> {
      use #(val, parser) <- result.try(parse_flow_sequence_single_pair(
        skip_whitespace(advance(parser)),
        True,
      ))
      continue_flow_sequence(val, parser, acc)
    }

    _ -> {
      use #(val, parser) <- result.try(parse_flow_sequence_entry(parser))
      continue_flow_sequence(val, parser, acc)
    }
  }
}

/// Continue parsing a flow sequence after parsing an entry.
fn continue_flow_sequence(
  val: YamlValue,
  parser: Parser,
  acc: List(YamlValue),
) -> Result(#(YamlValue, Parser), ParseError) {
  use parser <- result.try(skip_flow_whitespace(parser))
  case current(parser) {
    Some(lexer.Comma) ->
      parse_flow_sequence_items(advance(parser), [val, ..acc])
    Some(lexer.BracketClose) ->
      Ok(#(value.Sequence(list.reverse([val, ..acc])), advance(parser)))
    _ -> Error(ParseError("Expected ',' or ']'", parser.pos))
  }
}

/// Parse a flow sequence entry, which might be a single-pair implicit mapping.
fn parse_flow_sequence_entry(
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.BracketOpen) -> {
      use #(seq_val, parser) <- result.try(parse_flow_sequence(advance(parser)))
      check_for_mapping_key(seq_val, parser)
    }

    Some(lexer.BraceOpen) -> {
      use #(map_val, parser) <- result.try(parse_flow_mapping(advance(parser)))
      check_for_mapping_key(map_val, parser)
    }

    Some(lexer.Plain(s)) -> {
      let #(full_scalar, parser) =
        collect_flow_plain_scalar_parts(advance(parser), s)
      check_for_plain_mapping_key(full_scalar, parser)
    }

    Some(lexer.SingleQuoted(s)) ->
      check_for_quoted_mapping_key(s, advance(parser))

    Some(lexer.DoubleQuoted(s)) ->
      check_for_quoted_mapping_key(s, advance(parser))
    Some(lexer.Anchor(name)) -> {
      use #(val, parser) <- result.try(parse_flow_sequence_entry(
        advance(parser) |> skip_whitespace,
      ))
      let parser =
        Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
      Ok(#(val, parser))
    }

    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(val, parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }

    Some(lexer.Tag(tag)) -> {
      let parser = advance(parser) |> skip_whitespace
      // Check if this is a string tag with empty content
      let is_str_tag =
        tag == "!!str" || string.contains(tag, "tag:taffy.org,2002:str")
      case current(parser), is_str_tag {
        // String tag followed by comma, close bracket, or EOF = empty string
        Some(lexer.Comma), True
        | Some(lexer.BracketClose), True
        | Some(lexer.BraceClose), True
        | Some(lexer.Eof), True
        | option.None, True
        -> Ok(#(value.String(""), parser))
        // Otherwise parse normally
        _, _ -> parse_flow_sequence_entry(parser)
      }
    }

    Some(lexer.Colon) -> {
      use #(val, parser) <- result.try(
        parse_flow_value(skip_whitespace(advance(parser))),
      )
      Ok(#(value.Mapping([#("", val)]), parser))
    }

    Some(lexer.Comma) | Some(lexer.BracketClose) | _ ->
      Ok(#(value.Null, parser))
  }
}

/// Check if a value is used as a mapping key (followed by colon).
fn check_for_mapping_key(
  val: YamlValue,
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_whitespace(parser)
  case current(parser) {
    Some(lexer.Colon) -> {
      let key = value_to_key_string(val)
      use #(map_val, parser) <- result.try(
        parse_flow_value(skip_whitespace(advance(parser))),
      )
      Ok(#(value.Mapping([#(key, map_val)]), parser))
    }
    Some(lexer.Plain(p)) -> check_adjacent_colon(val, p, parser)
    _ -> Ok(#(val, parser))
  }
}

/// Check for adjacent colon pattern (no space between value and colon).
fn check_adjacent_colon(
  key_val: YamlValue,
  plain: String,
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case string.starts_with(plain, ":") {
    True -> {
      let key = value_to_key_string(key_val)
      let val = parse_adjacent_value(string.drop_start(plain, 1))
      Ok(#(value.Mapping([#(key, val)]), advance(parser)))
    }
    False -> Ok(#(key_val, parser))
  }
}

/// Parse value from adjacent colon pattern.
fn parse_adjacent_value(val_str: String) -> YamlValue {
  case val_str {
    "" -> value.Null
    _ -> parse_scalar(val_str)
  }
}

/// Check if a plain scalar is used as a mapping key.
/// Only checks for colon on the same line (implicit keys can't span lines in flow).
fn check_for_plain_mapping_key(
  full_scalar: String,
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  // Don't skip newlines - implicit keys must be on a single line
  case current(parser) {
    Some(lexer.Colon) -> {
      use parser <- result.try(skip_flow_whitespace(advance(parser)))
      use #(val, parser) <- result.try(parse_flow_value(parser))
      Ok(#(value.Mapping([#(full_scalar, val)]), parser))
    }
    _ -> Ok(#(parse_scalar(full_scalar), parser))
  }
}

/// Check if a quoted string is used as a mapping key.
/// Only checks for colon on the same line (not after newline).
fn check_for_quoted_mapping_key(
  s: String,
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) -> {
      use parser <- result.try(skip_flow_whitespace(advance(parser)))
      use #(val, parser) <- result.try(parse_flow_value(parser))
      Ok(#(value.Mapping([#(s, val)]), parser))
    }
    Some(lexer.Plain(p)) -> {
      case string.starts_with(p, ":") {
        True -> {
          let val = parse_adjacent_value(string.drop_start(p, 1))
          Ok(#(value.Mapping([#(s, val)]), advance(parser)))
        }
        False -> Ok(#(value.String(s), parser))
      }
    }
    // Skip past whitespace for non-mapping-key quoted strings
    Some(lexer.Newline) | Some(lexer.Indent(_)) ->
      Ok(#(value.String(s), parser))
    _ -> Ok(#(value.String(s), parser))
  }
}

/// Parse the value part of an explicit key-value pair.
fn parse_explicit_key_value(
  key: String,
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) -> {
      use #(val, parser) <- result.try(
        parse_flow_value(skip_whitespace(advance(parser))),
      )
      Ok(#(value.Mapping([#(key, val)]), parser))
    }
    _ -> Ok(#(value.Mapping([#(key, value.Null)]), parser))
  }
}

/// Parse a single-pair mapping in a flow sequence (after seeing ?).
fn parse_flow_sequence_single_pair(
  parser: Parser,
  _saw_question: Bool,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      let #(full_key, parser) =
        collect_flow_plain_scalar_parts(advance(parser), s)
      parse_explicit_key_value(full_key, skip_whitespace(parser))
    }

    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      parse_explicit_key_value(s, skip_whitespace(advance(parser)))

    Some(lexer.Colon) -> {
      use #(val, parser) <- result.try(
        parse_flow_value(skip_whitespace(advance(parser))),
      )
      Ok(#(value.Mapping([#("", val)]), parser))
    }
    // No key or colon - empty mapping with empty key
    _ -> {
      Ok(#(value.Mapping([#("", value.Null)]), parser))
    }
  }
}

/// Parse a flow mapping.
pub fn parse_flow_mapping(
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  use parser <- result.try(skip_flow_whitespace(parser))
  parse_flow_mapping_pairs(parser, [])
}

fn parse_flow_mapping_pairs(
  parser: Parser,
  acc: List(#(String, YamlValue)),
) -> Result(#(YamlValue, Parser), ParseError) {
  use parser <- result.try(skip_flow_whitespace(parser))

  case current(parser) {
    Some(lexer.BraceClose) ->
      Ok(#(value.Mapping(list.reverse(acc)), advance(parser)))

    Some(lexer.Eof) | option.None ->
      Error(ParseError("Unterminated flow mapping", parser.pos))

    Some(lexer.Question) ->
      parse_explicit_flow_mapping_pair(skip_whitespace(advance(parser)), acc)

    _ -> {
      use #(key, parser) <- result.try(parse_flow_key_with_colon(parser))
      parse_flow_mapping_value(key, skip_whitespace(parser), acc)
    }
  }
}

/// Parse an explicit key-value pair in a flow mapping (after ?).
fn parse_explicit_flow_mapping_pair(
  parser: Parser,
  acc: List(#(String, YamlValue)),
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.BraceClose) ->
      Ok(#(
        value.Mapping(list.reverse([#("", value.Null), ..acc])),
        advance(parser),
      ))

    Some(lexer.Comma) ->
      parse_flow_mapping_pairs(advance(parser), [#("", value.Null), ..acc])

    Some(lexer.Colon) ->
      parse_flow_mapping_colon_value("", skip_whitespace(advance(parser)), acc)

    _ -> {
      use #(key, parser) <- result.try(parse_flow_key_with_colon(parser))
      let parser = skip_whitespace(parser)
      case current(parser) {
        Some(lexer.Colon) ->
          parse_flow_mapping_colon_value(
            key,
            skip_whitespace(advance(parser)),
            acc,
          )
        Some(lexer.Comma) ->
          parse_flow_mapping_pairs(advance(parser), [#(key, value.Null), ..acc])
        Some(lexer.BraceClose) ->
          Ok(#(
            value.Mapping(list.reverse([#(key, value.Null), ..acc])),
            advance(parser),
          ))
        _ -> Error(ParseError("Expected ':', ',' or '}'", parser.pos))
      }
    }
  }
}

/// Parse the value part of a flow mapping pair after seeing colon.
fn parse_flow_mapping_colon_value(
  key: String,
  parser: Parser,
  acc: List(#(String, YamlValue)),
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Comma) ->
      parse_flow_mapping_pairs(advance(parser), [#(key, value.Null), ..acc])
    Some(lexer.BraceClose) ->
      Ok(#(
        value.Mapping(list.reverse([#(key, value.Null), ..acc])),
        advance(parser),
      ))
    _ -> {
      use #(val, parser) <- result.try(parse_flow_value(parser))
      continue_flow_mapping(key, val, parser, acc)
    }
  }
}

/// Continue parsing flow mapping after parsing a value.
fn continue_flow_mapping(
  key: String,
  val: YamlValue,
  parser: Parser,
  acc: List(#(String, YamlValue)),
) -> Result(#(YamlValue, Parser), ParseError) {
  let acc = [#(key, val), ..acc]
  use parser <- result.try(skip_flow_whitespace(parser))
  case current(parser) {
    Some(lexer.Comma) -> parse_flow_mapping_pairs(advance(parser), acc)
    Some(lexer.BraceClose) ->
      Ok(#(value.Mapping(list.reverse(acc)), advance(parser)))
    _ -> Error(ParseError("Expected ',' or '}'", parser.pos))
  }
}

/// Parse a flow mapping value (handles regular colon and adjacent colon patterns).
fn parse_flow_mapping_value(
  key: String,
  parser: Parser,
  acc: List(#(String, YamlValue)),
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      case string.starts_with(s, ":") {
        True -> {
          let val = parse_adjacent_value(string.drop_start(s, 1))
          continue_flow_mapping(key, val, advance(parser), acc)
        }
        False -> Error(ParseError("Expected ':', ',' or '}'", parser.pos))
      }
    }

    Some(lexer.Colon) ->
      parse_flow_mapping_colon_value(key, skip_whitespace(advance(parser)), acc)

    Some(lexer.Comma) ->
      parse_flow_mapping_pairs(advance(parser), [#(key, value.Null), ..acc])

    Some(lexer.BraceClose) -> {
      let acc = [#(key, value.Null), ..acc]
      Ok(#(value.Mapping(list.reverse(acc)), advance(parser)))
    }

    _ -> Error(ParseError("Expected ':', ',' or '}'", parser.pos))
  }
}

/// Parse a flow key that might contain colons (like URLs) or be complex (like [a,b]).
pub fn parse_flow_key_with_colon(
  parser: Parser,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> collect_flow_key_parts(advance(parser), s)
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      Ok(#(s, advance(parser)))
    Some(lexer.BracketOpen) ->
      parse_flow_collection_key(parser, parse_flow_sequence)
    Some(lexer.BraceOpen) ->
      parse_flow_collection_key(parser, parse_flow_mapping)
    Some(lexer.Anchor(name)) -> parse_anchored_flow_key(parser, name)
    Some(lexer.Alias(name)) -> parse_alias_flow_key(parser, name)
    Some(lexer.Tag(tag)) -> parse_tagged_flow_key(parser, tag)
    Some(lexer.Colon) -> Ok(#("", parser))
    _ -> Error(ParseError("Expected mapping key", parser.pos))
  }
}

/// Parse a flow collection (sequence or mapping) as a mapping key.
fn parse_flow_collection_key(
  parser: Parser,
  parse_fn: fn(Parser) -> Result(#(YamlValue, Parser), ParseError),
) -> Result(#(String, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_fn(advance(parser)))
  Ok(#(value_to_key_string(val), parser))
}

/// Parse an anchored flow key — the anchor may precede a flow collection or plain key.
fn parse_anchored_flow_key(
  parser: Parser,
  name: String,
) -> Result(#(String, Parser), ParseError) {
  let parser = advance(parser) |> skip_whitespace
  case current(parser) {
    Some(lexer.BracketOpen) | Some(lexer.BraceOpen) -> {
      let parse_fn = case current(parser) {
        Some(lexer.BracketOpen) -> parse_flow_sequence
        _ -> parse_flow_mapping
      }
      use #(val, parser) <- result.try(parse_fn(advance(parser)))
      let parser =
        Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
      Ok(#(value_to_key_string(val), parser))
    }
    _ -> {
      use #(key, parser) <- result.try(parse_flow_key_with_colon(parser))
      let parser =
        Parser(
          ..parser,
          anchors: dict.insert(parser.anchors, name, value.String(key)),
        )
      Ok(#(key, parser))
    }
  }
}

/// Parse an alias reference as a flow mapping key.
fn parse_alias_flow_key(
  parser: Parser,
  name: String,
) -> Result(#(String, Parser), ParseError) {
  let parser = advance(parser)
  case dict.get(parser.anchors, name) {
    Ok(val) -> Ok(#(value_to_key_string(val), parser))
    Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
  }
}

/// Parse a tagged flow mapping key.
fn parse_tagged_flow_key(
  parser: Parser,
  tag: String,
) -> Result(#(String, Parser), ParseError) {
  let parser = advance(parser) |> skip_whitespace
  let is_str_tag =
    tag == "!!str" || string.contains(tag, "tag:taffy.org,2002:str")
  case current(parser), is_str_tag {
    Some(lexer.Colon), True -> Ok(#("", parser))
    _, _ -> parse_flow_key_with_colon(parser)
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
      // Validate flow indentation
      case skip_flow_whitespace(parser) {
        Error(_) -> Ok(#(string.trim_end(acc), parser))
        Ok(parser_after_ws) ->
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
    }
    _ -> Ok(#(string.trim_end(acc), parser))
  }
}

/// Collect parts of a plain scalar in a flow sequence that might span multiple lines.
/// This is similar to collect_flow_key_parts but for values, not keys.
fn collect_flow_plain_scalar_parts(
  parser: Parser,
  acc: String,
) -> #(String, Parser) {
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
      // Comments break scalar continuation in flow context
      case flow_whitespace_has_comment(parser) {
        True -> #(string.trim_end(acc), parser)
        False ->
          // Validate flow indentation
          case skip_flow_whitespace(parser) {
            Error(_) -> #(string.trim_end(acc), parser)
            Ok(parser_after_ws) ->
              case current(parser_after_ws) {
                Some(lexer.Plain(s)) -> {
                  // Continue with a space between the parts
                  let parser = advance(parser_after_ws)
                  collect_flow_plain_scalar_parts(parser, acc <> " " <> s)
                }
                // If not followed by plain text, return parser BEFORE the newline
                _ -> #(string.trim_end(acc), parser)
              }
          }
      }
    }
    _ -> #(string.trim_end(acc), parser)
  }
}

/// Parse a flow value.
pub fn parse_flow_value(
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  use parser <- result.try(skip_flow_whitespace(parser))

  case current(parser) {
    Some(lexer.BracketOpen) -> parse_flow_sequence(advance(parser))
    Some(lexer.BraceOpen) -> parse_flow_mapping(advance(parser))
    Some(lexer.Plain(s)) -> Ok(#(parse_scalar(s), advance(parser)))
    Some(lexer.SingleQuoted(s)) -> Ok(#(value.String(s), advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(value.String(s), advance(parser)))
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_whitespace
      use #(val, parser) <- result.try(parse_flow_value(parser))
      let parser =
        Parser(..parser, anchors: dict.insert(parser.anchors, name, val))
      Ok(#(val, parser))
    }
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(val, parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    // Handle tags - check for string tag with empty value
    Some(lexer.Tag(tag)) -> {
      let parser = advance(parser) |> skip_whitespace
      let is_str_tag =
        tag == "!!str" || string.contains(tag, "tag:taffy.org,2002:str")
      case current(parser), is_str_tag {
        // String tag followed by separator = empty string
        Some(lexer.Comma), True
        | Some(lexer.BraceClose), True
        | Some(lexer.BracketClose), True
        | Some(lexer.Eof), True
        | option.None, True
        -> Ok(#(value.String(""), parser))
        _, _ -> parse_flow_value(parser)
      }
    }
    _ -> Ok(#(value.Null, parser))
  }
}
