//// Block collection parsing (sequences and mappings in block style).

import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
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
  let sei = case min_indent {
    0 -> Some(0)
    _ -> None
  }
  let parser = Parser(..parser, seq_entry_indent: sei)
  parse_block_sequence_items(parser, min_indent, [], parse_value_fn)
}

fn parse_block_sequence_items(
  parser: Parser,
  min_indent: Int,
  acc: List(YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_block_sequence_items_col(parser, min_indent, None, acc, parse_value_fn)
}

fn parse_block_sequence_items_col(
  parser: Parser,
  min_indent: Int,
  seq_col: Option(Int),
  acc: List(YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)
  let is_first_item = list.is_empty(acc)

  case current(parser) {
    // Raw Dash at indent 0: valid if first item or min_indent allows it
    Some(lexer.Dash) if is_first_item || min_indent == 0 ->
      parse_sequence_item_col(
        parser,
        min_indent,
        min_indent + 1,
        seq_col,
        acc,
        parse_value_fn,
      )

    Some(lexer.Indent(n)) -> {
      // Check indent matching for sequences
      let accepted = case seq_col {
        Some(col) -> n == col
        None -> n >= min_indent
      }
      case accepted {
        True -> {
          let new_col = case seq_col {
            Some(_) -> seq_col
            None -> Some(n)
          }
          let parser = advance(parser)
          case current(parser) {
            Some(lexer.Dash) ->
              parse_sequence_item_col(
                parser,
                min_indent,
                n + 1,
                new_col,
                acc,
                parse_value_fn,
              )
            _ -> Ok(#(value.Sequence(list.reverse(acc)), parser))
          }
        }
        False -> Ok(#(value.Sequence(list.reverse(acc)), parser))
      }
    }
    _ -> Ok(#(value.Sequence(list.reverse(acc)), parser))
  }
}

fn parse_sequence_item_col(
  parser: Parser,
  min_indent: Int,
  item_indent: Int,
  seq_col: Option(Int),
  acc: List(YamlValue),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = advance(parser) |> skip_spaces
  let saved_sei = parser.seq_entry_indent
  use #(val, parser) <- result.try(parse_value_fn(parser, item_indent))
  let parser = Parser(..parser, seq_entry_indent: saved_sei)
  parse_block_sequence_items_col(
    parser,
    min_indent,
    seq_col,
    [val, ..acc],
    parse_value_fn,
  )
}

/// Parse a block mapping starting from a key that was already parsed.
/// `first_key_col` optionally specifies the column of the first key
/// (for enforcing consistent indentation of subsequent keys).
pub fn parse_block_mapping_from_key(
  first_key: String,
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_block_mapping_from_key_col(
    first_key,
    parser,
    min_indent,
    None,
    parse_value_fn,
  )
}

/// Parse a block mapping with known first key column.
pub fn parse_block_mapping_from_key_col(
  first_key: String,
  parser: Parser,
  min_indent: Int,
  first_key_col: Option(Int),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_spaces(parser)

  // Use parse_mapping_value to handle all the complex cases (anchors, sequences, etc.)
  case parse_mapping_value(parser, min_indent, parse_value_fn) {
    Ok(#(first_val, parser)) -> {
      let initial = [#(first_key, first_val)]
      parse_block_mapping_pairs_col(
        parser,
        min_indent,
        first_key_col,
        initial,
        parse_value_fn,
      )
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
  case
    parse_mapping_value_with_anchor(parser, key_indent, None, parse_value_fn)
  {
    Ok(#(val, parser)) -> Ok(#(val, Parser(..parser, in_inline_value: False)))
    err -> err
  }
}

fn parse_mapping_value_with_anchor(
  parser: Parser,
  key_indent: Int,
  anchor: Option(String),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Newline) ->
      parse_mapping_value_after_newline(
        advance(parser),
        key_indent,
        anchor,
        parse_value_fn,
      )

    // Comments after key: skip and look for value on next line
    Some(lexer.Comment(_)) ->
      parse_mapping_value_with_anchor(
        advance(parser),
        key_indent,
        anchor,
        parse_value_fn,
      )

    Some(lexer.Tag(_)) ->
      parse_mapping_value_with_anchor(
        advance(parser) |> skip_spaces,
        key_indent,
        anchor,
        parse_value_fn,
      )

    Some(lexer.Anchor(name)) ->
      parse_mapping_value_after_anchor(
        advance(parser) |> skip_spaces,
        key_indent,
        name,
        parse_value_fn,
      )

    Some(lexer.Indent(n)) ->
      parse_indented_mapping_value(
        advance(parser),
        key_indent,
        n,
        anchor,
        parse_value_fn,
      )

    Some(lexer.Dash) ->
      Error(ParseError(
        "Block sequence not allowed on same line as mapping key",
        parser.pos,
      ))

    _ -> {
      let parser = Parser(..parser, in_inline_value: True)
      parse_value_with_anchor(parser, key_indent + 1, anchor, parse_value_fn)
    }
  }
}

/// Parse a value and wrap it with an optional anchor.
fn parse_value_with_anchor(
  parser: Parser,
  min_indent: Int,
  anchor: Option(String),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_value_fn(parser, min_indent))
  wrap_with_anchor(val, anchor, parser)
}

/// Handle indented mapping value (after seeing Indent token).
fn parse_indented_mapping_value(
  parser: Parser,
  key_indent: Int,
  indent_level: Int,
  anchor: Option(String),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Dash) if indent_level >= key_indent -> {
      let parser = Parser(..parser, pos: parser.pos - 1)
      use #(val, parser) <- result.try(parse_block_sequence(
        parser,
        indent_level,
        parse_value_fn,
      ))
      wrap_with_anchor(val, anchor, parser)
    }
    Some(lexer.Anchor(name)) ->
      parse_mapping_value_after_anchor(
        advance(parser) |> skip_spaces,
        key_indent,
        name,
        parse_value_fn,
      )
    _ -> {
      let parser = Parser(..parser, pos: parser.pos - 1)
      parse_value_with_anchor(parser, key_indent + 1, anchor, parse_value_fn)
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
    Some(lexer.Indent(n)) ->
      parse_indented_mapping_value(
        advance(parser),
        key_indent,
        n,
        anchor,
        parse_value_fn,
      )

    Some(lexer.Dash) if key_indent == 0 -> {
      use #(val, parser) <- result.try(parse_block_sequence(
        parser,
        0,
        parse_value_fn,
      ))
      wrap_with_anchor(val, anchor, parser)
    }

    // Anchor at indent 0 after newline: only valid as value if key_indent > 0
    // If key_indent == 0, the anchor is at the same level as the key, not its value
    Some(lexer.Anchor(name)) if key_indent > 0 ->
      parse_mapping_value_after_anchor(
        advance(parser) |> skip_spaces,
        key_indent,
        name,
        parse_value_fn,
      )

    // Skip comments and newlines, continue looking for value
    Some(lexer.Comment(_)) ->
      parse_mapping_value_after_newline(
        advance(parser),
        key_indent,
        anchor,
        parse_value_fn,
      )

    Some(lexer.Newline) ->
      parse_mapping_value_after_newline(
        advance(parser),
        key_indent,
        anchor,
        parse_value_fn,
      )

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
    // Reject anchor followed by alias (can't anchor an alias reference)
    Some(lexer.Alias(_)) ->
      Error(ParseError("Cannot place an anchor on an alias", parser.pos))
    // Reject double anchors
    Some(lexer.Anchor(_)) ->
      Error(ParseError("A node can only have one anchor", parser.pos))
    Some(lexer.Newline) ->
      parse_anchored_value_after_newline(
        advance(parser),
        key_indent,
        anchor_name,
        parse_value_fn,
      )
    Some(lexer.Indent(n)) if n > key_indent -> {
      // Check if the indented content starts with another anchor on a scalar
      // (double anchor on same node is invalid)
      let after_indent = advance(parser)
      case current(after_indent) {
        Some(lexer.Anchor(_)) -> {
          // Peek ahead: if anchor is on a mapping key, it's OK (different node)
          // If anchor is on a plain scalar (no colon after), it's a double anchor
          let after_anchor = advance(after_indent) |> skip_spaces
          case current(after_anchor) {
            Some(lexer.Plain(_)) -> {
              let after_plain = advance(after_anchor) |> skip_spaces
              case current(after_plain) {
                Some(lexer.Colon) ->
                  // Anchor on mapping key - valid (different nodes)
                  parse_and_register_anchor(
                    parser,
                    key_indent + 1,
                    anchor_name,
                    parse_value_fn,
                  )
                _ ->
                  // Anchor on scalar value - double anchor
                  Error(ParseError(
                    "A node can only have one anchor",
                    after_indent.pos,
                  ))
              }
            }
            Some(lexer.SingleQuoted(_)) | Some(lexer.DoubleQuoted(_)) -> {
              let after_quoted = advance(after_anchor) |> skip_spaces
              case current(after_quoted) {
                Some(lexer.Colon) ->
                  parse_and_register_anchor(
                    parser,
                    key_indent + 1,
                    anchor_name,
                    parse_value_fn,
                  )
                _ ->
                  Error(ParseError(
                    "A node can only have one anchor",
                    after_indent.pos,
                  ))
              }
            }
            _ ->
              // Not a mapping key pattern - assume valid (let parse_value handle it)
              parse_and_register_anchor(
                parser,
                key_indent + 1,
                anchor_name,
                parse_value_fn,
              )
          }
        }
        _ ->
          parse_and_register_anchor(
            parser,
            key_indent + 1,
            anchor_name,
            parse_value_fn,
          )
      }
    }
    _ ->
      parse_and_register_anchor(
        parser,
        key_indent + 1,
        anchor_name,
        parse_value_fn,
      )
  }
}

/// Parse value after newline following an anchor.
fn parse_anchored_value_after_newline(
  parser: Parser,
  key_indent: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Dash) if key_indent == 0 ->
      parse_sequence_and_register_anchor(parser, 0, anchor_name, parse_value_fn)

    Some(lexer.Indent(n)) ->
      parse_anchored_indented_value(
        advance(parser),
        key_indent,
        n,
        anchor_name,
        parse_value_fn,
      )

    _ -> register_anchor_with_null(parser, anchor_name)
  }
}

/// Parse indented value after anchor.
fn parse_anchored_indented_value(
  parser: Parser,
  key_indent: Int,
  indent_level: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    // Reject double anchor on same node
    Some(lexer.Anchor(_)) ->
      Error(ParseError("A node can only have one anchor", parser.pos))
    Some(lexer.Dash) if indent_level >= key_indent -> {
      let parser = Parser(..parser, pos: parser.pos - 1)
      parse_sequence_and_register_anchor(
        parser,
        indent_level,
        anchor_name,
        parse_value_fn,
      )
    }
    _ if indent_level > key_indent -> {
      let parser = Parser(..parser, pos: parser.pos - 1)
      parse_and_register_anchor(
        parser,
        key_indent + 1,
        anchor_name,
        parse_value_fn,
      )
    }
    _ -> {
      let parser = Parser(..parser, pos: parser.pos - 1)
      register_anchor_with_null(parser, anchor_name)
    }
  }
}

/// Parse a value and register it with the anchor.
fn parse_and_register_anchor(
  parser: Parser,
  min_indent: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_value_fn(parser, min_indent))
  let parser =
    Parser(..parser, anchors: dict.insert(parser.anchors, anchor_name, val))
  Ok(#(val, parser))
}

/// Parse a block sequence and register it with the anchor.
fn parse_sequence_and_register_anchor(
  parser: Parser,
  min_indent: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_block_sequence(
    parser,
    min_indent,
    parse_value_fn,
  ))
  let parser =
    Parser(..parser, anchors: dict.insert(parser.anchors, anchor_name, val))
  Ok(#(val, parser))
}

/// Register anchor with null value.
fn register_anchor_with_null(
  parser: Parser,
  anchor_name: String,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser =
    Parser(
      ..parser,
      anchors: dict.insert(parser.anchors, anchor_name, value.Null),
    )
  Ok(#(value.Null, parser))
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
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_block_mapping_pairs_col(parser, min_indent, None, acc, parse_value_fn)
}

fn parse_block_mapping_pairs_col(
  parser: Parser,
  min_indent: Int,
  mapping_col: Option(Int),
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)
  let done = Ok(#(value.Mapping(acc), parser))

  case current(parser) {
    Some(lexer.Indent(n)) -> {
      // Check indent matching:
      // - If mapping_col is set, must match exactly
      // - If min_indent == 0, reject (indent-0 keys come through Newline path)
      // - Otherwise, n >= min_indent establishes the column
      let accepted = case mapping_col {
        Some(col) -> n == col
        None ->
          case min_indent {
            0 -> False
            _ -> n >= min_indent
          }
      }
      case accepted {
        True -> {
          let new_col = case mapping_col {
            Some(_) -> mapping_col
            None -> Some(n)
          }
          parse_indented_mapping_pair_col(
            advance(parser),
            min_indent,
            n,
            new_col,
            acc,
            parser,
            parse_value_fn,
          )
        }
        False -> done
      }
    }

    // Tokens at indent 0 - only valid if min_indent == 0
    Some(lexer.Colon) if min_indent == 0 ->
      add_key_value_pair_col(
        "",
        advance(parser) |> skip_spaces,
        min_indent,
        min_indent,
        Some(0),
        acc,
        parse_value_fn,
      )

    Some(lexer.Question) if min_indent == 0 ->
      parse_explicit_key_in_mapping(parser, min_indent, 0, acc, parse_value_fn)

    Some(lexer.Plain(s)) if min_indent == 0 -> {
      let after_key = advance(parser) |> skip_spaces
      case current(after_key) {
        Some(lexer.Colon) ->
          add_key_value_pair_col(
            s,
            advance(after_key) |> skip_spaces,
            min_indent,
            min_indent,
            Some(0),
            acc,
            parse_value_fn,
          )
        _ -> done
      }
    }

    Some(lexer.SingleQuoted(s)) if min_indent == 0 -> {
      let after_key = advance(parser) |> skip_spaces
      case current(after_key) {
        Some(lexer.Colon) ->
          add_key_value_pair_col(
            s,
            advance(after_key) |> skip_spaces,
            min_indent,
            min_indent,
            Some(0),
            acc,
            parse_value_fn,
          )
        _ -> done
      }
    }

    Some(lexer.DoubleQuoted(s)) if min_indent == 0 -> {
      let after_key = advance(parser) |> skip_spaces
      case current(after_key) {
        Some(lexer.Colon) ->
          add_key_value_pair_col(
            s,
            advance(after_key) |> skip_spaces,
            min_indent,
            min_indent,
            Some(0),
            acc,
            parse_value_fn,
          )
        _ -> done
      }
    }

    Some(lexer.Alias(name)) if min_indent == 0 ->
      parse_alias_key(
        name,
        advance(parser) |> skip_spaces,
        min_indent,
        acc,
        parse_value_fn,
      )

    Some(lexer.Anchor(name)) if min_indent == 0 ->
      parse_anchored_key(
        name,
        advance(parser) |> skip_spaces,
        min_indent,
        acc,
        parse_value_fn,
      )

    Some(lexer.Tag(_)) if min_indent == 0 ->
      parse_tagged_key(
        advance(parser) |> skip_spaces,
        min_indent,
        acc,
        parse_value_fn,
      )

    _ -> done
  }
}

/// Parse a mapping pair after seeing an Indent token (with column tracking).
fn parse_indented_mapping_pair_col(
  parser: Parser,
  min_indent: Int,
  indent_level: Int,
  mapping_col: Option(Int),
  acc: List(#(String, YamlValue)),
  pre_indent_parser: Parser,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let backtrack = Ok(#(value.Mapping(acc), pre_indent_parser))
  case current(parser) {
    Some(lexer.Colon) ->
      add_key_value_pair_col(
        "",
        advance(parser) |> skip_spaces,
        indent_level,
        min_indent,
        mapping_col,
        acc,
        parse_value_fn,
      )

    Some(lexer.Question) ->
      parse_explicit_key_in_mapping(
        parser,
        min_indent,
        indent_level,
        acc,
        parse_value_fn,
      )

    _ -> {
      case parse_mapping_key(parser) {
        Ok(#(key, parser)) -> {
          let parser = skip_spaces(parser)
          case current(parser) {
            Some(lexer.Colon) ->
              add_key_value_pair_col(
                key,
                advance(parser) |> skip_spaces,
                indent_level,
                min_indent,
                mapping_col,
                acc,
                parse_value_fn,
              )
            _ -> backtrack
          }
        }
        Error(_) -> backtrack
      }
    }
  }
}

/// Add a key-value pair to the mapping and continue parsing (with column tracking).
fn add_key_value_pair_col(
  key: String,
  parser: Parser,
  key_indent: Int,
  mapping_indent: Int,
  mapping_col: Option(Int),
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_mapping_value(
    parser,
    key_indent,
    parse_value_fn,
  ))
  let acc = value.ordered_insert(acc, key, val)
  parse_block_mapping_pairs_col(
    parser,
    mapping_indent,
    mapping_col,
    acc,
    parse_value_fn,
  )
}

/// Add a key-value pair to the mapping and continue parsing.
fn add_key_value_pair(
  key: String,
  parser: Parser,
  key_indent: Int,
  mapping_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  add_key_value_pair_col(
    key,
    parser,
    key_indent,
    mapping_indent,
    None,
    acc,
    parse_value_fn,
  )
}

/// Parse an alias used as a mapping key.
fn parse_alias_key(
  name: String,
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case dict.get(parser.anchors, name) {
    Ok(key_val) -> {
      let key = value_to_string(key_val)
      let parser = skip_spaces(parser)
      case current(parser) {
        Some(lexer.Colon) ->
          add_key_value_pair(
            key,
            advance(parser) |> skip_spaces,
            min_indent,
            min_indent,
            acc,
            parse_value_fn,
          )
        _ -> Ok(#(value.Mapping(acc), parser))
      }
    }
    Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
  }
}

/// Parse an anchored mapping key.
fn parse_anchored_key(
  anchor_name: String,
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  // Reject anchor+alias as key
  case current(parser) {
    Some(lexer.Alias(_)) ->
      Error(ParseError("Cannot place an anchor on an alias", parser.pos))
    _ ->
      case parse_mapping_key(parser) {
        Ok(#(key, parser)) -> {
          let parser = skip_spaces(parser)
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = advance(parser) |> skip_spaces
              use #(val, parser) <- result.try(parse_mapping_value(
                parser,
                min_indent,
                parse_value_fn,
              ))
              let parser =
                Parser(
                  ..parser,
                  anchors: dict.insert(
                    parser.anchors,
                    anchor_name,
                    value.String(key),
                  ),
                )
              let acc = value.ordered_insert(acc, key, val)
              parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
            }
            _ -> Ok(#(value.Mapping(acc), parser))
          }
        }
        Error(_) -> Ok(#(value.Mapping(acc), parser))
      }
  }
}

/// Parse a tagged mapping key (skip tag and parse key).
fn parse_tagged_key(
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case parse_mapping_key(parser) {
    Ok(#(key, parser)) -> {
      let parser = skip_spaces(parser)
      case current(parser) {
        Some(lexer.Colon) ->
          add_key_value_pair(
            key,
            advance(parser) |> skip_spaces,
            min_indent,
            min_indent,
            acc,
            parse_value_fn,
          )
        _ -> Ok(#(value.Mapping(acc), parser))
      }
    }
    Error(_) -> Ok(#(value.Mapping(acc), parser))
  }
}

/// Parse a mapping key.
/// In block context, flow indicators like `,`, `[`, `]`, `{`, `}` are plain characters.
pub fn parse_mapping_key(
  parser: Parser,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> collect_block_key(advance(parser), s)

    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      Ok(#(s, advance(parser)))

    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(value_to_string(val), parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }

    Some(lexer.Anchor(name)) -> {
      use #(key, parser) <- result.try(parse_mapping_key(
        advance(parser) |> skip_spaces,
      ))
      let parser =
        Parser(
          ..parser,
          anchors: dict.insert(parser.anchors, name, value.String(key)),
        )
      Ok(#(key, parser))
    }

    Some(lexer.Tag(_)) -> parse_mapping_key(advance(parser) |> skip_spaces)

    _ -> Error(ParseError("Expected mapping key", parser.pos))
  }
}

/// Collect additional tokens that form part of a block key.
/// In block context, flow indicators (`,`, `[`, `]`, `{`, `}`) are plain characters.
/// The key ends when we see a colon followed by whitespace.
fn collect_block_key(
  parser: Parser,
  acc: String,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    // Flow indicators in block context are part of the key
    Some(lexer.Comma) -> collect_block_key(advance(parser), acc <> ",")
    Some(lexer.BracketOpen) -> collect_block_key(advance(parser), acc <> "[")
    Some(lexer.BracketClose) -> collect_block_key(advance(parser), acc <> "]")
    Some(lexer.BraceOpen) -> collect_block_key(advance(parser), acc <> "{")
    Some(lexer.BraceClose) -> collect_block_key(advance(parser), acc <> "}")
    Some(lexer.Plain(s)) -> collect_block_key(advance(parser), acc <> s)

    Some(lexer.Colon) -> {
      let after_colon = advance(parser)
      case is_key_terminator(current(after_colon)) {
        True -> Ok(#(acc, parser))
        False -> collect_block_key(after_colon, acc <> ":")
      }
    }

    _ -> Ok(#(acc, parser))
  }
}

/// Check if a token indicates the end of a key (colon separator).
fn is_key_terminator(token: Option(lexer.Token)) -> Bool {
  case token {
    Some(lexer.Plain(_))
    | Some(lexer.SingleQuoted(_))
    | Some(lexer.DoubleQuoted(_)) -> True
    Some(lexer.Newline)
    | Some(lexer.Indent(_))
    | Some(lexer.Eof)
    | Some(lexer.Comment(_)) -> True
    Some(lexer.Anchor(_)) | Some(lexer.Alias(_)) | Some(lexer.Tag(_)) -> True
    Some(lexer.Dash)
    | Some(lexer.Question)
    | Some(lexer.Literal(_))
    | Some(lexer.Folded(_)) -> True
    // Flow collection starts after colon indicate the colon is a key separator
    Some(lexer.BraceOpen) | Some(lexer.BracketOpen) -> True
    None -> True
    _ -> False
  }
}

/// Parse an explicit key (?) within a block mapping.
fn parse_explicit_key_in_mapping(
  parser: Parser,
  min_indent: Int,
  key_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = advance(parser) |> skip_spaces
  use #(key, parser) <- result.try(parse_explicit_key(parser, parse_value_fn))

  let parser = skip_newlines_and_comments(parser)
  handle_explicit_key_value(
    key,
    parser,
    min_indent,
    key_indent,
    acc,
    parse_value_fn,
  )
}

/// Handle the value part after parsing an explicit key.
fn handle_explicit_key_value(
  key: String,
  parser: Parser,
  min_indent: Int,
  key_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) -> {
      use #(val, parser) <- result.try(parse_value_fn(
        advance(parser) |> skip_spaces,
        key_indent + 1,
      ))
      parse_block_mapping_pairs(
        parser,
        min_indent,
        value.ordered_insert(acc, key, val),
        parse_value_fn,
      )
    }

    Some(lexer.Indent(n)) ->
      handle_indented_explicit_key_value(
        key,
        advance(parser),
        min_indent,
        n,
        acc,
        parse_value_fn,
      )

    Some(lexer.Question) -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_explicit_key_in_mapping(
        parser,
        min_indent,
        key_indent,
        acc,
        parse_value_fn,
      )
    }

    _ -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
    }
  }
}

/// Handle indented content after an explicit key.
fn handle_indented_explicit_key_value(
  key: String,
  parser: Parser,
  min_indent: Int,
  indent_level: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) -> {
      use #(val, parser) <- result.try(parse_value_fn(
        advance(parser) |> skip_spaces,
        indent_level + 1,
      ))
      parse_block_mapping_pairs(
        parser,
        min_indent,
        value.ordered_insert(acc, key, val),
        parse_value_fn,
      )
    }

    Some(lexer.Question) -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_explicit_key_in_mapping(
        parser,
        min_indent,
        indent_level,
        acc,
        parse_value_fn,
      )
    }

    Some(lexer.Plain(s)) -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      let parser = advance(parser) |> skip_spaces
      case current(parser) {
        Some(lexer.Colon) -> {
          use #(val, parser) <- result.try(parse_value_fn(
            advance(parser) |> skip_spaces,
            indent_level + 1,
          ))
          parse_block_mapping_pairs(
            parser,
            min_indent,
            value.ordered_insert(acc, s, val),
            parse_value_fn,
          )
        }
        _ -> Ok(#(value.Mapping(acc), parser))
      }
    }

    _ -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_block_mapping_pairs(parser, min_indent, acc, parse_value_fn)
    }
  }
}

/// Collect a multiline explicit key value.
/// Stops at `:` (value separator) or when indent decreases.
fn collect_explicit_key_value(parser: Parser, acc: String) -> #(String, Parser) {
  case current(parser) {
    // Plain text continuation
    Some(lexer.Plain(s)) ->
      collect_explicit_key_value(advance(parser), acc <> " " <> s)
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
            Some(lexer.Plain(s)) ->
              collect_explicit_key_value(advance(after_indent), acc <> " " <> s)
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
        Some(lexer.Plain(s)) ->
          collect_explicit_key_value(advance(after_indent), acc <> " " <> s)
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
fn parse_explicit_key(
  parser: Parser,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
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
                Ok(#(seq_val, parser)) ->
                  Ok(#(value_to_string(seq_val), parser))
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
