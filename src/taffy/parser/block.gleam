//// Block collection parsing (sequences and mappings in block style).

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import taffy/lexer
import taffy/parser/helpers.{advance, current, skip_newlines_and_comments}
import taffy/parser/scalar.{value_to_key_string}
import taffy/parser/types.{type ParseError, type Parser, ParseError, Parser}
import taffy/value.{type YamlValue}

pub type ParseValueFn =
  fn(Parser, Int) -> Result(#(YamlValue, Parser), ParseError)

pub fn parse_block_sequence(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  parse_block_sequence_at(parser, min_indent, None, parse_value_fn)
}

/// Variant of `parse_block_sequence` that lets the caller declare the actual
/// column of the first dash. Callers that arrive via an `Indent(n)` token
/// know `n`; top-level callers know `0`. When `dash_col` is `Some`, dashes
/// at any other column are rejected (per YAML 1.2 §8.2.1). When `None`, the
/// caller doesn't know the column and the parser falls back to the older
/// lenient behaviour — the first `Indent`-preceded dash establishes seq_col.
pub fn parse_block_sequence_at(
  parser: Parser,
  min_indent: Int,
  dash_col: Option(Int),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let entry_indent = case min_indent {
    0 -> Some(0)
    _ -> None
  }
  let parser = Parser(..parser, seq_entry_indent: entry_indent)
  parse_block_sequence_items_col(
    parser,
    min_indent,
    dash_col,
    [],
    parse_value_fn,
  )
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
          let after_indent = advance(parser)
          case current(after_indent) {
            Some(lexer.Dash) ->
              parse_sequence_item_col(
                after_indent,
                min_indent,
                n + 1,
                new_col,
                acc,
                parse_value_fn,
              )
            // The Indent didn't introduce another sequence item. Put it back
            // so the enclosing collection can decide whether the next line
            // is a sibling key/dash or terminates its own block.
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
  let parser = advance(parser)
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

pub fn parse_block_mapping_from_key_col(
  first_key: String,
  parser: Parser,
  min_indent: Int,
  first_key_col: Option(Int),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(first_val, parser) <- result.try(parse_mapping_value(
    parser,
    min_indent,
    parse_value_fn,
  ))
  let initial = [#(first_key, first_val)]
  parse_block_mapping_pairs_col(
    parser,
    min_indent,
    first_key_col,
    initial,
    parse_value_fn,
  )
}

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

    Some(lexer.Comment(_)) ->
      parse_mapping_value_with_anchor(
        advance(parser),
        key_indent,
        anchor,
        parse_value_fn,
      )

    Some(lexer.Tag(_)) ->
      parse_mapping_value_with_anchor(
        advance(parser),
        key_indent,
        anchor,
        parse_value_fn,
      )

    Some(lexer.Anchor(name)) ->
      parse_mapping_value_after_anchor(
        advance(parser),
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

fn parse_value_with_anchor(
  parser: Parser,
  min_indent: Int,
  anchor: Option(String),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_value_fn(parser, min_indent))
  wrap_with_anchor(val, anchor, parser)
}

fn parse_indented_mapping_value(
  parser: Parser,
  key_indent: Int,
  indent_level: Int,
  anchor: Option(String),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Dash) if indent_level >= key_indent -> {
      let parser = helpers.backtrack(parser)
      use #(val, parser) <- result.try(parse_block_sequence(
        parser,
        indent_level,
        parse_value_fn,
      ))
      wrap_with_anchor(val, anchor, parser)
    }
    Some(lexer.Anchor(name)) ->
      parse_mapping_value_after_anchor(
        advance(parser),
        key_indent,
        name,
        parse_value_fn,
      )
    _ -> {
      let parser = helpers.backtrack(parser)
      parse_value_with_anchor(parser, key_indent + 1, anchor, parse_value_fn)
    }
  }
}

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

    Some(lexer.Anchor(name)) if key_indent > 0 ->
      parse_mapping_value_after_anchor(
        advance(parser),
        key_indent,
        name,
        parse_value_fn,
      )

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

fn parse_mapping_value_after_anchor(
  parser: Parser,
  key_indent: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Alias(_)) ->
      Error(ParseError("Cannot place an anchor on an alias", parser.pos))
    Some(lexer.Anchor(_)) ->
      Error(ParseError("A node can only have one anchor", parser.pos))
    Some(lexer.Newline) ->
      parse_anchored_value_after_newline(
        advance(parser),
        key_indent,
        anchor_name,
        parse_value_fn,
      )
    Some(lexer.Indent(n)) if n > key_indent ->
      check_double_anchor_then_parse(
        parser,
        key_indent,
        anchor_name,
        parse_value_fn,
      )
    _ ->
      parse_and_register_anchor(
        parser,
        key_indent + 1,
        anchor_name,
        parse_value_fn,
      )
  }
}

fn check_double_anchor_then_parse(
  parser: Parser,
  key_indent: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let after_indent = advance(parser)
  case current(after_indent) {
    Some(lexer.Anchor(_)) ->
      case is_anchor_on_mapping_key(after_indent) {
        True ->
          parse_and_register_anchor(
            parser,
            key_indent + 1,
            anchor_name,
            parse_value_fn,
          )
        False ->
          Error(ParseError("A node can only have one anchor", after_indent.pos))
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

fn is_anchor_on_mapping_key(parser: Parser) -> Bool {
  let after_anchor = advance(parser)
  case current(after_anchor) {
    Some(lexer.Plain(_))
    | Some(lexer.SingleQuoted(_))
    | Some(lexer.DoubleQuoted(_)) -> {
      let after_scalar = advance(after_anchor)
      case current(after_scalar) {
        Some(lexer.Colon) -> True
        _ -> False
      }
    }
    _ -> True
  }
}

fn parse_anchored_value_after_newline(
  parser: Parser,
  key_indent: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Dash) if key_indent == 0 -> {
      use #(val, parser) <- result.try(parse_block_sequence(
        parser,
        0,
        parse_value_fn,
      ))
      let parser = types.register_anchor(parser, anchor_name, val)
      Ok(#(val, parser))
    }

    Some(lexer.Indent(n)) ->
      parse_anchored_indented_value(
        advance(parser),
        key_indent,
        n,
        anchor_name,
        parse_value_fn,
      )

    _ -> {
      let parser = types.register_anchor(parser, anchor_name, value.Null)
      Ok(#(value.Null, parser))
    }
  }
}

fn parse_anchored_indented_value(
  parser: Parser,
  key_indent: Int,
  indent_level: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Anchor(_)) ->
      Error(ParseError("A node can only have one anchor", parser.pos))
    Some(lexer.Dash) if indent_level >= key_indent -> {
      let parser = helpers.backtrack(parser)
      use #(val, parser) <- result.try(parse_block_sequence(
        parser,
        indent_level,
        parse_value_fn,
      ))
      let parser = types.register_anchor(parser, anchor_name, val)
      Ok(#(val, parser))
    }
    _ if indent_level > key_indent -> {
      let parser = helpers.backtrack(parser)
      parse_and_register_anchor(
        parser,
        key_indent + 1,
        anchor_name,
        parse_value_fn,
      )
    }
    _ -> {
      let parser = helpers.backtrack(parser)
      let parser = types.register_anchor(parser, anchor_name, value.Null)
      Ok(#(value.Null, parser))
    }
  }
}

fn parse_and_register_anchor(
  parser: Parser,
  min_indent: Int,
  anchor_name: String,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_value_fn(parser, min_indent))
  let parser = types.register_anchor(parser, anchor_name, val)
  Ok(#(val, parser))
}

fn wrap_with_anchor(
  val: YamlValue,
  anchor: Option(String),
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case anchor {
    Some(name) -> {
      let parser = types.register_anchor(parser, name, val)
      Ok(#(val, parser))
    }
    None -> Ok(#(val, parser))
  }
}

fn parse_block_mapping_pairs_col(
  parser: Parser,
  min_indent: Int,
  mapping_col: Option(Int),
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)
  let done = Ok(#(value.Mapping(list.reverse(acc)), parser))

  case current(parser) {
    Some(lexer.Indent(n)) -> {
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

    Some(lexer.Colon) if min_indent == 0 ->
      add_key_value_pair_col(
        "",
        advance(parser),
        min_indent,
        min_indent,
        Some(0),
        acc,
        parse_value_fn,
      )

    Some(lexer.Question) if min_indent == 0 ->
      parse_explicit_key_in_mapping(parser, min_indent, 0, acc, parse_value_fn)

    Some(lexer.Plain(s))
      | Some(lexer.SingleQuoted(s))
      | Some(lexer.DoubleQuoted(s))
      if min_indent == 0
    ->
      try_scalar_as_mapping_key(
        s,
        parser,
        min_indent,
        acc,
        done,
        parse_value_fn,
      )

    Some(lexer.Alias(name)) if min_indent == 0 ->
      parse_alias_key(name, advance(parser), min_indent, acc, parse_value_fn)

    Some(lexer.Anchor(name)) if min_indent == 0 ->
      parse_anchored_key(name, advance(parser), min_indent, acc, parse_value_fn)

    Some(lexer.Tag(_)) if min_indent == 0 ->
      parse_tagged_key(advance(parser), min_indent, acc, parse_value_fn)

    _ -> done
  }
}

fn parse_indented_mapping_pair_col(
  parser: Parser,
  min_indent: Int,
  indent_level: Int,
  mapping_col: Option(Int),
  acc: List(#(String, YamlValue)),
  pre_indent_parser: Parser,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let backtrack = Ok(#(value.Mapping(list.reverse(acc)), pre_indent_parser))
  case current(parser) {
    Some(lexer.Colon) ->
      add_key_value_pair_col(
        "",
        advance(parser),
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
          case current(parser) {
            Some(lexer.Colon) ->
              add_key_value_pair_col(
                key,
                advance(parser),
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
  let acc = [#(key, val), ..acc]
  parse_block_mapping_pairs_col(
    parser,
    mapping_indent,
    mapping_col,
    acc,
    parse_value_fn,
  )
}

fn try_scalar_as_mapping_key(
  key: String,
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  done: Result(#(YamlValue, Parser), ParseError),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let after_key = advance(parser)
  case current(after_key) {
    Some(lexer.Colon) ->
      add_key_value_pair_col(
        key,
        advance(after_key),
        min_indent,
        min_indent,
        Some(0),
        acc,
        parse_value_fn,
      )
    _ -> done
  }
}

fn parse_alias_key(
  name: String,
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case types.resolve_alias(parser, name) {
    Ok(#(key_val, parser)) -> {
      let key = value_to_key_string(key_val)
      case current(parser) {
        Some(lexer.Colon) ->
          add_key_value_pair_col(
            key,
            advance(parser),
            min_indent,
            min_indent,
            None,
            acc,
            parse_value_fn,
          )
        _ -> Ok(#(value.Mapping(list.reverse(acc)), parser))
      }
    }
    Error("budget exceeded") ->
      Error(ParseError(
        "Alias expansion budget exceeded (possible alias-bomb)",
        parser.pos,
      ))
    Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
  }
}

fn parse_anchored_key(
  anchor_name: String,
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Alias(_)) ->
      Error(ParseError("Cannot place an anchor on an alias", parser.pos))
    _ ->
      case parse_mapping_key(parser) {
        Ok(#(key, parser)) -> {
          case current(parser) {
            Some(lexer.Colon) -> {
              let parser = advance(parser)
              use #(val, parser) <- result.try(parse_mapping_value(
                parser,
                min_indent,
                parse_value_fn,
              ))
              let parser =
                types.register_anchor(parser, anchor_name, value.String(key))
              let acc = [#(key, val), ..acc]
              parse_block_mapping_pairs_col(
                parser,
                min_indent,
                None,
                acc,
                parse_value_fn,
              )
            }
            _ -> Ok(#(value.Mapping(list.reverse(acc)), parser))
          }
        }
        Error(_) -> Ok(#(value.Mapping(list.reverse(acc)), parser))
      }
  }
}

fn parse_tagged_key(
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case parse_mapping_key(parser) {
    Ok(#(key, parser)) -> {
      case current(parser) {
        Some(lexer.Colon) ->
          add_key_value_pair_col(
            key,
            advance(parser),
            min_indent,
            min_indent,
            None,
            acc,
            parse_value_fn,
          )
        _ -> Ok(#(value.Mapping(list.reverse(acc)), parser))
      }
    }
    Error(_) -> Ok(#(value.Mapping(list.reverse(acc)), parser))
  }
}

pub fn parse_mapping_key(
  parser: Parser,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> collect_block_key(advance(parser), s)

    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      Ok(#(s, advance(parser)))

    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case types.resolve_alias(parser, name) {
        Ok(#(val, parser)) -> Ok(#(value_to_key_string(val), parser))
        Error("budget exceeded") ->
          Error(ParseError(
            "Alias expansion budget exceeded (possible alias-bomb)",
            parser.pos,
          ))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }

    Some(lexer.Anchor(name)) -> {
      use #(key, parser) <- result.try(parse_mapping_key(advance(parser)))
      let parser = types.register_anchor(parser, name, value.String(key))
      Ok(#(key, parser))
    }

    Some(lexer.Tag(_)) -> parse_mapping_key(advance(parser))

    _ -> Error(ParseError("Expected mapping key", parser.pos))
  }
}

fn collect_block_key(
  parser: Parser,
  acc: String,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
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
    Some(lexer.BraceOpen) | Some(lexer.BracketOpen) -> True
    None -> True
    _ -> False
  }
}

fn parse_explicit_key_in_mapping(
  parser: Parser,
  min_indent: Int,
  key_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = advance(parser)
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
        advance(parser),
        key_indent + 1,
      ))
      parse_block_mapping_pairs_col(
        parser,
        min_indent,
        None,
        [#(key, val), ..acc],
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
      let acc = [#(key, value.Null), ..acc]
      parse_explicit_key_in_mapping(
        parser,
        min_indent,
        key_indent,
        acc,
        parse_value_fn,
      )
    }

    _ -> {
      let acc = [#(key, value.Null), ..acc]
      parse_block_mapping_pairs_col(
        parser,
        min_indent,
        None,
        acc,
        parse_value_fn,
      )
    }
  }
}

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
        advance(parser),
        indent_level + 1,
      ))
      parse_block_mapping_pairs_col(
        parser,
        min_indent,
        None,
        [#(key, val), ..acc],
        parse_value_fn,
      )
    }

    Some(lexer.Question) -> {
      let acc = [#(key, value.Null), ..acc]
      parse_explicit_key_in_mapping(
        parser,
        min_indent,
        indent_level,
        acc,
        parse_value_fn,
      )
    }

    Some(lexer.Plain(s)) -> {
      let acc = [#(key, value.Null), ..acc]
      let parser = advance(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          use #(val, parser) <- result.try(parse_value_fn(
            advance(parser),
            indent_level + 1,
          ))
          parse_block_mapping_pairs_col(
            parser,
            min_indent,
            None,
            [#(s, val), ..acc],
            parse_value_fn,
          )
        }
        _ -> Ok(#(value.Mapping(list.reverse(acc)), parser))
      }
    }

    _ -> {
      let acc = [#(key, value.Null), ..acc]
      parse_block_mapping_pairs_col(
        parser,
        min_indent,
        None,
        acc,
        parse_value_fn,
      )
    }
  }
}

fn collect_explicit_key_value(parser: Parser, acc: String) -> #(String, Parser) {
  case current(parser) {
    Some(lexer.Plain(s)) ->
      collect_explicit_key_value(advance(parser), acc <> " " <> s)
    Some(lexer.Newline) -> {
      let after_newline = advance(parser)
      case current(after_newline) {
        Some(lexer.Indent(n)) if n > 0 -> {
          let after_indent = advance(after_newline)
          case current(after_indent) {
            Some(lexer.Colon) -> #(acc, after_newline)
            Some(lexer.Plain(s)) ->
              collect_explicit_key_value(advance(after_indent), acc <> " " <> s)
            _ -> #(acc, after_newline)
          }
        }
        _ -> #(acc, parser)
      }
    }
    Some(lexer.Indent(n)) if n > 0 -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        Some(lexer.Colon) -> #(acc, parser)
        Some(lexer.Plain(s)) ->
          collect_explicit_key_value(advance(after_indent), acc <> " " <> s)
        _ -> #(acc, parser)
      }
    }
    Some(lexer.Colon) -> #(acc, parser)
    Some(lexer.Comment(_)) -> #(acc, parser)
    _ -> #(acc, parser)
  }
}

fn parse_explicit_key(
  parser: Parser,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      let #(full_key, parser) = collect_explicit_key_value(advance(parser), s)
      Ok(#(full_key, parser))
    }
    Some(lexer.SingleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.DoubleQuoted(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.Literal(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.Folded(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case types.resolve_alias(parser, name) {
        Ok(#(val, parser)) -> Ok(#(value_to_key_string(val), parser))
        Error("budget exceeded") ->
          Error(ParseError(
            "Alias expansion budget exceeded (possible alias-bomb)",
            parser.pos,
          ))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser)
      use #(key, parser) <- result.try(parse_explicit_key(
        parser,
        parse_value_fn,
      ))
      let parser = types.register_anchor(parser, name, value.String(key))
      Ok(#(key, parser))
    }
    Some(lexer.Tag(_)) -> {
      let parser = advance(parser)
      parse_explicit_key(parser, parse_value_fn)
    }
    Some(lexer.Newline) ->
      parse_explicit_key_after_newline(advance(parser), parser, parse_value_fn)
    Some(lexer.Indent(n)) ->
      parse_explicit_key_after_indent(parser, n, parse_value_fn)
    Some(lexer.Colon) -> Ok(#("", parser))
    None | Some(lexer.Eof) -> Ok(#("", parser))
    Some(lexer.Comment(_)) -> {
      let parser = advance(parser)
      parse_explicit_key(parser, parse_value_fn)
    }
    _ -> Ok(#("", parser))
  }
}

fn parse_explicit_key_after_newline(
  after_newline: Parser,
  original_parser: Parser,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  case current(after_newline) {
    Some(lexer.Dash) ->
      parse_sequence_as_explicit_key(after_newline, 0, parse_value_fn)
    Some(lexer.Indent(n)) ->
      parse_explicit_key_after_newline_indent(after_newline, n, parse_value_fn)
    Some(lexer.Colon) -> Ok(#("", after_newline))
    _ -> Ok(#("", original_parser))
  }
}

fn parse_explicit_key_after_newline_indent(
  indent_parser: Parser,
  n: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  let after_indent = advance(indent_parser)
  case current(after_indent) {
    Some(lexer.Dash) -> {
      let parser = helpers.backtrack(after_indent)
      parse_sequence_as_explicit_key(parser, n, parse_value_fn)
    }
    Some(lexer.Colon) -> Ok(#("", indent_parser))
    _ -> Ok(#("", indent_parser))
  }
}

fn parse_explicit_key_after_indent(
  parser: Parser,
  n: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  let after_indent = advance(parser)
  case current(after_indent) {
    Some(lexer.Dash) -> {
      let backtracked = helpers.backtrack(after_indent)
      parse_sequence_as_explicit_key(backtracked, n, parse_value_fn)
    }
    Some(lexer.Colon) -> Ok(#("", parser))
    _ -> Ok(#("", parser))
  }
}

fn parse_sequence_as_explicit_key(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  use #(seq_val, parser) <- result.try(parse_block_sequence(
    parser,
    min_indent,
    parse_value_fn,
  ))
  Ok(#(value_to_key_string(seq_val), parser))
}
