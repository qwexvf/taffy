//// Explicit mapping parsing (using ? for keys).

import gleam/dict
import gleam/option.{Some}
import gleam/result
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
  parse_explicit_mapping_items(parser, min_indent, [], parse_value_fn)
}

fn parse_explicit_mapping_items(
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)

  case current(parser) {
    Some(lexer.Question) -> {
      let parser = advance(parser) |> skip_spaces
      use #(key, parser) <- result.try(parse_explicit_key_value(
        parser,
        min_indent,
        parse_value_fn,
      ))
      parse_after_explicit_key(key, parser, min_indent, acc, parse_value_fn)
    }
    Some(lexer.Indent(n)) if n >= min_indent ->
      parse_explicit_mapping_items_at_indent(
        parser,
        min_indent,
        n,
        acc,
        parse_value_fn,
      )
    // Handle implicit keys at indent 0 (when min_indent == 0)
    Some(lexer.Plain(s)) if min_indent == 0 ->
      try_implicit_key(
        s,
        advance(parser) |> skip_spaces,
        min_indent,
        1,
        acc,
        parse_value_fn,
      )
    Some(lexer.SingleQuoted(s)) if min_indent == 0 ->
      try_implicit_key(
        s,
        advance(parser) |> skip_spaces,
        min_indent,
        1,
        acc,
        parse_value_fn,
      )
    Some(lexer.DoubleQuoted(s)) if min_indent == 0 ->
      try_implicit_key(
        s,
        advance(parser) |> skip_spaces,
        min_indent,
        1,
        acc,
        parse_value_fn,
      )
    // Empty key entry (: value) at indent 0
    Some(lexer.Colon) if min_indent == 0 ->
      parse_key_value_pair(
        "",
        advance(parser) |> skip_spaces,
        min_indent,
        1,
        acc,
        parse_value_fn,
      )
    _ -> Ok(#(value.Mapping(acc), parser))
  }
}

/// After parsing an explicit key, look for the colon separator and value.
fn parse_after_explicit_key(
  key: String,
  parser: Parser,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)
  case current(parser) {
    Some(lexer.Colon) -> {
      let parser = advance(parser) |> skip_spaces
      use #(val, parser) <- result.try(parse_explicit_value(
        parser,
        min_indent,
        parse_value_fn,
      ))
      let acc = value.ordered_insert(acc, key, val)
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
    Some(lexer.Indent(n)) ->
      parse_after_explicit_key_indent(
        key,
        advance(parser),
        n,
        min_indent,
        acc,
        parse_value_fn,
      )
    // No colon, key maps to null — check for continuation
    Some(lexer.Question) -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
    _ -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
  }
}

/// Handle the Indent branch after an explicit key (looking for colon or next key).
fn parse_after_explicit_key_indent(
  key: String,
  parser: Parser,
  n: Int,
  min_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) -> {
      let parser = advance(parser) |> skip_spaces
      use #(val, parser) <- result.try(parse_value_fn(parser, n + 1))
      let acc = value.ordered_insert(acc, key, val)
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
    Some(lexer.Question) -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_explicit_mapping_items_at_indent(
        parser,
        min_indent,
        n,
        acc,
        parse_value_fn,
      )
    }
    Some(lexer.Plain(s)) -> {
      let acc = value.ordered_insert(acc, key, value.Null)
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
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
  }
}

/// Try to parse an implicit key (scalar followed by colon).
/// If no colon follows, the mapping ends.
fn try_implicit_key(
  key: String,
  parser: Parser,
  min_indent: Int,
  value_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) ->
      parse_key_value_pair(
        key,
        advance(parser) |> skip_spaces,
        min_indent,
        value_indent,
        acc,
        parse_value_fn,
      )
    _ -> Ok(#(value.Mapping(acc), parser))
  }
}

/// Parse a key-value pair where the key is known and colon has been consumed.
fn parse_key_value_pair(
  key: String,
  parser: Parser,
  min_indent: Int,
  value_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_value_fn(parser, value_indent))
  let acc = value.ordered_insert(acc, key, val)
  parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
}

fn parse_explicit_mapping_items_at_indent(
  parser: Parser,
  min_indent: Int,
  n: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = advance(parser)
  case current(parser) {
    Some(lexer.Question) -> {
      let parser = advance(parser) |> skip_spaces
      use #(key, parser) <- result.try(parse_explicit_key_value(
        parser,
        n,
        parse_value_fn,
      ))
      parse_after_explicit_key_at_indent(
        key,
        parser,
        min_indent,
        n,
        acc,
        parse_value_fn,
      )
    }
    Some(lexer.Plain(s)) ->
      parse_mixed_mapping_from_plain(
        parser,
        s,
        min_indent,
        n,
        acc,
        parse_value_fn,
      )
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      try_implicit_key(
        s,
        advance(parser) |> skip_spaces,
        min_indent,
        n + 1,
        acc,
        parse_value_fn,
      )
    _ -> Ok(#(value.Mapping(acc), parser))
  }
}

/// After parsing an explicit key at a specific indent, find the colon and value.
fn parse_after_explicit_key_at_indent(
  key: String,
  parser: Parser,
  min_indent: Int,
  n: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = skip_newlines_and_comments(parser)
  case current(parser) {
    Some(lexer.Colon) -> {
      let parser = advance(parser) |> skip_spaces
      use #(val, parser) <- result.try(parse_value_fn(parser, n + 1))
      let acc = value.ordered_insert(acc, key, val)
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
    Some(lexer.Indent(i)) -> {
      let parser = advance(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = advance(parser) |> skip_spaces
          use #(val, parser) <- result.try(parse_value_fn(parser, i + 1))
          let acc = value.ordered_insert(acc, key, val)
          parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
        }
        _ -> {
          let acc = value.ordered_insert(acc, key, value.Null)
          parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
        }
      }
    }
    _ -> {
      let acc = value.ordered_insert(acc, key, value.Null)
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
  }
}

fn parse_mixed_mapping_from_plain(
  parser: Parser,
  s: String,
  min_indent: Int,
  n: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  try_implicit_key(
    s,
    advance(parser) |> skip_spaces,
    min_indent,
    n + 1,
    acc,
    parse_value_fn,
  )
}

/// Collect multiline text for an explicit key.
/// Stops at `:` (value separator), `?` (another key), or when indent decreases.
fn collect_explicit_key_multiline(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  case current(parser) {
    Some(lexer.Plain(s)) ->
      collect_explicit_key_multiline(
        advance(parser),
        acc <> " " <> s,
        min_indent,
      )
    Some(lexer.Newline) ->
      collect_key_after_newline(advance(parser), acc, min_indent)
    Some(lexer.Indent(n)) if n > min_indent ->
      collect_key_after_indent(advance(parser), acc, min_indent)
    Some(lexer.Colon) | Some(lexer.Question) | Some(lexer.Comment(_)) -> #(
      acc,
      parser,
    )
    _ -> #(acc, parser)
  }
}

/// Continue collecting key text after a newline.
fn collect_key_after_newline(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  case current(parser) {
    Some(lexer.Indent(n)) if n > min_indent ->
      collect_key_after_indent(advance(parser), acc, min_indent)
    _ -> #(acc, parser)
  }
}

/// Continue collecting key text after an indent token.
fn collect_key_after_indent(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  case current(parser) {
    Some(lexer.Colon) | Some(lexer.Question) -> #(acc, parser)
    Some(lexer.Plain(s)) ->
      collect_explicit_key_multiline(
        advance(parser),
        acc <> " " <> s,
        min_indent,
      )
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
    Some(lexer.Plain(s)) ->
      parse_explicit_plain_key(advance(parser), s, min_indent, parse_value_fn)
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      Ok(#(s, advance(parser)))
    Some(lexer.Literal(s)) | Some(lexer.Folded(s)) -> Ok(#(s, advance(parser)))
    Some(lexer.Dash) ->
      parse_sequence_as_key(parser, min_indent, parse_value_fn)
    Some(lexer.BracketOpen) ->
      parse_flow_collection_as_key(parser, flow.parse_flow_sequence)
    Some(lexer.BraceOpen) ->
      parse_flow_collection_as_key(parser, flow.parse_flow_mapping)
    Some(lexer.Anchor(name)) ->
      parse_anchored_key(parser, name, min_indent, parse_value_fn)
    Some(lexer.Alias(name)) -> parse_alias_as_key(parser, name)
    Some(lexer.Tag(_)) ->
      parse_explicit_key_value(
        advance(parser) |> skip_spaces,
        min_indent,
        parse_value_fn,
      )
    Some(lexer.Newline) ->
      parse_key_after_newline(advance(parser), min_indent, parse_value_fn)
    Some(lexer.Indent(n)) ->
      parse_key_after_indent_token(parser, n, min_indent, parse_value_fn)
    Some(lexer.Colon) | option.None | Some(lexer.Eof) -> Ok(#("", parser))
    Some(lexer.Comment(_)) ->
      parse_explicit_key_value(advance(parser), min_indent, parse_value_fn)
    Some(token) ->
      Error(ParseError(
        "Expected explicit key value, got " <> token_to_string(token),
        parser.pos,
      ))
  }
}

/// Parse a plain scalar that might be an inline mapping key.
fn parse_explicit_plain_key(
  parser: Parser,
  s: String,
  min_indent: Int,
  _parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) ->
      parse_possible_inline_mapping(
        advance(parser) |> skip_spaces,
        s,
        min_indent,
      )
    _ -> {
      let #(full_key, parser) =
        collect_explicit_key_multiline(parser, s, min_indent)
      Ok(#(full_key, parser))
    }
  }
}

/// After seeing "key:" inside an explicit key, check if it's an inline mapping.
fn parse_possible_inline_mapping(
  parser: Parser,
  key: String,
  min_indent: Int,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(v)) -> {
      let parser = skip_newlines_and_comments(advance(parser))
      let key_str = "{" <> key <> ": " <> v <> "}"
      Ok(#(key_str, parser))
    }
    Some(lexer.SingleQuoted(v)) | Some(lexer.DoubleQuoted(v)) -> {
      let key_str = "{" <> key <> ": " <> v <> "}"
      Ok(#(key_str, advance(parser)))
    }
    _ -> {
      let #(full_key, parser) =
        collect_explicit_key_multiline(parser, key, min_indent)
      Ok(#(full_key, parser))
    }
  }
}

/// Parse a block sequence and convert to a key string.
fn parse_sequence_as_key(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  use #(seq_val, parser) <- result.try(block.parse_block_sequence(
    parser,
    min_indent,
    parse_value_fn,
  ))
  Ok(#(value_to_key_string(seq_val), parser))
}

/// Parse a flow collection (sequence or mapping) and convert to a key string.
fn parse_flow_collection_as_key(
  parser: Parser,
  parse_fn: fn(Parser) -> Result(#(YamlValue, Parser), ParseError),
) -> Result(#(String, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_fn(advance(parser)))
  Ok(#(value_to_key_string(val), parser))
}

/// Parse an anchor followed by a key value.
fn parse_anchored_key(
  parser: Parser,
  name: String,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  let parser = advance(parser) |> skip_spaces
  use #(key, parser) <- result.try(parse_explicit_key_value(
    parser,
    min_indent,
    parse_value_fn,
  ))
  let parser =
    Parser(
      ..parser,
      anchors: dict.insert(parser.anchors, name, value.String(key)),
    )
  Ok(#(key, parser))
}

/// Parse an alias reference as a key.
fn parse_alias_as_key(
  parser: Parser,
  name: String,
) -> Result(#(String, Parser), ParseError) {
  let parser = advance(parser)
  case dict.get(parser.anchors, name) {
    Ok(val) -> Ok(#(value_to_string(val), parser))
    Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
  }
}

/// After a newline in an explicit key, look for content on the next line.
fn parse_key_after_newline(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Dash) -> parse_sequence_as_key(parser, 0, parse_value_fn)
    Some(lexer.Indent(n)) ->
      parse_key_after_indent_token(parser, n, min_indent, parse_value_fn)
    Some(lexer.Colon) -> Ok(#("", parser))
    _ -> Ok(#("", parser))
  }
}

/// Parse key content after an Indent token.
fn parse_key_after_indent_token(
  parser: Parser,
  n: Int,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  let after_indent = advance(parser)
  case current(after_indent) {
    Some(lexer.Dash) -> {
      let parser = Parser(..after_indent, pos: after_indent.pos - 1)
      parse_sequence_as_key(parser, n, parse_value_fn)
    }
    Some(lexer.Colon) -> Ok(#("", parser))
    _ -> parse_explicit_key_value(after_indent, min_indent, parse_value_fn)
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
    Some(lexer.Newline) ->
      parse_explicit_value_after_newline(advance(parser), parse_value_fn)
    _ -> parse_value_fn(parser, min_indent + 1)
  }
}

/// Parse explicit value content after a newline.
fn parse_explicit_value_after_newline(
  parser: Parser,
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Dash) -> block.parse_block_sequence(parser, 0, parse_value_fn)
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
    _ -> Ok(#(value.Null, parser))
  }
}
