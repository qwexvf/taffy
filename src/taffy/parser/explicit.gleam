//// Explicit mapping parsing (using ? for keys).

import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/result
import taffy/lexer
import taffy/parser/block
import taffy/parser/flow
import taffy/parser/helpers.{
  advance, current, skip_newlines_and_comments, token_for_error,
}
import taffy/parser/scalar.{value_to_key_string}
import taffy/parser/types.{type ParseError, type Parser, ParseError}
import taffy/value.{type YamlValue}

pub type ParseValueFn =
  fn(Parser, Int) -> Result(#(YamlValue, Parser), ParseError)

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
      let parser = advance(parser)
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
    Some(lexer.Plain(s))
      | Some(lexer.SingleQuoted(s))
      | Some(lexer.DoubleQuoted(s))
      if min_indent == 0
    -> try_implicit_key(s, advance(parser), min_indent, 1, acc, parse_value_fn)
    Some(lexer.Colon) if min_indent == 0 ->
      parse_key_value_pair(
        "",
        advance(parser),
        min_indent,
        1,
        acc,
        parse_value_fn,
      )
    _ -> Ok(#(value.Mapping(list.reverse(acc)), parser))
  }
}

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
      let parser = advance(parser)
      use #(val, parser) <- result.try(parse_explicit_value(
        parser,
        min_indent,
        parse_value_fn,
      ))
      let acc = [#(key, val), ..acc]
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
    Some(lexer.Question) -> {
      let acc = [#(key, value.Null), ..acc]
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
    _ -> {
      let acc = [#(key, value.Null), ..acc]
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
  }
}

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
      let parser = advance(parser)
      use #(val, parser) <- result.try(parse_value_fn(parser, n + 1))
      let acc = [#(key, val), ..acc]
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
    Some(lexer.Question) -> {
      let acc = [#(key, value.Null), ..acc]
      parse_explicit_mapping_items_at_indent(
        parser,
        min_indent,
        n,
        acc,
        parse_value_fn,
      )
    }
    Some(lexer.Plain(s)) -> {
      let acc = [#(key, value.Null), ..acc]
      try_implicit_key(
        s,
        advance(parser),
        min_indent,
        n + 1,
        acc,
        parse_value_fn,
      )
    }
    _ -> {
      let acc = [#(key, value.Null), ..acc]
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
  }
}

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
        advance(parser),
        min_indent,
        value_indent,
        acc,
        parse_value_fn,
      )
    _ -> Ok(#(value.Mapping(list.reverse(acc)), parser))
  }
}

fn parse_key_value_pair(
  key: String,
  parser: Parser,
  min_indent: Int,
  value_indent: Int,
  acc: List(#(String, YamlValue)),
  parse_value_fn: ParseValueFn,
) -> Result(#(YamlValue, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_value_fn(parser, value_indent))
  let acc = [#(key, val), ..acc]
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
      let parser = advance(parser)
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
      try_implicit_key(
        s,
        advance(parser),
        min_indent,
        n + 1,
        acc,
        parse_value_fn,
      )
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      try_implicit_key(
        s,
        advance(parser),
        min_indent,
        n + 1,
        acc,
        parse_value_fn,
      )
    _ -> Ok(#(value.Mapping(list.reverse(acc)), parser))
  }
}

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
      let parser = advance(parser)
      use #(val, parser) <- result.try(parse_value_fn(parser, n + 1))
      let acc = [#(key, val), ..acc]
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
    Some(lexer.Indent(i)) -> {
      let parser = advance(parser)
      case current(parser) {
        Some(lexer.Colon) -> {
          let parser = advance(parser)
          use #(val, parser) <- result.try(parse_value_fn(parser, i + 1))
          let acc = [#(key, val), ..acc]
          parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
        }
        _ -> {
          let acc = [#(key, value.Null), ..acc]
          parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
        }
      }
    }
    _ -> {
      let acc = [#(key, value.Null), ..acc]
      parse_explicit_mapping_items(parser, min_indent, acc, parse_value_fn)
    }
  }
}

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
    Some(lexer.Newline) -> {
      let after_newline = advance(parser)
      case current(after_newline) {
        Some(lexer.Indent(n)) if n > min_indent -> {
          let after_indent = advance(after_newline)
          case current(after_indent) {
            Some(lexer.Colon) | Some(lexer.Question) -> #(acc, after_newline)
            Some(lexer.Plain(s)) ->
              collect_explicit_key_multiline(
                advance(after_indent),
                acc <> " " <> s,
                min_indent,
              )
            _ -> #(acc, after_newline)
          }
        }
        _ -> #(acc, parser)
      }
    }
    Some(lexer.Indent(n)) if n > min_indent -> {
      let after_indent = advance(parser)
      case current(after_indent) {
        Some(lexer.Colon) | Some(lexer.Question) -> #(acc, parser)
        Some(lexer.Plain(s)) ->
          collect_explicit_key_multiline(
            advance(after_indent),
            acc <> " " <> s,
            min_indent,
          )
        _ -> #(acc, parser)
      }
    }
    Some(lexer.Colon) | Some(lexer.Question) | Some(lexer.Comment(_)) -> #(
      acc,
      parser,
    )
    _ -> #(acc, parser)
  }
}

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
    Some(lexer.Dash) -> {
      use #(seq_val, parser) <- result.try(block.parse_block_sequence(
        parser,
        min_indent,
        parse_value_fn,
      ))
      Ok(#(value_to_key_string(seq_val), parser))
    }
    Some(lexer.BracketOpen) -> {
      use #(val, parser) <- result.try(
        flow.parse_flow_sequence(advance(parser)),
      )
      Ok(#(value_to_key_string(val), parser))
    }
    Some(lexer.BraceOpen) -> {
      use #(val, parser) <- result.try(flow.parse_flow_mapping(advance(parser)))
      Ok(#(value_to_key_string(val), parser))
    }
    Some(lexer.Anchor(name)) ->
      parse_anchored_key(parser, name, min_indent, parse_value_fn)
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case dict.get(parser.anchors, name) {
        Ok(val) -> Ok(#(value_to_key_string(val), parser))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    Some(lexer.Tag(_)) ->
      parse_explicit_key_value(advance(parser), min_indent, parse_value_fn)
    Some(lexer.Newline) ->
      parse_key_after_newline(advance(parser), min_indent, parse_value_fn)
    Some(lexer.Indent(n)) ->
      parse_key_after_indent_token(parser, n, min_indent, parse_value_fn)
    Some(lexer.Colon) | option.None | Some(lexer.Eof) -> Ok(#("", parser))
    Some(lexer.Comment(_)) ->
      parse_explicit_key_value(advance(parser), min_indent, parse_value_fn)
    Some(token) ->
      Error(ParseError(
        "Expected explicit key value, got " <> token_for_error(token),
        parser.pos,
      ))
  }
}

fn parse_explicit_plain_key(
  parser: Parser,
  s: String,
  min_indent: Int,
  _parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) ->
      parse_possible_inline_mapping(advance(parser), s, min_indent)
    _ -> {
      let #(full_key, parser) =
        collect_explicit_key_multiline(parser, s, min_indent)
      Ok(#(full_key, parser))
    }
  }
}

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

fn parse_anchored_key(
  parser: Parser,
  name: String,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  let parser = advance(parser)
  use #(key, parser) <- result.try(parse_explicit_key_value(
    parser,
    min_indent,
    parse_value_fn,
  ))
  let parser = types.register_anchor(parser, name, value.String(key))
  Ok(#(key, parser))
}

fn parse_key_after_newline(
  parser: Parser,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Dash) -> {
      use #(seq_val, parser) <- result.try(block.parse_block_sequence(
        parser,
        0,
        parse_value_fn,
      ))
      Ok(#(value_to_key_string(seq_val), parser))
    }
    Some(lexer.Indent(n)) ->
      parse_key_after_indent_token(parser, n, min_indent, parse_value_fn)
    Some(lexer.Colon) -> Ok(#("", parser))
    _ -> Ok(#("", parser))
  }
}

fn parse_key_after_indent_token(
  parser: Parser,
  n: Int,
  min_indent: Int,
  parse_value_fn: ParseValueFn,
) -> Result(#(String, Parser), ParseError) {
  let after_indent = advance(parser)
  case current(after_indent) {
    Some(lexer.Dash) -> {
      let parser = helpers.backtrack(after_indent)
      use #(seq_val, parser) <- result.try(block.parse_block_sequence(
        parser,
        n,
        parse_value_fn,
      ))
      Ok(#(value_to_key_string(seq_val), parser))
    }
    Some(lexer.Colon) -> Ok(#("", parser))
    _ -> parse_explicit_key_value(after_indent, min_indent, parse_value_fn)
  }
}

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
          let parser = helpers.backtrack(after_indent)
          block.parse_block_sequence(parser, n, parse_value_fn)
        }
        _ -> {
          let parser = helpers.backtrack(after_indent)
          parse_value_fn(parser, n)
        }
      }
    }
    _ -> Ok(#(value.Null, parser))
  }
}
