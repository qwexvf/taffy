//// YAML parser - parses tokens into YAML values.

import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import taffy/lexer.{type Token}
import taffy/parser/block
import taffy/parser/explicit
import taffy/parser/flow
import taffy/parser/helpers.{
  advance, current, is_terminator, skip_newlines_and_comments, token_for_error,
}
import taffy/parser/scalar
import taffy/parser/types.{type ParseError, type Parser, ParseError, Parser}
import taffy/value.{type YamlValue}

fn new(tokens: List(#(Token, Int))) -> Parser {
  types.new(tokens)
}

fn with_limits(parser: Parser, alias_budget: Int, max_depth: Int) -> Parser {
  Parser(..parser, alias_budget: alias_budget, max_depth: max_depth)
}

pub fn parse(tokens: List(#(Token, Int))) -> Result(YamlValue, ParseError) {
  parse_from(new(tokens))
}

pub fn parse_with(
  tokens: List(#(Token, Int)),
  alias_budget: Int,
  max_depth: Int,
) -> Result(YamlValue, ParseError) {
  parse_from(with_limits(new(tokens), alias_budget, max_depth))
}

fn parse_from(parser: Parser) -> Result(YamlValue, ParseError) {
  let parser = skip_newlines_and_comments(parser)

  let parser = case current(parser) {
    Some(lexer.DocStart) -> advance(parser) |> skip_newlines_and_comments
    _ -> parser
  }

  use #(val, parser) <- result.try(parse_value(parser, 0))
  let parser = skip_newlines_and_comments(parser)
  case current(parser) {
    None | Some(lexer.Eof) -> Ok(val)
    Some(lexer.DocStart) -> Ok(val)
    Some(lexer.DocEnd) -> Ok(val)
    Some(token) ->
      Error(ParseError(
        "Unexpected trailing content: " <> token_for_error(token),
        parser.pos,
      ))
  }
}

pub fn parse_all(
  tokens: List(#(Token, Int)),
) -> Result(List(YamlValue), ParseError) {
  parse_all_from(new(tokens))
}

pub fn parse_all_with(
  tokens: List(#(Token, Int)),
  alias_budget: Int,
  max_depth: Int,
) -> Result(List(YamlValue), ParseError) {
  parse_all_from(with_limits(new(tokens), alias_budget, max_depth))
}

fn parse_all_from(parser: Parser) -> Result(List(YamlValue), ParseError) {
  use parser <- result.try(consume_directives(parser))
  parse_documents(parser, [])
}

fn validate_directive(
  content: String,
  is_yaml: Bool,
  pos: Int,
) -> Result(Nil, ParseError) {
  case is_yaml {
    True -> {
      let before_comment = case string.split_once(content, " #") {
        Ok(#(before, _)) -> string.trim(before)
        Error(_) -> string.trim(content)
      }
      let has_invalid_hash = string.contains(before_comment, "#")
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

fn consume_directives(parser: Parser) -> Result(Parser, ParseError) {
  let parser = Parser(..parser, tag_handles: [])
  consume_directives_loop(parser)
}

fn consume_directives_loop(parser: Parser) -> Result(Parser, ParseError) {
  let parser = skip_newlines_and_comments(parser)
  case current(parser) {
    Some(lexer.Directive(d)) -> {
      let is_yaml_dir = string.starts_with(d, "YAML")
      let is_tag_dir = string.starts_with(d, "TAG")
      use _ <- result.try(validate_directive(d, is_yaml_dir, parser.pos))
      let parser = case is_tag_dir {
        True -> {
          let handle = extract_tag_handle(d)
          Parser(..parser, tag_handles: [handle, ..parser.tag_handles])
        }
        False -> parser
      }
      let parser = advance(parser)
      let parser = skip_newlines_and_comments(parser)
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

fn extract_tag_handle(directive_content: String) -> String {
  let after_tag = string.drop_start(directive_content, 4)
  let trimmed = string.trim_start(after_tag)
  case string.split_once(trimmed, " ") {
    Ok(#(handle, _)) -> handle
    Error(_) -> trimmed
  }
}

fn validate_tag_handle(tag: String, parser: Parser) -> Result(Nil, ParseError) {
  case
    string.starts_with(tag, "!<") || tag == "!" || string.starts_with(tag, "!!")
  {
    True -> Ok(Nil)
    False -> {
      case string.starts_with(tag, "!") {
        False -> Ok(Nil)
        True -> {
          let rest = string.drop_start(tag, 1)
          case string.split_once(rest, "!") {
            Ok(#(handle_body, _suffix)) -> {
              let handle = "!" <> handle_body <> "!"
              case list.contains(parser.tag_handles, handle) {
                True -> Ok(Nil)
                False ->
                  Error(ParseError(
                    "Undefined tag handle: " <> handle,
                    parser.pos,
                  ))
              }
            }
            Error(_) -> Ok(Nil)
          }
        }
      }
    }
  }
}

fn check_doc_start_anchor(parser: Parser) -> Result(Nil, ParseError) {
  case current(parser) {
    Some(lexer.Anchor(_)) -> {
      let after_anchor = advance(parser)
      case current(after_anchor) {
        Some(lexer.Plain(_)) -> {
          let after_plain = advance(after_anchor)
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

fn check_after_doc_end(parser: Parser) -> Result(Parser, ParseError) {
  case is_terminator(current(parser)) {
    True -> Ok(parser)
    False ->
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
      use _ <- result.try(check_doc_start_anchor(after_doc_start))
      let parser = skip_newlines_and_comments(after_doc_start)
      case current(parser) {
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
                "Unexpected trailing content: " <> token_for_error(token),
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
    Some(lexer.Directive(_)) -> {
      use parser <- result.try(consume_directives(parser))
      parse_documents(parser, acc)
    }
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
        None | Some(lexer.Eof) -> Ok(list.reverse([val, ..acc]))
        Some(token) ->
          Error(ParseError(
            "Unexpected trailing content: " <> token_for_error(token),
            parser.pos,
          ))
      }
    }
  }
}

fn parse_value(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  case types.enter_depth(parser) {
    Error(_) ->
      Error(ParseError(
        "Maximum recursion depth exceeded (possible nesting bomb)",
        parser.pos,
      ))
    Ok(parser) -> {
      use #(val, parser) <- result.try(parse_value_dispatch(parser, min_indent))
      Ok(#(val, types.exit_depth(parser)))
    }
  }
}

fn parse_value_dispatch(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Comment(_)) -> parse_value(advance(parser), min_indent)
    Some(lexer.Newline) -> parse_value_after_newline(parser, min_indent)
    None
    | Some(lexer.Eof)
    | Some(lexer.DocEnd)
    | Some(lexer.DocStart)
    | Some(lexer.Directive(_)) -> Ok(#(value.Null, parser))
    Some(lexer.Anchor(name)) ->
      parse_anchored_value(advance(parser), name, min_indent)
    Some(lexer.Alias(name)) ->
      parse_alias_value(advance(parser), name, min_indent)
    Some(lexer.Tag(tag)) -> parse_tagged_value(parser, tag, min_indent)
    Some(lexer.BracketOpen) -> parse_flow_sequence_value(parser, min_indent)
    Some(lexer.BraceOpen) -> parse_flow_mapping_value(parser, min_indent)
    Some(lexer.Dash) -> {
      // INVARIANT: every caller of `parse_value(parser, 0)` is positioned at
      // column 0 (the entry points in `parse` and `parse_documents` start at
      // doc-start, and `parse_indented_value` only reaches min_indent==0 via
      // an outer Indent(0) which is itself column 0). New callers must
      // preserve this — otherwise misindented top-level dashes would slip
      // past `parse_block_sequence_at`'s column lock again. For min_indent>0
      // the dash shares a line with its parent (e.g. `- - foo`) and the
      // actual column is unknown, so we leave the lock unset.
      let dash_col = case min_indent {
        0 -> Some(0)
        _ -> None
      }
      block.parse_block_sequence_at(parser, min_indent, dash_col, parse_value)
    }
    Some(lexer.Question) ->
      explicit.parse_explicit_mapping(parser, min_indent, parse_value)
    Some(lexer.Colon) -> parse_colon_value(parser, min_indent)
    Some(lexer.Literal(content)) ->
      Ok(#(value.String(content), advance(parser)))
    Some(lexer.Folded(content)) -> Ok(#(value.String(content), advance(parser)))
    Some(lexer.Plain(s)) -> parse_plain_value(parser, s, min_indent)
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      parse_quoted_value(parser, s, min_indent)
    Some(lexer.Indent(n)) if n >= min_indent ->
      parse_indented_value(parser, n, min_indent)
    Some(lexer.Indent(_)) -> Ok(#(value.Null, parser))
    Some(token) ->
      Error(ParseError(
        "Unexpected token: " <> token_for_error(token),
        parser.pos,
      ))
  }
}

fn parse_value_after_newline(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = Parser(..advance(parser), in_inline_value: False)
  case min_indent > 0 {
    True -> Ok(#(value.Null, parser))
    False -> parse_value(parser, min_indent)
  }
}

fn parse_alias_value(
  parser: Parser,
  name: String,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  case types.resolve_alias(parser, name) {
    Ok(#(alias_val, parser)) ->
      case current(parser) {
        Some(lexer.Colon) -> {
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
    Error("budget exceeded") ->
      Error(ParseError(
        "Alias expansion budget exceeded (possible alias-bomb)",
        parser.pos,
      ))
    Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
  }
}

fn parse_tagged_value(
  parser: Parser,
  tag: String,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  use _ <- result.try(validate_tag_handle(tag, parser))
  let parser = advance(parser)
  let is_str_tag =
    tag == "!!str" || string.contains(tag, "tag:yaml.org,2002:str")
  let is_non_specific = tag == "!"
  case current(parser), is_str_tag, is_non_specific {
    Some(lexer.Eof), True, _ | None, True, _ -> Ok(#(value.String(""), parser))
    Some(lexer.Newline), True, _ -> {
      let after_newline = advance(parser)
      case current(after_newline) {
        Some(lexer.Eof) | None | Some(lexer.DocStart) | Some(lexer.DocEnd) ->
          Ok(#(value.String(""), parser))
        Some(lexer.Dash) -> Ok(#(value.String(""), parser))
        _ -> parse_value(parser, min_indent)
      }
    }
    Some(lexer.Plain(s)), _, True ->
      Ok(#(value.String(string.trim(s)), advance(parser)))
    _, _, _ -> parse_value(parser, min_indent)
  }
}

fn parse_flow_sequence_value(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser =
    Parser(
      ..advance(parser),
      flow_min_indent: min_indent,
      flow_multiline: False,
    )
  use #(seq_val, parser) <- result.try(flow.parse_flow_sequence(parser))
  let was_multiline = parser.flow_multiline
  case current(parser) {
    Some(lexer.Colon) ->
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
    Some(lexer.Plain(_))
    | Some(lexer.SingleQuoted(_))
    | Some(lexer.DoubleQuoted(_))
    | Some(lexer.Dash) ->
      Error(ParseError("Invalid content after flow sequence", parser.pos))
    _ -> Ok(#(seq_val, parser))
  }
}

fn parse_flow_mapping_value(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = Parser(..advance(parser), flow_min_indent: min_indent)
  use #(map_val, parser) <- result.try(flow.parse_flow_mapping(parser))
  case current(parser) {
    Some(lexer.Colon) -> {
      let key = scalar.value_to_key_string(map_val)
      block.parse_block_mapping_from_key(
        key,
        advance(parser),
        min_indent,
        parse_value,
      )
    }
    Some(lexer.Plain(_))
    | Some(lexer.SingleQuoted(_))
    | Some(lexer.DoubleQuoted(_))
    | Some(lexer.Dash) ->
      Error(ParseError("Invalid content after flow mapping", parser.pos))
    _ -> Ok(#(map_val, parser))
  }
}

fn parse_colon_value(
  parser: Parser,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let after_colon = advance(parser)
  case current(after_colon) {
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
    Some(lexer.Colon)
    | Some(lexer.Comma)
    | Some(lexer.BracketClose)
    | Some(lexer.BraceClose) -> {
      let #(rest, parser) = collect_plain_scalar_from_colon(after_colon)
      Ok(#(scalar.parse_scalar(":" <> rest), parser))
    }
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

fn parse_plain_value(
  parser: Parser,
  s: String,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let #(full_key, parser) = collect_block_plain_key(advance(parser), s)
  case current(parser) {
    Some(lexer.Colon) -> parse_plain_after_colon(parser, full_key, min_indent)
    Some(lexer.Newline) -> {
      let after_newline = advance(parser)
      let acc = case current(after_newline) {
        Some(lexer.Indent(_)) -> full_key <> "\n"
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

fn parse_plain_after_colon(
  parser: Parser,
  full_key: String,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let after_colon = advance(parser)
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
      case
        parser.in_inline_value && is_inline_value_token(current(after_colon))
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
    _ -> {
      let #(rest, parser) =
        collect_block_plain_value(after_colon, full_key <> ":", min_indent)
      Ok(#(scalar.parse_scalar(rest), parser))
    }
  }
}

fn parse_quoted_value(
  parser: Parser,
  s: String,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = advance(parser)
  case current(parser) {
    Some(lexer.Colon) -> {
      let after_colon = advance(parser)
      case
        parser.in_inline_value && is_inline_value_token(current(after_colon))
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

fn parse_indented_value(
  parser: Parser,
  n: Int,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  let parser = Parser(..advance(parser), in_inline_value: False)
  case current(parser) {
    Some(lexer.Dash) ->
      case is_sequence_dash(parser, n) {
        True -> block.parse_block_sequence_at(parser, n, Some(n), parse_value)
        False -> parse_value(parser, min_indent)
      }
    Some(lexer.Plain(_))
    | Some(lexer.SingleQuoted(_))
    | Some(lexer.DoubleQuoted(_)) ->
      parse_value_after_indent(parser, n, min_indent)
    _ -> parse_value(parser, min_indent)
  }
}

fn is_sequence_dash(parser: Parser, n: Int) -> Bool {
  case parser.seq_entry_indent {
    option.Some(entry_indent) -> n == entry_indent
    option.None -> True
  }
}

fn should_absorb_dash_in_scalar(
  seq_entry_indent: option.Option(Int),
  n: Int,
  min_indent: Int,
) -> Bool {
  // YAML 1.2 W4TN: a misindented dash on a continuation line of a still-
  // active plain scalar is folded into the scalar, not treated as a new
  // sequence item. We only get here while collecting a plain scalar, so
  // `n > entry_indent` is enough to confirm the dash isn't a sibling at
  // the parent sequence's column.
  case seq_entry_indent {
    option.Some(entry_indent) -> n > entry_indent && n == min_indent
    option.None -> False
  }
}

fn parse_value_after_indent(
  parser: Parser,
  n: Int,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      let #(full_key, after_key) = collect_block_plain_key(advance(parser), s)
      case current(after_key) {
        Some(lexer.Colon) -> {
          let after_colon = advance(after_key)
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
      let after_key = advance(parser)
      case current(after_key) {
        Some(lexer.Colon) ->
          block.parse_block_mapping_from_key_col(
            s,
            advance(after_key),
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
    _ -> #("", parser)
  }
}

fn collect_block_plain_key(parser: Parser, acc: String) -> #(String, Parser) {
  case current(parser) {
    Some(lexer.Comma) -> collect_block_plain_key(advance(parser), acc <> ",")
    Some(lexer.BracketOpen) ->
      collect_block_plain_key(advance(parser), acc <> "[")
    Some(lexer.BracketClose) ->
      collect_block_plain_key(advance(parser), acc <> "]")
    Some(lexer.BraceOpen) ->
      collect_block_plain_key(advance(parser), acc <> "{")
    Some(lexer.BraceClose) ->
      collect_block_plain_key(advance(parser), acc <> "}")
    Some(lexer.Colon) -> {
      let after_colon = advance(parser)
      case current(after_colon) {
        Some(lexer.BracketClose)
        | Some(lexer.BraceClose)
        | Some(lexer.Colon)
        | Some(lexer.Comma) -> collect_block_plain_key(after_colon, acc <> ":")
        _ -> #(acc, parser)
      }
    }
    Some(lexer.Plain(s)) -> collect_block_plain_key(advance(parser), acc <> s)
    _ -> #(acc, parser)
  }
}

/// Append `fragment` to `acc`, separating with a space unless `acc` already
/// ends with a newline (in which case the fragment opens a new line in a
/// multi-line plain scalar).
fn append_continuation(acc: String, fragment: String) -> String {
  case string.ends_with(acc, "\n") {
    True -> acc <> fragment
    False -> acc <> " " <> fragment
  }
}

/// View a token as a fragment that can extend a plain scalar across a line
/// break. `Plain(s)` and `Tag(s)` keep their text; `Anchor(s)` re-prepends
/// `&`. Anything else returns `None`, signalling the scalar has ended.
fn token_as_scalar_fragment(token: lexer.Token) -> option.Option(String) {
  case token {
    lexer.Plain(s) -> Some(s)
    lexer.Tag(s) -> Some(s)
    lexer.Anchor(s) -> Some("&" <> s)
    _ -> None
  }
}

fn collect_block_plain_value(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  case current(parser) {
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
    Some(lexer.Plain(s)) ->
      collect_block_plain_value(advance(parser), acc <> " " <> s, min_indent)
    Some(lexer.Tag(s)) ->
      collect_block_plain_value(advance(parser), acc <> " " <> s, min_indent)
    Some(lexer.Anchor(s)) ->
      collect_block_plain_value(advance(parser), acc <> " &" <> s, min_indent)
    Some(lexer.Newline) -> {
      let after_newline = advance(parser)
      case current(after_newline) {
        Some(lexer.Indent(_)) ->
          check_multiline_continuation(after_newline, acc <> "\n", min_indent)
        _ -> check_multiline_continuation(after_newline, acc, min_indent)
      }
    }
    Some(lexer.Indent(n)) if n >= min_indent ->
      collect_after_indent(parser, acc, min_indent, n)
    _ -> #(acc, parser)
  }
}

fn collect_after_indent(
  parser: Parser,
  acc: String,
  min_indent: Int,
  n: Int,
) -> #(String, Parser) {
  let after_indent = advance(parser)
  case current(after_indent) {
    Some(lexer.Dash) ->
      case
        should_absorb_dash_in_scalar(parser.seq_entry_indent, n, min_indent)
      {
        True ->
          collect_block_plain_value(
            advance(after_indent),
            append_continuation(acc, "-"),
            min_indent,
          )
        False -> #(acc, parser)
      }
    Some(lexer.Question) | Some(lexer.Colon) | Some(lexer.Comment(_)) -> #(
      acc,
      parser,
    )
    Some(lexer.Indent(m)) if m >= min_indent ->
      collect_block_plain_value(after_indent, acc <> "\n", min_indent)
    Some(lexer.Plain(s)) ->
      case is_mapping_key(after_indent) {
        True -> #(acc, parser)
        False ->
          collect_block_plain_value(
            advance(after_indent),
            append_continuation(acc, s),
            min_indent,
          )
      }
    Some(token) ->
      case token_as_scalar_fragment(token) {
        Some(fragment) ->
          collect_block_plain_value(
            advance(after_indent),
            append_continuation(acc, fragment),
            min_indent,
          )
        None -> #(acc, parser)
      }
    None -> #(acc, parser)
  }
}

fn check_multiline_continuation(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  case current(parser) {
    Some(lexer.Newline) ->
      check_multiline_continuation(advance(parser), acc <> "\n", min_indent)
    Some(lexer.Indent(n)) if n >= min_indent ->
      continue_after_continuation_indent(parser, acc, min_indent)
    Some(lexer.Plain(s)) if min_indent == 0 ->
      case is_mapping_key(parser) {
        True -> #(acc, helpers.backtrack(parser))
        False ->
          collect_block_plain_value(
            advance(parser),
            append_continuation(acc, s),
            min_indent,
          )
      }
    _ -> #(acc, helpers.backtrack(parser))
  }
}

fn continue_after_continuation_indent(
  parser: Parser,
  acc: String,
  min_indent: Int,
) -> #(String, Parser) {
  let after_indent = advance(parser)
  case current(after_indent) {
    Some(lexer.Newline) ->
      check_multiline_continuation(
        advance(after_indent),
        acc <> "\n",
        min_indent,
      )
    Some(lexer.Plain(s)) ->
      case is_mapping_key(after_indent) {
        True -> #(acc, helpers.backtrack(parser))
        False ->
          collect_block_plain_value(
            advance(after_indent),
            append_continuation(acc, s),
            min_indent,
          )
      }
    Some(token) ->
      case token_as_scalar_fragment(token) {
        Some(fragment) ->
          collect_block_plain_value(
            advance(after_indent),
            append_continuation(acc, fragment),
            min_indent,
          )
        None -> #(acc, helpers.backtrack(parser))
      }
    None -> #(acc, helpers.backtrack(parser))
  }
}

fn parse_anchored_value(
  parser: Parser,
  name: String,
  min_indent: Int,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Anchor(_)) ->
      Error(ParseError("A node can only have one anchor", parser.pos))
    Some(lexer.Alias(_)) ->
      Error(ParseError("Cannot place an anchor on an alias", parser.pos))
    Some(lexer.Plain(s)) ->
      parse_anchored_scalar_or_key(
        parser,
        name,
        s,
        min_indent,
        scalar.parse_scalar,
      )
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      parse_anchored_scalar_or_key(parser, name, s, min_indent, fn(v) {
        value.String(v)
      })
    Some(lexer.Dash) ->
      Error(ParseError(
        "Block sequence not allowed on same line as anchor",
        parser.pos,
      ))
    _ -> {
      use #(val, parser) <- result.try(parse_value(parser, min_indent))
      let parser = types.register_anchor(parser, name, val)
      Ok(#(val, parser))
    }
  }
}

fn parse_anchored_scalar_or_key(
  parser: Parser,
  anchor_name: String,
  s: String,
  min_indent: Int,
  to_value: fn(String) -> YamlValue,
) -> Result(#(YamlValue, Parser), ParseError) {
  let after_scalar = advance(parser)
  case current(after_scalar) {
    Some(lexer.Colon) -> {
      let parser =
        types.register_anchor(after_scalar, anchor_name, value.String(s))
      block.parse_block_mapping_from_key(
        s,
        advance(parser),
        min_indent,
        parse_value,
      )
    }
    _ -> {
      let val = to_value(s)
      let parser = types.register_anchor(advance(parser), anchor_name, val)
      Ok(#(val, parser))
    }
  }
}

fn is_inline_value_token(token: option.Option(lexer.Token)) -> Bool {
  !is_terminator(token)
}

fn is_mapping_key(parser: Parser) -> Bool {
  case current(parser) {
    Some(lexer.Plain(_))
    | Some(lexer.SingleQuoted(_))
    | Some(lexer.DoubleQuoted(_)) -> {
      let after_scalar = advance(parser)
      case current(after_scalar) {
        Some(lexer.Colon) -> True
        _ -> False
      }
    }
    _ -> False
  }
}
