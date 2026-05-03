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
import taffy/parser/types.{type ParseError, type Parser, ParseError}
import taffy/value.{type YamlValue}

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
      let parser = types.register_anchor(parser, name, val)
      Ok(#(val, parser))
    }

    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case types.resolve_alias(parser, name) {
        Ok(#(val, parser)) -> Ok(#(val, parser))
        Error("budget exceeded") ->
          Error(ParseError(
            "Alias expansion budget exceeded (possible alias-bomb)",
            parser.pos,
          ))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }

    Some(lexer.Tag(tag)) -> {
      let parser = advance(parser) |> skip_whitespace
      let is_str_tag =
        tag == "!!str" || string.contains(tag, "tag:yaml.org,2002:str")
      case current(parser), is_str_tag {
        Some(lexer.Comma), True
        | Some(lexer.BracketClose), True
        | Some(lexer.BraceClose), True
        | Some(lexer.Eof), True
        | option.None, True
        -> Ok(#(value.String(""), parser))
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

fn check_adjacent_colon(
  key_val: YamlValue,
  plain: String,
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case string.starts_with(plain, ":") {
    True -> {
      let key = value_to_key_string(key_val)
      let rest = string.drop_start(plain, 1)
      let val = case rest {
        "" -> value.Null
        _ -> parse_scalar(rest)
      }
      Ok(#(value.Mapping([#(key, val)]), advance(parser)))
    }
    False -> Ok(#(key_val, parser))
  }
}

fn check_for_plain_mapping_key(
  full_scalar: String,
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) -> {
      use parser <- result.try(skip_flow_whitespace(advance(parser)))
      use #(val, parser) <- result.try(parse_flow_value(parser))
      Ok(#(value.Mapping([#(full_scalar, val)]), parser))
    }
    _ -> Ok(#(parse_scalar(full_scalar), parser))
  }
}

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
          let rest = string.drop_start(p, 1)
          let val = case rest {
            "" -> value.Null
            _ -> parse_scalar(rest)
          }
          Ok(#(value.Mapping([#(s, val)]), advance(parser)))
        }
        False -> Ok(#(value.String(s), parser))
      }
    }
    Some(lexer.Newline) | Some(lexer.Indent(_)) ->
      Ok(#(value.String(s), parser))
    _ -> Ok(#(value.String(s), parser))
  }
}

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
    _ -> {
      Ok(#(value.Mapping([#("", value.Null)]), parser))
    }
  }
}

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

fn parse_flow_mapping_value(
  key: String,
  parser: Parser,
  acc: List(#(String, YamlValue)),
) -> Result(#(YamlValue, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Plain(s)) -> {
      case string.starts_with(s, ":") {
        True -> {
          let rest = string.drop_start(s, 1)
          let val = case rest {
            "" -> value.Null
            _ -> parse_scalar(rest)
          }
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

fn parse_flow_collection_key(
  parser: Parser,
  parse_fn: fn(Parser) -> Result(#(YamlValue, Parser), ParseError),
) -> Result(#(String, Parser), ParseError) {
  use #(val, parser) <- result.try(parse_fn(advance(parser)))
  Ok(#(value_to_key_string(val), parser))
}

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
      let parser = types.register_anchor(parser, name, val)
      Ok(#(value_to_key_string(val), parser))
    }
    _ -> {
      use #(key, parser) <- result.try(parse_flow_key_with_colon(parser))
      let parser = types.register_anchor(parser, name, value.String(key))
      Ok(#(key, parser))
    }
  }
}

fn parse_alias_flow_key(
  parser: Parser,
  name: String,
) -> Result(#(String, Parser), ParseError) {
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

fn parse_tagged_flow_key(
  parser: Parser,
  tag: String,
) -> Result(#(String, Parser), ParseError) {
  let parser = advance(parser) |> skip_whitespace
  let is_str_tag =
    tag == "!!str" || string.contains(tag, "tag:yaml.org,2002:str")
  case current(parser), is_str_tag {
    Some(lexer.Colon), True -> Ok(#("", parser))
    _, _ -> parse_flow_key_with_colon(parser)
  }
}

fn collect_flow_key_parts(
  parser: Parser,
  acc: String,
) -> Result(#(String, Parser), ParseError) {
  case current(parser) {
    Some(lexer.Colon) -> {
      let after_colon = advance(parser)
      case current(after_colon) {
        Some(lexer.Indent(_)) -> Ok(#(string.trim_end(acc), parser))
        Some(lexer.Plain(_))
        | Some(lexer.SingleQuoted(_))
        | Some(lexer.DoubleQuoted(_))
        | Some(lexer.BracketOpen)
        | Some(lexer.BraceOpen) -> {
          Ok(#(string.trim_end(acc), parser))
        }
        _ -> Ok(#(string.trim_end(acc), parser))
      }
    }
    Some(lexer.Plain(s)) -> {
      let parser = advance(parser)
      collect_flow_key_parts(parser, acc <> s)
    }
    Some(lexer.Indent(_)) | Some(lexer.Newline) -> {
      // Indentation error inside a flow context just means the key has ended;
      // hand the accumulated text back and let the caller re-encounter it.
      case skip_flow_whitespace(parser) {
        Error(_) -> Ok(#(string.trim_end(acc), parser))
        Ok(parser_after_ws) ->
          case current(parser_after_ws) {
            Some(lexer.Plain(s)) -> {
              let parser = advance(parser_after_ws)
              collect_flow_key_parts(parser, acc <> " " <> s)
            }
            _ -> Ok(#(string.trim_end(acc), parser))
          }
      }
    }
    _ -> Ok(#(string.trim_end(acc), parser))
  }
}

fn collect_flow_plain_scalar_parts(
  parser: Parser,
  acc: String,
) -> #(String, Parser) {
  case current(parser) {
    Some(lexer.Comma)
    | Some(lexer.BracketClose)
    | Some(lexer.BraceClose)
    | Some(lexer.Colon) -> #(string.trim_end(acc), parser)
    Some(lexer.Plain(s)) -> {
      let parser = advance(parser)
      collect_flow_plain_scalar_parts(parser, acc <> s)
    }
    Some(lexer.Indent(_)) | Some(lexer.Newline) -> {
      case flow_whitespace_has_comment(parser) {
        True -> #(string.trim_end(acc), parser)
        False ->
          // See collect_flow_key_parts: indentation error => scalar ended.
          case skip_flow_whitespace(parser) {
            Error(_) -> #(string.trim_end(acc), parser)
            Ok(parser_after_ws) ->
              case current(parser_after_ws) {
                Some(lexer.Plain(s)) -> {
                  let parser = advance(parser_after_ws)
                  collect_flow_plain_scalar_parts(parser, acc <> " " <> s)
                }
                _ -> #(string.trim_end(acc), parser)
              }
          }
      }
    }
    _ -> #(string.trim_end(acc), parser)
  }
}

pub fn parse_flow_value(
  parser: Parser,
) -> Result(#(YamlValue, Parser), ParseError) {
  use parser <- result.try(skip_flow_whitespace(parser))

  case current(parser) {
    Some(lexer.BracketOpen) -> parse_flow_sequence(advance(parser))
    Some(lexer.BraceOpen) -> parse_flow_mapping(advance(parser))
    Some(lexer.Plain(s)) -> Ok(#(parse_scalar(s), advance(parser)))
    Some(lexer.SingleQuoted(s)) | Some(lexer.DoubleQuoted(s)) ->
      Ok(#(value.String(s), advance(parser)))
    Some(lexer.Anchor(name)) -> {
      let parser = advance(parser) |> skip_whitespace
      use #(val, parser) <- result.try(parse_flow_value(parser))
      let parser = types.register_anchor(parser, name, val)
      Ok(#(val, parser))
    }
    Some(lexer.Alias(name)) -> {
      let parser = advance(parser)
      case types.resolve_alias(parser, name) {
        Ok(#(val, parser)) -> Ok(#(val, parser))
        Error("budget exceeded") ->
          Error(ParseError(
            "Alias expansion budget exceeded (possible alias-bomb)",
            parser.pos,
          ))
        Error(_) -> Error(ParseError("Unknown anchor: " <> name, parser.pos))
      }
    }
    Some(lexer.Tag(tag)) -> {
      let parser = advance(parser) |> skip_whitespace
      let is_str_tag =
        tag == "!!str" || string.contains(tag, "tag:yaml.org,2002:str")
      case current(parser), is_str_tag {
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
