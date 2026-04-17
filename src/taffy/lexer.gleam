//// YAML lexer - tokenizes YAML input.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Token {
  DocStart
  DocEnd
  Colon
  Question
  Dash
  BracketOpen
  BracketClose
  BraceOpen
  BraceClose
  Comma
  Anchor(String)
  Alias(String)
  Tag(String)
  Literal(String)
  Folded(String)
  Plain(String)
  SingleQuoted(String)
  DoubleQuoted(String)
  Comment(String)
  Newline
  Indent(Int)
  Directive(String)
  Eof
}

pub type Lexer {
  Lexer(
    input: String,
    input_chars: List(String),
    chars: List(String),
    pos: Int,
    line: Int,
    col: Int,
    in_document: Bool,
    flow_level: Int,
    quoted_open_col: Int,
  )
}

pub fn new(input: String) -> Lexer {
  let chars = string.to_graphemes(input)
  Lexer(
    input: input,
    input_chars: chars,
    chars: chars,
    pos: 0,
    line: 1,
    col: 0,
    in_document: False,
    flow_level: 0,
    quoted_open_col: 0,
  )
}

fn list_to_string(chars: List(String)) -> String {
  chars |> list.reverse |> string.join("")
}

fn back_up(lexer: Lexer) -> Lexer {
  let new_pos = lexer.pos - 1
  Lexer(..lexer, pos: new_pos, chars: list.drop(lexer.input_chars, new_pos))
}

pub fn tokenize(input: String) -> Result(List(Token), String) {
  let lexer = new(input)
  case count_indent(lexer) {
    Error(e) -> Error(e)
    Ok(#(0, lexer)) -> tokenize_all(lexer, [])
    Ok(#(n, lexer)) -> tokenize_all(lexer, [Indent(n)])
  }
}

fn tokenize_all(lexer: Lexer, acc: List(Token)) -> Result(List(Token), String) {
  case next_token(lexer) {
    Ok(#(token, new_lexer)) -> {
      case token {
        Eof -> Ok(list.reverse([Eof, ..acc]))
        _ -> tokenize_all(new_lexer, [token, ..acc])
      }
    }
    Error(e) -> Error(e)
  }
}

pub fn next_token(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Ok(#(Eof, lexer))
    Some("\n") -> lex_after_newline(advance(lexer))
    Some("\r") -> {
      let lexer = advance(lexer)
      let lexer = case peek(lexer) {
        Some("\n") -> advance(lexer)
        _ -> lexer
      }
      lex_after_newline(lexer)
    }
    Some(" ") | Some("\t") -> next_token(advance(lexer))
    Some("#") -> lex_comment(lexer)
    Some(":") -> lex_colon(advance(lexer))
    Some("?") -> lex_question(advance(lexer))
    Some("-") -> lex_dash(lexer)
    Some(".") -> lex_dot(lexer)
    Some("[") -> {
      let lexer = advance(lexer)
      Ok(#(BracketOpen, Lexer(..lexer, flow_level: lexer.flow_level + 1)))
    }
    Some("]") -> {
      let lexer = advance(lexer)
      Ok(#(
        BracketClose,
        Lexer(..lexer, flow_level: lexer.flow_level - 1),
      ))
    }
    Some("{") -> {
      let lexer = advance(lexer)
      Ok(#(BraceOpen, Lexer(..lexer, flow_level: lexer.flow_level + 1)))
    }
    Some("}") -> {
      let lexer = advance(lexer)
      Ok(#(
        BraceClose,
        Lexer(..lexer, flow_level: lexer.flow_level - 1),
      ))
    }
    Some(",") -> Ok(#(Comma, advance(lexer)))
    Some("&") -> {
      let #(name, lexer) = read_identifier(advance(lexer))
      Ok(#(Anchor(name), lexer))
    }
    Some("*") -> {
      let #(name, lexer) = read_identifier(advance(lexer))
      Ok(#(Alias(name), lexer))
    }
    Some("!") -> {
      let #(tag, lexer) = read_tag(advance(lexer))
      Ok(#(Tag(tag), lexer))
    }
    Some("|") -> read_literal_block(advance(lexer))
    Some(">") -> read_folded_block(advance(lexer))
    Some("'") -> read_single_quoted(advance(lexer))
    Some("\"") -> read_double_quoted(advance(lexer))
    Some("%") -> lex_percent(lexer)
    Some(_) -> read_plain_scalar(lexer)
  }
}

fn lex_after_newline(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case count_indent(lexer) {
    Error(e) -> Error(e)
    Ok(#(0, lexer)) -> Ok(#(Newline, lexer))
    Ok(#(n, lexer)) -> Ok(#(Indent(n), lexer))
  }
}

fn lex_comment(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  let valid = case lexer.pos > 0 {
    False -> True
    True -> {
      let before = string.slice(lexer.input, lexer.pos - 1, 1)
      case before {
        " " | "\t" | "\n" | "\r" -> True
        _ -> False
      }
    }
  }
  case valid {
    True -> {
      let #(text, lexer) = read_until_newline(advance(lexer))
      Ok(#(Comment(text), lexer))
    }
    False -> Error("'#' must be preceded by whitespace")
  }
}

fn lex_colon(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
      Ok(#(Colon, lexer))
    Some(",") | Some("]") | Some("}") -> Ok(#(Colon, lexer))
    Some("\"") | Some("'") | Some("[") | Some("{") -> Ok(#(Colon, lexer))
    _ ->
      read_plain_scalar(back_up(lexer))
  }
}

fn lex_question(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
      Ok(#(Question, lexer))
    _ ->
      read_plain_scalar(back_up(lexer))
  }
}

fn lex_dash(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek_n(lexer, 3) {
    "---" -> lex_doc_start(lexer)
    _ -> lex_dash_indicator(advance(lexer))
  }
}

fn lex_doc_start(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  let after_dashes = advance_n(lexer, 3)
  case peek(after_dashes) {
    Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
      Ok(#(DocStart, Lexer(..after_dashes, in_document: True)))
    _ ->
      read_plain_scalar(lexer)
  }
}

fn lex_dash_indicator(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
      Ok(#(Dash, lexer))
    Some(",") | Some("]") | Some("}") | Some("[") | Some("{")
      if lexer.flow_level > 0
    -> Error("Invalid use of dash indicator in flow context")
    _ ->
      read_plain_scalar(back_up(lexer))
  }
}

fn lex_dot(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek_n(lexer, 3) {
    "..." -> {
      let after_dots = advance_n(lexer, 3)
      case peek(after_dots) {
        Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
          Ok(#(DocEnd, Lexer(..after_dots, in_document: False)))
        _ -> read_plain_scalar(lexer)
      }
    }
    _ -> read_plain_scalar(lexer)
  }
}

fn lex_percent(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case lexer.col, lexer.in_document {
    0, False -> lex_possible_directive(lexer)
    _, _ -> read_plain_scalar(lexer)
  }
}

fn lex_possible_directive(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(advance(lexer)) {
    Some(next) -> {
      case is_letter(next) {
        True -> {
          let #(text, lexer) = read_until_newline(advance(lexer))
          Ok(#(Directive(text), lexer))
        }
        False -> read_plain_scalar(lexer)
      }
    }
    None -> read_plain_scalar(lexer)
  }
}

fn peek(lexer: Lexer) -> Option(String) {
  case lexer.chars {
    [c, ..] -> Some(c)
    [] -> None
  }
}

fn peek_n(lexer: Lexer, n: Int) -> String {
  case n {
    3 ->
      case lexer.chars {
        [a, b, c, ..] -> a <> b <> c
        [a, b] -> a <> b
        [a] -> a
        [] -> ""
      }
    _ -> take_n_string(lexer.chars, n, [])
  }
}

fn take_n_string(chars: List(String), n: Int, acc: List(String)) -> String {
  case n, chars {
    0, _ -> list.reverse(acc) |> string.join("")
    _, [c, ..rest] -> take_n_string(rest, n - 1, [c, ..acc])
    _, [] -> list.reverse(acc) |> string.join("")
  }
}

fn advance(lexer: Lexer) -> Lexer {
  case lexer.chars {
    [_, ..rest] ->
      Lexer(..lexer, pos: lexer.pos + 1, col: lexer.col + 1, chars: rest)
    [] -> lexer
  }
}

fn advance_n(lexer: Lexer, n: Int) -> Lexer {
  Lexer(
    ..lexer,
    pos: lexer.pos + n,
    col: lexer.col + n,
    chars: list.drop(lexer.chars, n),
  )
}

fn count_indent(lexer: Lexer) -> Result(#(Int, Lexer), String) {
  count_indent_loop(lexer, 0)
}

fn count_indent_loop(lexer: Lexer, count: Int) -> Result(#(Int, Lexer), String) {
  case peek(lexer) {
    Some(" ") -> count_indent_loop(advance(lexer), count + 1)
    Some("\t") ->
      case count {
        0 -> check_tab_at_line_start(lexer)
        _ -> Ok(#(count, Lexer(..lexer, col: count)))
      }
    _ -> Ok(#(count, Lexer(..lexer, col: count)))
  }
}

fn check_tab_at_line_start(lexer: Lexer) -> Result(#(Int, Lexer), String) {
  let after_tabs = skip_tabs_lexer(advance(lexer))
  case peek(after_tabs) {
    None | Some("\n") | Some("\r") -> Ok(#(0, Lexer(..lexer, col: 0)))
    Some("-") -> check_tab_before_dash(after_tabs, lexer)
    Some("?") -> check_tab_before_indicator(after_tabs)
    _ ->
      case has_mapping_indicator_after_tab(after_tabs) {
        True -> Error("Tabs are not allowed as indentation in YAML")
        False -> Ok(#(0, Lexer(..after_tabs, col: 0)))
      }
  }
}

fn check_tab_before_dash(
  after_tabs: Lexer,
  _original: Lexer,
) -> Result(#(Int, Lexer), String) {
  case peek_n(after_tabs, 3) {
    "---" -> Ok(#(0, Lexer(..after_tabs, col: 0)))
    _ -> {
      let after_dash = advance(after_tabs)
      case peek(after_dash) {
        Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
          Error("Tabs are not allowed as indentation in YAML")
        _ -> Ok(#(0, Lexer(..after_tabs, col: 0)))
      }
    }
  }
}

fn check_tab_before_indicator(
  after_tabs: Lexer,
) -> Result(#(Int, Lexer), String) {
  let after_indicator = advance(after_tabs)
  case peek(after_indicator) {
    Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
      Error("Tabs are not allowed as indentation in YAML")
    _ -> Ok(#(0, Lexer(..after_tabs, col: 0)))
  }
}

fn skip_tabs_lexer(lexer: Lexer) -> Lexer {
  case peek(lexer) {
    Some("\t") -> skip_tabs_lexer(advance(lexer))
    Some(" ") -> skip_tabs_lexer(advance(lexer))
    _ -> lexer
  }
}

fn has_mapping_indicator_after_tab(lexer: Lexer) -> Bool {
  case peek(lexer) {
    None | Some("\n") | Some("\r") -> False
    Some(":") -> {
      case peek(advance(lexer)) {
        Some(" ") | Some("\t") | Some("\n") | Some("\r") | None -> True
        _ -> False
      }
    }
    Some("[") | Some("]") | Some("{") | Some("}") -> False
    Some("'") | Some("\"") -> False
    _ -> scan_for_mapping_colon(advance(lexer))
  }
}

fn scan_for_mapping_colon(lexer: Lexer) -> Bool {
  case peek(lexer) {
    None | Some("\n") | Some("\r") -> False
    Some(":") -> {
      case peek(advance(lexer)) {
        Some(" ") | Some("\t") | Some("\n") | Some("\r") | None -> True
        _ -> scan_for_mapping_colon(advance(lexer))
      }
    }
    _ -> scan_for_mapping_colon(advance(lexer))
  }
}

fn read_until_newline(lexer: Lexer) -> #(String, Lexer) {
  read_until_newline_loop(lexer, [])
}

fn read_until_newline_loop(lexer: Lexer, acc: List(String)) -> #(String, Lexer) {
  case peek(lexer) {
    None -> #(list_to_string(acc), lexer)
    Some("\n") -> #(list_to_string(acc), lexer)
    Some("\r") -> #(list_to_string(acc), lexer)
    Some(c) -> read_until_newline_loop(advance(lexer), [c, ..acc])
  }
}

fn read_identifier(lexer: Lexer) -> #(String, Lexer) {
  read_identifier_loop(lexer, [])
}

fn read_identifier_loop(lexer: Lexer, acc: List(String)) -> #(String, Lexer) {
  case peek(lexer) {
    Some(" ")
    | Some("\t")
    | Some("\n")
    | Some("\r")
    | Some(",")
    | Some("[")
    | Some("]")
    | Some("{")
    | Some("}")
    | None -> #(list_to_string(acc), lexer)
    Some(c) -> read_identifier_loop(advance(lexer), [c, ..acc])
  }
}

fn read_tag(lexer: Lexer) -> #(String, Lexer) {
  case peek(lexer) {
    Some("<") -> read_verbatim_tag(advance(lexer), ["<", "!"])
    _ -> read_tag_loop(lexer, ["!"])
  }
}

fn read_verbatim_tag(lexer: Lexer, acc: List(String)) -> #(String, Lexer) {
  case peek(lexer) {
    Some(">") -> #(list_to_string([">", ..acc]), advance(lexer))
    Some(c) -> read_verbatim_tag(advance(lexer), [c, ..acc])
    None -> #(list_to_string(acc), lexer)
  }
}

fn read_tag_loop(lexer: Lexer, acc: List(String)) -> #(String, Lexer) {
  case peek(lexer) {
    Some(" ")
    | Some("\t")
    | Some("\n")
    | Some("\r")
    | Some(",")
    | Some("[")
    | Some("]")
    | Some("{")
    | Some("}")
    | None -> #(list_to_string(acc), lexer)
    Some(c) -> read_tag_loop(advance(lexer), [c, ..acc])
  }
}

fn read_single_quoted(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  read_single_quoted_loop(lexer, [], False)
}

fn read_single_quoted_loop(
  lexer: Lexer,
  acc: List(String),
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Error("Unterminated single-quoted string")
    Some("'") -> {
      let lexer = advance(lexer)
      case peek(lexer) {
        Some("'") ->
          read_single_quoted_loop(advance(lexer), ["'", ..acc], multiline)
        _ -> {
          let s = list_to_string(acc)
          case multiline {
            True -> check_multiline_implicit_key(lexer, SingleQuoted(s))
            False -> Ok(#(SingleQuoted(s), lexer))
          }
        }
      }
    }
    Some("\n") -> {
      let lexer = advance(lexer)
      case check_document_marker(lexer) {
        True ->
          Error("Unterminated single-quoted string")
        False -> {
          let lexer = skip_quoted_continuation_whitespace(lexer)
          case peek(lexer) {
            Some("\n") -> {
              read_single_quoted_loop(lexer, ["\n", ..acc], True)
            }
            _ -> {
              read_single_quoted_loop(lexer, [" ", ..acc], True)
            }
          }
        }
      }
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      let lexer = case peek(lexer) {
        Some("\n") -> advance(lexer)
        _ -> lexer
      }
      case check_document_marker(lexer) {
        True ->
          Error("Unterminated single-quoted string")
        False -> {
          let lexer = skip_quoted_continuation_whitespace(lexer)
          case peek(lexer) {
            Some("\n") | Some("\r") ->
              read_single_quoted_loop(lexer, ["\n", ..acc], True)
            _ -> read_single_quoted_loop(lexer, [" ", ..acc], True)
          }
        }
      }
    }
    Some(c) -> read_single_quoted_loop(advance(lexer), [c, ..acc], multiline)
  }
}

fn check_multiline_implicit_key(
  lexer: Lexer,
  token: Token,
) -> Result(#(Token, Lexer), String) {
  case lexer.flow_level > 0 {
    True -> Ok(#(token, lexer))
    False -> check_multiline_block_key(lexer, token)
  }
}

fn check_multiline_block_key(
  lexer: Lexer,
  token: Token,
) -> Result(#(Token, Lexer), String) {
  let check_lexer = skip_inline_spaces(lexer)
  case peek(check_lexer) {
    Some(":") -> check_colon_is_mapping_indicator(lexer, token, check_lexer)
    _ -> Ok(#(token, lexer))
  }
}

fn check_colon_is_mapping_indicator(
  lexer: Lexer,
  token: Token,
  check_lexer: Lexer,
) -> Result(#(Token, Lexer), String) {
  let after_colon = advance(check_lexer)
  case peek(after_colon) {
    Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
      Error("Multiline quoted string cannot be used as an implicit key")
    Some(",") | Some("]") | Some("}") ->
      Error("Multiline quoted string cannot be used as an implicit key")
    _ -> Ok(#(token, lexer))
  }
}

fn skip_inline_spaces(lexer: Lexer) -> Lexer {
  case peek(lexer) {
    Some(" ") -> skip_inline_spaces(advance(lexer))
    _ -> lexer
  }
}

fn check_document_marker(lexer: Lexer) -> Bool {
  case peek_n(lexer, 3) {
    "---" | "..." -> {
      let after = advance_n(lexer, 3)
      case peek(after) {
        Some(" ") | Some("\n") | Some("\r") | Some("\t") | None -> True
        _ -> False
      }
    }
    _ -> False
  }
}

fn skip_quoted_continuation_whitespace(lexer: Lexer) -> Lexer {
  case peek(lexer) {
    Some(" ") | Some("\t") ->
      skip_quoted_continuation_whitespace(advance(lexer))
    _ -> lexer
  }
}

fn count_continuation_indent(lexer: Lexer) -> #(Int, Lexer) {
  count_continuation_indent_loop(lexer, 0)
}

fn count_continuation_indent_loop(lexer: Lexer, count: Int) -> #(Int, Lexer) {
  case peek(lexer) {
    Some(" ") -> count_continuation_indent_loop(advance(lexer), count + 1)
    Some("\t") -> count_continuation_indent_loop(advance(lexer), count + 1)
    _ -> #(count, lexer)
  }
}

fn read_double_quoted(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  let in_mapping_value = is_after_mapping_colon(lexer)
  let lexer =
    Lexer(..lexer, quoted_open_col: case in_mapping_value {
      True -> lexer.col
      False -> 0
    })
  read_double_quoted_loop(lexer, [], "", False)
}

fn is_after_mapping_colon(lexer: Lexer) -> Bool {
  case lexer.pos >= 3 {
    True -> {
      let before_quote = string.slice(lexer.input, lexer.pos - 2, 1)
      let before_space = string.slice(lexer.input, lexer.pos - 3, 1)
      before_quote == " " && before_space == ":"
    }
    False -> False
  }
}

fn read_double_quoted_loop(
  lexer: Lexer,
  acc: List(String),
  pending_ws: String,
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Error("Unterminated double-quoted string")
    Some("\"") -> {
      let s = case pending_ws {
        "" -> list_to_string(acc)
        _ -> list_to_string([pending_ws, ..acc])
      }
      case multiline {
        True -> check_multiline_implicit_key(advance(lexer), DoubleQuoted(s))
        False -> Ok(#(DoubleQuoted(s), advance(lexer)))
      }
    }
    Some("\\") -> {
      let full_acc = case pending_ws {
        "" -> acc
        _ -> [pending_ws, ..acc]
      }
      let lexer = advance(lexer)
      case peek(lexer) {
        None -> Error("Unterminated escape sequence")
        Some("n") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\n", ..full_acc],
            "",
            multiline,
          )
        Some("t") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\t", ..full_acc],
            "",
            multiline,
          )
        Some("r") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\r", ..full_acc],
            "",
            multiline,
          )
        Some("\\") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\\", ..full_acc],
            "",
            multiline,
          )
        Some("\"") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\"", ..full_acc],
            "",
            multiline,
          )
        Some("/") ->
          read_double_quoted_loop(
            advance(lexer),
            ["/", ..full_acc],
            "",
            multiline,
          )
        Some("0") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{0000}", ..full_acc],
            "",
            multiline,
          )
        Some("a") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{0007}", ..full_acc],
            "",
            multiline,
          )
        Some("b") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{0008}", ..full_acc],
            "",
            multiline,
          )
        Some("e") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{001B}", ..full_acc],
            "",
            multiline,
          )
        Some("f") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{000C}", ..full_acc],
            "",
            multiline,
          )
        Some("v") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{000B}", ..full_acc],
            "",
            multiline,
          )
        Some("N") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{0085}", ..full_acc],
            "",
            multiline,
          )
        Some("_") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{00A0}", ..full_acc],
            "",
            multiline,
          )
        Some("L") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{2028}", ..full_acc],
            "",
            multiline,
          )
        Some("P") ->
          read_double_quoted_loop(
            advance(lexer),
            ["\u{2029}", ..full_acc],
            "",
            multiline,
          )
        Some(" ") ->
          read_double_quoted_loop(
            advance(lexer),
            [" ", ..full_acc],
            "",
            multiline,
          )
        Some("x") -> read_hex_escape(advance(lexer), full_acc, 2, multiline)
        Some("u") -> read_hex_escape(advance(lexer), full_acc, 4, multiline)
        Some("U") -> read_hex_escape(advance(lexer), full_acc, 8, multiline)
        Some("\n") -> {
          let next_lexer = advance(lexer)
          case check_document_marker(next_lexer) {
            True ->
              Error("Unterminated double-quoted string")
            False -> skip_line_continuation(next_lexer, full_acc)
          }
        }
        Some("\r") -> {
          let lexer = advance(lexer)
          let next_lexer = case peek(lexer) {
            Some("\n") -> advance(lexer)
            _ -> lexer
          }
          case check_document_marker(next_lexer) {
            True ->
              Error("Unterminated double-quoted string")
            False -> skip_line_continuation(next_lexer, full_acc)
          }
        }
        Some(c) -> Error("Invalid escape sequence: \\" <> c)
      }
    }
    Some("\n") -> {
      let lexer = advance(lexer)
      case check_document_marker(lexer) {
        True ->
          Error(
            "Unterminated double-quoted string",
          )
        False -> {
          let #(indent, lexer) = count_continuation_indent(lexer)
          case
            lexer.flow_level == 0 && indent == 0 && lexer.quoted_open_col > 0
          {
            True ->
              case peek(lexer) {
                Some("\n") | Some("\r") | Some("\"") | None ->
                  handle_double_quoted_fold(lexer, acc, "")
                _ ->
                  Error(
                    "Multiline double-quoted scalar continuation must be indented",
                  )
              }
            False -> handle_double_quoted_fold(lexer, acc, "")
          }
        }
      }
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      let lexer = case peek(lexer) {
        Some("\n") -> advance(lexer)
        _ -> lexer
      }
      case check_document_marker(lexer) {
        True ->
          Error(
            "Unterminated double-quoted string",
          )
        False -> {
          let #(indent, lexer) = count_continuation_indent(lexer)
          case
            lexer.flow_level == 0 && indent == 0 && lexer.quoted_open_col > 0
          {
            True ->
              case peek(lexer) {
                Some("\n") | Some("\r") | Some("\"") | None ->
                  handle_double_quoted_fold(lexer, acc, "")
                _ ->
                  Error(
                    "Multiline double-quoted scalar continuation must be indented",
                  )
              }
            False -> handle_double_quoted_fold(lexer, acc, "")
          }
        }
      }
    }
    Some(" ") ->
      read_double_quoted_loop(advance(lexer), acc, pending_ws <> " ", multiline)
    Some("\t") ->
      read_double_quoted_loop(
        advance(lexer),
        acc,
        pending_ws <> "\t",
        multiline,
      )
    Some(c) -> {
      let new_acc = case pending_ws {
        "" -> [c, ..acc]
        _ -> [c, pending_ws, ..acc]
      }
      read_double_quoted_loop(advance(lexer), new_acc, "", multiline)
    }
  }
}

fn skip_line_continuation(
  lexer: Lexer,
  acc: List(String),
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    Some(" ") -> skip_line_continuation(advance(lexer), acc)
    Some("\t") -> skip_line_continuation(advance(lexer), acc)
    _ -> read_double_quoted_loop(lexer, acc, "", True)
  }
}

fn handle_double_quoted_fold(
  lexer: Lexer,
  acc: List(String),
  newlines: String,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    Some("\n") -> {
      let lexer = advance(lexer)
      case check_document_marker(lexer) {
        True ->
          Error(
            "Unterminated double-quoted string",
          )
        False -> {
          let #(_indent, lexer) = count_continuation_indent(lexer)
          handle_double_quoted_fold(lexer, acc, newlines <> "\n")
        }
      }
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      let lexer = case peek(lexer) {
        Some("\n") -> advance(lexer)
        _ -> lexer
      }
      case check_document_marker(lexer) {
        True ->
          Error(
            "Unterminated double-quoted string",
          )
        False -> {
          let #(_indent, lexer) = count_continuation_indent(lexer)
          handle_double_quoted_fold(lexer, acc, newlines <> "\n")
        }
      }
    }
    _ -> {
      case newlines {
        "" -> read_double_quoted_loop(lexer, [" ", ..acc], "", True)
        _ -> read_double_quoted_loop(lexer, [newlines, ..acc], "", True)
      }
    }
  }
}

fn read_hex_escape(
  lexer: Lexer,
  acc: List(String),
  digits: Int,
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  use #(hex_str, lexer) <- result.try(read_hex_digits(lexer, [], digits))
  decode_hex_codepoint(lexer, acc, hex_str, multiline)
}

fn decode_hex_codepoint(
  lexer: Lexer,
  acc: List(String),
  hex_str: String,
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  case parse_hex(hex_str) {
    Error(_) -> Error("Invalid hex escape: " <> hex_str)
    Ok(codepoint) ->
      case string.utf_codepoint(codepoint) {
        Error(_) -> Error("Invalid unicode codepoint: " <> hex_str)
        Ok(cp) ->
          read_double_quoted_loop(
            lexer,
            [string.from_utf_codepoints([cp]), ..acc],
            "",
            multiline,
          )
      }
  }
}

fn read_hex_digits(
  lexer: Lexer,
  acc: List(String),
  remaining: Int,
) -> Result(#(String, Lexer), String) {
  case remaining, peek(lexer) {
    0, _ -> Ok(#(list_to_string(acc), lexer))
    _, None -> Error("Unterminated hex escape")
    _, Some(c) ->
      case is_hex_char(c) {
        True -> read_hex_digits(advance(lexer), [c, ..acc], remaining - 1)
        False -> Error("Invalid hex character: " <> c)
      }
  }
}

fn is_letter(c: String) -> Bool {
  case c {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" ->
      True
    "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ->
      True
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" ->
      True
    "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ->
      True
    _ -> False
  }
}

fn is_hex_char(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "a" | "b" | "c" | "d" | "e" | "f" -> True
    "A" | "B" | "C" | "D" | "E" | "F" -> True
    _ -> False
  }
}

fn parse_hex(s: String) -> Result(Int, Nil) {
  parse_hex_loop(string.to_graphemes(s), 0)
}

fn parse_hex_loop(chars: List(String), acc: Int) -> Result(Int, Nil) {
  case chars {
    [] -> Ok(acc)
    [c, ..rest] -> {
      case hex_value(c) {
        Ok(v) -> parse_hex_loop(rest, acc * 16 + v)
        Error(_) -> Error(Nil)
      }
    }
  }
}

fn hex_value(c: String) -> Result(Int, Nil) {
  case c {
    "0" -> Ok(0)
    "1" -> Ok(1)
    "2" -> Ok(2)
    "3" -> Ok(3)
    "4" -> Ok(4)
    "5" -> Ok(5)
    "6" -> Ok(6)
    "7" -> Ok(7)
    "8" -> Ok(8)
    "9" -> Ok(9)
    "a" | "A" -> Ok(10)
    "b" | "B" -> Ok(11)
    "c" | "C" -> Ok(12)
    "d" | "D" -> Ok(13)
    "e" | "E" -> Ok(14)
    "f" | "F" -> Ok(15)
    _ -> Error(Nil)
  }
}

fn read_plain_scalar(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  read_plain_scalar_loop(lexer, [])
}

fn read_plain_scalar_loop(
  lexer: Lexer,
  acc: List(String),
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Ok(#(Plain(string.trim_end(list_to_string(acc))), lexer))
    Some("\n") | Some("\r") ->
      Ok(#(Plain(string.trim_end(list_to_string(acc))), lexer))
    Some(",") | Some("]") | Some("}") ->
      Ok(#(Plain(string.trim_end(list_to_string(acc))), lexer))
    Some("#") -> {
      case acc {
        [" ", ..] | ["\t", ..] ->
          Ok(#(Plain(string.trim_end(list_to_string(acc))), lexer))
        _ -> read_plain_scalar_loop(advance(lexer), ["#", ..acc])
      }
    }
    Some(":") -> {
      let next_lexer = advance(lexer)
      case peek(next_lexer) {
        Some(" ") | Some("\t") | Some("\n") | Some("\r") | None ->
          Ok(#(Plain(string.trim_end(list_to_string(acc))), lexer))
        _ -> read_plain_scalar_loop(next_lexer, [":", ..acc])
      }
    }
    Some(c) -> read_plain_scalar_loop(advance(lexer), [c, ..acc])
  }
}

fn read_literal_block(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case read_block_header(lexer) {
    Error(e) -> Error(e)
    Ok(#(header, lexer)) -> read_literal_block_content(header, lexer)
  }
}

fn read_literal_block_content(
  header: BlockHeader,
  lexer: Lexer,
) -> Result(#(Token, Lexer), String) {
  let lexer = skip_to_eol(lexer)

  case skip_newline(lexer) {
    Ok(lexer) ->
      read_literal_content(lexer, header.chomping, header.explicit_indent)
    Error(Nil) -> Ok(#(Literal(""), lexer))
  }
}

fn read_folded_block(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case read_block_header(lexer) {
    Error(e) -> Error(e)
    Ok(#(header, lexer)) -> {
      let lexer = skip_to_eol(lexer)
      case skip_newline(lexer) {
        Ok(lexer) ->
          read_folded_content(lexer, header.chomping, header.explicit_indent)
        Error(Nil) -> Ok(#(Folded(""), lexer))
      }
    }
  }
}

type Chomping {
  Strip
  Clip
  Keep
}

type BlockHeader {
  BlockHeader(chomping: Chomping, explicit_indent: Option(Int))
}

fn read_block_header(lexer: Lexer) -> Result(#(BlockHeader, Lexer), String) {
  read_block_header_loop(lexer, BlockHeader(Clip, None), False)
}

fn read_block_header_loop(
  lexer: Lexer,
  header: BlockHeader,
  saw_whitespace: Bool,
) -> Result(#(BlockHeader, Lexer), String) {
  case peek(lexer) {
    Some("-") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, chomping: Strip),
        False,
      )
    Some("+") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, chomping: Keep),
        False,
      )
    Some("0") -> Error("Block scalar indent indicator cannot be 0")
    Some("1") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(1)),
        False,
      )
    Some("2") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(2)),
        False,
      )
    Some("3") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(3)),
        False,
      )
    Some("4") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(4)),
        False,
      )
    Some("5") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(5)),
        False,
      )
    Some("6") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(6)),
        False,
      )
    Some("7") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(7)),
        False,
      )
    Some("8") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(8)),
        False,
      )
    Some("9") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(9)),
        False,
      )
    Some(" ") | Some("\t") ->
      read_block_header_loop(advance(lexer), header, True)
    Some("#") -> {
      case saw_whitespace {
        True -> {
          let lexer = skip_to_eol(lexer)
          Ok(#(header, lexer))
        }
        False ->
          Error("Invalid block scalar header")
      }
    }
    Some("\n") | Some("\r") | None -> Ok(#(header, lexer))
    Some(c) -> Error("Invalid character in block scalar header: " <> c)
  }
}

fn skip_newline(lexer: Lexer) -> Result(Lexer, Nil) {
  case peek(lexer) {
    Some("\n") -> Ok(advance(lexer))
    Some("\r") -> {
      let lexer = advance(lexer)
      case peek(lexer) {
        Some("\n") -> Ok(advance(lexer))
        _ -> Ok(lexer)
      }
    }
    _ -> Error(Nil)
  }
}

fn skip_to_eol(lexer: Lexer) -> Lexer {
  case peek(lexer) {
    None -> lexer
    Some("\n") -> lexer
    Some("\r") -> lexer
    Some(_) -> skip_to_eol(advance(lexer))
  }
}

fn read_literal_content(
  lexer: Lexer,
  chomping: Chomping,
  explicit_indent: Option(Int),
) -> Result(#(Token, Lexer), String) {
  let #(leading_newlines, max_empty_indent, lexer) =
    count_leading_empty_lines(lexer, 0, 0)
  case find_block_indent(lexer) {
    Error(e) -> Error(e)
    Ok(NoContent(end_lexer)) -> {
      let actual_trailing = case peek(end_lexer) {
        None -> leading_newlines
        _ -> 0
      }
      let content = case chomping {
        Strip -> ""
        Clip -> ""
        Keep -> string.repeat("\n", actual_trailing)
      }
      Ok(#(Literal(content), lexer))
    }
    Ok(FoundContent(detected_indent, lexer)) -> {
      case max_empty_indent > detected_indent && detected_indent > 0 {
        True -> Error("Leading empty line has too many spaces in block scalar")
        False -> {
          let #(boundary_indent, base_indent) = case explicit_indent {
            Some(n) -> {
              let boundary = find_min_block_indent(lexer, detected_indent, n)
              #(boundary, boundary)
            }
            None -> #(detected_indent, detected_indent)
          }
          let prefix = string.repeat("\n", leading_newlines)
          let #(content, lexer) =
            read_literal_lines_ex(lexer, boundary_indent, base_indent, prefix)
          let content = apply_chomping(content, chomping)
          Ok(#(Literal(content), lexer))
        }
      }
    }
  }
}

fn read_folded_content(
  lexer: Lexer,
  chomping: Chomping,
  explicit_indent: Option(Int),
) -> Result(#(Token, Lexer), String) {
  let #(leading_newlines, max_empty_indent, lexer) =
    count_leading_empty_lines(lexer, 0, 0)
  let prefix = string.repeat("\n", leading_newlines)
  let initial_state = case leading_newlines > 0 {
    True -> FoldAfterBlank
    False -> FoldNormal
  }

  case explicit_indent {
    Some(indent) -> {
      let #(content, lexer) =
        read_folded_lines(lexer, indent, prefix, initial_state)
      let content = apply_chomping(content, chomping)
      Ok(#(Folded(content), lexer))
    }
    None -> {
      case find_block_indent(lexer) {
        Error(e) -> Error(e)
        Ok(NoContent(end_lexer)) -> {
          let actual_trailing = case peek(end_lexer) {
            None -> leading_newlines
            _ -> 0
          }
          let content = case chomping {
            Strip -> ""
            Clip -> ""
            Keep -> string.repeat("\n", actual_trailing)
          }
          Ok(#(Folded(content), lexer))
        }
        Ok(FoundContent(block_indent, lexer)) -> {
          case max_empty_indent > block_indent && block_indent > 0 {
            True ->
              Error("Leading empty line has too many spaces in block scalar")
            False -> {
              let #(content, lexer) =
                read_folded_lines(lexer, block_indent, prefix, initial_state)
              let content = apply_chomping(content, chomping)
              Ok(#(Folded(content), lexer))
            }
          }
        }
      }
    }
  }
}

fn count_leading_empty_lines(
  lexer: Lexer,
  count: Int,
  max_indent: Int,
) -> #(Int, Int, Lexer) {
  let #(indent, lexer_after_spaces) = count_leading_spaces(lexer, 0)
  case peek(lexer_after_spaces) {
    Some("\n") -> {
      let new_max = case indent > max_indent {
        True -> indent
        False -> max_indent
      }
      count_leading_empty_lines(advance(lexer_after_spaces), count + 1, new_max)
    }
    Some("\r") -> {
      let new_max = case indent > max_indent {
        True -> indent
        False -> max_indent
      }
      let lexer = advance(lexer_after_spaces)
      case peek(lexer) {
        Some("\n") ->
          count_leading_empty_lines(advance(lexer), count + 1, new_max)
        _ -> count_leading_empty_lines(lexer, count + 1, new_max)
      }
    }
    _ -> #(count, max_indent, lexer)
  }
}

type BlockIndentResult {
  FoundContent(indent: Int, lexer: Lexer)
  NoContent(lexer: Lexer)
}

fn find_block_indent(lexer: Lexer) -> Result(BlockIndentResult, String) {
  let line_start = lexer
  let #(indent, lexer_after_spaces) = count_leading_spaces(lexer, 0)
  case peek(lexer_after_spaces) {
    Some("\n") -> find_block_indent(advance(lexer_after_spaces))
    Some("\r") -> {
      let lexer = advance(lexer_after_spaces)
      case peek(lexer) {
        Some("\n") -> find_block_indent(advance(lexer))
        _ -> find_block_indent(lexer)
      }
    }
    Some("\t") if indent == 0 ->
      Error("Tabs are not allowed as indentation in YAML")
    Some(_) -> {
      case indent {
        0 -> {
          case is_block_terminator(lexer_after_spaces) {
            True -> Ok(NoContent(line_start))
            False -> Ok(FoundContent(0, line_start))
          }
        }
        _ -> Ok(FoundContent(indent, line_start))
      }
    }
    None -> Ok(NoContent(lexer_after_spaces))
  }
}

fn is_doc_marker(lexer: Lexer) -> Bool {
  case peek_n(lexer, 3) {
    "---" | "..." -> {
      let after = advance_n(lexer, 3)
      case peek(after) {
        Some(" ") | Some("\t") | Some("\n") | Some("\r") | None -> True
        _ -> False
      }
    }
    _ -> False
  }
}

fn is_block_terminator(lexer: Lexer) -> Bool {
  case peek_n(lexer, 3) {
    "---" | "..." -> True
    _ -> {
      check_for_colon_in_line(lexer)
    }
  }
}

fn check_for_colon_in_line(lexer: Lexer) -> Bool {
  case peek(lexer) {
    None | Some("\n") | Some("\r") -> False
    Some(":") -> {
      case peek(advance(lexer)) {
        None | Some(" ") | Some("\t") | Some("\n") | Some("\r") -> True
        _ -> check_for_colon_in_line(advance(lexer))
      }
    }
    _ -> check_for_colon_in_line(advance(lexer))
  }
}

fn count_leading_spaces(lexer: Lexer, count: Int) -> #(Int, Lexer) {
  case peek(lexer) {
    Some(" ") -> count_leading_spaces(advance(lexer), count + 1)
    _ -> #(count, lexer)
  }
}

fn find_min_block_indent(lexer: Lexer, current_min: Int, explicit: Int) -> Int {
  find_min_block_indent_loop(lexer, current_min, explicit)
}

fn find_min_block_indent_loop(
  lexer: Lexer,
  current_min: Int,
  explicit: Int,
) -> Int {
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)
  case peek(after_spaces) {
    Some("\n") ->
      find_min_block_indent_loop(advance(after_spaces), current_min, explicit)
    Some("\r") -> {
      let next = advance(after_spaces)
      case peek(next) {
        Some("\n") ->
          find_min_block_indent_loop(advance(next), current_min, explicit)
        _ -> find_min_block_indent_loop(next, current_min, explicit)
      }
    }
    None -> current_min
    Some(_) -> {
      case indent {
        0 -> current_min
        _ if indent > explicit -> {
          let new_min = case indent < current_min {
            True -> indent
            False -> current_min
          }
          skip_line_and_continue(after_spaces, new_min, explicit)
        }
        _ -> {
          case is_block_terminator(after_spaces) {
            True -> current_min
            False -> {
              let new_min = case indent < current_min {
                True -> indent
                False -> current_min
              }
              skip_line_and_continue(after_spaces, new_min, explicit)
            }
          }
        }
      }
    }
  }
}

fn skip_line_and_continue(lexer: Lexer, current_min: Int, explicit: Int) -> Int {
  let #(_, after_line) = read_until_newline(lexer)
  case peek(after_line) {
    Some("\n") ->
      find_min_block_indent_loop(advance(after_line), current_min, explicit)
    Some("\r") -> {
      let next = advance(after_line)
      case peek(next) {
        Some("\n") ->
          find_min_block_indent_loop(advance(next), current_min, explicit)
        _ -> find_min_block_indent_loop(next, current_min, explicit)
      }
    }
    _ -> current_min
  }
}

fn read_literal_lines_ex(
  lexer: Lexer,
  boundary_indent: Int,
  base_indent: Int,
  acc: String,
) -> #(String, Lexer) {
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)
  let extra_indent = case indent > base_indent {
    True -> string.repeat(" ", indent - base_indent)
    False -> ""
  }
  let #(line, lexer) = read_until_eol(after_spaces, extra_indent)
  let new_acc = case acc {
    "" -> line
    _ ->
      case string.ends_with(acc, "\n") {
        True -> acc <> line
        False -> acc <> "\n" <> line
      }
  }

  case peek(lexer) {
    Some("\n") -> {
      let lexer = advance(lexer)
      check_literal_continuation_ex(
        lexer,
        boundary_indent,
        base_indent,
        new_acc,
      )
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      case peek(lexer) {
        Some("\n") ->
          check_literal_continuation_ex(
            advance(lexer),
            boundary_indent,
            base_indent,
            new_acc,
          )
        _ ->
          check_literal_continuation_ex(
            lexer,
            boundary_indent,
            base_indent,
            new_acc,
          )
      }
    }
    _ -> #(new_acc, lexer)
  }
}

fn check_literal_continuation_ex(
  lexer: Lexer,
  boundary_indent: Int,
  base_indent: Int,
  acc: String,
) -> #(String, Lexer) {
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)

  case peek(after_spaces) {
    Some("\n") -> {
      let extra_content = case
        indent >= boundary_indent && indent > base_indent
      {
        True -> string.repeat(" ", indent - base_indent)
        False -> ""
      }
      check_literal_continuation_ex(
        advance(after_spaces),
        boundary_indent,
        base_indent,
        acc <> "\n" <> extra_content,
      )
    }
    Some("\r") -> {
      let lexer_after_cr = advance(after_spaces)
      let extra_content = case
        indent >= boundary_indent && indent > base_indent
      {
        True -> string.repeat(" ", indent - base_indent)
        False -> ""
      }
      case peek(lexer_after_cr) {
        Some("\n") ->
          check_literal_continuation_ex(
            advance(lexer_after_cr),
            boundary_indent,
            base_indent,
            acc <> "\n" <> extra_content,
          )
        _ ->
          check_literal_continuation_ex(
            lexer_after_cr,
            boundary_indent,
            base_indent,
            acc <> "\n" <> extra_content,
          )
      }
    }
    None -> #(acc, lexer)
    Some(_) -> {
      case indent >= boundary_indent {
        True -> {
          case indent == 0 && is_doc_marker(after_spaces) {
            True -> {
              let backed_up = back_up(lexer)
              #(acc, backed_up)
            }
            False -> {
              let extra_indent = case indent > base_indent {
                True -> string.repeat(" ", indent - base_indent)
                False -> ""
              }
              let #(line, lexer) = read_until_eol(after_spaces, extra_indent)
              let new_acc = acc <> "\n" <> line
              case peek(lexer) {
                Some("\n") ->
                  check_literal_continuation_ex(
                    advance(lexer),
                    boundary_indent,
                    base_indent,
                    new_acc,
                  )
                Some("\r") -> {
                  let lexer = advance(lexer)
                  case peek(lexer) {
                    Some("\n") ->
                      check_literal_continuation_ex(
                        advance(lexer),
                        boundary_indent,
                        base_indent,
                        new_acc,
                      )
                    _ ->
                      check_literal_continuation_ex(
                        lexer,
                        boundary_indent,
                        base_indent,
                        new_acc,
                      )
                  }
                }
                _ -> #(new_acc, lexer)
              }
            }
          }
        }
        False -> {
          let backed_up = back_up(lexer)
          #(acc, backed_up)
        }
      }
    }
  }
}

type FoldState {
  FoldNormal
  FoldAfterBlank
  FoldAfterMoreIndented
}

fn read_folded_lines(
  lexer: Lexer,
  block_indent: Int,
  acc: String,
  state: FoldState,
) -> #(String, Lexer) {
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)

  case peek(after_spaces) {
    Some("\n") -> {
      read_folded_lines(
        advance(after_spaces),
        block_indent,
        acc <> "\n",
        FoldAfterBlank,
      )
    }
    Some("\r") -> {
      let lexer = advance(after_spaces)
      case peek(lexer) {
        Some("\n") ->
          read_folded_lines(
            advance(lexer),
            block_indent,
            acc <> "\n",
            FoldAfterBlank,
          )
        _ -> read_folded_lines(lexer, block_indent, acc <> "\n", FoldAfterBlank)
      }
    }
    None -> #(acc, lexer)
    Some(_) -> {
      let should_continue = case indent >= block_indent {
        True ->
          case indent {
            0 -> !is_doc_marker(after_spaces)
            _ -> True
          }
        False -> False
      }
      case should_continue {
        True ->
          read_folded_lines_content(
            after_spaces,
            block_indent,
            indent,
            acc,
            state,
          )
        False -> {
          #(acc, lexer)
        }
      }
    }
  }
}

fn read_folded_lines_content(
  after_spaces: Lexer,
  block_indent: Int,
  indent: Int,
  acc: String,
  state: FoldState,
) -> #(String, Lexer) {
  let starts_with_tab = peek(after_spaces) == Some("\t")
  let is_more_indented = indent > block_indent || starts_with_tab
  let extra_indent = string.repeat(" ", indent - block_indent)
  let #(line, lexer) = read_until_eol(after_spaces, extra_indent)

  let new_acc = case acc {
    "" -> line
    _ ->
      case state, is_more_indented {
        FoldAfterBlank, True -> {
          case string.ends_with(acc, "\n\n") {
            True -> acc <> line
            False -> acc <> "\n" <> line
          }
        }
        FoldAfterBlank, False -> acc <> line
        FoldAfterMoreIndented, _ -> acc <> line
        FoldNormal, True -> acc <> "\n" <> line
        FoldNormal, False -> acc <> " " <> line
      }
  }

  case peek(lexer) {
    Some("\n") -> {
      case is_more_indented {
        True ->
          read_folded_lines(
            advance(lexer),
            block_indent,
            new_acc <> "\n",
            FoldAfterMoreIndented,
          )
        False ->
          read_folded_lines(advance(lexer), block_indent, new_acc, FoldNormal)
      }
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      case is_more_indented {
        True -> {
          case peek(lexer) {
            Some("\n") ->
              read_folded_lines(
                advance(lexer),
                block_indent,
                new_acc <> "\n",
                FoldAfterMoreIndented,
              )
            _ ->
              read_folded_lines(
                lexer,
                block_indent,
                new_acc <> "\n",
                FoldAfterMoreIndented,
              )
          }
        }
        False -> {
          case peek(lexer) {
            Some("\n") ->
              read_folded_lines(
                advance(lexer),
                block_indent,
                new_acc,
                FoldNormal,
              )
            _ -> read_folded_lines(lexer, block_indent, new_acc, FoldNormal)
          }
        }
      }
    }
    _ -> #(new_acc, lexer)
  }
}

fn read_until_eol(lexer: Lexer, prefix: String) -> #(String, Lexer) {
  read_until_eol_loop(lexer, case prefix {
    "" -> []
    _ -> [prefix]
  })
}

fn read_until_eol_loop(lexer: Lexer, acc: List(String)) -> #(String, Lexer) {
  case peek(lexer) {
    None -> #(list_to_string(acc), lexer)
    Some("\n") -> #(list_to_string(acc), lexer)
    Some("\r") -> #(list_to_string(acc), lexer)
    Some(c) -> read_until_eol_loop(advance(lexer), [c, ..acc])
  }
}

fn apply_chomping(content: String, chomping: Chomping) -> String {
  case chomping {
    Strip -> trim_trailing_newlines(content)
    Clip -> {
      let trimmed = trim_trailing_newlines(content)
      case trimmed {
        "" -> ""
        _ -> trimmed <> "\n"
      }
    }
    Keep -> content <> "\n"
  }
}

fn trim_trailing_newlines(s: String) -> String {
  case string.last(s) {
    Ok("\n") | Ok("\r") -> trim_trailing_newlines(string.drop_end(s, 1))
    _ -> s
  }
}
