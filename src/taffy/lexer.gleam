//// YAML lexer — tokenizes YAML input.
////
//// State is held on a `BitArray`. Lookahead is byte-prefix pattern matching
//// instead of `List(String)` walking; lookback into `input` is byte-indexed.
//// Multibyte UTF-8 only appears inside scalar bodies — structural matching
//// is ASCII, so the col counter advances 1 per byte on the structural paths
//// and 1 per codepoint on the scalar paths.

import gleam/bit_array
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
    input: BitArray,
    rest: BitArray,
    pos: Int,
    line: Int,
    col: Int,
    in_document: Bool,
    flow_level: Int,
    quoted_open_col: Int,
  )
}

pub fn new(input: String) -> Lexer {
  let bytes = bit_array.from_string(input)
  Lexer(
    input: bytes,
    rest: bytes,
    pos: 0,
    line: 1,
    col: 0,
    in_document: False,
    flow_level: 0,
    quoted_open_col: 0,
  )
}

// Advance the lexer past `bytes` ASCII bytes, switching `rest` to the tail
// already produced by a pattern match. Saves one bit_array.slice vs
// `advance_n`. Call sites: `consume(lexer, bytes: 1, rest: more)`.
fn consume(lexer: Lexer, bytes n: Int, rest more: BitArray) -> Lexer {
  Lexer(..lexer, pos: lexer.pos + n, col: lexer.col + n, rest: more)
}

// back_up is called after a single 1-byte ASCII advance. We slice the
// original input from pos-1 to the end to reconstruct rest.
fn back_up(lexer: Lexer) -> Lexer {
  let new_pos = lexer.pos - 1
  let total = bit_array.byte_size(lexer.input)
  let assert Ok(rest) = bit_array.slice(lexer.input, new_pos, total - new_pos)
  Lexer(..lexer, pos: new_pos, col: lexer.col - 1, rest: rest)
}

/// On error, the second element is the byte position at which the lexer
/// gave up — surfaced through `ParseError.pos` so callers can pinpoint the
/// failure rather than always reporting `pos: 0`.
///
/// On success, each token is paired with the byte offset in `input` where
/// the lexer was positioned when it began producing that token.
pub fn tokenize(input: String) -> Result(List(#(Token, Int)), #(String, Int)) {
  let lexer = new(input)
  case count_indent(lexer) {
    Error(e) -> Error(#(e, lexer.pos))
    Ok(#(0, lexer)) -> tokenize_all(lexer, [])
    Ok(#(n, lexer)) -> tokenize_all(lexer, [#(Indent(n), 0)])
  }
}

fn tokenize_all(
  lexer: Lexer,
  acc: List(#(Token, Int)),
) -> Result(List(#(Token, Int)), #(String, Int)) {
  let start_pos = lexer.pos
  case next_token(lexer) {
    Ok(#(token, new_lexer)) -> {
      case token {
        Eof -> Ok(list.reverse([#(Eof, start_pos), ..acc]))
        _ -> tokenize_all(new_lexer, [#(token, start_pos), ..acc])
      }
    }
    Error(e) -> Error(#(e, lexer.pos))
  }
}

pub fn next_token(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case lexer.rest {
    <<>> -> Ok(#(Eof, lexer))
    <<"\r\n", more:bits>> ->
      lex_after_newline(consume(lexer, bytes: 2, rest: more))
    <<"\n", more:bits>> ->
      lex_after_newline(consume(lexer, bytes: 1, rest: more))
    <<"\r", more:bits>> ->
      lex_after_newline(consume(lexer, bytes: 1, rest: more))
    <<" ", _:bits>> | <<"\t", _:bits>> -> next_token(advance_n(lexer, 1))
    <<"#", _:bits>> -> lex_comment(lexer)
    <<":", _:bits>> -> lex_colon(advance_n(lexer, 1))
    <<"?", _:bits>> -> lex_question(advance_n(lexer, 1))
    <<"-", _:bits>> -> lex_dash(lexer)
    <<".", _:bits>> -> lex_dot(lexer)
    <<"[", _:bits>> -> {
      let lexer = advance_n(lexer, 1)
      Ok(#(BracketOpen, Lexer(..lexer, flow_level: lexer.flow_level + 1)))
    }
    <<"]", _:bits>> -> {
      let lexer = advance_n(lexer, 1)
      Ok(#(BracketClose, Lexer(..lexer, flow_level: lexer.flow_level - 1)))
    }
    <<"{", _:bits>> -> {
      let lexer = advance_n(lexer, 1)
      Ok(#(BraceOpen, Lexer(..lexer, flow_level: lexer.flow_level + 1)))
    }
    <<"}", _:bits>> -> {
      let lexer = advance_n(lexer, 1)
      Ok(#(BraceClose, Lexer(..lexer, flow_level: lexer.flow_level - 1)))
    }
    <<",", _:bits>> -> Ok(#(Comma, advance_n(lexer, 1)))
    <<"&", _:bits>> -> {
      let #(name, lexer) = read_identifier(advance_n(lexer, 1))
      Ok(#(Anchor(name), lexer))
    }
    <<"*", _:bits>> -> {
      let #(name, lexer) = read_identifier(advance_n(lexer, 1))
      Ok(#(Alias(name), lexer))
    }
    <<"!", _:bits>> -> {
      let #(tag, lexer) = read_tag(advance_n(lexer, 1))
      Ok(#(Tag(tag), lexer))
    }
    <<"|", _:bits>> -> read_literal_block(advance_n(lexer, 1))
    <<">", _:bits>> -> read_folded_block(advance_n(lexer, 1))
    <<"'", _:bits>> -> read_single_quoted(advance_n(lexer, 1))
    <<"\"", _:bits>> -> read_double_quoted(advance_n(lexer, 1))
    <<"%", _:bits>> -> lex_percent(lexer)
    _ -> read_plain_scalar(lexer)
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
      case bit_array.slice(lexer.input, lexer.pos - 1, 1) {
        Ok(<<0x20>>) | Ok(<<0x09>>) | Ok(<<0x0A>>) | Ok(<<0x0D>>) -> True
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
    _ -> read_plain_scalar(back_up(lexer))
  }
}

fn lex_question(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
      Ok(#(Question, lexer))
    _ -> read_plain_scalar(back_up(lexer))
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
    _ -> read_plain_scalar(lexer)
  }
}

fn lex_dash_indicator(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
      Ok(#(Dash, lexer))
    Some(",") | Some("]") | Some("}") | Some("[") | Some("{")
      if lexer.flow_level > 0
    -> Error("Invalid use of dash indicator in flow context")
    _ -> read_plain_scalar(back_up(lexer))
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

// peek returns the next codepoint as a String, or None at EOF.
// ASCII bytes go through a small lookup; multibyte falls through to a slow path.
fn peek(lexer: Lexer) -> Option(String) {
  case lexer.rest {
    <<>> -> None
    <<b, _:bits>> if b < 0x80 -> Some(ascii_string(b))
    _ -> peek_codepoint_slow(lexer.rest)
  }
}

fn peek_codepoint_slow(rest: BitArray) -> Option(String) {
  // 2/3/4 byte UTF-8 leader handling. Only reached for non-ASCII content
  // inside scalars — not on the structural hot path.
  case rest {
    <<b1, b2, _:bits>> if b1 >= 0xC0 && b1 < 0xE0 -> {
      case bit_array.to_string(<<b1, b2>>) {
        Ok(s) -> Some(s)
        Error(_) -> None
      }
    }
    <<b1, b2, b3, _:bits>> if b1 >= 0xE0 && b1 < 0xF0 -> {
      case bit_array.to_string(<<b1, b2, b3>>) {
        Ok(s) -> Some(s)
        Error(_) -> None
      }
    }
    <<b1, b2, b3, b4, _:bits>> if b1 >= 0xF0 -> {
      case bit_array.to_string(<<b1, b2, b3, b4>>) {
        Ok(s) -> Some(s)
        Error(_) -> None
      }
    }
    _ -> None
  }
}

// codepoint_byte_len returns 1..4 for the leading byte of a UTF-8 codepoint.
fn codepoint_byte_len(b: Int) -> Int {
  case b {
    _ if b < 0x80 -> 1
    _ if b < 0xE0 -> 2
    _ if b < 0xF0 -> 3
    _ -> 4
  }
}

fn peek_n(lexer: Lexer, n: Int) -> String {
  // Currently only called with n=3 against ASCII markers ("---", "..."). The
  // implementation supports ASCII spans only — sufficient for the two callers.
  case n, lexer.rest {
    3, <<a, b, c, _:bits>> if a < 0x80 && b < 0x80 && c < 0x80 ->
      ascii_string(a) <> ascii_string(b) <> ascii_string(c)
    3, <<a, b>> if a < 0x80 && b < 0x80 -> ascii_string(a) <> ascii_string(b)
    3, <<a>> if a < 0x80 -> ascii_string(a)
    3, <<>> -> ""
    _, _ -> peek_n_slow(lexer, n)
  }
}

fn peek_n_slow(lexer: Lexer, n: Int) -> String {
  peek_n_loop(lexer.rest, n, [])
}

fn peek_n_loop(rest: BitArray, n: Int, acc: List(String)) -> String {
  case n, rest {
    0, _ -> list.reverse(acc) |> string.join("")
    _, <<>> -> list.reverse(acc) |> string.join("")
    _, <<b, more:bits>> if b < 0x80 ->
      peek_n_loop(more, n - 1, [ascii_string(b), ..acc])
    _, _ ->
      case peek_codepoint_slow(rest) {
        None -> list.reverse(acc) |> string.join("")
        Some(s) -> {
          let len = string.byte_size(s)
          let total = bit_array.byte_size(rest)
          let assert Ok(more) = bit_array.slice(rest, len, total - len)
          peek_n_loop(more, n - 1, [s, ..acc])
        }
      }
  }
}

// advance consumes one codepoint (1-4 bytes), bumping col by 1.
fn advance(lexer: Lexer) -> Lexer {
  case lexer.rest {
    <<>> -> lexer
    <<b, more:bits>> if b < 0x80 -> consume(lexer, bytes: 1, rest: more)
    <<b, _:bits>> -> {
      let n = codepoint_byte_len(b)
      let total = bit_array.byte_size(lexer.rest)
      let assert Ok(more) = bit_array.slice(lexer.rest, n, total - n)
      Lexer(..lexer, pos: lexer.pos + n, col: lexer.col + 1, rest: more)
    }
    _ -> lexer
  }
}

// advance_n consumes n bytes assuming all are single-byte ASCII codepoints.
// All callers feed ASCII-only spans (structural markers, indentation).
fn advance_n(lexer: Lexer, n: Int) -> Lexer {
  let total = bit_array.byte_size(lexer.rest)
  let take = case n > total {
    True -> total
    False -> n
  }
  let assert Ok(more) = bit_array.slice(lexer.rest, take, total - take)
  Lexer(..lexer, pos: lexer.pos + take, col: lexer.col + take, rest: more)
}

// ascii_string converts a single ASCII byte to a one-char String.
// The common chars used in pattern matching are explicit so the BEAM can
// share the literal atoms; less common ones fall through to bit_array.to_string.
fn ascii_string(b: Int) -> String {
  case b {
    0x20 -> " "
    0x09 -> "\t"
    0x0A -> "\n"
    0x0D -> "\r"
    0x21 -> "!"
    0x22 -> "\""
    0x23 -> "#"
    0x25 -> "%"
    0x26 -> "&"
    0x27 -> "'"
    0x28 -> "("
    0x29 -> ")"
    0x2A -> "*"
    0x2B -> "+"
    0x2C -> ","
    0x2D -> "-"
    0x2E -> "."
    0x2F -> "/"
    0x30 -> "0"
    0x31 -> "1"
    0x32 -> "2"
    0x33 -> "3"
    0x34 -> "4"
    0x35 -> "5"
    0x36 -> "6"
    0x37 -> "7"
    0x38 -> "8"
    0x39 -> "9"
    0x3A -> ":"
    0x3B -> ";"
    0x3C -> "<"
    0x3D -> "="
    0x3E -> ">"
    0x3F -> "?"
    0x40 -> "@"
    0x5B -> "["
    0x5C -> "\\"
    0x5D -> "]"
    0x5E -> "^"
    0x5F -> "_"
    0x60 -> "`"
    0x7B -> "{"
    0x7C -> "|"
    0x7D -> "}"
    0x7E -> "~"
    _ -> {
      let assert Ok(s) = bit_array.to_string(<<b>>)
      s
    }
  }
}

fn count_indent(lexer: Lexer) -> Result(#(Int, Lexer), String) {
  count_indent_loop(lexer, 0)
}

fn count_indent_loop(lexer: Lexer, count: Int) -> Result(#(Int, Lexer), String) {
  case lexer.rest {
    <<" ", more:bits>> ->
      count_indent_loop(consume(lexer, bytes: 1, rest: more), count + 1)
    <<"\t", _:bits>> ->
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
  case lexer.rest {
    <<"\t", more:bits>> | <<" ", more:bits>> ->
      skip_tabs_lexer(consume(lexer, bytes: 1, rest: more))
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

// span_string slices [start, lexer.pos) from the original input and decodes
// it as UTF-8. All callers establish the span only on byte boundaries that
// follow whole codepoints, so to_string never fails.
fn span_string(lexer: Lexer, start: Int) -> String {
  let len = lexer.pos - start
  let assert Ok(slice) = bit_array.slice(lexer.input, start, len)
  let assert Ok(s) = bit_array.to_string(slice)
  s
}

fn read_until_newline(lexer: Lexer) -> #(String, Lexer) {
  let start = lexer.pos
  let lexer = scan_until_newline(lexer)
  #(span_string(lexer, start), lexer)
}

fn scan_until_newline(lexer: Lexer) -> Lexer {
  case lexer.rest {
    <<>> -> lexer
    <<"\n", _:bits>> -> lexer
    <<"\r", _:bits>> -> lexer
    <<b, more:bits>> if b < 0x80 ->
      scan_until_newline(consume(lexer, bytes: 1, rest: more))
    _ -> scan_until_newline(advance(lexer))
  }
}

fn read_identifier(lexer: Lexer) -> #(String, Lexer) {
  let start = lexer.pos
  let lexer = scan_identifier(lexer)
  #(span_string(lexer, start), lexer)
}

fn scan_identifier(lexer: Lexer) -> Lexer {
  case lexer.rest {
    <<>>
    | <<" ", _:bits>>
    | <<"\t", _:bits>>
    | <<"\n", _:bits>>
    | <<"\r", _:bits>>
    | <<",", _:bits>>
    | <<"[", _:bits>>
    | <<"]", _:bits>>
    | <<"{", _:bits>>
    | <<"}", _:bits>> -> lexer
    <<b, more:bits>> if b < 0x80 ->
      scan_identifier(consume(lexer, bytes: 1, rest: more))
    _ -> scan_identifier(advance(lexer))
  }
}

fn read_tag(lexer: Lexer) -> #(String, Lexer) {
  case lexer.rest {
    <<"<", _:bits>> -> {
      // span starts before the consumed `!` (pos-1), captures "!<...>"
      let start = lexer.pos - 1
      let lexer = advance_n(lexer, 1)
      let lexer = scan_verbatim_tag(lexer)
      // include the closing '>'
      let lexer = case lexer.rest {
        <<">", _:bits>> -> advance_n(lexer, 1)
        _ -> lexer
      }
      #(span_string(lexer, start), lexer)
    }
    _ -> {
      let start = lexer.pos - 1
      let lexer = scan_tag(lexer)
      #(span_string(lexer, start), lexer)
    }
  }
}

fn scan_verbatim_tag(lexer: Lexer) -> Lexer {
  case lexer.rest {
    <<>> | <<">", _:bits>> -> lexer
    <<b, more:bits>> if b < 0x80 ->
      scan_verbatim_tag(consume(lexer, bytes: 1, rest: more))
    _ -> scan_verbatim_tag(advance(lexer))
  }
}

fn scan_tag(lexer: Lexer) -> Lexer {
  case lexer.rest {
    <<>>
    | <<" ", _:bits>>
    | <<"\t", _:bits>>
    | <<"\n", _:bits>>
    | <<"\r", _:bits>>
    | <<",", _:bits>>
    | <<"[", _:bits>>
    | <<"]", _:bits>>
    | <<"{", _:bits>>
    | <<"}", _:bits>> -> lexer
    <<b, more:bits>> if b < 0x80 ->
      scan_tag(consume(lexer, bytes: 1, rest: more))
    _ -> scan_tag(advance(lexer))
  }
}

fn read_single_quoted(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  read_single_quoted_loop(lexer, lexer.pos, [], False)
}

// `start` marks the start of the current uninterrupted plain run. When we
// hit a structural byte (`'`, `\n`, `\r`) we flush the run into `acc` as a
// single span string, then resume.
fn read_single_quoted_loop(
  lexer: Lexer,
  start: Int,
  acc: List(String),
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  case lexer.rest {
    <<>> -> Error("Unterminated single-quoted string")
    <<"'", more:bits>> -> {
      let acc = flush_span(lexer, start, acc)
      let lexer = consume(lexer, bytes: 1, rest: more)
      case lexer.rest {
        <<"'", more2:bits>> -> {
          // doubled quote — escaped single quote
          let lexer = consume(lexer, bytes: 1, rest: more2)
          read_single_quoted_loop(lexer, lexer.pos, ["'", ..acc], multiline)
        }
        _ -> {
          let s = list.reverse(acc) |> string.join("")
          case multiline {
            True -> check_multiline_implicit_key(lexer, SingleQuoted(s))
            False -> Ok(#(SingleQuoted(s), lexer))
          }
        }
      }
    }
    <<"\r\n", more:bits>> ->
      single_quoted_after_break(
        consume(lexer, bytes: 2, rest: more),
        flush_span(lexer, start, acc),
      )
    <<"\n", more:bits>> ->
      single_quoted_after_break(
        consume(lexer, bytes: 1, rest: more),
        flush_span(lexer, start, acc),
      )
    <<"\r", more:bits>> ->
      single_quoted_after_break(
        consume(lexer, bytes: 1, rest: more),
        flush_span(lexer, start, acc),
      )
    <<b, more:bits>> if b < 0x80 ->
      read_single_quoted_loop(
        consume(lexer, bytes: 1, rest: more),
        start,
        acc,
        multiline,
      )
    _ -> read_single_quoted_loop(advance(lexer), start, acc, multiline)
  }
}

fn single_quoted_after_break(
  lexer: Lexer,
  acc: List(String),
) -> Result(#(Token, Lexer), String) {
  case check_document_marker(lexer) {
    True -> Error("Unterminated single-quoted string")
    False -> {
      let lexer = skip_quoted_continuation_whitespace(lexer)
      // A second consecutive line break (any flavour) is preserved as a
      // literal "\n"; otherwise the break folds to a single space.
      case lexer.rest {
        <<"\n", _:bits>> | <<"\r", _:bits>> ->
          read_single_quoted_loop(lexer, lexer.pos, ["\n", ..acc], True)
        _ -> read_single_quoted_loop(lexer, lexer.pos, [" ", ..acc], True)
      }
    }
  }
}

fn flush_span(lexer: Lexer, start: Int, acc: List(String)) -> List(String) {
  case lexer.pos > start {
    True -> [span_string(lexer, start), ..acc]
    False -> acc
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
  case lexer.rest {
    <<" ", more:bits>> ->
      skip_inline_spaces(consume(lexer, bytes: 1, rest: more))
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
  case lexer.rest {
    <<" ", more:bits>> | <<"\t", more:bits>> ->
      skip_quoted_continuation_whitespace(consume(lexer, bytes: 1, rest: more))
    _ -> lexer
  }
}

fn count_continuation_indent(lexer: Lexer) -> #(Int, Lexer) {
  count_continuation_indent_loop(lexer, 0)
}

fn count_continuation_indent_loop(lexer: Lexer, count: Int) -> #(Int, Lexer) {
  case lexer.rest {
    <<" ", more:bits>> | <<"\t", more:bits>> ->
      count_continuation_indent_loop(
        consume(lexer, bytes: 1, rest: more),
        count + 1,
      )
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
  read_double_quoted_loop(lexer, [], [], False)
}

fn is_after_mapping_colon(lexer: Lexer) -> Bool {
  case lexer.pos >= 3 {
    True -> {
      case bit_array.slice(lexer.input, lexer.pos - 3, 2) {
        Ok(<<0x3A, 0x20>>) -> True
        _ -> False
      }
    }
    False -> False
  }
}

// pending_ws holds trailing whitespace bytes (space/tab) accumulated since
// the last non-ws character. It is emitted before further plain content or
// before the closing quote, but dropped if a newline arrives (folding rule).
fn read_double_quoted_loop(
  lexer: Lexer,
  acc: List(String),
  pending_ws: List(String),
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  case lexer.rest {
    <<>> -> Error("Unterminated double-quoted string")
    <<"\"", more:bits>> -> {
      let s =
        list.reverse(list.append(pending_ws, acc))
        |> string.join("")
      let lexer = consume(lexer, bytes: 1, rest: more)
      case multiline {
        True -> check_multiline_implicit_key(lexer, DoubleQuoted(s))
        False -> Ok(#(DoubleQuoted(s), lexer))
      }
    }
    <<"\\", more:bits>> -> {
      let full_acc = list.append(pending_ws, acc)
      let lexer = consume(lexer, bytes: 1, rest: more)
      read_double_quoted_escape(lexer, full_acc, multiline)
    }
    <<"\r\n", more:bits>> ->
      double_quoted_after_break(consume(lexer, bytes: 2, rest: more), acc)
    <<"\n", more:bits>> ->
      double_quoted_after_break(consume(lexer, bytes: 1, rest: more), acc)
    <<"\r", more:bits>> ->
      double_quoted_after_break(consume(lexer, bytes: 1, rest: more), acc)
    <<" ", more:bits>> ->
      read_double_quoted_loop(
        consume(lexer, bytes: 1, rest: more),
        acc,
        [" ", ..pending_ws],
        multiline,
      )
    <<"\t", more:bits>> ->
      read_double_quoted_loop(
        consume(lexer, bytes: 1, rest: more),
        acc,
        ["\t", ..pending_ws],
        multiline,
      )
    <<b, more:bits>> if b < 0x80 -> {
      let new_acc = [ascii_string(b), ..list.append(pending_ws, acc)]
      read_double_quoted_loop(
        consume(lexer, bytes: 1, rest: more),
        new_acc,
        [],
        multiline,
      )
    }
    _ ->
      // multibyte codepoint
      case peek(lexer) {
        None -> Error("Unterminated double-quoted string")
        Some(c) -> {
          let new_acc = [c, ..list.append(pending_ws, acc)]
          read_double_quoted_loop(advance(lexer), new_acc, [], multiline)
        }
      }
  }
}

fn double_quoted_after_break(
  lexer: Lexer,
  acc: List(String),
) -> Result(#(Token, Lexer), String) {
  case check_document_marker(lexer) {
    True -> Error("Unterminated double-quoted string")
    False -> {
      let #(indent, lexer) = count_continuation_indent(lexer)
      case lexer.flow_level == 0 && indent == 0 && lexer.quoted_open_col > 0 {
        True ->
          case lexer.rest {
            <<>> | <<"\n", _:bits>> | <<"\r", _:bits>> | <<"\"", _:bits>> ->
              handle_double_quoted_fold(lexer, acc, [])
            _ ->
              Error(
                "Multiline double-quoted scalar continuation must be indented",
              )
          }
        False -> handle_double_quoted_fold(lexer, acc, [])
      }
    }
  }
}

fn read_double_quoted_escape(
  lexer: Lexer,
  full_acc: List(String),
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  case lexer.rest {
    <<>> -> Error("Unterminated escape sequence")
    <<"n", more:bits>> -> dq_continue(lexer, more, "\n", full_acc, multiline)
    <<"t", more:bits>> -> dq_continue(lexer, more, "\t", full_acc, multiline)
    <<"r", more:bits>> -> dq_continue(lexer, more, "\r", full_acc, multiline)
    <<"\\", more:bits>> -> dq_continue(lexer, more, "\\", full_acc, multiline)
    <<"\"", more:bits>> -> dq_continue(lexer, more, "\"", full_acc, multiline)
    <<"/", more:bits>> -> dq_continue(lexer, more, "/", full_acc, multiline)
    <<"0", more:bits>> ->
      dq_continue(lexer, more, "\u{0000}", full_acc, multiline)
    <<"a", more:bits>> ->
      dq_continue(lexer, more, "\u{0007}", full_acc, multiline)
    <<"b", more:bits>> ->
      dq_continue(lexer, more, "\u{0008}", full_acc, multiline)
    <<"e", more:bits>> ->
      dq_continue(lexer, more, "\u{001B}", full_acc, multiline)
    <<"f", more:bits>> ->
      dq_continue(lexer, more, "\u{000C}", full_acc, multiline)
    <<"v", more:bits>> ->
      dq_continue(lexer, more, "\u{000B}", full_acc, multiline)
    <<"N", more:bits>> ->
      dq_continue(lexer, more, "\u{0085}", full_acc, multiline)
    <<"_", more:bits>> ->
      dq_continue(lexer, more, "\u{00A0}", full_acc, multiline)
    <<"L", more:bits>> ->
      dq_continue(lexer, more, "\u{2028}", full_acc, multiline)
    <<"P", more:bits>> ->
      dq_continue(lexer, more, "\u{2029}", full_acc, multiline)
    <<" ", more:bits>> -> dq_continue(lexer, more, " ", full_acc, multiline)
    <<"x", more:bits>> ->
      read_hex_escape(
        consume(lexer, bytes: 1, rest: more),
        full_acc,
        2,
        multiline,
      )
    <<"u", more:bits>> ->
      read_hex_escape(
        consume(lexer, bytes: 1, rest: more),
        full_acc,
        4,
        multiline,
      )
    <<"U", more:bits>> ->
      read_hex_escape(
        consume(lexer, bytes: 1, rest: more),
        full_acc,
        8,
        multiline,
      )
    <<"\r\n", more:bits>> ->
      dq_escape_break(consume(lexer, bytes: 2, rest: more), full_acc)
    <<"\n", more:bits>> ->
      dq_escape_break(consume(lexer, bytes: 1, rest: more), full_acc)
    <<"\r", more:bits>> ->
      dq_escape_break(consume(lexer, bytes: 1, rest: more), full_acc)
    _ ->
      case peek(lexer) {
        Some(c) -> Error("Invalid escape sequence: \\" <> c)
        None -> Error("Unterminated escape sequence")
      }
  }
}

fn dq_escape_break(
  lexer: Lexer,
  full_acc: List(String),
) -> Result(#(Token, Lexer), String) {
  case check_document_marker(lexer) {
    True -> Error("Unterminated double-quoted string")
    False -> skip_line_continuation(lexer, full_acc)
  }
}

fn dq_continue(
  lexer: Lexer,
  more: BitArray,
  emit: String,
  full_acc: List(String),
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  read_double_quoted_loop(
    consume(lexer, bytes: 1, rest: more),
    [emit, ..full_acc],
    [],
    multiline,
  )
}

fn skip_line_continuation(
  lexer: Lexer,
  acc: List(String),
) -> Result(#(Token, Lexer), String) {
  case lexer.rest {
    <<" ", more:bits>> | <<"\t", more:bits>> ->
      skip_line_continuation(consume(lexer, bytes: 1, rest: more), acc)
    _ -> read_double_quoted_loop(lexer, acc, [], True)
  }
}

// `newlines` is a list of "\n" tokens (one per consumed line break) — used to
// fold runs of blank lines. Storing as a list avoids per-line `<>` concat.
fn handle_double_quoted_fold(
  lexer: Lexer,
  acc: List(String),
  newlines: List(String),
) -> Result(#(Token, Lexer), String) {
  case lexer.rest {
    <<"\r\n", more:bits>> ->
      dq_fold_step(consume(lexer, bytes: 2, rest: more), acc, newlines)
    <<"\n", more:bits>> ->
      dq_fold_step(consume(lexer, bytes: 1, rest: more), acc, newlines)
    <<"\r", more:bits>> ->
      dq_fold_step(consume(lexer, bytes: 1, rest: more), acc, newlines)
    _ -> {
      // We entered after the caller consumed exactly one `\n`/`\r` line
      // break. `newlines` is the count of additional breaks observed inside
      // this loop. Total breaks = 1 + len(newlines). One break folds to a
      // single space; N>1 breaks emit (N-1) literal "\n"s.
      case newlines {
        [] -> read_double_quoted_loop(lexer, [" ", ..acc], [], True)
        _ ->
          read_double_quoted_loop(lexer, list.append(newlines, acc), [], True)
      }
    }
  }
}

fn dq_fold_step(
  lexer: Lexer,
  acc: List(String),
  newlines: List(String),
) -> Result(#(Token, Lexer), String) {
  case check_document_marker(lexer) {
    True -> Error("Unterminated double-quoted string")
    False -> {
      let #(_indent, lexer) = count_continuation_indent(lexer)
      handle_double_quoted_fold(lexer, acc, ["\n", ..newlines])
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
            [],
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
    0, _ -> Ok(#(finalize(acc), lexer))
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
  scan_plain_scalar(lexer, lexer.pos)
}

fn scan_plain_scalar(
  lexer: Lexer,
  start: Int,
) -> Result(#(Token, Lexer), String) {
  case lexer.rest {
    <<>>
    | <<"\n", _:bits>>
    | <<"\r", _:bits>>
    | <<",", _:bits>>
    | <<"]", _:bits>>
    | <<"}", _:bits>> ->
      Ok(#(Plain(string.trim_end(span_string(lexer, start))), lexer))
    <<"#", more:bits>> -> {
      // `#` only terminates if preceded (within the scalar span) by space/tab
      let terminated = case lexer.pos > start {
        True ->
          case bit_array.slice(lexer.input, lexer.pos - 1, 1) {
            Ok(<<0x20>>) | Ok(<<0x09>>) -> True
            _ -> False
          }
        False -> False
      }
      case terminated {
        True -> Ok(#(Plain(string.trim_end(span_string(lexer, start))), lexer))
        False -> scan_plain_scalar(consume(lexer, bytes: 1, rest: more), start)
      }
    }
    <<":", more:bits>> -> {
      let terminator = case more {
        <<>>
        | <<" ", _:bits>>
        | <<"\t", _:bits>>
        | <<"\n", _:bits>>
        | <<"\r", _:bits>> -> True
        _ -> False
      }
      case terminator {
        True -> Ok(#(Plain(string.trim_end(span_string(lexer, start))), lexer))
        False -> scan_plain_scalar(consume(lexer, bytes: 1, rest: more), start)
      }
    }
    <<b, more:bits>> if b < 0x80 ->
      scan_plain_scalar(consume(lexer, bytes: 1, rest: more), start)
    _ -> scan_plain_scalar(advance(lexer), start)
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
        False -> Error("Invalid block scalar header")
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
  case lexer.rest {
    <<>> -> lexer
    <<"\n", _:bits>> -> lexer
    <<"\r", _:bits>> -> lexer
    <<b, more:bits>> if b < 0x80 ->
      skip_to_eol(consume(lexer, bytes: 1, rest: more))
    _ -> skip_to_eol(advance(lexer))
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
          let prefix = repeat_string("\n", leading_newlines, [])
          let #(content, lexer) =
            read_literal_lines_ex(lexer, boundary_indent, base_indent, prefix)
          let content = apply_chomping(finalize(content), chomping)
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
  let prefix = repeat_string("\n", leading_newlines, [])
  let initial_state = case leading_newlines > 0 {
    True -> FoldAfterBlank
    False -> FoldNormal
  }

  case explicit_indent {
    Some(indent) -> {
      let #(content, lexer) =
        read_folded_lines(lexer, indent, prefix, initial_state)
      let content = apply_chomping(finalize(content), chomping)
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
              let content = apply_chomping(finalize(content), chomping)
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
  case lexer.rest {
    <<" ", more:bits>> ->
      count_leading_spaces(consume(lexer, bytes: 1, rest: more), count + 1)
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

// `acc` holds output segments in reverse order (most recent first). All
// runtime concat sites become cons. `string.ends_with(acc_str, "\n")` becomes
// `case acc { ["\n", ..] -> True }` because every "\n" is consed as its own
// single-char element (extra_indent is space-only, line never contains \n).
fn read_literal_lines_ex(
  lexer: Lexer,
  boundary_indent: Int,
  base_indent: Int,
  acc: List(String),
) -> #(List(String), Lexer) {
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)
  let extra_indent = case indent > base_indent {
    True -> string.repeat(" ", indent - base_indent)
    False -> ""
  }
  let #(line, lexer) = read_until_eol(after_spaces, extra_indent)
  let new_acc = case acc {
    [] -> [line]
    ["\n", ..] -> [line, ..acc]
    _ -> [line, "\n", ..acc]
  }

  case lexer.rest {
    <<"\r\n", more:bits>> ->
      check_literal_continuation_ex(
        consume(lexer, bytes: 2, rest: more),
        boundary_indent,
        base_indent,
        new_acc,
      )
    <<"\n", more:bits>> ->
      check_literal_continuation_ex(
        consume(lexer, bytes: 1, rest: more),
        boundary_indent,
        base_indent,
        new_acc,
      )
    <<"\r", more:bits>> ->
      check_literal_continuation_ex(
        consume(lexer, bytes: 1, rest: more),
        boundary_indent,
        base_indent,
        new_acc,
      )
    _ -> #(new_acc, lexer)
  }
}

fn check_literal_continuation_ex(
  lexer: Lexer,
  boundary_indent: Int,
  base_indent: Int,
  acc: List(String),
) -> #(List(String), Lexer) {
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)
  case after_spaces.rest {
    <<"\r\n", more:bits>> ->
      continue_blank_literal(
        consume(after_spaces, bytes: 2, rest: more),
        indent,
        boundary_indent,
        base_indent,
        acc,
      )
    <<"\n", more:bits>> ->
      continue_blank_literal(
        consume(after_spaces, bytes: 1, rest: more),
        indent,
        boundary_indent,
        base_indent,
        acc,
      )
    <<"\r", more:bits>> ->
      continue_blank_literal(
        consume(after_spaces, bytes: 1, rest: more),
        indent,
        boundary_indent,
        base_indent,
        acc,
      )
    <<>> -> #(acc, lexer)
    _ ->
      case indent >= boundary_indent {
        True ->
          case indent == 0 && is_doc_marker(after_spaces) {
            True -> #(acc, back_up(lexer))
            False -> {
              let extra_indent = case indent > base_indent {
                True -> string.repeat(" ", indent - base_indent)
                False -> ""
              }
              let #(line, lexer) = read_until_eol(after_spaces, extra_indent)
              let new_acc = [line, "\n", ..acc]
              case lexer.rest {
                <<"\r\n", more:bits>> ->
                  check_literal_continuation_ex(
                    consume(lexer, bytes: 2, rest: more),
                    boundary_indent,
                    base_indent,
                    new_acc,
                  )
                <<"\n", more:bits>> ->
                  check_literal_continuation_ex(
                    consume(lexer, bytes: 1, rest: more),
                    boundary_indent,
                    base_indent,
                    new_acc,
                  )
                <<"\r", more:bits>> ->
                  check_literal_continuation_ex(
                    consume(lexer, bytes: 1, rest: more),
                    boundary_indent,
                    base_indent,
                    new_acc,
                  )
                _ -> #(new_acc, lexer)
              }
            }
          }
        False -> #(acc, back_up(lexer))
      }
  }
}

fn continue_blank_literal(
  lexer: Lexer,
  indent: Int,
  boundary_indent: Int,
  base_indent: Int,
  acc: List(String),
) -> #(List(String), Lexer) {
  let extra_content = case indent >= boundary_indent && indent > base_indent {
    True -> string.repeat(" ", indent - base_indent)
    False -> ""
  }
  let acc = cons_extra("\n", extra_content, acc)
  check_literal_continuation_ex(lexer, boundary_indent, base_indent, acc)
}

// cons newline + optional extra content (spaces) onto the reversed acc.
fn cons_extra(nl: String, extra: String, acc: List(String)) -> List(String) {
  case extra {
    "" -> [nl, ..acc]
    _ -> [extra, nl, ..acc]
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
  acc: List(String),
  state: FoldState,
) -> #(List(String), Lexer) {
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)

  case after_spaces.rest {
    <<"\r\n", more:bits>> ->
      read_folded_lines(
        consume(after_spaces, bytes: 2, rest: more),
        block_indent,
        ["\n", ..acc],
        FoldAfterBlank,
      )
    <<"\n", more:bits>> ->
      read_folded_lines(
        consume(after_spaces, bytes: 1, rest: more),
        block_indent,
        ["\n", ..acc],
        FoldAfterBlank,
      )
    <<"\r", more:bits>> ->
      read_folded_lines(
        consume(after_spaces, bytes: 1, rest: more),
        block_indent,
        ["\n", ..acc],
        FoldAfterBlank,
      )
    <<>> -> #(acc, lexer)
    _ -> {
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
  acc: List(String),
  state: FoldState,
) -> #(List(String), Lexer) {
  let starts_with_tab = case after_spaces.rest {
    <<"\t", _:bits>> -> True
    _ -> False
  }
  let is_more_indented = indent > block_indent || starts_with_tab
  let extra_indent = string.repeat(" ", indent - block_indent)
  let #(line, lexer) = read_until_eol(after_spaces, extra_indent)

  // Decide separator before the new line. acc is in reverse order, so the
  // most recently appended segments are at the head — `["\n", "\n", ..]`
  // means acc previously ended with "\n\n".
  let new_acc = case acc {
    [] -> [line]
    _ ->
      case state, is_more_indented {
        FoldAfterBlank, True ->
          case acc {
            ["\n", "\n", ..] -> [line, ..acc]
            _ -> [line, "\n", ..acc]
          }
        FoldAfterBlank, False -> [line, ..acc]
        FoldAfterMoreIndented, _ -> [line, ..acc]
        FoldNormal, True -> [line, "\n", ..acc]
        FoldNormal, False -> [line, " ", ..acc]
      }
  }

  case lexer.rest {
    <<"\r\n", more:bits>> ->
      continue_folded(
        consume(lexer, bytes: 2, rest: more),
        block_indent,
        new_acc,
        is_more_indented,
      )
    <<"\n", more:bits>> ->
      continue_folded(
        consume(lexer, bytes: 1, rest: more),
        block_indent,
        new_acc,
        is_more_indented,
      )
    <<"\r", more:bits>> ->
      continue_folded(
        consume(lexer, bytes: 1, rest: more),
        block_indent,
        new_acc,
        is_more_indented,
      )
    _ -> #(new_acc, lexer)
  }
}

fn continue_folded(
  lexer: Lexer,
  block_indent: Int,
  new_acc: List(String),
  is_more_indented: Bool,
) -> #(List(String), Lexer) {
  case is_more_indented {
    True ->
      read_folded_lines(
        lexer,
        block_indent,
        ["\n", ..new_acc],
        FoldAfterMoreIndented,
      )
    False -> read_folded_lines(lexer, block_indent, new_acc, FoldNormal)
  }
}

fn read_until_eol(lexer: Lexer, prefix: String) -> #(String, Lexer) {
  let start = lexer.pos
  let lexer = scan_until_newline(lexer)
  let body = span_string(lexer, start)
  let result = case prefix {
    "" -> body
    _ -> prefix <> body
  }
  #(result, lexer)
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

// Builds [s, s, ..., s] of length `n`. Used to seed block-scalar accumulators
// with one cons cell per leading newline (instead of a single n-char string)
// so that head-cons checks like `["\n", ..]` work correctly.
fn repeat_string(s: String, n: Int, acc: List(String)) -> List(String) {
  case n {
    0 -> acc
    _ -> repeat_string(s, n - 1, [s, ..acc])
  }
}

fn finalize(acc: List(String)) -> String {
  list.reverse(acc) |> string.join("")
}
