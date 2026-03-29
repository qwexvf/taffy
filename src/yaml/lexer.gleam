//// YAML lexer - tokenizes YAML input.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Token types for YAML lexer.
pub type Token {
  /// Start of document ---
  DocStart
  /// End of document ...
  DocEnd
  /// Mapping key indicator :
  Colon
  /// Explicit key indicator ?
  Question
  /// Sequence entry indicator -
  Dash
  /// Flow sequence start [
  BracketOpen
  /// Flow sequence end ]
  BracketClose
  /// Flow mapping start {
  BraceOpen
  /// Flow mapping end }
  BraceClose
  /// Item separator ,
  Comma
  /// Anchor &name
  Anchor(String)
  /// Alias *name
  Alias(String)
  /// Tag !tag
  Tag(String)
  /// Literal block scalar | with content
  Literal(String)
  /// Folded block scalar > with content
  Folded(String)
  /// Plain scalar (unquoted string)
  Plain(String)
  /// Single-quoted string
  SingleQuoted(String)
  /// Double-quoted string
  DoubleQuoted(String)
  /// Comment
  Comment(String)
  /// Newline (tracks for indentation)
  Newline
  /// Indentation (number of spaces)
  Indent(Int)
  /// End of file
  Eof
}

/// Lexer state.
pub type Lexer {
  Lexer(input: String, pos: Int, line: Int, col: Int)
}

/// Creates a new lexer.
pub fn new(input: String) -> Lexer {
  Lexer(input: input, pos: 0, line: 1, col: 0)
}

/// Tokenizes the entire input.
pub fn tokenize(input: String) -> Result(List(Token), String) {
  let lexer = new(input)
  tokenize_all(lexer, [])
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

/// Gets the next token.
pub fn next_token(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Ok(#(Eof, lexer))
    Some(c) -> {
      case c {
        // Newline
        "\n" -> {
          let lexer = advance(lexer)
          let #(indent, lexer) = count_indent(lexer)
          case indent {
            0 -> Ok(#(Newline, lexer))
            n -> Ok(#(Indent(n), lexer))
          }
        }
        "\r" -> {
          let lexer = advance(lexer)
          // Handle \r\n
          case peek(lexer) {
            Some("\n") -> {
              let lexer = advance(lexer)
              let #(indent, lexer) = count_indent(lexer)
              case indent {
                0 -> Ok(#(Newline, lexer))
                n -> Ok(#(Indent(n), lexer))
              }
            }
            _ -> {
              let #(indent, lexer) = count_indent(lexer)
              case indent {
                0 -> Ok(#(Newline, lexer))
                n -> Ok(#(Indent(n), lexer))
              }
            }
          }
        }
        // Skip spaces (not at line start)
        " " -> next_token(advance(lexer))
        "\t" -> next_token(advance(lexer))
        // Comment
        "#" -> {
          let #(text, lexer) = read_until_newline(advance(lexer))
          Ok(#(Comment(text), lexer))
        }
        // Colon (check for mapping)
        ":" -> {
          let lexer = advance(lexer)
          case peek(lexer) {
            // Always a mapping indicator if followed by whitespace or end
            Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
              Ok(#(Colon, lexer))
            // In flow context, colon before flow indicators is also a separator
            Some(",") | Some("]") | Some("}") -> Ok(#(Colon, lexer))
            // After a quoted string, colon is typically a separator even if adjacent
            Some("\"") | Some("'") | Some("[") | Some("{") ->
              Ok(#(Colon, lexer))
            _ -> {
              // Part of a plain scalar
              read_plain_scalar(Lexer(..lexer, pos: lexer.pos - 1))
            }
          }
        }
        // Question mark (explicit key indicator)
        "?" -> {
          let lexer = advance(lexer)
          case peek(lexer) {
            Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
              Ok(#(Question, lexer))
            _ -> {
              // Part of a plain scalar
              read_plain_scalar(Lexer(..lexer, pos: lexer.pos - 1))
            }
          }
        }
        // Dash (check for sequence or document start)
        "-" -> {
          case peek_n(lexer, 3) {
            "---" -> {
              // Only treat as document start if followed by whitespace or end
              let after_dashes = advance_n(lexer, 3)
              case peek(after_dashes) {
                Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
                  Ok(#(DocStart, after_dashes))
                _ ->
                  // Part of a plain scalar (like ---word)
                  read_plain_scalar(lexer)
              }
            }
            _ -> {
              let lexer = advance(lexer)
              case peek(lexer) {
                Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
                  Ok(#(Dash, lexer))
                _ -> {
                  // Part of a plain scalar (like negative number)
                  read_plain_scalar(Lexer(..lexer, pos: lexer.pos - 1))
                }
              }
            }
          }
        }
        // Document end
        "." -> {
          case peek_n(lexer, 3) {
            "..." -> {
              // Only treat as document end if followed by whitespace or end
              let after_dots = advance_n(lexer, 3)
              case peek(after_dots) {
                Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
                  Ok(#(DocEnd, after_dots))
                _ ->
                  // Part of a plain scalar (like ...word)
                  read_plain_scalar(lexer)
              }
            }
            _ -> read_plain_scalar(lexer)
          }
        }
        // Flow indicators
        "[" -> Ok(#(BracketOpen, advance(lexer)))
        "]" -> Ok(#(BracketClose, advance(lexer)))
        "{" -> Ok(#(BraceOpen, advance(lexer)))
        "}" -> Ok(#(BraceClose, advance(lexer)))
        "," -> Ok(#(Comma, advance(lexer)))
        // Anchor
        "&" -> {
          let #(name, lexer) = read_identifier(advance(lexer))
          Ok(#(Anchor(name), lexer))
        }
        // Alias
        "*" -> {
          let #(name, lexer) = read_identifier(advance(lexer))
          Ok(#(Alias(name), lexer))
        }
        // Tag
        "!" -> {
          let #(tag, lexer) = read_tag(advance(lexer))
          Ok(#(Tag(tag), lexer))
        }
        // Block scalars
        "|" -> read_literal_block(advance(lexer))
        ">" -> read_folded_block(advance(lexer))
        // Quoted strings
        "'" -> read_single_quoted(advance(lexer))
        "\"" -> read_double_quoted(advance(lexer))
        // Directive (only at start of line AND followed by letter) - skip entire line
        // Directives are %YAML, %TAG, or %RESERVED-NAME
        "%" -> {
          case lexer.col {
            0 -> {
              // Check if followed by a letter (directive name)
              case peek(advance(lexer)) {
                Some(next) -> {
                  case is_letter(next) {
                    True -> {
                      // Skip the directive line and continue with next token
                      let #(_, lexer) = read_until_newline(advance(lexer))
                      next_token(lexer)
                    }
                    False -> read_plain_scalar(lexer)
                  }
                }
                None -> read_plain_scalar(lexer)
              }
            }
            // Not at start of line - treat as plain scalar
            _ -> read_plain_scalar(lexer)
          }
        }
        // Plain scalar
        _ -> read_plain_scalar(lexer)
      }
    }
  }
}

fn peek(lexer: Lexer) -> Option(String) {
  case string.drop_start(lexer.input, lexer.pos) {
    "" -> None
    s -> Some(string.slice(s, 0, 1))
  }
}

fn peek_n(lexer: Lexer, n: Int) -> String {
  lexer.input
  |> string.drop_start(lexer.pos)
  |> string.slice(0, n)
}

fn advance(lexer: Lexer) -> Lexer {
  Lexer(..lexer, pos: lexer.pos + 1, col: lexer.col + 1)
}

fn advance_n(lexer: Lexer, n: Int) -> Lexer {
  Lexer(..lexer, pos: lexer.pos + n, col: lexer.col + n)
}

fn count_indent(lexer: Lexer) -> #(Int, Lexer) {
  count_indent_loop(lexer, 0)
}

fn count_indent_loop(lexer: Lexer, count: Int) -> #(Int, Lexer) {
  case peek(lexer) {
    Some(" ") -> count_indent_loop(advance(lexer), count + 1)
    _ -> #(count, Lexer(..lexer, col: count))
  }
}

fn read_until_newline(lexer: Lexer) -> #(String, Lexer) {
  read_until_newline_loop(lexer, "")
}

fn read_until_newline_loop(lexer: Lexer, acc: String) -> #(String, Lexer) {
  case peek(lexer) {
    None -> #(acc, lexer)
    Some("\n") -> #(acc, lexer)
    Some("\r") -> #(acc, lexer)
    Some(c) -> read_until_newline_loop(advance(lexer), acc <> c)
  }
}

fn read_identifier(lexer: Lexer) -> #(String, Lexer) {
  read_identifier_loop(lexer, "")
}

fn read_identifier_loop(lexer: Lexer, acc: String) -> #(String, Lexer) {
  case peek(lexer) {
    Some(c) -> {
      case c {
        " " | "\t" | "\n" | "\r" | ":" | "," | "[" | "]" | "{" | "}" | "#" -> #(
          acc,
          lexer,
        )
        _ -> read_identifier_loop(advance(lexer), acc <> c)
      }
    }
    _ -> #(acc, lexer)
  }
}

fn read_tag(lexer: Lexer) -> #(String, Lexer) {
  // Check if this is a verbatim tag (starts with <)
  case peek(lexer) {
    Some("<") -> read_verbatim_tag(advance(lexer), "!<")
    _ -> read_tag_loop(lexer, "!")
  }
}

/// Read a verbatim tag (enclosed in angle brackets).
/// These can contain any characters until the closing >.
fn read_verbatim_tag(lexer: Lexer, acc: String) -> #(String, Lexer) {
  case peek(lexer) {
    Some(">") -> #(acc <> ">", advance(lexer))
    Some(c) -> read_verbatim_tag(advance(lexer), acc <> c)
    None -> #(acc, lexer)
  }
}

fn read_tag_loop(lexer: Lexer, acc: String) -> #(String, Lexer) {
  case peek(lexer) {
    Some(c) -> {
      case c {
        " " | "\t" | "\n" | "\r" | "," | "[" | "]" | "{" | "}" -> #(acc, lexer)
        _ -> read_tag_loop(advance(lexer), acc <> c)
      }
    }
    _ -> #(acc, lexer)
  }
}

fn read_single_quoted(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  read_single_quoted_loop(lexer, "")
}

fn read_single_quoted_loop(
  lexer: Lexer,
  acc: String,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Error("Unterminated single-quoted string")
    Some("'") -> {
      // Check for escaped quote ''
      let lexer = advance(lexer)
      case peek(lexer) {
        Some("'") -> read_single_quoted_loop(advance(lexer), acc <> "'")
        _ -> Ok(#(SingleQuoted(acc), lexer))
      }
    }
    // Handle newline folding in single-quoted strings
    Some("\n") -> {
      let lexer = advance(lexer)
      // Skip leading whitespace on continuation line
      let lexer = skip_quoted_continuation_whitespace(lexer)
      // Check for empty lines (they become actual newlines)
      case peek(lexer) {
        Some("\n") -> {
          // Empty line - preserve as newline
          read_single_quoted_loop(lexer, acc <> "\n")
        }
        _ -> {
          // Single newline is folded to space
          read_single_quoted_loop(lexer, acc <> " ")
        }
      }
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      // Handle \r\n
      let lexer = case peek(lexer) {
        Some("\n") -> advance(lexer)
        _ -> lexer
      }
      let lexer = skip_quoted_continuation_whitespace(lexer)
      case peek(lexer) {
        Some("\n") | Some("\r") -> read_single_quoted_loop(lexer, acc <> "\n")
        _ -> read_single_quoted_loop(lexer, acc <> " ")
      }
    }
    Some(c) -> read_single_quoted_loop(advance(lexer), acc <> c)
  }
}

fn skip_quoted_continuation_whitespace(lexer: Lexer) -> Lexer {
  case peek(lexer) {
    Some(" ") | Some("\t") ->
      skip_quoted_continuation_whitespace(advance(lexer))
    _ -> lexer
  }
}

fn read_double_quoted(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  read_double_quoted_loop(lexer, "")
}

fn read_double_quoted_loop(
  lexer: Lexer,
  acc: String,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Error("Unterminated double-quoted string")
    Some("\"") -> Ok(#(DoubleQuoted(acc), advance(lexer)))
    Some("\\") -> {
      let lexer = advance(lexer)
      case peek(lexer) {
        None -> Error("Unterminated escape sequence")
        Some("n") -> read_double_quoted_loop(advance(lexer), acc <> "\n")
        Some("t") -> read_double_quoted_loop(advance(lexer), acc <> "\t")
        Some("r") -> read_double_quoted_loop(advance(lexer), acc <> "\r")
        Some("\\") -> read_double_quoted_loop(advance(lexer), acc <> "\\")
        Some("\"") -> read_double_quoted_loop(advance(lexer), acc <> "\"")
        Some("/") -> read_double_quoted_loop(advance(lexer), acc <> "/")
        Some("0") -> read_double_quoted_loop(advance(lexer), acc <> "\u{0000}")
        Some("a") -> read_double_quoted_loop(advance(lexer), acc <> "\u{0007}")
        Some("b") -> read_double_quoted_loop(advance(lexer), acc <> "\u{0008}")
        Some("e") -> read_double_quoted_loop(advance(lexer), acc <> "\u{001B}")
        Some("f") -> read_double_quoted_loop(advance(lexer), acc <> "\u{000C}")
        Some("v") -> read_double_quoted_loop(advance(lexer), acc <> "\u{000B}")
        Some("N") -> read_double_quoted_loop(advance(lexer), acc <> "\u{0085}")
        Some("_") -> read_double_quoted_loop(advance(lexer), acc <> "\u{00A0}")
        Some("L") -> read_double_quoted_loop(advance(lexer), acc <> "\u{2028}")
        Some("P") -> read_double_quoted_loop(advance(lexer), acc <> "\u{2029}")
        Some(" ") -> read_double_quoted_loop(advance(lexer), acc <> " ")
        Some("x") -> read_hex_escape(advance(lexer), acc, 2)
        Some("u") -> read_hex_escape(advance(lexer), acc, 4)
        Some("U") -> read_hex_escape(advance(lexer), acc, 8)
        // Line continuation - skip newline and leading whitespace on next line
        Some("\n") -> skip_line_continuation(advance(lexer), acc)
        Some("\r") -> {
          let lexer = advance(lexer)
          case peek(lexer) {
            Some("\n") -> skip_line_continuation(advance(lexer), acc)
            _ -> skip_line_continuation(lexer, acc)
          }
        }
        Some(c) -> Error("Invalid escape sequence: \\" <> c)
      }
    }
    // Handle unescaped newlines - fold to space (like single-quoted)
    Some("\n") -> {
      let lexer = advance(lexer)
      let lexer = skip_quoted_continuation_whitespace(lexer)
      case peek(lexer) {
        Some("\n") -> read_double_quoted_loop(lexer, acc <> "\n")
        _ -> read_double_quoted_loop(lexer, acc <> " ")
      }
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      let lexer = case peek(lexer) {
        Some("\n") -> advance(lexer)
        _ -> lexer
      }
      let lexer = skip_quoted_continuation_whitespace(lexer)
      case peek(lexer) {
        Some("\n") | Some("\r") -> read_double_quoted_loop(lexer, acc <> "\n")
        _ -> read_double_quoted_loop(lexer, acc <> " ")
      }
    }
    Some(c) -> read_double_quoted_loop(advance(lexer), acc <> c)
  }
}

fn skip_line_continuation(
  lexer: Lexer,
  acc: String,
) -> Result(#(Token, Lexer), String) {
  // Skip leading whitespace on the continuation line
  case peek(lexer) {
    Some(" ") -> skip_line_continuation(advance(lexer), acc)
    Some("\t") -> skip_line_continuation(advance(lexer), acc)
    _ -> read_double_quoted_loop(lexer, acc)
  }
}

fn read_hex_escape(
  lexer: Lexer,
  acc: String,
  digits: Int,
) -> Result(#(Token, Lexer), String) {
  case read_hex_digits(lexer, "", digits) {
    Error(e) -> Error(e)
    Ok(#(hex_str, lexer)) -> {
      case parse_hex(hex_str) {
        Ok(codepoint) -> {
          case string.utf_codepoint(codepoint) {
            Ok(cp) ->
              read_double_quoted_loop(
                lexer,
                acc <> string.from_utf_codepoints([cp]),
              )
            Error(_) -> Error("Invalid unicode codepoint: " <> hex_str)
          }
        }
        Error(_) -> Error("Invalid hex escape: " <> hex_str)
      }
    }
  }
}

fn read_hex_digits(
  lexer: Lexer,
  acc: String,
  remaining: Int,
) -> Result(#(String, Lexer), String) {
  case remaining {
    0 -> Ok(#(acc, lexer))
    _ -> {
      case peek(lexer) {
        None -> Error("Unterminated hex escape")
        Some(c) -> {
          case is_hex_char(c) {
            True -> read_hex_digits(advance(lexer), acc <> c, remaining - 1)
            False -> Error("Invalid hex character: " <> c)
          }
        }
      }
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
  read_plain_scalar_loop(lexer, "")
}

fn read_plain_scalar_loop(
  lexer: Lexer,
  acc: String,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Ok(#(Plain(string.trim_end(acc)), lexer))
    Some("\n") | Some("\r") -> Ok(#(Plain(string.trim_end(acc)), lexer))
    Some(",") | Some("]") | Some("}") ->
      Ok(#(Plain(string.trim_end(acc)), lexer))
    Some("#") -> {
      // Check if preceded by space or tab (comment)
      case string.ends_with(acc, " ") || string.ends_with(acc, "\t") {
        True -> Ok(#(Plain(string.trim_end(acc)), lexer))
        False -> read_plain_scalar_loop(advance(lexer), acc <> "#")
      }
    }
    Some(":") -> {
      // Check if followed by space/tab/newline (mapping indicator)
      let next_lexer = advance(lexer)
      case peek(next_lexer) {
        Some(" ") | Some("\t") | Some("\n") | Some("\r") | None ->
          Ok(#(Plain(string.trim_end(acc)), lexer))
        _ -> read_plain_scalar_loop(next_lexer, acc <> ":")
      }
    }
    Some(c) -> read_plain_scalar_loop(advance(lexer), acc <> c)
  }
}

/// Read a literal block scalar (|)
fn read_literal_block(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  // Skip chomping/indentation indicators and comments until newline
  let #(header, lexer) = read_block_header(lexer)

  // Skip to end of header line
  let lexer = skip_to_eol(lexer)

  // Skip the newline
  case peek(lexer) {
    Some("\n") -> {
      let lexer = advance(lexer)
      read_literal_content(lexer, header.chomping, header.explicit_indent)
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      case peek(lexer) {
        Some("\n") ->
          read_literal_content(
            advance(lexer),
            header.chomping,
            header.explicit_indent,
          )
        _ ->
          read_literal_content(lexer, header.chomping, header.explicit_indent)
      }
    }
    _ -> Ok(#(Literal(""), lexer))
  }
}

/// Read a folded block scalar (>)
fn read_folded_block(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  // Skip chomping/indentation indicators and comments until newline
  let #(header, lexer) = read_block_header(lexer)

  // Skip to end of header line
  let lexer = skip_to_eol(lexer)

  // Skip the newline
  case peek(lexer) {
    Some("\n") -> {
      let lexer = advance(lexer)
      read_folded_content(lexer, header.chomping, header.explicit_indent)
    }
    Some("\r") -> {
      let lexer = advance(lexer)
      case peek(lexer) {
        Some("\n") ->
          read_folded_content(
            advance(lexer),
            header.chomping,
            header.explicit_indent,
          )
        _ -> read_folded_content(lexer, header.chomping, header.explicit_indent)
      }
    }
    _ -> Ok(#(Folded(""), lexer))
  }
}

type Chomping {
  Strip
  // -
  Clip
  // default
  Keep
  // +
}

/// Block header result includes chomping indicator and optional explicit indent
type BlockHeader {
  BlockHeader(chomping: Chomping, explicit_indent: Option(Int))
}

fn read_block_header(lexer: Lexer) -> #(BlockHeader, Lexer) {
  read_block_header_loop(lexer, BlockHeader(Clip, None))
}

fn read_block_header_loop(
  lexer: Lexer,
  header: BlockHeader,
) -> #(BlockHeader, Lexer) {
  case peek(lexer) {
    Some("-") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, chomping: Strip),
      )
    Some("+") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, chomping: Keep),
      )
    Some("1") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(1)),
      )
    Some("2") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(2)),
      )
    Some("3") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(3)),
      )
    Some("4") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(4)),
      )
    Some("5") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(5)),
      )
    Some("6") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(6)),
      )
    Some("7") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(7)),
      )
    Some("8") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(8)),
      )
    Some("9") ->
      read_block_header_loop(
        advance(lexer),
        BlockHeader(..header, explicit_indent: Some(9)),
      )
    Some(" ") | Some("\t") -> read_block_header_loop(advance(lexer), header)
    Some("#") -> {
      // Skip comment
      let lexer = skip_to_eol(lexer)
      #(header, lexer)
    }
    _ -> #(header, lexer)
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
  // Count leading empty lines before the first content line
  let #(leading_newlines, lexer) = count_leading_empty_lines(lexer, 0)
  // Always auto-detect indent from first non-empty line for block boundaries
  case find_block_indent(lexer) {
    NoContent(lexer) -> Ok(#(Literal(""), lexer))
    FoundContent(detected_indent, lexer) -> {
      // For extra space calculation, use explicit indent if provided
      // This allows content to start with leading spaces preserved
      let base_indent = case explicit_indent {
        Some(n) -> n
        None -> detected_indent
      }
      // Start with the leading empty lines
      let prefix = string.repeat("\n", leading_newlines)
      // Use detected_indent for block boundaries, base_indent for extra space calculation
      let #(content, lexer) =
        read_literal_lines_ex(lexer, detected_indent, base_indent, prefix)
      let content = apply_chomping(content, chomping)
      Ok(#(Literal(content), lexer))
    }
  }
}

fn read_folded_content(
  lexer: Lexer,
  chomping: Chomping,
  explicit_indent: Option(Int),
) -> Result(#(Token, Lexer), String) {
  case explicit_indent {
    Some(indent) -> {
      // Use explicit indent - count leading empty lines first
      let #(leading_newlines, lexer) = count_leading_empty_lines(lexer, 0)
      let prefix = string.repeat("\n", leading_newlines)
      let initial_state = case leading_newlines > 0 {
        True -> FoldAfterBlank
        False -> FoldNormal
      }
      let #(content, lexer) =
        read_folded_lines(lexer, indent, prefix, initial_state)
      let content = apply_chomping(content, chomping)
      Ok(#(Folded(content), lexer))
    }
    None -> {
      // Auto-detect indent from first non-empty line
      case find_block_indent(lexer) {
        NoContent(lexer) -> Ok(#(Folded(""), lexer))
        FoundContent(block_indent, lexer) -> {
          let #(content, lexer) =
            read_folded_lines(lexer, block_indent, "", FoldNormal)
          let content = apply_chomping(content, chomping)
          Ok(#(Folded(content), lexer))
        }
      }
    }
  }
}

/// Count leading empty lines (lines with only whitespace).
fn count_leading_empty_lines(lexer: Lexer, count: Int) -> #(Int, Lexer) {
  // Count leading spaces
  let #(_indent, lexer_after_spaces) = count_leading_spaces(lexer, 0)
  case peek(lexer_after_spaces) {
    // Empty line - count it and continue
    Some("\n") ->
      count_leading_empty_lines(advance(lexer_after_spaces), count + 1)
    Some("\r") -> {
      let lexer = advance(lexer_after_spaces)
      case peek(lexer) {
        Some("\n") -> count_leading_empty_lines(advance(lexer), count + 1)
        _ -> count_leading_empty_lines(lexer, count + 1)
      }
    }
    // Not an empty line - done counting
    _ -> #(count, lexer)
  }
}

/// Result of finding block indent: either content was found or not.
type BlockIndentResult {
  /// Content found at the given indent level
  FoundContent(indent: Int, lexer: Lexer)
  /// No content found (end of input or empty)
  NoContent(lexer: Lexer)
}

fn find_block_indent(lexer: Lexer) -> BlockIndentResult {
  // Save the start position of this line
  let line_start = lexer
  // Count leading spaces
  let #(indent, lexer_after_spaces) = count_leading_spaces(lexer, 0)
  case peek(lexer_after_spaces) {
    // Empty line - look at next line for indent
    Some("\n") -> find_block_indent(advance(lexer_after_spaces))
    Some("\r") -> {
      let lexer = advance(lexer_after_spaces)
      case peek(lexer) {
        Some("\n") -> find_block_indent(advance(lexer))
        _ -> find_block_indent(lexer)
      }
    }
    // Found content - return lexer at start of line (before spaces)
    Some(_) -> FoundContent(indent, line_start)
    // End of input
    None -> NoContent(lexer_after_spaces)
  }
}

fn count_leading_spaces(lexer: Lexer, count: Int) -> #(Int, Lexer) {
  case peek(lexer) {
    Some(" ") -> count_leading_spaces(advance(lexer), count + 1)
    _ -> #(count, lexer)
  }
}

/// Read literal lines with separate boundary and base indentation.
/// boundary_indent: the auto-detected indent, used to determine when block ends
/// base_indent: the explicit indent (or same as boundary), used for extra space calculation
fn read_literal_lines_ex(
  lexer: Lexer,
  boundary_indent: Int,
  base_indent: Int,
  acc: String,
) -> #(String, Lexer) {
  // Skip the block indent spaces, then read content
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)
  // Include any extra indentation beyond base_indent (from explicit indicator)
  let extra_indent = case indent > base_indent {
    True -> string.repeat(" ", indent - base_indent)
    False -> ""
  }
  let #(line, lexer) = read_until_eol(after_spaces, extra_indent)
  let new_acc = case acc {
    "" -> line
    _ -> acc <> "\n" <> line
  }

  // Check for newline and continuation
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

/// Check literal continuation with separate boundary and base indentation.
/// boundary_indent: used to determine when block ends
/// base_indent: used for extra space calculation
fn check_literal_continuation_ex(
  lexer: Lexer,
  boundary_indent: Int,
  base_indent: Int,
  acc: String,
) -> #(String, Lexer) {
  // Count leading spaces on this line
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)

  case peek(after_spaces) {
    // Empty line - include it and continue
    Some("\n") -> {
      check_literal_continuation_ex(
        advance(after_spaces),
        boundary_indent,
        base_indent,
        acc <> "\n",
      )
    }
    Some("\r") -> {
      let lexer_after_cr = advance(after_spaces)
      case peek(lexer_after_cr) {
        Some("\n") ->
          check_literal_continuation_ex(
            advance(lexer_after_cr),
            boundary_indent,
            base_indent,
            acc <> "\n",
          )
        _ ->
          check_literal_continuation_ex(
            lexer_after_cr,
            boundary_indent,
            base_indent,
            acc <> "\n",
          )
      }
    }
    None -> #(acc, lexer)
    Some(_) -> {
      // Check if indent is sufficient (using boundary_indent)
      case indent >= boundary_indent {
        True -> {
          // Continue reading, include extra indentation (using base_indent)
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
        False -> {
          // Block ends - back up to before the newline so it gets tokenized
          // The lexer is at start of line (after newline was consumed), so back up by 1
          let backed_up = Lexer(..lexer, pos: lexer.pos - 1)
          #(acc, backed_up)
        }
      }
    }
  }
}

/// Tracks what caused the previous line break to be preserved
type FoldState {
  /// Normal state - line breaks fold to space
  FoldNormal
  /// After a blank line - line break was preserved
  FoldAfterBlank
  /// After a more-indented line - line break was preserved
  FoldAfterMoreIndented
}

/// Read folded block content.
fn read_folded_lines(
  lexer: Lexer,
  block_indent: Int,
  acc: String,
  state: FoldState,
) -> #(String, Lexer) {
  // Count leading spaces on this line
  let #(indent, after_spaces) = count_leading_spaces(lexer, 0)

  case peek(after_spaces) {
    // Empty line - add a newline and mark that we came from blank
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
      case indent >= block_indent {
        True -> {
          // Check if this is a "more indented" line (indent > block_indent)
          let is_more_indented = indent > block_indent
          // Include extra indentation beyond block_indent
          let extra_indent = string.repeat(" ", indent - block_indent)
          // Read content with extra indent preserved
          let #(line, lexer) = read_until_eol(after_spaces, extra_indent)

          // Build the new accumulator
          // Rules:
          // - After blank + more-indented: add \n (line break before more-indented preserved)
          // - After blank + normal: just append (blank's \n is separator)
          // - After more-indented + anything: just append (\n already added after more-indented)
          // - Normal + more-indented: add \n (line break before more-indented preserved)
          // - Normal + normal: fold with space
          let new_acc = case acc {
            "" -> line
            _ ->
              case state, is_more_indented {
                // After blank + more-indented: add \n (preserved line break)
                FoldAfterBlank, True -> acc <> "\n" <> line
                // After blank + normal: just append
                FoldAfterBlank, False -> acc <> line
                // After more-indented + anything: just append (\n already added)
                FoldAfterMoreIndented, _ -> acc <> line
                // Normal + more-indented: add \n (preserved line break)
                FoldNormal, True -> acc <> "\n" <> line
                // Normal + normal: fold with space
                FoldNormal, False -> acc <> " " <> line
              }
          }

          // Peek at next line to determine how to continue
          case peek(lexer) {
            Some("\n") -> {
              // If this was a more-indented line, add newline and set state
              case is_more_indented {
                True ->
                  read_folded_lines(
                    advance(lexer),
                    block_indent,
                    new_acc <> "\n",
                    FoldAfterMoreIndented,
                  )
                False ->
                  read_folded_lines(
                    advance(lexer),
                    block_indent,
                    new_acc,
                    FoldNormal,
                  )
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
                    _ ->
                      read_folded_lines(
                        lexer,
                        block_indent,
                        new_acc,
                        FoldNormal,
                      )
                  }
                }
              }
            }
            _ -> #(new_acc, lexer)
          }
        }
        False -> {
          // Block ends - don't consume this line
          #(acc, lexer)
        }
      }
    }
  }
}

fn read_until_eol(lexer: Lexer, acc: String) -> #(String, Lexer) {
  case peek(lexer) {
    None -> #(acc, lexer)
    Some("\n") -> #(acc, lexer)
    Some("\r") -> #(acc, lexer)
    Some(c) -> read_until_eol(advance(lexer), acc <> c)
  }
}

fn apply_chomping(content: String, chomping: Chomping) -> String {
  case chomping {
    Strip -> string.trim_end(content)
    Clip -> {
      let trimmed = string.trim_end(content)
      case trimmed {
        "" -> ""
        _ -> trimmed <> "\n"
      }
    }
    Keep -> content <> "\n"
  }
}
