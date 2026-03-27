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
  /// Literal block scalar |
  Literal
  /// Folded block scalar >
  Folded
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
            Some(" ") | Some("\n") | Some("\r") | None -> Ok(#(Colon, lexer))
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
              let lexer = advance_n(lexer, 3)
              Ok(#(DocStart, lexer))
            }
            _ -> {
              let lexer = advance(lexer)
              case peek(lexer) {
                Some(" ") | Some("\n") | Some("\r") | None -> Ok(#(Dash, lexer))
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
            "..." -> Ok(#(DocEnd, advance_n(lexer, 3)))
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
        "|" -> Ok(#(Literal, advance(lexer)))
        ">" -> Ok(#(Folded, advance(lexer)))
        // Quoted strings
        "'" -> read_single_quoted(advance(lexer))
        "\"" -> read_double_quoted(advance(lexer))
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
        " " | "\t" | "\n" | "\r" | ":" | "," | "[" | "]" | "{" | "}" | "#" ->
          #(acc, lexer)
        _ -> read_identifier_loop(advance(lexer), acc <> c)
      }
    }
    _ -> #(acc, lexer)
  }
}

fn read_tag(lexer: Lexer) -> #(String, Lexer) {
  read_tag_loop(lexer, "!")
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
    Some(c) -> read_single_quoted_loop(advance(lexer), acc <> c)
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
        Some(c) -> read_double_quoted_loop(advance(lexer), acc <> c)
      }
    }
    Some(c) -> read_double_quoted_loop(advance(lexer), acc <> c)
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
      // Check if preceded by space (comment)
      case string.ends_with(acc, " ") {
        True -> Ok(#(Plain(string.trim_end(acc)), lexer))
        False -> read_plain_scalar_loop(advance(lexer), acc <> "#")
      }
    }
    Some(":") -> {
      // Check if followed by space/newline (mapping indicator)
      let next_lexer = advance(lexer)
      case peek(next_lexer) {
        Some(" ") | Some("\n") | Some("\r") | None ->
          Ok(#(Plain(string.trim_end(acc)), lexer))
        _ -> read_plain_scalar_loop(next_lexer, acc <> ":")
      }
    }
    Some(c) -> read_plain_scalar_loop(advance(lexer), acc <> c)
  }
}
