//// YAML lexer - tokenizes YAML input.

import gleam/int
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
  /// YAML directive (%YAML, %TAG, etc.)
  Directive(String)
  /// End of file
  Eof
}

/// Lexer state.
pub type Lexer {
  Lexer(
    input: String,
    pos: Int,
    line: Int,
    col: Int,
    in_document: Bool,
    flow_level: Int,
  )
}

/// Creates a new lexer.
pub fn new(input: String) -> Lexer {
  Lexer(
    input: input,
    pos: 0,
    line: 1,
    col: 0,
    in_document: False,
    flow_level: 0,
  )
}

/// Tokenizes the entire input.
pub fn tokenize(input: String) -> Result(List(Token), String) {
  let lexer = new(input)
  case count_indent(lexer) {
    Error(e) -> Error(e)
    Ok(#(indent, lexer)) ->
      case indent {
        0 -> tokenize_all(lexer, [])
        n -> tokenize_all(lexer, [Indent(n)])
      }
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

/// Gets the next token.
pub fn next_token(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Ok(#(Eof, lexer))
    Some(c) -> {
      case c {
        // Newline
        "\n" -> {
          let lexer = advance(lexer)
          case count_indent(lexer) {
            Error(e) -> Error(e)
            Ok(#(indent, lexer)) ->
              case indent {
                0 -> Ok(#(Newline, lexer))
                n -> Ok(#(Indent(n), lexer))
              }
          }
        }
        "\r" -> {
          let lexer = advance(lexer)
          // Handle \r\n
          let lexer = case peek(lexer) {
            Some("\n") -> advance(lexer)
            _ -> lexer
          }
          case count_indent(lexer) {
            Error(e) -> Error(e)
            Ok(#(indent, lexer)) ->
              case indent {
                0 -> Ok(#(Newline, lexer))
                n -> Ok(#(Indent(n), lexer))
              }
          }
        }
        // Skip spaces (not at line start)
        " " -> next_token(advance(lexer))
        "\t" -> next_token(advance(lexer))
        // Comment - must be preceded by whitespace or at start of line
        "#" -> {
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
            False ->
              Error("Invalid comment: '#' must be preceded by whitespace")
          }
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
                  // Set in_document to True when entering a document
                  Ok(#(DocStart, Lexer(..after_dashes, in_document: True)))
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
                // In flow context, dash followed by flow indicator is invalid
                Some(",") | Some("]") | Some("}") | Some("[") | Some("{")
                  if lexer.flow_level > 0
                -> Error("Invalid use of dash indicator in flow context")
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
                  // Set in_document to False when leaving a document
                  Ok(#(DocEnd, Lexer(..after_dots, in_document: False)))
                _ ->
                  // Part of a plain scalar (like ...word)
                  read_plain_scalar(lexer)
              }
            }
            _ -> read_plain_scalar(lexer)
          }
        }
        // Flow indicators
        "[" -> {
          let lexer = advance(lexer)
          Ok(#(BracketOpen, Lexer(..lexer, flow_level: lexer.flow_level + 1)))
        }
        "]" -> {
          let lexer = advance(lexer)
          Ok(#(
            BracketClose,
            Lexer(..lexer, flow_level: int.max(0, lexer.flow_level - 1)),
          ))
        }
        "{" -> {
          let lexer = advance(lexer)
          Ok(#(BraceOpen, Lexer(..lexer, flow_level: lexer.flow_level + 1)))
        }
        "}" -> {
          let lexer = advance(lexer)
          Ok(#(
            BraceClose,
            Lexer(..lexer, flow_level: int.max(0, lexer.flow_level - 1)),
          ))
        }
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
        // Directive (only at start of line, followed by letter, and NOT inside a document)
        // Directives are %YAML, %TAG, or %RESERVED-NAME
        // Inside a document (after ---), % is just content
        "%" -> {
          case lexer.col, lexer.in_document {
            // At start of line and NOT inside a document - might be directive
            0, False -> {
              // Check if followed by a letter (directive name)
              case peek(advance(lexer)) {
                Some(next) -> {
                  case is_letter(next) {
                    True -> {
                      // Emit directive token instead of skipping
                      let #(text, lexer) = read_until_newline(advance(lexer))
                      Ok(#(Directive(text), lexer))
                    }
                    False -> read_plain_scalar(lexer)
                  }
                }
                None -> read_plain_scalar(lexer)
              }
            }
            // Inside document or not at start of line - treat as plain scalar
            _, _ -> read_plain_scalar(lexer)
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

fn count_indent(lexer: Lexer) -> Result(#(Int, Lexer), String) {
  count_indent_loop(lexer, 0)
}

fn count_indent_loop(lexer: Lexer, count: Int) -> Result(#(Int, Lexer), String) {
  case peek(lexer) {
    Some(" ") -> count_indent_loop(advance(lexer), count + 1)
    Some("\t") ->
      case lexer.flow_level > 0 {
        // In flow context, tabs are fine
        True -> count_indent_loop(advance(lexer), count + 1)
        False ->
          case count {
            // Tab as first character of indentation
            0 -> {
              // Check what follows the tab(s)
              // Tabs before flow collections or non-indicator content are OK
              // Tabs before block indicators (mapping key, sequence dash) are errors
              let after_tabs = skip_tabs_lexer(advance(lexer))
              case peek(after_tabs) {
                // Block sequence/mapping/explicit key indicators after tab = error
                Some("-") | Some("?") ->
                  Error("Tabs are not allowed as indentation in YAML")
                // EOF or newline after tab = just whitespace
                None | Some("\n") | Some("\r") ->
                  Ok(#(0, Lexer(..lexer, col: 0)))
                // Everything else (flow, scalars, etc.) = tab is just whitespace
                _ -> Ok(#(0, Lexer(..after_tabs, col: 0)))
              }
            }
            // Tab after spaces - stop counting, tab is content/inline whitespace
            _ -> Ok(#(count, Lexer(..lexer, col: count)))
          }
      }
    _ -> Ok(#(count, Lexer(..lexer, col: count)))
  }
}

fn skip_tabs_lexer(lexer: Lexer) -> Lexer {
  case peek(lexer) {
    Some("\t") -> skip_tabs_lexer(advance(lexer))
    Some(" ") -> skip_tabs_lexer(advance(lexer))
    _ -> lexer
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
  // YAML anchor/alias names can contain most characters except:
  // - whitespace (space, tab, newline)
  // - flow indicators: , [ ] { }
  // NOTE: Colons ARE allowed in anchor names per YAML 1.2 spec
  case peek(lexer) {
    Some(c) -> {
      case c {
        " " | "\t" | "\n" | "\r" | "," | "[" | "]" | "{" | "}" -> #(acc, lexer)
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
  read_single_quoted_loop(lexer, "", False)
}

fn read_single_quoted_loop(
  lexer: Lexer,
  acc: String,
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Error("Unterminated single-quoted string")
    Some("'") -> {
      // Check for escaped quote ''
      let lexer = advance(lexer)
      case peek(lexer) {
        Some("'") ->
          read_single_quoted_loop(advance(lexer), acc <> "'", multiline)
        _ ->
          case multiline {
            True -> check_multiline_implicit_key(lexer, SingleQuoted(acc))
            False -> Ok(#(SingleQuoted(acc), lexer))
          }
      }
    }
    // Handle newline folding in single-quoted strings
    Some("\n") -> {
      let lexer = advance(lexer)
      // Check for document markers at column 0
      case check_document_marker(lexer) {
        True ->
          Error(
            "Unterminated single-quoted string (document marker in quoted string)",
          )
        False -> {
          // Skip leading whitespace on continuation line
          let lexer = skip_quoted_continuation_whitespace(lexer)
          // Check for empty lines (they become actual newlines)
          case peek(lexer) {
            Some("\n") -> {
              // Empty line - preserve as newline
              read_single_quoted_loop(lexer, acc <> "\n", True)
            }
            _ -> {
              // Single newline is folded to space
              read_single_quoted_loop(lexer, acc <> " ", True)
            }
          }
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
      // Check for document markers at column 0
      case check_document_marker(lexer) {
        True ->
          Error(
            "Unterminated single-quoted string (document marker in quoted string)",
          )
        False -> {
          let lexer = skip_quoted_continuation_whitespace(lexer)
          case peek(lexer) {
            Some("\n") | Some("\r") ->
              read_single_quoted_loop(lexer, acc <> "\n", True)
            _ -> read_single_quoted_loop(lexer, acc <> " ", True)
          }
        }
      }
    }
    Some(c) -> read_single_quoted_loop(advance(lexer), acc <> c, multiline)
  }
}

/// Check if a multiline quoted string is being used as an implicit mapping key.
/// If so, error in block context. In flow context, multiline keys are allowed.
fn check_multiline_implicit_key(
  lexer: Lexer,
  token: Token,
) -> Result(#(Token, Lexer), String) {
  // In flow context, multiline quoted keys are allowed
  case lexer.flow_level > 0 {
    True -> Ok(#(token, lexer))
    False -> {
      // Skip spaces (not newlines) to check for colon
      let check_lexer = skip_inline_spaces(lexer)
      case peek(check_lexer) {
        Some(":") -> {
          // Check if followed by whitespace or end (mapping indicator)
          let after_colon = advance(check_lexer)
          case peek(after_colon) {
            Some(" ") | Some("\n") | Some("\r") | Some("\t") | None ->
              Error("Multiline quoted string cannot be used as an implicit key")
            // In flow context, colon before flow indicators is also a separator
            Some(",") | Some("]") | Some("}") ->
              Error("Multiline quoted string cannot be used as an implicit key")
            _ -> Ok(#(token, lexer))
          }
        }
        _ -> Ok(#(token, lexer))
      }
    }
  }
}

fn skip_inline_spaces(lexer: Lexer) -> Lexer {
  case peek(lexer) {
    Some(" ") -> skip_inline_spaces(advance(lexer))
    _ -> lexer
  }
}

/// Check if the current position starts with a document marker (--- or ...) at column 0.
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

fn read_double_quoted(lexer: Lexer) -> Result(#(Token, Lexer), String) {
  // pending_ws tracks literal whitespace that may be trimmed if newline follows
  read_double_quoted_loop(lexer, "", "", False)
}

/// Read double-quoted string with trailing whitespace handling.
/// pending_ws tracks literal whitespace that gets trimmed if followed by newline.
fn read_double_quoted_loop(
  lexer: Lexer,
  acc: String,
  pending_ws: String,
  multiline: Bool,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    None -> Error("Unterminated double-quoted string")
    // Include pending whitespace before closing quote
    Some("\"") ->
      case multiline {
        True ->
          check_multiline_implicit_key(
            advance(lexer),
            DoubleQuoted(acc <> pending_ws),
          )
        False -> Ok(#(DoubleQuoted(acc <> pending_ws), advance(lexer)))
      }
    Some("\\") -> {
      // Escape sequences flush pending whitespace (they're not trailing)
      let full_acc = acc <> pending_ws
      let lexer = advance(lexer)
      case peek(lexer) {
        None -> Error("Unterminated escape sequence")
        Some("n") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\n",
            "",
            multiline,
          )
        Some("t") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\t",
            "",
            multiline,
          )
        Some("r") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\r",
            "",
            multiline,
          )
        Some("\\") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\\",
            "",
            multiline,
          )
        Some("\"") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\"",
            "",
            multiline,
          )
        Some("/") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "/",
            "",
            multiline,
          )
        Some("0") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{0000}",
            "",
            multiline,
          )
        Some("a") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{0007}",
            "",
            multiline,
          )
        Some("b") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{0008}",
            "",
            multiline,
          )
        Some("e") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{001B}",
            "",
            multiline,
          )
        Some("f") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{000C}",
            "",
            multiline,
          )
        Some("v") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{000B}",
            "",
            multiline,
          )
        Some("N") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{0085}",
            "",
            multiline,
          )
        Some("_") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{00A0}",
            "",
            multiline,
          )
        Some("L") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{2028}",
            "",
            multiline,
          )
        Some("P") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> "\u{2029}",
            "",
            multiline,
          )
        Some(" ") ->
          read_double_quoted_loop(
            advance(lexer),
            full_acc <> " ",
            "",
            multiline,
          )
        Some("x") -> read_hex_escape(advance(lexer), full_acc, 2, multiline)
        Some("u") -> read_hex_escape(advance(lexer), full_acc, 4, multiline)
        Some("U") -> read_hex_escape(advance(lexer), full_acc, 8, multiline)
        // Line continuation - skip newline and leading whitespace on next line
        // Note: pending_ws is preserved (it's before the \, not trailing)
        Some("\n") -> {
          let next_lexer = advance(lexer)
          case check_document_marker(next_lexer) {
            True ->
              Error(
                "Unterminated double-quoted string (document marker in quoted string)",
              )
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
              Error(
                "Unterminated double-quoted string (document marker in quoted string)",
              )
            False -> skip_line_continuation(next_lexer, full_acc)
          }
        }
        Some(c) -> Error("Invalid escape sequence: \\" <> c)
      }
    }
    // Handle unescaped newlines - fold to space, discard pending_ws (trailing whitespace)
    Some("\n") -> {
      let lexer = advance(lexer)
      case check_document_marker(lexer) {
        True ->
          Error(
            "Unterminated double-quoted string (document marker in quoted string)",
          )
        False -> {
          let lexer = skip_quoted_continuation_whitespace(lexer)
          // Check for blank lines (each becomes \n)
          handle_double_quoted_fold(lexer, acc, "")
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
            "Unterminated double-quoted string (document marker in quoted string)",
          )
        False -> {
          let lexer = skip_quoted_continuation_whitespace(lexer)
          // Check for blank lines (each becomes \n)
          handle_double_quoted_fold(lexer, acc, "")
        }
      }
    }
    // Literal whitespace - add to pending_ws instead of acc
    Some(" ") ->
      read_double_quoted_loop(advance(lexer), acc, pending_ws <> " ", multiline)
    Some("\t") ->
      read_double_quoted_loop(
        advance(lexer),
        acc,
        pending_ws <> "\t",
        multiline,
      )
    // Non-whitespace - flush pending_ws and add character
    Some(c) ->
      read_double_quoted_loop(
        advance(lexer),
        acc <> pending_ws <> c,
        "",
        multiline,
      )
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
    _ -> read_double_quoted_loop(lexer, acc, "", True)
  }
}

/// Handle line folding after skipping initial whitespace.
/// Blank lines (just whitespace) become \n, otherwise fold to space.
fn handle_double_quoted_fold(
  lexer: Lexer,
  acc: String,
  newlines: String,
) -> Result(#(Token, Lexer), String) {
  case peek(lexer) {
    // Blank line - add newline and continue checking
    Some("\n") -> {
      let lexer = advance(lexer)
      case check_document_marker(lexer) {
        True ->
          Error(
            "Unterminated double-quoted string (document marker in quoted string)",
          )
        False -> {
          let lexer = skip_quoted_continuation_whitespace(lexer)
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
            "Unterminated double-quoted string (document marker in quoted string)",
          )
        False -> {
          let lexer = skip_quoted_continuation_whitespace(lexer)
          handle_double_quoted_fold(lexer, acc, newlines <> "\n")
        }
      }
    }
    // Content found - add accumulated newlines or fold to space
    _ -> {
      case newlines {
        "" -> read_double_quoted_loop(lexer, acc <> " ", "", True)
        _ -> read_double_quoted_loop(lexer, acc <> newlines, "", True)
      }
    }
  }
}

fn read_hex_escape(
  lexer: Lexer,
  acc: String,
  digits: Int,
  multiline: Bool,
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
                "",
                multiline,
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
  case read_block_header(lexer) {
    Error(e) -> Error(e)
    Ok(#(header, lexer)) -> read_literal_block_content(header, lexer)
  }
}

fn read_literal_block_content(
  header: BlockHeader,
  lexer: Lexer,
) -> Result(#(Token, Lexer), String) {
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
  case read_block_header(lexer) {
    Error(e) -> Error(e)
    Ok(#(header, lexer)) -> {
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
            _ ->
              read_folded_content(
                lexer,
                header.chomping,
                header.explicit_indent,
              )
          }
        }
        _ -> Ok(#(Folded(""), lexer))
      }
    }
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
    Some("0") -> Error("Block scalar indentation indicator must be 1-9, got 0")
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
          Error("Comment in block scalar header must be preceded by whitespace")
      }
    }
    Some("\n") | Some("\r") | None -> Ok(#(header, lexer))
    Some(c) -> Error("Invalid character in block scalar header: " <> c)
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
    NoContent(end_lexer) -> {
      // For empty block scalars, check if the "leading" empty lines were actually
      // followed by content (even if at insufficient indent) or by true EOF
      // If followed by content at indent 0 (block terminator), those lines aren't part of block
      let actual_trailing = case peek(end_lexer) {
        // True EOF - the empty lines are trailing newlines for the block
        None -> leading_newlines
        // Content at indent 0 - those empty lines aren't part of this block
        _ -> 0
      }
      let content = case chomping {
        Strip -> ""
        Clip -> ""
        Keep -> string.repeat("\n", actual_trailing)
      }
      Ok(#(Literal(content), lexer))
    }
    FoundContent(detected_indent, lexer) -> {
      // For extra space calculation, use explicit indent if provided
      // This allows content to start with leading spaces preserved
      let #(boundary_indent, base_indent) = case explicit_indent {
        Some(n) -> {
          let boundary = find_min_block_indent(lexer, detected_indent, n)
          #(boundary, boundary)
        }
        None -> #(detected_indent, detected_indent)
      }
      // Start with the leading empty lines
      let prefix = string.repeat("\n", leading_newlines)
      // Use boundary_indent for block boundaries, base_indent for extra space calculation
      let #(content, lexer) =
        read_literal_lines_ex(lexer, boundary_indent, base_indent, prefix)
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
  // Count leading empty lines first (before finding content)
  let #(leading_newlines, lexer) = count_leading_empty_lines(lexer, 0)
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
      // Auto-detect indent from first non-empty line
      case find_block_indent(lexer) {
        NoContent(end_lexer) -> {
          // For empty block scalars, check if the "leading" empty lines were actually
          // followed by content (even if at insufficient indent) or by true EOF
          let actual_trailing = case peek(end_lexer) {
            // True EOF - the empty lines are trailing newlines for the block
            None -> leading_newlines
            // Content at indent 0 - those empty lines aren't part of this block
            _ -> 0
          }
          let content = case chomping {
            Strip -> ""
            Clip -> ""
            Keep -> string.repeat("\n", actual_trailing)
          }
          Ok(#(Folded(content), lexer))
        }
        FoundContent(block_indent, lexer) -> {
          let #(content, lexer) =
            read_folded_lines(lexer, block_indent, prefix, initial_state)
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
    // Found content - check if it's valid block scalar content
    Some(_) -> {
      case indent {
        // Content at indent 0: check if it looks like a new mapping key or document marker
        // If so, the block scalar is empty
        0 -> {
          case is_block_terminator(lexer_after_spaces) {
            True -> NoContent(line_start)
            False -> FoundContent(0, line_start)
          }
        }
        _ -> FoundContent(indent, line_start)
      }
    }
    // End of input
    None -> NoContent(lexer_after_spaces)
  }
}

/// Check if current position starts with --- or ... followed by space/tab/newline/EOF.
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

/// Check if the content at this position terminates a block scalar.
/// This includes mapping keys (word followed by colon) and document markers.
fn is_block_terminator(lexer: Lexer) -> Bool {
  // Check for document markers
  case peek_n(lexer, 3) {
    "---" | "..." -> True
    _ -> {
      // Check if this line contains a colon followed by space/newline/end
      // which would indicate a mapping key
      check_for_colon_in_line(lexer)
    }
  }
}

/// Scan the line for a colon that indicates a mapping key.
fn check_for_colon_in_line(lexer: Lexer) -> Bool {
  case peek(lexer) {
    None | Some("\n") | Some("\r") -> False
    Some(":") -> {
      // Check what follows the colon
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

/// Find the minimum indent in the block content when explicit indent is given.
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
    // Empty line - skip
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
  // Don't add extra newline if acc already ends with newline (from leading blank lines)
  let new_acc = case acc {
    "" -> line
    _ ->
      case string.ends_with(acc, "\n") {
        True -> acc <> line
        False -> acc <> "\n" <> line
      }
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
    // Line with only whitespace - check if extra indent should be content
    Some("\n") -> {
      // If indent >= boundary_indent, include extra spaces as content
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
      // If indent >= boundary_indent, include extra spaces as content
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
      // Check if indent is sufficient (using boundary_indent)
      case indent >= boundary_indent {
        True -> {
          // At indent 0, check for document markers which terminate the block
          case indent == 0 && is_doc_marker(after_spaces) {
            True -> {
              let backed_up = Lexer(..lexer, pos: lexer.pos - 1)
              #(acc, backed_up)
            }
            False -> {
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
          // Block ends - don't consume this line
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
  // Check if this is a "more indented" line (indent > block_indent)
  // Also treat lines starting with a tab as more-indented (for folding)
  let starts_with_tab = peek(after_spaces) == Some("\t")
  let is_more_indented = indent > block_indent || starts_with_tab
  // Include extra indentation beyond block_indent
  let extra_indent = string.repeat(" ", indent - block_indent)
  // Read content with extra indent preserved
  let #(line, lexer) = read_until_eol(after_spaces, extra_indent)

  // Build the new accumulator
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

  // Peek at next line to determine how to continue
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
    Strip -> trim_trailing_newlines(content)
    Clip -> {
      let trimmed = trim_trailing_newlines(content)
      case trimmed {
        "" -> ""
        _ -> trimmed <> "\n"
      }
    }
    // Keep: add the final newline (content doesn't include trailing newlines)
    // For content that's already just newlines (from NoContent case), add one more
    Keep -> content <> "\n"
  }
}

/// Trim only trailing newline characters (not all whitespace).
/// This preserves trailing spaces on the last content line.
fn trim_trailing_newlines(s: String) -> String {
  case string.last(s) {
    Ok("\n") | Ok("\r") -> trim_trailing_newlines(string.drop_end(s, 1))
    _ -> s
  }
}
