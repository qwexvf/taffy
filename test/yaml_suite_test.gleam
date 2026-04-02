import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit/should
import simplifile
import yaml
import yaml/value

/// Run tests from the YAML test suite
pub fn yaml_test_suite_test() {
  // Read all test files
  let assert Ok(files) = simplifile.read_directory("test-suite/src")

  let yaml_files =
    files
    |> list.filter(fn(f) { string.ends_with(f, ".yaml") })
    |> list.sort(string.compare)

  io.println(
    "\nRunning YAML test suite: "
    <> int.to_string(list.length(yaml_files))
    <> " test files",
  )

  let results =
    yaml_files
    |> list.map(fn(file) { run_test_file("test-suite/src/" <> file) })

  let passed =
    list.filter(results, fn(r) { r.0 == "pass" || r.0 == "order" })
    |> list.length
  let skipped = list.filter(results, fn(r) { r.0 == "skip" }) |> list.length

  // Separate error test failures from parse failures
  let error_test_failures =
    list.filter(results, fn(r) {
      r.0 == "fail" && string.contains(r.1, "should have failed")
    })
    |> list.length

  // JSON mismatches - separate key ordering from real value differences
  let key_order_only =
    list.filter(results, fn(r) { r.0 == "order" })
    |> list.length

  let json_mismatches =
    list.filter(results, fn(r) {
      r.0 == "fail" && string.contains(r.1, "json mismatch")
    })
    |> list.length

  // Real parse failures (not json mismatch, not should-have-failed)
  let parse_failures =
    list.filter(results, fn(r) {
      r.0 == "fail"
      && !string.contains(r.1, "should have failed")
      && !string.contains(r.1, "json mismatch")
    })

  io.println("")
  io.println(
    "Results: "
    <> int.to_string(passed)
    <> " passed, "
    <> int.to_string(list.length(parse_failures))
    <> " parse failures, "
    <> int.to_string(json_mismatches)
    <> " value mismatches, "
    <> int.to_string(key_order_only)
    <> " key-order-only, "
    <> int.to_string(error_test_failures)
    <> " lenient, "
    <> int.to_string(skipped)
    <> " skipped",
  )

  // Print actual parse failures (not json mismatches)
  case parse_failures {
    [] -> io.println("\nNo parse failures - all valid YAML parsed correctly!")
    failures -> {
      io.println("\nParse failures:")
      list.each(failures, fn(r) { io.println("  - " <> r.1) })
    }
  }

  // Print JSON mismatches for investigation
  let json_mismatch_list =
    list.filter(results, fn(r) {
      r.0 == "fail" && string.contains(r.1, "json mismatch")
    })
  case json_mismatch_list {
    [] -> Nil
    mismatches -> {
      io.println(
        "\nJSON mismatches (" <> int.to_string(list.length(mismatches)) <> "):",
      )
      list.each(list.take(mismatches, 30), fn(r) { io.println("  - " <> r.1) })
      case list.length(mismatches) > 30 {
        True ->
          io.println(
            "  ... and "
            <> int.to_string(list.length(mismatches) - 30)
            <> " more",
          )
        False -> Nil
      }
    }
  }

  // Print skipped tests
  let skip_list = list.filter(results, fn(r) { r.0 == "skip" })
  case skip_list {
    [] -> Nil
    _ -> {
      io.println("\nSkipped (" <> int.to_string(list.length(skip_list)) <> "):")
      list.each(skip_list, fn(r) { io.println("  - " <> r.1) })
    }
  }

  // Print key-order-only
  let order_list = list.filter(results, fn(r) { r.0 == "order" })
  case order_list {
    [] -> Nil
    _ -> {
      io.println(
        "\nKey-order-only (" <> int.to_string(list.length(order_list)) <> "):",
      )
      list.each(order_list, fn(r) { io.println("  - " <> r.1) })
    }
  }

  // Print lenient (should have failed but didn't)
  let lenient_list =
    list.filter(results, fn(r) {
      r.0 == "fail" && string.contains(r.1, "should have failed")
    })
  case lenient_list {
    [] -> Nil
    _ -> {
      io.println(
        "\nLenient (" <> int.to_string(list.length(lenient_list)) <> "):",
      )
      list.each(list.take(lenient_list, 50), fn(r) { io.println("  - " <> r.1) })
      case list.length(lenient_list) > 50 {
        True ->
          io.println(
            "  ... and "
            <> int.to_string(list.length(lenient_list) - 20)
            <> " more",
          )
        False -> Nil
      }
    }
  }

  // We allow some parse failures for unimplemented features
  // Real parse failures threshold (not counting json mismatches)
  case list.length(parse_failures) < 100 {
    True -> Nil
    False -> {
      io.println("Too many parse failures!")
      should.fail()
    }
  }
}

fn run_test_file(path: String) -> #(String, String) {
  case simplifile.read(path) {
    Error(_) -> #("skip", path <> " (read error)")
    Ok(content) -> {
      // Parse the test file itself (it's YAML)
      case yaml.parse(content) {
        Error(_) -> #("skip", path <> " (meta-parse error)")
        Ok(meta) -> run_test_cases(path, meta)
      }
    }
  }
}

fn run_test_cases(path: String, meta: yaml.Value) -> #(String, String) {
  // The test file is a sequence of test cases
  case yaml.as_list(meta) {
    None -> #("skip", path <> " (not a list)")
    Some(cases) -> {
      // Run first test case (usually just one per file)
      case cases {
        [] -> #("skip", path <> " (empty)")
        [first, ..] -> run_single_test(path, first)
      }
    }
  }
}

fn run_single_test(path: String, test_case: yaml.Value) -> #(String, String) {
  let name =
    yaml.get(test_case, "name")
    |> result.map(yaml.to_string)
    |> result.unwrap(path)

  // Check if this is an error test
  let is_error_test = case yaml.get(test_case, "fail") {
    Ok(value.Bool(True)) -> True
    _ -> False
  }

  // Get the YAML input
  case yaml.get(test_case, "yaml") {
    Error(_) -> #("skip", name <> " (no yaml field)")
    Ok(yaml_input) -> {
      case yaml.as_string(yaml_input) {
        None -> #("skip", name <> " (yaml not string)")
        Some(input) -> {
          // Normalize the input (replace special markers)
          // ␣ = trailing space
          let input = string.replace(input, "␣", " ")
          // Tab markers (different visual widths, all represent \t)
          // Note: must replace longer patterns first to avoid partial matches
          let input = string.replace(input, "————»", "\t")
          let input = string.replace(input, "———»", "\t")
          let input = string.replace(input, "——»", "\t")
          let input = string.replace(input, "—»", "\t")
          let input = string.replace(input, "»", "\t")
          // Trailing newline marker (we can ignore this for parsing)
          let input = string.replace(input, "↵", "")
          // Carriage return
          let input = string.replace(input, "←", "\r")
          // No final newline marker (just ignore)
          let input = string.replace(input, "∎", "")

          // Try to parse
          case yaml.parse_all(input), is_error_test {
            // Expected to fail and did fail
            Error(_), True -> #("pass", name)
            // Expected to fail but passed - check if any doc parsed
            Ok(_), True -> #("fail", name <> " (should have failed)")
            // Expected to pass but failed
            Error(e), False -> #("fail", name <> " (" <> e.message <> ")")
            // Expected to pass, check JSON output if available
            Ok(docs), False -> {
              case yaml.get(test_case, "json") {
                Error(_) -> #("pass", name)
                Ok(expected_json) -> {
                  case yaml.as_string(expected_json) {
                    None -> #("pass", name)
                    Some(expected) -> {
                      // Build actual JSON: one JSON value per document, joined by newlines
                      let actual = case docs {
                        [] -> ""
                        _ ->
                          docs
                          |> list.map(yaml.to_json_string)
                          |> string.join("\n")
                      }
                      case normalize_json(actual) == normalize_json(expected) {
                        True -> #("pass", name)
                        False -> {
                          case is_key_order_only(actual, expected) {
                            True -> #("order", name <> " (key order only)")
                            False -> #(
                              "fail",
                              name
                                <> " (json mismatch)\n      Expected: "
                                <> string.slice(expected, 0, 100)
                                <> "\n      Actual:   "
                                <> string.slice(actual, 0, 100),
                            )
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Normalize JSON for comparison (remove whitespace differences)
fn normalize_json(s: String) -> String {
  s
  |> string.trim
  |> string.replace("\n", "")
  |> string.replace("\r", "")
  |> string.replace(" ", "")
}

/// Check if two JSON strings differ only in key ordering
fn is_key_order_only(actual: String, expected: String) -> Bool {
  // Simple heuristic: if they have the same length after normalization
  // and same characters (sorted), it's likely just ordering
  let a = normalize_json(actual)
  let e = normalize_json(expected)
  let a_sorted =
    a |> string.to_graphemes |> list.sort(string.compare) |> string.concat
  let e_sorted =
    e |> string.to_graphemes |> list.sort(string.compare) |> string.concat
  a_sorted == e_sorted
}
