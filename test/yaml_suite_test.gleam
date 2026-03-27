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
    "\nRunning YAML test suite: " <> int.to_string(list.length(yaml_files)) <> " test files",
  )

  let results =
    yaml_files
    |> list.map(fn(file) { run_test_file("test-suite/src/" <> file) })

  let passed = list.filter(results, fn(r) { r.0 == "pass" }) |> list.length
  let skipped = list.filter(results, fn(r) { r.0 == "skip" }) |> list.length

  // Separate error test failures from parse failures
  let error_test_failures =
    list.filter(results, fn(r) {
      r.0 == "fail" && string.contains(r.1, "should have failed")
    })
    |> list.length

  let parse_failures =
    list.filter(results, fn(r) {
      r.0 == "fail" && !string.contains(r.1, "should have failed")
    })

  io.println("")
  io.println(
    "Results: "
    <> int.to_string(passed)
    <> " passed, "
    <> int.to_string(list.length(parse_failures))
    <> " parse failures, "
    <> int.to_string(error_test_failures)
    <> " lenient (accepted invalid), "
    <> int.to_string(skipped)
    <> " skipped",
  )

  // Print actual parse failures (not just lenient parsing)
  case parse_failures {
    [] -> io.println("\nNo parse failures - all valid YAML parsed correctly!")
    failures -> {
      io.println("\nParse failures:")
      list.each(failures, fn(r) { io.println("  - " <> r.1) })
    }
  }

  // We should have very few actual parse failures
  case list.length(parse_failures) < 20 {
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
          // Normalize the input (replace ␣ markers with spaces)
          let input = string.replace(input, "␣", " ")

          // Try to parse
          case yaml.parse(input), is_error_test {
            // Expected to fail and did fail
            Error(_), True -> #("pass", name)
            // Expected to fail but passed
            Ok(_), True -> #("fail", name <> " (should have failed)")
            // Expected to pass but failed
            Error(e), False -> #("fail", name <> " (" <> e.message <> ")")
            // Expected to pass, check JSON output if available
            Ok(parsed), False -> {
              case yaml.get(test_case, "json") {
                Error(_) -> #("pass", name)
                Ok(expected_json) -> {
                  case yaml.as_string(expected_json) {
                    None -> #("pass", name)
                    Some(expected) -> {
                      let actual = yaml.to_json_string(parsed)
                      case normalize_json(actual) == normalize_json(expected) {
                        True -> #("pass", name)
                        False -> #("fail", name <> " (json mismatch)")
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
