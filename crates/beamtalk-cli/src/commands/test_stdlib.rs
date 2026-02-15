// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Stdlib test compiler: parse `// =>` assertions and generate `EUnit` tests.
//!
//! **DDD Context:** CLI / Test System
//!
//! Compiles `.bt` test files with `// =>` assertions into `EUnit` test modules.
//! Each expression is compiled through the normal pipeline, then wrapped in
//! an `EUnit` test that calls the compiled eval function and asserts the result.
//!
//! Part of ADR 0014 (Beamtalk Test Framework), Phase 1.

use crate::beam_compiler::BeamCompiler;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write as _;
use std::fs;
use tracing::{debug, info, instrument};

// ──────────────────────────────────────────────────────────────────────────
// Test file parsing (lifted from e2e.rs)
// ──────────────────────────────────────────────────────────────────────────

/// What a test assertion expects: a value or an error.
#[derive(Debug, Clone, PartialEq)]
enum Expected {
    /// Match formatted result string (`_` for wildcard).
    Value(String),
    /// Match `#beamtalk_error{kind = Kind}` on error.
    Error { kind: String },
}

/// A single test assertion: expression + expected result.
#[derive(Debug)]
struct TestCase {
    /// The Beamtalk expression to evaluate.
    expression: String,
    /// Expected outcome (value or error).
    expected: Expected,
    /// Line number in the source file (1-based).
    line: usize,
}

/// Parsed test file with metadata.
#[derive(Debug)]
struct ParsedTestFile {
    /// Files to load before running tests (from `// @load` directives).
    load_files: Vec<String>,
    /// Test cases to run.
    cases: Vec<TestCase>,
    /// Warnings about expressions without assertions.
    warnings: Vec<String>,
}

/// Parse test cases from a `.bt` file.
///
/// Extracts `// =>` assertion pairs and `// @load` directives.
fn parse_test_file(content: &str) -> ParsedTestFile {
    let mut cases = Vec::new();
    let mut load_files = Vec::new();
    let mut warnings = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Check for @load directive
        if let Some(path) = line.strip_prefix("// @load") {
            let path = path.trim();
            if !path.is_empty() {
                load_files.push(path.to_string());
            }
            i += 1;
            continue;
        }

        // Skip empty lines and standalone comments
        if line.is_empty() || (line.starts_with("//") && !line.starts_with("// =>")) {
            i += 1;
            continue;
        }

        // Skip orphaned assertion markers
        if line.starts_with("// =>") {
            i += 1;
            continue;
        }

        // This should be an expression
        let expression = line.to_string();
        let expr_line = i + 1;

        // Look for the expected result on the next line
        i += 1;
        if i < lines.len() {
            let next_line = lines[i].trim();
            if let Some(expected) = next_line.strip_prefix("// =>") {
                let expected = expected.trim();
                let expected = if let Some(kind) = expected.strip_prefix("ERROR:") {
                    let kind = kind.trim();
                    if kind.is_empty() {
                        warnings.push(format!(
                            "Line {expr_line}: Expression will not be executed \
                             (invalid // => ERROR: assertion with missing error kind): \
                             {expression}"
                        ));
                        i += 1;
                        continue;
                    }
                    Expected::Error {
                        kind: kind.to_string(),
                    }
                } else {
                    Expected::Value(expected.to_string())
                };
                cases.push(TestCase {
                    expression,
                    expected,
                    line: expr_line,
                });
                i += 1;
            } else {
                warnings.push(format!(
                    "Line {expr_line}: Expression will not be executed \
                     (missing // => assertion): {expression}"
                ));
            }
        } else {
            warnings.push(format!(
                "Line {expr_line}: Expression will not be executed \
                 (missing // => assertion): {expression}"
            ));
        }
    }

    ParsedTestFile {
        load_files,
        cases,
        warnings,
    }
}

// ──────────────────────────────────────────────────────────────────────────
// Core Erlang compilation for test expressions
// ──────────────────────────────────────────────────────────────────────────

/// Compile a single Beamtalk expression to a Core Erlang eval module.
///
/// Returns the Core Erlang source string for a module with `eval/1`.
pub(crate) fn compile_expression_to_core(
    expression: &str,
    module_name: &str,
) -> std::result::Result<String, String> {
    // Lex and parse the expression
    let tokens = beamtalk_core::source_analysis::lex_with_eof(expression);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Check for parse errors
    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        let msgs: Vec<String> = diagnostics
            .iter()
            .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
            .map(|d| d.message.to_string())
            .collect();
        return Err(msgs.join("; "));
    }

    // Get the first expression from the parsed module
    let expr = module
        .expressions
        .first()
        .ok_or_else(|| "No expression found in parsed source".to_string())?;

    // Generate Core Erlang test module (no workspace bindings)
    beamtalk_core::codegen::core_erlang::generate_test_expression(expr, module_name)
        .map_err(|e| format!("{e}"))
}

// ──────────────────────────────────────────────────────────────────────────
// EUnit wrapper generation
// ──────────────────────────────────────────────────────────────────────────

/// Returns the Erlang `format_result/1` and `matches_pattern/2` helper
/// functions shared by stdlib tests and doc tests.
pub(crate) fn eunit_helper_functions() -> &'static str {
    "format_result(V) when is_integer(V) -> integer_to_binary(V);\n\
     format_result(V) when is_float(V) ->\n\
     \x20   %% Match Erlang/REPL float formatting\n\
     \x20   list_to_binary(io_lib:format(\"~p\", [V]));\n\
     format_result(true) -> <<\"true\">>;\n\
     format_result(false) -> <<\"false\">>;\n\
     format_result(nil) -> <<\"nil\">>;\n\
     format_result(V) when is_atom(V) -> atom_to_binary(V, utf8);\n\
     format_result(V) when is_binary(V) -> V;\n\
     format_result(V) when is_function(V) ->\n\
     \x20   {arity, A} = erlang:fun_info(V, arity),\n\
     \x20   iolist_to_binary([<<\"a Block/\">>, integer_to_binary(A)]);\n\
     format_result(V) when is_pid(V) ->\n\
     \x20   S = pid_to_list(V),\n\
     \x20   I = lists:sublist(S, 2, length(S) - 2),\n\
     \x20   iolist_to_binary([<<\"#Actor<\">>, I, <<\">\">>]);\n\
     format_result(V) when is_tuple(V), tuple_size(V) >= 2, element(1, V) =:= beamtalk_object ->\n\
     \x20   %% BT-412: Match REPL formatting for class objects vs actor instances\n\
     \x20   Class = element(2, V),\n\
     \x20   case beamtalk_class_registry:is_class_name(Class) of\n\
     \x20       true -> beamtalk_class_registry:class_display_name(Class);\n\
     \x20       false ->\n\
     \x20           Pid = element(4, V),\n\
     \x20           ClassBin = atom_to_binary(Class, utf8),\n\
     \x20           PidStr = pid_to_list(Pid),\n\
     \x20           Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),\n\
     \x20           iolist_to_binary([<<\"#Actor<\">>, ClassBin, <<\",\">>, Inner, <<\">\">>])\n\
     \x20   end;\n\
     format_result(V) when is_map(V) ->\n\
     \x20   %% BT-535: Use print_string for Beamtalk display format\n\
     \x20   beamtalk_primitive:print_string(V);\n\
     format_result(V) when is_list(V) ->\n\
     \x20   case V of\n\
     \x20       [] -> <<\"[]\">>;\n\
     \x20       _ ->\n\
     \x20           try jsx:encode([beamtalk_repl_json:term_to_json(E) || E <- V])\n\
     \x20           catch _:_ -> iolist_to_binary(io_lib:format(\"~p\", [V])) end\n\
     \x20   end;\n\
     format_result(V) -> iolist_to_binary(io_lib:format(\"~p\", [V])).\n\n\
     %% BT-502: Glob-style pattern matching where _ matches any substring,\n\
     %% but only when _ is NOT flanked by alphanumeric characters on both sides.\n\
     %% This preserves literal underscores in identifiers like does_not_understand.\n\
     matches_pattern(Pattern, Actual) ->\n\
     \x20   Segments = wildcard_segments(Pattern),\n\
     \x20   matches_segments(Segments, Actual, 0, true).\n\
     wildcard_segments(Pattern) ->\n\
     \x20   Chars = binary_to_list(Pattern),\n\
     \x20   [list_to_binary(S) || S <- wildcard_split(Chars, [], [], none)].\n\
     wildcard_split([], CurRev, SegsRev, _Prev) ->\n\
     \x20   lists:reverse([lists:reverse(CurRev) | SegsRev]);\n\
     wildcard_split([$_ | Rest], CurRev, SegsRev, Prev) ->\n\
     \x20   Next = case Rest of [N | _] -> N; [] -> none end,\n\
     \x20   case is_alnum(Prev) andalso is_alnum(Next) of\n\
     \x20       true -> wildcard_split(Rest, [$_ | CurRev], SegsRev, $_);\n\
     \x20       false -> wildcard_split(Rest, [], [lists:reverse(CurRev) | SegsRev], $_)\n\
     \x20   end;\n\
     wildcard_split([C | Rest], CurRev, SegsRev, _Prev) ->\n\
     \x20   wildcard_split(Rest, [C | CurRev], SegsRev, C).\n\
     is_alnum(C) when is_integer(C), C >= $0, C =< $9 -> true;\n\
     is_alnum(C) when is_integer(C), C >= $A, C =< $Z -> true;\n\
     is_alnum(C) when is_integer(C), C >= $a, C =< $z -> true;\n\
     is_alnum(_) -> false.\n\
     matches_segments([], _Actual, _Pos, IsFirst) -> not IsFirst;\n\
     matches_segments([<<>> | Rest], Actual, Pos, _IsFirst) ->\n\
     \x20   matches_segments(Rest, Actual, Pos, false);\n\
     matches_segments([Seg | Rest], Actual, Pos, true) ->\n\
     \x20   %% First segment must match at start\n\
     \x20   case binary:match(Actual, Seg, [{scope, {Pos, byte_size(Actual) - Pos}}]) of\n\
     \x20       {0, Len} -> matches_segments(Rest, Actual, Len, false);\n\
     \x20       _ -> false\n\
     \x20   end;\n\
     matches_segments([Seg], Actual, Pos, false) ->\n\
     \x20   %% Last non-empty segment must match at end\n\
     \x20   SLen = byte_size(Seg),\n\
     \x20   ALen = byte_size(Actual),\n\
     \x20   Start = ALen - SLen,\n\
     \x20   Start >= Pos andalso binary:part(Actual, Start, SLen) =:= Seg;\n\
     matches_segments([Seg | Rest], Actual, Pos, false) ->\n\
     \x20   case binary:match(Actual, Seg, [{scope, {Pos, byte_size(Actual) - Pos}}]) of\n\
     \x20       {Found, Len} -> matches_segments(Rest, Actual, Found + Len, false);\n\
     \x20       nomatch -> false\n\
     \x20   end.\n\n"
}

/// Convert an expected value string to an Erlang binary literal for
/// string-based comparison.
///
/// All expected values are represented as binaries (`<<"">>`) since
/// `format_result/1` always returns a binary. This matches E2E semantics
/// where the REPL compares string representations.
pub(crate) fn expected_to_binary_literal(expected: &str) -> String {
    let escaped = expected.replace('\\', "\\\\").replace('"', "\\\"");
    // Use /utf8 type to correctly encode multi-byte Unicode characters (BT-388).
    // Without /utf8, Erlang truncates codepoints > 255 to a single byte.
    format!("<<\"{escaped}\"/utf8>>")
}

/// Extract the variable name from an assignment expression (`x := expr`).
///
/// Mirrors the REPL's `extract_assignment/1`: matches `name := ...` pattern.
/// Returns `Some(var_name)` if the expression is an assignment, `None` otherwise.
pub(crate) fn extract_assignment_var(expression: &str) -> Option<String> {
    let trimmed = expression.trim();
    // Find `:=` and check that everything before it is a valid identifier
    let assign_pos = trimmed.find(":=")?;
    let before = trimmed[..assign_pos].trim();
    // Validate it's a simple identifier (letters, digits, underscores, starts with letter/underscore)
    if before.is_empty() {
        return None;
    }
    let first = before.chars().next()?;
    if !first.is_ascii_alphabetic() && first != '_' {
        return None;
    }
    if before
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_')
    {
        Some(before.to_string())
    } else {
        None
    }
}

/// Generate an `EUnit` test module (.erl) for a parsed test file.
///
/// Creates a single test function that evaluates all expressions
/// sequentially, threading variable bindings between them.
///
/// Results are formatted to strings before comparison (matching E2E
/// semantics where the REPL returns string representations).
/// Generate the `EUnit` try/catch block for an ERROR: assertion.
pub(crate) fn write_error_assertion(
    erl: &mut String,
    i: usize,
    eval_mod: &str,
    kind: &str,
    bindings_in: &str,
    bindings_out: &str,
) {
    // Error assertion: wrap eval in try/catch, match on #beamtalk_error{kind}
    // Also handles plain atom errors (e.g., badarith, badarity)
    // Variables are suffixed with index to avoid Erlang scoping conflicts.
    // Assertions are outside the try/catch to avoid the catch clause
    // swallowing EUnit assertion exceptions.
    // The case uses CaughtKind/CaughtReason (fresh variables) since
    // Erlang considers variables bound in catch as unsafe outside try.
    let _ = writeln!(
        erl,
        "    {bindings_out} =\n\
         \x20   begin\n\
         \x20       TryResult{i} = try '{eval_mod}':eval({bindings_in}) of\n\
         \x20           {{_V{i}, B{i}}} ->\n\
         \x20               {{ok, B{i}}}\n\
         \x20       catch\n\
         \x20           error:#{{\'$beamtalk_class\' := _, error := {{beamtalk_error, Kind{i}, _, _, _, _, _}}}} ->\n\
         \x20               {{beamtalk_error, Kind{i}}};\n\
         \x20           error:{{beamtalk_error, Kind{i}, _, _, _, _, _}} ->\n\
         \x20               {{beamtalk_error, Kind{i}}};\n\
         \x20           throw:{{future_rejected, {{beamtalk_error, Kind{i}, _, _, _, _, _}}}} ->\n\
         \x20               {{beamtalk_error, Kind{i}}};\n\
         \x20           error:'{kind}' ->\n\
         \x20               atom_error;\n\
         \x20           error:Reason{i} ->\n\
         \x20               {{other_error, Reason{i}}}\n\
         \x20       end,\n\
         \x20       case TryResult{i} of\n\
         \x20           {{ok, _}} ->\n\
         \x20               ?assert(false, <<\"Expected error '{kind}' but expression succeeded\">>),\n\
         \x20               {bindings_in};\n\
         \x20           {{beamtalk_error, CaughtKind{i}}} ->\n\
         \x20               ?assertEqual('{kind}', CaughtKind{i}),\n\
         \x20               {bindings_in};\n\
         \x20           atom_error ->\n\
         \x20               {bindings_in};\n\
         \x20           {{other_error, CaughtReason{i}}} ->\n\
         \x20               ?assertEqual('{kind}', CaughtReason{i}),\n\
         \x20               {bindings_in}\n\
         \x20       end\n\
         \x20   end,"
    );
}

/// Check if a string contains `_` that should be treated as a wildcard.
///
/// A `_` is a wildcard if it is NOT flanked on both sides by alphanumeric
/// characters. This preserves literal underscores in identifiers like
/// `does_not_understand` or `beamtalk_class`.
pub(crate) fn has_wildcard_underscore(s: &str) -> bool {
    let bytes = s.as_bytes();
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'_' {
            let before_alnum = i > 0 && bytes[i - 1].is_ascii_alphanumeric();
            let after_alnum = i + 1 < bytes.len() && bytes[i + 1].is_ascii_alphanumeric();
            if !before_alnum || !after_alnum {
                return true;
            }
        }
    }
    false
}

fn generate_eunit_wrapper(
    test_module_name: &str,
    test_file_path: &str,
    cases: &[TestCase],
    eval_module_names: &[String],
) -> String {
    let mut erl = String::new();

    // Module header (quote the module atom for Erlang atom safety)
    let _ = write!(
        erl,
        "%% Generated from {test_file_path}\n\
         -module('{test_module_name}').\n\
         -include_lib(\"eunit/include/eunit.hrl\").\n\n"
    );

    // format_result/1 mirrors the REPL's term_to_json formatting so that
    // expected values written as strings (like E2E tests) match correctly.
    erl.push_str(eunit_helper_functions());

    // Single test function with all assertions (stateful test)
    let _ = writeln!(erl, "{test_module_name}_test() ->");

    // Initial empty bindings
    erl.push_str("    Bindings0 = #{},\n");

    for (i, (case, eval_mod)) in cases.iter().zip(eval_module_names.iter()).enumerate() {
        let bindings_in = format!("Bindings{i}");
        let bindings_out = format!("Bindings{}", i + 1);

        match &case.expected {
            Expected::Error { kind } => {
                write_error_assertion(&mut erl, i, eval_mod, kind, &bindings_in, &bindings_out);
            }
            Expected::Value(v) => {
                // Normal eval: call the module and extract result + bindings
                let result_var = format!("Result{i}");
                let raw_bindings = format!("RawBindings{}", i + 1);
                let _ = writeln!(
                    erl,
                    "    {{{result_var}, {raw_bindings}}} = '{eval_mod}':eval({bindings_in}),"
                );

                // Persist assignment bindings
                if let Some(var_name) = extract_assignment_var(&case.expression) {
                    let _ = writeln!(
                        erl,
                        "    {bindings_out} = maps:put('{var_name}', {result_var}, {raw_bindings}),"
                    );
                } else {
                    let _ = writeln!(erl, "    {bindings_out} = {raw_bindings},");
                }

                // Add value assertion (unless bare wildcard)
                if v == "_" {
                    // Bare wildcard: run but don't check result
                } else if has_wildcard_underscore(v) {
                    // Pattern with wildcards: use glob-style matching (BT-502)
                    let expected_bin = expected_to_binary_literal(v);
                    let _ = writeln!(
                        erl,
                        "    ?assert(matches_pattern({expected_bin}, format_result({result_var}))),"
                    );
                } else {
                    let expected_bin = expected_to_binary_literal(v);
                    let _ = writeln!(
                        erl,
                        "    ?assertEqual({expected_bin}, format_result({result_var})),"
                    );
                }
            }
        }
    }

    erl.push_str("    ok.\n");

    erl
}

// ──────────────────────────────────────────────────────────────────────────
// @load support
// ──────────────────────────────────────────────────────────────────────────

/// Compile a fixture file referenced by `@load` directive.
fn compile_fixture(fixture_path: &Utf8Path, output_dir: &Utf8Path) -> Result<()> {
    let stem = fixture_path
        .file_stem()
        .ok_or_else(|| miette::miette!("Fixture file has no name: {}", fixture_path))?;

    // ADR 0016: User code modules use bt@ prefix
    let module_name = format!(
        "bt@{}",
        beamtalk_core::codegen::core_erlang::to_module_name(stem)
    );

    let core_file = output_dir.join(format!("{module_name}.core"));

    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: false,
        allow_primitives: false,
        workspace_mode: false,
    };

    crate::beam_compiler::compile_source(fixture_path, &module_name, &core_file, &options)
        .wrap_err_with(|| format!("Failed to compile fixture '{fixture_path}'"))?;

    // Compile .core → .beam
    let compiler = BeamCompiler::new(output_dir.to_owned());
    compiler
        .compile_batch(&[core_file])
        .wrap_err_with(|| format!("Failed to compile fixture BEAM for '{fixture_path}'"))?;

    Ok(())
}

// ──────────────────────────────────────────────────────────────────────────
// Main entry point
// ──────────────────────────────────────────────────────────────────────────

/// Metadata for a compiled test file, ready to be run.
struct CompiledTestFile {
    /// Original source file path.
    source_file: Utf8PathBuf,
    /// `EUnit` module name (e.g., `arithmetic_tests`).
    module_name: String,
    /// Number of assertions in this file.
    assertion_count: usize,
}

/// Run stdlib tests.
///
/// Finds all `.bt` files in the test directory, parses `// =>` assertions,
/// compiles expressions to Core Erlang, generates `EUnit` wrappers, and runs
/// all tests in a single BEAM process.
#[instrument(skip_all)]
pub fn run_tests(path: &str) -> Result<()> {
    info!("Starting stdlib test run");

    let test_dir = Utf8PathBuf::from(path);
    if !test_dir.exists() {
        miette::bail!("Test directory '{}' not found", test_dir);
    }

    // Find all .bt test files
    let test_files = find_test_files(&test_dir)?;
    if test_files.is_empty() {
        println!("No .bt test files found in '{test_dir}'");
        return Ok(());
    }

    println!("Compiling {} test file(s)...", test_files.len());

    // Create temporary build directory
    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory")?;
    let build_dir = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;

    // Phase 1: Compile all test files (Core Erlang + EUnit wrappers)
    let mut compiled_files = Vec::new();
    let mut all_core_files = Vec::new();
    let mut all_erl_files = Vec::new();

    let mut all_fixture_modules = Vec::new();

    for test_file in &test_files {
        let result = compile_single_test_file(test_file, &build_dir)?;
        all_core_files.extend(result.core_files);
        all_erl_files.push(result.erl_file);
        all_fixture_modules.extend(result.fixture_modules);
        compiled_files.push(CompiledTestFile {
            source_file: test_file.clone(),
            module_name: result.test_module_name,
            assertion_count: result.test_count,
        });
    }

    // Deduplicate fixture modules
    all_fixture_modules.sort();
    all_fixture_modules.dedup();

    // Phase 2: Batch compile all .core → .beam
    if !all_core_files.is_empty() {
        let compiler = BeamCompiler::new(build_dir.clone());
        compiler
            .compile_batch(&all_core_files)
            .wrap_err("Failed to batch-compile test expression modules to BEAM")?;
    }

    // Phase 3: Batch compile all EUnit .erl → .beam in a single erlc call
    compile_erl_files(&all_erl_files, &build_dir)?;

    // Phase 4: Run ALL EUnit test modules in a single BEAM process
    let test_module_names: Vec<&str> = compiled_files
        .iter()
        .map(|f| f.module_name.as_str())
        .collect();

    let total_tests: usize = compiled_files.iter().map(|f| f.assertion_count).sum();

    let eunit_result = run_all_eunit_tests(&test_module_names, &all_fixture_modules, &build_dir)?;

    // Phase 5: Report results per file
    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut failed_details = Vec::new();

    for compiled in &compiled_files {
        let file_stem = compiled.source_file.file_stem().unwrap_or("unknown");

        if let Some(failure) = eunit_result.failed_modules.get(&compiled.module_name) {
            total_failed += compiled.assertion_count;
            println!(
                "  {file_stem}: {} tests, 0 passed ✗",
                compiled.assertion_count
            );
            failed_details.push(format!("FAIL {}:\n  {}", compiled.source_file, failure));
        } else {
            total_passed += compiled.assertion_count;
            println!(
                "  {file_stem}: {} tests, {} passed ✓",
                compiled.assertion_count, compiled.assertion_count
            );
        }
    }

    println!();
    if total_failed == 0 {
        println!(
            "{} file(s), {} tests, {} passed, 0 failed",
            test_files.len(),
            total_tests,
            total_passed
        );
    } else {
        for detail in &failed_details {
            eprintln!("{detail}");
        }
        eprintln!();
        eprintln!(
            "{} file(s), {} tests, {} passed, {} failed",
            test_files.len(),
            total_tests,
            total_passed,
            total_failed
        );
        miette::bail!("{total_failed} test(s) failed");
    }

    Ok(())
}

/// Result of compiling a single test file (no execution yet).
struct CompilationResult {
    /// `EUnit` test module name.
    test_module_name: String,
    /// Core Erlang files generated for this test file's expressions.
    core_files: Vec<Utf8PathBuf>,
    /// `EUnit` wrapper `.erl` file.
    erl_file: Utf8PathBuf,
    /// Number of test assertions.
    test_count: usize,
    /// Fixture module names from `@load` directives (need `code:ensure_loaded`).
    fixture_modules: Vec<String>,
}

/// Compile a single `.bt` test file into Core Erlang modules + `EUnit` wrapper.
///
/// Does NOT execute — just produces files ready for batch compilation and execution.
fn compile_single_test_file(
    test_file: &Utf8Path,
    build_dir: &Utf8Path,
) -> Result<CompilationResult> {
    let content = fs::read_to_string(test_file)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{test_file}'"))?;

    let parsed = parse_test_file(&content);

    // Treat warnings as errors (BT-249)
    if !parsed.warnings.is_empty() {
        for warning in &parsed.warnings {
            eprintln!("⚠️  {test_file}: {warning}");
        }
        miette::bail!(
            "{} has {} expression(s) without assertions",
            test_file,
            parsed.warnings.len()
        );
    }

    // Compile @load fixtures and collect module names
    let mut fixture_modules = Vec::new();
    for load_path in &parsed.load_files {
        let fixture_path = Utf8PathBuf::from(load_path);
        if !fixture_path.exists() {
            miette::bail!(
                "Fixture file '{}' referenced by @load in '{}' not found",
                load_path,
                test_file
            );
        }
        compile_fixture(&fixture_path, build_dir)?;
        // Track fixture module name for code:ensure_loaded at runtime
        if let Some(stem) = fixture_path.file_stem() {
            let module_name = format!(
                "bt@{}",
                beamtalk_core::codegen::core_erlang::to_module_name(stem)
            );
            fixture_modules.push(module_name);
        }
    }

    let file_stem = test_file
        .file_stem()
        .ok_or_else(|| miette::miette!("Test file has no name: {}", test_file))?;

    // Sanitize stem for Erlang module name
    let safe_stem: String = file_stem
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect();

    // Compile each expression to a Core Erlang eval module
    let mut eval_module_names = Vec::new();
    let mut core_files = Vec::new();

    for (i, case) in parsed.cases.iter().enumerate() {
        let module_name = format!("test_{safe_stem}_{i}");
        match compile_expression_to_core(&case.expression, &module_name) {
            Ok(core_erlang) => {
                let core_file = build_dir.join(format!("{module_name}.core"));
                fs::write(&core_file, core_erlang)
                    .into_diagnostic()
                    .wrap_err_with(|| {
                        format!(
                            "Failed to write Core Erlang for {}:{}",
                            test_file, case.line
                        )
                    })?;
                core_files.push(core_file);
                eval_module_names.push(module_name);
            }
            Err(err) => {
                miette::bail!(
                    "Failed to compile expression at {}:{}: {}\n  Expression: {}",
                    test_file,
                    case.line,
                    err,
                    case.expression
                );
            }
        }
    }

    // Generate EUnit wrapper
    let test_module_name = format!("{safe_stem}_tests");
    let eunit_source = generate_eunit_wrapper(
        &test_module_name,
        test_file.as_str(),
        &parsed.cases,
        &eval_module_names,
    );

    let erl_file = build_dir.join(format!("{test_module_name}.erl"));
    fs::write(&erl_file, &eunit_source)
        .into_diagnostic()
        .wrap_err("Failed to write EUnit wrapper")?;

    Ok(CompilationResult {
        test_module_name,
        core_files,
        erl_file,
        test_count: parsed.cases.len(),
        fixture_modules,
    })
}

/// Compile Erlang source files with erlc (batch).
fn compile_erl_files(erl_files: &[Utf8PathBuf], output_dir: &Utf8Path) -> Result<()> {
    if erl_files.is_empty() {
        return Ok(());
    }
    debug!("Batch compiling {} EUnit wrappers", erl_files.len());

    let mut cmd = std::process::Command::new("erlc");
    cmd.arg("-o").arg(output_dir.as_str());
    for erl_file in erl_files {
        cmd.arg(erl_file.as_str());
    }

    let output = cmd
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erlc")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        miette::bail!("erlc batch compilation failed:\n{}{}", stdout, stderr);
    }

    Ok(())
}

/// Result of running all `EUnit` tests in a single BEAM process.
struct EunitBatchResult {
    /// Map of module name → failure message for modules that failed.
    failed_modules: std::collections::HashMap<String, String>,
}

/// Run all `EUnit` test modules in a single BEAM process.
///
/// This avoids the ~3s BEAM startup overhead per test file by running
/// all modules in one `erl` invocation.
/// Build cover instrumentation preamble and epilogue for the eval command.
fn cover_fragments(
    beam_paths: &beamtalk_cli::repl_startup::BeamPaths,
    runtime_dir: &std::path::Path,
) -> (String, String) {
    let cover_enabled = std::env::var("STDLIB_COVER").is_ok();
    if !cover_enabled {
        return (String::new(), String::new());
    }

    let cover_export_path = runtime_dir.join("_build/test/cover/stdlib.coverdata");
    let cover_dir = runtime_dir.join("_build/test/cover");
    let _ = std::fs::create_dir_all(&cover_dir);
    info!(
        "Cover mode enabled, will export to {}",
        cover_export_path.display()
    );

    let preamble = format!(
        "cover:start(), \
         cover:compile_beam_directory(\"{runtime_ebin}\"), \
         cover:compile_beam_directory(\"{workspace_ebin}\"), ",
        runtime_ebin = beam_paths.runtime_ebin.display(),
        workspace_ebin = beam_paths.workspace_ebin.display(),
    );
    let epilogue = format!(
        "cover:export(\"{export}\"), cover:stop(), ",
        export = cover_export_path.display(),
    );
    (preamble, epilogue)
}

fn run_all_eunit_tests(
    test_module_names: &[&str],
    fixture_modules: &[String],
    build_dir: &Utf8Path,
) -> Result<EunitBatchResult> {
    debug!(
        "Running {} EUnit modules in single process",
        test_module_names.len()
    );

    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()
        .wrap_err("Cannot find Erlang runtime directory")?;
    let beam_paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    let pa_args = beamtalk_cli::repl_startup::beam_pa_args(&beam_paths);

    // Build Erlang expression that runs each module and collects failures
    let module_list: String = test_module_names
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    let (cover_preamble, cover_epilogue) = cover_fragments(&beam_paths, &runtime_dir);

    // Build fixture loading commands (ensure_loaded triggers on_load → class registration)
    let fixture_load_cmd = if fixture_modules.is_empty() {
        String::new()
    } else {
        fixture_modules
            .iter()
            .map(|m| format!("code:ensure_loaded('{m}')"))
            .collect::<Vec<_>>()
            .join(", ")
            + ", "
    };

    let eval_cmd = format!(
        "{cover_preamble}\
         beamtalk_extensions:init(), \
         pg:start_link(), \
         beamtalk_bootstrap:start_link(), \
         beamtalk_stdlib:init(), \
         {fixture_load_cmd}\
         Modules = [{module_list}], \
         Failed = lists:foldl(fun(M, Acc) -> \
           case eunit:test(M, []) of \
             ok -> Acc; \
             error -> [M | Acc] \
           end \
         end, [], Modules), \
         {cover_epilogue}\
         case Failed of \
           [] -> init:stop(0); \
           _ -> \
             lists:foreach(fun(M) -> \
               io:format(\"FAILED_MODULE:~s~n\", [M]) \
             end, Failed), \
             init:stop(1) \
         end."
    );

    let mut cmd = std::process::Command::new("erl");
    cmd.arg("-noshell").arg("-pa").arg(build_dir.as_str());

    for arg in &pa_args {
        cmd.arg(arg);
    }

    cmd.arg("-eval").arg(&eval_cmd);

    let output = cmd
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run EUnit tests")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    debug!("EUnit stdout: {}", stdout);
    debug!("EUnit stderr: {}", stderr);

    let mut failed_modules = std::collections::HashMap::new();

    if !output.status.success() {
        // Parse which modules failed from our FAILED_MODULE: markers
        let combined = format!("{stdout}\n{stderr}");
        for line in combined.lines() {
            if let Some(module_name) = line.strip_prefix("FAILED_MODULE:") {
                let module_name = module_name.trim().to_string();
                // Collect any EUnit failure details
                let details = combined
                    .lines()
                    .filter(|l| {
                        l.contains(&module_name)
                            || l.contains("Failed")
                            || l.contains("failed")
                            || l.contains("assertEqual")
                            || l.contains("expected")
                            || l.contains("got")
                    })
                    .map(|l| format!("    {l}"))
                    .collect::<Vec<_>>()
                    .join("\n");
                failed_modules.insert(module_name, details);
            }
        }

        // If no FAILED_MODULE markers found but process failed, mark all as failed
        if failed_modules.is_empty() {
            let detail = format!("EUnit process failed:\n{combined}");
            for name in test_module_names {
                failed_modules.insert(name.to_string(), detail.clone());
            }
        }
    }

    Ok(EunitBatchResult { failed_modules })
}

/// Find all `.bt` files in the test directory.
fn find_test_files(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();

    for entry in fs::read_dir(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", dir))?;

        if path.extension() == Some("bt") {
            files.push(path);
        }
    }

    files.sort();
    Ok(files)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty_file() {
        let parsed = parse_test_file("");
        assert!(parsed.cases.is_empty());
        assert!(parsed.load_files.is_empty());
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_simple_assertion() {
        let content = "1 + 2\n// => 3\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expression, "1 + 2");
        assert_eq!(parsed.cases[0].expected, Expected::Value("3".to_string()));
        assert_eq!(parsed.cases[0].line, 1);
    }

    #[test]
    fn test_parse_wildcard_assertion() {
        let content = "Counter spawn\n// => _\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expected, Expected::Value("_".to_string()));
    }

    #[test]
    fn test_parse_load_directive() {
        let content = "// @load tests/fixtures/counter.bt\n1 + 2\n// => 3\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.load_files, vec!["tests/fixtures/counter.bt"]);
        assert_eq!(parsed.cases.len(), 1);
    }

    #[test]
    fn test_parse_missing_assertion_warning() {
        let content = "1 + 2\n3 + 4\n// => 7\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.warnings.len(), 1);
        assert!(parsed.warnings[0].contains("Line 1"));
    }

    #[test]
    fn test_parse_comments_and_blank_lines() {
        let content = "// A comment\n\n// Another comment\n1 + 2\n// => 3\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_expected_to_binary_literal() {
        assert_eq!(expected_to_binary_literal("42"), "<<\"42\"/utf8>>");
        assert_eq!(expected_to_binary_literal("-5"), "<<\"-5\"/utf8>>");
        assert_eq!(expected_to_binary_literal("0"), "<<\"0\"/utf8>>");
        assert_eq!(expected_to_binary_literal("3.14"), "<<\"3.14\"/utf8>>");
        assert_eq!(expected_to_binary_literal("-2.5"), "<<\"-2.5\"/utf8>>");
        assert_eq!(expected_to_binary_literal("true"), "<<\"true\"/utf8>>");
        assert_eq!(expected_to_binary_literal("false"), "<<\"false\"/utf8>>");
        assert_eq!(expected_to_binary_literal("nil"), "<<\"nil\"/utf8>>");
        assert_eq!(expected_to_binary_literal("hello"), "<<\"hello\"/utf8>>");
        assert_eq!(
            expected_to_binary_literal("hello world"),
            "<<\"hello world\"/utf8>>"
        );
        // BT-388: Unicode characters must be encoded correctly
        assert_eq!(expected_to_binary_literal("世界"), "<<\"世界\"/utf8>>");
        assert_eq!(
            expected_to_binary_literal("Hello 🌍"),
            "<<\"Hello 🌍\"/utf8>>"
        );
    }

    #[test]
    fn test_extract_assignment_var() {
        assert_eq!(extract_assignment_var("x := 5"), Some("x".to_string()));
        assert_eq!(
            extract_assignment_var("counter := Counter spawn"),
            Some("counter".to_string())
        );
        assert_eq!(extract_assignment_var("42"), None);
        assert_eq!(extract_assignment_var("x + y"), None);
        assert_eq!(extract_assignment_var("self.x := 5"), None);
    }

    #[test]
    fn test_generate_eunit_wrapper_simple() {
        let cases = vec![TestCase {
            expression: "1 + 2".to_string(),
            expected: Expected::Value("3".to_string()),
            line: 1,
        }];
        let eval_modules = vec!["test_arith_0".to_string()];
        let wrapper = generate_eunit_wrapper("arith_tests", "test/arith.bt", &cases, &eval_modules);
        assert!(wrapper.contains("-module('arith_tests')."));
        assert!(wrapper.contains("?assertEqual"));
        assert!(wrapper.contains("test_arith_0"));
    }

    #[test]
    fn test_generate_eunit_wrapper_wildcard() {
        let cases = vec![TestCase {
            expression: "Counter spawn".to_string(),
            expected: Expected::Value("_".to_string()),
            line: 1,
        }];
        let eval_modules = vec!["test_spawn_0".to_string()];
        let wrapper = generate_eunit_wrapper("spawn_tests", "test/spawn.bt", &cases, &eval_modules);
        assert!(wrapper.contains("test_spawn_0"));
        // Bare wildcard should not generate assertEqual or assert(matches_pattern(...))
        assert!(!wrapper.contains("assertEqual"));
        assert!(!wrapper.contains("?assert(matches_pattern"));
    }

    #[test]
    fn test_generate_eunit_wrapper_pattern() {
        let cases = vec![TestCase {
            expression: "Counter spawn".to_string(),
            expected: Expected::Value("#Actor<Counter,_>".to_string()),
            line: 1,
        }];
        let eval_modules = vec!["test_spawn_0".to_string()];
        let wrapper = generate_eunit_wrapper("spawn_tests", "test/spawn.bt", &cases, &eval_modules);
        assert!(wrapper.contains("matches_pattern"));
        assert!(!wrapper.contains("assertEqual"));
    }

    #[test]
    fn test_compile_expression_to_core_simple() {
        let result = compile_expression_to_core("1 + 2", "test_simple_0");
        assert!(result.is_ok(), "Failed: {:?}", result.err());
        let core = result.unwrap();
        assert!(core.contains("module 'test_simple_0'"));
        assert!(core.contains("eval"));
    }

    #[test]
    fn test_compile_expression_to_core_invalid() {
        let result = compile_expression_to_core("", "test_invalid_0");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_assertion() {
        let content = "42 foo\n// => ERROR: does_not_understand\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expression, "42 foo");
        assert_eq!(
            parsed.cases[0].expected,
            Expected::Error {
                kind: "does_not_understand".to_string()
            }
        );
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_error_assertion_type_error() {
        let content = "\"hello\" + 42\n// => ERROR: type_error\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(
            parsed.cases[0].expected,
            Expected::Error {
                kind: "type_error".to_string()
            }
        );
    }

    #[test]
    fn test_parse_mixed_value_and_error_assertions() {
        let content = "1 + 2\n// => 3\n42 foo\n// => ERROR: does_not_understand\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 2);
        assert_eq!(parsed.cases[0].expected, Expected::Value("3".to_string()));
        assert_eq!(
            parsed.cases[1].expected,
            Expected::Error {
                kind: "does_not_understand".to_string()
            }
        );
    }

    #[test]
    fn test_generate_eunit_wrapper_error() {
        let cases = vec![TestCase {
            expression: "42 foo".to_string(),
            expected: Expected::Error {
                kind: "does_not_understand".to_string(),
            },
            line: 1,
        }];
        let eval_modules = vec!["test_err_0".to_string()];
        let wrapper = generate_eunit_wrapper("err_tests", "test/err.bt", &cases, &eval_modules);
        assert!(wrapper.contains("try 'test_err_0':eval("));
        assert!(wrapper.contains("beamtalk_error"));
        assert!(wrapper.contains("does_not_understand"));
        // Error assertions should not call format_result in the assertion
        assert!(!wrapper.contains("format_result(Result"));
        // Assertions must be outside try/catch (not caught by own catch clause)
        assert!(wrapper.contains("case TryResult0 of"));
        assert!(wrapper.contains("{ok, _}"));
        assert!(wrapper.contains("CaughtKind0"));
        // Error wrapper catches future_rejected throws (actor errors via await)
        assert!(wrapper.contains("throw:{future_rejected, {beamtalk_error,"));
    }

    #[test]
    fn test_generate_eunit_wrapper_mixed() {
        let cases = vec![
            TestCase {
                expression: "1 + 2".to_string(),
                expected: Expected::Value("3".to_string()),
                line: 1,
            },
            TestCase {
                expression: "42 foo".to_string(),
                expected: Expected::Error {
                    kind: "does_not_understand".to_string(),
                },
                line: 3,
            },
        ];
        let eval_modules = vec!["test_mix_0".to_string(), "test_mix_1".to_string()];
        let wrapper = generate_eunit_wrapper("mix_tests", "test/mix.bt", &cases, &eval_modules);
        // First case: normal value assertion
        assert!(wrapper.contains("format_result(Result0)"));
        // Second case: error try/catch
        assert!(wrapper.contains("try 'test_mix_1':eval("));
    }

    #[test]
    fn test_parse_error_assertion_empty_kind() {
        let content = "42 foo\n// => ERROR:\n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 0);
        assert_eq!(parsed.warnings.len(), 1);
        assert!(parsed.warnings[0].contains("missing error kind"));
    }

    #[test]
    fn test_parse_error_assertion_whitespace_only_kind() {
        let content = "42 foo\n// => ERROR:   \n";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 0);
        assert_eq!(parsed.warnings.len(), 1);
        assert!(parsed.warnings[0].contains("missing error kind"));
    }
}
