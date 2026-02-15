// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `BUnit` test runner: discover `TestCase` subclasses and run tests via `EUnit`.
//!
//! **DDD Context:** CLI / Test System
//!
//! Discovers `.bt` files containing `TestCase subclass:` definitions,
//! compiles them through the normal pipeline, generates `EUnit` wrapper
//! modules, and runs tests with formatted output.
//!
//! Also discovers doc tests (`// =>` assertions in `///` doc comments)
//! and generates synthetic test modules for them.
//!
//! Part of ADR 0014 (Beamtalk Test Framework), Phase 2.

use crate::beam_compiler::BeamCompiler;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::HashMap;
use std::fmt::Write as _;
use std::fs;
use std::time::Instant;
use tracing::{debug, info, instrument, warn};

// ──────────────────────────────────────────────────────────────────────────
// Test discovery from AST
// ──────────────────────────────────────────────────────────────────────────

/// Metadata about a `TestCase` subclass discovered in a `.bt` file.
#[derive(Debug)]
struct TestCaseClass {
    /// Class name (e.g., `CounterTest`).
    class_name: String,
    /// Superclass name (e.g., `TestCase`).
    #[allow(dead_code)] // populated during discovery for future diagnostic use
    superclass_name: String,
    /// Module name for the compiled BEAM module (e.g., `bt@counter_test`).
    module_name: String,
    /// Methods whose names start with `test`.
    test_methods: Vec<String>,
    /// Whether a `setUp` method is defined.
    has_setup: bool,
    /// Whether a `tearDown` method is defined.
    has_teardown: bool,
}

/// Discover `TestCase` subclasses in a `.bt` file by parsing the AST.
///
/// Returns discovered test classes and any `@load` directives.
fn discover_test_classes(source_path: &Utf8Path) -> Result<(Vec<TestCaseClass>, Vec<String>)> {
    let content = fs::read_to_string(source_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{source_path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&content);
    let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let mut test_classes = Vec::new();

    // Extract @load directives from comments
    let load_files: Vec<String> = content
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            trimmed
                .strip_prefix("// @load ")
                .map(|path| path.trim().to_string())
        })
        .filter(|p| !p.is_empty())
        .collect();

    // Compiler only generates one module per .bt file (uses first class).
    // Warn if multiple TestCase subclasses are found and take only the first.
    let test_case_classes: Vec<_> = module
        .classes
        .iter()
        .filter(|c| c.superclass_name() == "TestCase")
        .collect();

    if test_case_classes.len() > 1 {
        warn!(
            "File '{}' contains {} TestCase subclasses; only the first will be compiled. \
             Split into separate files.",
            source_path,
            test_case_classes.len()
        );
    }

    for class in test_case_classes.into_iter().take(1) {
        let class_name = class.name.name.to_string();
        let stem = beamtalk_core::codegen::core_erlang::to_module_name(&class_name);
        let module_name = format!("bt@{stem}");

        let mut test_methods = Vec::new();
        let mut has_setup = false;
        let mut has_teardown = false;

        for method in &class.methods {
            let selector_name = method.selector.name();
            if selector_name.starts_with("test") {
                test_methods.push(selector_name.to_string());
            } else if selector_name == "setUp" {
                has_setup = true;
            } else if selector_name == "tearDown" {
                has_teardown = true;
            }
        }

        if !test_methods.is_empty() {
            test_classes.push(TestCaseClass {
                class_name,
                superclass_name: class.superclass_name().to_string(),
                module_name,
                test_methods,
                has_setup,
                has_teardown,
            });
        }
    }

    Ok((test_classes, load_files))
}

// ──────────────────────────────────────────────────────────────────────────
// EUnit wrapper generation
// ──────────────────────────────────────────────────────────────────────────

/// Generate an `EUnit` test wrapper module for a `TestCase` subclass.
///
/// Each `test*` method becomes an `EUnit` test function:
/// 1. Create fresh instance via `Module:new()`
/// 2. Call `Module:dispatch(setUp, [], Instance)` if defined
/// 3. Call `Module:dispatch(testMethod, [], Instance)`
/// 4. Call `Module:dispatch(tearDown, [], Instance)` if defined
fn generate_eunit_wrapper(test_class: &TestCaseClass, source_file: &str) -> String {
    let eunit_module = format!("{}_tests", test_class.module_name);
    let bt_module = &test_class.module_name;

    let mut erl = String::new();

    let _ = write!(
        erl,
        "%% Generated from {source_file} ({class})\n\
         -module('{eunit_module}').\n\
         -include_lib(\"eunit/include/eunit.hrl\").\n\n",
        class = test_class.class_name,
    );

    for method_name in &test_class.test_methods {
        // EUnit test function name: method_name + _test
        // Erlang atom-safe: quote with single quotes
        let _ = writeln!(erl, "'{method_name}_test'() ->");

        // Create fresh instance
        let _ = writeln!(erl, "    Instance = '{bt_module}':new(),");

        // setUp (if defined)
        if test_class.has_setup {
            let _ = writeln!(erl, "    '{bt_module}':dispatch('setUp', [], Instance),");
        }

        // Run test method inside try/after for tearDown
        if test_class.has_teardown {
            let _ = writeln!(erl, "    try");
            let _ = writeln!(
                erl,
                "        '{bt_module}':dispatch('{method_name}', [], Instance)"
            );
            let _ = writeln!(erl, "    after");
            let _ = writeln!(
                erl,
                "        '{bt_module}':dispatch('tearDown', [], Instance)"
            );
            let _ = writeln!(erl, "    end.");
        } else {
            let _ = writeln!(
                erl,
                "    '{bt_module}':dispatch('{method_name}', [], Instance)."
            );
        }

        erl.push('\n');
    }

    erl
}

// ──────────────────────────────────────────────────────────────────────────
// Fixture compilation
// ──────────────────────────────────────────────────────────────────────────

/// Compile a fixture file referenced by `@load` directive.
fn compile_fixture(fixture_path: &Utf8Path, output_dir: &Utf8Path) -> Result<String> {
    let module_name = fixture_module_name(fixture_path)?;

    let core_file = output_dir.join(format!("{module_name}.core"));

    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: false,
        allow_primitives: false,
        workspace_mode: false,
    };

    crate::beam_compiler::compile_source(fixture_path, &module_name, &core_file, &options)
        .wrap_err_with(|| format!("Failed to compile fixture '{fixture_path}'"))?;

    let compiler = BeamCompiler::new(output_dir.to_owned());
    compiler
        .compile_batch(&[core_file])
        .wrap_err_with(|| format!("Failed to compile fixture BEAM for '{fixture_path}'"))?;

    Ok(module_name)
}

fn fixture_module_name(fixture_path: &Utf8Path) -> Result<String> {
    let stem = fixture_path
        .file_stem()
        .ok_or_else(|| miette::miette!("Fixture file has no name: {}", fixture_path))?;

    Ok(format!(
        "bt@{}",
        beamtalk_core::codegen::core_erlang::to_module_name(stem)
    ))
}

// ──────────────────────────────────────────────────────────────────────────
// Test file compilation
// ──────────────────────────────────────────────────────────────────────────

/// Compile a `.bt` test file through the normal pipeline.
fn compile_test_file(
    source_path: &Utf8Path,
    module_name: &str,
    output_dir: &Utf8Path,
) -> Result<()> {
    let core_file = output_dir.join(format!("{module_name}.core"));

    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: false,
        allow_primitives: false,
        workspace_mode: false,
    };

    crate::beam_compiler::compile_source(source_path, module_name, &core_file, &options)
        .wrap_err_with(|| format!("Failed to compile test file '{source_path}'"))?;

    let compiler = BeamCompiler::new(output_dir.to_owned());
    compiler
        .compile_batch(&[core_file])
        .wrap_err_with(|| format!("Failed to compile BEAM for '{source_path}'"))?;

    Ok(())
}

/// Compile `EUnit` wrapper `.erl` files with erlc.
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
        miette::bail!("erlc compilation failed:\n{}{}", stdout, stderr);
    }

    Ok(())
}

// ──────────────────────────────────────────────────────────────────────────
// EUnit runner
// ──────────────────────────────────────────────────────────────────────────

/// Result from running `EUnit` tests.
struct EunitResult {
    /// Map of module name → failure details for modules that failed.
    failed_modules: std::collections::HashMap<String, String>,
}

/// Run all `EUnit` test wrapper modules in a single BEAM process.
fn run_eunit_tests(
    test_module_names: &[&str],
    fixture_modules: &[String],
    build_dir: &Utf8Path,
) -> Result<EunitResult> {
    debug!(
        "Running {} EUnit modules in single process",
        test_module_names.len()
    );

    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()
        .wrap_err("Cannot find Erlang runtime directory")?;
    let beam_paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    let pa_args = beamtalk_cli::repl_startup::beam_pa_args(&beam_paths);

    let module_list: String = test_module_names
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    // Build fixture loading commands
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
        "beamtalk_extensions:init(), \
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
         case Failed of \
           [] -> init:stop(0); \
           _ -> \
             lists:foreach(fun(M) -> \
               io:format(\"FAILED_MODULE:~s~n\", [atom_to_list(M)]) \
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
        let combined = format!("{stdout}\n{stderr}");
        for line in combined.lines() {
            if let Some(module_name) = line.strip_prefix("FAILED_MODULE:") {
                let module_name = module_name.trim().to_string();
                let details = combined
                    .lines()
                    .filter(|l| {
                        l.contains(&module_name)
                            || l.contains("Failed")
                            || l.contains("failed")
                            || l.contains("assertEqual")
                            || l.contains("expected")
                            || l.contains("assertion_failed")
                            || l.contains("got")
                    })
                    .map(|l| format!("    {l}"))
                    .collect::<Vec<_>>()
                    .join("\n");
                failed_modules.insert(module_name, details);
            }
        }

        if failed_modules.is_empty() {
            let detail = format!("EUnit process failed:\n{combined}");
            for name in test_module_names {
                failed_modules.insert(name.to_string(), detail.clone());
            }
        }
    }

    Ok(EunitResult { failed_modules })
}

// ──────────────────────────────────────────────────────────────────────────
// File discovery
// ──────────────────────────────────────────────────────────────────────────

/// Find all `.bt` files in a directory.
fn find_test_files(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();

    find_test_files_recursive(dir, &mut files)?;

    files.sort();
    Ok(files)
}

fn find_test_files_recursive(dir: &Utf8Path, files: &mut Vec<Utf8PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", dir))?;

        if path.is_dir() {
            find_test_files_recursive(&path, files)?;
        } else if path.extension() == Some("bt") {
            files.push(path);
        }
    }

    Ok(())
}

// ──────────────────────────────────────────────────────────────────────────
// Doc test discovery and compilation
// ──────────────────────────────────────────────────────────────────────────

/// Discover and compile doc tests from a source file.
///
/// Parses the file's AST to find `///` doc comments with `// =>` assertions
/// inside ` ```beamtalk ` code blocks, compiles each expression to Core Erlang,
/// and generates an `EUnit` wrapper module per class.
fn discover_and_compile_doc_tests(
    source_path: &Utf8Path,
    build_dir: &Utf8Path,
) -> Result<Vec<CompiledDocTestResult>> {
    let content = fs::read_to_string(source_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{source_path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&content);
    let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let doc_tests = super::doc_tests::extract_doc_tests(&module, &content);
    if doc_tests.is_empty() {
        return Ok(Vec::new());
    }

    // Compile the containing source file so its classes are available to doc tests
    compile_fixture(source_path, build_dir)
        .wrap_err_with(|| format!("Failed to compile source file for doc tests '{source_path}'"))?;

    let mut results = Vec::new();

    for class_doc in doc_tests {
        let all_cases: Vec<&super::doc_tests::DocTestCase> =
            class_doc.groups.iter().flat_map(|g| &g.cases).collect();

        if all_cases.is_empty() {
            continue;
        }

        let class_slug = beamtalk_core::codegen::core_erlang::to_module_name(&class_doc.class_name);
        let eunit_module = format!("{class_slug}_doc_tests");

        // Compile each expression to a Core Erlang eval module
        let mut eval_module_names = Vec::new();
        let mut core_files = Vec::new();

        for (i, case) in all_cases.iter().enumerate() {
            let eval_mod = format!("doctest_{class_slug}_{i}");
            match super::test_stdlib::compile_expression_to_core(&case.expression, &eval_mod) {
                Ok(core_erlang) => {
                    let core_file = build_dir.join(format!("{eval_mod}.core"));
                    fs::write(&core_file, core_erlang)
                        .into_diagnostic()
                        .wrap_err_with(|| {
                            format!(
                                "Failed to write Core Erlang for doc test {}:{} ({})",
                                source_path, case.source_line, case.expression
                            )
                        })?;
                    core_files.push(core_file);
                    eval_module_names.push(eval_mod);
                }
                Err(err) => {
                    miette::bail!(
                        "Failed to compile doc test at {}:{}: {}\n  Expression: {}",
                        source_path,
                        case.source_line,
                        err,
                        case.expression
                    );
                }
            }
        }

        // Batch compile Core Erlang files to BEAM
        if !core_files.is_empty() {
            let compiler = BeamCompiler::new(build_dir.to_owned());
            compiler.compile_batch(&core_files).wrap_err_with(|| {
                format!(
                    "Failed to compile doc test BEAM for '{}' class '{}'",
                    source_path, class_doc.class_name
                )
            })?;
        }

        // Generate EUnit wrapper
        let erl_source = generate_doc_test_eunit_wrapper(
            &eunit_module,
            source_path.as_str(),
            &class_doc.class_name,
            &all_cases,
            &eval_module_names,
        );

        let erl_file = build_dir.join(format!("{eunit_module}.erl"));
        fs::write(&erl_file, &erl_source)
            .into_diagnostic()
            .wrap_err("Failed to write doc test EUnit wrapper")?;

        results.push(CompiledDocTestResult {
            display_name: format!("{}DocTest", class_doc.class_name),
            eunit_module,
            erl_file,
            assertion_count: all_cases.len(),
        });
    }

    Ok(results)
}

/// Intermediate result from compiling doc tests for a single class.
struct CompiledDocTestResult {
    display_name: String,
    eunit_module: String,
    erl_file: Utf8PathBuf,
    assertion_count: usize,
}

/// Generate an `EUnit` wrapper module for doc test assertions.
///
/// Uses the same `format_result/1` + `matches_pattern/2` helpers as
/// stdlib tests for consistent result formatting and comparison.
fn generate_doc_test_eunit_wrapper(
    module_name: &str,
    source_file: &str,
    class_name: &str,
    cases: &[&super::doc_tests::DocTestCase],
    eval_module_names: &[String],
) -> String {
    use super::doc_tests::Expected;
    use super::test_stdlib::{
        expected_to_binary_literal, extract_assignment_var, has_wildcard_underscore,
        write_error_assertion,
    };

    let mut erl = String::new();

    let _ = write!(
        erl,
        "%% Doc tests from {source_file} ({class_name})\n\
         -module('{module_name}').\n\
         -include_lib(\"eunit/include/eunit.hrl\").\n\n"
    );

    // Shared Erlang helpers
    erl.push_str(super::test_stdlib::eunit_helper_functions());

    // Single test function with all assertions (stateful: bindings persist)
    let _ = writeln!(erl, "'{module_name}_test'() ->");
    erl.push_str("    Bindings0 = #{},\n");

    for (i, (case, eval_mod)) in cases.iter().zip(eval_module_names.iter()).enumerate() {
        let bindings_in = format!("Bindings{i}");
        let bindings_out = format!("Bindings{}", i + 1);

        match &case.expected {
            Expected::Error { kind } => {
                write_error_assertion(&mut erl, i, eval_mod, kind, &bindings_in, &bindings_out);
            }
            Expected::Value(v) => {
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

                // Add value assertion
                if v == "_" {
                    // Bare wildcard: run but don't check result
                } else if has_wildcard_underscore(v) {
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
// Main entry point
// ──────────────────────────────────────────────────────────────────────────

/// Compiled test file metadata, ready for execution.
struct CompiledTest {
    /// Source file path.
    source_file: Utf8PathBuf,
    /// `TestCase` class info.
    test_class: TestCaseClass,
    /// `EUnit` wrapper module name.
    eunit_module: String,
}

/// Compiled doc test metadata, ready for execution.
struct CompiledDocTest {
    /// Source file path.
    source_file: Utf8PathBuf,
    /// Display name (e.g., `IntegerDocTest`).
    display_name: String,
    /// `EUnit` wrapper module name.
    eunit_module: String,
    /// Number of assertion test cases.
    assertion_count: usize,
}

/// Run `BUnit` tests.
///
/// Discovers `TestCase` subclasses in `.bt` files, compiles them, generates
/// `EUnit` wrappers, and runs all tests in a single BEAM process.
#[instrument(skip_all)]
#[allow(clippy::too_many_lines)]
pub fn run_tests(path: &str) -> Result<()> {
    info!("Starting BUnit test run");
    let start_time = Instant::now();

    let test_path = Utf8PathBuf::from(path);

    // Determine if path is a single file or directory
    let test_files = if test_path.is_file() {
        vec![test_path.clone()]
    } else if test_path.is_dir() {
        let files = find_test_files(&test_path)?;
        if files.is_empty() {
            println!("No test files found in '{test_path}'");
            return Ok(());
        }
        files
    } else {
        miette::bail!("Test path '{}' not found", test_path);
    };

    // Create temporary build directory
    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory")?;
    let build_dir = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;

    // Phase 1: Discover test classes and compile
    let mut compiled_tests = Vec::new();
    let mut compiled_doc_tests: Vec<CompiledDocTest> = Vec::new();
    let mut all_erl_files = Vec::new();
    let mut all_fixture_modules = Vec::new();
    let mut fixture_modules_by_name: HashMap<String, Utf8PathBuf> = HashMap::new();

    for test_file in &test_files {
        let (test_classes, load_files) = discover_test_classes(test_file)?;

        // Compile @load fixtures (paths are CWD-relative, matching E2E/test_stdlib semantics)
        for load_path in &load_files {
            let fixture_path = Utf8PathBuf::from(load_path);

            let fixture_module = fixture_module_name(&fixture_path)?;
            if let Some(existing_path) = fixture_modules_by_name.get(&fixture_module) {
                if existing_path != &fixture_path {
                    miette::bail!(
                        "Fixture module name collision for '{fixture_module}': '{}' vs '{}'",
                        existing_path,
                        fixture_path
                    );
                }
                // Already compiled this fixture — skip
                continue;
            }
            fixture_modules_by_name.insert(fixture_module.clone(), fixture_path.clone());

            if !fixture_path.exists() {
                miette::bail!(
                    "Fixture file '{}' referenced by @load in '{}' not found",
                    load_path,
                    test_file
                );
            }
            let module_name = compile_fixture(&fixture_path, &build_dir)?;
            all_fixture_modules.push(module_name);
        }

        // Compile TestCase subclasses
        for test_class in test_classes {
            // Compile the test .bt file through normal pipeline
            compile_test_file(test_file, &test_class.module_name, &build_dir)?;

            // Generate EUnit wrapper
            let eunit_module = format!("{}_tests", test_class.module_name);
            let wrapper_source = generate_eunit_wrapper(&test_class, test_file.as_str());

            let erl_file = build_dir.join(format!("{eunit_module}.erl"));
            fs::write(&erl_file, &wrapper_source)
                .into_diagnostic()
                .wrap_err("Failed to write EUnit wrapper")?;
            all_erl_files.push(erl_file);

            compiled_tests.push(CompiledTest {
                source_file: test_file.clone(),
                test_class,
                eunit_module,
            });
        }

        // Discover doc tests in the same file
        let doc_results = discover_and_compile_doc_tests(test_file, &build_dir)?;
        for dr in doc_results {
            all_erl_files.push(dr.erl_file);
            compiled_doc_tests.push(CompiledDocTest {
                source_file: test_file.clone(),
                display_name: dr.display_name,
                eunit_module: dr.eunit_module,
                assertion_count: dr.assertion_count,
            });
        }
    }

    if compiled_tests.is_empty() && compiled_doc_tests.is_empty() {
        println!("No tests found");
        return Ok(());
    }

    // Deduplicate fixture modules
    all_fixture_modules.sort();
    all_fixture_modules.dedup();

    let total_bunit_tests: usize = compiled_tests
        .iter()
        .map(|t| t.test_class.test_methods.len())
        .sum();
    let total_doc_tests: usize = compiled_doc_tests.iter().map(|d| d.assertion_count).sum();
    let total_tests = total_bunit_tests + total_doc_tests;

    println!("Compiling {} test file(s)...", test_files.len());

    // Phase 2: Compile EUnit wrappers
    compile_erl_files(&all_erl_files, &build_dir)?;

    // Phase 3: Run tests
    println!("Running tests...\n");

    let mut eunit_modules: Vec<&str> = compiled_tests
        .iter()
        .map(|t| t.eunit_module.as_str())
        .collect();
    for dt in &compiled_doc_tests {
        eunit_modules.push(&dt.eunit_module);
    }

    let result = run_eunit_tests(&eunit_modules, &all_fixture_modules, &build_dir)?;

    // Phase 4: Report results
    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut failed_details = Vec::new();

    // Report BUnit TestCase results
    for compiled in &compiled_tests {
        let test_count = compiled.test_class.test_methods.len();
        let class_name = &compiled.test_class.class_name;

        if let Some(failure) = result.failed_modules.get(&compiled.eunit_module) {
            total_failed += test_count;
            println!("  {class_name}: {test_count} tests, 0 passed ✗");
            failed_details.push(format!(
                "FAIL {} ({}):\n{}",
                class_name, compiled.source_file, failure
            ));
        } else {
            total_passed += test_count;
            println!("  {class_name}: {test_count} tests, {test_count} passed ✓");
        }
    }

    // Report doc test results
    for doc_test in &compiled_doc_tests {
        let test_count = doc_test.assertion_count;
        let name = &doc_test.display_name;

        if let Some(failure) = result.failed_modules.get(&doc_test.eunit_module) {
            total_failed += test_count;
            println!("  {name}: {test_count} tests, 0 passed ✗");
            failed_details.push(format!(
                "FAIL {} ({}):\n{}",
                name, doc_test.source_file, failure
            ));
        } else {
            total_passed += test_count;
            println!("  {name}: {test_count} tests, {test_count} passed ✓");
        }
    }

    let elapsed = start_time.elapsed();
    let elapsed_secs = elapsed.as_secs_f64();

    println!();
    if total_failed == 0 {
        println!(
            "{} file(s), {} tests, {} passed, 0 failed ({:.1}s)",
            test_files.len(),
            total_tests,
            total_passed,
            elapsed_secs,
        );
    } else {
        for detail in &failed_details {
            eprintln!("{detail}");
        }
        eprintln!();
        eprintln!(
            "{} file(s), {} tests, {} passed, {} failed ({:.1}s)",
            test_files.len(),
            total_tests,
            total_passed,
            total_failed,
            elapsed_secs,
        );
        miette::bail!("{total_failed} test(s) failed");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover_no_test_classes() {
        let temp = tempfile::TempDir::new().unwrap();
        let file = Utf8PathBuf::from_path_buf(temp.path().join("plain.bt")).unwrap();
        fs::write(&file, "Object subclass: Plain\n  foo => 42\n").unwrap();

        let (classes, loads) = discover_test_classes(&file).unwrap();
        assert!(classes.is_empty());
        assert!(loads.is_empty());
    }

    #[test]
    fn test_discover_test_case_subclass() {
        let temp = tempfile::TempDir::new().unwrap();
        let file = Utf8PathBuf::from_path_buf(temp.path().join("my_test.bt")).unwrap();
        fs::write(
            &file,
            "TestCase subclass: MyTest\n  testAdd => self assert: (1 + 2) equals: 3\n  helper => nil\n",
        )
        .unwrap();

        let (classes, _) = discover_test_classes(&file).unwrap();
        assert_eq!(classes.len(), 1);
        assert_eq!(classes[0].class_name, "MyTest");
        assert_eq!(classes[0].test_methods, vec!["testAdd"]);
        assert!(!classes[0].has_setup);
        assert!(!classes[0].has_teardown);
    }

    #[test]
    fn test_discover_setup_teardown() {
        let temp = tempfile::TempDir::new().unwrap();
        let file = Utf8PathBuf::from_path_buf(temp.path().join("lifecycle.bt")).unwrap();
        fs::write(
            &file,
            "TestCase subclass: LifecycleTest\n  setUp => nil\n  tearDown => nil\n  testIt => self assert: true\n",
        )
        .unwrap();

        let (classes, _) = discover_test_classes(&file).unwrap();
        assert_eq!(classes.len(), 1);
        assert!(classes[0].has_setup);
        assert!(classes[0].has_teardown);
        assert_eq!(classes[0].test_methods, vec!["testIt"]);
    }

    #[test]
    fn test_discover_load_directives() {
        let temp = tempfile::TempDir::new().unwrap();
        let file = Utf8PathBuf::from_path_buf(temp.path().join("with_load.bt")).unwrap();
        fs::write(
            &file,
            "// @load test/fixtures/counter.bt\nTestCase subclass: T\n  testX => nil\n",
        )
        .unwrap();

        let (_, loads) = discover_test_classes(&file).unwrap();
        assert_eq!(loads, vec!["test/fixtures/counter.bt"]);
    }

    #[test]
    fn test_generate_eunit_wrapper_simple() {
        let test_class = TestCaseClass {
            class_name: "CounterTest".to_string(),
            superclass_name: "TestCase".to_string(),
            module_name: "bt@counter_test".to_string(),
            test_methods: vec!["testIncrement".to_string()],
            has_setup: false,
            has_teardown: false,
        };

        let wrapper = generate_eunit_wrapper(&test_class, "test/counter_test.bt");
        assert!(wrapper.contains("-module('bt@counter_test_tests')."));
        assert!(wrapper.contains("'testIncrement_test'()"));
        assert!(wrapper.contains("'bt@counter_test':new()"));
        assert!(wrapper.contains("'bt@counter_test':dispatch('testIncrement', [], Instance)"));
        // No setUp/tearDown
        assert!(!wrapper.contains("setUp"));
        assert!(!wrapper.contains("tearDown"));
        assert!(!wrapper.contains("try"));
    }

    #[test]
    fn test_generate_eunit_wrapper_with_lifecycle() {
        let test_class = TestCaseClass {
            class_name: "MyTest".to_string(),
            superclass_name: "TestCase".to_string(),
            module_name: "bt@my_test".to_string(),
            test_methods: vec!["testA".to_string(), "testB".to_string()],
            has_setup: true,
            has_teardown: true,
        };

        let wrapper = generate_eunit_wrapper(&test_class, "test/my_test.bt");
        assert!(wrapper.contains("dispatch('setUp', [], Instance)"));
        assert!(wrapper.contains("try"));
        assert!(wrapper.contains("after"));
        assert!(wrapper.contains("dispatch('tearDown', [], Instance)"));
        // Both test methods present
        assert!(wrapper.contains("'testA_test'()"));
        assert!(wrapper.contains("'testB_test'()"));
    }
}
