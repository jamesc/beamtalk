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

use crate::beam_compiler::{BeamCompiler, compile_source_with_bindings};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;
use std::fs;
use std::time::Instant;
use tracing::{debug, info, instrument, warn};

use super::manifest;

/// Per-package class module indexes: package root → (`class_module_index`, `class_superclass_index`).
type PkgClassIndexes = HashMap<Utf8PathBuf, (HashMap<String, String>, HashMap<String, String>)>;

// ──────────────────────────────────────────────────────────────────────────
// Test discovery from AST
// ──────────────────────────────────────────────────────────────────────────

/// Metadata about a `TestCase` subclass discovered in a `.bt` file.
#[derive(Debug)]
#[allow(clippy::struct_excessive_bools)]
pub(crate) struct TestCaseClass {
    /// Class name (e.g., `CounterTest`).
    pub(crate) class_name: String,
    /// Superclass name (e.g., `TestCase`).
    #[allow(dead_code)] // populated during discovery for future diagnostic use
    pub(crate) superclass_name: String,
    /// Module name for the compiled BEAM module (e.g., `bt@counter_test`).
    pub(crate) module_name: String,
    /// Methods whose names start with `test`.
    pub(crate) test_methods: Vec<String>,
    /// Whether a `setUp` method is defined.
    pub(crate) has_setup: bool,
    /// Whether a `tearDown` method is defined.
    pub(crate) has_teardown: bool,
    /// Whether a `setUpOnce` method is defined (BT-1549).
    pub(crate) has_setup_once: bool,
    /// Whether a `tearDownOnce` method is defined (BT-1549).
    pub(crate) has_teardown_once: bool,
}

/// Discover `TestCase` subclasses in a `.bt` file by parsing the AST.
///
/// Returns discovered test classes and any `@load` directives.
pub(crate) fn discover_test_classes(
    source_path: &Utf8Path,
) -> Result<(Vec<TestCaseClass>, Vec<String>)> {
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
        let mut has_setup_once = false;
        let mut has_teardown_once = false;

        for method in &class.methods {
            let selector_name = method.selector.name();
            if selector_name.starts_with("test") {
                test_methods.push(selector_name.to_string());
            } else if selector_name == "setUp" {
                has_setup = true;
            } else if selector_name == "tearDown" {
                has_teardown = true;
            } else if selector_name == "setUpOnce" {
                has_setup_once = true;
            } else if selector_name == "tearDownOnce" {
                has_teardown_once = true;
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
                has_setup_once,
                has_teardown_once,
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
/// 2. Call `Module:dispatch(setUp, [], Instance)` and capture return value if defined
/// 3. Call `Module:dispatch(testMethod, [], SetUpInstance)`
/// 4. Call `Module:dispatch(tearDown, [], SetUpInstance)` if defined
pub(crate) fn generate_eunit_wrapper(test_class: &TestCaseClass, source_file: &str) -> String {
    let eunit_module = format!("{}_tests", test_class.module_name);
    let bt_module = &test_class.module_name;
    let has_suite_lifecycle = test_class.has_setup_once || test_class.has_teardown_once;

    let mut erl = String::new();

    let _ = write!(
        erl,
        "%% Generated from {source_file} ({class})\n\
         -module('{eunit_module}').\n\
         -include_lib(\"eunit/include/eunit.hrl\").\n\n",
        class = test_class.class_name,
    );

    if has_suite_lifecycle {
        // BT-1549: Use EUnit {setup, Setup, Cleanup, Instantiator} fixture pattern
        // to run setUpOnce once before all tests and tearDownOnce once after.
        generate_eunit_suite_wrapper(&mut erl, test_class, bt_module);
    } else {
        generate_eunit_per_test_wrapper(&mut erl, test_class, bt_module);
    }

    erl
}

/// Generate `EUnit` wrapper with per-test functions (original pattern, no setUpOnce).
fn generate_eunit_per_test_wrapper(erl: &mut String, test_class: &TestCaseClass, bt_module: &str) {
    for method_name in &test_class.test_methods {
        // EUnit test generator name: method_name + _test_ (trailing underscore)
        // Using _test_ generator form with {timeout, 30, ...} to allow tests that
        // involve actor dispatch (e.g. deadlock detection) up to 30 seconds.
        // Erlang atom-safe: quote with single quotes
        let _ = writeln!(erl, "'{method_name}_test_'() ->");

        // Determine setUp
        let has_setup = test_class.has_setup;
        let has_teardown = test_class.has_teardown;

        let _ = writeln!(erl, "    {{timeout, 30, fun() ->");

        // Create fresh instance
        let _ = writeln!(erl, "        Instance = '{bt_module}':new(),");

        // setUp (if defined)
        // For value objects (immutable maps), setUp returns a new instance with modified fields.
        // BT-1293: We must validate the setUp return value before using it as self. When setUp
        // ends with an untaken conditional (e.g. `false ifTrue: [...]`), the return value is
        // `false`/`nil` rather than self — using that as the dispatch receiver corrupts all
        // subsequent test method dispatches with a DNU error. Only accept the return value when
        // it is a map of the same class; otherwise fall back to the original Instance.
        let instance_var = if has_setup {
            let _ = writeln!(
                erl,
                "        SetUpResult = '{bt_module}':dispatch('setUp', [], Instance),"
            );
            let _ = writeln!(
                erl,
                "        Instance1 = case beamtalk_test_case:is_valid_setUp_result(Instance, SetUpResult) of"
            );
            let _ = writeln!(erl, "            true -> SetUpResult;");
            let _ = writeln!(erl, "            false -> Instance");
            let _ = writeln!(erl, "        end,");
            "Instance1"
        } else {
            "Instance"
        };

        // Run test method inside try/after for tearDown
        // BT-1353: skipTest/1 uses error (not throw) to survive Erlang proxy dispatch.
        // The wrapped error is #{error := {beamtalk_error, bunit_skip, _, _, _, _, _}}.
        // Catch both the throw form (from skip/1) and the error form (from skipTest/1)
        // so that `self skip:` works correctly in EUnit wrappers on all platforms.
        if has_teardown {
            let _ = writeln!(erl, "        try");
            let _ = writeln!(
                erl,
                "            '{bt_module}':dispatch('{method_name}', [], {instance_var})"
            );
            let _ = writeln!(erl, "        catch");
            let _ = writeln!(erl, "            throw:{{bunit_skip, _}} -> ok;");
            let _ = writeln!(
                erl,
                "            error:#{{error := {{beamtalk_error, bunit_skip, _, _, _, _, _}}}} -> ok"
            );
            let _ = writeln!(erl, "        after");
            let _ = writeln!(
                erl,
                "            '{bt_module}':dispatch('tearDown', [], {instance_var})"
            );
            let _ = writeln!(erl, "        end");
        } else {
            let _ = writeln!(erl, "        try");
            let _ = writeln!(
                erl,
                "            '{bt_module}':dispatch('{method_name}', [], {instance_var})"
            );
            let _ = writeln!(erl, "        catch");
            let _ = writeln!(erl, "            throw:{{bunit_skip, _}} -> ok;");
            let _ = writeln!(
                erl,
                "            error:#{{error := {{beamtalk_error, bunit_skip, _, _, _, _, _}}}} -> ok"
            );
            let _ = writeln!(erl, "        end");
        }

        let _ = writeln!(erl, "    end}}.");

        erl.push('\n');
    }
}

/// Generate `EUnit` wrapper using `{setup, Setup, Cleanup, Instantiator}` fixture
/// pattern for classes with setUpOnce/tearDownOnce (BT-1549).
fn generate_eunit_suite_wrapper(erl: &mut String, test_class: &TestCaseClass, bt_module: &str) {
    let has_setup = test_class.has_setup;
    let has_teardown = test_class.has_teardown;
    let has_setup_once = test_class.has_setup_once;
    let has_teardown_once = test_class.has_teardown_once;

    // Single EUnit test generator that wraps all tests in a {setup, ...} fixture
    let _ = writeln!(erl, "'suite_test_'() ->");
    let _ = writeln!(erl, "    {{setup,");

    // Setup function: runs setUpOnce, returns Fixture
    if has_setup_once {
        let _ = writeln!(
            erl,
            "     fun() -> '{bt_module}':dispatch(setUpOnce, [], '{bt_module}':new()) end,"
        );
    } else {
        let _ = writeln!(erl, "     fun() -> nil end,");
    }

    // Cleanup function: runs tearDownOnce with fixture injected
    if has_teardown_once {
        let _ = writeln!(erl, "     fun(Fixture) ->");
        let _ = writeln!(erl, "         try");
        let _ = writeln!(
            erl,
            "             Inst = ('{bt_module}':new())#{{suiteFixture => Fixture}},"
        );
        let _ = writeln!(
            erl,
            "             '{bt_module}':dispatch(tearDownOnce, [], Inst)"
        );
        let _ = writeln!(erl, "         catch _:_ -> ok");
        let _ = writeln!(erl, "         end");
        let _ = writeln!(erl, "     end,");
    } else {
        let _ = writeln!(erl, "     fun(_Fixture) -> ok end,");
    }

    // Instantiator: returns list of test descriptors, each with Fixture injected
    let _ = writeln!(erl, "     fun(Fixture) -> [");

    for (i, method_name) in test_class.test_methods.iter().enumerate() {
        let comma = if i + 1 < test_class.test_methods.len() {
            ","
        } else {
            ""
        };

        let _ = writeln!(erl, "         {{\"{method_name}\", {{timeout, 30, fun() ->");
        let _ = writeln!(erl, "             Instance = '{bt_module}':new(),");

        // setUp
        let instance_var = if has_setup {
            let _ = writeln!(
                erl,
                "             SetUpResult = '{bt_module}':dispatch('setUp', [], Instance),"
            );
            let _ = writeln!(
                erl,
                "             Instance1 = case beamtalk_test_case:is_valid_setUp_result(Instance, SetUpResult) of"
            );
            let _ = writeln!(erl, "                 true -> SetUpResult;");
            let _ = writeln!(erl, "                 false -> Instance");
            let _ = writeln!(erl, "             end,");
            "Instance1"
        } else {
            "Instance"
        };

        // Inject suite fixture
        let _ = writeln!(
            erl,
            "             {instance_var}F = {instance_var}#{{suiteFixture => Fixture}},"
        );
        let fixture_var = format!("{instance_var}F");

        // Run test + tearDown
        if has_teardown {
            let _ = writeln!(erl, "             try");
            let _ = writeln!(
                erl,
                "                 '{bt_module}':dispatch('{method_name}', [], {fixture_var})"
            );
            let _ = writeln!(erl, "             catch");
            let _ = writeln!(erl, "                 throw:{{bunit_skip, _}} -> ok;");
            let _ = writeln!(
                erl,
                "                 error:#{{error := {{beamtalk_error, bunit_skip, _, _, _, _, _}}}} -> ok"
            );
            let _ = writeln!(erl, "             after");
            let _ = writeln!(
                erl,
                "                 '{bt_module}':dispatch('tearDown', [], {fixture_var})"
            );
            let _ = writeln!(erl, "             end");
        } else {
            let _ = writeln!(erl, "             try");
            let _ = writeln!(
                erl,
                "                 '{bt_module}':dispatch('{method_name}', [], {fixture_var})"
            );
            let _ = writeln!(erl, "             catch");
            let _ = writeln!(erl, "                 throw:{{bunit_skip, _}} -> ok;");
            let _ = writeln!(
                erl,
                "                 error:#{{error := {{beamtalk_error, bunit_skip, _, _, _, _, _}}}} -> ok"
            );
            let _ = writeln!(erl, "             end");
        }

        let _ = writeln!(erl, "         end}}}}{comma}");
    }

    let _ = writeln!(erl, "     ] end");
    let _ = writeln!(erl, "    }}.");
}

// ──────────────────────────────────────────────────────────────────────────
// Fixture compilation
// ──────────────────────────────────────────────────────────────────────────

/// Pre-compile all `.bt` fixture files in a directory into the build directory.
///
/// Returns the set of compiled module names. These modules will be available
/// on the BEAM code path during test execution, making fixture classes
/// available without explicit `// @load` directives — similar to how all
/// classes exist in a Smalltalk image.
fn compile_fixtures_directory(
    fixtures_dir: &Utf8Path,
    output_dir: &Utf8Path,
    class_module_index: &HashMap<String, String>,
    class_superclass_index: &HashMap<String, String>,
    warnings_as_errors: bool,
) -> Result<Vec<String>> {
    if !fixtures_dir.is_dir() {
        return Ok(Vec::new());
    }

    let fixture_files = find_test_files(fixtures_dir)?;
    if fixture_files.is_empty() {
        return Ok(Vec::new());
    }

    info!(
        "Compiling {} fixture(s) from '{}'",
        fixture_files.len(),
        fixtures_dir
    );

    let mut module_names = Vec::new();
    let mut core_files = Vec::new();
    let mut fixtures_by_module: HashMap<String, Utf8PathBuf> = HashMap::new();
    for fixture_path in &fixture_files {
        let module_name = fixture_module_name(fixture_path)?;
        if let Some(existing_path) = fixtures_by_module.get(&module_name) {
            if existing_path != fixture_path {
                miette::bail!(
                    "Fixture module name collision for '{module_name}': '{}' vs '{}'",
                    existing_path,
                    fixture_path
                );
            }
        }
        fixtures_by_module.insert(module_name.clone(), fixture_path.clone());

        let core_file = generate_core_file(
            fixture_path,
            &module_name,
            output_dir,
            class_module_index,
            class_superclass_index,
            warnings_as_errors,
        )
        .wrap_err_with(|| format!("Failed to compile fixture '{fixture_path}'"))?;
        core_files.push(core_file);
        module_names.push(module_name);
    }

    // Batch compile all fixture cores in a single BEAM process startup
    if !core_files.is_empty() {
        let compiler = BeamCompiler::new(output_dir.to_owned());
        compiler
            .compile_batch(&core_files)
            .wrap_err("Failed to compile fixture BEAM files")?;
    }

    Ok(module_names)
}

/// Compile a fixture file referenced by `@load` directive.
fn compile_fixture(
    fixture_path: &Utf8Path,
    output_dir: &Utf8Path,
    class_module_index: &HashMap<String, String>,
    class_superclass_index: &HashMap<String, String>,
    warnings_as_errors: bool,
) -> Result<String> {
    let module_name = fixture_module_name(fixture_path)?;

    let core_file = generate_core_file(
        fixture_path,
        &module_name,
        output_dir,
        class_module_index,
        class_superclass_index,
        warnings_as_errors,
    )
    .wrap_err_with(|| format!("Failed to compile fixture '{fixture_path}'"))?;

    let compiler = BeamCompiler::new(output_dir.to_owned());
    compiler
        .compile_batch(&[core_file])
        .wrap_err_with(|| format!("Failed to compile fixture BEAM for '{fixture_path}'"))?;

    Ok(module_name)
}

/// Derive a BEAM module name from a fixture file path (e.g. `counter.bt` → `bt@counter`).
fn fixture_module_name(fixture_path: &Utf8Path) -> Result<String> {
    let stem = fixture_path
        .file_stem()
        .ok_or_else(|| miette::miette!("Fixture file has no name: {}", fixture_path))?;

    Ok(format!(
        "bt@{}",
        beamtalk_core::codegen::core_erlang::to_module_name(stem)
    ))
}

/// Build a class → module name index from fixture files.
///
/// Parses each fixture file to extract class names, then maps each class name
/// to its module name (e.g. `"Env"` → `"bt@env"` for `fixtures/scheme/env.bt`).
///
/// This index is merged into the main `class_module_index` before any
/// compilation so that both fixture files and test files can correctly resolve
/// cross-file references to fixture classes in subdirectories.
fn build_fixture_class_module_index(
    fixture_files: &[Utf8PathBuf],
) -> Result<HashMap<String, String>> {
    let mut index = HashMap::new();

    for file in fixture_files {
        let module_name = fixture_module_name(file)?;
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);

        for class in &module.classes {
            index.insert(class.name.name.to_string(), module_name.clone());
        }
    }

    Ok(index)
}

// ──────────────────────────────────────────────────────────────────────────
// Test file compilation
// ──────────────────────────────────────────────────────────────────────────

/// Compile a `.bt` source file to Core Erlang only (no BEAM compilation).
///
/// Returns the path to the generated `.core` file. Callers should collect
/// multiple core files and batch-compile them with a single `BeamCompiler`
/// call to avoid repeated Erlang VM startup overhead.
fn generate_core_file(
    source_path: &Utf8Path,
    module_name: &str,
    output_dir: &Utf8Path,
    class_module_index: &HashMap<String, String>,
    class_superclass_index: &HashMap<String, String>,
    warnings_as_errors: bool,
) -> Result<Utf8PathBuf> {
    let core_file = output_dir.join(format!("{module_name}.core"));

    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: false,
        allow_primitives: false,
        workspace_mode: false,
        warnings_as_errors,
        ..Default::default()
    };

    compile_source_with_bindings(
        source_path,
        module_name,
        &core_file,
        &options,
        &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
        class_module_index,
        class_superclass_index,
        &[],
        None,
    )
    .wrap_err_with(|| format!("Failed to compile '{source_path}'"))?;

    Ok(core_file)
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
#[allow(clippy::too_many_lines)] // test orchestration: bootstrap, load modules, run, parse results
fn run_eunit_tests(
    test_module_names: &[&str],
    fixture_modules: &[String],
    package_modules: &[String],
    build_dir: &Utf8Path,
    package_ebin_dirs: &[Utf8PathBuf],
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

    // Build package module loading commands (triggers on_load → class registration)
    let package_load_cmd = if package_modules.is_empty() {
        String::new()
    } else {
        package_modules
            .iter()
            .map(|m| format!("code:ensure_loaded('{m}')"))
            .collect::<Vec<_>>()
            .join(", ")
            + ", "
    };

    let eval_cmd = format!(
        "{{ok, _}} = application:ensure_all_started(beamtalk_stdlib), \
         {package_load_cmd}\
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
    #[cfg(windows)]
    {
        // Convert Windows backslashes to forward slashes for Erlang (BT-661)
        let build_dir_path = build_dir.as_str().replace('\\', "/");
        cmd.arg("-noshell").arg("-pa").arg(build_dir_path);
    }
    #[cfg(not(windows))]
    {
        cmd.arg("-noshell").arg("-pa").arg(build_dir.as_str());
    }

    // Add all package ebin directories to code path so package modules can be loaded
    for ebin_dir in package_ebin_dirs {
        #[cfg(windows)]
        {
            let ebin_dir_path = ebin_dir.as_str().replace('\\', "/");
            cmd.arg("-pa").arg(ebin_dir_path);
        }
        #[cfg(not(windows))]
        {
            cmd.arg("-pa").arg(ebin_dir.as_str());
        }
    }

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

/// Collect BEAM module names from `.beam` files in a directory.
///
/// Each `.beam` file's stem is returned as a module name (e.g. `bt@my_app@counter`).
/// Only `.beam` files are considered; `.core`, `.app`, and other files are skipped.
fn collect_beam_module_names(ebin_dir: &Utf8Path) -> Result<Vec<String>> {
    let mut modules = Vec::new();

    if !ebin_dir.exists() {
        return Ok(modules);
    }

    for entry in fs::read_dir(ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read ebin directory '{ebin_dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", ebin_dir))?;

        if path.extension() == Some("beam") {
            if let Some(stem) = path.file_stem() {
                modules.push(stem.to_string());
            }
        }
    }

    modules.sort();
    Ok(modules)
}

/// Canonicalize `path` to an absolute form; fall back to the original if canonicalization fails.
///
/// Used to normalize package root paths before deduplication so that `"."` (relative CWD)
/// and the same directory expressed as an absolute path compare equal.
fn canonical_path(p: &Utf8Path) -> Utf8PathBuf {
    std::fs::canonicalize(p)
        .ok()
        .and_then(|abs| Utf8PathBuf::from_path_buf(abs).ok())
        .unwrap_or_else(|| p.to_owned())
}

/// Walk from `path` to its package root (via `find_package_root`) and return the
/// canonical (absolute, symlink-resolved) form of that root.
///
/// All map keys in `pkg_class_indexes` and `pkg_root_to_name` are canonicalized at
/// build time, so every lookup into those maps must go through this function to
/// guarantee path-shape consistency (relative vs absolute, symlinked paths).
fn canonical_package_root(path: &Utf8Path) -> Option<Utf8PathBuf> {
    find_package_root(path).map(|r| canonical_path(&r))
}

/// Walk up from `path` to find the nearest ancestor directory containing `beamtalk.toml`.
///
/// If `path` is a file, starts at its parent directory. If `path` is a directory,
/// starts there. Returns the directory path if found, `None` if no manifest exists
/// anywhere in the ancestor chain.
fn find_package_root(path: &Utf8Path) -> Option<Utf8PathBuf> {
    let start_dir = if path.is_file() { path.parent()? } else { path };

    let mut current = start_dir.to_owned();
    loop {
        // Guard against empty paths — this happens when walking up from a
        // single-component relative path (e.g. parent of "test" is "").
        // Without this guard, `"".join("beamtalk.toml")` resolves to
        // `"beamtalk.toml"` relative to the CWD and returns `Some("")`,
        // which later causes `build("", ...)` to fail with
        // "Path '' does not exist". (BT-1228)
        if current.as_str().is_empty() {
            return None;
        }
        if current.join("beamtalk.toml").exists() {
            return Some(current);
        }
        current = current.parent()?.to_owned();
    }
}

/// Return the effective class module index for a test file.
///
/// Uses the per-package index for the file's owning package (if found), so test
/// files only see their own package's class names — not class names from other
/// packages that happen to be compiled in the same `beamtalk test` run. Fixture
/// classes are merged on top. Falls back to the merged combined index for files
/// that don't belong to any discovered package.
fn class_index_for_file(
    test_file: &Utf8Path,
    pkg_class_indexes: &PkgClassIndexes,
    fixture_class_index: &HashMap<String, String>,
    merged_class_index: &HashMap<String, String>,
    merged_super_index: &HashMap<String, String>,
) -> (HashMap<String, String>, HashMap<String, String>) {
    let (base_class, base_super) = canonical_package_root(test_file)
        .and_then(|r| pkg_class_indexes.get(&r))
        .map_or_else(
            || (merged_class_index.clone(), merged_super_index.clone()),
            |(c, s)| (c.clone(), s.clone()),
        );

    let mut effective = base_class;
    effective.extend(
        fixture_class_index
            .iter()
            .map(|(k, v)| (k.clone(), v.clone())),
    );
    (effective, base_super)
}

/// Recursively collect `.bt` files from `dir` into `files`.
fn find_test_files_recursive(dir: &Utf8Path, files: &mut Vec<Utf8PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", dir))?;

        if path.is_dir() {
            // Skip the fixtures/ subdirectory — fixtures are pre-compiled in Phase 0
            // and should not be treated as test files.
            if path.file_name() == Some("fixtures") {
                continue;
            }
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
    class_module_index: &HashMap<String, String>,
    class_superclass_index: &HashMap<String, String>,
    warnings_as_errors: bool,
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
    compile_fixture(
        source_path,
        build_dir,
        class_module_index,
        class_superclass_index,
        warnings_as_errors,
    )
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
    /// Human-readable name for test output (e.g., `IntegerDocTest`).
    display_name: String,
    /// `EUnit` wrapper module name for this class's doc tests.
    eunit_module: String,
    /// Path to the generated `.erl` wrapper file.
    erl_file: Utf8PathBuf,
    /// Number of `// =>` assertions compiled into tests.
    assertion_count: usize,
}

/// Generate an `EUnit` wrapper module for doc test assertions.
///
/// Delegates to `beamtalk_stdlib_test:run_and_assert/2` for consistent
/// result formatting and per-assertion error reporting.
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
    };

    let mut erl = String::new();

    let _ = writeln!(
        erl,
        "%% Doc tests from {source_file} ({class_name})\n\
         -module('{module_name}').\n\
         -include_lib(\"eunit/include/eunit.hrl\").\n"
    );

    // Test generator — thin wrapper that delegates to beamtalk_stdlib_test.
    let _ = writeln!(
        erl,
        "'{module_name}_test_'() ->\n\
         \x20   {{timeout, 60, fun() ->\n\
         \x20       beamtalk_stdlib_test:run_and_assert('{module_name}', ["
    );

    for (i, (case, eval_mod)) in cases.iter().zip(eval_module_names.iter()).enumerate() {
        let escaped_file = source_file.replace('\\', "\\\\").replace('"', "\\\"");
        let escaped_expr = case.expression.replace('\\', "\\\\").replace('"', "\\\"");
        let location = format!("{escaped_file}:{} `{escaped_expr}`", case.source_line);
        let location_bin = format!("<<\"{location}\"/utf8>>");

        let var_atom = match extract_assignment_var(&case.expression) {
            Some(name) => format!("'{name}'"),
            None => "none".to_string(),
        };

        let comma = if i < cases.len() - 1 { "," } else { "" };

        match &case.expected {
            Expected::Error { kind } => {
                let _ = writeln!(
                    erl,
                    "           {{error, '{eval_mod}', '{kind}', {var_atom}, {location_bin}}}{comma}",
                );
            }
            Expected::Value(v) if v == "_" => {
                let _ = writeln!(
                    erl,
                    "           {{value_any, '{eval_mod}', {var_atom}, {location_bin}}}{comma}",
                );
            }
            Expected::Value(v) if has_wildcard_underscore(v) => {
                let expected_bin = expected_to_binary_literal(v);
                let _ = writeln!(
                    erl,
                    "           {{value_wildcard, '{eval_mod}', {expected_bin}, {var_atom}, {location_bin}}}{comma}",
                );
            }
            Expected::Value(v) => {
                let expected_bin = expected_to_binary_literal(v);
                let _ = writeln!(
                    erl,
                    "           {{value, '{eval_mod}', {expected_bin}, {var_atom}, {location_bin}}}{comma}",
                );
            }
        }
    }

    erl.push_str("       ])\n    end}.\n");
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

/// Shared state threaded through the test pipeline phases.
///
/// Each phase reads from and writes to this struct, avoiding the need to pass
/// many individual variables between the extracted phase functions.
struct TestPipeline {
    /// Original test path from CLI.
    test_path: Utf8PathBuf,
    /// Resolved list of `.bt` test files to process.
    test_files: Vec<Utf8PathBuf>,
    /// Temporary build directory for compiled artifacts.
    build_dir: Utf8PathBuf,
    /// Whether to treat warnings as errors.
    warnings_as_errors: bool,
    /// Discovered packages: `(canonical_root, manifest)`.
    discovered_packages: Vec<(Utf8PathBuf, manifest::PackageManifest)>,
    /// Package root to package name mapping.
    pkg_root_to_name: HashMap<Utf8PathBuf, String>,
    /// Per-package class module indexes.
    pkg_class_indexes: PkgClassIndexes,
    /// Merged class-name to module-name index across all packages.
    class_module_index: HashMap<String, String>,
    /// Merged class-name to superclass-name index across all packages.
    class_superclass_index: HashMap<String, String>,
    /// Fixture class-name to module-name index.
    fixture_class_index: HashMap<String, String>,
    /// Module names of pre-compiled fixtures (from `fixtures/` directory).
    precompiled_modules: HashSet<String>,
    /// All fixture module names (pre-compiled + `@load`-compiled).
    all_fixture_modules: Vec<String>,
    /// All generated `.erl` `EUnit` wrapper files to compile.
    all_erl_files: Vec<Utf8PathBuf>,
    /// Compiled `BUnit` test metadata.
    compiled_tests: Vec<CompiledTest>,
    /// Compiled doc test metadata.
    compiled_doc_tests: Vec<CompiledDocTest>,
    /// Package BEAM module names (from built packages).
    package_modules: Vec<String>,
    /// Package ebin directories (for code path).
    package_ebin_dirs: Vec<Utf8PathBuf>,
}

/// Run `BUnit` tests.
///
/// Discovers `TestCase` subclasses in `.bt` files, compiles them, generates
/// `EUnit` wrappers, and runs all tests in a single BEAM process.
#[instrument(skip_all)]
pub fn run_tests(path: &str, warnings_as_errors: bool) -> Result<()> {
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

    let mut pipeline = initialize_pipeline(test_path, test_files, build_dir, warnings_as_errors)?;
    compile_fixtures(&mut pipeline)?;
    discover_and_compile_tests(&mut pipeline)?;

    if pipeline.compiled_tests.is_empty() && pipeline.compiled_doc_tests.is_empty() {
        println!("No tests found");
        return Ok(());
    }

    build_packages(&mut pipeline)?;
    let result = execute_tests(&pipeline)?;
    report_results(&pipeline, &result, start_time)
}

/// Initialize the test pipeline: discover packages and build class indexes.
fn initialize_pipeline(
    test_path: Utf8PathBuf,
    test_files: Vec<Utf8PathBuf>,
    build_dir: Utf8PathBuf,
    warnings_as_errors: bool,
) -> Result<TestPipeline> {
    // Discover all unique package roots: walk up from each test file's directory,
    // and also check the CWD. This allows `beamtalk test .` from a parent directory
    // (e.g. `examples/`) to find and build all packages that contain test files.
    //
    // All roots are canonicalized before deduplication so that `"."` (relative CWD)
    // and an absolute path to the same directory compare equal in `seen`. Without
    // this, passing absolute test file paths could insert the same package twice
    // (once as `"."`, once as `/abs/path`), incorrectly flipping multi-package mode.
    let discovered_packages: Vec<(Utf8PathBuf, manifest::PackageManifest)> = {
        let mut roots: Vec<Utf8PathBuf> = Vec::new();
        let mut seen: HashSet<Utf8PathBuf> = HashSet::new();

        // Check CWD first
        let cwd = canonical_path(Utf8Path::new("."));
        if seen.insert(cwd.clone()) {
            roots.push(cwd);
        }

        // Walk up from each test file to find its package root
        for test_file in &test_files {
            if let Some(root) = canonical_package_root(test_file) {
                if seen.insert(root.clone()) {
                    roots.push(root);
                }
            }
        }

        // Load manifests, skip roots without a beamtalk.toml
        let mut pkgs = Vec::new();
        for root in roots {
            if let Some(pkg) = manifest::find_manifest(&root)? {
                pkgs.push((root, pkg));
            }
        }
        pkgs
    };

    // Map from package root -> package name for test module naming.
    // Used to prefix test module names with the package name when multiple
    // packages are in scope, preventing module collisions (e.g. two packages
    // that both define a `SmokeTest` class would otherwise both compile to
    // `bt@smoke_test.beam` in the shared temp build dir).
    let pkg_root_to_name: HashMap<Utf8PathBuf, String> = discovered_packages
        .iter()
        .map(|(root, pkg)| (root.clone(), pkg.name.clone()))
        .collect();

    // Build per-package class indexes and a merged combined index.
    // Per-package indexes let each test file see only its own package's classes,
    // preventing false cross-package class name collisions when running tests
    // from a parent directory containing multiple packages. The merged index is
    // used for fixture compilation (which can reference any package's classes).
    let mut pkg_class_indexes: PkgClassIndexes = HashMap::new();
    let mut class_module_index: HashMap<String, String> = HashMap::new();
    let mut class_superclass_index: HashMap<String, String> = HashMap::new();
    for (pkg_root, pkg) in &discovered_packages {
        let src_dir = pkg_root.join("src");
        if let Ok(src_files) = super::build::collect_source_files_from_dir(&src_dir) {
            let source_root = src_dir.exists().then_some(src_dir);
            if let Ok((pkg_class_map, pkg_super_map, _class_infos, _cached_asts)) =
                super::build::build_class_module_index(
                    &src_files,
                    source_root.as_deref(),
                    &pkg.name,
                )
            {
                pkg_class_indexes.insert(
                    pkg_root.clone(),
                    (pkg_class_map.clone(), pkg_super_map.clone()),
                );
                class_module_index.extend(pkg_class_map);
                class_superclass_index.extend(pkg_super_map);
            }
        }
    }

    Ok(TestPipeline {
        test_path,
        test_files,
        build_dir,
        warnings_as_errors,
        discovered_packages,
        pkg_root_to_name,
        pkg_class_indexes,
        class_module_index,
        class_superclass_index,
        fixture_class_index: HashMap::new(),
        precompiled_modules: HashSet::new(),
        all_fixture_modules: Vec::new(),
        all_erl_files: Vec::new(),
        compiled_tests: Vec::new(),
        compiled_doc_tests: Vec::new(),
        package_modules: Vec::new(),
        package_ebin_dirs: Vec::new(),
    })
}

/// Phase 0: Pre-compile all fixtures in the `fixtures/` subdirectory.
///
/// Makes all fixture classes available during test execution -- like
/// a Smalltalk image where all classes exist in the running environment.
fn compile_fixtures(pipeline: &mut TestPipeline) -> Result<()> {
    let fixtures_dir = if pipeline.test_path.is_dir() {
        pipeline.test_path.join("fixtures")
    } else {
        pipeline
            .test_path
            .parent()
            .map(|p| p.join("fixtures"))
            .unwrap_or_default()
    };

    // Build the fixture class index separately so it can be merged per-file in
    // Phase 1. Also merge into the combined index for Phase 0 fixture compilation,
    // where cross-package and cross-fixture references are allowed.
    let fixture_class_index: HashMap<String, String> = if fixtures_dir.is_dir() {
        let fixture_files = find_test_files(&fixtures_dir)?;
        build_fixture_class_module_index(&fixture_files)?
    } else {
        HashMap::new()
    };
    pipeline
        .class_module_index
        .extend(fixture_class_index.clone());

    let precompiled = compile_fixtures_directory(
        &fixtures_dir,
        &pipeline.build_dir,
        &pipeline.class_module_index,
        &pipeline.class_superclass_index,
        pipeline.warnings_as_errors,
    )?;
    for module_name in &precompiled {
        pipeline.precompiled_modules.insert(module_name.clone());
    }
    pipeline.all_fixture_modules.extend(precompiled);
    pipeline.fixture_class_index = fixture_class_index;

    Ok(())
}

/// Phase 1: Discover test classes, compile test files and doc tests.
///
/// For each test file: resolves `@load` fixtures, compiles `TestCase` subclasses
/// to Core Erlang, generates `EUnit` wrappers, and discovers doc tests. Core files
/// are batch-compiled to BEAM at the end for efficiency.
fn discover_and_compile_tests(pipeline: &mut TestPipeline) -> Result<()> {
    let mut fixture_modules_by_name: HashMap<String, Utf8PathBuf> = HashMap::new();
    // Accumulate all test-file core paths; batch-compile after the loop to avoid
    // one BEAM process startup per file.
    let mut pending_test_cores: Vec<Utf8PathBuf> = Vec::new();

    let fixtures_dir = if pipeline.test_path.is_dir() {
        pipeline.test_path.join("fixtures")
    } else {
        pipeline
            .test_path
            .parent()
            .map(|p| p.join("fixtures"))
            .unwrap_or_default()
    };

    // Clone test_files to avoid borrow conflict with pipeline fields.
    let test_files = pipeline.test_files.clone();

    for test_file in &test_files {
        compile_single_test_file(
            pipeline,
            test_file,
            &fixtures_dir,
            &mut fixture_modules_by_name,
            &mut pending_test_cores,
        )?;
    }

    // Batch compile all test file cores in a single BEAM process startup.
    // This replaces the previous per-file compile_batch calls (one BEAM startup per file).
    if !pending_test_cores.is_empty() {
        let compiler = BeamCompiler::new(pipeline.build_dir.clone());
        compiler
            .compile_batch(&pending_test_cores)
            .wrap_err("Failed to compile test file BEAM files")?;
    }

    // Deduplicate fixture modules
    pipeline.all_fixture_modules.sort();
    pipeline.all_fixture_modules.dedup();

    Ok(())
}

/// Discover, compile, and generate wrappers for a single test file.
///
/// Handles `@load` directive resolution, multi-package module name prefixing,
/// `TestCase` compilation, `EUnit` wrapper generation, and doc test discovery.
fn compile_single_test_file(
    pipeline: &mut TestPipeline,
    test_file: &Utf8Path,
    fixtures_dir: &Utf8Path,
    fixture_modules_by_name: &mut HashMap<String, Utf8PathBuf>,
    pending_test_cores: &mut Vec<Utf8PathBuf>,
) -> Result<()> {
    // Build per-file effective class index: own package's classes + fixtures.
    // This prevents class name collisions between packages when running tests
    // from a common parent directory.
    let (file_class_index, file_super_index) = class_index_for_file(
        test_file,
        &pipeline.pkg_class_indexes,
        &pipeline.fixture_class_index,
        &pipeline.class_module_index,
        &pipeline.class_superclass_index,
    );

    let (mut test_classes, load_files) = discover_test_classes(test_file)?;

    // When multiple packages are in scope, prefix the test module name with
    // the owning package name to prevent `bt@smoke_test.beam` collisions
    // between two packages that each define a class named `SmokeTest`.
    // Single-package runs are unaffected (no prefix added when only one package
    // is discovered), preserving backwards-compatible module names.
    if pipeline.discovered_packages.len() > 1 {
        let pkg_name_slug = canonical_package_root(test_file)
            .and_then(|r| pipeline.pkg_root_to_name.get(&r))
            .map(|n| beamtalk_core::codegen::core_erlang::to_module_name(n));

        if let Some(ref prefix) = pkg_name_slug {
            for tc in &mut test_classes {
                let stem = beamtalk_core::codegen::core_erlang::to_module_name(&tc.class_name);
                tc.module_name = format!("bt@{prefix}@{stem}");
            }
        }
    }

    // Handle deprecated @load directives as fallback.
    // Fixtures in the fixtures/ directory are already compiled above.
    resolve_load_directives(
        pipeline,
        test_file,
        &load_files,
        fixtures_dir,
        fixture_modules_by_name,
        &file_class_index,
        &file_super_index,
    )?;

    // Compile TestCase subclasses
    for test_class in test_classes {
        // Generate Core Erlang for the test file (BEAM compilation is batched below)
        let core_file = generate_core_file(
            test_file,
            &test_class.module_name,
            &pipeline.build_dir,
            &file_class_index,
            &file_super_index,
            pipeline.warnings_as_errors,
        )
        .wrap_err_with(|| format!("Failed to compile test file '{test_file}'"))?;
        pending_test_cores.push(core_file);

        // Generate EUnit wrapper
        let eunit_module = format!("{}_tests", test_class.module_name);
        let wrapper_source = generate_eunit_wrapper(&test_class, test_file.as_str());

        let erl_file = pipeline.build_dir.join(format!("{eunit_module}.erl"));
        fs::write(&erl_file, &wrapper_source)
            .into_diagnostic()
            .wrap_err("Failed to write EUnit wrapper")?;
        pipeline.all_erl_files.push(erl_file);

        pipeline.compiled_tests.push(CompiledTest {
            source_file: test_file.to_path_buf(),
            test_class,
            eunit_module,
        });
    }

    // Discover doc tests in the same file
    let doc_results = discover_and_compile_doc_tests(
        test_file,
        &pipeline.build_dir,
        &file_class_index,
        &file_super_index,
        pipeline.warnings_as_errors,
    )?;
    for dr in doc_results {
        pipeline.all_erl_files.push(dr.erl_file);
        pipeline.compiled_doc_tests.push(CompiledDocTest {
            source_file: test_file.to_path_buf(),
            display_name: dr.display_name,
            eunit_module: dr.eunit_module,
            assertion_count: dr.assertion_count,
        });
    }

    Ok(())
}

/// Resolve and compile deprecated `@load` directives for a test file.
///
/// Validates against pre-compiled fixtures, detects module name collisions,
/// and compiles any non-fixture `@load` references that aren't yet available.
fn resolve_load_directives(
    pipeline: &mut TestPipeline,
    test_file: &Utf8Path,
    load_files: &[String],
    fixtures_dir: &Utf8Path,
    fixture_modules_by_name: &mut HashMap<String, Utf8PathBuf>,
    file_class_index: &HashMap<String, String>,
    file_super_index: &HashMap<String, String>,
) -> Result<()> {
    for load_path in load_files {
        let fixture_path = Utf8PathBuf::from(load_path);

        let fixture_module = fixture_module_name(&fixture_path)?;

        // Skip if already pre-compiled from the fixtures directory
        if pipeline.precompiled_modules.contains(&fixture_module) {
            if fixture_path.starts_with(fixtures_dir) {
                // This @load points into fixtures_dir itself -- already compiled in Phase 0
                eprintln!(
                    "Deprecated: '// @load {load_path}' in '{test_file}' \u{2014} fixture is already available \
                     from the fixtures directory. Remove this directive."
                );
                continue;
            }
            // A non-fixture @load shares a module name with a precompiled fixture --
            // running both would silently use the wrong implementation.
            miette::bail!(
                "Fixture module name collision for '{fixture_module}': \
                 precompiled fixture vs '@load {load_path}' in '{test_file}'"
            );
        }

        // Non-fixture @load (e.g., examples/) -- compile and warn
        eprintln!(
            "Deprecated: '// @load {load_path}' in '{test_file}' \u{2014} move fixture to '{fixtures_dir}' to \
             make it automatically available."
        );

        if let Some(existing_path) = fixture_modules_by_name.get(&fixture_module) {
            if existing_path != &fixture_path {
                miette::bail!(
                    "Fixture module name collision for '{fixture_module}': '{}' vs '{}'",
                    existing_path,
                    fixture_path
                );
            }
            // Already compiled this fixture -- skip
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
        let module_name = compile_fixture(
            &fixture_path,
            &pipeline.build_dir,
            file_class_index,
            file_super_index,
            pipeline.warnings_as_errors,
        )?;
        pipeline.all_fixture_modules.push(module_name);
    }

    Ok(())
}

/// Phase 2: Build all discovered packages so their `.beam` files are available.
///
/// Ensures package-defined classes are compiled and their `on_load` hooks
/// will register them in the class registry when loaded during the test run.
fn build_packages(pipeline: &mut TestPipeline) -> Result<()> {
    for (pkg_root, pkg) in &pipeline.discovered_packages {
        let ebin_dir = pkg_root.join("_build").join("dev").join("ebin");
        println!("Building package '{}'...", pkg.name);
        let build_options = beamtalk_core::CompilerOptions {
            stdlib_mode: false,
            allow_primitives: false,
            workspace_mode: false,
            warnings_as_errors: pipeline.warnings_as_errors,
            ..Default::default()
        };
        super::build::build(pkg_root.as_str(), &build_options).wrap_err_with(|| {
            format!(
                "Failed to build package '{}' before running tests",
                pkg.name
            )
        })?;
        let modules = collect_beam_module_names(&ebin_dir)?;
        debug!(count = modules.len(), "Discovered package modules");
        pipeline.package_modules.extend(modules);
        pipeline.package_ebin_dirs.push(ebin_dir);
    }

    Ok(())
}

/// Phase 3: Compile `EUnit` wrappers and run all tests.
///
/// Returns the `EunitResult` for result reporting.
fn execute_tests(pipeline: &TestPipeline) -> Result<EunitResult> {
    println!("Compiling {} test file(s)...", pipeline.test_files.len());

    // Compile EUnit wrappers
    compile_erl_files(&pipeline.all_erl_files, &pipeline.build_dir)?;

    // Run tests
    println!("Running tests...\n");

    let mut eunit_modules: Vec<&str> = pipeline
        .compiled_tests
        .iter()
        .map(|t| t.eunit_module.as_str())
        .collect();
    for dt in &pipeline.compiled_doc_tests {
        eunit_modules.push(&dt.eunit_module);
    }

    run_eunit_tests(
        &eunit_modules,
        &pipeline.all_fixture_modules,
        &pipeline.package_modules,
        &pipeline.build_dir,
        &pipeline.package_ebin_dirs,
    )
}

/// Phase 4: Aggregate and report test results.
fn report_results(
    pipeline: &TestPipeline,
    result: &EunitResult,
    start_time: Instant,
) -> Result<()> {
    let total_bunit_tests: usize = pipeline
        .compiled_tests
        .iter()
        .map(|t| t.test_class.test_methods.len())
        .sum();
    let total_doc_tests: usize = pipeline
        .compiled_doc_tests
        .iter()
        .map(|d| d.assertion_count)
        .sum();
    let total_tests = total_bunit_tests + total_doc_tests;

    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut failed_details = Vec::new();

    // Report BUnit TestCase results
    for compiled in &pipeline.compiled_tests {
        let test_count = compiled.test_class.test_methods.len();
        let class_name = &compiled.test_class.class_name;

        if let Some(failure) = result.failed_modules.get(&compiled.eunit_module) {
            total_failed += test_count;
            println!("  {class_name}: {test_count} tests, 0 passed \u{2717}");
            failed_details.push(format!(
                "FAIL {} ({}):\n{}",
                class_name, compiled.source_file, failure
            ));
        } else {
            total_passed += test_count;
            println!("  {class_name}: {test_count} tests, {test_count} passed \u{2713}");
        }
    }

    // Report doc test results
    for doc_test in &pipeline.compiled_doc_tests {
        let test_count = doc_test.assertion_count;
        let name = &doc_test.display_name;

        if let Some(failure) = result.failed_modules.get(&doc_test.eunit_module) {
            total_failed += test_count;
            println!("  {name}: {test_count} tests, 0 passed \u{2717}");
            failed_details.push(format!(
                "FAIL {} ({}):\n{}",
                name, doc_test.source_file, failure
            ));
        } else {
            total_passed += test_count;
            println!("  {name}: {test_count} tests, {test_count} passed \u{2713}");
        }
    }

    let elapsed = start_time.elapsed();
    let elapsed_secs = elapsed.as_secs_f64();

    println!();
    if total_failed == 0 {
        println!(
            "{} file(s), {} tests, {} passed, 0 failed ({:.1}s)",
            pipeline.test_files.len(),
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
            pipeline.test_files.len(),
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
            "// @load stdlib/test/fixtures/counter.bt\nTestCase subclass: T\n  testX => nil\n",
        )
        .unwrap();

        let (_, loads) = discover_test_classes(&file).unwrap();
        assert_eq!(loads, vec!["stdlib/test/fixtures/counter.bt"]);
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
            has_setup_once: false,
            has_teardown_once: false,
        };

        let wrapper = generate_eunit_wrapper(&test_class, "test/counter_test.bt");
        assert!(wrapper.contains("-module('bt@counter_test_tests')."));
        assert!(wrapper.contains("'testIncrement_test_'()"));
        assert!(wrapper.contains("{timeout, 30, fun() ->"));
        assert!(wrapper.contains("'bt@counter_test':new()"));
        assert!(wrapper.contains("'bt@counter_test':dispatch('testIncrement', [], Instance)"));
        // No setUp/tearDown, but skip handling wraps dispatch in try/catch
        assert!(!wrapper.contains("setUp"));
        assert!(!wrapper.contains("tearDown"));
        // BT-1353: Must catch both throw form (skip/1) and error form (skipTest/1)
        assert!(wrapper.contains("throw:{bunit_skip, _}"));
        assert!(
            wrapper.contains("error:#{error := {beamtalk_error, bunit_skip,"),
            "EUnit wrapper must catch error-form bunit_skip from skipTest/1"
        );
    }

    #[test]
    fn test_collect_beam_module_names() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create some .beam files and non-.beam files
        fs::write(dir.join("bt@my_app@counter.beam"), b"fake beam").unwrap();
        fs::write(dir.join("bt@my_app@math.beam"), b"fake beam").unwrap();
        fs::write(dir.join("bt@my_app@counter.core"), b"fake core").unwrap();
        fs::write(dir.join("my_app.app"), b"fake app").unwrap();

        let modules = collect_beam_module_names(&dir).unwrap();
        assert_eq!(modules.len(), 2);
        assert!(modules.contains(&"bt@my_app@counter".to_string()));
        assert!(modules.contains(&"bt@my_app@math".to_string()));
    }

    #[test]
    fn test_collect_beam_module_names_empty_dir() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let modules = collect_beam_module_names(&dir).unwrap();
        assert!(modules.is_empty());
    }

    #[test]
    fn test_build_fixture_class_module_index_flat() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        fs::write(
            dir.join("counter.bt"),
            "Object subclass: Counter\n  count => 0\n",
        )
        .unwrap();

        let files = vec![dir.join("counter.bt")];
        let index = build_fixture_class_module_index(&files).unwrap();
        assert_eq!(index.get("Counter").map(String::as_str), Some("bt@counter"));
    }

    #[test]
    fn test_build_fixture_class_module_index_subdirectory() {
        let temp = tempfile::TempDir::new().unwrap();
        let subdir = Utf8PathBuf::from_path_buf(temp.path().join("scheme")).unwrap();
        fs::create_dir_all(&subdir).unwrap();

        fs::write(
            subdir.join("env.bt"),
            "Object subclass: SchemeEnv\n  lookup => nil\n",
        )
        .unwrap();

        // fixture_module_name uses the file stem only, so env.bt → bt@env
        let files = vec![subdir.join("env.bt")];
        let index = build_fixture_class_module_index(&files).unwrap();
        assert_eq!(index.get("SchemeEnv").map(String::as_str), Some("bt@env"));
    }

    #[test]
    fn test_collect_beam_module_names_nonexistent_dir() {
        let dir = Utf8PathBuf::from("/nonexistent/path");
        let modules = collect_beam_module_names(&dir).unwrap();
        assert!(modules.is_empty());
    }

    #[test]
    fn test_find_package_root_found() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create nested structure: pkg/test/my_test.bt with beamtalk.toml at pkg/
        let pkg_dir = dir.join("pkg");
        let test_dir = pkg_dir.join("test");
        fs::create_dir_all(&test_dir).unwrap();
        fs::write(
            pkg_dir.join("beamtalk.toml"),
            "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(test_dir.join("my_test.bt"), "").unwrap();

        let result = find_package_root(&test_dir.join("my_test.bt"));
        assert_eq!(result, Some(pkg_dir));
    }

    #[test]
    fn test_find_package_root_not_found() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // No beamtalk.toml anywhere
        fs::create_dir_all(dir.join("sub")).unwrap();
        fs::write(dir.join("sub/test.bt"), "").unwrap();

        let result = find_package_root(&dir.join("sub/test.bt"));
        assert!(result.is_none());
    }

    /// BT-1228: Walking up from a relative single-component path (e.g. "test")
    /// yields "" as the parent directory. The guard in `find_package_root` must
    /// return `None` before treating "" as a valid package root, since
    /// `"".join("beamtalk.toml")` resolves to `"beamtalk.toml"` relative to
    /// CWD and would return `Some("")`, causing `build("", ...)` to fail.
    ///
    /// This test is hermetic: it sets up a temp dir with a `beamtalk.toml` at
    /// its root, changes the process CWD to that dir, and asserts that passing
    /// a single-component relative path like `"test"` returns `None` (not
    /// `Some("") `) even though `beamtalk.toml` is now discoverable from the
    /// empty parent path. Uses `#[serial(cwd)]` to avoid CWD races.
    #[test]
    #[serial_test::serial(cwd)]
    fn test_find_package_root_relative_path_does_not_return_empty() {
        // RAII guard so CWD is restored even if an assertion panics.
        struct CwdGuard(std::path::PathBuf);
        impl Drop for CwdGuard {
            fn drop(&mut self) {
                std::env::set_current_dir(&self.0).unwrap();
            }
        }

        let temp = tempfile::TempDir::new().unwrap();

        // Write a beamtalk.toml at the temp dir root so that "".join("beamtalk.toml")
        // would find it if the guard were absent.
        fs::write(
            temp.path().join("beamtalk.toml"),
            "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let _cwd_guard = CwdGuard(std::env::current_dir().unwrap());
        std::env::set_current_dir(temp.path()).unwrap();

        // Single-component relative paths: parent is "" which would resolve
        // "beamtalk.toml" in the CWD without the guard. With the guard they
        // must return None — the CWD root is handled by the canonical_path(".")
        // check in run_tests, not by walking relative path ancestors.
        for rel in &["test", "src", "lib"] {
            let result = find_package_root(Utf8Path::new(rel));
            assert!(
                result.is_none(),
                "find_package_root(\"{rel}\") should return None for single-component \
                 relative paths, got {result:?}"
            );
        }
    }

    #[test]
    fn test_find_package_root_directory_input() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let result = find_package_root(&dir);
        assert_eq!(result, Some(dir));
    }

    #[test]
    fn test_class_index_for_file_uses_own_package() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Two packages, each defining a class named "Counter"
        let pkg_a = dir.join("pkg_a");
        let pkg_b = dir.join("pkg_b");
        fs::create_dir_all(pkg_a.join("test")).unwrap();
        fs::create_dir_all(pkg_b.join("test")).unwrap();
        fs::write(
            pkg_a.join("beamtalk.toml"),
            "[package]\nname = \"pkg_a\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(
            pkg_b.join("beamtalk.toml"),
            "[package]\nname = \"pkg_b\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let mut index_a: HashMap<String, String> = HashMap::new();
        index_a.insert("Counter".to_string(), "bt@pkg_a@counter".to_string());
        let mut index_b: HashMap<String, String> = HashMap::new();
        index_b.insert("Counter".to_string(), "bt@pkg_b@counter".to_string());

        let mut pkg_class_indexes: PkgClassIndexes = HashMap::new();
        pkg_class_indexes.insert(canonical_path(&pkg_a), (index_a.clone(), HashMap::new()));
        pkg_class_indexes.insert(canonical_path(&pkg_b), (index_b.clone(), HashMap::new()));

        // Merged index has pkg_b's entry (inserted last, overwrites pkg_a's)
        let mut merged: HashMap<String, String> = HashMap::new();
        merged.extend(index_a);
        merged.extend(index_b); // overwrites Counter

        let fixture_index: HashMap<String, String> = HashMap::new();
        let merged_super: HashMap<String, String> = HashMap::new();

        // Test file in pkg_a should get pkg_a's Counter, not pkg_b's
        let test_file_a = pkg_a.join("test/counter_test.bt");
        fs::write(&test_file_a, "").unwrap();
        let (class_idx, _) = class_index_for_file(
            &test_file_a,
            &pkg_class_indexes,
            &fixture_index,
            &merged,
            &merged_super,
        );
        assert_eq!(
            class_idx.get("Counter").map(String::as_str),
            Some("bt@pkg_a@counter")
        );

        // Test file in pkg_b should get pkg_b's Counter
        let test_file_b = pkg_b.join("test/counter_test.bt");
        fs::write(&test_file_b, "").unwrap();
        let (class_idx, _) = class_index_for_file(
            &test_file_b,
            &pkg_class_indexes,
            &fixture_index,
            &merged,
            &merged_super,
        );
        assert_eq!(
            class_idx.get("Counter").map(String::as_str),
            Some("bt@pkg_b@counter")
        );
    }

    #[test]
    fn test_class_index_for_file_fixture_classes_available() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let pkg_a = dir.join("pkg_a");
        fs::create_dir_all(pkg_a.join("test")).unwrap();
        fs::write(
            pkg_a.join("beamtalk.toml"),
            "[package]\nname = \"pkg_a\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let mut pkg_a_class_index: HashMap<String, String> = HashMap::new();
        pkg_a_class_index.insert("Counter".to_string(), "bt@pkg_a@counter".to_string());

        let mut pkg_class_indexes: PkgClassIndexes = HashMap::new();
        pkg_class_indexes.insert(canonical_path(&pkg_a), (pkg_a_class_index, HashMap::new()));

        let mut fixture_index: HashMap<String, String> = HashMap::new();
        fixture_index.insert("MockHelper".to_string(), "bt@mock_helper".to_string());

        let merged: HashMap<String, String> = HashMap::new();
        let merged_super: HashMap<String, String> = HashMap::new();

        let test_file = pkg_a.join("test/my_test.bt");
        fs::write(&test_file, "").unwrap();
        let (class_idx, _) = class_index_for_file(
            &test_file,
            &pkg_class_indexes,
            &fixture_index,
            &merged,
            &merged_super,
        );
        // Own class is accessible
        assert_eq!(
            class_idx.get("Counter").map(String::as_str),
            Some("bt@pkg_a@counter")
        );
        // Fixture class is also accessible
        assert_eq!(
            class_idx.get("MockHelper").map(String::as_str),
            Some("bt@mock_helper")
        );
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
            has_setup_once: false,
            has_teardown_once: false,
        };

        let wrapper = generate_eunit_wrapper(&test_class, "test/my_test.bt");
        // BT-1293: setUp return is validated before use — check the new pattern
        assert!(wrapper.contains("SetUpResult = 'bt@my_test':dispatch('setUp', [], Instance)"));
        assert!(
            wrapper.contains("beamtalk_test_case:is_valid_setUp_result(Instance, SetUpResult)")
        );
        assert!(wrapper.contains("Instance1 = case beamtalk_test_case:is_valid_setUp_result"));
        assert!(wrapper.contains("true -> SetUpResult"));
        assert!(wrapper.contains("false -> Instance"));
        assert!(wrapper.contains("try"));
        assert!(wrapper.contains("after"));
        assert!(wrapper.contains("dispatch('tearDown', [], Instance1)"));
        // Both test methods present
        assert!(wrapper.contains("'testA_test_'()"));
        assert!(wrapper.contains("'testB_test_'()"));
        // BT-1353: skip handling must catch both forms even with tearDown
        assert!(wrapper.contains("throw:{bunit_skip, _}"));
        assert!(
            wrapper.contains("error:#{error := {beamtalk_error, bunit_skip,"),
            "EUnit wrapper with tearDown must catch error-form bunit_skip"
        );
    }

    /// Inserting `"."` and the absolute CWD into `seen` without canonicalization would
    /// treat them as different entries, making a single-package run appear to have two
    /// packages and incorrectly triggering multi-package module-name prefixing.
    ///
    /// This test verifies that the canonicalization in `discovered_packages` deduplicates
    /// the relative `"."` and the same path expressed as an absolute path.
    #[test]
    fn test_canonical_dedup_relative_and_absolute_same_path() {
        // Get the absolute CWD — this is the value that `find_package_root()` would
        // return for a file inside the current directory, while the production code
        // also inserts `"."` (relative). Both must canonicalize to the same path so
        // they deduplicate in `seen` and don't incorrectly flip multi-package mode.
        let cwd_abs = Utf8PathBuf::from_path_buf(std::env::current_dir().unwrap()).unwrap();

        let a = canonical_path(Utf8Path::new("."));
        let b = canonical_path(&cwd_abs);

        assert_eq!(
            a, b,
            "'.' and the absolute CWD should canonicalize to the same path"
        );

        // Inserting both into a seen-set must yield exactly 1 entry.
        let mut seen: HashSet<Utf8PathBuf> = HashSet::new();
        seen.insert(a);
        seen.insert(b);
        assert_eq!(
            seen.len(),
            1,
            "relative '.' and absolute CWD must deduplicate in the seen-set"
        );
    }

    /// When two packages each define a class with the same name (e.g. `SmokeTest`),
    /// the multi-package run would compile both to `bt@smoke_test.beam` in the same
    /// temp build dir, with the second silently overwriting the first. The fix:
    /// when `discovered_packages.len() > 1`, prefix each test module name with the
    /// owning package name (e.g. `bt@pkg_a@smoke_test` and `bt@pkg_b@smoke_test`).
    ///
    /// This test verifies the renaming logic that produces those unique names.
    #[test]
    fn test_module_name_prefixed_for_multi_package_collision_avoidance() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Two packages, each with a SmokeTest class
        let pkg_a = dir.join("pkg_a");
        let pkg_b = dir.join("pkg_b");
        for pkg in [&pkg_a, &pkg_b] {
            fs::create_dir_all(pkg.join("test")).unwrap();
        }
        fs::write(
            pkg_a.join("beamtalk.toml"),
            "[package]\nname = \"pkg_a\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(
            pkg_b.join("beamtalk.toml"),
            "[package]\nname = \"pkg_b\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let file_a = pkg_a.join("test/smoke_test.bt");
        let file_b = pkg_b.join("test/smoke_test.bt");
        for f in [&file_a, &file_b] {
            fs::write(
                f,
                "TestCase subclass: SmokeTest\n  testOk => self assert: true\n",
            )
            .unwrap();
        }

        // Build pkg_root_to_name as the production code does (with canonicalized keys)
        let pkg_root_to_name: HashMap<Utf8PathBuf, String> = [
            (canonical_path(&pkg_a), "pkg_a".to_string()),
            (canonical_path(&pkg_b), "pkg_b".to_string()),
        ]
        .into_iter()
        .collect();

        // Simulate the override applied in run_tests when len > 1
        for (file, expected_module) in [
            (&file_a, "bt@pkg_a@smoke_test"),
            (&file_b, "bt@pkg_b@smoke_test"),
        ] {
            let (mut test_classes, _) = discover_test_classes(file).unwrap();
            // Default (single-pkg) name
            assert_eq!(test_classes[0].module_name, "bt@smoke_test");

            // Apply multi-package prefix (mirrors run_tests logic)
            let pkg_name_slug = canonical_package_root(file)
                .and_then(|r| pkg_root_to_name.get(&r))
                .map(|n| beamtalk_core::codegen::core_erlang::to_module_name(n));

            if let Some(ref prefix) = pkg_name_slug {
                for tc in &mut test_classes {
                    let stem = beamtalk_core::codegen::core_erlang::to_module_name(&tc.class_name);
                    tc.module_name = format!("bt@{prefix}@{stem}");
                }
            }

            assert_eq!(test_classes[0].module_name, expected_module);
        }
    }
}
