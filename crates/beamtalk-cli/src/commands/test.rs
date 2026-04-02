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
use beamtalk_core::file_walker::FileWalker;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;
use std::fs;
use std::time::Instant;
use tracing::{debug, info, instrument, warn};

use super::build_layout::BuildLayout;
use super::manifest;

/// Per-package class module indexes: package root → (`class_module_index`, `class_superclass_index`).
type PkgClassIndexes = HashMap<Utf8PathBuf, (HashMap<String, String>, HashMap<String, String>)>;

// ──────────────────────────────────────────────────────────────────────────
// Test discovery from AST
// ──────────────────────────────────────────────────────────────────────────

/// Metadata about a `TestCase` subclass discovered in a `.bt` file.
#[derive(Debug)]
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
    // BT-1631: Removed has_setup, has_teardown, has_setup_once, has_teardown_once, is_serial
    // fields — test lifecycle and serial detection are now handled entirely by
    // beamtalk_test_runner in the Erlang runtime.
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

        for method in &class.methods {
            let selector_name = method.selector.name();
            if selector_name.starts_with("test") {
                test_methods.push(selector_name.to_string());
            }
        }

        // BT-1631: Removed setUp/tearDown/serial detection — the BUnit runner
        // (beamtalk_test_runner) handles all lifecycle and serialization in Erlang.

        if !test_methods.is_empty() {
            test_classes.push(TestCaseClass {
                class_name,
                superclass_name: class.superclass_name().to_string(),
                module_name,
                test_methods,
            });
        }
    }

    Ok((test_classes, load_files))
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
    pre_loaded_classes: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
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
            pre_loaded_classes,
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
    pre_loaded_classes: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
) -> Result<String> {
    let module_name = fixture_module_name(fixture_path)?;

    let core_file = generate_core_file(
        fixture_path,
        &module_name,
        output_dir,
        class_module_index,
        class_superclass_index,
        warnings_as_errors,
        pre_loaded_classes,
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

/// Build class → module name and class → superclass indexes from fixture files.
///
/// Parses each fixture file to extract class names and superclass relationships,
/// then maps each class name to its module name (e.g. `"Env"` → `"bt@env"` for
/// `fixtures/scheme/env.bt`) and to its superclass name (e.g. `"ValueSubCircle"`
/// → `"ValueBaseShape"`).
///
/// Both indexes are merged into the pipeline before fixture compilation so that
/// cross-file references and class hierarchy resolution work correctly — in
/// particular, Value sub-subclasses are recognized as value types rather than
/// defaulting to actor codegen (BT-1564).
#[allow(clippy::type_complexity)]
fn build_fixture_class_indexes(
    fixture_files: &[Utf8PathBuf],
) -> Result<(
    HashMap<String, String>,
    HashMap<String, String>,
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
)> {
    let mut module_index = HashMap::new();
    let mut superclass_index = HashMap::new();
    let mut class_infos = Vec::new();

    for file in fixture_files {
        let module_name = fixture_module_name(file)?;
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);

        // Extract full ClassInfo for validator/type checker resolution.
        class_infos
            .extend(beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module));

        for class in &module.classes {
            let class_name = class.name.name.to_string();
            module_index.insert(class_name.clone(), module_name.clone());
            let superclass_name = class.superclass_name();
            if superclass_name != "none" {
                superclass_index.insert(class_name, superclass_name.to_string());
            }
        }
    }

    Ok((module_index, superclass_index, class_infos))
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
    pre_loaded_classes: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
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
        pre_loaded_classes,
        None,
        None,  // TODO: pass dep_registry for test compilation with deps
        false, // Test compilation doesn't use strict-deps
    )
    .wrap_err_with(|| format!("Failed to compile '{source_path}'"))?;

    Ok(core_file)
}

// ──────────────────────────────────────────────────────────────────────────
// BUnit runner (BT-1631)
// ──────────────────────────────────────────────────────────────────────────

/// Result from running `BUnit` tests via `beamtalk_test_runner:run_all/1`.
///
/// Holds the parsed JSON result containing aggregated test metrics and per-test details.
#[derive(Debug)]
struct BunitResult {
    /// Total number of tests across all classes.
    total: usize,
    /// Number of passing tests.
    passed: usize,
    /// Number of failing tests.
    failed: usize,
    /// Number of skipped tests.
    #[allow(dead_code)] // populated for future skip-reason display
    skipped: usize,
    /// Total duration in seconds.
    #[allow(dead_code)] // populated for future per-class duration display
    duration: f64,
    /// Per-test result details (name, status, optional error).
    tests: Vec<TestResultDetail>,
}

#[derive(Debug)]
struct TestResultDetail {
    name: String,
    status: String, // "pass", "fail", "skip"
    error: Option<String>,
}

/// Parsed per-module `EUnit` output sections, split by `NATIVE_EUNIT_DONE` markers.
struct EunitModuleSections {
    /// Per-module results: `(module_name, passed, failed, skipped)`.
    modules: Vec<(String, usize, usize, usize)>,
    /// Total passing tests across all modules.
    total_passed: usize,
    /// Total failing tests across all modules.
    total_failed: usize,
    /// Total skipped tests across all modules.
    total_skipped: usize,
    /// Captured `EUnit` output from failing modules.
    eunit_output: String,
}

/// Result from running native `EUnit` tests.
#[derive(Debug, Default)]
struct NativeEunitResult {
    /// Total number of native `EUnit` tests (passed + failed + skipped).
    total: usize,
    /// Number of passing tests.
    passed: usize,
    /// Number of failing tests.
    failed: usize,
    /// Number of skipped tests.
    #[allow(dead_code)] // populated for future skip-reason display
    skipped: usize,
    /// Per-module results: `(module_name, passed, failed, skipped)`.
    modules: Vec<(String, usize, usize, usize)>,
    /// Captured `EUnit` output (failures, errors).
    output: String,
}

/// Combined test results from `BUnit` and native `EUnit` runs.
#[derive(Debug)]
struct TestResults {
    bunit: Option<BunitResult>,
    native: Option<NativeEunitResult>,
}

/// Build the effective class index for a test file: base classes + fixtures.
///
/// For single-package runs, uses the merged index. For multi-package runs,
/// uses the per-package index for the test file's package. Fixture classes
/// are merged last to override base classes if needed.
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

    // Merge base classes with fixture classes to build the effective class index
    // for this test file. Fixture classes are merged last so they can override
    // any base class (though in practice fixtures should be in a different namespace).
    let mut file_class_index = base_class;
    file_class_index.extend(fixture_class_index.clone());

    let mut file_super_index = base_super;
    file_super_index.extend(
        fixture_class_index
            .iter()
            .filter_map(|(class_name, _module_name)| {
                merged_super_index
                    .get(class_name)
                    .map(|s| (class_name.clone(), s.clone()))
            }),
    );

    (file_class_index, file_super_index)
}

// ──────────────────────────────────────────────────────────────────────────
// File discovery
// ──────────────────────────────────────────────────────────────────────────

/// Find all `.bt` files in a directory.
fn find_test_files(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    FileWalker::test_files().walk(dir)
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
    pre_loaded_classes: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
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
        pre_loaded_classes,
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
    #[allow(dead_code)] // TODO: used when doc tests are migrated to BUnit runner
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
#[allow(dead_code)] // fields populated during discovery for future per-file reporting
struct CompiledTest {
    /// Source file path.
    source_file: Utf8PathBuf,
    /// `TestCase` class info.
    test_class: TestCaseClass,
}

/// Compiled doc test metadata, ready for execution.
#[allow(dead_code)] // fields populated during discovery; doc tests TODO for BUnit migration
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
    /// BT-1559: Cross-file class metadata for type checker hierarchy resolution.
    all_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// Fixture class-name to module-name index.
    fixture_class_index: HashMap<String, String>,
    /// Module names of pre-compiled fixtures (from `fixtures/` directory).
    precompiled_modules: HashSet<String>,
    /// All fixture module names (pre-compiled + `@load`-compiled).
    all_fixture_modules: Vec<String>,
    /// Compiled `BUnit` test metadata.
    compiled_tests: Vec<CompiledTest>,
    /// Compiled doc test metadata.
    compiled_doc_tests: Vec<CompiledDocTest>,
    /// Package BEAM module names (from built packages).
    package_modules: Vec<String>,
    /// Package ebin directories (for code path).
    package_ebin_dirs: Vec<Utf8PathBuf>,
    /// Max concurrent test classes (0 = auto, 1 = sequential).
    jobs: usize,
    /// ADR 0072: Hex dependency names that need OTP app startup before tests.
    hex_dep_names: Vec<String>,
    /// Native `EUnit` test module names (modules ending in `_test` or `_tests`).
    native_test_modules: Vec<String>,
}

/// Run `BUnit` tests.
///
/// Discovers `TestCase` subclasses in `.bt` files, compiles them, generates
/// `EUnit` wrappers, and runs all tests in a single BEAM process.
///
/// `jobs` controls test class concurrency: `0` = auto (scheduler count),
/// `1` = sequential, `N` = up to N concurrent classes.
#[instrument(skip_all)]
pub fn run_tests(path: &str, warnings_as_errors: bool, jobs: usize) -> Result<()> {
    info!("Starting BUnit test run");
    let start_time = Instant::now();

    let test_path = Utf8PathBuf::from(path);

    // Determine if path is a single file or directory.
    // Allow empty test_files — native-only packages have no .bt test files
    // but still have native/test/*.erl to discover later.
    let test_files = if test_path.is_file() {
        vec![test_path.clone()]
    } else if test_path.is_dir() {
        find_test_files(&test_path)?
    } else {
        miette::bail!("Test path '{}' not found", test_path);
    };

    // Create temporary build directory
    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory")?;
    let build_dir = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;

    let mut pipeline =
        initialize_pipeline(test_path, test_files, build_dir, warnings_as_errors, jobs)?;
    compile_fixtures(&mut pipeline)?;
    discover_and_compile_tests(&mut pipeline)?;

    // Cheap pre-scan: check if any package might have native test files
    // so we can skip build_packages entirely when there are no tests at all.
    let has_native_test_dir = pipeline.discovered_packages.iter().any(|(pkg_root, _)| {
        let native_test_dir = pkg_root.join("native").join("test");
        native_test_dir.is_dir()
    });

    if pipeline.compiled_tests.is_empty()
        && pipeline.compiled_doc_tests.is_empty()
        && !has_native_test_dir
    {
        println!("No tests found");
        return Ok(());
    }

    build_packages(&mut pipeline)?;

    if pipeline.compiled_tests.is_empty()
        && pipeline.compiled_doc_tests.is_empty()
        && pipeline.native_test_modules.is_empty()
    {
        println!("No tests found");
        return Ok(());
    }

    let results = execute_tests(&pipeline)?;
    report_results(&pipeline, &results, start_time)
}

/// Discover unique package roots and load their manifests.
///
/// Walks up from each test file's directory, seeds from CWD and `test_path`,
/// and deduplicates by canonical path so relative and absolute references to
/// the same directory compare equal.
fn discover_packages_with_manifests(
    test_path: &Utf8Path,
    test_files: &[Utf8PathBuf],
) -> Result<Vec<(Utf8PathBuf, manifest::PackageManifest)>> {
    let mut roots: Vec<Utf8PathBuf> = Vec::new();
    let mut seen: HashSet<Utf8PathBuf> = HashSet::new();

    // Check CWD first
    let cwd = canonical_path(Utf8Path::new("."));
    if seen.insert(cwd.clone()) {
        roots.push(cwd);
    }

    // Also seed from the test_path itself when it's a directory.
    // This ensures `beamtalk test path/to/pkg` from a parent directory
    // discovers the package even when it has no .bt test files
    // (native-only packages).
    if test_path.is_dir() {
        let tp = canonical_path(test_path);
        if seen.insert(tp.clone()) {
            roots.push(tp);
        }
    }

    // Walk up from each test file to find its package root
    for test_file in test_files {
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
    Ok(pkgs)
}

/// Build per-package class indexes and a merged combined index.
///
/// Per-package indexes let each test file see only its own package's classes,
/// preventing false cross-package class name collisions when running tests
/// from a parent directory containing multiple packages. The merged index is
/// used for fixture compilation (which can reference any package's classes).
///
/// Returns `(pkg_class_indexes, class_module_index, class_superclass_index, all_class_infos)`.
fn build_merged_class_indexes(
    discovered_packages: &[(Utf8PathBuf, manifest::PackageManifest)],
) -> (
    PkgClassIndexes,
    HashMap<String, String>,
    HashMap<String, String>,
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
) {
    let mut pkg_class_indexes: PkgClassIndexes = HashMap::new();
    let mut class_module_index: HashMap<String, String> = HashMap::new();
    let mut class_superclass_index: HashMap<String, String> = HashMap::new();
    // BT-1733: Collect source and dep ClassInfos separately, then merge
    // via collect_all_class_infos for a single unified collection point.
    let mut source_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> =
        Vec::new();
    let mut dep_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> =
        Vec::new();
    for (pkg_root, pkg) in discovered_packages {
        let src_dir = pkg_root.join("src");
        if let Ok(src_files) = super::build::collect_source_files_from_dir(&src_dir) {
            let source_root = src_dir.exists().then_some(src_dir);
            if let Ok((pkg_class_map, pkg_super_map, class_infos, _cached_asts)) =
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
                source_class_infos.extend(class_infos);
            }
        }

        // Load dependency class metadata so the type checker and validator
        // can resolve cross-package class references in test files.
        let dep_options = beamtalk_core::CompilerOptions::default();
        match super::deps::ensure_deps_resolved(pkg_root, &dep_options) {
            Ok(resolved_deps) => {
                for dep in &resolved_deps {
                    for (class_name, module_name) in &dep.class_module_index {
                        class_module_index.insert(class_name.clone(), module_name.clone());
                    }
                    dep_class_infos.extend(dep.class_infos.clone());
                }
            }
            Err(e) => {
                warn!(
                    error = %e,
                    "Failed to resolve dependencies for test pipeline; \
                     dependency classes may not be available"
                );
            }
        }
    }

    // BT-1733: Single unified collection of all ClassInfo from all sources.
    // Fixture ClassInfo is added later in compile_fixtures() via
    // pipeline.all_class_infos.extend(fixture_class_infos).
    // To add a new .bt source location, add its ClassInfo slice here.
    let all_class_infos =
        super::build::collect_all_class_infos(&[&source_class_infos, &dep_class_infos]);

    (
        pkg_class_indexes,
        class_module_index,
        class_superclass_index,
        all_class_infos,
    )
}

/// Initialize the test pipeline: discover packages and build class indexes.
fn initialize_pipeline(
    test_path: Utf8PathBuf,
    test_files: Vec<Utf8PathBuf>,
    build_dir: Utf8PathBuf,
    warnings_as_errors: bool,
    jobs: usize,
) -> Result<TestPipeline> {
    let discovered_packages = discover_packages_with_manifests(&test_path, &test_files)?;

    // Map from package root -> package name for test module naming.
    // Used to prefix test module names with the package name when multiple
    // packages are in scope, preventing module collisions (e.g. two packages
    // that both define a `SmokeTest` class would otherwise both compile to
    // `bt@smoke_test.beam` in the shared temp build dir).
    let pkg_root_to_name: HashMap<Utf8PathBuf, String> = discovered_packages
        .iter()
        .map(|(root, pkg)| (root.clone(), pkg.name.clone()))
        .collect();

    let (pkg_class_indexes, class_module_index, class_superclass_index, all_class_infos) =
        build_merged_class_indexes(&discovered_packages);

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
        all_class_infos,
        fixture_class_index: HashMap::new(),
        precompiled_modules: HashSet::new(),
        all_fixture_modules: Vec::new(),
        compiled_tests: Vec::new(),
        compiled_doc_tests: Vec::new(),
        package_modules: Vec::new(),
        package_ebin_dirs: Vec::new(),
        jobs,
        hex_dep_names: Vec::new(),
        native_test_modules: Vec::new(),
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
    // BT-1564: Also build a fixture superclass index so the class hierarchy
    // resolves correctly for Value sub-subclasses defined across fixture files.
    let (fixture_class_index, fixture_superclass_index, fixture_class_infos) =
        if fixtures_dir.is_dir() {
            let fixture_files = find_test_files(&fixtures_dir)?;
            build_fixture_class_indexes(&fixture_files)?
        } else {
            (HashMap::new(), HashMap::new(), Vec::new())
        };
    pipeline
        .class_module_index
        .extend(fixture_class_index.clone());
    pipeline
        .class_superclass_index
        .extend(fixture_superclass_index);
    // BT-1736: Fixture ClassInfo is now included in all_class_infos so the type
    // checker can validate cross-file references in test files. The Shape class
    // name collision between abstract_shape.bt and class_method_self_new.bt was
    // resolved by renaming abstract_shape's class to AbstractShape.
    pipeline.all_class_infos.extend(fixture_class_infos);

    let precompiled = compile_fixtures_directory(
        &fixtures_dir,
        &pipeline.build_dir,
        &pipeline.class_module_index,
        &pipeline.class_superclass_index,
        pipeline.warnings_as_errors,
        &pipeline.all_class_infos,
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
            &pipeline.all_class_infos,
        )
        .wrap_err_with(|| format!("Failed to compile test file '{test_file}'"))?;
        pending_test_cores.push(core_file);

        // BT-1631: No longer generate EUnit wrappers; beamtalk_test_runner:run_all/1
        // handles test discovery and execution directly in the Erlang runtime.
        pipeline.compiled_tests.push(CompiledTest {
            source_file: test_file.to_path_buf(),
            test_class,
        });
    }

    // Discover doc tests in the same file
    let doc_results = discover_and_compile_doc_tests(
        test_file,
        &pipeline.build_dir,
        &file_class_index,
        &file_super_index,
        pipeline.warnings_as_errors,
        &pipeline.all_class_infos,
    )?;
    for dr in doc_results {
        // BT-1631: Doc test EUnit wrappers are still generated but not compiled here.
        // TODO: Migrate doc tests to use BUnit runner directly.
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
            &pipeline.all_class_infos,
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
        let pkg_layout = BuildLayout::new(pkg_root);
        let ebin_dir = pkg_layout.ebin_dir();
        println!("Building package '{}'...", pkg.name);
        let build_options = beamtalk_core::CompilerOptions {
            stdlib_mode: false,
            allow_primitives: false,
            workspace_mode: false,
            warnings_as_errors: pipeline.warnings_as_errors,
            ..Default::default()
        };
        super::build::build(pkg_root.as_str(), &build_options, false).wrap_err_with(|| {
            format!(
                "Failed to build package '{}' before running tests",
                pkg.name
            )
        })?;
        let modules = collect_beam_module_names(&ebin_dir)?;
        debug!(count = modules.len(), "Discovered package modules");
        pipeline.package_modules.extend(modules);

        // BT-1750: Use BeamEnvironment for unified code path and OTP app collection.
        // The environment includes package ebin, dep ebins, native ebin, rebar3 ebins.
        let beam_env =
            super::beam_environment::BeamEnvironment::from_layout(&pkg_layout, pkg_root)?;

        // ADR 0070: Discover dep modules for on_load class registration.
        // Only BT dependency ebins need explicit module loading — native and
        // rebar3 ebins contain plain Erlang modules that load on demand.
        for dep_ebin in super::deps::collect_dep_ebin_paths(&pkg_layout) {
            if let Ok(dep_modules) = collect_beam_module_names(&dep_ebin) {
                pipeline.package_modules.extend(dep_modules);
            }
        }

        pipeline.package_ebin_dirs.extend(beam_env.code_paths);
        for name in beam_env.otp_apps {
            if !pipeline.hex_dep_names.contains(&name) {
                pipeline.hex_dep_names.push(name);
            }
        }

        // Compile native/test/*.erl test helpers via erlc.
        // These are Erlang modules only needed during test runs (e.g. test
        // servers, mocks). They go into the same native ebin directory.
        let native_test_dir = pkg_root.join("native").join("test");
        if native_test_dir.exists() && native_test_dir.is_dir() {
            compile_native_test_erlang(pkg_root, &native_test_dir, &pipeline.package_ebin_dirs)?;

            // Discover native EUnit test modules (_test or _tests suffix)
            // from source files to avoid picking up stale .beam artifacts.
            // Skip duplicates (BEAM code path is global, first module wins).
            for erl_file in fs::read_dir(&native_test_dir)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read {native_test_dir}"))?
            {
                let entry = erl_file.into_diagnostic()?;
                let path = entry.path();
                if path.is_file() && path.extension().is_some_and(|ext| ext == "erl") {
                    if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                        if stem.ends_with("_test") || stem.ends_with("_tests") {
                            let module = stem.to_string();
                            if pipeline.native_test_modules.contains(&module) {
                                eprintln!(
                                    "Warning: duplicate native test module '{}' in package \
                                     '{}' — skipping (already discovered in another package)",
                                    module, pkg.name
                                );
                            } else {
                                pipeline.native_test_modules.push(module);
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

/// Compile native Erlang test helpers from `native/test/` via erlc.
///
/// These modules (test servers, mocks, etc.) are only needed during test runs.
/// Output goes to `_build/dev/native/ebin/` alongside the regular native modules.
fn compile_native_test_erlang(
    pkg_root: &Utf8Path,
    native_test_dir: &Utf8Path,
    ebin_dirs: &[Utf8PathBuf],
) -> Result<()> {
    use std::fs;

    let mut erl_files: Vec<Utf8PathBuf> = Vec::new();
    let entries = fs::read_dir(native_test_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read {native_test_dir}"))?;
    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "erl") {
            let utf8 = Utf8PathBuf::from_path_buf(path)
                .map_err(|p| miette::miette!("Non-UTF-8 path: {}", p.display()))?;
            erl_files.push(utf8);
        }
    }

    if erl_files.is_empty() {
        return Ok(());
    }

    // Sort for deterministic compilation order across platforms.
    erl_files.sort();

    let test_layout = BuildLayout::new(pkg_root);
    let ebin_dir = test_layout.native_ebin_dir();
    fs::create_dir_all(&ebin_dir)
        .into_diagnostic()
        .wrap_err("Failed to create native ebin dir for test helpers")?;

    debug!(
        count = erl_files.len(),
        "Compiling native test Erlang helpers"
    );

    beamtalk_cli::erlc::ErlcInvocation::new(&ebin_dir)
        .debug_info()
        .erl_libs(&test_layout.rebar_lib_dir())
        .code_paths(ebin_dirs)
        .runtime_include()
        .include_dir(pkg_root.join("native").join("include"))
        .source_files(&erl_files)
        .run("Native test Erlang compilation")?;

    Ok(())
}

/// Phase 3: Run all tests (`BUnit` + native `EUnit`).
///
/// Runs `BUnit` tests via `beamtalk_test_runner:run_all(Jobs)` if any `BUnit`
/// tests were compiled, then runs native `EUnit` test modules if any were
/// discovered in `native/test/`.
fn execute_tests(pipeline: &TestPipeline) -> Result<TestResults> {
    println!("Running tests...\n");

    let bunit = if pipeline.compiled_tests.is_empty() && pipeline.compiled_doc_tests.is_empty() {
        None
    } else {
        Some(run_bunit_tests(pipeline)?)
    };

    let native = if pipeline.native_test_modules.is_empty() {
        None
    } else {
        Some(run_native_eunit_tests(pipeline)?)
    };

    Ok(TestResults { bunit, native })
}

/// Run tests via `beamtalk_test_runner:run_all(Jobs)` in a single BEAM process.
///
/// Build Erlang `ensure_loaded_or_warn` calls for fixtures, packages, and test modules.
///
/// Returns a tuple of `(fixture_load_cmd, package_load_cmd, test_load_cmd)` strings
/// ready for inclusion in the `-eval` command. Each string is empty or ends with `, `
/// so they can be concatenated directly.
fn build_load_commands(pipeline: &TestPipeline) -> (String, String, String) {
    // Build fixture loading commands
    // BT-1732: Use ensure_loaded_or_warn for consistent on_load failure reporting.
    let fixture_load_cmd = if pipeline.all_fixture_modules.is_empty() {
        String::new()
    } else {
        pipeline
            .all_fixture_modules
            .iter()
            .map(|m| format!("beamtalk_test_runner:ensure_loaded_or_warn('{m}')"))
            .collect::<Vec<_>>()
            .join(", ")
            + ", "
    };

    // Build package module loading commands (triggers on_load -> class registration)
    // BT-1732: Check return values and report which class failed to load and why.
    let package_load_cmd = if pipeline.package_modules.is_empty() {
        String::new()
    } else {
        pipeline
            .package_modules
            .iter()
            .map(|m| format!("beamtalk_test_runner:ensure_loaded_or_warn('{m}')"))
            .collect::<Vec<_>>()
            .join(", ")
            + ", "
    };

    // Build test module loading commands (triggers on_load -> class registration)
    // BT-1732: Check return values and report which test class failed to load.
    let test_load_cmd = if pipeline.compiled_tests.is_empty() {
        String::new()
    } else {
        pipeline
            .compiled_tests
            .iter()
            .map(|t| {
                format!(
                    "beamtalk_test_runner:ensure_loaded_or_warn('{}')",
                    t.test_class.module_name
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
            + ", "
    };

    (fixture_load_cmd, package_load_cmd, test_load_cmd)
}

/// Build a BEAM command (`erl -noshell`) with `-pa` paths for the build dir,
/// package ebin dirs, and runtime paths, then appends the `-eval` command.
///
/// Shared by both `BUnit` and native `EUnit` test runners.
fn build_beam_test_command(
    pipeline: &TestPipeline,
    pa_args: &[std::ffi::OsString],
    eval_cmd: &str,
) -> std::process::Command {
    let mut cmd = std::process::Command::new("erl");
    #[cfg(windows)]
    {
        let build_dir_path = pipeline.build_dir.as_str().replace('\\', "/");
        cmd.arg("-noshell").arg("-pa").arg(build_dir_path);
    }
    #[cfg(not(windows))]
    {
        cmd.arg("-noshell")
            .arg("-pa")
            .arg(pipeline.build_dir.as_str());
    }

    // Add all package ebin directories to code path
    for ebin_dir in &pipeline.package_ebin_dirs {
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

    for arg in pa_args {
        cmd.arg(arg);
    }

    cmd.arg("-eval").arg(eval_cmd);
    cmd
}

/// Parse `BUnit` runner JSON output into a `BunitResult`.
///
/// Extracts the JSON line from stdout (skipping Erlang error/info reports),
/// deserializes the summary fields (`total`, `passed`, `failed`, `skipped`,
/// `duration`) and the `tests` array into structured results.
fn parse_bunit_result_json(stdout: &str, stderr: &str) -> Result<BunitResult> {
    // Extract JSON from stdout -- skip any Erlang error/info reports that may precede it.
    // The JSON line starts with `{"` (a JSON object key). Using `{"` instead of just `{`
    // avoids matching Erlang error tuples like `{error, {beamtalk_error,...}}` which also
    // start with `{` but are not valid JSON.
    let json_line = stdout
        .lines()
        .find(|line| line.starts_with("{\""))
        .ok_or_else(|| {
            let combined = format!("{stdout}\n{stderr}");
            miette::miette!("No JSON output from BUnit runner:\n{combined}")
        })?;

    // Parse JSON result
    let result_json: serde_json::Value = serde_json::from_str(json_line)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to parse BUnit JSON result: {json_line}"))?;

    // Deserialize into BunitResult
    let parse_usize = |key: &str| -> Result<usize> {
        result_json[key]
            .as_u64()
            .map(|v| usize::try_from(v).unwrap_or(usize::MAX))
            .ok_or_else(|| miette::miette!("Missing '{key}' in BUnit result"))
    };
    let total = parse_usize("total")?;
    let passed = parse_usize("passed")?;
    let failed = parse_usize("failed")?;
    let skipped = parse_usize("skipped")?;
    let duration = result_json["duration"]
        .as_f64()
        .ok_or_else(|| miette::miette!("Missing 'duration' in BUnit result"))?;

    let tests_array = result_json["tests"]
        .as_array()
        .ok_or_else(|| miette::miette!("Missing 'tests' array in BUnit result"))?;

    let tests = tests_array
        .iter()
        .map(|t| {
            let name = t["name"].as_str().unwrap_or("unknown").to_string();
            let status = t["status"].as_str().unwrap_or("unknown").to_string();
            let error = t["error"].as_str().map(String::from);
            TestResultDetail {
                name,
                status,
                error,
            }
        })
        .collect();

    Ok(BunitResult {
        total,
        passed,
        failed,
        skipped,
        duration,
        tests,
    })
}

/// Calls the `BUnit` runner directly, bypassing `EUnit` wrappers. The runner
/// discovers test classes, executes them with the specified concurrency level,
/// and returns aggregated results as JSON.
fn run_bunit_tests(pipeline: &TestPipeline) -> Result<BunitResult> {
    debug!("Running BUnit tests with jobs={}", pipeline.jobs);

    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()
        .wrap_err("Cannot find Erlang runtime directory")?;
    let beam_paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    let pa_args = beamtalk_cli::repl_startup::beam_pa_args(&beam_paths);

    let (fixture_load_cmd, package_load_cmd, test_load_cmd) = build_load_commands(pipeline);

    // ADR 0072: Start hex dep OTP applications before tests
    let hex_deps_start_cmd =
        beamtalk_cli::repl_startup::hex_deps_start_fragment(&pipeline.hex_dep_names);

    // Call beamtalk_test_runner:run_all(Jobs) and serialize result to JSON
    let eval_cmd = format!(
        "{{ok, _}} = application:ensure_all_started(beamtalk_stdlib), \
         {hex_deps_start_cmd}\
         {package_load_cmd}\
         {fixture_load_cmd}\
         {test_load_cmd}\
         Result = beamtalk_test_runner:run_all({jobs}), \
         JSON = beamtalk_test_runner:result_to_json(Result), \
         io:format(\"~s~n\", [JSON]), \
         init:stop(case beamtalk_test_runner:result_has_passed(Result) of true -> 0; _ -> 1 end).",
        jobs = pipeline.jobs
    );

    let mut cmd = build_beam_test_command(pipeline, &pa_args, &eval_cmd);

    let output = cmd
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run BUnit tests")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    debug!("BUnit stdout: {}", stdout);
    debug!("BUnit stderr: {}", stderr);

    parse_bunit_result_json(&stdout, &stderr)
}

/// Parse `EUnit`'s summary line from verbose output to extract test counts.
///
/// `EUnit` outputs one of two summary formats:
/// - `"  N tests passed."` — all passed
/// - `"  Failed: F.  Skipped: S.  Passed: P."` — mixed results
///
/// Returns `(passed, failed, skipped)`. Falls back to `(0, 0, 0)` if no summary found.
fn parse_eunit_summary(output: &str) -> (usize, usize, usize) {
    for line in output.lines().rev() {
        let trimmed = line.trim();

        // "All N tests passed.", "N tests passed.", or "Test passed." (single test)
        if trimmed.ends_with("tests passed.") || trimmed == "Test passed." {
            if trimmed == "Test passed." {
                return (1, 0, 0);
            }
            if let Some(n_str) = trimmed
                .strip_suffix(" tests passed.")
                .and_then(|s| s.strip_prefix("All ").or(Some(s)))
            {
                if let Ok(n) = n_str.parse::<usize>() {
                    return (n, 0, 0);
                }
            }
        }

        // "Failed: F.  Skipped: S.  Passed: P."
        if trimmed.starts_with("Failed:") && trimmed.contains("Passed:") {
            let mut passed = 0;
            let mut failed = 0;
            let mut skipped = 0;
            for segment in trimmed.split('.') {
                let seg = segment.trim();
                if let Some(val) = seg.strip_prefix("Failed:") {
                    failed = val.trim().parse().unwrap_or(0);
                } else if let Some(val) = seg.strip_prefix("Passed:") {
                    passed = val.trim().parse().unwrap_or(0);
                } else if let Some(val) = seg.strip_prefix("Skipped:") {
                    skipped = val.trim().parse().unwrap_or(0);
                }
            }
            return (passed, failed, skipped);
        }
    }
    (0, 0, 0)
}

/// Parse `EUnit` verbose output into per-module results using `NATIVE_EUNIT_DONE` markers.
///
/// Splits stdout by marker lines to attribute each `EUnit` summary to the correct
/// module. Falls back to counting modules as all-passed or all-failed if no
/// markers are found (e.g. `EUnit` crashed before emitting any output).
///
fn parse_eunit_module_sections(
    stdout: &str,
    success: bool,
    native_test_modules: &[String],
) -> EunitModuleSections {
    let mut modules = Vec::new();
    let mut total_passed: usize = 0;
    let mut total_failed: usize = 0;
    let mut total_skipped: usize = 0;
    let mut eunit_output = String::new();

    // Split output into per-module sections using the marker lines
    let mut current_section = String::new();
    let mut module_idx = 0;
    for line in stdout.lines() {
        if let Some(module_name) = line.strip_prefix("NATIVE_EUNIT_DONE:") {
            // Parse the section for this module's EUnit summary
            let (passed, mut failed, skipped) = parse_eunit_summary(&current_section);

            // If EUnit crashed before printing a summary (e.g. {error, Reason}),
            // parse_eunit_summary returns (0, 0, 0). Detect this by checking the
            // exit status -- if the process failed and no tests were counted, treat
            // the module as having 1 failure.
            if passed == 0 && failed == 0 && skipped == 0 && !success {
                // Check if the section contains EUnit error indicators
                if current_section.contains("*failed*")
                    || current_section.contains("Error in")
                    || current_section.contains("{error,")
                {
                    failed = 1;
                }
            }

            modules.push((module_name.to_string(), passed, failed, skipped));
            total_passed += passed;
            total_failed += failed;
            total_skipped += skipped;

            if failed > 0 {
                eunit_output.push_str(&current_section);
            }

            current_section.clear();
            module_idx += 1;
        } else {
            current_section.push_str(line);
            current_section.push('\n');
        }
    }

    // If no markers found, fall back to counting test modules as all-passed or all-failed
    if module_idx == 0 && !native_test_modules.is_empty() {
        warn!("No NATIVE_EUNIT_DONE markers found in EUnit output");
        for module_name in native_test_modules {
            if success {
                modules.push((module_name.clone(), 1, 0, 0));
                total_passed += 1;
            } else {
                modules.push((module_name.clone(), 0, 1, 0));
                total_failed += 1;
            }
        }
        eunit_output = stdout.to_string();
    }

    EunitModuleSections {
        modules,
        total_passed,
        total_failed,
        total_skipped,
        eunit_output,
    }
}

/// Run native `EUnit` test modules in a BEAM process.
///
/// Executes `eunit:test/2` for each discovered native test module and
/// collects per-module pass/fail counts by parsing `EUnit`'s verbose
/// summary output.
fn run_native_eunit_tests(pipeline: &TestPipeline) -> Result<NativeEunitResult> {
    debug!(
        count = pipeline.native_test_modules.len(),
        "Running native EUnit tests"
    );

    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()
        .wrap_err("Cannot find Erlang runtime directory")?;
    let beam_paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    let pa_args = beamtalk_cli::repl_startup::beam_pa_args(&beam_paths);

    // ADR 0072: Start hex dep OTP applications before tests
    let hex_deps_start_cmd =
        beamtalk_cli::repl_startup::hex_deps_start_fragment(&pipeline.hex_dep_names);

    // Build a list of module atoms for EUnit
    let module_list = pipeline
        .native_test_modules
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    // Inline Erlang: run EUnit per module with `verbose` so per-test
    // pass/fail output is printed to stdout. After each module completes,
    // emit a `NATIVE_EUNIT_DONE:<module>` marker line. The Rust side
    // parses EUnit's own summary lines to get exact pass/fail counts
    // (e.g. "  2 tests passed." or "  Failed: 1.  Skipped: 0.  Passed: 2.")
    // rather than relying on the coarse ok/error return value.
    let eval_cmd = format!(
        "{{ok, _}} = application:ensure_all_started(beamtalk_stdlib), \
         {hex_deps_start_cmd}\
         Mods = [{module_list}], \
         HasFailed = lists:foldl(fun(M, Acc) -> \
             Res = eunit:test(M, [verbose]), \
             io:format(\"NATIVE_EUNIT_DONE:~s~n\", [M]), \
             case Res of ok -> Acc; _ -> true end \
         end, false, Mods), \
         init:stop(case HasFailed of true -> 1; false -> 0 end)."
    );

    let mut cmd = build_beam_test_command(pipeline, &pa_args, &eval_cmd);

    let output = cmd
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run native EUnit tests")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    debug!("Native EUnit stdout: {}", stdout);
    debug!("Native EUnit stderr: {}", stderr);

    let sections = parse_eunit_module_sections(
        &stdout,
        output.status.success(),
        &pipeline.native_test_modules,
    );

    Ok(NativeEunitResult {
        total: sections.total_passed + sections.total_failed + sections.total_skipped,
        passed: sections.total_passed,
        failed: sections.total_failed,
        skipped: sections.total_skipped,
        modules: sections.modules,
        output: sections.eunit_output,
    })
}

/// Phase 4: Aggregate and report test results.
///
/// BT-1631: Displays results from `beamtalk_test_runner:run_all/1` with
/// Beamtalk class names (not Erlang module names) and per-class summary.
/// Also reports native `EUnit` test results when present.
fn report_results(
    pipeline: &TestPipeline,
    results: &TestResults,
    start_time: Instant,
) -> Result<()> {
    let mut failed_details = Vec::new();
    let mut total_tests: usize = 0;
    let mut total_passed: usize = 0;
    let mut total_failed: usize = 0;

    // Report BUnit results
    if let Some(result) = &results.bunit {
        // Group test results by class name (prefix of test name)
        let mut class_results: HashMap<String, (usize, usize, usize)> = HashMap::new();

        for test in &result.tests {
            let test_name = &test.name;
            // Extract class name from test name (class method names are formed as "ClassName testMethod")
            let class_name = if let Some(first_word) = test_name.split_whitespace().next() {
                first_word.to_string()
            } else {
                test_name.clone()
            };

            let (passed, failed, skipped) =
                class_results.entry(class_name.clone()).or_insert((0, 0, 0));

            match test.status.as_str() {
                "pass" => *passed += 1,
                "fail" => {
                    *failed += 1;
                    if let Some(error) = &test.error {
                        failed_details.push(format!("FAIL {test_name}: {error}"));
                    }
                }
                "skip" => *skipped += 1,
                _ => {}
            }
        }

        // Print results per class (matches BUnit output format)
        for compiled in &pipeline.compiled_tests {
            let class_name = &compiled.test_class.class_name;
            if let Some((passed, failed, skipped)) = class_results.get(class_name) {
                let total = passed + failed + skipped;
                if *failed > 0 {
                    println!(
                        "  {class_name}: {total} tests, {passed} passed, {failed} failed \u{2717}"
                    );
                } else {
                    println!("  {class_name}: {total} tests, {total} passed \u{2713}");
                }
            }
        }

        total_tests += result.total;
        total_passed += result.passed;
        total_failed += result.failed;
    }

    // Report native EUnit results
    if let Some(native) = &results.native {
        for (module, passed, failed, skipped) in &native.modules {
            let total = passed + failed + skipped;
            if *failed > 0 {
                println!(
                    "  {module} (native): {total} tests, {passed} passed, {failed} failed \u{2717}"
                );
            } else if *skipped > 0 {
                println!(
                    "  {module} (native): {total} tests, {passed} passed, {skipped} skipped \u{2713}"
                );
            } else {
                println!("  {module} (native): {total} tests, {total} passed \u{2713}");
            }
        }

        if native.failed > 0 && !native.output.is_empty() {
            failed_details.push(native.output.clone());
        }

        total_tests += native.total;
        total_passed += native.passed;
        total_failed += native.failed;
    }

    let file_count = pipeline.test_files.len() + pipeline.native_test_modules.len();
    let elapsed = start_time.elapsed();
    let elapsed_secs = elapsed.as_secs_f64();

    println!();
    if total_failed == 0 {
        println!(
            "{file_count} file(s), {total_tests} tests, {total_passed} passed, 0 failed ({elapsed_secs:.1}s)",
        );
    } else {
        for detail in &failed_details {
            eprintln!("{detail}");
        }
        eprintln!();
        eprintln!(
            "{file_count} file(s), {total_tests} tests, {total_passed} passed, {total_failed} failed ({elapsed_secs:.1}s)",
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
        // BT-1631: setUp/tearDown detection removed — handled by the BUnit runtime.
        // Only test method discovery matters for the CLI now.
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

    // BT-1631: Removed test_generate_eunit_wrapper_simple — EUnit wrappers are no longer generated.
    // Test discovery and execution now use beamtalk_test_runner:run_all/1 directly.

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
        let (module_index, superclass_index, _class_infos) =
            build_fixture_class_indexes(&files).unwrap();
        assert_eq!(
            module_index.get("Counter").map(String::as_str),
            Some("bt@counter")
        );
        assert_eq!(
            superclass_index.get("Counter").map(String::as_str),
            Some("Object")
        );
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
        let (module_index, superclass_index, _class_infos) =
            build_fixture_class_indexes(&files).unwrap();
        assert_eq!(
            module_index.get("SchemeEnv").map(String::as_str),
            Some("bt@env")
        );
        assert_eq!(
            superclass_index.get("SchemeEnv").map(String::as_str),
            Some("Object")
        );
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

    // BT-1631: Removed test_generate_eunit_wrapper_with_lifecycle — EUnit wrappers are no longer generated.

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

    #[test]
    fn test_parse_eunit_summary_all_passed() {
        let output = "======================== EUnit ========================\n\
                      module 'foo_tests'\n\
                        foo_tests: bar_test...ok\n\
                        foo_tests: baz_test...ok\n\
                        [done in 0.006 s]\n\
                      =======================================================\n\
                        All 2 tests passed.\n";
        assert_eq!(parse_eunit_summary(output), (2, 0, 0));
    }

    #[test]
    fn test_parse_eunit_summary_single_test_passed() {
        let output = "  Test passed.\n";
        assert_eq!(parse_eunit_summary(output), (1, 0, 0));
    }

    #[test]
    fn test_parse_eunit_summary_mixed_results() {
        let output = "=======================================================\n\
                        Failed: 1.  Skipped: 0.  Passed: 2.\n";
        assert_eq!(parse_eunit_summary(output), (2, 1, 0));
    }

    #[test]
    fn test_parse_eunit_summary_with_skipped() {
        let output = "=======================================================\n\
                        Failed: 1.  Skipped: 3.  Passed: 5.\n";
        assert_eq!(parse_eunit_summary(output), (5, 1, 3));
    }

    #[test]
    fn test_parse_eunit_summary_no_summary() {
        assert_eq!(parse_eunit_summary("some random output\n"), (0, 0, 0));
    }

    #[test]
    fn test_parse_eunit_summary_61_passed() {
        let output = "  All 61 tests passed.\n";
        assert_eq!(parse_eunit_summary(output), (61, 0, 0));
    }
}
