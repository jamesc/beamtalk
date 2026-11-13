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

use crate::beam_compiler::{
    BeamCompiler, ClassHierarchyContext, CompileContext, compile_source_with_bindings,
};
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::time::Instant;
use tracing::{debug, info, instrument, warn};

use super::build_layout::BuildLayout;
use super::lint::find_package_root;
use super::manifest;
use super::util;

mod execution;
use execution::{TestResults, execute_tests};

/// Per-package class module indexes: package root → (`class_module_index`, `class_superclass_index`).
type PkgClassIndexes = HashMap<Utf8PathBuf, (HashMap<String, String>, HashMap<String, String>)>;

/// [`build_merged_class_indexes`]'s return: per-package indexes, the merged
/// class-name→module-name/superclass-name indexes, and the merged
/// `ClassInfo`/`AliasInfo` collections.
type MergedClassIndexes = (
    PkgClassIndexes,
    HashMap<String, String>,
    HashMap<String, String>,
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    beamtalk_core::compilation::extension_index::ExtensionIndex,
    bool,
);

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
    // No has_setup/has_teardown/has_setup_once/has_teardown_once/is_serial
    // fields — test lifecycle and serial detection are handled entirely by
    // beamtalk_test_runner in the Erlang runtime.
}

/// Discover direct `TestCase` subclasses in a `.bt` file by parsing the AST.
///
/// Returns discovered test classes and any `@load` directives. Equivalent to
/// [`discover_test_classes_in_hierarchy`] with no known superclasses, so a
/// class whose superclass is another (abstract) test case is not found.
pub(crate) fn discover_test_classes(
    source_path: &Utf8Path,
) -> Result<(Vec<TestCaseClass>, Vec<String>)> {
    discover_test_classes_in_hierarchy(source_path, &HashMap::new())
}

/// Whether `superclass` is `TestCase` or, following `superclass_index`
/// (class name → superclass name, e.g. the fixture classes), reaches it.
fn inherits_from_test_case(superclass: &str, superclass_index: &HashMap<String, String>) -> bool {
    let mut current = superclass;
    // Bounded by the index size so a cyclic index cannot loop forever.
    for _ in 0..=superclass_index.len() {
        if current == "TestCase" {
            return true;
        }
        match superclass_index.get(current) {
            Some(next) => current = next,
            None => return false,
        }
    }
    false
}

/// Discover `TestCase` subclasses in a `.bt` file by parsing the AST.
///
/// A class qualifies when its superclass is `TestCase` or reaches it through
/// `superclass_index` (BT-3768: an abstract test case under `fixtures/`
/// holding the `test*` methods, with one-line concrete subclasses in the test
/// files). An indirect subclass qualifies even with no `test*` method of its
/// own: the `BUnit` runtime runs the test methods it inherits from every
/// superclass below `TestCase` (`beamtalk_test_case:test_class_selectors/1`)
/// and skips an `abstract` test class.
///
/// Returns discovered test classes and any `@load` directives.
pub(crate) fn discover_test_classes_in_hierarchy(
    source_path: &Utf8Path,
    superclass_index: &HashMap<String, String>,
) -> Result<(Vec<TestCaseClass>, Vec<String>)> {
    let content = fs::read_to_string(source_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{source_path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&content);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // BT-4304: a file that fails to parse must fail the run with its
    // diagnostics. Parse recovery can drop the class (or its `test*`
    // methods), so ignoring the errors made a broken test file look like one
    // with no tests ("No tests found", exit 0).
    let parse_errors: Vec<_> = diagnostics
        .into_iter()
        .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .collect();
    if !parse_errors.is_empty() {
        crate::diagnostic::print_diagnostics_text(
            &parse_errors,
            source_path.as_str(),
            &content,
            &beamtalk_core::CompilerOptions::default(),
        );
        miette::bail!(
            "Failed to parse test file '{}': {} error(s)",
            source_path,
            parse_errors.len()
        );
    }

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

    // The compiler generates one module per .bt file, from its first
    // TestCase subclass; warn when there are several.
    let candidates = test_case_class_count(&module, superclass_index);
    if candidates > 1 {
        warn!(
            "File '{}' contains {} TestCase subclasses; only the first will be compiled. \
             Split into separate files.",
            source_path, candidates
        );
    }
    let test_classes = test_class_in_module(&module, superclass_index)
        .into_iter()
        .collect();

    Ok((test_classes, load_files))
}

/// How many classes in `module` inherit from `TestCase`.
fn test_case_class_count(
    module: &beamtalk_core::ast::Module,
    superclass_index: &HashMap<String, String>,
) -> usize {
    module
        .classes
        .iter()
        .filter(|c| inherits_from_test_case(c.superclass_name(), superclass_index))
        .count()
}

/// The test class `module` compiles to, if any — the pure part of
/// [`discover_test_classes_in_hierarchy`] (no I/O, no warnings), shared with
/// `collect_test_file_extensions`. The compiler generates one module per
/// `.bt` file from its first `TestCase` subclass, hence at most one.
fn test_class_in_module(
    module: &beamtalk_core::ast::Module,
    superclass_index: &HashMap<String, String>,
) -> Option<TestCaseClass> {
    let class = module
        .classes
        .iter()
        .find(|c| inherits_from_test_case(c.superclass_name(), superclass_index))?;
    let class_name = class.name.name.to_string();
    let stem = beamtalk_codegen::core_erlang::to_module_name(&class_name);
    let module_name = format!("bt@{stem}");

    let test_methods: Vec<String> = class
        .methods
        .iter()
        .map(|method| method.selector.name())
        .filter(|selector| selector.starts_with("test"))
        .map(|selector| selector.to_string())
        .collect();

    // No setUp/tearDown/serial detection here — the BUnit runner
    // (beamtalk_test_runner) handles all lifecycle and serialization in Erlang.

    // An indirect subclass inherits its test methods (see
    // `discover_test_classes_in_hierarchy`'s doc comment).
    if test_methods.is_empty() && class.superclass_name() == "TestCase" {
        return None;
    }
    Some(TestCaseClass {
        class_name,
        superclass_name: class.superclass_name().to_string(),
        module_name,
        test_methods,
    })
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
#[allow(clippy::too_many_arguments)] // Each param is load-bearing context, same as `beamtalk lint`'s pipeline.
fn compile_fixtures_directory(
    fixtures_dir: &Utf8Path,
    output_dir: &Utf8Path,
    hierarchy: &ClassHierarchyContext,
    warnings_as_errors: bool,
    snapshot_only: bool,
    current_package: Option<&str>,
    native_type_registry: Option<&std::sync::Arc<NativeTypeRegistry>>,
    known_erlang_modules: &beamtalk_core::ffi_type_specs::KnownErlangModules,
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
        if let Some(existing_path) = fixtures_by_module.get(&module_name)
            && existing_path != fixture_path
        {
            miette::bail!(
                "Fixture module name collision for '{module_name}': '{}' vs '{}'",
                existing_path,
                fixture_path
            );
        }
        fixtures_by_module.insert(module_name.clone(), fixture_path.clone());

        let core_file = generate_core_file(
            fixture_path,
            &module_name,
            output_dir,
            hierarchy,
            warnings_as_errors,
            current_package,
            native_type_registry.cloned(),
            known_erlang_modules,
        )
        .wrap_err_with(|| format!("Failed to compile fixture '{fixture_path}'"))?;
        core_files.push(core_file);
        module_names.push(module_name);
    }

    // Batch compile all fixture cores in a single BEAM process startup.
    // Skipped in snapshot-only mode (BT-3887): the `.beam` only ever
    // serves the code path at test execution, which never happens there.
    if !core_files.is_empty() && !snapshot_only {
        let compiler = BeamCompiler::new(output_dir.to_owned());
        compiler
            .compile_batch(&core_files)
            .wrap_err("Failed to compile fixture BEAM files")?;
    }

    Ok(module_names)
}

/// Compile a fixture file referenced by `@load` directive.
#[allow(clippy::too_many_arguments)] // Each param is load-bearing context, same as `beamtalk lint`'s pipeline.
fn compile_fixture(
    fixture_path: &Utf8Path,
    output_dir: &Utf8Path,
    hierarchy: &ClassHierarchyContext,
    warnings_as_errors: bool,
    snapshot_only: bool,
    current_package: Option<&str>,
    native_type_registry: Option<&std::sync::Arc<NativeTypeRegistry>>,
    known_erlang_modules: &beamtalk_core::ffi_type_specs::KnownErlangModules,
) -> Result<String> {
    let module_name = fixture_module_name(fixture_path)?;

    let core_file = generate_core_file(
        fixture_path,
        &module_name,
        output_dir,
        hierarchy,
        warnings_as_errors,
        current_package,
        native_type_registry.cloned(),
        known_erlang_modules,
    )
    .wrap_err_with(|| format!("Failed to compile fixture '{fixture_path}'"))?;

    // Snapshot-only mode (BT-3887) wants the `.core` above and nothing more.
    if !snapshot_only {
        let compiler = BeamCompiler::new(output_dir.to_owned());
        compiler
            .compile_batch(&[core_file])
            .wrap_err_with(|| format!("Failed to compile fixture BEAM for '{fixture_path}'"))?;
    }

    Ok(module_name)
}

/// Derive a BEAM module name from a fixture file path (e.g. `counter.bt` → `bt@counter`).
fn fixture_module_name(fixture_path: &Utf8Path) -> Result<String> {
    let stem = fixture_path
        .file_stem()
        .ok_or_else(|| miette::miette!("Fixture file has no name: {}", fixture_path))?;
    Ok(util::bt_module_name_from_stem(stem))
}

/// Build class → module name and class → superclass indexes from fixture files.
///
/// Parses each fixture file to extract class names, superclass relationships,
/// and protocol definitions. Returns:
///
/// 1. A module index mapping each class name to its module name (e.g. `"Env"`
///    → `"bt@scheme_env"` for `fixtures/scheme/scheme_env.bt`).
/// 2. A superclass index mapping each class name to its superclass (e.g.
///    `"ValueSubCircle"` → `"ValueBaseShape"`).
/// 3. A `Vec<ClassInfo>` of full class metadata for validator/type-checker
///    resolution.
/// 4. A `Vec<ProtocolInfo>` of fixture-defined protocols so the
///    unresolved-class validator recognises their names when analysing test
///    modules that reference them.
/// 5. A `Vec<AliasInfo>` of fixture-defined type aliases (ADR 0108, BT-3563)
///    so a `type Name = ...` declared in one fixture file (e.g.
///    `type LocalJsonValue = Nil | Boolean | ...`) resolves through
///    `AliasRegistry` when referenced as a declared field type from a
///    *different* fixture or test file, instead of looking like an opaque,
///    non-nilable class name to `requires_definite_assignment_for_declared_type`.
///    Mirrors `ClassInfo`/`ProtocolInfo` immediately above.
///
/// 6. The full ASTs (and source files) of fixture-defined carried
///    protocols (ADR 0127 §10a; BT-3625) — a `uses:` in one fixture (or test
///    file) flattens a trait declared in another fixture file, exactly as
///    `beamtalk build`'s Pass 1 carries `pre_loaded_protocol_defs`.
///
/// All outputs are merged into the pipeline before fixture compilation so
/// that cross-file references and class hierarchy resolution work correctly —
/// in particular, Value sub-subclasses are recognized as value types rather
/// than defaulting to actor codegen.
#[allow(clippy::type_complexity)]
fn build_fixture_class_indexes(
    fixture_files: &[Utf8PathBuf],
) -> Result<(
    HashMap<String, String>,
    HashMap<String, String>,
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    FixtureProtocolDefs,
    beamtalk_core::compilation::extension_index::ExtensionIndex,
)> {
    let mut module_index = HashMap::new();
    let mut superclass_index = HashMap::new();
    let mut class_infos = Vec::new();
    let mut protocol_infos = Vec::new();
    let mut alias_infos = Vec::new();
    let mut protocol_defs = FixtureProtocolDefs::default();
    // Fixture extension methods (`Target >> selector`), so a test file
    // calling one does not report does-not-understand once the dependency
    // guard lifts (ADR 0070 WS3, BT-4608).
    let mut extensions = beamtalk_core::compilation::extension_index::ExtensionIndex::new();
    let mut parsed_modules = Vec::new();

    for file in fixture_files {
        let module_name = fixture_module_name(file)?;
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        extensions.add_module(&module, file.as_std_path());

        // Extract ProtocolInfo so fixture-defined protocol names are
        // recognised by the unresolved-class validator when compiling test files.
        protocol_infos.extend(
            beamtalk_core::semantic_analysis::protocol_registry::ProtocolRegistry::extract_protocol_infos(
                &module,
            ),
        );

        // Extract AliasInfo (BT-3563) so a type alias declared in this
        // fixture file resolves when referenced from another fixture/test
        // file — mirrors the ClassInfo/ProtocolInfo extraction above.
        alias_infos.extend(
            beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::extract_alias_infos(
                &module,
            ),
        );

        for protocol in module
            .protocols
            .iter()
            .filter(|p| beamtalk_core::semantic_analysis::trait_expansion::is_carried_protocol(p))
        {
            protocol_defs.sources.insert(
                protocol.name.name.clone(),
                beamtalk_core::semantic_analysis::ProtocolSource {
                    path: Some(file.as_str().into()),
                    text: source.as_str().into(),
                    package: None,
                },
            );
            protocol_defs.defs.push(protocol.clone());
        }

        for class in &module.classes {
            let class_name = class.name.name.to_string();
            module_index.insert(class_name.clone(), module_name.clone());
            let superclass_name = class.superclass_name();
            if superclass_name != "none" {
                superclass_index.insert(class_name, superclass_name.to_string());
            }
        }
        parsed_modules.push(module);
    }

    // Extract full ClassInfo for validator/type checker resolution. Done
    // after every fixture has been scanned so a `uses:` whose trait is
    // declared in a *different* fixture is flattened into its class's info,
    // exactly as codegen flattens it (BT-3668) — otherwise a typed call to a
    // provided method on that class reports "does not understand".
    let external_protocols =
        beamtalk_core::semantic_analysis::trait_expansion::first_wins_protocol_map(
            protocol_defs.defs.iter().cloned(),
        );
    for module in &parsed_modules {
        class_infos.extend(
            beamtalk_core::semantic_analysis::trait_expansion::extract_flattened_class_infos(
                module,
                &external_protocols,
                // Fixtures and test files have no package identity.
                None,
            ),
        );
    }

    Ok((
        module_index,
        superclass_index,
        class_infos,
        protocol_infos,
        alias_infos,
        protocol_defs,
        extensions,
    ))
}

/// Provision-bearing protocol ASTs and their source files from a fixtures
/// directory (see [`build_fixture_class_indexes`]).
#[derive(Default)]
struct FixtureProtocolDefs {
    defs: Vec<beamtalk_core::ast::ProtocolDefinition>,
    sources: beamtalk_core::semantic_analysis::ProtocolSourceMap,
}

// ──────────────────────────────────────────────────────────────────────────
// Test file compilation
// ──────────────────────────────────────────────────────────────────────────

/// Compile a `.bt` source file to Core Erlang only (no BEAM compilation).
///
/// Returns the path to the generated `.core` file. Callers should collect
/// multiple core files and batch-compile them with a single `BeamCompiler`
/// call to avoid repeated Erlang VM startup overhead.
#[allow(clippy::too_many_arguments)] // Each param is load-bearing context, same as `beamtalk lint`'s pipeline.
fn generate_core_file(
    source_path: &Utf8Path,
    module_name: &str,
    output_dir: &Utf8Path,
    hierarchy: &ClassHierarchyContext,
    warnings_as_errors: bool,
    current_package: Option<&str>,
    native_type_registry: Option<std::sync::Arc<NativeTypeRegistry>>,
    known_erlang_modules: &beamtalk_core::ffi_type_specs::KnownErlangModules,
) -> Result<Utf8PathBuf> {
    let core_file = output_dir.join(format!("{module_name}.core"));

    // Set the current package so E0401/E0402 visibility checks fire
    // when compiling test/fixture files, matching what `beamtalk build` and
    // `beamtalk lint` enforce — the checks are gated on
    // `current_package: Some(_)` and silently emit zero diagnostics otherwise.
    //
    // BT-4334: declare the same checker scope `build` does. Without
    // `ProjectComplete`, cross-file receivers stay `Open` and their DNU/Type
    // diagnostics are suppressed, so `test --warnings-as-errors` passes code
    // the REPL live recheck (full image hierarchy) rejects.
    // The pre-WS3 dependency guard starts on whenever the package has
    // dependencies; analysis lifts it when `hierarchy` holds every
    // dependency's extensions (ADR 0070 WS3, BT-4608), as for `build`.
    let (knowledge_scope, has_package_dependencies) = test_compile_scope(source_path);
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: false,
        allow_primitives: false,
        warnings_as_errors,
        current_package: current_package.map(str::to_string),
        knowledge_scope,
        dependency_extensions_unknown: has_package_dependencies,
        // BT-4022: project-known Erlang modules so the `unresolved_ffi`
        // check agrees with `build`/`lint` on hex-dep and own-`native/`
        // modules.
        known_erlang_modules: known_erlang_modules.clone(),
        ..Default::default()
    };

    // ADR 0075 FFI typing: without this, `beamtalk test` type-checks every
    // `(Erlang mod) fn:` call as Dynamic regardless of the module's real
    // `-spec`, unlike `beamtalk build`/`beamtalk lint` — see
    // `initialize_pipeline`'s `native_type_registry` computation.
    let ctx = CompileContext {
        hierarchy: hierarchy.clone(),
        native_type_registry,
        ..CompileContext::default()
    };
    compile_source_with_bindings(
        source_path,
        module_name,
        &core_file,
        &options,
        &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
        &ctx,
        None,
    )
    .wrap_err_with(|| format!("Failed to compile '{source_path}'"))?;

    Ok(core_file)
}

/// Resolve the checker scope for compiling a test/fixture file (BT-4334).
///
/// Mirrors what `beamtalk build` declares in `commands::build::build`: a file
/// inside a package is analysed with project-complete knowledge (every
/// project file's classes are present in the hierarchy, so a genuinely
/// unresolved selector is eligible for a DNU/Type diagnostic), while a
/// manifest-less file keeps the conservative `ModuleOnly` default. `build`
/// additionally treats "has dependencies" as unknown-extension surface (the
/// pre-WS3 guard), so that flag is derived from the same manifest read —
/// without it `test` would emit diagnostics `build` suppresses (and vice
/// versa), reopening the CLI/REPL verdict split this helper closes.
///
/// Returns `(knowledge_scope, has_package_dependencies)`, by the shared
/// [`manifest::package_checker_scope`] rule.
fn test_compile_scope(
    source_path: &Utf8Path,
) -> (beamtalk_core::semantic_analysis::KnowledgeScope, bool) {
    let scope = manifest::package_checker_scope(find_package_root(source_path).as_deref());
    (scope.knowledge_scope, scope.has_package_dependencies)
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
    file_super_index.extend(fixture_class_index.keys().filter_map(|class_name| {
        merged_super_index
            .get(class_name)
            .map(|s| (class_name.clone(), s.clone()))
    }));

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

        if path.extension() == Some("beam")
            && let Some(stem) = path.file_stem()
        {
            modules.push(stem.to_string());
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

// ──────────────────────────────────────────────────────────────────────────
// Doc test discovery and compilation
// ──────────────────────────────────────────────────────────────────────────

/// Discover and compile doc tests from a source file.
///
/// Parses the file's AST to find `///` doc comments with `// =>` assertions
/// inside ` ```beamtalk ` code blocks, compiles each expression to Core Erlang,
/// and generates an `EUnit` wrapper module per class.
#[allow(clippy::too_many_arguments)] // Each param is load-bearing context, same as `beamtalk lint`'s pipeline.
fn discover_and_compile_doc_tests(
    source_path: &Utf8Path,
    build_dir: &Utf8Path,
    hierarchy: &ClassHierarchyContext,
    warnings_as_errors: bool,
    snapshot_only: bool,
    current_package: Option<&str>,
    native_type_registry: Option<&std::sync::Arc<NativeTypeRegistry>>,
    known_erlang_modules: &beamtalk_core::ffi_type_specs::KnownErlangModules,
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
        hierarchy,
        warnings_as_errors,
        snapshot_only,
        current_package,
        native_type_registry,
        known_erlang_modules,
    )
    .wrap_err_with(|| format!("Failed to compile source file for doc tests '{source_path}'"))?;

    let mut results = Vec::new();

    for class_doc in doc_tests {
        let all_cases: Vec<&super::doc_tests::DocTestCase> =
            class_doc.groups.iter().flat_map(|g| &g.cases).collect();

        if all_cases.is_empty() {
            continue;
        }

        let class_slug = beamtalk_codegen::core_erlang::to_module_name(&class_doc.class_name);
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

        // Batch compile Core Erlang files to BEAM — not in snapshot-only
        // mode (BT-3887), which stops at the `.core` files written above.
        if !core_files.is_empty() && !snapshot_only {
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
    use super::test_stdlib::{AssertionCase, generate_eunit_wrapper_for};

    let assertion_cases: Vec<AssertionCase<'_>> = cases
        .iter()
        .map(|c| AssertionCase {
            expression: &c.expression,
            expected: &c.expected,
            line: c.source_line,
        })
        .collect();
    generate_eunit_wrapper_for(
        module_name,
        &format!("'{module_name}_test_'"),
        &format!("%% Doc tests from {source_file} ({class_name})"),
        source_file,
        &assertion_cases,
        eval_module_names,
    )
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
#[allow(clippy::struct_excessive_bools)] // Independent orthogonal flags, not a state machine.
struct TestPipeline {
    /// Original test path from CLI.
    test_path: Utf8PathBuf,
    /// Resolved list of `.bt` test files to process.
    test_files: Vec<Utf8PathBuf>,
    /// Temporary build directory for compiled artifacts.
    build_dir: Utf8PathBuf,
    /// BT-3887: stop once every `.core` is written, before any batch
    /// `.beam` compile or test execution (`BEAMTALK_CORE_SNAPSHOT_ONLY=1`;
    /// see `util::CoreOutputDir::snapshot_only`).
    snapshot_only: bool,
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
    /// Every discovered package's `src/` extensions plus its dependencies'
    /// (ADR 0070 WS3, BT-4608), registered into each test/fixture file's
    /// hierarchy as `build` does.
    extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
    /// Whether `extension_index` holds every dependency's extensions; lifts
    /// the pre-WS3 dependency guard (`ClassHierarchyContext::
    /// dependency_extensions_complete`).
    dependency_extensions_complete: bool,
    /// ADR 0075 FFI type registry, merged across every discovered package's
    /// auto-extracted + stub-overridden native types (mirroring `beamtalk
    /// build`), or `extract_stdlib_type_specs()` when no package was
    /// discovered and the test files live under `stdlib/` (mirroring
    /// `beamtalk lint`'s stdlib fallback, BT-3606). Threaded into every
    /// `generate_core_file`/`compile_fixture` call so `(Erlang mod) fn:`
    /// calls type-check the same way under `beamtalk test` as they do under
    /// `build`/`lint`, instead of always inferring `Dynamic`.
    native_type_registry: Option<std::sync::Arc<NativeTypeRegistry>>,
    /// Project-known Erlang modules for the `unresolved_ffi` check (BT-4022),
    /// merged across every discovered package exactly like
    /// `native_type_registry` above. Threaded into every
    /// `generate_core_file`/`compile_fixture` call so an FFI call to a
    /// hex-dep or own-`native/` module does not warn under `beamtalk test`
    /// when it does not warn under `build`/`lint`.
    known_erlang_modules: beamtalk_core::ffi_type_specs::KnownErlangModules,
    /// Cross-file class metadata for type checker hierarchy resolution.
    all_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// Same-package `src/` (and dependency-exported) type-alias declarations
    /// (ADR 0108 `type Name = ...`), pre-loaded into every fixture/test-file
    /// compilation so a test or fixture can reference a stdlib-declared
    /// alias (e.g. `stdlib/src/json.bt`'s `JsonValue`) the same way it can
    /// already reference a stdlib class — mirrors `all_class_infos` above
    /// and `beamtalk build`'s own `pre_loaded_aliases` wiring
    /// (`beam_compiler.rs`).
    all_alias_infos: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    /// Protocols the unresolved-class validator and type checker recognise
    /// in fixtures and test files: each package's `src/` and dependency
    /// protocols (as `beamtalk build` pre-loads them; BT-4446), plus those
    /// declared in `fixtures/*.bt`.
    protocol_infos: Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    /// Full ASTs + source files of fixture-defined carried
    /// protocols, so a fixture/test `uses:` flattens a trait declared in a
    /// different fixture file (ADR 0127 §10a; BT-3625).
    fixture_protocol_defs: FixtureProtocolDefs,
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
    /// BT-4355: render the complete filtered stack trace for each failure
    /// instead of the 3-frame default (`beamtalk test --full-traces`).
    full_traces: bool,
    /// ADR 0072: Hex dependency names that need OTP app startup before tests.
    hex_dep_names: Vec<String>,
    /// Native `EUnit` test module names (modules ending in `_test` or `_tests`).
    native_test_modules: Vec<String>,
}

impl TestPipeline {
    /// Erlang fragment recording the program's root package for the test node
    /// (BT-3651): exactly one package under test is the root; several make
    /// `Program package` raise `ambiguous_program_package`.
    fn root_package_fragment(&self) -> String {
        match self.discovered_packages.as_slice() {
            [] => String::new(),
            [(_, pkg)] => beamtalk_cli::repl_startup::root_package_fragment(Some(&pkg.name)),
            _ => beamtalk_cli::repl_startup::AMBIGUOUS_ROOT_PACKAGE_FRAGMENT.to_string(),
        }
    }

    /// Resolve the owning package name for a file or directory.
    ///
    /// Walks up to the nearest `beamtalk.toml` and maps its canonical root
    /// through `pkg_root_to_name` — the same manifest-derived name `beamtalk
    /// build` uses for `CompilerOptions::current_package`. Returns `None` for
    /// paths outside any discovered package (no package boundary to enforce).
    ///
    /// The path is canonicalized *before* the walk-up: a relative path like
    /// `test/Foo.bt` would otherwise hit `find_package_root`'s empty-parent
    /// guard before ever reaching the package root on disk.
    fn current_package_for(&self, path: &Utf8Path) -> Option<String> {
        let package = canonical_package_root(&canonical_path(path))
            .and_then(|root| self.pkg_root_to_name.get(&root).cloned());
        if package.is_none() {
            debug!(
                path = %path,
                "No package resolved for file; E0401/E0402 visibility checks \
                 are disabled for it"
            );
        }
        package
    }
}

/// Run `BUnit` tests.
///
/// Discovers `TestCase` subclasses in `.bt` files, compiles them, generates
/// `EUnit` wrappers, and runs all tests in a single BEAM process.
///
/// `jobs` controls test class concurrency: `0` = auto (scheduler count),
/// `1` = sequential, `N` = up to N concurrent classes.
#[instrument(skip_all)]
pub fn run_tests(
    path: &str,
    warnings_as_errors: bool,
    jobs: usize,
    quiet: bool,
    full_traces: bool,
) -> Result<()> {
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

    // BT-3509: `.core` files here are a throwaway intermediate to `.beam`
    // unless the corpus `.core` diff harness asks to keep them — see
    // `util::core_output_dir`'s doc.
    let (build_dir, core_output_dir_guard) = util::core_output_dir()?;

    let mut pipeline = initialize_pipeline(
        test_path,
        test_files,
        build_dir,
        core_output_dir_guard.snapshot_only(),
        warnings_as_errors,
        jobs,
    )?;
    pipeline.full_traces = full_traces;
    compile_fixtures(&mut pipeline)?;
    discover_and_compile_tests(&mut pipeline)?;

    // BT-3887: every `.core` (fixtures, test classes, doc tests) is on disk
    // now; the corpus `.core` diff harness's snapshot-only mode stops here
    // rather than building packages and running the suite.
    if pipeline.snapshot_only {
        println!(
            "Snapshotted {} test file(s) to .core only (BEAMTALK_CORE_SNAPSHOT_ONLY=1); \
             skipped package build and test execution",
            pipeline.test_files.len()
        );
        return Ok(());
    }

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
    report_results(&pipeline, &results, start_time, quiet)
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
        if let Some(root) = canonical_package_root(test_file)
            && seen.insert(root.clone())
        {
            roots.push(root);
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
/// Returns `(pkg_class_indexes, class_module_index, class_superclass_index,
/// all_class_infos, all_alias_infos, all_protocol_infos, extension_index,
/// dependency_extensions_complete)`. The protocol infos
/// are every package's `src/` protocols plus its dependencies' exported ones,
/// the same set `beamtalk build` pre-loads (`build_class_index`), so a test
/// file resolves a package protocol — and `P class` (ADR 0144) — as `build`
/// does (BT-4446). The extension index likewise holds every package's `src/`
/// extensions plus its dependencies' (ADR 0070 WS3, BT-4608), and
/// `dependency_extensions_complete` says whether every dependency's were
/// loaded — `false` if any package's dependency resolution failed, or its own
/// sources were not scanned (no `src/`), since the guard may only lift when
/// every extension a test file could call is in the index.
#[allow(clippy::too_many_lines)] // linear merge pipeline (per package: src + deps), as `build_class_index`
fn build_merged_class_indexes(
    discovered_packages: &[(Utf8PathBuf, manifest::PackageManifest)],
) -> MergedClassIndexes {
    let mut pkg_class_indexes: PkgClassIndexes = HashMap::new();
    let mut class_module_index: HashMap<String, String> = HashMap::new();
    let mut class_superclass_index: HashMap<String, String> = HashMap::new();
    // Collect source and dep ClassInfos/AliasInfos separately, then merge
    // via collect_all_class_infos/collect_all_alias_infos for a single
    // unified collection point each.
    let mut source_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> =
        Vec::new();
    let mut dep_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> =
        Vec::new();
    let mut source_alias_infos: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo> =
        Vec::new();
    let mut dep_alias_infos: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo> =
        Vec::new();
    let mut source_protocol_infos: Vec<
        beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo,
    > = Vec::new();
    let mut dep_protocol_infos: Vec<
        beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo,
    > = Vec::new();
    let mut extension_index = beamtalk_core::compilation::extension_index::ExtensionIndex::new();
    let mut dependency_extensions_complete = true;
    for (pkg_root, pkg) in discovered_packages {
        let mut pkg_src = None;
        let src_dir = pkg_root.join("src");
        // The test pipeline only scans a package's `src/`. If it could not
        // (no `src/` — sources at the package root — or the scan failed),
        // the package's own extensions are missing from `extension_index`,
        // and lifting the dependency guard could report a false
        // does-not-understand on one of them; keep the guard on instead.
        let mut own_extensions_loaded = false;
        if let Ok(src_files) = super::build::collect_source_files_from_dir(&src_dir) {
            let has_src_dir = src_dir.is_dir();
            let source_root = src_dir.exists().then_some(src_dir);
            if let Ok((pkg_class_map, pkg_super_map, class_infos, src_extensions, cached_asts)) =
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
                // The package's own `src/` extensions, as `build` registers
                // them for every file (ADR 0066 / ADR 0100 Rule 2 WS1).
                extension_index.merge(&src_extensions);
                own_extensions_loaded = has_src_dir;
                // Flattened below, once this package's dependencies (and so
                // their protocols) are resolved.
                pkg_src = Some((class_infos, cached_asts));
            }
            // Same-package `src/` type-alias declarations (ADR 0108), so a
            // test/fixture file can reference a stdlib-declared `type X =
            // ...` alias — mirrors `build`'s own `collect_project_alias_infos`
            // call in `class_index.rs`'s `build_class_index`.
            source_alias_infos.extend(super::build::collect_project_alias_infos(
                &src_files, &pkg.name,
            ));
            source_protocol_infos.extend(super::build::collect_project_protocol_infos(&src_files));
        }
        dependency_extensions_complete &= own_extensions_loaded;

        // Load dependency class metadata so the type checker and validator
        // can resolve cross-package class references in test files.
        let dep_options = beamtalk_core::CompilerOptions::default();
        let mut dep_protocol_defs = Vec::new();
        match super::deps::ensure_deps_resolved(pkg_root, &dep_options) {
            Ok(resolved_deps) => {
                dependency_extensions_complete &=
                    beamtalk_cli::dependency_extensions::merge_dependency_extensions(
                        resolved_deps.iter().map(|dep| &dep.extensions),
                        &mut extension_index,
                    );
                for dep in &resolved_deps {
                    dep_protocol_defs.extend(dep.protocol_defs.iter().cloned());
                    for (class_name, module_name) in &dep.class_module_index {
                        class_module_index.insert(class_name.clone(), module_name.clone());
                        // BT-4094: each test file compiles against its own
                        // package's index (`class_index_for_file`), not the
                        // merged one — so the dependency entries must land
                        // here too. Without them a direct-called `sealed`
                        // dependency class method misses the codegen
                        // registry and falls back to the `bt@<snake>`
                        // heuristic (`bt@catalina`) instead of
                        // `bt@<package>@<module>`, failing at runtime with
                        // `undef`. Fixtures were unaffected because they
                        // compile against the merged index above.
                        if let Some((pkg_map, _)) = pkg_class_indexes.get_mut(pkg_root) {
                            pkg_map.insert(class_name.clone(), module_name.clone());
                        }
                    }
                    dep_class_infos.extend(dep.class_infos.clone());
                    dep_alias_infos.extend(dep.alias_infos.clone());
                    dep_protocol_infos.extend(dep.protocol_infos.clone());
                }
            }
            Err(e) => {
                dependency_extensions_complete = false;
                warn!(
                    error = %e,
                    "Failed to resolve dependencies for test pipeline; \
                     dependency classes may not be available"
                );
            }
        }

        // `beamtalk test` builds the `src/` index per package, so flatten
        // cross-file trait provisions here as `build` does (BT-3673): a typed
        // call to a provided method on a `src/` class whose trait lives in
        // another file must not report "does not understand".
        if let Some((mut class_infos, cached_asts)) = pkg_src {
            super::build::flatten_package_class_infos(
                &mut class_infos,
                &cached_asts,
                dep_protocol_defs,
                &pkg.name,
            );
            source_class_infos.extend(class_infos);
        }
    }

    // Single unified collection of all ClassInfo/AliasInfo from all sources.
    // Fixture ClassInfo is added later in compile_fixtures() via
    // pipeline.all_class_infos.extend(fixture_class_infos).
    // To add a new .bt source location, add its ClassInfo/AliasInfo slice here.
    let all_class_infos =
        super::build::collect_all_class_infos(&[&source_class_infos, &dep_class_infos]);
    let all_alias_infos =
        super::build::collect_all_alias_infos(&[&source_alias_infos, &dep_alias_infos]);
    let all_protocol_infos =
        super::build::collect_all_protocol_infos(&[&source_protocol_infos, &dep_protocol_infos]);

    (
        pkg_class_indexes,
        class_module_index,
        class_superclass_index,
        all_class_infos,
        all_alias_infos,
        all_protocol_infos,
        extension_index,
        dependency_extensions_complete,
    )
}

/// Compute the ADR 0075 FFI type registry for this test run.
///
/// Mirrors `beamtalk build`'s per-package resolution chain (auto-extract →
/// dependency stubs → project-local stubs, `crate::commands::build::stubs`)
/// for every discovered package, merged together via `apply_overrides` —
/// and `beamtalk lint`'s stdlib fallback (`extract_stdlib_type_specs`,
/// BT-3606) when no package was discovered at all and the test files live
/// under a `stdlib/` tree (which has no `beamtalk.toml` to discover).
///
/// Without this, `(Erlang mod) fn:` calls in `.bt` test files type-check as
/// `Dynamic` under `beamtalk test` regardless of the module's real `-spec`,
/// unlike `beamtalk build`/`beamtalk lint` — see `generate_core_file`'s
/// `native_type_registry` param.
///
/// Returns `None` when neither case applies (e.g. a bare directory of
/// `.bt` files with no `beamtalk.toml` and no `stdlib/` ancestry), matching
/// this function's pre-existing (implicit) behaviour.
fn compute_native_type_registry(
    discovered_packages: &[(Utf8PathBuf, manifest::PackageManifest)],
    test_files: &[Utf8PathBuf],
) -> Option<std::sync::Arc<NativeTypeRegistry>> {
    if discovered_packages.is_empty() {
        let is_stdlib_target = test_files
            .iter()
            .any(|f| beamtalk_project::package::is_under_stdlib_dir(f.as_std_path()));
        return if is_stdlib_target {
            super::build_stdlib::extract_stdlib_type_specs().map(std::sync::Arc::new)
        } else {
            None
        };
    }

    let mut merged = NativeTypeRegistry::default();
    let mut merged_any = false;
    for (pkg_root, _pkg) in discovered_packages {
        let layout = BuildLayout::new(pkg_root.clone());

        // Resolve (fetch/compile) dependencies *before* auto-extracting FFI
        // specs — `extract_type_specs` scans dependency `.beam` files on
        // disk, which don't exist yet on a cold `_build/deps/` until
        // `ensure_deps_resolved` compiles them. `beamtalk build`
        // (`execute_build_passes`, `build/mod.rs`) resolves dependencies
        // before its own `extract_type_specs` call for the same reason;
        // getting this backwards would silently fall back to `Dynamic` for
        // a dependency's native FFI calls on a fresh checkout/CI runner —
        // exactly the `build`/`test` drift this function exists to close.
        let dep_options = beamtalk_core::CompilerOptions::default();
        let resolved_deps = match super::deps::ensure_deps_resolved(pkg_root, &dep_options) {
            Ok(deps) => deps,
            Err(e) => {
                warn!(
                    error = %e,
                    "Failed to resolve dependencies for FFI type registry; \
                     dependency native modules' FFI calls may fall back to Dynamic"
                );
                Vec::new()
            }
        };
        let auto_extract = super::build::extract_type_specs(&layout, true, false);

        let pkg_registry = super::build::resolve_package_type_registry(
            pkg_root,
            auto_extract,
            &resolved_deps,
            Some(super::OutputFormat::Text),
        )
        .registry;

        if let Some(reg) = pkg_registry {
            merged.apply_overrides(reg);
            merged_any = true;
        }
    }

    merged_any.then(|| std::sync::Arc::new(merged))
}

/// Initialize the test pipeline: discover packages and build class indexes.
fn initialize_pipeline(
    test_path: Utf8PathBuf,
    test_files: Vec<Utf8PathBuf>,
    build_dir: Utf8PathBuf,
    snapshot_only: bool,
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

    let (
        pkg_class_indexes,
        class_module_index,
        class_superclass_index,
        all_class_infos,
        mut all_alias_infos,
        package_protocol_infos,
        extension_index,
        dependency_extensions_complete,
    ) = build_merged_class_indexes(&discovered_packages);

    // See `collect_sibling_src_alias_infos`'s doc: only engages when
    // manifest-based discovery found nothing at all, so a real manifest'd
    // project's own `src/` (already covered by `build_merged_class_indexes`
    // above) is never double-scanned.
    if discovered_packages.is_empty() {
        let extra_aliases = super::build::collect_sibling_src_alias_infos(&test_path);
        all_alias_infos =
            super::build::collect_all_alias_infos(&[&all_alias_infos, &extra_aliases]);
    }

    let native_type_registry = compute_native_type_registry(&discovered_packages, &test_files);

    // BT-4022: project-known Erlang modules for the `unresolved_ffi` check,
    // merged across every discovered package (same merge shape as
    // `compute_native_type_registry` above). Unlike `beamtalk build`, `test`
    // does not compile the package's own native/hex deps first, so the ebin
    // tier reflects whatever a previous build left on disk — the `native/`
    // source tier is known regardless.
    // Seeded with the installed set so loose test files (no discovered
    // package) still get the check.
    let mut known_erlang_modules =
        beamtalk_core::ffi_type_specs::collect_installed_erlang_modules();
    for (pkg_root, _pkg) in &discovered_packages {
        known_erlang_modules.extend_modules(
            beamtalk_core::ffi_type_specs::collect_project_known_erlang_modules(pkg_root),
        );
    }

    Ok(TestPipeline {
        test_path,
        test_files,
        build_dir,
        snapshot_only,
        native_type_registry,
        known_erlang_modules,
        warnings_as_errors,
        discovered_packages,
        pkg_root_to_name,
        pkg_class_indexes,
        class_module_index,
        class_superclass_index,
        all_class_infos,
        all_alias_infos,
        protocol_infos: package_protocol_infos,
        extension_index,
        dependency_extensions_complete,
        fixture_protocol_defs: FixtureProtocolDefs::default(),
        fixture_class_index: HashMap::new(),
        precompiled_modules: HashSet::new(),
        all_fixture_modules: Vec::new(),
        compiled_tests: Vec::new(),
        compiled_doc_tests: Vec::new(),
        package_modules: Vec::new(),
        package_ebin_dirs: Vec::new(),
        jobs,
        full_traces: false,
        hex_dep_names: Vec::new(),
        native_test_modules: Vec::new(),
    })
}

/// Resolve the fixtures directory for a test run (BT-4723).
///
/// A directory run containing `fixtures/` (e.g. `test/`) or a file run next to
/// one uses it directly. Otherwise a scoped run (`test/http1/foo_test.bt`,
/// `test/http1`) falls back to the owning package's `test/fixtures`, so a
/// single-file run sees the same fixtures as a full run.
fn resolve_fixtures_dir(test_path: &Utf8Path) -> Utf8PathBuf {
    let local = if test_path.is_dir() {
        test_path.join("fixtures")
    } else {
        test_path
            .parent()
            .map(|p| p.join("fixtures"))
            .unwrap_or_default()
    };
    if local.is_dir() {
        return local;
    }
    // Only a target under the package's `test/` (and not already inside
    // `test/fixtures`) is a scoped run that should see the package fixtures.
    if let Some(root) = canonical_package_root(test_path) {
        let target = canonical_path(test_path);
        let pkg_fixtures = root.join("test").join("fixtures");
        if target.starts_with(root.join("test"))
            && !target.starts_with(&pkg_fixtures)
            && pkg_fixtures.is_dir()
        {
            return pkg_fixtures;
        }
    }
    local
}

/// Phase 0: Pre-compile all fixtures in the `fixtures/` subdirectory.
///
/// Makes all fixture classes available during test execution -- like
/// a Smalltalk image where all classes exist in the running environment.
fn compile_fixtures(pipeline: &mut TestPipeline) -> Result<()> {
    let fixtures_dir = resolve_fixtures_dir(&pipeline.test_path);

    // Build the fixture class index separately so it can be merged per-file in
    // Phase 1. Also merge into the combined index for Phase 0 fixture compilation,
    // where cross-package and cross-fixture references are allowed.
    // Also build a fixture superclass index so the class hierarchy
    // resolves correctly for Value sub-subclasses defined across fixture files.
    let (
        fixture_class_index,
        fixture_superclass_index,
        fixture_class_infos,
        fixture_protocol_infos,
        fixture_alias_infos,
        fixture_protocol_defs,
        fixture_extensions,
    ) = if fixtures_dir.is_dir() {
        let fixture_files = find_test_files(&fixtures_dir)?;
        build_fixture_class_indexes(&fixture_files)?
    } else {
        (
            HashMap::new(),
            HashMap::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            FixtureProtocolDefs::default(),
            beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
        )
    };
    pipeline.fixture_protocol_defs = fixture_protocol_defs;
    pipeline.extension_index.merge(&fixture_extensions);
    pipeline
        .class_module_index
        .extend(fixture_class_index.clone());
    pipeline
        .class_superclass_index
        .extend(fixture_superclass_index);
    // Fixture ClassInfo is included in all_class_infos so the type
    // checker can validate cross-file references in test files. The Shape class
    // name collision between abstract_shape.bt and shape.bt was
    // resolved by renaming abstract_shape's class to AbstractShape.
    pipeline.all_class_infos.extend(fixture_class_infos);
    // Fixture-defined protocols need to flow through to each test
    // file's compilation so their names resolve in the unresolved-class validator.
    pipeline.protocol_infos.extend(fixture_protocol_infos);
    // Fixture-defined type aliases (BT-3563) need to flow through the same
    // way, so a `type Name = ...` declared in one fixture file resolves
    // when referenced as a declared field type from another fixture/test
    // file — mirrors `protocol_infos` immediately above.
    pipeline.all_alias_infos.extend(fixture_alias_infos);

    let fixture_hierarchy = ClassHierarchyContext {
        class_module_index: pipeline.class_module_index.clone(),
        class_superclass_index: pipeline.class_superclass_index.clone(),
        pre_loaded_classes: pipeline.all_class_infos.clone(),
        pre_loaded_protocols: pipeline.protocol_infos.clone(),
        pre_loaded_protocol_defs: pipeline.fixture_protocol_defs.defs.clone(),
        pre_loaded_protocol_sources: pipeline.fixture_protocol_defs.sources.clone(),
        pre_loaded_aliases: pipeline.all_alias_infos.clone(),
        extension_index: pipeline.extension_index.clone(),
        dependency_extensions_complete: pipeline.dependency_extensions_complete,
    };
    let fixtures_package = pipeline.current_package_for(&fixtures_dir);
    let precompiled = compile_fixtures_directory(
        &fixtures_dir,
        &pipeline.build_dir,
        &fixture_hierarchy,
        pipeline.warnings_as_errors,
        pipeline.snapshot_only,
        fixtures_package.as_deref(),
        pipeline.native_type_registry.as_ref(),
        &pipeline.known_erlang_modules,
    )?;
    for module_name in &precompiled {
        pipeline.precompiled_modules.insert(module_name.clone());
    }
    pipeline.all_fixture_modules.extend(precompiled);
    pipeline.fixture_class_index = fixture_class_index;

    Ok(())
}

/// The extension methods of the test files `beamtalk test` will compile and
/// load: those with at least one test class, found by the same discovery
/// `compile_single_test_file` uses (`test_class_in_module`, minus its
/// warnings, which the compile pass reports). A helper-only file is never compiled or
/// loaded, so its extensions must not hide a does-not-understand that would
/// fail at runtime. A file that cannot be read or parsed fails its own
/// compilation later.
fn collect_test_file_extensions(
    test_files: &[Utf8PathBuf],
    pkg_class_indexes: &PkgClassIndexes,
    fixture_class_index: &HashMap<String, String>,
    class_module_index: &HashMap<String, String>,
    class_superclass_index: &HashMap<String, String>,
) -> beamtalk_core::compilation::extension_index::ExtensionIndex {
    let mut extensions = beamtalk_core::compilation::extension_index::ExtensionIndex::new();
    for test_file in test_files {
        let (_, file_super_index) = class_index_for_file(
            test_file,
            pkg_class_indexes,
            fixture_class_index,
            class_module_index,
            class_superclass_index,
        );
        let Ok(source) = fs::read_to_string(test_file) else {
            continue;
        };
        let (module, _) = beamtalk_core::source_analysis::parse(
            beamtalk_core::source_analysis::lex_with_eof(&source),
        );
        if test_class_in_module(&module, &file_super_index).is_some() {
            extensions.add_module(&module, test_file.as_std_path());
        }
    }
    extensions
}

/// Whether a test file's analysis may lift the dependency guard (ADR 0070
/// WS3, BT-4608): every dependency's extensions were loaded, and the file
/// has no deprecated `// @load` directives — those pull in arbitrary files
/// whose extensions are not in the index.
fn test_file_dependency_extensions_complete(
    pipeline_complete: bool,
    load_files: &[String],
) -> bool {
    pipeline_complete && load_files.is_empty()
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

    let fixtures_dir = resolve_fixtures_dir(&pipeline.test_path);

    // Clone test_files to avoid borrow conflict with pipeline fields.
    let test_files = pipeline.test_files.clone();

    // The extensions of every test file that will be compiled and loaded,
    // so a test calling a helper extension declared in a sibling test file
    // is not reported once the dependency guard lifts (ADR 0070 WS3,
    // BT-4608). Collected after the fixtures are merged, so a test class
    // inheriting from a fixture class is recognised.
    let test_file_extensions = collect_test_file_extensions(
        &test_files,
        &pipeline.pkg_class_indexes,
        &pipeline.fixture_class_index,
        &pipeline.class_module_index,
        &pipeline.class_superclass_index,
    );
    pipeline.extension_index.merge(&test_file_extensions);

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
    // Skipped in snapshot-only mode (BT-3887): no `.beam` is ever read from
    // it, and `run_tests` returns right after this function anyway.
    if !pending_test_cores.is_empty() && !pipeline.snapshot_only {
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

/// Reject a `TestCase` class whose BEAM module name is already claimed by an
/// earlier test class in this run.
///
/// Module names derive from the class name (plus a package prefix only when
/// several packages are in scope), so two files in one package declaring the
/// same `TestCase` subclass compile to the same `<module>.core`/`.beam`. The
/// later file would silently clobber the earlier one and its tests would never
/// run, with counts depending on discovery order (BT-4350).
fn check_duplicate_test_module(
    compiled_tests: &[CompiledTest],
    test_class: &TestCaseClass,
    test_file: &Utf8Path,
) -> Result<()> {
    if let Some(existing) = compiled_tests
        .iter()
        .find(|c| c.test_class.module_name == test_class.module_name)
    {
        miette::bail!(
            "Duplicate test module '{}': test class '{}' in '{}' and test class '{}' in '{}' \
             compile to the same module. Rename one of the test classes.",
            test_class.module_name,
            existing.test_class.class_name,
            existing.source_file,
            test_class.class_name,
            test_file
        );
    }
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

    let (mut test_classes, load_files) =
        discover_test_classes_in_hierarchy(test_file, &file_super_index)?;

    // When multiple packages are in scope, prefix the test module name with
    // the owning package name to prevent `bt@smoke_test.beam` collisions
    // between two packages that each define a class named `SmokeTest`.
    // Single-package runs are unaffected (no prefix added when only one package
    // is discovered), preserving backwards-compatible module names.
    if pipeline.discovered_packages.len() > 1 {
        let pkg_name_slug = canonical_package_root(test_file)
            .and_then(|r| pipeline.pkg_root_to_name.get(&r))
            .map(|n| beamtalk_codegen::core_erlang::to_module_name(n));

        if let Some(ref prefix) = pkg_name_slug {
            for tc in &mut test_classes {
                let stem = beamtalk_codegen::core_erlang::to_module_name(&tc.class_name);
                tc.module_name = format!("bt@{prefix}@{stem}");
            }
        }
    }

    // Build hierarchy once for all compilations within this test file.
    let file_hierarchy = ClassHierarchyContext {
        class_module_index: file_class_index.clone(),
        class_superclass_index: file_super_index.clone(),
        pre_loaded_classes: pipeline.all_class_infos.clone(),
        pre_loaded_protocols: pipeline.protocol_infos.clone(),
        pre_loaded_protocol_defs: pipeline.fixture_protocol_defs.defs.clone(),
        pre_loaded_protocol_sources: pipeline.fixture_protocol_defs.sources.clone(),
        pre_loaded_aliases: pipeline.all_alias_infos.clone(),
        extension_index: pipeline.extension_index.clone(),
        dependency_extensions_complete: test_file_dependency_extensions_complete(
            pipeline.dependency_extensions_complete,
            &load_files,
        ),
    };

    // Handle deprecated @load directives as fallback.
    // Fixtures in the fixtures/ directory are already compiled above.
    resolve_load_directives(
        pipeline,
        test_file,
        &load_files,
        fixtures_dir,
        fixture_modules_by_name,
        &file_hierarchy,
    )?;

    // Compile TestCase subclasses
    let test_file_package = pipeline.current_package_for(test_file);
    for test_class in test_classes {
        // Fail fast before writing `<module>.core`: a second file with the
        // same module name would overwrite the first's output (BT-4350).
        check_duplicate_test_module(&pipeline.compiled_tests, &test_class, test_file)?;

        // Generate Core Erlang for the test file (BEAM compilation is batched below)
        let core_file = generate_core_file(
            test_file,
            &test_class.module_name,
            &pipeline.build_dir,
            &file_hierarchy,
            pipeline.warnings_as_errors,
            test_file_package.as_deref(),
            pipeline.native_type_registry.clone(),
            &pipeline.known_erlang_modules,
        )
        .wrap_err_with(|| format!("Failed to compile test file '{test_file}'"))?;
        pending_test_cores.push(core_file);

        // EUnit wrappers are not generated; beamtalk_test_runner:run_all/1
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
        &file_hierarchy,
        pipeline.warnings_as_errors,
        pipeline.snapshot_only,
        test_file_package.as_deref(),
        pipeline.native_type_registry.as_ref(),
        &pipeline.known_erlang_modules,
    )?;
    for dr in doc_results {
        // Doc test EUnit wrappers are still generated but not compiled here.
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
    hierarchy: &ClassHierarchyContext,
) -> Result<()> {
    for load_path in load_files {
        let fixture_path = Utf8PathBuf::from(load_path);

        let fixture_module = fixture_module_name(&fixture_path)?;

        // Skip if already pre-compiled from the fixtures directory
        if pipeline.precompiled_modules.contains(&fixture_module) {
            if canonical_path(&fixture_path).starts_with(canonical_path(fixtures_dir)) {
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
        let fixture_package = pipeline.current_package_for(&fixture_path);
        let module_name = compile_fixture(
            &fixture_path,
            &pipeline.build_dir,
            hierarchy,
            pipeline.warnings_as_errors,
            pipeline.snapshot_only,
            fixture_package.as_deref(),
            pipeline.native_type_registry.as_ref(),
            &pipeline.known_erlang_modules,
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

        // Use BeamEnvironment for unified code path and OTP app collection.
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
                if path.is_file()
                    && path.extension().is_some_and(|ext| ext == "erl")
                    && let Some(stem) = path.file_stem().and_then(|s| s.to_str())
                    && (stem.ends_with("_test") || stem.ends_with("_tests"))
                {
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
        .docs()
        .erl_libs(&test_layout.rebar_lib_dir())
        .code_paths(ebin_dirs)
        .runtime_include()
        .include_dir(pkg_root.join("native").join("include"))
        .source_files(&erl_files)
        .run("Native test Erlang compilation")?;

    Ok(())
}

/// Format one failing-test detail line.
///
/// See [`summary_line`] for the format's history and the tests that pin it.
fn fail_detail_line(display_name: &str, error: &str) -> String {
    format!("FAIL {display_name}: {error}")
}

/// Format the final one-line run summary.
///
/// `.github/workflows/cross-repo.yml` no longer parses the failure count
/// out of this line — that once let it tell a known `beamtalk-http` network
/// flake from a real regression, but the flaky test is now hermetic by
/// default, and nothing else parses this output. `summary_line_format` and
/// `fail_detail_line_format` keep pinning both formats anyway, since they're
/// still user-facing display text worth a regression test.
fn summary_line(
    file_count: usize,
    total_tests: usize,
    total_passed: usize,
    total_failed: usize,
    elapsed_secs: f64,
) -> String {
    format!(
        "{file_count} file(s), {total_tests} tests, {total_passed} passed, {total_failed} failed ({elapsed_secs:.1}s)"
    )
}

/// Phase 4: Aggregate and report test results.
///
/// Displays results from `beamtalk_test_runner:run_all/1` with
/// Beamtalk class names (not Erlang module names) and per-class summary.
/// Also reports native `EUnit` test results when present.
fn report_results(
    pipeline: &TestPipeline,
    results: &TestResults,
    start_time: Instant,
    quiet: bool,
) -> Result<()> {
    let mut failed_details = Vec::new();
    let mut total_tests: usize = 0;
    let mut total_passed: usize = 0;
    let mut total_failed: usize = 0;

    // Report BUnit results
    if let Some(result) = &results.bunit {
        // Group test results by class name from the JSON "class" field.
        let mut class_results: HashMap<String, (usize, usize, usize)> = HashMap::new();

        for test in &result.tests {
            let class_name = test.class.clone().unwrap_or_else(|| test.name.clone());

            let (passed, failed, skipped) = class_results.entry(class_name).or_insert((0, 0, 0));

            match test.status.as_str() {
                "pass" => *passed += 1,
                "fail" => {
                    *failed += 1;
                    let display_name = test
                        .class
                        .as_ref()
                        .map_or(test.name.clone(), |c| format!("{c} {}", test.name));
                    if let Some(error) = &test.error {
                        failed_details.push(fail_detail_line(&display_name, error));
                    }
                }
                "skip" => *skipped += 1,
                _ => {}
            }
        }

        // Print results per class in deterministic compile order (not JSON result
        // order, which varies when jobs > 1 due to concurrent class execution).
        for compiled in &pipeline.compiled_tests {
            let class_name = &compiled.test_class.class_name;
            if let Some(&(passed, failed, skipped)) = class_results.get(class_name) {
                let total = passed + failed + skipped;
                if failed > 0 {
                    println!(
                        "  {class_name}: {total} tests, {passed} passed, {failed} failed \u{2717}"
                    );
                } else if skipped > 0 {
                    println!(
                        "  {class_name}: {total} tests, {passed} passed, {skipped} skipped \u{2713}"
                    );
                } else if !quiet {
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
            } else if !quiet {
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

    let summary = summary_line(
        file_count,
        total_tests,
        total_passed,
        total_failed,
        elapsed_secs,
    );

    println!();
    if total_failed == 0 {
        println!("{summary}");
    } else {
        for detail in &failed_details {
            eprintln!("{detail}");
        }
        eprintln!();
        eprintln!("{summary}");
        miette::bail!("{total_failed} test(s) failed");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Only test files that will be compiled and loaded (they define a test
    /// class) contribute extensions; a helper-only file does not, so its
    /// extension cannot hide a does-not-understand that fails at runtime.
    #[test]
    fn test_file_extensions_come_only_from_compiled_test_files() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let with_test = dir.join("shape_test.bt");
        fs::write(
            &with_test,
            "TestCase subclass: ShapeTest\n  testShape => self assertShape: 1\n\n\
             Object >> assertShape: x => x\n",
        )
        .unwrap();
        let helper_only = dir.join("helpers.bt");
        fs::write(&helper_only, "Object >> unloadedHelper => 1\n").unwrap();

        let extensions = collect_test_file_extensions(
            &[with_test, helper_only],
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
            &HashMap::new(),
        );
        let selectors: HashSet<String> = extensions
            .entries()
            .keys()
            .map(|key| key.selector.to_string())
            .collect();
        assert!(selectors.contains("assertShape:"), "{selectors:?}");
        assert!(!selectors.contains("unloadedHelper"), "{selectors:?}");
    }

    /// A test file with a deprecated `// @load` keeps the dependency guard
    /// on: the loaded file's extensions are not in the index.
    #[test]
    fn load_directive_keeps_the_dependency_guard_on() {
        assert!(test_file_dependency_extensions_complete(true, &[]));
        assert!(!test_file_dependency_extensions_complete(
            true,
            &["examples/helper.bt".to_string()]
        ));
        assert!(!test_file_dependency_extensions_complete(false, &[]));
    }

    /// Fixture extension methods reach test files' hierarchies too, so a
    /// test calling one is not reported once the dependency guard lifts.
    #[test]
    fn fixture_class_indexes_collect_fixture_extensions() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::write(dir.join("helpers.bt"), "String >> slug -> String => self\n").unwrap();

        let (_, _, _, _, _, _, extensions) =
            build_fixture_class_indexes(&[dir.join("helpers.bt")]).unwrap();
        assert!(
            extensions
                .entries()
                .keys()
                .any(|key| key.selector == "slug"),
            "{extensions:?}"
        );
    }

    /// The test pipeline scans only a package's `src/`; a package without
    /// one keeps the dependency guard on, since its own extensions are not
    /// in the index.
    #[test]
    fn package_without_src_keeps_the_dependency_guard_on() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let consumer = crate::commands::deps::test_support::consumer_with_fresh_path_dependency(
            &root,
            &[("ext.bt", "Object >> fromDependency -> Integer => 1\n")],
            "",
            &[],
        );
        let manifest = manifest::parse_manifest_full(&consumer.join("beamtalk.toml"))
            .unwrap()
            .package;

        let (_, _, _, _, _, _, _, complete) = build_merged_class_indexes(&[(consumer, manifest)]);
        assert!(!complete);
    }

    /// ADR 0070 WS3 (BT-4608): a test file's hierarchy registers the
    /// package's own `src/` extensions and its dependencies', as `build`
    /// does, and says whether every dependency's were loaded.
    #[test]
    fn merged_class_indexes_carry_package_and_dependency_extensions() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let consumer = crate::commands::deps::test_support::consumer_with_fresh_path_dependency(
            &root,
            &[("ext.bt", "Object >> fromDependency -> Integer => 1\n")],
            "",
            &[("own.bt", "Object >> fromPackage -> Integer => 2\n")],
        );
        let manifest = manifest::parse_manifest_full(&consumer.join("beamtalk.toml"))
            .unwrap()
            .package;

        let (_, _, _, _, _, _, extensions, complete) =
            build_merged_class_indexes(&[(consumer, manifest)]);

        let selectors: HashSet<String> = extensions
            .entries()
            .keys()
            .map(|key| key.selector.to_string())
            .collect();
        assert!(selectors.contains("fromPackage"), "{selectors:?}");
        assert!(selectors.contains("fromDependency"), "{selectors:?}");
        assert!(complete);
    }

    /// Pins the summary line's display format. See [`summary_line`] for the
    /// format's history and why it's still worth pinning.
    #[test]
    fn summary_line_format() {
        let line = summary_line(8, 168, 167, 1, 39.84);
        assert_eq!(line, "8 file(s), 168 tests, 167 passed, 1 failed (39.8s)");

        let clean = summary_line(8, 168, 168, 0, 1.0);
        assert_eq!(clean, "8 file(s), 168 tests, 168 passed, 0 failed (1.0s)");
    }

    /// Pins the failing-test detail line's display format, including the
    /// leading `FAIL ` marker.
    #[test]
    fn fail_detail_line_format() {
        let line = fail_detail_line("HTTPTest testFoo", "assertion failed");
        assert_eq!(line, "FAIL HTTPTest testFoo: assertion failed");
    }

    fn write_bt_file(name: &str, content: &str) -> (tempfile::TempDir, Utf8PathBuf) {
        let temp = tempfile::TempDir::new().unwrap();
        let file = Utf8PathBuf::from_path_buf(temp.path().join(name)).unwrap();
        fs::write(&file, content).unwrap();
        (temp, file)
    }

    fn temp_utf8_dir() -> (tempfile::TempDir, Utf8PathBuf) {
        let temp = tempfile::TempDir::new().unwrap();
        let canonical =
            std::fs::canonicalize(temp.path()).unwrap_or_else(|_| temp.path().to_path_buf());
        let dir = Utf8PathBuf::from_path_buf(canonical).unwrap();
        (temp, dir)
    }

    #[test]
    fn test_discover_no_test_classes() {
        let (_temp, file) = write_bt_file("plain.bt", "Object subclass: Plain\n  foo => 42\n");

        let (classes, loads) = discover_test_classes(&file).unwrap();
        assert!(classes.is_empty());
        assert!(loads.is_empty());
    }

    #[test]
    fn test_discover_rejects_file_with_parse_error() {
        // BT-4304: a syntax error must surface, not read as "no tests".
        let (_temp, file) = write_bt_file(
            "broken_test.bt",
            "TestCase subclass: BrokenTest\n  testIt => self assert: (1 + ) equals: 3\n",
        );

        let err = discover_test_classes(&file).unwrap_err();
        let msg = format!("{err}");
        assert!(msg.contains("Failed to parse test file"), "{msg}");
        assert!(msg.contains("broken_test.bt"), "{msg}");
    }

    #[test]
    fn test_discover_test_case_subclass() {
        let (_temp, file) = write_bt_file(
            "my_test.bt",
            "TestCase subclass: MyTest\n  testAdd => self assert: (1 + 2) equals: 3\n  helper => nil\n",
        );

        let (classes, _) = discover_test_classes(&file).unwrap();
        assert_eq!(classes.len(), 1);
        assert_eq!(classes[0].class_name, "MyTest");
        assert_eq!(classes[0].test_methods, vec!["testAdd"]);
    }

    #[test]
    fn test_discover_indirect_subclass_through_superclass_index() {
        // BT-3768: the abstract test case lives in a fixture; the concrete
        // subclass has no test method of its own and inherits them.
        let (_temp, file) = write_bt_file(
            "concrete_test.bt",
            "AbstractMatrixTest subclass: ConcreteTest\n  subject => 42\n",
        );
        let index: HashMap<String, String> =
            [("AbstractMatrixTest".to_string(), "TestCase".to_string())]
                .into_iter()
                .collect();

        let (classes, _) = discover_test_classes_in_hierarchy(&file, &index).unwrap();
        assert_eq!(classes.len(), 1);
        assert_eq!(classes[0].class_name, "ConcreteTest");
        assert!(classes[0].test_methods.is_empty());

        // Without the index the superclass is unknown, so nothing is found.
        let (classes, _) = discover_test_classes(&file).unwrap();
        assert!(classes.is_empty());
    }

    #[test]
    fn test_inherits_from_test_case_stops_on_a_cycle() {
        let index: HashMap<String, String> = [
            ("A".to_string(), "B".to_string()),
            ("B".to_string(), "A".to_string()),
        ]
        .into_iter()
        .collect();
        assert!(!inherits_from_test_case("A", &index));
        assert!(inherits_from_test_case("TestCase", &index));
    }

    #[test]
    fn test_discover_setup_teardown() {
        let (_temp, file) = write_bt_file(
            "lifecycle.bt",
            "TestCase subclass: LifecycleTest\n  setUp => nil\n  tearDown => nil\n  testIt => self assert: true\n",
        );

        let (classes, _) = discover_test_classes(&file).unwrap();
        assert_eq!(classes.len(), 1);
        // No setUp/tearDown detection — handled by the BUnit runtime.
        // Only test method discovery matters for the CLI now.
        assert_eq!(classes[0].test_methods, vec!["testIt"]);
    }

    #[test]
    fn test_discover_load_directives() {
        let (_temp, file) = write_bt_file(
            "with_load.bt",
            "// @load stdlib/test/fixtures/counter.bt\nTestCase subclass: T\n  testX => nil\n",
        );

        let (_, loads) = discover_test_classes(&file).unwrap();
        assert_eq!(loads, vec!["stdlib/test/fixtures/counter.bt"]);
    }

    // test_generate_eunit_wrapper_simple no longer applies — EUnit wrappers are no longer generated.
    // Test discovery and execution now use beamtalk_test_runner:run_all/1 directly.

    #[test]
    fn test_collect_beam_module_names() {
        let (_temp, dir) = temp_utf8_dir();

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
        let (_temp, dir) = temp_utf8_dir();

        let modules = collect_beam_module_names(&dir).unwrap();
        assert!(modules.is_empty());
    }

    #[test]
    fn test_build_fixture_class_module_index_flat() {
        let (_temp, dir) = temp_utf8_dir();

        // Fixture also declares a protocol so we can assert the
        // returned `protocol_infos` carries it through.
        fs::write(
            dir.join("counter.bt"),
            "Protocol define: Tickable\n  tick -> Integer\n\n\
             Object subclass: Counter\n  count => 0\n",
        )
        .unwrap();

        let files = vec![dir.join("counter.bt")];
        let (module_index, superclass_index, _class_infos, protocol_infos, _alias_infos, _defs, _) =
            build_fixture_class_indexes(&files).unwrap();
        assert_eq!(
            module_index.get("Counter").map(String::as_str),
            Some("bt@counter")
        );
        assert_eq!(
            superclass_index.get("Counter").map(String::as_str),
            Some("Object")
        );
        assert!(
            protocol_infos.iter().any(|p| p.name == "Tickable"),
            "expected fixture-defined protocol `Tickable` to be extracted"
        );
    }

    #[test]
    fn test_build_fixture_class_module_index_subdirectory() {
        let temp = tempfile::TempDir::new().unwrap();
        let subdir = Utf8PathBuf::from_path_buf(temp.path().join("scheme")).unwrap();
        fs::create_dir_all(&subdir).unwrap();

        fs::write(
            subdir.join("scheme_env.bt"),
            "Object subclass: SchemeEnv\n  lookup => nil\n",
        )
        .unwrap();

        // fixture_module_name uses the file stem only, so scheme_env.bt → bt@scheme_env
        let files = vec![subdir.join("scheme_env.bt")];
        let (module_index, superclass_index, _class_infos, protocol_infos, _alias_infos, _defs, _) =
            build_fixture_class_indexes(&files).unwrap();
        assert_eq!(
            module_index.get("SchemeEnv").map(String::as_str),
            Some("bt@scheme_env")
        );
        assert_eq!(
            superclass_index.get("SchemeEnv").map(String::as_str),
            Some("Object")
        );
        assert!(
            protocol_infos.is_empty(),
            "fixture has no protocols — extracted list should be empty"
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
        let (_temp, dir) = temp_utf8_dir();

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
        let (_temp, dir) = temp_utf8_dir();

        // No beamtalk.toml anywhere
        fs::create_dir_all(dir.join("sub")).unwrap();
        fs::write(dir.join("sub/test.bt"), "").unwrap();

        let result = find_package_root(&dir.join("sub/test.bt"));
        assert!(result.is_none());
    }

    /// Walking up from a relative single-component path (e.g. "test")
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
        let (_temp, dir) = temp_utf8_dir();

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
        let (_temp, dir) = temp_utf8_dir();

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
        let (_temp, dir) = temp_utf8_dir();

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

    // test_generate_eunit_wrapper_with_lifecycle no longer applies — EUnit wrappers are no longer generated.

    /// Inserting `"."` and the absolute CWD into `seen` without canonicalization would
    /// treat them as different entries, making a single-package run appear to have two
    /// packages and incorrectly triggering multi-package module-name prefixing.
    ///
    /// This test verifies that the canonicalization in `discovered_packages` deduplicates
    /// the relative `"."` and the same path expressed as an absolute path.
    /// This test uses a temp dir with an explicit `set_current_dir` instead of
    /// relying on the cargo-provided CWD because another test in this file also
    /// mutates the process CWD. The `#[serial(cwd)]` annotation prevents the two
    /// from racing. On macOS, `/var` is a symlink to `/private/var`, so the temp
    /// dir's `current_dir()` path and `canonicalize(".")` can diverge unless both
    /// go through `canonical_path`.
    #[test]
    #[serial_test::serial(cwd)]
    fn test_canonical_dedup_relative_and_absolute_same_path() {
        // RAII guard so CWD is restored even if an assertion panics.
        struct CwdGuard(std::path::PathBuf);
        impl Drop for CwdGuard {
            fn drop(&mut self) {
                std::env::set_current_dir(&self.0).unwrap();
            }
        }

        let temp = tempfile::TempDir::new().unwrap();
        let _cwd_guard = CwdGuard(std::env::current_dir().unwrap());
        std::env::set_current_dir(temp.path()).unwrap();

        // Get the absolute CWD — on macOS this may be `/var/folders/…` (not
        // symlink-resolved) while `canonicalize(".")` returns `/private/var/…`.
        // Both must canonicalize to the same path so they deduplicate in `seen`
        // and don't incorrectly flip multi-package mode.
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
        let (_temp, dir) = temp_utf8_dir();

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
                .map(|n| beamtalk_codegen::core_erlang::to_module_name(n));

            if let Some(ref prefix) = pkg_name_slug {
                for tc in &mut test_classes {
                    let stem = beamtalk_codegen::core_erlang::to_module_name(&tc.class_name);
                    tc.module_name = format!("bt@{prefix}@{stem}");
                }
            }

            assert_eq!(test_classes[0].module_name, expected_module);
        }
    }

    /// BT-4350: two files in the *same* package declaring the same `TestCase`
    /// subclass get no package prefix, so both map to `bt@protocol_test`. The
    /// runner must reject the second instead of letting it overwrite the first's
    /// `.core`/`.beam`, and the error must name both files.
    #[test]
    fn test_duplicate_test_module_name_in_scope_is_rejected() {
        let (_temp, dir) = temp_utf8_dir();
        let file_a = dir.join("test/http1/protocol_test.bt");
        let file_b = dir.join("test/websocket/protocol_test.bt");
        for f in [&file_a, &file_b] {
            fs::create_dir_all(f.parent().unwrap()).unwrap();
            fs::write(
                f,
                "TestCase subclass: ProtocolTest\n  testOk => self assert: true\n",
            )
            .unwrap();
        }

        let (classes_a, _) = discover_test_classes(&file_a).unwrap();
        let (classes_b, _) = discover_test_classes(&file_b).unwrap();
        assert_eq!(classes_a[0].module_name, "bt@protocol_test");
        assert_eq!(classes_b[0].module_name, "bt@protocol_test");

        // First file in scope: no collision.
        let mut compiled = Vec::new();
        check_duplicate_test_module(&compiled, &classes_a[0], &file_a).unwrap();
        compiled.push(CompiledTest {
            source_file: file_a.clone(),
            test_class: classes_a.into_iter().next().unwrap(),
        });

        // Second file maps to the same module: error naming both files.
        let err = check_duplicate_test_module(&compiled, &classes_b[0], &file_b)
            .expect_err("duplicate test module name must be rejected");
        let msg = err.to_string();
        assert!(msg.contains("bt@protocol_test"), "{msg}");
        assert!(msg.contains(file_a.as_str()), "{msg}");
        assert!(msg.contains(file_b.as_str()), "{msg}");

        // A distinct class name in the same package is fine.
        let file_c = dir.join("test/http1/http1_protocol_test.bt");
        fs::write(
            &file_c,
            "TestCase subclass: HTTP1ProtocolTest\n  testOk => self assert: true\n",
        )
        .unwrap();
        let (classes_c, _) = discover_test_classes(&file_c).unwrap();
        check_duplicate_test_module(&compiled, &classes_c[0], &file_c).unwrap();
    }

    /// BT-4334: test-file compilation must declare the same checker scope as
    /// `build`, or `test --warnings-as-errors` passes code the REPL live
    /// recheck rejects (DNU/Type suppressed under `ModuleOnly`).
    #[test]
    fn test_compile_scope_matches_build() {
        use beamtalk_core::semantic_analysis::KnowledgeScope;

        // A package file without dependencies: project-complete, no dep guard.
        let (_temp, dir) = temp_utf8_dir();
        fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let src_dir = dir.join("test");
        fs::create_dir_all(&src_dir).unwrap();
        let file = src_dir.join("my_test.bt");
        fs::write(&file, "").unwrap();
        let (scope, has_deps) = test_compile_scope(&file);
        assert_eq!(scope, KnowledgeScope::ProjectComplete);
        assert!(!has_deps);

        // A package file with dependencies: the pre-WS3 unknown-extension
        // guard applies, exactly as `build` sets it.
        let (_temp, dir) = temp_utf8_dir();
        fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n\n[dependencies]\nfoo = { path = \"../foo\" }\n",
        )
        .unwrap();
        let src_dir = dir.join("test");
        fs::create_dir_all(&src_dir).unwrap();
        let file = src_dir.join("my_test.bt");
        fs::write(&file, "").unwrap();
        let (scope, has_deps) = test_compile_scope(&file);
        assert_eq!(scope, KnowledgeScope::ProjectComplete);
        assert!(has_deps);

        // A manifest-less file keeps the conservative default.
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("loose.bt");
        fs::write(&file, "").unwrap();
        let (scope, has_deps) = test_compile_scope(&file);
        assert_eq!(scope, KnowledgeScope::ModuleOnly);
        assert!(!has_deps);
    }
}
