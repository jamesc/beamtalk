// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build the Beamtalk standard library.
//!
//! **DDD Context:** CLI / Build System
//!
//! Compiles all `lib/*.bt` files through the normal pipeline with `--stdlib-mode`
//! and outputs `.beam` files to `runtime/apps/beamtalk_stdlib/ebin/`.
//! Uses incremental builds: skips compilation if all outputs are newer than
//! all inputs (source files, compiler binary, and runtime `.beam` files).
//!
//! Part of ADR 0007 (Compilable Stdlib with Primitive Injection).

use crate::beam_compiler::{
    BeamCompiler, ClassHierarchyContext, CompileContext, compile_source_with_bindings,
};
use crate::commands::app_file;
use crate::commands::build::build_alias_metadata;
use beamtalk_core::semantic_analysis::alias_registry::AliasRegistry;
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write;
use std::fs;
use std::time::SystemTime;
use tracing::{debug, info, instrument};

/// Default path to stdlib source files (relative to project root).
const STDLIB_SOURCE_DIR: &str = "stdlib/src";

/// Default output path for compiled stdlib BEAM files (relative to project root).
const STDLIB_EBIN_DIR: &str = "runtime/apps/beamtalk_stdlib/ebin";

/// Build the standard library.
///
/// Finds all `.bt` files in `lib/`, compiles them with stdlib mode enabled,
/// and writes `.beam` files to `runtime/apps/beamtalk_stdlib/ebin/`.
/// Skips the build if all outputs are newer than all inputs (incremental).
#[instrument(skip_all)]
pub fn build_stdlib(quiet: bool, warnings_as_errors: bool) -> Result<()> {
    info!("Starting stdlib build");

    let lib_dir = Utf8PathBuf::from(STDLIB_SOURCE_DIR);
    let ebin_dir = Utf8PathBuf::from(STDLIB_EBIN_DIR);

    if !lib_dir.exists() {
        miette::bail!("Standard library source directory '{}' not found", lib_dir);
    }

    // Find all .bt files in lib/
    let source_files = find_stdlib_files(&lib_dir)?;

    if source_files.is_empty() {
        println!("No .bt source files found in '{lib_dir}'");
        return Ok(());
    }

    check_duplicate_module_names(&source_files)?;

    // Incremental build: skip if all outputs are newer than all inputs
    if is_stdlib_up_to_date(
        &ebin_dir,
        &source_files,
        discover_runtime_ebin_dirs().as_deref(),
    ) {
        println!("Stdlib up to date (skipped)");
        return Ok(());
    }

    info!(count = source_files.len(), "Found stdlib source files");

    // Create ebin directory
    fs::create_dir_all(&ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create ebin directory '{ebin_dir}'"))?;

    // Clean stale .beam files from previous builds (e.g. renamed/removed .bt sources)
    clean_ebin_dir(&ebin_dir)?;

    if !quiet {
        println!("Compiling {} stdlib module(s)...", source_files.len());
    }

    // Create a temporary directory for .core files
    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory for Core Erlang files")?;
    let temp_path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;

    // Compiler options: stdlib mode enabled
    let options = stdlib_compiler_options(warnings_as_errors);

    // BT-295 / ADR 0007 Phase 3: Build primitive binding table from ALL stdlib sources.
    // This is used during compilation so that @primitive expressions in method bodies
    // can reference the runtime dispatch modules.
    info!("Building primitive binding table from stdlib sources");
    let bindings = beamtalk_codegen::core_erlang::primitive_bindings::load_from_directory(
        lib_dir.as_std_path(),
    );
    info!(
        binding_count = bindings.len(),
        "Loaded primitive bindings from stdlib"
    );

    // Extract native type specs from runtime .beam files so FFI calls in stdlib
    // get proper type inference instead of Dynamic (ADR 0075).
    let native_type_registry = extract_stdlib_type_specs();

    // BT-2935: live same-run alias pre-pass — see its doc for why.
    let alias_sources = collect_stdlib_alias_sources(&source_files)?;
    // BT-3034: live same-run protocol pre-pass, mirroring the alias pre-pass
    // immediately above — see `collect_stdlib_protocol_infos`'s doc for why.
    let protocol_infos = collect_stdlib_protocol_infos(&source_files);
    let compile_ctx = CompileContext {
        native_type_registry: native_type_registry.map(std::sync::Arc::new),
        hierarchy: ClassHierarchyContext {
            pre_loaded_protocols: protocol_infos,
            pre_loaded_aliases: stdlib_pre_loaded_aliases(&alias_sources),
            ..ClassHierarchyContext::default()
        },
        ..CompileContext::default()
    };

    // Compile each .bt file to .core (files are independent, no ordering required)
    let (core_files, class_metadata, protocol_modules) = compile_all_stdlib_files(
        &source_files,
        &temp_path,
        quiet,
        &options,
        &bindings,
        &compile_ctx,
    )?;

    // Batch compile .core → .beam into ebin directory
    info!("Compiling Core Erlang to BEAM");
    let compiler = BeamCompiler::new(ebin_dir.clone());
    compiler
        .compile_batch(&core_files)
        .wrap_err("Failed to compile stdlib Core Erlang to BEAM")?;

    // BT-2938: `{type_aliases, [...]}` `.app`/`.app.src` env metadata — the
    // same `build_alias_metadata` extraction the ordinary `beamtalk build`
    // pipeline already uses (`build.rs`'s `ClassIndexResult::all_alias_infos`
    // call site), independently re-parsing `source_files` for doc
    // comments/display expansions `AliasSource`/`ClassHierarchyContext`
    // above don't carry. This is what lets `beamtalk_repl_state:new/3` (the
    // REPL/workspace session's alias-seeding path) and `browse-type-aliases`
    // read stdlib's own aliases back via `application:get_env(beamtalk_stdlib,
    // type_aliases)`.
    let alias_metadata = build_alias_metadata(&source_files);

    generate_app_file(
        &ebin_dir,
        &source_files,
        &class_metadata,
        &protocol_modules,
        &alias_metadata,
    )?;
    // Also update .app.src so rebar3 picks up the classes env
    let app_src_dir = Utf8PathBuf::from("runtime/apps/beamtalk_stdlib/src");
    if app_src_dir.exists() {
        generate_app_src_file(
            &app_src_dir,
            &class_metadata,
            &protocol_modules,
            &alias_metadata,
        )?;
    }

    // Generate Rust builtins file from parsed class metadata and aliases.
    generate_builtins_rs(
        &class_metadata,
        &alias_source_texts_sorted_by_name(alias_sources),
    )?;

    // BT-3085: Generate the Erlang-side twin of the same builtin-class list,
    // replacing the hand-typed `beamtalk_class_metadata:all_builtins/0` table.
    generate_erlang_builtins_hrl(&class_metadata)?;

    println!("Built {} stdlib modules", source_files.len());

    Ok(())
}

/// Compiles every stdlib source file to Core Erlang, collecting class
/// metadata and protocol module names along the way.
///
/// Protocol-only files (e.g. `printable.bt`) have no class definition — they
/// are still compiled so the protocol gets registered at runtime, but
/// contribute no `ClassMeta`. Files are independent (no compile ordering
/// required); cross-file class/alias visibility comes entirely from
/// `compile_ctx` (built by the caller before this runs).
fn compile_all_stdlib_files(
    source_files: &[Utf8PathBuf],
    temp_path: &Utf8Path,
    quiet: bool,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable,
    compile_ctx: &CompileContext<'_>,
) -> Result<(Vec<Utf8PathBuf>, Vec<ClassMeta>, Vec<String>)> {
    let mut core_files = Vec::new();
    let mut class_metadata = Vec::new();
    let mut protocol_modules = Vec::new();
    for source_file in source_files {
        let module_name = module_name_from_path(source_file)?;
        let core_file = temp_path.join(format!("{module_name}.core"));

        if is_protocol_only_file(source_file)? {
            if !quiet {
                println!("  Compiling {source_file} (protocol)...");
            }
            compile_stdlib_file(
                source_file,
                &module_name,
                &core_file,
                options,
                bindings,
                compile_ctx,
            )?;
            core_files.push(core_file);
            protocol_modules.push(module_name);
            continue;
        }

        // Extract class metadata (class_name, superclass) before compilation
        let meta = extract_class_metadata(source_file, &module_name)?;
        class_metadata.push(meta);

        if !quiet {
            println!("  Compiling {source_file}...");
        }
        compile_stdlib_file(
            source_file,
            &module_name,
            &core_file,
            options,
            bindings,
            compile_ctx,
        )?;
        core_files.push(core_file);
    }
    Ok((core_files, class_metadata, protocol_modules))
}

/// Remove all `.beam` and `.app` files from the ebin directory.
///
/// Ensures no stale artifacts remain from renamed or removed `.bt` sources.
fn clean_ebin_dir(ebin_dir: &Utf8Path) -> Result<()> {
    for entry in fs::read_dir(ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read ebin directory '{ebin_dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if let Some(ext) = path.extension() {
            if ext == "beam" || ext == "app" {
                fs::remove_file(&path)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("Failed to remove '{}'", path.display()))?;
            }
        }
    }
    Ok(())
}

/// Discover the runtime/stdlib-Erlang/workspace/compiler `ebin/` directories
/// that `is_stdlib_up_to_date`'s final check treats as build inputs — a
/// `.beam` file in any of them newer than the stdlib output means native
/// specs may have changed underneath it.
///
/// Returns `None` when the runtime layout can't be located at all (dev vs.
/// installed candidates all missing, or an explicit `BEAMTALK_RUNTIME_DIR`
/// pointing nowhere useful); callers should treat that as "force rebuild to
/// be safe", same as any other missing-input case in this file.
///
/// Split out from `is_stdlib_up_to_date` itself (BT-3357) so the directory
/// *list* is an injectable parameter there: this function's real-filesystem
/// discovery (`beamtalk_cli::repl_startup::find_runtime_dir_with_layout`) has
/// no test seam of its own, but `is_stdlib_up_to_date` no longer needs one —
/// unit tests can hand it a synthetic list of temp directories directly.
fn discover_runtime_ebin_dirs() -> Option<Vec<std::path::PathBuf>> {
    use beamtalk_cli::repl_startup;
    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout().ok()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);
    Some(vec![
        paths.runtime_ebin,
        paths.stdlib_erlang_ebin,
        paths.workspace_ebin,
        paths.compiler_ebin,
    ])
}

/// Check if the stdlib build is up to date (all outputs newer than all inputs).
///
/// Inputs: `lib/*.bt` source files, the compiler binary (`current_exe`), and
/// the runtime `.beam` directories in `runtime_ebin_dirs` (see
/// [`discover_runtime_ebin_dirs`] for how the real build discovers these).
/// Output: the `ebin/` directory modification time.
///
/// `runtime_ebin_dirs` being `None` means the caller couldn't discover the
/// runtime layout at all and is itself forcing a rebuild to be safe — same
/// as any other missing-input case below.
///
/// Returns `false` (needs rebuild) if any input is missing or any error occurs.
fn is_stdlib_up_to_date(
    ebin_dir: &Utf8Path,
    source_files: &[Utf8PathBuf],
    runtime_ebin_dirs: Option<&[std::path::PathBuf]>,
) -> bool {
    // Must have .beam files already
    let Ok(entries) = fs::read_dir(ebin_dir) else {
        return false;
    };
    let beam_count = entries
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
        .count();
    if beam_count == 0 {
        return false;
    }

    // Detect deleted/renamed source files: beam count should match source count
    if beam_count != source_files.len() {
        info!(
            beam_count,
            source_count = source_files.len(),
            "Beam/source count mismatch — forcing rebuild"
        );
        return false;
    }

    // Get the oldest .beam output mtime
    let Some(oldest_output) = oldest_mtime_in_dir(ebin_dir, "beam") else {
        return false;
    };

    // Check source files
    for src in source_files {
        match fs::metadata(src.as_std_path()).and_then(|m| m.modified()) {
            Ok(t) if t > oldest_output => {
                info!(file = %src, "Source newer than stdlib output");
                return false;
            }
            Err(_) => return false,
            _ => {}
        }
    }

    // Check compiler binary — if we can't locate it, force rebuild to be safe
    match std::env::current_exe() {
        Ok(exe) => match fs::metadata(&exe).and_then(|m| m.modified()) {
            Ok(t) if t > oldest_output => {
                info!("Compiler binary newer than stdlib output");
                return false;
            }
            Err(_) => return false,
            _ => {}
        },
        Err(_) => return false,
    }

    // Check runtime .beam files — if any runtime/stdlib/workspace/compiler ebin
    // has a newer .beam, force rebuild (specs may have changed).
    // Caller passes layout-aware discovery so invalidation matches spec
    // extraction paths; `None` means discovery itself failed.
    let Some(runtime_ebins) = runtime_ebin_dirs else {
        return false;
    };
    for runtime_ebin in runtime_ebins {
        let Ok(entries) = fs::read_dir(runtime_ebin) else {
            continue;
        };
        for entry in entries.flatten() {
            if entry.path().extension().is_some_and(|ext| ext == "beam") {
                match entry.metadata().and_then(|m| m.modified()) {
                    Ok(t) if t > oldest_output => {
                        info!("Runtime .beam newer than stdlib output");
                        return false;
                    }
                    Err(_) => return false,
                    _ => {}
                }
            }
        }
    }

    info!("Stdlib is up to date");
    true
}

/// Find the oldest modification time of files with the given extension in a directory.
fn oldest_mtime_in_dir(dir: &Utf8Path, ext: &str) -> Option<SystemTime> {
    let mut oldest: Option<SystemTime> = None;
    let entries = fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        if entry.path().extension().is_some_and(|e| e == ext) {
            if let Ok(mtime) = entry.metadata().and_then(|m| m.modified()) {
                oldest = Some(match oldest {
                    Some(prev) if mtime < prev => mtime,
                    Some(prev) => prev,
                    None => mtime,
                });
            }
        }
    }
    oldest
}

/// Find all `.bt` files in the stdlib source directory, recursively.
///
/// Recursive so `stdlib/src/` can be grouped into subdirectories
/// (`collections/`, `actors/`, …) without silently dropping classes from the
/// build. Subdirectories are purely editorial — they do *not* affect module
/// names; see [`module_name_from_path`].
fn find_stdlib_files(lib_dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    beamtalk_core::file_walker::FileWalker::source_files().walk(lib_dir)
}

/// Reject two stdlib sources that would compile to the same module.
///
/// Module names come from the file stem alone, so `collections/array.bt` and
/// `legacy/array.bt` both become `bt@stdlib@array` — the second silently
/// clobbering the first in `ebin/`. Flat layouts got uniqueness for free from
/// the filesystem; nested ones have to check.
///
/// Keyed on the *derived module name*, not the raw stem, because
/// [`module_name_from_path`] case-folds: `beamerror.bt` and `Beamerror.bt` are
/// distinct filenames that both produce `bt@stdlib@beamerror`. Comparing stems
/// would wave that collision straight through.
fn check_duplicate_module_names(source_files: &[Utf8PathBuf]) -> Result<()> {
    let mut seen: std::collections::HashMap<String, &Utf8Path> = std::collections::HashMap::new();
    for file in source_files {
        let module = module_name_from_path(file)?;
        if let Some(first) = seen.insert(module.clone(), file.as_path()) {
            miette::bail!(
                "Stdlib sources '{first}' and '{file}' both compile to module '{module}'. \
                 Module names come from the file stem alone (case-folded), and subdirectories \
                 do not namespace them, so stdlib class file names must be unique across all \
                 subdirectories. Rename one of the two files."
            );
        }
    }
    Ok(())
}

/// Extract the module name from a `.bt` file path.
///
/// ADR 0016: All stdlib classes use `bt@stdlib@{snake_case}` prefix.
/// The `@` separator is legal in unquoted Erlang atoms and follows
/// the Gleam convention (`gleam@list`, `gleam@string`).
///
/// Only the file *stem* is used — a subdirectory under `stdlib/src/` never
/// becomes part of the module name. This deliberately diverges from user
/// packages, where `src/util/math.bt` becomes `util@math`
/// (`build::compute_relative_module`). Stdlib keeps the flat mapping because
/// it is a closed-form function of the class name: `compiled_module_name`,
/// `PrimitiveBindingTable::runtime_module_for_class` and
/// `beamtalk_primitive:module_for_value/1` all derive `bt@stdlib@{snake}`
/// with no path and no lookup table available. Folding directories into the
/// atom would couple that hot dispatch path to a purely cosmetic layout
/// choice.
fn module_name_from_path(path: &Utf8Path) -> Result<String> {
    let stem = path
        .file_stem()
        .ok_or_else(|| miette::miette!("File '{}' has no name", path))?;

    // Validate module name
    if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
        miette::bail!(
            "Invalid module name '{}': must contain only alphanumeric characters and underscores",
            stem
        );
    }

    let snake = beamtalk_codegen::core_erlang::to_module_name(stem);

    // ADR 0016: All stdlib modules use bt@stdlib@ prefix
    Ok(format!("bt@stdlib@{snake}"))
}

/// Extract native type specs from runtime `.beam` files for FFI type inference.
///
/// Discovers `.beam` files from the runtime and stdlib Erlang ebin directories
/// and extracts their `-spec` attributes. Returns `None` if no specs could be
/// extracted (e.g., runtime not yet compiled).
pub(crate) fn extract_stdlib_type_specs() -> Option<NativeTypeRegistry> {
    use crate::beam_compiler;
    use beamtalk_cli::repl_startup;

    // Use the same runtime discovery as the build worker so paths stay in sync
    // with the runtime layout (dev vs installed).
    let (runtime_dir, layout): (std::path::PathBuf, _) =
        if let Ok(result) = repl_startup::find_runtime_dir_with_layout() {
            result
        } else {
            debug!("Runtime not found — skipping FFI type spec extraction");
            return None;
        };
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    // Collect ebin directories that contain Erlang FFI modules with -spec attributes.
    // Convert from std PathBuf to camino Utf8PathBuf (required by beam_compiler API).
    let mut ebin_dirs: Vec<Utf8PathBuf> = Vec::new();
    let candidates: Vec<std::path::PathBuf> = vec![
        paths.runtime_ebin,
        paths.stdlib_erlang_ebin,
        paths.workspace_ebin,
        paths.compiler_ebin,
    ];
    for dir in candidates {
        if dir.exists() {
            if let Ok(utf8) = Utf8PathBuf::from_path_buf(dir) {
                ebin_dirs.push(utf8);
            }
        }
    }

    let beam_files = beam_compiler::discover_dependency_beam_files(&ebin_dirs);
    if beam_files.is_empty() {
        debug!("No runtime .beam files found for stdlib FFI type specs");
        return None;
    }

    // Cache directory under the runtime _build to avoid re-extracting specs
    let cache_dir: Utf8PathBuf = Utf8PathBuf::from_path_buf(runtime_dir.join("_build/type_cache"))
        .unwrap_or_else(|p| Utf8PathBuf::from(p.to_string_lossy().as_ref()));

    match beam_compiler::extract_beam_specs(&beam_files, &cache_dir) {
        Ok(registry) => {
            if registry.module_count() > 0 {
                info!(
                    modules = registry.module_count(),
                    functions = registry.function_count(),
                    "Extracted FFI type specs for stdlib build"
                );
            }
            Some(registry)
        }
        Err(e) => {
            debug!("Failed to extract type specs for stdlib build: {e}");
            None
        }
    }
}

/// Compile a single stdlib `.bt` file to Core Erlang.
fn compile_stdlib_file(
    path: &Utf8Path,
    module_name: &str,
    core_file: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable,
    ctx: &CompileContext<'_>,
) -> Result<()> {
    compile_source_with_bindings(path, module_name, core_file, options, bindings, ctx, None)
        .map(|_diags| ())
}

/// Class modifier flags from class hierarchy analysis.
///
/// Groups the boolean modifiers that describe a class's constraints (sealed,
/// abstract, typed, native). Extracted from `ClassMeta` to keep that struct
/// focused on identity and structure.
#[derive(Default)]
#[allow(clippy::struct_excessive_bools)] // these 4 bools are orthogonal class modifier flags
struct ClassModifiers {
    /// Whether the class is sealed (cannot be subclassed).
    is_sealed: bool,
    /// Whether the class is abstract (cannot be instantiated directly).
    is_abstract: bool,
    /// Whether the class has the explicit `typed` modifier.
    is_typed: bool,
    /// Whether this class delegates to a native Erlang backing module (ADR 0056).
    is_native: bool,
}

/// Metadata for a single stdlib class, used to generate the load-order file
/// and the generated builtins module.
#[allow(clippy::struct_field_names)] // domain names like class_name match the domain model
struct ClassMeta {
    /// Erlang module name (e.g., `bt@stdlib@integer`).
    module_name: String,
    /// Beamtalk class name (e.g., `Integer`).
    class_name: String,
    /// Name of the superclass, or `"none"` for root classes.
    superclass_name: String,
    /// Class modifier flags (sealed, abstract, typed, native).
    modifiers: ClassModifiers,
    /// Class kind: object, value, or actor (ADR 0067/0070).
    class_kind: beamtalk_core::ast::ClassKind,
    /// Instance state (field) names declared in the class.
    state: Vec<String>,
    /// Declared type annotations for state fields (field name → type).
    state_types: Vec<(String, DeclaredType)>,
    /// Which state fields have an explicit default value (field name → has default).
    /// BT-1976: Carries cross-file default-value presence so downstream
    /// consumers can identify typed-no-default fields without the AST.
    state_has_default: Vec<(String, bool)>,
    /// Instance method signatures.
    methods: Vec<MethodMeta>,
    /// Class-side method signatures.
    class_methods: Vec<MethodMeta>,
    /// Class variable names.
    class_variables: Vec<String>,
    /// Type parameters for generic classes (e.g., `["T", "E"]` for `Result(T, E)`).
    type_params: Vec<String>,
    /// Type arguments passed to the superclass, e.g. `["E"]` for
    /// `Collection(E) subclass: List(E)`. Empty when the parent isn't parametric.
    superclass_type_args: Vec<DeclaredType>,
    /// Declared sendability handle scope (ADR 0103, `handleScope: #symbol`).
    /// The bare symbol text (e.g. `"process"`), or `None` when undeclared.
    handle_scope: Option<String>,
}

/// Sort class metadata by class name — every generated-artifact writer
/// (`generate_builtins_rs`, `generate_erlang_builtins_hrl`) needs the same
/// deterministic ordering so the checked-in `.rs`/`.hrl` files are stable
/// across regenerations. Single implementation per CLAUDE.md's
/// "No duplicate implementations" rule.
fn sorted_by_class_name(class_metadata: &[ClassMeta]) -> Vec<&ClassMeta> {
    let mut sorted: Vec<&ClassMeta> = class_metadata.iter().collect();
    sorted.sort_by_key(|m| &m.class_name);
    sorted
}

/// Metadata for a single method, extracted from the AST.
struct MethodMeta {
    /// Message selector (e.g., `"increment"` or `"add:"`).
    selector: String,
    /// Number of arguments the method accepts.
    arity: usize,
    /// Method dispatch kind.
    kind: MethodKindMeta,
    /// Whether this method is sealed (cannot be overridden).
    is_sealed: bool,
    /// Whether this method is internal (package-scoped visibility, ADR 0071).
    is_internal: bool,
    /// Whether this method spawns its block argument in a separate BEAM process.
    spawns_block: bool,
    /// Return type annotation (e.g., `"Integer"`), if present.
    return_type: Option<DeclaredType>,
    /// Parameter type annotations, one per parameter. `None` means untyped.
    param_types: Vec<Option<DeclaredType>>,
    /// Doc comment extracted from the source (`///` lines before the method).
    doc: Option<String>,
}

/// Simplified method kind for code generation.
enum MethodKindMeta {
    /// Standard method dispatch.
    Primary,
}

impl MethodKindMeta {
    /// Convert from the AST method kind representation.
    fn from_ast(kind: beamtalk_core::ast::MethodKind) -> Self {
        match kind {
            beamtalk_core::ast::MethodKind::Primary => Self::Primary,
        }
    }

    /// Return the Rust expression string for this kind (used in codegen output).
    fn to_rust_expr(&self) -> &'static str {
        match self {
            Self::Primary => "MethodKind::Primary",
        }
    }
}

/// Convert an AST method definition to `MethodMeta`.
fn method_def_to_meta(m: &beamtalk_core::ast::MethodDefinition) -> MethodMeta {
    MethodMeta {
        selector: m.selector.name().to_string(),
        arity: m.selector.arity(),
        kind: MethodKindMeta::from_ast(m.kind),
        is_sealed: m.is_sealed,
        is_internal: m.is_internal,
        spawns_block: false,
        return_type: m.return_type.as_ref().map(DeclaredType::from),
        param_types: m
            .parameters
            .iter()
            .map(|p| p.type_annotation.as_ref().map(DeclaredType::from))
            .collect(),
        doc: m.doc_comment.clone(),
    }
}

/// Synthesize auto-generated getter, functional-updater, and keyword constructor
/// methods for Value subclasses with state declarations.
///
/// This mirrors the logic in `add_value_auto_methods` in the semantic analyzer
/// (`class_hierarchy/mod.rs`) and ensures the type checker can resolve
/// state-based accessors without having to process the source `.bt` file first.
#[allow(clippy::too_many_lines)] // synthesizes getter + updater + constructor metadata inline
fn synthesize_value_auto_methods(
    class: &beamtalk_core::ast::ClassDefinition,
    class_name: &str,
    methods: &mut Vec<MethodMeta>,
    class_methods: &mut Vec<MethodMeta>,
) {
    let user_selectors: std::collections::HashSet<String> =
        methods.iter().map(|m| m.selector.clone()).collect();
    let user_class_selectors: std::collections::HashSet<String> =
        class_methods.iter().map(|m| m.selector.clone()).collect();

    for slot in &class.state {
        let slot_name = slot.name.name.as_str();

        // Auto getter: `fieldName` → slot type (or Object if unannotated)
        if !user_selectors.contains(slot_name) {
            let default_str = slot.default_value.as_ref().map_or_else(
                || "nil".to_string(),
                beamtalk_core::semantic_analysis::class_hierarchy::format_default_value,
            );
            let getter_doc = format!(
                "Returns the `{slot_name}` field value. Default: `{default_str}`.\n\n*(compiler-generated)*"
            );
            methods.push(MethodMeta {
                selector: slot_name.to_string(),
                arity: 0,
                kind: MethodKindMeta::Primary,
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: slot.type_annotation.as_ref().map(DeclaredType::from),
                param_types: vec![],
                doc: Some(getter_doc),
            });
        }

        // Auto functional updater: `withFieldName:` → Self type
        let with_sel = beamtalk_core::synthetic_selectors::with_star_selector(slot_name);
        if !user_selectors.contains(&with_sel) {
            let param_type = slot.type_annotation.as_ref().map(DeclaredType::from);
            let setter_doc = format!(
                "Returns a new `{class_name}` with `{slot_name}` set to the given value.\n\n*(compiler-generated)*"
            );
            methods.push(MethodMeta {
                selector: with_sel,
                arity: 1,
                kind: MethodKindMeta::Primary,
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::simple(class_name)),
                param_types: vec![param_type],
                doc: Some(setter_doc),
            });
        }
    }

    // Auto keyword constructor on the class side: `field1:field2:...`
    let kw_sel: String = beamtalk_core::synthetic_selectors::keyword_constructor_selector(
        class.state.iter().map(|s| s.name.name.as_str()),
    );
    if !user_class_selectors.contains(&kw_sel) {
        let arity = class.state.len();
        let param_types = class
            .state
            .iter()
            .map(|s| s.type_annotation.as_ref().map(DeclaredType::from))
            .collect();
        let args_desc: String = class
            .state
            .iter()
            .map(|s| {
                let dv = s.default_value.as_ref().map_or_else(
                    || "nil".to_string(),
                    beamtalk_core::semantic_analysis::class_hierarchy::format_default_value,
                );
                format!("{} (default: {})", s.name.name, dv)
            })
            .collect::<Vec<_>>()
            .join(", ");
        let ctor_doc =
            format!("Creates a new `{class_name}`. Args: {args_desc}.\n\n*(compiler-generated)*");
        class_methods.push(MethodMeta {
            selector: kw_sel,
            arity,
            kind: MethodKindMeta::Primary,
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(DeclaredType::simple(class_name)),
            param_types,
            doc: Some(ctor_doc),
        });
    }
}

/// Iterate `methods`, set `spawns_block = true` for every selector listed in
/// `selectors`, then verify every selector was matched. Bails with a diagnostic
/// naming `context` and any unmatched selectors — an unmatched selector means
/// the stdlib source no longer defines a method this metadata assumes exists.
fn mark_spawns_for_selectors(
    methods: &mut [MethodMeta],
    selectors: &[&str],
    context: &str,
) -> Result<()> {
    for m in methods.iter_mut() {
        if selectors.iter().any(|&s| s == m.selector) {
            m.spawns_block = true;
        }
    }
    let missing: Vec<&str> = selectors
        .iter()
        .copied()
        .filter(|&s| methods.iter().all(|m| m.selector != s))
        .collect();
    if !missing.is_empty() {
        miette::bail!(
            "{context} metadata mismatch: expected spawning selectors {selectors:?} \
             but not found: {missing:?}"
        );
    }
    Ok(())
}

/// Mark Timer class methods that spawn their block argument in a separate BEAM process.
///
/// BT-1312: replaces hardcoded list in `validators.rs` so the self-capture validator
/// can skip false-positive warnings.
fn mark_timer_spawns(class_methods: &mut [MethodMeta]) -> Result<()> {
    mark_spawns_for_selectors(class_methods, &["after:do:", "every:do:"], "Timer")
}

/// Mark `Parallel` class methods that spawn their block arguments in separate
/// BEAM processes (BT-2974).
///
/// Feeds the `spawns_block_selectors()` metadata a future lint uses to warn when
/// a synchronous `self` send appears inside a spawned-block argument (deadlock
/// risk: an actor blocks in `Parallel all:`, a block does a synchronous send back
/// to the same actor, and hangs).
fn mark_parallel_spawns(class_methods: &mut [MethodMeta]) -> Result<()> {
    mark_spawns_for_selectors(class_methods, &["all:", "all:timeout:", "any:"], "Parallel")
}

/// Mark `Collection>>parallelCollect:` and `parallelCollect:maxConcurrency:`
/// as spawning their block argument (BT-2974, BT-3006 follow-up) — both
/// delegate to `Parallel all:` under the hood (directly, or via the internal
/// `runChunked:maxConcurrency:` helper), same rationale as
/// `mark_parallel_spawns`. The internal `runChunked:...` helpers are not
/// marked: their own parameter is an already-wrapped `List(Block)`, not the
/// user's raw block, so the same-actor-deadlock risk this metadata guards
/// against doesn't apply to their own call sites the way it does to the two
/// public entry points.
fn mark_parallel_collect_spawns(methods: &mut [MethodMeta]) -> Result<()> {
    mark_spawns_for_selectors(
        methods,
        &["parallelCollect:", "parallelCollect:maxConcurrency:"],
        "Collection",
    )
}

/// A single stdlib type-alias declaration collected by
/// `collect_stdlib_alias_sources` (BT-2935), carrying both its
/// reconstructed `AliasInfo` (for immediately seeding *this* run's own
/// `pre_loaded_aliases`) and its exact declaration source text (for
/// persisting into `generated_builtins.rs`, so a consumer with no direct
/// access to `stdlib/src/*.bt` — e.g. a REPL/workspace session, BT-2938 —
/// can still reconstruct it later via
/// `ClassHierarchy::generated_stdlib_aliases`).
struct AliasSource {
    info: beamtalk_core::semantic_analysis::alias_registry::AliasInfo,
    text: String,
}

/// Scans every stdlib source file for `type Name = ...` declarations,
/// reconstructing each into an `AliasSource` immediately (BT-2935).
///
/// **Why a live, same-run pre-pass — not a seed from
/// `ClassHierarchy::generated_stdlib_aliases`'s persisted snapshot** (the
/// design this replaced during review): `build-stdlib` always runs with
/// `--warnings-as-errors` (`Justfile`'s `build-stdlib` recipe), and an
/// unresolved cross-file alias reference surfaces as a
/// `DiagnosticCategory::Type` warning — a category *not* excluded from
/// warnings-as-errors promotion, unlike `UnresolvedClass` (see
/// `beam_compiler.rs`'s exclusion list). Seeding from the *previous* run's
/// persisted snapshot instead of a live scan would mean a stdlib change that
/// declares `type Foo = ...` in one file and references `Foo` cross-file in
/// another, landed in the *same* commit, bails before `generate_builtins_rs`
/// ever runs — so `Foo` would never get persisted, and every subsequent
/// `build-stdlib` run would repeat the exact same failure with no way out
/// short of splitting the change across two builds or dropping
/// `--warnings-as-errors`. A live pre-pass has direct access to every
/// stdlib source file right now, so — unlike a consumer with no source
/// access, which genuinely has no better option than the persisted
/// snapshot — it never needs to fall back to stale data at all.
///
/// Fails loudly (`Result::Err`, not a silent skip) if a freshly-sliced
/// declaration fails to re-parse via
/// [`AliasRegistry::from_source_text`] — that would indicate a bug in the
/// slicing logic itself (e.g. a span not covering a standalone-parseable
/// declaration), not a tolerable degradation. Contrast
/// `ClassHierarchy::generated_stdlib_aliases`'s defensive skip-on-failure,
/// which exists only to tolerate a corrupted or hand-edited *generated*
/// file at runtime, long after this build-time check already passed.
fn collect_stdlib_alias_sources(source_files: &[Utf8PathBuf]) -> Result<Vec<AliasSource>> {
    let mut all = Vec::new();
    for file in source_files {
        for text in extract_alias_source_snippets(file)? {
            let info = AliasRegistry::from_source_text(&text).ok_or_else(|| {
                miette::miette!(
                    "Internal error: sliced type-alias declaration in '{file}' failed to \
                     re-parse: {text:?}"
                )
            })?;
            all.push(AliasSource { info, text });
        }
    }
    Ok(all)
}

/// Scans every stdlib source file for `Protocol define: ...` declarations,
/// extracting each into a `ProtocolInfo` immediately (BT-3034).
///
/// **Why a live, same-run pre-pass — mirroring [`collect_stdlib_alias_sources`]
/// (BT-2935) exactly:** before this fix, `build_stdlib()`'s `CompileContext`
/// seeded `pre_loaded_aliases` from a live scan but left `pre_loaded_protocols`
/// at its `Vec::default()`, so a stdlib file compiled with a `:: SomeProtocol`
/// type annotation referencing a protocol declared in *another* stdlib file
/// (e.g. `console.bt`/`json.bt`'s `:: Printable` parameters, with `Printable`
/// declared in `printable.bt`) could never see that protocol as resolved —
/// each file is compiled independently, with only this pre-pass's output as
/// its window into the rest of stdlib.
///
/// That degrades *silently*, with no diagnostic at all — not even a
/// warning: `build_stdlib()` never sets `pre_loaded_classes` either, so
/// `has_cross_file_classes` is always `false` for every stdlib compile,
/// which gates off `check_unresolved_classes` entirely (see its call site's
/// doc in `semantic_analysis/mod.rs` — "only check ... when cross-file
/// metadata has been loaded"). So a missing `Printable` registration was
/// never going to surface as an `UnresolvedClass` diagnostic in this
/// pipeline in the first place. The actual damage is one level deeper: the
/// type checker's `check_protocol_argument_conformance` (BT-1928,
/// `type_checker/validation.rs`) short-circuits with
/// `let Some(_protocol) = protocol_registry.get(base_protocol) else { return; }`
/// whenever the named protocol isn't registered — so with `Printable`
/// unregistered, structural protocol-conformance checking on every
/// `Printable`-typed argument anywhere in stdlib was silently skipped
/// (never verifying, never warning), which is exactly the "silently
/// degrade to unresolved" failure mode this issue describes. Nothing short
/// of an explicit `pre_loaded_protocols`/`ProtocolRegistry` inspection
/// (see this function's regression test) makes the gap visible; no build
/// flag, including `--warnings-as-errors`, could ever have caught it.
///
/// Unlike [`collect_stdlib_alias_sources`], this needs no source-text
/// slicing/re-parse round trip and no `package` stamp: `ProtocolInfo` carries
/// no `package` field (protocols have no `internal` modifier at the AST
/// level — see `collect_project_protocol_and_alias_infos`'s doc in
/// `build.rs`, which this mirrors), and nothing downstream persists stdlib's
/// protocols into `generated_builtins.rs` the way aliases are for
/// REPL/workspace consumption. `ProtocolRegistry::extract_protocol_infos`
/// already returns owned, directly-usable `ProtocolInfo` values, so a single
/// per-file lex/parse/extract pass is enough.
///
/// Parse errors on an individual file are non-fatal here — consistent with
/// `build.rs`'s `collect_project_protocol_and_alias_infos`, which the
/// ordinary `beamtalk build` pipeline already uses for this exact purpose:
/// that file simply contributes no protocols to the merged set, and the same
/// parse error is already reported through the normal per-file diagnostics
/// path when that file is compiled on its own.
fn collect_stdlib_protocol_infos(
    source_files: &[Utf8PathBuf],
) -> Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    let mut all = Vec::new();
    for file in source_files {
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        all.extend(
            beamtalk_core::semantic_analysis::protocol_registry::ProtocolRegistry::extract_protocol_infos(
                &module,
            ),
        );
    }
    all
}

/// The `CompilerOptions` every stdlib compile in [`build_stdlib`] runs with.
///
/// BT-2964: `current_package` is `Some("stdlib")`, matching the
/// `package: Some("stdlib")` stamp on [`stdlib_pre_loaded_aliases`]'s entries
/// (and [`generate_class_entry`]'s `ClassInfo`s) and the LSP's
/// `STDLIB_PACKAGE_MARKER` — without it, `None` would make
/// `AliasRegistry::add_pre_loaded` fall into the open-world REPL path instead
/// of enforcing stdlib's own package boundary, and an `internal type` alias
/// would resolve by accident rather than by the same-package rule (ADR 0108).
/// Tests compile against this exact function so the two identities cannot
/// silently drift apart.
fn stdlib_compiler_options(warnings_as_errors: bool) -> beamtalk_core::CompilerOptions {
    beamtalk_core::CompilerOptions {
        stdlib_mode: true,
        allow_primitives: false,
        workspace_mode: false,
        warnings_as_errors,
        current_package: Some("stdlib".into()),
        ..Default::default()
    }
}

/// Extracts the `pre_loaded_aliases` value for
/// [`ClassHierarchyContext`](crate::beam_compiler::ClassHierarchyContext)
/// from [`collect_stdlib_alias_sources`]'s output, stamping every entry's
/// `package` as `"stdlib"` (mirroring [`generate_class_entry`]'s hardcoded
/// `package: Some("stdlib".into())` for generated `ClassInfo`).
fn stdlib_pre_loaded_aliases(
    alias_sources: &[AliasSource],
) -> Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo> {
    alias_sources
        .iter()
        .map(|a| {
            let mut info = a.info.clone();
            info.package = Some("stdlib".into());
            info
        })
        .collect()
}

/// Extracts `alias_sources`' declaration texts sorted by alias *name* (not
/// raw declaration text — `internal type Foo` would otherwise sort under
/// "i", not alongside plain `type` entries), for
/// [`generate_alias_sources_section`], which trusts its caller to have
/// already sorted (see that function's doc).
fn alias_source_texts_sorted_by_name(mut alias_sources: Vec<AliasSource>) -> Vec<String> {
    alias_sources.sort_by(|a, b| a.info.name.cmp(&b.info.name));
    alias_sources.into_iter().map(|a| a.text).collect()
}

/// Extracts every `type Name = ...` declaration in a `.bt` file as verbatim
/// source text (BT-2935), one string per alias.
///
/// Slices each declaration's exact span out of the original source rather
/// than reconstructing it from the parsed `TypeAnnotation` — see
/// `generate_builtins_rs`'s doc for why. A file with no type-alias
/// declarations (the common case) returns an empty `Vec`.
fn extract_alias_source_snippets(path: &Utf8Path) -> Result<Vec<String>> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    Ok(AliasRegistry::extract_alias_infos(&module)
        .iter()
        .filter_map(|info| {
            let start = usize::try_from(info.span.start()).ok()?;
            let end = usize::try_from(info.span.end()).ok()?;
            source.get(start..end).map(str::to_string)
        })
        .collect())
}

/// Check whether a `.bt` file contains only protocol definitions (no classes).
///
/// Protocol-only files (e.g. `printable.bt`) define structural protocols via
/// `Protocol define:` but contain no class definitions. These files still need
/// to be compiled so the protocol gets registered at runtime, but they have no
/// class metadata to extract.
fn is_protocol_only_file(path: &Utf8Path) -> Result<bool> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // If parsing produced errors, the class/protocol lists may be incomplete.
    // Be conservative: treat as a normal class file so extract_class_metadata
    // reports the real error instead of silently misclassifying.
    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        return Ok(false);
    }

    Ok(module.classes.is_empty() && !module.protocols.is_empty())
}

/// Extract class metadata from a `.bt` source file.
///
/// Parses the full class definition to extract the class name, superclass,
/// flags, state declarations, and method signatures. Each stdlib file
/// contains exactly one class definition.
fn extract_class_metadata(path: &Utf8Path, module_name: &str) -> Result<ClassMeta> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let class = module
        .classes
        .first()
        .ok_or_else(|| miette::miette!("No class definition in '{path}'"))?;

    if module.classes.len() > 1 {
        miette::bail!(
            "Expected exactly one class in '{path}', found {}",
            module.classes.len()
        );
    }

    let mut methods: Vec<MethodMeta> = class.methods.iter().map(method_def_to_meta).collect();
    let mut class_methods: Vec<MethodMeta> =
        class.class_methods.iter().map(method_def_to_meta).collect();

    let state = class
        .state
        .iter()
        .map(|s| s.name.name.to_string())
        .collect();

    let state_types = class
        .state
        .iter()
        .filter_map(|s| {
            s.type_annotation
                .as_ref()
                .map(|ty| (s.name.name.to_string(), DeclaredType::from(ty)))
        })
        .collect();

    let state_has_default = class
        .state
        .iter()
        .map(|s| (s.name.name.to_string(), s.default_value.is_some()))
        .collect();

    let class_variables = class
        .class_variables
        .iter()
        .map(|cv| cv.name.name.to_string())
        .collect();

    let class_name = class.name.name.to_string();

    // Synthesize auto-generated methods for Value subclasses with state
    if class.class_kind == beamtalk_core::ast::ClassKind::Value && !class.state.is_empty() {
        synthesize_value_auto_methods(class, &class_name, &mut methods, &mut class_methods);
    }

    // Mark Timer methods that spawn their block argument
    if class_name == "Timer" {
        mark_timer_spawns(&mut class_methods)?;
    }

    // Mark Parallel/Collection methods that spawn their block argument(s) (BT-2974)
    if class_name == "Parallel" {
        mark_parallel_spawns(&mut class_methods)?;
    }
    if class_name == "Collection" {
        mark_parallel_collect_spawns(&mut methods)?;
    }

    let type_params = class
        .type_params
        .iter()
        .map(|tp| tp.name.name.to_string())
        .collect();

    let superclass_type_args = class
        .superclass_type_args
        .iter()
        .map(DeclaredType::from)
        .collect();

    Ok(ClassMeta {
        module_name: module_name.to_string(),
        class_name,
        superclass_name: class.superclass_name().to_string(),
        modifiers: ClassModifiers {
            is_sealed: class.is_sealed,
            is_abstract: class.is_abstract,
            is_typed: class.is_typed,
            is_native: class.backing_module.is_some(),
        },
        class_kind: class.class_kind,
        state,
        state_types,
        state_has_default,
        methods,
        class_methods,
        class_variables,
        type_params,
        superclass_type_args,
        handle_scope: class.handle_scope.as_ref().map(|s| s.name.to_string()),
    })
}

/// Format a single stdlib class metadata entry in ADR 0070 Phase 4 map format.
fn format_stdlib_class_entry(m: &ClassMeta) -> String {
    let type_params = if m.type_params.is_empty() {
        "[]".to_string()
    } else {
        format!(
            "[{}]",
            m.type_params
                .iter()
                .map(|tp| format!("'{tp}'"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    // ADR 0103: `handle_scope` is intentionally omitted from this lightweight
    // `.app.src` registry (like `is_native`, `fields`, and `methods`). The
    // authoritative tier channel is `__beamtalk_meta/0` in the compiled `.beam`
    // (read back by the compiler-port), which does carry it. No stdlib class
    // declares `handleScope:` today, so there is no divergence to reconcile.
    format!(
        "#{{name => '{class}', module => '{module}', parent => '{super}', \
         package => 'stdlib', kind => {kind}, type_params => {type_params}}}",
        module = m.module_name,
        class = m.class_name,
        super = m.superclass_name,
        kind = m.class_kind.as_str(),
        type_params = type_params,
    )
}

/// Format stdlib class metadata entries joined with the given separator.
///
/// Sorted by class name so the generated `.app`/`.app.src` content depends
/// only on *which* classes exist, never on the order the source tree happened
/// to be walked in. Without this, moving a class into a `stdlib/src/`
/// subdirectory reshuffles a checked-in generated file for no semantic reason.
fn format_stdlib_classes_list(class_metadata: &[ClassMeta], separator: &str) -> String {
    sorted_by_class_name(class_metadata)
        .into_iter()
        .map(format_stdlib_class_entry)
        .collect::<Vec<_>>()
        .join(separator)
}

/// Format a sorted, quoted Erlang atom list (`'a', 'b', …`).
///
/// Same layout-independence rationale as [`format_stdlib_classes_list`].
fn format_sorted_atom_list(names: &[String]) -> String {
    let mut sorted = names.to_vec();
    sorted.sort();
    sorted
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Generate the `beamtalk_stdlib.app` file in the ebin directory.
///
/// Lists all modules and embeds class hierarchy metadata in the `env` section.
/// The metadata is used by `beamtalk_stdlib` to load modules in dependency order.
///
/// **`type_aliases` env key (ADR 0108 Phase 8, BT-2903/BT-2938):** mirrors
/// [`super::app_file::generate_app_file`] (the real `beamtalk build`
/// pipeline), which emits `{type_aliases, [...]}` via
/// [`super::build::build_alias_metadata`] + [`app_file::format_type_aliases_entry`].
/// Without this key, `application:get_env(beamtalk_stdlib, type_aliases)`
/// returns `undefined` forever, so neither `browse-type-aliases`
/// (`beamtalk_repl_ops_browse.erl`) nor the REPL/workspace session's
/// alias-seeding path (`beamtalk_repl_state:new/3`, BT-2938) can ever learn
/// stdlib's own `type Name = ...` declarations.
fn generate_app_file(
    ebin_dir: &Utf8Path,
    source_files: &[Utf8PathBuf],
    class_metadata: &[ClassMeta],
    protocol_modules: &[String],
    alias_metadata: &[app_file::AliasMetadata],
) -> Result<()> {
    let module_names: Vec<String> = source_files
        .iter()
        .map(|f| module_name_from_path(f))
        .collect::<Result<_>>()?;

    let modules_list = format_sorted_atom_list(&module_names);

    // ADR 0070 Phase 4: Generate extended class hierarchy entries for env
    let classes_list = format_stdlib_classes_list(class_metadata, ",\n                    ");

    // BT-1766: Protocol-only modules need to be loaded separately during stdlib init
    let protocol_modules_list = format_sorted_atom_list(protocol_modules);

    // BT-2938: same `{type_aliases, [...]}` entry the ordinary `beamtalk
    // build` pipeline emits (`app_file::format_type_aliases_entry`) — empty
    // string (no key at all) when stdlib declares no aliases.
    let type_aliases_entry = app_file::format_type_aliases_entry(alias_metadata);

    let version = env!("BEAMTALK_VERSION");
    let app_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, \"{version}\"}},\n\
         \x20   {{modules, [{modules_list}]}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, crypto, beamtalk_runtime]}},\n\
         \x20   {{env, [\n\
         \x20       {{classes, [{classes_list}]}},\n\
         \x20       {{protocol_modules, [{protocol_modules_list}]}}{type_aliases_entry}\n\
         \x20   ]}}\n\
         ]}}.\n"
    );

    let app_file = ebin_dir.join("beamtalk_stdlib.app");
    fs::write(&app_file, app_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{app_file}'"))?;

    debug!("Generated {}", app_file);
    Ok(())
}

/// Generate/update the `.app.src` file so rebar3 picks up the classes metadata.
///
/// The `.app.src` uses `{modules, []}` (rebar3 auto-fills modules) but embeds
/// the `{classes, [...]}` and (BT-2938) `{type_aliases, [...]}` envs for the
/// runtime to read via `application:get_env`.
fn generate_app_src_file(
    src_dir: &Utf8Path,
    class_metadata: &[ClassMeta],
    protocol_modules: &[String],
    alias_metadata: &[app_file::AliasMetadata],
) -> Result<()> {
    // ADR 0070 Phase 4: Generate extended class hierarchy entries for env
    let classes_list = format_stdlib_classes_list(class_metadata, ",\n            ");

    // BT-1766: Protocol-only modules need to be loaded separately during stdlib init
    let protocol_modules_list = format_sorted_atom_list(protocol_modules);

    // BT-2938: see `generate_app_file`'s doc.
    let type_aliases_entry = app_file::format_type_aliases_entry(alias_metadata);

    let app_src_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, {{cmd, \"escript ../../../scripts/version.escript\"}}}},\n\
         \x20   {{modules, []}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, crypto, beamtalk_runtime]}},\n\
         \x20   {{env, [\n\
         \x20       {{classes, [\n\
         \x20           {classes_list}\n\
         \x20       ]}},\n\
         \x20       {{protocol_modules, [{protocol_modules_list}]}}{type_aliases_entry}\n\
         \x20   ]}}\n\
         ]}}.\n"
    );

    let app_src_file = src_dir.join("beamtalk_stdlib.app.src");
    fs::write(&app_src_file, &app_src_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{app_src_file}'"))?;

    debug!("Generated {}", app_src_file);
    Ok(())
}

/// Default path for generated builtins file (relative to project root).
const GENERATED_BUILTINS_PATH: &str =
    "crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs";

/// Default path for the generated Erlang builtin-class-list header (BT-3085).
const GENERATED_BUILTINS_HRL_PATH: &str =
    "runtime/apps/beamtalk_runtime/include/beamtalk_generated_builtins.hrl";

/// Write generated `content` to `dest`, but only if it differs from what's
/// already there — skips touching the file (and downstream recompilation)
/// when a stdlib build produces byte-identical output.
///
/// Shared by `generate_builtins_rs` and `generate_erlang_builtins_hrl`, which
/// otherwise each hand-rolled this exact read-compare-write dance (BT-3357).
/// Taking `dest` as a parameter — rather than each caller reading its own
/// hardcoded `GENERATED_*_PATH` constant internally — is also what makes this
/// unit-testable: real call sites still pass the hardcoded constants, but
/// tests can point `dest` at a temp file instead of risking a write into the
/// real checked-in generated files (see CLAUDE.md's "Generated files" rule).
fn write_generated_file_if_changed(dest: &Utf8Path, content: &str) -> Result<()> {
    let needs_write = match fs::read_to_string(dest) {
        Ok(existing) => existing != content,
        Err(_) => true,
    };

    if needs_write {
        fs::write(dest, content)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to write '{dest}'"))?;
        debug!("Generated {}", dest);
    } else {
        debug!("Generated file unchanged, skipping write: {}", dest);
    }
    Ok(())
}

/// Generate the `generated_builtins.rs` file from parsed stdlib class and
/// type-alias metadata.
///
/// This produces a Rust source file that defines `generated_builtin_classes()`,
/// `is_generated_builtin_class()`, and (BT-2935) `generated_stdlib_alias_sources()`,
/// replacing the hand-written tables in `builtins.rs`.
///
/// **BT-2935 design decision — how a stdlib `type Name = ...` alias is
/// persisted here:** `AliasInfo::annotation` is a full, recursive
/// `TypeAnnotation` AST (unions, generics, `\`/`&`, nested `Box`es, `Span`s
/// on every node) — nothing like `ClassInfo`/`MethodInfo`'s flat
/// `Option<EcoString>` return/param-type strings, which this generator
/// already emits as simple quoted-string literals (see
/// `generate_method_list`). Three representations were considered:
///
/// 1. **Add `serde` derives to `TypeAnnotation`** (and transitively
///    `Identifier`) and embed a serialized (e.g. JSON) blob. Rejected: both
///    types are deep in the shared AST used by parser, semantic analysis,
///    codegen, and the LSP — adding derives there is a real, ongoing
///    maintenance surface (every future `TypeAnnotation` variant needs a
///    serde-compatible shape) for a feature only this one generator needs.
/// 2. **Hand-write a recursive Rust-literal-construction emitter** for
///    `TypeAnnotation` (mirroring `generate_superclass_type_args`'s much
///    simpler two-variant `SuperclassTypeArg` emitter). Rejected: it would
///    duplicate the parser's own understanding of the AST's shape in a
///    second, hand-maintained place that silently falls out of sync (no
///    compile error) whenever `TypeAnnotation` gains a variant — the emitter
///    would need a matching arm added by hand, and forgetting one would
///    silently miscompile or panic on a real alias RHS instead of failing to
///    build.
/// 3. **Store each alias's verbatim declaration source text** (`"type
///    RestartStrategy = #temporary | #transient | #permanent"`, sliced
///    directly from the original `.bt` file via
///    [`extract_alias_source_snippets`]) and re-parse it back into an
///    `AliasInfo` at load time via
///    [`beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::from_source_text`].
///    **Chosen.** Zero new derives, zero hand-maintained AST mirror — every
///    consumer goes through the same lexer/parser the rest of the compiler
///    already trusts, so a new `TypeAnnotation` variant just works the
///    moment the parser supports it. The cost is a handful of cheap
///    single-line re-lexes at `ClassHierarchy::generated_stdlib_aliases()`
///    call time (today: 5 stdlib aliases) — negligible next to compiling
///    every stdlib file in the same pipeline. This mirrors
///    `MethodInfo::return_type`'s existing stringly-typed precedent in this
///    very generator, just keeping the *whole* declaration instead of a bare
///    type name (a bare name isn't enough to reconstruct a union/generic
///    RHS).
fn generate_builtins_rs(class_metadata: &[ClassMeta], alias_sources: &[String]) -> Result<()> {
    let mut code = String::new();

    code.push_str(
        "// AUTO-GENERATED from lib/*.bt by `beamtalk build-stdlib` — do not edit manually.\n\
         // Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         //! Generated built-in class definitions derived from `stdlib/src/*.bt` sources.\n\
         //!\n\
         //! **Do not edit this file.** Modify the `.bt` source in `stdlib/src/` and\n\
         //! run `just build` (or `beamtalk build-stdlib`) to regenerate.\n\
         \n\
         use super::super::{ClassInfo, DeclaredType, MethodInfo, SuperclassTypeArg};\n\
         use crate::ast::MethodKind;\n\
         use ecow::EcoString;\n\
         use std::collections::HashMap;\n\
         \n",
    );

    // Generate is_generated_builtin_class()
    code.push_str(
        "/// Returns true if the given class name is a stdlib built-in class.\n\
         ///\n\
         /// Auto-generated from `lib/*.bt` file names.\n\
         #[allow(clippy::too_many_lines)] // auto-generated: one match arm per stdlib class\n\
         pub(super) fn is_generated_builtin_class(name: &str) -> bool {\n\
         \x20   matches!(\n\
         \x20       name,\n",
    );

    let sorted_meta = sorted_by_class_name(class_metadata);

    for (i, meta) in sorted_meta.iter().enumerate() {
        if i == 0 {
            let _ = write!(code, "        \"{}\"", meta.class_name);
        } else {
            let _ = write!(code, "\n            | \"{}\"", meta.class_name);
        }
    }
    code.push_str("\n    )\n}\n\n");

    // Generate generated_builtin_classes()
    code.push_str(
        "/// Returns all stdlib built-in class definitions.\n\
         ///\n\
         /// Auto-generated from parsed `lib/*.bt` ASTs.\n\
         #[allow(clippy::too_many_lines)] // auto-generated from 32 stdlib classes\n\
         #[rustfmt::skip] // preserve compact generated layout\n\
         pub(super) fn generated_builtin_classes() -> HashMap<EcoString, ClassInfo> {\n\
         \x20   let mut classes = HashMap::new();\n\n",
    );

    for meta in &sorted_meta {
        generate_class_entry(&mut code, meta);
    }

    code.push_str("    classes\n}\n");

    // Generate generated_stdlib_alias_sources() (BT-2935) — see this
    // function's own doc for the source-text-persisted-and-reparsed design.
    generate_alias_sources_section(&mut code, alias_sources);

    let dest = Utf8PathBuf::from(GENERATED_BUILTINS_PATH);
    write_generated_file_if_changed(&dest, &code)
}

/// Generate the `beamtalk_generated_builtins.hrl` Erlang header from parsed
/// stdlib class metadata (BT-3085).
///
/// Defines `?BEAMTALK_GENERATED_BUILTIN_CLASSES`, the Erlang-side twin of
/// `generated_builtins.rs`'s `is_generated_builtin_class` match arms — both
/// are emitted from the exact same `class_metadata` list computed earlier in
/// `build_stdlib()`, so they cannot drift the way the old hand-typed
/// `beamtalk_class_metadata:all_builtins/0` list did. `beamtalk_class_metadata.erl`
/// includes this header and prepends the one runtime-only exception
/// (`'Future'`, no `stdlib/src/Future.bt` source — mirrors `builtins.rs`'s
/// `is_builtin_class` on the Rust side).
fn generate_erlang_builtins_hrl(class_metadata: &[ClassMeta]) -> Result<()> {
    let sorted_meta = sorted_by_class_name(class_metadata);

    let mut code = String::new();
    code.push_str(
        "%% AUTO-GENERATED from stdlib/src/*.bt by `beamtalk build-stdlib` — do not edit manually.\n\
         %% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         \n\
         %% Generated built-in class list derived from `stdlib/src/*.bt` sources.\n\
         %%\n\
         %% Do not edit this file. Modify the `.bt` source in `stdlib/src/` and run\n\
         %% `just build` (or `beamtalk build-stdlib`) to regenerate.\n\
         %%\n\
         %% Mirrors `is_generated_builtin_class` in\n\
         %% crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs\n\
         %% (BT-3085) — both are generated from the same stdlib class-metadata pass\n\
         %% in build_stdlib.rs, so they cannot drift out of sync with each other.\n\
         \n\
         -ifndef(BEAMTALK_GENERATED_BUILTINS_HRL).\n\
         -define(BEAMTALK_GENERATED_BUILTINS_HRL, true).\n\
         \n\
         -define(BEAMTALK_GENERATED_BUILTIN_CLASSES, [\n",
    );

    for (i, meta) in sorted_meta.iter().enumerate() {
        let sep = if i + 1 == sorted_meta.len() { "" } else { "," };
        let _ = writeln!(code, "    '{}'{sep}", meta.class_name);
    }

    code.push_str("]).\n\n-endif.\n");

    let dest = Utf8PathBuf::from(GENERATED_BUILTINS_HRL_PATH);
    write_generated_file_if_changed(&dest, &code)
}

/// Generate a single class entry for `generated_builtin_classes()`.
#[allow(clippy::too_many_lines)] // one contiguous struct-literal emission
fn generate_class_entry(code: &mut String, meta: &ClassMeta) {
    let superclass = if meta.superclass_name == "none" {
        "None".to_string()
    } else {
        format!("Some(\"{}\".into())", meta.superclass_name)
    };

    let _ = write!(
        code,
        "    classes.insert(\n\
         \x20       \"{name}\".into(),\n\
         \x20       ClassInfo {{\n\
         \x20           name: \"{name}\".into(),\n\
         \x20           superclass: {superclass},\n\
         \x20           is_sealed: {sealed},\n\
         \x20           is_abstract: {abstract_},\n\
         \x20           is_typed: {typed},\n\
         \x20           is_internal: false,\n\
         \x20           package: Some(\"stdlib\".into()),\n\
         \x20           is_value: {is_value},\n\
         \x20           is_native: {is_native},\n\
         \x20           handle_scope: {handle_scope},\n\
         \x20           surface_incomplete: false,\n",
        name = meta.class_name,
        sealed = meta.modifiers.is_sealed,
        abstract_ = meta.modifiers.is_abstract,
        typed = meta.modifiers.is_typed,
        is_value = meta.superclass_name == "Value",
        is_native = meta.modifiers.is_native,
        // ADR 0103: emit the declared handle scope so it survives regeneration.
        handle_scope = meta
            .handle_scope
            .as_deref()
            .map_or_else(|| "None".to_string(), |s| format!("Some(\"{s}\".into())")),
    );

    // State
    if meta.state.is_empty() {
        code.push_str("            state: vec![],\n");
    } else {
        code.push_str("            state: vec![");
        for (i, s) in meta.state.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "\"{s}\".into()");
        }
        code.push_str("],\n");
    }

    // State types
    if meta.state_types.is_empty() {
        code.push_str("            state_types: HashMap::new(),\n");
    } else {
        code.push_str("            state_types: HashMap::from([");
        for (i, (field, ty)) in meta.state_types.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(
                code,
                "(\"{field}\".into(), {})",
                declared_type_to_rust_expr(ty)
            );
        }
        code.push_str("]),\n");
    }

    // BT-1976: state_has_default — used by gen_server post-initialize
    // validation to identify typed-no-default inherited fields without the AST.
    if meta.state_has_default.is_empty() {
        code.push_str("            state_has_default: HashMap::new(),\n");
    } else {
        code.push_str("            state_has_default: HashMap::from([");
        for (i, (field, has_default)) in meta.state_has_default.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "(\"{field}\".into(), {has_default})");
        }
        code.push_str("]),\n");
    }

    // Instance methods
    generate_method_list(code, "methods", &meta.methods, &meta.class_name);
    // Class methods
    generate_method_list(code, "class_methods", &meta.class_methods, &meta.class_name);

    // Class variables
    if meta.class_variables.is_empty() {
        code.push_str("            class_variables: vec![],\n");
    } else {
        code.push_str("            class_variables: vec![");
        for (i, cv) in meta.class_variables.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "\"{cv}\".into()");
        }
        code.push_str("],\n");
    }

    // Type parameters
    if meta.type_params.is_empty() {
        code.push_str("            type_params: vec![],\n");
        code.push_str("            type_param_bounds: vec![],\n");
    } else {
        code.push_str("            type_params: vec![");
        for (i, tp) in meta.type_params.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "\"{tp}\".into()");
        }
        code.push_str("],\n");
        // Type parameter bounds (ADR 0068 Phase 2d) — currently all None
        // until stdlib classes declare bounded type params
        code.push_str("            type_param_bounds: vec![");
        for (i, _) in meta.type_params.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            code.push_str("None");
        }
        code.push_str("],\n");
    }

    generate_superclass_type_args(code, &meta.superclass_type_args, &meta.type_params);

    code.push_str("        },\n    );\n\n");
}

/// Emit the `generated_stdlib_alias_sources()` function body (BT-2935): one
/// `&'static str` literal per stdlib type-alias declaration, in the order
/// given by `alias_sources`.
///
/// Unlike [`generate_class_entry`]'s `sorted_meta` (sorted right before
/// calling this file's class-entry generator), sorting alias entries by
/// *name* requires the caller's already-reconstructed `AliasInfo` — plain
/// declaration text alone would sort `internal type Foo = ...` under "i",
/// not alongside `type` entries — so the caller (`build_stdlib()`) is
/// responsible for passing `alias_sources` pre-sorted by name; this function
/// only formats whatever order it's given.
///
/// See `generate_builtins_rs`'s doc for why each alias is persisted as its
/// verbatim declaration source text rather than a literal-Rust
/// `TypeAnnotation` construction or a `serde` blob.
fn generate_alias_sources_section(code: &mut String, alias_sources: &[String]) {
    code.push_str(
        "\n/// Returns the verbatim `type Name = ...` declaration source text for every\n\
         /// stdlib type alias (BT-2935), sorted by alias name.\n\
         ///\n\
         /// Auto-generated from `stdlib/src/*.bt` ASTs. See\n\
         /// `crates/beamtalk-cli/src/commands/build_stdlib.rs`'s `generate_builtins_rs`\n\
         /// doc for why source text (re-parsed by\n\
         /// `AliasRegistry::from_source_text`), not a literal-Rust `TypeAnnotation`\n\
         /// construction or a `serde` blob, was chosen to represent each alias here.\n\
         pub(super) fn generated_stdlib_alias_sources() -> Vec<&'static str> {\n\
         \x20   vec![\n",
    );

    for text in alias_sources {
        let escaped = text.replace('\\', "\\\\").replace('"', "\\\"");
        let _ = writeln!(code, "        \"{escaped}\",");
    }
    code.push_str("    ]\n}\n");
}

/// Emit `superclass_type_args: vec![...]` for a subclass's parent binding.
///
/// Each entry is either a [`SuperclassTypeArg::ParamRef`] when the argument
/// names one of the subclass's own type parameters (e.g. `Collection(E)
/// subclass: Array(E)`) or a [`SuperclassTypeArg::Concrete`] otherwise (e.g.
/// `Collection(Integer) subclass: IntArray`).
fn generate_superclass_type_args(code: &mut String, args: &[DeclaredType], type_params: &[String]) {
    if args.is_empty() {
        code.push_str("            superclass_type_args: vec![],\n");
        return;
    }
    code.push_str("            superclass_type_args: vec![");
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            code.push_str(", ");
        }
        let param_ref_idx = match arg {
            DeclaredType::Simple(name) => type_params.iter().position(|p| p == name.as_str()),
            _ => None,
        };
        if let Some(idx) = param_ref_idx {
            let _ = write!(code, "SuperclassTypeArg::ParamRef {{ param_index: {idx} }}");
        } else {
            let _ = write!(
                code,
                "SuperclassTypeArg::Concrete {{ declared: {} }}",
                declared_type_to_rust_expr(arg)
            );
        }
    }
    code.push_str("],\n");
}

/// Serialise a [`DeclaredType`] into a Rust source expression that
/// reconstructs it verbatim — the structured counterpart to stringifying via
/// `Display` and re-parsing on read. Mirrors every `DeclaredType` variant
/// (BT-3076 stage 3b.2): `Simple`/`Singleton`/`Generic`/`Union` go through
/// the compact constructors added for this purpose
/// ([`DeclaredType::simple`], etc.); the remaining variants (`FalseOr`,
/// `Difference`, `Intersection`, `SelfType`, `SelfClass`, `ClassOf`) are rare
/// in stdlib signatures but still emitted structurally rather than silently
/// dropped, so a stdlib author who *does* write one of them gets a correct
/// generated artifact rather than a degraded one.
fn declared_type_to_rust_expr(dt: &DeclaredType) -> String {
    match dt {
        DeclaredType::Simple(name) => format!("DeclaredType::simple({})", rust_str_lit(name)),
        DeclaredType::Singleton(name) => {
            format!("DeclaredType::singleton({})", rust_str_lit(name))
        }
        DeclaredType::Union(members) => {
            let parts: Vec<String> = members.iter().map(declared_type_to_rust_expr).collect();
            format!("DeclaredType::union(vec![{}])", parts.join(", "))
        }
        DeclaredType::Generic { base, parameters } => {
            let parts: Vec<String> = parameters.iter().map(declared_type_to_rust_expr).collect();
            format!(
                "DeclaredType::generic({}, vec![{}])",
                rust_str_lit(base),
                parts.join(", ")
            )
        }
        DeclaredType::FalseOr(inner) => {
            format!(
                "DeclaredType::FalseOr(Box::new({}))",
                declared_type_to_rust_expr(inner)
            )
        }
        DeclaredType::Difference { base, excluded } => format!(
            "DeclaredType::Difference {{ base: Box::new({}), excluded: Box::new({}) }}",
            declared_type_to_rust_expr(base),
            declared_type_to_rust_expr(excluded)
        ),
        DeclaredType::Intersection { left, right } => format!(
            "DeclaredType::Intersection {{ left: Box::new({}), right: Box::new({}) }}",
            declared_type_to_rust_expr(left),
            declared_type_to_rust_expr(right)
        ),
        DeclaredType::SelfType => "DeclaredType::SelfType".to_string(),
        DeclaredType::SelfClass => "DeclaredType::SelfClass".to_string(),
        DeclaredType::ClassOf(name) => {
            format!("DeclaredType::ClassOf({})", rust_str_lit_into(name))
        }
    }
}

/// Escaped Rust string literal (no `.into()` suffix) — for use as an
/// argument to a `DeclaredType` compact constructor (`impl Into<EcoString>`
/// parameters accept a bare `&str` literal directly).
fn rust_str_lit(s: &str) -> String {
    let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
    format!("\"{escaped}\"")
}

/// Escaped Rust string literal with a trailing `.into()` — for direct
/// enum-variant construction where the field type is a bare `EcoString`
/// (no `impl Into` coercion available).
fn rust_str_lit_into(s: &str) -> String {
    format!("{}.into()", rust_str_lit(s))
}

/// Generate a method list field (`methods` or `class_methods`).
fn generate_method_list(
    code: &mut String,
    field_name: &str,
    methods: &[MethodMeta],
    class_name: &str,
) {
    if methods.is_empty() {
        let _ = writeln!(code, "            {field_name}: vec![],");
        return;
    }

    let _ = writeln!(code, "            {field_name}: vec![");
    for m in methods {
        let kind = m.kind.to_rust_expr();
        // Escape backslashes and quotes in selector for Rust string literals
        let selector = m.selector.replace('\\', "\\\\").replace('"', "\\\"");
        let return_type_expr = match &m.return_type {
            Some(t) => format!("Some({})", declared_type_to_rust_expr(t)),
            None => "None".to_string(),
        };
        let param_types_expr = if m.param_types.is_empty() {
            "vec![]".to_string()
        } else {
            let parts: Vec<_> = m
                .param_types
                .iter()
                .map(|p| match p {
                    Some(t) => format!("Some({})", declared_type_to_rust_expr(t)),
                    None => "None".to_string(),
                })
                .collect();
            format!("vec![{}]", parts.join(", "))
        };
        let doc_expr = match &m.doc {
            Some(doc) => {
                let escaped = doc
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\r', "\\r")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t");
                format!("Some(\"{escaped}\".into())")
            }
            None => "None".to_string(),
        };
        let _ = writeln!(
            code,
            "                MethodInfo {{ selector: \"{selector}\".into(), arity: {arity}, \
             kind: {kind}, defined_in: \"{class}\".into(), is_sealed: {sealed}, \
             is_internal: {internal}, spawns_block: {spawns_block}, \
             return_type: {return_type_expr}, param_types: {param_types_expr}, doc: {doc_expr} }},",
            arity = m.arity,
            class = class_name,
            sealed = m.is_sealed,
            internal = m.is_internal,
            spawns_block = m.spawns_block,
        );
    }
    code.push_str("            ],\n");
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::time::Duration;
    use tempfile::TempDir;

    fn temp_utf8_dir() -> (TempDir, Utf8PathBuf) {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        (temp, dir)
    }

    /// BT-2935: End-to-end regression fixture for stdlib's own cross-file
    /// type-alias resolution, proving the full round trip
    /// `build_stdlib.rs` relies on: `collect_stdlib_alias_sources` (a live,
    /// same-run pre-pass — *not* a seed from a previously-persisted
    /// `generated_builtins.rs` snapshot, see that function's doc for why)
    /// → `ClassHierarchyContext::pre_loaded_aliases` → cross-file resolution
    /// during a real compile, in the very same run the alias was first
    /// declared in. Mirrors the shape of `build.rs`'s
    /// `test_cross_file_alias_resolution_no_false_type_mismatch` (BT-2928),
    /// adapted to `build_stdlib`'s own manifest-less compile path
    /// (`compile_source_with_bindings`, no package/`build_class_module_index`).
    #[test]
    fn test_cross_file_alias_resolution_seeds_pre_loaded_aliases() {
        let (_temp, lib_dir) = temp_utf8_dir();

        // File 1: declares the alias and a class with a method that returns
        // it. `A` is a `Value` subclass so `A new` is instantiable (`Object`
        // classes are abstract and not directly instantiable).
        fs::write(
            lib_dir.join("AliasFixtureA.bt"),
            "type Direction = #north | #south | #east | #west\n\
             Value subclass: AliasFixtureA\n  heading -> Direction => #north\n",
        )
        .unwrap();

        // File 2: consumes A's alias-typed return value as an argument to a
        // parameter typed with the spelled-out equivalent union — the exact
        // shape of the real `RestartStrategy`/`Timeout` stdlib bug this issue
        // fixes (BT-2923).
        fs::write(
            lib_dir.join("AliasFixtureB.bt"),
            "Object subclass: AliasFixtureB\n  \
             useDirection: d :: #north | #south | #east | #west => d\n  \
             test => self useDirection: AliasFixtureA new heading\n",
        )
        .unwrap();

        let file_a = lib_dir.join("AliasFixtureA.bt");
        let file_b = lib_dir.join("AliasFixtureB.bt");

        // Mirrors build_stdlib()'s own per-file class extraction.
        let source_a = fs::read_to_string(&file_a).unwrap();
        let tokens_a = beamtalk_core::source_analysis::lex_with_eof(&source_a);
        let (module_a, _diags) = beamtalk_core::source_analysis::parse(tokens_a);
        let pre_loaded_classes =
            beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::extract_class_infos(
                &module_a,
            );
        assert_eq!(pre_loaded_classes.len(), 1, "expected one class in file A");

        // The real function under test, called the way `build_stdlib()`
        // actually calls it — over *all* source files in one pass, before
        // any compile happens — proving this is a live scan (file A's alias
        // is seeded immediately, in the same call, with no dependency on any
        // prior "generated" snapshot existing).
        let alias_sources =
            collect_stdlib_alias_sources(&[file_a.clone(), file_b.clone()]).unwrap();
        assert_eq!(
            alias_sources.len(),
            1,
            "expected exactly one alias declaration, from file A (file B declares none)"
        );
        assert!(alias_sources[0].text.starts_with("type Direction ="));
        assert_eq!(alias_sources[0].info.name.as_str(), "Direction");

        let pre_loaded_aliases = stdlib_pre_loaded_aliases(&alias_sources);
        assert_eq!(pre_loaded_aliases.len(), 1);
        assert_eq!(pre_loaded_aliases[0].name.as_str(), "Direction");

        let options = stdlib_compiler_options(false);
        let bindings =
            beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new();

        // Compile B with cross-file class info AND cross-file alias info —
        // should produce no "Argument ... expects" type-mismatch warning.
        let core_file = lib_dir.join("b.core");
        let diagnostics = compile_source_with_bindings(
            &file_b,
            "bt@stdlib@alias_fixture_b",
            &core_file,
            &options,
            &bindings,
            &CompileContext {
                hierarchy: ClassHierarchyContext {
                    pre_loaded_classes: pre_loaded_classes.clone(),
                    pre_loaded_aliases: pre_loaded_aliases.clone(),
                    ..ClassHierarchyContext::default()
                },
                ..CompileContext::default()
            },
            None,
        )
        .expect("cross-file alias-typed argument should compile without errors");

        assert!(
            diagnostics.iter().all(|d| !d.message.contains("Argument")),
            "expected no false argument-type-mismatch diagnostic once cross-file \
             aliases are seeded, got: {diagnostics:?}"
        );

        // Negative control: WITHOUT pre_loaded_aliases (but still with
        // pre_loaded_classes), the same compile reproduces the pre-BT-2935
        // false positive — proving this test actually exercises the fix
        // rather than a scenario that never warned.
        let core_file_unfixed = lib_dir.join("b_unfixed.core");
        let diagnostics_unfixed = compile_source_with_bindings(
            &file_b,
            "bt@stdlib@alias_fixture_b_unfixed",
            &core_file_unfixed,
            &options,
            &bindings,
            &CompileContext {
                hierarchy: ClassHierarchyContext {
                    pre_loaded_classes,
                    // No pre_loaded_aliases — reproduces the pre-fix gap.
                    ..ClassHierarchyContext::default()
                },
                ..CompileContext::default()
            },
            None,
        )
        .expect("compile should still succeed even with the false-positive warning");

        assert!(
            diagnostics_unfixed
                .iter()
                .any(|d| d.message.contains("Argument")),
            "expected the negative control (no pre_loaded_aliases) to reproduce \
             the pre-BT-2935 false positive, got: {diagnostics_unfixed:?}"
        );
    }

    /// BT-2964: an `internal type` stdlib alias must resolve cross-file
    /// *within* stdlib. `stdlib_pre_loaded_aliases` stamps every entry
    /// `package: Some("stdlib")` and `build_stdlib()`'s `CompilerOptions`
    /// set `current_package: Some("stdlib")` to match —
    /// `AliasRegistry::add_pre_loaded`'s seeding-boundary exclusion
    /// (ADR 0108) drops an `internal` entry whose `package` differs from
    /// `current_package`, so a mismatch between the two identities would
    /// silently drop the alias and surface as a confusing unresolved-type
    /// diagnostic in the consuming file.
    #[test]
    fn test_internal_alias_resolves_cross_file_within_stdlib() {
        let (_temp, lib_dir) = temp_utf8_dir();

        // File A declares a package-private alias.
        let file_a = lib_dir.join("InternalAliasFixtureA.bt");
        fs::write(
            &file_a,
            "internal type Direction = #north | #south | #east | #west\n\
             Object subclass: InternalAliasFixtureA\n  noop => nil\n",
        )
        .unwrap();

        // File B consumes it in an `internal` method signature — the one
        // position ADR 0108 permits an internal alias outside its declaring
        // file (a public signature would be an E0402 leak).
        let file_b = lib_dir.join("InternalAliasFixtureB.bt");
        fs::write(
            &file_b,
            "Object subclass: InternalAliasFixtureB\n  \
             internal go: d :: Direction -> Symbol => d\n",
        )
        .unwrap();

        let alias_sources =
            collect_stdlib_alias_sources(&[file_a.clone(), file_b.clone()]).unwrap();
        assert_eq!(alias_sources.len(), 1, "expected file A's alias only");
        assert!(alias_sources[0].info.is_internal);

        // The real stamping function under test.
        let pre_loaded_aliases = stdlib_pre_loaded_aliases(&alias_sources);
        assert_eq!(pre_loaded_aliases[0].package.as_deref(), Some("stdlib"));

        // BT-2965: `build.rs`'s `package_identity` names the *other* stdlib
        // compile path — manifest-less `beamtalk build --stdlib-mode <dir>`,
        // what `just dialyzer-specs` runs — with `STDLIB_PACKAGE_MARKER`. The
        // literal hardcoded here and that constant must stay the same string,
        // or the two paths would disagree about stdlib's package identity and
        // only one of them would seed `internal` aliases.
        assert_eq!(
            pre_loaded_aliases[0].package.as_deref(),
            Some(beamtalk_language_service::STDLIB_PACKAGE_MARKER),
            "stdlib_pre_loaded_aliases' stamp must match STDLIB_PACKAGE_MARKER"
        );
        assert_eq!(
            stdlib_compiler_options(false).current_package.as_deref(),
            Some(beamtalk_language_service::STDLIB_PACKAGE_MARKER),
            "stdlib_compiler_options' current_package must match STDLIB_PACKAGE_MARKER"
        );

        // The exact options `build_stdlib()` runs with — crucially with
        // `current_package: Some("stdlib")` matching the stamp above.
        let options = stdlib_compiler_options(false);
        let bindings =
            beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new();

        let core_file = lib_dir.join("internal_b.core");
        let diagnostics = compile_source_with_bindings(
            &file_b,
            "bt@stdlib@internal_alias_fixture_b",
            &core_file,
            &options,
            &bindings,
            &CompileContext {
                hierarchy: ClassHierarchyContext {
                    pre_loaded_aliases: pre_loaded_aliases.clone(),
                    ..ClassHierarchyContext::default()
                },
                ..CompileContext::default()
            },
            None,
        )
        .expect("internal stdlib alias should resolve cross-file within stdlib");
        assert!(
            diagnostics.iter().all(|d| !d.message.contains("Direction")),
            "expected no diagnostic about `Direction`, got: {diagnostics:?}"
        );

        // Negative control at the analysis level: a mismatched
        // `current_package` makes the seeding-boundary exclusion drop the
        // internal alias — proving the compile above passes because the two
        // "stdlib" identities line up, not because the boundary is
        // unenforced.
        let source_b = fs::read_to_string(&file_b).unwrap();
        let tokens_b = beamtalk_core::source_analysis::lex_with_eof(&source_b);
        let (module_b, _diags) = beamtalk_core::source_analysis::parse(tokens_b);
        let extensions = beamtalk_core::compilation::extension_index::ExtensionIndex::default();

        let mismatched = beamtalk_core::CompilerOptions {
            current_package: Some("not_stdlib".into()),
            ..options.clone()
        };
        let result = beamtalk_core::semantic_analysis::analyse_full(
            &module_b,
            beamtalk_core::semantic_analysis::AnalysisContext::default()
                .with_options(&mismatched)
                .with_pre_loaded_aliases(pre_loaded_aliases.clone())
                .with_cross_file_extensions(&extensions),
        );
        assert!(
            !result.alias_registry.has_alias("Direction"),
            "a mismatched current_package must drop the internal stdlib alias \
             at the seeding boundary"
        );

        let result = beamtalk_core::semantic_analysis::analyse_full(
            &module_b,
            beamtalk_core::semantic_analysis::AnalysisContext::default()
                .with_options(&options)
                .with_pre_loaded_aliases(pre_loaded_aliases)
                .with_cross_file_extensions(&extensions),
        );
        assert!(
            result.alias_registry.has_alias("Direction"),
            "the matching \"stdlib\" current_package must seed the internal alias"
        );
    }

    /// BT-3034: End-to-end regression fixture for stdlib's own cross-file
    /// protocol resolution, mirroring
    /// `test_internal_alias_resolves_cross_file_within_stdlib`/
    /// `test_cross_file_alias_resolution_seeds_pre_loaded_aliases` above but
    /// for `pre_loaded_protocols` instead of `pre_loaded_aliases`. Proves the
    /// full round trip `build_stdlib()` relies on:
    /// `collect_stdlib_protocol_infos` (a live, same-run pre-pass — same
    /// rationale as `collect_stdlib_alias_sources`) →
    /// `ClassHierarchyContext::pre_loaded_protocols` → the protocol getting
    /// registered into file B's own `ProtocolRegistry` during semantic
    /// analysis, in the very same run the protocol was first declared in.
    ///
    /// This is the exact shape of the real stdlib bug: `console.bt`/`json.bt`
    /// reference `Printable` (declared in `printable.bt`) in a `::
    /// Printable` parameter annotation — a different file entirely, compiled
    /// independently. Before this fix, `ProtocolRegistry::has_protocol` for a
    /// cross-file protocol name was always `false` while compiling any other
    /// stdlib file, which — per `check_protocol_argument_conformance`'s early
    /// return when `protocol_registry.get(name)` is `None` — makes structural
    /// protocol-conformance checking on `Printable`-typed arguments silently
    /// a no-op *everywhere* in stdlib, with no diagnostic at all (not even a
    /// warning): the exact "silently degrade to unresolved" failure mode
    /// BT-3034 describes. That's why this test asserts on
    /// `protocol_registry.has_protocol` directly instead of scanning for a
    /// diagnostic message — there is no diagnostic to find; the whole point
    /// of the bug is that the check never ran.
    #[test]
    fn test_cross_file_protocol_resolution_seeds_pre_loaded_protocols() {
        let (_temp, lib_dir) = temp_utf8_dir();

        // File A: declares the protocol, mirroring `printable.bt`.
        let file_a = lib_dir.join("ProtocolFixtureA.bt");
        fs::write(
            &file_a,
            "Protocol define: ProtocolFixtureA\n  asString -> String\n",
        )
        .unwrap();

        // File B: references it in a parameter type annotation — the exact
        // shape of `console.bt`'s `printLine: aValue :: Printable -> Nil`.
        let file_b = lib_dir.join("ProtocolFixtureB.bt");
        fs::write(
            &file_b,
            "Object subclass: ProtocolFixtureB\n  \
             show: aValue :: ProtocolFixtureA -> Nil => nil\n",
        )
        .unwrap();

        // The real function under test, called the way `build_stdlib()`
        // actually calls it — over *all* source files in one pass, before
        // any compile happens (a live scan, not a seed from any prior
        // "generated" snapshot).
        let protocol_infos = collect_stdlib_protocol_infos(&[file_a.clone(), file_b.clone()]);
        assert_eq!(
            protocol_infos.len(),
            1,
            "expected exactly one protocol declaration, from file A"
        );
        assert_eq!(protocol_infos[0].name.as_str(), "ProtocolFixtureA");

        let options = stdlib_compiler_options(false);
        let bindings =
            beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new();

        // Compile B the way `build_stdlib()` actually does — through
        // `compile_source_with_bindings` with `pre_loaded_protocols` seeded
        // via `CompileContext`/`ClassHierarchyContext` — and confirm the
        // cross-file `:: ProtocolFixtureA` parameter annotation compiles
        // cleanly end to end.
        let core_file = lib_dir.join("b.core");
        compile_source_with_bindings(
            &file_b,
            "bt@stdlib@protocol_fixture_b",
            &core_file,
            &options,
            &bindings,
            &CompileContext {
                hierarchy: ClassHierarchyContext {
                    pre_loaded_protocols: protocol_infos.clone(),
                    ..ClassHierarchyContext::default()
                },
                ..CompileContext::default()
            },
            None,
        )
        .expect("cross-file protocol-typed parameter should compile without errors");

        // The real mechanism under test: does file B's own semantic analysis
        // actually *know about* the cross-file protocol afterwards?
        // `compile_source_with_bindings` doesn't expose the `ProtocolRegistry`
        // it built, so re-run semantic analysis directly (same
        // `pre_loaded_protocols` input `compile_ctx.hierarchy` carries) to
        // inspect it.
        let source_b = fs::read_to_string(&file_b).unwrap();
        let tokens_b = beamtalk_core::source_analysis::lex_with_eof(&source_b);
        let (module_b, _diags) = beamtalk_core::source_analysis::parse(tokens_b);
        let extensions = beamtalk_core::compilation::extension_index::ExtensionIndex::default();

        let result = beamtalk_core::semantic_analysis::analyse_full(
            &module_b,
            beamtalk_core::semantic_analysis::AnalysisContext::default()
                .with_options(&options)
                .with_pre_loaded_protocols(protocol_infos)
                .with_cross_file_extensions(&extensions),
        );
        assert!(
            result.protocol_registry.has_protocol("ProtocolFixtureA"),
            "expected pre_loaded_protocols to seed the cross-file protocol into \
             file B's own protocol registry"
        );

        // Negative control: WITHOUT pre_loaded_protocols (the pre-BT-3034
        // gap), the cross-file protocol never gets registered — proving the
        // assertion above exercises the fix rather than a tautology.
        let result_unfixed = beamtalk_core::semantic_analysis::analyse_full(
            &module_b,
            beamtalk_core::semantic_analysis::AnalysisContext::default()
                .with_options(&options)
                .with_cross_file_extensions(&extensions),
        );
        assert!(
            !result_unfixed
                .protocol_registry
                .has_protocol("ProtocolFixtureA"),
            "expected the negative control (no pre_loaded_protocols) to reproduce \
             the pre-BT-3034 gap: the cross-file protocol should NOT be registered"
        );
    }

    #[test]
    fn test_extract_alias_source_snippets_captures_verbatim_declaration() {
        let (_temp, lib_dir) = temp_utf8_dir();
        let file = lib_dir.join("Fixture.bt");
        fs::write(
            &file,
            "type RestartStrategy = #temporary | #transient | #permanent\n\
             Object subclass: Fixture\n  noop => nil\n",
        )
        .unwrap();

        let snippets = extract_alias_source_snippets(&file).unwrap();
        assert_eq!(snippets.len(), 1);
        assert_eq!(
            snippets[0],
            "type RestartStrategy = #temporary | #transient | #permanent"
        );

        // Round-trips through the read side of the generated table.
        let info = AliasRegistry::from_source_text(&snippets[0])
            .expect("verbatim declaration text should re-parse");
        assert_eq!(info.name.as_str(), "RestartStrategy");
        assert!(!info.is_internal);
    }

    #[test]
    fn test_extract_alias_source_snippets_captures_internal_modifier() {
        let (_temp, lib_dir) = temp_utf8_dir();
        let file = lib_dir.join("Fixture.bt");
        fs::write(&file, "internal type Scratch = Integer\n").unwrap();

        let snippets = extract_alias_source_snippets(&file).unwrap();
        assert_eq!(snippets, vec!["internal type Scratch = Integer"]);

        let info = AliasRegistry::from_source_text(&snippets[0]).unwrap();
        assert!(
            info.is_internal,
            "the `internal` modifier should survive the source-text round trip"
        );
    }

    #[test]
    fn test_extract_alias_source_snippets_no_aliases() {
        let (_temp, lib_dir) = temp_utf8_dir();
        let file = lib_dir.join("Fixture.bt");
        fs::write(&file, "Object subclass: Fixture\n  noop => nil\n").unwrap();

        assert!(extract_alias_source_snippets(&file).unwrap().is_empty());
    }

    /// Pass 2 (system review) edge case: a declaration that spans multiple
    /// source lines (a long union wrapped for readability) embeds a literal
    /// newline in the sliced snippet. Rust string literals accept a raw
    /// newline verbatim (no escaping needed — `generate_alias_sources_section`
    /// only escapes `\` and `"`), so this must still round-trip cleanly
    /// through generation and `AliasRegistry::from_source_text` reparse.
    #[test]
    fn test_extract_alias_source_snippets_handles_multiline_declaration() {
        let (_temp, lib_dir) = temp_utf8_dir();
        let file = lib_dir.join("Fixture.bt");
        fs::write(
            &file,
            "type Wrapped =\n    #a\n    | #b\n    | #c\n\nObject subclass: Fixture\n  noop => nil\n",
        )
        .unwrap();

        let snippets = extract_alias_source_snippets(&file).unwrap();
        assert_eq!(snippets.len(), 1);
        assert!(
            snippets[0].contains('\n'),
            "expected the multiline declaration to be captured verbatim, got: {:?}",
            snippets[0]
        );

        // Generation must produce valid Rust even with an embedded newline.
        let mut code = String::new();
        generate_alias_sources_section(&mut code, &snippets);
        assert!(code.contains("type Wrapped ="));

        // And the read side must still reconstruct the alias correctly.
        let info = AliasRegistry::from_source_text(&snippets[0])
            .expect("a multiline declaration should still re-parse");
        assert_eq!(info.name.as_str(), "Wrapped");
    }

    #[test]
    fn test_generate_alias_sources_section_empty() {
        let mut code = String::new();
        generate_alias_sources_section(&mut code, &[]);
        assert!(
            code.contains("pub(super) fn generated_stdlib_alias_sources() -> Vec<&'static str> {"),
            "Should always emit the function signature. Got: {code}"
        );
        assert!(
            code.contains("vec![\n    ]"),
            "Should emit an empty vec when there are no aliases. Got: {code}"
        );
    }

    #[test]
    fn test_generate_alias_sources_section_preserves_caller_order() {
        // Sorting by alias *name* requires the caller's `AliasInfo` (plain
        // declaration text alone would put `internal type ...` out of order
        // — see this function's doc) — so it emits entries in whatever order
        // it's given, trusting the caller (`build_stdlib()`) to have already
        // sorted by name.
        let mut code = String::new();
        generate_alias_sources_section(
            &mut code,
            &[
                "type Zebra = Integer".to_string(),
                "type Alpha = String".to_string(),
            ],
        );

        let alpha_pos = code.find("type Alpha").unwrap();
        let zebra_pos = code.find("type Zebra").unwrap();
        assert!(
            zebra_pos < alpha_pos,
            "Should preserve caller order (Zebra before Alpha here), not re-sort. Got: {code}"
        );
    }

    #[test]
    fn test_generate_alias_sources_section_escapes_embedded_quotes() {
        // Not valid Beamtalk syntax — this only exercises the generator's
        // Rust-string-literal escaping, not the parser.
        let mut code = String::new();
        generate_alias_sources_section(&mut code, &[r#"contains "a quote""#.to_string()]);

        assert!(
            code.contains(r#"contains \"a quote\""#),
            "Embedded quotes should be escaped for the Rust string literal. Got: {code}"
        );
    }

    #[test]
    fn test_find_stdlib_files() {
        let (_temp, lib_dir) = temp_utf8_dir();

        fs::write(lib_dir.join("integer.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("string.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("README.md"), "not a bt file").unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("integer.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("string.bt")));
    }

    #[test]
    fn test_find_stdlib_files_sorted() {
        let (_temp, lib_dir) = temp_utf8_dir();

        fs::write(lib_dir.join("Zebra.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("Alpha.bt"), "// stub").unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files[0].as_str() < files[1].as_str());
    }

    #[test]
    fn test_find_stdlib_files_empty_dir() {
        let (_temp, lib_dir) = temp_utf8_dir();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert!(files.is_empty());
    }

    #[test]
    fn test_module_name_from_path() {
        // ADR 0016: All stdlib modules use bt@stdlib@ prefix
        let path = Utf8PathBuf::from("lib/integer.bt");
        assert_eq!(module_name_from_path(&path).unwrap(), "bt@stdlib@integer");
    }

    #[test]
    fn test_module_name_from_path_non_primitive() {
        let path = Utf8PathBuf::from("lib/beamtalk_interface.bt");
        assert_eq!(
            module_name_from_path(&path).unwrap(),
            "bt@stdlib@beamtalk_interface"
        );
    }

    #[test]
    fn test_module_name_from_path_multi_word() {
        let path = Utf8PathBuf::from("lib/proto_object.bt");
        assert_eq!(
            module_name_from_path(&path).unwrap(),
            "bt@stdlib@proto_object"
        );
    }

    #[test]
    fn test_module_name_from_path_invalid() {
        let path = Utf8PathBuf::from("lib/my-module.bt");
        assert!(module_name_from_path(&path).is_err());
    }

    #[test]
    fn test_clean_ebin_dir() {
        let (_temp, ebin_dir) = temp_utf8_dir();

        fs::write(ebin_dir.join("beamtalk_integer.beam"), "fake").unwrap();
        fs::write(ebin_dir.join("beamtalk_stdlib.app"), "fake").unwrap();
        fs::write(ebin_dir.join("keep_me.txt"), "keep").unwrap();

        clean_ebin_dir(&ebin_dir).unwrap();

        assert!(!ebin_dir.join("beamtalk_integer.beam").exists());
        assert!(!ebin_dir.join("beamtalk_stdlib.app").exists());
        assert!(ebin_dir.join("keep_me.txt").exists());
    }

    #[test]
    fn test_generate_app_file() {
        let (_temp, ebin_dir) = temp_utf8_dir();

        let source_files = vec![
            Utf8PathBuf::from("lib/integer.bt"),
            Utf8PathBuf::from("lib/string.bt"),
        ];

        generate_app_file(&ebin_dir, &source_files, &[], &[], &[]).unwrap();

        let app_file = ebin_dir.join("beamtalk_stdlib.app");
        assert!(app_file.exists());

        let content = fs::read_to_string(app_file).unwrap();
        assert!(content.contains("beamtalk_stdlib"));
        // ADR 0016: Module names use bt@stdlib@ prefix
        assert!(content.contains("'bt@stdlib@integer'"));
        assert!(content.contains("'bt@stdlib@string'"));
    }

    #[test]
    fn test_generate_app_file_empty() {
        let (_temp, ebin_dir) = temp_utf8_dir();

        generate_app_file(&ebin_dir, &[], &[], &[], &[]).unwrap();

        let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
        assert!(content.contains("{modules, []}"));
        // BT-2938: no type-alias metadata => no `type_aliases` env key at all
        // (matches `format_type_aliases_entry`'s empty-input contract).
        assert!(!content.contains("type_aliases"));
    }

    #[test]
    fn test_generate_app_file_invalid_name_errors() {
        let (_temp, ebin_dir) = temp_utf8_dir();

        let source_files = vec![
            Utf8PathBuf::from("lib/integer.bt"),
            Utf8PathBuf::from("lib/my-bad-name.bt"),
        ];

        let result = generate_app_file(&ebin_dir, &source_files, &[], &[], &[]);
        assert!(result.is_err());
    }

    #[test]
    fn test_generate_app_file_with_protocol_modules() {
        let (_temp, ebin_dir) = temp_utf8_dir();

        let source_files = vec![Utf8PathBuf::from("lib/integer.bt")];
        let protocol_modules = vec!["bt@stdlib@printable".to_string()];

        generate_app_file(&ebin_dir, &source_files, &[], &protocol_modules, &[]).unwrap();

        let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
        assert!(
            content.contains("{protocol_modules, ['bt@stdlib@printable']}"),
            "Should contain protocol_modules env key. Got:\n{content}"
        );
    }

    #[test]
    fn test_generate_app_file_with_type_aliases() {
        let (_temp, ebin_dir) = temp_utf8_dir();

        let source_files = vec![Utf8PathBuf::from("lib/supervisor.bt")];
        let alias_metadata = vec![app_file::AliasMetadata {
            name: "SupervisionStrategy".to_string(),
            expansion: "#oneForOne | #oneForAll | #restForOne".to_string(),
            doc: None,
            source_file: "lib/supervisor.bt".to_string(),
            internal: false,
        }];

        generate_app_file(&ebin_dir, &source_files, &[], &[], &alias_metadata).unwrap();

        let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
        assert!(
            content.contains("{type_aliases, [")
                && content.contains("name => 'SupervisionStrategy'")
                && content.contains("expansion => \"#oneForOne | #oneForAll | #restForOne\""),
            "Should contain type_aliases env key with the alias entry. Got:\n{content}"
        );
    }

    fn sample_class_meta() -> ClassMeta {
        ClassMeta {
            module_name: "bt@stdlib@counter".to_string(),
            class_name: "Counter".to_string(),
            superclass_name: "Actor".to_string(),
            modifiers: ClassModifiers::default(),
            class_kind: beamtalk_core::ast::ClassKind::Actor,
            state: vec!["count".to_string()],
            state_types: vec![],
            state_has_default: vec![],
            methods: vec![
                MethodMeta {
                    selector: "increment".to_string(),
                    arity: 0,
                    kind: MethodKindMeta::Primary,
                    is_sealed: false,
                    is_internal: false,
                    spawns_block: false,
                    return_type: None,
                    param_types: vec![],
                    doc: None,
                },
                MethodMeta {
                    selector: "add:".to_string(),
                    arity: 1,
                    kind: MethodKindMeta::Primary,
                    is_sealed: true,
                    is_internal: false,
                    spawns_block: false,
                    return_type: Some(DeclaredType::simple("Counter")),
                    param_types: vec![Some(DeclaredType::simple("Integer"))],
                    doc: None,
                },
            ],
            class_methods: vec![MethodMeta {
                selector: "default".to_string(),
                arity: 0,
                kind: MethodKindMeta::Primary,
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(DeclaredType::simple("Counter")),
                param_types: vec![],
                doc: None,
            }],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            handle_scope: None,
        }
    }

    #[test]
    fn test_generate_class_entry_emits_correct_structure() {
        let meta = sample_class_meta();
        let mut code = String::new();
        generate_class_entry(&mut code, &meta);

        assert!(code.contains("\"Counter\".into()"));
        assert!(code.contains("Some(\"Actor\".into())"));
        assert!(code.contains("is_sealed: false"));
        assert!(code.contains("is_abstract: false"));
        assert!(code.contains("\"count\".into()"));
        assert!(code.contains("selector: \"increment\".into()"));
        assert!(code.contains("selector: \"add:\".into()"));
        assert!(code.contains("arity: 1"));
        assert!(code.contains("selector: \"default\".into()"));
        // ADR 0103: the generated ClassInfo carries the handle_scope field.
        assert!(code.contains("handle_scope: None"));
    }

    #[test]
    fn test_generate_class_entry_sealed_methods() {
        let meta = sample_class_meta();
        let mut code = String::new();
        generate_class_entry(&mut code, &meta);

        // The add: method is sealed
        assert!(code.contains("is_sealed: true"));
    }

    #[test]
    fn test_generate_class_entry_root_class() {
        let meta = ClassMeta {
            module_name: "bt@stdlib@proto_object".to_string(),
            class_name: "ProtoObject".to_string(),
            superclass_name: "none".to_string(),
            modifiers: ClassModifiers {
                is_abstract: true,
                ..ClassModifiers::default()
            },
            class_kind: beamtalk_core::ast::ClassKind::Object,
            state: vec![],
            state_types: vec![],
            state_has_default: vec![],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            handle_scope: None,
        };
        let mut code = String::new();
        generate_class_entry(&mut code, &meta);

        assert!(code.contains("superclass: None"));
        assert!(code.contains("is_abstract: true"));
        assert!(code.contains("methods: vec![],"));
    }

    #[test]
    fn test_generate_method_list_empty() {
        let mut code = String::new();
        generate_method_list(&mut code, "methods", &[], "Test");
        assert!(code.contains("methods: vec![],"));
    }

    #[test]
    fn test_generate_superclass_type_args_empty() {
        let mut code = String::new();
        generate_superclass_type_args(&mut code, &[], &[]);
        assert!(
            code.contains("superclass_type_args: vec![],"),
            "Should emit empty vec when no args. Got: {code}"
        );
    }

    #[test]
    fn test_generate_superclass_type_args_param_ref() {
        let mut code = String::new();
        generate_superclass_type_args(&mut code, &[DeclaredType::simple("E")], &["E".to_string()]);
        assert!(
            code.contains(
                "superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 0 }]"
            ),
            "Should emit ParamRef for forwarded type params. Got: {code}"
        );
    }

    #[test]
    fn test_generate_superclass_type_args_param_ref_second_position() {
        // `Collection(V) subclass: Dictionary(K, V)` — V is at param_index 1.
        let mut code = String::new();
        generate_superclass_type_args(
            &mut code,
            &[DeclaredType::simple("V")],
            &["K".to_string(), "V".to_string()],
        );
        assert!(
            code.contains(
                "superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 1 }]"
            ),
            "Should compute param_index relative to subclass's own type_params. Got: {code}"
        );
    }

    #[test]
    fn test_generate_superclass_type_args_concrete() {
        // `Collection(Integer) subclass: IntArray` — Integer is not a type param.
        let mut code = String::new();
        generate_superclass_type_args(&mut code, &[DeclaredType::simple("Integer")], &[]);
        assert!(
            code.contains(
                "superclass_type_args: vec![SuperclassTypeArg::Concrete { declared: DeclaredType::simple(\"Integer\") }]"
            ),
            "Non-type-param args should emit Concrete. Got: {code}"
        );
    }

    #[test]
    fn test_generate_superclass_type_args_concrete_escaped() {
        // Guard against unescaped quotes/backslashes breaking the generated Rust.
        let mut code = String::new();
        generate_superclass_type_args(
            &mut code,
            &[DeclaredType::simple("My\\\"Type")],
            &["E".to_string()],
        );
        assert!(
            code.contains(
                "SuperclassTypeArg::Concrete { declared: DeclaredType::simple(\"My\\\\\\\"Type\") }"
            ),
            "Should emit escaped Concrete type_name. Got: {code}"
        );
    }

    #[test]
    fn test_generate_builtins_sorted_deterministic() {
        let meta = [
            ClassMeta {
                module_name: "bt@stdlib@zebra".to_string(),
                class_name: "Zebra".to_string(),
                superclass_name: "Object".to_string(),
                modifiers: ClassModifiers::default(),
                class_kind: beamtalk_core::ast::ClassKind::Object,
                state: vec![],
                state_types: vec![],
                state_has_default: vec![],
                methods: vec![],
                class_methods: vec![],
                class_variables: vec![],
                type_params: vec![],
                superclass_type_args: vec![],
                handle_scope: None,
            },
            ClassMeta {
                module_name: "bt@stdlib@alpha".to_string(),
                class_name: "Alpha".to_string(),
                superclass_name: "Object".to_string(),
                modifiers: ClassModifiers {
                    is_sealed: true,
                    ..ClassModifiers::default()
                },
                class_kind: beamtalk_core::ast::ClassKind::Object,
                state: vec![],
                state_types: vec![],
                state_has_default: vec![],
                methods: vec![],
                class_methods: vec![],
                class_variables: vec![],
                type_params: vec![],
                superclass_type_args: vec![],
                handle_scope: None,
            },
        ];

        let mut code = String::new();
        // Simulate the sorted generation from generate_builtins_rs
        let sorted = sorted_by_class_name(&meta);

        for m in &sorted {
            generate_class_entry(&mut code, m);
        }

        // Alpha should appear before Zebra
        let alpha_pos = code.find("\"Alpha\"").unwrap();
        let zebra_pos = code.find("\"Zebra\"").unwrap();
        assert!(
            alpha_pos < zebra_pos,
            "Classes should be sorted alphabetically"
        );
    }

    #[test]
    fn test_generate_method_list_emits_return_type() {
        let methods = vec![MethodMeta {
            selector: "add:".to_string(),
            arity: 1,
            kind: MethodKindMeta::Primary,
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(DeclaredType::simple("Counter")),
            param_types: vec![Some(DeclaredType::simple("Integer"))],
            doc: None,
        }];
        let mut code = String::new();
        generate_method_list(&mut code, "methods", &methods, "Counter");
        assert!(
            code.contains("return_type: Some(DeclaredType::simple(\"Counter\"))"),
            "Should emit return type. Got: {code}"
        );
        assert!(
            code.contains("param_types: vec![Some(DeclaredType::simple(\"Integer\"))]"),
            "Should emit param types. Got: {code}"
        );
    }

    #[test]
    fn test_generate_method_list_emits_none_return_type() {
        let methods = vec![MethodMeta {
            selector: "increment".to_string(),
            arity: 0,
            kind: MethodKindMeta::Primary,
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![],
            doc: None,
        }];
        let mut code = String::new();
        generate_method_list(&mut code, "methods", &methods, "Counter");
        assert!(
            code.contains("return_type: None"),
            "Should emit None for untyped methods. Got: {code}"
        );
        assert!(
            code.contains("param_types: vec![]"),
            "Should emit empty param_types. Got: {code}"
        );
    }

    #[test]
    fn test_generate_method_list_emits_spawns_block_true() {
        let methods = vec![MethodMeta {
            selector: "after:do:".to_string(),
            arity: 2,
            kind: MethodKindMeta::Primary,
            is_sealed: false,
            is_internal: false,
            spawns_block: true,
            return_type: None,
            param_types: vec![Some(DeclaredType::simple("Integer")), None],
            doc: None,
        }];
        let mut code = String::new();
        generate_method_list(&mut code, "class_methods", &methods, "Timer");
        assert!(
            code.contains("spawns_block: true"),
            "Should emit spawns_block: true. Got: {code}"
        );
    }

    #[test]
    fn test_generate_method_list_emits_is_internal_true() {
        // ADR 0101 Part 4: internal seam helpers must propagate `is_internal:
        // true` into the generated builtin metadata (it was previously
        // hardcoded `false`). Guards the `true` path of the `{internal}` field.
        let methods = vec![MethodMeta {
            selector: "xrefImplementorsOf:".to_string(),
            arity: 1,
            kind: MethodKindMeta::Primary,
            is_sealed: false,
            is_internal: true,
            spawns_block: false,
            return_type: Some(DeclaredType::simple("Dictionary")),
            param_types: vec![Some(DeclaredType::simple("Symbol"))],
            doc: None,
        }];
        let mut code = String::new();
        generate_method_list(&mut code, "methods", &methods, "SystemNavigation");
        assert!(
            code.contains("is_internal: true"),
            "Should emit is_internal: true for an internal method. Got: {code}"
        );
    }

    // --- stdlib/src/ subdirectory support ---

    #[test]
    fn find_stdlib_files_recurses_into_subdirectories() {
        let (_temp, dir) = temp_utf8_dir();
        fs::create_dir_all(dir.join("collections")).unwrap();
        fs::create_dir_all(dir.join("actors/supervision")).unwrap();
        fs::write(dir.join("object.bt"), "Object subclass: Object\n").unwrap();
        fs::write(dir.join("collections/array.bt"), "Object subclass: Array\n").unwrap();
        fs::write(
            dir.join("actors/supervision/supervisor.bt"),
            "Object subclass: Supervisor\n",
        )
        .unwrap();

        let files = find_stdlib_files(&dir).unwrap();

        let mut stems: Vec<&str> = files.iter().filter_map(|f| f.file_stem()).collect();
        stems.sort_unstable();
        assert_eq!(
            stems,
            vec!["array", "object", "supervisor"],
            "Nested classes must be found at any depth. Got: {files:?}"
        );
    }

    #[test]
    fn module_name_ignores_subdirectory() {
        // Subdirectories are editorial only — `bt@stdlib@array` regardless of
        // where the file sits. Deliberately unlike user packages, where
        // `src/util/math.bt` becomes `util@math`.
        let flat = module_name_from_path(Utf8Path::new("stdlib/src/array.bt")).unwrap();
        let nested =
            module_name_from_path(Utf8Path::new("stdlib/src/collections/array.bt")).unwrap();

        assert_eq!(flat, "bt@stdlib@array");
        assert_eq!(nested, flat, "Subdirectory must not change the module name");
    }

    #[test]
    fn check_duplicate_module_names_accepts_unique_names_across_subdirectories() {
        let files = vec![
            Utf8PathBuf::from("stdlib/src/object.bt"),
            Utf8PathBuf::from("stdlib/src/collections/array.bt"),
            Utf8PathBuf::from("stdlib/src/numeric/integer.bt"),
        ];
        assert!(check_duplicate_module_names(&files).is_ok());
    }

    #[test]
    fn check_duplicate_module_names_rejects_same_stem_in_two_subdirectories() {
        // Both compile to `bt@stdlib@array`, silently clobbering in ebin/.
        let files = vec![
            Utf8PathBuf::from("stdlib/src/collections/array.bt"),
            Utf8PathBuf::from("stdlib/src/legacy/array.bt"),
        ];

        let err = check_duplicate_module_names(&files)
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("stdlib/src/collections/array.bt")
                && err.contains("stdlib/src/legacy/array.bt"),
            "Error must name both colliding paths in full, so the user knows \
             which two files to rename. Got: {err}"
        );
        assert!(
            err.contains("bt@stdlib@array"),
            "Error should name the module they collide on. Got: {err}"
        );
    }

    #[test]
    fn check_duplicate_module_names_catches_case_folded_collision() {
        // `to_module_name` lowercases, so these distinct filenames both yield
        // `bt@stdlib@beamerror`. A raw-stem comparison would miss it — and
        // `beamerror.bt` is a real stdlib class, so the shape is not academic.
        let files = vec![
            Utf8PathBuf::from("stdlib/src/beamerror.bt"),
            Utf8PathBuf::from("stdlib/src/errors/Beamerror.bt"),
        ];

        let err = check_duplicate_module_names(&files)
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("bt@stdlib@beamerror"),
            "Case-folded collision must be rejected. Got: {err}"
        );
    }

    /// Project root, two levels up from this crate's manifest directory —
    /// mirrors `erlfmt.rs`'s `project_root()` test helper.
    fn project_root() -> Utf8PathBuf {
        let manifest_dir = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        manifest_dir
            .parent()
            .and_then(|p| p.parent())
            .expect("project root")
            .to_owned()
    }

    /// BT-3033: `beamtalk_primitive:is_string_binary_shared_selector/1` hand-lists
    /// the `binary.bt` instance selectors that `string.bt` inherits unchanged
    /// (byte-level primitives, safe to dispatch without the `is_utf8/1` scan).
    /// This test recomputes that set from the real `.bt` sources — Binary's own
    /// instance selectors minus whatever String redefines — and fails if it
    /// drifts from the hardcoded Erlang list, so a future edit to either file
    /// that changes the override relationship is caught here instead of
    /// silently reintroducing BT-2999-style misdispatch.
    ///
    /// BT-3049: this only sees overrides made by editing `binary.bt`/`string.bt`
    /// directly — it has no visibility into selectors added via the `extend`
    /// mechanism (ADR 0066), which lives in separate extension sources, not the
    /// class bodies this test parses. That's a known, currently-low-risk gap
    /// (an `extend` can't override a class-body-defined method per ADR 0066, and
    /// `beamtalk_primitive:module_for_value/2` separately checks the extension
    /// registry at runtime for `String`-side overrides), not a guarantee this
    /// test makes today.
    #[test]
    fn test_binary_string_shared_selectors_stay_in_sync() {
        let root = project_root();
        let binary_path = root.join("stdlib/src/binary.bt");
        let string_path = root.join("stdlib/src/string.bt");

        let binary_meta = extract_class_metadata(&binary_path, "bt@stdlib@binary")
            .expect("binary.bt should parse");
        let string_meta = extract_class_metadata(&string_path, "bt@stdlib@string")
            .expect("string.bt should parse");

        let string_overrides: std::collections::HashSet<&str> = string_meta
            .methods
            .iter()
            .map(|m| m.selector.as_str())
            .collect();

        let inherited_unchanged: std::collections::BTreeSet<&str> = binary_meta
            .methods
            .iter()
            .map(|m| m.selector.as_str())
            .filter(|selector| !string_overrides.contains(selector))
            .collect();

        // Mirrors beamtalk_primitive:is_string_binary_shared_selector/1 in
        // runtime/apps/beamtalk_runtime/src/beamtalk_primitive.erl — keep both
        // lists identical.
        let hardcoded: std::collections::BTreeSet<&str> = [
            "byteAt:",
            "byteSize",
            "part:size:",
            "concat:",
            "toBytes",
            "asStringUnchecked",
            "asBase64",
            "asBase64Url",
            "asHex",
        ]
        .into_iter()
        .collect();

        assert_eq!(
            inherited_unchanged, hardcoded,
            "binary.bt selectors NOT overridden by string.bt (left) no longer \
             match beamtalk_primitive.erl's is_string_binary_shared_selector/1 \
             (right). Update that Erlang function to match — a selector only \
             belongs there if string.bt truly inherits it unchanged from \
             binary.bt."
        );
    }

    // --- BT-3351: is_stdlib_up_to_date / oldest_mtime_in_dir ---
    //
    // `is_stdlib_up_to_date` also checks the real compiler binary
    // (`std::env::current_exe`), which isn't injectable, so the
    // compiler-binary-newer branch is exercised deterministically via a
    // beam/source mtime far enough in the past that any real compiler binary
    // is newer.
    //
    // BT-3357: the runtime-`.beam`-newer check used to call
    // `beamtalk_cli::repl_startup::find_runtime_dir_with_layout()` directly,
    // with no seam to point it at a synthetic directory. `is_stdlib_up_to_date`
    // now takes the runtime ebin directory list as a parameter (real
    // discovery lives in `discover_runtime_ebin_dirs`, called only from the
    // real `build_stdlib()` call site) — see the `runtime_ebin` tests below
    // for both the "newer" (rebuild) and "older" (no rebuild) branches, plus
    // the "discovery failed" (`None`) branch.

    /// Sets a file's mtime directly — lets these tests control the
    /// before/after ordering `is_stdlib_up_to_date` compares, without
    /// depending on real wall-clock timing between writes.
    fn set_mtime(path: &Utf8Path, time: SystemTime) {
        // Windows' `SetFileTime` needs a handle opened with write access;
        // a read-only `File::open` handle gets `PermissionDenied` (code 5).
        fs::OpenOptions::new()
            .write(true)
            .open(path.as_std_path())
            .unwrap()
            .set_modified(time)
            .unwrap();
    }

    #[test]
    fn test_is_stdlib_up_to_date_missing_ebin_dir() {
        let (_temp, base) = temp_utf8_dir();
        let ebin = base.join("does_not_exist");
        assert!(!is_stdlib_up_to_date(&ebin, &[], Some(&[])));
    }

    #[test]
    fn test_is_stdlib_up_to_date_no_beam_files() {
        let (_temp, ebin) = temp_utf8_dir();
        fs::write(ebin.join("readme.txt"), "not a beam file").unwrap();
        assert!(!is_stdlib_up_to_date(&ebin, &[], Some(&[])));
    }

    #[test]
    fn test_is_stdlib_up_to_date_beam_source_count_mismatch() {
        let (_temp, ebin) = temp_utf8_dir();
        fs::write(ebin.join("a.beam"), "").unwrap();

        let (_temp2, src_dir) = temp_utf8_dir();
        let src1 = src_dir.join("A.bt");
        let src2 = src_dir.join("B.bt");
        fs::write(&src1, "").unwrap();
        fs::write(&src2, "").unwrap();

        // One .beam but two sources: a renamed/removed source must force a
        // rebuild rather than silently leaving a stale .beam in place.
        assert!(!is_stdlib_up_to_date(&ebin, &[src1, src2], Some(&[])));
    }

    #[test]
    fn test_is_stdlib_up_to_date_source_newer_than_output() {
        let (_temp, ebin) = temp_utf8_dir();
        let beam = ebin.join("a.beam");
        fs::write(&beam, "").unwrap();
        set_mtime(
            &beam,
            SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000_000),
        );

        let (_temp2, src_dir) = temp_utf8_dir();
        let src = src_dir.join("A.bt");
        fs::write(&src, "").unwrap(); // freshly written: newer than the beam output above

        assert!(!is_stdlib_up_to_date(&ebin, &[src], Some(&[])));
    }

    #[test]
    fn test_is_stdlib_up_to_date_missing_source_file_metadata() {
        let (_temp, ebin) = temp_utf8_dir();
        fs::write(ebin.join("a.beam"), "").unwrap();

        let (_temp2, src_dir) = temp_utf8_dir();
        let gone = src_dir.join("Gone.bt"); // listed but never created

        assert!(!is_stdlib_up_to_date(&ebin, &[gone], Some(&[])));
    }

    #[test]
    fn test_is_stdlib_up_to_date_compiler_binary_newer_forces_rebuild() {
        let (_temp, ebin) = temp_utf8_dir();
        let beam = ebin.join("a.beam");
        fs::write(&beam, "").unwrap();
        // Far enough in the past (1970) that the real test binary — built
        // moments before this test ran — is deterministically newer.
        let ancient = SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000);
        set_mtime(&beam, ancient);

        let (_temp2, src_dir) = temp_utf8_dir();
        let src = src_dir.join("A.bt");
        fs::write(&src, "").unwrap();
        set_mtime(&src, ancient); // same age as the beam output: not "source newer"

        assert!(!is_stdlib_up_to_date(&ebin, &[src], Some(&[])));
    }

    /// Shared setup for the `runtime_ebin_dirs`-branch tests below: a
    /// same-age beam/source pair far enough in the future that the real
    /// compiler binary running this test is never "newer than output" —
    /// isolating each test to just the runtime-ebin-dirs check that follows
    /// it.
    fn up_to_date_beam_and_source() -> (TempDir, Utf8PathBuf, TempDir, Utf8PathBuf, SystemTime) {
        let (ebin_temp, ebin) = temp_utf8_dir();
        let beam = ebin.join("a.beam");
        fs::write(&beam, "").unwrap();
        let output_time = SystemTime::UNIX_EPOCH + Duration::from_secs(4_000_000_000);
        set_mtime(&beam, output_time);

        let (src_temp, src_dir) = temp_utf8_dir();
        let src = src_dir.join("A.bt");
        fs::write(&src, "").unwrap();
        set_mtime(&src, output_time - Duration::from_secs(1));

        (ebin_temp, ebin, src_temp, src, output_time)
    }

    #[test]
    fn test_is_stdlib_up_to_date_runtime_beam_newer_forces_rebuild() {
        let (_ebin_temp, ebin, _src_temp, src, output_time) = up_to_date_beam_and_source();

        let (_temp3, runtime_ebin) = temp_utf8_dir();
        let runtime_beam = runtime_ebin.join("beamtalk_runtime.beam");
        fs::write(&runtime_beam, "").unwrap();
        set_mtime(&runtime_beam, output_time + Duration::from_secs(1));

        assert!(!is_stdlib_up_to_date(
            &ebin,
            &[src],
            Some(&[runtime_ebin.into_std_path_buf()]),
        ));
    }

    #[test]
    fn test_is_stdlib_up_to_date_runtime_beam_older_no_rebuild() {
        let (_ebin_temp, ebin, _src_temp, src, output_time) = up_to_date_beam_and_source();

        let (_temp3, runtime_ebin) = temp_utf8_dir();
        let runtime_beam = runtime_ebin.join("beamtalk_runtime.beam");
        fs::write(&runtime_beam, "").unwrap();
        set_mtime(&runtime_beam, output_time - Duration::from_secs(1));

        assert!(is_stdlib_up_to_date(
            &ebin,
            &[src],
            Some(&[runtime_ebin.into_std_path_buf()]),
        ));
    }

    #[test]
    fn test_is_stdlib_up_to_date_runtime_dirs_undiscoverable_forces_rebuild() {
        let (_ebin_temp, ebin, _src_temp, src, _output_time) = up_to_date_beam_and_source();

        // `None` stands in for `discover_runtime_ebin_dirs()` failing to
        // locate the runtime layout at all — same "force rebuild to be
        // safe" contract as every other missing-input branch above.
        assert!(!is_stdlib_up_to_date(&ebin, &[src], None));
    }

    #[test]
    fn test_is_stdlib_up_to_date_missing_runtime_ebin_dir_is_skipped() {
        // A runtime ebin directory that doesn't exist in this layout (e.g.
        // an app not built yet) is skipped, not treated as an error.
        let (_ebin_temp, ebin, _src_temp, src, _output_time) = up_to_date_beam_and_source();

        let (_temp3, base) = temp_utf8_dir();
        let missing_runtime_ebin = base.join("does_not_exist");

        assert!(is_stdlib_up_to_date(
            &ebin,
            &[src],
            Some(&[missing_runtime_ebin.into_std_path_buf()]),
        ));
    }

    #[test]
    fn test_oldest_mtime_in_dir_returns_none_when_empty() {
        let (_temp, dir) = temp_utf8_dir();
        assert!(oldest_mtime_in_dir(&dir, "beam").is_none());
    }

    #[test]
    fn test_oldest_mtime_in_dir_ignores_other_extensions() {
        let (_temp, dir) = temp_utf8_dir();
        let unrelated = dir.join("skip.txt");
        fs::write(&unrelated, "").unwrap();
        assert!(oldest_mtime_in_dir(&dir, "beam").is_none());
    }

    #[test]
    fn test_oldest_mtime_in_dir_picks_oldest_matching_extension() {
        let (_temp, dir) = temp_utf8_dir();
        let old = dir.join("old.beam");
        let newer = dir.join("newer.beam");
        let wrong_ext = dir.join("oldest_but_wrong_ext.txt");
        fs::write(&old, "").unwrap();
        fs::write(&newer, "").unwrap();
        fs::write(&wrong_ext, "").unwrap();

        let old_time = SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000);
        let newer_time = SystemTime::UNIX_EPOCH + Duration::from_secs(2_000_000);
        set_mtime(&old, old_time);
        set_mtime(&newer, newer_time);
        // Older than either .beam file, but the wrong extension — must be ignored.
        set_mtime(&wrong_ext, SystemTime::UNIX_EPOCH);

        assert_eq!(oldest_mtime_in_dir(&dir, "beam"), Some(old_time));
    }

    // --- BT-3357: write_generated_file_if_changed ---
    //
    // `generate_builtins_rs`/`generate_erlang_builtins_hrl` themselves still
    // write to the real hardcoded `GENERATED_BUILTINS_PATH`/
    // `GENERATED_BUILTINS_HRL_PATH` constants and aren't unit-tested directly
    // (per CLAUDE.md's "Generated files" rule, and see BT-3357's cluster-2
    // follow-up for the rest of their orchestration). This shared helper is
    // where the "only write if changed" and write-error branches actually
    // live, and it takes `dest` as a parameter, so tests exercise it against
    // a temp file instead.

    #[test]
    fn test_write_generated_file_if_changed_creates_missing_file() {
        let (_temp, dir) = temp_utf8_dir();
        let dest = dir.join("generated.rs");

        write_generated_file_if_changed(&dest, "content v1").unwrap();

        assert_eq!(fs::read_to_string(&dest).unwrap(), "content v1");
    }

    #[test]
    fn test_write_generated_file_if_changed_overwrites_when_content_differs() {
        let (_temp, dir) = temp_utf8_dir();
        let dest = dir.join("generated.rs");
        fs::write(&dest, "content v1").unwrap();

        write_generated_file_if_changed(&dest, "content v2").unwrap();

        assert_eq!(fs::read_to_string(&dest).unwrap(), "content v2");
    }

    #[test]
    fn test_write_generated_file_if_changed_skips_write_when_unchanged() {
        let (_temp, dir) = temp_utf8_dir();
        let dest = dir.join("generated.rs");
        fs::write(&dest, "same content").unwrap();
        // Far enough in the past that an unwanted rewrite (which would bump
        // the mtime to "now") is trivially detectable below.
        let original_mtime = SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000);
        set_mtime(&dest, original_mtime);

        write_generated_file_if_changed(&dest, "same content").unwrap();

        assert_eq!(
            fs::metadata(dest.as_std_path())
                .unwrap()
                .modified()
                .unwrap(),
            original_mtime,
            "content was unchanged — the file must not have been rewritten"
        );
    }

    #[test]
    fn test_write_generated_file_if_changed_reports_write_error() {
        let (_temp, dir) = temp_utf8_dir();
        // Parent directory doesn't exist, so the write itself must fail —
        // exercises the write-error path without touching any real file.
        let dest = dir.join("no_such_subdir").join("generated.rs");

        let result = write_generated_file_if_changed(&dest, "content");

        assert!(result.is_err());
    }

    // --- BT-3351: mark_spawns_for_selectors and its Timer/Parallel/Collection callers ---

    fn method_meta(selector: &str) -> MethodMeta {
        MethodMeta {
            arity: selector.matches(':').count(),
            selector: selector.to_string(),
            kind: MethodKindMeta::Primary,
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![],
            doc: None,
        }
    }

    #[test]
    fn test_mark_spawns_for_selectors_marks_only_matching_selectors() {
        let mut methods = vec![method_meta("after:do:"), method_meta("other:")];
        mark_spawns_for_selectors(&mut methods, &["after:do:"], "Test").unwrap();
        assert!(methods[0].spawns_block);
        assert!(!methods[1].spawns_block);
    }

    #[test]
    fn test_mark_spawns_for_selectors_errors_on_unmatched_selector() {
        let mut methods = vec![method_meta("foo")];
        let err =
            mark_spawns_for_selectors(&mut methods, &["after:do:", "foo"], "Timer").unwrap_err();
        let message = err.to_string();
        assert!(message.contains("Timer"), "got: {message}");
        assert!(message.contains("after:do:"), "got: {message}");
    }

    #[test]
    fn test_mark_timer_spawns_marks_expected_selectors() {
        let mut class_methods = vec![
            method_meta("after:do:"),
            method_meta("every:do:"),
            method_meta("other:"),
        ];
        mark_timer_spawns(&mut class_methods).unwrap();
        assert!(class_methods[0].spawns_block);
        assert!(class_methods[1].spawns_block);
        assert!(!class_methods[2].spawns_block);
    }

    #[test]
    fn test_mark_timer_spawns_errors_when_selectors_missing() {
        let mut class_methods = vec![method_meta("other:")];
        assert!(mark_timer_spawns(&mut class_methods).is_err());
    }

    #[test]
    fn test_mark_parallel_spawns_marks_all_expected_selectors() {
        let mut class_methods = vec![
            method_meta("all:"),
            method_meta("all:timeout:"),
            method_meta("any:"),
        ];
        mark_parallel_spawns(&mut class_methods).unwrap();
        assert!(class_methods.iter().all(|m| m.spawns_block));
    }

    #[test]
    fn test_mark_parallel_spawns_errors_when_selectors_missing() {
        let mut class_methods = vec![method_meta("other:")];
        assert!(mark_parallel_spawns(&mut class_methods).is_err());
    }

    #[test]
    fn test_mark_parallel_collect_spawns_marks_expected_selectors() {
        let mut methods = vec![
            method_meta("parallelCollect:"),
            method_meta("parallelCollect:maxConcurrency:"),
        ];
        mark_parallel_collect_spawns(&mut methods).unwrap();
        assert!(methods.iter().all(|m| m.spawns_block));
    }

    #[test]
    fn test_mark_parallel_collect_spawns_errors_when_selectors_missing() {
        let mut methods = vec![method_meta("other:")];
        assert!(mark_parallel_collect_spawns(&mut methods).is_err());
    }

    // --- BT-3351: extract_class_metadata error paths and Timer/Parallel/Collection triggers ---

    #[test]
    fn test_extract_class_metadata_no_class_definition_errors() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("Empty.bt");
        fs::write(&file, "// just a comment, no class here\n").unwrap();

        let err = extract_class_metadata(&file, "bt@stdlib@empty")
            .err()
            .unwrap();
        assert!(err.to_string().contains("No class definition"));
    }

    #[test]
    fn test_extract_class_metadata_multiple_classes_errors() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("Two.bt");
        fs::write(
            &file,
            "Object subclass: FirstOne\n  noop => nil\n\
             Object subclass: SecondOne\n  noop => nil\n",
        )
        .unwrap();

        let err = extract_class_metadata(&file, "bt@stdlib@two")
            .err()
            .unwrap();
        assert!(err.to_string().contains("Expected exactly one class"));
    }

    #[test]
    fn test_extract_class_metadata_timer_marks_spawning_class_methods() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("timer.bt");
        fs::write(
            &file,
            "Object subclass: Timer\n  \
             class after: ms do: block => nil\n  \
             class every: ms do: block => nil\n",
        )
        .unwrap();

        let meta = extract_class_metadata(&file, "bt@stdlib@timer").unwrap();
        assert!(
            meta.class_methods
                .iter()
                .find(|m| m.selector == "after:do:")
                .expect("after:do: should be present")
                .spawns_block
        );
        assert!(
            meta.class_methods
                .iter()
                .find(|m| m.selector == "every:do:")
                .expect("every:do: should be present")
                .spawns_block
        );
    }

    #[test]
    fn test_extract_class_metadata_timer_missing_selectors_errors() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("timer.bt");
        fs::write(&file, "Object subclass: Timer\n  class noop => nil\n").unwrap();

        let err = extract_class_metadata(&file, "bt@stdlib@timer")
            .err()
            .unwrap();
        assert!(err.to_string().contains("Timer"));
    }

    #[test]
    fn test_extract_class_metadata_parallel_marks_spawning_class_methods() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("parallel.bt");
        fs::write(
            &file,
            "Object subclass: Parallel\n  \
             class all: blocks => nil\n  \
             class all: blocks timeout: ms => nil\n  \
             class any: blocks => nil\n",
        )
        .unwrap();

        let meta = extract_class_metadata(&file, "bt@stdlib@parallel").unwrap();
        assert!(meta.class_methods.iter().all(|m| m.spawns_block));
    }

    #[test]
    fn test_extract_class_metadata_collection_marks_spawning_methods() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("collection.bt");
        fs::write(
            &file,
            "Object subclass: Collection\n  \
             parallelCollect: block => nil\n  \
             parallelCollect: block maxConcurrency: n => nil\n",
        )
        .unwrap();

        let meta = extract_class_metadata(&file, "bt@stdlib@collection").unwrap();
        assert!(meta.methods.iter().all(|m| m.spawns_block));
    }

    // --- BT-3351: synthesize_value_auto_methods (via extract_class_metadata) ---

    #[test]
    fn test_extract_class_metadata_value_class_generates_all_auto_methods() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("Wrapper.bt");
        fs::write(
            &file,
            "Value subclass: Wrapper\n  field: value :: Integer = 0\n",
        )
        .unwrap();

        let meta = extract_class_metadata(&file, "bt@stdlib@wrapper").unwrap();

        let getter = meta
            .methods
            .iter()
            .find(|m| m.selector == "value")
            .expect("auto getter should be generated");
        assert_eq!(getter.arity, 0);
        assert_eq!(getter.return_type, Some(DeclaredType::simple("Integer")));

        let setter = meta
            .methods
            .iter()
            .find(|m| m.selector == "withValue:")
            .expect("auto functional updater should be generated");
        assert_eq!(setter.arity, 1);
        assert_eq!(setter.return_type, Some(DeclaredType::simple("Wrapper")));

        let ctor = meta
            .class_methods
            .iter()
            .find(|m| m.selector == "value:")
            .expect("auto keyword constructor should be generated");
        assert_eq!(ctor.arity, 1);
        assert_eq!(ctor.return_type, Some(DeclaredType::simple("Wrapper")));
    }

    #[test]
    fn test_extract_class_metadata_value_class_skips_auto_methods_user_already_defines() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("Point2.bt");
        fs::write(
            &file,
            "Value subclass: Point2\n  \
             field: x :: Integer = 0\n  \
             field: y :: Integer = 0\n  \
             x => 42\n  \
             withY: v => v\n  \
             class x: ax y: ay => nil\n",
        )
        .unwrap();

        let meta = extract_class_metadata(&file, "bt@stdlib@point2").unwrap();

        // User-defined "x" getter: not duplicated by the auto-getter.
        assert_eq!(meta.methods.iter().filter(|m| m.selector == "x").count(), 1);
        // User-defined "withY:" setter: not duplicated by the auto-updater.
        assert_eq!(
            meta.methods
                .iter()
                .filter(|m| m.selector == "withY:")
                .count(),
            1
        );
        // User-defined "x:y:" constructor: not duplicated by the auto-constructor.
        assert_eq!(
            meta.class_methods
                .iter()
                .filter(|m| m.selector == "x:y:")
                .count(),
            1
        );
        // The fields NOT covered by a user override still get their auto method.
        assert!(meta.methods.iter().any(|m| m.selector == "withX:"));
        assert!(meta.methods.iter().any(|m| m.selector == "y"));
    }

    // --- BT-3351: is_protocol_only_file ---

    #[test]
    fn test_is_protocol_only_file_true_for_protocol_only_source() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("printable.bt");
        fs::write(&file, "Protocol define: Printable\n  asString -> String\n").unwrap();
        assert!(is_protocol_only_file(&file).unwrap());
    }

    #[test]
    fn test_is_protocol_only_file_false_for_class_source() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("Foo.bt");
        fs::write(&file, "Object subclass: Foo\n  noop => nil\n").unwrap();
        assert!(!is_protocol_only_file(&file).unwrap());
    }

    #[test]
    fn test_is_protocol_only_file_false_on_parse_errors() {
        let (_temp, dir) = temp_utf8_dir();
        let file = dir.join("Broken.bt");
        // Conservative fallback: a file with parse errors is treated as a
        // normal class file so `extract_class_metadata` reports the real
        // error, rather than being silently misclassified as protocol-only.
        fs::write(&file, "Object subclass: ###!!!(((\n").unwrap();
        assert!(!is_protocol_only_file(&file).unwrap());
    }

    #[test]
    fn test_is_protocol_only_file_errors_when_file_missing() {
        let (_temp, dir) = temp_utf8_dir();
        let missing = dir.join("Missing.bt");
        assert!(is_protocol_only_file(&missing).is_err());
    }

    // --- BT-3351: collect_stdlib_protocol_infos non-fatal read errors ---

    #[test]
    fn test_collect_stdlib_protocol_infos_skips_unreadable_file() {
        let (_temp, dir) = temp_utf8_dir();
        let missing = dir.join("Missing.bt");
        let real = dir.join("printable.bt");
        fs::write(&real, "Protocol define: Printable\n  asString -> String\n").unwrap();

        let infos = collect_stdlib_protocol_infos(&[missing, real]);
        assert_eq!(infos.len(), 1);
        assert_eq!(infos[0].name.as_str(), "Printable");
    }

    // --- BT-3351: alias_source_texts_sorted_by_name ---

    #[test]
    fn test_alias_source_texts_sorted_by_name_sorts_by_alias_name_not_raw_text() {
        let (_temp, lib_dir) = temp_utf8_dir();
        let file = lib_dir.join("Fixture.bt");
        // Raw-text sort would put "internal type Zebra" before "type Alpha"
        // (`i` < `t`) — the opposite of sorting by alias *name*.
        fs::write(
            &file,
            "internal type Zebra = Integer\ntype Alpha = String\n\
             Object subclass: Fixture\n  noop => nil\n",
        )
        .unwrap();

        let alias_sources = collect_stdlib_alias_sources(&[file]).unwrap();
        assert_eq!(alias_sources.len(), 2);

        let sorted_texts = alias_source_texts_sorted_by_name(alias_sources);
        assert_eq!(sorted_texts[0], "type Alpha = String");
        assert_eq!(sorted_texts[1], "internal type Zebra = Integer");
    }

    // --- BT-3351: generate_class_entry multi-item fields (separator branches) ---

    #[test]
    fn test_generate_class_entry_multi_item_fields_use_separators() {
        let meta = ClassMeta {
            module_name: "bt@stdlib@pair".to_string(),
            class_name: "Pair".to_string(),
            superclass_name: "Object".to_string(),
            modifiers: ClassModifiers::default(),
            class_kind: beamtalk_core::ast::ClassKind::Object,
            state: vec!["first".to_string(), "second".to_string()],
            state_types: vec![
                ("first".to_string(), DeclaredType::simple("Integer")),
                ("second".to_string(), DeclaredType::simple("String")),
            ],
            state_has_default: vec![("first".to_string(), true), ("second".to_string(), false)],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec!["counterA".to_string(), "counterB".to_string()],
            type_params: vec!["T".to_string(), "U".to_string()],
            superclass_type_args: vec![DeclaredType::simple("T"), DeclaredType::simple("Concrete")],
            handle_scope: None,
        };
        let mut code = String::new();
        generate_class_entry(&mut code, &meta);

        assert!(code.contains(r#"state: vec!["first".into(), "second".into()]"#));
        assert!(code.contains(
            r#"state_types: HashMap::from([("first".into(), DeclaredType::simple("Integer")), ("second".into(), DeclaredType::simple("String"))])"#
        ));
        assert!(code.contains(
            r#"state_has_default: HashMap::from([("first".into(), true), ("second".into(), false)])"#
        ));
        assert!(code.contains(r#"class_variables: vec!["counterA".into(), "counterB".into()]"#));
        assert!(code.contains(r#"type_params: vec!["T".into(), "U".into()]"#));
        assert!(code.contains("type_param_bounds: vec![None, None]"));
        assert!(code.contains(
            r#"superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 0 }, SuperclassTypeArg::Concrete { declared: DeclaredType::simple("Concrete") }]"#
        ));
    }

    // --- BT-3351: declared_type_to_rust_expr — every DeclaredType variant ---

    #[test]
    fn test_declared_type_to_rust_expr_covers_every_variant() {
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::Singleton("nil".into())),
            r#"DeclaredType::singleton("nil")"#
        );
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::union(vec![
                DeclaredType::simple("Integer"),
                DeclaredType::simple("Float"),
            ])),
            r#"DeclaredType::union(vec![DeclaredType::simple("Integer"), DeclaredType::simple("Float")])"#
        );
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::generic(
                "List",
                vec![DeclaredType::simple("Integer")]
            )),
            r#"DeclaredType::generic("List", vec![DeclaredType::simple("Integer")])"#
        );
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::FalseOr(Box::new(DeclaredType::simple(
                "Integer"
            )))),
            r#"DeclaredType::FalseOr(Box::new(DeclaredType::simple("Integer")))"#
        );
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::Difference {
                base: Box::new(DeclaredType::simple("Object")),
                excluded: Box::new(DeclaredType::simple("Nil")),
            }),
            r#"DeclaredType::Difference { base: Box::new(DeclaredType::simple("Object")), excluded: Box::new(DeclaredType::simple("Nil")) }"#
        );
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::Intersection {
                left: Box::new(DeclaredType::simple("A")),
                right: Box::new(DeclaredType::simple("B")),
            }),
            r#"DeclaredType::Intersection { left: Box::new(DeclaredType::simple("A")), right: Box::new(DeclaredType::simple("B")) }"#
        );
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::SelfType),
            "DeclaredType::SelfType"
        );
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::SelfClass),
            "DeclaredType::SelfClass"
        );
        assert_eq!(
            declared_type_to_rust_expr(&DeclaredType::ClassOf("Counter".into())),
            r#"DeclaredType::ClassOf("Counter".into())"#
        );
    }

    // --- BT-3351: generate_method_list doc-string escaping ---

    #[test]
    fn test_generate_method_list_escapes_doc_special_characters() {
        let methods = vec![MethodMeta {
            selector: "help".to_string(),
            arity: 0,
            kind: MethodKindMeta::Primary,
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![],
            doc: Some("Line one\nLine \"two\"\tend\r".to_string()),
        }];
        let mut code = String::new();
        generate_method_list(&mut code, "methods", &methods, "Help");

        let expected = r#"doc: Some("Line one\nLine \"two\"\tend\r".into())"#;
        assert!(code.contains(expected), "Got: {code}");
    }
}
