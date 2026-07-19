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
use crate::commands::util;
use beamtalk_core::semantic_analysis::alias_registry::AliasRegistry;
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

    // Incremental build: skip if all outputs are newer than all inputs
    if is_stdlib_up_to_date(&ebin_dir, &source_files) {
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
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: true,
        allow_primitives: false,
        workspace_mode: false,
        warnings_as_errors,
        ..Default::default()
    };

    // BT-295 / ADR 0007 Phase 3: Build primitive binding table from ALL stdlib sources.
    // This is used during compilation so that @primitive expressions in method bodies
    // can reference the runtime dispatch modules.
    info!("Building primitive binding table from stdlib sources");
    let bindings =
        beamtalk_core::erlang::primitive_bindings::load_from_directory(lib_dir.as_std_path());
    info!(
        binding_count = bindings.len(),
        "Loaded primitive bindings from stdlib"
    );

    // Extract native type specs from runtime .beam files so FFI calls in stdlib
    // get proper type inference instead of Dynamic (ADR 0075).
    let native_type_registry = extract_stdlib_type_specs();

    // BT-2935: live same-run alias pre-pass — see its doc for why.
    let alias_sources = collect_stdlib_alias_sources(&source_files)?;
    let compile_ctx = CompileContext {
        native_type_registry: native_type_registry.map(std::sync::Arc::new),
        hierarchy: ClassHierarchyContext {
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

    generate_app_file(&ebin_dir, &source_files, &class_metadata, &protocol_modules)?;
    // Also update .app.src so rebar3 picks up the classes env
    let app_src_dir = Utf8PathBuf::from("runtime/apps/beamtalk_stdlib/src");
    if app_src_dir.exists() {
        generate_app_src_file(&app_src_dir, &class_metadata, &protocol_modules)?;
    }

    // Generate Rust builtins file from parsed class metadata and aliases.
    generate_builtins_rs(
        &class_metadata,
        &alias_source_texts_sorted_by_name(alias_sources),
    )?;

    println!("Built {} stdlib modules", source_files.len());

    Ok(())
}

/// Compiles every stdlib source file to Core Erlang, collecting class
/// metadata and protocol module names along the way.
///
/// Protocol-only files (e.g. `Printable.bt`) have no class definition — they
/// are still compiled so the protocol gets registered at runtime, but
/// contribute no `ClassMeta`. Files are independent (no compile ordering
/// required); cross-file class/alias visibility comes entirely from
/// `compile_ctx` (built by the caller before this runs).
fn compile_all_stdlib_files(
    source_files: &[Utf8PathBuf],
    temp_path: &Utf8Path,
    quiet: bool,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
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

/// Check if the stdlib build is up to date (all outputs newer than all inputs).
///
/// Inputs: `lib/*.bt` source files, the compiler binary (`current_exe`), and
/// runtime `.beam` files in `runtime/_build/default/lib/beamtalk_runtime/ebin/`.
/// Output: the `ebin/` directory modification time.
///
/// Returns `false` (needs rebuild) if any input is missing or any error occurs.
fn is_stdlib_up_to_date(ebin_dir: &Utf8Path, source_files: &[Utf8PathBuf]) -> bool {
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
    // Use layout-aware discovery so invalidation matches spec extraction paths.
    let runtime_ebins: Vec<std::path::PathBuf> = {
        use beamtalk_cli::repl_startup;
        if let Ok((runtime_dir, layout)) = repl_startup::find_runtime_dir_with_layout() {
            let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);
            vec![
                paths.runtime_ebin,
                paths.stdlib_erlang_ebin,
                paths.workspace_ebin,
                paths.compiler_ebin,
            ]
        } else {
            // Can't find runtime — force rebuild to be safe.
            return false;
        }
    };
    for runtime_ebin in &runtime_ebins {
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

/// Find all `.bt` files in the stdlib source directory.
fn find_stdlib_files(lib_dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    util::find_files(lib_dir, &["bt"])
}

/// Extract the module name from a `.bt` file path.
///
/// ADR 0016: All stdlib classes use `bt@stdlib@{snake_case}` prefix.
/// The `@` separator is legal in unquoted Erlang atoms and follows
/// the Gleam convention (`gleam@list`, `gleam@string`).
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

    let snake = beamtalk_core::codegen::core_erlang::to_module_name(stem);

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
    bindings: &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable,
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
    /// Declared type annotations for state fields (field name → type name).
    state_types: Vec<(String, String)>,
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
    superclass_type_args: Vec<String>,
    /// Declared sendability handle scope (ADR 0103, `handleScope: #symbol`).
    /// The bare symbol text (e.g. `"process"`), or `None` when undeclared.
    handle_scope: Option<String>,
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
    return_type: Option<String>,
    /// Parameter type annotations, one per parameter. `None` means untyped.
    param_types: Vec<Option<String>>,
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
        return_type: m.return_type.as_ref().map(|t| t.type_name().to_string()),
        param_types: m
            .parameters
            .iter()
            .map(|p| {
                p.type_annotation
                    .as_ref()
                    .map(|t| t.type_name().to_string())
            })
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
                return_type: slot
                    .type_annotation
                    .as_ref()
                    .map(|t| t.type_name().to_string()),
                param_types: vec![],
                doc: Some(getter_doc),
            });
        }

        // Auto functional updater: `withFieldName:` → Self type
        let with_sel = {
            let mut chars = slot_name.chars();
            match chars.next() {
                None => "with:".to_string(),
                Some(first) => {
                    let cap: String = first.to_uppercase().collect();
                    format!("with{}{}:", cap, chars.as_str())
                }
            }
        };
        if !user_selectors.contains(&with_sel) {
            let param_type = slot
                .type_annotation
                .as_ref()
                .map(|t| t.type_name().to_string());
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
                return_type: Some(class_name.to_string()),
                param_types: vec![param_type],
                doc: Some(setter_doc),
            });
        }
    }

    // Auto keyword constructor on the class side: `field1:field2:...`
    let kw_sel: String = class.state.iter().fold(String::new(), |mut acc, s| {
        acc.push_str(s.name.name.as_str());
        acc.push(':');
        acc
    });
    if !user_class_selectors.contains(&kw_sel) {
        let arity = class.state.len();
        let param_types = class
            .state
            .iter()
            .map(|s| {
                s.type_annotation
                    .as_ref()
                    .map(|t| t.type_name().to_string())
            })
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
            return_type: Some(class_name.to_string()),
            param_types,
            doc: Some(ctor_doc),
        });
    }
}

/// Mark Timer class methods that spawn their block argument in a separate BEAM process.
///
/// BT-1312: replaces hardcoded list in `validators.rs` so the self-capture validator
/// can skip false-positive warnings.
fn mark_timer_spawns(class_methods: &mut [MethodMeta]) -> Result<()> {
    let mut found_after = false;
    let mut found_every = false;
    for m in class_methods.iter_mut() {
        match m.selector.as_str() {
            "after:do:" => {
                m.spawns_block = true;
                found_after = true;
            }
            "every:do:" => {
                m.spawns_block = true;
                found_every = true;
            }
            _ => {}
        }
    }
    if !found_after || !found_every {
        miette::bail!(
            "Timer metadata mismatch: expected class methods `after:do:` and `every:do:` \
             but found_after={found_after}, found_every={found_every}"
        );
    }
    Ok(())
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
/// Protocol-only files (e.g. `Printable.bt`) define structural protocols via
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
                .map(|ty| (s.name.name.to_string(), ty.type_name().to_string()))
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

    let type_params = class
        .type_params
        .iter()
        .map(|tp| tp.name.name.to_string())
        .collect();

    let superclass_type_args = class
        .superclass_type_args
        .iter()
        .map(|ty| ty.type_name().to_string())
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
fn format_stdlib_classes_list(class_metadata: &[ClassMeta], separator: &str) -> String {
    class_metadata
        .iter()
        .map(format_stdlib_class_entry)
        .collect::<Vec<_>>()
        .join(separator)
}

/// Generate the `beamtalk_stdlib.app` file in the ebin directory.
///
/// Lists all modules and embeds class hierarchy metadata in the `env` section.
/// The metadata is used by `beamtalk_stdlib` to load modules in dependency order.
///
/// **No `type_aliases` env key (ADR 0108 Phase 8, BT-2903):** unlike
/// [`super::app_file::generate_app_file`] (the real `beamtalk build`
/// pipeline, which now emits `{type_aliases, [...]}` via
/// [`super::build::build_alias_metadata`]), this stdlib-specific writer does
/// not — stdlib itself declares no `type` aliases yet. When the ADR's
/// "Stdlib follow-through" work lands (`RestartStrategy`, `JsonValue`,
/// tracked against BT-2618/BT-2827 in the ADR), this function needs the same
/// `build_alias_metadata`/`format_type_aliases_entry` wiring, or
/// `browse-type-aliases` will never show stdlib's own aliases.
fn generate_app_file(
    ebin_dir: &Utf8Path,
    source_files: &[Utf8PathBuf],
    class_metadata: &[ClassMeta],
    protocol_modules: &[String],
) -> Result<()> {
    let module_names: Vec<String> = source_files
        .iter()
        .map(|f| module_name_from_path(f))
        .collect::<Result<_>>()?;

    let modules_list = module_names
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    // ADR 0070 Phase 4: Generate extended class hierarchy entries for env
    let classes_list = format_stdlib_classes_list(class_metadata, ",\n                    ");

    // BT-1766: Protocol-only modules need to be loaded separately during stdlib init
    let protocol_modules_list = protocol_modules
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    let version = env!("BEAMTALK_VERSION");
    let app_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, \"{version}\"}},\n\
         \x20   {{modules, [{modules_list}]}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, beamtalk_runtime]}},\n\
         \x20   {{env, [\n\
         \x20       {{classes, [{classes_list}]}},\n\
         \x20       {{protocol_modules, [{protocol_modules_list}]}}\n\
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
/// the `{classes, [...]}` env for the runtime to read via `application:get_env`.
fn generate_app_src_file(
    src_dir: &Utf8Path,
    class_metadata: &[ClassMeta],
    protocol_modules: &[String],
) -> Result<()> {
    // ADR 0070 Phase 4: Generate extended class hierarchy entries for env
    let classes_list = format_stdlib_classes_list(class_metadata, ",\n            ");

    // BT-1766: Protocol-only modules need to be loaded separately during stdlib init
    let protocol_modules_list = protocol_modules
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ");

    let app_src_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, {{cmd, \"escript ../../../scripts/version.escript\"}}}},\n\
         \x20   {{modules, []}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, beamtalk_runtime]}},\n\
         \x20   {{env, [\n\
         \x20       {{classes, [\n\
         \x20           {classes_list}\n\
         \x20       ]}},\n\
         \x20       {{protocol_modules, [{protocol_modules_list}]}}\n\
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
         use super::super::{ClassInfo, MethodInfo, SuperclassTypeArg};\n\
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

    let mut sorted_meta: Vec<&ClassMeta> = class_metadata.iter().collect();
    sorted_meta.sort_by_key(|m| &m.class_name);

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

    // Only write if content changed to avoid unnecessary recompilation
    let needs_write = match fs::read_to_string(&dest) {
        Ok(existing) => existing != code,
        Err(_) => true,
    };

    if needs_write {
        fs::write(&dest, &code)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to write '{dest}'"))?;
        debug!("Generated {}", dest);
    } else {
        debug!("Generated builtins unchanged, skipping write");
    }
    Ok(())
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
            let _ = write!(code, "(\"{field}\".into(), \"{ty}\".into())");
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
fn generate_superclass_type_args(code: &mut String, args: &[String], type_params: &[String]) {
    if args.is_empty() {
        code.push_str("            superclass_type_args: vec![],\n");
        return;
    }
    code.push_str("            superclass_type_args: vec![");
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            code.push_str(", ");
        }
        if let Some(idx) = type_params.iter().position(|p| p == arg) {
            let _ = write!(code, "SuperclassTypeArg::ParamRef {{ param_index: {idx} }}");
        } else {
            let escaped = arg.replace('\\', "\\\\").replace('"', "\\\"");
            let _ = write!(
                code,
                "SuperclassTypeArg::Concrete {{ type_name: \"{escaped}\".into() }}"
            );
        }
    }
    code.push_str("],\n");
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
            Some(t) => {
                let escaped = t.replace('\\', "\\\\").replace('"', "\\\"");
                format!("Some(\"{escaped}\".into())")
            }
            None => "None".to_string(),
        };
        let param_types_expr = if m.param_types.is_empty() {
            "vec![]".to_string()
        } else {
            let parts: Vec<_> = m
                .param_types
                .iter()
                .map(|p| match p {
                    Some(t) => {
                        let escaped = t.replace('\\', "\\\\").replace('"', "\\\"");
                        format!("Some(\"{escaped}\".into())")
                    }
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

        let pre_loaded_aliases: Vec<_> = alias_sources
            .iter()
            .map(|a| {
                let mut info = a.info.clone();
                info.package = Some("stdlib".into());
                info
            })
            .collect();
        assert_eq!(pre_loaded_aliases.len(), 1);
        assert_eq!(pre_loaded_aliases[0].name.as_str(), "Direction");

        let options = beamtalk_core::CompilerOptions {
            stdlib_mode: true,
            allow_primitives: false,
            workspace_mode: false,
            ..Default::default()
        };
        let bindings = beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new();

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

        fs::write(lib_dir.join("Integer.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("String.bt"), "// stub").unwrap();
        fs::write(lib_dir.join("README.md"), "not a bt file").unwrap();

        let files = find_stdlib_files(&lib_dir).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("Integer.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("String.bt")));
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
        let path = Utf8PathBuf::from("lib/Integer.bt");
        assert_eq!(module_name_from_path(&path).unwrap(), "bt@stdlib@integer");
    }

    #[test]
    fn test_module_name_from_path_non_primitive() {
        let path = Utf8PathBuf::from("lib/BeamtalkInterface.bt");
        assert_eq!(
            module_name_from_path(&path).unwrap(),
            "bt@stdlib@beamtalk_interface"
        );
    }

    #[test]
    fn test_module_name_from_path_multi_word() {
        let path = Utf8PathBuf::from("lib/ProtoObject.bt");
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
            Utf8PathBuf::from("lib/Integer.bt"),
            Utf8PathBuf::from("lib/String.bt"),
        ];

        generate_app_file(&ebin_dir, &source_files, &[], &[]).unwrap();

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

        generate_app_file(&ebin_dir, &[], &[], &[]).unwrap();

        let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
        assert!(content.contains("{modules, []}"));
    }

    #[test]
    fn test_generate_app_file_invalid_name_errors() {
        let (_temp, ebin_dir) = temp_utf8_dir();

        let source_files = vec![
            Utf8PathBuf::from("lib/Integer.bt"),
            Utf8PathBuf::from("lib/my-bad-name.bt"),
        ];

        let result = generate_app_file(&ebin_dir, &source_files, &[], &[]);
        assert!(result.is_err());
    }

    #[test]
    fn test_generate_app_file_with_protocol_modules() {
        let (_temp, ebin_dir) = temp_utf8_dir();

        let source_files = vec![Utf8PathBuf::from("lib/Integer.bt")];
        let protocol_modules = vec!["bt@stdlib@printable".to_string()];

        generate_app_file(&ebin_dir, &source_files, &[], &protocol_modules).unwrap();

        let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
        assert!(
            content.contains("{protocol_modules, ['bt@stdlib@printable']}"),
            "Should contain protocol_modules env key. Got:\n{content}"
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
                    return_type: Some("Counter".to_string()),
                    param_types: vec![Some("Integer".to_string())],
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
                return_type: Some("Counter".to_string()),
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
        generate_superclass_type_args(&mut code, &["E".to_string()], &["E".to_string()]);
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
            &["V".to_string()],
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
        generate_superclass_type_args(&mut code, &["Integer".to_string()], &[]);
        assert!(
            code.contains(
                "superclass_type_args: vec![SuperclassTypeArg::Concrete { type_name: \"Integer\".into() }]"
            ),
            "Non-type-param args should emit Concrete. Got: {code}"
        );
    }

    #[test]
    fn test_generate_superclass_type_args_concrete_escaped() {
        // Guard against unescaped quotes/backslashes breaking the generated Rust.
        let mut code = String::new();
        generate_superclass_type_args(&mut code, &["My\\\"Type".to_string()], &["E".to_string()]);
        assert!(
            code.contains("SuperclassTypeArg::Concrete { type_name: \"My\\\\\\\"Type\".into() }"),
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
        let mut sorted: Vec<&ClassMeta> = meta.iter().collect();
        sorted.sort_by_key(|m| &m.class_name);

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
            return_type: Some("Counter".to_string()),
            param_types: vec![Some("Integer".to_string())],
            doc: None,
        }];
        let mut code = String::new();
        generate_method_list(&mut code, "methods", &methods, "Counter");
        assert!(
            code.contains("return_type: Some(\"Counter\".into())"),
            "Should emit return type. Got: {code}"
        );
        assert!(
            code.contains("param_types: vec![Some(\"Integer\".into())]"),
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
            param_types: vec![Some("Integer".to_string()), None],
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
            return_type: Some("Dictionary".to_string()),
            param_types: vec![Some("Symbol".to_string())],
            doc: None,
        }];
        let mut code = String::new();
        generate_method_list(&mut code, "methods", &methods, "SystemNavigation");
        assert!(
            code.contains("is_internal: true"),
            "Should emit is_internal: true for an internal method. Got: {code}"
        );
    }
}
