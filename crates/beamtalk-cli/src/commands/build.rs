// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build beamtalk projects.

use crate::beam_compiler::{
    BeamCompiler, ClassHierarchyContext, CompileContext, compile_source_with_bindings,
};
use crate::commands::build_layout::BuildLayout;
use beamtalk_core::file_walker::FileWalker;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fs;
use std::path::PathBuf;
use std::time::SystemTime;
use tracing::{debug, error, info, instrument, warn};

use super::app_file;
use super::manifest;
use super::manifest::NativeDependencyMap;

/// Result of per-file change detection.
///
/// Compares each `.bt` source file's modification time against its corresponding
/// `.beam` output to determine which files need recompilation.
#[derive(Debug)]
#[allow(clippy::struct_field_names)] // `_files` postfix is clearer for this domain struct
pub(crate) struct ChangeDetectionResult {
    /// Source files that need (re)compilation — either modified since last build,
    /// newly added (no `.beam` exists), or included because of `--force`.
    pub changed_files: Vec<Utf8PathBuf>,
    /// Source files whose `.beam` output is up-to-date.
    pub unchanged_files: Vec<Utf8PathBuf>,
    /// `.beam` files in the build directory with no corresponding `.bt` source.
    /// These are reported as warnings but not deleted (users may have manually
    /// placed files or renamed sources intentionally).
    pub orphaned_beam_files: Vec<Utf8PathBuf>,
}

/// Detect which source files have changed relative to their compiled `.beam` output.
///
/// For each `.bt` source file, computes the expected `.beam` filename in `build_dir`
/// using the same module naming scheme as the build pipeline. A file is considered
/// changed if:
/// - Its `.beam` does not exist (new file or first build)
/// - Its mtime is newer than the `.beam` mtime
///
/// Also detects orphaned `.beam` files (no corresponding `.bt` source) and reports
/// them as warnings.
///
/// When `force` is true, all source files are treated as changed regardless of mtime.
pub(crate) fn detect_changes(
    source_files: &[Utf8PathBuf],
    build_dir: &Utf8Path,
    file_module_pairs: &[(Utf8PathBuf, String, Utf8PathBuf)],
    force: bool,
) -> ChangeDetectionResult {
    if force {
        info!("Force build requested — all files will be recompiled");
        // Still detect orphaned .beam files so users get consistent warnings
        let expected_beam_filenames: HashSet<String> = file_module_pairs
            .iter()
            .map(|(_, module_name, _)| format!("{module_name}.beam"))
            .collect();
        let orphaned_beam_files = detect_orphaned_beams(build_dir, &expected_beam_filenames);
        return ChangeDetectionResult {
            changed_files: source_files.to_vec(),
            unchanged_files: Vec::new(),
            orphaned_beam_files,
        };
    }

    let mut changed_files = Vec::new();
    let mut unchanged_files = Vec::new();

    // Build a set of expected .beam filenames from source files
    let mut expected_beam_stems: HashSet<String> = HashSet::new();

    for (source_file, module_name, _core_file) in file_module_pairs {
        let beam_file = build_dir.join(format!("{module_name}.beam"));
        expected_beam_stems.insert(format!("{module_name}.beam"));

        if !beam_file.exists() {
            debug!(file = %source_file, "No .beam output — needs compilation");
            changed_files.push(source_file.clone());
            continue;
        }

        // Compare mtimes: source newer than beam → needs recompilation
        let source_mtime = mtime_of(source_file);
        let beam_mtime = mtime_of(&beam_file);

        match (source_mtime, beam_mtime) {
            (Some(src_t), Some(beam_t)) if src_t > beam_t => {
                debug!(file = %source_file, "Source newer than .beam — needs recompilation");
                changed_files.push(source_file.clone());
            }
            (Some(_), Some(_)) => {
                debug!(file = %source_file, "Up-to-date");
                unchanged_files.push(source_file.clone());
            }
            _ => {
                // If we can't read mtimes, err on the side of recompilation
                debug!(file = %source_file, "Cannot read mtime — needs recompilation");
                changed_files.push(source_file.clone());
            }
        }
    }

    // Detect orphaned .beam files (exist in build dir but no corresponding source)
    let orphaned_beam_files = detect_orphaned_beams(build_dir, &expected_beam_stems);

    let total = changed_files.len() + unchanged_files.len();
    info!(
        changed = changed_files.len(),
        unchanged = unchanged_files.len(),
        orphaned = orphaned_beam_files.len(),
        total,
        "Change detection complete"
    );

    ChangeDetectionResult {
        changed_files,
        unchanged_files,
        orphaned_beam_files,
    }
}

/// Read the modification time of a file, returning `None` on any error.
fn mtime_of(path: &Utf8Path) -> Option<SystemTime> {
    fs::metadata(path).ok()?.modified().ok()
}

/// Find `bt@*` `.beam` files in the build directory that are not in the expected set.
fn detect_orphaned_beams(
    build_dir: &Utf8Path,
    expected_beam_filenames: &HashSet<String>,
) -> Vec<Utf8PathBuf> {
    let Ok(entries) = fs::read_dir(build_dir) else {
        return Vec::new();
    };

    let mut orphaned = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };

        // Only consider bt@* beam files (user/package modules)
        if name.starts_with("bt@")
            && path.extension().is_some_and(|ext| ext == "beam")
            && !expected_beam_filenames.contains(name)
        {
            if let Ok(utf8) = Utf8PathBuf::from_path_buf(path) {
                orphaned.push(utf8);
            }
        }
    }

    orphaned
}

/// Resolved environment for a build: source files, project root, manifest, and
/// build directory. Produced by [`setup_build_environment`].
struct BuildEnvironment {
    /// All `.bt` source files discovered for this build.
    source_files: Vec<Utf8PathBuf>,
    /// The project root directory (directory input or parent of file input).
    project_root: Utf8PathBuf,
    /// The fully parsed manifest, if `beamtalk.toml` exists.
    full_manifest: Option<manifest::ParsedManifest>,
    /// The build layout helper for computing standard paths.
    layout: BuildLayout,
    /// The output directory for compiled artifacts (ebin/ or build/).
    build_dir: Utf8PathBuf,
    /// The `src/` directory if it exists, used for relative module path computation.
    source_root: Option<Utf8PathBuf>,
}

impl BuildEnvironment {
    /// Convenience accessor for the package manifest, if present.
    fn pkg_manifest(&self) -> Option<&manifest::PackageManifest> {
        self.full_manifest.as_ref().map(|m| &m.package)
    }
}

/// Resolved dependencies and their associated metadata. Produced by
/// [`resolve_and_validate_dependencies`].
struct DependencyContext {
    /// The resolved dependency list (empty for non-package builds).
    resolved_deps: Vec<super::deps::path::ResolvedDependency>,
    /// Whether native hex dependencies exist in the dependency graph.
    has_native_deps: bool,
}

/// Results from the two compilation passes (class index + source compilation).
/// Produced by [`execute_build_passes`].
struct BuildPassesResult {
    /// All module names (both compiled and unchanged), needed for .app generation.
    module_names: Vec<String>,
    /// File-to-module mapping for all source files.
    file_module_pairs: Vec<(Utf8PathBuf, String, Utf8PathBuf)>,
    /// Class hierarchy context used for post-processing (app file generation, etc.).
    hierarchy: ClassHierarchyContext,
    /// Native compilation result, if native Erlang sources were compiled.
    native_result: Option<Rebar3Result>,
}

/// Build beamtalk source files.
///
/// This command compiles .bt files to .beam bytecode via Core Erlang.
///
/// When `force` is false, per-file change detection compares `.bt` source mtimes
/// against `.beam` output mtimes and only recompiles changed files.
#[instrument(skip_all, fields(path = %path))]
pub fn build(path: &str, options: &beamtalk_core::CompilerOptions, force: bool) -> Result<()> {
    info!("Starting build");

    let env = setup_build_environment(path)?;
    let dep_ctx = resolve_and_validate_dependencies(&env, options)?;
    let passes = execute_build_passes(&env, options, &dep_ctx, force)?;
    post_process_package_artifacts(&env, &dep_ctx, &passes)?;

    // ADR 0075 Phase 1: Extract type specs from OTP .beam files on the code
    // path. Results are cached in _build/type_cache/ — incremental builds
    // read zero .beam files when the cache is fresh.
    extract_type_specs(&env);

    Ok(())
}

/// Phase 1-3: Discover source files, resolve the project root and manifest,
/// and create the build output directory.
fn setup_build_environment(path: &str) -> Result<BuildEnvironment> {
    let source_path = Utf8PathBuf::from(path);

    // Find source files
    let source_files = find_source_files(&source_path)?;

    if source_files.is_empty() {
        error!("No .bt source files found in '{}'", path);
        miette::bail!("No .bt source files found in '{path}'");
    }

    info!(count = source_files.len(), "Found source files");
    debug!("Building {} file(s)", source_files.len());

    // Determine project root (for directory input, use the path; for file input, use parent)
    let project_root = if source_path.is_dir() {
        source_path.clone()
    } else {
        source_path
            .parent()
            .map_or_else(|| Utf8PathBuf::from("."), Utf8Path::to_path_buf)
    };

    // Look for package manifest (full parse includes dependencies for
    // transitive dep tracking — ADR 0070 Phase 3)
    let full_manifest = manifest::find_manifest_full(&project_root)?;
    if let Some(pkg) = full_manifest.as_ref().map(|m| &m.package) {
        info!(name = %pkg.name, version = %pkg.version, "Found package manifest");
        debug!(?pkg, "Package manifest details");
    } else {
        debug!("No beamtalk.toml found, using default behavior");
    }

    // Create build directory relative to project root
    // ADR 0026 §5: Package mode outputs to _build/dev/ebin/, single-file mode keeps build/
    let layout = BuildLayout::new(&project_root);
    let build_dir = if full_manifest.is_some() {
        layout.ebin_dir()
    } else {
        project_root.join("build")
    };

    debug!("Creating build directory: {}", build_dir);
    std::fs::create_dir_all(&build_dir)
        .into_diagnostic()
        .wrap_err("Failed to create build directory")?;

    // Determine the source root for computing relative module paths
    let src_dir = project_root.join("src");
    let source_root = if src_dir.exists() {
        Some(src_dir)
    } else {
        None
    };

    Ok(BuildEnvironment {
        source_files,
        project_root,
        full_manifest,
        layout,
        build_dir,
        source_root,
    })
}

/// Phase 4: Resolve transitive dependencies, check for native module collisions,
/// and determine whether native hex dependencies exist in the graph.
fn resolve_and_validate_dependencies(
    env: &BuildEnvironment,
    options: &beamtalk_core::CompilerOptions,
) -> Result<DependencyContext> {
    // ADR 0070 Phase 1: Resolve and compile dependencies when needed.
    // Uses staleness detection: no-op when lockfile is fresh and deps are compiled.
    // Otherwise resolves the full transitive graph in topological order.
    let resolved_deps = if env.pkg_manifest().is_some() {
        super::deps::ensure_deps_resolved(&env.project_root, options)?
    } else {
        Vec::new()
    };

    // ADR 0072 Phase 1: Check for native Erlang module name collisions across
    // packages before compilation. BEAM has a flat module namespace — only one
    // version of any module can be loaded, so duplicates must be caught early.
    if let Some(pkg) = env.pkg_manifest() {
        check_native_module_collisions(&env.project_root, &pkg.name, &resolved_deps)?;
    }

    // ADR 0072: Determine whether native hex deps exist (needed to choose
    // the compilation path later). The actual native compilation is deferred
    // until after Pass 1 so we can generate the beamtalk_classes.hrl header
    // (BT-1730) that native .erl files can include.
    let has_native_deps = env
        .full_manifest
        .as_ref()
        .is_some_and(|m| !m.native_dependencies.is_empty())
        || resolved_deps
            .iter()
            .try_fold(false, |acc, dep| -> Result<bool> {
                let manifest_path = dep.root.join("beamtalk.toml");
                if !manifest_path.exists() {
                    return Ok(acc);
                }
                let m = manifest::parse_manifest_full(&manifest_path).wrap_err_with(|| {
                    format!("Failed to parse dependency manifest at {manifest_path}")
                })?;
                Ok(acc || !m.native_dependencies.is_empty())
            })?;

    Ok(DependencyContext {
        resolved_deps,
        has_native_deps,
    })
}

/// Results from Pass 1: class index building and dependency merging.
struct ClassIndexResult {
    /// Merged class-to-module index (source + dependency classes).
    class_module_index: HashMap<String, String>,
    /// Class-to-superclass index.
    class_superclass_index: HashMap<String, String>,
    /// Unified collection of all `ClassInfo` from source and dependency classes.
    all_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// Dependency registry for cross-package collision detection.
    dep_registry: beamtalk_core::semantic_analysis::DependencyRegistry,
    /// Cached ASTs from incremental Pass 1, keyed by source file path.
    cached_asts: HashMap<Utf8PathBuf, CachedAst>,
    /// Whether the manifest changed, forcing Pass 2 recompilation.
    force_pass2: bool,
}

/// Phase 5-6: Build the class index (Pass 1) and merge dependency indexes.
///
/// Computes module names for all source files, builds the class-to-module and
/// class-to-superclass indexes, merges dependency class indexes, and validates
/// stdlib reservation violations.
fn build_class_index(
    env: &BuildEnvironment,
    dep_ctx: &DependencyContext,
    options: &beamtalk_core::CompilerOptions,
    force: bool,
) -> Result<ClassIndexResult> {
    let pkg_manifest = env.pkg_manifest();

    // Pass 1: compute module names and build the class → module index.
    // This allows later files to resolve cross-file class references (including
    // classes in package subdirectories) during code generation.
    // BT-1683: Use incremental cache to skip unchanged files in Pass 1.
    let manifest_path = if pkg_manifest.is_some() {
        let p = env.project_root.join("beamtalk.toml");
        if p.exists() { Some(p) } else { None }
    } else {
        None
    };
    let (
        mut class_module_index,
        class_superclass_index,
        source_class_infos,
        cached_asts,
        force_pass2,
    ) = if let Some(pkg) = pkg_manifest {
        let result = super::build_cache::incremental_build_class_module_index(
            &env.source_files,
            env.source_root.as_deref(),
            &pkg.name,
            &env.build_dir,
            manifest_path.as_deref(),
            force,
        )?;
        // If the manifest changed, force Pass 2 recompilation too —
        // .bt files may be unchanged but semantics depend on the manifest.
        (
            result.class_module_index,
            result.class_superclass_index,
            result.all_class_infos,
            result.cached_asts,
            result.manifest_invalidated,
        )
    } else {
        (
            HashMap::new(),
            HashMap::new(),
            Vec::new(),
            HashMap::new(),
            false,
        )
    };

    // ADR 0070: Merge dependency class indexes into the main package's indexes
    // so cross-package class references resolve during compilation.
    // Also build a DependencyRegistry for collision detection (Phase 3).
    // BT-1654: Track direct vs transitive dependencies for W0302 warnings.
    let dep_infos: Vec<beamtalk_core::semantic_analysis::DepInfo> = dep_ctx
        .resolved_deps
        .iter()
        .map(|dep| beamtalk_core::semantic_analysis::DepInfo {
            name: dep.name.clone(),
            class_module_index: dep.class_module_index.clone(),
            is_direct: dep.is_direct,
            via_chain: dep.via_chain.clone(),
        })
        .collect();
    let dep_registry =
        beamtalk_core::semantic_analysis::build_dependency_registry_with_graph(&dep_infos);

    // Collect dependency ClassInfo separately, then merge with source ClassInfo
    // via collect_all_class_infos (BT-1733).
    let mut dep_class_infos = Vec::new();
    for dep in &dep_ctx.resolved_deps {
        for (class_name, module_name) in &dep.class_module_index {
            debug!(
                dep = %dep.name,
                class = %class_name,
                module = %module_name,
                "Adding dependency class to index"
            );
            class_module_index.insert(class_name.clone(), module_name.clone());
        }
        dep_class_infos.extend(dep.class_infos.clone());
    }

    // BT-1733: Single unified collection of all ClassInfo from all sources.
    // To add a new .bt source location, add its ClassInfo slice here.
    let all_class_infos = collect_all_class_infos(&[&source_class_infos, &dep_class_infos]);

    // BT-1653 / ADR 0070 Phase 3: Eagerly check stdlib reservation violations.
    // Dependencies must not export classes with stdlib-reserved names.
    if !dep_registry.is_empty() && !options.stdlib_mode {
        check_stdlib_reservations(&dep_registry)?;
    }

    Ok(ClassIndexResult {
        class_module_index,
        class_superclass_index,
        all_class_infos,
        dep_registry,
        cached_asts,
        force_pass2,
    })
}

/// Check that no dependency exports classes with stdlib-reserved names.
fn check_stdlib_reservations(
    dep_registry: &beamtalk_core::semantic_analysis::DependencyRegistry,
) -> Result<()> {
    let mut reservation_diags = Vec::new();
    beamtalk_core::semantic_analysis::check_stdlib_reservation(
        dep_registry,
        &mut reservation_diags,
    );
    if reservation_diags.is_empty() {
        return Ok(());
    }
    let has_errors = reservation_diags
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        let messages: Vec<String> = reservation_diags
            .iter()
            .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
            .map(|d| {
                if let Some(ref hint) = d.hint {
                    format!("{}\n  help: {}", d.message, hint)
                } else {
                    d.message.to_string()
                }
            })
            .collect();
        miette::bail!("{}", messages.join("\n"));
    }
    Ok(())
}

/// Compile native Erlang sources (native/*.erl and hex dependencies).
///
/// Runs after Pass 1 so the generated `beamtalk_classes.hrl` header is available
/// for native `.erl` files to include. Also validates that native files don't
/// contain hardcoded `bt@<pkg>@` references.
fn compile_native_sources(
    env: &BuildEnvironment,
    dep_ctx: &DependencyContext,
    class_module_index: &HashMap<String, String>,
) -> Result<Option<Rebar3Result>> {
    let pkg_manifest = env.pkg_manifest();

    // BT-1730: Generate beamtalk_classes.hrl before native Erlang compilation.
    // This header provides ?BT_CLASS_MODULE macros so native .erl files can
    // reference Beamtalk class modules without hardcoding bt@<pkg>@<class> atoms.
    if pkg_manifest.is_some() {
        let hrl_dir = env.layout.native_include_dir();
        generate_class_header(&hrl_dir, class_module_index)?;
    }

    // ADR 0072: Compile native Erlang sources after Pass 1.
    // Native modules must be compiled before .bt files (Pass 2) because .bt
    // files may reference them via `(Erlang module)` FFI or `native:` annotations.
    // Moved after Pass 1 so the generated beamtalk_classes.hrl is available.
    let native_result = if pkg_manifest.is_some() && dep_ctx.has_native_deps {
        // Path B: rebar3 handles both hex deps and native/*.erl
        let native_deps = &env.full_manifest.as_ref().unwrap().native_dependencies;

        // Aggregate native dependencies from all packages in the dependency graph
        let aggregated = aggregate_native_dependencies(native_deps, &dep_ctx.resolved_deps);

        let rebar3_result = compile_with_rebar3(
            &env.project_root,
            &aggregated,
            false,
            &dep_ctx.resolved_deps,
        )?;
        eprintln!(
            "Compiled native deps via rebar3 ({} package(s), {} ebin dir(s))",
            aggregated.len(),
            rebar3_result.ebin_paths.len()
        );
        Some(rebar3_result)
    } else if pkg_manifest.is_some() {
        // Path A: compile native/*.erl directly via erlc (no hex deps).
        // Uses compile_native_erlang_with_deps to also compile transitive
        // BT dependency native sources (same as Path B's post-rebar3 step).
        let native_ebin =
            compile_native_erlang_with_deps(&env.project_root, &dep_ctx.resolved_deps)?;
        let module_names = {
            use crate::beam_compiler::discover_native_modules;
            discover_native_modules(&env.project_root)?
        };
        match native_ebin {
            Some(ebin) => Some(Rebar3Result {
                ebin_paths: vec![ebin],
                module_names,
            }),
            None if !module_names.is_empty() => {
                // Native modules discovered but no ebin produced — shouldn't happen
                None
            }
            None => None,
        }
    } else {
        None
    };

    // BT-1730: Validate native .erl files for hardcoded bt@<pkg>@ module references.
    // Warns at compile time so moving a class between packages doesn't cause
    // silent runtime failures.
    if let Some(pkg) = pkg_manifest {
        validate_native_class_references(&env.project_root, &pkg.name, class_module_index)?;
    }

    Ok(native_result)
}

/// Compute file-module-core triples for all source files.
///
/// For each `.bt` source file, computes the Erlang module name (using the
/// package naming convention for package builds) and the corresponding
/// `.core` output path.
fn compute_file_module_pairs(
    env: &BuildEnvironment,
) -> Result<Vec<(Utf8PathBuf, String, Utf8PathBuf)>> {
    let pkg_manifest = env.pkg_manifest();
    let mut pairs = Vec::new();

    for file in &env.source_files {
        let stem = file
            .file_stem()
            .ok_or_else(|| miette::miette!("File '{}' has no name", file))?;

        // Validate module name contains only safe characters
        if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
            miette::bail!(
                "Invalid module name '{}': must contain only alphanumeric characters and underscores",
                stem
            );
        }

        // ADR 0026: Package mode uses bt@{package}@{relative_path} naming
        // ADR 0016: Single-file mode uses bt@{module} naming
        let module_name = if let Some(pkg) = pkg_manifest {
            let relative_module = compute_relative_module(file, env.source_root.as_deref())?;
            format!("bt@{}@{}", pkg.name, relative_module)
        } else {
            format!(
                "bt@{}",
                beamtalk_core::codegen::core_erlang::to_module_name(stem)
            )
        };
        let core_file = env.build_dir.join(format!("{module_name}.core"));
        pairs.push((file.clone(), module_name, core_file));
    }

    Ok(pairs)
}

/// Compile Core Erlang files to BEAM bytecode, reporting incremental build
/// status to the user.
fn compile_to_beam(
    build_dir: &Utf8Path,
    core_files: &[Utf8PathBuf],
    changed_count: usize,
    unchanged_count: usize,
    total_files: usize,
) -> Result<()> {
    // Report incremental build status.
    if core_files.is_empty() {
        eprintln!("All {total_files} files unchanged — nothing to compile");
        info!("All files up-to-date — nothing to compile");
    } else if unchanged_count > 0 {
        eprintln!("Compiling {changed_count} of {total_files} files ({unchanged_count} unchanged)");
        info!(count = core_files.len(), "Compiling changed files to BEAM");
    } else {
        eprintln!("Compiling {total_files} files");
        info!(count = core_files.len(), "Compiling changed files to BEAM");
    }

    // Compile Core Erlang to BEAM (no-op if nothing changed).
    let beam_count = if core_files.is_empty() {
        0
    } else {
        let compiler = BeamCompiler::new(build_dir.to_path_buf());
        let beam_files = compiler
            .compile_batch(core_files)
            .wrap_err("Failed to compile Core Erlang to BEAM")?;
        beam_files.len()
    };

    info!(
        beam_count,
        skipped = unchanged_count,
        "Build completed successfully"
    );
    Ok(())
}

/// Phase 5-8: Build the class index (Pass 1), merge dependency indexes,
/// compile changed source files (Pass 2), and compile Core Erlang to BEAM.
fn execute_build_passes(
    env: &BuildEnvironment,
    options: &beamtalk_core::CompilerOptions,
    dep_ctx: &DependencyContext,
    force: bool,
) -> Result<BuildPassesResult> {
    let index = build_class_index(env, dep_ctx, options, force)?;
    let force = if index.force_pass2 { true } else { force };

    let native_result = compile_native_sources(env, dep_ctx, &index.class_module_index)?;

    let file_module_pairs = compute_file_module_pairs(env)?;

    // BT-1682: Per-file change detection — only recompile files whose source
    // is newer than the corresponding .beam output.
    let changes = detect_changes(&env.source_files, &env.build_dir, &file_module_pairs, force);

    // Warn about orphaned .beam files (source deleted but .beam remains)
    for orphan in &changes.orphaned_beam_files {
        eprintln!(
            "warning: orphaned .beam file {orphan} has no corresponding .bt source (source may have been deleted)"
        );
    }

    // Collect ALL module names for .app generation (both changed and unchanged),
    // but only compile changed files.
    let changed_set: HashSet<&Utf8Path> = changes
        .changed_files
        .iter()
        .map(Utf8PathBuf::as_path)
        .collect();

    // Pass 2: compile each file with the full class → module index.
    // BT-1544: Reuse cached ASTs from Pass 1 to avoid re-reading and re-parsing.
    // BT-1682: Only compile files that have changed since last build.
    let mut cached_asts = index.cached_asts;
    let mut core_files = Vec::new();
    let mut module_names = Vec::new();
    let strict_deps = env
        .full_manifest
        .as_ref()
        .is_some_and(|m| m.package.strict_deps);

    let registry_ref = if index.dep_registry.is_empty() {
        None
    } else {
        Some(&index.dep_registry)
    };
    let compile_ctx = CompileContext {
        hierarchy: ClassHierarchyContext {
            class_module_index: index.class_module_index.clone(),
            class_superclass_index: index.class_superclass_index.clone(),
            pre_loaded_classes: index.all_class_infos.clone(),
        },
        dep_registry: registry_ref,
        strict_deps,
    };
    for (file, module_name, core_file) in &file_module_pairs {
        module_names.push(module_name.clone());

        if !changed_set.contains(file.as_path()) {
            debug!(file = %file, "Skipping unchanged file");
            continue;
        }

        let cached = cached_asts.remove(file);
        compile_file(file, module_name, core_file, options, &compile_ctx, cached)?;
        core_files.push(core_file.clone());
    }

    compile_to_beam(
        &env.build_dir,
        &core_files,
        changes.changed_files.len(),
        changes.unchanged_files.len(),
        file_module_pairs.len(),
    )?;

    Ok(BuildPassesResult {
        module_names,
        file_module_pairs,
        hierarchy: compile_ctx.hierarchy,
        native_result,
    })
}

/// Phase 9: Clean stale artifacts and generate OTP application outputs
/// (.app file, corpus, supervisor callback) for package builds.
fn post_process_package_artifacts(
    env: &BuildEnvironment,
    dep_ctx: &DependencyContext,
    passes: &BuildPassesResult,
) -> Result<()> {
    let Some(pkg) = env.pkg_manifest() else {
        return Ok(());
    };

    // BT-1682: For stale artifact cleanup, we need ALL expected .beam files
    // (both newly compiled and unchanged), not just the ones from this build.
    let all_expected_beams: Vec<Utf8PathBuf> = passes
        .file_module_pairs
        .iter()
        .map(|(_, module_name, _)| env.build_dir.join(format!("{module_name}.beam")))
        .collect();
    clean_stale_artifacts(&env.build_dir, &all_expected_beams, &pkg.name)?;
    let native_module_names: Vec<String> = passes
        .native_result
        .as_ref()
        .map(|r| r.module_names.clone())
        .unwrap_or_default();
    // ADR 0072 §7: Direct hex dep names for the {applications, [...]} list
    let hex_dep_names: Vec<String> = env
        .full_manifest
        .as_ref()
        .map(|m| m.native_dependencies.keys().cloned().collect())
        .unwrap_or_default();
    let bt_dep_names: Vec<String> = dep_ctx
        .resolved_deps
        .iter()
        .filter(|d| d.is_direct)
        .map(|d| d.name.clone())
        .collect();
    generate_package_outputs(
        &env.build_dir,
        &env.project_root,
        pkg,
        &passes.hierarchy,
        &PackageBuildOutputs {
            module_names: &passes.module_names,
            native_module_names: &native_module_names,
            bt_dep_names: &bt_dep_names,
            hex_dep_names: &hex_dep_names,
            source_files: &env.source_files,
        },
    )?;

    Ok(())
}

/// ADR 0075 Phase 1: Extract type specs from OTP `.beam` files and cache them.
///
/// Non-fatal: if spec extraction fails (e.g., runtime not compiled), the build
/// succeeds without type information. The LSP will still provide untyped
/// completions in that case.
///
/// Only runs in package mode (when `beamtalk.toml` exists) — single-file builds
/// don't create the `_build/` directory.
fn extract_type_specs(env: &BuildEnvironment) {
    use crate::beam_compiler;

    // Skip in single-file mode — no _build/ directory to cache into.
    if env.pkg_manifest().is_none() {
        return;
    }

    let cache_dir = env.layout.type_cache_dir();

    // Discover OTP .beam files on the code path
    let beam_files = match beam_compiler::discover_otp_beam_files() {
        Ok(files) => files,
        Err(e) => {
            debug!("Skipping type spec extraction: {e}");
            return;
        }
    };

    if beam_files.is_empty() {
        debug!("No OTP .beam files found for type spec extraction");
        return;
    }

    match beam_compiler::extract_beam_specs(&beam_files, &cache_dir) {
        Ok(registry) => {
            if registry.module_count() > 0 {
                info!(
                    modules = registry.module_count(),
                    functions = registry.function_count(),
                    "Extracted Erlang FFI type specs"
                );
            }
        }
        Err(e) => {
            debug!("Type spec extraction failed (non-fatal): {e}");
        }
    }
}

/// Collected build outputs needed for OTP application packaging.
///
/// Groups the module name lists and source file paths that
/// `generate_package_outputs` requires, reducing its parameter count.
struct PackageBuildOutputs<'a> {
    /// All compiled Beamtalk module names (e.g. `"bt@my_app@main"`).
    module_names: &'a [String],
    /// Native Erlang module names compiled from `native/*.erl`.
    native_module_names: &'a [String],
    /// Names of Beamtalk path dependencies (for OTP `{applications}` list).
    bt_dep_names: &'a [String],
    /// Names of hex dependencies (for OTP `{applications}` list).
    hex_dep_names: &'a [String],
    /// All `.bt` source files in the package.
    source_files: &'a [Utf8PathBuf],
}

/// Generate OTP application artefacts for a package build.
///
/// Emits the `.app` file and, when `[application] supervisor` is set, the
/// OTP application callback module (`beamtalk_{appname}_app.erl` + `.beam`).
///
/// `hierarchy.class_module_index` maps Beamtalk class names to their compiled
/// Erlang module names (e.g. `"AppSup"` → `"bt@my_app@supervision@app_sup"`).
/// Used to resolve the supervisor class's actual module regardless of source
/// file path.
fn generate_package_outputs(
    build_dir: &Utf8Path,
    project_root: &Utf8PathBuf,
    pkg: &manifest::PackageManifest,
    hierarchy: &ClassHierarchyContext,
    outputs: &PackageBuildOutputs<'_>,
) -> Result<()> {
    let class_metadata = build_class_metadata(
        &hierarchy.pre_loaded_classes,
        &hierarchy.class_module_index,
        &pkg.name,
    );

    // BT-1191: Generate OTP application callback when [application] supervisor is set.
    let app_callback_module =
        if let Some(ref app_config) = manifest::find_application_config(project_root)? {
            let cb_module_name = format!("beamtalk_{}_app", pkg.name);
            // Resolve the supervisor's actual Erlang module via the class index.
            // This correctly handles classes in subdirectories (e.g. src/app/app_sup.bt).
            let sup_module = hierarchy
                .class_module_index
                .get(&app_config.supervisor)
                .ok_or_else(|| {
                    miette::miette!(
                        "Cannot find compiled module for supervisor class '{}'. \
                         Ensure the class is defined in a .bt source file in this package.",
                        app_config.supervisor
                    )
                })?;
            generate_otp_app_callback(
                build_dir,
                &app_config.supervisor,
                sup_module,
                &cb_module_name,
            )?;
            info!(
                supervisor = %app_config.supervisor,
                module = %cb_module_name,
                "Generated OTP application callback"
            );
            Some(cb_module_name)
        } else {
            None
        };

    // Include the generated callback module in the .app modules list so release
    // tooling (appup generation, etc.) can account for it.
    let all_modules: Vec<String> = if let Some(ref cb) = app_callback_module {
        let mut v = outputs.module_names.to_vec();
        v.push(cb.clone());
        v
    } else {
        outputs.module_names.to_vec()
    };

    app_file::generate_app_file(
        build_dir,
        pkg,
        &all_modules,
        &class_metadata,
        app_callback_module.as_deref(),
        outputs.native_module_names,
        outputs.bt_dep_names,
        outputs.hex_dep_names,
    )?;
    info!(name = %pkg.name, "Generated .app file");

    // BT-1722: Generate per-package corpus files for MCP discovery.
    // The corpus_dir is _build/dev/ (parent of ebin/) so MCP can find it
    // alongside the build output.
    let corpus_dir = build_dir.parent().unwrap_or(build_dir);
    generate_package_corpus(
        corpus_dir,
        &pkg.name,
        &hierarchy.pre_loaded_classes,
        outputs.source_files,
    )?;

    Ok(())
}

/// Generate per-package corpus files for MCP discovery (BT-1722).
///
/// Produces two files in `corpus_dir`:
/// - `class_corpus.json` — class metadata (name, superclass, methods, doc)
/// - `corpus.json` — source code examples from the package's `.bt` files
///
/// These are loaded at runtime by the MCP server to augment the bundled
/// stdlib corpus with package-specific classes and examples.
pub(crate) fn generate_package_corpus(
    corpus_dir: &Utf8Path,
    package_name: &str,
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    source_files: &[Utf8PathBuf],
) -> Result<()> {
    generate_class_corpus(corpus_dir, all_class_infos)?;
    generate_example_corpus(corpus_dir, package_name, source_files)?;
    Ok(())
}

/// Generate `class_corpus.json` from parsed class metadata.
fn generate_class_corpus(
    corpus_dir: &Utf8Path,
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
) -> Result<()> {
    let class_entries: Vec<serde_json::Value> = all_class_infos
        .iter()
        .filter(|ci| !ci.is_internal) // Skip internal classes (ADR 0071)
        .map(|ci| {
            let methods: Vec<String> = ci
                .methods
                .iter()
                .chain(ci.class_methods.iter())
                .filter(|m| !m.is_internal)
                .map(|m| m.selector.to_string())
                .collect();
            serde_json::json!({
                "name": ci.name.as_str(),
                "superclass": ci.superclass.as_deref().unwrap_or("Object"),
                "doc": serde_json::Value::Null,
                "methods": methods,
                "is_sealed": ci.is_sealed,
                "is_abstract": ci.is_abstract,
            })
        })
        .collect();

    if !class_entries.is_empty() {
        let class_json = serde_json::to_string_pretty(&class_entries).into_diagnostic()?;
        let class_corpus_path = corpus_dir.join("class_corpus.json");
        fs::write(&class_corpus_path, format!("{class_json}\n"))
            .into_diagnostic()
            .wrap_err("Failed to write class_corpus.json")?;
        info!(
            path = %class_corpus_path,
            count = class_entries.len(),
            "Generated package class corpus"
        );
    }
    Ok(())
}

/// Generate `corpus.json` from package source files.
#[allow(clippy::too_many_lines)] // Entry extraction loop — splitting further would obscure the flow
fn generate_example_corpus(
    corpus_dir: &Utf8Path,
    package_name: &str,
    source_files: &[Utf8PathBuf],
) -> Result<()> {
    let mut corpus_entries: Vec<serde_json::Value> = Vec::new();
    for file in source_files {
        let Ok(source) = fs::read_to_string(file.as_std_path()) else {
            continue;
        };
        let stem = file.file_stem().unwrap_or_default();

        // Strip license header from source
        let clean_source = source
            .lines()
            .skip_while(|line| {
                let trimmed = line.trim();
                trimmed.starts_with("// Copyright")
                    || trimmed.starts_with("// SPDX")
                    || trimmed.is_empty()
            })
            .collect::<Vec<_>>()
            .join("\n")
            .trim()
            .to_string();
        if clean_source.is_empty() {
            continue;
        }

        // Extract leading doc comments as explanation
        let explanation = extract_leading_comments(&source);

        // Parse the source to extract class names for tags and title
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let mut tags: Vec<String> = Vec::new();
        let mut class_names: Vec<String> = Vec::new();
        for class in &module.classes {
            class_names.push(class.name.name.to_string());
            tags.push(class.name.name.to_string());
            if let Some(ref superclass) = class.superclass {
                tags.push(superclass.name.to_string());
            }
            for method in &class.methods {
                let name = method.selector.name();
                if !name.is_empty() {
                    tags.push(name.to_string());
                }
            }
        }
        tags.push(package_name.to_string());
        tags.sort();
        tags.dedup();

        let title = if class_names.is_empty() {
            stem.replace(['_', '-'], " ")
        } else {
            class_names.join(", ")
        };

        let id = format!(
            "pkg-{}-{}",
            package_name,
            stem.to_lowercase()
                .replace(|c: char| !c.is_alphanumeric() && c != '-', "-")
        );

        corpus_entries.push(serde_json::json!({
            "id": id,
            "title": title,
            "category": format!("package-{package_name}"),
            "tags": tags,
            "source": clean_source,
            "explanation": explanation,
        }));
    }

    if !corpus_entries.is_empty() {
        corpus_entries.sort_by(|a, b| {
            let a_id = a["id"].as_str().unwrap_or("");
            let b_id = b["id"].as_str().unwrap_or("");
            a_id.cmp(b_id)
        });
        let corpus = serde_json::json!({ "entries": corpus_entries });
        let json = serde_json::to_string_pretty(&corpus).into_diagnostic()?;
        let corpus_path = corpus_dir.join("corpus.json");
        fs::write(&corpus_path, format!("{json}\n"))
            .into_diagnostic()
            .wrap_err("Failed to write corpus.json")?;
        info!(
            path = %corpus_path,
            count = corpus_entries.len(),
            "Generated package example corpus"
        );
    }
    Ok(())
}

/// Extract leading doc comments from source, skipping license headers.
fn extract_leading_comments(source: &str) -> String {
    source
        .lines()
        .filter(|line| {
            let trimmed = line.trim();
            !trimmed.starts_with("// Copyright") && !trimmed.starts_with("// SPDX")
        })
        .skip_while(|line| line.trim().is_empty())
        .take_while(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("// ") || trimmed == "//"
        })
        .map(|line| {
            line.trim()
                .strip_prefix("// ")
                .or_else(|| line.trim().strip_prefix("//"))
                .unwrap_or("")
        })
        .collect::<Vec<_>>()
        .join(" ")
        .trim()
        .to_string()
}

/// Generate an OTP application callback module (`beamtalk_{appname}_app.erl`).
///
/// The generated module implements the OTP `application` behaviour, calling
/// the Beamtalk supervisor's `start_link` from `start/2`. It is compiled to
/// BEAM and placed alongside the other package modules in the build directory.
fn generate_otp_app_callback(
    build_dir: &Utf8Path,
    supervisor_class: &str,
    sup_module: &str,
    cb_module_name: &str,
) -> Result<()> {
    let src = format!(
        "%% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         %%\n\
         %% Generated OTP application callback.\n\
         %% Do not edit — regenerated by `beamtalk build`.\n\
         -module({cb_module_name}).\n\
         -behaviour(application).\n\
         -export([start/2, stop/1]).\n\
         \n\
         start(_Type, _Args) ->\n\
             case '{sup_module}':'start_link'() of\n\
                 {{ok, Pid}} = Ok ->\n\
                     SupTuple = {{beamtalk_supervisor, '{supervisor_class}', '{sup_module}', Pid}},\n\
                     beamtalk_supervisor:register_root(SupTuple),\n\
                     Ok;\n\
                 Err ->\n\
                     Err\n\
             end.\n\
         \n\
         stop(_State) -> ok.\n"
    );

    // Write the .erl source next to the .core files so erlc can pick it up
    let erl_path = build_dir.join(format!("{cb_module_name}.erl"));
    fs::write(&erl_path, src)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write OTP app callback '{erl_path}'"))?;

    // Compile the generated .erl with erlc
    beamtalk_cli::erlc::ErlcInvocation::new(build_dir)
        .source_file(erl_path)
        .run_status(&format!("OTP app callback '{cb_module_name}' compilation"))?;

    Ok(())
}

/// Remove stale build artifacts from the build directory.
///
/// After a successful build, compares the set of `.beam` files just produced
/// against all `bt@*.beam` files on disk. Any on-disk file not in the produced
/// set is stale (from a deleted source file, renamed class, or package rename)
/// and is removed along with its corresponding `.core` file.
///
/// Also removes `.app` files that don't match the current package name (from
/// package renames).
fn clean_stale_artifacts(
    build_dir: &Utf8Path,
    produced_beams: &[Utf8PathBuf],
    pkg_name: &str,
) -> Result<()> {
    let produced: std::collections::HashSet<&Utf8Path> =
        produced_beams.iter().map(Utf8PathBuf::as_path).collect();

    let current_app_file = format!("{pkg_name}.app");

    let entries = fs::read_dir(build_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read build directory '{build_dir}'"))?;

    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };

        let ext = path.extension().and_then(|e| e.to_str());

        // Clean stale .app files from package renames
        if ext == Some("app") && name != current_app_file {
            let utf8 = Utf8PathBuf::from_path_buf(path.clone())
                .map_err(|_| miette::miette!("Non-UTF-8 path in build dir"))?;
            debug!(file = %utf8, "Removing stale .app artifact");
            fs::remove_file(&utf8).into_diagnostic()?;
            continue;
        }

        // Only consider bt@* files (user/package modules) for beam/core cleanup
        if !name.starts_with("bt@") {
            continue;
        }

        match ext {
            Some("beam") => {
                let utf8 = Utf8PathBuf::from_path_buf(path.clone())
                    .map_err(|_| miette::miette!("Non-UTF-8 path in build dir"))?;
                if !produced.contains(utf8.as_path()) {
                    debug!(file = %utf8, "Removing stale .beam artifact");
                    fs::remove_file(&utf8).into_diagnostic()?;
                    // Also remove the companion .core file if present
                    let core = utf8.with_extension("core");
                    if core.exists() {
                        fs::remove_file(&core).into_diagnostic()?;
                    }
                }
            }
            Some("core") => {
                // Stale .core files whose .beam was already removed (or never produced).
                // Tolerates NotFound because the .beam branch may have already removed
                // the companion .core in the same cleanup pass.
                let beam_path = path.with_extension("beam");
                if !beam_path.exists() {
                    let utf8 = Utf8PathBuf::from_path_buf(path.clone())
                        .map_err(|_| miette::miette!("Non-UTF-8 path in build dir"))?;
                    match fs::remove_file(&utf8) {
                        Ok(()) => debug!(file = %utf8, "Removing orphaned .core artifact"),
                        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                        Err(e) => return Err(e).into_diagnostic(),
                    }
                }
            }
            _ => {}
        }
    }

    Ok(())
}

/// Find all `.bt` source files at the given path.
///
/// If `path` is a file, returns it (must have `.bt` extension).
/// If `path` is a directory, searches `src/` subdirectory first, falling back
/// to the directory itself.
fn find_source_files(path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    if path.is_file() {
        return FileWalker::source_files().walk(path);
    }

    if !path.exists() {
        miette::bail!("Path '{}' does not exist", path);
    }

    // Look for src directory or .bt files in current directory
    let src_dir = path.join("src");
    let search_dir = if src_dir.exists() {
        src_dir
    } else {
        path.to_path_buf()
    };

    FileWalker::source_files().walk(&search_dir)
}

/// Collect all `.bt` source files from a directory tree.
///
/// Returns an error if the directory does not exist or cannot be read.
pub fn collect_source_files_from_dir(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    FileWalker::source_files().walk(dir)
}

/// Collect all `.bt` and `.btscript` source files from a directory tree.
///
/// Used by `beamtalk fmt` to find all formattable files when given a directory
/// path. Returns an error if the directory does not exist or cannot be read.
pub fn collect_formattable_files_from_dir(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    FileWalker::format_files().walk(dir)
}

/// Compute the relative module path from a source file.
///
/// When `source_root` is provided (i.e., a `src/` directory exists), computes
/// the path relative to it. Subdirectories become `@` segments.
///
/// Examples:
/// - `src/counter.bt` → `counter`
/// - `src/util/math.bt` → `util@math`
///
/// Falls back to the file stem when no source root is available.
pub(crate) fn compute_relative_module(
    file: &Utf8Path,
    source_root: Option<&Utf8Path>,
) -> Result<String> {
    if let Some(root) = source_root {
        if let Ok(relative) = file.strip_prefix(root) {
            let without_ext = relative.with_extension("");
            let segments: Vec<String> = without_ext
                .components()
                .map(|c| {
                    let segment = c.as_str();
                    // Validate each path segment
                    if !segment
                        .chars()
                        .all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
                    {
                        miette::bail!(
                            "Invalid directory name '{}' in source path '{}': must contain only alphanumeric characters and underscores",
                            segment,
                            file
                        );
                    }
                    Ok(beamtalk_core::codegen::core_erlang::to_module_name(segment))
                })
                .collect::<Result<_>>()?;
            return Ok(segments.join("@"));
        }
    }
    // Fallback: use file stem
    let stem = file.file_stem().unwrap_or("unknown");
    Ok(beamtalk_core::codegen::core_erlang::to_module_name(stem))
}

/// Compile a single `.bt` source file to Core Erlang, printing progress.
///
/// When `cached_ast` is `Some`, reuses the pre-parsed source and `Module` from
/// Pass 1 instead of re-reading and re-parsing the file (BT-1544).
fn compile_file(
    path: &Utf8Path,
    module_name: &str,
    core_file: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    ctx: &CompileContext<'_>,
    cached_ast: Option<CachedAst>,
) -> Result<()> {
    debug!("Compiling {path}");

    compile_source_with_bindings(
        path,
        module_name,
        core_file,
        options,
        &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
        ctx,
        cached_ast,
    )?;

    debug!("Generated Core Erlang: {core_file}");

    Ok(())
}

/// Cached AST from Pass 1 — holds the source text, parsed `Module`, and any
/// parse diagnostics so Pass 2 can skip re-reading and re-parsing the same file.
#[derive(Debug)]
pub(crate) struct CachedAst {
    pub(crate) source: String,
    pub(crate) module: beamtalk_core::ast::Module,
    pub(crate) diagnostics: Vec<beamtalk_core::source_analysis::Diagnostic>,
}

/// Build class indexes from a set of source files.
///
/// Returns four items:
/// 1. **Class module index:** Maps class names to compiled module names
///    (e.g. `"SchemeEnv"` → `"bt@sicp_example@scheme@env"`).
/// 2. **Class superclass index:** Maps class names to their direct superclass names
///    (e.g. `"MyChild"` → `"MyParent"`). Used by BT-894 to resolve cross-file
///    inheritance so the compiler can determine value-object vs actor codegen.
/// 3. **Class infos:** Full `ClassInfo` entries extracted from all source files.
///    BT-1523: Injected into the type checker's hierarchy during Pass 2 so
///    cross-file method resolution works without reading BEAM files.
/// 4. **Cached ASTs:** BT-1544: Maps file paths to their parsed `Module` + source
///    text so Pass 2 can reuse them instead of re-reading and re-parsing.
#[allow(clippy::type_complexity)]
pub(crate) fn build_class_module_index(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    pkg_name: &str,
) -> Result<(
    HashMap<String, String>,
    HashMap<String, String>,
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    HashMap<Utf8PathBuf, CachedAst>,
)> {
    let mut module_index = HashMap::new();
    let mut superclass_index = HashMap::new();
    let mut all_class_infos = Vec::new();
    let mut cached_asts: HashMap<Utf8PathBuf, CachedAst> = HashMap::new();

    for file in source_files {
        let relative_module = compute_relative_module(file, source_root)?;
        let module_name = format!("bt@{pkg_name}@{relative_module}");

        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                warn!(
                    file = %file,
                    error = %e,
                    "Cannot read source file during index pass; class resolution from this file will be skipped"
                );
                continue;
            }
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        if !diagnostics.is_empty() {
            warn!(
                file = %file,
                diagnostic_count = diagnostics.len(),
                "Source file has parse errors during index pass; class resolution from this file may be incomplete"
            );
        }

        // BT-1523: Extract full ClassInfo for cross-file hierarchy resolution.
        let class_infos =
            beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module);
        all_class_infos.extend(class_infos);

        for class in &module.classes {
            let class_name = class.name.name.to_string();
            if let Some(existing) = module_index.get(&class_name) {
                if existing != &module_name {
                    eprintln!(
                        "Warning: class '{class_name}' is defined in both '{existing}' and \
                         '{module_name}'; using '{module_name}' for cross-file dispatch"
                    );
                }
            }
            module_index.insert(class_name.clone(), module_name.clone());
            // BT-894: Record direct superclass for cross-file hierarchy resolution.
            // When a duplicate class overwrites a prior entry, keep the superclass
            // index consistent by removing any stale mapping if the new definition
            // has no explicit superclass.
            if let Some(ref superclass) = class.superclass {
                superclass_index.insert(class_name.clone(), superclass.name.to_string());
            } else {
                superclass_index.remove(&class_name);
            }
        }

        // BT-1544: Cache the parsed AST so Pass 2 doesn't re-read/re-parse.
        cached_asts.insert(
            file.clone(),
            CachedAst {
                source,
                module,
                diagnostics,
            },
        );
    }

    Ok((module_index, superclass_index, all_class_infos, cached_asts))
}

// ── BT-1730: Class module header generation ─────────────────────────────────

/// Generate a `beamtalk_classes.hrl` header file with Erlang preprocessor
/// macros that map Beamtalk class names to their compiled BEAM module atoms.
///
/// For each entry in the `class_module_index`, emits:
/// ```erlang
/// -define(BT_CLASS_MODULE_HTTPResponse, 'bt@http@httpresponse').
/// ```
///
/// Native `.erl` files include this header and use `?BT_CLASS_MODULE_ClassName`
/// instead of hardcoding `'bt@<pkg>@<class>'` atoms. When a class moves
/// between packages, the macro value updates automatically on rebuild.
///
/// The header is written to `_build/dev/native/include/beamtalk_classes.hrl`
/// and is added to the erlc include path during native compilation.
fn generate_class_header(
    include_dir: &Utf8Path,
    class_module_index: &HashMap<String, String>,
) -> Result<()> {
    fs::create_dir_all(include_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create native include directory '{include_dir}'"))?;

    let hrl_path = include_dir.join("beamtalk_classes.hrl");

    let mut content = String::new();
    content.push_str("%% Copyright 2026 James Casey\n");
    content.push_str("%% SPDX-License-Identifier: Apache-2.0\n\n");
    content.push_str("%% Generated by beamtalk build — do not edit.\n");
    content.push_str("%% BT-1730: Maps Beamtalk class names to compiled BEAM module atoms.\n");
    content.push_str("%%\n");
    content.push_str("%% Usage in native .erl files:\n");
    content.push_str("%%   -include(\"beamtalk_classes.hrl\").\n");
    content.push_str("%%   ?BT_CLASS_MODULE_HTTPResponse:some_function(Args).\n\n");
    content.push_str("-ifndef(BEAMTALK_CLASSES_HRL).\n");
    content.push_str("-define(BEAMTALK_CLASSES_HRL, true).\n\n");

    // Sort for deterministic output across builds.
    let mut entries: Vec<(&String, &String)> = class_module_index.iter().collect();
    entries.sort_by_key(|(class_name, _)| class_name.as_str());

    for (class_name, module_name) in &entries {
        let _ = writeln!(
            content,
            "-define(BT_CLASS_MODULE_{class_name}, '{module_name}')."
        );
    }

    content.push_str("\n-endif. %% BEAMTALK_CLASSES_HRL\n");

    fs::write(&hrl_path, &content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write {hrl_path}"))?;

    info!(
        path = %hrl_path,
        count = entries.len(),
        "Generated beamtalk_classes.hrl"
    );

    Ok(())
}

/// Scan native `.erl` files for hardcoded `bt@<pkg>@<class>` module references
/// and warn when they should use `?BT_CLASS_MODULE_*` macros instead.
///
/// This catches the footgun where native code directly references a compiled
/// Beamtalk module atom. If the class moves between packages (or the package
/// is renamed), the hardcoded atom becomes stale and causes silent runtime
/// failures.
///
/// Detects two categories:
/// 1. Any `'bt@<current_pkg>@...'` atom — these should always use macros.
/// 2. Any `'bt@<other_pkg>@...'` atom that matches a known class in the
///    `class_module_index` — catches stale references from a class move.
///
/// Only warns about references in code (not comments or doc strings).
fn validate_native_class_references(
    project_root: &Utf8Path,
    pkg_name: &str,
    class_module_index: &HashMap<String, String>,
) -> Result<()> {
    let native_dir = project_root.join("native");
    if !native_dir.exists() || !native_dir.is_dir() {
        return Ok(());
    }

    // Build a set of known module names for reverse-lookup.
    let known_modules: HashSet<&str> = class_module_index.values().map(String::as_str).collect();

    // Build a reverse index: module_name -> class_name for reporting.
    let module_to_class: HashMap<&str, &str> = class_module_index
        .iter()
        .map(|(class, module)| (module.as_str(), class.as_str()))
        .collect();

    let mut erl_files: Vec<Utf8PathBuf> = Vec::new();
    collect_erl_files(&native_dir, &mut erl_files)?;

    for erl_file in &erl_files {
        let Ok(source) = fs::read_to_string(erl_file) else {
            continue;
        };

        for (line_no, line) in source.lines().enumerate() {
            let trimmed = line.trim();
            // Skip comments and empty lines.
            if trimmed.starts_with('%') || trimmed.is_empty() {
                continue;
            }

            // Find all 'bt@...' atom references on this line.
            let mut search_from = 0;
            while let Some(offset) = line[search_from..].find("'bt@") {
                let pos = search_from + offset;
                let after_quote = &line[pos + 1..]; // skip leading quote
                if let Some(end) = after_quote.find('\'') {
                    let module_atom = &after_quote[..end];

                    // Category 1: current package reference — always warn
                    let is_current_pkg = module_atom.starts_with(&format!("bt@{pkg_name}@"));
                    // Category 2: known class from any package — warn about hardcoding
                    let is_known_module = known_modules.contains(module_atom);

                    if is_current_pkg || is_known_module {
                        let class_hint = module_to_class
                            .get(module_atom)
                            .map(|c| format!(" (class {c})"))
                            .unwrap_or_default();
                        eprintln!(
                            "warning: {}:{}:{}: hardcoded Beamtalk module reference \
                             '{module_atom}'{class_hint}; use ?BT_CLASS_MODULE_* macro \
                             from beamtalk_classes.hrl instead (BT-1730)",
                            erl_file,
                            line_no + 1,
                            pos + 1,
                        );
                    }

                    // Advance past this atom to find more on the same line.
                    search_from = pos + 1 + end + 1;
                } else {
                    break;
                }
            }
        }
    }

    Ok(())
}

/// Build `.app`-file class metadata from compiled `ClassInfo` entries.
///
/// Converts the compiler's `ClassInfo` into the `ClassMetadata` structs
/// that `generate_app_file` writes into `{env, [{classes, [...]}]}`.
/// Only classes belonging to `package_name` are included.
///
/// The `kind` field is resolved using a `ClassHierarchy` that includes both
/// the user/dep classes and stdlib builtins, so `Server subclass: MyServer`
/// correctly resolves to kind `"actor"`.
pub(crate) fn build_class_metadata(
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    class_module_index: &HashMap<String, String>,
    package_name: &str,
) -> Vec<app_file::ClassMetadata> {
    // Start from the cached stdlib hierarchy and add user/dep classes so
    // the superclass chain can be resolved across all sources.
    let mut hierarchy =
        beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(all_class_infos.to_vec());

    let module_prefix = format!("bt@{package_name}@");
    all_class_infos
        .iter()
        .filter(|ci| !ci.is_internal)
        .filter_map(|ci| {
            let module = class_module_index.get(ci.name.as_str())?;
            // Scope to this package using the module naming convention
            // bt@{package}@{class}. ClassInfo.package may be None when
            // extracted from source AST rather than BEAM metadata.
            if !module.starts_with(&module_prefix) {
                return None;
            }
            let kind = hierarchy.resolve_class_kind(&ci.name);
            Some(app_file::ClassMetadata {
                module: module.clone(),
                class_name: ci.name.to_string(),
                parent_class: ci
                    .superclass
                    .as_deref()
                    .unwrap_or("ProtoObject")
                    .to_string(),
                package: package_name.to_string(),
                kind: kind.as_str().to_string(),
                type_params: ci.type_params.iter().map(ToString::to_string).collect(),
            })
        })
        .collect()
}

/// Collect `ClassInfo` from multiple sources into a single unified vector.
///
/// BT-1733: This is the single entry point for gathering all `ClassInfo`
/// metadata across the project. Callers provide `ClassInfo` slices from each
/// source (package sources, dependencies, fixtures, etc.) and get back a
/// merged vector suitable for the type checker and structural validator.
///
/// Adding a new `.bt` source location only requires adding its `ClassInfo`
/// slice to the input list — no separate wiring needed.
pub(crate) fn collect_all_class_infos(
    sources: &[&[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo]],
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    let total_len: usize = sources.iter().map(|s| s.len()).sum();
    let mut result = Vec::with_capacity(total_len);
    for source in sources {
        result.extend_from_slice(source);
    }
    result
}

// ── Bundled rebar3 ────────────────────────────────────────────────

/// Locate the rebar3 escript for compiling hex dependencies.
///
/// Resolution order:
/// 1. **Bundled copy** — `runtime/tools/rebar3` relative to the runtime directory
///    (found via [`beamtalk_cli::repl_startup::find_runtime_dir`]). This is the
///    primary path and should always succeed in a standard Beamtalk installation.
/// 2. **System `rebar3`** — falls back to `rebar3` on `$PATH` if the bundled
///    copy is missing (e.g., when running from an unusual layout).
///
/// # Errors
///
/// Returns an error if neither the bundled copy nor a system `rebar3` is found.
///
/// # Version policy
///
/// The bundled rebar3 is pinned to 3.27.0. Update when:
/// - A new rebar3 release adds features we need or fixes bugs we hit
/// - OTP compatibility requires a newer version
/// - Security advisories are published
pub(crate) fn rebar3_path() -> Result<PathBuf> {
    // Try bundled copy relative to the runtime directory
    if let Ok(runtime_dir) = beamtalk_cli::repl_startup::find_runtime_dir() {
        let bundled = runtime_dir.join("tools").join("rebar3");
        if bundled.exists() {
            debug!(path = %bundled.display(), "Using bundled rebar3");
            return Ok(bundled);
        }
    }

    // Fall back to system rebar3 on $PATH
    let system_candidates = if cfg!(windows) {
        vec!["rebar3.cmd", "rebar3.exe", "rebar3"]
    } else {
        vec!["rebar3"]
    };

    for candidate in system_candidates {
        if let Ok(output) = std::process::Command::new(candidate)
            .arg("version")
            .output()
        {
            if output.status.success() {
                info!(
                    rebar3 = candidate,
                    "Using system rebar3 (bundled copy not found)"
                );
                return Ok(PathBuf::from(candidate));
            }
        }
    }

    Err(miette::miette!(
        "rebar3 not found.\n\
         Expected bundled copy at <runtime>/tools/rebar3.\n\
         Install rebar3 or run from the repository root."
    ))
}

// ── ADR 0072 Phase 2: rebar3 integration ──────────────────────────

/// Result of a rebar3 compilation step.
///
/// Contains the ebin paths that must be added to the BEAM code path
/// for subsequent `.bt` compilation and at runtime.
#[derive(Debug)]
pub(crate) struct Rebar3Result {
    /// Ebin directories produced by rebar3 (one per compiled hex dep,
    /// plus the package's own native modules if `native/` exists).
    /// Used by `run.rs` and `repl/mod.rs` via `collect_rebar3_ebin_paths`.
    #[allow(dead_code)] // Informational; code paths are collected from filesystem
    pub ebin_paths: Vec<Utf8PathBuf>,
    /// Erlang module names compiled from native sources.
    /// Used for `.app.src` module list generation.
    pub module_names: Vec<String>,
}

/// Aggregate native (hex) dependencies from the root package and all
/// resolved Beamtalk dependencies in the dependency graph.
///
/// ADR 0072 §5: Because BEAM has a flat module namespace, all hex deps
/// across all packages must be resolved in a single rebar3 invocation.
/// Constraints from different packages for the same hex dep are both
/// included — rebar3's resolver handles the intersection.
fn aggregate_native_dependencies(
    root_native_deps: &NativeDependencyMap,
    resolved_bt_deps: &[super::deps::path::ResolvedDependency],
) -> NativeDependencyMap {
    let mut aggregated = root_native_deps.clone();

    // Walk each resolved Beamtalk dependency and collect its [native.dependencies]
    for dep in resolved_bt_deps {
        let manifest_path = dep.root.join("beamtalk.toml");
        if !manifest_path.exists() {
            continue;
        }
        let Ok(parsed) = manifest::parse_manifest_full(&manifest_path) else {
            debug!(
                dep = %dep.name,
                "Failed to parse manifest for native dep aggregation — skipping"
            );
            continue;
        };
        for (name, native_dep) in &parsed.native_dependencies {
            if !aggregated.contains_key(name) {
                debug!(
                    dep = %dep.name,
                    hex_pkg = %name,
                    constraint = %native_dep.constraint,
                    "Aggregating native dependency from transitive package"
                );
                aggregated.insert(name.clone(), native_dep.clone());
            }
            // If the same hex dep is already present (from root or another dep),
            // we keep the first constraint. rebar3 resolves transitive deps
            // itself; duplicates in the deps list are not supported.
        }
    }

    aggregated
}

/// Generate a `rebar.config` file for compiling hex dependencies.
///
/// The generated file lives at `_build/dev/native/rebar.config` and is a
/// build artifact — it should not be checked in.
///
/// rebar3 reads `rebar.config` from its current working directory
/// (it has no `--config` flag). We run rebar3 from `_build/dev/native/`
/// so the config lives there.
///
/// When `locked_versions` is provided (from `beamtalk.lock`), the generated
/// config uses exact pinned versions instead of constraints for reproducible
/// builds (ADR 0072 §5).
///
/// Note: rebar3 only handles hex deps. Native `.erl` files from the project
/// and transitive BT dependencies are compiled separately via `erlc` in
/// [`compile_native_erlang_with_deps`].
fn generate_rebar_config(
    project_root: &Utf8Path,
    native_deps: &NativeDependencyMap,
    locked_versions: Option<
        &std::collections::BTreeMap<String, super::deps::lockfile::NativePackageLock>,
    >,
) -> Result<Utf8PathBuf> {
    let layout = BuildLayout::new(project_root);
    let rebar_dir = layout.native_dir();
    fs::create_dir_all(&rebar_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create rebar3 build directory '{rebar_dir}'"))?;

    let config_path = rebar_dir.join("rebar.config");

    let mut config = String::new();
    config.push_str("%% Auto-generated by beamtalk build — do not edit\n");

    // Generate {deps, [...]}.
    // When we have locked versions, use exact pinned versions for reproducibility.
    // Otherwise, use the constraint strings from beamtalk.toml.
    config.push_str("{deps, [\n");
    let dep_entries: Vec<String> = native_deps
        .iter()
        .map(|(name, dep)| {
            if let Some(locks) = locked_versions {
                if let Some(lock) = locks.get(name) {
                    return format!("    {{{name}, \"{}\"}}", lock.version);
                }
            }
            format!("    {{{name}, \"{}\"}}", dep.constraint)
        })
        .collect();
    config.push_str(&dep_entries.join(",\n"));
    config.push_str("\n]}.\n");

    // rebar3 only handles hex deps — native/*.erl files are compiled
    // separately via erlc after rebar3 finishes (see compile_native_erlang_with_deps).
    // This avoids rebar3 project discovery issues: rebar3 won't compile
    // src_dirs sources without a top-level .app.src, and generating one
    // creates conflicts with the package's own .app file.
    config.push_str("{erl_opts, [debug_info, warn_missing_doc]}.\n");

    debug!(path = %config_path, "Writing rebar.config");
    fs::write(&config_path, &config)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write rebar.config at '{config_path}'"))?;

    Ok(config_path)
}

/// Invoke `rebar3 compile` to fetch and compile hex dependencies
/// (and optionally the package's own `native/*.erl` files).
///
/// ADR 0072 §4 Path B: rebar3 runs from `_build/dev/native/` where the
/// generated `rebar.config` lives. `REBAR_BASE_DIR` is set to the same
/// directory so all output stays in the build tree. `{src_dirs}` and
/// `{include_dirs}` use relative paths back to the project root.
///
/// ADR 0072 §5 (Phase 2): After rebar3 resolves and compiles, the resolved
/// versions from `rebar.lock` are captured in `beamtalk.lock` as
/// `[[native_package]]` entries. On subsequent builds, exact pinned
/// versions from the lockfile are used instead of constraints.
#[instrument(skip_all, fields(project_root = %project_root))]
fn compile_with_rebar3(
    project_root: &Utf8Path,
    native_deps: &NativeDependencyMap,
    force_resolve: bool,
    resolved_bt_deps: &[super::deps::path::ResolvedDependency],
) -> Result<Rebar3Result> {
    use super::deps::lockfile::{self, Lockfile};

    let rebar3 = rebar3_path()?;

    // Read existing lockfile to check for pinned native versions
    let existing_lock = Lockfile::read(project_root)?;
    let locked_versions = if force_resolve {
        None
    } else {
        existing_lock
            .as_ref()
            .filter(|l| l.has_native_packages())
            .map(|l| &l.native_packages)
    };

    if locked_versions.is_some() {
        info!("Using pinned native package versions from beamtalk.lock");
    }

    // Generate the rebar.config in _build/dev/native/
    let config_path = generate_rebar_config(project_root, native_deps, locked_versions)?;

    let rebar_layout = BuildLayout::new(project_root);
    let rebar_base_dir = rebar_layout.native_dir();

    info!(
        rebar3 = %rebar3.display(),
        config = %config_path,
        base_dir = %rebar_base_dir,
        deps = native_deps.len(),
        "Invoking rebar3 compile"
    );

    let mut cmd = std::process::Command::new(&rebar3);
    cmd.arg("compile");
    // rebar3 reads rebar.config from cwd — run from _build/dev/native/
    cmd.current_dir(rebar_base_dir.as_std_path());
    // Keep all rebar3 output in the same build directory.
    // REBAR_BASE_DIR must be absolute — rebar3 resolves it relative to cwd,
    // and cwd is already rebar_base_dir, so a relative path would nest.
    let abs_base_dir = std::fs::canonicalize(rebar_base_dir.as_std_path())
        .into_diagnostic()
        .wrap_err("Failed to canonicalize rebar base dir")?;
    cmd.env("REBAR_BASE_DIR", &abs_base_dir);

    let output = cmd
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run rebar3 compile.\nIs Erlang/OTP installed?")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let combined = format!("{stdout}{stderr}");
        // Surface rebar3 errors with context
        miette::bail!("rebar3 compile failed:\n{}", combined.trim_end());
    }

    // Collect all ebin directories produced by rebar3
    let ebin_paths = collect_rebar3_ebin_paths(&rebar_layout);

    info!(ebin_count = ebin_paths.len(), "rebar3 compilation complete");

    // ADR 0072 Phase 2: Extract resolved versions from rebar.lock and
    // write them to beamtalk.lock as [[native_package]] entries.
    let resolved_native = lockfile::read_rebar_lock(&rebar_base_dir)?;
    if !resolved_native.is_empty() {
        let mut lockfile = existing_lock.unwrap_or_default();
        // Replace all native entries with the freshly resolved set
        lockfile.clear_native_packages();
        for pkg in resolved_native {
            info!(
                name = %pkg.name,
                version = %pkg.version,
                "Locking native package"
            );
            lockfile.insert_native(pkg);
        }
        lockfile.write(project_root)?;
        debug!("Updated beamtalk.lock with native package entries");
    }

    // Discover native module names from the project's native/ directory.
    // Only root package modules go into `module_names`; dependency native
    // modules are tracked separately so the root `.app` doesn't claim
    // ownership of modules that belong to dependencies.
    let module_names = {
        use crate::beam_compiler::discover_native_modules;
        discover_native_modules(project_root)?
    };

    // Compile native/*.erl files via erlc (root package + transitive deps).
    // rebar3 only handles hex deps; native Erlang sources need separate
    // compilation because rebar3 won't compile src_dirs without a top-level
    // .app.src, and generating one conflicts with the package's own .app.
    let native_ebin = compile_native_erlang_with_deps(project_root, resolved_bt_deps)?;
    let mut all_ebin_paths = ebin_paths;
    if let Some(ebin) = native_ebin {
        all_ebin_paths.push(ebin);
    }

    Ok(Rebar3Result {
        ebin_paths: all_ebin_paths,
        module_names,
    })
}

/// Compile native `.erl` files from the root package and transitive BT
/// dependencies via `erlc`, after rebar3 has compiled hex deps.
///
/// Include resolution:
/// - `ERL_LIBS` → rebar3 lib dir (for hex dep headers like cowboy, gun)
/// - `-I` → beamtalk runtime apps dir (for `beamtalk_runtime` headers)
///
/// Outputs `.beam` files to `_build/dev/native/ebin/`.
/// Returns the ebin path if any files were compiled.
fn compile_native_erlang_with_deps(
    project_root: &Utf8Path,
    resolved_bt_deps: &[super::deps::path::ResolvedDependency],
) -> Result<Option<Utf8PathBuf>> {
    // Collect all native/*.erl files from root + transitive deps
    let mut erl_files: Vec<Utf8PathBuf> = Vec::new();
    let mut include_dirs: Vec<Utf8PathBuf> = Vec::new();

    // Root package
    let native_dir = project_root.join("native");
    if native_dir.exists() && native_dir.is_dir() {
        collect_erl_files(&native_dir, &mut erl_files)?;
        let include_dir = native_dir.join("include");
        if include_dir.exists() && include_dir.is_dir() {
            include_dirs.push(include_dir);
        }
    }

    // Transitive BT dependencies
    for dep in resolved_bt_deps {
        let dep_native = dep.root.join("native");
        if dep_native.exists() && dep_native.is_dir() {
            collect_erl_files(&dep_native, &mut erl_files)?;
            let dep_include = dep_native.join("include");
            if dep_include.exists() && dep_include.is_dir() {
                include_dirs.push(dep_include);
            }
        }
    }

    if erl_files.is_empty() {
        return Ok(None);
    }

    // Sort for deterministic compilation order across platforms.
    erl_files.sort();

    // Create output directory
    let build_layout = BuildLayout::new(project_root);
    let ebin_dir = build_layout.native_ebin_dir();
    fs::create_dir_all(&ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create native ebin directory '{ebin_dir}'"))?;

    let mut invocation = beamtalk_cli::erlc::ErlcInvocation::new(&ebin_dir)
        .debug_info()
        .docs()
        .erl_libs(&build_layout.rebar_lib_dir())
        .runtime_include()
        // BT-1730: generated include dir for beamtalk_classes.hrl
        .include_dir(build_layout.native_include_dir());

    for inc in &include_dirs {
        invocation = invocation.include_dir(inc);
    }

    invocation = invocation.source_files(&erl_files);

    info!(
        count = erl_files.len(),
        ebin = %ebin_dir,
        "Compiling native Erlang files via erlc"
    );

    invocation.run("Native Erlang compilation")?;

    // BT-1732: User-visible output confirming native Erlang compilation.
    let count = erl_files.len();
    let plural = if count == 1 { "" } else { "s" };
    eprintln!("Compiled {count} native Erlang file{plural}");

    Ok(Some(ebin_dir))
}

/// Collect `.erl` files from a directory (non-recursive).
fn collect_erl_files(dir: &Utf8Path, out: &mut Vec<Utf8PathBuf>) -> Result<()> {
    let entries = fs::read_dir(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?;
    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "erl") {
            let utf8 = Utf8PathBuf::from_path_buf(path)
                .map_err(|p| miette::miette!("Non-UTF-8 path: {}", p.display()))?;
            out.push(utf8);
        }
    }
    Ok(())
}

/// Collect ebin directories from rebar3's output tree.
///
/// rebar3 places compiled output at `_build/dev/native/default/lib/{app}/ebin/`.
/// This scans for all such directories and returns them.
///
/// Uses [`BuildLayout::rebar_lib_dir`] as the source of truth for the library
/// directory location.
pub(crate) fn collect_rebar3_ebin_paths(layout: &BuildLayout) -> Vec<Utf8PathBuf> {
    let lib_dir = layout.rebar_lib_dir();

    let mut paths = Vec::new();

    if !lib_dir.exists() {
        return paths;
    }

    let Ok(entries) = fs::read_dir(&lib_dir) else {
        return paths;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let ebin = path.join("ebin");
        if ebin.exists() && ebin.is_dir() {
            if let Ok(utf8) = Utf8PathBuf::from_path_buf(ebin) {
                debug!(ebin = %utf8, "Found rebar3 ebin directory");
                paths.push(utf8);
            }
        }
    }

    paths.sort();
    paths
}

/// ADR 0072 Phase 1: Check for native Erlang module name collisions across
/// packages in the dependency graph.
///
/// BEAM has a flat module namespace — only one version of any module can be
/// loaded at runtime. If two packages define the same native module name
/// (e.g., both have `native/utils.erl`), the last one loaded silently wins,
/// causing subtle runtime breakage. This check runs before compilation to
/// fail fast with a clear error naming both packages and the conflicting module.
fn check_native_module_collisions(
    project_root: &Utf8Path,
    root_package_name: &str,
    resolved_deps: &[super::deps::path::ResolvedDependency],
) -> Result<()> {
    use crate::beam_compiler::discover_native_modules;
    use std::fmt::Write;

    // Collect native modules from root package
    let root_modules = discover_native_modules(project_root)?;

    // Collect native modules from each dependency
    let mut module_owners: HashMap<String, Vec<String>> = HashMap::new();

    for module in &root_modules {
        module_owners
            .entry(module.clone())
            .or_default()
            .push(root_package_name.to_string());
    }

    for dep in resolved_deps {
        let dep_modules = discover_native_modules(&dep.root)?;
        for module in &dep_modules {
            module_owners
                .entry(module.clone())
                .or_default()
                .push(dep.name.clone());
        }
    }

    // Find collisions: modules owned by more than one package
    let mut collisions: Vec<(String, Vec<String>)> = module_owners
        .into_iter()
        .filter(|(_, owners)| owners.len() > 1)
        .collect();
    collisions.sort_by(|(a, _), (b, _)| a.cmp(b));

    if collisions.is_empty() {
        return Ok(());
    }

    // Build a clear error message listing all collisions
    let mut msg = String::from(
        "Native Erlang module name collision detected.\n\
         BEAM has a flat module namespace — only one version of any module can be loaded.\n\n",
    );

    for (module, owners) in &collisions {
        let _ = writeln!(
            msg,
            "  Module '{}' is defined by: {}",
            module,
            owners.join(", ")
        );
    }

    msg.push_str(
        "\nRename the conflicting module(s) to avoid silent runtime breakage. \
         Convention: prefix native modules with the package name (e.g., 'mypackage_utils').",
    );

    miette::bail!("{msg}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_project(temp: &TempDir) -> Utf8PathBuf {
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        fs::create_dir_all(&src_path).unwrap();
        project_path
    }

    fn write_test_file(path: &Utf8Path, content: &str) {
        fs::write(path, content).unwrap();
    }

    fn default_options() -> beamtalk_core::CompilerOptions {
        beamtalk_core::CompilerOptions::default()
    }

    #[test]
    fn test_find_source_files_single_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        write_test_file(&test_file, "test := [1].");

        let files = find_source_files(&test_file).unwrap();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0], test_file);
    }

    #[test]
    fn test_find_source_files_in_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        write_test_file(&src_path.join("file1.bt"), "test := [1].");
        write_test_file(&src_path.join("file2.bt"), "test := [2].");
        write_test_file(&src_path.join("other.txt"), "not beamtalk");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("file1.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("file2.bt")));
    }

    #[test]
    fn test_find_source_files_no_src_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        write_test_file(&project_path.join("test.bt"), "test := [1].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn test_find_source_files_non_bt_file_error() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.txt");
        write_test_file(&test_file, "not beamtalk");

        let result = find_source_files(&test_file);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_source_files_nonexistent_path() {
        let path = Utf8PathBuf::from("/nonexistent/path");
        let result = find_source_files(&path);
        assert!(result.is_err());
    }

    #[test]
    fn test_compile_valid_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        let core_file = project_path.join("test.core");
        write_test_file(&test_file, "test := [1 + 2].");

        let result = compile_file(
            &test_file,
            "test",
            &core_file,
            &default_options(),
            &CompileContext::default(),
            None,
        );
        assert!(result.is_ok());
        assert!(core_file.exists());
    }

    #[test]
    fn test_compile_file_with_syntax_error() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        let core_file = project_path.join("test.core");
        write_test_file(&test_file, "test := [1 + ]."); // Syntax error

        let result = compile_file(
            &test_file,
            "test",
            &core_file,
            &default_options(),
            &CompileContext::default(),
            None,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_build_empty_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);

        let result = build(project_path.as_str(), &default_options(), false);
        assert!(result.is_err());
    }

    #[test]
    fn test_build_single_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");

        let result = build(project_path.as_str(), &default_options(), false);

        // If escript is not available, the test should fail at the BEAM compilation stage
        // We allow this in CI environments
        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                println!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_multiple_files() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        write_test_file(&src_path.join("file1.bt"), "test1 := [1].");
        write_test_file(&src_path.join("file2.bt"), "test2 := [2].");

        let result = build(project_path.as_str(), &default_options(), false);

        // If escript is not available, the test should fail at the BEAM compilation stage
        // We allow this in CI environments
        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                println!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                eprintln!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_malformed_manifest() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "this is not valid toml {{{{",
        );

        let result = build(project_path.as_str(), &default_options(), false);
        let err = result.expect_err("Expected build to fail due to malformed manifest");
        let error_msg = format!("{err:?}");
        assert!(
            error_msg.contains("Failed to parse manifest"),
            "Expected error to mention manifest parse failure, got: {error_msg}"
        );
    }

    #[test]
    fn test_find_source_files_recursive() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let util_path = src_path.join("util");
        fs::create_dir_all(&util_path).unwrap();

        write_test_file(&src_path.join("main.bt"), "main := [1].");
        write_test_file(&util_path.join("math.bt"), "math := [2].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("main.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("math.bt")));
    }

    #[test]
    fn test_find_source_files_deeply_nested() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let deep_path = src_path.join("a").join("b").join("c");
        fs::create_dir_all(&deep_path).unwrap();

        write_test_file(&deep_path.join("deep.bt"), "deep := [42].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().contains('a'));
    }

    #[test]
    fn test_compute_relative_module_flat() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/counter.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "counter"
        );
    }

    #[test]
    fn test_compute_relative_module_subdirectory() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/util/math.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "util@math"
        );
    }

    #[test]
    fn test_compute_relative_module_deep_subdirectory() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/a/b/c.bt");
        assert_eq!(compute_relative_module(file, Some(root)).unwrap(), "a@b@c");
    }

    #[test]
    fn test_compute_relative_module_camel_case() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/MyCounter.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "my_counter"
        );
    }

    #[test]
    fn test_compute_relative_module_no_root() {
        let file = Utf8Path::new("/project/counter.bt");
        assert_eq!(compute_relative_module(file, None).unwrap(), "counter");
    }

    #[test]
    fn test_compute_relative_module_invalid_dir_name() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/my-dir/counter.bt");
        let result = compute_relative_module(file, Some(root));
        assert!(result.is_err());
    }

    #[test]
    fn test_build_with_manifest_generates_package_module_name() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                // Verify the .core file was generated with package-qualified name
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@counter.core");
                assert!(
                    core_file.exists(),
                    "Expected package-qualified .core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_without_manifest_preserves_bt_prefix() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        // No beamtalk.toml

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                let core_file = project_path.join("build/bt@counter.core");
                assert!(
                    core_file.exists(),
                    "Expected bt@counter.core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_subdirectory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let util_path = src_path.join("util");
        fs::create_dir_all(&util_path).unwrap();
        write_test_file(&util_path.join("math.bt"), "math := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@util@math.core");
                assert!(
                    core_file.exists(),
                    "Expected package-qualified .core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_creates_build_dev_ebin() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        // Verify _build/dev/ebin/ directory structure exists
        let ebin_dir = project_path.join("_build/dev/ebin");
        assert!(ebin_dir.exists(), "Expected _build/dev/ebin/ directory");

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_without_manifest_uses_build_dir() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        // No beamtalk.toml

        let result = build(project_path.as_str(), &default_options(), false);

        // Verify build/ directory is used (not _build/)
        let build_dir = project_path.join("build");
        assert!(
            build_dir.exists(),
            "Expected build/ directory for single-file mode"
        );
        let ebin_dir = project_path.join("_build");
        assert!(
            !ebin_dir.exists(),
            "_build/ should not exist in single-file mode"
        );

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_generates_app_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\ndescription = \"Test app\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                // .app is generated after BEAM compile — may not exist if escript missing
                // but the .core file should exist
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@counter.core");
                assert!(core_file.exists(), "Expected .core file at {core_file}");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }

        // If BEAM compilation succeeded, .app file should exist
        let app_file = project_path.join("_build/dev/ebin/my_app.app");
        assert!(app_file.exists(), "Expected .app file at {app_file}");
        let content = fs::read_to_string(&app_file).unwrap();
        assert!(content.contains("{application, my_app, ["));
        assert!(content.contains("{description, \"Test app\"}"));
        assert!(content.contains("{vsn, \"0.1.0\"}"));
        assert!(content.contains("'bt@my_app@counter'"));
    }

    #[test]
    fn test_build_class_module_index_skips_unreadable_file() {
        // A path that does not exist on disk — simulates an unreadable file.
        // The function should succeed (not error) and return an empty index.
        let nonexistent = Utf8PathBuf::from("/nonexistent/no_such_file.bt");
        let result = build_class_module_index(&[nonexistent], None, "my_app");
        assert!(result.is_ok());
        let (module_index, superclass_index, _class_infos, _cached_asts) = result.unwrap();
        assert!(module_index.is_empty());
        assert!(superclass_index.is_empty());
    }

    #[test]
    fn test_build_class_module_index_handles_parse_errors_gracefully() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let bad_file = project_path.join("bad.bt");
        // Write a file with a syntax error — parse diagnostics should be non-empty.
        write_test_file(&bad_file, "class Foo { @@@@invalid syntax }");

        // Should succeed and return whatever classes (if any) were parsed before the error.
        let result = build_class_module_index(&[bad_file], None, "my_app");
        assert!(result.is_ok());
    }

    /// BT-906: Verify that `build_class_module_index` correctly maps actor classes
    /// defined in subdirectories to their full subdirectory-qualified module names.
    ///
    /// When a package has `src/observer/event_bus.bt` defining `EventBus`, the index
    /// must map `EventBus → bt@gang_of_four@observer@event_bus` (not the heuristic
    /// fallback `bt@gang_of_four@event_bus` which drops the `observer@` segment).
    #[test]
    fn test_build_class_module_index_subdirectory_actor_class() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        let observer_path = src_path.join("observer");
        fs::create_dir_all(&observer_path).unwrap();

        write_test_file(
            &observer_path.join("event_bus.bt"),
            "Actor subclass: EventBus\n  notify: e => nil\n",
        );

        let source_files = vec![observer_path.join("event_bus.bt")];
        let (index, _superclass, _class_infos, _cached_asts) =
            build_class_module_index(&source_files, Some(&src_path), "gang_of_four").unwrap();

        assert_eq!(
            index.get("EventBus").map(String::as_str),
            Some("bt@gang_of_four@observer@event_bus"),
            "EventBus in observer/ subdirectory must map to full subdirectory-qualified module name"
        );
    }

    /// BT-906: Verify end-to-end that cross-file actor spawn uses the correct
    /// subdirectory-qualified module path in the generated Core Erlang.
    ///
    /// When `src/main.bt` calls `EventBus spawn` and `EventBus` is defined in
    /// `src/observer/event_bus.bt`, the generated Core Erlang must reference
    /// `bt@gang_of_four@observer@event_bus` (not `bt@gang_of_four@event_bus`).
    #[test]
    fn test_cross_file_subdirectory_actor_spawn_uses_correct_module() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        let observer_path = src_path.join("observer");
        fs::create_dir_all(&observer_path).unwrap();

        // File 1: EventBus actor in observer/ subdirectory
        write_test_file(
            &observer_path.join("event_bus.bt"),
            "Actor subclass: EventBus\n  notify: e => nil\n",
        );

        // File 2: main.bt that references EventBus spawn
        write_test_file(
            &src_path.join("main.bt"),
            "Actor subclass: Main\n  run => EventBus spawn\n",
        );

        let build_dir = project_path.join("_build/dev/ebin");
        fs::create_dir_all(&build_dir).unwrap();

        let source_files = vec![observer_path.join("event_bus.bt"), src_path.join("main.bt")];
        let (class_module_index, class_superclass_index, all_class_infos, _cached_asts) =
            build_class_module_index(&source_files, Some(&src_path), "gang_of_four").unwrap();

        // Build the class index — EventBus should map to observer subdir module
        assert_eq!(
            class_module_index.get("EventBus").map(String::as_str),
            Some("bt@gang_of_four@observer@event_bus"),
            "Index pass should map EventBus to subdirectory module"
        );

        // Compile main.bt with the index
        let core_file = build_dir.join("bt@gang_of_four@main.core");
        let options = default_options();
        compile_file(
            &src_path.join("main.bt"),
            "bt@gang_of_four@main",
            &core_file,
            &options,
            &CompileContext {
                hierarchy: ClassHierarchyContext {
                    class_module_index: class_module_index.clone(),
                    class_superclass_index: class_superclass_index.clone(),
                    pre_loaded_classes: all_class_infos.clone(),
                },
                ..CompileContext::default()
            },
            None,
        )
        .unwrap();

        let core_content = fs::read_to_string(&core_file).unwrap();

        // The spawn call must reference the full subdirectory-qualified module name
        assert!(
            core_content.contains("bt@gang_of_four@observer@event_bus"),
            "Spawn call should use observer@ subdirectory module. Generated Core Erlang:\n{core_content}"
        );
        assert!(
            !core_content.contains("'bt@gang_of_four@event_bus'"),
            "Should NOT use heuristic module name that drops observer@ segment. Generated:\n{core_content}"
        );
    }

    /// BT-1523: Verify cross-file class hierarchy resolution.
    ///
    /// When `InheritingCounter` (file 2) subclasses `Counter` (file 1) and calls
    /// `self getValue`, the type checker should NOT emit a DNU warning because
    /// `getValue` is inherited from `Counter` via the cross-file `ClassInfo` injection.
    #[test]
    fn test_cross_file_inheritance_no_false_dnu_warning() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        fs::create_dir_all(&src_path).unwrap();

        // File 1: Counter with getValue method and count state
        write_test_file(
            &src_path.join("counter.bt"),
            "Actor subclass: Counter\n  state: count = 0\n  getValue => self.count\n  increment => self.count := self.count + 1\n",
        );

        // File 2: InheritingCounter that calls inherited getValue
        write_test_file(
            &src_path.join("inheriting_counter.bt"),
            "Counter subclass: InheritingCounter\n  getDoubled => self getValue * 2\n",
        );

        let build_dir = project_path.join("_build/dev/ebin");
        fs::create_dir_all(&build_dir).unwrap();

        let source_files = vec![
            src_path.join("counter.bt"),
            src_path.join("inheriting_counter.bt"),
        ];
        let (class_module_index, class_superclass_index, all_class_infos, _cached_asts) =
            build_class_module_index(&source_files, Some(&src_path), "test_pkg").unwrap();

        // Verify ClassInfo extraction captured Counter's methods
        assert!(
            all_class_infos.iter().any(|ci| ci.name == "Counter"),
            "Pass 1 should extract ClassInfo for Counter"
        );
        let counter_info = all_class_infos
            .iter()
            .find(|ci| ci.name == "Counter")
            .unwrap();
        assert!(
            counter_info
                .methods
                .iter()
                .any(|m| m.selector == "getValue"),
            "Counter ClassInfo should include getValue method"
        );

        // Compile InheritingCounter with cross-file class infos — should succeed
        // (no false DNU error for getValue which is inherited from Counter)
        let core_file = build_dir.join("bt@test_pkg@inheriting_counter.core");
        let options = default_options();
        compile_file(
            &src_path.join("inheriting_counter.bt"),
            "bt@test_pkg@inheriting_counter",
            &core_file,
            &options,
            &CompileContext {
                hierarchy: ClassHierarchyContext {
                    class_module_index: class_module_index.clone(),
                    class_superclass_index: class_superclass_index.clone(),
                    pre_loaded_classes: all_class_infos.clone(),
                },
                ..CompileContext::default()
            },
            None,
        )
        .expect("Cross-file inheritance should compile without errors");

        assert!(core_file.exists());
    }

    #[test]
    fn test_clean_stale_artifacts_removes_orphaned_beam() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create a stale .beam and .core from a previous build
        write_test_file(&build_dir.join("bt@old_pkg@counter.beam"), "stale");
        write_test_file(&build_dir.join("bt@old_pkg@counter.core"), "stale");
        // Create the current build's .beam
        let current = build_dir.join("bt@new_pkg@counter.beam");
        write_test_file(&current, "current");
        write_test_file(&build_dir.join("bt@new_pkg@counter.core"), "current");

        clean_stale_artifacts(&build_dir, std::slice::from_ref(&current), "new_pkg").unwrap();

        // Stale files should be removed
        assert!(!build_dir.join("bt@old_pkg@counter.beam").exists());
        assert!(!build_dir.join("bt@old_pkg@counter.core").exists());
        // Current files should remain
        assert!(current.exists());
        assert!(build_dir.join("bt@new_pkg@counter.core").exists());
    }

    #[test]
    fn test_clean_stale_artifacts_removes_old_app_file() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        write_test_file(&build_dir.join("old_name.app"), "stale");
        write_test_file(&build_dir.join("new_name.app"), "current");
        let beam = build_dir.join("bt@new_name@main.beam");
        write_test_file(&beam, "current");

        clean_stale_artifacts(&build_dir, &[beam], "new_name").unwrap();

        assert!(!build_dir.join("old_name.app").exists());
        assert!(build_dir.join("new_name.app").exists());
    }

    #[test]
    fn test_clean_stale_artifacts_preserves_non_bt_files() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Non-bt@ beam files (like OTP callback modules) should be preserved
        write_test_file(&build_dir.join("beamtalk_myapp_app.beam"), "callback");
        write_test_file(&build_dir.join("beamtalk_myapp_app.erl"), "source");
        let beam = build_dir.join("bt@myapp@main.beam");
        write_test_file(&beam, "current");

        clean_stale_artifacts(&build_dir, &[beam], "myapp").unwrap();

        assert!(build_dir.join("beamtalk_myapp_app.beam").exists());
        assert!(build_dir.join("beamtalk_myapp_app.erl").exists());
    }

    // ── BT-1682: Change detection tests ──────────────────────────────

    /// Helper: create `file_module_pairs` matching the format used by the build pipeline.
    fn make_pairs(
        source_files: &[Utf8PathBuf],
        build_dir: &Utf8Path,
    ) -> Vec<(Utf8PathBuf, String, Utf8PathBuf)> {
        source_files
            .iter()
            .map(|f| {
                let stem = f.file_stem().unwrap_or("unknown");
                let module_name = format!("bt@{stem}");
                let core_file = build_dir.join(format!("{module_name}.core"));
                (f.clone(), module_name, core_file)
            })
            .collect()
    }

    #[test]
    fn test_detect_changes_new_files_no_build_dir() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        fs::create_dir_all(&src_dir).unwrap();
        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");

        // Build dir doesn't exist yet — all files should be changed
        let build_dir = project.join("build");
        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(result.changed_files.len(), 1);
        assert!(result.unchanged_files.is_empty());
        assert!(result.orphaned_beam_files.is_empty());
    }

    #[test]
    fn test_detect_changes_no_beam_exists() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(
            result.changed_files.len(),
            1,
            "New file with no .beam should be changed"
        );
        assert!(result.unchanged_files.is_empty());
    }

    #[test]
    fn test_detect_changes_up_to_date() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // Create source FIRST (older mtime)
        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");
        // Small delay to ensure mtime differs
        std::thread::sleep(std::time::Duration::from_millis(50));
        // Then create beam (newer mtime)
        write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert!(
            result.changed_files.is_empty(),
            "Up-to-date file should not be changed"
        );
        assert_eq!(result.unchanged_files.len(), 1);
    }

    #[test]
    fn test_detect_changes_source_modified() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // Create beam FIRST (older mtime)
        write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
        std::thread::sleep(std::time::Duration::from_millis(50));
        // Then modify source (newer mtime)
        write_test_file(&src_dir.join("counter.bt"), "counter := [1].");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(
            result.changed_files.len(),
            1,
            "Modified source should be changed"
        );
        assert!(result.unchanged_files.is_empty());
    }

    #[test]
    fn test_detect_changes_force_flag() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // Source older than beam — normally unchanged
        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, true);
        assert_eq!(
            result.changed_files.len(),
            1,
            "--force should mark all files as changed"
        );
        assert!(result.unchanged_files.is_empty());
    }

    #[test]
    fn test_detect_changes_orphaned_beam() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // Source file exists for counter but not for deleted_module
        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
        // Orphaned beam — no corresponding .bt
        write_test_file(&build_dir.join("bt@deleted_module.beam"), "BEAM");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(
            result.orphaned_beam_files.len(),
            1,
            "Should detect orphaned .beam"
        );
        assert!(
            result.orphaned_beam_files[0]
                .as_str()
                .contains("deleted_module"),
            "Orphaned file should be the deleted_module beam"
        );
    }

    #[test]
    fn test_detect_changes_mixed_states() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // File 1: up-to-date (source older than beam)
        write_test_file(&src_dir.join("stable.bt"), "stable := [1].");
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_test_file(&build_dir.join("bt@stable.beam"), "BEAM");

        // File 2: modified (beam older than source)
        write_test_file(&build_dir.join("bt@changed.beam"), "BEAM");
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_test_file(&src_dir.join("changed.bt"), "changed := [2].");

        // File 3: new (no beam exists)
        write_test_file(&src_dir.join("new_file.bt"), "new_file := [3].");

        let source_files = vec![
            src_dir.join("changed.bt"),
            src_dir.join("new_file.bt"),
            src_dir.join("stable.bt"),
        ];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(
            result.changed_files.len(),
            2,
            "changed + new_file should need compilation"
        );
        assert_eq!(
            result.unchanged_files.len(),
            1,
            "stable should be unchanged"
        );
    }

    #[test]
    fn test_detect_changes_non_bt_beams_ignored() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let build_dir = project.join("build");
        fs::create_dir_all(&build_dir).unwrap();

        // Non-bt@ beam files should NOT appear as orphaned
        write_test_file(&build_dir.join("beamtalk_app.beam"), "callback");
        write_test_file(&build_dir.join("some_otp_module.beam"), "otp");

        let source_files: Vec<Utf8PathBuf> = Vec::new();
        let pairs: Vec<(Utf8PathBuf, String, Utf8PathBuf)> = Vec::new();

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert!(
            result.orphaned_beam_files.is_empty(),
            "Non-bt@ beam files should not be flagged as orphaned"
        );
    }

    #[test]
    fn test_rebar3_path_returns_bundled_when_exists() {
        // When running from the repo root (or with BEAMTALK_RUNTIME_DIR set),
        // rebar3_path() should find the bundled copy at runtime/tools/rebar3.
        let result = rebar3_path();
        // In CI or dev, the bundled rebar3 should be present.
        // If neither bundled nor system rebar3 is available, this test will
        // fail — that's intentional, as it means the vendored copy is missing.
        assert!(
            result.is_ok(),
            "rebar3_path() should find rebar3: {result:?}"
        );

        let path = result.unwrap();
        // When running in the dev repo, the path should be the bundled copy
        assert!(
            path.ends_with("tools/rebar3"),
            "Expected bundled rebar3 path ending in 'tools/rebar3', got: {path:?}"
        );
    }

    // ---- ADR 0072 Phase 1: native Erlang compilation in build ----

    #[test]
    fn test_build_without_native_dir_is_unaffected() {
        // Packages without native/ should build exactly as before.
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"no_native\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                eprintln!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }

        // native ebin should NOT be created
        let test_layout = BuildLayout::new(&project_path);
        let native_ebin = test_layout.native_ebin_dir();
        assert!(
            !native_ebin.exists(),
            "native ebin should not be created when no native/ directory exists"
        );
    }

    #[test]
    #[ignore = "requires erlc"]
    fn test_build_with_native_erlang() {
        // ADR 0072 Phase 1: build should discover and compile native/*.erl files
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        // Create beamtalk.toml
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"with_native\"\nversion = \"0.1.0\"\n",
        );

        // Create a .bt source file
        write_test_file(&src_path.join("main.bt"), "main := [42].");

        // Create native/ with an Erlang module
        let native_dir = project_path.join("native");
        fs::create_dir(&native_dir).unwrap();
        write_test_file(
            &native_dir.join("hello_native.erl"),
            "-module(hello_native).\n-export([greet/0]).\ngreet() -> <<\"hello\">>.\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);
        assert!(result.is_ok(), "Build should succeed: {result:?}");

        // Native BEAM file should exist in _build/dev/native/ebin/
        let test_layout = BuildLayout::new(&project_path);
        let native_beam = test_layout.native_ebin_dir().join("hello_native.beam");
        assert!(
            native_beam.exists(),
            "Native BEAM file should be produced at {native_beam}"
        );

        // Regular .bt BEAM should also exist
        let bt_beam = test_layout.ebin_dir().join("bt@with_native@main.beam");
        assert!(
            bt_beam.exists(),
            "Beamtalk BEAM file should be produced at {bt_beam}"
        );

        // ADR 0072: .app file should include native_modules in env
        let app_file = test_layout.ebin_dir().join("with_native.app");
        assert!(app_file.exists(), "Expected .app file at {app_file}");
        let app_content = fs::read_to_string(&app_file).unwrap();
        assert!(
            app_content.contains("{native_modules, [hello_native]}"),
            "Generated .app should contain native_modules. Got: {app_content}"
        );
    }

    // ---- ADR 0072 Phase 1: native module collision detection ----

    #[test]
    fn test_no_collision_single_package_no_native() {
        // Packages without native/ should pass collision check trivially.
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);

        let result = check_native_module_collisions(&project_path, "test_pkg", &[]);
        assert!(result.is_ok(), "No native dir should pass: {result:?}");
    }

    #[test]
    fn test_no_collision_single_package_with_native() {
        // A single package with native modules should not trigger collision.
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n",
        );

        let native_dir = project_path.join("native");
        fs::create_dir(&native_dir).unwrap();
        write_test_file(
            &native_dir.join("my_helper.erl"),
            "-module(my_helper).\n-export([hello/0]).\nhello() -> ok.\n",
        );

        let result = check_native_module_collisions(&project_path, "my_pkg", &[]);
        assert!(result.is_ok(), "Single package should pass: {result:?}");
    }

    #[test]
    fn test_collision_between_root_and_dependency() {
        // Root package and a dependency both define the same native module name.
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"root_pkg\"\nversion = \"0.1.0\"\n",
        );

        // Root package has native/utils.erl
        let root_native = project_path.join("native");
        fs::create_dir(&root_native).unwrap();
        write_test_file(
            &root_native.join("utils.erl"),
            "-module(utils).\n-export([]).\n",
        );

        // Dependency also has native/utils.erl
        let dep_dir = temp.path().join("dep_pkg");
        fs::create_dir_all(dep_dir.join("native")).unwrap();
        fs::create_dir_all(dep_dir.join("src")).unwrap();
        fs::write(
            dep_dir.join("native").join("utils.erl"),
            "-module(utils).\n-export([]).\n",
        )
        .unwrap();

        let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();
        let deps = vec![super::super::deps::path::ResolvedDependency {
            name: "dep_pkg".to_string(),
            root: dep_root.clone(),
            ebin_path: dep_root.join("ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
        }];

        let result = check_native_module_collisions(&project_path, "root_pkg", &deps);
        assert!(result.is_err(), "Should detect collision");

        let err_msg = format!("{:?}", result.unwrap_err());
        assert!(
            err_msg.contains("utils"),
            "Error should name the conflicting module: {err_msg}"
        );
        assert!(
            err_msg.contains("root_pkg"),
            "Error should name the root package: {err_msg}"
        );
        assert!(
            err_msg.contains("dep_pkg"),
            "Error should name the dependency: {err_msg}"
        );
    }

    #[test]
    fn test_collision_between_two_dependencies() {
        // Two dependencies define the same native module name (root has none).
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"root_pkg\"\nversion = \"0.1.0\"\n",
        );

        // first_dep has native/shared_mod.erl
        let first_dep_dir = temp.path().join("dep_a");
        fs::create_dir_all(first_dep_dir.join("native")).unwrap();
        fs::create_dir_all(first_dep_dir.join("src")).unwrap();
        fs::write(
            first_dep_dir.join("native").join("shared_mod.erl"),
            "-module(shared_mod).\n-export([]).\n",
        )
        .unwrap();

        // second_dep also has native/shared_mod.erl
        let second_dep_dir = temp.path().join("dep_b");
        fs::create_dir_all(second_dep_dir.join("native")).unwrap();
        fs::create_dir_all(second_dep_dir.join("src")).unwrap();
        fs::write(
            second_dep_dir.join("native").join("shared_mod.erl"),
            "-module(shared_mod).\n-export([]).\n",
        )
        .unwrap();

        let first_root = Utf8PathBuf::from_path_buf(first_dep_dir).unwrap();
        let second_root = Utf8PathBuf::from_path_buf(second_dep_dir).unwrap();
        let deps = vec![
            super::super::deps::path::ResolvedDependency {
                name: "dep_a".to_string(),
                root: first_root.clone(),
                ebin_path: first_root.join("ebin"),
                class_module_index: HashMap::new(),
                class_infos: Vec::new(),
                is_direct: true,
                via_chain: Vec::new(),
            },
            super::super::deps::path::ResolvedDependency {
                name: "dep_b".to_string(),
                root: second_root.clone(),
                ebin_path: second_root.join("ebin"),
                class_module_index: HashMap::new(),
                class_infos: Vec::new(),
                is_direct: true,
                via_chain: Vec::new(),
            },
        ];

        let result = check_native_module_collisions(&project_path, "root_pkg", &deps);
        assert!(result.is_err(), "Should detect collision between deps");

        let err_msg = format!("{:?}", result.unwrap_err());
        assert!(
            err_msg.contains("shared_mod"),
            "Error should name the module: {err_msg}"
        );
        assert!(
            err_msg.contains("dep_a"),
            "Error should name dep_a: {err_msg}"
        );
        assert!(
            err_msg.contains("dep_b"),
            "Error should name dep_b: {err_msg}"
        );
    }

    #[test]
    fn test_no_collision_different_native_modules() {
        // Root and dependency have native modules with different names — no collision.
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"root_pkg\"\nversion = \"0.1.0\"\n",
        );

        // Root has native/root_utils.erl
        let root_native = project_path.join("native");
        fs::create_dir(&root_native).unwrap();
        write_test_file(
            &root_native.join("root_utils.erl"),
            "-module(root_utils).\n-export([]).\n",
        );

        // Dependency has native/dep_utils.erl
        let dep_dir = temp.path().join("dep_pkg");
        fs::create_dir_all(dep_dir.join("native")).unwrap();
        fs::create_dir_all(dep_dir.join("src")).unwrap();
        fs::write(
            dep_dir.join("native").join("dep_utils.erl"),
            "-module(dep_utils).\n-export([]).\n",
        )
        .unwrap();

        let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();
        let deps = vec![super::super::deps::path::ResolvedDependency {
            name: "dep_pkg".to_string(),
            root: dep_root.clone(),
            ebin_path: dep_root.join("ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
        }];

        let result = check_native_module_collisions(&project_path, "root_pkg", &deps);
        assert!(
            result.is_ok(),
            "Different module names should pass: {result:?}"
        );
    }

    // ---- ADR 0072 Phase 2: rebar3 integration tests ----

    #[test]
    fn test_generate_rebar_config_basic() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let mut deps = NativeDependencyMap::new();
        deps.insert(
            "gun".to_string(),
            manifest::NativeDependency {
                name: "gun".to_string(),
                constraint: "~> 2.1".to_string(),
            },
        );
        deps.insert(
            "cowboy".to_string(),
            manifest::NativeDependency {
                name: "cowboy".to_string(),
                constraint: "~> 2.12".to_string(),
            },
        );

        let config_path = generate_rebar_config(&project_path, &deps, None).unwrap();

        assert!(config_path.exists(), "rebar.config should be created");
        let content = fs::read_to_string(&config_path).unwrap();
        assert!(
            content.contains("Auto-generated by beamtalk build"),
            "Should have header comment"
        );
        assert!(
            content.contains("{gun, \"~> 2.1\"}"),
            "Should contain gun dep"
        );
        assert!(
            content.contains("{cowboy, \"~> 2.12\"}"),
            "Should contain cowboy dep"
        );
        assert!(
            content.contains("{erl_opts, [debug_info, warn_missing_doc]}"),
            "Should include debug_info and warn_missing_doc"
        );
        // No native/ directory — should NOT include src_dirs
        assert!(
            !content.contains("src_dirs"),
            "Should not include src_dirs without native/"
        );
        assert!(
            !content.contains("include_dirs"),
            "Should not include include_dirs without native/include/"
        );
    }

    #[test]
    fn test_generate_rebar_config_with_native_dir() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create native/ directory
        let native_dir = project_path.join("native");
        fs::create_dir(&native_dir).unwrap();

        let mut deps = NativeDependencyMap::new();
        deps.insert(
            "jason".to_string(),
            manifest::NativeDependency {
                name: "jason".to_string(),
                constraint: "~> 1.4".to_string(),
            },
        );

        let config_path = generate_rebar_config(&project_path, &deps, None).unwrap();
        let content = fs::read_to_string(&config_path).unwrap();

        // rebar3 only handles hex deps — native/*.erl compiled separately via erlc
        assert!(
            !content.contains("src_dirs"),
            "rebar.config should not contain src_dirs (native .erl compiled via erlc)"
        );
        assert!(
            !content.contains("include_dirs"),
            "rebar.config should not contain include_dirs"
        );
    }

    #[test]
    fn test_generate_rebar_config_with_native_include() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create native/ and native/include/
        let native_dir = project_path.join("native");
        fs::create_dir(&native_dir).unwrap();
        let include_dir = native_dir.join("include");
        fs::create_dir(&include_dir).unwrap();

        let mut deps = NativeDependencyMap::new();
        deps.insert(
            "gun".to_string(),
            manifest::NativeDependency {
                name: "gun".to_string(),
                constraint: "~> 2.1".to_string(),
            },
        );

        let config_path = generate_rebar_config(&project_path, &deps, None).unwrap();
        let content = fs::read_to_string(&config_path).unwrap();

        // rebar3 only handles hex deps — native/*.erl compiled separately via erlc
        assert!(
            !content.contains("src_dirs"),
            "rebar.config should not contain src_dirs (native .erl compiled via erlc)"
        );
        assert!(
            !content.contains("include_dirs"),
            "rebar.config should not contain include_dirs (resolved via -I in erlc)"
        );
    }

    #[test]
    fn test_collect_rebar3_ebin_paths_empty() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&project_root);

        let paths = collect_rebar3_ebin_paths(&layout);
        assert!(paths.is_empty(), "Should return empty for nonexistent tree");
    }

    #[test]
    fn test_collect_rebar3_ebin_paths_finds_deps() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&project_root);

        // Create rebar3-style output tree under _build/dev/native/default/lib/
        let lib_dir = layout.rebar_lib_dir();
        let gun_ebin = lib_dir.join("gun").join("ebin");
        let cowboy_ebin = lib_dir.join("cowboy").join("ebin");
        let ranch_ebin = lib_dir.join("ranch").join("ebin");
        fs::create_dir_all(&gun_ebin).unwrap();
        fs::create_dir_all(&cowboy_ebin).unwrap();
        fs::create_dir_all(&ranch_ebin).unwrap();

        let paths = collect_rebar3_ebin_paths(&layout);
        assert_eq!(paths.len(), 3, "Should find 3 ebin directories");

        // Verify all paths end with /ebin and contain expected app names
        let path_strs: Vec<&str> = paths.iter().map(|p| p.as_str()).collect();
        assert!(
            path_strs.iter().any(|p| p.contains("cowboy")),
            "Should find cowboy ebin"
        );
        assert!(
            path_strs.iter().any(|p| p.contains("gun")),
            "Should find gun ebin"
        );
        assert!(
            path_strs.iter().any(|p| p.contains("ranch")),
            "Should find ranch ebin"
        );
    }

    #[test]
    fn test_aggregate_native_dependencies_root_only() {
        let mut root_deps = NativeDependencyMap::new();
        root_deps.insert(
            "gun".to_string(),
            manifest::NativeDependency {
                name: "gun".to_string(),
                constraint: "~> 2.1".to_string(),
            },
        );

        let resolved: Vec<super::super::deps::path::ResolvedDependency> = Vec::new();
        let aggregated = aggregate_native_dependencies(&root_deps, &resolved);

        assert_eq!(aggregated.len(), 1);
        assert_eq!(aggregated["gun"].constraint, "~> 2.1");
    }

    #[test]
    fn test_generate_rebar_config_path_lives_in_build_tree() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let mut deps = NativeDependencyMap::new();
        deps.insert(
            "gun".to_string(),
            manifest::NativeDependency {
                name: "gun".to_string(),
                constraint: "~> 2.1".to_string(),
            },
        );

        let config_path = generate_rebar_config(&project_path, &deps, None).unwrap();

        // Config should be in _build/dev/native/
        let expected_suffix = Utf8PathBuf::from("_build/dev/native/rebar.config");
        assert!(
            config_path.ends_with(&expected_suffix),
            "Config should live in build tree, got: {config_path}"
        );
    }

    #[test]
    fn test_generate_rebar_config_with_locked_versions() {
        use crate::commands::deps::lockfile::NativePackageLock;

        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let mut deps = NativeDependencyMap::new();
        deps.insert(
            "gun".to_string(),
            manifest::NativeDependency {
                name: "gun".to_string(),
                constraint: "~> 2.1".to_string(),
            },
        );
        deps.insert(
            "cowboy".to_string(),
            manifest::NativeDependency {
                name: "cowboy".to_string(),
                constraint: "~> 2.12".to_string(),
            },
        );

        // Provide locked versions — should use exact versions instead of constraints
        let mut locked = std::collections::BTreeMap::new();
        locked.insert(
            "gun".to_string(),
            NativePackageLock {
                name: "gun".to_string(),
                version: "2.1.3".to_string(),
                sha: "abc123".to_string(),
            },
        );
        locked.insert(
            "cowboy".to_string(),
            NativePackageLock {
                name: "cowboy".to_string(),
                version: "2.12.0".to_string(),
                sha: "def456".to_string(),
            },
        );

        let config_path = generate_rebar_config(&project_path, &deps, Some(&locked)).unwrap();
        let content = fs::read_to_string(&config_path).unwrap();

        // Should use exact locked versions, not constraints
        assert!(
            content.contains("{gun, \"2.1.3\"}"),
            "Should use locked version for gun, got: {content}"
        );
        assert!(
            content.contains("{cowboy, \"2.12.0\"}"),
            "Should use locked version for cowboy, got: {content}"
        );
        // Should NOT contain constraint strings
        assert!(
            !content.contains("~>"),
            "Should not contain constraint operators when locked"
        );
    }

    #[test]
    fn test_generate_rebar_config_partial_lock() {
        use crate::commands::deps::lockfile::NativePackageLock;

        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let mut deps = NativeDependencyMap::new();
        deps.insert(
            "gun".to_string(),
            manifest::NativeDependency {
                name: "gun".to_string(),
                constraint: "~> 2.1".to_string(),
            },
        );
        deps.insert(
            "cowboy".to_string(),
            manifest::NativeDependency {
                name: "cowboy".to_string(),
                constraint: "~> 2.12".to_string(),
            },
        );

        // Only lock gun — cowboy should use constraint
        let mut locked = std::collections::BTreeMap::new();
        locked.insert(
            "gun".to_string(),
            NativePackageLock {
                name: "gun".to_string(),
                version: "2.1.3".to_string(),
                sha: "abc123".to_string(),
            },
        );

        let config_path = generate_rebar_config(&project_path, &deps, Some(&locked)).unwrap();
        let content = fs::read_to_string(&config_path).unwrap();

        assert!(
            content.contains("{gun, \"2.1.3\"}"),
            "Should use locked version for gun"
        );
        assert!(
            content.contains("{cowboy, \"~> 2.12\"}"),
            "Should use constraint for unlocked cowboy"
        );
    }

    /// BT-1722: Verify that `generate_package_corpus` produces `class_corpus.json`
    /// and `corpus.json` in the output directory.
    #[test]
    fn test_generate_package_corpus_creates_files() {
        let temp = TempDir::new().unwrap();
        let corpus_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = corpus_dir.join("src");
        fs::create_dir_all(&src_dir).unwrap();

        // Write a simple .bt source file
        let bt_file = src_dir.join("greeter.bt");
        write_test_file(
            &bt_file,
            "// A simple greeter class.\nObject subclass: Greeter\n  greet: name => \"Hello, \" ++ name\n",
        );

        // Use build_class_module_index to extract ClassInfo from source
        let source_files = vec![bt_file];
        let (_module_index, _superclass_index, class_infos, _cached_asts) =
            build_class_module_index(&source_files, Some(&src_dir), "my_pkg").unwrap();

        let result = generate_package_corpus(&corpus_dir, "my_pkg", &class_infos, &source_files);
        assert!(result.is_ok(), "generate_package_corpus should succeed");

        // Verify class_corpus.json was created
        let class_corpus_path = corpus_dir.join("class_corpus.json");
        assert!(
            class_corpus_path.exists(),
            "Expected class_corpus.json at {class_corpus_path}"
        );
        let class_content = fs::read_to_string(&class_corpus_path).unwrap();
        let class_entries: Vec<serde_json::Value> = serde_json::from_str(&class_content).unwrap();
        assert_eq!(class_entries.len(), 1);
        assert_eq!(class_entries[0]["name"], "Greeter");
        assert_eq!(class_entries[0]["superclass"], "Object");
        assert!(
            class_entries[0]["methods"]
                .as_array()
                .unwrap()
                .iter()
                .any(|m| m == "greet:")
        );

        // Verify corpus.json was created
        let corpus_path = corpus_dir.join("corpus.json");
        assert!(
            corpus_path.exists(),
            "Expected corpus.json at {corpus_path}"
        );
        let corpus_content = fs::read_to_string(&corpus_path).unwrap();
        let corpus: serde_json::Value = serde_json::from_str(&corpus_content).unwrap();
        let entries = corpus["entries"].as_array().unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0]["category"], "package-my_pkg");
        assert!(
            entries[0]["tags"]
                .as_array()
                .unwrap()
                .iter()
                .any(|t| t == "my_pkg")
        );
    }

    /// BT-1722: Internal classes should be excluded from the generated class corpus.
    #[test]
    fn test_generate_package_corpus_excludes_internal_classes() {
        let temp = TempDir::new().unwrap();
        let corpus_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = corpus_dir.join("src");
        fs::create_dir_all(&src_dir).unwrap();

        // Write a .bt file with an internal class
        let bt_file = src_dir.join("internal_helper.bt");
        write_test_file(
            &bt_file,
            "internal Object subclass: InternalHelper\n  help => nil\n",
        );

        let source_files = vec![bt_file];
        let (_module_index, _superclass_index, class_infos, _cached_asts) =
            build_class_module_index(&source_files, Some(&src_dir), "my_pkg").unwrap();

        // Verify the class was parsed as internal
        assert!(
            class_infos.iter().all(|ci| ci.is_internal),
            "All classes should be internal"
        );

        let result = generate_package_corpus(&corpus_dir, "my_pkg", &class_infos, &[]);
        assert!(result.is_ok());

        // class_corpus.json should not be created (no non-internal classes)
        let class_corpus_path = corpus_dir.join("class_corpus.json");
        assert!(
            !class_corpus_path.exists(),
            "class_corpus.json should not be created for internal-only classes"
        );
    }

    // ---- Transitive native dependency detection tests (ADR 0072) ----

    /// Helper: create a minimal beamtalk.toml with native deps in a temp dir.
    fn write_dep_manifest(dir: &std::path::Path, name: &str, native_deps: &[(&str, &str)]) {
        let manifest_dir = dir.join(name);
        fs::create_dir_all(&manifest_dir).unwrap();
        let mut toml =
            format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n\n[native.dependencies]\n");
        for (dep_name, constraint) in native_deps {
            use std::fmt::Write;
            let _ = writeln!(toml, "{dep_name} = \"{constraint}\"");
        }
        fs::write(manifest_dir.join("beamtalk.toml"), toml).unwrap();
    }

    /// Helper: create a `ResolvedDependency` pointing at a temp dir.
    fn make_resolved_dep(
        root: &std::path::Path,
        name: &str,
    ) -> super::super::deps::path::ResolvedDependency {
        super::super::deps::path::ResolvedDependency {
            name: name.to_string(),
            root: Utf8PathBuf::from_path_buf(root.join(name)).unwrap(),
            ebin_path: Utf8PathBuf::from_path_buf(root.join(name).join("ebin")).unwrap(),
            class_module_index: std::collections::HashMap::new(),
            class_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
        }
    }

    #[test]
    fn test_aggregate_native_deps_from_transitive_dep() {
        let temp = TempDir::new().unwrap();
        write_dep_manifest(
            temp.path(),
            "http",
            &[("cowboy", "~> 2.12"), ("gun", "~> 2.1")],
        );

        let root_deps = NativeDependencyMap::new(); // root has NO native deps
        let resolved = [make_resolved_dep(temp.path(), "http")];

        let aggregated = aggregate_native_dependencies(&root_deps, &resolved);

        assert_eq!(
            aggregated.len(),
            2,
            "Should aggregate 2 native deps from transitive dep"
        );
        assert_eq!(aggregated["cowboy"].constraint, "~> 2.12");
        assert_eq!(aggregated["gun"].constraint, "~> 2.1");
    }

    #[test]
    fn test_aggregate_native_deps_root_takes_precedence() {
        let temp = TempDir::new().unwrap();
        write_dep_manifest(temp.path(), "http", &[("cowboy", "~> 2.10")]);

        let mut root_deps = NativeDependencyMap::new();
        root_deps.insert(
            "cowboy".to_string(),
            manifest::NativeDependency {
                name: "cowboy".to_string(),
                constraint: "~> 2.12".to_string(),
            },
        );

        let resolved = [make_resolved_dep(temp.path(), "http")];
        let aggregated = aggregate_native_dependencies(&root_deps, &resolved);

        assert_eq!(aggregated.len(), 1);
        assert_eq!(
            aggregated["cowboy"].constraint, "~> 2.12",
            "Root constraint should take precedence over transitive dep"
        );
    }

    #[test]
    fn test_aggregate_native_deps_dep_without_manifest() {
        let temp = TempDir::new().unwrap();
        // Create dep dir with NO beamtalk.toml
        fs::create_dir(temp.path().join("orphan")).unwrap();

        let root_deps = NativeDependencyMap::new();
        let resolved = [make_resolved_dep(temp.path(), "orphan")];
        let aggregated = aggregate_native_dependencies(&root_deps, &resolved);

        assert!(aggregated.is_empty(), "Should skip deps without manifests");
    }

    #[test]
    fn test_has_native_deps_detects_transitive() {
        // Simulates the has_native_deps check from build() — verifies that
        // transitive deps with [native.dependencies] are detected even when
        // the root project has none.
        let temp = TempDir::new().unwrap();
        write_dep_manifest(temp.path(), "http", &[("gun", "~> 2.1")]);

        let resolved = [make_resolved_dep(temp.path(), "http")];

        let has_native_deps = resolved.iter().any(|dep| {
            let manifest_path = dep.root.join("beamtalk.toml");
            manifest_path
                .exists()
                .then(|| manifest::parse_manifest_full(&manifest_path).ok())
                .flatten()
                .is_some_and(|m| !m.native_dependencies.is_empty())
        });

        assert!(
            has_native_deps,
            "Should detect native deps from transitive dependency"
        );
    }

    #[test]
    fn test_has_native_deps_false_when_no_native_anywhere() {
        let temp = TempDir::new().unwrap();
        // Dep with NO native.dependencies section
        let dep_dir = temp.path().join("utils");
        fs::create_dir(&dep_dir).unwrap();
        fs::write(
            dep_dir.join("beamtalk.toml"),
            "[package]\nname = \"utils\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let resolved = [make_resolved_dep(temp.path(), "utils")];

        let has_native_deps = resolved.iter().any(|dep| {
            let manifest_path = dep.root.join("beamtalk.toml");
            manifest_path
                .exists()
                .then(|| manifest::parse_manifest_full(&manifest_path).ok())
                .flatten()
                .is_some_and(|m| !m.native_dependencies.is_empty())
        });

        assert!(
            !has_native_deps,
            "Should be false when no native deps anywhere"
        );
    }

    // ── BT-1730: Class module header tests ──────────────────────────────────

    #[test]
    fn test_generate_class_header_creates_hrl_with_macros() {
        let temp = TempDir::new().unwrap();
        let include_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let mut index = HashMap::new();
        index.insert(
            "HTTPResponse".to_string(),
            "bt@http@httpresponse".to_string(),
        );
        index.insert("HTTPRequest".to_string(), "bt@http@httprequest".to_string());

        generate_class_header(&include_dir, &index).unwrap();

        let hrl_path = include_dir.join("beamtalk_classes.hrl");
        assert!(hrl_path.exists(), "Header file should be created");

        let content = fs::read_to_string(&hrl_path).unwrap();
        assert!(
            content.contains("-define(BT_CLASS_MODULE_HTTPRequest, 'bt@http@httprequest')."),
            "Should contain HTTPRequest macro"
        );
        assert!(
            content.contains("-define(BT_CLASS_MODULE_HTTPResponse, 'bt@http@httpresponse')."),
            "Should contain HTTPResponse macro"
        );
        assert!(
            content.contains("-ifndef(BEAMTALK_CLASSES_HRL)."),
            "Should have include guard"
        );
    }

    #[test]
    fn test_generate_class_header_sorted_deterministic() {
        let temp = TempDir::new().unwrap();
        let include_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let mut index = HashMap::new();
        index.insert("Zeta".to_string(), "bt@pkg@zeta".to_string());
        index.insert("Alpha".to_string(), "bt@pkg@alpha".to_string());
        index.insert("Middle".to_string(), "bt@pkg@middle".to_string());

        generate_class_header(&include_dir, &index).unwrap();

        let content = fs::read_to_string(include_dir.join("beamtalk_classes.hrl")).unwrap();
        let alpha_pos = content.find("BT_CLASS_MODULE_Alpha").unwrap();
        let middle_pos = content.find("BT_CLASS_MODULE_Middle").unwrap();
        let zeta_pos = content.find("BT_CLASS_MODULE_Zeta").unwrap();
        assert!(
            alpha_pos < middle_pos && middle_pos < zeta_pos,
            "Macros should be sorted alphabetically"
        );
    }

    #[test]
    fn test_generate_class_header_empty_index() {
        let temp = TempDir::new().unwrap();
        let include_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let index = HashMap::new();
        generate_class_header(&include_dir, &index).unwrap();

        let content = fs::read_to_string(include_dir.join("beamtalk_classes.hrl")).unwrap();
        assert!(
            content.contains("-ifndef(BEAMTALK_CLASSES_HRL)."),
            "Should still have include guard even with empty index"
        );
        assert!(
            !content.contains("-define(BT_CLASS_MODULE_"),
            "Should have no macro definitions with empty index"
        );
    }

    #[test]
    fn test_validate_native_class_references_warns_on_hardcoded() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let native_dir = project_root.join("native");
        fs::create_dir_all(&native_dir).unwrap();

        // Write a .erl file with a hardcoded reference
        fs::write(
            native_dir.join("test_mod.erl"),
            "-module(test_mod).\n\
             -export([f/0]).\n\
             f() -> 'bt@mypkg@myclass':hello().\n",
        )
        .unwrap();

        let mut index = HashMap::new();
        index.insert("MyClass".to_string(), "bt@mypkg@myclass".to_string());

        // Should not error (just warns to stderr)
        validate_native_class_references(&project_root, "mypkg", &index).unwrap();
    }

    #[test]
    fn test_validate_native_class_references_skips_comments() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let native_dir = project_root.join("native");
        fs::create_dir_all(&native_dir).unwrap();

        // Write a .erl file with reference only in a comment
        fs::write(
            native_dir.join("test_mod.erl"),
            "-module(test_mod).\n\
             %% This references 'bt@mypkg@myclass' but only in a comment.\n",
        )
        .unwrap();

        let index = HashMap::new();
        // Should not warn (comment-only references are fine)
        validate_native_class_references(&project_root, "mypkg", &index).unwrap();
    }

    #[test]
    fn test_validate_native_class_references_no_native_dir() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        // No native/ directory — should be a no-op
        let index = HashMap::new();
        validate_native_class_references(&project_root, "mypkg", &index).unwrap();
    }

    // ---- compile_native_erlang_with_deps tests ----

    #[test]
    fn test_compile_native_erlang_with_deps_no_native_dir() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
        assert!(
            result.is_none(),
            "Should return None when no native/ directory exists"
        );
    }

    #[test]
    fn test_compile_native_erlang_with_deps_empty_native_dir() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::create_dir(project_root.join("native")).unwrap();

        let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
        assert!(
            result.is_none(),
            "Should return None when native/ has no .erl files"
        );
    }

    #[test]
    fn test_compile_native_erlang_with_deps_ignores_non_erl_files() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let native_dir = project_root.join("native");
        fs::create_dir(&native_dir).unwrap();

        // Only non-.erl files
        fs::write(native_dir.join("readme.md"), "# Native stuff").unwrap();
        fs::write(native_dir.join("notes.txt"), "notes").unwrap();

        let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
        assert!(
            result.is_none(),
            "Should return None when no .erl files found"
        );
    }

    #[test]
    #[ignore = "requires erlc"]
    fn test_compile_native_erlang_with_deps_simple_module() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let native_dir = project_root.join("native");
        fs::create_dir(&native_dir).unwrap();

        fs::write(
            native_dir.join("hello_native.erl"),
            "-module(hello_native).\n-export([greet/0]).\ngreet() -> <<\"hello from native\">>.\n",
        )
        .unwrap();

        let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
        assert!(result.is_some(), "Should compile native .erl files");

        let ebin_dir = result.unwrap();
        assert!(
            ebin_dir.ends_with("_build/dev/native/ebin"),
            "ebin_dir should be _build/dev/native/ebin, got: {ebin_dir}"
        );
        assert!(
            ebin_dir.join("hello_native.beam").exists(),
            "BEAM file should exist"
        );
    }

    #[test]
    #[ignore = "requires erlc"]
    fn test_compile_native_erlang_with_deps_with_include() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let native_dir = project_root.join("native");
        let include_dir = native_dir.join("include");
        fs::create_dir_all(&include_dir).unwrap();

        fs::write(include_dir.join("my_defs.hrl"), "-define(MY_CONST, 42).\n").unwrap();

        fs::write(
            native_dir.join("with_include.erl"),
            "-module(with_include).\n-include(\"my_defs.hrl\").\n-export([value/0]).\nvalue() -> ?MY_CONST.\n",
        )
        .unwrap();

        let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
        assert!(result.is_some(), "Should compile with includes");
        let ebin_dir = result.unwrap();
        assert!(ebin_dir.join("with_include.beam").exists());
    }

    #[test]
    #[ignore = "requires erlc"]
    fn test_compile_native_erlang_with_deps_compile_error() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let native_dir = project_root.join("native");
        fs::create_dir(&native_dir).unwrap();

        // Write truly broken syntax (undefined calls just warn, not error)
        fs::write(
            native_dir.join("broken.erl"),
            "-module(broken).\n-export([oops/0]).\noops( -> ok.\n",
        )
        .unwrap();

        let result = compile_native_erlang_with_deps(&project_root, &[]);
        assert!(result.is_err(), "Should fail with syntax error");
        let err_msg = format!("{}", result.unwrap_err());
        assert!(
            err_msg.contains("Native Erlang compilation failed"),
            "Error should mention native compilation: {err_msg}"
        );
    }

    // ---- collect_erl_files tests ----

    #[test]
    fn test_collect_erl_files_filters_correctly() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        fs::write(dir.join("foo.erl"), "-module(foo).").unwrap();
        fs::write(dir.join("bar.erl"), "-module(bar).").unwrap();
        fs::write(dir.join("readme.md"), "# Notes").unwrap();
        fs::write(dir.join("data.json"), "{}").unwrap();

        let mut files = Vec::new();
        collect_erl_files(&dir, &mut files).unwrap();
        files.sort();

        assert_eq!(files.len(), 2);
        assert!(files[0].as_str().ends_with("bar.erl"));
        assert!(files[1].as_str().ends_with("foo.erl"));
    }
}
