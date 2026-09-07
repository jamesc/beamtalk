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
use tracing::{debug, error, info, instrument, warn};

use super::OutputFormat;
use super::app_file;
use super::manifest;
use super::manifest::NativeDependencyMap;
use super::util::content_hash_of;

/// Result of per-file change detection.
///
/// Compares each `.bt` source file's content hash against the hash recorded
/// for its corresponding `.beam` output (BT-3120) to determine which files
/// need recompilation.
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
    /// Content hash of every source file considered in this pass, keyed by
    /// path string (BT-3120). Callers persist this via
    /// `build_cache::save_beam_hash_cache` after a successful build, so the
    /// next `detect_changes` call has something to compare against.
    pub source_hashes: HashMap<String, String>,
}

/// Detect which source files have changed relative to their compiled `.beam` output.
///
/// For each `.bt` source file, computes the expected `.beam` filename in `build_dir`
/// using the same module naming scheme as the build pipeline. A file is considered
/// changed if:
/// - Its `.beam` does not exist (new file or first build)
/// - Its content hash differs from the hash recorded for it in the beam-hash
///   sidecar (BT-3120), including when no hash was recorded at all
///
/// mtime is deliberately not used for this decision: it lies under git
/// operations (branch switches restore old content under a fresh mtime) and
/// under tools that preserve or backdate mtimes on write, either of which can
/// make an mtime-keyed check serve a stale `.beam`.
///
/// Also detects orphaned `.beam` files (no corresponding `.bt` source) and reports
/// them as warnings.
///
/// When `force` is true, all source files are treated as changed regardless of
/// their recorded hash.
///
/// `known_hashes` is an optional set of already-computed content hashes,
/// keyed by path string — for a manifest-based package build, `build_rs`'s
/// Pass 1 (`build_cache::incremental_build_class_module_index`) has already
/// hashed every source file's content this same build (BT-3120), so a hit
/// here skips reading and hashing that file's content a second time. A miss
/// (manifest-less builds, where Pass 1 never runs; or any file Pass 1
/// couldn't read) falls back to hashing it directly here, same as before.
/// `known_hashes` is mainly an optimization, but not purely one: a file
/// edited in the window between Pass 1 hashing it and this call reuses the
/// now-stale Pass-1-time hash for this build cycle. That's narrow and
/// self-healing (Pass 1 always re-reads from disk on the next build), never
/// a permanent stale skip, but it means a same-build edit isn't caught as
/// immediately as the old mtime-based check (which re-read mtime fresh at
/// this call site).
pub(crate) fn detect_changes(
    source_files: &[Utf8PathBuf],
    build_dir: &Utf8Path,
    file_module_pairs: &[(Utf8PathBuf, String, Utf8PathBuf)],
    force: bool,
    known_hashes: &HashMap<String, String>,
) -> ChangeDetectionResult {
    // BT-3120: content hash of each source file as of the last successful
    // `.beam` build, keyed by path string.
    let previous_hashes = super::build_cache::load_beam_hash_cache(build_dir);

    // Hash every source file up front — both to decide staleness below and
    // to hand back to the caller for persisting after a successful build.
    // Reuse Pass 1's hash when we already have one (see `known_hashes`'s doc)
    // rather than reading and hashing the file's content again.
    let mut source_hashes: HashMap<String, String> = HashMap::new();
    for (source_file, _module_name, _core_file) in file_module_pairs {
        if let Some(hash) = known_hashes
            .get(source_file.as_str())
            .cloned()
            .or_else(|| content_hash_of(source_file))
        {
            source_hashes.insert(source_file.as_str().to_string(), hash);
        }
    }

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
            source_hashes,
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

        // Compare content hashes: no hash, or a hash mismatch, needs recompilation.
        match source_hashes.get(source_file.as_str()) {
            Some(current_hash)
                if previous_hashes.get(source_file.as_str()) == Some(current_hash) =>
            {
                debug!(file = %source_file, "Up-to-date (content hash unchanged)");
                unchanged_files.push(source_file.clone());
            }
            Some(_) => {
                debug!(file = %source_file, "Content hash changed — needs recompilation");
                changed_files.push(source_file.clone());
            }
            None => {
                // If we can't read the source, err on the side of recompilation.
                debug!(file = %source_file, "Cannot read source — needs recompilation");
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
        source_hashes,
    }
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
    /// BT-2014: Diagnostic summary aggregated from all compiled files.
    diagnostic_summary: beamtalk_core::source_analysis::DiagnosticSummary,
}

/// Build beamtalk source files.
///
/// This command compiles .bt files to .beam bytecode via Core Erlang.
///
/// When `force` is false, per-file change detection compares each `.bt`
/// source's content hash against the hash recorded for its `.beam` output
/// (BT-3120) and only recompiles changed files.
#[instrument(skip_all, fields(path = %path))]
pub fn build(path: &str, options: &beamtalk_core::CompilerOptions, force: bool) -> Result<()> {
    info!("Starting build");

    let env = setup_build_environment(path)?;

    // BT-2796: A package directory build walks every project source file
    // (Pass 1) before any per-file analysis runs (Pass 2), so the injected
    // cross-file knowledge is project-complete. Declare that to the
    // receiver-knowledge classifier (ADR 0100 Rule 2 sequencing guard).
    // A single-file build (`beamtalk build foo.bt`) sees only that file, and
    // a manifest-less directory build skips Pass 1 entirely — both keep the
    // conservative `ModuleOnly` default.
    let mut options = options.clone();
    if Utf8Path::new(path).is_dir() && env.full_manifest.is_some() {
        options.knowledge_scope = beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete;
    }
    // BT-2794 (pre-WS3 guard): dependency extension contributions are not
    // loaded until WS3 (ADR 0070 amendment), so declaring dependencies means
    // no receiver's method surface is provably complete.
    options.has_package_dependencies = env
        .full_manifest
        .as_ref()
        .is_some_and(|m| !m.dependencies.is_empty());
    // BT-2920: Set the current package so E0401/E0402 visibility checks
    // (`check_class_visibility`/`check_alias_leaked_visibility`) actually run
    // — they're gated on `current_package: Some(_)` and silently emit zero
    // diagnostics otherwise. `None` for single-file/manifest-less builds,
    // which have no package boundary to enforce.
    options.current_package =
        package_identity(env.pkg_manifest(), options.stdlib_mode).map(str::to_owned);
    let options = &options;

    let dep_ctx = resolve_and_validate_dependencies(&env, options)?;
    let passes = execute_build_passes(&env, options, &dep_ctx, force)?;

    // BT-2014: Print diagnostic summary at end of successful build.
    // Suppressed when --no-warnings is set (suppress_warnings), matching
    // the behaviour of individual warning suppression during compilation.
    if !options.suppress_warnings && !passes.diagnostic_summary.is_empty() {
        eprintln!();
        eprintln!("{}", passes.diagnostic_summary);
    }

    post_process_package_artifacts(&env, &dep_ctx, &passes)?;

    Ok(())
}

/// The name of the package this build is compiling, or `None` when the build
/// has no package boundary at all.
///
/// Normally that's just the manifest's package name — a single-file or
/// manifest-less directory build compiles a loose pile of sources with nothing
/// to enforce a boundary against.
///
/// BT-2965: `--stdlib-mode` is the exception. The stdlib carries no
/// `beamtalk.toml`, but every file it compiles belongs to one package, so any
/// manifest-less `--stdlib-mode` build — in practice `beamtalk build
/// --stdlib-mode <dir>`, what `just dialyzer-specs` runs over a flat copy of
/// `stdlib/src/*.bt` in a bare temp dir — reports the same identity
/// `build_stdlib::stdlib_compiler_options` (BT-2964) and the LSP's
/// [`STDLIB_PACKAGE_MARKER`](beamtalk_language_service::STDLIB_PACKAGE_MARKER)
/// use. (`test_internal_alias_resolves_cross_file_within_stdlib` pins
/// `build_stdlib`'s hardcoded literal to that constant so the two stdlib
/// compile paths cannot drift apart.)
///
/// Both callers below depend on the manifest and stdlib answers being the same
/// one: `build` stamps it onto `CompilerOptions::current_package` (the boundary
/// `AliasRegistry::add_pre_loaded` enforces) and `build_class_index` stamps it
/// onto each collected `AliasInfo::package` (the side that boundary is checked
/// against). A mismatch would silently drop every `internal` stdlib alias
/// instead of seeding it.
///
/// A manifest always wins: `--stdlib-mode` is about how to compile, not about
/// renaming a package that already named itself.
fn package_identity(
    pkg_manifest: Option<&manifest::PackageManifest>,
    stdlib_mode: bool,
) -> Option<&str> {
    pkg_manifest
        .map(|pkg| pkg.name.as_str())
        .or_else(|| stdlib_mode.then_some(beamtalk_language_service::STDLIB_PACKAGE_MARKER))
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
    /// BT-2928: Type alias declarations (`type Name = ...`) collected from
    /// every project source file, for cross-file/package alias resolution
    /// during Pass 2. See `collect_project_alias_infos`'s doc for why this
    /// is a plain full scan rather than threaded through the incremental
    /// Pass 1 cache the way `all_class_infos` is.
    ///
    /// BT-2910: Merged with dependency-exported alias infos
    /// (`ResolvedDependency.alias_infos`), so cross-package aliases (e.g.
    /// stdlib's `JsonValue`) resolve the same way cross-package classes do.
    all_alias_infos: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    /// BT-2910: Unified collection of all `ProtocolInfo` from same-package
    /// cross-file protocol definitions and dependency-exported protocols,
    /// mirroring `all_alias_infos`/`all_class_infos`. Protocols have no
    /// `internal` modifier at the AST level, so every declared protocol
    /// project-wide (and every dependency's) is included unconditionally.
    all_protocol_infos: Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    /// Project-wide standalone extension index from Pass 1 (BT-2795).
    extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
    /// Dependency registry for cross-package collision detection.
    dep_registry: beamtalk_core::semantic_analysis::DependencyRegistry,
    /// Cached ASTs from incremental Pass 1, keyed by source file path.
    cached_asts: HashMap<Utf8PathBuf, CachedAst>,
    /// Whether the manifest changed, forcing Pass 2 recompilation.
    force_pass2: bool,
    /// Content hash of every Pass-1-scanned source file, keyed by path
    /// string (BT-3120) — empty for manifest-less builds, where Pass 1 never
    /// runs. Reused by `detect_changes` (Pass 2) so it doesn't re-hash a file
    /// whose content Pass 1 already hashed this same build.
    source_hashes: HashMap<String, String>,
}

/// Phase 5-6: Build the class index (Pass 1) and merge dependency indexes.
///
/// Computes module names for all source files, builds the class-to-module and
/// class-to-superclass indexes, merges dependency class indexes, and validates
/// stdlib reservation violations.
#[allow(clippy::too_many_lines)] // linear merge pipeline (source + N deps) — split adds indirection, not clarity
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
        extension_index,
        cached_asts,
        force_pass2,
        source_hashes,
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
            result.extension_index,
            result.cached_asts,
            result.manifest_invalidated,
            result.source_hashes,
        )
    } else {
        (
            HashMap::new(),
            HashMap::new(),
            Vec::new(),
            beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
            HashMap::new(),
            false,
            HashMap::new(),
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

    // Collect dependency ClassInfo/ProtocolInfo/AliasInfo separately, then
    // merge with source-side infos via the collect_all_* helpers (BT-1733,
    // BT-2910).
    let mut dep_class_infos = Vec::new();
    let mut dep_protocol_infos = Vec::new();
    let mut dep_alias_infos = Vec::new();
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
        dep_protocol_infos.extend(dep.protocol_infos.clone());
        dep_alias_infos.extend(dep.alias_infos.clone());
    }

    // BT-1733: Single unified collection of all ClassInfo from all sources.
    // To add a new .bt source location, add its ClassInfo slice here.
    let all_class_infos = collect_all_class_infos(&[&source_class_infos, &dep_class_infos]);

    // BT-1653 / ADR 0070 Phase 3: Eagerly check stdlib reservation violations.
    // Dependencies must not export classes with stdlib-reserved names.
    if !dep_registry.is_empty() && !options.stdlib_mode {
        check_stdlib_reservations(&dep_registry)?;
    }

    // BT-2928 / BT-2910: Same-package cross-file protocol and type-alias
    // resolution, merged with dependency-exported protocols/aliases. Only
    // runs for package builds (matching `pre_loaded_classes`'s existing
    // scope) — a manifest-less directory/single-file build has no package
    // boundary and keeps today's same-file-only resolution.
    //
    // BT-2965: `--stdlib-mode` is the exception (see `package_identity`).
    // `beamtalk build --stdlib-mode <dir>` — what `just dialyzer-specs` runs,
    // over a flat copy of `stdlib/src/*.bt` in a bare temp dir — has no
    // manifest but *is* one coherent package. Without it, `field: restart ::
    // RestartStrategy = #temporary` in `supervision_spec.bt` couldn't see
    // `type RestartStrategy = ...` declared over in `actor.bt`, and
    // `check_state_defaults` reported a false "declared as RestartStrategy,
    // default is #temporary" mismatch against the unexpanded alias name.
    // `just build-stdlib` never hit this: `build_stdlib.rs` seeds its own
    // `pre_loaded_aliases` from a live same-run pre-pass (BT-2935). There are
    // no dependencies to merge in stdlib mode, so the dep-side vectors are
    // empty and the merge helpers are pass-throughs.
    //
    // Protocols and aliases are extracted together in one uncached scan
    // (`collect_project_protocol_and_alias_infos`) rather than two separate
    // ones — a build review finding: scanning the project source set twice
    // here, on top of `build_class_module_index`'s own (incrementally
    // cached) scan for classes, was doing up to three full parses of every
    // file per build.
    let (all_protocol_infos, all_alias_infos) =
        match package_identity(pkg_manifest, options.stdlib_mode) {
            Some(name) => {
                let (source_protocol_infos, source_alias_infos) =
                    collect_project_protocol_and_alias_infos(&env.source_files, name);
                (
                    collect_all_protocol_infos(&[&source_protocol_infos, &dep_protocol_infos]),
                    collect_all_alias_infos(&[&source_alias_infos, &dep_alias_infos]),
                )
            }
            None => (Vec::new(), Vec::new()),
        };

    Ok(ClassIndexResult {
        class_module_index,
        class_superclass_index,
        all_class_infos,
        all_alias_infos,
        all_protocol_infos,
        extension_index,
        dep_registry,
        cached_asts,
        force_pass2,
        source_hashes,
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
            format!("bt@{}", beamtalk_codegen::core_erlang::to_module_name(stem))
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
#[allow(clippy::too_many_lines)] // orchestration function — one call per build phase
fn execute_build_passes(
    env: &BuildEnvironment,
    options: &beamtalk_core::CompilerOptions,
    dep_ctx: &DependencyContext,
    force: bool,
) -> Result<BuildPassesResult> {
    // ADR 0098 Phase 1: gate the project build on provenance before Pass 1, so a
    // toolchain-version mismatch forces a full rebuild regardless of mtime.
    let force = force || provenance_requires_rebuild(env);

    let index = build_class_index(env, dep_ctx, options, force)?;
    let force = if index.force_pass2 { true } else { force };

    let native_result = compile_native_sources(env, dep_ctx, &index.class_module_index)?;

    // ADR 0075: Extract type specs from OTP and dependency .beam files after
    // native compilation so freshly compiled native modules are included.
    // Results are cached in _build/type_cache/ — incremental builds read zero
    // .beam files when the cache is fresh.
    let auto_extract_registry = extract_type_specs(
        &env.layout,
        env.pkg_manifest().is_some(),
        options.stdlib_mode,
    );
    // ADR 0075 full resolution chain (BT-1847, BT-3394): each stub tier
    // overrides auto-extract at the function/arity level, applied
    // lowest-precedence first so project-local always wins —
    // extracted -> distribution -> package-bundled -> project-local. Every
    // tier is checked for drift against the pre-merge auto-extract registry
    // so the drift check has real ground truth.
    let dependency_stubs = load_dependency_stub_registries(
        &dep_ctx.resolved_deps,
        distribution_stubs_dir().as_deref(),
        auto_extract_registry.as_ref(),
        OutputFormat::Text,
    );
    let project_stubs = load_project_stub_registry(
        &env.project_root,
        auto_extract_registry.as_ref(),
        OutputFormat::Text,
    );

    let (native_type_registry, stub_diagnostics) =
        if dependency_stubs.is_none() && project_stubs.is_none() {
            (auto_extract_registry, Vec::new())
        } else {
            let mut merged = auto_extract_registry.unwrap_or_default();
            let mut diagnostics = Vec::new();
            if let Some((dep_registry, dep_diags)) = dependency_stubs {
                merged.apply_overrides(dep_registry);
                diagnostics.extend(dep_diags);
            }
            if let Some((stub_registry, diags)) = project_stubs {
                merged.apply_overrides(stub_registry);
                diagnostics.extend(diags);
            }
            (Some(merged), diagnostics)
        };
    let native_type_registry = native_type_registry.map(std::sync::Arc::new);

    let file_module_pairs = compute_file_module_pairs(env)?;

    // BT-1682: Per-file change detection — only recompile files whose source
    // is newer than the corresponding .beam output. Pass 1's already-computed
    // content hashes (`index.source_hashes`, BT-3120) let this skip re-hashing
    // any file Pass 1 already hashed this build.
    let changes = detect_changes(
        &env.source_files,
        &env.build_dir,
        &file_module_pairs,
        force,
        &index.source_hashes,
    );

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
    // ADR 0100 Rule 3 (BT-2793): the package's `[diagnostics]` severity-override
    // table, empty when there's no manifest or no `[diagnostics]` section.
    let diagnostics_overrides = env
        .full_manifest
        .as_ref()
        .map(|m| m.diagnostics.clone())
        .unwrap_or_default();

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
            pre_loaded_protocols: index.all_protocol_infos.clone(),
            pre_loaded_aliases: index.all_alias_infos.clone(),
            extension_index: index.extension_index.clone(),
        },
        dep_registry: registry_ref,
        strict_deps,
        native_type_registry,
        diagnostics_overrides,
    };
    // BT-2014: Collect diagnostics from all compiled files for the summary.
    // BT-1847: seed with stubs/ diagnostics (skipped signatures, version drift).
    let mut all_build_diags: Vec<beamtalk_core::source_analysis::Diagnostic> = stub_diagnostics;

    // BT-3410: an unchanged file's diagnostics from its last actual compile,
    // so they can be replayed (shown and counted) instead of silently
    // vanishing the moment the file stops being recompiled. Rebuilt from
    // scratch below (rather than mutated in place) so a file removed from
    // the project, or one whose diagnostics changed, never leaves a stale
    // entry in the sidecar this build writes back out.
    let previous_diagnostics_cache = super::build_cache::load_diagnostics_cache(&env.build_dir);
    let mut new_diagnostics_cache: HashMap<
        String,
        Vec<beamtalk_core::source_analysis::Diagnostic>,
    > = HashMap::with_capacity(file_module_pairs.len());
    // BT-3410: every changed file (always known) plus every unchanged file
    // whose diagnostics were actually found in the cache and replayed, or
    // recompiled below because they weren't.
    let mut files_with_known_diagnostics = 0usize;
    // BT-3410: unchanged files recompiled this build solely because the
    // diagnostics sidecar had no entry for them (a fresh sidecar right after
    // upgrading to this feature, corruption, or a cache-version bump) — their
    // .beam is already current, only their diagnostics were unknown. Counted
    // separately so the "Compiling N of M files" summary below still
    // reflects what `core_files` actually holds.
    let mut diagnostics_cache_miss_recompiles = 0usize;

    for (file, module_name, core_file) in &file_module_pairs {
        module_names.push(module_name.clone());

        if changed_set.contains(file.as_path()) {
            let cached = cached_asts.remove(file);
            let file_diags =
                compile_file(file, module_name, core_file, options, &compile_ctx, cached)?;
            files_with_known_diagnostics += 1;
            new_diagnostics_cache.insert(file.as_str().to_string(), file_diags.clone());
            all_build_diags.extend(file_diags);
            core_files.push(core_file.clone());
            continue;
        }

        if let Some(cached_diags) = previous_diagnostics_cache.get(file.as_str()) {
            debug!(file = %file, "Skipping unchanged file (diagnostics replayed from cache)");
            if !cached_diags.is_empty() {
                // A cached diagnostic's span is only valid against the exact
                // content that produced it — `changed_set` already
                // guarantees this file's content hash hasn't moved since
                // then, so re-reading it now is safe to render against.
                if let Ok(source) = fs::read_to_string(file) {
                    crate::diagnostic::print_diagnostics_text(
                        cached_diags,
                        file.as_str(),
                        &source,
                        options,
                    );
                }
            }
            files_with_known_diagnostics += 1;
            all_build_diags.extend(cached_diags.iter().cloned());
            new_diagnostics_cache.insert(file.as_str().to_string(), cached_diags.clone());
            continue;
        }

        // BT-3410: unchanged, but no diagnostics cache entry — recompile now
        // so the gap closes within this one build instead of leaving the
        // file silently unreported until its content next changes or
        // `--force` is used. `compile_file` transparently falls back to
        // reading and parsing from disk when Pass 1 didn't cache this file's
        // AST either (its own, separate incremental cache), so this is safe
        // even though the file was never in `cached_asts`.
        debug!(file = %file, "Unchanged file has no diagnostics cache entry — recompiling");
        let cached = cached_asts.remove(file);
        let file_diags = compile_file(file, module_name, core_file, options, &compile_ctx, cached)?;
        files_with_known_diagnostics += 1;
        diagnostics_cache_miss_recompiles += 1;
        new_diagnostics_cache.insert(file.as_str().to_string(), file_diags.clone());
        all_build_diags.extend(file_diags);
        core_files.push(core_file.clone());
    }

    compile_to_beam(
        &env.build_dir,
        &core_files,
        changes.changed_files.len() + diagnostics_cache_miss_recompiles,
        changes.unchanged_files.len() - diagnostics_cache_miss_recompiles,
        file_module_pairs.len(),
    )?;

    // BT-3410: persist diagnostics before the beam-hash sidecar. If the
    // process dies between these two writes, the hash cache must be the one
    // left stale — an unchanged-looking file whose diagnostics entry is
    // missing/superseded falls through to "unknown" and gets skipped safely,
    // whereas writing the hash cache first would let a crash leave it
    // already matching the new content while the diagnostics cache still
    // held the superseded file's diagnostics, which would then get replayed
    // as if it were current.
    super::build_cache::save_diagnostics_cache(&env.build_dir, &new_diagnostics_cache);
    // BT-3120: only persist the beam-hash sidecar once compilation has
    // actually succeeded (the `?` above already returned on failure) — the
    // saved hashes assert "this content produced the `.beam` now on disk".
    super::build_cache::save_beam_hash_cache(&env.build_dir, &changes.source_hashes);

    let diagnostic_summary = beamtalk_core::source_analysis::DiagnosticSummary::from_diagnostics(
        &all_build_diags,
        files_with_known_diagnostics,
    );

    Ok(BuildPassesResult {
        module_names,
        file_module_pairs,
        hierarchy: compile_ctx.hierarchy,
        native_result,
        diagnostic_summary,
    })
}

/// ADR 0098 Phase 1: project provenance gate.
///
/// Returns `true` when the project's build scope was produced by a different
/// toolchain (a `beamtalk_version` or OTP-version mismatch) or the stamp is
/// missing/corrupt/unknown-schema — meaning every module must be recompiled
/// regardless of mtime. On a miss the version-sensitive Pass 1 metadata cache is
/// discarded too (its `ClassInfo` is compiler-derived). No-op (returns `false`)
/// for single-file builds, which have no `_build/dev/` scope or stamp.
fn provenance_requires_rebuild(env: &BuildEnvironment) -> bool {
    if env.pkg_manifest().is_none() {
        return false;
    }
    let current_otp = super::build_stamp::current_otp_version();
    match super::build_stamp::read_stamp_status(&env.layout.stamp_path(), current_otp) {
        super::build_stamp::StampStatus::Fresh => false,
        super::build_stamp::StampStatus::Stale(reason) => {
            info!("Build provenance miss ({reason}) — forcing full rebuild");
            super::build_cache::discard_pass1_cache(&env.build_dir);
            true
        }
    }
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

    // ADR 0098 Phase 1: write the provenance stamp LAST, after every artifact in
    // the scope has landed, so a crash mid-build never leaves a stamp claiming
    // freshness for incomplete output. Best-effort (a write failure just means
    // the next build sees no stamp and rebuilds).
    super::build_stamp::write_stamp(
        &env.layout.stamp_path(),
        super::build_stamp::current_otp_version(),
    );

    Ok(())
}

/// ADR 0075 Phase 1: Extract type specs from OTP and dependency `.beam` files
/// and cache them.
///
/// Non-fatal: if spec extraction fails (e.g., runtime not compiled), the build
/// succeeds without type information. The LSP will still provide untyped
/// completions in that case.
///
/// Runs in package mode (`has_manifest`) and in `--stdlib-mode` (used by
/// `dialyzer-specs` and the build-stdlib pipeline, which compile a directory
/// of stdlib `.bt` files without a manifest). Single-file builds without
/// either signal return `None`.
///
/// BT-2851: This is the single source of truth for populating a
/// [`NativeTypeRegistry`] from OTP/dependency `.beam` files. `beamtalk build`
/// (via [`execute_build_passes`]) and `beamtalk lint` (via
/// [`super::lint::run_lint`]) both call this function directly — rather than
/// lint reading a possibly-absent/stale on-disk cache written by a *previous*
/// build — so the two surfaces can never see different FFI type diagnostics
/// for the same code. The tiered cache in `cache_dir` still makes repeat
/// calls (from either surface) cheap: a fresh cache short-circuits to zero
/// `.beam` reads; a cold/stale one pays the extraction cost once and writes
/// the cache for the next caller, whichever surface that is.
pub(crate) fn extract_type_specs(
    layout: &BuildLayout,
    has_manifest: bool,
    stdlib_mode: bool,
) -> Option<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry> {
    // No manifest: only continue when compiling the stdlib (which has its own
    // FFI surface in runtime/stdlib/workspace ebins). Otherwise nothing to do.
    if !has_manifest {
        return if stdlib_mode {
            super::build_stdlib::extract_stdlib_type_specs()
        } else {
            None
        };
    }

    // BT-2858: the manifest-backed extraction path now lives in the
    // `beamtalk_cli` lib crate (`native_type_specs`) so `beamtalk-mcp` can
    // call the same single source of truth without reading a possibly-stale
    // on-disk cache. This is a thin delegation, not a duplicate.
    beamtalk_cli::native_type_specs::extract_project_type_specs(layout)
}

/// Locate the compiler distribution's own curated FFI type stubs (ADR 0075
/// layer 3), installed alongside the stdlib sources under
/// `{sysroot}/share/beamtalk/stubs/` — the same sysroot convention used by
/// `beamtalk --print-sysroot` and the LSP's `sysroot_stdlib_source_dir`,
/// both backed by the shared [`beamtalk_sysroot`] leaf crate.
/// `BEAMTALK_STUBS_DIR` overrides the sysroot-derived path for development.
///
/// Unlike `BEAMTALK_RUNTIME_DIR` (which hard-errors when set to an invalid
/// path), an unset/missing/non-directory override here just logs a warning
/// and falls through to `None` — this stub tier is optional and best-effort,
/// so a build should never fail over a distribution-stub misconfiguration.
///
/// Returns `None` if no such directory exists on disk — expected until
/// curated distribution stub content is added (BT-1848); the discovery/merge
/// plumbing is still correct with nothing to find.
pub(crate) fn distribution_stubs_dir() -> Option<Utf8PathBuf> {
    if let Ok(dir) = std::env::var("BEAMTALK_STUBS_DIR") {
        let candidate = Utf8PathBuf::from(dir);
        if candidate.is_dir() {
            return Some(candidate);
        }
        warn!(
            dir = %candidate,
            "BEAMTALK_STUBS_DIR is set but is not a directory; ignoring distribution stubs"
        );
        return None;
    }

    let sysroot = beamtalk_sysroot::current_sysroot()?;
    let candidate = Utf8PathBuf::from_path_buf(sysroot.join("share/beamtalk/stubs")).ok()?;
    candidate.is_dir().then_some(candidate)
}

/// ADR 0075 Phase 2 (BT-1847): scan `project_root/stubs/` for `declare
/// native:` stub files and build the project-local stub-tier registry, plus
/// its diagnostics (skipped-signature warnings and version-drift warnings
/// against `auto_extract`).
///
/// `stubs/` is auto-discovered — no `beamtalk.toml` config needed, matching
/// ADR 0075's project-local-stubs design. Returns `None` when there is no
/// `stubs/` directory or it contains no `.bt` files (the common case — most
/// projects have no stubs at all), so callers can tell "nothing to merge"
/// apart from "stubs/ exists but is empty of declarations".
///
/// Pass `auto_extract` as the *pre-merge* auto-extracted registry — the
/// drift check (`NativeTypeRegistry::detect_stub_drift`) needs it as ground
/// truth for what a `.beam` module actually exports, which only holds before
/// stub overrides are applied on top of it.
///
/// Shared by `beamtalk build` and `beamtalk lint`, mirroring
/// [`extract_type_specs`]'s "single source of truth" role, so both surfaces
/// agree on stub-derived types by construction.
///
/// `format` controls how each stub diagnostic (skipped signature, version
/// drift) is rendered as it's discovered — `beamtalk build` always passes
/// [`OutputFormat::Text`] (it has no JSON mode); `beamtalk lint` passes its
/// own `--format`, so a stub diagnostic streams as a JSON line under
/// `lint --format=json` the same way every other lint diagnostic does,
/// instead of always printing miette text regardless of the caller's
/// requested format.
pub(crate) fn load_project_stub_registry(
    project_root: &Utf8Path,
    auto_extract: Option<&beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    format: OutputFormat,
) -> Option<(
    beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
)> {
    load_stub_registry_from_dir(&project_root.join("stubs"), auto_extract, format)
}

/// Load and merge the package-bundled (ADR 0075 layer 2) and distribution
/// (layer 3) stub tiers, ranked in that precedence order: distribution
/// applied first, then each dependency's own bundled stubs on top —
/// matching the resolution chain's "project > package > distribution >
/// auto-extracted" order (project-local stubs are merged separately, last,
/// by [`load_project_stub_registry`]'s caller).
///
/// A dependency's `stubs_dir` (set by dependency resolution from its own
/// `beamtalk.toml` `[stubs] path`, ADR 0075 layer 2) covers only that
/// package's own native code — never its own dependencies' stubs, since
/// those are auto-extracted instead. Because each package is expected to
/// stub only its own native modules, two sibling dependencies declaring the
/// same `(module, function, arity)` is unexpected — `warn_on_package_stub_collisions`
/// flags it (naming both packages) rather than letting the later dependency
/// in iteration order silently win, mirroring how `check_native_module_collisions`
/// surfaces a native-module-implementation collision across dependencies.
///
/// `distribution_stubs_dir` is the compiler distribution's own `stubs/`
/// directory (layer 3), when one is discoverable — `None` until curated
/// distribution stub content exists (BT-1848). Distribution-vs-package
/// overlap is not flagged: package stubs are *meant* to override
/// distribution stubs, per the resolution order.
///
/// Returns `None` only when none of these layers contributed anything.
pub(crate) fn load_dependency_stub_registries(
    resolved_deps: &[super::deps::path::ResolvedDependency],
    distribution_stubs_dir: Option<&Utf8Path>,
    auto_extract: Option<&beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    format: OutputFormat,
) -> Option<(
    beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
)> {
    let mut registry = beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();
    let mut diagnostics = Vec::new();
    let mut found_any = false;

    if let Some(dist_dir) = distribution_stubs_dir {
        if let Some((dist_registry, dist_diagnostics)) =
            load_stub_registry_from_dir(dist_dir, auto_extract, format)
        {
            registry.apply_overrides(dist_registry);
            diagnostics.extend(dist_diagnostics);
            found_any = true;
        }
    }

    // Tracks which dependency first declared each (module, function, arity)
    // package-bundled stub, so a sibling dependency re-declaring the same
    // signature is flagged rather than silently overriding it.
    let mut package_stub_owners: HashMap<(String, String, u8), String> = HashMap::new();

    for dep in resolved_deps {
        let Some(stubs_dir) = dep.stubs_dir.as_deref() else {
            continue;
        };
        if let Some((dep_registry, dep_diagnostics)) =
            load_stub_registry_from_dir(stubs_dir, auto_extract, format)
        {
            diagnostics.extend(warn_on_package_stub_collisions(
                &dep_registry,
                &dep.name,
                &mut package_stub_owners,
                format,
            ));
            registry.apply_overrides(dep_registry);
            diagnostics.extend(dep_diagnostics);
            found_any = true;
        }
    }

    found_any.then_some((registry, diagnostics))
}

/// Records every `(module, function, arity)` in `dep_registry` as owned by
/// `dep_name` in `owners`, returning a warning diagnostic for each one
/// already owned by a *different* package — a same-module stub collision
/// between two sibling dependencies' own package-bundled stubs (see
/// [`load_dependency_stub_registries`]).
///
/// Each collision is rendered immediately (respecting `format`), mirroring
/// [`load_stub_registry_from_dir`]'s own drift/skipped-signature
/// diagnostics — without it, a collision would only ever show up as an
/// anonymous count in the build summary (`beamtalk build`) or be silently
/// dropped (`beamtalk lint`, which discards the diagnostics its caller
/// doesn't fold into `all_diags`). There is no single stub file's source to
/// render a code snippet against here (the two declarations live in two
/// different packages' stub files), so this prints a plain message rather
/// than a source-annotated miette report.
fn warn_on_package_stub_collisions(
    dep_registry: &beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
    dep_name: &str,
    owners: &mut HashMap<(String, String, u8), String>,
    format: OutputFormat,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let mut collisions = Vec::new();

    for module in dep_registry.module_names() {
        let Some(functions) = dep_registry.module_functions(module) else {
            continue;
        };
        for func in functions {
            let key = (module.to_string(), func.name.clone(), func.arity);
            match owners.insert(key, dep_name.to_string()) {
                Some(existing_owner) if existing_owner != dep_name => {
                    let span = match func.provenance {
                        beamtalk_core::semantic_analysis::type_checker::TypeProvenance::Declared(
                            s,
                        ) => s,
                        _ => beamtalk_core::source_analysis::Span::default(),
                    };
                    let message = format!(
                        "Package-bundled stub collision: both '{existing_owner}' and \
                         '{dep_name}' declare `{module}:{}/{}` — the later dependency's \
                         stub silently wins. Package stubs should cover only that \
                         package's own native code (ADR 0075); rename or remove the \
                         duplicate declaration.",
                        func.name, func.arity
                    );
                    warn!(
                        module,
                        function = %func.name,
                        arity = func.arity,
                        first_owner = %existing_owner,
                        second_owner = %dep_name,
                        "Package-bundled stub collision between sibling dependencies"
                    );
                    let diagnostic =
                        beamtalk_core::source_analysis::Diagnostic::warning(message, span);
                    match format {
                        OutputFormat::Text => eprintln!("warning: {}", diagnostic.message),
                        OutputFormat::Json => {
                            println!(
                                "{}",
                                crate::diagnostic::diagnostic_to_json(dep_name, &diagnostic)
                            );
                        }
                    }
                    collisions.push(diagnostic);
                }
                _ => {}
            }
        }
    }

    collisions
}

/// Parse and merge every `declare native:` stub file directly inside `stubs_dir`
/// into a single [`NativeTypeRegistry`], checking each against `auto_extract`
/// for version drift. Shared by every stub-resolution-chain layer
/// (project-local, package-bundled, distribution) — each layer is just a
/// different directory.
///
/// Returns `None` if `stubs_dir` does not exist or contains no `.bt` files.
fn load_stub_registry_from_dir(
    stubs_dir: &Utf8Path,
    auto_extract: Option<&beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    format: OutputFormat,
) -> Option<(
    beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
)> {
    if !stubs_dir.exists() {
        return None;
    }
    let stub_files = match collect_source_files_from_dir(stubs_dir) {
        Ok(files) => files,
        Err(e) => {
            warn!(dir = %stubs_dir, error = %e, "Cannot read stubs/ directory; skipping stub resolution");
            return None;
        }
    };
    if stub_files.is_empty() {
        return None;
    }

    let mut registry = beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();
    let mut all_diagnostics = Vec::new();

    for file in &stub_files {
        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                warn!(file = %file, error = %e, "Cannot read stub file; skipping");
                continue;
            }
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, mut file_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

        // BT-1847: this file's own declarations, checked for drift against
        // `auto_extract` *before* merging into the accumulating `registry` —
        // scoping the check to one file's functions lets each warning render
        // against that file's own source (below), rather than losing track
        // of which stub file a merged function came from.
        let mut file_registry =
            beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();
        file_diagnostics.extend(
            beamtalk_core::semantic_analysis::type_checker::load_native_declarations(
                &module.native_declarations,
                &mut file_registry,
            ),
        );
        if let Some(auto_extract) = auto_extract {
            for (module_name, sig) in auto_extract.detect_stub_drift(&file_registry) {
                let span = match sig.provenance {
                    beamtalk_core::semantic_analysis::type_checker::TypeProvenance::Declared(s) => {
                        s
                    }
                    _ => beamtalk_core::source_analysis::Span::default(),
                };
                file_diagnostics.push(
                    beamtalk_core::source_analysis::Diagnostic::warning(
                        format!(
                            "Stub declares `{module_name}:{}/{}`, which was not found in the \
                             compiled `{module_name}.beam` exports — the stub may be out of date",
                            sig.name, sig.arity
                        ),
                        span,
                    )
                    .with_hint(
                        "Regenerate this stub with `beamtalk generate stubs`, or remove the \
                         stale declaration.",
                    ),
                );
            }
        }
        // Per-function upsert (not `merge`'s whole-module keep-on-collision):
        // two stub files declaring the same Erlang module must combine, with
        // the later file's function/arity winning on overlap.
        registry.apply_overrides(file_registry);

        // Render immediately against this file's own source, mirroring
        // `compile_source_with_bindings`'s rendering for src/ files —
        // without it, a drift or skipped-signature warning would only ever
        // show up as an anonymous count in the build summary. Respects
        // `format` so `lint --format=json` gets these as JSON lines too
        // (see this function's doc).
        for diagnostic in &file_diagnostics {
            if diagnostic.severity == beamtalk_core::source_analysis::Severity::Lint {
                continue;
            }
            match format {
                OutputFormat::Text => {
                    let compile_diag = crate::diagnostic::CompileDiagnostic::from_core_diagnostic(
                        diagnostic,
                        file.as_str(),
                        &source,
                    );
                    eprintln!("{:?}", miette::Report::new(compile_diag));
                }
                OutputFormat::Json => {
                    let json = crate::diagnostic::diagnostic_to_json(file.as_str(), diagnostic);
                    println!("{json}");
                }
            }
        }

        all_diagnostics.extend(file_diagnostics);
    }

    Some((registry, all_diagnostics))
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
    let alias_metadata = build_alias_metadata(outputs.source_files);

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
        &alias_metadata,
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

    collect_project_source_files(path)
}

/// Finds a project's `.bt` source files: `src/` if it exists, else
/// `project_root` itself — excluding `project_root/stubs/`, which
/// `load_project_stub_registry` (ADR 0075, BT-1847) scans separately and
/// never compiles as ordinary source (a `declare native:` block there is a
/// hard error everywhere else).
///
/// Without the exclusion, a project with no `src/` directory would fall back
/// to searching the whole project root, sweeping `stubs/*.bt` into the
/// normal compile/lint pipeline. Shared by `find_source_files` (this file),
/// `type_coverage.rs`, and `deps/path.rs`'s path-dependency compilation — all
/// three previously duplicated the same `src/`-or-fallback logic without
/// this exclusion.
pub(crate) fn collect_project_source_files(project_root: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let src_dir = project_root.join("src");
    let search_dir = if src_dir.exists() {
        src_dir
    } else {
        project_root.to_path_buf()
    };

    let files = collect_source_files_from_dir(&search_dir)?;
    let stubs_dir = project_root.join("stubs");
    Ok(files
        .into_iter()
        .filter(|f| !f.starts_with(&stubs_dir))
        .collect())
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
///
/// BT-3435 (ADR 0119 step 0): the segment-validation and per-segment
/// `to_module_name` conversion delegate to
/// `beamtalk_core::semantic_analysis::relative_module_segments` — the one
/// implementation of "how a file path becomes module-name segments" shared
/// with the new `ClassModuleRegistry`'s Pass-1 construction (CLAUDE.md's "No
/// duplicate implementations" rule) — rather than re-implementing the loop
/// here.
pub(crate) fn compute_relative_module(
    file: &Utf8Path,
    source_root: Option<&Utf8Path>,
) -> Result<String> {
    if let Some(root) = source_root {
        if let Ok(relative) = file.strip_prefix(root) {
            let segments = beamtalk_core::semantic_analysis::relative_module_segments(relative)
                .into_diagnostic()?;
            return Ok(segments.join("@"));
        }
    }
    // Fallback: use file stem
    let stem = file.file_stem().unwrap_or("unknown");
    Ok(beamtalk_codegen::core_erlang::to_module_name(stem))
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
) -> Result<Vec<beamtalk_core::source_analysis::Diagnostic>> {
    debug!("Compiling {path}");

    let diags = compile_source_with_bindings(
        path,
        module_name,
        core_file,
        options,
        &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
        ctx,
        cached_ast,
    )?;

    debug!("Generated Core Erlang: {core_file}");

    Ok(diags)
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
    beamtalk_core::compilation::extension_index::ExtensionIndex,
    HashMap<Utf8PathBuf, CachedAst>,
)> {
    let mut module_index = HashMap::new();
    let mut superclass_index = HashMap::new();
    let mut all_class_infos = Vec::new();
    let mut extension_index = beamtalk_core::compilation::extension_index::ExtensionIndex::new();
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
        //
        // BT-2796: A file with parse *errors* may have an under-recovered
        // method surface (error recovery can drop method definitions), so its
        // classes are marked `surface_incomplete`. The receiver-knowledge
        // classifier downgrades receivers whose superclass chain contains a
        // marked class to `Open`, preventing false unresolved-selector hints
        // against a surface Pass 1 never fully saw.
        let has_parse_errors = diagnostics
            .iter()
            .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
        let mut class_infos =
            beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module);
        if has_parse_errors {
            for info in &mut class_infos {
                info.surface_incomplete = true;
            }
        }
        // BT-2920: Stamp the package now, while each file's classes are still
        // isolated — `analyse_full`'s `stamp_package` only reaches classes
        // built from the *current* module's own AST, never the cross-file
        // `ClassInfo` this Pass 1 index injects into every other file's
        // compilation. Without this, E0401/E0402 treat a same-package class
        // from a sibling file as package-less and never flag it as a leak.
        beamtalk_core::semantic_analysis::ClassHierarchy::stamp_package_on_infos(
            &mut class_infos,
            pkg_name,
        );
        all_class_infos.extend(class_infos);

        // BT-2795: Collect standalone extension definitions
        // (`ClassName >> selector => ...`) project-wide so Pass 2 can
        // register them into every file's class hierarchy — a same-project
        // cross-file extension then resolves instead of producing a false
        // `Dnu` hint (ADR 0066 / ADR 0100 Rule 2 WS1).
        extension_index.add_module(&module, file.as_std_path());

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

    Ok((
        module_index,
        superclass_index,
        all_class_infos,
        extension_index,
        cached_asts,
    ))
}

/// Extracts type-alias declarations (`type Name = ...`) from every project
/// source file, for cross-file/package alias resolution during Pass 2
/// (BT-2928).
///
/// Unlike `ClassInfo`, alias metadata is *not* threaded through the
/// incremental Pass 1 cache (`build_cache.rs`'s `.beamtalk-pass1-cache.json`):
/// `AliasInfo` embeds an unresolved `TypeAnnotation`, which (unlike
/// `ClassInfo`'s stringly-typed method signatures) has no `serde` support —
/// adding it would mean threading `Serialize`/`Deserialize` through the AST's
/// `TypeAnnotation` tree purely to satisfy the cache format, a disproportionate
/// blast radius for this fix (tracked as a BT-2928 follow-up for anyone who
/// wants the incremental-cache perf win later). Lexing and parsing every
/// project source file just to pull out `type` declarations is cheap relative
/// to the rest of a build, so this always does a full, uncached scan
/// regardless of per-file change detection — every call re-derives the
/// project's alias table from scratch rather than risking staleness.
///
/// Parse errors on an individual file are non-fatal here: that file simply
/// contributes no aliases to the merged set, and the same parse error is
/// already reported through the normal Pass 1 diagnostics path.
pub(crate) fn collect_project_alias_infos(
    source_files: &[Utf8PathBuf],
    pkg_name: &str,
) -> Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo> {
    collect_project_protocol_and_alias_infos(source_files, pkg_name).1
}

/// Merge alias infos from multiple sources (same-package cross-file +
/// dependency-exported) into one collection, mirroring `collect_all_class_infos`
/// (BT-2910). A plain concatenation — collision diagnostics are handled by
/// `AliasRegistry::add_pre_loaded`, not here.
pub(crate) fn collect_all_alias_infos(
    sources: &[&[beamtalk_core::semantic_analysis::alias_registry::AliasInfo]],
) -> Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo> {
    let total_len: usize = sources.iter().map(|s| s.len()).sum();
    let mut result = Vec::with_capacity(total_len);
    for source in sources {
        result.extend_from_slice(source);
    }
    result
}

/// Extracts protocol declarations (`Protocol define: Name ...`) from every
/// project source file, for same-package cross-file protocol resolution
/// during Pass 2 (BT-2910).
///
/// Mirrors `collect_project_alias_infos`: a plain, uncached full scan rather
/// than threaded through the incremental Pass 1 cache. Unlike `AliasInfo`,
/// `ProtocolInfo` carries no `package` field to stamp — protocols have no
/// `internal` modifier at the AST level, so every declared protocol is
/// exported project-wide.
///
/// Parse errors on an individual file are non-fatal here: that file simply
/// contributes no protocols to the merged set, and the same parse error is
/// already reported through the normal Pass 1 diagnostics path.
///
/// `build_class_index`'s production path calls
/// `collect_project_protocol_and_alias_infos` directly to extract both kinds
/// in one scan; this standalone single-purpose wrapper is kept for tests
/// that only care about protocols (mirrors `collect_project_alias_infos`,
/// which stays in production use via `compile_dependency_with_context`).
#[allow(dead_code)] // Used by tests; production path uses the combined extractor above
pub(crate) fn collect_project_protocol_infos(
    source_files: &[Utf8PathBuf],
) -> Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    collect_project_protocol_and_alias_infos(source_files, "").0
}

/// Extracts both protocol and type-alias declarations from every project
/// source file in a single lex/parse pass per file (BT-2910 review finding).
///
/// `collect_project_protocol_infos`/`collect_project_alias_infos` used to
/// each independently re-parse the full project source set, meaning a
/// manifest-based build did up to three full scans of every source file:
/// once (incrementally cached) in `build_class_module_index` for classes,
/// then two more *uncached* scans here for protocols and aliases. This
/// combines those last two into one uncached scan, mirroring how
/// `build_dep_class_index` already extracts both kinds from a single pass
/// over its cached ASTs. The class-index scan stays separate — it's
/// incrementally cached and produces different data
/// (`class_module_index`/`class_superclass_index`), not a natural fit for
/// this merge.
///
/// `pkg_name` is only used to stamp `AliasInfo.package`; pass `""` when only
/// the protocol half of the result is needed (see
/// `collect_project_protocol_infos`, which never stamps a package anyway).
fn collect_project_protocol_and_alias_infos(
    source_files: &[Utf8PathBuf],
    pkg_name: &str,
) -> (
    Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
) {
    let mut all_protocols = Vec::new();
    let mut all_aliases = Vec::new();
    for file in source_files {
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

        all_protocols.extend(
            beamtalk_core::semantic_analysis::protocol_registry::ProtocolRegistry::extract_protocol_infos(
                &module,
            ),
        );

        let mut infos =
            beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::extract_alias_infos(
                &module,
            );
        for info in &mut infos {
            info.package = Some(pkg_name.into());
        }
        all_aliases.extend(infos);
    }
    (all_protocols, all_aliases)
}

/// Merge protocol infos from multiple sources (same-package cross-file +
/// dependency-exported) into one collection, mirroring `collect_all_class_infos`
/// / `collect_all_alias_infos` (BT-2910).
pub(crate) fn collect_all_protocol_infos(
    sources: &[&[beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo]],
) -> Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    let total_len: usize = sources.iter().map(|s| s.len()).sum();
    let mut result = Vec::with_capacity(total_len);
    for source in sources {
        result.extend_from_slice(source);
    }
    result
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

/// Build `.app`-file type-alias metadata for a package (ADR 0108 Phase 8,
/// BT-2903).
///
/// Independent of the incremental Pass 1 class-index cache
/// (`build_class_module_index`/`build_cache.rs`): that cache exists to skip
/// re-parsing unchanged files for `ClassInfo` extraction, but persisting
/// alias metadata across incremental runs would mean extending its on-disk
/// cache schema for a small, cheap-to-reparse surface (a package's `type`
/// declarations are typically a handful of lines total). This always
/// re-parses every file in `source_files` fresh, so `browse-type-aliases`
/// (`beamtalk_repl_ops_browse.erl`) sees a complete, correct alias list on
/// every build — including an incremental build that skipped Pass 1 for
/// files whose classes didn't change.
///
/// A file that fails to read or has parse errors contributes no aliases
/// (best-effort, matching `build_class_module_index`'s handling of unreadable
/// files) rather than failing the whole build — alias metadata is a browse-op
/// convenience, not required for compilation to succeed.
///
/// Returned in `source_files` order, **not** sorted by name —
/// [`app_file::format_type_aliases_entry`] owns the sort for deterministic
/// `.app` output, so sorting here too would be redundant.
pub(crate) fn build_alias_metadata(source_files: &[Utf8PathBuf]) -> Vec<app_file::AliasMetadata> {
    let mut result = Vec::new();
    for file in source_files {
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        for alias_def in &module.type_aliases {
            result.push(app_file::AliasMetadata {
                name: alias_def.name.name.to_string(),
                expansion: beamtalk_core::unparse::unparse_type_annotation_display(
                    &alias_def.annotation,
                ),
                doc: alias_def.doc_comment.clone(),
                source_file: to_forward_slash(file),
                internal: alias_def.is_internal,
            });
        }
    }
    result
}

/// Render a `Utf8Path` as forward-slash-separated text regardless of host OS.
///
/// `Utf8PathBuf`'s `Display`/`ToString` preserve native separators (backslash
/// on Windows), which is wrong for values embedded in generated, checked-in
/// artifacts like `beamtalk_stdlib.app.src` — those must be byte-identical
/// regardless of the developer's OS (BT-3067). `Utf8Path` guarantees valid
/// UTF-8, so a plain byte-level replace is safe here: backslash can't appear
/// as a legitimate path separator component on any of our supported
/// platforms. Same fix pattern as `make_git_index` in
/// `deps/registry.rs`'s tests.
fn to_forward_slash(path: &Utf8Path) -> String {
    path.as_str().replace('\\', "/")
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
    config.push_str("{erl_opts, [debug_info]}.\n");

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
mod tests;
