// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build environment setup (source discovery, project root, manifest, build
//! directory) and dependency resolution/validation.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use tracing::{debug, error, info};

use crate::commands::build_layout::BuildLayout;
use crate::commands::manifest;

use super::native::check_native_module_collisions;
use super::sources::find_source_files;

/// Resolved environment for a build: source files, project root, manifest, and
/// build directory. Produced by [`setup_build_environment`].
pub(crate) struct BuildEnvironment {
    /// All `.bt` source files discovered for this build.
    pub(crate) source_files: Vec<Utf8PathBuf>,
    /// The project root directory (directory input or parent of file input).
    pub(crate) project_root: Utf8PathBuf,
    /// The fully parsed manifest, if `beamtalk.toml` exists.
    pub(crate) full_manifest: Option<manifest::ParsedManifest>,
    /// The build layout helper for computing standard paths.
    pub(crate) layout: BuildLayout,
    /// The output directory for compiled artifacts (ebin/ or build/).
    pub(crate) build_dir: Utf8PathBuf,
    /// The `src/` directory if it exists, used for relative module path computation.
    pub(crate) source_root: Option<Utf8PathBuf>,
}

impl BuildEnvironment {
    /// Convenience accessor for the package manifest, if present.
    pub(crate) fn pkg_manifest(&self) -> Option<&manifest::PackageManifest> {
        self.full_manifest.as_ref().map(|m| &m.package)
    }
}

/// Resolved dependencies and their associated metadata. Produced by
/// [`resolve_and_validate_dependencies`].
pub(crate) struct DependencyContext {
    /// The resolved dependency list (empty for non-package builds).
    pub(crate) resolved_deps: Vec<crate::commands::deps::path::ResolvedDependency>,
    /// Whether native hex dependencies exist in the dependency graph.
    pub(crate) has_native_deps: bool,
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
pub(crate) fn package_identity(
    pkg_manifest: Option<&manifest::PackageManifest>,
    stdlib_mode: bool,
) -> Option<&str> {
    pkg_manifest
        .map(|pkg| pkg.name.as_str())
        .or_else(|| stdlib_mode.then_some(beamtalk_language_service::STDLIB_PACKAGE_MARKER))
}

/// Phase 1-3: Discover source files, resolve the project root and manifest,
/// and create the build output directory.
pub(crate) fn setup_build_environment(path: &str) -> Result<BuildEnvironment> {
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
pub(crate) fn resolve_and_validate_dependencies(
    env: &BuildEnvironment,
    options: &beamtalk_core::CompilerOptions,
) -> Result<DependencyContext> {
    // ADR 0070 Phase 1: Resolve and compile dependencies when needed.
    // Uses staleness detection: no-op when lockfile is fresh and deps are compiled.
    // Otherwise resolves the full transitive graph in topological order.
    let resolved_deps = if env.pkg_manifest().is_some() {
        crate::commands::deps::ensure_deps_resolved(&env.project_root, options)?
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
