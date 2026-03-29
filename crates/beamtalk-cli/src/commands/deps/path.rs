// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Path dependency resolution for the Beamtalk package system (ADR 0070 Phase 1).
//!
//! **DDD Context:** Build System
//!
//! Resolves local path dependencies declared in `beamtalk.toml`:
//! - Resolves relative paths from the depending package's directory
//! - Locates and validates the dependency's `beamtalk.toml`
//! - Compiles the dependency and places output in `_build/deps/{name}/ebin/`
//! - Detects circular dependencies
//! - Returns ebin paths for BEAM code path setup

use beamtalk_core::compilation::DependencySource;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use tracing::{debug, info};

use crate::commands::manifest::{self, ParsedManifest};

/// A resolved dependency with its compiled ebin path and class index.
#[derive(Debug)]
#[allow(dead_code)] // Fields used by integration tests and future phases
pub struct ResolvedDependency {
    /// The dependency's package name.
    pub name: String,
    /// The absolute path to the dependency's root directory.
    pub root: Utf8PathBuf,
    /// The path to the dependency's compiled ebin directory.
    pub ebin_path: Utf8PathBuf,
    /// Maps class names to compiled BEAM module names for this dependency.
    /// E.g., `"Helper"` -> `"bt@utils@helper"`.
    pub class_module_index: HashMap<String, String>,
    /// Whether this is a direct dependency of the root package (ADR 0070 Phase 3).
    /// `false` means it is a transitive dependency.
    pub is_direct: bool,
    /// For transitive dependencies, the chain of packages through which this
    /// dep is reached. E.g., `["json"]` if the root depends on `json` which
    /// depends on this package. Empty for direct dependencies.
    pub via_chain: Vec<String>,
}

/// Resolve and compile all path dependencies for a package.
///
/// Given a project root containing a `beamtalk.toml`, resolves all path
/// dependencies, compiles them, and returns the list of ebin paths to add
/// to the BEAM code path.
///
/// Git dependencies are skipped (not yet implemented — ADR 0070 Phase 2+).
///
/// # Errors
///
/// Returns an error if:
/// - A path dependency's directory does not exist
/// - A path dependency is missing `beamtalk.toml`
/// - A path dependency has an invalid package name
/// - A circular dependency is detected
/// - Compilation of a dependency fails
#[allow(dead_code)] // Used by tests; superseded by graph::resolve_dependency_graph
pub fn resolve_path_dependencies(
    project_root: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
) -> Result<Vec<ResolvedDependency>> {
    let manifest_path = project_root.join("beamtalk.toml");
    if !manifest_path.exists() {
        return Ok(Vec::new());
    }

    let manifest = manifest::parse_manifest_full(&manifest_path)?;
    if manifest.dependencies.is_empty() {
        return Ok(Vec::new());
    }

    let mut resolved = Vec::new();
    let mut visited = HashSet::new();
    visited.insert(manifest.package.name.clone());

    resolve_deps_recursive(
        project_root,
        &manifest,
        options,
        &mut resolved,
        &mut visited,
    )?;

    Ok(resolved)
}

/// Collect ebin paths for already-compiled path dependencies.
///
/// Scans `_build/deps/*/ebin/` for existing dependency ebin directories.
/// Used by commands (run, test) that need to add dependency ebin paths to the
/// BEAM code path without re-resolving/re-compiling.
pub fn collect_dep_ebin_paths(project_root: &Utf8Path) -> Vec<Utf8PathBuf> {
    let deps_dir = project_root.join("_build").join("deps");
    let mut paths = Vec::new();

    if let Ok(entries) = std::fs::read_dir(&deps_dir) {
        for entry in entries.flatten() {
            let ebin = entry.path().join("ebin");
            if ebin.is_dir() {
                if let Ok(utf8) = Utf8PathBuf::from_path_buf(ebin) {
                    paths.push(utf8);
                }
            }
        }
    }

    paths.sort();
    paths
}

/// Recursively resolve and compile path dependencies.
///
/// Tracks the set of visited package names to detect circular dependencies.
/// Only path dependencies are resolved; git dependencies are logged and skipped.
#[allow(dead_code)] // Used by tests; superseded by graph::resolve_dependency_graph
fn resolve_deps_recursive(
    project_root: &Utf8Path,
    manifest: &ParsedManifest,
    options: &beamtalk_core::CompilerOptions,
    resolved: &mut Vec<ResolvedDependency>,
    visited: &mut HashSet<String>,
) -> Result<()> {
    for (name, spec) in &manifest.dependencies {
        // Skip dependencies that have already been resolved (shared transitive deps)
        if resolved.iter().any(|r| r.name == *name) {
            debug!(dep = %name, "Dependency already resolved, skipping");
            continue;
        }

        match &spec.source {
            DependencySource::Path { path } => {
                resolve_single_path_dep(project_root, name, path, options, resolved, visited)?;
            }
            DependencySource::Git { url, .. } => {
                debug!(
                    dep = %name,
                    url = %url,
                    "Skipping git dependency (not yet implemented)"
                );
            }
        }
    }
    Ok(())
}

/// Resolve a single path dependency: validate, compile, and recurse into its deps.
#[allow(dead_code)] // Used by tests; superseded by graph::resolve_dependency_graph
fn resolve_single_path_dep(
    project_root: &Utf8Path,
    name: &str,
    relative_path: &std::path::Path,
    options: &beamtalk_core::CompilerOptions,
    resolved: &mut Vec<ResolvedDependency>,
    visited: &mut HashSet<String>,
) -> Result<()> {
    // Resolve relative path from the depending package's directory
    let relative_utf8 = Utf8Path::from_path(relative_path).ok_or_else(|| {
        miette::miette!(
            "Dependency '{name}' has a non-UTF-8 path: {}",
            relative_path.display()
        )
    })?;
    let dep_root = canonicalize_dep_path(project_root, relative_utf8);

    // Validate the dependency directory exists
    if !dep_root.exists() {
        miette::bail!(
            "Path dependency '{name}' directory does not exist: {dep_root}\n  \
             (resolved from '{relative_utf8}' relative to '{project_root}')"
        );
    }

    // Locate and validate the dependency's beamtalk.toml
    let dep_manifest_path = dep_root.join("beamtalk.toml");
    if !dep_manifest_path.exists() {
        miette::bail!(
            "Path dependency '{name}' is missing beamtalk.toml: {dep_manifest_path}\n  \
             Every dependency must have a beamtalk.toml package manifest."
        );
    }

    let dep_manifest = manifest::parse_manifest_full(&dep_manifest_path)
        .wrap_err_with(|| format!("Failed to parse manifest for dependency '{name}'"))?;

    // Validate the declared package name matches the dependency key
    if dep_manifest.package.name != name {
        miette::bail!(
            "Path dependency '{name}' has mismatched package name.\n  \
             Expected: '{name}' (from [dependencies] key)\n  \
             Found: '{}' (in {dep_manifest_path})\n  \
             The dependency key must match the package name in beamtalk.toml.",
            dep_manifest.package.name
        );
    }

    // Check for circular dependencies
    if !visited.insert(name.to_string()) {
        miette::bail!(
            "Circular dependency detected: '{name}' appears in its own dependency chain.\n  \
             A package cannot depend on itself, directly or transitively."
        );
    }

    // Recurse into the dependency's own dependencies first (topological order)
    if !dep_manifest.dependencies.is_empty() {
        resolve_deps_recursive(&dep_root, &dep_manifest, options, resolved, visited)?;
    }

    // Compile the dependency
    let (ebin_path, dep_class_module_index) =
        compile_dependency(project_root, &dep_root, name, options)?;

    info!(dep = %name, ebin = %ebin_path, "Resolved path dependency");

    resolved.push(ResolvedDependency {
        name: name.to_string(),
        root: dep_root,
        ebin_path,
        class_module_index: dep_class_module_index,
        is_direct: true, // Legacy path — all treated as direct
        via_chain: Vec::new(),
    });

    Ok(())
}

/// Resolve a relative dependency path to an absolute path.
///
/// Joins the relative path with the project root and canonicalizes the result.
/// Does not require the target to exist (unlike `std::fs::canonicalize`).
pub(crate) fn canonicalize_dep_path(
    project_root: &Utf8Path,
    relative_path: &Utf8Path,
) -> Utf8PathBuf {
    let joined = project_root.join(relative_path);

    // Normalize the path by resolving `.` and `..` components
    normalize_path(&joined)
}

/// Normalize a path by resolving `.` and `..` components without filesystem access.
///
/// Unlike `std::fs::canonicalize`, this does not require the path to exist and
/// does not resolve symlinks.
fn normalize_path(path: &Utf8Path) -> Utf8PathBuf {
    use camino::Utf8Component;

    let mut components = Vec::new();
    for component in path.components() {
        match component {
            Utf8Component::CurDir => {
                // Skip `.`
            }
            Utf8Component::ParentDir => {
                // Pop the last component if possible
                if !components.is_empty() {
                    let last = components.last().copied();
                    if last != Some(Utf8Component::ParentDir) {
                        components.pop();
                        continue;
                    }
                }
                components.push(component);
            }
            _ => {
                components.push(component);
            }
        }
    }

    if components.is_empty() {
        return Utf8PathBuf::from(".");
    }

    let mut result = Utf8PathBuf::new();
    for component in components {
        result.push(component.as_str());
    }
    result
}

/// Compile a dependency and return a `ResolvedDependency`.
///
/// This is used by the topological graph resolver (`graph.rs`) which has already
/// resolved prior dependencies. Their class module indexes are merged so that
/// cross-dependency class references resolve during compilation.
///
/// Output goes to `{project_root}/_build/deps/{name}/ebin/`.
pub(crate) fn compile_dependency_at(
    project_root: &Utf8Path,
    dep_root: &Utf8Path,
    dep_name: &str,
    options: &beamtalk_core::CompilerOptions,
    prior_deps: &[ResolvedDependency],
) -> Result<ResolvedDependency> {
    let (ebin_path, class_module_index) =
        compile_dependency_with_context(project_root, dep_root, dep_name, options, prior_deps)?;

    Ok(ResolvedDependency {
        name: dep_name.to_string(),
        root: dep_root.to_path_buf(),
        ebin_path,
        class_module_index,
        is_direct: false, // Caller sets this based on graph knowledge
        via_chain: Vec::new(),
    })
}

/// Compile a path dependency and return the ebin path and class module index.
///
/// Output goes to `{project_root}/_build/deps/{name}/ebin/`.
/// The dependency is compiled using the same build pipeline as `beamtalk build`.
#[allow(dead_code)] // Convenience wrapper; superseded by compile_dependency_with_context
fn compile_dependency(
    project_root: &Utf8Path,
    dep_root: &Utf8Path,
    dep_name: &str,
    options: &beamtalk_core::CompilerOptions,
) -> Result<(Utf8PathBuf, HashMap<String, String>)> {
    compile_dependency_with_context(project_root, dep_root, dep_name, options, &[])
}

/// Compile a dependency with context from already-compiled dependencies.
///
/// The `prior_deps` parameter provides class module indexes from dependencies
/// that have already been compiled (in topological order), allowing this
/// dependency's source to reference classes from its own dependencies.
fn compile_dependency_with_context(
    project_root: &Utf8Path,
    dep_root: &Utf8Path,
    dep_name: &str,
    options: &beamtalk_core::CompilerOptions,
    prior_deps: &[ResolvedDependency],
) -> Result<(Utf8PathBuf, HashMap<String, String>)> {
    let ebin_path = project_root
        .join("_build")
        .join("deps")
        .join(dep_name)
        .join("ebin");

    // Create the ebin directory
    std::fs::create_dir_all(&ebin_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create dependency ebin directory: {ebin_path}"))?;

    info!(dep = %dep_name, ebin = %ebin_path, "Compiling path dependency");

    // Find source files in the dependency
    let src_dir = dep_root.join("src");
    let search_dir = if src_dir.exists() {
        src_dir.clone()
    } else {
        dep_root.to_path_buf()
    };

    let source_files = crate::commands::build::collect_source_files_from_dir(&search_dir)?;

    if source_files.is_empty() {
        debug!(dep = %dep_name, "No source files found in dependency, skipping compilation");
        return Ok((ebin_path, HashMap::new()));
    }

    info!(
        dep = %dep_name,
        count = source_files.len(),
        "Found source files in dependency"
    );

    // Build the class → module index for the dependency
    let source_root = if src_dir.exists() {
        Some(src_dir)
    } else {
        None
    };

    let (mut class_module_index, class_superclass_index, all_class_infos, cached_asts) =
        crate::commands::build::build_class_module_index(
            &source_files,
            source_root.as_deref(),
            dep_name,
        )?;

    // Merge class indexes from already-compiled dependencies so that
    // cross-dependency class references resolve during compilation.
    for prior in prior_deps {
        for (class_name, module_name) in &prior.class_module_index {
            class_module_index
                .entry(class_name.clone())
                .or_insert_with(|| module_name.clone());
        }
    }

    // Compile each source file
    let mut core_files = Vec::new();
    let mut module_names = Vec::new();
    let mut cached_asts = cached_asts;

    for file in &source_files {
        let stem = file
            .file_stem()
            .ok_or_else(|| miette::miette!("File '{}' has no name", file))?;

        // Validate module name
        if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
            miette::bail!(
                "Invalid module name '{}' in dependency '{dep_name}': \
                 must contain only alphanumeric characters and underscores",
                stem
            );
        }

        let relative_module =
            crate::commands::build::compute_relative_module(file, source_root.as_deref())?;
        let module_name = format!("bt@{dep_name}@{relative_module}");
        let core_file = ebin_path.join(format!("{module_name}.core"));

        let cached = cached_asts.remove(file);
        crate::beam_compiler::compile_source_with_bindings(
            file,
            &module_name,
            &core_file,
            options,
            &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &class_module_index,
            &class_superclass_index,
            &all_class_infos,
            cached,
            None,  // No collision detection within dependency compilation
            false, // Dependencies use their own strict-deps setting, not root's
        )?;

        core_files.push(core_file);
        module_names.push(module_name);
    }

    // Batch compile Core Erlang to BEAM
    let compiler = crate::beam_compiler::BeamCompiler::new(ebin_path.clone());
    compiler
        .compile_batch(&core_files)
        .wrap_err_with(|| format!("Failed to compile dependency '{dep_name}' to BEAM bytecode"))?;

    // Generate .app file for the dependency
    let dep_manifest = manifest::parse_manifest(&dep_root.join("beamtalk.toml"))?;
    let class_metadata: Vec<crate::commands::app_file::ClassMetadata> = Vec::new();
    crate::commands::app_file::generate_app_file(
        &ebin_path,
        &dep_manifest,
        &module_names,
        &class_metadata,
        None,
        // TODO(ADR 0072): Wire up native module discovery for path dependencies
        &[],
    )?;

    info!(dep = %dep_name, "Dependency compiled successfully");

    Ok((ebin_path, class_module_index))
}

/// Build a class module index for a dependency without compiling.
///
/// Scans the dependency's source files and extracts class-to-module mappings.
/// This is the fast path used when deps are fresh and don't need recompilation.
pub(crate) fn build_dep_class_index(
    dep_root: &Utf8Path,
    dep_name: &str,
) -> Result<HashMap<String, String>> {
    let src_dir = dep_root.join("src");
    let search_dir = if src_dir.exists() {
        src_dir.clone()
    } else {
        dep_root.to_path_buf()
    };

    let source_files = crate::commands::build::collect_source_files_from_dir(&search_dir)?;
    if source_files.is_empty() {
        return Ok(HashMap::new());
    }

    let source_root = if src_dir.exists() {
        Some(src_dir)
    } else {
        None
    };

    let (class_module_index, _, _, _) = crate::commands::build::build_class_module_index(
        &source_files,
        source_root.as_deref(),
        dep_name,
    )?;

    Ok(class_module_index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    /// Create a minimal beamtalk.toml manifest in a directory.
    fn write_manifest(dir: &std::path::Path, name: &str, deps: &str) {
        let content = format!(
            r#"[package]
name = "{name}"
version = "0.1.0"

{deps}"#
        );
        fs::write(dir.join("beamtalk.toml"), content).unwrap();
    }

    /// Create a minimal .bt source file.
    fn write_source(dir: &std::path::Path, filename: &str, content: &str) {
        let src_dir = dir.join("src");
        fs::create_dir_all(&src_dir).unwrap();
        fs::write(src_dir.join(filename), content).unwrap();
    }

    #[test]
    fn test_normalize_path_resolves_parent() {
        let path = Utf8PathBuf::from("/home/user/project/../dep");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/home/user/dep"));
    }

    #[test]
    fn test_normalize_path_resolves_dot() {
        let path = Utf8PathBuf::from("/home/user/./project");
        assert_eq!(
            normalize_path(&path),
            Utf8PathBuf::from("/home/user/project")
        );
    }

    #[test]
    fn test_normalize_path_multiple_parents() {
        let path = Utf8PathBuf::from("/home/user/project/../../dep");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/home/dep"));
    }

    #[test]
    fn test_canonicalize_dep_path() {
        let result = canonicalize_dep_path(
            Utf8Path::new("/home/user/my_app"),
            Utf8Path::new("../utils"),
        );
        assert_eq!(result, Utf8PathBuf::from("/home/user/utils"));
    }

    #[test]
    fn test_resolve_no_manifest() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_path_dependencies(&project_root, &options).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_resolve_no_dependencies() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        write_manifest(temp.path(), "my_app", "");
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_path_dependencies(&project_root, &options).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_resolve_missing_dep_directory() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        write_manifest(
            temp.path(),
            "my_app",
            r#"[dependencies]
utils = { path = "../nonexistent" }"#,
        );
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_path_dependencies(&project_root, &options);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("does not exist"),
            "should mention missing directory: {err}"
        );
    }

    #[test]
    fn test_resolve_missing_dep_manifest() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create a dependency directory without beamtalk.toml
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();

        write_manifest(
            temp.path(),
            "my_app",
            r#"[dependencies]
utils = { path = "utils" }"#,
        );
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_path_dependencies(&project_root, &options);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("missing beamtalk.toml"),
            "should mention missing manifest: {err}"
        );
    }

    #[test]
    fn test_resolve_mismatched_package_name() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dependency with a different package name
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "wrong_name", "");

        write_manifest(
            temp.path(),
            "my_app",
            r#"[dependencies]
utils = { path = "utils" }"#,
        );
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_path_dependencies(&project_root, &options);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("mismatched"),
            "should mention name mismatch: {err}"
        );
    }

    #[test]
    fn test_resolve_circular_dependency() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create two packages that depend on each other
        let dep_dir = temp.path().join("dep_a");
        fs::create_dir_all(&dep_dir).unwrap();

        // dep_a depends on my_app (circular)
        // Use forward slashes for TOML compatibility on Windows
        let project_root_str = project_root.as_str().replace('\\', "/");
        write_manifest(
            &dep_dir,
            "dep_a",
            &format!(
                r#"[dependencies]
my_app = {{ path = "{project_root_str}" }}"#,
            ),
        );

        // my_app depends on dep_a
        write_manifest(
            temp.path(),
            "my_app",
            r#"[dependencies]
dep_a = { path = "dep_a" }"#,
        );
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_path_dependencies(&project_root, &options);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Circular") || err.contains("circular"),
            "should detect circular dependency: {err}"
        );
    }

    #[test]
    fn test_resolve_dep_with_no_source_files() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dependency with manifest but no source files
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "");

        write_manifest(
            temp.path(),
            "my_app",
            r#"[dependencies]
utils = { path = "utils" }"#,
        );
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_path_dependencies(&project_root, &options).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name, "utils");
        // No source files means no class index entries
        assert!(result[0].class_module_index.is_empty());
    }

    #[test]
    fn test_collect_dep_ebin_paths_empty() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let paths = collect_dep_ebin_paths(&project_root);
        assert!(paths.is_empty());
    }

    #[test]
    fn test_collect_dep_ebin_paths_finds_dirs() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create _build/deps/utils/ebin/
        let ebin_dir = temp
            .path()
            .join("_build")
            .join("deps")
            .join("utils")
            .join("ebin");
        fs::create_dir_all(&ebin_dir).unwrap();

        let paths = collect_dep_ebin_paths(&project_root);
        assert_eq!(paths.len(), 1);
        assert!(paths[0].as_str().contains("utils"));
    }

    #[test]
    fn test_resolve_transitive_deps() {
        // A depends on B, B depends on C
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create C (leaf dependency)
        let c_dir = temp.path().join("pkg_c");
        fs::create_dir_all(&c_dir).unwrap();
        write_manifest(&c_dir, "pkg_c", "");

        // Create B (depends on C)
        let b_dir = temp.path().join("pkg_b");
        fs::create_dir_all(&b_dir).unwrap();
        write_manifest(
            &b_dir,
            "pkg_b",
            r#"[dependencies]
pkg_c = { path = "../pkg_c" }"#,
        );

        // Create A (depends on B)
        write_manifest(
            temp.path(),
            "my_app",
            r#"[dependencies]
pkg_b = { path = "pkg_b" }"#,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = resolve_path_dependencies(&project_root, &options).unwrap();

        // Should resolve both B and C (C first due to topological order)
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].name, "pkg_c");
        assert_eq!(result[1].name, "pkg_b");
    }

    #[test]
    fn test_resolve_git_dep_skipped() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        write_manifest(
            temp.path(),
            "my_app",
            r#"[dependencies]
json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }"#,
        );
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_path_dependencies(&project_root, &options).unwrap();
        // Git deps are skipped, so result should be empty
        assert!(result.is_empty());
    }

    /// Integration test: package A depends on path dep B, A can use B's classes.
    ///
    /// This test creates two packages on disk:
    /// - `dep_utils` with a `Helper` class
    /// - `my_app` with a class that references `Helper`
    ///
    /// Then compiles `my_app` (which resolves and compiles `dep_utils`) and verifies
    /// the dependency's ebin directory is populated with BEAM files.
    ///
    /// Requires erlc and the beamtalk binary (runs via `beamtalk build`).
    #[test]
    #[ignore = "requires erlc"]
    fn test_full_path_dep_compilation() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dependency package: dep_utils with a Helper class
        let dep_dir = temp.path().join("dep_utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "dep_utils", "");
        write_source(
            &dep_dir,
            "helper.bt",
            "Object subclass: Helper\n  greet => \"hello from dep\"\n",
        );

        // Create main package that depends on dep_utils
        write_manifest(
            temp.path(),
            "my_app",
            r#"[dependencies]
dep_utils = { path = "dep_utils" }"#,
        );
        write_source(
            temp.path(),
            "app.bt",
            "Object subclass: App\n  run => Helper greet\n",
        );

        let options = beamtalk_core::CompilerOptions::default();

        // Resolve and compile dependencies
        let result = resolve_path_dependencies(&project_root, &options).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name, "dep_utils");

        // Verify the class_module_index contains the Helper class
        assert!(
            result[0].class_module_index.contains_key("Helper"),
            "dep class index should contain Helper: {:?}",
            result[0].class_module_index
        );

        // Verify ebin directory was created with .beam files
        let dep_ebin = &result[0].ebin_path;
        assert!(dep_ebin.exists(), "dep ebin directory should exist");

        let beam_files: Vec<_> = fs::read_dir(dep_ebin.as_std_path())
            .unwrap()
            .filter_map(std::result::Result::ok)
            .filter(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
            .collect();
        assert!(
            !beam_files.is_empty(),
            "dep ebin should contain .beam files"
        );
    }
}
