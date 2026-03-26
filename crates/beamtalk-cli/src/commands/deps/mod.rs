// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dependency resolution for Beamtalk packages (ADR 0070 Phase 1).
//!
//! **DDD Context:** Build System
//!
//! This module handles fetching and resolving package dependencies declared in
//! `beamtalk.toml`. Phase 1 supports:
//! - **Path dependencies:** local filesystem paths (for monorepo/development)
//! - **Git dependencies:** clone repos, check out tag/branch/rev, resolve to exact SHA
//! - **Lockfile:** `beamtalk.lock` pins exact commit SHAs for reproducible builds
//! - **Topological ordering:** compile dependencies in correct order (leaves first)
//! - **Cycle detection:** clear error when circular dependencies are found
//! - **Single-version policy:** error when same package appears at different versions

pub mod cli;
pub mod git;
pub mod graph;
pub mod lockfile;
pub mod path;

// Re-export commonly used items
pub use path::collect_dep_ebin_paths;

use camino::Utf8Path;
use miette::Result;
use tracing::{debug, info};

use crate::commands::manifest;

/// Ensure dependencies are resolved and compiled, with staleness detection.
///
/// This implements the "Cargo model" from ADR 0070: build/test/repl
/// automatically fetch and compile dependencies when needed. When the
/// lockfile is fresh and all dependency ebin directories already contain
/// compiled `.beam` files, recompilation is skipped — only the class
/// module index is rebuilt from source (fast, no erlc).
///
/// Staleness triggers (any one causes full re-resolution + compilation):
/// - `beamtalk.toml` has dependencies but no lockfile exists and git deps are present
/// - `beamtalk.toml` was modified after the lockfile
/// - Any dependency's `_build/deps/{name}/ebin/` directory is missing or empty
///
/// Always returns `ResolvedDependency` structs with class module indexes
/// so that the caller can merge them for cross-package class resolution.
pub fn ensure_deps_resolved(
    project_root: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
) -> Result<Vec<path::ResolvedDependency>> {
    let manifest_path = project_root.join("beamtalk.toml");
    if !manifest_path.exists() {
        return Ok(Vec::new());
    }

    let parsed = manifest::parse_manifest_full(&manifest_path)?;
    if parsed.dependencies.is_empty() {
        return Ok(Vec::new());
    }

    if deps_are_fresh(project_root, &parsed) {
        info!("Dependencies are fresh, skipping compilation");
        return collect_fresh_deps(project_root, &parsed);
    }

    info!("Dependencies need resolution, resolving...");
    graph::resolve_dependency_graph(project_root, options)
}

/// Collect `ResolvedDependency` structs for already-compiled dependencies.
///
/// Rebuilds the class module index from each dependency's source files
/// without recompiling. This is the fast path for the "deps are fresh" case.
fn collect_fresh_deps(
    project_root: &Utf8Path,
    parsed: &manifest::ParsedManifest,
) -> Result<Vec<path::ResolvedDependency>> {
    use beamtalk_core::compilation::DependencySource;

    let mut resolved = Vec::new();

    for (dep_name, spec) in &parsed.dependencies {
        let dep_root = match &spec.source {
            DependencySource::Path { path } => {
                let relative_utf8 = camino::Utf8Path::from_path(path).ok_or_else(|| {
                    miette::miette!(
                        "Dependency '{dep_name}' has a non-UTF-8 path: {}",
                        path.display()
                    )
                })?;
                path::canonicalize_dep_path(project_root, relative_utf8)
            }
            DependencySource::Git { .. } => {
                // Git deps are cloned to _build/deps/{name}/
                project_root.join("_build").join("deps").join(dep_name)
            }
        };

        let ebin_path = project_root
            .join("_build")
            .join("deps")
            .join(dep_name)
            .join("ebin");

        // Rebuild class module index from source files (fast — no compilation)
        let class_module_index = path::build_dep_class_index(&dep_root, dep_name)?;

        debug!(
            dep = %dep_name,
            classes = class_module_index.len(),
            "Loaded fresh dependency class index"
        );

        resolved.push(path::ResolvedDependency {
            name: dep_name.clone(),
            root: dep_root,
            ebin_path,
            class_module_index,
            is_direct: true, // collect_fresh_deps only processes direct deps
            via_chain: Vec::new(),
        });
    }

    Ok(resolved)
}

/// Check whether all dependencies are already resolved and compiled.
///
/// Returns `true` (fresh) when:
/// 1. All dependency ebin directories exist under `_build/deps/{name}/ebin/`
/// 2. The lockfile exists and is newer than `beamtalk.toml` (for git deps),
///    OR there are no git dependencies (path-only deps don't use a lockfile)
fn deps_are_fresh(project_root: &Utf8Path, manifest: &manifest::ParsedManifest) -> bool {
    let has_git_deps = manifest.dependencies.values().any(|spec| {
        matches!(
            spec.source,
            beamtalk_core::compilation::DependencySource::Git { .. }
        )
    });

    // Check lockfile freshness for git deps
    if has_git_deps {
        let lockfile_path = project_root.join(lockfile::LOCKFILE_NAME);
        if !lockfile_path.exists() {
            debug!("Lockfile missing but git deps present — deps are stale");
            return false;
        }

        // Compare modification times: if beamtalk.toml is newer than lockfile, stale
        let manifest_path = project_root.join("beamtalk.toml");
        if let (Ok(manifest_meta), Ok(lock_meta)) = (
            std::fs::metadata(&manifest_path),
            std::fs::metadata(&lockfile_path),
        ) {
            if let (Ok(manifest_mtime), Ok(lock_mtime)) =
                (manifest_meta.modified(), lock_meta.modified())
            {
                if manifest_mtime > lock_mtime {
                    debug!("beamtalk.toml is newer than lockfile — deps are stale");
                    return false;
                }
            }
        }
    }

    // Check that all dependency ebin directories exist and are up-to-date
    for (dep_name, spec) in &manifest.dependencies {
        let ebin_dir = project_root
            .join("_build")
            .join("deps")
            .join(dep_name)
            .join("ebin");
        if !ebin_dir.exists() {
            debug!(dep = %dep_name, "Dependency ebin directory missing — deps are stale");
            return false;
        }

        // Check that ebin dir actually has .beam files
        let has_beam = std::fs::read_dir(&ebin_dir)
            .map(|entries| {
                entries
                    .filter_map(std::result::Result::ok)
                    .any(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
            })
            .unwrap_or(false);

        if !has_beam {
            debug!(dep = %dep_name, "Dependency ebin directory has no .beam files — deps are stale");
            return false;
        }

        // For path deps, check if source files are newer than compiled output.
        // This catches the common dev workflow of editing a path dep's source.
        if let beamtalk_core::compilation::DependencySource::Path { path: dep_path } = &spec.source
        {
            if let Some(relative_utf8) = camino::Utf8Path::from_path(dep_path) {
                let dep_root = path::canonicalize_dep_path(project_root, relative_utf8);
                if path_dep_source_newer_than_ebin(&dep_root, &ebin_dir) {
                    debug!(dep = %dep_name, "Path dependency source is newer than compiled output — deps are stale");
                    return false;
                }
            }
        }
    }

    true
}

/// Check if any `.bt` source file in a path dependency is newer than the
/// oldest `.beam` file in its ebin directory.
///
/// Returns `true` if recompilation is needed.
fn path_dep_source_newer_than_ebin(dep_root: &Utf8Path, ebin_dir: &Utf8Path) -> bool {
    // Find the oldest .beam mtime (conservative: if ANY beam is older than ANY source, recompile)
    let oldest_beam = std::fs::read_dir(ebin_dir.as_std_path())
        .into_iter()
        .flatten()
        .filter_map(std::result::Result::ok)
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
        .filter_map(|e| e.metadata().ok()?.modified().ok())
        .min();

    let Some(oldest_beam_mtime) = oldest_beam else {
        return true; // No beam files → stale
    };

    // Find the newest .bt source mtime
    let src_dir = dep_root.join("src");
    let search_dir = if src_dir.exists() {
        src_dir.as_std_path()
    } else {
        dep_root.as_std_path()
    };

    newest_bt_mtime(search_dir).is_some_and(|src_mtime| src_mtime > oldest_beam_mtime)
}

/// Find the newest modification time of any `.bt` file under a directory (recursive).
fn newest_bt_mtime(dir: &std::path::Path) -> Option<std::time::SystemTime> {
    fn walk(dir: &std::path::Path, newest: &mut Option<std::time::SystemTime>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, newest);
            } else if path.extension().is_some_and(|ext| ext == "bt") {
                if let Ok(meta) = path.metadata() {
                    if let Ok(mtime) = meta.modified() {
                        *newest = Some(newest.map_or(mtime, |n| n.max(mtime)));
                    }
                }
            }
        }
    }

    let mut newest = None;
    walk(dir, &mut newest);
    newest
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn write_manifest(dir: &std::path::Path, name: &str, deps: &str) {
        let content = format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n\n{deps}");
        fs::write(dir.join("beamtalk.toml"), content).unwrap();
    }

    fn write_source(dir: &std::path::Path, filename: &str, content: &str) {
        let src_dir = dir.join("src");
        fs::create_dir_all(&src_dir).unwrap();
        fs::write(src_dir.join(filename), content).unwrap();
    }

    fn create_dep_ebin_with_beam(project_root: &std::path::Path, dep_name: &str) {
        let ebin_dir = project_root
            .join("_build")
            .join("deps")
            .join(dep_name)
            .join("ebin");
        fs::create_dir_all(&ebin_dir).unwrap();
        // Create a fake .beam file
        fs::write(ebin_dir.join(format!("bt@{dep_name}@helper.beam")), b"BEAM").unwrap();
    }

    #[test]
    fn test_ensure_deps_no_manifest() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = ensure_deps_resolved(&root, &options).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_ensure_deps_no_dependencies() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        write_manifest(temp.path(), "my_app", "");
        let options = beamtalk_core::CompilerOptions::default();

        let result = ensure_deps_resolved(&root, &options).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_deps_fresh_with_compiled_path_dep() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep directory with manifest and source
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "");
        write_source(
            &dep_dir,
            "helper.bt",
            "Object subclass: Helper\n  greet => \"hi\"\n",
        );

        // Create compiled ebin
        create_dep_ebin_with_beam(temp.path(), "utils");

        // Create main manifest with path dep
        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Path deps with compiled ebin should be fresh
        assert!(deps_are_fresh(&root, &parsed));
    }

    #[test]
    fn test_deps_stale_missing_ebin() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep directory
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "");

        // Create main manifest with path dep — no ebin dir
        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        assert!(!deps_are_fresh(&root, &parsed));
    }

    #[test]
    fn test_deps_stale_empty_ebin() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep directory
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "");

        // Create ebin dir but no .beam files
        let ebin_dir = temp
            .path()
            .join("_build")
            .join("deps")
            .join("utils")
            .join("ebin");
        fs::create_dir_all(&ebin_dir).unwrap();

        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        assert!(!deps_are_fresh(&root, &parsed));
    }

    #[test]
    fn test_deps_stale_git_no_lockfile() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create compiled ebin for git dep
        create_dep_ebin_with_beam(temp.path(), "json");

        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\njson = { git = \"https://example.com/json\", tag = \"v1.0\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Git deps without lockfile should be stale
        assert!(!deps_are_fresh(&root, &parsed));
    }

    #[test]
    fn test_deps_stale_path_source_newer_than_ebin() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep directory with manifest
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "");

        // Create compiled ebin FIRST (older mtime)
        create_dep_ebin_with_beam(temp.path(), "utils");

        // Small delay to ensure mtime differs
        std::thread::sleep(std::time::Duration::from_millis(50));

        // Then create source file (newer mtime)
        write_source(
            &dep_dir,
            "helper.bt",
            "Object subclass: Helper\n  greet => \"hello\"\n",
        );

        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Source is newer than beam → stale
        assert!(!deps_are_fresh(&root, &parsed));
    }

    #[test]
    fn test_collect_fresh_deps_returns_class_index() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep with source
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "");
        write_source(
            &dep_dir,
            "helper.bt",
            "Object subclass: Helper\n  greet => \"hi\"\n",
        );

        // Create compiled ebin
        create_dep_ebin_with_beam(temp.path(), "utils");

        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        let result = collect_fresh_deps(&root, &parsed).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name, "utils");
        assert!(
            result[0].class_module_index.contains_key("Helper"),
            "Should contain Helper class in index: {:?}",
            result[0].class_module_index
        );
        assert_eq!(
            result[0].class_module_index.get("Helper").unwrap(),
            "bt@utils@helper"
        );
    }
}
