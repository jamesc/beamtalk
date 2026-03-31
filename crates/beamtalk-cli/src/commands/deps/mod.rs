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

use camino::{Utf8Path, Utf8PathBuf};
use miette::Result;
use std::collections::{BTreeMap, VecDeque};
use tracing::{debug, info};

use crate::commands::build_layout::BuildLayout;
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

/// A discovered dependency root (direct or transitive) for freshness checking.
struct DiscoveredDep {
    name: String,
    root: Utf8PathBuf,
    is_direct: bool,
    /// Whether this is a path dep (true) or git dep (false).
    is_path_dep: bool,
    /// For transitive deps, the chain of intermediate dep names.
    via_chain: Vec<String>,
}

/// Recursively discover all dependency names and roots by walking manifests.
///
/// Returns all deps (direct + transitive) without compilation or ebin checks.
/// Used by both `deps_are_fresh` and `collect_fresh_deps` to handle the full
/// transitive graph rather than just direct deps.
fn discover_all_dep_roots(
    project_root: &Utf8Path,
    manifest: &manifest::ParsedManifest,
) -> Result<Vec<DiscoveredDep>> {
    use beamtalk_core::compilation::DependencySource;

    let layout = BuildLayout::new(project_root);
    let direct_names: std::collections::HashSet<&str> =
        manifest.dependencies.keys().map(String::as_str).collect();

    // BFS queue: (parent_root, deps_map, via_chain)
    let mut queue: VecDeque<(
        Utf8PathBuf,
        BTreeMap<String, beamtalk_core::compilation::DependencySpec>,
        Vec<String>,
    )> = VecDeque::from([(
        project_root.to_path_buf(),
        manifest.dependencies.clone(),
        Vec::new(),
    )]);
    let mut visited = std::collections::HashSet::new();
    let mut result = Vec::new();

    while let Some((parent_root, deps, via_chain)) = queue.pop_front() {
        for (dep_name, spec) in &deps {
            if !visited.insert(dep_name.clone()) {
                continue; // Already discovered (diamond deps)
            }

            let (dep_root, is_path_dep) = match &spec.source {
                DependencySource::Path { path } => {
                    let relative_utf8 = camino::Utf8Path::from_path(path).ok_or_else(|| {
                        miette::miette!(
                            "Dependency '{dep_name}' has a non-UTF-8 path: {}",
                            path.display()
                        )
                    })?;
                    (
                        path::canonicalize_dep_path(&parent_root, relative_utf8),
                        true,
                    )
                }
                DependencySource::Git { .. } => (layout.dep_checkout_dir(dep_name), false),
            };

            let is_direct = direct_names.contains(dep_name.as_str());

            result.push(DiscoveredDep {
                name: dep_name.clone(),
                root: dep_root.clone(),
                is_direct,
                is_path_dep,
                via_chain: if is_direct {
                    Vec::new()
                } else {
                    via_chain.clone()
                },
            });

            // Enqueue this dep's own dependencies for discovery
            let dep_manifest_path = dep_root.join("beamtalk.toml");
            if let Ok(dep_parsed) = manifest::parse_manifest_full(&dep_manifest_path) {
                if !dep_parsed.dependencies.is_empty() {
                    let mut child_chain = via_chain.clone();
                    child_chain.push(dep_name.clone());
                    queue.push_back((dep_root, dep_parsed.dependencies, child_chain));
                }
            }
        }
    }

    Ok(result)
}

/// Collect `ResolvedDependency` structs for already-compiled dependencies.
///
/// Rebuilds the class module index from each dependency's source files
/// without recompiling. This is the fast path for the "deps are fresh" case.
/// Discovers the full transitive graph so that transitive deps are not lost.
fn collect_fresh_deps(
    project_root: &Utf8Path,
    parsed: &manifest::ParsedManifest,
) -> Result<Vec<path::ResolvedDependency>> {
    let layout = BuildLayout::new(project_root);
    let all_deps = discover_all_dep_roots(project_root, parsed)?;
    let mut resolved = Vec::new();

    for dep in &all_deps {
        let ebin_path = layout.dep_ebin_dir(&dep.name);

        // Rebuild class module index from source files (fast — no compilation)
        let (class_module_index, class_infos) = path::build_dep_class_index(&dep.root, &dep.name)?;

        debug!(
            dep = %dep.name,
            classes = class_module_index.len(),
            is_direct = dep.is_direct,
            "Loaded fresh dependency class index"
        );

        resolved.push(path::ResolvedDependency {
            name: dep.name.clone(),
            root: dep.root.clone(),
            ebin_path,
            class_module_index,
            class_infos,
            is_direct: dep.is_direct,
            via_chain: dep.via_chain.clone(),
        });
    }

    Ok(resolved)
}

/// Check whether all dependencies (direct and transitive) are already
/// resolved and compiled.
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

    // Discover all deps (direct + transitive) and check their ebin directories.
    // If discovery itself fails (e.g. non-UTF-8 path), treat as stale.
    let all_deps = match discover_all_dep_roots(project_root, manifest) {
        Ok(deps) => deps,
        Err(e) => {
            debug!(error = %e, "Failed to discover dep roots — deps are stale");
            return false;
        }
    };
    let layout = BuildLayout::new(project_root);

    for dep in &all_deps {
        let ebin_dir = layout.dep_ebin_dir(&dep.name);
        if !ebin_dir.exists() {
            debug!(dep = %dep.name, "Dependency ebin directory missing — deps are stale");
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
            debug!(dep = %dep.name, "Dependency ebin directory has no .beam files — deps are stale");
            return false;
        }

        // Check if the dep's beamtalk.toml is newer than compiled output.
        // This catches transitive manifest changes (e.g., a dep changing its
        // own git dep to a new tag) that wouldn't be caught by root mtime checks.
        let dep_manifest = dep.root.join("beamtalk.toml");
        if manifest_newer_than_ebin(&dep_manifest, &ebin_dir) {
            debug!(dep = %dep.name, manifest = %dep_manifest, "Dependency manifest is newer than compiled output — deps are stale");
            return false;
        }

        // For path deps, check if source files are newer than compiled output.
        // Git deps don't need mtime checking — the lockfile handles freshness.
        if dep.is_path_dep && path_dep_source_newer_than_ebin(&dep.root, &ebin_dir) {
            debug!(dep = %dep.name, "Path dependency source is newer than compiled output — deps are stale");
            return false;
        }
    }

    true
}

/// Check if any `.bt` source file in a path dependency is newer than the
/// oldest `.beam` file in its ebin directory.
///
/// Returns `true` if recompilation is needed.
fn manifest_newer_than_ebin(manifest_path: &Utf8Path, ebin_dir: &Utf8Path) -> bool {
    let Ok(manifest_meta) = std::fs::metadata(manifest_path.as_std_path()) else {
        return false; // No manifest → can't determine, not stale on this check
    };
    let Ok(manifest_mtime) = manifest_meta.modified() else {
        return false;
    };

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

    manifest_mtime > oldest_beam_mtime
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
        let root_utf8 = camino::Utf8PathBuf::from_path_buf(project_root.to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root_utf8);
        let ebin_dir = layout.dep_ebin_dir(dep_name);
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
        let test_layout = BuildLayout::new(&root);
        let ebin_dir = test_layout.dep_ebin_dir("utils");
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

    #[test]
    fn test_deps_stale_transitive_ebin_missing() {
        // my_app -> utils -> shared
        // utils ebin exists, shared ebin is missing → should be stale
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create shared (leaf dep)
        let shared_dir = temp.path().join("shared");
        fs::create_dir_all(&shared_dir).unwrap();
        write_manifest(&shared_dir, "shared", "");

        // Create utils (depends on shared)
        let utils_dir = temp.path().join("utils");
        fs::create_dir_all(&utils_dir).unwrap();
        write_manifest(
            &utils_dir,
            "utils",
            "[dependencies]\nshared = { path = \"../shared\" }",
        );

        // Create compiled ebin for utils (direct dep) but NOT for shared (transitive)
        create_dep_ebin_with_beam(temp.path(), "utils");

        // Create main manifest
        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Should be stale because transitive dep "shared" has no ebin
        assert!(
            !deps_are_fresh(&root, &parsed),
            "Should detect missing transitive dep ebin as stale"
        );
    }

    #[test]
    fn test_collect_fresh_deps_includes_transitive() {
        // my_app -> utils -> shared
        // Both compiled — collect_fresh_deps should return both
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create shared (leaf dep) with source
        let shared_dir = temp.path().join("shared");
        fs::create_dir_all(&shared_dir).unwrap();
        write_manifest(&shared_dir, "shared", "");
        write_source(
            &shared_dir,
            "base.bt",
            "Object subclass: Base\n  name => \"base\"\n",
        );

        // Create utils (depends on shared) with source
        let utils_dir = temp.path().join("utils");
        fs::create_dir_all(&utils_dir).unwrap();
        write_manifest(
            &utils_dir,
            "utils",
            "[dependencies]\nshared = { path = \"../shared\" }",
        );
        write_source(
            &utils_dir,
            "helper.bt",
            "Object subclass: Helper\n  greet => \"hi\"\n",
        );

        // Create compiled ebin for both deps
        create_dep_ebin_with_beam(temp.path(), "utils");
        create_dep_ebin_with_beam(temp.path(), "shared");

        // Create main manifest
        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        let result = collect_fresh_deps(&root, &parsed).unwrap();
        let names: Vec<&str> = result.iter().map(|r| r.name.as_str()).collect();

        assert!(
            names.contains(&"utils"),
            "Should include direct dep: {names:?}"
        );
        assert!(
            names.contains(&"shared"),
            "Should include transitive dep: {names:?}"
        );

        // Check is_direct metadata
        let utils = result.iter().find(|r| r.name == "utils").unwrap();
        assert!(utils.is_direct, "utils should be marked as direct");

        let shared = result.iter().find(|r| r.name == "shared").unwrap();
        assert!(!shared.is_direct, "shared should be marked as transitive");
    }

    #[test]
    fn test_discover_all_dep_roots_diamond_dedup() {
        // Diamond: my_app -> A, my_app -> B, A -> shared, B -> shared
        // shared should appear only once
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let shared_dir = temp.path().join("shared");
        fs::create_dir_all(&shared_dir).unwrap();
        write_manifest(&shared_dir, "shared", "");

        let a_dir = temp.path().join("pkg_a");
        fs::create_dir_all(&a_dir).unwrap();
        write_manifest(
            &a_dir,
            "pkg_a",
            "[dependencies]\nshared = { path = \"../shared\" }",
        );

        let b_dir = temp.path().join("pkg_b");
        fs::create_dir_all(&b_dir).unwrap();
        write_manifest(
            &b_dir,
            "pkg_b",
            "[dependencies]\nshared = { path = \"../shared\" }",
        );

        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\npkg_a = { path = \"pkg_a\" }\npkg_b = { path = \"pkg_b\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        let deps = discover_all_dep_roots(&root, &parsed).unwrap();
        let shared_count = deps.iter().filter(|d| d.name == "shared").count();
        assert_eq!(
            shared_count, 1,
            "Diamond dep 'shared' should appear exactly once, got {shared_count}"
        );
        assert_eq!(deps.len(), 3, "Should have pkg_a, pkg_b, shared");
    }

    #[test]
    fn test_deps_fresh_with_git_dep_and_lockfile() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create compiled ebin for git dep
        create_dep_ebin_with_beam(temp.path(), "json");

        // Create a fake git checkout dir (so discover_all_dep_roots finds it)
        let layout = BuildLayout::new(&root);
        let checkout = layout.dep_checkout_dir("json");
        fs::create_dir_all(&checkout).unwrap();
        fs::write(
            checkout.join("beamtalk.toml"),
            "[package]\nname = \"json\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();

        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\njson = { git = \"https://example.com/json\", tag = \"v1.0\" }",
        );

        // Create a lockfile newer than the manifest
        std::thread::sleep(std::time::Duration::from_millis(50));
        fs::write(
            temp.path().join("beamtalk.lock"),
            "# lockfile\n[[package]]\nname = \"json\"\nurl = \"https://example.com/json\"\nref = \"tag:v1.0\"\nsha = \"abc123\"\n",
        )
        .unwrap();

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Git dep with lockfile + compiled ebin should be fresh
        assert!(
            deps_are_fresh(&root, &parsed),
            "Git dep with lockfile and ebin should be fresh"
        );
    }

    #[test]
    fn test_deps_are_fresh_false_when_manifest_newer_than_lock() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create compiled ebin
        create_dep_ebin_with_beam(temp.path(), "json");

        // Create checkout
        let layout = BuildLayout::new(&root);
        let checkout = layout.dep_checkout_dir("json");
        fs::create_dir_all(&checkout).unwrap();
        fs::write(
            checkout.join("beamtalk.toml"),
            "[package]\nname = \"json\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();

        // Create lockfile first
        fs::write(
            temp.path().join("beamtalk.lock"),
            "# lockfile\n[[package]]\nname = \"json\"\nurl = \"https://example.com/json\"\nref = \"tag:v1.0\"\nsha = \"abc123\"\n",
        )
        .unwrap();

        std::thread::sleep(std::time::Duration::from_millis(50));

        // Then create manifest (newer than lockfile)
        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\njson = { git = \"https://example.com/json\", tag = \"v1.0\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        assert!(
            !deps_are_fresh(&root, &parsed),
            "Should be stale when manifest is newer than lockfile"
        );
    }

    #[test]
    fn test_deps_stale_transitive_manifest_changed() {
        // root -> middle(path) -> leaf(path)
        // All ebin compiled, but then middle's beamtalk.toml is modified
        // (e.g. changing leaf's path). Should detect as stale.
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create leaf dep
        let leaf_dir = temp.path().join("leaf");
        fs::create_dir_all(&leaf_dir).unwrap();
        write_manifest(&leaf_dir, "leaf", "");

        // Create middle dep (depends on leaf)
        let middle_dir = temp.path().join("middle");
        fs::create_dir_all(&middle_dir).unwrap();
        write_manifest(
            &middle_dir,
            "middle",
            "[dependencies]\nleaf = { path = \"../leaf\" }",
        );

        // Create compiled ebin for both FIRST (older mtime)
        create_dep_ebin_with_beam(temp.path(), "middle");
        create_dep_ebin_with_beam(temp.path(), "leaf");

        // Small delay then modify middle's manifest (newer mtime)
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_manifest(
            &middle_dir,
            "middle",
            "[dependencies]\nleaf = { path = \"../leaf\" }  # changed",
        );

        // Root manifest
        write_manifest(
            temp.path(),
            "my_app",
            "[dependencies]\nmiddle = { path = \"middle\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        assert!(
            !deps_are_fresh(&root, &parsed),
            "Should detect transitive manifest change as stale"
        );
    }
}
