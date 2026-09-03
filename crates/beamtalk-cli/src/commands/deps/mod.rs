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
//! - **Registry dependencies:** an exact version resolved through the registry
//!   index into a `(git url, tag)` pair, then fetched as a git dependency
//! - **Lockfile:** `beamtalk.lock` pins exact commit SHAs for reproducible builds
//! - **Topological ordering:** compile dependencies in correct order (leaves first)
//! - **Cycle detection:** clear error when circular dependencies are found
//! - **Single-version policy:** error when same package appears at different versions

pub mod cli;
pub mod git;
pub mod graph;
pub mod lockfile;
pub mod path;
pub mod registry;
mod snapshot;

// Re-export commonly used items
pub use path::collect_dep_ebin_paths;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, Result, WrapErr};
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
/// - The transitive dependency graph changed shape since the last successful
///   resolve — e.g. an intermediate manifest swapped a dependency between a
///   git and a path source (BT-3009)
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
        clean_all_stale_deps(project_root)?;
        return Ok(Vec::new());
    }

    let resolved = if deps_are_fresh(project_root, &parsed) {
        info!("Dependencies are fresh, skipping compilation");
        collect_fresh_deps(project_root, &parsed)?
    } else {
        info!("Dependencies need resolution, resolving...");
        let resolved = graph::resolve_dependency_graph(project_root, options)?;
        // Record the graph we just resolved as the baseline the next build's
        // structural freshness check compares against (BT-3009). Discovery is
        // re-run *after* resolution so every git/registry checkout and every
        // transitive manifest is on disk and the recorded graph is complete.
        record_dep_graph_snapshot(project_root, &parsed);
        resolved
    };

    clean_stale_deps(project_root, &resolved)?;

    Ok(resolved)
}

/// Remove stale dependency directories from `_build/deps/`.
///
/// Compares the expected set of dependency names derived from `resolved_deps`
/// against directories that exist on disk under `_build/deps/`. Any directory
/// not in the expected set is treated as stale and is deleted to prevent its
/// `.beam` files from polluting code paths and type specs.
///
/// This is a no-op only when `_build/deps/` does not exist. If the expected
/// set is empty, all dependency directories under `_build/deps/` are
/// considered stale and deleted.
pub fn clean_stale_deps(
    project_root: &Utf8Path,
    resolved_deps: &[path::ResolvedDependency],
) -> Result<()> {
    let layout = BuildLayout::new(project_root);
    let deps_dir = layout.deps_dir();

    if !deps_dir.exists() {
        return Ok(());
    }

    let expected: std::collections::HashSet<&str> =
        resolved_deps.iter().map(|d| d.name.as_str()).collect();

    let entries = std::fs::read_dir(&deps_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read deps directory '{deps_dir}'"))?;

    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();

        let Some(dir_name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };

        if !path.is_dir() {
            continue;
        }

        if !expected.contains(dir_name) {
            info!(dep = %dir_name, "Removing stale dependency directory");
            std::fs::remove_dir_all(&path)
                .into_diagnostic()
                .wrap_err_with(|| {
                    format!(
                        "Failed to remove stale dependency directory '{}'",
                        path.display()
                    )
                })?;
        }
    }

    Ok(())
}

/// Remove stale dependency directories when no dependencies are declared.
///
/// When a project's `beamtalk.toml` has no `[dependencies]` section, any
/// existing `_build/deps/` contents are entirely stale.
/// This is called from the empty-dependencies early-return path in
/// `ensure_deps_resolved`.
pub fn clean_all_stale_deps(project_root: &Utf8Path) -> Result<()> {
    clean_stale_deps(project_root, &[])
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
    /// How this dependency was declared by its immediate parent manifest
    /// (path / git / registry, with the declared version or reference).
    ///
    /// Carried so freshness checks (`deps_are_fresh`'s `has_locked_deps` and
    /// `locked_deps_match`) can compare the *declared* spec against the
    /// lockfile for every dependency in the transitive graph, not just the
    /// root manifest's `[dependencies]` (BT-2994) — a monorepo's root
    /// commonly declares only a path dependency, with the git/registry dep
    /// whose version actually matters appearing several levels down.
    source: beamtalk_core::compilation::DependencySource,
    /// This dependency's own package-bundled FFI type stubs directory (ADR
    /// 0075 layer 2), from its own `beamtalk.toml` `[stubs] path`.
    stubs_dir: Option<Utf8PathBuf>,
}

/// Recursively discover all dependency names and roots by walking manifests.
///
/// Returns all deps (direct + transitive) without compilation or ebin checks.
/// Used by both `deps_are_fresh` and `collect_fresh_deps` to handle the full
/// transitive graph rather than just direct deps.
///
/// Note (BT-2836): `dependency_classes.rs`'s offline MCP
/// `lint`/`diagnostic_summary` dependency-class resolution needs the same
/// transitive-walk reachability but cannot call this function directly — it
/// lives in the library crate (`lib.rs`) while this module is compiled only
/// into the `beamtalk-cli` binary (`mod commands;` in `main.rs`). It
/// reimplements an equivalent walk instead; keep the two in sync if this
/// algorithm changes.
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
                // Registry deps are fetched into the same checkout directory
                // as git deps — they *are* git deps once resolved.
                DependencySource::Git { .. } | DependencySource::Registry { .. } => {
                    (layout.dep_checkout_dir(dep_name), false)
                }
            };

            let is_direct = direct_names.contains(dep_name.as_str());

            // Parse this dep's own manifest up front so its package-bundled
            // stubs (ADR 0075 layer 2, one hop only — never its own deps'
            // stubs) can be recorded alongside it.
            let dep_manifest_path = dep_root.join("beamtalk.toml");
            let dep_parsed = manifest::parse_manifest_full(&dep_manifest_path).ok();
            let stubs_dir = dep_parsed
                .as_ref()
                .and_then(|m| path::resolve_dep_stubs_dir(m, &dep_root));

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
                source: spec.source.clone(),
                stubs_dir,
            });

            // Enqueue this dep's own dependencies for discovery
            if let Some(dep_parsed) = dep_parsed {
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

        // Rebuild class/protocol/alias indexes from source files (fast — no compilation)
        let (class_module_index, class_infos, protocol_infos, alias_infos) =
            path::build_dep_class_index(&dep.root, &dep.name)?;

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
            protocol_infos,
            alias_infos,
            is_direct: dep.is_direct,
            via_chain: dep.via_chain.clone(),
            stubs_dir: dep.stubs_dir.clone(),
        });
    }

    Ok(resolved)
}

/// Check that every git/registry dependency discovered anywhere in the
/// transitive graph — not just the root manifest's `[dependencies]` — still
/// matches what the lockfile actually pinned.
///
/// For a registry dep this means the locked version *and* registry
/// (`BEAMTALK_REGISTRY`, or a `[registry] url` edit — BT-2993) still match
/// the manifest's request. For a plain git dep it means the locked URL and
/// reference (tag/branch/rev) still match.
///
/// A version or tag bump must force re-resolution even when it's declared by
/// an *intermediate* path dependency's own manifest several levels below the
/// root (BT-2994) — e.g. the root declares only `utils = { path = "../utils" }`
/// and `utils/beamtalk.toml` bumps `yaml` from `"0.2.1"` to `"0.3.0"`. Mtime
/// comparisons can miss this (edits landing within the same timestamp
/// granularity, or an intermediate dep's own ebin mtime happening to still
/// look current), so this is a deterministic field comparison against the
/// lockfile instead — the same technique BT-2993 used for registry switches,
/// generalized to the whole transitive graph and to plain git deps too.
fn locked_deps_match(
    project_root: &Utf8Path,
    all_deps: &[DiscoveredDep],
    root_manifest: &manifest::ParsedManifest,
) -> bool {
    use beamtalk_core::compilation::DependencySource;

    let requested: Vec<&DiscoveredDep> = all_deps
        .iter()
        .filter(|dep| {
            matches!(
                dep.source,
                DependencySource::Git { .. } | DependencySource::Registry { .. }
            )
        })
        .collect();

    if requested.is_empty() {
        return true;
    }

    let lock = match lockfile::Lockfile::read(project_root) {
        Ok(Some(lock)) => lock,
        Ok(None) => {
            debug!("Lockfile missing for git/registry deps — deps are stale");
            return false;
        }
        Err(e) => {
            debug!(error = %e, "Failed to read lockfile — deps are stale");
            return false;
        }
    };

    // The registry is a property of the whole project being built — the
    // root manifest's `[registry]` governs every registry dependency in the
    // graph, direct or transitive, because a dependency cannot redirect
    // resolution to a registry of its own (see graph.rs's
    // `DiscoveryContext::registry_location`). So it's resolved once here
    // rather than per dependency.
    let current_registry = registry::registry_identity(root_manifest.registry.as_ref());

    for dep in requested {
        match &dep.source {
            DependencySource::Registry { version } => match lock.get(&dep.name) {
                Some(entry)
                    if entry.registry_version.as_ref().is_some_and(|rv| {
                        &rv.version == version
                            && rv.registry.as_deref() == Some(current_registry.as_str())
                    }) => {}
                Some(_) => {
                    debug!(
                        dep = %dep.name,
                        version = %version,
                        "Registry dep version or registry differs from lockfile — deps are stale"
                    );
                    return false;
                }
                None => {
                    debug!(dep = %dep.name, "Registry dep not in lockfile — deps are stale");
                    return false;
                }
            },
            DependencySource::Git { url, reference } => match lock.get(&dep.name) {
                Some(entry) if &entry.url == url && &entry.reference == reference => {}
                Some(_) => {
                    debug!(
                        dep = %dep.name,
                        "Git dep url or reference differs from lockfile — deps are stale"
                    );
                    return false;
                }
                None => {
                    debug!(dep = %dep.name, "Git dep not in lockfile — deps are stale");
                    return false;
                }
            },
            DependencySource::Path { .. } => {
                unreachable!("filtered to Git/Registry sources above")
            }
        }
    }

    true
}

/// Check whether all dependencies (direct and transitive) are already
/// resolved and compiled.
///
/// Returns `true` (fresh) when:
/// 1. All dependency ebin directories exist under `_build/deps/{name}/ebin/`
/// 2. The lockfile exists and is newer than `beamtalk.toml` (for git and
///    registry deps anywhere in the transitive graph), OR there are only
///    path dependencies (which don't use a lockfile)
/// 3. Every git/registry dep's locked version or reference still matches
///    what its declaring manifest (root or transitive) currently requests
/// 4. The transitive graph's shape (names, declaring chains, declared
///    sources) still matches the snapshot recorded by the last successful
///    resolve — see [`snapshot`] (BT-3009)
fn deps_are_fresh(project_root: &Utf8Path, manifest: &manifest::ParsedManifest) -> bool {
    use beamtalk_core::compilation::DependencySource;

    // Discover the full transitive graph up front: both the lockfile
    // requirement below and the version/reference-match check need to see
    // every git/registry dep in the graph, not just the root manifest's
    // `[dependencies]` — a monorepo's root commonly declares only a path
    // dependency, with the git/registry dep whose version actually matters
    // declared several levels down (BT-2994). If discovery itself fails
    // (e.g. non-UTF-8 path), treat as stale.
    let all_deps = match discover_all_dep_roots(project_root, manifest) {
        Ok(deps) => deps,
        Err(e) => {
            debug!(error = %e, "Failed to discover dep roots — deps are stale");
            return false;
        }
    };

    // Structural check (BT-3009): compare the graph's *shape* — every dep's
    // name, declaring chain, and declared source — against what the last
    // successful resolve recorded. This is the only check that notices a
    // dependency's source type swapping (a git dep becoming a path dep of the
    // same name, or vice versa) in an intermediate manifest: the root mtime
    // guard stays silent, `locked_deps_match` no longer sees the dep as
    // git/registry, and the stale `ebin/` from the old source is still sitting
    // at the same path, so every value-based check below is satisfied.
    if let snapshot::SnapshotStatus::Stale(reason) = snapshot::check(project_root, &all_deps) {
        debug!(%reason, "Dependency graph changed since last resolve — deps are stale");
        return false;
    }

    // Registry deps resolve into git checkouts and are pinned in the lockfile
    // just like git deps, so they carry the same lockfile requirement —
    // whether declared directly by the root or by a path dependency several
    // levels down.
    let has_locked_deps = all_deps.iter().any(|dep| {
        matches!(
            dep.source,
            DependencySource::Git { .. } | DependencySource::Registry { .. }
        )
    });

    // Check lockfile freshness for git/registry deps
    if has_locked_deps {
        let lockfile_path = project_root.join(lockfile::LOCKFILE_NAME);
        if !lockfile_path.exists() {
            debug!("Lockfile missing but git/registry deps present — deps are stale");
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

        // The mtime check above only looks at the root manifest, and even
        // there can miss an edit that lands within the same timestamp
        // granularity — so compare every discovered git/registry dep's
        // declared version or reference against what is actually locked,
        // wherever in the transitive graph it was declared.
        if !locked_deps_match(project_root, &all_deps, manifest) {
            return false;
        }
    }

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

        // ADR 0098 Phase 2: provenance gate. A dependency compiled by a different
        // toolchain (a missing/corrupt/version-mismatched stamp) must be rebuilt,
        // not reused — this is the beamtalk-http stale-`_build` fix, and it also
        // treats pre-stamp deps as stale exactly once on the first build after
        // this ships. mtime cannot detect a toolchain change.
        let stamp_path = layout.dep_stamp_path(&dep.name);
        if let crate::commands::build_stamp::StampStatus::Stale(reason) =
            crate::commands::build_stamp::read_stamp_status(
                &stamp_path,
                crate::commands::build_stamp::current_otp_version(),
            )
        {
            debug!(dep = %dep.name, %reason, "Dependency provenance miss — deps are stale");
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

/// Record the just-resolved dependency graph so the next build's structural
/// freshness check has a baseline to compare against (BT-3009).
///
/// Best-effort by design: discovery or writing failing here must not fail a
/// build that has already resolved and compiled successfully. A missing
/// snapshot simply makes the next `deps_are_fresh` return `false`, which
/// re-resolves and tries again.
fn record_dep_graph_snapshot(project_root: &Utf8Path, manifest: &manifest::ParsedManifest) {
    match discover_all_dep_roots(project_root, manifest) {
        Ok(all_deps) => snapshot::write(project_root, &all_deps),
        Err(e) => {
            debug!(error = %e, "Failed to discover dep roots — dependency-graph snapshot not recorded");
        }
    }
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

/// Shared test fixture helpers for all `deps` sub-module tests.
///
/// Accessible from child modules (`path`, `graph`, …) via
/// `use super::super::test_support::*`.
#[cfg(test)]
pub(super) mod test_support {
    use std::fs;

    /// Write a minimal `beamtalk.toml` into `dir`.
    pub fn write_manifest(dir: &std::path::Path, name: &str, version: &str, deps: &str) {
        let content = format!("[package]\nname = \"{name}\"\nversion = \"{version}\"\n\n{deps}");
        fs::write(dir.join("beamtalk.toml"), content).unwrap();
    }

    /// Create `dir/src/<filename>` with the given content.
    pub fn write_source(dir: &std::path::Path, filename: &str, content: &str) {
        let src_dir = dir.join("src");
        fs::create_dir_all(&src_dir).unwrap();
        fs::write(src_dir.join(filename), content).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::test_support::*;
    use super::*;
    use std::collections::HashMap;
    use std::fs;
    use tempfile::TempDir;

    /// `deps_are_fresh` with the BT-3009 dependency-graph snapshot pre-seeded
    /// from the graph currently on disk — i.e. as if the last successful
    /// resolve had produced exactly this graph.
    ///
    /// Every freshness test other than the structural ones below is about a
    /// *value* changing (an mtime, a locked version, a provenance stamp) while
    /// the graph's shape stays put, so seeding the snapshot keeps those
    /// assertions testing what they name rather than passing vacuously on a
    /// missing snapshot.
    fn deps_are_fresh_with_snapshot(
        root: &camino::Utf8Path,
        parsed: &manifest::ParsedManifest,
    ) -> bool {
        record_dep_graph_snapshot(root, parsed);
        deps_are_fresh(root, parsed)
    }

    fn create_dep_ebin_with_beam(project_root: &std::path::Path, dep_name: &str) {
        let root_utf8 = camino::Utf8PathBuf::from_path_buf(project_root.to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root_utf8);
        let ebin_dir = layout.dep_ebin_dir(dep_name);
        fs::create_dir_all(&ebin_dir).unwrap();
        // Create a fake .beam file
        fs::write(ebin_dir.join(format!("bt@{dep_name}@helper.beam")), b"BEAM").unwrap();
        // ADR 0098 Phase 2: a compiled dep carries a current-toolchain provenance
        // stamp, mirroring a real build, so the freshness check treats it as fresh.
        crate::commands::build_stamp::write_stamp(
            &layout.dep_stamp_path(dep_name),
            crate::commands::build_stamp::current_otp_version(),
        );
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
        write_manifest(temp.path(), "my_app", "0.1.0", "");
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
        write_manifest(&dep_dir, "utils", "0.1.0", "");
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
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Path deps with compiled ebin should be fresh
        assert!(deps_are_fresh_with_snapshot(&root, &parsed));
    }

    #[test]
    fn test_deps_stale_when_dep_stamp_version_mismatch() {
        // ADR 0098 Phase 2: a dep compiled by a different toolchain (here, a
        // forged older `beamtalk_version` in the stamp) must be treated as stale
        // even though its ebin/.beam and mtimes look fine — the beamtalk-http fix.
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "0.1.0", "");
        write_source(
            &dep_dir,
            "helper.bt",
            "Object subclass: Helper\n  greet => \"hi\"\n",
        );

        // Compiled ebin + a current-toolchain stamp → fresh.
        create_dep_ebin_with_beam(temp.path(), "utils");

        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();
        assert!(
            deps_are_fresh_with_snapshot(&root, &parsed),
            "should start fresh"
        );

        // Forge an older-toolchain stamp: same shape, different version.
        let layout = BuildLayout::new(&root);
        let stamp = layout.dep_stamp_path("utils");
        fs::write(
            &stamp,
            r#"{"schema":1,"beamtalk_version":"0.0.0-ancient","otp_release":null,"built_at":"2026-01-01T00:00:00Z"}"#,
        )
        .unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "dep with a mismatched-version stamp should be stale (rebuild, not reuse)"
        );
    }

    #[test]
    fn test_deps_stale_missing_ebin() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep directory
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "0.1.0", "");

        // Create main manifest with path dep — no ebin dir
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        assert!(!deps_are_fresh_with_snapshot(&root, &parsed));
    }

    #[test]
    fn test_deps_stale_empty_ebin() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep directory
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "0.1.0", "");

        // Create ebin dir but no .beam files
        let test_layout = BuildLayout::new(&root);
        let ebin_dir = test_layout.dep_ebin_dir("utils");
        fs::create_dir_all(&ebin_dir).unwrap();

        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        assert!(!deps_are_fresh_with_snapshot(&root, &parsed));
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
            "0.1.0",
            "[dependencies]\njson = { git = \"https://example.com/json\", tag = \"v1.0\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Git deps without lockfile should be stale
        assert!(!deps_are_fresh_with_snapshot(&root, &parsed));
    }

    #[test]
    fn test_deps_stale_path_source_newer_than_ebin() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep directory with manifest
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "0.1.0", "");

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
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Source is newer than beam → stale
        assert!(!deps_are_fresh_with_snapshot(&root, &parsed));
    }

    #[test]
    fn test_collect_fresh_deps_returns_class_index() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create dep with source
        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "0.1.0", "");
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
            "0.1.0",
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
        write_manifest(&shared_dir, "shared", "0.1.0", "");

        // Create utils (depends on shared)
        let utils_dir = temp.path().join("utils");
        fs::create_dir_all(&utils_dir).unwrap();
        write_manifest(
            &utils_dir,
            "utils",
            "0.1.0",
            "[dependencies]\nshared = { path = \"../shared\" }",
        );

        // Create compiled ebin for utils (direct dep) but NOT for shared (transitive)
        create_dep_ebin_with_beam(temp.path(), "utils");

        // Create main manifest
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Should be stale because transitive dep "shared" has no ebin
        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
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
        write_manifest(&shared_dir, "shared", "0.1.0", "");
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
            "0.1.0",
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
            "0.1.0",
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
        write_manifest(&shared_dir, "shared", "0.1.0", "");

        let a_dir = temp.path().join("pkg_a");
        fs::create_dir_all(&a_dir).unwrap();
        write_manifest(
            &a_dir,
            "pkg_a",
            "0.1.0",
            "[dependencies]\nshared = { path = \"../shared\" }",
        );

        let b_dir = temp.path().join("pkg_b");
        fs::create_dir_all(&b_dir).unwrap();
        write_manifest(
            &b_dir,
            "pkg_b",
            "0.1.0",
            "[dependencies]\nshared = { path = \"../shared\" }",
        );

        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
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

        // Create a fake git checkout dir FIRST (older mtime)
        let layout = BuildLayout::new(&root);
        let checkout = layout.dep_checkout_dir("json");
        fs::create_dir_all(&checkout).unwrap();
        fs::write(
            checkout.join("beamtalk.toml"),
            "[package]\nname = \"json\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();

        // Small delay so ebin is strictly newer than the checkout manifest
        std::thread::sleep(std::time::Duration::from_millis(50));

        // Create compiled ebin AFTER checkout (newer mtime)
        create_dep_ebin_with_beam(temp.path(), "json");

        // Create root manifest
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\njson = { git = \"https://example.com/json\", tag = \"v1.0\" }",
        );

        // Create a lockfile newer than the root manifest
        std::thread::sleep(std::time::Duration::from_millis(50));
        fs::write(
            temp.path().join("beamtalk.lock"),
            "# This is auto-generated by beamtalk. Do not edit manually.\n\
             # It pins exact versions of dependencies for reproducible builds.\n\n\
             [[package]]\n\
             name = \"json\"\n\
             url = \"https://example.com/json\"\n\
             reference = \"tag:v1.0\"\n\
             sha = \"abc123\"\n",
        )
        .unwrap();

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        // Git dep with lockfile + compiled ebin should be fresh
        assert!(
            deps_are_fresh_with_snapshot(&root, &parsed),
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
            "# This is auto-generated by beamtalk. Do not edit manually.\n\
             # It pins exact versions of dependencies for reproducible builds.\n\n\
             [[package]]\n\
             name = \"json\"\n\
             url = \"https://example.com/json\"\n\
             reference = \"tag:v1.0\"\n\
             sha = \"abc123\"\n",
        )
        .unwrap();

        std::thread::sleep(std::time::Duration::from_millis(50));

        // Then create manifest (newer than lockfile)
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\njson = { git = \"https://example.com/json\", tag = \"v1.0\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
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
        write_manifest(&leaf_dir, "leaf", "0.1.0", "");

        // Create middle dep (depends on leaf)
        let middle_dir = temp.path().join("middle");
        fs::create_dir_all(&middle_dir).unwrap();
        write_manifest(
            &middle_dir,
            "middle",
            "0.1.0",
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
            "0.1.0",
            "[dependencies]\nleaf = { path = \"../leaf\" }  # changed",
        );

        // Root manifest
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nmiddle = { path = \"middle\" }",
        );

        let manifest_path = root.join("beamtalk.toml");
        let parsed = manifest::parse_manifest_full(&manifest_path).unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "Should detect transitive manifest change as stale"
        );
    }

    #[test]
    fn test_clean_stale_deps_removes_unknown_dirs() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // Create ebin dirs for "utils" (expected) and "old_lib" (stale)
        create_dep_ebin_with_beam(temp.path(), "utils");
        create_dep_ebin_with_beam(temp.path(), "old_lib");

        let resolved = vec![path::ResolvedDependency {
            name: "utils".to_string(),
            root: camino::Utf8PathBuf::from_path_buf(temp.path().join("utils")).unwrap(),
            ebin_path: layout.dep_ebin_dir("utils"),
            class_module_index: HashMap::default(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: None,
        }];

        clean_stale_deps(&root, &resolved).unwrap();

        // "utils" should still exist, "old_lib" should be gone
        assert!(layout.dep_ebin_dir("utils").exists());
        assert!(
            !layout.dep_checkout_dir("old_lib").exists(),
            "Stale dep 'old_lib' should have been removed"
        );
    }

    #[test]
    fn test_clean_stale_deps_preserves_all_expected() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        create_dep_ebin_with_beam(temp.path(), "a");
        create_dep_ebin_with_beam(temp.path(), "b");

        let resolved = vec![
            path::ResolvedDependency {
                name: "a".to_string(),
                root: camino::Utf8PathBuf::from_path_buf(temp.path().join("a")).unwrap(),
                ebin_path: layout.dep_ebin_dir("a"),
                class_module_index: HashMap::default(),
                class_infos: Vec::new(),
                protocol_infos: Vec::new(),
                alias_infos: Vec::new(),
                is_direct: true,
                via_chain: Vec::new(),
                stubs_dir: None,
            },
            path::ResolvedDependency {
                name: "b".to_string(),
                root: camino::Utf8PathBuf::from_path_buf(temp.path().join("b")).unwrap(),
                ebin_path: layout.dep_ebin_dir("b"),
                class_module_index: HashMap::default(),
                class_infos: Vec::new(),
                protocol_infos: Vec::new(),
                alias_infos: Vec::new(),
                is_direct: true,
                via_chain: Vec::new(),
                stubs_dir: None,
            },
        ];

        clean_stale_deps(&root, &resolved).unwrap();

        assert!(layout.dep_ebin_dir("a").exists());
        assert!(layout.dep_ebin_dir("b").exists());
    }

    #[test]
    fn test_clean_stale_deps_no_deps_dir() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // No _build/deps/ directory — should be a no-op
        clean_stale_deps(&root, &[]).unwrap();
    }

    #[test]
    fn test_clean_all_stale_deps_removes_everything() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        create_dep_ebin_with_beam(temp.path(), "leftover");

        clean_all_stale_deps(&root).unwrap();

        assert!(
            !layout.dep_checkout_dir("leftover").exists(),
            "All deps should be removed when no deps are declared"
        );
    }

    // --- Registry dependency freshness (BT-2978) ---

    /// Lay out a project with one compiled registry dep locked at
    /// `locked_version`, whose manifest asks for `manifest_version`.
    fn setup_registry_project(
        temp: &TempDir,
        manifest_version: &str,
        locked_version: &str,
    ) -> camino::Utf8PathBuf {
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let layout = BuildLayout::new(&root);
        let checkout = layout.dep_checkout_dir("yaml");
        fs::create_dir_all(&checkout).unwrap();
        fs::write(
            checkout.join("beamtalk.toml"),
            format!("[package]\nname = \"yaml\"\nversion = \"{locked_version}\"\n"),
        )
        .unwrap();

        std::thread::sleep(std::time::Duration::from_millis(50));
        create_dep_ebin_with_beam(temp.path(), "yaml");

        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            &format!("[dependencies]\nyaml = \"{manifest_version}\"\n"),
        );

        // Lockfile must be newer than the manifest so only the version
        // comparison can make the deps stale. The project declares no
        // `[registry] url`, so the currently configured registry is always
        // `registry::DEFAULT_REGISTRY_URL` — record that as the lock entry's
        // registry so these tests exercise the version comparison alone,
        // not BT-2993's registry comparison (covered separately below).
        std::thread::sleep(std::time::Duration::from_millis(50));
        fs::write(
            temp.path().join("beamtalk.lock"),
            format!(
                "# This file is auto-generated by beamtalk. Do not edit manually.\n\n\
                 [[package]]\n\
                 name = \"yaml\"\n\
                 version = \"{locked_version}\"\n\
                 registry = \"{}\"\n\
                 url = \"https://example.test/yaml\"\n\
                 reference = \"tag:v{locked_version}\"\n\
                 sha = \"abc123\"\n",
                registry::DEFAULT_REGISTRY_URL
            ),
        )
        .unwrap();

        root
    }

    #[test]
    fn test_registry_dep_fresh_when_locked_version_matches() {
        let temp = TempDir::new().unwrap();
        let root = setup_registry_project(&temp, "0.2.1", "0.2.1");
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            deps_are_fresh_with_snapshot(&root, &parsed),
            "Registry dep locked at the requested version should be fresh"
        );
    }

    #[test]
    fn test_registry_dep_stale_when_manifest_version_differs() {
        let temp = TempDir::new().unwrap();
        // Manifest asks for 0.3.0 but the lockfile pins 0.2.1.
        let root = setup_registry_project(&temp, "0.3.0", "0.2.1");
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "A version bump in beamtalk.toml must force re-resolution"
        );
    }

    #[test]
    fn test_registry_dep_stale_without_lockfile() {
        let temp = TempDir::new().unwrap();
        let root = setup_registry_project(&temp, "0.2.1", "0.2.1");
        fs::remove_file(temp.path().join("beamtalk.lock")).unwrap();
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "Registry deps require a lockfile, like git deps"
        );
    }

    #[test]
    fn test_registry_dep_stale_when_lock_entry_has_no_version() {
        let temp = TempDir::new().unwrap();
        let root = setup_registry_project(&temp, "0.2.1", "0.2.1");

        // A pre-registry lock entry (no `version`) can't prove the pin matches.
        fs::write(
            temp.path().join("beamtalk.lock"),
            "# This file is auto-generated by beamtalk. Do not edit manually.\n\n\
             [[package]]\n\
             name = \"yaml\"\n\
             url = \"https://example.test/yaml\"\n\
             reference = \"tag:v0.2.1\"\n\
             sha = \"abc123\"\n",
        )
        .unwrap();

        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();
        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "An unversioned lock entry should force re-resolution"
        );
    }

    /// A registry configured via a *relative* `[registry] url` must stay
    /// fresh regardless of the project's absolute checkout path. Recording
    /// the resolved (and therefore checkout-specific) absolute path instead
    /// of the raw configured value would make a project using a vendored,
    /// relatively-pathed registry spuriously stale every time it's built
    /// from a different absolute path — e.g. a dev machine vs. CI (BT-2993).
    #[test]
    fn test_registry_dep_fresh_with_relative_local_registry_path() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // A vendored registry index checked into the project itself.
        fs::create_dir_all(root.join("vendor/registry/packages")).unwrap();

        let layout = BuildLayout::new(&root);
        let checkout = layout.dep_checkout_dir("yaml");
        fs::create_dir_all(&checkout).unwrap();
        fs::write(
            checkout.join("beamtalk.toml"),
            "[package]\nname = \"yaml\"\nversion = \"0.2.1\"\n",
        )
        .unwrap();

        std::thread::sleep(std::time::Duration::from_millis(50));
        create_dep_ebin_with_beam(temp.path(), "yaml");

        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[registry]\nurl = \"vendor/registry\"\n\n[dependencies]\nyaml = \"0.2.1\"\n",
        );

        // The lock records the raw configured value ("vendor/registry"), not
        // the absolute path it resolves to under this particular temp dir.
        std::thread::sleep(std::time::Duration::from_millis(50));
        fs::write(
            temp.path().join("beamtalk.lock"),
            "# This file is auto-generated by beamtalk. Do not edit manually.\n\n\
             [[package]]\n\
             name = \"yaml\"\n\
             version = \"0.2.1\"\n\
             registry = \"vendor/registry\"\n\
             url = \"https://example.test/yaml\"\n\
             reference = \"tag:v0.2.1\"\n\
             sha = \"abc123\"\n",
        )
        .unwrap();

        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();
        assert!(
            deps_are_fresh_with_snapshot(&root, &parsed),
            "a relative [registry] url must stay fresh regardless of the \
             project's absolute checkout path"
        );
    }

    /// A lock entry pinned by a different registry than the one currently
    /// configured must not be trusted, even when its recorded version still
    /// matches the manifest (BT-2993).
    #[test]
    fn test_registry_dep_stale_when_registry_differs() {
        let temp = TempDir::new().unwrap();
        let root = setup_registry_project(&temp, "0.2.1", "0.2.1");

        // The project declares no `[registry] url`, so the default registry
        // applies — which is not `https://old-registry.example/registry`.
        std::thread::sleep(std::time::Duration::from_millis(50));
        fs::write(
            temp.path().join("beamtalk.lock"),
            "# This file is auto-generated by beamtalk. Do not edit manually.\n\n\
             [[package]]\n\
             name = \"yaml\"\n\
             version = \"0.2.1\"\n\
             registry = \"https://old-registry.example/registry\"\n\
             url = \"https://example.test/yaml\"\n\
             reference = \"tag:v0.2.1\"\n\
             sha = \"abc123\"\n",
        )
        .unwrap();

        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();
        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "A lock entry from a different registry must force re-resolution"
        );
    }

    #[test]
    fn test_discover_all_dep_roots_registry_uses_checkout_dir() {
        let temp = TempDir::new().unwrap();
        let root = setup_registry_project(&temp, "0.2.1", "0.2.1");
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        let deps = discover_all_dep_roots(&root, &parsed).unwrap();
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].name, "yaml");
        assert_eq!(
            deps[0].root,
            BuildLayout::new(&root).dep_checkout_dir("yaml")
        );
        assert!(!deps[0].is_path_dep);
        assert!(deps[0].is_direct);
    }

    // --- Transitive freshness checks (BT-2994) ---
    //
    // `has_locked_deps` and the version/reference-vs-lockfile comparison must
    // account for git/registry deps declared by a *transitive* dependency,
    // not just the root manifest's `[dependencies]` — the standard monorepo
    // layout has the root declare only a path dependency, with the
    // git/registry dependency whose version actually matters declared
    // several levels down. Each helper below writes its dep's manifest,
    // then sleeps and writes that dep's compiled ebin, so its own
    // `manifest_newer_than_ebin` check never fires — isolating these tests
    // to the new transitive-lockfile comparison rather than the pre-existing
    // per-dep mtime fallback.

    /// Lay out `root -> utils (path) -> yaml (registry)`, with `utils`'s
    /// manifest requesting `manifest_version` of `yaml` and the lockfile
    /// pinning `locked_version`. `root` declares no dependency of its own
    /// other than the `utils` path dep.
    fn setup_transitive_registry_project(
        temp: &TempDir,
        manifest_version: &str,
        locked_version: &str,
    ) -> camino::Utf8PathBuf {
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // yaml: registry dep, checked out and compiled at `locked_version`.
        let checkout = layout.dep_checkout_dir("yaml");
        fs::create_dir_all(&checkout).unwrap();
        fs::write(
            checkout.join("beamtalk.toml"),
            format!("[package]\nname = \"yaml\"\nversion = \"{locked_version}\"\n"),
        )
        .unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
        create_dep_ebin_with_beam(temp.path(), "yaml");

        // utils: path dep declaring yaml, itself compiled fresh afterwards.
        let utils_dir = temp.path().join("utils");
        fs::create_dir_all(&utils_dir).unwrap();
        write_manifest(
            &utils_dir,
            "utils",
            "0.1.0",
            &format!("[dependencies]\nyaml = \"{manifest_version}\"\n"),
        );
        std::thread::sleep(std::time::Duration::from_millis(50));
        create_dep_ebin_with_beam(temp.path(), "utils");

        // root: only a path dep — no direct git/registry dep at all.
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        // Lockfile, newer than everything above, pinning yaml at
        // `locked_version` from the default registry.
        std::thread::sleep(std::time::Duration::from_millis(50));
        fs::write(
            temp.path().join("beamtalk.lock"),
            format!(
                "# This file is auto-generated by beamtalk. Do not edit manually.\n\n\
                 [[package]]\n\
                 name = \"yaml\"\n\
                 version = \"{locked_version}\"\n\
                 registry = \"{}\"\n\
                 url = \"https://example.test/yaml\"\n\
                 reference = \"tag:v{locked_version}\"\n\
                 sha = \"abc123\"\n",
                registry::DEFAULT_REGISTRY_URL
            ),
        )
        .unwrap();

        root
    }

    #[test]
    fn test_transitive_registry_dep_fresh_when_locked_version_matches() {
        let temp = TempDir::new().unwrap();
        let root = setup_transitive_registry_project(&temp, "0.2.1", "0.2.1");
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            deps_are_fresh_with_snapshot(&root, &parsed),
            "A transitive registry dep locked at the requested version should be fresh"
        );
    }

    #[test]
    fn test_transitive_registry_dep_requires_lockfile() {
        // The root manifest itself declares only a path dep, so
        // `has_locked_deps` must still come out true because `utils`
        // transitively declares a registry dep.
        let temp = TempDir::new().unwrap();
        let root = setup_transitive_registry_project(&temp, "0.2.1", "0.2.1");
        fs::remove_file(temp.path().join("beamtalk.lock")).unwrap();
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "A registry dep declared by a transitive path dependency must still require a lockfile"
        );
    }

    #[test]
    fn test_transitive_registry_dep_stale_when_manifest_version_bumped() {
        // utils/beamtalk.toml asks for 0.3.0 but the lockfile pins 0.2.1 —
        // this is the exact scenario from BT-2994: the root declares no
        // registry dependency of its own at all.
        let temp = TempDir::new().unwrap();
        let root = setup_transitive_registry_project(&temp, "0.3.0", "0.2.1");
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "A version bump in a transitive dependency's manifest must force re-resolution"
        );
    }

    /// Lay out `root -> middle (path) -> leaf (git)`, with `middle`'s
    /// manifest requesting `manifest_tag` for `leaf` and the lockfile
    /// pinning `locked_tag`. `root` declares no dependency of its own other
    /// than the `middle` path dep.
    fn setup_transitive_git_project(
        temp: &TempDir,
        manifest_tag: &str,
        locked_tag: &str,
    ) -> camino::Utf8PathBuf {
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // leaf: git dep, checked out and compiled.
        let checkout = layout.dep_checkout_dir("leaf");
        fs::create_dir_all(&checkout).unwrap();
        fs::write(
            checkout.join("beamtalk.toml"),
            "[package]\nname = \"leaf\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
        create_dep_ebin_with_beam(temp.path(), "leaf");

        // middle: path dep declaring leaf as a git dependency, itself
        // compiled fresh afterwards.
        let middle_dir = temp.path().join("middle");
        fs::create_dir_all(&middle_dir).unwrap();
        write_manifest(
            &middle_dir,
            "middle",
            "0.1.0",
            &format!(
                "[dependencies]\nleaf = {{ git = \"https://example.com/leaf\", tag = \"{manifest_tag}\" }}\n"
            ),
        );
        std::thread::sleep(std::time::Duration::from_millis(50));
        create_dep_ebin_with_beam(temp.path(), "middle");

        // root: only a path dep — no direct git/registry dep at all.
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nmiddle = { path = \"middle\" }",
        );

        // Lockfile, newer than everything above, pinning leaf at `locked_tag`.
        std::thread::sleep(std::time::Duration::from_millis(50));
        fs::write(
            temp.path().join("beamtalk.lock"),
            format!(
                "# This file is auto-generated by beamtalk. Do not edit manually.\n\n\
                 [[package]]\n\
                 name = \"leaf\"\n\
                 url = \"https://example.com/leaf\"\n\
                 reference = \"tag:{locked_tag}\"\n\
                 sha = \"abc123\"\n"
            ),
        )
        .unwrap();

        root
    }

    #[test]
    fn test_transitive_git_dep_fresh_when_locked_tag_matches() {
        let temp = TempDir::new().unwrap();
        let root = setup_transitive_git_project(&temp, "v1.0", "v1.0");
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            deps_are_fresh_with_snapshot(&root, &parsed),
            "A transitive git dep locked at the requested tag should be fresh"
        );
    }

    #[test]
    fn test_transitive_git_dep_requires_lockfile() {
        // The root manifest itself declares only a path dep, so
        // `has_locked_deps` must still come out true because `middle`
        // transitively declares a git dep.
        let temp = TempDir::new().unwrap();
        let root = setup_transitive_git_project(&temp, "v1.0", "v1.0");
        fs::remove_file(temp.path().join("beamtalk.lock")).unwrap();
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "A git dep declared by a transitive path dependency must still require a lockfile"
        );
    }

    #[test]
    fn test_transitive_git_dep_stale_when_tag_bumped() {
        // middle/beamtalk.toml asks for tag v2.0 but the lockfile pins v1.0 —
        // the git-dep analogue of BT-2994: the root declares no git
        // dependency of its own at all.
        let temp = TempDir::new().unwrap();
        let root = setup_transitive_git_project(&temp, "v2.0", "v1.0");
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            !deps_are_fresh_with_snapshot(&root, &parsed),
            "A tag bump in a transitive git dependency's manifest must force re-resolution"
        );
    }

    // --- Structural freshness checks (BT-3009) ---
    //
    // Every check above compares a *value* — an mtime, a locked version, a
    // provenance stamp. None of them notices a dependency's declared *source
    // type* changing while its name stays put in an intermediate manifest.
    // These tests cover that gap, and each one first proves the gap is real by
    // asserting the swapped graph looks fresh to every other check once the
    // snapshot is re-recorded to match it.

    /// Rewrite a compiled dependency's ebin so its `.beam` files are newer than
    /// any manifest the test has just edited — neutralising the
    /// `manifest_newer_than_ebin` mtime fallback so the assertion isolates the
    /// structural check.
    fn recompile_dep(temp: &TempDir, dep_name: &str) {
        std::thread::sleep(std::time::Duration::from_millis(50));
        create_dep_ebin_with_beam(temp.path(), dep_name);
    }

    #[test]
    fn test_intermediate_git_to_path_swap_is_stale() {
        // root -> middle (path) -> leaf, where middle's manifest swaps leaf
        // from a git dep to a path dep. The root manifest is never touched.
        let temp = TempDir::new().unwrap();
        let root = setup_transitive_git_project(&temp, "v1.0", "v1.0");
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        // Baseline: the graph as it stands is what the last resolve produced.
        record_dep_graph_snapshot(&root, &parsed);
        assert!(deps_are_fresh(&root, &parsed), "should start fresh");

        // The swap: leaf becomes a local path dependency of middle. Its stale
        // `_build/deps/leaf/ebin/` from the git checkout stays exactly where
        // it was, and `beamtalk.lock` still pins the old git revision.
        let leaf_src = temp.path().join("leaf");
        fs::create_dir_all(&leaf_src).unwrap();
        write_manifest(&leaf_src, "leaf", "1.0.0", "");
        write_manifest(
            &temp.path().join("middle"),
            "middle",
            "0.1.0",
            "[dependencies]\nleaf = { path = \"../leaf\" }\n",
        );
        recompile_dep(&temp, "middle");
        recompile_dep(&temp, "leaf");

        assert!(
            !deps_are_fresh(&root, &parsed),
            "A git-to-path swap in an intermediate manifest must be detected as stale"
        );

        // The gap this closes: with the snapshot re-recorded to match the
        // swapped graph, every other freshness check is satisfied — nothing
        // else in `deps_are_fresh` can see the swap at all.
        assert!(
            deps_are_fresh_with_snapshot(&root, &parsed),
            "no non-structural check should be able to detect the swap"
        );
    }

    #[test]
    fn test_intermediate_path_to_git_swap_is_stale() {
        // The reverse direction: middle declares leaf as a path dep, then
        // swaps it to a git dep at the same name. There is no lockfile at all
        // in the starting state, because the graph is pure-path.
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let leaf_src = temp.path().join("leaf");
        fs::create_dir_all(&leaf_src).unwrap();
        write_manifest(&leaf_src, "leaf", "1.0.0", "");
        create_dep_ebin_with_beam(temp.path(), "leaf");

        let middle_dir = temp.path().join("middle");
        fs::create_dir_all(&middle_dir).unwrap();
        write_manifest(
            &middle_dir,
            "middle",
            "0.1.0",
            "[dependencies]\nleaf = { path = \"../leaf\" }\n",
        );
        recompile_dep(&temp, "middle");

        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nmiddle = { path = \"middle\" }",
        );

        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();
        record_dep_graph_snapshot(&root, &parsed);
        assert!(deps_are_fresh(&root, &parsed), "should start fresh");

        // The swap. A git dep with no lock entry is already caught by
        // `locked_deps_match`, so this asserts the structural check agrees
        // rather than that it is the only thing catching it.
        write_manifest(
            &middle_dir,
            "middle",
            "0.1.0",
            "[dependencies]\nleaf = { git = \"https://example.com/leaf\", tag = \"v1.0\" }\n",
        );
        recompile_dep(&temp, "middle");

        assert!(
            !deps_are_fresh(&root, &parsed),
            "A path-to-git swap in an intermediate manifest must be detected as stale"
        );
    }

    #[test]
    fn test_intermediate_path_dep_moved_is_stale() {
        // A path dependency's declared *path* moving is the same class of
        // structural change: same name, different source, artifacts left
        // behind at `_build/deps/leaf/ebin/`.
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        for dir in ["leaf", "leaf_v2"] {
            let src = temp.path().join(dir);
            fs::create_dir_all(&src).unwrap();
            write_manifest(&src, "leaf", "1.0.0", "");
        }
        create_dep_ebin_with_beam(temp.path(), "leaf");

        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nleaf = { path = \"leaf\" }",
        );
        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();
        record_dep_graph_snapshot(&root, &parsed);
        assert!(deps_are_fresh(&root, &parsed), "should start fresh");

        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nleaf = { path = \"leaf_v2\" }",
        );
        let moved = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();

        assert!(
            !deps_are_fresh(&root, &moved),
            "A path dependency pointed at a different directory must be stale"
        );
    }

    #[test]
    fn test_deps_stale_when_no_graph_snapshot_recorded() {
        // "Fail toward rebuild", matching ADR 0098's stance on a missing
        // provenance stamp: with no record of what the last resolve produced,
        // there is nothing to compare the current graph against.
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "0.1.0", "");
        create_dep_ebin_with_beam(temp.path(), "utils");
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();
        assert!(
            !deps_are_fresh(&root, &parsed),
            "An otherwise-fresh project with no graph snapshot must re-resolve once"
        );

        record_dep_graph_snapshot(&root, &parsed);
        assert!(
            deps_are_fresh(&root, &parsed),
            "and be fresh once the snapshot has been recorded"
        );
    }

    #[test]
    fn test_deps_stale_when_graph_snapshot_corrupt() {
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "0.1.0", "");
        create_dep_ebin_with_beam(temp.path(), "utils");
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();
        assert!(
            deps_are_fresh_with_snapshot(&root, &parsed),
            "should start fresh"
        );

        fs::write(BuildLayout::new(&root).dep_graph_path(), "{not json").unwrap();
        assert!(
            !deps_are_fresh(&root, &parsed),
            "A corrupt graph snapshot must force re-resolution"
        );
    }

    #[test]
    fn test_ensure_deps_resolved_records_a_snapshot_that_reads_back_fresh() {
        // The round trip that matters for build times: the snapshot a real
        // resolve records must satisfy the *next* freshness check. If the
        // graph recorded after resolution ever disagreed with the graph
        // discovered before it, every build would re-resolve from scratch.
        let temp = TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let dep_dir = temp.path().join("utils");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "utils", "0.1.0", "");
        write_source(
            &dep_dir,
            "helper.bt",
            "Object subclass: Helper\n  greet => \"hi\"\n",
        );
        write_manifest(
            temp.path(),
            "my_app",
            "0.1.0",
            "[dependencies]\nutils = { path = \"utils\" }",
        );

        let options = beamtalk_core::CompilerOptions::default();
        let resolved = ensure_deps_resolved(&root, &options).unwrap();
        assert_eq!(resolved.len(), 1, "utils should have resolved");

        assert!(
            BuildLayout::new(&root).dep_graph_path().exists(),
            "a successful resolve must record the dependency-graph snapshot"
        );

        let parsed = manifest::parse_manifest_full(&root.join("beamtalk.toml")).unwrap();
        assert!(
            deps_are_fresh(&root, &parsed),
            "the recorded snapshot must read back as fresh — otherwise every \
             build re-resolves"
        );
    }
}
