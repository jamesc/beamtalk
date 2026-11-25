// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Initialize-params configuration: workspace roots, stdlib source
//! directories, and the startup source-file preload collection.

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use beamtalk_core::build_layout::BuildLayout;
use beamtalk_package::dependency_classes::DependencyCheckout;
use tower_lsp::lsp_types::InitializeParams;

/// Session-wide cap on preloaded source files: the startup preload and every
/// per-root load `did_open` triggers (BT-4158) together draw from one budget
/// of this size (BT-4383), tracked on `Backend::preload_budget_remaining`.
pub(in crate::server) const PRELOAD_MAX_FILES: usize = 5000;
#[derive(Clone)]
pub(in crate::server) struct PreloadConfig {
    pub(in crate::server) roots: Vec<PathBuf>,
    pub(in crate::server) stdlib_dirs: Vec<PathBuf>,
}
#[derive(Default)]
pub(in crate::server) struct PreloadedFiles {
    pub(in crate::server) user_files: Vec<(PathBuf, String)>,
    pub(in crate::server) stdlib_files: Vec<(PathBuf, String)>,
    /// Whether the preload file budget (`PRELOAD_MAX_FILES`) was exhausted
    /// mid-walk. When true, workspace coverage may be partial and the
    /// language service must NOT claim `KnowledgeScope::ProjectComplete`.
    pub(in crate::server) budget_exhausted: bool,
    /// How much of the budget this walk drew (paths found, before reading),
    /// to subtract from the session-wide remainder (BT-4383).
    pub(in crate::server) budget_used: usize,
}
pub(in crate::server) fn workspace_roots(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::new();

    if let Some(workspace_folders) = &params.workspace_folders {
        for folder in workspace_folders {
            if let Ok(path) = folder.uri.to_file_path() {
                roots.push(path);
            }
        }
    }

    if let Some(root_uri) = &params.root_uri
        && let Ok(path) = root_uri.to_file_path()
    {
        roots.push(path);
    }

    roots = roots
        .into_iter()
        .map(|root| beamtalk_project::discover_project_root(&root))
        .collect();

    roots.sort_unstable();
    roots.dedup();
    roots
}
/// The package root owning `path`: the ancestor
/// [`beamtalk_project::discover_project_root`] stops at, provided it actually
/// holds a `beamtalk.toml`.
///
/// [`workspace_roots`] resolves each workspace folder once at `initialize`,
/// but an editor may open a *parent* of one or more packages (e.g. `~/source`
/// holding several checkouts) whose own discovered root has no manifest.
/// Resolving the root per open file lets `did_open` register the file's real
/// package so its `[package]` name, sources and fetched dependencies load
/// (BT-4158). A root found only via a `.git` or `.beamtalk` marker is not a
/// package and yields `None`, as does a path with no on-disk file to start
/// from.
///
/// A fetched dependency checkout (`<pkg>/_build/deps/<name>/`) carries its own
/// manifest, but it is part of the package that fetched it — [`preload_covers`]
/// and [`collect_preload_files`] reach it through that package — so a file in
/// one resolves to the owning `<pkg>` instead.
pub(in crate::server) fn package_root_for_file(path: &Path) -> Option<PathBuf> {
    let root = beamtalk_project::discover_project_root(path);
    if let Some(owner) = beamtalk_core::build_layout::project_root_of_dep_checkout(&root) {
        return package_root_for_file(owner);
    }
    root.join("beamtalk.toml").is_file().then_some(root)
}
pub(in crate::server) fn configured_stdlib_source_dir(params: &InitializeParams) -> Option<String> {
    params
        .initialization_options
        .as_ref()
        .and_then(|value| value.get("stdlibSourceDir"))
        .and_then(|value| value.as_str())
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(ToString::to_string)
}
/// Read the `delegateToRuntime` flag from `initializationOptions`.
///
/// Defaults to `false`: foundation issue keeps all navigation on the
/// AST walker so behaviour is byte-for-byte identical to today. The
/// per-method children flip individual queries over and
/// rely on the editor / user enabling this flag once the runtime path is
/// stable.
pub(in crate::server) fn configured_delegate_to_runtime(params: &InitializeParams) -> bool {
    params
        .initialization_options
        .as_ref()
        .and_then(|value| value.get("delegateToRuntime"))
        .and_then(serde_json::Value::as_bool)
        .unwrap_or(false)
}
/// Returns the stdlib source directory auto-discovered from the LSP binary's sysroot.
///
/// Derives the sysroot via the shared `beamtalk_env` leaf crate — the
/// same convention used by `beamtalk --print-sysroot` — then looks for
/// `share/beamtalk/stdlib/src/` under that prefix.
pub(in crate::server) fn sysroot_stdlib_source_dir() -> Option<PathBuf> {
    let sysroot = beamtalk_env::current_sysroot()?;
    let candidate = sysroot.join("share/beamtalk/stdlib/src");
    canonicalize_existing_dir(&candidate)
}
pub(in crate::server) fn configured_stdlib_source_dirs(
    configured: Option<&str>,
    project_roots: &[PathBuf],
) -> Vec<PathBuf> {
    let Some(configured) = configured else {
        // No explicit config — fall back to sysroot auto-discovery.
        return sysroot_stdlib_source_dir().into_iter().collect();
    };

    let configured_path = PathBuf::from(configured);
    let canonical_roots: Vec<PathBuf> = project_roots
        .iter()
        .filter_map(|root| canonicalize_existing_dir(root))
        .collect();
    let mut dirs = Vec::new();

    if configured_path.is_absolute() {
        if let Some(candidate) = canonicalize_existing_dir(&configured_path) {
            dirs.push(candidate);
        }
    } else {
        for root in project_roots {
            let candidate = root.join(&configured_path);
            if let Some(canonical_candidate) = canonicalize_existing_dir(&candidate)
                && path_within_any_root(&canonical_candidate, &canonical_roots)
            {
                dirs.push(canonical_candidate);
            }
        }
    }

    dirs.sort_unstable();
    dirs.dedup();
    dirs
}
pub(in crate::server) fn canonicalize_existing_dir(path: &Path) -> Option<PathBuf> {
    if !path.is_dir() {
        return None;
    }
    beamtalk_core::paths::canonicalize(path).ok()
}
pub(in crate::server) fn path_within_any_root(path: &Path, roots: &[PathBuf]) -> bool {
    roots.iter().any(|root| path.starts_with(root))
}
/// The declared dependencies of the package at `root` (BT-4611): its
/// `beamtalk.toml` `[dependencies]` and theirs, transitively, located on disk
/// by [`beamtalk_package::dependency_classes::dependency_layout`] (BT-4777) —
/// the walk `beamtalk lint` and MCP share: path dependencies at their
/// declared path, git and registry ones under `_build/deps/<name>/`. Never
/// fetches.
///
/// A root with no `beamtalk.toml` is not a package and has none. One whose
/// manifest can't be read has none known; its fetched checkouts
/// (`_build/deps/*/`) are still returned so their classes resolve.
pub(in crate::server) fn root_dependencies(root: &Path) -> Vec<DependencyCheckout> {
    let Some(utf8_root) = camino::Utf8Path::from_path(root) else {
        // A non-UTF-8 root can't be walked: if it is a package, fall back to
        // whatever has been fetched.
        return if root.join("beamtalk.toml").is_file() {
            fetched_dependency_checkouts(root)
        } else {
            Vec::new()
        };
    };
    let layout = beamtalk_package::dependency_classes::dependency_layout(utf8_root);
    if layout.manifest_unreadable {
        // `debug`, not `warn`: this runs on every `did_open` (via
        // `preload_covers`), and `beamtalk build` reports the error.
        tracing::debug!(root = %root.display(), "Cannot read the manifest; using fetched dependency checkouts only");
        return fetched_dependency_checkouts(root);
    }
    layout.found
}
/// Every fetched checkout under `<root>/_build/deps/` with a `src/` — the
/// fallback when the manifest's `[dependencies]` can't be read. Empty for a
/// non-UTF-8 root, which the layout walk can't represent either.
fn fetched_dependency_checkouts(root: &Path) -> Vec<DependencyCheckout> {
    let Some(utf8_root) = camino::Utf8Path::from_path(root) else {
        return Vec::new();
    };
    let layout = BuildLayout::new(utf8_root);
    let Ok(entries) = layout.deps_dir().read_dir_utf8() else {
        return Vec::new();
    };
    let mut out: Vec<DependencyCheckout> = entries
        .flatten()
        .filter(|entry| entry.path().join("src").is_dir())
        .map(|entry| DependencyCheckout {
            name: entry.file_name().to_string(),
            src: entry.path().join("src"),
            root: entry.path().to_path_buf(),
            dependencies: Vec::new(),
            stubs_dir: None,
        })
        .collect();
    out.sort_unstable_by(|a, b| a.root.cmp(&b.root));
    out
}
/// The source directories of every dependency of `root` found on disk
/// ([`root_dependencies`]).
pub(in crate::server) fn dependency_src_dirs(root: &Path) -> Vec<PathBuf> {
    root_dependencies(root)
        .into_iter()
        .map(|dep| dep.src.into_std_path_buf())
        .collect()
}
/// Whether `path` lies in a directory [`collect_preload_files`] walks for
/// some workspace root — `src/`, `test/`, or a dependency's source directory
/// (path dependencies included).
/// Must stay in lockstep with that walk: it decides which closed files keep
/// their on-disk index entry in `did_close`.
pub(in crate::server) fn preload_covers(path: &Path, roots: &[PathBuf]) -> bool {
    roots.iter().any(|root| {
        path.starts_with(root.join("src"))
            || path.starts_with(root.join("test"))
            || dependency_src_dirs(root)
                .iter()
                .any(|dep_src| path.starts_with(dep_src))
    })
}
/// [`collect_preload_files_within`] with a whole fresh [`PRELOAD_MAX_FILES`]
/// budget — the walk's behaviour in isolation, for tests.
#[cfg(test)]
pub(in crate::server) fn collect_preload_files(config: PreloadConfig) -> PreloadedFiles {
    collect_preload_files_within(config, PRELOAD_MAX_FILES)
}
/// [`collect_preload_files`] with an explicit file budget — the share of the
/// session-wide [`PRELOAD_MAX_FILES`] budget still unspent (BT-4383). A zero
/// budget walks nothing and reports `budget_exhausted`.
pub(in crate::server) fn collect_preload_files_within(
    config: PreloadConfig,
    budget: usize,
) -> PreloadedFiles {
    use beamtalk_core::file_walker::FileWalker;

    let PreloadConfig { roots, stdlib_dirs } = config;
    let mut user_paths = Vec::new();
    let mut remaining_budget = budget;

    let preload_walker = FileWalker::preload_files(remaining_budget);

    // Preload both `src/` and `test/` so that opening a file in
    // `test/` immediately sees classes defined in `src/` (and vice versa).
    // Before this, the LSP would report spurious `Unresolved class` for every
    // test-to-src reference until the user touched the src file manually.
    //
    // Also preload each declared dependency's source ([`root_dependencies`]:
    // `_build/deps/<name>/src/` for a fetched one, the declared path for a
    // path dependency) so references to dependency classes (e.g.
    // `HTTPClient` from the `http` package) resolve without spurious
    // `Unresolved class` warnings. Dep dirs are walked
    // *after* every workspace root's `src/`/`test/` so that in a multi-root
    // workspace one root's deps cannot exhaust the shared preload budget
    // before later roots' user files are considered.
    for root in &roots {
        for subdir in ["src", "test"] {
            if remaining_budget == 0 {
                break;
            }
            let dir = root.join(subdir);
            if dir.is_dir()
                && let Ok(found) = preload_walker
                    .clone()
                    .max_files(remaining_budget)
                    .walk_pathbuf(&dir)
            {
                remaining_budget = remaining_budget.saturating_sub(found.len());
                user_paths.extend(found);
            }
        }
    }

    // Every declared dependency's source, path dependencies included
    // (BT-4611), so their classes are indexed.
    walk_dependency_sources(
        &roots,
        &preload_walker,
        &mut remaining_budget,
        &mut user_paths,
    );

    let mut stdlib_path_list = Vec::new();
    for dir in &stdlib_dirs {
        if remaining_budget == 0 {
            break;
        }
        if let Ok(found) = preload_walker
            .clone()
            .max_files(remaining_budget)
            .walk_pathbuf(dir)
        {
            remaining_budget = remaining_budget.saturating_sub(found.len());
            stdlib_path_list.extend(found);
        }
    }

    // Build the stdlib set first so user files that overlap with stdlib are
    // classified as stdlib (preserving the beamtalk-stdlib:// URI route).
    let stdlib_path_set: HashSet<PathBuf> = stdlib_path_list.iter().cloned().collect();

    let mut seen_user_files = HashSet::new();
    let mut user_files = Vec::new();
    for path in user_paths {
        // Skip paths that appear in stdlib: they must stay in the stdlib bucket.
        if stdlib_path_set.contains(&path) || !seen_user_files.insert(path.clone()) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        user_files.push((path, content));
    }

    let mut seen_stdlib_files = HashSet::new();
    let mut stdlib_files = Vec::new();
    for path in stdlib_path_list {
        if !seen_stdlib_files.insert(path.clone()) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        stdlib_files.push((path, content));
    }

    PreloadedFiles {
        user_files,
        stdlib_files,
        budget_exhausted: remaining_budget == 0,
        budget_used: budget - remaining_budget,
    }
}
/// The dependency half of [`collect_preload_files_within`]: walks every
/// root's declared dependencies' source directories within
/// `remaining_budget`, appending their files to `user_paths`.
fn walk_dependency_sources(
    roots: &[PathBuf],
    preload_walker: &beamtalk_core::file_walker::FileWalker,
    remaining_budget: &mut usize,
    user_paths: &mut Vec<PathBuf>,
) {
    for root in roots {
        for dep in root_dependencies(root) {
            if *remaining_budget == 0 {
                break;
            }
            if let Ok(found) = preload_walker
                .clone()
                .max_files(*remaining_budget)
                .walk_pathbuf(dep.src.as_std_path())
            {
                *remaining_budget = remaining_budget.saturating_sub(found.len());
                // `stubs/` holds FFI stubs, not source (as in the CLI's
                // dependency scan); it is only reachable when the
                // dependency has no `src/`.
                let stubs = dep.root.join("stubs").into_std_path_buf();
                user_paths.extend(found.into_iter().filter(|path| !path.starts_with(&stubs)));
            }
        }
    }
}
