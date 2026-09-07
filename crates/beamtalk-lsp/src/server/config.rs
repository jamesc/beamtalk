// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Initialize-params configuration: workspace roots, stdlib source
//! directories, and the startup source-file preload collection.

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use tower_lsp::lsp_types::InitializeParams;

const PRELOAD_MAX_FILES: usize = 5000;
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
    /// Whether any workspace root has fetched package dependencies
    /// (`_build/deps/*/src` present). Dependency extension contributions
    /// are invisible until fetched, so diagnostics must stay conservative.
    pub(in crate::server) deps_present: bool,
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

    if let Some(root_uri) = &params.root_uri {
        if let Ok(path) = root_uri.to_file_path() {
            roots.push(path);
        }
    }

    roots = roots
        .into_iter()
        .map(|root| beamtalk_project::discover_project_root(&root))
        .collect();

    roots.sort_unstable();
    roots.dedup();
    roots
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
/// Derives the sysroot via the shared `beamtalk_sysroot` leaf crate — the
/// same convention used by `beamtalk --print-sysroot` — then looks for
/// `share/beamtalk/stdlib/src/` under that prefix.
pub(in crate::server) fn sysroot_stdlib_source_dir() -> Option<PathBuf> {
    let sysroot = beamtalk_sysroot::current_sysroot()?;
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
    fs::canonicalize(path).ok()
}
pub(in crate::server) fn path_within_any_root(path: &Path, roots: &[PathBuf]) -> bool {
    roots.iter().any(|root| path.starts_with(root))
}
/// Enumerate `_build/deps/<name>/src/` directories for every fetched
/// dependency under `root`.
///
/// Filesystem-driven rather than manifest-driven: any directory under
/// `_build/deps/` with a `src/` subdirectory is treated as a dep. This avoids
/// adding a `beamtalk-cli` dependency on the LSP just to parse `beamtalk.toml`,
/// and matches the layout the build system writes.
pub(in crate::server) fn dependency_src_dirs(root: &Path) -> Vec<PathBuf> {
    let deps_dir = root.join("_build").join("deps");
    let Ok(entries) = fs::read_dir(&deps_dir) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for entry in entries.flatten() {
        let dep_path = entry.path();
        if !dep_path.is_dir() {
            continue;
        }
        let src = dep_path.join("src");
        if src.is_dir() {
            out.push(src);
        }
    }
    out.sort_unstable();
    out
}
/// Whether `path` lies in a directory [`collect_preload_files`] walks for
/// some workspace root — `src/`, `test/`, or a fetched dependency's `src/`.
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
pub(in crate::server) fn collect_preload_files(config: PreloadConfig) -> PreloadedFiles {
    use beamtalk_core::file_walker::FileWalker;

    let PreloadConfig { roots, stdlib_dirs } = config;
    let mut user_paths = Vec::new();
    let mut remaining_budget = PRELOAD_MAX_FILES;

    let preload_walker = FileWalker::preload_files(remaining_budget);

    // Preload both `src/` and `test/` so that opening a file in
    // `test/` immediately sees classes defined in `src/` (and vice versa).
    // Before this, the LSP would report spurious `Unresolved class` for every
    // test-to-src reference until the user touched the src file manually.
    //
    // Also preload `_build/deps/<name>/src/` for each fetched
    // dependency so references to classes from declared `beamtalk.toml`
    // dependencies (e.g. `HTTPClient` from the `http` package) resolve
    // without spurious `Unresolved class` warnings. Dep dirs are walked
    // *after* every workspace root's `src/`/`test/` so that in a multi-root
    // workspace one root's deps cannot exhaust the shared preload budget
    // before later roots' user files are considered.
    for root in &roots {
        for subdir in ["src", "test"] {
            if remaining_budget == 0 {
                break;
            }
            let dir = root.join(subdir);
            if dir.is_dir() {
                if let Ok(found) = preload_walker
                    .clone()
                    .max_files(remaining_budget)
                    .walk_pathbuf(&dir)
                {
                    remaining_budget = remaining_budget.saturating_sub(found.len());
                    user_paths.extend(found);
                }
            }
        }
    }

    let mut deps_present = false;
    for root in &roots {
        for dep_src in dependency_src_dirs(root) {
            deps_present = true;
            if remaining_budget == 0 {
                break;
            }
            if let Ok(found) = preload_walker
                .clone()
                .max_files(remaining_budget)
                .walk_pathbuf(&dep_src)
            {
                remaining_budget = remaining_budget.saturating_sub(found.len());
                user_paths.extend(found);
            }
        }
    }

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
        deps_present,
    }
}
