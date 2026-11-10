// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Initialize-params configuration: workspace roots, stdlib source
//! directories, and the startup source-file preload collection.

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

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
    /// What this walk learned about the walked roots' dependencies, for the
    /// pre-WS3 dependency guard (BT-4611).
    pub(in crate::server) dependencies: DependencyLoad,
}
/// One preload walk's dependency loading, for ADR 0100's pre-WS3 guard
/// (BT-4611). The guard stays on unless every declared transitive dependency
/// was found on disk and all of its source was preloaded; the caller adds the
/// last condition, that no preloaded dependency file has a parse error, once
/// the files are indexed ([`DependencyLoad::extensions_unknown`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::server) struct DependencyLoad {
    /// Whether any walked root declares dependencies (a manifest whose
    /// `[dependencies]` can't be read is assumed to).
    pub(in crate::server) declared: bool,
    /// Whether every walked root's transitive dependencies were found on
    /// disk and every one of their source files was read within budget.
    pub(in crate::server) loaded: bool,
    /// The dependency source directories the walk preloaded.
    pub(in crate::server) src_dirs: Vec<PathBuf>,
}
impl Default for DependencyLoad {
    /// No roots walked: nothing declared, so nothing missing.
    fn default() -> Self {
        Self {
            declared: false,
            loaded: true,
            src_dirs: Vec::new(),
        }
    }
}
impl DependencyLoad {
    /// The guard's verdict, given whether any preloaded dependency file is
    /// incomplete (parse error) — the CLI's rule,
    /// [`beamtalk_project::dependency_extensions::dependency_extensions_unknown`].
    pub(in crate::server) fn extensions_unknown(&self, any_incomplete_file: bool) -> bool {
        beamtalk_project::dependency_extensions::dependency_extensions_unknown(
            self.declared,
            self.loaded,
            !any_incomplete_file,
        )
    }
}
/// One dependency of a package root, as found on disk.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::server) struct DependencyDirs {
    /// The dependency's checkout root.
    pub(in crate::server) root: PathBuf,
    /// Where its Beamtalk source lives: `src/`, or the root itself
    /// ([`beamtalk_project::dependency_graph::dependency_source_dir`]).
    pub(in crate::server) src: PathBuf,
}
/// A package root's declared dependencies, as far as they are on disk.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::server) struct RootDependencies {
    /// Whether the root's manifest declares any dependency. An unreadable
    /// or malformed `[dependencies]` table conservatively counts as yes.
    pub(in crate::server) declared: bool,
    /// Every transitive dependency found on disk.
    pub(in crate::server) found: Vec<DependencyDirs>,
    /// Whether that is all of them: the manifest was read and every
    /// transitive dependency was found, with a readable manifest.
    pub(in crate::server) all_found: bool,
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
    if let Some(owner) = owning_package_of_dependency_checkout(&root) {
        return package_root_for_file(owner);
    }
    root.join("beamtalk.toml").is_file().then_some(root)
}
/// `Some(<pkg>)` when `dir` is a fetched dependency checkout
/// `<pkg>/_build/deps/<name>` (where git and registry dependencies live).
fn owning_package_of_dependency_checkout(dir: &Path) -> Option<&Path> {
    let deps = dir.parent()?;
    let build = deps.parent()?;
    (deps.file_name()? == "deps" && build.file_name()? == "_build")
        .then(|| build.parent())
        .flatten()
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
/// The declared dependencies of the package at `root` (BT-4611): its
/// `beamtalk.toml` `[dependencies]` and theirs, transitively, located on disk
/// with the walk the CLI's offline dependency scan uses
/// ([`beamtalk_project::dependency_graph::discover_dependency_graph`]) —
/// path dependencies at their declared path, git and registry ones under
/// `_build/deps/<name>/`. Never fetches.
///
/// A root with no `beamtalk.toml` is not a package and has none. One whose
/// `[dependencies]` can't be read is assumed to have some, none of them
/// known; its fetched checkouts (`_build/deps/*/`) are still returned so
/// their classes resolve.
pub(in crate::server) fn root_dependencies(root: &Path) -> RootDependencies {
    use beamtalk_project::dependency_graph::{dependency_source_dir, discover_dependency_graph};

    let manifest = root.join("beamtalk.toml");
    if !manifest.is_file() {
        return RootDependencies {
            declared: false,
            found: Vec::new(),
            all_found: true,
        };
    }
    let read = camino::Utf8Path::from_path(root).ok_or(()).and_then(|utf8_root| {
        beamtalk_project::dependencies::read_manifest_dependencies(&utf8_root.join("beamtalk.toml"))
            .map(|deps| (utf8_root, deps))
            .map_err(|e| {
                // `debug`, not `warn`: this runs on every `did_open` (via
                // `preload_covers`), and `beamtalk build` reports the error.
                tracing::debug!(root = %root.display(), error = %e, "Cannot read [dependencies]; keeping the dependency guard on");
            })
    });
    let Ok((utf8_root, direct)) = read else {
        return RootDependencies {
            declared: true,
            found: fetched_dependency_checkouts(root),
            all_found: false,
        };
    };
    let declared = !direct.is_empty();
    let graph = discover_dependency_graph(utf8_root, direct);
    RootDependencies {
        declared,
        found: graph
            .found
            .into_iter()
            .map(|dep| DependencyDirs {
                src: dependency_source_dir(&dep.root).into_std_path_buf(),
                root: dep.root.into_std_path_buf(),
            })
            .collect(),
        all_found: graph.all_found,
    }
}
/// Every fetched checkout under `root/_build/deps/` with a `src/` — the
/// fallback when the manifest's `[dependencies]` can't be read.
fn fetched_dependency_checkouts(root: &Path) -> Vec<DependencyDirs> {
    let deps_dir = root.join("_build").join("deps");
    let Ok(entries) = fs::read_dir(&deps_dir) else {
        return Vec::new();
    };
    let mut out: Vec<DependencyDirs> = entries
        .flatten()
        .map(|entry| entry.path())
        .filter(|dep| dep.join("src").is_dir())
        .map(|dep| DependencyDirs {
            src: dep.join("src"),
            root: dep,
        })
        .collect();
    out.sort_unstable_by(|a, b| a.root.cmp(&b.root));
    out
}
/// The source directories of every dependency of `root` found on disk
/// ([`root_dependencies`]).
pub(in crate::server) fn dependency_src_dirs(root: &Path) -> Vec<PathBuf> {
    root_dependencies(root)
        .found
        .into_iter()
        .map(|dep| dep.src)
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
    // `Unresolved class` warnings, and their extensions are visible. Dep dirs are walked
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

    // Every declared dependency's source, path dependencies included
    // (BT-4611), so their classes and extensions are indexed. Whether that
    // loading was complete decides the pre-WS3 dependency guard.
    let (mut dependencies, dependency_paths) = walk_dependency_sources(
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
            // An unread dependency file hides whatever extensions it holds.
            if dependency_paths.contains(&path) {
                dependencies.loaded = false;
            }
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
        dependencies,
    }
}
/// The dependency half of [`collect_preload_files_within`]: walks every
/// root's declared dependencies' source directories within
/// `remaining_budget`, appending their files to `user_paths`. Returns what it
/// learned for the dependency guard, and the set of dependency files (so an
/// unreadable one can still mark loading incomplete).
fn walk_dependency_sources(
    roots: &[PathBuf],
    preload_walker: &beamtalk_core::file_walker::FileWalker,
    remaining_budget: &mut usize,
    user_paths: &mut Vec<PathBuf>,
) -> (DependencyLoad, HashSet<PathBuf>) {
    let mut dependencies = DependencyLoad::default();
    let mut dependency_paths = HashSet::new();
    for root in roots {
        let root_deps = root_dependencies(root);
        dependencies.declared |= root_deps.declared;
        dependencies.loaded &= root_deps.all_found;
        for dep in root_deps.found {
            if *remaining_budget == 0 {
                dependencies.loaded = false;
                break;
            }
            match preload_walker
                .clone()
                .max_files(*remaining_budget)
                .walk_pathbuf(&dep.src)
            {
                Ok(found) => {
                    *remaining_budget = remaining_budget.saturating_sub(found.len());
                    // `stubs/` holds FFI stubs, not source (as in the CLI's
                    // dependency scan); it is only reachable when the
                    // dependency has no `src/`.
                    let stubs = dep.root.join("stubs");
                    for path in found {
                        if path.starts_with(&stubs) {
                            continue;
                        }
                        // A non-UTF-8 path can't be indexed.
                        if path.to_str().is_none() {
                            dependencies.loaded = false;
                        }
                        dependency_paths.insert(path.clone());
                        user_paths.push(path);
                    }
                }
                Err(_) => dependencies.loaded = false,
            }
            dependencies.src_dirs.push(dep.src);
        }
    }
    // A walk that ran the budget down to zero may have been cut short.
    if *remaining_budget == 0 {
        dependencies.loaded = false;
    }
    (dependencies, dependency_paths)
}
