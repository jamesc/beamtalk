// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Best-effort, offline resolution of a project's dependency classes (BT-2823).
//!
//! **DDD Context:** Build System
//!
//! `beamtalk build` / `beamtalk lint` merge each declared dependency's class
//! metadata into the `ClassHierarchy` via [`crate::deps`]'s full resolution
//! pipeline (`ensure_deps_resolved`), which can fetch missing git
//! dependencies over the network and recompile stale ones. The MCP server's
//! offline `lint` and `diagnostic_summary` tools are documented as working
//! without a live REPL connection and must not trigger surprise network I/O
//! or writes under `_build/` as a side effect of a diagnostics query.
//!
//! This module instead reads whatever dependency checkouts are *already*
//! present on disk — under `_build/deps/<name>/` for git dependencies (the
//! [`crate::build_layout::BuildLayout`] convention), or at their declared
//! local path for path dependencies — exactly the state left behind by a
//! prior `beamtalk build`. Dependencies that have never been fetched/built
//! are silently skipped, matching the existing best-effort philosophy of
//! `beamtalk lint`'s own dependency resolution (and BT-2134's native type
//! registry, which likewise falls back to "no data" when its build artifact
//! is missing).
//!
//! Without this, MCP `lint`/`diagnostic_summary` report false-positive
//! `Unresolved class` diagnostics for every class defined only in a
//! dependency, because [`beamtalk_core::project::package`] only walks the
//! package's own `src/`/`test/` directories.
//!
//! **Transitive dependencies (BT-2836):** [`resolve_dependency_class_infos`]
//! walks the full transitive dependency graph — not just the project's own
//! direct `[dependencies]` table — by recursively reading each discovered
//! dependency's own `beamtalk.toml` (when its checkout is already present on
//! disk) and queuing its dependencies in turn. This mirrors
//! `discover_all_dep_roots` in `crate::commands::deps` (the CLI's own
//! transitive-walk logic for `ensure_deps_resolved`'s freshness checks), but
//! is reimplemented rather than shared: `discover_all_dep_roots` lives in
//! the `beamtalk-cli` *binary* crate (`mod commands;` in `main.rs`), while
//! this module is part of the `beamtalk-cli` *library* crate that
//! `beamtalk-mcp` links against, so it cannot call into the binary's private
//! modules. Keep the two algorithms in sync if either changes. As with
//! direct dependencies, a checkout that hasn't been fetched yet (missing
//! `_build/deps/<name>/`) is silently skipped rather than fetched — no
//! network I/O.
//!
//! **Caching (BT-2837):** the MCP server's `lint`/`diagnostic_summary` tools
//! call [`resolve_dependency_class_infos`] on *every* request. Without
//! caching, a project with several sizeable dependencies re-lexes and
//! re-parses every dependency `.bt` file on every call — cost that scales
//! with total dependency source size, not just the files actually being
//! linted. [`collect_dep_class_infos`] therefore keeps a process-lifetime
//! cache of resolved `ClassInfo`s per dependency checkout path, keyed by a
//! cheap [`DepFingerprint`] (file count + latest mtime) so a checkout
//! replaced by a later `beamtalk build`/re-fetch is detected and
//! re-resolved. This adds no network I/O or persistent state.

use crate::build_layout::BuildLayout;
use crate::manifest;
use beamtalk_core::compilation::{DependencyMap, DependencySource};
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Mutex, OnceLock, PoisonError};
use std::time::SystemTime;
use tracing::warn;

/// Resolve dependency class metadata for `project_root` without performing
/// any network I/O.
///
/// Returns `(has_package_dependencies, class_infos)`:
/// - `has_package_dependencies` mirrors `beamtalk lint`'s
///   `resolve_dep_class_infos` flag (BT-2794): read from the manifest's
///   `[dependencies]` table regardless of whether any individual dependency
///   could actually be resolved on disk, so a dependency that hasn't been
///   fetched yet doesn't flip diagnostic behaviour between runs.
/// - `class_infos` contains every class defined in a dependency whose
///   checkout is present on disk.
///
/// Returns `(false, Vec::new())` if `project_root` has no `beamtalk.toml`.
#[must_use]
pub fn resolve_dependency_class_infos(project_root: &Utf8Path) -> (bool, Vec<ClassInfo>) {
    let manifest_path = project_root.join("beamtalk.toml");
    match manifest_path.try_exists() {
        Ok(true) => {}
        Ok(false) => return (false, Vec::new()),
        Err(e) => {
            warn!(
                error = %e,
                path = %manifest_path,
                "Failed to check for beamtalk.toml for offline dependency class resolution; \
                 assuming no manifest"
            );
            return (false, Vec::new());
        }
    }

    let parsed = match manifest::parse_manifest_full(&manifest_path) {
        Ok(parsed) => parsed,
        Err(e) => {
            warn!(
                error = %e,
                "Failed to parse beamtalk.toml for offline dependency class resolution; \
                 conservatively assuming dependencies are declared"
            );
            return (true, Vec::new());
        }
    };

    let has_package_dependencies = !parsed.dependencies.is_empty();
    let layout = BuildLayout::new(project_root);
    let mut class_infos = Vec::new();

    // BFS over the transitive dependency graph (BT-2836), matching
    // `discover_all_dep_roots`'s reachability: a queue of
    // `(declaring_root, deps_to_visit)` pairs, seeded with the project's own
    // direct `[dependencies]` table. `visited` dedups by name only (not
    // path), matching the CLI's single-version policy — a diamond dependency
    // (reached via two different parents) is resolved once.
    let mut visited: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<(Utf8PathBuf, DependencyMap)> =
        VecDeque::from([(project_root.to_path_buf(), parsed.dependencies)]);

    while let Some((declaring_root, deps)) = queue.pop_front() {
        for (name, spec) in deps {
            if !visited.insert(name.clone()) {
                continue; // Already discovered via another path (diamond dep).
            }

            let dep_root = match &spec.source {
                DependencySource::Path { path } => {
                    // Not sandboxed: a `..`-containing or absolute `path` can
                    // point outside `project_root`, matching the trust model
                    // of `beamtalk build`'s own path-dependency resolution
                    // (`deps/path.rs::canonicalize_dep_path`) — `beamtalk.toml`
                    // is authored by the project owner, not untrusted input.
                    // Relative to `declaring_root` (the package whose
                    // manifest declared this dependency), not necessarily
                    // `project_root` — a transitive path dep is relative to
                    // its own declaring package.
                    let Some(relative) = Utf8Path::from_path(path) else {
                        warn!(dep = %name, "Dependency path is not valid UTF-8; skipping");
                        continue;
                    };
                    normalize_path(&declaring_root.join(relative))
                }
                DependencySource::Git { .. } => layout.dep_checkout_dir(&name),
            };

            if !dep_root.is_dir() {
                // Not yet fetched/built — best-effort, skip silently. Its own
                // dependencies (if any) can't be discovered either, since
                // there's no checkout to read a `beamtalk.toml` from.
                continue;
            }

            collect_dep_class_infos(&dep_root, &name, &mut class_infos);

            // Queue this dependency's own dependencies for discovery,
            // reading whatever checkout is already on disk — still no
            // network I/O.
            let dep_manifest_path = dep_root.join("beamtalk.toml");
            if let Ok(dep_parsed) = manifest::parse_manifest_full(&dep_manifest_path) {
                if !dep_parsed.dependencies.is_empty() {
                    queue.push_back((dep_root, dep_parsed.dependencies));
                }
            }
        }
    }

    (has_package_dependencies, class_infos)
}

/// Normalize a path by resolving `.` and `..` components without filesystem
/// access (mirrors `commands::deps::path::normalize_path`, duplicated here
/// per the module doc's note on the binary/library crate split — BT-2836).
///
/// Unlike `std::fs::canonicalize`, this does not require the path to exist
/// and does not resolve symlinks.
fn normalize_path(path: &Utf8Path) -> Utf8PathBuf {
    let mut components = Vec::new();
    for component in path.components() {
        match component {
            Utf8Component::CurDir => {
                // Skip `.`
            }
            Utf8Component::ParentDir => match components.last().copied() {
                // Already at the filesystem root — an extra `..` is a no-op
                // rather than something to pop or accumulate (BT-2836 review
                // finding: popping `RootDir` here would turn `/foo/../..`
                // into `.` instead of `/`).
                Some(Utf8Component::RootDir) => {}
                // Nothing to pop yet, or a run of leading `..`s in a
                // relative path — accumulate.
                None | Some(Utf8Component::ParentDir) => components.push(component),
                // A real component precedes it — cancel the two out.
                Some(_) => {
                    components.pop();
                }
            },
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

/// Cheap staleness signal for a dependency's source tree (BT-2837): the
/// number of `.bt` files under it plus the latest modification time across
/// them, both far cheaper to compute than reading, lexing, and parsing every
/// file — so checking this on every call is worth it even though a cache
/// *hit* still pays for one [`std::fs::metadata`] call per file. A dependency
/// checkout is only ever replaced wholesale by a later `beamtalk
/// build`/re-fetch, so this (rather than hashing file contents) is enough to
/// detect that.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DepFingerprint {
    file_count: usize,
    max_mtime: Option<SystemTime>,
}

impl DepFingerprint {
    fn of(files: &[Utf8PathBuf]) -> Self {
        let max_mtime = files
            .iter()
            .filter_map(|f| std::fs::metadata(f).and_then(|m| m.modified()).ok())
            .max();
        Self {
            file_count: files.len(), // count from walker; metadata() failures don't adjust this
            max_mtime,
        }
    }
}

/// A dependency's cached class infos, tagged with the [`DepFingerprint`]
/// they were resolved under.
type CachedDepClassInfos = (DepFingerprint, Vec<ClassInfo>);

/// Process-lifetime cache of [`collect_dep_class_infos`] results, keyed by
/// dependency checkout path (BT-2837). See the module docs for why this
/// exists. Not persisted to disk — cleared automatically when the process
/// (e.g. the `beamtalk-mcp` server) restarts.
static CLASS_INFO_CACHE: OnceLock<Mutex<HashMap<Utf8PathBuf, CachedDepClassInfos>>> =
    OnceLock::new();

fn class_info_cache() -> &'static Mutex<HashMap<Utf8PathBuf, CachedDepClassInfos>> {
    CLASS_INFO_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

#[cfg(test)]
static PARSE_CALLS: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

/// Parse every `.bt` file under a dependency's `src/` directory (falling
/// back to its root if there is no `src/`) and append its class metadata to
/// `class_infos`, reusing a cached result (BT-2837) when the dependency's
/// [`DepFingerprint`] hasn't changed since the last call.
fn collect_dep_class_infos(dep_root: &Utf8Path, dep_name: &str, class_infos: &mut Vec<ClassInfo>) {
    let src_dir = dep_root.join("src");
    let search_dir = if src_dir.is_dir() {
        src_dir.as_path()
    } else {
        dep_root
    };

    let files = match FileWalker::source_files().walk(search_dir) {
        Ok(files) => files,
        Err(e) => {
            warn!(dep = %dep_name, error = %e, "Failed to walk dependency source directory");
            return;
        }
    };

    let fingerprint = DepFingerprint::of(&files);
    let cache = class_info_cache();
    let cached = cache
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .get(search_dir)
        .filter(|(cached_fingerprint, _)| *cached_fingerprint == fingerprint)
        .map(|(_, cached_infos)| cached_infos.clone());
    if let Some(cached_infos) = cached {
        class_infos.extend(cached_infos);
        return;
    }

    #[cfg(test)]
    PARSE_CALLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    let mut resolved = Vec::new();
    let mut all_read = true;
    for file in files {
        let source = match std::fs::read_to_string(&file) {
            Ok(source) => source,
            Err(e) => {
                warn!(dep = %dep_name, file = %file, error = %e, "Failed to read dependency source file");
                all_read = false;
                continue;
            }
        };

        let tokens = lex_with_eof(&source);
        let (module, _parse_diags) = parse(tokens);
        resolved
            .extend(beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module));
    }

    class_infos.extend(resolved.iter().cloned());
    // Only cache a result derived from every file being read successfully (BT-2837 review) —
    // caching a partial result under this fingerprint would make a transient read failure
    // (e.g. a lock from a concurrent `beamtalk build`) sticky until the fingerprint changes.
    if all_read {
        cache
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .insert(search_dir.to_path_buf(), (fingerprint, resolved));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn write(path: &std::path::Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, contents).unwrap();
    }

    // BT-2836: direct unit coverage for the duplicated `normalize_path`
    // (mirrors `commands::deps::path`'s own `test_normalize_path_*` tests).
    #[test]
    fn normalize_path_resolves_parent() {
        let path = Utf8PathBuf::from("/home/user/project/../dep");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/home/user/dep"));
    }

    #[test]
    fn normalize_path_resolves_dot() {
        let path = Utf8PathBuf::from("/home/user/./project");
        assert_eq!(
            normalize_path(&path),
            Utf8PathBuf::from("/home/user/project")
        );
    }

    #[test]
    fn normalize_path_multiple_parents() {
        let path = Utf8PathBuf::from("/home/user/project/../../dep");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/home/dep"));
    }

    #[test]
    fn normalize_path_parent_at_root() {
        // A `..` chain that consumes every real component and then hits
        // root must not pop `RootDir` itself — `/foo/../..` is still `/`,
        // not `.` (review finding on PR #2989).
        let path = Utf8PathBuf::from("/foo/../..");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/"));
    }

    // BT-2837: `resolve_dependency_class_infos` now reads/writes a
    // process-wide `CLASS_INFO_CACHE` (and, in test builds, the
    // `PARSE_CALLS` counter used to observe cache hits/misses). Every test
    // in this module is serialized under the same key so they can't race on
    // that shared state when the test binary runs with multiple threads.
    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn no_manifest_returns_empty() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(!has_deps);
        assert!(infos.is_empty());
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn no_dependencies_section_returns_false_and_empty() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\n",
        );
        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(!has_deps);
        assert!(infos.is_empty());
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn git_dependency_checkout_present_resolves_classes() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
        );
        // Simulate a prior `beamtalk build` having already fetched the dep.
        write(
            tmp.path()
                .join("_build/deps/http/src/http_server.bt")
                .as_path(),
            "Object subclass: HTTPServer\n",
        );

        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert!(
            infos.iter().any(|c| c.name == "HTTPServer"),
            "expected HTTPServer in resolved class infos, got {infos:?}"
        );
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn git_dependency_not_yet_fetched_is_skipped() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
        );

        // No `_build/deps/http/` checkout — best-effort skip, not an error.
        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert!(infos.is_empty());
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn path_dependency_resolves_classes() {
        // Both the project and its sibling path dependency live inside the
        // same `TempDir` (`<tmp>/app/` and `<tmp>/utils/`) so `../utils`
        // resolves within the isolated tempdir rather than escaping into its
        // shared parent, which would leak files outside the fixture and risk
        // collisions between concurrent test runs.
        let tmp = TempDir::new().unwrap();
        let project_dir = tmp.path().join("app");
        let root = Utf8Path::from_path(project_dir.as_path()).unwrap();
        write(
            project_dir.join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nutils = { path = \"../utils\" }\n",
        );
        write(
            tmp.path().join("utils/src/utils.bt").as_path(),
            "Object subclass: Utils\n",
        );

        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert!(
            infos.iter().any(|c| c.name == "Utils"),
            "expected Utils in resolved class infos, got {infos:?}"
        );
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn cache_hit_avoids_reparsing_unchanged_dependency() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path()
                .join("_build/deps/http/src/http_server.bt")
                .as_path(),
            "Object subclass: HTTPServer\n",
        );

        // Reset to a known baseline (BT-2870 review follow-up): a prior
        // test panicking mid-run under `#[serial]` would otherwise leave
        // this process-wide counter at an arbitrary value, making a
        // spurious failure here harder to diagnose than "expected 0, got N".
        PARSE_CALLS.store(0, std::sync::atomic::Ordering::Relaxed);

        let (_, infos1) = resolve_dependency_class_infos(root);
        let calls_after_first = PARSE_CALLS.load(std::sync::atomic::Ordering::Relaxed);
        assert!(
            calls_after_first > 0,
            "first call should have parsed the dependency file, got 0 parse calls"
        );
        let (_, infos2) = resolve_dependency_class_infos(root);
        let calls_after_second = PARSE_CALLS.load(std::sync::atomic::Ordering::Relaxed);

        assert_eq!(
            calls_after_first, calls_after_second,
            "second call against an unchanged checkout should be served from cache, not re-parsed"
        );
        assert_eq!(infos1, infos2);
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn changed_dependency_file_is_detected_and_reparsed() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
        );
        let dep_file = tmp.path().join("_build/deps/http/src/http_server.bt");
        write(dep_file.as_path(), "Object subclass: HTTPServer\n");

        // Reset to a known baseline — see the comment in
        // `cache_hit_avoids_reparsing_unchanged_dependency` above.
        PARSE_CALLS.store(0, std::sync::atomic::Ordering::Relaxed);

        let (_, infos1) = resolve_dependency_class_infos(root);
        assert!(infos1.iter().any(|c| c.name == "HTTPServer"));
        let calls_after_first = PARSE_CALLS.load(std::sync::atomic::Ordering::Relaxed);
        assert!(
            calls_after_first > 0,
            "first call should have parsed the dependency file, got 0 parse calls"
        );

        // Simulate a rebuild replacing the checkout's contents. Bump the
        // mtime forward explicitly (rather than relying on real wall-clock
        // time, which can land within the same mtime-resolution tick on
        // some filesystems) so the fingerprint reliably observes the change.
        write(dep_file.as_path(), "Object subclass: HTTPClient\n");
        let bumped = std::fs::metadata(&dep_file).unwrap().modified().unwrap()
            + std::time::Duration::from_secs(5);
        std::fs::OpenOptions::new()
            .write(true)
            .open(&dep_file)
            .unwrap()
            .set_modified(bumped)
            .unwrap();

        let (_, infos2) = resolve_dependency_class_infos(root);
        let calls_after_second = PARSE_CALLS.load(std::sync::atomic::Ordering::Relaxed);

        assert!(
            calls_after_second > calls_after_first,
            "changed dependency checkout should be re-parsed, not served from a stale cache"
        );
        assert!(
            infos2.iter().any(|c| c.name == "HTTPClient"),
            "expected fresh parse to see the updated class, got {infos2:?}"
        );
        assert!(
            !infos2.iter().any(|c| c.name == "HTTPServer"),
            "cache should not have served the stale HTTPServer class, got {infos2:?}"
        );
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn new_file_added_to_dependency_is_detected() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nutils = { git = \"https://example.com/utils.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path().join("_build/deps/utils/src/a.bt").as_path(),
            "Object subclass: A\n",
        );

        let (_, infos1) = resolve_dependency_class_infos(root);
        assert!(infos1.iter().any(|c| c.name == "A"));
        assert!(!infos1.iter().any(|c| c.name == "B"));

        // Adding a file changes the fingerprint's file count immediately,
        // unlike mtime alone which can be coarse-grained on some
        // filesystems.
        write(
            tmp.path().join("_build/deps/utils/src/b.bt").as_path(),
            "Object subclass: B\n",
        );

        let (_, infos2) = resolve_dependency_class_infos(root);
        assert!(
            infos2.iter().any(|c| c.name == "B"),
            "expected newly added file's class to be picked up, got {infos2:?}"
        );
    }

    // BT-2836: transitive dependency walk regression tests.

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn transitive_git_dependency_class_is_resolved() {
        // app -> a (direct, declared in app's own manifest) -> b (transitive,
        // declared only in a's checked-out manifest). Both checkouts already
        // exist under `_build/deps/`, simulating a prior `beamtalk build`.
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\na = { git = \"https://example.com/a.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path().join("_build/deps/a/beamtalk.toml").as_path(),
            "[package]\nname = \"a\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nb = { git = \"https://example.com/b.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path().join("_build/deps/a/src/a_class.bt").as_path(),
            "Object subclass: AClass\n",
        );
        // `b` is never declared in app's own beamtalk.toml — only reachable
        // by walking `a`'s manifest.
        write(
            tmp.path().join("_build/deps/b/src/b_class.bt").as_path(),
            "Object subclass: BClass\n",
        );

        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert!(
            infos.iter().any(|c| c.name == "AClass"),
            "expected direct dependency's class, got {infos:?}"
        );
        assert!(
            infos.iter().any(|c| c.name == "BClass"),
            "expected transitive dependency's class to be resolved, got {infos:?}"
        );
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn transitive_path_dependency_class_is_resolved() {
        // app -> a (path dep of app) -> b (path dep of a, relative to a's
        // own directory, not app's).
        let tmp = TempDir::new().unwrap();
        let app_dir = tmp.path().join("app");
        let root = Utf8Path::from_path(app_dir.as_path()).unwrap();
        write(
            app_dir.join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\na = { path = \"../a\" }\n",
        );
        write(
            tmp.path().join("a/beamtalk.toml").as_path(),
            "[package]\nname = \"a\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nb = { path = \"../b\" }\n",
        );
        write(
            tmp.path().join("a/src/a_class.bt").as_path(),
            "Object subclass: AClass\n",
        );
        write(
            tmp.path().join("b/src/b_class.bt").as_path(),
            "Object subclass: BClass\n",
        );

        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert!(
            infos.iter().any(|c| c.name == "BClass"),
            "expected transitive path dependency's class to be resolved, got {infos:?}"
        );
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn unfetched_transitive_dependency_is_skipped() {
        // `a` is checked out and declares a dependency on `b`, but `b`
        // itself has never been fetched — best-effort skip, not an error.
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\na = { git = \"https://example.com/a.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path().join("_build/deps/a/beamtalk.toml").as_path(),
            "[package]\nname = \"a\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nb = { git = \"https://example.com/b.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path().join("_build/deps/a/src/a_class.bt").as_path(),
            "Object subclass: AClass\n",
        );
        // No `_build/deps/b/` checkout.

        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert!(infos.iter().any(|c| c.name == "AClass"));
        assert!(!infos.iter().any(|c| c.name == "BClass"));
    }

    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn diamond_transitive_dependency_is_resolved_once() {
        // app depends directly on both `p` and `q`; both `p` and `q` depend
        // on the same `shared` package. `shared`'s classes must appear
        // exactly once despite being reachable via two paths.
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\n\
             p = { git = \"https://example.com/p.git\", tag = \"v1.0.0\" }\n\
             q = { git = \"https://example.com/q.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path().join("_build/deps/p/beamtalk.toml").as_path(),
            "[package]\nname = \"p\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nshared = { git = \"https://example.com/shared.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path().join("_build/deps/q/beamtalk.toml").as_path(),
            "[package]\nname = \"q\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nshared = { git = \"https://example.com/shared.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path()
                .join("_build/deps/shared/src/shared_class.bt")
                .as_path(),
            "Object subclass: SharedClass\n",
        );

        let (_, infos) = resolve_dependency_class_infos(root);
        let shared_count = infos.iter().filter(|c| c.name == "SharedClass").count();
        assert_eq!(
            shared_count, 1,
            "diamond-reachable dependency's class should appear exactly once, got {infos:?}"
        );
    }
}
