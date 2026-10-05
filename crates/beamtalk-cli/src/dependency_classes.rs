// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Best-effort, offline resolution of a project's dependency classes.
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
//! `beamtalk lint`'s own dependency resolution (and the native type
//! registry, which likewise falls back to "no data" when its build artifact
//! is missing).
//!
//! Without this, MCP `lint`/`diagnostic_summary` report false-positive
//! `Unresolved class` diagnostics for every class defined only in a
//! dependency, because [`beamtalk_project::package`] only walks the
//! package's own `src/`/`test/` directories.
//!
//! **Protocol ASTs (ADR 0127 §10a; BT-3591):** alongside each dependency's
//! `ClassInfo`s, [`resolve_dependency_protocol_defs`] yields the full AST of
//! every *provision-bearing* protocol declared in that dependency's source —
//! the trait-flattening counterpart to the class-metadata resolution above.
//! `trait_expansion::expand_module` needs a used protocol's actual provided
//! methods, not just its name/signature, to flatten a cross-package `uses:`
//! against it; this is the only source that data can come from on this
//! offline path (a checkout with no `.bt` source has none to give — see that
//! function's own doc for the "neither source nor
//! `'__beamtalk_protocol_source'/0`" case, which is not yet implementable
//! here since nothing on this Rust-side path reads a compiled `.beam`
//! module's exports).
//!
//! **Transitive dependencies:** [`resolve_dependency_class_infos`]
//! walks the full transitive dependency graph — not just the project's own
//! direct `[dependencies]` table — by recursively reading each discovered
//! dependency's own `beamtalk.toml` (when its checkout is already present on
//! disk) and queuing its dependencies in turn. The BFS structure is similar
//! to `discover_all_dep_roots` in `crate::commands::deps` (the CLI's own
//! transitive-walk logic for `ensure_deps_resolved`'s freshness checks), but
//! the two walkers produce different outputs (`ClassInfo` vs `DiscoveredDep`)
//! and use different error strategies (warn-and-skip vs. return an error), so
//! their BFS loops remain independent. The *checkout-path resolution* step
//! (path vs. git/registry dispatch) is the shared primitive and lives in
//! [`crate::path_util::dep_root_for_source`] — both walkers call it, so that
//! piece cannot drift. As with direct dependencies, a checkout that hasn't
//! been fetched yet (missing `_build/deps/<name>/`) is silently skipped
//! rather than fetched — no network I/O.
//!
//! **Caching:** the MCP server's `lint`/`diagnostic_summary` tools
//! call [`resolve_dependency_class_infos`] on *every* request. Without
//! caching, a project with several sizeable dependencies re-lexes and
//! re-parses every dependency `.bt` file on every call — cost that scales
//! with total dependency source size, not just the files actually being
//! linted. [`scan_dep`] therefore keeps a process-lifetime
//! cache of resolved `ClassInfo`s per dependency checkout path, keyed by a
//! cheap [`DepFingerprint`] (file count + latest mtime) so a checkout
//! replaced by a later `beamtalk build`/re-fetch is detected and
//! re-resolved. This adds no network I/O or persistent state.

use crate::build_layout::BuildLayout;
use crate::manifest;
use crate::path_util::dep_root_for_source;
use beamtalk_core::ast::ProtocolDefinition;
use beamtalk_core::compilation::DependencyMap;
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use camino::{Utf8Path, Utf8PathBuf};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Mutex, OnceLock, PoisonError};
use std::time::SystemTime;
use tracing::warn;

/// Resolve dependency class metadata for `project_root` without performing
/// any network I/O.
///
/// Returns `(has_package_dependencies, class_infos, protocol_defs)`:
/// - `has_package_dependencies` mirrors `beamtalk lint`'s `run_lint`-computed
///   flag: read from the manifest's
///   `[dependencies]` table regardless of whether any individual dependency
///   could actually be resolved on disk, so a dependency that hasn't been
///   fetched yet doesn't flip diagnostic behaviour between runs.
/// - `class_infos` contains every class defined in a dependency whose
///   checkout is present on disk.
/// - `protocol_defs` contains the full AST of every *provision-bearing*
///   protocol defined in a dependency whose checkout is present on disk
///   (ADR 0127 §10a; BT-3591) — the trait-flattening counterpart to
///   `class_infos`, so a cross-package `uses:` can flatten against it
///   instead of reporting "no source available" (see
///   `trait_expansion::expand_module`'s doc). A dependency whose checkout
///   isn't present on disk contributes nothing here either — same
///   best-effort skip as `class_infos`.
///
/// Returns `(false, Vec::new(), Vec::new())` if `project_root` has no
/// `beamtalk.toml`.
#[must_use]
pub fn resolve_dependency_class_infos(
    project_root: &Utf8Path,
) -> (bool, Vec<ClassInfo>, Vec<ProtocolDefinition>) {
    let manifest_path = project_root.join("beamtalk.toml");
    match manifest_path.try_exists() {
        Ok(true) => {}
        Ok(false) => return (false, Vec::new(), Vec::new()),
        Err(e) => {
            warn!(
                error = %e,
                path = %manifest_path,
                "Failed to check for beamtalk.toml for offline dependency class resolution; \
                 assuming no manifest"
            );
            return (false, Vec::new(), Vec::new());
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
            return (true, Vec::new(), Vec::new());
        }
    };

    let has_package_dependencies = !parsed.dependencies.is_empty();
    let layout = BuildLayout::new(project_root);
    let mut class_infos = Vec::new();
    let mut protocol_defs = Vec::new();
    let mut scanned: Vec<DiscoveredDep> = Vec::new();

    // BFS over the transitive dependency graph, matching
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

            // Not sandboxed: a `..`-containing or absolute path dep can point
            // outside `project_root`, matching the trust model of `beamtalk
            // build`'s own path-dependency resolution — `beamtalk.toml` is
            // authored by the project owner, not untrusted input. Path deps
            // are relative to `declaring_root` (the package whose manifest
            // declared them), not necessarily `project_root`.
            let dep_root = match dep_root_for_source(&declaring_root, &name, &spec.source, &layout)
            {
                Ok(root) => root,
                Err(path) => {
                    warn!(dep = %name, path = %path.display(), "Dependency path is not valid UTF-8; skipping");
                    continue;
                }
            };

            if !dep_root.is_dir() {
                // Not yet fetched/built — best-effort, skip silently. Its own
                // dependencies (if any) can't be discovered either, since
                // there's no checkout to read a `beamtalk.toml` from.
                continue;
            }

            let scan = scan_dep(&dep_root, &name);

            // Queue this dependency's own dependencies for discovery,
            // reading whatever checkout is already on disk — still no
            // network I/O.
            let dep_manifest_path = dep_root.join("beamtalk.toml");
            let mut dependencies = Vec::new();
            if let Ok(dep_parsed) = manifest::parse_manifest_full(&dep_manifest_path) {
                dependencies = dep_parsed.dependencies.keys().cloned().collect();
                if !dep_parsed.dependencies.is_empty() {
                    queue.push_back((dep_root, dep_parsed.dependencies));
                }
            }

            if let Some(scan) = scan {
                scanned.push(DiscoveredDep {
                    name,
                    dependencies,
                    scan,
                });
            }
        }
    }

    // Flatten in the compile order of the graph compile, each dependency
    // against its own protocols and then those of the dependencies before it
    // (BT-3678), so the exports equal a cold build's (BT-3684).
    let mut prior_protocol_defs: Vec<ProtocolDefinition> = Vec::new();
    for dep in compile_order(scanned, &parsed.package.name) {
        let mut infos = dep.scan.class_infos.clone();
        dep.scan
            .trait_users
            .flatten(&mut infos, prior_protocol_defs.iter().cloned(), None);
        class_infos.extend(infos);
        let own_protocol_defs = dep.scan.trait_users.protocol_defs();
        protocol_defs.extend(own_protocol_defs.iter().cloned());
        prior_protocol_defs.extend(own_protocol_defs.iter().cloned());
    }

    (has_package_dependencies, class_infos, protocol_defs)
}

/// A dependency found on disk by [`resolve_dependency_class_infos`]'s walk.
struct DiscoveredDep {
    name: String,
    /// Names of its own direct dependencies, from its `beamtalk.toml`.
    dependencies: Vec<String>,
    scan: std::sync::Arc<ScannedDep>,
}

/// `deps` in the order the graph compile (`beamtalk build`) compiles them
/// ([`crate::dep_order::topological_order`]); in discovery order, with a
/// warning, if the graph has a cycle. Dependencies named only as an edge —
/// whose checkout is not on disk — are not part of it.
fn compile_order(deps: Vec<DiscoveredDep>, root_name: &str) -> Vec<DiscoveredDep> {
    let graph = deps
        .iter()
        .map(|dep| (dep.name.clone(), dep.dependencies.clone()))
        .collect();
    let order = match crate::dep_order::topological_order(&graph, root_name) {
        Ok(order) => order,
        Err(e) => {
            warn!(error = %e, "Dependency graph is not orderable; flattening in discovery order");
            return deps;
        }
    };
    let mut by_name: HashMap<String, DiscoveredDep> = deps
        .into_iter()
        .map(|dep| (dep.name.clone(), dep))
        .collect();
    order
        .iter()
        .filter_map(|name| by_name.remove(name))
        .collect()
}

/// Cheap staleness signal for a dependency's source tree: the
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

/// A scanned dependency: its *unflattened* class infos plus the trait users
/// and provision-bearing protocol ASTs (ADR 0127 §10a; BT-3591) to flatten
/// them with once every dependency's protocols are known (BT-3678).
struct ScannedDep {
    class_infos: Vec<ClassInfo>,
    trait_users: beamtalk_core::semantic_analysis::trait_expansion::TraitUserCollector,
}

/// A [`ScannedDep`] tagged with the [`DepFingerprint`] it was scanned under.
type CachedDepClassInfos = (DepFingerprint, std::sync::Arc<ScannedDep>);

/// Process-lifetime cache of [`scan_dep`] results, keyed by
/// dependency checkout path. See the module docs for why this
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
/// back to its root if there is no `src/`) into a [`ScannedDep`], reusing a
/// cached result when the dependency's [`DepFingerprint`] hasn't changed
/// since the last call. `None` if its source directory can't be walked.
fn scan_dep(dep_root: &Utf8Path, dep_name: &str) -> Option<std::sync::Arc<ScannedDep>> {
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
            return None;
        }
    };

    let fingerprint = DepFingerprint::of(&files);
    let cache = class_info_cache();
    let cached = cache
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .get(search_dir)
        .filter(|(cached_fingerprint, _)| *cached_fingerprint == fingerprint)
        .map(|(_, scanned)| std::sync::Arc::clone(scanned));
    if cached.is_some() {
        return cached;
    }

    #[cfg(test)]
    PARSE_CALLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    let mut class_infos = Vec::new();
    // Trait users and provision-bearing protocols (ADR 0127 §10a; BT-3591).
    let mut trait_users =
        beamtalk_core::semantic_analysis::trait_expansion::TraitUserCollector::for_package(
            dep_name,
        );
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
        class_infos
            .extend(beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module));
        trait_users.add(&module);
    }

    let scanned = std::sync::Arc::new(ScannedDep {
        class_infos,
        trait_users,
    });
    // Only cache a result derived from every file being read successfully —
    // caching a partial result under this fingerprint would make a transient read failure
    // (e.g. a lock from a concurrent `beamtalk build`) sticky until the fingerprint changes.
    if all_read {
        cache.lock().unwrap_or_else(PoisonError::into_inner).insert(
            search_dir.to_path_buf(),
            (fingerprint, std::sync::Arc::clone(&scanned)),
        );
    }
    Some(scanned)
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

    // `resolve_dependency_class_infos` reads/writes a
    // process-wide `CLASS_INFO_CACHE` (and, in test builds, the
    // `PARSE_CALLS` counter used to observe cache hits/misses). Every test
    // in this module is serialized under the same key so they can't race on
    // that shared state when the test binary runs with multiple threads.
    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn no_manifest_returns_empty() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
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
        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
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

        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert!(
            infos.iter().any(|c| c.name == "HTTPServer"),
            "expected HTTPServer in resolved class infos, got {infos:?}"
        );
    }

    /// BT-3673: the offline dependency scan flattens a trait declared in one
    /// of the dependency's files into the `ClassInfo` of its user in another,
    /// so a consumer's typed call to the provided method does not report DNU.
    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn dependency_class_infos_flatten_cross_file_trait_provisions() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
        );
        let dep_src = tmp.path().join("_build/deps/http/src");
        write(
            dep_src.join("tagged.bt").as_path(),
            "Protocol define: Tagged\n  name -> String\n\n  tag -> String => self name\n",
        );
        write(
            dep_src.join("widget.bt").as_path(),
            "Object subclass: Widget\n  uses: Tagged\n  name -> String => \"w\"\n",
        );

        let (_, infos, _) = resolve_dependency_class_infos(root);
        let widget = infos
            .iter()
            .find(|c| c.name == "Widget")
            .expect("Widget resolved from the dependency checkout");
        assert!(
            widget.methods.iter().any(|m| m.selector == "tag"),
            "Widget must carry Tagged's provided `tag`: {:?}",
            widget
                .methods
                .iter()
                .map(|m| &m.selector)
                .collect::<Vec<_>>()
        );
    }

    /// ADR 0127 §10a / BT-3591: a dependency's *provision-bearing* protocol
    /// (a trait) is yielded as a full AST alongside its `ClassInfo`s, so a
    /// cross-package `uses: http@Retryable` can flatten against it — the
    /// trait-flattening counterpart to the class-resolution test above. A
    /// requirement-only protocol in the same dependency is still tracked
    /// (via the offline path's existing `ProtocolInfo` extraction,
    /// unaffected here) but contributes no AST.
    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn git_dependency_checkout_present_resolves_provision_bearing_protocol_defs() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
        );
        write(
            tmp.path()
                .join("_build/deps/http/src/retryable.bt")
                .as_path(),
            "Protocol define: Retryable\n  \
             attempt -> Boolean\n\n  \
             retryTwice -> Boolean => self attempt or: [self attempt]\n",
        );
        write(
            tmp.path()
                .join("_build/deps/http/src/printable.bt")
                .as_path(),
            "Protocol define: Printable\n  asString -> String\n",
        );

        let (has_deps, _infos, protocol_defs) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert_eq!(
            protocol_defs.len(),
            1,
            "expected only the provision-bearing protocol, got {protocol_defs:?}"
        );
        assert_eq!(protocol_defs[0].name.name.as_str(), "Retryable");
    }

    /// A registry dependency lands in the same `_build/deps/<name>/`
    /// checkout as a git dependency, so the offline walk must find its classes
    /// the same way.
    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn registry_dependency_checkout_present_resolves_classes() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        write(
            tmp.path().join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nyaml = \"0.2.1\"\n",
        );
        write(
            tmp.path()
                .join("_build/deps/yaml/src/yaml_parser.bt")
                .as_path(),
            "Object subclass: YAMLParser\n",
        );

        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
        assert!(has_deps);
        assert!(
            infos.iter().any(|c| c.name == "YAMLParser"),
            "expected YAMLParser in resolved class infos, got {infos:?}"
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
        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
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

        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
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

        // Reset to a known baseline: a prior
        // test panicking mid-run under `#[serial]` would otherwise leave
        // this process-wide counter at an arbitrary value, making a
        // spurious failure here harder to diagnose than "expected 0, got N".
        PARSE_CALLS.store(0, std::sync::atomic::Ordering::Relaxed);

        let (_, infos1, _) = resolve_dependency_class_infos(root);
        let calls_after_first = PARSE_CALLS.load(std::sync::atomic::Ordering::Relaxed);
        assert!(
            calls_after_first > 0,
            "first call should have parsed the dependency file, got 0 parse calls"
        );
        let (_, infos2, _) = resolve_dependency_class_infos(root);
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

        let (_, infos1, _) = resolve_dependency_class_infos(root);
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

        let (_, infos2, _) = resolve_dependency_class_infos(root);
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

        let (_, infos1, _) = resolve_dependency_class_infos(root);
        assert!(infos1.iter().any(|c| c.name == "A"));
        assert!(!infos1.iter().any(|c| c.name == "B"));

        // Adding a file changes the fingerprint's file count immediately,
        // unlike mtime alone which can be coarse-grained on some
        // filesystems.
        write(
            tmp.path().join("_build/deps/utils/src/b.bt").as_path(),
            "Object subclass: B\n",
        );

        let (_, infos2, _) = resolve_dependency_class_infos(root);
        assert!(
            infos2.iter().any(|c| c.name == "B"),
            "expected newly added file's class to be picked up, got {infos2:?}"
        );
    }

    // Transitive dependency walk regression tests.

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

        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
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

    /// BT-3678: a class of dependency `b` using a trait of `b`'s own
    /// dependency `a` (discovered after `b`) carries the provided method.
    #[test]
    #[serial_test::serial(dependency_class_cache)]
    fn transitive_dependency_trait_is_flattened_into_user() {
        let tmp = TempDir::new().unwrap();
        let app_dir = tmp.path().join("app");
        let root = Utf8Path::from_path(app_dir.as_path()).unwrap();
        write(
            app_dir.join("beamtalk.toml").as_path(),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nb = { path = \"../b\" }\n",
        );
        write(
            tmp.path().join("b/beamtalk.toml").as_path(),
            "[package]\nname = \"b\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\na = { path = \"../a\" }\n",
        );
        write(
            tmp.path().join("b/src/widget.bt").as_path(),
            "Object subclass: Widget\n  uses: a@Tagged\n  name -> String => \"w\"\n",
        );
        write(
            tmp.path().join("a/src/tagged.bt").as_path(),
            "Protocol define: Tagged\n  name -> String\n\n  tag -> String => self name\n",
        );

        let (_, infos, protocol_defs) = resolve_dependency_class_infos(root);
        let widget = infos.iter().find(|c| c.name == "Widget").unwrap();
        assert!(
            widget.methods.iter().any(|m| m.selector == "tag"),
            "Widget must carry a's provided `tag`: {:?}",
            widget
                .methods
                .iter()
                .map(|m| &m.selector)
                .collect::<Vec<_>>()
        );
        assert_eq!(
            protocol_defs.len(),
            1,
            "only a's Tagged is provision-bearing"
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

        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
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

        let (has_deps, infos, _) = resolve_dependency_class_infos(root);
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

        let (_, infos, _) = resolve_dependency_class_infos(root);
        let shared_count = infos.iter().filter(|c| c.name == "SharedClass").count();
        assert_eq!(
            shared_count, 1,
            "diamond-reachable dependency's class should appear exactly once, got {infos:?}"
        );
    }
}
