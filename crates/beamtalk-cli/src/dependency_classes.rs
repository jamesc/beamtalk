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
//! **Known limitation:** only *direct* dependencies (the project's own
//! `[dependencies]` table) are resolved — unlike `ensure_deps_resolved`,
//! this does not walk transitive dependency graphs. A class defined only in
//! a dependency-of-a-dependency (and referenced directly, which is unusual)
//! is not covered. See BT-2823's follow-up for extending this if it proves
//! necessary in practice.

use crate::build_layout::BuildLayout;
use crate::manifest;
use beamtalk_core::compilation::DependencySource;
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use camino::Utf8Path;
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

    for (name, spec) in &parsed.dependencies {
        let dep_root = match &spec.source {
            DependencySource::Path { path } => {
                // Not normalized/sandboxed: a `..`-containing or absolute
                // `path` can point outside `project_root`, matching the
                // trust model of `beamtalk build`'s own path-dependency
                // resolution (`deps/path.rs::canonicalize_dep_path`) —
                // `beamtalk.toml` is authored by the project owner, not
                // untrusted input.
                let Some(relative) = Utf8Path::from_path(path) else {
                    warn!(dep = %name, "Dependency path is not valid UTF-8; skipping");
                    continue;
                };
                project_root.join(relative)
            }
            DependencySource::Git { .. } => layout.dep_checkout_dir(name),
        };

        if !dep_root.is_dir() {
            // Not yet fetched/built — best-effort, skip silently.
            continue;
        }

        collect_dep_class_infos(&dep_root, name, &mut class_infos);
    }

    (has_package_dependencies, class_infos)
}

/// Parse every `.bt` file under a dependency's `src/` directory (falling
/// back to its root if there is no `src/`) and append its class metadata to
/// `class_infos`.
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

    for file in files {
        let source = match std::fs::read_to_string(&file) {
            Ok(source) => source,
            Err(e) => {
                warn!(dep = %dep_name, file = %file, error = %e, "Failed to read dependency source file");
                continue;
            }
        };

        let tokens = lex_with_eof(&source);
        let (module, _parse_diags) = parse(tokens);
        class_infos
            .extend(beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module));
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

    #[test]
    fn no_manifest_returns_empty() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        let (has_deps, infos) = resolve_dependency_class_infos(root);
        assert!(!has_deps);
        assert!(infos.is_empty());
    }

    #[test]
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
}
