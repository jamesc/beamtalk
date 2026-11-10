// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Offline discovery of a package's transitive dependency graph on disk.
//!
//! **DDD Context:** Build System
//!
//! Given a package's `[dependencies]` table, [`discover_dependency_graph`]
//! finds each dependency's checkout — at its declared path for a path
//! dependency, under `_build/deps/<name>/` for a git or registry one — reads
//! that checkout's own `beamtalk.toml`, and continues through its
//! dependencies. It never fetches anything: a dependency whose checkout is
//! not on disk is skipped, and [`DependencyGraph::all_found`] says so.
//!
//! The walk lives here, below both `beamtalk-cli` and the LSP, so the
//! offline dependency scan (MCP lint) and the editor see the same graph
//! (BT-4611). `beamtalk-cli`'s full resolution (`ensure_deps_resolved`) has
//! its own walk, because it fetches and reports errors, but it shares the
//! checkout-path step, [`dep_root_for_source`].

use crate::dependencies::read_manifest_dependencies;
use beamtalk_core::compilation::{DependencyMap, DependencySource};
use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use std::collections::{HashSet, VecDeque};

/// `<project_root>/_build/deps` — where `beamtalk build` checks out git and
/// registry dependencies (`_build/deps/<name>/`).
#[must_use]
pub fn dependency_checkouts_dir(project_root: &Utf8Path) -> Utf8PathBuf {
    project_root.join("_build").join("deps")
}

/// Normalize a path by resolving `.` and `..` components without filesystem access.
///
/// Unlike `std::fs::canonicalize`, this does not require the path to exist and
/// does not resolve symlinks.
#[must_use]
pub fn normalize_path(path: &Utf8Path) -> Utf8PathBuf {
    let mut components = Vec::new();
    for component in path.components() {
        match component {
            Utf8Component::CurDir => {
                // Skip `.`
            }
            Utf8Component::ParentDir => match components.last().copied() {
                // Already at the filesystem root — an extra `..` is a no-op
                // rather than something to pop or accumulate: popping
                // `RootDir` here would turn `/foo/../..` into `.` instead of
                // `/`.
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

/// Resolve the filesystem checkout root for a single dependency.
///
/// - Path deps: joins `declaring_root` with the declared relative path and
///   normalises `.`/`..` components without filesystem access.
/// - Git / Registry deps: `<checkouts_dir>/<name>/` (see
///   [`dependency_checkouts_dir`]), where the resolved checkout lives after a
///   `beamtalk build`.
///
/// The single shared resolution primitive used by [`discover_dependency_graph`]
/// and by `beamtalk-cli`'s full dependency resolution.
///
/// # Errors
///
/// Returns `Err(&Path)` carrying the non-UTF-8 path if `source` is a
/// `Path` dependency whose declared path cannot be represented as UTF-8.
/// The caller decides whether to warn-and-skip or surface an error. Git and
/// Registry sources always succeed.
pub fn dep_root_for_source<'s>(
    declaring_root: &Utf8Path,
    dep_name: &str,
    source: &'s DependencySource,
    checkouts_dir: &Utf8Path,
) -> Result<Utf8PathBuf, &'s std::path::Path> {
    match source {
        DependencySource::Path { path } => Utf8Path::from_path(path)
            .map(|relative| normalize_path(&declaring_root.join(relative)))
            .ok_or(path.as_path()),
        DependencySource::Git { .. } | DependencySource::Registry { .. } => {
            Ok(checkouts_dir.join(dep_name))
        }
    }
}

/// The directory holding a dependency's Beamtalk source: its `src/`, or the
/// checkout root itself when it has no `src/`. A `stubs/` directory under the
/// checkout root holds FFI stubs, not source, and is not part of it.
#[must_use]
pub fn dependency_source_dir(dep_root: &Utf8Path) -> Utf8PathBuf {
    let src_dir = dep_root.join("src");
    if src_dir.is_dir() {
        src_dir
    } else {
        dep_root.to_path_buf()
    }
}

/// One dependency [`discover_dependency_graph`] found on disk.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiscoveredDependency {
    /// The name it is declared under.
    pub name: String,
    /// Its checkout root.
    pub root: Utf8PathBuf,
    /// Names of its own direct dependencies, from its `beamtalk.toml` (empty
    /// when that manifest could not be read).
    pub dependencies: Vec<String>,
}

/// A package's transitive dependency graph, as far as it is on disk.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DependencyGraph {
    /// Every dependency found on disk, in breadth-first discovery order.
    pub found: Vec<DiscoveredDependency>,
    /// Whether the walk saw the whole graph: every transitive dependency was
    /// found on disk, with a UTF-8 path and a readable `beamtalk.toml` (so
    /// its own dependencies are known).
    pub all_found: bool,
}

/// Walk the transitive dependency graph of the package at `project_root`,
/// starting from its direct `[dependencies]` table, without network I/O.
///
/// Breadth-first; a dependency reached twice (a diamond) is visited once, by
/// name, matching the CLI's single-version policy. Path dependencies resolve
/// against the package that declared them; git and registry dependencies
/// against `project_root`'s `_build/deps/`. Not sandboxed: a path dependency
/// may point outside `project_root`, the same trust model as `beamtalk build`
/// (`beamtalk.toml` is authored by the project owner).
#[must_use]
pub fn discover_dependency_graph(
    project_root: &Utf8Path,
    direct: DependencyMap,
) -> DependencyGraph {
    let checkouts_dir = dependency_checkouts_dir(project_root);
    let mut found = Vec::new();
    let mut all_found = true;
    let mut visited: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<(Utf8PathBuf, DependencyMap)> =
        VecDeque::from([(project_root.to_path_buf(), direct)]);

    while let Some((declaring_root, deps)) = queue.pop_front() {
        for (name, spec) in deps {
            if !visited.insert(name.clone()) {
                continue;
            }
            let root = match dep_root_for_source(
                &declaring_root,
                &name,
                &spec.source,
                &checkouts_dir,
            ) {
                Ok(root) => root,
                Err(path) => {
                    tracing::warn!(dep = %name, path = %path.display(), "Dependency path is not valid UTF-8; skipping");
                    all_found = false;
                    continue;
                }
            };
            if !root.is_dir() {
                // Not fetched yet (or a path dependency that doesn't exist):
                // its own dependencies can't be discovered either.
                all_found = false;
                continue;
            }
            let mut dependencies = Vec::new();
            match read_manifest_dependencies(&root.join("beamtalk.toml")) {
                Ok(own) => {
                    dependencies = own.keys().cloned().collect();
                    if !own.is_empty() {
                        queue.push_back((root.clone(), own));
                    }
                }
                Err(e) => {
                    tracing::debug!(dep = %name, error = %e, "Dependency manifest unreadable; its dependencies are unknown");
                    all_found = false;
                }
            }
            found.push(DiscoveredDependency {
                name,
                root,
                dependencies,
            });
        }
    }

    DependencyGraph { found, all_found }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::compilation::{DependencySpec, GitReference};
    use std::fs;

    fn utf8(dir: &tempfile::TempDir) -> Utf8PathBuf {
        Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap()
    }

    fn write_package(root: &Utf8Path, name: &str, deps: &str) {
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(
            root.join("beamtalk.toml"),
            format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n\n[dependencies]\n{deps}"),
        )
        .unwrap();
    }

    fn path_dep(name: &str, path: &str) -> (String, DependencySpec) {
        (
            name.to_string(),
            DependencySpec {
                name: name.to_string(),
                source: DependencySource::Path {
                    path: std::path::PathBuf::from(path),
                },
            },
        )
    }

    #[test]
    fn dep_root_for_source_path_dep_is_normalized() {
        let source = DependencySource::Path {
            path: std::path::PathBuf::from("../my-dep"),
        };
        let result = dep_root_for_source(
            Utf8Path::new("/project/pkg"),
            "my-dep",
            &source,
            Utf8Path::new("/project/_build/deps"),
        );
        assert_eq!(result, Ok(Utf8PathBuf::from("/project/my-dep")));
    }

    #[test]
    fn dep_root_for_source_git_dep_uses_checkout_dir() {
        let source = DependencySource::Git {
            url: "https://example.com/repo.git".into(),
            reference: GitReference::Tag("v1.0.0".into()),
        };
        let checkouts = dependency_checkouts_dir(Utf8Path::new("/project"));
        let result =
            dep_root_for_source(Utf8Path::new("/project"), "my-git-dep", &source, &checkouts);
        assert_eq!(
            result,
            Ok(Utf8PathBuf::from("/project/_build/deps/my-git-dep"))
        );
    }

    #[test]
    fn dep_root_for_source_registry_dep_uses_checkout_dir() {
        let source = DependencySource::Registry {
            version: "1.2.3".into(),
        };
        let checkouts = dependency_checkouts_dir(Utf8Path::new("/project"));
        let result =
            dep_root_for_source(Utf8Path::new("/project"), "my-reg-dep", &source, &checkouts);
        assert_eq!(
            result,
            Ok(Utf8PathBuf::from("/project/_build/deps/my-reg-dep"))
        );
    }

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
        // not `.`.
        let path = Utf8PathBuf::from("/foo/../..");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/"));
    }

    #[test]
    fn dependency_source_dir_prefers_src() {
        let temp = tempfile::tempdir().unwrap();
        let root = utf8(&temp);
        assert_eq!(dependency_source_dir(&root), root);
        fs::create_dir(root.join("src")).unwrap();
        assert_eq!(dependency_source_dir(&root), root.join("src"));
    }

    #[test]
    fn discovers_transitive_path_dependencies() {
        let temp = tempfile::tempdir().unwrap();
        let base = utf8(&temp);
        write_package(&base.join("app"), "app", "a = { path = \"../a\" }\n");
        write_package(&base.join("a"), "a", "b = { path = \"../b\" }\n");
        write_package(&base.join("b"), "b", "");

        let graph = discover_dependency_graph(
            &base.join("app"),
            DependencyMap::from([path_dep("a", "../a")]),
        );
        assert!(graph.all_found);
        let names: Vec<_> = graph.found.iter().map(|d| d.name.as_str()).collect();
        assert_eq!(names, ["a", "b"]);
        assert_eq!(graph.found[0].root, base.join("a"));
        assert_eq!(graph.found[0].dependencies, ["b"]);
        assert_eq!(graph.found[1].root, base.join("b"));
    }

    #[test]
    fn a_missing_dependency_is_not_all_found() {
        let temp = tempfile::tempdir().unwrap();
        let base = utf8(&temp);
        write_package(&base.join("app"), "app", "");
        let graph = discover_dependency_graph(
            &base.join("app"),
            DependencyMap::from([path_dep("gone", "../gone")]),
        );
        assert!(!graph.all_found);
        assert!(graph.found.is_empty());
    }

    #[test]
    fn an_unreadable_dependency_manifest_is_not_all_found() {
        let temp = tempfile::tempdir().unwrap();
        let base = utf8(&temp);
        write_package(&base.join("app"), "app", "");
        fs::create_dir_all(base.join("a/src")).unwrap();
        fs::write(base.join("a/beamtalk.toml"), "not [valid toml").unwrap();
        let graph = discover_dependency_graph(
            &base.join("app"),
            DependencyMap::from([path_dep("a", "../a")]),
        );
        assert!(!graph.all_found);
        assert_eq!(graph.found.len(), 1, "the checkout itself was found");
    }

    #[test]
    fn git_dependencies_resolve_under_the_projects_build_deps() {
        let temp = tempfile::tempdir().unwrap();
        let base = utf8(&temp);
        write_package(&base, "app", "");
        write_package(&base.join("_build/deps/remote"), "remote", "");
        let direct = DependencyMap::from([(
            "remote".to_string(),
            DependencySpec {
                name: "remote".to_string(),
                source: DependencySource::Git {
                    url: "https://example.com/remote.git".into(),
                    reference: GitReference::Tag("v1".into()),
                },
            },
        )]);
        let graph = discover_dependency_graph(&base, direct);
        assert!(graph.all_found);
        assert_eq!(graph.found[0].root, base.join("_build/deps/remote"));
    }
}
