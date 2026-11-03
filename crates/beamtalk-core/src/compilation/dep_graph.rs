// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The resolved dependency graph a package last built against.
//!
//! **DDD Context:** Compilation
//!
//! After every successful dependency resolve, `beamtalk build` records the
//! effective dependency graph — every discovered dependency's name, the chain
//! of dependencies it was reached through, and how it was declared — as
//! `_build/deps/.dep-graph.json`. The build compares it against the next
//! resolve to notice structural graph changes (the CLI's `deps::snapshot`).
//!
//! The schema lives here, below both the CLI and the compiler port, because
//! the port reads the same file to learn which dependencies a project
//! actually resolved (BT-4378): a directory listing of `_build/deps/` also
//! holds checkouts of dependencies that are no longer declared anywhere.
//! [`resolved_dependency_names`] turns a snapshot plus the root manifest's
//! current `[dependencies]` into that set.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use serde::{Deserialize, Serialize};

use super::dep_order::dependency_closure;

/// File name of the snapshot inside the project's `_build/deps/` directory.
pub const DEP_GRAPH_SNAPSHOT_FILE: &str = ".dep-graph.json";

/// Current snapshot schema. Bump when the meaning of [`DepGraphEntry`]
/// changes; an unrecognised schema is treated as a miss, so a newer
/// toolchain's snapshot never lets an older binary reuse artifacts it cannot
/// reason about.
pub const DEP_GRAPH_SNAPSHOT_SCHEMA: u32 = 1;

/// One dependency as it was declared the last time the graph resolved.
///
/// `source` is [`super::DependencySource`]'s `Display` form (`path: ../utils`,
/// `git: <url> (tag: v1.0.0)`, `registry: 0.2.1`), so a type swap, a URL
/// change, a tag bump, or a moved path all show up as a different string —
/// and the file stays readable when debugging a rebuild.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DepGraphEntry {
    /// The dependency's package name.
    pub name: String,
    /// The chain of intermediate dependency names it was reached through
    /// (empty for a dependency the root manifest declares directly).
    #[serde(default)]
    pub via: Vec<String>,
    /// How the declaring manifest asked for it.
    pub source: String,
}

/// On-disk snapshot of the effective dependency graph.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DepGraphSnapshot {
    /// Snapshot schema version. An unrecognised value is a miss.
    pub schema: u32,
    /// Every dependency in the transitive graph, ordered by name.
    pub deps: Vec<DepGraphEntry>,
}

impl DepGraphSnapshot {
    /// A current-schema snapshot of `deps`, ordered by name.
    ///
    /// Dependency names are unique across a resolved graph, so sorting by
    /// name is a total order and makes comparison independent of discovery
    /// order.
    #[must_use]
    pub fn new(mut deps: Vec<DepGraphEntry>) -> Self {
        deps.sort_by(|a, b| a.name.cmp(&b.name));
        Self {
            schema: DEP_GRAPH_SNAPSHOT_SCHEMA,
            deps,
        }
    }
}

/// Why a snapshot could not be used. Every case means "no usable snapshot".
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DepGraphSnapshotError {
    /// The file does not exist or could not be read.
    Missing,
    /// The file is not a valid snapshot.
    Corrupt,
    /// The file carries a schema this toolchain does not understand.
    UnrecognisedSchema(u32),
}

impl std::fmt::Display for DepGraphSnapshotError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Missing => f.write_str("no dependency-graph snapshot"),
            Self::Corrupt => f.write_str("corrupt dependency-graph snapshot"),
            Self::UnrecognisedSchema(schema) => write!(
                f,
                "unrecognised dependency-graph schema {schema} (current {DEP_GRAPH_SNAPSHOT_SCHEMA})"
            ),
        }
    }
}

/// Read and validate the snapshot at `path`.
///
/// # Errors
///
/// Returns a [`DepGraphSnapshotError`] when the file is missing, unreadable,
/// not valid JSON for the schema, or carries an unrecognised schema version.
pub fn read_dep_graph_snapshot(path: &Path) -> Result<DepGraphSnapshot, DepGraphSnapshotError> {
    let data = std::fs::read_to_string(path).map_err(|_| DepGraphSnapshotError::Missing)?;
    let snapshot: DepGraphSnapshot =
        serde_json::from_str(&data).map_err(|_| DepGraphSnapshotError::Corrupt)?;
    if snapshot.schema != DEP_GRAPH_SNAPSHOT_SCHEMA {
        return Err(DepGraphSnapshotError::UnrecognisedSchema(snapshot.schema));
    }
    Ok(snapshot)
}

/// The names a `beamtalk.toml`'s `[dependencies]` table declares, read
/// straight from raw TOML for callers with no manifest struct of their own
/// (the compiler port), like
/// [`parse_package_name_from_manifest_toml`](super::parse_package_name_from_manifest_toml).
///
/// Only the table's keys are read — each key is the dependency's package
/// name, whatever its source. Invalid TOML or a missing/non-table
/// `[dependencies]` yields no names.
#[must_use]
pub fn declared_dependency_names_from_manifest_toml(manifest_toml: &str) -> Vec<String> {
    let Ok(value) = toml::from_str::<toml::Value>(manifest_toml) else {
        return Vec::new();
    };
    value
        .get("dependencies")
        .and_then(toml::Value::as_table)
        .map(|table| table.keys().cloned().collect())
        .unwrap_or_default()
}

/// The dependencies a project currently resolves: everything reachable from
/// the root manifest's currently declared `declared_direct` dependencies
/// through the edges `snapshot` recorded.
///
/// The snapshot is the build's resolved graph, but it is only rewritten by
/// the next resolve, so a dependency since deleted from `beamtalk.toml` is
/// still in it. The root's own edges therefore come from the manifest as it
/// is now, and only the dependency-to-dependency edges (each entry's `via`
/// chain) from the snapshot: a removed direct dependency drops out together
/// with everything only it pulled in, without waiting for a rebuild.
///
/// Limitation (BT-4394): the snapshot records only the *first* `via` chain
/// that reached each dependency, so in a diamond (`root → {a, b}`,
/// `a → c`, `b → c`, recorded as `c via [a]`) removing `a` also drops `c`
/// until the next resolve rewrites the snapshot, even though `b` still
/// needs it.
///
/// A name declared but absent from the snapshot (added since the last
/// resolve) is still returned; the caller finds nothing on disk for it.
#[must_use]
pub fn resolved_dependency_names(
    snapshot: &DepGraphSnapshot,
    declared_direct: &[String],
) -> BTreeSet<String> {
    // The empty string is never a package name, so it cannot collide with a
    // dependency node.
    const ROOT: &str = "";
    let mut edges: BTreeMap<String, Vec<String>> = BTreeMap::new();
    edges.insert(ROOT.to_string(), declared_direct.to_vec());
    for entry in &snapshot.deps {
        // `via` is the chain from a direct dependency down to `entry`; its
        // first hop is a root edge, which comes from the manifest instead.
        let mut chain = entry.via.iter().chain(std::iter::once(&entry.name));
        let Some(mut parent) = chain.next() else {
            continue;
        };
        for child in chain {
            edges.entry(parent.clone()).or_default().push(child.clone());
            parent = child;
        }
    }
    dependency_closure(&edges, ROOT)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(name: &str, via: &[&str]) -> DepGraphEntry {
        DepGraphEntry {
            name: name.to_string(),
            via: via.iter().map(|v| (*v).to_string()).collect(),
            source: format!("path: ../{name}"),
        }
    }

    fn names(set: &BTreeSet<String>) -> Vec<&str> {
        set.iter().map(String::as_str).collect()
    }

    fn declared(names: &[&str]) -> Vec<String> {
        names.iter().map(|n| (*n).to_string()).collect()
    }

    #[test]
    fn resolved_set_follows_recorded_chains_from_declared_roots() {
        // my_app -> {utils, json}; utils -> yaml -> core.
        let snapshot = DepGraphSnapshot::new(vec![
            entry("utils", &[]),
            entry("json", &[]),
            entry("yaml", &["utils"]),
            entry("core", &["utils", "yaml"]),
        ]);
        assert_eq!(
            names(&resolved_dependency_names(
                &snapshot,
                &declared(&["utils", "json"])
            )),
            ["core", "json", "utils", "yaml"]
        );
    }

    #[test]
    fn a_dependency_removed_from_the_manifest_drops_with_its_subtree() {
        // The snapshot still records `utils` (and what it pulled in), but the
        // manifest no longer declares it.
        let snapshot = DepGraphSnapshot::new(vec![
            entry("utils", &[]),
            entry("json", &[]),
            entry("yaml", &["utils"]),
        ]);
        assert_eq!(
            names(&resolved_dependency_names(&snapshot, &declared(&["json"]))),
            ["json"]
        );
    }

    #[test]
    fn a_newly_declared_dependency_is_included_before_it_resolves() {
        let snapshot = DepGraphSnapshot::new(vec![entry("json", &[])]);
        assert_eq!(
            names(&resolved_dependency_names(
                &snapshot,
                &declared(&["json", "fresh"])
            )),
            ["fresh", "json"]
        );
    }

    #[test]
    fn declared_names_are_the_dependency_table_keys() {
        let manifest = "[package]\nname = \"app\"\n\n[dependencies]\n\
                        utils = { path = \"../utils\" }\n\
                        json = { git = \"https://example.test/json\", tag = \"v1\" }\n";
        let mut names = declared_dependency_names_from_manifest_toml(manifest);
        names.sort();
        assert_eq!(names, ["json", "utils"]);
        assert!(
            declared_dependency_names_from_manifest_toml("[package]\nname = \"a\"\n").is_empty()
        );
        assert!(declared_dependency_names_from_manifest_toml("not = [toml").is_empty());
    }

    #[test]
    fn snapshot_round_trips_and_rejects_bad_files() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(DEP_GRAPH_SNAPSHOT_FILE);
        assert_eq!(
            read_dep_graph_snapshot(&path),
            Err(DepGraphSnapshotError::Missing)
        );

        let snapshot = DepGraphSnapshot::new(vec![entry("b", &["a"]), entry("a", &[])]);
        std::fs::write(&path, serde_json::to_string_pretty(&snapshot).unwrap()).unwrap();
        let read = read_dep_graph_snapshot(&path).unwrap();
        assert_eq!(read, snapshot);
        assert_eq!(read.deps[0].name, "a", "entries are ordered by name");

        std::fs::write(&path, "not json").unwrap();
        assert_eq!(
            read_dep_graph_snapshot(&path),
            Err(DepGraphSnapshotError::Corrupt)
        );

        std::fs::write(&path, r#"{"schema": 99, "deps": []}"#).unwrap();
        assert_eq!(
            read_dep_graph_snapshot(&path),
            Err(DepGraphSnapshotError::UnrecognisedSchema(99))
        );
    }
}
