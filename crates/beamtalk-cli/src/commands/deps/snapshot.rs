// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dependency-graph snapshot for structural freshness detection.
//!
//! **DDD Context:** Build System
//!
//! The freshness heuristics in [`super::deps_are_fresh`] are all *value*
//! comparisons: mtimes, ebin presence, provenance stamps, and each
//! git/registry dep's locked version or reference. None of them notice a
//! **structural** change to the dependency graph — one where a dependency's
//! declared *source type* changes while its name stays put.
//!
//! The motivating case: the root manifest declares only
//! `utils = { path = "../utils" }`, and `utils/beamtalk.toml` swaps
//! `yaml = { git = "…", tag = "v1" }` for `yaml = { path = "../yaml" }`.
//! Afterwards:
//!
//! * The root manifest's mtime is unchanged, so the lockfile mtime guard stays
//!   silent.
//! * `locked_deps_match` only inspects deps that are *currently* declared as
//!   git/registry, so `yaml` — now a path dep — is never compared against its
//!   stale lock entry. (The reverse swap, path → git, *is* caught: the newly
//!   git dep misses in the lockfile.)
//! * `_build/deps/yaml/ebin/` still holds the old git checkout's `.beam`
//!   files, so the ebin-presence check is satisfied too.
//!
//! Net effect without this module: the build silently links against artifacts
//! compiled from a source that is no longer declared anywhere.
//!
//! The fix is direction (b) from the issue — record the *effective dependency
//! graph* (every discovered dep's name, declaring chain, own direct
//! dependencies, and declared source)
//! after each successful resolve, and compare the current graph against it on
//! the next build. That is generic: it catches source-type swaps, a path dep's
//! declared path moving, and any add/remove/restructure anywhere in the
//! transitive graph, without adding yet another mtime heuristic.
//!
//! The snapshot lives under `_build/` (gitignored) alongside the ADR 0098
//! provenance stamps, and follows the same "fail toward rebuild" rule: a
//! missing, corrupt, or unrecognised-schema snapshot is a miss, so the first
//! build after this ships re-resolves once and then records a baseline.
//!
//! The on-disk schema and its reader live in
//! [`beamtalk_core::compilation::dep_graph`], which the compiler port also
//! reads to learn a project's resolved dependency set (BT-4378).

use beamtalk_core::compilation::dep_graph::{
    DepGraphEntry, DepGraphSnapshot, read_dep_graph_snapshot,
};
use camino::Utf8Path;
use tracing::{debug, warn};

use crate::commands::util::write_atomic;

use super::DiscoveredDep;
use crate::commands::build_layout::BuildLayout;

/// Result of comparing the current dependency graph against the recorded one.
pub(super) enum SnapshotStatus {
    /// The graph is structurally identical to the last successful resolve.
    Fresh,
    /// No usable snapshot, or the graph changed shape. Carries a
    /// human-readable reason for the build log.
    Stale(String),
}

/// Project-relative location of the snapshot: `_build/deps/.dep-graph.json`.
fn snapshot_path(project_root: &Utf8Path) -> camino::Utf8PathBuf {
    BuildLayout::new(project_root).dep_graph_path()
}

/// Project the discovered graph into its comparable form, ordered by name
/// (see [`DepGraphSnapshot::new`]).
fn snapshot_for(all_deps: &[DiscoveredDep]) -> DepGraphSnapshot {
    DepGraphSnapshot::new(
        all_deps
            .iter()
            .map(|dep| {
                let mut dependencies = dep.deps.clone();
                dependencies.sort();
                dependencies.dedup();
                DepGraphEntry {
                    name: dep.name.clone(),
                    via: dep.via_chain.clone(),
                    dependencies,
                    source: dep.source.to_string(),
                }
            })
            .collect(),
    )
}

/// Compare the currently discovered graph against the recorded snapshot.
///
/// Returns [`SnapshotStatus::Stale`] when there is no snapshot to compare
/// against — the same "fail toward rebuild" stance ADR 0098 takes for a
/// missing provenance stamp.
pub(super) fn check(project_root: &Utf8Path, all_deps: &[DiscoveredDep]) -> SnapshotStatus {
    let snapshot = match read_dep_graph_snapshot(snapshot_path(project_root).as_std_path()) {
        Ok(snapshot) => snapshot,
        Err(reason) => return SnapshotStatus::Stale(reason.to_string()),
    };

    let current = snapshot_for(all_deps).deps;
    if current == snapshot.deps {
        return SnapshotStatus::Fresh;
    }

    SnapshotStatus::Stale(describe_difference(&snapshot.deps, &current))
}

/// Build a short human-readable summary of how the graph changed, for the
/// build log. Reports the first difference found, scanning by name.
fn describe_difference(recorded: &[DepGraphEntry], current: &[DepGraphEntry]) -> String {
    for entry in current {
        match recorded.iter().find(|r| r.name == entry.name) {
            Some(previous) if previous.source != entry.source => {
                return format!(
                    "dependency '{}' changed source: was '{}', now '{}'",
                    entry.name, previous.source, entry.source
                );
            }
            Some(previous) if previous.via != entry.via => {
                return format!(
                    "dependency '{}' is now reached through a different chain",
                    entry.name
                );
            }
            Some(previous) if previous.dependencies != entry.dependencies => {
                return format!(
                    "dependency '{}' now declares different dependencies",
                    entry.name
                );
            }
            Some(_) => {}
            None => return format!("dependency '{}' is newly declared", entry.name),
        }
    }

    for entry in recorded {
        if !current.iter().any(|c| c.name == entry.name) {
            return format!("dependency '{}' is no longer declared", entry.name);
        }
    }

    "dependency graph changed".to_string()
}

/// Record the current dependency graph as the baseline for future freshness
/// checks.
///
/// Called **after** a successful resolve, so every checkout and transitive
/// manifest is on disk and discovery sees the complete graph. Best-effort: an
/// I/O or serialisation failure is logged but never fails the build (the next
/// build simply sees a missing snapshot and re-resolves).
pub(super) fn write(project_root: &Utf8Path, all_deps: &[DiscoveredDep]) {
    let snapshot = snapshot_for(all_deps);

    let data = match serde_json::to_string_pretty(&snapshot) {
        Ok(d) => d,
        Err(e) => {
            warn!(error = %e, "Failed to serialise dependency-graph snapshot");
            return;
        }
    };

    let path = snapshot_path(project_root);
    match write_atomic(&path, &data, ".beamtalk-dep-graph.") {
        Ok(()) => debug!("Wrote dependency-graph snapshot to {path}"),
        Err(e) => warn!(error = %e, "Failed to write dependency-graph snapshot to {path}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(name: &str, source: &str) -> DepGraphEntry {
        DepGraphEntry {
            name: name.to_string(),
            via: Vec::new(),
            dependencies: Vec::new(),
            source: source.to_string(),
        }
    }

    #[test]
    fn test_describe_difference_reports_changed_dependencies() {
        // A diamond edge (`b` now also declares `c`, already reached through
        // `a`) changes no `via` chain, only `b`'s recorded edges (BT-4394).
        let recorded = vec![entry("b", "path: ../b")];
        let current = vec![DepGraphEntry {
            dependencies: vec!["c".to_string()],
            ..entry("b", "path: ../b")
        }];

        assert!(describe_difference(&recorded, &current).contains("different dependencies"));
    }

    #[test]
    fn test_describe_difference_reports_source_swap() {
        let recorded = vec![entry(
            "yaml",
            "git: https://example.test/yaml (tag: v1.0.0)",
        )];
        let current = vec![entry("yaml", "path: ../yaml")];

        let message = describe_difference(&recorded, &current);
        assert!(message.contains("yaml"), "{message}");
        assert!(message.contains("changed source"), "{message}");
    }

    #[test]
    fn test_describe_difference_reports_added_and_removed() {
        let recorded = vec![entry("yaml", "path: ../yaml")];
        let current = vec![entry("json", "path: ../json")];

        assert!(describe_difference(&recorded, &current).contains("newly declared"));
        assert!(describe_difference(&current, &recorded).contains("newly declared"));

        let both = vec![
            entry("yaml", "path: ../yaml"),
            entry("json", "path: ../json"),
        ];
        assert!(describe_difference(&both, &recorded).contains("no longer declared"));
    }

    #[test]
    fn test_describe_difference_reports_changed_chain() {
        let recorded = vec![DepGraphEntry {
            via: vec!["utils".to_string()],
            ..entry("yaml", "path: ../yaml")
        }];
        let current = vec![entry("yaml", "path: ../yaml")];

        assert!(describe_difference(&recorded, &current).contains("different chain"));
    }
}
