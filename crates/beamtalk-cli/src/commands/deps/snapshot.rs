// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dependency-graph snapshot for structural freshness detection (BT-3009).
//!
//! **DDD Context:** Build System
//!
//! The freshness heuristics in [`super::deps_are_fresh`] are all *value*
//! comparisons: mtimes, ebin presence, provenance stamps, and (BT-2994) each
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
//! graph* (every discovered dep's name, declaring chain, and declared source)
//! after each successful resolve, and compare the current graph against it on
//! the next build. That is generic: it catches source-type swaps, a path dep's
//! declared path moving, and any add/remove/restructure anywhere in the
//! transitive graph, without adding yet another mtime heuristic.
//!
//! The snapshot lives under `_build/` (gitignored) alongside the ADR 0098
//! provenance stamps, and follows the same "fail toward rebuild" rule: a
//! missing, corrupt, or unrecognised-schema snapshot is a miss, so the first
//! build after this ships re-resolves once and then records a baseline.

use camino::Utf8Path;
use serde::{Deserialize, Serialize};
use std::fs;
use tracing::{debug, warn};

use super::DiscoveredDep;
use crate::commands::build_layout::BuildLayout;

/// Current snapshot schema. Bump when the meaning of [`DepGraphEntry`]
/// changes; an unrecognised schema is treated as a miss (re-resolve), so a
/// newer toolchain's snapshot never lets an older binary reuse artifacts it
/// cannot reason about.
const SNAPSHOT_SCHEMA: u32 = 1;

/// One dependency as it was declared the last time the graph resolved.
///
/// `source` is [`beamtalk_core::compilation::DependencySource`]'s `Display`
/// form (`path: ../utils`, `git: <url> (tag: v1.0.0)`, `registry: 0.2.1`), so
/// a type swap, a URL change, a tag bump, or a moved path all show up as a
/// different string — and the file stays readable when debugging a rebuild.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct DepGraphEntry {
    /// The dependency's package name.
    name: String,
    /// The chain of intermediate dependency names it was reached through
    /// (empty for a dependency the root manifest declares directly).
    #[serde(default)]
    via: Vec<String>,
    /// How the declaring manifest asked for it.
    source: String,
}

/// On-disk snapshot of the effective dependency graph.
#[derive(Debug, Serialize, Deserialize)]
struct DepGraphSnapshot {
    /// Snapshot schema version. An unrecognised value is a miss.
    schema: u32,
    /// Every dependency in the transitive graph, ordered by name.
    deps: Vec<DepGraphEntry>,
}

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

/// Project the discovered graph into its comparable form, ordered by name.
///
/// Dependency names are unique across the graph (`discover_all_dep_roots`
/// de-duplicates by name), so sorting by name is a total order and makes the
/// comparison independent of discovery order.
fn entries_for(all_deps: &[DiscoveredDep]) -> Vec<DepGraphEntry> {
    let mut entries: Vec<DepGraphEntry> = all_deps
        .iter()
        .map(|dep| DepGraphEntry {
            name: dep.name.clone(),
            via: dep.via_chain.clone(),
            source: dep.source.to_string(),
        })
        .collect();
    entries.sort_by(|a, b| a.name.cmp(&b.name));
    entries
}

/// Compare the currently discovered graph against the recorded snapshot.
///
/// Returns [`SnapshotStatus::Stale`] when there is no snapshot to compare
/// against — the same "fail toward rebuild" stance ADR 0098 takes for a
/// missing provenance stamp.
pub(super) fn check(project_root: &Utf8Path, all_deps: &[DiscoveredDep]) -> SnapshotStatus {
    let path = snapshot_path(project_root);

    let Ok(data) = fs::read_to_string(&path) else {
        return SnapshotStatus::Stale("no dependency-graph snapshot".to_string());
    };

    let snapshot: DepGraphSnapshot = match serde_json::from_str(&data) {
        Ok(s) => s,
        Err(_) => return SnapshotStatus::Stale("corrupt dependency-graph snapshot".to_string()),
    };

    if snapshot.schema != SNAPSHOT_SCHEMA {
        return SnapshotStatus::Stale(format!(
            "unrecognised dependency-graph schema {} (current {SNAPSHOT_SCHEMA})",
            snapshot.schema
        ));
    }

    let current = entries_for(all_deps);
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
    let snapshot = DepGraphSnapshot {
        schema: SNAPSHOT_SCHEMA,
        deps: entries_for(all_deps),
    };

    let data = match serde_json::to_string_pretty(&snapshot) {
        Ok(d) => d,
        Err(e) => {
            warn!(error = %e, "Failed to serialise dependency-graph snapshot");
            return;
        }
    };

    let path = snapshot_path(project_root);
    match write_atomic(&path, &data) {
        Ok(()) => debug!("Wrote dependency-graph snapshot to {path}"),
        Err(e) => warn!(error = %e, "Failed to write dependency-graph snapshot to {path}"),
    }
}

/// Write `contents` to `path` atomically: stage in a sibling temp file, then
/// rename into place (atomic on the same filesystem). The temp name is keyed
/// by pid so concurrent builders don't clobber each other's staging file.
fn write_atomic(path: &Utf8Path, contents: &str) -> std::io::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!(".beamtalk-dep-graph.{}.tmp", std::process::id()));
    fs::write(&tmp, contents)?;
    fs::rename(&tmp, path)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(name: &str, source: &str) -> DepGraphEntry {
        DepGraphEntry {
            name: name.to_string(),
            via: Vec::new(),
            source: source.to_string(),
        }
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
            name: "yaml".to_string(),
            via: vec!["utils".to_string()],
            source: "path: ../yaml".to_string(),
        }];
        let current = vec![entry("yaml", "path: ../yaml")];

        assert!(describe_difference(&recorded, &current).contains("different chain"));
    }
}
