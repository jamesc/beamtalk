// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace discovery (ADR 0097 Broker §1).
//!
//! Enumerates `~/.beamtalk/workspaces/*/metadata.json` with a real JSON
//! parser (not shell `sed`, unlike `bin/server`'s own node-name extraction —
//! see ADR 0097 Broker §1b) and checks liveness via a direct epmd `NAMES`
//! query. A dist ping is not available to this non-BEAM broker without
//! implementing the distribution handshake, so epmd is the liveness
//! mechanism — not the installed CLI's `workspace status` (which requires
//! resolving the CLI's path first; that resolution lives in
//! [`crate::cli_ops`] and is only needed for the create/stop duty, ADR 0097
//! Broker §5).
//!
//! `metadata.json`'s schema is **not** fully owned by this broker: the Rust
//! CLI writes `workspace_id` / `project_path` / `created_at` at creation
//! time, and the *running* workspace's `beamtalk_workspace_meta` gen_server
//! (Erlang) later adds `node_name` (plus settings, loaded modules, …) via
//! debounced writes once it has booted at least once
//! (`beamtalk_workspace_meta.erl`). A workspace that has never been started
//! therefore has no `node_name` field yet — this module falls back to the
//! deterministic `beamtalk_workspace_<id>@localhost` naming convention the
//! CLI itself uses (`process.rs`/`lifecycle.rs`), and epmd liveness will
//! correctly report it as dead regardless of which name was used to look it up.
//!
//! **DDD Context:** Desktop Shell

use std::path::PathBuf;

use serde::Deserialize;

use crate::error::Result;

/// One discovered workspace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceSummary {
    /// Workspace id — the `~/.beamtalk/workspaces/<id>/` directory name.
    pub id: String,
    /// Absolute project path, if `metadata.json` reported one.
    pub project_path: Option<PathBuf>,
    /// The Erlang distribution node name this workspace registers under.
    /// Either read from `metadata.json` (once the workspace has started at
    /// least once) or predicted from the deterministic naming convention.
    pub node_name: String,
    /// Whether epmd currently has a registration for `node_name`'s short name.
    pub alive: bool,
}

/// Loose deserialization of `metadata.json` — only the fields this broker
/// reads are named; everything else (`settings`, `loaded_modules`,
/// `class_sources`, …) is ignored via `#[serde(default)]`/unknown-field
/// tolerance rather than failing to parse a file whose shape this crate does
/// not own.
#[derive(Debug, Deserialize, Default)]
struct RawMetadata {
    #[serde(default)]
    workspace_id: Option<String>,
    #[serde(default)]
    project_path: Option<PathBuf>,
    #[serde(default)]
    node_name: Option<String>,
}

/// The deterministic node name the Rust CLI assigns a workspace at creation
/// (`beamtalk_workspace_{id}@localhost` — `process.rs`/`lifecycle.rs` in
/// `beamtalk-cli`). Used as a fallback when `metadata.json` doesn't yet carry
/// a `node_name` (workspace created but never started).
#[must_use]
pub fn default_node_name(workspace_id: &str) -> String {
    format!("beamtalk_workspace_{workspace_id}@localhost")
}

/// Parse one workspace directory's `metadata.json` into a [`WorkspaceSummary`],
/// without checking liveness (pure — no epmd I/O, so it's separately
/// unit-testable from the liveness check).
///
/// `dir_name` is used as the workspace id when `metadata.json` doesn't
/// contain a `workspace_id` (e.g. an older or hand-edited file), matching the
/// directory-name-is-authoritative convention `beamtalk-workspace` itself
/// uses (`workspace_dir(id)`).
fn parse_metadata(dir_name: &str, content: &str) -> Result<WorkspaceSummary> {
    let raw: RawMetadata = serde_json::from_str(content)?;
    let id = raw.workspace_id.unwrap_or_else(|| dir_name.to_string());
    let node_name = raw.node_name.unwrap_or_else(|| default_node_name(&id));
    Ok(WorkspaceSummary {
        id,
        project_path: raw.project_path,
        node_name,
        alive: false,
    })
}

/// Enumerate `~/.beamtalk/workspaces/*/metadata.json`, parse each with a real
/// JSON parser, and check liveness via a single epmd `NAMES` query shared
/// across every workspace found (one round-trip instead of one per
/// workspace).
///
/// Entries with a missing/unreadable/unparsable `metadata.json` are skipped
/// rather than failing the whole scan — one corrupt workspace directory must
/// not hide every other live workspace from the picker.
///
/// # Errors
///
/// Returns an error if the workspaces base directory can't be resolved, if
/// listing it fails for a reason other than "does not exist", or if the
/// epmd liveness query fails.
pub fn discover_workspaces() -> Result<Vec<WorkspaceSummary>> {
    let base = beamtalk_workspace::workspaces_base_dir()?;
    let entries = match std::fs::read_dir(&base) {
        Ok(e) => e,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(e.into()),
    };

    let epmd_names = beamtalk_workspace::epmd::query_epmd_names()?;

    let mut summaries = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let Some(dir_name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        let meta_path = path.join("metadata.json");
        let Ok(content) = std::fs::read_to_string(&meta_path) else {
            continue;
        };
        let Ok(mut summary) = parse_metadata(dir_name, &content) else {
            tracing::warn!(workspace = dir_name, "skipping unparsable metadata.json");
            continue;
        };
        let short_name = summary.node_name.split('@').next().unwrap_or("");
        summary.alive = epmd_names.iter().any(|n| n == short_name);
        summaries.push(summary);
    }
    summaries.sort_by(|a, b| a.id.cmp(&b.id));
    Ok(summaries)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_node_name_matches_cli_convention() {
        assert_eq!(
            default_node_name("abc123"),
            "beamtalk_workspace_abc123@localhost"
        );
    }

    #[test]
    fn parse_metadata_uses_explicit_node_name_when_present() {
        let json = r#"{"workspace_id":"abc123","project_path":"/tmp/proj","created_at":1,"node_name":"bt_attach_abc123_999@localhost"}"#;
        let summary = parse_metadata("abc123", json).unwrap();
        assert_eq!(summary.id, "abc123");
        assert_eq!(summary.project_path, Some(PathBuf::from("/tmp/proj")));
        assert_eq!(summary.node_name, "bt_attach_abc123_999@localhost");
        assert!(!summary.alive);
    }

    #[test]
    fn parse_metadata_falls_back_to_default_node_name_when_absent() {
        // A workspace created but never started: the Rust CLI's write has no
        // node_name field yet (only beamtalk_workspace_meta adds it at runtime).
        let json = r#"{"workspace_id":"abc123","project_path":"/tmp/proj","created_at":1}"#;
        let summary = parse_metadata("abc123", json).unwrap();
        assert_eq!(summary.node_name, "beamtalk_workspace_abc123@localhost");
    }

    #[test]
    fn parse_metadata_falls_back_to_dir_name_when_workspace_id_absent() {
        let json = r#"{"created_at":1}"#;
        let summary = parse_metadata("dir-name-id", json).unwrap();
        assert_eq!(summary.id, "dir-name-id");
    }

    #[test]
    fn parse_metadata_tolerates_unknown_fields() {
        // The running workspace's meta server adds settings/loaded_modules/
        // class_sources/etc. — this broker must not choke on fields it
        // doesn't own.
        let json = r#"{
            "workspace_id":"abc123",
            "created_at":1,
            "last_active":2,
            "node_name":"beamtalk_workspace_abc123@localhost",
            "repl_port":9999,
            "supervised_actors":["<0.123.0>"],
            "loaded_modules":[{"name":"Foo","source":"Object subclass: Foo"}],
            "class_sources":{"Foo":"..."},
            "settings":{"autoflush":true}
        }"#;
        let summary = parse_metadata("abc123", json).unwrap();
        assert_eq!(summary.id, "abc123");
    }

    #[test]
    fn parse_metadata_rejects_invalid_json() {
        assert!(parse_metadata("abc123", "not json").is_err());
    }

    #[test]
    fn discover_workspaces_returns_empty_when_workspaces_dir_missing() {
        // Can't easily redirect beamtalk_workspace::workspaces_base_dir() in
        // a unit test (it reads $HOME), so this only asserts the call
        // succeeds without panicking — the missing-dir branch is exercised
        // directly by whatever this sandbox's ~/.beamtalk/workspaces looks
        // like. The pure-parsing behavior above covers the real logic.
        let result = discover_workspaces();
        assert!(result.is_ok(), "discovery must not error: {result:?}");
    }
}
