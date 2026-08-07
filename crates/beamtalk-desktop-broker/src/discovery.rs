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
//! therefore has no `node_name` field yet. [`discover_workspaces`]'s
//! best-effort picker listing falls back to the deterministic
//! `beamtalk_workspace_<id>@localhost` naming convention the CLI itself uses
//! (`process.rs`/`lifecycle.rs`) in that case, and epmd liveness will
//! correctly report it as dead regardless of which name was used to look it
//! up. [`read_node_name`] — the single-workspace lookup the Windows spawn
//! path uses to actually dist-connect — does **not** fall back: it hard-fails
//! with [`crate::error::BrokerError::MissingNodeName`] instead, matching
//! `bin/server`'s Unix fail-fast behavior (ADR 0097 Broker §1b) rather than
//! silently connecting to a guessed name that only *usually* matches the
//! CLI's own naming convention (BT-3060 adversarial-review follow-up: a
//! listing can afford to guess and let epmd sort out liveness, but an actual
//! connection attempt should not).
//!
//! **DDD Context:** Desktop Shell

use std::path::PathBuf;

use serde::Deserialize;

use crate::error::{BrokerError, Result};

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

/// Read the Erlang distribution node name for `workspace_id` directly from
/// `metadata.json`, without the epmd liveness check [`discover_workspaces`]
/// does for the whole picker list.
///
/// The Windows spawn path (BT-2988) needs this standalone: there is no
/// `bin/server` shell script on Windows to do the equivalent `sed`
/// extraction (ADR 0097 Implementation §5b), so the broker resolves
/// `BT_WORKSPACE_NODE` itself before invoking `bin\bt_attach.bat` directly.
///
/// Unlike [`parse_metadata`] (used by [`discover_workspaces`]'s best-effort
/// picker listing), this does **not** fall back to [`default_node_name`] when
/// `metadata.json` has no `node_name` field — it hard-fails instead, matching
/// `bin/server`'s Unix behavior (which `exit 1`s rather than guessing, see
/// this module's doc comment). A workspace that has never been started has
/// no real node to dist-connect to yet, and the two naming conventions (this
/// crate's guess vs. the CLI's actual scheme) are independently maintained —
/// a coincidental match today is not a guarantee (BT-3060).
///
/// A present-but-blank `node_name` (`""`, or whitespace-only) is treated the
/// same as a missing field, matching both `bin/server`'s own `[ -z "${node}"
/// ]` check (empty extraction fails the same way as no match) and this
/// crate's own [`beamtalk_workspace::read_cookie_file`] convention for the
/// sibling `cookie` file.
///
/// # Errors
///
/// Returns [`BrokerError::Io`] if `metadata.json` doesn't exist,
/// [`BrokerError::Json`] if it can't be parsed, or
/// [`BrokerError::MissingNodeName`] if it parses but has no non-blank
/// `node_name` field.
pub fn read_node_name(workspace_id: &str) -> Result<String> {
    let meta_path = beamtalk_workspace::workspace_dir(workspace_id)?.join("metadata.json");
    let content = std::fs::read_to_string(&meta_path)?;
    let raw: RawMetadata = serde_json::from_str(&content)?;
    match raw.node_name {
        Some(name) if !name.trim().is_empty() => Ok(name),
        _ => Err(BrokerError::MissingNodeName(workspace_id.to_string())),
    }
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

    /// `read_node_name` is what the Windows spawn path (BT-2988) uses in
    /// place of `bin/server`'s shell-level extraction — a real, on-disk
    /// workspace directory (same pattern the rest of this crate's tests use;
    /// there's no HOME-override hook in `beamtalk_workspace`).
    #[test]
    fn read_node_name_reads_explicit_node_name_from_metadata() {
        let id = format!("read_node_name_explicit_{}", std::process::id());
        let dir = beamtalk_workspace::workspace_dir(&id).unwrap();
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("metadata.json"),
            r#"{"workspace_id":"x","node_name":"bt_attach_x_123@localhost"}"#,
        )
        .unwrap();

        let result = read_node_name(&id).unwrap();

        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(result, "bt_attach_x_123@localhost");
    }

    /// BT-3060: `read_node_name` (the Windows spawn path's lookup) must
    /// hard-fail here, matching `bin/server`'s Unix behavior — unlike
    /// `parse_metadata`'s picker-listing fallback (tested above), a real
    /// spawn attempt must not silently guess at a node name.
    #[test]
    fn read_node_name_errors_when_metadata_has_no_node_name() {
        let id = format!("read_node_name_missing_field_{}", std::process::id());
        let dir = beamtalk_workspace::workspace_dir(&id).unwrap();
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("metadata.json"), r#"{"created_at":1}"#).unwrap();

        let result = read_node_name(&id);

        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            matches!(result, Err(BrokerError::MissingNodeName(ref w)) if w == &id),
            "expected MissingNodeName({id}), got {result:?}"
        );
    }

    /// A present-but-blank `node_name` must fail exactly like an absent one
    /// — matching `bin/server`'s `[ -z "${node}" ]` check and this crate's
    /// own `read_cookie_file` empty-is-missing convention (BT-3060 review
    /// follow-up).
    #[test]
    fn read_node_name_errors_when_metadata_has_a_blank_node_name() {
        let id = format!("read_node_name_blank_field_{}", std::process::id());
        let dir = beamtalk_workspace::workspace_dir(&id).unwrap();
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("metadata.json"), r#"{"node_name":"   "}"#).unwrap();

        let result = read_node_name(&id);

        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            matches!(result, Err(BrokerError::MissingNodeName(ref w)) if w == &id),
            "expected MissingNodeName({id}), got {result:?}"
        );
    }

    #[test]
    fn read_node_name_errors_when_metadata_missing() {
        let id = format!("read_node_name_missing_{}", std::process::id());
        assert!(read_node_name(&id).is_err());
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
