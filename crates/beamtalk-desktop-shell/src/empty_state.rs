// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! First-run empty-state classification (ADR 0097 Broker §5, User Impact:
//! "First run with no workspaces is a real state").
//!
//! Zero discovered workspaces is a designed state, not an accident: the
//! picker must offer a "create a workspace" action (shelling out to the
//! installed `beamtalk` CLI, [`beamtalk_desktop_broker::cli_ops`]) when the
//! CLI can be resolved, or setup instructions when it can't — "never a
//! silent empty list" (ADR 0097). [`classify_empty_state`] is the pure
//! decision: given how many workspaces were discovered and a way to resolve
//! the CLI, which of the three states should the picker render.
//!
//! **DDD Context:** Desktop Shell

use std::path::PathBuf;

use beamtalk_desktop_broker::Result;

/// What the picker should render for its top-level "workspace list vs empty
/// state" decision.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PickerEmptyState {
    /// At least one workspace was discovered — render the list, not an
    /// empty state.
    NotEmpty,
    /// Zero workspaces, and the `beamtalk` CLI was resolved: offer
    /// "create a workspace" via `cli_path`.
    NoWorkspacesCliFound { cli_path: PathBuf },
    /// Zero workspaces, and the CLI could not be resolved (GUI apps don't
    /// inherit the shell `PATH` — ADR 0097 Broker §1a): show setup
    /// instructions instead of a create-workspace button.
    NoWorkspacesCliMissing,
}

/// Classify the picker's empty-state, given `workspace_count` (from
/// [`beamtalk_desktop_broker::discovery::discover_workspaces`]) and
/// `cli_lookup` (typically [`beamtalk_desktop_broker::cli_ops::resolve_cli_path`],
/// injected so this stays unit-testable without touching the real `PATH`/
/// filesystem — the same injection style
/// [`beamtalk_desktop_broker`]'s own tests use).
///
/// `cli_lookup` is only called when `workspace_count == 0` — a non-empty
/// list never needs to know whether the CLI is resolvable.
#[must_use]
pub fn classify_empty_state(
    workspace_count: usize,
    cli_lookup: impl FnOnce() -> Result<PathBuf>,
) -> PickerEmptyState {
    if workspace_count > 0 {
        return PickerEmptyState::NotEmpty;
    }
    match cli_lookup() {
        Ok(cli_path) => PickerEmptyState::NoWorkspacesCliFound { cli_path },
        Err(_) => PickerEmptyState::NoWorkspacesCliMissing,
    }
}

/// Validate a workspace id typed into the empty state's "create a workspace"
/// text field before it reaches [`beamtalk_desktop_broker::cli_ops::create_workspace`],
/// which passes it positionally to the installed `beamtalk` CLI
/// (`beamtalk workspace create <id> --background --persistent`).
///
/// This is defense in depth, not a duplicate of `beamtalk-cli`'s own
/// `validate_workspace_name` (`crates/beamtalk-cli/src/commands/workspace/storage.rs`):
/// that function checks the same charset but is only ever reached *after*
/// `clap` has already resolved a token as the positional `name` argument —
/// it never sees a value `clap` would instead interpret as a flag. The
/// picker's text field has no such gate: a user can type anything,
/// including a leading `-` (e.g. pasting `--persistent` or `-rf`), and this
/// crate — not `clap` — is the first thing that sees the raw string. Reject
/// it here rather than letting an ambiguous positional argument reach the
/// CLI subprocess at all.
///
/// # Errors
///
/// Returns `Err` with a user-facing message if `id` is empty, starts with
/// `-`, or contains any character other than an ASCII letter, digit, `-`,
/// or `_`.
pub fn validate_new_workspace_id(id: &str) -> std::result::Result<(), String> {
    if id.is_empty() {
        return Err("Workspace name cannot be empty".to_string());
    }
    if id.starts_with('-') {
        return Err("Workspace name cannot start with '-'".to_string());
    }
    if !id
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
    {
        return Err("Workspace name must contain only letters, numbers, '-' or '_'".to_string());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_desktop_broker::BrokerError;

    #[test]
    fn not_empty_short_circuits_before_resolving_the_cli() {
        // A non-empty workspace list never needs to know whether the CLI is
        // resolvable — the closure must not even be called.
        let state = classify_empty_state(3, || panic!("cli_lookup must not be called"));
        assert_eq!(state, PickerEmptyState::NotEmpty);
    }

    #[test]
    fn zero_workspaces_with_cli_found_offers_create() {
        let cli_path = PathBuf::from("/usr/local/bin/beamtalk");
        let state = classify_empty_state(0, || Ok(cli_path.clone()));
        assert_eq!(state, PickerEmptyState::NoWorkspacesCliFound { cli_path });
    }

    #[test]
    fn zero_workspaces_with_cli_missing_shows_setup_instructions() {
        let state = classify_empty_state(0, || Err(BrokerError::CliNotFound));
        assert_eq!(state, PickerEmptyState::NoWorkspacesCliMissing);
    }

    #[test]
    fn validate_new_workspace_id_accepts_sane_names() {
        assert!(validate_new_workspace_id("myws").is_ok());
        assert!(validate_new_workspace_id("my-ws").is_ok());
        assert!(validate_new_workspace_id("my_ws").is_ok());
        assert!(validate_new_workspace_id("my-ws_123").is_ok());
        assert!(validate_new_workspace_id("a").is_ok());
        assert!(validate_new_workspace_id("12345").is_ok());
    }

    #[test]
    fn validate_new_workspace_id_rejects_empty() {
        assert!(validate_new_workspace_id("").is_err());
    }

    #[test]
    fn validate_new_workspace_id_rejects_a_leading_hyphen() {
        // The exact ambiguity this function exists to catch before it
        // reaches `clap` on the CLI side: a leading `-` reads as a flag, not
        // a positional workspace name — e.g. pasting `--persistent` here
        // must not silently pass a flag-shaped string through to the CLI
        // subprocess as the `create` command's positional id.
        assert!(validate_new_workspace_id("-rf").is_err());
        assert!(validate_new_workspace_id("--persistent").is_err());
        assert!(validate_new_workspace_id("-").is_err());
    }

    #[test]
    fn validate_new_workspace_id_rejects_disallowed_characters() {
        assert!(validate_new_workspace_id("has space").is_err());
        assert!(validate_new_workspace_id("has.dot").is_err());
        assert!(validate_new_workspace_id("has/slash").is_err());
        assert!(validate_new_workspace_id("ws!@#").is_err());
        assert!(validate_new_workspace_id("workspäce").is_err());
    }
}
