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
}
