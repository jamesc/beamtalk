// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! JSON view models sent to the picker's frontend over Tauri's `invoke`/
//! event bridge. Kept separate from `beamtalk-desktop-broker`'s own types
//! (which deliberately don't derive `Serialize` — that crate has no reason
//! to know about JSON wire shapes for a UI it doesn't build) — this module
//! is the one place that translation happens.

use serde::Serialize;

use beamtalk_desktop_broker::discovery::WorkspaceSummary;
use beamtalk_desktop_broker::monitor::ConnectionState;
use beamtalk_desktop_broker::readiness::FailureReason;
use beamtalk_desktop_shell::empty_state::PickerEmptyState;

#[derive(Debug, Clone, Serialize)]
pub struct WorkspaceView {
    pub id: String,
    pub project_path: Option<String>,
    pub alive: bool,
    pub attached: bool,
}

impl WorkspaceView {
    pub fn from_summary(summary: &WorkspaceSummary, attached: bool) -> Self {
        Self {
            id: summary.id.clone(),
            project_path: summary
                .project_path
                .as_ref()
                .map(|p| p.display().to_string()),
            alive: summary.alive,
            attached,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum EmptyStateView {
    NotEmpty,
    CliFound { cli_path: String },
    CliMissing,
}

impl From<&PickerEmptyState> for EmptyStateView {
    fn from(state: &PickerEmptyState) -> Self {
        match state {
            PickerEmptyState::NotEmpty => Self::NotEmpty,
            PickerEmptyState::NoWorkspacesCliFound { cli_path } => Self::CliFound {
                cli_path: cli_path.display().to_string(),
            },
            PickerEmptyState::NoWorkspacesCliMissing => Self::CliMissing,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct PickerView {
    pub workspaces: Vec<WorkspaceView>,
    pub empty_state: EmptyStateView,
}

/// JSON-friendly failure taxonomy, mirroring
/// `beamtalk_desktop_broker::readiness::FailureReason` (which doesn't derive
/// `Serialize` itself — see module docs above).
#[must_use]
pub fn failure_reason_str(reason: &FailureReason) -> String {
    match reason {
        FailureReason::EpmdAbsent => "epmd_absent".to_string(),
        FailureReason::BadCookie => "bad_cookie".to_string(),
        FailureReason::DeadWorkspace => "dead_workspace".to_string(),
        FailureReason::Unknown(s) => s.clone(),
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ConnectionStateView {
    Connected,
    Disconnected { reason: String },
    FrontUnreachable,
}

impl From<&ConnectionState> for ConnectionStateView {
    fn from(state: &ConnectionState) -> Self {
        match state {
            ConnectionState::Connected(_) => Self::Connected,
            ConnectionState::Disconnected(reason) => Self::Disconnected {
                reason: failure_reason_str(reason),
            },
            ConnectionState::FrontUnreachable => Self::FrontUnreachable,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ConnectionStateChangedEvent {
    pub workspace_id: String,
    pub state: ConnectionStateView,
}

#[derive(Debug, Clone, Serialize)]
pub struct AttachProgressEvent {
    pub workspace_id: String,
    pub stage: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AttachOutcome {
    Opened,
    Focused,
}
