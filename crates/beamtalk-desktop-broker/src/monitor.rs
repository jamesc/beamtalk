// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Post-attach monitoring (ADR 0097 Broker §3).
//!
//! The two-stage [`crate::readiness`] probe runs once, before the window
//! opens. After that, attachment health isn't free: the front stays up even
//! when the *workspace* dies (`beamtalk workspace stop` doesn't crash the
//! front — its RPCs just start returning `{:badrpc, :nodedown}`), so a
//! polling broker is the only way to notice and grey the window instead of
//! leaving it silently hung. This module periodically re-probes `/readiness`
//! (already up, so no HTTP-up wait is needed here — unlike the initial
//! two-stage probe) and classifies the result into a [`ConnectionState`] a
//! UI layer can react to.
//!
//! [`Monitor`] only surfaces *transitions*, not every poll tick — a picker
//! window shouldn't re-render on every identical "still connected" tick.
//!
//! **DDD Context:** Desktop Shell

use std::time::Duration;

use crate::readiness::{FailureReason, VersionReport};

/// Health of an already-attached front, as observed by re-polling `/readiness`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConnectionState {
    /// `/readiness` returned 200 — the workspace is reachable.
    Connected(VersionReport),
    /// `/readiness` returned 503 with a taxonomy reason — the front process
    /// is alive and answering, but the workspace itself is not reachable
    /// (the `:nodedown`-equivalent case: dead workspace, bad cookie, or this
    /// front's own epmd went away).
    Disconnected(FailureReason),
    /// The front's HTTP port itself is not answering — a stronger failure
    /// than `Disconnected` (the front process is probably gone, not just its
    /// workspace connection), so a UI would want a different treatment (e.g.
    /// "front process exited" rather than "workspace disconnected").
    FrontUnreachable,
}

/// Outcome of a single monitoring poll — mirrors what an HTTP probe against
/// `/readiness` alone can observe (no HTTP-up-first stage; the front is
/// assumed already up since this only runs after a successful attach).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PollOutcome {
    ReadinessOk(VersionReport),
    ReadinessError(FailureReason),
    Unreachable,
}

/// Pure classification: map one poll outcome to a [`ConnectionState`].
#[must_use]
pub fn classify(outcome: PollOutcome) -> ConnectionState {
    match outcome {
        PollOutcome::ReadinessOk(v) => ConnectionState::Connected(v),
        PollOutcome::ReadinessError(reason) => ConnectionState::Disconnected(reason),
        PollOutcome::Unreachable => ConnectionState::FrontUnreachable,
    }
}

/// A transition the caller should act on (e.g. grey/ungrey a window).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateChange {
    pub from: Option<ConnectionState>,
    pub to: ConnectionState,
}

/// Tracks one attached front's connection state across polls and reports
/// only genuine transitions.
#[derive(Debug, Clone)]
pub struct Monitor {
    last: Option<ConnectionState>,
}

impl Default for Monitor {
    fn default() -> Self {
        Self::new()
    }
}

impl Monitor {
    #[must_use]
    pub fn new() -> Self {
        Self { last: None }
    }

    /// Record one poll outcome. Returns `Some(StateChange)` only when the
    /// classified state differs from the last-observed one (including the
    /// very first observation, `from: None`); returns `None` for a repeat of
    /// the same state, so callers only react to real changes.
    pub fn observe(&mut self, outcome: PollOutcome) -> Option<StateChange> {
        let new_state = classify(outcome);
        if self.last.as_ref() == Some(&new_state) {
            return None;
        }
        let change = StateChange {
            from: self.last.take(),
            to: new_state.clone(),
        };
        self.last = Some(new_state);
        Some(change)
    }

    /// The current known state, if any poll has run yet.
    #[must_use]
    pub fn current(&self) -> Option<&ConnectionState> {
        self.last.as_ref()
    }
}

/// Recommended interval between post-attach `/readiness` polls.
///
/// Frequent enough that a workspace death or OS sleep/resume is noticed
/// within a couple of seconds, infrequent enough not to spam a dev machine
/// with HTTP requests all day per attached workspace.
pub const DEFAULT_POLL_INTERVAL: Duration = Duration::from_secs(3);

#[cfg(test)]
mod tests {
    use super::*;

    fn version() -> VersionReport {
        VersionReport {
            runtime_version: "0.4.0".to_string(),
            protocol_version: "2.0".to_string(),
            otp_release: "27".to_string(),
            erts_version: "15.0".to_string(),
        }
    }

    #[test]
    fn classify_maps_each_outcome() {
        assert_eq!(
            classify(PollOutcome::ReadinessOk(version())),
            ConnectionState::Connected(version())
        );
        assert_eq!(
            classify(PollOutcome::ReadinessError(FailureReason::DeadWorkspace)),
            ConnectionState::Disconnected(FailureReason::DeadWorkspace)
        );
        assert_eq!(
            classify(PollOutcome::Unreachable),
            ConnectionState::FrontUnreachable
        );
    }

    #[test]
    fn monitor_reports_the_first_observation_as_a_change() {
        let mut monitor = Monitor::new();
        let change = monitor
            .observe(PollOutcome::ReadinessOk(version()))
            .expect("first observation must always be a change");
        assert_eq!(change.from, None);
        assert_eq!(change.to, ConnectionState::Connected(version()));
    }

    #[test]
    fn monitor_suppresses_repeats_of_the_same_state() {
        let mut monitor = Monitor::new();
        assert!(
            monitor
                .observe(PollOutcome::ReadinessOk(version()))
                .is_some()
        );
        assert!(
            monitor
                .observe(PollOutcome::ReadinessOk(version()))
                .is_none(),
            "repeated identical state must not be reported as a change"
        );
        assert!(
            monitor
                .observe(PollOutcome::ReadinessOk(version()))
                .is_none()
        );
    }

    /// The scenario ADR 0097 Broker §3 calls out: a workspace killed while
    /// attached must surface as `Disconnected`, not a silent hang.
    #[test]
    fn monitor_reports_transition_to_disconnected_on_workspace_death() {
        let mut monitor = Monitor::new();
        monitor.observe(PollOutcome::ReadinessOk(version()));

        let change = monitor
            .observe(PollOutcome::ReadinessError(FailureReason::DeadWorkspace))
            .expect("connected -> disconnected must be reported");
        assert_eq!(change.from, Some(ConnectionState::Connected(version())));
        assert_eq!(
            change.to,
            ConnectionState::Disconnected(FailureReason::DeadWorkspace)
        );
    }

    /// Sleep/resume self-heal (ADR 0097 Broker §3, spike criterion (f)): once
    /// `/readiness` starts succeeding again, the monitor must report the
    /// recovery transition, not stay silently stuck on `Disconnected`.
    #[test]
    fn monitor_reports_transition_back_to_connected_on_recovery() {
        let mut monitor = Monitor::new();
        monitor.observe(PollOutcome::ReadinessOk(version()));
        monitor.observe(PollOutcome::ReadinessError(FailureReason::DeadWorkspace));

        let change = monitor
            .observe(PollOutcome::ReadinessOk(version()))
            .expect("disconnected -> connected must be reported");
        assert_eq!(
            change.from,
            Some(ConnectionState::Disconnected(FailureReason::DeadWorkspace))
        );
        assert_eq!(change.to, ConnectionState::Connected(version()));
    }

    #[test]
    fn monitor_reports_front_unreachable_distinctly_from_disconnected() {
        let mut monitor = Monitor::new();
        monitor.observe(PollOutcome::ReadinessOk(version()));

        let change = monitor
            .observe(PollOutcome::Unreachable)
            .expect("connected -> unreachable must be reported");
        assert_eq!(change.to, ConnectionState::FrontUnreachable);
        assert_ne!(
            change.to,
            ConnectionState::Disconnected(FailureReason::DeadWorkspace),
            "front-process-gone must be a distinct state from workspace-unreachable"
        );
    }

    #[test]
    fn monitor_current_reflects_the_last_observation() {
        let mut monitor = Monitor::new();
        assert_eq!(monitor.current(), None);
        monitor.observe(PollOutcome::ReadinessOk(version()));
        assert_eq!(
            monitor.current(),
            Some(&ConnectionState::Connected(version()))
        );
    }
}
