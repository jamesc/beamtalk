// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Attach-twice / focus-existing decision and window-per-workspace
//! bookkeeping.
//!
//! The BT-2984 spike decided (`docs/research/desktop-shell-spike.md`,
//! "Single-instance policy & attach-twice semantics"): **attaching twice to
//! the same workspace focuses/reuses the existing front, it does not spawn a
//! second one.** [`AttachManager`] is the pure state that decision needs — it
//! tracks which workspaces currently have a live front attached (by
//! workspace id) and, given a click on "Attach" for some workspace, decides
//! [`AttachDecision::Spawn`] or [`AttachDecision::FocusExisting`].
//!
//! Window-per-workspace (ADR 0097 Implementation §4) falls out of the same
//! bookkeeping: [`window_label`] is the deterministic Tauri window label a
//! GUI shell should use for a given workspace, so "does this workspace
//! already have a window" and "what window do I focus" are answered by the
//! same lookup.
//!
//! **DDD Context:** Desktop Shell

use std::collections::HashMap;

/// A GUI shell's window identifier. Kept as an opaque `String` (rather than a
/// toolkit-specific type) so this crate stays GUI-toolkit-agnostic — a Tauri
/// shell treats it as a window label.
pub type WindowId = String;

/// The deterministic window label for `workspace_id` (ADR 0097
/// "Window-per-workspace"). Prefixed so it can never collide with a shell's
/// own non-workspace windows (e.g. the picker window itself).
#[must_use]
pub fn window_label(workspace_id: &str) -> WindowId {
    format!("ws-{workspace_id}")
}

/// Bookkeeping for one currently-attached front, as far as this crate's pure
/// logic needs to know. A GUI shell holds richer state alongside this
/// (the spawned `std::process::Child`, a [`beamtalk_desktop_broker::monitor::Monitor`]
/// instance, …) — this struct carries only what [`AttachManager`]'s decision
/// logic itself needs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttachedFront {
    pub workspace_id: String,
    pub port: u16,
    pub pid: u32,
}

/// What a GUI shell should do in response to an "Attach" action on some
/// workspace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttachDecision {
    /// No existing front for this workspace: spawn one (the normal path).
    Spawn,
    /// A front is already attached: focus/reuse its window rather than
    /// spawning a second one (BT-2984 spike decision).
    FocusExisting { window_id: WindowId, port: u16 },
}

/// Tracks which workspaces currently have an attached front, keyed by
/// workspace id (at most one front per workspace at a time — the spike's
/// decided semantics, not a hard technical limit; see the module docs).
#[derive(Debug, Default)]
pub struct AttachManager {
    attached: HashMap<String, AttachedFront>,
}

impl AttachManager {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Decide what an "Attach" click on `workspace_id` should do.
    #[must_use]
    pub fn decide(&self, workspace_id: &str) -> AttachDecision {
        match self.attached.get(workspace_id) {
            Some(front) => AttachDecision::FocusExisting {
                window_id: window_label(workspace_id),
                port: front.port,
            },
            None => AttachDecision::Spawn,
        }
    }

    /// Record a newly attached front after a successful spawn + readiness
    /// wait. Overwrites any prior record for the same workspace id (a
    /// detach must have called [`Self::remove`] first in the normal flow,
    /// but this stays a plain overwrite rather than panicking on the
    /// unexpected case — a stale record is still corrected).
    pub fn record_attached(&mut self, front: AttachedFront) {
        self.attached.insert(front.workspace_id.clone(), front);
    }

    /// Remove bookkeeping for `workspace_id` (detach, quit, or front death).
    /// Returns the removed record, if any, so a caller can use its `port`/
    /// `pid` to actually kill the process and clear a front record.
    pub fn remove(&mut self, workspace_id: &str) -> Option<AttachedFront> {
        self.attached.remove(workspace_id)
    }

    #[must_use]
    pub fn is_attached(&self, workspace_id: &str) -> bool {
        self.attached.contains_key(workspace_id)
    }

    #[must_use]
    pub fn get(&self, workspace_id: &str) -> Option<&AttachedFront> {
        self.attached.get(workspace_id)
    }

    /// Every currently-attached workspace id, for "detach all" on quit.
    #[must_use]
    pub fn attached_ids(&self) -> Vec<&str> {
        self.attached.keys().map(String::as_str).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn front(workspace_id: &str, port: u16) -> AttachedFront {
        AttachedFront {
            workspace_id: workspace_id.to_string(),
            port,
            pid: 4242,
        }
    }

    #[test]
    fn window_label_is_prefixed_and_deterministic() {
        assert_eq!(window_label("abc123"), "ws-abc123");
        assert_eq!(window_label("abc123"), window_label("abc123"));
    }

    #[test]
    fn window_label_distinguishes_different_workspaces() {
        assert_ne!(window_label("abc123"), window_label("def456"));
    }

    #[test]
    fn decide_spawns_when_nothing_attached() {
        let manager = AttachManager::new();
        assert_eq!(manager.decide("abc123"), AttachDecision::Spawn);
    }

    #[test]
    fn decide_focuses_the_existing_window_on_a_second_attach() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567));

        assert_eq!(
            manager.decide("abc123"),
            AttachDecision::FocusExisting {
                window_id: "ws-abc123".to_string(),
                port: 4567,
            }
        );
    }

    #[test]
    fn decide_is_scoped_per_workspace() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567));

        // A different, never-attached workspace still spawns.
        assert_eq!(manager.decide("def456"), AttachDecision::Spawn);
    }

    #[test]
    fn remove_clears_bookkeeping_so_the_next_attach_spawns_again() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567));

        let removed = manager.remove("abc123");
        assert_eq!(removed, Some(front("abc123", 4567)));
        assert_eq!(manager.decide("abc123"), AttachDecision::Spawn);
    }

    #[test]
    fn remove_of_an_unattached_workspace_is_a_no_op() {
        let mut manager = AttachManager::new();
        assert_eq!(manager.remove("nonexistent"), None);
    }

    #[test]
    fn is_attached_reflects_current_bookkeeping() {
        let mut manager = AttachManager::new();
        assert!(!manager.is_attached("abc123"));
        manager.record_attached(front("abc123", 4567));
        assert!(manager.is_attached("abc123"));
        manager.remove("abc123");
        assert!(!manager.is_attached("abc123"));
    }

    #[test]
    fn get_returns_the_recorded_front() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567));
        assert_eq!(manager.get("abc123"), Some(&front("abc123", 4567)));
        assert_eq!(manager.get("nonexistent"), None);
    }

    #[test]
    fn record_attached_overwrites_a_stale_prior_record() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567));
        manager.record_attached(front("abc123", 9999));
        assert_eq!(manager.get("abc123"), Some(&front("abc123", 9999)));
    }

    #[test]
    fn attached_ids_lists_every_currently_attached_workspace() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567));
        manager.record_attached(front("def456", 4568));

        let mut ids = manager.attached_ids();
        ids.sort_unstable();
        assert_eq!(ids, vec!["abc123", "def456"]);
    }

    #[test]
    fn attached_ids_is_empty_when_nothing_attached() {
        let manager = AttachManager::new();
        assert!(manager.attached_ids().is_empty());
    }
}
