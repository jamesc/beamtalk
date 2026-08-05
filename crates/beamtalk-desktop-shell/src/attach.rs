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
//! [`AttachDecision::Spawn`], [`AttachDecision::FocusExisting`], or
//! [`AttachDecision::AlreadyInFlight`].
//!
//! [`AttachManager::decide_and_claim`] is the whole reason this is a
//! stateful `&mut self` method rather than a read-only `decide`: the spike's
//! own throwaway coordinator hit exactly this race — two near-simultaneous
//! attach clicks for the same workspace both observing "nothing tracked" and
//! both spawning a front — and fixed it with an atomic claim-or-wait
//! (`Coordinator.State.claim_or_get/1`, `docs/research/desktop-shell-spike.md`
//! "No-shell coordinator"). A separate `decide()` read followed later by a
//! separate `record_attached()` write, with the lock released in between (as
//! a naive Tauri command handler would do — check under the lock, spawn
//! without it, re-lock to record), reopens the identical race: a Rust
//! `Mutex<AttachManager>` provides no atomicity across two separate
//! `lock()` calls. `decide_and_claim` closes it by making "check and mark
//! as in-progress" one atomic step, so a caller only ever needs a single
//! lock acquisition for the decision itself.
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
    /// The generation this front was spawned under (assigned by
    /// [`AttachManager::decide_and_claim`]) — see
    /// [`AttachManager::is_current_front`] for why a post-attach monitor's
    /// stop condition must key on this instead of `port`.
    pub generation: u64,
}

/// Internal per-workspace bookkeeping state. Not exposed directly — callers
/// only see it through [`AttachDecision`]/[`AttachManager`]'s accessor
/// methods, which already collapse it to what they need.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Slot {
    /// [`AttachManager::decide_and_claim`] has handed out [`AttachDecision::Spawn`]
    /// for this workspace and no matching [`AttachManager::record_attached`]
    /// or [`AttachManager::release_claim`] has happened yet — a spawn +
    /// readiness wait is presumed in flight. Carries the generation assigned
    /// at claim time, so [`AttachManager::record_attached_if_claiming`] and
    /// [`AttachManager::release_claim`] can each tell a live claim from a
    /// *different*, newer claim for the same workspace (the claim was
    /// cleared and re-claimed while the original caller's spawn/probe was
    /// still in flight) rather than treating any `Claiming` slot as a match
    /// for any caller.
    Claiming(u64),
    /// A front is attached and its window should already exist.
    Attached(AttachedFront),
}

/// What a GUI shell should do in response to an "Attach" action on some
/// workspace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttachDecision {
    /// No existing or in-flight front for this workspace: spawn one, and the
    /// workspace is now claimed — call [`AttachManager::record_attached`] on
    /// success or [`AttachManager::release_claim`] on failure so the claim
    /// doesn't linger forever. `generation` is this claim's generation
    /// number (see [`AttachManager::is_current_front`]) — carry it through
    /// to the [`AttachedFront`] passed to `record_attached`/
    /// `record_attached_if_claiming` on success, and to the post-attach
    /// monitor's stop-condition check.
    Spawn { generation: u64 },
    /// A front is already attached: focus/reuse its window rather than
    /// spawning a second one (BT-2984 spike decision).
    FocusExisting { window_id: WindowId, port: u16 },
    /// A concurrent attach for this same workspace is already in flight
    /// (racing this one) — do not spawn a second front. The in-flight
    /// attach will open the window once it resolves; there is nothing new
    /// for this caller to do.
    AlreadyInFlight,
}

/// Tracks which workspaces currently have an attached (or in-flight) front,
/// keyed by workspace id (at most one front per workspace at a time — the
/// spike's decided semantics, not a hard technical limit; see the module
/// docs).
#[derive(Debug, Default)]
pub struct AttachManager {
    attached: HashMap<String, Slot>,
    /// Monotonically increasing counter, one new value handed out per
    /// successful claim (every [`AttachDecision::Spawn`]). Never reused —
    /// see [`Self::is_current_front`] for why a monitor must be able to tell
    /// two attach attempts on the same `(workspace_id, port)` pair apart
    /// even when the OS's ephemeral-port allocator hands back the same port
    /// number for both.
    next_generation: u64,
}

impl AttachManager {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Atomically decide what an "Attach" click on `workspace_id` should do
    /// *and*, if the answer is [`AttachDecision::Spawn`], claim the
    /// workspace in the same step — see the module docs for why this must be
    /// one atomic operation rather than a separate read-then-write.
    pub fn decide_and_claim(&mut self, workspace_id: &str) -> AttachDecision {
        match self.attached.get(workspace_id) {
            Some(Slot::Attached(front)) => AttachDecision::FocusExisting {
                window_id: window_label(workspace_id),
                port: front.port,
            },
            Some(Slot::Claiming(_)) => AttachDecision::AlreadyInFlight,
            None => {
                let generation = self.next_generation;
                self.next_generation += 1;
                self.attached
                    .insert(workspace_id.to_string(), Slot::Claiming(generation));
                AttachDecision::Spawn { generation }
            }
        }
    }

    /// Record a newly attached front after a successful spawn + readiness
    /// wait, resolving an earlier [`Self::decide_and_claim`]'s claim (or, if
    /// called without a prior claim, still records — a plain overwrite
    /// rather than panicking on the unexpected case).
    pub fn record_attached(&mut self, front: AttachedFront) {
        self.attached
            .insert(front.workspace_id.clone(), Slot::Attached(front));
    }

    /// Like [`Self::record_attached`], but refuses to record if
    /// `front.workspace_id` is no longer [`Slot::Claiming`] *for the same
    /// generation `front.generation` was claimed under* — i.e. a concurrent
    /// [`Self::remove`] (from `detach`/`quit` racing the tail end of this
    /// same attach's spawn-and-probe) already cleared the claim out from
    /// under the caller, possibly followed by a *fresh* claim (a new
    /// generation) from a subsequent attach click. Returns `false` in either
    /// case so the caller (which just finished spawning a process and
    /// opening a window) knows to tear both back down instead of leaving a
    /// ghost [`Slot::Attached`] entry for a front nothing supervises
    /// anymore: nothing would ever clean it up, `is_attached` would wrongly
    /// report `true` for this workspace forever, and a future attach click
    /// would try to [`AttachDecision::FocusExisting`] a window that may
    /// already be closed — a permanently stuck workspace with no way to
    /// re-attach short of restarting the whole app. Checking the generation
    /// (not just "is *a* claim still pending") also stops this stale caller
    /// from clobbering a legitimately newer in-flight claim it knows nothing
    /// about.
    ///
    /// This is the real-world-reachable race [`Self::record_attached`]'s own
    /// "or, if called without a prior claim, still records" leniency does
    /// *not* protect against — that leniency exists for callers (like this
    /// crate's own tests) that never called [`Self::decide_and_claim`] at
    /// all, not for a claim that existed and was legitimately cleared by a
    /// concurrent detach.
    #[must_use]
    pub fn record_attached_if_claiming(&mut self, front: AttachedFront) -> bool {
        if !matches!(self.attached.get(&front.workspace_id), Some(Slot::Claiming(claimed_generation)) if *claimed_generation == front.generation)
        {
            return false;
        }
        self.attached
            .insert(front.workspace_id.clone(), Slot::Attached(front));
        true
    }

    /// Release a claim that didn't pan out (spawn or readiness failed) so
    /// the next attach click is free to try again instead of being stuck
    /// behind a claim nothing will ever resolve. A no-op unless `workspace_id`
    /// is currently claiming *under this exact `generation`* — mirroring
    /// [`Self::record_attached_if_claiming`]'s guard, and for the same
    /// reason: checking only "is *a* claim pending" would let a stale caller
    /// whose own claim was already cleared (e.g. by a concurrent `remove`)
    /// clear out a *different*, newer claim that has since replaced it —
    /// silently undoing another in-flight attach's `AlreadyInFlight`
    /// protection and letting a third `decide_and_claim` spawn a second
    /// front for the same workspace while that newer claim is still live.
    /// Safe to call defensively from every failure path regardless of
    /// whether `workspace_id` was ever actually claimed under this
    /// generation, was already attached, or was already released.
    pub fn release_claim(&mut self, workspace_id: &str, generation: u64) {
        if matches!(self.attached.get(workspace_id), Some(Slot::Claiming(claimed_generation)) if *claimed_generation == generation)
        {
            self.attached.remove(workspace_id);
        }
    }

    /// Remove bookkeeping for `workspace_id` (detach, quit, or front death).
    /// Returns the removed front record, if it was actually attached (not
    /// merely claiming) — a caller uses its `port`/`pid` to kill the process
    /// and clear a front record.
    pub fn remove(&mut self, workspace_id: &str) -> Option<AttachedFront> {
        match self.attached.remove(workspace_id) {
            Some(Slot::Attached(front)) => Some(front),
            Some(Slot::Claiming(_)) | None => None,
        }
    }

    #[must_use]
    pub fn is_attached(&self, workspace_id: &str) -> bool {
        matches!(self.attached.get(workspace_id), Some(Slot::Attached(_)))
    }

    /// Is `generation` the generation of the *currently* attached front for
    /// `workspace_id`? A post-attach monitor loop must use this, not
    /// [`Self::is_attached`], as its stop condition: detach-then-re-attach
    /// spawns a fresh front under a fresh generation, so an older monitor
    /// thread from a prior attach would otherwise see `is_attached` flip
    /// back to `true` for the new attachment and wrongly keep running.
    ///
    /// Keyed on `generation` (assigned by [`Self::decide_and_claim`], never
    /// reused), not `port`: `port` alone is not a reliable discriminator
    /// here, because `find_free_port`'s ephemeral-port allocation can — rare
    /// but not impossible — hand the same port back to a fresh spawn shortly
    /// after the workspace that used it was detached, at which point a
    /// stale monitor and the fresh one would become indistinguishable by
    /// `(workspace_id, port)` alone and both would run forever, polling the
    /// same port and fighting over the window's title/events.
    #[must_use]
    pub fn is_current_front(&self, workspace_id: &str, generation: u64) -> bool {
        matches!(self.attached.get(workspace_id), Some(Slot::Attached(front)) if front.generation == generation)
    }

    #[must_use]
    pub fn get(&self, workspace_id: &str) -> Option<&AttachedFront> {
        match self.attached.get(workspace_id) {
            Some(Slot::Attached(front)) => Some(front),
            Some(Slot::Claiming(_)) | None => None,
        }
    }

    /// Every currently-attached (not merely claiming) workspace id, for
    /// "detach all" on quit.
    #[must_use]
    pub fn attached_ids(&self) -> Vec<&str> {
        self.attached
            .iter()
            .filter_map(|(id, slot)| match slot {
                Slot::Attached(_) => Some(id.as_str()),
                Slot::Claiming(_) => None,
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn front(workspace_id: &str, port: u16, generation: u64) -> AttachedFront {
        AttachedFront {
            workspace_id: workspace_id.to_string(),
            port,
            pid: 4242,
            generation,
        }
    }

    /// `decide_and_claim` and unwrap the resulting generation, panicking if
    /// the decision wasn't `Spawn` — a test helper for the common case where
    /// a test just needs a fresh claim's generation number, not the
    /// decision itself.
    fn claim(manager: &mut AttachManager, workspace_id: &str) -> u64 {
        match manager.decide_and_claim(workspace_id) {
            AttachDecision::Spawn { generation } => generation,
            other => panic!("expected AttachDecision::Spawn, got {other:?}"),
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
    fn decide_and_claim_spawns_when_nothing_tracked() {
        let mut manager = AttachManager::new();
        assert_eq!(
            manager.decide_and_claim("abc123"),
            AttachDecision::Spawn { generation: 0 }
        );
    }

    #[test]
    fn decide_and_claim_assigns_a_fresh_generation_per_claim() {
        // Consecutive claims (even for different workspaces) must never
        // reuse a generation number — the monitor stale-port guard depends
        // on generations being unique for as long as this AttachManager
        // lives.
        let mut manager = AttachManager::new();
        let gen1 = claim(&mut manager, "abc123");
        manager.release_claim("abc123", gen1);
        let gen2 = claim(&mut manager, "abc123");
        assert_ne!(gen1, gen2);
    }

    #[test]
    fn decide_and_claim_reports_already_in_flight_on_a_racing_second_call() {
        // The exact race the BT-2984 spike found: two near-simultaneous
        // attach clicks for the same workspace. The first claims; the
        // second, before any record_attached/release_claim, must not also
        // get Spawn.
        let mut manager = AttachManager::new();
        claim(&mut manager, "abc123");
        assert_eq!(
            manager.decide_and_claim("abc123"),
            AttachDecision::AlreadyInFlight
        );
    }

    #[test]
    fn decide_and_claim_focuses_the_existing_window_on_a_second_attach() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));

        assert_eq!(
            manager.decide_and_claim("abc123"),
            AttachDecision::FocusExisting {
                window_id: "ws-abc123".to_string(),
                port: 4567,
            }
        );
    }

    #[test]
    fn decide_and_claim_is_scoped_per_workspace() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));

        // A different, never-attached workspace still spawns.
        assert!(matches!(
            manager.decide_and_claim("def456"),
            AttachDecision::Spawn { .. }
        ));
    }

    #[test]
    fn release_claim_lets_a_failed_attach_be_retried() {
        let mut manager = AttachManager::new();
        let generation = claim(&mut manager, "abc123");

        manager.release_claim("abc123", generation);

        assert!(matches!(
            manager.decide_and_claim("abc123"),
            AttachDecision::Spawn { .. }
        ));
    }

    #[test]
    fn release_claim_does_not_clear_a_real_attachment() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));

        // Defensive call from a failure path must never undo a real,
        // already-recorded attachment.
        manager.release_claim("abc123", 0);

        assert!(manager.is_attached("abc123"));
    }

    #[test]
    fn release_claim_of_an_untracked_workspace_is_a_no_op() {
        let mut manager = AttachManager::new();
        manager.release_claim("nonexistent", 0); // must not panic
        assert!(!manager.is_attached("nonexistent"));
    }

    #[test]
    fn release_claim_refuses_a_stale_generation_after_re_claim() {
        // The exact race an adversarial review pass found: caller A claims,
        // something else (e.g. a concurrent `remove`) clears A's claim
        // before A's own spawn/probe resolves, caller B then claims fresh
        // (a new generation) — and A's own failure path *still* calls
        // `release_claim` with its now-stale generation. Without the
        // generation check, that stale call would see "some claim is
        // pending" (true — it's B's) and clear B's live claim out from under
        // it, letting a third `decide_and_claim` spawn a second front for
        // this workspace while B's attach is still in flight — silently
        // defeating the `AttachDecision::AlreadyInFlight` protection.
        let mut manager = AttachManager::new();
        let stale_generation = claim(&mut manager, "abc123"); // caller A
        manager.remove("abc123"); // something clears A's claim
        let fresh_generation = claim(&mut manager, "abc123"); // caller B, fresh
        assert_ne!(stale_generation, fresh_generation);

        manager.release_claim("abc123", stale_generation); // A's stale failure path

        assert_eq!(
            manager.decide_and_claim("abc123"),
            AttachDecision::AlreadyInFlight,
            "a stale release_claim must not clear a newer, live claim"
        );
    }

    #[test]
    fn record_attached_resolves_a_prior_claim() {
        let mut manager = AttachManager::new();
        claim(&mut manager, "abc123");

        manager.record_attached(front("abc123", 4567, 0));

        assert!(manager.is_attached("abc123"));
        assert_eq!(manager.get("abc123"), Some(&front("abc123", 4567, 0)));
    }

    #[test]
    fn record_attached_if_claiming_succeeds_and_resolves_a_live_claim() {
        let mut manager = AttachManager::new();
        let generation = claim(&mut manager, "abc123");

        assert!(manager.record_attached_if_claiming(front("abc123", 4567, generation)));

        assert!(manager.is_attached("abc123"));
        assert_eq!(
            manager.get("abc123"),
            Some(&front("abc123", 4567, generation))
        );
    }

    #[test]
    fn record_attached_if_claiming_refuses_when_the_claim_was_cleared_concurrently() {
        // The exact race this method exists to close: `decide_and_claim`
        // hands out `Spawn`, but a concurrent `remove` (a `detach`/`quit`
        // racing the tail end of this same attach's spawn-and-probe) clears
        // the claim before the in-flight attach gets to record success.
        let mut manager = AttachManager::new();
        let generation = claim(&mut manager, "abc123");
        manager.remove("abc123"); // simulates the racing detach/quit

        assert!(!manager.record_attached_if_claiming(front("abc123", 4567, generation)));
        assert!(
            !manager.is_attached("abc123"),
            "a refused record must not leave a ghost Attached entry"
        );
    }

    #[test]
    fn record_attached_if_claiming_refuses_a_stale_generation_after_re_claim() {
        // Stronger than the plain "claim was cleared" race above: the claim
        // wasn't merely cleared, it was cleared *and* re-claimed (a fresh
        // generation) before the original, now-stale caller's
        // record_attached_if_claiming lands. The stale caller must not be
        // able to clobber the newer claim just because *some* claim is
        // still pending.
        let mut manager = AttachManager::new();
        let stale_generation = claim(&mut manager, "abc123");
        manager.remove("abc123"); // simulates the racing detach/quit
        let fresh_generation = claim(&mut manager, "abc123"); // a new attach click re-claims
        assert_ne!(stale_generation, fresh_generation);

        assert!(!manager.record_attached_if_claiming(front("abc123", 4567, stale_generation)));
        assert!(
            !manager.is_attached("abc123"),
            "a refused stale record must not clobber the newer live claim"
        );
    }

    #[test]
    fn record_attached_if_claiming_refuses_when_nothing_was_ever_claimed() {
        let mut manager = AttachManager::new();
        assert!(!manager.record_attached_if_claiming(front("abc123", 4567, 0)));
        assert!(!manager.is_attached("abc123"));
    }

    #[test]
    fn record_attached_if_claiming_refuses_when_already_attached() {
        // Can't happen via the real `decide_and_claim`/`record_attached_if_claiming`
        // flow (a second `decide_and_claim` for an attached workspace returns
        // `FocusExisting`, never `Spawn`), but the guard must still hold: this
        // method only ever resolves a `Claiming` slot, never overwrites an
        // existing real attachment the way `record_attached` will.
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));

        assert!(!manager.record_attached_if_claiming(front("abc123", 9999, 0)));
        assert_eq!(manager.get("abc123"), Some(&front("abc123", 4567, 0)));
    }

    #[test]
    fn is_current_front_matches_the_attached_generation() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 7));
        assert!(manager.is_current_front("abc123", 7));
    }

    #[test]
    fn is_current_front_is_false_for_a_stale_generation_after_re_attach() {
        // The exact scenario a post-attach monitor must survive: detach,
        // then re-attach — even if the fresh attach happens to land on the
        // *same* port (ephemeral-port reuse), a monitor still holding the
        // old generation must recognize it's stale.
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));
        manager.remove("abc123");
        manager.record_attached(front("abc123", 4567, 1)); // same port, new generation

        assert!(!manager.is_current_front("abc123", 0));
        assert!(manager.is_current_front("abc123", 1));
    }

    #[test]
    fn is_current_front_is_false_while_merely_claiming() {
        let mut manager = AttachManager::new();
        let generation = claim(&mut manager, "abc123");
        assert!(!manager.is_current_front("abc123", generation));
    }

    #[test]
    fn is_current_front_is_false_for_an_untracked_workspace() {
        let manager = AttachManager::new();
        assert!(!manager.is_current_front("nonexistent", 0));
    }

    #[test]
    fn remove_clears_bookkeeping_so_the_next_attach_spawns_again() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));

        let removed = manager.remove("abc123");
        assert_eq!(removed, Some(front("abc123", 4567, 0)));
        assert!(matches!(
            manager.decide_and_claim("abc123"),
            AttachDecision::Spawn { .. }
        ));
    }

    #[test]
    fn remove_of_an_unattached_workspace_is_a_no_op() {
        let mut manager = AttachManager::new();
        assert_eq!(manager.remove("nonexistent"), None);
    }

    #[test]
    fn remove_of_a_merely_claiming_workspace_returns_none() {
        let mut manager = AttachManager::new();
        claim(&mut manager, "abc123");
        // Never actually attached (no record_attached yet) — nothing real
        // to return, even though something is tracked.
        assert_eq!(manager.remove("abc123"), None);
    }

    #[test]
    fn is_attached_reflects_current_bookkeeping() {
        let mut manager = AttachManager::new();
        assert!(!manager.is_attached("abc123"));
        manager.record_attached(front("abc123", 4567, 0));
        assert!(manager.is_attached("abc123"));
        manager.remove("abc123");
        assert!(!manager.is_attached("abc123"));
    }

    #[test]
    fn is_attached_is_false_while_merely_claiming() {
        let mut manager = AttachManager::new();
        claim(&mut manager, "abc123");
        assert!(
            !manager.is_attached("abc123"),
            "a claim in flight is not yet a real attachment"
        );
    }

    #[test]
    fn get_returns_the_recorded_front() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));
        assert_eq!(manager.get("abc123"), Some(&front("abc123", 4567, 0)));
        assert_eq!(manager.get("nonexistent"), None);
    }

    #[test]
    fn record_attached_overwrites_a_stale_prior_record() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));
        manager.record_attached(front("abc123", 9999, 1));
        assert_eq!(manager.get("abc123"), Some(&front("abc123", 9999, 1)));
    }

    #[test]
    fn attached_ids_lists_every_currently_attached_workspace() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));
        manager.record_attached(front("def456", 4568, 1));

        let mut ids = manager.attached_ids();
        ids.sort_unstable();
        assert_eq!(ids, vec!["abc123", "def456"]);
    }

    #[test]
    fn attached_ids_excludes_merely_claiming_workspaces() {
        let mut manager = AttachManager::new();
        manager.record_attached(front("abc123", 4567, 0));
        claim(&mut manager, "def456");

        assert_eq!(manager.attached_ids(), vec!["abc123"]);
    }

    #[test]
    fn attached_ids_is_empty_when_nothing_attached() {
        let manager = AttachManager::new();
        assert!(manager.attached_ids().is_empty());
    }
}
