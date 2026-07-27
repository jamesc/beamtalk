// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Distribution sname seeding and collision avoidance (ADR 0097 Implementation
//! §1a, shipped in BT-2983 as `BtAttach.Workspace.attach_sname/2`).
//!
//! The front's `ensure_distributed/0` composes its own epmd registration name
//! as `bt_attach_<suffix>_<os_pid>@localhost`, where `<suffix>` comes from the
//! `BT_ATTACH_NODE_SUFFIX` env var this broker sets (see [`crate::spawn`]) and
//! `<os_pid>` is the front's *own* OS process id, appended by the Elixir side
//! automatically (`System.pid()`) — the broker does not need to inject it.
//!
//! Two functions here mirror that contract on the broker side, purely so the
//! broker can *predict* the name a front it just spawned will register under
//! (for monitoring/reaping bookkeeping) without parsing the front's logs:
//!
//! - [`attach_node_suffix`] — the `BT_ATTACH_NODE_SUFFIX` value to set (today,
//!   just the workspace id — see the doc comment there for why that alone is
//!   collision-free).
//! - [`predict_node_name`] — the exact epmd registration name a front spawned
//!   with that suffix and a known OS pid will end up with, once the broker
//!   knows the child's pid (available immediately after spawn).
//!
//! **Unverified assumption, flagged in review**: prediction assumes the OS
//! pid `std::process::Child::id()` reports for the spawned `bin/server`
//! process is the *same* pid the BEAM VM sees as `System.pid()` — true only
//! if `bin/server → bin/bt_attach → erlexec → beam.smp` is an unbroken `exec`
//! chain (no intermediate `fork`), which is standard `mix release` launcher
//! behavior but was not confirmed against this project's actual generated
//! release script — no built `dist-liveview` target was available in the
//! environment this crate was developed in (the spike instead validated
//! names by querying epmd directly after a real spawn, not by predicting
//! from a captured pid). Any consumer relying on [`predict_node_name`] for
//! correctness (not just a monitoring hint) should confirm this live before
//! depending on it, and prefer an epmd query as the source of truth where
//! one is available. The Windows launch path (BT-2988, `bin/bt_attach`
//! invoked directly, no `bin/server` wrapper) has one fewer hop and is
//! likelier to hold, but is equally unverified here.
//!
//! **DDD Context:** Desktop Shell

/// Value to set `BT_ATTACH_NODE_SUFFIX` to for a spawn attaching to `workspace_id`.
///
/// Per the shipped contract (`BtAttach.Workspace.attach_sname/2`), the suffix
/// alone does not need per-process entropy: the front always appends its own
/// OS pid on top, so two fronts attaching to the **same** workspace (a second
/// window, a crash→respawn racing the dying front's epmd deregistration)
/// still get distinct names even though both pass the identical suffix here.
/// An id-only *env value* is therefore correct — collision-freedom comes from
/// the OS pid the front adds, not from anything this function does.
#[must_use]
pub fn attach_node_suffix(workspace_id: &str) -> String {
    workspace_id.to_string()
}

/// Predict the epmd short registration name a front spawned with
/// `BT_ATTACH_NODE_SUFFIX=<suffix>` and OS pid `os_pid` will register under.
///
/// Must stay byte-for-byte in sync with `BtAttach.Workspace.attach_sname/2`
/// (`editors/liveview/lib/bt_attach/workspace.ex`):
/// `:"bt_attach_#{suffix}_#{os_pid}@localhost"`.
#[must_use]
pub fn predict_node_name(suffix: &str, os_pid: u32) -> String {
    format!("bt_attach_{suffix}_{os_pid}@localhost")
}

/// The short name portion (before `@`) of a predicted node name — what an
/// epmd `NAMES` query reports (see `beamtalk_workspace::epmd`).
#[must_use]
pub fn predict_short_name(suffix: &str, os_pid: u32) -> String {
    format!("bt_attach_{suffix}_{os_pid}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn attach_node_suffix_is_the_workspace_id() {
        assert_eq!(attach_node_suffix("abc123"), "abc123");
    }

    #[test]
    fn predict_node_name_matches_the_shipped_elixir_format() {
        // Byte-for-byte against BtAttach.Workspace.attach_sname/2's
        // `:"bt_attach_#{suffix}_#{os_pid}@localhost"`.
        assert_eq!(
            predict_node_name("abc123", 4242),
            "bt_attach_abc123_4242@localhost"
        );
    }

    #[test]
    fn predict_short_name_omits_the_host_part() {
        assert_eq!(predict_short_name("abc123", 4242), "bt_attach_abc123_4242");
        assert!(!predict_short_name("abc123", 4242).contains('@'));
    }

    /// Collision avoidance: two fronts attached to the SAME workspace (same
    /// suffix) but with different OS pids — the scenario ADR 0097 calls out
    /// explicitly (second window, or crash→respawn racing epmd
    /// deregistration) — must predict distinct names.
    #[test]
    fn predict_node_name_distinguishes_two_fronts_on_the_same_workspace() {
        let a = predict_node_name("spike-a", 1001);
        let b = predict_node_name("spike-a", 1002);
        assert_ne!(a, b, "same workspace, different pid must not collide");
    }

    /// Two different workspaces attached to by the broker at the same
    /// (coincidentally identical, e.g. across two totally separate broker
    /// runs) pid must also not collide — the suffix carries the workspace
    /// identity.
    #[test]
    fn predict_node_name_distinguishes_two_workspaces_at_the_same_pid() {
        let a = predict_node_name("spike-a", 5000);
        let b = predict_node_name("spike-b", 5000);
        assert_ne!(a, b, "different workspace, same pid must not collide");
    }

    #[test]
    fn predict_node_name_is_collision_free_across_a_realistic_pid_range() {
        // Simulates a broker restarting many fronts for the same workspace
        // over its lifetime (each with a distinct OS pid, as PIDs are
        // reused-but-not-concurrently by the OS) — every predicted name must
        // be unique.
        let names: HashSet<String> = (1000..1000 + 500)
            .map(|pid| predict_node_name("spike-a", pid))
            .collect();
        assert_eq!(names.len(), 500, "every (suffix, pid) pair must be unique");
    }

    #[test]
    fn attach_node_suffix_preserves_arbitrary_workspace_ids() {
        // Workspace ids are either a 12-hex-char hash or a user-chosen
        // [A-Za-z0-9_-]+ name (see beamtalk-workspace's `validate_workspace_name`) —
        // this function must not mangle either shape.
        for id in ["abc123def456", "my-feature", "my_feature_1"] {
            assert_eq!(attach_node_suffix(id), id);
        }
    }
}
