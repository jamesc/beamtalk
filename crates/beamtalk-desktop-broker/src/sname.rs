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
//! **Verified against a real `dist-liveview` release (BT-3004).** Prediction
//! assumes the OS pid `std::process::Child::id()` reports for the spawned
//! `bin/server` process is the *same* pid the BEAM VM sees as
//! `System.pid()` — true only if `bin/server → bin/bt_attach → erlexec →
//! beam.smp` is an unbroken `exec` chain (no intermediate `fork`). A
//! `Command::new(bin/server).spawn()` call was raced against a real
//! `dist-liveview` release (`just dist-liveview`) attached to a live
//! workspace: the captured `Child::id()` matched, byte for byte, the short
//! name epmd reported once the front's lazy `ensure_distributed/0` ran (the
//! first `/readiness` call) — e.g. `Child::id()` 26765 against a workspace
//! suffix `bt3004test` produced the exact epmd registration
//! `bt_attach_bt3004test_26765`. The launcher chain holds: no fork breaks it
//! on this platform.
//!
//! **The Windows launch path is actually worse here, not better — an earlier
//! draft of this comment wrongly claimed the opposite** (BT-2988,
//! adversarial-review correction): Windows cannot `CreateProcessW` a `.bat`
//! directly, so `bin\bt_attach.bat` runs via `cmd.exe`, which is what
//! `std::process::Child::id()` actually reports — not `erl.exe`'s pid, and
//! not `bin/server`'s single extra hop either. `predict_node_name` is
//! therefore expected to predict the *wrong* name on Windows, not just an
//! unverified one. See `crate::spawn`'s and `crate::winjob`'s module docs for
//! how orphan-*killing* is handled despite this (a job object, not
//! `Child::kill`/pid tracking) — `predict_node_name`'s pid guess plays no
//! part in that mechanism.
//!
//! **Fixed (BT-3045) by not trusting the pid guess on Windows at all**:
//! [`resolve_registered_node_name`] queries epmd directly — the same
//! `NAMES_REQ` protocol [`crate::discovery`]'s liveness check already uses —
//! for a registration matching the known `bt_attach_<suffix>_` prefix,
//! rather than predicting a pid it cannot correctly guess. This only returns
//! an answer once the front has actually distributed (`ensure_distributed/0`
//! runs lazily, on the first `/readiness` call — see
//! `crate::spawn`/`desktop/src-tauri/src/commands.rs`'s `attach_and_open_window`,
//! which calls it once readiness reaches `Ready`, not at spawn time), so it
//! cannot replace `predict_node_name` for the *earlier* orphan-reaping
//! bookkeeping write (`persist_front_record` in `commands.rs`, which must
//! record something as soon as the process exists, before readiness is
//! known) — Windows records an explicit "not yet known" placeholder there
//! instead of a wrong-looking real name, and corrects it once
//! `resolve_registered_node_name` has a real answer. `FrontRecord.node_name`
//! itself remains bookkeeping/display only: `crate::reap`'s sweep keys
//! entirely off `FrontRecord.pid` (and, on Windows, the
//! [`crate::winjob::JobHandle`] tree-kill — neither touches `node_name`), so
//! a stale or placeholder value there was never a kill/reap-correctness bug,
//! only a misleading one for anyone reading the on-disk record.
//!
//! **Suffix-only matching, deliberately not disambiguated by `pid` (BT-3062)**:
//! a Claude review Suggestion on the BT-3045 PR proposed closing the
//! crash→respawn race below by validating the resolved epmd name's own
//! embedded pid segment against the caller's `expected_pid`
//! (`Child::id()`/`FrontRecord.pid`) before [`crate::reap::update_record_node_name`]
//! writes it. That doesn't actually work *here*: this function is only ever
//! called from the Windows-only correction path
//! (`desktop/src-tauri/src/commands.rs`'s `update_windows_node_name_after_readiness`,
//! `#[cfg(windows)]`), and the pid segment epmd's real registration embeds is
//! `System.pid()` — `erl.exe`'s own pid — which the "**Windows launch path**"
//! paragraph above establishes is a *different* process from the one
//! `Child::id()`/`FrontRecord.pid` identifies (`cmd.exe`, the `.bat` wrapper).
//! Gating the match on `expected_pid == embedded pid` would therefore never
//! match on Windows — not just in the racy case this was meant to close, but
//! on *every* call, silently disabling the correction entirely rather than
//! narrowing its race window. (On Unix the two pids genuinely are the same
//! process per BT-3004's verification above, so the check would work there —
//! but Unix never calls this function; `predict_node_name` is trusted
//! directly there instead, see [`crate::spawn`]'s caller.)
//!
//! The crash→respawn race is real and stays open: front A (`FrontRecord`
//! still on disk, pid X) dies and deregisters from epmd before its detach
//! path runs, front B spawns for the *same* workspace under a different port
//! and registers with epmd, and the background thread still resolving A's
//! correction sees exactly one epmd match — B's — and (via
//! `update_record_node_name`'s CAS, which only ever validates the *on-disk
//! record's* pid, not the resolved name's) stamps B's real node name onto
//! A's own record. Accepted rather than closed, because `FrontRecord.node_name`
//! remains bookkeeping/display only everywhere it's read (`crate::reap`'s
//! sweep keys entirely off `pid`, never `node_name` — see that module's doc
//! comment) — the worst case is a transient, cosmetically-wrong value in a
//! record a future sweep or the dying front's own delayed detach will clear
//! anyway, not a kill/reap-correctness bug. A real fix would need a
//! genuinely shared identity between the broker and the front's own
//! `System.pid()` (e.g. baking per-spawn entropy — the HTTP port,
//! already OS-allocated per spawn per `crate::port`'s docs — into
//! [`attach_node_suffix`] itself, so two fronts for one workspace can never
//! share a suffix in the first place) — a real but materially larger,
//! cross-cutting change to a contract several other things depend on
//! (`attach_node_suffix`'s own tests, the Elixir side's
//! `BtAttach.Workspace.attach_sname/2` consumer), out of scope for this
//! narrow follow-up.
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

/// A placeholder `FrontRecord.node_name` value for a front just spawned on a
/// platform where [`predict_node_name`] cannot be trusted (Windows —
/// `desktop/src-tauri/src/commands.rs`'s `persist_front_record` calls this
/// instead of guessing a pid-based name it knows is wrong; see this module's
/// doc comment). Deliberately shaped so it can never collide
/// with a real `bt_attach_<suffix>_<pid>@localhost` registration (no `@`, and
/// the `pending:` tag makes it obviously not a dist node name to a human
/// reading the on-disk record) — callers should replace it via
/// [`resolve_registered_node_name`] once the front's readiness is confirmed,
/// but a record that never gets that follow-up call (e.g. attach fails
/// before reaching `Ready`) is at least honestly labeled rather than
/// silently wrong.
#[must_use]
pub fn pending_node_name(suffix: &str) -> String {
    format!("<pending:{suffix}>")
}

/// Resolve a front's *actual* epmd registration name by querying epmd
/// directly, rather than guessing from an OS pid — the Windows-correct
/// alternative to [`predict_node_name`] this module's doc comment describes
/// (BT-3045). Looks for exactly one currently-registered short name with the
/// `bt_attach_<suffix>_` prefix `BtAttach.Workspace.attach_sname/2` always
/// produces.
///
/// Deliberately **not** disambiguated further by the caller's own pid
/// (BT-3062) — see this module's doc comment's "**Suffix-only matching**"
/// paragraph for why a pid-based check would silently break this function on
/// the only platform it's actually called from, rather than narrow a race.
///
/// Returns `Ok(None)` — not a guess — when epmd reports zero matches (the
/// front hasn't distributed yet; `ensure_distributed/0` only runs lazily on
/// the first `/readiness` call, so this only has a real answer to give once
/// readiness has resolved at least once) or more than one (two fronts
/// attached to the same workspace concurrently — a real, ADR-anticipated
/// scenario, see [`attach_node_suffix`]'s doc comment — make the prefix alone
/// ambiguous; a caller needing to disambiguate that case has a better signal
/// available, e.g. the specific pid it just spawned combined with
/// `metadata.json`'s freshly-written `node_name` once the front persists it).
///
/// # Errors
///
/// Returns an error only if the epmd query itself fails (a transport-level
/// I/O error after a connection was established — "epmd not running" is not
/// an error there and surfaces here as `Ok(None)`, zero names to match
/// against). See [`beamtalk_workspace::epmd::query_epmd_names`].
pub fn resolve_registered_node_name(suffix: &str) -> crate::error::Result<Option<String>> {
    let names = beamtalk_workspace::epmd::query_epmd_names()?;
    Ok(find_unique_match(names.iter().map(String::as_str), suffix))
}

/// Pure matching logic behind [`resolve_registered_node_name`] — given a set
/// of currently-registered epmd short names, find the unique one (if any)
/// whose `bt_attach_<suffix>_<pid>` shape has exactly `suffix` for its
/// suffix segment. Split out so this decision is testable without a real
/// epmd running, matching the pure-decision/impure-I/O split
/// [`crate::reap::classify_record`] already uses for the same reason.
///
/// Deliberately **not** a plain `starts_with("bt_attach_{suffix}_")` check:
/// workspace ids (and thus suffixes — [`attach_node_suffix`] passes them
/// through unchanged) may themselves contain `_` (e.g. `"my_feature_1"`, see
/// [`attach_node_suffix`]'s own tests), so a bare prefix match on suffix
/// `"my"` would also match a *different* workspace's registration
/// `bt_attach_my_feature_1_42` — the shared `bt_attach_my_` prefix is a
/// false positive there, not a real match. Splitting off the pid at the
/// *last* `_` instead (the pid — `System.pid()` — is always a plain decimal
/// integer with no embedded `_` of its own) and comparing the remaining
/// suffix segment for exact equality closes that gap.
fn find_unique_match<'a>(names: impl IntoIterator<Item = &'a str>, suffix: &str) -> Option<String> {
    let mut matches = names.into_iter().filter(|n| {
        n.strip_prefix("bt_attach_")
            .and_then(|rest| rest.rsplit_once('_'))
            .is_some_and(|(name_suffix, pid)| {
                name_suffix == suffix && !pid.is_empty() && pid.bytes().all(|b| b.is_ascii_digit())
            })
    });
    match (matches.next(), matches.next()) {
        (Some(only), None) => Some(format!("{only}@localhost")),
        _ => None,
    }
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

    // ── pending_node_name / find_unique_match (BT-3045) ─────────────────

    #[test]
    fn pending_node_name_is_not_shaped_like_a_real_dist_node_name() {
        let placeholder = pending_node_name("abc123");
        assert!(
            !placeholder.contains('@'),
            "must not look like a real node@host dist name that could be \
             mistaken for a genuine (if stale) registration: {placeholder:?}"
        );
        assert!(placeholder.contains("abc123"));
    }

    #[test]
    fn find_unique_match_returns_none_when_epmd_has_no_matching_name() {
        let names = ["some_other_node", "bt_attach_other-workspace_123"];
        assert_eq!(find_unique_match(names, "spike-a"), None);
    }

    #[test]
    fn find_unique_match_returns_none_when_epmd_is_empty() {
        assert_eq!(find_unique_match([], "spike-a"), None);
    }

    #[test]
    fn find_unique_match_finds_the_single_matching_registration() {
        let names = ["unrelated_node", "bt_attach_spike-a_4242"];
        assert_eq!(
            find_unique_match(names, "spike-a"),
            Some("bt_attach_spike-a_4242@localhost".to_string())
        );
    }

    /// BT-3062 investigated closing this by requiring the resolved name's
    /// embedded pid to match the caller's own known pid, but concluded (see
    /// this module's doc comment, "**Suffix-only matching**" paragraph) that
    /// a pid-based check would silently break every call on the only
    /// platform this function runs on (Windows: the caller's known pid is
    /// `cmd.exe`'s, not the `erl.exe` pid epmd's registration actually
    /// embeds) — so this stays deliberately unresolved here, ambiguous
    /// suffix matches still refuse to guess rather than picking one
    /// arbitrarily.
    #[test]
    fn find_unique_match_returns_none_when_two_fronts_on_the_same_workspace_are_ambiguous() {
        // ADR 0097-anticipated scenario (see attach_node_suffix's doc
        // comment): a second window, or a crash→respawn racing the dying
        // front's epmd deregistration. The prefix alone can't disambiguate
        // which one a caller means — refuse to guess rather than picking one
        // arbitrarily.
        let names = ["bt_attach_spike-a_1001", "bt_attach_spike-a_1002"];
        assert_eq!(find_unique_match(names, "spike-a"), None);
    }

    #[test]
    fn find_unique_match_does_not_treat_a_longer_suffix_as_a_prefix_match() {
        // suffix "a" must not match a registration for workspace "ab".
        let names = ["bt_attach_ab_123"];
        assert_eq!(find_unique_match(names, "a"), None);
    }

    /// Regression test for a real ambiguity a naive `starts_with` prefix
    /// check would have: workspace ids (and thus suffixes) may themselves
    /// contain `_` (see `attach_node_suffix_preserves_arbitrary_workspace_ids`
    /// above, e.g. `"my_feature_1"`) — a *different*, unrelated workspace
    /// named `"my_feature_1"` running at the same time as one named `"my"`
    /// must never have its registration mistaken for `"my"`'s just because
    /// `"bt_attach_my_"` happens to be a string prefix of
    /// `"bt_attach_my_feature_1_42"`.
    #[test]
    fn find_unique_match_does_not_confuse_an_underscore_containing_suffix_with_a_shorter_one() {
        let names = ["bt_attach_my_feature_1_42"];
        assert_eq!(
            find_unique_match(names, "my"),
            None,
            "workspace suffix 'my' must not match a registration belonging to \
             workspace 'my_feature_1'"
        );
        assert_eq!(
            find_unique_match(names, "my_feature_1"),
            Some("bt_attach_my_feature_1_42@localhost".to_string()),
            "the actual owning workspace's suffix should still match correctly"
        );
    }

    #[test]
    fn find_unique_match_rejects_a_non_numeric_trailing_segment() {
        // A malformed/foreign registration that happens to share the
        // bt_attach_ prefix and suffix but has a non-pid trailing segment
        // must not be treated as a match.
        let names = ["bt_attach_spike-a_notapid"];
        assert_eq!(find_unique_match(names, "spike-a"), None);
    }
}
