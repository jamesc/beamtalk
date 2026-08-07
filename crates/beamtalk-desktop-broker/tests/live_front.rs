// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Live-runtime validation of the desktop broker's spawn/readiness
//! assumptions against a real `dist-liveview` release (BT-3004).
//!
//! BT-2985 built [`beamtalk_desktop_broker`] entirely against the ADR
//! 0097 / spike contract, with no built `dist-liveview` release available in
//! that development sandbox — `sname::predict_node_name`'s pid assumption,
//! `readiness::ProbeTimeouts::default_local`'s timeout budget, and
//! `spawn::DEFAULT_BIND_FAILURE_GRACE`'s bind-failure heuristic were all
//! reasoned from the ADR/OTP defaults rather than measured. This suite
//! re-runs those three checks against a real release, so the module doc
//! comments' calibrated numbers can be reproduced (and re-calibrated, if a
//! future front/OTP/Phoenix upgrade changes the timings).
//!
//! Every test here is `#[ignore]` — they need a real, running workspace and
//! a real `dist-liveview` release, neither of which any `just` recipe builds
//! as a side effect of `test`/`ci`. Set up both first:
//!
//! ```bash
//! just build
//! ./target/debug/beamtalk workspace create bt3004_live_probe --background --persistent
//! just web-setup
//! cd editors/liveview && mix assets.setup && cd ../..
//! just dist-liveview
//! ```
//!
//! Then run, from the repo root:
//!
//! ```bash
//! BT_DESKTOP_BROKER_LIVE_LAUNCHER="$PWD/dist-liveview/bin/server" \
//! BT_DESKTOP_BROKER_LIVE_WORKSPACE=bt3004_live_probe \
//!   cargo test -p beamtalk-desktop-broker --test live_front -- --ignored --test-threads=1
//! ```
//!
//! `--test-threads=1` matters: these tests bind real loopback ports and race
//! real processes against each other, so running them concurrently would
//! reintroduce the exact TOCTOU port race [`beamtalk_desktop_broker::port`]'s
//! module doc describes and its retry logic is meant to absorb.
//!
//! ## BT-2989: full attach/detach lifecycle + dead-workspace negative path
//!
//! Two further tests below round out this file into the Rust half of BT-2989's
//! E2E validation (ADR 0097 Phase 5) — the acceptance criteria's "attach →
//! confirm reachable → detach → confirm the front process exits" and "attach
//! with a dead workspace surfaces the failure taxonomy rather than hanging or
//! crashing" clauses, exercised against the exact production call shape
//! `desktop/src-tauri/src/commands.rs`'s `attach`/`detach` commands use
//! (`spawn_front_with_port_retry` → `wait_ready` → kill+wait). The browser/UI
//! half of BT-2989 (an eval actually round-tripping through the LiveView page
//! a workspace window loads, and the picker showing that failure) lives in
//! `desktop/e2e/` — see that directory's README for why it is a separate,
//! Node/Playwright-based script rather than more Rust here: driving the
//! *rendered* LiveView page needs a real browser, which this crate has no
//! reason to depend on.
//!
//! `dead_workspace_readiness_resolves_to_dead_workspace_not_a_hang` needs one
//! extra fixture beyond the module setup above: a workspace that was created
//! and then **stopped** (so `~/.beamtalk/workspaces/<id>/metadata.json` +
//! `cookie` still resolve — `spawn_front` only checks `metadata.json` exists,
//! per its doc comment — but no BEAM node is listening). Create one with:
//!
//! ```bash
//! ./target/debug/beamtalk workspace create bt2989_dead_probe --background --persistent
//! ./target/debug/beamtalk workspace stop bt2989_dead_probe
//! ```
//!
//! then export `BT_DESKTOP_BROKER_LIVE_DEAD_WORKSPACE=bt2989_dead_probe` for
//! the `cargo test` invocation above (it only affects that one test; the other
//! three still need `BT_DESKTOP_BROKER_LIVE_WORKSPACE` **running**).
//!
//! **DDD Context:** Desktop Shell

use std::process::{Child, Command};
use std::time::{Duration, Instant};

use beamtalk_desktop_broker::readiness::{self, ProbeTimeouts, ReadinessState};
use beamtalk_desktop_broker::sname::predict_short_name;
use beamtalk_desktop_broker::spawn::{self, DEFAULT_BIND_FAILURE_GRACE, SpawnConfig, SpawnedFront};
use beamtalk_desktop_broker::{SpawnAttemptConfig, spawn_front_with_port_retry};

/// A wrapped front value that can be killed and reaped on drop —
/// implemented for both a plain [`Child`] (the bad-cookie test below spawns
/// one directly, bypassing `spawn::spawn_front`) and [`SpawnedFront`] (which
/// additionally drops the Windows job handle via its `Deref`/`DerefMut` to
/// `Child`).
trait Killable {
    fn kill_and_wait(&mut self);
}

impl Killable for Child {
    fn kill_and_wait(&mut self) {
        let _ = self.kill();
        let _ = self.wait();
    }
}

impl Killable for SpawnedFront {
    fn kill_and_wait(&mut self) {
        let _ = self.kill();
        let _ = self.wait();
    }
}

/// Kills the wrapped front on drop, including on a mid-test panic (an
/// `assert!` failure otherwise skips any explicit `child.kill()` below it,
/// leaking a live BEAM process — and its held port — past the failing test).
struct KillOnDrop<T: Killable>(T);

impl<T: Killable> Drop for KillOnDrop<T> {
    fn drop(&mut self) {
        self.0.kill_and_wait();
    }
}

/// Resolve a required env var, panicking with the setup instructions above
/// (not a silent skip — these tests are `#[ignore]`d specifically so they
/// only run when a caller has deliberately opted in and is expected to have
/// followed the module doc's setup steps first).
fn require_env(name: &str) -> String {
    std::env::var(name).unwrap_or_else(|_| {
        panic!(
            "{name} is not set — see crates/beamtalk-desktop-broker/tests/live_front.rs's \
             module doc comment for setup steps"
        )
    })
}

fn launcher() -> std::path::PathBuf {
    require_env("BT_DESKTOP_BROKER_LIVE_LAUNCHER").into()
}

fn workspace_id() -> String {
    require_env("BT_DESKTOP_BROKER_LIVE_WORKSPACE")
}

/// BT-2989: a workspace whose directory + cookie exist on disk but whose BEAM
/// node has been stopped — see the module doc's "BT-2989" section for how to
/// create one. Only read by the dead-workspace negative-path test below.
fn dead_workspace_id() -> String {
    require_env("BT_DESKTOP_BROKER_LIVE_DEAD_WORKSPACE")
}

/// Drive a spawned front's readiness to a terminal state, with a generous
/// overall timeout — used to force `ensure_distributed/0` to run (the front
/// only distributes lazily, on the first `/readiness` call) before we query
/// epmd.
fn drive_to_terminal(port: u16, timeouts: ProbeTimeouts) -> ReadinessState {
    readiness::wait_ready(
        ReadinessState::Spawning,
        Duration::from_secs(20),
        Duration::from_millis(100),
        readiness::http_probe("127.0.0.1", port, timeouts),
    )
}

/// BT-3004 acceptance criterion 1: spawn a real front against a real
/// workspace and confirm `sname::predict_node_name` (via
/// `predict_short_name`, epmd's own vocabulary) matches what epmd actually
/// reports once the front distributes.
///
/// Unix-only (BT-3045): `predict_node_name`'s pid prediction is *only*
/// verified correct on Unix (see `sname`'s module doc comment — Windows'
/// `Child::id()` reports `cmd.exe`'s pid, never `erl.exe`'s, since
/// `bin\bt_attach.bat` can only run via that console-subsystem wrapper). This
/// exact assertion is therefore *expected* to fail on Windows, by
/// construction — not a gap this test should paper over. See
/// `resolve_registered_node_name_matches_a_live_epmd_registration` below for
/// the cross-platform (and Windows-correct) replacement.
#[cfg(not(windows))]
#[test]
#[ignore = "needs a live dist-liveview release + running workspace — see module doc comment"]
fn predict_node_name_matches_a_live_epmd_registration() {
    let ws = workspace_id();
    let mut config = SpawnAttemptConfig::new(launcher(), ws.clone());
    config.bind_failure_grace = Duration::from_millis(300); // fast path, not under test here

    let (child, port) =
        spawn_front_with_port_retry(&config).expect("live front should spawn and bind");
    let pid = child.id();
    let _guard = KillOnDrop(child);

    // Force distribution to actually start (lazy, first-/readiness-call).
    let state = drive_to_terminal(port, ProbeTimeouts::default_local());
    assert!(
        matches!(state, ReadinessState::Ready(_)),
        "expected the live front to reach Ready against a real workspace, got {state:?}"
    );

    let names = beamtalk_workspace::epmd::query_epmd_names()
        .expect("epmd query should succeed with a real front now registered");
    let expected = predict_short_name(&ws, pid);
    assert!(
        names.contains(&expected),
        "predicted short name {expected:?} not found in epmd's registered names {names:?}"
    );
}

/// BT-3045: the Windows-correct replacement for the pid-based prediction
/// above — spawn a real front, drive it to `Ready` (forcing
/// `ensure_distributed/0` to actually run), then confirm
/// `sname::resolve_registered_node_name` finds it via a real epmd query.
/// Cross-platform (unlike the pid-based test above): this is exactly the
/// mechanism `desktop/src-tauri/src/commands.rs`'s
/// `update_windows_node_name_after_readiness` uses to correct a Windows
/// front record's placeholder `node_name` once readiness confirms `Ready`.
#[test]
#[ignore = "needs a live dist-liveview release + running workspace — see module doc comment"]
fn resolve_registered_node_name_matches_a_live_epmd_registration() {
    let ws = workspace_id();
    let mut config = SpawnAttemptConfig::new(launcher(), ws.clone());
    config.bind_failure_grace = Duration::from_millis(300); // fast path, not under test here

    let (child, port) =
        spawn_front_with_port_retry(&config).expect("live front should spawn and bind");
    let _guard = KillOnDrop(child);

    // Force distribution to actually start (lazy, first-/readiness-call).
    let state = drive_to_terminal(port, ProbeTimeouts::default_local());
    assert!(
        matches!(state, ReadinessState::Ready(_)),
        "expected the live front to reach Ready against a real workspace, got {state:?}"
    );

    let suffix = beamtalk_desktop_broker::sname::attach_node_suffix(&ws);
    let resolved = beamtalk_desktop_broker::sname::resolve_registered_node_name(&suffix)
        .expect("epmd query should succeed with a real front now registered")
        .expect("exactly one front should be registered under this workspace's suffix");
    assert!(
        resolved.starts_with(&format!("bt_attach_{suffix}_")) && resolved.ends_with("@localhost"),
        "resolved node name {resolved:?} doesn't look like a real bt_attach registration \
         for suffix {suffix:?}"
    );
}

/// BT-3004 acceptance criterion 2: deliberately trigger a bad-cookie attach
/// and confirm `/readiness`'s default timeout budget comfortably covers the
/// real response latency (whatever it turns out to be — this asserts the
/// *outcome*, not a specific millisecond figure, so it stays meaningful if a
/// future OTP/front change alters the actual latency).
///
/// Bypasses `spawn::spawn_front` deliberately: that function always resolves
/// the workspace's real on-disk cookie (see its doc comment), so producing a
/// bad-cookie scenario means invoking the launcher directly with an
/// overridden `BT_WORKSPACE_COOKIE`, the same shape the spike used for its
/// own negative-path tests.
#[test]
#[ignore = "needs a live dist-liveview release + running workspace — see module doc comment"]
fn bad_cookie_readiness_resolves_within_the_default_budget() {
    let ws = workspace_id();
    let node_name = format!("beamtalk_workspace_{ws}@localhost");
    let port = beamtalk_desktop_broker::port::find_free_port().expect("free port");

    let mut cmd = Command::new(launcher());
    cmd.env("BT_WORKSPACE_NODE", &node_name);
    cmd.env("BT_WORKSPACE_COOKIE", "bt3004-deliberately-wrong-cookie");
    cmd.env("PORT", port.to_string());
    cmd.env("BT_ATTACH_BIND_IP", "127.0.0.1");
    cmd.env("BT_ATTACH_NODE_SUFFIX", "bt3004_bad_cookie_probe");
    cmd.env("RELEASE_DISTRIBUTION", "none");
    let child = cmd
        .spawn()
        .expect("front should spawn even with a bad cookie");
    let _guard = KillOnDrop(child);

    let timeouts = ProbeTimeouts::default_local();
    let start = Instant::now();
    let state = drive_to_terminal(port, timeouts);
    let elapsed = start.elapsed();

    assert_eq!(
        state,
        ReadinessState::Failed(readiness::FailureReason::BadCookie),
        "expected a definitive BadCookie failure, got {state:?} after {elapsed:?}"
    );
    // Regression guard, not a tight bound: measured BT-3004 runs resolved in
    // well under 100ms on loopback (see readiness.rs's ProbeTimeouts doc
    // comment) — several seconds of margin is intentional headroom, not the
    // expected figure.
    assert!(
        elapsed < timeouts.http_up + timeouts.readiness,
        "bad-cookie readiness took {elapsed:?}, which exceeds the configured budget \
         ({:?} + {:?}) — ProbeTimeouts::default_local's calibration may need revisiting",
        timeouts.http_up,
        timeouts.readiness
    );
}

/// BT-3004 acceptance criterion 3: race two spawns on the same port and
/// measure how long the losing front takes to actually exit, to calibrate
/// `spawn::DEFAULT_BIND_FAILURE_GRACE`. A grace window shorter than this
/// measured exit latency would misclassify a real port conflict as
/// `SpawnAttempt::Bound` (see `DEFAULT_BIND_FAILURE_GRACE`'s doc comment).
#[test]
#[ignore = "needs a live dist-liveview release + running workspace — see module doc comment"]
fn a_real_port_conflict_exits_within_the_calibrated_grace_period() {
    let ws = workspace_id();
    let port = beamtalk_desktop_broker::port::find_free_port().expect("free port");

    // Front A: binds and holds the port.
    let winner_config = SpawnConfig::new(launcher(), ws.clone(), port);
    let mut winner = KillOnDrop(spawn::spawn_front(&winner_config).expect("winner should spawn"));
    // Give it a real chance to actually bind before racing the loser —
    // measured healthy boots reach HTTP-up in under 1s (see
    // DEFAULT_BIND_FAILURE_GRACE's doc comment).
    std::thread::sleep(Duration::from_secs(2));
    assert_eq!(
        winner.0.try_wait().expect("try_wait should succeed"),
        None,
        "winner should still be running (holding the port) before the race"
    );

    // Front B: loses the eaddrinuse race for the same port. Guarded too: if
    // the 30s assert below ever fires, the loser genuinely never crashed and
    // would otherwise leak a process holding this port.
    let loser_config = SpawnConfig::new(launcher(), ws, port);
    let mut loser = KillOnDrop(
        spawn::spawn_front(&loser_config).expect("loser should spawn (exec succeeds, bind fails)"),
    );

    let start = Instant::now();
    let exit_status = loop {
        if let Some(status) = loser.0.try_wait().expect("try_wait should succeed") {
            break status;
        }
        assert!(
            start.elapsed() < Duration::from_secs(30),
            "loser never exited within 30s — a real conflict should surface as a crash"
        );
        std::thread::sleep(Duration::from_millis(20));
    };
    let elapsed = start.elapsed();

    assert!(
        !exit_status.success(),
        "the losing front should exit non-zero (eaddrinuse crash), got {exit_status}"
    );
    assert!(
        elapsed <= DEFAULT_BIND_FAILURE_GRACE,
        "a real :eaddrinuse crash took {elapsed:?} to surface, which exceeds \
         spawn::DEFAULT_BIND_FAILURE_GRACE ({DEFAULT_BIND_FAILURE_GRACE:?}) — the grace \
         period would misclassify this conflict as SpawnAttempt::Bound and needs \
         recalibrating (see that constant's doc comment for the BT-3004 methodology)"
    );
}

/// BT-2989 acceptance criterion 1 (the positive-path half): the full
/// attach/detach lifecycle `desktop/src-tauri/src/commands.rs`'s `attach` and
/// `detach` commands drive — spawn a real front against a real running
/// workspace, confirm it reaches [`ReadinessState::Ready`], then kill it
/// (`detach`'s `Child::kill()` + `.wait()`) and confirm the OS process has
/// actually exited, not merely that the kill syscall returned.
/// `Child::wait()` blocking-waits for the real exit (not `try_wait`'s
/// point-in-time poll), so a `Some(status)` back from it already proves the
/// process is gone; this also cross-checks against epmd, which independently
/// confirms the dist registration a live process would hold is gone too —
/// two different signals agreeing rather than trusting the OS reap alone.
#[test]
#[ignore = "needs a live dist-liveview release + running workspace — see module doc comment"]
fn detach_kills_the_front_and_it_exits_cleanly() {
    let ws = workspace_id();
    let mut config = SpawnAttemptConfig::new(launcher(), ws.clone());
    config.bind_failure_grace = Duration::from_millis(300); // fast path, not under test here

    let (mut child, port) =
        spawn_front_with_port_retry(&config).expect("live front should spawn and bind");
    let pid = child.id();

    let state = drive_to_terminal(port, ProbeTimeouts::default_local());
    assert!(
        matches!(state, ReadinessState::Ready(_)),
        "expected the live front to reach Ready before detaching, got {state:?}"
    );
    let expected_sname = predict_short_name(&ws, pid);
    let names_before = beamtalk_workspace::epmd::query_epmd_names()
        .expect("epmd query should succeed while the front is up");
    assert!(
        names_before.contains(&expected_sname),
        "front should be epmd-registered as {expected_sname:?} while Ready, saw {names_before:?}"
    );

    // "detach": kill + wait, exactly what commands.rs's detach_internal /
    // kill_and_untrack do (see their doc comments) — no KillOnDrop guard
    // needed from here on, since this IS the cleanup this test is asserting
    // actually works, not a leak to guard against.
    child.kill().expect("kill should succeed on a live child");
    let status = child
        .wait()
        .expect("wait should succeed after a successful kill");
    assert!(
        !status.success(),
        "a killed front should report a non-zero/signalled exit status, got {status}"
    );

    // epmd deregistration is not synchronous with process death (the BEAM's
    // own net_kernel/epmd client needs a moment to notice the socket closed
    // and epmd to drop the registration), so poll rather than asserting
    // immediately after `wait()` returns — bounded, not a hang.
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        let names = beamtalk_workspace::epmd::query_epmd_names().unwrap_or_default();
        if !names.contains(&expected_sname) {
            break;
        }
        assert!(
            Instant::now() < deadline,
            "epmd still lists {expected_sname:?} 10s after the front process exited — \
             detach should leave no stale dist registration behind"
        );
        std::thread::sleep(Duration::from_millis(100));
    }
}

/// BT-2989 acceptance criterion 2: attaching to a **dead** workspace (one
/// whose `metadata.json`/`cookie` are still on disk — `spawn_front` only
/// checks the former exists, per its doc comment — but whose BEAM node is no
/// longer running) must surface the `/readiness` failure taxonomy
/// (`FailureReason::DeadWorkspace`) within the normal probe budget, never
/// hang indefinitely or crash the broker. This is the scenario
/// `commands.rs::attach_and_open_window` turns into
/// `Err(format!("workspace '{{id}}' is unreachable: {{reason:?}}"))`, which
/// `desktop/ui/main.js`'s `attach()` catch handler then renders into the
/// picker's status line — the UI-surfacing half of this criterion, not
/// re-tested here since it needs the actual Tauri command + webview (see
/// `desktop/e2e/README.md`).
///
/// Distinct from `bad_cookie_readiness_resolves_within_the_default_budget`
/// above: that test spawns against a **fabricated** node name with a
/// deliberately wrong cookie (no real workspace involved at all), which
/// exercises the `BadCookie` branch. This one spawns against a real,
/// previously-live workspace directory whose node has since been stopped —
/// the on-disk cookie is genuinely correct, so the dist handshake itself
/// would succeed if anything were listening; nothing is, which is exactly
/// what should resolve to `DeadWorkspace` rather than `BadCookie`.
#[test]
#[ignore = "needs a live dist-liveview release + a STOPPED (but not deleted) workspace — see module doc comment"]
fn dead_workspace_readiness_resolves_to_dead_workspace_not_a_hang() {
    let ws = dead_workspace_id();
    let mut config = SpawnAttemptConfig::new(launcher(), ws);
    config.bind_failure_grace = Duration::from_millis(300); // fast path, not under test here

    let (child, port) = spawn_front_with_port_retry(&config)
        .expect("front should spawn and bind HTTP even though the workspace is dead");
    let _guard = KillOnDrop(child);

    let timeouts = ProbeTimeouts::default_local();
    let start = Instant::now();
    let state = drive_to_terminal(port, timeouts);
    let elapsed = start.elapsed();

    assert_eq!(
        state,
        ReadinessState::Failed(readiness::FailureReason::DeadWorkspace),
        "expected a definitive DeadWorkspace failure, got {state:?} after {elapsed:?}"
    );
    // Same bound as the bad-cookie test above: a clear failure, not a hang.
    assert!(
        elapsed < timeouts.http_up + timeouts.readiness,
        "dead-workspace readiness took {elapsed:?}, which exceeds the configured budget \
         ({:?} + {:?}) — this should fail fast, not hang the broker before it can \
         surface the error to the picker UI",
        timeouts.http_up,
        timeouts.readiness
    );
}
