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
//! **DDD Context:** Desktop Shell

use std::process::{Child, Command};
use std::time::{Duration, Instant};

use beamtalk_desktop_broker::readiness::{self, ProbeTimeouts, ReadinessState};
use beamtalk_desktop_broker::sname::predict_short_name;
use beamtalk_desktop_broker::spawn::{self, DEFAULT_BIND_FAILURE_GRACE, SpawnConfig};
use beamtalk_desktop_broker::{SpawnAttemptConfig, spawn_front_with_port_retry};

/// Kills the wrapped front on drop, including on a mid-test panic (an
/// `assert!` failure otherwise skips any explicit `child.kill()` below it,
/// leaking a live BEAM process — and its held port — past the failing test).
struct KillOnDrop(Child);

impl Drop for KillOnDrop {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
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
