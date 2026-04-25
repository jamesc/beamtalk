// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk doctor` (BT-2084).
//!
//! Drives the doctor against:
//! * the real workspace runtime (happy path) — every required check passes,
//! * a synthesized broken runtime directory (error path) — stdlib/runtime
//!   checks fail and the command exits non-zero with actionable output.

mod cli_common;

use predicates::prelude::*;
use predicates::str::contains;

#[test]
fn doctor_passes_against_real_workspace_runtime() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("doctor")
        .assert()
        .success()
        .stdout(contains("Erlang/OTP"))
        .stdout(contains("All checks passed").or(contains("required checks passed")));
}

#[test]
fn doctor_reports_failure_for_broken_runtime() {
    let project = cli_common::fixture_project();

    // Point BEAMTALK_RUNTIME_DIR at an empty dir — the runtime layout check
    // will fail because no apps/ or _build/ tree exists.
    let broken = tempfile::tempdir().expect("broken-runtime tempdir");

    cli_common::beamtalk()
        // Override the env var the harness pre-sets to point at a broken dir.
        .env("BEAMTALK_RUNTIME_DIR", broken.path())
        .current_dir(project.path())
        .arg("doctor")
        .assert()
        .failure()
        .stderr(contains("doctor found problems"));
}

#[test]
fn doctor_dev_flag_includes_developer_checks() {
    let project = cli_common::fixture_project();
    // The `--dev` flag adds rebar3/just/rustc checks. We don't assert success
    // for these since the test environment may lack `just`. We *do* assert
    // that the developer-tool labels appear in stdout.
    let output = cli_common::beamtalk()
        .current_dir(project.path())
        .args(["doctor", "--dev"])
        .output()
        .expect("run doctor --dev");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("rustc") || stdout.contains("rebar3") || stdout.contains("just"),
        "doctor --dev should mention at least one developer tool; got:\n{stdout}"
    );
}
