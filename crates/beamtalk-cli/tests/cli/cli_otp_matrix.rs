// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Drift test for the OTP support window (ADR 0125 §3.1).
//!
//! `just otp-matrix` (consumed by `ci.yml`'s `matrix.otp`) is a thin
//! wrapper over the hidden `beamtalk otp-matrix` subcommand
//! (`cargo run --bin beamtalk --quiet -- otp-matrix`) — this drives that
//! same compiled binary and asserts its JSON output equals the declared
//! window (`otp-support.toml`, read via `beamtalk_cli::otp_support::window`),
//! so the three consumers (doctor, build/release, CI matrix) can never
//! drift apart (CLAUDE.md: no "keep in sync" comment without a test).

use crate::cli_common;

#[test]
fn otp_matrix_subcommand_matches_declared_window() {
    let output = cli_common::beamtalk()
        .arg("otp-matrix")
        .output()
        .expect("run beamtalk otp-matrix");
    assert!(output.status.success(), "otp-matrix should exit 0");

    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
    assert_eq!(
        stdout,
        beamtalk_cli::otp_support::majors_as_json_array(),
        "`beamtalk otp-matrix` (what `just otp-matrix` prints for ci.yml's matrix.otp) must \
         equal the window declared in otp-support.toml"
    );
}

#[test]
fn otp_matrix_output_is_well_formed_json_array_of_strings() {
    let output = cli_common::beamtalk()
        .arg("otp-matrix")
        .output()
        .expect("run beamtalk otp-matrix");
    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();

    // e.g. ["27","28"] — every element a quoted major, ascending.
    assert!(stdout.starts_with('['), "got: {stdout}");
    assert!(stdout.ends_with(']'), "got: {stdout}");
    let window = beamtalk_cli::otp_support::window();
    for major in window.majors() {
        assert!(
            stdout.contains(&format!("\"{major}\"")),
            "expected major {major} in {stdout}"
        );
    }
}
