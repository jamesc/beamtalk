// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk --version` (BT-4812).
//!
//! The build identity is baked in at compile time, so the three build cases
//! (release, edge, no git metadata) are covered by `compose_build_id`'s unit
//! tests in `beamtalk-build`. These tests pin what survives into the binary:
//! the leading `beamtalk <semver>` prefix that `install.sh`, satellite CI and
//! other tooling may parse, and agreement with the shared `build_id()`.

use crate::cli_common;

#[test]
fn version_flag_prints_build_id_after_name() {
    let out = cli_common::beamtalk().arg("--version").output().unwrap();
    assert!(out.status.success());
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(
        stdout.trim_end(),
        format!("beamtalk {}", beamtalk_repl_protocol::handshake::build_id())
    );
}

#[test]
fn version_output_keeps_leading_semver() {
    let out = cli_common::beamtalk().arg("--version").output().unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    let rest = stdout
        .strip_prefix("beamtalk ")
        .expect("output must start with `beamtalk `");
    let semver: String = rest
        .chars()
        .take_while(|c| c.is_ascii_digit() || *c == '.')
        .collect();
    let parts: Vec<&str> = semver.split('.').collect();
    assert_eq!(parts.len(), 3, "leading X.Y.Z expected in {stdout:?}");
    assert!(parts.iter().all(|p| p.parse::<u32>().is_ok()), "{stdout:?}");
    assert_eq!(semver, env!("CARGO_PKG_VERSION"));
}
