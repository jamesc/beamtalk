// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk workspace logs` (BT-3326).
//!
//! `logs` is a pure file-reading command, but reading the actual log file
//! of a running workspace needs a live node. These tests cover the
//! resolution/validation surface reachable without one: missing workspace,
//! unknown workspace name, and invalid `--level`/`--format` argument values.

use crate::cli_common;

use predicates::str::contains;

#[test]
fn logs_without_workspace_in_cwd_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "logs"])
        .assert()
        .failure()
        .stderr(contains("No workspace found"));
}

#[test]
fn logs_with_unknown_workspace_name_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args([
            "workspace",
            "logs",
            "--workspace",
            "no-such-workspace-12345",
        ])
        .assert()
        .failure()
        .stderr(contains("does not exist"));
}

#[test]
fn logs_with_invalid_level_errors_before_resolving_workspace() {
    // Level/format parsing happens before workspace resolution, so this
    // should fail on the bad `--level` value even with no workspace present.
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "logs", "--level", "catastrophic"])
        .assert()
        .failure()
        .stderr(contains("Unknown log level"));
}

#[test]
fn logs_with_invalid_format_errors_before_resolving_workspace() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "logs", "--format", "yaml"])
        .assert()
        .failure()
        .stderr(contains("Unknown log format"));
}

#[test]
fn logs_valid_level_and_format_still_fails_without_a_workspace() {
    // Sanity check that valid --level/--format values pass parsing and the
    // command still correctly falls through to workspace resolution.
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args([
            "workspace",
            "logs",
            "--level",
            "warning",
            "--format",
            "json",
        ])
        .assert()
        .failure()
        .stderr(contains("No workspace found"));
}
