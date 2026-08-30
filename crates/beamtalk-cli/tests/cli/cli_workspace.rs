// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk workspace {list,status,stop,create}`
//! (BT-3326).
//!
//! `workspace create` (without `--background`) and a bare `workspace list`
//! success path both write real state under the machine's `~/.beamtalk`
//! directory — there is no `BEAMTALK_HOME`-style override to redirect that
//! into a hermetic tempdir (tracked as a follow-up), so this file sticks to
//! read-only and validation-only paths that never persist anything:
//! `list`/`status` against workspaces that don't exist, and the
//! `create --background` flag-validation errors that fire *before* any
//! workspace state is written.

use crate::cli_common;

use predicates::str::contains;

#[test]
fn list_json_produces_a_json_array() {
    // Read-only; does not depend on whether any workspaces exist on the
    // machine already, so this only asserts the output parses as an array.
    let project = cli_common::fixture_project();
    let output = cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "list", "--json"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();

    let parsed: serde_json::Value =
        serde_json::from_slice(&output).expect("workspace list --json should print valid JSON");
    assert!(parsed.is_array(), "expected a JSON array, got: {parsed}");
}

#[test]
fn list_text_output_succeeds() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "list"])
        .assert()
        .success();
}

#[test]
fn status_with_unknown_workspace_name_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "status", "no-such-workspace-12345"])
        .assert()
        .failure()
        .stderr(contains("does not exist"));
}

#[test]
fn stop_with_unknown_workspace_name_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "stop", "no-such-workspace-12345"])
        .assert()
        .failure()
        .stderr(contains("does not exist"));
}

#[test]
fn create_background_with_invalid_bind_address_errors_before_writing_state() {
    // `resolve_bind_addr` rejects a non-IP string before any workspace
    // metadata is written (create_workspace/get_or_start_workspace are never
    // reached), so this is safe to run without touching real state.
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args([
            "workspace",
            "create",
            "bt-3326-test-fixture",
            "--background",
            "--bind",
            "not-an-ip-address",
        ])
        .assert()
        .failure()
        .stderr(contains("Invalid bind address"));
}

#[test]
fn create_background_with_non_loopback_bind_requires_confirmation() {
    // `validate_network_binding` rejects a non-loopback bind without
    // `--confirm-network` before any workspace metadata is written.
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args([
            "workspace",
            "create",
            "bt-3326-test-fixture-2",
            "--background",
            "--bind",
            "0.0.0.0",
        ])
        .assert()
        .failure()
        .stderr(contains("confirm-network"));
}
