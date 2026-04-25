// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk workspace transcript` (BT-2084).
//!
//! `transcript` requires a running workspace to actually stream entries.
//! These tests cover the input-validation surface — error paths the user
//! sees when they invoke transcript without a workspace, or against a
//! workspace name that doesn't exist. Streaming behaviour is covered by
//! e2e tests against a live workspace.

mod cli_common;

use predicates::prelude::*;
use predicates::str::contains;

#[test]
fn transcript_without_workspace_in_cwd_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "transcript"])
        .assert()
        .failure()
        .stderr(contains("workspace").and(contains("beamtalk repl")));
}

#[test]
fn transcript_with_unknown_workspace_name_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "transcript", "no-such-workspace-12345"])
        .assert()
        .failure()
        .stderr(contains("does not exist").or(contains("workspace")));
}
