// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk workspace attach` (BT-3326).
//!
//! `attach` requires a running workspace to actually connect to a REPL
//! backend. These tests cover the input-validation surface — error paths
//! the user sees when they invoke attach without a workspace, against an
//! unknown workspace name, or with an inconsistent `--port`/`--cookie`
//! combination. Connecting to a live workspace is covered by e2e tests.

use crate::cli_common;

use predicates::prelude::*;
use predicates::str::contains;

#[test]
fn attach_without_workspace_in_cwd_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "attach"])
        .assert()
        .failure()
        .stderr(contains("No workspace found").or(contains("beamtalk workspace attach")));
}

#[test]
fn attach_with_unknown_workspace_name_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "attach", "no-such-workspace-12345"])
        .assert()
        .failure()
        .stderr(contains("does not exist"));
}

#[test]
fn attach_with_port_but_no_cookie_errors() {
    // clap's `requires = "port"` is declared on --cookie (cookie requires
    // port), not the other way around, so --port alone passes clap's own
    // validation — the friendlier "a --cookie is required" message comes
    // from attach::run's own check.
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["workspace", "attach", "--port", "54321"])
        .assert()
        .failure()
        .stderr(contains("cookie"));
}

#[test]
fn attach_with_port_and_cookie_to_nothing_listening_errors() {
    // Bind then immediately drop to get a port nothing is listening on.
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let port = listener.local_addr().expect("local_addr").port();
    drop(listener);

    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args([
            "workspace",
            "attach",
            "--port",
            &port.to_string(),
            "--cookie",
            "test-cookie",
        ])
        .assert()
        .failure()
        .stderr(contains("Failed to connect"));
}

#[test]
fn attach_port_conflicts_with_positional_name() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args([
            "workspace",
            "attach",
            "some-name",
            "--port",
            "54321",
            "--cookie",
            "x",
        ])
        .assert()
        .failure()
        .stderr(contains("cannot be used with").or(contains("conflicts")));
}
