// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk run` (BT-2084).
//!
//! Covers the script-mode entry-point invocation. Service mode (`run .`)
//! requires a running BEAM workspace and is exercised by the e2e tests.

mod cli_common;

use predicates::prelude::*;
use predicates::str::contains;

#[test]
fn run_script_mode_invokes_class_method() {
    let project = cli_common::fixture_project();
    // Add a class with a class method that returns a value `run` can invoke.
    std::fs::write(
        project.path().join("src/Smoke.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Smoke\n\
         \n\
         \x20\x20class run => 21 + 21\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["run", "Smoke", "run"])
        .assert()
        .success()
        .stdout(contains("Running Smoke>>run").or(contains("Smoke")));
}

#[test]
fn run_without_manifest_errors() {
    // `run` requires a beamtalk.toml in the cwd.
    let empty = tempfile::tempdir().unwrap();
    cli_common::beamtalk()
        .current_dir(empty.path())
        .args(["run", "Smoke", "run"])
        .assert()
        .failure()
        .stderr(contains("beamtalk.toml"));
}

#[test]
fn run_class_without_selector_errors() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["run", "Greeter"])
        .assert()
        .failure()
        .stderr(contains("selector").or(contains("Missing")));
}

#[test]
fn run_dot_without_application_section_errors() {
    // The fixture is a library, not an [application]; `run .` should bail
    // with an actionable error pointing the user at `[application]`.
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["run", "."])
        .assert()
        .failure()
        .stderr(contains("entry point").or(contains("[application]")));
}
