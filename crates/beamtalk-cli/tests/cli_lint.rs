// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk lint` (BT-2084).
//!
//! Verifies exit codes, stdout JSON shape, and stderr text output for both
//! happy- and error-path cases. Uses `assert_cmd` against the built
//! `beamtalk` binary and a hermetic fixture project per test.

mod cli_common;

use predicates::str::contains;

#[test]
fn lint_clean_project_text_format_exits_zero() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("lint")
        .assert()
        .success();
}

#[test]
fn lint_clean_project_json_format_emits_summary() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["lint", "--format=json"])
        .assert()
        .success()
        // JSON line summary is on stdout; text summary goes to stderr.
        // For a clean project we get the summary object only.
        .stdout(contains("\"type\":\"summary\""))
        .stdout(contains("\"total\":0"));
}

#[test]
fn lint_dirty_file_text_format_fails() {
    let project = cli_common::fixture_project();
    // Trigger a real lint (BT-948 unnecessary trailing `.` is Severity::Lint).
    std::fs::write(
        project.path().join("src/Bad.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Bad\n\
         \x20\x20greet => \"hi\".\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("lint")
        .assert()
        .failure()
        .stderr(contains("lint diagnostic"));
}

#[test]
fn lint_dirty_file_json_format_emits_per_diag_lines() {
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("src/Bad.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Bad\n\
         \x20\x20greet => \"hi\".\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["lint", "--format=json"])
        .assert()
        .failure()
        // Per-diagnostic JSON lines are emitted to stdout (BT-2031).
        .stdout(contains("\"severity\":\"lint\""))
        .stdout(contains("\"type\":\"summary\""));
}

#[test]
fn lint_missing_path_exits_nonzero() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["lint", "does/not/exist.bt"])
        .assert()
        .failure()
        .stderr(predicates::str::is_match("does not exist|not found").unwrap());
}
