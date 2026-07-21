// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk run` (BT-2084).
//!
//! Covers the script-mode entry-point invocation. Service mode (`run .`)
//! requires a running BEAM workspace and is exercised by the e2e tests.
//! Connected mode (`run ... --connect`) is covered by an `#[ignore]`d
//! end-to-end test below that boots a real workspace (BT-2890).

use crate::cli_common;

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
        // BT-2702: status/progress lines go to stderr, keeping stdout clean for
        // the program's own output (the entry's return value is discarded here).
        .stdout(contains("Running Smoke>>run").not())
        .stdout(contains("Building...").not())
        .stderr(contains("Running Smoke>>run"))
        .stderr(contains("Building..."));
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

// ---------------------------------------------------------------------------
// BT-2890: connected mode (`--connect`) status lines must go to stderr
// ---------------------------------------------------------------------------

/// Stops the fixture project's workspace on drop, so a failed assertion in
/// the test body doesn't leak a detached BEAM node between test runs.
struct WorkspaceStopGuard {
    project_dir: std::path::PathBuf,
}

impl Drop for WorkspaceStopGuard {
    fn drop(&mut self) {
        let _ = cli_common::beamtalk()
            .current_dir(&self.project_dir)
            .args(["workspace", "stop"])
            .timeout(std::time::Duration::from_secs(60))
            .output();
    }
}

#[test]
#[ignore = "requires beamtalk binary and erlang runtime (boots a live workspace, slow)"]
fn run_connected_status_lines_go_to_stderr_not_stdout() {
    // BT-2890 (BT-2702 follow-up): `run_connected`'s two status lines —
    // "Connecting to workspace ..." and "Running ... (connected)..." — must
    // land on stderr, keeping stdout clean for the program's own output.
    // `cli_run.rs`'s script-mode test covers the `run_script` path; this
    // covers the `--connect` path against a real shared workspace.
    let project = cli_common::fixture_project();
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

    // Boot the project's shared workspace via a headless REPL: piping "exit"
    // lets the CLI build the project, start the detached workspace node, and
    // return while the node stays up (the temp project dir gives this test a
    // unique workspace ID, so parallel runs don't collide).
    let _guard = WorkspaceStopGuard {
        project_dir: project.path().to_path_buf(),
    };
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("repl")
        .write_stdin("exit\n")
        .timeout(std::time::Duration::from_secs(120))
        .assert()
        .success();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["run", "Smoke", "run", "--connect"])
        .timeout(std::time::Duration::from_secs(60))
        .assert()
        .success()
        // Status lines must NOT leak onto stdout ...
        .stdout(contains("Connecting to workspace").not())
        .stdout(contains("Running Smoke>>run (connected)").not())
        // ... and must appear on stderr.
        .stderr(contains("Connecting to workspace"))
        .stderr(contains("Running Smoke>>run (connected)"));
}
