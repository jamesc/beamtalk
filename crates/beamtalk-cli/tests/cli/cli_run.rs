// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk run`.
//!
//! Covers the script-mode entry-point invocation. Connected mode
//! (`run ... --connect`) and service mode (`run .`) both need a live BEAM
//! workspace, so each is covered by an `#[ignore]`d end-to-end test below
//! that boots a real one.

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
        // Status/progress lines go to stderr, keeping stdout clean for
        // the program's own output (the entry's return value is discarded here).
        .stdout(contains("Running Smoke>>run").not())
        .stdout(contains("Building...").not())
        .stderr(contains("Running Smoke>>run"))
        .stderr(contains("Building..."));
}

#[test]
fn run_script_mode_dispatches_subdirectory_class_by_name() {
    // ADR 0119 Phase 3: package-compiler/e2e regression test for the
    // subdirectory-dispatch bug this guards against.
    //
    // A reference from a root-level class to a class declared in a
    // package subdirectory (`src/scheme/SchemeEnv.bt`) must not compile to
    // the wrong module name: reverse-parsing an already-computed module
    // name to guess a package prefix would discard subdirectory segments
    // (e.g. its `bt@sicp@scheme@eval` -> `bt@sicp@` example), making the
    // generated call target a nonexistent module and fail at runtime with
    // `undef`. `compiled_module_name` resolves such references through the
    // shared `ClassModuleRegistry`, built from the real, parsed file paths
    // (Pass 1) — no guessing, no dropped subdirectory segment.
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("src/scheme")).expect("mkdir src/scheme");
    std::fs::write(
        project.path().join("src/scheme/SchemeEnv.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         /// A class declared in a package subdirectory (BT-3437 regression fixture).\n\
         Value subclass: SchemeEnv\n\
         \n\
         \x20\x20greet => \"hello-from-subdir\"\n",
    )
    .expect("write src/scheme/SchemeEnv.bt");
    // A root-level class referencing the subdirectory class by bare name —
    // exactly the reference shape `user_package_prefix` mis-resolved.
    std::fs::write(
        project.path().join("src/Runner.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Runner\n\
         \n\
         \x20\x20class run => Console printLine: SchemeEnv new greet\n",
    )
    .expect("write src/Runner.bt");

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["run", "Runner", "run"])
        .assert()
        .success()
        .stdout(contains("hello-from-subdir"));
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
// Connected mode (`--connect`) stream split — the program's
// output on stdout, status lines on stderr
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
    // `run_connected`'s two status lines —
    // "Connecting to workspace ..." and "Running ... (connected)..." — must
    // land on stderr, keeping stdout clean for the program's own output.
    // `cli_run.rs`'s script-mode test covers the `run_script` path; this
    // covers the `--connect` path against a real shared workspace.
    //
    // The other half of that split — the entry's own `Console` output
    // must actually *reach* stdout: the entry runs in its class's
    // gen_server, which keeps the node's group leader, so without routing
    // the writes would never reach the dispatching session's IO capture.
    // `Helper shout` guards the nested hop as well, since a class method
    // calling another class method has to re-propagate the sink to keep
    // streaming.
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("src/Helper.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Helper\n\
         \n\
         \x20\x20class shout => Console printLine: \"connected-nested-99\"\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("src/Smoke.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Smoke\n\
         \n\
         \x20\x20class run =>\n\
         \x20\x20\x20\x20Console printLine: \"connected-output-42\".\n\
         \x20\x20\x20\x20Helper shout\n",
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
        // ... they belong on stderr ...
        .stderr(contains("Connecting to workspace"))
        .stderr(contains("Running Smoke>>run (connected)"))
        // ... and stdout carries the program's own output, in order, from both
        // the entry and the class method it calls.
        .stdout(contains("connected-output-42"))
        .stdout(contains("connected-nested-99"))
        .stdout(predicate::function(|out: &str| {
            out.find("connected-output-42") < out.find("connected-nested-99")
        }))
        // Exactly once: `do_dispatch` also returns the captured output in the
        // terminal reply, so a client that both streams and prints that fallback
        // would emit every line twice.
        .stdout(predicate::function(|out: &str| {
            out.matches("connected-output-42").count() == 1
                && out.matches("connected-nested-99").count() == 1
        }))
        // The program's output must not be duplicated onto stderr.
        .stderr(contains("connected-output-42").not());
}

// ---------------------------------------------------------------------------
// Service mode (`run .`) stream split — the third run mode joins
// script and connected mode in keeping status lines off stdout
// ---------------------------------------------------------------------------

#[test]
#[ignore = "requires beamtalk binary and erlang runtime (boots a live workspace, slow)"]
fn run_service_mode_status_lines_go_to_stderr_not_stdout() {
    // `run_package_as_otp_application`'s status
    // banner — "Building..." and the "Started <pkg> v<ver> / Supervisor /
    // REPL port" block — must land on stderr, matching the script-mode split
    // asserted by `run_script_mode_invokes_class_method` above. Service mode
    // has no dispatched program output of its own to protect, but leaving it
    // on stdout made `beamtalk run`'s stream contract mode-dependent.
    let project = cli_common::fixture_project();

    // Turn the library fixture into an OTP application: `run .` requires an
    // `[application]` section naming a root supervisor.
    std::fs::write(
        project.path().join("beamtalk.toml"),
        "# Copyright 2026 James Casey\n\
         # SPDX-License-Identifier: Apache-2.0\n\
         \n\
         [package]\n\
         name = \"cli_subprocess_fixture\"\n\
         version = \"0.1.0\"\n\
         \n\
         [application]\n\
         supervisor = \"SmokeSup\"\n\
         \n\
         [dependencies]\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("src/SmokeSup.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Supervisor subclass: SmokeSup\n\
         \n\
         \x20\x20class children => #()\n",
    )
    .unwrap();

    // `run .` leaves a detached workspace node running; stop it on drop so a
    // failed assertion doesn't leak a BEAM node between test runs.
    let _guard = WorkspaceStopGuard {
        project_dir: project.path().to_path_buf(),
    };

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["run", "."])
        .timeout(std::time::Duration::from_secs(120))
        .assert()
        .success()
        // Status lines must NOT leak onto stdout ...
        .stdout(contains("Building...").not())
        .stdout(contains("Started cli_subprocess_fixture").not())
        .stdout(contains("REPL port").not())
        // ... they belong on stderr.
        .stderr(contains("Building..."))
        .stderr(contains("Started cli_subprocess_fixture"))
        .stderr(contains("REPL port"));
}
