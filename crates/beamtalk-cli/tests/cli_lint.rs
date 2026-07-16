// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk lint` (BT-2084).
//!
//! Verifies exit codes, stdout JSON shape, and stderr text output for both
//! happy- and error-path cases. Uses `assert_cmd` against the built
//! `beamtalk` binary and a hermetic fixture project per test.

mod cli_common;

use predicates::prelude::*;
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
fn expect_type_on_ffi_arg_mismatch_is_not_stale_across_lint_and_build_bt_2851() {
    // BT-2851: `beamtalk lint`'s FFI arg-type check and the compiler's own
    // diagnostic pass (used by `build`/`test`) must agree on which
    // diagnostics an `@expect type` annotation suppresses. Before this fix,
    // lint populated its native-type registry by reading whatever
    // `_build/type_cache/` happened to hold (written by a *previous*
    // `beamtalk build`), while build always extracted live. On a project
    // that had never been built — the case here — lint's cache read
    // silently returned no registry, so it skipped the FFI arg-type check
    // build performs; an `@expect type` written for that build-time
    // diagnostic was then flagged "stale @expect" by lint even though it
    // legitimately suppressed the same diagnostic on the build surface.
    //
    // `Erlang lists reverse: 42` is a genuine FFI positional arg-type
    // mismatch (`lists:reverse/1` expects `List`) that both surfaces detect
    // via `check_ffi_argument_types` once they share a registry.
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("src/FfiMismatch.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: FfiMismatch\n\
         \n\
         \x20\x20@expect type\n\
         \x20\x20class badCall => Erlang lists reverse: 42\n",
    )
    .unwrap();

    // No `_build/` exists yet — the cold-cache case that originally
    // triggered the false "stale @expect" from lint.
    assert!(!project.path().join("_build").exists());

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("lint")
        .assert()
        .success()
        .stderr(contains("stale @expect").not());

    // Same project, now built — the compiler's own diagnostic pass must
    // agree with lint: the `@expect type` is not stale there either.
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(contains("stale @expect").not());
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
