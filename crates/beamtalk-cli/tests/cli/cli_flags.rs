// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5045: one flag name per concept across subcommands.
//!
//! Three kinds of test live here:
//!
//! * **Renames**: the new spelling is accepted by clap, the old one is a clap
//!   "unexpected argument" error (no hidden aliases).
//! * **Conflicts**: flags that do nothing together are rejected by clap with a
//!   "cannot be used with" error, before any command runs.
//! * **Warning flags**: `--warnings-as-errors` / `--no-warnings` behave the
//!   same on `build`, `check` and `test`.
//!
//! The rename and conflict tests never get past argument parsing, so they
//! touch no workspace, build directory or home directory.

use crate::cli_common;

use predicates::prelude::PredicateBooleanExt;
use predicates::str::contains;

/// `beamtalk <args>` must fail in clap with `needle` on stderr.
fn assert_clap_rejects(args: &[&str], needle: &str) {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(args)
        .assert()
        .failure()
        .code(2)
        .stderr(contains(needle));
}

/// `beamtalk <args> --help` must succeed, i.e. the flags in `args` parse.
fn assert_clap_accepts(args: &[&str]) {
    let project = cli_common::fixture_project();
    let mut full: Vec<&str> = args.to_vec();
    full.push("--help");
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(full)
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// Renames: new spelling parses, old spelling is "unexpected argument"
// ---------------------------------------------------------------------------

#[test]
fn repl_idle_timeout_replaces_timeout() {
    // `--help` after the flags only proves the whole line parses.
    assert_clap_accepts(&["repl", "--idle-timeout", "300"]);
    assert_clap_rejects(&["repl", "--timeout", "300"], "unexpected argument");
}

#[test]
fn workspace_logs_takes_a_positional_name_not_a_flag() {
    assert_clap_accepts(&["workspace", "logs", "some-workspace"]);
    assert_clap_rejects(
        &["workspace", "logs", "--workspace", "some-workspace"],
        "unexpected argument",
    );
}

#[test]
fn workspace_list_uses_format_not_json() {
    assert_clap_accepts(&["workspace", "list", "--format", "json"]);
    assert_clap_rejects(&["workspace", "list", "--json"], "unexpected argument");
    assert_clap_rejects(
        &["workspace", "list", "--format", "yaml"],
        "invalid value 'yaml'",
    );
}

#[test]
fn workspace_logs_log_format_is_a_validated_enum() {
    assert_clap_accepts(&["workspace", "logs", "--log-format", "json"]);
    assert_clap_accepts(&["workspace", "logs", "--log-format", "TEXT"]);
    assert_clap_rejects(
        &["workspace", "logs", "--log-format", "yaml"],
        "invalid value 'yaml'",
    );
    // `--format` on `logs` was the log-file format; it is gone, not aliased.
    assert_clap_rejects(
        &["workspace", "logs", "--format", "json"],
        "unexpected argument",
    );
}

// ---------------------------------------------------------------------------
// Conflicts
// ---------------------------------------------------------------------------

#[test]
fn repl_foreground_conflicts_with_workspace_flags() {
    for flag in [
        &["--persistent"][..],
        &["--ephemeral"][..],
        &["--workspace", "w"][..],
        &["--idle-timeout", "60"][..],
    ] {
        let mut args = vec!["repl", "--foreground"];
        args.extend_from_slice(flag);
        assert_clap_rejects(&args, "cannot be used with");
    }
}

#[test]
fn repl_persistent_conflicts_with_ephemeral() {
    assert_clap_rejects(
        &["repl", "--persistent", "--ephemeral"],
        "cannot be used with",
    );
}

#[test]
fn no_warnings_conflicts_with_warnings_as_errors_everywhere() {
    for command in [
        "build",
        "check",
        "test",
        "build-stdlib",
        "test-script",
        "test-docs",
        "test-metamorphic",
    ] {
        assert_clap_rejects(
            &[command, "--no-warnings", "--warnings-as-errors"],
            "cannot be used with",
        );
    }
}

#[test]
fn warning_flags_are_accepted_by_every_compile_command() {
    for command in [
        "build",
        "check",
        "test",
        "build-stdlib",
        "test-script",
        "test-docs",
        "test-metamorphic",
    ] {
        assert_clap_accepts(&[command, "--no-warnings"]);
        assert_clap_accepts(&[command, "--warnings-as-errors"]);
    }
}

#[test]
fn doc_docs_path_requires_site() {
    assert_clap_rejects(&["doc", "--docs-path", "guides"], "--site");
    assert_clap_accepts(&["doc", "--site", "--docs-path", "guides"]);
    // Without the flag the default applies and nothing is required.
    assert_clap_accepts(&["doc"]);
}

// ---------------------------------------------------------------------------
// Warning flags change behaviour, not just parsing
// ---------------------------------------------------------------------------

/// A project whose only diagnostic is a DNU *hint* (`42 frobnicate`): it
/// compiles, so only the warning flags decide the outcome.
fn project_with_a_warning() -> tempfile::TempDir {
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("src/Probe.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Value subclass: Probe\n\
         \n\
         \x20\x20go => 42 frobnicate\n",
    )
    .expect("write src/Probe.bt");
    project
}

const WARNING_TEXT: &str = "does not understand 'frobnicate'";

/// Each call gets its own project: `build` replays cached diagnostics for
/// unchanged files, so reusing one project would hide the flag under test.
fn run_in_warning_project(args: &[&str]) -> assert_cmd::assert::Assert {
    let project = project_with_a_warning();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(args)
        .assert()
}

#[test]
fn build_shows_warnings_and_succeeds_by_default() {
    run_in_warning_project(&["build"])
        .success()
        .stderr(contains(WARNING_TEXT));
}

#[test]
fn build_warnings_as_errors_fails_on_a_warning() {
    run_in_warning_project(&["build", "--warnings-as-errors"])
        .failure()
        .stderr(contains(WARNING_TEXT));
}

#[test]
fn build_no_warnings_suppresses_the_warning() {
    run_in_warning_project(&["build", "--no-warnings"])
        .success()
        .stderr(contains(WARNING_TEXT).not());
}

#[test]
fn check_shows_warnings_and_succeeds_by_default() {
    run_in_warning_project(&["check"])
        .success()
        .stderr(contains(WARNING_TEXT));
}

#[test]
fn check_warnings_as_errors_fails_on_a_warning() {
    run_in_warning_project(&["check", "--warnings-as-errors"])
        .failure()
        .stderr(contains(WARNING_TEXT));
}

#[test]
fn check_no_warnings_suppresses_the_warning() {
    run_in_warning_project(&["check", "--no-warnings"])
        .success()
        .stderr(contains(WARNING_TEXT).not());
}

#[test]
fn test_shows_the_warning_by_default_and_no_warnings_suppresses_it() {
    run_in_warning_project(&["test", "--quiet"])
        .success()
        .stderr(contains(WARNING_TEXT));
    run_in_warning_project(&["test", "--quiet", "--no-warnings"])
        .success()
        .stderr(contains(WARNING_TEXT).not());
}

// ---------------------------------------------------------------------------
// Warm caches: `build` replays an unchanged file's cached diagnostics, and
// `--warnings-as-errors` must judge the replayed ones too (CI runs on warm
// caches).
// ---------------------------------------------------------------------------

#[test]
fn build_warnings_as_errors_fails_on_every_run_with_a_warm_cache() {
    let project = project_with_a_warning();
    let build = |args: &[&str]| {
        cli_common::beamtalk()
            .current_dir(project.path())
            .args(args)
            .assert()
    };

    // A plain build warms the cache: the warning is reported, the build passes.
    build(&["build"]).success().stderr(contains(WARNING_TEXT));

    // Same sources, now gated: the cached warning must fail the build, each time.
    for _ in 0..2 {
        build(&["build", "--warnings-as-errors"])
            .failure()
            .stderr(contains(WARNING_TEXT));
    }

    // `--no-warnings` on the warm cache still passes and stays quiet.
    build(&["build", "--no-warnings"])
        .success()
        .stderr(contains(WARNING_TEXT).not());
}

#[test]
fn test_warnings_as_errors_fails_with_a_warm_build_cache() {
    // `test` builds the package first; that build replays cached diagnostics.
    let project = project_with_a_warning();
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test", "--quiet", "--warnings-as-errors"])
        .assert()
        .failure()
        .stderr(contains(WARNING_TEXT));
}

#[test]
fn check_warnings_as_errors_fails_on_every_run() {
    // `check` never replays a cache (always a cold full check); pin that.
    let project = project_with_a_warning();
    for _ in 0..2 {
        cli_common::beamtalk()
            .current_dir(project.path())
            .args(["check", "--warnings-as-errors"])
            .assert()
            .failure()
            .stderr(contains(WARNING_TEXT));
    }
}
