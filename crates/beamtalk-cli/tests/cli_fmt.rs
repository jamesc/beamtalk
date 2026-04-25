// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk fmt` and `beamtalk fmt-check` (BT-2084).
//!
//! Verifies in-place rewriting, unified diff output, and exit codes.

mod cli_common;

use predicates::prelude::*;
use predicates::str::contains;

const UNFORMATTED: &str = "// Copyright 2026 James Casey\n\
                          // SPDX-License-Identifier: Apache-2.0\n\
                          \n\
                          Object subclass:    Bad\n\
                          \x20\x20hello   =>   \"hi\"\n";

const FORMATTED_PREFIX: &str = "// Copyright 2026 James Casey\n\
                                // SPDX-License-Identifier: Apache-2.0\n\
                                \n\
                                Object subclass: Bad\n";

#[test]
fn fmt_check_clean_project_succeeds() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("fmt-check")
        .assert()
        .success();
}

#[test]
fn fmt_check_dirty_file_emits_diff_and_fails() {
    let project = cli_common::fixture_project();
    let bad = project.path().join("src/Bad.bt");
    std::fs::write(&bad, UNFORMATTED).unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("fmt-check")
        .assert()
        .failure()
        // Unified diff markers — `similar` emits `+++` / `---` headers.
        .stdout(contains("+++").or(contains("---")))
        .stderr(contains("would be reformatted"));

    // fmt-check must not modify files.
    let still_dirty = std::fs::read_to_string(&bad).unwrap();
    assert_eq!(still_dirty, UNFORMATTED);
}

#[test]
fn fmt_rewrites_files_in_place() {
    let project = cli_common::fixture_project();
    let bad = project.path().join("src/Bad.bt");
    std::fs::write(&bad, UNFORMATTED).unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("fmt")
        .assert()
        .success();

    let after = std::fs::read_to_string(&bad).unwrap();
    assert!(
        after.starts_with(FORMATTED_PREFIX),
        "fmt should have rewritten file in place; got:\n{after}"
    );
    assert_ne!(after, UNFORMATTED, "fmt should have changed the file");
}

#[test]
fn fmt_check_rejects_legacy_check_flag() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["fmt", "--check"])
        .assert()
        .failure()
        .stderr(contains("fmt-check"));
}

#[test]
fn fmt_missing_path_fails() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["fmt", "does/not/exist.bt"])
        .assert()
        .failure()
        .stderr(predicates::str::is_match("does not exist|not found").unwrap());
}
