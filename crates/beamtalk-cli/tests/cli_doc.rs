// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk doc` and `beamtalk test-docs` (BT-2084).

mod cli_common;

use predicates::str::contains;

#[test]
fn doc_generates_html_output() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["doc", "src/", "--output", "docs_out"])
        .assert()
        .success()
        .stdout(contains("Generated documentation"));

    let index = project.path().join("docs_out/index.html");
    assert!(
        index.exists(),
        "expected generated index.html at {}",
        index.display()
    );
}

#[test]
fn doc_errors_when_no_sources_found() {
    let project = cli_common::fixture_project();
    let empty = project.path().join("empty_src");
    std::fs::create_dir(&empty).unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["doc", "empty_src", "--output", "docs_out"])
        .assert()
        .failure()
        .stderr(contains("No .bt source files"));
}

#[test]
fn test_docs_runs_on_passing_doctest() {
    let project = cli_common::fixture_project();
    let docs_dir = project.path().join("docs_in");
    std::fs::create_dir(&docs_dir).unwrap();
    std::fs::write(
        docs_dir.join("guide.md"),
        "# Sample\n\
         \n\
         ```beamtalk\n\
         3 + 4\n\
         // => 7\n\
         ```\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-docs", "--quiet", "docs_in"])
        .assert()
        .success();
}

#[test]
fn test_docs_fails_on_assertion_mismatch() {
    let project = cli_common::fixture_project();
    let docs_dir = project.path().join("docs_in");
    std::fs::create_dir(&docs_dir).unwrap();
    std::fs::write(
        docs_dir.join("guide.md"),
        "# Sample\n\
         \n\
         ```beamtalk\n\
         3 + 4\n\
         // => 99\n\
         ```\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["test-docs", "--quiet", "docs_in"])
        .assert()
        .failure();
}
