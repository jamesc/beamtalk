// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk new` (BT-2084).

mod cli_common;

use predicates::str::contains;

#[test]
fn new_library_generates_project_structure() {
    let dir = tempfile::tempdir().unwrap();

    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "my_lib"])
        .assert()
        .success()
        .stdout(contains("Created library package"));

    let proj = dir.path().join("my_lib");
    assert!(proj.join("beamtalk.toml").exists(), "beamtalk.toml missing");
    assert!(proj.join("src").is_dir(), "src/ missing");
    assert!(proj.join("test").is_dir(), "test/ missing");
    assert!(proj.join(".gitignore").exists(), ".gitignore missing");
    assert!(proj.join("Justfile").exists(), "Justfile missing");
    assert!(proj.join("README.md").exists(), "README.md missing");

    let toml = std::fs::read_to_string(proj.join("beamtalk.toml")).unwrap();
    assert!(
        toml.contains("name = \"my_lib\""),
        "beamtalk.toml should contain the project name; got:\n{toml}"
    );
}

#[test]
fn new_app_emits_application_supervisor() {
    let dir = tempfile::tempdir().unwrap();

    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "my_app", "--app"])
        .assert()
        .success()
        .stdout(contains("Created application package"));

    let proj = dir.path().join("my_app");
    let toml = std::fs::read_to_string(proj.join("beamtalk.toml")).unwrap();
    assert!(
        toml.contains("[application]"),
        "application package should have [application] section; got:\n{toml}"
    );
    assert!(proj.join("src/Main.bt").exists(), "src/Main.bt missing");
}

#[test]
fn new_into_existing_directory_fails() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("existing")).unwrap();

    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "existing"])
        .assert()
        .failure()
        .stderr(contains("already exists"));
}

#[test]
fn new_rejects_invalid_package_name() {
    let dir = tempfile::tempdir().unwrap();
    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "1bad-name"])
        .assert()
        .failure();
}
