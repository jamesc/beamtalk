// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Integration test that runs the Erlang runtime test suite.
//!
//! This ensures the Erlang runtime tests are run as part of `cargo test`.

use std::env;
use std::path::PathBuf;
use std::process::Command;

fn find_runtime_dir() -> PathBuf {
    // Find the workspace root from CARGO_MANIFEST_DIR
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = PathBuf::from(&manifest_dir)
        .parent()
        .and_then(|p| p.parent())
        .expect("Cannot find workspace root")
        .to_path_buf();
    workspace_root.join("runtime")
}

#[test]
fn erlang_runtime_tests() {
    let runtime_dir = find_runtime_dir();

    // Check if rebar3 is available
    let rebar3_check = Command::new("rebar3").arg("--version").output();

    if rebar3_check.is_err() {
        eprintln!("rebar3 not found, skipping Erlang runtime tests");
        eprintln!("Install rebar3 to run the full test suite");
        return;
    }

    // Run rebar3 eunit
    let output = Command::new("rebar3")
        .arg("eunit")
        .current_dir(&runtime_dir)
        .output()
        .expect("Failed to run rebar3 eunit");

    // Print stdout/stderr for visibility
    if !output.stdout.is_empty() {
        println!("{}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    }

    assert!(
        output.status.success(),
        "rebar3 eunit failed with exit code: {:?}",
        output.status.code()
    );
}
