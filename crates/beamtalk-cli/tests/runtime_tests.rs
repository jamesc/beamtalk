// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Integration test that runs the Erlang runtime unit tests.
//!
//! This ensures the Erlang runtime tests are run as part of `cargo test`.
//! Only unit test modules are run here - integration tests that require
//! the compiler daemon are run separately in CI.

use serial_test::serial;
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

/// Unit test modules to run (matches CI workflow).
/// Integration tests (`beamtalk_repl_integration_tests`) require the daemon
/// and are run separately in CI.
const UNIT_TEST_MODULES: &str = "beamtalk_actor_tests,beamtalk_future_tests,beamtalk_repl_tests,beamtalk_codegen_simulation_tests";

/// Uses `#[serial(erlang_runtime)]` to prevent parallel rebar3 eunit runs
/// in the same runtime/ directory, which can cause build conflicts.
#[test]
#[serial(erlang_runtime)]
fn erlang_runtime_unit_tests() {
    let runtime_dir = find_runtime_dir();

    // Check if rebar3 is available
    let rebar3_check = Command::new("rebar3").arg("--version").output();

    if rebar3_check.is_err() {
        eprintln!("rebar3 not found, skipping Erlang runtime tests");
        eprintln!("Install rebar3 to run the full test suite");
        return;
    }

    // Run rebar3 eunit for unit test modules only (matching CI workflow)
    let output = Command::new("rebar3")
        .arg("eunit")
        .arg(format!("--module={UNIT_TEST_MODULES}"))
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
