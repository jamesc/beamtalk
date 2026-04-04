// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Integration tests for `beamtalk gen-native` (BT-1214).
//!
//! These tests verify the full round-trip:
//! `.bt` source with `native:` to skeleton `.erl` `gen_server` file.
//!
//! Tests are `#[ignore]` because they require the beamtalk binary.
//! Run with: `cargo test --test gen_native -- --ignored`

use std::path::PathBuf;
use std::process::Command;

fn project_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn beamtalk_binary() -> PathBuf {
    project_root().join("target/debug/beamtalk")
}

#[test]
#[ignore = "requires beamtalk binary"]
fn gen_native_generates_valid_erlang_stub() {
    let tmp = tempfile::tempdir().expect("create tempdir");

    // Write a test .bt file with native: and self delegate methods
    let bt_source = "Actor subclass: TestActor native: test_actor_impl\n\
                      \x20 getValue -> Integer => self delegate\n\
                      \x20 setValue: val -> Nil => self delegate\n\
                      \x20 at: idx put: val -> Object => self delegate\n\
                      \x20 close -> Nil => self delegate\n";
    std::fs::write(tmp.path().join("TestActor.bt"), bt_source).expect("write .bt");

    // Run gen-native
    let output = Command::new(beamtalk_binary())
        .args(["gen-native", "TestActor"])
        .current_dir(tmp.path())
        .output()
        .expect("run beamtalk gen-native");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "gen-native failed:\nstdout: {stdout}\nstderr: {stderr}"
    );

    // Check the generated .erl file exists
    let erl_path = tmp.path().join("test_actor_impl.erl");
    assert!(erl_path.exists(), "Expected {erl_path:?} to exist");

    let erl_content = std::fs::read_to_string(&erl_path).expect("read .erl");

    // Verify structural elements
    assert!(erl_content.contains("-module(test_actor_impl)."));
    assert!(erl_content.contains("-behaviour(gen_server)."));
    assert!(erl_content.contains("-export([start_link/1])."));
    assert!(erl_content.contains("start_link(Config) ->"));
    assert!(erl_content.contains("init(_Config) ->"));

    // Verify handle_call clauses for each delegate method
    assert!(erl_content.contains("handle_call({getValue, []}, _From, State) ->"));
    assert!(erl_content.contains("handle_call({'setValue:', [Val]}, _From, State) ->"));
    assert!(erl_content.contains("handle_call({'at:put:', [Idx, Val]}, _From, State) ->"));
    assert!(erl_content.contains("handle_call({close, []}, _From, State) ->"));

    // Verify {ok, Result} wrapping
    assert!(erl_content.contains("{reply, {ok, todo}, State}"));

    // Verify TODO comments
    assert!(erl_content.contains("%% TODO: implement getValue"));
    assert!(erl_content.contains("%% TODO: implement setValue:"));

    // Verify return type comments
    assert!(erl_content.contains("%% getValue -> Integer"));
    assert!(erl_content.contains("%% setValue: -> Nil"));

    // Verify catch-all clause
    assert!(erl_content.contains("handle_call(_Request, _From, State) ->"));
    assert!(erl_content.contains("{reply, {error, not_implemented}, State}."));

    // Print output for manual inspection
    println!("Generated Erlang:\n{erl_content}");
}

#[test]
#[ignore = "requires beamtalk binary"]
fn gen_native_errors_on_missing_native_declaration() {
    let tmp = tempfile::tempdir().expect("create tempdir");

    // Write a .bt file WITHOUT native:
    let bt_source = "Actor subclass: PlainActor\n  getValue -> Integer => 42\n";
    std::fs::write(tmp.path().join("PlainActor.bt"), bt_source).expect("write .bt");

    let output = Command::new(beamtalk_binary())
        .args(["gen-native", "PlainActor"])
        .current_dir(tmp.path())
        .output()
        .expect("run beamtalk gen-native");

    assert!(
        !output.status.success(),
        "gen-native should fail for non-native class"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("no `native:` declaration"),
        "Expected native: error, got: {stderr}"
    );
}

#[test]
#[ignore = "requires beamtalk binary"]
fn gen_native_errors_on_missing_file() {
    let tmp = tempfile::tempdir().expect("create tempdir");

    let output = Command::new(beamtalk_binary())
        .args(["gen-native", "NonExistent"])
        .current_dir(tmp.path())
        .output()
        .expect("run beamtalk gen-native");

    assert!(
        !output.status.success(),
        "gen-native should fail for missing file"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Cannot find") || stderr.contains("NonExistent.bt"),
        "Expected file-not-found error, got: {stderr}"
    );
}

#[test]
#[ignore = "requires beamtalk binary"]
fn gen_native_refuses_to_overwrite_existing_file() {
    let tmp = tempfile::tempdir().expect("create tempdir");

    let bt_source = "Actor subclass: TestActor native: test_actor_impl\n\
                      \x20 getValue -> Integer => self delegate\n";
    std::fs::write(tmp.path().join("TestActor.bt"), bt_source).expect("write .bt");

    // Create an existing .erl file with user content
    let existing_content = "%% My hand-edited implementation\n";
    std::fs::write(tmp.path().join("test_actor_impl.erl"), existing_content)
        .expect("write existing .erl");

    // Run gen-native without --force — should fail
    let output = Command::new(beamtalk_binary())
        .args(["gen-native", "TestActor"])
        .current_dir(tmp.path())
        .output()
        .expect("run beamtalk gen-native");

    assert!(
        !output.status.success(),
        "gen-native should refuse to overwrite without --force"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("already exists") && stderr.contains("--force"),
        "Expected overwrite error mentioning --force, got: {stderr}"
    );

    // Verify the existing file was NOT overwritten
    let content =
        std::fs::read_to_string(tmp.path().join("test_actor_impl.erl")).expect("read .erl");
    assert_eq!(
        content, existing_content,
        "Existing file should be preserved"
    );
}

#[test]
#[ignore = "requires beamtalk binary"]
fn gen_native_force_overwrites_existing_file() {
    let tmp = tempfile::tempdir().expect("create tempdir");

    let bt_source = "Actor subclass: TestActor native: test_actor_impl\n\
                      \x20 getValue -> Integer => self delegate\n";
    std::fs::write(tmp.path().join("TestActor.bt"), bt_source).expect("write .bt");

    // Create an existing .erl file
    std::fs::write(tmp.path().join("test_actor_impl.erl"), "%% Old content\n")
        .expect("write existing .erl");

    // Run gen-native with --force — should succeed
    let output = Command::new(beamtalk_binary())
        .args(["gen-native", "--force", "TestActor"])
        .current_dir(tmp.path())
        .output()
        .expect("run beamtalk gen-native");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "gen-native --force should succeed:\nstdout: {stdout}\nstderr: {stderr}"
    );

    // Verify the file was overwritten with generated content
    let content =
        std::fs::read_to_string(tmp.path().join("test_actor_impl.erl")).expect("read .erl");
    assert!(
        content.contains("-module(test_actor_impl)."),
        "File should contain generated content, got: {content}"
    );
}

#[test]
#[ignore = "requires beamtalk binary and erlc"]
fn gen_native_output_passes_erlc_syntax_check() {
    let tmp = tempfile::tempdir().expect("create tempdir");

    let bt_source = "Actor subclass: TestActor native: test_actor_impl\n\
                      \x20 getValue -> Integer => self delegate\n\
                      \x20 setValue: val -> Nil => self delegate\n";
    std::fs::write(tmp.path().join("TestActor.bt"), bt_source).expect("write .bt");

    // Generate the stub
    let output = Command::new(beamtalk_binary())
        .args(["gen-native", "TestActor"])
        .current_dir(tmp.path())
        .output()
        .expect("run beamtalk gen-native");
    assert!(output.status.success(), "gen-native failed");

    // Verify erlc can parse the output (syntax check only)
    let erlc_output = Command::new("erlc")
        .args(["+debug_info", "-o", tmp.path().to_str().unwrap()])
        .arg(tmp.path().join("test_actor_impl.erl"))
        .output()
        .expect("run erlc");

    let erlc_stderr = String::from_utf8_lossy(&erlc_output.stderr);
    assert!(
        erlc_output.status.success(),
        "erlc failed on generated .erl:\n{erlc_stderr}"
    );
}
