// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Auto-generated compiler snapshot tests.
//!
//! Test cases are discovered from the `cases/` directory.
//! Each subdirectory with a `main.bt` file becomes a test.
//!
//! Each test case generates three snapshots:
//! - `{case}_lexer` - Token stream from the lexer
//! - `{case}_parser` - AST from the parser
//! - `{case}_codegen` - Generated Core Erlang code
//!
//! Additionally, a compilation test verifies the generated Core Erlang
//! compiles successfully with `erlc +from_core` (skipped if erlc unavailable).

use beamtalk_core::erlang::generate_with_workspace;
use beamtalk_core::semantic_analysis;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use camino::Utf8PathBuf;
use std::fs;
use std::process::Command;

/// Helper function to read test case source files
fn read_test_case(case_name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Utf8PathBuf::from(manifest_dir)
        .join("cases")
        .join(case_name)
        .join("main.bt");
    fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "Failed to read test case '{}' at {}: {}",
            case_name, path, e
        )
    })
}

/// Test the lexer output for a given test case
fn test_lexer_snapshot(case_name: &str) {
    let source = read_test_case(case_name);
    let tokens = lex_with_eof(&source);

    // Format tokens for snapshot
    let output = tokens
        .iter()
        .map(|t| format!("{:?}", t))
        .collect::<Vec<_>>()
        .join("\n");

    insta::assert_snapshot!(format!("{}_lexer", case_name), output);
}

/// Test the parser AST output for a given test case
fn test_parser_snapshot(case_name: &str) {
    let source = read_test_case(case_name);
    let tokens = lex_with_eof(&source);
    let (module, diagnostics) = parse(tokens);

    // Format AST and diagnostics for snapshot
    let mut output = format!("AST:\n{:#?}\n", module);

    if !diagnostics.is_empty() {
        output.push_str("\nDiagnostics:\n");
        for diag in &diagnostics {
            output.push_str(&format!("{:?}\n", diag));
        }
    }

    insta::assert_snapshot!(format!("{}_parser", case_name), output);
}

/// Test the Core Erlang codegen output for a given test case
fn test_codegen_snapshot(case_name: &str) {
    let (_module_name, core_erlang) = generate_core_erlang(case_name);
    insta::assert_snapshot!(format!("{}_codegen", case_name), core_erlang);
}

/// Helper function to generate Core Erlang from a test case.
///
/// Returns a tuple of (module_name, core_erlang_code).
///
/// BT-374: Uses workspace mode so test cases referencing workspace bindings
/// (`Transcript`, `Beamtalk`) compile to `persistent_term` lookup + actor send.
/// Batch mode error handling is tested via dedicated test cases.
fn generate_core_erlang(case_name: &str) -> (String, String) {
    let source = read_test_case(case_name);
    let tokens = lex_with_eof(&source);
    let (module, _diagnostics) = parse(tokens);

    // Generate Core Erlang with a module name derived from the test case
    let module_name = case_name.replace('-', "_");
    let core_erlang = beamtalk_core::erlang::generate_with_workspace(&module, &module_name, true)
        .unwrap_or_else(|e| panic!("Codegen failed for '{}': {}", case_name, e));

    (module_name, core_erlang)
}

/// Test that the generated Core Erlang compiles successfully with erlc.
///
/// This test is skipped if `erlc` is not available on the system PATH.
/// The test writes the generated Core Erlang to a temp file and runs
/// `erlc +from_core` to verify the syntax is valid.
fn test_codegen_compiles(case_name: &str) {
    // Check if erlc is available
    let erlc_check = Command::new("erlc").arg("--version").output();
    if erlc_check.is_err() {
        eprintln!(
            "Skipping {} compilation test - erlc not available",
            case_name
        );
        return;
    }

    let (module_name, core_erlang) = generate_core_erlang(case_name);

    // Write to temp file
    let temp_dir = std::env::temp_dir();
    let core_file = temp_dir.join(format!("{}.core", module_name));
    fs::write(&core_file, &core_erlang)
        .unwrap_or_else(|e| panic!("Failed to write temp file: {}", e));

    // Compile with erlc
    let output = Command::new("erlc")
        .arg("+from_core")
        .arg(&core_file)
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to run erlc");

    // Clean up
    let _ = fs::remove_file(&core_file);
    let beam_file = temp_dir.join(format!("{}.beam", module_name));
    let _ = fs::remove_file(&beam_file);

    // Check result
    assert!(
        output.status.success(),
        "erlc compilation failed for '{}':\nstdout: {}\nstderr: {}\n\nGenerated Core Erlang:\n{}",
        case_name,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
        core_erlang
    );
}

/// Test semantic analysis diagnostics for a given test case.
///
/// Runs the full semantic analysis pipeline (class hierarchy, name resolution,
/// type checking, block analysis) and captures error diagnostics in a snapshot.
fn test_semantic_snapshot(case_name: &str) {
    let source = read_test_case(case_name);
    let tokens = lex_with_eof(&source);
    let (module, _parser_diagnostics) = parse(tokens);

    let result = semantic_analysis::analyse(&module);

    // Only capture error-level diagnostics (skip warnings for focused testing)
    let errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .collect();

    let mut output = String::new();
    if errors.is_empty() {
        output.push_str("No semantic errors\n");
    } else {
        output.push_str(&format!("Semantic errors ({}):\n", errors.len()));
        for diag in &errors {
            output.push_str(&format!("  - {}\n", diag.message));
        }
    }

    insta::assert_snapshot!(format!("{}_semantic", case_name), output);
}

// Test cases will be generated here by build.rs
include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

/// BT-374: Workspace bindings in batch mode produce a clear compile error.
#[test]
fn test_workspace_binding_batch_mode_error() {
    let source = "Actor subclass: Greeter\n  greet => Transcript show: 'Hello'";
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);

    // Batch mode (workspace_mode = false) should produce an error
    let result = generate_with_workspace(&module, "test_batch", false);
    assert!(
        result.is_err(),
        "Expected error for workspace binding in batch mode"
    );

    let err = result.unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("Transcript is a workspace binding"),
        "Error should mention binding name, got: {msg}"
    );
    assert!(
        msg.contains("batch compilation"),
        "Error should mention batch compilation, got: {msg}"
    );
}

/// BT-374: Non-binding ClassReferences are unchanged in batch mode.
#[test]
fn test_non_binding_class_unchanged_in_batch_mode() {
    let source = "Actor subclass: Foo\n  run => Counter spawn";
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);

    // Batch mode should work fine for non-binding classes
    let result = generate_with_workspace(&module, "test_batch", false);
    assert!(
        result.is_ok(),
        "Counter spawn should work in batch mode: {:?}",
        result.err()
    );

    let code = result.unwrap();
    assert!(
        code.contains("call 'bt@counter':'spawn'()"),
        "Should generate direct module call for Counter, got:\n{code}"
    );
}

/// BT-374: Workspace binding in workspace mode generates persistent_term lookup.
#[test]
fn test_workspace_binding_generates_persistent_term() {
    let source = "Actor subclass: Greeter\n  greet => Transcript show: 'Hello'";
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);

    let core_erlang = generate_with_workspace(&module, "test_ws", true)
        .expect("Should succeed in workspace mode");

    assert!(
        core_erlang.contains("persistent_term"),
        "Should generate persistent_term lookup, got:\n{core_erlang}"
    );
    assert!(
        core_erlang.contains("beamtalk_binding"),
        "Should reference beamtalk_binding key, got:\n{core_erlang}"
    );
    assert!(
        core_erlang.contains("async_send"),
        "Should use async actor send, got:\n{core_erlang}"
    );
}
