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
/// Uses workspace mode for code generation.
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

/// BT-490 / ADR 0019 Phase 3: Workspace bindings compile as normal class references.
/// Transcript, Beamtalk, Workspace are no longer special-cased — they go through
/// standard class dispatch (class_send) like any other class.
#[test]
fn test_workspace_binding_compiles_as_normal_class() {
    let source = "Actor subclass: Greeter\n  greet => Transcript show: 'Hello'";
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);

    // Both batch and workspace mode should succeed — no special treatment
    let batch_result = generate_with_workspace(&module, "test_batch", false);
    assert!(
        batch_result.is_ok(),
        "Transcript should compile in batch mode: {:?}",
        batch_result.err()
    );

    let ws_result = generate_with_workspace(&module, "test_ws", true);
    assert!(
        ws_result.is_ok(),
        "Transcript should compile in workspace mode: {:?}",
        ws_result.err()
    );

    let batch_code = batch_result.unwrap();
    assert!(
        batch_code.contains("class_send"),
        "Batch mode should use class_send for Transcript, got:\n{batch_code}"
    );
    assert!(
        !batch_code.contains("persistent_term"),
        "Should NOT generate persistent_term lookup, got:\n{batch_code}"
    );

    // ADR 0019: Actor methods in workspace mode use class_send with
    // persistent_term fallback for convenience binding names
    let ws_code = ws_result.unwrap();
    assert!(
        ws_code.contains("class_send"),
        "Actor methods in workspace mode should use class_send for Transcript, got:\n{ws_code}"
    );
    assert!(
        ws_code.contains("persistent_term"),
        "Actor methods should fall back to persistent_term for workspace bindings, got:\n{ws_code}"
    );
}
