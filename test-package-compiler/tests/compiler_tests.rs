// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Auto-generated compiler snapshot tests.
//!
//! Test cases are discovered from the `cases/` directory.
//! Each subdirectory with a `main.bt` file becomes a test.

use beamtalk_core::parse::{lex_with_eof, parse};
use camino::Utf8PathBuf;
use std::fs;

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

// Test cases will be generated here by build.rs
include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));
