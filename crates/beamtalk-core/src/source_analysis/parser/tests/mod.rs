// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for the Beamtalk recursive descent parser.
//!
//! Split into per-feature submodules for maintainability.

use super::*;
use crate::ast::{DeclaredKeyword, Identifier, MessageSelector, TypeAnnotation};
use crate::source_analysis::Span;
use crate::source_analysis::lex_with_eof;

/// Helper to parse a string and check for errors.
///
/// Passes if there are no Error or Warning diagnostics. Lint diagnostics
/// are ignored here since they do not block compilation.
fn parse_ok(source: &str) -> Module {
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    let non_lint: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity != Severity::Lint)
        .collect();
    assert!(non_lint.is_empty(), "Expected no errors, got: {non_lint:?}");
    module
}

/// Helper to parse a string expecting errors.
fn parse_err(source: &str) -> Vec<Diagnostic> {
    let tokens = lex_with_eof(source);
    let (_module, diagnostics) = parse(tokens);
    diagnostics
}

mod class_tests;
mod expression_tests;
mod literal_tests;
mod method_tests;
mod type_tests;
