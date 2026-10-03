// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for the Beamtalk recursive descent parser.
//!
//! Split into per-feature submodules for maintainability.

use super::*;
use crate::ast::{DeclaredKeyword, Identifier, MessageSelector, TypeAnnotation};
use crate::source_analysis::Span;
use crate::source_analysis::lex_with_eof;
use crate::test_helpers::test_support::parse_ok;

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
mod native_declaration_tests;
mod traits_tests;
mod type_alias_tests;
mod type_tests;
