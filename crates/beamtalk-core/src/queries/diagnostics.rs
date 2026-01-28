// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostics query implementation.
//!
//! This module provides diagnostic reporting (errors and warnings) for the language service.
//!
//! # Design
//!
//! Diagnostics come from multiple sources:
//! - Lexical errors (invalid tokens)
//! - Parse errors (syntax errors)
//! - Semantic errors (type errors, undefined references) - future work
//!
//! # Performance
//!
//! Must respond in <50ms for typical file sizes.

use crate::ast::Module;
use crate::parse::Diagnostic;

/// Computes diagnostics for a module.
///
/// This currently just returns the parse diagnostics, but in the future
/// it will also include semantic analysis diagnostics.
///
/// # Arguments
///
/// * `module` - The parsed AST
/// * `parse_diagnostics` - Diagnostics from the parser
///
/// # Returns
///
/// A list of all diagnostics (errors and warnings).
#[must_use]
pub fn compute_diagnostics(
    _module: &Module,
    parse_diagnostics: Vec<Diagnostic>,
) -> Vec<Diagnostic> {
    // Future: Add semantic analysis diagnostics
    // - Undefined variables
    // - Type errors
    // - Unused variables
    // - etc.

    parse_diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{lex_with_eof, parse};

    #[test]
    fn compute_diagnostics_returns_parse_errors() {
        let source = "x := :="; // Invalid syntax
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn compute_diagnostics_empty_for_valid_code() {
        let source = "x := 42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        assert!(diagnostics.is_empty());
    }
}
