// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostic provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `DiagnosticProvider` from the DDD model.
//! It collects errors and warnings from multiple compilation phases (lexing,
//! parsing, semantic analysis) and reports them to the editor. The provider
//! follows LSP terminology and aligns with the ubiquitous language defined in
//! `docs/beamtalk-ddd-model.md`.
//!
//! # Design
//!
//! Diagnostics come from multiple sources:
//! - Lexical errors (invalid tokens)
//! - Parse errors (syntax errors)
//! - Semantic errors (type errors, undefined references, mutations, etc.)
//!
//! # Performance
//!
//! Must respond in <50ms for typical file sizes.
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - LSP specification: Language Server Protocol publishDiagnostics notification

use crate::ast::Module;
use crate::semantic_analysis;
use crate::source_analysis::Diagnostic;

/// Computes diagnostics for a module.
///
/// This runs both parse-time and semantic analysis diagnostics.
///
/// # Arguments
///
/// * `module` - The parsed AST
/// * `parse_diagnostics` - Diagnostics from the parser
///
/// # Returns
///
/// A list of all diagnostics (errors and warnings).
///
/// # Examples
///
/// ```
/// use beamtalk_core::queries::diagnostic_provider::compute_diagnostics;
/// use beamtalk_core::source_analysis::{lex_with_eof, parse};
///
/// let source = "x := 42";
/// let tokens = lex_with_eof(source);
/// let (module, parse_diags) = parse(tokens);
///
/// let diagnostics = compute_diagnostics(&module, parse_diags);
/// assert!(diagnostics.is_empty()); // Valid code has no errors
/// ```
#[must_use]
pub fn compute_diagnostics(module: &Module, parse_diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
    compute_diagnostics_with_known_vars(module, parse_diagnostics, &[])
}

/// Computes diagnostics with pre-defined REPL variables.
///
/// Variables in `known_vars` are treated as already defined, preventing
/// "Undefined variable" errors for REPL session variables.
#[must_use]
pub fn compute_diagnostics_with_known_vars(
    module: &Module,
    parse_diagnostics: Vec<Diagnostic>,
    known_vars: &[&str],
) -> Vec<Diagnostic> {
    let mut all_diagnostics = parse_diagnostics;

    // Run semantic analysis with known variables
    let analysis_result = semantic_analysis::analyse_with_known_vars(module, known_vars);
    all_diagnostics.extend(analysis_result.diagnostics);

    all_diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

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

    #[test]
    fn compute_diagnostics_emits_error_for_field_assignment_in_stored_block() {
        // Test: myBlock := [self.sum := 0]
        let source = "myBlock := [self.sum := 0]";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        // Should have error for field assignment in stored block
        let has_field_error = diagnostics.iter().any(|d| {
            d.message.contains("cannot assign to field") && d.message.contains("stored closure")
        });
        assert!(
            has_field_error,
            "Expected field assignment error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn compute_diagnostics_emits_warning_for_captured_variable_mutation() {
        // Test: count := 0. myBlock := [count := count + 1]
        let source = "count := 0. myBlock := [count := count + 1]";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        // Should have warning for captured variable mutation
        let has_warning = diagnostics.iter().any(|d| {
            d.message.contains("assignment to 'count'")
                && d.message.contains("has no effect on outer scope")
                && d.severity == crate::source_analysis::Severity::Warning
        });
        assert!(
            has_warning,
            "Expected captured variable mutation warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn compute_diagnostics_no_error_for_control_flow_blocks() {
        // Test: 10 timesRepeat: [self.sum := 0]
        let source = "10 timesRepeat: [self.sum := 0]";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        // Should NOT have field assignment error for control flow blocks
        let has_field_error = diagnostics
            .iter()
            .any(|d| d.message.contains("cannot assign to field"));
        assert!(
            !has_field_error,
            "Should not have field assignment error for control flow, got: {diagnostics:?}"
        );
    }

    #[test]
    fn compute_diagnostics_with_known_vars_suppresses_undefined_error() {
        // Without known vars, 'x' would be undefined
        let source = "x + 1";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        // With empty known_vars, should report undefined variable
        let diags_without = compute_diagnostics_with_known_vars(&module, parse_diags.clone(), &[]);
        assert!(
            diags_without
                .iter()
                .any(|d| d.message.contains("Undefined variable: x")),
            "Should report undefined variable without known vars"
        );

        // With 'x' in known_vars, should NOT report undefined
        let diags_with = compute_diagnostics_with_known_vars(&module, parse_diags, &["x"]);
        assert!(
            !diags_with
                .iter()
                .any(|d| d.message.contains("Undefined variable: x")),
            "Should not report undefined variable when in known_vars, got: {diags_with:?}"
        );
    }

    #[test]
    fn compute_diagnostics_with_known_vars_handles_multiple_vars() {
        // Expression using multiple REPL variables
        let source = "x + y * z";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        // All three should be recognized as known
        let diagnostics =
            compute_diagnostics_with_known_vars(&module, parse_diags, &["x", "y", "z"]);

        let has_undefined = diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined variable"));
        assert!(
            !has_undefined,
            "Should not report any undefined variables, got: {diagnostics:?}"
        );
    }

    // ── BT-563: Actor subclass new/new: warnings ──

    #[test]
    fn warn_actor_subclass_new() {
        // Counter is an Actor subclass — using `new` should warn
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.message.contains("spawn")
                && d.severity == crate::source_analysis::Severity::Warning
        });
        assert!(
            has_warning,
            "Expected actor new warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn warn_actor_subclass_new_with_args() {
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter new: #{value => 0}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.message.contains("spawn")
                && d.message.contains("new:")
        });
        assert!(
            has_warning,
            "Expected actor new: warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_actor_subclass_spawn() {
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter spawn";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Actor subclass") && d.message.contains("spawn instead"));
        assert!(
            !has_warning,
            "Should not warn on spawn, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_non_actor_new() {
        // Object subclass using new should NOT warn
        let source =
            "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  getX => self.x\n\nPoint new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Actor subclass") && d.message.contains("spawn"));
        assert!(
            !has_warning,
            "Should not warn for non-Actor subclass, got: {diagnostics:?}"
        );
    }

    #[test]
    fn warn_actor_new_has_hint() {
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let diag = diagnostics
            .iter()
            .find(|d| d.message.contains("Actor subclass"))
            .expect("Should have actor warning");
        assert!(
            diag.hint.as_ref().is_some_and(|h| h.contains("spawn")),
            "Should have hint about spawn, got: {:?}",
            diag.hint
        );
    }

    // ── BT-563: Field name validation ──

    #[test]
    fn warn_unknown_field_in_new() {
        let source = "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  getX => self.x\n\nPoint new: #{#z => 1}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Unknown field") && d.message.contains('z'));
        assert!(
            has_warning,
            "Expected unknown field warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_valid_fields_in_new() {
        let source = "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  getX => self.x\n\nPoint new: #{#x => 1, #y => 2}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_field_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Unknown field"));
        assert!(
            !has_field_warning,
            "Should not warn for valid fields, got: {diagnostics:?}"
        );
    }

    #[test]
    fn warn_unknown_field_has_hint() {
        let source = "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  getX => self.x\n\nPoint new: #{#z => 1}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let diag = diagnostics
            .iter()
            .find(|d| d.message.contains("Unknown field"))
            .expect("Should have unknown field warning");
        assert!(
            diag.hint
                .as_ref()
                .is_some_and(|h| h.contains('x') && h.contains('y')),
            "Should hint about declared fields, got: {:?}",
            diag.hint
        );
    }

    // ── BT-563: Class variable access ──

    #[test]
    fn warn_undefined_classvar() {
        let source = "Object subclass: Config\n  classVar: debug = false\n  check => 1\n\nConfig classVar: #verbose";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics.iter().any(|d| {
            d.message.contains("Undefined class variable") && d.message.contains("verbose")
        });
        assert!(
            has_warning,
            "Expected undefined classVar warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_valid_classvar() {
        let source = "Object subclass: Config\n  classVar: debug = false\n  check => 1\n\nConfig classVar: #debug";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined class variable"));
        assert!(
            !has_warning,
            "Should not warn for valid classVar, got: {diagnostics:?}"
        );
    }

    #[test]
    fn warn_classvar_has_hint() {
        let source = "Object subclass: Config\n  classVar: debug = false\n  check => 1\n\nConfig classVar: #verbose";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let diag = diagnostics
            .iter()
            .find(|d| d.message.contains("Undefined class variable"))
            .expect("Should have classVar warning");
        assert!(
            diag.hint.as_ref().is_some_and(|h| h.contains("debug")),
            "Should hint about declared class vars, got: {:?}",
            diag.hint
        );
    }

    #[test]
    fn warn_actor_new_inside_method_body() {
        // Actor new warning should also fire inside method bodies
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nObject subclass: Factory\n  make => Counter new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Actor subclass") && d.message.contains("spawn"));
        assert!(
            has_warning,
            "Expected actor new warning inside method, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_class_without_state_in_new() {
        // Classes with no declared state should not warn about fields
        let source = "Object subclass: Empty\n  greet => 42\n\nEmpty new: #{#x => 1}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_field_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Unknown field"));
        assert!(
            !has_field_warning,
            "Should not warn about fields for class with no state, got: {diagnostics:?}"
        );
    }
}
