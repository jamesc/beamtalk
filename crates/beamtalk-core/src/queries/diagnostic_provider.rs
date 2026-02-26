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

use crate::ast::{ExpectCategory, Expression, Module};
use crate::semantic_analysis;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};

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

    apply_expect_directives(module, &mut all_diagnostics);

    all_diagnostics
}

/// Applies `@expect` directives to suppress matching diagnostics.
///
/// For each `@expect category` directive in the module, any diagnostic
/// whose span is contained within the *following* expression's span and
/// whose category matches is removed from `diagnostics`. If no matching
/// diagnostic is found, the directive itself becomes an error ("stale @expect").
///
/// This is called by both the language service (LSP/diagnostic provider) and
/// the CLI compiler after all diagnostics have been collected.
pub fn apply_expect_directives(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    let mut directives: Vec<(ExpectCategory, Span, Span)> = Vec::new(); // (cat, directive_span, target_span)

    collect_directives_from_exprs(&module.expressions, &mut directives);
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            collect_directives_from_exprs(&method.body, &mut directives);
        }
    }
    for standalone in &module.method_definitions {
        collect_directives_from_exprs(&standalone.method.body, &mut directives);
    }

    if directives.is_empty() {
        return;
    }

    let mut suppressed_indices: Vec<usize> = Vec::new();
    let mut stale_directives: Vec<(ExpectCategory, Span)> = Vec::new();

    for (cat, directive_span, target_span) in &directives {
        let mut matched = false;
        for (i, diag) in diagnostics.iter().enumerate() {
            if target_span.contains(diag.span) && category_matches(*cat, diag.category) {
                suppressed_indices.push(i);
                matched = true;
            }
        }
        if !matched {
            stale_directives.push((*cat, *directive_span));
        }
    }

    // Remove suppressed diagnostics (in reverse order to preserve indices)
    suppressed_indices.sort_unstable();
    suppressed_indices.dedup();
    for i in suppressed_indices.into_iter().rev() {
        diagnostics.remove(i);
    }

    // Emit errors for stale directives
    for (cat, span) in stale_directives {
        diagnostics.push(Diagnostic::error(
            format!(
                "stale @expect {}: no matching diagnostic found on the following expression",
                cat.as_str()
            ),
            span,
        ));
    }
}

/// Returns true if the `@expect` category matches a diagnostic category.
fn category_matches(expect_cat: ExpectCategory, diag_cat: Option<DiagnosticCategory>) -> bool {
    expect_cat == ExpectCategory::All
        || matches!(
            (expect_cat, diag_cat),
            (ExpectCategory::Dnu, Some(DiagnosticCategory::Dnu))
                | (ExpectCategory::Type, Some(DiagnosticCategory::Type))
                | (ExpectCategory::Unused, Some(DiagnosticCategory::Unused))
        )
}

/// Collects `@expect` directives from an expression list.
///
/// For each `ExpectDirective` at index `i`, the target span is the span of
/// the expression at index `i + 1` (if present).
fn collect_directives_from_exprs(
    exprs: &[Expression],
    directives: &mut Vec<(ExpectCategory, Span, Span)>,
) {
    for (i, expr) in exprs.iter().enumerate() {
        if let Expression::ExpectDirective { category, span } = expr {
            if let Some(next) = exprs.get(i + 1) {
                directives.push((*category, *span, next.span()));
            } else {
                // Trailing @expect with no following expression — treat as stale.
                // Use the directive's own span as the target span so it will
                // never match any real diagnostic and will always be reported stale.
                directives.push((*category, *span, *span));
            }
        }
    }
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
    fn compute_diagnostics_no_warning_for_captured_variable_mutation_in_stored_block() {
        // BT-856 (ADR 0041 Phase 3): Captured variable mutations in stored blocks are
        // now supported via the Tier 2 stateful block protocol (BT-852). No warning needed.
        let source = "count := 0. myBlock := [count := count + 1]";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        // Should NOT have a warning for captured variable mutation — Tier 2 handles it correctly
        let has_stale_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("has no effect on outer scope"));
        assert!(
            !has_stale_warning,
            "Unexpected stale warning for captured variable mutation: {diagnostics:?}"
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

        let has_warning = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.severity == crate::source_analysis::Severity::Warning
        });
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
        let source = "Object subclass: Config\n  classState: debug = false\n  check => 1\n\nConfig classState: #verbose";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics.iter().any(|d| {
            d.message.contains("Undefined class variable") && d.message.contains("verbose")
        });
        assert!(
            has_warning,
            "Expected undefined classState warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_valid_classvar() {
        let source = "Object subclass: Config\n  classState: debug = false\n  check => 1\n\nConfig classState: #debug";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined class variable"));
        assert!(
            !has_warning,
            "Should not warn for valid classState, got: {diagnostics:?}"
        );
    }

    #[test]
    fn warn_classvar_has_hint() {
        let source = "Object subclass: Config\n  classState: debug = false\n  check => 1\n\nConfig classState: #verbose";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let diag = diagnostics
            .iter()
            .find(|d| d.message.contains("Undefined class variable"))
            .expect("Should have classState warning");
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

    #[test]
    fn type_checker_warning_with_hint() {
        // Type checker should surface warnings with "Did you mean" hints
        let source = "42 lenght";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let type_warning = diagnostics
            .iter()
            .find(|d| d.message.contains("does not understand"));
        assert!(
            type_warning.is_some(),
            "Should emit type warning for unknown selector. Got: {diagnostics:?}"
        );
    }

    #[test]
    fn type_checker_dnu_severity_is_hint() {
        use crate::source_analysis::Severity;

        let source = "42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu_diag = diagnostics
            .iter()
            .find(|d| d.message.contains("does not understand"));
        assert!(
            dnu_diag.is_some(),
            "Should have DNU diagnostic. Got: {diagnostics:?}"
        );
        assert_eq!(
            dnu_diag.unwrap().severity,
            Severity::Hint,
            "DNU diagnostics should be Hint severity, not Warning"
        );
    }

    // ── BT-631: Empty method body warnings ──

    #[test]
    fn error_empty_instance_method_body() {
        let source = "Object subclass: Foo\n  doNothing =>";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("doNothing")
                && d.message.contains("empty body")
                && d.severity == crate::source_analysis::Severity::Error
        });
        assert!(has_error, "Expected empty body error, got: {diagnostics:?}");
    }

    #[test]
    fn error_empty_class_method_body() {
        let source = "Object subclass: Foo\n  class reset =>";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("reset")
                && d.message.contains("empty body")
                && d.severity == crate::source_analysis::Severity::Error
        });
        assert!(
            has_error,
            "Expected empty body error for class method, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warning_for_nonempty_method() {
        let source = "Object subclass: Foo\n  getValue => 42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_empty_warning = diagnostics.iter().any(|d| d.message.contains("empty body"));
        assert!(
            !has_empty_warning,
            "Should not warn about non-empty method, got: {diagnostics:?}"
        );
    }

    #[test]
    fn empty_body_error_has_hint() {
        let source = "Object subclass: Foo\n  doNothing =>";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let error = diagnostics
            .iter()
            .find(|d| d.message.contains("empty body"));
        assert!(error.is_some(), "Expected error, got: {diagnostics:?}");
        assert!(error.unwrap().hint.is_some(), "Error should have a hint");
    }

    // ── BT-782: @expect directive ──

    #[test]
    fn expect_dnu_suppresses_dnu_hint() {
        // @expect dnu before a message send that has a DNU hint
        let source = "@expect dnu\n42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "DNU hint should be suppressed by @expect dnu, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_dnu_stale_when_no_dnu() {
        // @expect dnu where there is no DNU diagnostic → stale error
        let source = "@expect dnu\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Should emit stale @expect error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_all_suppresses_any_diagnostic() {
        // @expect all suppresses any diagnostic on the following expression
        let source = "@expect all\n42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect all should suppress DNU, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_dnu_in_method_body() {
        // @expect dnu inside a method body
        let source = "Object subclass: Foo\n  test =>\n    @expect dnu\n    42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "DNU hint in method should be suppressed, got: {diagnostics:?}"
        );
    }
}
