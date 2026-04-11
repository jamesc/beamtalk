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

use crate::ast::{ExpectCategory, Expression, ExpressionStatement, Module};
use crate::semantic_analysis;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

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

/// Computes diagnostics with native type registry for FFI type warnings (ADR 0075).
///
/// When `native_types` is `Some`, FFI calls get typed return inference and
/// keyword mismatch / argument type warnings from the registry.
#[must_use]
pub fn compute_diagnostics_with_native_types(
    module: &crate::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    native_types: Option<
        std::sync::Arc<crate::semantic_analysis::type_checker::NativeTypeRegistry>,
    >,
) -> Vec<Diagnostic> {
    let mut all_diagnostics = parse_diagnostics;

    if native_types.is_some() {
        let options = crate::CompilerOptions::default();
        let analysis_result =
            crate::semantic_analysis::analyse_with_natives(module, &options, vec![], native_types);
        all_diagnostics.extend(analysis_result.diagnostics);
    } else {
        let analysis_result = crate::semantic_analysis::analyse(module);
        all_diagnostics.extend(analysis_result.diagnostics);
    }

    apply_expect_directives(module, &mut all_diagnostics);
    all_diagnostics
}

/// Computes diagnostics with pre-defined REPL variables and pre-loaded class
/// entries from BEAM metadata (ADR 0050 Phase 4).
///
/// `pre_loaded_classes` are injected into the `ClassHierarchy` before `TypeChecking`,
/// making REPL-session user classes visible to the `TypeChecker`.
#[must_use]
pub fn compute_diagnostics_with_known_vars_and_classes(
    module: &crate::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    known_vars: &[&str],
    pre_loaded_classes: Vec<crate::semantic_analysis::class_hierarchy::ClassInfo>,
) -> Vec<Diagnostic> {
    let mut all_diagnostics = parse_diagnostics;
    let analysis_result = crate::semantic_analysis::analyse_with_known_vars_and_classes(
        module,
        known_vars,
        pre_loaded_classes,
    );
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
    // (cat, reason, directive_span, target_span)
    let mut directives: Vec<(ExpectCategory, Option<EcoString>, Span, Span)> = Vec::new();

    collect_directives_from_exprs(&module.expressions, &mut directives);
    for class in &module.classes {
        // BT-1856: Collect declaration-level @expect from state declarations.
        // directive_span = the @expect token span (for stale warnings),
        // target_span = the declaration span (for matching diagnostics).
        for state_decl in class.state.iter().chain(class.class_variables.iter()) {
            if let Some((cat, ref reason, expect_span)) = state_decl.expect {
                directives.push((cat, reason.clone(), expect_span, state_decl.span));
            }
        }
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            // BT-1856: Collect declaration-level @expect from method declarations
            if let Some((cat, ref reason, expect_span)) = method.expect {
                directives.push((cat, reason.clone(), expect_span, method.span));
            }
            collect_directives_from_exprs(&method.body, &mut directives);
        }
    }
    for standalone in &module.method_definitions {
        if let Some((cat, ref reason, expect_span)) = standalone.method.expect {
            directives.push((cat, reason.clone(), expect_span, standalone.method.span));
        }
        collect_directives_from_exprs(&standalone.method.body, &mut directives);
    }

    if directives.is_empty() {
        return;
    }

    let mut suppressed_indices: Vec<usize> = Vec::new();
    let mut stale_directives: Vec<(ExpectCategory, Option<EcoString>, Span)> = Vec::new();

    for (cat, reason, directive_span, target_span) in &directives {
        let mut matched = false;
        for (i, diag) in diagnostics.iter().enumerate() {
            if target_span.contains(diag.span) && category_matches(*cat, diag.category) {
                suppressed_indices.push(i);
                matched = true;
            }
        }
        if !matched {
            stale_directives.push((*cat, reason.clone(), *directive_span));
        }
    }

    // Remove suppressed diagnostics (in reverse order to preserve indices)
    suppressed_indices.sort_unstable();
    suppressed_indices.dedup();
    for i in suppressed_indices.into_iter().rev() {
        diagnostics.remove(i);
    }

    // Emit warnings for stale directives (BT-1412: warning, not error, so
    // compilation can proceed — the annotation is just unnecessary).
    for (cat, reason, span) in stale_directives {
        let message = if let Some(reason) = reason {
            format!(
                "stale @expect {} \"{reason}\": no matching diagnostic found on the following expression — consider removing it",
                cat.as_str()
            )
        } else {
            format!(
                "stale @expect {}: no matching diagnostic found on the following expression — consider removing it",
                cat.as_str()
            )
        };
        diagnostics.push(
            Diagnostic::warning(message, span)
                .with_hint("Remove the `@expect` directive if the diagnostic was fixed"),
        );
    }
}

/// Returns true if the `@expect` category matches a diagnostic category.
///
/// `@expect type` matches both type-mismatch warnings (`DiagnosticCategory::Type`)
/// and method-not-found hints (`DiagnosticCategory::Dnu`).  A common motivation
/// for the latter is type-erasure boundaries — e.g. `Result.unwrap` returns
/// `Object`, so any method call on the result produces a DNU hint — but the
/// suppression applies unconditionally whenever `@expect type` is written.
fn category_matches(expect_cat: ExpectCategory, diag_cat: Option<DiagnosticCategory>) -> bool {
    expect_cat == ExpectCategory::All
        || matches!(
            (expect_cat, diag_cat),
            // BT-1273: @expect type also covers method-not-found (Dnu) hints so that
            // callers can use a single annotation for all type-related suppressions.
            // BT-1918: @expect type also covers missing type-annotation warnings
            // (TypeAnnotation) for backward compatibility.
            (
                ExpectCategory::Dnu | ExpectCategory::Type,
                Some(DiagnosticCategory::Dnu)
            ) | (ExpectCategory::Type, Some(DiagnosticCategory::Type))
                | (
                    ExpectCategory::Type | ExpectCategory::TypeAnnotation,
                    Some(DiagnosticCategory::TypeAnnotation)
                )
                | (ExpectCategory::Unused, Some(DiagnosticCategory::Unused))
                | (
                    ExpectCategory::DeadAssignment,
                    Some(DiagnosticCategory::DeadAssignment)
                )
                | (
                    ExpectCategory::Deprecation,
                    Some(DiagnosticCategory::Deprecation)
                )
                | (ExpectCategory::ActorNew, Some(DiagnosticCategory::ActorNew))
                | (
                    ExpectCategory::Visibility,
                    Some(DiagnosticCategory::Visibility)
                )
                | (
                    ExpectCategory::UnresolvedClass,
                    Some(DiagnosticCategory::UnresolvedClass)
                )
                | (
                    ExpectCategory::UnresolvedFfi,
                    Some(DiagnosticCategory::UnresolvedFfi)
                )
                | (
                    ExpectCategory::ArityMismatch,
                    Some(DiagnosticCategory::ArityMismatch)
                )
                | (
                    ExpectCategory::ShadowedClass,
                    Some(DiagnosticCategory::ShadowedClass)
                )
        )
}

/// Collects `@expect` directives from an expression list.
///
/// For each `ExpectDirective` at index `i`, the target span is the span of
/// the expression at index `i + 1` (if present).
fn collect_directives_from_exprs(
    exprs: &[ExpressionStatement],
    directives: &mut Vec<(ExpectCategory, Option<EcoString>, Span, Span)>,
) {
    for (i, stmt) in exprs.iter().enumerate() {
        if let Expression::ExpectDirective {
            category,
            reason,
            span,
        } = &stmt.expression
        {
            if let Some(next) = exprs.get(i + 1) {
                directives.push((*category, reason.clone(), *span, next.expression.span()));
            } else {
                // Trailing @expect with no following expression — treat as stale.
                // Use the directive's own span as the target span so it will
                // never match any real diagnostic and will always be reported stale.
                directives.push((*category, reason.clone(), *span, *span));
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

    // ── BT-563 / BT-1524: Actor subclass new/new: errors ──

    #[test]
    fn error_actor_subclass_new() {
        // Counter is an Actor subclass — using `new` should error
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.message.contains("spawn")
                && d.severity == crate::source_analysis::Severity::Error
        });
        assert!(has_error, "Expected actor new error, got: {diagnostics:?}");
    }

    #[test]
    fn error_actor_subclass_new_with_args() {
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter new: #{value => 0}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.message.contains("spawn")
                && d.message.contains("new:")
                && d.severity == crate::source_analysis::Severity::Error
        });
        assert!(has_error, "Expected actor new: error, got: {diagnostics:?}");
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
    fn error_actor_new_inside_method_body() {
        // Actor new error should also fire inside method bodies
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nObject subclass: Factory\n  make => Counter new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.message.contains("spawn")
                && d.severity == crate::source_analysis::Severity::Error
        });
        assert!(
            has_error,
            "Expected actor new error inside method, got: {diagnostics:?}"
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
        // @expect dnu where there is no DNU diagnostic → stale warning
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

    // ── BT-1273: @expect type covers method-not-found at type-erasure boundaries ──

    #[test]
    fn expect_type_suppresses_dnu_hint() {
        // BT-1273: @expect type suppresses DNU hints in addition to type-mismatch warnings.
        let source = "@expect type\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect type should suppress DNU hint, got: {diagnostics:?}"
        );
        // And no stale error either
        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect type must not be stale when DNU hint is present, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_stale_when_neither_type_nor_dnu() {
        // BT-1273: @expect type is still stale when there is no type or DNU diagnostic.
        let source = "@expect type\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "@expect type on `42` (no diagnostic) must emit stale warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unknown_expect_category_is_parse_error() {
        // @expect typo should emit a parse error (prevents silent suppression of nothing)
        let source = "@expect selfcapture\n42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics
            .iter()
            .any(|d| d.message.contains("unknown @expect category"));
        assert!(
            has_error,
            "Typo in @expect category should be a parse error, got: {diagnostics:?}"
        );
    }

    // ── BT-1476: Dead block assignment warning + @expect dead_assignment ──

    // ── BT-1476: @expect dead_assignment parsing and stale detection ──

    #[test]
    fn expect_dead_assignment_stale_when_no_diagnostic() {
        // @expect dead_assignment with no matching diagnostic → stale
        let source = "@expect dead_assignment\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Should emit stale @expect when no dead assignment, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_dead_assignment_parses_correctly() {
        // @expect dead_assignment should parse without errors
        let source = "@expect dead_assignment\n42";
        let tokens = lex_with_eof(source);
        let (_, parse_diags) = parse(tokens);

        let has_parse_error = parse_diags
            .iter()
            .any(|d| d.message.contains("unknown @expect"));
        assert!(
            !has_parse_error,
            "dead_assignment should be a recognized @expect category, got: {parse_diags:?}"
        );
    }

    // ── BT-1856: Declaration-level @expect ──────────────────────────────────────

    #[test]
    fn typed_state_no_default_no_warning() {
        // BT-1947: A type annotation replaces the need for a default value.
        // `state: deps :: OrchestratorDeps` (no default) should produce no
        // uninitialized warning.
        let source = "\
Actor subclass: MyActor
  state: running :: Dictionary = #{}
  state: deps :: OrchestratorDeps
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let uninitialized = diagnostics
            .iter()
            .any(|d| d.message.contains("uninitialized"));
        assert!(
            !uninitialized,
            "Typed state without default should not warn (BT-1947), got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_on_method_suppresses_missing_annotation() {
        // @expect type before a method in a typed class should suppress
        // missing-type-annotation warnings.
        let source = "\
typed Object subclass: MyTyped
  @expect type
  first => 42
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let missing_annotation = diagnostics
            .iter()
            .any(|d| d.message.contains("Missing") && d.message.contains("type annotation"));
        assert!(
            !missing_annotation,
            "@expect type should suppress missing annotation warnings, got: {diagnostics:?}"
        );
    }

    #[test]
    fn stale_expect_on_state_declaration() {
        // @expect unused on a state field that is actually used should emit
        // a stale warning (since there is no unused-field diagnostic).
        let source = "\
Object subclass: Foo
  @expect unused
  state: x = 0

  getX => self.x
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Should emit stale @expect when no matching diagnostic, got: {diagnostics:?}"
        );
    }

    #[test]
    fn stale_expect_on_method_declaration() {
        // @expect type on a fully-annotated method in a typed class should be stale.
        let source = "\
typed Object subclass: MyTyped
  @expect type
  getValue -> Integer => 42
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Should emit stale @expect when method already has annotations, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_on_state_round_trips_through_unparse() {
        // Parsing and unparsing @expect type on state should round-trip.
        let source = "\
Actor subclass: MyActor
  @expect type
  state: deps :: OrchestratorDeps
";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = crate::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect type"),
            "Unparsed output should contain @expect type, got: {output}"
        );
        assert!(
            output.contains("state: deps :: OrchestratorDeps"),
            "Unparsed output should contain the state declaration, got: {output}"
        );
    }

    #[test]
    fn expect_type_on_method_round_trips_through_unparse() {
        // Parsing and unparsing @expect type on a method should round-trip.
        let source = "\
typed Object subclass: MyTyped
  @expect type
  first => 42
";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = crate::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect type"),
            "Unparsed output should contain @expect type, got: {output}"
        );
    }

    #[test]
    fn expect_before_invalid_position_in_class_body() {
        // @expect at the end of a class body (not before state/method) should error.
        let source = "\
Object subclass: Foo
  state: x = 0
  @expect type

Object subclass: Bar
  state: y = 0
";
        let tokens = lex_with_eof(source);
        let (_, parse_diags) = parse(tokens);

        let invalid_pos = parse_diags
            .iter()
            .any(|d| d.message.contains("must precede"));
        assert!(
            invalid_pos,
            "@expect before invalid position should produce error, got: {parse_diags:?}"
        );
    }

    // ── BT-1918: TypeAnnotation category ──

    #[test]
    fn expect_type_annotation_suppresses_missing_annotation() {
        // @expect type_annotation before a method in a typed class should suppress
        // missing-type-annotation warnings but not type mismatch warnings.
        let source = "\
typed Object subclass: MyTyped
  @expect type_annotation
  first => 42
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let missing_annotation = diagnostics
            .iter()
            .any(|d| d.message.contains("Missing") && d.message.contains("type annotation"));
        assert!(
            !missing_annotation,
            "@expect type_annotation should suppress missing annotation warnings, got: {diagnostics:?}"
        );
        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect type_annotation should not be stale, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_annotation_does_not_suppress_type_mismatch() {
        // @expect type_annotation should NOT suppress DNU/type-mismatch warnings.
        let source = "@expect type_annotation\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            dnu,
            "@expect type_annotation should NOT suppress DNU hints, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_annotation_on_state_field() {
        // @expect type_annotation on a state field in a typed class should suppress
        // the missing-annotation warning for that field.
        let source = "\
typed Object subclass: MyTyped
  @expect type_annotation
  state: count = 0
  getValue -> Integer => 42
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let missing_state = diagnostics.iter().any(|d| {
            d.message
                .contains("Missing type annotation for state field `count`")
        });
        assert!(
            !missing_state,
            "@expect type_annotation should suppress state field warning, got: {diagnostics:?}"
        );
    }

    // ── BT-1918: @expect reason strings ──

    #[test]
    fn expect_with_reason_parses_correctly() {
        // @expect dnu "FFI boundary" should parse without errors.
        let source = "@expect dnu \"FFI boundary\"\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        assert!(
            parse_diags.is_empty(),
            "Should have no parse errors, got: {parse_diags:?}"
        );
        let diagnostics = compute_diagnostics(&module, Vec::new());
        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect dnu with reason should still suppress DNU, got: {diagnostics:?}"
        );
    }

    #[test]
    fn stale_expect_with_reason_includes_reason_text() {
        // Stale @expect with reason should include the reason in the warning message.
        let source = "@expect dnu \"FFI boundary\"\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .find(|d| d.message.contains("stale @expect"));
        assert!(
            stale.is_some(),
            "Should emit stale warning, got: {diagnostics:?}"
        );
        assert!(
            stale.unwrap().message.contains("FFI boundary"),
            "Stale warning should include reason text, got: {}",
            stale.unwrap().message
        );
    }

    #[test]
    fn expect_with_reason_round_trips_through_unparse() {
        // @expect type "reason" should round-trip through unparse.
        let source = "@expect dnu \"FFI boundary\"\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = crate::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect dnu \"FFI boundary\""),
            "Unparsed output should contain reason string, got: {output}"
        );
    }

    #[test]
    fn expect_with_reason_on_declaration_round_trips() {
        // @expect type_annotation "migrating" on a declaration should round-trip.
        let source = "\
typed Object subclass: MyTyped
  @expect type_annotation \"migrating\"
  first => 42
";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = crate::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect type_annotation \"migrating\""),
            "Unparsed output should contain reason string, got: {output}"
        );
    }

    // ── BT-1923: Drift prevention — every Warning/Hint must have a category ──

    /// Compiles a source snippet and returns only the Warning/Hint diagnostics.
    fn warnings_and_hints(source: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let all = compute_diagnostics(&module, parse_diags);
        all.into_iter()
            .filter(|d| {
                matches!(
                    d.severity,
                    crate::source_analysis::Severity::Warning
                        | crate::source_analysis::Severity::Hint
                        | crate::source_analysis::Severity::Lint
                )
            })
            .collect()
    }

    /// BT-1923: Every Warning/Hint/Lint diagnostic MUST have a category.
    ///
    /// This test compiles source snippets that trigger diagnostics from every
    /// compiler phase (parser, name resolver, semantic analysis, type checker,
    /// lint validators, etc.). If a new warning or hint is added anywhere in
    /// the compiler without setting `.with_category(...)`, this test will fail.
    ///
    /// To fix a failure: find the `Diagnostic::warning(...)` or
    /// `Diagnostic::hint(...)` call that produces the uncategorised diagnostic
    /// and chain `.with_category(DiagnosticCategory::Foo)` onto it.
    #[test]
    fn all_warnings_and_hints_have_categories() {
        // Each snippet is designed to trigger one or more Warning/Hint diagnostics
        // from a specific compiler phase. We collect them all and assert that every
        // single one has `category.is_some()`.
        let snippets: Vec<(&str, &str)> = vec![
            // ── Name resolver: unused variable ──
            (
                "unused variable",
                "Object subclass: Foo\n  bar => x := 42. 0",
            ),
            // ── Name resolver: variable shadowing ──
            (
                "variable shadowing",
                "Object subclass: Foo\n  bar =>\n    x := 1.\n    [| :x | x + 1] value: 2",
            ),
            // ── Name resolver: unreachable code ──
            (
                "unreachable code after early return",
                "Object subclass: Foo\n  bar => ^1. 2",
            ),
            // ── Type checker: DNU hint ──
            (
                "DNU hint on unknown method",
                "typed Object subclass: Foo\n  state: x :: Integer = 0\n  bar => self.x noSuchMethod",
            ),
            // ── Type checker: type mismatch hint ──
            (
                "type mismatch in typed class",
                "typed Object subclass: Foo\n  state: x :: Integer = 0\n  bar => self.x := \"hello\"",
            ),
            // ── Lint validator: always-true condition ──
            ("always-true condition", "true ifTrue: [1] ifFalse: [2]"),
            // ── Actor new error ──
            (
                "actor new",
                "Actor subclass: A\n  state: v = 0\n  go => self.v\n\nA new",
            ),
            // ── Type checker: missing type annotation in typed class ──
            (
                "missing type annotation in typed class",
                "typed Object subclass: Foo\n  state: x = 0\n  bar => self.x",
            ),
            // ── Type checker: Dynamic inference warning in typed class ──
            (
                "dynamic inference in typed class",
                "typed Object subclass: Foo\n  state: x :: Integer = 0\n  bar :: Integer => self.x abs",
            ),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (label, source) in &snippets {
            let diags = warnings_and_hints(source);
            for diag in &diags {
                if diag.category.is_none() {
                    failures.push(format!(
                        "[{label}] {severity:?} diagnostic has no category: \"{msg}\"",
                        severity = diag.severity,
                        msg = diag.message
                    ));
                }
            }
        }

        assert!(
            failures.is_empty(),
            "Found Warning/Hint/Lint diagnostics without categories \
             (drift prevention BT-1923):\n  {}",
            failures.join("\n  ")
        );
    }

    /// BT-1923: Sanity check — the drift prevention snippets actually produce diagnostics.
    ///
    /// If this test fails, it means the snippets no longer trigger any warnings/hints,
    /// which would make the category assertion vacuously true (and useless).
    #[test]
    fn drift_prevention_snippets_produce_diagnostics() {
        // A subset of snippets that should always produce at least one warning/hint.
        let must_produce = vec![
            (
                "unused variable",
                "Object subclass: Foo\n  bar => x := 42. 0",
            ),
            ("always-true condition", "true ifTrue: [1] ifFalse: [2]"),
            (
                "actor new",
                "Actor subclass: A\n  state: v = 0\n  go => self.v\n\nA new",
            ),
            (
                "missing type annotation in typed class",
                "typed Object subclass: Foo\n  state: x = 0\n  bar => self.x",
            ),
        ];

        for (label, source) in must_produce {
            let diags = warnings_and_hints(source);
            assert!(
                !diags.is_empty(),
                "Snippet [{label}] should produce at least one Warning/Hint/Lint diagnostic \
                 but produced none — update the drift prevention test snippets"
            );
        }
    }
}
