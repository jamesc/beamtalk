// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostic emitters shared by the validation checks, including the "did you mean?" unknown-selector warning.
//!
//! **DDD Context:** Semantic Analysis

//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::string_utils::edit_distance;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Severity, Span};
use ecow::EcoString;

use super::super::TypeChecker;
use super::super::shape_assignability::ShapeMismatch;
use super::super::types::{AssignmentTypeMismatch, CrossObjectFieldMutation, DynamicInTypedClass};

impl TypeChecker {
    /// Render an [`AssignmentTypeMismatch`] detected by
    /// `inference::infer_assignment`'s annotation-vs-inferred check.
    pub(in super::super) fn emit_assignment_type_mismatch(
        &mut self,
        mismatch: &AssignmentTypeMismatch,
        span: Span,
    ) {
        if let Some(nonconformance) = &mismatch.nonconformance {
            self.diagnostics
                .push(Self::protocol_metatype_nonconformance_diagnostic(
                    &mismatch.inferred_display,
                    nonconformance,
                    span,
                ));
            return;
        }
        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Type mismatch: declared as {}, got {}",
                    mismatch.declared_display, mismatch.inferred_display
                ),
                span,
            )
            .with_category(DiagnosticCategory::Type)
            .with_hint(
                match mismatch
                    .shape_mismatch
                    .as_ref()
                    .map(ShapeMismatch::describe)
                {
                    Some(detail) => format!(
                        "The right-hand side has type {} which is not assignable to {}: {detail}",
                        mismatch.inferred_display, mismatch.declared_display
                    ),
                    None => format!(
                        "The right-hand side has type {} which is not assignable to {}",
                        mismatch.inferred_display, mismatch.declared_display
                    ),
                },
            ),
        );
    }

    /// Render a [`CrossObjectFieldMutation`] detected by
    /// `inference::infer_assignment`'s `FieldAccess` target arm. The
    /// `withField:` suggestion's selector-capitalisation is message text,
    /// so it's built here rather than at the detection site.
    pub(in super::super) fn emit_cross_object_field_mutation(
        &mut self,
        mutation: &CrossObjectFieldMutation,
        span: Span,
    ) {
        let with_sel = {
            let mut chars = mutation.field_name.chars();
            match chars.next() {
                None => "with:".to_string(),
                Some(first) => {
                    let cap: String = first.to_uppercase().collect();
                    format!("with{}{}:", cap, chars.as_str())
                }
            }
        };
        let recv_name = &mutation.receiver_name;
        let field_name = &mutation.field_name;
        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Cannot assign to `{recv_name}.{field_name}` — objects cannot mutate another object's state"
                ),
                span,
            )
            .with_hint(format!(
                "Use `{recv_name} := {recv_name} {with_sel} newValue` to get an updated copy"
            ))
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// Render the improper-cons-tail fact
    /// (`InferredType::improper_cons_tail_display`) detected by
    /// `inference::infer_list_literal`.
    pub(in super::super) fn emit_improper_cons_tail(&mut self, tail_display: &str, span: Span) {
        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Cons tail of a list literal is {tail_display}, not a List — this builds an improper list"
                ),
                span,
            )
            .with_hint(format!(
                "A `[head | tail]` tail must be a List; {tail_display} would form an improper list at runtime"
            ))
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// Render a [`DynamicInTypedClass`] fact detected by
    /// `inference::post_process_expr_type`.
    pub(in super::super) fn emit_dynamic_in_typed_class(
        &mut self,
        fact: &DynamicInTypedClass,
        span: Span,
    ) {
        // An unresolved selector is not fixed by a type annotation: name the
        // selector so the diagnostic points at the real problem (BT-4526).
        let (detail, hint): (EcoString, EcoString) = match &fact.unresolved_selector {
            Some(selector) => (
                format!("{} '{selector}'", fact.description).into(),
                format!(
                    "No method '{selector}' is visible on the receiver — check the selector, or use `@expect type` if a dependency's extension defines it"
                )
                .into(),
            ),
            None => (
                fact.description.into(),
                "Add a type annotation or use `@expect type` to suppress if intentional".into(),
            ),
        };
        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "expression inferred as Dynamic in typed class `{}` ({detail})",
                    fact.class_name
                ),
                span,
            )
            .with_hint(hint)
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// Emit a diagnostic for an unknown selector, at the caller-chosen
    /// `severity`. `display_name` is the type the user wrote (e.g. a
    /// singleton `#infinity`), used only in the message text. `class_name`
    /// is the type we resolve selectors and "did you mean" suggestions
    /// against (e.g. `Symbol` for a singleton). They coincide for
    /// ordinary class receivers.
    ///
    /// `severity` exists because this is also the single-missing-member
    /// case's diagnostic builder for [`TypeChecker::infer_union_message_send`]
    /// — a union send the checker can *prove* fails (every member
    /// closed, none responds) is a `Warning` per ADR 0100 Rule 1's
    /// "provably failing union" row, while every other caller here passes
    /// `Hint`. Reusing this method, rather than a second copy, is what keeps
    /// the message shape and "did you mean" suggestion identical between a
    /// bare receiver's DNU and a union's single-culprit DNU.
    ///
    /// `context_suffix`, when `Some`, is appended verbatim after the "does
    /// not understand '<selector>'" clause — the union call site
    /// uses it for `" (in union A | B)"` so a union's single-culprit DNU
    /// keeps the same receiver context every other union DNU carries; every
    /// other caller passes `None`.
    ///
    /// `fallback_hint`, when `Some`, is attached verbatim whenever
    /// [`Self::find_similar_selector`] finds no "did you mean" suggestion —
    /// this keeps every DNU shape carrying actionable advice: the union
    /// multi-culprit branch a few lines below always attaches the generic
    /// `respondsTo:`/`@expect` hint, and a single-culprit union DNU should
    /// too when it has no nearby selector to suggest. Bare-receiver callers
    /// pass `None` here, keeping their existing no-fallback behavior.
    #[allow(clippy::too_many_arguments)] // display/lookup name split + severity/context + fallback hint
    pub(in super::super) fn emit_unknown_selector_warning(
        &mut self,
        display_name: &EcoString,
        class_name: &EcoString,
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
        severity: Severity,
        context_suffix: Option<&str>,
        fallback_hint: Option<&str>,
    ) {
        let side = if is_class_side { " class" } else { "" };
        let suffix = context_suffix.unwrap_or("");
        let message: EcoString =
            format!("{display_name}{side} does not understand '{selector}'{suffix}").into();

        let mut diag = match severity {
            Severity::Warning => Diagnostic::warning(message, span),
            _ => Diagnostic::hint(message, span),
        }
        .with_category(DiagnosticCategory::Dnu);

        // Try to suggest similar selectors; fall back to the caller-supplied
        // generic hint, if any, when no suggestion was found.
        if let Some(suggestion) =
            Self::find_similar_selector(class_name, selector, hierarchy, is_class_side)
        {
            diag = diag.with_hint(format!("Did you mean '{suggestion}'?"));
        } else if let Some(hint) = fallback_hint {
            diag = diag.with_hint(hint);
        }

        self.diagnostics.push(diag);
    }

    /// Find a similar selector for "did you mean" hints.
    fn find_similar_selector(
        class_name: &EcoString,
        selector: &str,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
    ) -> Option<EcoString> {
        let methods = if is_class_side {
            hierarchy.all_class_methods(class_name)
        } else {
            hierarchy.all_methods(class_name)
        };

        let mut best: Option<(EcoString, usize)> = None;
        for method in &methods {
            let dist = edit_distance(selector, method.selector.as_str());
            // Only suggest if distance ≤ 3 and less than half the selector length
            if dist <= 3
                && dist < selector.len() / 2 + 1
                && best.as_ref().is_none_or(|(_, d)| dist < *d)
            {
                best = Some((method.selector.clone(), dist));
            }
        }

        best.map(|(sel, _)| sel)
    }
}
