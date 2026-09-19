// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `late` slot declaration validation (ADR 0124 §1, §5, Implementation B1).
//!
//! **DDD Context:** Semantic Analysis
//!
//! `late` is a declaration-level modifier on `state:`/`classState:` marking a
//! slot that is legitimately unassigned after `initialize`. This module
//! covers the four Errors that are new to B1:
//! - `late field:` on a Value (`late` has no meaning for a fully-constructed,
//!   never-reassigned Value — ADR 0124 §5)
//! - `late` without a `::` type annotation
//! - `late` on a nilable type (`T | Nil`)
//! - `late` with a default value (`= v`)
//!
//! The fifth Error the ADR lists for B1 — `late state:` on a `native:`
//! Actor or on an `Object` subclass — needs no new code: those are already
//! errors (`check_native_state_fields`, `check_data_keyword_class_kind`)
//! regardless of `slot_kind`, pinned by tests alongside this module's own.

use crate::ast::{ClassKind, DeclaredKeyword, Module, SlotKind, StateDeclaration, TypeAnnotation};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::source_analysis::Diagnostic;

/// Runs every `late`-declaration check (ADR 0124 §1, §5) over a module's
/// instance (`state`/`field`) and class-variable (`classState`)
/// declarations.
pub(crate) fn check_late_slot_declarations(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let resolved_kind = hierarchy.resolve_class_kind(class.name.name.as_str());
        for decl in class.state.iter().chain(class.class_variables.iter()) {
            check_late_slot_declaration(decl, resolved_kind, diagnostics);
        }
    }
}

/// Runs the four checks against a single `late`-declared slot; a no-op for
/// an eager one.
fn check_late_slot_declaration(
    decl: &StateDeclaration,
    resolved_kind: ClassKind,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if decl.slot_kind != SlotKind::Late {
        return;
    }

    // ADR 0124 §5: `late field:` on a Value — a Value is fully constructed
    // by `new`/`new:`/the keyword constructor and never reassigned, so
    // "assigned later" has no meaning. Scoped to the case where `field:` is
    // otherwise the *correct* keyword (`resolved_kind == Value`); an
    // Actor/Object using `field:` already gets `check_data_keyword_class_kind`'s
    // keyword-mismatch error, which names the real problem.
    if decl.declared_keyword == DeclaredKeyword::Field && resolved_kind == ClassKind::Value {
        let field_name = decl.name.name.as_str();
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "'late' is not allowed on a Value 'field:' — `{field_name}` is fully \
                     constructed and never reassigned"
                ),
                decl.name.span,
            )
            .with_hint(format!(
                "Either make the field optional (`field: {field_name} :: Type | Nil = nil`) \
                 or hold the resource in an Actor"
            )),
        );
    }

    // ADR 0124 §1: a type annotation is required — an untyped slot already
    // defaults to `nil` with no check, so `late` on it declares nothing.
    // A nilable type is forbidden too — a slot whose type admits `nil` is
    // never in the state `late` describes, since `nil` is already a value
    // it can hold. These are independent of the default-value check below
    // (no early return), so `late state: x = 0` reports both problems.
    match &decl.type_annotation {
        None => {
            let field_name = decl.name.name.as_str();
            diagnostics.push(
                Diagnostic::error(
                    format!("'late' requires a type annotation on `{field_name}`"),
                    decl.name.span,
                )
                .with_hint(format!(
                    "Add a type: `late {} {field_name} :: Type`",
                    decl.declared_keyword.as_str().trim()
                )),
            );
        }
        Some(type_annotation) if is_nilable_type_annotation(type_annotation) => {
            let field_name = decl.name.name.as_str();
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "'late' is not allowed on `{field_name}` — its type admits `Nil`, so \
                         it already has a value for the unassigned state"
                    ),
                    type_annotation.span(),
                )
                .with_hint("Drop 'late' or make the type non-nilable".to_string()),
            );
        }
        Some(_) => {}
    }

    // ADR 0124 §1: a default is forbidden — a slot with a default is never
    // unset.
    if decl.default_value.is_some() {
        let field_name = decl.name.name.as_str();
        diagnostics.push(
            Diagnostic::error(
                format!("'late' is not allowed on `{field_name}` — it has a default value"),
                decl.name.span,
            )
            .with_hint("Remove the default value, or drop 'late'".to_string()),
        );
    }
}

/// `true` if `annotation` admits `Nil` — a bare `Nil`/`UndefinedObject`, or a
/// union with one as a member. This is a minimal, B1-scoped check (direct
/// `WellKnownClass` matching, no alias resolution); ADR 0124 §7 reimplements
/// the shared, alias-aware predicate in `beamtalk-core` for Part A's
/// definite-assignment analysis, which this does not need — B1 only rejects
/// the textually-nilable declaration the ADR's Error table describes
/// (`late state: x :: T | Nil`).
fn is_nilable_type_annotation(annotation: &TypeAnnotation) -> bool {
    match annotation {
        TypeAnnotation::Simple(id) => {
            WellKnownClass::from_str(id.name.as_str()).is_some_and(WellKnownClass::is_nil_class)
        }
        TypeAnnotation::Union { types, .. } => types.iter().any(is_nilable_type_annotation),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::Severity;
    use crate::source_analysis::lex_with_eof;
    use crate::source_analysis::parse;

    fn diagnostics_for(src: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut diagnostics = Vec::new();
        check_late_slot_declarations(&module, &hierarchy, &mut diagnostics);
        diagnostics
    }

    // ── late field: on a Value ────────────────────────────────────────────

    #[test]
    fn late_field_on_value_is_error() {
        let src = "Value subclass: Config\n  field: endpoint :: String = \"\"\n  late field: client :: HttpClient\n\n  get => 1";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late field: on Value, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0]
                .message
                .contains("not allowed on a Value 'field:'"),
            "got: {}",
            diagnostics[0].message
        );
    }

    #[test]
    fn eager_field_on_value_ok() {
        let src = "Value subclass: Config\n  field: client :: HttpClient\n\n  get => 1";
        assert!(diagnostics_for(src).is_empty());
    }

    // ── late without a type annotation ────────────────────────────────────

    #[test]
    fn late_state_without_type_is_error() {
        let src = "typed Actor subclass: CodexClient\n  late state: proc\n\n  launch => self.proc";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late without a type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("requires a type"));
    }

    // ── late on a nilable type ────────────────────────────────────────────

    #[test]
    fn late_state_on_nilable_union_type_is_error() {
        let src = "typed Actor subclass: CodexClient\n  late state: proc :: Subprocess | Nil\n\n  launch => self.proc";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late on a nilable type, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("admits `Nil`"));
        assert!(
            diagnostics[0]
                .hint
                .as_deref()
                .unwrap_or("")
                .contains("non-nilable")
        );
    }

    #[test]
    fn late_state_on_bare_nil_type_is_error() {
        let src =
            "typed Actor subclass: CodexClient\n  late state: proc :: Nil\n\n  launch => self.proc";
        let diagnostics = diagnostics_for(src);
        assert_eq!(diagnostics.len(), 1, "got: {diagnostics:?}");
        assert!(diagnostics[0].message.contains("admits `Nil`"));
    }

    #[test]
    fn late_state_on_non_nilable_type_ok() {
        let src = "typed Actor subclass: CodexClient\n  late state: proc :: Subprocess\n\n  launch => self.proc";
        assert!(diagnostics_for(src).is_empty());
    }

    // ── late with a default value ─────────────────────────────────────────

    #[test]
    fn late_state_with_default_is_error() {
        let src =
            "typed Actor subclass: Counter\n  late state: x :: Integer = 0\n\n  get => self.x";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late with a default, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("has a default value"));
    }

    // ── late classState: ──────────────────────────────────────────────────

    #[test]
    fn late_class_state_without_type_is_error() {
        let src = "typed Actor subclass: TranscriptStream native: beamtalk_transcript_stream\n  late classState: current\n\n  class current => self.current";
        let diagnostics = diagnostics_for(src);
        assert_eq!(diagnostics.len(), 1, "got: {diagnostics:?}");
        assert!(diagnostics[0].message.contains("requires a type"));
    }

    #[test]
    fn late_class_state_well_formed_ok() {
        let src = "typed Actor subclass: TranscriptStream native: beamtalk_transcript_stream\n  late classState: current :: TranscriptStream\n\n  class current => self.current";
        assert!(diagnostics_for(src).is_empty());
    }

    // ── eager declarations are untouched ──────────────────────────────────

    #[test]
    fn eager_state_declarations_no_errors() {
        let src = "typed Actor subclass: Counter\n  state: x :: Integer = 0\n  state: y :: Integer | Nil = nil\n  state: z\n\n  get => self.x";
        assert!(diagnostics_for(src).is_empty());
    }

    // ── multiple violations on one declaration ────────────────────────────

    #[test]
    fn late_without_type_and_with_default_both_reported() {
        let src = "typed Actor subclass: Counter\n  late state: x = 0\n\n  get => self.x";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            2,
            "Expected both the missing-type and has-default errors, got: {diagnostics:?}"
        );
    }
}
