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
//! - `late` on a type that admits `nil` (`T | Nil`, `Object`, `Dynamic`, an
//!   alias or protocol admitting `nil`, … — BT-4061)
//! - `late` with a default value (`= v`)
//!
//! The fifth Error the ADR lists for B1 — `late state:` on a `native:`
//! Actor or on an `Object` subclass — needs no new code: those are already
//! errors (`check_native_state_fields`, `check_data_keyword_class_kind`)
//! regardless of `slot_kind`, pinned by tests alongside this module's own.

use crate::ast::{ClassKind, DeclaredKeyword, Module, SlotKind, StateDeclaration};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::definite_assignment::admits_nil_type_annotation;
use crate::semantic_analysis::{AliasRegistry, ProtocolRegistry};
use crate::source_analysis::Diagnostic;

/// Runs every `late`-declaration check (ADR 0124 §1, §5) over a module's
/// instance (`state`/`field`) and class-variable (`classState`)
/// declarations.
pub(crate) fn check_late_slot_declarations(
    module: &Module,
    hierarchy: &ClassHierarchy,
    protocols: &ProtocolRegistry,
    aliases: &AliasRegistry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let resolved_kind = hierarchy.resolve_class_kind(class.name.name.as_str());
        for decl in class.state.iter().chain(class.class_variables.iter()) {
            check_late_slot_declaration(
                decl,
                resolved_kind,
                hierarchy,
                protocols,
                aliases,
                diagnostics,
            );
        }
    }
}

/// Runs the four checks against a single `late`-declared slot; a no-op for
/// an eager one.
fn check_late_slot_declaration(
    decl: &StateDeclaration,
    resolved_kind: ClassKind,
    hierarchy: &ClassHierarchy,
    protocols: &ProtocolRegistry,
    aliases: &AliasRegistry,
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
    // A type that admits `nil` is forbidden too — a slot whose type admits
    // `nil` is never in the state `late` describes, since `nil` is already
    // a value it can hold. "Admits" is the checker's own subtype-style
    // predicate (`admits_nil_type_annotation`), not a syntactic `| Nil`
    // match, so `:: Object`, `:: Dynamic`, aliases and protocols that admit
    // `nil` are caught as well (BT-4061). These are independent of the
    // default-value check below (no early return), so `late state: x = 0`
    // reports both problems.
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
        Some(type_annotation)
            if admits_nil_type_annotation(type_annotation, aliases, hierarchy, protocols) =>
        {
            let field_name = decl.name.name.as_str();
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "'late' is not allowed on `{field_name}` — its type admits `Nil`, so \
                         it already has a value for the unassigned state"
                    ),
                    type_annotation.span(),
                )
                .with_hint("Drop 'late' or narrow the type to exclude nil".to_string()),
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
        // Mirror `analyse_full`'s registration order (protocols, then
        // aliases) so alias- and protocol-typed `late` slots resolve as
        // they do in a real compile.
        let mut protocols = ProtocolRegistry::new();
        let proto_diags = protocols.register_module(&module, &hierarchy);
        assert!(
            proto_diags.is_empty(),
            "Protocol registration failed: {proto_diags:?}"
        );
        let mut aliases = AliasRegistry::new();
        let alias_diags = aliases.register_module(&module, &hierarchy, &protocols);
        assert!(
            alias_diags.is_empty(),
            "Alias registration failed: {alias_diags:?}"
        );
        let mut diagnostics = Vec::new();
        check_late_slot_declarations(&module, &hierarchy, &protocols, &aliases, &mut diagnostics);
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
                .contains("narrow the type")
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

    #[test]
    fn late_state_on_object_type_is_error() {
        // BT-4061: `nil` is an `Object` (`UndefinedObject` subclasses it),
        // so `:: Object` admits nil and `late` on it declares nothing.
        let src = "typed Actor subclass: Lt\n  late state: a :: Object\n\n  read => self.a";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late on :: Object, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("admits `Nil`"));
    }

    #[test]
    fn late_state_on_dynamic_type_is_error() {
        // BT-4061: `Dynamic` admits every value including nil, so `late`
        // on it is as meaningless as `late` on `:: Object`.
        let src = "typed Actor subclass: Lt\n  late state: a :: Dynamic\n\n  read => self.a";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late on :: Dynamic, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("admits `Nil`"));
    }

    #[test]
    fn late_state_on_proto_object_type_is_error() {
        // BT-4061: `nil` is assignable to every nominal superclass of
        // `UndefinedObject` — `Object` and `ProtoObject` alike.
        let src = "typed Actor subclass: Lt\n  late state: a :: ProtoObject\n\n  read => self.a";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late on :: ProtoObject, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("admits `Nil`"));
    }

    #[test]
    fn late_state_on_nilable_alias_is_error() {
        // BT-4061: the check resolves ADR 0108 aliases, not just syntactic
        // `| Nil` — `MaybeInt` admits nil through its expansion.
        let src = "type MaybeInt = Integer | Nil\n\ntyped Actor subclass: Lt\n  late state: a :: MaybeInt\n\n  read => self.a";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late on a nilable alias, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("admits `Nil`"));
    }

    #[test]
    fn late_state_on_non_nilable_alias_ok() {
        let src = "type IntOrString = Integer | String\n\ntyped Actor subclass: Lt\n  late state: a :: IntOrString\n\n  read => self.a";
        assert!(diagnostics_for(src).is_empty());
    }

    #[test]
    fn late_state_on_protocol_nil_satisfies_is_error() {
        // BT-4061: a protocol `nil` provably satisfies admits `nil` as a
        // value — `UndefinedObject` answers `isNil`, so `late` on `:: Nilish`
        // declares nothing.
        let src = "Protocol define: Nilish\n  isNil -> Boolean\n\ntyped Actor subclass: Lt\n  late state: a :: Nilish\n\n  read => self.a";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late on a nil-satisfying protocol, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("admits `Nil`"));
    }

    #[test]
    fn late_state_on_protocol_nil_does_not_satisfy_ok() {
        // `nil` answers no `frobnicate`, so the protocol does not admit it.
        let src = "Protocol define: Frobnicateable\n  frobnicate -> Integer\n\ntyped Actor subclass: Lt\n  late state: a :: Frobnicateable\n\n  read => self.a";
        assert!(diagnostics_for(src).is_empty());
    }

    #[test]
    fn late_state_on_unknown_class_ok() {
        // An unresolvable name (e.g. a cross-package class the checker
        // cannot see) proves no `nil` flow — an Error must not fire on it.
        let src =
            "typed Actor subclass: Lt\n  late state: a :: SomeUnknownClass\n\n  read => self.a";
        assert!(diagnostics_for(src).is_empty());
    }

    #[test]
    fn late_class_state_on_object_type_is_error() {
        // The `classState:` path shares the same admission check.
        let src = "typed Actor subclass: TranscriptStream native: beamtalk_transcript_stream\n  late classState: current :: Object\n\n  class current => self.current";
        let diagnostics = diagnostics_for(src);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for late classState on :: Object, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("admits `Nil`"));
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
