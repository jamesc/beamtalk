// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `check_return_type` no longer bails silently on `Union`/`Dynamic` method
//! bodies in `typed` classes (BT-2829).
//!
//! Part A: a `Union` body is validated against the declared return type —
//! any incompatible member produces the same `DiagnosticCategory::Type`
//! mismatch the `Known`-body arm already produces. A union whose members are
//! all compatible (including BT-2047's legitimate branch-union case) stays
//! silent.
//!
//! Part B: a `Dynamic` body with reason `AmbiguousControlFlow` or
//! `UnannotatedReturn` in a `typed` class with a declared concrete return
//! type now raises a hint — these reasons mean the checker *could* have
//! inferred something better. `DynamicReceiver`, `UntypedFfi`, `DynamicSpec`,
//! and `Unknown` stay silent (legitimately dynamic, not actionable).
//!
//! Both parts are scoped to `typed` classes only (see the issue title and
//! its acceptance criteria) — untyped/dynamic classes get no new diagnostics
//! regardless of body type.

use super::common::*;

/// Build a hierarchy containing a single empty `typed` class named
/// `class_name`, on top of the builtins (so `String`/`Integer`/`Object`
/// comparisons resolve). Mirrors the empty-class pattern already used by the
/// BT-2047 tests (`typed Object subclass: ReplaySnapshot` with no body).
fn typed_class_hierarchy(class_name: &str) -> ClassHierarchy {
    let source = format!("typed Object subclass: {class_name}\n");
    let module = parse_source(&source);
    ClassHierarchy::build(&module).0.unwrap()
}

/// Same as [`typed_class_hierarchy`] but without the `typed` modifier.
fn untyped_class_hierarchy(class_name: &str) -> ClassHierarchy {
    let source = format!("Object subclass: {class_name}\n");
    let module = parse_source(&source);
    ClassHierarchy::build(&module).0.unwrap()
}

/// Build a minimal unary method with the given declared return type
/// annotation. `body` is irrelevant here — `check_return_type` is called
/// directly with a hand-constructed `body_type`, bypassing inference.
fn method_with_return_type(return_type: TypeAnnotation) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Unary("probe".into()),
        parameters: vec![],
        body: vec![bare(int_lit(0))],
        return_type: Some(return_type),
        kind: MethodKind::Primary,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        span: span(),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    }
}

fn union_of(members: &[&str]) -> InferredType {
    InferredType::Union {
        members: members.iter().map(|m| InferredType::known(*m)).collect(),
        provenance: TypeProvenance::Inferred(span()),
    }
}

fn type_mismatch_diagnostics(checker: &TypeChecker) -> Vec<&Diagnostic> {
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect()
}

// ── Part A: Union body vs. declared return type ─────────────────────────

/// AC (a): a `Union` body whose members are all compatible with the declared
/// return type (here `Object`, an ancestor of both `String` and `Integer`)
/// produces no diagnostic.
#[test]
fn bt2829_union_body_all_compatible_no_diagnostic() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Simple(ident("Object")));
    let body_type = union_of(&["String", "Integer"]);

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    assert!(
        type_mismatch_diagnostics(&checker).is_empty(),
        "Union(String, Integer) body should not warn against declared Object: {:?}",
        checker.diagnostics()
    );
}

/// AC (a), BT-2047 case: the declared return type is itself a `Union`
/// (`String | Integer`) and the body infers to exactly that union — the
/// legitimate branch-union case (e.g. `ifNil:ifNotNil:`) must stay silent.
#[test]
fn bt2829_union_body_matches_declared_union_no_diagnostic() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Union {
        types: vec![
            TypeAnnotation::Simple(ident("String")),
            TypeAnnotation::Simple(ident("Integer")),
        ],
        span: span(),
    });
    let body_type = union_of(&["String", "Integer"]);

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    assert!(
        type_mismatch_diagnostics(&checker).is_empty(),
        "Union(String, Integer) body should match declared String | Integer with no diagnostic: {:?}",
        checker.diagnostics()
    );
}

/// AC (b): a `Union` body with one incompatible member (declared `-> String`,
/// body `Union(String, Integer)`) produces the same `DiagnosticCategory::Type`
/// mismatch diagnostic the `Known`-body arm already produces. This is the
/// exact repro from the issue: `foo -> String => self bar ifTrue: ["yes"]
/// ifFalse: [42]`.
#[test]
fn bt2829_union_body_incompatible_member_warns() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Simple(ident("String")));
    let body_type = union_of(&["String", "Integer"]);

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    let warnings = type_mismatch_diagnostics(&checker);
    assert_eq!(
        warnings.len(),
        1,
        "Union(String, Integer) body should warn against declared String: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        warnings[0].severity,
        crate::source_analysis::Severity::Warning
    );
    assert_eq!(warnings[0].category, Some(DiagnosticCategory::Type));
    assert!(warnings[0].message.contains("String"));
    assert!(warnings[0].message.contains("Integer"));
}

/// Conservative skip: a union with a non-`Known` member (nested `Dynamic`)
/// can't be reliably compared, so it stays silent — mirrors the existing
/// `classify_union_members` (BT-1832) fallback used for argument checking.
#[test]
fn bt2829_union_body_with_dynamic_member_skips() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Simple(ident("String")));
    let body_type = InferredType::Union {
        members: vec![
            InferredType::known("String"),
            InferredType::Dynamic(DynamicReason::Unknown),
        ],
        provenance: TypeProvenance::Inferred(span()),
    };

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    assert!(
        type_mismatch_diagnostics(&checker).is_empty(),
        "Union with a non-Known member should be skipped conservatively: {:?}",
        checker.diagnostics()
    );
}

// ── Part B: Dynamic body vs. declared return type ────────────────────────

fn dynamic_hint_diagnostics(checker: &TypeChecker) -> Vec<&Diagnostic> {
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("inferred as Dynamic"))
        .collect()
}

/// AC (c): `Dynamic(AmbiguousControlFlow)` body in a `typed` class with a
/// declared concrete return type raises a hint.
#[test]
fn bt2829_dynamic_body_ambiguous_control_flow_hints_in_typed_class() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Simple(ident("String")));
    let body_type = InferredType::Dynamic(DynamicReason::AmbiguousControlFlow);

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    let hints = dynamic_hint_diagnostics(&checker);
    assert_eq!(
        hints.len(),
        1,
        "Dynamic(AmbiguousControlFlow) body in a typed class should hint: {:?}",
        checker.diagnostics()
    );
    assert_eq!(hints[0].severity, crate::source_analysis::Severity::Hint);
    assert_eq!(hints[0].category, Some(DiagnosticCategory::Type));
    assert!(hints[0].message.contains("ambiguous control flow"));
}

/// AC (c): `Dynamic(UnannotatedReturn)` body in a `typed` class with a
/// declared concrete return type raises a hint.
#[test]
fn bt2829_dynamic_body_unannotated_return_hints_in_typed_class() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Simple(ident("String")));
    let body_type = InferredType::Dynamic(DynamicReason::UnannotatedReturn);

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    let hints = dynamic_hint_diagnostics(&checker);
    assert_eq!(
        hints.len(),
        1,
        "Dynamic(UnannotatedReturn) body in a typed class should hint: {:?}",
        checker.diagnostics()
    );
    assert_eq!(hints[0].severity, crate::source_analysis::Severity::Hint);
    assert!(hints[0].message.contains("unannotated return"));
}

/// AC (d): `DynamicReceiver`, `UntypedFfi`, `DynamicSpec`, and `Unknown`
/// reasons are legitimately dynamic and stay silent even in a `typed` class
/// with a declared return type.
#[test]
fn bt2829_dynamic_body_other_reasons_no_diagnostic_in_typed_class() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Simple(ident("String")));

    for reason in [
        DynamicReason::DynamicReceiver,
        DynamicReason::UntypedFfi,
        DynamicReason::DynamicSpec,
        DynamicReason::Unknown,
    ] {
        let body_type = InferredType::Dynamic(reason);
        let mut checker = TypeChecker::new();
        checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

        assert!(
            dynamic_hint_diagnostics(&checker).is_empty(),
            "Dynamic({reason:?}) body should not hint even in a typed class: {:?}",
            checker.diagnostics()
        );
    }
}

// ── Untyped classes: neither part fires ──────────────────────────────────

/// AC (e): in an untyped class, a `Union` body with an incompatible member
/// produces no diagnostic (Part A is scoped to `typed` classes).
#[test]
fn bt2829_untyped_class_union_incompatible_no_diagnostic() {
    let hierarchy = untyped_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Simple(ident("String")));
    let body_type = union_of(&["String", "Integer"]);

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    assert!(
        type_mismatch_diagnostics(&checker).is_empty(),
        "Union body mismatch should stay silent in an untyped class: {:?}",
        checker.diagnostics()
    );
}

/// AC (e): in an untyped class, a `Dynamic(AmbiguousControlFlow)` body
/// produces no hint (Part B is scoped to `typed` classes).
#[test]
fn bt2829_untyped_class_dynamic_ambiguous_control_flow_no_diagnostic() {
    let hierarchy = untyped_class_hierarchy("Probe");
    let method = method_with_return_type(TypeAnnotation::Simple(ident("String")));
    let body_type = InferredType::Dynamic(DynamicReason::AmbiguousControlFlow);

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    assert!(
        dynamic_hint_diagnostics(&checker).is_empty(),
        "Dynamic(AmbiguousControlFlow) body should stay silent in an untyped class: {:?}",
        checker.diagnostics()
    );
}

// ── Declaration-level `@expect type` suppression, end to end ────────────

/// Regression test mirroring the real `BeamtalkInterface.bt` shape that
/// exposed a parser bug while implementing this issue: several methods in a
/// row, each preceded by a declaration-level `@expect type` directive
/// suppressing the new Part B `Dynamic` hint this issue adds. Before the
/// parser fix (`parse_method_body` didn't stop at a leading `@expect` at
/// col <= 2), each `@expect` was swallowed as a trailing statement of the
/// *previous* method's body: the target method's `.expect` stayed `None`
/// (so the hint below survived), and the swallowed directive itself was
/// reported stale. This exercises the full pipeline — parse, infer, check,
/// `apply_expect_directives` — end to end so both bugs would be caught in
/// combination, not just in isolation.
#[test]
fn bt2829_consecutive_declaration_level_expect_all_suppress_cleanly() {
    let source = "typed Object subclass: Probe\n\
                  \x20\x20first -> String =>\n\
                  \x20\x20\x20\x20\"ok\"\n\
                  \n\
                  \x20\x20@expect type\n\
                  \x20\x20second -> String =>\n\
                  \x20\x20\x20\x20true ifTrue: [\"yes\"] ifFalse: [42]\n\
                  \n\
                  \x20\x20@expect type\n\
                  \x20\x20third -> String =>\n\
                  \x20\x20\x20\x20true ifTrue: [\"yes\"] ifFalse: [42]\n";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let class = &module.classes[0];
    assert_eq!(
        class.methods.len(),
        3,
        "expected first/second/third methods"
    );
    assert!(
        class.methods[0].expect.is_none(),
        "`first` has no preceding @expect"
    );
    assert!(
        class.methods[1].expect.is_some(),
        "`second`'s declaration-level @expect must attach to `second`, not `first`'s body"
    );
    assert!(
        class.methods[2].expect.is_some(),
        "`third`'s declaration-level @expect must attach to `third`, not `second`'s body"
    );

    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        diags.iter().all(|d| !d.message.contains("stale @expect")),
        "no @expect directive should be reported stale: {diags:#?}"
    );
    assert!(
        diags
            .iter()
            .all(|d| !d.message.contains("declares return type")),
        "the Part A Union mismatch warning on `second`/`third` must be suppressed: {diags:#?}"
    );
}
