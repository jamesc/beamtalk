// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `spawnWith:` map-literal call-site key checking (ADR 0104 Phase 2, BT-2750).
//!
//! `C spawnWith: #{...}` checks the literal map's keys against `C`'s declared
//! `state:` slots and warns (with a typo suggestion) on an unknown key. When a
//! slot is typed, the literal value's inferred type is checked against it.
//! A non-literal `spawnWith:` argument is never inspected.

use super::common::*;
use crate::ast::MapPair;

// --- Fixtures ------------------------------------------------------------

/// Build an Actor subclass `Counter` with the given typed state slots.
fn counter_actor(state: Vec<StateDeclaration>) -> ClassDefinition {
    ClassDefinition {
        name: ident("Counter"),
        superclass: Some(ident("Actor")),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    }
}

/// `count :: Integer = 0`
fn count_slot() -> StateDeclaration {
    StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )
}

/// `timeoutMs :: Integer | #infinity`
fn timeout_union_slot() -> StateDeclaration {
    StateDeclaration::with_type(
        ident("timeoutMs"),
        TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Singleton {
                    name: "infinity".into(),
                    span: span(),
                },
            ],
            span: span(),
        },
        span(),
    )
}

/// A symbol map key `#name` with the shared span.
fn key(name: &str) -> Expression {
    Expression::Literal(Literal::Symbol(name.into()), span())
}

/// An integer literal value with a *distinct* span so its inferred type is
/// keyed uniquely in the type map (the shared `span()` collides).
fn int_val(n: i64, lo: u32) -> Expression {
    Expression::Literal(Literal::Integer(n), Span::new(lo, lo + 1))
}

/// A string literal value with a distinct span.
fn str_val(s: &str, lo: u32) -> Expression {
    Expression::Literal(Literal::String(s.into()), Span::new(lo, lo + 1))
}

/// Build `#{ k1 => v1, ... }`.
fn map_lit(pairs: Vec<(Expression, Expression)>) -> Expression {
    Expression::MapLiteral {
        pairs: pairs
            .into_iter()
            .map(|(k, v)| MapPair::new(k, v, span()))
            .collect(),
        span: span(),
    }
}

/// `Counter spawnWith: <map>`
fn spawn_with(map: Expression) -> Expression {
    msg_send(
        class_ref("Counter"),
        MessageSelector::Keyword(vec![KeywordPart::new("spawnWith:", span())]),
        vec![map],
    )
}

/// Diagnostics whose message mentions a `spawnWith:` state-key concern.
fn key_diags(checker: &TypeChecker) -> Vec<Diagnostic> {
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("state key"))
        .cloned()
        .collect()
}

fn check(module: &Module, hierarchy: &ClassHierarchy) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    checker
}

// --- Tests ---------------------------------------------------------------

#[test]
fn known_keys_produce_no_diagnostic() {
    // Counter spawnWith: #{#count => 0} — `count` is a declared slot, value is
    // an Integer matching the declared type.
    let class = counter_actor(vec![count_slot()]);
    let module = make_module(vec![spawn_with(map_lit(vec![(
        key("count"),
        int_val(0, 100),
    )]))]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    assert!(
        key_diags(&checker).is_empty(),
        "known key should not warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn unknown_key_warns_with_typo_suggestion() {
    // Counter spawnWith: #{#cuont => 0} — `cuont` is not a slot; nearest is `count`.
    let class = counter_actor(vec![count_slot()]);
    let module = make_module(vec![spawn_with(map_lit(vec![(
        key("cuont"),
        int_val(0, 100),
    )]))]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    let diags = key_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "expected one unknown-key warning: {diags:?}"
    );
    assert_eq!(diags[0].severity, crate::source_analysis::Severity::Warning);
    assert!(
        diags[0].message.contains("cuont") && diags[0].message.contains("count"),
        "message should name the unknown key and the suggested slot: {}",
        diags[0].message
    );
    assert!(
        diags[0].message.contains("did you mean"),
        "message should include a typo suggestion: {}",
        diags[0].message
    );
}

#[test]
fn unknown_key_without_close_slot_warns_without_suggestion() {
    // A key nowhere near any slot still warns, just without a "did you mean".
    let class = counter_actor(vec![count_slot()]);
    let module = make_module(vec![spawn_with(map_lit(vec![(
        key("completelyUnrelated"),
        int_val(0, 100),
    )]))]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    let diags = key_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "expected one unknown-key warning: {diags:?}"
    );
    assert!(
        diags[0].message.contains("completelyUnrelated"),
        "message should name the unknown key: {}",
        diags[0].message
    );
    assert!(
        !diags[0].message.contains("did you mean"),
        "no close slot → no suggestion: {}",
        diags[0].message
    );
}

#[test]
fn value_type_mismatch_simple_slot_warns() {
    // count :: Integer, but the value is a String.
    let class = counter_actor(vec![count_slot()]);
    let module = make_module(vec![spawn_with(map_lit(vec![(
        key("count"),
        str_val("bad", 100),
    )]))]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    let diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("type mismatch for state key"))
        .collect();
    assert_eq!(diags.len(), 1, "expected one value-type warning: {diags:?}");
    assert!(
        diags[0].message.contains("count") && diags[0].message.contains("String"),
        "message should name the slot and the offending type: {}",
        diags[0].message
    );
}

#[test]
fn value_type_matches_union_slot_no_warning() {
    // timeoutMs :: Integer | #infinity, value is an Integer — compatible.
    let class = counter_actor(vec![timeout_union_slot()]);
    let module = make_module(vec![spawn_with(map_lit(vec![(
        key("timeoutMs"),
        int_val(30, 100),
    )]))]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    assert!(
        key_diags(&checker).is_empty(),
        "Integer is a member of `Integer | #infinity`: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn value_type_mismatch_union_slot_warns() {
    // timeoutMs :: Integer | #infinity, value is a String — no member matches.
    let class = counter_actor(vec![timeout_union_slot()]);
    let module = make_module(vec![spawn_with(map_lit(vec![(
        key("timeoutMs"),
        str_val("soon", 100),
    )]))]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    let diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("type mismatch for state key"))
        .collect();
    assert_eq!(
        diags.len(),
        1,
        "String is not a member of `Integer | #infinity`: {diags:?}"
    );
    assert!(
        diags[0].message.contains("timeoutMs"),
        "message should name the union slot: {}",
        diags[0].message
    );
}

#[test]
fn non_literal_dictionary_argument_is_silent() {
    // d := #{#cuont => 0}   (a Dictionary-typed variable, typo and all)
    // Counter spawnWith: d  — argument is NOT a MapLiteral → no key inspection.
    let class = counter_actor(vec![count_slot()]);
    let module = make_module(vec![
        assign("d", map_lit(vec![(key("cuont"), int_val(0, 100))])),
        msg_send(
            class_ref("Counter"),
            MessageSelector::Keyword(vec![KeywordPart::new("spawnWith:", span())]),
            vec![var("d")],
        ),
    ]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    assert!(
        key_diags(&checker).is_empty(),
        "a non-literal (Dictionary) argument must not be key-checked: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn spawn_with_on_class_reference_infers_known_instance() {
    // Pin (ADR 0083): `Counter spawnWith: #{#count => 0}` returns Known{Counter},
    // so a subsequent unknown unary send warns about `Counter`.
    let class = counter_actor(vec![count_slot()]);
    let module = make_module(vec![
        assign(
            "x",
            spawn_with(map_lit(vec![(key("count"), int_val(0, 100))])),
        ),
        msg_send(
            var("x"),
            MessageSelector::Unary("nonExistentMethod".into()),
            vec![],
        ),
    ]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("nonExistentMethod") || d.message.contains("Counter"))
        .collect();
    assert!(
        !dnu.is_empty(),
        "spawnWith: should infer Known{{Counter}} so the unknown send resolves against Counter: {:?}",
        checker.diagnostics()
    );
    assert!(
        key_diags(&checker).is_empty(),
        "known key must not warn on the pin: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn spawn_with_via_meta_typed_variable_checks_keys() {
    // Type-driven Meta{Counter} path: `cls := Counter` binds cls to Meta{Counter};
    // `cls spawnWith: #{#cuont => 0}` must still key-check via the Meta branch.
    let class = counter_actor(vec![count_slot()]);
    let module = make_module(vec![
        assign("cls", class_ref("Counter")),
        msg_send(
            var("cls"),
            MessageSelector::Keyword(vec![KeywordPart::new("spawnWith:", span())]),
            vec![map_lit(vec![(key("cuont"), int_val(0, 100))])],
        ),
    ]);
    let hierarchy = ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap();
    let checker = check(&module, &hierarchy);
    let diags = key_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "Meta{{Counter}}-typed receiver should key-check: {diags:?}"
    );
    assert!(
        diags[0].message.contains("did you mean") && diags[0].message.contains("count"),
        "Meta path should also suggest the nearest slot: {}",
        diags[0].message
    );
}
