// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value construction-site definite-assignment check (ADR 0124 §6, §7,
//! Implementation A3+A4).
//!
//! `Cls new` and a literal-map `Cls new: #{...}` are checked against `Cls`'s
//! declared, no-default, non-nilable, non-`late` fields — the only
//! definite-assignment check a Value class ever gets, since Values have no
//! runtime post-`initialize` check (that's Actor-only, ADR 0078).

use super::common::*;

// --- Fixtures --------------------------------------------------------------

/// Build a `Value subclass: StoredSnapshot` with the given state and
/// class-side methods.
fn stored_snapshot_value(
    state: Vec<StateDeclaration>,
    class_methods: Vec<MethodDefinition>,
) -> ClassDefinition {
    ClassDefinition {
        name: ident("StoredSnapshot"),
        superclass: Some(ident("Value")),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: true,
        is_internal: false,
        supervisor_kind: None,
        state,
        methods: vec![],
        class_methods,
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: span(),
    }
}

/// A native (`native:`-backed) `Value subclass:` — used to exercise the
/// Hint-tier "native ancestor" downgrade.
fn native_value_class(state: Vec<StateDeclaration>) -> ClassDefinition {
    let mut class = stored_snapshot_value(state, vec![]);
    class.backing_module = Some(ident("some_native_module"));
    class
}

/// An `Actor subclass:` with the given state — used to confirm the check
/// never fires for Actors (out of scope, BT-1948).
fn counter_actor(state: Vec<StateDeclaration>) -> ClassDefinition {
    ClassDefinition {
        name: ident("Counter"),
        superclass: Some(ident("Actor")),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: true,
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
        handle_scope: None,
        shape_version: None,
        span: span(),
    }
}

/// `field: state :: ReplaySnapshot` — typed, no default.
fn state_field() -> StateDeclaration {
    StateDeclaration::with_type(
        ident("state"),
        TypeAnnotation::simple("ReplaySnapshot", span()),
        span(),
    )
}

/// `field: eventId :: Integer = 0` — typed, defaulted.
fn event_id_field() -> StateDeclaration {
    StateDeclaration::with_type_and_default(
        ident("eventId"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )
}

/// `field: label` — untyped.
fn untyped_field(name: &str) -> StateDeclaration {
    StateDeclaration::new(ident(name), span())
}

/// `field: note :: String | Nil` — typed, nilable, no default.
fn nilable_field(name: &str) -> StateDeclaration {
    StateDeclaration::with_type(
        ident(name),
        TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Nil", span()),
            ],
            span(),
        ),
        span(),
    )
}

fn key(name: &str) -> Expression {
    Expression::Literal(Literal::Symbol(name.into()), span())
}

fn map_lit(pairs: Vec<(Expression, Expression)>) -> Expression {
    Expression::MapLiteral {
        pairs: pairs
            .into_iter()
            .map(|(k, v)| crate::ast::MapPair::new(k, v, span()))
            .collect(),
        span: span(),
    }
}

/// `StoredSnapshot new`
fn bare_new() -> Expression {
    msg_send(
        class_ref("StoredSnapshot"),
        MessageSelector::Unary("new".into()),
        vec![],
    )
}

/// `StoredSnapshot new: <map>`
fn new_with(map: Expression) -> Expression {
    msg_send(
        class_ref("StoredSnapshot"),
        MessageSelector::Keyword(vec![KeywordPart::new("new:", span())]),
        vec![map],
    )
}

fn check(module: &Module, hierarchy: &ClassHierarchy) -> TypeChecker {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    checker
}

fn definite_assignment_diags(checker: &TypeChecker) -> Vec<Diagnostic> {
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::DefiniteAssignment))
        .cloned()
        .collect()
}

fn build_hierarchy(class: ClassDefinition) -> ClassHierarchy {
    ClassHierarchy::build(&make_module_with_classes(vec![], vec![class]))
        .0
        .unwrap()
}

// --- Tests -------------------------------------------------------------

#[test]
fn bare_new_on_typed_no_default_field_warns() {
    // StoredSnapshot new — `state` is typed, no default, non-nilable, and
    // `new` resolves to the auto-generated constructor.
    let class = stored_snapshot_value(vec![state_field(), event_id_field()], vec![]);
    let module = make_module(vec![bare_new()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    let diags = definite_assignment_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "bare `new` should warn once for `state`: {:?}",
        checker.diagnostics()
    );
    assert_eq!(diags[0].severity, crate::source_analysis::Severity::Warning);
    assert!(
        diags[0].message.contains("state") && diags[0].message.contains("ReplaySnapshot"),
        "message should name the unassigned field and its type: {}",
        diags[0].message
    );
    // The three fixes: supply it, default it, widen to `| Nil`.
    let hint = diags[0].hint.as_ref().expect("expected a hint");
    assert!(
        hint.contains("Supply"),
        "hint should say to supply it: {hint}"
    );
    assert!(
        hint.contains("default"),
        "hint should say to give it a default: {hint}"
    );
    assert!(
        hint.contains("Nil"),
        "hint should say to widen the type to `| Nil`: {hint}"
    );
}

#[test]
fn literal_new_with_map_supplying_field_does_not_warn() {
    // StoredSnapshot new: #{#state => s} — the literal map supplies `state`.
    let class = stored_snapshot_value(vec![state_field()], vec![]);
    let module = make_module(vec![new_with(map_lit(vec![(key("state"), var("s"))]))]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "literal map supplying the field must not warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn literal_new_with_map_missing_field_warns() {
    // StoredSnapshot new: #{} — the literal map does not supply `state`.
    let class = stored_snapshot_value(vec![state_field()], vec![]);
    let module = make_module(vec![new_with(map_lit(vec![]))]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    let diags = definite_assignment_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "empty literal map should warn for `state`: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn non_literal_new_map_argument_is_silent() {
    // StoredSnapshot new: someMap — non-literal argument is no evidence.
    let class = stored_snapshot_value(vec![state_field()], vec![]);
    let module = make_module(vec![
        assign("someMap", map_lit(vec![])),
        new_with(var("someMap")),
    ]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a non-literal `new:` argument must not be checked: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn defaulted_field_does_not_warn() {
    let class = stored_snapshot_value(vec![event_id_field()], vec![]);
    let module = make_module(vec![bare_new()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a defaulted field must not warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn untyped_field_does_not_warn() {
    let class = stored_snapshot_value(vec![untyped_field("note")], vec![]);
    let module = make_module(vec![bare_new()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "an untyped field defaults to nil and must not warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn nilable_field_does_not_warn() {
    let class = stored_snapshot_value(vec![nilable_field("note")], vec![]);
    let module = make_module(vec![bare_new()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a nilable field must not warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn keyword_constructor_is_not_checked() {
    // StoredSnapshot state: v eventId: 5 — the auto-generated keyword
    // constructor is a different selector entirely and always supplies
    // every field by construction (ADR 0042); it must never be checked.
    let class = stored_snapshot_value(vec![state_field(), event_id_field()], vec![]);
    let module = make_module(vec![msg_send(
        class_ref("StoredSnapshot"),
        MessageSelector::Keyword(vec![
            KeywordPart::new("state:", span()),
            KeywordPart::new("eventId:", span()),
        ]),
        vec![var("s"), int_lit(5)],
    )]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "the auto keyword constructor must not warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn class_with_user_defined_new_is_exempt_on_bare_new() {
    // A user-defined `class new` means bare `new` no longer resolves to the
    // auto-generated default — no diagnostic even though `state` is
    // unassigned.
    let mut new_method = make_method("new", vec![symbol_lit("placeholder")]);
    new_method.is_class_method = true;
    let class = stored_snapshot_value(vec![state_field()], vec![new_method]);
    let module = make_module(vec![bare_new()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a user-defined `class new` must exempt bare `new`: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn overriding_new_does_not_exempt_new_colon() {
    // Overriding `class new` alone must not silently exempt `new:` too —
    // each selector's override is checked independently.
    let mut new_method = make_method("new", vec![symbol_lit("placeholder")]);
    new_method.is_class_method = true;
    let class = stored_snapshot_value(vec![state_field()], vec![new_method]);
    let module = make_module(vec![new_with(map_lit(vec![]))]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert_eq!(
        definite_assignment_diags(&checker).len(),
        1,
        "overriding `new` alone must not exempt an unassigned `new:` map: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn factory_self_new_colon_supplying_every_field_is_clean() {
    // typed Value subclass: WorkflowHandle
    //   field: workflowId :: String = ""
    //   field: client :: ExduraClient
    //
    //   class for: workflowId client: aClient -> WorkflowHandle =>
    //     self new: #{#workflowId => workflowId, #client => aClient}
    //
    // The class-method factory is the canonical construction site (ADR 0124
    // §6) — its internal `self new: #{...}` supplies every field, so it must
    // not warn, exercising the `self`-in-class-method call site.
    let client_field = StateDeclaration::with_type(
        ident("client"),
        TypeAnnotation::simple("ExduraClient", span()),
        span(),
    );
    let workflow_id_field = StateDeclaration::with_type_and_default(
        ident("workflowId"),
        TypeAnnotation::simple("String", span()),
        Expression::Literal(Literal::String(String::new().into()), span()),
        span(),
    );
    let mut factory = MethodDefinition {
        selector: MessageSelector::Keyword(vec![
            KeywordPart::new("for:", span()),
            KeywordPart::new("client:", span()),
        ]),
        parameters: vec![
            ParameterDefinition::new(ident("workflowId")),
            ParameterDefinition::new(ident("aClient")),
        ],
        body: vec![bare(msg_send(
            var("self"),
            MessageSelector::Keyword(vec![KeywordPart::new("new:", span())]),
            vec![map_lit(vec![
                (key("workflowId"), var("workflowId")),
                (key("client"), var("aClient")),
            ])],
        ))],
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: true,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    factory.is_class_method = true;
    let class = ClassDefinition {
        name: ident("WorkflowHandle"),
        superclass: Some(ident("Value")),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: true,
        is_internal: false,
        supervisor_kind: None,
        state: vec![workflow_id_field, client_field],
        methods: vec![],
        class_methods: vec![factory],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: span(),
    };
    let module = make_module(vec![]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "the factory's internal `self new:` supplies every field: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn native_ancestor_downgrades_severity_to_hint() {
    let class = native_value_class(vec![state_field()]);
    let module = make_module(vec![bare_new()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    let diags = definite_assignment_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "expected one diagnostic: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        diags[0].severity,
        crate::source_analysis::Severity::Hint,
        "a `native:` ancestor should downgrade confidence to Hint: {:?}",
        diags[0]
    );
}

#[test]
fn actor_class_is_never_checked() {
    // `new`/`new:` on an Actor is out of scope (BT-1948) — `check_actor_new_usage`
    // already raises `ActorNew` for this; the definite-assignment check must
    // not also fire.
    let class = counter_actor(vec![state_field()]);
    let module = make_module(vec![bare_new_for("Counter")]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "an Actor class must never get a DefiniteAssignment diagnostic: {:?}",
        checker.diagnostics()
    );
}

fn bare_new_for(class_name: &str) -> Expression {
    msg_send(
        class_ref(class_name),
        MessageSelector::Unary("new".into()),
        vec![],
    )
}

#[test]
fn shadowed_field_does_not_duplicate_diagnostic() {
    // `ChildValue` redeclares `count` — a supported shadowing pattern
    // (`ClassHierarchy::state_field_type`'s own doc comment). `all_state`
    // must report the shadowed name once, at its most-derived declaration,
    // not once per ancestor that (re)declares it — otherwise `ChildValue new`
    // would raise the same diagnostic twice for the one `count` slot.
    let count_field = || {
        StateDeclaration::with_type(
            ident("count"),
            TypeAnnotation::simple("Integer", span()),
            span(),
        )
    };
    let parent = ClassDefinition {
        name: ident("ParentValue"),
        superclass: Some(ident("Value")),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: true,
        is_internal: false,
        supervisor_kind: None,
        state: vec![count_field()],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: span(),
    };
    let mut child = parent.clone();
    child.name = ident("ChildValue");
    child.superclass = Some(ident("ParentValue"));
    child.state = vec![count_field()];

    let module = make_module_with_classes(vec![bare_new_for("ChildValue")], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let checker = check(&module, &hierarchy);
    let diags = definite_assignment_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "a shadowed field must be reported once, not once per declaring ancestor: {:?}",
        checker.diagnostics()
    );
}
