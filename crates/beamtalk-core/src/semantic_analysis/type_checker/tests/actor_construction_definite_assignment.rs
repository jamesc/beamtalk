// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor construction-site definite-assignment check (ADR 0124 §6
//! Implementation A2b, BT-1948).
//!
//! `C spawn` and a literal-map `C spawnWith: #{...}` are checked against
//! `C`'s declared, no-default, non-nilable, non-`late` state slots — the
//! predicate is the composed ADR 0078 chain summary
//! ([`ClassHierarchy::all_initialize_assigns`], A2a) union the literal
//! map's own keys. A non-literal `spawnWith:` argument is never inspected.

use super::common::*;
use crate::ast::MapPair;

// --- Fixtures --------------------------------------------------------------

/// Build an `Actor subclass: Counter` with the given typed state slots,
/// instance methods and class methods.
fn counter_actor(
    state: Vec<StateDeclaration>,
    methods: Vec<MethodDefinition>,
    class_methods: Vec<MethodDefinition>,
) -> ClassDefinition {
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
        methods,
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

/// A native (`native:`-backed) `Actor subclass:` — used to exercise the
/// Hint-tier "native ancestor" downgrade.
fn native_actor(state: Vec<StateDeclaration>) -> ClassDefinition {
    let mut class = counter_actor(state, vec![], vec![]);
    class.backing_module = Some(ident("some_native_module"));
    class
}

/// A `Value subclass:` with the given state — used to confirm the check
/// never fires for Values (that's BT-3552's own check).
fn widget_value(state: Vec<StateDeclaration>) -> ClassDefinition {
    ClassDefinition {
        name: ident("Widget"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Value,
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

/// `state: count :: Integer` — typed, no default.
fn count_slot() -> StateDeclaration {
    StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        span(),
    )
}

/// `late state: proc :: Subprocess` — typed, no default, `late`.
fn late_proc_slot() -> StateDeclaration {
    StateDeclaration {
        slot_kind: crate::ast::SlotKind::Late,
        ..StateDeclaration::with_type(
            ident("proc"),
            TypeAnnotation::simple("Subprocess", span()),
            span(),
        )
    }
}

fn key(name: &str) -> Expression {
    Expression::Literal(Literal::Symbol(name.into()), span())
}

fn map_lit(pairs: Vec<(Expression, Expression)>) -> Expression {
    Expression::MapLiteral {
        pairs: pairs
            .into_iter()
            .map(|(k, v)| MapPair::new(k, v, span()))
            .collect(),
        span: span(),
    }
}

/// `Counter spawn`
fn bare_spawn() -> Expression {
    msg_send(
        class_ref("Counter"),
        MessageSelector::Unary("spawn".into()),
        vec![],
    )
}

/// `Counter spawnWith: <map>`
fn spawn_with(map: Expression) -> Expression {
    msg_send(
        class_ref("Counter"),
        MessageSelector::Keyword(vec![KeywordPart::new("spawnWith:", span())]),
        vec![map],
    )
}

/// `self.count := 0` — a straight-line field assignment.
fn assign_count(value: Expression) -> Expression {
    Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(var("self")),
            field: ident("count"),
            span: span(),
        }),
        value: Box::new(value),
        type_annotation: None,
        span: span(),
    }
}

/// `cond ifTrue: [self.count := 0]` — a conditional assignment with no
/// `ifFalse:` counterpart, so it is never definite.
fn if_true_assign_count() -> Expression {
    msg_send(
        var("cond"),
        MessageSelector::Keyword(vec![KeywordPart::new("ifTrue:", span())]),
        vec![Expression::Block(Block::new(
            vec![],
            vec![bare(assign_count(int_lit(0)))],
            span(),
        ))],
    )
}

/// `self fieldAt: #count put: 0` — a dynamic field write BT-1948's
/// must-analysis can't attribute to `count`.
fn field_at_put_count() -> Expression {
    msg_send(
        var("self"),
        MessageSelector::Keyword(vec![
            KeywordPart::new("fieldAt:", span()),
            KeywordPart::new("put:", span()),
        ]),
        vec![symbol_lit("count"), int_lit(0)],
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
fn bare_spawn_on_unassigned_slot_warns() {
    // Counter spawn — `count` is typed, no default, non-nilable, and no
    // `initialize` in the (empty) chain assigns it.
    let class = counter_actor(vec![count_slot()], vec![], vec![]);
    let module = make_module(vec![bare_spawn()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    let diags = definite_assignment_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "bare `spawn` should warn once for `count`: {:?}",
        checker.diagnostics()
    );
    assert_eq!(diags[0].severity, crate::source_analysis::Severity::Warning);
    assert!(
        diags[0].message.contains("count") && diags[0].message.contains("Integer"),
        "message should name the unassigned field and its type: {}",
        diags[0].message
    );
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
    assert!(
        hint.contains("late"),
        "hint should say to declare it `late`: {hint}"
    );
}

#[test]
fn spawn_with_map_supplying_slot_does_not_warn() {
    // Counter spawnWith: #{#count => 0} — the literal map supplies `count`.
    let class = counter_actor(vec![count_slot()], vec![], vec![]);
    let module = make_module(vec![spawn_with(map_lit(vec![(key("count"), int_lit(0))]))]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "literal map supplying the slot must not warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn spawn_with_empty_map_missing_slot_warns() {
    let class = counter_actor(vec![count_slot()], vec![], vec![]);
    let module = make_module(vec![spawn_with(map_lit(vec![]))]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert_eq!(
        definite_assignment_diags(&checker).len(),
        1,
        "empty literal map should warn for `count`: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn spawn_with_non_literal_map_is_silent() {
    // Counter spawnWith: someMap — non-literal argument is no evidence.
    let class = counter_actor(vec![count_slot()], vec![], vec![]);
    let module = make_module(vec![
        assign("someMap", map_lit(vec![])),
        spawn_with(var("someMap")),
    ]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a non-literal `spawnWith:` argument must not be checked: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn initialize_straight_line_assignment_silences_the_warning() {
    // initialize => self.count := 0. Counter spawn — the composed summary
    // includes `count`, so no diagnostic.
    let class = counter_actor(
        vec![count_slot()],
        vec![make_method("initialize", vec![assign_count(int_lit(0))])],
        vec![],
    );
    let module = make_module(vec![bare_spawn()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a straight-line `initialize` assignment must silence the warning: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn initialize_conditional_if_true_only_still_warns() {
    // initialize => cond ifTrue: [self.count := 0]. Counter spawn — the
    // assignment is not definite (no `ifFalse:` counterpart), so this still
    // warns.
    let class = counter_actor(
        vec![count_slot()],
        vec![make_method("initialize", vec![if_true_assign_count()])],
        vec![],
    );
    let module = make_module(vec![bare_spawn()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    let diags = definite_assignment_diags(&checker);
    assert_eq!(
        diags.len(),
        1,
        "a conditional-only assignment must not silence the warning: {:?}",
        checker.diagnostics()
    );
    assert_eq!(diags[0].severity, crate::source_analysis::Severity::Warning);
}

#[test]
fn parent_assigns_in_initialize_across_files_does_not_warn() {
    // `A` (loaded from cross-file `__beamtalk_meta`, no AST) definitely
    // assigns `count` in its own `initialize`; `Counter subclass: A` spawns
    // cleanly because ADR 0078 auto-chains `A`'s `initialize` before
    // `Counter`'s own.
    let mut class = counter_actor(vec![count_slot()], vec![], vec![]);
    class.superclass = Some(ident("ParentActor"));
    let module_ast = make_module_with_classes(vec![bare_spawn()], vec![class]);
    let (Ok(mut hierarchy), _) = ClassHierarchy::build(&module_ast) else {
        panic!("build should succeed");
    };
    let parent_info = crate::semantic_analysis::class_hierarchy::ClassInfo {
        name: "ParentActor".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: true,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        surface_incomplete: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        state_kinds: std::collections::HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::from(["count".into()]),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    hierarchy.add_from_beam_meta(vec![parent_info]);
    let checker = check(&module_ast, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a cross-file parent's own `initialize` assignment must silence the warning: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn ancestor_dynamic_field_writer_downgrades_severity_to_hint() {
    // An ancestor's own `has_dynamic_field_writer` (not the leaf's) must
    // still demote confidence: an ancestor's `fieldAt:put:` is just as
    // invisible to the must-analysis as the leaf's own would be, and the
    // leaf here neither declares `initialize` nor a writer itself.
    let mut class = counter_actor(vec![count_slot()], vec![], vec![]);
    class.superclass = Some(ident("ParentWithWriter"));
    let module_ast = make_module_with_classes(vec![bare_spawn()], vec![class]);
    let (Ok(mut hierarchy), _) = ClassHierarchy::build(&module_ast) else {
        panic!("build should succeed");
    };
    let parent_info = crate::semantic_analysis::class_hierarchy::ClassInfo {
        name: "ParentWithWriter".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: true,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        surface_incomplete: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        state_kinds: std::collections::HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: true,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    hierarchy.add_from_beam_meta(vec![parent_info]);
    let checker = check(&module_ast, &hierarchy);
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
        "an ancestor's dynamic-writer flag should downgrade confidence to Hint, not just the leaf's own: {:?}",
        diags[0]
    );
}

#[test]
fn late_slot_does_not_warn() {
    let class = counter_actor(vec![late_proc_slot()], vec![], vec![]);
    let module = make_module(vec![bare_spawn()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a `late` slot must never warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn native_ancestor_downgrades_severity_to_hint() {
    let class = native_actor(vec![count_slot()]);
    let module = make_module(vec![bare_spawn()]);
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
fn dynamic_field_writer_downgrades_severity_to_hint() {
    // A `fieldAt:put:` send elsewhere in the class (a helper method, not
    // `initialize` itself) means the must-analysis can't be sure `count`
    // isn't assigned some other way — demote to Hint rather than suppress.
    let class = counter_actor(
        vec![count_slot()],
        vec![make_method("primeCount", vec![field_at_put_count()])],
        vec![],
    );
    let module = make_module(vec![bare_spawn()]);
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
        "a `fieldAt:put:` writer anywhere in the class should downgrade confidence to Hint: {:?}",
        diags[0]
    );
}

#[test]
fn value_class_is_never_checked() {
    // `spawn`/`spawnWith:` on a Value is a DNU, not this check's concern —
    // BT-3552's own construction-site check covers `new`/`new:` on Values.
    let class = widget_value(vec![count_slot()]);
    let module = make_module(vec![bare_spawn_for("Widget")]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a Value class must never get an Actor-construction DefiniteAssignment diagnostic: {:?}",
        checker.diagnostics()
    );
}

fn bare_spawn_for(class_name: &str) -> Expression {
    msg_send(
        class_ref(class_name),
        MessageSelector::Unary("spawn".into()),
        vec![],
    )
}

#[test]
fn untyped_slot_does_not_warn() {
    let class = counter_actor(
        vec![StateDeclaration::new(ident("label"), span())],
        vec![],
        vec![],
    );
    let module = make_module(vec![bare_spawn()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "an untyped slot defaults to nil and must not warn: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn no_state_slots_produces_no_diagnostic() {
    let class = counter_actor(vec![], vec![], vec![]);
    let module = make_module(vec![bare_spawn()]);
    let hierarchy = build_hierarchy(class);
    let checker = check(&module, &hierarchy);
    assert!(
        definite_assignment_diags(&checker).is_empty(),
        "a class with no declared state must never warn: {:?}",
        checker.diagnostics()
    );
}
