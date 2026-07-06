// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Control-flow narrowing tests (ADR 0068 Phase 1g) and respondsTo: narrowing (BT-1582, BT-1833).

use super::super::*;
use super::common::*;

// ---- Control-flow narrowing tests (ADR 0068 Phase 1g) ----

/// Helper: build `x class` unary message
fn x_class(var_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Unary("class".into()),
        vec![],
    )
}

/// Helper: build `(x class) = ClassName` binary expression
fn class_eq(var_name: &str, class_name: &str) -> Expression {
    msg_send(
        x_class(var_name),
        MessageSelector::Binary("=".into()),
        vec![class_ref(class_name)],
    )
}

/// Helper: build `(x class) =:= ClassName` binary expression
fn class_eqeq(var_name: &str, class_name: &str) -> Expression {
    msg_send(
        x_class(var_name),
        MessageSelector::Binary("=:=".into()),
        vec![class_ref(class_name)],
    )
}

/// Helper: build `x isKindOf: ClassName` keyword expression
fn is_kind_of(var_name: &str, class_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Keyword(vec![KeywordPart::new("isKindOf:", span())]),
        vec![class_ref(class_name)],
    )
}

/// Helper: build `x respondsTo: #selector` keyword expression
fn responds_to(var_name: &str, selector_name: &str) -> Expression {
    msg_send(
        var(var_name),
        MessageSelector::Keyword(vec![KeywordPart::new("respondsTo:", span())]),
        vec![sym_lit(selector_name)],
    )
}

/// Helper: build a block with a non-local return `[^value]`
fn block_with_return(value: Expression) -> Expression {
    Expression::Block(Block::new(
        vec![],
        vec![ExpressionStatement::bare(Expression::Return {
            value: Box::new(value),
            span: span(),
        })],
        span(),
    ))
}

#[test]
fn test_narrowing_class_eq_in_true_block() {
    // Build a class with a method:
    //   process: x :: Object =>
    //     (x class = Integer) ifTrue: [x + 1]
    //     x + 1    // should warn — x is Object outside the block
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x class = Integer) ifTrue: [x + 1]
                if_true(
                    class_eq("x", "Integer"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Binary("+".into()),
                        vec![int_lit(1)],
                    )]),
                ),
                // x + 1  — should warn (Object doesn't have +)
                msg_send(
                    var("x"),
                    MessageSelector::Binary("+".into()),
                    vec![int_lit(1)],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Inside the block: `x + 1` should NOT warn because x is narrowed to Integer
    // Outside the block: `x + 1` SHOULD warn because x is still Object
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    // The outside `x + 1` should produce a warning
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning for x + 1 outside the narrowed block, got {}:\n{:?}",
        warnings.len(),
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_narrowing_class_eqeq_in_true_block() {
    // Same as above but with =:= operator
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x class =:= Integer) ifTrue: [x + 1]
                if_true(
                    class_eqeq("x", "Integer"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Binary("+".into()),
                        vec![int_lit(1)],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Inside the block: `x + 1` should NOT warn because x is narrowed to Integer
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "Expected no warnings for x + 1 inside narrowed block, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_narrowing_is_kind_of_in_true_block() {
    // process: x :: Object =>
    //   (x isKindOf: Integer) ifTrue: [x + 1]
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![if_true(
                is_kind_of("x", "Integer"),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Binary("+".into()),
                    vec![int_lit(1)],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "Expected no warnings inside isKindOf: narrowed block, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_narrowing_is_nil_early_return() {
    // validate: x :: Object =>
    //   x isNil ifTrue: [^nil]
    //   x size   // x should be non-nil, but still Object
    let hierarchy = ClassHierarchy::with_builtins();

    let class = {
        let validate_method = make_keyword_method(
            &["validate:"],
            vec![("x", Some("Object"))],
            vec![
                // x isNil ifTrue: [^nil]
                if_true(is_nil("x"), block_with_return(var("nil"))),
                // x size  — after early return, x is non-nil
                msg_send(var("x"), MessageSelector::Unary("size".into()), vec![]),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![validate_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // After early return narrowing, x is still Object (non-nil) but Object
    // does respond to `size` (it's a built-in), so no warning expected.
    // This test just verifies the narrowing doesn't crash.
}

#[test]
fn test_narrowing_is_nil_if_true_if_false() {
    // process: x :: Object =>
    //   x isNil ifTrue: [42] ifFalse: [x size]
    // In the false block, x should be non-nil.
    let hierarchy = ClassHierarchy::with_builtins();

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![if_true_if_false(
                is_nil("x"),
                block_expr(vec![int_lit(42)]),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("size".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // This just verifies the narrowing path executes without crash.
    // Object responds to `size`, so no warning expected.
}

#[test]
fn test_narrowing_union_with_nil_check() {
    // Compose union checking with narrowing:
    // A parameter typed as String|UndefinedObject (union), after nil check,
    // should narrow to String in the false block.
    //
    // This validates the `non_nil_type` subtraction logic directly. (Declared
    // union annotations *do* resolve to `InferredType::Union` via
    // `type_resolver` — see BT-2016 — so the union-narrowing path is also
    // exercised end-to-end by the `ifNil:`/`ifTrue:ifFalse:` tests elsewhere.)
    let union = InferredType::simple_union(&["String", "UndefinedObject"]);
    let narrowed = TypeChecker::non_nil_type(&union);
    assert_eq!(
        narrowed,
        InferredType::known("String"),
        "Removing UndefinedObject from String|UndefinedObject should yield String"
    );
}

#[test]
fn test_narrowing_union_multi_member() {
    // String | Integer | UndefinedObject → String | Integer after non_nil
    let union = InferredType::simple_union(&["String", "Integer", "UndefinedObject"]);
    let narrowed = TypeChecker::non_nil_type(&union);
    match narrowed {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&InferredType::known("String")));
            assert!(members.contains(&InferredType::known("Integer")));
        }
        _ => panic!("Expected Union type after narrowing, got: {narrowed:?}"),
    }
}

#[test]
fn test_union_order_independent_equality() {
    // A | B should equal B | A
    let ab = InferredType::simple_union(&["String", "Integer"]);
    let ba = InferredType::simple_union(&["Integer", "String"]);
    assert_eq!(ab, ba, "Union equality should be order-independent");
}

#[test]
fn test_union_with_generic_members() {
    // Result(Integer, String) | UndefinedObject
    let result_ty = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![
            InferredType::known("Integer"),
            InferredType::known("String"),
        ],
        provenance: TypeProvenance::Inferred(Span::default()),
    };
    let nil_ty = InferredType::known("UndefinedObject");
    let union = InferredType::union_of(&[result_ty.clone(), nil_ty]);

    // display_name should render generics
    let display = union.display_name().unwrap();
    assert!(
        display.contains("Result(Integer, String)"),
        "Union display should include generic args, got: {display}"
    );
    assert!(
        display.contains("UndefinedObject"),
        "Union display should include nil member, got: {display}"
    );

    // non_nil_type should preserve generic args
    let narrowed = TypeChecker::non_nil_type(&union);
    assert_eq!(
        narrowed, result_ty,
        "Narrowing away nil should preserve the full generic Result type"
    );
}

#[test]
fn test_union_deduplication() {
    // union_of([String, String, Integer]) should deduplicate to String | Integer
    let members = vec![
        InferredType::known("String"),
        InferredType::known("String"),
        InferredType::known("Integer"),
    ];
    let union = InferredType::union_of(&members);
    match union {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2, "Duplicate members should be removed");
        }
        _ => panic!("Expected Union, got: {union:?}"),
    }
}

#[test]
fn test_detect_narrowing_class_eq_pattern() {
    // (x class = Integer) → should detect narrowing for x to Integer
    let expr = class_eq("x", "Integer");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect class = narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable, EnvKey::local("x"));
    assert_eq!(info.true_type, InferredType::known("Integer"));
    assert!(!info.is_nil_check);
}

#[test]
fn test_detect_narrowing_is_kind_of_pattern() {
    let expr = is_kind_of("x", "Number");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect isKindOf: narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable, EnvKey::local("x"));
    assert_eq!(info.true_type, InferredType::known("Number"));
    assert!(!info.is_nil_check);
}

#[test]
fn test_detect_narrowing_is_nil_pattern() {
    let expr = is_nil("x");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect isNil narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable, EnvKey::local("x"));
    assert!(info.is_nil_check);
}

#[test]
fn test_detect_narrowing_no_match() {
    // `x + 1` is not a narrowing pattern
    let expr = msg_send(
        var("x"),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    );
    assert!(
        TypeChecker::detect_narrowing(&expr).is_none(),
        "x + 1 should not be detected as narrowing"
    );
}

// ---- Singleton (in)equality narrowing (BT-2617) ----

#[test]
fn test_detect_narrowing_singleton_eq() {
    // `ms = #infinity` records the singleton, not negated.
    let expr = msg_send(
        var("ms"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("infinity")],
    );
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect singleton =");
    assert_eq!(info.variable, EnvKey::local("ms"));
    let eq = info.singleton_eq.expect("singleton_eq recorded");
    assert_eq!(eq.singleton.as_str(), "#infinity");
    assert!(!eq.negated);
}

#[test]
fn test_detect_narrowing_singleton_eq_symmetric() {
    // `#infinity = ms` detects the same as `ms = #infinity`.
    let expr = msg_send(
        symbol_lit("infinity"),
        MessageSelector::Binary("=".into()),
        vec![var("ms")],
    );
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect #foo = x");
    assert_eq!(info.variable, EnvKey::local("ms"));
    assert_eq!(info.singleton_eq.unwrap().singleton.as_str(), "#infinity");
}

#[test]
fn test_detect_narrowing_singleton_inequality_is_negated() {
    for op in ["/=", "=/="] {
        let expr = msg_send(
            var("ms"),
            MessageSelector::Binary(op.into()),
            vec![symbol_lit("infinity")],
        );
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect singleton /=");
        assert!(
            info.singleton_eq.unwrap().negated,
            "operator {op} should be treated as negated"
        );
    }
}

#[test]
fn test_detect_narrowing_singleton_eq_through_nested_parens() {
    // `ms = ((#infinity))` — `symbol_literal` unwraps all paren levels.
    let nested = Expression::Parenthesized {
        expression: Box::new(Expression::Parenthesized {
            expression: Box::new(symbol_lit("infinity")),
            span: span(),
        }),
        span: span(),
    };
    let expr = msg_send(var("ms"), MessageSelector::Binary("=".into()), vec![nested]);
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect through nested parens");
    assert_eq!(info.variable, EnvKey::local("ms"));
    assert_eq!(info.singleton_eq.unwrap().singleton.as_str(), "#infinity");
}

#[test]
fn test_detect_narrowing_two_symbol_literals_no_match() {
    // `#a = #b` has no variable to narrow.
    let expr = msg_send(
        symbol_lit("a"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("b")],
    );
    assert!(TypeChecker::detect_narrowing(&expr).is_none());
}

#[test]
fn test_refine_singleton_eq_splits_union() {
    // ms :: Integer | #infinity, after `ms = #infinity`:
    //   true branch → #infinity, false branch → Integer.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("ms", InferredType::simple_union(&["Integer", "#infinity"]));
    let expr = msg_send(
        var("ms"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("infinity")],
    );
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    let refined = TypeChecker::refine_singleton_narrowing(info, &env, &hierarchy);
    assert_eq!(refined.true_type, InferredType::known("#infinity"));
    assert_eq!(refined.false_type, Some(InferredType::known("Integer")));
}

#[test]
fn test_refine_singleton_inequality_swaps_branches() {
    // `ms /= #infinity`: true branch is the remainder (Integer), false is the singleton.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("ms", InferredType::simple_union(&["Integer", "#infinity"]));
    let expr = msg_send(
        var("ms"),
        MessageSelector::Binary("/=".into()),
        vec![symbol_lit("infinity")],
    );
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    let refined = TypeChecker::refine_singleton_narrowing(info, &env, &hierarchy);
    assert_eq!(refined.true_type, InferredType::known("Integer"));
    assert_eq!(refined.false_type, Some(InferredType::known("#infinity")));
}

#[test]
fn test_refine_singleton_eq_multi_member_union_keeps_rest() {
    // d :: #north | #south | #east, after `d = #north`:
    //   false branch keeps #south | #east.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local(
        "d",
        InferredType::simple_union(&["#north", "#south", "#east"]),
    );
    let expr = msg_send(
        var("d"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("north")],
    );
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    let refined = TypeChecker::refine_singleton_narrowing(info, &env, &hierarchy);
    assert_eq!(refined.true_type, InferredType::known("#north"));
    assert_eq!(
        refined.false_type,
        Some(InferredType::simple_union(&["#south", "#east"]))
    );
}

#[test]
fn test_refine_singleton_eq_all_removed_is_never() {
    // d :: #north (already narrowed), after `d = #north`:
    //   the false branch is unreachable → Never.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("d", InferredType::known("#north"));
    let expr = msg_send(
        var("d"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("north")],
    );
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    let refined = TypeChecker::refine_singleton_narrowing(info, &env, &hierarchy);
    assert_eq!(refined.true_type, InferredType::known("#north"));
    assert_eq!(refined.false_type, Some(InferredType::Never));
}

#[test]
fn test_refine_singleton_eq_dynamic_variable_keeps_false_dynamic() {
    // An unbound variable resolves to Dynamic; `x = #foo` narrows the true
    // branch to #foo and leaves the false branch Dynamic — `difference(Dynamic,
    // #foo) = Dynamic` (Dynamic is opaque to subtraction, ADR 0102 §1).
    let hierarchy = ClassHierarchy::with_builtins();
    let env = TypeEnv::new(); // `x` is absent → Dynamic
    let expr = msg_send(
        var("x"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("foo")],
    );
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    let refined = TypeChecker::refine_singleton_narrowing(info, &env, &hierarchy);
    assert_eq!(refined.true_type, InferredType::known("#foo"));
    assert_eq!(
        refined.false_type,
        Some(InferredType::Dynamic(DynamicReason::Unknown))
    );
}

#[test]
fn test_refine_singleton_symbol_left_inequality_swaps_branches() {
    // `#infinity /= ms` — symbol on the left, negated. Same result as
    // `ms /= #infinity`: true branch is the remainder (Integer), false branch
    // is the singleton. (`negated` comes from the operator, not operand order.)
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("ms", InferredType::simple_union(&["Integer", "#infinity"]));
    let expr = msg_send(
        symbol_lit("infinity"),
        MessageSelector::Binary("/=".into()),
        vec![var("ms")],
    );
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    let refined = TypeChecker::refine_singleton_narrowing(info, &env, &hierarchy);
    assert_eq!(refined.true_type, InferredType::known("Integer"));
    assert_eq!(refined.false_type, Some(InferredType::known("#infinity")));
}

/// BT-2740 / ADR 0102 §2 (pinned corner 1): an impossible singleton test
/// (`x :: Integer; x = #foo`) now narrows the *true* branch to `Never` via
/// `intersect(Integer, #foo) = Never`. Previously `union_without` returned the
/// singleton (`#foo`) unconditionally for the true branch even though it is
/// unreachable. The "can never be true" diagnostic still fires elsewhere
/// (`check_impossible_singleton_comparison`); only the branch type changed.
#[test]
fn test_refine_singleton_eq_impossible_true_branch_is_never() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::known("Integer"));
    let expr = msg_send(
        var("x"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("foo")],
    );
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    let refined = TypeChecker::refine_singleton_narrowing(info, &env, &hierarchy);
    // True branch is unreachable — the value can never be `#foo`.
    assert_eq!(refined.true_type, InferredType::Never);
    // False branch keeps the full `Integer` type (nothing removable).
    assert_eq!(refined.false_type, Some(InferredType::known("Integer")));
}

/// BT-2740 / ADR 0102 §2 (new capability): the false branch of `x = #foo` on a
/// bare `Symbol` receiver narrows to the negation `Symbol \ #foo` (every symbol
/// except `#foo`) and renders as `Symbol \ #foo` in hover.
#[test]
fn test_refine_singleton_eq_symbol_false_branch_is_negation() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("s", InferredType::known("Symbol"));
    let expr = msg_send(
        var("s"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("foo")],
    );
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    let refined = TypeChecker::refine_singleton_narrowing(info, &env, &hierarchy);
    // True branch narrows to the singleton itself.
    assert_eq!(refined.true_type, InferredType::known("#foo"));
    // False branch is the complement `Symbol \ #foo`.
    let false_ty = refined.false_type.expect("false branch present");
    assert!(
        matches!(false_ty, InferredType::Negation { .. }),
        "expected a Negation, got: {false_ty:?}"
    );
    assert_eq!(false_ty.display_for_diagnostic().unwrap(), "Symbol \\ #foo");
}

/// BT-2624 (item 1) / BT-2631: testing a variable against a singleton it can
/// never hold (`#west` is not a member of `#north | #south`) is statically false
/// — emit exactly one "can never be true" hint even when the comparison guards
/// an `ifTrue:` (the guard receiver and the narrowing must not double-fire).
#[test]
fn bt2624_singleton_eq_impossible_member_emits_hint() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("d", InferredType::simple_union(&["#north", "#south"]));
    // `(d = #west) ifTrue: [nil]` — the comparison is the guard receiver.
    let guard = msg_send(
        var("d"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("west")],
    );
    let expr = if_true(guard, block_expr(vec![var("nil")]));
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    let hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("can never be true"))
        .collect();
    assert_eq!(
        hints.len(),
        1,
        "expected exactly one impossibility hint (no double-fire), got: {:?}",
        checker.diagnostics()
    );
    assert!(
        hints[0].message.contains("#west") && hints[0].message.contains("#north | #south"),
        "hint should name the singleton and the type: {}",
        hints[0].message
    );
}

/// BT-2624 (item 1): the negated form against an impossible member is always
/// true — emit the complementary "always true" hint (here in a guard position).
#[test]
fn bt2624_singleton_inequality_impossible_member_always_true() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("d", InferredType::simple_union(&["#north", "#south"]));
    let guard = msg_send(
        var("d"),
        MessageSelector::Binary("/=".into()),
        vec![symbol_lit("west")],
    );
    let expr = if_true(guard, block_expr(vec![var("nil")]));
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    let hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("always true") && d.message.contains("#west"))
        .collect();
    assert_eq!(
        hints.len(),
        1,
        "expected exactly one 'always true' hint, got: {:?}",
        checker.diagnostics()
    );
}

/// BT-2624 (item 1): a singleton that IS a member must not be flagged, and a
/// `Symbol`-typed variable admits any singleton — no hint.
#[test]
fn bt2624_singleton_eq_possible_member_no_hint() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();

    // `(d = #north) ifTrue: [nil]` — #north is a member, so satisfiable.
    let mut env = TypeEnv::new();
    env.set_local("d", InferredType::simple_union(&["#north", "#south"]));
    let member_guard = if_true(
        msg_send(
            var("d"),
            MessageSelector::Binary("=".into()),
            vec![symbol_lit("north")],
        ),
        block_expr(vec![var("nil")]),
    );
    let _ = checker.infer_expr(&member_guard, &hierarchy, &mut env, false);

    // `s :: Symbol` admits every singleton — no impossibility hint for
    // `s = #anything`.
    let mut sym_env = TypeEnv::new();
    sym_env.set_local("s", InferredType::known("Symbol"));
    let sym_expr = msg_send(
        var("s"),
        MessageSelector::Binary("=".into()),
        vec![symbol_lit("anything")],
    );
    let _ = checker.infer_expr(&sym_expr, &hierarchy, &mut sym_env, false);

    // No "can never be true" / "always true" impossibility hint from any of the
    // satisfiable comparisons above. (Other diagnostic kinds are out of scope
    // for this test.)
    assert!(
        !checker.diagnostics().iter().any(|d| {
            d.message.contains("can never be true") || d.message.contains("always true")
        }),
        "satisfiable comparisons must not emit an impossibility hint, got: {:?}",
        checker.diagnostics()
    );
}

/// BT-2631: the impossible-comparison hint must also fire for a *standalone*
/// singleton equality send (`unionVar =:= #west`), not only inside an
/// `ifTrue:`/`ifFalse:` guard. The send still infers `Boolean`; the hint is the
/// only added signal.
#[test]
fn bt2631_standalone_singleton_eq_impossible_member_emits_hint() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["#north", "#south"]));
    // `x =:= #west` — #west is not a member of `#north | #south`.
    let expr = msg_send(
        var("x"),
        MessageSelector::Binary("=:=".into()),
        vec![symbol_lit("west")],
    );
    let mut checker = TypeChecker::new();
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(
        ty,
        InferredType::known("Boolean"),
        "the comparison is still typed Boolean"
    );
    let hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("can never be true"))
        .collect();
    assert_eq!(
        hints.len(),
        1,
        "expected one standalone-send impossibility hint, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        hints[0].message.contains("#west") && hints[0].message.contains("#north | #south"),
        "hint should name the singleton and the type: {}",
        hints[0].message
    );
}

/// BT-2631: the negated standalone send (`unionVar =/= #west`) is always true.
#[test]
fn bt2631_standalone_singleton_inequality_impossible_member_always_true() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["#north", "#south"]));
    let expr = msg_send(
        var("x"),
        MessageSelector::Binary("=/=".into()),
        vec![symbol_lit("west")],
    );
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert!(
        checker
            .diagnostics()
            .iter()
            .any(|d| d.message.contains("always true") && d.message.contains("#west")),
        "expected a standalone-send 'always true' hint, got: {:?}",
        checker.diagnostics()
    );
}

/// BT-2631: the union operand may be on either side — `#west =:= unionVar`
/// (singleton on the left, union as the *argument*) is just as decidable as the
/// receiver-side form, so the hint fires there too.
#[test]
fn bt2631_standalone_singleton_eq_union_on_right_emits_hint() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["#north", "#south"]));
    // `#west =:= x` — the union `x` is the argument, the singleton the receiver.
    let expr = msg_send(
        symbol_lit("west"),
        MessageSelector::Binary("=:=".into()),
        vec![var("x")],
    );
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    let hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("can never be true"))
        .collect();
    assert_eq!(
        hints.len(),
        1,
        "expected one hint for the union-on-right form, got: {:?}",
        checker.diagnostics()
    );
}

/// BT-2631: a standalone send whose singleton IS a member, or whose union
/// admits any singleton (a `Symbol` member), must stay silent — matching the
/// conservative guard-path behaviour.
#[test]
fn bt2631_standalone_singleton_eq_admitted_no_hint() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();

    // `x =:= #north` — #north IS a member, so the test is satisfiable.
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["#north", "#south"]));
    let member_expr = msg_send(
        var("x"),
        MessageSelector::Binary("=:=".into()),
        vec![symbol_lit("north")],
    );
    let _ = checker.infer_expr(&member_expr, &hierarchy, &mut env, false);

    // `#north | Symbol` admits every singleton (the `Symbol` member), so even a
    // non-listed singleton like #west is satisfiable — no hint.
    let mut sym_env = TypeEnv::new();
    sym_env.set_local(
        "y",
        InferredType::union_of(&[InferredType::known("#north"), InferredType::known("Symbol")]),
    );
    let sym_expr = msg_send(
        var("y"),
        MessageSelector::Binary("=:=".into()),
        vec![symbol_lit("west")],
    );
    let _ = checker.infer_expr(&sym_expr, &hierarchy, &mut sym_env, false);

    assert!(
        checker.diagnostics().is_empty(),
        "satisfiable comparisons must not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_narrowing_does_not_leak_outside_block() {
    // Verify narrowing is scoped to block only:
    //   process: x :: Object =>
    //     (x class = String) ifTrue: [x size]
    //     x unknownThing   // Object doesn't have unknownThing → warning
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                if_true(
                    class_eq("x", "String"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("size".into()),
                        vec![],
                    )]),
                ),
                msg_send(
                    var("x"),
                    MessageSelector::Unary("unknownThing".into()),
                    vec![],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    // 'unknownThing' on Object should warn (narrowing doesn't leak)
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning for unknownThing outside narrowed block, got {}",
        warnings.len(),
    );
}

// ---- BT-1582: respondsTo: narrowing tests (ADR 0068 Phase 2e) ----

#[test]
fn test_detect_narrowing_responds_to_pattern() {
    // `x respondsTo: #asString` → should detect narrowing for x
    let expr = responds_to("x", "asString");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect respondsTo: narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable, EnvKey::local("x"));
    assert_eq!(
        info.true_type,
        InferredType::Dynamic(DynamicReason::Unknown)
    );
    assert!(!info.is_nil_check);
    assert_eq!(
        info.responded_selector.as_deref(),
        Some("asString"),
        "Should record the tested selector"
    );
}

#[test]
fn test_detect_narrowing_responds_to_non_symbol_arg() {
    // `x respondsTo: someVar` — not a symbol literal → no narrowing
    let expr = msg_send(
        var("x"),
        MessageSelector::Keyword(vec![KeywordPart::new("respondsTo:", span())]),
        vec![var("someVar")],
    );
    assert!(
        TypeChecker::detect_narrowing(&expr).is_none(),
        "respondsTo: with non-symbol argument should not be detected as narrowing"
    );
}

#[test]
fn test_narrowing_responds_to_in_true_block() {
    // Build a class with a method:
    //   process: x :: Object =>
    //     (x respondsTo: #customMethod) ifTrue: [x customMethod]
    //     x customMethod    // should warn — x is Object outside the block
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #customMethod) ifTrue: [x customMethod]
                if_true(
                    responds_to("x", "customMethod"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("customMethod".into()),
                        vec![],
                    )]),
                ),
                // x customMethod — should warn (Object doesn't have customMethod)
                msg_send(
                    var("x"),
                    MessageSelector::Unary("customMethod".into()),
                    vec![],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Inside the block: `x customMethod` should NOT warn because x is Dynamic
    // (respondsTo: narrowing sets the variable to Dynamic in the true block)
    // Outside the block: `x customMethod` SHOULD warn because x is still Object
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning for customMethod outside the narrowed block, got {}:\n{:?}",
        warnings.len(),
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_narrowing_responds_to_does_not_leak() {
    // Verify narrowing from respondsTo: is scoped to block only:
    //   process: x :: Object =>
    //     (x respondsTo: #asString) ifTrue: [x asString]
    //     x unknownSelector   // Object doesn't have this → warning
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                if_true(
                    responds_to("x", "asString"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("asString".into()),
                        vec![],
                    )]),
                ),
                msg_send(
                    var("x"),
                    MessageSelector::Unary("unknownSelector".into()),
                    vec![],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    // 'unknownSelector' on Object should warn (narrowing doesn't leak)
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning for unknownSelector outside narrowed block, got {}",
        warnings.len(),
    );
}

#[test]
fn test_narrowing_responds_to_if_true_if_false() {
    // Build: (x respondsTo: #customMethod) ifTrue: [x customMethod] ifFalse: [x customMethod]
    // True block: no warning (narrowed to Dynamic)
    // False block: should warn (x is still Object, no respondsTo: narrowing in false branch)
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![if_true_if_false(
                responds_to("x", "customMethod"),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("customMethod".into()),
                    vec![],
                )]),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("customMethod".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // True block: no warning (Dynamic narrowing)
    // False block: should warn (Object doesn't have customMethod)
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 warning in false block (no respondsTo: narrowing there), got {}:\n{:?}",
        warnings.len(),
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_protocol_inference() {
    // When `x respondsTo: #asString` is detected, and a Printable protocol
    // requiring asString exists, the responded_selector can be used to infer
    // protocol conformance (ADR 0068 Phase 2e).
    let hierarchy = ClassHierarchy::with_builtins();

    // Build a Printable protocol requiring asString
    let printable_proto = ProtocolDefinition {
        name: ident("Printable"),
        type_params: vec![],
        extending: None,
        method_signatures: vec![ProtocolMethodSignature {
            selector: MessageSelector::Unary("asString".into()),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(ident("String"))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_method_signatures: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let module = Module {
        protocols: vec![printable_proto],
        ..Module::new(vec![], span())
    };
    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&module, &hierarchy);
    assert!(diags.is_empty());

    // Detect narrowing from `x respondsTo: #asString`
    let expr = responds_to("x", "asString");
    let info = TypeChecker::detect_narrowing(&expr).unwrap();
    assert_eq!(info.responded_selector.as_deref(), Some("asString"));

    // The responded_selector matches the Printable protocol's required method.
    // Integer conforms to Printable (has asString), String does too.
    let result = registry.check_conformance("Integer", "Printable", &hierarchy);
    assert!(
        result.is_ok(),
        "Integer responds to asString → conforms to Printable"
    );

    // A class without asString would not conform
    // (This verifies the protocol registry is usable for narrowing inference)
    let proto = registry.get("Printable").unwrap();
    let required: Vec<&str> = proto
        .all_required_selectors(&registry)
        .iter()
        .map(|s| s.as_str())
        .collect();
    assert!(
        required.contains(&"asString"),
        "Printable protocol requires asString"
    );
}

// ---- BT-1833: respondsTo: narrows to protocol type ----

/// Helper: build a protocol module with a single protocol requiring a single selector.
fn make_protocol_module(
    protocol_name: &str,
    required_selector: &str,
) -> (Module, ProtocolRegistry, ClassHierarchy) {
    let hierarchy = ClassHierarchy::with_builtins();
    let proto_def = ProtocolDefinition {
        name: ident(protocol_name),
        type_params: vec![],
        extending: None,
        method_signatures: vec![ProtocolMethodSignature {
            selector: MessageSelector::Unary(required_selector.into()),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(ident("String"))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_method_signatures: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let module = Module {
        protocols: vec![proto_def],
        ..Module::new(vec![], span())
    };
    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&module, &hierarchy);
    assert!(diags.is_empty());
    (module, registry, hierarchy)
}

#[test]
fn test_responds_to_narrows_to_protocol_type() {
    // When `x respondsTo: #asString` with a Printable protocol requiring asString,
    // the true-branch should narrow x to Printable (not Dynamic).
    let (_, registry, hierarchy) = make_protocol_module("Printable", "asString");

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #asString) ifTrue: [x asString]
                if_true(
                    responds_to("x", "asString"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("asString".into()),
                        vec![],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // asString is a valid method on Printable protocol, so no DNU warning
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings (x narrowed to Printable), got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_no_protocol_match_falls_back_to_dynamic() {
    // When `x respondsTo: #unknownMethod` and no protocol requires that selector,
    // the narrowing should fall back to Dynamic (no warnings in true block).
    let (_, registry, hierarchy) = make_protocol_module("Printable", "asString");

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #unknownMethod) ifTrue: [x unknownMethod]
                if_true(
                    responds_to("x", "unknownMethod"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("unknownMethod".into()),
                        vec![],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // Dynamic suppresses DNU, so no warning in the true block
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings (x narrowed to Dynamic), got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_protocol_narrows_validates_methods() {
    // When narrowed to Printable, sending a method NOT on the protocol should warn.
    let (_, registry, hierarchy) = make_protocol_module("Printable", "asString");

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #asString) ifTrue: [x nonExistentMethod]
                if_true(
                    responds_to("x", "asString"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("nonExistentMethod".into()),
                        vec![],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // Printable only has asString — nonExistentMethod should warn
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_warnings.len(),
        1,
        "Expected 1 DNU warning for nonExistentMethod on Printable, got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_ambiguous_protocols_falls_back_to_dynamic() {
    // When multiple protocols require the same selector, fall back to Dynamic.
    let hierarchy = ClassHierarchy::with_builtins();

    // Create two protocols both requiring 'asString'
    let proto1 = ProtocolDefinition {
        name: ident("Printable"),
        type_params: vec![],
        extending: None,
        method_signatures: vec![ProtocolMethodSignature {
            selector: MessageSelector::Unary("asString".into()),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(ident("String"))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_method_signatures: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let proto2 = ProtocolDefinition {
        name: ident("Displayable"),
        type_params: vec![],
        extending: None,
        method_signatures: vec![ProtocolMethodSignature {
            selector: MessageSelector::Unary("asString".into()),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple(ident("String"))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_method_signatures: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let proto_module = Module {
        protocols: vec![proto1, proto2],
        ..Module::new(vec![], span())
    };
    let mut registry = ProtocolRegistry::new();
    registry.register_module(&proto_module, &hierarchy);

    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![
                // (x respondsTo: #asString) ifTrue: [x unknownMethod]
                if_true(
                    responds_to("x", "asString"),
                    block_expr(vec![msg_send(
                        var("x"),
                        MessageSelector::Unary("unknownMethod".into()),
                        vec![],
                    )]),
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // With ambiguous protocols, x stays Dynamic — no DNU warning for unknownMethod
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings (ambiguous protocols → Dynamic), got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_responds_to_without_protocol_registry_stays_dynamic() {
    // When check_module is called without protocols (no registry),
    // respondsTo: narrowing stays Dynamic (backward compatible).
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Object"))],
            vec![if_true(
                responds_to("x", "customMethod"),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("customMethod".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    // Use check_module (no protocol registry) — should still work
    checker.check_module(&module, &hierarchy);

    // Dynamic suppresses DNU, so no warning
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Expected no DNU warnings (Dynamic narrowing, no registry), got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_find_unique_protocol_for_selector() {
    // Unit test for the ProtocolRegistry helper method.
    let (_, registry, _) = make_protocol_module("Printable", "asString");

    // Unique match: asString → Printable
    let result = registry.find_unique_protocol_for_selector("asString");
    assert_eq!(
        result.map(EcoString::as_str),
        Some("Printable"),
        "asString should uniquely match Printable"
    );

    // No match: unknownSelector → None
    let result = registry.find_unique_protocol_for_selector("unknownSelector");
    assert!(
        result.is_none(),
        "unknownSelector should not match any protocol"
    );
}
