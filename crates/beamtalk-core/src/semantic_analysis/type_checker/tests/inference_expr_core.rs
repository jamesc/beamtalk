// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Core `infer_expr` expression-type inference, `Pattern::Nil`/`Pattern::Type`
//! narrowing, `build_substitution_map`, and `infer_stmts`.

use super::common::*;
use crate::ast::BlockParameter;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;

// ---- infer_expr: core expression type inference ----

#[test]
fn infer_expr_integer_literal() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let ty = checker.infer_expr(&int_lit(42), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Integer"));
}

#[test]
fn infer_expr_string_literal() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let ty = checker.infer_expr(&str_lit("hello"), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("String"));
}

#[test]
fn infer_expr_true_identifier() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let ty = checker.infer_expr(&var("true"), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Boolean"));
}

#[test]
fn infer_expr_false_identifier() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let ty = checker.infer_expr(&var("false"), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Boolean"));
}

#[test]
fn infer_expr_nil_identifier() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let ty = checker.infer_expr(&var("nil"), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("UndefinedObject"));
}

#[test]
fn infer_expr_self_identifier() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Counter"));
    let ty = checker.infer_expr(&var("self"), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Counter"));
}

#[test]
fn infer_expr_env_variable() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("myVar", InferredType::known("String"));
    let ty = checker.infer_expr(&var("myVar"), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("String"));
}

#[test]
fn infer_expr_unknown_var_is_dynamic() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let ty = checker.infer_expr(&var("unknownVar"), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::Dynamic(DynamicReason::Unknown));
}

#[test]
fn infer_expr_class_reference() {
    // a bare class literal `Integer` is the class *object*, whose
    // type is the metatype `Meta{Integer}` — not an instance of `Integer`.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let ty = checker.infer_expr(&class_ref("Integer"), &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::meta("Integer"));
}

#[test]
fn infer_expr_assignment_tracks_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::Assignment {
        target: Box::new(var("x")),
        value: Box::new(int_lit(42)),
        type_annotation: None,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Integer"));
    // The variable should now be tracked in the environment
    assert_eq!(env.get_local("x"), Some(InferredType::known("Integer")));
}

#[test]
fn infer_expr_block_returns_block_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let block = Expression::Block(Block::new(
        vec![BlockParameter::new("x", span())],
        vec![ExpressionStatement::bare(int_lit(1))],
        span(),
    ));
    let ty = checker.infer_expr(&block, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Block"));
}

#[test]
fn infer_expr_map_literal() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::MapLiteral {
        pairs: vec![],
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    // empty map literal carries no key/value info → Dictionary(Dynamic, Dynamic).
    assert_eq!(
        ty,
        InferredType::known_with_args(
            "Dictionary",
            vec![
                InferredType::Dynamic(DynamicReason::Unknown),
                InferredType::Dynamic(DynamicReason::Unknown),
            ],
        )
    );
}

#[test]
fn infer_expr_array_literal() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::ArrayLiteral {
        elements: vec![int_lit(1), int_lit(2)],
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    // homogeneous Integer elements → Array(Integer).
    assert_eq!(
        ty,
        InferredType::known_with_args("Array", vec![InferredType::known("Integer")])
    );
}

#[test]
fn infer_expr_list_literal() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::ListLiteral {
        elements: vec![int_lit(1)],
        tail: None,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    // homogeneous Integer elements → List(Integer).
    assert_eq!(
        ty,
        InferredType::known_with_args("List", vec![InferredType::known("Integer")])
    );
}

#[test]
fn infer_expr_array_literal_heterogeneous_joins_to_union() {
    // mixed element types join into a union element type.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::ArrayLiteral {
        elements: vec![int_lit(1), str_lit("a")],
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(
        ty,
        InferredType::known_with_args(
            "Array",
            vec![InferredType::simple_union(&["Integer", "String"])]
        )
    );
}

#[test]
fn infer_expr_array_literal_empty_is_dynamic_element() {
    // an empty literal carries no element info → Array(Dynamic).
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::ArrayLiteral {
        elements: vec![],
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(
        ty,
        InferredType::known_with_args("Array", vec![InferredType::Dynamic(DynamicReason::Unknown)])
    );
}

#[test]
fn infer_expr_list_literal_folds_typed_tail_element() {
    // a `List(Integer)` tail contributes its `Integer` element to
    // the join, so `[1 | someIntList]` stays `List(Integer)`.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "rest",
        InferredType::known_with_args("List", vec![InferredType::known("Integer")]),
    );
    let expr = Expression::ListLiteral {
        elements: vec![int_lit(1)],
        tail: Some(Box::new(var("rest"))),
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(
        ty,
        InferredType::known_with_args("List", vec![InferredType::known("Integer")])
    );
}

#[test]
fn infer_expr_list_literal_array_tail_widens_to_dynamic() {
    // an Array is not a valid BEAM cons tail (it would form an
    // improper list), so it contributes no element type — folding a
    // `Dynamic` into the join collapses the element to `Dynamic`, giving
    // `List(Dynamic)` rather than a false `List(Integer)`.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "rest",
        InferredType::known_with_args("Array", vec![InferredType::known("Integer")]),
    );
    let expr = Expression::ListLiteral {
        elements: vec![int_lit(1)],
        tail: Some(Box::new(var("rest"))),
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(
        ty,
        InferredType::known_with_args("List", vec![InferredType::Dynamic(DynamicReason::Unknown)])
    );
    // the silent widening is now also surfaced as a diagnostic so
    // the user sees the likely improper-list bug.
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Array cons tail should emit one improper-list diagnostic, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        checker.diagnostics()[0].message.contains("improper list"),
        "Diagnostic should mention improper list, got: {:?}",
        checker.diagnostics()[0]
    );
    assert!(
        checker.diagnostics()[0].message.contains("Array"),
        "Diagnostic should name the offending Array tail, got: {:?}",
        checker.diagnostics()[0]
    );
}

#[test]
fn infer_expr_list_literal_list_tail_no_diagnostic() {
    // a proper `List(T)` cons tail is valid — no diagnostic.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "rest",
        InferredType::known_with_args("List", vec![InferredType::known("Integer")]),
    );
    let expr = Expression::ListLiteral {
        elements: vec![int_lit(1)],
        tail: Some(Box::new(var("rest"))),
        span: span(),
    };
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert!(
        checker.diagnostics().is_empty(),
        "List cons tail is proper — no diagnostic expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn infer_expr_list_literal_dynamic_tail_no_diagnostic() {
    // a `Dynamic` tail (e.g. unannotated param) is too uncertain to
    // flag — staying silent avoids false-positive noise.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("rest", InferredType::Dynamic(DynamicReason::Unknown));
    let expr = Expression::ListLiteral {
        elements: vec![int_lit(1)],
        tail: Some(Box::new(var("rest"))),
        span: span(),
    };
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert!(
        checker.diagnostics().is_empty(),
        "Dynamic cons tail should not be flagged, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn infer_expr_list_literal_non_collection_tail_warns() {
    // any known non-List tail (e.g. an Integer) would form an
    // improper list, so it is flagged too — not just Array.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("rest", InferredType::known("Integer"));
    let expr = Expression::ListLiteral {
        elements: vec![int_lit(1)],
        tail: Some(Box::new(var("rest"))),
        span: span(),
    };
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Integer cons tail should emit one improper-list diagnostic, got: {:?}",
        checker.diagnostics()
    );
    assert!(checker.diagnostics()[0].message.contains("Integer"));
}

#[test]
fn infer_expr_string_interpolation() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::StringInterpolation {
        segments: vec![
            crate::ast::StringSegment::Literal("hello ".into()),
            crate::ast::StringSegment::Interpolation(var("name")),
        ],
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("String"));
}

#[test]
fn infer_expr_return_propagates_value_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::Return {
        value: Box::new(str_lit("done")),
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("String"));
}

#[test]
fn infer_expr_parenthesized_unwraps() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::Parenthesized {
        expression: Box::new(int_lit(7)),
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Integer"));
}

#[test]
fn infer_expr_primitive_is_dynamic() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::Primitive {
        name: "add".into(),
        is_quoted: false,
        is_intrinsic: false,
        is_inferred: false,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::Dynamic(DynamicReason::Unknown));
}

#[test]
fn infer_expr_match_is_dynamic() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let expr = Expression::Match {
        value: Box::new(int_lit(1)),
        arms: vec![],
        exhaustive: false,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::Dynamic(DynamicReason::Unknown));
}

// ---- ADR 0107 Phase A: `Pattern::Nil` narrowing ----

/// A `nil` arm's body sees the scrutinee narrowed to `UndefinedObject`,
/// mirroring `x isNil ifTrue:` (reuses the same true-branch type as
/// `is_nil.rs`).
#[test]
fn infer_expr_match_nil_arm_narrows_scrutinee_to_undefined_object() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["String", "Nil"]));

    // x match: [nil -> x; _ -> 0]
    let expr = Expression::Match {
        value: Box::new(var("x")),
        arms: vec![
            MatchArm::new(Pattern::Nil(span()), var("x"), span()),
            MatchArm::new(Pattern::Wildcard(span()), int_lit(0), span()),
        ],
        exhaustive: false,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    // Arm types: [UndefinedObject (nil arm's `x`), Integer (wildcard arm)]
    assert_eq!(
        ty,
        InferredType::union_of(&[
            InferredType::known(WellKnownClass::UndefinedObject.as_str()),
            InferredType::known("Integer"),
        ])
    );
}

/// An unguarded `nil` arm removes `Nil` from what subsequent arms see,
/// mirroring `x isNil ifFalse:`'s `non_nil_type` narrowing.
#[test]
fn infer_expr_match_unguarded_nil_arm_narrows_subsequent_arms() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["String", "Nil"]));

    // x match: [nil -> 0; y -> x]  -- second arm's body reads `x`, which
    // should be narrowed to non-nil (`String`) after the unguarded nil arm.
    let expr = Expression::Match {
        value: Box::new(var("x")),
        arms: vec![
            MatchArm::new(Pattern::Nil(span()), int_lit(0), span()),
            MatchArm::new(
                Pattern::Variable(Identifier::new("y", span())),
                var("x"),
                span(),
            ),
        ],
        exhaustive: false,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    // Arm types: [Integer (nil arm), String (narrowed `x` in 2nd arm)]
    assert_eq!(
        ty,
        InferredType::union_of(&[
            InferredType::known("Integer"),
            InferredType::known("String")
        ])
    );
}

/// A *guarded* `nil when: [...] ->` arm does not guarantee coverage, so
/// it must not narrow away `Nil` for subsequent arms.
#[test]
fn infer_expr_match_guarded_nil_arm_does_not_narrow_subsequent_arms() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["String", "Nil"]));

    let expr = Expression::Match {
        value: Box::new(var("x")),
        arms: vec![
            MatchArm::with_guard(Pattern::Nil(span()), var("x"), int_lit(0), span()),
            MatchArm::new(
                Pattern::Variable(Identifier::new("y", span())),
                var("x"),
                span(),
            ),
        ],
        exhaustive: false,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    // Second arm still sees `x` as `String | Nil` (unchanged residual).
    assert_eq!(
        ty,
        InferredType::union_of(&[
            InferredType::known("Integer"),
            InferredType::simple_union(&["String", "Nil"]),
        ])
    );
}

// ---- ADR 0107 Phase A: `Pattern::Type` narrowing ----

/// A `binding :: ClassName` arm's body sees `binding` statically
/// narrowed to `ClassName`, mirroring `isKindOf:`'s true-branch
/// narrowing (`intersect_with_class`).
#[test]
fn infer_expr_match_type_arm_narrows_binding_to_class() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["String", "Integer"]));

    // x match: [s :: String -> s; n :: Integer -> n]
    let expr = Expression::Match {
        value: Box::new(var("x")),
        arms: vec![
            MatchArm::new(
                Pattern::Type {
                    binding: Identifier::new("s", span()),
                    class: Identifier::new("String", span()),
                    span: span(),
                },
                var("s"),
                span(),
            ),
            MatchArm::new(
                Pattern::Type {
                    binding: Identifier::new("n", span()),
                    class: Identifier::new("Integer", span()),
                    span: span(),
                },
                var("n"),
                span(),
            ),
        ],
        exhaustive: false,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    // Each arm's body is the narrowed binding, not the original union.
    assert_eq!(
        ty,
        InferredType::union_of(&[
            InferredType::known("String"),
            InferredType::known("Integer"),
        ])
    );
}

/// An unguarded `Type` arm removes `ClassName` from what subsequent arms
/// see, mirroring `isKindOf:`'s false-branch `\` (difference) narrowing
/// — and the *scrutinee's own name* (not just the pattern's binding)
/// sees the same narrowed type, since they denote the same value.
#[test]
fn infer_expr_match_unguarded_type_arm_narrows_subsequent_arms() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["String", "Integer"]));

    // x match: [s :: String -> 0; y -> x]  -- second arm's body reads
    // `x` (the scrutinee, not `s`), which should be narrowed to
    // `Integer` after the unguarded String arm.
    let expr = Expression::Match {
        value: Box::new(var("x")),
        arms: vec![
            MatchArm::new(
                Pattern::Type {
                    binding: Identifier::new("s", span()),
                    class: Identifier::new("String", span()),
                    span: span(),
                },
                int_lit(0),
                span(),
            ),
            MatchArm::new(
                Pattern::Variable(Identifier::new("y", span())),
                var("x"),
                span(),
            ),
        ],
        exhaustive: false,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(
        ty,
        InferredType::union_of(&[
            InferredType::known("Integer"),
            InferredType::known("Integer"),
        ])
    );
}

/// A *guarded* `binding :: ClassName when: [...] ->` arm does not
/// guarantee coverage, so it must not narrow away `ClassName` for
/// subsequent arms (same rule the guarded `nil` case above uses).
#[test]
fn infer_expr_match_guarded_type_arm_does_not_narrow_subsequent_arms() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["String", "Integer"]));

    let expr = Expression::Match {
        value: Box::new(var("x")),
        arms: vec![
            MatchArm::with_guard(
                Pattern::Type {
                    binding: Identifier::new("s", span()),
                    class: Identifier::new("String", span()),
                    span: span(),
                },
                var("s"),
                int_lit(0),
                span(),
            ),
            MatchArm::new(
                Pattern::Variable(Identifier::new("y", span())),
                var("x"),
                span(),
            ),
        ],
        exhaustive: false,
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    // Second arm still sees `x` as `String | Integer` (unchanged residual).
    assert_eq!(
        ty,
        InferredType::union_of(&[
            InferredType::known("Integer"),
            InferredType::simple_union(&["String", "Integer"]),
        ])
    );
}

// ---- build_substitution_map ----

#[test]
fn build_substitution_map_empty_args() {
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::build_substitution_map(&hierarchy, "Array", &[]);
    assert!(result.is_empty());
}

// ---- infer_stmts ----

#[test]
fn infer_stmts_empty_is_dynamic() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let result = checker.infer_stmts(&[], &hierarchy, &mut env, false);
    assert_eq!(result, InferredType::Dynamic(DynamicReason::Unknown));
}

#[test]
fn infer_stmts_returns_last_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let stmts = vec![
        ExpressionStatement::bare(int_lit(1)),
        ExpressionStatement::bare(str_lit("hello")),
    ];
    let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn infer_stmts_skips_expect_directives() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let stmts = vec![
        ExpressionStatement::bare(int_lit(42)),
        ExpressionStatement::bare(Expression::ExpectDirective {
            categories: vec![ExpectCategory::Dnu],
            reason: None,
            span: span(),
        }),
    ];
    // The last non-directive is int_lit(42), so result should be Integer
    let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn infer_stmts_stops_at_return() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let stmts = vec![
        ExpressionStatement::bare(Expression::Return {
            value: Box::new(int_lit(1)),
            span: span(),
        }),
        ExpressionStatement::bare(str_lit("unreachable")),
    ];
    let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
    assert_eq!(result, InferredType::known("Integer"));
}

// ---- is_self_receiver ----

#[test]
fn is_self_receiver_true() {
    assert!(TypeChecker::is_self_receiver(&var("self")));
}

#[test]
fn is_self_receiver_false_for_other_ident() {
    assert!(!TypeChecker::is_self_receiver(&var("other")));
}

#[test]
fn is_self_receiver_false_for_non_ident() {
    assert!(!TypeChecker::is_self_receiver(&int_lit(1)));
}
