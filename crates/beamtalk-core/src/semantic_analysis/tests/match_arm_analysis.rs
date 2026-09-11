// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `match:` arm analysis: pattern binding and scoping (simple/
//! tuple/list patterns, guards, multiple arms) and undefined-
//! variable diagnostics inside arm bodies and guards.

use super::*;

#[test]
fn test_match_arm_analysis_with_simple_pattern() {
    // Test that pattern variables are accessible in the body
    // value match: [x -> x]
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "value",
            test_span(),
        ))),
        arms: vec![MatchArm::new(
            Pattern::Variable(Identifier::new("x", test_span())),
            Expression::Identifier(Identifier::new("x", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should have diagnostic for 'value' only, not 'x'
    assert_eq!(result.diagnostics.len(), 1);
    assert!(
        result.diagnostics[0]
            .message
            .contains("Undefined variable: value")
    );
}

#[test]
fn test_match_arm_analysis_with_guard() {
    // Test that pattern variables are accessible in guards
    // value match: [x when x > 0 -> x]
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "value",
            test_span(),
        ))),
        arms: vec![MatchArm::with_guard(
            Pattern::Variable(Identifier::new("x", test_span())),
            Expression::Identifier(Identifier::new("x", test_span())), // guard uses x
            Expression::Identifier(Identifier::new("x", test_span())), // body uses x
            test_span(),
        )],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should have diagnostic for 'value' only, not 'x' (used in guard and body)
    assert_eq!(result.diagnostics.len(), 1);
    assert!(
        result.diagnostics[0]
            .message
            .contains("Undefined variable: value")
    );
}

#[test]
fn test_match_arm_analysis_with_tuple_pattern() {
    // Test nested patterns: {#ok, value} -> value
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "result",
            test_span(),
        ))),
        arms: vec![MatchArm::new(
            Pattern::Tuple {
                elements: vec![
                    Pattern::Literal(Literal::Symbol("ok".into()), test_span()),
                    Pattern::Variable(Identifier::new("value", test_span())),
                ],
                span: test_span(),
            },
            Expression::Identifier(Identifier::new("value", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should have diagnostic for 'result' only, not 'value' (pattern-bound)
    assert_eq!(result.diagnostics.len(), 1);
    assert!(
        result.diagnostics[0]
            .message
            .contains("Undefined variable: result")
    );
}

#[test]
fn test_match_arm_analysis_multiple_arms() {
    // Test multiple arms with different patterns
    // result match: [
    //   {#ok, value} -> value;
    //   {#error, msg} -> msg
    // ]
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "result",
            test_span(),
        ))),
        arms: vec![
            MatchArm::new(
                Pattern::Tuple {
                    elements: vec![
                        Pattern::Literal(Literal::Symbol("ok".into()), test_span()),
                        Pattern::Variable(Identifier::new("value", test_span())),
                    ],
                    span: test_span(),
                },
                Expression::Identifier(Identifier::new("value", test_span())),
                test_span(),
            ),
            MatchArm::new(
                Pattern::Tuple {
                    elements: vec![
                        Pattern::Literal(Literal::Symbol("error".into()), test_span()),
                        Pattern::Variable(Identifier::new("msg", test_span())),
                    ],
                    span: test_span(),
                },
                Expression::Identifier(Identifier::new("msg", test_span())),
                test_span(),
            ),
        ],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should have diagnostic for 'result' only, not 'value' or 'msg' (pattern-bound)
    assert_eq!(result.diagnostics.len(), 1);
    assert!(
        result.diagnostics[0]
            .message
            .contains("Undefined variable: result")
    );
}

#[test]
fn test_match_arm_analysis_with_list_pattern() {
    // Test list patterns: [head | tail] -> head
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new("list", test_span()))),
        arms: vec![MatchArm::new(
            Pattern::List {
                elements: vec![Pattern::Variable(Identifier::new("head", test_span()))],
                tail: Some(Box::new(Pattern::Variable(Identifier::new(
                    "tail",
                    test_span(),
                )))),
                span: test_span(),
            },
            Expression::Identifier(Identifier::new("head", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should have diagnostic for 'list' only, not 'head' or 'tail' (pattern-bound)
    assert_eq!(result.diagnostics.len(), 1);
    assert!(
        result.diagnostics[0]
            .message
            .contains("Undefined variable: list")
    );
}

#[test]
fn test_match_arm_scope_isolation() {
    // Test that variables from one arm don't leak to another
    // This test verifies that each arm gets its own scope
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "value",
            test_span(),
        ))),
        arms: vec![
            MatchArm::new(
                Pattern::Variable(Identifier::new("x", test_span())),
                Expression::Identifier(Identifier::new("x", test_span())),
                test_span(),
            ),
            MatchArm::new(
                Pattern::Variable(Identifier::new("y", test_span())),
                Expression::Identifier(Identifier::new("y", test_span())),
                test_span(),
            ),
        ],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should have diagnostic for 'value' only, not 'x' or 'y' (pattern-bound in separate arms)
    assert_eq!(result.diagnostics.len(), 1);
    assert!(
        result.diagnostics[0]
            .message
            .contains("Undefined variable: value")
    );
}

#[test]
fn test_undefined_variable_in_match_arm_body() {
    // Test that undefined variables produce diagnostics
    // value match: [x -> undefined_var]
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "value",
            test_span(),
        ))),
        arms: vec![MatchArm::new(
            Pattern::Variable(Identifier::new("x", test_span())),
            Expression::Identifier(Identifier::new("undefined_var", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should have 2 diagnostics: value and undefined_var
    assert_eq!(result.diagnostics.len(), 2);
    assert!(
        result.diagnostics[1]
            .message
            .contains("Undefined variable: undefined_var")
    );
}

#[test]
fn test_undefined_variable_in_guard() {
    // Test that undefined variables in guards produce diagnostics
    // value match: [x when undefined_var -> x]
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "value",
            test_span(),
        ))),
        arms: vec![MatchArm::with_guard(
            Pattern::Variable(Identifier::new("x", test_span())),
            Expression::Identifier(Identifier::new("undefined_var", test_span())),
            Expression::Identifier(Identifier::new("x", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should have diagnostic for undefined_var in guard
    assert_eq!(result.diagnostics.len(), 2); // value and undefined_var
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined variable: undefined_var"))
    );
}

#[test]
fn test_pattern_bound_variable_no_error() {
    // Test that pattern-bound variables do NOT produce diagnostics
    // value match: [x -> x]
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "value",
            test_span(),
        ))),
        arms: vec![MatchArm::new(
            Pattern::Variable(Identifier::new("x", test_span())),
            Expression::Identifier(Identifier::new("x", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should only have diagnostic for 'value', not 'x'
    assert_eq!(result.diagnostics.len(), 1);
    assert!(
        result.diagnostics[0]
            .message
            .contains("Undefined variable: value")
    );
}

#[test]
fn test_nested_pattern_variables_accessible() {
    // Test nested tuple pattern variables are accessible
    // result match: [{#ok, {x, y}} -> x]
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new(
            "result",
            test_span(),
        ))),
        arms: vec![MatchArm::new(
            Pattern::Tuple {
                elements: vec![
                    Pattern::Literal(Literal::Symbol("ok".into()), test_span()),
                    Pattern::Tuple {
                        elements: vec![
                            Pattern::Variable(Identifier::new("x", test_span())),
                            Pattern::Variable(Identifier::new("y", test_span())),
                        ],
                        span: test_span(),
                    },
                ],
                span: test_span(),
            },
            Expression::Identifier(Identifier::new("x", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let module = Module::new(vec![bare(match_expr)], test_span());
    let result = analyse(&module);

    // Should only error on 'result', not 'x' or 'y'
    assert_eq!(result.diagnostics.len(), 1);
    assert!(
        result.diagnostics[0]
            .message
            .contains("Undefined variable: result")
    );
}
