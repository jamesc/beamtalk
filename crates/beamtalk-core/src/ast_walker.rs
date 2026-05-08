// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared AST expression walker for lint and validator passes.
//!
//! **DDD Context:** Compilation
//!
//! Provides three functions used by lint and validator passes:
//!
//! - [`for_each_expr_seq`] — iterate every top-level statement sequence in a module
//!   (module-level expressions, method bodies, standalone method bodies).
//!
//! - [`walk_expression`] — pre-order recursive walk of a single expression tree,
//!   calling a visitor closure on every node.
//!
//! - [`walk_module`] — convenience: pre-order walk of **all** expressions in a module.
//!
//! # Why this exists
//!
//! Before this module, every lint pass and many validator checks had their own
//! hand-rolled traversal. The module-level loop appeared 7+ times identically,
//! and the expression match had 15+ arms duplicated 5+ times. This module
//! eliminates that duplication.
//!
//! # What is NOT handled here
//!
//! Passes with state that must be threaded through the traversal (e.g. scope
//! tracking in `shadowed_block_param`) or passes that need sequence-level
//! awareness (e.g. `cascade_candidate`) keep their own recursive traversal.
//! This module handles the common pre-order-visitor pattern.

use crate::ast::{Expression, ExpressionStatement, Module, StringSegment};

// ── Module-level iterators ────────────────────────────────────────────────────

/// Calls `f` once for each top-level statement sequence in the module.
///
/// The three sequences are:
/// - The module-level expression list (`module.expressions`)
/// - Every method body (both instance and class methods)
/// - Every standalone method definition body
///
/// Block bodies are **not** included — they are nested inside expression trees,
/// not top-level statement sequences.
pub(crate) fn for_each_expr_seq<F>(module: &Module, mut f: F)
where
    F: FnMut(&[ExpressionStatement]),
{
    f(&module.expressions);
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            f(&method.body);
        }
    }
    for standalone in &module.method_definitions {
        f(&standalone.method.body);
    }
}

// ── Expression walker ─────────────────────────────────────────────────────────

/// Recursively walks an expression tree in pre-order, calling `f` on every node.
///
/// The visitor is called on the current node **before** recursing into its children.
/// All nineteen `Expression` variants are handled — including `ArrayLiteral`,
/// which was missing from several hand-rolled walkers.
pub(crate) fn walk_expression<F>(expr: &Expression, f: &mut F)
where
    F: FnMut(&Expression),
{
    f(expr);
    match expr {
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            walk_expression(receiver, f);
            for arg in arguments {
                walk_expression(arg, f);
            }
        }
        Expression::Block(block) => {
            for stmt in &block.body {
                walk_expression(&stmt.expression, f);
            }
        }
        Expression::Assignment { target, value, .. } => {
            walk_expression(target, f);
            walk_expression(value, f);
        }
        Expression::Return { value, .. } | Expression::DestructureAssignment { value, .. } => {
            walk_expression(value, f);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            walk_expression(receiver, f);
            for msg in messages {
                for arg in &msg.arguments {
                    walk_expression(arg, f);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => {
            walk_expression(expression, f);
        }
        Expression::FieldAccess { receiver, .. } => {
            walk_expression(receiver, f);
        }
        Expression::Match { value, arms, .. } => {
            walk_expression(value, f);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_expression(guard, f);
                }
                walk_expression(&arm.body, f);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_expression(&pair.key, f);
                walk_expression(&pair.value, f);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                walk_expression(elem, f);
            }
            if let Some(t) = tail {
                walk_expression(t, f);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                walk_expression(elem, f);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let StringSegment::Interpolation(e) = seg {
                    walk_expression(e, f);
                }
            }
        }
        // Leaf nodes — nothing to recurse into.
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Error { .. }
        | Expression::Spread { .. } => {}
    }
}

/// Walks all expressions in every statement sequence of a module (pre-order).
///
/// Equivalent to calling `walk_expression` on every expression in every
/// sequence yielded by [`for_each_expr_seq`]. Use this when a pass needs
/// to visit every expression node in the module without any sequence-level
/// awareness.
pub(crate) fn walk_module<F>(module: &Module, f: &mut F)
where
    F: FnMut(&Expression),
{
    for_each_expr_seq(module, |seq| {
        for stmt in seq {
            walk_expression(&stmt.expression, f);
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_support::parse_bt;

    /// Returns the first module-level expression from `source`.
    ///
    /// Panics if the module has no top-level expressions.
    fn first_module_expr(source: &str) -> Expression {
        let module = parse_bt(source);
        module
            .expressions
            .first()
            .expect("expected at least one module-level expression")
            .expression
            .clone()
    }

    /// Counts how many nodes `walk_expression` visits when walking `expr`.
    fn count_visits(expr: &Expression) -> usize {
        let mut count = 0;
        walk_expression(expr, &mut |_| count += 1);
        count
    }

    /// Counts how many times `for_each_expr_seq` invokes its callback,
    /// and the total number of statements summed across all sequences.
    fn for_each_tally(module: &Module) -> (usize, usize) {
        let mut calls = 0;
        let mut stmts = 0;
        for_each_expr_seq(module, |seq| {
            calls += 1;
            stmts += seq.len();
        });
        (calls, stmts)
    }

    // ── for_each_expr_seq ────────────────────────────────────────────────

    #[test]
    fn for_each_visits_module_expressions_once() {
        let module = parse_bt("1\n2\n3\n");
        let (calls, stmts) = for_each_tally(&module);
        assert_eq!(calls, 1, "only module.expressions should produce a call");
        assert_eq!(stmts, 3, "all three module-level statements should be seen");
    }

    #[test]
    fn for_each_visits_empty_module_once() {
        let module = parse_bt("Object subclass: Empty\n");
        let (calls, stmts) = for_each_tally(&module);
        assert_eq!(
            calls, 1,
            "module.expressions is always visited (even if empty)"
        );
        assert_eq!(stmts, 0);
    }

    #[test]
    fn for_each_visits_each_instance_method_body() {
        let module = parse_bt("Object subclass: C\n  one => 1\n  two => 2\n");
        let (calls, _) = for_each_tally(&module);
        assert_eq!(calls, 3, "1 module + 2 instance methods");
    }

    #[test]
    fn for_each_visits_each_class_method_body() {
        let module = parse_bt("Object subclass: C\n  class makeOne => 1\n  class makeTwo => 2\n");
        let (calls, _) = for_each_tally(&module);
        assert_eq!(calls, 3, "1 module + 2 class methods");
    }

    #[test]
    fn for_each_visits_standalone_method_definitions() {
        let module = parse_bt("String >> shout => self\nString >> hush => self\n");
        let (calls, _) = for_each_tally(&module);
        assert_eq!(calls, 3, "1 module + 2 standalone methods");
    }

    #[test]
    fn for_each_visits_all_sequence_kinds() {
        let module = parse_bt(
            "42\n\
             Object subclass: C\n  \
                 inst => 1\n  \
                 class cls => 2\n\
             String >> ext => self\n",
        );
        let (calls, stmts) = for_each_tally(&module);
        // 1 (module) + 1 instance + 1 class + 1 standalone = 4 callbacks.
        assert_eq!(calls, 4);
        // 1 module-level stmt + 3 single-stmt method bodies = 4 stmts.
        assert_eq!(stmts, 4);
    }

    // ── walk_expression: leaf nodes ─────────────────────────────────────

    #[test]
    fn walk_visits_literal_once() {
        let expr = first_module_expr("42\n");
        assert!(matches!(expr, Expression::Literal(..)));
        assert_eq!(count_visits(&expr), 1);
    }

    #[test]
    fn walk_visits_identifier_once() {
        let expr = first_module_expr("x := 1\nx\n");
        let module = parse_bt("x := 1\nx\n");
        let ident = module.expressions[1].expression.clone();
        assert!(matches!(ident, Expression::Identifier(..)));
        assert_eq!(count_visits(&ident), 1);
        // Also covers the assignment branch using the originally captured expr:
        assert!(matches!(expr, Expression::Assignment { .. }));
    }

    // ── walk_expression: recursive branches ─────────────────────────────

    #[test]
    fn walk_message_send_visits_receiver_and_arguments() {
        // `1 + 2` → MessageSend with receiver=1 and one argument=2 → 3 visits.
        let expr = first_module_expr("1 + 2\n");
        assert!(matches!(expr, Expression::MessageSend { .. }));
        assert_eq!(count_visits(&expr), 3);
    }

    #[test]
    fn walk_block_visits_body_statements() {
        // Block `[1. 2]` has two body statements → 1 (Block) + 2 leaves = 3.
        let expr = first_module_expr("[1. 2]\n");
        assert!(matches!(expr, Expression::Block(_)));
        assert_eq!(count_visits(&expr), 3);
    }

    #[test]
    fn walk_assignment_visits_target_and_value() {
        // `x := 1` → Assignment + target Identifier + value Literal = 3.
        let expr = first_module_expr("x := 1\n");
        assert!(matches!(expr, Expression::Assignment { .. }));
        assert_eq!(count_visits(&expr), 3);
    }

    #[test]
    fn walk_destructure_assignment_visits_value() {
        // `{a, b} := someTuple` → DestructureAssignment + value Identifier = 2.
        // (The pattern is not an Expression and is not walked.)
        let expr = first_module_expr("{a, b} := someTuple\n");
        assert!(matches!(expr, Expression::DestructureAssignment { .. }));
        assert_eq!(count_visits(&expr), 2);
    }

    #[test]
    fn walk_return_visits_value() {
        // `^42` only parses inside a method body.
        let module = parse_bt("Object subclass: C\n  m => ^42\n");
        let body = &module.classes[0].methods[0].body;
        let expr = body[0].expression.clone();
        assert!(matches!(expr, Expression::Return { .. }));
        assert_eq!(count_visits(&expr), 2);
    }

    #[test]
    fn walk_cascade_visits_receiver_and_message_arguments() {
        // The parser stores the first message as part of the cascade receiver
        // (a MessageSend), and the remaining cascaded messages in `messages`.
        // For `obj foo: 1; bar: 2`:
        //   - Cascade node                              (1 visit)
        //   - receiver = `obj foo: 1` MessageSend      → 3 visits
        //       (MessageSend itself, obj, literal 1)
        //   - messages = [`bar: 2`] → arg literal `2`   (1 visit)
        // Total = 5.
        let expr = first_module_expr("obj foo: 1; bar: 2\n");
        assert!(matches!(expr, Expression::Cascade { .. }));
        assert_eq!(count_visits(&expr), 5);
    }

    #[test]
    fn walk_parenthesized_visits_inner() {
        // `(42)` → Parenthesized + Literal = 2.
        let expr = first_module_expr("(42)\n");
        assert!(matches!(expr, Expression::Parenthesized { .. }));
        assert_eq!(count_visits(&expr), 2);
    }

    #[test]
    fn walk_field_access_visits_receiver() {
        // `self.x` → FieldAccess + receiver Identifier = 2. Only valid inside a method.
        let module = parse_bt("Object subclass: C\n  state: x = 0\n  m => self.x\n");
        let expr = module.classes[0].methods[0].body[0].expression.clone();
        assert!(matches!(expr, Expression::FieldAccess { .. }));
        assert_eq!(count_visits(&expr), 2);
    }

    #[test]
    fn walk_match_without_guard_visits_value_and_arm_body() {
        // `x match: [_ -> 1]` → Match + value(x) + arm body(1) = 3.
        let expr = first_module_expr("x match: [_ -> 1]\n");
        assert!(matches!(expr, Expression::Match { .. }));
        assert_eq!(count_visits(&expr), 3);
    }

    #[test]
    fn walk_match_with_guard_visits_guard_and_body() {
        // The parser stores the guard as the *inner* expression of the
        // `when: [...]` block, not the surrounding block itself. For
        // `x match: [_ when: [true] -> 1]`:
        //   - Match node     (1)
        //   - value `x`      (1)
        //   - guard `true`   (1, literal — block wrapping is stripped)
        //   - body `1`       (1)
        // Total = 4.
        let expr = first_module_expr("x match: [_ when: [true] -> 1]\n");
        assert!(matches!(expr, Expression::Match { .. }));
        assert_eq!(count_visits(&expr), 4);
    }

    #[test]
    fn walk_map_literal_visits_keys_and_values() {
        // `#{#a => 1, #b => 2}` → MapLiteral + 2 keys + 2 values = 5.
        let expr = first_module_expr("#{#a => 1, #b => 2}\n");
        assert!(matches!(expr, Expression::MapLiteral { .. }));
        assert_eq!(count_visits(&expr), 5);
    }

    #[test]
    fn walk_list_literal_visits_elements() {
        // `#(1, 2, 3)` → ListLiteral + 3 elements = 4. No tail.
        let expr = first_module_expr("#(1, 2, 3)\n");
        let tail_is_none = matches!(&expr, Expression::ListLiteral { tail: None, .. });
        assert!(tail_is_none);
        assert_eq!(count_visits(&expr), 4);
    }

    #[test]
    fn walk_list_literal_with_tail_visits_tail() {
        // `#(1 | rest)` → ListLiteral + element(1) + tail(rest) = 3.
        let expr = first_module_expr("#(1 | rest)\n");
        let tail_is_some = matches!(&expr, Expression::ListLiteral { tail: Some(_), .. });
        assert!(tail_is_some);
        assert_eq!(count_visits(&expr), 3);
    }

    #[test]
    fn walk_array_literal_visits_elements() {
        // `#[1, 2, 3]` → ArrayLiteral + 3 elements = 4.
        let expr = first_module_expr("#[1, 2, 3]\n");
        assert!(matches!(expr, Expression::ArrayLiteral { .. }));
        assert_eq!(count_visits(&expr), 4);
    }

    #[test]
    fn walk_string_interpolation_visits_only_interpolated_segments() {
        // `"hi {name}!"` → StringInterpolation + interpolated identifier(name) = 2.
        // Literal text segments are not Expression nodes and are not visited.
        let expr = first_module_expr("\"hi {name}!\"\n");
        assert!(matches!(expr, Expression::StringInterpolation { .. }));
        assert_eq!(count_visits(&expr), 2);
    }

    // ── walk_module ─────────────────────────────────────────────────────

    #[test]
    fn walk_module_visits_expressions_across_all_sequences() {
        // 1 module-level literal (1 visit)
        // + instance method body `1 + 2` (3 visits)
        // + class method body `99` (1 visit)
        // + standalone method body `self` (1 visit, identifier)
        // = 6 visits total.
        let module = parse_bt(
            "42\n\
             Object subclass: C\n  \
                 add => 1 + 2\n  \
                 class makeIt => 99\n\
             String >> ext => self\n",
        );
        let mut count = 0;
        walk_module(&module, &mut |_| count += 1);
        assert_eq!(count, 6);
    }
}
