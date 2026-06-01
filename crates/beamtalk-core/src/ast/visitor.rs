// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generic AST visitor with **opaque-block** default recursion (BT-2063).
//!
//! This visitor exists for the type-checker's narrowing machinery, where an
//! inert [`Block`] literal is a *value*, not a point of control flow.
//! `[self error: "…"]` constructs a block but does not execute it, so walkers
//! that answer "does this statement diverge?" or "does this branch contain
//! `^`?" must not descend into nested blocks by default — doing so would
//! unsoundly treat a captured-but-never-invoked block as if its body ran.
//!
//! # Relationship to [`crate::ast_walker`]
//!
//! [`crate::ast_walker::walk_expression`] also walks every variant of
//! [`Expression`], but serves a different audience: lint and validator
//! passes that *do* want to see every expression, including nested block
//! bodies. Both walkers are exhaustive over variants (so adding a new
//! [`Expression`] variant is a compile error at the match, preventing the
//! "silent-miss" bug that motivated this trait), but they differ in whether
//! they descend into [`Block`]:
//!
//! | Walker | Block default | Audience |
//! | -- | -- | -- |
//! | [`crate::ast_walker::walk_expression`] | descend | lint / validators / codegen |
//! | [`Visitor`] (this module) | **opaque** | narrowing / divergence / `^` detection |
//!
//! # Usage
//!
//! Implement [`Visitor::visit_expr`] to do the per-node work. Call
//! [`walk_expr`] to recurse into sub-expressions. Override
//! [`Visitor::visit_block`] to opt into descending into block literals.
//!
//! ```ignore
//! struct ContainsReturn(bool);
//! impl<'ast> Visitor<'ast> for ContainsReturn {
//!     fn visit_expr(&mut self, e: &'ast Expression) {
//!         if self.0 { return }
//!         if matches!(e, Expression::Return { .. }) { self.0 = true; return }
//!         walk_expr(self, e);
//!     }
//!     // visit_block default (opaque) is correct here.
//! }
//! ```

use super::{Block, Expression, StringSegment};

/// Pre-order visitor over [`Expression`] trees with opaque nested-block
/// default. See the module-level documentation for the design rationale.
pub(crate) trait Visitor<'ast>: Sized {
    /// Called on every expression node encountered during the walk.
    ///
    /// The default implementation simply recurses into sub-expressions via
    /// [`walk_expr`]. Override to inspect the node before/after recursion or
    /// to short-circuit traversal.
    fn visit_expr(&mut self, expr: &'ast Expression) {
        walk_expr(self, expr);
    }

    /// Called when a nested [`Expression::Block`] literal is encountered.
    ///
    /// **Default: opaque** — the block's body is not visited. This matches
    /// the narrowing semantics (BT-2050 / BT-2051): an inert block literal
    /// is a value construction, not a point of control flow, so walkers
    /// answering "does this diverge?" / "does this return?" must not descend.
    ///
    /// Visitors that need to see inside blocks override this method to call
    /// [`walk_block`], which iterates the block body.
    fn visit_block(&mut self, _block: &'ast Block) {}
}

/// Exhaustively recurse into every sub-expression of `expr`.
///
/// Leaf variants (literals, identifiers, class references, `super`,
/// primitives, expect directives, spread, error nodes) have no children, so
/// this is a no-op for them. [`Expression::Block`] is delegated to
/// [`Visitor::visit_block`] — **not** recursed into here — preserving the
/// opaque-block default.
///
/// This match is deliberately **not** a catch-all: every variant must appear
/// explicitly, so that adding a new [`Expression`] variant in the future is
/// a compile error at this site (see BT-2063 acceptance criteria).
pub(crate) fn walk_expr<'ast, V: Visitor<'ast>>(v: &mut V, expr: &'ast Expression) {
    match expr {
        Expression::FieldAccess { receiver, .. } => {
            v.visit_expr(receiver);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            v.visit_expr(receiver);
            for arg in arguments {
                v.visit_expr(arg);
            }
        }
        Expression::Block(block) => {
            v.visit_block(block);
        }
        Expression::Assignment { target, value, .. } => {
            v.visit_expr(target);
            v.visit_expr(value);
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            v.visit_expr(value);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            v.visit_expr(receiver);
            for msg in messages {
                for arg in &msg.arguments {
                    v.visit_expr(arg);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => {
            v.visit_expr(expression);
        }
        Expression::Match { value, arms, .. } => {
            v.visit_expr(value);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    v.visit_expr(guard);
                }
                v.visit_expr(&arm.body);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                v.visit_expr(&pair.key);
                v.visit_expr(&pair.value);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                v.visit_expr(elem);
            }
            if let Some(t) = tail {
                v.visit_expr(t);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                v.visit_expr(elem);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let StringSegment::Interpolation(e) = seg {
                    v.visit_expr(e);
                }
            }
        }
        // Leaf nodes — nothing to recurse into. Listed explicitly (no `_`
        // arm) so that adding a new variant is a compile error here.
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

/// Visit every top-level statement in `block`.
///
/// Call this from an overridden [`Visitor::visit_block`] to opt into
/// descending through a nested block literal.
///
/// Currently only used by test helpers (e.g. `find_send_inferred_ty`), so
/// permitted to be dead code in non-test builds. This is part of the
/// trait's documented API — don't gate it behind `#[cfg(test)]`.
#[allow(dead_code)]
pub(crate) fn walk_block<'ast, V: Visitor<'ast>>(v: &mut V, block: &'ast Block) {
    for stmt in &block.body {
        v.visit_expr(&stmt.expression);
    }
}

#[cfg(test)]
mod tests {
    use super::{Block, Expression, Visitor, walk_block, walk_expr};
    use crate::test_helpers::test_support::parse_bt;

    /// Visitor that counts every `visit_expr` call and recurses into children
    /// via `walk_expr`. Blocks are **opaque** (default `visit_block` is a no-op).
    struct Counter(usize);

    impl<'ast> Visitor<'ast> for Counter {
        fn visit_expr(&mut self, expr: &'ast Expression) {
            self.0 += 1;
            walk_expr(self, expr);
        }
    }

    /// Like `Counter` but overrides `visit_block` to call `walk_block`,
    /// descending into nested block bodies.
    struct BlockDescendingCounter(usize);

    impl<'ast> Visitor<'ast> for BlockDescendingCounter {
        fn visit_expr(&mut self, expr: &'ast Expression) {
            self.0 += 1;
            walk_expr(self, expr);
        }
        fn visit_block(&mut self, block: &'ast Block) {
            walk_block(self, block);
        }
    }

    fn first_module_expr(source: &str) -> Expression {
        let module = parse_bt(source);
        module
            .expressions
            .first()
            .expect("at least one module-level expression")
            .expression
            .clone()
    }

    fn count_visits(expr: &Expression) -> usize {
        let mut c = Counter(0);
        c.visit_expr(expr);
        c.0
    }

    fn count_with_block_descent(expr: &Expression) -> usize {
        let mut c = BlockDescendingCounter(0);
        c.visit_expr(expr);
        c.0
    }

    // ── Opaque-block default ─────────────────────────────────────────────

    #[test]
    fn default_visitor_block_is_opaque() {
        // Core semantic: the default visitor must NOT descend into block bodies.
        // Only the Block node itself is counted.
        let expr = first_module_expr("[42]\n");
        assert!(matches!(expr, Expression::Block(_)));
        assert_eq!(
            count_visits(&expr),
            1,
            "block body must not be visited by default"
        );
    }

    // ── walk_block ────────────────────────────────────────────────────────

    #[test]
    fn walk_block_descends_into_body_when_visit_block_overridden() {
        // `[1. 2. 3]`: Block (1) + 3 body literals = 4 when visit_block overridden.
        let expr = first_module_expr("[1. 2. 3]\n");
        assert!(matches!(expr, Expression::Block(_)));
        assert_eq!(count_with_block_descent(&expr), 4);
    }

    // ── walk_expr: FieldAccess ────────────────────────────────────────────

    #[test]
    fn walk_field_access_visits_receiver() {
        // `self.x` — only valid inside a method with a state field.
        // FieldAccess (1) + receiver (1) = 2.
        let module = parse_bt("Object subclass: C\n  state: x = 0\n  m => self.x\n");
        let expr = module.classes[0].methods[0].body[0].expression.clone();
        assert!(matches!(expr, Expression::FieldAccess { .. }));
        assert_eq!(count_visits(&expr), 2);
    }

    // ── walk_expr: Assignment ─────────────────────────────────────────────

    #[test]
    fn walk_assignment_visits_target_and_value() {
        // `x := 42` → Assignment (1) + target Identifier (1) + value Literal (1) = 3.
        let expr = first_module_expr("x := 42\n");
        assert!(matches!(expr, Expression::Assignment { .. }));
        assert_eq!(count_visits(&expr), 3);
    }

    // ── walk_expr: Cascade inner-messages loop ────────────────────────────

    #[test]
    fn walk_cascade_visits_cascade_message_arguments() {
        // `obj foo: 1; bar: 2`
        // Cascade (1) + receiver MessageSend(obj foo: 1): send(1)+obj(1)+1(1)
        // + cascade-message arg 2 (1) = 5.
        let expr = first_module_expr("obj foo: 1; bar: 2\n");
        assert!(matches!(expr, Expression::Cascade { .. }));
        assert_eq!(count_visits(&expr), 5);
    }

    // ── walk_expr: Parenthesized ──────────────────────────────────────────

    #[test]
    fn walk_parenthesized_visits_inner_expression() {
        // `(42)` → Parenthesized (1) + inner Literal (1) = 2.
        let expr = first_module_expr("(42)\n");
        assert!(matches!(expr, Expression::Parenthesized { .. }));
        assert_eq!(count_visits(&expr), 2);
    }

    // ── walk_expr: Match ──────────────────────────────────────────────────

    #[test]
    fn walk_match_visits_value_and_arm_body() {
        // `x match: [_ -> 1]` → Match (1) + value x (1) + arm body 1 (1) = 3.
        let expr = first_module_expr("x match: [_ -> 1]\n");
        assert!(matches!(expr, Expression::Match { .. }));
        assert_eq!(count_visits(&expr), 3);
    }

    #[test]
    fn walk_match_with_guard_visits_guard_expression() {
        // Guard is stored as the inner expression of the `when: [...]` block.
        // Match (1) + value x (1) + guard true (1) + body 1 (1) = 4.
        let expr = first_module_expr("x match: [_ when: [true] -> 1]\n");
        assert!(matches!(expr, Expression::Match { .. }));
        assert_eq!(count_visits(&expr), 4);
    }

    // ── walk_expr: MapLiteral ─────────────────────────────────────────────

    #[test]
    fn walk_map_literal_visits_keys_and_values() {
        // `#{#a => 1, #b => 2}` → MapLiteral (1) + 2 keys + 2 values = 5.
        let expr = first_module_expr("#{#a => 1, #b => 2}\n");
        assert!(matches!(expr, Expression::MapLiteral { .. }));
        assert_eq!(count_visits(&expr), 5);
    }

    // ── walk_expr: ListLiteral tail branch ────────────────────────────────

    #[test]
    fn walk_list_literal_with_tail_visits_tail() {
        // `#(1 | rest)` → ListLiteral (1) + element 1 (1) + tail rest (1) = 3.
        let expr = first_module_expr("#(1 | rest)\n");
        assert!(matches!(
            &expr,
            Expression::ListLiteral { tail: Some(_), .. }
        ));
        assert_eq!(count_visits(&expr), 3);
    }

    // ── walk_expr: ArrayLiteral ───────────────────────────────────────────

    #[test]
    fn walk_array_literal_visits_all_elements() {
        // `#[1, 2, 3]` → ArrayLiteral (1) + 3 Literal elements = 4.
        let expr = first_module_expr("#[1, 2, 3]\n");
        assert!(matches!(expr, Expression::ArrayLiteral { .. }));
        assert_eq!(count_visits(&expr), 4);
    }

    // ── walk_expr: StringInterpolation ────────────────────────────────────

    #[test]
    fn walk_string_interpolation_visits_only_interpolated_segments() {
        // `"hi {name}!"` → StringInterpolation (1) + identifier name (1) = 2.
        // Literal text segments are not Expression nodes and are not visited.
        let expr = first_module_expr("\"hi {name}!\"\n");
        assert!(matches!(expr, Expression::StringInterpolation { .. }));
        assert_eq!(count_visits(&expr), 2);
    }
}
