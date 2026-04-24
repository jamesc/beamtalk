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
