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

use crate::ast::{Expression, Module, StringSegment};

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
    F: FnMut(&[Expression]),
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
            for e in &block.body {
                walk_expression(e, f);
            }
        }
        Expression::Assignment { target, value, .. } => {
            walk_expression(target, f);
            walk_expression(value, f);
        }
        Expression::Return { value, .. } => {
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
        | Expression::Error { .. } => {}
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
        for expr in seq {
            walk_expression(expr, f);
        }
    });
}
