// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! AST visitors used by narrowing rules.
//!
//! These helpers answer structural questions about blocks and expressions that
//! multiple rules need:
//!
//! * [`block_has_return`] — top-level `^` only (used by BT-2049's guard-block
//!   divergence check, where nested sub-expressions don't count).
//! * [`block_has_any_return`] — `^` anywhere, including nested sub-expressions
//!   but **not** inside nested `Block` literals. A nested `[^1]` that is not
//!   `value`-sent is just a block object that never runs, so it should stay
//!   opaque (BT-2051). The `Block` arm of [`expr_contains_return`] preserves
//!   this by recursing into `block_has_any_return`, which in turn only inspects
//!   its own statements.
//! * [`block_may_reassign`] — top-level assignments to `var_name`. Used by
//!   BT-2049's soundness guard for `ifTrue:ifFalse:` so that reassigning the
//!   tested variable in the false branch cancels post-guard narrowing.
//!
//! These are kept as siblings (rather than one unified visitor) because the
//! "opaque-nested-block" rule of [`block_has_any_return`] and the "top-level
//! only" rule of [`block_has_return`] / [`block_may_reassign`] are subtly
//! different — collapsing them would make it too easy to reintroduce
//! BT-2049/BT-2051 regressions.
//!
//! Extracted from `inference.rs` under BT-2050.

use crate::ast::{Block, Expression};
use crate::semantic_analysis::type_checker::EnvKey;

use super::extract::extract_variable_name;

/// Does `block`'s body contain a top-level `^` return statement?
///
/// Only inspects direct children — does **not** recurse into sub-expressions
/// or nested blocks. Callers that need the deeper check use
/// [`block_has_any_return`].
pub(crate) fn block_has_return(block: &Block) -> bool {
    block
        .body
        .iter()
        .any(|stmt| matches!(stmt.expression, Expression::Return { .. }))
}

/// Does `block`'s body contain a `^` anywhere — including buried inside
/// arguments, cascades, or assignments — but **not** inside nested `Block`
/// literals that are never invoked?
///
/// BT-2047 uses this to detect branches that exit the enclosing method even
/// when the `^` is wrapped in a sub-expression like `[[^1] value]` or
/// `[foo: (^bar)]`.
pub(crate) fn block_has_any_return(block: &Block) -> bool {
    block
        .body
        .iter()
        .any(|stmt| expr_contains_return(&stmt.expression))
}

/// Does `expr` contain a `^` anywhere?
///
/// Recurses through parenthesization, assignments, message sends, and cascades.
/// **Nested block literals are opaque**: `[[^1] value]` — or any branch body
/// that constructs a block containing `^` without invoking it — does NOT
/// count as diverging. The inner block is a value, not control flow, so
/// treating it as a method-exit would make `apply_early_return_narrowing`
/// (and BT-2047's `if_nil_branch_union_ret_ty`) unsoundly narrow after
/// branches that can still fall through. Mirrors the same opacity rule in
/// [`expr_contains_never`] (BT-2051).
pub(crate) fn expr_contains_return(expr: &Expression) -> bool {
    match expr {
        Expression::Return { .. } => true,
        Expression::Parenthesized { expression, .. } => expr_contains_return(expression),
        Expression::Assignment { target, value, .. } => {
            expr_contains_return(target) || expr_contains_return(value)
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => expr_contains_return(receiver) || arguments.iter().any(expr_contains_return),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            expr_contains_return(receiver)
                || messages
                    .iter()
                    .any(|m| m.arguments.iter().any(expr_contains_return))
        }
        // - `Block` literal: opaque — an inert block value never runs here.
        // - Literals, identifiers, class references, field access, etc.
        //   have no sub-expressions that could contain `^`.
        _ => false,
    }
}

/// Conservative scan: does `block` contain an assignment whose target is the
/// same binding as `key`?
///
/// `key` may be a lexical local or a synthetic `self.<field>` key
/// (BT-2048 / BT-2062). Only inspects top-level statements in the block,
/// which matches the shapes post-guard narrowing currently reasons about.
/// False positives are safe (we skip narrowing); false negatives would be
/// unsound, so anything non-trivial defaults to "assume reassignment".
pub(crate) fn block_may_reassign(block: &Block, key: &EnvKey) -> bool {
    block.body.iter().any(|stmt| {
        if let Expression::Assignment { target, .. } = &stmt.expression {
            extract_variable_name(target).is_some_and(|n| &n == key)
        } else {
            false
        }
    })
}
