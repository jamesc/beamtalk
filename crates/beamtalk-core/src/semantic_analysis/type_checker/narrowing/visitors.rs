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
//!   opaque (BT-2051). Implemented on top of the shared [`crate::ast::visitor`]
//!   trait, whose default `visit_block` is opaque.
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
//! Extracted from `inference.rs` under BT-2050. Re-expressed as
//! [`crate::ast::visitor::Visitor`] impls under BT-2063 so that the
//! previously-hand-rolled structural match is exhaustive over
//! [`Expression`] variants (the same bug — "your walker doesn't cover variant
//! X" — surfaced multiple times during the BT-2044 epic).
//!
//! `block_has_return` and `block_may_reassign` are left as simple top-level
//! iterations: they intentionally only inspect direct block children (not
//! nested expressions) so the [`Visitor`] trait's structural recursion
//! wouldn't be in scope anyway.

use crate::ast::visitor::{Visitor, walk_expr};
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
/// Recurses through every [`Expression`] variant with sub-expressions via the
/// shared [`crate::ast::visitor`] trait. **Nested block literals are opaque**:
/// `[[^1] value]` — or any branch body that constructs a block containing `^`
/// without invoking it — does NOT count as diverging. The inner block is a
/// value, not control flow, so treating it as a method-exit would make
/// `apply_early_return_narrowing` (and BT-2047's `if_nil_branch_union_ret_ty`)
/// unsoundly narrow after branches that can still fall through. Mirrors the
/// same opacity rule in [`crate::semantic_analysis::type_checker::TypeChecker`]'s
/// `expr_contains_never` (BT-2051).
pub(crate) fn expr_contains_return(expr: &Expression) -> bool {
    struct Finder(bool);
    impl<'ast> Visitor<'ast> for Finder {
        fn visit_expr(&mut self, e: &'ast Expression) {
            if self.0 {
                return;
            }
            if matches!(e, Expression::Return { .. }) {
                self.0 = true;
                return;
            }
            walk_expr(self, e);
        }
        // `visit_block` default is opaque, which is what we want here:
        // a nested `[...^...]` literal that is never invoked does not
        // cause the enclosing expression to diverge.
    }
    let mut finder = Finder(false);
    finder.visit_expr(expr);
    finder.0
}

/// Conservative scan: does `block` contain an assignment whose target is the
/// same binding as `key`?
///
/// `key` may be a lexical local or a synthetic `self.<field>` key
/// (BT-2048 / BT-2062). Only inspects **top-level** statements in the block,
/// which matches the shapes post-guard narrowing currently reasons about.
/// False positives are safe (we skip narrowing); false negatives would be
/// unsound, so anything non-trivial defaults to "assume reassignment".
///
/// BT-2063 note: this intentionally does NOT use [`crate::ast::visitor`].
/// The structural recursion is a non-goal here: we only want to see
/// reassignments at the statement level of the block body (mirrored in how
/// the narrowing machinery observes linear statement-order effects).
/// Recursing into sub-expressions would flip this from a "top-level scan"
/// into a "deep search", changing the soundness story for post-guard
/// narrowing. If a deeper scan is ever wanted, it should be a new function.
pub(crate) fn block_may_reassign(block: &Block, key: &EnvKey) -> bool {
    block.body.iter().any(|stmt| {
        if let Expression::Assignment { target, .. } = &stmt.expression {
            extract_variable_name(target).is_some_and(|n| &n == key)
        } else {
            false
        }
    })
}
