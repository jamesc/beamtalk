// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! AST visitors used by narrowing rules.
//!
//! These helpers answer structural questions about blocks and expressions that
//! multiple rules need:
//!
//! * [`block_has_return`] — top-level `^` only (used by the guard-block
//!   divergence check, where nested sub-expressions don't count).
//! * [`block_has_any_return`] — `^` anywhere, including nested sub-expressions
//!   but **not** inside nested `Block` literals. A nested `[^1]` that is not
//!   `value`-sent is just a block object that never runs, so it should stay
//!   opaque. Implemented on top of the shared [`crate::ast::visitor`]
//!   trait, whose default `visit_block` is opaque.
//! * [`block_may_reassign`] — top-level assignments to `var_name`. Used by
//!   the soundness guard for `ifTrue:ifFalse:` so that reassigning the
//!   tested variable in the false branch cancels post-guard narrowing.
//!
//! These are kept as siblings (rather than one unified visitor) because the
//! "opaque-nested-block" rule of [`block_has_any_return`] and the "top-level
//! only" rule of [`block_has_return`] / [`block_may_reassign`] are subtly
//! different — collapsing them would make it too easy to reintroduce
//! regressions in either soundness rule.
//!
//! Extracted from `inference.rs`. Re-expressed as
//! [`crate::ast::visitor::Visitor`] impls so that the
//! previously-hand-rolled structural match is exhaustive over
//! [`Expression`] variants (the same bug — "your walker doesn't cover variant
//! X" — surfaced multiple times historically).
//!
//! `block_has_return` and `block_may_reassign` are left as simple top-level
//! iterations: they intentionally only inspect direct block children (not
//! nested expressions) so the [`Visitor`] trait's structural recursion
//! wouldn't be in scope anyway.

use crate::ast::visitor::{Visitor, walk_block, walk_expr};
use crate::ast::{Block, Expression};
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::semantic_analysis::type_checker::types::is_generic_type_param;
use crate::semantic_analysis::type_checker::{EnvKey, InferredType, TypeMap};

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
/// Used to detect branches that exit the enclosing method even
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
/// `apply_early_return_narrowing` (and `if_nil_branch_union_ret_ty`)
/// unsoundly narrow after branches that can still fall through. Mirrors the
/// same opacity rule in [`crate::semantic_analysis::type_checker::TypeChecker`]'s
/// `expr_contains_never`.
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

/// Does `expr` contain a message send to `self`/`super` that can return,
/// anywhere, **including inside nested block literals**?
///
/// Unlike [`expr_contains_return`] a nested block is not opaque: a block that
/// is built here may run before the code that follows, so a self-send inside
/// it counts. A send the checker already typed `Never` (`self error: "…"`)
/// does not count: it never hands control back, so it cannot have
/// reassigned a field the code after it reads. Used to decide whether a
/// `self.<field>` narrowing proved by a guard can still be trusted after the
/// guard statement (BT-4715), so `expr` must already have been inferred.
pub(crate) fn expr_contains_returning_self_send(expr: &Expression, type_map: &TypeMap) -> bool {
    struct Finder<'m>(bool, &'m TypeMap);
    impl<'ast> Visitor<'ast> for Finder<'_> {
        fn visit_expr(&mut self, e: &'ast Expression) {
            if self.0 {
                return;
            }
            if let Expression::MessageSend { receiver, .. } = e {
                let to_self = matches!(receiver.as_ref(), Expression::Super(_))
                    || matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self");
                if to_self && !matches!(self.1.get(e.span()), Some(InferredType::Never)) {
                    self.0 = true;
                    return;
                }
            }
            walk_expr(self, e);
        }
        fn visit_block(&mut self, block: &'ast Block) {
            walk_block(self, block);
        }
    }
    let mut finder = Finder(false, type_map);
    finder.visit_expr(expr);
    finder.0
}

/// Conservative scan: does `block` contain an assignment whose target is the
/// same binding as `key`?
///
/// `key` may be a lexical local or a synthetic `self.<field>` key.
/// Only inspects **top-level** statements in the block,
/// which matches the shapes post-guard narrowing currently reasons about.
/// False positives are safe (we skip narrowing); false negatives would be
/// unsound, so anything non-trivial defaults to "assume reassignment".
///
/// This intentionally does NOT use [`crate::ast::visitor`].
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

/// The bindings `expr` may assign, found by a **deep** walk (nested block
/// literals included, since a block built here may run before the code that
/// follows) — unlike the top-level-only [`block_may_reassign`]. A write the
/// walk cannot attribute to a key (a destructuring assignment, an unreadable
/// target) makes the answer [`Writes::Unknown`]. Used to drop the facts a
/// right-hand `and:` / `or:` argument may invalidate (BT-4710).
pub(crate) fn expr_writes(expr: &Expression) -> Writes {
    struct Finder(Writes);
    impl<'ast> Visitor<'ast> for Finder {
        fn visit_expr(&mut self, e: &'ast Expression) {
            match e {
                Expression::Assignment { target, .. } => match extract_variable_name(target) {
                    Some(key) => {
                        if let Writes::Keys(keys) = &mut self.0 {
                            keys.push(key);
                        }
                    }
                    None => self.0 = Writes::Unknown,
                },
                Expression::DestructureAssignment { .. } => self.0 = Writes::Unknown,
                _ => {}
            }
            walk_expr(self, e);
        }
        fn visit_block(&mut self, block: &'ast Block) {
            walk_block(self, block);
        }
    }
    let mut finder = Finder(Writes::Keys(Vec::new()));
    finder.visit_expr(expr);
    finder.0
}

/// What an expression may assign — see [`expr_writes`].
pub(crate) enum Writes {
    /// Exactly these bindings (possibly none).
    Keys(Vec<EnvKey>),
    /// Something the walk cannot name: assume any binding.
    Unknown,
}

/// Could a value of static type `ty` (`None`: never inferred) be a block?
/// Answers `false` only for a type provably disjoint from `Block`: a nominal
/// class that is neither `Block` nor one of its ancestors (`Object`,
/// `ProtoObject`), nor a type variable, a metaclass, or `Never`. `Dynamic`, `Object`, a protocol (a
/// `Block` may conform structurally), and any union, intersection or
/// negation containing one of those could be a block.
fn could_be_block(ty: Option<&InferredType>, protocols: Option<&ProtocolRegistry>) -> bool {
    match ty {
        Some(InferredType::Known { class_name, .. }) => {
            matches!(class_name.as_str(), "Block" | "Object" | "ProtoObject")
                // A rigid type variable (`:: E`) may be instantiated with `Block`.
                || is_generic_type_param(class_name)
                || protocols.is_some_and(|registry| registry.has_protocol(class_name))
        }
        Some(
            InferredType::Meta { .. } | InferredType::ProtocolMeta { .. } | InferredType::Never,
        ) => false,
        Some(InferredType::Negation { base, .. }) => could_be_block(Some(base), protocols),
        Some(InferredType::Union { members, .. } | InferredType::Intersection { members, .. }) => {
            members
                .iter()
                .any(|member| could_be_block(Some(member), protocols))
        }
        // `Dynamic`, no recorded type, or a shape added later: assume it could.
        _ => true,
    }
}

/// Can evaluating `expr` run code that reassigns an actor field? True for a
/// returning send to `self`/`super` ([`expr_contains_returning_self_send`]) and
/// for any send that receives a non-literal operand that *could* be a block as
/// its receiver or an argument — a stored block writes actor fields live
/// wherever it runs (ADR 0137), so `blk value`, `cb value: x` and
/// `items anySatisfy: blk` all count, whether `blk` is typed `Block`,
/// `Dynamic`, `Object` or a union containing one. Dropping a field fact is the
/// safe direction. `expr` must already have been inferred.
pub(crate) fn expr_may_write_fields(
    expr: &Expression,
    type_map: &TypeMap,
    protocols: Option<&ProtocolRegistry>,
) -> bool {
    struct Finder<'m>(bool, &'m TypeMap, Option<&'m ProtocolRegistry>);
    impl<'ast> Visitor<'ast> for Finder<'_> {
        fn visit_expr(&mut self, e: &'ast Expression) {
            if self.0 {
                return;
            }
            if let Expression::MessageSend {
                receiver,
                arguments,
                ..
            } = e
            {
                let is_block_value = |operand: &Expression| {
                    !matches!(operand, Expression::Block(_))
                        && could_be_block(self.1.get(operand.span()), self.2)
                };
                if is_block_value(receiver) || arguments.iter().any(is_block_value) {
                    self.0 = true;
                    return;
                }
            }
            walk_expr(self, e);
        }
        fn visit_block(&mut self, block: &'ast Block) {
            walk_block(self, block);
        }
    }
    if expr_contains_returning_self_send(expr, type_map) {
        return true;
    }
    let mut finder = Finder(false, type_map, protocols);
    finder.visit_expr(expr);
    finder.0
}

#[cfg(test)]
mod could_be_block_tests {
    use super::*;

    fn known(name: &str) -> InferredType {
        InferredType::known(name)
    }

    #[test]
    fn possible_blocks() {
        for name in ["Block", "Object", "ProtoObject", "E", "T"] {
            assert!(could_be_block(Some(&known(name)), None), "{name}");
        }
        assert!(could_be_block(None, None));
        assert!(could_be_block(
            Some(&InferredType::Dynamic(
                crate::semantic_analysis::type_checker::DynamicReason::Unknown
            )),
            None
        ));
    }

    #[test]
    fn provably_not_blocks() {
        for name in ["Integer", "String", "List"] {
            assert!(!could_be_block(Some(&known(name)), None), "{name}");
        }
        assert!(!could_be_block(Some(&InferredType::Never), None));
    }
}
