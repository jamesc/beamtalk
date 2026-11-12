// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared block helpers: Tier 2 stored-block promotion facts (ADR 0041),
//! cascade normalisation and literal-block predicates.
//!
//! **DDD Context:** Compilation — Semantic Analysis

use super::BlockMutationAnalysis;
use crate::ast::{BinarySegment, Expression, MessageSelector};

// ---------------------------------------------------------------------------
// Tier 2 stored-block promotion facts (ADR 0041). Moved from beamtalk-codegen's
// `gen_server/methods.rs` (BT-3745) so codegen's `prescan_tier2_local_vars` and
// the ADR 0131 §6 check (`validators/local_threading.rs`) share one
// implementation of "which uses of a stored Tier 2 block are safe".
// ---------------------------------------------------------------------------

/// The outer locals a block literal both reads and writes (`local_writes` ∩
/// `captured_reads` of its [`analyze_block`](super::analyze_block) analysis), sorted: the set codegen's Tier 2
/// block protocol threads back (`captured_mutations_for_block`). A block whose
/// outer-local write is not in this set (a write-only write, or one this
/// analysis does not propagate out of a nested block) is not promoted, and
/// the write is lost.
#[must_use]
pub fn captured_local_mutations_from_analysis(analysis: &BlockMutationAnalysis) -> Vec<String> {
    analysis
        .local_writes
        .intersection(&analysis.captured_reads)
        .cloned()
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect()
}

/// Normalizes a `Cascade` into its true underlying receiver and the
/// full ordered list of messages sent to it.
///
/// The parser (`parse_cascade`) folds the cascade's *first* message into
/// `Cascade.receiver` as a whole `MessageSend` — e.g. for `blk value: x;
/// value: y`, `receiver` is `MessageSend(blk, value:, [x])` and `messages`
/// holds only the remaining `value: y`. Every safety/codegen decision needs
/// the TRUE receiver (`blk`) and ALL messages sent to it (both `value: x`
/// and `value: y`), so this mirrors the same normalization
/// `generate_cascade` (expressions.rs) already performs for ordinary
/// (non-Tier-2) cascade codegen.
#[must_use]
pub fn normalize_cascade<'a>(
    receiver: &'a Expression,
    messages: &'a [crate::ast::CascadeMessage],
) -> (&'a Expression, Vec<(&'a MessageSelector, &'a [Expression])>) {
    if let Expression::MessageSend {
        receiver: inner,
        selector: first_selector,
        arguments: first_arguments,
        ..
    } = receiver
    {
        let mut all: Vec<(&MessageSelector, &[Expression])> =
            Vec::with_capacity(messages.len() + 1);
        all.push((first_selector, first_arguments.as_slice()));
        for msg in messages {
            all.push((&msg.selector, msg.arguments.as_slice()));
        }
        (inner.as_ref(), all)
    } else {
        let all: Vec<(&MessageSelector, &[Expression])> = messages
            .iter()
            .map(|msg| (&msg.selector, msg.arguments.as_slice()))
            .collect();
        (receiver, all)
    }
}

/// Returns true if `selector` is a `value`/`value:`/
/// `value:value:`/`value:value:value:` send — the "safe" family that lets a
/// Tier 2 block value be invoked without escaping to a call site that
/// doesn't know to thread state through it.
#[must_use]
pub fn is_safe_value_family_selector(selector: &MessageSelector) -> bool {
    selector
        .well_known()
        .is_some_and(crate::ast::WellKnownSelector::is_block_value)
}

/// Scans `expr` for references to `var_name`, returning
/// `(has_unsafe_use, has_safe_use)`.
///
/// A *safe* use is the receiver of a `value`/`value:`/`value:value:`/
/// `value:value:value:` send. Any other reference — a bare return, an
/// argument to another call, a reassignment, ... — is *unsafe*, since it
/// would let a Tier 2 block value escape to a call site that doesn't know
/// to thread state through it. A variable that's *never* referenced at
/// all yields `(false, false)`, which the caller must treat as unsafe
/// (not "no unsafe use found") — see `prescan_tier2_local_vars`.
///
/// Deliberately conservative: exhaustively matches every `Expression`
/// variant so a use hidden inside e.g. a map literal or string
/// interpolation is never silently missed. A shadowing block parameter
/// with the same name is *not* special-cased — that only makes this
/// over-conservative (a missed promotion), never unsafe.
#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over every Expression variant, kept as one function for locality with its single caller"
)]
#[must_use]
pub fn stored_block_var_uses(expr: &Expression, var_name: &str) -> (bool, bool) {
    match expr {
        Expression::Identifier(id) => (id.name == var_name, false),
        Expression::Literal(..)
        | Expression::ClassReference { .. }
        | Expression::Super(_)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Error { .. } => (false, false),
        Expression::Spread { name, .. } => (name.name == var_name, false),
        Expression::FieldAccess { receiver, .. } => stored_block_var_uses(receiver, var_name),
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            let is_safe_value_send = matches!(
                receiver.as_ref(),
                Expression::Identifier(id) if id.name == var_name
            ) && is_safe_value_family_selector(selector);
            let (mut unsafe_, mut safe) = if is_safe_value_send {
                (false, true)
            } else {
                stored_block_var_uses(receiver, var_name)
            };
            for arg in arguments {
                let (u, s) = stored_block_var_uses(arg, var_name);
                unsafe_ |= u;
                safe |= s;
            }
            (unsafe_, safe)
        }
        Expression::Block(block) => {
            // Any reference to var_name inside a nested block literal is
            // unsafe — see the safety invariant note on
            // prescan_tier2_local_vars above (a nested block compiles
            // through a completely different path with no Tier2-tuple
            // unpacking and no tier2_local_vars reset of its own).
            let (any_unsafe, any_safe) = block
                .body
                .iter()
                .map(|stmt| stored_block_var_uses(&stmt.expression, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
            (any_unsafe || any_safe, false)
        }
        Expression::Assignment { target, value, .. } => {
            let (u1, s1) = stored_block_var_uses(target, var_name);
            let (u2, s2) = stored_block_var_uses(value, var_name);
            (u1 || u2, s1 || s2)
        }
        Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
            stored_block_var_uses(value, var_name)
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            // When the cascade's true underlying receiver (see
            // `normalize_cascade`) *is* var_name itself (e.g. `blk value: x;
            // value: y`), the generic recursive scan would hit the plain
            // `Identifier` arm and unconditionally report it unsafe. Mirror the
            // `MessageSend` arm's `is_safe_value_send` check instead: if EVERY
            // message sent to that receiver (including the one folded into
            // `receiver` by the parser) is itself a safe
            // `value`/`value:`/`value:value:`/`value:value:value:` send, the
            // whole cascade is as safe as a single safe value send would be.
            let (underlying_receiver, all_messages) = normalize_cascade(receiver, messages);
            let receiver_is_var = matches!(
                underlying_receiver,
                Expression::Identifier(id) if id.name == var_name
            );
            let all_messages_safe_value_sends = receiver_is_var
                && !all_messages.is_empty()
                && all_messages
                    .iter()
                    .all(|(sel, _)| is_safe_value_family_selector(sel));
            let (mut unsafe_, mut safe) = if all_messages_safe_value_sends {
                (false, true)
            } else {
                stored_block_var_uses(underlying_receiver, var_name)
            };
            for (_, args) in &all_messages {
                for arg in *args {
                    let (u, s) = stored_block_var_uses(arg, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
            }
            (unsafe_, safe)
        }
        Expression::Parenthesized { expression, .. } => stored_block_var_uses(expression, var_name),
        Expression::Match { value, arms, .. } => {
            let (mut unsafe_, mut safe) = stored_block_var_uses(value, var_name);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    let (u, s) = stored_block_var_uses(guard, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
                let (u, s) = stored_block_var_uses(&arm.body, var_name);
                unsafe_ |= u;
                safe |= s;
            }
            (unsafe_, safe)
        }
        Expression::MapLiteral { pairs, .. } => pairs
            .iter()
            .map(|pair| {
                let (u1, s1) = stored_block_var_uses(&pair.key, var_name);
                let (u2, s2) = stored_block_var_uses(&pair.value, var_name);
                (u1 || u2, s1 || s2)
            })
            .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
        Expression::ListLiteral { elements, tail, .. } => {
            let (mut unsafe_, mut safe) = elements
                .iter()
                .map(|e| stored_block_var_uses(e, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
            if let Some(t) = tail {
                let (u, s) = stored_block_var_uses(t, var_name);
                unsafe_ |= u;
                safe |= s;
            }
            (unsafe_, safe)
        }
        Expression::ArrayLiteral { elements, .. } | Expression::TupleLiteral { elements, .. } => {
            elements
                .iter()
                .map(|e| stored_block_var_uses(e, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2))
        }
        Expression::StringInterpolation { segments, .. } => segments
            .iter()
            .map(|seg| match seg {
                crate::ast::StringSegment::Interpolation(e) => stored_block_var_uses(e, var_name),
                crate::ast::StringSegment::Literal(_) => (false, false),
            })
            .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
        Expression::BinaryConstruction { segments, .. } => segments
            .iter()
            .flat_map(BinarySegment::sub_expressions)
            .map(|e| stored_block_var_uses(e, var_name))
            .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
    }
}

/// Checks if a block is a literal block (not a variable reference).
pub(crate) fn is_literal_block(expr: &Expression) -> bool {
    matches!(expr, Expression::Block(_))
}

/// Checks if a message send is a control flow construct with a literal block.
#[cfg(test)]
pub fn is_control_flow_construct(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
) -> bool {
    match selector {
        MessageSelector::Keyword(parts) => {
            let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

            match selector_name.as_str() {
                // whileTrue: / whileFalse: - block receiver + literal block arg
                "whileTrue:" | "whileFalse:" => {
                    is_literal_block(receiver) && arguments.first().is_some_and(is_literal_block)
                }

                // timesRepeat: - integer receiver + literal block
                "timesRepeat:" => arguments.first().is_some_and(is_literal_block),

                // to:do: and inject:into: - literal block as second arg
                "to:do:" | "inject:into:" => arguments.get(1).is_some_and(is_literal_block),

                // to:by:do: - literal block as third arg
                "to:by:do:" => arguments.get(2).is_some_and(is_literal_block),

                // Collection iteration: do:, collect:, select:, reject: - literal block as first arg
                "do:" | "collect:" | "select:" | "reject:" => {
                    arguments.first().is_some_and(is_literal_block)
                }

                _ => false,
            }
        }
        _ => false,
    }
}
