// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-class "`initialize` definitely assigns" must-analysis (ADR 0124 §6,
//! Implementation A2a).
//!
//! **DDD Context:** Semantic Analysis
//!
//! [`analyze_initialize_assigns`] walks a single class's own `initialize`
//! method body and answers, for each declared slot, whether *every* possible
//! completion of that method (falling off the end, or an early `^` return)
//! assigns it. This is a **must-analysis** (intersection over branches/
//! completions) — the opposite of [`crate::semantic_analysis::block_facts::BlockMutationAnalysis::field_writes`],
//! a *may*-analysis (union) used for state-threading, which this module does
//! not reuse (CLAUDE.md's no-duplicate-implementations rule doesn't apply
//! here: the two answer genuinely different questions over the same syntax).
//!
//! **Rules** (ADR 0124 §6):
//! - Sequencing is straight-line: an assignment earlier in a statement list
//!   is visible to every statement after it.
//! - A slot assigned in only one arm of `ifTrue:ifFalse:`/`match:` is not
//!   definite — only a slot assigned in *every* arm survives the
//!   intersection.
//! - A slot assigned inside a block that may not run at all (`ifTrue:`,
//!   `ifFalse:` with no counterpart, loops, `on:do:`'s handler, or any other
//!   block-valued message argument/receiver not specifically recognised as
//!   "always runs exactly once") is not definite.
//! - An early `^` before the assignment, on some path, makes it not
//!   definite: every early-return completion is intersected against every
//!   other possible completion (including falling off the end) exactly like
//!   an ordinary branch.
//! - `match:`/`matchExhaustive:` is treated as exhaustive (branches
//!   intersected like `ifTrue:ifFalse:`) when it carries a wildcard `_` arm
//!   or was written `matchExhaustive:` (checker-proven exhaustive); otherwise
//!   an unmatched value could raise at runtime, so nothing inside any arm is
//!   definite.
//! - `on:do:`'s *protected* (receiver) block is treated as straight-line
//!   (ADR 0124 §6's documented simplification: this analysis does not model
//!   an exception firing mid-body); its *handler* may not run at all.
//!
//! Self-sends are opaque — this analysis never traces into another method's
//! body (that is BT-1948's documented Hint tier, out of this issue's scope).

use crate::ast::{
    CascadeMessage, Expression, ExpressionStatement, MatchArm, MessageSelector, Pattern,
};
use crate::semantic_analysis::block_facts::is_self_reference;
use ecow::EcoString;
use std::collections::BTreeSet;

/// The result of analyzing a statement sequence (or a single expression) for
/// effect: what's definitely assigned if control falls off the end (`None`
/// means every path through this sequence diverges via an early return before
/// reaching the end), plus one completion set per early `^` return
/// encountered along the way (each already includes whatever was assigned
/// before that return).
struct FlowResult {
    fallthrough: Option<BTreeSet<EcoString>>,
    diverging: Vec<BTreeSet<EcoString>>,
}

/// Computes the set of state slots that `body` (a class's own `initialize`
/// method body) definitely assigns — i.e. assigns on every possible way the
/// method can complete, whether by falling off the end or an early `^`
/// return.
///
/// Empty for a class with no `initialize` (call this with `&[]`, or simply
/// don't call it — an absent `initialize` assigns nothing).
#[must_use]
pub fn analyze_initialize_assigns(body: &[ExpressionStatement]) -> BTreeSet<EcoString> {
    let result = analyze_sequence(body, &BTreeSet::new());
    let mut completions = result.diverging;
    if let Some(fallthrough) = result.fallthrough {
        completions.push(fallthrough);
    }
    intersect_all(&completions)
}

/// Intersects every set in `sets`. Empty input yields the empty set (the
/// conservative answer when there is no known completion to reason about).
fn intersect_all(sets: &[BTreeSet<EcoString>]) -> BTreeSet<EcoString> {
    let mut iter = sets.iter();
    let Some(first) = iter.next() else {
        return BTreeSet::new();
    };
    let mut result = first.clone();
    for s in iter {
        result = result.intersection(s).cloned().collect();
    }
    result
}

/// Analyzes a straight-line statement sequence, threading `base` (whatever is
/// already guaranteed assigned before the first statement) through each
/// statement in order.
fn analyze_sequence(stmts: &[ExpressionStatement], base: &BTreeSet<EcoString>) -> FlowResult {
    let mut current = base.clone();
    let mut diverging = Vec::new();
    for stmt in stmts {
        let step = analyze_expr(&stmt.expression, &current);
        diverging.extend(step.diverging);
        match step.fallthrough {
            Some(set) => current = set,
            None => {
                // Every path through this statement diverges — anything
                // syntactically following it in the same sequence is
                // unreachable.
                return FlowResult {
                    fallthrough: None,
                    diverging,
                };
            }
        }
    }
    FlowResult {
        fallthrough: Some(current),
        diverging,
    }
}

/// [`analyze_sequence`] over a block's own body.
fn analyze_block_body(block: &crate::ast::Block, base: &BTreeSet<EcoString>) -> FlowResult {
    analyze_sequence(&block.body, base)
}

/// Analyzes a single expression for its effect on the definitely-assigned
/// set, given `current` (what's guaranteed assigned just before it).
fn analyze_expr(expr: &Expression, current: &BTreeSet<EcoString>) -> FlowResult {
    match expr {
        Expression::Assignment { target, value, .. } => {
            let value_result = analyze_expr(value, current);
            let Some(mut after_value) = value_result.fallthrough else {
                return FlowResult {
                    fallthrough: None,
                    diverging: value_result.diverging,
                };
            };
            if let Expression::FieldAccess {
                receiver, field, ..
            } = target.as_ref()
            {
                if is_self_reference(receiver) {
                    after_value.insert(field.name.clone());
                }
            }
            FlowResult {
                fallthrough: Some(after_value),
                diverging: value_result.diverging,
            }
        }

        Expression::Return { value, .. } => {
            let value_result = analyze_expr(value, current);
            // The return itself always diverges, regardless of whether its
            // value expression does — the completion set is whatever was
            // guaranteed just before the `^` (falling back to `current` if
            // the value expression's own analysis had nothing further to add).
            let completion = value_result.fallthrough.unwrap_or_else(|| current.clone());
            let mut diverging = value_result.diverging;
            diverging.push(completion);
            FlowResult {
                fallthrough: None,
                diverging,
            }
        }

        Expression::Parenthesized { expression, .. } => analyze_expr(expression, current),

        Expression::Match {
            value,
            arms,
            exhaustive,
            ..
        } => analyze_match(value, arms, *exhaustive, current),

        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => analyze_message_send(receiver, selector, arguments, current),

        Expression::Cascade {
            receiver, messages, ..
        } => analyze_cascade(receiver, messages, current),

        // Every other expression kind (identifiers, literals, field reads,
        // class references, collection literals, destructuring of *local*
        // bindings, an un-invoked block literal used as a value, etc.) has no
        // effect on which `self` slots are definitely assigned, and always
        // falls through.
        _ => FlowResult {
            fallthrough: Some(current.clone()),
            diverging: Vec::new(),
        },
    }
}

/// Analyzes a `Cascade` (`receiver msg1; msg2; ...`). A cascade is not one of
/// ADR 0124 §6's named control constructs, so it gets the same conservative
/// generic treatment [`analyze_message_send`]'s fallback gives an ordinary
/// send's arguments (mirrored here rather than shared, since a cascade has no
/// single selector to special-case on): the receiver runs once — unless it is
/// itself a bare block literal (e.g. `[...] value; value`, cascading directly
/// onto a block value), which gets the same "may not run" treatment as any
/// other block-valued receiver — and then every cascaded message's arguments
/// are threaded in source order, with each block-literal argument treated as
/// "may not run" (its own assignments discarded, but any early `^` inside it
/// still harvested as a real completion path — the block may be invoked
/// later by whatever the cascaded selector does, and a non-local return fires
/// back through `initialize` regardless of who invokes it).
///
/// Getting this right matters even though nothing upstream reads
/// `initialize_assigns` as a diagnostic yet (BT-1948): a block argument's
/// `^` is a real divergent completion, and dropping it here (as the
/// catch-all fallback used to) would make [`intersect_all`] see one fewer
/// completion than actually exists — silently over-approximating what's
/// "definite".
fn analyze_cascade(
    receiver: &Expression,
    messages: &[CascadeMessage],
    current: &BTreeSet<EcoString>,
) -> FlowResult {
    let (mut acc, mut diverging) = if let Expression::Block(block) = receiver {
        let branch_result = analyze_block_body(block, current);
        (current.clone(), branch_result.diverging)
    } else {
        let recv_result = analyze_expr(receiver, current);
        let Some(after) = recv_result.fallthrough else {
            return FlowResult {
                fallthrough: None,
                diverging: recv_result.diverging,
            };
        };
        (after, recv_result.diverging)
    };

    for message in messages {
        for arg in &message.arguments {
            if let Expression::Block(block) = arg {
                let branch_result = analyze_block_body(block, &acc);
                diverging.extend(branch_result.diverging);
                // Discard branch_result.fallthrough — not guaranteed to run.
            } else {
                let arg_result = analyze_expr(arg, &acc);
                diverging.extend(arg_result.diverging);
                match arg_result.fallthrough {
                    Some(set) => acc = set,
                    None => {
                        return FlowResult {
                            fallthrough: None,
                            diverging,
                        };
                    }
                }
            }
        }
    }

    FlowResult {
        fallthrough: Some(acc),
        diverging,
    }
}

/// Analyzes a `match:`/`matchExhaustive:` expression (ADR 0124 §6): each
/// arm's body is analyzed independently from the post-scrutinee state, and
/// combined per the module doc's exhaustiveness rule.
fn analyze_match(
    value: &Expression,
    arms: &[MatchArm],
    exhaustive: bool,
    current: &BTreeSet<EcoString>,
) -> FlowResult {
    let value_result = analyze_expr(value, current);
    let Some(base) = value_result.fallthrough else {
        return FlowResult {
            fallthrough: None,
            diverging: value_result.diverging,
        };
    };

    let has_wildcard = arms
        .iter()
        .any(|arm| matches!(arm.pattern, Pattern::Wildcard(_)));
    let is_exhaustive = exhaustive || has_wildcard;

    let mut diverging = value_result.diverging;
    let mut fallthrough_sets: Vec<BTreeSet<EcoString>> = Vec::new();
    for arm in arms {
        let arm_result = analyze_expr(&arm.body, &base);
        diverging.extend(arm_result.diverging);
        if let Some(ft) = arm_result.fallthrough {
            fallthrough_sets.push(ft);
        }
    }

    let fallthrough = if is_exhaustive {
        if fallthrough_sets.is_empty() {
            // Every arm diverges (already captured in `diverging`) — this
            // match never falls through when reached.
            None
        } else {
            Some(intersect_all(&fallthrough_sets))
        }
    } else {
        // Not provably exhaustive: an unmatched value raises at runtime, so
        // no arm's assignments are guaranteed — same treatment as a block
        // that may not run.
        Some(base.clone())
    };

    FlowResult {
        fallthrough,
        diverging,
    }
}

/// Analyzes a `MessageSend`, special-casing the constructs ADR 0124 §6 names
/// explicitly (`ifTrue:ifFalse:`, single-arm `ifTrue:`/`ifFalse:`, `on:do:`)
/// and falling back to a conservative generic treatment for everything else.
fn analyze_message_send(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
    current: &BTreeSet<EcoString>,
) -> FlowResult {
    let selector_name = selector.name();

    if selector_name == "ifTrue:ifFalse:" {
        if let (Some(Expression::Block(true_block)), Some(Expression::Block(false_block))) =
            (arguments.first(), arguments.get(1))
        {
            let recv_result = analyze_expr(receiver, current);
            let Some(base) = recv_result.fallthrough else {
                return FlowResult {
                    fallthrough: None,
                    diverging: recv_result.diverging,
                };
            };
            let true_result = analyze_block_body(true_block, &base);
            let false_result = analyze_block_body(false_block, &base);
            let mut diverging = recv_result.diverging;
            diverging.extend(true_result.diverging);
            diverging.extend(false_result.diverging);
            // Both arms are exhaustive (there is no third path), so a slot
            // is only definite here if BOTH arms assign it.
            let fallthrough = match (true_result.fallthrough, false_result.fallthrough) {
                (Some(t), Some(f)) => Some(t.intersection(&f).cloned().collect()),
                (Some(t), None) => Some(t),
                (None, Some(f)) => Some(f),
                (None, None) => None,
            };
            return FlowResult {
                fallthrough,
                diverging,
            };
        }
    }

    if selector_name == "ifTrue:" || selector_name == "ifFalse:" {
        if let Some(Expression::Block(block)) = arguments.first() {
            let recv_result = analyze_expr(receiver, current);
            let Some(base) = recv_result.fallthrough else {
                return FlowResult {
                    fallthrough: None,
                    diverging: recv_result.diverging,
                };
            };
            let branch_result = analyze_block_body(block, &base);
            let mut diverging = recv_result.diverging;
            diverging.extend(branch_result.diverging);
            // The block may not run at all — its own assignments are not
            // definite, but an early return inside it is still a real
            // completion path.
            return FlowResult {
                fallthrough: Some(base),
                diverging,
            };
        }
    }

    if selector_name == "on:do:" {
        if let (Expression::Block(protected), Some(Expression::Block(handler))) =
            (receiver, arguments.get(1))
        {
            let protected_result = analyze_block_body(protected, current);
            // The handler may not run at all (only on a raised exception) —
            // analyzed from the pre-try state, since the protected body may
            // have thrown before assigning anything.
            let handler_result = analyze_block_body(handler, current);
            let mut diverging = protected_result.diverging;
            diverging.extend(handler_result.diverging);
            // ADR 0124 §6's documented simplification: the protected body is
            // treated as straight-line (an exception firing mid-body is not
            // modelled), so its fallthrough is this construct's own.
            return FlowResult {
                fallthrough: protected_result.fallthrough,
                diverging,
            };
        }
    }

    // Generic fallback: the receiver runs inline UNLESS it is itself a bare
    // block literal (e.g. `whileTrue:`/`whileFalse:`'s condition block,
    // `ensure:`'s protected body, or any other selector taking a block
    // receiver) — such a block is not guaranteed to run exactly once, so it
    // gets the same "may not run" treatment as a block-valued argument
    // (loops, `do:`/`collect:`/etc., `on:do:`'s handler already handled
    // above): early returns are harvested, but its own assignments are
    // discarded.
    let (mut acc, mut diverging) = if let Expression::Block(block) = receiver {
        let branch_result = analyze_block_body(block, current);
        (current.clone(), branch_result.diverging)
    } else {
        let recv_result = analyze_expr(receiver, current);
        let Some(after) = recv_result.fallthrough else {
            return FlowResult {
                fallthrough: None,
                diverging: recv_result.diverging,
            };
        };
        (after, recv_result.diverging)
    };

    for arg in arguments {
        if let Expression::Block(block) = arg {
            let branch_result = analyze_block_body(block, &acc);
            diverging.extend(branch_result.diverging);
            // Discard branch_result.fallthrough — not guaranteed to run.
        } else {
            let arg_result = analyze_expr(arg, &acc);
            diverging.extend(arg_result.diverging);
            match arg_result.fallthrough {
                Some(set) => acc = set,
                None => {
                    return FlowResult {
                        fallthrough: None,
                        diverging,
                    };
                }
            }
        }
    }

    FlowResult {
        fallthrough: Some(acc),
        diverging,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, Identifier, KeywordPart, Literal};
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn id(name: &str) -> Identifier {
        Identifier::new(name, span())
    }

    fn self_expr() -> Expression {
        Expression::Identifier(id("self"))
    }

    fn field(name: &str) -> Expression {
        Expression::FieldAccess {
            receiver: Box::new(self_expr()),
            field: id(name),
            span: span(),
        }
    }

    fn nil_lit() -> Expression {
        Expression::Identifier(id("nil"))
    }

    fn assign_field(name: &str, value: Expression) -> Expression {
        Expression::Assignment {
            target: Box::new(field(name)),
            value: Box::new(value),
            type_annotation: None,
            span: span(),
        }
    }

    fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    fn block(body: Vec<Expression>) -> Block {
        Block::new(vec![], body.into_iter().map(bare).collect(), span())
    }

    fn ret(value: Expression) -> Expression {
        Expression::Return {
            value: Box::new(value),
            span: span(),
        }
    }

    fn cond() -> Expression {
        Expression::Identifier(id("cond"))
    }

    fn if_true_if_false(true_body: Vec<Expression>, false_body: Vec<Expression>) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(cond()),
            selector: MessageSelector::Keyword(vec![
                KeywordPart::new("ifTrue:", span()),
                KeywordPart::new("ifFalse:", span()),
            ]),
            arguments: vec![
                Expression::Block(block(true_body)),
                Expression::Block(block(false_body)),
            ],
            is_cast: false,
            span: span(),
        }
    }

    fn if_true(body: Vec<Expression>) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(cond()),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("ifTrue:", span())]),
            arguments: vec![Expression::Block(block(body))],
            is_cast: false,
            span: span(),
        }
    }

    fn on_do(protected: Vec<Expression>, handler: Vec<Expression>) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(Expression::Block(block(protected))),
            selector: MessageSelector::Keyword(vec![
                KeywordPart::new("on:", span()),
                KeywordPart::new("do:", span()),
            ]),
            arguments: vec![
                Expression::Identifier(id("Error")),
                Expression::Block(block(handler)),
            ],
            is_cast: false,
            span: span(),
        }
    }

    fn cascade_message(selector: &str, arguments: Vec<Expression>) -> CascadeMessage {
        CascadeMessage::new(
            MessageSelector::Keyword(vec![KeywordPart::new(selector, span())]),
            arguments,
            span(),
        )
    }

    fn cascade(receiver: Expression, messages: Vec<CascadeMessage>) -> Expression {
        Expression::Cascade {
            receiver: Box::new(receiver),
            messages,
            span: span(),
        }
    }

    fn while_true(cond_body: Vec<Expression>, loop_body: Vec<Expression>) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(Expression::Block(block(cond_body))),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", span())]),
            arguments: vec![Expression::Block(block(loop_body))],
            is_cast: false,
            span: span(),
        }
    }

    fn wildcard_arm(body: Expression) -> MatchArm {
        MatchArm::new(Pattern::Wildcard(span()), body, span())
    }

    fn symbol_arm(name: &str, body: Expression) -> MatchArm {
        MatchArm::new(
            Pattern::Literal(Literal::Symbol(name.into()), span()),
            body,
            span(),
        )
    }

    fn match_expr(value: Expression, arms: Vec<MatchArm>, exhaustive: bool) -> Expression {
        Expression::Match {
            value: Box::new(value),
            arms,
            exhaustive,
            span: span(),
        }
    }

    fn assigns(body: Vec<Expression>) -> BTreeSet<EcoString> {
        let stmts: Vec<ExpressionStatement> = body.into_iter().map(bare).collect();
        analyze_initialize_assigns(&stmts)
    }

    fn set(names: &[&str]) -> BTreeSet<EcoString> {
        names.iter().map(|n| EcoString::from(*n)).collect()
    }

    #[test]
    fn straight_line_assignment_is_definite() {
        let result = assigns(vec![
            assign_field("a", Expression::Literal(Literal::Integer(1), span())),
            assign_field("b", Expression::Literal(Literal::Integer(2), span())),
        ]);
        assert_eq!(result, set(&["a", "b"]));
    }

    #[test]
    fn both_arms_assigning_same_field_is_definite() {
        let result = assigns(vec![if_true_if_false(
            vec![assign_field("x", nil_lit())],
            vec![assign_field("x", nil_lit())],
        )]);
        assert_eq!(result, set(&["x"]));
    }

    #[test]
    fn one_arm_only_is_not_definite() {
        let result = assigns(vec![if_true_if_false(
            vec![assign_field("x", nil_lit())],
            vec![], // false arm doesn't assign x
        )]);
        assert!(
            result.is_empty(),
            "a slot assigned in only one arm of ifTrue:ifFalse: must not be definite"
        );
    }

    #[test]
    fn block_that_may_not_run_is_not_definite() {
        // cond ifTrue: [self.x := nil] — no else, so the block may not run.
        let result = assigns(vec![if_true(vec![assign_field("x", nil_lit())])]);
        assert!(
            result.is_empty(),
            "an ifTrue: with no else must not make its assignment definite"
        );
    }

    #[test]
    fn early_return_before_assignment_is_not_definite() {
        // cond ifTrue: [^nil]. self.x := nil
        let result = assigns(vec![
            if_true(vec![ret(nil_lit())]),
            assign_field("x", nil_lit()),
        ]);
        assert!(
            result.is_empty(),
            "an early return on some path before the assignment must not be definite"
        );
    }

    #[test]
    fn early_return_after_assignment_stays_definite() {
        // self.x := nil. cond ifTrue: [^nil]
        let result = assigns(vec![
            assign_field("x", nil_lit()),
            if_true(vec![ret(nil_lit())]),
        ]);
        assert_eq!(
            result,
            set(&["x"]),
            "an assignment before every possible early return stays definite"
        );
    }

    #[test]
    fn match_with_wildcard_intersects_arms() {
        let value = Expression::Identifier(id("kind"));
        let result = assigns(vec![match_expr(
            value,
            vec![
                symbol_arm("a", assign_field("x", nil_lit())),
                symbol_arm("b", assign_field("x", nil_lit())),
                wildcard_arm(assign_field("x", nil_lit())),
            ],
            false,
        )]);
        assert_eq!(
            result,
            set(&["x"]),
            "an exhaustive match: (via wildcard) whose every arm assigns x makes it definite"
        );
    }

    #[test]
    fn match_without_wildcard_is_not_definite() {
        let value = Expression::Identifier(id("kind"));
        let result = assigns(vec![match_expr(
            value,
            vec![
                symbol_arm("a", assign_field("x", nil_lit())),
                symbol_arm("b", assign_field("x", nil_lit())),
            ],
            false,
        )]);
        assert!(
            result.is_empty(),
            "a non-exhaustive match: (no wildcard, not asserted) must not be definite \
             even when every known arm assigns the field"
        );
    }

    #[test]
    fn on_do_protected_body_assignment_is_definite() {
        let result = assigns(vec![on_do(vec![assign_field("x", nil_lit())], vec![])]);
        assert_eq!(
            result,
            set(&["x"]),
            "on:do:'s protected body is treated as straight-line"
        );
    }

    #[test]
    fn on_do_handler_assignment_is_not_definite() {
        let result = assigns(vec![on_do(vec![], vec![assign_field("x", nil_lit())])]);
        assert!(
            result.is_empty(),
            "on:do:'s handler may not run at all, so its assignment must not be definite"
        );
    }

    #[test]
    fn loop_body_assignment_is_not_definite() {
        // [cond] whileTrue: [self.x := nil]
        let result = assigns(vec![while_true(
            vec![cond()],
            vec![assign_field("x", nil_lit())],
        )]);
        assert!(
            result.is_empty(),
            "a loop body may run zero times, so its assignment must not be definite"
        );
    }

    #[test]
    fn no_initialize_body_assigns_nothing() {
        let result = assigns(vec![]);
        assert!(result.is_empty());
    }

    #[test]
    fn early_return_inside_cascade_block_argument_is_not_definite() {
        // self log: "x"; onFailure: [^nil]. self.x := nil.
        //
        // `onFailure:` is an ordinary cascaded keyword message, not one of
        // ADR 0124 §6's named control constructs — but its block-literal
        // argument may still be invoked by whatever `onFailure:` does, and if
        // it is, the `^nil` inside it fires a non-local return back through
        // `initialize` *before* `self.x := nil` ever runs. That's a real
        // completion path the must-analysis has to see, even though the
        // cascade's own "effect" (a message send whose result is discarded)
        // has no direct bearing on any slot.
        let result = assigns(vec![
            cascade(
                self_expr(),
                vec![
                    cascade_message(
                        "log:",
                        vec![Expression::Literal(Literal::String("x".into()), span())],
                    ),
                    cascade_message(
                        "onFailure:",
                        vec![Expression::Block(block(vec![ret(nil_lit())]))],
                    ),
                ],
            ),
            assign_field("x", nil_lit()),
        ]);
        assert!(
            result.is_empty(),
            "an early return inside a cascaded message's block argument must not be dropped — \
             it is a real completion path that skips the assignment following the cascade"
        );
    }

    #[test]
    fn cascade_with_no_block_arguments_has_no_effect_on_assignment() {
        // self log: "x"; log: "y". self.x := nil.
        // A cascade with no block-valued arguments at all still falls
        // through normally and doesn't spuriously affect the definite set.
        let result = assigns(vec![
            cascade(
                self_expr(),
                vec![
                    cascade_message(
                        "log:",
                        vec![Expression::Literal(Literal::String("x".into()), span())],
                    ),
                    cascade_message(
                        "log:",
                        vec![Expression::Literal(Literal::String("y".into()), span())],
                    ),
                ],
            ),
            assign_field("x", nil_lit()),
        ]);
        assert_eq!(
            result,
            set(&["x"]),
            "a cascade with no block arguments must not block an assignment after it"
        );
    }

    #[test]
    fn unrelated_local_assignment_does_not_leak_in() {
        let result = assigns(vec![Expression::Assignment {
            target: Box::new(Expression::Identifier(id("temp"))),
            value: Box::new(nil_lit()),
            type_annotation: None,
            span: span(),
        }]);
        assert!(
            result.is_empty(),
            "a local-variable assignment is not a `self` slot assignment"
        );
    }
}
