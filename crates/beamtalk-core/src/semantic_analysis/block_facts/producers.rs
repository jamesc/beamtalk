// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Local-threading producer classification (ADR 0131).
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! Recognises local-threading constructs ([`LocalThreadingConstruct`]) and the
//! conditional, exception, `match:` and REPL-site producers that rebind outer
//! locals, with the threaded-write sets they carry.

use super::{
    LoopBodyContext, NestedBlocks, OuterLocalWrite, OuterWriteWalker, is_loop_body_producer_site,
    is_result_try_do, is_safe_value_family_selector, is_self_field_target,
    loop_body_producer_sites, names_assigned_outside_nested_blocks, sequenced_operand_sites,
    walk_construct_blocks,
};
use crate::ast::{
    Block, BlockParameter, Expression, ExpressionStatement, MatchArm, MessageSelector,
};
use crate::semantic_analysis::type_checker::InferredType;
use crate::source_analysis::Span;
use ecow::EcoString;
use std::collections::HashSet;

/// ADR 0131 §1: the kind of a local-threading construct (see
/// [`local_threading_construct`]). Codegen lowers each family with its own
/// generator. The families not threaded yet are still recognized, so the
/// phase that makes one a producer only flips
/// [`LocalThreadingConstruct::is_threaded_today`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalThreadingFamily {
    /// `whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:`, the
    /// unary `whileTrue`/`whileFalse`/`timesRepeat`, and `repeat`.
    Loop,
    /// A collection fold or `do:`: any other
    /// [`crate::state_threading_selectors::is_state_threading_keyword_selector`]
    /// selector, including `detect:ifNone:`, `eachWithIndex:` and
    /// `do:separatedBy:`.
    Fold,
    /// The conditional family
    /// ([`crate::state_threading_selectors::is_conditional_selector`]).
    Conditional,
    /// `on:do:` / `ensure:`.
    Exception,
    /// `value`/`value:`… sent to a block literal of the send's arity
    /// ([`block_literal_arity_matches`], BT-3884).
    BlockValue,
    /// A block-taking lookup selector
    /// ([`crate::state_threading_selectors::LOOKUP_SHAPES`]). Threaded only
    /// for the rows that are
    /// [`dictionary_inline`](crate::state_threading_selectors::LookupShape::dictionary_inline)
    /// (ADR 0131 Phase 2d, BT-3798): see
    /// [`LocalThreadingConstruct::is_threaded_today`].
    Lookup,
    /// `Result tryDo:` (ADR 0131 §5, Phase 4): a catch-boundary construct
    /// whose block literal compiles to a Tier 2 `fun(StateAcc)` and is a
    /// producer through [`exception_producer_writes`], like `on:do:`.
    TryDo,
}

/// ADR 0131 §1: a local-threading construct. It is a send whose literal
/// block(s) run in the enclosing activation, so an outer-local write inside
/// them is a state effect of the send. Built by
/// [`local_threading_construct`].
#[derive(Debug, Clone)]
pub struct LocalThreadingConstruct<'a> {
    /// The send's selector.
    pub selector: String,
    /// The selector as written (`ast::normalize::selector_as_written`):
    /// `repeat` or a unary `whileTrue`/`whileFalse` for a loop the
    /// normalising pass canonicalised (BT-4589), else [`Self::selector`]. For
    /// diagnostics only.
    pub written_selector: EcoString,
    /// Which construct it is.
    pub family: LocalThreadingFamily,
    /// The construct's block literals, receiver first.
    pub blocks: Vec<&'a Block>,
    /// Whether codegen inlines the construct's blocks. False only for an
    /// `on:do:`/`ensure:` whose protected body or handler is not a block
    /// literal (`b ensure: [w := 9]`, `[w := 9] ensure: c`): codegen compiles
    /// such a send's literal block as a closure (BT-4430), so the block still
    /// runs during the send ([`Self::runs_in_place`]) but its outer-local
    /// writes are not threaded back ([`Self::is_threaded_today`]).
    pub inlined: bool,
}

impl LocalThreadingConstruct<'_> {
    /// Whether codegen threads this construct's outer-local writes back today
    /// (every family but a lookup, whose selector must be
    /// [`dictionary_inline`](crate::state_threading_selectors::LookupShape::dictionary_inline)).
    /// The one predicate the nested-construct walks ([`threaded_today_block_writes`])
    /// and the Phase 0 check filter on.
    ///
    /// BT-4430: and not an `on:do:`/`ensure:` that codegen does not inline
    /// ([`Self::inlined`]): its literal block is a closure, which can write no
    /// outer local, so the Phase 0 check reports such a write (ADR 0133).
    #[must_use]
    pub fn is_threaded_today(&self) -> bool {
        self.inlined && self.runs_in_place()
    }

    /// Whether the construct's blocks run during the send, in the activation
    /// that holds it: every family but a lookup whose selector is not
    /// [`dictionary_inline`](crate::state_threading_selectors::LookupShape::dictionary_inline)
    /// (whose block is handed to the runtime). Unlike
    /// [`Self::is_threaded_today`], an `on:do:`/`ensure:` that codegen does not
    /// inline still runs its literal block here, as a closure applied inside
    /// the `try` (BT-4430): a `^` in it ends the method or REPL input, and it
    /// reads the locals as they stand. The predicate the `^` classification
    /// ([`classify_in_place_returns`]) and the evaluation-order walks read.
    #[must_use]
    pub fn runs_in_place(&self) -> bool {
        match self.family {
            LocalThreadingFamily::Lookup => {
                crate::state_threading_selectors::is_dictionary_inline_lookup(&self.selector)
            }
            _ => true,
        }
    }
}

/// ADR 0131 §1 (BT-3745, BT-3746): the one recognizer for a local-threading
/// construct. `None` when `expr` (parentheses peeled) is not one.
///
/// Derived only from [`crate::state_threading_selectors`], with no selector
/// table of its own. The blocks it collects:
///
/// - a state-threading, conditional or exception keyword selector: each
///   literal block argument, plus the literal block receiver for
///   `whileTrue:`/`whileFalse:` (the condition) and `on:do:`/`ensure:` (the
///   protected body);
/// - a lookup selector: its block arguments; `tryDo:`: its block argument;
/// - a unary loop selector (`whileTrue`, `whileFalse`, `timesRepeat`,
///   `repeat`) or a block `value` send: its literal block receiver.
///
/// A `value`-family send is a [`LocalThreadingFamily::BlockValue`] construct
/// only when its block literal's arity is the send's (BT-3884): a block
/// literal of another arity ([`block_literal_of_other_arity`], the predicate
/// codegen's lowering reads) is applied and raises `arity_mismatch`, so the
/// send is no construct.
///
/// Only direct `[...]` literals count, because only those are inlined. A
/// parenthesized or stored block is a block value (ADR 0131 §6).
///
/// The Phase 0 allow-set check (`validators/local_threading.rs`) and
/// codegen's `threaded_locals_of` both recognize constructs with this.
#[must_use]
pub fn local_threading_construct(expr: &Expression) -> Option<LocalThreadingConstruct<'_>> {
    use crate::state_threading_selectors::{
        is_conditional_selector, is_exception_selector, is_state_threaded_block_receiver,
        is_state_threading_keyword_selector, lookup_block_arg_indices,
    };
    // BT-4603: analysis reads only the normalised tree, where every unary loop
    // on a parameterless literal block is its keyword loop (BT-4313, BT-4589).
    // A raw parse would classify its receiver as a loop body, while codegen
    // lowers it as the keyword loop's condition.
    let node = expr.unwrap_parens();
    debug_assert!(
        !crate::ast::normalize::is_uncanonicalised_loop(node),
        "local_threading_construct reached an uncanonicalised unary loop; \
         run `ast::normalize` first"
    );
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: false,
        ..
    } = node
    else {
        return None;
    };
    let sel = selector.name().to_string();
    let receiver_block = match receiver.as_ref() {
        Expression::Block(b) => Some(b),
        _ => None,
    };
    // BT-3884: a `value`-family send is a block evaluation only when the
    // block literal's arity is the send's. One of another arity
    // ([`block_literal_of_other_arity`]: `[t := t + 1] value: 1`) is applied,
    // never inlined: it raises `arity_mismatch` and its body never runs, so it
    // is no construct and its writes are no state effect of the send.
    let evaluated_block = receiver_block
        .filter(|_| block_literal_of_other_arity(receiver, selector, arguments.len()).is_none());
    let literal_args = || {
        arguments.iter().filter_map(|a| match a {
            Expression::Block(b) => Some(b),
            _ => None,
        })
    };
    let mut blocks: Vec<&Block> = Vec::new();
    let mut inlined = true;
    let family = match selector {
        MessageSelector::Keyword(_) => {
            if is_exception_selector(&sel) {
                blocks.extend(receiver_block);
                blocks.extend(literal_args());
                inlined = exception_send_inlined(&sel, receiver, arguments);
                LocalThreadingFamily::Exception
            } else if is_conditional_selector(&sel) {
                blocks.extend(literal_args());
                LocalThreadingFamily::Conditional
            } else if is_state_threading_keyword_selector(&sel) {
                let is_while = matches!(sel.as_str(), "whileTrue:" | "whileFalse:");
                if is_while {
                    blocks.extend(receiver_block);
                }
                blocks.extend(literal_args());
                if is_while || matches!(sel.as_str(), "timesRepeat:" | "to:do:" | "to:by:do:") {
                    LocalThreadingFamily::Loop
                } else {
                    LocalThreadingFamily::Fold
                }
            } else if is_state_threaded_block_receiver(&sel) {
                blocks.extend(evaluated_block);
                LocalThreadingFamily::BlockValue
            } else if !lookup_block_arg_indices(&sel).is_empty() {
                blocks.extend(lookup_block_arg_indices(&sel).iter().filter_map(|&i| {
                    match arguments.get(i) {
                        Some(Expression::Block(b)) => Some(b),
                        _ => None,
                    }
                }));
                LocalThreadingFamily::Lookup
            } else if is_result_try_do(receiver, &sel) {
                blocks.extend(literal_args());
                LocalThreadingFamily::TryDo
            } else {
                return None;
            }
        }
        MessageSelector::Unary(_) => {
            // A literal `[c] whileTrue`/`[b] repeat` reaches analysis as the
            // keyword loop it is canonicalised to (BT-4313, BT-4589); any
            // other unary loop send is an ordinary send to `Block` (BT-4603).
            if is_state_threaded_block_receiver(&sel) {
                blocks.extend(evaluated_block);
                LocalThreadingFamily::BlockValue
            } else {
                return None;
            }
        }
        MessageSelector::Binary(_) => return None,
    };
    if blocks.is_empty() {
        None
    } else {
        Some(LocalThreadingConstruct {
            written_selector: crate::ast::normalize::selector_as_written(
                receiver, selector, arguments,
            ),
            selector: sel,
            family,
            blocks,
            inlined,
        })
    }
}

/// BT-4430: whether codegen inlines the `on:do:`/`ensure:` send of `selector`
/// to `receiver` with `arguments`: its protected body (the receiver) and its
/// handler or cleanup are both block literals, the shape
/// [`exception_arm_blocks`] and codegen's `generate_on_do`/`generate_ensure`
/// lower in place. Any other such send compiles its literal block as a
/// closure. The one rule behind [`LocalThreadingConstruct::inlined`] and the
/// ADR 0133 escape check (`validators/local_threading.rs`); codegen's
/// generators decide through [`inlined_exception_blocks`], which it reads.
#[must_use]
pub fn exception_send_inlined(
    selector: &str,
    receiver: &Expression,
    arguments: &[Expression],
) -> bool {
    match crate::state_threading_selectors::state_threaded_block_arg_indices(selector) {
        &[handler_index] => arguments
            .get(handler_index)
            .is_some_and(|handler| inlined_exception_blocks(receiver, handler).is_some()),
        _ => false,
    }
}

/// BT-4430: the protected body and the handler or cleanup of an
/// `on:do:`/`ensure:` when the send is inlined ([`exception_send_inlined`]):
/// both are bare block literals. `None` when either is not, and codegen's
/// `generate_on_do`/`generate_ensure` compile the send's blocks as closures.
#[must_use]
pub fn inlined_exception_blocks<'a>(
    receiver: &'a Expression,
    handler: &'a Expression,
) -> Option<(&'a Block, &'a Block)> {
    match (receiver, handler) {
        (Expression::Block(protected), Expression::Block(handler)) => Some((protected, handler)),
        _ => None,
    }
}

/// BT-4288 / BT-4312: the `^`s of a top-level scope (a REPL input), by where
/// they run. The one rule both the REPL check
/// (`validators/local_threading.rs`) and codegen's REPL eval boundary
/// (`beamtalk-codegen`'s `repl_nlr.rs`, through [`repl_eval_returns`]) read.
///
/// A block is *run in place* when it is a block of a
/// [`local_threading_construct`] whose blocks run during the send
/// ([`LocalThreadingConstruct::runs_in_place`]): a conditional arm, an
/// evaluated or protected block (`value`, `on:do:`, `ensure:`), a loop or
/// fold body. Any other block literal is a *closure* (stored, passed to a
/// method, ...).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct InPlaceReturns {
    /// No closure encloses the `^` (it is at the root, or every enclosing
    /// block is run in place): the `^` runs while the method or REPL input
    /// that holds it runs, so it ends it with its value, as in a method.
    pub in_place: Vec<Span>,
    /// The `^` is in a loop or fold body (walking outward, a loop or fold
    /// body is reached before any closure) and a closure encloses that loop
    /// (`b := [#(1, 2) do: [:x | ^x]]`, `Holder keep: [...]`): the `^` may
    /// run in a later REPL input, after the eval that created it has ended,
    /// so no REPL boundary can catch it. At the REPL it answers its value
    /// inside the loop, which ignores it, so the REPL check rejects it
    /// (BT-4003). A `^` held by a closure but in no loop body of its own
    /// (`b := [:x | ^x]`) is in neither list: it answers its value where it
    /// stands, the closure's value.
    pub in_closure: Vec<Span>,
}

/// Classifies the `^`s of `exprs` ([`InPlaceReturns`]).
#[must_use]
pub fn classify_in_place_returns<'a>(
    exprs: impl IntoIterator<Item = &'a Expression>,
) -> InPlaceReturns {
    let mut in_place: HashSet<Span> = HashSet::new();
    let mut loop_bodies: HashSet<Span> = HashSet::new();
    let mut blocks: Vec<Span> = Vec::new();
    let mut returns: Vec<Span> = Vec::new();
    for expr in exprs {
        crate::ast_walker::walk_expression(expr, &mut |e| match e {
            Expression::Block(b) => blocks.push(b.span),
            Expression::Return { span, .. } => returns.push(*span),
            Expression::MessageSend { .. } => {
                let Some(construct) =
                    local_threading_construct(e).filter(LocalThreadingConstruct::runs_in_place)
                else {
                    return;
                };
                let is_loop = matches!(
                    construct.family,
                    LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
                );
                for block in construct.blocks {
                    in_place.insert(block.span);
                    if is_loop {
                        loop_bodies.insert(block.span);
                    }
                }
            }
            _ => {}
        });
    }
    let encloses =
        |outer: Span, inner: Span| outer.start() <= inner.start() && inner.end() <= outer.end();
    let mut result = InPlaceReturns::default();
    for ret in returns {
        // The blocks enclosing the `^`, innermost first.
        let mut enclosing: Vec<Span> = blocks
            .iter()
            .copied()
            .filter(|b| encloses(*b, ret))
            .collect();
        enclosing.sort_by_key(|b| b.end() - b.start());
        if enclosing.iter().all(|b| in_place.contains(b)) {
            result.in_place.push(ret);
        } else if enclosing
            .iter()
            .find(|b| !in_place.contains(*b) || loop_bodies.contains(*b))
            .is_some_and(|b| loop_bodies.contains(b))
        {
            result.in_closure.push(ret);
        }
    }
    result
}

/// The [`InPlaceReturns::in_place`] returns of the REPL input `exprs` that
/// throw to the eval's boundary: every one but a `^` that is the input's
/// whole last statement (`x := 1. ^x`). That one already ends the input where
/// it stands, so it keeps its bare-value lowering and, in trace mode, the
/// steps of the statements before it.
#[must_use]
pub fn repl_eval_returns(exprs: &[Expression]) -> Vec<Span> {
    let last_root = match exprs.last() {
        Some(Expression::Return { span, .. }) => Some(*span),
        _ => None,
    };
    classify_in_place_returns(exprs)
        .in_place
        .into_iter()
        .filter(|span| Some(*span) != last_root)
        .collect()
}

/// ADR 0131 §1 "Transitive closure" (BT-3746): the outer locals a
/// local-threading construct writes, in source order (first write of each
/// name only). This is the construct's threaded set.
///
/// It holds the writes in the construct's own blocks, plus the writes of
/// every construct nested in them, transitively (o7/o8: an outer `on:do:`
/// carries a local written only inside a conditional arm's inner `on:do:`).
/// A nested construct counts whether or not its family is threaded today:
/// the set is what ADR 0131 threads, and
/// [`threaded_today_block_writes`] is the part threaded now. Any other
/// nested block is a closure, and its writes are not this construct's: the
/// §6 check deals with them. `bound_outside` and the scope rules are those
/// of [`outer_local_writes`](super::outer_local_writes).
#[must_use]
pub fn construct_outer_local_writes(
    construct: &LocalThreadingConstruct<'_>,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    threaded_block_writes(&construct.blocks, bound_outside)
}

/// [`construct_outer_local_writes`] over a given list of construct blocks.
#[must_use]
pub fn threaded_block_writes(
    blocks: &[&Block],
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    walk_construct_blocks(blocks, bound_outside, NestedBlocks::Producers)
}

/// [`threaded_block_writes`] closed only over nested constructs whose
/// construct [`LocalThreadingConstruct::is_threaded_today`]: the writes today's
/// lowering threads back. Codegen's tuple builders pack this set; ADR 0131
/// phases 2-4 grow it until it is [`threaded_block_writes`].
#[must_use]
pub fn threaded_today_block_writes(
    blocks: &[&Block],
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    walk_construct_blocks(blocks, bound_outside, NestedBlocks::ProducersThreadedToday)
}

/// ADR 0131 §1a: the outer locals written inside every local-threading
/// construct reachable from `expr` without crossing a closure boundary, in
/// source order (first write of each name only) — the union of the threaded
/// sets ([`construct_outer_local_writes`]) of `expr` itself if it is a
/// construct, of a construct passed as an argument or used as a receiver
/// anywhere inside it (`self id: (c ifTrue: [t := …] …)`, `(c ifTrue: [t := …]
/// …) + 0`), and transitively of the constructs nested in those. A block
/// operand that is not a construct block is a closure and is not entered;
/// an assignment in `expr` outside every construct block is `expr`'s own
/// write and is not included. `bound_outside` and the scope rules are those
/// of [`outer_local_writes`](super::outer_local_writes).
#[must_use]
pub fn expression_threaded_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 0,
        nested: NestedBlocks::Producers,
    };
    walker.expr(expr);
    walker.writes
}

/// ADR 0131 Phase 2 (BT-3749): a loop or fold in
/// [`crate::state_threading_selectors::producer_body_arg_index`]'s table, as
/// its selector and its literal body block, or `None`.
#[must_use]
pub fn producer_body_block(expr: &Expression) -> Option<(String, &Block)> {
    let Expression::MessageSend {
        selector: MessageSelector::Keyword(parts),
        arguments,
        is_cast: false,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
    let index = crate::state_threading_selectors::producer_body_arg_index(&sel, arguments.len())?;
    match arguments.get(index) {
        Some(Expression::Block(body)) => Some((sel, body)),
        _ => None,
    }
}

/// ADR 0131 Phase 2 (BT-3749, BT-3794): the literal blocks a loop or fold
/// producer packs into its construct tuple, in source order: the condition
/// block of `whileTrue:`/`whileFalse:` (the receiver), the body block
/// ([`producer_body_block`]) and, for `detect:ifNone:`, the `ifNone:` handler
/// ([`crate::state_threading_selectors::producer_handler_arg_index`]). Empty
/// when `expr` is not a producer send.
///
/// The one rule for what a producer packs: [`operand_producer_writes`] (the
/// Phase 0 allow-set check) and codegen's `lowered_construct_blocks` and fold
/// plans all read it, so the diagnostic and the lowering cannot pack
/// different sets.
#[must_use]
pub fn producer_packed_blocks(expr: &Expression) -> Vec<&Block> {
    let Some((sel, body)) = producer_body_block(expr) else {
        return Vec::new();
    };
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = expr.unwrap_parens()
    else {
        return Vec::new();
    };
    let mut packed: Vec<&Block> = Vec::with_capacity(3);
    if matches!(sel.as_str(), "whileTrue:" | "whileFalse:") {
        if let Expression::Block(cond) = receiver.as_ref() {
            packed.push(cond);
        }
    }
    packed.push(body);
    if let Some(index) =
        crate::state_threading_selectors::producer_handler_arg_index(&sel, arguments.len())
    {
        if let Some(Expression::Block(handler)) = arguments.get(index) {
            packed.push(handler);
        }
    }
    packed
}

/// BT-3855: the handler block of the producer `expr` when the producer runs it
/// inside its own body frame as the arm of a conditional statement
/// ([`crate::state_threading_selectors::producer_handler_is_body_arm`]:
/// `do:separatedBy:`'s separator), or `None`.
///
/// That conditional is a statement of the fold body, lowered by the body's
/// frame at the handler block's span: a lowering the producer performs for its
/// own construct, like the statements of its body, not a site of the sites
/// rule. Codegen's `do:separatedBy:` desugar gives the conditional this span,
/// and the producer agreement test reads the same rule.
#[must_use]
pub fn producer_body_arm_handler(expr: &Expression) -> Option<&Block> {
    let (sel, _) = producer_body_block(expr)?;
    let Expression::MessageSend { arguments, .. } = expr.unwrap_parens() else {
        return None;
    };
    if !crate::state_threading_selectors::producer_handler_is_body_arm(&sel, arguments.len()) {
        return None;
    }
    let index =
        crate::state_threading_selectors::producer_handler_arg_index(&sel, arguments.len())?;
    match arguments.get(index) {
        Some(Expression::Block(handler)) => Some(handler),
        _ => None,
    }
}

/// ADR 0131 Phase 2 (BT-3749): the threaded set of a loop or fold that
/// codegen lowers as a local-threading producer in operand position, or
/// `None` when `expr` is not one.
///
/// `expr` qualifies when it is a [`producer_body_block`] send whose
/// threaded set ([`construct_outer_local_writes`]) is not empty and equals
/// the set its tuple packs ([`threaded_today_block_writes`] over
/// [`producer_packed_blocks`]). A construct whose set is larger than what it
/// packs (a write inside a nested lookup or `tryDo:`) would drop the
/// difference, so it does not qualify.
///
/// Read by the Phase 0 allow-set check; codegen's producer applies the same
/// rule through `threaded_locals_of` (`ThreadedLocals::lowered ==
/// ThreadedLocals::names`).
#[must_use]
pub fn operand_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Option<Vec<OuterLocalWrite>> {
    let construct = local_threading_construct(expr)?;
    let packed = producer_packed_blocks(expr);
    if packed.is_empty() {
        return None;
    }
    packed_writes_agree(&construct, &packed, bound_outside)
}

/// The agreement tail of every producer rule: the construct's threaded set
/// ([`construct_outer_local_writes`]) is not empty and equals the set its
/// `packed` blocks pack (`threaded_today_block_writes`), so no write is dropped.
/// Returns the construct's set, or `None` when the rule fails.
fn packed_writes_agree(
    construct: &LocalThreadingConstruct<'_>,
    packed: &[&Block],
    bound_outside: &dyn Fn(&str) -> bool,
) -> Option<Vec<OuterLocalWrite>> {
    let names = |writes: &[OuterLocalWrite]| {
        let mut names: Vec<&str> = writes.iter().map(|w| w.name.as_str()).collect();
        names.sort_unstable();
        names.dedup();
        names.into_iter().map(str::to_string).collect::<Vec<_>>()
    };
    let all = construct_outer_local_writes(construct, bound_outside);
    let today = threaded_today_block_writes(packed, bound_outside);
    (!all.is_empty() && names(&all) == names(&today)).then_some(all)
}

/// ADR 0131 (BT-3841, BT-4263): the block of an *evaluated literal block*, a
/// positional `value`-family send to a block literal of the send's arity
/// (`[t := t + 1. t] value`, `[:y | t := y] value: 5`,
/// [`literal_block_value_send`]), or `None` for any other expression.
///
/// The **receiver rule** of this family: the receiver is a `[...]` literal
/// (parentheses not peeled: `([...]) value` is a block value, ADR 0131 §6), so
/// its class is statically `Block` and no other class's `value` can run; the
/// send is never dispatched, the block body is inlined where the send sits. A
/// stored or computed block (`b value`, `(f at: 1) value`) is not a literal and
/// keeps the Tier 2 protocol (`prescan_tier2_local_vars`) or the §6 error.
/// `valueWithArguments:` is not recognized here: its arguments are one runtime
/// list, not one per parameter.
///
/// BT-4263: the arguments of `value:`, `value:value:` … are operands of the
/// enclosing frame, evaluated in order **before** the block runs and bound to
/// its parameters (codegen's `generate_evaluated_block_producer`); the block
/// itself is the same arm that always runs, so the family is one construct
/// whatever its arity. A write to a parameter of the block is the parameter's
/// own (shadowed) binding, never an outer local's
/// ([`construct_outer_local_writes`]).
///
/// The one site rule for the family: read by [`conditional_arm_blocks`] (and so
/// by the Phase 0 check, the producer rule and the site rules) and by codegen's
/// producer, which lowers the block as a `BranchArm` frame that always runs.
#[must_use]
pub fn literal_block_value_arm(expr: &Expression) -> Option<&Block> {
    literal_block_value_send(expr).map(|(block, _)| block)
}

/// BT-3982: the block and arguments of an *evaluated literal block* of the
/// whole positional `value` family (`[...] value`, `[:y | ...] value: x`,
/// `value:value:`, `value:value:value:`, [`is_safe_value_family_selector`]) whose block literal's arity is the
/// send's ([`block_literal_arity_matches`]), or `None`. Parentheses around
/// the send are peeled, not around the receiver (the receiver rule of
/// [`literal_block_value_arm`]). `valueWithArguments:` is not one: its
/// arguments are one runtime list, not one per parameter.
///
/// The one recognizer codegen's evaluated-block producers read (actor
/// `State`, value-type `Self`, local-threading) and [`literal_block_value_arm`]
/// projects to its block.
#[must_use]
pub fn literal_block_value_send(expr: &Expression) -> Option<(&Block, &[Expression])> {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: false,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    let Expression::Block(block) = receiver.as_ref() else {
        return None;
    };
    (is_safe_value_family_selector(selector) && block_literal_arity_matches(block, arguments.len()))
        .then_some((block, arguments.as_slice()))
}

/// ADR 0131 Phase 3 (BT-3738): the literal arm blocks of a conditional
/// (`ifTrue:` … `ifNil:ifNotNil:`, `and:`/`or:`) whose every block position
/// holds a block literal, in source order, or `None` for any other send.
///
/// A conditional with a non-literal arm (`c ifTrue: aBlock`) is not inlined
/// by codegen (the arm is a closure value), so it is not a producer.
///
/// ADR 0131 Phase 2d (BT-3798): a [`dictionary_inline`] lookup
/// (`d at: k ifAbsent: [...]`) is a conditional in this sense: its handler is
/// an arm that runs only when the key is missing, lowered by the same
/// producer (a `BranchArm` frame, seeded set, `LocalRebind`s) behind a
/// runtime receiver guard. Its arm blocks are its handler blocks
/// ([`lookup_arm_blocks`]).
///
/// [`dictionary_inline`]: crate::state_threading_selectors::LookupShape::dictionary_inline
///
/// ADR 0131 (BT-3841, BT-4263): an evaluated literal block (`[...] value`,
/// `[:y | ...] value: x`, see [`literal_block_value_arm`]) is the degenerate
/// conditional: one arm that always runs. Its block is the single arm, so
/// every rule that reads this function (the conditional producer rule, the
/// site rules, the Phase 0 check and codegen's producer) treats it as an
/// inline arm-like frame.
#[must_use]
pub fn conditional_arm_blocks(expr: &Expression) -> Option<Vec<&Block>> {
    let construct = local_threading_construct(expr)?;
    if construct.family == LocalThreadingFamily::Lookup {
        return lookup_arm_blocks(expr);
    }
    if construct.family == LocalThreadingFamily::BlockValue {
        return literal_block_value_arm(expr).map(|block| vec![block]);
    }
    if construct.family != LocalThreadingFamily::Conditional {
        return None;
    }
    let Expression::MessageSend { arguments, .. } = expr.unwrap_parens() else {
        return None;
    };
    (!arguments.is_empty() && arguments.iter().all(|a| matches!(a, Expression::Block(_))))
        .then_some(construct.blocks)
}

/// ADR 0131 Phase 2d (BT-3798 review) and BT-3841: whether the receiver of a
/// `Dictionary`-inline send (a lookup, `keysAndValuesDo:`/`doWithKey:`) is
/// statically known NOT to be a `Dictionary`, so the send can only ever reach
/// another class's method of that name (`Ets`, a user class) and the runtime
/// guard (`beamtalk_map:lookup_for_threading/3`, `pairs_for_threading/2`) would
/// raise on every call (for a lookup, even when the key is present). Such a
/// send keeps its §6 compile error instead of becoming a producer; the guard is
/// for receivers whose class is not known (unannotated, `Dynamic`, or a class
/// that may hold a `Dictionary`).
///
/// The one rule the Phase 0 check reads for both families (codegen lowers every
/// send core's [`lookup_arm_blocks`] or keyed-iteration rule names and never sees a program Phase 0 rejects, so it
/// needs no copy of it). A receiver is statically non-`Dictionary` when it is
///
/// - `super`, or `self` where `self_is_dictionary` is false (the enclosing
///   class is not `Dictionary` or a subclass, or the method is class-side, whose
///   `self` is the class object);
/// - of an inferred type `ty` that is a known class (or the metatype of one)
///   that is neither `Dictionary`, a subclass of it, nor one of its ancestors
///   (`Object`, `Collection`, … may hold a `Dictionary`), or a union of such.
#[must_use]
pub fn receiver_statically_not_dictionary(
    receiver: &Expression,
    ty: Option<&InferredType>,
    self_is_dictionary: bool,
    hierarchy: &crate::semantic_analysis::ClassHierarchy,
) -> bool {
    match receiver.unwrap_parens() {
        Expression::Super(_) => return !self_is_dictionary,
        Expression::Identifier(id) if id.name == "self" => return !self_is_dictionary,
        _ => {}
    }
    ty.is_some_and(|ty| excludes(ty, hierarchy))
}

/// Whether `name` is a known class that can never hold a `Dictionary`.
fn class_excludes_dictionary(
    name: &str,
    hierarchy: &crate::semantic_analysis::ClassHierarchy,
) -> bool {
    hierarchy.has_class(name)
        && name != "Dictionary"
        && !hierarchy
            .superclass_chain(name)
            .iter()
            .any(|c| c == "Dictionary")
        && !hierarchy
            .superclass_chain("Dictionary")
            .iter()
            .any(|c| c == name)
}
/// [`class_excludes_dictionary`] over an inferred type.
fn excludes(ty: &InferredType, hierarchy: &crate::semantic_analysis::ClassHierarchy) -> bool {
    match ty {
        InferredType::Known { class_name, .. } => class_excludes_dictionary(class_name, hierarchy),
        InferredType::Meta { .. } => true,
        InferredType::Union { members, .. } => members.iter().all(|m| excludes(m, hierarchy)),
        _ => false,
    }
}

/// ADR 0131 Phase 2d (BT-3798): the handler blocks of a lookup send that
/// codegen lowers inline for a `Dictionary` receiver (a
/// [`dictionary_inline`](crate::state_threading_selectors::LookupShape::dictionary_inline)
/// row of [`crate::state_threading_selectors::LOOKUP_SHAPES`]), in source
/// order, or `None` for any other send.
///
/// The lowering needs every handler position to hold a block literal taking
/// the parameters the primitive calls it with (the row's
/// [`block_params`](crate::state_threading_selectors::LookupShape::block_params):
/// none for an absent handler, the found value for an `at:ifPresent:`
/// handler, which the lowering binds to the parameter), every
/// other argument to be an ordinary value (a block literal there is a key that
/// is a block value, not a handler), and a receiver that is not itself a block
/// literal. Anything else keeps the closure lowering, which the §6 check
/// rejects when its block writes an outer local.
#[must_use]
pub fn lookup_arm_blocks(expr: &Expression) -> Option<Vec<&Block>> {
    let construct = local_threading_construct(expr)?;
    if construct.family != LocalThreadingFamily::Lookup {
        return None;
    }
    let shape = crate::state_threading_selectors::lookup_shape(&construct.selector)?;
    if !shape.dictionary_inline {
        return None;
    }
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    let handler_is_arm = |i: usize, params: usize| matches!(arguments.get(i), Some(Expression::Block(b)) if b.parameters.len() == params);
    let values_are_not_blocks = arguments
        .iter()
        .enumerate()
        .all(|(i, a)| shape.block_args.contains(&i) || !matches!(a, Expression::Block(_)));
    (!matches!(receiver.as_ref(), Expression::Block(_))
        && values_are_not_blocks
        && shape
            .block_args
            .iter()
            .zip(shape.block_params)
            .all(|(&i, &params)| handler_is_arm(i, params)))
    .then_some(construct.blocks)
}

/// ADR 0131 Phase 3b (BT-3817): the literal blocks of an `on:do:` / `ensure:`
/// send whose every block position holds a block literal, in source order
/// (the protected body, then the handler or cleanup), or `None` for any other
/// send. A construct with a non-literal protected body or handler
/// (`aBlock on: Error do: aHandler`) is not inlined by codegen, so it is not a
/// producer.
///
/// ADR 0131 Phase 4 (BT-3750): and the one block of a `tryDo:` send whose
/// argument is a block literal, the other exception-like catch boundary. It
/// is not inlined into a `try`: it compiles to a `fun(StateAcc)` handed to
/// `beamtalk_result:'tryDo:'/2`, whose body is a `BranchArm` frame like any
/// other arm of an exception producer.
#[must_use]
pub fn exception_arm_blocks(expr: &Expression) -> Option<Vec<&Block>> {
    let construct = local_threading_construct(expr)?;
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = expr.unwrap_parens()
    else {
        return None;
    };
    match construct.family {
        LocalThreadingFamily::Exception => {
            if !construct.inlined {
                return None;
            }
            let &[handler_index] =
                crate::state_threading_selectors::state_threaded_block_arg_indices(
                    &construct.selector,
                )
            else {
                return None;
            };
            // `inlined` ([`exception_send_inlined`]) holds: both are literals.
            match (receiver.as_ref(), arguments.get(handler_index)) {
                (Expression::Block(protected), Some(Expression::Block(handler))) => {
                    Some(vec![protected, handler])
                }
                _ => None,
            }
        }
        // Only the `Result` class lowers to `beamtalk_result:'tryDo:'/2`: any
        // other receiver keeps its own `tryDo:` and a dynamic send.
        LocalThreadingFamily::TryDo => match arguments.as_slice() {
            [Expression::Block(body)] if is_result_try_do(receiver, &construct.selector) => {
                Some(vec![body])
            }
            _ => None,
        },
        _ => None,
    }
}

/// The kind of method body a method-root local-threading producer sits in
/// (ADR 0131 §2's frame modes). The rules for which conditionals are
/// producers depend on it ([`conditional_arms_lower_as_producer`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RootMethodKind {
    /// An actor instance method: `State` is a family, so a conditional's
    /// scratch map continues it.
    Actor,
    /// A class-side method: no `State`, the scratch map is private.
    Class,
    /// An instance method of a non-actor class: no `State`, the scratch map
    /// is private, and a field write is a second family.
    ValueType,
}

/// ADR 0131 Phase 3 (BT-3738): whether the arms of a conditional can be
/// lowered by the conditional producer in a method of kind `kind`.
///
/// In a class or value-type method a conditional's scratch map is private
/// (a fresh map seeded with the construct's own locals, ADR 0131 §3), and each
/// arm is a `BranchArm` frame (§2): a loop or fold in an arm rebinds its
/// locals into the arm by a `LocalRebind`, which is a producer at one of the
/// arm statement's [`loop_body_producer_sites`] (narrowed by
/// [`is_loop_body_producer_site`], the rule a loop body uses), and so is a
/// nested conditional or `on:do:`/`ensure:` that is itself such a producer. A
/// construct in an arm that writes an outer local, or a local the arm itself
/// defines, and is not such a producer would rebuild the arm's `StateAcc` from
/// scratch and drop its other keys: the conditional is not a producer then, and
/// keeps its statement lowering. In a value-type method an arm that writes a
/// field carries a second family the producer's tuple does not. An actor method
/// has neither restriction: its `State` map is continued, never rebuilt.
///
/// `bound_outside` is the scope at the conditional; the names the arms assign
/// are bound inside them. One rule, read by the Phase 0 check and by codegen's
/// producer predicate.
#[must_use]
pub fn conditional_arms_lower_as_producer(
    arms: &[&Block],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
) -> bool {
    arms_lower_as_producer(arms, kind, bound_outside, false)
}

/// ADR 0131 Phase 3b (BT-3817): whether the protected body and handler or
/// cleanup of an `on:do:`/`ensure:` can be lowered by the exception producer
/// in a method of kind `kind`. The rule of
/// [`conditional_arms_lower_as_producer`], except that a value-type arm may
/// write a field: an exception construct's result tuple carries the `SelfVt`
/// family in a trailing slot of its own (ADR 0122, BT-3506), which the producer
/// extracts after the construct.
#[must_use]
pub fn exception_arms_lower_as_producer(
    arms: &[&Block],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
) -> bool {
    arms_lower_as_producer(arms, kind, bound_outside, true)
}

/// The one rule behind [`conditional_arms_lower_as_producer`] and
/// [`exception_arms_lower_as_producer`]; `field_writes_carried` says whether a
/// value-type arm's field write rides the construct's own tuple.
fn arms_lower_as_producer(
    arms: &[&Block],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
    field_writes_carried: bool,
) -> bool {
    if kind == RootMethodKind::Actor {
        return true;
    }
    // The names the arm itself binds: assigned by one of its statements, not
    // inside a block nested in them. A block's own first assignment binds the
    // name in that block (`_b := [i := 0. [i < 3] whileTrue: [i := i + 1]]`
    // stores a closure whose `i` is not the arm's), so a construct writing
    // only such a name writes no local of the arm.
    let mut assigned: HashSet<ecow::EcoString> = HashSet::new();
    for arm in arms {
        for stmt in &arm.body {
            assigned.extend(names_assigned_outside_nested_blocks(&stmt.expression));
        }
    }
    let arm_bound = |name: &str| bound_outside(name) || assigned.contains(name);
    for arm in arms {
        for stmt in &arm.body {
            if !arm_statement_lowers(&stmt.expression, kind, &arm_bound, field_writes_carried) {
                return false;
            }
        }
    }
    true
}

/// [`conditional_arms_lower_as_producer`] for one arm statement.
fn arm_statement_lowers(
    stmt: &Expression,
    kind: RootMethodKind,
    arm_bound: &dyn Fn(&str) -> bool,
    field_writes_carried: bool,
) -> bool {
    let ctx = LoopBodyContext::new(stmt, false, false, false).in_arm();
    let sites: std::collections::HashSet<Span> = loop_body_producer_sites(stmt)
        .into_iter()
        .filter(|site| is_loop_body_producer_site(site, &ctx))
        .map(Expression::span)
        .collect();
    // The producers of the statement: constructs at its sites that are
    // loop/fold producers. A construct inside one is its own frame's business.
    let mut producers: Vec<Span> = Vec::new();
    crate::ast_walker::walk_expression(stmt, &mut |e| {
        if sites.contains(&e.span())
            && (operand_producer_writes(e, arm_bound).is_some()
                || conditional_producer_writes(e, arm_bound, kind).is_some()
                || exception_producer_writes(e, arm_bound, kind).is_some()
                || match_producer_writes(e, arm_bound, kind).is_some())
        {
            producers.push(e.span());
        }
    });
    let inside_producer = |span: Span| {
        producers
            .iter()
            .any(|p| p.start() <= span.start() && span.end() <= p.end())
    };
    let mut ok = true;
    crate::ast_walker::walk_expression(stmt, &mut |e| {
        // (`local_threading_construct` peels parentheses, so a `Parenthesized`
        // node would be judged as its construct, with a span wider than the
        // producer's own.)
        if matches!(e, Expression::Parenthesized { .. }) {
            return;
        }
        if let Some(construct) = local_threading_construct(e) {
            if !inside_producer(e.span())
                && !construct_outer_local_writes(&construct, arm_bound).is_empty()
            {
                ok = false;
            }
        }
        // BT-3947: a nested `match:` that writes an outer local and is not a
        // producer here would drop the write, like any construct.
        if let Expression::Match { arms, .. } = e {
            if !inside_producer(e.span()) && !match_arm_body_writes(arms, arm_bound).is_empty() {
                ok = false;
            }
        }
        if kind == RootMethodKind::ValueType && !field_writes_carried {
            if let Expression::Assignment { target, .. } = e {
                if is_self_field_target(target) {
                    ok = false;
                }
            }
        }
    });
    ok
}

/// ADR 0131 Phase 3 (BT-3738): the threaded set of a conditional that
/// codegen lowers as a local-threading producer at a method-root site
/// ([`sequenced_operand_sites`], a method-body assignment's whole value, a
/// method-body `^` value), or `None` when `expr` is not one.
///
/// The conditional counterpart of [`operand_producer_writes`], with the same
/// agreement rule: the construct writes an outer local, and the set its
/// arms pack (`threaded_today_block_writes` over [`conditional_arm_blocks`])
/// is the whole set ([`construct_outer_local_writes`]), so no write is
/// dropped. Read by the Phase 0 allow-set check; codegen's producer applies
/// the same rule through `threaded_locals_of` (`ThreadedLocals::lowered ==
/// ThreadedLocals::names`).
#[must_use]
pub fn conditional_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
    kind: RootMethodKind,
) -> Option<Vec<OuterLocalWrite>> {
    let arms = conditional_arm_blocks(expr)?;
    if !conditional_arms_lower_as_producer(&arms, kind, bound_outside) {
        return None;
    }
    let construct = local_threading_construct(expr)?;
    packed_writes_agree(&construct, &arms, bound_outside)
}

/// ADR 0131 Phase 3b (BT-3817): the threaded set of an `on:do:`/`ensure:` that
/// codegen lowers as a local-threading producer at a method-root site (or in an
/// arm frame), or `None` when `expr` is not one.
///
/// The exception counterpart of [`conditional_producer_writes`], with the same
/// agreement rule: the protected body and the handler or cleanup are block
/// literals ([`exception_arm_blocks`]), the construct writes an outer local, the
/// set its blocks pack (`threaded_today_block_writes` over them) is the whole
/// set ([`construct_outer_local_writes`]) so no write is dropped, and the
/// blocks are ones the producer can lower in this kind of method
/// ([`exception_arms_lower_as_producer`]). Read by the Phase 0 allow-set check;
/// codegen's producer applies the same rule through `threaded_locals_of`
/// (`ThreadedLocals::lowered == ThreadedLocals::names`).
#[must_use]
pub fn exception_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
    kind: RootMethodKind,
) -> Option<Vec<OuterLocalWrite>> {
    let arms = exception_arm_blocks(expr)?;
    if !exception_send_lowers_as_producer(expr, &arms, kind, bound_outside) {
        return None;
    }
    let construct = local_threading_construct(expr)?;
    if construct_outer_local_writes(&construct, bound_outside).is_empty() {
        // No outer local is written. A value-type exception that writes only
        // `self.field` still threads the `Self` family (its result tuple's
        // trailing slot, ADR 0122), so it is a producer with an empty set and
        // no `LocalRebind`s; anything else is not a producer. (`tryDo:`'s
        // Tier 2 block threads no field: it never is.)
        return (construct.family == LocalThreadingFamily::Exception
            && kind == RootMethodKind::ValueType
            && arms_write_self_field(&arms))
        .then(Vec::new);
    }
    packed_writes_agree(&construct, &arms, bound_outside)
}

/// ADR 0131 Phase 4 (BT-3750): whether the blocks `arms` of the exception-like
/// send `expr` ([`exception_arm_blocks`]) can be lowered by the exception
/// producer in a method of kind `kind`: [`exception_arms_lower_as_producer`]
/// for `on:do:`/`ensure:`, [`conditional_arms_lower_as_producer`] for `tryDo:`'s one block. The one
/// predicate the Phase 0 check and codegen's `exception_producer_locals` read.
#[must_use]
pub fn exception_send_lowers_as_producer(
    expr: &Expression,
    arms: &[&Block],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
) -> bool {
    match local_threading_construct(expr).map(|c| c.family) {
        // The block is a `BranchArm` frame whose `StateAcc` the runtime's
        // `tryDo:/2` threads, so the rule is a conditional arm's: a nested
        // construct that writes a local must itself be a producer, and a
        // value-type block writes no field (its tuple carries no `Self`).
        Some(LocalThreadingFamily::TryDo) => {
            matches!(arms, [body] if body.parameters.is_empty())
                && conditional_arms_lower_as_producer(arms, kind, bound_outside)
        }
        _ => exception_arms_lower_as_producer(arms, kind, bound_outside),
    }
}

/// Whether any statement of `arms` assigns a `self.field`.
fn arms_write_self_field(arms: &[&Block]) -> bool {
    let mut found = false;
    for arm in arms {
        for stmt in &arm.body {
            crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
                if let Expression::Assignment { target, .. } = e {
                    found |= is_self_field_target(target);
                }
            });
        }
    }
    found
}

/// ADR 0131 Phase 3 (BT-3738): [`operand_producer_writes`],
/// [`conditional_producer_writes`] or [`exception_producer_writes`]: the
/// threaded set of a local-threading producer at a method-root site, whichever
/// family it is.
#[must_use]
pub fn root_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
    kind: RootMethodKind,
) -> Option<Vec<OuterLocalWrite>> {
    operand_producer_writes(expr, bound_outside)
        .or_else(|| conditional_producer_writes(expr, bound_outside, kind))
        .or_else(|| exception_producer_writes(expr, bound_outside, kind))
        .or_else(|| match_producer_writes(expr, bound_outside, kind))
}

/// BT-3947 (closes BT-3875): the arms of the `match:` expression `expr`
/// (parentheses peeled), or `None` for any other expression.
///
/// A `match:` is a local-threading construct whose arms are *expressions*, not
/// blocks, so [`local_threading_construct`] cannot name it. Its arm is the same
/// frame a conditional's block is, though: [`match_arm_block`] gives the arm as
/// that one-statement block, and every rule below reads those blocks.
#[must_use]
pub fn match_arms(expr: &Expression) -> Option<&[MatchArm]> {
    match expr.unwrap_parens() {
        Expression::Match { arms, .. } => Some(arms),
        _ => None,
    }
}

/// BT-3947: the arm of a `match:` as a block whose one statement is the arm's
/// body and whose parameters are the arm pattern's bindings (the names the arm
/// binds for itself, `extract_match_arm_bindings`). The frame the arm is
/// lowered as; spans are the arm's own, so a producer site in the body keeps
/// its span; the block's own is the arm's.
#[must_use]
pub fn match_arm_block(arm: &MatchArm) -> Block {
    let (bindings, _) = crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
    Block::new(
        bindings
            .into_iter()
            .map(|id| BlockParameter::new(id.name, id.span))
            .collect(),
        vec![ExpressionStatement::bare(arm.body.clone())],
        arm.span,
    )
}

/// The outer locals the arms of a `match:` write, closed over `nested` blocks as
/// [`threaded_block_writes`] is. The walk is the one every construct block gets
/// ([`OuterWriteWalker`]): an arm's pattern bindings are bound inside it, and its
/// guard is walked with it.
fn match_arm_writes_in(
    arms: &[MatchArm],
    bound_outside: &dyn Fn(&str) -> bool,
    nested: NestedBlocks,
    with_guards: bool,
) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 1,
        nested,
    };
    for arm in arms {
        let (ids, _) = crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
        walker
            .frames
            .push(ids.into_iter().map(|id| id.name).collect());
        if with_guards {
            if let Some(guard) = &arm.guard {
                walker.expr(guard);
            }
        }
        walker.expr(&arm.body);
        walker.frames.pop();
    }
    walker.writes
}

/// BT-3947: the outer locals the arm *bodies* of a `match:` write, closed over
/// the nested constructs of its arms ([`threaded_block_writes`]'s rule).
#[must_use]
pub fn match_arm_body_writes(
    arms: &[MatchArm],
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    match_arm_writes_in(arms, bound_outside, NestedBlocks::Producers, false)
}

/// BT-3947: the outer locals the *guards* of a `match:` write. A guard runs
/// before its arm is chosen, in no frame of the arm's, so a write there is never
/// threaded: it keeps the `match:` arm error.
#[must_use]
pub fn match_guard_writes(
    arms: &[MatchArm],
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 1,
        nested: NestedBlocks::Producers,
    };
    for arm in arms {
        let (ids, _) = crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
        walker
            .frames
            .push(ids.into_iter().map(|id| id.name).collect());
        if let Some(guard) = &arm.guard {
            walker.expr(guard);
        }
        walker.frames.pop();
    }
    walker.writes
}

/// BT-3947 (closes BT-3875): whether the arms of a `match:` can be lowered by the
/// `match:` producer in a method of kind `kind`: [`conditional_arms_lower_as_producer`]'s
/// rule over the arms as blocks ([`match_arm_block`]). Read by the Phase 0 check and
/// by codegen's producer predicate.
#[must_use]
pub fn match_arms_lower_as_producer(
    arms: &[MatchArm],
    kind: RootMethodKind,
    bound_outside: &dyn Fn(&str) -> bool,
) -> bool {
    let blocks: Vec<Block> = arms.iter().map(match_arm_block).collect();
    let refs: Vec<&Block> = blocks.iter().collect();
    // An arm binds its pattern variables for itself, on top of the names the
    // scope holds: they are the block's parameters, so the shared rule sees
    // them as the arm's own.
    arms_lower_as_producer(&refs, kind, bound_outside, false)
}

/// BT-3947 (closes BT-3875): the threaded set of a `match:` that codegen lowers
/// as a local-threading producer (a conditional with one arm per `match:` arm,
/// [`conditional_producer_writes`]'s counterpart), or `None` when `expr` is not
/// one: it has at least one arm that writes an outer local, no guard writes one,
/// the set its arms pack (the nested constructs threaded today) is the whole set
/// so no write is dropped, and the arms are ones the producer can lower in this
/// kind of method ([`match_arms_lower_as_producer`]).
#[must_use]
pub fn match_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
    kind: RootMethodKind,
) -> Option<Vec<OuterLocalWrite>> {
    let arms = match_arms(expr)?;
    // Any assignment in the subject keeps the `match:` the loud `MatchArmWrite`
    // error. Codegen compiles the subject before it seeds the arms' scratch map
    // (BT-4196), but, unlike a conditional's receiver, the subject is not a
    // producer site: a local assigned there is not lowered as a `LocalRebind`,
    // so even an unrelated one is an unthreaded assignment (the verifier's
    // `UnthreadedLocalAssignment`).
    if let Expression::Match { value, .. } = expr.unwrap_parens() {
        let mut assigns = false;
        crate::ast_walker::walk_expression(value, &mut |e| {
            assigns |= matches!(e, Expression::Assignment { .. });
        });
        if assigns {
            return None;
        }
    }
    if !match_guard_writes(arms, bound_outside).is_empty()
        || !match_arms_lower_as_producer(arms, kind, bound_outside)
    {
        return None;
    }
    let all = match_arm_body_writes(arms, bound_outside);
    let today = match_arm_writes_in(
        arms,
        bound_outside,
        NestedBlocks::ProducersThreadedToday,
        false,
    );
    let names = |writes: &[OuterLocalWrite]| {
        let mut names: Vec<&str> = writes.iter().map(|w| w.name.as_str()).collect();
        names.sort_unstable();
        names.dedup();
        names.into_iter().map(str::to_string).collect::<Vec<_>>()
    };
    (!all.is_empty() && names(&all) == names(&today)).then_some(all)
}

/// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): the threaded set of `expr`
/// when it is a local-threading producer at a REPL site (an operand site of
/// the input, [`repl_operand_sites`](super::repl_operand_sites), or a REPL arm site,
/// [`repl_arm_producer_sites`]), or `None`.
///
/// The REPL threads its bindings map, so its producer adopts the construct's
/// next map whole whatever the family (codegen's `lower_repl_construct_tuple`):
/// a loop, fold, conditional or exception construct by the method rule
/// ([`root_producer_writes`], read as in an actor method, whose `State` the
/// bindings map plays), and a `value`-family send to a block literal with or
/// without parameters (`[:k | t := t + k] value: 4`), whose block the REPL
/// inlines over the map. A lookup is not one: the REPL keeps its §6 rejection.
/// The one rule the Phase 0 check and codegen's REPL producer both read.
#[must_use]
pub fn repl_producer_writes(
    expr: &Expression,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Option<Vec<OuterLocalWrite>> {
    let inner = expr.unwrap_parens();
    // BT-3947: a `match:` whose arms write a binding (no guard does) adopts the
    // bindings map like a REPL conditional.
    if let Some(arms) = match_arms(inner) {
        if !match_guard_writes(arms, bound_outside).is_empty() {
            return None;
        }
        let writes = match_arm_body_writes(arms, bound_outside);
        return (!writes.is_empty()).then_some(writes);
    }
    let construct = local_threading_construct(inner)?;
    match construct.family {
        LocalThreadingFamily::Lookup => None,
        LocalThreadingFamily::BlockValue => {
            repl_evaluated_block(inner)?;
            let writes = construct_outer_local_writes(&construct, bound_outside);
            (!writes.is_empty()).then_some(writes)
        }
        _ => root_producer_writes(inner, bound_outside, RootMethodKind::Actor),
    }
}

/// BT-3881: whether a `value`-family send with `arity` arguments to the block
/// literal `block` may inline it: only when the arity is the block's own. A
/// block literal sent a selector of another arity (`[t := t + 1] value: 1`,
/// `[:k | t := t + k] value`) is never inlined, in the REPL (root or arm) or
/// in a class, value-type or actor method: it is applied, so it raises the
/// same structured `arity_mismatch` error as any other block called with the
/// wrong number of arguments, and its body never runs
/// ([`block_literal_of_other_arity`]). The one arity rule every inlining of a
/// block literal `value` send reads.
#[must_use]
pub fn block_literal_arity_matches(block: &Block, arity: usize) -> bool {
    block.parameters.len() == arity
}

/// BT-3884: the block literal `receiver` when it is sent the `value`-family
/// `selector` ([`is_safe_value_family_selector`]) with `arity` arguments and
/// its arity is not the send's ([`block_literal_arity_matches`]), or `None`.
/// Such a send is applied, never inlined: codegen lowers it through
/// `generate_block_arity_mismatch`, a stand-in fun of the block's own arity
/// whose application raises the structured `arity_mismatch` error, and the
/// body never runs. So it is no [`LocalThreadingFamily::BlockValue`]
/// construct ([`local_threading_construct`]), and the ADR 0133 escaping-block
/// check reports no write of it. The one predicate codegen's lowering, the
/// recognizer and the escape check read.
#[must_use]
pub fn block_literal_of_other_arity<'a>(
    receiver: &'a Expression,
    selector: &MessageSelector,
    arity: usize,
) -> Option<&'a Block> {
    // BT-3942: a parenthesized literal (`([t := t + 1]) value: 1`) is the
    // same literal, so it is applied and raises `arity_mismatch` alike: the
    // send receiver reaches here operand-normalised (BT-4059, BT-4238).
    match receiver {
        Expression::Block(block)
            if is_safe_value_family_selector(selector)
                && !block_literal_arity_matches(block, arity) =>
        {
            Some(block)
        }
        _ => None,
    }
}

/// BT-3823: the block of an *evaluated block* in the REPL sense: a
/// `value`-family send to a bare block literal (`[...] value`,
/// `[:k | ...] value: x`) whose arity matches the block's
/// ([`block_literal_arity_matches`]), or `None`. The REPL inlines such a block
/// over its bindings map when it writes a binding (a REPL arm); a block sent a
/// selector of another arity is applied instead, and raises `arity_mismatch`
/// at the root and in an arm alike (BT-3881). The one rule the Phase 0 check
/// and codegen read.
#[must_use]
pub fn repl_evaluated_block(expr: &Expression) -> Option<&Block> {
    local_threading_construct(expr)
        .filter(|construct| construct.family == LocalThreadingFamily::BlockValue)
        .and_then(|construct| construct.blocks.first().copied())
}

/// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): the sub-expressions of
/// `stmt`, a statement of a *REPL arm* — a block the REPL inlines over its
/// bindings map (the arms of a conditional, the protected body, handler and
/// cleanup of an `on:do:`/`ensure:`, the block of a `Result tryDo:`) — where a
/// local-threading producer adopts that map: the statement's
/// [`sequenced_operand_sites`], and the statement itself when it is a
/// local-threading construct ([`local_threading_construct`]; whether it is a
/// producer there is [`repl_producer_writes`]). The arm's `StateAcc` *is* the
/// bindings map, so a producer there continues it exactly as one at an operand
/// site of the input does (codegen's `lower_repl_construct_tuple`).
///
/// Unlike the input root ([`repl_operand_sites`](super::repl_operand_sites)), the whole value of an
/// assignment is a site (no assembler unpacks it in an arm) and so are
/// `eachWithIndex:`/`do:separatedBy:` (measured right in every REPL arm,
/// BT-3823). The value of a `self.f := V` write is not: a field write has no
/// REPL meaning.
///
/// The one rule the Phase 0 check and codegen's REPL arms
/// (`with_repl_arm_producer_sites`) both read.
#[must_use]
pub fn repl_arm_producer_sites(stmt: &Expression) -> Vec<&Expression> {
    let inner = stmt.unwrap_parens();
    if let Expression::Assignment { target, .. } = inner {
        if is_self_field_target(target) {
            return Vec::new();
        }
    }
    let mut sites = sequenced_operand_sites(stmt);
    if local_threading_construct(inner).is_some() || match_arms(inner).is_some() {
        sites.push(inner);
    }
    sites
}
