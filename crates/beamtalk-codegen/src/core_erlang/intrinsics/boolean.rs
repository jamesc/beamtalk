// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Boolean conditional intrinsics.
//!
//! Lowers `ifTrue:`/`ifFalse:`/`and:`/`or:`/`not` and the nil-conditional
//! forms inline, including the pure-boolean shapes and the mutation-threading
//! decisions for conditional blocks.
//!
//! **DDD Context:** Compilation — Code Generation

use super::{
    Block, CodeGenContext, CoreErlangGenerator, Document, Expression, MessageSelector, PureArm,
    PureBooleanShape, Result, SoloAnswer, SoloBoolean, WellKnownSelector,
};

impl CoreErlangGenerator {
    /// Tries to generate inline code for Boolean conditionals with field mutation
    /// state threading.
    ///
    /// Handles `ifTrue:`, `ifFalse:`, and `ifTrue:ifFalse:` in actor context when at
    /// least one block argument contains field mutations (`self.slot :=`).
    ///
    /// - Returns `Ok(Some(doc))` generating `{Result, NewState}` tuple when mutations detected
    /// - Returns `Ok(None)` otherwise (fall through to `beamtalk_message_dispatch:send`)
    #[allow(
        clippy::too_many_lines,
        reason = "one arm per conditional selector, each with its load-bearing rationale comment"
    )]
    pub(in crate::core_erlang) fn try_generate_boolean_protocol(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        use super::super::CodeGenContext;

        // BT-4070: pure-inline for literal, non-mutating blocks — BEFORE the
        // Actor/Repl/loop-body early return below. That context gate exists
        // for the state-threading mutation path; pure-inline is
        // state-transparent in every context (no field/local writes, no
        // self-sends, no threading receiver, guaranteed by
        // `!needs_threading`), so it runs in value-type and class methods
        // too, wherever the send reaches this handler.
        if let Some(doc) =
            self.try_generate_pure_boolean_conditional(receiver, selector, arguments)?
        {
            return Ok(Some(doc));
        }

        // Only applies in actor context, REPL context, or when inside a loop body
        // (value-type methods may have inline conditionals mutating captured
        // locals inside do: blocks).
        // Allow REPL context — the REPL State map threads bindings the same
        // way as actor State, so inline case expressions work directly.
        if self.context != CodeGenContext::Actor
            && self.context != CodeGenContext::Repl
            && !self.in_loop_body
        {
            return Ok(None);
        }

        // `and:`/`or:` short-circuit boolean protocol. Both are
        // ordinary self-hosted `Boolean` methods (`boolean.bt`: `and: aBlock
        // => self ifTrue: aBlock ifFalse: [false]`, `or: aBlock => self
        // ifTrue: [true] ifFalse: aBlock`) — not `WellKnownSelector`s (see
        // `state_threading_selectors`'s doc comment), so generic dispatch
        // reaches them with no special-casing at all today, and the block
        // literal argument compiles as an ordinary Tier 1 closure. A
        // self-send (or `self.slot :=` field mutation) nested inside that
        // closure then discards its own `NewState` exactly like a plain
        // `ifTrue:`/`ifFalse:` closure would — see this function's `IfTrue`/`IfFalse`
        // arms below, which this mirrors. Recognizing the literal `and:`/
        // `or:` call shape here, before generic dispatch (and boolean.bt's
        // self-hosted definition) is ever reached, lets the block compile
        // through the same `generate_conditional_branch_inline` machinery
        // `ifTrue:`/`ifFalse:` use, so nested self-sends and field
        // mutations thread state correctly. Only intercepts when threading
        // is actually needed; otherwise falls through to ordinary generic
        // dispatch so the ordinary (non-mutating) codegen shape is
        // unchanged.
        if let MessageSelector::Keyword(parts) = selector
            && parts.len() == 1
            && arguments.len() == 1
        {
            let kw = parts[0].keyword.as_str();
            if (kw == "and:" || kw == "or:")
                && let Expression::Block(block) = &arguments[0]
            {
                // Same threading gate as IfTrue/IfFalse below, shared
                // with `compile_conditional_receiver` so the two can never disagree about
                // whether a given `and:`/`or:` needs the inline
                // mutation-threading path — see
                // `and_or_needs_mutation_threading`'s doc comment.
                let needs_threading = self.and_or_needs_mutation_threading(receiver, block);
                if needs_threading {
                    if self.is_repl_mode() {
                        self.set_repl_loop_mutated(true);
                    }
                    let sel = if kw == "and:" {
                        SoloBoolean::And
                    } else {
                        SoloBoolean::Or
                    };
                    let doc = self.generate_solo_boolean_with_mutations_tuple(
                        sel,
                        receiver,
                        block,
                        self.conditional_scratch(self.current_frame()),
                        false,
                    )?;
                    return Ok(Some(doc));
                }
            }
        }

        match selector.well_known() {
            Some(WellKnownSelector::IfTrue) => {
                debug_assert_eq!(arguments.len(), 1);
                if let Expression::Block(block) = &arguments[0] {
                    // When inside a loop body, also trigger for any local write
                    // (the outer loop has already determined which locals need threading).
                    // also inline when the branch contains a nested list op that
                    // mutates an outer local — otherwise the conditional falls through to a
                    // runtime `send` whose plain (untupled) result breaks the `{Value, State}`
                    // contract the method-body sequencer expects (badarg on element/2).
                    // also inline when the RECEIVER itself is an actor self-send
                    // (`(self recordOnce: x) ifTrue:...`) — its own state mutation must be
                    // threaded through before evaluating either branch, even when neither
                    // block contains a mutation of its own — this also widens to any
                    // hoistable self-send in the receiver's sub-tree (`((self recordOnce:
                    // x) and: [y]) ifTrue:`) via the same walker
                    // `compile_conditional_receiver` emits with. See that function's doc
                    // comment for how the receiver position threads it.
                    let needs_threading =
                        self.conditional_needs_mutation_threading(receiver, &[block]);
                    if needs_threading {
                        // Set repl_loop_mutated so the REPL unpacks {Result, State}
                        if self.is_repl_mode() {
                            self.set_repl_loop_mutated(true);
                        }
                        let doc = self.generate_solo_boolean_with_mutations_tuple(
                            SoloBoolean::IfTrue,
                            receiver,
                            block,
                            self.conditional_scratch(self.current_frame()),
                            false,
                        )?;
                        return Ok(Some(doc));
                    }
                }
            }
            Some(WellKnownSelector::IfFalse) => {
                debug_assert_eq!(arguments.len(), 1);
                if let Expression::Block(block) = &arguments[0] {
                    // inline when a nested list op mutates an outer local (see IfTrue).
                    // also inline for a self-send receiver (see IfTrue above).
                    let needs_threading =
                        self.conditional_needs_mutation_threading(receiver, &[block]);
                    if needs_threading {
                        if self.is_repl_mode() {
                            self.set_repl_loop_mutated(true);
                        }
                        let doc = self.generate_solo_boolean_with_mutations_tuple(
                            SoloBoolean::IfFalse,
                            receiver,
                            block,
                            self.conditional_scratch(self.current_frame()),
                            false,
                        )?;
                        return Ok(Some(doc));
                    }
                }
            }
            Some(WellKnownSelector::IfTrueIfFalse) => {
                debug_assert_eq!(arguments.len(), 2);
                if let (Expression::Block(true_block), Expression::Block(false_block)) =
                    (&arguments[0], &arguments[1])
                {
                    // inline when either branch has a nested list op mutating an
                    // outer local (see IfTrue).
                    // also inline for a self-send receiver (see IfTrue above).
                    let needs_threading = self
                        .conditional_needs_mutation_threading(receiver, &[true_block, false_block]);
                    if needs_threading {
                        if self.is_repl_mode() {
                            self.set_repl_loop_mutated(true);
                        }
                        let doc = self.generate_if_true_if_false_with_mutations_tuple(
                            receiver,
                            true_block,
                            false_block,
                            self.conditional_scratch(self.current_frame()),
                            false,
                        )?;
                        return Ok(Some(doc));
                    }
                }
            }
            _ => {}
        }

        Ok(None)
    }

    /// BT-4070: pure-inline gate for `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`
    /// and `and:`/`or:` with literal, non-mutating blocks — the check
    /// [`Self::try_generate_boolean_protocol`] runs before its
    /// Actor/Repl/loop-body early return. Fires only when the existing
    /// `needs_threading` check is false (the same predicate the mutation
    /// path reads, so the two halves can never disagree about what
    /// "pure" means) AND every block argument is a direct, zero-arity
    /// literal `Block` — the same direct-match style as the mutation
    /// gate (no paren-unwrapping). An arity-≠-0 block keeps today's
    /// runtime arity-error behavior by falling through to dynamic
    /// dispatch, as does any shape this does not name (`xor:`, the
    /// `ifNil:` family, non-literal blocks).
    ///
    /// Never fires on a bare-`self`, `super`, or class-reference receiver:
    /// those sends are (or may be) intercepted by the self/super/class
    /// dispatch paths — in a class method a runtime `send/3` to the
    /// class's own process would deadlock where today's direct class-side
    /// call does not (the BT-3985/BT-4005 hazard class) — so the fallback
    /// arm's dynamic send would not be behavior-identical there. (`super`
    /// is also intercepted before the handler chain ever runs; excluding
    /// it here is defense in depth.)
    ///
    /// Never fires while [`Self::decline_pure_boolean_inline`] is set (the
    /// non-Boolean fallback closures — see the field's doc comment).
    ///
    /// Never fires in an actor instance method when a branch block holds a
    /// guarded operator or another call codegen brackets (see
    /// [`Self::pure_blocks_hold_bracketed_call`]): the
    /// arm compiles via `generate_block_body`, whose closure frame carries
    /// the `Loop(DirectParams)` kind — and the ADR 0137 bracket machinery
    /// reads that kind as a stateless loop, so the operator's slow path
    /// would flag a loop marker that was never pushed (or skip its
    /// `SyncedCall`), tripping `UnbracketedGuardedOperator`. The receiver
    /// is exempt: it compiles in the enclosing (truthful) frame via
    /// `compile_conditional_receiver`, so its operators bracket normally.
    fn try_generate_pure_boolean_conditional(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        let Some(shape) = self.pure_boolean_shape(receiver, selector, arguments) else {
            return Ok(None);
        };
        let doc = match shape {
            PureBooleanShape::Solo(sel, block) => {
                self.generate_solo_boolean_pure(sel, receiver, block)?
            }
            PureBooleanShape::Not => self.generate_pure_boolean_case(
                receiver,
                "not",
                PureArm::Answer(SoloAnswer::Literal(false)),
                PureArm::Answer(SoloAnswer::Literal(true)),
                &[],
            )?,
            PureBooleanShape::IfTrueIfFalse(true_block, false_block) => {
                self.generate_if_true_if_false_pure(receiver, true_block, false_block)?
            }
        };
        Ok(Some(doc))
    }

    /// The gates of [`Self::try_generate_pure_boolean_conditional`]: the shape
    /// it lowers `receiver selector arguments` as, or `None` when it declines
    /// (the send then takes the mutation path or dynamic dispatch). The one
    /// decision the lowering and the bracket classifier read (BT-4524:
    /// `classify_inlined` brackets a conditional this declines whose blocks
    /// hold a `^` and thread nothing, because it runs them as closures).
    pub(in crate::core_erlang) fn pure_boolean_shape<'a>(
        &self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &'a [Expression],
    ) -> Option<PureBooleanShape<'a>> {
        // Fallback closures of a pure-inline conditional keep dynamic
        // dispatch (exponential duplication otherwise).
        if self.decline_pure_boolean_inline {
            return None;
        }
        // A `self`/`super`/class receiver keeps today's dispatch (see the
        // doc comment). Parens are transparent here — this is a receiver
        // classification, not the block-argument shape match below.
        match receiver.unwrap_parens() {
            Expression::Super(_) | Expression::ClassReference { .. } => return None,
            recv if super::super::expr_shape::is_self_identifier(recv) => return None,
            _ => {}
        }

        // `and:`/`or:` are not `WellKnownSelector`s (see
        // `try_generate_boolean_protocol`'s own `and:`/`or:` arm); match the
        // keyword shape directly, as it does.
        if let MessageSelector::Keyword(parts) = selector
            && parts.len() == 1
            && arguments.len() == 1
        {
            let kw = parts[0].keyword.as_str();
            if (kw == "and:" || kw == "or:")
                && let Expression::Block(block) = &arguments[0]
                && block.parameters.is_empty()
                && !self.and_or_needs_mutation_threading(receiver, block)
                && !self.pure_blocks_hold_bracketed_call(&[block])
            {
                let sel = if kw == "and:" {
                    SoloBoolean::And
                } else {
                    SoloBoolean::Or
                };
                return Some(PureBooleanShape::Solo(sel, block));
            }
        }

        // BT-4473: `R not` — `True>>not`/`False>>not` are compiled methods of
        // sealed stdlib classes, answered by their `dispatch/3` arm before any
        // extension, so a `true`/`false` receiver's answer is fixed. Any
        // other receiver takes the wildcard arm's ordinary send.
        if let MessageSelector::Unary(name) = selector
            && name.as_str() == "not"
            && arguments.is_empty()
        {
            return Some(PureBooleanShape::Not);
        }

        match selector.well_known() {
            Some(WellKnownSelector::IfTrue) if arguments.len() == 1 => {
                if let Expression::Block(block) = &arguments[0]
                    && block.parameters.is_empty()
                    && !self.conditional_needs_mutation_threading(receiver, &[block])
                    && !self.pure_blocks_hold_bracketed_call(&[block])
                {
                    return Some(PureBooleanShape::Solo(SoloBoolean::IfTrue, block));
                }
            }
            Some(WellKnownSelector::IfFalse) if arguments.len() == 1 => {
                if let Expression::Block(block) = &arguments[0]
                    && block.parameters.is_empty()
                    && !self.conditional_needs_mutation_threading(receiver, &[block])
                    && !self.pure_blocks_hold_bracketed_call(&[block])
                {
                    return Some(PureBooleanShape::Solo(SoloBoolean::IfFalse, block));
                }
            }
            Some(WellKnownSelector::IfTrueIfFalse) if arguments.len() == 2 => {
                if let (Expression::Block(true_block), Expression::Block(false_block)) =
                    (&arguments[0], &arguments[1])
                    && true_block.parameters.is_empty()
                    && false_block.parameters.is_empty()
                    && !self
                        .conditional_needs_mutation_threading(receiver, &[true_block, false_block])
                    && !self.pure_blocks_hold_bracketed_call(&[true_block, false_block])
                {
                    return Some(PureBooleanShape::IfTrueIfFalse(true_block, false_block));
                }
            }
            _ => {}
        }

        None
    }

    /// BT-4070: whether pure-inline must decline `blocks` here — an actor
    /// instance method (where calls are bracketed) with a guarded operator,
    /// or another call codegen brackets (BT-4524, below), in a branch.
    /// Read off [`Self::block_contains_guarded_operator`], the
    /// same classifier the send emitter exempts, so the two cannot disagree
    /// about what an operator is; the scope is the enclosing method's (the
    /// arm's own scope starts empty — the arity gate admits no block
    /// parameters — so lookups resolve identically when the arm lowers).
    ///
    /// BT-4524: the same holds for a call codegen brackets that the block
    /// analysis does not see as user code (a loop, an evaluated block or a
    /// closure `ensure:` running a block that holds a `^`,
    /// `classify_loop`/`classify_inlined`): it too would sit in that
    /// closure frame with no `State` to store, so the conditional declines
    /// pure-inline ([`Self::expr_holds_bracketed_call`], the probe loop mode
    /// selection asks).
    fn pure_blocks_hold_bracketed_call(&self, blocks: &[&Block]) -> bool {
        self.synced_calls_active()
            && blocks.iter().any(|block| {
                self.block_contains_guarded_operator(block)
                    || block
                        .body
                        .iter()
                        .any(|s| self.expr_holds_bracketed_call(&s.expression))
            })
    }

    /// Whether a nil-testing conditional (`ifNil:`, `ifNotNil:`, the two-arm
    /// forms) whose block needs threading compiles inline to a
    /// `{Result, NewState}` tuple rather than to a closure: in an actor
    /// method, in a loop body and (ADR 0131 Phase 3, BT-3738) at the REPL,
    /// whose bindings map is the threaded state
    /// (`generate_nil_conditional_with_mutations_tuple` sets
    /// `repl_loop_mutated` there, as the boolean family does).
    pub(super) fn nil_conditional_inlines(&self) -> bool {
        self.context == CodeGenContext::Actor || self.in_loop_body || self.is_repl_mode()
    }

    /// The shared "does this
    /// `and:`/`or:` need the inline mutation-threading path" gate —
    /// block-body mutations, a cross-scope list-op mutation, an outer
    /// loop-body local write, or a hoistable actor self-send anywhere in the
    /// receiver's hoistable sub-tree (the receiver itself, or nested inside
    /// it — e.g. a binary-op operand). Shared by
    /// [`Self::try_generate_boolean_protocol`] (deciding whether `and:`/`or:`
    /// is itself the top-level construct to inline) and
    /// [`super::super::control_flow::conditionals::CoreErlangGenerator::compile_conditional_receiver`]
    /// (deciding whether a NESTED `and:`/`or:` sub-expression — e.g. `((self
    /// recordOnce: x) and: [y]) ifTrue:...` — must be compiled through
    /// [`Self::generate_solo_boolean_with_mutations`] (`and:`/`or:`)
    /// and its `{Result, NewState}` tuple unwrapped, rather than through the
    /// generic self-send-hoisting walk).
    ///
    /// Extracting one gate for both call sites matters here specifically:
    /// without it, `compile_conditional_receiver`'s hoist walk would treat
    /// `and:`/`or:` as opaque (`is_conditional_selector`) and leave threading
    /// entirely to `try_generate_boolean_protocol`'s own re-entrant compile of
    /// the receiver — but that path returns a *tuple*-valued document where
    /// `compile_conditional_receiver`'s contract requires a plain boolean
    /// value, which would corrupt the enclosing conditional's state-version
    /// numbering (`unbound variable 'State1'` from `core_lint` on a fixture
    /// combining `((self recordOnce: x) and: [true]) ifTrue:ifFalse:` with a
    /// mutation-threaded `and:`/`or:`). The two decisions must
    /// agree on exactly the same predicate — AGENTS.md's
    /// no-duplicate-implementations rule — so `compile_conditional_receiver`
    /// special-cases this shape itself using this same gate.
    pub(in crate::core_erlang) fn and_or_needs_mutation_threading(
        &self,
        receiver: &Expression,
        block: &Block,
    ) -> bool {
        self.conditional_needs_mutation_threading(receiver, &[block])
    }

    /// ADR 0118 phase 0: the one gate behind `ifTrue:`, `ifFalse:`,
    /// `ifTrue:ifFalse:`, `ifNotNil:`, and (via
    /// [`Self::and_or_needs_mutation_threading`]) `and:`/`or:` — "does this
    /// conditional-shaped construct need the inline mutation-threading path
    /// rather than a plain closure?" Block-body mutations, a cross-scope
    /// list-op mutation, an outer loop-body local write in ANY branch block,
    /// or a hoistable actor self-send anywhere in the receiver's hoistable
    /// sub-tree. `ifTrue:ifFalse:` passes both its branch blocks; every other
    /// caller passes its one block. Extracted so the "same four-term
    /// disjunction inlined four times" the ADR 0118 §Context problem
    /// statement calls out collapses to one function, callable (and thus
    /// testable for agreement — see `threading_gates_agree_on_fixture_set`)
    /// instead of four `let` bindings.
    pub(in crate::core_erlang) fn conditional_needs_mutation_threading(
        &self,
        receiver: &Expression,
        blocks: &[&Block],
    ) -> bool {
        self.conditional_blocks_need_threading(blocks)
            || self.conditional_receiver_needs_threading(receiver)
    }

    /// The block half of [`Self::conditional_needs_mutation_threading`]: does
    /// any branch block need the inline mutation-threading path. The receiver
    /// half is sequenced ahead of the conditional (and so changes once it has
    /// been); the conditional proper is lowered on this half alone.
    pub(in crate::core_erlang) fn conditional_blocks_need_threading(
        &self,
        blocks: &[&Block],
    ) -> bool {
        blocks.iter().any(|block| {
            // shares `block_arg_needs_threading`'s block-body check
            // (block-local/field mutations plus a cross-scope list-op
            // mutation) — the combinator `control_flow_has_mutations` uses
            // for conditional arms and exception blocks (loops and folds
            // read `loop_body_has_carried_mutations`, BT-3834) — extended here
            // with the in-loop-body-local-write disjunct only this caller
            // needs.
            self.block_arg_needs_threading(block)
                || (self.in_loop_body
                    && !self.block_profile_or_analyze(block).local_writes.is_empty())
        })
    }
}
