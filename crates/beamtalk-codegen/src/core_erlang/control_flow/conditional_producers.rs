// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Solo-boolean, nil, `ifTrue:ifFalse:` and `match:` conditional producers and
//! the pure boolean arms (split from `conditionals.rs`).

use super::super::patterns::MatchThreading;
use super::super::threaded_expr::ProducerTail;
use super::super::threaded_ir::{ThreadedStmt, ThreadedValue};
use super::super::{CoreErlangGenerator, Result};
use super::conditionals::{
    FALLBACK_WRITING_BLOCK_HINT, IF_TRUE_IF_FALSE, ProducerSeed, PureArm, ScratchStart, SoloBoolean,
};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::normalize::IF_FALSE_IF_TRUE;
use beamtalk_core::ast::{Block, Expression, MatchArm};
use beamtalk_core::semantic_analysis::block_facts::match_arm_block;
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// BT-3947 (closes BT-3875): the [`ThreadedValue`] producer for a `match:` that
    /// is a local-threading producer ([`CoreErlangGenerator::conditional_producer_locals`]):
    /// a conditional with one arm per `match:` arm. The scratch map is seeded with
    /// the locals its arms write (`seed_scratch_locals` over the arms as blocks,
    /// [`match_arm_block`]), each arm is a `BranchArm` frame carrying them, and the
    /// result tuple's `StateAcc` comes back through one `LocalRebind` per local,
    /// lowered by the enclosing frame (`conditional_tuple_to_threaded_value`).
    ///
    /// `frame` is captured before the arms are lowered, like every conditional
    /// producer's: the arms mint frames of their own.
    pub(in crate::core_erlang) fn generate_match_producer(
        &mut self,
        value: &Expression,
        arms: &[MatchArm],
        span: Span,
        tail: ProducerTail,
    ) -> Result<ThreadedValue> {
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let blocks: Vec<Block> = arms.iter().map(match_arm_block).collect();
        let block_refs: Vec<&Block> = blocks.iter().collect();
        // Test-only: the pre-BT-4196 ordering, seeding before the scrutinee
        // (`StaleConstructSeed`).
        let early_seed: Option<ProducerSeed> = fault!(SeedBeforeScrutinee)
            .then(|| self.producer_scratch_seed(&block_refs, scratch, frame));
        // BT-4196: the scrutinee compiles FIRST, so its State steps (a field
        // write, a bracket reload) are in the base State the scratch seed and
        // the arms thread from, like `ifTrue:`'s receiver and the lookup
        // producer's receiver/key.
        let recv_tv = self.compile_match_scrutinee(value)?;
        let value_doc = self.threaded_value_doc(&recv_tv.value);
        let splice = self.splices_operand_preludes(&[&recv_tv.prelude]);
        let (head, inline_prelude): (Vec<ThreadedStmt>, Document<'static>) = if splice {
            (recv_tv.prelude, Document::Nil)
        } else {
            (Vec::new(), self.threaded_prelude_doc(&recv_tv.prelude))
        };
        let seed = match early_seed {
            Some(seed) => seed,
            None => self.producer_scratch_seed(&block_refs, scratch, frame),
        };
        let arm_frame = self.conditional_arm_frame(&block_refs, true);
        let threading = MatchThreading {
            base_state: &seed.base_state,
            arm: arm_frame.as_ref(),
        };
        let match_doc = self.generate_match_after_scrutinee(arms, &value_doc, Some(&threading))?;
        let tuple_doc = docvec![inline_prelude, seed.doc, match_doc];
        let tv = self.conditional_tuple_to_threaded_value(
            tuple_doc,
            frame,
            span,
            &block_refs,
            tail,
            true,
        )?;
        Ok(Self::with_producer_head(head, seed.base, tv))
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for a single-block
    /// boolean conditional (`flag ifTrue: [block]`, `ifFalse:`, `and:`,
    /// `or:` — [`SoloBoolean`]) — the expression-position counterpart of
    /// [`Self::generate_solo_boolean_with_mutations_tuple`], whose raw tuple
    /// `Document` this wraps via
    /// [`Self::conditional_tuple_to_threaded_value`]. Called by
    /// [`CoreErlangGenerator::threaded_expression`]'s inline-control-flow
    /// producer branch and by [`super::super::threaded_expr`]'s Actor-body
    /// consumers — never by [`super::super::intrinsics::CoreErlangGenerator::try_generate_boolean_protocol`],
    /// which keeps calling the `_tuple` builder directly for its own
    /// Document-returning contract.
    pub(in crate::core_erlang) fn generate_solo_boolean_with_mutations(
        &mut self,
        sel: SoloBoolean,
        receiver: &Expression,
        block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc = self
            .generate_solo_boolean_with_mutations_tuple(sel, receiver, block, scratch, producer)?;
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &[block], tail, producer)
    }

    /// Generates inline code for a single-block boolean conditional
    /// (`flag ifTrue: [block]`, `ifFalse:`, `and:`, `or:`) when the block
    /// contains field mutations or threaded local writes (or the receiver is
    /// itself an actor self-send needing threading — see
    /// `compile_conditional_receiver`).
    ///
    /// Returns `{Result, NewState}`:
    /// - The arm that runs the block: `{block_result, mutated_state}`.
    /// - The non-taken arm: `{<answer>, unchanged_state}`, where `<answer>`
    ///   comes from [`SoloBoolean::non_taken`] — the table the pure path reads
    ///   too — so it matches dynamic dispatch: the receiver for
    ///   `ifTrue:`/`ifFalse:` (`False>>ifTrue: => self`), the literal
    ///   `false`/`true` for `and:`/`or:` (BT-4189).
    pub(in crate::core_erlang) fn generate_solo_boolean_with_mutations_tuple(
        &mut self,
        sel: SoloBoolean,
        receiver: &Expression,
        block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let cond_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let cond_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so the non-taken branch and the
        // post-conditional extraction always see the `__local__` keys.
        let (seed_doc, base_state) = self.seed_scratch_locals(&[block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[block], producer);

        // ADR 0111 Addendum 5: `generate_conditional_branch_inline`
        // builds, `verify()`s, and `render()`s this arm's real per-frame
        // `ThreadedIr` internally.
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.conditional_arm(block, arm_frame.as_ref()))?;
        // BT-4244: a non-Boolean receiver sends the message, as the pure
        // path does; the clause also keeps the `case` statically exhaustive
        // (see `case_clause_fallback`'s doc comment).
        let no_match_fallback = self.mutation_boolean_fallback_clause(
            &cond_var,
            sel.selector_atom(),
            &[block],
            &base_state,
        )?;
        // The taken arm threads from the seeded base state; the non-taken
        // arm's `{<answer>, <state>}` tuple goes through the shared family
        // emission helper with [State] (ADR 0122 Decision 3/BT-3514), its
        // answer read from the one `SoloBoolean` table (BT-4189).
        let taken = docvec![
            "let StateAcc = ",
            leaf::var(base_state.clone()),
            " in ",
            branch_doc,
        ];
        let answer = self.conditional_baseline_tuple(sel.non_taken().doc(&cond_var), &base_state);
        let (true_doc, false_doc) = if sel.block_on_true() {
            (taken, answer)
        } else {
            (answer, taken)
        };

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> ",
            true_doc,
            " <'false'> when 'true' -> ",
            false_doc,
            no_match_fallback,
            " end",
        ])
    }

    /// BT-3982: the [`ThreadedValue`] producer for an evaluated literal block
    /// with its arguments (`[:y | ...] value: x`, `beamtalk_core`'s
    /// `literal_block_value_send`): each argument is threaded in order in the
    /// enclosing frame and bound to its parameter, **before** the body, then
    /// the block is the degenerate conditional of
    /// [`Self::generate_block_value_with_mutations`]: one carrier in the
    /// enclosing frame followed by the frame's `State` `Bind` (and
    /// `LocalRebind`s when it is a local-threading producer), answering
    /// `element(1, Carrier)`. Its `{Value, State}` carrier never crosses an
    /// expression boundary.
    pub(in crate::core_erlang) fn generate_evaluated_block_producer(
        &mut self,
        block: &Block,
        arguments: &[Expression],
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        if arguments.is_empty() {
            return self.generate_block_value_with_mutations(block, tail, producer);
        }
        let frame = self.current_frame();
        let mut prelude = Vec::new();
        let mut arg_vars = Vec::with_capacity(arguments.len());
        for (param, arg) in block.parameters.iter().zip(arguments) {
            let tv = self.threaded_expression(arg, frame)?;
            prelude.extend(tv.prelude);
            let arg_var = self.fresh_temp_var(&param.name);
            let value_doc = self.threaded_value_doc(&tv.value);
            prelude.push(ThreadedStmt::Statement(
                docvec!["let ", leaf::var(arg_var.clone()), " = ", value_doc, " in "],
                arg.span(),
            ));
            arg_vars.push(arg_var);
        }
        // The parameters are bound in a scope of their own, like the inline
        // path's (`bind_inline_block_value_args`).
        self.push_scope();
        for (param, arg_var) in block.parameters.iter().zip(&arg_vars) {
            self.bind_var(&param.name, arg_var);
        }
        let body = self.generate_block_value_with_mutations(block, tail, producer);
        self.pop_scope();
        let body = body?;
        // BT-4263: a local-threading producer's `LocalRebind`s are the
        // enclosing frame's: the carrier lands there, so the frame reads each
        // rebound local from its `value_var`. `construct_rebind_prelude`
        // bound them in the parameter scope popped above, so they are bound
        // again in the scope the frame keeps (`VerifyError::RebindNotVisible`
        // otherwise). A parameter is never one: it is bound only inside the
        // block, and a write to it is the block's own binding.
        let producer = producer && !fault!(EvaluatedBlockRebindsScoped);
        if producer {
            for stmt in &body.prelude {
                if let ThreadedStmt::LocalRebind {
                    local, value_var, ..
                } = stmt
                {
                    self.bind_var(local, value_var);
                }
            }
        }
        prelude.extend(body.prelude);
        Ok(ThreadedValue {
            prelude,
            value: body.value,
        })
    }

    /// BT-3841: the [`ThreadedValue`] producer for an evaluated literal block
    /// (`[block] value`, `beamtalk_core`'s `literal_block_value_arm`) that is a
    /// local-threading producer: the degenerate conditional whose one arm always
    /// runs. It shares the conditional producer's scratch map, arm frame and
    /// tuple-to-`ThreadedValue` step, so the arm's locals come back through
    /// `LocalRebind`s lowered by the enclosing frame.
    pub(in crate::core_erlang) fn generate_block_value_with_mutations(
        &mut self,
        block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc = self.generate_block_value_with_mutations_tuple(block, scratch, producer)?;
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &[block], tail, producer)
    }

    /// The `{Result, NewState}` tuple of [`Self::generate_block_value_with_mutations`]:
    /// the seeded scratch map, then the arm. There is no `case`: the arm is
    /// taken every time.
    pub(in crate::core_erlang) fn generate_block_value_with_mutations_tuple(
        &mut self,
        block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let outer_state = self.current_state_var();
        let (seed_doc, base_state) = self.seed_scratch_locals(&[block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[block], producer);
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.conditional_arm(block, arm_frame.as_ref()))?;
        Ok(docvec![
            seed_doc,
            "let StateAcc = ",
            leaf::var(base_state),
            " in ",
            branch_doc,
        ])
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for `flag
    /// ifTrue: [t_block] ifFalse: [f_block]` — see
    /// [`Self::generate_solo_boolean_with_mutations`]'s doc comment for the
    /// shape every one of these producers shares.
    pub(in crate::core_erlang) fn generate_if_true_if_false_with_mutations(
        &mut self,
        receiver: &Expression,
        true_block: &Block,
        false_block: &Block,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = false_block.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc = self.generate_if_true_if_false_with_mutations_tuple(
            receiver,
            true_block,
            false_block,
            scratch,
            producer,
        )?;
        self.conditional_tuple_to_threaded_value(
            tuple_doc,
            frame,
            span,
            &[true_block, false_block],
            tail,
            producer,
        )
    }

    /// Generates inline code for `flag ifTrue: [t_block] ifFalse: [f_block]` in actor context
    /// when at least one block contains field mutations.
    ///
    /// Returns `{Result, NewState}` from whichever branch is taken.
    pub(in crate::core_erlang) fn generate_if_true_if_false_with_mutations_tuple(
        &mut self,
        receiver: &Expression,
        true_block: &Block,
        false_block: &Block,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let cond_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let cond_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so a branch that does not itself
        // write a given local (and the post-conditional extraction) still sees the
        // `__local__` key.
        let (seed_doc, base_state) =
            self.seed_scratch_locals(&[true_block, false_block], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[true_block, false_block], producer);

        // True branch
        let (true_branch_doc, _true_final) =
            self.with_branch_context(|this| this.conditional_arm(true_block, arm_frame.as_ref()))?;

        // False branch (reset to same initial state)
        let (false_branch_doc, _false_final) =
            self.with_branch_context(|this| this.conditional_arm(false_block, arm_frame.as_ref()))?;

        // ADR 0111 Addendum 5: the true/false arms are sibling
        // with_branch_context frames — each `generate_conditional_branch_inline`
        // call mints its own fresh FrameId (`current_branch_frame`) and
        // `verify()`s its own real IR internally, so either arm
        // independently reaching the same version number as the other
        // (e.g. both perform exactly one field mutation, both producing
        // "StateAcc1" in their own frame) is correctly NOT a
        // NonLinearVersion violation — the check that used to run here
        // (`check_branch_frame_linearity`) is gone; real per-frame
        // verification now happens where the IR is actually built.
        // BT-4244: a non-Boolean receiver sends the message, as the pure
        // path does; the clause also keeps the `case` statically exhaustive
        // (see `case_clause_fallback`'s doc comment).
        let no_match_fallback = self.mutation_boolean_fallback_clause(
            &cond_var,
            IF_TRUE_IF_FALSE,
            &[true_block, false_block],
            &base_state,
        )?;

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> let StateAcc = ",
            leaf::var(base_state.clone()),
            " in ",
            true_branch_doc,
            " <'false'> when 'true' -> let StateAcc = ",
            leaf::var(base_state.clone()),
            " in ",
            false_branch_doc,
            no_match_fallback,
            " end",
        ])
    }

    /// ADR 0118 phase 4: the [`ThreadedValue`] producer for the
    /// nil-testing conditional family — `ifNil:` (`nil_block: Some`,
    /// `not_nil_block: None`), `ifNotNil:` (`nil_block: None`,
    /// `not_nil_block: Some`), and `ifNil:ifNotNil:`/`ifNotNil:ifNil:`
    /// (both `Some` — the caller passes them by MEANING, not by source
    /// keyword order, so this generator itself doesn't need to know which
    /// selector produced the call). Generalizes the single-block
    /// `ifNotNil:`-only generator this replaces (see
    /// [`Self::generate_solo_boolean_with_mutations`]'s doc comment for the
    /// producer shape every one of these shares) so `ifNil:` and the
    /// two-block forms get the same inline-case treatment instead of
    /// falling through to a plain Tier 1 closure that discards a nested
    /// self-send's mutation.
    pub(in crate::core_erlang) fn generate_nil_conditional_with_mutations(
        &mut self,
        receiver: &Expression,
        nil_block: Option<&Block>,
        not_nil_block: Option<&Block>,
        tail: ProducerTail,
        producer: bool,
    ) -> Result<ThreadedValue> {
        let span = not_nil_block
            .or(nil_block)
            .map_or_else(|| receiver.span(), |b| b.span);
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc = self.generate_nil_conditional_with_mutations_tuple(
            receiver,
            nil_block,
            not_nil_block,
            scratch,
            producer,
        )?;
        let arms: Vec<&Block> = nil_block.into_iter().chain(not_nil_block).collect();
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &arms, tail, producer)
    }

    /// Generates inline code for the nil-testing conditional family in
    /// actor context when a block contains field mutations. See
    /// [`Self::generate_nil_conditional_with_mutations`]'s doc comment for
    /// what `nil_block`/`not_nil_block` mean.
    ///
    /// Returns `{Result, NewState}`:
    /// - Nil branch: `nil_block`'s own `{value, state}` when present,
    ///   else `{'nil', unchanged_state}` (bare `ifNotNil:`).
    /// - Non-nil branch: `not_nil_block`'s own `{value, state}` when
    ///   present, else `{receiver_value, unchanged_state}` (bare `ifNil:`
    ///   — Smalltalk's "answers self when not nil" contract).
    ///
    /// If `not_nil_block` has a parameter (`:v`), it is bound to the
    /// receiver object value inside its branch body.
    pub(in crate::core_erlang) fn generate_nil_conditional_with_mutations_tuple(
        &mut self,
        receiver: &Expression,
        nil_block: Option<&Block>,
        not_nil_block: Option<&Block>,
        scratch: ScratchStart,
        producer: bool,
    ) -> Result<Document<'static>> {
        // The REPL assembler (and `lower_repl_construct_tuple` for an operand)
        // unpacks the `{Result, Bindings}` tuple this compiles to.
        if self.is_repl_mode() {
            self.set_repl_loop_mutated(true);
        }
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let recv_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let recv_val_doc = self.threaded_value_doc(&recv_tv.value);
        let obj_var = self.fresh_temp_var("Obj");
        let outer_state = self.current_state_var();
        // seed threaded outer-locals so a branch that does not
        // itself write a given local (and the post-conditional extraction)
        // still sees the `__local__` key.
        let mut seed_blocks: Vec<&Block> = Vec::new();
        seed_blocks.extend(nil_block);
        seed_blocks.extend(not_nil_block);
        let (seed_doc, base_state) = self.seed_scratch_locals(&seed_blocks, scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&seed_blocks, producer);

        // ADR 0111 Addendum 5: see generate_solo_boolean_with_mutations'
        // matching comment — real per-frame verify() now runs inside
        // generate_conditional_branch_inline itself.
        let nil_arm = if let Some(block) = nil_block {
            self.conditional_arm_clause(block, None, arm_frame.as_ref(), &base_state)?
        } else {
            // ADR 0122 Decision 3/BT-3514: through the shared family
            // emission helper with [State] rather than a hand-rolled splice.
            self.conditional_baseline_tuple(Document::Str("'nil'"), &base_state)
        };

        let not_nil_arm = if let Some(block) = not_nil_block {
            // The block parameter is bound to the receiver value (`obj_var`).
            self.conditional_arm_clause(block, Some(&obj_var), arm_frame.as_ref(), &base_state)?
        } else {
            // ADR 0122 Decision 3/BT-3514: through the shared family
            // emission helper with [State] rather than a hand-rolled splice.
            self.conditional_baseline_tuple(leaf::var(obj_var.clone()), &base_state)
        };

        Ok(docvec![
            recv_preamble,
            seed_doc,
            "let ",
            leaf::var(obj_var.clone()),
            " = ",
            recv_val_doc,
            " in case ",
            leaf::var(obj_var),
            " of <'nil'> when 'true' -> ",
            nil_arm,
            " <_> when 'true' -> ",
            not_nil_arm,
            " end",
        ])
    }

    /// BT-4070: the pure-inline producer for a single-block boolean
    /// conditional (`R ifTrue: [B]`, `ifFalse:`, `and:`, `or:` —
    /// [`SoloBoolean`]) — the expression-position counterpart of
    /// [`Self::generate_solo_boolean_with_mutations_tuple`], but a plain
    /// value, not a `{Result, NewState}` tuple: the gate
    /// ([`Self::try_generate_pure_boolean_conditional`]) guarantees no block
    /// or receiver needs threading, so no state is threaded and no classifier
    /// change is needed (`control_flow_has_mutations` is false, the plain
    /// consumer path). Matches dynamic dispatch exactly: the arms come from
    /// the [`SoloBoolean`] table the mutation builder reads too, so a
    /// non-taken `ifTrue:`/`ifFalse:` answers the receiver
    /// (`False>>ifTrue: => self`) on both paths (BT-4189).
    pub(in crate::core_erlang) fn generate_solo_boolean_pure(
        &mut self,
        sel: SoloBoolean,
        receiver: &Expression,
        block: &Block,
    ) -> Result<Document<'static>> {
        let (true_arm, false_arm) = sel.pure_arms(block);
        self.generate_pure_boolean_case(
            receiver,
            sel.selector_atom(),
            true_arm,
            false_arm,
            &[block],
        )
    }

    /// BT-4070: the pure-inline producer for `R ifTrue: [T] ifFalse: [F]`.
    pub(in crate::core_erlang) fn generate_if_true_if_false_pure(
        &mut self,
        receiver: &Expression,
        true_block: &Block,
        false_block: &Block,
    ) -> Result<Document<'static>> {
        self.generate_pure_boolean_case(
            receiver,
            IF_TRUE_IF_FALSE,
            PureArm::Inline(true_block),
            PureArm::Inline(false_block),
            &[true_block, false_block],
        )
    }

    /// BT-4070: the shared core behind the pure-inline builders
    /// ([`Self::generate_solo_boolean_pure`],
    /// [`Self::generate_if_true_if_false_pure`]) — `R <sel> [blocks]` with literal, non-mutating blocks as an
    /// inline `case` over the once-evaluated receiver `C`:
    ///
    /// ```erlang
    /// let _Cond1 = <receiver> in
    /// case _Cond1 of
    ///   <'true'> when 'true' -> <true_arm>
    ///   <'false'> when 'true' -> <false_arm>
    ///   <_CondNoMatch2> when 'true' -> call 'beamtalk_message_dispatch':'send'(
    ///     _Cond1, '<sel>', [fun () -> <block> end, ...])
    /// end
    /// ```
    ///
    /// The wildcard arm is a real dynamic send — never a `case_clause`
    /// raise — so a non-Boolean receiver (a custom `ifTrue:`
    /// implementor, DNU, a proxy, `perform:`) behaves exactly as today; it
    /// doubles as the static-exhaustiveness arm (see
    /// `case_clause_fallback`'s doc comment) and shares its `CondNoMatch`
    /// naming for grep-ability. The fallback closures compile through
    /// today's Tier-1 builder via `expression_doc`, over the original
    /// argument expressions, so they are byte-identical to the closures the
    /// fully-dynamic send would allocate — allocated only when the arm is
    /// taken. Pure-inline is declined while compiling them: they run only
    /// for non-Boolean receivers, so the optimisation buys nothing there,
    /// and inlining inside them would duplicate nested blocks exponentially
    /// (each level compiles its blocks both inline and as fallback
    /// closures). Codegen warnings are deduplicated across the fallback
    /// loop for the same reason — the fallback lowers the same AST the
    /// inline arm already reported, and `add_codegen_warning` does not
    /// dedup — while lowering-specific findings are kept (see
    /// `dedup_append_codegen_warnings`).
    ///
    /// The receiver compiles through [`Self::compile_conditional_receiver`],
    /// the same path the mutation builders use: the gate guarantees an
    /// empty prelude, but any residual one still runs before `C` binds, so
    /// the binding order can never be wrong.
    pub(in crate::core_erlang) fn generate_pure_boolean_case(
        &mut self,
        receiver: &Expression,
        selector_atom: &'static str,
        true_arm: PureArm<'_>,
        false_arm: PureArm<'_>,
        blocks: &[&Block],
    ) -> Result<Document<'static>> {
        let recv_frame = self.current_frame();
        let recv_tv = self.compile_conditional_receiver(receiver, recv_frame)?;
        let recv_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let recv_val_doc = self.threaded_value_doc(&recv_tv.value);
        let cond_var = self.fresh_temp_var("Cond");
        let true_doc = self.pure_arm_doc(true_arm, &cond_var)?;
        let false_doc = self.pure_arm_doc(false_arm, &cond_var)?;
        let fallback = self.boolean_fallback_send(&cond_var, selector_atom, blocks, false)?;
        let no_match_var = self.fresh_temp_var("CondNoMatch");

        Ok(docvec![
            recv_preamble,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            recv_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> ",
            true_doc,
            " <'false'> when 'true' -> ",
            false_doc,
            " <",
            leaf::var(no_match_var),
            "> when 'true' -> ",
            fallback,
            " end",
        ])
    }

    /// BT-4244: the dynamic send a boolean conditional's wildcard arm makes for
    /// a receiver that is neither `true` nor `false`:
    /// `beamtalk_message_dispatch:send(<cond_var>, '<selector>', [<blocks>])`.
    /// It is the one fallback every inline boolean lowering uses: the pure
    /// `case` ([`Self::generate_pure_boolean_case`]), the mutation `case`s
    /// ([`Self::mutation_boolean_fallback_clause`]) and the value-type
    /// conditionals. A user-defined `ifTrue:` therefore runs, and any other
    /// receiver raises a structured `does_not_understand`, whether or not an
    /// arm writes state.
    ///
    /// The arms go to the send as real blocks ([`Self::fallback_block_doc`]),
    /// compiled while pure-inline is declined (same save/restore shape as
    /// `while_loops.rs`: bind the result, restore, then `?`, so an error never
    /// leaks the flag). They run only for a non-Boolean receiver, so inlining
    /// buys nothing there, and inlining inside them would duplicate nested
    /// blocks exponentially (each level compiles its blocks both inline and as
    /// fallback closures). Codegen warnings are deduplicated for the same
    /// reason: the closures lower the same AST the inline arms already
    /// reported, and `add_codegen_warning` does not dedup, while
    /// lowering-specific findings are kept (see
    /// `dedup_append_codegen_warnings`).
    ///
    /// BT-4524 (ADR 0146 P0): a block holding a `^` that goes as a closure
    /// carries the cell in its throw, so the send stores first: `bracketed`
    /// when the caller wraps it in a `SyncedCall`
    /// ([`Self::mutation_boolean_fallback_clause`]), else a pre-store of the
    /// state an inline `^` here would store
    /// ([`CoreErlangGenerator::nlr_closure_prestore`]).
    pub(in crate::core_erlang) fn boolean_fallback_send(
        &mut self,
        cond_var: &str,
        selector_atom: &str,
        blocks: &[&Block],
        bracketed: bool,
    ) -> Result<Document<'static>> {
        // BT-4991: an `ifFalse:ifTrue:` written with two literal blocks is
        // lowered as `ifTrue:ifFalse:` (`ast::normalize`), but a non-Boolean
        // receiver may implement `ifFalse:ifTrue:` itself: send it as
        // written, the arms in the order they were written.
        let swapped: Vec<&Block>;
        let (selector_atom, blocks) = match blocks {
            [true_block, false_block]
                if selector_atom == IF_TRUE_IF_FALSE
                    && true_block.if_false_if_true_arm
                    && false_block.if_false_if_true_arm =>
            {
                swapped = vec![*false_block, *true_block];
                (IF_FALSE_IF_TRUE, swapped.as_slice())
            }
            _ => (selector_atom, blocks),
        };
        let at = blocks.first().map(|b| b.span).unwrap_or_default();
        // The send's `^`-holding arms that go as closures are checked
        // whichever path lowered the conditional (the mutation path never
        // reaches `note_send`): uncovered, each is an `UnbracketedNlrClosure`.
        // An arm passed as a raising stub never becomes a closure, so it is
        // not recorded (BT-4532).
        let closure_arms: Vec<&Block> = blocks
            .iter()
            .copied()
            .filter(|block| self.fallback_passes_closure(block))
            .collect();
        self.note_nlr_blocks(closure_arms.iter().copied(), selector_atom);
        let prestore = if bracketed {
            None
        } else {
            self.nlr_closure_prestore(&closure_arms, at)
        };
        let cover: Vec<_> = if bracketed || prestore.is_some() {
            closure_arms.iter().map(|block| block.span).collect()
        } else {
            Vec::new()
        };
        let prev = std::mem::replace(&mut self.decline_pure_boolean_inline, true);
        let warn_len = self.codegen_warnings.len();
        let closures: Result<Vec<_>> = self.with_nlr_closure_cover(&cover, |this| {
            blocks
                .iter()
                .map(|block| this.fallback_block_doc(block, selector_atom))
                .collect()
        });
        self.decline_pure_boolean_inline = prev;
        let new_warnings = self.codegen_warnings.split_off(warn_len);
        self.dedup_append_codegen_warnings(new_warnings);
        let send =
            self.runtime_send_call(leaf::var(cond_var.to_string()), selector_atom, closures?);
        Ok(match prestore {
            Some(prestore) => self.nlr_closure_prestored_call(prestore, send),
            None => send,
        })
    }

    /// BT-4244: one block argument of [`Self::boolean_fallback_send`]: the
    /// block as the closure the fully dynamic send would pass
    /// ([`Self::generate_block`]), so ADR 0133's escaping-block rules apply to
    /// it once a non-Boolean receiver's method runs it: an actor field write
    /// goes through the cell (ADR 0137) and a class-variable write is in place
    /// (ADR 0130).
    ///
    /// A block that writes what no escaping block can write (a captured local,
    /// ADR 0133, or a value-type field, [`Self::check_closure_field_writes`])
    /// is not a compile error here: the inline arms thread that write, and
    /// only a receiver that is not a Boolean reaches this closure. Nor is a
    /// block that touches the actor's state where the send cannot be
    /// bracketed (`stub_state`, [`Self::conditional_fallback_stubs_state`]).
    /// Either is passed as a `fun` of the block's arity that raises
    /// `stateful_block_dispatch` when called, the error a Tier 2 block raises
    /// when generic dispatch calls it, so the send still runs (and DNUs, or
    /// reaches a user `ifTrue:`) and the block's state effects are never
    /// silently lost.
    fn fallback_block_doc(
        &mut self,
        block: &Block,
        selector_atom: &str,
    ) -> Result<Document<'static>> {
        if self.fallback_passes_closure(block) {
            return self.generate_block(block);
        }
        let params: Vec<Document<'static>> = block
            .parameters
            .iter()
            .map(|_| leaf::var(self.fresh_temp_var("FbArg")))
            .collect();
        let raise = self.generate_stateful_block_dispatch_error(
            selector_atom,
            "Block",
            FALLBACK_WRITING_BLOCK_HINT,
        );
        Ok(docvec![
            "fun (",
            beamtalk_cerl_doc::join(params, &Document::Str(", ")),
            ") -> ",
            raise,
        ])
    }

    /// BT-4244: whether [`Self::fallback_block_doc`] passes `block` as a real
    /// closure (`true`) or as a raising stub (`false`). The one decision both
    /// the closure emission and the bracket
    /// ([`Self::mutation_boolean_fallback_clause`]) read, so a fallback whose
    /// blocks are all stubs is never bracketed.
    fn fallback_passes_closure(&self, block: &Block) -> bool {
        let analysis = crate::core_erlang::block_analysis::analyze_block(block);
        !self.conditional_arms_write_outer_locals(&[block])
            && self.check_closure_field_writes(block, &analysis).is_ok()
            && !(self.conditional_fallback_stubs_state()
                && self.analysis_touches_actor_state(&analysis))
    }

    /// BT-4244: the wildcard clause of a mutation-path boolean `case`
    /// ([`Self::generate_solo_boolean_with_mutations_tuple`],
    /// [`Self::generate_if_true_if_false_with_mutations_tuple`]): the shared
    /// dynamic send ([`Self::boolean_fallback_send`]) in the `{Result,
    /// NewState}` shape the other arms answer. Before BT-4244 it raised a raw
    /// `case_clause`, where the pure path sends the message.
    ///
    /// In an actor instance method whose frame holds a whole `State`, the
    /// send is bracketed ([`Self::conditional_fallback_brackets`]) when a
    /// block passed as a closure touches the actor's state: the closures it passes read and
    /// write the actor's fields through the cell, so the arm stores
    /// `base_state` first and answers the reloaded cell as its state (and
    /// the method stores its final `State` on exit, `with_exit_store`). A
    /// fallback whose blocks touch nothing of the actor's needs no bracket
    /// and costs the method no exit store. Elsewhere nothing the closures
    /// can do reaches the scratch map (a captured local is never written by
    /// one, and a block touching the actor's state is a stub where the send
    /// cannot bracket, [`Self::fallback_block_doc`]), so the arm answers
    /// `base_state` unchanged, as the non-taken arm does.
    fn mutation_boolean_fallback_clause(
        &mut self,
        cond_var: &str,
        selector_atom: &str,
        blocks: &[&Block],
        base_state: &str,
    ) -> Result<Document<'static>> {
        // Bracket only when a block passed as a real closure touches the
        // actor's state: a stub touches nothing. Decided before the send is
        // compiled, so a `^` in a bracketed closure needs no pre-store of its
        // own (BT-4524).
        let brackets = self.conditional_fallback_brackets()
            && blocks.iter().any(|block| {
                self.fallback_passes_closure(block) && self.block_touches_actor_state(block)
            });
        let send = self.boolean_fallback_send(cond_var, selector_atom, blocks, brackets)?;
        let span = blocks.first().map(|b| b.span).unwrap_or_default();
        let no_match_var = self.fresh_temp_var("CondNoMatch");
        let body = if brackets {
            let bracket = self.bracket_conditional_fallback(send, base_state, span);
            let tuple = self.conditional_baseline_tuple(leaf::var(bracket.result), &bracket.state);
            docvec![bracket.prelude, tuple]
        } else {
            self.conditional_baseline_tuple(send, base_state)
        };
        Ok(docvec![
            " <",
            leaf::var(no_match_var),
            "> when 'true' -> ",
            body,
        ])
    }

    /// BT-4070: one arm's document of [`Self::generate_pure_boolean_case`].
    fn pure_arm_doc(&mut self, arm: PureArm<'_>, cond_var: &str) -> Result<Document<'static>> {
        match arm {
            PureArm::Inline(block) => {
                // A `case` arm is a Core Erlang scope of its own, but the
                // generator's variable scope is not: a block-local temp the
                // arm binds (`[t := 5. t]`) must not leak into the
                // enclosing method's scope, where a later `t` would resolve
                // to the arm's `let`-bound variable. Push/pop around the
                // arm, as every closed-`fun` lowering does — but WITHOUT
                // `with_closed_fun_scope`'s state-version reset, which is
                // for closed funs only: a pure arm is inlined and shares
                // the enclosing versioning.
                self.push_scope();
                let body = self.generate_block_body(block);
                self.pop_scope();
                body
            }
            PureArm::Answer(answer) => Ok(answer.doc(cond_var)),
        }
    }
}
