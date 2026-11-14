// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Search/detect list operations: `anySatisfy:`, `allSatisfy:`, and `detect:`.

use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CoreErlangGenerator, Result};
use super::fold_driver::FoldOp;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

impl CoreErlangGenerator {
    /// Generates code for `list anySatisfy:` with mutation analysis.
    pub(in crate::core_erlang) fn generate_list_any_satisfy(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "anySatisfy:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list anySatisfy: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_bool_predicate_with_mutations(receiver, body_block, false);
        }
        // No mutations: fall through to simple BIF call (lists:any/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "anySatisfy:", 1)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'lists':'any'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'anySatisfy:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates code for `list allSatisfy:` with mutation analysis.
    pub(in crate::core_erlang) fn generate_list_all_satisfy(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "allSatisfy:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list allSatisfy: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_bool_predicate_with_mutations(receiver, body_block, true);
        }
        // No mutations: fall through to simple BIF call (lists:all/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "allSatisfy:", 1)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'lists':'all'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'allSatisfy:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `anySatisfy:`/`allSatisfy:` using `lists:foldl`
    /// with state threading and a boolean accumulator.
    ///
    /// Like `lists:any/2` and `lists:all/2` the block stops running at the first
    /// decisive element (BT-3805): the fold lambda returns its accumulator
    /// unchanged once `BoolAcc` is decided, so the later elements' writes never
    /// happen ([`skip_when_decided`]).
    ///
    /// Accumulator is `{BoolAcc, StateVars...}` (tuple) or `{BoolAcc, StateAcc}` (map).
    /// - `is_all = false` (anySatisfy): `BoolAcc` starts `false`, set to `true` on match
    /// - `is_all = true` (allSatisfy): `BoolAcc` starts `true`, set to `false` on failure
    pub(in crate::core_erlang) fn generate_list_bool_predicate_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        is_all: bool,
    ) -> Result<Document<'static>> {
        let op = if is_all {
            FoldOp::AllSatisfy
        } else {
            FoldOp::AnySatisfy
        };
        self.generate_fold_with_mutations(receiver, body, op)
    }

    /// Generates code for `list detect:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:detect/2` BIF.
    /// With mutations: uses `lists:foldl` and stops running the block at the first
    /// match (BT-3805), so the mutations of the elements up to and including it
    /// are applied. Returns the first matching element or `nil`.
    pub(in crate::core_erlang) fn generate_list_detect(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "detect:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list detect: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_detect_with_mutations(receiver, body_block);
        }
        // No mutations: fall through to BIF call (beamtalk_list:detect/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "detect:", 1)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'beamtalk_list':'detect'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::var(body_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'detect:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// ADR 0118 phase 4: compiles `detect:ifNone:`'s `ifNone:`
    /// handler as a BRANCH ARM seeded from `state_var` — the same
    /// `generate_conditional_branch_inline` per-frame `ThreadedIr` machinery
    /// `ifTrue:`/`ifFalse:` branches use — returning a `Document` that
    /// evaluates to `{value, finalstate}` directly. This is a branch arm,
    /// not a closure: a self-send or field mutation inside `[...]` threads
    /// through it the same way it would inside any other conditional
    /// branch, instead of being compiled as an ordinary Tier 1 closure
    /// (`apply <fun> ()`) that discards the mutation (or, once wrapped and
    /// handed to a generic runtime dispatch, crashes).
    ///
    /// Falls back to applying the callable — wrapped in the same
    /// `{value, state_var}` tuple shape so callers don't need to
    /// distinguish — when `if_none` is not a literal block (e.g. a variable
    /// holding one): there is no block AST to inline, so state cannot
    /// thread through an opaque callable value. The callable goes through
    /// the shared `non_literal_callable_doc` wrapper at arity 0.
    pub(super) fn generate_if_none_branch_tuple(
        &mut self,
        if_none: &Expression,
        state_var: &str,
    ) -> Result<Document<'static>> {
        let Expression::Block(block) = if_none else {
            let none_code = self.non_literal_callable_doc(if_none, "detect:ifNone:", 0)?;
            let none_result = self.fresh_temp_var("NoneResult");
            return Ok(docvec![
                "let ",
                leaf::var(none_result.clone()),
                " = ",
                self.annotate_current_send_call(docvec!["apply ", none_code, " ()"]),
                " in {",
                leaf::var(none_result),
                ", ",
                leaf::var(state_var.to_string()),
                "}",
            ]);
        };
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(block))?;
        Ok(docvec![
            "let StateAcc = ",
            leaf::var(state_var.to_string()),
            " in ",
            branch_doc,
        ])
    }

    /// Generates code for `list detect:ifNone:` with mutation analysis.
    ///
    /// Without mutations: falls through to runtime dispatch.
    /// With mutations: uses `lists:foldl` like `detect:`, then applies the ifNone
    /// block if no match was found.
    pub(in crate::core_erlang) fn generate_list_detect_if_none(
        &mut self,
        receiver: &Expression,
        predicate: &Expression,
        if_none: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            predicate,
            1,
            "detect:ifNone:",
            "Fix: The detect block must take one argument (each element):\n\
             \x20 list detect: [:item | item > 0] ifNone: ['not found']",
        )?;
        validate_block_arity_exact(
            if_none,
            0,
            "detect:ifNone:",
            "Fix: The ifNone block must take no arguments:\n\
             \x20 list detect: [:item | item > 0] ifNone: ['not found']",
        )?;

        if let Some(pred_block) = self.block_needs_mutation_threading(predicate) {
            return self.generate_list_detect_if_none_with_mutations(receiver, pred_block, if_none);
        }
        // ADR 0118 phase 4: the predicate alone may be mutation-
        // free while the `ifNone:` handler itself contains a self-send or
        // field mutation (`items detect: [:x | x > 100] ifNone: [self
        // bumpCount]`) — route through the with-mutations fold so
        // `generate_list_detect_if_none_with_mutations`'s `ifNone:` handling
        // (see its own doc comment) threads it, instead of falling to
        // `generate_detect_if_none_simple`'s bare closure, which silently
        // drops (or — since that closure closes over a state map ultimately
        // wrapped and passed to a runtime dispatch — crashes) that mutation.
        //
        // ADR 0131 Phase 2c (BT-3794): a handler that only writes an outer
        // local (`[t := 5]`, which `block_needs_mutation_threading` misses
        // in class and value-type methods when the block does not also read
        // it) threads through the same fold: the fold packs the handler's
        // writes (`loop_threaded_locals_with_handler`).
        // The handler-only write routes only for a LITERAL search block (no
        // `unwrap_parens`): the shape core's `producer_packed_blocks` packs
        // and Phase 0 accepts (`producer_body_block`).
        let packs_handler_writes = match (predicate, if_none) {
            (Expression::Block(pred_block), Expression::Block(handler)) => !self
                .loop_threaded_locals_with_handler(pred_block, None, Some(handler))
                .is_empty(),
            _ => false,
        };
        if let Expression::Block(pred_block) = predicate
            && (self.block_needs_mutation_threading(if_none).is_some() || packs_handler_writes)
        {
            return self.generate_list_detect_if_none_with_mutations(receiver, pred_block, if_none);
        }

        // No mutations: fall through to runtime dispatch
        self.generate_detect_if_none_simple(receiver, predicate, if_none)
    }

    /// Simple (non-mutating) detect:ifNone: — dispatches to runtime.
    fn generate_detect_if_none_simple(
        &mut self,
        receiver: &Expression,
        predicate: &Expression,
        if_none: &Expression,
    ) -> Result<Document<'static>> {
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let pred_var = self.fresh_temp_var("temp");
        let pred_code = self.list_op_callable_doc(predicate, "detect:ifNone:", 1)?;
        let none_var = self.fresh_temp_var("temp");
        let none_code = self.list_op_callable_doc(if_none, "detect:ifNone:", 0)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(pred_var.clone()),
            " = ",
            pred_code,
            " in let ",
            leaf::var(none_var.clone()),
            " = ",
            none_code,
            " in ",
            // BT-4366: the send is nested in the argument `let`s, so the call
            // node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'detect:ifNone:', [",
                leaf::var(pred_var),
                ", ",
                leaf::var(none_var),
                "])"
            ]),
        ])
    }

    /// Wrapper for `detect:ifNone:` with mutations — runs the detect foldl,
    /// then checks the `FoundFlag` and evaluates the ifNone block when no match was found.
    ///
    /// The detect foldl accumulator uses `{FoundItem, FoundFlag, State...}`. We extract
    /// `FoundFlag` from the fold result to distinguish "found nil" from "not found" —
    /// checking `FoundItem == nil` would be incorrect when nil is a valid search result.
    fn generate_list_detect_if_none_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        if_none: &Expression,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(receiver, body, FoldOp::DetectIfNone { if_none })
    }

    /// Generates stateful `detect:` using `lists:foldl`
    /// with state threading and a found-item accumulator.
    ///
    /// The elements are processed up to and including the first match
    /// ([`skip_when_decided`], BT-3805). Accumulator is
    /// `{FoundItem, FoundFlag, StateVars...}`.
    ///
    /// Returns `{FoundItem, StateAcc}` where `FoundItem` is `nil` if no match.
    pub(in crate::core_erlang) fn generate_list_detect_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_fold_with_mutations(receiver, body, FoldOp::Detect)
    }
}
