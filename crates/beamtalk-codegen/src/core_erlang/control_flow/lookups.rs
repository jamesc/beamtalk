// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Dictionary` lookups (`at:ifAbsent:`) whose handler writes an outer local,
//! as local-threading producers.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ADR 0131 Phase 2d (BT-3798). `r := d at: #k ifAbsent: [t := t + 1. 0]`
//! cannot hand the handler to the runtime as a closure: the handler is Tier 2
//! (it takes and returns the `StateAcc` carrying `t`), and the primitive calls
//! it with arity 0. The receiver type is unknown at compile time too: the
//! selector is implemented by the `Dictionary` primitive, by stdlib methods
//! that call `block value` themselves (`Ets at:ifAbsent:`) and by any user
//! class, and an inline lowering is only valid for the first.
//!
//! So the send is lowered like a conditional whose condition is a runtime
//! receiver guard (`beamtalk_map:lookup_for_threading/3`): a `Dictionary`
//! receiver answers `{ok, Value}` or `error`, any other receiver raises a
//! structured `#beamtalk_error{}` before the handler could be dropped.
//!
//! # Generated Pattern
//!
//! ```erlang
//! let Recv = <receiver> in
//! let Key = <key> in
//! let SeededState = <the construct's own locals> in
//! case call 'beamtalk_map':'lookup_for_threading'(Recv, 'at:ifAbsent:', Key) of
//!   <{'ok', Found}> when 'true' -> {Found, SeededState}
//!   <_> when 'true' -> let StateAcc = SeededState in <handler arm>   % {Value, StateAcc'}
//! end
//! ```
//!
//! A receiver statically known not to be a `Dictionary` never gets here: the
//! Phase 0 check keeps its §6 error (`lookup_receiver_statically_not_dictionary`
//! in core), so the guard serves receivers whose class is not known.
//!
//! The result tuple, the handler's `BranchArm` frame and the `LocalRebind`s
//! that bring the threaded locals back are the conditional producer's
//! (`conditional_tuple_to_threaded_value`): core's `conditional_arm_blocks`
//! names the handler as the lookup's arm, so the Phase 0 check and this
//! lowering read the same rule.

use super::super::threaded_expr::ProducerTail;
use super::super::threaded_ir::ThreadedValue;
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use super::conditionals::ScratchStart;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression, MessageSelector};
use beamtalk_core::semantic_analysis::block_facts::lookup_arm_blocks;
use beamtalk_core::state_threading_selectors::lookup_shape;

impl CoreErlangGenerator {
    /// The producer lowering of the lookup send `expr` (paren-unwrapped), or
    /// `Ok(None)` when it is not one core's `lookup_arm_blocks` names. The
    /// caller has already decided the send is a local-threading producer in
    /// this frame ([`Self::conditional_producer_locals`]); a lookup is a producer
    /// only at the method root or in an arm, never at a loop or fold body site.
    pub(in crate::core_erlang) fn generate_lookup_producer(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
    ) -> Result<Option<ThreadedValue>> {
        let Some(arms) = lookup_arm_blocks(expr) else {
            return Ok(None);
        };
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return Ok(None);
        };
        let selector: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let Some(shape) = lookup_shape(&selector) else {
            return Ok(None);
        };
        // Every `dictionary_inline` row has its own result shape; today's only
        // one is `at:ifAbsent:` (the key, then the absent handler).
        let keys: Vec<&Expression> = arguments
            .iter()
            .enumerate()
            .filter(|(i, _)| !shape.block_args.contains(i))
            .map(|(_, a)| a)
            .collect();
        let (&[1], [handler], [key]) = (shape.block_args, arms.as_slice(), keys.as_slice()) else {
            return Err(CodeGenError::Internal(format!(
                "lookup `{selector}` has no inline lowering (ADR 0131 Phase 2d)"
            )));
        };
        let span = handler.span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let tuple_doc = self.generate_lookup_tuple(receiver, &selector, key, handler, scratch)?;
        self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &[handler], tail, true)
            .map(Some)
    }

    /// The raw `{Value, StateAcc}` case of [`Self::generate_lookup_producer`]
    /// (see the module docs for its shape).
    fn generate_lookup_tuple(
        &mut self,
        receiver: &Expression,
        selector: &str,
        key: &Expression,
        handler: &Block,
        scratch: ScratchStart,
    ) -> Result<Document<'static>> {
        let frame = self.current_frame();
        // The receiver is evaluated first and its value bound before the key's
        // own prelude (an actor self-send in the key) can run.
        let recv_tv = self.threaded_expression(receiver, frame)?;
        let recv_preamble = self.threaded_prelude_doc(&recv_tv.prelude);
        let recv_value = self.threaded_value_doc(&recv_tv.value);
        let recv_var = self.fresh_temp_var("LookupRecv");
        let key_tv = self.threaded_expression(key, frame)?;
        let key_preamble = self.threaded_prelude_doc(&key_tv.prelude);
        let key_value = self.threaded_value_doc(&key_tv.value);
        let key_var = self.fresh_temp_var("LookupKey");
        let found_var = self.fresh_temp_var("LookupFound");
        let outer_state = self.current_state_var();
        // Seed the construct's own locals so the handler arm, the found path
        // and the post-lookup extraction all see every `__local__` key.
        let (seed_doc, base_state) = self.seed_scratch_locals(&[handler], scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(&[handler], true);
        let (handler_doc, _final) =
            self.with_branch_context(|this| this.conditional_arm(handler, arm_frame.as_ref()))?;
        let found_arm = self.conditional_baseline_tuple(leaf::var(found_var.clone()), &base_state);
        Ok(docvec![
            recv_preamble,
            "let ",
            leaf::var(recv_var.clone()),
            " = ",
            recv_value,
            " in ",
            key_preamble,
            "let ",
            leaf::var(key_var.clone()),
            " = ",
            key_value,
            " in ",
            seed_doc,
            "case call 'beamtalk_map':'lookup_for_threading'(",
            leaf::var(recv_var),
            ", ",
            leaf::atom(selector.to_string()),
            ", ",
            leaf::var(key_var),
            ") of <{'ok', ",
            leaf::var(found_var),
            "}> when 'true' -> ",
            found_arm,
            " <_> when 'true' -> let StateAcc = ",
            leaf::var(base_state),
            " in ",
            handler_doc,
            " end",
        ])
    }
}
