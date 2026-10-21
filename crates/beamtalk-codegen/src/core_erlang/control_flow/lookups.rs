// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Dictionary` lookups (`at:ifAbsent:`, `at:ifPresent:`,
//! `at:ifPresent:ifAbsent:`) whose handler writes an outer local, as
//! local-threading producers.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ADR 0131 Phase 2d (BT-3798, BT-3824). `r := d at: #k ifAbsent: [t := t + 1. 0]`
//! cannot hand the handler to the runtime as a closure: the handler is Tier 2
//! (it takes and returns the `StateAcc` carrying `t`), and the primitive calls
//! it with its own arity. The receiver type is unknown at compile time too: the
//! selector is implemented by the `Dictionary` primitive, by stdlib methods
//! that call `block value` themselves (`Ets at:ifAbsent:`, `Ets at:ifPresent:`)
//! and by any user class, and an inline lowering is only valid for the first.
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
//! case call 'beamtalk_map':'lookup_for_threading'(Recv, <selector>, Key) of
//!   <{'ok', Found}> when 'true' -> <found arm>
//!   <_> when 'true' -> <absent arm>
//! end
//! ```
//!
//! Each arm is the row's handler for that case, run as
//! `let StateAcc = SeededState in <handler arm>` (`{Value, StateAcc'}`), or,
//! when the selector has no handler for it, what the primitive answers there:
//!
//! | selector | found arm | absent arm |
//! |---|---|---|
//! | `at:ifAbsent:` | `{Found, SeededState}` | the handler |
//! | `at:ifPresent:` | the handler, its parameter bound to `Found` | `{'nil', SeededState}` |
//! | `at:ifPresent:ifAbsent:` | the present handler, its parameter bound to `Found` | the absent handler |
//!
//! A handler is the found arm exactly when the row's
//! [`block_params`](beamtalk_core::state_threading_selectors::LookupShape::block_params)
//! give it one parameter (the found value); `lookup_arm_blocks` admits a
//! handler only with that parameter count.
//!
//! A receiver statically known not to be a `Dictionary` never gets here: the
//! Phase 0 check keeps its §6 error (`receiver_statically_not_dictionary`
//! in core), so the guard serves receivers whose class is not known.
//!
//! The result tuple, the handlers' `BranchArm` frames and the `LocalRebind`s
//! that bring the threaded locals back are the conditional producer's
//! (`conditional_tuple_to_threaded_value`): core's `conditional_arm_blocks`
//! names the handlers as the lookup's arms, so the Phase 0 check and this
//! lowering read the same rule.

use super::super::threaded_expr::ProducerTail;
use super::super::threaded_ir::{ThreadedStmt, ThreadedValue};
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use super::conditionals::ScratchStart;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression, MessageSelector};
use beamtalk_core::semantic_analysis::block_facts::lookup_arm_blocks;
use beamtalk_core::state_threading_selectors::lookup_shape;

/// The handlers of an inline lookup, by the case they run in.
struct LookupArms<'a> {
    /// Runs with the found value bound to its parameter.
    found: Option<&'a Block>,
    /// Runs when the key is missing.
    absent: Option<&'a Block>,
}

impl CoreErlangGenerator {
    /// The producer lowering of the lookup send `expr` (paren-unwrapped), or
    /// `Ok(None)` when it is not one core's `lookup_arm_blocks` names. The
    /// caller has already decided the send is a local-threading producer in
    /// this frame ([`Self::conditional_producer_locals`]): at the method root, in
    /// an arm, or (BT-3839) at a loop or fold body's producer site, where the
    /// arms' `LocalRebind`s lower by the frame's mode as a conditional's do.
    pub(in crate::core_erlang) fn generate_lookup_producer(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
    ) -> Result<Option<ThreadedValue>> {
        let Some(handlers) = lookup_arm_blocks(expr) else {
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
        let keys: Vec<&Expression> = arguments
            .iter()
            .enumerate()
            .filter(|(i, _)| !shape.block_args.contains(i))
            .map(|(_, a)| a)
            .collect();
        // Each handler (source order, parallel to `block_params`) is the found
        // arm when it takes the found value, else the absent arm.
        let mut arms = LookupArms {
            found: None,
            absent: None,
        };
        let mut well_formed = handlers.len() == shape.block_params.len();
        for (&handler, &params) in handlers.iter().zip(shape.block_params) {
            let slot = if params == 1 {
                &mut arms.found
            } else {
                &mut arms.absent
            };
            well_formed &= params <= 1 && slot.replace(handler).is_none();
        }
        let ([key], true) = (keys.as_slice(), well_formed) else {
            return Err(CodeGenError::Internal(format!(
                "lookup `{selector}` has no inline lowering (ADR 0131 Phase 2d)"
            )));
        };
        let span = handlers[0].span;
        let frame = self.current_frame();
        let scratch = self.conditional_scratch(frame);
        let (head, tuple_doc) =
            self.generate_lookup_tuple(receiver, &selector, key, &arms, &handlers, scratch)?;
        let mut tv =
            self.conditional_tuple_to_threaded_value(tuple_doc, frame, span, &handlers, tail, true)?;
        if !head.is_empty() {
            // ADR 0137: the receiver's and the key's own preludes (a bracketed
            // call steps `State`) belong to the producer's frame, ahead of the
            // tuple, not inside the tuple's document where their versions die.
            let mut prelude = head;
            prelude.append(&mut tv.prelude);
            tv.prelude = prelude;
        }
        Ok(Some(tv))
    }

    /// The raw `{Value, StateAcc}` case of [`Self::generate_lookup_producer`]
    /// (see the module docs for its shape).
    fn generate_lookup_tuple(
        &mut self,
        receiver: &Expression,
        selector: &str,
        key: &Expression,
        arms: &LookupArms<'_>,
        handlers: &[&Block],
        scratch: ScratchStart,
    ) -> Result<(Vec<ThreadedStmt>, Document<'static>)> {
        let frame = self.current_frame();
        // The receiver is evaluated first and its value bound before the key's
        // own prelude (an actor self-send in the key) can run.
        let recv_tv = self.threaded_expression(receiver, frame)?;
        let recv_value = self.threaded_value_doc(&recv_tv.value);
        let recv_var = self.fresh_temp_var("LookupRecv");
        let key_tv = self.threaded_expression(key, frame)?;
        let key_value = self.threaded_value_doc(&key_tv.value);
        let key_var = self.fresh_temp_var("LookupKey");
        // With the synced-calls flag on, a non-empty prelude is spliced into the
        // producer's frame (see the caller) instead of rendered into the tuple.
        let splice = self.synced_calls_active()
            && !(recv_tv.prelude.is_empty() && key_tv.prelude.is_empty());
        let mut head: Vec<ThreadedStmt> = Vec::new();
        let (recv_preamble, key_preamble) = if splice {
            head.extend(recv_tv.prelude);
            head.push(ThreadedStmt::Statement(
                docvec!["let ", leaf::var(recv_var.clone()), " = ", recv_value.clone(), " in "],
                receiver.span(),
            ));
            head.extend(key_tv.prelude);
            head.push(ThreadedStmt::Statement(
                docvec!["let ", leaf::var(key_var.clone()), " = ", key_value.clone(), " in "],
                key.span(),
            ));
            (Document::Nil, Document::Nil)
        } else {
            (
                self.threaded_prelude_doc(&recv_tv.prelude),
                self.threaded_prelude_doc(&key_tv.prelude),
            )
        };
        let found_var = self.fresh_temp_var("LookupFound");
        let outer_state = self.current_state_var();
        // Seed the construct's own locals so every handler arm, the
        // handler-less path and the post-lookup extraction all see every
        // `__local__` key.
        let (seed_doc, base_state) = self.seed_scratch_locals(handlers, scratch, &outer_state);
        let arm_frame = self.conditional_arm_frame(handlers, true);
        let found_arm = match arms.found {
            Some(handler) => self.conditional_arm_clause(
                handler,
                Some(&found_var),
                arm_frame.as_ref(),
                &base_state,
            )?,
            None => self.conditional_baseline_tuple(leaf::var(found_var.clone()), &base_state),
        };
        let absent_arm = match arms.absent {
            Some(handler) => {
                self.conditional_arm_clause(handler, None, arm_frame.as_ref(), &base_state)?
            }
            None => self.conditional_baseline_tuple(Document::Str("'nil'"), &base_state),
        };
        let (recv_let, key_let) = if splice {
            (Document::Nil, Document::Nil)
        } else {
            (
                docvec!["let ", leaf::var(recv_var.clone()), " = ", recv_value, " in "],
                docvec!["let ", leaf::var(key_var.clone()), " = ", key_value, " in "],
            )
        };
        Ok((head, docvec![
            recv_preamble,
            recv_let,
            key_preamble,
            key_let,
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
            " <_> when 'true' -> ",
            absent_arm,
            " end",
        ]))
    }
}
