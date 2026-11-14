// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Filter list operations: `select:` and `reject:`.

use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CoreErlangGenerator, Result};
use super::fold_driver::FoldOp;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

impl CoreErlangGenerator {
    pub(in crate::core_erlang) fn generate_list_select(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "select:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list select: [:item | item > 0]",
        )?;

        // Check if body has state-affecting operations
        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_filter_with_mutations(receiver, body_block, false);
        }

        // Simple case: no mutations, use standard lists:filter
        self.generate_simple_list_op(receiver, body, "filter")
    }

    pub(in crate::core_erlang) fn generate_list_reject(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "reject:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list reject: [:item | item < 0]",
        )?;

        // Check if body has state-affecting operations
        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_filter_with_mutations(receiver, body_block, true);
        }

        // list reject: is opposite of filter - we need to negate the predicate
        // Add runtime is_list guard for non-list receivers
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.list_op_callable_doc(body, "reject:", 1)?;
        let wrapper_var = self.fresh_temp_var("temp");

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
            ") of <'true'> when 'true' -> let ",
            leaf::var(wrapper_var.clone()),
            " = fun (X) -> call 'erlang':'not'(",
            // BT-4366: nested in the wrapper fun, so the apply carries the
            // send's line itself.
            self.annotate_current_send_call(docvec!["apply ", leaf::var(body_var.clone()), " (X)"]),
            ") in call 'lists':'filter'(",
            leaf::var(wrapper_var),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> ",
            // BT-4366: the fallback send is nested in the list-type `case`, so the
            // call node carries the send's line itself.
            self.annotate_current_send_call(docvec![
                "call 'beamtalk_message_dispatch':'send'(",
                leaf::var(list_var),
                ", 'reject:', [",
                leaf::var(body_var),
                "])"
            ]),
            " end",
        ])
    }

    /// Generates stateful `select:`/`reject:` using `lists:foldl` with state threading.
    pub(in crate::core_erlang) fn generate_list_filter_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        negate: bool,
    ) -> Result<Document<'static>> {
        let op = if negate {
            FoldOp::Reject
        } else {
            FoldOp::Select
        };
        self.generate_fold_with_mutations(receiver, body, op)
    }
}
