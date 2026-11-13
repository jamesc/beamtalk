// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Basic list iteration operations: `do:` and `collect:`.

use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CoreErlangGenerator, Result};
use super::super::family_slots::FoldFamilySlots;
use super::super::{BodyKind, ListOpKind, ThreadingPlan};
use super::fold_driver::{AccumulateOp, FoldHead};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

impl CoreErlangGenerator {
    /// Generates code for `list do:` iteration.
    ///
    /// Analyzes the body block for state mutations and chooses the appropriate
    /// compilation strategy (pure functional vs stateful with threading).
    pub(in crate::core_erlang) fn generate_list_do(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "do:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list do: [:item | item printString]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_do_with_mutations(receiver, body_block);
        }

        // Simple case: no mutations, use standard lists:foreach
        self.generate_simple_list_op(receiver, body, "foreach")
    }

    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_list_do_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // Use tuple accumulator when eligible (no field/self-send mutations).
        let FoldHead {
            plan,
            discard,
            list_var,
            recv_code,
            safe_list_var,
            lambda_var,
            item_var,
        } = self.begin_list_fold(receiver, body, ListOpKind::Do, "do:", true)?;

        if plan.use_tuple_acc {
            // Tuple-accumulator path — no StateAcc map allocation per iteration.
            // Initial fold accumulator: {Var1, ..., VarN} built from outer-scope bindings.
            let init_tuple_doc = plan.initial_vars_tuple_doc(self);
            // BT-4178: the accumulator is a tuple, so it gets a fresh name
            // that cannot shadow an enclosing `StateAcc` map the body reads.
            let acc_var = self.fresh_temp_var("AccSt");

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                None,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_var.clone()),
                ") -> ",
            ]);

            // Unpack vars from the tuple accumulator (element(1..N, AccSt))
            // and lower the body, merged into ONE verified `Threaded` node.
            let lowered = self.lower_tuple_fold_body(
                body,
                &plan,
                &BodyKind::FoldlDo,
                &item_var,
                &acc_var,
                0,
            )?;
            docs.push(lowered.body_doc);
            docs.insert(0, lowered.prestore);

            // After foldl: extract vars from result tuple, repack into StateAcc.
            let fold_result = self.fresh_temp_var("FoldResult");
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 1, self);
            let result_doc = if self.loop_mode.in_direct_params_loop {
                // In direct-params loop context, skip StateAcc repack and omit
                // trailing 'nil'. The extracted vars are left as open let-bindings so they
                // escape to the outer scope (the caller chains the next expression directly).
                // ADR 0118 phase 5b: Signal open scope with no
                // single value (multiple accumulator vars may have been rebound; `do:`
                // itself always answers `nil`) so the annotation guard in
                // generate_expression does not wrap this open let-chain in `( ... -|
                // [annotation] )`, and `threaded_expression`'s generic fallback
                // (`generate_expression_as_value`) substitutes `do:`'s own `nil` contract
                // instead of referencing a nonexistent variable.
                self.loop_mode.direct_params_do_open_chain = true;
                docvec![
                    ThreadingPlan::foldl_call_doc(
                        &lambda_var,
                        init_tuple_doc,
                        &safe_list_var,
                        &fold_result
                    ),
                    extract_doc,
                ]
            } else {
                // Re-pack updated locals into StateAcc so the outer method-body
                // threading (`maps:get` in `lower_body_exprs_with_reply`) can extract them.
                let tail = plan.closed_fold_tail_doc(
                    self,
                    extract_doc,
                    lowered.guarded_op_bracket,
                    discard,
                );
                docvec![
                    ThreadingPlan::foldl_call_doc(
                        &lambda_var,
                        init_tuple_doc,
                        &safe_list_var,
                        &fold_result
                    ),
                    tail,
                ]
            };
            docs.push(result_doc);
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path (field mutations or complex control flow present).
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            None,
            list_var,
            safe_list_var.clone(),
        ));
        // BT-3827: a value-type body that writes a field carries `Self` in a
        // `{Acc, Self}` wrapper around the accumulator.
        let slots = plan.fold_family_slots(self);
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(
                slots
                    .as_ref()
                    .map_or("StateAcc", FoldFamilySlots::lambda_param)
                    .to_string()
            ),
            ") -> ",
            slots
                .as_ref()
                .map_or(Document::Nil, |s| s.lambda_prologue("StateAcc")),
        ]);

        docs.push(self.lower_map_fold_body(
            body,
            &plan,
            &BodyKind::FoldlDo,
            &item_var,
            "StateAcc",
            0,
        )?);

        // After foldl, extract threaded locals and rebind them.
        let fold_result = self.fresh_temp_var("FoldResult");
        let init_acc = match &slots {
            Some(s) => s.wrap_init(leaf::var(init_state), self),
            None => leaf::var(init_state),
        };
        let fold_out = slots
            .as_ref()
            .map_or(fold_result.as_str(), FoldFamilySlots::result_var);
        let mut post_docs: Vec<Document<'static>> = vec![ThreadingPlan::foldl_call_doc(
            &lambda_var,
            init_acc,
            &safe_list_var,
            fold_out,
        )];
        if let Some(s) = &slots {
            post_docs.push(s.unwrap_result(&fold_result));
        }
        post_docs.push(plan.generate_extract_suffix_doc(&fold_result, self));

        let result_open = docvec!["{'nil', ", leaf::var(fold_result)];
        post_docs.push(match &slots {
            Some(s) => s.close_tuple(result_open, self),
            None => docvec![result_open, "}"],
        });
        docs.push(Document::Vec(post_docs));

        Ok(Document::Vec(docs))
    }

    pub(in crate::core_erlang) fn generate_list_collect(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "collect:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list collect: [:item | item * 2]",
        )?;

        // Check if body has state-affecting operations (self-sends, field writes)
        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_collect_with_mutations(receiver, body_block);
        }

        // Simple case: no mutations, use standard lists:map
        self.generate_simple_list_op(receiver, body, "map")
    }

    /// Generates stateful `collect:` using `lists:foldl` with state threading.
    pub(in crate::core_erlang) fn generate_list_collect_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_accumulate_fold_with_mutations(receiver, body, AccumulateOp::Collect)
    }
}
