// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Filter list operations: `select:` and `reject:`.

use super::super::super::document::Document;
use super::super::super::document::leaf;
use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CoreErlangGenerator, Result, block_analysis};
use super::super::{BodyKind, ThreadingPlan};
use crate::ast::{Block, Expression};
use crate::docvec;

impl CoreErlangGenerator {
    pub(in crate::codegen::core_erlang) fn generate_list_select(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "select:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list select: [:item | item > 0]",
        )?;

        // BT-904: Check if body has state-affecting operations
        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_filter_with_mutations(receiver, body_block, false);
            }
        }

        // Simple case: no mutations, use standard lists:filter
        self.generate_simple_list_op(receiver, body, "filter")
    }

    pub(in crate::codegen::core_erlang) fn generate_list_reject(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "reject:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list reject: [:item | item < 0]",
        )?;

        // BT-904: Check if body has state-affecting operations
        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_filter_with_mutations(receiver, body_block, true);
            }
        }

        // list reject: is opposite of filter - we need to negate the predicate
        // BT-416: Add runtime is_list guard for non-list receivers
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;
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
            " = fun (X) -> call 'erlang':'not'(apply ",
            leaf::var(body_var.clone()),
            " (X)) in call 'lists':'filter'(",
            leaf::var(wrapper_var),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", 'reject:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-904: Generates stateful `select:`/`reject:` using `lists:foldl` with state threading.
    ///
    /// In map mode: accumulator is `{ResultList, StateAcc}`.
    /// In tuple mode (BT-1276): accumulator is `{ResultList, Var1, ..., VarN}`.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_filter_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        negate: bool,
    ) -> Result<Document<'static>> {
        // BT-1276: Use tuple accumulator when eligible.
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        // BT-1489: Save recv var for is_binary check after foldl (String-aware result).
        let recv_var_for_str_check = list_var.clone();
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");

        if plan.use_tuple_acc {
            // BT-1276: Tuple-accumulator path.
            let vars_doc = plan.current_vars_doc(self); // Before push_scope.
            let init_tuple_doc = docvec!["{ [], ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(docvec![
                "let ",
                leaf::var(list_var.clone()),
                " = ",
                recv_code,
                " in let ",
                leaf::var(safe_list_var.clone()),
                " = case call 'erlang':'is_list'(",
                leaf::var(list_var.clone()),
                ") of <'true'> when 'true' -> ",
                leaf::var(list_var.clone()),
                " <'false'> when 'true' -> call 'beamtalk_collection':'to_list'(",
                leaf::var(list_var),
                ") end in let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let AccList = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlFilter {
                    item_var: item_var.clone(),
                    negate,
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let rev_list = self.fresh_temp_var("RevList");
            let final_list = self.fresh_temp_var("FinalList");

            // Extract each updated local var from tuple positions 2..N.
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 2, self);

            // BT-1489: String-aware result wrapping for filter ops.
            let (str_binding, str_result) =
                self.generate_string_aware_result_binding(&recv_var_for_str_check, &final_list);

            if self.in_direct_params_loop {
                // BT-1329: Skip StateAcc repack. Emit open let-chain so variable rebindings
                // escape to the outer scope. Store the result var for the caller.
                self.direct_params_list_op_result = Some(str_result);
                docs.push(docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(rev_list.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result),
                    ") in let ",
                    leaf::var(final_list.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_list),
                    ") in ",
                    str_binding.clone(),
                    " in ",
                    extract_doc,
                ]);
            } else {
                // BT-1276: Re-pack into StateAcc for outer method-body `maps:get` extraction.
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docs.push(docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(rev_list.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result),
                    ") in let ",
                    leaf::var(final_list.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_list),
                    ") in ",
                    str_binding.clone(),
                    " in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(str_result),
                    ", ",
                    leaf::var(stateacc),
                    "}",
                ]);
            }
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(safe_list_var.clone()),
            " = case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> ",
            leaf::var(list_var.clone()),
            " <'false'> when 'true' -> call 'beamtalk_collection':'to_list'(",
            leaf::var(list_var),
            ") end in let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(acc_state_var.clone()),
            ") -> let AccList = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var),
            ") in ",
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(
            body,
            &plan,
            &BodyKind::FoldlFilter {
                item_var: item_var.clone(),
                negate,
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let rev_list = self.fresh_temp_var("RevList");
        let final_list = self.fresh_temp_var("FinalList");
        let state_out = self.fresh_temp_var("StOut");

        // BT-1489: String-aware result wrapping for map-acc filter path.
        let (str_binding_doc, str_result) =
            self.generate_string_aware_result_binding(&recv_var_for_str_check, &final_list);

        let pre_doc = docvec![
            " in let ",
            leaf::var(fold_result.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(lambda_var),
            ", {[], ",
            leaf::var(init_state),
            "}, ",
            leaf::var(safe_list_var),
            ") in let ",
            leaf::var(rev_list.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(final_list.clone()),
            " = call 'lists':'reverse'(",
            leaf::var(rev_list),
            ") in ",
        ];
        let post_doc = docvec![
            " in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(str_result),
            ", ",
            leaf::var(state_out),
            "}",
        ];
        docs.push(docvec![pre_doc, str_binding_doc, post_doc]);

        Ok(Document::Vec(docs))
    }
}
