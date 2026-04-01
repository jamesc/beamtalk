// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Basic list iteration operations: `do:` and `collect:`.

use super::super::super::document::Document;
use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CodeGenContext, CoreErlangGenerator, Result, block_analysis};
use super::super::{BodyKind, ThreadingPlan};
use crate::ast::{Block, Expression};
use crate::docvec;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates code for `list do:` iteration.
    ///
    /// Analyzes the body block for state mutations and chooses the appropriate
    /// compilation strategy (pure functional vs stateful with threading).
    pub(in crate::codegen::core_erlang) fn generate_list_do(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "do:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list do: [:item | item printString]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Analyze block for mutations
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_do_with_mutations(receiver, body_block);
            }
        }

        // Simple case: no mutations, use standard lists:foreach
        self.generate_simple_list_op(receiver, body, "foreach")
    }

    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_do_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // BT-1276: Use tuple accumulator when eligible (no field/self-send mutations).
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        // BT-524: Add is_list guard for non-list collection types.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        if plan.use_tuple_acc {
            // BT-1276: Tuple-accumulator path — no StateAcc map allocation per iteration.
            // Initial fold accumulator: {Var1, ..., VarN} built from outer-scope bindings.
            let init_tuple_doc = plan.initial_vars_tuple_doc(self);

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(docvec![
                "let ",
                Document::String(list_var.clone()),
                " = ",
                recv_code,
                " in let ",
                Document::String(safe_list_var.clone()),
                " = case call 'erlang':'is_list'(",
                Document::String(list_var.clone()),
                ") of <'true'> when 'true' -> ",
                Document::String(list_var.clone()),
                " <'false'> when 'true' -> call 'beamtalk_collection':'to_list'(",
                Document::String(list_var),
                ") end in let ",
                Document::String(lambda_var.clone()),
                " = fun (",
                Document::String(item_var.clone()),
                ", StateAcc) -> ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            // Unpack vars from the tuple accumulator: element(1..N, StateAcc).
            docs.extend(plan.generate_tuple_unpack_docs(self, "StateAcc", 1));

            let (body_doc, _) =
                self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlDo)?;
            docs.push(body_doc);
            self.pop_scope();

            // After foldl: extract vars from result tuple, repack into StateAcc.
            let fold_result = self.fresh_temp_var("FoldResult");
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 1, self);
            let result_doc = if self.in_direct_params_loop {
                // BT-1329: In direct-params loop context, skip StateAcc repack and omit
                // trailing 'nil'. The extracted vars are left as open let-bindings so they
                // escape to the outer scope (the caller chains the next expression directly).
                // BT-1448: Signal open scope so the annotation guard in generate_expression
                // does not wrap this open let-chain in `( ... -| [...] )`.
                self.last_open_scope_result = Some("_".to_string());
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in ",
                    extract_doc,
                ]
            } else if matches!(plan.context, CodeGenContext::ValueType) {
                // ValueType context: skip StateAcc repack (no actor State to pack into).
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in ",
                    extract_doc,
                    "'nil'",
                ]
            } else {
                // BT-1276: Re-pack updated locals into StateAcc so the outer method-body
                // threading (`maps:get` in `generate_method_body_with_reply`) can extract them.
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{'nil', ",
                    Document::String(stateacc),
                    "}",
                ]
            };
            docs.push(result_doc);
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path (field mutations or complex control flow present).
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(
                " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> {list_var} \
                 <'false'> when 'true' -> \
                 call 'beamtalk_collection':'to_list'({list_var}) end \
                 in let {lambda_var} = fun ({item_var}, StateAcc) -> "
            ),
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlDo)?;
        docs.push(body_doc);
        self.pop_scope();

        // BT-598: After foldl, extract threaded locals and rebind them.
        let fold_result = self.fresh_temp_var("FoldResult");
        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {init_state}, {safe_list_var}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&fold_result, self));

        // BT-1053: Value types return 'nil' (no state); actors return {'nil', FinalState}.
        if !plan.threaded_locals.is_empty() && matches!(plan.context, CodeGenContext::ValueType) {
            post_code.push_str("'nil'");
        } else {
            let _ = write!(post_code, "{{'nil', {fold_result}}}");
        }
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    pub(in crate::codegen::core_erlang) fn generate_list_collect(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "collect:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list collect: [:item | item * 2]",
        )?;

        // BT-904: Check if body has state-affecting operations (self-sends, field writes)
        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_collect_with_mutations(receiver, body_block);
            }
        }

        // Simple case: no mutations, use standard lists:map
        self.generate_simple_list_op(receiver, body, "map")
    }

    /// BT-904: Generates stateful `collect:` using `lists:foldl` with state threading.
    ///
    /// In map mode: accumulator is `{ResultList, StateAcc}`.
    /// In tuple mode (BT-1276): accumulator is `{ResultList, Var1, ..., VarN}`.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_collect_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
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
            // Initial fold acc: {[], Var1, ..., VarN} (flat tuple, no StateAcc map).
            let vars_doc = plan.current_vars_doc(self); // Before push_scope.
            let init_tuple_doc = docvec!["{ [], ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(docvec![
                "let ",
                Document::String(list_var.clone()),
                " = ",
                recv_code,
                " in let ",
                Document::String(safe_list_var.clone()),
                " = case call 'erlang':'is_list'(",
                Document::String(list_var.clone()),
                ") of <'true'> when 'true' -> ",
                Document::String(list_var.clone()),
                " <'false'> when 'true' -> call 'beamtalk_collection':'to_list'(",
                Document::String(list_var),
                ") end in let ",
                Document::String(lambda_var.clone()),
                " = fun (",
                Document::String(item_var.clone()),
                ", ",
                Document::String(acc_state_var.clone()),
                ") -> let AccList = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            // Unpack vars starting at index 2 (slot 1 is AccList).
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

            let (body_doc, _) =
                self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlCollect)?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let rev_list = self.fresh_temp_var("RevList");
            let final_list = self.fresh_temp_var("FinalList");

            // Extract each updated local var from tuple positions 2..N.
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 2, self);

            // BT-1489: String-aware result wrapping — if the original receiver
            // was a binary (String), convert the reversed list back to a binary.
            let (str_binding, str_result) =
                self.generate_string_aware_result_binding(&recv_var_for_str_check, &final_list);

            if self.in_direct_params_loop {
                // BT-1329: Skip StateAcc repack. Emit open let-chain so variable rebindings
                // escape to the outer scope. Store the result var for the caller.
                self.direct_params_list_op_result = Some(str_result);
                docs.push(docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(rev_list.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in let ",
                    Document::String(final_list.clone()),
                    " = call 'lists':'reverse'(",
                    Document::String(rev_list),
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
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(rev_list.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in let ",
                    Document::String(final_list.clone()),
                    " = call 'lists':'reverse'(",
                    Document::String(rev_list),
                    ") in ",
                    str_binding.clone(),
                    " in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    Document::String(str_result),
                    ", ",
                    Document::String(stateacc),
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
            format!("let {list_var} = "),
            recv_code,
            format!(
                " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> {list_var} \
                 <'false'> when 'true' -> \
                 call 'beamtalk_collection':'to_list'({list_var}) end \
                 in let {lambda_var} = fun ({item_var}, {acc_state_var}) -> \
                 let AccList = call 'erlang':'element'(1, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(2, {acc_state_var}) in "
            ),
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) =
            self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlCollect)?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let rev_list = self.fresh_temp_var("RevList");
        let final_list = self.fresh_temp_var("FinalList");
        let state_out = self.fresh_temp_var("StOut");

        // BT-1489: String-aware result wrapping for map-acc path.
        let (str_binding_doc, str_result) =
            self.generate_string_aware_result_binding(&recv_var_for_str_check, &final_list);

        let pre_str = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {{[], {init_state}}}, {safe_list_var}) \
             in let {rev_list} = call 'erlang':'element'(1, {fold_result}) \
             in let {final_list} = call 'lists':'reverse'({rev_list}) in "
        );
        let mut post_str =
            format!(" in let {state_out} = call 'erlang':'element'(2, {fold_result}) in ");
        post_str.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_str, "{{{str_result}, {state_out}}}");
        docs.push(docvec![
            Document::String(pre_str),
            str_binding_doc,
            Document::String(post_str),
        ]);

        Ok(Document::Vec(docs))
    }
}
