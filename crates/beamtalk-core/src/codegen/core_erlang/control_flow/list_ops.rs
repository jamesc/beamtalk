// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List iteration control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for list iteration constructs: `do:`, `collect:`,
//! `select:`, `reject:`, and `inject:into:`.

use super::super::document::Document;
use super::super::intrinsics::validate_block_arity_exact;
use super::super::{CodeGenContext, CoreErlangGenerator, Result, block_analysis};
use super::{BodyKind, ThreadingPlan};
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
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 let {wrapper_var} = fun (X) -> call 'erlang':'not'(apply {body_var} (X)) \
                 in call 'lists':'filter'({wrapper_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'reject:', [{body_var}]) end"
            ),
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

    pub(in crate::codegen::core_erlang) fn generate_simple_list_op(
        &mut self,
        receiver: &Expression,
        body: &Expression,
        operation: &str,
    ) -> Result<Document<'static>> {
        // BT-416: Map Erlang list operation to Beamtalk selector for runtime fallback
        let selector = match operation {
            "foreach" => "do:",
            "map" => "collect:",
            "filter" => "select:",
            _ => operation,
        };

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");

        // BT-855: When the body is a stateful block (captured mutations), wrap it so
        // Erlang receives a plain fun(Args) -> Result without the StateAcc protocol.
        // Mutations inside the block are dropped (Erlang cannot propagate NewStateAcc).
        // BT-855 follow-up: Also unwrap parenthesized block literals (e.g. `([:x | ...])`).
        let body_code = if let Some(block) = Self::extract_block_literal(body) {
            let (wrapped_doc, is_stateful) = self.generate_erlang_interop_wrapper(block)?;
            if is_stateful {
                self.warn_stateful_block_at_erlang_boundary(
                    &format!("'lists':'{operation}'"),
                    block.span,
                );
            }
            wrapped_doc
        } else {
            // BT-909: Non-literal callable — emit a runtime arity check that wraps
            // Tier-2 (arity 2) blocks to satisfy the arity-1 contract expected by
            // lists:foreach / lists:map / lists:filter.
            //
            // Generated pattern (uses is_function/2 to avoid badarg on non-funs):
            //   let _Callable = <expr> in
            //   case call 'erlang':'is_function'(_Callable, 1) of
            //     <'true'> when 'true' -> _Callable           -- arity-1, pass through
            //     <'false'> when 'true' ->
            //       case call 'erlang':'is_function'(_Callable, 2) of
            //         <'true'> when 'true' -> fun (_WArg) ->  -- Tier-2, wrap it
            //           let _T = apply _Callable (_WArg, <SeedState>) in
            //           let _WRes = call 'erlang':'element'(1, _T) in _WRes
            //         <'false'> when 'true' -> _Callable      -- not a fun, pass through
            //       end
            //   end
            //
            // NOTE 1: `let {_WRes, _} = apply ...` is invalid Core Erlang inside a fun
            // body (erlc rejects tuple patterns in let). Use element/2 calls instead.
            // NOTE 2: In Core Erlang, `fun (Params) -> Body` does NOT use `end` to
            // terminate the fun — the Body expression ends the fun. Two `end`s close
            // the two nested `case` expressions.
            //
            // SeedState is current_state_var() for Actor/Repl; for ValueType there is
            // no State in scope so ~{}~ is bound to a fresh variable first.
            self.warn_non_literal_callable_at_erlang_boundary(
                &format!("'lists':'{operation}'"),
                body.span(),
            );
            let callable_var = self.fresh_temp_var("Callable");
            let raw_code = self.expression_doc(body)?;
            let wrap_arg = self.fresh_temp_var("WArg");
            let wrap_tuple = self.fresh_temp_var("T");
            let wrap_res = self.fresh_temp_var("WRes");

            // Seed StateAcc: for value types there is no State variable in scope.
            // Bind ~{}~ to a fresh variable (mirroring generate_erlang_interop_wrapper)
            // so ~{}~ is not used as a literal in an apply argument position.
            let (state_preamble, state_var): (Document<'static>, String) =
                if matches!(self.context, CodeGenContext::ValueType) {
                    let sv = self.fresh_temp_var("EmptyState");
                    let pre = docvec!["let ", Document::String(sv.clone()), " = ~{}~ in "];
                    (pre, sv)
                } else {
                    (Document::Str(""), self.current_state_var())
                };

            // Use is_function/2 instead of fun_info to avoid exception on non-functions
            docvec![
                "let ",
                Document::String(callable_var.clone()),
                " = ",
                raw_code,
                " in case call 'erlang':'is_function'(",
                Document::String(callable_var.clone()),
                ", 1) of <'true'> when 'true' -> ",
                Document::String(callable_var.clone()),
                " <'false'> when 'true' -> ",
                "case call 'erlang':'is_function'(",
                Document::String(callable_var.clone()),
                ", 2) of <'true'> when 'true' -> fun (",
                Document::String(wrap_arg.clone()),
                ") -> ",
                state_preamble,
                "let ",
                Document::String(wrap_tuple.clone()),
                " = apply ",
                Document::String(callable_var.clone()),
                " (",
                Document::String(wrap_arg),
                ", ",
                Document::String(state_var),
                ") in let ",
                Document::String(wrap_res.clone()),
                " = call 'erlang':'element'(1, ",
                Document::String(wrap_tuple),
                ") in ",
                Document::String(wrap_res),
                " <'false'> when 'true' -> ",
                Document::String(callable_var),
                " end end",
            ]
        };

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'lists':'{operation}'({body_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, '{selector}', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1481: Generates code for `list anySatisfy:` with mutation analysis.
    pub(in crate::codegen::core_erlang) fn generate_list_any_satisfy(
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

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self
                    .generate_list_bool_predicate_with_mutations(receiver, body_block, false);
            }
        }

        // No mutations: fall through to simple BIF call (lists:any/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'lists':'any'({body_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'anySatisfy:', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1481: Generates code for `list allSatisfy:` with mutation analysis.
    pub(in crate::codegen::core_erlang) fn generate_list_all_satisfy(
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

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self
                    .generate_list_bool_predicate_with_mutations(receiver, body_block, true);
            }
        }

        // No mutations: fall through to simple BIF call (lists:all/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'lists':'all'({body_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'allSatisfy:', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1481: Generates stateful `anySatisfy:`/`allSatisfy:` using `lists:foldl`
    /// with state threading and a boolean accumulator.
    ///
    /// Note: Unlike `lists:any/2` and `lists:all/2`, this does NOT short-circuit.
    /// All elements are always processed because field mutations must execute for
    /// every element to maintain correct state.
    ///
    /// Accumulator is `{BoolAcc, StateVars...}` (tuple) or `{BoolAcc, StateAcc}` (map).
    /// - `is_all = false` (anySatisfy): `BoolAcc` starts `false`, set to `true` on match
    /// - `is_all = true` (allSatisfy): `BoolAcc` starts `true`, set to `false` on failure
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_bool_predicate_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        is_all: bool,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");
        let init_bool = if is_all { "'true'" } else { "'false'" };

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {BoolAcc, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{", init_bool, ", ", vars_doc, "}"];

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
                ") -> let BoolAcc = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
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
                &BodyKind::FoldlBoolPredicate { is_all },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let bool_result = self.fresh_temp_var("BoolResult");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 2, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(bool_result.clone());
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
                    Document::String(bool_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                ]);
            } else {
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
                    Document::String(bool_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    Document::String(bool_result),
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
                 let BoolAcc = call 'erlang':'element'(1, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(2, {acc_state_var}) in "
            ),
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(
            body,
            &plan,
            &BodyKind::FoldlBoolPredicate { is_all },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let bool_result = self.fresh_temp_var("BoolResult");
        let state_out = self.fresh_temp_var("StOut");

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, \
             {{{init_bool}, {init_state}}}, {safe_list_var}) \
             in let {bool_result} = call 'erlang':'element'(1, {fold_result}) \
             in let {state_out} = call 'erlang':'element'(2, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{bool_result}, {state_out}}}");
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    /// BT-1486: Generates code for `list detect:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:detect/2` BIF.
    /// With mutations: uses `lists:foldl` to process all elements (no short-circuit)
    /// so that field mutations are applied for every element. Returns the first
    /// matching element or `nil`.
    pub(in crate::codegen::core_erlang) fn generate_list_detect(
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

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_detect_with_mutations(receiver, body_block);
            }
        }

        // No mutations: fall through to BIF call (beamtalk_list:detect/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'beamtalk_list':'detect'({list_var}, {body_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'detect:', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1486: Generates code for `list detect:ifNone:` with mutation analysis.
    ///
    /// Without mutations: falls through to runtime dispatch.
    /// With mutations: uses `lists:foldl` like `detect:`, then applies the ifNone
    /// block if no match was found.
    pub(in crate::codegen::core_erlang) fn generate_list_detect_if_none(
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

        if let Expression::Block(pred_block) = predicate {
            let analysis = block_analysis::analyze_block(pred_block);
            if self.needs_mutation_threading(&analysis) {
                return self
                    .generate_list_detect_if_none_with_mutations(receiver, pred_block, if_none);
            }
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
        let pred_code = self.expression_doc(predicate)?;
        let none_var = self.fresh_temp_var("temp");
        let none_code = self.expression_doc(if_none)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {pred_var} = "),
            pred_code,
            format!(" in let {none_var} = "),
            none_code,
            format!(
                " in call 'beamtalk_primitive':'send'({list_var}, \
                 'detect:ifNone:', [{pred_var}, {none_var}])"
            ),
        ])
    }

    /// BT-1486: Wrapper for `detect:ifNone:` with mutations — runs the detect foldl,
    /// then checks the `FoundFlag` and evaluates the ifNone block when no match was found.
    ///
    /// The detect foldl accumulator uses `{FoundItem, FoundFlag, State...}`. We extract
    /// `FoundFlag` from the fold result to distinguish "found nil" from "not found" —
    /// checking `FoundItem == nil` would be incorrect when nil is a valid search result.
    #[allow(clippy::too_many_lines)]
    fn generate_list_detect_if_none_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        if_none: &Expression,
    ) -> Result<Document<'static>> {
        // The detect foldl generates code that processes all elements and returns
        // {FoundItem, StateAcc}. But we need the FoundFlag to distinguish found-nil
        // from not-found. Rather than restructuring the entire detect codegen, we
        // use a 3-element fold result: {FoundItem, FoundFlag, StateAcc}.
        //
        // Strategy: call generate_list_detect_with_mutations which already uses
        // {FoundItem, FoundFlag, State...} internally. But the external result is
        // {FoundItem, StateAcc} (FoundFlag is extracted and discarded). We need to
        // preserve FoundFlag in the external result.
        //
        // Alternative: inline the foldl codegen here with a modified post-processing.
        // For simplicity, we use a wrapper that examines the foldl result directly.
        //
        // The inner foldl produces result tuple: {FoundItem, FoundFlag, State...}
        // The outer detect codegen extracts element(1) for FoundItem and element(3) for State.
        // We need element(2) for FoundFlag too.
        //
        // Since generate_list_detect_with_mutations already does the extraction and
        // repacking, we'll use a different approach: emit the full foldl inline here
        // but return {FoundItem, FoundFlag, StateAcc} and post-process.

        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");

        // Compile the ifNone block upfront.
        let none_code = self.expression_doc(if_none)?;

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {FoundItem, FoundFlag, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{'nil', 'false', ", vars_doc, "}"];

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
                ") -> let FoundItem = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
                ") in let FoundFlag = call 'erlang':'element'(2, ",
                Document::String(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 3));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlDetect {
                    item_var: item_var.clone(),
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let found_item = self.fresh_temp_var("FoundItem");
            let found_flag = self.fresh_temp_var("FoundFlag");
            let final_result = self.fresh_temp_var("FinalResult");
            let none_result = self.fresh_temp_var("NoneResult");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 3, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(final_result.clone());
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
                    Document::String(found_item.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result.clone()),
                    ") in let ",
                    Document::String(found_flag.clone()),
                    " = call 'erlang':'element'(2, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    "let ",
                    Document::String(final_result.clone()),
                    " = case ",
                    Document::String(found_flag),
                    " of <'true'> when 'true' -> ",
                    Document::String(found_item.clone()),
                    " <'false'> when 'true' -> let ",
                    Document::String(none_result.clone()),
                    " = apply ",
                    none_code,
                    " () in ",
                    Document::String(none_result),
                    " end in ",
                ]);
            } else {
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
                    Document::String(found_item.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result.clone()),
                    ") in let ",
                    Document::String(found_flag.clone()),
                    " = call 'erlang':'element'(2, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "case ",
                    Document::String(found_flag),
                    " of <'true'> when 'true' -> {",
                    Document::String(found_item.clone()),
                    ", ",
                    Document::String(stateacc.clone()),
                    "} <'false'> when 'true' -> let ",
                    Document::String(none_result.clone()),
                    " = apply ",
                    none_code,
                    " () in {",
                    Document::String(none_result),
                    ", ",
                    Document::String(stateacc),
                    "} end",
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
                 let FoundItem = call 'erlang':'element'(1, {acc_state_var}) in \
                 let FoundFlag = call 'erlang':'element'(2, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(3, {acc_state_var}) in "
            ),
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(
            body,
            &plan,
            &BodyKind::FoldlDetect {
                item_var: item_var.clone(),
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let found_item = self.fresh_temp_var("FoundItem");
        let found_flag = self.fresh_temp_var("FoundFlag");
        let state_out = self.fresh_temp_var("StOut");
        let none_result = self.fresh_temp_var("NoneResult");

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, \
             {{'nil', 'false', {init_state}}}, {safe_list_var}) \
             in let {found_item} = call 'erlang':'element'(1, {fold_result}) \
             in let {found_flag} = call 'erlang':'element'(2, {fold_result}) \
             in let {state_out} = call 'erlang':'element'(3, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        docs.push(Document::String(post_code));

        // Case on FoundFlag to decide result.
        docs.push(docvec![
            "case ",
            Document::String(found_flag),
            " of <'true'> when 'true' -> {",
            Document::String(found_item),
            ", ",
            Document::String(state_out.clone()),
            "} <'false'> when 'true' -> let ",
            Document::String(none_result.clone()),
            " = apply ",
            none_code,
            " () in {",
            Document::String(none_result),
            ", ",
            Document::String(state_out),
            "} end",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1486: Generates stateful `detect:` using `lists:foldl`
    /// with state threading and a found-item accumulator.
    ///
    /// All elements are processed (no short-circuit) because field mutations must
    /// execute for every element. Accumulator is `{FoundItem, FoundFlag, StateVars...}`.
    ///
    /// Returns `{FoundItem, StateAcc}` where `FoundItem` is `nil` if no match.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_detect_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {FoundItem, FoundFlag, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{'nil', 'false', ", vars_doc, "}"];

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
                ") -> let FoundItem = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
                ") in let FoundFlag = call 'erlang':'element'(2, ",
                Document::String(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            // Unpack vars starting at index 3 (slot 1 = FoundItem, slot 2 = FoundFlag).
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 3));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlDetect {
                    item_var: item_var.clone(),
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let found_result = self.fresh_temp_var("FoundResult");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 3, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(found_result.clone());
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
                    Document::String(found_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                ]);
            } else {
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
                    Document::String(found_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    Document::String(found_result),
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
                 let FoundItem = call 'erlang':'element'(1, {acc_state_var}) in \
                 let FoundFlag = call 'erlang':'element'(2, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(3, {acc_state_var}) in "
            ),
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(
            body,
            &plan,
            &BodyKind::FoldlDetect {
                item_var: item_var.clone(),
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let found_result = self.fresh_temp_var("FoundResult");
        let state_out = self.fresh_temp_var("StOut");

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, \
             {{'nil', 'false', {init_state}}}, {safe_list_var}) \
             in let {found_result} = call 'erlang':'element'(1, {fold_result}) \
             in let {state_out} = call 'erlang':'element'(3, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{found_result}, {state_out}}}");
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    /// BT-1486: Generates code for `list count:` with mutation analysis.
    ///
    /// Without mutations: falls through to `lists:filter` + `erlang:length`.
    /// With mutations: uses `lists:foldl` with a count accumulator, processing
    /// all elements for state threading.
    pub(in crate::codegen::core_erlang) fn generate_list_count(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "count:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list count: [:item | item > 0]",
        )?;

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_count_with_mutations(receiver, body_block);
            }
        }

        // No mutations: use lists:filter + erlang:length
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;
        let filtered_var = self.fresh_temp_var("temp");

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 let {filtered_var} = call 'lists':'filter'({body_var}, {list_var}) in \
                 call 'erlang':'length'({filtered_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'count:', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1486: Generates stateful `count:` using `lists:foldl` with state threading
    /// and a count accumulator.
    ///
    /// All elements are processed. Accumulator is `{Count, StateVars...}`.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_count_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {CountAcc, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{0, ", vars_doc, "}"];

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
                ") -> let CountAcc = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

            let (body_doc, _) =
                self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlCount)?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let count_result = self.fresh_temp_var("CountResult");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 2, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(count_result.clone());
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
                    Document::String(count_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                ]);
            } else {
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
                    Document::String(count_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    Document::String(count_result),
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
                 let CountAcc = call 'erlang':'element'(1, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(2, {acc_state_var}) in "
            ),
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlCount)?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let count_result = self.fresh_temp_var("CountResult");
        let state_out = self.fresh_temp_var("StOut");

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, \
             {{0, {init_state}}}, {safe_list_var}) \
             in let {count_result} = call 'erlang':'element'(1, {fold_result}) \
             in let {state_out} = call 'erlang':'element'(2, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{count_result}, {state_out}}}");
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    /// BT-1486: Generates code for `list flatMap:` with mutation analysis.
    ///
    /// Without mutations: falls through to `lists:flatmap/2`.
    /// With mutations: uses `lists:foldl` like `collect:` (accumulates reversed
    /// sub-lists), then `lists:append(lists:reverse(...))` to flatten.
    pub(in crate::codegen::core_erlang) fn generate_list_flat_map(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "flatMap:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list flatMap: [:item | #(item, item * 2)]",
        )?;

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_flat_map_with_mutations(receiver, body_block);
            }
        }

        // No mutations: use lists:flatmap
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'lists':'flatmap'({body_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'flatMap:', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1486: Generates stateful `flatMap:` using `lists:foldl` with state threading.
    ///
    /// Uses `FoldlCollect` body kind to build a reversed list of sub-lists, then
    /// reverses and appends (flattens) the result.
    ///
    /// Accumulator is `{ResultList, StateVars...}` (tuple) or `{ResultList, StateAcc}` (map).
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_flat_map_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {ResultList, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
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
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

            let (body_doc, _) =
                self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlCollect)?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let rev_list = self.fresh_temp_var("RevList");
            let ordered_list = self.fresh_temp_var("OrderedList");
            let final_list = self.fresh_temp_var("FinalList");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 2, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(final_list.clone());
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
                    Document::String(ordered_list.clone()),
                    " = call 'lists':'reverse'(",
                    Document::String(rev_list),
                    ") in let ",
                    Document::String(final_list.clone()),
                    " = call 'lists':'append'(",
                    Document::String(ordered_list),
                    ") in ",
                    extract_doc,
                ]);
            } else {
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
                    Document::String(ordered_list.clone()),
                    " = call 'lists':'reverse'(",
                    Document::String(rev_list),
                    ") in let ",
                    Document::String(final_list.clone()),
                    " = call 'lists':'append'(",
                    Document::String(ordered_list),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    Document::String(final_list),
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
        let ordered_list = self.fresh_temp_var("OrderedList");
        let final_list = self.fresh_temp_var("FinalList");
        let state_out = self.fresh_temp_var("StOut");

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {{[], {init_state}}}, {safe_list_var}) \
             in let {rev_list} = call 'erlang':'element'(1, {fold_result}) \
             in let {ordered_list} = call 'lists':'reverse'({rev_list}) \
             in let {final_list} = call 'lists':'append'({ordered_list}) \
             in let {state_out} = call 'erlang':'element'(2, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{final_list}, {state_out}}}");
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    pub(in crate::codegen::core_erlang) fn generate_list_inject(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 2-arg: accumulator and element)
        validate_block_arity_exact(
            body,
            2,
            "inject:into:",
            "Fix: The body block must take two arguments (accumulator and element):\n\
             \x20 list inject: 0 into: [:sum :each | sum + each]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Analyze block for mutations
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_inject_with_mutations(receiver, initial, body_block);
            }
        }

        // BT-1327: Pure-block fast path — emit inline lists:foldl instead of
        // calling beamtalk_collection:inject_into at runtime.
        // This eliminates: (1) the runtime function call overhead, (2) the to_list
        // indirection when receiver is already a list.
        //
        // BT-820: lists:foldl calls Fun(Elem, Acc) but Beamtalk convention is Block(Acc, Elem).
        // For literal blocks, we compile the body directly with swapped parameter order
        // (Elem, Acc) to avoid any wrapper overhead. For non-literal callables, we fall
        // back to beamtalk_collection:inject_into which handles arg swapping at runtime.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let init_var = self.fresh_temp_var("temp");
        let init_code = self.expression_doc(initial)?;
        let lambda_var = self.fresh_temp_var("temp");

        // Generate the foldl fun: for literal blocks, compile body with swapped
        // parameter order; for non-literal, use runtime arg-swap wrapper.
        let foldl_fun_doc = if let Expression::Block(body_block) = body {
            // Literal block: compile directly with foldl parameter order (Elem, Acc).
            // Block params: [0] = acc, [1] = elem (Beamtalk convention)
            // Foldl fun params: (Elem, Acc) (Erlang convention)
            let acc_param = body_block
                .parameters
                .first()
                .map_or("_", |p| p.name.as_str());
            let elem_param = body_block
                .parameters
                .get(1)
                .map_or("_", |p| p.name.as_str());
            let acc_var = Self::to_core_erlang_var(acc_param);
            let elem_var = Self::to_core_erlang_var(elem_param);

            self.push_scope();
            // Bind in swapped order: foldl's first param is Elem, second is Acc
            if let Some(param) = body_block.parameters.get(1) {
                self.bind_var(&param.name, &elem_var);
            }
            if let Some(param) = body_block.parameters.first() {
                self.bind_var(&param.name, &acc_var);
            }
            let body_doc = self.generate_block_body(body_block)?;
            self.pop_scope();

            // fun (Elem, Acc) -> <body>
            docvec![
                "fun (",
                Document::String(elem_var),
                ", ",
                Document::String(acc_var),
                ") -> ",
                body_doc,
            ]
        } else {
            // Non-literal callable: wrap to swap args at runtime.
            let body_var = self.fresh_temp_var("temp");
            let body_code = self.expression_doc(body)?;
            docvec![
                "let ",
                Document::String(body_var.clone()),
                " = ",
                body_code,
                " in fun (Elem, Acc) -> apply ",
                Document::String(body_var),
                " (Acc, Elem)",
            ]
        };

        Ok(docvec![
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
            Document::String(init_var.clone()),
            " = ",
            init_code,
            " in let ",
            Document::String(lambda_var.clone()),
            " = ",
            foldl_fun_doc,
            " in call 'lists':'foldl'(",
            Document::String(lambda_var),
            ", ",
            Document::String(init_var),
            ", ",
            Document::String(safe_list_var),
            ")",
        ])
    }

    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_inject_with_mutations(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // BT-1276: Use tuple accumulator when eligible.
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        // BT-524: Add is_list guard for non-list collection types.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let init_var = self.fresh_temp_var("temp");
        let init_code = self.expression_doc(initial)?;
        let lambda_var = self.fresh_temp_var("temp");
        let acc_state_var = self.fresh_temp_var("AccSt");

        if plan.use_tuple_acc {
            // BT-1276: Tuple-accumulator path.
            // Initial fold acc: {InitVar, Var1, ..., VarN} (flat tuple).
            let vars_doc = plan.current_vars_doc(self); // Before push_scope.

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
                Document::String(init_var.clone()),
                " = ",
                init_code,
                " in let ",
                Document::String(lambda_var.clone()),
                " = fun (Item, ",
                Document::String(acc_state_var.clone()),
                ") -> let Acc = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if !body.parameters.is_empty() {
                self.bind_var(&body.parameters[0].name, "Acc");
            }
            if body.parameters.len() >= 2 {
                self.bind_var(&body.parameters[1].name, "Item");
            }
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

            let (body_doc, _) =
                self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlInject)?;
            docs.push(body_doc);
            self.pop_scope();

            let result_var = self.fresh_temp_var("temp");
            let acc_out = self.fresh_temp_var("AccOut");

            // Extract each updated local var from tuple positions 2..N.
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&result_var, 2, self);
            if self.in_direct_params_loop {
                // BT-1329: Skip StateAcc repack. Emit open let-chain so variable rebindings
                // escape to the outer scope. Store the result var for the caller.
                self.direct_params_list_op_result = Some(acc_out.clone());
                docs.push(docvec![
                    " in let ",
                    Document::String(result_var.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", {",
                    Document::String(init_var),
                    ", ",
                    vars_doc,
                    "}, ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(acc_out.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(result_var),
                    ") in ",
                    extract_doc,
                ]);
            } else {
                // BT-1276: Re-pack into StateAcc for outer method-body `maps:get` extraction.
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docs.push(docvec![
                    " in let ",
                    Document::String(result_var.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", {",
                    Document::String(init_var),
                    ", ",
                    vars_doc,
                    "}, ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(acc_out.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(result_var),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    Document::String(acc_out),
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
                 in let {init_var} = "
            ),
            init_code,
            format!(
                " in let {lambda_var} = fun (Item, {acc_state_var}) -> \
                 let Acc = call 'erlang':'element'(1, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(2, {acc_state_var}) in "
            ),
        ]);

        self.push_scope();
        if !body.parameters.is_empty() {
            self.bind_var(&body.parameters[0].name, "Acc");
        }
        if body.parameters.len() >= 2 {
            self.bind_var(&body.parameters[1].name, "Item");
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) =
            self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlInject)?;
        docs.push(body_doc);
        self.pop_scope();

        // BT-483: Return {Result, State} tuple — inject:into: returns accumulator as result.
        let result_var = self.fresh_temp_var("temp");
        let acc_out = self.fresh_temp_var("AccOut");
        let state_out = self.fresh_temp_var("StOut");

        let mut post_code = format!(
            " in let {result_var} = call 'lists':'foldl'({lambda_var}, {{{init_var}, {init_state}}}, {safe_list_var}) \
             in let {acc_out} = call 'erlang':'element'(1, {result_var}) \
             in let {state_out} = call 'erlang':'element'(2, {result_var}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{acc_out}, {state_out}}}");
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    /// BT-1489: Generates a `let` binding that converts a list result to a
    /// binary string when the original receiver was a binary (String).
    ///
    /// Returns `(binding_code, result_var)` where `binding_code` is:
    /// ```text
    /// let <out_var> = case call 'erlang':'is_binary'(<recv_var>) of
    ///   <'true'> when 'true' -> call 'erlang':'iolist_to_binary'(<list_var>)
    ///   <'false'> when 'true' -> <list_var>
    /// end
    /// ```
    ///
    /// Note: does NOT include a trailing ` in ` — callers chain with ` in `.
    ///
    /// This allows `collect:`, `select:`, and `reject:` to return a binary
    /// string when the receiver is a String, while still returning a list
    /// when the receiver is a List.
    fn generate_string_aware_result_binding(
        &mut self,
        recv_var: &str,
        list_var: &str,
    ) -> (Document<'static>, String) {
        let out_var = self.fresh_temp_var("StrAwareResult");
        let binding = docvec![
            "let ",
            Document::String(out_var.clone()),
            " = case call 'erlang':'is_binary'(",
            Document::String(recv_var.to_string()),
            ") of <'true'> when 'true' -> call 'erlang':'iolist_to_binary'(",
            Document::String(list_var.to_string()),
            ") <'false'> when 'true' -> ",
            Document::String(list_var.to_string()),
            " end"
        ];
        (binding, out_var)
    }
}

#[cfg(test)]
mod tests {
    fn codegen(src: &str) -> String {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, _) = crate::source_analysis::parse(tokens);
        crate::codegen::core_erlang::generate_module(
            &module,
            crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
        )
        .expect("codegen should succeed")
    }

    #[test]
    fn test_list_do_pure_generates_foreach() {
        // Pure do: (no mutations) generates lists:foreach
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items do: [:item | item]\n";
        let code = codegen(src);
        assert!(
            code.contains("'lists':'foreach'"),
            "Pure do: should generate lists:foreach. Got:\n{code}"
        );
    }

    #[test]
    fn test_list_collect_generates_map() {
        // collect: (no mutations) generates lists:map
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items collect: [:item | item]\n";
        let code = codegen(src);
        assert!(
            code.contains("'lists':'map'"),
            "collect: should generate lists:map. Got:\n{code}"
        );
    }

    #[test]
    fn test_list_inject_into_pure_generates_inline_foldl() {
        // BT-1327: inject:into: (no mutations) with a literal block emits inline
        // lists:foldl with the block body compiled directly in foldl arg order
        // (Elem, Acc) — no wrapper function, no runtime helper call.
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items inject: 0 into: [:acc :item | acc + item]\n";
        let code = codegen(src);
        assert!(
            code.contains("'lists':'foldl'"),
            "Pure inject:into: should emit inline lists:foldl. Got:\n{code}"
        );
        // Should NOT call the runtime helper
        assert!(
            !code.contains("'beamtalk_collection':'inject_into'"),
            "Pure inject:into: should NOT delegate to beamtalk_collection:inject_into. Got:\n{code}"
        );
        // Should NOT have a wrapper apply (literal block compiles body directly)
        assert!(
            !code.contains("fun (Elem, Acc) -> apply"),
            "Literal block should compile body directly, not via wrapper apply. Got:\n{code}"
        );
    }

    #[test]
    fn test_list_inject_into_non_literal_generates_wrapper() {
        // BT-1327: inject:into: with a non-literal block (variable) emits inline
        // lists:foldl with an arg-swap wrapper: fun (Elem, Acc) -> apply Block (Acc, Elem).
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items with: block =>\n    items inject: 0 into: block\n";
        let code = codegen(src);
        assert!(
            code.contains("'lists':'foldl'"),
            "Non-literal inject:into: should emit inline lists:foldl. Got:\n{code}"
        );
        // Non-literal needs an arg-swap wrapper
        assert!(
            code.contains("fun (Elem, Acc) -> apply"),
            "Non-literal inject:into: should emit arg-swap wrapper. Got:\n{code}"
        );
        // Should NOT call the runtime helper
        assert!(
            !code.contains("'beamtalk_collection':'inject_into'"),
            "Non-literal inject:into: should NOT delegate to runtime helper. Got:\n{code}"
        );
    }

    #[test]
    fn test_list_do_with_field_mutation_threads_state() {
        // do: with field mutation generates a state-threading foldl (not foreach)
        let src = "Actor subclass: Ctr\n  state: sum = 0\n\n  run: items =>\n    items do: [:item | self.sum := self.sum + item]\n";
        let code = codegen(src);
        // Mutation-threading uses foldl to thread state
        assert!(
            code.contains("'lists':'foldl'"),
            "do: with mutation should use lists:foldl for state threading. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('sum'"),
            "do: body should update 'sum' via maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt1290_local_var_captured_by_nested_timer_block() {
        // BT-1290: local var `y` assigned in do: block must be capturable by a nested block.
        // Before the fix, `let Y = ... in <Timer_case_expr> in StateAcc` was generated,
        // which is invalid Core Erlang (orphaned `in StateAcc` after a closed expression).
        // After the fix, `let Y = ... in let _ = <Timer_case_expr> in StateAcc` is generated.
        let src = concat!(
            "Actor subclass: BugDemo\n",
            "  tick =>\n",
            "    #(1, 2, 3) do: [:x |\n",
            "      y := x + 1\n",
            "      Timer after: 0 do: [self use: y]\n",
            "    ]\n",
            "  use: n => nil\n"
        );
        let code = codegen(src);
        // Y (CoreErlang name for y) must appear inside the nested fun's argument list
        assert!(
            code.contains("'use:', [Y]"),
            "Y should be captured by nested block. Got:\n{code}"
        );
        // The foldl lambda must use `let _ = <Timer_expr> in StateAcc`, not bare `<Timer_expr> in StateAcc`
        assert!(
            code.contains("let _ = case call 'beamtalk_class_registry'"),
            "Last expr in do: body with plain lets must use let _ = binding. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt1291_destructure_then_on_do_last_expr() {
        // BT-1291: #[...] list destructuring (has_plain_lets=true) followed by
        // `on:Exception do:` as the last expression produced the same invalid
        // `<bindings> in <on_do_expr> in StateAcc` pattern fixed by BT-1290.
        let src = concat!(
            "Actor subclass: BugDemo\n",
            "  tick =>\n",
            "    #() do: [:te |\n",
            "      #[a, b] := te\n",
            "      [a printString] on: Exception do: [:e | nil]\n",
            "    ]\n"
        );
        // Must compile without panic (before the fix this generated invalid Core Erlang)
        let code = codegen(src);
        // The last expr must be wrapped with `let _ =` to be valid Core Erlang
        assert!(
            code.contains("let _ = "),
            "Last expr after destructure must use let _ = binding. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt1290_field_mutation_then_general_last_expr() {
        // BT-1290: same fix also applies when has_mutations=true (field write before general last expr).
        // Before the fix: `let StateAcc1 = maps:put(...) in <external_call> in StateAcc1` — invalid.
        // After the fix: `let StateAcc1 = maps:put(...) in let _ = <external_call> in StateAcc1`.
        let src = concat!(
            "Actor subclass: Ctr\n",
            "  state: n = 0\n",
            "  run: items =>\n",
            "    items do: [:item | self.n := self.n + item. Timer after: 0 do: [item printString]]\n"
        );
        let code = codegen(src);
        // The last expr (Timer send) must be wrapped with `let _ =`
        assert!(
            code.contains("let _ = case call 'beamtalk_class_registry'"),
            "Last expr after field mutation must use let _ = binding. Got:\n{code}"
        );
    }

    #[test]
    fn test_list_do_multi_stmt_first_is_pure_generates_let_underscore() {
        // Multi-statement do: body where the first statement is a pure expression
        // must emit `let _ = <expr> in ...` (not bare `<expr> in ...`) — Core Erlang requires
        // non-last expressions to be bound.  The "+" call must appear as the RHS of a let.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items do: [:item | item + 1. self.n := self.n + 1]\n";
        let code = codegen(src);
        // The first expression (item + 1) is non-last; it must be bound as `let _ = ... in`
        // not emitted bare as `<expr> in` which is invalid Core Erlang.
        let has_bare_expr_in = code.contains("call 'erlang':'+'") && {
            // Find the position of the '+' call and check what precedes it
            if let Some(pos) = code.find("call 'erlang':'+'") {
                // Look for "let _ = " immediately before the + call (within 20 chars)
                let before = &code[pos.saturating_sub(20)..pos];
                !before.contains("let _ = ") && !before.contains("let _")
            } else {
                false
            }
        };
        assert!(
            !has_bare_expr_in,
            "First non-last pure expr in do: body must emit 'let _ = ...' binding, not bare expr. Got:\n{code}"
        );
    }

    #[test]
    fn test_list_collect_multi_stmt_first_is_pure_generates_let_underscore() {
        // Same fix for collect: — first non-last pure expr must be bound.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items collect: [:item | item + 1. self.n := self.n + 1. item * 2]\n";
        let code = codegen(src);
        // item + 1 is first and non-last — must be wrapped with let _ = ... in
        let has_bare_expr_in = code.contains("call 'erlang':'+'") && {
            if let Some(pos) = code.find("call 'erlang':'+'") {
                let before = &code[pos.saturating_sub(20)..pos];
                !before.contains("let _ = ") && !before.contains("let _")
            } else {
                false
            }
        };
        assert!(
            !has_bare_expr_in,
            "First non-last pure expr in collect: body must emit 'let _ = ...' binding. Got:\n{code}"
        );
    }

    // ── BT-1276: Tuple-accumulator tests ──────────────────────────────────

    #[test]
    fn test_do_with_local_mutation_uses_tuple_acc() {
        // do: with only local mutation should use tuple accumulator:
        // - element(N, ...) inside the lambda (not maps:get per iteration)
        // - exactly 1 maps:get outside the loop (outer method body extraction)
        // - exactly 1 maps:put outside the loop (repack for outer method body)
        let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    total := 0\n    items do: [:item | total := total + item]\n    total\n";
        let code = codegen(src);
        // In tuple mode: 1 maps:get (outer extraction), 1 maps:put (repack).
        // In StateAcc mode: ≥2 maps:get, ≥2 maps:put (inside lambda + extract_suffix/pack).
        let get_count = code.matches("maps':'get'('__local__total'").count();
        let put_count = code.matches("maps':'put'('__local__total'").count();
        assert_eq!(
            get_count, 1,
            "do: tuple mode should have exactly 1 maps:get (outer extraction). Got:\n{code}"
        );
        assert_eq!(
            put_count, 1,
            "do: tuple mode should have exactly 1 maps:put (repack after loop). Got:\n{code}"
        );
        assert!(
            code.contains("'erlang':'element'(1,"),
            "do: tuple mode should use element(1, ...) to read threaded local. Got:\n{code}"
        );
    }

    #[test]
    fn test_do_with_field_mutation_uses_stateacc() {
        // do: with field mutation must still use maps:put (no tuple acc — state effects present).
        let src = "Actor subclass: Ctr\n  state: sum = 0\n\n  run: items =>\n    items do: [:item | self.sum := self.sum + item]\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'('sum'"),
            "do: with field mutation must still use maps:put for field. Got:\n{code}"
        );
    }

    #[test]
    fn test_collect_with_local_mutation_uses_tuple_acc() {
        // collect: with only local mutation should use element/2 (tuple acc).
        let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items collect: [:item | count := count + 1. item * 2]\n";
        let code = codegen(src);
        assert!(
            !code.contains("maps':'get'('__local__count'"),
            "collect: with local mutation should NOT use maps:get inside lambda. Got:\n{code}"
        );
        assert!(
            code.contains("'erlang':'element'(2,"),
            "collect: tuple acc should use element(2, ...) for first threaded var. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_with_local_mutation_uses_tuple_acc() {
        // inject:into: with only local mutation should use element/2 (tuple acc).
        let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items inject: 0 into: [:acc :item | count := count + 1. acc + item]\n";
        let code = codegen(src);
        assert!(
            !code.contains("maps':'get'('__local__count'"),
            "inject: with local mutation should NOT use maps:get inside lambda. Got:\n{code}"
        );
        assert!(
            code.contains("'erlang':'element'(2,"),
            "inject: tuple acc should use element(2, ...) for first threaded var. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_literal_initial_elides_acc_maybe_await() {
        // BT-1304 (now trivially passing since BT-1321 removed all maybe_await from binary ops):
        // maybe_await is never emitted on the fold accumulator.
        let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items inject: 0 into: [:acc :item | count := count + 1. acc + item]\n";
        let code = codegen(src);
        assert!(
            !code.contains("'maybe_await'(Acc)"),
            "BT-1321: maybe_await should not appear on the fold accumulator. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_sync_var_initial_elides_acc_maybe_await() {
        // BT-1304 (now trivially passing since BT-1321 removed all maybe_await from binary ops):
        // maybe_await is never emitted on the fold accumulator.
        let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items start: start =>\n    count := 0\n    items inject: start into: [:acc :item | count := count + 1. acc + item]\n";
        let code = codegen(src);
        assert!(
            !code.contains("'maybe_await'(Acc)"),
            "BT-1321: maybe_await should not appear on the fold accumulator. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_non_literal_initial_no_maybe_await() {
        // BT-1321: Binary op codegen no longer emits maybe_await on any operand (ADR-0043).
        // Even when the initial accumulator is a non-literal field read, the generated
        // binary op for `acc + item` must not wrap either operand with maybe_await.
        let src = "Actor subclass: Ctr\n  state: x = 0\n  state: initial = 0\n\n  run: items =>\n    count := 0\n    items inject: self.initial into: [:acc :item | count := count + 1. acc + item]\n";
        let code = codegen(src);
        assert!(
            !code.contains("'maybe_await'(Acc)"),
            "BT-1321: binary op should not wrap acc with maybe_await. Got:\n{code}"
        );
        assert!(
            !code.contains("'maybe_await'(Item)"),
            "BT-1321: binary op should not wrap item with maybe_await. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_non_literal_initial_no_maybe_await_map_acc() {
        // BT-1321: Binary op codegen no longer emits maybe_await (ADR-0043), even on the
        // map-accumulator path when the initial is a non-literal field read.
        let src = "Actor subclass: Ctr\n  state: count = 0\n  state: initial = 0\n\n  run: items =>\n    items inject: self.initial into: [:acc :item | self.count := self.count + 1. acc + item]\n";
        let code = codegen(src);
        assert!(
            !code.contains("'maybe_await'(Acc)"),
            "BT-1321: binary op should not wrap acc with maybe_await (map-acc path). Got:\n{code}"
        );
        assert!(
            !code.contains("'maybe_await'(Item)"),
            "BT-1321: binary op should not wrap item with maybe_await (map-acc path). Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_nested_scope_no_maybe_await() {
        // BT-1321: Binary op codegen no longer emits maybe_await on any operand (ADR-0043).
        // Verifies that neither the outer nor the inner fold's accumulator is wrapped.
        let src = concat!(
            "Actor subclass: Ctr\n",
            "  state: initial = 0\n\n",
            "  run: outerList with: innerList =>\n",
            "    sharedCount := 0\n",
            "    outerList inject: 0 into: [:acc :x |\n",
            "      sharedCount := sharedCount + 1.\n",
            "      innerList inject: self.initial into: [:acc :y |\n",
            "        sharedCount := sharedCount + y.\n",
            "        acc + y]]\n",
        );
        let code = codegen(src);
        assert!(
            !code.contains("'maybe_await'(Acc)"),
            "BT-1321: no fold accumulator should be wrapped with maybe_await. Got:\n{code}"
        );
    }

    #[test]
    fn test_filter_with_local_mutation_uses_tuple_acc() {
        // BT-1276: select: with only local mutation should use tuple accumulator.
        // element(2, ...) reads the first threaded var (slot 1 is AccList).
        let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items select: [:item | count := count + 1. item > 0]\n";
        let code = codegen(src);
        assert!(
            !code.contains("maps':'get'('__local__count'"),
            "select: with local mutation should NOT use maps:get inside lambda. Got:\n{code}"
        );
        assert!(
            code.contains("'erlang':'element'(2,"),
            "select: tuple acc should use element(2, ...) for first threaded var. Got:\n{code}"
        );
    }

    #[test]
    fn test_collect_with_mutation_has_string_aware_result() {
        // BT-1489: collect: with mutations should emit is_binary guard so String
        // receivers get iolist_to_binary applied to the result list.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items collect: [:x | self.n := self.n + 1. x]\n";
        let code = codegen(src);
        assert!(
            code.contains("'erlang':'is_binary'("),
            "BT-1489: collect: with mutations should check is_binary on receiver. Got:\n{code}"
        );
        assert!(
            code.contains("'erlang':'iolist_to_binary'("),
            "BT-1489: collect: with mutations should call iolist_to_binary for string receivers. Got:\n{code}"
        );
    }

    #[test]
    fn test_select_with_mutation_has_string_aware_result() {
        // BT-1489: select: with mutations should emit is_binary guard.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items select: [:x | self.n := self.n + 1. x > 0]\n";
        let code = codegen(src);
        assert!(
            code.contains("'erlang':'is_binary'("),
            "BT-1489: select: with mutations should check is_binary on receiver. Got:\n{code}"
        );
        assert!(
            code.contains("'erlang':'iolist_to_binary'("),
            "BT-1489: select: with mutations should call iolist_to_binary for string receivers. Got:\n{code}"
        );
    }
}
