// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Transform/aggregate list operations: `inject:into:`, `flatMap:`, `count:`,
//! `takeWhile:`, `dropWhile:`, `partition:`, `groupBy:`, and `sort:`.

use super::super::super::document::Document;
use super::super::super::document::leaf;
use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CoreErlangGenerator, Result};
use super::super::{BodyKind, ThreadingPlan};
use crate::ast::{Block, Expression};
use crate::docvec;

impl CoreErlangGenerator {
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

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_count_with_mutations(receiver, body_block);
        }

        // No mutations: use lists:filter + erlang:length
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;
        let filtered_var = self.fresh_temp_var("temp");

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
            leaf::var(filtered_var.clone()),
            " = call 'lists':'filter'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") in call 'erlang':'length'(",
            leaf::var(filtered_var),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", 'count:', [",
            leaf::var(body_var),
            "]) end",
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
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let CountAcc = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
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
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(count_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result),
                    ") in ",
                    extract_doc,
                ]);
            } else {
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
                    leaf::var(count_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(count_result),
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
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(acc_state_var.clone()),
            ") -> let CountAcc = call 'erlang':'element'(1, ",
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

        let (body_doc, _) = self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlCount)?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let count_result = self.fresh_temp_var("CountResult");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            " in let ",
            leaf::var(fold_result.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(lambda_var),
            ", {0, ",
            leaf::var(init_state),
            "}, ",
            leaf::var(safe_list_var),
            ") in let ",
            leaf::var(count_result.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(count_result),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

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

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_flat_map_with_mutations(receiver, body_block);
        }

        // No mutations: use lists:flatmap
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'lists':'flatmap'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", 'flatMap:', [",
            leaf::var(body_var),
            "]) end",
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
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
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
                    leaf::var(ordered_list.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_list),
                    ") in let ",
                    leaf::var(final_list.clone()),
                    " = call 'lists':'append'(",
                    leaf::var(ordered_list),
                    ") in ",
                    extract_doc,
                ]);
            } else {
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
                    leaf::var(ordered_list.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_list),
                    ") in let ",
                    leaf::var(final_list.clone()),
                    " = call 'lists':'append'(",
                    leaf::var(ordered_list),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(final_list),
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
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
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

        let (body_doc, _) =
            self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlCollect)?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let rev_list = self.fresh_temp_var("RevList");
        let ordered_list = self.fresh_temp_var("OrderedList");
        let final_list = self.fresh_temp_var("FinalList");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
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
            leaf::var(ordered_list.clone()),
            " = call 'lists':'reverse'(",
            leaf::var(rev_list),
            ") in let ",
            leaf::var(final_list.clone()),
            " = call 'lists':'append'(",
            leaf::var(ordered_list),
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(final_list),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

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
        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_inject_with_mutations(receiver, initial, body_block);
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
                leaf::var(elem_var),
                ", ",
                leaf::var(acc_var),
                ") -> ",
                body_doc,
            ]
        } else {
            // Non-literal callable: wrap to swap args at runtime.
            let body_var = self.fresh_temp_var("temp");
            let body_code = self.expression_doc(body)?;
            docvec![
                "let ",
                leaf::var(body_var.clone()),
                " = ",
                body_code,
                " in fun (Elem, Acc) -> apply ",
                leaf::var(body_var),
                " (Acc, Elem)",
            ]
        };

        Ok(docvec![
            super::list_recv_to_safe_list_doc(recv_code, list_var, safe_list_var.clone()),
            "let ",
            leaf::var(init_var.clone()),
            " = ",
            init_code,
            " in let ",
            leaf::var(lambda_var.clone()),
            " = ",
            foldl_fun_doc,
            " in call 'lists':'foldl'(",
            leaf::var(lambda_var),
            ", ",
            leaf::var(init_var),
            ", ",
            leaf::var(safe_list_var),
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
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(init_var.clone()),
                " = ",
                init_code,
                " in let ",
                leaf::var(lambda_var.clone()),
                " = fun (Item, ",
                leaf::var(acc_state_var.clone()),
                ") -> let Acc = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
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
                    leaf::var(result_var.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", {",
                    leaf::var(init_var),
                    ", ",
                    vars_doc,
                    "}, ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(acc_out.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(result_var),
                    ") in ",
                    extract_doc,
                ]);
            } else {
                // BT-1276: Re-pack into StateAcc for outer method-body `maps:get` extraction.
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docs.push(docvec![
                    " in let ",
                    leaf::var(result_var.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", {",
                    leaf::var(init_var),
                    ", ",
                    vars_doc,
                    "}, ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(acc_out.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(result_var),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(acc_out),
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
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
            leaf::var(init_var.clone()),
            " = ",
            init_code,
            " in let ",
            leaf::var(lambda_var.clone()),
            " = fun (Item, ",
            leaf::var(acc_state_var.clone()),
            ") -> let Acc = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var),
            ") in ",
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

        docs.push(docvec![
            " in let ",
            leaf::var(result_var.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(lambda_var),
            ", {",
            leaf::var(init_var),
            ", ",
            leaf::var(init_state),
            "}, ",
            leaf::var(safe_list_var),
            ") in let ",
            leaf::var(acc_out.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(result_var.clone()),
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(result_var),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(acc_out),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }

    // ── BT-1487: takeWhile:/dropWhile:/groupBy:/partition:/sort: ─────────

    /// BT-1487: Generates code for `list takeWhile:` with mutation analysis.
    ///
    /// Without mutations: falls through to `lists:takewhile/2`.
    /// With mutations: uses `lists:foldl` processing ALL elements, tracking
    /// a `StillTaking` flag that flips to false on first predicate failure.
    pub(in crate::codegen::core_erlang) fn generate_list_take_while(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "takeWhile:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list takeWhile: [:item | item < 10]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_take_while_with_mutations(receiver, body_block);
        }

        // No mutations: use lists:takewhile
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'lists':'takewhile'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", 'takeWhile:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-1487: Generates stateful `takeWhile:` using `lists:foldl` with state threading.
    ///
    /// Accumulator is `{ResultList, StillTaking, StateVars...}`.
    /// All elements are processed. Once predicate returns false, `StillTaking` flips
    /// to false and all subsequent elements are excluded regardless of predicate.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_take_while_with_mutations(
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
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{ [], 'true', ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let AccList = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in let StillTaking = call 'erlang':'element'(2, ",
                leaf::var(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            // Unpack state vars starting at position 3 (after AccList and StillTaking).
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 3));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlTakeWhile {
                    item_var: item_var.clone(),
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let rev_list = self.fresh_temp_var("RevList");
            let final_list = self.fresh_temp_var("FinalList");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 3, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(final_list.clone());
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
                    extract_doc,
                ]);
            } else {
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
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(final_list),
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
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(acc_state_var.clone()),
            ") -> let AccList = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let StillTaking = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(3, ",
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
            &BodyKind::FoldlTakeWhile {
                item_var: item_var.clone(),
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let rev_list = self.fresh_temp_var("RevList");
        let final_list = self.fresh_temp_var("FinalList");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            " in let ",
            leaf::var(fold_result.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(lambda_var),
            ", {[], 'true', ",
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
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(3, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(final_list),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1487: Generates code for `list dropWhile:` with mutation analysis.
    ///
    /// Without mutations: falls through to `lists:dropwhile/2`.
    /// With mutations: uses `lists:foldl` processing ALL elements, tracking
    /// a `StillDropping` flag that flips to false on first predicate failure.
    pub(in crate::codegen::core_erlang) fn generate_list_drop_while(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "dropWhile:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list dropWhile: [:item | item < 10]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_drop_while_with_mutations(receiver, body_block);
        }

        // No mutations: use lists:dropwhile
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'lists':'dropwhile'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", 'dropWhile:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-1487: Generates stateful `dropWhile:` using `lists:foldl` with state threading.
    ///
    /// Accumulator is `{ResultList, StillDropping, StateVars...}`.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_drop_while_with_mutations(
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
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{ [], 'true', ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let AccList = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in let StillDropping = call 'erlang':'element'(2, ",
                leaf::var(acc_state_var.clone()),
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
                &BodyKind::FoldlDropWhile {
                    item_var: item_var.clone(),
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let rev_list = self.fresh_temp_var("RevList");
            let final_list = self.fresh_temp_var("FinalList");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 3, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(final_list.clone());
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
                    extract_doc,
                ]);
            } else {
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
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(final_list),
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
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(acc_state_var.clone()),
            ") -> let AccList = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let StillDropping = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(3, ",
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
            &BodyKind::FoldlDropWhile {
                item_var: item_var.clone(),
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let rev_list = self.fresh_temp_var("RevList");
        let final_list = self.fresh_temp_var("FinalList");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            " in let ",
            leaf::var(fold_result.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(lambda_var),
            ", {[], 'true', ",
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
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(3, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(final_list),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1487: Generates code for `list partition:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:partition/2`.
    /// With mutations: uses `lists:foldl` routing each element to one of two lists.
    pub(in crate::codegen::core_erlang) fn generate_list_partition(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "partition:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list partition: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_partition_with_mutations(receiver, body_block);
        }

        // No mutations: use beamtalk_list:partition
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'beamtalk_list':'partition'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::var(body_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", 'partition:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-1487: Generates stateful `partition:` using `lists:foldl` with state threading.
    ///
    /// Accumulator is `{MatchList, NoMatchList, StateVars...}`.
    /// Result is `{lists:reverse(MatchList), lists:reverse(NoMatchList)}` (a 2-tuple, converted to
    /// `#(matches, nonMatches)` by the runtime).
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_partition_with_mutations(
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
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{ [], [], ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let MatchList = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in let NoMatchList = call 'erlang':'element'(2, ",
                leaf::var(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            // State vars start at position 3 (after MatchList and NoMatchList).
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 3));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlPartition {
                    item_var: item_var.clone(),
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let rev_match = self.fresh_temp_var("RevMatch");
            let rev_no_match = self.fresh_temp_var("RevNoMatch");
            let final_match = self.fresh_temp_var("FinalMatch");
            let final_no_match = self.fresh_temp_var("FinalNoMatch");
            let result_tuple = self.fresh_temp_var("PartResult");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 3, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(result_tuple.clone());
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
                    leaf::var(rev_match.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result.clone()),
                    ") in let ",
                    leaf::var(rev_no_match.clone()),
                    " = call 'erlang':'element'(2, ",
                    leaf::var(fold_result),
                    ") in let ",
                    leaf::var(final_match.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_match),
                    ") in let ",
                    leaf::var(final_no_match.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_no_match),
                    ") in let ",
                    leaf::var(result_tuple.clone()),
                    " = [",
                    leaf::var(final_match),
                    " | [",
                    leaf::var(final_no_match),
                    " | []]] in ",
                    extract_doc,
                ]);
            } else {
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
                    leaf::var(rev_match.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result.clone()),
                    ") in let ",
                    leaf::var(rev_no_match.clone()),
                    " = call 'erlang':'element'(2, ",
                    leaf::var(fold_result),
                    ") in let ",
                    leaf::var(final_match.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_match),
                    ") in let ",
                    leaf::var(final_no_match.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_no_match),
                    ") in let ",
                    leaf::var(result_tuple.clone()),
                    " = [",
                    leaf::var(final_match),
                    " | [",
                    leaf::var(final_no_match),
                    " | []]] in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(result_tuple),
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
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(acc_state_var.clone()),
            ") -> let MatchList = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let NoMatchList = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(3, ",
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
            &BodyKind::FoldlPartition {
                item_var: item_var.clone(),
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let rev_match = self.fresh_temp_var("RevMatch");
        let rev_no_match = self.fresh_temp_var("RevNoMatch");
        let final_match = self.fresh_temp_var("FinalMatch");
        let final_no_match = self.fresh_temp_var("FinalNoMatch");
        let result_tuple = self.fresh_temp_var("PartResult");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            " in let ",
            leaf::var(fold_result.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(lambda_var),
            ", {[], [], ",
            leaf::var(init_state),
            "}, ",
            leaf::var(safe_list_var),
            ") in let ",
            leaf::var(rev_match.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(rev_no_match.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(final_match.clone()),
            " = call 'lists':'reverse'(",
            leaf::var(rev_match),
            ") in let ",
            leaf::var(final_no_match.clone()),
            " = call 'lists':'reverse'(",
            leaf::var(rev_no_match),
            ") in let ",
            leaf::var(result_tuple.clone()),
            " = [",
            leaf::var(final_match),
            " | [",
            leaf::var(final_no_match),
            " | []]] in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(3, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(result_tuple),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1487: Generates code for `list groupBy:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:group_by/2`.
    /// With mutations: uses `lists:foldl` building a map where each key
    /// maps to the list of elements that produced that key.
    pub(in crate::codegen::core_erlang) fn generate_list_group_by(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "groupBy:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list groupBy: [:item | item isEven]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_group_by_with_mutations(receiver, body_block);
        }

        // No mutations: use beamtalk_list:group_by
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'beamtalk_list':'group_by'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::var(body_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", 'groupBy:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-1487: Generates stateful `groupBy:` using `lists:foldl` with state threading.
    ///
    /// Accumulator is `{GroupMap, StateVars...}`.
    /// The key block result is used to group elements into a map.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_group_by_with_mutations(
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
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{ ~{}~, ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let GroupMap = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            // State vars start at position 2 (after GroupMap).
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlGroupBy {
                    item_var: item_var.clone(),
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let raw_map = self.fresh_temp_var("RawMap");
            let final_map = self.fresh_temp_var("FinalMap");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 2, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(final_map.clone());
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
                    leaf::var(raw_map.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result),
                    ") in let ",
                    leaf::var(final_map.clone()),
                    " = call 'beamtalk_list':'reverse_group_values'(",
                    leaf::var(raw_map),
                    ") in ",
                    extract_doc,
                ]);
            } else {
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
                    leaf::var(raw_map.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result),
                    ") in let ",
                    leaf::var(final_map.clone()),
                    " = call 'beamtalk_list':'reverse_group_values'(",
                    leaf::var(raw_map),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(final_map),
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
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(acc_state_var.clone()),
            ") -> let GroupMap = call 'erlang':'element'(1, ",
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
            &BodyKind::FoldlGroupBy {
                item_var: item_var.clone(),
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let raw_map = self.fresh_temp_var("RawMap");
        let final_map = self.fresh_temp_var("FinalMap");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            " in let ",
            leaf::var(fold_result.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(lambda_var),
            ", {~{}~, ",
            leaf::var(init_state),
            "}, ",
            leaf::var(safe_list_var),
            ") in let ",
            leaf::var(raw_map.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(final_map.clone()),
            " = call 'beamtalk_list':'reverse_group_values'(",
            leaf::var(raw_map),
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(final_map),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1487: Generates code for `list sort:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:sort_with/2`.
    /// With mutations: uses process dictionary to thread state through the
    /// sort comparator, since `lists:sort/2` controls comparison order and
    /// the comparator must return a plain boolean.
    pub(in crate::codegen::core_erlang) fn generate_list_sort(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            2,
            "sort:",
            "Fix: The body block must take two arguments (elements to compare):\n\
             \x20 list sort: [:a :b | a < b]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_sort_with_mutations(receiver, body_block);
        }

        // No mutations: use beamtalk_list:sort_with
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'beamtalk_list':'sort_with'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::var(body_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", 'sort:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-1487: Generates stateful `sort:` using process dictionary for state threading.
    ///
    /// Strategy:
    /// 1. Pack current state into a process dictionary key unique to this invocation
    ///    (BT-2948: a fresh `erlang:make_ref/0`, not a fixed atom — nested/recursive
    ///    calls to the same call site each get their own key, so they can't stomp
    ///    each other's state).
    /// 2. Build a wrapper comparator that reads state, calls the block body,
    ///    writes updated state back, returns boolean result
    /// 3. Call `lists:sort/2` with the wrapper
    /// 4. Read final state from process dictionary
    ///
    /// This approach is necessary because `lists:sort` controls comparison order
    /// and the comparator must return a plain boolean — we can't use foldl.
    ///
    /// Always uses the map-accumulator path because `generate_field_assignment_open`
    /// produces `maps:put` calls on `StateAcc` which requires a map-based state.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_list_sort_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let wrapper_var = self.fresh_temp_var("temp");

        let param_a = body.parameters.first().map_or("_A", |p| p.name.as_str());
        let param_b = body.parameters.get(1).map_or("_B", |p| p.name.as_str());
        let var_a = Self::to_core_erlang_var(param_a);
        let var_b = Self::to_core_erlang_var(param_b);

        let state_key_var = self.fresh_temp_var("SortStateKey");
        let state_key = leaf::var(state_key_var.clone());

        // Map-accumulator path (always used for sort — see doc comment above).
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        docs.push(docvec![
            "let ",
            leaf::var(state_key_var),
            " = call 'erlang':'make_ref'() in let _ = call 'erlang':'put'(",
            state_key.clone(),
            ", ",
            leaf::var(init_state),
            ") in let ",
            leaf::var(wrapper_var.clone()),
            " = fun (",
            leaf::var(var_a.clone()),
            ", ",
            leaf::var(var_b.clone()),
            ") -> let StateAcc = call 'erlang':'get'(",
            state_key.clone(),
            ") in ",
        ]);

        // Save state version before generating lambda body — the field assignments
        // inside the lambda advance the version counter, but the outer scope should
        // not see those advancements (state is managed via process dictionary).
        let saved_state_version = self.state_version();

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &var_a);
        }
        if let Some(param) = body.parameters.get(1) {
            self.bind_var(&param.name, &var_b);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        // Generate block body manually (can't use generate_threaded_loop_body for sort)
        let filtered_body = super::super::super::util::collect_body_exprs(&body.body);

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;

            if Self::is_field_assignment(expr) {
                let (doc, _) = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
                if is_last {
                    let current = self.current_state_var();
                    docs.push(docvec![
                        "let _ = call 'erlang':'put'(",
                        state_key.clone(),
                        ", ",
                        leaf::var(current),
                        ") in 'true'",
                    ]);
                }
            } else if Self::is_local_var_assignment(expr) {
                let (assign_doc, _) = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);
                if is_last {
                    let current = self.current_state_var();
                    docs.push(docvec![
                        "let _ = call 'erlang':'put'(",
                        state_key.clone(),
                        ", ",
                        leaf::var(current),
                        ") in 'true'",
                    ]);
                }
            } else if is_last {
                let pred_result = self.fresh_temp_var("SortPred");
                let expr_code = self.generate_expression(expr)?;
                let current = self.current_state_var();
                docs.push(docvec![
                    "let ",
                    leaf::var(pred_result.clone()),
                    " = ",
                    expr_code,
                    " in let _ = call 'erlang':'put'(",
                    state_key.clone(),
                    ", ",
                    leaf::var(current),
                    ") in ",
                    leaf::var(pred_result),
                ]);
            } else {
                let doc = self.generate_expression(expr)?;
                docs.push(docvec!["let _ = ", doc, " in "]);
            }
        }

        self.pop_scope();

        // Restore state version — the lambda's state mutations are isolated in process dict.
        self.set_state_version(saved_state_version);

        let sorted_var = self.fresh_temp_var("SortedList");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            " in let ",
            leaf::var(sorted_var.clone()),
            " = call 'lists':'sort'(",
            leaf::var(wrapper_var),
            ", ",
            leaf::var(safe_list_var),
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'get'(",
            state_key.clone(),
            ") in let _ = call 'erlang':'erase'(",
            state_key,
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(sorted_var),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }
}
