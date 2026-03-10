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
            let result_doc = if matches!(plan.context, CodeGenContext::ValueType) {
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

        let list_var = self.fresh_temp_var("temp");
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
                extract_doc,
                repack_doc,
                "{",
                Document::String(final_list),
                ", ",
                Document::String(stateacc),
                "}",
            ]);
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

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {{[], {init_state}}}, {safe_list_var}) \
             in let {rev_list} = call 'erlang':'element'(1, {fold_result}) \
             in let {final_list} = call 'lists':'reverse'({rev_list}) \
             in let {state_out} = call 'erlang':'element'(2, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{final_list}, {state_out}}}");
        docs.push(Document::String(post_code));

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

        let list_var = self.fresh_temp_var("temp");
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
                extract_doc,
                repack_doc,
                "{",
                Document::String(final_list),
                ", ",
                Document::String(stateacc),
                "}",
            ]);
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

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {{[], {init_state}}}, {safe_list_var}) \
             in let {rev_list} = call 'erlang':'element'(1, {fold_result}) \
             in let {final_list} = call 'lists':'reverse'({rev_list}) \
             in let {state_out} = call 'erlang':'element'(2, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{final_list}, {state_out}}}");
        docs.push(Document::String(post_code));

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

        // Simple case: no mutations, delegate to beamtalk_collection:inject_into
        // which wraps the block to call Block(Acc, Elem) matching Beamtalk convention.
        // BT-820: lists:foldl calls Fun(Elem, Acc) but Beamtalk convention is Block(Acc, Elem).
        let recv_code = self.expression_doc(receiver)?;
        let init_code = self.expression_doc(initial)?;
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            "call 'beamtalk_collection':'inject_into'(",
            recv_code,
            ", ",
            init_code,
            ", ",
            body_code,
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
            // BT-1304: Elide maybe_await on the fold accumulator when the initial value is
            // provably sync. The accumulator is re-extracted at each iteration via
            // erlang:element/2; if the initial value is sync and the body only produces
            // sync values (e.g., arithmetic), the accumulator remains non-future throughout.
            //
            // **Invariant assumption**: this optimization is sound when the fold body's
            // return expression is always sync across all branches. For the common case —
            // numerical reduction (e.g., `acc + x`) — this holds. If the body returns an
            // async value (e.g., an actor method call) on any iteration, the next iteration's
            // `Acc` would be a future and arithmetic on it would crash with badarg. Such
            // bodies are user errors: `inject:into:` is designed for synchronous
            // accumulation; bodies that produce futures should use explicit `await:`.
            // The `expressions.rs` assignment handler additionally removes the acc param from
            // `current_sync_vars` if it is reassigned (`:=`) to a non-sync value inside the
            // body, covering explicit reassignment patterns.
            //
            // The block parameter *shadows* any outer variable with the same name. We must
            // not let an outer sync-var for, say, `acc` bleed into a nested fold that uses
            // the same parameter name but a non-sync initial. Save/restore the outer state:
            //   1. Remove any outer sync-var for this name (it is now shadowed).
            //   2. Insert the new sync-var if *this* fold's initial is provably sync.
            //   3. After the body, remove what we inserted and restore what we removed.
            let acc_param_name = body.parameters.first().map(|p| p.name.to_string());
            let outer_acc_sync = acc_param_name
                .as_ref()
                .is_some_and(|name| self.current_sync_vars.remove(name.as_str()));
            let acc_sync_inserted = acc_param_name.as_ref().is_some_and(|name| {
                if self.is_definitely_sync(initial) {
                    self.current_sync_vars.insert(name.clone());
                    true
                } else {
                    false
                }
            });
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

            let (body_doc, _) =
                self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlInject)?;
            docs.push(body_doc);
            self.pop_scope();
            // BT-1304: Restore the accumulator sync state.
            if let Some(ref name) = acc_param_name {
                if acc_sync_inserted {
                    self.current_sync_vars.remove(name.as_str());
                }
                if outer_acc_sync {
                    self.current_sync_vars.insert(name.clone());
                }
            }

            let result_var = self.fresh_temp_var("temp");
            let acc_out = self.fresh_temp_var("AccOut");

            // Extract each updated local var from tuple positions 2..N.
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&result_var, 2, self);
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
        // BT-1304: Elide maybe_await on the fold accumulator (same invariant assumption and
        // save/restore logic as the tuple-acc path — see comment above for full rationale).
        let map_acc_param_name = body.parameters.first().map(|p| p.name.to_string());
        let outer_map_acc_sync = map_acc_param_name
            .as_ref()
            .is_some_and(|name| self.current_sync_vars.remove(name.as_str()));
        let map_acc_sync_inserted = map_acc_param_name.as_ref().is_some_and(|name| {
            if self.is_definitely_sync(initial) {
                self.current_sync_vars.insert(name.clone());
                true
            } else {
                false
            }
        });
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) =
            self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlInject)?;
        docs.push(body_doc);
        self.pop_scope();
        // BT-1304: Restore the accumulator sync state.
        if let Some(ref name) = map_acc_param_name {
            if map_acc_sync_inserted {
                self.current_sync_vars.remove(name.as_str());
            }
            if outer_map_acc_sync {
                self.current_sync_vars.insert(name.clone());
            }
        }

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
    fn test_list_inject_into_generates_collection_ops_wrapper() {
        // inject:into: (no mutations) delegates to beamtalk_collection:inject_into
        // which adapts the Erlang lists:foldl(Fun, Acc, List) argument order to
        // Beamtalk's Block(Acc, Elem) convention (BT-820).
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items inject: 0 into: [:acc :item | acc + item]\n";
        let code = codegen(src);
        assert!(
            code.contains("'beamtalk_collection':'inject_into'"),
            "inject:into: should delegate to beamtalk_collection:inject_into. Got:\n{code}"
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
        // BT-1304: When the initial accumulator is a literal (provably sync), maybe_await
        // should be elided on the accumulator parameter inside the fold body.
        let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items inject: 0 into: [:acc :item | count := count + 1. acc + item]\n";
        let code = codegen(src);
        // The accumulator `acc` binds to Erlang var `Acc` inside the foldl lambda.
        // With BT-1304, Acc is provably sync (initial = literal 0), so maybe_await is elided.
        assert!(
            !code.contains("'maybe_await'(Acc)"),
            "BT-1304: maybe_await should be elided on the fold accumulator when initial is a literal. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_sync_var_initial_elides_acc_maybe_await() {
        // BT-1304: When the initial accumulator is a sync var (method parameter), maybe_await
        // is also elided on the fold accumulator.
        let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items start: start =>\n    count := 0\n    items inject: start into: [:acc :item | count := count + 1. acc + item]\n";
        let code = codegen(src);
        // `start` is a method parameter → sync. acc is provably sync → maybe_await elided.
        assert!(
            !code.contains("'maybe_await'(Acc)"),
            "BT-1304: maybe_await should be elided on the fold accumulator when initial is a sync param. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_non_literal_initial_keeps_acc_maybe_await() {
        // BT-1304: When the initial accumulator is NOT provably sync (e.g. a local var that
        // was never declared sync), maybe_await is retained on the accumulator.
        // Note: `self.someState` is a field read — not a literal → NOT in sync_vars.
        let src = "Actor subclass: Ctr\n  state: x = 0\n  state: initial = 0\n\n  run: items =>\n    count := 0\n    items inject: self.initial into: [:acc :item | count := count + 1. acc + item]\n";
        let code = codegen(src);
        // `self.initial` is a state field read → not a literal, not a sync var.
        // maybe_await should be retained on Acc.
        assert!(
            code.contains("'maybe_await'(Acc)"),
            "BT-1304: maybe_await should be KEPT on the fold accumulator when initial is a non-sync field read. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_non_literal_initial_keeps_acc_maybe_await_map_acc() {
        // BT-1304: When the initial accumulator is NOT provably sync and the block forces
        // the map-accumulator path (due to a field mutation), maybe_await is still retained.
        let src = "Actor subclass: Ctr\n  state: count = 0\n  state: initial = 0\n\n  run: items =>\n    items inject: self.initial into: [:acc :item | self.count := self.count + 1. acc + item]\n";
        let code = codegen(src);
        assert!(
            code.contains("'maybe_await'(Acc)"),
            "BT-1304: maybe_await should be KEPT on the fold accumulator (map-acc path) when initial is a non-sync field read. Got:\n{code}"
        );
    }

    #[test]
    fn test_inject_nested_scope_bleeding_regression() {
        // BT-1304 regression: outer inject has sync initial (adds `acc` to sync_vars);
        // inner inject has a non-sync initial (`self.initial`) with the *same* parameter
        // name `acc`. Both blocks mutate `sharedCount`, which forces both injects through
        // `generate_list_inject_with_mutations` where the BT-1304 sync-var logic runs.
        //
        // Before the save/restore fix, the outer fold's sync marking for `acc` would bleed
        // into the inner fold body, causing `maybe_await(Acc)` to be silently elided for
        // an accumulator that might be a future. After the fix, `maybe_await(Acc)` must
        // be present in the generated code.
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
        // The inner fold has non-sync initial → must retain maybe_await on its Acc.
        assert!(
            code.contains("'maybe_await'(Acc)"),
            "BT-1304 scope-bleeding: inner fold with non-sync initial must retain \
             maybe_await(Acc) even when outer fold has same param name. Got:\n{code}"
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
}
