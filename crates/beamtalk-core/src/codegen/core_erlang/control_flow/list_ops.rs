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

    pub(in crate::codegen::core_erlang) fn generate_list_do_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new(self, body, None);

        // BT-524: Add is_list guard for non-list collection types.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        // BT-598/BT-1053: Pack threaded locals into StateAcc before the foldl.
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
    /// Accumulator is `{ResultList, StateAcc}`. Each iteration runs the body
    /// with state threading (for self-sends, field writes), prepends the result
    /// to the list, and threads the new state. After foldl, reverses the list.
    pub(in crate::codegen::core_erlang) fn generate_list_collect_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new(self, body, None);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        // Accumulator is {ResultList, StateAcc}
        let acc_state_var = self.fresh_temp_var("AccSt");

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

        // After foldl: extract result list and reverse it, extract final state.
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
    /// Uses foldl with `{ResultList, StateAcc}` accumulator. Each iteration runs the body
    /// with state threading, then conditionally includes the item based on the block result.
    /// For `reject:` (negate=true), the condition is inverted.
    pub(in crate::codegen::core_erlang) fn generate_list_filter_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        negate: bool,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new(self, body, None);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        // Accumulator is {ResultList, StateAcc}
        let acc_state_var = self.fresh_temp_var("AccSt");

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

        // After foldl: reverse result list, extract final state.
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

    pub(in crate::codegen::core_erlang) fn generate_list_inject_with_mutations(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new(self, body, None);

        // BT-524: Add is_list guard for non-list collection types.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let init_var = self.fresh_temp_var("temp");
        let init_code = self.expression_doc(initial)?;
        let lambda_var = self.fresh_temp_var("temp");

        // BT-598: Pack threaded locals into StateAcc before foldl.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let acc_state_var = self.fresh_temp_var("AccSt");

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
}
