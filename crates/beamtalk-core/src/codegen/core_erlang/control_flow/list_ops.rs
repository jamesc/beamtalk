// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List iteration control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for list iteration constructs: `do:`, `collect:`,
//! `select:`, `reject:`, and `inject:into:`.

use super::super::document::Document;
use super::super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
use crate::docvec;

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
        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // BT-524: Add is_list guard for non-list collection types.
        // Convert non-list receivers to a list via beamtalk_collection_ops:to_list/1
        // so that state mutations are properly threaded through lists:foldl.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");

        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        let mut docs: Vec<Document<'static>> = vec![docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(
                " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> {list_var} \
                 <'false'> when 'true' -> \
                 call 'beamtalk_collection_ops':'to_list'({list_var}) end \
                 in let {lambda_var} = fun ({item_var}, StateAcc) -> "
            ),
        ]];

        // Generate body with state threading
        let body_doc = self.generate_list_do_body_with_threading(body, &item_var)?;
        docs.push(body_doc);

        // Capture current state before the foldl call
        let initial_state = self.current_state_var();

        // BT-483: Return {Result, State} tuple — do: returns nil as result
        let fold_result = self.fresh_temp_var("FoldResult");
        docs.push(docvec![format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {initial_state}, {safe_list_var}) \
             in {{'nil', {fold_result}}}"
        )]);

        Ok(Document::Vec(docs))
    }

    pub(in crate::codegen::core_erlang) fn generate_list_do_body_with_threading(
        &mut self,
        body: &Block,
        item_var: &str,
    ) -> Result<Document<'static>> {
        // Temporarily bind the block parameter to the item variable
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, item_var);
        }

        // Replace "State" with "StateAcc" for this nested context
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // Mark that we're in a loop body so field reads use StateAcc
        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Generate the body expression(s), threading state through assignments
        for (i, expr) in body.body.iter().enumerate() {
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);

                if is_last {
                    // Last expression: close with the final state variable
                    docs.push(docvec![self.current_state_var()]);
                }
            } else if self.is_actor_self_send(expr) {
                // BT-245: Self-sends may mutate state — thread state through dispatch
                let doc = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);

                if is_last {
                    docs.push(docvec![self.current_state_var()]);
                }
            } else {
                // Non-assignment expression
                if i > 0 {
                    // Sequence with previous expression using let _ = ... in
                    docs.push(docvec!["let _ = "]);
                }
                let doc = self.generate_expression(expr)?;
                docs.push(doc);

                if !is_last {
                    docs.push(docvec![" in "]);
                }
            }
        }

        self.set_state_version(saved_state_version);
        self.in_loop_body = previous_in_loop_body;
        self.pop_scope();

        Ok(Document::Vec(docs))
    }

    pub(in crate::codegen::core_erlang) fn generate_list_collect(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // list collect: is map
        self.generate_simple_list_op(receiver, body, "map")
    }

    pub(in crate::codegen::core_erlang) fn generate_list_select(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // list select: is filter
        self.generate_simple_list_op(receiver, body, "filter")
    }

    pub(in crate::codegen::core_erlang) fn generate_list_reject(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
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
        let body_code = self.expression_doc(body)?;

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
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Analyze block for mutations
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_inject_with_mutations(receiver, initial, body_block);
            }
        }

        // Simple case: no mutations, use standard lists:foldl
        // BT-505/BT-511: Add is_list guard for non-list collection types (Stream, etc.)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let init_var = self.fresh_temp_var("temp");
        let init_code = self.expression_doc(initial)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {init_var} = "),
            init_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'lists':'foldl'({body_var}, {init_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'inject:into:', [{init_var}, {body_var}]) end"
            ),
        ])
    }

    pub(in crate::codegen::core_erlang) fn generate_list_inject_with_mutations(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // BT-524: Add is_list guard for non-list collection types.
        // Convert non-list receivers to a list via beamtalk_collection_ops:to_list/1
        // so that state mutations are properly threaded through lists:foldl.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let init_var = self.fresh_temp_var("temp");
        let init_code = self.expression_doc(initial)?;
        let lambda_var = self.fresh_temp_var("temp");

        let acc_state_var = self.fresh_temp_var("AccSt");
        let mut docs: Vec<Document<'static>> = vec![docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(
                " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> {list_var} \
                 <'false'> when 'true' -> \
                 call 'beamtalk_collection_ops':'to_list'({list_var}) end \
                 in let {init_var} = "
            ),
            init_code,
            format!(
                " in let {lambda_var} = fun (Item, {acc_state_var}) -> \
                 let Acc = call 'erlang':'element'(1, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(2, {acc_state_var}) in "
            ),
        ]];

        // Generate body with state threading
        let body_doc = self.generate_list_inject_body_with_threading(body)?;
        docs.push(body_doc);

        // BT-483: Return {Result, State} tuple — inject:into: returns accumulator as result
        let initial_state = self.current_state_var();
        let result_var = self.fresh_temp_var("temp");
        let acc_out = self.fresh_temp_var("AccOut");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![format!(
            " in let {result_var} = call 'lists':'foldl'({lambda_var}, {{{init_var}, {initial_state}}}, {safe_list_var}) \
             in let {acc_out} = call 'erlang':'element'(1, {result_var}) \
             in let {state_out} = call 'erlang':'element'(2, {result_var}) \
             in {{{acc_out}, {state_out}}}"
        )]);

        Ok(Document::Vec(docs))
    }

    pub(in crate::codegen::core_erlang) fn generate_list_inject_body_with_threading(
        &mut self,
        body: &Block,
    ) -> Result<Document<'static>> {
        // The block should have 2 parameters: item and accumulator
        self.push_scope();

        if !body.parameters.is_empty() {
            self.bind_var(&body.parameters[0].name, "Item");
        }
        if body.parameters.len() >= 2 {
            self.bind_var(&body.parameters[1].name, "Acc");
        }

        // Replace "State" with "StateAcc" for this nested context
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // Mark that we're in a loop body so field reads use StateAcc
        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Generate the body expression(s), threading state through assignments
        let mut has_mutations = false;
        for (i, expr) in body.body.iter().enumerate() {
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                has_mutations = true;
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);

                if is_last {
                    let final_state = self.current_state_var();
                    docs.push(docvec![format!("{{_Val, {final_state}}}")]);
                }
            } else if self.is_actor_self_send(expr) {
                has_mutations = true;
                // BT-245: Self-sends may mutate state — thread state through dispatch
                let doc = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);

                if is_last {
                    let final_state = self.current_state_var();
                    if let Some(dv) = self.last_dispatch_var.clone() {
                        let acc_result = self.fresh_temp_var("AccResult");
                        docs.push(docvec![format!(
                            "let {acc_result} = call 'erlang':'element'(1, {dv}) in {{{acc_result}, {final_state}}}"
                        )]);
                    } else {
                        docs.push(docvec![format!("{{'nil', {final_state}}}")]);
                    }
                }
            } else {
                // Non-assignment expression
                if i > 0 && !has_mutations {
                    // Previous expression was not a field assignment, so we need to sequence
                    docs.push(docvec!["let _ = "]);
                }

                if is_last {
                    // Last expression: capture its value as the new accumulator
                    let acc_var = self.fresh_temp_var("AccOut");
                    let expr_code = self.expression_doc(expr)?;

                    // Return {NewAcc, NewState}
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    docs.push(docvec![
                        format!("let {acc_var} = "),
                        expr_code,
                        format!(" in {{{acc_var}, {final_state}}}"),
                    ]);
                } else {
                    let doc = self.generate_expression(expr)?;
                    docs.push(doc);
                    docs.push(docvec![" in "]);
                }
            }
        }

        self.set_state_version(saved_state_version);
        self.in_loop_body = previous_in_loop_body;
        self.pop_scope();

        Ok(Document::Vec(docs))
    }
}
