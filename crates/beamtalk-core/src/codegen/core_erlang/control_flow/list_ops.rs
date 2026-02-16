// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List iteration control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for list iteration constructs: `do:`, `collect:`,
//! `select:`, `reject:`, and `inject:into:`.

use std::fmt::Write;

use super::super::document::Document;
use super::super::intrinsics::validate_block_arity_exact;
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
        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // BT-598: Determine which local variables need threading through the loop.
        let threaded_locals = self.compute_threaded_locals_for_loop(body, None);

        // BT-524: Add is_list guard for non-list collection types.
        // Convert non-list receivers to a list via beamtalk_collection_ops:to_list/1
        // so that state mutations are properly threaded through lists:foldl.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");

        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        // BT-598: Before the foldl, pack threaded locals into the StateAcc map
        let initial_state = self.current_state_var();
        let mut init_state_code = initial_state.clone();
        if !threaded_locals.is_empty() && !self.is_repl_mode {
            // Pack local vars into a copy of State for the initial accumulator
            let mut pack_prefix = String::new();
            let mut current = initial_state.clone();
            for (idx, var_name) in threaded_locals.iter().enumerate() {
                let packed_var = self.fresh_temp_var("Packed");
                let core_var = self
                    .lookup_var(var_name)
                    .cloned()
                    .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                let key = Self::local_state_key(var_name);
                let _ = write!(
                    pack_prefix,
                    "let {packed_var} = call 'maps':'put'('{key}', {core_var}, {current}) in "
                );
                current.clone_from(&packed_var);
                if idx == threaded_locals.len() - 1 {
                    init_state_code = packed_var;
                }
            }
            // Prepend packing code
            let mut pre_docs: Vec<Document<'static>> = vec![Document::String(pack_prefix)];
            pre_docs.push(docvec![
                format!("let {list_var} = "),
                recv_code,
                format!(
                    " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                     <'true'> when 'true' -> {list_var} \
                     <'false'> when 'true' -> \
                     call 'beamtalk_collection_ops':'to_list'({list_var}) end \
                     in let {lambda_var} = fun ({item_var}, StateAcc) -> "
                ),
            ]);

            let body_doc = self.generate_list_do_body_with_threading(body, &item_var)?;
            pre_docs.push(body_doc);

            // BT-598: After foldl, extract threaded locals and rebind them
            let fold_result = self.fresh_temp_var("FoldResult");
            let mut post_code = format!(
                " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {init_state_code}, {safe_list_var}) in "
            );
            // Extract locals from the fold result (which is the final StateAcc)
            for var_name in &threaded_locals {
                let core_var = self
                    .lookup_var(var_name)
                    .cloned()
                    .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                let key = Self::local_state_key(var_name);
                let _ = write!(
                    post_code,
                    "let {core_var} = call 'maps':'get'('{key}', {fold_result}) in "
                );
            }
            // Extract the clean state (with locals still in it, but that's harmless)
            let _ = write!(post_code, "{{'nil', {fold_result}}}");
            pre_docs.push(Document::String(post_code));

            return Ok(Document::Vec(pre_docs));
        }

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

        // BT-483: Return {Result, State} tuple — do: returns nil as result
        let fold_result = self.fresh_temp_var("FoldResult");
        docs.push(Document::String(format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {initial_state}, {safe_list_var}) \
             in {{'nil', {fold_result}}}"
        )));

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
        let mut has_mutations = false;

        // BT-598: At the start of each foldl iteration, read threaded locals from StateAcc.
        // This ensures that local variables resolve to their loop-accumulated values,
        // not the stale outer-scope let-bound values.
        let threaded = self.compute_threaded_locals_for_loop(body, None);
        for var_name in &threaded {
            let core_var = Self::to_core_erlang_var(var_name);
            let key = Self::local_state_key(var_name);
            // Rebind in the loop scope so identifier lookup uses the StateAcc value
            self.bind_var(var_name, &core_var);
            docs.push(Document::String(format!(
                "let {core_var} = call 'maps':'get'('{key}', StateAcc) in "
            )));
        }

        // Generate the body expression(s), threading state through assignments
        for (i, expr) in body.body.iter().enumerate() {
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                has_mutations = true;
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);

                if is_last {
                    // Last expression: close with the final state variable
                    docs.push(Document::String(self.current_state_var()));
                }
            } else if self.is_actor_self_send(expr) {
                has_mutations = true;
                // BT-245: Self-sends may mutate state — thread state through dispatch
                let doc = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);

                if is_last {
                    docs.push(Document::String(self.current_state_var()));
                }
            } else if Self::is_local_var_assignment(expr) {
                has_mutations = true;
                // BT-598: Local variable mutation — thread through StateAcc map
                let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);

                if is_last {
                    // Last expression: close with the final state variable
                    docs.push(Document::String(format!(" {}", self.current_state_var())));
                }
            } else {
                // Non-assignment expression
                if i > 0 {
                    // Previous expression or mutation left scope open, wrap to discard value
                    docs.push(Document::Str("let _ = "));
                }
                let doc = self.generate_expression(expr)?;
                docs.push(doc);

                if is_last && has_mutations {
                    // Return final state after non-assignment last expression
                    docs.push(Document::String(format!(
                        " in {}",
                        self.current_state_var()
                    )));
                } else if !is_last {
                    docs.push(Document::Str(" in "));
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
        // BT-493: Validate body block arity (must be 1-arg)
        validate_block_arity_exact(
            body,
            1,
            "collect:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list collect: [:item | item * 2]",
        )?;

        // list collect: is map
        self.generate_simple_list_op(receiver, body, "map")
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

        // list select: is filter
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
        // BT-493: Validate body block arity (must be 2-arg: element and accumulator)
        validate_block_arity_exact(
            body,
            2,
            "inject:into:",
            "Fix: The body block must take two arguments (element and accumulator):\n\
             \x20 list inject: 0 into: [:each :sum | sum + each]",
        )?;

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

        // BT-598: Determine which local variables need threading through the loop.
        let threaded_locals = self.compute_threaded_locals_for_loop(body, None);

        // BT-524: Add is_list guard for non-list collection types.
        // Convert non-list receivers to a list via beamtalk_collection_ops:to_list/1
        // so that state mutations are properly threaded through lists:foldl.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let init_var = self.fresh_temp_var("temp");
        let init_code = self.expression_doc(initial)?;
        let lambda_var = self.fresh_temp_var("temp");

        // BT-598: Pack threaded locals into StateAcc before foldl
        let initial_state = self.current_state_var();
        let mut init_state_code = initial_state.clone();
        let mut pack_prefix = String::new();
        if !threaded_locals.is_empty() && !self.is_repl_mode {
            let mut current = initial_state.clone();
            for var_name in &threaded_locals {
                let packed_var = self.fresh_temp_var("Packed");
                let core_var = self
                    .lookup_var(var_name)
                    .cloned()
                    .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                let key = Self::local_state_key(var_name);
                let _ = write!(
                    pack_prefix,
                    "let {packed_var} = call 'maps':'put'('{key}', {core_var}, {current}) in "
                );
                current = packed_var;
            }
            init_state_code = current;
        }

        let acc_state_var = self.fresh_temp_var("AccSt");
        let mut docs: Vec<Document<'static>> = vec![
            Document::String(pack_prefix),
            docvec![
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
            ],
        ];

        // Generate body with state threading
        let body_doc = self.generate_list_inject_body_with_threading(body)?;
        docs.push(body_doc);

        // BT-483: Return {Result, State} tuple — inject:into: returns accumulator as result
        let result_var = self.fresh_temp_var("temp");
        let acc_out = self.fresh_temp_var("AccOut");
        let state_out = self.fresh_temp_var("StOut");

        let mut post_code = format!(
            " in let {result_var} = call 'lists':'foldl'({lambda_var}, {{{init_var}, {init_state_code}}}, {safe_list_var}) \
             in let {acc_out} = call 'erlang':'element'(1, {result_var}) \
             in let {state_out} = call 'erlang':'element'(2, {result_var}) in "
        );

        // BT-598: Extract threaded locals from the final state
        if !threaded_locals.is_empty() && !self.is_repl_mode {
            for var_name in &threaded_locals {
                let core_var = self
                    .lookup_var(var_name)
                    .cloned()
                    .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                let key = Self::local_state_key(var_name);
                let _ = write!(
                    post_code,
                    "let {core_var} = call 'maps':'get'('{key}', {state_out}) in "
                );
            }
        }

        let _ = write!(post_code, "{{{acc_out}, {state_out}}}");
        docs.push(Document::String(post_code));

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

        // BT-598: At the start of each foldl iteration, read threaded locals from StateAcc.
        let threaded = self.compute_threaded_locals_for_loop(body, None);
        for var_name in &threaded {
            let core_var = Self::to_core_erlang_var(var_name);
            let key = Self::local_state_key(var_name);
            self.bind_var(var_name, &core_var);
            docs.push(Document::String(format!(
                "let {core_var} = call 'maps':'get'('{key}', StateAcc) in "
            )));
        }

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
                    docs.push(Document::String(format!("{{_Val, {final_state}}}")));
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
                        docs.push(Document::String(format!(
                            "let {acc_result} = call 'erlang':'element'(1, {dv}) in {{{acc_result}, {final_state}}}"
                        )));
                    } else {
                        docs.push(Document::String(format!("{{'nil', {final_state}}}")));
                    }
                }
            } else if Self::is_local_var_assignment(expr) {
                has_mutations = true;
                // BT-598: Local variable mutation — thread through StateAcc map
                let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);

                if is_last {
                    // inject:into: last expression should be the accumulator value
                    let final_state = self.current_state_var();
                    docs.push(Document::String(format!(" {{_Val, {final_state}}}")));
                }
            } else {
                // Non-assignment expression
                if i > 0 && !is_last {
                    // Sequence with previous expression using let _ = ... in
                    docs.push(Document::Str("let _ = "));
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
                    docs.push(Document::Str(" in "));
                }
            }
        }

        self.set_state_version(saved_state_version);
        self.in_loop_body = previous_in_loop_body;
        self.pop_scope();

        Ok(Document::Vec(docs))
    }
}
