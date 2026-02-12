// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Counted loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for counted loop constructs: `repeat`, `timesRepeat:`,
//! `to:do:`, and `to:by:do:`.

use super::super::document::Document;
use super::super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
use crate::docvec;

impl CoreErlangGenerator {
    pub(in crate::codegen::core_erlang) fn generate_repeat(
        &mut self,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // repeat: infinite loop - execute body forever
        // Generate: letrec '_LoopN'/0 = fun () ->
        //     let _BodyFun = <body> in
        //     let _ = apply _BodyFun& () in apply '_LoopN'/0 ()
        // in apply '_LoopN'/0 ()

        let loop_fn = self.fresh_temp_var("Loop");
        let body_var = self.fresh_temp_var("BodyFun");
        let body_code = self.expression_doc(body)?;
        Ok(docvec![
            format!("letrec '{loop_fn}'/0 = fun () -> "),
            format!("let {body_var} = "),
            body_code,
            format!(" in let _ = apply {body_var} () in apply '{loop_fn}'/0 () "),
            format!("in apply '{loop_fn}'/0 ()"),
        ])
    }

    pub(in crate::codegen::core_erlang) fn generate_times_repeat(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_times_repeat_with_mutations(receiver, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_times_repeat_simple(receiver, body)
    }

    pub(in crate::codegen::core_erlang) fn generate_times_repeat_simple(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Generate: let N = <receiver> in
        //           letrec 'repeat'/1 = fun (I) ->
        //               case call 'erlang':'=<'(I, N) of
        //                 'true' -> let _ = <body> in apply 'repeat'/1 (I+1)
        //                 'false' -> 'nil'
        //               end
        //           in apply 'repeat'/1 (1)

        let n_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {n_var} = "),
            receiver_code,
            format!(" in letrec 'repeat'/1 = fun (I) -> "),
            format!("case call 'erlang':'=<'(I, {n_var}) of "),
            "<'true'> when 'true' -> ",
            "let _ = ",
            body_code,
            format!(" in apply 'repeat'/1 (call 'erlang':'+'(I, 1)) "),
            "<'false'> when 'true' -> 'nil' ",
            "end ",
            "in apply 'repeat'/1 (1)",
        ])
    }

    pub(in crate::codegen::core_erlang) fn generate_times_repeat_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // BT-478: Simplified loop signature — only (I, StateAcc), no separate field params.

        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // Generate: let N = <receiver> in
        //           letrec 'repeat'/2 = fun (I, StateAcc) ->
        //               case I =< N of
        //                 'true' -> <body with threading>
        //                          apply 'repeat'/2 (I+1, StateAcc')
        //                 'false' -> StateAcc
        //               end
        //           in apply 'repeat'/2 (1, State)

        let n_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;

        let mut docs: Vec<Document<'static>> = vec![docvec![
            format!("let {n_var} = "),
            receiver_code,
            " in letrec 'repeat'/2 = fun (I, StateAcc) -> ",
            format!("case call 'erlang':'=<'(I, {n_var}) of "),
            "<'true'> when 'true' -> ",
        ]];

        let (body_doc, final_state_version) =
            self.generate_times_repeat_body_with_threading(body, &[])?;
        docs.push(body_doc);
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };

        // Initial call
        let prev_state = self.current_state_var();
        docs.push(docvec![
            format!(" apply 'repeat'/2 (call 'erlang':'+'(I, 1), {final_state_var}) "),
            "<'false'> when 'true' -> {'nil', StateAcc} ",
            "end ",
            format!("in apply 'repeat'/2 (1, {prev_state})"),
        ]);

        Ok(Document::Vec(docs))
    }

    pub(in crate::codegen::core_erlang) fn generate_times_repeat_body_with_threading(
        &mut self,
        body: &Block,
        _mutated_vars: &[String],
    ) -> Result<(Document<'static>, usize)> {
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // BT-153: Mark that we're in a loop body so identifier lookup uses StateAcc
        // Save the previous loop-body context so nested loops don't corrupt it
        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        // BT-478: Check if body has direct field assignments. If not, mutations
        // come from nested constructs and the last expression's result must be bound.
        let has_direct_field_assignments = body.body.iter().any(Self::is_field_assignment);

        let mut docs: Vec<Document<'static>> = Vec::new();

        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                docs.push(docvec![" "]);
            }
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                // Field assignment handles state version internally
                // (removed - generate_field_assignment_open does this)
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
                // Note: generate_field_assignment_open already writes trailing " in "
            } else if self.is_actor_self_send(expr) {
                // BT-245: Self-sends may mutate state — thread state through dispatch
                let doc = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);
            } else if Self::is_local_var_assignment(expr) {
                // BT-153: Handle local variable assignments for REPL context
                // Generate: let _Val = <value> in let StateAccN = maps:put('var', _Val, StateAcc{N-1}) in
                let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);
            } else if is_last && !has_direct_field_assignments {
                // BT-478/BT-483: Last expression with no direct field assignments in body.
                // Mutations come from nested constructs (e.g., inner to:do:).
                // Nested constructs return {Result, State} — extract State via element(2).
                let next_version = self.state_version() + 1;
                let next_var = format!("StateAcc{next_version}");
                let tuple_var = format!("_NestTuple{next_version}");
                let expr_code = self.expression_doc(expr)?;
                self.set_state_version(next_version);
                docs.push(docvec![
                    format!("let {tuple_var} = "),
                    expr_code,
                    format!(" in let {next_var} = call 'erlang':'element'(2, {tuple_var}) in"),
                ]);
            } else {
                let expr_code = self.expression_doc(expr)?;
                docs.push(docvec!["let _ = ", expr_code, " in"]);
            }
        }

        // Capture the final state version before restoring
        let final_state_version = self.state_version();

        // Restore state
        self.in_loop_body = previous_in_loop_body;
        self.set_state_version(saved_state_version);
        Ok((Document::Vec(docs), final_state_version))
    }

    pub(in crate::codegen::core_erlang) fn generate_to_do(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_to_do_with_mutations(receiver, limit, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_to_do_simple(receiver, limit, body)
    }

    pub(in crate::codegen::core_erlang) fn generate_to_do_simple(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Generate: let Start = <receiver> in let End = <limit> in
        //           letrec 'loop'/1 = fun (I) ->
        //               case I =< End of
        //                 'true' -> let _ = apply <body>/1 (I) in
        //                          apply 'loop'/1 (I+1)
        //                 'false' -> 'nil'
        //               end
        //           in apply 'loop'/1 (Start)

        let start_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;

        let end_var = self.fresh_temp_var("temp");
        let limit_code = self.expression_doc(limit)?;

        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {start_var} = "),
            receiver_code,
            format!(" in let {end_var} = "),
            limit_code,
            format!(" in let {body_var} = "),
            body_code,
            " in letrec 'loop'/1 = fun (I) -> ",
            format!("case call 'erlang':'=<'(I, {end_var}) of "),
            "<'true'> when 'true' -> ",
            format!("let _ = apply {body_var} (I) in "),
            "apply 'loop'/1 (call 'erlang':'+'(I, 1)) ",
            "<'false'> when 'true' -> 'nil' ",
            "end ",
            format!("in apply 'loop'/1 ({start_var})"),
        ])
    }

    pub(in crate::codegen::core_erlang) fn generate_to_do_with_mutations(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // BT-478: Simplified loop signature — only (I, StateAcc), no separate field params.
        // Field values are read from StateAcc when needed. This correctly handles nested
        // control flow constructs that modify state and return the updated StateAcc.

        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // Generate: let Start = <receiver> in let End = <limit> in
        //           letrec 'loop'/2 = fun (I, StateAcc) ->
        //               case I =< End of
        //                 'true' -> <body with state threading>
        //                          apply 'loop'/2 (I+1, StateAcc')
        //                 'false' -> StateAcc
        //               end
        //           in apply 'loop'/2 (Start, State)

        let start_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;

        let end_var = self.fresh_temp_var("temp");
        let limit_code = self.expression_doc(limit)?;

        let mut docs: Vec<Document<'static>> = vec![docvec![
            format!("let {start_var} = "),
            receiver_code,
            format!(" in let {end_var} = "),
            limit_code,
            " in letrec 'loop'/2 = fun (I, StateAcc) -> ",
            format!("case call 'erlang':'=<'(I, {end_var}) of "),
            "<'true'> when 'true' -> ",
        ]];

        let (body_doc, final_state_version) =
            self.generate_to_do_body_with_threading(body, &[])?;
        docs.push(body_doc);
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };

        // Initial call
        let prev_state = self.current_state_var();
        docs.push(docvec![
            format!(" apply 'loop'/2 (call 'erlang':'+'(I, 1), {final_state_var}) "),
            "<'false'> when 'true' -> {'nil', StateAcc} ",
            "end ",
            format!(" in apply 'loop'/2 ({start_var}, {prev_state})"),
        ]);

        Ok(Document::Vec(docs))
    }

    pub(in crate::codegen::core_erlang) fn generate_to_do_body_with_threading(
        &mut self,
        body: &Block,
        _mutated_vars: &[String],
    ) -> Result<(Document<'static>, usize)> {
        // Bind the block parameter to I
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, "I");
        }

        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // BT-153: Mark that we're in a loop body so identifier lookup uses StateAcc
        // Save the previous loop-body context so nested loops don't corrupt it
        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        // BT-478: Check if body has direct field assignments vs only nested mutations.
        // Direct field assignments update StateAcc via generate_field_assignment_open.
        // Nested mutations (e.g., inner to:do: with field writes) return the updated
        // state as the expression result.
        let has_direct_field_assignments = body.body.iter().any(Self::is_field_assignment);

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Generate body expressions
        for (i, expr) in body.body.iter().enumerate() {
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                // Field assignment - generate_field_assignment_open writes:
                //   "let _Val = <value> in let StateAccN = maps:put(..., StateAccN-1) in "
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);

                // BT-478: Do NOT write a bare state var for the last expression.
                // The trailing "in " from generate_field_assignment_open means the
                // caller's recursive apply will be the body of the let, which is valid
                // Core Erlang: "let StateAcc1 = ... in apply 'loop'/2 (I+1, StateAcc1)"
            } else if Self::is_local_var_assignment(expr) {
                // BT-153: Handle local variable assignments for REPL context
                // No "in" prefix needed — prior expressions already end with "in "
                // (field_assignment_open, local_var_assignment_in_loop, and non-assignment
                // branches all emit trailing "in" or "in ").
                let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);
                docs.push(docvec![" "]);
            } else {
                // Non-assignment expression (could be a nested control flow construct)
                if is_last && !has_direct_field_assignments {
                    // BT-478/BT-483: Last expression with no direct field assignments in body.
                    // This means mutations come from nested constructs (e.g., inner to:do:).
                    // Nested constructs return {Result, State} — extract State via element(2).
                    //
                    // IMPORTANT: Compute the next var name WITHOUT incrementing state_version
                    // first, so the inner expression generates code that reads from the
                    // CURRENT StateAcc, not the binding we're about to create.
                    let next_version = self.state_version() + 1;
                    let next_var = format!("StateAcc{next_version}");
                    let tuple_var = format!("_NestTuple{next_version}");
                    let expr_code = self.expression_doc(expr)?;
                    // NOW increment state version
                    self.set_state_version(next_version);
                    // Write: let _NestTupleN = <inner loop> in let StateAccN = element(2, _NestTupleN) in
                    docs.push(docvec![
                        format!("let {tuple_var} = "),
                        expr_code,
                        format!(
                            " in let {next_var} = call 'erlang':'element'(2, {tuple_var}) in "
                        ),
                    ]);
                } else {
                    // Not last, or there are direct field assignments that update state
                    let expr_code = self.expression_doc(expr)?;
                    docs.push(docvec!["let _ = ", expr_code, " in "]);
                }
            }
        }

        // Capture the final state version before restoring
        let final_state_version = self.state_version();

        // Restore state
        self.in_loop_body = previous_in_loop_body;
        self.set_state_version(saved_state_version);
        self.pop_scope();

        Ok((Document::Vec(docs), final_state_version))
    }

    /// Generates code for `to:by:do:` iteration with custom step.
    ///
    /// Handles both positive and negative steps:
    /// - Positive step: iterate while I <= End
    /// - Negative step: iterate while I >= End
    /// - Step of 0: Returns 'nil' immediately (prevents infinite loop)
    ///
    /// # Safety
    ///
    /// The generated condition ensures that a step of 0 will not execute,
    /// preventing infinite loops.
    pub(in crate::codegen::core_erlang) fn generate_to_by_do(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        step: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_to_by_do_with_mutations(receiver, limit, step, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_to_by_do_simple(receiver, limit, step, body)
    }

    pub(in crate::codegen::core_erlang) fn generate_to_by_do_simple(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        step: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // Generate: let Start = <receiver> in let End = <limit> in let Step = <step> in
        //           letrec 'loop'/1 = fun (I) ->
        //               case (Step > 0 andalso I =< End) orelse (Step < 0 andalso I >= End) of
        //                 'true' -> let _ = apply <body>/1 (I) in
        //                          apply 'loop'/1 (I + Step)
        //                 'false' -> 'nil'
        //               end
        //           in apply 'loop'/1 (Start)

        let start_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;

        let end_var = self.fresh_temp_var("temp");
        let limit_code = self.expression_doc(limit)?;

        let step_var = self.fresh_temp_var("temp");
        let step_code = self.expression_doc(step)?;

        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        // Generate condition: (Step > 0 andalso I =< End) orelse (Step < 0 andalso I >= End)
        // Use nested case statements since andalso/orelse aren't BIFs
        Ok(docvec![
            format!("let {start_var} = "),
            receiver_code,
            format!(" in let {end_var} = "),
            limit_code,
            format!(" in let {step_var} = "),
            step_code,
            format!(" in let {body_var} = "),
            body_code,
            " in letrec 'loop'/1 = fun (I) -> ",
            format!("let Continue = case call 'erlang':'>'({step_var}, 0) of "),
            format!("<'true'> when 'true' -> call 'erlang':'=<'(I, {end_var}) "),
            "<'false'> when 'true' -> ",
            format!("case call 'erlang':'<'({step_var}, 0) of "),
            format!("<'true'> when 'true' -> call 'erlang':'>='(I, {end_var}) "),
            "<'false'> when 'true' -> 'false' ",
            "end ",
            "end in case Continue of ",
            "<'true'> when 'true' -> ",
            format!("let _ = apply {body_var} (I) in "),
            format!("apply 'loop'/1 (call 'erlang':'+'(I, {step_var})) "),
            "<'false'> when 'true' -> 'nil' ",
            "end ",
            format!("in apply 'loop'/1 ({start_var})"),
        ])
    }

    pub(in crate::codegen::core_erlang) fn generate_to_by_do_with_mutations(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        step: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        // BT-478: Simplified loop signature — only (I, StateAcc), no separate field params.

        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // Generate: let Start = <receiver> in let End = <limit> in let Step = <step> in
        //           letrec 'loop'/2 = fun (I, StateAcc) ->
        //               case (Step > 0 andalso I =< End) orelse (Step < 0 andalso I >= End) of
        //                 'true' -> <body with state threading>
        //                          apply 'loop'/2 (I+Step, StateAcc')
        //                 'false' -> StateAcc
        //               end
        //           in apply 'loop'/2 (Start, State)

        let start_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;

        let end_var = self.fresh_temp_var("temp");
        let limit_code = self.expression_doc(limit)?;

        let step_var = self.fresh_temp_var("temp");
        let step_code = self.expression_doc(step)?;

        // Generate condition: (Step > 0 andalso I =< End) orelse (Step < 0 andalso I >= End)
        let mut docs: Vec<Document<'static>> = vec![docvec![
            format!("let {start_var} = "),
            receiver_code,
            format!(" in let {end_var} = "),
            limit_code,
            format!(" in let {step_var} = "),
            step_code,
            " in letrec 'loop'/2 = fun (I, StateAcc) -> ",
            format!("let Continue = case call 'erlang':'>'({step_var}, 0) of "),
            format!("<'true'> when 'true' -> call 'erlang':'=<'(I, {end_var}) "),
            "<'false'> when 'true' -> ",
            format!("case call 'erlang':'<'({step_var}, 0) of "),
            format!("<'true'> when 'true' -> call 'erlang':'>='(I, {end_var}) "),
            "<'false'> when 'true' -> 'false' ",
            "end ",
            "end in case Continue of ",
            "<'true'> when 'true' -> ",
        ]];

        let (body_doc, final_state_version) =
            self.generate_to_do_body_with_threading(body, &[])?;
        docs.push(body_doc);
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };

        // Initial call
        let prev_state = self.current_state_var();
        docs.push(docvec![
            format!(" apply 'loop'/2 (call 'erlang':'+'(I, {step_var}), {final_state_var}) "),
            "<'false'> when 'true' -> {'nil', StateAcc} ",
            "end ",
            format!(" in apply 'loop'/2 ({start_var}, {prev_state})"),
        ]);

        Ok(Document::Vec(docs))
    }
}
