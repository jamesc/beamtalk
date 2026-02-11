// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List iteration control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for list iteration constructs: `do:`, `collect:`,
//! `select:`, `reject:`, and `inject:into:`.

use super::super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
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
    ) -> Result<()> {
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
    ) -> Result<()> {
        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // Generate: let List = <receiver> in let Body = <body> in
        //           let State1 = lists:foldl(
        //               fun (Item, StateAcc) -> <body with state threading> end,
        //               State, List)
        //           in <continue with State1>

        // Generate: let List = <receiver> in
        //           let Lambda = fun (Item, StateAcc) -> body in
        //           let State{n} = call 'lists':'foldl'(Lambda, initial, List) in <continuation>
        write!(self.output, "let ")?;
        let list_var = self.fresh_temp_var("temp");
        write!(self.output, "{list_var} = ")?;
        self.generate_expression(receiver)?;

        // Bind lambda to a variable (Core Erlang requires this for foldl)
        write!(self.output, " in let ")?;
        let lambda_var = self.fresh_temp_var("temp");
        write!(self.output, "{lambda_var} = fun (")?;
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        write!(self.output, "{item_var}, StateAcc) -> ")?;

        // Generate body with state threading
        self.generate_list_do_body_with_threading(body, &item_var)?;

        // Capture current state before the foldl call
        let initial_state = self.current_state_var();

        // Call foldl with the lambda variable - returns the final state
        // The caller will bind this result to the appropriate state variable
        write!(
            self.output,
            " in call 'lists':'foldl'({lambda_var}, {initial_state}, {list_var})"
        )?;

        // Note: Do NOT increment state version here - the caller does that
        // when binding the result to the state variable

        Ok(())
    }

    pub(in crate::codegen::core_erlang) fn generate_list_do_body_with_threading(
        &mut self,
        body: &Block,
        item_var: &str,
    ) -> Result<()> {
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

        // Generate the body expression(s), threading state through assignments
        // Note: generate_field_assignment_open already writes trailing " in "
        for (i, expr) in body.body.iter().enumerate() {
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                // generate_field_assignment_open increments state_version internally
                self.generate_field_assignment_open(expr)?;

                if is_last {
                    // Last expression: close with the final state variable
                    write!(self.output, "{}", self.current_state_var())?;
                }
                // Otherwise, the trailing " in " from generate_field_assignment_open
                // allows the next expression to become the body
            } else if self.is_actor_self_send(expr) {
                // BT-245: Self-sends may mutate state — thread state through dispatch
                self.generate_self_dispatch_open(expr)?;

                if is_last {
                    write!(self.output, "{}", self.current_state_var())?;
                }
            } else {
                // Non-assignment expression
                if i > 0 {
                    // Sequence with previous expression using let _ = ... in
                    write!(self.output, "let _ = ")?;
                }
                self.generate_expression(expr)?;

                if !is_last {
                    write!(self.output, " in ")?;
                }
            }
        }

        self.set_state_version(saved_state_version);
        self.in_loop_body = previous_in_loop_body;
        self.pop_scope();

        Ok(())
    }

    pub(in crate::codegen::core_erlang) fn generate_list_collect(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // list collect: is map
        self.generate_simple_list_op(receiver, body, "map")
    }

    pub(in crate::codegen::core_erlang) fn generate_list_select(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // list select: is filter
        self.generate_simple_list_op(receiver, body, "filter")
    }

    pub(in crate::codegen::core_erlang) fn generate_list_reject(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // list reject: is opposite of filter - we need to negate the predicate
        // BT-416: Add runtime is_list guard for non-list receivers
        // Generate: let List = ... in let Body = ... in
        //           case is_list(List) of
        //             true -> let Wrapper = fun(X) -> not Body(X) in lists:filter(Wrapper, List)
        //             false -> beamtalk_primitive:send(List, 'reject:', [Body])
        //           end

        write!(self.output, "let ")?;
        let list_var = self.fresh_temp_var("temp");
        write!(self.output, "{list_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in let ")?;
        let body_var = self.fresh_temp_var("temp");
        write!(self.output, "{body_var} = ")?;
        self.generate_expression(body)?;

        write!(
            self.output,
            " in case call 'erlang':'is_list'({list_var}) of \
             <'true'> when 'true' -> "
        )?;

        // Bind the negation wrapper to a variable (Core Erlang requires this)
        let wrapper_var = self.fresh_temp_var("temp");
        write!(
            self.output,
            "let {wrapper_var} = fun (X) -> call 'erlang':'not'(apply {body_var} (X)) \
             in call 'lists':'filter'({wrapper_var}, {list_var}) \
             <'false'> when 'true' -> \
             call 'beamtalk_primitive':'send'({list_var}, 'reject:', [{body_var}]) end"
        )?;

        Ok(())
    }

    pub(in crate::codegen::core_erlang) fn generate_simple_list_op(
        &mut self,
        receiver: &Expression,
        body: &Expression,
        operation: &str,
    ) -> Result<()> {
        // BT-416: Map Erlang list operation to Beamtalk selector for runtime fallback
        let selector = match operation {
            "foreach" => "do:",
            "map" => "collect:",
            "filter" => "select:",
            _ => operation,
        };

        // Generate: let Recv = <receiver> in let Body = <body> in
        //           case call 'erlang':'is_list'(Recv) of
        //             <'true'>  -> call 'lists':<operation>(Body, Recv)
        //             <'false'> -> call 'beamtalk_primitive':'send'(Recv, '<selector>', [Body])
        //           end

        write!(self.output, "let ")?;
        let list_var = self.fresh_temp_var("temp");
        write!(self.output, "{list_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in let ")?;
        let body_var = self.fresh_temp_var("temp");
        write!(self.output, "{body_var} = ")?;
        self.generate_expression(body)?;

        write!(
            self.output,
            " in case call 'erlang':'is_list'({list_var}) of \
             <'true'> when 'true' -> \
             call 'lists':'{operation}'({body_var}, {list_var}) \
             <'false'> when 'true' -> \
             call 'beamtalk_primitive':'send'({list_var}, '{selector}', [{body_var}]) end"
        )?;

        Ok(())
    }

    pub(in crate::codegen::core_erlang) fn generate_list_inject(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Analyze block for mutations
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_inject_with_mutations(receiver, initial, body_block);
            }
        }

        // Simple case: no mutations, use standard lists:foldl
        // Generate: let List = <receiver> in let Init = <initial> in let Body = <body> in
        //           call 'lists':'foldl'(Body, Init, List)

        write!(self.output, "let ")?;
        let list_var = self.fresh_temp_var("temp");
        write!(self.output, "{list_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in let ")?;
        let init_var = self.fresh_temp_var("temp");
        write!(self.output, "{init_var} = ")?;
        self.generate_expression(initial)?;

        write!(self.output, " in let ")?;
        let body_var = self.fresh_temp_var("temp");
        write!(self.output, "{body_var} = ")?;
        self.generate_expression(body)?;

        write!(
            self.output,
            " in call 'lists':'foldl'({body_var}, {init_var}, {list_var})"
        )?;

        Ok(())
    }

    pub(in crate::codegen::core_erlang) fn generate_list_inject_with_mutations(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Block,
    ) -> Result<()> {
        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // Generate: let List = <receiver> in let Init = <initial> in
        //           let State1 = lists:foldl(
        //               fun (Item, Accumulator, StateAcc) ->
        //                   <body with state threading, returns {NewAcc, NewState}>
        //               end,
        //               {Init, State}, List)
        //           in let {FinalAcc, State2} = State1 in <continue with FinalAcc, State2>

        write!(self.output, "let ")?;
        let list_var = self.fresh_temp_var("temp");
        write!(self.output, "{list_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in let ")?;
        let init_var = self.fresh_temp_var("temp");
        write!(self.output, "{init_var} = ")?;
        self.generate_expression(initial)?;

        // Bind lambda to a variable (Core Erlang requires this for foldl)
        write!(self.output, " in let ")?;
        let lambda_var = self.fresh_temp_var("temp");
        write!(
            self.output,
            "{lambda_var} = fun (Item, {{Acc, StateAcc}}) -> "
        )?;

        // Generate body with state threading
        self.generate_list_inject_body_with_threading(body)?;

        // Call foldl with the lambda variable
        write!(self.output, " in let ")?;
        // Capture current state BEFORE incrementing for the foldl result
        let initial_state = self.current_state_var();
        let result_var = self.fresh_temp_var("temp");
        write!(
            self.output,
            "{result_var} = call 'lists':'foldl'({lambda_var}, {{{init_var}, {initial_state}}}, {list_var})"
        )?;

        // Unpack result
        write!(self.output, " in let ")?;
        let acc_var = self.fresh_temp_var("temp");
        let new_state = self.next_state_var();
        write!(
            self.output,
            "{{{acc_var}, {new_state}}} = {result_var} in {acc_var}"
        )?;

        Ok(())
    }

    pub(in crate::codegen::core_erlang) fn generate_list_inject_body_with_threading(
        &mut self,
        body: &Block,
    ) -> Result<()> {
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

        // Generate the body expression(s), threading state through assignments
        // Note: generate_field_assignment_open already writes trailing " in "
        let mut has_mutations = false;
        for (i, expr) in body.body.iter().enumerate() {
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                has_mutations = true;
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                // generate_field_assignment_open increments state_version internally
                self.generate_field_assignment_open(expr)?;

                if is_last {
                    // LIMITATION: If the last expression in inject:into: is a field assignment,
                    // we return the assigned value (_Val) as the accumulator, not a computed value.
                    // This is a degenerate case - idiomatic inject:into: blocks should compute
                    // and return an accumulator value, with field assignments being side effects.
                    //
                    // Example degenerate case:
                    //   items inject: 0 into: [:acc :item | self.field := acc + item]
                    //   => Returns the last assigned value, not the accumulator
                    //
                    // Correct usage:
                    //   items inject: 0 into: [:acc :item |
                    //     self.count := self.count + 1.
                    //     acc + item
                    //   ]
                    //   => Returns the accumulator (acc + item), field mutation is side effect
                    let final_state = self.current_state_var();
                    write!(self.output, "{{_Val, {final_state}}}")?;
                } else {
                    // Not the last expression - the trailing " in " allows the next expression
                }
            } else if self.is_actor_self_send(expr) {
                has_mutations = true;
                // BT-245: Self-sends may mutate state — thread state through dispatch
                self.generate_self_dispatch_open(expr)?;

                if is_last {
                    let final_state = self.current_state_var();
                    write!(self.output, "{{'nil', {final_state}}}")?;
                }
            } else {
                // Non-assignment expression
                if i > 0 && !has_mutations {
                    // Previous expression was not a field assignment, so we need to sequence
                    write!(self.output, "let _ = ")?;
                }

                if is_last {
                    // Last expression: capture its value as the new accumulator
                    let acc_var = self.fresh_temp_var("AccOut");
                    write!(self.output, "let {acc_var} = ")?;
                    self.generate_expression(expr)?;

                    // Return {NewAcc, NewState}
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    write!(self.output, " in {{{acc_var}, {final_state}}}")?;
                } else {
                    self.generate_expression(expr)?;
                    write!(self.output, " in ")?;
                }
            }
        }

        self.set_state_version(saved_state_version);
        self.in_loop_body = previous_in_loop_body;
        self.pop_scope();

        Ok(())
    }
}
