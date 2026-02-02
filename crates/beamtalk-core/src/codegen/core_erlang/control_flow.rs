// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Control flow compilation with state mutation analysis.
//!
//! This module handles the compilation of iteration and loop constructs that may
//! mutate actor state. Each construct follows a consistent pattern:
//!
//! 1. **Pure variant**: No state mutations detected, uses simple functional style
//! 2. **Stateful variant**: Mutations detected, requires state threading
//!
//! # Supported Constructs
//!
//! - **List iteration**: `do:`, `collect:`, `select:`, `reject:`, `inject:into:`
//! - **While loops**: `whileTrue:`, `whileFalse:`
//! - **Counted loops**: `repeat`, `timesRepeat:`, `to:do:`
//!
//! # State Threading
//!
//! When mutations are detected (via [`block_analysis`]), the compiler generates
//! code that threads state variables through the loop body:
//!
//! ```erlang
//! let State1 = <mutate field A in State> in
//! let State2 = <mutate field B in State1> in
//! ...
//! ```

use super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates code for `list do:` iteration.
    ///
    /// Analyzes the body block for state mutations and chooses the appropriate
    /// compilation strategy (pure functional vs stateful with threading).
    pub(super) fn generate_list_do(
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

    pub(super) fn generate_list_do_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<()> {
        // Generate: let List = <receiver> in let Body = <body> in
        //           let State1 = lists:foldl(
        //               fun (Item, StateAcc) -> <body with state threading> end,
        //               State, List)
        //           in <continue with State1>

        write!(self.output, "let ")?;
        let list_var = self.fresh_temp_var("temp");
        write!(self.output, "{list_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in let ")?;
        let body_var = self.fresh_temp_var("temp");
        write!(self.output, "{body_var} = ")?;
        self.generate_expression(&Expression::Block(body.clone()))?;

        // Use foldl to thread state through list iteration
        write!(self.output, " in let ")?;
        let new_state = self.next_state_var();
        write!(self.output, "{new_state} = call 'lists':'foldl'(")?;

        // Generate lambda: fun (Item, StateAcc) -> ... end
        write!(self.output, "fun (")?;
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        write!(self.output, "{item_var}, StateAcc) -> ")?;

        // Generate body with state threading
        self.generate_list_do_body_with_threading(body, &item_var)?;

        write!(self.output, ", {}, {list_var})", self.current_state_var())?;

        Ok(())
    }

    pub(super) fn generate_list_do_body_with_threading(
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

        // Generate the body expression(s), threading state through assignments
        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                write!(self.output, " in ")?;
            }

            if Self::is_field_assignment(expr) {
                // Field assignment - will generate: let StateAcc1 = ... in
                write!(self.output, "let ")?;
                let next_state = format!("StateAcc{}", self.state_version() + 1);
                self.set_state_version(self.state_version() + 1);
                self.generate_field_assignment_open(expr)?;
                write!(self.output, " in {next_state}")?;
            } else {
                self.generate_expression(expr)?;
            }
        }

        self.set_state_version(saved_state_version);
        self.pop_scope();

        Ok(())
    }

    pub(super) fn generate_list_collect(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // list collect: is map
        self.generate_simple_list_op(receiver, body, "map")
    }

    pub(super) fn generate_list_select(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // list select: is filter
        self.generate_simple_list_op(receiver, body, "filter")
    }

    pub(super) fn generate_list_reject(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // list reject: is opposite of filter - we need to negate the predicate
        // Generate: lists:filter(fun(X) -> not (Body(X)) end, List)

        write!(self.output, "let ")?;
        let list_var = self.fresh_temp_var("temp");
        write!(self.output, "{list_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in let ")?;
        let body_var = self.fresh_temp_var("temp");
        write!(self.output, "{body_var} = ")?;
        self.generate_expression(body)?;

        write!(self.output, " in call 'lists':'filter'(")?;
        write!(
            self.output,
            "fun (X) -> call 'erlang':'not'(apply {body_var} (X)) end"
        )?;
        write!(self.output, ", {list_var})")?;

        Ok(())
    }

    pub(super) fn generate_simple_list_op(
        &mut self,
        receiver: &Expression,
        body: &Expression,
        operation: &str,
    ) -> Result<()> {
        // Generate: let List = <receiver> in let Body = <body> in
        //           call 'lists':<operation>(Body, List)

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
            " in call 'lists':'{operation}'({body_var}, {list_var})"
        )?;

        Ok(())
    }

    pub(super) fn generate_list_inject(
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

    pub(super) fn generate_list_inject_with_mutations(
        &mut self,
        receiver: &Expression,
        initial: &Expression,
        body: &Block,
    ) -> Result<()> {
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

        write!(self.output, " in let ")?;
        let result_var = self.fresh_temp_var("temp");
        write!(self.output, "{result_var} = call 'lists':'foldl'(")?;

        // Generate lambda: fun (Item, {Acc, StateAcc}) -> ... end
        write!(self.output, "fun (Item, {{Acc, StateAcc}}) -> ")?;

        // Generate body with state threading
        self.generate_list_inject_body_with_threading(body)?;

        write!(
            self.output,
            ", {{{init_var}, {}}}, {list_var})",
            self.current_state_var()
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

    pub(super) fn generate_list_inject_body_with_threading(&mut self, body: &Block) -> Result<()> {
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

        // Generate the body expression(s), threading state through assignments
        let mut has_mutations = false;
        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                write!(self.output, " in ")?;
            }

            if Self::is_field_assignment(expr) {
                has_mutations = true;
                write!(self.output, "let ")?;
                let next_state = format!("StateAcc{}", self.state_version() + 1);
                self.set_state_version(self.state_version() + 1);
                self.generate_field_assignment_open(expr)?;
                write!(self.output, " in {next_state}")?;
            } else {
                self.generate_expression(expr)?;
            }
        }

        // Return {NewAcc, NewState}
        if has_mutations {
            let final_state = format!("StateAcc{}", self.state_version());
            // The last expression is the new accumulator value
            write!(self.output, " in {{<last expression>, {final_state}}}")?;
        } else {
            write!(self.output, " in {{<last expression>, StateAcc}}")?;
        }

        self.set_state_version(saved_state_version);
        self.pop_scope();

        Ok(())
    }

    pub(super) fn generate_while_true(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_while_true_with_mutations(condition, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_while_true_simple(condition, body)
    }

    pub(super) fn generate_while_true_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Generate a recursive function:
        // letrec '_LoopN'/0 = fun () ->
        //     let _CondFun = <condition> in
        //     case apply _CondFun& () of
        //       'true' -> let _BodyFun = <body> in
        //                let _ = apply _BodyFun& () in apply '_LoopN'/0 ()
        //       'false' -> 'nil'
        //     end
        // in apply '_LoopN'/0 ()

        let loop_fn = self.fresh_temp_var("Loop");
        write!(self.output, "letrec '{loop_fn}'/0 = fun () -> ")?;

        // Bind condition block to a variable
        let cond_var = self.fresh_temp_var("CondFun");
        write!(self.output, "let {cond_var} = ")?;
        self.generate_expression(condition)?;
        write!(self.output, " in ")?;

        write!(self.output, "case apply {cond_var} () of ")?;
        write!(self.output, "<'true'> when 'true' -> ")?;

        // Bind body block to a variable
        let body_var = self.fresh_temp_var("BodyFun");
        write!(self.output, "let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(
            self.output,
            " in let _ = apply {body_var} () in apply '{loop_fn}'/0 () "
        )?;

        write!(self.output, "<'false'> when 'true' -> 'nil' ")?;
        write!(self.output, "end ")?;
        write!(self.output, "in apply '{loop_fn}'/0 ()")?;

        Ok(())
    }

    pub(super) fn generate_while_true_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<()> {
        // Analyze which variables are mutated
        // BT-153: Include both field_writes AND local_writes for REPL context
        let analysis = block_analysis::analyze_block(body);
        let mutated_vars: Vec<_> = analysis.field_writes.into_iter().collect();

        // Generate: letrec 'while'/N = fun (Var1, Var2, ..., StateAcc) ->
        //     let _CondFun = <condition> in
        //     case apply _CondFun () of
        //       'true' -> <body with state threading>
        //                 apply 'while'/N (Var1, Var2, ..., NewState)
        //       'false' -> StateAcc
        //     end
        // in apply 'while'/N (InitVar1, InitVar2, ..., State)

        let arity = mutated_vars.len() + 1; // +1 for StateAcc
        write!(self.output, "letrec 'while'/{arity} = fun (")?;

        // Generate parameters for mutated variables
        let mut param_names = Vec::new();
        for (i, var) in mutated_vars.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            let param = Self::to_core_erlang_var(var);
            write!(self.output, "{param}")?;
            param_names.push(param);
        }
        if !mutated_vars.is_empty() {
            write!(self.output, ", ")?;
        }
        write!(self.output, "StateAcc) -> ")?;

        // Generate condition check - bind block to variable and apply it
        // BT-181: Condition needs to read from StateAcc, not outer State
        // Generate: let _CondFun = fun (StateAcc) -> <condition body> in case apply _CondFun (StateAcc) of
        let cond_var = self.fresh_temp_var("CondFun");
        write!(self.output, "let {cond_var} = fun (StateAcc) -> ")?;
        self.in_loop_body = true; // Enable StateAcc lookup for condition
        // Extract block body from condition expression
        if let Expression::Block(cond_block) = condition {
            self.generate_block_body(cond_block)?;
        } else {
            // Fallback: generate as expression (shouldn't happen for whileTrue:)
            self.generate_expression(condition)?;
        }
        self.in_loop_body = false;
        write!(self.output, " in case apply {cond_var} (StateAcc) of ")?;

        // True case: execute body and recurse
        write!(self.output, "<'true'> when 'true' -> ")?;
        self.generate_while_body_with_threading(body, &mutated_vars)?;
        write!(self.output, " apply 'while'/{arity} (")?;
        for (i, param) in param_names.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "{param}1")?; // Updated variables
        }
        if !param_names.is_empty() {
            write!(self.output, ", ")?;
        }
        write!(self.output, "StateAcc1) ")?;

        // False case: return final state
        write!(self.output, "<'false'> when 'true' -> StateAcc ")?;
        write!(self.output, "end ")?;

        // Initial call - capture current state BEFORE incrementing
        let prev_state = self.current_state_var();
        write!(self.output, "in let ")?;
        let new_state = self.next_state_var();
        write!(self.output, "{new_state} = apply 'while'/{arity} (")?;
        for (i, var) in mutated_vars.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "call 'maps':'get'('{var}', {prev_state})",)?;
        }
        if !mutated_vars.is_empty() {
            write!(self.output, ", ")?;
        }
        write!(self.output, "{prev_state})")?;

        // Close the let by returning the new state (BT-153)
        // This makes the expression a valid value in REPL context
        write!(self.output, " in {new_state}")?;

        Ok(())
    }

    pub(super) fn generate_while_body_with_threading(
        &mut self,
        body: &Block,
        _mutated_vars: &[String],
    ) -> Result<()> {
        // Generate the body with state threading
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // BT-153: Mark that we're in a loop body so identifier lookup uses StateAcc
        self.in_loop_body = true;

        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                write!(self.output, " ")?;
            }

            if Self::is_field_assignment(expr) {
                write!(self.output, "let ")?;
                let _next_state = format!("StateAcc{}", self.state_version() + 1);
                self.set_state_version(self.state_version() + 1);
                self.generate_field_assignment_open(expr)?;
                write!(self.output, " in")?;
            } else if Self::is_local_var_assignment(expr) {
                // BT-153: Handle local variable assignments for REPL context
                self.generate_local_var_assignment_in_loop(expr)?;
            } else {
                write!(self.output, "let _ = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in")?;
            }
        }

        // Restore state
        self.in_loop_body = false;
        self.set_state_version(saved_state_version);
        Ok(())
    }

    pub(super) fn generate_while_false(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_while_false_with_mutations(condition, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_while_false_simple(condition, body)
    }

    pub(super) fn generate_while_false_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Generate: whileFalse is whileTrue with negated condition
        let loop_fn = self.fresh_temp_var("Loop");
        write!(self.output, "letrec '{loop_fn}'/0 = fun () -> ")?;

        let cond_var = self.fresh_temp_var("CondFun");
        write!(self.output, "let {cond_var} = ")?;
        self.generate_expression(condition)?;
        write!(self.output, " in ")?;

        write!(self.output, "case apply {cond_var} () of ")?;
        write!(self.output, "<'false'> when 'true' -> ")?;

        let body_var = self.fresh_temp_var("BodyFun");
        write!(self.output, "let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(
            self.output,
            " in let _ = apply {body_var} () in apply '{loop_fn}'/0 () "
        )?;

        write!(self.output, "<'true'> when 'true' -> 'nil' ")?;
        write!(self.output, "end ")?;
        write!(self.output, "in apply '{loop_fn}'/0 ()")?;

        Ok(())
    }

    pub(super) fn generate_while_false_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<()> {
        // Same as while_true but with false/true swapped in the case
        // BT-153: Include both field_writes AND local_writes for REPL context
        let analysis = block_analysis::analyze_block(body);
        let mutated_vars: Vec<_> = analysis.field_writes.into_iter().collect();

        let arity = mutated_vars.len() + 1;
        write!(self.output, "letrec 'while'/{arity} = fun (")?;

        let mut param_names = Vec::new();
        for (i, var) in mutated_vars.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            let param = Self::to_core_erlang_var(var);
            write!(self.output, "{param}")?;
            param_names.push(param);
        }
        if !mutated_vars.is_empty() {
            write!(self.output, ", ")?;
        }
        write!(self.output, "StateAcc) -> ")?;

        // Generate condition check - bind block to variable and apply it
        // BT-181: Condition needs to read from StateAcc, not outer State
        let cond_var = self.fresh_temp_var("CondFun");
        write!(self.output, "let {cond_var} = fun (StateAcc) -> ")?;
        self.in_loop_body = true; // Enable StateAcc lookup for condition
        if let Expression::Block(cond_block) = condition {
            self.generate_block_body(cond_block)?;
        } else {
            self.generate_expression(condition)?;
        }
        self.in_loop_body = false;
        write!(self.output, " in case apply {cond_var} (StateAcc) of ")?;

        // FALSE case: execute body and recurse
        write!(self.output, "<'false'> when 'true' -> ")?;
        self.generate_while_body_with_threading(body, &mutated_vars)?;
        write!(self.output, " apply 'while'/{arity} (")?;
        for (i, param) in param_names.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "{param}1")?;
        }
        if !param_names.is_empty() {
            write!(self.output, ", ")?;
        }
        write!(self.output, "StateAcc1) ")?;

        // TRUE case: return final state
        write!(self.output, "<'true'> when 'true' -> StateAcc ")?;
        write!(self.output, "end ")?;

        // Initial call - capture current state BEFORE incrementing
        let prev_state = self.current_state_var();
        write!(self.output, "in let ")?;
        let new_state = self.next_state_var();
        write!(self.output, "{new_state} = apply 'while'/{arity} (")?;
        for (i, var) in mutated_vars.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            write!(self.output, "call 'maps':'get'('{var}', {prev_state})",)?;
        }
        if !mutated_vars.is_empty() {
            write!(self.output, ", ")?;
        }
        write!(self.output, "{prev_state})")?;

        // Close the let by returning the new state (BT-153)
        write!(self.output, " in {new_state}")?;

        Ok(())
    }

    pub(super) fn generate_repeat(&mut self, body: &Expression) -> Result<()> {
        // repeat: infinite loop - execute body forever
        // Generate: letrec '_LoopN'/0 = fun () ->
        //     let _BodyFun = <body> in
        //     let _ = apply _BodyFun& () in apply '_LoopN'/0 ()
        // in apply '_LoopN'/0 ()

        let loop_fn = self.fresh_temp_var("Loop");
        write!(self.output, "letrec '{loop_fn}'/0 = fun () -> ")?;

        let body_var = self.fresh_temp_var("BodyFun");
        write!(self.output, "let {body_var} = ")?;
        self.generate_expression(body)?;
        write!(
            self.output,
            " in let _ = apply {body_var} () in apply '{loop_fn}'/0 () "
        )?;

        write!(self.output, "in apply '{loop_fn}'/0 ()")?;

        Ok(())
    }

    pub(super) fn generate_times_repeat(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<()> {
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

    pub(super) fn generate_times_repeat_simple(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Generate: let N = <receiver> in
        //           letrec 'repeat'/1 = fun (I) ->
        //               case call 'erlang':'=<'(I, N) of
        //                 'true' -> let _ = <body> in apply 'repeat'/1 (I+1)
        //                 'false' -> 'nil'
        //               end
        //           in apply 'repeat'/1 (1)

        write!(self.output, "let ")?;
        let n_var = self.fresh_temp_var("temp");
        write!(self.output, "{n_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in letrec 'repeat'/1 = fun (I) -> ")?;
        write!(self.output, "case call 'erlang':'=<'(I, {n_var}) of ")?;
        write!(self.output, "<'true'> when 'true' -> ")?;
        write!(self.output, "let _ = ")?;
        self.generate_expression(body)?;
        write!(
            self.output,
            " in apply 'repeat'/1 (call 'erlang':'+'(I, 1)) "
        )?;
        write!(self.output, "<'false'> when 'true' -> 'nil' ")?;
        write!(self.output, "end ")?;
        write!(self.output, "in apply 'repeat'/1 (1)")?;

        Ok(())
    }

    pub(super) fn generate_times_repeat_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<()> {
        // Analyze which variables are mutated
        // BT-153: Only include field_writes for loop parameters (actor state),
        // local_writes are handled by updating StateAcc in the body via maps:put
        let analysis = block_analysis::analyze_block(body);
        let mutated_vars: Vec<_> = analysis.field_writes.into_iter().collect();

        // Generate: let N = <receiver> in
        //           letrec 'repeat'/N+1 = fun (I, Var1, Var2, ..., StateAcc) ->
        //               case I =< N of
        //                 'true' -> <body with threading>
        //                          apply 'repeat'/N+1 (I+1, Var1', Var2', ..., StateAcc')
        //                 'false' -> StateAcc
        //               end
        //           in apply 'repeat'/N+1 (1, InitVar1, InitVar2, ..., State)

        write!(self.output, "let ")?;
        let n_var = self.fresh_temp_var("temp");
        write!(self.output, "{n_var} = ")?;
        self.generate_expression(receiver)?;

        let arity = mutated_vars.len() + 2; // +1 for I, +1 for StateAcc
        write!(self.output, " in letrec 'repeat'/{arity} = fun (I")?;

        let mut param_names = Vec::new();
        for var in &mutated_vars {
            let param = Self::to_core_erlang_var(var);
            write!(self.output, ", {param}")?;
            param_names.push(param);
        }
        write!(self.output, ", StateAcc) -> ")?;

        write!(self.output, "case call 'erlang':'=<'(I, {n_var}) of ")?;
        write!(self.output, "<'true'> when 'true' -> ")?;

        self.generate_times_repeat_body_with_threading(body, &mutated_vars)?;

        write!(
            self.output,
            " apply 'repeat'/{arity} (call 'erlang':'+'(I, 1)"
        )?;
        for param in &param_names {
            write!(self.output, ", {param}1")?;
        }
        write!(self.output, ", StateAcc1) ")?;

        write!(self.output, "<'false'> when 'true' -> StateAcc ")?;
        write!(self.output, "end ")?;

        // Initial call - capture current state BEFORE incrementing
        let prev_state = self.current_state_var();
        write!(self.output, "in let ")?;
        let new_state = self.next_state_var();
        write!(self.output, "{new_state} = apply 'repeat'/{arity} (1")?;
        for var in &mutated_vars {
            write!(self.output, ", call 'maps':'get'('{var}', {prev_state})",)?;
        }
        write!(self.output, ", {prev_state})")?;

        // Close the let by returning the new state (BT-153)
        write!(self.output, " in {new_state}")?;

        Ok(())
    }

    pub(super) fn generate_times_repeat_body_with_threading(
        &mut self,
        body: &Block,
        _mutated_vars: &[String],
    ) -> Result<()> {
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // BT-153: Mark that we're in a loop body so identifier lookup uses StateAcc
        self.in_loop_body = true;

        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                write!(self.output, " ")?;
            }

            if Self::is_field_assignment(expr) {
                write!(self.output, "let ")?;
                let _next_state = format!("StateAcc{}", self.state_version() + 1);
                self.set_state_version(self.state_version() + 1);
                self.generate_field_assignment_open(expr)?;
                write!(self.output, " in")?;
            } else if Self::is_local_var_assignment(expr) {
                // BT-153: Handle local variable assignments for REPL context
                // Generate: let _Val = <value> in let StateAccN = maps:put('var', _Val, StateAcc{N-1}) in
                self.generate_local_var_assignment_in_loop(expr)?;
            } else {
                write!(self.output, "let _ = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in")?;
            }
        }

        // Restore state
        self.in_loop_body = false;
        self.set_state_version(saved_state_version);
        Ok(())
    }

    /// Generate a local variable assignment inside a loop body with state threading (BT-153).
    ///
    /// Generates code like:
    /// ```erlang
    /// let _Val = <value> in let StateAccN = maps:put('varname', _Val, StateAcc{N-1}) in
    /// ```
    fn generate_local_var_assignment_in_loop(&mut self, expr: &Expression) -> Result<()> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");
                let current_state = if self.state_version() == 0 {
                    "StateAcc".to_string()
                } else {
                    format!("StateAcc{}", self.state_version())
                };

                // let _Val = <value> in
                write!(self.output, "let {val_var} = ")?;
                self.generate_expression(value)?;

                // Increment state version
                self.set_state_version(self.state_version() + 1);
                let new_state = format!("StateAcc{}", self.state_version());

                // let StateAccN = call 'maps':'put'('varname', _Val, StateAcc{N-1}) in
                write!(
                    self.output,
                    " in let {new_state} = call 'maps':'put'('{}', {val_var}, {current_state}) in",
                    id.name
                )?;
            }
        }
        Ok(())
    }

    pub(super) fn generate_to_do(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        body: &Expression,
    ) -> Result<()> {
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

    pub(super) fn generate_to_do_simple(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        body: &Expression,
    ) -> Result<()> {
        // Generate: let Start = <receiver> in let End = <limit> in
        //           letrec 'loop'/1 = fun (I) ->
        //               case I =< End of
        //                 'true' -> let _ = apply <body>/1 (I) in
        //                          apply 'loop'/1 (I+1)
        //                 'false' -> 'nil'
        //               end
        //           in apply 'loop'/1 (Start)

        write!(self.output, "let ")?;
        let start_var = self.fresh_temp_var("temp");
        write!(self.output, "{start_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in let ")?;
        let end_var = self.fresh_temp_var("temp");
        write!(self.output, "{end_var} = ")?;
        self.generate_expression(limit)?;

        write!(self.output, " in let ")?;
        let body_var = self.fresh_temp_var("temp");
        write!(self.output, "{body_var} = ")?;
        self.generate_expression(body)?;

        write!(self.output, " in letrec 'loop'/1 = fun (I) -> ")?;
        write!(self.output, "case call 'erlang':'=<'(I, {end_var}) of ")?;
        write!(self.output, "<'true'> when 'true' -> ")?;
        write!(self.output, "let _ = apply {body_var} (I) in ")?;
        write!(self.output, "apply 'loop'/1 (call 'erlang':'+'(I, 1)) ")?;
        write!(self.output, "<'false'> when 'true' -> 'nil' ")?;
        write!(self.output, "end ")?;
        write!(self.output, "in apply 'loop'/1 ({start_var})")?;

        Ok(())
    }

    pub(super) fn generate_to_do_with_mutations(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        body: &Block,
    ) -> Result<()> {
        // Analyze which variables are mutated
        // BT-153: Include both field_writes AND local_writes for REPL context
        let analysis = block_analysis::analyze_block(body);
        let mutated_vars: Vec<_> = analysis.field_writes.into_iter().collect();

        // Generate: let Start = <receiver> in let End = <limit> in
        //           letrec 'loop'/N+2 = fun (I, Var1, Var2, ..., StateAcc) ->
        //               case I =< End of
        //                 'true' -> <body with I parameter and state threading>
        //                          apply 'loop'/N+2 (I+1, Var1', Var2', ..., StateAcc')
        //                 'false' -> StateAcc
        //               end
        //           in apply 'loop'/N+2 (Start, InitVar1, InitVar2, ..., State)

        write!(self.output, "let ")?;
        let start_var = self.fresh_temp_var("temp");
        write!(self.output, "{start_var} = ")?;
        self.generate_expression(receiver)?;

        write!(self.output, " in let ")?;
        let end_var = self.fresh_temp_var("temp");
        write!(self.output, "{end_var} = ")?;
        self.generate_expression(limit)?;

        let arity = mutated_vars.len() + 2; // +1 for I, +1 for StateAcc
        write!(self.output, " in letrec 'loop'/{arity} = fun (I")?;

        let mut param_names = Vec::new();
        for var in &mutated_vars {
            let param = Self::to_core_erlang_var(var);
            write!(self.output, ", {param}")?;
            param_names.push(param);
        }
        write!(self.output, ", StateAcc) -> ")?;

        write!(self.output, "case call 'erlang':'=<'(I, {end_var}) of ")?;
        write!(self.output, "<'true'> when 'true' -> ")?;

        self.generate_to_do_body_with_threading(body, &mutated_vars)?;

        write!(
            self.output,
            " apply 'loop'/{arity} (call 'erlang':'+'(I, 1)"
        )?;
        for param in &param_names {
            write!(self.output, ", {param}1")?;
        }
        write!(self.output, ", StateAcc1) ")?;

        write!(self.output, "<'false'> when 'true' -> StateAcc ")?;
        write!(self.output, "end ")?;

        // Initial call - capture current state BEFORE incrementing
        let prev_state = self.current_state_var();
        write!(self.output, " in let ")?;
        let new_state = self.next_state_var();
        write!(
            self.output,
            "{new_state} = apply 'loop'/{arity} ({start_var}"
        )?;
        for var in &mutated_vars {
            write!(self.output, ", call 'maps':'get'('{var}', {prev_state})",)?;
        }
        write!(self.output, ", {prev_state})")?;

        // Close the let by returning the new state (BT-153)
        write!(self.output, " in {new_state}")?;

        Ok(())
    }

    pub(super) fn generate_to_do_body_with_threading(
        &mut self,
        body: &Block,
        mutated_vars: &[String],
    ) -> Result<()> {
        // Bind the block parameter to I
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, "I");
        }

        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // BT-153: Mark that we're in a loop body so identifier lookup uses StateAcc
        self.in_loop_body = true;

        // Track which variables are assigned in the body
        let mut assigned_vars = std::collections::HashSet::new();
        for expr in &body.body {
            if Self::is_field_assignment(expr) {
                if let Expression::Assignment { target, .. } = expr {
                    if let Expression::FieldAccess { field, .. } = target.as_ref() {
                        assigned_vars.insert(field.name.as_str());
                    }
                }
            }
        }

        // Generate let bindings for mutated variables from StateAcc
        let core_vars: Vec<String> = mutated_vars
            .iter()
            .map(|v| Self::to_core_erlang_var(v))
            .collect();

        for (i, (var, core_var)) in mutated_vars.iter().zip(core_vars.iter()).enumerate() {
            if i > 0 || body.body.is_empty() {
                // Add spacing
            }
            write!(
                self.output,
                "let {core_var} = call 'maps':'get'('{var}', StateAcc) in "
            )?;
        }

        // Generate body expressions
        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                write!(self.output, " in ")?;
            }

            if Self::is_field_assignment(expr) {
                write!(self.output, "let ")?;
                let _next_state = format!("StateAcc{}", self.state_version() + 1);
                self.set_state_version(self.state_version() + 1);
                self.generate_field_assignment_open(expr)?;
                write!(self.output, " in ")?;
            } else if Self::is_local_var_assignment(expr) {
                // BT-153: Handle local variable assignments for REPL context
                self.generate_local_var_assignment_in_loop(expr)?;
                write!(self.output, " ")?;
            } else {
                write!(self.output, "let _ = ")?;
                self.generate_expression(expr)?;

                // After last expression, rebuild StateAcc with updated values
                if i == body.body.len() - 1 {
                    write!(self.output, " in let StateAcc1 = ~{{")?;

                    // Copy all fields from StateAcc
                    write!(
                        self.output,
                        "'__class__' => call 'maps':'get'('__class__', StateAcc), "
                    )?;
                    write!(
                        self.output,
                        "'__methods__' => call 'maps':'get'('__methods__', StateAcc)"
                    )?;

                    // Update mutated fields
                    for (var, core_var) in mutated_vars.iter().zip(core_vars.iter()) {
                        write!(self.output, ", ")?;
                        if assigned_vars.contains(var.as_str()) {
                            write!(self.output, "'{var}' => {core_var}1")?;
                        } else {
                            write!(self.output, "'{var}' => {core_var}")?;
                        }
                    }
                    write!(self.output, "}}~ in StateAcc1")?;
                } else {
                    write!(self.output, " in ")?;
                }
            }
        }

        // Restore state
        self.in_loop_body = false;
        self.set_state_version(saved_state_version);
        self.pop_scope();

        Ok(())
    }
}
