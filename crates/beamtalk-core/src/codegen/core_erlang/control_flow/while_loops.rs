// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! While loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for `whileTrue:` and `whileFalse:` loop constructs
//! with both pure and state-threading variants.

use super::super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
use std::fmt::Write;

impl CoreErlangGenerator {
    pub(in crate::codegen::core_erlang) fn generate_while_true(
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

    pub(in crate::codegen::core_erlang) fn generate_while_true_simple(
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

    pub(in crate::codegen::core_erlang) fn generate_while_true_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<()> {
        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // Analyze which variables are mutated
        // BT-153: Mutated variables are derived from field_writes for REPL context
        let analysis = block_analysis::analyze_block(body);
        let mut mutated_vars: Vec<_> = analysis.field_writes.into_iter().collect();
        mutated_vars.sort();

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

        // Save and override loop/body context while generating the condition
        let prev_in_loop_body = self.in_loop_body;
        let prev_state_version = self.state_version();
        self.in_loop_body = true; // Enable StateAcc lookup for condition
        self.set_state_version(0); // Ensure we refer to plain StateAcc inside the fun

        // Extract block body from condition expression
        if let Expression::Block(cond_block) = condition {
            self.generate_block_body(cond_block)?;
        } else {
            // Fallback: generate as expression (shouldn't happen for whileTrue:)
            self.generate_expression(condition)?;
        }

        // Restore prior context so nested loops/conditions behave correctly
        self.in_loop_body = prev_in_loop_body;
        self.set_state_version(prev_state_version);
        write!(self.output, " in case apply {cond_var} (StateAcc) of ")?;

        // True case: execute body and recurse
        write!(self.output, "<'true'> when 'true' -> ")?;
        let final_state_version = self.generate_while_body_with_threading(body, &mutated_vars)?;
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };
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
        write!(self.output, "{final_state_var}) ")?;

        // False case: return final state
        write!(self.output, "<'false'> when 'true' -> StateAcc ")?;
        write!(self.output, "end ")?;

        // Initial call - the caller will bind the result to a state variable
        let prev_state = self.current_state_var();
        write!(self.output, "in apply 'while'/{arity} (")?;
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

        Ok(())
    }

    pub(in crate::codegen::core_erlang) fn generate_while_body_with_threading(
        &mut self,
        body: &Block,
        _mutated_vars: &[String],
    ) -> Result<usize> {
        // Generate the body with state threading
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        // BT-153: Mark that we're in a loop body so identifier lookup uses StateAcc
        // Save the previous loop-body context so nested loops don't corrupt it
        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                write!(self.output, " ")?;
            }

            if Self::is_field_assignment(expr) {
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                // Field assignment handles state version internally
                // (removed - generate_field_assignment_open does this)
                self.generate_field_assignment_open(expr)?;
                // Note: generate_field_assignment_open already writes trailing " in "
            } else if self.is_actor_self_send(expr) {
                // BT-245: Self-sends may mutate state — thread state through dispatch
                self.generate_self_dispatch_open(expr)?;
            } else if Self::is_local_var_assignment(expr) {
            } else {
                write!(self.output, "let _ = ")?;
                self.generate_expression(expr)?;
                write!(self.output, " in")?;
            }
        }

        // Capture the final state version before restoring
        let final_state_version = self.state_version();

        // Restore state
        self.in_loop_body = previous_in_loop_body;
        self.set_state_version(saved_state_version);
        Ok(final_state_version)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_false(
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

    pub(in crate::codegen::core_erlang) fn generate_while_false_simple(
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

    pub(in crate::codegen::core_erlang) fn generate_while_false_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<()> {
        // BT-245: Signal to REPL codegen that this loop mutates bindings
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        // Same as while_true but with false/true swapped in the case
        // BT-153: Mutated variables are derived from field_writes for REPL context
        let analysis = block_analysis::analyze_block(body);
        let mut mutated_vars: Vec<_> = analysis.field_writes.into_iter().collect();
        mutated_vars.sort();

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

        // Save and override loop/body context while generating the condition
        let prev_in_loop_body = self.in_loop_body;
        let prev_state_version = self.state_version();
        self.in_loop_body = true; // Enable StateAcc lookup for condition
        self.set_state_version(0); // Ensure we refer to plain StateAcc inside the fun

        if let Expression::Block(cond_block) = condition {
            self.generate_block_body(cond_block)?;
        } else {
            self.generate_expression(condition)?;
        }

        // Restore prior context so nested loops/conditions behave correctly
        self.in_loop_body = prev_in_loop_body;
        self.set_state_version(prev_state_version);
        write!(self.output, " in case apply {cond_var} (StateAcc) of ")?;

        // FALSE case: execute body and recurse
        write!(self.output, "<'false'> when 'true' -> ")?;
        let final_state_version = self.generate_while_body_with_threading(body, &mutated_vars)?;
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };
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
        write!(self.output, "{final_state_var}) ")?;

        // TRUE case: return final state
        write!(self.output, "<'true'> when 'true' -> StateAcc ")?;
        write!(self.output, "end ")?;

        // Initial call - the caller will bind the result to a state variable
        let prev_state = self.current_state_var();
        write!(self.output, "in apply 'while'/{arity} (")?;
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

        Ok(())
    }
}
