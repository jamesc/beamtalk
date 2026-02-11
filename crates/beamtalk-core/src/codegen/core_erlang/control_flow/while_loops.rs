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
        // BT-478: Simplified loop signature — only (StateAcc), no separate field params.

        // Generate: letrec 'while'/1 = fun (StateAcc) ->
        //     let _CondFun = <condition> in
        //     case apply _CondFun (StateAcc) of
        //       'true' -> <body with state threading>
        //                 apply 'while'/1 (StateAcc')
        //       'false' -> StateAcc
        //     end
        // in apply 'while'/1 (State)

        write!(self.output, "letrec 'while'/1 = fun (StateAcc) -> ")?;

        // Generate condition check - bind block to variable and apply it
        // BT-181: Condition needs to read from StateAcc, not outer State
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
        let final_state_version = self.generate_while_body_with_threading(body, &[])?;
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };
        write!(self.output, " apply 'while'/1 ({final_state_var}) ")?;

        // False case: return final state
        write!(self.output, "<'false'> when 'true' -> StateAcc ")?;
        write!(self.output, "end ")?;

        // Initial call
        let prev_state = self.current_state_var();
        write!(self.output, "in apply 'while'/1 ({prev_state})")?;

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

        // BT-478: Check if body has direct field assignments. If not, mutations
        // come from nested constructs and the last expression's result must be bound.
        let has_direct_field_assignments = body.body.iter().any(Self::is_field_assignment);

        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                write!(self.output, " ")?;
            }
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                // Field assignment - already writes "let _Val = ... in let StateAcc{n} = ... in "
                // Field assignment handles state version internally
                // (removed - generate_field_assignment_open does this)
                self.generate_field_assignment_open(expr)?;
                // Note: generate_field_assignment_open already writes trailing " in "
            } else if Self::is_local_var_assignment(expr) {
                // BT-153: Handle local variable assignments for REPL context
                self.generate_local_var_assignment_in_loop(expr)?;
            } else if is_last && !has_direct_field_assignments {
                // BT-478: Last expression with no direct field assignments in body.
                // Mutations come from nested constructs (e.g., inner to:do:).
                // Bind the nested construct's returned state to the next StateAcc.
                let next_version = self.state_version() + 1;
                let next_var = format!("StateAcc{next_version}");
                write!(self.output, "let {next_var} = ")?;
                self.generate_expression(expr)?;
                self.set_state_version(next_version);
                write!(self.output, " in")?;
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
        // BT-478: Simplified loop signature — only (StateAcc), no separate field params.

        write!(self.output, "letrec 'while'/1 = fun (StateAcc) -> ")?;

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
        let final_state_version = self.generate_while_body_with_threading(body, &[])?;
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };
        write!(self.output, " apply 'while'/1 ({final_state_var}) ")?;

        // TRUE case: return final state
        write!(self.output, "<'true'> when 'true' -> StateAcc ")?;
        write!(self.output, "end ")?;

        // Initial call
        let prev_state = self.current_state_var();
        write!(self.output, "in apply 'while'/1 ({prev_state})")?;

        Ok(())
    }
}
