// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Exception handling code generation (Block `on:do:` and `ensure:`).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates Core Erlang `try/catch` for `on:do:` and `try/after` for `ensure:`.
//! These are structural intrinsics because they must wrap the block execution
//! in Core Erlang exception handling constructs at compile time.
//!
//! # `on:do:` — Exception Handling (try/catch)
//!
//! ```beamtalk
//! [risky operation] on: Exception do: [:e | handle error]
//! ```
//!
//! Generates:
//! ```erlang
//! let _BlockFun = <receiver> in
//! let _ExClass = <exClass> in
//! let _HandlerFun = <handler> in
//! try apply _BlockFun ()
//! of _Result -> _Result
//! catch <_Type, _Error, _Stacktrace> ->
//!     let _ExObj = call 'beamtalk_exception_handler':'ensure_wrapped'(_Error) in
//!     case matches_class(ExClass, Error) of
//!         true  -> apply _HandlerFun (_ExObj)
//!         false -> primop 'raw_raise'(_Type, _Error, _Stacktrace)
//! ```
//!
//! # `ensure:` — Cleanup (try/after)
//!
//! ```beamtalk
//! [operation] ensure: [cleanup]
//! ```
//!
//! Generates:
//! ```erlang
//! let _BlockFun = <receiver> in
//! let _CleanupFun = <cleanup> in
//! try
//!     let _TryResult = apply _BlockFun () in _TryResult
//! of _Result -> let _ = apply _CleanupFun () in _Result
//! catch <_Type, _Error, _Stacktrace> ->
//!     do apply _CleanupFun ()
//!     primop 'raw_raise'(_Type, _Error, _Stacktrace)
//! ```

use super::super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
use crate::docvec;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates `on:do:` — wraps block in try/catch, wraps error as Exception
    /// object and passes to handler block.
    ///
    /// Analyzes both receiver and handler blocks for state mutations and chooses
    /// the appropriate compilation strategy (closure-based vs inlined with state threading).
    pub(in crate::codegen::core_erlang) fn generate_on_do(
        &mut self,
        receiver: &Expression,
        ex_class: &Expression,
        handler: &Expression,
    ) -> Result<()> {
        // BT-410: Check both blocks for field/state mutations
        let receiver_needs = if let Expression::Block(b) = receiver {
            self.needs_mutation_threading(&block_analysis::analyze_block(b))
        } else {
            false
        };
        let handler_needs = if let Expression::Block(b) = handler {
            self.needs_mutation_threading(&block_analysis::analyze_block(b))
        } else {
            false
        };

        if receiver_needs || handler_needs {
            if let (Expression::Block(recv_block), Expression::Block(handler_block)) =
                (receiver, handler)
            {
                return self.generate_on_do_with_mutations(recv_block, ex_class, handler_block);
            }
        }

        // Simple case: no mutations, use closure-based approach
        let block_var = self.fresh_temp_var("BlockFun");
        let ex_class_var = self.fresh_temp_var("ExClass");
        let handler_var = self.fresh_temp_var("HandlerFun");
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let ex_obj_var = self.fresh_temp_var("ExObj");
        let match_var = self.fresh_temp_var("Match");

        // Capture expression outputs (ADR 0018 bridge pattern)
        let receiver_code = self.capture_expression(receiver)?;
        let ex_class_code = self.capture_expression(ex_class)?;
        let handler_code = self.capture_expression(handler)?;

        let doc = docvec![
            format!("let {block_var} = "),
            receiver_code,
            format!(" in let {ex_class_var} = "),
            ex_class_code,
            format!(" in let {handler_var} = "),
            handler_code,
            format!(" in try apply {block_var} () "),
            format!("of {result_var} -> {result_var} "),
            format!(
                "catch <{type_var}, {error_var}, {stack_var}> -> \
                 let {ex_obj_var} = call 'beamtalk_exception_handler':'ensure_wrapped'({error_var}) in \
                 let {match_var} = call 'beamtalk_exception_handler':'matches_class'({ex_class_var}, {error_var}) in \
                 case {match_var} of \
                 <'true'> when 'true' -> apply {handler_var} ({ex_obj_var}) \
                 <'false'> when 'true' -> primop 'raw_raise'({type_var}, {error_var}, {stack_var}) end"
            ),
        ];

        self.write_document(&doc);
        Ok(())
    }

    /// BT-410: Generates `on:do:` with state mutation threading.
    ///
    /// Inlines receiver (try body) and handler block bodies with state threading
    /// instead of wrapping them as closures. This ensures field mutations in
    /// handler blocks are properly threaded back to the actor state.
    ///
    /// Generated Core Erlang:
    /// ```erlang
    /// let _ExClass = <ex_class> in
    /// let StateAcc = <current_state> in
    /// try
    ///     <inlined receiver body with state threading>
    ///     StateAccN
    /// of StateAfterTry -> StateAfterTry
    /// catch <Type, Error, Stack> ->
    ///     let ExObj = call 'beamtalk_exception_handler':'ensure_wrapped'(Error) in
    ///     let Match = call 'beamtalk_exception_handler':'matches_class'(ExClass, Error) in
    ///     case Match of
    ///         true  -> let _e = ExObj in <handler body with threading> StateAccM
    ///         false -> primop 'raw_raise'(Type, Error, Stack)
    /// ```
    fn generate_on_do_with_mutations(
        &mut self,
        receiver_block: &Block,
        ex_class: &Expression,
        handler_block: &Block,
    ) -> Result<()> {
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        let ex_class_var = self.fresh_temp_var("ExClass");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let ex_obj_var = self.fresh_temp_var("ExObj");
        let match_var = self.fresh_temp_var("Match");
        let state_after_try = self.fresh_temp_var("StateAfterTry");

        // Bind exception class
        write!(self.output, "let {ex_class_var} = ")?;
        self.generate_expression(ex_class)?;

        // Rename current state to StateAcc for uniform threading
        let current_state = self.current_state_var();
        write!(self.output, " in let StateAcc = {current_state} in try ")?;

        // Generate try body (receiver block) with state threading
        let try_final = self.generate_exception_body_with_threading(receiver_block)?;
        let try_final_var = if try_final == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{try_final}")
        };
        write!(self.output, " {try_final_var} ")?;

        // Success: pass state through
        write!(self.output, "of {state_after_try} -> {state_after_try} ")?;

        // Catch clause
        write!(
            self.output,
            "catch <{type_var}, {error_var}, {stack_var}> -> "
        )?;
        write!(
            self.output,
            "let {ex_obj_var} = call 'beamtalk_exception_handler':'ensure_wrapped'({error_var}) in "
        )?;
        write!(
            self.output,
            "let {match_var} = call 'beamtalk_exception_handler':'matches_class'(\
             {ex_class_var}, {error_var}) in "
        )?;
        write!(self.output, "case {match_var} of ")?;
        write!(self.output, "<'true'> when 'true' -> ")?;

        // Bind handler parameter (e.g., [:e | ...] binds e to exception object)
        self.push_scope();
        if let Some(param) = handler_block.parameters.first() {
            let param_var = Self::to_core_erlang_var(&param.name);
            self.bind_var(&param.name, &param_var);
            write!(self.output, "let {param_var} = {ex_obj_var} in ")?;
        }

        // Generate handler body with state threading (from original StateAcc)
        let handler_final = self.generate_exception_body_with_threading(handler_block)?;
        let handler_final_var = if handler_final == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{handler_final}")
        };
        write!(self.output, " {handler_final_var} ")?;
        self.pop_scope();

        // Re-raise non-matching exceptions
        write!(
            self.output,
            "<'false'> when 'true' -> \
             primop 'raw_raise'({type_var}, {error_var}, {stack_var}) "
        )?;
        write!(self.output, "end")?;

        Ok(())
    }

    /// Generates `ensure:` — wraps block in try, always runs cleanup block.
    ///
    /// Analyzes both receiver and cleanup blocks for state mutations and chooses
    /// the appropriate compilation strategy.
    pub(in crate::codegen::core_erlang) fn generate_ensure(
        &mut self,
        receiver: &Expression,
        cleanup: &Expression,
    ) -> Result<()> {
        // BT-410: Check both blocks for field/state mutations
        let receiver_needs = if let Expression::Block(b) = receiver {
            self.needs_mutation_threading(&block_analysis::analyze_block(b))
        } else {
            false
        };
        let cleanup_needs = if let Expression::Block(b) = cleanup {
            self.needs_mutation_threading(&block_analysis::analyze_block(b))
        } else {
            false
        };

        if receiver_needs || cleanup_needs {
            if let (Expression::Block(recv_block), Expression::Block(cleanup_block)) =
                (receiver, cleanup)
            {
                return self.generate_ensure_with_mutations(recv_block, cleanup_block);
            }
        }

        // Simple case: no mutations
        let block_var = self.fresh_temp_var("BlockFun");
        let cleanup_var = self.fresh_temp_var("CleanupFun");
        let try_result_var = self.fresh_temp_var("TryResult");
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");

        // Capture expression outputs (ADR 0018 bridge pattern)
        let receiver_code = self.capture_expression(receiver)?;
        let cleanup_code = self.capture_expression(cleanup)?;

        let doc = docvec![
            format!("let {block_var} = "),
            receiver_code,
            format!(" in let {cleanup_var} = "),
            cleanup_code,
            format!(" in try let {try_result_var} = apply {block_var} () in {try_result_var} "),
            format!("of {result_var} -> let _ = apply {cleanup_var} () in {result_var} "),
            format!(
                "catch <{type_var}, {error_var}, {stack_var}> -> \
                 do apply {cleanup_var} () \
                 primop 'raw_raise'({type_var}, {error_var}, {stack_var})"
            ),
        ];

        self.write_document(&doc);
        Ok(())
    }

    /// BT-410: Generates `ensure:` with state mutation threading.
    ///
    /// Inlines receiver (try body) and cleanup block bodies with state threading.
    /// On success, cleanup runs with the try body's final state.
    /// On error, cleanup runs with the original state, then re-raises.
    ///
    /// Generated Core Erlang:
    /// ```erlang
    /// let StateAcc = <current_state> in
    /// try
    ///     <inlined try body with state threading>
    ///     StateAccN
    /// of StateAfterTry ->
    ///     let StateAcc = StateAfterTry in
    ///     <inlined cleanup with state threading>
    ///     StateAccM
    /// catch <Type, Error, Stack> ->
    ///     <inlined cleanup with state threading from original StateAcc>
    ///     primop 'raw_raise'(Type, Error, Stack)
    /// ```
    fn generate_ensure_with_mutations(
        &mut self,
        receiver_block: &Block,
        cleanup_block: &Block,
    ) -> Result<()> {
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let state_after_try = self.fresh_temp_var("StateAfterTry");

        // Rename current state to StateAcc
        let current_state = self.current_state_var();
        write!(self.output, "let StateAcc = {current_state} in try ")?;

        // Generate try body with state threading
        let try_final = self.generate_exception_body_with_threading(receiver_block)?;
        let try_final_var = if try_final == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{try_final}")
        };
        write!(self.output, " {try_final_var} ")?;

        // Success: run cleanup starting from try body's state
        write!(
            self.output,
            "of {state_after_try} -> let StateAcc = {state_after_try} in "
        )?;
        let cleanup_success_final = self.generate_exception_body_with_threading(cleanup_block)?;
        let cleanup_success_var = if cleanup_success_final == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{cleanup_success_final}")
        };
        write!(self.output, " {cleanup_success_var} ")?;

        // Error: run cleanup for side effects (from original StateAcc), then re-raise
        write!(
            self.output,
            "catch <{type_var}, {error_var}, {stack_var}> -> "
        )?;
        // Cleanup body generates state mutations that are discarded (re-raise follows)
        let _cleanup_error_final = self.generate_exception_body_with_threading(cleanup_block)?;
        write!(
            self.output,
            " primop 'raw_raise'({type_var}, {error_var}, {stack_var})"
        )?;

        Ok(())
    }

    /// BT-410: Generates block body expressions with state mutation threading.
    ///
    /// Follows the same pattern as `generate_while_body_with_threading`:
    /// - Sets `in_loop_body = true` so field reads/writes use `StateAcc`
    /// - Resets `state_version` to 0 (`StateAcc` is version 0)
    /// - Threads field assignments, self-sends, and local var assignments
    /// - Returns the final state version number
    ///
    /// The caller must have already bound `StateAcc` to the current state
    /// before calling this function.
    fn generate_exception_body_with_threading(&mut self, body: &Block) -> Result<usize> {
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        let has_direct_field_assignments = body.body.iter().any(Self::is_field_assignment);

        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                write!(self.output, " ")?;
            }
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                self.generate_field_assignment_open(expr)?;
            } else if self.is_actor_self_send(expr) {
                self.generate_self_dispatch_open(expr)?;
            } else if Self::is_local_var_assignment(expr) {
                self.generate_local_var_assignment_in_loop(expr)?;
            } else if is_last && !has_direct_field_assignments {
                // Last expression with no direct field assignments in body.
                // Mutations come from nested constructs; bind result to StateAcc.
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

        let final_state_version = self.state_version();
        self.in_loop_body = previous_in_loop_body;
        self.set_state_version(saved_state_version);
        Ok(final_state_version)
    }
}
