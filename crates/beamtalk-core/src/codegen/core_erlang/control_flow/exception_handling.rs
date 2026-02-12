// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Exception handling code generation (Block `on:do:` and `ensure:`).
//!
//! **DDD Compilation Context:** Code Generation
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

use super::super::document::Document;
use super::super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
use crate::docvec;

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
    ) -> Result<Document<'static>> {
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
        let receiver_code = self.expression_doc(receiver)?;
        let ex_class_code = self.expression_doc(ex_class)?;
        let handler_code = self.expression_doc(handler)?;

        Ok(docvec![
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
        ])
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
    ) -> Result<Document<'static>> {
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
        let ex_class_code = self.expression_doc(ex_class)?;

        // Rename current state to StateAcc for uniform threading
        let current_state = self.current_state_var();

        let mut docs: Vec<Document<'static>> = vec![docvec![
            format!("let {ex_class_var} = "),
            ex_class_code,
            format!(" in let StateAcc = {current_state} in try "),
        ]];

        // Generate try body (receiver block) with state threading
        // BT-483: Now returns (doc, result_var, state_version)
        let (try_body_doc, try_result_var, try_final) =
            self.generate_exception_body_with_threading(receiver_block)?;
        docs.push(try_body_doc);
        let try_final_var = if try_final == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{try_final}")
        };
        // BT-483: Return {Result, State} from try body
        // Success: pass {Result, State} through + catch clause header
        docs.push(docvec![
            format!(" {{{try_result_var}, {try_final_var}}} "),
            format!("of {state_after_try} -> {state_after_try} "),
            format!("catch <{type_var}, {error_var}, {stack_var}> -> "),
            format!(
                "let {ex_obj_var} = call 'beamtalk_exception_handler':'ensure_wrapped'({error_var}) in "
            ),
            format!(
                "let {match_var} = call 'beamtalk_exception_handler':'matches_class'({ex_class_var}, {error_var}) in "
            ),
            format!("case {match_var} of "),
            "<'true'> when 'true' -> ",
        ]);

        // Bind handler parameter (e.g., [:e | ...] binds e to exception object)
        self.push_scope();
        if let Some(param) = handler_block.parameters.first() {
            let param_var = Self::to_core_erlang_var(&param.name);
            self.bind_var(&param.name, &param_var);
            docs.push(docvec![format!("let {param_var} = {ex_obj_var} in ")]);
        }

        // Generate handler body with state threading (from original StateAcc)
        // BT-483: Now returns (doc, result_var, state_version)
        let (handler_body_doc, handler_result_var, handler_final) =
            self.generate_exception_body_with_threading(handler_block)?;
        docs.push(handler_body_doc);
        let handler_final_var = if handler_final == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{handler_final}")
        };
        // BT-483: Return {Result, State} from handler
        docs.push(docvec![format!(
            " {{{handler_result_var}, {handler_final_var}}} "
        )]);
        self.pop_scope();

        // Re-raise non-matching exceptions
        docs.push(docvec![
            format!(
                "<'false'> when 'true' -> primop 'raw_raise'({type_var}, {error_var}, {stack_var}) "
            ),
            "end",
        ]);

        Ok(Document::Vec(docs))
    }

    /// Generates `ensure:` — wraps block in try, always runs cleanup block.
    ///
    /// Analyzes both receiver and cleanup blocks for state mutations and chooses
    /// the appropriate compilation strategy.
    pub(in crate::codegen::core_erlang) fn generate_ensure(
        &mut self,
        receiver: &Expression,
        cleanup: &Expression,
    ) -> Result<Document<'static>> {
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
        let receiver_code = self.expression_doc(receiver)?;
        let cleanup_code = self.expression_doc(cleanup)?;

        Ok(docvec![
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
        ])
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
    ) -> Result<Document<'static>> {
        if self.is_repl_mode {
            self.repl_loop_mutated = true;
        }

        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let state_after_try = self.fresh_temp_var("StateAfterTry");

        // Rename current state to StateAcc
        let current_state = self.current_state_var();
        let mut docs: Vec<Document<'static>> =
            vec![docvec![format!("let StateAcc = {current_state} in try ")]];

        // Generate try body with state threading
        // BT-483: Now returns (doc, result_var, state_version)
        let (try_body_doc, try_result_var, try_final) =
            self.generate_exception_body_with_threading(receiver_block)?;
        docs.push(try_body_doc);
        let try_final_var = if try_final == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{try_final}")
        };
        // BT-483: Return {Result, State} from try body
        docs.push(docvec![format!(" {{{try_result_var}, {try_final_var}}} ")]);

        // Success: run cleanup starting from try body's state
        // BT-483: Extract Result and State from {Result, State} tuple using element/N
        let result_from_try = self.fresh_temp_var("TryResult");
        docs.push(docvec![format!(
            "of {state_after_try} -> \
             let {result_from_try} = call 'erlang':'element'(1, {state_after_try}) in \
             let StateAcc = call 'erlang':'element'(2, {state_after_try}) in "
        )]);

        let (cleanup_success_doc, _, cleanup_success_final) =
            self.generate_exception_body_with_threading(cleanup_block)?;
        docs.push(cleanup_success_doc);
        let cleanup_success_var = if cleanup_success_final == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{cleanup_success_final}")
        };
        // BT-483: Return try body result with cleanup's final state
        docs.push(docvec![format!(
            " {{{result_from_try}, {cleanup_success_var}}} "
        )]);

        // Error: run cleanup for side effects (from original StateAcc), then re-raise
        docs.push(docvec![format!(
            "catch <{type_var}, {error_var}, {stack_var}> -> "
        )]);

        // Cleanup body generates state mutations that are discarded (re-raise follows)
        let (cleanup_error_doc, _, _) =
            self.generate_exception_body_with_threading(cleanup_block)?;
        docs.push(cleanup_error_doc);

        docs.push(docvec![format!(
            " primop 'raw_raise'({type_var}, {error_var}, {stack_var})"
        )]);

        Ok(Document::Vec(docs))
    }

    /// BT-410/BT-483: Generates block body expressions with state mutation threading.
    ///
    /// Follows the same pattern as `generate_while_body_with_threading`:
    /// - Sets `in_loop_body = true` so field reads/writes use `StateAcc`
    /// - Resets `state_version` to 0 (`StateAcc` is version 0)
    /// - Threads field assignments, self-sends, and local var assignments
    /// - Returns `(doc, result_var, final_state_version)` — the Document holding
    ///   the generated code, the variable holding the last expression's result,
    ///   and the final state version number
    ///
    /// The caller must have already bound `StateAcc` to the current state
    /// before calling this function.
    fn generate_exception_body_with_threading(
        &mut self,
        body: &Block,
    ) -> Result<(Document<'static>, String, usize)> {
        let saved_state_version = self.state_version();
        self.set_state_version(0);

        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        let has_direct_field_assignments = body.body.iter().any(Self::is_field_assignment);

        let mut result_var = "'nil'".to_string();
        let mut docs: Vec<Document<'static>> = Vec::new();

        for (i, expr) in body.body.iter().enumerate() {
            if i > 0 {
                docs.push(Document::String(" ".to_string()));
            }
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
                if is_last {
                    // BT-483: Field assignment returns the assigned value
                    // The val was already bound by generate_field_assignment_open
                    // Use the current state var for the state, and the assigned value as result
                    // Note: generate_field_assignment_open binds _ValN = <value>
                    // We need to capture what was assigned - use nil since field assignment
                    // semantically returns the value but we don't easily have the var name here
                    result_var = "'nil'".to_string();
                }
            } else if self.is_actor_self_send(expr) {
                let doc = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);
                if is_last {
                    // BT-483: Self-dispatch result is in last_dispatch_var
                    if let Some(dv) = self.last_dispatch_var.clone() {
                        let rv = self.fresh_temp_var("ExResult");
                        docs.push(docvec![format!(
                            "let {rv} = call 'erlang':'element'(1, {dv}) in "
                        )]);
                        result_var = rv;
                    }
                }
            } else if Self::is_local_var_assignment(expr) {
                let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);
            } else if is_last {
                if has_direct_field_assignments {
                    // Has direct field assignments — last non-assignment expr result is captured
                    let rv = self.fresh_temp_var("ExResult");
                    let expr_doc = self.expression_doc(expr)?;
                    docs.push(docvec![format!("let {rv} = "), expr_doc, " in"]);
                    result_var = rv;
                } else {
                    // BT-483: Last expression with no direct field assignments.
                    // If this is a nested control flow construct returning {Result, State},
                    // destructure it. Otherwise just capture the result.
                    if self.control_flow_has_mutations(expr) {
                        // Nested mutation construct returns {Result, State} tuple
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let rv = self.fresh_temp_var("ExResult");
                        let next_version = self.state_version() + 1;
                        let next_var = format!("StateAcc{next_version}");
                        let expr_doc = self.expression_doc(expr)?;
                        docs.push(docvec![
                            format!("let {tuple_var} = "),
                            expr_doc,
                            format!(" in let {rv} = call 'erlang':'element'(1, {tuple_var}) in "),
                            format!("let {next_var} = call 'erlang':'element'(2, {tuple_var}) in"),
                        ]);
                        self.set_state_version(next_version);
                        result_var = rv;
                    } else {
                        // Regular expression — capture result, state unchanged
                        let rv = self.fresh_temp_var("ExResult");
                        let expr_doc = self.expression_doc(expr)?;
                        docs.push(docvec![format!("let {rv} = "), expr_doc, " in"]);
                        result_var = rv;
                    }
                }
            } else {
                let expr_doc = self.expression_doc(expr)?;
                docs.push(docvec!["let _ = ", expr_doc, " in",]);
            }
        }

        let final_state_version = self.state_version();
        self.in_loop_body = previous_in_loop_body;
        self.set_state_version(saved_state_version);
        Ok((Document::Vec(docs), result_var, final_state_version))
    }
}
