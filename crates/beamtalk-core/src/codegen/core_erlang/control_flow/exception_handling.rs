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
//! catch <_Type, _Error, _RawStack> ->
//!     let _BuiltStack = primop 'build_stacktrace'(_RawStack) in
//!     let _ExObj = call 'beamtalk_exception_handler':'ensure_wrapped'(_Type, _Error, _BuiltStack) in
//!     case matches_class(ExClass, ExObj) of
//!         true  -> apply _HandlerFun (_ExObj)
//!         false -> primop 'raw_raise'(_Type, _Error, _RawStack)
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
use super::super::document::{join, leaf};
use super::super::intrinsics::{
    STATEFUL_BLOCK_DISPATCH_HINT, validate_block_arity_exact, validate_on_do_handler,
};
use super::super::{CoreErlangGenerator, Result, block_analysis};
use crate::ast::{Block, Expression};
use crate::docvec;

impl CoreErlangGenerator {
    fn state_acc_var_doc(state_version: usize) -> Document<'static> {
        match state_version {
            0 => docvec!["StateAcc"],
            n => leaf::var(super::super::util::versioned_var("StateAcc", n)),
        }
    }

    fn emit_raw_raise(type_var: String, error_var: String, stack_var: String) -> Document<'static> {
        docvec![
            "primop 'raw_raise'(",
            leaf::var(type_var),
            ", ",
            leaf::var(error_var),
            ", ",
            leaf::var(stack_var),
            ")",
        ]
    }

    /// Generates the NLR-passthrough catch clause preamble shared by both
    /// `generate_on_do` and `generate_on_do_with_mutations`.
    ///
    /// Produces an open-ended fragment; caller appends the `<'true'>` branch
    /// body, the `<'false'>` re-raise arm, and the closing `end end`.
    ///
    /// BT-754/BT-761/BT-854: NLR throws (`{'$bt_nlr', ...}`) must bypass
    /// on:do: so the enclosing method's NLR handler can intercept them.
    #[allow(clippy::too_many_arguments)]
    fn on_do_catch_preamble(
        type_var: &str,
        error_var: &str,
        stack_var: String,
        nlr_tok_var: String,
        nlr_val_var: String,
        nlr_state_var: String,
        nlr_tok_var2: String,
        nlr_val_var2: String,
        other_pair_var: String,
        built_stack_var: String,
        ex_obj_var: String,
        match_var: String,
        ex_class_var: String,
    ) -> Document<'static> {
        docvec![
            "catch <",
            leaf::var(type_var.to_string()),
            ", ",
            leaf::var(error_var.to_string()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> ",
            "case {",
            leaf::var(type_var.to_string()),
            ", ",
            leaf::var(error_var.to_string()),
            "} of ",
            "<{'throw', {'$bt_nlr', ",
            leaf::var(nlr_tok_var),
            ", ",
            leaf::var(nlr_val_var),
            ", ",
            leaf::var(nlr_state_var),
            "}}> when 'true' -> ",
            Self::emit_raw_raise(
                type_var.to_string(),
                error_var.to_string(),
                stack_var.clone(),
            ),
            " ",
            "<{'throw', {'$bt_nlr', ",
            leaf::var(nlr_tok_var2),
            ", ",
            leaf::var(nlr_val_var2),
            "}}> when 'true' -> ",
            Self::emit_raw_raise(
                type_var.to_string(),
                error_var.to_string(),
                stack_var.clone(),
            ),
            " ",
            "<",
            leaf::var(other_pair_var),
            "> when 'true' -> ",
            "let ",
            leaf::var(built_stack_var.clone()),
            " = primop 'build_stacktrace'(",
            leaf::var(stack_var),
            ") in ",
            "let ",
            leaf::var(ex_obj_var.clone()),
            " = call 'beamtalk_exception_handler':'ensure_wrapped'(",
            leaf::var(type_var.to_string()),
            ", ",
            leaf::var(error_var.to_string()),
            ", ",
            leaf::var(built_stack_var),
            ") in ",
            "let ",
            leaf::var(match_var.clone()),
            " = call 'beamtalk_exception_handler':'matches_class'(",
            leaf::var(ex_class_var),
            ", ",
            leaf::var(ex_obj_var),
            ") in ",
            "case ",
            leaf::var(match_var),
            " of ",
            "<'true'> when 'true' -> ",
        ]
    }

    /// Builds the `apply HandlerFun (ExObj)` or `apply HandlerFun ()` fragment
    /// for `on:do:` exception handlers.
    fn make_handler_apply(
        handler_var: String,
        ex_obj_var: String,
        takes_arg: bool,
    ) -> Document<'static> {
        if takes_arg {
            docvec![
                "apply ",
                leaf::var(handler_var),
                " (",
                leaf::var(ex_obj_var),
                ")",
            ]
        } else {
            docvec!["apply ", leaf::var(handler_var), " ()"]
        }
    }

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
        // BT-493: Validate protected block arity (must be 0-arg)
        validate_block_arity_exact(
            receiver,
            0,
            "on:do:",
            "Fix: The protected block must take no arguments:\n\
             \x20 [riskyOperation] on: Exception do: [:e | handle error]",
        )?;
        // BT-493: Validate handler block arity (must be 0 or 1-arg)
        // Returns true if handler takes an argument, false for 0-arg
        let handler_takes_arg = validate_on_do_handler(handler, "on:do:")?;

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
        let built_stack_var = self.fresh_temp_var("BuiltStack");
        let ex_obj_var = self.fresh_temp_var("ExObj");
        let match_var = self.fresh_temp_var("Match");

        // Capture expression outputs (ADR 0018 bridge pattern)
        let receiver_code = self.expression_doc(receiver)?;
        let ex_class_code = self.expression_doc(ex_class)?;
        let handler_code = self.expression_doc(handler)?;

        let handler_apply =
            Self::make_handler_apply(handler_var.clone(), ex_obj_var.clone(), handler_takes_arg);

        // BT-754: Fresh variable names for the NLR pattern guard (Core Erlang
        // does not support anonymous `_` wildcards — each must be unique).
        let nlr_tok_var = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var = self.fresh_temp_var("NlrCheckVal");
        // BT-761: Actor NLR throws include state as a 4th element.
        let nlr_state_var = self.fresh_temp_var("NlrCheckState");
        let nlr_tok_var2 = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var2 = self.fresh_temp_var("NlrCheckVal");
        // Fallback pattern: ONE variable binding the whole 2-tuple (not two separate elements).
        let other_pair_var = self.fresh_temp_var("OtherPair");

        Ok(docvec![
            "let ",
            leaf::var(block_var.clone()),
            " = ",
            receiver_code,
            " in let ",
            leaf::var(ex_class_var.clone()),
            " = ",
            ex_class_code,
            " in let ",
            leaf::var(handler_var),
            " = ",
            handler_code,
            " in try apply ",
            leaf::var(block_var),
            " () ",
            "of ",
            leaf::var(result_var.clone()),
            " -> ",
            leaf::var(result_var),
            " ",
            Self::on_do_catch_preamble(
                &type_var,
                &error_var,
                stack_var.clone(),
                nlr_tok_var,
                nlr_val_var,
                nlr_state_var,
                nlr_tok_var2,
                nlr_val_var2,
                other_pair_var,
                built_stack_var,
                ex_obj_var,
                match_var,
                ex_class_var,
            ),
            handler_apply,
            " ",
            "<'false'> when 'true' -> ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
            " end ",
            "end",
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
    /// catch <Type, Error, RawStack> ->
    ///     let BuiltStack = primop 'build_stacktrace'(RawStack) in
    ///     let ExObj = call 'beamtalk_exception_handler':'ensure_wrapped'(Type, Error, BuiltStack) in
    ///     let Match = call 'beamtalk_exception_handler':'matches_class'(ExClass, ExObj) in
    ///     case Match of
    ///         true  -> let _e = ExObj in <handler body with threading> StateAccM
    ///         false -> primop 'raw_raise'(Type, Error, RawStack)
    /// ```
    fn generate_on_do_with_mutations(
        &mut self,
        receiver_block: &Block,
        ex_class: &Expression,
        handler_block: &Block,
    ) -> Result<Document<'static>> {
        if self.is_repl_mode() {
            self.set_repl_loop_mutated(true);
        }

        let ex_class_var = self.fresh_temp_var("ExClass");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let built_stack_var = self.fresh_temp_var("BuiltStack");
        let ex_obj_var = self.fresh_temp_var("ExObj");
        let match_var = self.fresh_temp_var("Match");
        let state_after_try = self.fresh_temp_var("StateAfterTry");
        // BT-754: Unique names for NLR pattern variables (no anonymous _ in Core Erlang).
        let nlr_tok_var = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var = self.fresh_temp_var("NlrCheckVal");
        // BT-761: Actor NLR throws include state as a 4th element.
        let nlr_state_var = self.fresh_temp_var("NlrCheckState");
        let nlr_tok_var2 = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var2 = self.fresh_temp_var("NlrCheckVal");
        // Fallback pattern: ONE variable binding the whole 2-tuple (not two separate elements).
        let other_pair_var = self.fresh_temp_var("OtherPair");

        // Bind exception class
        let ex_class_code = self.expression_doc(ex_class)?;
        // Rename current state to StateAcc for uniform threading
        let current_state = self.current_state_var();

        let mut docs: Vec<Document<'static>> = vec![docvec![
            "let ",
            leaf::var(ex_class_var.clone()),
            " = ",
            ex_class_code,
            " in let StateAcc = ",
            leaf::var(current_state),
            " in try ",
        ]];

        // Generate try body (receiver block) with state threading
        // BT-483: Now returns (doc, result_var, state_version)
        let (try_body_doc, try_result_var, try_final) =
            self.generate_exception_body_with_threading(receiver_block)?;
        docs.push(try_body_doc);
        // BT-483: Return {Result, State} from try body
        // Success: pass {Result, State} through + catch clause with NLR passthrough.
        // BT-754/BT-761/BT-854: NLR re-raise via on_do_catch_preamble (see generate_on_do).
        docs.push(docvec![
            " {",
            leaf::var(try_result_var),
            ", ",
            Self::state_acc_var_doc(try_final),
            "} ",
            "of ",
            leaf::var(state_after_try.clone()),
            " -> ",
            leaf::var(state_after_try),
            " ",
            Self::on_do_catch_preamble(
                &type_var,
                &error_var,
                stack_var.clone(),
                nlr_tok_var,
                nlr_val_var,
                nlr_state_var,
                nlr_tok_var2,
                nlr_val_var2,
                other_pair_var,
                built_stack_var,
                ex_obj_var.clone(),
                match_var,
                ex_class_var,
            ),
        ]);
        // Bind handler parameter (e.g., [:e | ...] binds e to exception object)
        self.push_scope();
        if let Some(param) = handler_block.parameters.first() {
            let param_var = Self::to_core_erlang_var(&param.name);
            self.bind_var(&param.name, &param_var);
            docs.push(docvec![
                "let ",
                leaf::var(param_var),
                " = ",
                leaf::var(ex_obj_var),
                " in ",
            ]);
        }

        // Generate handler body with state threading (from original StateAcc)
        // BT-483: Now returns (doc, result_var, state_version)
        let (handler_body_doc, handler_result_var, handler_final) =
            self.generate_exception_body_with_threading(handler_block)?;
        docs.push(handler_body_doc);
        // BT-483: Return {Result, State} from handler
        docs.push(docvec![
            " {",
            leaf::var(handler_result_var),
            ", ",
            Self::state_acc_var_doc(handler_final),
            "} ",
        ]);
        self.pop_scope();

        // Re-raise non-matching exceptions; close the matches_class case and the outer NLR case.
        docs.push(docvec![
            "<'false'> when 'true' -> ",
            Self::emit_raw_raise(type_var.clone(), error_var.clone(), stack_var),
            " end end",
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
        // BT-493: Validate cleanup block arity (must be 0-arg)
        validate_block_arity_exact(
            cleanup,
            0,
            "ensure:",
            "Fix: The cleanup block must take no arguments:\n\
             \x20 [operation] ensure: [resource close]",
        )?;

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
            "let ",
            leaf::var(block_var.clone()),
            " = ",
            receiver_code,
            " in let ",
            leaf::var(cleanup_var.clone()),
            " = ",
            cleanup_code,
            " in try let ",
            leaf::var(try_result_var.clone()),
            " = apply ",
            leaf::var(block_var),
            " () in ",
            leaf::var(try_result_var),
            " ",
            "of ",
            leaf::var(result_var.clone()),
            " -> let _ = apply ",
            leaf::var(cleanup_var.clone()),
            " () in ",
            leaf::var(result_var),
            " ",
            "catch <",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(error_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> do apply ",
            leaf::var(cleanup_var),
            " () ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
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
        if self.is_repl_mode() {
            self.set_repl_loop_mutated(true);
        }

        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let state_after_try = self.fresh_temp_var("StateAfterTry");

        // Rename current state to StateAcc
        let current_state = self.current_state_var();
        let mut docs: Vec<Document<'static>> = vec![docvec![
            "let StateAcc = ",
            leaf::var(current_state),
            " in try ",
        ]];

        // Generate try body with state threading
        // BT-483: Now returns (doc, result_var, state_version)
        let (try_body_doc, try_result_var, try_final) =
            self.generate_exception_body_with_threading(receiver_block)?;
        docs.push(try_body_doc);
        // BT-483: Return {Result, State} from try body
        docs.push(docvec![
            " {",
            leaf::var(try_result_var),
            ", ",
            Self::state_acc_var_doc(try_final),
            "} ",
        ]);

        // Success: run cleanup starting from try body's state
        // BT-483: Extract Result and State from {Result, State} tuple using element/N
        let result_from_try = self.fresh_temp_var("TryResult");
        docs.push(docvec![
            "of ",
            leaf::var(state_after_try.clone()),
            " -> let ",
            leaf::var(result_from_try.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(state_after_try.clone()),
            ") in let StateAcc = call 'erlang':'element'(2, ",
            leaf::var(state_after_try),
            ") in ",
        ]);

        let (cleanup_success_doc, _, cleanup_success_final) =
            self.generate_exception_body_with_threading(cleanup_block)?;
        docs.push(cleanup_success_doc);
        // BT-483: Return try body result with cleanup's final state
        docs.push(docvec![
            " {",
            leaf::var(result_from_try),
            ", ",
            Self::state_acc_var_doc(cleanup_success_final),
            "} ",
        ]);

        // Error: run cleanup for side effects (from original StateAcc), then re-raise
        docs.push(docvec![
            "catch <",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(error_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> ",
        ]);

        // Cleanup body generates state mutations that are discarded (re-raise follows)
        let (cleanup_error_doc, _, _) =
            self.generate_exception_body_with_threading(cleanup_block)?;
        docs.push(cleanup_error_doc);

        docs.push(docvec![
            " ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
        ]);

        Ok(Document::Vec(docs))
    }

    /// Builds the Tier 1 (pure protected block) `try`/`catch` body for
    /// `generate_on_do_structural_fallback` — factored out to keep that
    /// function under clippy's line-count limit. Reuses
    /// `on_do_catch_preamble`'s NLR-passthrough + `matches_class` structure;
    /// only the handler's tier (arity 0 = pure 0-arg, arity 1 = pure 1-arg,
    /// anything else = stateful) is discriminated dynamically here, since it
    /// isn't known statically the way `generate_on_do` knows it from the
    /// literal block AST.
    fn generate_on_do_tier1_try(
        &mut self,
        self_var: &'static str,
        ex_class_param: String,
        handler_param: String,
        class_name: &str,
    ) -> Document<'static> {
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        let built_stack_var = self.fresh_temp_var("BuiltStack");
        let ex_obj_var = self.fresh_temp_var("ExObj");
        let match_var = self.fresh_temp_var("Match");
        let nlr_tok_var = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var = self.fresh_temp_var("NlrCheckVal");
        let nlr_state_var = self.fresh_temp_var("NlrCheckState");
        let nlr_tok_var2 = self.fresh_temp_var("NlrCheckTok");
        let nlr_val_var2 = self.fresh_temp_var("NlrCheckVal");
        let other_pair_var = self.fresh_temp_var("OtherPair");

        // arity 1 is ambiguous between a pure 1-arg handler and a stateful
        // 0-arg handler — same documented ambiguity as BT-2812's blockValue*
        // (deferred disambiguation, see BT-2892); anything else is Tier 2.
        let handler_stateful_error = self.generate_stateful_block_dispatch_error(
            "on:do:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let handler_dispatch = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(handler_param.clone()),
            ", 0) of <'true'> when 'true' -> apply ",
            leaf::var(handler_param.clone()),
            " () <'false'> when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(handler_param.clone()),
            ", 1) of <'true'> when 'true' -> apply ",
            leaf::var(handler_param),
            " (",
            leaf::var(ex_obj_var.clone()),
            ") <'false'> when 'true' -> ",
            handler_stateful_error,
            " end end",
        ];

        let catch_preamble = Self::on_do_catch_preamble(
            &type_var,
            &error_var,
            stack_var.clone(),
            nlr_tok_var,
            nlr_val_var,
            nlr_state_var,
            nlr_tok_var2,
            nlr_val_var2,
            other_pair_var,
            built_stack_var,
            ex_obj_var,
            match_var,
            ex_class_param,
        );

        docvec![
            "try apply ",
            Document::Str(self_var),
            " () of ",
            leaf::var(result_var.clone()),
            " -> ",
            leaf::var(result_var),
            " ",
            catch_preamble,
            handler_dispatch,
            " <'false'> when 'true' -> ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
            " end end",
        ]
    }

    /// BT-2908: Generates the fallback method body for `onDo` — Block's
    /// `on:do:`. Reached only via generic dispatch bypassing the call-site
    /// interception `generate_on_do` normally provides (e.g. `perform:`). See
    /// `generate_block_value_structural_fallback` (BT-2812) for the general
    /// Tier 1/Tier 2 discrimination rationale.
    ///
    /// Reuses `on_do_catch_preamble`'s NLR-passthrough + `matches_class`
    /// structure so the Tier 1 (pure) case stays behaviourally identical to
    /// the AST-driven `generate_on_do` — only the receiver/handler *tier*
    /// discrimination differs, since a generically dispatched handler's
    /// declared arity (0 or 1) isn't known statically the way it is when
    /// `generate_on_do` reads it straight off the literal block AST.
    /// `current_method_params` are `[ExClass, Handler]` (the `on:`/`do:`
    /// keyword arguments); `Self` is the protected block.
    pub(in crate::codegen::core_erlang) fn generate_on_do_structural_fallback(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let ex_class_param = self
            .current_method_params
            .first()
            .cloned()
            .unwrap_or_else(|| "ExClass".to_string());
        let handler_param = self
            .current_method_params
            .get(1)
            .cloned()
            .unwrap_or_else(|| "Handler".to_string());

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let params_doc = join(
            [
                leaf::var(ex_class_param.clone()),
                leaf::var(handler_param.clone()),
            ],
            &Document::Str(", "),
        );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'('onDo', [",
            params_doc,
            "], ",
            Document::Str(self_var),
            ")",
        ];

        let tier1_try =
            self.generate_on_do_tier1_try(self_var, ex_class_param, handler_param, class_name);

        let self_stateful_error = self.generate_stateful_block_dispatch_error(
            "on:do:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 0) of <'true'> when 'true' -> ",
            tier1_try,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 1) of <'true'> when 'true' -> ",
            self_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
    }

    /// BT-2908: Generates the fallback method body for `ensure` — Block's
    /// `ensure:`. Reached only via generic dispatch bypassing the call-site
    /// interception `generate_ensure` normally provides (e.g. `perform:`).
    /// See `generate_block_value_structural_fallback` (BT-2812) for the
    /// general Tier 1/Tier 2 discrimination rationale.
    ///
    /// Both receiver and cleanup block must be Tier 1 (pure, 0-arg funs) for
    /// the generic try/catch below to be correct — Core Erlang's try/catch
    /// mechanics don't themselves need the block's AST, only ADR-0041's
    /// state-threading convention does, so the pure case is fully generic.
    /// `current_method_params[0]` is the cleanup block; `Self` is the
    /// protected block.
    pub(in crate::codegen::core_erlang) fn generate_ensure_structural_fallback(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let cleanup_param = self
            .current_method_params
            .first()
            .cloned()
            .unwrap_or_else(|| "CleanupBlock".to_string());

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'('ensure', [",
            leaf::var(cleanup_param.clone()),
            "], ",
            Document::Str(self_var),
            ")",
        ];

        let try_result_var = self.fresh_temp_var("TryResult");
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");

        let tier1_try = docvec![
            "try let ",
            leaf::var(try_result_var.clone()),
            " = apply ",
            Document::Str(self_var),
            " () in ",
            leaf::var(try_result_var),
            " of ",
            leaf::var(result_var.clone()),
            " -> let _ = apply ",
            leaf::var(cleanup_param.clone()),
            " () in ",
            leaf::var(result_var),
            " catch <",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(error_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "> -> do apply ",
            leaf::var(cleanup_param.clone()),
            " () ",
            Self::emit_raw_raise(type_var, error_var, stack_var),
        ];

        let cleanup_stateful_error = self.generate_stateful_block_dispatch_error(
            "ensure:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let cleanup_tier_check = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(cleanup_param.clone()),
            ", 0) of <'true'> when 'true' -> ",
            tier1_try,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(cleanup_param),
            ", 1) of <'true'> when 'true' -> ",
            cleanup_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch.clone(),
            " end end",
        ];

        let self_stateful_error = self.generate_stateful_block_dispatch_error(
            "ensure:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 0) of <'true'> when 'true' -> ",
            cleanup_tier_check,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 1) of <'true'> when 'true' -> ",
            self_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
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
        self.with_branch_context(|this| this.generate_exception_body_with_threading_inner(body))
    }

    /// Inner implementation called inside `with_branch_context`.
    fn generate_exception_body_with_threading_inner(
        &mut self,
        body: &Block,
    ) -> Result<(Document<'static>, String, usize)> {
        let has_direct_field_assignments = body
            .body
            .iter()
            .any(|s| Self::is_field_assignment(&s.expression));

        let mut result_var = "'nil'".to_string();
        let mut docs: Vec<Document<'static>> = Vec::new();

        for (i, stmt) in body.body.iter().enumerate() {
            let expr = &stmt.expression;
            if i > 0 {
                docs.push(Document::Str(" "));
            }
            let is_last = i == body.body.len() - 1;

            if Self::is_field_assignment(expr) {
                let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
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
                let (doc, dispatch_var) = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);
                if is_last {
                    // BT-483: Self-dispatch result is in dispatch_var
                    let rv = self.fresh_temp_var("ExResult");
                    docs.push(docvec![
                        "let ",
                        leaf::var(rv.clone()),
                        " = call 'erlang':'element'(1, ",
                        leaf::var(dispatch_var),
                        ") in ",
                    ]);
                    result_var = rv;
                }
            } else if Self::is_local_var_assignment(expr) {
                let (assign_doc, _val_var) = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(assign_doc);
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    docs.push(d);
                }
            } else if is_last {
                if has_direct_field_assignments {
                    // Has direct field assignments — last non-assignment expr result is captured
                    let rv = self.fresh_temp_var("ExResult");
                    let expr_doc = self.expression_doc(expr)?;
                    docs.push(docvec![
                        "let ",
                        leaf::var(rv.clone()),
                        " = ",
                        expr_doc,
                        " in",
                    ]);
                    result_var = rv;
                } else {
                    // BT-483: Last expression with no direct field assignments.
                    // If this is a nested control flow construct returning {Result, State},
                    // destructure it. Otherwise just capture the result.
                    if self.control_flow_has_mutations(expr) {
                        // Nested mutation construct returns {Result, State} tuple
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let rv = self.fresh_temp_var("ExResult");
                        let next_var = self.peek_next_state_var();
                        let expr_doc = self.expression_doc(expr)?;
                        docs.push(docvec![
                            "let ",
                            leaf::var(tuple_var.clone()),
                            " = ",
                            expr_doc,
                            " in let ",
                            leaf::var(rv.clone()),
                            " = call 'erlang':'element'(1, ",
                            leaf::var(tuple_var.clone()),
                            ") in ",
                            "let ",
                            leaf::var(next_var),
                            " = call 'erlang':'element'(2, ",
                            leaf::var(tuple_var),
                            ") in",
                        ]);
                        let _ = self.next_state_var();
                        result_var = rv;
                    } else {
                        // Regular expression — capture result, state unchanged
                        let rv = self.fresh_temp_var("ExResult");
                        let expr_doc = self.expression_doc(expr)?;
                        docs.push(docvec![
                            "let ",
                            leaf::var(rv.clone()),
                            " = ",
                            expr_doc,
                            " in",
                        ]);
                        result_var = rv;
                    }
                }
            } else {
                let expr_doc = self.expression_doc(expr)?;
                docs.push(docvec!["let _ = ", expr_doc, " in",]);
            }
        }

        let final_state_version = self.state_version();
        Ok((Document::Vec(docs), result_var, final_state_version))
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::core_erlang::tests::codegen;

    #[test]
    fn test_on_do_generates_try_catch_with_nlr_passthrough() {
        // on:do: generates a try/catch via closure approach with exception wrapping,
        // class matching, and NLR passthrough (re-raises $bt_nlr throws)
        let src =
            "Actor subclass: Srv\n  state: x = 0\n\n  run =>\n    [42] on: Error do: [:e | 0]\n";
        let code = codegen(src);
        assert!(
            code.contains("try apply"),
            "on:do: should generate a try/catch via apply. Got:\n{code}"
        );
        assert!(
            code.contains("'beamtalk_exception_handler':'ensure_wrapped'"),
            "on:do: should wrap the error via ensure_wrapped. Got:\n{code}"
        );
        assert!(
            code.contains("'beamtalk_exception_handler':'matches_class'"),
            "on:do: should check the class via matches_class. Got:\n{code}"
        );
        // NLR throws must be re-raised, not caught by on:do:
        assert!(
            code.contains("'$bt_nlr'"),
            "on:do: should detect NLR throws. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "on:do: should re-raise NLR throws via raw_raise. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_in_class_method_simple() {
        // BT-1346: ensure: in a class method (no mutations) should compile
        let src = "Object subclass: Foo\n\n  class bar =>\n    [42] ensure: [nil]\n";
        let code = codegen(src);
        assert!(
            code.contains("try"),
            "class method ensure: should generate try. Got:\n{code}"
        );
        // Must NOT reference State (class methods have no actor state)
        assert!(
            !code.contains("let StateAcc = State"),
            "class method ensure: must not reference actor State. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_in_class_method_with_captured_local_mutation() {
        // BT-1346: ensure: in a class method where locals declared outside
        // the block are reassigned inside — must use closure path, not mutation threading
        let src = "\
Actor subclass: Foo
  state: x = 0

  class build: block =>
    routeList := nil
    nfHandler := nil
    [
      routeList := 42
      nfHandler := 99
    ] ensure: [nil]
    routeList
";
        let code = codegen(src);
        assert!(
            code.contains("try"),
            "class method ensure: with captured mutation should generate try. Got:\n{code}"
        );
        // Must NOT reference State (class methods have no actor state)
        assert!(
            !code.contains("let StateAcc = State"),
            "class method ensure: must not reference actor State. Got:\n{code}"
        );
        // Should use closure-based approach (BlockFun/CleanupFun), not mutation threading
        assert!(
            code.contains("apply") && code.contains("do apply"),
            "class method ensure: should use closure-based try/catch. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_generates_try_of_catch_with_cleanup() {
        // ensure: generates a try/of/catch (not Core Erlang try/after) with cleanup
        // applied in both success and error paths
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run =>\n    [42] ensure: [0]\n";
        let code = codegen(src);
        // ensure:-specific pattern: cleanup is applied via `do apply` in the catch clause
        assert!(
            code.contains("do apply"),
            "ensure: catch should run cleanup via 'do apply'. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "ensure: catch should re-raise after cleanup. Got:\n{code}"
        );
    }

    #[test]
    fn test_on_do_zero_arg_handler_no_exception_binding() {
        // Handler block takes no argument: [0] instead of [:e | 0].
        // make_handler_apply is called with takes_arg=false, generating
        // `apply HandlerFun ()` rather than `apply HandlerFun (ExObj)`.
        let src = "Actor subclass: Srv\n  state: x = 0\n\n  run =>\n    [42] on: Error do: [0]\n";
        let code = codegen(src);
        assert!(
            code.split("apply _HandlerFun").skip(1).any(|suffix| {
                suffix
                    .trim_start_matches(|ch: char| ch.is_ascii_digit())
                    .starts_with(" ()")
            }),
            "on:do: with 0-arg handler should apply handler with empty args. Got:\n{code}"
        );
        assert!(
            !code.contains(" (_ExObj"),
            "on:do: with 0-arg handler must not pass exception object to handler apply. Got:\n{code}"
        );
        assert!(
            code.contains("try apply"),
            "on:do: with 0-arg handler should generate try/catch via apply. Got:\n{code}"
        );
        assert!(
            code.contains("'$bt_nlr'"),
            "on:do: with 0-arg handler should detect NLR throws. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "on:do: with 0-arg handler should re-raise non-matching exceptions. Got:\n{code}"
        );
    }

    #[test]
    fn test_on_do_with_state_mutation_in_handler_uses_threading() {
        // Handler block mutates actor field — triggers generate_on_do_with_mutations,
        // which inlines block bodies with StateAcc threading instead of wrapping as
        // closures. Also exercises on_do_catch_preamble and
        // generate_exception_body_with_threading.
        let src = "\
Actor subclass: Srv
  state: count = 0

  run =>
    [42] on: Error do: [:e | self.count := self.count + 1]
";
        let code = codegen(src);
        assert!(
            code.contains("StateAcc"),
            "on:do: with handler state mutation must use StateAcc threading. Got:\n{code}"
        );
        assert!(
            code.contains("'beamtalk_exception_handler':'ensure_wrapped'"),
            "on:do: with mutation must still wrap exceptions. Got:\n{code}"
        );
        assert!(
            code.contains("'$bt_nlr'"),
            "on:do: with mutation must still detect NLR throws. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "on:do: with mutation must re-raise non-matching exceptions. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_with_state_mutation_in_cleanup_uses_threading() {
        // Cleanup block mutates actor field — triggers generate_ensure_with_mutations,
        // which inlines both try body and cleanup with StateAcc threading.
        let src = "\
Actor subclass: Srv
  state: count = 0

  run =>
    [42] ensure: [self.count := self.count + 1]
";
        let code = codegen(src);
        assert!(
            code.contains("StateAcc"),
            "ensure: with cleanup state mutation must use StateAcc threading. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "ensure: with mutation must re-raise after cleanup on error. Got:\n{code}"
        );
    }

    #[test]
    fn test_ensure_with_state_mutation_in_try_body_uses_threading() {
        // Try body (receiver block) mutates actor field — also triggers
        // generate_ensure_with_mutations. Exercises the try-body threading branch
        // (generate_exception_body_with_threading called for receiver_block).
        let src = "\
Actor subclass: Srv
  state: count = 0

  run =>
    [self.count := self.count + 1] ensure: [nil]
";
        let code = codegen(src);
        assert!(
            code.contains("StateAcc"),
            "ensure: with try-body state mutation must use StateAcc threading. Got:\n{code}"
        );
        assert!(
            code.contains("primop 'raw_raise'"),
            "ensure: with mutation must re-raise after cleanup on error. Got:\n{code}"
        );
    }
}
