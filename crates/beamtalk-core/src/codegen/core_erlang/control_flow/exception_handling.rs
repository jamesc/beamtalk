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

use super::super::{CoreErlangGenerator, Result};
use crate::ast::Expression;
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates `on:do:` — wraps block in try/catch, wraps error as Exception
    /// object and passes to handler block.
    ///
    /// The `ExClass` argument is bound but currently passed to the runtime for
    /// future class-based exception filtering.
    pub(in crate::codegen::core_erlang) fn generate_on_do(
        &mut self,
        receiver: &Expression,
        ex_class: &Expression,
        handler: &Expression,
    ) -> Result<()> {
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

    /// Generates `ensure:` — wraps block in try, always runs cleanup block.
    ///
    /// If the body raises an error, the error is re-raised after cleanup.
    pub(in crate::codegen::core_erlang) fn generate_ensure(
        &mut self,
        receiver: &Expression,
        cleanup: &Expression,
    ) -> Result<()> {
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
}
