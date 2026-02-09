// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Exception handling code generation (Block `on:` and `ensure:`).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates Core Erlang `try/catch` for `on:` and `try/after` for `ensure:`.
//! These are structural intrinsics because they must wrap the block execution
//! in Core Erlang exception handling constructs at compile time.
//!
//! # `on:` — Exception Handling (try/catch)
//!
//! ```beamtalk
//! [risky operation] on: [:error | handle error]
//! ```
//!
//! Generates:
//! ```erlang
//! let _BlockFun = <receiver> in
//! let _HandlerFun = <handler> in
//! try apply _BlockFun ()
//! of _Result -> _Result
//! catch <_Type, _Error, _Stacktrace> ->
//!     apply _HandlerFun (_Error)
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
//!     let _ = apply _CleanupFun () in
//!     call 'erlang':'raise'(_Type, _Error, _Stacktrace)
//! ```

use super::{CoreErlangGenerator, Result};
use crate::ast::Expression;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates `on:` — wraps block in try/catch, passes error to handler block.
    ///
    /// The handler block receives the error value (typically a `#beamtalk_error{}`
    /// record) as its single argument.
    pub(in crate::codegen::core_erlang) fn generate_on(
        &mut self,
        receiver: &Expression,
        handler: &Expression,
    ) -> Result<()> {
        // Bind receiver block and handler block to variables
        let block_var = self.fresh_temp_var("BlockFun");
        let handler_var = self.fresh_temp_var("HandlerFun");
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");

        // let _BlockFun = <receiver> in
        write!(self.output, "let {block_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in ")?;

        // let _HandlerFun = <handler> in
        write!(self.output, "let {handler_var} = ")?;
        self.generate_expression(handler)?;
        write!(self.output, " in ")?;

        // try apply _BlockFun ()
        write!(self.output, "try apply {block_var} () ")?;

        // of _Result -> _Result
        write!(self.output, "of {result_var} -> {result_var} ")?;

        // catch <Type, Error, Stacktrace> -> apply _HandlerFun (Error)
        write!(
            self.output,
            "catch <{type_var}, {error_var}, {stack_var}> -> apply {handler_var} ({error_var})"
        )?;

        Ok(())
    }

    /// Generates `ensure:` — wraps block in try, always runs cleanup block.
    ///
    /// The cleanup block runs regardless of whether the body succeeds or fails.
    /// If the body raises an error, the error is re-raised after cleanup.
    pub(in crate::codegen::core_erlang) fn generate_ensure(
        &mut self,
        receiver: &Expression,
        cleanup: &Expression,
    ) -> Result<()> {
        // Bind receiver block and cleanup block to variables
        let block_var = self.fresh_temp_var("BlockFun");
        let cleanup_var = self.fresh_temp_var("CleanupFun");
        let try_result_var = self.fresh_temp_var("TryResult");
        let result_var = self.fresh_temp_var("Result");
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");

        // let _BlockFun = <receiver> in
        write!(self.output, "let {block_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in ")?;

        // let _CleanupFun = <cleanup> in
        write!(self.output, "let {cleanup_var} = ")?;
        self.generate_expression(cleanup)?;
        write!(self.output, " in ")?;

        // try let _TryResult = apply _BlockFun () in _TryResult
        write!(
            self.output,
            "try let {try_result_var} = apply {block_var} () in {try_result_var} "
        )?;

        // of _Result -> let _ = apply _CleanupFun () in _Result
        write!(
            self.output,
            "of {result_var} -> let _ = apply {cleanup_var} () in {result_var} "
        )?;

        // catch <Type, Error, Stack> -> let _ = apply _CleanupFun () in call 'erlang':'raise'(Type, Error, Stack)
        write!(
            self.output,
            "catch <{type_var}, {error_var}, {stack_var}> -> \
             let _ = apply {cleanup_var} () in \
             call 'erlang':'raise'({type_var}, {error_var}, {stack_var})"
        )?;

        Ok(())
    }
}
