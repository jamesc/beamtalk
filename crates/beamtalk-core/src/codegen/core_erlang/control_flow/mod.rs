// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Control flow compilation with state mutation analysis.
//!
//! **DDD Context:** Compilation — Code Generation
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
//! - **Counted loops**: `repeat`, `timesRepeat:`, `to:do:`, `to:by:do:`
//!
//! Submodules organize the code by domain:
//! - [`list_ops`] — List iteration constructs
//! - [`while_loops`] — While loop constructs
//! - [`counted_loops`] — Counted loop constructs

mod counted_loops;
mod list_ops;
mod while_loops;

use super::{CoreErlangGenerator, Result};
use crate::ast::Expression;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generate a local variable assignment inside a loop body with state threading (BT-153).
    ///
    /// Generates code like:
    /// ```erlang
    /// let _Val = <value> in let StateAccN = maps:put('varname', _Val, StateAcc{N-1}) in
    /// ```
    pub(super) fn generate_local_var_assignment_in_loop(
        &mut self,
        expr: &Expression,
    ) -> Result<()> {
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

                // Increment state version for the new state
                let _ = self.next_state_var();
                let new_state = if self.in_loop_body {
                    self.current_state_var()
                } else {
                    format!("State{}", self.state_version())
                };

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
}
