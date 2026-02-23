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
mod exception_handling;
mod list_ops;
mod while_loops;

use super::document::Document;
use super::{CodeGenContext, CoreErlangGenerator, Result, block_analysis};
use crate::ast::Expression;
use crate::docvec;

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
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");
                let current_state = if self.state_version() == 0 {
                    "StateAcc".to_string()
                } else {
                    format!("StateAcc{}", self.state_version())
                };

                // Capture value expression (ADR 0018 bridge)
                let value_code = self.expression_doc(value)?;

                // Increment state version for the new state
                let _ = self.next_state_var();
                let new_state = if self.in_loop_body {
                    self.current_state_var()
                } else {
                    format!("State{}", self.state_version())
                };

                // BT-790: In REPL mode, use the plain variable name as the key
                // (no __local__ prefix) since there are no actor fields to collide with.
                // This ensures reads (`maps:get('x', StateAcc)`) match writes
                // (`maps:put('x', ..., StateAcc)`), allowing mutations to accumulate
                // correctly across loop iterations.
                let state_key = if self.is_repl_mode {
                    id.name.clone()
                } else {
                    Self::local_state_key(&id.name).into()
                };

                return Ok(docvec![
                    "let ",
                    val_var.as_str(),
                    " = ",
                    value_code,
                    " in let ",
                    new_state,
                    " = call 'maps':'put'('",
                    state_key.to_string(),
                    "', ",
                    val_var.as_str(),
                    ", ",
                    current_state,
                    ") in ",
                ]);
            }
        }
        Ok(Document::Nil)
    }

    /// BT-598: Compute local variables that need threading through a loop's `StateAcc`.
    /// Returns the sorted list of local vars that are both read and written in the block
    /// (excluding block parameters). For actor methods only; returns empty for REPL mode.
    pub(super) fn compute_threaded_locals_for_loop(
        &self,
        body: &crate::ast::Block,
        condition: Option<&Expression>,
    ) -> Vec<String> {
        if self.is_repl_mode || self.context != CodeGenContext::Actor {
            return Vec::new();
        }

        let analysis = block_analysis::analyze_block(body);
        let block_params: std::collections::HashSet<String> =
            body.parameters.iter().map(|p| p.name.to_string()).collect();

        // Include reads from condition block if provided
        let mut all_reads = analysis.local_reads.clone();
        if let Some(Expression::Block(cond_block)) = condition {
            let cond_analysis = block_analysis::analyze_block(cond_block);
            all_reads = all_reads
                .union(&cond_analysis.local_reads)
                .cloned()
                .collect();
        }

        all_reads
            .intersection(&analysis.local_writes)
            .filter(|v| !block_params.contains(*v))
            .cloned()
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect()
    }

    /// BT-598: Returns the state map key for a local variable.
    /// Uses a `__local__` prefix to prevent collision with actor field names.
    pub(super) fn local_state_key(var_name: &str) -> String {
        format!("__local__{var_name}")
    }
}
