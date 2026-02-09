// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL evaluation module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for REPL evaluation modules,
//! creating simple modules with an `eval/1` function that evaluates
//! an expression with the provided bindings map.

use super::{CodeGenContext, CoreErlangGenerator, Result};
use crate::ast::Expression;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates a simple REPL evaluation module.
    ///
    /// Creates a module with a single `eval/1` function that evaluates
    /// an expression with the provided bindings map.
    ///
    /// # Arguments
    ///
    /// * `expression` - The expression to evaluate
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// module 'repl_eval_42' ['eval'/1]
    ///   attributes []
    ///
    /// 'eval'/1 = fun (Bindings) ->
    ///     let State = Bindings in
    ///     <expression>
    /// end
    /// ```
    ///
    /// The `State` alias ensures that identifier lookups work correctly,
    /// since `generate_identifier` falls back to `maps:get(Name, State)`
    /// for variables not bound in the current scope.
    pub(super) fn generate_repl_module(&mut self, expression: &Expression) -> Result<()> {
        // BT-213: Set context to Repl for this module
        self.context = CodeGenContext::Repl;
        self.is_repl_mode = true; // Also set legacy flag for compatibility
        // BT-374 / ADR 0010: REPL runs in workspace context, bindings are available
        self.workspace_mode = true;

        // Module header - simple module with just eval/1
        writeln!(self.output, "module '{}' ['eval'/1]", self.module_name)?;
        writeln!(self.output, "  attributes []")?;
        writeln!(self.output)?;

        // Generate eval/1 function
        // Returns {Result, UpdatedBindings} to support mutation threading
        writeln!(self.output, "'eval'/1 = fun (Bindings) ->")?;
        self.indent += 1;

        // Register Bindings in scope for variable lookups
        self.push_scope();
        self.bind_var("__bindings__", "Bindings");

        // BT-153: Set REPL mode so mutations to local variables update bindings
        // Save the previous value so generator can be reused
        let previous_is_repl_mode = self.is_repl_mode;
        self.is_repl_mode = true;

        // Alias State to Bindings for identifier fallback lookup
        self.write_indent()?;
        writeln!(self.output, "let State = Bindings in")?;

        // Generate the expression, capturing intermediate result
        self.write_indent()?;
        write!(self.output, "let Result = ")?;
        self.generate_expression(expression)?;
        writeln!(self.output, " in")?;

        // Return tuple {ResultValue, UpdatedBindings}
        // BT-153: For mutation-threaded loops, Result IS the updated state.
        // For simple expressions, Result is the value and State is unchanged.
        let final_state = self.current_state_var();
        self.write_indent()?;
        if final_state == "State" {
            // No mutation - Result is the value, State is unchanged bindings
            writeln!(self.output, "{{Result, State}}")?;
        } else {
            // Mutation occurred - Result is the updated state
            // Return {nil, Result} since loops return nil but state was updated
            writeln!(self.output, "{{'nil', Result}}")?;
        }

        self.pop_scope();
        self.indent -= 1;

        // Restore REPL mode flag
        self.is_repl_mode = previous_is_repl_mode;

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }
}
