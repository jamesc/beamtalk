// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL and test evaluation module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for evaluation modules, creating
//! simple modules with an `eval/1` function that evaluates an expression
//! with the provided bindings map.

use super::{CodeGenContext, CoreErlangGenerator, Result};
use crate::ast::Expression;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates a REPL evaluation module (with workspace bindings).
    ///
    /// Creates a module with a single `eval/1` function that evaluates
    /// an expression with the provided bindings map.
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
        let previous_is_repl_mode = self.is_repl_mode;
        self.context = CodeGenContext::Repl;
        self.is_repl_mode = true;
        // BT-374 / ADR 0010: REPL runs in workspace context
        self.workspace_mode = true;

        self.generate_eval_module_body(expression)?;

        self.is_repl_mode = previous_is_repl_mode;
        Ok(())
    }

    /// Generates a test evaluation module (no workspace bindings).
    ///
    /// Like [`generate_repl_module`] but with `workspace_mode = false`.
    /// Used by `beamtalk test` for compiled expression tests (ADR 0014).
    pub(super) fn generate_test_module(&mut self, expression: &Expression) -> Result<()> {
        let previous_is_repl_mode = self.is_repl_mode;
        self.context = CodeGenContext::Repl;
        self.is_repl_mode = true;
        self.workspace_mode = false;

        self.generate_eval_module_body(expression)?;

        self.is_repl_mode = previous_is_repl_mode;
        Ok(())
    }

    /// Common eval module body shared by REPL and test codegen.
    fn generate_eval_module_body(&mut self, expression: &Expression) -> Result<()> {
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

        // Module end
        writeln!(self.output, "end")?;

        Ok(())
    }
}
