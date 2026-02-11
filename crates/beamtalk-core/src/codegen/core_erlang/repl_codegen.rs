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
use crate::docvec;

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
    /// Used by `beamtalk test-stdlib` for compiled expression tests (ADR 0014).
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
        // Register Bindings in scope for variable lookups
        self.push_scope();
        self.bind_var("__bindings__", "Bindings");

        // Capture the expression output (ADR 0018 bridge pattern)
        let expr_code = self.capture_expression(expression)?;

        // Check if state was mutated (must happen after capture)
        let final_state = self.current_state_var();

        let return_tuple = if final_state == "State" {
            // No mutation - Result is the value, State is unchanged bindings
            "{Result, State}".to_string()
        } else {
            // Mutation occurred - Result is the updated state
            // Return {nil, Result} since loops return nil but state was updated
            "{'nil', Result}".to_string()
        };

        let module_name = &self.module_name;
        let doc = docvec![
            format!("module '{module_name}' ['eval'/1]\n"),
            "  attributes []\n",
            "\n",
            "'eval'/1 = fun (Bindings) ->\n",
            "    let State = Bindings in\n",
            "    let Result = ",
            expr_code,
            " in\n",
            "    ",
            return_tuple,
            "\n",
            "end\n",
        ];

        self.write_document(&doc);

        self.pop_scope();

        Ok(())
    }
}
