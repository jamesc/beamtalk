// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL and test evaluation module code generation.
//!
//! **DDD Context:** Code Generation
//!
//! This module handles code generation for evaluation modules, creating
//! simple modules with an `eval/1` function that evaluates an expression
//! with the provided bindings map.

use super::document::Document;
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
    pub(super) fn generate_repl_module(
        &mut self,
        expression: &Expression,
    ) -> Result<Document<'static>> {
        let previous_is_repl_mode = self.is_repl_mode;
        self.context = CodeGenContext::Repl;
        self.is_repl_mode = true;
        // BT-374 / ADR 0010: REPL runs in workspace context
        self.workspace_mode = true;

        let doc = self.generate_eval_module_body(expression)?;

        self.is_repl_mode = previous_is_repl_mode;
        Ok(doc)
    }

    /// Generates a test evaluation module (no workspace bindings).
    ///
    /// Like [`generate_repl_module`] but with `workspace_mode = false`.
    /// Used by `beamtalk test-stdlib` for compiled expression tests (ADR 0014).
    pub(super) fn generate_test_module(
        &mut self,
        expression: &Expression,
    ) -> Result<Document<'static>> {
        let previous_is_repl_mode = self.is_repl_mode;
        self.context = CodeGenContext::Repl;
        self.is_repl_mode = true;
        self.workspace_mode = false;

        let doc = self.generate_eval_module_body(expression)?;

        self.is_repl_mode = previous_is_repl_mode;
        Ok(doc)
    }

    /// Common eval module body shared by REPL and test codegen.
    fn generate_eval_module_body(&mut self, expression: &Expression) -> Result<Document<'static>> {
        // Register Bindings in scope for variable lookups
        self.push_scope();
        self.bind_var("__bindings__", "Bindings");

        // Capture the expression output (ADR 0018 bridge)
        let expr_code = self.expression_doc(expression)?;

        // Check if state was mutated (must happen after capture)
        let final_state = self.current_state_var();

        let return_tuple = if self.repl_loop_mutated {
            // BT-483: Mutation-threaded control flow returns {Result, State} tuple.
            // Extract display value and updated bindings using element/2.
            // BT-245: repl_loop_mutated catches mutations inside StateAcc-threaded loops
            // where current_state_var() is restored after the loop.
            "let _LoopResult = call 'erlang':'element'(1, Result) in \
             let _LoopState = call 'erlang':'element'(2, Result) in \
             {_LoopResult, _LoopState}"
                .to_string()
        } else if final_state != "State" {
            // Direct state mutation (field assignment) — Result is the value, use updated state
            format!("{{Result, {final_state}}}")
        } else {
            // No mutation - Result is the value, State is unchanged bindings
            "{Result, State}".to_string()
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

        self.pop_scope();

        Ok(doc)
    }
}
