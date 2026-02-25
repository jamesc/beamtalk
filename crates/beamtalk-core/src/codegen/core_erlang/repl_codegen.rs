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

    /// Generates a REPL evaluation module for multiple expressions (BT-780).
    ///
    /// When given multiple expressions (period-separated REPL input like
    /// `x := 1. y := 2.`), generates an `eval/1` function that evaluates
    /// all expressions in sequence, threading the bindings State between them.
    ///
    /// Identifier assignments update State inline so subsequent expressions
    /// can reference newly-bound variables.
    ///
    /// Delegates to [`generate_repl_module`] for single-expression input.
    pub(super) fn generate_repl_module_multi(
        &mut self,
        expressions: &[Expression],
    ) -> Result<Document<'static>> {
        if expressions.len() == 1 {
            return self.generate_repl_module(&expressions[0]);
        }

        let previous_is_repl_mode = self.is_repl_mode;
        self.context = CodeGenContext::Repl;
        self.is_repl_mode = true;
        self.workspace_mode = true;

        let doc = self.generate_repl_multi_module_body(expressions)?;

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

        let return_tuple: Document<'static> = if self.repl_loop_mutated {
            // BT-483: Mutation-threaded control flow returns {Result, State} tuple.
            // Extract display value and updated bindings using element/2.
            // BT-245: repl_loop_mutated catches mutations inside StateAcc-threaded loops
            // where current_state_var() is restored after the loop.
            Document::Str(
                "let _LoopResult = call 'erlang':'element'(1, Result) in \
                 let _LoopState = call 'erlang':'element'(2, Result) in \
                 {_LoopResult, _LoopState}",
            )
        } else if final_state != "State" {
            // Direct state mutation (field assignment) — Result is the value, use updated state
            Document::String(format!("{{Result, {final_state}}}"))
        } else {
            // No mutation - Result is the value, State is unchanged bindings
            Document::Str("{Result, State}")
        };

        let module_name = &self.module_name;
        let doc = docvec![
            Document::String(format!("module '{module_name}' ['eval'/1]\n")),
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

    /// Multi-expression eval module body for BT-780.
    ///
    /// Generates a chained `eval/1` that evaluates each expression in sequence,
    /// threading the bindings State. Identifier assignments update State so that
    /// subsequent expressions can reference newly-bound variables.
    ///
    /// # Generated Code (for `x := 1. y := x + 1.`)
    ///
    /// ```erlang
    /// 'eval'/1 = fun (Bindings) ->
    ///     let State = Bindings in
    ///     let _R1 = 1 in
    ///     let State1 = call 'maps':'put'('x', _R1, State) in
    ///     let Result = call 'erlang':'+'(call 'maps':'get'('x', State1), 1) in
    ///     let State2 = call 'maps':'put'('y', Result, State1) in
    ///     {Result, State2}
    /// end
    /// ```
    fn generate_repl_multi_module_body(
        &mut self,
        expressions: &[Expression],
    ) -> Result<Document<'static>> {
        self.push_scope();
        self.bind_var("__bindings__", "Bindings");

        let n = expressions.len();
        let mut body_parts: Vec<Document<'static>> = Vec::new();

        // Process all but the last expression
        for (i, expr) in expressions[..n - 1].iter().enumerate() {
            // BT-790: Reset mutation flag before each intermediate expression so a loop in
            // intermediate position doesn't bleed its flag into subsequent expressions.
            self.repl_loop_mutated = false;
            let result_var = format!("_R{}", i + 1);
            let part = self.generate_repl_intermediate_expr(expr, &result_var)?;
            body_parts.push(part);
        }

        // BT-790: Reset mutation flag before the last expression so a loop in an intermediate
        // position doesn't incorrectly trigger element/2 unwrapping on the final return tuple.
        self.repl_loop_mutated = false;
        let last_expr = &expressions[n - 1];
        let (last_val_doc, return_tuple) = self.generate_repl_last_expr(last_expr)?;

        let module_name = self.module_name.clone();
        let mut all_parts: Vec<Document<'static>> = vec![
            Document::String(format!("module '{module_name}' ['eval'/1]\n")),
            Document::Str("  attributes []\n"),
            Document::Str("\n"),
            Document::Str("'eval'/1 = fun (Bindings) ->\n"),
            Document::Str("    let State = Bindings in\n"),
        ];
        all_parts.extend(body_parts);
        all_parts.push(Document::Str("    let Result = "));
        all_parts.push(last_val_doc);
        all_parts.push(Document::Str(" in\n    "));
        all_parts.push(return_tuple);
        all_parts.push(Document::Str("\nend\n"));

        self.pop_scope();

        Ok(Document::Vec(all_parts))
    }

    /// Generates code for a non-last expression in a multi-statement REPL sequence.
    ///
    /// For identifier assignments (`x := value`), generates:
    /// ```erlang
    /// let _Rx = <value> in let StateN = call 'maps':'put'('x', _Rx, StateN-1) in
    /// ```
    ///
    /// For other expressions, generates:
    /// ```erlang
    /// let _Rx = <expr> in
    /// ```
    fn generate_repl_intermediate_expr(
        &mut self,
        expr: &Expression,
        result_var: &str,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = id.name.clone();
                let current_state = self.current_state_var();
                let val_doc = self.expression_doc(value)?;
                let new_state = self.next_state_var();
                return Ok(docvec![
                    "    let ",
                    result_var.to_string(),
                    " = ",
                    val_doc,
                    Document::String(format!(
                        " in let {new_state} = call 'maps':'put'('{var_name}', {result_var}, {current_state}) in\n"
                    )),
                ]);
            }
        }
        // Non-assignment: evaluate for side effects, discard value.
        // BT-790: If the expression is a mutation-threaded loop, it returns {Result, StateAcc}.
        // Extract the updated StateAcc and thread it to subsequent expressions.
        let expr_doc = self.expression_doc(expr)?;
        if self.repl_loop_mutated {
            // BT-790: Loop with mutations returns {Result, StateAcc} — extract StateAcc and
            // thread it forward so subsequent expressions see the updated bindings.
            let new_state = self.next_state_var();
            Ok(docvec![
                "    let ",
                result_var.to_string(),
                " = ",
                expr_doc,
                Document::String(format!(
                    " in let {new_state} = call 'erlang':'element'(2, {result_var}) in\n"
                )),
            ])
        } else {
            Ok(docvec![
                "    let ",
                result_var.to_string(),
                " = ",
                expr_doc,
                " in\n",
            ])
        }
    }

    /// Generates the value and return tuple for the last expression in a
    /// multi-statement REPL sequence.
    ///
    /// For identifier assignments, also adds the binding to State before returning.
    /// For other expressions, uses the same mutation-detection logic as the
    /// single-expression path.
    fn generate_repl_last_expr(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, Document<'static>)> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = id.name.clone();
                let current_state = self.current_state_var();
                let val_doc = self.expression_doc(value)?;
                let new_state = self.next_state_var();
                let return_tuple = Document::String(format!(
                    "let {new_state} = call 'maps':'put'('{var_name}', Result, {current_state}) in {{Result, {new_state}}}"
                ));
                return Ok((val_doc, return_tuple));
            }
        }
        // Non-assignment: same logic as single-expression path
        let val_doc = self.expression_doc(expr)?;
        let final_state = self.current_state_var();
        let return_tuple: Document<'static> = if self.repl_loop_mutated {
            Document::Str(
                "let _LoopResult = call 'erlang':'element'(1, Result) in \
                 let _LoopState = call 'erlang':'element'(2, Result) in \
                 {_LoopResult, _LoopState}",
            )
        } else if final_state != "State" {
            Document::String(format!("{{Result, {final_state}}}"))
        } else {
            Document::Str("{Result, State}")
        };
        Ok((val_doc, return_tuple))
    }
}
