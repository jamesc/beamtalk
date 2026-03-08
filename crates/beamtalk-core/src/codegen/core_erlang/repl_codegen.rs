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
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::Expression;
use crate::docvec;

impl CoreErlangGenerator {
    /// Rejects destructuring assignments in REPL context (BT-1264).
    ///
    /// Destructuring works in compiled class methods and test-eval modules,
    /// but the REPL's state-map threading model requires dedicated codegen
    /// that hasn't been implemented yet.
    fn reject_repl_destructure(expr: &Expression) -> Result<()> {
        if matches!(expr, Expression::DestructureAssignment { .. }) {
            return Err(CodeGenError::UnsupportedFeature {
                feature: "Destructuring assignment is not yet supported in the REPL".to_string(),
                location: format!("{:?}", expr.span()),
            });
        }
        Ok(())
    }

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
        Self::reject_repl_destructure(expression)?;

        let previous_is_repl_mode = self.is_repl_mode;
        self.context = CodeGenContext::Repl;
        self.is_repl_mode = true;
        // BT-374 / ADR 0010: REPL runs in workspace context
        self.workspace_mode = true;

        let doc = self.generate_eval_module_body(expression)?;

        self.is_repl_mode = previous_is_repl_mode;
        Ok(doc)
    }

    /// Generates a REPL evaluation module in trace mode (BT-1238).
    ///
    /// Returns `{[{<<"source">>, Value}, ...], FinalState}` instead of `{Result, FinalState}`,
    /// giving the caller a value for every top-level statement.
    /// `source_texts` must have the same length as `expressions`.
    pub(super) fn generate_repl_module_multi_traced(
        &mut self,
        expressions: &[Expression],
        source_texts: &[String],
    ) -> Result<Document<'static>> {
        let previous_is_repl_mode = self.is_repl_mode;
        self.context = CodeGenContext::Repl;
        self.is_repl_mode = true;
        self.workspace_mode = true;

        let doc = if expressions.len() == 1 {
            self.generate_repl_single_traced(&expressions[0], &source_texts[0])?
        } else {
            self.generate_repl_multi_traced_body(expressions, source_texts)?
        };

        self.is_repl_mode = previous_is_repl_mode;
        Ok(doc)
    }

    /// Single-expression trace body.
    fn generate_repl_single_traced(
        &mut self,
        expression: &Expression,
        source_text: &str,
    ) -> Result<Document<'static>> {
        Self::reject_repl_destructure(expression)?;

        self.push_scope();
        self.bind_var("__bindings__", "Bindings");

        let expr_code = self.expression_doc(expression)?;
        let final_state = self.current_state_var();
        let src_bin = Self::binary_string_literal(source_text);

        let return_tuple: Document<'static> = if self.repl_loop_mutated {
            docvec![
                "let _LoopResult = call 'erlang':'element'(1, Result) in \
                 let _LoopState = call 'erlang':'element'(2, Result) in \
                 {[{",
                src_bin,
                ", _LoopResult}], _LoopState}",
            ]
        } else if final_state != "State" {
            docvec!["{[{", src_bin, ", Result}], ", final_state, "}"]
        } else {
            docvec!["{[{", src_bin, ", Result}], State}"]
        };

        let doc = docvec![
            "module '",
            self.module_name.clone(),
            "' ['eval'/1]\n",
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

    /// Multi-expression trace body (BT-1238).
    ///
    /// Mirrors `generate_repl_multi_module_body` but changes the return from
    /// `{Result, FinalState}` to `{[{<<"src0">>, _R1}, ..., {<<"srcN">>, Result}], FinalState}`.
    fn generate_repl_multi_traced_body(
        &mut self,
        expressions: &[Expression],
        source_texts: &[String],
    ) -> Result<Document<'static>> {
        self.push_scope();
        self.bind_var("__bindings__", "Bindings");

        let n = expressions.len();
        let mut body_parts: Vec<Document<'static>> = Vec::new();
        // (source_text, step_value_doc) pairs used to build the final steps list.
        let mut step_pairs: Vec<(String, Document<'static>)> = Vec::new();

        for (i, expr) in expressions[..n - 1].iter().enumerate() {
            self.repl_loop_mutated = false;
            let result_var = format!("_R{}", i + 1);
            let part = self.generate_repl_intermediate_expr(expr, &result_var)?;
            body_parts.push(part);
            // BT-790: When `repl_loop_mutated` is true, `result_var` holds `{Result, StateAcc}`.
            // Extract element 1 for the trace step so callers see the unwrapped value.
            let step_val: Document<'static> = if self.repl_loop_mutated {
                docvec!["call 'erlang':'element'(1, ", result_var, ")",]
            } else {
                Document::String(result_var)
            };
            step_pairs.push((source_texts[i].clone(), step_val));
        }

        self.repl_loop_mutated = false;
        let last_expr = &expressions[n - 1];
        let (last_val_doc, trace_return) =
            self.generate_trace_last_expr(last_expr, &source_texts[n - 1], step_pairs)?;

        let module_name = self.module_name.clone();
        let mut all_parts: Vec<Document<'static>> = vec![
            docvec!["module '", module_name, "' ['eval'/1]\n"],
            Document::Str("  attributes []\n"),
            Document::Str("\n"),
            Document::Str("'eval'/1 = fun (Bindings) ->\n"),
            Document::Str("    let State = Bindings in\n"),
        ];
        all_parts.extend(body_parts);
        all_parts.push(Document::Str("    let Result = "));
        all_parts.push(last_val_doc);
        all_parts.push(Document::Str(" in\n    "));
        all_parts.push(trace_return);
        all_parts.push(Document::Str("\nend\n"));

        self.pop_scope();
        Ok(Document::Vec(all_parts))
    }

    /// Generates the value doc and trace return expression for the last statement.
    ///
    /// Mirrors `generate_repl_last_expr` but returns `{StepsList, FinalState}` instead of
    /// `{Result, FinalState}`.
    fn generate_trace_last_expr(
        &mut self,
        expr: &Expression,
        source_text: &str,
        prev_steps: Vec<(String, Document<'static>)>,
    ) -> Result<(Document<'static>, Document<'static>)> {
        Self::reject_repl_destructure(expr)?;

        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = id.name.to_string();
                let current_state = self.current_state_var();
                let val_doc = self.expression_doc(value)?;
                let new_state = self.next_state_var();
                let mut all_steps = prev_steps;
                all_steps.push((source_text.to_string(), Document::Str("Result")));
                let steps_doc = Self::build_steps_list_doc(all_steps);
                let trace_return = docvec![
                    "let ",
                    new_state.clone(),
                    " = call 'maps':'put'('",
                    var_name,
                    "', Result, ",
                    current_state,
                    ") in {",
                    steps_doc,
                    ", ",
                    new_state,
                    "}",
                ];
                return Ok((val_doc, trace_return));
            }
        }

        let val_doc = self.expression_doc(expr)?;
        let final_state = self.current_state_var();

        let mut all_steps = prev_steps;
        let trace_return: Document<'static> = if self.repl_loop_mutated {
            all_steps.push((source_text.to_string(), Document::Str("_LoopTraceResult")));
            let steps_doc = Self::build_steps_list_doc(all_steps);
            let new_state = self.next_state_var();
            docvec![
                "let _LoopTraceResult = call 'erlang':'element'(1, Result) in \
                 let ",
                new_state.clone(),
                " = call 'erlang':'element'(2, Result) in \
                 {",
                steps_doc,
                ", ",
                new_state,
                "}",
            ]
        } else if final_state != "State" {
            all_steps.push((source_text.to_string(), Document::Str("Result")));
            let steps_doc = Self::build_steps_list_doc(all_steps);
            docvec!["{", steps_doc, ", ", final_state, "}"]
        } else {
            all_steps.push((source_text.to_string(), Document::Str("Result")));
            let steps_doc = Self::build_steps_list_doc(all_steps);
            docvec!["{", steps_doc, ", State}"]
        };

        Ok((val_doc, trace_return))
    }

    /// Builds a Core Erlang list literal from (`source_text`, `value_doc`) pairs.
    ///
    /// Produces: `[{<<"src0">>, V0} | [{<<"src1">>, V1} | ... | []]]`
    fn build_steps_list_doc(step_pairs: Vec<(String, Document<'static>)>) -> Document<'static> {
        let mut result: Document<'static> = Document::Str("[]");
        for (src, val_doc) in step_pairs.into_iter().rev() {
            let src_bin = Self::binary_string_literal(&src);
            result = docvec!["[{", src_bin, ", ", val_doc, "} | ", result, "]"];
        }
        result
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
            docvec!["{Result, ", final_state, "}"]
        } else {
            // No mutation - Result is the value, State is unchanged bindings
            Document::Str("{Result, State}")
        };

        let doc = docvec![
            "module '",
            self.module_name.clone(),
            "' ['eval'/1]\n",
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
            docvec!["module '", module_name, "' ['eval'/1]\n"],
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
        Self::reject_repl_destructure(expr)?;

        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = id.name.to_string();
                let current_state = self.current_state_var();
                let val_doc = self.expression_doc(value)?;
                let new_state = self.next_state_var();
                return Ok(docvec![
                    "    let ",
                    result_var.to_string(),
                    " = ",
                    val_doc,
                    " in let ",
                    new_state,
                    " = call 'maps':'put'('",
                    var_name,
                    "', ",
                    result_var.to_string(),
                    ", ",
                    current_state,
                    ") in\n",
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
                " in let ",
                new_state,
                " = call 'erlang':'element'(2, ",
                result_var.to_string(),
                ") in\n",
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
        Self::reject_repl_destructure(expr)?;

        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = id.name.to_string();
                let current_state = self.current_state_var();
                let val_doc = self.expression_doc(value)?;
                let new_state = self.next_state_var();
                let return_tuple = docvec![
                    "let ",
                    new_state.clone(),
                    " = call 'maps':'put'('",
                    var_name,
                    "', Result, ",
                    current_state,
                    ") in {Result, ",
                    new_state,
                    "}",
                ];
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
            docvec!["{Result, ", final_state, "}"]
        } else {
            Document::Str("{Result, State}")
        };
        Ok((val_doc, return_tuple))
    }
}
