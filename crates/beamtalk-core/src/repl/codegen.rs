// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL and test evaluation module assembly (BT-1462).
//!
//! **DDD Context:** REPL
//!
//! This module handles the assembly of REPL evaluation modules, creating
//! `eval/1` functions that evaluate expressions with workspace bindings.
//! Expression compilation is delegated to the core codegen generator;
//! this module owns the REPL-specific concerns:
//!
//! - Module structure (`module '...' ['eval'/1]`)
//! - Workspace binding threading (`State = Bindings`, `maps:put` updates)
//! - Trace mode wrapping (per-expression `{<<"source">>, Value}` pairs)
//! - Multi-expression sequencing with state threading
//! - Test module generation (no workspace bindings)

use crate::ast::{Expression, Pattern};
use crate::codegen::core_erlang::document::Document;
use crate::codegen::core_erlang::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::docvec;

// ── Public API ──────────────────────────────────────────────────────────

/// Generates Core Erlang for a REPL expression.
///
/// This creates a simple module that evaluates a single expression and
/// returns its value. Used by the REPL for interactive evaluation.
///
/// The generated module has an `eval/1` function that takes a bindings map
/// and returns the expression result.
///
/// # Arguments
///
/// * `expression` - The Beamtalk expression AST to evaluate
/// * `module_name` - Unique module name (e.g., `repl_eval_42`)
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_repl_expression(expression: &Expression, module_name: &str) -> Result<String> {
    let mut generator = CoreErlangGenerator::new(module_name);
    let mut assembler = ReplAssembler::new(&mut generator);
    let doc = assembler.generate_repl_module(expression)?;
    Ok(doc.to_pretty_string())
}

/// Generates Core Erlang for multiple REPL expressions (BT-780).
///
/// Like [`generate_repl_expression`] but accepts a slice of expressions,
/// generating a single `eval/1` that evaluates all of them in sequence
/// and threads the bindings State between identifier assignments.
///
/// For a single expression, delegates to [`generate_repl_expression`].
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_repl_expressions(expressions: &[Expression], module_name: &str) -> Result<String> {
    generate_repl_expressions_with_index(expressions, module_name, std::collections::HashMap::new())
}

/// Generates Core Erlang for multiple REPL expressions with a class module index.
///
/// Like [`generate_repl_expressions`] but accepts a `class_module_index` that maps
/// class names to their compiled module names (e.g. `"Counter"` -> `"bt@getting_started@counter"`).
/// This is needed in workspace/package mode so that `compiled_module_name` resolves
/// class references correctly instead of falling back to the heuristic prefix.
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
#[allow(clippy::implicit_hasher)] // concrete HashMap matches internal generator field type
pub fn generate_repl_expressions_with_index(
    expressions: &[Expression],
    module_name: &str,
    class_module_index: std::collections::HashMap<String, String>,
) -> Result<String> {
    if expressions.is_empty() {
        return Err(CodeGenError::UnsupportedFeature {
            feature: "empty expression list".to_string(),
            span: None,
        });
    }
    let mut generator = CoreErlangGenerator::new(module_name);
    generator.set_class_module_index(class_module_index);
    let mut assembler = ReplAssembler::new(&mut generator);
    let doc = assembler.generate_repl_module_multi(expressions)?;
    Ok(doc.to_pretty_string())
}

/// Generates Core Erlang for trace mode eval (BT-1238).
///
/// Generates a single `eval/1` module that returns
/// `{[{<<"source0">>, Value0}, ...], FinalState}` instead of `{Result, FinalState}`,
/// giving the Erlang runtime a value for every top-level statement in one call.
/// Source texts are extracted from the input using expression spans.
///
/// Intended for the `eval` MCP tool's `trace: true` mode.
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
#[allow(clippy::implicit_hasher)]
pub fn generate_repl_expressions_traced(
    expressions: &[Expression],
    source: &str,
    module_name: &str,
    class_module_index: std::collections::HashMap<String, String>,
) -> Result<String> {
    if expressions.is_empty() {
        return Err(CodeGenError::UnsupportedFeature {
            feature: "empty expression list".to_string(),
            span: None,
        });
    }
    let source_texts: Vec<String> = expressions
        .iter()
        .map(|expr| {
            let span = expr.span();
            let start = span.start() as usize;
            let end = span.end() as usize;
            source.get(start..end).unwrap_or("").trim().to_string()
        })
        .collect();

    let mut generator = CoreErlangGenerator::new(module_name);
    generator.set_class_module_index(class_module_index);
    let mut assembler = ReplAssembler::new(&mut generator);
    let doc = assembler.generate_repl_module_multi_traced(expressions, &source_texts)?;
    Ok(doc.to_pretty_string())
}

/// Generates Core Erlang for a test expression (no workspace bindings).
///
/// Like [`generate_repl_expression`] but with `workspace_mode = false`,
/// suitable for compiled tests that don't need REPL/workspace context.
/// Used by `beamtalk test-stdlib` (ADR 0014 Phase 1).
///
/// # Errors
///
/// Returns [`CodeGenError`] if code generation fails.
pub fn generate_test_expression(expression: &Expression, module_name: &str) -> Result<String> {
    let mut generator = CoreErlangGenerator::new(module_name);
    let mut assembler = ReplAssembler::new(&mut generator);
    let doc = assembler.generate_test_module(expression)?;
    Ok(doc.to_pretty_string())
}

// ── REPL Assembly ───────────────────────────────────────────────────────

/// Assembles REPL evaluation modules around a core codegen generator.
///
/// This struct owns the REPL-specific assembly concerns (module structure,
/// binding threading, trace wrapping) and delegates expression compilation
/// to the wrapped [`CoreErlangGenerator`].
pub(crate) struct ReplAssembler<'a> {
    generator: &'a mut CoreErlangGenerator,
}

impl<'a> ReplAssembler<'a> {
    /// Creates a new REPL assembler wrapping the given generator.
    pub(crate) fn new(generator: &'a mut CoreErlangGenerator) -> Self {
        Self { generator }
    }

    /// Generates a REPL evaluation module (with workspace bindings).
    ///
    /// Creates a module with a single `eval/1` function that evaluates
    /// an expression with the provided bindings map.
    pub(crate) fn generate_repl_module(
        &mut self,
        expression: &Expression,
    ) -> Result<Document<'static>> {
        let previous_is_repl_mode = self.generator.is_repl_mode();
        self.generator.context = CodeGenContext::Repl;
        self.generator.set_is_repl_mode(true);
        // BT-374 / ADR 0010: REPL runs in workspace context
        self.generator.set_workspace_mode(true);

        let doc = self.generate_eval_module_body(expression)?;

        self.generator.set_is_repl_mode(previous_is_repl_mode);
        Ok(doc)
    }

    /// Generates a REPL evaluation module in trace mode (BT-1238).
    ///
    /// Returns `{[{<<"source">>, Value}, ...], FinalState}` instead of `{Result, FinalState}`,
    /// giving the caller a value for every top-level statement.
    /// `source_texts` must have the same length as `expressions`.
    pub(crate) fn generate_repl_module_multi_traced(
        &mut self,
        expressions: &[Expression],
        source_texts: &[String],
    ) -> Result<Document<'static>> {
        let previous_is_repl_mode = self.generator.is_repl_mode();
        self.generator.context = CodeGenContext::Repl;
        self.generator.set_is_repl_mode(true);
        self.generator.set_workspace_mode(true);

        let doc = if expressions.len() == 1 {
            self.generate_repl_single_traced(&expressions[0], &source_texts[0])?
        } else {
            self.generate_repl_multi_traced_body(expressions, source_texts)?
        };

        self.generator.set_is_repl_mode(previous_is_repl_mode);
        Ok(doc)
    }

    /// Single-expression trace body.
    fn generate_repl_single_traced(
        &mut self,
        expression: &Expression,
        source_text: &str,
    ) -> Result<Document<'static>> {
        self.generator.push_scope();
        self.generator.bind_var("__bindings__", "Bindings");

        // DestructureAssignment: generate binding chain + state updates, then build module.
        if let Expression::DestructureAssignment { pattern, value, .. } = expression {
            let (binding_docs, rhs_var) = self.generate_repl_destructure(pattern, value)?;
            let final_state = self.generator.current_state_var();
            let src_bin = CoreErlangGenerator::binary_string_literal(source_text);
            let module_name = self.generator.module_name.clone();
            let mut parts: Vec<Document<'static>> = vec![
                docvec!["module '", module_name, "' ['eval'/1]\n"],
                Document::Str("  attributes []\n"),
                Document::Str("\n"),
                Document::Str("'eval'/1 = fun (Bindings) ->\n"),
                Document::Str("    let State = Bindings in\n"),
            ];
            parts.extend(binding_docs);
            parts.push(docvec![
                "    let Result = ",
                Document::String(rhs_var),
                " in\n",
            ]);
            parts.push(docvec![
                "    {[{",
                src_bin,
                ", Result}], ",
                Document::String(final_state),
                "}\n",
            ]);
            parts.push(Document::Str("end\n"));
            self.generator.pop_scope();
            return Ok(Document::Vec(parts));
        }

        let (expr_code, repl_mutated) = self
            .generator
            .expression_doc_with_repl_mutation_tracking(expression)?;
        let final_state = self.generator.current_state_var();
        let src_bin = CoreErlangGenerator::binary_string_literal(source_text);

        let return_tuple: Document<'static> = if repl_mutated {
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
            self.generator.module_name.clone(),
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

        self.generator.pop_scope();
        Ok(doc)
    }

    /// Multi-expression trace body (BT-1238).
    fn generate_repl_multi_traced_body(
        &mut self,
        expressions: &[Expression],
        source_texts: &[String],
    ) -> Result<Document<'static>> {
        self.generator.push_scope();
        self.generator.bind_var("__bindings__", "Bindings");

        let n = expressions.len();
        let mut body_parts: Vec<Document<'static>> = Vec::new();
        let mut step_pairs: Vec<(String, Document<'static>)> = Vec::new();

        for (i, expr) in expressions[..n - 1].iter().enumerate() {
            let result_var = format!("_R{}", i + 1);
            let (part, repl_mutated) = self.generate_repl_intermediate_expr(expr, &result_var)?;
            body_parts.push(part);
            let step_val: Document<'static> = if repl_mutated {
                docvec!["call 'erlang':'element'(1, ", result_var, ")",]
            } else {
                Document::String(result_var)
            };
            step_pairs.push((source_texts[i].clone(), step_val));
        }

        let last_expr = &expressions[n - 1];
        let (pre_bindings, last_val_doc, trace_return) =
            self.generate_trace_last_expr(last_expr, &source_texts[n - 1], step_pairs)?;

        let module_name = self.generator.module_name.clone();
        let mut all_parts: Vec<Document<'static>> = vec![
            docvec!["module '", module_name, "' ['eval'/1]\n"],
            Document::Str("  attributes []\n"),
            Document::Str("\n"),
            Document::Str("'eval'/1 = fun (Bindings) ->\n"),
            Document::Str("    let State = Bindings in\n"),
        ];
        all_parts.extend(body_parts);
        all_parts.extend(pre_bindings);
        all_parts.push(Document::Str("    let Result = "));
        all_parts.push(last_val_doc);
        all_parts.push(Document::Str(" in\n    "));
        all_parts.push(trace_return);
        all_parts.push(Document::Str("\nend\n"));

        self.generator.pop_scope();
        Ok(Document::Vec(all_parts))
    }

    /// Generates the value doc and trace return expression for the last statement.
    fn generate_trace_last_expr(
        &mut self,
        expr: &Expression,
        source_text: &str,
        prev_steps: Vec<(String, Document<'static>)>,
    ) -> Result<(Vec<Document<'static>>, Document<'static>, Document<'static>)> {
        // DestructureAssignment: generate binding chain + state updates.
        if let Expression::DestructureAssignment { pattern, value, .. } = expr {
            let (binding_docs, rhs_var) = self.generate_repl_destructure(pattern, value)?;
            let final_state = self.generator.current_state_var();
            let mut all_steps = prev_steps;
            all_steps.push((source_text.to_string(), Document::Str("Result")));
            let steps_doc = Self::build_steps_list_doc(all_steps);
            let trace_return = docvec!["{", steps_doc, ", ", Document::String(final_state), "}"];
            return Ok((binding_docs, Document::String(rhs_var), trace_return));
        }

        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = id.name.to_string();
                let current_state = self.generator.current_state_var();
                let val_doc = self.generator.expression_doc(value)?;
                let new_state = self.generator.next_state_var();
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
                return Ok((Vec::new(), val_doc, trace_return));
            }
        }

        let (val_doc, repl_mutated) = self
            .generator
            .expression_doc_with_repl_mutation_tracking(expr)?;
        let final_state = self.generator.current_state_var();

        let mut all_steps = prev_steps;
        let trace_return: Document<'static> = if repl_mutated {
            all_steps.push((source_text.to_string(), Document::Str("_LoopTraceResult")));
            let steps_doc = Self::build_steps_list_doc(all_steps);
            let new_state = self.generator.next_state_var();
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

        Ok((Vec::new(), val_doc, trace_return))
    }

    /// Builds a Core Erlang list literal from (`source_text`, `value_doc`) pairs.
    ///
    /// Produces: `[{<<"src0">>, V0} | [{<<"src1">>, V1} | ... | []]]`
    fn build_steps_list_doc(step_pairs: Vec<(String, Document<'static>)>) -> Document<'static> {
        let mut result: Document<'static> = Document::Str("[]");
        for (src, val_doc) in step_pairs.into_iter().rev() {
            let src_bin = CoreErlangGenerator::binary_string_literal(&src);
            result = docvec!["[{", src_bin, ", ", val_doc, "} | ", result, "]"];
        }
        result
    }

    /// Generates a REPL evaluation module for multiple expressions (BT-780).
    pub(crate) fn generate_repl_module_multi(
        &mut self,
        expressions: &[Expression],
    ) -> Result<Document<'static>> {
        if expressions.len() == 1 {
            return self.generate_repl_module(&expressions[0]);
        }

        let previous_is_repl_mode = self.generator.is_repl_mode();
        self.generator.context = CodeGenContext::Repl;
        self.generator.set_is_repl_mode(true);
        self.generator.set_workspace_mode(true);

        let doc = self.generate_repl_multi_module_body(expressions)?;

        self.generator.set_is_repl_mode(previous_is_repl_mode);
        Ok(doc)
    }

    /// Generates a test evaluation module (no workspace bindings).
    ///
    /// Like [`generate_repl_module`] but with `workspace_mode = false`.
    /// Used by `beamtalk test-stdlib` for compiled expression tests (ADR 0014).
    pub(crate) fn generate_test_module(
        &mut self,
        expression: &Expression,
    ) -> Result<Document<'static>> {
        let previous_is_repl_mode = self.generator.is_repl_mode();
        self.generator.context = CodeGenContext::Repl;
        self.generator.set_is_repl_mode(true);
        self.generator.set_workspace_mode(false);

        let doc = self.generate_eval_module_body(expression)?;

        self.generator.set_is_repl_mode(previous_is_repl_mode);
        Ok(doc)
    }

    /// Common eval module body shared by REPL and test codegen.
    fn generate_eval_module_body(&mut self, expression: &Expression) -> Result<Document<'static>> {
        // Register Bindings in scope for variable lookups
        self.generator.push_scope();
        self.generator.bind_var("__bindings__", "Bindings");

        // DestructureAssignment: generate binding chain + state updates, then build module.
        if let Expression::DestructureAssignment { pattern, value, .. } = expression {
            let (binding_docs, rhs_var) = self.generate_repl_destructure(pattern, value)?;
            let final_state = self.generator.current_state_var();
            let module_name = self.generator.module_name.clone();
            let mut parts: Vec<Document<'static>> = vec![
                docvec!["module '", module_name, "' ['eval'/1]\n"],
                Document::Str("  attributes []\n"),
                Document::Str("\n"),
                Document::Str("'eval'/1 = fun (Bindings) ->\n"),
                Document::Str("    let State = Bindings in\n"),
            ];
            parts.extend(binding_docs);
            parts.push(docvec![
                "    let Result = ",
                Document::String(rhs_var),
                " in\n",
            ]);
            parts.push(docvec![
                "    {Result, ",
                Document::String(final_state),
                "}\n",
            ]);
            parts.push(Document::Str("end\n"));
            self.generator.pop_scope();
            return Ok(Document::Vec(parts));
        }

        // Capture the expression output (ADR 0018 bridge)
        let (expr_code, repl_mutated) = self
            .generator
            .expression_doc_with_repl_mutation_tracking(expression)?;

        // Check if state was mutated (must happen after capture)
        let final_state = self.generator.current_state_var();

        let return_tuple: Document<'static> = if repl_mutated {
            // BT-483: Mutation-threaded control flow returns {Result, State} tuple.
            // Extract display value and updated bindings using element/2.
            // BT-245: repl_mutated catches mutations inside StateAcc-threaded loops
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
            self.generator.module_name.clone(),
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

        self.generator.pop_scope();

        Ok(doc)
    }

    /// Multi-expression eval module body for BT-780.
    fn generate_repl_multi_module_body(
        &mut self,
        expressions: &[Expression],
    ) -> Result<Document<'static>> {
        self.generator.push_scope();
        self.generator.bind_var("__bindings__", "Bindings");

        let n = expressions.len();
        let mut body_parts: Vec<Document<'static>> = Vec::new();

        // Process all but the last expression
        for (i, expr) in expressions[..n - 1].iter().enumerate() {
            let result_var = format!("_R{}", i + 1);
            let (part, _repl_mutated) = self.generate_repl_intermediate_expr(expr, &result_var)?;
            body_parts.push(part);
        }

        let last_expr = &expressions[n - 1];
        let (pre_bindings, last_val_doc, return_tuple) = self.generate_repl_last_expr(last_expr)?;

        let module_name = self.generator.module_name.clone();
        let mut all_parts: Vec<Document<'static>> = vec![
            docvec!["module '", module_name, "' ['eval'/1]\n"],
            Document::Str("  attributes []\n"),
            Document::Str("\n"),
            Document::Str("'eval'/1 = fun (Bindings) ->\n"),
            Document::Str("    let State = Bindings in\n"),
        ];
        all_parts.extend(body_parts);
        all_parts.extend(pre_bindings);
        all_parts.push(Document::Str("    let Result = "));
        all_parts.push(last_val_doc);
        all_parts.push(Document::Str(" in\n    "));
        all_parts.push(return_tuple);
        all_parts.push(Document::Str("\nend\n"));

        self.generator.pop_scope();

        Ok(Document::Vec(all_parts))
    }

    /// Generates code for a non-last expression in a multi-statement REPL sequence.
    ///
    /// Returns `(doc, repl_mutated)` where `repl_mutated` indicates whether the
    /// expression was mutation-threaded (returned `{Result, StateAcc}`).
    fn generate_repl_intermediate_expr(
        &mut self,
        expr: &Expression,
        result_var: &str,
    ) -> Result<(Document<'static>, bool)> {
        if let Expression::DestructureAssignment { pattern, value, .. } = expr {
            let (binding_docs, rhs_var) = self.generate_repl_destructure(pattern, value)?;
            let mut result: Vec<Document<'static>> = binding_docs;
            result.push(docvec![
                "    let ",
                result_var.to_string(),
                " = ",
                Document::String(rhs_var),
                " in\n",
            ]);
            return Ok((Document::Vec(result), false));
        }

        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = id.name.to_string();
                let current_state = self.generator.current_state_var();
                let val_doc = self.generator.expression_doc(value)?;
                let new_state = self.generator.next_state_var();
                return Ok((
                    docvec![
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
                    ],
                    false,
                ));
            }
        }
        // Non-assignment: evaluate for side effects, discard value.
        let (expr_doc, repl_mutated) = self
            .generator
            .expression_doc_with_repl_mutation_tracking(expr)?;
        if repl_mutated {
            // BT-790: Loop with mutations returns {Result, StateAcc} — extract StateAcc and
            // thread it forward so subsequent expressions see the updated bindings.
            let new_state = self.generator.next_state_var();
            Ok((
                docvec![
                    "    let ",
                    result_var.to_string(),
                    " = ",
                    expr_doc,
                    " in let ",
                    new_state,
                    " = call 'erlang':'element'(2, ",
                    result_var.to_string(),
                    ") in\n",
                ],
                true,
            ))
        } else {
            Ok((
                docvec!["    let ", result_var.to_string(), " = ", expr_doc, " in\n",],
                false,
            ))
        }
    }

    /// Generates the value and return tuple for the last expression in a
    /// multi-statement REPL sequence.
    ///
    /// Returns `(pre_binding_docs, val_doc, return_tuple)`.
    fn generate_repl_last_expr(
        &mut self,
        expr: &Expression,
    ) -> Result<(Vec<Document<'static>>, Document<'static>, Document<'static>)> {
        if let Expression::DestructureAssignment { pattern, value, .. } = expr {
            let (binding_docs, rhs_var) = self.generate_repl_destructure(pattern, value)?;
            let final_state = self.generator.current_state_var();
            let return_tuple = docvec!["{Result, ", Document::String(final_state), "}"];
            return Ok((binding_docs, Document::String(rhs_var), return_tuple));
        }

        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = id.name.to_string();
                let current_state = self.generator.current_state_var();
                let val_doc = self.generator.expression_doc(value)?;
                let new_state = self.generator.next_state_var();
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
                return Ok((Vec::new(), val_doc, return_tuple));
            }
        }
        // Non-assignment: same logic as single-expression path
        let (val_doc, repl_mutated) = self
            .generator
            .expression_doc_with_repl_mutation_tracking(expr)?;
        let final_state = self.generator.current_state_var();
        let return_tuple: Document<'static> = if repl_mutated {
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
        Ok((Vec::new(), val_doc, return_tuple))
    }

    /// Generates REPL state-threading let-bindings for a destructuring assignment.
    ///
    /// Returns `(binding_docs, rhs_var)` where:
    /// - `binding_docs` is the flat chain of let-bindings for the RHS eval, optional
    ///   mutation unwrap, element extractions, and state updates
    /// - `rhs_var` is the temp variable holding the plain RHS value (used as display result)
    fn generate_repl_destructure(
        &mut self,
        pattern: &Pattern,
        value: &Expression,
    ) -> Result<(Vec<Document<'static>>, String)> {
        let mut docs: Vec<Document<'static>> = Vec::new();

        // Pre-evaluate the RHS with mutation tracking so we can detect and unwrap
        // mutation-threaded results ({Result, StateAcc}) before extraction.
        let raw_var = self.generator.fresh_temp_var("Rhs");
        let (val_doc, repl_mutated) = self
            .generator
            .expression_doc_with_repl_mutation_tracking(value)?;
        docs.push(docvec![
            "    let ",
            Document::String(raw_var.clone()),
            " = ",
            val_doc,
            " in\n",
        ]);

        // If the RHS was a mutation-threaded expression (e.g. a loop), it returned
        // {Result, StateAcc}. Hoist the unwrapped value and advance REPL state.
        let rhs_var = if repl_mutated {
            let value_var = self.generator.fresh_temp_var("RhsVal");
            let new_state = self.generator.next_state_var();
            docs.push(docvec![
                "    let ",
                Document::String(value_var.clone()),
                " = call 'erlang':'element'(1, ",
                Document::String(raw_var.clone()),
                ") in\n",
            ]);
            docs.push(docvec![
                "    let ",
                Document::String(new_state),
                " = call 'erlang':'element'(2, ",
                Document::String(raw_var),
                ") in\n",
            ]);
            value_var
        } else {
            raw_var
        };

        // Generate element extractions using the (possibly unwrapped) RHS value.
        let (extraction_docs, bound_pairs) = self
            .generator
            .generate_pattern_extractions_from_var(pattern, &rhs_var, "    let ", " in\n")?;
        docs.extend(extraction_docs);

        // Persist each bound variable to the REPL State map.
        for (name, core_var) in bound_pairs {
            let current_state = self.generator.current_state_var();
            let new_state = self.generator.next_state_var();
            docs.push(docvec![
                "    let ",
                Document::String(new_state),
                " = call 'maps':'put'('",
                Document::String(name),
                "', ",
                Document::String(core_var),
                ", ",
                Document::String(current_state),
                ") in\n",
            ]);
        }

        Ok((docs, rhs_var))
    }
}
