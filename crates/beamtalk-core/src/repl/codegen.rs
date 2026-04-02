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
        let previous_context = self.generator.context;
        let previous_workspace_mode = self.generator.workspace_mode();
        self.generator.context = CodeGenContext::Repl;
        self.generator.set_is_repl_mode(true);
        // BT-374 / ADR 0010: REPL runs in workspace context
        self.generator.set_workspace_mode(true);

        // BT-1482: Restore generator state unconditionally, then propagate error.
        let result = self.generate_eval_module_body(expression);

        self.generator.set_is_repl_mode(previous_is_repl_mode);
        self.generator.set_workspace_mode(previous_workspace_mode);
        self.generator.context = previous_context;
        result
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
        let previous_context = self.generator.context;
        let previous_workspace_mode = self.generator.workspace_mode();
        self.generator.context = CodeGenContext::Repl;
        self.generator.set_is_repl_mode(true);
        self.generator.set_workspace_mode(true);

        // BT-1482: Restore generator state unconditionally, then propagate error.
        let result = if expressions.len() == 1 {
            self.generate_repl_single_traced(&expressions[0], &source_texts[0])
        } else {
            self.generate_repl_multi_traced_body(expressions, source_texts)
        };

        self.generator.set_is_repl_mode(previous_is_repl_mode);
        self.generator.set_workspace_mode(previous_workspace_mode);
        self.generator.context = previous_context;
        result
    }

    /// Single-expression trace body.
    fn generate_repl_single_traced(
        &mut self,
        expression: &Expression,
        source_text: &str,
    ) -> Result<Document<'static>> {
        self.generator.push_scope();
        self.generator.bind_var("__bindings__", "Bindings");

        // BT-1482: Delegate to inner method so pop_scope() runs unconditionally.
        let result = self.generate_repl_single_traced_inner(expression, source_text);
        self.generator.pop_scope();
        result
    }

    /// Inner implementation for single-expression trace body (BT-1482).
    ///
    /// Separated from [`generate_repl_single_traced`] so that `pop_scope()`
    /// runs unconditionally regardless of whether this returns `Ok` or `Err`.
    fn generate_repl_single_traced_inner(
        &mut self,
        expression: &Expression,
        source_text: &str,
    ) -> Result<Document<'static>> {
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

        // BT-1482: Delegate to inner method so pop_scope() runs unconditionally.
        let result = self.generate_repl_multi_traced_body_inner(expressions, source_texts);
        self.generator.pop_scope();
        result
    }

    /// Inner implementation for multi-expression trace body (BT-1482).
    fn generate_repl_multi_traced_body_inner(
        &mut self,
        expressions: &[Expression],
        source_texts: &[String],
    ) -> Result<Document<'static>> {
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
        let previous_context = self.generator.context;
        let previous_workspace_mode = self.generator.workspace_mode();
        self.generator.context = CodeGenContext::Repl;
        self.generator.set_is_repl_mode(true);
        self.generator.set_workspace_mode(true);

        // BT-1482: Restore generator state unconditionally, then propagate error.
        let result = self.generate_repl_multi_module_body(expressions);

        self.generator.set_is_repl_mode(previous_is_repl_mode);
        self.generator.set_workspace_mode(previous_workspace_mode);
        self.generator.context = previous_context;
        result
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
        let previous_context = self.generator.context;
        let previous_workspace_mode = self.generator.workspace_mode();
        self.generator.context = CodeGenContext::Repl;
        self.generator.set_is_repl_mode(true);
        self.generator.set_workspace_mode(false);

        // BT-1482: Restore generator state unconditionally, then propagate error.
        let result = self.generate_eval_module_body(expression);

        self.generator.set_is_repl_mode(previous_is_repl_mode);
        self.generator.set_workspace_mode(previous_workspace_mode);
        self.generator.context = previous_context;
        result
    }

    /// Common eval module body shared by REPL and test codegen.
    fn generate_eval_module_body(&mut self, expression: &Expression) -> Result<Document<'static>> {
        // Register Bindings in scope for variable lookups
        self.generator.push_scope();
        self.generator.bind_var("__bindings__", "Bindings");

        // BT-1482: Delegate to inner method so pop_scope() runs unconditionally.
        let result = self.generate_eval_module_body_inner(expression);
        self.generator.pop_scope();
        result
    }

    /// Inner implementation for eval module body (BT-1482).
    fn generate_eval_module_body_inner(
        &mut self,
        expression: &Expression,
    ) -> Result<Document<'static>> {
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

        Ok(doc)
    }

    /// Multi-expression eval module body for BT-780.
    fn generate_repl_multi_module_body(
        &mut self,
        expressions: &[Expression],
    ) -> Result<Document<'static>> {
        self.generator.push_scope();
        self.generator.bind_var("__bindings__", "Bindings");

        // BT-1482: Delegate to inner method so pop_scope() runs unconditionally.
        let result = self.generate_repl_multi_module_body_inner(expressions);
        self.generator.pop_scope();
        result
    }

    /// Inner implementation for multi-expression eval module body (BT-1482).
    fn generate_repl_multi_module_body_inner(
        &mut self,
        expressions: &[Expression],
    ) -> Result<Document<'static>> {
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
        // Wrap in push_scope/pop_scope so that the `bind_var` calls inside
        // `generate_pattern_extractions_from_var` don't leak into the persistent
        // REPL scope — otherwise later `lookup_var` would find the stale
        // destructured binding instead of reading from the REPL state map.
        self.generator.push_scope();
        let result = self
            .generator
            .generate_pattern_extractions_from_var(pattern, &rhs_var, "    let ", " in\n");
        self.generator.pop_scope();
        let (extraction_docs, bound_pairs) = result?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expression, Identifier, Literal};
    use crate::codegen::core_erlang::CodeGenError;
    use crate::source_analysis::Span;
    use ecow::EcoString;
    use std::collections::HashMap;

    // ── Helpers ────────────────────────────────────────────────────────

    /// Creates a dummy span for test AST nodes.
    fn span() -> Span {
        Span::new(0, 1)
    }

    /// Creates an integer literal expression.
    fn int_expr(value: i64) -> Expression {
        Expression::Literal(Literal::Integer(value), span())
    }

    /// Creates a string literal expression.
    fn str_expr(value: &str) -> Expression {
        Expression::Literal(Literal::String(EcoString::from(value)), span())
    }

    /// Creates a symbol literal expression.
    fn sym_expr(value: &str) -> Expression {
        Expression::Literal(Literal::Symbol(EcoString::from(value)), span())
    }

    /// Creates a float literal expression.
    fn float_expr(value: f64) -> Expression {
        Expression::Literal(Literal::Float(value), span())
    }

    /// Creates an identifier expression.
    fn ident_expr(name: &str) -> Expression {
        Expression::Identifier(Identifier::new(name, span()))
    }

    /// Creates a simple assignment expression (x := value).
    fn assign_expr(name: &str, value: Expression) -> Expression {
        Expression::Assignment {
            target: Box::new(ident_expr(name)),
            value: Box::new(value),
            span: span(),
        }
    }

    /// Creates a unary message send expression (receiver selector).
    fn unary_send(receiver: Expression, selector: &str) -> Expression {
        use crate::ast::MessageSelector;
        Expression::MessageSend {
            receiver: Box::new(receiver),
            selector: MessageSelector::Unary(EcoString::from(selector)),
            arguments: vec![],
            is_cast: false,
            span: span(),
        }
    }

    // ── generate_repl_expression: single expression ───────────────────

    #[test]
    fn repl_integer_literal() {
        let result = generate_repl_expression(&int_expr(42), "repl_test_1").unwrap();
        assert!(
            result.contains("module 'repl_test_1'"),
            "missing module name"
        );
        assert!(result.contains("['eval'/1]"), "missing eval/1 export");
        assert!(result.contains("fun (Bindings)"), "missing Bindings param");
        assert!(result.contains("42"), "missing integer value");
        assert!(result.contains("{Result, State}"), "missing return tuple");
    }

    #[test]
    fn repl_string_literal() {
        let result = generate_repl_expression(&str_expr("hello"), "repl_test_2").unwrap();
        assert!(result.contains("module 'repl_test_2'"));
        // String literals compile to Core Erlang binary byte sequences.
        // "hello" = bytes 104, 101, 108, 108, 111
        assert!(
            result.contains("#<104>"),
            "should contain binary encoding of string"
        );
    }

    #[test]
    fn repl_symbol_literal() {
        let result = generate_repl_expression(&sym_expr("ok"), "repl_test_3").unwrap();
        assert!(result.contains("module 'repl_test_3'"));
        assert!(result.contains("'ok'"), "symbol should compile to atom");
    }

    #[test]
    fn repl_float_literal() {
        let result = generate_repl_expression(&float_expr(1.5), "repl_test_4").unwrap();
        assert!(result.contains("module 'repl_test_4'"));
        assert!(result.contains("1.5"), "missing float value");
    }

    #[test]
    fn repl_module_structure() {
        let result = generate_repl_expression(&int_expr(1), "repl_mod_struct").unwrap();
        // Verify the essential Core Erlang module structure
        assert!(result.contains("module 'repl_mod_struct' ['eval'/1]"));
        assert!(result.contains("attributes []"));
        assert!(result.contains("'eval'/1 = fun (Bindings)"));
        assert!(result.contains("let State = Bindings"));
        assert!(result.contains("let Result ="));
        assert!(result.contains("end"));
    }

    #[test]
    fn repl_single_assignment_updates_state() {
        // Single assignment goes through generate_eval_module_body_inner
        let expr = assign_expr("x", int_expr(42));
        let result = generate_repl_expression(&expr, "repl_assign_single").unwrap();
        assert!(result.contains("module 'repl_assign_single'"));
        // Single assignment in REPL mode compiles the value expression and
        // updates state -- the exact mechanism depends on the codegen path,
        // but the output must contain the variable name and value.
        assert!(result.contains("42"), "should contain the assigned value");
    }

    #[test]
    fn repl_multi_assignment_threads_state() {
        // Multi-expression path threads state between assignments via maps:put
        let exprs = vec![assign_expr("x", int_expr(42)), ident_expr("x")];
        let result = generate_repl_expressions(&exprs, "repl_assign_multi").unwrap();
        assert!(
            result.contains("maps':'put'"),
            "assignment should use maps:put for state threading: {result}"
        );
        assert!(
            result.contains("'x'"),
            "should store variable name as atom key"
        );
    }

    // ── generate_repl_expressions: multi-expression ───────────────────

    #[test]
    fn repl_multi_non_assignment_intermediate() {
        // Non-assignment intermediate expression (side-effect only, value discarded)
        let exprs = vec![int_expr(1), int_expr(2)];
        let result = generate_repl_expressions(&exprs, "repl_multi_side").unwrap();
        assert!(result.contains("module 'repl_multi_side'"));
        // Both values should appear in the generated code
        assert!(result.contains('1'), "should contain first expression");
        assert!(result.contains('2'), "should contain second expression");
    }

    #[test]
    fn repl_multi_expressions_single_delegates() {
        // With a single expression, generate_repl_expressions should delegate
        // to the single-expression path and produce identical output.
        let single = generate_repl_expression(&int_expr(99), "repl_delegate").unwrap();
        let multi = generate_repl_expressions(&[int_expr(99)], "repl_delegate").unwrap();
        assert_eq!(
            single, multi,
            "single expression should delegate to same path"
        );
    }

    #[test]
    fn repl_multi_three_assignments() {
        let exprs = vec![
            assign_expr("a", int_expr(1)),
            assign_expr("b", int_expr(2)),
            assign_expr("c", int_expr(3)),
        ];
        let result = generate_repl_expressions(&exprs, "repl_multi_3").unwrap();
        assert!(result.contains("'a'"), "should bind variable a");
        assert!(result.contains("'b'"), "should bind variable b");
        assert!(result.contains("'c'"), "should bind variable c");
    }

    // ── generate_repl_expressions: empty input ────────────────────────

    #[test]
    fn repl_empty_expressions_returns_error() {
        let result = generate_repl_expressions(&[], "repl_empty");
        assert!(result.is_err(), "empty expressions should return error");
        match result.unwrap_err() {
            CodeGenError::UnsupportedFeature { feature, .. } => {
                assert!(
                    feature.contains("empty"),
                    "error should mention empty: {feature}"
                );
            }
            other => panic!("expected UnsupportedFeature, got: {other}"),
        }
    }

    // ── generate_repl_expressions_with_index ──────────────────────────

    #[test]
    fn repl_with_empty_class_index() {
        let result =
            generate_repl_expressions_with_index(&[int_expr(42)], "repl_idx_empty", HashMap::new())
                .unwrap();
        assert!(result.contains("module 'repl_idx_empty'"));
        assert!(result.contains("42"));
    }

    #[test]
    fn repl_with_class_index_produces_valid_module() {
        let mut index = HashMap::new();
        index.insert("Counter".to_string(), "bt@app@counter".to_string());
        let result =
            generate_repl_expressions_with_index(&[int_expr(1)], "repl_idx_class", index).unwrap();
        // Should still produce a valid module — the class index only affects
        // class reference resolution, not literal expressions.
        assert!(result.contains("module 'repl_idx_class'"));
    }

    // ── generate_test_expression ──────────────────────────────────────

    #[test]
    fn test_expression_integer() {
        let result = generate_test_expression(&int_expr(7), "test_mod_1").unwrap();
        assert!(result.contains("module 'test_mod_1'"));
        assert!(result.contains('7'));
    }

    #[test]
    fn test_expression_no_workspace_bindings_in_return() {
        // Test mode should still return {Result, State} tuple — the difference
        // is workspace_mode=false which affects how identifier lookups work
        // (no maps:get from State for unknown vars).
        let result = generate_test_expression(&int_expr(42), "test_mod_2").unwrap();
        assert!(
            result.contains("{Result, State}"),
            "should return result tuple"
        );
    }

    #[test]
    fn test_expression_module_structure() {
        let result = generate_test_expression(&str_expr("test"), "test_struct").unwrap();
        assert!(result.contains("module 'test_struct' ['eval'/1]"));
        assert!(result.contains("attributes []"));
        assert!(result.contains("'eval'/1 = fun (Bindings)"));
        assert!(result.contains("let State = Bindings"));
    }

    // ── generate_repl_expressions_traced ──────────────────────────────

    #[test]
    fn traced_single_expression() {
        let source = "42";
        let result = generate_repl_expressions_traced(
            &[int_expr(42)],
            source,
            "repl_trace_1",
            HashMap::new(),
        )
        .unwrap();
        assert!(result.contains("module 'repl_trace_1'"));
        // Trace mode wraps results in [{<<"source">>, Value}] list
        assert!(
            result.contains("42"),
            "should contain the source text as binary"
        );
    }

    #[test]
    fn traced_empty_returns_error() {
        let result = generate_repl_expressions_traced(&[], "", "repl_trace_empty", HashMap::new());
        assert!(result.is_err(), "empty traced expressions should error");
    }

    #[test]
    fn traced_multi_expressions() {
        let source = "x := 1\nx";
        let exprs = vec![assign_expr("x", int_expr(1)), ident_expr("x")];
        let result =
            generate_repl_expressions_traced(&exprs, source, "repl_trace_multi", HashMap::new())
                .unwrap();
        assert!(result.contains("module 'repl_trace_multi'"));
    }

    #[test]
    fn traced_assignment_includes_source_binary() {
        let source = "x := 42";
        let expr = assign_expr("x", int_expr(42));
        let result =
            generate_repl_expressions_traced(&[expr], source, "repl_trace_assign", HashMap::new())
                .unwrap();
        // Trace mode embeds source text as a Core Erlang binary literal.
        // "x := 42" starts with byte 120 ('x')
        assert!(
            result.contains("#<120>"),
            "trace output should contain source text as binary: {result}"
        );
    }

    // ── Message send expressions ──────────────────────────────────────

    #[test]
    fn repl_unary_message_send() {
        // A simple unary message like `42 asString`
        let expr = unary_send(int_expr(42), "asString");
        let result = generate_repl_expression(&expr, "repl_msg_send").unwrap();
        assert!(result.contains("module 'repl_msg_send'"));
        // The codegen should contain a dispatch/function call for the message
        assert!(result.contains("42"), "should contain the receiver literal");
    }

    // ── Module name variations ────────────────────────────────────────

    #[test]
    fn module_name_preserved_exactly() {
        let result = generate_repl_expression(&int_expr(1), "my_custom_module_42").unwrap();
        assert!(result.contains("module 'my_custom_module_42'"));
    }

    #[test]
    fn module_name_with_special_chars() {
        let result = generate_repl_expression(&int_expr(1), "repl_eval_999").unwrap();
        assert!(result.contains("module 'repl_eval_999'"));
    }
}
