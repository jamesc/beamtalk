// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method body code generation and class registration.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates method dispatch case clauses, method body with state threading
//! and reply tuples, and the `register_class/0` on-load function.

use super::super::document::{Document, INDENT, line, nest};
use super::super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis};
use crate::ast::{
    Block, ClassDefinition, ClassKind, Expression, Literal, MessageSelector, MethodDefinition,
    MethodKind, Module, StateDeclaration,
};
use crate::docvec;
use crate::unparse::unparse_method_display_signature;

/// Classification of how a method body expression should be handled for
/// state threading.  Produced by [`CoreErlangGenerator::classify_body_expr`]
/// and consumed by the unified [`CoreErlangGenerator::generate_body_exprs_with_reply`].
enum BodyExprKind {
    /// `^ value` — early return from method.
    EarlyReturn,
    /// `self fieldAt: name put: val` — reflective field mutation.
    SelfFieldAtPut,
    /// `self.field := value` — direct field assignment.
    FieldAssignment,
    /// `var := expr` where the RHS is a Tier 2 `value:` call.
    LocalAssignTier2,
    /// `var := expr` where the RHS is control flow with field mutations.
    LocalAssignControlFlow,
    /// `var := expr` — simple local assignment.
    LocalAssignPure,
    /// `{a, b} := expr` — destructure assignment.
    DestructureAssignment,
    /// `super method` — super message send.
    SuperSend,
    /// `self error: "..."` — never returns.
    ErrorSend,
    /// Tier 2 `value:` call — returns `{Result, NewState}`.
    Tier2ValueCall,
    /// Tier 2 self-send with stateful block arguments.
    Tier2SelfSend(Vec<(usize, Vec<String>)>),
    /// Control flow with field mutations — returns `{Result, State}`.
    ControlFlowWithMutations,
    /// `self userMethod` — dispatching self-send via `safe_dispatch` (BT-1420).
    DispatchingSelfSend,
    /// Regular expression with no special state-threading needs.
    Pure,
}

/// Tuple representing a method entry for `method_info` / `class_method_info` meta maps.
///
/// Fields: (`erlang_selector`, `arity`, `return_type`, `param_types`, `is_sealed`)
pub(super) type MethodInfoEntry = (String, usize, Option<String>, Vec<Option<String>>, bool);

impl CoreErlangGenerator {
    /// Generates dispatch case clauses for all methods in a class definition.
    pub(in crate::codegen::core_erlang) fn generate_class_method_dispatches(
        &mut self,
        class: &ClassDefinition,
        indent_level: isize,
    ) -> Result<Document<'static>> {
        // BT-851: Pre-scan for Tier 2 block parameters before generating method bodies
        self.scan_class_for_tier2_blocks(class);

        let mut docs = Vec::new();
        for method in &class.methods {
            // Only generate dispatch for primary methods for now
            if method.kind == MethodKind::Primary {
                docs.push(self.generate_method_dispatch(method, indent_level)?);
            }
        }
        Ok(Document::Vec(docs))
    }

    /// Generates a single method dispatch case clause.
    pub(in crate::codegen::core_erlang) fn generate_method_dispatch(
        &mut self,
        method: &MethodDefinition,
        indent_level: isize,
    ) -> Result<Document<'static>> {
        // Reset state version at the start of each method
        self.reset_state_version();

        // Push a new scope for this method's parameter bindings
        self.push_scope();
        // BT-295: Clear method params (will be populated below if present)
        self.current_method_params.clear();

        let selector_name = method.selector.name();

        // BT-295: Collect parameter variable names (mutates scope via fresh_var)
        let param_vars: Vec<String> = method
            .parameters
            .iter()
            .map(|p| {
                let var_name = self.fresh_var(&p.name.name);
                self.current_method_params.push(var_name.clone());
                var_name
            })
            .collect();

        // BT-851: Populate tier2_block_params for this method from pre-scanned info
        self.tier2_block_params.clear();
        let selector_name_for_t2 = selector_name.to_string();
        if let Some(positions) = self.tier2_method_info.get(&selector_name_for_t2).cloned() {
            for pos in &positions {
                if *pos < method.parameters.len() {
                    self.tier2_block_params
                        .insert(method.parameters[*pos].name.name.to_string());
                }
            }
        }

        // BT-761: Detect whether any block argument in this method body contains ^.
        // If so, set up a non-local return token so ^ inside blocks can throw to escape
        // the closure and return from the enclosing actor method.
        let needs_nlr = self
            .semantic_facts
            .has_block_nlr_or_walk(&method.span, &method.body);

        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.current_nlr_token = Some(token_var.clone());
            Some(token_var)
        } else {
            None
        };

        // Generate body as Document
        let method_body_doc = self.generate_method_definition_body_with_reply(method)?;

        self.current_nlr_token = None;

        // BT-761/BT-764: If NLR was detected, wrap body in a letrec function with
        // try/catch via the shared helper. letrec creates a genuine separate
        // function frame, avoiding BEAM validator ambiguous_catch_try_state
        // errors that arise when try/catch is nested inside case arms.
        // BT-774: Compose at Document level without intermediate string rendering.
        let method_body_doc = if let Some(ref token_var) = nlr_token_var {
            self.wrap_actor_body_with_nlr_catch(method_body_doc, token_var, true)
        } else {
            method_body_doc
        };

        // Build method clause as Document tree
        let has_params = !param_vars.is_empty();
        let body_doc: Document = if has_params {
            let params_pattern = param_vars.join(", ");
            docvec![
                "<'",
                Document::String(selector_name.to_string()),
                "'> when 'true' ->",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "case Args of",
                        nest(
                            INDENT,
                            docvec![
                                line(),
                                "<[",
                                Document::String(params_pattern),
                                "]> when 'true' ->",
                                nest(INDENT, docvec![line(), method_body_doc,]),
                                line(),
                                "<_> when 'true' -> {'reply', {'error', 'bad_arity'}, State}",
                            ]
                        ),
                        line(),
                        "end",
                    ]
                ),
                "\n",
            ]
        } else {
            docvec![
                "<'",
                Document::String(selector_name.to_string()),
                "'> when 'true' ->",
                nest(INDENT, docvec![line(), method_body_doc,]),
                "\n",
            ]
        };

        // Render at correct indent level
        let indent_spaces = indent_level * INDENT;
        #[allow(clippy::cast_sign_loss)] // indent_spaces is always non-negative
        let indent_str = " ".repeat(indent_spaces as usize);
        let result_doc = docvec![indent_str, nest(indent_spaces, body_doc)];

        // Pop the scope when done with this method
        self.pop_scope();

        Ok(result_doc)
    }

    /// Generates a method definition body wrapped in a reply tuple.
    ///
    /// For `MethodDefinition` nodes with explicit body expressions.
    /// Delegates to [`Self::generate_body_exprs_with_reply`].
    pub(in crate::codegen::core_erlang) fn generate_method_definition_body_with_reply(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<Document<'static>> {
        let body: Vec<&Expression> = method
            .body
            .iter()
            .map(|s| &s.expression)
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();
        self.generate_body_exprs_with_reply(&body, true)
    }

    /// Generates a block-based method body with state threading and reply tuple.
    ///
    /// For methods using block syntax with implicit returns.
    /// Delegates to [`Self::generate_body_exprs_with_reply`].
    pub(in crate::codegen::core_erlang) fn generate_method_body_with_reply(
        &mut self,
        block: &Block,
    ) -> Result<Document<'static>> {
        let body: Vec<&Expression> = block
            .body
            .iter()
            .map(|s| &s.expression)
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();
        self.generate_body_exprs_with_reply(&body, false)
    }

    // ── BT-1422: Unified method body state-threading ──────────────────

    /// Classify a body expression for state-threading dispatch.
    ///
    /// The order of checks matters: more specific patterns (e.g. field assignment)
    /// must come before general ones (e.g. pure expression).
    fn classify_body_expr(&self, expr: &Expression) -> BodyExprKind {
        // Early return — `^ value`
        if matches!(expr, Expression::Return { .. }) {
            return BodyExprKind::EarlyReturn;
        }

        // self fieldAt: name put: val
        if self.is_self_field_at_put(expr) {
            return BodyExprKind::SelfFieldAtPut;
        }

        // self.field := value
        if Self::is_field_assignment(expr) {
            return BodyExprKind::FieldAssignment;
        }

        // var := expr — sub-classify by RHS
        if Self::is_local_var_assignment(expr) {
            if let Expression::Assignment { value, .. } = expr {
                if self.is_tier2_value_call(value) {
                    return BodyExprKind::LocalAssignTier2;
                }
                if self.control_flow_has_mutations(value) {
                    return BodyExprKind::LocalAssignControlFlow;
                }
            }
            return BodyExprKind::LocalAssignPure;
        }

        // {a, b} := expr
        if matches!(expr, Expression::DestructureAssignment { .. }) {
            return BodyExprKind::DestructureAssignment;
        }

        // super send
        if Self::is_super_message_send(expr) {
            return BodyExprKind::SuperSend;
        }

        // self error: "..." — never returns
        if Self::is_error_message_send(expr) {
            return BodyExprKind::ErrorSend;
        }

        // Tier 2 value: call
        if self.is_tier2_value_call(expr) {
            return BodyExprKind::Tier2ValueCall;
        }

        // Tier 2 self-send with block args
        if let Some(tier2_args) = self.detect_tier2_self_send(expr) {
            return BodyExprKind::Tier2SelfSend(tier2_args);
        }

        // Control flow with field mutations
        if self.control_flow_has_mutations(expr) {
            return BodyExprKind::ControlFlowWithMutations;
        }

        // Dispatching self-send (BT-1420)
        if self.is_dispatching_actor_self_send(expr) {
            return BodyExprKind::DispatchingSelfSend;
        }

        BodyExprKind::Pure
    }

    /// Unified method body code generation with state threading and reply tuple.
    ///
    /// Both `generate_method_definition_body_with_reply` (for `MethodDefinition`)
    /// and `generate_method_body_with_reply` (for `Block`) delegate here.
    ///
    /// `supports_early_return` controls whether `^ value` expressions are handled.
    /// Method definitions support it; block bodies do not (NLR uses throw/catch).
    #[expect(
        clippy::too_many_lines,
        reason = "unified handler for all method body expression types with state threading"
    )]
    fn generate_body_exprs_with_reply(
        &mut self,
        body: &[&Expression],
        supports_early_return: bool,
    ) -> Result<Document<'static>> {
        if body.is_empty() {
            let state = self.current_state_var();
            return Ok(docvec!["{'reply', Self, ", state, "}"]);
        }

        // Phase 1: classify every expression upfront.  Classification is
        // stateless w.r.t. codegen state (state_version, variable bindings),
        // so pre-computing is safe and separates "what" from "how".
        let plan: Vec<BodyExprKind> = body
            .iter()
            .map(|expr| {
                let kind = self.classify_body_expr(expr);
                if matches!(&kind, BodyExprKind::EarlyReturn) && !supports_early_return {
                    BodyExprKind::Pure
                } else {
                    kind
                }
            })
            .collect();

        // Phase 2: emit code for each (expression, kind) pair.
        let mut docs: Vec<Document<'static>> = Vec::new();
        let body_len = body.len();

        for (i, (expr, kind)) in body.iter().zip(plan.into_iter()).enumerate() {
            let is_last = i == body_len - 1;
            let is_early_return = matches!(&kind, BodyExprKind::EarlyReturn);

            // Early return — always terminates generation regardless of position.
            if is_early_return && supports_early_return {
                if let Expression::Return { value, .. } = expr {
                    if self.control_flow_has_mutations(value) {
                        let value_str = self.expression_doc(value)?;
                        docs.push(self.emit_tuple_unpack_reply("Tuple", value_str));
                    } else {
                        let final_state = self.current_state_var();
                        let value_str = self.expression_doc(value)?;
                        docs.push(docvec![
                            "let _ReturnValue = ",
                            value_str,
                            " in {'reply', _ReturnValue, ",
                            Document::String(final_state),
                            "}",
                        ]);
                    }
                    return Ok(Document::Vec(docs));
                }
            }

            match kind {
                BodyExprKind::SelfFieldAtPut => {
                    let doc = self.generate_self_field_at_put_open(expr)?;
                    docs.push(doc);
                    if is_last {
                        let val_var = self
                            .last_open_scope_result
                            .clone()
                            .unwrap_or_else(|| "_".to_string());
                        let final_state = self.current_state_var();
                        docs.push(docvec![
                            "{'reply', ",
                            Document::String(val_var),
                            ", ",
                            Document::String(final_state),
                            "}",
                        ]);
                    }
                }
                BodyExprKind::FieldAssignment => {
                    if is_last {
                        if let Expression::Assignment { target, value, .. } = expr {
                            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                                let val_var = self.fresh_temp_var("Val");
                                let current_state = self.current_state_var();
                                let value_str = self.expression_doc(value)?;
                                let new_state = self.next_state_var();
                                docs.push(docvec![
                                    "let ",
                                    Document::String(val_var.clone()),
                                    " = ",
                                    value_str,
                                    " in let ",
                                    Document::String(new_state.clone()),
                                    " = call 'maps':'put'('",
                                    Document::String(field.name.to_string()),
                                    "', ",
                                    Document::String(val_var.clone()),
                                    ", ",
                                    Document::String(current_state),
                                    ") in {'reply', ",
                                    Document::String(val_var),
                                    ", ",
                                    Document::String(new_state),
                                    "}",
                                ]);
                            }
                        }
                    } else {
                        let doc = self.generate_field_assignment_open(expr)?;
                        docs.push(doc);
                    }
                }
                BodyExprKind::LocalAssignTier2 => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let tuple_var = self.fresh_temp_var("T2Tuple");
                            let value_str = self.expression_doc(value)?;
                            self.bind_var(var_name, &core_var);
                            let new_state = self.next_state_var();
                            docs.push(docvec![
                                "let ",
                                Document::String(tuple_var.clone()),
                                " = ",
                                value_str,
                                " in let ",
                                Document::String(core_var),
                                " = call 'erlang':'element'(1, ",
                                Document::String(tuple_var.clone()),
                                ")\n in let ",
                                Document::String(new_state),
                                " = call 'erlang':'element'(2, ",
                                Document::String(tuple_var),
                                ") in ",
                            ]);
                        }
                    }
                    if is_last {
                        self.emit_pure_reply(&mut docs);
                    }
                }
                BodyExprKind::LocalAssignControlFlow => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let tuple_var = self.fresh_temp_var("Tuple");
                            let new_state = self.peek_next_state_var();
                            let value_str = self.expression_doc(value)?;
                            let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                                "let ",
                                Document::String(tuple_var.clone()),
                                " = ",
                                value_str,
                                " in let ",
                                Document::String(core_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                Document::String(tuple_var.clone()),
                                ") in let ",
                                Document::String(new_state.clone()),
                                " = call 'erlang':'element'(2, ",
                                Document::String(tuple_var),
                                ") in ",
                            ]];
                            let _ = self.next_state_var();
                            self.bind_var(var_name, &core_var);

                            if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value)
                            {
                                for var in &threaded_vars {
                                    let tv_core = self.lookup_var(var).map_or_else(
                                        || Self::to_core_erlang_var(var),
                                        String::clone,
                                    );
                                    doc_parts.push(docvec![
                                        "let ",
                                        Document::String(tv_core),
                                        " = call 'maps':'get'('",
                                        Document::String(Self::local_state_key(var)),
                                        "', ",
                                        Document::String(new_state.clone()),
                                        ") in ",
                                    ]);
                                }
                            }
                            docs.push(Document::Vec(doc_parts));
                        }
                    }
                    if is_last {
                        self.emit_pure_reply(&mut docs);
                    }
                }
                BodyExprKind::LocalAssignPure => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let value_str = self.expression_doc(value)?;
                            self.bind_var(var_name, &core_var);
                            docs.push(docvec![
                                "let ",
                                Document::String(core_var),
                                " = ",
                                value_str,
                                " in ",
                            ]);
                        }
                    }
                    if is_last {
                        self.emit_pure_reply(&mut docs);
                    }
                }
                BodyExprKind::DestructureAssignment => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                        for d in binding_docs {
                            docs.push(d);
                        }
                    }
                    if is_last {
                        let post_state = self.current_state_var();
                        docs.push(docvec![
                            "{'reply', 'nil', ",
                            Document::String(post_state),
                            "}",
                        ]);
                    }
                }
                BodyExprKind::SuperSend => {
                    if is_last {
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(docvec![
                            "let _SuperTuple = ",
                            expr_str,
                            " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                            " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                            " in {'reply', _Result, _NewState}",
                        ]);
                    } else {
                        self.emit_super_send_open(expr, &mut docs)?;
                    }
                }
                BodyExprKind::ErrorSend => {
                    if is_last {
                        // Error send never returns — no reply tuple needed.
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(docvec![expr_str]);
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(docvec![
                            "let ",
                            Document::String(tmp_var),
                            " = ",
                            expr_str,
                            " in ",
                        ]);
                    }
                }
                BodyExprKind::Tier2ValueCall => {
                    if is_last {
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(self.emit_tuple_unpack_reply("T2Tuple", expr_str));
                    } else {
                        let tuple_var = self.fresh_temp_var("T2Tuple");
                        let discard_var = self.fresh_temp_var("T2Discard");
                        let expr_str = self.expression_doc(expr)?;
                        let new_state = self.next_state_var();
                        let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                            "let ",
                            Document::String(tuple_var.clone()),
                            " = ",
                            expr_str,
                            " in let ",
                            Document::String(discard_var),
                            " = call 'erlang':'element'(1, ",
                            Document::String(tuple_var.clone()),
                            ")\n in let ",
                            Document::String(new_state.clone()),
                            " = call 'erlang':'element'(2, ",
                            Document::String(tuple_var),
                            ") in ",
                        ]];

                        // BT-1213: Extract captured local mutations from NewState
                        if let Some(mutations) = Self::get_inline_block_captured_mutations(expr) {
                            for var in &mutations {
                                let core_var = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    Document::String(core_var),
                                    " = call 'maps':'get'('",
                                    Document::String(Self::local_state_key(var)),
                                    "', ",
                                    Document::String(new_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        docs.push(Document::Vec(doc_parts));
                    }
                }
                BodyExprKind::Tier2SelfSend(ref tier2_args) => {
                    let doc = self.generate_tier2_self_send_open(expr, tier2_args)?;
                    docs.push(doc);
                    if is_last {
                        self.emit_dispatch_reply(
                            &mut docs,
                            "missing dispatch var after Tier 2 self-send",
                        )?;
                    }
                }
                BodyExprKind::ControlFlowWithMutations => {
                    if is_last {
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(self.emit_tuple_unpack_reply("Tuple", expr_str));
                    } else {
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let new_state = self.peek_next_state_var();
                        let expr_str = self.expression_doc(expr)?;
                        let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                            "let ",
                            Document::String(tuple_var.clone()),
                            " = ",
                            expr_str,
                            " in let ",
                            Document::String(new_state.clone()),
                            " = call 'erlang':'element'(2, ",
                            Document::String(tuple_var),
                            ") in ",
                        ]];
                        let _ = self.next_state_var();

                        // Extract threaded locals from the updated state
                        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(expr) {
                            for var in &threaded_vars {
                                let core_var = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    Document::String(core_var),
                                    " = call 'maps':'get'('",
                                    Document::String(Self::local_state_key(var)),
                                    "', ",
                                    Document::String(new_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        docs.push(Document::Vec(doc_parts));
                    }
                }
                BodyExprKind::DispatchingSelfSend => {
                    let doc = self.generate_self_dispatch_open(expr)?;
                    docs.push(doc);
                    if is_last {
                        self.emit_dispatch_reply(
                            &mut docs,
                            "missing dispatch var after self-send",
                        )?;
                    }
                }
                BodyExprKind::EarlyReturn => {
                    return Err(CodeGenError::Internal(
                        "EarlyReturn should be handled before match dispatch".to_string(),
                    ));
                }
                BodyExprKind::Pure => {
                    if is_last {
                        let expr_str = self.expression_doc(expr)?;
                        let post_state = self.current_state_var();
                        docs.push(docvec![
                            "let _Result = ",
                            expr_str,
                            " in {'reply', _Result, ",
                            Document::String(post_state),
                            "}",
                        ]);
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(docvec![
                            "let ",
                            Document::String(tmp_var),
                            " = ",
                            expr_str,
                            " in ",
                        ]);
                    }
                }
            }
        }

        Ok(Document::Vec(docs))
    }

    /// Emit a generic `{'reply', _Result, State}` close for the last expression
    /// when the expression itself has already been emitted as an open let chain.
    /// Used by local assignments and other open-chain handlers in last position.
    fn emit_pure_reply(&mut self, docs: &mut Vec<Document<'static>>) {
        let post_state = self.current_state_var();
        docs.push(docvec![
            "{'reply', 'nil', ",
            Document::String(post_state),
            "}",
        ]);
    }

    /// Emit the last-position reply for a dispatch open call (Tier 2 self-send
    /// or dispatching self-send).  Reads `last_dispatch_var` and `current_state_var`.
    fn emit_dispatch_reply(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        invariant_msg: &str,
    ) -> Result<()> {
        let dispatch_var = self.last_dispatch_var.clone().ok_or_else(|| {
            CodeGenError::Internal(format!("invariant violation: {invariant_msg}"))
        })?;
        let final_state = self.current_state_var();
        docs.push(docvec![
            "{'reply', call 'erlang':'element'(1, ",
            Document::String(dispatch_var),
            "), ",
            Document::String(final_state),
            "}",
        ]);
        Ok(())
    }

    /// Emit the last-position reply for an expression that returns a
    /// `{Result, State}` tuple (Tier 2 value calls, control flow with
    /// mutations, early returns with mutations).
    fn emit_tuple_unpack_reply(
        &mut self,
        tuple_label: &str,
        expr_doc: Document<'static>,
    ) -> Document<'static> {
        let tuple_var = self.fresh_temp_var(tuple_label);
        docvec![
            "let ",
            Document::String(tuple_var.clone()),
            " = ",
            expr_doc,
            " in let _Result = call 'erlang':'element'(1, ",
            Document::String(tuple_var.clone()),
            ") in let _NewState = call 'erlang':'element'(2, ",
            Document::String(tuple_var),
            ") in {'reply', _Result, _NewState}",
        ]
    }

    /// Emit a super message send in non-last position, threading state.
    fn emit_super_send_open(
        &mut self,
        expr: &Expression,
        docs: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        let super_result_var = self.fresh_temp_var("SuperReply");
        let current_state = self.current_state_var();
        let new_state = self.next_state_var();
        let class_name = self.class_name();

        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            let selector_atom = selector.to_erlang_atom();
            let mut arg_docs: Vec<Document<'static>> = Vec::new();
            for (j, arg) in arguments.iter().enumerate() {
                if j > 0 {
                    arg_docs.push(Document::Str(", "));
                }
                arg_docs.push(self.expression_doc(arg)?);
            }
            docs.push(docvec![
                "let ",
                Document::String(super_result_var.clone()),
                " = call 'beamtalk_dispatch':'super'('",
                Document::String(selector_atom),
                "', [",
                Document::Vec(arg_docs),
                "], Self, ",
                Document::String(current_state),
                ", '",
                Document::String(class_name),
                "')",
            ]);
        }

        docs.push(docvec![
            " in let ",
            Document::String(new_state),
            " = call 'erlang':'element'(3, ",
            Document::String(super_result_var),
            ") in ",
        ]);
        Ok(())
    }

    /// BT-877: Detect the `new => self error: "..."` pattern that indicates a class
    /// is not constructible via `new`. Returns `true` if any method named `new` (unary)
    /// has a single-expression body that is `self error: <StringLiteral>`.
    fn has_raising_new(class: &ClassDefinition) -> bool {
        class
            .methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .filter(|m| m.selector == MessageSelector::Unary("new".into()))
            .any(|m| Self::is_self_error_body(&m.body))
    }

    /// Check if a method body is a single `self error: <StringLiteral>` expression.
    fn is_self_error_body(body: &[crate::ast::ExpressionStatement]) -> bool {
        if body.len() != 1 {
            return false;
        }
        matches!(
            &body[0].expression,
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
                && parts.len() == 1
                && parts[0].keyword == "error:"
                && arguments.len() == 1
                && matches!(&arguments[0], Expression::Literal(Literal::String(_), _))
        )
    }

    /// Generates the `register_class/0` on-load function using the `ClassBuilder`
    /// protocol (ADR 0038 Phase 3 / BT-837).
    ///
    /// This function is called automatically via `-on_load` when the module loads.
    /// Instead of calling `beamtalk_object_class:start/2` directly, it builds a
    /// `ClassBuilder` state map and calls `beamtalk_class_builder:register/1`.
    /// This routes all compiled class registration through the `ClassBuilder`
    /// protocol, which handles both first registration and hot reload.
    ///
    /// If `beamtalk_class_builder:register/1` raises, the exception is re-raised
    /// via `primop 'raw_raise'` so the BEAM `-on_load` mechanism reports a visible
    /// load failure rather than silently succeeding with an unregistered class (BT-998).
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'register_class'/0 = fun () ->
    ///     try
    ///         let _BuilderState0 = ~{
    ///             'className' => 'Counter',
    ///             'superclassRef' => 'Actor',
    ///             'moduleName' => 'class_definition',
    ///             'methodSource' => ~{...}~,
    ///             'methodSignatures' => ~{...}~,
    ///             'classMethodSignatures' => ~{}~,
    ///             'classState' => ~{}~,
    ///             'classDoc' => 'none',
    ///             'methodDocs' => ~{}~,
    ///             'meta' => ~{...}~
    ///         }~
    ///         in let _Reg0 = case call 'beamtalk_class_builder':'register'(_BuilderState0) of
    ///             <{'ok', _Pid0}> when 'true' -> 'ok'
    ///             <{'error', _Err0}> when 'true' -> {'error', _Err0}
    ///         end
    ///         in _Reg0
    ///     of RegResult -> RegResult
    ///     catch <CatchType, CatchError, CatchStack> ->
    ///         primop 'raw_raise'(CatchType, CatchError, CatchStack)
    /// ```
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_register_class(
        &mut self,
        module: &Module,
        synthesize_supervision_spec: bool,
    ) -> Result<Document<'static>> {
        // Skip if no class definitions
        if module.classes.is_empty() {
            return Ok(Document::Nil);
        }

        let mut class_docs = Vec::new();

        for (i, class) in module.classes.iter().enumerate() {
            // Instance methods — used for methodSource, methodSignatures, and methodDocs
            let instance_methods: Vec<_> = class
                .methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .collect();

            // BT-101: Method source
            let mut method_source_docs: Vec<Document<'static>> = Vec::new();
            for (method_idx, method) in instance_methods.iter().enumerate() {
                if method_idx > 0 {
                    method_source_docs.push(Document::Str(", "));
                }
                let source_str = self.extract_method_source(method);
                let binary = Self::binary_string_literal(&source_str);
                method_source_docs.push(docvec![
                    "'",
                    Document::String(method.selector.name().to_string()),
                    "' => ",
                    Document::String(binary),
                ]);
            }
            let method_source_doc = Document::Vec(method_source_docs);

            // BT-988: Method display signatures for :help command
            let mut method_sig_docs: Vec<Document<'static>> = Vec::new();
            for (method_idx, method) in instance_methods.iter().enumerate() {
                if method_idx > 0 {
                    method_sig_docs.push(Document::Str(", "));
                }
                let sig_str = unparse_method_display_signature(method);
                let binary = Self::binary_string_literal(&sig_str);
                method_sig_docs.push(docvec![
                    "'",
                    Document::String(method.selector.name().to_string()),
                    "' => ",
                    Document::String(binary),
                ]);
            }
            let method_sigs_doc = Document::Vec(method_sig_docs);

            // BT-990: Class-side method display signatures for :help command
            let mut class_method_sig_docs: Vec<Document<'static>> = Vec::new();
            for (method_idx, method) in class
                .class_methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .enumerate()
            {
                if method_idx > 0 {
                    class_method_sig_docs.push(Document::Str(", "));
                }
                let sig_str = unparse_method_display_signature(method);
                let binary = Self::binary_string_literal(&sig_str);
                class_method_sig_docs.push(docvec![
                    "'",
                    Document::String(method.selector.name().to_string()),
                    "' => ",
                    Document::String(binary),
                ]);
            }
            let class_method_sigs_doc = Document::Vec(class_method_sig_docs);

            // BT-412: Class variable initial values
            let mut class_var_parts: Vec<Document<'static>> = Vec::new();
            for (cv_idx, cv) in class.class_variables.iter().enumerate() {
                if cv_idx > 0 {
                    class_var_parts.push(Document::Str(", "));
                }
                let val = if let Some(ref default_value) = cv.default_value {
                    self.expression_doc(default_value)?
                } else {
                    Document::Str("'nil'")
                };
                class_var_parts.push(docvec![
                    "'",
                    Document::String(cv.name.name.to_string()),
                    "' => ",
                    val,
                ]);
            }
            let class_vars_doc = Document::Vec(class_var_parts);

            // BT-771: Class-level doc comment
            let class_doc_value: Document<'static> = if let Some(ref doc) = class.doc_comment {
                Document::String(Self::binary_string_literal(doc))
            } else {
                Document::Str("'none'")
            };

            // BT-771: Method-level doc comments
            let mut method_docs_parts: Vec<Document<'static>> = Vec::new();
            for method in &instance_methods {
                if let Some(ref doc) = method.doc_comment {
                    if !method_docs_parts.is_empty() {
                        method_docs_parts.push(Document::Str(", "));
                    }
                    let binary = Self::binary_string_literal(doc);
                    method_docs_parts.push(docvec![
                        "'",
                        Document::String(method.selector.name().to_string()),
                        "' => ",
                        Document::String(binary),
                    ]);
                }
            }
            let method_docs_doc = Document::Vec(method_docs_parts);

            // BT-877: Detect non-constructible classes at compile time.
            // Emit `isConstructible = false` for: abstract classes, actors, and
            // classes with `new => self error: "..."`. For all others, omit the key
            // so the runtime can fall back to lazy computation — this is needed
            // because primitive classes (String, Integer, etc.) have raising new/0
            // in Erlang, not in Beamtalk AST.
            let is_non_constructible = class.is_abstract
                || self.context == CodeGenContext::Actor
                || Self::has_raising_new(class);

            // ADR 0050 Phase 5: BuilderState carries only module/source/signature/doc metadata.
            // Static fields (flags, fields, method signatures) are read from __beamtalk_meta/0
            // by beamtalk_object_class:init/1.
            let class_doc = docvec![
                line(),
                "let _BuilderState",
                i,
                " = ~{",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        docvec![
                            "'className' => '",
                            Document::String(class.name.name.to_string()),
                            "',"
                        ],
                        line(),
                        docvec![
                            "'superclassRef' => '",
                            Document::String(class.superclass_name().to_string()),
                            "',"
                        ],
                        line(),
                        docvec![
                            "'moduleName' => '",
                            Document::String(self.module_name.clone()),
                            "',"
                        ],
                        line(),
                        "'methodSource' => ~{",
                        method_source_doc,
                        "}~,",
                        line(),
                        "'methodSignatures' => ~{",
                        method_sigs_doc,
                        "}~,",
                        line(),
                        "'classMethodSignatures' => ~{",
                        class_method_sigs_doc,
                        "}~,",
                        line(),
                        "'classState' => ~{",
                        class_vars_doc,
                        "}~,",
                        line(),
                        "'classDoc' => ",
                        class_doc_value,
                        ",",
                        line(),
                        "'methodDocs' => ~{",
                        method_docs_doc,
                        "}~,",
                        // ADR 0050 Phase 5: Include meta map in BuilderState so that
                        // beamtalk_object_class:init/1 can access it during on_load.
                        // erlang:function_exported/3 returns false during on_load execution,
                        // making Module:'__beamtalk_meta'() unavailable at registration time.
                        line(),
                        "'meta' => ",
                        // include_standalone: true — standalone methods included in BuilderState.meta
                        // so that init/1 can register their return types during on_load.
                        Self::build_meta_map_doc(class, module, true, synthesize_supervision_spec),
                        if is_non_constructible {
                            docvec![",", line(), "'isConstructible' => 'false'"]
                        } else {
                            Document::Nil
                        },
                        // BT-791: Emit stdlibMode flag for stdlib compilations so the
                        // runtime can bypass the sealed-superclass check in register/1.
                        // Character (extends sealed Integer) needs this to load correctly.
                        if self.stdlib_mode {
                            docvec![",", line(), "'stdlibMode' => 'true'"]
                        } else {
                            Document::Nil
                        },
                    ]
                ),
                line(),
                "}~",
                line(),
                "in let _Reg",
                i,
                " = case call 'beamtalk_class_builder':'register'(_BuilderState",
                i,
                ") of",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "<{'ok', _Pid",
                        i,
                        "}> when 'true' -> 'ok'",
                        line(),
                        "<{'error', _Err",
                        i,
                        "}> when 'true' -> {'error', _Err",
                        i,
                        "}",
                    ]
                ),
                line(),
                "end",
            ];
            class_docs.push(class_doc);
        }

        // BT-738 / BT-749: Build a short-circuit chain so that the first
        // {error, ...} from register/1 propagates out of on_load, regardless
        // of which class position caused it.
        //
        // For N classes, generates:
        //   let _BuilderState0 = ... in let _Reg0 = case ... end
        //   in case _Reg0 of
        //     <{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}
        //     <_> when 'true' ->
        //       let _BuilderState1 = ... in let _Reg1 = case ... end
        //       in _Reg1   % (or nested case if more classes follow)
        //   end
        let last_i = class_docs.len() - 1;
        // Start from innermost: just last class + final result
        let mut try_body: Document<'static> =
            docvec![class_docs[last_i].clone(), "\n", line(), "in _Reg", last_i,];
        // Wrap from second-to-last down to first, adding short-circuit cases
        for i in (0..last_i).rev() {
            try_body = docvec![
                class_docs[i].clone(),
                "\n",
                line(),
                "in case _Reg",
                i,
                " of",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "<{'error', _RegErr",
                        i,
                        "}> when 'true' -> {'error', _RegErr",
                        i,
                        "}",
                        line(),
                        "<_> when 'true' ->",
                        nest(INDENT, docvec![line(), try_body]),
                    ]
                ),
                line(),
                "end",
            ];
        }
        let doc = docvec![
            "'register_class'/0 = fun () ->",
            nest(
                INDENT,
                docvec![line(), "try", nest(INDENT, docvec![try_body, "\n",]),]
            ),
            nest(
                INDENT,
                docvec![
                    line(),
                    "of RegResult -> RegResult",
                    line(),
                    "catch <CatchType, CatchError, CatchStack> -> primop 'raw_raise'(CatchType, CatchError, CatchStack)",
                ]
            ),
            "\n\n",
        ];

        Ok(doc)
    }

    /// Generates standalone function bodies for class-side methods.
    ///
    /// Class methods are module-level functions with a `class_` prefix.
    /// They take `ClassSelf` as the first parameter (the class object),
    /// followed by any user-defined parameters.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'class_defaultValue'/1 = fun (ClassSelf) ->
    ///     42
    ///
    /// 'class_create'/1 = fun (ClassSelf) ->
    ///     let _Result = call 'beamtalk_object_class':'class_send'(
    ///         call 'erlang':'element'(4, ClassSelf), 'new:', [~{}~])
    ///     in _Result
    /// ```
    pub(in crate::codegen::core_erlang) fn generate_class_method_functions(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        // BT-412: Populate class variable names for field access validation
        self.class_var_names = class
            .class_variables
            .iter()
            .map(|cv| cv.name.name.to_string())
            .collect();

        // BT-412: Populate class method selectors for self-send routing
        self.class_method_selectors = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| m.selector.to_erlang_atom())
            .collect();

        // BT-996: Populate auto-generated keyword constructor selector for Value subclass: classes.
        // This allows `ClassName slot: value` inside a class method to route to the correct
        // class-side constructor instead of falling through to the instance-side getter.
        self.class_slot_constructor_selector =
            crate::codegen::core_erlang::value_type_codegen::compute_auto_slot_methods(class)
                .and_then(|auto| auto.keyword_constructor);

        let mut docs: Vec<Document<'static>> = Vec::new();

        for method in &class.class_methods {
            if method.kind != MethodKind::Primary {
                continue;
            }

            let selector_name = method.selector.name();
            // BT-412: +2 for ClassSelf and ClassVars parameters
            let arity = method.selector.arity() + 2;

            // Push scope for parameter bindings
            self.push_scope();
            self.current_method_params.clear();
            self.reset_state_version();
            self.class_var_version = 0;
            self.class_var_mutated = false;

            // Bind ClassSelf as 'self' in scope
            self.bind_var("self", "ClassSelf");
            self.in_class_method = true;

            // Collect parameter names (mutates scope via fresh_var)
            let param_vars: Vec<String> = method
                .parameters
                .iter()
                .map(|p| {
                    let var_name = self.fresh_var(&p.name.name);
                    self.current_method_params.push(var_name.clone());
                    var_name
                })
                .collect();

            // BT-1202: Detect if method body has ^ inside blocks (needs NLR).
            let needs_nlr = self
                .semantic_facts
                .has_block_nlr_or_walk(&method.span, &method.body);

            let nlr_token_var = if needs_nlr {
                let token_var = self.fresh_temp_var("NlrToken");
                self.current_nlr_token = Some(token_var.clone());
                Some(token_var)
            } else {
                None
            };

            // Generate body as Document and keep it in the Document pipeline (BT-875).
            let body_doc: Document<'static> = if method.body.is_empty() {
                self.current_nlr_token = None;
                // Empty class method body returns self (ClassSelf)
                docvec!["ClassSelf"]
            } else {
                let inner_doc = self.generate_class_method_body(method, &class.class_variables)?;
                self.current_nlr_token = None;
                // BT-1202: Use self.class_var_mutated (not just whether class vars are declared)
                // to preserve the {class_var_result, ...} contract. The normal path only wraps
                // in class_var_result when class vars were actually mutated; the NLR path must
                // match. class_var_mutated is set by generate_class_method_body when it sees a
                // class var assignment.
                let returns_class_var_result = self.class_var_mutated;
                if let Some(ref token_var) = nlr_token_var {
                    // BT-1202: Wrap body in try/catch to catch NLR throws from ^ inside blocks.
                    self.wrap_class_method_body_with_nlr_catch(
                        inner_doc,
                        token_var,
                        returns_class_var_result,
                    )
                } else {
                    inner_doc
                }
            };

            // Build function header with params
            let params_suffix = if param_vars.is_empty() {
                String::new()
            } else {
                format!(", {}", param_vars.join(", "))
            };

            let doc = docvec![
                "\n",
                "'class_",
                Document::String(selector_name.to_string()),
                "'/",
                Document::String(arity.to_string()),
                " = fun (ClassSelf, ClassVars",
                Document::String(params_suffix),
                ") ->",
                nest(INDENT, docvec![line(), body_doc,]),
                "\n",
            ];
            docs.push(doc);

            self.pop_scope();
            self.in_class_method = false;
        }
        self.class_var_names.clear();
        self.class_method_selectors.clear();
        self.class_slot_constructor_selector = None;
        Ok(Document::Vec(docs))
    }

    /// Generates the body of a class-side method.
    ///
    /// Unlike instance methods, class methods have no state threading.
    /// They simply evaluate expressions and return the last value.
    /// BT-412: If class variables were mutated, wraps the final result
    /// in `{class_var_result, Result, ClassVarsN}`.
    #[allow(clippy::too_many_lines)]
    fn generate_class_method_body(
        &mut self,
        method: &MethodDefinition,
        class_vars: &[crate::ast::StateDeclaration],
    ) -> Result<Document<'static>> {
        let has_class_vars = !class_vars.is_empty();

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Filter out @expect directives (compile-time only, no runtime representation).
        let body: Vec<&Expression> = method
            .body
            .iter()
            .map(|s| &s.expression)
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();
        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;

            if let Expression::Return { value, .. } = expr {
                if has_class_vars {
                    let result_var = self.fresh_temp_var("Ret");
                    let value_str = self.expression_doc(value)?;
                    if self.class_var_mutated {
                        let final_cv = self.current_class_var();
                        let doc = docvec![
                            "let ",
                            Document::String(result_var.clone()),
                            " = ",
                            value_str,
                            " in {'class_var_result', ",
                            Document::String(result_var),
                            ", ",
                            Document::String(final_cv),
                            "}",
                        ];
                        docs.push(doc);
                    } else {
                        let doc = docvec![
                            "let ",
                            Document::String(result_var.clone()),
                            " = ",
                            value_str,
                            " in ",
                            Document::String(result_var),
                        ];
                        docs.push(doc);
                    }
                } else {
                    let value_str = self.expression_doc(value)?;
                    docs.push(docvec![value_str]);
                }
                return Ok(Document::Vec(docs));
            }

            if is_last {
                if has_class_vars {
                    if self.is_class_var_assignment(expr) || self.is_class_method_self_send(expr) {
                        // Open-scope expression: generate it and close with class_var_result.
                        // The result var is stored in last_open_scope_result.
                        self.last_open_scope_result = None;
                        let expr_str = self.expression_doc(expr)?;
                        let final_cv = self.current_class_var();
                        if let Some(result_var) = &self.last_open_scope_result.clone() {
                            let doc = docvec![
                                expr_str,
                                "{'class_var_result', ",
                                Document::String(result_var.clone()),
                                ", ",
                                Document::String(final_cv),
                                "}",
                            ];
                            docs.push(doc);
                        } else {
                            // Fallback: shouldn't happen
                            let doc = docvec![
                                expr_str,
                                "{'class_var_result', 'nil', ",
                                Document::String(final_cv),
                                "}",
                            ];
                            docs.push(doc);
                        }
                    } else {
                        let result_var = self.fresh_temp_var("Ret");
                        // BT-1201: Clear before expression_doc so we only see open-scope results
                        // produced by THIS expression, not by a previous field assignment.
                        self.last_open_scope_result = None;
                        let expr_str = self.expression_doc(expr)?;
                        // BT-1201: If expression produced an open scope (e.g. `x := self classMethod`),
                        // close it with the open-scope result variable, then wrap.
                        if let Some(open_scope_result) = self.last_open_scope_result.take() {
                            if self.class_var_mutated {
                                let final_cv = self.current_class_var();
                                let doc = docvec![
                                    expr_str,
                                    "let ",
                                    Document::String(result_var.clone()),
                                    " = ",
                                    Document::String(open_scope_result),
                                    " in {'class_var_result', ",
                                    Document::String(result_var),
                                    ", ",
                                    Document::String(final_cv),
                                    "}",
                                ];
                                docs.push(doc);
                            } else {
                                docs.push(docvec![expr_str, Document::String(open_scope_result),]);
                            }
                        } else if self.class_var_mutated {
                            let final_cv = self.current_class_var();
                            let doc = docvec![
                                "let ",
                                Document::String(result_var.clone()),
                                " = ",
                                expr_str,
                                " in {'class_var_result', ",
                                Document::String(result_var),
                                ", ",
                                Document::String(final_cv),
                                "}",
                            ];
                            docs.push(doc);
                        } else {
                            let doc = docvec![
                                "let ",
                                Document::String(result_var.clone()),
                                " = ",
                                expr_str,
                                " in ",
                                Document::String(result_var),
                            ];
                            docs.push(doc);
                        }
                    }
                } else if self.is_class_method_self_send(expr) {
                    // BT-891: Class method self-send as last expression with no class vars.
                    // The generated code leaves an open scope ending with `in ` — we must
                    // close it with the unwrapped result variable.
                    self.last_open_scope_result = None;
                    let expr_str = self.expression_doc(expr)?;
                    if let Some(result_var) = &self.last_open_scope_result.clone() {
                        docs.push(docvec![expr_str, Document::String(result_var.clone()),]);
                    } else {
                        docs.push(docvec![expr_str]);
                    }
                } else {
                    // BT-1201: Clear before expression_doc so we only see open-scope results
                    // produced by THIS expression, not by a previous field assignment.
                    self.last_open_scope_result = None;
                    let expr_str = self.expression_doc(expr)?;
                    // BT-1201: If expression produced an open scope, close it with the result.
                    if let Some(open_scope_result) = self.last_open_scope_result.take() {
                        docs.push(docvec![expr_str, Document::String(open_scope_result)]);
                    } else {
                        docs.push(docvec![expr_str]);
                    }
                }
            } else if self.is_class_var_assignment(expr) || self.is_class_method_self_send(expr) {
                // Class var assignment or class method self-send: the generated code
                // ends with `in ` (open scope) so ClassVarsN stays visible for the
                // remaining body expressions.
                docs.push(self.generate_expression(expr)?);
            } else if Self::is_local_var_assignment(expr) {
                // BT-741: Local variable assignment — create a proper `let` binding so
                // subsequent expressions can reference the variable by name.
                // Without this, the variable is unbound in scope and falls through to
                // `generate_identifier`, which generates `call 'maps':'get'('d', State)`.
                // Class methods have no State parameter, causing {unbound_var, 'State', ...}.
                if let Expression::Assignment { target, value, .. } = expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = &id.name;
                        let core_var = self
                            .lookup_var(var_name)
                            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                        // BT-1201: Clear before expression_doc so we only see open-scope results
                        // produced by THIS expression, not by a previous field assignment.
                        self.last_open_scope_result = None;
                        let val_doc = self.expression_doc(value)?;
                        self.bind_var(var_name, &core_var);
                        // BT-1201: If the RHS produced an open scope (e.g., `x := self classMethod`),
                        // the doc ends with `in ` and the actual result is in last_open_scope_result.
                        // Emit the open scope first, then bind the variable to the result.
                        if let Some(open_scope_result) = self.last_open_scope_result.take() {
                            docs.push(docvec![
                                val_doc,
                                "let ",
                                Document::String(core_var),
                                " = ",
                                Document::String(open_scope_result),
                                " in "
                            ]);
                        } else {
                            docs.push(docvec![
                                "let ",
                                Document::String(core_var),
                                " = ",
                                val_doc,
                                " in "
                            ]);
                        }
                    }
                }
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    docs.push(d);
                }
            } else if self.is_do_with_vt_local_threading(expr) {
                // BT-1414: Non-last `do:` loop that mutates captured outer locals
                // in a class method. Generate foldl + extract locals as open let
                // chains so the updated values are visible to subsequent expressions.
                let doc = self.generate_value_type_do_open(expr)?;
                docs.push(doc);
            } else if self.is_conditional_with_vt_local_threading(expr) {
                // BT-1392: Non-last `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` that
                // mutates captured outer locals. Inline case + rebind.
                let doc = self.generate_vt_conditional_open(expr)?;
                docs.push(doc);
            } else {
                let tmp_var = self.fresh_temp_var("seq");
                let expr_str = self.expression_doc(expr)?;
                let doc = docvec!["let ", Document::String(tmp_var), " = ", expr_str, " in ",];
                docs.push(doc);
            }
        }
        Ok(Document::Vec(docs))
    }

    /// Extracts source text for a method using the AST unparser (BT-977).
    ///
    /// The unparser produces complete, comment-inclusive source for all methods,
    /// whether parsed from a `.bt` file or constructed programmatically by a live
    /// tool (synthesized methods have no source text but still produce valid output).
    ///
    /// Previously this used raw byte-range slicing (`source[span.start..span.end]`),
    /// which silently dropped leading comments (they appear before `method.span.start()`)
    /// and fell back to the selector name for synthesized methods. The unparser fixes
    /// both deficiencies — see ADR 0044 Phase 4.
    #[allow(clippy::unused_self)]
    pub(super) fn extract_method_source(&self, method: &MethodDefinition) -> String {
        crate::unparse::unparse_method(method)
    }

    /// BT-851: Checks if an expression is a `value:` call on a Tier 2 block parameter.
    ///
    /// When true, the expression will generate a `{Result, NewState}` tuple via
    /// `generate_block_value_call_stateful()` and must be unpacked by the caller.
    pub(in crate::codegen::core_erlang) fn is_tier2_value_call(&self, expr: &Expression) -> bool {
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            let is_value_selector = match selector {
                crate::ast::MessageSelector::Unary(name) => name == "value",
                crate::ast::MessageSelector::Keyword(parts) => {
                    let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                    matches!(
                        selector_name.as_str(),
                        "value:" | "value:value:" | "value:value:value:"
                    )
                }
                crate::ast::MessageSelector::Binary(_) => false,
            };
            if is_value_selector {
                // BT-851: Tier 2 block parameter (variable holding a stateful block)
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if self.tier2_block_params.contains(id.name.as_str()) {
                        return true;
                    }
                }
                // BT-1213: Inline block literal with captured mutations
                // (e.g. [errors := errors add: #foo] value)
                // Only in Actor/REPL context — ValueType inlines as plain value (no tuple).
                if let Expression::Block(block) = receiver.as_ref() {
                    if self.context != super::super::CodeGenContext::ValueType
                        && !Self::captured_mutations_for_block(block).is_empty()
                    {
                        return true;
                    }
                }
            }
        }
        false
    }

    // BT-1213: Delegates to the shared implementation in expressions.rs.
    fn get_inline_block_captured_mutations(expr: &Expression) -> Option<Vec<String>> {
        Self::inline_block_captured_mutations(expr)
    }

    /// Checks if a control flow expression actually threads state through mutations.
    ///
    /// This goes beyond mere selector-based classification by analysing whether
    /// the block argument(s) contain mutations that require state threading.
    ///
    /// Returns `true` only if:
    /// 1. The expression is a `ControlFlow` dispatch (from pre-computed `dispatch_kinds`),
    ///    or — when semantic facts are unavailable — the selector matches a known
    ///    exception/conditional selector as a fallback.
    /// 2. The relevant block argument(s) need state threading in the current context
    ///    (checked via `needs_mutation_threading` on pre-computed `block_profiles`).
    ///
    /// Using pre-computed `dispatch_kinds` and `block_profiles` avoids the repeated
    /// selector-based re-classification and `analyze_block` calls that the original
    /// implementation performed (BT-1309).
    pub(in crate::codegen::core_erlang) fn control_flow_has_mutations(
        &self,
        expr: &Expression,
    ) -> bool {
        let Expression::MessageSend {
            receiver,
            arguments,
            selector: crate::ast::MessageSelector::Keyword(parts),
            span,
            ..
        } = expr
        else {
            return false;
        };

        // Use pre-computed dispatch classification instead of re-deriving it.
        // When semantic_facts is empty (e.g. in unit tests constructed via
        // `CoreErlangGenerator::new`), `dispatch_kind` returns `Unknown`.
        // In that case fall back to local selector-based classification so the
        // function still returns the correct result for known control-flow
        // selectors rather than silently returning `false` for all of them.
        let dispatch_kind = self.semantic_facts.dispatch_kind(span);
        let sel_str: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let is_control_flow = match dispatch_kind {
            crate::semantic_analysis::DispatchKind::ControlFlow => true,
            crate::semantic_analysis::DispatchKind::Unknown => {
                crate::state_threading_selectors::is_exception_selector(sel_str.as_str())
                    || crate::state_threading_selectors::is_conditional_selector(sel_str.as_str())
            }
            _ => false,
        };
        if !is_control_flow {
            return false;
        }

        // BT-410: For on:do: and ensure:, the receiver (try body) is also
        // a block that may contain field mutations.
        if crate::state_threading_selectors::is_exception_selector(sel_str.as_str()) {
            if let Expression::Block(block) = receiver.as_ref() {
                // Use pre-computed block profile when available.
                let analysis = self
                    .semantic_facts
                    .block_profile(&block.span)
                    .cloned()
                    .unwrap_or_else(|| block_analysis::analyze_block(block));
                if self.needs_mutation_threading(&analysis) {
                    return true;
                }
            }
        }

        // BT-915: For Boolean conditionals, any block argument may contain mutations.
        // BT-1226: ifNotNil: also needs per-block mutation detection.
        if crate::state_threading_selectors::is_conditional_selector(sel_str.as_str()) {
            for arg in arguments {
                if let Expression::Block(block) = arg {
                    let analysis = self
                        .semantic_facts
                        .block_profile(&block.span)
                        .cloned()
                        .unwrap_or_else(|| block_analysis::analyze_block(block));
                    if self.needs_mutation_threading(&analysis) {
                        return true;
                    }
                }
            }
            return false;
        }

        // Standard check: analyse the last argument block.
        if let Some(Expression::Block(block)) = arguments.last() {
            let analysis = self
                .semantic_facts
                .block_profile(&block.span)
                .cloned()
                .unwrap_or_else(|| block_analysis::analyze_block(block));
            if self.needs_mutation_threading(&analysis) {
                return true;
            }
            // BT-1329: Also check for nested list ops with cross-scope mutations.
            // `analyze_block` doesn't propagate local_writes from nested blocks,
            // so variables mutated inside do:/collect:/inject:/select:/reject: blocks
            // are invisible to the standard `needs_mutation_threading` check.
            if self.body_has_list_op_cross_scope_mutations(block) {
                return true;
            }
        }

        false
    }

    /// Generates the `__beamtalk_meta/0` function (BT-942).
    ///
    /// Embeds static reflection metadata directly in the compiled BEAM module.
    /// This enables zero-process reflection queries for structural data:
    /// class name, superclass, fields, instance methods, and class methods.
    ///
    /// Dynamic classes created via `beamtalk_class_builder` do not have this function;
    /// the runtime falls back to `gen_server` calls when `erlang:function_exported/3` (BIF)
    /// returns false.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// '__beamtalk_meta'/0 = fun () ->
    ///     ~{'class' => 'Counter',
    ///       'superclass' => 'Actor',
    ///       'fields' => ['value'],
    ///       'methods' => [{'increment', 0}, {'decrement', 0}, {'getValue', 0}],
    ///       'class_methods' => [{'new', 0}]
    ///     }~
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_meta_function(
        &self,
        module: &Module,
        synthesize_supervision_spec: bool,
    ) -> Result<Document<'static>> {
        let Some(class) = module.classes.first() else {
            return Ok(Document::Nil);
        };

        Ok(docvec![
            "'__beamtalk_meta'/0 = fun () ->\n",
            "    ",
            // include_standalone: false — standalone methods are runtime-patched, not static
            Self::build_meta_map_doc(class, module, false, synthesize_supervision_spec),
            "\n\n",
        ])
    }

    /// Builds the Core Erlang map document for the static class metadata.
    ///
    /// Used by both `generate_meta_function` (for `__beamtalk_meta/0`) and
    /// `generate_register_class` (for the `'meta'` key in `BuilderState`).
    ///
    /// ADR 0050 Phase 5: `erlang:function_exported/3` returns `false` during `on_load`,
    /// so `__beamtalk_meta/0` cannot be called from within the `on_load` callback chain.
    /// Including this map literal in `BuilderState` makes the data available during `init/1`.
    ///
    /// When `include_standalone` is `false` (used for `__beamtalk_meta/0`), standalone
    /// Tonel-style methods (`module.method_definitions`) are excluded — they are
    /// runtime-patched and deliberately absent from the static meta. When `true`
    /// (used for `BuilderState.meta`), standalone methods are included so that
    /// return-type information is available to `init/1` during `on_load`.
    pub(super) fn build_meta_map_doc(
        class: &ClassDefinition,
        module: &Module,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
    ) -> Document<'static> {
        Self::build_meta_map_doc_with_extra(
            class,
            module,
            include_standalone,
            synthesize_supervision_spec,
            Document::Nil,
        )
    }

    /// Like `build_meta_map_doc` but appends extra map entries before closing the map.
    ///
    /// Used by native facade codegen to add `'native'` and `'backing_module'` keys
    /// while reusing the standard meta map structure.
    pub(super) fn build_meta_map_doc_with_extra(
        class: &ClassDefinition,
        module: &Module,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
        extra_entries: Document<'static>,
    ) -> Document<'static> {
        let class_name = class.name.name.to_string();
        let superclass_name = class
            .superclass
            .as_ref()
            .map_or_else(|| "nil".to_string(), |s| s.name.to_string());

        // Build fields list from instance state declarations
        let fields: Vec<String> = class
            .state
            .iter()
            .map(|s| s.name.name.to_string())
            .collect();

        let fields_doc = Self::meta_atom_list(&fields);

        // Boolean flags
        let is_sealed_doc = Self::meta_bool(class.is_sealed);
        let is_abstract_doc = Self::meta_bool(class.is_abstract);
        let is_value_doc = Self::meta_bool(class.class_kind == ClassKind::Value);
        let is_typed_doc = Self::meta_bool(class.is_typed);

        // field_types: map of field name → declared type atom or 'none'
        let field_types_doc = Self::meta_field_types_map(&class.state);

        // Compute auto-slot methods once and share across method_info / class_method_info
        let auto =
            crate::codegen::core_erlang::value_type_codegen::compute_auto_slot_methods(class);
        let method_info_doc = Self::meta_method_info_map(&Self::meta_instance_method_entries(
            class,
            module,
            auto.as_ref(),
            include_standalone,
        ));
        let class_method_info_doc = Self::meta_method_info_map(&Self::meta_class_method_entries(
            class,
            module,
            auto.as_ref(),
            include_standalone,
            synthesize_supervision_spec,
        ));

        docvec![
            "~{'class' => '",
            Document::String(class_name),
            "',\n      'superclass' => '",
            Document::String(superclass_name),
            "',\n      'fields' => ",
            fields_doc,
            ",\n      'is_sealed' => ",
            is_sealed_doc,
            ",\n      'is_abstract' => ",
            is_abstract_doc,
            ",\n      'is_value' => ",
            is_value_doc,
            ",\n      'is_typed' => ",
            is_typed_doc,
            ",\n      'field_types' => ",
            field_types_doc,
            ",\n      'method_info' => ",
            method_info_doc,
            ",\n      'class_method_info' => ",
            class_method_info_doc,
            extra_entries,
            "\n    }~",
        ]
    }

    /// Builds a Core Erlang atom list document from a slice of string names.
    ///
    /// Example: `["field1", "field2"]` → `['field1', 'field2']`
    /// Empty slice → `[]`
    pub(super) fn meta_atom_list(names: &[String]) -> Document<'static> {
        if names.is_empty() {
            return Document::Str("[]");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("["));
        for (i, name) in names.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(docvec!["'", Document::String(name.clone()), "'"]);
        }
        parts.push(Document::Str("]"));
        Document::Vec(parts)
    }

    /// Produces the Core Erlang atom for a boolean value.
    pub(super) fn meta_bool(b: bool) -> Document<'static> {
        if b {
            Document::Str("'true'")
        } else {
            Document::Str("'false'")
        }
    }

    /// Builds a Core Erlang map of field name → declared type atom or `'none'`.
    ///
    /// Example: `[StateDecl{name: "value", type: Integer}]` → `~{'value' => 'Integer'}~`
    /// Empty slice → `~{}~`
    pub(super) fn meta_field_types_map(state: &[StateDeclaration]) -> Document<'static> {
        if state.is_empty() {
            return Document::Str("~{}~");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("~{"));
        for (i, s) in state.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let type_doc = match &s.type_annotation {
                Some(ta) => docvec!["'", Document::String(ta.type_name().to_string()), "'"],
                None => Document::Str("'none'"),
            };
            parts.push(docvec![
                "'",
                Document::String(s.name.name.to_string()),
                "' => ",
                type_doc,
            ]);
        }
        parts.push(Document::Str("}~"));
        Document::Vec(parts)
    }

    pub(super) fn meta_instance_method_entries(
        class: &ClassDefinition,
        module: &Module,
        auto: Option<&crate::codegen::core_erlang::value_type_codegen::AutoSlotMethods>,
        include_standalone: bool,
    ) -> Vec<MethodInfoEntry> {
        let sealed = class.is_sealed;
        let mut entries: Vec<MethodInfoEntry> = class
            .methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| Self::meta_method_entry(m, sealed))
            .collect();
        // BT-1005: Standalone methods are excluded from __beamtalk_meta/0 (runtime-patched)
        // but included in BuilderState.meta so init/1 can register their return types.
        if include_standalone {
            for standalone in module.method_definitions.iter().filter(|m| {
                m.class_name.name == class.name.name
                    && !m.is_class_method
                    && m.method.kind == MethodKind::Primary
            }) {
                entries.push(Self::meta_method_entry(&standalone.method, sealed));
            }
        }
        if let Some(auto) = auto {
            use crate::codegen::core_erlang::value_type_codegen::AutoSlotMethods;
            for field in &auto.getters {
                entries.push((field.clone(), 0, None, vec![], sealed));
            }
            for field in &auto.setters {
                entries.push((
                    AutoSlotMethods::with_star_selector(field),
                    1,
                    None,
                    vec![None],
                    sealed,
                ));
            }
        }
        entries
    }

    /// Collects `MethodInfoEntry` tuples for all primary class methods of `class`,
    /// including the auto-generated keyword constructor for Value subclasses.
    pub(super) fn meta_class_method_entries(
        class: &ClassDefinition,
        module: &Module,
        auto: Option<&crate::codegen::core_erlang::value_type_codegen::AutoSlotMethods>,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
    ) -> Vec<MethodInfoEntry> {
        let sealed = class.is_sealed;
        let mut entries: Vec<MethodInfoEntry> = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| Self::meta_method_entry(m, sealed))
            .collect();
        // BT-1005: Standalone methods are excluded from __beamtalk_meta/0 (runtime-patched)
        // but included in BuilderState.meta so init/1 can register their return types.
        if include_standalone {
            for standalone in module.method_definitions.iter().filter(|m| {
                m.class_name.name == class.name.name
                    && m.is_class_method
                    && m.method.kind == MethodKind::Primary
            }) {
                entries.push(Self::meta_method_entry(&standalone.method, sealed));
            }
        }
        if let Some(auto) = auto {
            if let Some(kw_sel) = &auto.keyword_constructor {
                let arity = class.state.len();
                // BT-1408: Hash long keyword constructor selectors to stay within
                // Erlang's 255-char atom limit. The meta selector must match what
                // class_send emits so runtime dispatch finds the method.
                let safe_sel =
                    crate::codegen::core_erlang::selector_mangler::safe_class_method_selector(
                        kw_sel,
                    );
                entries.push((safe_sel, arity, None, vec![None; arity], sealed));
            }
        }
        // BT-1218: Register the synthesized supervisionSpec so class dispatch finds it locally
        // rather than walking the chain to Actor's version (which always returns #temporary).
        if synthesize_supervision_spec {
            entries.push((
                "supervisionSpec".to_string(),
                0,
                Some("SupervisionSpec".to_string()),
                vec![],
                sealed,
            ));
        }
        entries
    }

    /// Converts a `MethodDefinition` into a `MethodInfoEntry`.
    fn meta_method_entry(m: &MethodDefinition, class_is_sealed: bool) -> MethodInfoEntry {
        let return_type = m.return_type.as_ref().map(|rt| rt.type_name().to_string());
        let param_types: Vec<Option<String>> = m
            .parameters
            .iter()
            .map(|p| {
                p.type_annotation
                    .as_ref()
                    .map(|ta| ta.type_name().to_string())
            })
            .collect();
        (
            m.selector.to_erlang_atom(),
            m.selector.arity(),
            return_type,
            param_types,
            m.is_sealed || class_is_sealed,
        )
    }

    /// Builds a Core Erlang map of selector → method info map.
    ///
    /// Each entry: `'selector' => ~{'arity' => N, 'param_types' => [...], 'return_type' => 'T'}~`
    /// Empty slice → `~{}~`
    pub(super) fn meta_method_info_map(methods: &[MethodInfoEntry]) -> Document<'static> {
        if methods.is_empty() {
            return Document::Str("~{}~");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("~{"));
        for (i, (sel, arity, return_type, param_types, is_sealed)) in methods.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let param_types_doc = if param_types.is_empty() {
                Document::Str("[]")
            } else {
                let mut pts: Vec<Document<'static>> = Vec::new();
                pts.push(Document::Str("["));
                for (j, pt) in param_types.iter().enumerate() {
                    if j > 0 {
                        pts.push(Document::Str(", "));
                    }
                    pts.push(match pt {
                        Some(name) => docvec!["'", Document::String(name.clone()), "'"],
                        None => Document::Str("'none'"),
                    });
                }
                pts.push(Document::Str("]"));
                Document::Vec(pts)
            };
            let return_type_doc = match return_type {
                Some(name) => docvec!["'", Document::String(name.clone()), "'"],
                None => Document::Str("'none'"),
            };
            let is_sealed_doc = if *is_sealed { "'true'" } else { "'false'" };
            parts.push(docvec![
                "'",
                Document::String(sel.clone()),
                "' => ~{'arity' => ",
                Document::String(arity.to_string()),
                ", 'param_types' => ",
                param_types_doc,
                ", 'return_type' => ",
                return_type_doc,
                ", 'is_sealed' => ",
                Document::String(is_sealed_doc.to_string()),
                "}~",
            ]);
        }
        parts.push(Document::Str("}~"));
        Document::Vec(parts)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        ClassDefinition, Expression, ExpressionStatement, Identifier, Literal, MessageSelector,
        MethodDefinition, Module,
    };
    use crate::codegen::core_erlang::CoreErlangGenerator;
    use crate::source_analysis::Span;

    fn s() -> Span {
        Span::new(0, 0)
    }

    fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    fn simple_unary_method(selector: &str) -> MethodDefinition {
        MethodDefinition::new(
            MessageSelector::Unary(selector.into()),
            vec![],
            vec![bare(Expression::Literal(Literal::Integer(42), s()))],
            s(),
        )
    }

    fn empty_actor_class(name: &str) -> ClassDefinition {
        ClassDefinition::new(
            Identifier::new(name, s()),
            Identifier::new("Actor", s()),
            vec![],
            vec![],
            s(),
        )
    }

    #[test]
    fn test_generate_register_class_empty_module_renders_empty() {
        let mut generator = CoreErlangGenerator::new("test");
        let module = Module {
            classes: vec![],
            method_definitions: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = generator.generate_register_class(&module, false).unwrap();
        assert_eq!(
            doc.to_pretty_string(),
            "",
            "empty module should produce empty doc"
        );
    }

    #[test]
    fn test_generate_register_class_includes_class_name() {
        let mut generator = CoreErlangGenerator::new("test");
        let module = Module {
            classes: vec![empty_actor_class("Counter")],
            method_definitions: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = generator.generate_register_class(&module, false).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'Counter'"),
            "register_class should include class name atom. Got: {output}"
        );
        assert!(
            output.contains("register_class"),
            "register_class should define register_class/0. Got: {output}"
        );
    }

    #[test]
    fn test_generate_method_dispatch_unary_includes_selector() {
        let mut generator = CoreErlangGenerator::new("test");
        let method = simple_unary_method("increment");
        let doc = generator.generate_method_dispatch(&method, 2).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'increment'"),
            "method dispatch should include selector atom. Got: {output}"
        );
    }

    #[test]
    fn test_generate_class_method_dispatches_empty_class() {
        let mut generator = CoreErlangGenerator::new("test");
        let class = empty_actor_class("Counter");
        let doc = generator
            .generate_class_method_dispatches(&class, 2)
            .unwrap();
        assert_eq!(
            doc.to_pretty_string(),
            "",
            "class with no methods should produce empty dispatch doc"
        );
    }

    #[test]
    fn test_generate_class_method_functions_empty_class() {
        let mut generator = CoreErlangGenerator::new("test");
        let class = empty_actor_class("Counter");
        let doc = generator.generate_class_method_functions(&class).unwrap();
        assert_eq!(
            doc.to_pretty_string(),
            "",
            "class with no class methods should produce empty doc"
        );
    }
}
