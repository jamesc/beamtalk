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
    Block, ClassDefinition, Expression, Literal, MessageSelector, MethodDefinition, MethodKind,
    Module,
};
use crate::docvec;

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
        let needs_nlr = method
            .body
            .iter()
            .any(|expr| Self::expr_has_block_nlr(expr, false));

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
    #[expect(
        clippy::too_many_lines,
        reason = "method body generation handles many expression types and state threading"
    )]
    pub(in crate::codegen::core_erlang) fn generate_method_definition_body_with_reply(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<Document<'static>> {
        if method.body.is_empty() {
            // Empty method body returns self (the actor object reference)
            let state = self.current_state_var();
            return Ok(docvec!["{'reply', Self, ", state, "}"]);
        }

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Generate all expressions except the last with state threading
        // Filter out @expect directives — they are compile-time only and generate no code.
        let body: Vec<&Expression> = method
            .body
            .iter()
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();
        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);
            let is_local_assignment = Self::is_local_var_assignment(expr);

            // Check for early return
            if let Expression::Return { value, .. } = expr {
                // BT-904: If the returned value is a state-threading control flow
                // (e.g., collect: with self-sends), it returns {Result, State} tuple.
                // Unpack it instead of wrapping it again.
                if self.control_flow_has_mutations(value) {
                    let tuple_var = self.fresh_temp_var("Tuple");
                    let value_str = self.expression_doc(value)?;
                    let doc = docvec![
                        "let ",
                        Document::String(tuple_var.clone()),
                        " = ",
                        value_str,
                        " in let _Result = call 'erlang':'element'(1, ",
                        Document::String(tuple_var.clone()),
                        ") in let _NewState = call 'erlang':'element'(2, ",
                        Document::String(tuple_var),
                        ") in {'reply', _Result, _NewState}",
                    ];
                    docs.push(doc);
                } else {
                    let final_state = self.current_state_var();
                    let value_str = self.expression_doc(value)?;
                    let doc = docvec![
                        "let _ReturnValue = ",
                        value_str,
                        " in {'reply', _ReturnValue, ",
                        Document::String(final_state),
                        "}",
                    ];
                    docs.push(doc);
                }
                return Ok(Document::Vec(docs));
            }

            if is_last {
                // Last expression: bind to Result and generate reply tuple
                // If the last expression is a field assignment, handle specially
                if is_field_assignment {
                    // Generate the assignment (leaves state binding open)
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let val_var = self.fresh_temp_var("Val");
                            let current_state = self.current_state_var();

                            let value_str = self.expression_doc(value)?;

                            let new_state = self.next_state_var();
                            let doc = docvec![
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
                            ];
                            docs.push(doc);
                        }
                    }
                } else if Self::is_super_message_send(expr) {
                    // Super message send as last expression: unpack {reply, Result, NewState}
                    let expr_str = self.expression_doc(expr)?;
                    let doc = docvec![
                        "let _SuperTuple = ",
                        expr_str,
                        " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                        " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                        " in {'reply', _Result, _NewState}",
                    ];
                    docs.push(doc);
                } else if Self::is_error_message_send(expr) {
                    // Error message send: never returns, so just emit the call directly
                    // without wrapping in a reply tuple (would be unreachable code)
                    let expr_str = self.expression_doc(expr)?;
                    docs.push(docvec![expr_str]);
                } else if self.is_tier2_value_call(expr) {
                    // BT-851: Last expression is a Tier 2 value: call.
                    // Returns {Result, NewState} tuple — unpack for reply.
                    let tuple_var = self.fresh_temp_var("T2Tuple");
                    let expr_str = self.expression_doc(expr)?;
                    let doc = docvec![
                        "let ",
                        Document::String(tuple_var.clone()),
                        " = ",
                        expr_str,
                        " in let _Result = call 'erlang':'element'(1, ",
                        Document::String(tuple_var.clone()),
                        ") in let _NewState = call 'erlang':'element'(2, ",
                        Document::String(tuple_var),
                        ") in {'reply', _Result, _NewState}",
                    ];
                    docs.push(doc);
                } else if self.control_flow_has_mutations(expr) {
                    // BT-483: Last expression is control flow with field mutations.
                    // The mutation variant returns {Result, State} tuple.
                    let tuple_var = self.fresh_temp_var("Tuple");
                    let expr_str = self.expression_doc(expr)?;
                    let doc = docvec![
                        "let ",
                        Document::String(tuple_var.clone()),
                        " = ",
                        expr_str,
                        " in let _Result = call 'erlang':'element'(1, ",
                        Document::String(tuple_var.clone()),
                        ") in let _NewState = call 'erlang':'element'(2, ",
                        Document::String(tuple_var),
                        ") in {'reply', _Result, _NewState}",
                    ];
                    docs.push(doc);
                } else if let Some(tier2_args) = self.detect_tier2_self_send(expr) {
                    // BT-870: Last expression is a Tier 2 self-send (or promoted Tier 1 at
                    // a known Tier 2 HOM position). generate_tier2_self_send_open emits the
                    // dispatch + state extraction as an open let chain; close with reply tuple.
                    let doc = self.generate_tier2_self_send_open(expr, &tier2_args)?;
                    docs.push(doc);
                    let dispatch_var = self.last_dispatch_var.clone().ok_or_else(|| {
                        CodeGenError::Internal(
                            "invariant violation: missing dispatch var after Tier 2 self-send"
                                .to_string(),
                        )
                    })?;
                    let final_state = self.current_state_var();
                    docs.push(docvec![
                        "{'reply', call 'erlang':'element'(1, ",
                        Document::String(dispatch_var),
                        "), ",
                        Document::String(final_state),
                        "}",
                    ]);
                } else {
                    // Regular last expression: bind to Result and reply
                    // BT-851: Get state AFTER expression generation, since Tier 2
                    // value: calls advance state version inside expression_doc.
                    let expr_str = self.expression_doc(expr)?;
                    let post_state = self.current_state_var();
                    let doc = docvec![
                        "let _Result = ",
                        expr_str,
                        " in {'reply', _Result, ",
                        Document::String(post_state),
                        "}",
                    ];
                    docs.push(doc);
                }
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
            } else if is_local_assignment {
                // BT-598: Local variable assignment in method body
                if let Expression::Assignment { target, value, .. } = expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = &id.name;
                        let core_var = self
                            .lookup_var(var_name)
                            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);

                        // BT-853: If RHS is a Tier 2 value: call, it returns
                        // {Result, NewState} — unpack result and thread NewState.
                        if self.is_tier2_value_call(value) {
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
                        // BT-598: If RHS is control flow with mutations, it returns
                        // {Result, State} — unpack and update state variable.
                        } else if self.control_flow_has_mutations(value) {
                            let tuple_var = self.fresh_temp_var("Tuple");
                            let next_version = self.state_version() + 1;
                            let new_state = if next_version == 1 {
                                "State1".to_string()
                            } else {
                                format!("State{next_version}")
                            };
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

                            // Extract threaded locals from updated state
                            if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(value)
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
                        } else {
                            let value_str = self.expression_doc(value)?;
                            self.bind_var(var_name, &core_var);
                            let doc = docvec![
                                "let ",
                                Document::String(core_var),
                                " = ",
                                value_str,
                                " in ",
                            ];
                            docs.push(doc);
                        }
                    }
                }
            } else if self.is_tier2_value_call(expr) {
                // BT-853: Tier 2 value: call in non-last position.
                // Returns {Result, NewState} — discard Result, thread NewState.
                let tuple_var = self.fresh_temp_var("T2Tuple");
                let discard_var = self.fresh_temp_var("T2Discard");
                let expr_str = self.expression_doc(expr)?;
                let new_state = self.next_state_var();
                docs.push(docvec![
                    "let ",
                    Document::String(tuple_var.clone()),
                    " = ",
                    expr_str,
                    " in let ",
                    Document::String(discard_var),
                    " = call 'erlang':'element'(1, ",
                    Document::String(tuple_var.clone()),
                    ")\n in let ",
                    Document::String(new_state),
                    " = call 'erlang':'element'(2, ",
                    Document::String(tuple_var),
                    ") in ",
                ]);
            } else if let Some(tier2_args) = self.detect_tier2_self_send(expr) {
                // BT-851: Tier 2 self-send with stateful block arguments.
                // Pack captured locals into State, send, extract locals from returned State.
                let doc = self.generate_tier2_self_send_open(expr, &tier2_args)?;
                docs.push(doc);
            } else if self.control_flow_has_mutations(expr) {
                // BT-483: Control flow that threads state returns {Result, State} tuple.
                // Extract State for subsequent expressions.
                let tuple_var = self.fresh_temp_var("Tuple");
                let next_version = self.state_version() + 1;
                let new_state = if next_version == 1 {
                    "State1".to_string()
                } else {
                    format!("State{next_version}")
                };
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

                // BT-598: Extract threaded locals from the updated state
                if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(expr) {
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
            } else {
                // Regular intermediate expression: wrap in let to discard value
                let tmp_var = self.fresh_temp_var("seq");
                let expr_str = self.expression_doc(expr)?;
                let doc = docvec!["let ", Document::String(tmp_var), " = ", expr_str, " in ",];
                docs.push(doc);
            }
        }

        Ok(Document::Vec(docs))
    }

    /// Generates a block-based method body with state threading and reply tuple.
    ///
    /// For methods using block syntax with implicit returns.
    #[expect(
        clippy::too_many_lines,
        reason = "method body generation handles many expression types and state threading"
    )]
    pub(in crate::codegen::core_erlang) fn generate_method_body_with_reply(
        &mut self,
        block: &Block,
    ) -> Result<Document<'static>> {
        if block.body.is_empty() {
            // Empty method body returns self (the actor object reference)
            let final_state = self.current_state_var();
            return Ok(docvec!["{'reply', Self, ", final_state, "}"]);
        }

        let mut docs: Vec<Document<'static>> = Vec::new();

        // Generate all expressions except the last with state threading
        // Filter out @expect directives — they are compile-time only and generate no code.
        let body: Vec<&Expression> = block
            .body
            .iter()
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();
        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);
            let is_local_assignment = Self::is_local_var_assignment(expr);

            if is_last {
                // Last expression: bind to Result and generate reply tuple
                // If the last expression is a field assignment, handle specially
                if is_field_assignment {
                    // Generate the assignment (leaves state binding open)
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let val_var = self.fresh_temp_var("Val");
                            let current_state = self.current_state_var();

                            let value_str = self.expression_doc(value)?;

                            let new_state = self.next_state_var();
                            let doc = docvec![
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
                            ];
                            docs.push(doc);
                        }
                    }
                } else if Self::is_super_message_send(expr) {
                    // Super message send as last expression: unpack {reply, Result, NewState}
                    let expr_str = self.expression_doc(expr)?;
                    let doc = docvec![
                        "let _SuperTuple = ",
                        expr_str,
                        " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                        " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                        " in {'reply', _Result, _NewState}",
                    ];
                    docs.push(doc);
                } else if Self::is_error_message_send(expr) {
                    // Error message send: never returns, so just emit the call directly
                    // without wrapping in a reply tuple (would be unreachable code)
                    let expr_str = self.expression_doc(expr)?;
                    docs.push(docvec![expr_str]);
                } else if self.is_tier2_value_call(expr) {
                    // BT-851: Last expression is a Tier 2 value: call.
                    // Returns {Result, NewState} tuple — unpack for reply.
                    let tuple_var = self.fresh_temp_var("T2Tuple");
                    let expr_str = self.expression_doc(expr)?;
                    let doc = docvec![
                        "let ",
                        Document::String(tuple_var.clone()),
                        " = ",
                        expr_str,
                        " in let _Result = call 'erlang':'element'(1, ",
                        Document::String(tuple_var.clone()),
                        ") in let _NewState = call 'erlang':'element'(2, ",
                        Document::String(tuple_var),
                        ") in {'reply', _Result, _NewState}",
                    ];
                    docs.push(doc);
                } else if self.control_flow_has_mutations(expr) {
                    // BT-483: Last expression is control flow with field mutations.
                    // The mutation variant returns {Result, State} tuple.
                    let tuple_var = self.fresh_temp_var("Tuple");
                    let expr_str = self.expression_doc(expr)?;
                    let doc = docvec![
                        "let ",
                        Document::String(tuple_var.clone()),
                        " = ",
                        expr_str,
                        " in let _Result = call 'erlang':'element'(1, ",
                        Document::String(tuple_var.clone()),
                        ") in let _NewState = call 'erlang':'element'(2, ",
                        Document::String(tuple_var),
                        ") in {'reply', _Result, _NewState}",
                    ];
                    docs.push(doc);
                } else if let Some(tier2_args) = self.detect_tier2_self_send(expr) {
                    // BT-870: Last expression is a Tier 2 self-send (or promoted Tier 1 at
                    // a known Tier 2 HOM position). generate_tier2_self_send_open emits the
                    // dispatch + state extraction as an open let chain; close with reply tuple.
                    let doc = self.generate_tier2_self_send_open(expr, &tier2_args)?;
                    docs.push(doc);
                    let dispatch_var = self.last_dispatch_var.clone().ok_or_else(|| {
                        CodeGenError::Internal(
                            "invariant violation: missing dispatch var after Tier 2 self-send"
                                .to_string(),
                        )
                    })?;
                    let final_state = self.current_state_var();
                    docs.push(docvec![
                        "{'reply', call 'erlang':'element'(1, ",
                        Document::String(dispatch_var),
                        "), ",
                        Document::String(final_state),
                        "}",
                    ]);
                } else {
                    // Regular last expression: bind to Result and reply
                    // BT-851: Get state AFTER expression generation, since Tier 2
                    // value: calls advance state version inside expression_doc.
                    let expr_str = self.expression_doc(expr)?;
                    let post_state = self.current_state_var();
                    let doc = docvec![
                        "let _Result = ",
                        expr_str,
                        " in {'reply', _Result, ",
                        Document::String(post_state),
                        "}",
                    ];
                    docs.push(doc);
                }
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
            } else if is_local_assignment {
                // Local variable assignment: generate with proper binding
                if let Expression::Assignment { target, value, .. } = expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = &id.name;
                        let core_var = self
                            .lookup_var(var_name)
                            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);

                        // BT-853: If RHS is a Tier 2 value: call, it returns
                        // {Result, NewState} — unpack result and thread NewState.
                        if self.is_tier2_value_call(value) {
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
                        // BT-598: If RHS is control flow with mutations, it returns
                        // {Result, State} — unpack and update state variable.
                        } else if self.control_flow_has_mutations(value) {
                            let tuple_var = self.fresh_temp_var("Tuple");
                            let next_version = self.state_version() + 1;
                            let new_state = if next_version == 1 {
                                "State1".to_string()
                            } else {
                                format!("State{next_version}")
                            };
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

                            // Extract threaded locals from updated state
                            if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(value)
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
                        } else {
                            let value_str = self.expression_doc(value)?;
                            self.bind_var(var_name, &core_var);
                            let doc = docvec![
                                "let ",
                                Document::String(core_var),
                                " = ",
                                value_str,
                                " in ",
                            ];
                            docs.push(doc);
                        }
                    }
                }
            } else if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(expr) {
                // Control flow with local variable threading - need to rebind threaded vars after loop
                if self.control_flow_has_mutations(expr) {
                    // BT-598: Control flow with mutations returns {Result, State} tuple.
                    // Threaded locals are packed into the State map.
                    // Extract State, then extract each threaded local from it.
                    let tuple_var = self.fresh_temp_var("Tuple");
                    let next_version = self.state_version() + 1;
                    let new_state = if next_version == 1 {
                        "State1".to_string()
                    } else {
                        format!("State{next_version}")
                    };
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

                    // Extract each threaded local from the updated state
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
                    docs.push(Document::Vec(doc_parts));
                } else if threaded_vars.len() == 1 {
                    let var = &threaded_vars[0];
                    let core_var = self
                        .lookup_var(var)
                        .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                    let expr_str = self.expression_doc(expr)?;
                    let doc = docvec!["let ", Document::String(core_var), " = ", expr_str, " in ",];
                    docs.push(doc);
                } else {
                    // Multiple threaded vars - fall back for now
                    let tmp_var = self.fresh_temp_var("seq");
                    let expr_str = self.expression_doc(expr)?;
                    let doc = docvec!["let ", Document::String(tmp_var), " = ", expr_str, " in ",];
                    docs.push(doc);
                }
            } else if Self::is_super_message_send(expr) {
                // Super message send: must thread state from {reply, Result, NewState} tuple
                let super_result_var = self.fresh_temp_var("SuperReply");
                let current_state = self.current_state_var();
                let new_state = self.next_state_var();
                let class_name = self.class_name();

                // Generate beamtalk_dispatch:super/5 call (ADR 0006)
                if let Expression::MessageSend {
                    selector,
                    arguments,
                    ..
                } = expr
                {
                    // Use the domain service method for selector-to-atom conversion
                    let selector_atom = selector.to_erlang_atom();
                    let mut arg_docs: Vec<Document<'static>> = Vec::new();
                    for (j, arg) in arguments.iter().enumerate() {
                        if j > 0 {
                            arg_docs.push(Document::Str(", "));
                        }
                        arg_docs.push(self.expression_doc(arg)?);
                    }
                    let doc = docvec![
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
                    ];
                    docs.push(doc);
                }

                // Extract state from the {reply, Result, NewState} tuple using element/2
                let doc = docvec![
                    " in let ",
                    Document::String(new_state),
                    " = call 'erlang':'element'(3, ",
                    Document::String(super_result_var),
                    ") in ",
                ];
                docs.push(doc);
            } else if self.is_tier2_value_call(expr) {
                // BT-853: Tier 2 value: call in non-last position.
                // Returns {Result, NewState} — discard Result, thread NewState.
                let tuple_var = self.fresh_temp_var("T2Tuple");
                let discard_var = self.fresh_temp_var("T2Discard");
                let expr_str = self.expression_doc(expr)?;
                let new_state = self.next_state_var();
                docs.push(docvec![
                    "let ",
                    Document::String(tuple_var.clone()),
                    " = ",
                    expr_str,
                    " in let ",
                    Document::String(discard_var),
                    " = call 'erlang':'element'(1, ",
                    Document::String(tuple_var.clone()),
                    ")\n in let ",
                    Document::String(new_state),
                    " = call 'erlang':'element'(2, ",
                    Document::String(tuple_var),
                    ") in ",
                ]);
            } else if let Some(tier2_args) = self.detect_tier2_self_send(expr) {
                // BT-851: Tier 2 self-send with stateful block arguments.
                let doc = self.generate_tier2_self_send_open(expr, &tier2_args)?;
                docs.push(doc);
            } else if self.control_flow_has_mutations(expr) {
                // BT-483: Control flow with field mutations returns {Result, State} tuple.
                // Extract State for subsequent expressions.
                let tuple_var = self.fresh_temp_var("Tuple");
                let next_version = self.state_version() + 1;
                let new_state = if next_version == 1 {
                    "State1".to_string()
                } else {
                    format!("State{next_version}")
                };
                let expr_str = self.expression_doc(expr)?;
                let doc = docvec![
                    "let ",
                    Document::String(tuple_var.clone()),
                    " = ",
                    expr_str,
                    " in let ",
                    Document::String(new_state),
                    " = call 'erlang':'element'(2, ",
                    Document::String(tuple_var),
                    ") in ",
                ];
                docs.push(doc);
                let _ = self.next_state_var();
            } else {
                // Non-assignment intermediate expression: wrap in let
                let tmp_var = self.fresh_temp_var("seq");
                let expr_str = self.expression_doc(expr)?;
                let doc = docvec!["let ", Document::String(tmp_var), " = ", expr_str, " in ",];
                docs.push(doc);
            }
        }
        Ok(Document::Vec(docs))
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
    fn is_self_error_body(body: &[Expression]) -> bool {
        if body.len() != 1 {
            return false;
        }
        matches!(
            &body[0],
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
    /// The function is defensive — if `beamtalk_class_builder` is not available
    /// (e.g., during early module loading), the try/catch returns `ok` to allow
    /// the module to load.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'register_class'/0 = fun () ->
    ///     try
    ///         let _BuilderState0 = ~{
    ///             'className' => 'Counter',
    ///             'superclassRef' => 'Actor',
    ///             'fieldSpecs' => ~{'value' => 0}~,
    ///             'methodSpecs' => ~{...}~,
    ///             'modifiers' => [],
    ///             ...
    ///         }~
    ///         in let _Reg0 = case call 'beamtalk_class_builder':'register'(_BuilderState0) of
    ///             <{'ok', _Pid0}> when 'true' -> 'ok'
    ///             <{'error', _Err0}> when 'true' -> {'error', _Err0}
    ///         end
    ///         in _Reg0
    ///     catch <_,_,_> -> 'ok'
    /// ```
    #[allow(clippy::too_many_lines)] // builds class metadata map with methods, fields, and source
    pub(in crate::codegen::core_erlang) fn generate_register_class(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Skip if no class definitions
        if module.classes.is_empty() {
            return Ok(Document::Nil);
        }

        let mut class_docs = Vec::new();

        for (i, class) in module.classes.iter().enumerate() {
            // Instance methods — used for methodSpecs and methodSource
            let instance_methods: Vec<_> = class
                .methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .collect();

            // methodSpecs: selector => #{arity => N, is_sealed => bool}
            let mut method_spec_docs: Vec<Document<'static>> = Vec::new();
            for (m_idx, method) in instance_methods.iter().enumerate() {
                if m_idx > 0 {
                    method_spec_docs.push(Document::Str(", "));
                }
                let is_sealed = if method.is_sealed || class.is_sealed {
                    "'true'"
                } else {
                    "'false'"
                };
                method_spec_docs.push(docvec![
                    "'",
                    Document::String(method.selector.name().to_string()),
                    "' => ~{'arity' => ",
                    Document::String(method.selector.arity().to_string()),
                    ", 'is_sealed' => ",
                    Document::String(is_sealed.to_string()),
                    "}~",
                ]);
            }
            let method_specs_doc = Document::Vec(method_spec_docs);

            // fieldSpecs: field_name => default_value (map, not list)
            let mut field_spec_docs: Vec<Document<'static>> = Vec::new();
            for (s_idx, s) in class.state.iter().enumerate() {
                if s_idx > 0 {
                    field_spec_docs.push(Document::Str(", "));
                }
                let default_val = if let Some(ref default_value) = s.default_value {
                    self.expression_doc(default_value)?
                } else {
                    Document::Str("'nil'")
                };
                field_spec_docs.push(docvec![
                    "'",
                    Document::String(s.name.name.to_string()),
                    "' => ",
                    default_val,
                ]);
            }
            let field_specs_doc = Document::Vec(field_spec_docs);

            // modifiers list
            let mut modifiers: Vec<&str> = Vec::new();
            if class.is_sealed {
                modifiers.push("'sealed'");
            }
            if class.is_abstract {
                modifiers.push("'abstract'");
            }
            let mut modifier_docs: Vec<Document<'static>> = Vec::new();
            for (i, modifier) in modifiers.iter().enumerate() {
                if i > 0 {
                    modifier_docs.push(Document::Str(", "));
                }
                modifier_docs.push(Document::String((*modifier).to_string()));
            }
            let modifiers_doc = docvec!["[", Document::Vec(modifier_docs), "]"];

            // classMethods: class method specs (spawn/new + user-defined)
            let mut class_method_entries: Vec<Document<'static>> = Vec::new();
            if self.context == CodeGenContext::Actor {
                class_method_entries.push(Document::Str("'spawn' => ~{'arity' => 0}~"));
                class_method_entries.push(Document::Str("'spawnWith:' => ~{'arity' => 1}~"));
            } else {
                class_method_entries.push(Document::Str("'new' => ~{'arity' => 0}~"));
                class_method_entries.push(Document::Str("'new:' => ~{'arity' => 1}~"));
            }
            // BT-411: User-defined class methods
            for method in &class.class_methods {
                if method.kind == MethodKind::Primary {
                    class_method_entries.push(docvec![
                        "'",
                        Document::String(method.selector.name().to_string()),
                        "' => ~{'arity' => ",
                        Document::String(method.selector.arity().to_string()),
                        "}~",
                    ]);
                }
            }

            let num_entries = class_method_entries.len();
            let class_methods_lines: Vec<_> = class_method_entries
                .into_iter()
                .enumerate()
                .map(|(i, entry)| {
                    if i < num_entries - 1 {
                        docvec![line(), entry, ","]
                    } else {
                        docvec![line(), entry]
                    }
                })
                .collect();
            let class_methods_doc = docvec![
                "'classMethods' => ~{",
                nest(INDENT, Document::Vec(class_methods_lines)),
                line(),
                "}~,",
            ];

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

            // BT-877: Detect raising `new` pattern to emit isConstructible.
            // Only emit `false` — omit the key for constructible classes so the
            // runtime can fall back to lazy computation (which also checks
            // whether the class exports spawn/0, etc.).
            let has_raising_new = Self::has_raising_new(class);

            // BT-837: Build ClassBuilder state map and call register/1
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
                        "'fieldSpecs' => ~{",
                        field_specs_doc,
                        "}~,",
                        line(),
                        "'methodSpecs' => ~{",
                        method_specs_doc,
                        "}~,",
                        line(),
                        "'modifiers' => ",
                        modifiers_doc,
                        ",",
                        line(),
                        docvec![
                            "'moduleName' => '",
                            Document::String(self.module_name.clone()),
                            "',"
                        ],
                        line(),
                        class_methods_doc,
                        line(),
                        "'methodSource' => ~{",
                        method_source_doc,
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
                        "}~",
                        if has_raising_new {
                            docvec![",", line(), "'isConstructible' => 'false'"]
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
                    "catch <CatchType, CatchError, CatchStack> -> 'ok'",
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

            // Generate body as Document, render to string for embedding
            let body_str = if method.body.is_empty() {
                // Empty class method body returns self (ClassSelf)
                "ClassSelf".to_string()
            } else {
                let body_doc = self.generate_class_method_body(method, &class.class_variables)?;
                body_doc.to_pretty_string()
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
                nest(INDENT, docvec![line(), body_str,]),
                "\n",
            ];
            docs.push(doc);

            self.pop_scope();
            self.in_class_method = false;
        }
        self.class_var_names.clear();
        self.class_method_selectors.clear();
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
                        let expr_str = self.expression_doc(expr)?;
                        if self.class_var_mutated {
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
                    let expr_str = self.expression_doc(expr)?;
                    docs.push(docvec![expr_str]);
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
                        let val_doc = self.expression_doc(value)?;
                        self.bind_var(var_name, &core_var);
                        docs.push(docvec![
                            "let ",
                            Document::String(core_var),
                            " = ",
                            val_doc,
                            " in "
                        ]);
                    }
                }
            } else {
                let tmp_var = self.fresh_temp_var("seq");
                let expr_str = self.expression_doc(expr)?;
                let doc = docvec!["let ", Document::String(tmp_var), " = ", expr_str, " in ",];
                docs.push(doc);
            }
        }
        Ok(Document::Vec(docs))
    }

    /// Extracts source text for a method from the original source (BT-101).
    fn extract_method_source(&self, method: &MethodDefinition) -> String {
        if let Some(ref source) = self.source_text {
            let start = method.span.start() as usize;
            let end = method.span.end() as usize;
            if end <= source.len() {
                return source[start..end].trim().to_string();
            }
        }
        // Fallback: use selector name
        method.selector.name().to_string()
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
                if let Expression::Identifier(id) = receiver.as_ref() {
                    return self.tier2_block_params.contains(id.name.as_str());
                }
            }
        }
        false
    }

    /// Checks if a control flow expression actually threads state through mutations.
    ///
    /// This goes beyond `is_state_threading_control_flow` by analyzing whether
    /// the block argument contains field mutations that require state threading.
    ///
    /// Returns `true` only if:
    /// 1. The expression is a state-threading control flow construct (do:, whileTrue:, etc.)
    /// 2. The block argument contains field writes that need threading
    ///
    /// This prevents binding non-state return values (like `ok` or `nil`) to `StateN`.
    pub(in crate::codegen::core_erlang) fn control_flow_has_mutations(
        &self,
        expr: &Expression,
    ) -> bool {
        // First check if it's a potential state-threading construct
        if !Self::is_state_threading_control_flow(expr) {
            return false;
        }

        if let Expression::MessageSend {
            receiver,
            arguments,
            selector: crate::ast::MessageSelector::Keyword(parts),
            ..
        } = expr
        {
            // BT-410: For on:do: and ensure:, the receiver (try body) is also
            // a block that may contain field mutations
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            if matches!(sel.as_str(), "on:do:" | "ensure:") {
                if let Expression::Block(block) = receiver.as_ref() {
                    let analysis = block_analysis::analyze_block(block);
                    if self.needs_mutation_threading(&analysis) {
                        return true;
                    }
                }
            }

            // Standard check: analyze the last argument block
            if let Some(Expression::Block(block)) = arguments.last() {
                let analysis = block_analysis::analyze_block(block);
                return self.needs_mutation_threading(&analysis);
            }
        }

        // If we can't analyze (e.g., block isn't a literal), conservatively return false
        // to avoid binding non-state values to StateN
        false
    }
}
