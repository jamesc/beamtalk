// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expression code generation.
//!
//! This module handles code generation for Beamtalk expressions:
//! - Literals (integers, floats, strings, symbols)
//! - Identifiers and variable references
//! - Map literals
//! - Field access (`self.field`)
//! - Field assignment (`self.field := value`)
//! - Blocks (closures)
//! - Await expressions
//! - Cascades
//!
//! Note: Message sending is handled by [`super::dispatch_codegen`].

use super::document::Document;
use super::{CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{
    Block, CascadeMessage, Expression, Identifier, Literal, MapPair, MatchArm, MessageSelector,
    Pattern,
};
use crate::docvec;
use std::fmt::Write; // For write!() on local String buffers (not self.output)

impl CoreErlangGenerator {
    /// Generates code for a literal value.
    ///
    /// Maps Beamtalk literals to Core Erlang:
    /// - Integers: `42` → `42`
    /// - Floats: `3.14` → `3.14`
    /// - Strings: `"hello"` → binary syntax `#{#<104>(8,1,...), #<101>(8,1,...), ...}#`
    /// - Symbols: `#foo` → atom `'foo'`
    /// - Characters: `$a` → integer `97`
    /// - Arrays: `[1, 2, 3]` → list `[1, 2, 3]`
    #[allow(clippy::self_only_used_in_recursion)]
    pub(super) fn generate_literal(&self, lit: &Literal) -> Result<Document<'static>> {
        match lit {
            Literal::Integer(n) => Ok(docvec![format!("{n}")]),
            Literal::Float(f) => Ok(docvec![format!("{f}")]),
            Literal::String(s) => {
                let result = Self::binary_string_literal(s);
                Ok(docvec![result])
            }
            Literal::Symbol(s) => Ok(docvec![format!("'{s}'")]),
            Literal::Character(c) => Ok(docvec![format!("{}", *c as u32)]),
            Literal::List(elements) => {
                let mut parts: Vec<Document<'static>> = vec![Document::String("[".to_string())];
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::String(", ".to_string()));
                    }
                    parts.push(self.generate_literal(elem)?);
                }
                parts.push(Document::String("]".to_string()));
                Ok(Document::Vec(parts))
            }
        }
    }

    /// Generates code for an identifier reference.
    ///
    /// Handles three cases:
    /// 1. Reserved keywords (`true`, `false`, `nil`, `self`) → Core Erlang atoms/variables
    /// 2. Bound variables (in scope) → Core Erlang variable name
    /// 3. Unbound identifiers → Field access via `maps:get/2` from context-appropriate variable:
    ///    - **Actor context**: `State` or `StateAcc` (with threading)
    ///    - **`ValueType` context**: `Self` parameter
    ///    - **Repl context**: `State` from bindings
    pub(super) fn generate_identifier(&mut self, id: &Identifier) -> Result<Document<'static>> {
        // Handle special reserved identifiers as atoms
        match id.name.as_str() {
            "true" => Ok(docvec!["'true'"]),
            "false" => Ok(docvec!["'false'"]),
            "nil" => Ok(docvec!["'nil'"]),
            "self" => {
                // BT-411: Check if self is explicitly bound (e.g., in class methods)
                if let Some(var_name) = self.lookup_var("self").cloned() {
                    Ok(docvec![var_name])
                } else {
                    Ok(docvec!["Self"]) // self → Self parameter (BT-161)
                }
            }
            "super" => {
                // super alone is an error - must be used in message send (super method: args)
                Err(CodeGenError::UnsupportedFeature {
                    feature: "super used alone (must be in message send like 'super method: arg')"
                        .to_string(),
                    location: format!("byte offset {}", id.span.start()),
                })
            }
            _ => {
                // Check if it's a bound variable in current or outer scopes
                if let Some(var_name) = self.lookup_var(id.name.as_str()).cloned() {
                    Ok(docvec![var_name])
                } else {
                    // Field access from state/self
                    // BT-213: Context determines which variable to use
                    let state_var = match self.context {
                        super::CodeGenContext::ValueType => {
                            // Value types use Self parameter
                            "Self".to_string()
                        }
                        super::CodeGenContext::Actor => {
                            // Actors use State with threading
                            // BT-153: Use StateAcc when inside loop body
                            if self.in_loop_body {
                                if self.state_version() == 0 {
                                    "StateAcc".to_string()
                                } else {
                                    format!("StateAcc{}", self.state_version())
                                }
                            } else {
                                self.current_state_var()
                            }
                        }
                        super::CodeGenContext::Repl => {
                            // REPL uses State from bindings, but StateAcc in loops
                            // BT-153: Use StateAcc when inside loop body
                            if self.in_loop_body {
                                if self.state_version() == 0 {
                                    "StateAcc".to_string()
                                } else {
                                    format!("StateAcc{}", self.state_version())
                                }
                            } else {
                                self.current_state_var()
                            }
                        }
                    };
                    Ok(docvec![format!(
                        "call 'maps':'get'('{}', {state_var})",
                        id.name
                    )])
                }
            }
        }
    }

    /// Generates code for a map literal: `~{key => value, ...}~`
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// ~{'name' => <<"Alice">>, 'age' => 30}~
    /// ```
    pub(super) fn generate_map_literal(&mut self, pairs: &[MapPair]) -> Result<Document<'static>> {
        if pairs.is_empty() {
            return Ok(docvec!["~{}~"]);
        }

        let mut parts: Vec<Document<'static>> = vec![Document::String("~{ ".to_string())];

        for (i, pair) in pairs.iter().enumerate() {
            if i > 0 {
                parts.push(Document::String(", ".to_string()));
            }

            // Generate the key
            parts.push(self.expression_doc(&pair.key)?);
            parts.push(Document::String(" => ".to_string()));

            // Generate the value
            parts.push(self.expression_doc(&pair.value)?);
        }

        parts.push(Document::String(" }~".to_string()));
        Ok(Document::Vec(parts))
    }

    /// Generates code for a list literal: `#(1, 2, 3)` → `[1, 2, 3]`
    ///
    /// Cons syntax `#(head | tail)` → `[head | tail]`
    pub(super) fn generate_list_literal(
        &mut self,
        elements: &[Expression],
        tail: Option<&Expression>,
    ) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = vec![Document::String("[".to_string())];
        for (i, elem) in elements.iter().enumerate() {
            if i > 0 {
                parts.push(Document::String(", ".to_string()));
            }
            parts.push(self.expression_doc(elem)?);
        }
        if let Some(t) = tail {
            if !elements.is_empty() {
                parts.push(Document::String(" | ".to_string()));
            }
            parts.push(self.expression_doc(t)?);
        }
        parts.push(Document::String("]".to_string()));
        Ok(Document::Vec(parts))
    }

    /// Generates code for field access (e.g., `self.value`).
    ///
    /// Maps to Erlang `maps:get/2` call:
    /// ```erlang
    /// call 'maps':'get'('value', State)  // Actor context
    /// call 'maps':'get'('value', Self)   // ValueType context
    /// ```
    pub(super) fn generate_field_access(
        &mut self,
        receiver: &Expression,
        field: &Identifier,
    ) -> Result<Document<'static>> {
        // BT-412: Class methods access class variables directly from ClassVars map
        if self.in_class_method {
            if let Expression::Identifier(recv_id) = receiver {
                if recv_id.name == "self" && self.class_var_names.contains(field.name.as_str()) {
                    let cv = self.current_class_var();
                    return Ok(docvec![format!(
                        "call 'maps':'get'('{}', {cv})",
                        field.name
                    )]);
                }
            }
            return Err(CodeGenError::UnsupportedFeature {
                feature: format!(
                    "cannot access instance field '{}' in a class method",
                    field.name
                ),
                location: format!("{:?}", field.span),
            });
        }
        // For now, assume receiver is 'self' and access from State/Self
        if let Expression::Identifier(recv_id) = receiver {
            if recv_id.name == "self" {
                // BT-213: Use appropriate variable based on context
                let state_var = match self.context {
                    super::CodeGenContext::ValueType => "Self".to_string(),
                    super::CodeGenContext::Actor => self.current_state_var(),
                    super::CodeGenContext::Repl => "State".to_string(),
                };
                return Ok(docvec![format!(
                    "call 'maps':'get'('{}', {state_var})",
                    field.name
                )]);
            }
        }

        Err(CodeGenError::UnsupportedFeature {
            feature: "complex field access".to_string(),
            location: format!("{:?}", receiver.span()),
        })
    }

    /// Generates code for a field assignment (`self.field := value`).
    ///
    /// Uses state threading to simulate mutation in Core Erlang:
    /// ```erlang
    /// let _Val = <value> in
    /// let State{n} = call 'maps':'put'('fieldName', _Val, State{n-1}) in
    /// _Val
    /// ```
    ///
    /// The assignment returns the assigned value (Smalltalk semantics).
    ///
    /// # BT-213: Value Type Restriction
    ///
    /// Field assignments are not supported in value type methods because value types
    /// are immutable. This function will return an error if called in `ValueType` context.
    /// Use semantic analysis to prevent field assignments in value types at compile time.
    pub(super) fn generate_field_assignment(
        &mut self,
        field_name: &str,
        value: &Expression,
    ) -> Result<Document<'static>> {
        // BT-412: Class methods assign to class variables via ClassVars map threading.
        // The generated code leaves `let ClassVarsN = ...` open — the caller (sequential
        // expression handler) must provide the continuation.
        if self.in_class_method {
            if self.class_var_names.contains(field_name) {
                let val_var = self.fresh_temp_var("Val");
                let current_cv = self.current_class_var();
                let val_doc = self.expression_doc(value)?;
                let new_cv = self.next_class_var();
                let doc = docvec![
                    format!("let {val_var} = "),
                    val_doc,
                    format!(
                        " in let {new_cv} = call 'maps':'put'('{field_name}', {val_var}, {current_cv}) in "
                    )
                ];
                // Store result var name for callers that need to reference it
                self.last_open_scope_result = Some(val_var);
                return Ok(doc);
            }
            return Err(CodeGenError::UnsupportedFeature {
                feature: format!(
                    "cannot assign to instance field '{field_name}' in a class method"
                ),
                location: "class method body".to_string(),
            });
        }
        // BT-213: Reject field assignments in value type context
        if matches!(self.context, super::CodeGenContext::ValueType) {
            return Err(CodeGenError::UnsupportedFeature {
                feature: format!(
                    "field assignment 'self.{field_name} := ...' in value type method (value types are immutable)"
                ),
                location: "value type method body".to_string(),
            });
        }

        let val_var = self.fresh_temp_var("Val");

        // Capture current state BEFORE generating value expression,
        // because the value expression may reference state (e.g., self.value + 1)
        let current_state = self.current_state_var();

        // Capture value expression (preserves side effects on state)
        let val_doc = self.expression_doc(value)?;

        // Now increment state version for the new state after assignment
        let new_state = self.next_state_var();

        let doc = docvec![
            format!("let {val_var} = "),
            val_doc,
            format!(
                " in let {new_state} = call 'maps':'put'('{field_name}', {val_var}, {current_state}) in "
            ),
            val_var
        ];
        Ok(doc)
    }

    /// Generates code for a block (closure).
    ///
    /// Blocks are Core Erlang funs with parameters:
    /// ```erlang
    /// fun (Param1, Param2) -> <body> end
    /// ```
    pub(super) fn generate_block(&mut self, block: &Block) -> Result<Document<'static>> {
        // Push a new scope for block parameters
        self.push_scope();

        let mut params = String::new();
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                params.push_str(", ");
            }
            let var_name = self.fresh_var(&param.name);
            params.push_str(&var_name);
        }
        let header = docvec!["fun (", params, ") -> "];

        // Generate block body as Document
        let body_doc = self.generate_block_body(block)?;

        // Pop the scope when done with the block
        self.pop_scope();
        Ok(docvec![header, body_doc])
    }

    /// Generates await expression.
    ///
    /// Delegates to `beamtalk_future:await/1` which blocks until the future
    /// is resolved or rejected (with 30-second default timeout):
    /// ```erlang
    /// call 'beamtalk_future':'await'(Future)
    /// ```
    pub(super) fn generate_await(&mut self, future: &Expression) -> Result<Document<'static>> {
        // Delegate to beamtalk_future:await/1, which uses 30s default timeout
        let future_doc = self.expression_doc(future)?;
        Ok(docvec!["call 'beamtalk_future':'await'(", future_doc, ")"])
    }

    /// Generates await with explicit timeout.
    ///
    /// Delegates to `beamtalk_future:await/2` with an explicit timeout value:
    /// ```erlang
    /// call 'beamtalk_future':'await'(Future, Timeout)
    /// ```
    pub(super) fn generate_await_with_timeout(
        &mut self,
        future: &Expression,
        timeout: &Expression,
    ) -> Result<Document<'static>> {
        let future_doc = self.expression_doc(future)?;
        let timeout_doc = self.expression_doc(timeout)?;
        Ok(docvec![
            "call 'beamtalk_future':'await'(",
            future_doc,
            ", ",
            timeout_doc,
            ")"
        ])
    }

    /// Generates awaitForever expression.
    ///
    /// Delegates to `beamtalk_future:await_forever/1` which waits indefinitely:
    /// ```erlang
    /// call 'beamtalk_future':'await_forever'(Future)
    /// ```
    pub(super) fn generate_await_forever(
        &mut self,
        future: &Expression,
    ) -> Result<Document<'static>> {
        let future_doc = self.expression_doc(future)?;
        Ok(docvec![
            "call 'beamtalk_future':'await_forever'(",
            future_doc,
            ")"
        ])
    }

    /// Generates code for cascade expressions.
    ///
    /// Cascades send multiple messages to the same receiver using semicolon separators.
    /// The receiver is evaluated once and each message is sent to that receiver.
    ///
    /// # Example
    ///
    /// ```beamtalk
    /// collection add: 1; add: 2; add: 3
    /// ```
    ///
    /// Generates:
    ///
    /// ```erlang
    /// let Receiver = <evaluate collection> in
    ///   let _ = <send add: 1 to Receiver> in
    ///   let _ = <send add: 2 to Receiver> in
    ///   <send add: 3 to Receiver>
    /// ```
    ///
    /// The cascade returns the result of the final message.
    pub(super) fn generate_cascade(
        &mut self,
        receiver: &Expression,
        messages: &[CascadeMessage],
    ) -> Result<Document<'static>> {
        if messages.is_empty() {
            // Edge case: cascade with no messages just evaluates to the receiver
            return self.generate_expression(receiver);
        }

        // The parser represents cascades such that `receiver` is the *first*
        // message send expression, e.g. for:
        //
        //   counter increment; increment; getValue
        //
        // `receiver` is a MessageSend for `counter increment`, and `messages`
        // holds the remaining cascade messages. We need to:
        //   1. Evaluate the underlying receiver expression (`counter`) once,
        //      bind it to a temp variable.
        //   2. Send the first message (`increment`) and all subsequent
        //      cascade messages to that same bound receiver.
        if let Expression::MessageSend {
            receiver: underlying_receiver,
            selector: first_selector,
            arguments: first_arguments,
            ..
        } = receiver
        {
            let mut result = String::new();
            let receiver_var = {
                // Bind the underlying receiver once
                let receiver_var = self.fresh_temp_var("Receiver");
                let recv_str = self.capture_expression(underlying_receiver)?;
                write!(result, "let {receiver_var} = ").unwrap();
                result.push_str(&recv_str);
                result.push_str(" in ");
                receiver_var
            };

            // Total number of messages in the cascade: first + remaining
            let total_messages = messages.len() + 1;

            for index in 0..total_messages {
                let is_last = index == total_messages - 1;

                if !is_last {
                    // For all but the last message, discard the result
                    result.push_str("let _ = ");
                }

                // Determine which selector/arguments to use:
                // index 0 -> first message from the initial MessageSend
                // index > 0 -> messages[index - 1]
                let (selector, arguments): (&MessageSelector, &[Expression]) = if index == 0 {
                    (first_selector, first_arguments.as_slice())
                } else {
                    let msg = &messages[index - 1];
                    (&msg.selector, msg.arguments.as_slice())
                };

                // Unified message dispatch to the bound receiver
                let selector_atom = selector.to_erlang_atom();
                if matches!(selector, MessageSelector::Binary(_)) {
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "binary selectors in cascades".to_string(),
                        location: "cascade message with binary selector".to_string(),
                    });
                }
                write!(
                    result,
                    "call 'beamtalk_message_dispatch':'send'({receiver_var}, '{selector_atom}', ["
                )
                .unwrap();

                // Arguments
                for (j, arg) in arguments.iter().enumerate() {
                    if j > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&self.capture_expression(arg)?);
                }

                result.push_str("])");

                if !is_last {
                    result.push_str(" in ");
                }
            }

            Ok(Document::String(result))
        } else {
            // Fallback: if the receiver is not a MessageSend (which should not
            // happen for well-formed cascades), preserve the previous behavior:
            // evaluate the receiver once and send all cascade messages to it.
            let mut result = String::new();
            let receiver_var = self.fresh_temp_var("Receiver");
            let recv_str = self.capture_expression(receiver)?;
            write!(result, "let {receiver_var} = ").unwrap();
            result.push_str(&recv_str);
            result.push_str(" in ");

            // Generate each message send, discarding intermediate results
            for (i, message) in messages.iter().enumerate() {
                let is_last = i == messages.len() - 1;

                if !is_last {
                    // For all but the last message, discard the result
                    result.push_str("let _ = ");
                }

                // Unified message dispatch to the bound receiver
                let selector_atom = message.selector.to_erlang_atom();
                if matches!(message.selector, MessageSelector::Binary(_)) {
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "binary selectors in cascades".to_string(),
                        location: "cascade message with binary selector".to_string(),
                    });
                }
                write!(
                    result,
                    "call 'beamtalk_message_dispatch':'send'({receiver_var}, '{selector_atom}', ["
                )
                .unwrap();

                // Arguments
                for (j, arg) in message.arguments.iter().enumerate() {
                    if j > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&self.capture_expression(arg)?);
                }

                result.push_str("])");

                if !is_last {
                    result.push_str(" in ");
                }
            }

            Ok(Document::String(result))
        }
    }

    /// Generates the body of a block with proper state threading.
    ///
    /// Handles field assignments specially to keep State{n} variables in scope
    /// for subsequent expressions. See inline comments for threading details.
    pub(super) fn generate_block_body(&mut self, block: &Block) -> Result<Document<'static>> {
        if block.body.is_empty() {
            return Ok(docvec!["'nil'"]);
        }

        // Generate body expressions in sequence
        // For state threading to work correctly, field assignments must leave
        // their let bindings OPEN so that State{n} is visible to subsequent expressions.
        //
        // For a block like: [self.value := self.value + 1. ^self.value]
        // We need:
        //   let _Val1 = ... in let State1 = ... in <return expression>
        // NOT:
        //   let _seq1 = (let _Val1 = ... in let State1 = ... in _Val1) in <return expression>
        //
        // The difference is crucial: in the first form, State1 is visible in <return expression>.
        //
        // Similarly, for local variable assignments like: [count := 0. count + 1]
        // We need:
        //   let Count = 0 in Count + 1
        // NOT:
        //   let _seq1 = 0 in <expression that can't see Count>

        let mut docs: Vec<Document<'static>> = Vec::new();

        for (i, expr) in block.body.iter().enumerate() {
            let is_last = i == block.body.len() - 1;
            let is_field_assignment = Self::is_field_assignment(expr);
            let is_local_assignment = Self::is_local_var_assignment(expr);

            if is_last {
                // Last expression: generate directly (its value is the block's result)
                docs.push(self.generate_expression(expr)?);
            } else if is_field_assignment {
                // Field assignment not at end: generate WITHOUT closing the value
                // This leaves the let bindings open for subsequent expressions
                docs.push(self.generate_field_assignment_open(expr)?);
            } else if is_local_assignment {
                // Local variable assignment: generate with proper binding
                if let Expression::Assignment { target, value, .. } = expr {
                    // Check if we're storing a block with mutations (ERROR)
                    if let Expression::Block(block) = value.as_ref() {
                        Self::validate_stored_closure(block, format!("{:?}", expr.span()))?;
                    }

                    if let Expression::Identifier(id) = target.as_ref() {
                        let var_name = &id.name;
                        // Determine the Core Erlang variable name:
                        // - If the variable is already bound (e.g. block parameter), reuse that Core var.
                        // - Otherwise, create a new Core Erlang variable name.
                        let core_var = self
                            .lookup_var(var_name)
                            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                        // Capture the value expression (preserves side effects)
                        // Important: capture BEFORE updating the mapping,
                        // so that any uses of the variable in the RHS see the previous binding.
                        let val_doc = self.expression_doc(value)?;
                        // Now update the mapping so subsequent expressions see this binding.
                        self.bind_var(var_name, &core_var);
                        docs.push(docvec![format!("let {core_var} = "), val_doc, " in "]);
                    }
                }
            } else if let Some(threaded_vars) = Self::get_control_flow_threaded_vars(expr) {
                // whileTrue:/whileFalse:/timesRepeat: with mutations - need to rebind threaded vars after loop
                // For single var, the loop returns its final value directly
                // For multiple vars, we'd need a tuple (not yet supported)
                if threaded_vars.len() == 1 {
                    let var = &threaded_vars[0];
                    // Get the Core Erlang variable name for this var
                    let core_var = self
                        .lookup_var(var)
                        .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                    let expr_doc = self.expression_doc(expr)?;
                    docs.push(docvec![format!("let {core_var} = "), expr_doc, " in "]);
                } else {
                    // Multi-var case not supported yet
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "Multiple threaded variables in control flow".to_string(),
                        location: "generate_block_body".to_string(),
                    });
                }
            } else {
                // Not an assignment or loop - generate and discard result
                let expr_doc = self.expression_doc(expr)?;
                docs.push(docvec!["let _Unit = ", expr_doc, " in "]);
            }
        }

        Ok(Document::Vec(docs))
    }

    /// Generates code for a match expression.
    ///
    /// `value match: [pattern -> body ...]` compiles to Core Erlang:
    /// `let _Match1 = <value> in case _Match1 of <Pattern1> when Guard1 -> Body1 ... end`
    pub(super) fn generate_match(
        &mut self,
        value: &Expression,
        arms: &[MatchArm],
    ) -> Result<Document<'static>> {
        if arms.is_empty() {
            return Err(CodeGenError::UnsupportedFeature {
                feature: "match expression with no arms".to_string(),
                location: format!("{:?}", value.span()),
            });
        }

        let match_var = self.fresh_temp_var("Match");
        let value_doc = self.expression_doc(value)?;

        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::String(format!("let {match_var} = ")));
        parts.push(value_doc);
        parts.push(Document::String(format!(" in case {match_var} of ")));

        for (i, arm) in arms.iter().enumerate() {
            if i > 0 {
                parts.push(Document::String(" ".to_string()));
            }

            // Generate pattern
            let pattern_doc = self.generate_pattern(&arm.pattern)?;
            parts.push(Document::String("<".to_string()));
            parts.push(pattern_doc);
            parts.push(Document::String(">".to_string()));

            // Push scope and bind pattern variables so the guard and body
            // can reference them directly instead of looking up the bindings map.
            self.push_scope();
            Self::collect_pattern_variables(&arm.pattern, |name, core_var| {
                self.bind_var(name, core_var);
            });

            // Generate guard
            if let Some(guard) = &arm.guard {
                parts.push(Document::String(" when ".to_string()));
                let guard_doc = self.generate_guard_expression(guard)?;
                parts.push(guard_doc);
            } else {
                parts.push(Document::String(" when 'true'".to_string()));
            }

            // Generate body
            parts.push(Document::String(" -> ".to_string()));
            let body_doc = self.expression_doc(&arm.body)?;
            parts.push(body_doc);

            self.pop_scope();
        }

        parts.push(Document::String(" end".to_string()));
        Ok(Document::Vec(parts))
    }

    /// Generates a Core Erlang pattern from a Pattern AST node.
    fn generate_pattern(&self, pattern: &Pattern) -> Result<Document<'static>> {
        match pattern {
            Pattern::Wildcard(_) => Ok(Document::String("_".to_string())),
            Pattern::Variable(id) => {
                let var_name = Self::to_core_erlang_var(&id.name);
                Ok(Document::String(var_name))
            }
            Pattern::Literal(lit, _) => self.generate_literal(lit),
            Pattern::Tuple { elements, .. } => {
                let mut parts = vec![Document::String("{".to_string())];
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::String(", ".to_string()));
                    }
                    parts.push(self.generate_pattern(elem)?);
                }
                parts.push(Document::String("}".to_string()));
                Ok(Document::Vec(parts))
            }
            Pattern::List { elements, tail, .. } => {
                if elements.is_empty() && tail.is_none() {
                    return Ok(Document::String("[]".to_string()));
                }
                let mut parts = vec![Document::String("[".to_string())];
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Document::String(", ".to_string()));
                    }
                    parts.push(self.generate_pattern(elem)?);
                }
                if let Some(tail_pat) = tail {
                    parts.push(Document::String(" | ".to_string()));
                    parts.push(self.generate_pattern(tail_pat)?);
                }
                parts.push(Document::String("]".to_string()));
                Ok(Document::Vec(parts))
            }
            Pattern::Binary { .. } => Err(CodeGenError::UnsupportedFeature {
                feature: "binary pattern matching".to_string(),
                location: format!("{:?}", pattern.span()),
            }),
        }
    }

    /// Generates a Core Erlang guard expression.
    ///
    /// Core Erlang guards only allow a restricted set of BIFs:
    /// comparisons, arithmetic, and type checks.
    fn generate_guard_expression(&mut self, expr: &Expression) -> Result<Document<'static>> {
        match expr {
            Expression::Literal(lit, _) => self.generate_literal(lit),
            Expression::Identifier(id) => match id.name.as_str() {
                "true" => Ok(Document::String("'true'".to_string())),
                "false" => Ok(Document::String("'false'".to_string())),
                "nil" => Ok(Document::String("'nil'".to_string())),
                _ => {
                    if let Some(var_name) = self.lookup_var(&id.name) {
                        Ok(Document::String(var_name.clone()))
                    } else {
                        let var_name = Self::to_core_erlang_var(&id.name);
                        Ok(Document::String(var_name))
                    }
                }
            },
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                let left = self.generate_guard_expression(receiver)?;
                let right = self.generate_guard_expression(&arguments[0])?;
                let erlang_op = match op.as_str() {
                    ">" => ">",
                    "<" => "<",
                    ">=" => ">=",
                    "<=" => "=<",
                    "=:=" => "=:=",
                    "/=" => "/=",
                    "=/=" => "=/=",
                    "+" => "+",
                    "-" => "-",
                    "*" => "*",
                    "/" => "/",
                    _ => {
                        return Err(CodeGenError::UnsupportedFeature {
                            feature: format!("operator '{op}' in guard expression"),
                            location: format!("{:?}", expr.span()),
                        });
                    }
                };
                Ok(docvec![
                    format!("call 'erlang':'{erlang_op}'("),
                    left,
                    ", ",
                    right,
                    ")",
                ])
            }
            _ => Err(CodeGenError::UnsupportedFeature {
                feature: "complex guard expression (only comparisons and arithmetic allowed)"
                    .to_string(),
                location: format!("{:?}", expr.span()),
            }),
        }
    }

    /// Collects all variable names from a pattern and calls the callback
    /// with the Beamtalk name and the corresponding Core Erlang variable name.
    fn collect_pattern_variables(pattern: &Pattern, mut bind: impl FnMut(&str, &str)) {
        Self::collect_pattern_variables_inner(pattern, &mut bind);
    }

    fn collect_pattern_variables_inner(pattern: &Pattern, bind: &mut impl FnMut(&str, &str)) {
        match pattern {
            Pattern::Variable(id) => {
                let core_var = Self::to_core_erlang_var(&id.name);
                bind(&id.name, &core_var);
            }
            Pattern::Tuple { elements, .. } | Pattern::List { elements, .. } => {
                for elem in elements {
                    Self::collect_pattern_variables_inner(elem, bind);
                }
                if let Pattern::List { tail: Some(t), .. } = pattern {
                    Self::collect_pattern_variables_inner(t, bind);
                }
            }
            Pattern::Wildcard(_) | Pattern::Literal(_, _) | Pattern::Binary { .. } => {}
        }
    }
}
