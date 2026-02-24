// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compiler intrinsics for language-level constructs.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! These are message patterns that the compiler must generate inline code for,
//! regardless of receiver type. They represent language-level semantics, not
//! type-specific dispatch:
//!
//! - **Block evaluation**: `value`, `whileTrue:`, `repeat` → Function application & loops
//! - **`ProtoObject`**: `class` → Type introspection via pattern matching
//! - **Object protocol** (split into domain-aligned groups):
//!   - **Nil protocol**: `isNil`, `notNil`, `ifNil:`, `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:` → Nil-testing boolean protocol
//!   - **Error signaling**: `error:` → Error construction and signaling
//!   - **Object identity**: `yourself`, `hash` → Identity and representation
//!   - **Object reflection**: `respondsTo:`, `fieldNames`, `fieldAt:`, `fieldAt:put:` → Runtime introspection
//! - **Dynamic dispatch**: `perform:`, `perform:withArguments:` → Runtime type-based dispatch (actors → async/Future, primitives → sync/value)
//!
//! Unlike type-specific dispatch (which goes through `beamtalk_primitive:send/3`
//! at runtime), these intrinsics generate efficient inline code because they are
//! fundamental language operations that cannot be deferred to runtime dispatch.

use super::document::Document;
use super::{CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{Expression, Literal, MessageSelector};
use crate::docvec;

/// Returns the arity of a block expression, or `None` if the expression is not a block literal.
fn block_arity(expr: &Expression) -> Option<usize> {
    match expr {
        Expression::Block(b) => Some(b.parameters.len()),
        _ => None,
    }
}

/// BT-493: Validates that a block has exactly the expected arity.
/// Non-literal blocks are assumed correct (can't check at compile time).
/// Returns `Ok(())` if valid, `Err(BlockArityError)` if wrong arity.
pub(in crate::codegen::core_erlang) fn validate_block_arity_exact(
    expr: &Expression,
    expected: usize,
    selector: &str,
    hint: &str,
) -> Result<()> {
    if let Some(actual) = block_arity(expr) {
        if actual != expected {
            return Err(CodeGenError::BlockArityError {
                selector: selector.to_string(),
                expected: expected.to_string(),
                actual,
                hint: hint.to_string(),
            });
        }
    }
    Ok(())
}

/// BT-493: Validates that a block has arity within a range (inclusive).
/// Non-literal blocks are assumed correct (can't check at compile time).
/// Returns `Ok(())` if valid, `Err(BlockArityError)` if out of range.
#[cfg(test)]
fn validate_block_arity_range(
    expr: &Expression,
    min: usize,
    max: usize,
    selector: &str,
    hint: &str,
) -> Result<()> {
    if let Some(actual) = block_arity(expr) {
        if actual < min || actual > max {
            return Err(CodeGenError::BlockArityError {
                selector: selector.to_string(),
                expected: format!("{min} or {max}"),
                actual,
                hint: hint.to_string(),
            });
        }
    }
    Ok(())
}

/// BT-493: Validates that an `on:do:` handler block has arity 0 or 1.
/// Returns `true` if the handler takes an argument (arity 1 or non-literal), `false` for arity 0.
/// Follows the same pattern as `validate_if_not_nil_block`.
pub(in crate::codegen::core_erlang) fn validate_on_do_handler(
    expr: &Expression,
    selector: &str,
) -> Result<bool> {
    match block_arity(expr) {
        Some(0) => Ok(false),
        Some(n) if n > 1 => Err(CodeGenError::BlockArityError {
            selector: selector.to_string(),
            expected: "0 or 1".to_string(),
            actual: n,
            hint: "Fix: The handler block must take 0 or 1 arguments (the exception):\n\
                   \x20 [...] on: Exception do: [:e | e message]\n\
                   \x20 [...] on: Exception do: ['error occurred']"
                .to_string(),
        }),
        // Arity 1 or non-literal block — pass the exception
        _ => Ok(true),
    }
}

/// Validates that an `ifNotNil:` block has arity 0 or 1.
/// Returns `true` if the block takes an argument (arity 1 or non-literal), `false` for arity 0.
fn validate_if_not_nil_block(expr: &Expression, selector: &str) -> Result<bool> {
    match block_arity(expr) {
        Some(0) => Ok(false),
        Some(n) if n > 1 => Err(CodeGenError::BlockArityMismatch {
            selector: selector.to_string(),
            arity: n,
        }),
        // Arity 1 or non-literal block — pass the receiver (Smalltalk convention)
        _ => Ok(true),
    }
}

/// Generates a Core Erlang `apply` call that adapts to block arity.
/// For 0-arg blocks: `apply BlockVar ()`, for 1-arg blocks: `apply BlockVar (RecvVar)`.
fn not_nil_apply(block_var: &str, recv_var: &str, block_takes_arg: bool) -> Document<'static> {
    if block_takes_arg {
        Document::String(format!("apply {block_var} ({recv_var})"))
    } else {
        Document::String(format!("apply {block_var} ()"))
    }
}

impl CoreErlangGenerator {
    /// Tries to generate code for Block evaluation and loop intrinsics.
    ///
    /// These are structural intrinsics that the compiler generates inline code for.
    /// This function:
    ///
    /// - Returns `Ok(Some(doc))` if the message was an intrinsic and code was generated
    /// - Returns `Ok(None)` if the message is NOT an intrinsic (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Block Evaluation
    ///
    /// - `value` (0 args) → evaluate block with no arguments
    /// - `value:` (1 arg) → evaluate block with one argument
    /// - `value:value:` (2 args) → evaluate block with two arguments  
    /// - `value:value:value:` (3 args) → evaluate block with three arguments
    ///
    /// # Loop Constructs
    ///
    /// - `repeat` (0 args) → infinite loop
    /// - `whileTrue:` (1 arg) → loop while condition block returns true
    /// - `whileFalse:` (1 arg) → loop while condition block returns false
    /// - `timesRepeat:` (1 arg) → repeat body N times
    /// - `to:do:` (2 args) → range iteration from start to end
    /// - `to:by:do:` (3 args) → range iteration with custom step
    pub(in crate::codegen::core_erlang) fn try_generate_block_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector {
            // `value` - evaluate block with no arguments
            // BT-335: When the receiver is a block literal, use fast inline apply.
            // For other receivers, generate a runtime type check to handle both
            // blocks (apply) and non-blocks (runtime dispatch via send).
            MessageSelector::Unary(name) if name == "value" => {
                let doc = if matches!(receiver, Expression::Block { .. }) {
                    self.generate_block_value_call(receiver, &[])?
                } else {
                    let recv_var = self.fresh_temp_var("ValRecv");
                    let recv_code = self.expression_doc(receiver)?;
                    docvec![
                        format!("let {recv_var} = "),
                        recv_code,
                        format!(
                            " in case call 'erlang':'is_function'({recv_var}) of \
                             'true' when 'true' -> apply {recv_var} () \
                             'false' when 'true' -> \
                             call 'beamtalk_primitive':'send'({recv_var}, 'value', []) end"
                        ),
                    ]
                };
                Ok(Some(doc))
            }

            // `repeat` - infinite loop
            MessageSelector::Unary(name) if name == "repeat" => {
                let doc = self.generate_repeat(receiver)?;
                Ok(Some(doc))
            }

            // Keyword messages for block evaluation
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    // `value:`, `value:value:`, `value:value:value:` - evaluate block with args
                    "value:" | "value:value:" | "value:value:value:" => {
                        // BT-851: Check if receiver is a Tier 2 block parameter
                        if let Expression::Identifier(id) = receiver {
                            if self.tier2_block_params.contains(id.name.as_str()) {
                                let doc =
                                    self.generate_block_value_call_stateful(receiver, arguments)?;
                                return Ok(Some(doc));
                            }
                        }
                        let doc = self.generate_block_value_call(receiver, arguments)?;
                        Ok(Some(doc))
                    }

                    // `whileTrue:` - loop while condition block returns true
                    "whileTrue:" => {
                        let doc = self.generate_while_true(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }

                    // `whileFalse:` - loop while condition block returns false
                    "whileFalse:" => {
                        let doc = self.generate_while_false(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }

                    // `timesRepeat:` - repeat body N times
                    "timesRepeat:" => {
                        let doc = self.generate_times_repeat(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }

                    // `to:do:` - range iteration (structural intrinsic from Integer)
                    "to:do:" if arguments.len() == 2 => {
                        let doc = self.generate_to_do(receiver, &arguments[0], &arguments[1])?;
                        Ok(Some(doc))
                    }

                    // `to:by:do:` - range iteration with step (structural intrinsic from Integer)
                    "to:by:do:" if arguments.len() == 3 => {
                        let doc = self.generate_to_by_do(
                            receiver,
                            &arguments[0],
                            &arguments[1],
                            &arguments[2],
                        )?;
                        Ok(Some(doc))
                    }

                    // `on:do:` - exception handling (try/catch with class matching)
                    "on:do:" if arguments.len() == 2 => {
                        let doc = self.generate_on_do(receiver, &arguments[0], &arguments[1])?;
                        Ok(Some(doc))
                    }

                    // `ensure:` - cleanup (try/after via try/catch + re-raise)
                    "ensure:" => {
                        let doc = self.generate_ensure(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }

                    // Not a block evaluation message
                    _ => Ok(None),
                }
            }

            // Not a block evaluation message
            _ => Ok(None),
        }
    }

    /// Tries to generate code for List/Array methods.
    ///
    /// List methods are structural intrinsics that require inline code generation
    /// for proper state threading when used inside actor methods with field mutations.
    ///
    /// **BT-416**: This intrinsic now checks the receiver type to avoid intercepting
    /// String primitive methods. String literals use `@primitive` codegen that delegates
    /// to `beamtalk_string_ops`, not `lists:map/filter`.
    ///
    /// - Returns `Ok(Some(doc))` if the message was a List method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a List method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # List Methods
    ///
    /// - `do:` (1 arg block) → iterate over elements with side effects
    /// - `collect:` (1 arg block) → map to new list
    /// - `select:` (1 arg block) → filter elements
    /// - `reject:` (1 arg block) → filter out elements
    /// - `inject:into:` (2 args) → fold with accumulator
    pub(in crate::codegen::core_erlang) fn try_generate_list_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // String has its own @primitive implementations (collect:, select:, etc.)
        // that delegate to beamtalk_string_ops, not lists:map/filter.
        if matches!(receiver, Expression::Literal(Literal::String(_), _)) {
            return Ok(None);
        }

        match selector {
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "do:" if arguments.len() == 1 => {
                        let doc = self.generate_list_do(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "collect:" if arguments.len() == 1 => {
                        let doc = self.generate_list_collect(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "select:" if arguments.len() == 1 => {
                        let doc = self.generate_list_select(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "reject:" if arguments.len() == 1 => {
                        let doc = self.generate_list_reject(receiver, &arguments[0])?;
                        Ok(Some(doc))
                    }
                    "inject:into:" if arguments.len() == 2 => {
                        let doc =
                            self.generate_list_inject(receiver, &arguments[0], &arguments[1])?;
                        Ok(Some(doc))
                    }
                    // BT-493: Validate detect:ifNone: block arities even though it
                    // dispatches at runtime (detect: 1-arg, ifNone: 0-arg)
                    "detect:ifNone:" if arguments.len() == 2 => {
                        validate_block_arity_exact(
                            &arguments[0],
                            1,
                            "detect:ifNone:",
                            "Fix: The detect block must take one argument (each element):\n\
                             \x20 list detect: [:item | item > 0] ifNone: ['not found']",
                        )?;
                        validate_block_arity_exact(
                            &arguments[1],
                            0,
                            "detect:ifNone:",
                            "Fix: The ifNone block must take no arguments:\n\
                             \x20 list detect: [:item | item > 0] ifNone: ['not found']",
                        )?;
                        Ok(None)
                    }
                    _ => Ok(None),
                }
            }

            _ => Ok(None),
        }
    }

    /// Generates a block value call: `let _Fun = <receiver> in apply _Fun (Args...)`.
    ///
    /// In Core Erlang, we bind the receiver to a variable first to ensure proper
    /// evaluation order and handle complex receiver expressions correctly.
    ///
    /// ```erlang
    /// let _Fun1 = <receiver-expr> in apply _Fun1 (Arg1, Arg2, ...)
    /// ```
    pub(in crate::codegen::core_erlang) fn generate_block_value_call(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let fun_var = self.fresh_temp_var("Fun");
        let recv_code = self.expression_doc(receiver)?;

        let mut arg_parts: Vec<Document<'static>> = Vec::new();
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                arg_parts.push(Document::Str(", "));
            }
            arg_parts.push(self.expression_doc(arg)?);
        }
        let args_doc = Document::Vec(arg_parts);

        let doc = docvec![
            format!("let {fun_var} = "),
            recv_code,
            format!(" in apply {fun_var} ("),
            args_doc,
            ")",
        ];
        Ok(doc)
    }

    /// BT-851: Generates a Tier 2 stateful block value call (ADR 0041 Phase 0).
    ///
    /// Calls a Tier 2 block using the stateful protocol:
    /// `apply _Fun(Args..., State) → {Result, NewState}`
    ///
    /// Returns a `{Result, NewState}` tuple (like control flow with mutations).
    /// The method body generator must unpack this tuple to extract the result
    /// and thread the new state through to the reply tuple.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _Fun = <receiver> in apply _Fun (Arg1, ..., ArgN, State)
    /// ```
    ///
    /// Returns `{Result, NewState}` tuple from the block.
    pub(in crate::codegen::core_erlang) fn generate_block_value_call_stateful(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let fun_var = self.fresh_temp_var("Fun");
        let recv_code = self.expression_doc(receiver)?;
        let current_state = self.current_state_var();

        let mut arg_parts: Vec<Document<'static>> = Vec::new();
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                arg_parts.push(Document::Str(", "));
            }
            arg_parts.push(self.expression_doc(arg)?);
        }
        // Append State as last argument
        if !arguments.is_empty() {
            arg_parts.push(Document::Str(", "));
        }
        arg_parts.push(Document::String(current_state));
        let args_doc = Document::Vec(arg_parts);

        let doc = docvec![
            "let ",
            Document::String(fun_var.clone()),
            " = ",
            recv_code,
            " in apply ",
            Document::String(fun_var),
            " (",
            args_doc,
            ")",
        ];
        Ok(doc)
    }

    /// Tries to generate code for `ProtoObject` methods.
    ///
    /// `ProtoObject` methods are fundamental operations available on all objects:
    ///
    /// - `class` - Returns the class name (atom) of the receiver
    /// - `perform:withArguments:` - Dynamic message dispatch
    ///
    /// This function:
    /// - Returns `Ok(Some(doc))` if the message was a `ProtoObject` method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a `ProtoObject` method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # `ProtoObject` Methods
    ///
    /// ## class
    ///
    /// Returns the class name of any object. For primitives (Integer, String, Boolean),
    /// uses pattern matching. For actors, extracts the class from the object record.
    ///
    /// ```core-erlang
    /// % For primitives:
    /// case Receiver of
    ///   <I> when call 'erlang':'is_integer'(I) -> 'Integer'
    ///   <S> when call 'erlang':'is_binary'(S) -> 'String'
    ///   <'true'> when 'true' -> 'True'
    ///   <'false'> when 'true' -> 'False'
    ///   <'nil'> when 'true' -> 'Nil'
    ///   <Obj> when 'true' -> call 'erlang':'element'(2, Obj)  % Extract from record
    /// end
    /// ```
    ///
    /// ## perform:withArguments:
    ///
    /// Performs dynamic message dispatch - sends a message to an object at runtime.
    /// This is used by doesNotUnderstand handlers to forward messages.
    ///
    /// ```core-erlang
    /// % For actors (objects):
    /// let Pid = call 'erlang':'element'(4, Receiver) in
    /// let Future = call 'beamtalk_future':'new'() in
    /// let _ = call 'beamtalk_actor':'async_send'(Pid, Selector, Arguments, Future) in
    /// Future
    /// ```
    #[allow(clippy::too_many_lines)] // one arm per ProtoObject intrinsic message
    pub(in crate::codegen::core_erlang) fn try_generate_protoobject_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "class" if arguments.is_empty() => {
                    // BT-412: Return class as first-class object (#beamtalk_object{})
                    let recv_var = self.fresh_temp_var("Obj");
                    let recv_code = self.expression_doc(receiver)?;
                    let doc = docvec![
                        format!("let {recv_var} = "),
                        recv_code,
                        format!(" in call 'beamtalk_primitive':'class_of_object'({recv_var})"),
                    ];
                    Ok(Some(doc))
                }
                _ => Ok(None),
            },
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "perform:withArguments:" if arguments.len() == 2 => {
                        let receiver_var = self.fresh_var("Receiver");
                        let selector_var = self.fresh_var("Selector");
                        let args_var = self.fresh_var("Args");

                        let recv_code = self.expression_doc(receiver)?;
                        let sel_code = self.expression_doc(&arguments[0])?;
                        let args_code = self.expression_doc(&arguments[1])?;

                        let doc = docvec![
                            format!("let {receiver_var} = "),
                            recv_code,
                            format!(" in let {selector_var} = "),
                            sel_code,
                            format!(" in let {args_var} = "),
                            args_code,
                            format!(
                                " in call 'beamtalk_message_dispatch':'send'({receiver_var}, {selector_var}, {args_var})"
                            ),
                        ];
                        Ok(Some(doc))
                    }
                    "perform:" if arguments.len() == 1 => {
                        let receiver_var = self.fresh_var("Receiver");
                        let selector_var = self.fresh_var("Selector");

                        let recv_code = self.expression_doc(receiver)?;
                        let sel_code = self.expression_doc(&arguments[0])?;

                        let doc = docvec![
                            format!("let {receiver_var} = "),
                            recv_code,
                            format!(" in let {selector_var} = "),
                            sel_code,
                            format!(
                                " in call 'beamtalk_message_dispatch':'send'({receiver_var}, {selector_var}, [])"
                            ),
                        ];
                        Ok(Some(doc))
                    }
                    _ => Ok(None),
                }
            }
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Tries to generate code for Object protocol methods.
    ///
    /// **DDD Context:** Object Protocol — Orchestrator
    ///
    /// Delegates to domain-aligned intrinsic groups:
    /// - Nil protocol (`isNil`, `notNil`, `ifNil:`, `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:`)
    /// - Error signaling (`error:`)
    /// - Object identity (`yourself`, `hash`)
    /// - Object reflection (`respondsTo:`, `fieldNames`, `fieldAt:`, `fieldAt:put:`)
    ///
    /// - Returns `Ok(Some(doc))` if the message was an Object method and code was generated
    /// - Returns `Ok(None)` if the message is NOT an Object method (caller should continue)
    /// - Returns `Err(...)` on error
    pub(in crate::codegen::core_erlang) fn try_generate_object_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if let Some(doc) = self.try_generate_nil_protocol(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        if let Some(doc) = self.try_generate_error_signaling(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        if let Some(doc) = self.try_generate_object_identity(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        if let Some(doc) = self.try_generate_object_reflection(receiver, selector, arguments)? {
            return Ok(Some(doc));
        }
        Ok(None)
    }

    /// Generates code for nil-testing protocol methods.
    ///
    /// **DDD Context:** Object Protocol — Nil Testing
    ///
    /// - `isNil` — Returns true only for nil, false for everything else
    /// - `notNil` — Returns false only for nil, true for everything else
    /// - `ifNil:` — Conditional execution if nil
    /// - `ifNotNil:` — Conditional execution if not nil
    /// - `ifNil:ifNotNil:` / `ifNotNil:ifNil:` — Two-way conditional
    #[expect(
        clippy::too_many_lines,
        reason = "Six nil-testing variants (isNil, notNil, ifNil:, ifNotNil:, ifNil:ifNotNil:, ifNotNil:ifNil:) in single match"
    )]
    fn try_generate_nil_protocol(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "isNil" if arguments.is_empty() => {
                    let recv_var = self.fresh_temp_var("Obj");
                    let recv_code = self.expression_doc(receiver)?;
                    let doc = docvec![
                        format!("let {recv_var} = "),
                        recv_code,
                        format!(
                            " in case {recv_var} of <'nil'> when 'true' -> 'true' <_> when 'true' -> 'false' end"
                        ),
                    ];
                    Ok(Some(doc))
                }
                "notNil" if arguments.is_empty() => {
                    let recv_var = self.fresh_temp_var("Obj");
                    let recv_code = self.expression_doc(receiver)?;
                    let doc = docvec![
                        format!("let {recv_var} = "),
                        recv_code,
                        format!(
                            " in case {recv_var} of <'nil'> when 'true' -> 'false' <_> when 'true' -> 'true' end"
                        ),
                    ];
                    Ok(Some(doc))
                }
                _ => Ok(None),
            },
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "ifNil:" if arguments.len() == 1 => {
                        let recv_var = self.fresh_temp_var("Obj");
                        let block_var = self.fresh_temp_var("NilBlk");
                        let recv_code = self.expression_doc(receiver)?;
                        let block_code = self.expression_doc(&arguments[0])?;
                        let doc = docvec![
                            format!("let {recv_var} = "),
                            recv_code,
                            format!(" in let {block_var} = "),
                            block_code,
                            format!(
                                " in case {recv_var} of <'nil'> when 'true' -> apply {block_var} () <_> when 'true' -> {recv_var} end"
                            ),
                        ];
                        Ok(Some(doc))
                    }
                    "ifNotNil:" if arguments.len() == 1 => {
                        // If the block has 0 parameters, don't pass the receiver (avoids badarity)
                        let recv_var = self.fresh_temp_var("Obj");
                        let block_var = self.fresh_temp_var("NotNilBlk");
                        let block_takes_arg =
                            validate_if_not_nil_block(&arguments[0], "ifNotNil:")?;
                        let recv_code = self.expression_doc(receiver)?;
                        let block_code = self.expression_doc(&arguments[0])?;
                        let apply = not_nil_apply(&block_var, &recv_var, block_takes_arg);
                        let doc = docvec![
                            format!("let {recv_var} = "),
                            recv_code,
                            format!(" in let {block_var} = "),
                            block_code,
                            format!(
                                " in case {recv_var} of <'nil'> when 'true' -> 'nil' <_> when 'true' -> "
                            ),
                            apply,
                            " end",
                        ];
                        Ok(Some(doc))
                    }
                    "ifNil:ifNotNil:" if arguments.len() == 2 => {
                        // If the notNil block has 0 parameters, don't pass the receiver
                        let recv_var = self.fresh_temp_var("Obj");
                        let nil_var = self.fresh_temp_var("NilBlk");
                        let not_nil_var = self.fresh_temp_var("NotNilBlk");
                        let block_takes_arg =
                            validate_if_not_nil_block(&arguments[1], "ifNil:ifNotNil:")?;
                        let recv_code = self.expression_doc(receiver)?;
                        let nil_code = self.expression_doc(&arguments[0])?;
                        let not_nil_code = self.expression_doc(&arguments[1])?;
                        let apply = not_nil_apply(&not_nil_var, &recv_var, block_takes_arg);
                        let doc = docvec![
                            format!("let {recv_var} = "),
                            recv_code,
                            format!(" in let {nil_var} = "),
                            nil_code,
                            format!(" in let {not_nil_var} = "),
                            not_nil_code,
                            format!(
                                " in case {recv_var} of <'nil'> when 'true' -> apply {nil_var} () <_> when 'true' -> "
                            ),
                            apply,
                            " end",
                        ];
                        Ok(Some(doc))
                    }
                    "ifNotNil:ifNil:" if arguments.len() == 2 => {
                        // If the notNil block has 0 parameters, don't pass the receiver
                        let recv_var = self.fresh_temp_var("Obj");
                        let not_nil_var = self.fresh_temp_var("NotNilBlk");
                        let nil_var = self.fresh_temp_var("NilBlk");
                        let block_takes_arg =
                            validate_if_not_nil_block(&arguments[0], "ifNotNil:ifNil:")?;
                        let recv_code = self.expression_doc(receiver)?;
                        let not_nil_code = self.expression_doc(&arguments[0])?;
                        let nil_code = self.expression_doc(&arguments[1])?;
                        let apply = not_nil_apply(&not_nil_var, &recv_var, block_takes_arg);
                        let doc = docvec![
                            format!("let {recv_var} = "),
                            recv_code,
                            format!(" in let {not_nil_var} = "),
                            not_nil_code,
                            format!(" in let {nil_var} = "),
                            nil_code,
                            format!(
                                " in case {recv_var} of <'nil'> when 'true' -> apply {nil_var} () <_> when 'true' -> "
                            ),
                            apply,
                            " end",
                        ];
                        Ok(Some(doc))
                    }
                    _ => Ok(None),
                }
            }
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Generates code for error signaling methods.
    ///
    /// **DDD Context:** Object Protocol — Error Signaling
    ///
    /// - `error:` — Smalltalk-style error signaling with receiver's class
    fn try_generate_error_signaling(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector {
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "error:" if arguments.len() == 1 => {
                        let recv_var = self.fresh_temp_var("Obj");
                        let msg_var = self.fresh_temp_var("Msg");
                        let class_var = self.fresh_temp_var("Class");
                        let err0 = self.fresh_temp_var("Err");
                        let err1 = self.fresh_temp_var("Err");

                        let recv_code = self.expression_doc(receiver)?;
                        let msg_code = self.expression_doc(&arguments[0])?;

                        let doc = docvec![
                            format!("let {recv_var} = "),
                            recv_code,
                            format!(" in let {msg_var} = "),
                            msg_code,
                            format!(
                                " in let {class_var} = call 'beamtalk_primitive':'class_of'({recv_var}) in \
                                 let {err0} = call 'beamtalk_error':'new'('user_error', {class_var}) in \
                                 let {err1} = call 'beamtalk_error':'with_message'({err0}, {msg_var}) in \
                                 call 'beamtalk_error':'raise'({err1})"
                            ),
                        ];
                        Ok(Some(doc))
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    /// Generates code for object identity and representation methods.
    ///
    /// **DDD Context:** Object Protocol — Object Identity
    ///
    /// - `yourself` — Identity: returns the receiver unchanged
    /// - `hash` — Hash using `erlang:phash2/1`
    fn try_generate_object_identity(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "yourself" if arguments.is_empty() => {
                    // Identity: just return the receiver
                    let recv_code = self.expression_doc(receiver)?;
                    Ok(Some(docvec![recv_code]))
                }
                "hash" if arguments.is_empty() => {
                    let recv_var = self.fresh_temp_var("Obj");
                    let recv_code = self.expression_doc(receiver)?;
                    let doc = docvec![
                        format!("let {recv_var} = "),
                        recv_code,
                        format!(" in call 'erlang':'phash2'({recv_var})"),
                    ];
                    Ok(Some(doc))
                }
                // BT-477: printString removed as intrinsic — now uses polymorphic
                // dispatch via Object >> printString and per-class overrides.
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    /// Generates code for object reflection and introspection methods.
    ///
    /// **DDD Context:** Object Protocol — Object Reflection
    ///
    /// - `respondsTo:` — Check if object responds to a selector
    /// - `fieldNames` — Get list of instance variable names (actors only)
    /// - `fieldAt:` — Read instance variable by name
    /// - `fieldAt:put:` — Write instance variable by name
    #[expect(
        clippy::too_many_lines,
        reason = "fieldAt: and fieldAt:put: require verbose type guards for actor vs primitive dispatch"
    )]
    fn try_generate_object_reflection(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "fieldNames" if arguments.is_empty() => {
                    let receiver_var = self.fresh_var("Receiver");
                    let pid_var = self.fresh_var("Pid");
                    let future_var = self.fresh_var("Future");
                    let future_pid_var = self.fresh_var("FuturePid");

                    let recv_code = self.expression_doc(receiver)?;

                    let doc = docvec![
                        format!("let {receiver_var} = "),
                        recv_code,
                        format!(
                            " in let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in \
                                 let {future_var} = call 'beamtalk_future':'new'() in \
                                 let {future_pid_var} = call 'beamtalk_future':'pid'({future_var}) in \
                                 let _ = call 'beamtalk_actor':'async_send'({pid_var}, 'fieldNames', [], {future_pid_var}) in \
                                 {future_var}"
                        ),
                    ];
                    Ok(Some(doc))
                }
                _ => Ok(None),
            },
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "respondsTo:" if arguments.len() == 1 => {
                        let receiver_var = self.fresh_var("Receiver");
                        let selector_var = self.fresh_var("Selector");

                        let recv_code = self.expression_doc(receiver)?;
                        let sel_code = self.expression_doc(&arguments[0])?;

                        let doc = docvec![
                            format!("let {receiver_var} = "),
                            recv_code,
                            format!(" in let {selector_var} = "),
                            sel_code,
                            format!(
                                " in call 'beamtalk_primitive':'responds_to'({receiver_var}, {selector_var})"
                            ),
                        ];
                        Ok(Some(doc))
                    }
                    "fieldAt:" if arguments.len() == 1 => {
                        let receiver_var = self.fresh_var("Receiver");
                        let name_var = self.fresh_var("Name");
                        let pid_var = self.fresh_var("Pid");
                        let future_var = self.fresh_var("Future");
                        let future_pid_var = self.fresh_var("FuturePid");
                        let class_var = self.fresh_var("Class");
                        let error_base = self.fresh_var("Err");
                        let error_sel = self.fresh_var("Err");
                        let error_hint = self.fresh_var("Err");
                        let hint =
                            Self::binary_string_literal("Value types have no instance variables");

                        let recv_code = self.expression_doc(receiver)?;
                        let name_code = self.expression_doc(&arguments[0])?;

                        let doc = docvec![
                            format!("let {receiver_var} = "),
                            recv_code,
                            format!(" in let {name_var} = "),
                            name_code,
                            format!(
                                " in case case call 'erlang':'is_tuple'({receiver_var}) of \
                                 <'true'> when 'true' -> \
                                 case call 'erlang':'=='(call 'erlang':'tuple_size'({receiver_var}), 4) of \
                                 <'true'> when 'true' -> \
                                 call 'erlang':'=='(call 'erlang':'element'(1, {receiver_var}), 'beamtalk_object') \
                                 <_> when 'true' -> 'false' end \
                                 <_> when 'true' -> 'false' end of "
                            ),
                            format!(
                                "<'true'> when 'true' -> \
                                 let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in \
                                 let {future_var} = call 'beamtalk_future':'new'() in \
                                 let {future_pid_var} = call 'beamtalk_future':'pid'({future_var}) in \
                                 let _ = call 'beamtalk_actor':'async_send'({pid_var}, 'fieldAt:', [{name_var}], {future_pid_var}) in \
                                 {future_var} "
                            ),
                            format!(
                                "<_> when 'true' -> \
                                 let {class_var} = call 'beamtalk_primitive':'class_of'({receiver_var}) in \
                                 let {error_base} = call 'beamtalk_error':'new'('immutable_value', {class_var}) in \
                                 let {error_sel} = call 'beamtalk_error':'with_selector'({error_base}, 'fieldAt:') in \
                                 let {error_hint} = call 'beamtalk_error':'with_hint'({error_sel}, {hint}) in \
                                 call 'beamtalk_error':'raise'({error_hint}) \
                                 end"
                            ),
                        ];
                        Ok(Some(doc))
                    }
                    "fieldAt:put:" if arguments.len() == 2 => {
                        let receiver_var = self.fresh_var("Receiver");
                        let name_var = self.fresh_var("Name");
                        let value_var = self.fresh_var("Value");
                        let pid_var = self.fresh_var("Pid");
                        let future_var = self.fresh_var("Future");
                        let future_pid_var = self.fresh_var("FuturePid");
                        let class_var = self.fresh_var("Class");
                        let error_base = self.fresh_var("Err");
                        let error_sel = self.fresh_var("Err");
                        let error_hint = self.fresh_var("Err");
                        let hint = Self::binary_string_literal(
                            "Value types are immutable. Use a method that returns a new instance instead.",
                        );

                        let recv_code = self.expression_doc(receiver)?;
                        let name_code = self.expression_doc(&arguments[0])?;
                        let value_code = self.expression_doc(&arguments[1])?;

                        let doc = docvec![
                            format!("let {receiver_var} = "),
                            recv_code,
                            format!(" in let {name_var} = "),
                            name_code,
                            format!(" in let {value_var} = "),
                            value_code,
                            format!(
                                " in case case call 'erlang':'is_tuple'({receiver_var}) of \
                                 <'true'> when 'true' -> \
                                 case call 'erlang':'=='(call 'erlang':'tuple_size'({receiver_var}), 4) of \
                                 <'true'> when 'true' -> \
                                 call 'erlang':'=='(call 'erlang':'element'(1, {receiver_var}), 'beamtalk_object') \
                                 <_> when 'true' -> 'false' end \
                                 <_> when 'true' -> 'false' end of "
                            ),
                            format!(
                                "<'true'> when 'true' -> \
                                 let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in \
                                 let {future_var} = call 'beamtalk_future':'new'() in \
                                 let {future_pid_var} = call 'beamtalk_future':'pid'({future_var}) in \
                                 let _ = call 'beamtalk_actor':'async_send'({pid_var}, 'fieldAt:put:', [{name_var}, {value_var}], {future_pid_var}) in \
                                 {future_var} "
                            ),
                            format!(
                                "<_> when 'true' -> \
                                 let {class_var} = call 'beamtalk_primitive':'class_of'({receiver_var}) in \
                                 let {error_base} = call 'beamtalk_error':'new'('immutable_value', {class_var}) in \
                                 let {error_sel} = call 'beamtalk_error':'with_selector'({error_base}, 'fieldAt:put:') in \
                                 let {error_hint} = call 'beamtalk_error':'with_hint'({error_sel}, {hint}) in \
                                 call 'beamtalk_error':'raise'({error_hint}) \
                                 end"
                            ),
                        ];
                        Ok(Some(doc))
                    }
                    _ => Ok(None),
                }
            }
            MessageSelector::Binary(_) => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, BlockParameter, Literal};
    use crate::source_analysis::Span;

    #[test]
    fn test_validate_if_not_nil_block_zero_args() {
        let block = Expression::Block(Block {
            parameters: vec![],
            body: vec![Expression::Literal(Literal::Integer(1), Span::new(1, 2))],
            span: Span::new(0, 3),
        });
        assert!(!validate_if_not_nil_block(&block, "ifNotNil:").unwrap());
    }

    #[test]
    fn test_validate_if_not_nil_block_one_arg() {
        let block = Expression::Block(Block {
            parameters: vec![BlockParameter::new("v", Span::new(1, 2))],
            body: vec![Expression::Literal(Literal::Integer(1), Span::new(5, 6))],
            span: Span::new(0, 7),
        });
        assert!(validate_if_not_nil_block(&block, "ifNotNil:").unwrap());
    }

    #[test]
    fn test_validate_if_not_nil_block_two_args_errors() {
        let block = Expression::Block(Block {
            parameters: vec![
                BlockParameter::new("a", Span::new(1, 2)),
                BlockParameter::new("b", Span::new(3, 4)),
            ],
            body: vec![Expression::Literal(Literal::Integer(1), Span::new(7, 8))],
            span: Span::new(0, 9),
        });
        let result = validate_if_not_nil_block(&block, "ifNotNil:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityMismatch { selector, arity }) = result {
            assert_eq!(selector, "ifNotNil:");
            assert_eq!(arity, 2);
        } else {
            panic!("Expected BlockArityMismatch error");
        }
    }

    #[test]
    fn test_validate_if_not_nil_block_non_literal() {
        // Non-block expression (variable) — assumes 1 arg
        let expr = Expression::Identifier(crate::ast::Identifier::new("myBlock", Span::new(0, 7)));
        assert!(validate_if_not_nil_block(&expr, "ifNotNil:").unwrap());
    }

    // BT-493: Tests for validate_block_arity_exact

    fn make_block(arity: usize) -> Expression {
        let params: Vec<BlockParameter> = (0..arity)
            .map(|i| {
                #[allow(clippy::cast_possible_truncation)]
                // synthetic span positions for generated block params
                let span = Span::new((i * 2 + 1) as u32, (i * 2 + 2) as u32);
                BlockParameter::new(format!("p{i}"), span)
            })
            .collect();
        Expression::Block(Block {
            parameters: params,
            body: vec![Expression::Literal(Literal::Integer(1), Span::new(1, 2))],
            span: Span::new(0, 10),
        })
    }

    #[test]
    fn test_validate_exact_correct_arity() {
        assert!(validate_block_arity_exact(&make_block(0), 0, "timesRepeat:", "hint").is_ok());
        assert!(validate_block_arity_exact(&make_block(1), 1, "to:do:", "hint").is_ok());
        assert!(validate_block_arity_exact(&make_block(2), 2, "inject:into:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_wrong_arity() {
        let result = validate_block_arity_exact(&make_block(1), 0, "timesRepeat:", "use to:do:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError {
            selector,
            expected,
            actual,
            hint,
        }) = result
        {
            assert_eq!(selector, "timesRepeat:");
            assert_eq!(expected, "0");
            assert_eq!(actual, 1);
            assert!(hint.contains("to:do:"));
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_exact_non_literal_passes() {
        // Non-block expressions can't be checked at compile time — always pass
        let expr = Expression::Identifier(crate::ast::Identifier::new("myBlock", Span::new(0, 7)));
        assert!(validate_block_arity_exact(&expr, 0, "timesRepeat:", "hint").is_ok());
        assert!(validate_block_arity_exact(&expr, 1, "do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_times_repeat_zero_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(0), 0, "timesRepeat:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_times_repeat_one_arg_error() {
        let result = validate_block_arity_exact(&make_block(1), 0, "timesRepeat:", "hint");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_exact_to_do_one_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(1), 1, "to:do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_to_do_zero_arg_error() {
        let result = validate_block_arity_exact(&make_block(0), 1, "to:do:", "hint");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_exact_inject_into_two_arg_ok() {
        assert!(validate_block_arity_exact(&make_block(2), 2, "inject:into:", "hint").is_ok());
    }

    #[test]
    fn test_validate_exact_inject_into_one_arg_error() {
        let result = validate_block_arity_exact(&make_block(1), 2, "inject:into:", "hint");
        assert!(result.is_err());
    }

    // BT-493: Tests for validate_block_arity_range

    #[test]
    fn test_validate_range_within_bounds() {
        assert!(validate_block_arity_range(&make_block(0), 0, 1, "on:do:", "hint").is_ok());
        assert!(validate_block_arity_range(&make_block(1), 0, 1, "on:do:", "hint").is_ok());
    }

    #[test]
    fn test_validate_range_out_of_bounds() {
        let result = validate_block_arity_range(&make_block(2), 0, 1, "on:do:", "hint");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError {
            selector,
            expected,
            actual,
            ..
        }) = result
        {
            assert_eq!(selector, "on:do:");
            assert_eq!(expected, "0 or 1");
            assert_eq!(actual, 2);
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_range_non_literal_passes() {
        let expr = Expression::Identifier(crate::ast::Identifier::new("myBlock", Span::new(0, 7)));
        assert!(validate_block_arity_range(&expr, 0, 1, "on:do:", "hint").is_ok());
    }

    // BT-493: Tests for validate_on_do_handler

    #[test]
    fn test_validate_on_do_handler_zero_args() {
        // 0-arg handler: valid, returns false (don't pass exception)
        assert!(!validate_on_do_handler(&make_block(0), "on:do:").unwrap());
    }

    #[test]
    fn test_validate_on_do_handler_one_arg() {
        // 1-arg handler: valid, returns true (pass exception)
        assert!(validate_on_do_handler(&make_block(1), "on:do:").unwrap());
    }

    #[test]
    fn test_validate_on_do_handler_two_args_errors() {
        let result = validate_on_do_handler(&make_block(2), "on:do:");
        assert!(result.is_err());
        if let Err(CodeGenError::BlockArityError { actual, .. }) = result {
            assert_eq!(actual, 2);
        } else {
            panic!("Expected BlockArityError");
        }
    }

    #[test]
    fn test_validate_on_do_handler_non_literal() {
        // Non-block expression — assumes 1 arg (pass exception)
        let expr = Expression::Identifier(crate::ast::Identifier::new("myBlock", Span::new(0, 7)));
        assert!(validate_on_do_handler(&expr, "on:do:").unwrap());
    }
}
