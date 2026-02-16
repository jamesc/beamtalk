// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Message sending and dispatch compilation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This domain service handles the **core domain operation** of Beamtalk: message
//! sending. In Smalltalk and Beamtalk, message sending is THE fundamental operation,
//! not method calls or function invocation.
//!
//! # Message Send Protocol (ADR 0007 Phase 4)
//!
//! Messages are dispatched through the following strategy:
//!
//! 1. **Compiler intrinsics**: Language-level constructs that the compiler must
//!    generate inline code for (binary operators, block evaluation, spawn/await,
//!    class/nil testing). These are structural requirements, not type-specific dispatch.
//!
//! 2. **Runtime dispatch**: All other messages go through the unified entry point
//!    `beamtalk_message_dispatch:send/3` (BT-430), which routes to:
//!    - **Actors** (`beamtalk_object` records): Async via `beamtalk_actor:async_send` with futures
//!    - **Class objects**: Sync via `beamtalk_object_class:class_send/3`
//!    - **Primitives** (everything else): Sync via `beamtalk_primitive:send/3`
//!
//! The primitive binding table from `lib/*.bt` (ADR 0007) drives stdlib method
//! compilation, while call-site dispatch uses runtime type checking since we
//! don't have static type information.
//!
//! # Special Cases (Compiler Intrinsics)
//!
//! - **Binary operators**: `+`, `-`, `*`, `/` → Direct Erlang arithmetic
//! - **Block evaluation**: `value`, `whileTrue:`, `repeat` → Direct function calls
//! - **ProtoObject/Object**: `class`, `isNil`, `respondsTo:` → Pattern matching
//! - **Spawn messages**: `Class spawn`, `Class spawnWith: args` → `gen_server:start_link`
//! - **Await messages**: `future await` → Blocking future resolution
//! - **Super sends**: `super methodName:` → Parent class dispatch

use super::document::Document;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{Expression, MessageSelector};
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates a comma-separated argument list for function/message calls.
    ///
    /// This is a shared helper that eliminates the repeated pattern of iterating
    /// over arguments with comma separation found throughout dispatch codegen.
    /// Captures a comma-separated argument list as a `Document` (ADR 0018 bridge).
    ///
    /// Uses `expression_doc` for each argument, joining with commas.
    fn capture_argument_list_doc(&mut self, arguments: &[Expression]) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(self.expression_doc(arg)?);
        }
        Ok(Document::Vec(parts))
    }

    /// Generates code for a message send.
    ///
    /// This is the **main entry point** for message compilation. It dispatches
    /// to specialized handlers for different message patterns, and falls back
    /// to runtime dispatch via `beamtalk_message_dispatch:send/3` (BT-430)
    /// which handles actors, class objects, and primitives uniformly.
    ///
    /// # Message Dispatch Strategy (ADR 0007 Phase 4)
    ///
    /// 1. **Super sends** → `generate_super_send`
    /// 2. **Binary operators** → `generate_binary_op` (synchronous Erlang ops)
    /// 3. **`ProtoObject` messages** → `try_generate_protoobject_message` (synchronous)
    /// 4. **Object messages** → `try_generate_object_message` → delegates to nil protocol, error signaling, object identity, object reflection
    /// 5. **Block messages** → `try_generate_block_message` (structural intrinsics)
    /// 6. **Spawn/Await** → `try_handle_spawn_await` (spawn, await intrinsics)
    /// 7. **Class references** → `try_handle_class_reference` (workspace bindings, class methods)
    /// 8. **Self-sends** → `try_handle_self_dispatch` (synchronous actor self-dispatch)
    /// 9. **Default** → Runtime dispatch (BT-223: actor vs primitive check)
    pub(super) fn generate_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Special case: super message send
        // Super calls invoke the superclass implementation
        if matches!(receiver, Expression::Super(_)) {
            return self.generate_super_send(selector, arguments);
        }

        // Compile-time type assertion: `expr asType: SomeClass` (ADR 0025 Phase 2b)
        // Erased at codegen — generates only the receiver expression (zero runtime cost)
        if let MessageSelector::Keyword(parts) = selector {
            let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            if selector_name == "asType:" {
                return self.expression_doc(receiver);
            }
        }

        // For binary operators, use Erlang's built-in operators (these are synchronous)
        if let MessageSelector::Binary(op) = selector {
            // BT-101: Method lookup via `>>` operator (e.g., Counter >> #increment)
            // BT-323: Support `>>` on any expression, not just class literals
            if op.as_str() == ">>" {
                if let Expression::ClassReference { name, .. } = receiver {
                    return self.generate_method_lookup(&name.name, arguments);
                }
                // Runtime fallback: evaluate receiver and call method/2
                return self.generate_runtime_method_lookup(receiver, arguments);
            }

            // BT-335: Association creation via `->` binary message
            if op.as_str() == "->" {
                if arguments.len() != 1 {
                    return Err(CodeGenError::Internal(
                        "-> operator must have exactly one argument".to_string(),
                    ));
                }
                let key_doc = self.expression_doc(receiver)?;
                let val_doc = self.expression_doc(&arguments[0])?;
                let doc = docvec![
                    "~{'$beamtalk_class' => 'Association', 'key' => ",
                    key_doc,
                    ", 'value' => ",
                    val_doc,
                    "}~"
                ];
                return Ok(doc);
            }
            let doc = self.generate_binary_op(op, receiver, arguments)?;
            return Ok(doc);
        }

        // Special case: ProtoObject methods - fundamental operations on all objects
        // class returns the class name for any object (primitives or actors)
        if let Some(doc) = self.try_generate_protoobject_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: Object methods - reflection and introspection
        // respondsTo:, instVarNames, instVarAt: enable runtime introspection
        if let Some(doc) = self.try_generate_object_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: Block evaluation messages (value, value:, whileTrue:, etc.)
        // These are synchronous function calls, not async actor messages
        if let Some(doc) = self.try_generate_block_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: List iteration messages (do:, collect:, select:, reject:, inject:into:)
        // These are structural intrinsics that require inline code generation for proper
        // state threading when used inside actor methods with field mutations.
        if let Some(doc) = self.try_generate_list_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: spawn, spawnWith:, await, awaitForever, await:
        if let Some(doc) = self.try_handle_spawn_await(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-374 / ADR 0010: Workspace binding dispatch + class method calls
        if let Some(doc) = self.try_handle_class_reference(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-412: Self-sends in class methods route through class_send
        if let Some(doc) = self.try_handle_class_method_self_send(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-330: Self-sends in actor methods use direct synchronous dispatch
        if let Some(doc) = self.try_handle_self_dispatch(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-430: Unified dispatch via beamtalk_message_dispatch:send/3
        self.generate_runtime_dispatch(receiver, selector, arguments)
    }

    /// Generates unified runtime dispatch via `beamtalk_message_dispatch:send/3` (BT-430).
    ///
    /// This is the fallback path for messages that don't match any compiler intrinsic.
    /// Routes through the unified entry point which handles actors (async with futures),
    /// class objects (sync), and primitives (sync).
    fn generate_runtime_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.to_erlang_atom();
        if matches!(selector, MessageSelector::Binary(_)) {
            return Err(CodeGenError::Internal(format!(
                "unexpected binary selector in generate_message_send: {selector_atom}"
            )));
        }

        let receiver_doc = self.expression_doc(receiver)?;
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            receiver_doc,
            Document::String(format!(", '{selector_atom}', [")),
            args_doc,
            "])"
        ];

        Ok(doc)
    }

    /// Handles spawn, spawnWith:, await, awaitForever, and await: intrinsics.
    ///
    /// Returns `Some(())` if the message was handled, `None` if it should
    /// fall through to the next dispatch strategy.
    fn try_handle_spawn_await(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // Unary spawn/await messages
        if let MessageSelector::Unary(name) = selector {
            // BT-246: Only match ClassReference, not Identifier.
            if name == "spawn" && arguments.is_empty() {
                if let Expression::ClassReference { name, .. } = receiver {
                    let doc = self.generate_actor_spawn(&name.name, None)?;
                    return Ok(Some(doc));
                }
            }
            if name == "await" && arguments.is_empty() {
                let doc = self.generate_await(receiver)?;
                return Ok(Some(doc));
            }
            if name == "awaitForever" && arguments.is_empty() {
                let doc = self.generate_await_forever(receiver)?;
                return Ok(Some(doc));
            }
        }

        // Keyword await:/spawnWith: messages
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "await:" && arguments.len() == 1 {
                let doc = self.generate_await_with_timeout(receiver, &arguments[0])?;
                return Ok(Some(doc));
            }
            // BT-246: Only match ClassReference, not Identifier.
            if parts.len() == 1 && parts[0].keyword == "spawnWith:" && arguments.len() == 1 {
                if let Expression::ClassReference { name, .. } = receiver {
                    let doc = self.generate_actor_spawn(&name.name, Some(&arguments[0]))?;
                    return Ok(Some(doc));
                }
            }
        }

        Ok(None)
    }

    /// Handles `ClassReference` receivers as class method calls.
    ///
    /// ADR 0019 Phase 3: In REPL context, checks REPL bindings first for
    /// convenience names (Transcript, Beamtalk, Workspace). If found in bindings,
    /// dispatches via `beamtalk_message_dispatch:send/3` (instance dispatch).
    /// Falls back to `class_send` for actual class names.
    ///
    /// In actor/value-type methods compiled in workspace mode, uses `class_send`
    /// with fallback to workspace binding for convenience names.
    ///
    /// Returns `Some(doc)` if the receiver is a `ClassReference`, `None` otherwise.
    fn try_handle_class_reference(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if let Expression::ClassReference { name, .. } = receiver {
            if self.workspace_mode && self.context == CodeGenContext::Repl {
                // REPL top-level: check session bindings first
                let doc =
                    self.generate_binding_aware_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            if self.workspace_mode {
                // Actor/ValueType methods in workspace mode: try class_send,
                // fall back to workspace binding for convenience names
                let doc = self.generate_workspace_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            let doc = self.generate_class_method_call(&name.name, selector, arguments)?;
            return Ok(Some(doc));
        }
        Ok(None)
    }

    /// Handles self-sends inside actor methods (BT-330).
    ///
    /// Returns `Some(())` if the receiver is `self` in an Actor context, `None` otherwise.
    fn try_handle_self_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if self.context == CodeGenContext::Actor {
            if let Expression::Identifier(id) = receiver {
                if id.name == "self" {
                    let doc = self.generate_self_dispatch(selector, arguments)?;
                    return Ok(Some(doc));
                }
            }
        }
        Ok(None)
    }

    /// BT-412: Handles self-sends in class method context.
    ///
    /// When a class method sends a message to `self` (the class object),
    /// we call the module function directly (not through `gen_server`) to avoid
    /// deadlock since class methods execute inside a `gen_server:call` handler.
    ///
    /// For user-defined class methods, generates `class_<selector>(ClassSelf, ClassVars, ...)`.
    /// For built-in exports (spawn, new, etc.), generates `module:selector(...)`.
    fn try_handle_class_method_self_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        if !self.in_class_method {
            return Ok(None);
        }
        if let Expression::Identifier(id) = receiver {
            if id.name == "self" {
                let selector_atom = selector.to_erlang_atom();

                if self.class_method_selectors.contains(&selector_atom) {
                    // Route to class_<selector>(ClassSelf, ClassVars, ...)
                    let call_result = self.fresh_temp_var("CMR");
                    let cv = self.current_class_var();
                    let module = self.module_name.clone();
                    let args_doc = self.capture_argument_list_doc(arguments)?;
                    let comma = if arguments.is_empty() { "" } else { ", " };

                    let new_cv = self.next_class_var();
                    let inner_cv = self.fresh_temp_var("CV");
                    let inner_res = self.fresh_temp_var("MR");
                    let plain_cv = self.fresh_temp_var("PCV");
                    let result = self.fresh_temp_var("Unwrapped");
                    let wrapped_res = self.fresh_temp_var("WR");
                    let plain_res = self.fresh_temp_var("PR");

                    let doc = docvec![
                        Document::String(format!(
                            "let {call_result} = call '{module}':'class_{selector_atom}'(ClassSelf, {cv}"
                        )),
                        comma,
                        args_doc,
                        Document::String(format!(
                            ") in \
                             let {new_cv} = case {call_result} of \
                             <{{'class_var_result', {inner_res}, {inner_cv}}}> when 'true' -> {inner_cv} \
                             <{plain_cv}> when 'true' -> {cv} \
                             end in \
                             let {result} = case {call_result} of \
                             <{{'class_var_result', {wrapped_res}, _}}> when 'true' -> {wrapped_res} \
                             <{plain_res}> when 'true' -> {plain_res} \
                             end in "
                        ))
                    ];
                    // NOTE: scope is OPEN — caller provides continuation
                    self.last_open_scope_result = Some(result);
                    return Ok(Some(doc));
                }
                // Built-in export (spawn, new, superclass, etc.)
                let fun_name = match selector_atom.as_str() {
                    "spawnWith:" => "spawn".to_string(),
                    _ => selector_atom.replace(':', ""),
                };
                let module = self.module_name.clone();
                let args_doc = self.capture_argument_list_doc(arguments)?;

                let doc = docvec![
                    Document::String(format!("call '{module}':'{fun_name}'(")),
                    args_doc,
                    ")"
                ];
                return Ok(Some(doc));
            }
        }
        Ok(None)
    }

    /// Generates synchronous self-dispatch for actor self-sends (BT-330).
    ///
    /// When an actor method sends a message to `self`, we bypass the async
    /// `gen_server:cast` path and call `safe_dispatch/3` directly. This ensures
    /// the result is a value (not a Future), enabling recursive algorithms like
    /// factorial and fibonacci to work correctly.
    ///
    /// # Sealed Class Optimization (BT-403)
    ///
    /// For sealed classes, we skip the `safe_dispatch/3` try/catch overhead and
    /// call `dispatch/4` directly. Since sealed classes have all methods known at
    /// compile time, the error isolation overhead is unnecessary.
    ///
    /// # Generated Code (normal)
    ///
    /// ```erlang
    /// case call 'module':'safe_dispatch'('selector', [Args], State) of
    ///   <{'reply', Result, _NewState}> when 'true' -> Result
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end
    /// ```
    ///
    /// # Generated Code (sealed class)
    ///
    /// ```erlang
    /// let Self = call 'beamtalk_actor':'make_self'(State) in
    /// case call 'module':'dispatch'('selector', [Args], Self, State) of
    ///   <{'reply', Result, _NewState}> when 'true' -> Result
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end
    /// ```
    fn generate_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // BT-403: Sealed class optimization — skip safe_dispatch try/catch
        if self.is_class_sealed() {
            return self.generate_sealed_self_dispatch(selector, arguments);
        }

        let selector_atom = selector.to_erlang_atom();
        let result_var = self.fresh_var("SelfResult");
        let state_var = self.fresh_var("SelfState");
        let error_var = self.fresh_var("SelfError");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            Document::String(format!(
                "case call '{module}':'safe_dispatch'('{selector_atom}', ["
            )),
            args_doc,
            Document::String(format!("], {current_state}) of ")),
            Document::String(format!(
                "<{{'reply', {result_var}, {state_var}}}> when 'true' -> {result_var} "
            )),
            Document::String(format!(
                "<{{'error', {error_var}, _}}> when 'true' -> call 'beamtalk_error':'raise'({error_var}) "
            )),
            "end"
        ];

        Ok(doc)
    }

    /// BT-245: Generates self-dispatch with state threading (open binding pattern).
    ///
    /// Like `generate_self_dispatch`, but captures the new state from the dispatch
    /// result and advances the state version. The let binding is left open so
    /// subsequent expressions see the updated state.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _SD0 = case call 'module':'safe_dispatch'('sel', [Args], State) of
    ///   <{'reply', R, S}> when 'true' -> {R, S}
    ///   <{'error', E, _}> when 'true' -> call 'beamtalk_error':'raise'(E)
    /// end in let State1 = call 'erlang':'element'(2, _SD0) in
    /// ```
    ///
    /// The expression value `call 'erlang':'element'(1, _SD0)` is NOT emitted —
    /// it's discarded since this is used for non-last expressions in block bodies.
    ///
    /// Uses Document/docvec! (ADR 0018) for composable rendering.
    pub(super) fn generate_self_dispatch_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            let selector_atom = selector.to_erlang_atom();
            let dispatch_var = self.fresh_temp_var("SD");
            let result_var = self.fresh_var("SDResult");
            let state_var = self.fresh_var("SDState");
            let error_var = self.fresh_var("SDError");
            let current_state = self.current_state_var();

            // Capture arguments via bridge (ADR 0018 Phase 0)
            let args_doc = self.capture_argument_list_doc(arguments)?;

            // Build the dispatch call (varies by sealed optimization level)
            let call_doc = if self.is_class_sealed() {
                let selector_name = selector.name().to_string();
                if self.sealed_method_selectors.contains(&selector_name) {
                    // Level 1: Direct __sealed_ call
                    let self_var = self.fresh_temp_var("SealedSelf");
                    let module = self.module_name.clone();
                    let comma = if arguments.is_empty() { "" } else { ", " };
                    docvec![
                        Document::String(format!(
                            "let {self_var} = call 'beamtalk_actor':'make_self'({current_state}) in "
                        )),
                        Document::String(format!(
                            "let {dispatch_var} = case call '{module}':'__sealed_{selector_name}'("
                        )),
                        args_doc,
                        Document::String(format!("{comma}{self_var}, {current_state}) of "))
                    ]
                } else {
                    // Level 2: Direct dispatch/4 call
                    let self_var = self.fresh_temp_var("SealedSelf");
                    let module = self.module_name.clone();
                    docvec![
                        Document::String(format!(
                            "let {self_var} = call 'beamtalk_actor':'make_self'({current_state}) in "
                        )),
                        Document::String(format!(
                            "let {dispatch_var} = case call '{module}':'dispatch'('{selector_atom}', ["
                        )),
                        args_doc,
                        Document::String(format!("], {self_var}, {current_state}) of "))
                    ]
                }
            } else {
                // Normal: safe_dispatch/3
                let module = self.module_name.clone();
                docvec![
                    Document::String(format!(
                        "let {dispatch_var} = case call '{module}':'safe_dispatch'('{selector_atom}', ["
                    )),
                    args_doc,
                    Document::String(format!("], {current_state}) of "))
                ]
            };

            // Result/error clauses + state extraction
            let new_state = self.next_state_var();
            let doc = docvec![
                call_doc,
                Document::String(format!(
                    "<{{'reply', {result_var}, {state_var}}}> when 'true' -> {{{result_var}, {state_var}}} "
                )),
                Document::String(format!(
                    "<{{'error', {error_var}, _}}> when 'true' -> call 'beamtalk_error':'raise'({error_var}) "
                )),
                "end in ",
                Document::String(format!(
                    "let {new_state} = call 'erlang':'element'(2, {dispatch_var}) in "
                ))
            ];

            self.last_dispatch_var = Some(dispatch_var);
            return Ok(doc);
        }
        // Fallback
        self.generate_expression(expr)
    }
    ///
    /// Two levels of optimization:
    /// 1. **Known sealed method**: Direct function call to `__sealed_{selector}`,
    ///    bypassing both `safe_dispatch/3` and `dispatch/4` case matching.
    /// 2. **Unknown method** (inherited): Direct `dispatch/4` call, skipping
    ///    only the `safe_dispatch/3` try/catch overhead.
    fn generate_sealed_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_name = selector.name().to_string();

        // Level 1: Direct call to standalone sealed method function
        if self.sealed_method_selectors.contains(&selector_name) {
            return self.generate_direct_sealed_call(&selector_name, arguments);
        }

        // Level 2: Direct dispatch/4 call (skip safe_dispatch try/catch)
        let selector_atom = selector.to_erlang_atom();
        let result_var = self.fresh_var("SealedResult");
        let error_var = self.fresh_var("SealedError");
        let self_var = self.fresh_temp_var("SealedSelf");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            Document::String(format!(
                "let {self_var} = call 'beamtalk_actor':'make_self'({current_state}) in "
            )),
            Document::String(format!(
                "case call '{module}':'dispatch'('{selector_atom}', ["
            )),
            args_doc,
            Document::String(format!("], {self_var}, {current_state}) of ")),
            Document::String(format!(
                "<{{'reply', {result_var}, _}}> when 'true' -> {result_var} "
            )),
            Document::String(format!(
                "<{{'error', {error_var}, _}}> when 'true' -> call 'beamtalk_error':'raise'({error_var}) "
            )),
            "end"
        ];

        Ok(doc)
    }

    /// Generates a direct call to a sealed method's standalone function (BT-403).
    ///
    /// This is the most optimized self-dispatch path: calls `__sealed_{selector}`
    /// directly, bypassing `safe_dispatch`, dispatch, and case selector matching.
    fn generate_direct_sealed_call(
        &mut self,
        selector_name: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let result_var = self.fresh_var("SealedResult");
        let error_var = self.fresh_var("SealedError");
        let self_var = self.fresh_temp_var("SealedSelf");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let comma = if arguments.is_empty() { "" } else { ", " };

        let doc = docvec![
            Document::String(format!(
                "let {self_var} = call 'beamtalk_actor':'make_self'({current_state}) in "
            )),
            Document::String(format!("case call '{module}':'__sealed_{selector_name}'(")),
            args_doc,
            Document::String(format!("{comma}{self_var}, {current_state}) of ")),
            Document::String(format!(
                "<{{'reply', {result_var}, _}}> when 'true' -> {result_var} "
            )),
            Document::String(format!(
                "<{{'error', {error_var}, _}}> when 'true' -> call 'beamtalk_error':'raise'({error_var}) "
            )),
            "end"
        ];

        Ok(doc)
    }
    ///
    /// This is used to detect state mutations that require threading through
    /// control flow constructs.
    pub(super) fn is_field_assignment(expr: &Expression) -> bool {
        if let Expression::Assignment { target, .. } = expr {
            if let Expression::FieldAccess { receiver, .. } = target.as_ref() {
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    return recv_id.name == "self";
                }
            }
        }
        false
    }

    /// Checks if an expression is a class variable assignment (`self.classVar := value`).
    pub(super) fn is_class_var_assignment(&self, expr: &Expression) -> bool {
        if !self.in_class_method {
            return false;
        }
        if let Expression::Assignment { target, .. } = expr {
            if let Expression::FieldAccess {
                receiver, field, ..
            } = target.as_ref()
            {
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    return recv_id.name == "self"
                        && self.class_var_names.contains(field.name.as_str());
                }
            }
        }
        false
    }

    /// Checks if an expression is a self-send to a class method (BT-412).
    /// These need special scoping in class method bodies because they may
    /// update `ClassVars` via `let ClassVarsN = ... in` which must not be wrapped.
    pub(super) fn is_class_method_self_send(&self, expr: &Expression) -> bool {
        if !self.in_class_method || self.class_method_selectors.is_empty() {
            return false;
        }
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            if let Expression::Identifier(id) = receiver.as_ref() {
                if id.name == "self" {
                    let sel_atom = selector.to_erlang_atom();
                    return self.class_method_selectors.contains(&sel_atom);
                }
            }
        }
        false
    }

    /// Checks if an expression is a local variable assignment (`identifier := value`).
    pub(super) fn is_local_var_assignment(expr: &Expression) -> bool {
        if let Expression::Assignment { target, .. } = expr {
            matches!(target.as_ref(), Expression::Identifier(_))
        } else {
            false
        }
    }

    /// Checks if an expression is a super message send (`super methodName:`).
    pub(super) fn is_super_message_send(expr: &Expression) -> bool {
        if let Expression::MessageSend { receiver, .. } = expr {
            matches!(receiver.as_ref(), Expression::Super(_))
        } else {
            false
        }
    }

    /// BT-245: Checks if an expression is a self-send in actor context.
    /// These may mutate actor state and need state threading in loop bodies.
    pub(super) fn is_actor_self_send(&self, expr: &Expression) -> bool {
        if self.context != super::CodeGenContext::Actor {
            return false;
        }
        if let Expression::MessageSend { receiver, .. } = expr {
            if let Expression::Identifier(id) = receiver.as_ref() {
                return id.name == "self";
            }
        }
        false
    }

    /// Checks if an expression is an `error:` message send.
    ///
    /// Since `erlang:error/1` never returns (always throws an exception),
    /// expressions ending with `error:` should not be wrapped in reply tuples.
    pub(super) fn is_error_message_send(expr: &Expression) -> bool {
        matches!(
            expr,
            Expression::MessageSend {
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } if parts.len() == 1 && parts[0].keyword == "error:" && arguments.len() == 1
        )
    }

    /// Checks if an expression is a control flow construct that *potentially*
    /// threads state through a block (e.g. `do:`, `whileTrue:`, etc.).
    ///
    /// **IMPORTANT:** This helper is **selector-based only** - it does not analyze
    /// whether the literal block argument actually mutates any captured state, nor which
    /// variant of the underlying control-flow primitive (pure vs. with mutations) will
    /// be used. As a result, it may return `true` for expressions whose final result
    /// is a non-state value (such as `ok` or `nil`).
    ///
    /// **Callers that use this to decide whether to rebind a threaded `StateN` variable
    /// MUST also perform mutation analysis** on the block argument (or use equivalent
    /// information such as `control_flow_has_mutations()`) rather than assuming that
    /// the return value is always the updated state.
    ///
    /// Note: `inject:into:` is included because it may appear inside actor methods
    /// with field mutations, requiring inline compilation for proper state threading.
    pub(super) fn is_state_threading_control_flow(expr: &Expression) -> bool {
        match expr {
            Expression::MessageSend {
                selector: MessageSelector::Keyword(parts),
                ..
            } => {
                // Check for control flow selectors that thread state
                let selector: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                matches!(
                    selector.as_str(),
                    "whileTrue:"
                        | "whileFalse:"
                        | "timesRepeat:"
                        | "to:do:"
                        | "to:by:do:"
                        | "do:"
                        | "collect:"
                        | "select:"
                        | "reject:"
                        | "inject:into:"
                        | "on:do:"
                        | "ensure:"
                )
            }
            Expression::MessageSend {
                selector: MessageSelector::Unary(name),
                ..
            } => {
                // Check for unary control flow
                matches!(name.as_str(), "whileTrue" | "whileFalse" | "timesRepeat")
            }
            _ => false,
        }
    }

    /// Generates the opening part of a field assignment with state threading.
    ///
    /// For `self.field := value`, generates:
    /// ```erlang
    /// let _Val = <value> in
    /// let StateN = call 'maps':'put'('field', _Val, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression (generating the body
    /// that uses the new state).
    pub(super) fn generate_field_assignment_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");
                let current_state = self.current_state_var();
                let val_doc = self.expression_doc(value)?;

                let new_state = self.next_state_var();

                let doc = docvec![
                    Document::String(format!("let {val_var} = ")),
                    val_doc,
                    Document::String(format!(
                        " in let {new_state} = call 'maps':'put'('{}', {val_var}, {current_state}) in ",
                        field.name
                    ))
                ];

                return Ok(doc);
            }
        }
        // Fallback: should not reach here if is_field_assignment check was correct
        self.generate_expression(expr)
    }

    /// Generates code for a super message send.
    ///
    /// Super calls use `beamtalk_dispatch:super/5` to invoke the superclass
    /// implementation via hierarchy walking (ADR 0006).
    ///
    /// # Example
    ///
    /// ```beamtalk
    /// super increment
    /// super getValue
    /// super at: 1 put: value
    /// ```
    ///
    /// Generates:
    ///
    /// ```erlang
    /// call 'beamtalk_dispatch':'super'('increment', [], Self, State, 'Counter')
    /// call 'beamtalk_dispatch':'super'('getValue', [], Self, State, 'Counter')
    /// call 'beamtalk_dispatch':'super'('at:put:', [1, Value], Self, State, 'Counter')
    /// ```
    pub(super) fn generate_super_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.to_erlang_atom();
        let class_name = self.class_name();
        let current_state = self.current_state_var();
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            Document::String(format!(
                "call 'beamtalk_dispatch':'super'('{selector_atom}', ["
            )),
            args_doc,
            Document::String(format!("], Self, {current_state}, '{class_name}')"))
        ];
        Ok(doc)
    }

    /// Generates code for actor spawn with conditional REPL registry integration.
    ///
    /// When spawning an actor in the REPL, check for `__repl_actor_registry__` in
    /// bindings and register the spawned actor. In all cases, the module's own
    /// `spawn/0` or `spawn/1` is called (which handles initialize protocol).
    /// In non-REPL contexts (regular code, tests), fall back to normal `module:spawn`.
    ///
    /// # Arguments
    ///
    /// * `class_name` - The Beamtalk class name (e.g., "Counter")
    /// * `init_args` - Optional initialization arguments for spawnWith:
    ///
    /// # Generated Code (REPL context)
    ///
    /// ```erlang
    /// case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of
    ///   <'undefined'> when 'true' ->
    ///     call 'bt@counter':'spawn'()
    ///   <RegistryPid> when 'true' ->
    ///     let SpawnResult = call 'bt@counter':'spawn'() in
    ///     let {'beamtalk_object', _, _, SpawnPid} = SpawnResult in
    ///     let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, 'Counter', 'bt@counter') in
    ///     SpawnResult
    /// end
    /// ```
    ///
    /// # Generated Code (non-REPL context)
    ///
    /// ```erlang
    /// call 'bt@counter':'spawn'()
    /// ```
    pub(super) fn generate_actor_spawn(
        &mut self,
        class_name: &str,
        init_args: Option<&Expression>,
    ) -> Result<Document<'static>> {
        let module_name = Self::compiled_module_name(class_name);
        let in_repl_context = self.lookup_var("__bindings__").is_some();

        let args_doc = match init_args {
            Some(args) => self.expression_doc(args)?,
            None => Document::Nil,
        };

        if in_repl_context {
            let doc = docvec![
                "case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of ",
                Document::String(format!(
                    "<'undefined'> when 'true' -> call '{module_name}':'spawn'("
                )),
                args_doc.clone(),
                Document::String(format!(
                    ") <RegistryPid> when 'true' -> let SpawnResult = call '{module_name}':'spawn'("
                )),
                args_doc,
                ") in ",
                "let SpawnPid = call 'erlang':'element'(4, SpawnResult) in ",
                Document::String(format!(
                    "let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, '{class_name}', '{module_name}') in "
                )),
                "SpawnResult ",
                "end"
            ];
            Ok(doc)
        } else {
            let doc = docvec![
                Document::String(format!("call '{module_name}':'spawn'(")),
                args_doc,
                ")"
            ];
            Ok(doc)
        }
    }

    /// Generates a method lookup via `>>` operator (BT-101).
    ///
    /// `Counter >> #increment` compiles to:
    /// ```erlang
    /// call 'beamtalk_method_resolver':'resolve'('Counter', 'increment')
    /// ```
    ///
    /// Returns a `CompiledMethod` map with selector, source, and arity metadata.
    fn generate_method_lookup(
        &mut self,
        class_name: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        let arg_doc = self.expression_doc(&arguments[0])?;
        let doc = docvec![
            Document::String(format!(
                "call 'beamtalk_method_resolver':'resolve'('{class_name}', "
            )),
            arg_doc,
            ")"
        ];
        Ok(doc)
    }

    /// Generates a runtime method resolution via `>>` for non-class-literal receivers (BT-323).
    ///
    /// `cls >> #increment` (where cls holds a class object) compiles to:
    /// ```erlang
    /// call 'beamtalk_method_resolver':'resolve'(cls, 'increment')
    /// ```
    ///
    /// The `MethodResolver` domain service accepts pids, atoms, and class object tuples.
    fn generate_runtime_method_lookup(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        let receiver_doc = self.expression_doc(receiver)?;
        let arg_doc = self.expression_doc(&arguments[0])?;
        let doc = docvec![
            "call 'beamtalk_method_resolver':'resolve'(",
            receiver_doc,
            ", ",
            arg_doc,
            ")"
        ];
        Ok(doc)
    }

    /// Generates a binding-aware class method call (ADR 0019 Phase 3).
    ///
    /// In workspace mode, checks REPL bindings first for convenience names.
    /// If the name is found in bindings, it's an instance (e.g., Transcript is a
    /// `TranscriptStream` actor), so dispatch via `beamtalk_message_dispatch:send/3`.
    /// If not found, fall back to `class_send` for actual class names.
    ///
    /// ```erlang
    /// let ClassPid = call 'beamtalk_class_registry':'whereis_class'('Name') in
    /// case call 'maps':'find'('Name', State) of
    ///   <{'ok', BindingVal}> -> call 'beamtalk_message_dispatch':'send'(BindingVal, Sel, Args)
    ///   <'error'> -> call 'beamtalk_object_class':'class_send'(ClassPid, Sel, Args)
    /// end
    /// ```
    fn generate_binding_aware_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.to_erlang_atom();
        let class_pid_var = self.fresh_var("ClassPid");
        let binding_val_var = self.fresh_var("BindingVal");
        let state_var = self.current_state_var();
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            Document::String(format!(
                "case call 'maps':'find'('{class_name}', {state_var}) of "
            )),
            Document::String(format!("<{{'ok', {binding_val_var}}}> when 'true' -> ")),
            Document::String(format!(
                "call 'beamtalk_message_dispatch':'send'({binding_val_var}, '{selector_atom}', ["
            )),
            args_doc.clone(),
            "]) ",
            "<'error'> when 'true' -> ",
            Document::String(format!(
                "let {class_pid_var} = call 'beamtalk_class_registry':'whereis_class'('{class_name}') in "
            )),
            Document::String(format!(
                "call 'beamtalk_object_class':'class_send'({class_pid_var}, '{selector_atom}', ["
            )),
            args_doc,
            "]) end"
        ];

        Ok(doc)
    }

    /// Generates workspace-mode class send for actor/value-type methods.
    ///
    /// Tries `class_send` first (for real class names like `Counter`),
    /// returns nil for unresolved names. ADR 0019 Phase 4: No `persistent_term`
    /// fallback — convenience names resolve via session bindings in REPL context.
    fn generate_workspace_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.to_erlang_atom();
        let class_pid_var = self.fresh_var("ClassPid");
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            Document::String(format!(
                "case call 'beamtalk_class_registry':'whereis_class'('{class_name}') of "
            )),
            "<'undefined'> when 'true' -> 'nil' ",
            Document::String(format!("<{class_pid_var}> when 'true' -> ")),
            Document::String(format!(
                "call 'beamtalk_object_class':'class_send'({class_pid_var}, '{selector_atom}', ["
            )),
            args_doc,
            "]) end"
        ];

        Ok(doc)
    }

    /// Generates a class-level method call (BT-215).
    ///
    /// Class methods are just module functions, so we generate a direct call:
    /// ```erlang
    /// call 'ModuleName':'methodName'(Args)
    /// ```
    ///
    /// Examples:
    /// - `Beamtalk allClasses` → `call 'Beamtalk':'allClasses'()`
    /// - `Point new` → `call 'Point':'new'()`
    /// - `Counter spawn` → Already handled by `generate_actor_spawn`
    /// - `File exists: 'test.txt'` → `call 'beamtalk_file':'exists:'(...)` (BT-336)
    fn generate_class_method_call(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.to_erlang_atom();
        let class_pid_var = self.fresh_var("ClassPid");
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            Document::String(format!(
                "let {class_pid_var} = call 'beamtalk_class_registry':'whereis_class'('{class_name}') in "
            )),
            Document::String(format!(
                "call 'beamtalk_object_class':'class_send'({class_pid_var}, '{selector_atom}', ["
            )),
            args_doc,
            "])"
        ];

        Ok(doc)
    }
}

// NOTE: class_method_module_name and related helpers (is_primitive_stdlib_class,
// is_bt_stdlib_class, is_erlang_stdlib_module) were removed in BT-411.
// Class dispatch now goes through runtime class_send/3 instead of
// compile-time module name resolution.
