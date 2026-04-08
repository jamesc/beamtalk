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
//!    - **Actors** (`beamtalk_object` records): Sync via `beamtalk_actor:sync_send/3` (BT-918 / ADR 0043)
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
    /// BT-1935: Uses `expression_doc_with_open_scope` to detect and close any
    /// open let-chains produced by class method self-sends used as arguments.
    /// Without this, an argument like `(self classMethod: x)` embeds an open
    /// `let ... in ` chain inside the argument list, producing invalid Core Erlang.
    fn capture_argument_list_doc(&mut self, arguments: &[Expression]) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let saved_cv = self.class_var_version();
            let (doc, open_scope) = self.expression_doc_with_open_scope(arg)?;
            if let Some(result_var) = open_scope {
                // Close the open scope inline: the let-chain + result_var forms
                // a valid closed expression (e.g., `let X = ... in X`).
                // Roll back class var version since the ClassVarsN binding is
                // scoped inside the closed expression and not visible to
                // subsequent code.
                self.set_class_var_version(saved_cv);
                parts.push(docvec![doc, Document::String(result_var)]);
            } else {
                parts.push(doc);
            }
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
    /// 7. **Erlang interop** → `try_handle_erlang_interop` (ADR 0028 direct call / proxy)
    /// 8. **Class references** → `try_handle_class_reference` (workspace bindings, class methods)
    /// 9. **Self-sends** → `try_handle_self_dispatch` (synchronous actor self-dispatch)
    /// 10. **Default** → Runtime dispatch (BT-223: actor vs primitive check)
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
            if parts.len() == 1 && parts[0].keyword == "asType:" && arguments.len() == 1 {
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

            let doc = self.generate_binary_op(op, receiver, arguments)?;
            return Ok(doc);
        }

        // Special case: ProtoObject methods - fundamental operations on all objects
        // class returns the class name for any object (primitives or actors)
        if let Some(doc) = self.try_generate_protoobject_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: Object methods - reflection and introspection
        // respondsTo:, fieldNames, fieldAt: enable runtime introspection
        if let Some(doc) = self.try_generate_object_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: Block evaluation messages (value, value:, whileTrue:, etc.)
        // These are synchronous function calls, not async actor messages
        if let Some(doc) = self.try_generate_block_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: Dictionary iteration messages (do:, doWithKey:, keysAndValuesDo:)
        // Must come before list messages so dictionary-specific selectors are handled correctly.
        if let Some(doc) = self.try_generate_dict_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: List iteration messages (do:, collect:, select:, reject:, inject:into:)
        // These are structural intrinsics that require inline code generation for proper
        // state threading when used inside actor methods with field mutations.
        if let Some(doc) = self.try_generate_list_message(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-915: Boolean conditionals (ifTrue:, ifFalse:, ifTrue:ifFalse:) in actor context
        // with field mutations. Generates inline case expressions that thread state correctly
        // through both branches.
        if let Some(doc) = self.try_generate_boolean_protocol(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // Special case: spawn, spawnWith:, await, awaitForever, await:
        if let Some(doc) = self.try_handle_spawn_await(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-677 / BT-682 / ADR 0028: Erlang interop — direct calls and proxy construction
        if let Some(doc) = self.try_handle_erlang_interop(receiver, selector, arguments)? {
            return Ok(doc);
        }

        // BT-1435: Logger intrinsics — inline logger:log/3 with domain metadata
        if let Some(doc) = self.try_generate_logger_intrinsic(receiver, selector, arguments)? {
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

    /// Generates a cast (fire-and-forget) message send (BT-920).
    ///
    /// Called when the AST `MessageSend` node has `is_cast: true` (the `!` suffix).
    ///
    /// # Dispatch Strategy
    ///
    /// - **Self-sends in actor context** (`self someMethod!`): Calls `safe_dispatch` directly
    ///   but discards both the result and any state update — fire-and-forget semantics
    ///   within the same process.
    /// - **All other sends**: Routes through `beamtalk_message_dispatch:cast/3`, which
    ///   extracts the actor PID and calls `beamtalk_actor:cast_send/3`. Non-actor
    ///   receivers are silently ignored.
    ///
    /// Cast sends always evaluate to `'ok'`.
    pub(super) fn generate_cast_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // Self-sends with ! in actor context: direct dispatch, discard result.
        // BT-1475: Only use direct safe_dispatch when NOT inside a block (block_depth == 0).
        // Blocks may execute in a different process (Timer callbacks, cross-actor callbacks),
        // so self-cast sends inside blocks must route through the actor mailbox via
        // beamtalk_message_dispatch:cast/3 to reach the actor's gen_server process.
        if self.context == CodeGenContext::Actor && self.block_depth == 0 {
            if let Expression::Identifier(id) = receiver {
                if id.name == "self" {
                    return self.generate_self_cast_send(selector, arguments);
                }
            }
        }

        // Non-self cast sends (and self-casts inside blocks): route through
        // beamtalk_message_dispatch:cast/3
        self.generate_runtime_cast(receiver, selector, arguments)
    }

    /// Generates a self-cast send in actor context (BT-920).
    ///
    /// Calls `safe_dispatch` synchronously but discards the result (and any state
    /// mutation from the callee). Returns `'ok'` as the expression value.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let _Cast0 = call 'module':'safe_dispatch'('selector', [Args], State) in 'ok'
    /// ```
    fn generate_self_cast_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.to_erlang_atom();
        let discard_var = self.fresh_temp_var("Cast");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            "let ",
            Document::String(discard_var),
            " = call '",
            Document::Eco(module),
            "':'safe_dispatch'('",
            Document::String(selector_atom),
            "', [",
            args_doc,
            "], ",
            Document::String(current_state),
            ") in 'ok'",
        ];

        Ok(doc)
    }

    /// Generates unified runtime cast via `beamtalk_message_dispatch:cast/3` (BT-920).
    ///
    /// Fire-and-forget path: routes to the actor's message queue via
    /// `beamtalk_actor:cast_send/3`. Non-actor receivers are silently ignored.
    /// Always returns `'ok'`.
    fn generate_runtime_cast(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.to_erlang_atom();
        // BT-1935: Close open-scope let-chains from class method self-sends.
        let saved_cv = self.class_var_version();
        let (receiver_doc, receiver_open_scope) = self.expression_doc_with_open_scope(receiver)?;
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let actual_receiver = if let Some(result_var) = receiver_open_scope {
            self.set_class_var_version(saved_cv);
            docvec![receiver_doc, Document::String(result_var)]
        } else {
            receiver_doc
        };

        let doc = docvec![
            "call 'beamtalk_message_dispatch':'cast'(",
            actual_receiver,
            ", '",
            Document::String(selector_atom),
            "', [",
            args_doc,
            "])",
        ];

        Ok(doc)
    }

    /// Generates unified runtime dispatch via `beamtalk_message_dispatch:send/3` (BT-430).
    ///
    /// This is the fallback path for messages that don't match any compiler intrinsic.
    /// Routes through the unified entry point which handles actors (sync via `gen_server:call`),
    /// class objects (sync), and primitives (sync). Returns a value directly — no Future
    /// wrapping (BT-918 / ADR 0043).
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

        // BT-1343: Emit dynamic dispatch fallback diagnostic.
        if self.codegen_diagnostics_enabled {
            let span = receiver.span();
            let line_info = self
                .span_to_line(span)
                .map_or(String::new(), |l| format!(" at line {l}"));
            self.emit_codegen_diagnostic(
                format!(
                    "Send '{selector_atom}'{line_info}: dynamic dispatch (receiver type unknown)"
                ),
                span,
            );
        }

        // BT-1935: Use expression_doc_with_open_scope to detect open let-chains
        // from class method self-sends used as the receiver expression.
        let saved_cv = self.class_var_version();
        let (receiver_doc, receiver_open_scope) = self.expression_doc_with_open_scope(receiver)?;
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let actual_receiver = if let Some(result_var) = receiver_open_scope {
            // Close the open scope: append the result variable to form a valid
            // closed expression in receiver position. Roll back class var version.
            self.set_class_var_version(saved_cv);
            docvec![receiver_doc, Document::String(result_var)]
        } else {
            receiver_doc
        };

        let doc = docvec![
            "call 'beamtalk_message_dispatch':'send'(",
            actual_receiver,
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
                if let Expression::ClassReference { name, package, .. } = receiver {
                    let pkg = package.as_ref().map(|p| p.name.as_str());
                    let doc = self.generate_actor_spawn_qualified(&name.name, pkg, None)?;
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
                if let Expression::ClassReference { name, package, .. } = receiver {
                    let pkg = package.as_ref().map(|p| p.name.as_str());
                    let doc =
                        self.generate_actor_spawn_qualified(&name.name, pkg, Some(&arguments[0]))?;
                    return Ok(Some(doc));
                }
            }
        }

        Ok(None)
    }

    /// BT-677 / ADR 0028: Handles `Erlang` class reference for BEAM interop.
    ///
    /// Two cases are handled:
    ///
    /// 1. **Direct call optimization (BT-682, ADR 0028 Phase 4):** When the
    ///    receiver is `MessageSend(ClassReference("Erlang"), Unary(module))` and
    ///    the outer selector is a function call, emits a direct BEAM call:
    ///    ```erlang
    ///    call 'lists':'reverse'(Xs)
    ///    ```
    ///    This eliminates proxy map allocation entirely.
    ///
    /// 2. **Proxy construction (BT-677):** When the receiver is
    ///    `ClassReference("Erlang")` and the message is a unary module name,
    ///    generates an inline `ErlangModule` proxy map:
    ///    ```erlang
    ///    ~{'$beamtalk_class' => 'ErlangModule', 'module' => 'lists'}~
    ///    ```
    ///    This fallback handles `proxy := Erlang lists` (standalone proxy).
    ///
    /// Standard class-protocol selectors (e.g. `class`, `new`, `superclass`)
    /// fall through to normal class dispatch so that `Erlang class` returns the
    /// metaclass rather than a proxy for module `'class'`.
    fn try_handle_erlang_interop(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        /// Class-protocol selectors that must NOT be intercepted as module
        /// lookups. These are handled by `beamtalk_object_class:class_send/3`.
        const CLASS_PROTOCOL_SELECTORS: &[&str] = &[
            "new",
            "spawn",
            "class",
            "methods",
            "superclass",
            "subclasses",
            "allSubclasses",
            "class_name",
            "module_name",
            "printString",
        ];

        // BT-682: Direct call optimization — `Erlang lists reverse: xs` →
        // `call 'lists':'reverse'(Xs)` with no proxy map allocation.
        // Only when the module name is a compile-time literal (ClassReference path).
        if let Expression::MessageSend {
            receiver: inner_receiver,
            selector: MessageSelector::Unary(module_name),
            ..
        } = receiver
        {
            if let Expression::ClassReference { name, .. } = inner_receiver.as_ref() {
                if name.name == "Erlang"
                    && !CLASS_PROTOCOL_SELECTORS.contains(&module_name.as_str())
                {
                    return self.generate_direct_erlang_call(module_name, selector, arguments);
                }
            }
        }

        // BT-677: Proxy construction — `Erlang lists` → inline proxy map
        if let Expression::ClassReference { name, .. } = receiver {
            if name.name != "Erlang" {
                return Ok(None);
            }
            match selector {
                MessageSelector::Unary(module_name)
                    if !CLASS_PROTOCOL_SELECTORS.contains(&module_name.as_str()) =>
                {
                    let doc = docvec![Document::String(format!(
                        "~{{'$beamtalk_class' => 'ErlangModule', 'module' => '{module_name}'}}~"
                    ))];
                    Ok(Some(doc))
                }
                _ => {
                    // Keyword/binary on Erlang class itself, or a class-protocol
                    // selector, falls through to normal class dispatch.
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }

    /// BT-682: Generates a proxy-routed call for Erlang interop (BT-1127).
    ///
    /// Converts Beamtalk selectors to Erlang function names and routes through
    /// `beamtalk_erlang_proxy:direct_call/3` for automatic binary→charlist coercion:
    /// - Unary: `node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [])` (zero-arg)
    /// - Keyword single: `reverse:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'reverse', [Xs])`
    /// - Keyword multi: `seq:with:` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'seq', [1, 10])`
    ///
    /// Returns `None` for selectors that are Object/ProtoObject protocol methods
    /// (e.g. `printString`, `asString`) — these must go through runtime dispatch
    /// so the proxy's inherited protocol methods are called, not a non-existent
    /// Erlang function.
    ///
    /// BT-855: Block arguments are automatically wrapped via
    /// [`generate_erlang_interop_wrapper`] to strip the Tier 2 `StateAcc` protocol.
    /// A diagnostic warning is emitted when a stateful block (one with captured
    /// mutations) crosses the Erlang boundary, since mutations will be dropped.
    fn generate_direct_erlang_call(
        &mut self,
        module_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        /// Object protocol selectors that must NOT be optimized as direct Erlang
        /// calls. These are inherited from ProtoObject/Object and handled by
        /// runtime dispatch. Selectors already handled as compiler intrinsics
        /// (class, isNil, notNil, hash, yourself, respondsTo:, error:) never
        /// reach here — they're intercepted earlier in the dispatch chain.
        const OBJECT_PROTOCOL_SELECTORS: &[&str] = &["printString", "asString", "inspect"];

        match selector {
            MessageSelector::Unary(function_name) => {
                if OBJECT_PROTOCOL_SELECTORS.contains(&function_name.as_str()) {
                    return Ok(None);
                }
                // BT-1127: Route zero-arg calls through proxy (consistent with keyword sends).
                // `Erlang erlang node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [])`
                let doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'('",
                    Document::String(module_name.to_string()),
                    "', '",
                    Document::String(function_name.to_string()),
                    "', [])"
                ];
                Ok(Some(doc))
            }
            MessageSelector::Keyword(parts) => {
                // Extract function name from first keyword (before the colon)
                let function_name = parts[0].keyword.trim_end_matches(':');

                // BT-855: Process arguments individually so Block arguments can be
                // wrapped via generate_erlang_interop_wrapper before crossing the
                // Erlang boundary. Non-block arguments pass through unchanged.
                let mut preamble_docs: Vec<Document<'static>> = Vec::new();
                let mut arg_parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());

                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        arg_parts.push(Document::Str(", "));
                    }
                    if let Some(block) = Self::extract_block_literal(arg) {
                        let (wrapped_doc, is_stateful) =
                            self.generate_erlang_interop_wrapper(block)?;
                        if is_stateful {
                            self.warn_stateful_block_at_erlang_boundary(
                                &format!("'{module_name}':'{function_name}'"),
                                block.span,
                            );
                        }
                        // Bind the wrapper to a temp var to avoid repeating complex exprs.
                        let wrapper_var = self.fresh_temp_var("ErlWrapper");
                        preamble_docs.push(docvec![
                            "let ",
                            Document::String(wrapper_var.clone()),
                            " = ",
                            wrapped_doc,
                            " in "
                        ]);
                        arg_parts.push(Document::String(wrapper_var));
                    } else {
                        arg_parts.push(self.expression_doc(arg)?);
                    }
                }

                // BT-1127: Route through beamtalk_erlang_proxy:direct_call/3 to
                // enable binary→charlist coercion for functions like os:cmd/1.
                // Args are wrapped in a list: call 'proxy':'direct_call'('M','F',[args])
                let call_doc = docvec![
                    "call 'beamtalk_erlang_proxy':'direct_call'('",
                    Document::String(module_name.to_string()),
                    "', '",
                    Document::String(function_name.to_string()),
                    "', [",
                    Document::Vec(arg_parts),
                    "])"
                ];

                let doc = if preamble_docs.is_empty() {
                    call_doc
                } else {
                    docvec![Document::Vec(preamble_docs), call_doc]
                };

                Ok(Some(doc))
            }
            MessageSelector::Binary(_) => {
                // Binary operators on Erlang module proxy — fall through to runtime
                Ok(None)
            }
        }
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
        if let Expression::ClassReference { name, package, .. } = receiver {
            let pkg = package.as_ref().map(|p| p.name.as_str());
            // BT-773: When inside a class method and the explicit class name matches
            // the current class, use direct dispatch (same as `self` sends) to avoid
            // deadlock. The class actor is already processing the outer call, so
            // routing through class_send would deadlock on gen_server:call.
            if self.in_class_method() && name.name == self.class_name() && pkg.is_none() {
                let doc = self.generate_class_method_self_send(selector, arguments)?;
                return Ok(Some(doc));
            }
            if self.workspace_mode() && self.context == CodeGenContext::Repl {
                // REPL top-level: check session bindings first
                let doc =
                    self.generate_binding_aware_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            if self.workspace_mode() {
                // Actor/ValueType methods in workspace mode: try class_send,
                // fall back to workspace binding for convenience names
                let doc = self.generate_workspace_class_send(&name.name, selector, arguments)?;
                return Ok(Some(doc));
            }
            // ADR 0070 Phase 2: Class method calls always go through the class
            // registry using the short class name. The package qualifier doesn't
            // affect dispatch — it's used for module name resolution in spawns and
            // standalone references, not for class method calls.
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
        if !self.in_class_method() {
            return Ok(None);
        }
        if let Expression::Identifier(id) = receiver {
            if id.name == "self" {
                let doc = self.generate_class_method_self_send(selector, arguments)?;
                return Ok(Some(doc));
            }
        }
        Ok(None)
    }

    /// Core logic for direct dispatch of class method calls.
    ///
    /// Used by both `self` sends and explicit class name sends (BT-773) within
    /// class methods. Generates direct module function calls to avoid deadlock
    /// since class methods execute inside a `gen_server:call` handler.
    fn generate_class_method_self_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_atom = selector.to_erlang_atom();

        if self.class_method_selectors().contains(&selector_atom) {
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
            return Ok(doc);
        }
        // BT-996: Auto-generated keyword constructor for Value subclass: classes.
        // `ClassName slot: value` inside a class method routes here when the selector
        // matches the auto-generated slot keyword constructor (e.g. `symName:` → `class_symName:/3`).
        // The constructor returns a plain map (not a `class_var_result` tuple), so no
        // class-var threading boilerplate is needed.
        if self
            .class_slot_constructor_selector()
            .map(String::as_str)
            .is_some_and(|kw| kw == selector_atom)
        {
            let module = self.module_name.clone();
            let cv = self.current_class_var();
            let args_doc = self.capture_argument_list_doc(arguments)?;
            let comma = if arguments.is_empty() { "" } else { ", " };
            // BT-1408: Hash long keyword constructor atoms to stay within
            // Erlang's 255-char atom limit.
            let safe_fn = super::selector_mangler::safe_class_method_fn_name(&selector_atom);
            let doc = docvec![
                "call '",
                Document::Eco(module),
                "':'",
                Document::String(safe_fn),
                "'(ClassSelf, ",
                Document::String(cv),
                comma,
                args_doc,
                ")"
            ];
            return Ok(doc);
        }
        // BT-893: Instantiation selectors (new, new:, spawn, spawnWith:) must bypass
        // gen_server to avoid deadlock — route through class_self_new/class_self_spawn.
        //
        // BT-908: Use the process dictionary instead of hardcoded class name/module so that
        // inherited class factory methods (e.g. `wrap:` from a parent) create an instance of
        // the CALLING class, not the DEFINING class. The class gen_server process sets
        // `beamtalk_class_name`, `beamtalk_class_module`, and `beamtalk_class_is_abstract` in
        // its process dictionary during init (beamtalk_object_class:init/1). When a subclass
        // inherits a factory method and invokes it, the subclass gen_server process is running,
        // so the process dictionary reflects the subclass, not the parent.
        let module = self.module_name.clone();
        match selector_atom.as_str() {
            "new" | "new:" => {
                let args_doc = self.capture_argument_list_doc(arguments)?;
                let doc = docvec![
                    "call 'beamtalk_class_instantiation':'class_self_new'(",
                    "call 'erlang':'get'('beamtalk_class_name'), ",
                    "call 'erlang':'get'('beamtalk_class_module'), [",
                    args_doc,
                    "])"
                ];
                return Ok(doc);
            }
            "spawn" | "spawnWith:" => {
                let args_doc = self.capture_argument_list_doc(arguments)?;
                let doc = docvec![
                    "call 'beamtalk_class_instantiation':'class_self_spawn'(",
                    "call 'erlang':'get'('beamtalk_class_name'), ",
                    "call 'erlang':'get'('beamtalk_class_module'), ",
                    "call 'erlang':'get'('beamtalk_class_is_abstract'), [",
                    args_doc,
                    "])"
                ];
                return Ok(doc);
            }
            _ => {}
        }

        // Other built-in exports (superclass, methods, etc.)
        let fun_name = selector_atom.replace(':', "");
        let args_doc = self.capture_argument_list_doc(arguments)?;

        let doc = docvec![
            Document::String(format!("call '{module}':'{fun_name}'(")),
            args_doc,
            ")"
        ];
        Ok(doc)
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
    ) -> Result<(Document<'static>, String)> {
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
                if self.sealed_method_selectors().contains(&selector_name) {
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

            return Ok((doc, dispatch_var));
        }
        Err(CodeGenError::Internal(
            "generate_self_dispatch_open called on non-MessageSend expression".to_string(),
        ))
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
        if self.sealed_method_selectors().contains(&selector_name) {
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
        if !self.in_class_method() {
            return false;
        }
        if let Expression::Assignment { target, .. } = expr {
            if let Expression::FieldAccess {
                receiver, field, ..
            } = target.as_ref()
            {
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    return recv_id.name == "self"
                        && self.class_var_names().contains(field.name.as_str());
                }
            }
        }
        false
    }

    /// Checks if an expression is a self-send to a class method (BT-412).
    /// These need special scoping in class method bodies because they may
    /// update `ClassVars` via `let ClassVarsN = ... in` which must not be wrapped.
    pub(super) fn is_class_method_self_send(&self, expr: &Expression) -> bool {
        if !self.in_class_method() || self.class_method_selectors().is_empty() {
            return false;
        }
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            if let Expression::Identifier(id) = receiver.as_ref() {
                if id.name == "self" {
                    let sel_atom = selector.to_erlang_atom();
                    return self.class_method_selectors().contains(&sel_atom);
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
    /// BT-920: Excludes cast sends (`self method!`), which are fire-and-forget
    /// and must not thread state through the loop accumulator.
    pub(super) fn is_actor_self_send(&self, expr: &Expression) -> bool {
        if self.context != super::CodeGenContext::Actor {
            return false;
        }
        if let Expression::MessageSend {
            receiver, is_cast, ..
        } = expr
        {
            if *is_cast {
                return false;
            }
            if let Expression::Identifier(id) = receiver.as_ref() {
                return id.name == "self";
            }
        }
        false
    }

    /// BT-1420: Checks if an expression is a self-send that goes through `safe_dispatch`
    /// (or sealed dispatch) and returns `{reply, Result, NewState}`.
    ///
    /// Excludes self-sends with selectors that are intercepted by handlers before
    /// `try_handle_self_dispatch` in `generate_message_send`:
    /// - Binary operators (`+`, `-`, `*`, etc.)
    /// - `asType:` (compile-time erasure)
    /// - `ProtoObject` messages (`class`, `perform:`, `perform:withArguments:`)
    /// - Object reflection (`fieldAt:`, `fieldAt:put:`, `fieldNames`, `respondsTo:`)
    /// - Nil protocol (`isNil`, `notNil`, `ifNil:`, etc.)
    /// - Identity (`yourself`, `hash`)
    /// - Error signaling (`error:`)
    /// - Block evaluation (`value`, `value:`, `repeat`, `whileTrue:`, etc.)
    pub(super) fn is_dispatching_actor_self_send(&self, expr: &Expression) -> bool {
        if !self.is_actor_self_send(expr) {
            return false;
        }
        if let Expression::MessageSend { selector, .. } = expr {
            // Binary operators are always intercepted by generate_binary_op
            if matches!(selector, MessageSelector::Binary(_)) {
                return false;
            }
            let name = selector.name();
            // Selectors intercepted before try_handle_self_dispatch
            if matches!(
                name.as_str(),
                // asType: (compile-time erasure)
                "asType:"
                // ProtoObject
                | "class" | "perform:" | "perform:withArguments:" | "performLocally:withArguments:"
                // Object reflection
                | "fieldAt:" | "fieldAt:put:" | "fieldNames" | "respondsTo:"
                // Nil protocol
                | "isNil" | "notNil" | "ifNil:" | "ifNotNil:"
                | "ifNil:ifNotNil:" | "ifNotNil:ifNil:"
                // Identity
                | "yourself" | "hash"
                // Error signaling
                | "error:"
                // Block evaluation
                | "value" | "value:" | "value:value:" | "value:value:value:"
                | "repeat" | "whileTrue:" | "whileFalse:"
            ) {
                return false;
            }
        }
        true
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
    ) -> Result<(Document<'static>, String)> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                // BT-1342: Full-extract mode — rebind field param instead of maps:put.
                // When the field is in hybrid_mutated_fields, the field has been extracted
                // to a direct fun parameter. We rebind it to a fresh variable and update
                // the readonly params map so subsequent reads use the new variable.
                if self.in_hybrid_loop && self.hybrid_mutated_fields.contains(field.name.as_str()) {
                    let val_var = self.fresh_temp_var("Val");
                    // Snapshot field params before evaluating RHS so nested field
                    // assignments (e.g. `self.x := (self.y := 42)`) don't leak
                    // inner updates past the outer assignment.
                    let saved_field_params = self.hybrid_readonly_field_params.clone();
                    let val_doc = self.expression_doc(value)?;
                    self.hybrid_readonly_field_params = saved_field_params;
                    let new_field_var = self
                        .fresh_temp_var(&format!("{}Field", Self::to_core_erlang_var(&field.name)));
                    // Update the param map so subsequent reads use the new var.
                    self.hybrid_readonly_field_params
                        .insert(field.name.to_string(), new_field_var.clone());
                    return Ok((
                        docvec![
                            "let ",
                            Document::String(val_var.clone()),
                            " = ",
                            val_doc,
                            " in let ",
                            Document::String(new_field_var),
                            " = ",
                            Document::String(val_var.clone()),
                            " in ",
                        ],
                        val_var,
                    ));
                }

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

                // BT-884: Return the val var so callers (e.g. cascade codegen) can
                // reference the assigned value after hoisting the binding.
                return Ok((doc, val_var));
            }
        }
        Err(CodeGenError::Internal(
            "generate_field_assignment_open called on non-field-assignment expression".to_string(),
        ))
    }

    /// BT-1324: Checks if an expression is `self fieldAt: <name> put: <value>` in actor context.
    /// These need state threading via maps:put, similar to field assignments.
    pub(super) fn is_self_field_at_put(&self, expr: &Expression) -> bool {
        if self.context != super::CodeGenContext::Actor {
            return false;
        }
        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            if let Expression::Identifier(id) = receiver.as_ref() {
                if id.name == "self"
                    && self.lookup_var("self").is_none()
                    && parts.len() == 2
                    && parts[0].keyword == "fieldAt:"
                    && parts[1].keyword == "put:"
                    && arguments.len() == 2
                {
                    return true;
                }
            }
        }
        false
    }

    /// BT-1324: Generates the opening part of a `self fieldAt: name put: value` with state threading.
    ///
    /// Similar to `generate_field_assignment_open` but with a dynamic field name.
    /// Generates:
    /// ```erlang
    /// let _Name = <name> in
    /// let _Val = <value> in
    /// let StateN = call 'maps':'put'(_Name, _Val, StateN-1) in
    /// ```
    ///
    /// The caller is responsible for closing the expression.
    pub(super) fn generate_self_field_at_put_open(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend { arguments, .. } = expr {
            let name_var = self.fresh_var("Name");
            let val_var = self.fresh_temp_var("Val");
            let name_code = self.expression_doc(&arguments[0])?;
            // Capture state before value expression, consistent with
            // generate_field_assignment_open. If the value expression itself
            // threads state (e.g., contains a nested field assignment), the
            // maps:put uses the pre-value state — same semantics as self.x := expr.
            let current_state = self.current_state_var();
            let val_code = self.expression_doc(&arguments[1])?;
            let new_state = self.next_state_var();

            let doc = docvec![
                "let ",
                Document::String(name_var.clone()),
                " = ",
                name_code,
                " in let ",
                Document::String(val_var.clone()),
                " = ",
                val_code,
                " in let ",
                Document::String(new_state),
                " = call 'maps':'put'(",
                Document::String(name_var),
                ", ",
                Document::String(val_var.clone()),
                ", ",
                Document::String(current_state),
                ") in ",
            ];

            return Ok((doc, val_var));
        }
        Err(CodeGenError::Internal(
            "generate_self_field_at_put_open called on non-fieldAt:put: expression".to_string(),
        ))
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
    /// The emitted module atom is computed dynamically via `compiled_module_name`:
    /// - Package mode: `bt@{package}@{class}` (e.g. `bt@my_pkg@counter`)
    /// - Workspace/legacy mode: `bt@{class}` (e.g. `bt@counter`)
    ///
    /// # Arguments
    ///
    /// * `class_name` - The Beamtalk class name (e.g., "Counter")
    /// * `init_args` - Optional initialization arguments for spawnWith:
    ///
    /// # Generated Code (REPL context, package mode with package `my_pkg`)
    ///
    /// ```erlang
    /// case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of
    ///   <'undefined'> when 'true' ->
    ///     call 'bt@my_pkg@counter':'spawn'()
    ///   <RegistryPid> when 'true' ->
    ///     let SpawnResult = call 'bt@my_pkg@counter':'spawn'() in
    ///     let {'beamtalk_object', _, _, SpawnPid} = SpawnResult in
    ///     let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, 'Counter', 'bt@my_pkg@counter') in
    ///     SpawnResult
    /// end
    /// ```
    ///
    /// # Generated Code (non-REPL context, package mode with package `my_pkg`)
    ///
    /// ```erlang
    /// call 'bt@my_pkg@counter':'spawn'()
    /// ```
    /// Generates actor spawn with optional package qualifier (ADR 0070 Phase 2).
    ///
    /// When `package` is `Some`, uses `resolve_qualified_module_name` to compute
    /// the BEAM module name directly (e.g., `json@Parser` → `bt@json@parser`).
    /// When `None`, falls back to `compiled_module_name` for standard resolution.
    ///
    /// In REPL context, registers the spawned actor with the REPL actor registry.
    /// In non-REPL contexts, calls the module's `spawn/0` or `spawn/1` directly.
    pub(super) fn generate_actor_spawn_qualified(
        &mut self,
        class_name: &str,
        package: Option<&str>,
        init_args: Option<&Expression>,
    ) -> Result<Document<'static>> {
        let module_name = self.compiled_module_name_qualified(class_name, package);
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
        // BT-1935: Close open-scope let-chains from class method self-sends.
        let saved_cv = self.class_var_version();
        let (receiver_doc, receiver_open_scope) = self.expression_doc_with_open_scope(receiver)?;
        let actual_receiver = if let Some(result_var) = receiver_open_scope {
            self.set_class_var_version(saved_cv);
            docvec![receiver_doc, Document::String(result_var)]
        } else {
            receiver_doc
        };

        let saved_cv2 = self.class_var_version();
        let (arg_doc, arg_open_scope) = self.expression_doc_with_open_scope(&arguments[0])?;
        let actual_arg = if let Some(result_var) = arg_open_scope {
            self.set_class_var_version(saved_cv2);
            docvec![arg_doc, Document::String(result_var)]
        } else {
            arg_doc
        };

        let doc = docvec![
            "call 'beamtalk_method_resolver':'resolve'(",
            actual_receiver,
            ", ",
            actual_arg,
            ")"
        ];
        Ok(doc)
    }

    /// Generates a binding-aware class method call (ADR 0019 Phase 3).
    ///
    /// In workspace mode, checks REPL bindings first for convenience names.
    /// If the name is found in bindings, it's an instance (e.g., Transcript is a
    /// `TranscriptStream` actor), so dispatch via `beamtalk_message_dispatch:send/3`.
    /// If not found, fall back to direct call (BT-1639) or `class_send`.
    ///
    /// ```erlang
    /// case call 'maps':'find'('Name', State) of
    ///   <{'ok', BindingVal}> -> call 'beamtalk_message_dispatch':'send'(BindingVal, Sel, Args)
    ///   <'error'> -> call 'module':'class_selector'('nil', ~{}~, Args)  %% BT-1639 direct
    ///                %% OR: class_send fallback for non-eligible classes
    /// end
    /// ```
    fn generate_binding_aware_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // BT-1408: The binding branch dispatches to instances via
        // beamtalk_message_dispatch:send — use the raw selector (only hashed
        // if the selector itself exceeds the atom limit) so instance method
        // lookup works normally.  The class_send fallback uses the class-method
        // mangled selector which triggers earlier (when "class_" + selector
        // exceeds the limit).
        let raw = selector.to_erlang_atom();
        let instance_selector = super::selector_mangler::safe_atom_name(&raw);
        let binding_val_var = self.fresh_var("BindingVal");
        let state_var = self.current_state_var();
        let args_doc = self.capture_argument_list_doc(arguments)?;

        // BT-1639: Build the class-side fallback: direct call or gen_server
        let class_fallback: Document<'static> =
            if let Some(info) = self.direct_call_eligible.get(class_name) {
                if info.selectors.contains(&raw) {
                    let safe_fn = super::selector_mangler::safe_class_method_fn_name(&raw);
                    let comma = if arguments.is_empty() { "" } else { ", " };
                    docvec![
                        "call '",
                        Document::Eco(info.module_name.clone()),
                        "':'",
                        Document::String(safe_fn),
                        "'('nil', ~{}~",
                        comma,
                        args_doc.clone(),
                        ")"
                    ]
                } else {
                    self.generate_class_send_fallback(class_name, &raw, args_doc.clone())
                }
            } else {
                self.generate_class_send_fallback(class_name, &raw, args_doc.clone())
            };

        let doc = docvec![
            Document::String(format!(
                "case call 'maps':'find'('{class_name}', {state_var}) of "
            )),
            Document::String(format!("<{{'ok', {binding_val_var}}}> when 'true' -> ")),
            Document::String(format!(
                "call 'beamtalk_message_dispatch':'send'({binding_val_var}, '{instance_selector}', ["
            )),
            args_doc,
            "]) ",
            "<'error'> when 'true' -> ",
            class_fallback,
            " end"
        ];

        Ok(doc)
    }

    /// Generates workspace-mode class send for actor/value-type methods.
    ///
    /// BT-1639: For sealed classes eligible for direct call, generates a direct
    /// function call instead of `gen_server` dispatch. Otherwise tries `class_send`
    /// first (for real class names like `Counter`), returns nil for unresolved names.
    /// ADR 0019 Phase 4: No `persistent_term` fallback — convenience names resolve
    /// via session bindings in REPL context.
    fn generate_workspace_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let raw_selector = selector.to_erlang_atom();

        // BT-1639: Direct call optimization for sealed class methods
        if let Some(info) = self.direct_call_eligible.get(class_name) {
            if info.selectors.contains(&raw_selector) {
                return self.generate_direct_class_method_call(
                    &info.module_name.clone(),
                    &raw_selector,
                    arguments,
                );
            }
        }

        // BT-1408: Hash long selector atoms to stay within Erlang's 255-char atom limit.
        let selector_atom = super::selector_mangler::safe_class_method_selector(&raw_selector);
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
    /// For sealed classes with no class variables (BT-1639), generates a direct
    /// function call to `module:class_<selector>(nil, #{}, Args...)`, bypassing
    /// the `gen_server` round-trip. This is safe because the methods are pure functions.
    ///
    /// For all other classes (or unrecognized selectors), falls back to the
    /// `gen_server` dispatch path via `beamtalk_object_class:class_send/3`.
    ///
    /// # Generated Code (direct call, BT-1639)
    ///
    /// ```erlang
    /// call 'bt@stdlib@tracing':'class_setContext:'('nil', ~{}~, Ctx)
    /// ```
    ///
    /// # Generated Code (`gen_server` fallback)
    ///
    /// ```erlang
    /// let ClassPid = call 'beamtalk_class_registry':'whereis_class'('Tracing') in
    /// call 'beamtalk_object_class':'class_send'(ClassPid, 'setContext:', [Ctx])
    /// ```
    fn generate_class_method_call(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let raw_selector = selector.to_erlang_atom();

        // BT-1639: Check if this class method is eligible for direct call optimization.
        if let Some(info) = self.direct_call_eligible.get(class_name) {
            if info.selectors.contains(&raw_selector) {
                return self.generate_direct_class_method_call(
                    &info.module_name.clone(),
                    &raw_selector,
                    arguments,
                );
            }
        }

        // Fallback: gen_server dispatch via class_send
        // BT-1408: Hash long selector atoms (e.g. keyword constructors with many
        // fields) to stay within Erlang's 255-char atom limit.
        let selector_atom = super::selector_mangler::safe_class_method_selector(&raw_selector);
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

    /// BT-1639: Generates a direct function call to a sealed class method.
    ///
    /// Passes `nil` for `ClassSelf` and `#{}` for `ClassVars` since sealed classes
    /// with no class variables never reference these parameters.
    ///
    /// ```erlang
    /// call 'module':'class_<selector>'('nil', #{}, Args...)
    /// ```
    fn generate_direct_class_method_call(
        &mut self,
        module_name: &str,
        selector: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // BT-1408: Hash long selector atoms to stay within Erlang's 255-char atom limit.
        let safe_fn = super::selector_mangler::safe_class_method_fn_name(selector);
        let args_doc = self.capture_argument_list_doc(arguments)?;
        let comma = if arguments.is_empty() { "" } else { ", " };

        // Core Erlang empty map is ~{}~ (not #{} which is Erlang source syntax)
        let doc = docvec![
            "call '",
            Document::String(module_name.to_string()),
            "':'",
            Document::String(safe_fn),
            "'('nil', ~{}~",
            comma,
            args_doc,
            ")"
        ];

        Ok(doc)
    }

    /// BT-1639: Generates the `gen_server` `class_send` fallback for binding-aware dispatch.
    ///
    /// Used when a class method is not eligible for direct call optimization.
    fn generate_class_send_fallback(
        &mut self,
        class_name: &str,
        raw_selector: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        let class_selector = super::selector_mangler::safe_class_method_selector(raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        docvec![
            "let ",
            Document::String(class_pid_var.clone()),
            " = call 'beamtalk_class_registry':'whereis_class'('",
            Document::String(class_name.to_string()),
            "') in ",
            "call 'beamtalk_object_class':'class_send'(",
            Document::String(class_pid_var),
            ", '",
            Document::String(class_selector),
            "', [",
            args_doc,
            "])"
        ]
    }

    /// BT-851: Pre-scans a class for self-sends that pass Tier 2 (stateful) block arguments.
    ///
    /// Walks all method bodies looking for `self <selector>: args` where an argument
    /// is a literal block with captured mutations (`captured_reads ∩ local_writes` non-empty).
    /// Records the target method selector and parameter position in `tier2_method_info`.
    pub(super) fn scan_class_for_tier2_blocks(&mut self, class: &crate::ast::ClassDefinition) {
        use super::block_analysis::analyze_block;

        // Clear previous class's info to avoid cross-class pollution in multi-class modules
        self.tier2_method_info.clear();

        for method in &class.methods {
            for stmt in &method.body {
                self.scan_expr_for_tier2(&stmt.expression, &analyze_block);
            }
        }
    }

    /// BT-851: Recursively scans an expression for Tier 2 block arguments in self-sends.
    fn scan_expr_for_tier2(
        &mut self,
        expr: &Expression,
        analyze: &dyn Fn(&crate::ast::Block) -> super::block_analysis::BlockMutationAnalysis,
    ) {
        match expr {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                // Check for self-sends
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if id.name == "self" {
                        let sel_name = selector.name().to_string();
                        for (i, arg) in arguments.iter().enumerate() {
                            if let Expression::Block(block) = arg {
                                let analysis = analyze(block);
                                let has_captured_mutations = analysis
                                    .local_writes
                                    .intersection(&analysis.captured_reads)
                                    .next()
                                    .is_some();
                                // BT-1140: Also promote blocks with field writes to Tier 2.
                                let has_field_writes = !analysis.field_writes.is_empty();
                                if has_captured_mutations || has_field_writes {
                                    self.tier2_method_info
                                        .entry(sel_name.clone())
                                        .or_default()
                                        .push(i);
                                }
                            }
                        }
                    }
                }
                // Recurse into receiver and arguments
                self.scan_expr_for_tier2(receiver, analyze);
                for arg in arguments {
                    self.scan_expr_for_tier2(arg, analyze);
                }
            }
            Expression::Assignment { target, value, .. } => {
                self.scan_expr_for_tier2(target, analyze);
                self.scan_expr_for_tier2(value, analyze);
            }
            Expression::Block(block) => {
                for body_stmt in &block.body {
                    self.scan_expr_for_tier2(&body_stmt.expression, analyze);
                }
            }
            Expression::Return { value, .. } => {
                self.scan_expr_for_tier2(value, analyze);
            }
            Expression::Parenthesized { expression, .. } => {
                self.scan_expr_for_tier2(expression, analyze);
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // Detect cascaded self-sends as Tier 2 call sites
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if id.name == "self" {
                        for msg in messages {
                            let sel_name = msg.selector.name().to_string();
                            for (i, arg) in msg.arguments.iter().enumerate() {
                                if let Expression::Block(block) = arg {
                                    let analysis = analyze(block);
                                    let has_captured_mutations = analysis
                                        .local_writes
                                        .intersection(&analysis.captured_reads)
                                        .next()
                                        .is_some();
                                    // BT-1140: Also promote blocks with field writes to Tier 2.
                                    let has_field_writes = !analysis.field_writes.is_empty();
                                    if has_captured_mutations || has_field_writes {
                                        self.tier2_method_info
                                            .entry(sel_name.clone())
                                            .or_default()
                                            .push(i);
                                    }
                                }
                            }
                        }
                    }
                }
                // Recurse into receiver and arguments
                self.scan_expr_for_tier2(receiver, analyze);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.scan_expr_for_tier2(arg, analyze);
                    }
                }
            }
            Expression::Match { value, arms, .. } => {
                self.scan_expr_for_tier2(value, analyze);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.scan_expr_for_tier2(guard, analyze);
                    }
                    self.scan_expr_for_tier2(&arm.body, analyze);
                }
            }
            _ => {}
        }
    }

    /// BT-851: Checks if an expression is a self-send with Tier 2 block arguments.
    ///
    /// Returns the captured-mutated variable names for each Tier 2 block argument
    /// if this is a Tier 2 self-send, or `None` if it's a regular self-send.
    ///
    /// BT-870: Also promotes literal Tier 1 blocks at call sites where the target
    /// method is a known Tier 2 HOM (present in `tier2_method_info`). A promoted
    /// block is compiled with the Tier 2 signature (`fun(Args, StateAcc) -> {Result, StateAcc}`)
    /// even though it has no captured mutations, ensuring the callee's arity expectation is met.
    pub(super) fn detect_tier2_self_send(
        &self,
        expr: &Expression,
    ) -> Option<Vec<(usize, Vec<String>)>> {
        use super::block_analysis::analyze_block;

        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            if let Expression::Identifier(id) = receiver.as_ref() {
                if id.name == "self" {
                    let sel_name = selector.name().to_string();
                    // BT-870: Collect positions the scanner identified as Tier 2 for this selector.
                    let hom_positions: std::collections::HashSet<usize> = self
                        .tier2_method_info
                        .get(&sel_name)
                        .map(|positions| positions.iter().copied().collect())
                        .unwrap_or_default();

                    let mut tier2_args = Vec::new();
                    for (i, arg) in arguments.iter().enumerate() {
                        if let Expression::Block(block) = arg {
                            let analysis = analyze_block(block);
                            let captured_mutations: Vec<String> = analysis
                                .local_writes
                                .intersection(&analysis.captured_reads)
                                .cloned()
                                .collect::<std::collections::BTreeSet<_>>()
                                .into_iter()
                                .collect();
                            if !captured_mutations.is_empty() {
                                tier2_args.push((i, captured_mutations));
                            } else if !analysis.field_writes.is_empty() {
                                // BT-1140: Field-write block — promote to Tier 2 with no local
                                // vars. The actor State IS the StateAcc; field reads/writes
                                // are threaded through it automatically inside the block body.
                                tier2_args.push((i, vec![]));
                            } else if hom_positions.contains(&i) {
                                // BT-870: Block has no mutations but this position is a known
                                // Tier 2 HOM param. Promote to Tier 2 with empty captured vars
                                // so it gets `fun(Args, StateAcc) -> {Result, StateAcc}` signature
                                // (StateAcc passthrough), matching the callee's arity expectation.
                                tier2_args.push((i, vec![]));
                            }
                        } else if let Expression::Identifier(arg_id) = arg {
                            // BT-912: If the argument is an identifier that is a known Tier 2
                            // block parameter of the current method, treat it as a Tier 2 HOM
                            // argument with no captured mutations. This handles nested HOMs where
                            // one method delegates a Tier 2 block to another (e.g.
                            // `outerEachItem: aBlock => self eachItem: aBlock`). The block was
                            // already compiled as Tier 2 by the outer caller; we need to ensure
                            // the returned state is threaded back through the delegation chain.
                            if self.tier2_block_params.contains(arg_id.name.as_str()) {
                                tier2_args.push((i, vec![]));
                            }
                        }
                    }
                    if !tier2_args.is_empty() {
                        return Some(tier2_args);
                    }
                }
            }
        }
        None
    }

    /// BT-851: Generates a self-dispatch with Tier 2 block arguments and state threading.
    ///
    /// Before the self-send:
    /// 1. Packs captured-mutated locals into State
    /// 2. Generates block arguments with Tier 2 stateful signature
    ///
    /// After the self-send:
    /// 1. Extracts captured-mutated locals from the returned State
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// let State1 = call 'maps':'put'('__local__count', Count, State) in
    /// let _SD0 = case call 'module':'safe_dispatch'('applyBlock:to:',
    ///     [fun (X, StateAcc) -> ... {Result, StateAcc1} end, 5], State1) of
    ///   <{'reply', R, S}> when 'true' -> {R, S}
    ///   <{'error', E, _}> when 'true' -> call 'beamtalk_error':'raise'(E)
    /// end in let State2 = call 'erlang':'element'(2, _SD0) in
    /// let Count = call 'maps':'get'('__local__count', State2) in
    /// ```
    pub(super) fn generate_tier2_self_send_open(
        &mut self,
        expr: &Expression,
        tier2_args: &[(usize, Vec<String>)],
    ) -> Result<(Document<'static>, String)> {
        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            let mut docs: Vec<Document<'static>> = Vec::new();

            // Step 1: Pack captured-mutated locals into State
            for (_pos, captured_vars) in tier2_args {
                for var_name in captured_vars {
                    let core_var = self
                        .lookup_var(var_name)
                        .cloned()
                        .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                    let key = Self::local_state_key(var_name);
                    let current_state = self.current_state_var();
                    let new_state = self.next_state_var();
                    docs.push(docvec![
                        "let ",
                        Document::String(new_state),
                        " = call 'maps':'put'('",
                        Document::String(key),
                        "', ",
                        Document::String(core_var),
                        ", ",
                        Document::String(current_state),
                        ") in "
                    ]);
                }
            }

            // Step 2: Generate argument list with Tier 2 blocks
            let selector_atom = selector.to_erlang_atom();
            let dispatch_var = self.fresh_temp_var("SD");
            let result_var = self.fresh_var("SDResult");
            let state_var = self.fresh_var("SDState");
            let error_var = self.fresh_var("SDError");
            let current_state = self.current_state_var();
            let module = self.module_name.clone();
            let args_doc = self.generate_tier2_args(arguments, tier2_args)?;

            // Step 3: Generate the self-dispatch (using safe_dispatch or sealed path)
            let call_doc = self.generate_tier2_dispatch_call(
                selector,
                arguments.is_empty(),
                &selector_atom,
                &dispatch_var,
                &current_state,
                &module,
                args_doc,
            );

            // Result/error clauses + state extraction
            let new_state = self.next_state_var();
            docs.push(docvec![
                call_doc,
                "<{'reply', ",
                Document::String(result_var.clone()),
                ", ",
                Document::String(state_var.clone()),
                "}> when 'true' -> {",
                Document::String(result_var),
                ", ",
                Document::String(state_var),
                "} <{'error', ",
                Document::String(error_var.clone()),
                ", _}> when 'true' -> call 'beamtalk_error':'raise'(",
                Document::String(error_var),
                ") end in let ",
                Document::String(new_state),
                " = call 'erlang':'element'(2, ",
                Document::String(dispatch_var.clone()),
                ") in "
            ]);

            // Step 4: Extract captured-mutated locals from the returned State
            let final_state = self.current_state_var();
            for (_pos, captured_vars) in tier2_args {
                for var_name in captured_vars {
                    let core_var = self
                        .lookup_var(var_name)
                        .cloned()
                        .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
                    let key = Self::local_state_key(var_name);
                    docs.push(docvec![
                        "let ",
                        Document::String(core_var),
                        " = call 'maps':'get'('",
                        Document::String(key),
                        "', ",
                        Document::String(final_state.clone()),
                        ") in "
                    ]);
                }
            }

            return Ok((Document::Vec(docs), dispatch_var));
        }
        Err(CodeGenError::Internal(
            "generate_tier2_self_send_open called on non-MessageSend expression".to_string(),
        ))
    }

    /// BT-851: Builds argument list for a Tier 2 self-send, using stateful block
    /// generation for marked positions.
    fn generate_tier2_args(
        &mut self,
        arguments: &[Expression],
        tier2_args: &[(usize, Vec<String>)],
    ) -> Result<Document<'static>> {
        let tier2_positions: std::collections::HashSet<usize> =
            tier2_args.iter().map(|(pos, _)| *pos).collect();
        let tier2_vars_by_pos: std::collections::HashMap<usize, &Vec<String>> =
            tier2_args.iter().map(|(pos, vars)| (*pos, vars)).collect();

        let mut arg_parts: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                arg_parts.push(Document::Str(", "));
            }
            if tier2_positions.contains(&i) {
                if let Expression::Block(block) = arg {
                    let captured_vars = tier2_vars_by_pos[&i];
                    arg_parts.push(self.generate_block_stateful(block, captured_vars)?);
                } else {
                    arg_parts.push(self.expression_doc(arg)?);
                }
            } else {
                arg_parts.push(self.expression_doc(arg)?);
            }
        }
        Ok(Document::Vec(arg_parts))
    }

    /// BT-851: Generates the dispatch call for a Tier 2 self-send.
    ///
    /// Handles sealed (direct/dispatch) and non-sealed (`safe_dispatch`) paths.
    #[allow(clippy::too_many_arguments)]
    fn generate_tier2_dispatch_call(
        &mut self,
        selector: &MessageSelector,
        no_args: bool,
        selector_atom: &str,
        dispatch_var: &str,
        current_state: &str,
        module: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        if self.is_class_sealed() {
            let selector_name = selector.name().to_string();
            if self.sealed_method_selectors().contains(&selector_name) {
                let self_var = self.fresh_temp_var("SealedSelf");
                let comma = if no_args { "" } else { ", " };
                docvec![
                    "let ",
                    Document::String(self_var.clone()),
                    " = call 'beamtalk_actor':'make_self'(",
                    Document::String(current_state.to_string()),
                    ") in let ",
                    Document::String(dispatch_var.to_string()),
                    " = case call '",
                    Document::String(module.to_string()),
                    "':'__sealed_",
                    Document::String(selector_name),
                    "'(",
                    args_doc,
                    comma,
                    Document::String(self_var),
                    ", ",
                    Document::String(current_state.to_string()),
                    ") of "
                ]
            } else {
                let self_var = self.fresh_temp_var("SealedSelf");
                docvec![
                    "let ",
                    Document::String(self_var.clone()),
                    " = call 'beamtalk_actor':'make_self'(",
                    Document::String(current_state.to_string()),
                    ") in let ",
                    Document::String(dispatch_var.to_string()),
                    " = case call '",
                    Document::String(module.to_string()),
                    "':'dispatch'('",
                    Document::String(selector_atom.to_string()),
                    "', [",
                    args_doc,
                    "], ",
                    Document::String(self_var),
                    ", ",
                    Document::String(current_state.to_string()),
                    ") of "
                ]
            }
        } else {
            docvec![
                "let ",
                Document::String(dispatch_var.to_string()),
                " = case call '",
                Document::String(module.to_string()),
                "':'safe_dispatch'('",
                Document::String(selector_atom.to_string()),
                "', [",
                args_doc,
                "], ",
                Document::String(current_state.to_string()),
                ") of "
            ]
        }
    }
}

// NOTE: class_method_module_name and related helpers (is_primitive_stdlib_class,
// is_bt_stdlib_class, is_erlang_stdlib_module) were removed in BT-411.
// Class dispatch now goes through runtime class_send/3 instead of
// compile-time module name resolution.

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Identifier, KeywordPart, Literal, MessageSelector};
    use crate::codegen::core_erlang::CoreErlangGenerator;
    use crate::source_analysis::Span;

    fn s() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn test_generate_message_send_unary_uses_dispatch() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Identifier(Identifier::new("counter", s()));
        let selector = MessageSelector::Unary("increment".into());
        let doc = generator
            .generate_message_send(&receiver, &selector, &[])
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_message_dispatch':'send'("),
            "unary send should use unified dispatch. Got: {output}"
        );
        assert!(
            output.contains("'increment'"),
            "should include selector atom. Got: {output}"
        );
    }

    #[test]
    fn test_generate_cast_send_non_actor_routes_via_cast() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Identifier(Identifier::new("other", s()));
        let selector = MessageSelector::Unary("doIt".into());
        let doc = generator
            .generate_cast_send(&receiver, &selector, &[])
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_message_dispatch':'cast'("),
            "non-actor cast send should route through cast/3. Got: {output}"
        );
    }

    #[test]
    fn test_generate_super_send_uses_beamtalk_dispatch() {
        let mut generator = CoreErlangGenerator::new("test");
        let selector = MessageSelector::Unary("initialize".into());
        let doc = generator.generate_super_send(&selector, &[]).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_dispatch':'super'("),
            "super send should use beamtalk_dispatch:super. Got: {output}"
        );
        assert!(
            output.contains("'initialize'"),
            "should include selector. Got: {output}"
        );
    }

    #[test]
    fn test_generate_actor_spawn_non_repl() {
        let mut generator = CoreErlangGenerator::new("test");
        let doc = generator
            .generate_actor_spawn_qualified("Counter", None, None)
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'spawn'()"),
            "spawn should call spawn/0. Got: {output}"
        );
        assert!(
            output.contains("counter"),
            "spawn should reference module. Got: {output}"
        );
    }

    #[test]
    fn test_generate_message_send_keyword_includes_selector() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Identifier(Identifier::new("obj", s()));
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("at:", s()),
            KeywordPart::new("put:", s()),
        ]);
        let arguments = vec![
            Expression::Literal(Literal::Integer(1), s()),
            Expression::Literal(Literal::Integer(2), s()),
        ];
        let doc = generator
            .generate_message_send(&receiver, &selector, &arguments)
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'at:put:'"),
            "keyword send should combine selector parts. Got: {output}"
        );
    }

    #[test]
    fn test_generate_message_send_binary_op_addition() {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::Literal(Literal::Integer(3), s());
        let selector = MessageSelector::Binary("+".into());
        let arguments = vec![Expression::Literal(Literal::Integer(4), s())];
        let doc = generator
            .generate_message_send(&receiver, &selector, &arguments)
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("erlang':'+'("),
            "binary + should compile to erlang arithmetic. Got: {output}"
        );
    }

    #[test]
    fn test_generate_cast_send_actor_self_uses_safe_dispatch() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.context = crate::codegen::core_erlang::CodeGenContext::Actor;
        let receiver = Expression::Identifier(Identifier::new("self", s()));
        let selector = MessageSelector::Unary("doIt".into());
        let doc = generator
            .generate_cast_send(&receiver, &selector, &[])
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("safe_dispatch"),
            "actor self cast should use safe_dispatch. Got: {output}"
        );
        assert!(
            output.contains("'ok'"),
            "actor self cast should return 'ok'. Got: {output}"
        );
    }

    /// BT-1475: Self-cast inside a block must route through the actor mailbox,
    /// not call `safe_dispatch` directly, because the block may execute in a
    /// different process (Timer callback, cross-actor callback).
    #[test]
    fn test_generate_cast_send_actor_self_in_block_uses_mailbox() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.context = crate::codegen::core_erlang::CodeGenContext::Actor;
        generator.block_depth = 1; // Simulate being inside a block
        let receiver = Expression::Identifier(Identifier::new("self", s()));
        let selector = MessageSelector::Unary("bump".into());
        let doc = generator
            .generate_cast_send(&receiver, &selector, &[])
            .unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("beamtalk_message_dispatch"),
            "self cast inside block should route through mailbox. Got: {output}"
        );
        assert!(
            output.contains("cast"),
            "self cast inside block should use cast dispatch. Got: {output}"
        );
        assert!(
            !output.contains("safe_dispatch"),
            "self cast inside block must NOT use safe_dispatch. Got: {output}"
        );
    }
}
