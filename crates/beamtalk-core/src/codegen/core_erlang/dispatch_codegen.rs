// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Message sending and dispatch compilation.
//!
//! This module handles the **core domain operation** of Beamtalk: message sending.
//! In Smalltalk and Beamtalk, message sending is THE fundamental operation, not
//! method calls or function invocation.
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

use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{Expression, MessageSelector};
use std::fmt::Write;

/// BT-374 / ADR 0010: Compile-time known workspace binding names.
///
/// These names resolve to workspace singletons via `persistent_term` rather than
/// direct module function calls. The set is static — dynamic bindings are out of scope.
const WORKSPACE_BINDING_NAMES: &[&str] = &["Transcript", "Beamtalk", "Workspace"];

/// Returns true if the given class name is a workspace binding (ADR 0010).
pub(super) fn is_workspace_binding(name: &str) -> bool {
    WORKSPACE_BINDING_NAMES.contains(&name)
}

impl CoreErlangGenerator {
    /// Generates a comma-separated argument list for function/message calls.
    ///
    /// This is a shared helper that eliminates the repeated pattern of iterating
    /// over arguments with comma separation found throughout dispatch codegen.
    fn generate_argument_list(&mut self, arguments: &[Expression]) -> Result<()> {
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
        }
        Ok(())
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
    ) -> Result<()> {
        // Special case: super message send
        // Super calls invoke the superclass implementation
        if matches!(receiver, Expression::Super(_)) {
            return self.generate_super_send(selector, arguments);
        }

        // For binary operators, use Erlang's built-in operators (these are synchronous)
        if let MessageSelector::Binary(op) = selector {
            // BT-101: Method lookup via `>>` operator (e.g., Counter >> #increment)
            if op.as_str() == ">>" {
                if let Expression::ClassReference { name, .. } = receiver {
                    return self.generate_method_lookup(&name.name, arguments);
                }
                // >> is only supported on class literals for now
                return Err(CodeGenError::UnsupportedFeature {
                    feature: ">> (method lookup) is only supported on class literals (e.g., Counter >> #increment), not on expressions".to_string(),
                    location: format!("{receiver:?}"),
                });
            }

            // BT-335: Association creation via `->` binary message
            if op.as_str() == "->" {
                if arguments.len() != 1 {
                    return Err(CodeGenError::Internal(
                        "-> operator must have exactly one argument".to_string(),
                    ));
                }
                write!(
                    self.output,
                    "~{{'$beamtalk_class' => 'Association', 'key' => "
                )?;
                self.generate_expression(receiver)?;
                write!(self.output, ", 'value' => ")?;
                self.generate_expression(&arguments[0])?;
                write!(self.output, "}}~")?;
                return Ok(());
            }
            return self.generate_binary_op(op, receiver, arguments);
        }

        // Special case: ProtoObject methods - fundamental operations on all objects
        // class returns the class name for any object (primitives or actors)
        if let Some(result) =
            self.try_generate_protoobject_message(receiver, selector, arguments)?
        {
            return Ok(result);
        }

        // Special case: Object methods - reflection and introspection
        // respondsTo:, instVarNames, instVarAt: enable runtime introspection
        if let Some(result) = self.try_generate_object_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: Block evaluation messages (value, value:, whileTrue:, etc.)
        // These are synchronous function calls, not async actor messages
        if let Some(result) = self.try_generate_block_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: List iteration messages (do:, collect:, select:, reject:, inject:into:)
        // These are structural intrinsics that require inline code generation for proper
        // state threading when used inside actor methods with field mutations.
        if let Some(result) = self.try_generate_list_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: spawn, spawnWith:, await, awaitForever, await:
        if let Some(result) = self.try_handle_spawn_await(receiver, selector, arguments)? {
            return Ok(result);
        }

        // BT-374 / ADR 0010: Workspace binding dispatch + class method calls
        if let Some(result) = self.try_handle_class_reference(receiver, selector, arguments)? {
            return Ok(result);
        }

        // BT-330: Self-sends in actor methods use direct synchronous dispatch
        if let Some(result) = self.try_handle_self_dispatch(receiver, selector, arguments)? {
            return Ok(result);
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
    ) -> Result<()> {
        // Generate selector atom
        let selector_atom = selector.to_erlang_atom();
        if matches!(selector, MessageSelector::Binary(_)) {
            return Err(CodeGenError::Internal(format!(
                "unexpected binary selector in generate_message_send: {selector_atom}"
            )));
        }

        write!(self.output, "call 'beamtalk_message_dispatch':'send'(")?;
        self.generate_expression(receiver)?;
        write!(self.output, ", '{selector_atom}', [")?;

        self.generate_argument_list(arguments)?;

        write!(self.output, "])")?;

        Ok(())
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
    ) -> Result<Option<()>> {
        // Unary spawn/await messages
        if let MessageSelector::Unary(name) = selector {
            // BT-246: Only match ClassReference, not Identifier.
            if name == "spawn" && arguments.is_empty() {
                if let Expression::ClassReference { name, .. } = receiver {
                    self.generate_actor_spawn(&name.name, None)?;
                    return Ok(Some(()));
                }
            }
            if name == "await" && arguments.is_empty() {
                self.generate_await(receiver)?;
                return Ok(Some(()));
            }
            if name == "awaitForever" && arguments.is_empty() {
                self.generate_await_forever(receiver)?;
                return Ok(Some(()));
            }
        }

        // Keyword await:/spawnWith: messages
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "await:" && arguments.len() == 1 {
                self.generate_await_with_timeout(receiver, &arguments[0])?;
                return Ok(Some(()));
            }
            // BT-246: Only match ClassReference, not Identifier.
            if parts.len() == 1 && parts[0].keyword == "spawnWith:" && arguments.len() == 1 {
                if let Expression::ClassReference { name, .. } = receiver {
                    self.generate_actor_spawn(&name.name, Some(&arguments[0]))?;
                    return Ok(Some(()));
                }
            }
        }

        Ok(None)
    }

    /// Handles `ClassReference` receivers: workspace bindings and class method calls.
    ///
    /// Returns `Some(())` if the receiver is a `ClassReference`, `None` otherwise.
    fn try_handle_class_reference(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        if let Expression::ClassReference { name, .. } = receiver {
            if is_workspace_binding(&name.name) {
                if self.workspace_mode {
                    self.generate_workspace_binding_send(&name.name, selector, arguments)?;
                    return Ok(Some(()));
                }
                return Err(CodeGenError::WorkspaceBindingInBatchMode {
                    name: name.name.to_string(),
                });
            }
            self.generate_class_method_call(&name.name, selector, arguments)?;
            return Ok(Some(()));
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
    ) -> Result<Option<()>> {
        if self.context == CodeGenContext::Actor {
            if let Expression::Identifier(id) = receiver {
                if id.name == "self" {
                    self.generate_self_dispatch(selector, arguments)?;
                    return Ok(Some(()));
                }
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
    ) -> Result<()> {
        // BT-403: Sealed class optimization — skip safe_dispatch try/catch
        if self.is_class_sealed() {
            return self.generate_sealed_self_dispatch(selector, arguments);
        }

        let selector_atom = selector.to_erlang_atom();
        let result_var = self.fresh_var("SelfResult");
        let state_var = self.fresh_var("SelfState");
        let error_var = self.fresh_var("SelfError");

        let current_state = self.current_state_var();

        // Call safe_dispatch directly (synchronous, with error isolation)
        write!(
            self.output,
            "case call '{}':'safe_dispatch'('{selector_atom}', [",
            self.module_name
        )?;
        self.generate_argument_list(arguments)?;

        write!(self.output, "], {current_state}) of ")?;

        // Success: extract result value
        write!(
            self.output,
            "<{{'reply', {result_var}, {state_var}}}> when 'true' -> {result_var} "
        )?;

        // Error: re-raise for proper error propagation
        write!(
            self.output,
            "<{{'error', {error_var}, _}}> when 'true' -> call 'beamtalk_error':'raise'({error_var}) "
        )?;

        write!(self.output, "end")?;

        Ok(())
    }

    /// Generates optimized self-dispatch for sealed classes (BT-403).
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
    ) -> Result<()> {
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

        // Create Self object reference for dispatch/4
        write!(
            self.output,
            "let {self_var} = call 'beamtalk_actor':'make_self'({current_state}) in "
        )?;

        // Call dispatch/4 directly (skip safe_dispatch try/catch)
        write!(
            self.output,
            "case call '{}':'dispatch'('{selector_atom}', [",
            self.module_name
        )?;
        self.generate_argument_list(arguments)?;

        write!(self.output, "], {self_var}, {current_state}) of ")?;

        // Success: extract result value
        write!(
            self.output,
            "<{{'reply', {result_var}, _}}> when 'true' -> {result_var} "
        )?;

        // Error: re-raise for proper error propagation
        write!(
            self.output,
            "<{{'error', {error_var}, _}}> when 'true' -> call 'beamtalk_error':'raise'({error_var}) "
        )?;

        write!(self.output, "end")?;

        Ok(())
    }

    /// Generates a direct call to a sealed method's standalone function (BT-403).
    ///
    /// This is the most optimized self-dispatch path: calls `__sealed_{selector}`
    /// directly, bypassing `safe_dispatch`, dispatch, and case selector matching.
    fn generate_direct_sealed_call(
        &mut self,
        selector_name: &str,
        arguments: &[Expression],
    ) -> Result<()> {
        let result_var = self.fresh_var("SealedResult");
        let error_var = self.fresh_var("SealedError");
        let self_var = self.fresh_temp_var("SealedSelf");

        let current_state = self.current_state_var();

        // Create Self for method body access
        write!(
            self.output,
            "let {self_var} = call 'beamtalk_actor':'make_self'({current_state}) in "
        )?;

        // Direct call to sealed method function: __sealed_{selector}(args..., Self, State)
        write!(
            self.output,
            "case call '{}':'__sealed_{selector_name}'(",
            self.module_name
        )?;

        // Arguments, then Self, then State
        self.generate_argument_list(arguments)?;
        if !arguments.is_empty() {
            write!(self.output, ", ")?;
        }
        write!(self.output, "{self_var}, {current_state}) of ")?;

        // Success: extract result value
        write!(
            self.output,
            "<{{'reply', {result_var}, _}}> when 'true' -> {result_var} "
        )?;

        // Error: re-raise
        write!(
            self.output,
            "<{{'error', {error_var}, _}}> when 'true' -> call 'beamtalk_error':'raise'({error_var}) "
        )?;

        write!(self.output, "end")?;

        Ok(())
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
                        | "to:do:"
                        | "to:by:do:"
                        | "do:"
                        | "collect:"
                        | "select:"
                        | "reject:"
                        | "inject:into:"
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
    pub(super) fn generate_field_assignment_open(&mut self, expr: &Expression) -> Result<()> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");
                let current_state = self.current_state_var();

                // let _Val = <value> in
                write!(self.output, "let {val_var} = ")?;
                self.generate_expression(value)?;

                // Increment state version for the new state after assignment
                let new_state = self.next_state_var();

                // let State{n} = call 'maps':'put'('field', _Val, State{n-1}) in
                // Note: we do NOT close with the value - subsequent expressions are the body
                write!(
                    self.output,
                    " in let {new_state} = call 'maps':'put'('{}', {val_var}, {current_state}) in ",
                    field.name
                )?;

                return Ok(());
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
    ) -> Result<()> {
        // Use the domain service method for selector-to-atom conversion
        let selector_atom = selector.to_erlang_atom();
        let class_name = self.class_name();

        // Generate: call 'beamtalk_dispatch':'super'('selector', [Args], Self, State, 'ClassName')
        write!(
            self.output,
            "call 'beamtalk_dispatch':'super'('{selector_atom}', [",
        )?;
        self.generate_argument_list(arguments)?;

        write!(
            self.output,
            "], Self, {}, '{class_name}')",
            self.current_state_var()
        )?;
        Ok(())
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
    ) -> Result<()> {
        let module_name = Self::compiled_module_name(class_name);

        // Check if we're in REPL context by looking for __bindings__ in scope
        let in_repl_context = self.lookup_var("__bindings__").is_some();

        if in_repl_context {
            // REPL context - generate conditional code checking for actor registry
            // Both branches must return the same #beamtalk_object{} record type
            write!(
                self.output,
                "case call 'maps':'get'('__repl_actor_registry__', Bindings, 'undefined') of "
            )?;

            // Pattern 1: No registry (undefined) - call module:spawn() directly
            write!(self.output, "<'undefined'> when 'true' -> ")?;
            write!(self.output, "call '{module_name}':'spawn'(")?;
            if let Some(args) = init_args {
                self.generate_expression(args)?;
            }
            write!(self.output, ") ")?;

            // Pattern 2: Registry present - call Module:spawn() then register
            // Module:spawn() handles initialize protocol and returns #beamtalk_object{}.
            // We extract the Pid and register with the REPL actor registry.
            write!(self.output, "<RegistryPid> when 'true' -> ")?;
            write!(
                self.output,
                "let SpawnResult = call '{module_name}':'spawn'("
            )?;
            if let Some(args) = init_args {
                self.generate_expression(args)?;
            }
            write!(self.output, ") in ")?;

            // Extract Pid (4th element) from #beamtalk_object{class, mod, pid} tuple
            write!(
                self.output,
                "let SpawnPid = call 'erlang':'element'(4, SpawnResult) in "
            )?;
            write!(
                self.output,
                "let _RegResult = call 'beamtalk_actor':'register_spawned'(RegistryPid, SpawnPid, '{class_name}', '{module_name}') in "
            )?;
            write!(self.output, "SpawnResult ")?;

            write!(self.output, "end")?; // end outer case
        } else {
            // Non-REPL context (normal compilation) - direct module spawn call
            write!(self.output, "call '{module_name}':'spawn'(")?;
            if let Some(args) = init_args {
                self.generate_expression(args)?;
            }
            write!(self.output, ")")?;
        }

        Ok(())
    }

    /// Generates a method lookup via `>>` operator (BT-101).
    ///
    /// `Counter >> #increment` compiles to:
    /// ```erlang
    /// call 'beamtalk_object_class':'method'('Counter', 'increment')
    /// ```
    ///
    /// Returns a `CompiledMethod` map with selector, source, and arity metadata.
    fn generate_method_lookup(&mut self, class_name: &str, arguments: &[Expression]) -> Result<()> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        write!(
            self.output,
            "call 'beamtalk_object_class':'method'('{class_name}', "
        )?;
        self.generate_expression(&arguments[0])?;
        write!(self.output, ")")?;
        Ok(())
    }

    /// Generates a workspace binding message send (BT-374 / ADR 0010).
    ///
    /// Workspace bindings (`Transcript`, `Beamtalk`) are singleton actors whose
    /// `beamtalk_object` tuples are stored in `persistent_term`. The generated code:
    ///
    /// 1. Looks up binding: `persistent_term:get({beamtalk_binding, 'Name'})`
    /// 2. Extracts PID: `element(4, Binding)`
    /// 3. Creates a future: `beamtalk_future:new()`
    /// 4. Sends async message: `beamtalk_actor:async_send(Pid, Selector, Args, Future)`
    /// 5. Returns the future
    ///
    /// ```erlang
    /// let Binding = call 'persistent_term':'get'({'beamtalk_binding', 'Transcript'}) in
    /// let Pid = call 'erlang':'element'(4, Binding) in
    /// let Future = call 'beamtalk_future':'new'() in
    /// let _ = call 'beamtalk_actor':'async_send'(Pid, 'show:', [Arg], Future) in Future
    /// ```
    pub(super) fn generate_workspace_binding_send(
        &mut self,
        binding_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<()> {
        let pid_var = self.fresh_var("BindingPid");
        let future_var = self.fresh_var("Future");
        let selector_atom = selector.to_erlang_atom();

        // Look up binding object from persistent_term (beamtalk_object tuple)
        let binding_var = self.fresh_var("BindingObj");
        write!(
            self.output,
            "let {binding_var} = call 'persistent_term':'get'({{'beamtalk_binding', '{binding_name}'}}) in "
        )?;

        // Extract PID from beamtalk_object record (4th element)
        write!(
            self.output,
            "let {pid_var} = call 'erlang':'element'(4, {binding_var}) in "
        )?;

        // Create future for async result
        write!(
            self.output,
            "let {future_var} = call 'beamtalk_future':'new'() in "
        )?;

        // Send async message via beamtalk_actor:async_send
        write!(
            self.output,
            "let _ = call 'beamtalk_actor':'async_send'({pid_var}, '{selector_atom}', ["
        )?;
        self.generate_argument_list(arguments)?;

        write!(self.output, "], {future_var}) in {future_var}")?;

        Ok(())
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
    ) -> Result<()> {
        let selector_atom = selector.to_erlang_atom();
        let class_pid_var = self.fresh_var("ClassPid");

        // Look up the class process by name and dispatch via class_send/3.
        // This handles both built-in class methods (new, methods, superclass)
        // and user-defined class methods (BT-411).
        write!(
            self.output,
            "let {class_pid_var} = call 'beamtalk_object_class':'whereis_class'('{class_name}') in "
        )?;
        write!(
            self.output,
            "call 'beamtalk_object_class':'class_send'({class_pid_var}, '{selector_atom}', ["
        )?;
        self.generate_argument_list(arguments)?;

        write!(self.output, "])")?;

        Ok(())
    }
}

// NOTE: class_method_module_name and related helpers (is_primitive_stdlib_class,
// is_bt_stdlib_class, is_erlang_stdlib_module) were removed in BT-411.
// Class dispatch now goes through runtime class_send/3 instead of
// compile-time module name resolution.
