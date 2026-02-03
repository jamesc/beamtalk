// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Message sending and dispatch compilation.
//!
//! This module handles the **core domain operation** of Beamtalk: message sending.
//! In Smalltalk and Beamtalk, message sending is THE fundamental operation, not
//! method calls or function invocation.
//!
//! # Message Send Protocol
//!
//! By default, messages are **asynchronous** and return futures:
//!
//! ```erlang
//! let Receiver = <receiver expression> in
//! let Pid = call 'erlang':'element'(4, Receiver) in  % Extract pid from object
//! let Future = call 'beamtalk_future':'new'() in
//! let _ = call 'gen_server':'cast'(Pid, {Selector, Args, Future}) in
//! Future
//! ```
//!
//! # Special Cases
//!
//! Several message patterns are compiled to **synchronous** operations:
//!
//! - **Binary operators**: `+`, `-`, `*`, `/` → Direct Erlang arithmetic
//! - **Block evaluation**: `value`, `whileTrue:`, `repeat` → Direct function calls
//! - **Built-in types**: String, Dictionary, Boolean, Integer, List operations
//! - **Spawn messages**: `Class spawn`, `Class spawnWith: args` → `gen_server:start_link`
//! - **Await messages**: `future await` → Blocking future resolution
//! - **Super sends**: `super methodName:` → Parent class dispatch
//!
//! # Domain Concepts
//!
//! - **Receiver**: The target object receiving the message
//! - **Selector**: The message name (unary, binary, or keyword)
//! - **Arguments**: Parameters for keyword messages
//! - **Future**: Asynchronous result container

use super::{CodeGenError, CoreErlangGenerator, Result, util::to_module_name};
use crate::ast::{Expression, MessageSelector};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates code for a message send.
    ///
    /// This is the **main entry point** for message compilation. It dispatches
    /// to specialized handlers for different message patterns, and falls back
    /// to the async protocol for user-defined messages.
    ///
    /// # Message Dispatch Strategy
    ///
    /// 1. **Super sends** → `generate_super_send`
    /// 2. **Binary operators** → `generate_binary_op` (synchronous)
    /// 3. **`ProtoObject` messages** → `try_generate_protoobject_message` (synchronous)
    /// 4. **Block messages** → `try_generate_block_message` (synchronous)
    /// 5. **String messages** → `try_generate_string_message` (synchronous)
    /// 6. **Dictionary messages** → `try_generate_dictionary_message` (synchronous)
    /// 7. **Boolean messages** → `try_generate_boolean_message` (synchronous)
    /// 8. **Integer messages** → `try_generate_integer_message` (synchronous)
    /// 9. **List messages** → `try_generate_list_message` (synchronous)
    /// 10. **Spawn messages** → Special `spawn/0` or `spawn/1` calls
    /// 11. **Await messages** → Blocking future resolution
    /// 12. **Default** → Async actor message with future
    #[expect(
        clippy::too_many_lines,
        reason = "BT-223: Runtime dispatch for primitives adds necessary complexity"
    )]
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

        // Special case: String methods - synchronous Erlang string operations
        // Check BEFORE Dictionary because both use at: but string handles literal strings
        if let Some(result) = self.try_generate_string_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: Dictionary/Map methods - direct calls to Erlang maps module
        // These are synchronous operations, not async actor messages
        if let Some(result) = self.try_generate_dictionary_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: Boolean methods - synchronous case expressions
        // ifTrue:ifFalse:, and:, or:, not generate direct Erlang case expressions
        if let Some(result) = self.try_generate_boolean_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: Integer methods - synchronous Erlang operations
        // negated, abs, isZero, isEven, isOdd generate direct arithmetic/comparison
        if let Some(result) = self.try_generate_integer_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: List/Array methods - synchronous Erlang list operations
        // do:, collect:, select:, inject:into: generate direct iteration
        if let Some(result) = self.try_generate_list_message(receiver, selector, arguments)? {
            return Ok(result);
        }

        // Special case: unary "spawn" message on a class/identifier
        // This creates a new actor instance via gen_server:start_link
        if let MessageSelector::Unary(name) = selector {
            if name == "spawn" && arguments.is_empty() {
                // Handle both ClassReference and Identifier (for backwards compat)
                match receiver {
                    Expression::ClassReference { name, .. } | Expression::Identifier(name) => {
                        return self.generate_actor_spawn(&name.name, None);
                    }
                    _ => {}
                }
            }

            // Special case: "await" is a blocking operation on a future
            if name == "await" && arguments.is_empty() {
                return self.generate_await(receiver);
            }
        }

        // Special case: "spawnWith:" keyword message on a class/identifier
        // This creates a new actor instance with initialization arguments
        if let MessageSelector::Keyword(parts) = selector {
            if parts.len() == 1 && parts[0].keyword == "spawnWith:" && arguments.len() == 1 {
                // Handle both ClassReference and Identifier (for backwards compat)
                match receiver {
                    Expression::ClassReference { name, .. } | Expression::Identifier(name) => {
                        return self.generate_actor_spawn(&name.name, Some(&arguments[0]));
                    }
                    _ => {}
                }
            }

            // Special case: "error:" raises an error with the given message
            // Compiles to: erlang:error({beamtalk_error, Message})
            if parts.len() == 1 && parts[0].keyword == "error:" && arguments.len() == 1 {
                write!(self.output, "call 'erlang':'error'(")?;
                write!(self.output, "{{'beamtalk_error', ")?;
                self.generate_expression(&arguments[0])?;
                write!(self.output, "}})")?;
                return Ok(());
            }
        }

        // BT-223: Runtime dispatch - check if receiver is actor or primitive
        //
        // For actors (beamtalk_object records): Use async dispatch with futures
        // For primitives (everything else): Use beamtalk_primitive:send/3
        //
        // This allows the same generated code to handle both cases correctly.

        let receiver_var = self.fresh_var("Receiver");
        let pid_var = self.fresh_var("Pid");
        let future_var = self.fresh_var("Future");

        // Bind receiver to a variable
        write!(self.output, "let {receiver_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in ")?;

        // Generate selector atom
        let selector_atom = selector.to_erlang_atom();
        if matches!(selector, MessageSelector::Binary(_)) {
            return Err(CodeGenError::Internal(format!(
                "unexpected binary selector in generate_message_send: {selector_atom}"
            )));
        }

        // Runtime type check: is this a beamtalk_object tuple?
        write!(self.output, "case call 'erlang':'and'(")?;
        write!(
            self.output,
            "call 'erlang':'and'(call 'erlang':'is_tuple'({receiver_var}), "
        )?;
        write!(
            self.output,
            "call 'erlang':'=='(call 'erlang':'tuple_size'({receiver_var}), 4)), "
        )?;
        write!(
            self.output,
            "call 'erlang':'=='(call 'erlang':'element'(1, {receiver_var}), 'beamtalk_object')) of "
        )?;

        // Case 1: Actor - use async dispatch with futures
        write!(self.output, "<'true'> when 'true' -> ")?;

        // Extract PID from actor record (4th element)
        write!(
            self.output,
            "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
        )?;

        // Create future
        write!(
            self.output,
            "let {future_var} = call 'beamtalk_future':'new'() in "
        )?;

        // Send async message via gen_server:cast
        write!(
            self.output,
            "let _ = call 'gen_server':'cast'({pid_var}, {{'{selector_atom}', ["
        )?;

        // Generate argument list
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
        }

        write!(self.output, "], {future_var}}}) in {future_var} ")?; // No semicolon!

        // Case 2: Primitive - use synchronous dispatch
        write!(self.output, "<'false'> when 'true' -> ")?;
        write!(
            self.output,
            "call 'beamtalk_primitive':'send'({receiver_var}, '{selector_atom}', ["
        )?;

        // Generate argument list again
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
        }

        write!(self.output, "]) end")?;

        Ok(())
    }

    /// Checks if an expression is a field assignment (`self.field := value`).
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
    /// Super calls use `beamtalk_class:super_dispatch/3` to invoke the superclass
    /// implementation. This is case #1 in the message dispatch strategy - super sends
    /// are handled before any other message type.
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
    /// call 'beamtalk_class':'super_dispatch'(State, 'increment', [])
    /// call 'beamtalk_class':'super_dispatch'(State, 'getValue', [])
    /// call 'beamtalk_class':'super_dispatch'(State, 'at:put:', [1, Value])
    /// ```
    pub(super) fn generate_super_send(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<()> {
        // Use the domain service method for selector-to-atom conversion
        let selector_atom = selector.to_erlang_atom();

        // Generate: call 'beamtalk_class':'super_dispatch'(State, 'selector', [Args])
        write!(
            self.output,
            "call 'beamtalk_class':'super_dispatch'({}, '{selector_atom}', [",
            self.current_state_var()
        )?;

        // Generate arguments
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
        }

        write!(self.output, "])")?;
        Ok(())
    }

    /// Generates code for actor spawn with conditional REPL registry integration.
    ///
    /// When spawning an actor in the REPL, check for `__repl_actor_registry__` in
    /// bindings and use `beamtalk_actor:spawn_with_registry` to register it. In
    /// non-REPL contexts (regular code, tests), fall back to normal `module:spawn`.
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
    ///     call 'counter':'spawn'()
    ///   <RegistryPid> when 'true' ->
    ///     case call 'beamtalk_actor':'spawn_with_registry'(RegistryPid, 'counter', ~{}~, 'Counter') of
    ///       <{'ok', Pid}> when 'true' ->
    ///         {'beamtalk_object', 'Counter', 'counter', Pid}
    ///       <{'error', Reason}> when 'true' ->
    ///         call 'erlang':'error'({'spawn_failed', Reason})
    ///     end
    /// end
    /// ```
    ///
    /// # Generated Code (non-REPL context)
    ///
    /// ```erlang
    /// call 'counter':'spawn'()
    /// ```
    pub(super) fn generate_actor_spawn(
        &mut self,
        class_name: &str,
        init_args: Option<&Expression>,
    ) -> Result<()> {
        let module_name = to_module_name(class_name);

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

            // Pattern 2: Registry present - call spawn_with_registry and wrap result
            write!(self.output, "<RegistryPid> when 'true' -> ")?;
            write!(
                self.output,
                "case call 'beamtalk_actor':'spawn_with_registry'(RegistryPid, '{module_name}', "
            )?;

            // Args - use empty map if no init args (consistent with spawn/0)
            if let Some(args) = init_args {
                self.generate_expression(args)?;
            } else {
                write!(self.output, "~{{}}~")?;
            }

            // Class name for display
            write!(self.output, ", '{class_name}') of ")?;

            // Wrap the {ok, Pid} result in #beamtalk_object{} record
            write!(self.output, "<{{'ok', Pid}}> when 'true' -> ")?;
            write!(
                self.output,
                "{{'beamtalk_object', '{class_name}', '{module_name}', Pid}} "
            )?;

            // Handle error case
            write!(self.output, "<{{'error', Reason}}> when 'true' -> ")?;
            write!(
                self.output,
                "call 'erlang':'error'({{'spawn_failed', Reason}}) "
            )?;

            write!(self.output, "end ")?; // end inner case
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
}
