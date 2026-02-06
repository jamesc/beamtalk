// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block, list, proto-object, and object method code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, MessageSelector};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Tries to generate code for Block evaluation messages.
    ///
    /// Block evaluation messages are synchronous operations that execute blocks.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a Block method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a Block method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Block Methods
    ///
    /// - `value` (0 args) → evaluate block with no arguments
    /// - `value:` (1 arg) → evaluate block with one argument
    /// - `value:value:` (2 args) → evaluate block with two arguments  
    /// - `value:value:value:` (3 args) → evaluate block with three arguments
    /// - `repeat` (0 args) → infinite loop
    /// - `whileTrue:` (1 arg) → loop while condition block returns true
    /// - `whileFalse:` (1 arg) → loop while condition block returns false
    /// - `timesRepeat:` (1 arg) → repeat body N times
    pub(in crate::codegen::core_erlang) fn try_generate_block_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            // `value` - evaluate block with no arguments
            MessageSelector::Unary(name) if name == "value" => {
                self.generate_block_value_call(receiver, &[])?;
                Ok(Some(()))
            }

            // `repeat` - infinite loop
            MessageSelector::Unary(name) if name == "repeat" => {
                self.generate_repeat(receiver)?;
                Ok(Some(()))
            }

            // Keyword messages for block evaluation
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    // `value:`, `value:value:`, `value:value:value:` - evaluate block with args
                    "value:" | "value:value:" | "value:value:value:" => {
                        self.generate_block_value_call(receiver, arguments)?;
                        Ok(Some(()))
                    }

                    // `whileTrue:` - loop while condition block returns true
                    "whileTrue:" => {
                        self.generate_while_true(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }

                    // `whileFalse:` - loop while condition block returns false
                    "whileFalse:" => {
                        self.generate_while_false(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }

                    // `timesRepeat:` - repeat body N times
                    "timesRepeat:" => {
                        self.generate_times_repeat(receiver, &arguments[0])?;
                        Ok(Some(()))
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
    /// List methods are synchronous operations that generate direct Erlang list calls.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a List method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a List method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # List Methods
    ///
    /// - `first` (0 args) → `hd(List)`
    /// - `rest` (0 args) → `tl(List)`
    /// - `size` (0 args) → `length(List)`
    /// - `isEmpty` (0 args) → `List =:= []`
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
    ) -> Result<Option<()>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "first" if arguments.is_empty() => {
                    // call 'erlang':'hd'(Receiver)
                    write!(self.output, "call 'erlang':'hd'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "rest" if arguments.is_empty() => {
                    // call 'erlang':'tl'(Receiver)
                    write!(self.output, "call 'erlang':'tl'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "size" if arguments.is_empty() => {
                    // call 'erlang':'length'(Receiver)
                    write!(self.output, "call 'erlang':'length'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "isEmpty" if arguments.is_empty() => {
                    // call 'erlang':'=:='(Receiver, [])
                    write!(self.output, "call 'erlang':'=:='(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ", [])")?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "do:" if arguments.len() == 1 => {
                        // list do: [:item | body]
                        self.generate_list_do(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }
                    "collect:" if arguments.len() == 1 => {
                        // list collect: [:item | transform]
                        self.generate_list_collect(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }
                    "select:" if arguments.len() == 1 => {
                        // list select: [:item | predicate]
                        self.generate_list_select(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }
                    "reject:" if arguments.len() == 1 => {
                        // list reject: [:item | predicate]
                        self.generate_list_reject(receiver, &arguments[0])?;
                        Ok(Some(()))
                    }
                    "inject:into:" if arguments.len() == 2 => {
                        // list inject: 0 into: [:acc :item | acc + item]
                        self.generate_list_inject(receiver, &arguments[0], &arguments[1])?;
                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }

            MessageSelector::Binary(_) => Ok(None),
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
    ) -> Result<()> {
        // Bind receiver to a variable first for proper evaluation
        // Use fresh_temp_var to avoid shadowing user identifiers named "Fun"
        let fun_var = self.fresh_temp_var("Fun");
        write!(self.output, "let {fun_var} = ")?;
        self.generate_expression(receiver)?;
        write!(self.output, " in apply {fun_var} (")?;
        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            self.generate_expression(arg)?;
        }
        write!(self.output, ")")?;
        Ok(())
    }

    /// Tries to generate code for `ProtoObject` methods.
    ///
    /// `ProtoObject` methods are fundamental operations available on all objects:
    ///
    /// - `class` - Returns the class name (atom) of the receiver
    /// - `perform:withArguments:` - Dynamic message dispatch
    ///
    /// This function:
    /// - Returns `Ok(Some(()))` if the message was a `ProtoObject` method and code was generated
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
    /// let _ = call 'gen_server':'cast'(Pid, {Selector, Arguments, Future}) in
    /// Future
    /// ```
    pub(in crate::codegen::core_erlang) fn try_generate_protoobject_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "class" if arguments.is_empty() => {
                    // Generate: case Receiver of pattern matching for primitives + record extraction
                    let recv_var = self.fresh_temp_var("Obj");
                    let int_var = self.fresh_temp_var("I");
                    let str_var = self.fresh_temp_var("S");
                    let obj_var = self.fresh_temp_var("O");
                    let map_var = self.fresh_temp_var("M");

                    write!(self.output, "let {recv_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(
                        self.output,
                        " in case {recv_var} of \
                         <{int_var}> when call 'erlang':'is_integer'({int_var}) -> 'Integer' \
                         <{str_var}> when call 'erlang':'is_binary'({str_var}) -> 'String' \
                         <'true'> when 'true' -> 'True' \
                         <'false'> when 'true' -> 'False' \
                         <'nil'> when 'true' -> 'Nil' \
                         <{map_var}> when call 'erlang':'is_map'({map_var}) -> call 'beamtalk_primitive':'class_of'({map_var}) \
                         <{obj_var}> when 'true' -> call 'erlang':'element'(2, {obj_var}) \
                         end"
                    )?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "perform:withArguments:" if arguments.len() == 2 => {
                        // Dynamic message dispatch: receiver perform: selector withArguments: args
                        // This generates async message send via gen_server:cast
                        let receiver_var = self.fresh_var("Receiver");
                        let selector_var = self.fresh_var("Selector");
                        let args_var = self.fresh_var("Args");
                        let pid_var = self.fresh_var("Pid");
                        let future_var = self.fresh_var("Future");

                        // Bind receiver
                        write!(self.output, "let {receiver_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in ")?;

                        // Bind selector (should be an atom)
                        write!(self.output, "let {selector_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in ")?;

                        // Bind arguments (should be a list)
                        write!(self.output, "let {args_var} = ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(self.output, " in ")?;

                        // Extract pid from object record
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
                            "let _ = call 'gen_server':'cast'({pid_var}, {{{selector_var}, {args_var}, {future_var}}}) in "
                        )?;

                        // Return the future
                        write!(self.output, "{future_var}")?;

                        Ok(Some(()))
                    }
                    "perform:" if arguments.len() == 1 => {
                        // Dynamic message dispatch (zero-arity): receiver perform: selector
                        // This is shorthand for perform:withArguments: with empty args list
                        let receiver_var = self.fresh_var("Receiver");
                        let selector_var = self.fresh_var("Selector");
                        let pid_var = self.fresh_var("Pid");
                        let future_var = self.fresh_var("Future");

                        // Bind receiver
                        write!(self.output, "let {receiver_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in ")?;

                        // Bind selector (should be an atom)
                        write!(self.output, "let {selector_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in ")?;

                        // Extract pid from object record
                        write!(
                            self.output,
                            "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
                        )?;

                        // Create future
                        write!(
                            self.output,
                            "let {future_var} = call 'beamtalk_future':'new'() in "
                        )?;

                        // Send async message via gen_server:cast with empty args
                        write!(
                            self.output,
                            "let _ = call 'gen_server':'cast'({pid_var}, {{{selector_var}, [], {future_var}}}) in "
                        )?;

                        // Return the future
                        write!(self.output, "{future_var}")?;

                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Tries to generate code for Object reflection methods.
    ///
    /// Object provides reflection and introspection capabilities for all objects
    /// that inherit from it. This function handles both:
    /// - Reflection methods (respondsTo:, instVarNames, instVarAt:) for actors
    /// - Nil-testing protocol (isNil, notNil, ifNil:, ifNotNil:) for all values
    ///
    /// - Returns `Ok(Some(()))` if the message was an Object method and code was generated
    /// - Returns `Ok(None)` if the message is NOT an Object method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Object Reflection Methods (Actors only)
    ///
    /// - `respondsTo:` - Check if object responds to a selector
    /// - `instVarNames` - Get list of instance variable names
    /// - `instVarAt:` - Read instance variable by name
    ///
    /// # Nil-Testing Protocol (All values)
    ///
    /// - `isNil` - Returns true only for nil, false for everything else
    /// - `notNil` - Returns false only for nil, true for everything else
    /// - `ifNil:` - Conditional execution if nil
    /// - `ifNotNil:` - Conditional execution if not nil
    /// - `ifNil:ifNotNil:` / `ifNotNil:ifNil:` - Two-way conditional
    #[expect(
        clippy::too_many_lines,
        reason = "handles multiple Object protocol methods"
    )]
    pub(in crate::codegen::core_erlang) fn try_generate_object_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "isNil" if arguments.is_empty() => {
                    // Check if value is nil: case Receiver of 'nil' -> true; _ -> false
                    let recv_var = self.fresh_temp_var("Obj");
                    write!(self.output, "let {recv_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(
                        self.output,
                        " in case {recv_var} of <'nil'> when 'true' -> 'true' <_> when 'true' -> 'false' end"
                    )?;
                    Ok(Some(()))
                }
                "notNil" if arguments.is_empty() => {
                    // Check if value is not nil: case Receiver of 'nil' -> false; _ -> true
                    let recv_var = self.fresh_temp_var("Obj");
                    write!(self.output, "let {recv_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(
                        self.output,
                        " in case {recv_var} of <'nil'> when 'true' -> 'false' <_> when 'true' -> 'true' end"
                    )?;
                    Ok(Some(()))
                }
                "instVarNames" if arguments.is_empty() => {
                    // For actors: Extract instance variable names from state map
                    // For primitives: Intended future semantics is to return empty list
                    //                 (they have no instance vars); current implementation
                    //                 only supports actor instances (see BT-164).
                    //
                    // Generate async call since actors need mailbox serialization:
                    // gen_server:cast(Pid, {instVarNames, [], Future})

                    let receiver_var = self.fresh_var("Receiver");
                    let pid_var = self.fresh_var("Pid");
                    let future_var = self.fresh_var("Future");

                    write!(self.output, "let {receiver_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, " in ")?;

                    write!(
                        self.output,
                        "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
                    )?;

                    write!(
                        self.output,
                        "let {future_var} = call 'beamtalk_future':'new'() in "
                    )?;

                    write!(
                        self.output,
                        "let _ = call 'gen_server':'cast'({pid_var}, {{'instVarNames', [], {future_var}}}) in "
                    )?;

                    write!(self.output, "{future_var}")?;

                    Ok(Some(()))
                }
                _ => Ok(None),
            },
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "ifNil:" if arguments.len() == 1 => {
                        // case Receiver of 'nil' -> apply Block (); _ -> Receiver end
                        let recv_var = self.fresh_temp_var("Obj");
                        let block_var = self.fresh_temp_var("NilBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {block_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'nil'> when 'true' -> apply {block_var} () <_> when 'true' -> {recv_var} end"
                        )?;
                        Ok(Some(()))
                    }
                    "ifNotNil:" if arguments.len() == 1 => {
                        // case Receiver of 'nil' -> 'nil'; _ -> apply Block (Receiver) end
                        let recv_var = self.fresh_temp_var("Obj");
                        let block_var = self.fresh_temp_var("NotNilBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {block_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'nil'> when 'true' -> 'nil' <_> when 'true' -> apply {block_var} ({recv_var}) end"
                        )?;
                        Ok(Some(()))
                    }
                    "ifNil:ifNotNil:" if arguments.len() == 2 => {
                        // case Receiver of 'nil' -> apply NilBlock (); _ -> apply NotNilBlock (Receiver) end
                        let recv_var = self.fresh_temp_var("Obj");
                        let nil_var = self.fresh_temp_var("NilBlk");
                        let not_nil_var = self.fresh_temp_var("NotNilBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {nil_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in let {not_nil_var} = ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'nil'> when 'true' -> apply {nil_var} () <_> when 'true' -> apply {not_nil_var} ({recv_var}) end"
                        )?;
                        Ok(Some(()))
                    }
                    "ifNotNil:ifNil:" if arguments.len() == 2 => {
                        // Reversed order: case Receiver of 'nil' -> apply NilBlock (); _ -> apply NotNilBlock (Receiver) end
                        let recv_var = self.fresh_temp_var("Obj");
                        let not_nil_var = self.fresh_temp_var("NotNilBlk");
                        let nil_var = self.fresh_temp_var("NilBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {not_nil_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in let {nil_var} = ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'nil'> when 'true' -> apply {nil_var} () <_> when 'true' -> apply {not_nil_var} ({recv_var}) end"
                        )?;
                        Ok(Some(()))
                    }
                    "respondsTo:" if arguments.len() == 1 => {
                        // Check if object responds to a selector
                        // Use beamtalk_primitive:responds_to/2 which handles both actors and primitives
                        // This is a synchronous call that returns a boolean immediately

                        let receiver_var = self.fresh_var("Receiver");
                        let selector_var = self.fresh_var("Selector");

                        write!(self.output, "let {receiver_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in ")?;

                        write!(self.output, "let {selector_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in ")?;

                        // Call beamtalk_primitive:responds_to/2 for uniform handling
                        write!(
                            self.output,
                            "call 'beamtalk_primitive':'responds_to'({receiver_var}, {selector_var})"
                        )?;

                        Ok(Some(()))
                    }
                    "instVarAt:" if arguments.len() == 1 => {
                        // Read instance variable by name
                        // For actors: Read from state map via gen_server
                        // For primitives: Intended future behavior is to return nil
                        //                 (they have no instance vars); current
                        //                 implementation only supports actor instances
                        //                 (see BT-164).

                        let receiver_var = self.fresh_var("Receiver");
                        let name_var = self.fresh_var("Name");
                        let pid_var = self.fresh_var("Pid");
                        let future_var = self.fresh_var("Future");

                        write!(self.output, "let {receiver_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in ")?;

                        write!(self.output, "let {name_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in ")?;

                        write!(
                            self.output,
                            "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
                        )?;

                        write!(
                            self.output,
                            "let {future_var} = call 'beamtalk_future':'new'() in "
                        )?;

                        write!(
                            self.output,
                            "let _ = call 'gen_server':'cast'({pid_var}, {{'instVarAt:', [{name_var}], {future_var}}}) in "
                        )?;

                        write!(self.output, "{future_var}")?;

                        Ok(Some(()))
                    }
                    "instVarAt:put:" if arguments.len() == 2 => {
                        // Set instance variable by name
                        // For actors: Write to state map via gen_server
                        // For primitives: Intended future behavior is error
                        //                 (they have no mutable instance vars); current
                        //                 implementation only supports actor instances
                        //                 (see BT-164).

                        let receiver_var = self.fresh_var("Receiver");
                        let name_var = self.fresh_var("Name");
                        let value_var = self.fresh_var("Value");
                        let pid_var = self.fresh_var("Pid");
                        let future_var = self.fresh_var("Future");

                        write!(self.output, "let {receiver_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in ")?;

                        write!(self.output, "let {name_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in ")?;

                        write!(self.output, "let {value_var} = ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(self.output, " in ")?;

                        write!(
                            self.output,
                            "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
                        )?;

                        write!(
                            self.output,
                            "let {future_var} = call 'beamtalk_future':'new'() in "
                        )?;

                        write!(
                            self.output,
                            "let _ = call 'gen_server':'cast'({pid_var}, {{'instVarAt:put:', [{name_var}, {value_var}], {future_var}}}) in "
                        )?;

                        write!(self.output, "{future_var}")?;

                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }
            MessageSelector::Binary(_) => Ok(None),
        }
    }
}
