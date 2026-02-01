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
//! - Message sends (unary, binary, keyword)
//! - Await expressions
//! - Cascades

use super::{CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{Block, CascadeMessage, Expression, Identifier, Literal, MapPair, MessageSelector};
use std::fmt::Write;

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
    pub(super) fn generate_literal(&mut self, lit: &Literal) -> Result<()> {
        match lit {
            Literal::Integer(n) => write!(self.output, "{n}")?,
            Literal::Float(f) => write!(self.output, "{f}")?,
            Literal::String(s) => {
                // Core Erlang binary syntax: #{segment, segment, ...}#
                // Each segment is #<value>(size, units, type, flags)
                write!(self.output, "#{{")?;
                for (i, ch) in s.chars().enumerate() {
                    if i > 0 {
                        write!(self.output, ",")?;
                    }
                    write!(
                        self.output,
                        "#<{}>(8,1,'integer',['unsigned'|['big']])",
                        ch as u32
                    )?;
                }
                write!(self.output, "}}#")?;
            }
            Literal::Symbol(s) => write!(self.output, "'{s}'")?,
            Literal::Character(c) => write!(self.output, "{}", *c as u32)?,
            Literal::Array(elements) => {
                write!(self.output, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ")?;
                    }
                    self.generate_literal(elem)?;
                }
                write!(self.output, "]")?;
            }
        }
        Ok(())
    }

    /// Generates code for an identifier reference.
    ///
    /// Handles three cases:
    /// 1. Reserved keywords (`true`, `false`, `nil`) → Core Erlang atoms
    /// 2. Bound variables (in scope) → Core Erlang variable name
    /// 3. Unbound identifiers → Field access from actor state via `maps:get/2`
    pub(super) fn generate_identifier(&mut self, id: &Identifier) -> Result<()> {
        // Handle special reserved identifiers as atoms
        match id.name.as_str() {
            "true" => write!(self.output, "'true'")?,
            "false" => write!(self.output, "'false'")?,
            "nil" => write!(self.output, "'nil'")?,
            _ => {
                // Check if it's a bound variable in current or outer scopes
                if let Some(var_name) = self.lookup_var(id.name.as_str()).cloned() {
                    write!(self.output, "{var_name}")?;
                } else {
                    // Field access from state (uses current state variable for state threading)
                    let state_var = self.current_state_var();
                    write!(self.output, "call 'maps':'get'('{}', {state_var})", id.name)?;
                }
            }
        }
        Ok(())
    }

    /// Generates code for a map literal: `~{key => value, ...}~`
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// ~{'name' => <<"Alice">>, 'age' => 30}~
    /// ```
    pub(super) fn generate_map_literal(&mut self, pairs: &[MapPair]) -> Result<()> {
        if pairs.is_empty() {
            write!(self.output, "~{{}}~")?;
            return Ok(());
        }

        write!(self.output, "~{{ ")?;

        for (i, pair) in pairs.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }

            // Generate the key
            self.generate_expression(&pair.key)?;
            write!(self.output, " => ")?;

            // Generate the value
            self.generate_expression(&pair.value)?;
        }

        write!(self.output, " }}~")?;

        Ok(())
    }

    /// Generates code for field access (e.g., `self.value`).
    ///
    /// Maps to Erlang `maps:get/2` call:
    /// ```erlang
    /// call 'maps':'get'('value', State)
    /// ```
    pub(super) fn generate_field_access(
        &mut self,
        receiver: &Expression,
        field: &Identifier,
    ) -> Result<()> {
        // For now, assume receiver is 'self' and access from State
        if let Expression::Identifier(recv_id) = receiver {
            if recv_id.name == "self" {
                let state_var = self.current_state_var();
                write!(
                    self.output,
                    "call 'maps':'get'('{}', {state_var})",
                    field.name
                )?;
                return Ok(());
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
    pub(super) fn generate_field_assignment(
        &mut self,
        field_name: &str,
        value: &Expression,
    ) -> Result<()> {
        let val_var = self.fresh_temp_var("Val");

        // Capture current state BEFORE generating value expression,
        // because the value expression may reference state (e.g., self.value + 1)
        let current_state = self.current_state_var();

        // let _Val = <value> in
        write!(self.output, "let {val_var} = ")?;
        self.generate_expression(value)?;

        // Now increment state version for the new state after assignment
        let new_state = self.next_state_var();

        // let State{n} = call 'maps':'put'('field', _Val, State{n-1}) in
        write!(
            self.output,
            " in let {new_state} = call 'maps':'put'('{field_name}', {val_var}, {current_state}) in "
        )?;

        // _Val (assignment returns the assigned value)
        write!(self.output, "{val_var}")?;

        Ok(())
    }

    /// Generates code for a block (closure).
    ///
    /// Blocks are Core Erlang funs with parameters:
    /// ```erlang
    /// fun (Param1, Param2) -> <body> end
    /// ```
    pub(super) fn generate_block(&mut self, block: &Block) -> Result<()> {
        // Push a new scope for block parameters
        self.push_scope();

        write!(self.output, "fun (")?;
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ")?;
            }
            let var_name = self.fresh_var(&param.name);
            write!(self.output, "{var_name}")?;
        }
        write!(self.output, ") -> ")?;
        self.generate_block_body(block)?;

        // Pop the scope when done with the block
        self.pop_scope();
        Ok(())
    }

    /// Generates await expression.
    ///
    /// Delegates to `beamtalk_future:await/1` which blocks until the future
    /// is resolved or rejected:
    /// ```erlang
    /// call 'beamtalk_future':'await'(Future)
    /// ```
    pub(super) fn generate_await(&mut self, future: &Expression) -> Result<()> {
        // Delegate to beamtalk_future:await/1, which blocks until resolution/rejection
        write!(self.output, "call 'beamtalk_future':'await'(")?;
        self.generate_expression(future)?;
        write!(self.output, ")")?;
        Ok(())
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
    #[expect(
        clippy::too_many_lines,
        reason = "cascade codegen handles both normal and fallback paths"
    )]
    pub(super) fn generate_cascade(
        &mut self,
        receiver: &Expression,
        messages: &[CascadeMessage],
    ) -> Result<()> {
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
            // Bind the underlying receiver once
            let receiver_var = self.fresh_temp_var("Receiver");
            write!(self.output, "let {receiver_var} = ")?;
            self.generate_expression(underlying_receiver)?;
            write!(self.output, " in ")?;

            // Extract pid from #beamtalk_object{} record (4th element, 1-indexed)
            let pid_var = self.fresh_temp_var("Pid");
            write!(
                self.output,
                "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
            )?;

            // Total number of messages in the cascade: first + remaining
            let total_messages = messages.len() + 1;

            for index in 0..total_messages {
                let is_last = index == total_messages - 1;

                if !is_last {
                    // For all but the last message, discard the result
                    write!(self.output, "let _ = ")?;
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

                // Generate async message send to the receiver pid variable
                let future_var = self.fresh_var("Future");
                write!(
                    self.output,
                    "let {future_var} = call 'beamtalk_future':'new'() in "
                )?;
                write!(
                    self.output,
                    "let _ = call 'gen_server':'cast'({pid_var}, {{"
                )?;

                // Selector
                write!(self.output, "'")?;
                match selector {
                    MessageSelector::Unary(name) => write!(self.output, "{name}")?,
                    MessageSelector::Keyword(parts) => {
                        let name = parts.iter().map(|p| p.keyword.as_str()).collect::<String>();
                        write!(self.output, "{name}")?;
                    }
                    MessageSelector::Binary(_) => {
                        return Err(CodeGenError::UnsupportedFeature {
                            feature: "binary selectors in cascades".to_string(),
                            location: "cascade message with binary selector".to_string(),
                        });
                    }
                }
                write!(self.output, "', [")?;

                // Arguments
                for (j, arg) in arguments.iter().enumerate() {
                    if j > 0 {
                        write!(self.output, ", ")?;
                    }
                    self.generate_expression(arg)?;
                }

                write!(self.output, "], {future_var}}}) in {future_var}")?;

                if !is_last {
                    write!(self.output, " in ")?;
                }
            }

            Ok(())
        } else {
            // Fallback: if the receiver is not a MessageSend (which should not
            // happen for well-formed cascades), preserve the previous behavior:
            // evaluate the receiver once and send all cascade messages to it.
            let receiver_var = self.fresh_temp_var("Receiver");
            write!(self.output, "let {receiver_var} = ")?;
            self.generate_expression(receiver)?;
            write!(self.output, " in ")?;

            // Extract pid from #beamtalk_object{} record (4th element, 1-indexed)
            let pid_var = self.fresh_temp_var("Pid");
            write!(
                self.output,
                "let {pid_var} = call 'erlang':'element'(4, {receiver_var}) in "
            )?;

            // Generate each message send, discarding intermediate results
            for (i, message) in messages.iter().enumerate() {
                let is_last = i == messages.len() - 1;

                if !is_last {
                    // For all but the last message, discard the result
                    write!(self.output, "let _ = ")?;
                }

                // Generate async message send to the receiver pid variable
                let future_var = self.fresh_var("Future");
                write!(
                    self.output,
                    "let {future_var} = call 'beamtalk_future':'new'() in "
                )?;
                write!(
                    self.output,
                    "let _ = call 'gen_server':'cast'({pid_var}, {{"
                )?;

                // Selector
                write!(self.output, "'")?;
                match &message.selector {
                    MessageSelector::Unary(name) => write!(self.output, "{name}")?,
                    MessageSelector::Keyword(parts) => {
                        let name = parts.iter().map(|p| p.keyword.as_str()).collect::<String>();
                        write!(self.output, "{name}")?;
                    }
                    MessageSelector::Binary(_) => {
                        return Err(CodeGenError::UnsupportedFeature {
                            feature: "binary selectors in cascades".to_string(),
                            location: "cascade message with binary selector".to_string(),
                        });
                    }
                }
                write!(self.output, "', [")?;

                // Arguments
                for (j, arg) in message.arguments.iter().enumerate() {
                    if j > 0 {
                        write!(self.output, ", ")?;
                    }
                    self.generate_expression(arg)?;
                }

                write!(self.output, "], {future_var}}}) in {future_var}")?;

                if !is_last {
                    write!(self.output, " in ")?;
                }
            }

            Ok(())
        }
    }
}
