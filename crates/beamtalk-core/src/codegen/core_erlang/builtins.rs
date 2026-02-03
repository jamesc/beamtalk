// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Built-in operation code generation.
//!
//! This module handles code generation for built-in operations like:
//! - Block evaluation (`value`, `whileTrue:`, `whileFalse:`, `repeat`)
//! - Dictionary operations (`at:`, `at:put:`, etc.)
//! - Boolean operations (`ifTrue:`, `ifFalse:`, etc.)
//! - Integer arithmetic (`+`, `-`, `*`, `/`, etc.)
//! - String operations (`++`, `size`, etc.)
//! - Binary operators with standard math precedence

use super::{CodeGenError, CoreErlangGenerator, Result};
use crate::ast::{Expression, MessageSelector};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates code for binary operators.
    ///
    /// Maps Beamtalk binary operators to Erlang's built-in operators:
    /// - Arithmetic: `+`, `-`, `*`, `/`, `%` (rem)
    /// - Comparison: `==`, `=` (strict), `~=` (inequality), `<`, `>`, `<=`, `>=`
    /// - String: `++` (concatenation via `iolist_to_binary`)
    ///
    /// # Arguments
    ///
    /// * `op` - The binary operator symbol
    /// * `left` - The left operand expression
    /// * `arguments` - Array containing the right operand (must have exactly one element)
    ///
    /// # Errors
    ///
    /// Returns error if arguments length is not exactly 1 or operator is unsupported.
    pub(super) fn generate_binary_op(
        &mut self,
        op: &str,
        left: &Expression,
        arguments: &[Expression],
    ) -> Result<()> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(
                "binary operator must have exactly one argument".to_string(),
            ));
        }

        // Special case: string concatenation uses iolist_to_binary
        if op == "++" {
            write!(
                self.output,
                "call 'erlang':'iolist_to_binary'([call 'erlang':'binary_to_list'("
            )?;
            self.generate_expression(left)?;
            write!(self.output, "), call 'erlang':'binary_to_list'(")?;
            self.generate_expression(&arguments[0])?;
            write!(self.output, ")])")?;
            return Ok(());
        }

        let erlang_op = match op {
            "+" => "+",
            "-" => "-",
            "*" => "*",
            "/" => "/",
            "%" => "rem",
            "==" => "==",
            "=" => "=:=", // Strict equality
            "~=" => "/=", // Inequality (negation of ==)
            "<" => "<",
            ">" => ">",
            "<=" => "=<",
            ">=" => ">=",
            _ => {
                return Err(CodeGenError::UnsupportedFeature {
                    feature: format!("binary operator: {op}"),
                    location: "unknown".to_string(),
                });
            }
        };

        write!(self.output, "call 'erlang':'{erlang_op}'(")?;
        self.generate_expression(left)?;
        write!(self.output, ", ")?;
        self.generate_expression(&arguments[0])?;
        write!(self.output, ")")?;

        Ok(())
    }

    /// Tries to generate code for Integer methods.
    ///
    /// Integer methods are synchronous operations that generate direct Erlang calls.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was an Integer method and code was generated
    /// - Returns `Ok(None)` if the message is NOT an Integer method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Integer Methods
    ///
    /// - `negated` (0 args) → `-Receiver`
    /// - `abs` (0 args) → absolute value
    /// - `isZero` (0 args) → `Receiver =:= 0`
    /// - `isEven` (0 args) → `Receiver rem 2 =:= 0`
    /// - `isOdd` (0 args) → `Receiver rem 2 =/= 0`
    /// - `to:do:` (2 args) → iteration from start to end with body block
    /// - `to:by:do:` (3 args) → iteration from start to end with custom step
    pub(super) fn try_generate_integer_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "negated" if arguments.is_empty() => {
                    // call 'erlang':'-'(0, Receiver)
                    write!(self.output, "call 'erlang':'-'(0, ")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "abs" if arguments.is_empty() => {
                    // case call 'erlang':'<'(N, 0) of 'true' -> call 'erlang':'-'(0, N); 'false' -> N end
                    let n_var = self.fresh_temp_var("N");
                    write!(self.output, "let {n_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(
                        self.output,
                        " in case call 'erlang':'<'({n_var}, 0) of <'true'> when 'true' -> call 'erlang':'-'(0, {n_var}) <'false'> when 'true' -> {n_var} end"
                    )?;
                    Ok(Some(()))
                }
                "isZero" if arguments.is_empty() => {
                    // call 'erlang':'=:='(Receiver, 0)
                    write!(self.output, "call 'erlang':'=:='(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ", 0)")?;
                    Ok(Some(()))
                }
                "isEven" if arguments.is_empty() => {
                    // call 'erlang':'=:='(call 'erlang':'rem'(Receiver, 2), 0)
                    write!(self.output, "call 'erlang':'=:='(call 'erlang':'rem'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ", 2), 0)")?;
                    Ok(Some(()))
                }
                "isOdd" if arguments.is_empty() => {
                    // call 'erlang':'=/='(call 'erlang':'rem'(Receiver, 2), 0)
                    write!(self.output, "call 'erlang':'=/='(call 'erlang':'rem'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ", 2), 0)")?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "to:do:" if arguments.len() == 2 => {
                        // 1 to: 10 do: [:n | body]
                        // receiver = start, arguments[0] = end, arguments[1] = body block
                        self.generate_to_do(receiver, &arguments[0], &arguments[1])?;
                        Ok(Some(()))
                    }
                    "to:by:do:" if arguments.len() == 3 => {
                        // 1 to: 10 by: 2 do: [:n | body]
                        // receiver = start, arguments[0] = end, arguments[1] = step, arguments[2] = body block
                        self.generate_to_by_do(
                            receiver,
                            &arguments[0],
                            &arguments[1],
                            &arguments[2],
                        )?;
                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }

            // No binary Integer methods handled here
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Tries to generate code for Boolean methods.
    ///
    /// Boolean methods are synchronous operations that generate case expressions.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a Boolean method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a Boolean method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Boolean Methods
    ///
    /// - `not` (0 args) → `case Bool of true -> false; false -> true end`
    /// - `ifTrue:ifFalse:` (2 args) → conditional with both branches
    /// - `ifTrue:` (1 arg) → conditional with true branch only
    /// - `ifFalse:` (1 arg) → conditional with false branch only
    /// - `and:` (1 arg) → short-circuit logical AND (block is evaluated lazily)
    /// - `or:` (1 arg) → short-circuit logical OR (block is evaluated lazily)
    pub(super) fn try_generate_boolean_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            // Unary messages
            MessageSelector::Unary(name) => match name.as_str() {
                "not" if arguments.is_empty() => {
                    // case Receiver of 'true' -> 'false'; 'false' -> 'true' end
                    let recv_var = self.fresh_temp_var("Bool");
                    write!(self.output, "let {recv_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(
                        self.output,
                        " in case {recv_var} of <'true'> when 'true' -> 'false' <'false'> when 'true' -> 'true' end"
                    )?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            // Keyword messages
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "ifTrue:ifFalse:" if arguments.len() == 2 => {
                        // case Receiver of 'true' -> apply TrueBlock (); 'false' -> apply FalseBlock () end
                        let recv_var = self.fresh_temp_var("Bool");
                        let true_var = self.fresh_temp_var("TrueBlk");
                        let false_var = self.fresh_temp_var("FalseBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {true_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, " in let {false_var} = ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> apply {true_var} () <'false'> when 'true' -> apply {false_var} () end"
                        )?;
                        Ok(Some(()))
                    }
                    "ifTrue:" if arguments.len() == 1 => {
                        // case Receiver of 'true' -> apply TrueBlock (); 'false' -> Receiver end
                        let recv_var = self.fresh_temp_var("Bool");
                        let true_var = self.fresh_temp_var("TrueBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {true_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> apply {true_var} () <'false'> when 'true' -> {recv_var} end"
                        )?;
                        Ok(Some(()))
                    }
                    "ifFalse:" if arguments.len() == 1 => {
                        // case Receiver of 'true' -> Receiver; 'false' -> apply FalseBlock () end
                        let recv_var = self.fresh_temp_var("Bool");
                        let false_var = self.fresh_temp_var("FalseBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {false_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> {recv_var} <'false'> when 'true' -> apply {false_var} () end"
                        )?;
                        Ok(Some(()))
                    }
                    "and:" if arguments.len() == 1 => {
                        // case Receiver of 'true' -> apply Block (); 'false' -> 'false' end
                        let recv_var = self.fresh_temp_var("Bool");
                        let block_var = self.fresh_temp_var("AndBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {block_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> apply {block_var} () <'false'> when 'true' -> 'false' end"
                        )?;
                        Ok(Some(()))
                    }
                    "or:" if arguments.len() == 1 => {
                        // case Receiver of 'true' -> 'true'; 'false' -> apply Block () end
                        let recv_var = self.fresh_temp_var("Bool");
                        let block_var = self.fresh_temp_var("OrBlk");
                        write!(self.output, "let {recv_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in let {block_var} = ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(
                            self.output,
                            " in case {recv_var} of <'true'> when 'true' -> 'true' <'false'> when 'true' -> apply {block_var} () end"
                        )?;
                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }

            // Binary selectors - not used for Boolean methods
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Tries to generate code for Dictionary/Map methods.
    ///
    /// Dictionary methods are synchronous operations that generate direct Erlang `maps` module calls.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a Dictionary method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a Dictionary method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Dictionary Methods
    ///
    /// - `keys` (0 args) → `maps:keys(Map)`
    /// - `values` (0 args) → `maps:values(Map)`
    /// - `size` (0 args) → `maps:size(Map)`
    /// - `at:` (1 arg) → `maps:get(Key, Map)`
    /// - `at:ifAbsent:` (2 args) → `case maps:find(Key, Map) of {ok, V} -> V; error -> Block() end`
    /// - `at:put:` (2 args) → `maps:put(Key, Value, Map)`
    /// - `includesKey:` (1 arg) → `maps:is_key(Key, Map)`
    /// - `removeKey:` (1 arg) → `maps:remove(Key, Map)`
    /// - `merge:` (1 arg) → `maps:merge(Map1, Map2)`
    /// - `keysAndValuesDo:` (1 arg) → `maps:foreach(Fun, Map)`
    pub(super) fn try_generate_dictionary_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            // Unary messages
            MessageSelector::Unary(name) => match name.as_str() {
                "keys" if arguments.is_empty() => {
                    write!(self.output, "call 'maps':'keys'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "values" if arguments.is_empty() => {
                    write!(self.output, "call 'maps':'values'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "size" if arguments.is_empty() => {
                    write!(self.output, "call 'maps':'size'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            // Keyword messages
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();

                match selector_name.as_str() {
                    "at:" if arguments.len() == 1 => {
                        // maps:get(Key, Map)
                        write!(self.output, "call 'maps':'get'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "at:ifAbsent:" if arguments.len() == 2 => {
                        // case maps:find(Key, Map) of {ok, V} -> V; error -> Block() end
                        let result_var = self.fresh_var("FindResult");
                        write!(self.output, "case call 'maps':'find'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ") of ")?;
                        write!(
                            self.output,
                            "<{{'ok', {result_var}}}> when 'true' -> {result_var}"
                        )?;
                        write!(self.output, "; <'error'> when 'true' -> apply ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(self.output, " () end")?;
                        Ok(Some(()))
                    }
                    "at:put:" if arguments.len() == 2 => {
                        // maps:put(Key, Value, Map)
                        write!(self.output, "call 'maps':'put'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(&arguments[1])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "includesKey:" if arguments.len() == 1 => {
                        // maps:is_key(Key, Map)
                        write!(self.output, "call 'maps':'is_key'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "removeKey:" if arguments.len() == 1 => {
                        // maps:remove(Key, Map)
                        write!(self.output, "call 'maps':'remove'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "merge:" if arguments.len() == 1 => {
                        // maps:merge(Map1, Map2)
                        write!(self.output, "call 'maps':'merge'(")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ", ")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    "keysAndValuesDo:" if arguments.len() == 1 => {
                        // maps:foreach(Fun, Map)
                        write!(self.output, "call 'maps':'foreach'(")?;
                        self.generate_expression(&arguments[0])?;
                        write!(self.output, ", ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, ")")?;
                        Ok(Some(()))
                    }
                    _ => Ok(None),
                }
            }

            // Binary selectors - not used for Dictionary methods
            MessageSelector::Binary(_) => Ok(None),
        }
    }

    /// Tries to generate code for String methods.
    ///
    /// String methods are synchronous operations that generate direct Erlang `string` module calls.
    /// This function:
    ///
    /// - Returns `Ok(Some(()))` if the message was a String method and code was generated
    /// - Returns `Ok(None)` if the message is NOT a String method (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # String Methods
    ///
    /// - `length` (0 args) → `string:length(String)`
    /// - `isEmpty` (0 args) → `string:length(String) =:= 0`
    pub(super) fn try_generate_string_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "length" if arguments.is_empty() => {
                    // call 'string':'length'(Receiver)
                    write!(self.output, "call 'string':'length'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "isEmpty" if arguments.is_empty() => {
                    // call 'erlang':'=:='(call 'string':'length'(Receiver), 0)
                    // Using string:length == 0 because Core Erlang empty binary syntax is complex
                    write!(self.output, "call 'erlang':'=:='(call 'string':'length'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, "), 0)")?;
                    Ok(Some(()))
                }
                _ => Ok(None),
            },

            // No keyword or binary String methods handled here
            _ => Ok(None),
        }
    }

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
    pub(super) fn try_generate_block_message(
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
    pub(super) fn try_generate_list_message(
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
    pub(super) fn generate_block_value_call(
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
    pub(super) fn try_generate_protoobject_message(
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
    pub(super) fn try_generate_object_message(
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
                        // For actors: Query method table via gen_server
                        // For primitives: Planned to check primitive's method table
                        //                 (not yet implemented - see BT-163)

                        let receiver_var = self.fresh_var("Receiver");
                        let selector_var = self.fresh_var("Selector");
                        let pid_var = self.fresh_var("Pid");
                        let future_var = self.fresh_var("Future");

                        write!(self.output, "let {receiver_var} = ")?;
                        self.generate_expression(receiver)?;
                        write!(self.output, " in ")?;

                        write!(self.output, "let {selector_var} = ")?;
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
                            "let _ = call 'gen_server':'cast'({pid_var}, {{'respondsTo', [{selector_var}], {future_var}}}) in "
                        )?;

                        write!(self.output, "{future_var}")?;

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
                            "let _ = call 'gen_server':'cast'({pid_var}, {{'instVarAt', [{name_var}], {future_var}}}) in "
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
