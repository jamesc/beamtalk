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
    /// - Comparison: `==`, `=` (strict), `~=` (strict inequality), `<`, `>`, `<=`, `>=`
    /// - String: `++` (concatenation via iolist_to_binary)
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
            "=" => "=:=",  // Strict equality
            "~=" => "=/=", // Strict inequality
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
                    _ => Ok(None),
                }
            }

            // No binary Integer methods handled here
            MessageSelector::Binary(_) => Ok(None),
        }
    }
}
