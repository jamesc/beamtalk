// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Integer method code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, MessageSelector};
use std::fmt::Write;

impl CoreErlangGenerator {
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
    pub(in crate::codegen::core_erlang) fn try_generate_integer_message(
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
}
