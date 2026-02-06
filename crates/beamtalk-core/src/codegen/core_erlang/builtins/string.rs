// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! String method code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, Literal, MessageSelector};
use std::fmt::Write;

impl CoreErlangGenerator {
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
    pub(in crate::codegen::core_erlang) fn try_generate_string_message(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<()>> {
        // Only handle methods for string LITERALS
        // For identifiers/variables, let them fall through to runtime dispatch
        let is_string_literal = matches!(receiver, Expression::Literal(Literal::String(_), _));

        match selector {
            MessageSelector::Unary(name) => match name.as_str() {
                "size" if arguments.is_empty() && is_string_literal => {
                    // BT-223: Route through beamtalk_primitive:send for runtime dispatch
                    // This handles string literals; string variables/identifiers use normal runtime dispatch
                    let recv_var = self.fresh_var("Str");
                    write!(self.output, "let {recv_var} = ")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, " in ")?;
                    write!(
                        self.output,
                        "call 'beamtalk_primitive':'send'({recv_var}, 'size', [])"
                    )?;
                    Ok(Some(()))
                }
                "length" if arguments.is_empty() && is_string_literal => {
                    // call 'string':'length'(Receiver)
                    write!(self.output, "call 'string':'length'(")?;
                    self.generate_expression(receiver)?;
                    write!(self.output, ")")?;
                    Ok(Some(()))
                }
                "isEmpty" if arguments.is_empty() && is_string_literal => {
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
}
