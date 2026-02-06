// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Boolean method code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, MessageSelector};
use std::fmt::Write;

impl CoreErlangGenerator {
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
    pub(in crate::codegen::core_erlang) fn try_generate_boolean_message(
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
}
