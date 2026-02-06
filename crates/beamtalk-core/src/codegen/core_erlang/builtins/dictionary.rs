// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dictionary/map method code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, MessageSelector};
use std::fmt::Write;

impl CoreErlangGenerator {
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
    pub(in crate::codegen::core_erlang) fn try_generate_dictionary_message(
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
}
