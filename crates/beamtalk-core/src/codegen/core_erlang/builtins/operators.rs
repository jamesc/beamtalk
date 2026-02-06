// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary operator code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::super::{CodeGenError, CoreErlangGenerator, Result};
use crate::ast::Expression;
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
    pub(in crate::codegen::core_erlang) fn generate_binary_op(
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
}
