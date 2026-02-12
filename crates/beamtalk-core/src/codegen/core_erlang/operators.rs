// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary operator code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::document::Document;
use super::{CodeGenError, CoreErlangGenerator, Result};
use crate::ast::Expression;
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates code for binary operators.
    ///
    /// Maps Beamtalk binary operators to Erlang's built-in operators:
    /// - Arithmetic: `+`, `-`, `*`, `/`, `%` (rem)
    /// - Comparison: `==`, `=` (strict), `~=` (inequality), `<`, `>`, `<=`, `>=`
    /// - Concatenation: `++` (list append via `erlang:'++'`, string via `iolist_to_binary`)
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
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(
                "binary operator must have exactly one argument".to_string(),
            ));
        }

        // Special case: ++ works on both lists and strings
        if op == "++" {
            return self.generate_concat_op(left, &arguments[0]);
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

        let left_code = self.expression_doc(left)?;
        let right_code = self.expression_doc(&arguments[0])?;

        Ok(docvec![
            format!("call 'erlang':'{erlang_op}'("),
            left_code,
            ", ",
            right_code,
            ")",
        ])
    }

    /// Generates `++` concatenation with runtime type dispatch.
    ///
    /// Lists use `erlang:'++'`, strings use `iolist_to_binary`.
    /// When the receiver type is known at compile time (literal), we emit
    /// the optimal path directly. Otherwise, a runtime `is_list` check selects.
    fn generate_concat_op(&mut self, left: &Expression, right: &Expression) -> Result<Document<'static>> {
        use crate::ast::Literal;

        // Compile-time optimization: detect known types from AST
        let is_list = matches!(
            left,
            Expression::ListLiteral { .. } | Expression::Literal(Literal::List(_), _)
        );
        let is_string = matches!(left, Expression::Literal(Literal::String(_), _));

        if is_list {
            // List concatenation: erlang:'++'
            let left_code = self.expression_doc(left)?;
            let right_code = self.expression_doc(right)?;
            Ok(docvec!["call 'erlang':'++'(", left_code, ", ", right_code, ")",])
        } else if is_string {
            // String concatenation: iolist_to_binary
            let left_code = self.expression_doc(left)?;
            let right_code = self.expression_doc(right)?;
            Ok(docvec![
                "call 'erlang':'iolist_to_binary'([call 'erlang':'binary_to_list'(",
                left_code,
                "), call 'erlang':'binary_to_list'(",
                right_code,
                ")])",
            ])
        } else {
            // Runtime dispatch: check is_list at runtime
            let left_var = self.fresh_temp_var("ConcatLeft");
            let right_var = self.fresh_temp_var("ConcatRight");
            let left_code = self.expression_doc(left)?;
            let right_code = self.expression_doc(right)?;
            Ok(docvec![
                format!("let {left_var} = "),
                left_code,
                format!(" in let {right_var} = "),
                right_code,
                format!(
                    " in case call 'erlang':'is_list'({left_var}) of \
                     <'true'> when 'true' -> call 'erlang':'++'({left_var}, {right_var}) \
                     <'false'> when 'true' -> \
                       call 'erlang':'iolist_to_binary'(\
                         [call 'erlang':'binary_to_list'({left_var}), \
                          call 'erlang':'binary_to_list'({right_var})]) \
                     end"
                ),
            ])
        }
    }
}
