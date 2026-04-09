// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary operator code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::document::Document;
use super::{CodeGenError, CoreErlangGenerator, Result};
use crate::ast::Expression;
use crate::docvec;
use crate::source_analysis::Span;

impl CoreErlangGenerator {
    /// Generates code for binary operators.
    ///
    /// Maps Beamtalk binary operators to Erlang's built-in operators:
    /// - Arithmetic: `+`, `-`, `*`, `/`, `%` (rem), `**` (exponentiation)
    /// - Comparison: `==`, `=:=` (strict), `/=` (inequality), `=/=` (strict inequality), `<`, `>`, `<=`, `>=`
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

        // Special case: ** uses math:pow (no direct Erlang operator)
        if op == "**" {
            return self.generate_power_op(left, &arguments[0]);
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
            "=:=" | "=" => "=:=", // Strict equality (ADR 0002) / Legacy strict equality alias (BT-952)
            "/=" => "/=",         // Loose inequality (ADR 0002)
            "=/=" => "=/=",       // Strict inequality (ADR 0002)
            "<" => "<",
            ">" => ">",
            "<=" => "=<",
            ">=" => ">=",
            _ => {
                let right = &arguments[0];
                return Err(CodeGenError::UnsupportedFeature {
                    feature: format!("binary operator: {op}"),
                    span: Some(Span::new(left.span().start(), right.span().end())),
                });
            }
        };

        // BT-1937: A class method self-send used as a binary operand produces
        // an open let-chain that we must hoist out so the operator call ends
        // up with valid Core Erlang AND class var mutations from the operand
        // are visible to the enclosing method body.
        let (left_preamble, left_code) = self.split_subexpr_for_preamble(left)?;
        let (right_preamble, right_code) = self.split_subexpr_for_preamble(&arguments[0])?;
        let preamble = Self::combine_preambles(left_preamble, right_preamble);

        let call_doc = docvec![
            format!("call 'erlang':'{erlang_op}'("),
            left_code,
            ", ",
            right_code,
            ")",
        ];

        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "BinOp"))
    }

    /// Generates `**` exponentiation via `math:pow/2` + `erlang:round/1`.
    ///
    /// Converts both operands to float for `math:pow`, then rounds the result
    /// back to integer for consistent integer exponentiation behavior.
    ///
    /// Note: `math:pow` uses IEEE 754 floats, so very large exponents (e.g.,
    /// `2 ** 100`) may lose precision. A future improvement could use repeated
    /// multiplication for exact arbitrary-precision integer results.
    fn generate_power_op(
        &mut self,
        left: &Expression,
        right: &Expression,
    ) -> Result<Document<'static>> {
        // BT-1937: Hoist open let-chains from class method self-send operands.
        let (left_preamble, left_code) = self.split_subexpr_for_preamble(left)?;
        let (right_preamble, right_code) = self.split_subexpr_for_preamble(right)?;
        let preamble = Self::combine_preambles(left_preamble, right_preamble);
        let call_doc = docvec![
            "call 'erlang':'round'(call 'math':'pow'(call 'erlang':'float'(",
            left_code,
            "), call 'erlang':'float'(",
            right_code,
            ")))",
        ];
        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "PowRes"))
    }

    /// Generates `++` concatenation with runtime type dispatch.
    ///
    /// Lists use `erlang:'++'`, strings use `iolist_to_binary`.
    /// When the receiver type is known at compile time (literal), we emit
    /// the optimal path directly. Otherwise, a runtime `is_list` check selects.
    fn generate_concat_op(
        &mut self,
        left: &Expression,
        right: &Expression,
    ) -> Result<Document<'static>> {
        use crate::ast::Literal;

        // Compile-time optimization: detect known types from AST
        let is_list = matches!(
            left,
            Expression::ListLiteral { .. } | Expression::Literal(Literal::List(_), _)
        );
        let is_string = matches!(left, Expression::Literal(Literal::String(_), _));

        // BT-1937: Hoist open let-chains from class method self-send operands
        // so class var mutations propagate to the enclosing scope and the
        // generated Core Erlang is well-formed.
        let (left_preamble, left_code) = self.split_subexpr_for_preamble(left)?;
        let (right_preamble, right_code) = self.split_subexpr_for_preamble(right)?;
        let preamble = Self::combine_preambles(left_preamble, right_preamble);

        let call_doc = if is_list {
            // List concatenation: erlang:'++'
            docvec!["call 'erlang':'++'(", left_code, ", ", right_code, ")",]
        } else if is_string {
            // String concatenation: iolist_to_binary
            docvec![
                "call 'erlang':'iolist_to_binary'([call 'erlang':'binary_to_list'(",
                left_code,
                "), call 'erlang':'binary_to_list'(",
                right_code,
                ")])",
            ]
        } else {
            // Runtime dispatch: check is_list at runtime
            let left_var = self.fresh_temp_var("ConcatLeft");
            let right_var = self.fresh_temp_var("ConcatRight");
            docvec![
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
            ]
        };

        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "ConcatRes"))
    }
}
