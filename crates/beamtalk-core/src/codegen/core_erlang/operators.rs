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
    /// Wraps a Core Erlang expression with `beamtalk_future:maybe_await/1`.
    ///
    /// If the operand is a future (e.g. from an actor method call), it will be
    /// awaited before use. Non-future values pass through with negligible overhead
    /// (one failed pattern match).
    fn wrap_maybe_await(doc: Document<'static>) -> Document<'static> {
        docvec!["call 'beamtalk_future':'maybe_await'(", doc, ")"]
    }

    /// Returns `true` if `expr` is provably non-future and does not need
    /// `beamtalk_future:maybe_await/1` wrapping before use in a binary operation.
    ///
    /// Conservative: only handles cases where the type is known statically from
    /// the AST shape alone, without any scope tracking.
    fn is_definitely_sync(expr: &Expression) -> bool {
        match expr {
            // All literal values (integers, floats, strings, symbols, booleans,
            // nil, atoms, lists) are immediate values — never futures.
            Expression::Literal(_, _) => true,
            // `self` refers to the current actor PID / value object — never a future.
            Expression::Identifier(id) if id.name == "self" => true,
            _ => false,
        }
    }

    /// Wraps `doc` with `beamtalk_future:maybe_await/1` only when `expr` might be
    /// a future at runtime.
    ///
    /// If `is_definitely_sync` returns `true` for `expr`, the document is returned
    /// unchanged; otherwise the standard `maybe_await` wrapper is applied.
    fn wrap_if_needed(expr: &Expression, doc: Document<'static>) -> Document<'static> {
        if Self::is_definitely_sync(expr) {
            doc
        } else {
            Self::wrap_maybe_await(doc)
        }
    }

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
                return Err(CodeGenError::UnsupportedFeature {
                    feature: format!("binary operator: {op}"),
                    location: "unknown".to_string(),
                });
            }
        };

        let left_doc = self.expression_doc(left)?;
        let right_doc = self.expression_doc(&arguments[0])?;
        let left_code = Self::wrap_if_needed(left, left_doc);
        let right_code = Self::wrap_if_needed(&arguments[0], right_doc);

        Ok(docvec![
            format!("call 'erlang':'{erlang_op}'("),
            left_code,
            ", ",
            right_code,
            ")",
        ])
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
        let left_doc = self.expression_doc(left)?;
        let right_doc = self.expression_doc(right)?;
        let left_code = Self::wrap_if_needed(left, left_doc);
        let right_code = Self::wrap_if_needed(right, right_doc);
        Ok(docvec![
            "call 'erlang':'round'(call 'math':'pow'(call 'erlang':'float'(",
            left_code,
            "), call 'erlang':'float'(",
            right_code,
            ")))",
        ])
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

        if is_list {
            // List concatenation: erlang:'++'
            let left_doc = self.expression_doc(left)?;
            let right_doc = self.expression_doc(right)?;
            let left_code = Self::wrap_if_needed(left, left_doc);
            let right_code = Self::wrap_if_needed(right, right_doc);
            Ok(docvec![
                "call 'erlang':'++'(",
                left_code,
                ", ",
                right_code,
                ")",
            ])
        } else if is_string {
            // String concatenation: iolist_to_binary
            let left_doc = self.expression_doc(left)?;
            let right_doc = self.expression_doc(right)?;
            let left_code = Self::wrap_if_needed(left, left_doc);
            let right_code = Self::wrap_if_needed(right, right_doc);
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
            let left_doc = self.expression_doc(left)?;
            let right_doc = self.expression_doc(right)?;
            let left_code = Self::wrap_if_needed(left, left_doc);
            let right_code = Self::wrap_if_needed(right, right_doc);
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
