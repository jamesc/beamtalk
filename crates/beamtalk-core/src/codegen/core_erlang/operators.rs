// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary operator code generation.
//!
//! **DDD Context:** Compilation — Code Generation

use super::document::Document;
use super::document::leaf::{atom, var};
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

        // BT-2709: Arithmetic operators (`+ - * /`) are dispatchable messages so
        // user value-types can overload them. When the receiver is *statically*
        // known to be numeric (a numeric literal, `self` inside Integer/Float, or
        // a `:: Integer/Float/Number`-annotated parameter), we keep the bare BIF
        // fast path (zero cost — all stdlib hot arithmetic). Otherwise we emit a
        // runtime `is_number` guard that selects the BIF for numbers and falls
        // back to message dispatch for objects (so `aMoney + bMoney` reaches
        // `Money>>+` and `vector + 5` raises a DNU rather than raw `badarith`).
        // Comparison/`%`/`**` are not yet message-dispatched here (Phase 2+).
        let is_arithmetic = matches!(op, "+" | "-" | "*" | "/");
        let guard_dispatch = is_arithmetic && !self.receiver_is_statically_numeric(left);

        // BT-1937: Capture both operands in evaluation order. When either
        // operand produces an open let-chain (e.g., a class method self-send
        // mutating a class var), capture_subexpr_sequence force-hoists BOTH
        // operands into a preamble so left-to-right evaluation order is
        // preserved. When neither has an open scope, both operands stay
        // inline and there is no hoisting overhead.
        let exprs: [&Expression; 2] = [left, &arguments[0]];
        let (preamble, mut docs) = self.capture_subexpr_sequence(&exprs, "BinOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");

        let call_doc = if guard_dispatch {
            self.guarded_arithmetic_doc(op, erlang_op, left_code, right_code)
        } else {
            // CLAUDE.md: Core Erlang fragments MUST use Document/docvec!, never
            // format!(). erlang_op is one of the static literals in the match
            // arms above, so Document::Str is safe.
            docvec![
                "call 'erlang':'",
                Document::Str(erlang_op),
                "'(",
                left_code,
                ", ",
                right_code,
                ")",
            ]
        };

        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "BinOp"))
    }

    /// BT-2709: Whether `expr` is statically known to evaluate to a number, so
    /// the arithmetic fast path can skip the runtime `is_number` guard.
    ///
    /// Mirrors the gradual-typing contract: an annotation (or a syntactic fact)
    /// *removes* the guard; its absence keeps the guard, which is always correct.
    /// Recognises sources of numeric certainty already present in the
    /// AST / codegen context — no new analysis pass:
    /// * a numeric literal receiver (`1 + x`),
    /// * `self` inside a numeric class (`Integer`/`Float` method bodies),
    /// * an identifier bound to a `:: Integer/Float/Number` parameter, and
    /// * a `self.<field>` read.
    ///
    /// The last case keeps actor/value-type instance-field arithmetic on the
    /// bare numeric path exactly as before Phase 1 — both to avoid regressing
    /// the broad `self.count := self.count + 1` counter pattern (and the
    /// dependent `ifTrue:` inline-case state-threading optimisation) and because
    /// the receiver-dispatch contract this issue introduces targets
    /// local-variable receivers (`aMoney + bMoney`), not fields. Dispatching on
    /// value-type-valued fields is left to a follow-up.
    pub(in crate::codegen::core_erlang) fn receiver_is_statically_numeric(
        &self,
        expr: &Expression,
    ) -> bool {
        use crate::ast::Literal;
        match expr {
            Expression::Literal(Literal::Integer(_) | Literal::Float(_), _) => true,
            Expression::Identifier(id) => {
                if id.name == "self" {
                    matches!(self.class_name().as_str(), "Integer" | "Float")
                } else {
                    self.param_is_numeric(&id.name)
                }
            }
            // `self.<field>` reads stay on the numeric fast path (see doc above).
            Expression::FieldAccess { receiver, .. } => {
                matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
            }
            _ => false,
        }
    }

    /// BT-2709: Builds the runtime-guarded arithmetic dispatch for an
    /// unknown/generic receiver, mirroring `generate_concat_op`'s `is_list` arm:
    ///
    /// ```erlang
    /// let <Lhs> = <left> in
    ///   let <Rhs> = <right> in
    ///     case call 'erlang':'is_number'(Lhs) of
    ///       <'true'>  when 'true' -> call 'erlang':'+'(Lhs, Rhs)
    ///       <'false'> when 'true' -> call 'beamtalk_message_dispatch':'send'(Lhs, '+', [Rhs])
    ///     end
    /// ```
    ///
    /// Numbers take the bare BIF; objects route through unified message dispatch
    /// (receiver-overloaded operators, or a DNU `#beamtalk_error` when undefined).
    /// `erlang_op` is a static match-arm literal (safe as `Document::Str`); the
    /// dispatch selector uses `leaf::atom` for the original Beamtalk operator.
    fn guarded_arithmetic_doc(
        &mut self,
        op: &str,
        erlang_op: &'static str,
        left_code: Document<'static>,
        right_code: Document<'static>,
    ) -> Document<'static> {
        let left_var = self.fresh_temp_var("BinLeft");
        let right_var = self.fresh_temp_var("BinRight");
        docvec![
            "let ",
            var(left_var.clone()),
            " = ",
            left_code,
            " in let ",
            var(right_var.clone()),
            " = ",
            right_code,
            " in case call 'erlang':'is_number'(",
            var(left_var.clone()),
            ") of <'true'> when 'true' -> call 'erlang':'",
            Document::Str(erlang_op),
            "'(",
            var(left_var.clone()),
            ", ",
            var(right_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_message_dispatch':'send'(",
            var(left_var),
            ", ",
            atom(op.to_string()),
            ", [",
            var(right_var),
            "]) end",
        ]
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
        // BT-1937: Capture both operands preserving evaluation order.
        let exprs: [&Expression; 2] = [left, right];
        let (preamble, mut docs) = self.capture_subexpr_sequence(&exprs, "PowOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");
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

        // BT-1937: Capture both operands preserving evaluation order. When
        // either operand has an open scope, BOTH are force-hoisted into the
        // preamble so left-to-right evaluation order is preserved.
        let exprs: [&Expression; 2] = [left, right];
        let (preamble, mut docs) = self.capture_subexpr_sequence(&exprs, "ConcatOp")?;
        let right_code = docs.pop().expect("right operand");
        let left_code = docs.pop().expect("left operand");

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
            // Runtime dispatch: check is_list at runtime.
            // CLAUDE.md: built entirely with Document/docvec!, no format!().
            let left_var = self.fresh_temp_var("ConcatLeft");
            let right_var = self.fresh_temp_var("ConcatRight");
            docvec![
                "let ",
                var(left_var.clone()),
                " = ",
                left_code,
                " in let ",
                var(right_var.clone()),
                " = ",
                right_code,
                " in case call 'erlang':'is_list'(",
                var(left_var.clone()),
                ") of <'true'> when 'true' -> call 'erlang':'++'(",
                var(left_var.clone()),
                ", ",
                var(right_var.clone()),
                ") <'false'> when 'true' -> \
                   call 'erlang':'iolist_to_binary'([call 'erlang':'binary_to_list'(",
                var(left_var),
                "), call 'erlang':'binary_to_list'(",
                var(right_var),
                ")]) end",
            ]
        };

        Ok(self.finalize_dispatch_with_preamble(preamble, call_doc, "ConcatRes"))
    }
}
