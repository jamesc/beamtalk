// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! List iteration control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for list iteration constructs: `do:`, `collect:`,
//! `select:`, `reject:`, `inject:into:`, `detect:`, `anySatisfy:`,
//! `allSatisfy:`, `count:`, `flatMap:`, `takeWhile:`, `dropWhile:`,
//! `partition:`, `groupBy:`, and `sort:`.
//!
//! # Submodule organisation
//!
//! - [`basic_ops`] — `do:`, `collect:` codegen
//! - [`filter_ops`] — `select:`, `reject:` codegen
//! - [`search_ops`] — `detect:`, `anySatisfy:`, `allSatisfy:` codegen
//! - [`transform_ops`] — `inject:into:`, `flatMap:`, `count:`, `takeWhile:`,
//!   `dropWhile:`, `partition:`, `groupBy:`, `sort:` codegen

mod basic_ops;
mod filter_ops;
mod search_ops;
mod transform_ops;

#[cfg(test)]
mod tests;

use super::super::document::Document;
use super::super::{CodeGenContext, CoreErlangGenerator, Result};
use crate::ast::Expression;
use crate::docvec;

impl CoreErlangGenerator {
    pub(in crate::codegen::core_erlang) fn generate_simple_list_op(
        &mut self,
        receiver: &Expression,
        body: &Expression,
        operation: &str,
    ) -> Result<Document<'static>> {
        // BT-416: Map Erlang list operation to Beamtalk selector for runtime fallback
        let selector = match operation {
            "foreach" => "do:",
            "map" => "collect:",
            "filter" => "select:",
            _ => operation,
        };

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");

        // BT-855: When the body is a stateful block (captured mutations), wrap it so
        // Erlang receives a plain fun(Args) -> Result without the StateAcc protocol.
        // Mutations inside the block are dropped (Erlang cannot propagate NewStateAcc).
        // BT-855 follow-up: Also unwrap parenthesized block literals (e.g. `([:x | ...])`).
        let body_code = if let Some(block) = Self::extract_block_literal(body) {
            let (wrapped_doc, is_stateful) = self.generate_erlang_interop_wrapper(block)?;
            if is_stateful {
                self.warn_stateful_block_at_erlang_boundary(
                    &format!("'lists':'{operation}'"),
                    block.span,
                );
            }
            wrapped_doc
        } else {
            // BT-909: Non-literal callable — emit a runtime arity check that wraps
            // Tier-2 (arity 2) blocks to satisfy the arity-1 contract expected by
            // lists:foreach / lists:map / lists:filter.
            //
            // Generated pattern (uses is_function/2 to avoid badarg on non-funs):
            //   let _Callable = <expr> in
            //   case call 'erlang':'is_function'(_Callable, 1) of
            //     <'true'> when 'true' -> _Callable           -- arity-1, pass through
            //     <'false'> when 'true' ->
            //       case call 'erlang':'is_function'(_Callable, 2) of
            //         <'true'> when 'true' -> fun (_WArg) ->  -- Tier-2, wrap it
            //           let _T = apply _Callable (_WArg, <SeedState>) in
            //           let _WRes = call 'erlang':'element'(1, _T) in _WRes
            //         <'false'> when 'true' -> _Callable      -- not a fun, pass through
            //       end
            //   end
            //
            // NOTE 1: `let {_WRes, _} = apply ...` is invalid Core Erlang inside a fun
            // body (erlc rejects tuple patterns in let). Use element/2 calls instead.
            // NOTE 2: In Core Erlang, `fun (Params) -> Body` does NOT use `end` to
            // terminate the fun — the Body expression ends the fun. Two `end`s close
            // the two nested `case` expressions.
            //
            // SeedState is current_state_var() for Actor/Repl; for ValueType there is
            // no State in scope so ~{}~ is bound to a fresh variable first.
            self.warn_non_literal_callable_at_erlang_boundary(
                &format!("'lists':'{operation}'"),
                body.span(),
            );
            let callable_var = self.fresh_temp_var("Callable");
            let raw_code = self.expression_doc(body)?;
            let wrap_arg = self.fresh_temp_var("WArg");
            let wrap_tuple = self.fresh_temp_var("T");
            let wrap_res = self.fresh_temp_var("WRes");

            // Seed StateAcc: for value types there is no State variable in scope.
            // Bind ~{}~ to a fresh variable (mirroring generate_erlang_interop_wrapper)
            // so ~{}~ is not used as a literal in an apply argument position.
            let (state_preamble, state_var): (Document<'static>, String) =
                if matches!(self.context, CodeGenContext::ValueType) {
                    let sv = self.fresh_temp_var("EmptyState");
                    let pre = docvec!["let ", Document::String(sv.clone()), " = ~{}~ in "];
                    (pre, sv)
                } else {
                    (Document::Str(""), self.current_state_var())
                };

            // Use is_function/2 instead of fun_info to avoid exception on non-functions
            docvec![
                "let ",
                Document::String(callable_var.clone()),
                " = ",
                raw_code,
                " in case call 'erlang':'is_function'(",
                Document::String(callable_var.clone()),
                ", 1) of <'true'> when 'true' -> ",
                Document::String(callable_var.clone()),
                " <'false'> when 'true' -> ",
                "case call 'erlang':'is_function'(",
                Document::String(callable_var.clone()),
                ", 2) of <'true'> when 'true' -> fun (",
                Document::String(wrap_arg.clone()),
                ") -> ",
                state_preamble,
                "let ",
                Document::String(wrap_tuple.clone()),
                " = apply ",
                Document::String(callable_var.clone()),
                " (",
                Document::String(wrap_arg),
                ", ",
                Document::String(state_var),
                ") in let ",
                Document::String(wrap_res.clone()),
                " = call 'erlang':'element'(1, ",
                Document::String(wrap_tuple),
                ") in ",
                Document::String(wrap_res),
                " <'false'> when 'true' -> ",
                Document::String(callable_var),
                " end end",
            ]
        };

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'lists':'{operation}'({body_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, '{selector}', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1489: Generates a `let` binding that converts a list result to a
    /// binary string when the original receiver was a binary (String).
    ///
    /// Returns `(binding_code, result_var)` where `binding_code` is:
    /// ```text
    /// let <out_var> = case call 'erlang':'is_binary'(<recv_var>) of
    ///   <'true'> when 'true' -> call 'erlang':'iolist_to_binary'(<list_var>)
    ///   <'false'> when 'true' -> <list_var>
    /// end
    /// ```
    ///
    /// Note: does NOT include a trailing ` in ` — callers chain with ` in `.
    ///
    /// This allows `collect:`, `select:`, and `reject:` to return a binary
    /// string when the receiver is a String, while still returning a list
    /// when the receiver is a List.
    pub(in crate::codegen::core_erlang) fn generate_string_aware_result_binding(
        &mut self,
        recv_var: &str,
        list_var: &str,
    ) -> (Document<'static>, String) {
        let out_var = self.fresh_temp_var("StrAwareResult");
        let binding = docvec![
            "let ",
            Document::String(out_var.clone()),
            " = case call 'erlang':'is_binary'(",
            Document::String(recv_var.to_string()),
            ") of <'true'> when 'true' -> call 'erlang':'iolist_to_binary'(",
            Document::String(list_var.to_string()),
            ") <'false'> when 'true' -> ",
            Document::String(list_var.to_string()),
            " end"
        ];
        (binding, out_var)
    }
}
