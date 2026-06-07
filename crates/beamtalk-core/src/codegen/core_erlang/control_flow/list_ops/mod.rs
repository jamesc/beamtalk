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
use super::super::document::leaf;
use super::super::{CodeGenContext, CoreErlangGenerator, Result};
use crate::ast::Expression;
use crate::docvec;

/// Emits the Core Erlang preamble that binds a receiver to a guaranteed-list
/// variable (BT-524 `is_list` guard):
///
/// ```text
/// let {list_var} = {recv_code}
/// in let {safe_list_var} =
///     case call 'erlang':'is_list'({list_var})
///     of <'true'>  when 'true' -> {list_var}
///        <'false'> when 'true' -> call 'beamtalk_collection':'to_list'({list_var})
///     end
/// in
/// ```
///
/// `list_var` is consumed. Callers that still need `safe_list_var` after this
/// call should pass `safe_list_var.clone()` and retain the original.
pub(super) fn list_recv_to_safe_list_doc(
    recv_code: Document<'static>,
    list_var: String,
    safe_list_var: impl Into<String>,
) -> Document<'static> {
    docvec![
        "let ",
        leaf::var(list_var.clone()),
        " = ",
        recv_code,
        " in let ",
        leaf::var(safe_list_var),
        " = case call 'erlang':'is_list'(",
        leaf::var(list_var.clone()),
        ") of <'true'> when 'true' -> ",
        leaf::var(list_var.clone()),
        " <'false'> when 'true' -> call 'beamtalk_collection':'to_list'(",
        leaf::var(list_var),
        ") end in ",
    ]
}

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
                    let pre = docvec!["let ", leaf::var(sv.clone()), " = ~{}~ in "];
                    (pre, sv)
                } else {
                    (Document::Str(""), self.current_state_var())
                };

            // Use is_function/2 instead of fun_info to avoid exception on non-functions
            docvec![
                "let ",
                leaf::var(callable_var.clone()),
                " = ",
                raw_code,
                " in case call 'erlang':'is_function'(",
                leaf::var(callable_var.clone()),
                ", 1) of <'true'> when 'true' -> ",
                leaf::var(callable_var.clone()),
                " <'false'> when 'true' -> ",
                "case call 'erlang':'is_function'(",
                leaf::var(callable_var.clone()),
                ", 2) of <'true'> when 'true' -> fun (",
                leaf::var(wrap_arg.clone()),
                ") -> ",
                state_preamble,
                "let ",
                leaf::var(wrap_tuple.clone()),
                " = apply ",
                leaf::var(callable_var.clone()),
                " (",
                leaf::var(wrap_arg),
                ", ",
                leaf::var(state_var),
                ") in let ",
                leaf::var(wrap_res.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(wrap_tuple),
                ") in ",
                leaf::var(wrap_res),
                " <'false'> when 'true' -> ",
                leaf::var(callable_var),
                " end end",
            ]
        };

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in case call 'erlang':'is_list'(",
            leaf::var(list_var.clone()),
            ") of <'true'> when 'true' -> call 'lists':",
            leaf::atom(operation.to_string()),
            "(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_primitive':'send'(",
            leaf::var(list_var),
            ", ",
            leaf::atom(selector.to_string()),
            ", [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-1489/BT-2342: Generates a `let` binding that reconstructs a list result
    /// so its type matches the original receiver, mirroring the pure list-op path.
    ///
    /// Returns `(binding_code, result_var)` where `binding_code` is:
    /// ```text
    /// let <out_var> = call 'beamtalk_collection':'from_list_like'(<recv_var>, <list_var>)
    /// ```
    ///
    /// Note: does NOT include a trailing ` in ` — callers chain with ` in `.
    ///
    /// `beamtalk_collection:from_list_like/2` wraps the raw fold result back into:
    /// - a binary when the receiver was a String,
    /// - an `Array` when the receiver was an `Array` (BT-2342: the stateful foldl
    ///   path previously leaked a raw list where the pure path returns an Array),
    /// - the list unchanged otherwise (already-an-Erlang-list receivers).
    pub(in crate::codegen::core_erlang) fn generate_list_like_result_binding(
        &mut self,
        recv_var: &str,
        list_var: &str,
    ) -> (Document<'static>, String) {
        let out_var = self.fresh_temp_var("ListLikeResult");
        let binding = docvec![
            "let ",
            leaf::var(out_var.clone()),
            " = call 'beamtalk_collection':'from_list_like'(",
            leaf::var(recv_var.to_string()),
            ", ",
            leaf::var(list_var.to_string()),
            ")"
        ];
        (binding, out_var)
    }
}
