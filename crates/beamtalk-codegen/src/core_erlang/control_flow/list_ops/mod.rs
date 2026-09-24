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
//! - [`enumeration_ops`] — `eachWithIndex:`, `do:separatedBy:` desugar codegen
//! - [`opaque_fold`] — ADR 0128 opaque-callable (non-literal block) fold
//!   threading shared by `do:`/`collect:`/`select:`/`inject:into:`/`detect:`/
//!   `detect:ifNone:`/`count:`/`anySatisfy:`/`allSatisfy:`

mod basic_ops;
mod enumeration_ops;
mod filter_ops;
mod opaque_fold;
mod search_ops;
mod transform_ops;

#[cfg(test)]
mod tests;

pub(in crate::core_erlang) use opaque_fold::OpaqueFoldOp;

use super::super::{CoreErlangGenerator, Result, block_analysis};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

// ─── BodyKind ─────────────────────────────────────────────────────────────────

/// Controls how `generate_threaded_loop_body` handles the final expression.
///
/// ADR 0111 Addendum 15: `Letrec` (the only non-`Foldl*` variant) was
/// deleted — `while_loops.rs`/`counted_loops.rs` now lower onto
/// `ThreadedStmt::ConditionalLoop` via `generate_letrec_body_ir` instead of
/// this enum's own dispatch — leaving every remaining variant a `Foldl*`
/// shape by construction, not a naming accident.
#[allow(clippy::enum_variant_names)]
pub(in crate::core_erlang) enum BodyKind {
    /// Foldl `do:` body: final accumulator is `StateAcc{N}`.
    FoldlDo,

    /// Foldl `collect:` body: final accumulator is `{[Result | AccList], StateAcc{N}}`.
    FoldlCollect,

    /// Foldl `select:`/`reject:` body: last expression becomes a predicate;
    /// a `case` expression conditionally includes the item.
    FoldlFilter {
        /// The item variable used to include in the result list.
        item_var: String,
        /// When `true`, negates the predicate (for `reject:`).
        negate: bool,
    },

    /// Foldl `inject:into:` body: final accumulator is `{NewAcc, StateAcc{N}}`.
    FoldlInject,

    /// Foldl `anySatisfy:`/`allSatisfy:` body: last expression becomes a predicate;
    /// a `case` expression updates a boolean accumulator.
    /// Accumulator is `{BoolAcc, StateAcc{N}}`.
    FoldlBoolPredicate {
        /// When `true`, semantics = `allSatisfy:` (start `true`, set `false` on failure).
        /// When `false`, semantics = `anySatisfy:` (start `false`, set `true` on match).
        is_all: bool,
    },

    /// Foldl `detect:` / `detect:ifNone:` body: last expression becomes a predicate;
    /// a `case` expression updates the found-item accumulator on first match.
    /// Accumulator is `{FoundItem, FoundFlag, StateAcc{N}}`.
    FoldlDetect {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// Foldl `count:` body: last expression becomes a predicate;
    /// a `case` expression increments the count accumulator on match.
    /// Accumulator is `{Count, StateAcc{N}}`.
    FoldlCount,

    /// Foldl `takeWhile:` body: last expression becomes a predicate;
    /// a `case` expression includes the item only while the predicate holds.
    /// Once the predicate returns false, all subsequent elements are excluded.
    /// Accumulator is `{ResultList, StillTaking, StateVars...}`.
    FoldlTakeWhile {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// Foldl `dropWhile:` body: last expression becomes a predicate;
    /// a `case` expression drops elements while the predicate holds.
    /// Once the predicate returns false, all subsequent elements are included.
    /// Accumulator is `{ResultList, StillDropping, StateVars...}`.
    FoldlDropWhile {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// Foldl `partition:` body: last expression becomes a predicate;
    /// a `case` expression routes the item to one of two lists.
    /// Accumulator is `{MatchList, NoMatchList, StateVars...}`.
    FoldlPartition {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// Foldl `groupBy:` body: last expression is the key function result;
    /// each element is grouped by its key into a map.
    /// Accumulator is `{Map, StateVars...}`.
    FoldlGroupBy {
        /// The item variable (element being iterated).
        item_var: String,
    },
}

/// Emits the Core Erlang preamble that binds a receiver to a guaranteed-list
/// variable (`is_list` guard):
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
    /// Returns `Some(&Block)` if `body` is a literal block that needs mutation
    /// threading via `lists:foldl`; `None` when simple dispatch is sufficient.
    pub(in crate::core_erlang) fn block_needs_mutation_threading<'a>(
        &mut self,
        body: &'a Expression,
    ) -> Option<&'a Block> {
        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return Some(body_block);
            }
        }
        None
    }

    /// Checks a bare (no-mutation-threading) list-op
    /// block body for a class-var-mutating self-send. Call this after
    /// `block_needs_mutation_threading` returns `None`, before falling
    /// through to a plain/BIF dispatch that compiles the block via
    /// `generate_block` (or `expression_doc`/`generate_erlang_interop_wrapper`
    /// on top of it) — that fallback has no way to thread a classState
    /// mutation back to the class method that owns it. See
    /// `check_no_unsafe_class_method_self_sends`'s doc comment for the full
    /// rationale.
    pub(in crate::core_erlang) fn check_bare_list_op_block_self_sends(
        &self,
        body: &Expression,
    ) -> Result<()> {
        if let Some(block) = Self::extract_block_literal(body) {
            let analysis = block_analysis::analyze_block(block);
            self.check_no_unsafe_class_method_self_sends(&analysis, block, block.span)?;
        }
        Ok(())
    }

    /// Whether a non-literal (opaque) callable forwarded to a collection HOM
    /// (`beamtalk-core`'s `opaque_fold_callable_arg`) should route through the
    /// ADR 0128 `lists:foldl` state-fold rewrite
    /// (`generate_opaque_callable_fold`) rather
    /// than the pre-existing plain-value wrapper
    /// (`generate_non_literal_callable_erlang_wrapper`).
    ///
    /// The single source of truth for this decision — shared by the codegen
    /// call sites (via `routes_through_opaque_callable_fold`, used by every
    /// covered operator's codegen) and the classifier call site (`control_flow_has_mutations`,
    /// `gen_server/methods.rs`) that decides whether a caller should expect
    /// a `{Result, NewState}` tuple back. These two decisions must never
    /// diverge — the class-method crash fixed on PR #4030 (an unbound
    /// `State` reference inside `class_<selector>`) arose from exactly this
    /// class of hand-duplicated condition drifting apart; extracting one
    /// predicate makes that impossible instead of merely documented.
    ///
    /// `true` only in an Actor INSTANCE method
    /// (`in_actor_instance_context`). `false` for `CodeGenContext::ValueType`
    /// (no `State` map to thread) and for a class method
    /// (`class_<selector>(ClassSelf, ClassVars, Args...)` has no
    /// `State`/`StateAcc` parameter either — class-side threading goes
    /// through `ClassVars`). BT-3615: also `false` for
    /// `CodeGenContext::Repl` — the REPL eval module unpacks a
    /// `{Result, State}` tuple only when a builder flags
    /// `repl_loop_mutated`, and does not splice ADR 0118 expression
    /// preludes, so the fold's tuple leaked into the displayed value
    /// (`#(1, 2, 3) collect: blk` answered `{#(2, 4, 6), #{...}}`) and an
    /// expression-position fold referenced an unbound `State1`. The REPL
    /// keeps the plain-value wrapper, as before BT-3583.
    pub(in crate::core_erlang) fn opaque_callable_list_op_needs_state_fold(&self) -> bool {
        self.in_actor_instance_context()
    }

    pub(in crate::core_erlang) fn generate_simple_list_op(
        &mut self,
        receiver: &Expression,
        body: &Expression,
        operation: &str,
    ) -> Result<Document<'static>> {
        // `do:`/`collect:`/`select:` all route through here — a
        // same-class mutating self-send inside a bare block has no way to
        // thread its class-var mutation back (this always runs in-process,
        // never a genuine cross-class gen_server call). See
        // `check_bare_list_op_block_self_sends`'s doc comment.
        self.check_bare_list_op_block_self_sends(body)?;

        // The Actor-instance non-literal (opaque) callable path (below) does not
        // use the shared footer at all — it has its own receiver/temp-var
        // allocation. Only compute `list_var`/`recv_code` here, ahead of
        // compiling the body, for the two paths that DO share the footer
        // (literal-block, and ValueType/REPL/class-method non-literal) —
        // preserving the exact gensym allocation order snapshot tests pin
        // (`list_var` before the body compiles), unchanged from before this
        // function was split.
        //
        // See `opaque_callable_list_op_needs_state_fold`'s doc comment for
        // why `ValueType` and `in_class_method()` are excluded — the SAME
        // predicate the classifier (`control_flow_has_mutations`,
        // `gen_server/methods.rs`) uses to decide whether a caller expects
        // a `{Result, NewState}` tuple back, so the two decisions cannot
        // drift apart (review-flagged on PR #4030).
        if self.routes_through_opaque_callable_fold(body) {
            // Actor instance: fold-based rewrite (ADR 0128 / BT-3583). The
            // callable's tier is unknown until runtime, so the fold's own
            // accumulator IS the actor `State` map itself — folding it
            // through `lists:foldl` genuinely threads it across every
            // element (unlike the old frozen-`State` wrapper this
            // replaces), and the fold lambda discriminates Tier 1 vs Tier 2
            // once PER ELEMENT rather than once at wrap time. See the
            // ADR's "Concrete lowering sketch" for the full annotated
            // Before/After.
            let op = match operation {
                "foreach" => OpaqueFoldOp::Do,
                "map" => OpaqueFoldOp::Collect,
                _ => OpaqueFoldOp::Select,
            };
            return self.generate_opaque_callable_fold(receiver, body, op);
        }

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");

        // When the body is a literal block, mutation analysis is possible
        // (`block_needs_mutation_threading`, checked by the caller before
        // falling through here) — for the pure/no-mutation case handled in
        // THIS function, wrap it so Erlang receives a plain fun(Args) ->
        // Result without the StateAcc protocol.
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
            // Non-literal (opaque) callable in `CodeGenContext::ValueType`
            // or `Repl`, OR in a class method (any context) — see
            // `opaque_callable_list_op_needs_state_fold`'s doc comment for
            // why each is excluded from the fold path.
            //
            // ADR 0128 / BT-3583: `ValueType` has no `State` map in scope,
            // and value-type method-body classification never routes a
            // non-literal do:/collect:/select: call through the
            // tuple-threading machinery this ADR adds for Actor
            // (`is_foldl_list_op_with_vt_local_threading`/
            // `foldl_list_op_body_block` only recognize a LITERAL block
            // argument) — so a `ValueType` method keeps emitting exactly
            // the pre-existing plain-value wrapper below, seeded with a
            // fresh empty map per BT-909's Tier-2 arity-check convention. A
            // stateful callable's mutations are still dropped here exactly
            // as before; this ADR does not claim to fix that (`ValueType`
            // has no channel to return them through at this call site) —
            // only that this shared function must keep producing the same,
            // already-correct plain value `ValueType` callers expect.
            self.generate_non_literal_callable_erlang_wrapper(body, operation)?
        };

        Ok(Self::generate_simple_list_op_footer(
            list_var, recv_code, body_var, body_code, operation,
        ))
    }

    /// Non-literal-callable wrapper used by `ValueType` context (and, before
    /// ADR 0128, uniformly by every context) — a runtime arity check that
    /// wraps a Tier-2 (arity 2) block to satisfy the arity-1 contract
    /// expected by `lists:foreach`/`lists:map`/`lists:filter`. Mutations
    /// inside a Tier-2 callable are dropped here: the wrapper's own
    /// `SeedState` (a fresh empty map — `ValueType` has no `State` in
    /// scope) is frozen at wrap time and never threaded back out.
    ///
    /// Generated pattern (uses `is_function/2` to avoid badarg on non-funs):
    /// ```text
    ///   let _Callable = <expr> in
    ///   case call 'erlang':'is_function'(_Callable, 1) of
    ///     <'true'> when 'true' -> _Callable           -- arity-1, pass through
    ///     <'false'> when 'true' ->
    ///       case call 'erlang':'is_function'(_Callable, 2) of
    ///         <'true'> when 'true' -> fun (_WArg) ->  -- Tier-2, wrap it
    ///           let _T = apply _Callable (_WArg, ~{}~) in
    ///           let _WRes = call 'erlang':'element'(1, _T) in _WRes
    ///         <'false'> when 'true' -> _Callable      -- not a fun, pass through
    ///       end
    ///   end
    /// ```
    ///
    /// NOTE 1: `let {_WRes, _} = apply ...` is invalid Core Erlang inside a fun
    /// body (erlc rejects tuple patterns in let). Use element/2 calls instead.
    /// NOTE 2: In Core Erlang, `fun (Params) -> Body` does NOT use `end` to
    /// terminate the fun — the Body expression ends the fun. Two `end`s close
    /// the two nested `case` expressions.
    fn generate_non_literal_callable_erlang_wrapper(
        &mut self,
        body: &Expression,
        operation: &str,
    ) -> Result<Document<'static>> {
        self.warn_non_literal_callable_at_erlang_boundary(
            &format!("'lists':'{operation}'"),
            body.span(),
        );
        let callable_var = self.fresh_temp_var("Callable");
        let raw_code = self.expression_doc(body)?;
        let wrap_arg = self.fresh_temp_var("WArg");
        let wrap_tuple = self.fresh_temp_var("T");
        let wrap_res = self.fresh_temp_var("WRes");

        // Seed StateAcc: ValueType has no State variable in scope.
        // Bind ~{}~ to a fresh variable (mirroring generate_erlang_interop_wrapper)
        // so ~{}~ is not used as a literal in an apply argument position.
        let sv = self.fresh_temp_var("EmptyState");
        let state_preamble = docvec!["let ", leaf::var(sv.clone()), " = ~{}~ in "];

        // Use is_function/2 instead of fun_info to avoid exception on non-functions
        Ok(docvec![
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
            leaf::var(sv),
            ") in let ",
            leaf::var(wrap_res.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(wrap_tuple),
            ") in ",
            leaf::var(wrap_res),
            " <'false'> when 'true' -> ",
            leaf::var(callable_var),
            " end end",
        ])
    }

    /// Common footer shared by the literal-block and `ValueType`
    /// non-literal-callable paths: guards the receiver with `is_list`,
    /// dispatching to `lists:{operation}` when it is, or falling back to an
    /// ordinary message send (for a receiver that responds to the selector
    /// itself, e.g. a user-defined collection) when it is not.
    fn generate_simple_list_op_footer(
        list_var: String,
        recv_code: Document<'static>,
        body_var: String,
        body_code: Document<'static>,
        operation: &str,
    ) -> Document<'static> {
        let selector = match operation {
            "foreach" => "do:",
            "map" => "collect:",
            "filter" => "select:",
            _ => operation,
        };

        docvec![
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
            ") <'false'> when 'true' -> call 'beamtalk_message_dispatch':'send'(",
            leaf::var(list_var),
            ", ",
            leaf::atom(selector.to_string()),
            ", [",
            leaf::var(body_var),
            "]) end",
        ]
    }

    /// Generates a `let` binding that reconstructs a list result
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
    /// - an `Array` when the receiver was an `Array` (the stateful foldl
    ///   path returns a raw list, while the pure path returns an Array),
    /// - the list unchanged otherwise (already-an-Erlang-list receivers).
    pub(in crate::core_erlang) fn generate_list_like_result_binding(
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
