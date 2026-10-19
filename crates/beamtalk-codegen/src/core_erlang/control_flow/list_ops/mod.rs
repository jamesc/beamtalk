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

mod basic_ops;
mod enumeration_ops;
mod filter_ops;
mod search_ops;
mod transform_ops;

#[cfg(test)]
mod tests;

use super::super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

/// The Beamtalk selector of a simple list op, from the `lists` function it
/// lowers to (`foreach` → `do:`, `map` → `collect:`, `filter` → `select:`);
/// any other name is already the selector. The one table the plain footer's
/// fallback send and the non-literal-callable wrapper's error both read.
fn list_op_selector(operation: &str) -> &str {
    match operation {
        "foreach" => "do:",
        "map" => "collect:",
        "filter" => "select:",
        other => other,
    }
}

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

    /// `sort:` comparator body (BT-3771): last expression becomes the
    /// predicate `lists:sort/2` receives. Not a fold step — `lists:sort`
    /// controls comparison order — so the comparator threads its `StateAcc`
    /// through the process dictionary instead of an accumulator: it reads
    /// `StateAcc` from `state_key_var` on entry and the epilogue writes the
    /// final `StateAcc{N}` back before answering the predicate.
    FoldlSort {
        /// The variable bound to the per-invocation process-dictionary key.
        state_key_var: String,
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
/// `guard_selector` is the collection message the user sent and the fold stands
/// in for (BT-3916, ADR 0133). `Some(selector)`: the non-list branch calls
/// `beamtalk_collection:to_list_for_threading({list_var}, '{selector}')`, the
/// receiver guard: it raises `stateful_block_dispatch` when the receiver's
/// class resolves the selector to a method that is not the stdlib's (the fold
/// would bypass it) and `does_not_understand` when it does not respond to it.
/// `None`: the plain `to_list/1`, for `do:` (the fold's own source, so never
/// guarded; this is the one place that rule lives) and for the pure inline
/// `inject:into:` fold.
///
/// `list_var` is consumed. Callers that still need `safe_list_var` after this
/// call should pass `safe_list_var.clone()` and retain the original.
pub(in crate::core_erlang) fn list_recv_to_safe_list_doc(
    recv_code: Document<'static>,
    guard_selector: Option<&str>,
    list_var: String,
    safe_list_var: impl Into<String>,
) -> Document<'static> {
    let to_list = match guard_selector {
        Some(selector) => docvec![
            "call 'beamtalk_collection':'to_list_for_threading'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::atom(selector),
            ")"
        ],
        _ => docvec![
            "call 'beamtalk_collection':'to_list'(",
            leaf::var(list_var.clone()),
            ")"
        ],
    };
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
        leaf::var(list_var),
        " <'false'> when 'true' -> ",
        to_list,
        " end in ",
    ]
}

impl CoreErlangGenerator {
    /// Returns `Some(&Block)` if `body` is a literal block that needs mutation
    /// threading via `lists:foldl`; `None` when simple dispatch is sufficient.
    pub(in crate::core_erlang) fn block_needs_mutation_threading<'a>(
        &mut self,
        body: &'a Expression,
    ) -> Option<&'a Block> {
        // The one loop/fold gate (BT-3834): the body's mutations, or a write
        // buried in a nested loop or fold (ADR 0131 Phase 2b) that the
        // enclosing construct's threaded set (`loop_threaded_locals`) and the
        // caller that unpacks its tuple already include, and only when the
        // fold has something to carry.
        match body {
            Expression::Block(body_block) if self.loop_body_has_carried_mutations(body_block) => {
                Some(body_block)
            }
            _ => None,
        }
    }

    pub(in crate::core_erlang) fn generate_simple_list_op(
        &mut self,
        receiver: &Expression,
        body: &Expression,
        operation: &str,
    ) -> Result<Document<'static>> {
        // `list_var` is allocated before the body compiles, preserving the
        // gensym order the snapshot tests pin.
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
            // Non-literal (opaque) callable: the shared lowering
            // (`non_literal_callable_doc`), in every context.
            self.warn_non_literal_callable_at_erlang_boundary(
                &format!("'lists':'{operation}'"),
                body.span(),
            );
            self.non_literal_callable_doc(body, list_op_selector(operation), 1)?
        };

        Ok(Self::generate_simple_list_op_footer(
            list_var, recv_code, body_var, body_code, operation,
        ))
    }

    /// The selector and hint of the `stateful_block_dispatch` error
    /// [`Self::non_literal_callable_doc`] raises in an actor: the self-send
    /// selector the block was passed to (the method being compiled), and the
    /// list op it forwarded the block to.
    fn forwarded_state_change_error(&self, list_selector: &str) -> (String, String) {
        match self.current_method_selector.clone() {
            Some(method) => {
                let hint = format!(
                    "The block passed to `{method}` changed actor state (a field write, or \
                     a self-send that changed state) when `{method}` forwarded it to \
                     `{list_selector}`; the change cannot be kept. Change the state in \
                     the method that owns the loop, and pass a block that returns the \
                     value instead."
                );
                (method, hint)
            }
            None => (
                list_selector.to_string(),
                format!(
                    "A block whose run changed actor state (a field write, or a self-send \
                     that changed state) was passed on to `{list_selector}`; the change \
                     cannot be kept. Change the state in the method that owns the loop, \
                     and pass a block that returns the value instead."
                ),
            ),
        }
    }

    /// Whether `callable` is a parameter the method being compiled receives
    /// as a promoted Tier 2 block (`tier2_block_params`, from the self-send
    /// pre-scan).
    fn is_promoted_tier2_param(&self, callable: &Expression) -> bool {
        matches!(
            callable.unwrap_parens(),
            Expression::Identifier(id) if self.tier2_block_params.contains(id.name.as_str())
        )
    }

    /// The callable argument of a list op that calls it with `arity`
    /// arguments: a literal block compiles as itself (the caller has already
    /// sent a mutating one down its `_with_mutations` path); anything else
    /// goes through [`Self::non_literal_callable_doc`].
    pub(in crate::core_erlang) fn list_op_callable_doc(
        &mut self,
        callable: &Expression,
        list_selector: &str,
        arity: usize,
    ) -> Result<Document<'static>> {
        if Self::extract_block_literal(callable).is_some() {
            self.expression_doc(callable)
        } else {
            self.non_literal_callable_doc(callable, list_selector, arity)
        }
    }

    /// The one lowering of a non-literal (opaque) callable handed to a list
    /// op: every callable argument of every list op in this module (`do:`,
    /// `collect:`, `select:`, `reject:`, `inject:into:`, `detect:`,
    /// `detect:ifNone:` and its `ifNone:` handler, `count:`, `anySatisfy:`,
    /// `allSatisfy:`, `flatMap:`, `takeWhile:`, `dropWhile:`, `partition:`,
    /// `groupBy:`, `sort:`), used in every context since ADR 0133 removed the
    /// ADR 0128 actor fold (BT-3908). `arity` is the number of arguments the
    /// list op calls it with (`inject:into:` and `sort:` 2, the `ifNone:`
    /// handler 0, the rest 1); `list_selector` names the list op for the
    /// error.
    ///
    /// A runtime arity check: a callable of `arity` arguments (a Tier 1
    /// block) and anything that is not a fun pass through unchanged. A
    /// Tier 2 block (`arity + 1` arguments, the trailing `StateAcc`) is
    /// wrapped to `arity` arguments. Outside an actor instance method there
    /// is no `State`: its seed is a fresh empty map and the returned map is
    /// dropped. In an actor instance method a Tier 2 block here is one
    /// `detect_tier2_self_send` promoted for a self-send to the current
    /// method: because it writes a field (BT-1140, which BT-3922 makes a
    /// compile error) or as a passthrough at a position another call site
    /// promoted (BT-3936). Its seed is the live `State`, so it reads fields
    /// and may `^`; a block whose run changed actor state (a field write, or
    /// a self-send that changed state) returns a different map, and with the
    /// fold gone that change has no way back, so it raises
    /// `stateful_block_dispatch` rather than drop it. A block that writes a
    /// captured local never gets here (the ADR 0133 escaping-block
    /// diagnostic).
    ///
    /// Arity alone cannot tell a Tier 2 block from a Tier 1 block of one
    /// argument too many (an `ifNone:` handler `[:x | ...]`, a 3-argument
    /// `sort:` block): both are funs of `arity + 1` arguments, and Tier 2
    /// funs carry no mark. So the Tier 2 arm also checks the reply shape: a
    /// reply that is not `{Result, Map}` raises `stateful_block_dispatch`
    /// (the wrong-arity error) rather than being misread. The remaining
    /// limitation: a wrong-arity Tier 1 block that happens to answer a
    /// two-tuple whose second element is a map is still read as a Tier 2
    /// reply.
    ///
    /// In an actor, only a callable that is a promoted Tier 2 parameter of
    /// the method being compiled (`tier2_block_params`) is seeded with the
    /// live `State`; every other one gets a fresh empty map, so a
    /// wrong-arity block from outside never sees the actor's state. The
    /// remaining exposure: a wrong-arity block passed at a *promoted*
    /// parameter position is applied to the live `State` before the guard
    /// raises. That lasts until BT-3922 removes the promotion.
    ///
    /// Generated pattern (`is_function/2` avoids badarg on non-funs), for
    /// `arity` 1 outside an actor:
    /// ```text
    ///   let _Callable = <expr> in
    ///   case call 'erlang':'is_function'(_Callable, 1) of
    ///     <'true'> when 'true' -> _Callable           -- Tier 1, pass through
    ///     <'false'> when 'true' ->
    ///       case call 'erlang':'is_function'(_Callable, 2) of
    ///         <'true'> when 'true' -> fun (_WArg) ->  -- Tier 2, wrap it
    ///           let _EmptyState = ~{}~ in
    ///           let _T = apply _Callable (_WArg, _EmptyState) in
    ///           case _T of
    ///             <{_WRes, _WSt}> when call 'erlang':'is_map'(_WSt) -> _WRes
    ///             <_> when 'true' -> <raise stateful_block_dispatch>
    ///           end
    ///         <'false'> when 'true' -> _Callable      -- not a fun, pass through
    ///       end
    ///   end
    /// ```
    ///
    /// NOTE 1: `let {_WRes, _} = apply ...` is invalid Core Erlang inside a fun
    /// body (erlc rejects tuple patterns in let); a `case` clause pattern is
    /// how the reply is destructured.
    /// NOTE 2: In Core Erlang, `fun (Params) -> Body` does NOT use `end` to
    /// terminate the fun — the Body expression ends the fun. Two `end`s close
    /// the two nested `case` expressions.
    pub(in crate::core_erlang) fn non_literal_callable_doc(
        &mut self,
        callable: &Expression,
        list_selector: &str,
        arity: usize,
    ) -> Result<Document<'static>> {
        self.non_literal_callable_doc_seeded(callable, list_selector, arity, None)
    }

    /// [`Self::non_literal_callable_doc`] with the actor state the Tier 2
    /// arm is seeded from (and compared against) named by the caller: the
    /// `ifNone:` handler after a `detect:ifNone:` fold runs on the fold's
    /// final state, not the state before the fold. `None` is the current
    /// field-read state.
    pub(in crate::core_erlang) fn non_literal_callable_doc_seeded(
        &mut self,
        callable: &Expression,
        list_selector: &str,
        arity: usize,
        seed_state: Option<&str>,
    ) -> Result<Document<'static>> {
        let callable_var = self.fresh_temp_var("Callable");
        let raw_code = self.expression_doc(callable)?;
        let wrap_args: Vec<String> = (0..arity).map(|_| self.fresh_temp_var("WArg")).collect();
        let wrap_tuple = self.fresh_temp_var("T");
        let wrap_res = self.fresh_temp_var("WRes");
        let wrap_state = self.fresh_temp_var("WSt");
        let actor = self.in_actor_instance_context();
        // Only a promoted Tier 2 parameter is seeded with the live actor
        // `State`; any other fun of `arity + 1` arguments (a wrong-arity block
        // from another process, say) gets a fresh empty map, so it never sees
        // the actor's private state, and the guard below still raises.
        let (seed, seed_preamble) = if actor && self.is_promoted_tier2_param(callable) {
            let seed =
                seed_state.map_or_else(|| self.current_field_read_state_var(), str::to_string);
            (seed, Document::Str(""))
        } else {
            let seed = self.fresh_temp_var("EmptyState");
            let preamble = docvec!["let ", leaf::var(seed.clone()), " = ~{}~ in "];
            (seed, preamble)
        };
        // What a Tier 2 reply `{WRes, WSt}` answers. In an actor, a state
        // change raises; elsewhere the map is dropped. The comparison keeps
        // the codegen-internal `__local__` keys on purpose: a promoted block
        // that rebinds a block-local in an inner loop returns a stale value
        // with that key leaked into its map (BT-3939, the same on main), and
        // comparing with `beamtalk_actor:strip_local_temps` would turn that
        // loud error into the stale value.
        let on_reply = if actor {
            let (error_selector, hint) = self.forwarded_state_change_error(list_selector);
            let raise =
                self.generate_stateful_block_dispatch_error(&error_selector, "Block", &hint);
            docvec![
                "case call 'erlang':'=:='(",
                leaf::var(wrap_state.clone()),
                ", ",
                leaf::var(seed.clone()),
                ") of <'true'> when 'true' -> ",
                leaf::var(wrap_res.clone()),
                " <'false'> when 'true' -> ",
                raise,
                " end",
            ]
        } else {
            leaf::var(wrap_res.clone())
        };
        // A fun of `arity + 1` arguments that does not reply `{Result, Map}`
        // is a Tier 1 block of the wrong arity, not a Tier 2 one: raise the
        // same structured error as a wrong-arity block reached by `perform:`
        // rather than misread its value.
        let wrong_arity = self.generate_stateful_block_dispatch_error(
            list_selector,
            "Block",
            super::super::intrinsics::STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let params = beamtalk_cerl_doc::join(
            wrap_args
                .iter()
                .map(|a| leaf::var(a.clone()))
                .collect::<Vec<_>>(),
            &Document::Str(", "),
        );
        let mut apply_args: Vec<Document<'static>> =
            wrap_args.iter().map(|a| leaf::var(a.clone())).collect();
        apply_args.push(leaf::var(seed));
        let tier2_arm = docvec![
            "fun (",
            params,
            ") -> ",
            seed_preamble,
            "let ",
            leaf::var(wrap_tuple.clone()),
            " = apply ",
            leaf::var(callable_var.clone()),
            " (",
            beamtalk_cerl_doc::join(apply_args, &Document::Str(", ")),
            ") in case ",
            leaf::var(wrap_tuple),
            " of <{",
            leaf::var(wrap_res),
            ", ",
            leaf::var(wrap_state.clone()),
            "}> when call 'erlang':'is_map'(",
            leaf::var(wrap_state),
            ") -> ",
            on_reply,
            " <_> when 'true' -> ",
            wrong_arity,
            " end",
        ];
        let tier1_arity = i64::try_from(arity).unwrap_or(i64::MAX);
        Ok(docvec![
            "let ",
            leaf::var(callable_var.clone()),
            " = ",
            raw_code,
            " in case call 'erlang':'is_function'(",
            leaf::var(callable_var.clone()),
            ", ",
            leaf::int_lit(tier1_arity),
            ") of <'true'> when 'true' -> ",
            leaf::var(callable_var.clone()),
            " <'false'> when 'true' -> ",
            "case call 'erlang':'is_function'(",
            leaf::var(callable_var.clone()),
            ", ",
            leaf::int_lit(tier1_arity.saturating_add(1)),
            ") of <'true'> when 'true' -> ",
            tier2_arm,
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
        let selector = list_op_selector(operation);

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
