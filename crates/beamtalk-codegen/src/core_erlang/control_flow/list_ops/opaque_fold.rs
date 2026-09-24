// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0128 opaque-callable fold threading for collection HOMs.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! When a collection HOM (`do:`, `collect:`, `select:`, `inject:into:`,
//! `detect:`, `detect:ifNone:`, `count:`, `anySatisfy:`, `allSatisfy:`)
//! receives a *non-literal* (opaque) callable in an Actor instance method, the
//! callable's tier is unknown until runtime: it may be a plain Tier 1
//! `fun (Args...) -> Result`, or a Tier 2 stateful block
//! `fun (Args..., StateAcc) -> {Result, NewStateAcc}` whose captured-local
//! writes live in the `StateAcc` map it is handed. This module lowers every
//! such call site onto ONE `lists:foldl`-driven loop whose accumulator
//! threads the actor `State` map (plus an operator-specific slot —
//! result list, fold accumulator, count, found item, boolean), and
//! discriminates the callable's tier once PER ELEMENT via
//! `erlang:is_function/2` (`opaque_callable_apply_doc`).
//!
//! The whole construct evaluates to a raw `{Result, NewState}` tuple, unpacked
//! by the caller's generic `BodyExprKind::ControlFlowWithMutations`
//! machinery (`gen_server/methods.rs`) at statement level, or — in any other
//! expression position (a receiver, argument, operand, conditional
//! receiver) — turned into a real `State` `Bind` prelude by ADR 0118's
//! `inline_control_flow_producer` (`opaque_callable_fold_needs_threading`).
//! The classifier, the expression-position producer and every codegen call
//! site share `beamtalk-core`'s `opaque_fold_callable_arg` table and
//! `routes_through_opaque_callable_fold`, so "is this a `{Result, NewState}`
//! tuple?" and "did codegen emit one?" cannot disagree. The same table feeds
//! the block-analysis fact `has_opaque_callable_hom_send`, so a conditional
//! or loop body containing such a send threads `State` through it too.
//!
//! Short-circuiting operators (`detect:`, `detect:ifNone:`, `anySatisfy:`,
//! `allSatisfy:`) keep their short-circuit *call* semantics: once the answer
//! is decided, the fold passes the accumulator through unchanged without
//! invoking the callable for the remaining elements.

use super::super::super::threaded_ir::StateAccFallbackReason;
use super::super::super::{CoreErlangGenerator, Result};
use super::list_recv_to_safe_list_doc;
use super::search_ops::bind_detect_found_or_raise_doc;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::Expression;

/// The collection HOM being lowered through the ADR 0128 opaque-callable fold.
#[derive(Clone, Copy)]
pub(in crate::core_erlang) enum OpaqueFoldOp<'a> {
    /// `do:` — result `nil`.
    Do,
    /// `collect:` — result is the mapped collection (receiver-shaped).
    Collect,
    /// `select:` — result is the filtered collection (receiver-shaped).
    Select,
    /// `inject:into:` — callable takes `(Acc, Each)`; result is the final `Acc`.
    Inject { initial: &'a Expression },
    /// `detect:` — result is the first match; raises `not_found` otherwise.
    Detect,
    /// `detect:ifNone:` — result is the first match, else `if_none value`.
    DetectIfNone { if_none: &'a Expression },
    /// `count:` — result is the number of elements the predicate accepts.
    Count,
    /// `anySatisfy:` (`is_all == false`) / `allSatisfy:` (`is_all == true`).
    Satisfy { is_all: bool },
}

/// Type of the fold-accumulator continuation: `k(Result, NewStAcc)`.
type AccContinuation = dyn Fn(Document<'static>, Document<'static>) -> Document<'static>;

impl CoreErlangGenerator {
    /// Whether `callable` — the block-position argument of a collection HOM
    /// covered by `beamtalk-core`'s `opaque_fold_callable_arg` table — routes
    /// through the ADR 0128
    /// opaque-callable fold (`generate_opaque_callable_fold`), producing a raw
    /// `{Result, NewState}` tuple instead of a plain value.
    ///
    /// True exactly when `callable` is not a literal block AND the current
    /// context has a `State` map to thread
    /// (`opaque_callable_list_op_needs_state_fold`).
    pub(in crate::core_erlang) fn routes_through_opaque_callable_fold(
        &self,
        callable: &Expression,
    ) -> bool {
        Self::extract_block_literal(callable).is_none()
            && self.opaque_callable_list_op_needs_state_fold()
    }

    /// Whether `expr` (paren-unwrapped here) is a whole collection-HOM send
    /// that compiles through the ADR 0128 opaque-callable fold — and so
    /// evaluates to a raw `{Result, NewState}` tuple wherever it appears.
    ///
    /// The syntactic half is `beamtalk-core`'s `is_opaque_callable_hom_send`
    /// (a bare-`self` receiver is excluded there: semantic analysis
    /// classifies it as a `SelfSend`, not `ControlFlow`, so neither
    /// `control_flow_has_mutations` nor `threaded_expression` ever treats it
    /// as this fold); the context half is
    /// `opaque_callable_list_op_needs_state_fold`.
    ///
    /// The gate [`Self::inline_control_flow_needs_threading`] consults so
    /// that `threaded_expression`/`subexpr_needs_prelude` (ADR 0118) turn the
    /// fold's tuple into a real prelude (`Bind` of the next `State`) plus an
    /// unwrapped value in ANY expression position — a receiver
    /// (`(items collect: block) size`), an argument, a binary operand, a
    /// conditional receiver — instead of leaking the tuple as a value.
    pub(in crate::core_erlang) fn opaque_callable_fold_needs_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        beamtalk_core::state_threading_selectors::is_opaque_callable_hom_send(expr)
            && self.opaque_callable_list_op_needs_state_fold()
    }

    /// The raw `{Result, NewState}` tuple `Document` for an expression
    /// [`Self::opaque_callable_fold_needs_threading`] accepted — compiled
    /// straight through the list intrinsic (`try_generate_list_message`),
    /// the same dispatch `generate_message_send` would reach for it.
    /// `Ok(None)` when `expr` is not such a send.
    pub(in crate::core_erlang) fn opaque_callable_fold_tuple_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<Option<Document<'static>>> {
        if !self.opaque_callable_fold_needs_threading(expr) {
            return Ok(None);
        }
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr.unwrap_parens()
        else {
            return Ok(None);
        };
        self.try_generate_list_message(receiver, selector, arguments)
    }

    /// Emits one tier-discriminated invocation of an opaque callable:
    ///
    /// ```text
    /// case call 'erlang':'is_function'(Callable, N) of
    ///   <'true'>  when 'true' -> let R = apply Callable (Args) in K(R, StAcc)
    ///   <'false'> when 'true' ->
    ///     let T = apply Callable (Args, StAcc) in
    ///     let R2 = element(1, T) in let S2 = element(2, T) in K(R2, S2)
    /// end
    /// ```
    ///
    /// `N` is `args.len()`: a Tier 1 callable takes exactly the operator's
    /// own arguments; anything else is treated as the Tier 2 protocol (one
    /// trailing `StateAcc` parameter, `{Result, NewStateAcc}` reply). `k`
    /// builds the fold's next accumulator from the call's result and the
    /// (possibly updated) state map.
    fn opaque_callable_apply_doc(
        &mut self,
        callable_var: &str,
        args: &[String],
        st_acc_var: &str,
        k: &AccContinuation,
    ) -> Document<'static> {
        let tier1_result = self.fresh_temp_var("R");
        let tier2_tuple = self.fresh_temp_var("T");
        let tier2_result = self.fresh_temp_var("R");
        let tier2_state = self.fresh_temp_var("NewSt");

        let args_doc = |with_state: bool| -> Document<'static> {
            let mut parts: Vec<Document<'static>> =
                args.iter().map(|a| leaf::var(a.clone())).collect();
            if with_state {
                parts.push(leaf::var(st_acc_var.to_string()));
            }
            beamtalk_cerl_doc::join(parts, &Document::Str(", "))
        };
        let arity = i64::try_from(args.len()).unwrap_or(i64::MAX);

        docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(callable_var.to_string()),
            ", ",
            leaf::int_lit(arity),
            ") of <'true'> when 'true' -> let ",
            leaf::var(tier1_result.clone()),
            " = apply ",
            leaf::var(callable_var.to_string()),
            " (",
            args_doc(false),
            ") in ",
            k(leaf::var(tier1_result), leaf::var(st_acc_var.to_string())),
            " <'false'> when 'true' -> let ",
            leaf::var(tier2_tuple.clone()),
            " = apply ",
            leaf::var(callable_var.to_string()),
            " (",
            args_doc(true),
            ") in let ",
            leaf::var(tier2_result.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(tier2_tuple.clone()),
            ") in let ",
            leaf::var(tier2_state.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(tier2_tuple),
            ") in ",
            k(leaf::var(tier2_result), leaf::var(tier2_state)),
            " end",
        ]
    }

    /// ADR 0128: lowers a collection HOM forwarding an opaque callable in
    /// an Actor instance method onto a `lists:foldl` loop that threads the
    /// `State` map — see this module's doc comment. Returns a raw
    /// `{Result, NewState}` tuple `Document`.
    ///
    /// No `is_list`/message-send fallback: `list_recv_to_safe_list_doc`'s
    /// `beamtalk_collection:to_list` conversion handles a non-list receiver
    /// (`Array`, `Set`, a user collection answering `do:`), and
    /// `beamtalk_collection:from_list_like` rebuilds a receiver-shaped
    /// `collect:`/`select:` result — mirroring the literal-block
    /// `_with_mutations` siblings.
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_opaque_callable_fold(
        &mut self,
        receiver: &Expression,
        callable: &Expression,
        op: OpaqueFoldOp<'_>,
    ) -> Result<Document<'static>> {
        let line_info = self
            .span_to_line(callable.span())
            .map_or(String::new(), |l| format!(" at line {l}"));
        self.emit_stateacc_fallback_diagnostic(
            format!(
                "Loop{line_info}: StateAcc fallback — {}",
                StateAccFallbackReason::NonLiteralCallable
            ),
            callable.span(),
        );

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let callable_var = self.fresh_temp_var("Callable");
        let raw_code = self.expression_doc(callable)?;
        let seed_state = self.current_field_read_state_var();

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(docvec![
            "let ",
            leaf::var(callable_var.clone()),
            " = ",
            raw_code,
            " in "
        ]);
        docs.push(list_recv_to_safe_list_doc(
            recv_code,
            list_var.clone(),
            safe_list_var.clone(),
        ));

        let elem_var = self.fresh_temp_var("Elem");
        let acc_var = self.fresh_temp_var("Acc");
        let fold_fun_var = self.fresh_temp_var("FoldFun");
        let fold_result_var = self.fresh_temp_var("FoldResult");

        // Every operator threads a `{Slot, [FoundFlag,] StAcc}` tuple whose
        // LAST element is the raw `State` map — the SAME shape a Tier 2
        // callable's own `StateAcc` parameter and the reply's `NewState`
        // slot both require (a Tier 2 reply's element 2, never the reply
        // pair itself, becomes the next `StAcc`).
        let slot = self.fresh_temp_var("Slot");
        let st_acc = self.fresh_temp_var("StAcc");
        let slot_seed: Document<'static> = match op {
            OpaqueFoldOp::Inject { initial } => {
                let init_var = self.fresh_temp_var("temp");
                let init_code = self.expression_doc(initial)?;
                docs.push(docvec![
                    "let ",
                    leaf::var(init_var.clone()),
                    " = ",
                    init_code,
                    " in "
                ]);
                leaf::var(init_var)
            }
            OpaqueFoldOp::Count => leaf::int_lit(0),
            OpaqueFoldOp::Satisfy { is_all } => leaf::atom(if is_all { "true" } else { "false" }),
            OpaqueFoldOp::Collect | OpaqueFoldOp::Select => Document::Str("[]"),
            OpaqueFoldOp::Do | OpaqueFoldOp::Detect | OpaqueFoldOp::DetectIfNone { .. } => {
                leaf::atom("nil")
            }
        };
        // `detect:` carries a found flag in slot 2 (`'nil'` is both the seed
        // and a legitimate match), the layout `bind_detect_found_or_raise_doc`
        // reads.
        let is_detect = matches!(op, OpaqueFoldOp::Detect | OpaqueFoldOp::DetectIfNone { .. });
        let flag = self.fresh_temp_var("Flag");
        let state_index: i64 = if is_detect { 3 } else { 2 };

        let args: Vec<String> = match op {
            OpaqueFoldOp::Inject { .. } => vec![slot.clone(), elem_var.clone()],
            _ => vec![elem_var.clone()],
        };
        let slot_v = slot.clone();
        let elem_v = elem_var.clone();
        let slot_doc = move || leaf::var(slot_v.clone());
        let elem_doc = move || leaf::var(elem_v.clone());
        let k: Box<AccContinuation> = match op {
            OpaqueFoldOp::Do => Box::new(move |_r, s| docvec!["{", slot_doc(), ", ", s, "}"]),
            OpaqueFoldOp::Collect => {
                Box::new(move |r, s| docvec!["{[", r, " | ", slot_doc(), "], ", s, "}"])
            }
            OpaqueFoldOp::Select => Box::new(move |r, s| {
                docvec![
                    "case ",
                    r,
                    " of <'true'> when 'true' -> {[",
                    elem_doc(),
                    " | ",
                    slot_doc(),
                    "], ",
                    s.clone(),
                    "} <'false'> when 'true' -> {",
                    slot_doc(),
                    ", ",
                    s,
                    "} end"
                ]
            }),
            OpaqueFoldOp::Inject { .. } => Box::new(|r, s| docvec!["{", r, ", ", s, "}"]),
            OpaqueFoldOp::Count => Box::new(move |r, s| {
                docvec![
                    "case ",
                    r,
                    " of <'true'> when 'true' -> {call 'erlang':'+'(",
                    slot_doc(),
                    ", 1), ",
                    s.clone(),
                    "} <'false'> when 'true' -> {",
                    slot_doc(),
                    ", ",
                    s,
                    "} end"
                ]
            }),
            OpaqueFoldOp::Satisfy { .. } => Box::new(|r, s| {
                docvec![
                    "case ",
                    r,
                    " of <'true'> when 'true' -> {'true', ",
                    s.clone(),
                    "} <'false'> when 'true' -> {'false', ",
                    s,
                    "} end"
                ]
            }),
            OpaqueFoldOp::Detect | OpaqueFoldOp::DetectIfNone { .. } => Box::new(move |r, s| {
                docvec![
                    "case ",
                    r,
                    " of <'true'> when 'true' -> {",
                    elem_doc(),
                    ", 'true', ",
                    s.clone(),
                    "} <'false'> when 'true' -> {'nil', 'false', ",
                    s,
                    "} end"
                ]
            }),
        };
        let body = self.opaque_callable_apply_doc(&callable_var, &args, &st_acc, k.as_ref());

        // Short-circuit guard: once the answer is decided, pass the
        // accumulator through without invoking the callable again.
        // `(scrutinee, decided_value, pending_value)`.
        let decided = match op {
            OpaqueFoldOp::Detect | OpaqueFoldOp::DetectIfNone { .. } => {
                Some((leaf::var(flag.clone()), true))
            }
            OpaqueFoldOp::Satisfy { is_all } => Some((leaf::var(slot.clone()), !is_all)),
            _ => None,
        };
        let guarded_body = match decided {
            Some((scrutinee, done)) => docvec![
                "case ",
                scrutinee,
                " of <",
                leaf::atom(if done { "true" } else { "false" }),
                "> when 'true' -> ",
                leaf::var(acc_var.clone()),
                " <",
                leaf::atom(if done { "false" } else { "true" }),
                "> when 'true' -> ",
                body,
                " end"
            ],
            None => body,
        };

        let flag_bind = if is_detect {
            docvec![
                "let ",
                leaf::var(flag),
                " = call 'erlang':'element'(2, ",
                leaf::var(acc_var.clone()),
                ") in "
            ]
        } else {
            Document::Nil
        };
        let init_acc = if is_detect {
            docvec!["{", slot_seed, ", 'false', ", leaf::var(seed_state), "}"]
        } else {
            docvec!["{", slot_seed, ", ", leaf::var(seed_state), "}"]
        };

        docs.push(docvec![
            "let ",
            leaf::var(fold_fun_var.clone()),
            " = fun (",
            leaf::var(elem_var),
            ", ",
            leaf::var(acc_var.clone()),
            ") -> let ",
            leaf::var(slot),
            " = call 'erlang':'element'(1, ",
            leaf::var(acc_var.clone()),
            ") in ",
            flag_bind,
            "let ",
            leaf::var(st_acc),
            " = call 'erlang':'element'(",
            leaf::int_lit(state_index),
            ", ",
            leaf::var(acc_var),
            ") in ",
            guarded_body,
            " in let ",
            leaf::var(fold_result_var.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(fold_fun_var),
            ", ",
            init_acc,
            ", ",
            leaf::var(safe_list_var),
            ") in ",
        ]);

        let final_state = self.fresh_temp_var("FinalState");
        docs.push(docvec![
            "let ",
            leaf::var(final_state.clone()),
            " = call 'erlang':'element'(",
            leaf::int_lit(state_index),
            ", ",
            leaf::var(fold_result_var.clone()),
            ") in ",
        ]);

        match op {
            OpaqueFoldOp::Do => {
                docs.push(docvec!["{'nil', ", leaf::var(final_state), "}"]);
            }
            OpaqueFoldOp::Collect | OpaqueFoldOp::Select => {
                let rev_list = self.fresh_temp_var("RevList");
                let final_list = self.fresh_temp_var("FinalList");
                docs.push(docvec![
                    "let ",
                    leaf::var(rev_list.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result_var),
                    ") in let ",
                    leaf::var(final_list.clone()),
                    " = call 'lists':'reverse'(",
                    leaf::var(rev_list),
                    ") in ",
                ]);
                let (like_binding, like_result) =
                    self.generate_list_like_result_binding(&list_var, &final_list);
                docs.push(docvec![
                    like_binding,
                    " in {",
                    leaf::var(like_result),
                    ", ",
                    leaf::var(final_state),
                    "}",
                ]);
            }
            OpaqueFoldOp::Inject { .. } | OpaqueFoldOp::Count | OpaqueFoldOp::Satisfy { .. } => {
                docs.push(docvec![
                    "{call 'erlang':'element'(1, ",
                    leaf::var(fold_result_var),
                    "), ",
                    leaf::var(final_state),
                    "}",
                ]);
            }
            OpaqueFoldOp::Detect => {
                let found_result = self.fresh_temp_var("FoundResult");
                let class_var = self.fresh_temp_var("DetectRecvClass");
                docs.push(docvec![
                    bind_detect_found_or_raise_doc(
                        &found_result,
                        &fold_result_var,
                        &list_var,
                        &class_var
                    ),
                    "{",
                    leaf::var(found_result),
                    ", ",
                    leaf::var(final_state),
                    "}",
                ]);
            }
            OpaqueFoldOp::DetectIfNone { if_none } => {
                let if_none_arm = self.generate_if_none_branch_tuple(if_none, &final_state)?;
                docs.push(docvec![
                    "case call 'erlang':'element'(2, ",
                    leaf::var(fold_result_var.clone()),
                    ") of <'true'> when 'true' -> {call 'erlang':'element'(1, ",
                    leaf::var(fold_result_var),
                    "), ",
                    leaf::var(final_state),
                    "} <'false'> when 'true' -> ",
                    if_none_arm,
                    " end",
                ]);
            }
        }

        Ok(Document::Vec(docs))
    }
}
