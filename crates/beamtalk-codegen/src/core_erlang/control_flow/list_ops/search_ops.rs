// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Search/detect list operations: `anySatisfy:`, `allSatisfy:`, and `detect:`.

use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CoreErlangGenerator, Result};
use super::super::{BodyKind, ListOpKind, ThreadingPlan};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

/// BT-3028: binds the `detect:` answer out of a mutation-threading fold result,
/// raising `not_found` when nothing matched.
///
/// The fold seeds its accumulator with `{'nil', 'false', …}` and processes every
/// element, so slot 1 alone cannot say whether the search succeeded — `'nil'` is
/// both the seed and a legitimate match. Slot 2 is the found flag, and it is the
/// one consulted here, which also makes `#(nil) detect: [:x | x isNil]` answer
/// `nil` rather than raise.
///
/// The class named in the error comes from the receiver, so a `Set` routed
/// through the list-op path reports `Set` rather than `List`.
fn bind_detect_found_or_raise_doc(
    found_result: &str,
    fold_result: &str,
    recv_var: &str,
    class_var: &str,
) -> Document<'static> {
    docvec![
        "let ",
        leaf::var(found_result.to_owned()),
        " = case call 'erlang':'element'(2, ",
        leaf::var(fold_result.to_owned()),
        ") of <'true'> when 'true' -> call 'erlang':'element'(1, ",
        leaf::var(fold_result.to_owned()),
        ") <'false'> when 'true' -> let ",
        leaf::var(class_var.to_owned()),
        " = call 'beamtalk_primitive':'class_of'(",
        leaf::var(recv_var.to_owned()),
        ") in call 'beamtalk_collection':'raiseDetectNotFound'(",
        leaf::var(class_var.to_owned()),
        ") end in ",
    ]
}

impl CoreErlangGenerator {
    /// BT-1481: Generates code for `list anySatisfy:` with mutation analysis.
    pub(in crate::core_erlang) fn generate_list_any_satisfy(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "anySatisfy:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list anySatisfy: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_bool_predicate_with_mutations(receiver, body_block, false);
        }

        // No mutations: fall through to simple BIF call (lists:any/2)
        // BT-3151: see `check_bare_list_op_block_self_sends`'s doc comment.
        self.check_bare_list_op_block_self_sends(body)?;
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'lists':'any'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_message_dispatch':'send'(",
            leaf::var(list_var),
            ", 'anySatisfy:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-1481: Generates code for `list allSatisfy:` with mutation analysis.
    pub(in crate::core_erlang) fn generate_list_all_satisfy(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "allSatisfy:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list allSatisfy: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_bool_predicate_with_mutations(receiver, body_block, true);
        }

        // No mutations: fall through to simple BIF call (lists:all/2)
        // BT-3151: see `check_bare_list_op_block_self_sends`'s doc comment.
        self.check_bare_list_op_block_self_sends(body)?;
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'lists':'all'(",
            leaf::var(body_var.clone()),
            ", ",
            leaf::var(list_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_message_dispatch':'send'(",
            leaf::var(list_var),
            ", 'allSatisfy:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// BT-1481: Generates stateful `anySatisfy:`/`allSatisfy:` using `lists:foldl`
    /// with state threading and a boolean accumulator.
    ///
    /// Note: Unlike `lists:any/2` and `lists:all/2`, this does NOT short-circuit.
    /// All elements are always processed because field mutations must execute for
    /// every element to maintain correct state.
    ///
    /// Accumulator is `{BoolAcc, StateVars...}` (tuple) or `{BoolAcc, StateAcc}` (map).
    /// - `is_all = false` (anySatisfy): `BoolAcc` starts `false`, set to `true` on match
    /// - `is_all = true` (allSatisfy): `BoolAcc` starts `true`, set to `false` on failure
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_list_bool_predicate_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        is_all: bool,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body, ListOpKind::Accumulate);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");
        let init_bool = if is_all { "'true'" } else { "'false'" };

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {BoolAcc, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{", init_bool, ", ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let BoolAcc = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            docs.push(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlBoolPredicate { is_all },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let bool_result = self.fresh_temp_var("BoolResult");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 2, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(bool_result.clone());
                docs.push(docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(bool_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result),
                    ") in ",
                    extract_doc,
                ]);
            } else {
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docs.push(docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(bool_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(bool_result),
                    ", ",
                    leaf::var(stateacc),
                    "}",
                ]);
            }
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        // BT-3169: when this class-method body threads ClassVars, the fold
        // fun's own accumulator parameter is a raw {ClassVars, AccSt} tuple,
        // unwrapped by `cv_prelude` immediately below — see
        // `ThreadingPlan::class_var_fun_param`'s doc comment.
        let (fun_param, cv_prelude) = plan.class_var_fun_param(self, &acc_state_var);
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(fun_param),
            ") -> ",
            cv_prelude,
            "let BoolAcc = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var),
            ") in ",
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(
            body,
            &plan,
            &BodyKind::FoldlBoolPredicate { is_all },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let bool_result = self.fresh_temp_var("BoolResult");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            plan.foldl_call_doc(
                self,
                &lambda_var,
                docvec!["{", init_bool, ", ", leaf::var(init_state), "}"],
                &safe_list_var,
                &fold_result,
            ),
            "let ",
            leaf::var(bool_result.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(bool_result),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1486: Generates code for `list detect:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:detect/2` BIF.
    /// With mutations: uses `lists:foldl` to process all elements (no short-circuit)
    /// so that field mutations are applied for every element. Returns the first
    /// matching element or `nil`.
    pub(in crate::core_erlang) fn generate_list_detect(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "detect:",
            "Fix: The body block must take one argument (each element):\n\
             \x20 list detect: [:item | item > 0]",
        )?;

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_list_detect_with_mutations(receiver, body_block);
        }

        // No mutations: fall through to BIF call (beamtalk_list:detect/2)
        // BT-3151: see `check_bare_list_op_block_self_sends`'s doc comment.
        self.check_bare_list_op_block_self_sends(body)?;
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

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
            ") of <'true'> when 'true' -> call 'beamtalk_list':'detect'(",
            leaf::var(list_var.clone()),
            ", ",
            leaf::var(body_var.clone()),
            ") <'false'> when 'true' -> call 'beamtalk_message_dispatch':'send'(",
            leaf::var(list_var),
            ", 'detect:', [",
            leaf::var(body_var),
            "]) end",
        ])
    }

    /// ADR 0118 phase 4 (BT-3420): compiles `detect:ifNone:`'s `ifNone:`
    /// handler as a BRANCH ARM seeded from `state_var` — the same
    /// `generate_conditional_branch_inline` per-frame `ThreadedIr` machinery
    /// `ifTrue:`/`ifFalse:` branches use — returning a `Document` that
    /// evaluates to `{value, finalstate}` directly. This is a branch arm,
    /// not a closure: a self-send or field mutation inside `[...]` threads
    /// through it the same way it would inside any other conditional
    /// branch, instead of being compiled as an ordinary Tier 1 closure
    /// (`apply <fun> ()`) that discards the mutation (or, once wrapped and
    /// handed to a generic runtime dispatch, crashes).
    ///
    /// Falls back to the ordinary closure+apply shape — wrapped in the same
    /// `{value, state_var}` tuple shape so callers don't need to
    /// distinguish — when `if_none` is not a literal block (e.g. a variable
    /// holding one): there is no block AST to inline, so state cannot
    /// thread through an opaque callable value.
    fn generate_if_none_branch_tuple(
        &mut self,
        if_none: &Expression,
        state_var: &str,
    ) -> Result<Document<'static>> {
        // BT-3151: unconditional (both branches below) — a class-method
        // self-send here still cannot thread its `ClassVars` mutation
        // through `detect:ifNone:`'s fold result, branch-arm routing or
        // not, so it stays a compile error regardless of which shape
        // `if_none` takes. See `check_bare_list_op_block_self_sends`'s doc
        // comment for the full rationale.
        self.check_bare_list_op_block_self_sends(if_none)?;
        let Expression::Block(block) = if_none.unwrap_parens() else {
            let none_code = self.expression_doc(if_none)?;
            let none_result = self.fresh_temp_var("NoneResult");
            return Ok(docvec![
                "let ",
                leaf::var(none_result.clone()),
                " = apply ",
                none_code,
                " () in {",
                leaf::var(none_result),
                ", ",
                leaf::var(state_var.to_string()),
                "}",
            ]);
        };
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(block))?;
        Ok(docvec![
            "let StateAcc = ",
            leaf::var(state_var.to_string()),
            " in ",
            branch_doc,
        ])
    }

    /// BT-1486: Generates code for `list detect:ifNone:` with mutation analysis.
    ///
    /// Without mutations: falls through to runtime dispatch.
    /// With mutations: uses `lists:foldl` like `detect:`, then applies the ifNone
    /// block if no match was found.
    pub(in crate::core_erlang) fn generate_list_detect_if_none(
        &mut self,
        receiver: &Expression,
        predicate: &Expression,
        if_none: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            predicate,
            1,
            "detect:ifNone:",
            "Fix: The detect block must take one argument (each element):\n\
             \x20 list detect: [:item | item > 0] ifNone: ['not found']",
        )?;
        validate_block_arity_exact(
            if_none,
            0,
            "detect:ifNone:",
            "Fix: The ifNone block must take no arguments:\n\
             \x20 list detect: [:item | item > 0] ifNone: ['not found']",
        )?;

        if let Some(pred_block) = self.block_needs_mutation_threading(predicate) {
            return self.generate_list_detect_if_none_with_mutations(receiver, pred_block, if_none);
        }
        // BT-3420 (ADR 0118 phase 4): the predicate alone may be mutation-
        // free while the `ifNone:` handler itself contains a self-send or
        // field mutation (`items detect: [:x | x > 100] ifNone: [self
        // bumpCount]`) — route through the with-mutations fold so
        // `generate_list_detect_if_none_with_mutations`'s `ifNone:` handling
        // (see its own doc comment) threads it, instead of falling to
        // `generate_detect_if_none_simple`'s bare closure, which silently
        // drops (or — since that closure closes over a state map ultimately
        // wrapped and passed to a runtime dispatch — crashes) that mutation.
        if self.block_needs_mutation_threading(if_none).is_some() {
            if let Expression::Block(pred_block) = predicate.unwrap_parens() {
                return self
                    .generate_list_detect_if_none_with_mutations(receiver, pred_block, if_none);
            }
        }

        // No mutations: fall through to runtime dispatch
        self.generate_detect_if_none_simple(receiver, predicate, if_none)
    }

    /// Simple (non-mutating) detect:ifNone: — dispatches to runtime.
    fn generate_detect_if_none_simple(
        &mut self,
        receiver: &Expression,
        predicate: &Expression,
        if_none: &Expression,
    ) -> Result<Document<'static>> {
        // BT-3151: both blocks reach `generate_block` via `expression_doc`
        // below — see `check_bare_list_op_block_self_sends`'s doc comment.
        self.check_bare_list_op_block_self_sends(predicate)?;
        self.check_bare_list_op_block_self_sends(if_none)?;
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let pred_var = self.fresh_temp_var("temp");
        let pred_code = self.expression_doc(predicate)?;
        let none_var = self.fresh_temp_var("temp");
        let none_code = self.expression_doc(if_none)?;

        Ok(docvec![
            "let ",
            leaf::var(list_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(pred_var.clone()),
            " = ",
            pred_code,
            " in let ",
            leaf::var(none_var.clone()),
            " = ",
            none_code,
            " in call 'beamtalk_message_dispatch':'send'(",
            leaf::var(list_var),
            ", 'detect:ifNone:', [",
            leaf::var(pred_var),
            ", ",
            leaf::var(none_var),
            "])",
        ])
    }

    /// BT-1486: Wrapper for `detect:ifNone:` with mutations — runs the detect foldl,
    /// then checks the `FoundFlag` and evaluates the ifNone block when no match was found.
    ///
    /// The detect foldl accumulator uses `{FoundItem, FoundFlag, State...}`. We extract
    /// `FoundFlag` from the fold result to distinguish "found nil" from "not found" —
    /// checking `FoundItem == nil` would be incorrect when nil is a valid search result.
    #[allow(clippy::too_many_lines)]
    fn generate_list_detect_if_none_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        if_none: &Expression,
    ) -> Result<Document<'static>> {
        // The detect foldl generates code that processes all elements and returns
        // {FoundItem, StateAcc}. But we need the FoundFlag to distinguish found-nil
        // from not-found. Rather than restructuring the entire detect codegen, we
        // use a 3-element fold result: {FoundItem, FoundFlag, StateAcc}.
        //
        // Strategy: call generate_list_detect_with_mutations which already uses
        // {FoundItem, FoundFlag, State...} internally. But the external result is
        // {FoundItem, StateAcc} (FoundFlag is extracted and discarded). We need to
        // preserve FoundFlag in the external result.
        //
        // Alternative: inline the foldl codegen here with a modified post-processing.
        // For simplicity, we use a wrapper that examines the foldl result directly.
        //
        // The inner foldl produces result tuple: {FoundItem, FoundFlag, State...}
        // The outer detect codegen extracts element(1) for FoundItem and element(3) for State.
        // We need element(2) for FoundFlag too.
        //
        // Since generate_list_detect_with_mutations already does the extraction and
        // repacking, we'll use a different approach: emit the full foldl inline here
        // but return {FoundItem, FoundFlag, StateAcc} and post-process.

        let plan = ThreadingPlan::new_for_foldl_list_op(self, body, ListOpKind::TwoSlot);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {FoundItem, FoundFlag, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{'nil', 'false', ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var,
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let FoundItem = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in let FoundFlag = call 'erlang':'element'(2, ",
                leaf::var(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            docs.push(plan.generate_tuple_unpack_docs(self, &acc_state_var, 3));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlDetect {
                    item_var: item_var.clone(),
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let found_item = self.fresh_temp_var("FoundItem");
            let found_flag = self.fresh_temp_var("FoundFlag");
            let final_result = self.fresh_temp_var("FinalResult");
            let none_result = self.fresh_temp_var("NoneResult");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 3, self);
            if self.in_direct_params_loop {
                // BT-3151: direct-params loops are a value-type-only
                // optimization with no actor `State` to thread through the
                // `ifNone:` handler in the first place (see
                // `check_bare_list_op_block_self_sends`'s doc comment) —
                // compiled as an ordinary closure, unaffected by BT-3420.
                self.check_bare_list_op_block_self_sends(if_none)?;
                let none_code = self.expression_doc(if_none)?;
                self.direct_params_list_op_result = Some(final_result.clone());
                docs.push(docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(found_item.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result.clone()),
                    ") in let ",
                    leaf::var(found_flag.clone()),
                    " = call 'erlang':'element'(2, ",
                    leaf::var(fold_result),
                    ") in ",
                    extract_doc,
                    "let ",
                    leaf::var(final_result.clone()),
                    " = case ",
                    leaf::var(found_flag),
                    " of <'true'> when 'true' -> ",
                    leaf::var(found_item.clone()),
                    " <'false'> when 'true' -> let ",
                    leaf::var(none_result.clone()),
                    " = apply ",
                    none_code,
                    " () in ",
                    leaf::var(none_result),
                    " end in ",
                ]);
            } else {
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                let if_none_arm = self.generate_if_none_branch_tuple(if_none, &stateacc)?;
                docs.push(docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in let ",
                    leaf::var(found_item.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(fold_result.clone()),
                    ") in let ",
                    leaf::var(found_flag.clone()),
                    " = call 'erlang':'element'(2, ",
                    leaf::var(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "case ",
                    leaf::var(found_flag),
                    " of <'true'> when 'true' -> {",
                    leaf::var(found_item.clone()),
                    ", ",
                    leaf::var(stateacc.clone()),
                    "} <'false'> when 'true' -> ",
                    if_none_arm,
                    " end",
                ]);
            }
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var,
            safe_list_var.clone(),
        ));
        // BT-3169: when this class-method body threads ClassVars, the fold
        // fun's own accumulator parameter is a raw {ClassVars, AccSt} tuple,
        // unwrapped by `cv_prelude` immediately below — see
        // `ThreadingPlan::class_var_fun_param`'s doc comment.
        let (fun_param, cv_prelude) = plan.class_var_fun_param(self, &acc_state_var);
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(fun_param),
            ") -> ",
            cv_prelude,
            "let FoundItem = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let FoundFlag = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(3, ",
            leaf::var(acc_state_var),
            ") in ",
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(
            body,
            &plan,
            &BodyKind::FoldlDetect {
                item_var: item_var.clone(),
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let found_item = self.fresh_temp_var("FoundItem");
        let found_flag = self.fresh_temp_var("FoundFlag");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            plan.foldl_call_doc(
                self,
                &lambda_var,
                docvec!["{'nil', 'false', ", leaf::var(init_state), "}"],
                &safe_list_var,
                &fold_result,
            ),
            "let ",
            leaf::var(found_item.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(found_flag.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(fold_result.clone()),
            ") in let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(3, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
        ]);

        // Case on FoundFlag to decide result.
        let if_none_arm = self.generate_if_none_branch_tuple(if_none, &state_out)?;
        docs.push(docvec![
            "case ",
            leaf::var(found_flag),
            " of <'true'> when 'true' -> {",
            leaf::var(found_item),
            ", ",
            leaf::var(state_out.clone()),
            "} <'false'> when 'true' -> ",
            if_none_arm,
            " end",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1486: Generates stateful `detect:` using `lists:foldl`
    /// with state threading and a found-item accumulator.
    ///
    /// All elements are processed (no short-circuit) because field mutations must
    /// execute for every element. Accumulator is `{FoundItem, FoundFlag, StateVars...}`.
    ///
    /// Returns `{FoundItem, StateAcc}` where `FoundItem` is `nil` if no match.
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_list_detect_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body, ListOpKind::TwoSlot);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {FoundItem, FoundFlag, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{'nil', 'false', ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(super::list_recv_to_safe_list_doc(
                recv_code,
                list_var.clone(),
                safe_list_var.clone(),
            ));
            docs.push(docvec![
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(acc_state_var.clone()),
                ") -> let FoundItem = call 'erlang':'element'(1, ",
                leaf::var(acc_state_var.clone()),
                ") in let FoundFlag = call 'erlang':'element'(2, ",
                leaf::var(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            // Unpack vars starting at index 3 (slot 1 = FoundItem, slot 2 = FoundFlag).
            docs.push(plan.generate_tuple_unpack_docs(self, &acc_state_var, 3));

            let (body_doc, _) = self.generate_threaded_loop_body(
                body,
                &plan,
                &BodyKind::FoldlDetect {
                    item_var: item_var.clone(),
                },
            )?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let found_result = self.fresh_temp_var("FoundResult");
            let class_var = self.fresh_temp_var("DetectRecvClass");

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 3, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(found_result.clone());
                docs.push(docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in ",
                    bind_detect_found_or_raise_doc(
                        &found_result,
                        &fold_result,
                        &list_var,
                        &class_var
                    ),
                    extract_doc,
                ]);
            } else {
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docs.push(docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(safe_list_var),
                    ") in ",
                    bind_detect_found_or_raise_doc(
                        &found_result,
                        &fold_result,
                        &list_var,
                        &class_var
                    ),
                    extract_doc,
                    repack_doc,
                    "{",
                    leaf::var(found_result),
                    ", ",
                    leaf::var(stateacc),
                    "}",
                ]);
            }
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(super::list_recv_to_safe_list_doc(
            recv_code,
            list_var.clone(),
            safe_list_var.clone(),
        ));
        // BT-3169: when this class-method body threads ClassVars, the fold
        // fun's own accumulator parameter is a raw {ClassVars, AccSt} tuple,
        // unwrapped by `cv_prelude` immediately below — see
        // `ThreadingPlan::class_var_fun_param`'s doc comment.
        let (fun_param, cv_prelude) = plan.class_var_fun_param(self, &acc_state_var);
        docs.push(docvec![
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(fun_param),
            ") -> ",
            cv_prelude,
            "let FoundItem = call 'erlang':'element'(1, ",
            leaf::var(acc_state_var.clone()),
            ") in let FoundFlag = call 'erlang':'element'(2, ",
            leaf::var(acc_state_var.clone()),
            ") in let StateAcc = call 'erlang':'element'(3, ",
            leaf::var(acc_state_var),
            ") in ",
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(
            body,
            &plan,
            &BodyKind::FoldlDetect {
                item_var: item_var.clone(),
            },
        )?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let found_result = self.fresh_temp_var("FoundResult");
        let class_var = self.fresh_temp_var("DetectRecvClass");
        let state_out = self.fresh_temp_var("StOut");

        docs.push(docvec![
            plan.foldl_call_doc(
                self,
                &lambda_var,
                docvec!["{'nil', 'false', ", leaf::var(init_state), "}"],
                &safe_list_var,
                &fold_result,
            ),
            bind_detect_found_or_raise_doc(&found_result, &fold_result, &list_var, &class_var),
            "let ",
            leaf::var(state_out.clone()),
            " = call 'erlang':'element'(3, ",
            leaf::var(fold_result),
            ") in ",
            plan.generate_extract_suffix_doc(&state_out, self),
            "{",
            leaf::var(found_result),
            ", ",
            leaf::var(state_out),
            "}",
        ]);

        Ok(Document::Vec(docs))
    }
}
