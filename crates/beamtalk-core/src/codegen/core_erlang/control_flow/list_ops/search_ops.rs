// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Search/detect list operations: `anySatisfy:`, `allSatisfy:`, and `detect:`.

use super::super::super::document::Document;
use super::super::super::intrinsics::validate_block_arity_exact;
use super::super::super::{CoreErlangGenerator, Result, block_analysis};
use super::super::{BodyKind, ThreadingPlan};
use crate::ast::{Block, Expression};
use crate::docvec;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// BT-1481: Generates code for `list anySatisfy:` with mutation analysis.
    pub(in crate::codegen::core_erlang) fn generate_list_any_satisfy(
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

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self
                    .generate_list_bool_predicate_with_mutations(receiver, body_block, false);
            }
        }

        // No mutations: fall through to simple BIF call (lists:any/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'lists':'any'({body_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'anySatisfy:', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1481: Generates code for `list allSatisfy:` with mutation analysis.
    pub(in crate::codegen::core_erlang) fn generate_list_all_satisfy(
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

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self
                    .generate_list_bool_predicate_with_mutations(receiver, body_block, true);
            }
        }

        // No mutations: fall through to simple BIF call (lists:all/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'lists':'all'({body_var}, {list_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'allSatisfy:', [{body_var}]) end"
            ),
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
    pub(in crate::codegen::core_erlang) fn generate_list_bool_predicate_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        is_all: bool,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
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
            docs.push(docvec![
                "let ",
                Document::String(list_var.clone()),
                " = ",
                recv_code,
                " in let ",
                Document::String(safe_list_var.clone()),
                " = case call 'erlang':'is_list'(",
                Document::String(list_var.clone()),
                ") of <'true'> when 'true' -> ",
                Document::String(list_var.clone()),
                " <'false'> when 'true' -> call 'beamtalk_collection':'to_list'(",
                Document::String(list_var),
                ") end in let ",
                Document::String(lambda_var.clone()),
                " = fun (",
                Document::String(item_var.clone()),
                ", ",
                Document::String(acc_state_var.clone()),
                ") -> let BoolAcc = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 2));

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
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(bool_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                ]);
            } else {
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docs.push(docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(bool_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    Document::String(bool_result),
                    ", ",
                    Document::String(stateacc),
                    "}",
                ]);
            }
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(
                " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> {list_var} \
                 <'false'> when 'true' -> \
                 call 'beamtalk_collection':'to_list'({list_var}) end \
                 in let {lambda_var} = fun ({item_var}, {acc_state_var}) -> \
                 let BoolAcc = call 'erlang':'element'(1, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(2, {acc_state_var}) in "
            ),
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

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, \
             {{{init_bool}, {init_state}}}, {safe_list_var}) \
             in let {bool_result} = call 'erlang':'element'(1, {fold_result}) \
             in let {state_out} = call 'erlang':'element'(2, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{bool_result}, {state_out}}}");
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    /// BT-1486: Generates code for `list detect:` with mutation analysis.
    ///
    /// Without mutations: falls through to `beamtalk_list:detect/2` BIF.
    /// With mutations: uses `lists:foldl` to process all elements (no short-circuit)
    /// so that field mutations are applied for every element. Returns the first
    /// matching element or `nil`.
    pub(in crate::codegen::core_erlang) fn generate_list_detect(
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

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_list_detect_with_mutations(receiver, body_block);
            }
        }

        // No mutations: fall through to BIF call (beamtalk_list:detect/2)
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let body_var = self.fresh_temp_var("temp");
        let body_code = self.expression_doc(body)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {body_var} = "),
            body_code,
            format!(
                " in case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> \
                 call 'beamtalk_list':'detect'({list_var}, {body_var}) \
                 <'false'> when 'true' -> \
                 call 'beamtalk_primitive':'send'({list_var}, 'detect:', [{body_var}]) end"
            ),
        ])
    }

    /// BT-1486: Generates code for `list detect:ifNone:` with mutation analysis.
    ///
    /// Without mutations: falls through to runtime dispatch.
    /// With mutations: uses `lists:foldl` like `detect:`, then applies the ifNone
    /// block if no match was found.
    pub(in crate::codegen::core_erlang) fn generate_list_detect_if_none(
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

        if let Expression::Block(pred_block) = predicate {
            let analysis = block_analysis::analyze_block(pred_block);
            if self.needs_mutation_threading(&analysis) {
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
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let pred_var = self.fresh_temp_var("temp");
        let pred_code = self.expression_doc(predicate)?;
        let none_var = self.fresh_temp_var("temp");
        let none_code = self.expression_doc(if_none)?;

        Ok(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(" in let {pred_var} = "),
            pred_code,
            format!(" in let {none_var} = "),
            none_code,
            format!(
                " in call 'beamtalk_primitive':'send'({list_var}, \
                 'detect:ifNone:', [{pred_var}, {none_var}])"
            ),
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

        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        let acc_state_var = self.fresh_temp_var("AccSt");

        // Compile the ifNone block upfront.
        let none_code = self.expression_doc(if_none)?;

        if plan.use_tuple_acc {
            // Tuple-accumulator path: {FoundItem, FoundFlag, Var1, ..., VarN}
            let vars_doc = plan.current_vars_doc(self);
            let init_tuple_doc = docvec!["{'nil', 'false', ", vars_doc, "}"];

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(docvec![
                "let ",
                Document::String(list_var.clone()),
                " = ",
                recv_code,
                " in let ",
                Document::String(safe_list_var.clone()),
                " = case call 'erlang':'is_list'(",
                Document::String(list_var.clone()),
                ") of <'true'> when 'true' -> ",
                Document::String(list_var.clone()),
                " <'false'> when 'true' -> call 'beamtalk_collection':'to_list'(",
                Document::String(list_var),
                ") end in let ",
                Document::String(lambda_var.clone()),
                " = fun (",
                Document::String(item_var.clone()),
                ", ",
                Document::String(acc_state_var.clone()),
                ") -> let FoundItem = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
                ") in let FoundFlag = call 'erlang':'element'(2, ",
                Document::String(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 3));

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
                self.direct_params_list_op_result = Some(final_result.clone());
                docs.push(docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(found_item.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result.clone()),
                    ") in let ",
                    Document::String(found_flag.clone()),
                    " = call 'erlang':'element'(2, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    "let ",
                    Document::String(final_result.clone()),
                    " = case ",
                    Document::String(found_flag),
                    " of <'true'> when 'true' -> ",
                    Document::String(found_item.clone()),
                    " <'false'> when 'true' -> let ",
                    Document::String(none_result.clone()),
                    " = apply ",
                    none_code,
                    " () in ",
                    Document::String(none_result),
                    " end in ",
                ]);
            } else {
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docs.push(docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(found_item.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result.clone()),
                    ") in let ",
                    Document::String(found_flag.clone()),
                    " = call 'erlang':'element'(2, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "case ",
                    Document::String(found_flag),
                    " of <'true'> when 'true' -> {",
                    Document::String(found_item.clone()),
                    ", ",
                    Document::String(stateacc.clone()),
                    "} <'false'> when 'true' -> let ",
                    Document::String(none_result.clone()),
                    " = apply ",
                    none_code,
                    " () in {",
                    Document::String(none_result),
                    ", ",
                    Document::String(stateacc),
                    "} end",
                ]);
            }
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(
                " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> {list_var} \
                 <'false'> when 'true' -> \
                 call 'beamtalk_collection':'to_list'({list_var}) end \
                 in let {lambda_var} = fun ({item_var}, {acc_state_var}) -> \
                 let FoundItem = call 'erlang':'element'(1, {acc_state_var}) in \
                 let FoundFlag = call 'erlang':'element'(2, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(3, {acc_state_var}) in "
            ),
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
        let none_result = self.fresh_temp_var("NoneResult");

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, \
             {{'nil', 'false', {init_state}}}, {safe_list_var}) \
             in let {found_item} = call 'erlang':'element'(1, {fold_result}) \
             in let {found_flag} = call 'erlang':'element'(2, {fold_result}) \
             in let {state_out} = call 'erlang':'element'(3, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        docs.push(Document::String(post_code));

        // Case on FoundFlag to decide result.
        docs.push(docvec![
            "case ",
            Document::String(found_flag),
            " of <'true'> when 'true' -> {",
            Document::String(found_item),
            ", ",
            Document::String(state_out.clone()),
            "} <'false'> when 'true' -> let ",
            Document::String(none_result.clone()),
            " = apply ",
            none_code,
            " () in {",
            Document::String(none_result),
            ", ",
            Document::String(state_out),
            "} end",
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
    pub(in crate::codegen::core_erlang) fn generate_list_detect_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
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
            docs.push(docvec![
                "let ",
                Document::String(list_var.clone()),
                " = ",
                recv_code,
                " in let ",
                Document::String(safe_list_var.clone()),
                " = case call 'erlang':'is_list'(",
                Document::String(list_var.clone()),
                ") of <'true'> when 'true' -> ",
                Document::String(list_var.clone()),
                " <'false'> when 'true' -> call 'beamtalk_collection':'to_list'(",
                Document::String(list_var),
                ") end in let ",
                Document::String(lambda_var.clone()),
                " = fun (",
                Document::String(item_var.clone()),
                ", ",
                Document::String(acc_state_var.clone()),
                ") -> let FoundItem = call 'erlang':'element'(1, ",
                Document::String(acc_state_var.clone()),
                ") in let FoundFlag = call 'erlang':'element'(2, ",
                Document::String(acc_state_var.clone()),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            // Unpack vars starting at index 3 (slot 1 = FoundItem, slot 2 = FoundFlag).
            docs.extend(plan.generate_tuple_unpack_docs(self, &acc_state_var, 3));

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

            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 3, self);
            if self.in_direct_params_loop {
                self.direct_params_list_op_result = Some(found_result.clone());
                docs.push(docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(found_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                ]);
            } else {
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docs.push(docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(safe_list_var),
                    ") in let ",
                    Document::String(found_result.clone()),
                    " = call 'erlang':'element'(1, ",
                    Document::String(fold_result),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{",
                    Document::String(found_result),
                    ", ",
                    Document::String(stateacc),
                    "}",
                ]);
            }
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path.
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec![
            format!("let {list_var} = "),
            recv_code,
            format!(
                " in let {safe_list_var} = case call 'erlang':'is_list'({list_var}) of \
                 <'true'> when 'true' -> {list_var} \
                 <'false'> when 'true' -> \
                 call 'beamtalk_collection':'to_list'({list_var}) end \
                 in let {lambda_var} = fun ({item_var}, {acc_state_var}) -> \
                 let FoundItem = call 'erlang':'element'(1, {acc_state_var}) in \
                 let FoundFlag = call 'erlang':'element'(2, {acc_state_var}) in \
                 let StateAcc = call 'erlang':'element'(3, {acc_state_var}) in "
            ),
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
        let state_out = self.fresh_temp_var("StOut");

        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, \
             {{'nil', 'false', {init_state}}}, {safe_list_var}) \
             in let {found_result} = call 'erlang':'element'(1, {fold_result}) \
             in let {state_out} = call 'erlang':'element'(3, {fold_result}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&state_out, self));
        let _ = write!(post_code, "{{{found_result}, {state_out}}}");
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }
}
