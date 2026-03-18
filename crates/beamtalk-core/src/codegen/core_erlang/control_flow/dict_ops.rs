// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dictionary iteration control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for dictionary iteration constructs: `do:` and `doWithKey:`.
//! For mutation-threading, iterates over `maps:values` (for `do:`) or
//! `maps:to_list` (for `doWithKey:`) using `lists:foldl` with state threading.

use super::super::document::Document;
use super::super::intrinsics::validate_block_arity_exact;
use super::super::{CodeGenContext, CoreErlangGenerator, Result, block_analysis};
use super::{BodyKind, ThreadingPlan};
use crate::ast::{Block, Expression};
use crate::docvec;
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates code for `dictionary do:` iteration with state threading.
    ///
    /// Converts the dictionary to a list of values via `maps:values/1`,
    /// then uses `lists:foldl` with state threading identical to list `do:`.
    pub(in crate::codegen::core_erlang) fn generate_dict_do(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            1,
            "do:",
            "Fix: The body block must take one argument (each value):\n\
             \x20 dict do: [:value | value printString]",
        )?;

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_dict_do_with_mutations(receiver, body_block);
            }
        }

        // No mutations: delegate to runtime helper
        Ok(Document::Nil)
    }

    /// Generates stateful `dictionary do:` using `lists:foldl` over `maps:values`.
    #[allow(clippy::too_many_lines)]
    pub(in crate::codegen::core_erlang) fn generate_dict_do_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let dict_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let values_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        if plan.use_tuple_acc {
            let init_tuple_doc = plan.initial_vars_tuple_doc(self);

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(docvec![
                "let ",
                Document::String(dict_var.clone()),
                " = ",
                recv_code,
                " in let ",
                Document::String(values_var.clone()),
                " = call 'maps':'values'(",
                Document::String(dict_var),
                ") in let ",
                Document::String(lambda_var.clone()),
                " = fun (",
                Document::String(item_var.clone()),
                ", StateAcc) -> ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            docs.extend(plan.generate_tuple_unpack_docs(self, "StateAcc", 1));

            let (body_doc, _) =
                self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlDo)?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 1, self);
            let result_doc = if self.in_direct_params_loop {
                self.last_open_scope_result = Some("_".to_string());
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(values_var),
                    ") in ",
                    extract_doc,
                ]
            } else if matches!(plan.context, CodeGenContext::ValueType) {
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(values_var),
                    ") in ",
                    extract_doc,
                    "'nil'",
                ]
            } else {
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(values_var),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{'nil', ",
                    Document::String(stateacc),
                    "}",
                ]
            };
            docs.push(result_doc);
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path (field mutations or complex control flow present).
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec![
            format!("let {dict_var} = "),
            recv_code,
            format!(
                " in let {values_var} = call 'maps':'values'({dict_var}) \
                 in let {lambda_var} = fun ({item_var}, StateAcc) -> "
            ),
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlDo)?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {init_state}, {values_var}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&fold_result, self));

        if !plan.threaded_locals.is_empty() && matches!(plan.context, CodeGenContext::ValueType) {
            post_code.push_str("'nil'");
        } else {
            let _ = write!(post_code, "{{'nil', {fold_result}}}");
        }
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }

    /// Generates code for `dictionary doWithKey:` iteration with state threading.
    ///
    /// Converts the dictionary to a list of `{K, V}` pairs via `maps:to_list/1`,
    /// then uses `lists:foldl` with state threading. The lambda destructures each
    /// pair to bind both key and value parameters.
    pub(in crate::codegen::core_erlang) fn generate_dict_do_with_key(
        &mut self,
        receiver: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            2,
            "doWithKey:",
            "Fix: The body block must take two arguments (key and value):\n\
             \x20 dict doWithKey: [:key :value | key printString]",
        )?;

        if let Expression::Block(body_block) = body {
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis) {
                return self.generate_dict_do_with_key_mutations(receiver, body_block);
            }
        }

        // No mutations: delegate to runtime helper
        Ok(Document::Nil)
    }

    /// Generates stateful `dictionary doWithKey:` using `lists:foldl` over `maps:to_list`.
    ///
    /// The foldl lambda receives `{K, V}` tuples. We destructure these inside the
    /// lambda body by extracting element(1, Pair) and element(2, Pair).
    #[allow(clippy::too_many_lines, clippy::similar_names)]
    pub(in crate::codegen::core_erlang) fn generate_dict_do_with_key_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let dict_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let pairs_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let pair_var = self.fresh_temp_var("Pair");

        // Get parameter names for key and value
        let key_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let val_param = body.parameters.get(1).map_or("_", |p| p.name.as_str());
        let key_var = Self::to_core_erlang_var(key_param);
        let val_var = Self::to_core_erlang_var(val_param);

        if plan.use_tuple_acc {
            let init_tuple_doc = plan.initial_vars_tuple_doc(self);

            let mut docs: Vec<Document<'static>> = Vec::new();
            docs.push(docvec![
                "let ",
                Document::String(dict_var.clone()),
                " = ",
                recv_code,
                " in let ",
                Document::String(pairs_var.clone()),
                " = call 'maps':'to_list'(",
                Document::String(dict_var),
                ") in let ",
                Document::String(lambda_var.clone()),
                " = fun (",
                Document::String(pair_var.clone()),
                ", StateAcc) -> let ",
                Document::String(key_var.clone()),
                " = call 'erlang':'element'(1, ",
                Document::String(pair_var.clone()),
                ") in let ",
                Document::String(val_var.clone()),
                " = call 'erlang':'element'(2, ",
                Document::String(pair_var),
                ") in ",
            ]);

            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &key_var);
            }
            if let Some(param) = body.parameters.get(1) {
                self.bind_var(&param.name, &val_var);
            }
            docs.extend(plan.generate_tuple_unpack_docs(self, "StateAcc", 1));

            let (body_doc, _) =
                self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlDo)?;
            docs.push(body_doc);
            self.pop_scope();

            let fold_result = self.fresh_temp_var("FoldResult");
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 1, self);
            let result_doc = if self.in_direct_params_loop {
                self.last_open_scope_result = Some("_".to_string());
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(pairs_var),
                    ") in ",
                    extract_doc,
                ]
            } else if matches!(plan.context, CodeGenContext::ValueType) {
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(pairs_var),
                    ") in ",
                    extract_doc,
                    "'nil'",
                ]
            } else {
                let (repack_doc, stateacc) = plan.append_repack_stateacc_doc(self);
                docvec![
                    " in let ",
                    Document::String(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    Document::String(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    Document::String(pairs_var),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{'nil', ",
                    Document::String(stateacc),
                    "}",
                ]
            };
            docs.push(result_doc);
            return Ok(Document::Vec(docs));
        }

        // Map-accumulator path (field mutations or complex control flow present).
        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec![
            format!("let {dict_var} = "),
            recv_code,
            format!(
                " in let {pairs_var} = call 'maps':'to_list'({dict_var}) \
                 in let {lambda_var} = fun ({pair_var}, StateAcc) -> \
                 let {key_var} = call 'erlang':'element'(1, {pair_var}) \
                 in let {val_var} = call 'erlang':'element'(2, {pair_var}) in "
            ),
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &key_var);
        }
        if let Some(param) = body.parameters.get(1) {
            self.bind_var(&param.name, &val_var);
        }
        docs.extend(plan.generate_unpack_at_iteration_start(self));

        let (body_doc, _) = self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlDo)?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let mut post_code = format!(
            " in let {fold_result} = call 'lists':'foldl'({lambda_var}, {init_state}, {pairs_var}) in "
        );
        post_code.push_str(&plan.generate_extract_suffix(&fold_result, self));

        if !plan.threaded_locals.is_empty() && matches!(plan.context, CodeGenContext::ValueType) {
            post_code.push_str("'nil'");
        } else {
            let _ = write!(post_code, "{{'nil', {fold_result}}}");
        }
        docs.push(Document::String(post_code));

        Ok(Document::Vec(docs))
    }
}
