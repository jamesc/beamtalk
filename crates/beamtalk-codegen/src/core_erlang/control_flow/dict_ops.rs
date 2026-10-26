// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dictionary iteration control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for dictionary iteration constructs: `do:` and `doWithKey:`.
//! For mutation-threading, iterates over `maps:values` (for `do:`) or
//! `maps:to_list` (for `doWithKey:`) using `lists:foldl` with state threading.

use super::super::intrinsics::validate_block_arity_exact;
use super::super::{CoreErlangGenerator, Result};
use super::{BodyKind, ListOpKind, ThreadingPlan};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

impl CoreErlangGenerator {
    /// Generates code for `dictionary do:` iteration with state threading.
    ///
    /// Converts the dictionary to a list of values via `maps:values/1`,
    /// then uses `lists:foldl` with state threading identical to list `do:`.
    pub(in crate::core_erlang) fn generate_dict_do(
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

        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_dict_do_with_mutations(receiver, body_block);
        }

        // No mutations: delegate to runtime helper
        Ok(Document::Nil)
    }

    /// Generates stateful `dictionary do:` using `lists:foldl` over `maps:values`.
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_dict_do_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body, ListOpKind::Do);
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
                leaf::var(dict_var.clone()),
                " = ",
                recv_code,
                " in let ",
                leaf::var(values_var.clone()),
                " = call 'maps':'values'(",
                leaf::var(dict_var),
                ") in let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", StateAcc) -> ",
            ]);

            // BT-4121: the guarded-operator marker spans the body lowering.
            let prestore_state = self.current_field_read_state_var();
            self.push_guarded_op_loop(body, None);
            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &item_var);
            }
            let (body_doc, _) =
                self.generate_foldl_loop_body(body, &plan, &BodyKind::FoldlDo, "StateAcc", 0)?;
            docs.push(body_doc);
            self.pop_scope();
            // BT-4121: check out the marker; splice the pre-store (nil
            // unless the loop brackets with a `State` in scope).
            let (guarded_op_bracket, prestore) = self.checkout_guarded_op_loop_docs(
                self.pop_guarded_op_loop(),
                false,
                body.span,
                &prestore_state,
            );
            docs.insert(0, prestore);

            let fold_result = self.fresh_temp_var("FoldResult");
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 1, self);
            let result_doc = if self.loop_mode.in_direct_params_loop {
                // see the identical branch in
                // `control_flow/list_ops/basic_ops.rs`'s `do:` — same shape here for a
                // dictionary iteration: multiple rebound accumulator vars, no single
                // "result" value, so signal open-with-no-value rather than naming one.
                self.loop_mode.direct_params_do_open_chain = true;
                docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(values_var),
                    ") in ",
                    extract_doc,
                ]
            } else {
                let (repack_doc, stateacc) =
                    plan.append_repack_stateacc_doc(self, guarded_op_bracket);
                docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(values_var),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{'nil', ",
                    leaf::var(stateacc),
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
            "let ",
            leaf::var(dict_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(values_var.clone()),
            " = call 'maps':'values'(",
            leaf::var(dict_var),
            ") in let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var("StateAcc"),
            ") -> ",
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        let (body_doc, _) =
            self.generate_foldl_loop_body(body, &plan, &BodyKind::FoldlDo, "StateAcc", 0)?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let mut post_docs: Vec<Document<'static>> = vec![ThreadingPlan::foldl_call_doc(
            &lambda_var,
            leaf::var(init_state),
            &values_var,
            &fold_result,
        )];
        post_docs.push(plan.generate_extract_suffix_doc(&fold_result, self));

        post_docs.push(docvec!["{'nil', ", leaf::var(fold_result), "}",]);
        docs.push(Document::Vec(post_docs));

        Ok(Document::Vec(docs))
    }

    /// Generates code for `dictionary doWithKey:` iteration with state threading.
    ///
    /// Converts the dictionary to a list of `{K, V}` pairs via `maps:to_list/1`,
    /// then uses `lists:foldl` with state threading. The lambda destructures each
    /// pair to bind both key and value parameters.
    pub(in crate::core_erlang) fn generate_dict_do_with_key(
        &mut self,
        receiver: &Expression,
        body: &Expression,
        selector: &str,
    ) -> Result<Document<'static>> {
        validate_block_arity_exact(
            body,
            2,
            "doWithKey:",
            "Fix: The body block must take two arguments (key and value):\n\
             \x20 dict doWithKey: [:key :value | key printString]",
        )?;

        // The one predicate every fold generator reads (`block_needs_mutation_threading`):
        // it also sees a write buried in a nested loop or fold, which a bare
        // `analyze_block` does not propagate out of the nested block.
        if let Some(body_block) = self.block_needs_mutation_threading(body) {
            return self.generate_dict_do_with_key_mutations(receiver, body_block, selector);
        }

        // No mutations: delegate to runtime helper
        Ok(Document::Nil)
    }

    /// The `{K, V}` pairs of the dictionary bound to `dict_var` that the keyed
    /// fold iterates: `maps:to_list/1` for a map literal, the receiver guard
    /// `beamtalk_map:pairs_for_threading/2` otherwise (it answers an untagged
    /// map's pairs and raises a structured `type_error` for `Ets`, a value-type
    /// instance, a user class or a non-map, instead of walking the wrong
    /// entries; the keyed counterpart of `lookup_for_threading/3`, BT-3841).
    fn keyed_pairs_doc(
        dict_var: &str,
        literal_receiver: bool,
        selector: &str,
    ) -> Document<'static> {
        if literal_receiver {
            docvec![
                "call 'maps':'to_list'(",
                leaf::var(dict_var.to_string()),
                ")"
            ]
        } else {
            docvec![
                "call 'beamtalk_map':'pairs_for_threading'(",
                leaf::var(dict_var.to_string()),
                ", ",
                leaf::atom(selector.to_string()),
                ")"
            ]
        }
    }

    /// Generates stateful `dictionary doWithKey:` using `lists:foldl` over `maps:to_list`.
    ///
    /// The foldl lambda receives `{K, V}` tuples. We destructure these inside the
    /// lambda body by extracting element(1, Pair) and element(2, Pair).
    #[allow(clippy::too_many_lines, clippy::similar_names)]
    pub(in crate::core_erlang) fn generate_dict_do_with_key_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
        selector: &str,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_foldl_list_op(self, body, ListOpKind::Do);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let dict_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let pairs_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let pair_var = self.fresh_temp_var("Pair");
        // The fold is inlined for a `Dictionary` only. A map literal is one; any
        // other receiver goes through the runtime guard (`Ets`, a value-type
        // instance or a user class defining the selector would run its own
        // method, which the inline fold must not replace). A receiver core
        // knows is not a `Dictionary` never gets here (the Phase 0 / §6 check
        // keeps its error).
        let literal_receiver = matches!(receiver.unwrap_parens(), Expression::MapLiteral { .. });

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
                leaf::var(dict_var.clone()),
                " = ",
                recv_code,
                " in let ",
                leaf::var(pairs_var.clone()),
                " = ",
                Self::keyed_pairs_doc(&dict_var, literal_receiver, selector),
                " in let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(pair_var.clone()),
                ", StateAcc) -> let ",
                leaf::var(key_var.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(pair_var.clone()),
                ") in let ",
                leaf::var(val_var.clone()),
                " = call 'erlang':'element'(2, ",
                leaf::var(pair_var),
                ") in ",
            ]);

            // BT-4121: the guarded-operator marker spans the body lowering.
            let prestore_state = self.current_field_read_state_var();
            self.push_guarded_op_loop(body, None);
            self.push_scope();
            if let Some(param) = body.parameters.first() {
                self.bind_var(&param.name, &key_var);
            }
            if let Some(param) = body.parameters.get(1) {
                self.bind_var(&param.name, &val_var);
            }
            let (body_doc, _) =
                self.generate_foldl_loop_body(body, &plan, &BodyKind::FoldlDo, "StateAcc", 0)?;
            docs.push(body_doc);
            self.pop_scope();
            // BT-4121: check out the marker; splice the pre-store (nil
            // unless the loop brackets with a `State` in scope).
            let (guarded_op_bracket, prestore) = self.checkout_guarded_op_loop_docs(
                self.pop_guarded_op_loop(),
                false,
                body.span,
                &prestore_state,
            );
            docs.insert(0, prestore);

            let fold_result = self.fresh_temp_var("FoldResult");
            let extract_doc = plan.generate_tuple_extract_suffix_doc(&fold_result, 1, self);
            let result_doc = if self.loop_mode.in_direct_params_loop {
                // see the identical branch in
                // `control_flow/list_ops/basic_ops.rs`'s `do:` — same shape here for a
                // dictionary iteration: multiple rebound accumulator vars, no single
                // "result" value, so signal open-with-no-value rather than naming one.
                self.loop_mode.direct_params_do_open_chain = true;
                docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(pairs_var),
                    ") in ",
                    extract_doc,
                ]
            } else {
                let (repack_doc, stateacc) =
                    plan.append_repack_stateacc_doc(self, guarded_op_bracket);
                docvec![
                    " in let ",
                    leaf::var(fold_result.clone()),
                    " = call 'lists':'foldl'(",
                    leaf::var(lambda_var),
                    ", ",
                    init_tuple_doc,
                    ", ",
                    leaf::var(pairs_var),
                    ") in ",
                    extract_doc,
                    repack_doc,
                    "{'nil', ",
                    leaf::var(stateacc),
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
            "let ",
            leaf::var(dict_var.clone()),
            " = ",
            recv_code,
            " in let ",
            leaf::var(pairs_var.clone()),
            " = ",
            Self::keyed_pairs_doc(&dict_var, literal_receiver, selector),
            " in let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(pair_var.clone()),
            ", ",
            leaf::var("StateAcc"),
            ") -> ",
            "let ",
            leaf::var(key_var.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(pair_var.clone()),
            ") in let ",
            leaf::var(val_var.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(pair_var),
            ") in ",
        ]);

        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &key_var);
        }
        if let Some(param) = body.parameters.get(1) {
            self.bind_var(&param.name, &val_var);
        }
        let (body_doc, _) =
            self.generate_foldl_loop_body(body, &plan, &BodyKind::FoldlDo, "StateAcc", 0)?;
        docs.push(body_doc);
        self.pop_scope();

        let fold_result = self.fresh_temp_var("FoldResult");
        let mut post_docs: Vec<Document<'static>> = vec![ThreadingPlan::foldl_call_doc(
            &lambda_var,
            leaf::var(init_state),
            &pairs_var,
            &fold_result,
        )];
        post_docs.push(plan.generate_extract_suffix_doc(&fold_result, self));

        post_docs.push(docvec!["{'nil', ", leaf::var(fold_result), "}",]);
        docs.push(Document::Vec(post_docs));

        Ok(Document::Vec(docs))
    }
}

#[cfg(test)]
mod tests {
    use crate::core_erlang::tests::codegen;

    #[test]
    fn test_dict_do_with_field_mutation_uses_maps_values_foldl() {
        // Map-literal do: with a field mutation generates maps:values + lists:foldl
        // for state threading (not a simple foreach, because the field must be updated).
        let src = concat!(
            "Actor subclass: Srv\n",
            "  state: n = 0\n\n",
            "  run: dict =>\n",
            "    #{#a => 1, #b => 2} do: [:v | self.n := self.n + v]\n",
        );
        let code = codegen(src);
        assert!(
            code.contains("'maps':'values'"),
            "dict do: with field mutation should use maps:values for iteration list. Got:\n{code}"
        );
        assert!(
            code.contains("'lists':'foldl'"),
            "dict do: with field mutation should use lists:foldl for state threading. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "dict do: body should update 'n' field via maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_dict_do_with_local_mutation_uses_tuple_acc() {
        // Map-literal do: with only a local variable mutation uses the tuple-accumulator
        // path: the threaded local is packed into a flat tuple {Total} as the
        // foldl accumulator instead of a StateAcc map, so element/2 reads it inside the
        // lambda. One maps:get appears outside the loop for the final extraction.
        let src = concat!(
            "Actor subclass: Srv\n",
            "  state: x = 0\n\n",
            "  run: dict =>\n",
            "    total := 0\n",
            "    #{#a => 1, #b => 2} do: [:v | total := total + v]\n",
            "    total\n",
        );
        let code = codegen(src);
        assert!(
            code.contains("'maps':'values'"),
            "dict do: with local mutation should still use maps:values. Got:\n{code}"
        );
        assert!(
            code.contains("'lists':'foldl'"),
            "dict do: with local mutation should use lists:foldl. Got:\n{code}"
        );
        // Tuple-accumulator: element(1, ...) reads the threaded local inside the lambda.
        assert!(
            code.contains("'erlang':'element'(1,"),
            "dict do: tuple-acc path should use element(1, ...) to read the threaded local. Got:\n{code}"
        );
        // Exactly one maps:get for the outer extraction after the loop (not inside the lambda).
        let get_count = code.matches("maps':'get'('__local__total'").count();
        let put_count = code.matches("maps':'put'('__local__total'").count();
        assert_eq!(
            get_count, 1,
            "dict do: tuple-acc path should have exactly 1 maps:get (outer extraction after loop). Got:\n{code}"
        );
        assert_eq!(
            put_count, 1,
            "dict do: tuple-acc path should have exactly 1 maps:put (repack after loop). Got:\n{code}"
        );
    }

    #[test]
    fn test_dict_do_nested_in_direct_params_loop_fed_directly_to_nlr_return() {
        // same shape as
        // control_flow::list_ops::tests::test_do_nested_in_direct_params_loop_fed_directly_to_nlr_return,
        // but for the dictionary `do:` producer (dict_ops.rs:104) rather than
        // list `do:` (basic_ops.rs:104) — both set the identical
        // `direct_params_do_open_chain` flag, but only the list-ops path had a
        // regression test pinning the fix (flagged by the Claude review bot
        // on the original PR). `^` (Expression::Return, via the NLR-throw
        // path) fed the direct result of a mutation-threaded dictionary
        // `do:` nested inside a direct-params loop must not reference the
        // old bare `"_"` sentinel as if it were a bound variable.
        let src = concat!(
            "Actor subclass: Ctr3053Dict\n",
            "  state: x = 0\n\n",
            "  run: dict =>\n",
            "    count := 0\n",
            "    seen := 0\n",
            "    1 to: 3 do: [:i |\n",
            "      count := count + 1\n",
            "      ^dict do: [:v | seen := seen + v]\n",
            "    ]\n",
            "    count\n",
        );
        let code = codegen(src);
        let nlr_throw_idx = code
            .find("call 'erlang':'throw'({'$bt_nlr'")
            .expect("expected an NLR throw call in generated code");
        let nlr_throw_window = &code[nlr_throw_idx..(nlr_throw_idx + 120).min(code.len())];
        assert!(
            !nlr_throw_window.contains(", _,"),
            "NLR throw tuple must not reference a bare unbound `_` as the return \
             value — the NoValue case must substitute 'nil'. Got window:\n{nlr_throw_window}\n\nFull:\n{code}"
        );
        // ADR 0131 Phase 2c (BT-3796): `do:` is a local-threading producer, so
        // the nested `do:` is lowered to its own `{nil, StateAcc}` carrier and
        // the throw's value is that carrier's element 1 (`do:` answers nil).
        assert!(
            nlr_throw_window.contains("'element'(1, _ThreadedResult"),
            "NLR throw tuple should carry the nested `do:` producer's value (element 1 of \
             its carrier). Got window:\n{nlr_throw_window}\n\nFull:\n{code}"
        );
    }

    #[test]
    fn test_dict_do_with_key_field_mutation_iterates_guarded_pairs_with_foldl() {
        // doWithKey: with a field mutation generates the guarded pairs + lists:foldl.
        // The foldl lambda receives {K, V} pairs extracted from the list.
        let src = concat!(
            "Actor subclass: Srv\n",
            "  state: n = 0\n\n",
            "  run: dict =>\n",
            "    dict doWithKey: [:k :v | self.n := self.n + v]\n",
        );
        let code = codegen(src);
        assert!(
            code.contains("'beamtalk_map':'pairs_for_threading'"),
            "doWithKey: with field mutation should iterate the receiver's pairs through the guard. Got:\n{code}"
        );
        assert!(
            code.contains("'lists':'foldl'"),
            "doWithKey: with field mutation should use lists:foldl for state threading. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "doWithKey: body should update 'n' field via maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_dict_do_with_key_destructures_pair_into_key_and_value() {
        // The doWithKey: lambda receives {K, V} pairs from maps:to_list.
        // Each pair is destructured: key = element(1, Pair), value = element(2, Pair).
        let src = concat!(
            "Actor subclass: Srv\n",
            "  state: n = 0\n\n",
            "  run: dict =>\n",
            "    dict doWithKey: [:k :v | self.n := self.n + v]\n",
        );
        let code = codegen(src);
        assert!(
            code.contains("call 'erlang':'element'(1,"),
            "doWithKey: lambda should extract the key via element(1, Pair). Got:\n{code}"
        );
        assert!(
            code.contains("call 'erlang':'element'(2,"),
            "doWithKey: lambda should extract the value via element(2, Pair). Got:\n{code}"
        );
    }

    #[test]
    fn test_dict_do_with_key_local_mutation_iterates_guarded_pairs_with_foldl() {
        // doWithKey: with a local variable mutation generates the guarded pairs + lists:foldl
        // and the lambda destructs each {K, V} pair via element/2 calls.
        let src = concat!(
            "Actor subclass: Srv\n",
            "  state: x = 0\n\n",
            "  run: dict =>\n",
            "    total := 0\n",
            "    dict doWithKey: [:k :v | total := total + v]\n",
            "    total\n",
        );
        let code = codegen(src);
        assert!(
            code.contains("'beamtalk_map':'pairs_for_threading'"),
            "doWithKey: with local mutation should iterate the receiver's pairs through the guard. Got:\n{code}"
        );
        assert!(
            code.contains("'lists':'foldl'"),
            "doWithKey: with local mutation should use lists:foldl. Got:\n{code}"
        );
        // Pair destructuring: element(1, Pair) for key, element(2, Pair) for value.
        assert!(
            code.contains("'erlang':'element'"),
            "doWithKey: lambda should use element/2 for {{K,V}} pair destructuring. Got:\n{code}"
        );
    }

    #[test]
    fn test_keys_and_values_do_is_alias_for_do_with_key_and_guards_the_receiver() {
        // keysAndValuesDo: is handled by the same code path as doWithKey:.
        // Both selectors must trigger the guarded pairs + lists:foldl for mutation blocks.
        let src_dwk = concat!(
            "Actor subclass: Srv\n",
            "  state: n = 0\n\n",
            "  run: dict =>\n",
            "    dict doWithKey: [:k :v | self.n := self.n + v]\n",
        );
        let src_kavd = concat!(
            "Actor subclass: Srv\n",
            "  state: n = 0\n\n",
            "  run: dict =>\n",
            "    dict keysAndValuesDo: [:k :v | self.n := self.n + v]\n",
        );
        let code_dwk = codegen(src_dwk);
        let code_kavd = codegen(src_kavd);
        assert!(
            code_kavd.contains("'beamtalk_map':'pairs_for_threading'"),
            "keysAndValuesDo: should use the same guarded pairs path as doWithKey:. Got:\n{code_kavd}"
        );
        assert!(
            code_dwk.contains("pairs_for_threading") && code_kavd.contains("pairs_for_threading"),
            "Both doWithKey: and keysAndValuesDo: must use pairs_for_threading. dwk:\n{code_dwk}\nkavd:\n{code_kavd}"
        );
    }

    #[test]
    fn test_dict_do_wrong_arity_block_is_compile_error() {
        // validate_block_arity_exact requires a 1-arg block; a 0-arg block must
        // propagate BlockArityError at compile time (covers line 36 of dict_ops.rs).
        let src = concat!(
            "Actor subclass: Ctr\n",
            "  state: x = 0\n\n",
            "  run =>\n",
            "    #{#a => 1} do: [nil]\n",
        );
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let result = crate::core_erlang::generate_module(
            &module,
            crate::core_erlang::CodegenOptions::new("test"),
        );
        assert!(
            matches!(
                result,
                Err(crate::core_erlang::CodeGenError::BlockArityError { .. })
            ),
            "dict do: with a 0-arg block must be a compile-time BlockArityError. Got: {result:?}"
        );
    }

    #[test]
    fn test_dict_do_with_key_wrong_arity_block_is_compile_error() {
        // validate_block_arity_exact requires a 2-arg block; a 1-arg block must
        // propagate BlockArityError at compile time (covers line 229 of dict_ops.rs).
        let src = concat!(
            "Actor subclass: Ctr\n",
            "  state: x = 0\n\n",
            "  run: dict =>\n",
            "    dict doWithKey: [:k | nil]\n",
        );
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let result = crate::core_erlang::generate_module(
            &module,
            crate::core_erlang::CodegenOptions::new("test"),
        );
        assert!(
            matches!(
                result,
                Err(crate::core_erlang::CodeGenError::BlockArityError { .. })
            ),
            "doWithKey: with a 1-arg block must be a compile-time BlockArityError. Got: {result:?}"
        );
    }

    #[test]
    fn test_dict_do_pure_no_mutation_falls_through_to_list_handler() {
        // Pure dict do: (no mutations) — generate_dict_do returns Document::Nil,
        // dispatcher returns Ok(None), falls through to the list handler which
        // calls generate_list_do → lists:foreach (covers lines 42-43, 46 of dict_ops.rs).
        let src = concat!(
            "Actor subclass: Srv\n",
            "  state: x = 0\n\n",
            "  run =>\n",
            "    #{#a => 1, #b => 2} do: [:v | v printString]\n",
        );
        let code = codegen(src);
        assert!(
            code.contains("'lists':'foreach'"),
            "pure dict do: falls through to list handler which uses lists:foreach. Got:\n{code}"
        );
        assert!(
            !code.contains("'maps':'values'"),
            "pure dict do: must NOT use maps:values (dict mutation-threading). Got:\n{code}"
        );
        assert!(
            !code.contains("'lists':'foldl'"),
            "pure dict do: must NOT use lists:foldl (mutation state threading). Got:\n{code}"
        );
    }

    #[test]
    fn test_dict_do_with_key_pure_no_mutation_falls_through_to_runtime() {
        // Pure doWithKey: (no mutations) — generate_dict_do_with_key returns Document::Nil,
        // dispatcher returns Ok(None) and falls through to runtime dispatch
        // (covers lines 235-236, 239 of dict_ops.rs).
        let src = concat!(
            "Actor subclass: Srv\n",
            "  state: x = 0\n\n",
            "  run: dict =>\n",
            "    dict doWithKey: [:k :v | k printString]\n",
        );
        let code = codegen(src);
        assert!(
            !code.contains("'maps':'to_list'"),
            "pure doWithKey: must NOT use maps:to_list (dict mutation-threading). Got:\n{code}"
        );
        assert!(
            !code.contains("'lists':'foldl'"),
            "pure doWithKey: must NOT use lists:foldl (mutation state threading). Got:\n{code}"
        );
    }

    #[test]
    fn test_dict_do_value_type_with_local_mutation_emits_nil_result() {
        // ValueType context + dict do: + local mutation uses the map-acc path in
        // generate_dict_do_with_mutations. When threaded_locals is non-empty and
        // context is ValueType, the result suffix is the atom 'nil' rather than
        // {'nil', FoldResult} (covers lines 204-205 of dict_ops.rs).
        let src = concat!(
            "Value subclass: V\n",
            "  state: dummy = 0\n\n",
            "  run =>\n",
            "    total := 0\n",
            "    #{#a => 1, #b => 2} do: [:v | total := total + v]\n",
        );
        let code = codegen(src);
        assert!(
            code.contains("'maps':'values'"),
            "ValueType dict do: with local mutation must use maps:values. Got:\n{code}"
        );
        assert!(
            code.contains("'lists':'foldl'"),
            "ValueType dict do: with local mutation must use lists:foldl. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__total'"),
            "ValueType dict do: map-acc path should extract local via maps:get. Got:\n{code}"
        );
    }

    #[test]
    fn test_dict_do_with_key_value_type_with_local_mutation_answers_the_carrier() {
        // ValueType context + doWithKey: + local mutation uses the map-acc path in
        // generate_dict_do_with_key_mutations. Like `do:` (BT-3796) the closed form
        // answers the `{nil, FoldResult}` carrier that the producer site unpacks
        // (BT-3841), not a bare `nil`.
        let src = concat!(
            "Value subclass: V\n",
            "  state: dummy = 0\n\n",
            "  run =>\n",
            "    total := 0\n",
            "    #{#a => 1, #b => 2} doWithKey: [:k :v | total := total + v]\n",
            "    total\n",
        );
        let code = codegen(src);
        assert!(
            code.contains("'maps':'to_list'"),
            "ValueType doWithKey: with local mutation must use maps:to_list. Got:\n{code}"
        );
        assert!(
            code.contains("'lists':'foldl'"),
            "ValueType doWithKey: with local mutation must use lists:foldl. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__total'"),
            "ValueType doWithKey: map-acc path should extract local via maps:get. Got:\n{code}"
        );
        assert!(
            code.contains("{'nil', _FoldResult"),
            "ValueType doWithKey: closed form answers the carrier. Got:\n{code}"
        );
    }

    #[test]
    fn test_keyed_fold_guards_a_non_literal_receiver_and_not_a_map_literal() {
        // BT-3841: the keyed fold is inlined for a `Dictionary` only. A map
        // literal is one (plain `maps:to_list`); any other receiver goes through
        // `beamtalk_map:pairs_for_threading/2`, which raises for `Ets`, a
        // value-type instance, a user class or a non-map instead of walking the
        // wrong entries.
        for (sel, arity_block) in [
            ("keysAndValuesDo:", "[:k :v | total := total + v]"),
            ("doWithKey:", "[:k :v | total := total + v]"),
        ] {
            for header in [
                "Value subclass: V\n  state: dummy = 0\n\n",
                "Actor subclass: V\n  state: dummy = 0\n\n",
            ] {
                let literal = format!(
                    "{header}  run =>\n    total := 0\n    #{{#a => 1}} {sel} {arity_block}\n    total\n"
                );
                let code = codegen(&literal);
                assert!(
                    code.contains("'maps':'to_list'") && !code.contains("pairs_for_threading"),
                    "a map literal is a Dictionary: no guard. Got:\n{code}"
                );
                let dynamic = format!(
                    "{header}  run: d =>\n    total := 0\n    d {sel} {arity_block}\n    total\n"
                );
                let code = codegen(&dynamic);
                assert!(
                    code.contains("'beamtalk_map':'pairs_for_threading'(")
                        && code.contains(&format!("'{sel}')"))
                        && !code.contains("'maps':'to_list'"),
                    "an unknown receiver goes through the guard with the selector. Got:\n{code}"
                );
            }
        }
    }
}
