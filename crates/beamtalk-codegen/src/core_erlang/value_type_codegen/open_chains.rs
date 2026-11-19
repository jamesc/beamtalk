// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `*_open` lowerings: a non-last threading statement as an open let-chain whose rebinds scope over the rest of the method body (split from `body`, BT-5041).
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::control_flow::analysis::ThreadedFamilies;
use crate::core_erlang::control_flow::family_slots::{self};
use crate::core_erlang::control_flow::{BodyKind, ListOpKind, ThreadingPlan};
use crate::core_erlang::intrinsics::validate_block_arity_exact;
use crate::core_erlang::threaded_rebind::{RebindLayout, RebindNaming};
use crate::core_erlang::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::{Document, concat, leaf};
use beamtalk_core::ast::Expression;

impl CoreErlangGenerator {
    /// Generates an open Self-threading let chain for a non-last field assignment.
    ///
    /// For `self.field := value`, produces:
    /// ```erlang
    ///     let _Val = <value> in let Self{N} = call 'maps':'put'('field', _Val, Self{N-1}) in
    /// ```
    ///
    /// The trailing `in` leaves the let chain open so subsequent `body_parts` expressions
    /// flow as the continuation, keeping `Self{N}` in scope.
    ///
    /// # Reached by
    /// BT-5009: reached by a non-last `self.field := …` statement that
    /// `lower_root_statement_with_producer` declines, e.g. `setUp => self.col := EventCollector spawn`
    /// in `stdlib/test/actor_coordination_test.bt` (`ActorCoordinationTest`).
    pub(super) fn generate_vt_field_assignment_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        crate::core_erlang::reach::hit("generate_vt_field_assignment_open");
        if let Expression::Assignment { target, value, .. } = expr
            && let Expression::FieldAccess { field, .. } = target.as_ref()
        {
            let val_var = self.fresh_temp_var("Val");
            // Capture current Self BEFORE generating value so RHS field reads
            // (e.g., `self.x := self.x + 1`) see the current snapshot.
            let current_self = self.current_self_var();
            let val_doc = self.expression_doc(value)?;
            let new_self = self.next_self_var();
            return Ok(docvec![
                "    let ",
                leaf::var(val_var.clone()),
                " = ",
                val_doc,
                " in let ",
                leaf::var(new_self),
                " = call 'maps':'put'(",
                leaf::atom(field.name.clone()),
                ", ",
                leaf::var(val_var),
                ", ",
                leaf::var(current_self),
                ") in\n",
            ]);
        }
        // Fallback: sequence as a side-effecting expression
        let tmp_var = self.fresh_temp_var("seq");
        let expr_doc = self.expression_doc(expr)?;
        Ok(docvec![
            "    let ",
            leaf::var(tmp_var),
            " = ",
            expr_doc,
            " in\n",
        ])
    }

    /// Generate an open let-chain for a non-last `[block_withmutations] value`
    /// in `ValueType` context. Each assignment in the block body becomes a top-level
    /// `let Var = val in\n` so the mutation is visible to subsequent expressions.
    ///
    /// # Reached by
    /// BT-5009: reached only by a nested local-only block value inside a field-writing
    /// block value, `x := 0. [[x := x + 1] value. self.n := x] value. x`, pinned by
    /// `stdlib/test/bt4889nested_local_block_value_in_field_block_test.bt`. No corpus source
    /// other than that test hits it (BT-4889 measurement).
    pub(super) fn generate_vt_block_value_open(
        &mut self,
        expr: &Expression,
        mutations: &[String],
    ) -> Result<Document<'static>> {
        crate::core_erlang::reach::hit("generate_vt_block_value_open");
        let (block, arguments) = if let Expression::MessageSend {
            receiver,
            arguments,
            ..
        } = expr
        {
            if let Expression::Block(block) = receiver.as_ref() {
                (block, arguments.as_slice())
            } else {
                // Fallback: sequence as side effect
                let tmp_var = self.fresh_temp_var("seq");
                let doc = self.expression_doc(expr)?;
                return Ok(docvec!["    let ", leaf::var(tmp_var), " = ", doc, " in\n",]);
            }
        } else {
            let tmp_var = self.fresh_temp_var("seq");
            let doc = self.expression_doc(expr)?;
            return Ok(docvec!["    let ", leaf::var(tmp_var), " = ", doc, " in\n",]);
        };

        let mut parts: Vec<Document<'static>> = Vec::new();

        // Use a nested scope so block parameters and block-local variables
        // don't leak into the method scope. Only captured mutations are
        // re-bound in the outer scope after the block finishes.
        self.push_scope();

        // Bind block parameters to arguments (for value: variants)
        for (i, param) in block.parameters.iter().enumerate() {
            if i < arguments.len() {
                let arg_var = self.fresh_temp_var(&param.name);
                let arg_doc = self.expression_doc(&arguments[i])?;
                parts.push(docvec![
                    "    let ",
                    leaf::var(arg_var.clone()),
                    " = ",
                    arg_doc,
                    " in\n",
                ]);
                self.bind_var(&param.name, &arg_var);
            }
        }

        // Emit block body as open let-chains.
        // Track which variables are captured mutations so we can re-bind them
        // in the outer scope after popping the parameter scope.
        let mut rebound_vars: Vec<(String, String)> = Vec::new();

        let body = super::super::util::collect_body_exprs(&block.body);

        for body_expr in &body {
            if Self::is_local_var_assignment(body_expr) {
                if let Expression::Assignment { target, value, .. } = body_expr
                    && let Expression::Identifier(id) = target.as_ref()
                {
                    let core_var = self.resolve_local_core_var(&id.name);
                    let val_doc = self.expression_doc(value)?;
                    parts.push(docvec![
                        "    let ",
                        leaf::var(core_var.clone()),
                        " = ",
                        val_doc,
                        " in\n",
                    ]);
                    self.bind_var(&id.name, &core_var);
                    // Track for re-binding in outer scope
                    if mutations.iter().any(|m| m == id.name.as_str()) {
                        rebound_vars.push((id.name.to_string(), core_var));
                    }
                }
            } else {
                let tmp = self.fresh_temp_var("seq");
                let doc = self.expression_doc(body_expr)?;
                parts.push(docvec!["    let ", leaf::var(tmp), " = ", doc, " in\n",]);
            }
        }

        // Pop the block scope, then re-bind only captured mutations in the outer scope
        self.pop_scope();
        for (name, core_var) in &rebound_vars {
            self.bind_var(name, core_var);
        }

        Ok(Document::Vec(parts))
    }

    /// Generates a non-last `whileTrue:` / `whileFalse:` loop (with value-type
    /// captured local threading) as an **open let chain**.
    ///
    /// The while loop returns `{'nil', StateAcc}`. This method:
    /// 1. Binds the tuple result to a temp var
    /// 2. Extracts the `StateAcc` from element 2
    /// 3. Extracts each threaded local from the `StateAcc` via `maps:get`
    /// 4. Rebinds the variable names in scope so subsequent code sees the updated values
    ///
    /// # Reached by
    /// BT-5009: reached by a non-last `whileTrue:`/`whileFalse:` that rebinds an outer local,
    /// e.g. `Bt3746StatefulWhileValue>>run` in `stdlib/test/fixtures/bt3746stateful_while_value.bt`.
    pub(in crate::core_erlang) fn generate_vt_while_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        crate::core_erlang::reach::hit("generate_vt_while_open");
        // computed BEFORE `expression_doc` runs the loop's own codegen —
        // keeping it adjacent to the other pre-loop reads
        // (`get_while_threaded_locals`) matches the loop codegen's own read
        // order.
        let families = self.vt_loop_threaded_families(expr);
        // Generate the while loop expression (returns {'nil', StateAcc} tuple)
        let loop_doc = self.expression_doc(expr)?;
        let threaded_locals = self.get_while_threaded_locals(expr);
        Ok(self.emit_vt_loop_open_extraction(
            loop_doc,
            &threaded_locals,
            &families,
            expr.span(),
            "WhileResult",
            "WhileState",
        ))
    }

    /// Shared open-let-chain extraction for non-last value-type loops
    /// that return a `{'nil', StateAcc}` tuple (`whileTrue:`/`whileFalse:` and the counted
    /// loops `to:do:`/`to:by:do:`/`timesRepeat:`).
    ///
    /// 1. Binds the tuple result to a temp var
    /// 2. Extracts the `StateAcc` from element 2
    /// 3. Extracts each threaded local from the `StateAcc` via `maps:get`, rebinding the
    ///    variable names in scope so subsequent code sees the updated values
    /// 4. ADR 0111 Addendum 9, Question 3 / ADR 0122 Decision 3: when
    ///    `families` is non-empty, ALSO extracts the loop's trailing element
    ///    3 and rebinds it via [`Self::extract_vt_loop_family_slot`] —
    ///    matching the extra trailing tuple slot `while_loops.rs`/
    ///    `counted_loops.rs` append to the `{'nil', StateAcc, …}` result.
    ///    Empty for a fold unless it writes a field and is one of
    ///    `SELF_THREADING_FOLDS` (BT-3827: `do:`/`collect:`/`inject:into:`
    ///    return `Self` there through `FoldFamilySlots::close_tuple`);
    ///    `select:` and the other folds' accumulators have no such slot.
    ///
    /// When there are no threaded locals AND no extra family slot, falls
    /// back to sequencing the loop as a side effect (`let _seqN = <loop> in`).
    ///
    /// # Reached by
    /// BT-5009: every caller below, so the same inputs as
    /// [`Self::generate_vt_while_open`], [`Self::generate_vt_counted_loop_open`],
    /// [`Self::generate_vt_foldl_list_op_open`] and [`Self::generate_value_type_do_open`].
    pub(super) fn emit_vt_loop_open_extraction(
        &mut self,
        loop_doc: Document<'static>,
        threaded_locals: &[String],
        families: &ThreadedFamilies,
        span: beamtalk_core::source_analysis::Span,
        result_var_prefix: &str,
        state_var_prefix: &str,
    ) -> Document<'static> {
        crate::core_erlang::reach::hit("emit_vt_loop_open_extraction");
        if threaded_locals.is_empty() && families.as_slice().is_empty() {
            // Fallback: just sequence the expression
            let tmp = self.fresh_temp_var("seq");
            return docvec!["    let ", leaf::var(tmp), " = ", loop_doc, " in\n"];
        }

        let tuple_var = self.fresh_temp_var(result_var_prefix);

        let mut parts: Vec<Document<'static>> = Vec::new();

        // Bind the tuple result
        parts.push(docvec![
            "    let ",
            leaf::var(tuple_var.clone()),
            " = ",
            loop_doc,
            " in\n",
        ]);

        if !threaded_locals.is_empty() {
            let state_var = self.fresh_temp_var(state_var_prefix);

            // Extract StateAcc from element 2 of the tuple
            parts.push(docvec![
                "    let ",
                leaf::var(state_var.clone()),
                " = ",
                form::element(2, leaf::var(tuple_var.clone())),
                " in\n",
            ]);

            // Extract each threaded local from the state map
            parts.extend(self.rebind_threaded_locals(
                threaded_locals,
                |_, v| Self::local_state_key(v),
                &state_var,
                RebindNaming::FreshBound,
                RebindLayout::Line,
            ));
        }

        if let Some(rebind_doc) = self.extract_vt_loop_family_slot(&tuple_var, families, span) {
            parts.push(docvec!["    ", rebind_doc, "\n"]);
        }

        Document::Vec(parts)
    }

    /// Generates a non-last counted loop (`to:do:`/`to:by:do:`/`timesRepeat:`)
    /// with value-type captured local threading as an **open let chain**, extracting the
    /// threaded locals from the loop's `{'nil', StateAcc}` result (see
    /// [`Self::emit_vt_loop_open_extraction`]).
    ///
    /// # Reached by
    /// BT-5009: reached by a non-last `to:do:`/`timesRepeat:` that rebinds an outer local,
    /// e.g. `Bt3947InlineBlockLoopValue>>cell0` in `stdlib/test/fixtures/bt3947inline_block_loop_value.bt`.
    pub(in crate::core_erlang) fn generate_vt_counted_loop_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        crate::core_erlang::reach::hit("generate_vt_counted_loop_open");
        // see the analogous comment in `generate_vt_while_open`.
        let families = self.vt_loop_threaded_families(expr);
        // Generate the counted loop expression (returns {'nil', StateAcc} tuple).
        let loop_doc = self.expression_doc(expr)?;
        let threaded_locals = Self::counted_loop_body_block(expr)
            .map(|body| self.loop_threaded_locals(body, None))
            .unwrap_or_default();
        Ok(self.emit_vt_loop_open_extraction(
            loop_doc,
            &threaded_locals,
            &families,
            expr.span(),
            "CountedLoopResult",
            "CountedLoopState",
        ))
    }

    /// Generates a non-last foldl list-op (`collect:`/`select:`/`reject:`/
    /// `inject:into:`) with value-type captured local threading as an **open let chain**,
    /// extracting the threaded locals from the list-op's `{value, StateAcc}` result (see
    /// [`Self::emit_vt_loop_open_extraction`]).
    ///
    /// In non-last position the list-op's logical value (element 1) is discarded; only the
    /// threaded-local mutations need to escape.
    ///
    /// # Reached by
    /// BT-5009: reached by a non-last `collect:`/`select:`/`inject:into:` that rebinds an outer
    /// local or writes a field, e.g. `Bt3956OwnDoColl>>selfCollect` in
    /// `stdlib/test/fixtures/bt3956own_do_coll.bt`.
    pub(in crate::core_erlang) fn generate_vt_foldl_list_op_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        crate::core_erlang::reach::hit("generate_vt_foldl_list_op_open");
        // BT-3827: the `Self` family a field-writing fold carries at element 3.
        let families = self.vt_loop_threaded_families(expr);
        // Generate the list-op expression (returns a {value, StateAcc} tuple).
        let loop_doc = self.with_fold_self_slot_consumer(expr, |this| this.expression_doc(expr))?;
        let threaded_locals = Self::foldl_list_op_body_block(expr)
            .map(|_| self.producer_threaded_locals(expr))
            .unwrap_or_default();
        Ok(self.emit_vt_loop_open_extraction(
            loop_doc,
            &threaded_locals,
            &families,
            expr.span(),
            "FoldlListOpResult",
            "FoldlListOpState",
        ))
    }

    /// Generates a non-last `do:` loop (with value-type/class-method
    /// captured local threading) as an **open let chain**.
    ///
    /// Unlike the closed form (which buries the extracted locals inside `_seqN = (... 'nil')`)
    /// this form emits the foldl and the extractions (plain lets / `element/2` for the
    /// tuple/bare accumulator, `maps:get` for the map accumulator) directly in
    /// the method body, ending with `in ` so the caller's next `body_part` provides the
    /// continuation.  This makes the updated locals visible to all subsequent expressions.
    ///
    /// # Reached by
    /// BT-5009: reached by a non-last `do:` that rebinds an outer local, including two stdlib
    /// methods: `Behaviour>>superclassChain` (`stdlib/src/behaviour.bt`) and
    /// `List>>withoutDuplicatesBy:` (`stdlib/src/list.bt`). Core Erlang for both must stay
    /// byte-identical (`just core-diff`).
    #[allow(clippy::too_many_lines)] // Document-based foldl scaffolding spans many lines
    pub(in crate::core_erlang) fn generate_value_type_do_open(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        crate::core_erlang::reach::hit("generate_value_type_do_open");
        let (receiver, block_expr, body) = match expr {
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } if matches!(arguments.first(), Some(Expression::Block(_))) => {
                let block_arg = arguments.first().expect("checked above");
                let Some(Expression::Block(b)) = arguments.first() else {
                    return Ok(Document::Nil);
                };
                (receiver.as_ref(), block_arg, b)
            }
            _ => return Ok(Document::Nil),
        };

        // Validate that the block has exactly 1 parameter,
        // matching the check in the normal generate_list_do path.
        validate_block_arity_exact(
            block_expr,
            1,
            "do:",
            "The do: block must take exactly one argument: [:each | ...]",
        )?;

        // The fold's real plan (BT-4116: `new_for_foldl_list_op`, so it can pick
        // the tuple/bare accumulator). It drives either the tuple/bare open
        // form (`generate_value_type_do_open_tuple`) or the map open form
        // below; both emit the loop-convention diagnostic from it.
        let fold_plan = self.with_fold_self_slot_consumer(expr, |this| {
            ThreadingPlan::new_for_foldl_list_op(this, body, ListOpKind::Do)
                .with_value_self_slot(this, body)
        })?;
        let threaded_locals = fold_plan.threaded_locals.clone();

        // BT-4116: a body that threads only outer locals folds a bare value (one
        // local) or an N-tuple (several) — no map, no per-element `maps:get`/`put`.
        if fold_plan.use_tuple_acc {
            return self.generate_value_type_do_open_tuple(receiver, body, &fold_plan);
        }

        // Phase 1: create a fresh map and pack each captured local into it.
        let init_map_var = self.fresh_temp_var("InitMap");
        let mut pack_docs: Vec<Document<'static>> = vec![docvec![
            "let ",
            leaf::var(init_map_var.clone()),
            " = call 'maps':'new'() in ",
        ]];
        let mut current = init_map_var;
        for var_name in &threaded_locals {
            let packed_var = self.fresh_temp_var("Packed");
            let core_var = self.resolve_local_core_var(var_name);
            let key = Self::local_state_key(var_name);
            pack_docs.push(docvec![
                "let ",
                leaf::var(packed_var.clone()),
                " = call 'maps':'put'(",
                leaf::atom(key),
                ", ",
                leaf::var(core_var),
                ", ",
                leaf::var(current),
                ") in ",
            ]);
            current = packed_var;
        }
        let init_state_code = current;

        // Phase 2: generate receiver + list guard + lambda.
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);
        // BT-3827: a body that writes a field carries `Self` in a `{Acc, Self}`
        // wrapper around the accumulator (`generate_list_do_with_mutations`).
        let slots = fold_plan.fold_family_slots(self);

        let mut docs: Vec<Document<'static>> = vec![
            concat(pack_docs),
            docvec![
                super::super::control_flow::list_recv_to_safe_list_doc(
                    recv_code,
                    None,
                    list_var,
                    safe_list_var.clone(),
                ),
                "let ",
                leaf::var(lambda_var.clone()),
                " = fun (",
                leaf::var(item_var.clone()),
                ", ",
                leaf::var(
                    slots
                        .as_ref()
                        .map_or("StateAcc", family_slots::FoldFamilySlots::lambda_param)
                        .to_string()
                ),
                ") -> ",
                slots
                    .as_ref()
                    .map_or(Document::Nil, |s| s.lambda_prologue("StateAcc")),
            ],
        ];

        // Phase 3: generate foldl lambda body — ADR 0111 Addendum 15 (Foldl
        // migration): inlined replacement for the deleted
        // `generate_list_do_body_with_threading` compat shim, which built
        // its own second, throwaway `ThreadingPlan::new(self, body, None)`
        // (documented above as pure and computed identically to `fold_plan`)
        // purely to pass to `emit_loop_convention_diagnostic` — reusing
        // `fold_plan` here instead avoids that duplication while keeping the
        // diagnostic emitted exactly once, at this same relative point.
        self.emit_loop_convention_diagnostic(&fold_plan, body.span);
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        let (body_doc, _) =
            self.generate_foldl_loop_body(body, &fold_plan, &BodyKind::FoldlDo, "StateAcc", 0)?;
        self.pop_scope();
        docs.push(body_doc);

        // Phase 4: call foldl, then extract each local as an open `let X = ... in `.
        // These `let` bindings are emitted into the method body scope, not inside an
        // expression, so they shadow the pre-loop bindings and are visible to all
        // subsequent `body_parts`.
        let fold_result = self.fresh_temp_var("FoldResult");
        let init_acc = match &slots {
            Some(s) => s.wrap_init(leaf::var(init_state_code), self),
            None => leaf::var(init_state_code),
        };
        let mut post_docs: Vec<Document<'static>> = vec![ThreadingPlan::foldl_call_doc(
            &lambda_var,
            init_acc,
            &safe_list_var,
            slots.as_ref().map_or(
                fold_result.as_str(),
                family_slots::FoldFamilySlots::result_var,
            ),
        )];
        if let Some(s) = &slots {
            post_docs.push(s.unwrap_result(&fold_result));
        }
        // Binding the canonical names updates the scope so subsequent
        // method-body expressions look up these Erlang variables.
        post_docs.extend(self.rebind_threaded_locals(
            &threaded_locals,
            |_, v| Self::local_state_key(v),
            &fold_result,
            RebindNaming::Canonical,
            RebindLayout::Inline,
        ));
        // BT-3827: the open chain's `Self` is the fold's final one.
        if let Some(s) = &slots {
            for out in s.out_vars() {
                post_docs.push(self.rebind_value_self_from_doc(leaf::var(out.clone()), body.span));
                post_docs.push(Document::Str(" "));
            }
        }
        // Intentionally open — the caller's next body_part provides the continuation.
        docs.push(concat(post_docs));

        Ok(Document::Vec(docs))
    }

    /// BT-4116: the tuple/bare-accumulator form of
    /// [`Self::generate_value_type_do_open`]. The fold seed is the threaded
    /// locals' current values (the single value for a bare accumulator), the
    /// lambda unpacks them per element, and the post-fold chain rebinds each
    /// local as an open `let` for the caller's next body part.
    ///
    /// # Reached by
    /// BT-5009: reached from [`Self::generate_value_type_do_open`] whenever the `do:` threads
    /// more than one local (or a bare accumulator), e.g. `stdlib/test/fixtures/do_accumulator_value.bt`.
    fn generate_value_type_do_open_tuple(
        &mut self,
        receiver: &Expression,
        body: &beamtalk_core::ast::Block,
        fold_plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        crate::core_erlang::reach::hit("generate_value_type_do_open_tuple");
        let init_acc = fold_plan.initial_vars_tuple_doc(self);
        let acc_var = self.fresh_temp_var("AccSt");
        let list_var = self.fresh_temp_var("temp");
        let recv_code = self.expression_doc(receiver)?;
        let safe_list_var = self.fresh_temp_var("temp");
        let lambda_var = self.fresh_temp_var("temp");
        let item_param = body.parameters.first().map_or("_", |p| p.name.as_str());
        let item_var = Self::to_core_erlang_var(item_param);

        let mut docs: Vec<Document<'static>> = vec![docvec![
            super::super::control_flow::list_recv_to_safe_list_doc(
                recv_code,
                None,
                list_var,
                safe_list_var.clone(),
            ),
            "let ",
            leaf::var(lambda_var.clone()),
            " = fun (",
            leaf::var(item_var.clone()),
            ", ",
            leaf::var(acc_var.clone()),
            ") -> ",
        ]];

        self.emit_loop_convention_diagnostic(fold_plan, body.span);
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, &item_var);
        }
        let (body_doc, _) =
            self.generate_foldl_loop_body(body, fold_plan, &BodyKind::FoldlDo, &acc_var, 0)?;
        self.pop_scope();
        docs.push(body_doc);

        let fold_result = self.fresh_temp_var("FoldResult");
        docs.push(ThreadingPlan::foldl_call_doc(
            &lambda_var,
            init_acc,
            &safe_list_var,
            &fold_result,
        ));
        // The extraction rebinds each local under its current Core name (the
        // suffix reads it via `lookup_var`), shadowing the pre-loop binding for
        // every later body part.
        docs.push(fold_plan.generate_tuple_extract_suffix_doc(&fold_result, 1, self));
        // Intentionally open — the caller's next body_part provides the continuation.
        Ok(Document::Vec(docs))
    }
}
