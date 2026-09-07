// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Counted loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for counted loop constructs: `repeat`, and mutation-threading
//! variants of `timesRepeat:`, `to:do:`, and `to:by:do:`.
//! Non-mutating cases are handled by the pure-BT tail-recursive Integer methods (BT-1054).

use super::super::{CoreErlangGenerator, Result};
use super::plan::ThreadingPlan;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::join;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

// ─── CountedLoopFrame ─────────────────────────────────────────────────────────

/// Describes the loop-type-specific structure of a counted (`letrec`-based) loop.
///
/// Each `generate_*_with_mutations` for counted loops becomes a thin wrapper
/// that builds a `CountedLoopFrame` and calls `generate_counted_stateful_loop`.
///
/// ADR 0118 phase 3 (BT-3419) scope note: this is a plain `Document`-level
/// struct, unrelated to [`threaded_ir::ThreadedStmt::ConditionalLoop`]'s own
/// `condition`/`condition_value` fields, despite the field-name overlap
/// (`continue_header` here vs. that node's own, now-split `continue_arm`) —
/// `counted_loops.rs` never constructs a `ConditionalLoop` node (no counted
/// loop does; see that variant's own `#[allow(dead_code)]` status). A
/// counted loop's `continue_header` is always a pure counter compare (e.g.
/// `Counter =&lt; N`) built once from the receiver/limit/step, captured
/// before the letrec — never itself state-effecting — so had this frame
/// been unified onto `ConditionalLoop`, its `condition` would always be
/// empty and `condition_value` the bare compare, exactly the "pure counter
/// compare" case that variant's own field docs already name.
pub(super) struct CountedLoopFrame {
    /// Variable bindings emitted before the `letrec` (e.g. `let N = recv in`).
    pub preamble: Document<'static>,
    /// Name of the letrec function (e.g. `"repeat"` or `"loop"`).
    pub fn_name: String,
    /// The condition header up to `<'true'> when 'true' ->` for the continue arm.
    pub continue_header: Document<'static>,
    /// Expression used as the next counter in the recursive call
    /// (e.g. `"call 'erlang':'+'(I, 1)"`).
    pub next_counter: Document<'static>,
    /// Initial counter argument for the first `apply` call: an integer literal
    /// (e.g. `1`) for `timesRepeat:`, or a variable (e.g. `StartVar`) for `to:do:`.
    pub initial_counter: Document<'static>,
    /// The `false` arm and `end` (e.g. `"<'false'> when 'true' -> {'nil', StateAcc} end"`).
    pub false_arm: Document<'static>,
    /// Optional Beamtalk block-parameter name to bind to the gensym'd counter.
    pub body_param: Option<String>,
    /// BT-2354: gensym'd Core Erlang counter variable name (e.g. `_loopidx3`).
    ///
    /// Used as the loop fun's first parameter and threaded through
    /// `continue_header`/`next_counter`. Produced by `fresh_temp_var` (leading
    /// underscore + a monotonic counter), so it does not collide with the common
    /// case of a user local named `i`, which maps to `I` via `to_core_var`. The
    /// unique suffix also keeps it distinct from underscore-prefixed user
    /// identifiers (which `to_core_var` passes through verbatim).
    pub counter: String,
    /// BT-3168 (ADR 0111 Addendum 9, Question 3): the pre-loop `ClassVars`
    /// name (`current_class_var()`, captured before body generation runs),
    /// when the body threads a `ClassVars` mutation through the loop's own
    /// recursive tail call. `None` when it doesn't. Used, verbatim, as both
    /// the letrec fun's extra trailing formal parameter and the initial
    /// `apply`'s trailing argument — see [`class_var_arg_doc`].
    pub class_var_param: Option<String>,
}

/// BT-3168 (ADR 0111 Addendum 9, Question 3): renders `", <name>"` for a
/// threaded `ClassVars` fun-argument slot, or nothing when the loop doesn't
/// thread class vars. Shared by `while_loops.rs`'s and `counted_loops.rs`'s
/// (via `generate_counted_stateful_loop`) Letrec base-path `ClassVars`
/// plumbing — the letrec fun signature, both `apply` call sites, and the
/// exit arm all need the identical "extra trailing arg, or nothing" shape,
/// so it is written once rather than copy-evolved per call site.
pub(super) fn class_var_arg_doc(name: Option<&String>) -> Document<'static> {
    name.map_or(Document::Nil, |v| docvec![", ", leaf::var(v.clone())])
}

impl CoreErlangGenerator {
    pub(in crate::core_erlang) fn generate_repeat(
        &mut self,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // repeat: infinite loop - execute body forever
        // Generate: letrec '_LoopN'/0 = fun () ->
        //     let _BodyFun = <body> in
        //     let _ = apply _BodyFun& () in apply '_LoopN'/0 ()
        // in apply '_LoopN'/0 ()

        let loop_fn = self.fresh_temp_var("Loop");
        let body_var = self.fresh_temp_var("BodyFun");
        let body_code = self.expression_doc(body)?;
        Ok(docvec![
            "letrec ",
            leaf::fname(loop_fn.clone(), 0),
            " = fun () -> let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in let _ = apply ",
            leaf::var(body_var),
            " () in apply ",
            leaf::fname(loop_fn.clone(), 0),
            " () in apply ",
            leaf::fname(loop_fn, 0),
            " ()",
        ])
    }

    pub(in crate::core_erlang) fn generate_times_repeat_with_mutations(
        &mut self,
        receiver: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, None);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let n_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;
        // BT-2354: gensym the loop counter so a user local named `i` (→ Core `I`)
        // cannot collide with the loop fun parameter.
        let counter = self.fresh_temp_var("loopidx");

        // BT-3168 (ADR 0111 Addendum 9, Question 3): pre-loop ClassVars name,
        // captured before `generate_counted_stateful_loop` runs —
        // `with_branch_context` inherits (never resets) the outer
        // `class_var_version`, so this is both the letrec fun's own extra
        // trailing formal parameter and the exit arm's reference to it.
        let class_var_param = plan.threads_class_vars.then(|| self.current_class_var());
        let cv_arg = class_var_arg_doc(class_var_param.as_ref());

        let frame = CountedLoopFrame {
            preamble: docvec![
                "let ",
                leaf::var(n_var.clone()),
                " = ",
                receiver_code,
                " in"
            ],
            fn_name: "repeat".to_string(),
            continue_header: docvec![
                "case call 'erlang':'=<'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(n_var),
                ") of ",
                "<'true'> when 'true' -> ",
            ],
            next_counter: docvec!["call 'erlang':'+'(", leaf::var(counter.clone()), ", 1)"],
            initial_counter: leaf::int_lit(1),
            false_arm: docvec![
                "<'false'> when 'true' -> {'nil', StateAcc",
                cv_arg,
                "} ",
                "end "
            ],
            body_param: None,
            counter,
            class_var_param,
        };

        self.generate_counted_stateful_loop(&frame, body, &plan)
    }

    pub(in crate::core_erlang) fn generate_to_do_with_mutations(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, None);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let start_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;
        let end_var = self.fresh_temp_var("temp");
        let limit_code = self.expression_doc(limit)?;
        // BT-2354: gensym the loop counter (the block param is aliased to it).
        let counter = self.fresh_temp_var("loopidx");

        // Bind the block parameter name (e.g. "i" in [:i | ...])
        let body_param = body.parameters.first().map(|p| p.name.to_string());

        // BT-3168 (ADR 0111 Addendum 9, Question 3): see the analogous
        // comment in `generate_times_repeat_with_mutations`.
        let class_var_param = plan.threads_class_vars.then(|| self.current_class_var());
        let cv_arg = class_var_arg_doc(class_var_param.as_ref());

        let frame = CountedLoopFrame {
            preamble: docvec![
                "let ",
                leaf::var(start_var.clone()),
                " = ",
                receiver_code,
                " in let ",
                leaf::var(end_var.clone()),
                " = ",
                limit_code,
                " in",
            ],
            fn_name: "loop".to_string(),
            continue_header: docvec![
                "case call 'erlang':'=<'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(end_var),
                ") of ",
                "<'true'> when 'true' -> ",
            ],
            next_counter: docvec!["call 'erlang':'+'(", leaf::var(counter.clone()), ", 1)"],
            initial_counter: leaf::var(start_var),
            false_arm: docvec![
                "<'false'> when 'true' -> {'nil', StateAcc",
                cv_arg,
                "} ",
                "end "
            ],
            body_param,
            counter,
            class_var_param,
        };

        self.generate_counted_stateful_loop(&frame, body, &plan)
    }

    pub(in crate::core_erlang) fn generate_to_by_do_with_mutations(
        &mut self,
        receiver: &Expression,
        limit: &Expression,
        step: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, None);
        self.emit_loop_convention_diagnostic(&plan, body.span);

        let start_var = self.fresh_temp_var("temp");
        let receiver_code = self.expression_doc(receiver)?;
        let end_var = self.fresh_temp_var("temp");
        let limit_code = self.expression_doc(limit)?;
        let step_var = self.fresh_temp_var("temp");
        let step_code = self.expression_doc(step)?;
        // BT-2354: gensym the loop counter (the block param is aliased to it).
        let counter = self.fresh_temp_var("loopidx");

        let body_param = body.parameters.first().map(|p| p.name.to_string());

        // BT-3168 (ADR 0111 Addendum 9, Question 3): see the analogous
        // comment in `generate_times_repeat_with_mutations`.
        let class_var_param = plan.threads_class_vars.then(|| self.current_class_var());
        let cv_arg = class_var_arg_doc(class_var_param.as_ref());

        let frame = CountedLoopFrame {
            preamble: docvec![
                "let ",
                leaf::var(start_var.clone()),
                " = ",
                receiver_code,
                " in let ",
                leaf::var(end_var.clone()),
                " = ",
                limit_code,
                " in let ",
                leaf::var(step_var.clone()),
                " = ",
                step_code,
                " in",
            ],
            fn_name: "loop".to_string(),
            continue_header: docvec![
                "let Continue = case call 'erlang':'>'(",
                leaf::var(step_var.clone()),
                ", 0) of ",
                "<'true'> when 'true' -> call 'erlang':'=<'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(end_var.clone()),
                ") ",
                "<'false'> when 'true' -> ",
                "case call 'erlang':'<'(",
                leaf::var(step_var.clone()),
                ", 0) of ",
                "<'true'> when 'true' -> call 'erlang':'>='(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(end_var),
                ") ",
                "<'false'> when 'true' -> 'false' ",
                "end ",
                "end in case Continue of ",
                "<'true'> when 'true' -> ",
            ],
            next_counter: docvec![
                "call 'erlang':'+'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(step_var),
                ")"
            ],
            initial_counter: leaf::var(start_var),
            false_arm: docvec![
                "<'false'> when 'true' -> {'nil', StateAcc",
                cv_arg,
                "} ",
                "end "
            ],
            body_param,
            counter,
            class_var_param,
        };

        self.generate_counted_stateful_loop(&frame, body, &plan)
    }

    /// Generates a stateful counted loop using a `letrec`.
    ///
    /// Handles `timesRepeat:`, `to:do:`, and `to:by:do:` by accepting a `CountedLoopFrame`
    /// that captures the loop-type-specific preamble, condition, and step expression.
    ///
    /// In standard mode the fun signature is `(I, StateAcc)`.
    /// In direct-params mode (BT-1275, no field mutations) it is `(I, Var1, ..., VarN)`
    /// eliminating per-iteration `maps:get` / `maps:put` calls.
    pub(super) fn generate_counted_stateful_loop(
        &mut self,
        frame: &CountedLoopFrame,
        body: &Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        if plan.use_direct_params {
            return self.generate_counted_stateful_loop_direct(frame, body, plan);
        }
        if plan.use_hybrid_params {
            return self.generate_counted_stateful_loop_hybrid(frame, body, plan);
        }

        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        // BT-3168 (ADR 0111 Addendum 9, Question 3): an extra, explicit
        // trailing fun parameter when the body threads `ClassVars` — the
        // arity grows to 3 (`counter, StateAcc, ClassVars`) instead of 2.
        // `frame.class_var_param` was captured pre-loop by the
        // `counted_loops.rs` constructor that built this frame.
        let arity = if frame.class_var_param.is_some() {
            3
        } else {
            2
        };
        let cv_param_doc = class_var_arg_doc(frame.class_var_param.as_ref());

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(frame.preamble.clone());
        docs.push(docvec![
            " letrec ",
            leaf::fname(frame.fn_name.clone(), arity),
            " = fun (",
            leaf::var(frame.counter.clone()),
            ", StateAcc",
            cv_param_doc.clone(),
            ") -> ",
        ]);

        self.push_scope();

        // Bind the block counter param if any (e.g. to:do: [:i | ...] → bind "i" → counter)
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, &frame.counter);
        }

        // Unpack threaded locals at the top of each iteration
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        docs.extend(unpack_docs);

        // Condition + true arm
        docs.push(frame.continue_header.clone());

        // Body
        let (body_doc, final_state_version) =
            self.generate_threaded_loop_body(body, plan, &super::list_ops::BodyKind::Letrec)?;
        let final_class_var = self.last_loop_class_var.take();
        docs.push(body_doc);
        let final_state_var = super::super::util::versioned_var("StateAcc", final_state_version);
        let recur_cv_doc = final_class_var
            .as_ref()
            .map_or(Document::Nil, |v| docvec![", ", leaf::var(v.clone())]);

        self.pop_scope();

        // Recursive call + false arm + initial apply
        docs.push(docvec![
            " apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            frame.next_counter.clone(),
            ", ",
            leaf::var(final_state_var),
            recur_cv_doc,
            ") ",
            frame.false_arm.clone(),
            docvec![
                "in apply ",
                leaf::fname(frame.fn_name.clone(), arity),
                " (",
                frame.initial_counter.clone(),
                ", ",
                leaf::var(init_state),
                cv_param_doc,
                ")",
            ],
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1275: Direct-params variant of `generate_counted_stateful_loop`.
    ///
    /// Uses `fun (I, Var1, ..., VarN)` instead of `fun (I, StateAcc)`.
    /// The `StateAcc` map is rebuilt only once in the false (exit) arm.
    fn generate_counted_stateful_loop_direct(
        &mut self,
        frame: &CountedLoopFrame,
        body: &Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // Collect initial arg values from the outer scope (before push_scope overwrites them).
        let initial_direct_args = plan.initial_direct_args(self);

        // Build the fun parameter list: (<counter>, Var1, ..., VarN)
        let param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let arity = 1 + param_names.len();
        let param_list_doc = join(
            std::iter::once(leaf::var(frame.counter.clone()))
                .chain(param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(frame.preamble.clone());
        docs.push(docvec![
            " letrec ",
            leaf::fname(frame.fn_name.clone(), arity),
            " = fun (",
            param_list_doc,
            ") -> ",
        ]);

        self.push_scope();

        // Bind the block counter param if any (e.g. to:do: [:i | ...] → bind "i" → counter)
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, &frame.counter);
        }

        // Register var → param bindings (no unpack docs emitted in
        // direct-params mode — structurally guaranteed by
        // `generate_unpack_at_iteration_start`'s own
        // `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

        // Condition + true arm
        docs.push(frame.continue_header.clone());

        // Body — set in_direct_params_loop so nested list ops skip StateAcc repack (BT-1329).
        let prev_direct_params_loop = self.in_direct_params_loop;
        self.in_direct_params_loop = true;
        let (body_doc, _) =
            self.generate_threaded_loop_body(body, plan, &super::list_ops::BodyKind::Letrec)?;
        self.in_direct_params_loop = prev_direct_params_loop;
        docs.push(body_doc);

        // Collect final var names after body execution (updated bindings inside scope).
        let final_args: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| {
                self.lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect();

        // Build exit StateAcc using the INITIAL param names (current iteration values).
        let exit_stateacc = plan.generate_exit_stateacc(&param_names, self);

        self.pop_scope();

        // Build Document arg lists for the recursive call and the initial apply.
        let recursive_args_doc = join(
            std::iter::once(frame.next_counter.clone())
                .chain(final_args.into_iter().map(leaf::var)),
            &Document::Str(", "),
        );
        let initial_args_doc = join(
            std::iter::once(frame.initial_counter.clone())
                .chain(initial_direct_args.into_iter().map(leaf::var)),
            &Document::Str(", "),
        );

        // Recursive call + false arm (with rebuilt StateAcc) + initial apply.
        docs.push(docvec![
            " apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            recursive_args_doc,
            ") ",
            "<'false'> when 'true' -> ",
            exit_stateacc,
            " end ",
            "in apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            initial_args_doc,
            ")",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1326/BT-1342: Full-extract variant of `generate_counted_stateful_loop`.
    ///
    /// Uses `fun (I, Var1, ..., VarN, RField1, ..., MField1, ...)` — locals, read-only fields,
    /// AND mutated fields as direct fun parameters. No `State` parameter.
    ///
    /// Field reads resolve to direct parameters. Field writes become simple variable
    /// rebindings (no `maps:put` per iteration). At loop exit, mutated fields are repacked
    /// into the initial State map.
    fn generate_counted_stateful_loop_hybrid(
        &mut self,
        frame: &CountedLoopFrame,
        body: &Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // Collect initial arg values from the outer scope (before push_scope overwrites them).
        let initial_local_args = plan.initial_direct_args(self);
        let initial_state = plan.initial_state_var.clone();

        // Pre-extract ALL fields (readonly + mutated) before the letrec.
        // Each field is read once from the outer state via maps:get.
        // The leading space before "let" matches the surrounding Core Erlang formatting.
        let (pre_extract_docs, readonly_params, mutated_params) =
            self.pre_extract_hybrid_fields(plan, &initial_state, (" ", ""));

        // Fun param names: locals + readonly fields + mutated fields (NO State param).
        let local_param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let readonly_param_names: Vec<String> =
            readonly_params.iter().map(|(_, v)| v.clone()).collect();
        let mutated_param_names: Vec<String> =
            mutated_params.iter().map(|(_, v)| v.clone()).collect();
        let arity =
            1 + local_param_names.len() + readonly_param_names.len() + mutated_param_names.len();

        // Build param list doc: (<counter>, Var1, ..., VarN, RField1, ..., MField1, ...)
        let param_list_doc = join(
            std::iter::once(leaf::var(frame.counter.clone()))
                .chain(local_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(mutated_param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(frame.preamble.clone());
        docs.extend(pre_extract_docs);
        docs.push(docvec![
            " letrec ",
            leaf::fname(frame.fn_name.clone(), arity),
            " = fun (",
            param_list_doc,
            ") -> ",
        ]);

        self.push_scope();

        // Bind block counter param if any (e.g. to:do: [:i | ...] → bind "i" → counter)
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, &frame.counter);
        }

        // Register local var bindings (no unpack docs emitted in hybrid mode
        // — structurally guaranteed by `generate_unpack_at_iteration_start`'s
        // own `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

        // Condition + true arm
        docs.push(frame.continue_header.clone());

        // BT-1326/BT-1342: Run body with hybrid field params active; pop scope on error.
        let (body_doc, final_mutated_field_args) =
            self.run_counted_hybrid_body(body, plan, &readonly_params, &mutated_params)?;
        docs.push(body_doc);

        // Final local var args after body (updated bindings from scope).
        let final_local_args = self.collect_final_local_args(plan);

        // Exit StateAcc: uses initial param names (current iteration's starting values).
        // In the exit arm (false branch), the body hasn't executed, so params are unchanged.
        let exit_stateacc = plan.generate_exit_stateacc_full_extract(
            &local_param_names,
            &mutated_param_names,
            &initial_state,
            self,
        );

        self.pop_scope();

        Self::append_counted_hybrid_loop_tail(
            &mut docs,
            frame,
            arity,
            final_local_args,
            &readonly_param_names,
            &mutated_param_names,
            initial_local_args,
            final_mutated_field_args,
            exit_stateacc,
        );

        Ok(Document::Vec(docs))
    }

    /// Appends the recursive call, exit arm, and initial apply call to the counted hybrid loop docs.
    /// Executes the counted hybrid loop body with hybrid-mode field params active.
    ///
    /// Sets up `hybrid_readonly_field_params` and `hybrid_mutated_fields` from the
    /// pre-extracted params, runs the threaded body, captures final mutated field arg names,
    /// and restores all hybrid state. Calls `pop_scope` and returns an error if body fails.
    fn run_counted_hybrid_body(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        readonly_params: &[(String, String)],
        mutated_params: &[(String, String)],
    ) -> Result<(Document<'static>, Vec<String>)> {
        let prev_hybrid = self.in_hybrid_loop;
        let prev_direct_params_loop = self.in_direct_params_loop;
        let mut all_field_params: std::collections::HashMap<String, String> =
            readonly_params.iter().cloned().collect();
        for (field, var) in mutated_params {
            all_field_params.insert(field.clone(), var.clone());
        }
        let prev_readonly_field_params =
            std::mem::replace(&mut self.hybrid_readonly_field_params, all_field_params);
        let prev_mutated_fields = std::mem::replace(
            &mut self.hybrid_mutated_fields,
            plan.mutated_fields.iter().cloned().collect(),
        );
        self.in_hybrid_loop = true;
        self.in_direct_params_loop = true; // BT-1329: nested list ops skip StateAcc repack
        let body_result =
            self.generate_threaded_loop_body(body, plan, &super::list_ops::BodyKind::Letrec);

        // BT-1342: Capture final mutated field var names BEFORE restoring maps.
        let final_mutated_field_args: Vec<String> = plan
            .mutated_fields
            .iter()
            .map(|field| {
                self.hybrid_readonly_field_params
                    .get(field)
                    .cloned()
                    .unwrap_or_else(|| {
                        mutated_params
                            .iter()
                            .find(|(f, _)| f == field)
                            .map(|(_, v)| v.clone())
                            .unwrap_or_default()
                    })
            })
            .collect();

        self.hybrid_mutated_fields = prev_mutated_fields;
        self.hybrid_readonly_field_params = prev_readonly_field_params;
        self.in_hybrid_loop = prev_hybrid;
        self.in_direct_params_loop = prev_direct_params_loop;
        let (body_doc, _) = match body_result {
            Ok(result) => result,
            Err(err) => {
                self.pop_scope();
                return Err(err);
            }
        };
        Ok((body_doc, final_mutated_field_args))
    }

    #[allow(clippy::too_many_arguments)]
    fn append_counted_hybrid_loop_tail(
        docs: &mut Vec<Document<'static>>,
        frame: &CountedLoopFrame,
        arity: usize,
        final_local_args: Vec<String>,
        readonly_param_names: &[String],
        mutated_param_names: &[String],
        initial_local_args: Vec<String>,
        final_mutated_field_args: Vec<String>,
        exit_stateacc: Document<'static>,
    ) {
        // Recursive call args: next_counter, updated locals, readonly fields (unchanged), updated mutated fields
        let recursive_args_doc = join(
            std::iter::once(frame.next_counter.clone())
                .chain(final_local_args.into_iter().map(leaf::var))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(final_mutated_field_args.into_iter().map(leaf::var)),
            &Document::Str(", "),
        );

        // Initial apply args: initial_counter, initial locals, initial readonly vals, initial mutated vals
        let initial_args_doc = join(
            std::iter::once(frame.initial_counter.clone())
                .chain(initial_local_args.into_iter().map(leaf::var))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(mutated_param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        );

        docs.push(docvec![
            " apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            recursive_args_doc,
            ") ",
            "<'false'> when 'true' -> ",
            exit_stateacc,
            " end ",
            "in apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            initial_args_doc,
            ")",
        ]);
    }
}

#[cfg(test)]
mod tests {
    use crate::core_erlang::tests::codegen;

    #[test]
    fn test_repeat_generates_infinite_letrec_loop() {
        // repeat generates a letrec that loops forever (no condition check)
        let src = "Actor subclass: Server\n  state: x = 0\n\n  run =>\n    [42] repeat\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "repeat should generate a letrec. Got:\n{code}"
        );
        // repeat uses a simple loop function that recurses unconditionally
        assert!(
            !code.contains("case apply"),
            "repeat should NOT have a case on a condition. Got:\n{code}"
        );
    }

    #[test]
    fn test_times_repeat_with_field_mutation_threads_actor_state() {
        // timesRepeat: with field mutation generates state-threading loop with StateAcc
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    3 timesRepeat: [self.n := self.n + 1]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "timesRepeat: with mutation should generate a letrec. Got:\n{code}"
        );
        assert!(
            code.contains("StateAcc"),
            "timesRepeat: with mutation should thread state via StateAcc. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "timesRepeat: body should update 'n' via maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_to_do_with_field_mutation_generates_counted_loop_with_state() {
        // to:do: with field mutation generates a counted loop with state threading
        let src = "Actor subclass: Ctr\n  state: sum = 0\n\n  run =>\n    1 to: 5 do: [:i | self.sum := self.sum + i]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "to:do: with mutation should generate a letrec. Got:\n{code}"
        );
        assert!(
            code.contains("StateAcc"),
            "to:do: with mutation should thread state via StateAcc. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('sum'"),
            "to:do: body should update 'sum' via maps:put. Got:\n{code}"
        );
    }

    // ── BT-1275: direct-params optimisation ──────────────────────────────────

    #[test]
    fn test_to_do_local_var_only_uses_direct_params() {
        // to:do: with only local-var mutations must use direct fun params, not StateAcc map.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + i]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "to:do: with local mutation should generate a letrec. Got:\n{code}"
        );
        // Fun signature must be (<gensym'd counter>, Sum), not (_, StateAcc).
        // BT-2354: the counter is gensym'd (e.g. `_loopidx3`) so it can never
        // collide with a user local named `i` (→ Core `I`).
        assert!(
            code.contains("= fun (_loopidx") && code.contains(", Sum) ->"),
            "direct-params: letrec fun should have gensym'd counter + Sum params. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "direct-params: letrec fun must not use StateAcc signature. Got:\n{code}"
        );
        // At most one maps:put for the local variable (the exit StateAcc rebuild).
        // Per-iteration maps:put is eliminated.
        assert!(
            code.match_indices("maps':'put'('__local__sum'").count() == 1,
            "direct-params: exactly one maps:put for local sum (exit rebuild). Got:\n{code}"
        );
        // Rebuild at exit must be present.
        assert!(
            code.contains("ExitSA"),
            "direct-params: exit StateAcc must be rebuilt before returning. Got:\n{code}"
        );
    }

    #[test]
    fn test_times_repeat_local_var_only_uses_direct_params() {
        // timesRepeat: with local-var-only mutation uses direct params.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    3 timesRepeat: [sum := sum + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("= fun (_loopidx") && code.contains(", Sum) ->"),
            "timesRepeat: direct-params: letrec fun should have gensym'd counter + Sum. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "timesRepeat: direct-params: must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "timesRepeat: direct-params: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    #[test]
    fn test_to_by_do_local_var_only_uses_direct_params() {
        // to:by:do: with local-var-only mutation uses direct params.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    1 to: 10 by: 2 do: [:i | sum := sum + i]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("= fun (_loopidx") && code.contains(", Sum) ->"),
            "to:by:do: direct-params: letrec fun should have gensym'd counter + Sum. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "to:by:do: direct-params: must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "to:by:do: direct-params: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    // ── BT-1326/BT-1342: full-extract direct-params + field extraction ───────

    #[test]
    fn test_to_do_field_plus_local_mutation_uses_full_extract() {
        // BT-1342: When both field AND local vars are mutated, full-extract mode
        // extracts mutated fields to direct params (no State param in loop).
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + i. self.n := self.n + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "full-extract: should generate a letrec. Got:\n{code}"
        );
        // Fun signature must NOT have State — mutated field 'n' is a direct param.
        assert!(
            !code.contains(", Sum, State) ->"),
            "full-extract: letrec fun must not have State as param. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "full-extract: letrec fun must not use StateAcc signature. Got:\n{code}"
        );
        // Mutated field 'n' is pre-extracted before the letrec.
        assert!(
            code.contains("NField"),
            "full-extract: mutated field 'n' should be extracted as NField param. Got:\n{code}"
        );
        // Field 'n' is pre-extracted with maps:get('n') and repacked at exit.
        assert!(
            code.contains("maps':'get'('n'"),
            "full-extract: field 'n' should be pre-extracted via maps:get. Got:\n{code}"
        );
        // Exit arm packs mutated field AND locals back.
        // Verify the repack is inside the exit arm (near ExitSA), not just from
        // the trailing `self.n := sum` outside the loop.
        assert!(
            code.match_indices("maps':'put'('n'").count() >= 2,
            "full-extract: exit arm must repack 'n' (separate from post-loop assignment). Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('__local__sum'"),
            "full-extract: exit arm must pack sum into ExitSA. Got:\n{code}"
        );
    }

    #[test]
    fn test_times_repeat_field_plus_local_mutation_uses_full_extract() {
        // BT-1342: timesRepeat: with both field + local mutations uses full-extract mode.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    3 timesRepeat: [sum := sum + 1. self.n := self.n + 1]\n    self.n := sum\n";
        let code = codegen(src);
        // No State in fun signature.
        assert!(
            !code.contains(", Sum, State) ->"),
            "timesRepeat: full-extract: must not have State as param. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "timesRepeat: full-extract: must not use StateAcc signature. Got:\n{code}"
        );
        // Mutated field 'n' is a direct param.
        assert!(
            code.contains("NField"),
            "timesRepeat: full-extract: 'n' should be extracted as NField param. Got:\n{code}"
        );
        // Exit arm repacks 'n' (separate from post-loop `self.n := sum`).
        assert!(
            code.match_indices("maps':'put'('n'").count() >= 2,
            "timesRepeat: full-extract: exit arm must repack 'n'. Got:\n{code}"
        );
        // Exit arm packs locals back.
        assert!(
            code.contains("maps':'put'('__local__sum'"),
            "timesRepeat: full-extract: exit arm must pack sum into ExitSA. Got:\n{code}"
        );
    }

    #[test]
    fn test_to_by_do_field_plus_local_mutation_uses_full_extract() {
        // BT-1342: to:by:do: with both field + local mutations uses full-extract mode.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    1 to: 10 by: 2 do: [:i | sum := sum + i. self.n := self.n + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            !code.contains(", Sum, State) ->"),
            "to:by:do: full-extract: must not have State as param. Got:\n{code}"
        );
        assert!(
            !code.contains(", StateAcc) ->"),
            "to:by:do: full-extract: must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("NField"),
            "to:by:do: full-extract: 'n' should be extracted as NField param. Got:\n{code}"
        );
        // Exit arm repacks 'n' (separate from post-loop `self.n := sum`).
        assert!(
            code.match_indices("maps':'put'('n'").count() >= 2,
            "to:by:do: full-extract: exit arm must repack 'n'. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('__local__sum'"),
            "to:by:do: full-extract: exit arm must pack sum into ExitSA. Got:\n{code}"
        );
    }

    // ── BT-2308: value-type local threading for counted loops ────────────────

    #[test]
    fn test_value_type_to_do_write_only_threads_local() {
        // BT-2308: `[:i | last := i]` mutates an outer local write-only. In value-type
        // context this must thread `last` back via the {'nil', StateAcc} tuple even
        // though it is never read inside the block.
        let src = "Object subclass: Calc\n\n  run =>\n    last := 0\n    1 to: 5 do: [:i | last := i]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("CountedLoopResult"),
            "value-type to:do: should bind loop result to CountedLoopResult. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2,"),
            "value-type to:do: should extract state from element 2. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "value-type to:do: should extract write-only local 'last'. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_to_do_read_write_threads_local() {
        // BT-2308: read+write `[:i | sum := sum + i]` must thread `sum` back to the caller.
        let src = "Object subclass: Calc\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + i]\n    sum\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__sum'"),
            "value-type to:do: should extract 'sum' from state. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_times_repeat_write_only_threads_local() {
        // BT-2308: timesRepeat: with a write-only outer-local mutation threads it back.
        let src = "Object subclass: Calc\n\n  run =>\n    last := 0\n    3 timesRepeat: [last := 7]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "value-type timesRepeat: should extract write-only local 'last'. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_to_by_do_threads_local() {
        // BT-2308: to:by:do: threads outer-local mutations in value-type context.
        let src = "Object subclass: Calc\n\n  run =>\n    last := 0\n    1 to: 10 by: 2 do: [:i | last := i]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "value-type to:by:do: should extract write-only local 'last'. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_counted_loop_last_expr_unwraps_nil() {
        // BT-2308: a mutating counted loop as the method's LAST expression must return
        // the loop's logical value (element 1 = nil), not the raw {'nil', StateAcc} tuple.
        let src = "Object subclass: Calc\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + i]\n";
        let code = codegen(src);
        assert!(
            code.contains("element'(1,"),
            "last-expr counted loop should unwrap element 1 (nil). Got:\n{code}"
        );
    }

    #[test]
    fn test_to_do_readonly_field_pre_extracted_as_direct_param() {
        // BT-1326: When the loop body reads a field that it never writes, that field is
        // pre-extracted before the letrec and passed as a direct fun parameter.
        // Body reads self.step (readonly) and writes self.n + local sum (hybrid mode).
        let src = "Actor subclass: Ctr\n  state: n = 0\n  state: step = 1\n\n  run =>\n    sum := 0\n    1 to: 5 do: [:i | sum := sum + self.step. self.n := self.n + 1]\n    sum\n";
        let code = codegen(src);
        // Hybrid mode triggered (field write for n, local sum).
        assert!(
            !code.contains(", StateAcc) ->"),
            "readonly field: must not use StateAcc signature. Got:\n{code}"
        );
        // Readonly field pre-extracted before the letrec with maps:get.
        assert!(
            code.contains("maps':'get'('step'"),
            "readonly field: 'step' should be pre-extracted via maps:get. Got:\n{code}"
        );
        // The pre-extracted value used as a fun parameter (StepField1 or similar).
        assert!(
            code.contains("StepField"),
            "readonly field: fun should have a StepField param. Got:\n{code}"
        );
        // Inside the loop, self.step should NOT generate additional maps:get for 'step'.
        // The single maps:get is the pre-extraction; body uses the param directly.
        assert!(
            code.match_indices("maps':'get'('step'").count() == 1,
            "readonly field: exactly one maps:get for 'step' (pre-extraction only). Got:\n{code}"
        );
        // Mutable field 'n' still uses maps:put for writes.
        assert!(
            code.contains("maps':'put'('n'"),
            "readonly field: 'n' writes should still use maps:put. Got:\n{code}"
        );
    }
}

#[cfg(test)]
mod bt2363_nested_counted_loops {
    use crate::core_erlang::tests::codegen;

    // BT-2363: A nested `timesRepeat:` mutating an outer local must thread the inner
    // loop's `{value, StateAcc}` result back out through the OUTER loop. Previously the
    // outer loop was emitted as a plain `beamtalk_message_dispatch:send` (no threading)
    // and the inner mutation was dropped, returning 0 instead of the accumulated value.
    #[test]
    fn test_nested_times_repeat_threads_outer_local() {
        let src = "Object subclass: NestedRepro\n\n  run =>\n    total := 0\n    3 timesRepeat: [2 timesRepeat: [total := total + 1]]\n    total\n";
        let code = codegen(src);
        // Outer loop must thread via StateAcc (not direct-params), so it can unpack the
        // inner loop's tuple result.
        assert!(
            code.contains("fun (_loopidx") && code.contains(", StateAcc) ->"),
            "outer nested-loop must use StateAcc threading, not direct-params. Got:\n{code}"
        );
        // The inner loop's tuple result is unpacked via element(2, _NestTuple...) and the
        // updated StateAcc is fed to the next outer iteration.
        assert!(
            code.contains("_NestTuple"),
            "outer loop must bind the inner loop result to a _NestTuple var. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2, _NestTuple"),
            "outer loop must extract element(2) of the inner loop tuple. Got:\n{code}"
        );
        // `total` is packed/unpacked under the __local__ key throughout.
        assert!(
            code.contains("maps':'get'('__local__total'")
                && code.contains("maps':'put'('__local__total'"),
            "nested loop must thread 'total' under the __local__ key. Got:\n{code}"
        );
    }

    // BT-2363: write-only outer-local mutation inside a nested counted loop must also be
    // threaded back (the read+write detector alone misses write-only mutations).
    #[test]
    fn test_nested_times_repeat_write_only_outer_local_threaded() {
        let src = "Object subclass: NestedRepro\n\n  run =>\n    last := 0\n    3 timesRepeat: [2 timesRepeat: [last := 7]]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "write-only nested mutation of 'last' must be threaded. Got:\n{code}"
        );
    }

    // BT-2363 (Copilot review): the inner counted loop may be parenthesized
    // (`(2 timesRepeat: [...])`). The cross-scope mutation collectors must peel
    // parens or the write-only detection is silently skipped.
    #[test]
    fn test_nested_parenthesized_times_repeat_threaded() {
        let src = "Object subclass: NestedRepro\n\n  run =>\n    last := 0\n    3 timesRepeat: [(2 timesRepeat: [last := 7])]\n    last\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__last'"),
            "parenthesized nested mutation of 'last' must be threaded. Got:\n{code}"
        );
    }
}
