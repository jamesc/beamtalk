// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Counted loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for counted loop constructs: `repeat`, and mutation-threading
//! variants of `timesRepeat:`, `to:do:`, and `to:by:do:`.
//! Non-mutating cases are handled by the pure-BT tail-recursive Integer methods (BT-1054).

use super::super::threaded_ir::{
    self, LoopCounter, ThreadedStmt, ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
};
use super::super::{CoreErlangGenerator, Result};
use super::plan::ThreadingPlan;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

// ─── CountedLoopFrame ─────────────────────────────────────────────────────────

/// Describes the loop-type-specific structure of a counted (`letrec`-based) loop.
///
/// Each `generate_*_with_mutations` for counted loops becomes a thin wrapper
/// that builds a `CountedLoopFrame` and calls `generate_counted_stateful_loop`.
///
/// ADR 0111 Addendum 15: `condition_prelude`/`condition_value` split out of
/// what was one opaque `continue_header` Document (`case <compare> of
/// <'true'> when 'true' -> `) so `generate_counted_stateful_loop*` can feed
/// them into a real `ThreadedStmt::ConditionalLoop` node — empty prelude and
/// a bare compare for `timesRepeat:`/`to:do:`/`to:by:do:`'s `Continue`-free
/// cases, the `let Continue = case ... end in ` sequence plus `Continue`
/// itself for `to:by:do:`. The continue arm itself (`<'true'> when 'true' ->
/// `) is always the same literal across every counted-loop kind (counted
/// loops never negate), so it is not a frame field.
pub(super) struct CountedLoopFrame {
    /// Variable bindings emitted before the `letrec` (e.g. `let N = recv in`).
    pub preamble: Document<'static>,
    /// Name of the letrec function (e.g. `"repeat"` or `"loop"`).
    pub fn_name: String,
    /// Statements that must run before `condition_value` is read — empty for
    /// `timesRepeat:`/`to:do:`/`to:by:do:` without a step sign check;
    /// `to:by:do:`'s `let Continue = case ... end in ` sequence otherwise.
    pub condition_prelude: Document<'static>,
    /// The condition's own scrutinee (a bare counter compare, or `Continue`).
    pub condition_value: Document<'static>,
    /// Expression used as the next counter in the recursive call
    /// (e.g. `"call 'erlang':'+'(I, 1)"`).
    pub next_counter: Document<'static>,
    /// Initial counter argument for the first `apply` call: an integer literal
    /// (e.g. `1`) for `timesRepeat:`, or a variable (e.g. `StartVar`) for `to:do:`.
    pub initial_counter: Document<'static>,
    /// Optional Beamtalk block-parameter name to bind to the gensym'd counter.
    pub body_param: Option<String>,
    /// BT-2354: gensym'd Core Erlang counter variable name (e.g. `_loopidx3`).
    ///
    /// Used as the loop fun's first parameter and threaded through
    /// `condition_value`/`next_counter`. Produced by `fresh_temp_var` (leading
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
    /// `apply`'s trailing argument — see [`extra_threaded_arg_doc`].
    pub class_var_param: Option<String>,
    /// BT-3484: the pre-loop value-type `Self` name (`current_self_var()`,
    /// captured before body generation runs), when the body threads a
    /// `self.field := ...` value-type mutation through the loop's own
    /// recursive tail call. `None` when it doesn't. The `SelfVt` mirror of
    /// `class_var_param`, used identically (fun formal parameter + exit-arm
    /// reference) and never set at the same time as it.
    pub self_param: Option<String>,
}

impl CountedLoopFrame {
    /// ADR 0111 Addendum 15: builds this frame's `ThreadedStmt::ConditionalLoop::counter`.
    fn loop_counter(&self) -> LoopCounter {
        LoopCounter::new(
            self.counter.clone(),
            self.initial_counter.clone(),
            self.next_counter.clone(),
        )
    }
}

/// BT-3168 (ADR 0111 Addendum 9, Question 3) / BT-3484: renders
/// `", <name>"` for a threaded extra fun-argument slot — the `ClassVars`
/// one (class-method loops) or the value-type `Self` one — or nothing when
/// the loop threads neither. Shared by `while_loops.rs`'s and
/// `counted_loops.rs`'s (via `generate_counted_stateful_loop`) Letrec
/// base-path plumbing: the letrec fun signature, both `apply` call sites,
/// and the exit arm all need the identical "extra trailing arg, or nothing"
/// shape, so it is written once rather than copy-evolved per call site.
///
/// At most ONE of the two slots is ever present on a given loop —
/// `ThreadingPlan::threads_class_vars` and `threads_value_self` are mutually
/// exclusive by construction (see
/// [`CoreErlangGenerator::loop_body_threads_value_self`]'s doc comment) — so
/// the threaded slot is always at tuple position 3, whichever it is.
pub(super) fn extra_threaded_arg_doc(name: Option<&String>) -> Document<'static> {
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
        // BT-3484: the `SelfVt` mirror — see `CountedLoopFrame::self_param`.
        let self_param = plan.threads_value_self.then(|| self.current_self_var());

        let frame = CountedLoopFrame {
            preamble: docvec![
                "let ",
                leaf::var(n_var.clone()),
                " = ",
                receiver_code,
                " in"
            ],
            fn_name: "repeat".to_string(),
            condition_prelude: Document::Nil,
            condition_value: docvec![
                "call 'erlang':'=<'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(n_var),
                ")",
            ],
            next_counter: docvec!["call 'erlang':'+'(", leaf::var(counter.clone()), ", 1)"],
            initial_counter: leaf::int_lit(1),
            body_param: None,
            counter,
            class_var_param,
            self_param,
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
        // BT-3484: the `SelfVt` mirror — see `CountedLoopFrame::self_param`.
        let self_param = plan.threads_value_self.then(|| self.current_self_var());

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
            condition_prelude: Document::Nil,
            condition_value: docvec![
                "call 'erlang':'=<'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(end_var),
                ")",
            ],
            next_counter: docvec!["call 'erlang':'+'(", leaf::var(counter.clone()), ", 1)"],
            initial_counter: leaf::var(start_var),
            body_param,
            counter,
            class_var_param,
            self_param,
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
        // BT-3484: the `SelfVt` mirror — see `CountedLoopFrame::self_param`.
        let self_param = plan.threads_value_self.then(|| self.current_self_var());

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
            condition_prelude: docvec![
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
                "end in ",
            ],
            condition_value: Document::Str("Continue"),
            next_counter: docvec![
                "call 'erlang':'+'(",
                leaf::var(counter.clone()),
                ", ",
                leaf::var(step_var),
                ")"
            ],
            initial_counter: leaf::var(start_var),
            body_param,
            counter,
            class_var_param,
            self_param,
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
    ///
    /// ADR 0111 Addendum 15: lowers to one `ThreadedStmt::ConditionalLoop`
    /// node — `produces` is `[State@0]` plus `ClassVars` (at its own live
    /// version, since `class_var_version` never resets across
    /// `with_branch_context`) when `frame.class_var_param` is set;
    /// `counter` carries `frame`'s own gensym'd index name plus its
    /// initial/next expressions (ADR 0111 Addendum 2 Gap 1).
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
        let cv_param_doc = extra_threaded_arg_doc(frame.class_var_param.as_ref());
        let class_var_seed_version = self.class_var_version();
        // BT-3484: the value-type `Self` mirror of the two lines above —
        // `self_version`, like `class_var_version`, is inherited (never
        // reset) across `with_branch_context`, so this names the identity
        // the loop body's own first `SelfVt` `Bind` will source from.
        let self_param_doc = extra_threaded_arg_doc(frame.self_param.as_ref());
        let self_seed_version = self.self_version();

        self.push_scope();

        // Bind the block counter param if any (e.g. to:do: [:i | ...] → bind "i" → counter)
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, &frame.counter);
        }

        // Unpack threaded locals at the top of each iteration. BT-3470 (ADR
        // 0111 Addendum 15): the returned docs are real
        // `let I = call 'maps':'get'(...) in` text that must render at the
        // top of the letrec fun's own body, before the condition — legacy
        // pushed them into the fun-body `docs` vec immediately before
        // `frame.continue_header` (see `Self::generate_counted_stateful_loop`'s
        // pre-ADR-0111-Addendum-15 history); dropping this return value
        // leaves a threaded local's condition read wired to its PRE-LOOP
        // identity forever, never advancing across iterations.
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);

        let condition_stmt = ThreadedStmt::Statement(frame.condition_prelude.clone(), body.span);
        let condition_value = ValueRef::Doc(frame.condition_value.clone());
        let condition: Vec<ThreadedStmt> = unpack_docs
            .into_iter()
            .map(|doc| ThreadedStmt::Statement(doc, body.span))
            .chain(std::iter::once(condition_stmt))
            .collect();

        let (mut body_stmts, ir_frame) = self.generate_letrec_body_ir(body, plan)?;

        self.pop_scope();

        let exit_arm = docvec![
            "<'false'> when 'true' -> {'nil', StateAcc",
            cv_param_doc,
            self_param_doc,
            "} end "
        ];

        let mut produces = vec![VersionedVar::new(VersionPrefix::State, 0, ir_frame)];
        if let Some(cv_name) = &frame.class_var_param {
            // ADR 0111 Addendum 15: see `Self::rebase_loop_seed`'s doc
            // comment for why this loop's own `ClassVars` `produces` entry
            // must be `Gensym`-seeded, not the method's live (possibly
            // nonzero) `ClassVars` version.
            let real_seed =
                VersionedVar::new(VersionPrefix::ClassVars, class_var_seed_version, ir_frame);
            let gensym_seed =
                VersionedVar::new(VersionPrefix::Gensym(cv_name.clone()), 0, ir_frame);
            Self::rebase_loop_seed(&mut body_stmts, &real_seed, &gensym_seed);
            produces.push(gensym_seed);
        }
        // BT-3484: identical treatment for the value-type `Self` slot —
        // mutually exclusive with the `ClassVars` one above, so at most one
        // of these two `produces` entries ever exists.
        if let Some(self_name) = &frame.self_param {
            let real_seed = VersionedVar::new(VersionPrefix::SelfVt, self_seed_version, ir_frame);
            let gensym_seed =
                VersionedVar::new(VersionPrefix::Gensym(self_name.clone()), 0, ir_frame);
            Self::rebase_loop_seed(&mut body_stmts, &real_seed, &gensym_seed);
            produces.push(gensym_seed);
        }
        // See `ConditionalLoop::outer_args`'s doc comment: the value
        // actually live at the call site for `produces[0]` is whatever
        // `generate_pack_prefix` produced above, never necessarily the
        // generic ambient-context "State" spelling its own derivation would
        // otherwise fall back to. A trailing `ClassVars` `produces` entry
        // (index 1, when present) needs no such override — it is already
        // `Gensym`-seeded above, and `Gensym` renders identically in every
        // context, so leaving `outer_args` one element short here
        // deliberately falls through to the SAME generic per-entry
        // derivation `render_loop_skeleton` uses for every other entry.
        let outer_args = vec![leaf::var(init_state)];

        let shadow_write_eligible = self.block_depth == 0;
        let ir = vec![ThreadedStmt::ConditionalLoop {
            fn_name: frame.fn_name.clone(),
            mode: ThreadingMode::StateAcc(plan.fallback_reason.clone()),
            frame: ir_frame,
            shadow_write_eligible,
            counter: Some(frame.loop_counter()),
            condition,
            condition_value,
            continue_arm: Document::Str("<'true'> when 'true' -> "),
            body: body_stmts,
            produces,
            outer_args: Some(outer_args),
            exit_arm,
            span: body.span,
        }];
        let errors = threaded_ir::verify(&ir);
        self.report_threaded_ir_verify_errors(
            &errors,
            "counted StateAcc ConditionalLoop",
            body.span,
        );
        let rendered = {
            let mut ctx = threaded_ir::RenderCtx::new(self);
            threaded_ir::render(&ir, &mut ctx)
        };

        Ok(docvec![pack_doc, frame.preamble.clone(), " ", rendered])
    }

    /// BT-1275: Direct-params variant of `generate_counted_stateful_loop`.
    ///
    /// Uses `fun (I, Var1, ..., VarN)` instead of `fun (I, StateAcc)`.
    /// The `StateAcc` map is rebuilt only once in the false (exit) arm.
    ///
    /// ADR 0111 Addendum 15: lowers to one `ThreadedStmt::ConditionalLoop`
    /// node — `produces` is each threaded local's `VersionPrefix::Local`
    /// seed; `counter` carries `frame`'s own gensym'd index.
    fn generate_counted_stateful_loop_direct(
        &mut self,
        frame: &CountedLoopFrame,
        body: &Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // Build the fun parameter list: (<counter>, Var1, ..., VarN)
        let param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();

        // Captured BEFORE `push_scope`/`generate_unpack_at_iteration_start`
        // rebind each threaded local to its generic fun-parameter name —
        // see `ThreadingPlan::initial_direct_args`'s doc comment for why the
        // OUTER call's own argument can differ from that generic name (e.g.
        // a method parameter's own gensym'd `Args`-pattern binding).
        let initial_direct_args = plan.initial_direct_args(self);

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

        let condition_stmt = ThreadedStmt::Statement(frame.condition_prelude.clone(), body.span);
        let condition_value = ValueRef::Doc(frame.condition_value.clone());

        // Body — set in_direct_params_loop so nested list ops skip StateAcc repack (BT-1329).
        let prev_direct_params_loop = self.loop_mode.in_direct_params_loop;
        self.loop_mode.in_direct_params_loop = true;
        let (body_stmts, ir_frame) = self.generate_letrec_body_ir(body, plan)?;
        self.loop_mode.in_direct_params_loop = prev_direct_params_loop;

        // Build exit StateAcc using the INITIAL param names (current iteration values).
        let exit_stateacc = plan.generate_exit_stateacc(&param_names, self);

        self.pop_scope();

        let exit_arm = docvec!["<'false'> when 'true' -> ", exit_stateacc, " end "];

        let produces: Vec<VersionedVar> = plan
            .threaded_locals
            .iter()
            .map(|name| VersionedVar::new(VersionPrefix::Local(name.clone()), 0, ir_frame))
            .collect();

        let shadow_write_eligible = self.block_depth == 0;
        let ir = vec![ThreadedStmt::ConditionalLoop {
            fn_name: frame.fn_name.clone(),
            mode: ThreadingMode::DirectParams,
            frame: ir_frame,
            shadow_write_eligible,
            counter: Some(frame.loop_counter()),
            condition: vec![condition_stmt],
            condition_value,
            continue_arm: Document::Str("<'true'> when 'true' -> "),
            body: body_stmts,
            produces,
            outer_args: Some(initial_direct_args.into_iter().map(leaf::var).collect()),
            exit_arm,
            span: body.span,
        }];
        let errors = threaded_ir::verify(&ir);
        self.report_threaded_ir_verify_errors(
            &errors,
            "counted direct-params ConditionalLoop",
            body.span,
        );
        let rendered = {
            let mut ctx = threaded_ir::RenderCtx::new(self);
            threaded_ir::render(&ir, &mut ctx)
        };

        Ok(docvec![frame.preamble.clone(), " ", rendered])
    }

    /// BT-1326/BT-1342: Full-extract variant of `generate_counted_stateful_loop`.
    ///
    /// Uses `fun (I, Var1, ..., VarN, RField1, ..., MField1, ...)` — locals, read-only fields,
    /// AND mutated fields as direct fun parameters. No `State` parameter.
    ///
    /// Field reads resolve to direct parameters. Field writes become simple variable
    /// rebindings (no `maps:put` per iteration). At loop exit, mutated fields are repacked
    /// into the initial State map.
    ///
    /// ADR 0111 Addendum 15: lowers to one `ThreadedStmt::ConditionalLoop`
    /// node — `produces` is locals then readonly then mutated fields
    /// (matching the param-list ordering below); `counter` carries `frame`'s
    /// own gensym'd index.
    fn generate_counted_stateful_loop_hybrid(
        &mut self,
        frame: &CountedLoopFrame,
        body: &Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
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
        let mutated_param_names: Vec<String> =
            mutated_params.iter().map(|(_, v)| v.clone()).collect();

        let all_field_params: std::collections::HashMap<String, String> = readonly_params
            .iter()
            .cloned()
            .chain(mutated_params.iter().cloned())
            .collect();

        // Captured BEFORE `push_scope`/`generate_unpack_at_iteration_start`
        // rebind each threaded local to its generic fun-parameter name —
        // see `ThreadingPlan::initial_direct_args`'s doc comment for why the
        // OUTER call's own argument can differ from that generic name (e.g.
        // a method parameter's own gensym'd `Args`-pattern binding). The
        // readonly/mutated field params need no such correction — their
        // fun-parameter name IS the pre-extracted temp var already, both
        // inside the loop and at the outer call site.
        let initial_local_args = plan.initial_direct_args(self);

        self.push_scope();

        // Bind block counter param if any (e.g. to:do: [:i | ...] → bind "i" → counter)
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, &frame.counter);
        }

        // Register local var bindings (no unpack docs emitted in hybrid mode
        // — structurally guaranteed by `generate_unpack_at_iteration_start`'s
        // own `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

        let condition_stmt = ThreadedStmt::Statement(frame.condition_prelude.clone(), body.span);
        let condition_value = ValueRef::Doc(frame.condition_value.clone());

        // BT-1326/BT-1342: Run body with hybrid field params active.
        let (body_stmts, ir_frame) =
            self.generate_letrec_hybrid_body_ir(body, plan, &all_field_params)?;

        // Exit StateAcc: uses initial param names (current iteration's starting values).
        // In the exit arm (false branch), the body hasn't executed, so params are unchanged.
        let exit_stateacc = plan.generate_exit_stateacc_full_extract(
            &local_param_names,
            &mutated_param_names,
            &initial_state,
            self,
        );

        self.pop_scope();

        let exit_arm = docvec!["<'false'> when 'true' -> ", exit_stateacc, " end "];

        let produces: Vec<VersionedVar> =
            plan.threaded_locals
                .iter()
                .map(|name| VersionedVar::new(VersionPrefix::Local(name.clone()), 0, ir_frame))
                .chain(readonly_params.iter().map(|(_, var)| {
                    VersionedVar::new(VersionPrefix::Gensym(var.clone()), 0, ir_frame)
                }))
                .chain(mutated_params.iter().map(|(_, var)| {
                    VersionedVar::new(VersionPrefix::Gensym(var.clone()), 0, ir_frame)
                }))
                .collect();

        let shadow_write_eligible = self.block_depth == 0;
        let ir = vec![ThreadedStmt::ConditionalLoop {
            fn_name: frame.fn_name.clone(),
            mode: ThreadingMode::Hybrid,
            frame: ir_frame,
            shadow_write_eligible,
            counter: Some(frame.loop_counter()),
            condition: vec![condition_stmt],
            condition_value,
            continue_arm: Document::Str("<'true'> when 'true' -> "),
            body: body_stmts,
            produces,
            outer_args: Some(
                initial_local_args
                    .into_iter()
                    .chain(readonly_params.iter().map(|(_, var)| var.clone()))
                    .chain(mutated_param_names.clone())
                    .map(leaf::var)
                    .collect(),
            ),
            exit_arm,
            span: body.span,
        }];
        let errors = threaded_ir::verify(&ir);
        self.report_threaded_ir_verify_errors(&errors, "counted hybrid ConditionalLoop", body.span);
        let rendered = {
            let mut ctx = threaded_ir::RenderCtx::new(self);
            threaded_ir::render(&ir, &mut ctx)
        };

        Ok(docvec![
            frame.preamble.clone(),
            Document::Vec(pre_extract_docs),
            " ",
            rendered,
        ])
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
