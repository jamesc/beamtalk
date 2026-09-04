// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! While loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for `whileTrue:` and `whileFalse:` loop constructs
//! with both pure and state-threading variants.
//!
//! ## BT-3163: `case apply CondFun(...) of <'true'>/<'false'>` needs no
//! explicit wildcard clause
//!
//! Every condition-dispatch `case` this file generates (`generate_while_simple`,
//! `generate_while_loop_with_mutations`, `generate_while_loop_direct`,
//! `generate_while_loop_hybrid`) matches only `<'true'>`/`<'false'>`, the
//! same non-exhaustive-to-the-compiler
//! shape [`super::super::CoreErlangGenerator::case_clause_fallback`] exists to
//! guard (see its doc comment and ADR 0111 Addendum 5, "Production bugs
//! found", bug 3 / BT-3161). Unlike BT-3161's two flavors, this one is **not
//! reachable**: the case is always the tail of a `letrec`-bound loop `fun`'s
//! own body, entered via `apply` — a genuinely separate BEAM function from
//! whatever function contains the loop expression (e.g. `dispatch/4`'s `try`
//! body, when the loop is a try's last statement). `beam_validator`'s
//! `ambiguous_catch_try_state` check tracks catch/try state per function, so
//! a non-exhaustive case belonging to a *different* function than the `try`
//! cannot trip it. Confirmed empirically (BT-3163 investigation): a
//! `[... whileTrue: [...]]` as an `ensure:`/`on:do:` try body's last
//! statement, with a field mutation earlier in the same try body (forcing
//! the inlined, non-closure `try` shape), still compiles cleanly through
//! `erlc`. No fallback added here; if a future refactor ever inlines one of
//! these `case`s directly into the same function as an enclosing `try`
//! (removing the `letrec`/`apply` boundary), reconsider this note.

use super::super::intrinsics::{STATEFUL_BLOCK_DISPATCH_HINT, validate_block_arity_exact};
use super::super::{CoreErlangGenerator, Result, block_analysis};
use super::{BodyKind, ThreadingPlan};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};
use beamtalk_core::ast::{Block, Expression};

/// Result of pre-extracting hybrid loop fields: pre-extraction docs, readonly params, mutated params.
///
/// Each param is a `(field_name, core_erlang_var)` pair.
type HybridFieldExtraction = (
    Vec<Document<'static>>,
    Vec<(String, String)>,
    Vec<(String, String)>,
);

impl CoreErlangGenerator {
    pub(in crate::core_erlang) fn generate_while_true(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 0-arg)
        validate_block_arity_exact(
            body,
            0,
            "whileTrue:",
            "Fix: The body block must take no arguments:\n\
             \x20 [x < 10] whileTrue: [x := x + 1]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            // BT-1329: Also check for nested list ops with cross-scope mutations
            let analysis = block_analysis::analyze_block(body_block);
            // ADR 0118 phase 3 (BT-3419): a condition-only self-send/field
            // write (`whileTrue: [nil]` with a mutating CONDITION) must also
            // route here — `needs_mutation_threading`/
            // `body_has_list_op_cross_scope_mutations` only look at the BODY,
            // so a trivial body previously fell through to
            // `generate_while_true_simple`, which compiles the condition via
            // `generate_expression` → `generate_block` → `generate_block_stateful`
            // (a real closure boundary that must CLOSE its own state chain
            // into a `{Result, State}` tuple) instead of the loop's own
            // frame — the exact shape that panics the verifier or crashes at
            // runtime (see the `bt3414_*_inside_while_true_condition_panics_verifier`
            // tests, `tests/gen_server.rs`).
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
                || super::condition_has_state_effects(condition)
            {
                return self.generate_while_true_with_mutations(condition, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_while_true_simple(condition, body)
    }

    pub(in crate::core_erlang) fn generate_while_true_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        self.generate_while_simple(condition, body, false)
    }

    pub(in crate::core_erlang) fn generate_while_true_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_while_loop_with_mutations(condition, body, false)
    }

    pub(in crate::core_erlang) fn generate_while_false(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 0-arg)
        validate_block_arity_exact(
            body,
            0,
            "whileFalse:",
            "Fix: The body block must take no arguments:\n\
             \x20 [x > 0] whileFalse: [x := x + 1]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            // BT-1329: Also check for nested list ops with cross-scope mutations
            let analysis = block_analysis::analyze_block(body_block);
            // ADR 0118 phase 3 (BT-3419): see the analogous comment in
            // `generate_while_true`.
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
                || super::condition_has_state_effects(condition)
            {
                return self.generate_while_false_with_mutations(condition, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_while_false_simple(condition, body)
    }

    pub(in crate::core_erlang) fn generate_while_false_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        self.generate_while_simple(condition, body, true)
    }

    /// Shared letrec loop structure for `whileTrue:` (negate=false) and `whileFalse:` (negate=true).
    ///
    /// Generates a recursive function:
    /// ```text
    /// letrec '_LoopN'/0 = fun () ->
    ///     let _CondFun = <condition> in
    ///     case apply _CondFun () of
    ///       <continue_atom> when 'true' -> let _BodyFun = <body> in
    ///                                      let _ = apply _BodyFun () in
    ///                                      apply '_LoopN'/0 ()
    ///       <exit_atom> when 'true' -> 'nil'
    ///     end
    /// in apply '_LoopN'/0 ()
    /// ```
    fn generate_while_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
        negate: bool,
    ) -> Result<Document<'static>> {
        let (continue_atom, exit_atom) = if negate {
            ("'false'", "'true'")
        } else {
            ("'true'", "'false'")
        };

        let loop_fn = self.fresh_temp_var("Loop");
        let cond_var = self.fresh_temp_var("CondFun");
        let cond_code = self.expression_doc(condition)?;
        let body_var = self.fresh_temp_var("BodyFun");
        let body_code = self.expression_doc(body)?;

        let doc = docvec![
            "letrec ",
            leaf::fname(loop_fn.clone(), 0),
            " = fun () -> let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_code,
            " in case apply ",
            leaf::var(cond_var),
            " () of <",
            continue_atom,
            "> when 'true' -> let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in let _ = apply ",
            leaf::var(body_var),
            " () in apply ",
            leaf::fname(loop_fn.clone(), 0),
            " () <",
            exit_atom,
            "> when 'true' -> 'nil' end in apply ",
            leaf::fname(loop_fn, 0),
            " ()",
        ];

        Ok(doc)
    }

    pub(in crate::core_erlang) fn generate_while_false_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_while_loop_with_mutations(condition, body, true)
    }

    /// Shared implementation for `whileTrue:` and `whileFalse:` stateful loops.
    ///
    /// `negate = false` → continue when condition is `'true'` (whileTrue:).
    /// `negate = true`  → continue when condition is `'false'` (whileFalse:).
    ///
    /// In direct-params mode (BT-1275, no field mutations) the fun signature is
    /// `(Var1, ..., VarN)` instead of `(StateAcc)`.
    #[allow(clippy::too_many_lines)] // state-threading while-loop codegen, ADR 0118 phase 3 (BT-3419) added the condition-effects branch
    fn generate_while_loop_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
        negate: bool,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, Some(condition));
        self.emit_loop_convention_diagnostic(&plan, body.span);

        if plan.use_direct_params {
            return self.generate_while_loop_direct(condition, body, &plan, negate);
        }
        if plan.use_hybrid_params {
            return self.generate_while_loop_hybrid(condition, body, &plan, negate);
        }

        let cond_var = self.fresh_temp_var("CondFun");
        // ADR 0118 phase 3 (BT-3419): whether the condition itself has state
        // effects (a self-send, or an `and:`/`or:`/`ifTrue:ifFalse:` that
        // carries one) — decides whether `CondFun` must return a
        // `{Bool, FinalStateAcc}` pair instead of a bare boolean, below.
        let cond_effects = super::condition_has_state_effects(condition);

        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        // BT-3168 (ADR 0111 Addendum 9, Question 3): when the body threads a
        // `ClassVars` mutation through the loop's own recursive tail call,
        // the letrec fun grows an extra, explicit trailing parameter —
        // `fun (StateAcc, ClassVars)`, never folded into `StateAcc`'s own
        // map. Captured before `generate_threaded_loop_body` runs:
        // `with_branch_context` inherits (never resets) the outer
        // `class_var_version`, so whatever name `current_class_var()`
        // reports here (bare "ClassVars" the first time a method mutates
        // one, "ClassVarsN" otherwise) is both the fun's own formal
        // parameter identifier and the initial `apply`'s argument.
        let class_var_param = plan.threads_class_vars.then(|| self.current_class_var());
        let arity = if class_var_param.is_some() { 2 } else { 1 };
        let cv_param_doc = super::class_var_arg_doc(class_var_param.as_ref());

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec![
            "letrec ",
            leaf::fname("while".to_string(), arity),
            " = fun (StateAcc",
            cv_param_doc.clone(),
            ") -> ",
        ]);

        // BT-598: At the start of each loop iteration, read threaded locals from StateAcc.
        // Use push_scope so bindings don't leak to caller after the letrec.
        self.push_scope();
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        docs.extend(unpack_docs);

        docs.push(docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (StateAcc) -> ",
        ]);

        // Generate condition inside branch context
        let cond_doc = self.with_branch_context(|this| {
            if let Expression::Block(cond_block) = condition {
                // BT-3151: this condition block bypasses `generate_block`'s own
                // self-send check by calling `generate_block_body`/
                // `generate_stateful_while_condition` directly — see
                // `check_no_unsafe_class_method_self_sends`'s doc comment.
                let analysis = crate::core_erlang::block_analysis::analyze_block(cond_block);
                this.check_no_unsafe_class_method_self_sends(&analysis, cond_block.span)?;
                if cond_effects {
                    this.generate_stateful_while_condition(cond_block)
                } else {
                    this.generate_block_body(cond_block)
                }
            } else if cond_effects {
                this.generate_stateful_while_condition_tail(condition)
            } else {
                this.generate_expression(condition)
            }
        })?;
        docs.push(cond_doc);

        // Condition application and true/false arm headers
        let cond_apply_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        if cond_effects {
            // ADR 0118 phase 3 (BT-3419): `CondFun` evaluates to
            // `{Bool, FinalStateAcc}` (see `generate_stateful_while_condition`),
            // never a bare boolean — unpack it and REBIND the literal name
            // `StateAcc` (shadowing the fun's own incoming parameter, the
            // same idiom `generate_while_loop_with_mutations`'s own
            // `and:`-with-mutations codegen already uses for
            // `let StateAcc = StateAcc in`) so the body compile below, the
            // recursive tail call, and the exit arm all transparently see
            // the condition's own state-effecting mutation under the SAME
            // name they already reference, with no further plumbing.
            let cond_pair_var = self.fresh_temp_var("CondPair");
            let cond_bool_var = self.fresh_temp_var("CondBool");
            docs.push(docvec![
                " in let ",
                leaf::var(cond_pair_var.clone()),
                " = apply ",
                leaf::var(cond_var),
                " (StateAcc) in let ",
                leaf::var(cond_bool_var.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(cond_pair_var.clone()),
                ") in let StateAcc = call 'erlang':'element'(2, ",
                leaf::var(cond_pair_var),
                ") in case ",
                leaf::var(cond_bool_var),
                " of ",
                cond_apply_arm,
            ]);
        } else {
            docs.push(docvec![
                " in case apply ",
                leaf::var(cond_var),
                " (StateAcc) of ",
                cond_apply_arm,
            ]);
        }

        let (body_doc, final_state_version) =
            self.generate_threaded_loop_body(body, &plan, &BodyKind::Letrec)?;
        let final_class_var = self.last_loop_class_var.take();
        docs.push(body_doc);
        let final_state_var = super::super::util::versioned_var("StateAcc", final_state_version);
        let recur_cv_doc = final_class_var
            .as_ref()
            .map_or(Document::Nil, |v| docvec![", ", leaf::var(v.clone())]);

        // BT-3168: the exit arm is reached WITHOUT running the body this
        // round (the condition check failed) — it must reference the fun's
        // own incoming `ClassVars` parameter (`class_var_param`, the SAME
        // text as the fun signature above), never the post-body
        // `final_class_var`.
        let exit_arm = if negate {
            docvec![
                "<'true'> when 'true' -> {'nil', StateAcc",
                cv_param_doc.clone(),
                "} "
            ]
        } else {
            docvec![
                "<'false'> when 'true' -> {'nil', StateAcc",
                cv_param_doc.clone(),
                "} "
            ]
        };
        docs.push(docvec![
            " apply ",
            leaf::fname("while".to_string(), arity),
            " (",
            leaf::var(final_state_var),
            recur_cv_doc,
            ") ",
            exit_arm,
            "end ",
        ]);

        // Pop scope to restore original bindings (before the letrec)
        self.pop_scope();

        // Initial call with packed state
        docs.push(docvec![
            "in apply ",
            leaf::fname("while".to_string(), arity),
            " (",
            leaf::var(init_state),
            cv_param_doc,
            ")",
        ]);

        Ok(Document::Vec(docs))
    }

    /// ADR 0118 phase 3 (BT-3419): compiles a `whileTrue:`/`whileFalse:`
    /// condition BLOCK whose tail has state effects (`condition_has_state_effects`)
    /// into a document that evaluates to `{BoolResult, FinalStateAcc}` —
    /// the shape `generate_while_loop_with_mutations`'s condition-application
    /// site can unpack uniformly, whether the state effect is a bare
    /// self-send nested in an ordinary expression (needs the ADR 0118
    /// sequencing rule via `threaded_expression` — see
    /// `generate_stateful_while_condition_tail`) or an `and:`/`or:`/
    /// `ifTrue:ifFalse:` construct the pre-existing mutation-threaded
    /// intrinsic path already compiles to its own `{Value, State}` pair.
    ///
    /// Non-tail statements (e.g. a plain local-var assignment ahead of the
    /// boolean, `[i := i + 1. <bool>]`) reuse the SAME per-statement
    /// dispatch every other block body already gets
    /// (`CoreErlangGenerator::classify_block_expr`/`generate_block_expr`,
    /// `generate_block_body_slice`'s own machinery) — the condition's own
    /// local writes thread exactly as before this phase; only the TAIL
    /// boolean gets the new `{Bool, State}` contract.
    fn generate_stateful_while_condition(
        &mut self,
        cond_block: &Block,
    ) -> Result<Document<'static>> {
        let filtered = super::super::util::collect_body_exprs(&cond_block.body);
        let Some((tail, rest)) = filtered.split_last() else {
            // Unreachable in practice — the parser requires at least a
            // boolean tail for a condition block — but mirrors
            // `generate_block_body_slice`'s own empty-body fallback rather
            // than panicking on a hand-built fixture.
            return Ok(Document::Str("{'true', StateAcc}"));
        };
        let mut docs: Vec<Document<'static>> = Vec::with_capacity(filtered.len());
        for expr in rest {
            if CoreErlangGenerator::is_local_var_assignment(expr) {
                // Mirrors `generate_threaded_loop_body_inner`'s own dispatch
                // for this exact statement shape: in base `StateAcc` mode
                // (the only mode a state-effecting condition ever runs
                // under — `condition_has_state_effects`'s two call sites
                // both force it), a local write must pack its new value
                // into `StateAcc` the same way a loop BODY statement's own
                // local write does — NOT the plain, unthreaded block-local
                // rebind `classify_block_expr`/`generate_block_expr` give a
                // genuine Tier 1 closure, which leaves the write invisible
                // to the next iteration's `maps:get` read (confirmed
                // empirically: `i` silently reset to its pre-loop value
                // every iteration, so the loop's own exit condition never
                // advances).
                let (doc, _val_var) = self.generate_local_var_assignment_in_loop(expr)?;
                docs.push(doc);
            } else {
                let kind = CoreErlangGenerator::classify_block_expr(self, expr, false);
                docs.push(self.generate_block_expr(expr, &kind)?);
            }
        }
        docs.push(self.generate_stateful_while_condition_tail(tail)?);
        Ok(Document::Vec(docs))
    }

    /// The tail (boolean) statement of a state-effecting while condition —
    /// see [`Self::generate_stateful_while_condition`]'s doc comment. Always
    /// produces `{BoolValue, FinalStateAcc}`.
    fn generate_stateful_while_condition_tail(
        &mut self,
        tail: &Expression,
    ) -> Result<Document<'static>> {
        if self.control_flow_has_mutations(tail) {
            // `and:`/`or:`/`ifTrue:ifFalse:` with a nested state effect:
            // the pre-existing mutation-threaded intrinsic path already
            // compiles this to a `{Value, FinalStateAcc}` case-expression
            // (confirmed against real compiled output — the loop's own
            // only threaded var in tail position is `StateAcc` itself, so
            // that pair IS already exactly the contract this function
            // promises). No further wrapping needed.
            return self.generate_expression(tail);
        }
        // A bare self-send (or one nested in an ordinary binary/keyword
        // send) NOT wrapped by `and:`/`or:`/`ifTrue:ifFalse:`: thread it
        // via the ADR 0118 sequencing rule so its mutation isn't silently
        // dropped (`threaded_expression`'s producer/sequencing rule — the
        // SAME mechanism an Actor method body statement already gets),
        // then wrap the result explicitly. The pure case (no state effects
        // at all — `condition_has_state_effects` looks at the whole block,
        // so a pure tail can still reach here when an EARLIER statement is
        // the effecting one) costs one no-op prelude and an unchanged
        // `StateAcc` reference.
        let frame = self.current_frame();
        let tv = self.threaded_expression(tail, frame)?;
        let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
        let value_doc = self.threaded_value_doc(&tv.value);
        let final_state_var = self.current_state_var();
        Ok(docvec![
            prelude_doc,
            "{",
            value_doc,
            ", ",
            leaf::var(final_state_var),
            "}"
        ])
    }

    /// The loop's condition body is ordinary AST-directed expression codegen
    /// with no state-threading content of its own. Factored out of
    /// `generate_while_loop_direct` (its only caller, BT-3182: the
    /// `ThreadedIr` while-direct pilot this was also shared with, ADR 0111
    /// Addendum 2/13, was deleted).
    fn generate_loop_condition_body(
        &mut self,
        condition: &Expression,
    ) -> Result<Document<'static>> {
        self.with_branch_context(|this| {
            if let Expression::Block(cond_block) = condition {
                // BT-3151: see the analogous check in `generate_while_loop`.
                let analysis = crate::core_erlang::block_analysis::analyze_block(cond_block);
                this.check_no_unsafe_class_method_self_sends(&analysis, cond_block.span)?;
                this.generate_block_body(cond_block)
            } else {
                this.generate_expression(condition)
            }
        })
    }

    /// BT-1275: Direct-params variant of `generate_while_loop_with_mutations`.
    ///
    /// Uses `fun (Var1, ..., VarN)` instead of `fun (StateAcc)`.
    /// The `StateAcc` map is rebuilt only once in the false (exit) arm.
    #[allow(clippy::too_many_lines)] // direct-params state-threading codegen, BT-1275
    fn generate_while_loop_direct(
        &mut self,
        condition: &Expression,
        body: &Block,
        plan: &ThreadingPlan,
        negate: bool,
    ) -> Result<Document<'static>> {
        // BT-3182: the `BEAMTALK_THREADED_IR_WHILE_DIRECT` pilot that used to
        // route eligible bodies through `ThreadedIr` here was deleted — see
        // ADR 0111 Addendum 13. This construct stays on side-channel
        // `ThreadedIr` verification only (BT-3132's checks still run against
        // every while/counted loop body below), same as before BT-3145.

        // Collect initial arg values from the outer scope (before push_scope).
        let initial_direct_args = plan.initial_direct_args(self);

        let param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let arity = param_names.len();
        let param_list_doc = || {
            join(
                param_names.iter().map(|v| leaf::var(v.clone())),
                &Document::Str(", "),
            )
        };

        let cond_var = self.fresh_temp_var("CondFun");

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(docvec![
            "letrec ",
            leaf::fname("while", arity),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        self.push_scope();
        // Register var bindings — no unpack docs in direct-params mode
        // (structurally guaranteed by `generate_unpack_at_iteration_start`'s
        // own `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

        // The condition closure captures the current vars from scope.
        // We pass only the params (not StateAcc) since there is no StateAcc.
        docs.push(docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        let cond_doc = self.generate_loop_condition_body(condition)?;
        docs.push(cond_doc);

        // Apply condition with current params.
        let case_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        docs.push(docvec![
            " in case apply ",
            leaf::var(cond_var),
            " (",
            param_list_doc(),
            ") of ",
            case_arm,
        ]);

        let (body_doc, _) = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec)?;
        docs.push(body_doc);

        // Collect final var names after body execution.
        let final_args = self.collect_final_local_args(plan);

        // Build exit StateAcc using the CURRENT iteration's param names.
        let exit_stateacc = plan.generate_exit_stateacc(&param_names, self);

        let final_args_doc = join(final_args.into_iter().map(leaf::var), &Document::Str(", "));
        let exit_arm = if negate {
            "<'true'> when 'true' -> "
        } else {
            "<'false'> when 'true' -> "
        };
        docs.push(docvec![
            " apply ",
            leaf::fname("while", arity),
            " (",
            final_args_doc,
            ") ",
            exit_arm,
            exit_stateacc,
            " end ",
        ]);

        self.pop_scope();

        let initial_args_doc = join(
            initial_direct_args.into_iter().map(leaf::var),
            &Document::Str(", "),
        );
        docs.push(docvec![
            "in apply ",
            leaf::fname("while", arity),
            " (",
            initial_args_doc,
            ")",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1326/BT-1342: Full-extract variant of `generate_while_loop_with_mutations`.
    ///
    /// Uses `fun (Var1, ..., VarN, RField1, ..., MField1, ...)` — locals, read-only fields,
    /// AND mutated fields as direct fun parameters. No `State` parameter.
    ///
    /// Field reads resolve to direct parameters. Field writes become simple variable
    /// rebindings (no `maps:put` per iteration). At loop exit, mutated fields are repacked
    /// into the initial State map.
    fn generate_while_loop_hybrid(
        &mut self,
        condition: &Expression,
        body: &Block,
        plan: &ThreadingPlan,
        negate: bool,
    ) -> Result<Document<'static>> {
        let initial_local_args = plan.initial_direct_args(self);
        let initial_state = plan.initial_state_var.clone();

        // Pre-extract ALL fields (readonly + mutated) before the letrec.
        let (pre_extract_docs, readonly_params, mutated_params) =
            self.pre_extract_hybrid_fields(plan, &initial_state, ("", " "));

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
            local_param_names.len() + readonly_param_names.len() + mutated_param_names.len();

        let param_list_doc = || {
            Self::build_hybrid_param_list(
                &local_param_names,
                &readonly_param_names,
                &mutated_param_names,
            )
        };

        let cond_var = self.fresh_temp_var("CondFun");

        let all_field_params = Self::build_field_params_map(&readonly_params, &mutated_params);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.extend(pre_extract_docs);
        docs.push(docvec![
            "letrec ",
            leaf::fname("while", arity),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        self.push_scope();
        // Register var bindings — no unpack docs in hybrid mode (structurally
        // guaranteed by `generate_unpack_at_iteration_start`'s own
        // `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

        docs.push(docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        let cond_doc = self.generate_hybrid_condition(condition, plan, &all_field_params)?;
        docs.push(cond_doc);

        let case_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        docs.push(docvec![
            " in case apply ",
            leaf::var(cond_var),
            " (",
            param_list_doc(),
            ") of ",
            case_arm,
        ]);

        let (body_doc, final_mutated_field_args) =
            self.generate_hybrid_loop_body(body, plan, &all_field_params, &mutated_params)?;
        docs.push(body_doc);

        let final_local_args = self.collect_final_local_args(plan);
        let exit_stateacc = plan.generate_exit_stateacc_full_extract(
            &local_param_names,
            &mutated_param_names,
            &initial_state,
            self,
        );

        Self::append_hybrid_loop_tail(
            &mut docs,
            negate,
            arity,
            &final_local_args,
            &readonly_param_names,
            final_mutated_field_args,
            exit_stateacc,
        );

        self.pop_scope();

        Self::append_hybrid_initial_call(
            &mut docs,
            "while",
            arity,
            initial_local_args,
            &readonly_param_names,
            &mutated_param_names,
        );

        Ok(Document::Vec(docs))
    }

    /// Pre-extracts all fields (readonly + mutated) from the state map into local variables.
    ///
    /// Returns the pre-extraction docs, readonly params (field, var) pairs, and mutated params.
    /// The `let_wrap` pair controls formatting before `let` and after `in`.
    pub(super) fn pre_extract_hybrid_fields(
        &mut self,
        plan: &ThreadingPlan,
        initial_state: &str,
        let_wrap: (&'static str, &'static str),
    ) -> HybridFieldExtraction {
        let (prefix, suffix) = let_wrap;
        let mut pre_extract_docs: Vec<Document<'static>> = Vec::new();

        let extract_field =
            |docs: &mut Vec<Document<'static>>, codegen: &mut Self, field: &str| -> String {
                let var_name = codegen.fresh_temp_var(&format!(
                    "{}Field",
                    CoreErlangGenerator::to_core_erlang_var(field)
                ));
                docs.push(docvec![
                    prefix,
                    "let ",
                    leaf::var(var_name.clone()),
                    " = call 'maps':'get'(",
                    leaf::atom(field.to_string()),
                    ", ",
                    leaf::var(initial_state.to_string()),
                    ") in",
                    suffix,
                ]);
                var_name
            };

        let readonly_params: Vec<(String, String)> = plan
            .readonly_fields
            .iter()
            .map(|field| {
                let var_name = extract_field(&mut pre_extract_docs, self, field);
                (field.clone(), var_name)
            })
            .collect();

        let mutated_params: Vec<(String, String)> = plan
            .mutated_fields
            .iter()
            .map(|field| {
                let var_name = extract_field(&mut pre_extract_docs, self, field);
                (field.clone(), var_name)
            })
            .collect();

        (pre_extract_docs, readonly_params, mutated_params)
    }

    /// Builds a combined field params map from readonly and mutated params.
    fn build_field_params_map(
        readonly_params: &[(String, String)],
        mutated_params: &[(String, String)],
    ) -> std::collections::HashMap<String, String> {
        let mut all_field_params: std::collections::HashMap<String, String> =
            readonly_params.iter().cloned().collect();
        for (field, var) in mutated_params {
            all_field_params.insert(field.clone(), var.clone());
        }
        all_field_params
    }

    /// Builds the param list document for hybrid mode: `(Var1, ..., VarN, RField1, ..., MField1, ...)`.
    fn build_hybrid_param_list(
        local_param_names: &[String],
        readonly_param_names: &[String],
        mutated_param_names: &[String],
    ) -> Document<'static> {
        join(
            local_param_names
                .iter()
                .map(|v| leaf::var(v.clone()))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(mutated_param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        )
    }

    /// Generates the condition expression within hybrid loop context.
    ///
    /// Sets up hybrid field params, generates the condition, and restores state on exit.
    fn generate_hybrid_condition(
        &mut self,
        condition: &Expression,
        plan: &ThreadingPlan,
        all_field_params: &std::collections::HashMap<String, String>,
    ) -> Result<Document<'static>> {
        let prev_readonly_field_params = std::mem::replace(
            &mut self.hybrid_readonly_field_params,
            all_field_params.clone(),
        );
        let prev_mutated_fields = std::mem::replace(
            &mut self.hybrid_mutated_fields,
            plan.mutated_fields.iter().cloned().collect(),
        );
        let cond_result = self.with_branch_context(|this| {
            let prev_hybrid = this.in_hybrid_loop;
            this.in_hybrid_loop = true;
            let result = if let Expression::Block(cond_block) = condition {
                // BT-3151: see the analogous check in `generate_while_loop`.
                let analysis = crate::core_erlang::block_analysis::analyze_block(cond_block);
                this.check_no_unsafe_class_method_self_sends(&analysis, cond_block.span)
                    .and_then(|()| this.generate_block_body(cond_block))
            } else {
                this.generate_expression(condition)
            };
            this.in_hybrid_loop = prev_hybrid;
            result
        });

        self.hybrid_readonly_field_params = prev_readonly_field_params;
        self.hybrid_mutated_fields = prev_mutated_fields;
        cond_result
    }

    /// Generates the body of a hybrid loop and captures final mutated field var names.
    ///
    /// Returns the body document and the final mutated field argument names.
    /// Saves and restores all hybrid loop state (`in_hybrid_loop`, `in_direct_params_loop`,
    /// `hybrid_readonly_field_params`, `hybrid_mutated_fields`).
    pub(super) fn generate_hybrid_loop_body(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        all_field_params: &std::collections::HashMap<String, String>,
        mutated_params: &[(String, String)],
    ) -> Result<(Document<'static>, Vec<String>)> {
        let prev_hybrid = self.in_hybrid_loop;
        let prev_direct_params_loop = self.in_direct_params_loop;
        let prev_readonly_field_params = std::mem::replace(
            &mut self.hybrid_readonly_field_params,
            all_field_params.clone(),
        );
        let prev_mutated_fields = std::mem::replace(
            &mut self.hybrid_mutated_fields,
            plan.mutated_fields.iter().cloned().collect(),
        );
        self.in_hybrid_loop = true;
        self.in_direct_params_loop = true;
        let body_result = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec);

        // BT-1342: Capture final mutated field var names BEFORE restoring maps.
        let final_mutated_field_args: Vec<String> = plan
            .mutated_fields
            .iter()
            .map(|field| {
                self.hybrid_readonly_field_params
                    .get(field)
                    .cloned()
                    .unwrap_or_else(|| {
                        mutated_params.iter().find(|(f, _)| f == field).map_or_else(
                            || {
                                unreachable!(
                                    "hybrid while: missing mutated field mapping for `{field}`"
                                )
                            },
                            |(_, v)| v.clone(),
                        )
                    })
            })
            .collect();

        self.hybrid_readonly_field_params = prev_readonly_field_params;
        self.hybrid_mutated_fields = prev_mutated_fields;
        self.in_hybrid_loop = prev_hybrid;
        self.in_direct_params_loop = prev_direct_params_loop;
        let (body_doc, _) = body_result?;
        Ok((body_doc, final_mutated_field_args))
    }

    /// Collects the current Core Erlang variable names for each threaded local after the body executes.
    ///
    /// Uses the current scope bindings; falls back to the canonical `to_core_erlang_var` name
    /// if the variable has not been rebound in this iteration.
    pub(super) fn collect_final_local_args(&self, plan: &ThreadingPlan) -> Vec<String> {
        plan.threaded_locals
            .iter()
            .map(|v| {
                self.lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect()
    }

    /// Appends the recursive call and exit arm to the while loop docs.
    #[allow(clippy::too_many_arguments)]
    fn append_hybrid_loop_tail(
        docs: &mut Vec<Document<'static>>,
        negate: bool,
        arity: usize,
        final_local_args: &[String],
        readonly_param_names: &[String],
        final_mutated_field_args: Vec<String>,
        exit_stateacc: Document<'static>,
    ) {
        let final_args_doc = join(
            final_local_args
                .iter()
                .map(|v| leaf::var(v.clone()))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(final_mutated_field_args.into_iter().map(leaf::var)),
            &Document::Str(", "),
        );
        let exit_arm = if negate {
            "<'true'> when 'true' -> "
        } else {
            "<'false'> when 'true' -> "
        };
        docs.push(docvec![
            " apply ",
            leaf::fname("while", arity),
            " (",
            final_args_doc,
            ") ",
            exit_arm,
            exit_stateacc,
            " end ",
        ]);
    }

    /// BT-2908: Generates the fallback method body for `whileTrue`/`whileFalse`
    /// — Block's `whileTrue:`/`whileFalse:`. Reached only when something
    /// bypasses the call-site interception these selectors normally get (e.g.
    /// `perform:`/`perform:withArguments:`) — the same gap BT-2812 closed for
    /// Block's `value*` family. See
    /// `generate_block_value_structural_fallback`'s doc comment for the full
    /// rationale (Tier 1/Tier 2 discrimination via `erlang:is_function/2`
    /// arity, the arity-ambiguity caveat, and the `stateful_block_dispatch`
    /// error shape).
    ///
    /// Unlike `value*` (a single receiver to discriminate), both the
    /// *condition* (`Self`) and the *body* (`current_method_params[0]`) must
    /// independently be Tier 1 for the generic loop below to be correct —
    /// either being Tier 2 raises `stateful_block_dispatch`, mirroring
    /// `generate_block_value_structural_fallback`'s single-receiver case.
    pub(in crate::core_erlang) fn generate_while_structural_fallback(
        &mut self,
        intrinsic_name: &str,
        negate: bool,
        real_selector: &str,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let body_param = self
            .current_method_params
            .first()
            .cloned()
            .unwrap_or_else(|| "BodyBlock".to_string());

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'(",
            leaf::atom(intrinsic_name),
            ", [",
            leaf::var(body_param.clone()),
            "], ",
            Document::Str(self_var),
            ")",
        ];

        let (continue_atom, exit_atom) = if negate {
            ("'false'", "'true'")
        } else {
            ("'true'", "'false'")
        };
        let loop_fn = self.fresh_temp_var("Loop");
        let tier1_loop = docvec![
            "letrec ",
            leaf::fname(loop_fn.clone(), 0),
            " = fun () -> case apply ",
            Document::Str(self_var),
            " () of <",
            continue_atom,
            "> when 'true' -> let _ = apply ",
            leaf::var(body_param.clone()),
            " () in apply ",
            leaf::fname(loop_fn.clone(), 0),
            " () <",
            exit_atom,
            "> when 'true' -> 'nil' end in apply ",
            leaf::fname(loop_fn, 0),
            " ()",
        ];

        let body_stateful_error = self.generate_stateful_block_dispatch_error(
            real_selector,
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let body_tier_check = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(body_param.clone()),
            ", 0) of <'true'> when 'true' -> ",
            tier1_loop,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(body_param),
            ", 1) of <'true'> when 'true' -> ",
            body_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch.clone(),
            " end end",
        ];

        let self_stateful_error = self.generate_stateful_block_dispatch_error(
            real_selector,
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 0) of <'true'> when 'true' -> ",
            body_tier_check,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 1) of <'true'> when 'true' -> ",
            self_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
    }

    /// BT-2908: Generates the fallback method body for `repeat` — Block's
    /// `repeat`. See `generate_while_structural_fallback` for the general
    /// rationale; `repeat` only has a receiver to discriminate (no argument
    /// block — `[self processNextMessage] repeat` takes no arguments).
    pub(in crate::core_erlang) fn generate_repeat_structural_fallback(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'('repeat', [], ",
            Document::Str(self_var),
            ")",
        ];

        let loop_fn = self.fresh_temp_var("Loop");
        let tier1_loop = docvec![
            "letrec ",
            leaf::fname(loop_fn.clone(), 0),
            " = fun () -> let _ = apply ",
            Document::Str(self_var),
            " () in apply ",
            leaf::fname(loop_fn.clone(), 0),
            " () in apply ",
            leaf::fname(loop_fn, 0),
            " ()",
        ];

        let stateful_error = self.generate_stateful_block_dispatch_error(
            "repeat",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 0) of <'true'> when 'true' -> ",
            tier1_loop,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 1) of <'true'> when 'true' -> ",
            stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
    }

    /// Appends the initial call to a hybrid loop function.
    pub(super) fn append_hybrid_initial_call(
        docs: &mut Vec<Document<'static>>,
        fn_name: &str,
        arity: usize,
        initial_local_args: Vec<String>,
        readonly_param_names: &[String],
        mutated_param_names: &[String],
    ) {
        let initial_args_doc = join(
            initial_local_args
                .into_iter()
                .map(leaf::var)
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(mutated_param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        );
        docs.push(docvec![
            "in apply ",
            leaf::fname(fn_name.to_string(), arity),
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
    fn test_while_true_simple_generates_letrec() {
        // Pure whileTrue: (no mutations) generates a letrec-based loop
        let src =
            "Actor subclass: Runner\n  state: x = 0\n\n  run =>\n    [false] whileTrue: [42]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: should generate a letrec for the loop. Got:\n{code}"
        );
        // Simple variant evaluates condition via case apply (distinguishing it from repeat)
        assert!(
            code.contains("case apply"),
            "whileTrue: should evaluate condition via case apply. Got:\n{code}"
        );
        assert!(
            code.contains("<'true'> when 'true'"),
            "whileTrue: should match true to continue. Got:\n{code}"
        );
        assert!(
            code.contains("<'false'> when 'true' -> 'nil'"),
            "whileTrue: should return nil when condition is false. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_false_simple_generates_letrec_with_opposite_pattern() {
        // Pure whileFalse: (no mutations) generates a letrec matching false to continue
        let src =
            "Actor subclass: Runner\n  state: x = 0\n\n  run =>\n    [false] whileFalse: [42]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileFalse: should generate a letrec. Got:\n{code}"
        );
        assert!(
            code.contains("<'false'> when 'true' ->"),
            "whileFalse: should continue when condition is false. Got:\n{code}"
        );
        assert!(
            code.contains("<'true'> when 'true' -> 'nil'"),
            "whileFalse: should stop when condition is true. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_true_with_field_mutation_threads_actor_state() {
        // whileTrue: with field mutation uses actor state threading (not simple letrec)
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    [self.n < 10] whileTrue: [self.n := self.n + 1]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: with mutation should generate a letrec. Got:\n{code}"
        );
        // State-threading variant uses 'while'/1 with StateAcc parameter
        assert!(
            code.contains("'while'/1"),
            "Mutating whileTrue: should use 'while'/1 with state parameter. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "whileTrue: body should update 'n' via maps:put. Got:\n{code}"
        );
    }

    // ── BT-1275: direct-params optimisation ──────────────────────────────────

    #[test]
    fn test_while_true_local_var_only_uses_direct_params() {
        // whileTrue: with only local-var mutations uses direct fun params, not StateAcc.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: with local mutation should generate a letrec. Got:\n{code}"
        );
        // Direct-params: fun takes (Sum) not (StateAcc).
        assert!(
            code.contains("fun (Sum)"),
            "direct-params: whileTrue: fun should take Sum as direct param. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "direct-params: whileTrue: fun must not use StateAcc signature. Got:\n{code}"
        );
        // Exactly one maps:put for exit StateAcc rebuild.
        assert!(
            code.match_indices("maps':'put'('__local__sum'").count() == 1,
            "direct-params: whileTrue: at most one maps:put (exit rebuild). Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "direct-params: whileTrue: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_false_local_var_only_uses_direct_params() {
        // whileFalse: with only local-var mutations uses direct fun params.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum >= 10] whileFalse: [sum := sum + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("fun (Sum)"),
            "direct-params: whileFalse: fun should take Sum as direct param. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "direct-params: whileFalse: fun must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "direct-params: whileFalse: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    // ── BT-1326/BT-1342: full-extract direct-params + field extraction ───────

    #[test]
    fn test_while_true_field_plus_local_mutation_uses_full_extract() {
        // BT-1342: whileTrue: with BOTH local var mutation AND field mutation uses
        // full-extract mode — mutated field 'n' is a direct param, no State param.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + 1. self.n := self.n + 1]\n    sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "full-extract: whileTrue: should generate a letrec. Got:\n{code}"
        );
        // No State in fun signature — mutated field 'n' is a direct param.
        assert!(
            !code.contains("fun (Sum, State)"),
            "full-extract: whileTrue: fun must not have State param. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "full-extract: whileTrue: fun must not use StateAcc signature. Got:\n{code}"
        );
        // Mutated field 'n' pre-extracted as NField param.
        assert!(
            code.contains("NField"),
            "full-extract: whileTrue: 'n' should be extracted as NField param. Got:\n{code}"
        );
        // Exit rebuilds StateAcc with repacked field + locals.
        assert!(
            code.contains("ExitSA"),
            "full-extract: whileTrue: exit StateAcc rebuild expected. Got:\n{code}"
        );
        // Exit packs mutated field + local into ExitSA.
        assert!(
            code.contains("maps':'put'('n'"),
            "full-extract: whileTrue: exit arm must repack 'n'. Got:\n{code}"
        );
        assert!(
            code.match_indices("maps':'put'('__local__sum'").count() <= 1,
            "full-extract: whileTrue: at most one maps:put for local (exit rebuild). Got:\n{code}"
        );
    }

    #[test]
    fn test_while_true_readonly_field_pre_extracted_as_direct_param() {
        // BT-1326/BT-1342: whileTrue: body reads self.step (never written) and writes self.n:
        // self.step is pre-extracted as read-only param, self.n is pre-extracted as mutated param.
        let src = "Actor subclass: Ctr\n  state: n = 0\n  state: step = 1\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + self.step. self.n := self.n + 1]\n    sum\n";
        let code = codegen(src);
        assert!(
            !code.contains("fun (StateAcc)"),
            "readonly field: whileTrue: must not use StateAcc signature. Got:\n{code}"
        );
        // 'step' is read-only — pre-extracted before letrec.
        assert!(
            code.contains("StepField"),
            "readonly field: whileTrue: fun should have a StepField param. Got:\n{code}"
        );
        // Only one maps:get for 'step' (the pre-extraction).
        assert!(
            code.match_indices("maps':'get'('step'").count() == 1,
            "readonly field: whileTrue: exactly one maps:get for 'step'. Got:\n{code}"
        );
        // Mutable field 'n' is also pre-extracted and repacked at exit.
        assert!(
            code.contains("NField"),
            "readonly field: whileTrue: 'n' should be extracted as NField param. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "readonly field: whileTrue: exit arm must repack 'n'. Got:\n{code}"
        );
    }

    // ── BT-1343: codegen diagnostics ─────────────────────────────────────────

    fn codegen_with_diagnostics(
        src: &str,
        enabled: bool,
    ) -> (String, Vec<beamtalk_core::source_analysis::Diagnostic>) {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let result = crate::core_erlang::generate_module_with_warnings(
            &module,
            crate::core_erlang::CodegenOptions::new("test")
                .with_workspace_mode(true)
                .with_codegen_diagnostics(enabled),
        )
        .expect("codegen should succeed");
        (result.code, result.warnings)
    }

    #[test]
    fn test_bt1343_diagnostics_off_by_default() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    x := 0\n    [x < 10] whileTrue: [x := x + 1]\n";
        let (_, warnings) = codegen_with_diagnostics(src, false);
        let codegen_diags: Vec<&str> = warnings
            .iter()
            .filter(|w| {
                let m = w.message.as_str();
                m.contains("direct-params")
                    || m.contains("tuple-acc")
                    || m.contains("StateAcc fallback")
                    || m.contains("dynamic dispatch")
            })
            .map(|w| w.message.as_str())
            .collect();
        assert!(
            codegen_diags.is_empty(),
            "Expected no codegen diagnostics when disabled. Got: {codegen_diags:?}"
        );
    }

    #[test]
    fn test_bt1343_direct_params_diagnostic() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    x := 0\n    [x < 10] whileTrue: [x := x + 1]\n";
        let (_, warnings) = codegen_with_diagnostics(src, true);
        let diag_msgs: Vec<&str> = warnings.iter().map(|w| w.message.as_str()).collect();
        assert!(
            diag_msgs.iter().any(|m| m.contains("direct-params")),
            "Expected direct-params diagnostic. Got: {diag_msgs:?}"
        );
    }

    #[test]
    fn test_bt1343_stateacc_fallback_diagnostic() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    [self.n < 10] whileTrue: [self.n := self.n + 1]\n";
        let (_, warnings) = codegen_with_diagnostics(src, true);
        let diag_msgs: Vec<&str> = warnings.iter().map(|w| w.message.as_str()).collect();
        assert!(
            diag_msgs
                .iter()
                .any(|m| m.contains("StateAcc fallback") || m.contains("hybrid")),
            "Expected StateAcc or hybrid diagnostic for field-only mutations. Got: {diag_msgs:?}"
        );
    }

    // ── BT-1609: value-type local threading ─────────────────────────────

    #[test]
    fn test_value_type_while_true_extracts_threaded_locals() {
        // BT-1609: whileTrue: in value-type context must extract threaded locals
        // from the returned {'nil', StateAcc} tuple after the loop.
        let src = "Object subclass: Calc\n\n  run =>\n    counter := 3\n    steps := 0\n    [counter > 0] whileTrue: [\n      counter := counter - 1\n      steps := steps + 1\n    ]\n    steps\n";
        let code = codegen(src);
        assert!(
            code.contains("WhileResult") || code.contains("whileResult"),
            "value-type whileTrue: should bind loop result to WhileResult var. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2,"),
            "value-type whileTrue: should extract state from element 2. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__counter'"),
            "value-type whileTrue: should extract counter from state. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__steps'"),
            "value-type whileTrue: should extract steps from state. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_while_false_extracts_threaded_locals() {
        let src = "Object subclass: Calc\n\n  run =>\n    counter := 0\n    [counter >= 3] whileFalse: [\n      counter := counter + 1\n    ]\n    counter\n";
        let code = codegen(src);
        assert!(
            code.contains("WhileResult") || code.contains("whileResult"),
            "value-type whileFalse: should bind loop result. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__counter'"),
            "value-type whileFalse: should extract counter from state. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_while_last_expr_unwraps_nil() {
        // BT-2308: a mutating whileTrue: as the method's LAST expression must return the
        // loop's logical value (element 1 = nil), not the raw {nil, StateAcc} tuple.
        let src =
            "Object subclass: Calc\n\n  run =>\n    n := 0\n    [n < 3] whileTrue: [n := n + 1]\n";
        let code = codegen(src);
        assert!(
            code.contains("element'(1,"),
            "last-expr whileTrue: should unwrap element 1 (nil). Got:\n{code}"
        );
    }
}

#[cfg(test)]
mod bt3419_stateful_condition_tests {
    use crate::core_erlang::tests::{assert_compiles_through_erlc, codegen};

    // ADR 0118 phase 3 (BT-3419): `whileTrue:`/`whileFalse:` conditions with
    // a state effect (a self-send, or an `and:`/`or:` that carries one) now
    // compile and thread state correctly instead of panicking the verifier
    // or crashing at runtime — see the matching `#[should_panic]`→pass
    // inversions in `tests/gen_server.rs` for the exact two shapes these
    // mirror, and `stdlib/test/actor_self_send_position_matrix_test.bt`'s
    // `testWhileTrueCondition*` rows for the end-to-end runtime proof.

    #[test]
    fn condition_binary_op_self_send_threads_state_and_compiles() {
        // A bare self-send nested in an ordinary binary-op chain, itself the
        // condition's tail: `(self bumpCount) + i < 5`. Before this phase,
        // `generate_while_true`'s selection only looked at the BODY's own
        // mutations (trivially none here — `[nil]`), so this fell to the
        // simple (non-threading) codegen path and crashed. Now routes
        // through the mutation-threading path and correctly repacks `i` and
        // `count` into `StateAcc` every iteration.
        let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    i := 0\n    [\n      i := i + 1\n      (self bumpCount) + i < 5\n    ] whileTrue: [nil]\n    i\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n";
        let code = codegen(src);
        assert!(
            code.contains("'while'/1"),
            "a condition-only mutation must route through the mutation-threading \
             ('while'/1) codegen, not the simple letrec. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__i'") && code.contains("maps':'put'('__local__i'"),
            "the condition's own local write (`i := i + 1`) must pack into \
             StateAcc like a loop body statement, not a plain unthreaded \
             block-local rebind. Got:\n{code}"
        );
        assert!(
            code.contains("'bumpCount'"),
            "the condition's self-send must dispatch via safe_dispatch. Got:\n{code}"
        );
    }

    #[test]
    fn condition_and_block_self_send_threads_state_and_compiles() {
        // A self-send inside an `and:`'s block argument, the condition's
        // tail: `(i < 3) and: [(self bumpCount) > 0]`. The existing
        // mutation-threaded `and:` intrinsic already compiles this to a
        // `{Bool, State}` pair; before this phase the loop's own condition-
        // application site treated it as a bare boolean, mismatching the
        // pair it actually got (a runtime crash) or (with a trivial body)
        // never reached this codegen at all (verifier panic — see
        // `tests/gen_server.rs`).
        let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    i := 0\n    [\n      i := i + 1\n      (i < 3) and: [(self bumpCount) > 0]\n    ] whileTrue: [nil]\n    i\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n";
        let code = codegen(src);
        assert!(
            code.contains("'while'/1"),
            "a condition-only mutation must route through the mutation-threading \
             ('while'/1) codegen, not the simple letrec. Got:\n{code}"
        );
        assert!(
            code.contains("CondPair") && code.contains("CondBool"),
            "the condition's `{{Bool, State}}` pair must be unpacked at the \
             application site. Got:\n{code}"
        );
    }

    #[test]
    fn whilefalse_condition_binary_op_self_send_threads_state_and_compiles() {
        // Review finding: every test above exercises `whileTrue:` only, but
        // `generate_while_loop_with_mutations`/`generate_while_false` share
        // the SAME `cond_effects`-branching code via the `negate` bool — an
        // atom mismatch (continuing on `'false'` instead of `'true'`, say)
        // would silently break only `whileFalse:` while every `whileTrue:`
        // test here still passes. `(self bumpCount) + i >= 5` — continues
        // while FALSE, the negated counterpart of the binary-op test above.
        let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    i := 0\n    [\n      i := i + 1\n      (self bumpCount) + i >= 5\n    ] whileFalse: [nil]\n    i\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n";
        let code = codegen(src);
        assert!(
            code.contains("'while'/1"),
            "a condition-only mutation must route through the mutation-threading \
             ('while'/1) codegen, not the simple letrec. Got:\n{code}"
        );
        assert!(
            code.contains("<'false'> when 'true' -> ")
                && code.contains("<'true'> when 'true' -> {'nil',"),
            "whileFalse: must continue on 'false' and exit on 'true' — got the \
             wrong atom on one of the two case arms. Got:\n{code}"
        );
    }

    #[test]
    fn condition_self_send_exit_on_first_check_still_threads_mutation() {
        // Review finding: every runtime-verified case (BUnit matrix,
        // scratch-project manual runs during review) continues the loop at
        // least once. The EXIT arm rebinds `StateAcc` from the SAME
        // `CondPair` unpack as the continue arm (see
        // `generate_while_loop_with_mutations`'s doc comment on the
        // `cond_effects` branch) — confirm that holds even when the
        // condition is FALSE on the very first check, so a self-send's
        // mutation is not lost when the loop body never runs at all.
        let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    [(self bumpCount) > 100] whileTrue: [nil]\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n";
        let code = codegen(src);
        // `codegen()` always names the generated module 'test' (workspace-mode
        // default) — `assert_compiles_through_erlc`'s own `module_name` param
        // must match that for `erlc` to accept the file (it checks the
        // `-module` attribute against the source filename).
        assert_compiles_through_erlc("test", &code);
    }
}
