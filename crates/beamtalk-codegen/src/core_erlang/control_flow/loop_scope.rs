// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The scope bracket shared by the four direct-params / hybrid letrec loop
//! emitters (`generate_while_loop_direct`, `generate_while_loop_hybrid`,
//! `generate_counted_stateful_loop_direct`, `generate_counted_stateful_loop_hybrid`)
//! (BT-4888).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! All four lower a loop whose `letrec` fun binds each threaded local (and,
//! hybrid, each field) as a direct parameter instead of a `StateAcc` map. The
//! sequence around the body is the same in each and used to be written four
//! times, held together by "mirrors the identical … fix" comments:
//!
//! 1. *(direct only)* capture the pre-loop `State` variable name in
//!    [`LoopMode::direct_params_outer_state_var`] — the loop's `letrec` fun
//!    binds no `StateAcc`, so a bare mid-body field read needs *some* real,
//!    lexically-in-scope name for the actor state (BT-3562). A loop nested in
//!    another direct-params loop reuses the OUTER loop's captured name (by
//!    then `in_loop_body` is already set, so `current_state_var()` would name
//!    a `StateAcc*` variable one level too deep that no fun ever bound). It
//!    is restored when the body is lowered;
//! 2. push the BT-4121 guarded-operator marker and a variable scope;
//! 3. bind the counter parameter (counted loops) and register the
//!    threaded-local → parameter bindings;
//! 4. lower the condition and body (the caller's closure);
//! 5. check out the marker — a direct loop may bracket with a `State`
//!    pre-store; a hybrid loop cannot (its parameters advance past the entry
//!    `State`), so there the checkout only reports (loudly) a missed operator
//!    and any `^` lowered in the loop (`NlrThrowInHybridLoop`);
//! 6. build the exit `StateAcc` (the caller's closure) and pop the scope.
//!
//! [`LoopMode::direct_params_outer_state_var`]: super::loop_mode::LoopMode::direct_params_outer_state_var

use super::super::threaded_ir::VerifyError;
use super::super::{CoreErlangGenerator, Result};
use super::plan::ThreadingPlan;
use beamtalk_cerl_doc::Document;
use beamtalk_core::ast::{Block, Expression};

/// Which letrec parameter convention the loop uses.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum DirectParamsKind {
    /// Locals are the fun's parameters; fields read through `State`.
    Direct,
    /// Locals AND fields are the fun's parameters; no `State` at all.
    Hybrid,
}

/// What the loop being lowered looks like to [`CoreErlangGenerator::with_direct_params_loop_scope`].
pub(super) struct DirectParamsLoopScope<'a> {
    pub kind: DirectParamsKind,
    pub plan: &'a ThreadingPlan,
    pub body: &'a Block,
    /// The `whileTrue:`/`whileFalse:` receiver block's expression, when the
    /// guarded-operator marker must scan it too.
    pub condition: Option<&'a Expression>,
    /// `(Beamtalk name, Core var)` of a counted loop's block parameter.
    pub counter_param: Option<(&'a str, &'a str)>,
}

/// What [`CoreErlangGenerator::with_direct_params_loop_scope`] hands back.
pub(super) struct DirectParamsLoopOutcome<B> {
    /// Whatever the `lower` closure answered.
    pub lowered: B,
    /// The exit-arm `StateAcc` the `exit_stateacc` closure built.
    pub exit_stateacc: Document<'static>,
    /// The `let _ = store(State) in ` prefix ([`Document::Nil`] for a loop
    /// that needs none, and always for a hybrid one).
    pub prestore: Document<'static>,
    /// Hybrid only: `^`-in-loop findings to report with the loop's own.
    pub nlr_errors: Vec<VerifyError>,
}

impl CoreErlangGenerator {
    /// Runs `lower` (the condition + body) inside the shared direct-params /
    /// hybrid loop bracket described in the [module docs](self), then builds
    /// the exit `StateAcc` with `exit_stateacc` (handed whether the loop
    /// brackets its guarded operators; always `false` for a hybrid loop)
    /// before popping the scope.
    pub(super) fn with_direct_params_loop_scope<B>(
        &mut self,
        scope: &DirectParamsLoopScope<'_>,
        lower: impl FnOnce(&mut Self) -> Result<B>,
        exit_stateacc: impl FnOnce(&mut Self, bool) -> Document<'static>,
    ) -> Result<DirectParamsLoopOutcome<B>> {
        let direct = scope.kind == DirectParamsKind::Direct;

        // (1) Capture the pre-loop `State` name (direct only).
        let prev_outer_state = if direct {
            let prev = self.loop_mode.direct_params_outer_state_var.take();
            self.loop_mode.direct_params_outer_state_var =
                Some(prev.clone().unwrap_or_else(|| self.current_state_var()));
            prev
        } else {
            None
        };

        // BT-4121: the marker spans the lowering. `prestore_state` is the
        // loop-entry `State` a pre-store captures when the marker asks for
        // the bracket.
        let prestore_state = direct.then(|| self.state_var_to_store());
        self.push_guarded_op_loop(scope.body, scope.condition);
        self.push_scope();
        if let Some((bt_name, core_var)) = scope.counter_param {
            self.bind_var(bt_name, core_var);
        }
        // No unpack docs in direct/hybrid mode (structurally guaranteed by
        // `generate_unpack_at_iteration_start`'s own
        // `if !use_direct_params && !use_hybrid_params` guard) — this only
        // registers each threaded local's parameter binding.
        scope.plan.generate_unpack_at_iteration_start(self);

        let lowered = lower(self)?;
        if direct {
            self.loop_mode.direct_params_outer_state_var = prev_outer_state;
        }

        let (bracket, prestore, nlr_errors) = match prestore_state {
            Some(state) => {
                let (bracket, prestore) = self.checkout_guarded_op_loop_docs(
                    self.pop_guarded_op_loop(),
                    false,
                    scope.body.span,
                    &state,
                );
                (bracket, prestore, Vec::new())
            }
            None => (
                false,
                Document::Nil,
                self.checkout_hybrid_loop(scope.body.span),
            ),
        };

        let exit_stateacc = exit_stateacc(self, bracket);
        self.pop_scope();
        Ok(DirectParamsLoopOutcome {
            lowered,
            exit_stateacc,
            prestore,
            nlr_errors,
        })
    }
}
