// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Context-agnostic outer-local mutation threading (BT-2361 steps 1-2).
//!
//! **DDD Context:** Code Generation
//!
//! A captured-local loop (`whileTrue:`/`whileFalse:`, `to:do:`/`to:by:do:`/
//! `timesRepeat:`) or a foldl list-op
//! (`collect:`/`select:`/`reject:`/`inject:into:`/`count:`/`detect:`) lowers to a
//! `{Value, StateAcc}` 2-tuple whose element 1 is the logical result and element 2 is
//! a map of the mutated outer locals (ADR 0041's calling convention). A read+write
//! conditional (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`) is instead inlined as a
//! `case` — in last position binding only its branch's logical value, in non-last /
//! assign-RHS position yielding a flat `{LogicalValue, Mut1, …, MutN}` tuple whose
//! trailing elements rebind the threaded locals positionally. Either way every
//! construct exposes the same two things — a logical value and the outer-local
//! mutations — so the three former consumption paths (value-type, class-method, …)
//! did not disagree about *what* a construct produces, only about what to do with it
//! at the **method boundary**.
//!
//! This module splits that tangle along one line:
//!
//! * **Transform (shared, written once).** Given a last/return-position threading
//!   construct, [`CoreErlangGenerator::lower_threaded_last`] binds its logical value
//!   (tuple element 1) to a fresh result var. The threaded locals do not escape in
//!   last position, so element 2 is discarded. This subsumes the value-type
//!   `emit_vt_last_expr` threading branch (loops, foldl list-ops, and read+write
//!   conditionals alike) and the class-method `try_generate_class_method_threaded_last`.
//! * **Boundary (per-context adapter, the only thing that varies).** A
//!   [`ThreadingBoundary`] captures how the bound result var is returned/stored:
//!   the value-type `{Result, Self{N}}` / bare-`Result` shape, the class-method
//!   `{class_var_result, Result, ClassVarsN}` / bare-`Result` shape, or the Actor
//!   `{'reply', Reply, NewState}` `gen_server` reply shape (BT-2378). This mirrors the
//!   [`NlrBoundary`](super::NlrBoundary) precedent (BT-2361 step 4 / PR #2408).
//!
//! The Actor boundary is structurally distinct from the other two: its mutated outer
//! locals do not ride a *separate* `StateAcc` map that is discarded at the boundary —
//! element 2 of the construct's `{Value, NewState}` tuple **is** the `gen_server` `State`
//! map (with `__local__`-prefixed local keys threaded in). The boundary therefore binds
//! element 2 to the next state version and threads it onward, supplying that primitive
//! via [`CoreErlangGenerator::emit_actor_threaded_last_stmts`] /
//! [`CoreErlangGenerator::emit_actor_threaded_assign_rhs_stmts`] — a genuine *extension* of
//! the seam, not a fold of the existing `{Value, StateAcc}` transform.
//!
//! ## BT-3148 task 4: `ThreadingBoundary` audit — survives, narrowed to one job
//!
//! ADR 0111 Addendum 4 asked whether `ThreadingBoundary` survives BT-3148's routing
//! unification (task 1) as pure lowering-time classification, or is fully replaced.
//! Audited: it **survives**, but its job has narrowed to exactly one thing —
//! [`CoreErlangGenerator::threading_result_tail`]'s return-shape adapter (which
//! `{Result, ...}` Document a bound value renders as). Its *other*, pre-BT-3148 job —
//! deciding whether a construct routes through the shared emitter at all, by rechecking
//! [`CoreErlangGenerator::control_flow_has_mutations`] a second time — is gone:
//! [`CoreErlangGenerator::lower_threaded_last`] and
//! [`CoreErlangGenerator::emit_threaded_assign_rhs`] no longer take a `boundary` param to
//! redirect on (`ThreadingBoundary::Actor` used to short-circuit both into the Actor
//! transform below); the Actor path is now reached directly, by construction, from
//! `gen_server/methods.rs`'s already-classified `BodyExprKind::ControlFlowWithMutations`/
//! `LocalAssignControlFlow` arms calling [`CoreErlangGenerator::emit_actor_threaded_last_stmts`]/
//! [`CoreErlangGenerator::emit_actor_threaded_assign_rhs_stmts`] — functions that take no
//! `boundary` at all and never decline. `ThreadingBoundary::Actor` itself still exists,
//! with exactly one remaining construction site (`emit_actor_threaded_last_stmts`, passed
//! straight to `threading_result_tail`) and one remaining match site
//! (`threading_result_tail`'s own `match`) — a pure per-context reply-shape selector, not
//! a routing decision. `classify_body_expr` (`gen_server/methods.rs`) is now the sole
//! caller of `control_flow_has_mutations` for this routing question; every other call site
//! of that function is for an unrelated purpose (conditionals/exception-handling/match-arm
//! classification), never a second recheck of the same Actor-body-mutation question.

use super::threaded_ir::{BindOp, FrameId, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar};
use super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::Expression;

/// Where a threading construct sits in its enclosing method body. Drives how the
/// bound logical value (tuple element 1) is returned/stored.
///
/// Both `Last` and `Return` flow through the unified emitter (the class-method
/// generator passes `Return` for explicit `^ expr` and `Last` for the implicit final
/// expression; the value-type generator passes `Last`). They currently receive
/// identical post-construct treatment — `Return` is recorded distinctly so the call
/// site's intent is preserved for the later non-last / assign-RHS migration steps. The
/// non-last open-let-chain paths (`generate_vt_*_open`) are already context-shared and
/// are not (yet) routed through this emitter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ThreadingPosition {
    /// Implicit last expression of the method body.
    Last,
    /// Explicit `^ expr` return.
    Return,
}

/// The per-context threading boundary — the *only* thing that differs between the
/// value-type and class-method consumption of a threaded construct's logical value
/// once the transform is shared.
///
/// Both contexts bind the construct's logical value (tuple element 1) to a result var
/// identically; they disagree only about the Document that returns/stores it. This
/// enum captures that single axis so the last/return-position emitter is written once
/// (see [`CoreErlangGenerator::threading_result_tail`]) instead of being copy-evolved
/// per context — exactly mirroring the [`NlrBoundary`](super::NlrBoundary) seam.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ThreadingBoundary {
    /// Value-type methods: the tail yields `{Result, Self{N}}` when a non-local return
    /// is active (so the normal and NLR-catch paths share a shape), or bare `Result`.
    ValueType { has_nlr: bool },
    /// Class methods: the tail yields `{'class_var_result', Result, ClassVarsN}` when an
    /// earlier statement mutated a class var, or bare `Result` otherwise. Threading
    /// constructs mutate *locals*, not class vars, so the wrapping is driven solely by
    /// `class_var_mutated()`.
    ClassMethod,
    /// Actor (`gen_server`) methods: BT-2378. Structurally distinct from the value-type
    /// and class-method boundaries:
    ///
    /// * **Where threaded state lives.** Actors do not carry mutated outer locals in a
    ///   *separate* `StateAcc` map that is discarded at the boundary; element 2 of the
    ///   construct's `{Value, NewState}` tuple **is** the `gen_server` `State` map (with
    ///   `__local__`-prefixed local keys threaded in). The boundary therefore binds
    ///   element 2 to the next state version and threads it onward, rather than discarding
    ///   it.
    /// * **How the body returns.** Actor method bodies return the `gen_server` reply tuple
    ///   `{'reply', Reply, NewState}`, not a bare value (or a `{Value, Self}` /
    ///   `{class_var_result, …}` value-type shape).
    Actor,
}

/// A threading construct normalized for the boundary to consume.
///
/// The transform lowers a construct into this; the boundary lowers it out to Core
/// Erlang via [`CoreErlangGenerator::threading_result_tail`]. `value_doc` is the open
/// let-chain that binds the logical value (tuple element 1) to `result_var`;
/// `threaded_locals` is the set of outer locals carried in tuple element 2 (discarded
/// in last position, where they do not escape); `position` records last vs explicit
/// return.
pub(super) struct ThreadedExpr {
    /// Open let-chain binding the construct's logical value to `result_var`.
    pub(super) value_doc: Document<'static>,
    /// Core Erlang variable bound to the construct's logical value (tuple element 1).
    pub(super) result_var: String,
    /// Core Erlang variable bound to the construct's threaded state (tuple element 2),
    /// when the boundary needs it. `None` for value-type / class-method last position,
    /// where the mutated outer locals ride a *separate* `StateAcc` map that does not
    /// escape and is discarded. `Some` for the Actor boundary (BT-2378), where element 2
    /// **is** the `gen_server` `State` map that must be returned in the reply tuple.
    pub(super) state_var: Option<String>,
    /// Outer locals the construct threads via tuple element 2. Unused in last position
    /// (the mutations do not escape), retained for the design's representation and any
    /// future non-last routing through the unified emitter.
    #[allow(dead_code)]
    pub(super) threaded_locals: Vec<String>,
    /// Last expression vs explicit `^`-return (same post-construct treatment).
    #[allow(dead_code)]
    pub(super) position: ThreadingPosition,
}

impl CoreErlangGenerator {
    /// Lowers a last/return-position threading construct into a [`ThreadedExpr`],
    /// binding its logical value (tuple element 1) to a fresh result var. The
    /// value-type / class-method boundaries share the BT-2342
    /// `{Value, StateAcc}` transform primitives below.
    ///
    /// Returns `None` when `expr` (after peeling redundant parentheses) is not a
    /// recognized threading construct, so the caller falls back to its generic
    /// last-expression path. Handles (value-type / class-method boundaries):
    ///
    /// * loops / foldl list-ops yielding `{Value, StateAcc}` — element 1 is unwrapped
    ///   (loops put `'nil'` there; foldl list-ops put the collected/folded value);
    /// * read+write conditionals — inlined as a `case` binding the branch's logical
    ///   value, avoiding the 0-arg dispatch crash on a stateful arity-1 block.
    ///
    /// The threaded locals do not escape in last position, so tuple element 2 is
    /// discarded. BT-2358: redundant parentheses (`^(items collect: …)`) are peeled so
    /// the construct inside is unwrapped rather than leaking its raw tuple.
    ///
    /// BT-3148 (ADR 0111 Addendum 4): the Actor boundary no longer routes
    /// through this recognizer — `gen_server/methods.rs`'s
    /// `classify_body_expr` is the single classification pass, and its
    /// `ControlFlowWithMutations`/`LocalAssignControlFlow` arms call
    /// [`Self::emit_actor_threaded_last_stmts`]/
    /// [`Self::emit_actor_threaded_assign_rhs_stmts`] directly, which never
    /// decline (no second `control_flow_has_mutations` recheck to disagree
    /// with — the `RoutingMismatch` drift class is unrepresentable by
    /// construction).
    pub(super) fn lower_threaded_last(
        &mut self,
        expr: &Expression,
        position: ThreadingPosition,
    ) -> Result<Option<ThreadedExpr>> {
        let expr = expr.unwrap_parens();
        let mut parts: Vec<Document<'static>> = Vec::new();
        let result_var = if self.expr_yields_vt_threaded_tuple(expr) {
            self.emit_vt_threaded_tuple_unwrap_to_var(expr, &mut parts)?
        } else if self.is_conditional_with_vt_local_threading(expr) {
            match self.emit_vt_conditional_case_to_var(expr, &mut parts)? {
                Some(result_var) => result_var,
                None => return Ok(None),
            }
        } else {
            return Ok(None);
        };
        Ok(Some(ThreadedExpr {
            value_doc: Document::Vec(parts),
            result_var,
            state_var: None,
            threaded_locals: Vec::new(),
            position,
        }))
    }

    /// BT-2378/BT-3148: Actor-boundary transform for a last-position
    /// control-flow construct that threads state (field mutations,
    /// conditionals, loops, foldl list-ops, exception handlers). Such a
    /// construct lowers — via the actor-context `expression_doc` path — to a
    /// `{Value, NewState}` tuple whose element 2 **is** the `gen_server`
    /// `State` map (with any `__local__`-prefixed outer-local keys threaded
    /// in). Binds element 1 to a fresh result var, rebinds the state version
    /// to element 2 through a real [`ThreadedStmt::Bind`], and closes with
    /// the Actor `{'reply', Result, NewState}` tail.
    ///
    /// ADR 0111 Addendum 4 (BT-3148 task 1): this NEVER declines — the
    /// caller is `gen_server/methods.rs`'s already-classified
    /// `BodyExprKind::ControlFlowWithMutations` arm, and `classify_body_expr`
    /// is the single classification pass. The pre-BT-3148 shape re-checked
    /// `control_flow_has_mutations` here and could fall through to the
    /// generic path, which is exactly the "two independently-computed
    /// decisions must agree" drift `verify_routing_invariant` existed to
    /// compare; with one decision consumed instead of two compared,
    /// `RoutingMismatch` is unrepresentable and that check is deleted.
    ///
    /// The state-version step is a real `Bind` (target/source read off the
    /// live counter) sitting in the method body's `Vec<ThreadedStmt>`; the
    /// tuple/result bindings and the reply tail are opaque
    /// [`ThreadedStmt::Statement`]s (ordinary AST-directed codegen with no
    /// state-threading content of their own).
    pub(super) fn emit_actor_threaded_last_stmts(
        &mut self,
        expr: &Expression,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let span = expr.span();

        // ADR 0118 phase 4 (BT-3420): when `expr` is itself an inline-
        // threaded control-flow construct (`ifTrue:`/`ifFalse:`/
        // `ifTrue:ifFalse:`, `and:`/`or:`, the nil-conditional family, or
        // `match:`) needing mutation threading, this function's OWN caller
        // (`gen_server/methods.rs`'s `ControlFlowWithMutations` arm) already
        // called `thread_ahead(expr, ..)` immediately before this — which,
        // now that `subexpr_needs_prelude` recognizes this shape (via
        // `inline_control_flow_needs_threading`), already spliced the
        // construct's real prelude into `stmts` and registered its
        // ALREADY-UNWRAPPED value for substitution. `expression_doc` below
        // then returns that value directly (not a `{Result, NewState}`
        // tuple) via `take_precompiled_subexpr` — no further `element/2`
        // unwrap needed, and no `{Result, State}` tuple crosses this
        // boundary. The `tuple_var`/manual-`Bind` dance below remains the
        // path for every other `ControlFlowWithMutations` shape (loops,
        // list-ops, exception handlers) that still returns a raw tuple
        // `Document` from `expression_doc`.
        if self.inline_control_flow_needs_threading(expr.unwrap_parens()) {
            let result_var = self.fresh_temp_var("Result");
            let result_doc = self.expression_doc(expr)?;
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    result_doc,
                    " in "
                ],
                span,
            ));
            let new_state = self.current_state_var();
            stmts.push(ThreadedStmt::Statement(
                self.threading_result_tail(&result_var, Some(&new_state), ThreadingBoundary::Actor),
                span,
            ));
            return Ok(());
        }

        let tuple_var = self.fresh_temp_var("Tuple");
        let result_var = self.fresh_temp_var("Result");
        let source_version = self.state_version();
        let expr_doc = self.expression_doc(expr)?;
        let new_state = self.next_state_var();
        let target_version = self.state_version();
        stmts.push(ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(tuple_var.clone()),
                " = ",
                expr_doc,
                " in let ",
                leaf::var(result_var.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(tuple_var.clone()),
                ") in ",
            ],
            span,
        ));
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, source_version, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Doc(docvec![
                "call 'erlang':'element'(2, ",
                leaf::var(tuple_var),
                ")",
            ])),
            shadow_write: false,
            span,
        });
        stmts.push(ThreadedStmt::Statement(
            self.threading_result_tail(&result_var, Some(&new_state), ThreadingBoundary::Actor),
            span,
        ));
        Ok(())
    }

    /// Emits a last/return-position threading construct, applying `boundary`'s return
    /// shape — or returns `None` (without mutating `body_parts`) when `expr` is not a
    /// threading construct, so the caller falls back to its generic path.
    ///
    /// This is the single shared entry point that subsumes the value-type
    /// `emit_vt_last_expr` threading branch (loops, foldl list-ops, and read+write
    /// conditionals) and the class-method `try_generate_class_method_threaded_last`.
    /// The Actor boundary does not route here (BT-3148) — see
    /// [`Self::emit_actor_threaded_last_stmts`].
    pub(super) fn emit_threaded_last(
        &mut self,
        expr: &Expression,
        position: ThreadingPosition,
        boundary: ThreadingBoundary,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<bool> {
        // BT-3169: captured before lowering — `lower_threaded_last`'s two
        // internal builders (`emit_vt_threaded_tuple_unwrap_to_var`,
        // `emit_vt_conditional_case_to_var`) both bind the construct's own
        // Document opaquely (`let TupleVar = <construct> in ...`), which
        // confines any `ClassVarsN` a class-method self-send inside a
        // `Foldl*` body minted (ADR 0111 Addendum 9 Question 6) to that
        // `let`'s own RHS. Refreshed below so `threading_result_tail`'s
        // `ClassMethod` boundary references a name that's actually visible —
        // see `refresh_class_var_after_opaque_scope`'s own doc comment. A
        // no-op (`None`) for the `ValueType`/`Actor` boundaries, where
        // `class_var_version` never advances.
        let cv_version_before = self.class_var_version();
        let Some(threaded) = self.lower_threaded_last(expr, position)? else {
            return Ok(false);
        };
        body_parts.push(threaded.value_doc);
        if let Some(refresh) = self.refresh_class_var_after_opaque_scope(cv_version_before) {
            body_parts.push(refresh);
        }
        body_parts.push(self.threading_result_tail(
            &threaded.result_var,
            threaded.state_var.as_deref(),
            boundary,
        ));
        Ok(true)
    }

    /// Emits a local assignment whose RHS is a threading construct, binding the target
    /// to the construct's logical value (tuple element 1) and rebinding any threaded
    /// sibling outer-locals from element 2 — so both the assigned value and the
    /// mutations are visible to subsequent statements, rather than the target being
    /// bound to the raw `{value, StateAcc}` tuple.
    ///
    /// Returns the Core Erlang variable bound to the assignment target, or `None` (without
    /// mutating `body_parts`) when the RHS is not a threading construct, so the caller
    /// falls back to its generic local-binding path. Handles both the loop / foldl
    /// list-op RHS (`emit_vt_threaded_local_assignment`) and the read+write conditional
    /// RHS (`emit_vt_conditional_assign_rhs`, BT-2359/BT-2371). BT-2358: the conditional
    /// RHS is peeled of redundant parentheses first.
    ///
    /// Shared by the value-type instance-method body sequencer and the class-method
    /// non-last local-var binder, which previously re-derived this branch independently.
    /// The Actor boundary does not route here (BT-3148) — see
    /// [`Self::emit_actor_threaded_assign_rhs_stmts`].
    pub(super) fn emit_threaded_assign_rhs(
        &mut self,
        var_name: &str,
        value: &Expression,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<Option<String>> {
        if self.expr_yields_vt_threaded_tuple(value) {
            return Ok(Some(
                self.emit_vt_threaded_local_assignment(var_name, value, body_parts)?,
            ));
        }
        let rhs = value.unwrap_parens();
        if self.is_conditional_with_vt_local_threading(rhs) {
            return Ok(Some(
                self.emit_vt_conditional_assign_rhs(var_name, rhs, body_parts)?,
            ));
        }
        Ok(None)
    }

    /// BT-2378/BT-3148: Actor-boundary assign-RHS transform —
    /// `var := <control-flow-with-mutations>`.
    ///
    /// The RHS lowers to a `{Value, NewState}` tuple whose element 2 **is** the `gen_server`
    /// `State` map. Binds the target to element 1, rebinds the state version to element 2
    /// through a real [`ThreadedStmt::Bind`], and rebinds any `__local__`-threaded sibling
    /// outer-locals from the new state so both the assigned value and the mutations are
    /// visible to subsequent statements.
    ///
    /// ADR 0111 Addendum 4 (BT-3148 task 1): this NEVER declines — see
    /// [`Self::emit_actor_threaded_last_stmts`]'s doc comment for the
    /// single-classification-pass rationale (the caller is the already-classified
    /// `BodyExprKind::LocalAssignControlFlow` arm; the deleted
    /// `control_flow_has_mutations` recheck here is what `RoutingMismatch`
    /// existed to compare against).
    pub(super) fn emit_actor_threaded_assign_rhs_stmts(
        &mut self,
        var_name: &str,
        value: &Expression,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let span = value.span();
        let core_var = self
            .lookup_var(var_name)
            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);

        // ADR 0118 phase 4 (BT-3420): see `emit_actor_threaded_last_stmts`'s
        // matching check — this function's own caller
        // (`gen_server/methods.rs`'s `LocalAssignControlFlow` arm) already
        // called `thread_ahead(value, ..)` immediately before this, which
        // now splices an inline-threaded control-flow RHS's real prelude
        // and registers its already-unwrapped value. `expression_doc` below
        // then returns that value directly — no `element/2` unwrap needed.
        if self.inline_control_flow_needs_threading(value.unwrap_parens()) {
            let value_doc = self.expression_doc(value)?;
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(core_var.clone()),
                    " = ",
                    value_doc,
                    " in "
                ],
                span,
            ));
            self.bind_var(var_name, &core_var);
            let new_state = self.current_state_var();
            self.push_threaded_var_rebinds(value, var_name, &new_state, span, stmts);
            return Ok(());
        }

        let tuple_var = self.fresh_temp_var("Tuple");
        let source_version = self.state_version();
        let new_state = self.peek_next_state_var();
        let value_str = self.expression_doc(value)?;
        stmts.push(ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(tuple_var.clone()),
                " = ",
                value_str,
                " in let ",
                leaf::var(core_var.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(tuple_var.clone()),
                ") in ",
            ],
            span,
        ));
        let _ = self.next_state_var();
        let target_version = self.state_version();
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, source_version, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Doc(docvec![
                "call 'erlang':'element'(2, ",
                leaf::var(tuple_var),
                ")",
            ])),
            shadow_write: false,
            span,
        });
        self.bind_var(var_name, &core_var);
        self.push_threaded_var_rebinds(value, var_name, &new_state, span, stmts);
        Ok(())
    }

    /// Rebinds every outer local `value`'s construct threads (via
    /// [`CoreErlangGenerator::get_control_flow_threaded_vars`]) from
    /// `new_state`, except `var_name` itself (the assignment target — see
    /// the inline comment at the call site for why). Shared by
    /// [`Self::emit_actor_threaded_assign_rhs_stmts`]'s two paths (already-
    /// spliced inline-control-flow producer, and the raw-tuple fallback for
    /// loops/list-ops) so the rebind logic is written once.
    fn push_threaded_var_rebinds(
        &mut self,
        value: &Expression,
        var_name: &str,
        new_state: &str,
        span: beamtalk_core::source_analysis::Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let mut rebind_parts: Vec<Document<'static>> = Vec::new();
        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value) {
            for var in &threaded_vars {
                // BT-2378: skip the assignment target itself. When the RHS construct mutates
                // the same local internally (`x := (1 to: 3 do: [:i | x := x + i])`), `var_name`
                // appears in `threaded_vars`; rebinding it from `NewState` would emit a second
                // `let` overwriting the value already bound from element 1 (the construct's
                // *logical* result) with the threaded-local value. The target must observe the
                // logical result, so it is excluded here — mirroring the value-type assign-RHS
                // guard in `emit_vt_threaded_local_assignment`.
                if var == var_name {
                    continue;
                }
                let tv_core = self
                    .lookup_var(var)
                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                rebind_parts.push(docvec![
                    "let ",
                    leaf::var(tv_core),
                    " = call 'maps':'get'(",
                    leaf::atom(Self::local_state_key(var)),
                    ", ",
                    leaf::var(new_state.to_string()),
                    ") in ",
                ]);
            }
        }
        if !rebind_parts.is_empty() {
            stmts.push(ThreadedStmt::Statement(Document::Vec(rebind_parts), span));
        }
    }

    /// Builds the Document that returns/stores an already-bound threaded result var for
    /// `boundary`. This is the single place the value-type vs class-method divergence
    /// lives — the boundary adapter the design calls for.
    ///
    /// BT-875: Use Document/docvec! — never format!() for Core Erlang fragments.
    pub(super) fn threading_result_tail(
        &self,
        result_var: &str,
        state_var: Option<&str>,
        boundary: ThreadingBoundary,
    ) -> Document<'static> {
        match boundary {
            ThreadingBoundary::Actor => {
                // BT-2378: gen_server reply. `state_var` is element 2 of the construct's
                // `{Value, NewState}` tuple — the threaded gen_server `State` map.
                let state = state_var.map_or_else(
                    || self.current_state_var(),
                    std::string::ToString::to_string,
                );
                docvec![
                    "{'reply', ",
                    leaf::var(result_var.to_string()),
                    ", ",
                    leaf::var(state),
                    "}",
                ]
            }
            ThreadingBoundary::ValueType { has_nlr: true } => {
                let final_self = self.current_self_var();
                docvec![
                    "    {",
                    leaf::var(result_var.to_string()),
                    ", ",
                    leaf::var(final_self),
                    "}\n",
                ]
            }
            ThreadingBoundary::ValueType { has_nlr: false } => {
                docvec!["    ", leaf::var(result_var.to_string()), "\n"]
            }
            ThreadingBoundary::ClassMethod if self.class_var_mutated() => {
                let final_cv = self.current_class_var();
                docvec![
                    "{'class_var_result', ",
                    leaf::var(result_var.to_string()),
                    ", ",
                    leaf::var(final_cv),
                    "}",
                ]
            }
            ThreadingBoundary::ClassMethod => leaf::var(result_var.to_string()),
        }
    }
}
