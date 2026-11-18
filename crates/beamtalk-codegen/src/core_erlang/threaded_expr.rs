// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Context-agnostic outer-local mutation threading.
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
//!   the value-type and class-method bare-`Result` shape, or the Actor
//!   `{'reply', Reply, NewState}` `gen_server` reply shape. This mirrors the
//!   [`NlrBoundary`](super::NlrBoundary) precedent.
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
//! ## `ThreadingBoundary`'s narrowed job
//!
//! ADR 0111 Addendum 4: `ThreadingBoundary`'s job has narrowed to exactly one
//! thing — [`CoreErlangGenerator::threading_result_tail`]'s return-shape
//! adapter (which `{Result, ...}` Document a bound value renders as). It no
//! longer decides whether a construct routes through the shared emitter at
//! all, by rechecking [`CoreErlangGenerator::control_flow_has_mutations`] a
//! second time:
//! [`CoreErlangGenerator::lower_threaded_last`] and
//! [`CoreErlangGenerator::emit_threaded_assign_rhs`] take no `boundary` param
//! to redirect on; the Actor path is reached directly, by construction, from
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

use super::control_flow::ThreadingPlan;
use super::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, VersionedVar,
};
use super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

/// What a local-threading producer does with its construct's threaded
/// locals once the construct tuple is bound (ADR 0131 §2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ProducerTail {
    /// One `LocalRebind` per threaded local: the construct sits in an
    /// operand position, and later code reads the new values.
    Rebind,
    /// One `DiscardLocals`: the construct is the method root's last
    /// expression, so nothing can read them ("Last position does not
    /// discard, except at the method root").
    Discard,
}

/// A REPL input statement compiled through the local-threading producer
/// (`CoreErlangGenerator::repl_statement_with_producers`): the producer's
/// prelude, which the REPL assembler emits ahead of its own
/// `let Result = <value> in`, the statement's value, and whether that value is
/// itself a `{Value, Bindings}` tuple the assembler must still unpack.
#[derive(Debug)]
pub struct ReplProducerDoc {
    /// The producer's construct tuples and `State` steps, as an open `let`
    /// chain.
    pub prelude: Document<'static>,
    /// The statement's value, compiled after the prelude.
    pub value: Document<'static>,
    /// Whether `value` is a `{Value, Bindings}` tuple
    /// (`repl_loop_mutated`).
    pub mutated: bool,
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
    /// Value-type methods: the tail yields the bare `Result`, with or without a
    /// non-local return (BT-4503: the NLR boundary answers the bare value too).
    ValueType,
    /// Class methods: the tail yields the bare `Result`. Class variables are written
    /// in place in the class process (ADR 0130 §3), so nothing is returned beside the
    /// result.
    ClassMethod,
    /// Actor (`gen_server`) methods. Structurally distinct from the value-type
    /// and class-method boundaries:
    ///
    /// * **Where threaded state lives.** Actors do not carry mutated outer locals in a
    ///   *separate* `StateAcc` map that is discarded at the boundary; element 2 of the
    ///   construct's `{Value, NewState}` tuple **is** the `gen_server` `State` map (with
    ///   `__local__`-prefixed local keys threaded in). The boundary therefore binds
    ///   element 2 to the next state version and threads it onward, rather than discarding
    ///   it.
    /// * **How the body returns.** Actor method bodies return the `gen_server` reply tuple
    ///   `{'reply', Reply, NewState}`, not a bare value (or a `{Value, Self}`
    ///   value-type shape).
    Actor,
}

/// A threading construct normalized for the boundary to consume.
///
/// The transform lowers a construct into this; the boundary lowers it out to Core
/// Erlang via [`CoreErlangGenerator::threading_result_tail`]. `value_doc` is the open
/// let-chain that binds the logical value (tuple element 1) to `result_var`.
pub(super) struct ThreadedExpr {
    /// Open let-chain binding the construct's logical value to `result_var`.
    pub(super) value_doc: Document<'static>,
    /// Core Erlang variable bound to the construct's logical value (tuple element 1).
    pub(super) result_var: String,
    /// Core Erlang variable bound to the construct's threaded state (tuple element 2),
    /// when the boundary needs it. `None` for value-type / class-method last position,
    /// where the mutated outer locals ride a *separate* `StateAcc` map that does not
    /// escape and is discarded. `Some` for the Actor boundary, where element 2
    /// **is** the `gen_server` `State` map that must be returned in the reply tuple.
    pub(super) state_var: Option<String>,
}

/// BT-4499: a block parameter [`CoreErlangGenerator::seed_repl_params`] put in
/// the REPL bindings map for one construct: its binding and the Core variable
/// that holds the key's previous entry (`maps:find/2`).
#[derive(Clone)]
pub(in crate::core_erlang) struct ReplParamSeed {
    pub(super) local: String,
    pub(super) previous: String,
}

impl CoreErlangGenerator {
    /// Lowers a last/return-position threading construct into a [`ThreadedExpr`],
    /// binding its logical value (tuple element 1) to a fresh result var. The
    /// value-type / class-method boundaries share the
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
    /// discarded. Redundant parentheses (`^(items collect: …)`) are peeled so
    /// the construct inside is unwrapped rather than leaking its raw tuple.
    ///
    /// ADR 0111 Addendum 4: the Actor boundary no longer routes
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
    ) -> Result<Option<ThreadedExpr>> {
        let expr = expr.unwrap_parens();
        let mut parts: Vec<Document<'static>> = Vec::new();
        let result_var = if let Some(locals) = self.construct_tuple_locals(expr) {
            // ADR 0131 §2 (Phase 2): a loop or fold is a local-threading
            // producer; as the method root's last expression it discards its
            // locals with an explicit `DiscardLocals`.
            let (prelude, value) = self.lower_local_threading_construct(
                expr,
                &locals,
                ProducerTail::Discard,
                FrameId::ROOT,
            )?;
            let prelude_doc = self.verified_root_prelude_doc(&prelude, expr.span());
            let value_doc = self.threaded_value_doc(&value);
            let result_var = self.fresh_temp_var("ThreadedValue");
            parts.push(docvec![
                "    ",
                prelude_doc,
                "let ",
                leaf::var(result_var.clone()),
                " = ",
                value_doc,
                " in\n",
            ]);
            result_var
        } else if self.conditional_producer_locals(expr).is_some() {
            // ADR 0131 §1/§2 (Phase 3): a conditional is a local-threading
            // producer too; as the method root's last expression it discards
            // its locals with an explicit `DiscardLocals`.
            let Some(tv) = self.inline_control_flow_producer_with(expr, ProducerTail::Discard)?
            else {
                return Ok(None);
            };
            let prelude_doc = self.verified_root_prelude_doc(&tv.prelude, expr.span());
            let value_doc = self.threaded_value_doc(&tv.value);
            let result_var = self.fresh_temp_var("ThreadedValue");
            parts.push(docvec![
                "    ",
                prelude_doc,
                "let ",
                leaf::var(result_var.clone()),
                " = ",
                value_doc,
                " in\n",
            ]);
            result_var
        } else if let Some(locals) = self.exception_producer_locals(expr) {
            // ADR 0131 §1/§2 (Phase 3b): `on:do:`/`ensure:` is a local-threading
            // producer too; as the method root's last expression it discards its
            // locals with an explicit `DiscardLocals`.
            let (prelude, value) =
                self.lower_exception_producer(expr, &locals, ProducerTail::Discard, FrameId::ROOT)?;
            let prelude_doc = self.verified_root_prelude_doc(&prelude, expr.span());
            let value_doc = self.threaded_value_doc(&value);
            let result_var = self.fresh_temp_var("ThreadedValue");
            parts.push(docvec![
                "    ",
                prelude_doc,
                "let ",
                leaf::var(result_var.clone()),
                " = ",
                value_doc,
                " in\n",
            ]);
            result_var
        } else if self.is_conditional_with_vt_local_threading(expr) {
            match self.emit_vt_conditional_case_to_var(expr, &mut parts)? {
                Some(result_var) => result_var,
                None => return Ok(None),
            }
        } else if self.is_exception_construct_with_vt_local_threading(expr) {
            // `on:do:`/`ensure:` — the third construct family this
            // recognizes, alongside loops/foldl-list-ops and read+write
            // conditionals. (ADR 0122: this predicate also answers `true`
            // for `SelfVt` family threading, folding what used to be a
            // two-predicate OR here into one call.)
            self.emit_vt_exception_tuple_unwrap_to_var(expr, &mut parts)?
        } else {
            return Ok(None);
        };
        Ok(Some(ThreadedExpr {
            value_doc: Document::Vec(parts),
            result_var,
            state_var: None,
        }))
    }

    /// Actor-boundary transform for a last-position
    /// control-flow construct that threads state (field mutations,
    /// conditionals, loops, foldl list-ops, exception handlers). Such a
    /// construct lowers — via the actor-context `expression_doc` path — to a
    /// `{Value, NewState}` tuple whose element 2 **is** the `gen_server`
    /// `State` map (with any `__local__`-prefixed outer-local keys threaded
    /// in). Binds element 1 to a fresh result var, rebinds the state version
    /// to element 2 through a real [`ThreadedStmt::Bind`], and closes with
    /// the Actor `{'reply', Result, NewState}` tail.
    ///
    /// ADR 0111 Addendum 4: this NEVER declines — the
    /// caller is `gen_server/methods.rs`'s already-classified
    /// `BodyExprKind::ControlFlowWithMutations` arm, and `classify_body_expr`
    /// is the single classification pass, so there is no second,
    /// independently-computed `control_flow_has_mutations` recheck here to
    /// possibly disagree with it — the "two independently-computed
    /// decisions must agree" drift class `verify_routing_invariant` existed
    /// to compare is unrepresentable by construction, and that check is
    /// deleted.
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

        // ADR 0118 phase 4: when `expr` is itself an inline-
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
        if self.thread_ahead_unwraps(expr) {
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
                " = ",
                form::element(1, leaf::var(tuple_var.clone())),
                " in ",
            ],
            span,
        ));
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, source_version, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Doc(docvec![form::element(
                2,
                leaf::var(tuple_var)
            ),])),
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
    /// The Actor boundary does not route here — see
    /// [`Self::emit_actor_threaded_last_stmts`].
    pub(super) fn emit_threaded_last(
        &mut self,
        expr: &Expression,
        boundary: ThreadingBoundary,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<bool> {
        let Some(threaded) = self.lower_threaded_last(expr)? else {
            return Ok(false);
        };
        body_parts.push(threaded.value_doc);
        body_parts.push(self.threading_result_tail(
            &threaded.result_var,
            threaded.state_var.as_deref(),
            boundary,
        ));
        Ok(true)
    }

    /// Actor-boundary assign-RHS transform for an inline-threaded conditional
    /// (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`, `and:`/`or:`, the
    /// nil-conditional family, `match:`) —
    /// `var := <conditional-with-mutations>`.
    ///
    /// The caller (`gen_server/methods.rs`'s `LocalAssignControlFlow` arm)
    /// already called `thread_ahead(value, ..)`, which splices the
    /// conditional producer's real prelude (its `State` `Bind`) and
    /// registers its already-unwrapped value, so `expression_doc` returns
    /// that value directly. This binds the target to it and rebinds the
    /// `__local__`-threaded sibling outer-locals from the new state.
    ///
    /// ADR 0131 Phase 2 (BT-3749): every other actor
    /// `var := <control-flow-with-mutations>` — a loop, fold, `do:` or
    /// `on:do:`/`ensure:`, whose `{Value, NewState}` tuple this used to
    /// unpack on a raw-tuple path — is a local-threading producer
    /// (`threaded_construct_value`), so that path is gone. Phase 3 makes the
    /// conditional family a local-threading producer too and deletes this.
    pub(super) fn emit_actor_threaded_assign_rhs_stmts(
        &mut self,
        var_name: &str,
        value: &Expression,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let span = value.span();
        if !self.inline_producer_construct(value.unwrap_parens()) {
            return Err(CodeGenError::Internal(
                "emit_actor_threaded_assign_rhs_stmts: a non-conditional construct must be \
                 lowered by threaded_construct_value"
                    .to_string(),
            ));
        }
        let core_var = self.resolve_local_core_var(var_name);
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
        Ok(())
    }

    /// Builds the Document that returns/stores an already-bound threaded result var for
    /// `boundary`. This is the single place the value-type vs class-method divergence
    /// lives — the boundary adapter the design calls for.
    ///
    /// Use Document/docvec! — never format!() for Core Erlang fragments.
    pub(super) fn threading_result_tail(
        &self,
        result_var: &str,
        state_var: Option<&str>,
        boundary: ThreadingBoundary,
    ) -> Document<'static> {
        match boundary {
            ThreadingBoundary::Actor => {
                // gen_server reply. `state_var` is element 2 of the construct's
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
            ThreadingBoundary::ValueType => {
                docvec!["    ", leaf::var(result_var.to_string()), "\n"]
            }
            ThreadingBoundary::ClassMethod => leaf::var(result_var.to_string()),
        }
    }
}

// ─── ADR 0131 Phase 2: the loop / fold local-threading producer ────────────

impl CoreErlangGenerator {
    /// Runs `f` (a method body's lowering) with
    /// [`Self::local_producer_sites`] set to the sequenced operand sites of
    /// every statement of `body` (`beamtalk_core`'s
    /// `sequenced_operand_sites`, the rule the Phase 0 allow-set check reads
    /// too), restoring the previous set afterwards.
    pub(super) fn with_local_producer_sites<T>(
        &mut self,
        body: &[&Expression],
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let sites = body
            .iter()
            .flat_map(|stmt| self.registered_root_producer_site_exprs(stmt))
            .map(Expression::span)
            .collect();
        let saved = std::mem::replace(&mut self.local_producer_sites, sites);
        let result = self.with_value_assign_sites(body, f);
        self.local_producer_sites = saved;
        result
    }

    /// The producer sites of the method-body statement `stmt`: its
    /// `sequenced_operand_sites` (`beamtalk_core`, the rule the Phase 0 check
    /// reads too) and, in an actor method, the loops and folds threading only
    /// `State` at its other operands ([`Self::family_only_operand_site_exprs`],
    /// BT-4310). The one list [`Self::with_local_producer_sites`] registers,
    /// [`Self::statement_has_local_producer`] routes on and
    /// `lower_root_statement_with_producer` checks.
    pub(super) fn root_producer_site_exprs<'a>(&self, stmt: &'a Expression) -> Vec<&'a Expression> {
        let mut sites =
            beamtalk_core::semantic_analysis::block_facts::sequenced_operand_sites(stmt);
        Self::extend_new_sites(&mut sites, self.family_only_operand_site_exprs(stmt));
        sites
    }

    /// [`Self::root_producer_site_exprs`] as [`Self::with_local_producer_sites`]
    /// registers them (the test-only fault `UnsequencedOperandInPlace`
    /// withholds the BT-4310 sites).
    fn registered_root_producer_site_exprs<'a>(&self, stmt: &'a Expression) -> Vec<&'a Expression> {
        let mut sites =
            beamtalk_core::semantic_analysis::block_facts::sequenced_operand_sites(stmt);
        Self::extend_new_sites(
            &mut sites,
            self.registered_family_only_operand_site_exprs(stmt),
        );
        sites
    }

    /// BT-3912 (ADR 0133 Phase 3, BT-3947 cell 2): runs `f` (the lowering of a
    /// plain, Tier 1 closure's body, `body` its statements) with the body as a
    /// frame of its own: a fresh [`FrameId`] registered as a `MethodBody`-kind
    /// [`RebindFrame`] that threads no local (so every local a producer in the
    /// body rebinds is a plain `let`, ADR 0131 §2), [`Self::closure_frame`] set
    /// so [`Self::current_frame`] answers it, and the body's own statements
    /// registered as producer sites like an arm frame's
    /// ([`Self::with_body_producer_sites`] with `arm`: the statement itself when
    /// it is a loop, fold, conditional or exception construct, and its sequenced
    /// operand sites). A closure is a separate Core Erlang scope: the frame it is
    /// written in is not the one its locals are rebound in.
    ///
    /// Outside the REPL only: the REPL root adopts its bindings map whole
    /// (BT-3876), so a closure there keeps its enclosing arm's frame.
    pub(super) fn with_closure_body_frame<T>(
        &mut self,
        body: &[&Expression],
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        if self.is_repl_mode() {
            return f(self);
        }
        self.branch_frame_counter += 1;
        let frame = FrameId::new(self.branch_frame_counter);
        let rebind = ThreadingPlan::closure_rebind_frame(frame);
        let saved = self.closure_frame.replace(frame);
        // (The value-position assignments of the body are already in the set
        // the method body installed: it descends into block literals with the
        // closure rule, `value_position_assignment_spans`.)
        let out = self.with_rebind_frame(rebind, |this| {
            this.with_body_producer_sites(body, false, true, f)
        });
        self.closure_frame = saved;
        out
    }

    /// BT-3912: whether `frame` is the plain closure body being lowered in an
    /// actor instance method. A construct directly in it seeds its scratch map
    /// from the `State` the closure captured, not from an empty map: its arms
    /// may self-send or read a field, which dispatch on (and read) that state
    /// exactly as the closure's other statements do. The scratch is still
    /// private (the closure hands back only its value), so nothing adopts it as
    /// the next `State` ([`Self::conditional_has_private_scratch`]).
    pub(in crate::core_erlang) fn closure_scratch_continues_state(&self, frame: FrameId) -> bool {
        self.closure_frame == Some(frame) && self.in_actor_instance_context()
    }

    /// BT-3912: renders a producer `prelude` lowered in the plain closure body
    /// frame ([`Self::closure_frame`]) to a `Document` the closure's flat
    /// statement sequence splices, after verifying it as that frame's body (a
    /// `MethodBody` node, like [`Self::verified_root_prelude_doc`] does at the
    /// method root): a closure body has no `Vec<ThreadedStmt>` of its own, so
    /// this is where its producers' rebinds are checked. Any other prelude is
    /// rendered by [`Self::threaded_prelude_doc`].
    pub(super) fn closure_prelude_doc(
        &mut self,
        prelude: &[ThreadedStmt],
        span: Span,
    ) -> Document<'static> {
        let Some(frame) = self.closure_frame else {
            return self.threaded_prelude_doc(prelude);
        };
        // BT-4926: no backfill. A plain closure body's producers lower every
        // version step as a real `Bind`/`LocalRebind` (measured: no gaps in
        // the corpus, which `just test-corpus` enforces), so a `State` version bump
        // without a `Bind` here is a verifier error, not a gap.
        let body = prelude.to_vec();
        let wrapper = [ThreadedStmt::MethodBody {
            frame,
            threads: Vec::new(),
            body,
            span,
        }];
        let errors = self.verify_threaded_ir(&wrapper);
        self.report_threaded_ir_verify_errors(
            &errors,
            "plain closure body local-threading prelude must be well-formed",
            span,
        );
        self.threaded_prelude_doc(prelude)
    }

    /// BT-3910: runs `f` (a body's lowering) with [`Self::value_assign_sites`]
    /// set to the value-position local assignments of `body`, judged by the kind
    /// of body it is ([`Self::root_method_kind`]; `None` for a legacy workspace
    /// block body), restoring the previous set afterwards. A REPL input sets its
    /// own once, so this is a no-op there.
    pub(super) fn with_value_assign_sites<T>(
        &mut self,
        body: &[&Expression],
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        if self.is_repl_mode() {
            return f(self);
        }
        // (A legacy workspace block body is lowered like an actor method body.)
        let sites = beamtalk_core::semantic_analysis::block_facts::value_position_assignment_spans(
            body.iter().copied(),
            self.root_method_kind().or(Some(
                beamtalk_core::semantic_analysis::block_facts::RootMethodKind::Actor,
            )),
        );
        let saved = std::mem::replace(&mut self.value_assign_sites, sites);
        let result = f(self);
        self.value_assign_sites = saved;
        result
    }

    /// ADR 0131 Phase 2: the threaded locals of `expr` when it is a
    /// local-threading producer at a sequenced operand site of the method
    /// body being lowered, or (BT-3793) at one of a loop or fold body's
    /// `loop_body_producer_sites` (`with_body_producer_sites` narrows the
    /// site set to the body's statements while it is lowered) — a loop or fold (`beamtalk_core`'s
    /// `producer_body_block`) whose construct tuple threads every local its
    /// blocks write (`ThreadedLocals::lowered == names`, the rule
    /// `operand_producer_writes` applies for the diagnostic), possibly none
    /// when it threads only a family (an actor fold's `State`) — or `None`.
    ///
    /// ADR 0131 Phase 2d (BT-3795): in the REPL, whose root frame threads its
    /// bindings map, a loop or fold at one of the input's
    /// `repl_operand_sites` that writes a binding is a producer that threads
    /// no local on its own: the carrier's element 2 *is* the new bindings map
    /// and the producer adopts it whole (`lower_repl_construct_tuple`).
    ///
    /// Invariant (BT-3800): every REPL construct for which this answers
    /// `Some` (one with a non-empty threaded set) compiles to a
    /// `{Value, Bindings}` tuple and so sets `repl_loop_mutated`; the same flag
    /// is what makes the assembler unpack a construct that is a statement or an
    /// assignment's whole value. It is enforced over a generated corpus of
    /// every producer selector, with and without a producer in the receiver,
    /// by `beamtalk-repl`'s `repl_local_producer.rs`
    /// (`every_threaded_construct_is_a_tuple_and_a_whole_value_one_is_unpacked`).
    pub(super) fn local_producer_locals(&self, expr: &Expression) -> Option<Vec<String>> {
        let inner = expr.unwrap_parens();
        if self.local_producer_sites.is_empty()
            || !self.local_producer_sites.contains(&inner.span())
        {
            return None;
        }
        self.producer_locals_at_site(inner)
    }

    /// [`Self::local_producer_locals`] for an `inner` (paren-unwrapped) that is
    /// already known to sit at a producer site: the rule that says whether it
    /// is a producer there and the locals it threads, independent of whether
    /// the site was registered ([`Self::local_producer_sites`]). The BT-3939
    /// check of a Tier 2 block body asks it of the sites the core rule names,
    /// so a body that registered none cannot also hide them from the check.
    pub(super) fn producer_locals_at_site(&self, inner: &Expression) -> Option<Vec<String>> {
        // BT-3823: the REPL's one rule, for every family.
        if self.is_repl_mode() {
            return self.repl_producer_locals(inner);
        }
        // ADR 0131 Phase 3 (BT-3738): a conditional at a method-root site.
        if let Some(locals) = self.conditional_producer_locals(inner) {
            return Some(locals);
        }
        // ADR 0131 Phase 3b (BT-3817): and an `on:do:`/`ensure:`.
        if let Some(locals) = self.exception_producer_locals(inner) {
            return Some(locals);
        }
        beamtalk_core::semantic_analysis::block_facts::producer_body_block(inner)?;
        let locals = self.construct_tuple_locals(inner)?;
        // A construct that writes no outer local still compiles to a tuple
        // when it threads a family (an actor fold's `State`, a value-type
        // loop's `Self`): it is a producer with no `LocalRebind`, so its
        // tuple never leaks into the operand.
        match self.threaded_locals_of(inner) {
            None => Some(locals),
            Some(set) => (set.lowered == set.names).then_some(set.lowered),
        }
    }

    /// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): the threaded locals of
    /// `expr` when it is a REPL producer by the one rule the Phase 0 check
    /// reads too (`beamtalk_core`'s `repl_producer_writes`): none, since every
    /// family adopts the bindings map whole (`lower_repl_construct_tuple`)
    /// and threads no local of its own; `None` when it is not one.
    fn repl_producer_locals(&self, expr: &Expression) -> Option<Vec<String>> {
        beamtalk_core::semantic_analysis::block_facts::repl_producer_writes(expr, &|name| {
            self.binds_threadable_local(name)
        })?;
        // BT-3947: a `match:` has no construct of its own; it adopts the
        // bindings map like a REPL conditional.
        if beamtalk_core::semantic_analysis::block_facts::match_arms(expr).is_some() {
            return Some(Vec::new());
        }
        self.threaded_locals_of(expr).map(|_| Vec::new())
    }

    /// ADR 0131 Phase 3 (BT-3738): the threaded locals of the conditional
    /// `expr` (paren-unwrapped: `ifTrue:` … `ifNil:ifNotNil:`, `and:`/`or:`)
    /// when it is a local-threading producer at the method root, in an arm
    /// of another conditional producer or in a loop or fold body, or `None`.
    ///
    /// The codegen half of `beamtalk_core`'s `conditional_producer_writes`,
    /// which the Phase 0 check reads: every arm is a block literal
    /// (`conditional_arm_blocks`), the construct writes an outer local, the
    /// set its arms pack is the whole set (`ThreadedLocals::lowered ==
    /// names`, so no write is dropped), and the arms are ones the producer can
    /// lower in this kind of method (`conditional_arms_lower_as_producer`: in a
    /// class or value-type method no nested construct that is not itself a
    /// producer, and in a value-type method no field write). In the REPL it
    /// answers for an operand site that writes a binding. The site set decides
    /// the frame: a method-root site, a method-body assignment's whole value or
    /// statement, or a statement of a conditional producer's arm
    /// (`with_body_producer_sites` with `arm`), or a producer site of a loop or
    /// fold body (`is_loop_body_producer_site`).
    pub(super) fn conditional_producer_locals(&self, expr: &Expression) -> Option<Vec<String>> {
        use beamtalk_core::semantic_analysis::block_facts::{
            conditional_arm_blocks, conditional_arms_lower_as_producer, literal_block_value_arm,
        };
        if self.is_repl_mode() {
            // The REPL threads its bindings map: a conditional (or an
            // evaluated literal block) is a producer by the REPL's one rule
            // (`repl_producer_locals`). Reached here only from the inline
            // conditional lowering (`inline_control_flow_producer_with`);
            // `local_producer_locals` answers for the REPL itself.
            if beamtalk_core::semantic_analysis::block_facts::match_arms(expr).is_none() {
                conditional_arm_blocks(expr)?;
            }
            return self.repl_producer_locals(expr);
        }
        let kind = self.root_method_kind()?;
        // BT-3947: a `match:` is the same construct with one arm per
        // `match:` arm, judged by core's one rule over those arms.
        if let Some(writes) = beamtalk_core::semantic_analysis::block_facts::match_producer_writes(
            expr,
            &|name| self.binds_threadable_local(name),
            kind,
        ) {
            return Some(Self::write_names(writes));
        }
        let arms = conditional_arm_blocks(expr)?;
        if !conditional_arms_lower_as_producer(&arms, kind, &|name| {
            self.binds_threadable_local(name)
        }) {
            return None;
        }
        // BT-3841: an evaluated literal block (`[...] value`, and since
        // BT-4263 `[:y | ...] value: x`) packs its one arm like a conditional
        // does. `threaded_locals_of` leaves the construct's `lowered` set
        // empty (the legacy inline-block path unpacks its own tuple), so the
        // producer reads the set off the arm directly.
        let set = if literal_block_value_arm(expr).is_some() {
            self.block_value_threaded_locals(&arms)?
        } else {
            self.threaded_locals_of(expr)?
        };
        (set.lowered == set.names).then_some(set.lowered)
    }

    /// BT-3841: whether `expr` (paren-unwrapped) is an evaluated literal block
    /// (`[...] value`, `[:y | ...] value: x`, [`literal_block_value_arm`]) that
    /// [`Self::threaded_expression`] lowers as a local-threading producer in the
    /// frame being lowered ([`Self::is_producer_frame_for`]: a method root, a
    /// registered `BranchArm` frame, or a producer site of a loop or fold body),
    /// outside the REPL. The one predicate behind the statement classifiers
    /// (`control_flow_has_mutations`, `is_tier2_value_call`), the prelude probe
    /// and [`Self::inline_producer_construct`], so none of them can send a
    /// producer to the legacy Tier 2 inline path or the other way round.
    pub(super) fn block_value_producer_in_frame(&self, expr: &Expression) -> bool {
        let inner = expr.unwrap_parens();
        !self.is_repl_mode()
            && beamtalk_core::semantic_analysis::block_facts::literal_block_value_arm(inner)
                .is_some()
            && self.is_producer_frame_for(self.current_frame(), inner)
            && self.conditional_producer_locals(inner).is_some()
    }

    /// BT-3982: whether `expr` (paren-unwrapped) is an evaluated literal block
    /// that [`Self::threaded_expression`] lowers as a producer here, wherever
    /// it sits: a local-threading one ([`Self::block_value_producer_in_frame`])
    /// or an actor one that threads `State`
    /// ([`Self::actor_evaluated_block_producer_parts`]). The one predicate the
    /// statement classifiers (`control_flow_has_mutations`,
    /// `is_tier2_value_call`) read, so a statement is lowered by the same
    /// producer as an operand and never through the legacy Tier 2 inline-block
    /// tuple.
    ///
    /// BT-4461: in the REPL, one at a registered REPL producer site (a REPL
    /// arm's or a loop or fold body's) is a producer too, the REPL's one rule
    /// ([`Self::local_producer_locals`]): lowered as the legacy Tier 2 call in
    /// a loop or fold body, it adopted the bindings map but left the bindings
    /// the body holds in variables stale (`ReplCachedBindingStale`).
    pub(super) fn evaluated_block_is_producer(&self, expr: &Expression) -> bool {
        self.block_value_producer_in_frame(expr)
            || self.actor_evaluated_block_producer_parts(expr).is_some()
            || (self.is_repl_mode()
                && beamtalk_core::semantic_analysis::block_facts::literal_block_value_arm(expr)
                    .is_some()
                && self.local_producer_locals(expr).is_some())
    }

    /// The kind of method body being lowered, `None` outside a class,
    /// value-type or actor method (the REPL, a legacy workspace block body).
    fn root_method_kind(
        &self,
    ) -> Option<beamtalk_core::semantic_analysis::block_facts::RootMethodKind> {
        use beamtalk_core::semantic_analysis::block_facts::RootMethodKind;
        // ADR 0137 §3: an actor closure threads no `State`, like a class method
        // (locals through a fresh scratch map, fields in place).
        if self.in_class_method() || self.in_cell_closure() {
            Some(RootMethodKind::Class)
        } else if self.in_actor_instance_context() {
            Some(RootMethodKind::Actor)
        } else if self.context == super::CodeGenContext::ValueType {
            Some(RootMethodKind::ValueType)
        } else {
            None
        }
    }

    /// ADR 0131 Phase 3b (BT-3817): the threaded locals of the `on:do:` /
    /// `ensure:` `expr` (paren-unwrapped) when it is a local-threading producer
    /// at the method root or in an arm frame, or `None`.
    ///
    /// The codegen half of `beamtalk_core`'s `exception_producer_writes`, which
    /// the Phase 0 check reads: the protected body and the handler or cleanup are
    /// block literals (`exception_arm_blocks`), the construct writes an outer
    /// local, the set its blocks pack is the whole set
    /// (`ThreadedLocals::lowered == names`, so no write is dropped), and the
    /// blocks are ones the producer can lower in this kind of method
    /// (`exception_send_lowers_as_producer`). In the REPL it answers `None`:
    /// every REPL producer is decided by [`Self::repl_producer_locals`]
    /// (`local_producer_locals`), and every other caller is gated on a method
    /// frame.
    pub(super) fn exception_producer_locals(&self, expr: &Expression) -> Option<Vec<String>> {
        use beamtalk_core::semantic_analysis::block_facts::{
            exception_arm_blocks, exception_send_lowers_as_producer,
        };
        let arms = exception_arm_blocks(expr)?;
        let kind = self.root_method_kind()?;
        if !exception_send_lowers_as_producer(expr, &arms, kind, &|name| {
            self.binds_threadable_local(name)
        }) {
            return None;
        }
        let Some(set) = self.threaded_locals_of(expr) else {
            // ADR 0137: an actor `on:do:`/`ensure:` whose blocks thread `State`
            // (they write a field, send to `self`, or read state the method holds
            // ahead of the cell) answers `{Value, State}`; at any position it is a
            // producer with an empty local set, so its consumer reads the value back.
            if kind == beamtalk_core::semantic_analysis::block_facts::RootMethodKind::Actor
                && self.context == crate::core_erlang::CodeGenContext::Actor
                && !self.threads_no_actor_state()
                && arms
                    .iter()
                    .any(|block| self.block_needs_exception_threading(block))
            {
                return Some(Vec::new());
            }
            // No outer local is written: the core rule alone decides whether a
            // field-only value-type exception is a producer (an empty set).
            return beamtalk_core::semantic_analysis::block_facts::exception_producer_writes(
                expr,
                &|name| self.binds_threadable_local(name),
                kind,
            )
            .map(|_| Vec::new());
        };
        (set.lowered == set.names).then_some(set.lowered)
    }

    /// Whether `expr` (paren-unwrapped) is an `on:do:`/`ensure:` that
    /// [`Self::threaded_expression`] lowers as a local-threading producer in the
    /// frame being lowered ([`Self::is_producer_frame_for`]: a method root, a
    /// registered `BranchArm` frame, or a producer site of a loop or fold body),
    /// outside the REPL (whose root adopts the bindings map and lowers its
    /// constructs itself). The one predicate behind
    /// [`Self::threaded_expression`]'s exception hook and
    /// [`Self::subexpr_needs_prelude`]'s probe.
    pub(super) fn exception_producer_in_frame(&self, expr: &Expression) -> bool {
        let inner = expr.unwrap_parens();
        !self.is_repl_mode()
            && self.is_exception_producer_frame(self.current_frame(), inner)
            && self.exception_producer_locals(inner).is_some()
    }

    /// Whether `frame` lowers the rebinds of the `on:do:`/`ensure:` `expr`: the
    /// method root, a `BranchArm` frame ([`Self::is_conditional_producer_frame`]),
    /// or the registered frame of a loop or fold body when `expr` is one of that
    /// body's producer sites (`with_body_producer_sites`, the rule the Phase 0
    /// check reads).
    pub(super) fn is_exception_producer_frame(&self, frame: FrameId, expr: &Expression) -> bool {
        self.is_producer_frame_for(frame, expr)
    }

    /// Whether `frame` lowers the rebinds of the conditional, `on:do:` or
    /// `ensure:` `expr` as a local-threading producer: the method root, a
    /// `BranchArm` frame ([`Self::is_conditional_producer_frame`]), or the
    /// registered frame of a loop or fold body when `expr` is one of that
    /// body's producer sites (`with_body_producer_sites`, the rule the Phase 0
    /// check reads). ADR 0131 Phase 3c (BT-3822): the one frame rule for the
    /// conditional and exception families.
    pub(super) fn is_producer_frame_for(&self, frame: FrameId, expr: &Expression) -> bool {
        self.is_conditional_producer_frame(frame)
            || (self.rebind_frames.contains_key(&frame)
                && self
                    .local_producer_sites
                    .contains(&expr.unwrap_parens().span()))
    }

    /// The outer locals the threading construct `expr` (paren-unwrapped)
    /// packs into the construct tuple it compiles to in this context —
    /// possibly none (a value-type loop that threads only `Self`, an actor
    /// fold that threads only `State`) — or `None` when it does not compile
    /// to one. In a class or value-type method that is a loop or fold
    /// (`expr_yields_vt_threaded_tuple`); in an actor instance method, any
    /// control-flow construct with mutations that is not inlined as a
    /// conditional (`control_flow_has_mutations` without
    /// `inline_control_flow_needs_threading`: loops, folds, `do:`,
    /// `on:do:`/`ensure:`), whose element 2 is the next `State`.
    pub(super) fn construct_tuple_locals(&self, expr: &Expression) -> Option<Vec<String>> {
        let yields_tuple =
            if self.threads_no_actor_state() || self.context == super::CodeGenContext::ValueType {
                self.expr_yields_vt_threaded_tuple(expr)
            } else {
                self.in_actor_instance_context()
                    && self.control_flow_has_mutations(expr)
                    && !self.inline_control_flow_needs_threading(expr)
                    // BT-3841: an evaluated literal block is lowered as a
                    // conditional (its own prelude and rebinds), never as a
                    // construct tuple.
                    && !self.block_value_producer_in_frame(expr)
            };
        yields_tuple.then(|| {
            self.threaded_locals_of(expr)
                .map(|set| set.lowered)
                .unwrap_or_default()
        })
    }

    /// ADR 0131 §1 (Phase 2): the local-threading producer for a loop or
    /// fold at a sequenced operand site ([`Self::local_producer_locals`]).
    /// Its own receiver and non-block arguments are sequenced first (they
    /// evaluate before the construct packs its locals), then the construct
    /// tuple is bound and each threaded local rebound from it:
    ///
    /// ```text
    /// prelude: [ <sequenced operands>,
    ///            ConstructTuple { carrier: CF, threads: [t, …] },
    ///            Bind State_{n+1} ← element(2, CF)        (actor instance only),
    ///            LocalRebind { t ← maps:get('__local__t', …) }, … ]
    /// value:   element(1, CF)
    /// ```
    ///
    /// `Ok(None)` for anything else, so [`Self::threaded_expression`] falls
    /// through to its other cases.
    pub(super) fn local_threading_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        let inner = expr.unwrap_parens();
        if self.local_producer_locals(inner).is_none() {
            return Ok(None);
        }
        self.threaded_construct_value(inner, frame)
    }

    /// The producer lowering of a loop or fold that compiles to a construct
    /// tuple ([`Self::construct_tuple_locals`]), wherever the caller has
    /// decided it is one: an operand site ([`Self::local_threading_producer`])
    /// or a method-body assignment's whole value. `Ok(None)` when `expr` is
    /// not such a construct.
    pub(super) fn threaded_construct_value(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        let inner = expr.unwrap_parens();
        // ADR 0131 Phase 3b (BT-3817): an `on:do:`/`ensure:` that is a producer
        // here lowers its protected body, handler and cleanup as `BranchArm`
        // frames.
        if self.exception_producer_in_frame(inner) && self.is_exception_producer_frame(frame, inner)
        {
            return self.exception_producer_value(inner, ProducerTail::Rebind, frame);
        }
        let locals = if self.is_repl_mode() {
            // The REPL root threads no local on its own (see
            // `local_producer_locals`).
            self.threaded_locals_of(inner).map(|_| Vec::new())
        } else {
            self.construct_tuple_locals(inner)
        };
        let Some(locals) = locals else {
            return Ok(None);
        };
        let (prelude, value) =
            self.lower_local_threading_construct(inner, &locals, ProducerTail::Rebind, frame)?;
        self.report_rebinds_visible(&prelude, inner.span());
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// BT-3813: the producer lowering of a `^` value inside a block (a
    /// non-local return) that is a loop or fold compiling to a construct tuple
    /// in a class or value-type method ([`Self::construct_tuple_locals`]), or
    /// `Ok(None)`.
    ///
    /// Invariant, for every scope kind a `^` can sit in (an arm, a handler, a
    /// loop or fold body, any other closure): the value a `^` throws is never
    /// a construct's `{Value, StateAcc}` carrier. A `^` at a producer site of
    /// an arm or loop frame has already been sequenced by that frame's
    /// producer (the `Return` lowering reads the precompiled value back); this
    /// is the lowering for every other `^`, so no site can fall through to the
    /// raw carrier. The `^` ends the method, so like the method root's last
    /// expression the construct discards its threaded locals
    /// (`ProducerTail::Discard`, legitimate only in the root frame, which is
    /// the frame the prelude is lowered and verified in: the method's
    /// rebinds cannot be observed past the throw). The value is
    /// `element(1, CF)`.
    ///
    /// A value-type construct that threads the `Self` family (a fold whose
    /// block writes a field) is not covered either: it keeps the structured
    /// rejection of a field-writing fold in a closed position (BT-3827).
    ///
    /// An actor instance method is not covered here: its `^` value goes
    /// through [`Self::threaded_expression`], whose construct lowering steps
    /// the `State` family the throw carries; the REPL threads its own
    /// bindings map.
    pub(super) fn nlr_return_construct_value(
        &mut self,
        value: &Expression,
    ) -> Result<Option<ThreadedValue>> {
        if self.is_repl_mode() || self.in_actor_instance_context() {
            return Ok(None);
        }
        let inner = value.unwrap_parens();
        let Some(locals) = self.construct_tuple_locals(inner) else {
            return Ok(None);
        };
        // A value-type fold that writes a field also carries `Self`; a `^`
        // inside a block has no reader for that slot in the root frame, so it
        // keeps its structured rejection (`value_self_fold_slot`, BT-3827).
        if !self.vt_loop_threaded_families(inner).as_slice().is_empty() {
            return Ok(None);
        }
        let (prelude, value) = self.lower_local_threading_construct(
            inner,
            &locals,
            ProducerTail::Discard,
            FrameId::ROOT,
        )?;
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// Whether any sequenced operand site of the method-body statement
    /// `stmt` is a local-threading producer ([`Self::local_producer_locals`]),
    /// so the statement must be compiled through [`Self::threaded_expression`]
    /// and its prelude spliced into the method body.
    pub(super) fn statement_has_local_producer(&self, stmt: &Expression) -> bool {
        // BT-3910: a value-position local assignment inside the statement is
        // a producer too.
        !self.contained_local_assignment_targets(stmt).is_empty()
            || !self.local_producer_sites.is_empty()
                && self
                    .root_producer_site_exprs(stmt)
                    .into_iter()
                    .any(|site| self.local_producer_locals(site).is_some())
    }
}

#[cfg(test)]
mod closure_prelude_tests {
    use super::*;
    use crate::core_erlang::threaded_ir::{BindOp, ValueRef, VersionPrefix, VersionedVar};

    /// BT-4926: a plain closure body's producers lower every version step as a
    /// real `Bind`, so `closure_prelude_doc` no longer backfills opaque gaps. A
    /// prelude whose opaque `Statement` advanced `State` without a `Bind` is
    /// reported (a `debug_assert!` in debug, an `internal:` warning in release).
    fn gapped_closure_prelude(generator: &mut CoreErlangGenerator) {
        let span = Span::default();
        generator
            .with_closure_body_frame(&[], |this| {
                let frame = this.closure_frame.expect("closure frame is set");
                let prelude = vec![
                    ThreadedStmt::Statement(docvec!["<opaque, mints State1>"], span),
                    ThreadedStmt::Bind {
                        target: VersionedVar::new(VersionPrefix::State, 2, frame),
                        source: VersionedVar::new(VersionPrefix::State, 1, frame),
                        op: BindOp::Direct(ValueRef::Literal("'_'")),
                        span,
                    },
                ];
                let _ = this.closure_prelude_doc(&prelude, span);
                Ok(())
            })
            .unwrap();
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic(expected = "plain closure body local-threading prelude must be well-formed")]
    fn closure_prelude_doc_reports_an_unexplained_version_gap() {
        let mut generator = CoreErlangGenerator::new("test");
        gapped_closure_prelude(&mut generator);
    }

    #[cfg(not(debug_assertions))]
    #[test]
    fn closure_prelude_doc_reports_an_unexplained_version_gap() {
        let mut generator = CoreErlangGenerator::new("test");
        gapped_closure_prelude(&mut generator);
        assert!(
            generator
                .take_codegen_warnings()
                .iter()
                .any(|d| d.message.contains("plain closure body")),
            "release builds report an internal warning"
        );
    }
}
