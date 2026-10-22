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
//!   the value-type `{Result, Self{N}}` / bare-`Result` shape, the class-method
//!   bare-`Result` shape, or the Actor
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
    self, BindOp, CarrierSlot, FrameId, RebindFrame, RebindFrameKind, RebindLowering, RebindShape,
    ThreadedLocalSlot, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, VersionedVar,
};
use super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

/// ADR 0131 Phase 2 (BT-3748 review): test-only fault injection into the
/// local-threading producer and the §1a sequencing rule, so each ADR 0131 §4
/// verifier check can be shown to fire on a real compile rather than only on
/// hand-built IR (`tests/local_rebind_verifier_corpus.rs`), and a recorder of
/// the producer lowerings a compile performs
/// (`tests/local_producer_agreement.rs`).
#[cfg(test)]
pub(super) mod fault {
    use beamtalk_core::source_analysis::Span;
    use std::cell::{Cell, RefCell};

    /// One producer lowering: the construct's span and its threaded locals.
    pub(in crate::core_erlang) type ProducerRecord = (Span, Vec<String>);

    /// One deliberately broken lowering.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(in crate::core_erlang) enum LocalRebindFault {
        /// `sequence_children` reads a plain-variable sibling in place even
        /// though a later sibling rebinds it (`LocalReadAfterSiblingRebind`).
        SkipSiblingSnapshot,
        /// The producer omits its last `LocalRebind` (`ThreadedLocalDropped`).
        DropRebind,
        /// The producer lowers a `LocalRebind` as a map `Put` at the method
        /// root, which threads nothing (`LocalRebindModeMismatch`).
        WrongLowering,
        /// The producer records a frame other than the one enclosing it
        /// (`LocalRebindFrameMismatch`).
        WrongFrame,
        /// BT-3821: a value-type conditional producer omits the merge `Bind` of
        /// the `Self` family its arms advanced (`ConstructFamilyDropped`).
        DropFamilyBind,
        /// BT-3827: a value-type fold lambda omits the `Return` that carries the
        /// `Self` family its body advanced out through the accumulator
        /// (`FoldFamilyNotCarried`).
        DropFoldFamilySlot,
        /// BT-3857: the producer's last `MapPut` rebind is rewritten into a
        /// `MapHeld` over the version that rebind would have produced, read
        /// through it. That version is never produced (a held rebind steps
        /// nothing), so the elided write is not an identity
        /// (`HeldRebindNotIdentity`).
        StaleHeld,
        /// BT-3863: a fold body's statement classifier recognises none of
        /// its statements as a local-threading producer or a construct with
        /// carried mutations, while the fold's own gate still threads the
        /// locals they write: the gate split BT-3839 removed. A fold whose
        /// last statement is such a construct then ends in its raw value
        /// instead of its accumulator (`FoldLocalsNotCarried`).
        SplitFoldStatementGate,
        /// BT-3911: an assignment to a local its arm frame does not carry is
        /// lowered with the scratch-map `Put` every assignment used to get,
        /// whatever the frame carries (`ArmLocalPutNotCarried`).
        PutUncarriedLocal,
        /// BT-3910: the producer gate for a value-position local assignment
        /// answers `false`, so the assignment reaches the value-only lowering
        /// (`UnthreadedLocalAssignment`).
        NotAProducer,
        /// BT-3939: a Tier 2 block body registers no producer sites, so a loop
        /// or fold statement of it is lowered inside an opaque `let _ = ( ... )`
        /// instead of as a producer (`ProducerSiteNotThreaded`): the shape that
        /// answered the pre-loop value of a block-local the loop rebinds.
        Tier2OpaqueProducers,
        /// BT-4002: a `Letrec` loop takes its families from its body only, as
        /// before BT-4002, so the `Self` a `whileTrue:`/`whileFalse:` condition
        /// writes is not carried (`ConditionFamilyNotCarried`).
        BodyOnlyLoopFamilies,
        /// BT-4000: an actor conditional's state-advancing receiver is not
        /// sequenced ahead, as before BT-4000: its prelude is rendered into
        /// the conditional's own tuple document, out of the frame's sight, so
        /// in a loop body the conditional's `State` extraction reads a version
        /// the frame never binds (`UnboundVersion`).
        ReceiverPreludeInTuple,
        /// BT-4015: a `whileTrue:`/`whileFalse:` condition's outer-local (in
        /// the REPL, binding) write is not a carried effect, as before BT-4015,
        /// so the condition closure answers only its boolean and the write
        /// dies with it (`ConditionFamilyNotCarried` over a `Local` family).
        ConditionWritesNotCarried,
        /// BT-4017: a fold body statement that `thread_ahead` would register
        /// unwrapped takes the fold's raw `{Value, StateAcc}` unpack instead,
        /// as before BT-4017. With a non-literal block argument the
        /// conditional is a plain send, so the unpack raises `badarg` at run
        /// time; the verifier and `erlc` cannot see a run-time value's shape,
        /// so the corpus's codegen-shape check is what catches it.
        FoldRawTupleUnpack,
        /// BT-4014: a class-side self-send's arguments are not sequenced, as
        /// before BT-4014, so a producer in one is compiled inside the send's
        /// closed argument list and its `LocalRebind`s are scoped there
        /// (`ProducerSiteNotThreaded` at the method root).
        ClassSendOperandsInPlace,
    }

    thread_local! {
        static ACTIVE: Cell<Option<LocalRebindFault>> = const { Cell::new(None) };
        static PRODUCERS: RefCell<Option<Vec<ProducerRecord>>> = const { RefCell::new(None) };
    }

    /// Records a producer lowering of the construct at `span` threading
    /// `locals`, when [`recording_producers`] is collecting.
    pub(in crate::core_erlang) fn record_producer(span: Span, locals: &[String]) {
        PRODUCERS.with(|p| {
            if let Some(records) = p.borrow_mut().as_mut() {
                records.push((span, locals.to_vec()));
            }
        });
    }

    /// Runs `body`, returning its result and every local-threading producer
    /// lowering it performed (the construct's span and threaded locals).
    pub(in crate::core_erlang) fn recording_producers<T>(
        body: impl FnOnce() -> T,
    ) -> (T, Vec<ProducerRecord>) {
        PRODUCERS.with(|p| *p.borrow_mut() = Some(Vec::new()));
        let result = body();
        let records = PRODUCERS
            .with(|p| p.borrow_mut().take())
            .unwrap_or_default();
        (result, records)
    }

    /// The fault active on this thread, if any.
    pub(in crate::core_erlang) fn active() -> Option<LocalRebindFault> {
        ACTIVE.with(Cell::get)
    }

    /// Runs `body` with `fault` active, clearing it afterwards even if
    /// `body` panics.
    pub(in crate::core_erlang) fn with<T>(fault: LocalRebindFault, body: impl FnOnce() -> T) -> T {
        struct Reset;
        impl Drop for Reset {
            fn drop(&mut self) {
                ACTIVE.with(|c| c.set(None));
            }
        }
        ACTIVE.with(|c| c.set(Some(fault)));
        let _reset = Reset;
        body()
    }
}

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
    /// escape and is discarded. `Some` for the Actor boundary, where element 2
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
        position: ThreadingPosition,
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
            threaded_locals: Vec::new(),
            position,
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
        position: ThreadingPosition,
        boundary: ThreadingBoundary,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<bool> {
        let Some(threaded) = self.lower_threaded_last(expr, position)? else {
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
        let core_var = self
            .lookup_var(var_name)
            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
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
            .flat_map(|stmt| {
                beamtalk_core::semantic_analysis::block_facts::sequenced_operand_sites(stmt)
            })
            .map(Expression::span)
            .collect();
        let saved = std::mem::replace(&mut self.local_producer_sites, sites);
        let result = self.with_value_assign_sites(body, f);
        self.local_producer_sites = saved;
        result
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
        let body = threaded_ir::backfill_opaque_version_gaps(
            prelude,
            frame,
            &[threaded_ir::VersionPrefix::State],
        );
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
        // BT-3841: an evaluated literal block (`[...] value`) packs its one arm
        // like a conditional does. `threaded_locals_of` leaves the construct's
        // `lowered` set empty (the legacy inline-block path unpacks its own
        // tuple), so the producer reads the set off the arm directly.
        let set = if literal_block_value_arm(expr).is_some() {
            self.block_value_threaded_locals(&arms)?
        } else {
            self.threaded_locals_of(expr)?
        };
        (set.lowered == set.names).then_some(set.lowered)
    }

    /// BT-3841: whether `expr` (paren-unwrapped) is an evaluated literal block
    /// (`[...] value`, [`literal_block_value_arm`]) that
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

    /// Whether `thread_ahead` has already spliced `expr` (paren-unwrapped) as a
    /// producer when a consumer compiles it: an inline-threaded conditional or
    /// `match:` ([`Self::inline_control_flow_needs_threading`]) or an
    /// `on:do:`/`ensure:` producer ([`Self::exception_producer_in_frame`]).
    /// `expression_doc` then answers the already-unwrapped value, not a
    /// `{Value, State}` tuple.
    pub(super) fn inline_producer_construct(&self, expr: &Expression) -> bool {
        self.inline_control_flow_needs_threading(expr)
            || self.exception_producer_in_frame(expr)
            || self.block_value_producer_in_frame(expr)
    }

    /// Whether [`Self::thread_ahead`] registers `expr`'s value already
    /// unwrapped, so a consumer must read it back rather than unpack a
    /// `{Value, State}` tuple: an inline-threaded construct
    /// ([`Self::inline_producer_construct`]), or (BT-4000) a conditional whose
    /// receiver is sequenced ahead (`conditional_receiver_is_sequenced`), which
    /// is then lowered over the sequenced receiver as a producer or as a plain
    /// send (`(self done) ifTrue: aBlock`).
    pub(super) fn thread_ahead_unwraps(&self, expr: &Expression) -> bool {
        self.inline_producer_construct(expr.unwrap_parens())
            || self.is_conditional_with_sequenced_receiver(expr)
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
                && beamtalk_core::semantic_analysis::block_facts::sequenced_operand_sites(stmt)
                    .into_iter()
                    .any(|site| self.local_producer_locals(site).is_some())
    }

    /// The producer lowering of an `on:do:`/`ensure:` in `frame`
    /// ([`Self::exception_producer_locals`]), or `Ok(None)` when it is not one
    /// here.
    pub(super) fn exception_producer_value(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
        frame: FrameId,
    ) -> Result<Option<ThreadedValue>> {
        let inner = expr.unwrap_parens();
        let Some(locals) = self.exception_producer_locals(inner) else {
            return Ok(None);
        };
        let (prelude, value) = self.lower_exception_producer(inner, &locals, tail, frame)?;
        Ok(Some(ThreadedValue { prelude, value }))
    }

    /// ADR 0131 §1/§2 (Phase 3b): the local-threading producer for an
    /// `on:do:`/`ensure:`:
    ///
    /// ```text
    /// prelude: [ ConstructTuple { carrier: CF, doc: <try/catch tuple>, threads },
    ///            Bind State_{n+1} ← element(2, CF)           (actor instance only),
    ///            LocalRebind { t ← maps:get('__local__t', …) }, …,
    ///            Statement(Self ← element(3, CF))             (value-type `SelfVt` only) ]
    /// value:   element(1, CF)
    /// ```
    ///
    /// The protected body, handler and cleanup are `BranchArm` frames
    /// (`generate_exception_body_with_threading`) whose threads are the
    /// construct's seeded set; the catch boundary (`OnDoCatch`, the class-variable
    /// snapshot and restore of ADR 0130) is untouched. In a class or value-type
    /// method the scratch map is private (`exception_body_outer_state`), so no
    /// `State` `Bind` exists; in an actor instance method element 2 is the next
    /// `State` version and each rebind reads through it.
    pub(super) fn lower_exception_producer(
        &mut self,
        expr: &Expression,
        locals: &[String],
        tail: ProducerTail,
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, ValueRef)> {
        #[cfg(test)]
        fault::record_producer(expr.span(), locals);
        let span = expr.span();
        let actor = self.in_actor_instance_context();
        let enclosing = self.rebind_frame_of(frame)?;
        let self_families = self.exception_construct_threaded_families(expr);
        let source_version = self.state_version();
        let doc = self.exception_producer_tuple_doc(expr)?;
        let carrier = self.fresh_temp_var("ThreadedResult");
        let element = |k: i64, carrier: &str| {
            docvec![
                "call 'erlang':'element'(",
                leaf::int_lit(k),
                ", ",
                leaf::var(carrier.to_string()),
                ")",
            ]
        };
        let mut family_binds = Vec::new();
        if actor && enclosing.kind.extends_state_chain() {
            let _ = self.next_state_var();
            family_binds.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, self.state_version(), frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(element(2, &carrier))),
                span,
            });
        }
        let mut prelude = self.construct_rebind_prelude(
            &enclosing,
            &carrier,
            doc,
            locals,
            tail,
            family_binds,
            frame,
            span,
        );
        if let Some(rebind) = self.extract_vt_loop_family_slot(&carrier, &self_families, span) {
            prelude.push(ThreadedStmt::Statement(docvec![rebind, " "], span));
        }
        Ok((prelude, ValueRef::Doc(element(1, &carrier))))
    }

    /// Lowers the loop or fold `expr` to its construct-tuple prelude (ADR
    /// 0131 §1) and its logical value, `element(1, CF)`. `locals` is the set
    /// the construct packs ([`Self::construct_tuple_locals`]); `tail` says
    /// whether they are rebound or, at the method root's last expression,
    /// discarded.
    ///
    /// The frame is the method root ([`RebindFrame::implicit_root`]): outside
    /// the REPL it threads nothing, so ADR 0131 §2's table lowers every
    /// rebind to a plain `let` of a fresh variable that `local` is then bound
    /// to. In an actor instance method the construct's element 2 is also the
    /// `State` family's next version: its `Bind` comes first and each
    /// rebind reads through it (`state_first`). In a value-type method a
    /// Letrec loop that threads `Self` carries it at element 3, rebound last.
    pub(super) fn lower_local_threading_construct(
        &mut self,
        expr: &Expression,
        locals: &[String],
        tail: ProducerTail,
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, ValueRef)> {
        // A REPL producer is recorded by `lower_repl_construct_tuple` once it
        // knows the construct produced its `{Value, Bindings}` carrier.
        #[cfg(test)]
        if !self.is_repl_mode() {
            fault::record_producer(expr.span(), locals);
        }
        // The construct's own receiver and non-block arguments evaluate
        // before it packs its locals (ADR 0118 §Decision 3): they are
        // sequenced here, for every tail, so `Rebind` and `Discard` cannot
        // diverge on a producer nested in an operand.
        let children = Self::sequenced_send_children(expr).unwrap_or_default();
        let (mut prelude, scope) = self.sequence_children(&children, frame)?;
        let (stmts, value) = if self.is_repl_mode() {
            self.lower_repl_construct_tuple(expr, frame)?
        } else {
            self.lower_construct_tuple(expr, locals, tail, frame)?
        };
        self.finish_precompiled_scope(scope)?;
        prelude.extend(stmts);
        Ok((prelude, value))
    }

    /// The REPL root's half of [`Self::lower_construct_tuple`] (ADR 0131
    /// Phase 2d, BT-3795; §2's `MethodBody (REPL)` row).
    ///
    /// The REPL threads a loop or fold through its bindings map
    /// (`KeyStyle::ReplPlain`): when the construct compiles to a
    /// `{Value, Bindings}` tuple, the builder says so by setting
    /// `repl_loop_mutated`, the flag the REPL assembler reads to unpack a
    /// statement-level construct. This producer is the unpacker for one at an
    /// operand site, so it consumes the flag: the tuple is bound, element 2 —
    /// the whole new bindings map — becomes the next `State` version, and the
    /// value is element 1.
    ///
    /// The bindings are adopted whole, not rebound one `Put` per local: a name
    /// the construct introduces (`xs do: [:x | y := x]`) is in the map only
    /// when the loop ran, and a per-key `maps:get` would crash on an empty
    /// one. The statement-level REPL path adopts element 2 the same way.
    /// Hence no `LocalRebind`, and an empty `threads`: the construct threads
    /// the `State` family (the bindings map), as an actor fold does.
    ///
    /// A construct that does not compile to a tuple (the flag stayed unset)
    /// is a plain value with no prelude.
    ///
    /// `frame` is the frame the prelude is spliced into: the REPL root, or
    /// (BT-3823) a REPL arm, whose `StateAcc` is the bindings map itself
    /// ([`Self::with_repl_arm_producer_sites`]), so the adoption is the same
    /// there: element 2 is the arm's next `StateAcc`.
    fn lower_repl_construct_tuple(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, ValueRef)> {
        let span = expr.span();
        let enclosing = self.repl_loop_mutated();
        self.set_repl_loop_mutated(false);
        let source_version = self.state_version();
        let previous = self.set_repl_unpack_site(Some(Self::repl_unpack_span(expr)));
        let doc = self.with_cleared_direct_params_loop(|this| this.expression_doc(expr));
        self.set_repl_unpack_site(previous);
        let doc = doc?;
        let yields_tuple = self.repl_loop_mutated();
        self.set_repl_loop_mutated(enclosing);
        if !yields_tuple {
            return Ok((Vec::new(), ValueRef::Doc(doc)));
        }
        // BT-3823 review: recorded only once the carrier exists, so an
        // agreement test counts a REPL site as lowered only when its bindings
        // map is adopted (a site the producer reached without a carrier would
        // drop its write). The bindings the construct writes are recorded: the
        // REPL producer threads no local of its own.
        #[cfg(test)]
        fault::record_producer(
            span,
            &self
                .threaded_locals_of(expr)
                .map(|set| set.names)
                .unwrap_or_default(),
        );
        let carrier = self.fresh_temp_var("ThreadedResult");
        let element = |k: i64, carrier: &str| {
            docvec![
                "call 'erlang':'element'(",
                leaf::int_lit(k),
                ", ",
                leaf::var(carrier.to_string()),
                ")",
            ]
        };
        let _ = self.next_state_var();
        let prelude = vec![
            threaded_ir::build_construct_tuple(carrier.clone(), doc, Vec::new(), span),
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, self.state_version(), frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(element(2, &carrier))),
                span,
            },
        ];
        Ok((prelude, ValueRef::Doc(element(1, &carrier))))
    }

    /// ADR 0131 Phase 2d (BT-3795): compiles `target`, an expression of the
    /// REPL input statement `stmt` (the statement itself, or the value of an
    /// assignment in a multi-statement input), when `stmt` holds a
    /// local-threading producer at one of its `repl_operand_sites`: through
    /// [`Self::threaded_expression`], returning the producer's prelude and the
    /// value, for the REPL assembler to emit ahead of its own
    /// `let Result = <value> in`. `None`, with nothing compiled, for any other
    /// statement — it keeps the assembler's lowering, byte for byte.
    ///
    /// `mutated` is `repl_loop_mutated` after the compile: whether the value
    /// is itself a `{Value, Bindings}` tuple (a construct that is the
    /// statement or the whole value of an assignment, which is not a site),
    /// exactly as `expression_doc_with_repl_mutation_tracking` reports it.
    ///
    /// # Errors
    ///
    /// Returns [`CodeGenError`] if compiling `target` fails.
    pub fn repl_statement_with_producers(
        &mut self,
        stmt: &Expression,
        target: &Expression,
    ) -> Result<Option<ReplProducerDoc>> {
        let sites: std::collections::HashSet<Span> =
            beamtalk_core::semantic_analysis::block_facts::repl_operand_sites(stmt)
                .into_iter()
                .map(Expression::span)
                .collect();
        let saved = std::mem::replace(&mut self.local_producer_sites, sites);
        let result = self.repl_statement_in_sites(stmt, target);
        self.local_producer_sites = saved;
        result
    }

    /// [`Self::repl_statement_with_producers`] once the sites are set.
    fn repl_statement_in_sites(
        &mut self,
        stmt: &Expression,
        target: &Expression,
    ) -> Result<Option<ReplProducerDoc>> {
        if !self.statement_has_repl_producer(stmt) {
            return Ok(None);
        }
        self.set_repl_loop_mutated(false);
        let tv = self.threaded_expression(target, FrameId::ROOT)?;
        let mutated = self.repl_loop_mutated();
        self.set_repl_loop_mutated(false);
        let prelude = self.verified_repl_root_prelude_doc(tv.prelude, target.span());
        let value = self.threaded_value_doc(&tv.value);
        Ok(Some(ReplProducerDoc {
            prelude,
            value,
            mutated,
        }))
    }

    /// Whether any `repl_operand_sites` site of `stmt` is a REPL producer.
    fn statement_has_repl_producer(&self, stmt: &Expression) -> bool {
        // BT-3910: a value-position local assignment in the statement.
        !self.contained_local_assignment_targets(stmt).is_empty()
            || beamtalk_core::semantic_analysis::block_facts::repl_operand_sites(stmt)
                .into_iter()
                .any(|site| self.local_producer_locals(site).is_some())
    }

    /// Runs `f` with the enclosing direct-params loop's list-op mode
    /// (`LoopMode::in_direct_params_loop`) off, restoring it afterwards.
    fn with_cleared_direct_params_loop<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let saved = std::mem::replace(&mut self.loop_mode.in_direct_params_loop, false);
        let out = f(self);
        self.loop_mode.in_direct_params_loop = saved;
        out
    }

    /// The frame a local-threading producer in `frame` lowers its
    /// `LocalRebind`s by (ADR 0131 §2): the loop or fold body registered for
    /// it ([`Self::with_rebind_frame`]), or the method root.
    pub(super) fn rebind_frame_of(&self, frame: FrameId) -> Result<RebindFrame> {
        if frame == FrameId::ROOT {
            return Ok(RebindFrame::implicit_root());
        }
        self.rebind_frames.get(&frame).cloned().ok_or_else(|| {
            CodeGenError::Internal(format!(
                "ADR 0131: a local-threading producer in frame {frame:?} has no registered \
                 enclosing frame"
            ))
        })
    }

    /// Whether `frame` lowers a conditional producer's `LocalRebind`s: the
    /// method root, or a registered `BranchArm` frame (ADR 0131 Phase 3).
    pub(super) fn is_conditional_producer_frame(&self, frame: FrameId) -> bool {
        frame == FrameId::ROOT
            || self
                .rebind_frames
                .get(&frame)
                .is_some_and(|rebind| rebind.kind == RebindFrameKind::BranchArm)
    }

    /// Runs `f` (a loop or fold body's lowering in frame `rebind.frame`)
    /// with `rebind` registered as that frame's [`RebindFrame`], so a
    /// local-threading producer nested in the body lowers its `LocalRebind`s
    /// by it ([`Self::rebind_frame_of`]). `rebind` must be what
    /// [`RebindFrame::of`] reads off the node built around the body
    /// (`ThreadingPlan::rebind_frame`); the verifier's
    /// `LocalRebindModeMismatch` checks that they agree.
    pub(super) fn with_rebind_frame<T>(
        &mut self,
        rebind: RebindFrame,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let frame = rebind.frame;
        let saved = self.rebind_frames.insert(frame, rebind);
        let out = f(self);
        match saved {
            Some(previous) => self.rebind_frames.insert(frame, previous),
            None => self.rebind_frames.remove(&frame),
        };
        out
    }

    /// The producer sites of the loop, fold or arm body statement `stmt`
    /// (`beamtalk_core`'s `loop_body_producer_sites` narrowed by
    /// `is_loop_body_producer_site`): the one rule behind
    /// [`Self::with_body_producer_sites`] and the BT-3939 check that every
    /// such site a Tier 2 block body threads a local through was lowered as a
    /// producer.
    pub(super) fn body_producer_site_exprs<'a>(
        &self,
        stmt: &'a Expression,
        fold: bool,
        arm: bool,
    ) -> Vec<&'a Expression> {
        use beamtalk_core::semantic_analysis::block_facts::{
            LoopBodyContext, is_loop_body_producer_site, loop_body_producer_sites,
        };
        let mut ctx = LoopBodyContext::new(
            stmt,
            fold,
            self.in_fold_frame,
            self.in_actor_instance_context(),
        );
        if arm {
            ctx = ctx.in_arm();
        }
        loop_body_producer_sites(stmt)
            .into_iter()
            .filter(|site| is_loop_body_producer_site(site, &ctx))
            .collect()
    }

    /// BT-3939 / BT-4014: the producer sites of the frame-body statement
    /// `stmt` that thread a local, as `(site span, locals)`, for
    /// [`threaded_ir::verify_producer_sites_threaded`] once the body is built:
    /// [`Self::body_producer_site_exprs`] narrowed to the sites
    /// [`Self::producer_site_locals`] names a producer. Read just before
    /// `stmt` is lowered, so a local an earlier statement bound is live. Empty
    /// in the REPL, whose producers adopt the bindings map. Shared by the
    /// Tier 2 block body and the Letrec and fold bodies, so every frame body
    /// checks its own statements by one rule.
    pub(super) fn frame_producer_sites(
        &self,
        stmt: &Expression,
        fold: bool,
        arm: bool,
    ) -> Vec<(Span, Vec<String>)> {
        if self.is_repl_mode() {
            return Vec::new();
        }
        self.body_producer_site_exprs(stmt, fold, arm)
            .into_iter()
            .filter_map(|site| {
                self.producer_site_locals(site)
                    .map(|locals| (site.unwrap_parens().span(), locals))
            })
            .collect()
    }

    /// BT-4014: the locals the construct at the producer site `site` threads
    /// when it is a local-threading producer there, as
    /// `ProducerSiteNotThreaded` reads it at every frame: a loop, fold,
    /// conditional or exception construct ([`Self::producer_locals_at_site`]),
    /// or a value-position local assignment (BT-3910, whose carrier is a
    /// `ConstructTuple` at its own span), which threads its outermost target.
    /// `None` for anything else.
    pub(super) fn producer_site_locals(&self, site: &Expression) -> Option<Vec<String>> {
        if let Some(locals) = self.producer_locals_at_site(site.unwrap_parens()) {
            return Some(locals);
        }
        if !self.is_local_assignment_producer(site) {
            return None;
        }
        beamtalk_core::semantic_analysis::block_facts::assignments_outside_blocks(site)
            .into_iter()
            .max_by_key(|(_, span)| span.len())
            .map(|(local, _)| vec![local.to_string()])
    }

    /// Runs `f` (a loop or fold body's lowering) with the body's own
    /// statements as local-producer sites (ADR 0131 Phase 2b): each
    /// statement that is itself a loop or fold, and each sequenced operand
    /// site of a statement (`sequenced_operand_sites`: an assignment's whole
    /// value, a `^` value, a send's receiver and arguments, a literal's
    /// elements) — narrowed by `beamtalk_core`'s `is_loop_body_producer_site`,
    /// the rule the Phase 0 check reads too. `fold` says whether the body is a
    /// fold's (`is_fold_frame_body`) rather than a letrec loop's. Restores the
    /// enclosing set afterwards, so a site is a producer only in the body that
    /// lowers it through [`Self::threaded_expression`] under a registered
    /// frame.
    pub(super) fn with_body_producer_sites<T>(
        &mut self,
        body: &[&Expression],
        fold: bool,
        arm: bool,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer_fold = self.in_fold_frame;
        let mut sites: std::collections::HashSet<Span> = self.local_producer_sites.clone();
        for stmt in body {
            sites.extend(
                self.body_producer_site_exprs(stmt, fold, arm)
                    .into_iter()
                    .map(Expression::span),
            );
        }
        let saved = std::mem::replace(&mut self.local_producer_sites, sites);
        let saved_fold = std::mem::replace(&mut self.in_fold_frame, outer_fold || fold);
        let out = f(self);
        self.local_producer_sites = saved;
        self.in_fold_frame = saved_fold;
        out
    }

    /// BT-3910 (ADR 0131 §2): runs `f` (the lowering of a REPL arm, inlined over
    /// the bindings map) with the arm registered as a rebind frame, so a
    /// value-position assignment in it lowers by that frame. The arm's
    /// `StateAcc` *is* the bindings map: the `StateAcc` loop row of §2's table,
    /// every binding a member (`RebindFrame::with_repl_binding`). The frame is
    /// the current branch frame. (A legacy workspace block body inlines its
    /// `[...] value` over the same arm lowering outside the REPL; there the
    /// frame carries nothing, so a write stays arm-local.)
    pub(super) fn with_repl_arm_rebind_frame<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let rebind = RebindFrame {
            frame: self.current_branch_frame(),
            kind: RebindFrameKind::Loop(threaded_ir::ThreadingMode::StateAcc(
                threaded_ir::StateAccFallbackReason::None,
            )),
            threads: Vec::new(),
        };
        self.with_rebind_frame(rebind, f)
    }

    /// BT-3910: the threaded set a legacy (non-`BranchArm`) arm's `Threaded`
    /// wrapper records: that of the `StateAcc` frame registered for `frame` (a
    /// Tier 2 stateful block body, [`Self::with_rebind_frame`]); empty for any
    /// other arm. The verifier re-derives a rebind's shape from the node.
    pub(super) fn registered_state_acc_threads(&self, frame: FrameId) -> Vec<String> {
        match self.rebind_frames.get(&frame) {
            Some(rebind)
                if matches!(
                    rebind.kind,
                    RebindFrameKind::Loop(threaded_ir::ThreadingMode::StateAcc(_))
                ) =>
            {
                rebind.threads.clone()
            }
            _ => Vec::new(),
        }
    }

    /// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): runs `f` (the lowering
    /// of a REPL arm: a conditional arm, an `on:do:`/`ensure:` block or a
    /// `tryDo:` block, inlined over the bindings map) with the arm statements'
    /// `repl_arm_producer_sites` added to the local-producer sites, so a loop,
    /// fold, conditional or exception construct there that writes a binding is
    /// a producer adopting the arm's `StateAcc` (the bindings map itself,
    /// [`Self::lower_repl_construct_tuple`]) instead of leaking its
    /// `{Value, Bindings}` tuple into an operand. A no-op outside the REPL.
    pub(super) fn with_repl_arm_producer_sites<T>(
        &mut self,
        body: &[&Expression],
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        if !self.is_repl_mode() {
            return f(self);
        }
        let mut sites: std::collections::HashSet<Span> = self.local_producer_sites.clone();
        for stmt in body {
            sites.extend(
                beamtalk_core::semantic_analysis::block_facts::repl_arm_producer_sites(stmt)
                    .into_iter()
                    .map(Expression::span),
            );
        }
        let saved = std::mem::replace(&mut self.local_producer_sites, sites);
        let out = f(self);
        self.local_producer_sites = saved;
        out
    }

    /// Whether `expr` is a local-threading producer
    /// ([`Self::local_producer_locals`]) or has one at a sequenced operand
    /// site — the question a loop-body statement lowering asks to leave the
    /// statement to [`Self::threaded_expression`] (which splices the
    /// producer's prelude) instead of unpacking a construct tuple itself.
    pub(super) fn expr_has_local_producer(&self, expr: &Expression) -> bool {
        // BT-3910: a value-position local assignment inside it is a producer.
        !self.contained_local_assignment_targets(expr).is_empty()
            || self.local_producer_locals(expr).is_some()
            || beamtalk_core::semantic_analysis::block_facts::loop_body_operand_sites(expr)
                .into_iter()
                .any(|site| self.local_producer_locals(site).is_some())
    }

    /// BT-3910 (ADR 0131 §1): whether `expr` (paren-unwrapped) is a local
    /// assignment in value position, a producer
    /// ([`Self::lower_local_assignment_producer`]). One rule, decided once per
    /// body by `beamtalk_core`'s `value_position_assignment_spans`: a
    /// statement-position assignment is never in the set.
    pub(super) fn is_local_assignment_producer(&self, expr: &Expression) -> bool {
        #[cfg(test)]
        if fault::active() == Some(fault::LocalRebindFault::NotAProducer) {
            return false;
        }
        let inner = expr.unwrap_parens();
        matches!(
            inner,
            Expression::Assignment { target, .. }
                if matches!(target.as_ref(), Expression::Identifier(_))
        ) && self.value_assign_sites.contains(&inner.span())
    }

    /// BT-3910 (ADR 0131 §1, §2 `CarrierSlot::Whole`): lowers the
    /// value-position local assignment `(t := V)` as `V`'s own prelude, then
    /// the assigned value bound as the carrier (`ConstructTuple`), then one
    /// `LocalRebind { local: t, carrier, slot: Whole }` lowered by the
    /// enclosing `frame` (§2's table). Answers the carrier, the assigned value.
    pub(super) fn lower_local_assignment_producer(
        &mut self,
        expr: &Expression,
        frame: FrameId,
    ) -> Result<ThreadedValue> {
        let inner = expr.unwrap_parens();
        let span = inner.span();
        let Expression::Assignment { target, value, .. } = inner else {
            return Err(CodeGenError::Internal(
                "lower_local_assignment_producer on a non-assignment".to_string(),
            ));
        };
        let Expression::Identifier(id) = target.as_ref() else {
            return Err(CodeGenError::Internal(
                "lower_local_assignment_producer on a field assignment".to_string(),
            ));
        };
        let local = id.name.to_string();
        let inner_value = self.threaded_expression(value, frame)?;
        let mut prelude = inner_value.prelude;
        let value_doc = self.threaded_value_doc(&inner_value.value);
        // In a REPL input the frame's map is the bindings map: the assigned
        // binding is a member of it (`RebindFrame::with_repl_binding`), so it is
        // `Put` there.
        let repl_root = self.is_repl_mode() && frame == FrameId::ROOT;
        let enclosing = if repl_root {
            RebindFrame::implicit_root()
        } else {
            self.rebind_frame_of(frame)?
        };
        let enclosing = if self.is_repl_mode() {
            enclosing.with_repl_binding(&local)
        } else {
            enclosing
        };
        let carrier = self.fresh_temp_var("Assigned");
        let slot = ThreadedLocalSlot {
            local: local.clone(),
            slot: CarrierSlot::Whole,
            value_var: self.fresh_temp_var(&local),
        };
        prelude.extend(threaded_ir::build_local_threading_prelude(
            &enclosing,
            &carrier,
            value_doc,
            std::slice::from_ref(&slot),
            Vec::new(),
            span,
            |slot, shape| self.mint_rebind_lowering(slot, shape, frame),
        ));
        // (At the REPL root a binding is read back from the bindings map, which
        // the `Put` above stepped, so it is not bound to a variable.)
        if !repl_root {
            self.bind_var(&local, &slot.value_var);
        }
        #[cfg(test)]
        let prelude = Self::inject_local_rebind_fault(prelude);
        Ok(ThreadedValue {
            prelude,
            value: ValueRef::Var(carrier),
        })
    }

    /// The `RebindLowering` ADR 0131 §2's table assigns the rebind of `slot`
    /// in `shape`'s cell, with its lowering-time names minted from the live
    /// generator: a `LoopParam` steps the local's loop identity (the chain
    /// `final_loop_arg_identities` follows into the recursive call) to
    /// `Gensym(value_var)`; a `MapPut` steps the frame's `State` map by one
    /// version.
    fn mint_rebind_lowering(
        &mut self,
        slot: &ThreadedLocalSlot,
        shape: RebindShape,
        frame: FrameId,
    ) -> RebindLowering {
        match shape {
            RebindShape::Let => RebindLowering::Let,
            RebindShape::LoopParam => {
                let canonical = Self::to_core_erlang_var(&slot.local);
                let current = self
                    .lookup_var(&slot.local)
                    .cloned()
                    .unwrap_or_else(|| canonical.clone());
                let source = if current == canonical {
                    VersionedVar::new(VersionPrefix::Local(slot.local.clone()), 0, frame)
                } else {
                    VersionedVar::new(VersionPrefix::Gensym(current), 1, frame)
                };
                RebindLowering::LoopParam {
                    source,
                    target: VersionedVar::new(
                        VersionPrefix::Gensym(slot.value_var.clone()),
                        1,
                        frame,
                    ),
                }
            }
            RebindShape::MapPut => {
                let source_version = self.state_version();
                let _ = self.next_state_var();
                let target_version = self.state_version();
                let key = match &slot.slot {
                    CarrierSlot::Key(key) => key.clone(),
                    // The REPL's bindings map is keyed by the bare name.
                    CarrierSlot::Whole if self.is_repl_mode() => slot.local.clone(),
                    CarrierSlot::Pos(_) | CarrierSlot::Whole => Self::local_state_key(&slot.local),
                };
                RebindLowering::MapPut {
                    key,
                    source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                    target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                }
            }
        }
    }

    /// The construct-tuple half of [`Self::lower_local_threading_construct`],
    /// once its operands are sequenced.
    fn lower_construct_tuple(
        &mut self,
        expr: &Expression,
        locals: &[String],
        tail: ProducerTail,
        frame: FrameId,
    ) -> Result<(Vec<ThreadedStmt>, ValueRef)> {
        let span = expr.span();
        let actor = self.in_actor_instance_context();
        let enclosing = self.rebind_frame_of(frame)?;
        let self_families = if actor {
            super::control_flow::analysis::ThreadedFamilies::default()
        } else {
            self.vt_loop_threaded_families(expr)
        };
        let source_version = self.state_version();
        // A nested construct returns its own `{Value, StateAcc}` tuple: the
        // enclosing direct-params loop's open-chain list-op forms (which
        // rebind by name and return no tuple) must not apply to it.
        let doc = self.with_fold_self_slot_consumer(expr, |this| {
            this.with_cleared_direct_params_loop(|this| this.expression_doc(expr))
        })?;
        let carrier = self.fresh_temp_var("ThreadedResult");
        let element = |k: i64, carrier: &str| {
            docvec![
                "call 'erlang':'element'(",
                leaf::int_lit(k),
                ", ",
                leaf::var(carrier.to_string()),
                ")",
            ]
        };
        let mut family_binds = Vec::new();
        // In an actor instance frame the carrier's element 2 is the `State`
        // family's next version (ADR 0131 §2). A direct-params or tuple-acc
        // frame has no `State` chain to extend: its body has no state effect
        // (`select_direct_params`/`select_tuple_acc`), so the nested
        // construct's `State` is the one already in scope.
        if actor && enclosing.kind.extends_state_chain() {
            let _ = self.next_state_var();
            family_binds.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, self.state_version(), frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(element(2, &carrier))),
                span,
            });
        }
        let mut prelude = self.construct_rebind_prelude(
            &enclosing,
            &carrier,
            doc,
            locals,
            tail,
            family_binds,
            frame,
            span,
        );
        if let Some(rebind) = self.extract_vt_loop_family_slot(&carrier, &self_families, span) {
            prelude.push(ThreadedStmt::Statement(docvec![rebind, " "], span));
        }
        Ok((prelude, ValueRef::Doc(element(1, &carrier))))
    }

    /// The prelude every local-threading producer ends with (ADR 0131 §1): the
    /// construct tuple bound to `carrier`, the construct's family extraction
    /// `family_binds` and, per `tail`, one `LocalRebind` per entry of `locals`
    /// lowered by `enclosing` (§2's table) or a `DiscardLocals`. Shared by the
    /// loop/fold producer ([`Self::lower_construct_tuple`]) and the conditional
    /// producer, so the two cannot disagree on the rebind shape.
    #[allow(clippy::too_many_arguments)] // one argument per part of the §1 prelude
    pub(super) fn construct_rebind_prelude(
        &mut self,
        enclosing: &RebindFrame,
        carrier: &str,
        doc: Document<'static>,
        locals: &[String],
        tail: ProducerTail,
        family_binds: Vec<ThreadedStmt>,
        frame: FrameId,
        span: Span,
    ) -> Vec<ThreadedStmt> {
        match tail {
            ProducerTail::Rebind => {
                let slots: Vec<ThreadedLocalSlot> = locals
                    .iter()
                    .map(|local| ThreadedLocalSlot {
                        local: local.clone(),
                        slot: CarrierSlot::Key(Self::local_state_key(local)),
                        value_var: self.fresh_temp_var(local),
                    })
                    .collect();
                let prelude = threaded_ir::build_local_threading_prelude(
                    enclosing,
                    carrier,
                    doc,
                    &slots,
                    family_binds,
                    span,
                    |slot, shape| self.mint_rebind_lowering(slot, shape, frame),
                );
                for slot in &slots {
                    self.bind_var(&slot.local, &slot.value_var);
                }
                #[cfg(test)]
                let prelude = Self::inject_local_rebind_fault(prelude);
                prelude
            }
            ProducerTail::Discard => {
                let mut prelude = vec![threaded_ir::build_construct_tuple(
                    carrier.to_string(),
                    doc,
                    locals.to_vec(),
                    span,
                )];
                prelude.extend(family_binds);
                prelude.push(threaded_ir::build_discard_locals(carrier.to_string(), span));
                prelude
            }
        }
    }

    /// Applies the active [`fault::LocalRebindFault`] (test-only) to a
    /// producer's `Rebind` prelude.
    #[cfg(test)]
    fn inject_local_rebind_fault(mut prelude: Vec<ThreadedStmt>) -> Vec<ThreadedStmt> {
        use fault::LocalRebindFault;
        let last_rebind = prelude
            .iter()
            .rposition(|stmt| matches!(stmt, ThreadedStmt::LocalRebind { .. }));
        match (fault::active(), last_rebind) {
            (Some(LocalRebindFault::DropRebind), Some(i)) => {
                prelude.remove(i);
            }
            (Some(LocalRebindFault::WrongLowering), Some(i)) => {
                if let ThreadedStmt::LocalRebind {
                    local, lowering, ..
                } = &mut prelude[i]
                {
                    *lowering = RebindLowering::MapPut {
                        key: Self::local_state_key(local),
                        source: VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT),
                        target: VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT),
                    };
                }
            }
            (Some(LocalRebindFault::StaleHeld), _) => {
                // The last `MapPut` rebind (not necessarily the last rebind).
                let last_put = prelude.iter().rposition(|stmt| {
                    matches!(
                        stmt,
                        ThreadedStmt::LocalRebind {
                            lowering: RebindLowering::MapPut { .. },
                            ..
                        }
                    )
                });
                if let Some(i) = last_put
                    && let ThreadedStmt::LocalRebind {
                        state_first,
                        lowering,
                        ..
                    } = &mut prelude[i]
                    && let RebindLowering::MapPut { key, target, .. } = lowering.clone()
                {
                    *state_first = Some(target.clone());
                    *lowering = RebindLowering::MapHeld { key, map: target };
                }
            }
            (Some(LocalRebindFault::WrongFrame), Some(i)) => {
                if let ThreadedStmt::LocalRebind { frame, .. } = &mut prelude[i] {
                    *frame = FrameId::new(u32::MAX);
                }
            }
            _ => {}
        }
        prelude
    }

    /// BT-3910: [`Self::verified_root_prelude_doc`] for a REPL input
    /// statement's prelude. A value-position assignment `Put`s into the
    /// bindings map, the REPL root's threaded set (§2's `MethodBody (REPL)`
    /// row), so a prelude holding `LocalRebind`s is verified and rendered as
    /// that explicit frame node.
    pub(super) fn verified_repl_root_prelude_doc(
        &mut self,
        prelude: Vec<ThreadedStmt>,
        span: Span,
    ) -> Document<'static> {
        let threads: Vec<String> = prelude
            .iter()
            .filter_map(|stmt| match stmt {
                ThreadedStmt::LocalRebind { local, .. } => Some(local.clone()),
                _ => None,
            })
            .collect();
        if threads.is_empty() {
            return self.verified_root_prelude_doc(&prelude, span);
        }
        let backfilled = threaded_ir::backfill_opaque_version_gaps(
            &prelude,
            FrameId::ROOT,
            &[VersionPrefix::State, VersionPrefix::SelfVt],
        );
        let checked = [threaded_ir::build_method_body(
            FrameId::ROOT,
            threads.clone(),
            backfilled,
        )];
        let errors = self.verify_threaded_ir(&checked);
        self.report_threaded_ir_verify_errors(
            &errors,
            "REPL-root local-threading prelude must be well-formed",
            span,
        );
        let root = [threaded_ir::build_method_body(
            FrameId::ROOT,
            threads,
            prelude,
        )];
        self.threaded_prelude_doc(&root)
    }

    /// Renders a producer prelude a value-type method body splices as
    /// `Document`s (it has no `Vec<ThreadedStmt>` of its own), after
    /// verifying it at the method root like every other method-body IR.
    pub(super) fn verified_root_prelude_doc(
        &mut self,
        prelude: &[ThreadedStmt],
        span: Span,
    ) -> Document<'static> {
        let errors =
            threaded_ir::verify_body_with_opaque_version_gaps(prelude, self.threaded_scope());
        self.report_threaded_ir_verify_errors(
            &errors,
            "method-root local-threading prelude must be well-formed",
            span,
        );
        self.threaded_prelude_doc(prelude)
    }
}
