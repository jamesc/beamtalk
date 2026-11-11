// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadedIr` verification (ADR 0111 § The verifier) — checks a lowered
//! [`super::ThreadedStmt`] slice against the invariants documented on each
//! [`VerifyError`] variant. Depends only on [`super::ir`]; nothing here
//! renders or builds IR.
//!
//! See `docs/development/debugging.md` § `ThreadedIr` verifier for the
//! variant-by-variant reference, and
//! [`CoreErlangGenerator::report_threaded_ir_verify_errors`] for how callers
//! turn a non-empty result into a debug/CI failure or a release diagnostic.

use std::collections::HashMap;

use super::super::nlr::NlrArmShape;
use super::super::{CodeGenContext, CoreErlangGenerator, NlrBoundary};
use super::ir::{
    BindOp, CarrierSlot, CatchClause, CatchEntry, CatchStep, CellBoundary, EnsureCleanup,
    EnsureForeignNlr, EnsureNlrRelay, FoldAccShape, FoldCarry, ForeignCleanupBody, FrameId,
    LOCAL_STATE_KEY_PREFIX, NlrCarry, NlrThrowShape, OnDoCatchVars, RebindFrame, RebindFrameKind,
    RebindLowering, RebindShape, RethrowSlot, StateAccFallbackReason, ThreadedStmt, ThreadingMode,
    UserCodeRegion, ValueRef, VersionPrefix, VersionedVar,
};
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};

// ─── Verifier ───────────────────────────────────────────────────────────────

/// The Core Erlang construct a [`VerifyError::RacingOperands`] finds its
/// operands in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum RacingConstruct {
    /// A list cell (`[A, B | T]`): erlc evaluates the tail before the head.
    ListCell,
    /// A map literal (`~{ K => V, ... }~`): erlc evaluates the last pair first.
    MapLiteral,
}

/// A violated invariant, found by [`verify`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum VerifyError {
    /// BT-4591: a list cell or map literal of the finished Core Erlang holds
    /// `operands` (two or more) operands that may run code (a `call`/`apply`),
    /// so erlc may evaluate them out of source order (BT-4296). Found by
    /// `operand_order_check` over the module text, not by [`verify`], so it
    /// names the top-level `function` rather than a span.
    RacingOperands {
        construct: RacingConstruct,
        function: String,
        operands: usize,
        /// The start of the construct's text, to find it in the module.
        snippet: String,
    },

    /// A versioned var referenced with no producing `Bind` in its frame (or
    /// an ancestor frame currently on the frame stack — the explicit
    /// frame-flow rule). Catches the unbound-`StateX` class one layer earlier
    /// than `core_lint`, with a Beamtalk-source-attributable message instead
    /// of erlc's raw "unbound variable 'State3' in myMethod/2". Diagnosis-quality
    /// improvement over an existing backstop, not net-new detection.
    UnboundVersion { var: VersionedVar, at: Span },

    /// Per-frame linearity: within one `FrameId`, each version produced by
    /// exactly one `Bind`, consumed as the source of at most one successor.
    /// Frame-scoped by design — see [`FrameId`].
    NonLinearVersion {
        var: VersionedVar,
        producers: usize,
        consumers: usize,
    },

    /// Replaces the four "unpack should emit no code" `debug_assert!`s as a
    /// structural property: an optimized `ThreadingMode` node cannot contain
    /// an unpack `Bind`. (In release today this failure degrades to a
    /// `core_lint` unbound-variable error; the gain is a correct,
    /// centralized, source-attributed diagnosis.)
    ThreadingModeUnpackMismatch { mode: ThreadingMode, at: Span },

    /// Invariant class 1: a [`ThreadedStmt::TupleAccUnpack`] node
    /// (the flat positional-unpack accumulator discipline) appeared outside
    /// a [`ThreadingMode::TupleAcc`] body. Mirrors `ThreadingModeUnpackMismatch`
    /// for the tuple-shaped (rather than map-shaped) accumulator; in release
    /// today this degrades to a `core_lint` unbound-variable or badarg-on-
    /// `element/2` error, one layer further from the cause.
    TupleAccUnpackModeMismatch { mode: ThreadingMode, at: Span },

    /// Invariant class 4 ("early-exit accumulator liveness"): a
    /// [`ThreadedStmt::TupleAccUnpack`] node's own `gate_slots` disagrees with
    /// its enclosing [`ThreadingMode::TupleAcc`]'s `gate_slots`. Each list-op
    /// family reserves a different number of leading accumulator slots for
    /// its own result/continuation state (`do:`: 0; `collect:`/`select:`/
    /// boolean-predicate ops: 1; `takeWhile:`/`dropWhile:`/`detect:`/
    /// `partition:`-family: 2) — a mismatch here means the unpack would read
    /// threaded-local values from the wrong tuple positions: well-formed Core
    /// Erlang, silently wrong values (the ADR 0110 danger class, not a
    /// `core_lint` failure). A live check, not scaffolding — see
    /// [`build_tuple_acc_unpack`]'s doc comment for the two independent
    /// sources (`ListOpKind::gate_slots` at lowering time vs. each call
    /// site's own `index_offset - 1` at rendering time).
    EarlyExitGateSlotMismatch {
        mode_gate_slots: usize,
        node_gate_slots: usize,
        at: Span,
    },

    /// BT-4116 (replaces the retired `ValueType`-context rule, invariant
    /// class 2): a tuple-accumulator fold whose plan also threads a family
    /// (`Self`, ...). The tuple accumulator carries locals only; a family
    /// rides the map path (`select_tuple_acc`'s `field_writes` guard).
    TupleAccWithFamily { at: Span },

    /// BT-4116: a `TupleAccUnpack` node's `shape` disagrees with its
    /// enclosing `TupleAcc` mode's (`element` on a bare value, or the whole
    /// tuple bound to one local).
    UnpackShapeMismatch {
        mode: FoldAccShape,
        node: FoldAccShape,
        at: Span,
    },

    /// BT-4116: a [`FoldAccShape::Bare`] unpack with other than exactly one
    /// target (`render_tuple_acc_unpack` would bind each to the whole value).
    BareAccTargets { targets: usize, at: Span },

    /// BT-4116: the fold's accumulator statement disagrees with its mode's
    /// [`FoldAccShape`] — a bare value where the mode unpacks a tuple, or a
    /// tuple where it unpacks a bare value (the next iteration would read
    /// the wrong shape).
    FoldAccShapeMismatch {
        mode: FoldAccShape,
        found: FoldCarry,
        at: Span,
    },

    /// Invariant class 3: the recursive inter-construct fallback
    /// invariant `list_op_needs_stateacc_fallback_recursive` encodes
    /// (`control_flow/mod.rs`), pinned structurally. A nested list-op whose
    /// own inner block cannot use tuple-acc (so it falls back to a
    /// `StateAcc`-map accumulator to thread its cross-scope mutation) is
    /// incompatible with an *enclosing* `DirectParams` loop: `DirectParams`
    /// has no `StateAcc` map for the inner loop's `{value, StateAcc}` result
    /// to unpack into. Fires if `select_direct_params`'s
    /// `!effects.has_non_tuple_safe_list_op` guard is ever dropped or
    /// reordered past the point where `DirectParams` is selected.
    ///
    /// `#[cfg(test)]`: this variant's sole constructor
    /// ([`verify_nested_list_op_stateacc_compat`]) is itself test-only —
    /// see that function's doc comment for why production never reaches
    /// it structurally.
    #[cfg(test)]
    NestedStateAccFallbackUnderDirectParams { at: Span },

    /// ADR 0118 §Decision 5: a [`ThreadedValue`] whose prelude
    /// carries a versioned `Bind` for `prefix` was [`ThreadedValue::close`]d
    /// in a context that cannot thread that prefix
    /// ([`CloseContext::Opaque`]) — the closed `Document` scopes the new
    /// version away, so the state effect the expression performed is lost
    /// to everything after it. This is the "silent drop" class of bug
    /// (a nested actor self-send whose `NewState` is discarded) made into
    /// a verifier finding: the fix is for the enclosing consumer to
    /// *splice* the prelude into its own frame (`stmts.extend(tv.prelude)`)
    /// instead of closing it, or — at a genuine boundary such as a Tier 1
    /// closure body — to surface a user-facing diagnostic built from this
    /// error rather than from a second predicate.
    ///
    /// ADR 0118 phase 1a: constructed by [`ThreadedValue::close`].
    ///
    /// No production caller yet — see [`CloseContext`].
    #[allow(dead_code)]
    StateEffectEscapesExpression { prefix: VersionPrefix, at: Span },

    /// ADR 0130 §4: a compiled `on:do:`'s catch is a class-variable catch
    /// boundary, and its [`ThreadedStmt::OnDoCatch`] node does not honour the
    /// obligation — the entry `let Snap = beamtalk_class_vars:snapshot() in`
    /// is missing, the non-NLR clause does not begin with
    /// `beamtalk_class_vars:restore(Snap)`, or it is missing, or the two
    /// `$bt_nlr` pass-through clauses (3-tuple and actor 4-tuple) are not both
    /// ordered before it. A wrong restore is wrong-value Core Erlang, not an
    /// unbound variable: an error keeps the writes made inside the protected
    /// region, or a `^` throw discards writes it must keep. See
    /// [`CatchRestoreDefect`].
    ///
    /// **What it guards:** today one builder (`on_do_catch_boundary`) makes
    /// every node, verifies it as a one-element IR and renders it at once, so
    /// a well-formed builder cannot trip this. The check exists for a second
    /// constructor, or an edit to that builder, that drops either half; the
    /// `catch_boundary.rs` tests build such nodes directly.
    CatchWithoutClassVarRestore {
        defect: CatchRestoreDefect,
        at: Span,
    },

    /// ADR 0137 §5: a compiled `on:do:` in an actor instance method or an actor
    /// closure is an actor-state catch boundary, and its
    /// [`ThreadedStmt::OnDoCatch`] node does not honour the obligation: its
    /// non-NLR clause must restore the cell to the region's pre-region state
    /// ([`CellBoundary`]) right after the class-variable restore, before the
    /// exception is wrapped and the handler runs. Without it a closure or
    /// callee in the handler reads the writes the failed region made. Inside a
    /// loop that brackets guarded operators (BT-4179,
    /// [`StateRestoreDefect::PreStateOverLiveCell`]) the boundary must
    /// snapshot the cell, not store a threaded `State` that is stale for the
    /// writes the cell holds; that half is checked at the loop's checkout,
    /// against its own bracket decision, and reported through the same
    /// `report_threaded_ir_verify_errors` path as the loop's other findings.
    ///
    /// **What it guards:** like [`Self::CatchWithoutClassVarRestore`], the one
    /// builder (`on_do_catch_boundary`) cannot trip it; the check is for a
    /// second constructor or an edit that drops the step.
    CatchWithoutStateRestore {
        defect: StateRestoreDefect,
        at: Span,
    },

    /// A compiled `on:do:`'s [`ThreadedStmt::OnDoCatch`] opens the exception
    /// class filter ([`CatchStep::ClassFilter`]) but does not close it with
    /// its handler arm followed by [`CatchStep::FilterMiss`], the `'false'`
    /// re-raise and the exhaustive wildcard clause. A filter `case` with no
    /// wildcard is not provably exhaustive to the Core Erlang compiler, and
    /// when the `on:do:` sits inside another protected region `erlc` rejects
    /// the module with `ambiguous_catch_try_state` (BT-3718). Every `on:do:`
    /// in any nesting position closes its filter, so the check is per node.
    CatchFilterNotClosed { at: Span },

    /// BT-3725 (the verifier backing for the BT-3694 fix): a
    /// [`ScopeKind::ClassMethod`] scope opens a `StateAcc` loop that carries no
    /// threaded local, or threads the actor `State` / value-type `Self` family.
    /// A class method has no `State` parameter and threads no family at all
    /// (ADR 0130 §3: a class-variable write is an in-place `put` into the class
    /// process and a self-send rebinds nothing), so the only thing a
    /// class-method `StateAcc` loop may carry is a map of threaded *locals*,
    /// seeded from a fresh `maps:new()`. A `StateAcc` loop with no locals can
    /// only be seeded from the ambient actor `State` — unbound in a class
    /// method, which `erlc` rejects as `unbound variable 'State'` (BT-3694).
    /// See [`ClassMethodDefect`].
    ///
    /// ADR 0131 §3/§4 extends the rule to [`ScopeKind::ValueType`]: a
    /// value-type method has no `State` parameter either, so a method-level
    /// `State` version is the same unbound-`State` bug there (o3/o12/s2 in
    /// value-type methods). Its own `SelfVt` family is legitimate. BT-3999
    /// extends the locals-less loop half too: a value-type `StateAcc` loop that
    /// carries neither a local nor `SelfVt` is seeded from the same unbound
    /// `State` (`[self done] whileTrue: [nil]`).
    ActorStateInClassMethod { defect: ClassMethodDefect, at: Span },

    /// ADR 0131 §4: a [`ThreadedStmt::ConstructTuple`] whose `threads` lists
    /// `local` is not followed, in the same frame, by a
    /// [`ThreadedStmt::LocalRebind`] for `local` before the frame ends or
    /// before anything other than a family `Bind` (a `Direct` step of the
    /// `State`/`SelfVt` family: the extraction that legitimately reads the
    /// carrier first; its value is an opaque `Doc`, so which carrier it reads
    /// is not checked) or a
    /// [`ThreadedStmt::DiscardLocals`] at a `MethodBody` frame — so the
    /// construct's write to `local` is lost (o4, o7, o8, s2, s9, actor
    /// o3/o12). The opaque statement that follows may be the consumer's read
    /// of `element(1, carrier)`; the verifier cannot see inside a
    /// `Statement`, so it treats every one as a potential carrier read.
    ///
    /// Also reported for a `LocalRebind` of `carrier` that its construct's
    /// `threads` does not list (or with no construct of that carrier
    /// open): the transitive-closure gap of ADR 0131 §1, where a frame
    /// rebinds `t` while the construct that carries it out omits `t`.
    /// Applied at every nesting level. A `^` thrown from inside the construct
    /// skips its rebinds by design; this is defined on the fall-through path.
    ThreadedLocalDropped {
        local: String,
        carrier: String,
        at: Span,
    },

    /// ADR 0131 §4: a [`ThreadedStmt::LocalRebind`] lowered to a shape
    /// ([`RebindLowering::shape`](super::ir::RebindLowering::shape),
    /// `lowered_as`) that ADR 0131 §2's table
    /// ([`RebindShape::for_frame`]) does not allow for its enclosing frame's
    /// mode and the local's membership in that frame's `threads`: a plain
    /// `let` for a member of a `StateAcc` frame (the write never reaches the
    /// map the frame threads out), a `Put` for a non-member (a stray
    /// `__local__` key, persisted into an actor's `gen_server` `State` —
    /// the BT-2717 class), a `Gensym` chain entry for a local that is not a
    /// loop parameter. The enclosing frame is read off the node that
    /// actually encloses the rebind ([`RebindFrame::of`]), exactly as the
    /// lowering reads it.
    LocalRebindModeMismatch {
        frame: FrameId,
        mode: RebindFrameKind,
        member: bool,
        lowered_as: RebindShape,
        at: Span,
    },

    /// BT-3811: a [`ThreadedStmt::Bind`] whose `target` was minted in a
    /// different [`FrameId`] than the frame node (or, for a prelude spliced
    /// into a caller's statement list, the caller's frame) that encloses it.
    /// A `Bind` is lowered against one frame's version history and spliced
    /// into another's, so its version step belongs to the wrong frame: the
    /// per-frame linearity and frame-flow checks are then answered against
    /// the wrong frame (a nested hybrid-field rebind threaded ahead with a
    /// hardcoded `FrameId::ROOT` inside a loop-body frame was invisible
    /// because it was flattened to an opaque `Statement`). See
    /// [`verify_bind_frames`].
    BindFrameMismatch {
        var: VersionedVar,
        enclosing: FrameId,
        at: Span,
    },

    /// ADR 0131 §2: a [`ThreadedStmt::LocalRebind`]'s recorded `frame` is not
    /// the frame of the node that encloses it — the rebind was lowered
    /// against one frame's mode and spliced into another, so its shape (and
    /// any `LoopParam`/`MapPut` version step) belongs to the wrong frame.
    LocalRebindFrameMismatch {
        local: String,
        recorded: FrameId,
        enclosing: FrameId,
        at: Span,
    },

    /// ADR 0131 §1a: sibling `at` of a sequenced send reads outer local
    /// `local` in place — compiled by its parent after every sibling's
    /// prelude has been spliced, so by `local`'s post-rebind identity —
    /// while a [`ThreadedStmt::LocalRebind`] for `local` sits in a *later*
    /// sibling's prelude. Source order (and Pharo) says the earlier sibling
    /// reads the value before the rebind: `t + ([t := t + 1. 1] on: Error
    /// do: [:e | 0])` is `0 + 1`. The fix is `sequence_children`'s snapshot
    /// of such a sibling to a `Tmp`; see [`verify_sibling_reads`].
    LocalReadAfterSiblingRebind { local: String, at: Span },

    /// BT-4263 (ADR 0131 §2): a producer handed its enclosing frame a prelude
    /// whose top-level [`ThreadedStmt::LocalRebind`] of `local` (the last one
    /// for that local) binds `value_var`, but the frame's later code reads
    /// `local` from another variable (`bound`, or none): the producer bound the
    /// rebind in a variable scope it then popped. Every statement after the
    /// construct reads the pre-construct value, so the write is silently lost
    /// (`erlc`-clean). The repro is an evaluated block with arguments,
    /// `{[:y | t := y] value: 5, t}`, whose producer binds the parameters in a
    /// scope of its own (`generate_evaluated_block_producer`) and answered
    /// `{5, 0}`. Applies to every producer funnel in every frame, whatever the
    /// scope kind; checked by [`verify_rebinds_visible`].
    RebindNotVisible {
        local: String,
        value_var: String,
        bound: Option<String>,
        at: Span,
    },

    /// BT-3821: a [`ThreadedStmt::ConstructTuple`] declares that its arms or
    /// body advanced `family` (`families`), but no `Direct` [`ThreadedStmt::Bind`]
    /// of that family follows it in the same frame before anything else does.
    /// The construct's `Self`/`State` rebind lives inside its tuple and dies
    /// with the carrier: every later statement reads the pre-construct
    /// version, so the write is silently lost (`self take: (c ifTrue:
    /// [self.m := 1] ifFalse: [0])` then `self.m`). Applies to every
    /// construct in every frame, whatever the scope kind.
    ConstructFamilyDropped {
        family: VersionPrefix,
        carrier: String,
        at: Span,
    },

    /// BT-4206: a [`ThreadedStmt::ConstructTuple`]'s scratch map starts from
    /// `State` version `seed` (`scratch_base`) while an earlier statement of
    /// the same frame already bound the newer version `newest`: the seed was
    /// read before a step that precedes it, so every arm threads from the
    /// stale `State` and the step's field writes are lost when the construct's
    /// `StateAcc` is adopted as the next `State` (BT-4196: a `match:`
    /// producer that read its base `State` before compiling its scrutinee).
    /// Checked over every statement list, at every nesting level.
    StaleConstructSeed {
        seed: VersionedVar,
        newest: VersionedVar,
        carrier: String,
        at: Span,
    },

    /// BT-4612: a store-only [`ThreadedStmt::SyncedCall`] stores `State`
    /// version `reads` (its `reads`) while an earlier statement of the same
    /// frame already bound the newer version `newest`: the store was read
    /// before a step that precedes it, so the cell it fills misses that
    /// step's field writes, and a `^` thrown from a closure the call runs
    /// carries the stale cell out of the method (the `^`-closure pre-store
    /// read before the call's operands were threaded, BT-4532). The same
    /// check as [`Self::StaleConstructSeed`], over every statement list at
    /// every nesting level, including the frame's `State` bound before a
    /// verified slice ([`verify_in_scope_after`]).
    ///
    /// Today the pre-store's builder takes `reads` and the frame's bound
    /// version from the same `frame_state_version()` read with no frame step
    /// in between, so this cannot fire on shipped code. It guards a future
    /// builder that reads the store earlier; only the test-only
    /// `FAULT_PRESTORE_READS_BEFORE_OPERANDS` makes it fire now.
    StalePrestore {
        reads: VersionedVar,
        newest: VersionedVar,
        at: Span,
    },

    /// ADR 0133 Phase 3 (BT-3911), [`Self::ConstructFamilyDropped`] extended
    /// to the `Local` family: a [`ThreadedStmt::BranchArm`]'s body writes the
    /// scratch-map slot of a local (a `Put` of its `__local__` key) that the
    /// arm's frame does not carry (`local` is not in the arm's `threads`).
    /// The write cannot reach the construct's result tuple, so it is dead at
    /// best; at worst it is the sign that the arm was lowered against a
    /// scratch map its frame does not have (a conditional nested in a
    /// `whileTrue:` condition closure wrote its block-local into an unbound
    /// `StateAcc`, BT-3891). A local the arm does not carry is a plain `let`.
    /// Applies to every arm of every frame.
    ArmLocalPutNotCarried { local: String, at: Span },

    /// BT-3910 (ADR 0131 §4): an identifier-target assignment whose value is
    /// consumed reached value-only lowering (`generate_expression`'s
    /// `Expression::Assignment` arm) instead of the value-position producer
    /// (`lower_local_assignment_producer`): `local` is evaluated but never
    /// rebound, so every later read sees its old binding (`x := (t := t + 1) + 1`
    /// then `t`). A consumer that compiled a producer assignment as a plain
    /// sub-expression, or a producer gate that missed the shape, is the cause.
    /// Applies in every frame and context.
    UnthreadedLocalAssignment { local: String, span: Span },

    /// BT-3982 (ADR 0118 §Decision 2): an evaluated literal block (`[...]
    /// value`, `[:y | ...] value: x`) reached the legacy inline lowering
    /// (`generate_block_value_inline_with_mutations`), whose value is a raw
    /// `{Value, State}` tuple, at a site no body sequencer consumes as a
    /// Tier 2 statement (`generate_tier2_value_call_doc`, which unpacks the
    /// tuple and binds the next `State`). Anywhere else the tuple is the
    /// value (`{[:y | self.count := y] value: 5, self.count}` answered
    /// `{{5, Counter(count: 5)}, 0}`) and the block's `State` is dropped,
    /// `erlc`-clean. An evaluated block that threads `State` is a producer
    /// wherever it sits (`generate_evaluated_block_producer`), so a producer
    /// gate that missed the shape, or a consumer that compiled it as a plain
    /// sub-expression, is the cause. `at` is the block literal's span.
    /// Applies in every actor frame (the REPL unpacks its own tuple).
    UnthreadedEvaluatedBlock { at: Span },

    /// BT-3830: a child an enclosing sequencing pass compiled ahead
    /// (`precompiled_subexprs`) was read back `reads` (> 1) times by its
    /// parent's compile. Its prelude ran once; every read after the first
    /// re-substitutes the value into a second position (or re-threads it), so
    /// a state-effecting child dispatches twice. A never-read entry is the
    /// converse and is a hard `CodeGenError::Internal` in
    /// `finish_precompiled_scope`.
    PrecompiledSubexprReadMoreThanOnce { at: Span, reads: u32 },

    /// BT-3827: a fold's lambda body (`do:`/`collect:`/`inject:into:`) advances
    /// the `Self` family (`family`, a top-level `Bind` chain in the body's own
    /// frame) but does not return that chain's final version out through the
    /// fold accumulator: its last statement is not a [`ThreadedStmt::Return`]
    /// carrying the chain's last `Bind` target. The lambda's `Self{N}` rebind
    /// dies with the closure, so every later statement reads the pre-fold
    /// `Self` and the field write is silently lost (`erlc`-clean) — the
    /// shape the fold lowering used to reject rather than miscompile. Applies
    /// to every fold of every kind; checked by [`verify_fold_family_carried`].
    FoldFamilyNotCarried { family: VersionPrefix, at: Span },

    /// BT-3863 (ADR 0131 §4): a fold frame that threads outer locals
    /// (`threads`, non-empty) does not end its lambda body in the
    /// accumulator its mode carries them in: its last statement is not a
    /// [`ThreadedStmt::FoldAccumulator`] whose `carry` is the repack of
    /// `threads` (in order) for a tuple accumulator, or the `StateAcc` map
    /// for a map accumulator. The next iteration then reads its locals out
    /// of whatever the body ended with (BT-3839: a nested loop's raw value
    /// in place of the repacked tuple), and every write is silently lost
    /// (`erlc`-clean). `found` is the carry the body ended in, or `None`
    /// when its last statement is not a fold accumulator at all. Checked
    /// by [`verify_fold_locals_carried`].
    ///
    /// Scope: the check is on the accumulator's *shape*, not on which
    /// version of each local (or which `StateAcc`) it reads. In production
    /// every `Locals` carry is the plan's own `threaded_locals`, so what it
    /// rules out is a fold that threads locals ending in a raw value (only
    /// the `do:` tail can) or in the other mode's accumulator. A write lost
    /// inside an opaque statement before a correctly shaped tail (a nested
    /// loop the statement classifier missed, ending a `collect:` body) is
    /// the loop-body producer-agreement check's to catch, not this check's
    /// (proved under the same fault by `fold_locals_carried_corpus.rs`).
    FoldLocalsNotCarried {
        threads: Vec<String>,
        found: Option<FoldCarry>,
        at: Span,
    },

    /// BT-3835: a [`ThreadedStmt::LocalRebind`] lowered to
    /// [`RebindLowering::MapHeld`](super::ir::RebindLowering::MapHeld) — the
    /// `MapPut` cell with its `Put` elided as an identity write — whose
    /// elision is not provably the identity: its read is not a `Key` read of
    /// its own `key` through `state_first == map`, `map` is not a `State`
    /// version of the enclosing frame, or a later `State` step of that frame
    /// in the same statement list has moved the frame's map past `map`. The
    /// elided write would then be a real one (a different value, or into a
    /// different/stale map): the local's new value never reaches the map the
    /// frame threads out, and the write is silently lost (`erlc`-clean).
    HeldRebindNotIdentity { local: String, at: Span },

    /// BT-3939: a producer site of a frame's body (a loop or fold the core rule
    /// names as a local-threading producer there, writing the outer locals
    /// `locals`) has no [`ThreadedStmt::ConstructTuple`] at its span that
    /// threads them. It was lowered inside an opaque statement
    /// (`let _ = ( ... )`), so its `LocalRebind`s are scoped inside that
    /// statement: every later statement of the frame reads the pre-construct
    /// local and the frame returns the pre-construct state map, and the write
    /// is silently lost (`erlc`-clean). The repro is a Tier 2 block body
    /// (`[:xs | n := 0. xs do: [:y | n := n + y]. n]` promoted by an actor
    /// self-send), which answered the pre-loop `n`. Applies to every frame
    /// whose lowering registers its producer sites; checked by
    /// [`verify_producer_sites_threaded`].
    ProducerSiteNotThreaded { locals: Vec<String>, at: Span },

    /// BT-3949: [`Self::ProducerSiteNotThreaded`] for a producer site whose
    /// construct writes no outer local but still compiles to a
    /// `{Value, State}` carrier, because it threads a family (an actor fold
    /// whose block self-sends, so the fold hands back the next `State`). No
    /// [`ThreadedStmt::ConstructTuple`] at the site means the construct was
    /// compiled in place inside the consumer's own document (a map-literal
    /// value, an array element, a keyword argument): the consumer reads the
    /// carrier tuple as the construct's value (a `Tuple` where a `List` was
    /// expected), and the family's next version is dropped with it
    /// (`erlc`-clean). The repro is an actor method's
    /// `flag ifTrue: [#{"xs" => (xs collect: [:s | self wrap: s])}]`, whose
    /// arm registered no producer sites. Checked by
    /// [`verify_producer_sites_threaded`] at every frame that registers its
    /// producer sites.
    ConstructCarrierInPlace { at: Span },

    /// BT-4460 / BT-4462 (ADR 0131 §2): a construct that threads the locals
    /// `locals` (`threaded_locals_of` names them) was compiled in place, a
    /// [`ThreadedStmt::ConstructLocalsInPlace`] after an opaque
    /// `element(2, ...)` extraction of its state carrier, instead of a
    /// [`ThreadedStmt::ConstructTuple`] and one [`ThreadedStmt::LocalRebind`]
    /// per local. Its writes ride in the carrier's scratch keys, which the
    /// frame then folds into its own `State` (an actor's arm-local
    /// `__local__` key leaks into the actor's state), and no frame rule
    /// rebinds the locals, so later statements read their pre-construct
    /// values. The repro is an actor method's `c ifTrue: [m := 0. c ifTrue:
    /// [m := 5. 7] ifFalse: [0]. m] ifFalse: [0]`, whose plain arm (before
    /// BT-4460) registered only loop and fold sites, so the nested
    /// conditional was not a producer there. Until BT-4462 the frame reread
    /// each local out of the carrier (the legacy write-back); that reread is
    /// deleted, so this finding is the only thing standing between such a
    /// shape and a silently dropped write. Every frame registers every
    /// producer at its sites, so no accepted program reaches it; reported by
    /// [`VerifyWalk::walk_stmt`] wherever one is.
    ConstructLocalsNotRebound { locals: Vec<String>, at: Span },

    /// BT-4461 (ADR 0131 §2, the `MethodBody (REPL)` row): a REPL frame other
    /// than the root (a conditional arm, an `on:do:`/`ensure:` block) holds
    /// the bindings it has written in Core variables, and a construct at one
    /// of its producer sites writes `locals`, some of them, but its
    /// [`ThreadedStmt::ConstructTuple`] leaves them out of `threads` (BT-4461): the producer
    /// adopted the bindings map whole (`element(2, carrier)`) and no
    /// [`ThreadedStmt::LocalRebind`] read them back out of it, so the
    /// frame's later reads answer the value from before the construct, and
    /// a loop of the construct that reads them never sees its own writes. The
    /// repros were `c ifTrue: [m := 1. c ifTrue: [m := 5] ifFalse: [nil]. m]
    /// ifFalse: [0]`, answering 1, and `c ifTrue: [m := 1. [m < 5]
    /// whileTrue: [m := m + 1]. m] ifFalse: [0]`, which never ended. Checked
    /// by [`verify_repl_producer_sites_threaded`] in every REPL frame that
    /// registers its producer sites.
    ReplCachedBindingStale { locals: Vec<String>, at: Span },

    /// An [`ThreadedStmt::NlrThrow`] with no slot version, in **every** scope:
    /// it would render `{'$bt_nlr', Tok, Val, {}}`, an empty tuple no catch
    /// unpacks. A throw always carries the state its boundary hands back (the
    /// scope's own single state).
    NlrThrowWithoutSlot { at: Span },

    /// ADR 0146 (BT-4535): an [`ThreadedStmt::NlrThrow`] where the
    /// actor-state cell is live (`cell_live`, an actor instance method's
    /// threaded body) that does not store its state to the cell before it
    /// throws. Invariant, in every scope kind whose state lives in the cell:
    /// the cell is current whenever a `^` starts, because the method's NLR
    /// boundary commits the cell, not the copy the throw carries, and a
    /// cleanup the throw runs reads the cell. The repro: `flag ifTrue:
    /// [^(self.count := self.count + 10)]` (BT-3495's arm lowering) threw
    /// the written `State` without storing it, so the boundary committed
    /// the cell from before the write; found by the test-build check in
    /// `beamtalk_actor_state:commit/2` over `BUnit`.
    NlrThrowWithoutCellStore { at: Span },

    /// BT-4288 / BT-4312: a REPL `^` run in place (`beamtalk-core`'s
    /// `block_facts::repl_eval_returns`: at the root, in a conditional arm,
    /// an evaluated or protected block, a loop or fold body; not the input's
    /// whole last statement) lowered with no NLR boundary to throw to, so it
    /// compiles to its bare value. In a loop's `fun` the loop ignores it and
    /// runs on (`#(1, 2) do: [:x | ^x]` answered the loop's value; `[n < 3]
    /// whileTrue: [^10]` never ended); elsewhere the input's later statements
    /// run on (`x := 1. ^7. x := 2` answered 2; `true ifTrue: [^3]. 5`
    /// answered 5). Invariant, in every scope: a `^` that must end its scope
    /// from inside a construct's `fun`, or before the scope's later
    /// statements, is a throw to the scope's boundary. A
    /// method, a class method and a value-type method mint theirs whenever a
    /// block holds a `^` (`body_has_block_nlr`); the REPL eval mints its own
    /// for exactly these returns (`repl_nlr`). The repros were every REPL
    /// loop or fold whose body held a `^` (BT-4288) and every non-final root
    /// or arm `^` (BT-4312).
    ReplReturnWithoutBoundary { at: Span },

    /// BT-4503: an [`ThreadedStmt::NlrCatch`] in a value-type or class-method
    /// scope whose catch arm answers anything but the bare value
    /// ([`NlrArmShape`](crate::core_erlang::nlr::NlrArmShape)). Invariant, per
    /// scope kind: the function an NLR boundary closes answers what its
    /// callers take as the send's result. A value-type method (or value-type
    /// extension fun) and a class method answer the bare value: `dispatch/3`'s
    /// arm, the own-module typed direct call of the function (BT-4473) and
    /// the runtime's extension apply
    /// (`beamtalk_dispatch:apply_extension_by_arity/4`) all take the return
    /// as is. An actor method answers the `gen_server` reply and a REPL eval its
    /// `{Value, Bindings}` pair; those scopes are not constrained here. The
    /// repro: every value-type method with a block `^` answered `{Result,
    /// Self}`, which only its own `dispatch/3` arm and the BT-4473 direct
    /// call unwrapped, so a value-type
    /// extension with a block `^` answered the pair to the sender.
    MethodNlrAnswersPair { boundary: NlrBoundary, at: Span },

    /// ADR 0146 (BT-4535): an [`ThreadedStmt::NlrCatch`] whose actor reply
    /// commits the state the throw carries (`{'reply', Value, State}`)
    /// instead of the actor's cell (`{'reply', Value,
    /// beamtalk_actor_state:commit(Self, State)}`,
    /// [`NlrArmShape::Reply`]). Invariant, in every scope kind: a `^` that
    /// reaches an actor method's boundary commits the cell, the one place the
    /// actor's fields live; the throw's copy is not the state of record. The
    /// copy misses whatever was written to the cell after the `^` started (an
    /// `ensure:` cleanup of another method it unwound through, ADR 0146 R1)
    /// and, for a block the actor created and another actor ran, is the
    /// block's creation-time capture, so the actor lost every write it made
    /// after creating the block. The repro was BT-4459: `blk := [^1]. self.n
    /// := 5. peer call: blk` left `n` at its value before the write.
    ActorNlrCommitBypassesCell { at: Span },

    /// BT-4312: an [`ThreadedStmt::EnsureCatch`] whose cleanup a `^` of the
    /// scope runs drops the cleanup's writes. Invariant, in every scope kind:
    /// an `ensure:` is not a boundary, so a write its cleanup makes reaches
    /// whichever boundary the in-flight NLR or exception reaches. For an
    /// error the cleanup runs and the error propagates (the boundary that
    /// catches it decides what is kept). A `^` carries the scope's state to
    /// its boundary in the throw (the REPL's bindings map, an actor's
    /// `State`, a value type's `Self`; a class method carries none, its class
    /// variables are written in place), so the catch must run the cleanup from
    /// that carried state and resume the throw with the cleanup's result.
    /// That holds however the cleanup runs: inlined, or applied as a closure
    /// when an operand is not a block literal (which writes the actor-state
    /// cell in place, so the result is the carried state with the cell's
    /// fields over it).
    /// Fires when the cleanup can write the carried state (an inlined one
    /// advances the carried family; a closure one in an actor method; any
    /// cleanup where the state lives in the cell) and the node has no relay
    /// ([`EnsureRelayDefect::NoRelay`]), or its relay re-throws anything but
    /// the cleanup's result ([`EnsureRelayDefect::RethrowsStaleState`]), or,
    /// where the state lives in the cell, a state rebuilt from the `ensure:`
    /// site's bindings instead of from the carried one
    /// (`EnsureRelayDefect::RebuildsCarriedStateAtSite`, test builds). The repros:
    /// `w := 0. [^5] ensure: [w := 9]` at the REPL left `w` at 0 once
    /// BT-4312 made that `^` a throw (BT-4288's loop-body `^` had the same
    /// gap); an actor method's `[^1] ensure: [self.n := 9]` never kept `n`,
    /// nor did `c := [self.n := 9]. [^1] ensure: c`. A `^` foreign to the
    /// `ensure:` (no `token`), such as every one through `Block>>ensure:`
    /// reached with `perform:`, owes no relay: a closure cleanup writes only
    /// the actor-state cell, which the `^`'s method commits (ADR 0146;
    /// [`VerifyError::ActorNlrCommitBypassesCell`]).
    EnsureCleanupLostOnNlr { defect: EnsureRelayDefect, at: Span },

    /// ADR 0146 R1: where the actor-state cell is live (an actor instance
    /// method's threaded body), the clause a foreign `$bt_nlr` takes through
    /// an [`ThreadedStmt::EnsureCatch`] does not store the cleanup's final
    /// `StateAcc` to the cell. Invariant, for every scope kind: a `^` is
    /// never an error for its owner, so every unwinding exit of a scope that
    /// threads actor `State` leaves the cell holding that scope's final
    /// version. `EnsureCatch` is the only node that runs user code while
    /// unwinding, so the rule is its: a `^` of another method of the same
    /// actor (from a block it passed in) that unwinds through an inlined
    /// cleanup which advanced `StateAcc` must find the cleanup's final
    /// version in the cell. Scopes with no actor `State` (a class method
    /// running in its caller, ADR 0142; a value type; the REPL) and a
    /// closure, whose state already is the cell, have no obligation
    /// (`cell_live` is false). The repro: a method calls `self run: [^3]`,
    /// where `run:` does `[blk value] ensure: [self.n := 40]`; the `^` took
    /// the error path, whose cleanup runs with write-through off, so `n`
    /// kept its old value.
    EnsureForeignCleanupNotStored {
        defect: ForeignStoreDefect,
        at: Span,
    },

    /// BT-4002: a [`ThreadedStmt::ConditionalLoop`]'s condition advanced
    /// `family` (its `condition_families`), but no `Bind` of that family in
    /// the condition slot hands the condition's version back into the loop's
    /// frame from one of the loop's own `produces` seeds. The condition is a
    /// closure called once per iteration: a `Self{N}` it binds dies with it
    /// unless the closure returns it and the loop carries it as a family
    /// parameter. Every iteration then restarts from the pre-condition
    /// version, so the write is silently lost (`[self.n := self.n + 1. self.n
    /// < 3] whileTrue: [k := k + 1]` in a value-type method loops forever,
    /// `erlc`-clean). Applies to every loop of every scope kind.
    ///
    /// BT-4015: the same for a `Local` family, an outer local (in the REPL, a
    /// binding) the condition writes. Its new value rides the `StateAcc` map
    /// the condition closure answers, so the condition slot must hold the
    /// [`ThreadedStmt::ConditionHandBack`] that rebinds the loop's `StateAcc`
    /// to it. Without one the closure answers only its boolean and the write
    /// dies with it: the loop re-reads the pre-condition value every iteration
    /// (`[cw := cw + 1. cw < 3] whileTrue: [ck := ck + 1]` at the REPL never
    /// ends), or a body that reads the local reads the closure's own variable
    /// (`erlc`: unbound variable).
    ConditionFamilyNotCarried { family: VersionPrefix, at: Span },

    /// BT-4584: [`Self::ConditionFamilyNotCarried`]'s converse inside the
    /// condition closure. A carried `whileTrue:`/`whileFalse:` condition hands
    /// its `StateAcc` map back to the loop (`ConditionHandBack`), so a statement
    /// of it that writes `local` (an outer local, in the REPL a binding, of the
    /// condition's `Local` families: `name := ...` or a destructuring
    /// `#(a, b) := ...`) must `Put` it into that map at its own span. Without
    /// the `Put` the hand-back is well-formed but the map lacks the write: the
    /// name was bound by a plain `let` that dies with the closure, so the body,
    /// the next iteration and the code after the loop read the old value
    /// (`[x := x + 1. #(w, b) := #(w + x, 0). x < 3] whileTrue: [nil]` at the
    /// REPL left `w` at 5, not 11), `erlc`-clean. Applies to every carried
    /// condition in every scope kind; checked by
    /// [`verify_condition_writes_in_map`].
    ConditionWriteNotInMap { local: String, at: Span },

    /// ADR 0137 §2 (Phase 2a, BT-4020): a call that may run user code in this
    /// process (a non-inlined `value`-family call, a send with a block
    /// argument, a send to an in-process or unknown receiver, an Erlang FFI
    /// call, an unmarked primitive, the dispatching branch of an operator
    /// guard, ...) in an actor instance method was not built into a
    /// [`ThreadedStmt::SyncedCall`], so the cell the closures and aliased
    /// self-sends read holds an earlier `State` than the method's own and the
    /// call's writes are lost to the method. `selector` names the send, `region` the kind of
    /// region the lowering was in when it emitted it without a bracket (the
    /// node that owns the fix). Reported by the generator at the send
    /// emitter, because the sends in an unbracketed region are exactly the
    /// ones that are not in the IR to be walked.
    UnbracketedUserCodeSend {
        selector: String,
        region: UserCodeRegion,
        at: Span,
    },

    /// BT-4524 (ADR 0146 P0, the `SyncedCall` rule for a `^`): a block
    /// literal holding a `^` of this scope was compiled as an actor closure
    /// for a call (`selector`) that stores nothing to the cell first: the
    /// send was classified `UserCodeCall::No` outside every bracket window,
    /// and no conditional-fallback pre-store or `on:do:` entry store covers
    /// it. The closure's `^` carries the cell (`view`), which then holds
    /// an older version than the method's `State`, so the method's earlier
    /// field writes are lost (`self.n := 6. [^2] ensure: [nil]` left `n` at
    /// 0; so did `timesRepeat:`, `to:do:`, `whileTrue:`, `[^2] value` and a
    /// non-Boolean receiver's `ifTrue:`). The invariant: a call that may run
    /// a block holding a `^` of the current scope stores the threaded state
    /// first. Recorded by the send emitter (`note_send`, the operands) and
    /// checked where the block becomes a closure (`with_actor_closure`), so
    /// an operand the lowering inlines is never a finding. A store covers
    /// only the block literals it stores for (`with_nlr_closure_cover`,
    /// BT-4532), never a closure of another send nested in an operand.
    ///
    /// BT-4532 (ADR 0146): the same rule for every operand that may be a
    /// block. A send that runs as a closure something other than a block
    /// literal (a block variable or parameter, any expression:
    /// `SendUserCode::ClosureOperand`) and is left unbracketed is itself the
    /// finding, at the send: the value may hold a `^` of this method.
    UnbracketedNlrClosure {
        selector: String,
        region: UserCodeRegion,
        at: Span,
    },

    /// BT-4121 (ADR 0137): the dispatching branch of a guarded operator
    /// (`selector`, e.g. `+`) in an actor instance method compiled in a loop
    /// that holds no whole `State` (direct-params, hybrid or tuple-accumulator
    /// parameters) with no loop-level bracket around it, so a user-defined
    /// operator that touches this actor's fields would run against a stale
    /// cell and its writes would never load back. The loop that owns the
    /// region brackets the slow path (a store before the loop, a reload
    /// woven into its exit); this fires only when no enclosing loop did (a
    /// loop lowering that never pushed a guarded-operator marker, or a
    /// hybrid loop whose operator the mode selection missed). Detected at
    /// the send emitter and reported there or at the loop checkout, like
    /// [`Self::UnbracketedUserCodeSend`].
    UnbracketedGuardedOperator {
        selector: String,
        region: UserCodeRegion,
        at: Span,
    },

    /// BT-4179 (ADR 0137): a `StateAcc` loop or fold nested in a stateless
    /// loop that brackets guarded operators packs its accumulator from that
    /// loop's entry `State`. The bracket's slow paths have written the cell
    /// past it, so every store the nested construct makes (a bracket, an
    /// `on:do:` or `ensure:` boundary) puts the stale entry back and drops
    /// those writes. The seed must come from the cell (`take()`), as the
    /// loop's own exit pack does. Reported by the loop checkout from its own
    /// bracket decision and entry `State`, like
    /// [`StateRestoreDefect::PreStateOverLiveCell`].
    StaleStateSeedInBracketedLoop { at: Span },

    /// BT-4369 (ADR 0137 §4b): an actor field write was lowered in the body of
    /// a loop that brackets guarded operators. Such a loop holds no whole
    /// `State` (it keeps its variables in parameters), and the prose rule is
    /// that mode selection never routes a body with a field write to it. The
    /// rule matters because a `StateAcc` loop or fold nested in the region
    /// seeds its accumulator from the cell (`take()`, not the `State` in scope):
    /// a field write held only in a frame's threaded `State` has not reached the
    /// cell, so that seed would silently drop it. Each field-write lowering
    /// records the write on the innermost loop marker
    /// (`CoreErlangGenerator::note_field_write`); the checkout that brackets
    /// reports every one from its own decision, and a loop that does not
    /// bracket hands them to the enclosing marker, so a write in a nested
    /// non-bracketing loop is still seen by a bracketing ancestor. A write
    /// inside an actor closure goes through the cell view and records nothing.
    FieldWriteInBracketedLoop { at: Span },

    /// BT-4523 (ADR 0146 P0): a `^` was lowered in the body or condition of a
    /// hybrid-params loop, or of a nested loop that holds no whole `State` and
    /// so hands its uses outward. A `^` inside a nested loop or fold that
    /// threads a whole `State` (`StateAcc`) records nothing here; mode
    /// selection keeps such a body out of hybrid mode instead, because
    /// `facts::block_has_nlr` looks inside nested blocks. A hybrid loop holds
    /// every field it writes in a fun parameter
    /// (`_NField`), so the `State` in scope is the loop-entry map; the `^`
    /// stores that map to the cell and carries it in its throw, dropping
    /// every write the loop made so far (`1 to: 5 do: [:i | self.n := i.
    /// i == 3 ifTrue: [^i]]` answered 3 and left `n` at 0). The invariant,
    /// for every loop mode: a `^` in a loop body stores and carries a state
    /// that includes every field written so far. `StateAcc` threads the whole
    /// `State`; direct-params and tuple-accumulator bodies write no field
    /// (mode selection, checked by `FieldWriteInBracketedLoop`); a hybrid body
    /// therefore admits no `^`. The `^`'s state read is recorded on the
    /// innermost loop marker (`actor_state_read_source`,
    /// `StateReadSite::NlrThrowState`), and each hybrid lowering checks its
    /// marker before checkout.
    NlrThrowInHybridLoop { at: Span },

    /// BT-4180 (ADR 0137 §4b): an actor-state read source emitted inside a
    /// **bracketed region** (the body and condition of a stateless loop
    /// whose checkout bracketed guarded operators, plus everything lowered
    /// inside it) was not the cell. The bracket's slow paths write the cell
    /// past the loop-entry `State`, so a field read, a `^`'s carried state,
    /// an actor closure's creation-time capture or a nested loop's exit seed
    /// taken from a threaded `State` variable or a hybrid loop's
    /// pre-extracted field parameter answers the actor as of loop entry,
    /// silently dropping earlier iterations' writes.
    ///
    /// Each read site records the source it emitted on the innermost loop
    /// marker through one choke point
    /// (`CoreErlangGenerator::actor_state_read_source`); the records travel
    /// outward with the marker's other threaded-`State` uses and the loop
    /// checkout that brackets reports every one taken in a frame that holds
    /// no whole `State`, from its own decision (never from the push-time
    /// prediction `guarded_op_take_reads` that chose the source). A read in
    /// a whole-`State` frame nested in the region (a `StateAcc` loop or
    /// fold) is exempt: that frame's seed is the cell, checked as
    /// [`Self::StaleStateSeedInBracketedLoop`]. A read inside an actor
    /// closure goes through the cell view and records nothing.
    StaleReadInBracketedLoop {
        site: StateReadSite,
        defect: StaleReadDefect,
        region: UserCodeRegion,
        at: Span,
    },

    /// BT-4122 (ADR 0137 Tier 2 merge): a field read from a Tier 2 block's
    /// `StateAcc`. After the merge a Tier 2 `StateAcc` carries only locals —
    /// fields go through the cell — so a Tier 2 fun body accessing fields
    /// via `StateAcc` (a read or a write in a legacy `fun`) is reported
    /// here, forcing the merged shape. `selector` names the access,
    /// suffixed with which arm fired. Own shaping, own gaps list, same
    /// separability pattern as `UnbracketedGuardedOperator`.
    FieldReadFromStateAcc {
        selector: String,
        region: UserCodeRegion,
        at: Span,
    },

    /// BT-4194 (ADR 0137): a [`ThreadedStmt::SyncedCall`] built in a frame
    /// whose `StateAcc` is a private scratch map, not the actor's whole
    /// `State`: a direct-params or tuple-accumulator loop frame, or an arm
    /// frame nested in one (seeded fresh from the loop's locals). The
    /// bracket's `store` would overwrite the cell with that scratch map and
    /// its `load` hand it back as the `State`, losing the loop-level
    /// bracket's snapshot: a loop that holds no whole `State` brackets at the
    /// loop (`note_stateless_guarded_op`), never at a site inside it. Carried
    /// by [`ThreadedStmt::SyncedCall::on_private_scratch`], since a node is
    /// verified without its ancestor frames.
    SyncedCallOnPrivateScratch { at: Span },

    /// BT-4279: a [`ThreadedStmt::SyncedCall`] carries a
    /// [`FastPath`](super::ir::FastPath) but is not a value bracket that
    /// reloads (`result` and `load` both set, no Tier 2 rebinds): the fast
    /// arm hands the stored `State` through as `load` and answers the bracket's
    /// `result`, so a store-only, statement-form or Tier 2 merge bracket has
    /// no shape for it. The emitter renders such a node as the plain bracket.
    FastPathOnUnsplittableBracket { at: Span },

    /// BT-4178: a tuple-accumulator fold body resolved the actor-state map
    /// (a field read, a `^`'s carried state, a closure's capture, a nested
    /// loop's seed) to `read_from`, which is not the fold's entry state
    /// `entry_state`, or is the lambda's own accumulator parameter. The
    /// lambda binds no `StateAcc` map, only a tuple of locals, so the loop
    /// context's `StateAcc` name is unbound there (or, in a `do:` whose
    /// accumulator is named `StateAcc`, is the tuple itself: `badmap`). Its
    /// body writes no field, so the entry state is the one map every
    /// iteration reads. Checked by the fold's own lowering
    /// ([`verify_tuple_acc_state_reads`]) over every name the generator's
    /// field-read accessor answered inside it.
    TupleAccStateReadNotEntryState {
        read_from: String,
        entry_state: String,
        at: Span,
    },
}

/// BT-4178, [`VerifyError::TupleAccStateReadNotEntryState`] over one fold
/// body: in [`ThreadingMode::TupleAcc`] mode every actor-state name the body
/// resolved (`reads`) must be the fold's `entry_state` and must not be the
/// lambda's accumulator parameter `acc_param` (which would shadow it). Any
/// other mode binds a whole state map and is not constrained here.
pub(in crate::core_erlang) fn verify_tuple_acc_state_reads(
    mode: &ThreadingMode,
    acc_param: &str,
    entry_state: &str,
    reads: &[String],
    at: Span,
) -> Vec<VerifyError> {
    if !matches!(mode, ThreadingMode::TupleAcc(..)) {
        return Vec::new();
    }
    let mut errors: Vec<VerifyError> = Vec::new();
    for read in reads {
        let bad = read != entry_state || read == acc_param;
        let seen = errors.iter().any(|e| {
            matches!(e, VerifyError::TupleAccStateReadNotEntryState { read_from, .. } if read_from == read)
        });
        if bad && !seen {
            errors.push(VerifyError::TupleAccStateReadNotEntryState {
                read_from: read.clone(),
                entry_state: entry_state.to_string(),
                at,
            });
        }
    }
    errors
}

/// What a [`ThreadedStmt`] tree is lowered *for*: decides which families the
/// scope may thread (the per-scope-kind invariant [`verify_in_scope`]
/// enforces, BT-3725).
///
/// (`State` inside a threading frame is the `StateAcc` map's version,
/// legitimate in a class method — see `VerifyWalk::check_class_method_family`.)
///
/// | scope         | method-level `State` | `SelfVt` family      | `StateAcc` loop with no locals |
/// |---------------|----------------------|----------------------|--------------------------------|
/// | `ClassMethod` | forbidden            | forbidden            | forbidden                      |
/// | `ValueType`   | forbidden            | not constrained here | not constrained here           |
/// | `Instance`    | not constrained here | not constrained here | not constrained here           |
///
/// **What enforces it:** exactly the entry points that take a `ScopeKind` —
/// [`verify_in_scope`], `verify_body_with_opaque_version_gaps` and
/// `verify_simple_bind` — which every production lowering site reaches via
/// `CoreErlangGenerator::threaded_scope` / `verify_threaded_ir`. Bare
/// [`verify`] is `Instance`-scoped and enforces nothing here; it is used only
/// by tests and test-local wrappers.
///
/// `Instance` (actor instance, REPL and every non-method fixture) is
/// deliberately unconstrained by this check: which of `State`/`SelfVt` is
/// eligible there is decided by `CoreErlangGenerator::eligible_families`
/// (ADR 0122), not re-derived here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum ScopeKind {
    /// A class-side method body (`CoreErlangGenerator::in_class_method`).
    ClassMethod,
    /// ADR 0131 §3: a value-type instance method body
    /// (`CodeGenContext::ValueType`, not a class method). Threads `SelfVt`
    /// when it writes a field, never the actor `State` family.
    ValueType,
    /// BT-3910: a REPL input. Like [`Self::Instance`] it constrains no family,
    /// but its frames' maps are the bindings map: every binding is a member of
    /// the root, an arm and a `StateAcc` loop body (ADR 0131 §2, `MethodBody
    /// (REPL)` row; [`RebindFrame::with_repl_binding`]).
    Repl,
    /// Any other scope.
    Instance,
}

/// What is wrong with a class-method scope
/// ([`VerifyError::ActorStateInClassMethod`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum ClassMethodDefect {
    /// A `StateAcc` loop whose fallback reason is
    /// [`StateAccFallbackReason::NoThreadedLocals`]: it carries nothing, so its
    /// seed is the ambient actor `State` — unbound in a class method, and in a
    /// value-type method unless the loop carries `SelfVt` (BT-3999).
    StateAccLoopWithoutLocals,
    /// A versioned `Bind` / `Return` of the actor `State` family produced at
    /// method level (outside every threading frame, where `State` is not the
    /// `StateAcc` map), or of `SelfVt` anywhere: a class method threads no
    /// family.
    FamilyVersion(VersionPrefix),
    /// BT-4089: a tuple-acc fold's post-fold repack seeds from the ambient
    /// actor `State` in a scope whose frame has none (a class method, a
    /// value-type method, an actor cell closure) — `erlc` rejects the module
    /// with `unbound variable 'State'`. Reported by the repack lowering
    /// itself (`ThreadingPlan::append_repack_stateacc_doc`), which is
    /// AST-directed document emission outside the IR (the
    /// [`VerifyError::UnbracketedUserCodeSend`] shape), not by the structural
    /// walk: the repack's seed is not in any node for the walk to check.
    TupleAccRepackFromAmbientState,
}

/// What is wrong with an [`ThreadedStmt::EnsureCatch`]'s `$bt_nlr` clause
/// ([`VerifyError::EnsureCleanupLostOnNlr`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum EnsureRelayDefect {
    /// The scope's own throw is re-raised untouched.
    NoRelay,
    /// The relay re-throws a state other than the cleanup's result.
    RethrowsStaleState,
    /// Where the state lives in the actor-state cell, the relay re-throws
    /// what a fresh `^` at the `ensure:` site would carry
    /// ([`RethrowSlot::SiteState`]) instead of the carried state with the
    /// cell over it: in a merged Tier 2 fun that re-flushes the captured
    /// locals as bound at the `ensure:`, dropping the values the `^` carried.
    /// Test builds only, with the one node shape that has it.
    #[cfg(any(test, feature = "test"))]
    RebuildsCarriedStateAtSite,
}

/// What is wrong with an [`ThreadedStmt::EnsureCatch`]'s foreign `$bt_nlr`
/// clause ([`VerifyError::EnsureForeignCleanupNotStored`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum ForeignStoreDefect {
    /// There is no foreign clause: the throw takes the error path.
    NoClause,
    /// The clause stores nothing to the cell.
    NoStore,
    /// The clause stores a `StateAcc` version other than the cleanup's final
    /// one.
    StoresStaleVersion { stored: usize, expected: usize },
    /// BT-4598: the clause shares the relay's cleanup
    /// ([`ForeignCleanupBody::SharedWithRelay`]) but there is no relay of the
    /// scope's own throw that runs it from the cell (no token, no relay, or
    /// one that neither stores to the cell nor resumes with a `StateAcc`
    /// version), so the clause has no cleanup to run.
    SharedWithoutCellRelay,
}

/// What is wrong with an [`ThreadedStmt::OnDoCatch`] node
/// ([`VerifyError::CatchWithoutClassVarRestore`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum CatchRestoreDefect {
    /// The entry half is missing: no `let Snap = beamtalk_class_vars:snapshot()
    /// in` before the `try`, so the restore reads an unbound variable.
    NoSnapshotEntry,
    /// There is no non-NLR clause at all.
    NoNonNlrClause,
    /// The non-NLR clause does not begin with the class-variable restore.
    RestoreNotFirst,
    /// The restore reads a variable other than the node's snapshot variable.
    RestoreReadsWrongSnapshot,
    /// An NLR pass-through shape is missing before the non-NLR clause (it is
    /// absent, or ordered after it): a `^` of that shape would be restored.
    NlrArmNotBeforeRestore(NlrThrowShape),
}

/// BT-4180: the codegen site that emitted an actor-state read source
/// ([`VerifyError::StaleReadInBracketedLoop`]). Every site that hands a
/// lowering the actor's state map to read from routes through
/// `CoreErlangGenerator::actor_state_read_source`, which records it. (An
/// `on:do:` boundary's pre-region `State` is a store, noted where the
/// boundary is chosen and reported as
/// [`StateRestoreDefect::PreStateOverLiveCell`]; a `StateAcc` producer's
/// seed is [`VerifyError::StaleStateSeedInBracketedLoop`].)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum StateReadSite {
    /// `self.x` (a `late` field included).
    FieldRead,
    /// A bare field name (the identifier fallback).
    ImplicitFieldRead,
    /// A `^`'s carried state and its pre-throw store.
    NlrThrowState,
    /// The creation-time capture of an actor closure (`with_actor_closure`):
    /// what its reads see away from home.
    ClosureCapture,
    /// The state an `on:do:`/`ensure:` body continues in a plain closure
    /// frame (`exception_body_outer_state`).
    ExceptionBodyOuterState,
    /// The scratch seed of a conditional in a plain closure frame
    /// (`seed_scratch_locals`, `ScratchStart::Fresh`).
    BranchScratchSeed,
    /// A loop plan's initial state where a lowering emits it as a source: a
    /// nested loop's exit-arm rebuild or repack seed, or a hybrid loop's
    /// pre-extracted fields.
    LoopPlanInitialState,
}

impl StateReadSite {
    /// The name the invariant probe and reports use.
    pub(in crate::core_erlang) const fn label(self) -> &'static str {
        match self {
            Self::FieldRead => "field-read",
            Self::ImplicitFieldRead => "implicit-field-read",
            Self::NlrThrowState => "nlr-throw-state",
            Self::ClosureCapture => "closure-capture",
            Self::ExceptionBodyOuterState => "exception-body-outer-state",
            Self::BranchScratchSeed => "branch-scratch-seed",
            Self::LoopPlanInitialState => "loop-plan-initial-state",
        }
    }
}

/// BT-4180: which stale source a [`VerifyError::StaleReadInBracketedLoop`]
/// read took.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum StaleReadDefect {
    /// A threaded `State` variable: the loop-entry `State`, the captured
    /// pre-loop state of a direct-params loop or tuple-accumulator fold
    /// (BT-3562, BT-4178), or any other state map name.
    ThreadedState,
    /// A hybrid loop's pre-extracted field parameter.
    PreExtractedParam,
}

/// What is wrong with the cell half of an [`ThreadedStmt::OnDoCatch`] node
/// ([`VerifyError::CatchWithoutStateRestore`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum StateRestoreDefect {
    /// The non-NLR clause does not have the cell restore right after the
    /// class-variable restore.
    NotAfterClassVarRestore,
    /// The cell restore reads a different variable than the node's entry binds.
    RestoresWrongVariable,
    /// BT-4179: the node sits inside a loop whose checkout brackets guarded
    /// operators but stores a threaded `State` ([`CellBoundary::PreState`])
    /// instead of snapshotting the cell. That `State` is stale for the writes
    /// the bracket's slow paths leave in the cell (the loop-entry version), so
    /// the entry store and the catch restore both drop them. Reported by the
    /// loop checkout (`checkout_guarded_op_loop`), whose decision comes from
    /// the loop's mode and operators, not from the gate that picked the
    /// boundary.
    PreStateOverLiveCell,
}

/// Checks `ir` against the invariants documented on each [`VerifyError`]
/// variant. Returns an empty `Vec` when `ir` is well-formed.
///
/// **Failure behavior** (per ADR 0111 §The verifier / AGENTS.md error-recovery
/// rules): this function never panics — callers in debug/CI treat a non-empty
/// result as a hard failure; release callers must degrade a non-empty result
/// to an internal-error diagnostic, never a panic or a refusal to compile.
/// (No call site does either yet — see module docs §Status.)
///
/// `Instance`-scoped, so it enforces no per-[`ScopeKind`] invariant; no
/// production code calls it (they use [`verify_in_scope`]), hence test-only.
#[cfg(test)]
pub(in crate::core_erlang) fn verify(ir: &[ThreadedStmt]) -> Vec<VerifyError> {
    verify_in_scope(ir, ScopeKind::Instance)
}

/// [`verify`] for IR lowered in `scope`: additionally enforces the
/// per-[`ScopeKind`] family invariant ([`VerifyError::ActorStateInClassMethod`]).
/// Every production caller reaches this with `CoreErlangGenerator::threaded_scope`
/// — via `CoreErlangGenerator::verify_threaded_ir`, or by passing
/// `threaded_scope()` to `verify_body_with_opaque_version_gaps` / `verify_simple_bind`.
pub(in crate::core_erlang) fn verify_in_scope(
    ir: &[ThreadedStmt],
    scope: ScopeKind,
) -> Vec<VerifyError> {
    verify_in_scope_after(ir, scope, None)
}

/// BT-4612: [`verify_in_scope`] for a slice that runs after its root frame
/// has already bound `State` version `bound_before` (a node the generator
/// builds and renders on its own, mid-frame: the `^`-closure pre-store). The
/// slice's top-level statements are checked against that version as if an
/// earlier statement of the list had bound it
/// ([`VerifyError::StalePrestore`], [`VerifyError::StaleConstructSeed`]).
pub(in crate::core_erlang) fn verify_in_scope_after(
    ir: &[ThreadedStmt],
    scope: ScopeKind,
    bound_before: Option<VersionedVar>,
) -> Vec<VerifyError> {
    let mut producers: HashMap<VersionedVar, usize> = HashMap::new();
    let mut consumers: HashMap<VersionedVar, usize> = HashMap::new();
    collect_producer_consumer_counts(ir, &mut producers, &mut consumers);

    let mut errors = Vec::new();
    // NonLinearVersion: iterate every var that was ever produced *or*
    // consumed, since a version consumed zero times but produced twice (or
    // vice-versa) is still a linearity violation. Version-0 vars are each
    // frame's implicit entry parameter — never produced by a `Bind` by
    // design (see `VersionedVar` docs) — so they are exempt from the
    // "exactly one producer" rule entirely; an unproduced, un-owned version-0
    // reference is checked separately by `UnboundVersion`.
    let mut all_vars: Vec<&VersionedVar> = producers.keys().filter(|v| v.version != 0).collect();
    for var in consumers.keys() {
        if var.version != 0 && !producers.contains_key(var) {
            all_vars.push(var);
        }
    }
    // Deterministic order: `producers`/`consumers` are `HashMap`s, whose
    // iteration order is per-process-random — sort so `verify`'s output
    // (and any future diagnostic built from it) is stable across runs.
    all_vars.sort();
    for var in all_vars {
        let produced = producers.get(var).copied().unwrap_or(0);
        let consumed = consumers.get(var).copied().unwrap_or(0);
        if produced != 1 || consumed > 1 {
            errors.push(VerifyError::NonLinearVersion {
                var: var.clone(),
                producers: produced,
                consumers: consumed,
            });
        }
    }

    let mut walk = VerifyWalk {
        producers: &producers,
        frame_stack: vec![FrameId::ROOT],
        mode_stack: Vec::new(),
        rebind_frames: Vec::new(),
        scope,
        bound_before,
        errors: &mut errors,
    };
    walk.walk(ir);

    errors
}

/// [`VerifyError::BindFrameMismatch`] for one `Bind` target, shared by the
/// full walk and [`verify_bind_frames`].
fn bind_frame_mismatch(target: &VersionedVar, enclosing: FrameId, at: Span) -> Option<VerifyError> {
    (target.frame != enclosing).then(|| VerifyError::BindFrameMismatch {
        var: target.clone(),
        enclosing,
        at,
    })
}

/// [`VerifyError::BindFrameMismatch`] over a prelude about to be spliced into
/// (or rendered for) a statement list at `enclosing`: every top-level `Bind`
/// must target `enclosing`. Nested frame nodes carry their own frame and are
/// checked by the full walk once they are part of a verified tree. Needs no
/// version history, so a fragment can be checked in isolation where the full
/// walk would report its caller-produced sources as unbound.
pub(in crate::core_erlang) fn verify_bind_frames(
    ir: &[ThreadedStmt],
    enclosing: FrameId,
) -> Vec<VerifyError> {
    ir.iter()
        .filter_map(|stmt| match stmt {
            ThreadedStmt::Bind { target, span, .. } => {
                bind_frame_mismatch(target, enclosing, *span)
            }
            _ => None,
        })
        .collect()
}

/// BT-3827, [`VerifyError::FoldFamilyNotCarried`] over one fold lambda body
/// (the statement list of a `Foldl*` `Threaded` node in `frame`): when the body
/// advances the `Self` family with a top-level `Bind` chain, its last statement
/// must be the [`ThreadedStmt::Return`] that carries the chain's final version
/// out through the accumulator. Needs no version history (like
/// [`verify_bind_frames`]), so it runs on the body fragment alone.
pub(in crate::core_erlang) fn verify_fold_family_carried(
    body: &[ThreadedStmt],
    frame: FrameId,
) -> Vec<VerifyError> {
    let last_self_bind = body.iter().rev().find_map(|stmt| match stmt {
        ThreadedStmt::Bind { target, span, .. }
            if target.prefix == VersionPrefix::SelfVt && target.frame == frame =>
        {
            Some((target, *span))
        }
        _ => None,
    });
    let Some((last, at)) = last_self_bind else {
        return Vec::new();
    };
    match body.last() {
        Some(ThreadedStmt::Return(_, state, _)) if state == last => Vec::new(),
        _ => vec![VerifyError::FoldFamilyNotCarried {
            family: VersionPrefix::SelfVt,
            at,
        }],
    }
}

/// BT-4116: [`VerifyError::TupleAccWithFamily`] — a tuple-accumulator fold
/// must thread no family.
pub(in crate::core_erlang) fn verify_tuple_acc_no_family(
    mode: &ThreadingMode,
    families_empty: bool,
    at: Span,
) -> Vec<VerifyError> {
    if matches!(mode, ThreadingMode::TupleAcc(..)) && !families_empty {
        vec![VerifyError::TupleAccWithFamily { at }]
    } else {
        Vec::new()
    }
}

/// BT-3863, [`VerifyError::FoldLocalsNotCarried`] over one fold lambda body
/// (the statement list of a `Foldl*` `Threaded` node with `mode`, threading
/// `threads`): when it threads any local, its last statement must be the
/// [`ThreadedStmt::FoldAccumulator`] whose carry has `mode`'s shape (see the
/// variant's scope note: the shape, not the versions read). Needs no version
/// history (like [`verify_fold_family_carried`]), so it runs
/// on the body fragment alone, before `carry_fold_families` wraps the
/// accumulator in the `Self` family's `Return`.
pub(in crate::core_erlang) fn verify_fold_locals_carried(
    body: &[ThreadedStmt],
    mode: &ThreadingMode,
    threads: &[String],
    at: Span,
) -> Vec<VerifyError> {
    if threads.is_empty() {
        return Vec::new();
    }
    let (found, at) = match body.last() {
        Some(ThreadedStmt::FoldAccumulator { carry, span, .. }) => (Some(carry), *span),
        _ => (None, at),
    };
    if let (ThreadingMode::TupleAcc(_, shape), Some(carry)) = (mode, found) {
        let carry_shape = match carry {
            FoldCarry::Locals(_) => Some(FoldAccShape::Tuple),
            FoldCarry::BareLocal(_) => Some(FoldAccShape::Bare),
            _ => None,
        };
        if carry_shape.is_some_and(|c| c != *shape) {
            return vec![VerifyError::FoldAccShapeMismatch {
                mode: *shape,
                found: carry.clone(),
                at,
            }];
        }
    }
    let carried = match (mode, found) {
        (ThreadingMode::TupleAcc(..), Some(FoldCarry::Locals(locals))) => locals == threads,
        (ThreadingMode::TupleAcc(..), Some(FoldCarry::BareLocal(local))) => {
            threads.len() == 1 && threads[0] == *local
        }
        (ThreadingMode::StateAcc(_), Some(FoldCarry::StateMap)) => true,
        _ => false,
    };
    if carried {
        return Vec::new();
    }
    vec![VerifyError::FoldLocalsNotCarried {
        threads: threads.to_vec(),
        found: found.cloned(),
        at,
    }]
}

/// BT-3939, [`VerifyError::ProducerSiteNotThreaded`] over one frame's
/// statement list: each `(site, locals)` of `sites` (a producer site of the
/// frame's body that writes `locals`) must have a
/// [`ThreadedStmt::ConstructTuple`] in `body` inside the site's span (a loop's is
/// at the site's own span, a conditional's or `on:do:`'s at its tail) whose
/// `threads` lists every one of `locals`. The sites are read off the AST by
/// the frame's owner before it lowers a statement; the nodes are read off the
/// lowered IR, so the two cannot agree by construction. Needs no version
/// history (like [`verify_fold_locals_carried`]).
///
/// A site whose `locals` is empty (a construct that writes no outer local but
/// threads a family, so it still compiles to a carrier tuple, BT-3949) needs a
/// `ConstructTuple` inside its span all the same; its absence is
/// [`VerifyError::ConstructCarrierInPlace`].
pub(in crate::core_erlang) fn verify_producer_sites_threaded(
    body: &[ThreadedStmt],
    sites: &[(Span, Vec<String>)],
) -> Vec<VerifyError> {
    unthreaded_sites(body, sites)
        .map(|(site, locals)| {
            if locals.is_empty() {
                VerifyError::ConstructCarrierInPlace { at: *site }
            } else {
                VerifyError::ProducerSiteNotThreaded {
                    locals: locals.clone(),
                    at: *site,
                }
            }
        })
        .collect()
}

/// The `(site, locals)` of `sites` with no [`ThreadedStmt::ConstructTuple`] in
/// `body` inside the site's span threading every one of `locals`: the one
/// matcher of [`verify_producer_sites_threaded`] and
/// [`verify_repl_producer_sites_threaded`].
fn unthreaded_sites<'s>(
    body: &'s [ThreadedStmt],
    sites: &'s [(Span, Vec<String>)],
) -> impl Iterator<Item = &'s (Span, Vec<String>)> {
    sites.iter().filter(|(site, locals)| {
        !body.iter().any(|stmt| match stmt {
            ThreadedStmt::ConstructTuple { threads, span, .. } => {
                span.start() >= site.start()
                    && span.end() <= site.end()
                    && locals.iter().all(|l| threads.contains(l))
            }
            _ => false,
        })
    })
}

/// BT-4461, [`VerifyError::ReplCachedBindingStale`] over one REPL frame's
/// statement list: each `(site, locals)` of `sites` (a producer site of the
/// frame whose construct writes `locals`, bindings the frame holds in Core
/// variables) must have a [`ThreadedStmt::ConstructTuple`] in `body` inside the
/// site's span whose `threads` lists every one of `locals`, and so (by
/// [`VerifyError::ThreadedLocalDropped`]) one [`ThreadedStmt::LocalRebind`]
/// each. The sites are read off the AST and the generator's scope before the
/// statement is lowered; the nodes off the lowered IR.
pub(in crate::core_erlang) fn verify_repl_producer_sites_threaded(
    body: &[ThreadedStmt],
    sites: &[(Span, Vec<String>)],
) -> Vec<VerifyError> {
    unthreaded_sites(body, sites)
        .map(|(site, locals)| VerifyError::ReplCachedBindingStale {
            locals: locals.clone(),
            at: *site,
        })
        .collect()
}

/// First pass: collects, per [`VersionedVar`], how many `Bind`s produce it
/// (`target`) and how many `Bind`s consume it as their `source`. `VersionedVar`
/// already encodes frame identity, so counts naturally stay frame-scoped
/// without extra bookkeeping.
fn collect_producer_consumer_counts(
    stmts: &[ThreadedStmt],
    producers: &mut HashMap<VersionedVar, usize>,
    consumers: &mut HashMap<VersionedVar, usize>,
) {
    for stmt in stmts {
        match stmt {
            ThreadedStmt::Bind { target, source, .. } => {
                *producers.entry(target.clone()).or_insert(0) += 1;
                *consumers.entry(source.clone()).or_insert(0) += 1;
            }
            // ADR 0131 §2: the `MethodBody`/`BranchArm` frame nodes scope
            // their body exactly like a `Threaded` node does.
            ThreadedStmt::Threaded { body, .. }
            | ThreadedStmt::MethodBody { body, .. }
            | ThreadedStmt::BranchArm { body, .. } => {
                collect_producer_consumer_counts(body, producers, consumers);
            }
            // ADR 0118 phase 3: `condition`'s own Binds are
            // real IR now too — collected in the SAME pass as `body`'s
            // (order doesn't matter here: both just accumulate into the
            // same producer/consumer maps).
            ThreadedStmt::ConditionalLoop {
                condition, body, ..
            } => {
                collect_producer_consumer_counts(condition, producers, consumers);
                collect_producer_consumer_counts(body, producers, consumers);
            }
            ThreadedStmt::TupleAccUnpack { targets, .. } => {
                // Each target is produced exactly once by this node, straight
                // from the unversioned `AccParam` — never consumed as another
                // Bind's `source` here (that would be a later ordinary
                // `Bind`), so only `producers` is touched.
                for target in targets {
                    *producers.entry(target.clone()).or_insert(0) += 1;
                }
            }
            // ADR 0131 §2: a `LoopParam`/`MapPut` rebind is one version step,
            // counted like a `Bind`'s.
            ThreadedStmt::LocalRebind { lowering, .. } => {
                if let Some((source, target)) = lowering.version_step() {
                    *producers.entry(target.clone()).or_insert(0) += 1;
                    *consumers.entry(source.clone()).or_insert(0) += 1;
                }
            }
            // ADR 0137: a bracket steps the `State` family from `store` to
            // `load` like a `Bind`; the store-only form consumes nothing.
            ThreadedStmt::SyncedCall { store, load, .. } => {
                if let Some(load) = load {
                    *producers.entry(load.clone()).or_insert(0) += 1;
                    *consumers.entry(store.clone()).or_insert(0) += 1;
                }
            }
            ThreadedStmt::NlrCatch { .. }
            | ThreadedStmt::NlrThrow { .. }
            | ThreadedStmt::Return(..)
            | ThreadedStmt::Statement(..)
            | ThreadedStmt::FoldAccumulator { .. }
            | ThreadedStmt::ConditionHandBack { .. }
            | ThreadedStmt::OnDoCatch { .. }
            | ThreadedStmt::EnsureCatch { .. }
            | ThreadedStmt::ConstructTuple { .. }
            | ThreadedStmt::ConstructLocalsInPlace { .. }
            | ThreadedStmt::DiscardLocals { .. } => {}
        }
    }
}

/// Whether a locals-less `StateAcc` loop/fold node carries a storage family
/// (BT-3999), so its seed is a fresh map rather than the ambient `State`.
///
/// A `Letrec` loop that threads a family passes it as a trailing loop
/// parameter: a `produces` entry other than the `StateAcc` map's own `State`
/// (`ThreadingPlan::capture_loop_family_params`, a `Gensym` seed). A fold
/// carries it through its accumulator instead (`carry_fold_families`): its
/// body binds the family's versions (a `SelfVt` `Bind`) or ends in the
/// `Return` that hands the family out (a `SelfVt` version, or the `Gensym`
/// seed when the body never rebinds it). A nested loop or fold is its own node,
/// checked on its own, so the walk stops there: an inner loop's family does
/// not vouch for an outer loop's seed.
fn carries_family(produces: &[VersionedVar], body: &[ThreadedStmt]) -> bool {
    fn binds_self_vt(stmts: &[ThreadedStmt]) -> bool {
        stmts.iter().any(|stmt| match stmt {
            ThreadedStmt::Bind { target, .. } => target.prefix == VersionPrefix::SelfVt,
            ThreadedStmt::Return(_, var, _) => {
                matches!(var.prefix, VersionPrefix::SelfVt | VersionPrefix::Gensym(_))
            }
            ThreadedStmt::MethodBody { body, .. } | ThreadedStmt::BranchArm { body, .. } => {
                binds_self_vt(body)
            }
            _ => false,
        })
    }
    produces
        .iter()
        .any(|v| !matches!(v.prefix, VersionPrefix::State))
        || binds_self_vt(body)
}

/// BT-4002, [`VerifyError::ConditionFamilyNotCarried`] over one
/// `ConditionalLoop` (in `frame`): every family the condition advanced
/// (`condition_families`) must be handed back into the loop's frame by a
/// top-level `Bind` in `condition` whose target is that family in `frame` and
/// whose source is one of the loop's own `produces` seeds (the family's loop
/// parameter, which the condition closure read). A `Local` family (BT-4015)
/// rides the condition's `StateAcc` instead, so it is carried by a top-level
/// [`ThreadedStmt::ConditionHandBack`]. Needs no version history.
fn condition_families_not_carried(
    condition_families: &[VersionPrefix],
    condition: &[ThreadedStmt],
    produces: &[VersionedVar],
    frame: FrameId,
    at: Span,
) -> Vec<VerifyError> {
    let hands_back_map = condition
        .iter()
        .any(|stmt| matches!(stmt, ThreadedStmt::ConditionHandBack { .. }));
    condition_families
        .iter()
        .filter(|family| {
            if matches!(family, VersionPrefix::Local(_)) {
                return !hands_back_map;
            }
            !condition.iter().any(|stmt| {
                matches!(
                    stmt,
                    ThreadedStmt::Bind { target, source, .. }
                        if &target.prefix == *family
                            && target.frame == frame
                            && produces.contains(source)
                )
            })
        })
        .map(|family| VerifyError::ConditionFamilyNotCarried {
            family: family.clone(),
            at,
        })
        .collect()
}

/// BT-4584: a statement of a carried loop condition that writes outer
/// locals: its span, and each local with its scratch-map key (the binding
/// name in the REPL, the `__local__` key in a method).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct ConditionWriteSite {
    pub(in crate::core_erlang) span: Span,
    pub(in crate::core_erlang) writes: Vec<(String, String)>,
}

/// BT-4584, [`VerifyError::ConditionWriteNotInMap`] over one carried loop
/// condition's lowered statements (`body`, the condition frame's own list):
/// each local of each `site` must be written into the frame's map by a
/// top-level `Bind` whose op is a `Put` of the local's key, inside the site's
/// span. The sites are read off the AST before the condition is lowered; the
/// `Put`s off the lowered IR.
pub(in crate::core_erlang) fn verify_condition_writes_in_map(
    body: &[ThreadedStmt],
    sites: &[ConditionWriteSite],
) -> Vec<VerifyError> {
    sites
        .iter()
        .flat_map(|site| {
            site.writes
                .iter()
                .filter(|(_, key)| {
                    !body.iter().any(|stmt| {
                        matches!(
                            stmt,
                            ThreadedStmt::Bind { op: BindOp::Put { field, .. }, span, .. }
                                if field == key
                                    && span.start() >= site.span.start()
                                    && span.end() <= site.span.end()
                        )
                    })
                })
                .map(|(local, _)| VerifyError::ConditionWriteNotInMap {
                    local: local.clone(),
                    at: site.span,
                })
        })
        .collect()
}

/// Second pass: walks `ir` tracking the active frame/mode nesting, checking
/// [`VerifyError::UnboundVersion`] and [`VerifyError::ThreadingModeUnpackMismatch`]
/// (`NonLinearVersion` is fully determined by the first pass's counts and
/// checked before this walk runs).
struct VerifyWalk<'a> {
    producers: &'a HashMap<VersionedVar, usize>,
    frame_stack: Vec<FrameId>,
    mode_stack: Vec<ThreadingMode>,
    /// ADR 0131 §2: the frame nodes enclosing the current statement
    /// ([`RebindFrame::of`] each `MethodBody`/`BranchArm`/`Threaded`/
    /// `ConditionalLoop`), innermost last. Empty means the implicit non-REPL
    /// method root ([`RebindFrame::implicit_root`]).
    rebind_frames: Vec<RebindFrame>,
    scope: ScopeKind,
    /// BT-4612: the root frame's `State` bound before the slice
    /// ([`verify_in_scope_after`]), taken by the first statement list
    /// checked (the slice's top level).
    bound_before: Option<VersionedVar>,
    errors: &'a mut Vec<VerifyError>,
}

impl VerifyWalk<'_> {
    fn walk(&mut self, stmts: &[ThreadedStmt]) {
        self.check_construct_rebinds(stmts);
        self.check_frame_state_reads(stmts);
        for stmt in stmts {
            self.walk_stmt(stmt);
        }
    }

    /// The frame node enclosing the statement being walked.
    fn enclosing_frame(&self) -> RebindFrame {
        self.rebind_frames
            .last()
            .cloned()
            .unwrap_or_else(RebindFrame::implicit_root)
    }

    /// Walks a frame node's children with `node` pushed as the enclosing
    /// [`RebindFrame`].
    fn in_frame(&mut self, node: &ThreadedStmt, f: impl FnOnce(&mut Self)) {
        let frame = RebindFrame::of(node);
        let pushed = frame.is_some();
        if let Some(frame) = frame {
            self.rebind_frames.push(frame);
        }
        f(self);
        if pushed {
            self.rebind_frames.pop();
        }
    }

    /// [`VerifyError::ThreadedLocalDropped`] over one frame's straight-line
    /// statement list (called by [`Self::walk`] for every list, so at every
    /// nesting level). A construct's prelude is `ConstructTuple`, its family
    /// `Bind`s, then one `LocalRebind` per threaded local
    /// (`build_local_threading_prelude`); anything else before every listed
    /// local is rebound closes the construct and drops the rest. A
    /// `DiscardLocals` of the open carrier drops them legitimately only at a
    /// `MethodBody` frame.
    fn check_construct_rebinds(&mut self, stmts: &[ThreadedStmt]) {
        /// The construct whose rebinds are still being read.
        struct Open<'s> {
            carrier: &'s str,
            threads: &'s [String],
            pending: Vec<&'s str>,
            /// BT-3821: declared families whose merge `Bind` has not been seen.
            pending_families: Vec<VersionPrefix>,
            at: Span,
        }
        fn close(open: &mut Option<Open<'_>>, dropped: &mut Vec<VerifyError>) {
            if let Some(o) = open.take() {
                dropped.extend(o.pending.into_iter().map(|local| {
                    VerifyError::ThreadedLocalDropped {
                        local: local.to_string(),
                        carrier: o.carrier.to_string(),
                        at: o.at,
                    }
                }));
                dropped.extend(o.pending_families.into_iter().map(|family| {
                    VerifyError::ConstructFamilyDropped {
                        family,
                        carrier: o.carrier.to_string(),
                        at: o.at,
                    }
                }));
            }
        }
        let discard_ok = self.enclosing_frame().kind == RebindFrameKind::MethodBody;
        let mut open: Option<Open<'_>> = None;
        let mut dropped: Vec<VerifyError> = Vec::new();
        for stmt in stmts {
            match stmt {
                ThreadedStmt::ConstructTuple {
                    carrier,
                    threads,
                    families,
                    span,
                    ..
                } => {
                    close(&mut open, &mut dropped);
                    open = Some(Open {
                        carrier,
                        threads,
                        pending: threads.iter().map(String::as_str).collect(),
                        pending_families: families.clone(),
                        at: *span,
                    });
                }
                ThreadedStmt::LocalRebind {
                    local,
                    carrier,
                    span,
                    ..
                } => match open.as_mut() {
                    Some(o) if o.carrier == carrier && o.threads.contains(local) => {
                        o.pending.retain(|p| p != local);
                    }
                    _ => dropped.push(VerifyError::ThreadedLocalDropped {
                        local: local.clone(),
                        carrier: carrier.clone(),
                        at: *span,
                    }),
                },
                // A family extraction reads the carrier ahead of the rebinds:
                // a `Direct` step of the `State`/`SelfVt` family
                // (`extract_family_slots`). Its value is an opaque `Doc`, so
                // which carrier it reads is not checked.
                ThreadedStmt::Bind {
                    target,
                    op: BindOp::Direct(_),
                    ..
                } if open.is_some()
                    && matches!(target.prefix, VersionPrefix::State | VersionPrefix::SelfVt) =>
                {
                    if let Some(o) = open.as_mut() {
                        o.pending_families.retain(|f| *f != target.prefix);
                    }
                }
                ThreadedStmt::DiscardLocals { carrier, .. }
                    if discard_ok && open.as_ref().is_some_and(|o| o.carrier == carrier) =>
                {
                    // The discard legitimately drops the threaded *locals*
                    // (the method ends), never a declared *family*: its merge
                    // `Bind` still has to have run, or the tail reads a stale
                    // `Self`/`State` (BT-3830).
                    if let Some(o) = open.as_mut() {
                        o.pending.clear();
                    }
                    close(&mut open, &mut dropped);
                }
                _ => close(&mut open, &mut dropped),
            }
        }
        close(&mut open, &mut dropped);
        self.errors.extend(dropped);
    }

    /// [`VerifyError::ArmLocalPutNotCarried`] over the straight-line statements
    /// of one arm: no `Put` of a `__local__` key whose local the arm's
    /// `threads` does not list.
    fn check_arm_local_puts(&mut self, threads: &[String], body: &[ThreadedStmt]) {
        for stmt in body {
            let ThreadedStmt::Bind {
                op: BindOp::Put { field, .. },
                span,
                ..
            } = stmt
            else {
                continue;
            };
            if let Some(local) = field.strip_prefix(LOCAL_STATE_KEY_PREFIX)
                && !threads.iter().any(|t| t == local)
            {
                self.errors.push(VerifyError::ArmLocalPutNotCarried {
                    local: local.to_string(),
                    at: *span,
                });
            }
        }
    }

    /// The reads of the enclosing frame's current `State` version over one
    /// statement list, against the last step of that frame's `State` map the
    /// list has made so far:
    ///
    /// - [`VerifyError::HeldRebindNotIdentity`]: each `MapHeld` rebind must
    ///   read its own `key` through `state_first == map`, `map` must be a
    ///   bound `State` version of the enclosing frame (its entry version 0,
    ///   or one some step produces), and — when this list steps that frame's
    ///   `State` map before the rebind — the last such step's target must be
    ///   `map`.
    /// - [`VerifyError::StaleConstructSeed`]: a `ConstructTuple`'s
    ///   `scratch_base` in this frame is no older than the last such step.
    /// - [`VerifyError::StalePrestore`] (BT-4612): a `SyncedCall`'s `reads`
    ///   in this frame is no older than the last such step, read before the
    ///   node's own reload (if any) steps the frame.
    ///
    /// A list that has not stepped the map yet inherits its current version
    /// from outside the list, which these checks reconstruct only for the
    /// top-level list of a slice verified with the frame's `State` bound
    /// before it ([`verify_in_scope_after`]).
    fn check_frame_state_reads(&mut self, stmts: &[ThreadedStmt]) {
        let frame = self.enclosing_frame().frame;
        let own_state = |v: &VersionedVar| v.prefix == VersionPrefix::State && v.frame == frame;
        // Only the first list walked (the slice's top level) runs after the
        // bound `State`; every nested list is a frame or region of its own.
        let bound_before = self.bound_before.take();
        let mut current: Option<&VersionedVar> = bound_before.as_ref();
        for stmt in stmts {
            if let ThreadedStmt::SyncedCall {
                reads: Some(reads),
                span,
                ..
            } = stmt
                && let Some(newest) = current
                && own_state(reads)
                && reads.version < newest.version
            {
                self.errors.push(VerifyError::StalePrestore {
                    reads: reads.clone(),
                    newest: newest.clone(),
                    at: *span,
                });
            }
            match stmt {
                ThreadedStmt::Bind { target, .. }
                | ThreadedStmt::SyncedCall {
                    load: Some(target), ..
                } if own_state(target) => current = Some(target),
                ThreadedStmt::TupleAccUnpack { targets, .. } => {
                    if let Some(target) = targets.iter().rev().find(|t| own_state(t)) {
                        current = Some(target);
                    }
                }
                ThreadedStmt::ConstructTuple {
                    carrier,
                    scratch_base: Some(seed),
                    span,
                    ..
                } => {
                    if let Some(newest) = current
                        && own_state(seed)
                        && seed.version < newest.version
                    {
                        self.errors.push(VerifyError::StaleConstructSeed {
                            seed: seed.clone(),
                            newest: newest.clone(),
                            carrier: carrier.clone(),
                            at: *span,
                        });
                    }
                }
                ThreadedStmt::LocalRebind {
                    local,
                    slot,
                    state_first,
                    lowering,
                    span,
                    ..
                } => match lowering {
                    RebindLowering::MapHeld { key, map } => {
                        let identity = state_first.as_ref() == Some(map)
                            && *slot == CarrierSlot::Key(key.clone())
                            && own_state(map)
                            && (map.version == 0
                                || self.producers.get(map).copied().unwrap_or(0) > 0)
                            && current.is_none_or(|c| c == map);
                        if !identity {
                            self.errors.push(VerifyError::HeldRebindNotIdentity {
                                local: local.clone(),
                                at: *span,
                            });
                        }
                    }
                    _ => {
                        if let Some((_, target)) = lowering.version_step()
                            && own_state(target)
                        {
                            current = Some(target);
                        }
                    }
                },
                _ => {}
            }
        }
    }

    /// [`VerifyError::LocalRebindFrameMismatch`] and
    /// [`VerifyError::LocalRebindModeMismatch`] for one rebind, against the
    /// node that encloses it.
    fn check_rebind_lowering(
        &mut self,
        local: &str,
        frame: FrameId,
        lowered_as: RebindShape,
        at: Span,
    ) {
        let enclosing = self.enclosing_frame();
        if frame != enclosing.frame {
            self.errors.push(VerifyError::LocalRebindFrameMismatch {
                local: local.to_string(),
                recorded: frame,
                enclosing: enclosing.frame,
                at,
            });
        }
        // In a REPL input the frame's map is the bindings map: `local` is a
        // member of it whatever the frame's recorded set (the one rule,
        // `RebindFrame::with_repl_binding`, the producer lowers by too).
        let enclosing = if self.scope == ScopeKind::Repl {
            enclosing.with_repl_binding(local)
        } else {
            enclosing
        };
        if enclosing.shape_for(local) != lowered_as {
            self.errors.push(VerifyError::LocalRebindModeMismatch {
                frame: enclosing.frame,
                member: enclosing.threads_local(local),
                mode: enclosing.kind,
                lowered_as,
                at,
            });
        }
    }

    /// A version-0 var is the frame's implicit entry parameter — always
    /// bound. A version>0 var must have a producing `Bind` in a frame
    /// currently on the stack (its own frame, or an ancestor's — the
    /// frame-flow rule).
    fn check_use(&mut self, var: &VersionedVar, at: Span) {
        if var.version == 0 {
            return;
        }
        let bound = self.frame_stack.contains(&var.frame)
            && self.producers.get(var).copied().unwrap_or(0) > 0;
        if !bound {
            self.errors.push(VerifyError::UnboundVersion {
                var: var.clone(),
                at,
            });
        }
    }

    /// [`VerifyError::ActorStateInClassMethod`]: a `State`/`SelfVt` versioned
    /// var in a [`ScopeKind::ClassMethod`] scope that denotes a *family*.
    ///
    /// The `State` prefix is overloaded: inside a threading frame (a loop,
    /// fold or branch/handler arm — any `Threaded`/`ConditionalLoop` node, so
    /// `mode_stack` is non-empty) it is the `StateAcc` map's version, which
    /// carries threaded locals in a class method and is legitimate there (the
    /// class-method corpus lowers `State1`, … in such frames). Only a `State`
    /// version produced at method level (empty `mode_stack`) is the actor
    /// `State` family. `SelfVt` is never a `StateAcc` map, so it is flagged
    /// everywhere. Version 0 is a frame's entry parameter, never produced.
    ///
    /// In a [`ScopeKind::ValueType`] scope only the method-level `State`
    /// family is flagged: `SelfVt` is the value type's own family.
    fn check_class_method_family(&mut self, var: &VersionedVar, at: Span) {
        if matches!(self.scope, ScopeKind::Instance | ScopeKind::Repl) || var.version == 0 {
            return;
        }
        let is_family = match var.prefix {
            VersionPrefix::State => self.mode_stack.is_empty(),
            VersionPrefix::SelfVt => self.scope == ScopeKind::ClassMethod,
            VersionPrefix::Local(_) | VersionPrefix::Gensym(_) => false,
        };
        if is_family {
            self.errors.push(VerifyError::ActorStateInClassMethod {
                defect: ClassMethodDefect::FamilyVersion(var.prefix.clone()),
                at,
            });
        }
    }

    /// [`VerifyError::ActorStateInClassMethod`]: a loop/fold node whose mode is
    /// a locals-less `StateAcc` in a scope with no ambient carrier to seed it.
    ///
    /// A `StateAcc` loop that threads no local is seeded from the scope's
    /// ambient map: the actor `State` ([`ScopeKind::Instance`]) or the REPL
    /// bindings ([`ScopeKind::Repl`]). A [`ScopeKind::ClassMethod`] has
    /// neither, and threads no family. A [`ScopeKind::ValueType`] method has
    /// neither either, so there such a loop is legitimate only when it carries
    /// the value type's own `SelfVt` family (the pack prefix then starts from a
    /// fresh `maps:new()`, `ThreadingPlan::generate_pack_prefix`); otherwise
    /// its seed is an unbound `State` (BT-3999: `[self done] whileTrue: [nil]`
    /// in a value-type or `TestCase` method). See [`carries_family`].
    fn check_class_method_mode(
        &mut self,
        mode: &ThreadingMode,
        produces: &[VersionedVar],
        body: &[ThreadedStmt],
        at: Span,
    ) {
        let locals_less = matches!(
            mode,
            ThreadingMode::StateAcc(StateAccFallbackReason::NoThreadedLocals)
        );
        let no_carrier = match self.scope {
            ScopeKind::ClassMethod => true,
            ScopeKind::ValueType => !carries_family(produces, body),
            ScopeKind::Instance | ScopeKind::Repl => false,
        };
        if locals_less && no_carrier {
            self.errors.push(VerifyError::ActorStateInClassMethod {
                defect: ClassMethodDefect::StateAccLoopWithoutLocals,
                at,
            });
        }
    }

    #[allow(clippy::too_many_lines)] // ADR 0118 phase 3 (BT-3419) split the Threaded/ConditionalLoop arm in two
    fn walk_stmt(&mut self, stmt: &ThreadedStmt) {
        match stmt {
            ThreadedStmt::Bind {
                target,
                source,
                op,
                span,
            } => {
                self.check_use(source, *span);
                self.check_class_method_family(target, *span);
                let enclosing = self.frame_stack.last().copied().unwrap_or(FrameId::ROOT);
                if let Some(error) = bind_frame_mismatch(target, enclosing, *span) {
                    self.errors.push(error);
                }
                match op {
                    BindOp::Put { value, .. } | BindOp::Direct(value) => {
                        if let ValueRef::Version(v) = value {
                            self.check_use(v, *span);
                        }
                    }
                    BindOp::Unpack { .. } => {
                        if let Some(mode) = self.mode_stack.last()
                            && !matches!(mode, ThreadingMode::StateAcc(_))
                        {
                            self.errors.push(VerifyError::ThreadingModeUnpackMismatch {
                                mode: mode.clone(),
                                at: *span,
                            });
                        }
                    }
                }
            }
            ThreadedStmt::Threaded {
                mode,
                frame,
                body,
                produces,
                span,
                ..
            } => {
                self.check_class_method_mode(mode, produces, body, *span);
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.in_frame(stmt, |this| this.walk(body));
                for v in produces {
                    self.check_use(v, Span::default());
                    self.check_class_method_family(v, *span);
                }
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            ThreadedStmt::ConditionalLoop {
                mode,
                frame,
                condition,
                condition_families,
                condition_value,
                body,
                produces,
                span,
                ..
            } => {
                self.check_class_method_mode(mode, produces, body, *span);
                self.errors.extend(condition_families_not_carried(
                    condition_families,
                    condition,
                    produces,
                    *frame,
                    *span,
                ));
                // ADR 0111 Addendum 2, Gap 1 / ADR 0118 phase 3:
                // `ConditionalLoop` verifies almost exactly like `Threaded`
                // — push frame/mode once, walk `condition` THEN `body` (both
                // in the SAME frame — the condition's own `Bind`s are now
                // real IR the loop's later references can see), check_use
                // `condition_value` and each `produces` entry, pop.
                // `continue_arm`/`exit_arm`/`fn_name`/`counter` are opaque
                // (or caller-supplied, non-threading) fields `verify()` does
                // not, and is not meant to, inspect — see the variant's doc
                // comment.
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.in_frame(stmt, |this| {
                    this.walk(condition);
                    if let ValueRef::Version(v) = condition_value {
                        this.check_use(v, Span::default());
                    }
                    this.walk(body);
                });
                for v in produces {
                    self.check_use(v, Span::default());
                    self.check_class_method_family(v, *span);
                }
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            // Opaque by design: a `NlrCatch` has no body field (`render`
            // treats the rest of the slice as its body, walked as siblings);
            // a `Statement` is
            // ordinary AST-directed codegen with no state-threading content
            // of its own (see the variant's doc comment).
            //
            // ADR 0131: a `ConstructTuple`/`DiscardLocals` is checked with
            // its siblings, by `check_construct_rebinds` (`ThreadedLocalDropped`).
            //
            // BT-3863: a `FoldAccumulator` is checked against its fold frame
            // by `verify_fold_locals_carried`, which runs on the fold body
            // before `carry_fold_families` can wrap it.
            //
            // BT-4015: a `ConditionHandBack` is checked against its loop's
            // `condition_families` by `condition_families_not_carried`.
            // BT-4503: the boundary of a value-type or class-method function
            // answers the bare value its callers take as the send's result.
            ThreadedStmt::NlrCatch { boundary, span, .. } => {
                if matches!(self.scope, ScopeKind::ValueType | ScopeKind::ClassMethod)
                    && boundary.arm_shape() != NlrArmShape::Value
                {
                    self.errors.push(VerifyError::MethodNlrAnswersPair {
                        boundary: boundary.clone(),
                        at: *span,
                    });
                }
                // ADR 0146: an actor method's boundary commits the cell.
                if boundary.arm_shape() == NlrArmShape::CarriedReply {
                    self.errors
                        .push(VerifyError::ActorNlrCommitBypassesCell { at: *span });
                }
            }
            ThreadedStmt::Statement(..)
            | ThreadedStmt::FoldAccumulator { .. }
            | ThreadedStmt::ConditionHandBack { .. }
            | ThreadedStmt::ConstructTuple { .. }
            | ThreadedStmt::DiscardLocals { .. } => {}
            // BT-4460 / BT-4462: a construct compiled in place whose locals
            // no producer rebinds.
            ThreadedStmt::ConstructLocalsInPlace { locals, span } => {
                if !locals.is_empty() {
                    self.errors.push(VerifyError::ConstructLocalsNotRebound {
                        locals: locals.clone(),
                        at: *span,
                    });
                }
            }
            // ADR 0137: the bracket reads `store` and binds `load` in the
            // frame it sits in, exactly as the `Bind` it steps like.
            ThreadedStmt::SyncedCall {
                store,
                result,
                load,
                span,
                on_private_scratch,
                tier2_local_rebinds,
                fast_path,
                ..
            } => {
                self.check_use(store, *span);
                if *on_private_scratch {
                    self.errors
                        .push(VerifyError::SyncedCallOnPrivateScratch { at: *span });
                }
                if fast_path.is_some()
                    && (result.is_none() || load.is_none() || !tier2_local_rebinds.is_empty())
                {
                    self.errors
                        .push(VerifyError::FastPathOnUnsplittableBracket { at: *span });
                }
                if let Some(load) = load {
                    self.check_class_method_family(load, *span);
                    let enclosing = self.frame_stack.last().copied().unwrap_or(FrameId::ROOT);
                    if let Some(error) = bind_frame_mismatch(load, enclosing, *span) {
                        self.errors.push(error);
                    }
                }
            }
            // ADR 0041 §State-Carrying NLR: a `^` throw reads its slot
            // versions and value like a `Return` reads its own.
            ThreadedStmt::NlrThrow {
                value,
                slot,
                cell_live,
                cell_store,
                span,
                ..
            } => {
                if slot.is_empty() {
                    self.errors
                        .push(VerifyError::NlrThrowWithoutSlot { at: *span });
                }
                if *cell_live && !*cell_store {
                    self.errors
                        .push(VerifyError::NlrThrowWithoutCellStore { at: *span });
                }
                for v in slot {
                    self.check_use(v, *span);
                }
                if let ValueRef::Version(v) = value {
                    self.check_use(v, *span);
                }
            }
            // ADR 0131 §2/§4: a rebind's version step is checked exactly like
            // the `Bind` it renders as, and its frame and shape against the
            // node that encloses it.
            ThreadedStmt::LocalRebind {
                local,
                frame,
                state_first,
                lowering,
                span,
                ..
            } => {
                if let Some(state) = state_first {
                    self.check_use(state, *span);
                }
                if let Some((source, target)) = lowering.version_step() {
                    self.check_use(source, *span);
                    self.check_class_method_family(target, *span);
                }
                self.check_rebind_lowering(local, *frame, lowering.shape(), *span);
            }
            // The method's root frame: method level, so no mode is pushed (a
            // `State` version here is the actor family, exactly as for the
            // top-level slice).
            ThreadedStmt::MethodBody { frame, body, .. } => {
                self.frame_stack.push(*frame);
                self.in_frame(stmt, |this| this.walk(body));
                self.frame_stack.pop();
            }
            // A branch arm threads through its seeded `StateAcc` — the same
            // `StateAcc(None)` mode `render_branch_arm`'s wrapper
            // records for an arm today.
            ThreadedStmt::BranchArm {
                frame,
                threads,
                body,
            } => {
                self.check_arm_local_puts(threads, body);
                self.frame_stack.push(*frame);
                self.mode_stack
                    .push(ThreadingMode::StateAcc(StateAccFallbackReason::None));
                self.in_frame(stmt, |this| this.walk(body));
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            ThreadedStmt::OnDoCatch {
                vars,
                entry,
                cell,
                clauses,
                span,
                ..
            } => {
                if let Some(defect) = check_catch_restore(vars, *entry, clauses) {
                    self.errors
                        .push(VerifyError::CatchWithoutClassVarRestore { defect, at: *span });
                }
                if let Some(defect) = check_cell_restore(cell.as_ref(), clauses) {
                    self.errors
                        .push(VerifyError::CatchWithoutStateRestore { defect, at: *span });
                }
                if !catch_filter_closed(clauses) {
                    self.errors
                        .push(VerifyError::CatchFilterNotClosed { at: *span });
                }
            }
            ThreadedStmt::EnsureCatch {
                token,
                carry,
                private_scratch,
                cleanup,
                relay,
                cell_live,
                foreign,
                span,
                ..
            } => {
                let carry = carry.filter(|carry| carry.writable_by_cleanup(*private_scratch));
                if let Some(defect) =
                    check_ensure_relay(token.is_some(), carry, *cleanup, relay.as_ref())
                {
                    self.errors
                        .push(VerifyError::EnsureCleanupLostOnNlr { defect, at: *span });
                }
                if let Some(defect) = check_ensure_foreign_store(
                    *cell_live,
                    *cleanup,
                    foreign.as_ref(),
                    token.is_some().then_some(relay.as_ref()).flatten(),
                ) {
                    self.errors
                        .push(VerifyError::EnsureForeignCleanupNotStored { defect, at: *span });
                }
            }
            ThreadedStmt::Return(value, state, span) => {
                self.check_use(state, *span);
                self.check_class_method_family(state, *span);
                if let ValueRef::Version(v) = value {
                    self.check_use(v, *span);
                }
            }
            ThreadedStmt::TupleAccUnpack {
                gate_slots,
                shape,
                targets,
                span,
                ..
            } => {
                // BT-4116: a bare accumulator IS its one local; a bare unpack
                // of several targets would bind each to the whole accumulator.
                if *shape == FoldAccShape::Bare && targets.len() != 1 {
                    self.errors.push(VerifyError::BareAccTargets {
                        targets: targets.len(),
                        at: *span,
                    });
                }
                // Mirrors the `BindOp::Unpack` check above (invariant class 1:
                // legal only inside `TupleAcc` mode), plus invariant class 4:
                // the node's own `gate_slots` must match the enclosing mode's.
                if let Some(mode) = self.mode_stack.last() {
                    match mode {
                        ThreadingMode::TupleAcc(mode_gate_slots, mode_shape) => {
                            if *mode_shape != *shape {
                                self.errors.push(VerifyError::UnpackShapeMismatch {
                                    mode: *mode_shape,
                                    node: *shape,
                                    at: *span,
                                });
                            }
                            if *mode_gate_slots != *gate_slots {
                                self.errors.push(VerifyError::EarlyExitGateSlotMismatch {
                                    mode_gate_slots: *mode_gate_slots,
                                    node_gate_slots: *gate_slots,
                                    at: *span,
                                });
                            }
                        }
                        _ => {
                            self.errors.push(VerifyError::TupleAccUnpackModeMismatch {
                                mode: mode.clone(),
                                at: *span,
                            });
                        }
                    }
                }
                // Targets are producers-only (see `collect_producer_consumer_counts`);
                // NonLinearVersion's first pass already validated their
                // producer counts, so no further per-target `check_use` is
                // needed here — a `TupleAccUnpack` target is never itself a
                // consumer of a prior version.
            }
        }
    }
}

/// BT-4312 obligation of one [`ThreadedStmt::EnsureCatch`]: when a `^` of
/// the scope can cross it (`has_token`) carrying state its cleanup can write,
/// the relay must re-throw the cleanup's result. An inlined cleanup writes
/// the carried family when it advances it (its result is that version); a
/// closure writes only through the actor-state cell, so it can write an actor
/// method's `State` but never the REPL's bindings or a value type's `Self`;
/// where the state lives in the cell, any cleanup that may write the cell
/// writes it. A result in the cell is the carried state with the cell's
/// fields over it, never one rebuilt from the `ensure:` site's bindings.
/// A foreign throw is not the scope's to relay: an inlined cleanup's writes
/// reach the cell through the foreign clause
/// ([`check_ensure_foreign_store`]) and a closure's are made in the cell, which
/// the throw's own method commits (ADR 0146).
fn check_ensure_relay(
    has_token: bool,
    carry: Option<NlrCarry>,
    cleanup: EnsureCleanup,
    relay: Option<&EnsureNlrRelay>,
) -> Option<EnsureRelayDefect> {
    let carry = carry.filter(|_| has_token)?;
    let expected = match (carry, cleanup) {
        (
            NlrCarry::Live,
            EnsureCleanup::Inlined {
                writes_cell: true, ..
            }
            | EnsureCleanup::Closure { writes_cell: true },
        )
        | (NlrCarry::ActorState, EnsureCleanup::Closure { writes_cell: true }) => {
            RethrowSlot::CellOverCarried
        }
        (
            NlrCarry::Bindings | NlrCarry::ActorState | NlrCarry::SelfVt,
            EnsureCleanup::Inlined {
                final_version: Some(version),
                ..
            },
        ) => RethrowSlot::Version(version),
        _ => return None,
    };
    let Some(relay) = relay else {
        return Some(EnsureRelayDefect::NoRelay);
    };
    match (&relay.rethrow, &expected) {
        (found, expected) if found == expected => None,
        #[cfg(any(test, feature = "test"))]
        (RethrowSlot::SiteState(_), RethrowSlot::CellOverCarried) => {
            Some(EnsureRelayDefect::RebuildsCarriedStateAtSite)
        }
        _ => Some(EnsureRelayDefect::RethrowsStaleState),
    }
}

/// ADR 0146 R1 obligation of one [`ThreadedStmt::EnsureCatch`]: where the
/// actor-state cell is live and the cleanup is inlined and advances
/// `StateAcc` (`final_version`), the clause a foreign `$bt_nlr` takes stores
/// that final version to the cell. A cleanup applied as a closure writes the
/// cell in place and owes nothing; one that threads no write has nothing to
/// store. BT-4598: a clause that shares the relay's cleanup
/// ([`ForeignCleanupBody::SharedWithRelay`]) needs a relay of the scope's own
/// throw (`own_relay`, `None` where the scope has no token) that runs it from
/// the cell, whatever the cleanup: without one it has nothing to run.
fn check_ensure_foreign_store(
    cell_live: bool,
    cleanup: EnsureCleanup,
    foreign: Option<&EnsureForeignNlr>,
    own_relay: Option<&EnsureNlrRelay>,
) -> Option<ForeignStoreDefect> {
    if foreign.is_some_and(|foreign| foreign.body == ForeignCleanupBody::SharedWithRelay)
        && !own_relay.is_some_and(|relay| {
            relay.cell_store && matches!(relay.rethrow, RethrowSlot::Version(_))
        })
    {
        return Some(ForeignStoreDefect::SharedWithoutCellRelay);
    }
    let EnsureCleanup::Inlined {
        final_version: Some(expected),
        ..
    } = cleanup
    else {
        return None;
    };
    if !cell_live {
        return None;
    }
    match foreign.map(|foreign| foreign.store) {
        None => Some(ForeignStoreDefect::NoClause),
        Some(None) => Some(ForeignStoreDefect::NoStore),
        Some(Some(stored)) if stored == expected => None,
        Some(Some(stored)) => Some(ForeignStoreDefect::StoresStaleVersion { stored, expected }),
    }
}

/// ADR 0130 §4 catch-boundary obligation of one [`ThreadedStmt::OnDoCatch`]:
/// the first defect found, or `None` when the entry binds the snapshot, the
/// non-NLR clause begins with the restore of that same snapshot and both NLR
/// pass-through shapes are ordered before it.
fn check_catch_restore(
    vars: &OnDoCatchVars,
    entry: Option<CatchEntry>,
    clauses: &[CatchClause],
) -> Option<CatchRestoreDefect> {
    match entry {
        Some(CatchEntry::ClassVarSnapshot) => {}
        None => return Some(CatchRestoreDefect::NoSnapshotEntry),
    }
    let Some(non_nlr_at) = clauses
        .iter()
        .position(|c| matches!(c, CatchClause::NonNlr { .. }))
    else {
        return Some(CatchRestoreDefect::NoNonNlrClause);
    };
    for shape in [NlrThrowShape::Tuple3, NlrThrowShape::Tuple4] {
        let before = clauses[..non_nlr_at]
            .iter()
            .any(|c| matches!(c, CatchClause::NlrPassThrough(s) if *s == shape));
        if !before {
            return Some(CatchRestoreDefect::NlrArmNotBeforeRestore(shape));
        }
    }
    let CatchClause::NonNlr { steps } = &clauses[non_nlr_at] else {
        return Some(CatchRestoreDefect::NoNonNlrClause);
    };
    match steps.first() {
        Some(CatchStep::ClassVarRestore { snapshot }) if *snapshot == vars.snapshot_var => None,
        Some(CatchStep::ClassVarRestore { .. }) => {
            Some(CatchRestoreDefect::RestoreReadsWrongSnapshot)
        }
        _ => Some(CatchRestoreDefect::RestoreNotFirst),
    }
}

/// ADR 0137 §5 obligation of one [`ThreadedStmt::OnDoCatch`] that carries a
/// [`CellBoundary`]: its non-NLR clause has the matching cell restore as its
/// second step, right after the class-variable restore.
fn check_cell_restore(
    cell: Option<&CellBoundary>,
    clauses: &[CatchClause],
) -> Option<StateRestoreDefect> {
    let boundary = cell?;
    let steps = clauses.iter().find_map(|c| match c {
        CatchClause::NonNlr { steps } => Some(steps),
        CatchClause::NlrPassThrough(_) => None,
    })?;
    match steps.get(1) {
        Some(CatchStep::CellRestore(found)) if found == boundary => None,
        Some(CatchStep::CellRestore(_)) => Some(StateRestoreDefect::RestoresWrongVariable),
        _ => Some(StateRestoreDefect::NotAfterClassVarRestore),
    }
}

/// Whether the non-NLR clause of a [`ThreadedStmt::OnDoCatch`] closes the class
/// filter it opens: `ClassFilter`, then `FilterHandler`, then `FilterMiss`, the
/// last three steps in that order. A node with no non-NLR clause has no filter
/// to close (the restore check reports that).
fn catch_filter_closed(clauses: &[CatchClause]) -> bool {
    clauses.iter().all(|clause| match clause {
        CatchClause::NlrPassThrough(_) => true,
        CatchClause::NonNlr { steps } => matches!(
            steps.as_slice(),
            [
                ..,
                CatchStep::ClassFilter,
                CatchStep::FilterHandler(_),
                CatchStep::FilterMiss
            ]
        ),
    })
}

/// One child of a sequenced send, as `sequence_children` splices it (ADR
/// 0131 §1a): the input to [`verify_sibling_reads`].
#[derive(Debug, Clone, Copy)]
pub(in crate::core_erlang) struct SequencedSibling<'a> {
    /// The outer local this child reads *in place*: a plain-variable child
    /// the sequencing rule left for its parent to compile, which therefore
    /// reads the local after every sibling's prelude has run. `None` for a
    /// child compiled ahead (its value, or its `Tmp` snapshot, is fixed
    /// before any later sibling's prelude).
    pub(in crate::core_erlang) reads_in_place: Option<&'a str>,
    /// The prelude this child contributed, in splice order.
    pub(in crate::core_erlang) prelude: &'a [ThreadedStmt],
    pub(in crate::core_erlang) span: Span,
}

/// [`VerifyError::LocalReadAfterSiblingRebind`] (ADR 0131 §1a): reports every
/// sibling that reads local `x` in place while a later sibling's prelude
/// holds a top-level [`ThreadedStmt::LocalRebind`] for `x`. A rebind nested
/// inside a frame node of that prelude belongs to the inner frame and does
/// not change the binding the parent reads. Reached in production through
/// `sequence_children`, which reports the result via
/// [`CoreErlangGenerator::report_threaded_ir_verify_errors`].
///
/// ADR 0131 Phase 2 (BT-3749)'s `local_threading_producer` is the first
/// production source of a sibling `LocalRebind`; besides the hand-built IR
/// of `threaded_ir/tests/local_rebind.rs`, `tests/local_rebind_verifier_corpus.rs`
/// shows this firing on a real compile once `sequence_children`'s snapshot
/// is disabled.
pub(in crate::core_erlang) fn verify_sibling_reads(
    siblings: &[SequencedSibling<'_>],
) -> Vec<VerifyError> {
    let mut errors = Vec::new();
    for (i, sibling) in siblings.iter().enumerate() {
        let Some(local) = sibling.reads_in_place else {
            continue;
        };
        let rebound_later = siblings[i + 1..].iter().any(|later| {
            later.prelude.iter().any(
                |stmt| matches!(stmt, ThreadedStmt::LocalRebind { local: l, .. } if l == local),
            )
        });
        if rebound_later {
            errors.push(VerifyError::LocalReadAfterSiblingRebind {
                local: local.to_string(),
                at: sibling.span,
            });
        }
    }
    errors
}

/// [`VerifyError::RebindNotVisible`] (BT-4263): for each local the prelude a
/// producer hands its enclosing frame rebinds at top level, the last such
/// [`ThreadedStmt::LocalRebind`]'s `value_var` must be what `lookup` (the
/// generator's variable scope at the hand-off) answers for the local, so the
/// frame's later code reads the rebound value. A rebind nested inside a frame
/// node belongs to the inner frame, as in [`verify_sibling_reads`]. Reached in
/// production through [`CoreErlangGenerator::report_rebinds_visible`].
pub(in crate::core_erlang) fn verify_rebinds_visible<'a>(
    prelude: &[ThreadedStmt],
    lookup: impl Fn(&str) -> Option<&'a String>,
) -> Vec<VerifyError> {
    let mut last: Vec<(&str, &str, Span)> = Vec::new();
    for stmt in prelude {
        if let ThreadedStmt::LocalRebind {
            local,
            value_var,
            span,
            ..
        } = stmt
        {
            last.retain(|(l, _, _)| *l != local.as_str());
            last.push((local.as_str(), value_var.as_str(), *span));
        }
    }
    last.into_iter()
        .filter_map(|(local, value_var, at)| {
            let bound = lookup(local);
            (bound.map(String::as_str) != Some(value_var)).then(|| VerifyError::RebindNotVisible {
                local: local.to_string(),
                value_var: value_var.to_string(),
                bound: bound.cloned(),
                at,
            })
        })
        .collect()
}

impl CoreErlangGenerator {
    /// BT-4263: reports [`VerifyError::RebindNotVisible`] for the prelude a
    /// producer returns to the frame being lowered ([`verify_rebinds_visible`]
    /// against the current variable scope). Called by every producer funnel
    /// once the construct is lowered and any scope of its own is popped.
    pub(in crate::core_erlang) fn report_rebinds_visible(
        &mut self,
        prelude: &[ThreadedStmt],
        span: Span,
    ) {
        let errors = verify_rebinds_visible(prelude, |name| self.lookup_var(name));
        self.report_threaded_ir_verify_errors(
            &errors,
            "producer rebind not visible to its enclosing frame",
            span,
        );
    }

    /// The [`ScopeKind`] the generator is currently lowering — the one place
    /// that maps generator state to the verifier's scope (BT-3725).
    pub(in crate::core_erlang) fn threaded_scope(&self) -> ScopeKind {
        if self.threads_no_actor_state() {
            ScopeKind::ClassMethod
        } else if self.context == CodeGenContext::ValueType {
            ScopeKind::ValueType
        } else if self.is_repl_mode() {
            ScopeKind::Repl
        } else {
            ScopeKind::Instance
        }
    }

    /// [`verify_in_scope`] with the current [`Self::threaded_scope`] — what
    /// every production lowering site calls instead of bare [`verify`].
    pub(in crate::core_erlang) fn verify_threaded_ir(
        &self,
        ir: &[ThreadedStmt],
    ) -> Vec<VerifyError> {
        verify_in_scope(ir, self.threaded_scope())
    }

    /// BT-4612: [`Self::verify_threaded_ir`] for a slice rendered mid-frame,
    /// after the frame bound `State` version `bound_before`
    /// ([`verify_in_scope_after`]).
    pub(in crate::core_erlang) fn verify_threaded_ir_after(
        &self,
        ir: &[ThreadedStmt],
        bound_before: VersionedVar,
    ) -> Vec<VerifyError> {
        verify_in_scope_after(ir, self.threaded_scope(), Some(bound_before))
    }

    /// Shared failure-reporting path for every `ThreadedIr` production
    /// invariant check (ADR 0111 §The verifier / AGENTS.md's "never panic
    /// on user input" rule): hard-fails in debug/CI via `debug_assert!`,
    /// exactly as the deleted `debug_assert!`s this migration's checks
    /// replace did; in release builds (where `debug_assert!` is compiled
    /// out), degrades to an internal-error diagnostic on the compile result
    /// instead of silently doing nothing — the compile still succeeds with
    /// the generator's (unverified) output. Shared by every state-threading
    /// invariant check in `control_flow`, `expressions.rs`,
    /// `dispatch_codegen.rs`, and `gen_server/methods.rs` — each consolidated
    /// onto this one helper instead of an independently-added copy
    /// (AGENTS.md's no-duplicate-implementations rule).
    ///
    /// Moved here (out of `control_flow/mod.rs`) together with
    /// [`StateAccFallbackReason`] to remove a `threaded_ir → control_flow`
    /// import cycle — this crate's `control_flow` module previously defined
    /// both and `threaded_ir.rs` imported `StateAccFallbackReason` from it;
    /// now `threaded_ir.rs` defines both and `control_flow` imports from
    /// here instead.
    pub(in crate::core_erlang) fn report_threaded_ir_verify_errors(
        &mut self,
        errors: &[VerifyError],
        invariant_label: &str,
        span: Span,
    ) {
        if errors.is_empty() {
            return;
        }
        // The invariant probe (test-only count mode) tallies findings instead of aborting.
        if self.note_counted_verify_errors() {
            return;
        }
        debug_assert!(
            false,
            "ThreadedIr verify found a {invariant_label}: {errors:?}"
        );
        self.add_codegen_warning(verify_errors_to_diagnostic(errors, invariant_label, span));
    }

    /// Records one synthetic verifier finding through the release-mode path
    /// (the warning, without [`Self::report_threaded_ir_verify_errors`]'s
    /// debug-build hard failure). Backs
    /// [`CodegenOptions::with_injected_verifier_violation`], the test hook
    /// that lets driver-level tests prove an `internal:` diagnostic surfaces
    /// without a release build or a real codegen bug.
    ///
    /// `pub` (hidden) so `beamtalk-repl`'s REPL assembler, which drives its
    /// own generator rather than `generate_module_with_warnings`, can honor
    /// the same hook (BT-3778).
    #[doc(hidden)]
    pub fn inject_synthetic_verifier_violation(&mut self, enabled: bool, span: Span) {
        if !enabled {
            return;
        }
        let errors = [VerifyError::ThreadingModeUnpackMismatch {
            mode: ThreadingMode::DirectParams,
            at: span,
        }];
        self.add_codegen_warning(verify_errors_to_diagnostic(
            &errors,
            "injected verifier violation (test hook)",
            span,
        ));
    }
}

/// Builds the release-mode diagnostic for a `ThreadedIr` verifier finding
/// (ADR 0111 amendment, BT-3724): a **warning** (never an error, so it can
/// not fail a build that would otherwise produce usable code) whose message
/// starts with `internal:` and whose category is
/// [`DiagnosticCategory::InternalVerifier`], so drivers can forward exactly
/// these and nothing else out of `GeneratedModule::warnings`.
///
/// Independent of `cfg(debug_assertions)` so a unit test can assert it
/// without a release build; [`CoreErlangGenerator::report_threaded_ir_verify_errors`]
/// is its only production caller.
#[must_use]
pub(in crate::core_erlang) fn verify_errors_to_diagnostic(
    errors: &[VerifyError],
    invariant_label: &str,
    span: Span,
) -> Diagnostic {
    Diagnostic::warning(format!("internal: {invariant_label}: {errors:?}"), span)
        .with_category(DiagnosticCategory::InternalVerifier)
}
