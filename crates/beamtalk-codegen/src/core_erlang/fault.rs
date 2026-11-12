// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test-only fault injection for the code generator (BT-4688).
//!
//! A *fault* switches one lowering back to a shape it had before a fix (or to
//! a deliberately wrong one), so a verifier check or a corpus test can be
//! shown to fire on a real compile rather than only on hand-built IR. Every
//! fault is named by a [`Fault`] variant and consulted at its production call
//! site with one macro, `fault!(Variant)`.
//!
//! # Release vs test builds
//!
//! The registry exists only under `#[cfg(any(test, feature = "test"))]`:
//!
//! * this crate's own unit tests (`cfg(test)`);
//! * dependent crates' tests, which enable the `test` cargo feature in their
//!   `[dev-dependencies]` (`cfg(test)` does not cross crate boundaries), e.g.
//!   `beamtalk-repl`'s `repl_loop_body_return` test.
//!
//! In any other build `fault!(Variant)` expands to the constant `false`, so
//! the call site is dead code the compiler removes and no registry, enum,
//! thread-local or setter is compiled at all.
//!
//! Test-only *observation* probes (producer recorders, the synced-call site
//! log, ...) are gated the same way, but are not faults and live beside the
//! code they observe.

/// `true` while `Fault::$variant` is switched on for the current thread;
/// the constant `false` in a build without the test configuration.
#[cfg(any(test, feature = "test"))]
macro_rules! fault {
    ($variant:ident) => {
        $crate::core_erlang::fault::is_active($crate::core_erlang::fault::Fault::$variant)
    };
}

/// `true` while `Fault::$variant` is switched on for the current thread;
/// the constant `false` in a build without the test configuration.
#[cfg(not(any(test, feature = "test")))]
macro_rules! fault {
    ($variant:ident) => {
        false
    };
}

#[cfg(any(test, feature = "test"))]
pub use registry::{Fault, is_active, set, with};
#[cfg(any(test, feature = "test"))]
#[cfg_attr(not(test), allow(unused_imports))]
pub(super) use registry::{PlainArmScope, in_plain_arm, record_producer, recording_producers};

#[cfg(any(test, feature = "test"))]
mod registry {
    use beamtalk_core::source_analysis::Span;
    use std::cell::{Cell, RefCell};
    use std::collections::HashSet;

    /// One producer lowering: the construct's span and its threaded locals.
    pub(in crate::core_erlang) type ProducerRecord = (Span, Vec<String>);

    /// A deliberately wrong (or pre-fix) lowering. Switch one on with
    /// [`with`] or [`set`]; production code reads it with `fault!(Variant)`.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Fault {
        // ── Non-local return (`nlr.rs`) ─────────────────────────────────
        /// An actor `^` as it was before ADR 0146: the catch arm commits the
        /// state the throw carries, `{'reply', Value, State}`, instead of
        /// the actor's cell (`VerifyError::ActorNlrCommitBypassesCell`).
        NlrCommitCarried,
        /// A `^` a conditional arm lowers to a
        /// [`ThreadedStmt::NlrThrow`](crate::core_erlang::threaded_ir::ThreadedStmt::NlrThrow)
        /// throws without storing its state to the cell, as BT-3374's and
        /// BT-3495's arm throws did before BT-4535
        /// (`VerifyError::NlrThrowWithoutCellStore`).
        NlrThrowWithoutCellStore,

        // ── `ensure:` relay (`ensure_nlr_relay.rs`) ─────────────────────
        /// No relay: the catch re-raises the scope's own throw after the
        /// cleanup, as every `ensure:` did before BT-4312
        /// (`VerifyError::EnsureCleanupLostOnNlr`).
        RelayNoRelay,
        /// A cell-carried throw resumes with the state a `^` at the
        /// `ensure:` site would carry, rebuilt from the site's bindings, as
        /// the first BT-4312 relay did. Builds the test-only
        /// [`RethrowSlot::SiteState`](crate::core_erlang::threaded_ir::RethrowSlot::SiteState).
        RelayRebuildAtSite,
        /// ADR 0146 R1: no foreign clause. A `^` of another method of the
        /// same actor takes the error path, which runs the cleanup with
        /// write-through off and re-raises, so the cleanup's writes never
        /// reach the cell (`VerifyError::EnsureForeignCleanupNotStored`).
        RelayForeignOnErrorPath,

        // ── REPL eval boundary (`repl_nlr.rs`) ──────────────────────────
        /// REPL inputs compile without their eval boundary, as before
        /// BT-4288 (`VerifyError::ReplReturnWithoutBoundary`).
        ReplBoundaryOff,

        // ── Synced-call brackets and loop modes (`synced_call.rs`) ──────
        /// No call is bracketed (every user-code call is a gap), so the
        /// invariant probe can be shown to see a missing bracket.
        NoBrackets,
        /// BT-4122: Tier 2 threads the legacy way (fields through
        /// `StateAcc`, callers rebinding `State` from its return), so the
        /// `FieldReadFromStateAcc` rule can be shown to fire on it.
        LegacyTier2,
        /// BT-4179: inside a loop that brackets guarded operators, an
        /// `on:do:` stores the threaded `State` (`Snapshot` only in an actor
        /// closure) and a nested `StateAcc` producer seeds from it, as before
        /// the fix, so the loop checkout can be shown to report both.
        StaleStateInBracketedLoop,
        /// BT-4369: mode selection ignores a body's field writes, so a loop
        /// with a field write and a guarded operator is lowered stateless
        /// (direct params or tuple accumulator) and brackets, as the §4b
        /// prose says never happens (`FieldWriteInBracketedLoop`).
        StatelessFieldWrites,
        /// BT-4180: the read choke point
        /// (`CoreErlangGenerator::actor_state_read_source`) ignores the
        /// take-reads decision while the loop still brackets, so every read
        /// site emits the threaded `State` (or a pre-extracted parameter) and
        /// the checkout can be shown to report `StaleReadInBracketedLoop`.
        EntryStateReads,
        /// BT-4524: a `^` in a block run as a closure does not make its call
        /// store first (no bracket, no fallback pre-store), as before the
        /// fix (`UnbracketedNlrClosure`).
        NlrClosuresUnstored,
        /// BT-4523: mode selection ignores a body's `^`, so a body with a
        /// field write and a `^` is lowered hybrid, as before the fix
        /// (`NlrThrowInHybridLoop`).
        HybridAdmitsNlr,
        /// BT-4612: the `^`-closure pre-store of a conditional's non-Boolean
        /// fallback stores the state read before the conditional's receiver
        /// was sequenced (`(self flip: p) ifTrue: [^8]`), as BT-4532's first
        /// cut did for the runtime dispatch's operands (`StalePrestore`).
        PrestoreReadsBeforeOperands,

        // ── Local-rebind producers (ADR 0131; `threaded_expr.rs` & co) ──
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
        /// BT-4499: a REPL producer does not seed a block parameter it writes
        /// into the bindings map, so the frame keeps reading the parameter's
        /// variable (`ReplCachedBindingStale`).
        ReplParamsNotSeeded,
        /// BT-3910: the producer gate for a value-position local assignment
        /// answers `false`, so the assignment reaches the value-only lowering
        /// (`UnthreadedLocalAssignment`).
        NotAProducer,
        /// BT-3939: a Tier 2 block body registers no producer sites, so a loop
        /// or fold statement of it is lowered inside an opaque `let _ = ( ... )`
        /// instead of as a producer (`ProducerSiteNotThreaded`): the shape that
        /// answered the pre-loop value of a block-local the loop rebinds.
        Tier2OpaqueProducers,
        /// BT-3949: the arm of a construct that is not a local-threading
        /// producer (`generate_plain_arm`: a conditional, `match:` or
        /// `detect:ifNone:` arm) registers no producer sites, as before
        /// BT-3949, so an actor fold that self-sends at an operand site of an
        /// arm statement (a map-literal value) is compiled in place and its
        /// carrier tuple becomes the operand (`ConstructCarrierInPlace`).
        PlainArmOpaqueProducers,
        /// BT-4460: a plain arm's producer sites (registered and checked) are
        /// only its loops and folds, the plain-arm rule before BT-4460,
        /// so a nested conditional, `ifNil:`, `and:`/`or:`, `match:` or
        /// `on:do:`/`ensure:` that writes a local of a plain arm is compiled in
        /// place and no frame rule rebinds its locals
        /// (`ConstructLocalsNotRebound`).
        ArmLoopOrFoldSitesOnly,
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
        /// BT-4584: a destructuring statement of a carried
        /// `whileTrue:`/`whileFalse:` condition binds its names with plain
        /// `let`s, as before BT-4584, so the map the condition hands back
        /// lacks the write (`ConditionWriteNotInMap`).
        ConditionDestructurePlain,
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
        /// BT-4089: a tuple-acc fold's post-fold repack seeds from the
        /// ambient actor `State`, as before BT-4089, whatever the scope
        /// (`ActorStateInClassMethod` in a scope whose frame has none).
        TupleAccRepackFromAmbientState,
        /// BT-4206: a `match:` producer reads its base `State` and seeds its
        /// scratch map before compiling its scrutinee, as before BT-4196, so
        /// the arms thread from the `State` the scrutinee's spliced prelude
        /// has already stepped past (`StaleConstructSeed`).
        SeedBeforeScrutinee,
        /// BT-4178: a tuple-accumulator fold does not capture its entry
        /// state for its body, as before BT-4178, so a field read there
        /// names the loop context's `StateAcc`
        /// (`TupleAccStateReadNotEntryState`).
        TupleAccNoEntryCapture,
        /// BT-4116: no fold selects a tuple/bare accumulator — every fold
        /// threads its locals through the `StateAcc` map, as before BT-4116.
        /// The agreement corpus's oracle build (`fold_acc_shape_agreement`).
        ForceStateAccFold,
        /// BT-4116: a gate-slot-free single-local `do:` fold repacks its
        /// accumulator as a one-element tuple while its mode unpacks a bare
        /// value (`FoldAccShapeMismatch`).
        BareFoldTupleRepack,
        /// BT-3982: an actor evaluated literal block that threads `State` is
        /// no producer (`actor_evaluated_block_producer_parts` answers
        /// `None`), as before BT-3982: in value position it reaches the
        /// legacy inline lowering, whose raw `{Value, State}` tuple becomes
        /// the value (`UnthreadedEvaluatedBlock`).
        EvaluatedBlockNotAProducer,
        /// BT-4287: a frame registers none of its statements' value-bearing
        /// sites that hold an actor loop or fold threading only `State`
        /// (`is_family_only_fold_value_site`), as before BT-4287, so such a
        /// fold as an operand in a fold body is compiled in place and its
        /// `{Value, State}` carrier becomes the operand
        /// (`ConstructCarrierInPlace`).
        FoldOperandFamilyOnlyInPlace,
        /// BT-4310: a frame (the method root included) registers none of
        /// its statements' operands that the sequencing rule leaves to
        /// another lowering (a conditional's receiver, a lookup key, a cascade
        /// message's argument, an argument of a `super`, `error:`,
        /// block-invocation, cast or block-taking send) holding an actor loop
        /// or fold that threads only `State`
        /// ([`super::CoreErlangGenerator::family_only_operand_site_exprs`]),
        /// as before BT-4310, so such a fold there is compiled in place and
        /// its `{Value, State}` carrier becomes the operand
        /// (`ConstructCarrierInPlace`).
        UnsequencedOperandInPlace,
        /// BT-4263: an evaluated block with arguments that is a
        /// local-threading producer leaves its `LocalRebind`s bound only in
        /// the parameter scope it pops (`generate_evaluated_block_producer`
        /// skips binding them again), so the frame reads the pre-construct
        /// local (`RebindNotVisible`).
        EvaluatedBlockRebindsScoped,
        /// BT-4461: a REPL producer adopts the bindings map whole and
        /// rethreads none of the bindings its frame holds in Core variables,
        /// as before BT-4461, so the frame reads the value from before the
        /// construct (`ReplCachedBindingStale`).
        ReplCachedBindingsNotRethreaded,
        /// BT-4318: the `super` producer gate answers `false`, so a
        /// `State`-threading `super` send is compiled in place and its
        /// `{reply, Result, NewState}` tuple becomes the value
        /// (`StateEffectEscapesExpression`).
        SuperSendNotAProducer,
        /// BT-4591: the enclosing `sequence_children` pre-sequencing of a
        /// `super` send's arguments is skipped, so a state-writing argument
        /// reaches the cell/loop-body `super` lowering un-sequenced and its
        /// `State` `Bind` is closed into that lowering's argument list
        /// (`StateEffectEscapesExpression`).
        SuperArgsNotPresequenced,
        /// BT-4591: `thread_subexprs` leaves operands in place even when two or
        /// more may run code — the in-place list builder BT-4296 removed — so a
        /// list cell or map literal evaluates its later operand first
        /// (`operand_order_check`'s `RacingOperands`).
        OperandsInPlace,
    }

    thread_local! {
        static ACTIVE: RefCell<HashSet<Fault>> = RefCell::new(HashSet::new());
        static PRODUCERS: RefCell<Option<Vec<ProducerRecord>>> = const { RefCell::new(None) };
        static IN_PLAIN_ARM: Cell<bool> = const { Cell::new(false) };
    }

    /// Whether `fault` is switched on on this thread. Read through the
    /// `fault!` macro, not directly.
    pub fn is_active(fault: Fault) -> bool {
        ACTIVE.with(|active| active.borrow().contains(&fault))
    }

    /// Switches `fault` on or off on this thread. Prefer [`with`], which
    /// restores the fault even if the body panics; this exists for tests that
    /// hold a fault across several compiles.
    pub fn set(fault: Fault, on: bool) {
        ACTIVE.with(|active| {
            if on {
                active.borrow_mut().insert(fault);
            } else {
                active.borrow_mut().remove(&fault);
            }
        });
    }

    /// Runs `body` with `fault` switched on, restoring its previous state
    /// afterwards even if `body` panics.
    pub fn with<T>(fault: Fault, body: impl FnOnce() -> T) -> T {
        struct Restore(Fault, bool);
        impl Drop for Restore {
            fn drop(&mut self) {
                set(self.0, self.1);
            }
        }
        let _restore = Restore(fault, is_active(fault));
        set(fault, true);
        body()
    }

    /// Whether the arm frame being lowered is a plain arm
    /// (`generate_plain_arm`), for [`Fault::ArmLoopOrFoldSitesOnly`].
    pub(in crate::core_erlang) fn in_plain_arm() -> bool {
        IN_PLAIN_ARM.with(Cell::get)
    }

    /// Marks the arm frame being lowered as plain (or not) until the guard is
    /// dropped, restoring the enclosing arm's mark.
    pub(in crate::core_erlang) struct PlainArmScope(bool);

    impl PlainArmScope {
        pub(in crate::core_erlang) fn enter(plain: bool) -> Self {
            Self(IN_PLAIN_ARM.with(|c| c.replace(plain)))
        }
    }

    impl Drop for PlainArmScope {
        fn drop(&mut self) {
            IN_PLAIN_ARM.with(|c| c.set(self.0));
        }
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
    #[cfg_attr(not(test), allow(dead_code))]
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
}
