// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Identity, value, and statement types for [`super::ThreadedIr`] — the
//! bottom layer of the `threaded_ir` module split (ADR 0111 § Addendum 15):
//! [`super::verify`], [`super::emit`], and [`super::build`] all build on the
//! types here; nothing here depends back on them.
//!
//! - Identity/value types: [`FrameId`], [`VersionPrefix`], [`VersionedVar`],
//!   [`AccParam`], [`LoopCounter`], [`VersionCounter`], [`ValueRef`],
//!   [`TokenId`].
//! - Statements: [`ThreadedStmt`], [`BindOp`], [`ThreadingMode`].
//! - Expression-position state effects (ADR 0118): [`ThreadedValue`],
//!   [`CloseContext`].
//!
//! `VersionPrefix::Local`, `ThreadingMode::DirectParams`,
//! `ThreadingMode::Hybrid`, [`LoopCounter`], and `ThreadedStmt::ConditionalLoop`
//! got their first production constructors from ADR 0111 § Addendum 15's
//! Letrec migration (`while_loops.rs`/`counted_loops.rs` lowering onto
//! `ConditionalLoop`); the Foldl migration (the same addendum's next issue)
//! merges each fold's own per-iteration unpack (`TupleAccUnpack`, or a
//! `StateAcc`-map `Statement` prelude) and per-statement body into ONE
//! `Threaded` node (`control_flow::body::generate_foldl_loop_body`),
//! `verify()`d and rendered once — `ThreadingMode::TupleAcc`/`StateAcc`
//! are now full-fidelity, not render-only skeletons.

use super::super::NlrBoundary;
use beamtalk_cerl_doc::Document;
use beamtalk_core::source_analysis::Span;

// ─── StateAccFallbackReason ─────────────────────────────────────────────────

/// Reason why a loop fell back to `StateAcc` threading instead of an optimized mode.
///
/// `PartialEq`/`Eq` added so [`ThreadingMode`] (which wraps this in
/// its `StateAcc` variant) can derive them too — needed for verifier
/// unit-test assertions comparing [`VerifyError`]s.
///
/// Moved here (out of `control_flow/mod.rs`) together with
/// [`CoreErlangGenerator::report_threaded_ir_verify_errors`] to remove a
/// `threaded_ir → control_flow` import cycle — `control_flow` code that
/// needs either now imports from `threaded_ir` instead.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum StateAccFallbackReason {
    /// No fallback — an optimized convention was selected.
    None,
    /// Body contains self-sends (async dispatch requires `gen_server` state).
    SelfSendInBody,
    /// Nested list op with cross-scope mutations incompatible with direct-params.
    NestedListOpCrossScope,
    /// Tier-2 value call on a threaded local (returns `{Result, StateAcc}` tuple).
    Tier2ValueCallOnThreaded,
    /// Inline conditional writes to a threaded local.
    InlineConditionalThreadedWrite,
    /// Condition block has state effects.
    ConditionStateEffects,
    /// Control-flow sub-expression with mutations (e.g. `ifTrue:` with field writes).
    ControlFlowMutations,
    /// No threaded locals (nothing to optimize).
    NoThreadedLocals,
    /// `ValueType` context (no actor State to thread).
    ValueTypeContext,
    /// Not a letrec loop (foldl loops don't support direct-params).
    NotLetrec,
    /// Destructure assignment as last expression (incompatible with tuple-acc).
    DestructureAsLastExpr,
}

impl std::fmt::Display for StateAccFallbackReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::SelfSendInBody => write!(f, "self-send in loop body"),
            Self::NestedListOpCrossScope => {
                write!(f, "nested list op with cross-scope mutation")
            }
            Self::Tier2ValueCallOnThreaded => {
                write!(f, "tier-2 value call on threaded local")
            }
            Self::InlineConditionalThreadedWrite => {
                write!(f, "inline conditional writing to threaded local")
            }
            Self::ConditionStateEffects => write!(f, "condition has state effects"),
            Self::ControlFlowMutations => {
                write!(f, "control-flow sub-expression with mutations")
            }
            Self::NoThreadedLocals => write!(f, "no threaded locals"),
            Self::ValueTypeContext => write!(f, "ValueType context"),
            Self::NotLetrec => write!(f, "not a letrec loop"),
            Self::DestructureAsLastExpr => {
                write!(f, "destructure assignment as last expression")
            }
        }
    }
}

// ─── Frame identity ─────────────────────────────────────────────────────────

/// Frame identity — allocated at each method entry, `with_branch_context`
/// entry, builder-fun entry. Version linearity is PER FRAME: the existing
/// counters are deliberately not SSA (`with_branch_context` resets
/// `state_version` to 0 per branch arm, so sibling arms legitimately both
/// produce `State1` in disjoint scopes). Without frame identity, a linearity
/// check false-positives on every branching method — this field is a design
/// requirement, not a nicety.
///
/// By convention, the top-level `&[ThreadedStmt]` slice passed to [`verify`]
/// always belongs to [`FrameId::ROOT`] (the method's own entry frame); nested
/// frames (branch arms, loop bodies) are allocated fresh `FrameId`s by the
/// lowering pass and introduced via [`ThreadedStmt::Threaded`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(in crate::core_erlang) struct FrameId(u32);

impl FrameId {
    /// The method's own entry frame — the implicit frame of the top-level
    /// slice passed to [`verify`].
    pub(in crate::core_erlang) const ROOT: FrameId = FrameId(0);

    /// Allocates a frame identity for a nested scope (branch arm, loop body,
    /// builder fun).
    pub(in crate::core_erlang) const fn new(id: u32) -> Self {
        Self(id)
    }
}

// ─── VersionedVar ───────────────────────────────────────────────────────────

/// One of the three (formerly independent) version counters, unified in
/// NAMING AND IDENTITY ONLY — plus [`VersionPrefix::Local`] (see module docs
/// §Deviations). Per-prefix scope discipline (state: reset+restore per
/// branch; `class_vars`: restore-only, mutated-flag sticky; self: reset+
/// restore, closing the prior "neither" landmine — see
/// `with_branch_context`'s doc comment in `mod.rs`) remains explicit
/// per-prefix policy, enforced by the generator's `BranchContextGuard`; this
/// type only unifies the *shape*.
///
/// `State`/`ClassVars`/`SelfVt` get their first production call
/// site here — [`VersionCounter`] is the single implementation behind
/// `CoreErlangGenerator`'s three (formerly independently implemented)
/// counters (`StateThreading`, `ClassContext::class_var_version`,
/// `ValueTypeContext::self_version`). `TupleAcc`/`Hybrid`/`StateAcc`,
/// `Put`/`Unpack`, `NlrCatch`/`Return`, and `ValueRef::Version`/`Literal`
/// remain unit-test-only until a control-flow generator migrates onto the
/// full `ThreadedIr`/`verify()` pipeline (later issues — this issue is
/// naming/identity unification only, not IR construction). `Local` gets its
/// own production call site via
/// [`verify_tuple_acc_unpack_invariant`]. `#[allow(dead_code)]` here
/// documents that the remaining variants stay test-only for now, instead of
/// forcing artificial non-test construction sites.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(in crate::core_erlang) enum VersionPrefix {
    /// Actor/instance state (`State`, `State1`, … — rendered as `StateAcc{N}`
    /// inside non-hybrid loop bodies, `with_branch_context` conditional
    /// branches, and `on:do:`/`ensure:` bodies; that rendering
    /// choice is a function of generator context and stays outside the IR,
    /// decided at Document-construction time).
    State,
    /// Class variables (`ClassVars`, `ClassVars1`, … — ADR 0110's mechanism).
    ClassVars,
    /// Value-type fields (`Self`, `Self1`, …).
    SelfVt,
    /// A directly-named loop-threaded local (`Sum`, `Count`, …) as produced
    /// by `ThreadingPlan::threaded_locals` in direct-params / hybrid mode.
    /// See module docs §Deviations for why this exists beyond the ADR's
    /// three-prefix sketch.
    Local(String),
    /// A pre-minted, verbatim Core Erlang name (e.g. `_Sum7`), ADR 0111
    /// Addendum 2 "Gap 2" (`docs/ADR/0111-...md` §Addendum 2, "naming-scheme
    /// mismatch"): real per-iteration loop-local rebinds go through
    /// `fresh_temp_var` (a single module-wide gensym counter shared across
    /// ALL codegen in the method), never through [`VersionPrefix::Local`]'s
    /// bare-prefix-plus-sequential-version scheme (`Sum1`, `Sum2`, …) —
    /// confirmed against real compiled output, not just source reading.
    /// `render_name`
    /// returns the stored string VERBATIM, regardless of `version` — minting
    /// happens once, at LOWERING time (via the same `fresh_temp_var` call
    /// production already makes, in the same order), never at render time.
    /// A render-time memoizing cache was considered and rejected (Addendum
    /// 2, Gap 2 option 1): it defers minting to `render()`, which inverts
    /// mint order relative to legacy whenever a construct (like
    /// `ConditionalLoop`) builds its `exit_arm` from lowered body state —
    /// `exit_arm`'s temps must be minted LAST, after every body rebind, and
    /// a lazy render-time cache cannot honor that ordering. See
    /// [`ThreadedStmt::ConditionalLoop`]'s doc comment for the full
    /// ordering contract.
    Gensym(String),
}

/// A version-identified Core Erlang variable, scoped to the frame that
/// produced it.
///
/// Derives `Ord` so [`verify`] can sort its `NonLinearVersion` findings into
/// a deterministic order before reporting — `HashMap` iteration order is
/// otherwise per-process-random, which would make verifier output (and any
/// future diagnostic consuming it) nondeterministic across compiler runs on
/// identical input.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(in crate::core_erlang) struct VersionedVar {
    pub(in crate::core_erlang) prefix: VersionPrefix,
    pub(in crate::core_erlang) version: usize,
    pub(in crate::core_erlang) frame: FrameId,
}

impl VersionedVar {
    pub(in crate::core_erlang) const fn new(
        prefix: VersionPrefix,
        version: usize,
        frame: FrameId,
    ) -> Self {
        Self {
            prefix,
            version,
            frame,
        }
    }

    /// Renders this variable's Core Erlang name (e.g. `State2`, `ClassVars1`,
    /// `Sum`). Delegates to [`super::super::util::versioned_var`], the single
    /// canonical `prefix{version}` namer (never `format!()` for Core
    /// Erlang fragments). `Local` names are passed through
    /// [`super::super::CoreErlangGenerator::to_core_erlang_var`] — the same
    /// capitalization every other Core Erlang variable name goes through —
    /// so callers may pass the raw Beamtalk identifier (`"sum"`) as it comes
    /// out of `ThreadingPlan::threaded_locals`.
    ///
    /// `pub(in crate::core_erlang)` (widened from private) so [`VersionCounter`]'s
    /// emitter-facing accessors — and `CoreErlangGenerator`'s `StateAcc*`
    /// rendering, which stays outside the IR (see [`VersionPrefix::State`]'s
    /// doc comment) — can render through this single canonical namer instead
    /// of duplicating it.
    pub(in crate::core_erlang) fn render_name(&self) -> String {
        match &self.prefix {
            VersionPrefix::State => super::super::util::versioned_var("State", self.version),
            VersionPrefix::ClassVars => {
                super::super::util::versioned_var("ClassVars", self.version)
            }
            VersionPrefix::SelfVt => super::super::util::versioned_var("Self", self.version),
            VersionPrefix::Local(name) => {
                let core_name = super::super::CoreErlangGenerator::to_core_erlang_var(name);
                super::super::util::versioned_var(&core_name, self.version)
            }
            VersionPrefix::Gensym(name) => name.clone(),
        }
    }
}

// ─── AccParam ─────────────────────────────────────────────────────

/// An unversioned foldl-accumulator lambda **parameter** — e.g. the literal
/// `"StateAcc"` bound by `fun (Item, StateAcc) -> ...` in `basic_ops.rs:84`/
/// `dict_ops.rs:90`, or the `acc_state_var` fresh temp (`AccSt0`, …) other
/// list-ops bind their fold lambda's second parameter to.
///
/// ADR 0111 §Deviations flags this exact identity confusion by name: the
/// unversioned `StateAcc` lambda parameter of foldl list-ops is "two distinct
/// roles one string currently covers" versus the actor/instance `State`
/// [`VersionPrefix::State`] counter, "which the IR must model as distinct
/// nodes." `AccParam` is that distinct node — never a [`VersionedVar`] (it is
/// bound exactly once per lambda invocation, not threaded across a version
/// sequence), so [`verify`]'s linearity/liveness checks do not apply to it;
/// only [`ThreadedStmt::TupleAccUnpack`]'s mode/shape checks do (invariant
/// class 1 — "flat positional-unpack accumulator distinct from
/// parameter threading").
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct AccParam(pub(in crate::core_erlang) String);

impl AccParam {
    pub(in crate::core_erlang) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

// ─── LoopCounter (ADR 0111 Addendum 2, Gap 1) ──────────────────────────────

/// A counted loop's (`to:do:`/`to:by:do:`/`timesRepeat:`/`repeat`) gensym'd
/// loop *index* fun parameter (`frame.counter`, `fresh_temp_var("loopidx")`
/// in production, `counted_loops.rs`'s `CountedLoopFrame::counter`) — an
/// extra [`ThreadedStmt::ConditionalLoop`] fun parameter that is never a
/// `Bind` target or source: `initial`/`next` carry its own opaque, non-`Bind`
/// threading (a literal `1` or the receiver's value for `initial`; `call
/// 'erlang':'+'(Counter, Step)` for `next`) — the same accepted opacity class
/// [`ThreadedStmt::ConditionalLoop`]'s own `exit_arm` field documents.
/// Mirrors [`AccParam`]'s existing precedent for an unversioned,
/// generator-allocated identity kept outside [`VersionedVar`]'s
/// producer/consumer bookkeeping — see Addendum 2's Gap 1 evidence
/// (`docs/ADR/0111-...md` §Addendum 2). `None` for while/`whileFalse:`
/// loops, which have no counter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct LoopCounter {
    /// The gensym'd counter name — the fun's own extra leading parameter,
    /// and every reference to it inside `condition`/`body`.
    pub(in crate::core_erlang) name: String,
    /// The value passed for this parameter by the OUTER (pre-loop) `apply`
    /// call — an integer literal for `timesRepeat:`, the receiver's value
    /// for `to:do:`/`to:by:do:`.
    pub(in crate::core_erlang) initial: Document<'static>,
    /// The expression passed for this parameter by the loop's own
    /// RECURSIVE `apply` call (e.g. `call 'erlang':'+'(Counter, 1)`).
    pub(in crate::core_erlang) next: Document<'static>,
}

impl LoopCounter {
    /// Constructed by `counted_loops.rs`'s Letrec lowering (ADR 0111
    /// Addendum 15) for every counted loop's `ConditionalLoop::counter` —
    /// `None` for while/`whileFalse:` loops, which have no counter.
    pub(in crate::core_erlang) fn new(
        name: impl Into<String>,
        initial: Document<'static>,
        next: Document<'static>,
    ) -> Self {
        Self {
            name: name.into(),
            initial,
            next,
        }
    }
}

// ─── VersionCounter ───────────────────────────────────────────────

/// The single counter implementation behind `CoreErlangGenerator`'s three
/// (formerly independently implemented) version counters — the earlier
/// `StateThreading` struct (`state_codegen.rs`), `ClassContext`'s raw
/// `class_var_version: usize` arithmetic, and `ValueTypeContext`'s raw
/// `self_version: usize` arithmetic. One implementation, reused per prefix.
///
/// **Constructor-only production**: [`Self::next_var`] is the only way to
/// mint a version beyond the counter's current one — an unproduced version
/// cannot be named. [`Self::current_var`] only renders the version already
/// reached; a caller that needs to name the version *the next
/// [`Self::next_var`] call would* reach, without advancing the counter,
/// renders `self.0 + 1` directly (`mod.rs`'s `peek_next_state_var`, via
/// `render_state_prefix`) rather than through a dedicated `VersionCounter`
/// method.
///
/// **Emitter-facing accessors render through [`VersionedVar`]**: every
/// method here goes through [`VersionedVar::new`] + [`VersionedVar::render_name`]
/// rather than re-deriving the `prefix{version}` naming convention, so the
/// naming logic lives in exactly one place (ADR 0111 §Phase A2).
///
/// **Frame identity**: always [`FrameId::ROOT`]. `CoreErlangGenerator` does
/// not track frame identity today (that is the later `ThreadedIr`/`verify()`
/// migration's job) — this counter's own per-prefix
/// save/reset/restore *policy* around branch entry/exit is enforced by the
/// generator's `BranchContextGuard` (`mod.rs`), not by this type.
#[derive(Debug, Clone, Copy, Default)]
pub(in crate::core_erlang) struct VersionCounter(usize);

impl VersionCounter {
    pub(in crate::core_erlang) const fn new() -> Self {
        Self(0)
    }

    /// The raw version number, for callers that need to snapshot/restore it
    /// as a plain `usize` (e.g. `with_branch_context`'s save/restore, or the
    /// scoped inline rollbacks in `dispatch_codegen.rs`/`expressions.rs` that
    /// close an open let-chain from a class-method self-send).
    pub(in crate::core_erlang) const fn version(self) -> usize {
        self.0
    }

    /// Overwrites the raw version number (restore half of a save/restore
    /// pair, or a branch-entry reset to a specific value).
    pub(in crate::core_erlang) fn set_version(&mut self, version: usize) {
        self.0 = version;
    }

    /// Resets to version 0 (the "State"/"Self" bare-prefix, frame-entry
    /// parameter — never itself a [`Self::next_var`] product).
    pub(in crate::core_erlang) fn reset(&mut self) {
        self.0 = 0;
    }

    /// Names the version already reached — never mints.
    pub(in crate::core_erlang) fn current_var(self, prefix: VersionPrefix) -> String {
        VersionedVar::new(prefix, self.0, FrameId::ROOT).render_name()
    }

    /// Mints and names the next version — the only production path.
    pub(in crate::core_erlang) fn next_var(&mut self, prefix: VersionPrefix) -> String {
        self.0 += 1;
        self.current_var(prefix)
    }
}

// ─── Threading mode ─────────────────────────────────────────────────────────

/// A loop or mutation-carrying conditional's already-resolved threading
/// convention — the existing `ThreadingPlan`/`select_direct_params` etc.
/// decision, recorded as durable IR data instead of re-derived at emission.
/// See [`VersionPrefix`]'s doc comment for why several variants are
/// test-only for now.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum ThreadingMode {
    /// `fun (Var1, ..., VarN)` — no `StateAcc` map at all.
    DirectParams,
    /// A flat `{Gate1, ..., GateG, Var1, ..., VarN}` positional-unpack
    /// accumulator (foldl list-ops). The `usize` is `gate_slots` —
    /// the count of leading tuple positions reserved for the op's own result
    /// channel(s) *before* the threaded locals begin (invariant class
    /// 4, "early-exit accumulator liveness"): `0` for `do:`
    /// (`{Var1, ..., VarN}`, `basic_ops.rs`'s `index_offset: 1`); `1` for
    /// `collect:`/`select:`/`reject:`/`inject:into:`/`anySatisfy:`/
    /// `allSatisfy:`/`count:`/`flatMap:`/`groupBy:`/`sort:`
    /// (`{AccList, Var1, ...}` or `{BoolAcc, Var1, ...}`, `index_offset: 2`);
    /// `2` for `takeWhile:`/`dropWhile:`/`detect:`-family ops and
    /// `partition:` (`{AccList, StillTaking, Var1, ...}` / `{FoundItem,
    /// FoundFlag, Var1, ...}` / `{MatchList, NoMatchList, Var1, ...}`,
    /// `index_offset: 3`) — the extra gate slot(s) hold the op's own
    /// in-flight result/continue-flag (or, for `partition:`, its second
    /// result list — same tuple shape, no early exit involved), read
    /// directly by the op's post-fold wrapper (e.g. `search_ops.rs`'s
    /// `bind_detect_found_or_raise_doc`) rather than threaded back out as a
    /// [`VersionedVar`]. See `control_flow::ListOpKind` for the
    /// canonical per-op table this classification is independently
    /// cross-checked against.
    TupleAcc(usize),
    /// `fun (Var1, ..., VarN, RField1, ..., MField1, ...)` — locals plus
    /// pre-extracted read-only/mutated fields as direct params.
    Hybrid,
    /// Fallback: threading rides a `StateAcc` map, unpacked at iteration
    /// start. `reason` records why an optimized mode was not selected
    /// (diagnostics).
    StateAcc(StateAccFallbackReason),
}

// ─── NLR token identity ─────────────────────────────────────────────────────

/// Identifies one NLR try/catch wrapper's fresh token variable
/// (`call 'erlang':'make_ref'()`), distinguishing sibling NLR boundaries from
/// each other. Not a [`VersionedVar`] — the token is a single-use `make_ref`
/// value, never rebound.
///
/// Was `TokenId(u32)` — an opaque numeric identity never rendered directly
/// (`render_nlr_catch` minted its own name independently, at render time,
/// AFTER the body had already rendered). ADR 0111 Addendum 4 §Gap 3 found
/// that inverted relative to production's real mint order (the token
/// consumes the module-wide `fresh_temp_var` counter slot BEFORE the body's
/// own temps — `gen_server/methods.rs`'s `generate_method_dispatch` /
/// `generate_class_method_functions` both mint `NlrToken` before generating
/// the body), a silent byte-identity hazard the moment a real, temp-minting
/// body sits next to an `NlrCatch`. Now carries the literal Core Erlang name
/// minted at lowering time, in production's real mint position — the same
/// "lowering-time pre-allocation, IR carries the rendered name" idiom
/// [`VersionPrefix::Gensym`] established for `Bind` targets. Rendering no
/// longer mints anything for this node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct TokenId(String);

impl TokenId {
    pub(in crate::core_erlang) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(super) fn name(&self) -> &str {
        &self.0
    }
}

// ─── Values ─────────────────────────────────────────────────────────────────

/// A value referenced by a [`BindOp`] or [`ThreadedStmt::Return`]. See
/// [`VersionPrefix`]'s doc comment for why `Version` is test-only for now.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum ValueRef {
    /// A previously-bound versioned variable (e.g. the source of a chained
    /// mutation).
    ///
    /// Still genuinely unconstructed in production — every real
    /// `Bind`/`Return` producer that reaches for a prior version threads
    /// it through the `source: VersionedVar` field directly (`Bind`'s own
    /// dedicated slot) rather than wrapping it as a `ValueRef`; nothing
    /// yet needs a *second*, value-position version reference alongside
    /// `source` in the same node. `render_value`'s arm for it is real,
    /// tested production code (see `render_value_tests`) — only a
    /// constructor is missing, kept ready for a future shape that needs
    /// one (e.g. a `Put`/`Direct` RHS that is itself a bare prior
    /// version, not a fresh temp or opaque `Doc`).
    #[allow(dead_code)]
    Version(VersionedVar),
    /// A fresh, non-versioned Core Erlang variable name (e.g. a computed
    /// `_Val0` RHS temp).
    Var(String),
    /// A literal Core Erlang fragment (e.g. `'nil'`).
    Literal(&'static str),
    /// An opaque, pre-rendered `Document` — ordinary AST-directed
    /// expression codegen with no state-threading content of its own,
    /// exactly the class of embed [`ThreadedStmt::ConditionalLoop`]'s
    /// `continue_arm`/`exit_arm` already use (the `NlrCatch` precedent,
    /// ADR 0111 Addendum 2 §Gap 1). Needed because a real loop-local rebind's
    /// RHS is an arbitrary computed expression (e.g. `call
    /// 'erlang':'+'(Sum, 1)` for `sum := sum + 1`), not merely a reference to
    /// a previously-bound version — [`ValueRef::Version`]/`Var`/`Literal`
    /// alone cannot represent it. `verify()`'s `check_use` does not (and
    /// cannot) look inside a `Doc` value for hidden `VersionedVar`
    /// references — same accepted §Verifier-honesty-class limitation as
    /// `exit_arm`'s own opacity: the RHS is built by the SAME live
    /// generator/scope resolution production already uses, so any prior
    /// version it references resolves correctly by construction, just
    /// unverified by this pass.
    Doc(Document<'static>),
}

/// The mutation an individual [`ThreadedStmt::Bind`] performs. See
/// [`VersionPrefix`]'s doc comment for why `Put`/`Unpack` are test-only for
/// now (the Phase A0 prototype only exercises `Direct`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum BindOp {
    /// A field/class-var mutation: `call 'maps':'put'(field, value, source)`.
    /// `class_tag` is the dynamic class-identity value the shadow write (when
    /// `shadow_write` is set) is keyed on — ADR 0110's amendment:
    /// `{'$bt_class_vars_shadow', element(2, class_tag)}`, never a bare atom,
    /// so two classes relaying through the same process don't clobber each
    /// other's shadow (see `generate_field_assignment`, `expressions.rs:576-588`).
    Put {
        field: String,
        value: ValueRef,
        class_tag: ValueRef,
    },
    /// Unpacks a threaded local from the incoming `StateAcc` map at
    /// loop-iteration start (`generate_unpack_at_iteration_start`) — legal
    /// only inside a [`ThreadingMode::StateAcc`] body; see
    /// [`VerifyError::ThreadingModeUnpackMismatch`].
    ///
    /// Still genuinely unconstructed in production —
    /// `generate_unpack_at_iteration_start`'s `StateAcc`-mode per-iteration
    /// unpack never migrated to real `ThreadedIr` emission (only the
    /// `TupleAcc`-mode unpack did, via
    /// [`ThreadedStmt::TupleAccUnpack`]). `render_bind`'s arm for
    /// it is real, tested production code (see `render_bind_tests`) —
    /// only a lowering-side constructor is missing.
    #[allow(dead_code)]
    Unpack { field: String },
    /// A direct rebind from a computed value (e.g. a direct-params loop's
    /// per-iteration local rebind, or a value-type `Self{N}` rebind).
    Direct(ValueRef),
}

// ─── Statements ─────────────────────────────────────────────────────────────

/// One statement of the lowered IR. See [`VersionPrefix`]'s doc comment for
/// why `NlrCatch`/`Return` are test-only for now (the Phase A0 prototype only
/// exercises `Threaded`/`Bind`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum ThreadedStmt {
    /// A mutation: binds a fresh version from a prior one in the same frame.
    /// `shadow_write` records whether this `Bind` also emits the ADR 0110
    /// process-dictionary shadow write — modeling the side channel explicitly
    /// is what makes [`VerifyError::ShadowWriteMissing`] possible.
    Bind {
        target: VersionedVar,
        source: VersionedVar,
        op: BindOp,
        shadow_write: bool,
        span: Span,
    },

    /// A loop or mutation-carrying conditional, with its mode already
    /// resolved. `produces` lists the versions the body makes available to
    /// its enclosing frame once the construct completes.
    ///
    /// `shadow_write_eligible` (ADR 0111 Addendum 9, Question 1): whether a
    /// class-var mutation inside `body` is eligible for the ADR 0110
    /// shadow write / [`VerifyError::ShadowWriteMissing`] check — read fresh
    /// from `self.block_depth == 0` at construction time (independently
    /// re-derived, never reused from a caller's `shadow_write` value; see
    /// [`construct_and_verify_class_var_bind`]'s doc comment). Orthogonal to
    /// `frame`: `frame` scopes version-linearity, `shadow_write_eligible`
    /// scopes shadow-write eligibility — they only coincided at
    /// `FrameId::ROOT` historically because no non-ROOT frame ever carried a
    /// legitimate class-var mutation before loop/fold bodies did.
    Threaded {
        mode: ThreadingMode,
        frame: FrameId,
        shadow_write_eligible: bool,
        body: Vec<ThreadedStmt>,
        produces: Vec<VersionedVar>,
        span: Span,
    },

    /// An NLR boundary. Faithful to what codegen actually emits
    /// (`wrap_body_with_nlr_catch`, `mod.rs:2480-2549`): the
    /// token-matching catch arm binds its state from the thrown 4-tuple's
    /// pattern variable (`NlrCatchVars::state_var` — a fresh pattern binding,
    /// not a versioned var), and the token-non-matching (foreign) arm carries
    /// nothing and re-raises. The IR does NOT pretend the relay "carries" a
    /// `VersionedVar`.
    NlrCatch {
        boundary: NlrBoundary,
        token: TokenId,
        frame: FrameId,
        span: Span,
    },

    /// A return: the logical value plus the state version threaded out
    /// alongside it.
    Return(ValueRef, VersionedVar, Span),

    /// ADR 0111 Phase C: the `TupleAcc` mode's per-iteration
    /// positional destructure of a flat `{Gate1, .., GateG, Var1, .., VarN}`
    /// accumulator into fresh per-iteration versions of each threaded local —
    /// `generate_foldl_loop_body`'s `element(idx, source)` chain. `param`
    /// is the unversioned fold-lambda parameter the tuple is read from (an
    /// [`AccParam`], never a [`VersionedVar`] — see its doc comment);
    /// `gate_slots` is the number of leading tuple positions this node skips
    /// before `targets` begins (invariant class 4). Distinct from
    /// [`BindOp::Unpack`] (`maps:get`, `StateAcc`-mode only) — this is
    /// invariant class 1, "the flat positional-unpack accumulator distinct
    /// from parameter threading."
    TupleAccUnpack {
        param: AccParam,
        gate_slots: usize,
        targets: Vec<VersionedVar>,
        frame: FrameId,
        span: Span,
    },

    /// A while/counted loop's condition/case-split skeleton (ADR 0111
    /// Addendum 2, Gap 1; condition fields per ADR 0118 phase 3):
    /// `letrec 'fn_name'/N = fun (Params) -> <condition> case
    /// <condition_value> of <continue_arm> <body> apply 'fn_name'/N
    /// (<final_args>) <exit_arm> in apply 'fn_name'/N (<outer_args>)`.
    /// Unlike bare [`Threaded`](Self::Threaded)
    /// (which has no slot for a condition or exit arm — the unconditional
    /// tail-recursion skeleton [`Threaded`](Self::Threaded)'s `DirectParams`/`Hybrid`
    /// modes render), this variant is the REAL shape every while/counted
    /// loop call site emits: a condition test gates a two-arm `case`, one
    /// arm continuing the recursion, the other exiting.
    ///
    /// `produces` carries each threaded local's INITIAL (`version == 0`)
    /// identity, one per fun parameter, in the SAME order as the fun's
    /// parameter list — exactly mirroring bare [`Threaded`](Self::Threaded)'s
    /// existing `produces` convention (`param_list`/`outer_args` are derived
    /// from it by construction, never independently supplied). The loop's
    /// FINAL per-iteration values (the recursive tail call's actual
    /// arguments) are NOT read from `produces` directly — they are
    /// reconstructed by following each local's `Bind` chain forward through
    /// `body` from its `produces` seed (see `render`'s
    /// `final_loop_arg_identities` helper) — because a real rebind's target
    /// is [`VersionPrefix::Gensym`], whose rendering does not vary with
    /// `version`, so the "reset version to 0" trick bare `Threaded` uses to
    /// derive `param_list`/`outer_args` from the SAME field would not recover
    /// the fun's plain parameter name from a Gensym'd final identity.
    ///
    /// Constructed by `while_loops.rs`/`counted_loops.rs`'s Letrec lowering
    /// (ADR 0111 Addendum 15) for every `whileTrue:`/`whileFalse:`/
    /// `timesRepeat:`/`to:do:`/`to:by:do:`/`repeat` loop.
    ConditionalLoop {
        /// Static per-construct function name — `"while"`, `"loop"`,
        /// `"repeat"` — never gensym'd by `render` (ADR 0111 Addendum 2's
        /// "loop-fn-name" finding: production never gensyms this name
        /// either, `while_loops.rs`'s `"while"` literal /
        /// `CountedLoopFrame::fn_name`). Caller-supplied at lowering time,
        /// mirroring `CountedLoopFrame::fn_name`'s existing field type.
        fn_name: String,
        mode: ThreadingMode,
        frame: FrameId,
        /// See [`ThreadedStmt::Threaded`]'s field of the same name (ADR
        /// 0111 Addendum 9, Question 1) — identical meaning and
        /// construction discipline, on the loop-shaped variant.
        shadow_write_eligible: bool,
        /// Present only for counted loops (`to:do:`/`to:by:do:`/
        /// `timesRepeat:`/`repeat`) — `None` for while/`whileFalse:`. See
        /// [`LoopCounter`]'s doc comment.
        counter: Option<LoopCounter>,
        /// ADR 0118 phase 3: the condition block's own prelude,
        /// verified in the SAME frame as `body` (unlike the earlier
        /// `continue_header`, an opaque `Document` compiled OUTSIDE the
        /// loop's frame — the exact shape that panicked the verifier or
        /// miscompiled whenever the condition contained a self-send or an
        /// inline-threaded `and:`/`or:`). Each statement of the condition
        /// block except its tail is a real `ThreadedStmt` here (typically
        /// `Bind`s from a self-send producer, or a plain local-var rebind);
        /// empty for a pure counter compare (a counted loop's own
        /// `continue_arm`/`condition_value` pair, e.g. `Counter =&lt; N`,
        /// needs no prelude — see the sibling `counter` field, above,
        /// unaffected by this phase).
        condition: Vec<ThreadedStmt>,
        /// The condition's own final boolean value — pure with respect to
        /// `condition`, exactly like [`ThreadedValue::value`] is pure with
        /// respect to its `prelude`. The case's scrutinee.
        condition_value: ValueRef,
        /// Opaque continue-arm pattern, e.g. `"<'true'> when 'true' -> "` —
        /// the counterpart of `exit_arm`, naming which case branch
        /// continues looping. Split out of the earlier `continue_header`
        /// now that the scrutinee itself (`condition`/`condition_value`) is
        /// real IR; this remaining fragment carries no state-threading
        /// content of its own (a bare case-clause pattern), the same
        /// accepted opacity class as `exit_arm`'s own pattern half.
        continue_arm: Document<'static>,
        body: Vec<ThreadedStmt>,
        produces: Vec<VersionedVar>,
        /// Overrides the OUTER call's argument list wholesale (`param_list`,
        /// `body`, and the recursive `final_args` are unaffected — they keep
        /// resolving `produces` exactly as documented above). Every real
        /// `while_loops.rs`/`counted_loops.rs` lowering passes `Some` — see
        /// below for why the generic per-entry derivation can never be
        /// trusted for an outer call; `None` exists only for hand-built
        /// `ConditionalLoop` test fixtures that don't exercise outer-call
        /// correctness (a bare counter/local-only loop whose caller happens
        /// to already hold the generic spelling).
        ///
        /// `param_list` renders each `produces` entry under LOOP context, as
        /// the fixed convention name every hand-rolled reference inside
        /// `body` hardcodes (`StateAcc` for the `VersionPrefix::State` entry
        /// — e.g. `generate_unpack_at_iteration_start`'s `maps:get` prelude
        /// — or a fun parameter's own generic `to_core_erlang_var` spelling
        /// for a `VersionPrefix::Local` entry). The value actually LIVE at
        /// the call site, immediately before the loop, does not always match
        /// that spelling:
        /// - a `VersionPrefix::Local` entry (`DirectParams`/`Hybrid` mode's
        ///   threaded locals) is a METHOD PARAMETER's Core Erlang binding
        ///   whenever the local was never reassigned by a plain `:=` before
        ///   the loop — bound to a gensym'd pattern name (e.g. `_startFlag1`
        ///   from unpacking `Args`), not the generic `StartFlag` spelling —
        ///   see [`ThreadingPlan::initial_direct_args`]'s doc comment;
        /// - a `VersionPrefix::State` entry (`StateAcc` mode) is whatever
        ///   [`ThreadingPlan::generate_pack_prefix`] produced: `self`'s own
        ///   ambient `State`/`StateN` only when `threaded_locals` is empty
        ///   and the method already had a live actor state (never true for a
        ///   class method or `ValueType` method, which pack from a fresh
        ///   `maps:new()` instead), and a fresh `PackedN`
        ///   temp whenever any threaded local is packed.
        ///
        /// `produces`' own generic (version-0, ambient-context) derivation
        /// can only ever spell the generic-parameter case above, so it is
        /// wrong whenever a threaded local is a method parameter, packing
        /// occurred, or the method's ambient state version was already
        /// nonzero — this field carries the real values instead, mirroring
        /// legacy's own direct use of `ThreadingPlan::initial_direct_args`/
        /// `generate_pack_prefix`'s returned `init_state` in the initial
        /// `apply` (pre-ADR-0111-Addendum-15 `while_loops.rs`/
        /// `counted_loops.rs`).
        outer_args: Option<Vec<Document<'static>>>,
        /// Opaque exit arm: pattern + exit value + `"end "` — e.g.
        /// `"<'false'> when 'true' -> {'nil', _ExitSA8} end "` (built by
        /// `generate_exit_stateacc`). ORDERING CONSTRAINT (load-bearing for
        /// byte-identity): must be constructed AFTER `body` is lowered — its
        /// `ExitSA` temps share the SAME module-wide `fresh_temp_var`
        /// counter as `body`'s rebind temps, and legacy mints body temps
        /// first. This opacity is NOT sound on `continue_arm`'s grounds —
        /// `exit_arm` genuinely hides state-threading content (the loop
        /// exit's `StateAcc` repack). Deliberate, named
        /// §Verifier-honesty-class limitation for this pilot: the loop exit
        /// repack is unverified by design (ADR 0111 Addendum 2, Gap 1 —
        /// deferred, not rejected; a future structural model is a `Bind`
        /// chain of `Put`s with `Gensym` targets, which this variant already
        /// makes directly expressible).
        exit_arm: Document<'static>,
        span: Span,
    },

    /// An ordinary AST-directed statement, embedded verbatim as one opaque
    /// entry in a straight-line `ThreadedStmt` sequence — the statement-level
    /// counterpart of [`ValueRef::Doc`]'s value-level opacity (ADR 0111
    /// Addendum 3), built by the SAME codegen call production already runs at
    /// this point (`expression_doc`, `generate_self_dispatch_open`, the
    /// `{'reply', ...}`/`{'class_var_result', ...}` epilogue builders, …).
    /// Legal in any straight-line sequence rendered by [`render`]'s top-level
    /// loop (a `gen_server` method body, an [`NlrCatch`](Self::NlrCatch)
    /// try-body). A `Statement`'s `Document` must carry its own correct
    /// trailing glue (e.g. `"let _seq4 = <expr> in "`) — [`render`]'s loop
    /// concatenates with no separator. **Separator note (ADR 0111 Addendum
    /// 4):** `ConditionalLoop` bodies render through
    /// `render_loop_body_statements`, which inserts a literal `" "` between
    /// statements — the first implementation that routes a mixed
    /// `Bind`/`Statement` loop body through `ConditionalLoop` must add a
    /// dual-run byte-parity test before relying on that composition.
    ///
    /// Carries no state-threading content of its own by construction — a
    /// statement that DOES mutate a threaded version must be a `Bind`, never
    /// a `Statement`; there is no `BindOp` escape hatch here the way
    /// [`ValueRef::Doc`] is one inside a `Bind`'s own `op`. This is a
    /// type-level rule enforced by convention and code review, not by
    /// `verify()` (the `Document` is opaque by definition) — the same
    /// distinction `ConditionalLoop` draws between `continue_arm` (sound
    /// opacity) and `exit_arm` (a named, deliberate limitation); a
    /// `Statement` is only ever the `continue_arm` kind.
    Statement(Document<'static>, Span),
}

// ─── ThreadedValue (ADR 0118, Decision 1) ────────────────────────

/// The result of compiling one expression in a state-threading context
/// (ADR 0118 §Decision 1). `prelude` runs first, in source evaluation
/// order, and may advance a versioned prefix (`State` today; `ClassVars`/
/// `Self` in later phases); `value` is then a pure reference to the
/// expression's result — a temp, a literal, or an opaque `Document` that
/// reads only from variables the prelude (or the enclosing frame) already
/// bound.
///
/// A pure expression is `ThreadedValue { prelude: vec![], value }` — the
/// common case costs nothing beyond the wrapper.
///
/// There are exactly two things a holder may do with one:
/// - **splice** it: `stmts.extend(tv.prelude)` into the enclosing frame's
///   own `ThreadedIr`, then use `tv.value` — the prelude's `Bind`s become
///   real, verified nodes of that frame (ADR 0118 §Decision 4);
/// - **close** it via [`ThreadedValue::close`], which renders the prelude as
///   nested `let`s around the value and reports every versioned `Bind`
///   the enclosing context cannot thread as
///   [`VerifyError::StateEffectEscapesExpression`] (§Decision 5).
///
/// `#[must_use]` turns "forgot to do either" from a silent state drop into
/// a compiler warning (denied in CI via `clippy` with warnings as errors).
#[must_use = "a ThreadedValue's prelude carries state Binds; splice it or close it"]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct ThreadedValue {
    /// Statements that must run before `value` is read, in order. Each
    /// state-effecting sub-expression contributes a `Statement` (its
    /// computation) followed by a real `Bind` (the version step it
    /// performs); a sequencing temp is a lone `Statement`.
    pub prelude: Vec<ThreadedStmt>,
    /// The expression's own result, pure with respect to `prelude`.
    pub value: ValueRef,
}

/// What the context that closes a [`ThreadedValue`] can do with the
/// versioned `Bind`s in its prelude — the single input to
/// [`ThreadedValue::close`]'s escape check (ADR 0118 §Decision 5).
///
/// ADR 0118 phase 1a: constructed only by [`ThreadedValue::close`]'s
/// unit tests so far — every phase-1a consumer *splices*; the production
/// `close()` call sites arrive with the consumers that must produce a
/// self-contained `Document` (`expression_doc` in Actor context, phase 2b —
/// see that function's doc comment for why not sooner). Same status as
/// [`ValueRef::Version`]'s constructor.
///
/// An investigation into `Opaque` for exactly the class-method self-send case
/// this variant's own doc names ("a block passed to a class method"):
/// `close_threaded_value_doc` (`util.rs`) is the real, already-shipping
/// choke point every ambient (non-`threaded_expression`) class-method
/// self-send's `ThreadedValue` closes through, and it renders prelude-then-
/// value unconditionally rather than calling `close()` — see its own doc
/// comment for why threading a correct `Opaque`-vs-not signal into it is
/// blocked on the same receiver-class-identity ambiguity
/// `check_no_unsafe_class_method_self_sends`'s doc comment (`expressions.rs`)
/// describes for its own call sites, not on anything specific to `close()`
/// or this enum.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum CloseContext {
    /// Nothing outside the closed `Document` can observe a version the
    /// prelude binds: a Tier 1 closure body, an Erlang FFI argument, a
    /// block passed to a class method, spec/doc codegen, or any consumer
    /// not yet migrated to splicing. Every versioned `Bind` in the prelude
    /// is an escape and is reported.
    Opaque,
    /// The enclosing frame threads the prelude's prefixes itself and the
    /// closed value carries the final versions out (e.g. the closed
    /// `Document` is the `{Value, State}` tail of a threaded arm whose
    /// state operand is the prelude's last `State` version). Closing is
    /// then a pure rendering choice, not a drop — nothing is reported.
    ThreadsState,
}

impl ThreadedValue {
    /// A pure expression: no prelude, just its value.
    ///
    /// ADR 0118 phase 1a: `threaded_expression` builds its pure results
    /// inline (it always has a prelude `Vec` in hand); used by `close()`'s
    /// unit tests. Same status as [`CloseContext`].
    #[allow(dead_code)]
    pub(in crate::core_erlang) fn pure(value: ValueRef) -> Self {
        Self {
            prelude: Vec::new(),
            value,
        }
    }

    /// `true` if `value` is a bare variable or literal — reading it can
    /// neither raise nor observe state, so a later sibling's prelude may
    /// be spliced ahead of it without binding it to a sequencing temp
    /// first (ADR 0118 §Decision 3's "not a literal or plain variable"
    /// exemption).
    pub(in crate::core_erlang) fn value_is_trivial(&self) -> bool {
        matches!(
            self.value,
            ValueRef::Var(_) | ValueRef::Literal(_) | ValueRef::Version(_)
        )
    }
}
