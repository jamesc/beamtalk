// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadedIr` — a small, narrowly-scoped mid-level IR for the
//! state-threading / control-flow subset of Core Erlang codegen (ADR 0111).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ## Status (BT-3133 — ADR 0111 Phase C, list-op migration)
//!
//! This module lands the IR types, the [`verify`] checker, and the
//! [`lower_and_render`] test shim (BT-3129), the unified `VersionedVar`/
//! `VersionCounter` production path (BT-3131), and — as of BT-3132 —
//! `while_loops.rs`'s and `counted_loops.rs`'s (via `control_flow/mod.rs`)
//! direct-params/hybrid loop generators construct and [`verify`] a real
//! [`ThreadedIr`] fragment for their per-iteration "optimized mode implies
//! no `StateAcc` unpack" invariant, via [`verify_loop_unpack_invariant`] —
//! this replaces the four `debug_assert!`s the pre-BT-3132 code used for the
//! same check with [`VerifyError::ThreadingModeUnpackMismatch`]. The Phase
//! A0 measurement prototype ([`prototype_direct_params_ir`], gated behind
//! `BEAMTALK_THREADED_IR_WHILE=1`) remains alongside it — it measures a
//! different, larger fixture (one `Bind` per threaded local's full
//! per-iteration rebind) and its output is still discarded, unlike the
//! unpack-invariant check's `Vec<VerifyError>`, which drives real
//! debug-fail/release-diagnostic behavior.
//!
//! As of BT-3133, every `list_ops/*.rs` and `dict_ops.rs` foldl call site —
//! `do:`, `collect:`, `select:`/`reject:`, `inject:into:`, `anySatisfy:`/
//! `allSatisfy:`, `detect:`, `takeWhile:`/`dropWhile:`, and dictionary `do:`
//! — also constructs and [`verify`]s a real `ThreadedIr` fragment, via
//! [`verify_tuple_acc_unpack_invariant`] (invariant classes 1 + 4,
//! single-sourced at `ThreadingPlan::generate_tuple_unpack_docs`),
//! [`verify_tuple_acc_value_type_exclusion`] (invariant class 2, at
//! `ThreadingPlan::new_impl`), and [`verify_nested_list_op_stateacc_compat`]
//! (invariant class 3, also at `ThreadingPlan::new_impl`) — introducing
//! [`AccParam`] (the unversioned foldl-accumulator lambda parameter,
//! distinct from [`VersionedVar`]) and [`ThreadedStmt::TupleAccUnpack`] (the
//! flat positional-unpack accumulator shape) to model them. No
//! `debug_assert!`s existed in these files to delete (list-ops never had the
//! loop-unpack duplication BT-3132 closed). The rest of the four production
//! generators' state (field mutations, NLR, shadow writes) does not yet
//! construct `ThreadedIr` — later phases (BT-3134/BT-3135) extend coverage.
//!
//! ## Scope
//!
//! Covers state-version bindings (with frame identity), threading-mode
//! selection, shadow-write emission (the ADR 0110 contract), and NLR relay
//! boundaries. Everything else in codegen stays AST-directed and unaffected —
//! see ADR 0111 §Decision / §Constraints for the full narrow-scope rationale.
//!
//! ## Deviations from the ADR's illustrative pseudocode
//!
//! The ADR's `## The IR` code block is deliberately abbreviated for
//! readability; two additions were necessary to make the types real and
//! source-attributable:
//!
//! 1. **`Span` fields** on [`ThreadedStmt::Bind`], [`ThreadedStmt::Threaded`],
//!    [`ThreadedStmt::NlrCatch`], and [`ThreadedStmt::Return`] — required so
//!    [`VerifyError`] can carry a Beamtalk-source-attributed location, which
//!    is the entire diagnosis-quality point of the verifier (§The verifier).
//! 2. **[`VersionPrefix::Local`]**, a fourth prefix beyond the ADR's
//!    `State | ClassVars | SelfVt` sketch. The three original prefixes name
//!    the counters this ADR *unifies* (`next_state_var`/`next_class_var`/
//!    `next_self_var`); `whileTrue:`/`whileFalse:` direct-params mode (BT-1275
//!    — this issue's Phase A0 target) threads *named loop locals*
//!    (`Sum`/`Sum1`/…) that never go through any of those three counters at
//!    all — they come from `ThreadingPlan::threaded_locals` and ordinary
//!    scope binding. `Local` models that identity uniformly so the verifier's
//!    frame/linearity checks apply to it too, without touching the three
//!    counters' unification story.

use std::collections::HashMap;

use super::NlrBoundary;
use super::control_flow::StateAccFallbackReason;
use super::document::{Document, leaf};
use crate::docvec;
use crate::source_analysis::Span;

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
pub(super) struct FrameId(u32);

impl FrameId {
    /// The method's own entry frame — the implicit frame of the top-level
    /// slice passed to [`verify`].
    pub(super) const ROOT: FrameId = FrameId(0);

    /// Allocates a frame identity for a nested scope (branch arm, loop body,
    /// builder fun).
    pub(super) const fn new(id: u32) -> Self {
        Self(id)
    }
}

// ─── VersionedVar ───────────────────────────────────────────────────────────

/// One of the three (formerly independent) version counters, unified in
/// NAMING AND IDENTITY ONLY — plus [`VersionPrefix::Local`] (see module docs
/// §Deviations). Per-prefix scope discipline (state: reset+restore per
/// branch; `class_vars`: restore-only, mutated-flag sticky; self: reset+
/// restore, BT-3131's fix for the prior "neither" landmine — see
/// `with_branch_context`'s doc comment in `mod.rs`) remains explicit
/// per-prefix policy, enforced by the generator's `BranchContextGuard`; this
/// type only unifies the *shape*.
///
/// BT-3131: `State`/`ClassVars`/`SelfVt` get their first production call
/// site here — [`VersionCounter`] is the single implementation behind
/// `CoreErlangGenerator`'s three (formerly independently implemented)
/// counters (`StateThreading`, `ClassContext::class_var_version`,
/// `ValueTypeContext::self_version`). `TupleAcc`/`Hybrid`/`StateAcc`,
/// `Put`/`Unpack`, `NlrCatch`/`Return`, and `ValueRef::Version`/`Literal`
/// remain unit-test-only until a control-flow generator migrates onto the
/// full `ThreadedIr`/`verify()` pipeline (later issues — this issue is
/// naming/identity unification only, not IR construction). `Local` stays
/// exercised only by the Phase A0 measurement prototype
/// (`prototype_direct_params_ir`). `#[allow(dead_code)]` here documents that
/// expectation instead of forcing artificial non-test construction sites.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) enum VersionPrefix {
    /// Actor/instance state (`State`, `State1`, … — rendered as `StateAcc{N}`
    /// inside non-hybrid loop bodies; that rendering choice is a function of
    /// generator context and stays outside the IR, decided at
    /// Document-construction time).
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
pub(super) struct VersionedVar {
    pub(super) prefix: VersionPrefix,
    pub(super) version: usize,
    pub(super) frame: FrameId,
}

impl VersionedVar {
    pub(super) const fn new(prefix: VersionPrefix, version: usize, frame: FrameId) -> Self {
        Self {
            prefix,
            version,
            frame,
        }
    }

    /// Renders this variable's Core Erlang name (e.g. `State2`, `ClassVars1`,
    /// `Sum`). Delegates to [`super::util::versioned_var`], the single
    /// canonical `prefix{version}` namer (BT-875: never `format!()` for Core
    /// Erlang fragments). `Local` names are passed through
    /// [`super::CoreErlangGenerator::to_core_erlang_var`] — the same
    /// capitalization every other Core Erlang variable name goes through —
    /// so callers may pass the raw Beamtalk identifier (`"sum"`) as it comes
    /// out of `ThreadingPlan::threaded_locals`.
    ///
    /// BT-3131: `pub(super)` (widened from private) so [`VersionCounter`]'s
    /// emitter-facing accessors — and `CoreErlangGenerator`'s `StateAcc*`
    /// rendering, which stays outside the IR (see [`VersionPrefix::State`]'s
    /// doc comment) — can render through this single canonical namer instead
    /// of duplicating it.
    pub(super) fn render_name(&self) -> String {
        match &self.prefix {
            VersionPrefix::State => super::util::versioned_var("State", self.version),
            VersionPrefix::ClassVars => super::util::versioned_var("ClassVars", self.version),
            VersionPrefix::SelfVt => super::util::versioned_var("Self", self.version),
            VersionPrefix::Local(name) => {
                let core_name = super::CoreErlangGenerator::to_core_erlang_var(name);
                super::util::versioned_var(&core_name, self.version)
            }
        }
    }
}

// ─── AccParam (BT-3133) ─────────────────────────────────────────────────────

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
/// only [`ThreadedStmt::TupleAccUnpack`]'s mode/shape checks do (BT-3133
/// invariant class 1 — "flat positional-unpack accumulator distinct from
/// parameter threading").
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct AccParam(pub(super) String);

impl AccParam {
    pub(super) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

// ─── VersionCounter (BT-3131) ───────────────────────────────────────────────

/// The single counter implementation behind `CoreErlangGenerator`'s three
/// (formerly independently implemented) version counters — the pre-BT-3131
/// `StateThreading` struct (`state_codegen.rs`), `ClassContext`'s raw
/// `class_var_version: usize` arithmetic, and `ValueTypeContext`'s raw
/// `self_version: usize` arithmetic. One implementation, reused per prefix.
///
/// **Constructor-only production**: [`Self::next_var`] is the only way to
/// mint a version beyond the counter's current one — an unproduced version
/// cannot be named. [`Self::current_var`] and [`Self::peek_next_var`] never
/// mint; they only render the version already reached (or, for `peek`, the
/// version *the next [`Self::next_var`] call would* reach, without
/// advancing the counter — used where the caller needs the name before
/// calling `expression_doc`, which may itself advance the counter).
///
/// **Emitter-facing accessors render through [`VersionedVar`]**: every
/// method here goes through [`VersionedVar::new`] + [`VersionedVar::render_name`]
/// rather than re-deriving the `prefix{version}` naming convention, so the
/// naming logic lives in exactly one place (ADR 0111 §Phase A2).
///
/// **Frame identity**: always [`FrameId::ROOT`]. `CoreErlangGenerator` does
/// not track frame identity today (that is the later `ThreadedIr`/`verify()`
/// migration's job, BT-3132 onward) — this counter's own per-prefix
/// save/reset/restore *policy* around branch entry/exit is enforced by the
/// generator's `BranchContextGuard` (`mod.rs`), not by this type.
#[derive(Debug, Clone, Copy, Default)]
pub(super) struct VersionCounter(usize);

impl VersionCounter {
    pub(super) const fn new() -> Self {
        Self(0)
    }

    /// The raw version number, for callers that need to snapshot/restore it
    /// as a plain `usize` (e.g. `with_branch_context`'s save/restore, or the
    /// scoped inline rollbacks in `dispatch_codegen.rs`/`expressions.rs` that
    /// close an open let-chain from a class-method self-send).
    pub(super) const fn version(self) -> usize {
        self.0
    }

    /// Overwrites the raw version number (restore half of a save/restore
    /// pair, or a branch-entry reset to a specific value).
    pub(super) fn set_version(&mut self, version: usize) {
        self.0 = version;
    }

    /// Resets to version 0 (the "State"/"Self" bare-prefix, frame-entry
    /// parameter — never itself a [`Self::next_var`] product).
    pub(super) fn reset(&mut self) {
        self.0 = 0;
    }

    /// Names the version already reached — never mints.
    pub(super) fn current_var(self, prefix: VersionPrefix) -> String {
        VersionedVar::new(prefix, self.0, FrameId::ROOT).render_name()
    }

    /// Mints and names the next version — the only production path.
    pub(super) fn next_var(&mut self, prefix: VersionPrefix) -> String {
        self.0 += 1;
        self.current_var(prefix)
    }

    /// Names the version [`Self::next_var`] would mint, without advancing
    /// the counter.
    pub(super) fn peek_next_var(self, prefix: VersionPrefix) -> String {
        VersionedVar::new(prefix, self.0 + 1, FrameId::ROOT).render_name()
    }
}

// ─── Threading mode ─────────────────────────────────────────────────────────

/// A loop or mutation-carrying conditional's already-resolved threading
/// convention — the existing `ThreadingPlan`/`select_direct_params` etc.
/// decision, recorded as durable IR data instead of re-derived at emission.
/// See [`VersionPrefix`]'s doc comment for why several variants are
/// test-only for now.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum ThreadingMode {
    /// `fun (Var1, ..., VarN)` — no `StateAcc` map at all (BT-1275).
    DirectParams,
    /// A flat `{Gate1, ..., GateG, Var1, ..., VarN}` positional-unpack
    /// accumulator (foldl list-ops, BT-1276). The `usize` is `gate_slots` —
    /// the count of leading tuple positions reserved for the op's own result
    /// channel(s) *before* the threaded locals begin (BT-3133 invariant class
    /// 4, "early-exit accumulator liveness"): `0` for `do:`
    /// (`{Var1, ..., VarN}`, `basic_ops.rs`'s `index_offset: 1`); `1` for
    /// `collect:`/`select:`/`reject:`/`inject:into:`/`anySatisfy:`/
    /// `allSatisfy:`/`count:`/`flatMap:`/`partition:`/`groupBy:`/`sort:`
    /// (`{AccList, Var1, ...}` or `{BoolAcc, Var1, ...}`, `index_offset: 2`);
    /// `2` for `takeWhile:`/`dropWhile:`/`detect:`-family ops
    /// (`{AccList, StillTaking, Var1, ...}` / `{FoundItem, FoundFlag, Var1,
    /// ...}`, `index_offset: 3`) — the extra gate slot(s) hold the op's
    /// own in-flight result/continue-flag, read directly by the op's
    /// post-fold wrapper (e.g. `search_ops.rs`'s `bind_detect_found_or_raise_doc`)
    /// rather than threaded back out as a [`VersionedVar`].
    TupleAcc(usize),
    /// `fun (Var1, ..., VarN, RField1, ..., MField1, ...)` — locals plus
    /// pre-extracted read-only/mutated fields as direct params (BT-1326/BT-1342).
    Hybrid,
    /// Fallback: threading rides a `StateAcc` map, unpacked at iteration
    /// start. `reason` records why an optimized mode was not selected
    /// (BT-1343 diagnostics).
    StateAcc(StateAccFallbackReason),
}

// ─── NLR token identity ─────────────────────────────────────────────────────

/// Identifies one NLR try/catch wrapper's fresh token variable
/// (`call 'erlang':'make_ref'()`), distinguishing sibling NLR boundaries from
/// each other. Not a [`VersionedVar`] — the token is a single-use `make_ref`
/// value, never rebound. See [`VersionPrefix`]'s doc comment: constructed
/// only by unit tests until NLR lowering migrates onto this IR (BT-3135).
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct TokenId(u32);

impl TokenId {
    #[allow(dead_code)]
    pub(super) const fn new(id: u32) -> Self {
        Self(id)
    }
}

// ─── Values ─────────────────────────────────────────────────────────────────

/// A value referenced by a [`BindOp`] or [`ThreadedStmt::Return`]. See
/// [`VersionPrefix`]'s doc comment for why `Version` is test-only for now.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum ValueRef {
    /// A previously-bound versioned variable (e.g. the source of a chained
    /// mutation).
    Version(VersionedVar),
    /// A fresh, non-versioned Core Erlang variable name (e.g. a computed
    /// `_Val0` RHS temp).
    Var(String),
    /// A literal Core Erlang fragment (e.g. `'nil'`).
    Literal(&'static str),
}

/// The mutation an individual [`ThreadedStmt::Bind`] performs. See
/// [`VersionPrefix`]'s doc comment for why `Put`/`Unpack` are test-only for
/// now (the Phase A0 prototype only exercises `Direct`).
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum BindOp {
    /// A field/class-var mutation: `call 'maps':'put'(field, value, source)`.
    /// `class_tag` is the dynamic class-identity value the shadow write (when
    /// `shadow_write` is set) is keyed on — ADR 0110's BT-3039 amendment:
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
    Unpack { field: String },
    /// A direct rebind from a computed value (e.g. a direct-params loop's
    /// per-iteration local rebind, or a value-type `Self{N}` rebind).
    Direct(ValueRef),
}

// ─── Statements ─────────────────────────────────────────────────────────────

/// One statement of the lowered IR. See [`VersionPrefix`]'s doc comment for
/// why `NlrCatch`/`Return` are test-only for now (the Phase A0 prototype only
/// exercises `Threaded`/`Bind`).
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum ThreadedStmt {
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
    Threaded {
        mode: ThreadingMode,
        frame: FrameId,
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

    /// BT-3133 (ADR 0111 Phase C): the `TupleAcc` mode's per-iteration
    /// positional destructure of a flat `{Gate1, .., GateG, Var1, .., VarN}`
    /// accumulator into fresh per-iteration versions of each threaded local —
    /// `generate_tuple_unpack_docs`'s `element(idx, source)` chain. `param`
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
}

// ─── Verifier ───────────────────────────────────────────────────────────────

/// A violated invariant, found by [`verify`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum VerifyError {
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

    /// The ADR 0110 CONTRACT check — regression-pinning, not counterfactual
    /// detection (see ADR §Verifier honesty). A class-var `Bind` at frame
    /// depth 0 (method top frame) inside a method whose body can relay a
    /// foreign NLR (a `NlrCatch` with `boundary: ClassMethod { has_class_vars:
    /// true }` present) MUST have `shadow_write: true`. Fires if a future
    /// emission path forgets the shadow write ADR 0110's fix depends on, or
    /// if a new mutation site is added without it.
    ShadowWriteMissing { mutated: VersionedVar, at: Span },

    /// BT-3133 invariant class 1: a [`ThreadedStmt::TupleAccUnpack`] node
    /// (the flat positional-unpack accumulator discipline) appeared outside
    /// a [`ThreadingMode::TupleAcc`] body. Mirrors `ThreadingModeUnpackMismatch`
    /// for the tuple-shaped (rather than map-shaped) accumulator; in release
    /// today this degrades to a `core_lint` unbound-variable or badarg-on-
    /// `element/2` error, one layer further from the cause.
    TupleAccUnpackModeMismatch { mode: ThreadingMode, at: Span },

    /// BT-3133 invariant class 4 ("early-exit accumulator liveness"): a
    /// [`ThreadedStmt::TupleAccUnpack`] node's own `gate_slots` disagrees with
    /// its enclosing [`ThreadingMode::TupleAcc`]'s `gate_slots`. Each list-op
    /// family reserves a different number of leading accumulator slots for
    /// its own result/continuation state (`do:`: 0; `collect:`/`select:`/
    /// boolean-predicate ops: 1; `takeWhile:`/`dropWhile:`/`detect:`-family:
    /// 2) — a mismatch here means the unpack would read threaded-local values
    /// from the wrong tuple positions: well-formed Core Erlang, silently
    /// wrong values (the ADR 0110 danger class, not a `core_lint` failure).
    EarlyExitGateSlotMismatch {
        mode_gate_slots: usize,
        node_gate_slots: usize,
        at: Span,
    },

    /// BT-3133 invariant class 2: `select_tuple_acc`'s `ValueType`-context
    /// exclusion (`control_flow/mod.rs`'s `select_tuple_acc`), pinned
    /// structurally. `ValueType` methods have no actor `State` `gen_server`
    /// variable to reference — `TupleAcc` mode is unconditionally
    /// unavailable there, and this fires if a future change to
    /// `select_tuple_acc`'s guard ordering ever lets `use_tuple_acc` become
    /// `true` in a `ValueType` context. Regression-pinning, like
    /// `ShadowWriteMissing` (see ADR §Verifier honesty) — `select_tuple_acc`'s
    /// own early-return already makes this unreachable today.
    TupleAccInValueTypeContext { at: Span },

    /// BT-3133 invariant class 3: the recursive inter-construct fallback
    /// invariant `list_op_needs_stateacc_fallback_recursive` encodes
    /// (`control_flow/mod.rs`), pinned structurally. A nested list-op whose
    /// own inner block cannot use tuple-acc (so it falls back to a
    /// `StateAcc`-map accumulator to thread its cross-scope mutation) is
    /// incompatible with an *enclosing* `DirectParams` loop: `DirectParams`
    /// has no `StateAcc` map for the inner loop's `{value, StateAcc}` result
    /// to unpack into. Fires if `select_direct_params`'s
    /// `!effects.has_non_tuple_safe_list_op` guard is ever dropped or
    /// reordered past the point where `DirectParams` is selected.
    NestedStateAccFallbackUnderDirectParams { at: Span },
}

/// Checks `ir` against the invariants documented on each [`VerifyError`]
/// variant. Returns an empty `Vec` when `ir` is well-formed.
///
/// **Failure behavior** (per ADR 0111 §The verifier / CLAUDE.md error-recovery
/// rules): this function never panics — callers in debug/CI treat a non-empty
/// result as a hard failure; release callers must degrade a non-empty result
/// to an internal-error diagnostic, never a panic or a refusal to compile.
/// (No call site does either yet — see module docs §Status.)
pub(super) fn verify(ir: &[ThreadedStmt]) -> Vec<VerifyError> {
    let has_class_vars_nlr = contains_class_var_nlr_catch(ir);

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
        has_class_vars_nlr,
        frame_stack: vec![FrameId::ROOT],
        mode_stack: Vec::new(),
        errors: &mut errors,
    };
    walk.walk(ir);

    errors
}

/// Recursively scans `ir` for an `NlrCatch` whose boundary is
/// `ClassMethod { has_class_vars: true }` — the precondition for
/// [`VerifyError::ShadowWriteMissing`].
fn contains_class_var_nlr_catch(ir: &[ThreadedStmt]) -> bool {
    ir.iter().any(|stmt| match stmt {
        ThreadedStmt::NlrCatch { boundary, .. } => {
            matches!(
                boundary,
                NlrBoundary::ClassMethod {
                    has_class_vars: true
                }
            )
        }
        ThreadedStmt::Threaded { body, .. } => contains_class_var_nlr_catch(body),
        ThreadedStmt::Bind { .. }
        | ThreadedStmt::Return(..)
        | ThreadedStmt::TupleAccUnpack { .. } => false,
    })
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
            ThreadedStmt::Threaded { body, .. } => {
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
            ThreadedStmt::NlrCatch { .. } | ThreadedStmt::Return(..) => {}
        }
    }
}

/// Second pass: walks `ir` tracking the active frame/mode nesting, checking
/// [`VerifyError::UnboundVersion`], [`VerifyError::ThreadingModeUnpackMismatch`],
/// and [`VerifyError::ShadowWriteMissing`] (`NonLinearVersion` is fully
/// determined by the first pass's counts and checked before this walk runs).
struct VerifyWalk<'a> {
    producers: &'a HashMap<VersionedVar, usize>,
    has_class_vars_nlr: bool,
    frame_stack: Vec<FrameId>,
    mode_stack: Vec<ThreadingMode>,
    errors: &'a mut Vec<VerifyError>,
}

impl VerifyWalk<'_> {
    fn walk(&mut self, stmts: &[ThreadedStmt]) {
        for stmt in stmts {
            self.walk_stmt(stmt);
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

    fn walk_stmt(&mut self, stmt: &ThreadedStmt) {
        match stmt {
            ThreadedStmt::Bind {
                target,
                source,
                op,
                shadow_write,
                span,
            } => {
                self.check_use(source, *span);
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
                if matches!(target.prefix, VersionPrefix::ClassVars)
                    && target.frame == FrameId::ROOT
                    && !*shadow_write
                    && self.has_class_vars_nlr
                {
                    self.errors.push(VerifyError::ShadowWriteMissing {
                        mutated: target.clone(),
                        at: *span,
                    });
                }
            }
            ThreadedStmt::Threaded {
                mode,
                frame,
                body,
                produces,
                span: _,
            } => {
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.walk(body);
                for v in produces {
                    self.check_use(v, Span::default());
                }
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            ThreadedStmt::NlrCatch { .. } => {}
            ThreadedStmt::Return(value, state, span) => {
                self.check_use(state, *span);
                if let ValueRef::Version(v) = value {
                    self.check_use(v, *span);
                }
            }
            ThreadedStmt::TupleAccUnpack {
                gate_slots, span, ..
            } => {
                // Mirrors the `BindOp::Unpack` check above (invariant class 1:
                // legal only inside `TupleAcc` mode), plus invariant class 4:
                // the node's own `gate_slots` must match the enclosing mode's.
                if let Some(mode) = self.mode_stack.last() {
                    match mode {
                        ThreadingMode::TupleAcc(mode_gate_slots) => {
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

// ─── Test shim: lower_and_render ────────────────────────────────────────────

/// Renders `ir` to a [`Document`], mirroring (at skeleton fidelity) the
/// `let Target = <op> in ...` chains the real generator emits for the same
/// decisions. This is the "existing `Document`-asserting tests survive
/// verbatim" shim the ADR commits to for later migration issues (BT-3131
/// onward): once a production call site's return type moves from `Document`
/// to `ThreadedIr`, its tests migrate to `lower_and_render(&ir).to_pretty_string()`
/// unchanged.
///
/// `Threaded` and `NlrCatch` render at **skeleton fidelity only** — full
/// `letrec`/try-catch scaffolding requires fresh-variable allocation from
/// `CoreErlangGenerator` (`fresh_temp_var`, `alloc_nlr_catch_vars`) that this
/// pure `&[ThreadedStmt] -> Document` shim deliberately does not have access
/// to (constructing that scaffolding is exactly what the later migration
/// issues add, one call site at a time).
pub(super) fn lower_and_render(ir: &[ThreadedStmt]) -> Document<'static> {
    Document::Vec(ir.iter().map(render_stmt).collect())
}

fn render_stmt(stmt: &ThreadedStmt) -> Document<'static> {
    match stmt {
        ThreadedStmt::Bind {
            target,
            source,
            op,
            shadow_write,
            span: _,
        } => render_bind(target, source, op, *shadow_write),
        ThreadedStmt::Threaded { body, .. } => lower_and_render(body),
        ThreadedStmt::NlrCatch {
            boundary, token, ..
        } => render_nlr_catch(*boundary, *token),
        ThreadedStmt::Return(value, state, _) => render_return(value, state),
        ThreadedStmt::TupleAccUnpack {
            param,
            gate_slots,
            targets,
            ..
        } => render_tuple_acc_unpack(param, *gate_slots, targets),
    }
}

/// Skeleton-fidelity rendering of [`ThreadedStmt::TupleAccUnpack`], mirroring
/// `generate_tuple_unpack_docs`'s `let V = call 'erlang':'element'(idx, Param)
/// in` chain — `idx` starts at `gate_slots + 1` (1-based, past the leading
/// gate slots) for the first target.
fn render_tuple_acc_unpack(
    param: &AccParam,
    gate_slots: usize,
    targets: &[VersionedVar],
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();
    for (i, target) in targets.iter().enumerate() {
        let idx = gate_slots + 1 + i;
        docs.push(docvec![
            "let ",
            leaf::var(target.render_name()),
            " = call 'erlang':'element'(",
            leaf::int_lit(i64::try_from(idx).unwrap_or(0)),
            ", ",
            leaf::var(param.0.clone()),
            ") in ",
        ]);
    }
    Document::Vec(docs)
}

fn render_value(value: &ValueRef) -> Document<'static> {
    match value {
        ValueRef::Version(v) => leaf::var(v.render_name()),
        ValueRef::Var(name) => leaf::var(name.clone()),
        ValueRef::Literal(lit) => Document::Str(lit),
    }
}

fn render_bind(
    target: &VersionedVar,
    source: &VersionedVar,
    op: &BindOp,
    shadow_write: bool,
) -> Document<'static> {
    let target_name = target.render_name();
    let source_name = source.render_name();
    match op {
        BindOp::Put {
            field,
            value,
            class_tag,
        } => {
            let put_doc = docvec![
                "let ",
                leaf::var(target_name.clone()),
                " = call 'maps':'put'(",
                leaf::atom(field.clone()),
                ", ",
                render_value(value),
                ", ",
                leaf::var(source_name),
                ") in ",
            ];
            if shadow_write {
                docvec![
                    put_doc,
                    "let _ = call 'erlang':'put'({",
                    leaf::atom("$bt_class_vars_shadow"),
                    ", call 'erlang':'element'(2, ",
                    render_value(class_tag),
                    ")}, ",
                    leaf::var(target_name),
                    ") in ",
                ]
            } else {
                put_doc
            }
        }
        BindOp::Unpack { field } => docvec![
            "let ",
            leaf::var(target_name),
            " = call 'maps':'get'(",
            leaf::atom(field.clone()),
            ", ",
            leaf::var(source_name),
            ") in ",
        ],
        BindOp::Direct(value) => docvec![
            "let ",
            leaf::var(target_name),
            " = ",
            render_value(value),
            " in ",
        ],
    }
}

/// Skeleton-fidelity placeholder — see [`lower_and_render`]'s doc comment.
fn render_nlr_catch(boundary: NlrBoundary, token: TokenId) -> Document<'static> {
    let tag = match boundary {
        NlrBoundary::ActorReply => "actor_reply",
        NlrBoundary::ClassMethod { .. } => "class_method",
        NlrBoundary::ValueType => "value_type",
    };
    docvec![
        "% nlr_catch(",
        tag,
        ", token=",
        leaf::var(token.render_name()),
        ")\n"
    ]
}

fn render_return(value: &ValueRef, state: &VersionedVar) -> Document<'static> {
    docvec![
        "{",
        render_value(value),
        ", ",
        leaf::var(state.render_name()),
        "}"
    ]
}

impl TokenId {
    /// Renders a placeholder variable name for skeleton-fidelity NLR
    /// rendering (see [`render_nlr_catch`]).
    fn render_name(self) -> String {
        super::util::versioned_var("CatchTok", self.0 as usize)
    }
}

// ─── Loop unpack-invariant check (BT-3132) ─────────────────────────────────

/// Builds a minimal `ThreadedIr` fixture for one loop's per-iteration unpack
/// step under `mode` (whichever of [`ThreadingMode::DirectParams`] /
/// [`ThreadingMode::Hybrid`] the loop's `ThreadingPlan` resolved) and
/// verifies it.
///
/// `unpack_emitted` must be exactly what
/// `ThreadingPlan::generate_unpack_at_iteration_start` actually returned for
/// this loop (non-empty iff every threaded local produced a `maps:get`
/// unpack `Bind`) — this function checks the OBSERVED emission against the
/// already-resolved mode, not a re-derivation of `ThreadingPlan`'s own
/// `use_direct_params`/`use_hybrid_params` flags, so it stays a structural
/// check on what was actually emitted rather than "the generator agreeing
/// with itself" (ADR 0111 §Verifier honesty).
///
/// Replaces the four "unpack should emit no code" `debug_assert!`s
/// (`while_loops.rs`'s direct-params/hybrid call sites, `counted_loops.rs`'s
/// via `control_flow/mod.rs`'s direct/hybrid call sites) — their invariant
/// is now [`VerifyError::ThreadingModeUnpackMismatch`].
pub(super) fn verify_loop_unpack_invariant(
    mode: ThreadingMode,
    threaded_locals: &[String],
    unpack_emitted: bool,
    span: Span,
) -> Vec<VerifyError> {
    let frame = FrameId::new(1);
    let body: Vec<ThreadedStmt> = if unpack_emitted {
        threaded_locals
            .iter()
            .map(|local| {
                let source = VersionedVar::new(VersionPrefix::Local(local.clone()), 0, frame);
                let target = VersionedVar::new(VersionPrefix::Local(local.clone()), 1, frame);
                ThreadedStmt::Bind {
                    target,
                    source,
                    op: BindOp::Unpack {
                        field: local.clone(),
                    },
                    shadow_write: false,
                    span,
                }
            })
            .collect()
    } else {
        Vec::new()
    };
    let produces = body
        .iter()
        .filter_map(|stmt| match stmt {
            ThreadedStmt::Bind { target, .. } => Some(target.clone()),
            ThreadedStmt::Threaded { .. }
            | ThreadedStmt::NlrCatch { .. }
            | ThreadedStmt::Return(..)
            | ThreadedStmt::TupleAccUnpack { .. } => None,
        })
        .collect();
    verify(&[ThreadedStmt::Threaded {
        mode,
        frame,
        body,
        produces,
        span,
    }])
}

// ─── BT-3133 (ADR 0111 Phase C) invariant checks ───────────────────────────

/// Invariant classes 1 + 4 ("flat positional-unpack accumulator" and
/// "early-exit accumulator liveness"): builds a minimal fixture for one
/// `TupleAcc`-mode fold lambda's per-iteration positional unpack step and
/// verifies it. Single-sourced at `generate_tuple_unpack_docs`
/// (`control_flow/mod.rs`) — the sole production emitter of this unpack shape
/// across every list-op and dict-op call site (`basic_ops.rs`, `filter_ops.rs`,
/// `search_ops.rs`, `transform_ops.rs`, `dict_ops.rs`), so this one check
/// covers all of them.
///
/// `use_tuple_acc` is the loop's already-resolved `ThreadingPlan::use_tuple_acc`
/// decision; when `false`, this call site should never have been reached in
/// `TupleAcc` mode at all — `fallback_reason` names why (mirrors
/// `verify_loop_unpack_invariant`'s `mode` parameter, which is likewise the
/// generator's own already-resolved decision, not a re-derivation).
/// `gate_slots` is `index_offset - 1`, the caller's own already-computed
/// tuple-shape parameter (`1` for `do:`/`dict do:`'s `index_offset: 1`, `2`
/// for `collect:`/boolean-predicate ops' `index_offset: 2`, `3` for
/// `takeWhile:`/`dropWhile:`/`detect:`-family's `index_offset: 3`).
pub(super) fn verify_tuple_acc_unpack_invariant(
    use_tuple_acc: bool,
    fallback_reason: StateAccFallbackReason,
    threaded_locals: &[String],
    gate_slots: usize,
    span: Span,
) -> Vec<VerifyError> {
    let frame = FrameId::new(1);
    let param = AccParam::new("StateAcc");
    let targets: Vec<VersionedVar> = threaded_locals
        .iter()
        .enumerate()
        .map(|(i, local)| VersionedVar::new(VersionPrefix::Local(local.clone()), i + 1, frame))
        .collect();
    let mode = if use_tuple_acc {
        ThreadingMode::TupleAcc(gate_slots)
    } else {
        ThreadingMode::StateAcc(fallback_reason)
    };
    verify(&[ThreadedStmt::Threaded {
        mode,
        frame,
        body: vec![ThreadedStmt::TupleAccUnpack {
            param,
            gate_slots,
            targets: targets.clone(),
            frame,
            span,
        }],
        produces: targets,
        span,
    }])
}

/// Invariant class 2: `select_tuple_acc`'s `ValueType`-context exclusion.
/// Single-sourced at `ThreadingPlan::new_impl` (`control_flow/mod.rs`),
/// called once per loop right after `use_tuple_acc` is resolved, comparing it
/// against a direct re-check of the same `context` value `select_tuple_acc`
/// itself already guarded on.
pub(super) fn verify_tuple_acc_value_type_exclusion(
    use_tuple_acc: bool,
    context_is_value_type: bool,
    span: Span,
) -> Vec<VerifyError> {
    if use_tuple_acc && context_is_value_type {
        vec![VerifyError::TupleAccInValueTypeContext { at: span }]
    } else {
        Vec::new()
    }
}

/// Invariant class 3: the recursive inter-construct `StateAcc`-fallback
/// invariant `list_op_needs_stateacc_fallback_recursive` encodes.
/// Single-sourced at `ThreadingPlan::new_impl` (`control_flow/mod.rs`), called
/// once per loop right after `use_direct_params` is resolved, comparing it
/// against `BodyEffects::has_non_tuple_safe_list_op` (an independently
/// computed recursive scan of the loop body for nested list ops that need a
/// `StateAcc` fallback).
pub(super) fn verify_nested_list_op_stateacc_compat(
    direct_params_selected: bool,
    inner_needs_stateacc_fallback: bool,
    span: Span,
) -> Vec<VerifyError> {
    if direct_params_selected && inner_needs_stateacc_fallback {
        vec![VerifyError::NestedStateAccFallbackUnderDirectParams { at: span }]
    } else {
        Vec::new()
    }
}

// ─── Phase A0 measurement prototype ────────────────────────────────────────

/// The env var gating the `whileTrue:`/`whileFalse:` direct-params
/// `ThreadedIr` measurement prototype (ADR 0111 Phase A0). Mirrors the
/// `BEAMTALK_CODEGEN_DIAGNOSTICS` pattern (`mod.rs`).
const PROTOTYPE_ENV_VAR: &str = "BEAMTALK_THREADED_IR_WHILE";

/// Whether the Phase A0 measurement prototype is enabled.
pub(super) fn prototype_enabled() -> bool {
    std::env::var(PROTOTYPE_ENV_VAR).is_ok_and(|v| v == "1")
}

/// Builds a representative (not production-consumed) `ThreadedIr` fixture for
/// a direct-params `whileTrue:`/`whileFalse:` loop: one `Bind` per threaded
/// local, modeling that local's per-iteration rebind, wrapped in a single
/// `Threaded { mode: DirectParams, .. }` node. Proportional in size to the
/// real loop (one `Bind` per threaded local) so the Phase A0 measurement
/// reflects realistic construction cost.
///
/// Called only from the flagged prototype call site in
/// `control_flow/while_loops.rs`; its result is verified and rendered, then
/// discarded (see module docs §Status) — this function and its callers never
/// affect generated output.
pub(super) fn prototype_direct_params_ir(
    threaded_locals: &[String],
    span: Span,
) -> Vec<ThreadedStmt> {
    let frame = FrameId::new(1);
    let body: Vec<ThreadedStmt> = threaded_locals
        .iter()
        .map(|local| {
            let source = VersionedVar::new(VersionPrefix::Local(local.clone()), 0, frame);
            let target = VersionedVar::new(VersionPrefix::Local(local.clone()), 1, frame);
            ThreadedStmt::Bind {
                target,
                source: source.clone(),
                op: BindOp::Direct(ValueRef::Version(source)),
                shadow_write: false,
                span,
            }
        })
        .collect();
    let produces = threaded_locals
        .iter()
        .map(|local| VersionedVar::new(VersionPrefix::Local(local.clone()), 1, frame))
        .collect();
    vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::DirectParams,
        frame,
        body,
        produces,
        span,
    }]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn local(name: &str, version: usize, frame: FrameId) -> VersionedVar {
        VersionedVar::new(VersionPrefix::Local(name.to_string()), version, frame)
    }

    fn class_var(version: usize, frame: FrameId) -> VersionedVar {
        VersionedVar::new(VersionPrefix::ClassVars, version, frame)
    }

    // ── VersionedVar / FrameId basics ───────────────────────────────────

    #[test]
    fn render_name_version_zero_is_bare_prefix() {
        assert_eq!(
            VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT).render_name(),
            "State"
        );
    }

    #[test]
    fn render_name_nonzero_version_appends_number() {
        assert_eq!(
            VersionedVar::new(VersionPrefix::ClassVars, 2, FrameId::ROOT).render_name(),
            "ClassVars2"
        );
        assert_eq!(local("Sum", 1, FrameId::ROOT).render_name(), "Sum1");
    }

    // ── VersionCounter (BT-3131) ─────────────────────────────────────────
    // Pins the same semantics the pre-BT-3131 `StateThreading` struct
    // (`state_codegen.rs`) pinned, now against the single shared
    // implementation reused for all three prefixes.

    #[test]
    fn version_counter_starts_at_zero() {
        let counter = VersionCounter::new();
        assert_eq!(counter.version(), 0);
        assert_eq!(counter.current_var(VersionPrefix::State), "State");
    }

    #[test]
    fn version_counter_next_var_increments_and_persists() {
        let mut counter = VersionCounter::new();
        assert_eq!(counter.next_var(VersionPrefix::State), "State1");
        assert_eq!(counter.version(), 1);
        assert_eq!(counter.current_var(VersionPrefix::State), "State1");
        assert_eq!(counter.next_var(VersionPrefix::State), "State2");
        assert_eq!(counter.version(), 2);
    }

    #[test]
    fn version_counter_peek_next_var_does_not_advance() {
        let counter = VersionCounter::new();
        assert_eq!(
            counter.peek_next_var(VersionPrefix::ClassVars),
            "ClassVars1"
        );
        // peek must not have minted — the counter is still at version 0.
        assert_eq!(counter.version(), 0);
        assert_eq!(counter.current_var(VersionPrefix::ClassVars), "ClassVars");
    }

    #[test]
    fn version_counter_reset_returns_to_zero() {
        let mut counter = VersionCounter::new();
        counter.next_var(VersionPrefix::SelfVt);
        counter.next_var(VersionPrefix::SelfVt);
        assert_eq!(counter.version(), 2);
        counter.reset();
        assert_eq!(counter.version(), 0);
        assert_eq!(counter.current_var(VersionPrefix::SelfVt), "Self");
    }

    #[test]
    fn version_counter_set_version_overwrites_directly() {
        let mut counter = VersionCounter::new();
        counter.set_version(5);
        assert_eq!(counter.version(), 5);
        assert_eq!(counter.current_var(VersionPrefix::State), "State5");
    }

    #[test]
    fn version_counter_is_reused_identically_across_prefixes() {
        // Same counter value, three different prefixes — pins that naming is
        // purely a function of (prefix, version), never counter identity.
        let mut counter = VersionCounter::new();
        counter.set_version(3);
        assert_eq!(counter.current_var(VersionPrefix::State), "State3");
        assert_eq!(counter.current_var(VersionPrefix::ClassVars), "ClassVars3");
        assert_eq!(counter.current_var(VersionPrefix::SelfVt), "Self3");
    }

    // ── verify(): the clean/silent case ─────────────────────────────────

    #[test]
    fn verify_silent_on_well_formed_direct_params_fixture() {
        let ir = prototype_direct_params_ir(&["sum".to_string(), "count".to_string()], span());
        assert_eq!(verify(&ir), Vec::new());
    }

    // ── UnboundVersion ───────────────────────────────────────────────────

    #[test]
    fn verify_unbound_version_when_source_has_no_producer() {
        let f0 = FrameId::ROOT;
        // ClassVars2 is used as a Bind source but nothing ever produced it.
        let ir = vec![ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(2, f0),
            op: BindOp::Put {
                field: "runs".to_string(),
                value: ValueRef::Var("_Val0".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: false,
            span: span(),
        }];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(
                |e| matches!(e, VerifyError::UnboundVersion { var, .. } if *var == class_var(2, f0))
            ),
            "expected UnboundVersion for ClassVars2, got: {errors:?}"
        );
    }

    #[test]
    fn verify_version_zero_is_always_bound() {
        let f0 = FrameId::ROOT;
        let ir = vec![ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0), // frame-entry parameter — never a Bind target
            op: BindOp::Put {
                field: "runs".to_string(),
                value: ValueRef::Var("_Val0".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: true,
            span: span(),
        }];
        assert_eq!(verify(&ir), Vec::new());
    }

    #[test]
    fn verify_unbound_version_when_frame_not_on_stack() {
        // References a version from a frame that is never entered (not
        // ROOT, not any nested Threaded's frame).
        let stray_frame = FrameId::new(99);
        let ir = vec![ThreadedStmt::Return(
            ValueRef::Literal("'nil'"),
            class_var(1, stray_frame),
            span(),
        )];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::UnboundVersion { var, .. } if var.frame == stray_frame
            )),
            "expected UnboundVersion for the out-of-scope frame, got: {errors:?}"
        );
    }

    #[test]
    fn verify_frame_flow_grandparent_binding_visible_in_grandchild_frame() {
        // The explicit frame-flow rule: a version produced in an ancestor
        // frame is visible to a doubly-nested descendant frame, not just an
        // immediate child. F0 (root) binds ClassVars1; F2 (nested two
        // Threaded levels deep, inside F1) uses it as a Bind source.
        let f0 = FrameId::ROOT;
        let f1 = FrameId::new(1);
        let f2 = FrameId::new(2);
        let ir = vec![
            ThreadedStmt::Bind {
                target: class_var(1, f0),
                source: class_var(0, f0),
                op: BindOp::Direct(ValueRef::Literal("'a'")),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Threaded {
                mode: ThreadingMode::DirectParams,
                frame: f1,
                body: vec![ThreadedStmt::Threaded {
                    mode: ThreadingMode::DirectParams,
                    frame: f2,
                    body: vec![ThreadedStmt::Bind {
                        target: class_var(2, f2),
                        source: class_var(1, f0), // grandparent's binding
                        op: BindOp::Direct(ValueRef::Literal("'b'")),
                        shadow_write: false,
                        span: span(),
                    }],
                    produces: vec![class_var(2, f2)],
                    span: span(),
                }],
                produces: vec![],
                span: span(),
            },
        ];
        assert_eq!(verify(&ir), Vec::new());
    }

    #[test]
    fn verify_frame_flow_sibling_frame_binding_not_visible() {
        // Two sibling Threaded frames (F1, F2) both nested directly under
        // root. A version produced inside F1 is NOT visible inside F2 — the
        // frame stack only contains ancestors, never siblings.
        let f1 = FrameId::new(1);
        let f2 = FrameId::new(2);
        let ir = vec![
            ThreadedStmt::Threaded {
                mode: ThreadingMode::DirectParams,
                frame: f1,
                body: vec![ThreadedStmt::Bind {
                    target: local("sum", 1, f1),
                    source: local("sum", 0, f1),
                    op: BindOp::Direct(ValueRef::Literal("'a'")),
                    shadow_write: false,
                    span: span(),
                }],
                produces: vec![local("sum", 1, f1)],
                span: span(),
            },
            ThreadedStmt::Threaded {
                mode: ThreadingMode::DirectParams,
                frame: f2,
                body: vec![ThreadedStmt::Bind {
                    target: local("count", 1, f2),
                    // References F1's Sum1 — F1 is a sibling, not an
                    // ancestor, of F2, so this must be UnboundVersion.
                    source: local("sum", 1, f1),
                    op: BindOp::Direct(ValueRef::Literal("'b'")),
                    shadow_write: false,
                    span: span(),
                }],
                produces: vec![local("count", 1, f2)],
                span: span(),
            },
        ];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::UnboundVersion { var, .. } if *var == local("sum", 1, f1)
            )),
            "expected UnboundVersion for the sibling frame's Sum1, got: {errors:?}"
        );
    }

    // ── NonLinearVersion ─────────────────────────────────────────────────

    #[test]
    fn verify_non_linear_version_when_produced_twice() {
        let f0 = FrameId::ROOT;
        // ClassVars1 is bound by two different Binds in the same frame.
        let ir = vec![
            ThreadedStmt::Bind {
                target: class_var(1, f0),
                source: class_var(0, f0),
                op: BindOp::Direct(ValueRef::Literal("'a'")),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: class_var(1, f0),
                source: class_var(0, f0),
                op: BindOp::Direct(ValueRef::Literal("'b'")),
                shadow_write: false,
                span: span(),
            },
        ];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::NonLinearVersion { var, producers: 2, .. } if *var == class_var(1, f0)
            )),
            "expected NonLinearVersion(producers=2), got: {errors:?}"
        );
    }

    #[test]
    fn verify_non_linear_version_when_consumed_twice() {
        let f0 = FrameId::ROOT;
        // ClassVars1 (produced once) is used as the `source` of two
        // successor Binds — fans out instead of chaining linearly.
        let ir = vec![
            ThreadedStmt::Bind {
                target: class_var(1, f0),
                source: class_var(0, f0),
                op: BindOp::Direct(ValueRef::Literal("'a'")),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: class_var(2, f0),
                source: class_var(1, f0),
                op: BindOp::Direct(ValueRef::Literal("'b'")),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: class_var(3, f0),
                source: class_var(1, f0),
                op: BindOp::Direct(ValueRef::Literal("'c'")),
                shadow_write: false,
                span: span(),
            },
        ];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::NonLinearVersion { var, consumers: 2, .. } if *var == class_var(1, f0)
            )),
            "expected NonLinearVersion(consumers=2), got: {errors:?}"
        );
    }

    #[test]
    fn verify_non_linear_version_findings_are_deterministically_ordered() {
        // Two independent NonLinearVersion violations (Count1 and Sum1, each
        // produced twice) in the same frame. HashMap iteration order is
        // per-process-random, so this pins verify()'s sort — run repeatedly
        // (`cargo test -- --test-threads=1` doesn't reseed per test, but a
        // fresh process would) this must always come back in the same order.
        let f0 = FrameId::ROOT;
        let ir = vec![
            ThreadedStmt::Bind {
                target: local("sum", 1, f0),
                source: local("sum", 0, f0),
                op: BindOp::Direct(ValueRef::Literal("'a'")),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: local("sum", 1, f0),
                source: local("sum", 0, f0),
                op: BindOp::Direct(ValueRef::Literal("'b'")),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: local("count", 1, f0),
                source: local("count", 0, f0),
                op: BindOp::Direct(ValueRef::Literal("'c'")),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: local("count", 1, f0),
                source: local("count", 0, f0),
                op: BindOp::Direct(ValueRef::Literal("'d'")),
                shadow_write: false,
                span: span(),
            },
        ];
        let errors = verify(&ir);
        assert_eq!(
            errors,
            vec![
                VerifyError::NonLinearVersion {
                    var: local("count", 1, f0),
                    producers: 2,
                    consumers: 0,
                },
                VerifyError::NonLinearVersion {
                    var: local("sum", 1, f0),
                    producers: 2,
                    consumers: 0,
                },
            ],
            "expected Count1 before Sum1 (sorted VersionedVar order), got: {errors:?}"
        );
    }

    // ── ThreadingModeUnpackMismatch ──────────────────────────────────────

    #[test]
    fn verify_unpack_mismatch_inside_direct_params_mode() {
        let frame = FrameId::new(1);
        let ir = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::DirectParams,
            frame,
            body: vec![ThreadedStmt::Bind {
                target: local("sum", 1, frame),
                source: local("sum", 0, frame),
                op: BindOp::Unpack {
                    field: "__local__sum".to_string(),
                },
                shadow_write: false,
                span: span(),
            }],
            produces: vec![local("sum", 1, frame)],
            span: span(),
        }];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::ThreadingModeUnpackMismatch {
                    mode: ThreadingMode::DirectParams,
                    ..
                }
            )),
            "expected ThreadingModeUnpackMismatch, got: {errors:?}"
        );
    }

    #[test]
    fn verify_unpack_silent_inside_stateacc_mode() {
        let frame = FrameId::new(1);
        let ir = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::SelfSendInBody),
            frame,
            body: vec![ThreadedStmt::Bind {
                target: local("sum", 1, frame),
                source: local("sum", 0, frame),
                op: BindOp::Unpack {
                    field: "__local__sum".to_string(),
                },
                shadow_write: false,
                span: span(),
            }],
            produces: vec![local("sum", 1, frame)],
            span: span(),
        }];
        assert_eq!(verify(&ir), Vec::new());
    }

    // ── ShadowWriteMissing (ADR 0110 contract) ───────────────────────────

    #[test]
    fn verify_shadow_write_missing_fires_on_top_frame_mutation_without_shadow() {
        let f0 = FrameId::ROOT;
        let ir = vec![
            ThreadedStmt::Bind {
                target: class_var(1, f0),
                source: class_var(0, f0),
                op: BindOp::Put {
                    field: "runs".to_string(),
                    value: ValueRef::Var("_Val0".to_string()),
                    class_tag: ValueRef::Var("ClassSelf".to_string()),
                },
                shadow_write: false, // BUG: forgot the shadow write
                span: span(),
            },
            ThreadedStmt::NlrCatch {
                boundary: NlrBoundary::ClassMethod {
                    has_class_vars: true,
                },
                token: TokenId::new(0),
                frame: f0,
                span: span(),
            },
        ];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::ShadowWriteMissing { mutated, .. } if *mutated == class_var(1, f0)
            )),
            "expected ShadowWriteMissing, got: {errors:?}"
        );
    }

    #[test]
    fn verify_shadow_write_silent_on_fixed_shape() {
        // ADR 0110's actual post-fix shape: shadow_write: true.
        let f0 = FrameId::ROOT;
        let ir = vec![
            ThreadedStmt::Bind {
                target: class_var(1, f0),
                source: class_var(0, f0),
                op: BindOp::Put {
                    field: "runs".to_string(),
                    value: ValueRef::Var("_Val0".to_string()),
                    class_tag: ValueRef::Var("ClassSelf".to_string()),
                },
                shadow_write: true,
                span: span(),
            },
            ThreadedStmt::NlrCatch {
                boundary: NlrBoundary::ClassMethod {
                    has_class_vars: true,
                },
                token: TokenId::new(0),
                frame: f0,
                span: span(),
            },
        ];
        assert_eq!(verify(&ir), Vec::new());
    }

    #[test]
    fn verify_shadow_write_silent_without_class_var_nlr_relay() {
        // No NlrCatch with has_class_vars: true present at all — the
        // ShadowWriteMissing precondition doesn't hold, so a missing
        // shadow write is not (yet) flagged.
        let f0 = FrameId::ROOT;
        let ir = vec![ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Put {
                field: "runs".to_string(),
                value: ValueRef::Var("_Val0".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: false,
            span: span(),
        }];
        assert_eq!(verify(&ir), Vec::new());
    }

    #[test]
    fn verify_shadow_write_missing_silent_below_top_frame() {
        // Same missing-shadow-write shape, but the Bind is inside a nested
        // frame (block_depth > 0 analogue) — not a top-frame mutation, so
        // the ADR 0110 contract doesn't apply here (matches
        // `generate_field_assignment`'s `block_depth == 0` gate).
        let f0 = FrameId::ROOT;
        let inner = FrameId::new(1);
        let ir = vec![
            ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame: inner,
                body: vec![ThreadedStmt::Bind {
                    target: VersionedVar::new(VersionPrefix::ClassVars, 1, inner),
                    source: VersionedVar::new(VersionPrefix::ClassVars, 0, inner),
                    op: BindOp::Put {
                        field: "runs".to_string(),
                        value: ValueRef::Var("_Val0".to_string()),
                        class_tag: ValueRef::Var("ClassSelf".to_string()),
                    },
                    shadow_write: false,
                    span: span(),
                }],
                produces: vec![VersionedVar::new(VersionPrefix::ClassVars, 1, inner)],
                span: span(),
            },
            ThreadedStmt::NlrCatch {
                boundary: NlrBoundary::ClassMethod {
                    has_class_vars: true,
                },
                token: TokenId::new(0),
                frame: f0,
                span: span(),
            },
        ];
        assert_eq!(verify(&ir), Vec::new());
    }

    // ── lower_and_render (test shim) ─────────────────────────────────────

    #[test]
    fn lower_and_render_put_renders_maps_put_chain() {
        let f0 = FrameId::ROOT;
        let ir = vec![ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Put {
                field: "runs".to_string(),
                value: ValueRef::Var("_Val0".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: false,
            span: span(),
        }];
        let rendered = lower_and_render(&ir).to_pretty_string();
        assert!(
            rendered.contains("let ClassVars1 = call 'maps':'put'('runs', _Val0, ClassVars) in"),
            "got: {rendered}"
        );
    }

    #[test]
    fn lower_and_render_put_with_shadow_write_appends_erlang_put() {
        let f0 = FrameId::ROOT;
        let ir = vec![ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Put {
                field: "runs".to_string(),
                value: ValueRef::Var("_Val0".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: true,
            span: span(),
        }];
        let rendered = lower_and_render(&ir).to_pretty_string();
        assert!(
            rendered.contains(
                "call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1) in"
            ),
            "got: {rendered}"
        );
    }

    #[test]
    fn lower_and_render_unpack_renders_maps_get() {
        let frame = FrameId::new(1);
        let ir = vec![ThreadedStmt::Bind {
            target: local("sum", 1, frame),
            source: local("sum", 0, frame),
            op: BindOp::Unpack {
                field: "__local__sum".to_string(),
            },
            shadow_write: false,
            span: span(),
        }];
        let rendered = lower_and_render(&ir).to_pretty_string();
        assert_eq!(
            rendered,
            "let Sum1 = call 'maps':'get'('__local__sum', Sum) in "
        );
    }

    #[test]
    fn lower_and_render_direct_renders_plain_let() {
        let f0 = FrameId::ROOT;
        let ir = vec![ThreadedStmt::Bind {
            target: local("sum", 1, f0),
            source: local("sum", 0, f0),
            op: BindOp::Direct(ValueRef::Var("Sum0".to_string())),
            shadow_write: false,
            span: span(),
        }];
        assert_eq!(
            lower_and_render(&ir).to_pretty_string(),
            "let Sum1 = Sum0 in "
        );
    }

    #[test]
    fn lower_and_render_return_renders_tuple() {
        let ir = vec![ThreadedStmt::Return(
            ValueRef::Literal("'nil'"),
            VersionedVar::new(VersionPrefix::State, 2, FrameId::ROOT),
            span(),
        )];
        assert_eq!(lower_and_render(&ir).to_pretty_string(), "{'nil', State2}");
    }

    #[test]
    fn lower_and_render_threaded_flattens_body() {
        let frame = FrameId::new(1);
        let ir = prototype_direct_params_ir(&["sum".to_string()], span());
        let rendered = lower_and_render(&ir).to_pretty_string();
        assert_eq!(rendered, "let Sum1 = Sum in ");
        let _ = frame; // used only to document scope of the fixture above
    }

    // ── Phase A0 prototype plumbing ──────────────────────────────────────

    #[test]
    fn prototype_direct_params_ir_produces_one_bind_per_local() {
        let ir = prototype_direct_params_ir(&["sum".to_string(), "count".to_string()], span());
        let ThreadedStmt::Threaded {
            mode,
            body,
            produces,
            ..
        } = &ir[0]
        else {
            panic!("expected a single Threaded node, got: {ir:?}");
        };
        assert_eq!(*mode, ThreadingMode::DirectParams);
        assert_eq!(body.len(), 2);
        assert_eq!(produces.len(), 2);
    }

    #[test]
    #[serial_test::serial(beamtalk_threaded_ir_env)]
    fn prototype_enabled_reads_env_var() {
        // SAFETY: serialised via #[serial]; the var is removed before this
        // assertion so no other test's process-wide env state leaks in.
        unsafe {
            std::env::remove_var(PROTOTYPE_ENV_VAR);
        }
        assert!(!prototype_enabled());
        // SAFETY: serialised via #[serial]; sets the var this test asserts on.
        unsafe {
            std::env::set_var(PROTOTYPE_ENV_VAR, "1");
        }
        assert!(prototype_enabled());
        // SAFETY: serialised via #[serial]; restores the unset state.
        unsafe {
            std::env::remove_var(PROTOTYPE_ENV_VAR);
        }
    }

    // ── verify_loop_unpack_invariant (BT-3132) ───────────────────────────
    // Pins the production replacement for the four deleted
    // "unpack should emit no code" `debug_assert!`s in `while_loops.rs` /
    // `control_flow/mod.rs` (counted loops).

    #[test]
    fn verify_loop_unpack_invariant_silent_when_no_unpack_emitted_direct_params() {
        // The expected, common case: direct-params mode with no unpack docs.
        let errors = verify_loop_unpack_invariant(
            ThreadingMode::DirectParams,
            &["sum".to_string(), "count".to_string()],
            false,
            span(),
        );
        assert_eq!(errors, Vec::new());
    }

    #[test]
    fn verify_loop_unpack_invariant_silent_when_no_unpack_emitted_hybrid() {
        let errors =
            verify_loop_unpack_invariant(ThreadingMode::Hybrid, &["n".to_string()], false, span());
        assert_eq!(errors, Vec::new());
    }

    #[test]
    fn verify_loop_unpack_invariant_fires_when_unpack_emitted_under_direct_params() {
        // Simulates the regression the deleted debug_assert! guarded against:
        // `generate_unpack_at_iteration_start` emitted unpack code even though
        // the loop resolved to DirectParams mode. Asserts the exact error set
        // (not just "contains a match") — pins that a single mismatched local
        // produces exactly one finding, with no spurious UnboundVersion/
        // NonLinearVersion noise from how the fixture is built.
        let errors = verify_loop_unpack_invariant(
            ThreadingMode::DirectParams,
            &["sum".to_string()],
            true,
            span(),
        );
        assert_eq!(
            errors.len(),
            1,
            "expected exactly one error, got: {errors:?}"
        );
        assert!(
            matches!(
                &errors[0],
                VerifyError::ThreadingModeUnpackMismatch {
                    mode: ThreadingMode::DirectParams,
                    ..
                }
            ),
            "expected ThreadingModeUnpackMismatch, got: {errors:?}"
        );
    }

    #[test]
    fn verify_loop_unpack_invariant_fires_when_unpack_emitted_under_hybrid() {
        // Two threaded locals: pins that each gets its own
        // ThreadingModeUnpackMismatch finding, with no cross-local
        // interference (e.g. NonLinearVersion from misattributed versions).
        let errors = verify_loop_unpack_invariant(
            ThreadingMode::Hybrid,
            &["n".to_string(), "sum".to_string()],
            true,
            span(),
        );
        assert_eq!(
            errors.len(),
            2,
            "expected exactly two errors, got: {errors:?}"
        );
        assert!(
            errors.iter().all(|e| matches!(
                e,
                VerifyError::ThreadingModeUnpackMismatch {
                    mode: ThreadingMode::Hybrid,
                    ..
                }
            )),
            "expected only ThreadingModeUnpackMismatch findings, got: {errors:?}"
        );
    }

    #[test]
    fn verify_loop_unpack_invariant_silent_under_stateacc_with_unpack() {
        // Sanity check: StateAcc mode legitimately emits unpack code, so this
        // must stay silent — mirrors `verify_unpack_silent_inside_stateacc_mode`.
        let errors = verify_loop_unpack_invariant(
            ThreadingMode::StateAcc(StateAccFallbackReason::SelfSendInBody),
            &["sum".to_string()],
            true,
            span(),
        );
        assert_eq!(errors, Vec::new());
    }

    // ── BT-3133 (ADR 0111 Phase C): invariant class 1 — flat
    // positional-unpack accumulator ──────────────────────────────────────

    #[test]
    fn verify_tuple_acc_unpack_invariant_silent_under_tuple_acc_mode() {
        // The expected, common case: TupleAcc mode with a matching gate_slots
        // unpack — this is what every real `generate_tuple_unpack_docs` call
        // site produces (`use_tuple_acc: true` implies `TupleAcc(gate_slots)`).
        let errors = verify_tuple_acc_unpack_invariant(
            true,
            StateAccFallbackReason::None,
            &["sum".to_string(), "count".to_string()],
            0, // do:'s index_offset(1) - 1
            span(),
        );
        assert_eq!(errors, Vec::new());
    }

    #[test]
    fn verify_tuple_acc_unpack_invariant_fires_when_use_tuple_acc_is_false() {
        // Simulates the regression class this check guards against: a
        // TupleAccUnpack node reached with `use_tuple_acc: false` — i.e.
        // `generate_tuple_unpack_docs` called from a StateAcc-fallback path.
        let errors = verify_tuple_acc_unpack_invariant(
            false,
            StateAccFallbackReason::SelfSendInBody,
            &["sum".to_string()],
            0,
            span(),
        );
        assert_eq!(
            errors.len(),
            1,
            "expected exactly one error, got: {errors:?}"
        );
        assert!(
            matches!(
                &errors[0],
                VerifyError::TupleAccUnpackModeMismatch {
                    mode: ThreadingMode::StateAcc(StateAccFallbackReason::SelfSendInBody),
                    ..
                }
            ),
            "expected TupleAccUnpackModeMismatch, got: {errors:?}"
        );
    }

    #[test]
    fn verify_tuple_acc_unpack_mode_mismatch_fires_outside_any_tuple_acc_context() {
        // A TupleAccUnpack node hand-placed inside a DirectParams body (never
        // legitimate — DirectParams has no accumulator tuple at all).
        let frame = FrameId::new(1);
        let param = AccParam::new("StateAcc");
        let target = local("sum", 1, frame);
        let ir = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::DirectParams,
            frame,
            body: vec![ThreadedStmt::TupleAccUnpack {
                param,
                gate_slots: 0,
                targets: vec![target.clone()],
                frame,
                span: span(),
            }],
            produces: vec![target],
            span: span(),
        }];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::TupleAccUnpackModeMismatch {
                    mode: ThreadingMode::DirectParams,
                    ..
                }
            )),
            "expected TupleAccUnpackModeMismatch, got: {errors:?}"
        );
    }

    // ── BT-3133: invariant class 4 — early-exit accumulator liveness ─────

    #[test]
    fn verify_tuple_acc_unpack_invariant_silent_with_gate_slots_for_early_exit_op() {
        // detect:/takeWhile:/dropWhile:'s shape: 2 leading gate slots
        // (index_offset: 3) before the threaded locals.
        let errors = verify_tuple_acc_unpack_invariant(
            true,
            StateAccFallbackReason::None,
            &["item_var".to_string()],
            2,
            span(),
        );
        assert_eq!(errors, Vec::new());
    }

    #[test]
    fn verify_early_exit_gate_slot_mismatch_fires_when_node_disagrees_with_mode() {
        // Hand-built fixture: the enclosing TupleAcc mode resolved 1 gate slot
        // (collect:/select:-shaped), but the unpack node itself was built with
        // 2 (detect:-shaped) — e.g. a future refactor that copies one op's
        // codegen onto another without updating its index_offset. Silently
        // wrong values, not a core_lint failure — the exact danger class
        // EarlyExitGateSlotMismatch exists to catch.
        let frame = FrameId::new(1);
        let param = AccParam::new("AccSt0");
        let target = local("item_var", 1, frame);
        let ir = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::TupleAcc(1),
            frame,
            body: vec![ThreadedStmt::TupleAccUnpack {
                param,
                gate_slots: 2,
                targets: vec![target.clone()],
                frame,
                span: span(),
            }],
            produces: vec![target],
            span: span(),
        }];
        let errors = verify(&ir);
        assert_eq!(
            errors,
            vec![VerifyError::EarlyExitGateSlotMismatch {
                mode_gate_slots: 1,
                node_gate_slots: 2,
                at: span(),
            }]
        );
    }

    #[test]
    fn verify_tuple_acc_unpack_invariant_distinguishes_gate_slot_shapes() {
        // do: (0 gates), collect:/select:-family (1 gate), and
        // detect:/takeWhile:-family (2 gates) are all independently silent
        // when the node's gate_slots matches its enclosing mode — pins that
        // the check is genuinely parameterized per op-family shape, not
        // hardcoded to one.
        for gate_slots in [0usize, 1, 2] {
            let errors = verify_tuple_acc_unpack_invariant(
                true,
                StateAccFallbackReason::None,
                &["v".to_string()],
                gate_slots,
                span(),
            );
            assert_eq!(
                errors,
                Vec::new(),
                "gate_slots={gate_slots} should be silent, got: {errors:?}"
            );
        }
    }

    // ── BT-3133: invariant class 2 — select_tuple_acc's ValueType-context
    // exclusion ────────────────────────────────────────────────────────────

    #[test]
    fn verify_tuple_acc_value_type_exclusion_silent_when_tuple_acc_not_selected() {
        assert_eq!(
            verify_tuple_acc_value_type_exclusion(false, true, span()),
            Vec::new()
        );
    }

    #[test]
    fn verify_tuple_acc_value_type_exclusion_silent_in_actor_context() {
        assert_eq!(
            verify_tuple_acc_value_type_exclusion(true, false, span()),
            Vec::new()
        );
    }

    #[test]
    fn verify_tuple_acc_value_type_exclusion_fires_when_both_true() {
        // Simulates the regression `select_tuple_acc`'s own ValueType guard
        // prevents today: `use_tuple_acc: true` selected in a `ValueType`
        // context, which would reference an unbound `State`.
        let errors = verify_tuple_acc_value_type_exclusion(true, true, span());
        assert_eq!(
            errors,
            vec![VerifyError::TupleAccInValueTypeContext { at: span() }]
        );
    }

    // ── BT-3133: invariant class 3 — recursive inter-construct StateAcc
    // fallback ─────────────────────────────────────────────────────────────

    #[test]
    fn verify_nested_list_op_stateacc_compat_silent_when_direct_params_not_selected() {
        assert_eq!(
            verify_nested_list_op_stateacc_compat(false, true, span()),
            Vec::new()
        );
    }

    #[test]
    fn verify_nested_list_op_stateacc_compat_silent_when_no_nested_fallback() {
        assert_eq!(
            verify_nested_list_op_stateacc_compat(true, false, span()),
            Vec::new()
        );
    }

    #[test]
    fn verify_nested_list_op_stateacc_compat_fires_when_both_true() {
        // Simulates the regression `select_direct_params`'s own
        // `!effects.has_non_tuple_safe_list_op` guard prevents today: a
        // DirectParams loop containing a nested list op whose inner block
        // needs a StateAcc fallback — DirectParams has no StateAcc map for
        // that inner `{value, StateAcc}` result to unpack into.
        let errors = verify_nested_list_op_stateacc_compat(true, true, span());
        assert_eq!(
            errors,
            vec![VerifyError::NestedStateAccFallbackUnderDirectParams { at: span() }]
        );
    }

    // ── BT-3133: lower_and_render (test shim) for TupleAccUnpack ─────────

    #[test]
    fn lower_and_render_tuple_acc_unpack_renders_element_chain() {
        let frame = FrameId::new(1);
        let ir = vec![ThreadedStmt::TupleAccUnpack {
            param: AccParam::new("StateAcc"),
            gate_slots: 0,
            targets: vec![local("sum", 1, frame), local("count", 1, frame)],
            frame,
            span: span(),
        }];
        let rendered = lower_and_render(&ir).to_pretty_string();
        assert_eq!(
            rendered,
            "let Sum1 = call 'erlang':'element'(1, StateAcc) in \
             let Count1 = call 'erlang':'element'(2, StateAcc) in "
        );
    }

    #[test]
    fn lower_and_render_tuple_acc_unpack_respects_gate_slots_offset() {
        // detect:-shaped: 2 gate slots, so the first threaded local starts
        // at tuple position 3, not 1.
        let frame = FrameId::new(1);
        let ir = vec![ThreadedStmt::TupleAccUnpack {
            param: AccParam::new("AccSt0"),
            gate_slots: 2,
            targets: vec![local("n", 1, frame)],
            frame,
            span: span(),
        }];
        let rendered = lower_and_render(&ir).to_pretty_string();
        assert_eq!(rendered, "let N1 = call 'erlang':'element'(3, AccSt0) in ");
    }
}
