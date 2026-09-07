// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadedIr` — a small, narrowly-scoped mid-level IR for the
//! state-threading / control-flow subset of Core Erlang codegen (ADR 0111).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A migrated construct's mutation sequence lowers to a `Vec<ThreadedStmt>`,
//! checked once by [`verify`], then turned into the `Document` codegen
//! emits by [`render`] — for everything this module covers, the rendered
//! `Document` IS the emission; there is no separate hand-built path beside
//! it. This replaces what used to be scattered `debug_assert!`s at each
//! emission site, each re-deriving the same state-threading invariants by
//! hand.
//!
//! ## Coverage
//!
//! Lowered through this IR: conditional branch arms (`ifTrue:`/`ifFalse:`/
//! `ifTrue:ifFalse:`/`ifNotNil:`/`match:`), `on:do:`/`ensure:` arms, Actor
//! and class-method bodies (including NLR relay via `NlrCatch`), Tier 2
//! stateful-block bodies, the list-op/dict-op per-iteration
//! tuple-accumulator unpack ([`build_tuple_acc_unpack`]), and
//! expression-position state effects via [`ThreadedValue`] preludes (ADR
//! 0118).
//!
//! **Not yet lowered:** the loop skeleton and body-statement sequence for
//! while/counted loops, and the fold accumulator for list-op/dict-op bodies
//! (`control_flow/mod.rs`'s `generate_threaded_loop_body_inner`) — both
//! stay on the pre-ADR-0111 AST-directed path. `ConditionalLoop`,
//! `ThreadingMode::DirectParams`, and `VersionPrefix::Local` are the shapes
//! that migration will construct; they carry `#[allow(dead_code)]` until it
//! lands. See ADR 0111 Addendum 15 for the design.
//!
//! ## Layout
//!
//! - Identity/value types: [`FrameId`], [`VersionPrefix`], [`VersionedVar`],
//!   [`AccParam`], [`LoopCounter`], [`ValueRef`].
//! - Statements: [`ThreadedStmt`], [`BindOp`], [`ThreadingMode`],
//!   [`ThreadedValue`].
//! - Checker: [`verify`], [`VerifyError`] — see `docs/development/debugging.md`
//!   § `ThreadedIr` verifier for the variant-by-variant reference.
//! - Emitter: [`render`], [`RenderCtx`].
//! - Builders: [`build_tuple_acc_unpack`], [`construct_and_verify_class_var_bind`],
//!   [`verify_body_with_opaque_version_gaps`], [`backfill_opaque_version_gap`].
//!
//! ## Invariants
//!
//! - A statement that advances a threaded version is a `Bind`; `Statement`
//!   and `ValueRef::Doc` are opaque and carry no threading of their own.
//! - Versions are linear within one [`FrameId`]; sibling branch/handler arms
//!   get distinct frames so independently-minted versions never collide.
//! - A class-var `Bind` at a shadow-write-eligible point, in a method whose
//!   body can relay a foreign NLR, must set `shadow_write` (the ADR 0110
//!   contract).
//! - A body with version steps hidden inside a shared multi-module helper is
//!   verified after [`backfill_opaque_version_gap`] closes those gaps.
//!
//! ## Scope
//!
//! Covers state-version bindings (with frame identity), threading-mode
//! selection, shadow-write emission, and NLR relay boundaries. Everything
//! else in codegen stays AST-directed and unaffected — see ADR 0111
//! §Decision / §Constraints for the full narrow-scope rationale.
//!
//! ## Deviations from ADR 0111's illustrative IR
//!
//! - `Span` fields on `ThreadedStmt`'s `Bind`, `Threaded`, `NlrCatch`, and
//!   `Return` variants, so [`VerifyError`] can carry a source-attributed
//!   location.
//! - [`VersionPrefix`]'s `Local` variant, a fourth prefix beyond the ADR's
//!   `State | ClassVars | SelfVt` sketch, for named loop locals that never
//!   go through any of those three counters.
//! - [`ValueRef`]'s `Doc` variant and [`ThreadedStmt`]'s `Statement`
//!   variant, the opaque AST-directed escape hatches Addenda 3 and 4 added
//!   once the migration found real value and statement positions this IR
//!   does not need to understand.
//!
//! Migration history — what shipped when, what was tried and rejected —
//! lives in ADR 0111's addenda, not here.

use std::collections::HashMap;

use super::control_flow::StateAccFallbackReason;
use super::{CoreErlangGenerator, NlrBoundary};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};
use beamtalk_core::source_analysis::Span;

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
/// naming/identity unification only, not IR construction). `Local` gets its
/// own production call site as of BT-3133's
/// [`verify_tuple_acc_unpack_invariant`]. `#[allow(dead_code)]` here
/// documents that the remaining variants stay test-only for now, instead of
/// forcing artificial non-test construction sites.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) enum VersionPrefix {
    /// Actor/instance state (`State`, `State1`, … — rendered as `StateAcc{N}`
    /// inside non-hybrid loop bodies, `with_branch_context` conditional
    /// branches (BT-3134), and `on:do:`/`ensure:` bodies; that rendering
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
    ///
    /// BT-3182 (ADR 0111 Addendum 13): its one production constructor
    /// (`while_loops.rs`'s deleted `try_render_while_direct_via_threaded_ir`
    /// pilot) is gone, so this is currently unconstructed — kept, not
    /// deleted, because it is the pre-existing representation
    /// [`ThreadedStmt::ConditionalLoop`]'s own doc comments already name a
    /// second, not-yet-attempted consumer for (a real counted-loop
    /// migration); see that variant's doc comment for the same reasoning.
    #[allow(dead_code)]
    Local(String),
    /// A pre-minted, verbatim Core Erlang name (e.g. `_Sum7`), ADR 0111
    /// Addendum 2 "Gap 2" (`docs/ADR/0111-...md` §Addendum 2, "naming-scheme
    /// mismatch"): real per-iteration loop-local rebinds go through
    /// `fresh_temp_var` (a single module-wide gensym counter shared across
    /// ALL codegen in the method), never through [`VersionPrefix::Local`]'s
    /// bare-prefix-plus-sequential-version scheme (`Sum1`, `Sum2`, …) —
    /// confirmed against real compiled output, not just source reading (the
    /// investigation trail is on the BT-3145 Linear issue). `render_name`
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
            VersionPrefix::Gensym(name) => name.clone(),
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

// ─── LoopCounter (ADR 0111 Addendum 2, Gap 1) ──────────────────────────────

/// A counted loop's (`to:do:`/`to:by:do:`/`timesRepeat:`/`repeat`) gensym'd
/// loop *index* fun parameter (`frame.counter`, `fresh_temp_var("loopidx")`
/// in production, `control_flow/mod.rs`'s `CountedLoopFrame::counter`) — an
/// extra [`ThreadedStmt::ConditionalLoop`] fun parameter that is never a
/// `Bind` target or source, threaded instead by a raw "next value"
/// expression (e.g. `call 'erlang':'+'(Counter, 1)`). Mirrors [`AccParam`]'s
/// existing precedent for an unversioned, generator-allocated identity kept
/// outside [`VersionedVar`]'s producer/consumer bookkeeping — see Addendum
/// 2's Gap 1 evidence (`docs/ADR/0111-...md` §Addendum 2). `None` for
/// while/`whileFalse:` loops, which have no counter; counted loops are a
/// later call site (BT-3145 wires only `generate_while_loop_direct` first).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct LoopCounter(pub(super) String);

impl LoopCounter {
    /// BT-3149: still genuinely unconstructed — counted loops
    /// (`to:do:`/`to:by:do:`/`timesRepeat:`/`repeat`) never migrated to
    /// `ThreadedIr` emission in this close-out (only `while_loops.rs`'s
    /// `DirectParams` path did, BT-3145); no other item in this file's
    /// remaining `#[allow(dead_code)]`s has an even indirect test
    /// exerciser the way the pinned-invariant `VerifyError` variants do.
    /// Left in place — not deleted — because `ConditionalLoop::counter`'s
    /// `Option<LoopCounter>` field is real production IR shape (`None`
    /// today, `Some` once counted loops migrate), and `LoopCounter` with
    /// no way to construct one would be a type that documents an
    /// unreachable state.
    #[allow(dead_code)]
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
}

// ─── Threading mode ─────────────────────────────────────────────────────────

/// A loop or mutation-carrying conditional's already-resolved threading
/// convention — the existing `ThreadingPlan`/`select_direct_params` etc.
/// decision, recorded as durable IR data instead of re-derived at emission.
/// See [`VersionPrefix`]'s doc comment for why several variants are
/// test-only for now.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum ThreadingMode {
    /// `fun (Var1, ..., VarN)` — no `StateAcc` map at all (BT-1275).
    ///
    /// BT-3182: currently unconstructed in production — see
    /// [`ThreadedStmt::ConditionalLoop`]'s doc comment.
    #[allow(dead_code)]
    DirectParams,
    /// A flat `{Gate1, ..., GateG, Var1, ..., VarN}` positional-unpack
    /// accumulator (foldl list-ops, BT-1276). The `usize` is `gate_slots` —
    /// the count of leading tuple positions reserved for the op's own result
    /// channel(s) *before* the threaded locals begin (BT-3133 invariant class
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
    /// [`VersionedVar`]. See `control_flow::ListOpKind` (BT-3147) for the
    /// canonical per-op table this classification is now independently
    /// cross-checked against.
    TupleAcc(usize),
    /// `fun (Var1, ..., VarN, RField1, ..., MField1, ...)` — locals plus
    /// pre-extracted read-only/mutated fields as direct params (BT-1326/BT-1342).
    ///
    /// BT-3149: still genuinely unconstructed in production — BT-3145
    /// wired only `DirectParams` (`generate_while_loop_direct`) to real
    /// `ThreadedIr` emission; `generate_while_loop_hybrid`/
    /// `generate_counted_stateful_loop_hybrid` remain on the pre-migration
    /// hand-rolled path. `render_threaded`'s `DirectParams | Hybrid =>`
    /// arm (this file) already renders both identically once given real
    /// IR, so a future hybrid-loop migration is a lowering-side change
    /// only. Kept `#[allow(dead_code)]` rather than deleted — the
    /// render/`LoopContextFlags` plumbing for it is real, tested
    /// production code today (see `render_threaded_tests`), only its
    /// lowering-side constructor is missing.
    #[allow(dead_code)]
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
pub(super) struct TokenId(String);

impl TokenId {
    pub(super) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    fn name(&self) -> &str {
        &self.0
    }
}

// ─── Values ─────────────────────────────────────────────────────────────────

/// A value referenced by a [`BindOp`] or [`ThreadedStmt::Return`]. See
/// [`VersionPrefix`]'s doc comment for why `Version` is test-only for now.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum ValueRef {
    /// A previously-bound versioned variable (e.g. the source of a chained
    /// mutation).
    ///
    /// BT-3149: still genuinely unconstructed in production — every real
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
    ///
    /// BT-3149: still genuinely unconstructed in production —
    /// `generate_unpack_at_iteration_start`'s `StateAcc`-mode per-iteration
    /// unpack never migrated to real `ThreadedIr` emission in this
    /// close-out (only the `TupleAcc`-mode unpack did, via
    /// [`ThreadedStmt::TupleAccUnpack`], BT-3147). `render_bind`'s arm for
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

    /// A while/counted loop's condition/case-split skeleton (ADR 0111
    /// Addendum 2, Gap 1; condition fields per ADR 0118 phase 3, BT-3419):
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
    /// BT-3182 (ADR 0111 Addendum 13): this variant's one production
    /// constructor — `while_loops.rs`'s `try_render_while_direct_via_threaded_ir`
    /// pilot, gated behind `BEAMTALK_THREADED_IR_WHILE_DIRECT` — was deleted
    /// (inconclusive measurement gate, no concrete trigger to finish full
    /// body-shape coverage; see the ADR addendum for the full decision).
    /// Kept, not deleted, because `counter`'s own doc comment already names
    /// a second, not-yet-attempted consumer this variant was designed for:
    /// a real counted-loop (`to:do:`/`timesRepeat:`/`repeat`) migration.
    /// `#[allow(dead_code)]` on the now-unconstructed `DirectParams`/`Local`
    /// variants it depends on reflects the same status, not a new one.
    #[allow(dead_code)]
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
        /// ADR 0118 phase 3 (BT-3419): the condition block's own prelude,
        /// verified in the SAME frame as `body` (unlike the pre-BT-3419
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
        /// continues looping. Split out of the pre-BT-3419 `continue_header`
        /// now that the scrutinee itself (`condition`/`condition_value`) is
        /// real IR; this remaining fragment carries no state-threading
        /// content of its own (a bare case-clause pattern), the same
        /// accepted opacity class as `exit_arm`'s own pattern half.
        continue_arm: Document<'static>,
        body: Vec<ThreadedStmt>,
        produces: Vec<VersionedVar>,
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

// ─── ThreadedValue (ADR 0118, Decision 1 / BT-3415) ────────────────────────

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
pub(super) struct ThreadedValue {
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
/// ADR 0118 phase 1a (BT-3415): constructed only by [`ThreadedValue::close`]'s
/// unit tests so far — every phase-1a consumer *splices*; the production
/// `close()` call sites arrive with the consumers that must produce a
/// self-contained `Document` (`expression_doc` in Actor context, phase 2b —
/// see that function's doc comment for why not sooner). Same status as
/// [`ValueRef::Version`]'s constructor.
///
/// BT-3430 investigated `Opaque` for exactly the class-method self-send case
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
pub(super) enum CloseContext {
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
    pub(super) fn pure(value: ValueRef) -> Self {
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
    pub(super) fn value_is_trivial(&self) -> bool {
        matches!(
            self.value,
            ValueRef::Var(_) | ValueRef::Literal(_) | ValueRef::Version(_)
        )
    }

    /// Renders the prelude as nested `let`s around the value, producing one
    /// self-contained `Document` — the ONLY way to discard a prelude (ADR
    /// 0118 §Decision 5). Reports one
    /// [`VerifyError::StateEffectEscapesExpression`] per versioned `Bind`
    /// in the prelude when `context` is [`CloseContext::Opaque`]; reports
    /// nothing when the context threads the prelude's prefixes itself
    /// ([`CloseContext::ThreadsState`]). Callers route the errors through
    /// `report_threaded_ir_verify_errors` (debug/CI hard failure, release
    /// `internal:` diagnostic), never drop them.
    ///
    /// `Bind`s nested inside a `Threaded`/`ConditionalLoop`/`NlrCatch` node
    /// of the prelude are that node's own frame's business (it threads them
    /// to its own `{Value, State}` result) and are not reported — only the
    /// prelude's top-level `Bind`s escape the closed document.
    ///
    /// Renders through the same [`render`]/[`render_value`] production every
    /// spliced prelude goes through, so a closed and a spliced prelude can
    /// never differ in bytes for the statements they share.
    ///
    /// ADR 0118 phase 1a: no production caller yet — see [`CloseContext`].
    #[allow(dead_code)]
    pub(super) fn close(
        self,
        ctx: &mut RenderCtx<'_>,
        context: CloseContext,
    ) -> (Document<'static>, Vec<VerifyError>) {
        let errors = match context {
            CloseContext::ThreadsState => Vec::new(),
            CloseContext::Opaque => self
                .prelude
                .iter()
                .filter_map(|stmt| match stmt {
                    ThreadedStmt::Bind { target, span, .. } => {
                        Some(VerifyError::StateEffectEscapesExpression {
                            prefix: target.prefix.clone(),
                            at: *span,
                        })
                    }
                    _ => None,
                })
                .collect(),
        };
        let prelude_doc = render(&self.prelude, ctx);
        let value_doc = render_value(&self.value, ctx);
        let doc = if self.prelude.is_empty() {
            value_doc
        } else {
            docvec![prelude_doc, value_doc]
        };
        (doc, errors)
    }
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
    /// boolean-predicate ops: 1; `takeWhile:`/`dropWhile:`/`detect:`/
    /// `partition:`-family: 2) — a mismatch here means the unpack would read
    /// threaded-local values from the wrong tuple positions: well-formed Core
    /// Erlang, silently wrong values (the ADR 0110 danger class, not a
    /// `core_lint` failure). BT-3147: a live check, not scaffolding — see
    /// [`build_tuple_acc_unpack`]'s doc comment for the two now-independent
    /// sources (`ListOpKind::gate_slots` at lowering time vs. each call
    /// site's own `index_offset - 1` at rendering time).
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
    ///
    /// `#[cfg(test)]`: this variant's sole constructor
    /// ([`verify_tuple_acc_value_type_exclusion`]) is itself test-only —
    /// see that function's doc comment for why production never reaches
    /// it structurally.
    #[cfg(test)]
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
    ///
    /// `#[cfg(test)]`: this variant's sole constructor
    /// ([`verify_nested_list_op_stateacc_compat`]) is itself test-only —
    /// see that function's doc comment for why production never reaches
    /// it structurally.
    #[cfg(test)]
    NestedStateAccFallbackUnderDirectParams { at: Span },

    /// ADR 0118 §Decision 5 (BT-3415): a [`ThreadedValue`] whose prelude
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
    /// ADR 0118 phase 1a: constructed by [`ThreadedValue::close`], which has
    /// no production caller yet — see [`CloseContext`].
    ///
    /// BT-3430: the "genuine boundary such as a Tier 1 closure body" case
    /// above is exactly the class-method self-send-in-a-bare-block scenario
    /// `check_no_unsafe_class_method_self_sends` (`expressions.rs`) already
    /// diagnoses from a separate, pre-flight static predicate — investigated
    /// replacing that diagnostic with this variant (surfaced via `close()`
    /// at `close_threaded_value_doc`, `util.rs`) and found it blocked on a
    /// real signal-propagation gap, not a small wiring change. See that
    /// predicate's own doc comment for the full finding; still no
    /// production caller.
    #[allow(dead_code)]
    StateEffectEscapesExpression { prefix: VersionPrefix, at: Span },
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
        shadow_write_eligible_stack: vec![true],
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
        // ADR 0118 phase 3 (BT-3419): `condition` scans too — a class-var
        // NLR catch nested there is exactly as relevant to `ShadowWriteMissing`
        // as one nested in `body`, even though no real lowering produces one
        // (a while condition has no NLR boundary of its own).
        ThreadedStmt::ConditionalLoop {
            condition, body, ..
        } => contains_class_var_nlr_catch(condition) || contains_class_var_nlr_catch(body),
        ThreadedStmt::Bind { .. }
        | ThreadedStmt::Return(..)
        | ThreadedStmt::TupleAccUnpack { .. }
        | ThreadedStmt::Statement(..) => false,
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
            // ADR 0118 phase 3 (BT-3419): `condition`'s own Binds are
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
            ThreadedStmt::NlrCatch { .. }
            | ThreadedStmt::Return(..)
            | ThreadedStmt::Statement(..) => {}
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
    /// ADR 0111 Addendum 9, Question 1: parallel to `frame_stack`, seeded
    /// `[true]` (a method's own top level is always shadow-write-eligible),
    /// pushed/popped in lockstep in the `Threaded | ConditionalLoop` arm of
    /// `walk_stmt`, AND-combined with the parent's current top for
    /// defense-in-depth on hand-built fixtures (correct lowering never needs
    /// the AND — a nested node's own `block_depth`-derived flag already
    /// encodes total nesting depth). `ShadowWriteMissing`'s gate reads this
    /// stack's top instead of `target.frame == FrameId::ROOT`.
    shadow_write_eligible_stack: Vec<bool>,
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

    #[allow(clippy::too_many_lines)] // ADR 0118 phase 3 (BT-3419) split the Threaded/ConditionalLoop arm in two
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
                // ADR 0118 phase 5a (BT-3421): `BindOp::Put` only — a
                // `BindOp::Direct` rebind (`emit_class_var_result_unwrap`'s
                // inherited-self-dispatch/loop-construct rebind,
                // `rebind_class_vars_from_doc`) is never itself a
                // shadow-write producer regardless of `shadow_write`'s
                // value (its own doc comments: the underlying mutation was
                // already shadow-written by the callee's own `Put` under
                // the identical `ClassSelf`-tagged key) — `render_bind`'s
                // `BindOp::Put` arm is the only place the ADR 0110 shadow
                // write is even constructed, so `shadow_write` is inert for
                // `Direct`. Before this issue every `Direct`-rebind call
                // site passed `shadow_write_eligible: false` to
                // `construct_and_verify_class_var_bind`'s OWN isolated
                // check and was never spliced into a real, jointly-verified
                // body (always rendered eagerly into an opaque `Document`
                // instead) — so this distinction was never exercised here.
                // Splicing a same-class self-send's real `Bind` directly
                // (ADR 0118 phase 5a) exercises it for the first time: the
                // isolated check's `shadow_write_eligible` exemption has no
                // way to reach this joint walk (it is a fixture-only
                // wrapping trick, never part of the returned `Bind` node —
                // see `construct_and_verify_class_var_bind`'s `needs_wrap`),
                // so the joint check must know the same invariant directly.
                if matches!(target.prefix, VersionPrefix::ClassVars)
                    && matches!(op, BindOp::Put { .. })
                    && *self.shadow_write_eligible_stack.last().unwrap()
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
                shadow_write_eligible,
                body,
                produces,
                span: _,
            } => {
                // ADR 0111 Addendum 9, Question 1: `shadow_write_eligible`
                // pushes/pops in lockstep with `frame`/`mode`, AND-combined
                // with the parent's current top.
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.shadow_write_eligible_stack.push(
                    *self.shadow_write_eligible_stack.last().unwrap() && *shadow_write_eligible,
                );
                self.walk(body);
                for v in produces {
                    self.check_use(v, Span::default());
                }
                self.shadow_write_eligible_stack.pop();
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            ThreadedStmt::ConditionalLoop {
                mode,
                frame,
                shadow_write_eligible,
                condition,
                condition_value,
                body,
                produces,
                span: _,
                ..
            } => {
                // ADR 0111 Addendum 2, Gap 1 / ADR 0118 phase 3 (BT-3419):
                // `ConditionalLoop` verifies almost exactly like `Threaded`
                // — push frame/mode once, walk `condition` THEN `body` (both
                // in the SAME frame — the condition's own `Bind`s are now
                // real IR the loop's later references can see), check_use
                // `condition_value` and each `produces` entry, pop.
                // `continue_arm`/`exit_arm`/`fn_name`/`counter` are opaque
                // (or caller-supplied, non-threading) fields `verify()` does
                // not, and is not meant to, inspect — see the variant's doc
                // comment.
                //
                // ADR 0111 Addendum 9, Question 1: `shadow_write_eligible`
                // pushes/pops in lockstep with `frame`/`mode`, AND-combined
                // with the parent's current top.
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.shadow_write_eligible_stack.push(
                    *self.shadow_write_eligible_stack.last().unwrap() && *shadow_write_eligible,
                );
                self.walk(condition);
                if let ValueRef::Version(v) = condition_value {
                    self.check_use(v, Span::default());
                }
                self.walk(body);
                for v in produces {
                    self.check_use(v, Span::default());
                }
                self.shadow_write_eligible_stack.pop();
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            // Opaque by design: `NlrCatch` has no body field (`render`
            // treats the rest of the slice as its body); a `Statement` is
            // ordinary AST-directed codegen with no state-threading content
            // of its own (see the variant's doc comment).
            ThreadedStmt::NlrCatch { .. } | ThreadedStmt::Statement(..) => {}
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

// ─── RenderCtx (BT-3144, ADR 0111 §Addendum "Renderer design sketch") ──────

/// Loop-context flags captured/restored around rendering a nested
/// [`ThreadedStmt::Threaded`] body — see [`RenderCtx::with_loop_context`].
/// Mirrors `CoreErlangGenerator::in_loop_body`/`in_hybrid_loop` exactly (a
/// third, boolean-pair copy would be a duplication CLAUDE.md's
/// no-duplicate-implementations rule forbids; this type only names the pair
/// [`RenderCtx`] threads, it never becomes a second source of truth for
/// their values — [`RenderCtx::with_loop_context`] always reads/writes them
/// through the wrapped generator).
#[derive(Debug, Clone, Copy)]
struct LoopContextFlags {
    in_loop_body: bool,
    in_hybrid_loop: bool,
}

/// A narrow, purpose-built borrow of [`CoreErlangGenerator`]'s
/// rendering-time facilities (ADR 0111 §Addendum, "Renderer design
/// sketch") — deliberate god-object containment, NOT `&mut
/// CoreErlangGenerator` used wholesale: [`render`] and its helpers only
/// ever reach the generator through the three methods below, never through
/// a raw field or an AST-directed emission path.
///
/// Wraps `&mut CoreErlangGenerator` directly rather than a narrow trait
/// (the alternative the Addendum explicitly left open to this issue).
/// Chosen because it lets NLR-catch rendering reuse
/// [`CoreErlangGenerator::wrap_body_with_nlr_catch`] verbatim — the exact
/// function every real NLR try/catch in the codebase already goes through
/// — instead of re-deriving its scaffolding a second time (CLAUDE.md: "A
/// rule crossing the Rust/Erlang boundary needs a shared conformance
/// fixture or code generation, not a comment"; the same logic applies
/// within Rust here — reuse beats a parallel copy that could drift). The
/// trade-off this accepts is the trait form's unit-testing convenience
/// (constructing a `RenderCtx` without a full generator instance); since
/// [`CoreErlangGenerator::new`] is cheap (no I/O, pure data-structure
/// init), [`lower_and_render`] pays that cost directly instead.
pub(super) struct RenderCtx<'g> {
    generator: &'g mut CoreErlangGenerator,
}

impl<'g> RenderCtx<'g> {
    pub(super) fn new(generator: &'g mut CoreErlangGenerator) -> Self {
        Self { generator }
    }

    /// Fresh-variable allocation for `letrec` loop-function names and NLR
    /// token variables — delegates to
    /// [`CoreErlangGenerator::fresh_temp_var`], the single canonical
    /// allocator (BT-875: naming stays centralized, never re-derived).
    fn fresh_temp_var(&mut self, base: &str) -> String {
        self.generator.fresh_temp_var(base)
    }

    /// Resolves `var`'s render-time name, honoring loop context for
    /// `VersionPrefix::State` (`StateN` vs `StateAccN`) exactly as
    /// [`CoreErlangGenerator::current_state_var`]/`next_state_var` do — both
    /// paths call the same [`super::render_state_prefix`] helper, so this
    /// can never independently drift from live-generator rendering. Every
    /// other prefix is context-independent and renders through
    /// [`VersionedVar::render_name`] unchanged.
    fn resolve_prefix(&self, var: &VersionedVar) -> String {
        match &var.prefix {
            VersionPrefix::State => super::render_state_prefix(
                self.generator.in_hybrid_loop,
                self.generator.in_loop_body,
                var.version,
            ),
            VersionPrefix::ClassVars
            | VersionPrefix::SelfVt
            | VersionPrefix::Local(_)
            | VersionPrefix::Gensym(_) => var.render_name(),
        }
    }

    /// Runs `f` with `in_loop_body`/`in_hybrid_loop` set to `flags`,
    /// unconditionally restoring the previous values afterward — the
    /// render-time counterpart of
    /// [`CoreErlangGenerator::enter_branch_context`]'s save/restore
    /// discipline, narrowed to just the two loop-context flags
    /// [`Self::resolve_prefix`] reads (ADR 0111 §Addendum: "loop-context
    /// `StateAcc`/`State` prefix selection... decided at render time").
    ///
    /// RAII-restored via [`LoopContextGuard`]'s `Drop` impl — not a plain
    /// save/set/call/restore sequence — so a panic inside `f` still leaves
    /// `self.generator`'s flags correctly restored, matching
    /// [`CoreErlangGenerator::enter_branch_context`]'s own panic-safety
    /// guarantee (`BranchContextGuard`) rather than merely resembling it.
    fn with_loop_context<T>(
        &mut self,
        flags: LoopContextFlags,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let guard = LoopContextGuard::enter(self, flags);
        f(guard.ctx)
    }
}

/// RAII guard restoring [`CoreErlangGenerator::in_loop_body`]/
/// `in_hybrid_loop` to their pre-[`RenderCtx::with_loop_context`] values on
/// drop — including on unwind, mirroring [`BranchContextGuard`]'s
/// panic-safety discipline (this module's render path is currently
/// infallible, but a guard costs nothing extra and keeps the two save/
/// restore mechanisms in this file at parity instead of one silently being
/// weaker than the other it's explicitly modeled after).
struct LoopContextGuard<'a, 'g> {
    ctx: &'a mut RenderCtx<'g>,
    saved: LoopContextFlags,
}

impl<'a, 'g> LoopContextGuard<'a, 'g> {
    fn enter(ctx: &'a mut RenderCtx<'g>, flags: LoopContextFlags) -> Self {
        let saved = LoopContextFlags {
            in_loop_body: ctx.generator.in_loop_body,
            in_hybrid_loop: ctx.generator.in_hybrid_loop,
        };
        ctx.generator.in_loop_body = flags.in_loop_body;
        ctx.generator.in_hybrid_loop = flags.in_hybrid_loop;
        Self { ctx, saved }
    }
}

impl Drop for LoopContextGuard<'_, '_> {
    fn drop(&mut self) {
        self.ctx.generator.in_loop_body = self.saved.in_loop_body;
        self.ctx.generator.in_hybrid_loop = self.saved.in_hybrid_loop;
    }
}

// ─── render: full-fidelity ThreadedIr -> Document (BT-3144) ────────────────

/// Renders `ir` to a [`Document`], full-fidelity for `Bind`, `Return`,
/// `TupleAccUnpack`, `NlrCatch`, and `Threaded` under
/// [`ThreadingMode::DirectParams`]/[`ThreadingMode::Hybrid`] — real
/// `letrec`/try-catch scaffolding, not the pre-BT-3144 skeleton. See the
/// module docs §Status for exactly which shapes are full-fidelity today and
/// why (`TupleAcc`/`StateAcc` extend later, driven by a real migration's
/// needs — issue body point 3).
///
/// An [`ThreadedStmt::NlrCatch`] node has no `body` field of its own by
/// design (module docs on the variant): it models the true
/// `wrap_body_with_nlr_catch` call site, whose `body_doc` is "everything
/// that follows," so this function treats it as a boundary marker that
/// consumes the REST of `ir` at its own list position as its try-body, then
/// returns — nothing after an `NlrCatch` renders a second time outside the
/// wrap.
///
/// Has real (non-test) production callers — every later `ThreadedIr`
/// migration this module's §Status log records (conditionals, exception
/// handling, `gen_server` state threading, class-var/NLR routing) renders
/// through this function; see module docs §Status for the full history.
/// The FIRST such caller, `control_flow::while_loops`'s
/// `try_render_while_direct_via_threaded_ir` pilot (BT-3145, gated behind
/// `BEAMTALK_THREADED_IR_WHILE_DIRECT=1`), was deleted by BT-3182 — see
/// §Status (BT-3182) / ADR 0111 Addendum 13.
pub(super) fn render(ir: &[ThreadedStmt], ctx: &mut RenderCtx) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();
    for (i, stmt) in ir.iter().enumerate() {
        match stmt {
            ThreadedStmt::Bind {
                target,
                source,
                op,
                shadow_write,
                span: _,
            } => docs.push(render_bind(target, source, op, *shadow_write, ctx)),
            ThreadedStmt::Threaded {
                mode,
                frame,
                shadow_write_eligible: _, // rendering-irrelevant: verify()-only, see the field's doc comment
                body,
                produces,
                span: _,
            } => docs.push(render_threaded(mode, *frame, body, produces, ctx)),
            ThreadedStmt::NlrCatch {
                boundary, token, ..
            } => {
                let body_doc = render(&ir[i + 1..], ctx);
                docs.push(render_nlr_catch(*boundary, token.name(), body_doc, ctx));
                return Document::Vec(docs);
            }
            ThreadedStmt::Return(value, state, _) => docs.push(render_return(value, state, ctx)),
            ThreadedStmt::TupleAccUnpack {
                param,
                gate_slots,
                targets,
                ..
            } => docs.push(render_tuple_acc_unpack(param, *gate_slots, targets)),
            ThreadedStmt::ConditionalLoop {
                fn_name,
                mode,
                frame,
                shadow_write_eligible: _, // rendering-irrelevant: verify()-only, see the field's doc comment
                counter: _counter, // counted-loop rendering: a later migration wires a real counted-loop call site (BT-3182: the while-direct pilot that used `ConditionalLoop` for its own `mode`/`counter: None` shape was deleted — see ADR 0111 Addendum 13)
                condition,
                condition_value,
                continue_arm,
                body,
                produces,
                exit_arm,
                span: _,
            } => docs.push(render_conditional_loop(
                fn_name,
                mode,
                *frame,
                condition,
                condition_value,
                continue_arm,
                body,
                produces,
                exit_arm,
                ctx,
            )),
            // ADR 0111 Addendum 4: opaque AST-directed statement, emitted
            // verbatim. The doc carries its own trailing glue; this loop
            // adds no separator, and rendering mints nothing.
            ThreadedStmt::Statement(doc, _) => docs.push(doc.clone()),
        }
    }
    Document::Vec(docs)
}

/// Full-fidelity rendering of a [`ThreadedStmt::Threaded`] node: real
/// `letrec` scaffolding for [`ThreadingMode::DirectParams`]/
/// [`ThreadingMode::Hybrid`] (the while-loop family modes this issue's
/// dual-run harness proves parity for — see `render_tests` below);
/// `TupleAcc`/`StateAcc` still flatten the body (module docs §Status).
fn render_threaded(
    mode: &ThreadingMode,
    frame: FrameId,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    ctx: &mut RenderCtx,
) -> Document<'static> {
    match mode {
        ThreadingMode::DirectParams => {
            let fn_name = ctx.fresh_temp_var("Loop");
            render_loop_skeleton(fn_name, frame, body, produces, ctx, None, None)
        }
        ThreadingMode::Hybrid => {
            let fn_name = ctx.fresh_temp_var("Loop");
            render_loop_skeleton(
                fn_name,
                frame,
                body,
                produces,
                ctx,
                Some(LoopContextFlags {
                    in_loop_body: true,
                    in_hybrid_loop: true,
                }),
                None,
            )
        }
        ThreadingMode::TupleAcc(_) | ThreadingMode::StateAcc(_) => render(body, ctx),
    }
}

/// Bundles the pieces [`ThreadedStmt::ConditionalLoop`]'s real condition/
/// case-split shape needs, so [`render_loop_skeleton`] can share its
/// `param_list`/`body_doc`/`final_args` plumbing with the bare unconditional
/// [`Threaded`](ThreadedStmt::Threaded) skeleton via one `Option`. ADR 0118
/// phase 3 (BT-3419): `condition`/`condition_value` replace the pre-BT-3419
/// opaque `continue_header` — the condition must render INSIDE this
/// function's own loop-context closure (alongside `body_doc`), never
/// outside it, because a self-send in the condition references the SAME
/// per-iteration params `body_doc` does (see the variant's own doc
/// comment). `Copy`: every field is a borrow, mirroring the pre-BT-3419
/// `header_and_exit: Option<(&Document, &Document)>` tuple this replaces.
#[derive(Clone, Copy)]
struct ConditionalLoopHeader<'a> {
    condition: &'a [ThreadedStmt],
    condition_value: &'a ValueRef,
    continue_arm: &'a Document<'static>,
    exit_arm: &'a Document<'static>,
}

/// Builds `letrec 'FnName'/arity = fun (Param1, .., ParamN) -> <body> apply
/// 'FnName'/arity (<produces>) in apply 'FnName'/arity (<OuterArg1, ..,
/// OuterArgN>)` — the shared skeleton behind [`ThreadingMode::DirectParams`]
/// (`loop_context: None`) and [`ThreadingMode::Hybrid`] (`loop_context:
/// Some(..)`, flipping `in_loop_body`/`in_hybrid_loop` for the `fun`
/// declaration/body/recursive-call render so any `VersionPrefix::State`
/// reference among them resolves the same way `generate_hybrid_loop_body`
/// makes it resolve live: `State`/`StateN`, not `StateAcc`/`StateAccN`).
///
/// `produces`' own (post-body) versions render as the `fun`'s declared
/// parameters AND the recursive self-call's arguments — both textually
/// inside the same `fun` block as `body_doc`, so both resolve under the
/// loop's OWN context (`render_in_loop_body`, below). The OUTER initial
/// call is different: it is the calling scope's own reference to
/// `produces` at version 0, so it resolves under whatever context was
/// already ambient *before* this `Threaded` node — never the loop's own.
/// For a `Local`-prefixed var these two resolutions coincide
/// ([`VersionedVar::render_name`] names it purely from `(name, version)`,
/// context-independent — the common case, matching how the real
/// direct-params/hybrid generators reuse `to_core_erlang_var`-derived names
/// on both sides, `while_loops.rs`'s `initial_direct_args`/
/// `param_list_doc`); for a `State`-prefixed var nested inside a
/// differently-flagged ambient loop, they do NOT (the `fun`'s formal
/// parameter and the outer call's argument are legitimately different
/// names — a `fun`'s parameter name never has to match its caller's
/// argument expression).
///
///
/// ADR 0111 Addendum 2, Gap 1: factored out of the pre-existing
/// `render_loop_letrec` so the bare unconditional [`Threaded`](ThreadedStmt::Threaded)
/// skeleton (`header: None`) and [`ConditionalLoop`](ThreadedStmt::ConditionalLoop)'s
/// real condition/case-split skeleton (`header: Some(ConditionalLoopHeader { .. })`,
/// ADR 0118 phase 3/BT-3419) share one implementation instead of two
/// near-duplicates (CLAUDE.md's no-duplicate-implementations rule applies
/// within this file, not just across the Rust/Erlang boundary).
///
/// `fn_name` is already resolved by the caller — the bare shape mints it via
/// `ctx.fresh_temp_var("Loop")` (`render_threaded`, unchanged behavior);
/// `ConditionalLoop` carries its own caller-supplied, never-gensym'd name
/// (the variant's own doc comment: production never gensyms this name
/// either).
///
/// `produces`' own (post-body) versions render as the `fun`'s declared
/// parameters AND (for the bare shape only) the recursive self-call's
/// arguments — both textually inside the same `fun` block as `body_doc`, so
/// both resolve under the loop's OWN context (`render_in_loop_body`, below).
/// The OUTER initial call is different: it is the calling scope's own
/// reference to `produces` at version 0, so it resolves under whatever
/// context was already ambient *before* this node — never the loop's own.
/// For a `Local`-prefixed var these two resolutions coincide
/// ([`VersionedVar::render_name`] names it purely from `(name, version)`,
/// context-independent — the common case, matching how the real
/// direct-params/hybrid generators reuse `to_core_erlang_var`-derived names
/// on both sides, `while_loops.rs`'s `initial_direct_args`/
/// `param_list_doc`); for a `State`-prefixed var nested inside a
/// differently-flagged ambient loop, they do NOT (the `fun`'s formal
/// parameter and the outer call's argument are legitimately different
/// names — a `fun`'s parameter name never has to match its caller's
/// argument expression).
///
/// For `ConditionalLoop` (`header: Some(..)`), the recursive
/// self-call's arguments are NOT `produces` verbatim — they are
/// [`final_loop_arg_identities`]'s reconstruction of each local's REAL final
/// `Bind` target (a [`VersionPrefix::Gensym`] identity production actually
/// minted), because `produces` only ever carries each local's INITIAL
/// (`version == 0`) identity (see the variant's doc comment for why the
/// bare shape's "reset version to 0" trick cannot be reused the other
/// direction for a `Gensym` prefix). The body itself also renders
/// differently for `ConditionalLoop`: real loop bodies are `BodyKind::Letrec`
/// (`generate_threaded_loop_body_inner`, `control_flow/mod.rs`), which
/// inserts a literal `" "` between statements — [`render_loop_body_statements`]
/// reproduces that; the bare shape's body is a synthetic, condition-free
/// fixture with no such production twin, so it keeps rendering via plain
/// [`render`].
fn render_loop_skeleton(
    fn_name: String,
    frame: FrameId,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    ctx: &mut RenderCtx,
    loop_context: Option<LoopContextFlags>,
    header: Option<ConditionalLoopHeader<'_>>,
) -> Document<'static> {
    let arity = produces.len();

    // The OUTER initial call's arguments — this is the calling scope's own
    // reference to `produces` at version 0, so it must resolve under
    // whatever context was already ambient *before* this node, never the
    // loop's own context (that would rename a var the caller never bound
    // under).
    let outer_args = join(
        produces
            .iter()
            .map(|v| leaf::var(ctx.resolve_prefix(&VersionedVar::new(v.prefix.clone(), 0, frame)))),
        &Document::Str(", "),
    );

    // BT-3144 review: `param_list` (the `fun (...)` declaration) and
    // `final_args` (the recursive self-call's arguments) both sit textually
    // inside the SAME `fun (...) -> <body_doc> apply ...` block as
    // `body_doc`, so all three must resolve `produces`' prefixes under the
    // identical loop-context flags — computing any of them under the
    // pre-loop ambient context instead would pick a different
    // `State`/`StateAcc` prefix than `body_doc` bound whenever a Hybrid loop
    // is nested inside a differently-flagged ambient context (e.g. inside a
    // `StateAcc`-mode loop), producing a reference to an unbound Core Erlang
    // variable — the `fun`'s own declared parameter name must match every
    // reference to it inside the `fun`'s body, including the recursive tail
    // call.
    let render_in_loop_body = |ctx: &mut RenderCtx| {
        let param_list = join(
            produces.iter().map(|v| {
                leaf::var(ctx.resolve_prefix(&VersionedVar::new(v.prefix.clone(), 0, frame)))
            }),
            &Document::Str(", "),
        );
        // ADR 0118 phase 3 (BT-3419): the condition prelude renders INSIDE
        // this closure, under the identical loop-context flags as
        // `body_doc` — a self-send's `Bind` in `condition` must resolve
        // `State`/`StateAcc` the same way the body's own Binds do (the
        // same reasoning `param_list`/`final_args` already document above).
        let condition_doc = header.map(|h| {
            docvec![
                render(h.condition, ctx),
                "case ",
                render_value(h.condition_value, ctx),
                " of ",
                h.continue_arm.clone(),
            ]
        });
        let body_doc = if header.is_some() {
            render_loop_body_statements(body, ctx)
        } else {
            render(body, ctx)
        };
        let final_args = if header.is_some() {
            join(
                final_loop_arg_identities(body, produces)
                    .iter()
                    .map(|v| leaf::var(ctx.resolve_prefix(v))),
                &Document::Str(", "),
            )
        } else {
            join(
                produces.iter().map(|v| leaf::var(ctx.resolve_prefix(v))),
                &Document::Str(", "),
            )
        };
        (param_list, condition_doc, body_doc, final_args)
    };
    let (param_list, condition_doc, body_doc, final_args) = match loop_context {
        Some(flags) => ctx.with_loop_context(flags, render_in_loop_body),
        None => render_in_loop_body(ctx),
    };

    match header {
        None => docvec![
            "letrec ",
            leaf::fname(fn_name.clone(), arity),
            " = fun (",
            param_list,
            ") -> ",
            body_doc,
            "apply ",
            leaf::fname(fn_name.clone(), arity),
            " (",
            final_args,
            ")",
            " in apply ",
            leaf::fname(fn_name, arity),
            " (",
            outer_args,
            ")",
        ],
        Some(h) => docvec![
            "letrec ",
            leaf::fname(fn_name.clone(), arity),
            " = fun (",
            param_list,
            ") -> ",
            condition_doc.expect("condition_doc is always Some when header is Some"),
            body_doc,
            " apply ",
            leaf::fname(fn_name.clone(), arity),
            " (",
            final_args,
            ") ",
            h.exit_arm.clone(),
            // NOTE: no leading space here (unlike the bare-shape arm above) —
            // `exit_arm` (e.g. `"<'false'> ... end "`) already ends with a
            // trailing space, matching production's own `" end ",` +
            // `"in apply "` concatenation (`while_loops.rs`'s
            // `generate_while_loop_direct`) exactly; an extra leading space
            // here would double it.
            "in apply ",
            leaf::fname(fn_name, arity),
            " (",
            outer_args,
            ")",
        ],
    }
}

/// Renders a real (`ConditionalLoop`) loop body's statements with the
/// literal `" "` separator `generate_threaded_loop_body_inner` inserts
/// between statements for `BodyKind::Letrec` (`control_flow/mod.rs`) —
/// confirmed against real compiled output (two consecutive threaded-local
/// rebinds emit `"... in  let ..."`, a double space: the statement's own
/// trailing `" in "` plus this separator). Each statement renders through
/// the general [`render`] dispatch (a one-element slice), so nested shapes
/// (a future `ConditionalLoop` body statement that isn't a bare `Bind`)
/// stay correctly handled without this function re-deriving `render`'s own
/// match.
fn render_loop_body_statements(body: &[ThreadedStmt], ctx: &mut RenderCtx) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::with_capacity(body.len() * 2);
    for (i, stmt) in body.iter().enumerate() {
        if i > 0 {
            docs.push(Document::Str(" "));
        }
        docs.push(render(std::slice::from_ref(stmt), ctx));
    }
    Document::Vec(docs)
}

/// Reconstructs each `produces` entry's REAL final identity by following its
/// `Bind` chain forward through `body` (ADR 0111 Addendum 2, Gap 1 — see
/// [`ThreadedStmt::ConditionalLoop`]'s doc comment for why this cannot be
/// derived from `produces` alone the way the bare loop shape derives
/// `param_list`/`outer_args` from it). `produces` seeds the search at each
/// local's INITIAL (`version == 0`) identity; a local never rebound in
/// `body` keeps that identity (matching legacy's `collect_final_local_args`
/// fallback — the fun's own unchanged parameter name IS the final arg).
/// Only scans `body`'s own top-level `Bind`s — a nested construct's `Bind`s
/// belong to a different [`FrameId`] and can never be this loop's own
/// rebind chain.
fn final_loop_arg_identities(
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
) -> Vec<VersionedVar> {
    produces
        .iter()
        .map(|seed| {
            let mut current = seed.clone();
            while let Some(next) = body.iter().find_map(|stmt| match stmt {
                ThreadedStmt::Bind { source, target, .. } if *source == current => {
                    Some(target.clone())
                }
                _ => None,
            }) {
                current = next;
            }
            current
        })
        .collect()
}

/// Full-fidelity rendering of a [`ThreadedStmt::ConditionalLoop`] node (ADR
/// 0111 Addendum 2, Gap 1; condition fields per ADR 0118 phase 3, BT-3419):
/// delegates to [`render_loop_skeleton`] with the real `condition`/
/// `condition_value`/`continue_arm`/`exit_arm` bundle, reusing the exact
/// same `param_list`/`outer_args` plumbing the bare loop shape uses.
#[allow(clippy::too_many_arguments)]
fn render_conditional_loop(
    fn_name: &str,
    mode: &ThreadingMode,
    frame: FrameId,
    condition: &[ThreadedStmt],
    condition_value: &ValueRef,
    continue_arm: &Document<'static>,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    exit_arm: &Document<'static>,
    ctx: &mut RenderCtx,
) -> Document<'static> {
    let loop_context = match mode {
        ThreadingMode::Hybrid => Some(LoopContextFlags {
            in_loop_body: true,
            in_hybrid_loop: true,
        }),
        ThreadingMode::DirectParams | ThreadingMode::TupleAcc(_) | ThreadingMode::StateAcc(_) => {
            None
        }
    };
    render_loop_skeleton(
        fn_name.to_string(),
        frame,
        body,
        produces,
        ctx,
        loop_context,
        Some(ConditionalLoopHeader {
            condition,
            condition_value,
            continue_arm,
            exit_arm,
        }),
    )
}

/// Full-fidelity rendering of [`ThreadedStmt::TupleAccUnpack`]: `let V = call
/// 'erlang':'element'(idx, Param) in` chain — `idx` starts at `gate_slots + 1`
/// (1-based, past the leading gate slots) for the first target. No generator
/// context needed (every target renders through [`VersionPrefix::Gensym`]'s
/// context-independent verbatim naming, [`build_tuple_acc_unpack`]'s doc
/// comment). Real production output as of BT-3147 — see module docs §Status
/// — not just a byte-identical-by-inspection shape as pre-BT-3147.
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

pub(super) fn render_value(value: &ValueRef, ctx: &RenderCtx) -> Document<'static> {
    match value {
        ValueRef::Version(v) => leaf::var(ctx.resolve_prefix(v)),
        ValueRef::Var(name) => leaf::var(name.clone()),
        ValueRef::Literal(lit) => Document::Str(lit),
        ValueRef::Doc(doc) => doc.clone(),
    }
}

fn render_bind(
    target: &VersionedVar,
    source: &VersionedVar,
    op: &BindOp,
    shadow_write: bool,
    ctx: &RenderCtx,
) -> Document<'static> {
    let target_name = ctx.resolve_prefix(target);
    let source_name = ctx.resolve_prefix(source);
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
                render_value(value, ctx),
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
                    render_value(class_tag, ctx),
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
            render_value(value, ctx),
            " in ",
        ],
    }
}

/// Full-fidelity NLR try/catch scaffolding: reuses
/// [`CoreErlangGenerator::wrap_body_with_nlr_catch`] verbatim — the exact
/// function every real NLR try/catch in the codebase already goes through
/// (module docs on [`ThreadedStmt::NlrCatch`]: "the true call site
/// `ThreadedStmt::NlrCatch` faithfully models"). Zero re-derivation, so
/// this can never drift from production's try/catch shape.
///
/// ADR 0111 Addendum 4 §Gap 3: `token_var` is the [`TokenId`]-carried name
/// the lowering pass minted BEFORE the body's own temps (production's real
/// mint order — `gen_server/methods.rs`'s call sites mint `NlrToken` first,
/// unconditionally, then generate the body). This function no longer mints
/// anything; only the catch-scaffolding vars (`NlrResult`, `NlrCls`, …) are
/// still allocated here, matching production's own post-body
/// `alloc_nlr_catch_vars` position.
fn render_nlr_catch(
    boundary: NlrBoundary,
    token_var: &str,
    body_doc: Document<'static>,
    ctx: &mut RenderCtx,
) -> Document<'static> {
    ctx.generator
        .wrap_body_with_nlr_catch(body_doc, token_var, boundary)
}

fn render_return(value: &ValueRef, state: &VersionedVar, ctx: &RenderCtx) -> Document<'static> {
    docvec![
        "{",
        render_value(value, ctx),
        ", ",
        leaf::var(ctx.resolve_prefix(state)),
        "}"
    ]
}

// ─── BT-3133/BT-3147 (ADR 0111 Phase C) TupleAcc unpack: emission input ────

/// Builds the real `ThreadedIr` for one `TupleAcc`-mode fold lambda's
/// per-iteration positional unpack step — the single production emitter of
/// this unpack shape across every list-op and dict-op call site
/// (`basic_ops.rs`, `filter_ops.rs`, `search_ops.rs`, `transform_ops.rs`,
/// `dict_ops.rs`, all via `ThreadingPlan::generate_tuple_unpack_docs`,
/// `control_flow/mod.rs`).
///
/// BT-3147: promoted from a verification-only side channel (BT-3133) to
/// genuine emission input — [`render`]ing this exact `ThreadedStmt` IS how
/// `generate_tuple_unpack_docs` now produces its `Document` output, not a
/// second, independently hand-Document-built duplicate of it. Each target's
/// identity is a [`VersionPrefix::Gensym`] (never [`VersionPrefix::Local`]):
/// the real per-iteration unpack binds the BARE `to_core_erlang_var` name
/// (`Sum`, never `Sum1`) — matching every real call site's pre-BT-3147
/// hand-rolled loop byte-for-byte (confirmed against `basic_ops.rs`'s
/// `generate_list_do_with_mutations`, the simplest call site) — never the
/// sequential `prefix{version}` scheme `Local`'s renderer would apply to a
/// nonzero version. Same naming-scheme-mismatch class ADR 0111 Addendum 2
/// "Gap 2" already named and closed for loop-local rebinds; `Gensym`'s
/// existing verbatim-regardless-of-version rendering closes it here too,
/// with no new IR machinery.
///
/// **Independent gate-slot derivation (BT-3147, invariant class 4 goes
/// live)**: `mode_gate_slots` and `node_gate_slots` are now two genuinely
/// separate sources, no longer the same argument threaded twice. Callers
/// pass `mode_gate_slots` from [`super::control_flow::ListOpKind::gate_slots`]
/// — a canonical per-op-family table fixed at `ThreadingPlan` construction
/// (lowering time, before any particular call site's `index_offset` is even
/// chosen) — and `node_gate_slots` from their own already-computed
/// `index_offset - 1` (rendering-side construction, unchanged from BT-3133).
/// A call site whose `index_offset` disagrees with its declared
/// `ListOpKind` (e.g. a future op miscategorized when copy-pasted from a
/// same-shaped sibling) now trips [`VerifyError::EarlyExitGateSlotMismatch`]
/// for real — see [`verify_tuple_acc_unpack_invariant`] and
/// `control_flow::mod::ListOpKind`'s own doc comment for the full per-op
/// gate-slot table (`0` for `do:`/`dict do:`; `1` for `collect:`/
/// `select:`/`reject:`/`inject:into:`/`anySatisfy:`/`allSatisfy:`/`count:`/
/// `flatMap:`/`groupBy:`; `2` for `detect:`/`takeWhile:`/`dropWhile:`/
/// `partition:` — `partition:`'s two result lists need the same two leading
/// slots an early-exit op's found-item/continue-flag pair does, even though
/// it never early-exits itself).
pub(super) fn build_tuple_acc_unpack(
    param_name: &str,
    mode_gate_slots: usize,
    node_gate_slots: usize,
    threaded_locals: &[String],
    span: Span,
) -> (ThreadedStmt, Vec<VersionedVar>) {
    let frame = FrameId::new(1);
    let param = AccParam::new(param_name);
    let targets: Vec<VersionedVar> = threaded_locals
        .iter()
        .enumerate()
        .map(|(i, local)| {
            let core_name = CoreErlangGenerator::to_core_erlang_var(local);
            VersionedVar::new(VersionPrefix::Gensym(core_name), i + 1, frame)
        })
        .collect();
    let stmt = ThreadedStmt::Threaded {
        mode: ThreadingMode::TupleAcc(mode_gate_slots),
        frame,
        // `TupleAcc` unpacks only local threaded vars — never a class var
        // (per ADR 0111 Addendum 9, Question 4/6: `TupleAcc(>0)` is
        // unconditionally excluded whenever a body threads `ClassVars`), so
        // this node can never itself contain a class-var `Bind` needing the
        // eligibility check. `true` is the neutral default, matching
        // `verify()`'s own `[true]` seed.
        shadow_write_eligible: true,
        body: vec![ThreadedStmt::TupleAccUnpack {
            param,
            gate_slots: node_gate_slots,
            targets: targets.clone(),
            frame,
            span,
        }],
        produces: targets.clone(),
        span,
    };
    (stmt, targets)
}

// ─── Class-var Bind construction (BT-3135/BT-3148, ADR 0110 contract) ─────

/// Constructs the single class-var version `Bind` a production emission site
/// is about to render, and verifies it against a synthetic ADR 0110
/// NLR-relay marker — the two producer sites named in ADR 0111 §Phase D:
/// `expressions.rs::generate_field_assignment`'s class-var branch (a
/// [`BindOp::Put`] field mutation, `shadow_write` set from the real
/// `block_depth == 0` gate) and `dispatch_codegen.rs::emit_class_var_result_unwrap`
/// (a [`BindOp::Direct`] rebind from an inherited self-dispatched call's
/// returned `class_var_result` tuple — never itself a shadow-write producer,
/// since self-dispatch runs in the same class `gen_server` process and any
/// mutation it reflects was already shadow-written by the *callee's own*
/// `generate_field_assignment` call under the same `ClassSelf`-tagged key;
/// see ADR 0110 §Runtime change's "no per-nesting-level save/restore is
/// needed" reasoning).
///
/// **BT-3148**: unlike the deleted `verify_class_var_bind` (which verified a
/// hardcoded `0 -> 1` step, disconnected from the version actually in play —
/// both call sites rendered their real `current_class_var()`/`next_class_var()`
/// names by hand, and separately passed an always-`0->1` fixture here purely
/// to run the shadow-write check), the [`ThreadedStmt::Bind`] returned here
/// carries the REAL `source_version`/`target_version` already read off the
/// live generator counter and IS what [`render`] renders — there is no
/// second, independently-reconstructed shape. `1..=source_version` backfills
/// a dummy `Bind` chain so [`VerifyWalk::check_use`]'s frame-flow rule
/// doesn't spuriously fail `UnboundVersion` for a mutation past a method's
/// first (the same technique [`verify_simple_bind`] uses).
///
/// The synthetic `NlrCatch { has_class_vars: true }` marker is still
/// unconditionally included in the *verified* fixture (not in the returned
/// `Bind`, which callers render alone) — an isolated per-call-site
/// verification cannot observe whether THIS method's body really contains a
/// foreign-NLR-relay-capable boundary, so it assumes one, same as the
/// deleted fixture did (ADR 0111 §Verifier honesty).
///
/// `frame` (ADR 0111 Addendum 9, Question 2) is the `Bind`'s (and the
/// marker's) real `FrameId` — the caller's own `current_branch_frame()` for
/// a loop/fold-body class-var mutation, or [`FrameId::ROOT`] for a
/// top-frame one. Unlike the deleted `at_method_top_frame: bool` this
/// replaces, `frame` no longer doubles as the shadow-write-eligibility
/// signal — that is `shadow_write_eligible`'s own, independent job
/// (Addendum 9, Question 1): whether a foreign NLR relayed out of this
/// class method is still guaranteed to observe this mutation via the ADR
/// 0110 process-dictionary shadow write. `FrameId` scopes version linearity
/// (sibling branch arms/loop iterations getting fresh, disjoint identities);
/// `shadow_write_eligible` scopes shadow-write eligibility
/// (`self.block_depth == 0` — "is this still the method's own top level, not
/// nested inside a first-class block-literal closure boundary"). The two
/// axes coincided at `FrameId::ROOT` only because no non-`ROOT` frame ever
/// carried a legitimate class-var mutation before loop/fold bodies did.
///
/// **This is deliberately NOT `self.current_nlr_token().is_some()`** (whether
/// *this* method's own body happens to contain a literal `^` inside one of
/// its own block literals, gating whether `lower_class_method_body`'s
/// caller prepends a real `NlrCatch` — BT-3164, formerly `wrap_class_method_body_with_nlr_catch`)
/// — that would silently exempt the *exact* ADR 0110 repro shape from this
/// check: `CollectionDriver countedRun:over:` mutates a class var and then
/// invokes a **caller-supplied** block (`aBlock value: x`, inside its own
/// `[:x | aBlock value: x]` block, which contains no literal `^` of its
/// own) — `has_block_nlr_or_walk` is `false` for that method, so it gets no
/// local NLR try/catch at all, and the whole relay is caught one layer
/// out, unconditionally, by `apply_class_method_fun/6`'s
/// `throw:Nlr:NlrST when ?IS_NLR(Nlr)` clause. The shadow write in
/// production code is correspondingly unconditional too — gated only on
/// `block_depth == 0` (`generate_field_assignment`), never on local
/// NLR-catch presence. Callers must therefore pass an *independently
/// re-derived* `shadow_write_eligible` (`self.block_depth == 0`, read fresh
/// from live generator state — not reused from whatever `shadow_write` value
/// a future regression might compute wrong), matching production's actual
/// gate 1:1 (ADR 0111 §Verifier honesty: comparing the generator against
/// itself is silent when both are consistently wrong).
///
/// The `dispatch_codegen.rs` rebind site passes `FrameId::ROOT`/`false`
/// unconditionally — the frame is now honestly `ROOT` (that call site never
/// claims a real nested identity), and `shadow_write_eligible: false` is a
/// deliberate exemption: that `Bind` structurally never carries its own
/// shadow-write obligation regardless of nesting (see its own call-site
/// comment), so it is modeled as never eligible for this check.
pub(super) fn construct_and_verify_class_var_bind(
    op: BindOp,
    shadow_write: bool,
    frame: FrameId,
    shadow_write_eligible: bool,
    source_version: usize,
    target_version: usize,
    span: Span,
) -> (ThreadedStmt, Vec<VerifyError>) {
    // `shadow_write: true` here is a backfill-scaffolding assumption, not a
    // claim about the earlier mutation's real shape (this fixture never
    // inspects it): only the LAST Bind below — the one this call site is
    // actually about to render — is what `ShadowWriteMissing` is checking.
    // Backfilling `false` would spuriously flag every earlier synthetic step
    // at `FrameId::ROOT`, since `has_class_vars_nlr` is unconditionally true
    // here (the synthetic marker below).
    let mut body = backfill_version_chain(
        &VersionPrefix::ClassVars,
        frame,
        0,
        source_version,
        true,
        span,
    );
    let bind = ThreadedStmt::Bind {
        target: VersionedVar::new(VersionPrefix::ClassVars, target_version, frame),
        source: VersionedVar::new(VersionPrefix::ClassVars, source_version, frame),
        op,
        shadow_write,
        span,
    };
    body.push(bind.clone());
    // Fixture-only synthetic marker (never rendered — not in the returned
    // `Bind`, which callers render alone), so its token name is a literal
    // placeholder, never a real lowering-minted `NlrToken` temp.
    let marker = ThreadedStmt::NlrCatch {
        boundary: NlrBoundary::ClassMethod {
            has_class_vars: true,
        },
        token: TokenId::new("NlrTokenFixtureOnly"),
        frame,
        span,
    };
    // `verify()` seeds its frame stack with just `[FrameId::ROOT]` (module
    // docs on `FrameId`) and its shadow-write-eligibility stack with just
    // `[true]` — `body`'s Binds are only reachable at the top level with no
    // wrapper when BOTH `frame == FrameId::ROOT` AND `shadow_write_eligible`
    // (ADR 0111 Addendum 9, Question 2's correction to this branch: an OR of
    // two independent triggers, not a replacement of one by the other).
    //
    // Trigger 1 (unchanged from before Addendum 9): `frame != FrameId::ROOT`
    // must PUSH `frame` via a `Threaded` node, or `VerifyWalk::check_use`'s
    // frame-flow rule can never find a backfilled version `>0` at that frame
    // (a bare top-level `Bind`/`NlrCatch` never pushes anything) — this is
    // exactly the gap that produced a spurious `UnboundVersion` before
    // BT-3148 added real backfill history here (the `dispatch_codegen.rs`
    // rebind site's frame is now honestly `FrameId::ROOT` too, so it no
    // longer triggers this one — see its own call-site comment).
    //
    // Trigger 2 (new in Addendum 9): `!shadow_write_eligible` must ALSO wrap,
    // independently of `frame` — the `dispatch_codegen.rs` rebind site's
    // `shadow_write_eligible: false` at its now-`FrameId::ROOT` frame would
    // otherwise fall onto the bare path and be silently exempt from
    // `ShadowWriteMissing`'s eligibility check for the wrong reason (a bare
    // top-level `Bind` is unconditionally eligible per `verify()`'s `[true]`
    // seed) — wrapping still resolves that false exemption, because the
    // wrapper's own `shadow_write_eligible: false` correctly AND-combines
    // down to `false` on the stack, matching production's real "never
    // eligible" semantics for this call site.
    let needs_wrap = frame != FrameId::ROOT || !shadow_write_eligible;
    let fixture: Vec<ThreadedStmt> = if needs_wrap {
        vec![
            ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                shadow_write_eligible,
                body,
                produces: Vec::new(),
                span,
            },
            marker,
        ]
    } else {
        let mut f = body;
        f.push(marker);
        f
    };
    (bind, verify(&fixture))
}

// ─── Method-body verification with opaque version gaps (BT-3148) ──────────

/// [`verify`]s a straight-line, [`FrameId::ROOT`]-frame method-body IR (ADR
/// 0111 Addendum 4 / BT-3148 task 1: `gen_server/methods.rs`'s
/// `lower_body_exprs_with_reply` output — real `Bind`s interleaved with
/// opaque [`ThreadedStmt::Statement`]s) whose opaque statements may have
/// advanced the `State` version counter invisibly: a dispatching self-send,
/// `super` send, or Tier-2 helper (`generate_self_dispatch_open`,
/// `emit_super_send_open`, `generate_tier2_self_send_open`,
/// `generate_field_assignment_open`, …) calls `next_state_var` inside its
/// own shared, multi-module `Document` builder, so the version step it
/// produces has no `Bind` node in this body's IR.
///
/// Without accounting for those gaps, the first real `Bind` after such a
/// helper would spuriously fail [`VerifyError::UnboundVersion`] (its source
/// version has no producing `Bind` in the fixture). The fix reuses
/// [`verify_simple_bind`]/[`construct_and_verify_class_var_bind`]'s
/// established backfill technique, generalized from "backfill everything
/// before the one Bind under test" to "backfill exactly the gaps between
/// this body's real Binds": walk the IR in order, tracking the last
/// `State`-prefix version produced at [`FrameId::ROOT`], and insert a
/// synthetic `Direct('_')` `Bind` chain for any versions an opaque
/// statement consumed. The backfill is verification-fixture-only — the real
/// IR (and therefore [`render`]'s output) is untouched.
///
/// What this still genuinely checks across the REAL Binds, for the first
/// time over whole-method emission structure rather than per-call-site
/// fixtures: per-version linearity (a broken `next_state_var` regressing to
/// an already-produced version now collides with the accumulated real/
/// backfill history — the BT-3131 regression shape,
/// `verify_would_catch_the_bt_3131_regression_shape_given_accumulated_history`'s
/// previously-hypothetical capability made live), `UnboundVersion` for any
/// source the chain never reached, and [`VerifyError::ShadowWriteMissing`]
/// over any class-var `Bind` sharing the body with a real `NlrCatch`.
/// What it cannot check (ADR 0111 §Verifier honesty, same class as
/// `ValueRef::Doc`/`exit_arm`): mutations hidden inside the opaque
/// statements themselves — those are exactly the backfilled gaps.
pub(super) fn verify_body_with_opaque_version_gaps(ir: &[ThreadedStmt]) -> Vec<VerifyError> {
    let mut fixture: Vec<ThreadedStmt> = Vec::with_capacity(ir.len());
    let mut last_state_version = 0usize;
    let mut last_class_var_version = 0usize;
    for stmt in ir {
        if let ThreadedStmt::Bind { target, source, .. } = stmt {
            backfill_opaque_version_gap(
                &mut fixture,
                &VersionPrefix::State,
                target,
                source,
                &mut last_state_version,
            );
            backfill_opaque_version_gap(
                &mut fixture,
                &VersionPrefix::ClassVars,
                target,
                source,
                &mut last_class_var_version,
            );
        }
        fixture.push(stmt.clone());
    }
    verify(&fixture)
}

/// Shared backfill step for one `VersionPrefix` inside
/// [`verify_body_with_opaque_version_gaps`]'s per-`Bind` scan — extracted so
/// the identical technique isn't hand-duplicated once per prefix (CLAUDE.md's
/// no-duplicate-implementations rule). The synthetic `Bind`s this inserts
/// always carry `shadow_write: true` (BT-3164, fixing a real bug this
/// issue's own review caught: an earlier `false` here spuriously tripped
/// [`VerifyError::ShadowWriteMissing`] on a `ClassVars` gap step whenever a
/// real `NlrCatch` was present — see the `shadow_write: true` assignment
/// below for the full reasoning, the same [`construct_and_verify_class_var_bind`]
/// already established for its own backfill loop). Moot for `State`
/// (`ShadowWriteMissing` never inspects `State`-prefix `Bind`s); load-bearing
/// for `ClassVars`.
fn backfill_opaque_version_gap(
    fixture: &mut Vec<ThreadedStmt>,
    prefix: &VersionPrefix,
    target: &VersionedVar,
    source: &VersionedVar,
    last_version: &mut usize,
) {
    if source.prefix == *prefix && source.frame == FrameId::ROOT && source.version > *last_version {
        // BT-3164: `shadow_write: true`, not `false` — same reasoning
        // `construct_and_verify_class_var_bind`'s own backfill chain (above)
        // already documents for its synthetic steps: this stands in for a
        // REAL mutation this verifier cannot see (it lives inside an opaque
        // `Statement`, e.g. `emit_class_var_result_unwrap`'s own internal
        // `next_class_var()` bump for a class-method self-send). For `State`
        // this is moot (`ShadowWriteMissing` never inspects `State`-prefix
        // `Bind`s), but for `ClassVars` a `false` here would claim "this
        // top-frame mutation is definitely missing its ADR 0110 shadow
        // write" about a step whose real emission site this verifier never
        // inspected — exactly the false-positive `ShadowWriteMissing` a
        // class method with a class-var-mutating self-send followed by its
        // own real last-statement class-var `Bind` spuriously tripped before
        // this fix (confirmed by
        // `verify_body_with_opaque_version_gaps_classvars_backfill_does_not_spuriously_fire_shadow_write_missing`
        // below). ADR 0111 §Verifier honesty: a check that cannot see the
        // real site must not assert a verdict about it — `true` is silence,
        // not a claim of compliance either way.
        fixture.extend(backfill_version_chain(
            prefix,
            FrameId::ROOT,
            *last_version,
            source.version,
            true,
            Span::default(),
        ));
        *last_version = source.version;
    }
    if target.prefix == *prefix && target.frame == FrameId::ROOT {
        *last_version = (*last_version).max(target.version);
    }
}

/// Builds a synthetic `Direct('_')` `Bind` chain backfilling version history
/// `(from+1)..=to` at `frame` for `prefix` — the technique
/// [`construct_and_verify_class_var_bind`], [`backfill_opaque_version_gap`],
/// and [`verify_simple_bind`] all need to give [`VerifyWalk::check_use`]'s
/// frame-flow rule a producing `Bind` for version history a fixture can't
/// otherwise see (BT-3179: extracted from three hand-duplicated copies of
/// this loop, CLAUDE.md's no-duplicate-implementations rule).
fn backfill_version_chain(
    prefix: &VersionPrefix,
    frame: FrameId,
    from: usize,
    to: usize,
    shadow_write: bool,
    span: Span,
) -> Vec<ThreadedStmt> {
    let mut chain = Vec::with_capacity(to.saturating_sub(from));
    for v in (from + 1)..=to {
        chain.push(ThreadedStmt::Bind {
            target: VersionedVar::new(prefix.clone(), v, frame),
            source: VersionedVar::new(prefix.clone(), v - 1, frame),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            shadow_write,
            span,
        });
    }
    chain
}

// ─── Simple version-bind construction (BT-3139) ────────────────────────────

/// Builds and verifies a minimal `ThreadedIr` fixture for a single `Self{N}`
/// or `State{N}` version `Bind`, given the real source/target version
/// numbers already read off the live generator counter at the call site
/// (BT-3139: `generate_field_assignment`'s value-type and instance-actor
/// branches, `expressions.rs` around lines 634/664 — the two sibling
/// branches of the class-var branch [`construct_and_verify_class_var_bind`]
/// already covers, BT-3135/BT-3148). Reused for both prefixes instead of
/// copy-pasting [`construct_and_verify_class_var_bind`]'s body three times
/// (CLAUDE.md's no-duplicate-implementations rule).
///
/// Like [`construct_and_verify_class_var_bind`] (BT-3148 onward, both are
/// handed the real version numbers already read off the live generator
/// counter and share its `1..=source_version` backfill technique), but
/// without that function's class-var-specific `ShadowWriteMissing`
/// machinery (the synthetic `NlrCatch` marker, the `frame`/
/// `shadow_write_eligible` pair) — `Self{N}`/`State{N}` mutations never
/// carry that ADR 0110 obligation. This helper is handed the *actual* version numbers,
/// which may already be arbitrarily large after earlier mutations in the
/// same method. [`VerifyWalk::check_use`]'s
/// frame-flow rule requires every version `>0` referenced as a `Bind`'s
/// source to have a producing `Bind` visible on the frame stack — so without
/// backfilling that history, every mutation past a method's first would
/// spuriously fail `UnboundVersion` (its `source_version` would have no
/// producer in an isolated single-`Bind` fixture). The backfill chain
/// (`1..=source_version`) is exactly the technique BT-3134's
/// branch-frame-linearity check used (retired, ADR 0111 Addendum 5 /
/// BT-3165), generalized here to an arbitrary prefix and reused rather than
/// re-implemented.
///
/// **Scope, honestly stated (ADR 0111 §Verifier honesty):** each call is
/// independently verified — `check_simple_field_bind_invariant`
/// (`expressions.rs`) invokes this fresh per mutation site, with no
/// generator field accumulating a method-wide `Bind` history across calls.
/// So this catches a version going non-monotonic **within one call**
/// (`target_version` colliding with a version the backfilled
/// `1..=source_version` chain already reached — e.g. a broken
/// `next_self_var()`/`next_state_var()` that returns a version `<=
/// source_version`). It does **not** catch the historical BT-3131
/// `self_version` "reset instead of inherit on branch entry" shape as it
/// actually manifested: two *separate* mutation call sites each
/// independently computing `source=0, target=1`, which are each
/// individually valid in isolation and never compared against each other.
/// `verify_would_catch_the_bt_3131_regression_shape_given_accumulated_history`
/// below demonstrates that `verify()` itself *would* catch that cross-call shape
/// given an accumulated history — it is a test of `verify()`'s capability,
/// not a claim about what today's isolated per-call wiring provides.
/// Threading a real per-method `Bind` history through
/// `check_simple_field_bind_invariant` so this call actually closes that
/// gap is tracked as a follow-up, not attempted here (BT-3139 is scoped to
/// coverage extension via the existing checks, not new generator state).
pub(super) fn verify_simple_bind(
    prefix: VersionPrefix,
    source_version: usize,
    target_version: usize,
    span: Span,
) -> Vec<VerifyError> {
    let frame = FrameId::ROOT;
    let mut ir = backfill_version_chain(&prefix, frame, 0, source_version, false, span);
    ir.push(ThreadedStmt::Bind {
        target: VersionedVar::new(prefix.clone(), target_version, frame),
        source: VersionedVar::new(prefix, source_version, frame),
        op: BindOp::Direct(ValueRef::Literal("'_'")),
        shadow_write: false,
        span,
    });
    verify(&ir)
}

#[cfg(test)]
mod tests;
