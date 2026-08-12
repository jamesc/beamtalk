// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadedIr` — a small, narrowly-scoped mid-level IR for the
//! state-threading / control-flow subset of Core Erlang codegen (ADR 0111).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ## Status (as of BT-3144 — see ADR 0111 §Addendum)
//!
//! `ThreadedIr` is still a **verification-only side channel** at every
//! production `verify_*` call site — none of them route their fixture into
//! `Document` construction; that remains directly from AST + generator
//! state, on a separate, unconnected path (ADR 0111's own Alternative 1b
//! (§B2) shape, delivered under Option A's name — the honest accounting
//! lives in the ADR's Addendum, not here). [BT-3141](https://linear.app/beamtalk/issue/BT-3141)
//! is the epic re-scoped to complete Option A (`ThreadedIr` as real emission
//! input, single renderer, verifier on the true IR).
//!
//! BT-3144 (this issue) lands that epic's renderer *foundation*: [`render`]
//! is a **full-fidelity** `&[ThreadedStmt] -> Document` function for the
//! while-loop family's `DirectParams`/`Hybrid` `Threaded` modes and for
//! `NlrCatch` — real `letrec` scaffolding, real try/catch scaffolding (the
//! latter by directly reusing [`super::CoreErlangGenerator::wrap_body_with_nlr_catch`],
//! not a re-derivation), via [`RenderCtx`] — the narrow borrow of generator
//! rendering services the ADR's Addendum sketches. `TupleAcc` (list-ops) and
//! the generic `StateAcc` fallback mode still render at the pre-BT-3144
//! skeleton fidelity (flattened body, no letrec wrapper) — no production
//! call site or dual-run proof needs them yet; a later migration issue
//! extends `render` to them when it does (issue body point 3: "extend node
//! types only where fidelity requires it").
//!
//! ## Status (as of BT-3145 — see ADR 0111 Addendum 2)
//!
//! BT-3145 (the pilot migration) adds [`ThreadedStmt::ConditionalLoop`] (the
//! real condition/case-split loop skeleton, Addendum 2's "Gap 1") and
//! [`VersionPrefix::Gensym`] (a pre-minted, verbatim Core Erlang name,
//! Addendum 2's "Gap 2"), and points the FIRST real (non-test) emission site
//! at [`render`]: `while_loops.rs`'s `generate_while_loop_direct`, gated
//! behind the `BEAMTALK_THREADED_IR_WHILE_DIRECT=1` env flag (off by
//! default). Coverage is deliberately narrow — only straight-line,
//! `Bind`-representable loop bodies (`while_loops.rs`'s
//! `while_direct_body_is_bind_representable`) route through this module;
//! anything else falls back to the legacy path untouched. A real
//! implementation attempt surfaced a third, smaller-but-real gap beyond
//! Addendum 2's own two (an opaque-RHS-value need, closed here via
//! [`ValueRef::Doc`], plus a `BodyKind::Letrec` inter-statement whitespace
//! quirk, confirmed against real compiled output) — recorded on the BT-3145
//! Linear issue rather than silently folded into "done." The default stays
//! the legacy path (this pilot's own measurement gate did not clear
//! deleting it) — see BT-3145's Linear issue for the full evidentiary trail
//! and the ≤3% gate's outcome.
//!
//! ## Status (as of BT-3148 — partial: class-var Bind producers only)
//!
//! BT-3148 ("the deepest slice") targeted four sub-tasks: (1) unify
//! `gen_server/methods.rs`'s upfront `classify_body_expr` routing
//! classification with `threaded_expr.rs`'s downstream recheck into one IR
//! construction, deleting the `verify_routing_invariant` debug-assert
//! replacements; (2) give `wrap_body_with_nlr_catch` a production `NlrCatch`
//! node instead of the deliberately-IR-free side channel it still is
//! (`mod.rs`, doc comment on [`super::CoreErlangGenerator::wrap_body_with_nlr_catch`]);
//! (3) the two class-var `Bind` producer sites
//! (`expressions.rs::generate_field_assignment`,
//! `dispatch_codegen.rs::emit_class_var_result_unwrap`) emit through real
//! [`ThreadedStmt::Bind`] nodes rendered by [`render`], not a hand-rolled
//! `Document` that mirrors it; (4) `threaded_expr.rs`'s `ThreadingBoundary`
//! adapter absorbed into or replaced by the IR path.
//!
//! **Only (3) landed.** [`construct_and_verify_class_var_bind`] replaces the
//! deleted `verify_class_var_bind` fixture-mirror: both class-var Bind
//! producer sites now construct a real `Bind` carrying the actual
//! `source_version`/`target_version` read off the live generator counter,
//! verify it, and render it through [`render`] — [`render_bind`]'s
//! `BindOp::Put` arm is the only place the ADR 0110 shadow write is
//! constructed in either the verification path or the emitted `Document`.
//! The existing `invoke_class_method/7` cross-boundary conformance fixture
//! (`tests/class_var_shadow_contract.rs`) and the full
//! `compiles_through_erlc`/snapshot-corpus suite pin that this produces
//! byte-identical output to the hand-rolled `Document` it replaces.
//!
//! **(1), (2), (4) were investigated and NOT attempted** — closing them
//! requires a real method-body-wide IR (every ordinary AST-directed
//! statement — message sends, dispatch, Tier 2 calls, `EarlyReturn`, the
//! other ~15 `BodyExprKind` variants `gen_server/methods.rs` classifies —
//! embeddable in a straight-line `Vec<ThreadedStmt>` alongside `Bind`, so
//! `wrap_body_with_nlr_catch`'s `body_doc` can become a real `NlrCatch`'s
//! *rest-of-slice* body the way [`render`]'s `NlrCatch` arm already expects
//! production callers to supply it). That is architecturally a peer of
//! BT-3145's `ConditionalLoop`/`Gensym`/`ValueRef::Doc` gaps (Addendum 2),
//! not a variant of the class-var Bind shape (3) closed — a full
//! `Threaded`/`Bind` sequence can already represent state-only bodies, but
//! `gen_server` method bodies are NOT state-only; ordinary control-flow-free
//! statements have no IR representation today. Closing it is scoped as a
//! design-detour follow-up (mirroring BT-3145's BT-3153 ADR-amendment
//! pattern), not attempted inline here — see the BT-3148 Linear issue for
//! the full investigation trail and source citations.
//!
//! This module lands the IR types, the [`verify`] checker, and the
//! [`lower_and_render`] test shim (BT-3129), the unified `VersionedVar`/
//! `VersionCounter` production path (BT-3131). BT-3132 originally added a
//! per-loop [`ThreadedIr`]-fixture wrapper (`verify_loop_unpack_invariant`)
//! checking `while_loops.rs`'s and `counted_loops.rs`'s (via
//! `control_flow/mod.rs`) direct-params/hybrid loop generators' "optimized
//! mode implies no `StateAcc` unpack" invariant, plus a Phase A0 measurement
//! prototype (`prototype_direct_params_ir`, gated behind
//! `BEAMTALK_THREADED_IR_WHILE=1`); BT-3154 deleted both, since the invariant
//! both were checking already holds structurally —
//! `ThreadingPlan::generate_unpack_at_iteration_start`'s own
//! `if !use_direct_params && !use_hybrid_params` guard means an optimized
//! mode can never reach the unpack-emitting branch in the first place.
//! [`VerifyError::ThreadingModeUnpackMismatch`] and [`verify`]'s general
//! check for it remain, exercised directly by hand-built-IR unit tests.
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
//! loop-unpack duplication BT-3132 closed).
//!
//! As of BT-3134, `conditionals.rs`'s mutation-carrying `ifTrue:`/
//! `ifFalse:`/`ifTrue:ifFalse:`/`ifNotNil:` inliners and
//! `exception_handling.rs`'s `on:do:`/`ensure:` mutation-threading
//! generators each construct and [`verify`] a
//! [`verify_branch_frame_linearity`] `ThreadedIr` fixture — one [`FrameId`]
//! per `with_branch_context` arm (branch bodies for conditionals; try/
//! handler bodies for `on:do:`; try/success-cleanup/error-cleanup bodies
//! for `ensure:`) — checking that sibling arms independently minting the
//! same `StateAcc` version number in disjoint frames never trips
//! [`VerifyError::NonLinearVersion`]. This is the first production call
//! site to exercise [`FrameId`]'s sibling-frame discipline: BT-3132's loop
//! migration never branches, so it never allocated more than one non-root
//! frame per `verify()` call. The rest of these generators' state (field
//! mutations, NLR relay, shadow writes) does not yet construct `ThreadedIr`
//! — later phases (BT-3135 onward) extend coverage.
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

use super::control_flow::StateAccFallbackReason;
use super::document::{Document, join, leaf};
use super::{CoreErlangGenerator, NlrBoundary};
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
/// naming/identity unification only, not IR construction). `Local` gets its
/// own production call site as of BT-3133's
/// [`verify_tuple_acc_unpack_invariant`]. `#[allow(dead_code)]` here
/// documents that the remaining variants stay test-only for now, instead of
/// forcing artificial non-test construction sites.
#[allow(dead_code)]
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
    #[allow(dead_code)] // counted-loop call sites are a later migration, not this pilot
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
    /// An opaque, pre-rendered `Document` — ordinary AST-directed
    /// expression codegen with no state-threading content of its own,
    /// exactly the class of embed [`ThreadedStmt::ConditionalLoop`]'s
    /// `continue_header`/`exit_arm` already use (the `NlrCatch` precedent,
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

    /// A while/counted loop's condition/case-split skeleton (ADR 0111
    /// Addendum 2, Gap 1): `letrec 'fn_name'/N = fun (Params) ->
    /// <continue_header> <body> apply 'fn_name'/N (<final_args>) <exit_arm>
    /// in apply 'fn_name'/N (<outer_args>)`. Unlike bare [`Threaded`](Self::Threaded)
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
        /// Present only for counted loops (`to:do:`/`to:by:do:`/
        /// `timesRepeat:`/`repeat`) — `None` for while/`whileFalse:`. See
        /// [`LoopCounter`]'s doc comment.
        counter: Option<LoopCounter>,
        /// Opaque, AST-directed condition scrutinee + the case's chosen
        /// continue-arm pattern, up to and including `"-> "` — e.g. `"let
        /// CondFun = <cond> in case apply CondFun (Params) of <'true'> when
        /// 'true' -> "`. Built by the SAME condition-codegen call production
        /// already runs — the IR does not re-derive condition semantics.
        /// Sound opacity: the condition body is ordinary AST-directed
        /// expression codegen with no state-threading content of its own
        /// (the `NlrCatch` precedent — see the ADR's §Verifier honesty).
        continue_header: Document<'static>,
        body: Vec<ThreadedStmt>,
        produces: Vec<VersionedVar>,
        /// Opaque exit arm: pattern + exit value + `"end "` — e.g.
        /// `"<'false'> when 'true' -> {'nil', _ExitSA8} end "` (built by
        /// `generate_exit_stateacc`). ORDERING CONSTRAINT (load-bearing for
        /// byte-identity): must be constructed AFTER `body` is lowered — its
        /// `ExitSA` temps share the SAME module-wide `fresh_temp_var`
        /// counter as `body`'s rebind temps, and legacy mints body temps
        /// first. This opacity is NOT sound on `continue_header`'s grounds —
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

    /// BT-3135: replaces the two `gen_server/methods.rs` routing
    /// `debug_assert!`s (`:1258`/`:1450`, pre-BT-3135) as a structural
    /// property: `classify_body_expr`'s upfront classification
    /// (`BodyExprKind::LocalAssignControlFlow` /
    /// `BodyExprKind::ControlFlowWithMutations`) commits a construct to
    /// routing through the shared Actor `threaded_expr.rs` emitter: the
    /// emitter's own downstream `control_flow_has_mutations` recheck MUST
    /// also recognize it (`Ok(Some(_))`/`Ok(true)`), not decline
    /// (`Ok(None)`/`Ok(false)`) and fall through to the generic path. See
    /// [`verify_routing_invariant`].
    RoutingMismatch { at: Span },
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
        ThreadedStmt::Threaded { body, .. } | ThreadedStmt::ConditionalLoop { body, .. } => {
            contains_class_var_nlr_catch(body)
        }
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
            ThreadedStmt::Threaded { body, .. } | ThreadedStmt::ConditionalLoop { body, .. } => {
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
            }
            | ThreadedStmt::ConditionalLoop {
                mode,
                frame,
                body,
                produces,
                span: _,
                ..
            } => {
                // ADR 0111 Addendum 2, Gap 1: `ConditionalLoop` verifies
                // exactly like `Threaded` — push frame/mode, walk body,
                // check_use each `produces` entry, pop. `continue_header`/
                // `exit_arm`/`fn_name`/`counter` are opaque (or caller-
                // supplied, non-threading) fields `verify()` does not, and
                // is not meant to, inspect — see the variant's doc comment.
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
/// Has a real (non-test) production caller as of BT-3145:
/// `control_flow::while_loops`'s `try_render_while_direct_via_threaded_ir`,
/// gated behind `BEAMTALK_THREADED_IR_WHILE_DIRECT=1` — see module docs
/// §Status (BT-3145).
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
                body,
                produces,
                span: _,
            } => docs.push(render_threaded(mode, *frame, body, produces, ctx)),
            ThreadedStmt::NlrCatch { boundary, .. } => {
                let body_doc = render(&ir[i + 1..], ctx);
                docs.push(render_nlr_catch(*boundary, body_doc, ctx));
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
                counter: _counter, // counted-loop rendering: a later migration wires a real counted-loop call site (BT-3145 pilots while-direct only)
                continue_header,
                body,
                produces,
                exit_arm,
                span: _,
            } => docs.push(render_conditional_loop(
                fn_name,
                mode,
                *frame,
                body,
                produces,
                continue_header,
                exit_arm,
                ctx,
            )),
        }
    }
    Document::Vec(docs)
}

/// The pre-BT-3144 skeleton-fidelity test shim signature, kept working per
/// the issue body's point 5: delegates to [`render`] against a throwaway
/// [`CoreErlangGenerator`] (cheap — no I/O) so every existing
/// `lower_and_render(&ir).to_pretty_string()` test call survives verbatim.
///
/// `#[allow(dead_code)]`: unlike [`render`] itself (which now has a real
/// production caller as of BT-3145 — see [`render`]'s doc comment), this
/// shim remains test-only: `while_loops.rs`'s
/// `try_render_while_direct_via_threaded_ir` builds its own
/// [`RenderCtx`] directly against the live generator rather than a
/// throwaway one, so this convenience wrapper has no production caller.
#[allow(dead_code)]
pub(super) fn lower_and_render(ir: &[ThreadedStmt]) -> Document<'static> {
    let mut generator = CoreErlangGenerator::new("__threaded_ir_render_shim");
    let mut ctx = RenderCtx::new(&mut generator);
    render(ir, &mut ctx)
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
/// skeleton (`header_and_exit: None`) and [`ConditionalLoop`](ThreadedStmt::ConditionalLoop)'s
/// real condition/case-split skeleton (`header_and_exit: Some((continue_header,
/// exit_arm))`) share one implementation instead of two near-duplicates
/// (CLAUDE.md's no-duplicate-implementations rule applies within this file,
/// not just across the Rust/Erlang boundary).
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
/// For `ConditionalLoop` (`header_and_exit: Some(..)`), the recursive
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
    header_and_exit: Option<(&Document<'static>, &Document<'static>)>,
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
        let body_doc = if header_and_exit.is_some() {
            render_loop_body_statements(body, ctx)
        } else {
            render(body, ctx)
        };
        let final_args = if header_and_exit.is_some() {
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
        (param_list, body_doc, final_args)
    };
    let (param_list, body_doc, final_args) = match loop_context {
        Some(flags) => ctx.with_loop_context(flags, render_in_loop_body),
        None => render_in_loop_body(ctx),
    };

    match header_and_exit {
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
        Some((continue_header, exit_arm)) => docvec![
            "letrec ",
            leaf::fname(fn_name.clone(), arity),
            " = fun (",
            param_list,
            ") -> ",
            continue_header.clone(),
            body_doc,
            " apply ",
            leaf::fname(fn_name.clone(), arity),
            " (",
            final_args,
            ") ",
            exit_arm.clone(),
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
/// 0111 Addendum 2, Gap 1): delegates to [`render_loop_skeleton`] with the
/// real `continue_header`/`exit_arm` pair, reusing the exact same
/// `param_list`/`outer_args` plumbing the bare loop shape uses.
#[allow(clippy::too_many_arguments)]
fn render_conditional_loop(
    fn_name: &str,
    mode: &ThreadingMode,
    frame: FrameId,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    continue_header: &Document<'static>,
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
        Some((continue_header, exit_arm)),
    )
}

/// Skeleton-fidelity rendering of [`ThreadedStmt::TupleAccUnpack`], mirroring
/// `generate_tuple_unpack_docs`'s `let V = call 'erlang':'element'(idx, Param)
/// in` chain — `idx` starts at `gate_slots + 1` (1-based, past the leading
/// gate slots) for the first target. Already full-fidelity (no generator
/// context needed): this shape was real before BT-3144, unchanged here.
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

fn render_value(value: &ValueRef, ctx: &RenderCtx) -> Document<'static> {
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

/// Full-fidelity NLR try/catch scaffolding: allocates a fresh token var
/// (matching every real call site — `self.fresh_temp_var("NlrToken")`,
/// e.g. `actor_codegen.rs:544`, `gen_server/methods.rs:306`) and reuses
/// [`CoreErlangGenerator::wrap_body_with_nlr_catch`] verbatim — the exact
/// function every real NLR try/catch in the codebase already goes through
/// (module docs on [`ThreadedStmt::NlrCatch`]: "the true call site
/// `ThreadedStmt::NlrCatch` faithfully models"). Zero re-derivation, so
/// this can never drift from production's try/catch shape.
fn render_nlr_catch(
    boundary: NlrBoundary,
    body_doc: Document<'static>,
    ctx: &mut RenderCtx,
) -> Document<'static> {
    let token_var = ctx.fresh_temp_var("NlrToken");
    ctx.generator
        .wrap_body_with_nlr_catch(body_doc, &token_var, boundary)
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
/// `TupleAcc` mode at all — `fallback_reason` names why (the same pattern
/// `check_loop_unpack_invariant`'s deleted `mode` parameter used: the
/// generator's own already-resolved decision, not a re-derivation). Class 1
/// (`TupleAccUnpackModeMismatch`) has real teeth: `use_tuple_acc` is an
/// externally-supplied bool that genuinely varies per call site, so this
/// guards against `generate_tuple_unpack_docs` being reached from a
/// `StateAcc`-mode code path.
///
/// `gate_slots` is `index_offset - 1`, the caller's own already-computed
/// tuple-shape parameter (`1` for `do:`/`dict do:`'s `index_offset: 1`, `2`
/// for `collect:`/boolean-predicate ops' `index_offset: 2`, `3` for
/// `takeWhile:`/`dropWhile:`/`detect:`-family's `index_offset: 3`). **Class 4
/// (`EarlyExitGateSlotMismatch`) is currently a tautology, not a live check**:
/// both the synthesized `ThreadingMode::TupleAcc(gate_slots)` and the
/// `TupleAccUnpack` node's own `gate_slots` are built from this single
/// `gate_slots` argument below, so they cannot disagree at any real call
/// site — only the hand-built-IR unit test
/// (`verify_early_exit_gate_slot_mismatch_fires_when_node_disagrees_with_mode`)
/// exercises the mismatch branch by deliberately diverging them. Unlike
/// class 1, there is today no second, independently-derived source for
/// `gate_slots` to disagree with (e.g. resolved from the op selector/kind
/// rather than threaded through as the caller's `index_offset`), so this
/// currently exists as structural scaffolding for a future independent
/// derivation, not as a regression guard against an op-family miscopy.
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

// ─── Branch-frame linearity check (BT-3134) ────────────────────────────────

/// Builds a minimal `ThreadedIr` fixture modeling N sibling
/// `with_branch_context` arms — `ifTrue:ifFalse:`'s two branches,
/// `on:do:`'s try/handler bodies, `ensure:`'s try/success-cleanup/
/// error-cleanup bodies — each its own [`FrameId`], and [`verify`]s
/// branch-frame version linearity across them.
///
/// Each arm mints a `StateAcc` `Bind` chain from version 0 (the frame's
/// entry parameter — `StateAcc`, bound by the caller from the outer
/// state before entering the arm) up to `final_version`
/// (`state_version()` reached at the end of
/// `generate_conditional_branch_inline`/
/// `generate_exception_body_with_threading`'s `with_branch_context`
/// call), mirroring the real generator's sequential per-mutation
/// `StateAcc{N}` rebind. `arms` carries each arm's already-allocated
/// `FrameId` alongside its `final_version` — distinct arms are distinct
/// frames BY CONSTRUCTION, so two sibling arms that reach the SAME
/// `final_version` (e.g. both `ifTrue:`/`ifFalse:` bodies perform exactly
/// one field mutation, each independently producing `StateAcc1` in their
/// own frame) must NOT trip [`VerifyError::NonLinearVersion`] — this is
/// the ADR 0111 §The IR frame-identity requirement (see [`FrameId`]'s doc
/// comment).
///
/// **Scaffolding, not yet a live regression guard**: because the caller
/// ([`super::control_flow::CoreErlangGenerator::check_branch_frame_linearity`])
/// always allocates a fresh, distinct `FrameId` per arm and this function
/// only ever synthesizes a `Bind` chain from a scalar `final_version` count
/// (not the real per-arm mutation sequence the generator produced), no two
/// arms can ever collide at any of today's nine call sites (BT-3134's
/// original six plus BT-3139's three) — this smoke-tests
/// the verifier's `FrameId`/linearity plumbing (and pins the acceptance
/// criterion above), but cannot yet catch a real generator bug. Giving it
/// that teeth requires threading the real per-arm `Bind` producers through,
/// left to BT-3135+ as it migrates the mutation-`Bind` emission sites
/// themselves onto `ThreadedIr`.
pub(super) fn verify_branch_frame_linearity(
    arms: &[(FrameId, usize)],
    span: Span,
) -> Vec<VerifyError> {
    let mut ir = Vec::with_capacity(arms.len());
    for &(frame, final_version) in arms {
        let mut body = Vec::with_capacity(final_version);
        for v in 1..=final_version {
            let source = VersionedVar::new(VersionPrefix::State, v - 1, frame);
            let target = VersionedVar::new(VersionPrefix::State, v, frame);
            body.push(ThreadedStmt::Bind {
                target,
                source,
                op: BindOp::Direct(ValueRef::Literal("'_'")),
                shadow_write: false,
                span,
            });
        }
        let produces = if final_version > 0 {
            vec![VersionedVar::new(
                VersionPrefix::State,
                final_version,
                frame,
            )]
        } else {
            Vec::new()
        };
        ir.push(ThreadedStmt::Threaded {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
            frame,
            body,
            produces,
            span,
        });
    }
    verify(&ir)
}

// ─── Routing invariant check (BT-3135) ─────────────────────────────────────

/// Replaces the two "classifier and emitter agree on Actor-threaded routing"
/// `debug_assert!`s (`gen_server/methods.rs:1258`/`:1450`, pre-BT-3135) as a
/// structural check: `gen_server/methods.rs`'s upfront `classify_body_expr`
/// commits to `BodyExprKind::LocalAssignControlFlow` /
/// `BodyExprKind::ControlFlowWithMutations` — i.e. that this construct routes
/// through the shared Actor [`super::threaded_expr`] emitter
/// (`emit_threaded_assign_rhs`/`emit_threaded_last`) — for exactly the
/// expressions where [`super::CoreErlangGenerator::control_flow_has_mutations`]
/// returns `true`. The downstream emitter re-checks the same predicate
/// (`threaded_expr.rs`'s `lower_actor_threaded_last`/
/// `emit_actor_threaded_assign_rhs`) against generator state that may have
/// advanced since classification ran (Phase 1 classifies the whole body
/// upfront; Phase 2 emits statement-by-statement, mutating `self` as it
/// goes) — a drift between the two is exactly the "two independently
/// computed decisions must agree" shape ADR 0111 §Current state names for
/// both routing asserts.
///
/// `routed` is the emitter's own `Ok(Some(_))`/`Ok(Some(true))` outcome —
/// this function checks what was *actually observed*, not a re-derivation of
/// `control_flow_has_mutations` (ADR 0111 §Verifier honesty: a check
/// comparing the generator against itself is silent when both are
/// consistently wrong).
pub(super) fn verify_routing_invariant(routed: bool, span: Span) -> Vec<VerifyError> {
    if routed {
        Vec::new()
    } else {
        vec![VerifyError::RoutingMismatch { at: span }]
    }
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
/// `at_method_top_frame` selects the `Bind`'s (and the marker's) `FrameId`:
/// [`FrameId::ROOT`] when `true`, a nested [`FrameId`] otherwise — mirroring
/// [`VerifyError::ShadowWriteMissing`]'s own `target.frame == FrameId::ROOT`
/// gate. **This is deliberately NOT `self.current_nlr_token().is_some()`**
/// (whether *this* method's own body happens to contain a literal `^` inside
/// one of its own block literals, gating `wrap_class_method_body_with_nlr_catch`)
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
/// re-derived* top-frame signal (`self.block_depth == 0`, read fresh from
/// live generator state — not reused from whatever `shadow_write` value a
/// future regression might compute wrong), matching production's actual
/// gate 1:1 (ADR 0111 §Verifier honesty: comparing the generator against
/// itself is silent when both are consistently wrong).
///
/// The `dispatch_codegen.rs` rebind site passes `false`/`false` unconditionally
/// — not a claim about its real block-depth, but a deliberate exemption: that
/// `Bind` structurally never carries its own shadow-write obligation
/// regardless of nesting (see its own call-site comment), so it is modeled
/// as never sitting at the frame this check inspects.
pub(super) fn construct_and_verify_class_var_bind(
    op: BindOp,
    shadow_write: bool,
    at_method_top_frame: bool,
    source_version: usize,
    target_version: usize,
    span: Span,
) -> (ThreadedStmt, Vec<VerifyError>) {
    let frame = if at_method_top_frame {
        FrameId::ROOT
    } else {
        FrameId::new(1)
    };
    let mut body = Vec::with_capacity(source_version + 1);
    for v in 1..=source_version {
        body.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::ClassVars, v, frame),
            source: VersionedVar::new(VersionPrefix::ClassVars, v - 1, frame),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            // `shadow_write: true` here is a backfill-scaffolding
            // assumption, not a claim about the earlier mutation's real
            // shape (this fixture never inspects it): only the LAST Bind
            // below — the one this call site is actually about to
            // render — is what `ShadowWriteMissing` is checking. Backfilling
            // `false` would spuriously flag every earlier synthetic step at
            // `FrameId::ROOT`, since `has_class_vars_nlr` is unconditionally
            // true here (the synthetic marker below).
            shadow_write: true,
            span,
        });
    }
    let bind = ThreadedStmt::Bind {
        target: VersionedVar::new(VersionPrefix::ClassVars, target_version, frame),
        source: VersionedVar::new(VersionPrefix::ClassVars, source_version, frame),
        op,
        shadow_write,
        span,
    };
    body.push(bind.clone());
    let marker = ThreadedStmt::NlrCatch {
        boundary: NlrBoundary::ClassMethod {
            has_class_vars: true,
        },
        token: TokenId::new(0),
        frame,
        span,
    };
    // `verify()` seeds its frame stack with just `[FrameId::ROOT]` (module
    // docs on `FrameId`) — when `frame == FrameId::ROOT` (`at_method_top_frame`),
    // `body`'s Binds are already reachable at the top level with no wrapper.
    // Otherwise `frame` must be PUSHED via a `Threaded` node, or
    // `VerifyWalk::check_use`'s frame-flow rule can never find a backfilled
    // version `>0` at that frame (a bare top-level `Bind`/`NlrCatch` never
    // pushes anything) — this is exactly the gap that produced a spurious
    // `UnboundVersion` before BT-3148 added real backfill history here (a
    // nested class-var rebind is always `at_method_top_frame: false`, so it
    // always took this branch once `source_version > 0`).
    let fixture: Vec<ThreadedStmt> = if at_method_top_frame {
        let mut f = body;
        f.push(marker);
        f
    } else {
        vec![
            ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                body,
                produces: Vec::new(),
                span,
            },
            marker,
        ]
    };
    (bind, verify(&fixture))
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
/// machinery (the synthetic `NlrCatch` marker, `at_method_top_frame`'s
/// `FrameId` selection) — `Self{N}`/`State{N}` mutations never carry that
/// ADR 0110 obligation. This helper is handed the *actual* version numbers,
/// which may already be arbitrarily large after earlier mutations in the
/// same method. [`VerifyWalk::check_use`]'s
/// frame-flow rule requires every version `>0` referenced as a `Bind`'s
/// source to have a producing `Bind` visible on the frame stack — so without
/// backfilling that history, every mutation past a method's first would
/// spuriously fail `UnboundVersion` (its `source_version` would have no
/// producer in an isolated single-`Bind` fixture). The backfill chain
/// (`1..=source_version`) is exactly [`verify_branch_frame_linearity`]'s own
/// technique, generalized here to an arbitrary prefix and reused rather than
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
    let mut ir = Vec::with_capacity(source_version + 1);
    for v in 1..=source_version {
        ir.push(ThreadedStmt::Bind {
            target: VersionedVar::new(prefix.clone(), v, frame),
            source: VersionedVar::new(prefix.clone(), v - 1, frame),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            shadow_write: false,
            span,
        });
    }
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
        let frame = FrameId::new(1);
        let sum_source = local("sum", 0, frame);
        let count_source = local("count", 0, frame);
        let ir = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::DirectParams,
            frame,
            body: vec![
                ThreadedStmt::Bind {
                    target: local("sum", 1, frame),
                    source: sum_source.clone(),
                    op: BindOp::Direct(ValueRef::Version(sum_source)),
                    shadow_write: false,
                    span: span(),
                },
                ThreadedStmt::Bind {
                    target: local("count", 1, frame),
                    source: count_source.clone(),
                    op: BindOp::Direct(ValueRef::Version(count_source)),
                    shadow_write: false,
                    span: span(),
                },
            ],
            produces: vec![local("sum", 1, frame), local("count", 1, frame)],
            span: span(),
        }];
        assert_eq!(verify(&ir), Vec::new());
    }

    #[test]
    fn verify_silent_on_well_formed_conditional_loop_fixture() {
        // ADR 0111 Addendum 2, Gap 1: `verify()` treats `ConditionalLoop`
        // exactly like `Threaded` — push frame/mode, walk body, check_use
        // each `produces` entry, pop — no new `VerifyError` variant. Opaque
        // fields (`continue_header`/`exit_arm`) and caller-supplied,
        // non-threading fields (`fn_name`/`counter`) are not, and cannot be,
        // inspected — see the variant's doc comment.
        let frame = FrameId::new(1);
        let sum_v0 = local("sum", 0, frame);
        let sum_v1 = VersionedVar::new(VersionPrefix::Gensym("_Sum7".to_string()), 1, frame);
        let ir = vec![ThreadedStmt::ConditionalLoop {
            fn_name: "while".to_string(),
            mode: ThreadingMode::DirectParams,
            frame,
            counter: None,
            continue_header: Document::Str("<opaque condition>"),
            body: vec![ThreadedStmt::Bind {
                target: sum_v1,
                source: sum_v0.clone(),
                op: BindOp::Direct(ValueRef::Doc(Document::Str("<opaque rhs>"))),
                shadow_write: false,
                span: span(),
            }],
            produces: vec![sum_v0],
            exit_arm: Document::Str("<opaque exit>"),
            span: span(),
        }];
        assert_eq!(verify(&ir), Vec::new());
    }

    #[test]
    fn verify_unbound_version_conditional_loop_frame_flow_matches_threaded() {
        // ConditionalLoop's frame is pushed/popped exactly like Threaded's —
        // a Bind referencing a version from a frame never entered is still
        // caught.
        let frame = FrameId::new(1);
        let stray_frame = FrameId::new(99);
        let ir = vec![ThreadedStmt::ConditionalLoop {
            fn_name: "while".to_string(),
            mode: ThreadingMode::DirectParams,
            frame,
            counter: None,
            continue_header: Document::Str("<opaque condition>"),
            body: vec![ThreadedStmt::Bind {
                target: local("sum", 1, frame),
                source: class_var(1, stray_frame),
                op: BindOp::Direct(ValueRef::Doc(Document::Str("<opaque rhs>"))),
                shadow_write: false,
                span: span(),
            }],
            produces: vec![local("sum", 1, frame)],
            exit_arm: Document::Str("<opaque exit>"),
            span: span(),
        }];
        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::UnboundVersion { var, .. } if var.frame == stray_frame
            )),
            "expected UnboundVersion for the out-of-scope frame, got: {errors:?}"
        );
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
    fn lower_and_render_threaded_direct_params_emits_real_letrec() {
        // BT-3144: `Threaded { mode: DirectParams, .. }` now renders a real
        // `letrec` (fresh-named via `fresh_temp_var("Loop")`, hence the
        // `_Loop1` — see `VariableContext::fresh_var`), not a flattened body
        // — the pre-BT-3144 skeleton behavior this test used to pin.
        let frame = FrameId::new(1);
        let sum_source = local("sum", 0, frame);
        let ir = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::DirectParams,
            frame,
            body: vec![ThreadedStmt::Bind {
                target: local("sum", 1, frame),
                source: sum_source.clone(),
                op: BindOp::Direct(ValueRef::Version(sum_source)),
                shadow_write: false,
                span: span(),
            }],
            produces: vec![local("sum", 1, frame)],
            span: span(),
        }];
        let rendered = lower_and_render(&ir).to_pretty_string();
        assert_eq!(
            rendered,
            "letrec '_Loop1'/1 = fun (Sum) -> let Sum1 = Sum in \
             apply '_Loop1'/1 (Sum1) in apply '_Loop1'/1 (Sum)"
        );
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

    // ── verify_branch_frame_linearity (BT-3134) ──────────────────────────
    // Pins the production check behind `ifTrue:ifFalse:`'s two branches,
    // `on:do:`'s try/handler bodies, and `ensure:`'s try/success-cleanup/
    // error-cleanup bodies.

    #[test]
    fn verify_branch_frame_linearity_silent_when_arms_are_empty() {
        assert_eq!(verify_branch_frame_linearity(&[], span()), Vec::new());
    }

    #[test]
    fn verify_branch_frame_linearity_silent_for_single_arm_with_mutations() {
        // One arm, three mutations (final_version = 3) — a plain linear
        // Bind chain within one frame, no sibling to interact with.
        let arms = [(FrameId::new(1), 3)];
        assert_eq!(verify_branch_frame_linearity(&arms, span()), Vec::new());
    }

    #[test]
    fn verify_branch_frame_linearity_silent_for_arms_with_no_mutations() {
        // Both branches of `ifTrue:ifFalse:` performed zero field mutations
        // (final_version = 0 — nothing to bind, `produces` stays empty).
        let arms = [(FrameId::new(1), 0), (FrameId::new(2), 0)];
        assert_eq!(verify_branch_frame_linearity(&arms, span()), Vec::new());
    }

    #[test]
    fn verify_branch_frame_linearity_sibling_arms_same_version_do_not_trip_non_linear() {
        // THE acceptance-criteria case: `ifTrue:` and `ifFalse:` each
        // perform exactly one field mutation, so BOTH sibling arms produce
        // "State1" — but in disjoint frames. Frame identity must keep these
        // from being counted as two producers of the same VersionedVar.
        let true_arm = FrameId::new(1);
        let false_arm = FrameId::new(2);
        let arms = [(true_arm, 1), (false_arm, 1)];
        assert_eq!(
            verify_branch_frame_linearity(&arms, span()),
            Vec::new(),
            "sibling arms independently minting State1 in disjoint frames must not report \
             NonLinearVersion"
        );
    }

    #[test]
    fn verify_branch_frame_linearity_three_sibling_arms_same_version() {
        // `ensure:`'s shape: try body, success-path cleanup, error-path
        // cleanup — three sibling arms (not just two), each reaching the
        // same final_version.
        let arms = [
            (FrameId::new(1), 2),
            (FrameId::new(2), 2),
            (FrameId::new(3), 2),
        ];
        assert_eq!(verify_branch_frame_linearity(&arms, span()), Vec::new());
    }

    #[test]
    fn verify_branch_frame_linearity_sibling_arms_different_versions_still_silent() {
        // Sibling arms need not reach the same final_version at all — e.g.
        // the try body does two mutations while the handler does none.
        let arms = [(FrameId::new(1), 2), (FrameId::new(2), 0)];
        assert_eq!(verify_branch_frame_linearity(&arms, span()), Vec::new());
    }

    #[test]
    fn verify_branch_frame_linearity_detects_genuine_cross_frame_leak() {
        // Negative control: if a hypothetical future bug had a "sibling" arm
        // reuse an ANCESTOR frame's FrameId instead of allocating its own,
        // the two arms' Bind chains would collide as the same VersionedVar
        // sequence, in the same frame, and NonLinearVersion must fire. This
        // pins that the check is not vacuously silent for every input.
        let shared_frame = FrameId::new(1);
        let arms = [(shared_frame, 1), (shared_frame, 1)];
        let errors = verify_branch_frame_linearity(&arms, span());
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::NonLinearVersion { var, producers: 2, .. }
                    if *var == VersionedVar::new(VersionPrefix::State, 1, shared_frame)
            )),
            "expected NonLinearVersion for the colliding shared-frame State1, got: {errors:?}"
        );
    }
    // ── verify_routing_invariant (BT-3135) ───────────────────────────────
    // Pins the production replacement for `gen_server/methods.rs`'s two
    // deleted routing `debug_assert!`s.

    #[test]
    fn verify_routing_invariant_silent_when_routed() {
        assert_eq!(verify_routing_invariant(true, span()), Vec::new());
    }

    #[test]
    fn verify_routing_invariant_fires_when_not_routed() {
        let errors = verify_routing_invariant(false, span());
        assert_eq!(
            errors,
            vec![VerifyError::RoutingMismatch { at: span() }],
            "expected RoutingMismatch, got: {errors:?}"
        );
    }

    // ── construct_and_verify_class_var_bind (BT-3135/BT-3148, ADR 0110
    // contract) ─────────────────────────────────────────────────────────
    // Pins the production replacement/extension covering the two Bind-
    // emission sites named in ADR 0111 §Phase D:
    // `expressions.rs::generate_field_assignment` (Put) and
    // `dispatch_codegen.rs::emit_class_var_result_unwrap` (Direct rebind).

    fn put_op() -> BindOp {
        BindOp::Put {
            field: "runs".to_string(),
            value: ValueRef::Var("_Val0".to_string()),
            class_tag: ValueRef::Var("ClassSelf".to_string()),
        }
    }

    #[test]
    fn construct_and_verify_class_var_bind_put_silent_with_shadow_write() {
        // generate_field_assignment's real post-ADR-0110 shape: block_depth
        // == 0 (shadow_write: true), first mutation in the method
        // (source_version: 0, target_version: 1).
        let (bind, errors) =
            construct_and_verify_class_var_bind(put_op(), true, true, 0, 1, span());
        assert_eq!(errors, Vec::new());
        assert_eq!(
            bind,
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::ClassVars, 1, FrameId::ROOT),
                source: VersionedVar::new(VersionPrefix::ClassVars, 0, FrameId::ROOT),
                op: put_op(),
                shadow_write: true,
                span: span(),
            },
            "the returned Bind must be the exact node a caller renders — no second shape"
        );
    }

    #[test]
    fn construct_and_verify_class_var_bind_put_fires_when_shadow_write_missing_at_top_frame() {
        // The regression this exists to catch: block_depth == 0 (a top-frame
        // mutation) but the shadow write was forgotten.
        let (_, errors) = construct_and_verify_class_var_bind(put_op(), false, true, 0, 1, span());
        assert_eq!(
            errors,
            vec![VerifyError::ShadowWriteMissing {
                mutated: VersionedVar::new(VersionPrefix::ClassVars, 1, FrameId::ROOT),
                at: span(),
            }],
            "expected ShadowWriteMissing, got: {errors:?}"
        );
    }

    #[test]
    fn construct_and_verify_class_var_bind_put_fires_even_without_a_local_nlr_catch_in_this_method()
    {
        // Pins the exact ADR 0110 repro shape, not just the ADR's abbreviated
        // worked example: `CollectionDriver countedRun:over:` mutates a class
        // var, then invokes a *caller-supplied* block (`aBlock value: x`)
        // that contains no literal `^` of its own — `has_block_nlr_or_walk`
        // is `false` for that method, so codegen allocates it NO local NLR
        // try/catch at all (`self.current_nlr_token()` is `None` throughout
        // its body). The foreign `^` still relays, caught one layer out by
        // `apply_class_method_fun/6`'s unconditional `?IS_NLR` clause — so
        // `at_method_top_frame` must be driven by `block_depth == 0` alone,
        // NEVER by whether this method happens to have its own NLR catch:
        // gating on the latter (an earlier version of this helper's bug)
        // would silently exempt this exact shape from the check.
        let (_, errors) = construct_and_verify_class_var_bind(put_op(), false, true, 0, 1, span());
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, VerifyError::ShadowWriteMissing { .. })),
            "expected ShadowWriteMissing even with no local NLR catch, got: {errors:?}"
        );
    }

    #[test]
    fn construct_and_verify_class_var_bind_put_silent_below_top_frame() {
        // A class-var mutation inside a nested block (block_depth > 0) is
        // legitimately shadow_write: false — already discarded on normal
        // return (BT-1550), not a regression. Matches
        // `verify_shadow_write_missing_silent_below_top_frame`.
        let (_, errors) = construct_and_verify_class_var_bind(put_op(), false, false, 0, 1, span());
        assert_eq!(errors, Vec::new());
    }

    #[test]
    fn construct_and_verify_class_var_bind_direct_rebind_silent_never_requires_shadow_write() {
        // dispatch_codegen.rs's inherited-self-dispatch rebind: never itself
        // a shadow-write producer (the callee's own Bind already wrote it
        // under the same ClassSelf-tagged key) — its call site always passes
        // at_method_top_frame: false, so this stays silent unconditionally.
        let op = BindOp::Direct(ValueRef::Var("_CV0".to_string()));
        let (_, errors) = construct_and_verify_class_var_bind(op, false, false, 0, 1, span());
        assert_eq!(errors, Vec::new());
    }

    #[test]
    fn construct_and_verify_class_var_bind_direct_rebind_silent_with_a_nonzero_source_version() {
        // Regression for a real BT-3148 bug caught by this migration's own
        // tests: `at_method_top_frame: false` selects a non-`ROOT` `FrameId`,
        // which `verify()`'s frame stack (seeded as `[FrameId::ROOT]` only)
        // cannot see without an explicit `Threaded` wrapper — a nested
        // class-var rebind (`emit_class_var_result_unwrap`) is ALWAYS
        // `at_method_top_frame: false`, and its `source_version` is the real,
        // possibly-nonzero `class_var_version()` (e.g. an earlier top-frame
        // mutation already minted `ClassVars1` before this nested rebind
        // runs) — this must stay silent, not spuriously report
        // `UnboundVersion` for the backfilled version.
        let op = BindOp::Direct(ValueRef::Var("_CV3".to_string()));
        let (bind, errors) = construct_and_verify_class_var_bind(op, false, false, 2, 3, span());
        assert_eq!(errors, Vec::new(), "got: {errors:?}");
        assert_eq!(
            bind,
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::ClassVars, 3, FrameId::new(1)),
                source: VersionedVar::new(VersionPrefix::ClassVars, 2, FrameId::new(1)),
                op: BindOp::Direct(ValueRef::Var("_CV3".to_string())),
                shadow_write: false,
                span: span(),
            }
        );
    }

    #[test]
    fn construct_and_verify_class_var_bind_uses_the_real_version_numbers_not_a_fixed_0_to_1_step() {
        // BT-3148: the whole point of the migration off the deleted
        // `verify_class_var_bind` — a mutation later in the method (source
        // version 3, minted to 4) must be silent, not spuriously flagged,
        // and the returned Bind must carry those exact versions (never the
        // old fixture's hardcoded 0 -> 1).
        let (bind, errors) =
            construct_and_verify_class_var_bind(put_op(), true, true, 3, 4, span());
        assert_eq!(errors, Vec::new(), "got: {errors:?}");
        assert_eq!(
            bind,
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::ClassVars, 4, FrameId::ROOT),
                source: VersionedVar::new(VersionPrefix::ClassVars, 3, FrameId::ROOT),
                op: put_op(),
                shadow_write: true,
                span: span(),
            }
        );
    }

    // ── verify_simple_bind (BT-3139) ──────────────────────────────────────
    // Pins the new coverage for `generate_field_assignment`'s two previously
    // uninstrumented sibling branches (`Self{N}`/`State{N}`), which — unlike
    // the class-var branch — construct zero `ThreadedIr` fixture on `main`
    // today.

    #[test]
    fn verify_simple_bind_silent_on_a_method_first_mutation() {
        // The first `self.field := ...`/actor state mutation in a method:
        // source_version == 0 (the bare `Self`/`State` parameter, always
        // bound), target_version == 1.
        assert_eq!(
            verify_simple_bind(VersionPrefix::SelfVt, 0, 1, span()),
            Vec::new()
        );
        assert_eq!(
            verify_simple_bind(VersionPrefix::State, 0, 1, span()),
            Vec::new()
        );
    }

    #[test]
    fn verify_simple_bind_silent_after_several_prior_mutations() {
        // A later mutation in the same method (source_version > 0) must not
        // spuriously fire UnboundVersion — the whole point of this helper's
        // (and, as of BT-3148, `construct_and_verify_class_var_bind`'s)
        // backfill chain.
        assert_eq!(
            verify_simple_bind(VersionPrefix::SelfVt, 4, 5, span()),
            Vec::new()
        );
        assert_eq!(
            verify_simple_bind(VersionPrefix::State, 7, 8, span()),
            Vec::new()
        );
    }

    #[test]
    fn verify_simple_bind_fires_when_target_reuses_an_already_minted_version() {
        // A counter bug that re-mints a version already reached earlier
        // *within this single call's* backfilled history (instead of
        // advancing past it) is a genuine NonLinearVersion collision — this
        // is the within-call shape the backfill chain actually catches (see
        // `verify_simple_bind`'s doc comment's "Scope, honestly stated"
        // section for how this differs from the cross-call BT-3131 shape).
        let errors = verify_simple_bind(VersionPrefix::SelfVt, 2, 1, span());
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::NonLinearVersion { var, producers: 2, .. }
                    if *var == VersionedVar::new(VersionPrefix::SelfVt, 1, FrameId::ROOT)
            )),
            "expected a NonLinearVersion collision on version 1, got: {errors:?}"
        );
    }

    /// Demonstrates that `verify()` itself *would* catch the historical
    /// BT-3131 `self_version` bug shape (`with_branch_context` briefly reset
    /// `self_version` to 0 on branch entry instead of inheriting the outer
    /// version — fixed in commit `d436ad7`, "Fix `self_version` stale-read
    /// regression from branch-entry reset") *if* the two mutation call
    /// sites' `Bind`s were accumulated into one shared history before
    /// verifying — a `self.field := ...` immediately preceding a branch
    /// (minting `Self1`) followed by another `self.field := ...`
    /// immediately *inside* the branch would, under the reset-to-0 policy,
    /// also compute `source_version = 0, target_version = 1`, re-minting the
    /// SAME `Self1` a second time, which `verify()` reports as
    /// `NonLinearVersion`.
    ///
    /// **This is NOT a regression test for `check_simple_field_bind_invariant`
    /// / `verify_simple_bind`'s real production wiring** — see
    /// `verify_simple_bind`'s doc comment's "Scope, honestly stated" section.
    /// That wiring verifies each mutation site in isolation with no
    /// accumulated method-wide history, so it would NOT catch this exact
    /// bug shape today; this test only proves the underlying `verify()`
    /// primitive is capable of it, motivating the follow-up to thread real
    /// history through.
    #[test]
    fn verify_would_catch_the_bt_3131_regression_shape_given_accumulated_history() {
        let span = span();
        // outer: self.x := 1, before the branch (correct: source 0, target 1).
        let mut ir = verify_bind_ir_for_test(VersionPrefix::SelfVt, 0, 1, span);
        // buggy: self.y := 2, immediately inside a `with_branch_context` arm
        // whose entry wrongly reset `self_version` to 0 instead of
        // inheriting the outer value of 1 — recomputes source 0, target 1.
        ir.extend(verify_bind_ir_for_test(VersionPrefix::SelfVt, 0, 1, span));

        let errors = verify(&ir);
        assert!(
            errors.iter().any(|e| matches!(
                e,
                VerifyError::NonLinearVersion { var, producers: 2, .. }
                    if *var == VersionedVar::new(VersionPrefix::SelfVt, 1, FrameId::ROOT)
            )),
            "expected the reset-on-entry bug to collide on Self1, got: {errors:?}"
        );
    }

    /// Builds the same single-`Bind` IR fragment `verify_simple_bind` would
    /// verify in isolation, but without calling `verify` itself — lets the
    /// BT-3131 regression test above accumulate two call sites' fixtures
    /// into one shared history before verifying, exactly as two real
    /// `generate_field_assignment` call sites in the same method would both
    /// contribute `Bind`s toward the same method-wide `Self{N}` sequence.
    fn verify_bind_ir_for_test(
        prefix: VersionPrefix,
        source_version: usize,
        target_version: usize,
        span: Span,
    ) -> Vec<ThreadedStmt> {
        vec![ThreadedStmt::Bind {
            target: VersionedVar::new(prefix.clone(), target_version, FrameId::ROOT),
            source: VersionedVar::new(prefix, source_version, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            shadow_write: false,
            span,
        }]
    }

    // ── Dual-run byte-parity harness (BT-3144) ───────────────────────────
    //
    // The issue's acceptance criterion: "Dual-run harness proves byte
    // parity for at least direct-params and hybrid while loops against the
    // legacy path." `ThreadedStmt::Threaded` does not model a loop's
    // condition/exit-case (that stays AST-directed — ADR 0111 §Constraints,
    // "everything else in codegen stays AST-directed and unaffected"), so
    // there is no single production function today that emits *only* the
    // letrec skeleton these tests check — `while_loops.rs`'s
    // `generate_while_loop_direct`/`generate_while_loop_hybrid` interleave
    // it with condition codegen. Each test below therefore hand-authors
    // that skeleton directly against a real `CoreErlangGenerator` — via the
    // SAME generator accessors `render`/`RenderCtx` themselves call
    // (`fresh_temp_var`, `current_state_var`/`next_state_var`) wherever a
    // production twin exists, never a re-derivation of the decision itself
    // — and asserts `render(lower(..))`'s output against it byte-for-byte
    // on a separately-constructed but identically-seeded generator. This is
    // the harness's own proof that its rendering mechanism reproduces
    // hand-authored `docvec!` output exactly; BT-3145 (the pilot migration)
    // is what first runs it against a real, refactored-to-be-swappable
    // legacy call site.

    #[test]
    #[allow(clippy::too_many_lines)] // hand-authored dual-run fixture mirroring generate_while_loop_direct's real shape, ADR 0111 Addendum 2
    fn dual_run_conditional_loop_direct_params_byte_parity() {
        // ADR 0111 Addendum 2, Gap 1's own closing instruction: hand-author
        // the REAL condition/case-split shape `generate_while_loop_direct`
        // actually emits (`while_loops.rs:287-406`) — not the condition-free
        // skeleton this test checked pre-Addendum-2 — proving `render()`'s
        // `ConditionalLoop` arm reproduces production's real output
        // byte-for-byte, closing the exact honesty gap this module's own
        // §Status doc names ("no single production function today emits
        // only the letrec skeleton these tests check").
        //
        // Condition/RHS bodies are deliberately trivial, mint-free fragments
        // (not real binop codegen, which mints its own internal temps and is
        // separately tested elsewhere) — this test's job is proving the
        // SKELETON and the GENSYM'D NAMING match, not re-verifying
        // expression codegen. Both concerns (Gap 1's shape, Gap 2's naming)
        // are exercised together: `CondFun`/`Sum`-rebind/`ExitSA` are all
        // minted via the SAME `fresh_temp_var` calls production makes, IN
        // THE SAME ORDER (condition first, then the body's rebind, then the
        // exit arm's `ExitSA` chain LAST) — an inverted order would still
        // pass this assertion by accident only if both sides inverted
        // identically, which is exactly why `legacy_gen`/`render_gen` mint
        // independently, on separately-seeded generators, rather than
        // sharing one generator's counter.
        let mut legacy_gen = CoreErlangGenerator::new("dual_run_conditional_loop_direct");
        let cond_var = legacy_gen.fresh_temp_var("CondFun"); // mint #1: CondFun, matching `while_loops.rs:310`
        let cond_doc = docvec![
            "call 'erlang':'<'(",
            leaf::var("Sum"),
            ", ",
            leaf::int_lit(10),
            ")",
        ];
        let continue_header = docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (",
            leaf::var("Sum"),
            ") -> ",
            cond_doc,
            " in case apply ",
            leaf::var(cond_var),
            " (",
            leaf::var("Sum"),
            ") of ",
            "<'true'> when 'true' -> ",
        ];
        let sum_rebind = legacy_gen.fresh_temp_var("Sum"); // mint #2: body rebind, matching `generate_direct_var_update_in_loop`
        let body = docvec![
            "let ",
            leaf::var(sum_rebind.clone()),
            " = call 'erlang':'+'(",
            leaf::var("Sum"),
            ", ",
            leaf::int_lit(1),
            ") in ",
        ];
        let exit_sa = legacy_gen.fresh_temp_var("ExitSA"); // mint #3: exit arm, LAST — matching `generate_exit_stateacc`
        let exit_arm = docvec![
            "<'false'> when 'true' -> let ",
            leaf::var(exit_sa.clone()),
            " = call 'maps':'put'(",
            leaf::atom("__local__sum"),
            ", ",
            leaf::var("Sum"),
            ", State) in {'nil', ",
            leaf::var(exit_sa),
            "} end ",
        ];
        let legacy_doc = docvec![
            "letrec ",
            leaf::fname("while", 1),
            " = fun (",
            leaf::var("Sum"),
            ") -> ",
            continue_header,
            body,
            " apply ",
            leaf::fname("while", 1),
            " (",
            leaf::var(sum_rebind),
            ") ",
            exit_arm,
            "in apply ",
            leaf::fname("while", 1),
            " (",
            leaf::var("Sum"),
            ")",
        ]
        .to_pretty_string();

        // `render(..)` side: a real `ConditionalLoop` node, built on a
        // separately-constructed but identically-seeded generator — the
        // opaque `continue_header`/`exit_arm` fields and the body's `Bind`
        // are minted via the SAME calls, in the SAME order, mirroring
        // exactly how the lowering pass would build them (Addendum 2's
        // ordering contract: condition, then body rebinds, then exit_arm
        // LAST).
        let mut render_gen = CoreErlangGenerator::new("dual_run_conditional_loop_direct");
        let ir_cond_var = render_gen.fresh_temp_var("CondFun");
        let ir_cond_doc = docvec![
            "call 'erlang':'<'(",
            leaf::var("Sum"),
            ", ",
            leaf::int_lit(10),
            ")",
        ];
        let ir_continue_header = docvec![
            "let ",
            leaf::var(ir_cond_var.clone()),
            " = fun (",
            leaf::var("Sum"),
            ") -> ",
            ir_cond_doc,
            " in case apply ",
            leaf::var(ir_cond_var),
            " (",
            leaf::var("Sum"),
            ") of ",
            "<'true'> when 'true' -> ",
        ];
        let frame = FrameId::new(1);
        let sum_v0 = local("sum", 0, frame);
        let ir_sum_rebind = render_gen.fresh_temp_var("Sum");
        let sum_v1 = VersionedVar::new(VersionPrefix::Gensym(ir_sum_rebind), 1, frame);
        let value_doc = docvec![
            "call 'erlang':'+'(",
            leaf::var("Sum"),
            ", ",
            leaf::int_lit(1),
            ")",
        ];
        let ir_body = vec![ThreadedStmt::Bind {
            target: sum_v1,
            source: sum_v0.clone(),
            op: BindOp::Direct(ValueRef::Doc(value_doc)),
            shadow_write: false,
            span: span(),
        }];
        let ir_exit_sa = render_gen.fresh_temp_var("ExitSA");
        let ir_exit_arm = docvec![
            "<'false'> when 'true' -> let ",
            leaf::var(ir_exit_sa.clone()),
            " = call 'maps':'put'(",
            leaf::atom("__local__sum"),
            ", ",
            leaf::var("Sum"),
            ", State) in {'nil', ",
            leaf::var(ir_exit_sa),
            "} end ",
        ];
        let ir = vec![ThreadedStmt::ConditionalLoop {
            fn_name: "while".to_string(),
            mode: ThreadingMode::DirectParams,
            frame,
            counter: None,
            continue_header: ir_continue_header,
            body: ir_body,
            produces: vec![sum_v0],
            exit_arm: ir_exit_arm,
            span: span(),
        }];
        let mut ctx = RenderCtx::new(&mut render_gen);
        let rendered_doc = render(&ir, &mut ctx).to_pretty_string();

        assert_eq!(
            rendered_doc, legacy_doc,
            "render(..)'s ConditionalLoop arm must reproduce the real \
             direct-params while-loop condition/case-split shape byte-for-byte"
        );
    }

    #[test]
    #[allow(clippy::too_many_lines)] // hand-authored dual-run fixture mirroring generate_while_loop_hybrid's real shape, ADR 0111 Addendum 2
    fn dual_run_conditional_loop_hybrid_state_prefix_matches_live_generator() {
        // Hybrid-mode counterpart of the direct-params test above: proves
        // BOTH that `ConditionalLoop`'s real condition/case-split shape
        // renders correctly under Hybrid loop context, AND (mirroring the
        // pre-Addendum-2 test this replaces) that a `State`-prefixed `Bind`
        // nested in the body (e.g. a nested construct's field mutation
        // running inside the hybrid loop body) resolves through the REAL
        // production accessors `current_state_var`/`next_state_var` under
        // hybrid loop context — not a hand-copy of their logic — so
        // `RenderCtx::resolve_prefix` (via the shared `render_state_prefix`
        // helper both paths call) matches live generator behavior
        // bit-for-bit even when interleaved with the new condition/exit
        // scaffolding.
        let mut legacy_gen = CoreErlangGenerator::new("dual_run_conditional_loop_hybrid");
        let cond_var = legacy_gen.fresh_temp_var("CondFun");
        legacy_gen.in_hybrid_loop = true;
        legacy_gen.in_loop_body = true;
        let cond_doc = docvec![
            "call 'erlang':'<'(",
            leaf::var("Sum"),
            ", ",
            leaf::int_lit(10),
            ")"
        ];
        let continue_header = docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (",
            leaf::var("Sum"),
            ") -> ",
            cond_doc,
            " in case apply ",
            leaf::var(cond_var),
            " (",
            leaf::var("Sum"),
            ") of ",
            "<'true'> when 'true' -> ",
        ];
        let sum_rebind = legacy_gen.fresh_temp_var("Sum");
        let state_source_name = legacy_gen.current_state_var();
        let state_target_name = legacy_gen.next_state_var();
        let body = docvec![
            "let ",
            leaf::var(sum_rebind.clone()),
            " = call 'erlang':'+'(",
            leaf::var("Sum"),
            ", ",
            leaf::int_lit(1),
            ") in ",
            " ", // BodyKind::Letrec's inter-statement separator (generate_threaded_loop_body_inner)
            "let ",
            leaf::var(state_target_name),
            " = ",
            leaf::var(state_source_name),
            " in ",
        ];
        legacy_gen.in_hybrid_loop = false;
        legacy_gen.in_loop_body = false;
        let exit_sa = legacy_gen.fresh_temp_var("ExitSA");
        let exit_arm = docvec![
            "<'false'> when 'true' -> let ",
            leaf::var(exit_sa.clone()),
            " = call 'maps':'put'(",
            leaf::atom("__local__sum"),
            ", ",
            leaf::var("Sum"),
            ", State) in {'nil', ",
            leaf::var(exit_sa),
            "} end ",
        ];
        let legacy_doc = docvec![
            "letrec ",
            leaf::fname("while", 1),
            " = fun (",
            leaf::var("Sum"),
            ") -> ",
            continue_header,
            body,
            " apply ",
            leaf::fname("while", 1),
            " (",
            leaf::var(sum_rebind),
            ") ",
            exit_arm,
            "in apply ",
            leaf::fname("while", 1),
            " (",
            leaf::var("Sum"),
            ")",
        ]
        .to_pretty_string();

        let mut render_gen = CoreErlangGenerator::new("dual_run_conditional_loop_hybrid");
        let ir_cond_var = render_gen.fresh_temp_var("CondFun");
        let ir_cond_doc = docvec![
            "call 'erlang':'<'(",
            leaf::var("Sum"),
            ", ",
            leaf::int_lit(10),
            ")"
        ];
        let ir_continue_header = docvec![
            "let ",
            leaf::var(ir_cond_var.clone()),
            " = fun (",
            leaf::var("Sum"),
            ") -> ",
            ir_cond_doc,
            " in case apply ",
            leaf::var(ir_cond_var),
            " (",
            leaf::var("Sum"),
            ") of ",
            "<'true'> when 'true' -> ",
        ];
        let frame = FrameId::new(1);
        let sum_v0 = local("sum", 0, frame);
        let ir_sum_rebind = render_gen.fresh_temp_var("Sum");
        let sum_v1 = VersionedVar::new(VersionPrefix::Gensym(ir_sum_rebind), 1, frame);
        let value_doc = docvec![
            "call 'erlang':'+'(",
            leaf::var("Sum"),
            ", ",
            leaf::int_lit(1),
            ")",
        ];
        let ir_body = vec![
            ThreadedStmt::Bind {
                target: sum_v1,
                source: sum_v0.clone(),
                op: BindOp::Direct(ValueRef::Doc(value_doc)),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, 1, frame),
                source: VersionedVar::new(VersionPrefix::State, 0, frame),
                op: BindOp::Direct(ValueRef::Version(VersionedVar::new(
                    VersionPrefix::State,
                    0,
                    frame,
                ))),
                shadow_write: false,
                span: span(),
            },
        ];
        let ir_exit_sa = render_gen.fresh_temp_var("ExitSA");
        let ir_exit_arm = docvec![
            "<'false'> when 'true' -> let ",
            leaf::var(ir_exit_sa.clone()),
            " = call 'maps':'put'(",
            leaf::atom("__local__sum"),
            ", ",
            leaf::var("Sum"),
            ", State) in {'nil', ",
            leaf::var(ir_exit_sa),
            "} end ",
        ];
        let ir = vec![ThreadedStmt::ConditionalLoop {
            fn_name: "while".to_string(),
            mode: ThreadingMode::Hybrid,
            frame,
            counter: None,
            continue_header: ir_continue_header,
            body: ir_body,
            produces: vec![sum_v0],
            exit_arm: ir_exit_arm,
            span: span(),
        }];
        let mut ctx = RenderCtx::new(&mut render_gen);
        let rendered_doc = render(&ir, &mut ctx).to_pretty_string();

        assert_eq!(
            rendered_doc, legacy_doc,
            "render(..)'s ConditionalLoop arm must reproduce the real hybrid \
             while-loop condition/case-split shape byte-for-byte, including \
             the loop-context-aware State prefix"
        );
    }

    #[test]
    fn render_loop_letrec_param_list_and_final_args_use_hybrid_context_even_when_nested_in_stateacc_ambient()
     {
        // Regression test for `render_loop_letrec`'s loop-context ordering:
        // the `fun (<param_list>) -> <body_doc> apply 'FnName'/N (<final_args>)`
        // declaration, its body, and its recursive tail call are all the
        // SAME `fun` block, so `param_list` and `final_args` must resolve
        // `VersionPrefix::State` under the identical Hybrid context
        // `body_doc` renders under — computing either of them under the
        // pre-loop ambient context instead would name the `fun`'s declared
        // parameter differently from what the body/recursive-call actually
        // reference, producing invalid Core Erlang (a reference to a
        // variable the `fun` never bound). The OUTER initial call remains
        // the one place `produces` correctly resolves under ambient context
        // — it's the calling scope's own reference, not the `fun`'s.
        //
        // Simulates a Hybrid loop nested inside an outer `StateAcc`-mode
        // loop body (`in_loop_body = true, in_hybrid_loop = false` ambient —
        // the exact shape that would trip this bug) to prove the `fun`
        // declaration/body/recursive-call all agree on `State`/`State1`
        // (the Hybrid loop's own context), while only the outer call uses
        // `StateAcc` (the ambient it's actually invoked from).
        let frame = FrameId::new(1);
        let ir = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::Hybrid,
            frame,
            body: vec![ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, 1, frame),
                source: VersionedVar::new(VersionPrefix::State, 0, frame),
                op: BindOp::Direct(ValueRef::Version(VersionedVar::new(
                    VersionPrefix::State,
                    0,
                    frame,
                ))),
                shadow_write: false,
                span: span(),
            }],
            produces: vec![VersionedVar::new(VersionPrefix::State, 1, frame)],
            span: span(),
        }];
        let mut render_gen = CoreErlangGenerator::new("dual_run_hybrid_nested");
        // Ambient context as if this `Threaded` node sits inside an outer
        // StateAcc-mode loop body — the pre-existing, pre-fix bug would have
        // computed `param_list`/`final_args` under exactly this (wrong)
        // ambient.
        render_gen.in_loop_body = true;
        render_gen.in_hybrid_loop = false;
        let mut ctx = RenderCtx::new(&mut render_gen);
        let rendered = render(&ir, &mut ctx).to_pretty_string();

        assert_eq!(
            rendered,
            "letrec '_Loop1'/1 = fun (State) -> let State1 = State in \
             apply '_Loop1'/1 (State1) in apply '_Loop1'/1 (StateAcc)",
            "the fun declaration, body, and recursive tail call must all \
             agree on State/State1 (the Hybrid loop's own context) — only \
             the outer initial call should reference StateAcc (the outer \
             StateAcc ambient) — got: {rendered}"
        );
    }

    #[test]
    fn dual_run_nlr_catch_reuses_wrap_body_with_nlr_catch_verbatim() {
        // NLR-catch rendering doesn't re-derive `wrap_body_with_nlr_catch`'s
        // scaffolding — it calls the exact same function real NLR call
        // sites use, so this test's "legacy" and `render(lower(..))` sides
        // are provably byte-identical by construction, not by luck. Kept as
        // an explicit dual-run test (rather than relying on that reasoning
        // alone) so a future refactor that breaks the direct-reuse
        // invariant fails loudly here instead of silently drifting.
        let mut legacy_gen = CoreErlangGenerator::new("dual_run_nlr");
        let legacy_token = legacy_gen.fresh_temp_var("NlrToken");
        let inner_body = docvec!["let ", leaf::var("Sum1"), " = ", leaf::var("Sum"), " in "];
        let legacy_doc = legacy_gen
            .wrap_body_with_nlr_catch(inner_body, &legacy_token, NlrBoundary::ActorReply)
            .to_pretty_string();

        let frame = FrameId::new(1);
        let ir = vec![
            ThreadedStmt::NlrCatch {
                boundary: NlrBoundary::ActorReply,
                token: TokenId::new(0),
                frame,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: local("sum", 1, frame),
                source: local("sum", 0, frame),
                op: BindOp::Direct(ValueRef::Version(local("sum", 0, frame))),
                shadow_write: false,
                span: span(),
            },
        ];
        let mut render_gen = CoreErlangGenerator::new("dual_run_nlr");
        let mut ctx = RenderCtx::new(&mut render_gen);
        let rendered_doc = render(&ir, &mut ctx).to_pretty_string();

        assert_eq!(rendered_doc, legacy_doc);
    }
}
