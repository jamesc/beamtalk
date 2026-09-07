// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Builders that construct-and-verify (or construct-and-render) `ThreadedIr`
//! fixtures for call sites that aren't themselves full `ThreadedIr`-emitting
//! generators: [`build_tuple_acc_unpack`], [`construct_and_verify_class_var_bind`],
//! [`verify_body_with_opaque_version_gaps`], [`verify_simple_bind`], and
//! [`super::ThreadedValue::close`]. Depends on [`super::ir`], [`super::verify`],
//! and [`super::emit`] — the top of the `threaded_ir` module split.

use super::super::{CoreErlangGenerator, NlrBoundary};
use super::emit::{RenderCtx, render, render_value};
use super::ir::{
    AccParam, BindOp, CloseContext, FrameId, StateAccFallbackReason, ThreadedStmt, ThreadedValue,
    ThreadingMode, TokenId, ValueRef, VersionPrefix, VersionedVar,
};
use super::verify::{VerifyError, verify};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_core::source_analysis::Span;

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
/// pass `mode_gate_slots` from [`super::super::control_flow::ListOpKind::gate_slots`]
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
pub(in crate::core_erlang) fn build_tuple_acc_unpack(
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
pub(in crate::core_erlang) fn construct_and_verify_class_var_bind(
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
pub(in crate::core_erlang) fn verify_body_with_opaque_version_gaps(
    ir: &[ThreadedStmt],
) -> Vec<VerifyError> {
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
pub(in crate::core_erlang) fn verify_simple_bind(
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

// ─── ThreadedValue::close (ADR 0118, Decision 5) ───────────────────────────

impl ThreadedValue {
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
    pub(in crate::core_erlang) fn close(
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
