// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Builders that construct-and-verify (or construct-and-render) `ThreadedIr`
//! fixtures for call sites that aren't themselves full `ThreadedIr`-emitting
//! generators: [`build_tuple_acc_unpack`],
//! [`verify_body_with_opaque_version_gaps`], [`verify_simple_bind`],
//! [`super::ThreadedValue::close`], and ADR 0131's local-rebind prelude
//! constructors ([`build_local_threading_prelude`] and friends). Depends on [`super::ir`], [`super::verify`],
//! and [`super::emit`] — the top of the `threaded_ir` module split.

use super::super::CoreErlangGenerator;
use super::emit::{RenderCtx, render, render_value};
use super::ir::{
    AccParam, BindOp, CarrierSlot, CloseContext, FrameId, RebindFrameKind, RebindLowering,
    RebindShape, ThreadedStmt, ThreadedValue, ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
};
use super::verify::{ScopeKind, VerifyError, verify_in_scope};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_core::source_analysis::Span;

// ─── ADR 0111 Phase C TupleAcc unpack: emission input ────

/// Builds the real `ThreadedIr` for one `TupleAcc`-mode fold lambda's
/// per-iteration positional unpack step — the single production emitter of
/// this unpack shape across every list-op and dict-op call site
/// (`basic_ops.rs`, `filter_ops.rs`, `search_ops.rs`, `transform_ops.rs`,
/// `dict_ops.rs`, all via `generate_foldl_loop_body`, `control_flow/body.rs`).
///
/// Promoted from a verification-only side channel to
/// genuine emission input — [`render`]ing this exact `ThreadedStmt` IS how
/// `generate_foldl_loop_body` now produces its `Document` output, not a
/// second, independently hand-Document-built duplicate of it. Each target's
/// identity is a [`VersionPrefix::Gensym`] (never [`VersionPrefix::Local`]):
/// the real per-iteration unpack binds the BARE `to_core_erlang_var` name
/// (`Sum`, never `Sum1`) — matching every real call site's
/// hand-rolled loop byte-for-byte (confirmed against `basic_ops.rs`'s
/// `generate_list_do_with_mutations`, the simplest call site) — never the
/// sequential `prefix{version}` scheme `Local`'s renderer would apply to a
/// nonzero version. Same naming-scheme-mismatch class ADR 0111 Addendum 2
/// "Gap 2" already named and closed for loop-local rebinds; `Gensym`'s
/// existing verbatim-regardless-of-version rendering closes it here too,
/// with no new IR machinery.
///
/// **Independent gate-slot derivation**: `mode_gate_slots` and
/// `node_gate_slots` are two genuinely separate sources, not the same
/// argument threaded twice. Callers
/// pass `mode_gate_slots` from [`super::super::control_flow::ListOpKind::gate_slots`]
/// — a canonical per-op-family table fixed at `ThreadingPlan` construction
/// (lowering time, before any particular call site's `index_offset` is even
/// chosen) — and `node_gate_slots` from their own already-computed
/// `index_offset - 1` (rendering-side construction).
/// A call site whose `index_offset` disagrees with its declared
/// `ListOpKind` (e.g. a future op miscategorized when copy-pasted from a
/// same-shaped sibling) trips [`VerifyError::EarlyExitGateSlotMismatch`]
/// — see [`verify_tuple_acc_unpack_invariant`] and
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

// ─── Method-body verification with opaque version gaps ──────────

/// [`verify`]s a straight-line, [`FrameId::ROOT`]-frame method-body IR (ADR
/// 0111 Addendum 4: `gen_server/methods.rs`'s
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
/// [`verify_simple_bind`]'s
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
/// backfill history — the shape
/// `verify_would_catch_the_bt_3131_regression_shape_given_accumulated_history`'s
/// previously-hypothetical capability made live), `UnboundVersion` for any
/// source the chain never reached. What it cannot check (ADR 0111 §Verifier
/// honesty, same class as `ValueRef::Doc`/`exit_arm`): mutations hidden inside the opaque
/// statements themselves — those are exactly the backfilled gaps.
pub(in crate::core_erlang) fn verify_body_with_opaque_version_gaps(
    ir: &[ThreadedStmt],
    scope: ScopeKind,
) -> Vec<VerifyError> {
    verify_in_scope(&backfill_opaque_version_gaps(ir, FrameId::ROOT), scope)
}

/// The fixture-building half of [`verify_body_with_opaque_version_gaps`]:
/// walks `ir` inserting a synthetic backfill `Bind` chain ahead of any real
/// `Bind` whose source version an opaque `Statement` advanced past
/// unrecorded, at `frame`. Split out (ADR 0111 Addendum 15's Foldl
/// migration) so a caller that must WRAP the backfilled body in its own
/// node before verifying — `control_flow::body::generate_foldl_loop_body`'s
/// merged `Threaded` node, whose body lives at its own non-`ROOT` frame, not
/// a flat method-level fixture — can reuse the identical technique instead
/// of re-deriving it at a hardcoded [`FrameId::ROOT`] (CLAUDE.md's
/// no-duplicate-implementations rule). `verify_body_with_opaque_version_gaps`
/// itself is unchanged in behavior: `frame: FrameId::ROOT` reproduces
/// exactly what it always built inline.
pub(in crate::core_erlang) fn backfill_opaque_version_gaps(
    ir: &[ThreadedStmt],
    frame: FrameId,
) -> Vec<ThreadedStmt> {
    let mut fixture: Vec<ThreadedStmt> = Vec::with_capacity(ir.len());
    let mut last_state_version = 0usize;
    for stmt in ir {
        if let ThreadedStmt::Bind { target, source, .. } = stmt {
            backfill_opaque_version_gap(
                &mut fixture,
                &VersionPrefix::State,
                target,
                source,
                frame,
                &mut last_state_version,
            );
        }
        fixture.push(stmt.clone());
    }
    fixture
}

/// Backfill step for one `VersionPrefix` inside
/// [`backfill_opaque_version_gaps`]'s per-`Bind` scan.
fn backfill_opaque_version_gap(
    fixture: &mut Vec<ThreadedStmt>,
    prefix: &VersionPrefix,
    target: &VersionedVar,
    source: &VersionedVar,
    frame: FrameId,
    last_version: &mut usize,
) {
    if source.prefix == *prefix && source.frame == frame && source.version > *last_version {
        fixture.extend(backfill_version_chain(
            prefix,
            frame,
            *last_version,
            source.version,
            Span::default(),
        ));
        *last_version = source.version;
    }
    if target.prefix == *prefix && target.frame == frame {
        *last_version = (*last_version).max(target.version);
    }
}

/// Builds a synthetic `Direct('_')` `Bind` chain backfilling version history
/// `(from+1)..=to` at `frame` for `prefix` — the technique
/// [`backfill_opaque_version_gap`] and [`verify_simple_bind`] both need to
/// give [`VerifyWalk::check_use`]'s frame-flow rule a producing `Bind` for
/// version history a fixture can't otherwise see (one shared loop, per
/// CLAUDE.md's no-duplicate-implementations rule).
fn backfill_version_chain(
    prefix: &VersionPrefix,
    frame: FrameId,
    from: usize,
    to: usize,
    span: Span,
) -> Vec<ThreadedStmt> {
    let mut chain = Vec::with_capacity(to.saturating_sub(from));
    for v in (from + 1)..=to {
        chain.push(ThreadedStmt::Bind {
            target: VersionedVar::new(prefix.clone(), v, frame),
            source: VersionedVar::new(prefix.clone(), v - 1, frame),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            span,
        });
    }
    chain
}

// ─── Simple version-bind construction ────────────────────────────

/// Builds and verifies a minimal `ThreadedIr` fixture for a single `Self{N}`
/// or `State{N}` version `Bind`, given the real source/target version
/// numbers already read off the live generator counter at the call site
/// (`generate_field_assignment`'s value-type and instance-actor
/// branches, `expressions.rs`). Reused for both prefixes. This helper is
/// handed the *actual* version numbers,
/// which may already be arbitrarily large after earlier mutations in the
/// same method. [`VerifyWalk::check_use`]'s
/// frame-flow rule requires every version `>0` referenced as a `Bind`'s
/// source to have a producing `Bind` visible on the frame stack — so without
/// backfilling that history, every mutation past a method's first would
/// spuriously fail `UnboundVersion` (its `source_version` would have no
/// producer in an isolated single-`Bind` fixture). The backfill chain
/// (`1..=source_version`) is exactly the technique the retired
/// branch-frame-linearity check used (ADR 0111 Addendum 5), generalized
/// here to an arbitrary prefix and reused rather than
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
/// source_version`). It does **not** catch the historical
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
/// gap is tracked as a follow-up, not attempted here (this helper is
/// scoped to coverage extension via the existing checks, not new generator
/// state).
///
/// **Per-scope invariant (BT-3725 follow-up):** verified with `scope` via
/// [`verify_in_scope`], so a `State{N}`/`Self{N}` mint reached from a
/// [`ScopeKind::ClassMethod`] scope is a
/// [`VerifyError::ActorStateInClassMethod`] (`FamilyVersion`), exactly as it
/// is for a whole-body check. The fixture's `Bind` sits at method level
/// (empty `mode_stack`), so a `State` version here is always the actor
/// family, never a `StateAcc` map's version.
pub(in crate::core_erlang) fn verify_simple_bind(
    prefix: VersionPrefix,
    source_version: usize,
    target_version: usize,
    span: Span,
    scope: ScopeKind,
) -> Vec<VerifyError> {
    let frame = FrameId::ROOT;
    let mut ir = backfill_version_chain(&prefix, frame, 0, source_version, span);
    ir.push(ThreadedStmt::Bind {
        target: VersionedVar::new(prefix.clone(), target_version, frame),
        source: VersionedVar::new(prefix, source_version, frame),
        op: BindOp::Direct(ValueRef::Literal("'_'")),
        span,
    });
    verify_in_scope(&ir, scope)
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

// ─── ADR 0131 §1/§2: local-rebind preludes ─────────────────────────────────
//
// No production caller yet: ADR 0131 Phase 2's `local_threading_producer`
// (loops, list-ops, `do:`, lookup selectors) is the first, Phase 3 the
// conditional/handler arms — mirroring how `CloseContext` landed ahead of its
// consumers in ADR 0118 phase 1a. Hence the `#[allow(dead_code)]`s below.

/// The enclosing frame a [`ThreadedStmt::LocalRebind`] is lowered by: its
/// identity, its kind/mode (the frame-mode half of ADR 0131 §2's lowering
/// key) and its threaded set (the membership half). One entry of a
/// producer's frame stack, read off the enclosing node by [`Self::of`] — a
/// rebind's lowering is never looked up in a side table.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct RebindFrame {
    pub(in crate::core_erlang) frame: FrameId,
    pub(in crate::core_erlang) kind: RebindFrameKind,
    pub(in crate::core_erlang) threads: Vec<String>,
}

#[allow(dead_code)]
impl RebindFrame {
    /// Reads the frame off its node. A [`ThreadedStmt::MethodBody`] or
    /// [`ThreadedStmt::BranchArm`] records its own `threads`; a loop/fold
    /// ([`ThreadedStmt::Threaded`]/[`ThreadedStmt::ConditionalLoop`])
    /// records its [`ThreadingMode`] but not the locals it threads (a
    /// `StateAcc` loop's locals ride its map, a `TupleAcc` fold's are
    /// `Gensym` unpack targets), so `loop_threads` supplies them — the
    /// loop's own `ThreadingPlan::threaded_locals`. `None` for a node that
    /// is not a frame.
    pub(in crate::core_erlang) fn of(node: &ThreadedStmt, loop_threads: &[String]) -> Option<Self> {
        let (frame, kind, threads) = match node {
            ThreadedStmt::MethodBody { frame, threads, .. } => {
                (*frame, RebindFrameKind::MethodBody, threads.clone())
            }
            ThreadedStmt::BranchArm { frame, threads, .. } => {
                (*frame, RebindFrameKind::BranchArm, threads.clone())
            }
            ThreadedStmt::Threaded { mode, frame, .. }
            | ThreadedStmt::ConditionalLoop { mode, frame, .. } => (
                *frame,
                RebindFrameKind::Loop(mode.clone()),
                loop_threads.to_vec(),
            ),
            _ => return None,
        };
        Some(Self {
            frame,
            kind,
            threads,
        })
    }

    /// Whether `local` is one of this frame's own threaded locals.
    pub(in crate::core_erlang) fn threads_local(&self, local: &str) -> bool {
        self.threads.iter().any(|t| t == local)
    }

    /// ADR 0131 §2's table cell for `local` in this frame.
    pub(in crate::core_erlang) fn shape_for(&self, local: &str) -> RebindShape {
        RebindShape::for_frame(&self.kind, self.threads_local(local))
    }
}

/// One threaded local of a construct, as its producer reports it: where it
/// sits in the construct tuple and the lowering-time name its new value
/// binds to (the producer `bind_var`s `local` to `value_var`).
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct ThreadedLocalSlot {
    pub(in crate::core_erlang) local: String,
    pub(in crate::core_erlang) slot: CarrierSlot,
    pub(in crate::core_erlang) value_var: String,
}

/// Builds a [`ThreadedStmt::ConstructTuple`]: `let <carrier> = <doc> in`.
#[allow(dead_code)]
pub(in crate::core_erlang) fn build_construct_tuple(
    carrier: impl Into<String>,
    doc: Document<'static>,
    threads: Vec<String>,
    span: Span,
) -> ThreadedStmt {
    ThreadedStmt::ConstructTuple {
        carrier: carrier.into(),
        doc,
        threads,
        span,
    }
}

/// Builds one [`ThreadedStmt::LocalRebind`] lowered by `enclosing`: the
/// shape is ADR 0131 §2's table cell ([`RebindFrame::shape_for`]); `lower`
/// supplies that shape's lowering-time names (a `LoopParam`'s source and
/// target identities, a `MapPut`'s key and `State` version step — minted
/// from the live generator by a production caller). A `lower` that answers
/// a different shape is what Phase 1c's `LocalRebindModeMismatch` reports.
#[allow(dead_code)]
pub(in crate::core_erlang) fn build_local_rebind(
    enclosing: &RebindFrame,
    carrier: &str,
    local: &ThreadedLocalSlot,
    state_first: Option<VersionedVar>,
    span: Span,
    lower: impl FnOnce(RebindShape) -> RebindLowering,
) -> ThreadedStmt {
    ThreadedStmt::LocalRebind {
        local: local.local.clone(),
        carrier: carrier.to_string(),
        slot: local.slot.clone(),
        frame: enclosing.frame,
        value_var: local.value_var.clone(),
        state_first,
        lowering: lower(enclosing.shape_for(&local.local)),
        span,
    }
}

/// A local-threading producer's whole prelude (ADR 0131 §1): the
/// [`ThreadedStmt::ConstructTuple`], then `family_binds` — the construct's
/// ADR 0122 family extraction (`extract_family_slots` over `carrier`) —
/// then one [`ThreadedStmt::LocalRebind`] per entry of `locals`, in order.
///
/// ADR 0131 §2's ordering rule lives here: in an actor instance frame the
/// carrier's `StateAcc` slot is also the `State` family's next version, so
/// the `State` family `Bind` renders **first** and every `Key` rebind reads
/// from the version it bound (`state_first`) rather than `element(2, CF)`.
/// With no `State` family `Bind` (class methods, value types, flat tuples)
/// each rebind reads the carrier directly.
#[allow(dead_code)]
pub(in crate::core_erlang) fn build_local_threading_prelude(
    enclosing: &RebindFrame,
    carrier: &str,
    doc: Document<'static>,
    locals: &[ThreadedLocalSlot],
    family_binds: Vec<ThreadedStmt>,
    span: Span,
    mut lower: impl FnMut(&ThreadedLocalSlot, RebindShape) -> RebindLowering,
) -> Vec<ThreadedStmt> {
    let state_first = family_binds.iter().find_map(|stmt| match stmt {
        ThreadedStmt::Bind { target, .. } if target.prefix == VersionPrefix::State => {
            Some(target.clone())
        }
        _ => None,
    });
    let mut prelude = Vec::with_capacity(1 + family_binds.len() + locals.len());
    prelude.push(build_construct_tuple(
        carrier,
        doc,
        locals.iter().map(|l| l.local.clone()).collect(),
        span,
    ));
    prelude.extend(family_binds);
    for local in locals {
        prelude.push(build_local_rebind(
            enclosing,
            carrier,
            local,
            state_first.clone(),
            span,
            |shape| lower(local, shape),
        ));
    }
    prelude
}

/// Builds a [`ThreadedStmt::DiscardLocals`]: a `MethodBody` frame's last
/// expression dropping its construct's rebinds (ADR 0131 §2).
#[allow(dead_code)]
pub(in crate::core_erlang) fn build_discard_locals(
    carrier: impl Into<String>,
    span: Span,
) -> ThreadedStmt {
    ThreadedStmt::DiscardLocals {
        carrier: carrier.into(),
        span,
    }
}

/// Builds a [`ThreadedStmt::MethodBody`] frame node. `threads` is empty
/// except in the REPL, where it is the bindings map's keys.
#[allow(dead_code)]
pub(in crate::core_erlang) fn build_method_body(
    frame: FrameId,
    threads: Vec<String>,
    body: Vec<ThreadedStmt>,
) -> ThreadedStmt {
    ThreadedStmt::MethodBody {
        frame,
        threads,
        body,
    }
}

/// Builds a [`ThreadedStmt::BranchArm`] frame node. `threads` is the set the
/// arm's closer packs into its seeded `StateAcc` (`seed_conditional_locals`).
#[allow(dead_code)]
pub(in crate::core_erlang) fn build_branch_arm(
    frame: FrameId,
    threads: Vec<String>,
    body: Vec<ThreadedStmt>,
) -> ThreadedStmt {
    ThreadedStmt::BranchArm {
        frame,
        threads,
        body,
    }
}
