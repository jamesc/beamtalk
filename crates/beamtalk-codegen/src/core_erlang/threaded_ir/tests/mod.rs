// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for `ThreadedIr`.
//!
//! Tests are organized into domain-focused sub-modules:
//! - [`render_naming`] — `VersionedVar`/`FrameId` naming and `VersionCounter`
//!   basics
//! - [`verify_walk`] — `verify()`'s clean/silent case, `UnboundVersion`,
//!   `NonLinearVersion`, and `ThreadingModeUnpackMismatch`
//! - [`lower_and_render_shim`] — the `lower_and_render` test shim's coverage
//!   of `render()`'s `BindOp`/`ThreadedStmt` variants
//! - [`tuple_acc`] — ADR 0111 Phase C `TupleAcc` unpack invariants
//! - [`simple_bind`] — `verify_simple_bind` and
//!   `verify_body_with_opaque_version_gaps`, and `verify_simple_bind`'s
//!   per-scope invariant
//! - [`dual_run`] — the dual-run byte-parity harness
//! - [`catch_boundary`] — ADR 0130 §4 `OnDoCatch` / `CatchWithoutClassVarRestore`
//! - [`class_method_scope`] — BT-3725 `ActorStateInClassMethod`
//! - [`close_and_value`] — `ThreadedValue::close` (ADR 0118 §Decision 5)
//! - [`local_rebind`] — ADR 0131 §2 `LocalRebind` lowering per (frame mode ×
//!   membership), and the `ConstructTuple`/`DiscardLocals`/`MethodBody`/
//!   `BranchArm` nodes
//! - [`verifier_diagnostic`] — BT-3724 release-mode `internal:` warning

use super::*;

fn span() -> Span {
    Span::new(0, 1)
}

fn local(name: &str, version: usize, frame: FrameId) -> VersionedVar {
    VersionedVar::new(VersionPrefix::Local(name.to_string()), version, frame)
}

fn self_var(version: usize, frame: FrameId) -> VersionedVar {
    VersionedVar::new(VersionPrefix::SelfVt, version, frame)
}

/// A stable skeleton-fidelity test shim signature: delegates to [`render`]
/// against a throwaway [`CoreErlangGenerator`] (cheap — no I/O) so every
/// `lower_and_render(&ir).to_pretty_string()` test call stays simple.
///
/// Unlike [`render`] itself (which has real production callers — see
/// [`render`]'s doc comment), every one of those callers builds its own
/// [`RenderCtx`] directly against the live generator rather than a
/// throwaway one, so this convenience wrapper has no production caller.
fn lower_and_render(ir: &[ThreadedStmt]) -> Document<'static> {
    let mut generator = CoreErlangGenerator::new("__threaded_ir_render_shim");
    let mut ctx = RenderCtx::new(&mut generator);
    render(ir, &mut ctx)
}

mod catch_boundary;
mod class_method_scope;
mod close_and_value;
mod dual_run;
mod local_rebind;
mod lower_and_render_shim;
mod render_naming;
mod simple_bind;
mod tuple_acc;
mod verifier_diagnostic;
mod verify_walk;
