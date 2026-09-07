// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for `ThreadedIr`.
//!
//! Tests are organized into domain-focused sub-modules:
//! - [`render_naming`] — `VersionedVar`/`FrameId` naming and `VersionCounter`
//!   basics
//! - [`verify_walk`] — `verify()`'s clean/silent case, `UnboundVersion`,
//!   `NonLinearVersion`, `ThreadingModeUnpackMismatch`, and
//!   `ShadowWriteMissing` (ADR 0110 contract)
//! - [`lower_and_render_shim`] — the `lower_and_render` test shim's coverage
//!   of `render()`'s `BindOp`/`ThreadedStmt` variants
//! - [`tuple_acc`] — ADR 0111 Phase C `TupleAcc` unpack invariants
//! - [`class_var_bind`] — `construct_and_verify_class_var_bind` and
//!   `verify_simple_bind` (ADR 0110 contract), and
//!   `verify_body_with_opaque_version_gaps`
//! - [`dual_run`] — the dual-run byte-parity harness
//! - [`close_and_value`] — `ThreadedValue::close` (ADR 0118 §Decision 5)

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

mod class_var_bind;
mod close_and_value;
mod dual_run;
mod lower_and_render_shim;
mod render_naming;
mod tuple_acc;
mod verify_walk;
