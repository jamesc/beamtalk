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
//! it.
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
//! stay on the pre-ADR-0111 AST-directed path. See ADR 0111 § Addendum 15
//! for the design and the `#[allow(dead_code)]` loop-migration shapes this
//! keeps ready (`ir` module docs).
//!
//! ## Module layout
//!
//! This module is split into four submodules, each depending only on the
//! ones before it (`emit`/`verify` → `ir`; `build` → `ir`/`verify`/`emit`):
//!
//! - [`ir`] — identity/value types, `ThreadedStmt`, `ThreadedValue`.
//! - [`verify`] — the checker: `VerifyError`, `verify`.
//! - [`emit`] — the emitter: `render`, `RenderCtx`.
//! - [`build`] — builders that construct-and-verify or construct-and-render
//!   fixtures: `build_tuple_acc_unpack`, `construct_and_verify_class_var_bind`,
//!   `verify_body_with_opaque_version_gaps`, `verify_simple_bind`.
//!
//! Every item re-exported below keeps the path it had before this split
//! (`threaded_ir::X`) — see each submodule's own doc comment for what it
//! owns.
//!
//! ## Invariants
//!
//! - A statement that advances a threaded version is a `Bind`; `Statement`
//!   and `ValueRef::Doc` are opaque and carry no threading of their own.
//! - Versions are linear within one [`ir::FrameId`]; sibling branch/handler
//!   arms get distinct frames so independently-minted versions never
//!   collide.
//! - A class-var `Bind` at a shadow-write-eligible point, in a method whose
//!   body can relay a foreign NLR, must set `shadow_write` (the ADR 0110
//!   contract).
//! - A body with version steps hidden inside a shared multi-module helper is
//!   verified after `build::backfill_opaque_version_gap` closes those gaps.
//!
//! ## Scope
//!
//! Covers state-version bindings (with frame identity), threading-mode
//! selection, shadow-write emission, and NLR relay boundaries. Everything
//! else in codegen stays AST-directed and unaffected — see ADR 0111
//! §Decision / §Constraints for the full narrow-scope rationale.

mod build;
mod emit;
mod ir;
mod verify;

pub(super) use build::{
    build_tuple_acc_unpack, construct_and_verify_class_var_bind,
    verify_body_with_opaque_version_gaps, verify_simple_bind,
};
pub(super) use emit::{RenderCtx, render, render_value};
pub(super) use ir::{
    BindOp, FrameId, StateAccFallbackReason, ThreadedStmt, ThreadedValue, ThreadingMode, TokenId,
    ValueRef, VersionCounter, VersionPrefix, VersionedVar,
};
pub(super) use verify::verify;

// `AccParam`/`CloseContext`/`LoopCounter`/`VerifyError` have no production
// caller by name today (constructed only in tests, or reached only through
// `verify()`'s inferred return type) — `#[cfg(test)]` keeps the plain build
// warning-free without narrowing what a future non-test caller can reach:
// `threaded_ir::verify::VerifyError` (etc.) stays valid regardless, since
// each item's own `pub(in crate::core_erlang)` visibility doesn't depend on
// this re-export.
#[cfg(test)]
pub(super) use ir::{AccParam, CloseContext};
#[cfg(test)]
pub(super) use verify::VerifyError;
// `LoopCounter` has no caller anywhere yet (kept per ADR 0111 § Addendum 15
// — see `ir`'s module docs); re-exported for path parity with the rest of
// this list, not because anything reaches it through this path today.
#[cfg(test)]
#[allow(unused_imports)]
pub(super) use ir::LoopCounter;

// Test-only: brings the ambient names `threaded_ir.rs`'s own top-level
// imports used to provide into scope for `tests`' `use super::*` — the
// submodules each import these privately for their own code, so nothing
// else here needs them.
#[cfg(test)]
use super::{CoreErlangGenerator, NlrBoundary};
#[cfg(test)]
use beamtalk_cerl_doc::docvec;
#[cfg(test)]
use beamtalk_cerl_doc::{Document, leaf};
#[cfg(test)]
use beamtalk_core::source_analysis::Span;

#[cfg(test)]
mod tests;
