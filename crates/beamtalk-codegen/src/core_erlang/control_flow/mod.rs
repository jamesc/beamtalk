// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Control flow compilation with state mutation analysis.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This module handles the compilation of iteration and loop constructs that may
//! mutate actor state. Each construct follows a consistent pattern:
//!
//! 1. **Pure variant**: No state mutations detected, uses simple functional style
//! 2. **Stateful variant**: Mutations detected, requires state threading
//!
//! # Supported Constructs
//!
//! - **List iteration**: `do:`, `collect:`, `select:`, `reject:`, `inject:into:`
//! - **Dictionary iteration**: `do:`, `doWithKey:`
//! - **While loops**: `whileTrue:`, `whileFalse:`
//! - **Counted loops**: `repeat`, `timesRepeat:`, `to:do:`, `to:by:do:`
//!
//! Submodules organize the code by domain:
//! - [`plan`] — `ThreadingPlan` (mode selection) and its `Document` emitters
//! - [`analysis`] — diagnostics and class-var/list-op analysis predicates
//! - [`body`] — the unified per-statement threaded loop/fold body generator
//! - [`local_assign`] — local-variable assignment codegen inside loop bodies
//! - [`util`] — shared state-threading helpers (state-map keys, threaded-locals computation)
//! - [`list_ops`] — List iteration constructs (and `BodyKind`)
//! - [`dict_ops`] — Dictionary iteration constructs
//! - [`while_loops`] — While loop constructs
//! - [`counted_loops`] — Counted loop constructs (and `CountedLoopFrame`)
//! - [`conditionals`] — `ifTrue:`/`ifFalse:`/`match:` etc.
//! - [`exception_handling`] — `on:do:`/`ensure:`
//! - [`loop_mode`] — [`LoopMode`], the generator's own loop-body context field

mod analysis;
mod body;
mod conditionals;
mod counted_loops;
mod dict_ops;
mod exception_handling;
mod list_ops;
mod local_assign;
mod loop_mode;
mod plan;
mod util;
mod while_loops;

// Re-exports so existing `super::X` / `super::super::X` references across
// this subtree keep resolving to the same logical names now that their
// definitions live in sibling files. `ThreadingPlan`/`condition_has_state_effects`
// are also consumed outside `control_flow` (e.g. `value_type_codegen.rs`,
// `gen_server/methods.rs`), so those two re-exports stay `pub(super)`
// (visible to all of `core_erlang`, matching their own declared visibility);
// the others have no caller outside this subtree, so a plain (module-private,
// visible-to-descendants) `use` is enough.
use super::threaded_ir::StateAccFallbackReason;
use counted_loops::extra_threaded_arg_doc;
use plan::ListOpKind;
// ADR 0111 Addendum 15 (Foldl migration): `BodyKind` is also consumed by
// `value_type_codegen.rs`'s `generate_value_type_do_open`, which now calls
// `CoreErlangGenerator::generate_foldl_loop_body` directly (the deleted
// `generate_list_do_body_with_threading` compat shim's inlined replacement)
// — `pub(super)`, matching `ThreadingPlan`'s own cross-module visibility.
pub(super) use list_ops::BodyKind;
// `FieldWriteSite`/`Closure` name the two axes
// `CoreErlangGenerator::lower_field_write` unifies field-assignment lowering
// behind; consumed outside `control_flow` by `expressions.rs`'s
// `generate_field_assignment` and `dispatch_codegen.rs`'s
// `generate_field_assignment_open`, both now thin wrappers around it —
// `pub(super)`, matching `BodyKind`/`ThreadingPlan`'s own cross-module
// visibility.
pub(super) use conditionals::{Closure, FieldWriteSite};
// `LoopMode` is a field on `CoreErlangGenerator` (`generator/mod.rs`) and is
// read/written across the whole `core_erlang` tree (`dispatch_codegen.rs`,
// `expressions.rs`, `threaded_ir/`, ...) — `pub(super)`, matching
// `BodyKind`/`ThreadingPlan`'s own cross-module visibility.
pub(super) use loop_mode::LoopMode;
pub(super) use plan::{ThreadingPlan, condition_has_state_effects};
