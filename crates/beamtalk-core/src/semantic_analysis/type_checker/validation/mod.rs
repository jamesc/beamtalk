// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type validation — checking types against declarations and constraints.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module contains the validation methods of [`TypeChecker`], split by
//! concern into one `impl TypeChecker` block per file:
//! - `selectors` — class-side / instance selector validation, typed annotations
//! - `compat` — type compatibility, assignability, metatypes, aliases, generics
//! - `arguments` — argument and binary-operand checks
//! - `returns` — return-type checks
//! - `overrides` — override compatibility
//! - `construction` — field assignment, definite assignment, state defaults
//! - `emit` — diagnostic emitters
//! - `protocol_args` — protocol / union / meta argument conformance
//!
//! Together they cover:
//! - Class-side and instance-side selector validation
//! - Type annotation checking for typed classes
//! - Argument and return type compatibility
//! - Binary operand type checking
//! - Field assignment and state default validation
//! - "Did you mean?" suggestions for unknown selectors
//!
//! Dependency direction is one-way: `inference/` calls into this module to
//! render diagnostics (inference dispatches, validation decides), never the
//! reverse. `check_state_defaults` needs an inferred type for each state
//! field's default-value expression but does not call `infer_expr` itself —
//! it reads the result `inference/mod.rs::check_module` already cached in
//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

mod arguments;
mod compat;
mod construction;
mod emit;
mod overrides;
mod protocol_args;
mod returns;
mod selectors;

pub(super) use protocol_args::UnionArgCompat;
pub(super) use selectors::ProtocolSendResolution;
