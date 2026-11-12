// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Offline package analysis shared by CLI and MCP tooling.
//!
//! **DDD Context:** Build System
//!
//! - [`manifest`] — `beamtalk.toml` parsing and validation
//! - [`build_layout`] — centralised `_build/` path construction
//! - [`dependency_classes`] — best-effort, offline dependency class resolution
//! - [`ffi_type_registry`] — ADR 0075's FFI type resolution chain
//! - [`native_type_specs`] — Erlang FFI type-spec extraction
//! - [`erlang_lint`] — text-based lint checks for native `.erl` files
//! - [`lint_pipeline`] — the `beamtalk lint` analysis pipeline
//! - [`content_hash`] — the shared SHA-256 hex digest

pub mod build_layout;
pub mod content_hash;
pub mod dependency_classes;
// Lives in `beamtalk-project` so the LSP applies the same rule (BT-4611).
pub use beamtalk_project::dependency_extensions;
pub mod erlang_lint;
pub mod ffi_type_registry;
pub mod lint_pipeline;
pub mod manifest;
pub mod native_type_specs;
