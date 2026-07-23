// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk-cli`-specific glue for OTP/dependency Erlang FFI type-spec
//! extraction (ADR 0075).
//!
//! BT-2859: The extraction logic itself (`beamtalk_build_worker` spawning,
//! OTP/dependency `.beam` discovery, tiered on-disk caching) moved into
//! `beamtalk-core`'s `ffi_type_specs` module, so `beamtalk-lsp` can call the
//! same single source of truth `beamtalk build`/`beamtalk lint` and MCP
//! `lint`/`diagnostic_summary` (BT-2858) already share, without a
//! `beamtalk-lsp -> beamtalk-cli` dependency (forbidden — see
//! `docs/development/architecture-principles.md`). This module now only
//! contains the `beamtalk-cli`-specific glue: resolving `BuildLayout`'s
//! `_build/` paths into the cache dir + dependency ebin dirs
//! `ffi_type_specs::extract_type_specs` needs.
//!
//! Re-exports the moved items so existing `beam_compiler::X` references
//! throughout the CLI binary (which re-exports from here — BT-2858) keep
//! working unchanged.

pub use beamtalk_core::ffi_type_specs::{
    OtpDiscovery, current_spec_mapping_stamp, discover_dependency_beam_files,
    discover_otp_beam_files, discover_otp_version, extract_beam_specs, extract_beam_specs_tiered,
    load_type_cache_registry, shared_otp_cache_dir, spawn_build_worker_node,
};

use crate::build_layout::BuildLayout;
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;

/// ADR 0075 Phase 1 / BT-2851: Extract type specs from OTP and dependency
/// `.beam` files for a manifest-backed project and cache them.
///
/// Thin `BuildLayout`-resolving wrapper around `beamtalk_core::
/// ffi_type_specs::extract_type_specs` (BT-2859) — the single source of
/// truth for populating a [`NativeTypeRegistry`] from `.beam` files, shared
/// by `beamtalk build`/`beamtalk lint`, `beamtalk-mcp`'s `lint`/
/// `diagnostic_summary` tools, and `beamtalk-lsp`.
///
/// Non-fatal: if spec extraction fails (e.g., runtime not compiled), returns
/// `None` rather than erroring — callers fall back to untyped FFI checking.
pub fn extract_project_type_specs(layout: &BuildLayout) -> Option<NativeTypeRegistry> {
    let dep_ebin_dirs =
        beamtalk_core::ffi_type_specs::collect_project_dependency_ebin_dirs(layout.project_root());
    beamtalk_core::ffi_type_specs::extract_type_specs(&layout.type_cache_dir(), &dep_ebin_dirs)
}
