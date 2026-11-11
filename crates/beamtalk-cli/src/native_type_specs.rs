// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk-cli`-specific glue for OTP/dependency Erlang FFI type-spec
//! extraction (ADR 0075).
//!
//! The extraction logic itself (`beamtalk_build_worker` spawning,
//! OTP/dependency `.beam` discovery, tiered on-disk caching) moved into
//! `beamtalk-core`'s `ffi_type_specs` module, so `beamtalk-lsp` can call the
//! same single source of truth `beamtalk build`/`beamtalk lint` and MCP
//! `lint`/`diagnostic_summary` already share, without a
//! `beamtalk-lsp -> beamtalk-cli` dependency (forbidden — see
//! `docs/development/architecture-principles.md`). This module now only
//! contains the `beamtalk-cli`-specific glue: resolving `BuildLayout`'s
//! `_build/` paths into the cache dir + dependency ebin dirs
//! `ffi_type_specs::extract_type_specs` needs.
//!
//! Re-exports the moved items so existing `beam_compiler::X` references
//! throughout the CLI binary (which re-exports from here) keep
//! working unchanged.

pub use beamtalk_core::ffi_type_specs::{
    OtpDiscovery, current_spec_mapping_stamp, discover_dependency_beam_files,
    discover_otp_beam_files, discover_otp_version, extract_beam_specs, extract_beam_specs_tiered,
    load_type_cache_registry, shared_otp_cache_dir, spawn_build_worker_node,
};

use crate::build_layout::BuildLayout;
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use camino::Utf8PathBuf;

/// ADR 0075 Phase 1: Extract type specs from OTP and dependency
/// `.beam` files for a manifest-backed project and cache them.
///
/// Thin `BuildLayout`-resolving wrapper around `beamtalk_core::
/// ffi_type_specs::extract_type_specs` — the single source of
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

/// Extract native type specs from runtime `.beam` files for FFI type inference.
///
/// Discovers `.beam` files from the runtime and stdlib Erlang ebin directories
/// and extracts their `-spec` attributes. Returns `None` if no specs could be
/// extracted (e.g., runtime not yet compiled).
///
/// The registry `beamtalk build-stdlib` types the stdlib against, and the
/// fallback `beamtalk lint` and MCP `lint` use for a `stdlib/src` target,
/// which has no `beamtalk.toml` (BT-4643).
#[must_use]
pub fn extract_stdlib_type_specs() -> Option<NativeTypeRegistry> {
    use crate::repl_startup;

    // Use the same runtime discovery as the build worker so paths stay in sync
    // with the runtime layout (dev vs installed).
    let (runtime_dir, layout): (std::path::PathBuf, _) =
        if let Ok(result) = repl_startup::find_runtime_dir_with_layout() {
            result
        } else {
            tracing::debug!("Runtime not found — skipping FFI type spec extraction");
            return None;
        };
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    // Collect ebin directories that contain Erlang FFI modules with -spec attributes.
    // Convert from std PathBuf to camino Utf8PathBuf (required by beam_compiler API).
    let mut ebin_dirs: Vec<Utf8PathBuf> = Vec::new();
    let candidates: Vec<std::path::PathBuf> = vec![
        paths.runtime_ebin,
        paths.stdlib_erlang_ebin,
        paths.workspace_ebin,
        paths.compiler_ebin,
    ];
    for dir in candidates {
        if dir.exists() {
            if let Ok(utf8) = Utf8PathBuf::from_path_buf(dir) {
                ebin_dirs.push(utf8);
            }
        }
    }

    let beam_files = discover_dependency_beam_files(&ebin_dirs);
    if beam_files.is_empty() {
        tracing::debug!("No runtime .beam files found for stdlib FFI type specs");
        return None;
    }

    // Cache directory under the runtime _build to avoid re-extracting specs
    let cache_dir: Utf8PathBuf = Utf8PathBuf::from_path_buf(runtime_dir.join("_build/type_cache"))
        .unwrap_or_else(|p| Utf8PathBuf::from(p.to_string_lossy().as_ref()));

    match extract_beam_specs(&beam_files, &cache_dir) {
        Ok(mut registry) => {
            // Runtime `beamtalk_*` specs are first-party: they may prove
            // direct FFI calls (BT-4507).
            registry.trust_all_modules();
            if registry.module_count() > 0 {
                tracing::info!(
                    modules = registry.module_count(),
                    functions = registry.function_count(),
                    "Extracted FFI type specs for stdlib build"
                );
            }
            Some(registry)
        }
        Err(e) => {
            tracing::debug!("Failed to extract type specs for stdlib build: {e}");
            None
        }
    }
}
