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
use camino::Utf8PathBuf;

/// Collects dependency/native `.beam` ebin directories for `layout`'s project.
///
/// Duplicates the directory-scanning logic of `beamtalk-cli`'s (binary-only)
/// `commands::deps::collect_dep_ebin_paths` + `commands::build::
/// collect_rebar3_ebin_paths`, so this module doesn't need to depend on the
/// `commands` module tree (which lives in the CLI binary, not this library
/// crate). Each side is a handful of `read_dir` calls over paths `BuildLayout`
/// already owns, so duplication carries little drift risk — unlike the
/// FFI-spec-extraction logic itself, which is *not* duplicated (`beamtalk-
/// core`'s `ffi_type_specs` is its single source of truth, consumed by
/// `beamtalk-cli`, `beamtalk-mcp`, and `beamtalk-lsp`).
fn collect_dependency_ebin_dirs(layout: &BuildLayout) -> Vec<Utf8PathBuf> {
    let mut dirs = Vec::new();

    // Path dependencies: `_build/deps/*/ebin/`.
    if let Ok(entries) = std::fs::read_dir(layout.deps_dir()) {
        for entry in entries.flatten() {
            let ebin = entry.path().join("ebin");
            if ebin.is_dir() {
                if let Ok(utf8) = Utf8PathBuf::from_path_buf(ebin) {
                    dirs.push(utf8);
                }
            }
        }
    }

    // The project's own native/ code, compiled to `_build/dev/native/ebin/`.
    let native_ebin = layout.native_ebin_dir();
    if native_ebin.exists() {
        dirs.push(native_ebin);
    }

    // rebar3 hex/git deps: `_build/dev/native/default/lib/*/ebin/`.
    let lib_dir = layout.rebar_lib_dir();
    if let Ok(entries) = std::fs::read_dir(&lib_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let ebin = path.join("ebin");
            if ebin.is_dir() {
                if let Ok(utf8) = Utf8PathBuf::from_path_buf(ebin) {
                    dirs.push(utf8);
                }
            }
        }
    }

    dirs.sort();
    dirs.dedup();
    dirs
}

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
    let dep_ebin_dirs = collect_dependency_ebin_dirs(layout);
    beamtalk_core::ffi_type_specs::extract_type_specs(&layout.type_cache_dir(), &dep_ebin_dirs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn collect_dependency_ebin_dirs_empty_project() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        let dirs = collect_dependency_ebin_dirs(&layout);
        assert!(dirs.is_empty(), "fresh project has no dependency ebin dirs");
    }

    #[test]
    fn collect_dependency_ebin_dirs_finds_path_dep_ebin() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        let dep_ebin = layout.deps_dir().join("json").join("ebin");
        fs::create_dir_all(&dep_ebin).unwrap();

        let dirs = collect_dependency_ebin_dirs(&layout);
        assert!(
            dirs.contains(&dep_ebin),
            "should find the path dependency's ebin dir: {dirs:?}"
        );
    }
}
