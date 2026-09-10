// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script to inject compile-time metadata.
//!
//! Injects the `BEAMTALK_SPEC_MAPPING_STAMP` compile-time env var that
//! `ffi_type_specs` uses to invalidate its on-disk FFI type-spec cache
//! when the compiler's Erlang→Beamtalk type-mapping logic changes — lives
//! here because `ffi_type_specs` itself lives in this crate.
//!
//! This crate's `build.rs` does not generate a `STDLIB_CLASS_NAMES` table
//! for `beamtalk-codegen`: `core_erlang::value_type_codegen`'s
//! `is_known_stdlib_type()` delegates directly to
//! `ClassHierarchy::is_generated_builtin_class` (ADR 0119 step 0), so
//! `beamtalk-codegen` has no `build.rs` of its own.

use std::env;
use std::path::Path;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = Path::new(&manifest_dir)
        .parent()
        .and_then(Path::parent)
        .expect("Cannot find workspace root");

    beamtalk_build::emit_spec_mapping_stamp(workspace_root);
}
