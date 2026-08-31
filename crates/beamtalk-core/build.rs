// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script to inject compile-time metadata.
//!
//! Injects the `BEAMTALK_SPEC_MAPPING_STAMP` compile-time env var (BT-2852)
//! that `ffi_type_specs` uses to invalidate its on-disk FFI type-spec cache
//! when the compiler's Erlang→Beamtalk type-mapping logic changes — moved
//! here from `beamtalk-cli`'s `build.rs` (BT-2859) since `ffi_type_specs`
//! itself moved into this crate.
//!
//! BT-3362 (ADR 0117 Decision step 5): the `STDLIB_CLASS_NAMES` /
//! `is_known_stdlib_type()` generator that used to live alongside this moved
//! to `beamtalk-codegen/build.rs` along with its sole consumer,
//! `core_erlang::value_type_codegen`.

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
