// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script for beamtalk-cli.
//!
//! Injects the `BEAMTALK_VERSION` compile-time env var from VERSION + git state,
//! and the `BEAMTALK_SPEC_MAPPING_STAMP` env var (BT-2852) — a content hash of
//! `beamtalk_spec_reader.erl` used to invalidate the FFI type-spec cache when
//! the compiler's Erlang→Beamtalk type-mapping logic changes. The Erlang
//! runtime is built by `just build-erlang`, not here.

use std::env;
use std::path::Path;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = Path::new(&manifest_dir)
        .parent()
        .and_then(Path::parent)
        .expect("Cannot find workspace root");

    beamtalk_build::emit_beamtalk_version(workspace_root);
    beamtalk_build::emit_spec_mapping_stamp(workspace_root);
}
