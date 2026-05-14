// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script for beamtalk-cli.
//!
//! Injects the `BEAMTALK_VERSION` compile-time env var from VERSION + git state.
//! The Erlang runtime is built by `just build-erlang`, not here.

use std::env;
use std::path::Path;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = Path::new(&manifest_dir)
        .parent()
        .and_then(Path::parent)
        .expect("Cannot find workspace root");

    beamtalk_build::emit_beamtalk_version(workspace_root);
}
