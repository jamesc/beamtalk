// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script for beamtalk-repl-protocol.
//!
//! Injects the `BEAMTALK_VERSION` compile-time env var from VERSION + git state,
//! so `handshake::beamtalk_version()` reports the same toolchain version every
//! other binary crate bakes in (all derive from the one workspace `VERSION`
//! file via `beamtalk-build`). The handshake gate compares this against the
//! workspace node's Erlang app `vsn`, which comes from the same source via
//! `scripts/version.escript`.

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
