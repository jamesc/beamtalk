// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script to inject `BEAMTALK_VERSION` from the workspace VERSION file.

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
