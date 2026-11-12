// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for `manifest`, split by concern (BT-4700).

use super::*;
use beamtalk_core::compilation::GitReference;
use tempfile::TempDir;

pub(super) fn write_manifest(dir: &TempDir, content: &str) -> Utf8PathBuf {
    let path = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
    fs::write(path.join("beamtalk.toml"), content).unwrap();
    path
}

mod dependencies;
mod diagnostics;
mod hex;
mod native;
mod package_name;
mod parse;
mod release;
