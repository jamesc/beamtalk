// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for parity integration-test binaries.

use std::path::Path;

/// Recursively copy a directory tree from `src` to `dst`.
///
/// `dst` is created if it does not exist. Existing files are overwritten.
pub fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}
