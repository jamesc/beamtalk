// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared corpus-walking helpers for `.bt` corpus tests.
//!
//! Multiple corpus test suites need "every `.bt` file under `stdlib/src` and
//! `examples/`" — the byte-span round-trip proof
//! ([`super::method_span_corpus_tests`], ADR 0082 Phase 0) and the
//! method-category divider validation
//! ([`super::method_category_corpus_tests`], BT-2626) both walk the same
//! corpus. This module is the single place that knows where the corpus lives
//! and how to walk it, so the two suites can't drift on what "the corpus"
//! means (see `docs/development/architecture-principles.md` § Duplication &
//! the Shared-Leaf-Module Pattern).

use std::path::{Path, PathBuf};

/// Returns the repository root (`CARGO_MANIFEST_DIR/../..`).
pub(super) fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf()
}

/// Recursively collects every `.bt` file under `dir`.
fn collect_bt_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut entries: Vec<_> = entries.filter_map(Result::ok).map(|e| e.path()).collect();
    // Deterministic order for stable test output.
    entries.sort();
    for path in entries {
        if path.is_dir() {
            collect_bt_files(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "bt") {
            out.push(path);
        }
    }
}

/// The corpus directories: every `.bt` file lives under one of these.
pub(super) fn corpus_dirs() -> [PathBuf; 2] {
    let root = repo_root();
    [root.join("stdlib/src"), root.join("examples")]
}

/// Whether the corpus directories are present in this checkout.
///
/// They are absent when the crate is built from a source distribution or a
/// partial checkout that omits the workspace `stdlib/` and `examples/` trees. In
/// that case the corpus tests skip rather than hard-fail (a present-but-
/// unreadable *file*, by contrast, is always a hard failure).
pub(super) fn corpus_present() -> bool {
    corpus_dirs().iter().any(|dir| dir.is_dir())
}

/// Gathers the whole corpus: every `.bt` file under `stdlib/src` and `examples`.
pub(super) fn corpus_files() -> Vec<PathBuf> {
    let mut files = Vec::new();
    for dir in corpus_dirs() {
        collect_bt_files(&dir, &mut files);
    }
    files
}

/// Reads a corpus file, hard-failing (with the path + error) if it is present
/// but cannot be read. Unlike skipping an absent *directory*, an unreadable file
/// that the walk already discovered must never be silently ignored — that would
/// let a corpus proof go false-green.
pub(super) fn read_corpus_file(path: &Path) -> String {
    std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("could not read corpus file {}: {e}", path.display()))
}
