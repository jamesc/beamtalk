// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script for beamtalk-cli.
//!
//! Injects the `BEAMTALK_VERSION` compile-time env var from VERSION + git state.
//! The Erlang runtime is built by `just build-erlang`, not here.

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = Path::new(&manifest_dir)
        .parent()
        .and_then(Path::parent)
        .expect("Cannot find workspace root");

    // --- Version injection from VERSION file + git state ---
    emit_beamtalk_version(workspace_root);
}

/// Read VERSION file and git state, then expose `BEAMTALK_VERSION` at compile time.
///
/// On a release tag: `0.2.0`
/// Off a tag:        `0.2.0-dev+abc1234`
fn emit_beamtalk_version(workspace_root: &Path) {
    let version_file = workspace_root.join("VERSION");
    println!("cargo:rerun-if-changed={}", version_file.display());

    // Track git state so version is recomputed on new commits/tags.
    // Uses `git rev-parse --git-path` to resolve each path correctly in both
    // regular repos and worktrees (where HEAD is worktree-private but refs
    // and packed-refs live in the shared git-common-dir).
    for p in ["HEAD", "refs/heads", "refs/tags", "packed-refs"] {
        if let Some(path) = git_path(p) {
            println!("cargo:rerun-if-changed={path}");
        }
    }

    let base = fs::read_to_string(&version_file)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", version_file.display()));
    let base = base.trim();
    assert!(
        !base.is_empty(),
        "VERSION file is empty: {}",
        version_file.display()
    );
    assert!(
        base.chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '+')),
        "VERSION contains invalid characters (allowed: [A-Za-z0-9.+-]): {}",
        version_file.display()
    );

    let version = if git_on_tag() {
        base.to_string()
    } else if let Some(sha) = git_short_sha() {
        format!("{base}-dev+{sha}")
    } else {
        base.to_string()
    };

    println!("cargo:rustc-env=BEAMTALK_VERSION={version}");
}

fn git_on_tag() -> bool {
    Command::new("git")
        .args(["describe", "--exact-match", "--tags", "HEAD"])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}

fn git_path(path: &str) -> Option<String> {
    Command::new("git")
        .args(["rev-parse", "--git-path", path])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
}

fn git_short_sha() -> Option<String> {
    Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
}
