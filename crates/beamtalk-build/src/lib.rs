// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared build-script helpers for beamtalk crates.
//!
//! Provides `emit_beamtalk_version` which reads the workspace `VERSION` file
//! and git state, then exposes the `BEAMTALK_VERSION` compile-time env var.
//! Also provides `emit_spec_mapping_stamp`, which hashes
//! `beamtalk_spec_reader.erl` and exposes the result as
//! `BEAMTALK_SPEC_MAPPING_STAMP` (BT-2852).
//!
//! # Usage
//!
//! In a crate's `build.rs`:
//!
//! ```rust,ignore
//! fn main() {
//!     let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
//!     let workspace_root = std::path::Path::new(&manifest_dir)
//!         .parent()
//!         .and_then(std::path::Path::parent)
//!         .expect("Cannot find workspace root");
//!     beamtalk_build::emit_beamtalk_version(workspace_root);
//! }
//! ```

use std::fs;
use std::path::Path;
use std::process::Command;

/// Read VERSION file and git state, then expose `BEAMTALK_VERSION` at compile time.
///
/// On a release tag: `0.2.0`
/// Off a tag:        `0.2.0-dev+abc1234`
///
/// # Panics
///
/// Panics if the `VERSION` file cannot be read, is empty, or contains
/// characters outside `[A-Za-z0-9.+-]`.
pub fn emit_beamtalk_version(workspace_root: &Path) {
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

/// Hash `runtime/apps/beamtalk_compiler/src/beamtalk_spec_reader.erl` and
/// expose the result as the `BEAMTALK_SPEC_MAPPING_STAMP` compile-time env var
/// (BT-2852).
///
/// The FFI type-spec cache (`_build/type_cache/`, and the shared
/// OTP-version-keyed tier) keys cached specs by module name, `.beam` mtime,
/// and OTP version only — none of which change just because the *compiler's*
/// Erlang→Beamtalk type-mapping logic (`beamtalk_spec_reader.erl`'s
/// `map_type/1` and friends) changed. Baking a content hash of that file into
/// the binary at compile time gives cache entries a stamp to compare against:
/// a build whose mapping logic differs produces a different stamp, so stale
/// entries are detected as a miss rather than silently reused forever.
///
/// Deriving the stamp from a hash (rather than a hand-maintained constant)
/// means there is nothing to remember to bump — any change to the reader's
/// source, including comment-only edits, rotates the stamp automatically.
/// The hash itself runs once per `cargo build` (via `rerun-if-changed`), not
/// per `beamtalk` invocation, so comparing it at runtime is a cheap string
/// comparison against a `&'static str` baked in via `env!()`.
///
/// # Panics
///
/// Panics if `beamtalk_spec_reader.erl` cannot be read — this file is
/// committed source, so its absence indicates a broken checkout.
pub fn emit_spec_mapping_stamp(workspace_root: &Path) {
    let reader_path = workspace_root
        .join("runtime")
        .join("apps")
        .join("beamtalk_compiler")
        .join("src")
        .join("beamtalk_spec_reader.erl");
    println!("cargo:rerun-if-changed={}", reader_path.display());

    let content = fs::read(&reader_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", reader_path.display()));

    let hash = fnv1a_64(&content);
    println!("cargo:rustc-env=BEAMTALK_SPEC_MAPPING_STAMP={hash:016x}");
}

/// FNV-1a 64-bit hash. Not cryptographic — used only as a cheap, deterministic
/// content fingerprint so the type-mapping stamp doesn't depend on the
/// standard library's hasher (which offers no cross-version stability
/// guarantee) or an external hashing crate.
fn fnv1a_64(data: &[u8]) -> u64 {
    const FNV_OFFSET_BASIS: u64 = 0xcbf2_9ce4_8422_2325;
    const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;
    let mut hash = FNV_OFFSET_BASIS;
    for &byte in data {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash
}

#[cfg(test)]
mod fnv_tests {
    use super::fnv1a_64;

    #[test]
    fn deterministic_for_same_input() {
        assert_eq!(fnv1a_64(b"hello world"), fnv1a_64(b"hello world"));
    }

    #[test]
    fn differs_for_different_input() {
        assert_ne!(fnv1a_64(b"hello world"), fnv1a_64(b"hello worlds"));
    }

    #[test]
    fn matches_known_fnv1a_64_vector() {
        // Standard FNV-1a 64-bit test vector for the empty string.
        assert_eq!(fnv1a_64(b""), 0xcbf2_9ce4_8422_2325);
    }
}
