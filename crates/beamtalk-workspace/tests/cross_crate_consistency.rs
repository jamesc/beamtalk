// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-crate consistency tests.
//!
//! `beamtalk-workspace` is the single source of truth for workspace IDs and
//! for reading port/cookie files; `beamtalk-cli` and `beamtalk-mcp` both
//! import it rather than re-implementing the hashing/parsing logic.
//!
//! This file used to duplicate the CLI's and MCP's (pre-extraction) hashing
//! algorithms inline just to assert `generate_workspace_id` agreed with both.
//! Per the consistency-test disposition rule
//! (`docs/development/architecture-principles.md` §7 — a test between two
//! *deletable* copies is a smell, not enforcement), those re-implementations
//! were deleted once the CLI/MCP duplicates they modeled were gone; what
//! remains is an ordinary golden test of the one surviving implementation.

use beamtalk_workspace::{
    generate_workspace_id, read_cookie_file, read_port_file, workspaces_base_dir,
};
use std::fs;

/// `generate_workspace_id` must keep producing the same 12-hex-char ID for a
/// known input — SHA-256 of the path string, first 6 bytes hex-encoded.
/// This is a golden test, not a consistency check: the expected value was
/// computed once and is now the contract callers (CLI, MCP) rely on for
/// stable workspace directories across runs.
#[test]
fn test_workspace_id_is_stable_for_known_input() {
    use sha2::{Digest, Sha256};
    use std::fmt::Write as _;

    // Use temp_dir which exists on all platforms.
    let path = std::env::temp_dir();
    let canonical = path.canonicalize().unwrap();
    let path_str = canonical.to_str().unwrap();

    let mut hasher = Sha256::new();
    hasher.update(path_str.as_bytes());
    let result = hasher.finalize();
    // Expected: 12 hex chars from the first 6 hash bytes.
    let expected_id = result[..6]
        .iter()
        .fold(String::with_capacity(12), |mut s, b| {
            let _ = write!(s, "{b:02x}");
            s
        });

    let shared_id = generate_workspace_id(&path).unwrap();
    assert_eq!(
        shared_id, expected_id,
        "generate_workspace_id must produce a stable 12-hex-char ID for a given path"
    );
    assert_eq!(
        shared_id.len(),
        12,
        "workspace ID must always be 12 hex chars"
    );
}

/// Verify that port + nonce round-trip through `read_port_file`.
#[test]
fn test_port_file_round_trip() {
    let workspace_id = format!("test_consistency_port_{}", std::process::id());
    let dir = workspaces_base_dir().unwrap().join(&workspace_id);
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("port"), "7777\nmynonce42").unwrap();

    let result = read_port_file(&workspace_id).unwrap();
    assert_eq!(
        result,
        Some((7777, Some("mynonce42".to_string()))),
        "read_port_file must return port and nonce"
    );

    let _ = fs::remove_dir_all(&dir);
}

/// Verify that cookie round-trips through `read_cookie_file` with whitespace stripped.
#[test]
fn test_cookie_file_round_trip() {
    let workspace_id = format!("test_consistency_cookie_{}", std::process::id());
    let dir = workspaces_base_dir().unwrap().join(&workspace_id);
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("cookie"), "  secretcookie123  \n").unwrap();

    let result = read_cookie_file(&workspace_id).unwrap();
    assert_eq!(
        result,
        Some("secretcookie123".to_string()),
        "read_cookie_file must strip surrounding whitespace"
    );

    let _ = fs::remove_dir_all(&dir);
}
