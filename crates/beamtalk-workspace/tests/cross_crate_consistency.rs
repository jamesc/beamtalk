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
    generate_workspace_id, hash_workspace_path_string, read_cookie_file, read_port_file,
    workspaces_base_dir,
};
use std::fs;

/// `hash_workspace_path_string` must keep producing the same 12-hex-char ID
/// for a fixed literal input — SHA-256 of the string, first 6 bytes
/// hex-encoded. A true golden test: the expected value below was computed
/// once (`python3 -c "import hashlib; print(hashlib.sha256(b'...').hexdigest()[:12])"`)
/// and is now the contract callers (CLI, MCP) rely on for stable workspace
/// directories across runs. Fixed to a literal string rather than
/// `std::env::temp_dir()` so the expected value doesn't vary per OS/CI
/// runner, and calls the production hashing function directly rather than
/// re-deriving SHA-256 inline — see `docs/development/architecture-principles.md`
/// §7 (a test that hand-reimplements the algorithm it's checking is the
/// same "two implementations, hand-kept in sync" smell this rule exists to
/// catch).
#[test]
fn test_workspace_id_hash_is_stable_for_known_input() {
    assert_eq!(
        hash_workspace_path_string("/beamtalk/golden-test/fixed-workspace-path"),
        "43e09723da2c",
        "hash_workspace_path_string must produce a stable, known 12-hex-char ID for a fixed input"
    );
}

/// `generate_workspace_id` (the canonicalize-then-hash public entry point)
/// delegates to `hash_workspace_path_string` and returns a well-formed
/// 12-hex-char ID for a real path. Structural only — the exact value isn't
/// pinned here because `canonicalize()` resolves `std::env::temp_dir()`
/// differently per OS/CI runner; the algorithm itself is pinned by the
/// golden test above.
#[test]
fn test_generate_workspace_id_returns_well_formed_id_for_real_path() {
    let path = std::env::temp_dir();
    let id = generate_workspace_id(&path).unwrap();
    assert_eq!(id.len(), 12, "workspace ID must always be 12 hex chars");
    assert!(
        id.chars().all(|c| c.is_ascii_hexdigit()),
        "workspace ID must be lowercase hex, got {id:?}"
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
