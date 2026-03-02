// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-crate consistency tests.
//!
//! Verifies that the shared `beamtalk-workspace` crate produces workspace IDs
//! and reads port/cookie files identically to the logic previously duplicated
//! in `beamtalk-cli` and `beamtalk-mcp`.

use beamtalk_workspace::{
    generate_workspace_id, read_cookie_file, read_port_file, workspaces_base_dir,
};
use std::fs;

/// The CLI's algorithm (before extraction) used `format!("{result:x}")[..12]`.
/// The MCP's algorithm used a custom `hex_encode(&result[..6])`.
/// Both produce 12 hex chars. This test verifies the shared implementation
/// matches both algorithms for a known input.
#[test]
fn test_workspace_id_matches_cli_algorithm() {
    use sha2::{Digest, Sha256};
    use std::fmt::Write as _;

    // Use temp_dir which exists on all platforms.
    let path = std::env::temp_dir();
    let canonical = path.canonicalize().unwrap();
    let path_str = canonical.to_str().unwrap();

    let mut hasher = Sha256::new();
    hasher.update(path_str.as_bytes());
    let result = hasher.finalize();
    // Build 12 hex chars from the first 6 bytes (avoids str byte-slicing).
    let cli_id = result[..6]
        .iter()
        .fold(String::with_capacity(12), |mut s, b| {
            let _ = write!(s, "{b:02x}");
            s
        });

    let shared_id = generate_workspace_id(&path).unwrap();
    assert_eq!(
        shared_id, cli_id,
        "Shared crate must produce the same ID as the old CLI algorithm"
    );
}

/// Simulate the MCP's old `hex_encode` algorithm and verify it matches.
#[test]
fn test_workspace_id_matches_mcp_algorithm() {
    use sha2::{Digest, Sha256};

    let path = std::env::temp_dir();
    let canonical = path.canonicalize().unwrap();
    // MCP used to_string_lossy; for valid UTF-8 paths this is identical to to_str().unwrap()
    let path_str = canonical.to_string_lossy();

    let mut hasher = Sha256::new();
    hasher.update(path_str.as_bytes());
    let result = hasher.finalize();

    // MCP's old hex_encode(&result[..6])
    let mcp_id: String = result[..6]
        .iter()
        .fold(String::with_capacity(12), |mut s, b| {
            use std::fmt::Write;
            let _ = write!(s, "{b:02x}");
            s
        });

    let shared_id = generate_workspace_id(&path).unwrap();
    assert_eq!(
        shared_id, mcp_id,
        "Shared crate must produce the same ID as the old MCP algorithm for UTF-8 paths"
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
