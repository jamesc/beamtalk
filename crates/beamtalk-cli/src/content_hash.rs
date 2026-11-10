// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The one digest-to-hex-string routine shared by the CLI binary (build
//! cache keys) and its library target (the FFI stub-tier fingerprint in
//! [`crate::ffi_type_registry`]).

use beamtalk_workspace::hex_encode;
use sha2::{Digest, Sha256};

/// SHA-256 hash of arbitrary bytes, as a lowercase hex string.
#[must_use]
pub fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    hex_encode(&hasher.finalize())
}
