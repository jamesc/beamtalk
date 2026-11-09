// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test-only version-lock fixtures (BT-4329).
//!
//! The "node built from an older toolchain" `versions` map and the
//! `describe` reply carrying it are needed by both the async clients'
//! tests (`beamtalk-lsp`, `beamtalk-mcp`, via [`crate::test_support`]) and
//! the synchronous `beamtalk-cli` tests, so they live here with no async
//! dependency, behind the same `test-ports` feature as
//! [`crate::test_ports`] (the `test` feature implies it).

use serde_json::{Value, json};

/// The `versions` map a node built from an older toolchain reports in its
/// `describe` reply — guaranteed to mismatch any real build on both
/// `beamtalk` and `protocol`.
#[must_use]
pub fn stale_versions() -> Value {
    json!({"beamtalk": "0.0.0-stale", "protocol": "0.0"})
}

/// The `versions` map a node built from this checkout reports.
#[must_use]
pub fn matching_versions() -> Value {
    json!({
        "beamtalk": crate::handshake::beamtalk_version(),
        "protocol": crate::handshake::PROTOCOL_VERSION,
    })
}

/// The JSON reply a node sends to the `describe` `request`: echoes its
/// `id`, carries `versions`, or no `versions` map at all (like a node
/// predating the version handshake) when `None`.
#[must_use]
pub fn describe_reply_json(request: &Value, versions: Option<&Value>) -> Value {
    let mut reply = json!({"id": request.get("id"), "status": ["done"], "ops": {}});
    if let Some(versions) = versions {
        reply["versions"] = versions.clone();
    }
    reply
}
