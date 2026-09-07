// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for the MCP server (`beamtalk-mcp`'s tool handlers and their
//! supporting helpers).
//!
//! Tests are organized into feature-focused sub-modules:
//! - [`lint_diagnostics`] — near-miss dividers, module-name mismatches,
//!   stub-file suppression, native-declaration locations, and the FFI /
//!   native-type-registry fixtures they exercise
//! - [`lint_dependency_fixtures`] — cross-file visibility leaks (E0402),
//!   git and transitive git dependencies, unreadable package files
//! - [`search`] — `search_examples` / `search_classes` / `list_classes`
//! - [`validation`] — `validate_class_name` / `validate_erlang_module_name`
//!   / `validate_selector`
//! - [`tool_registration`] — tool-router registration checks and the small
//!   offline handlers
//! - [`repl_query_tools`] — MCP tool handlers that query a live REPL,
//!   driven through the [`fake_mcp`] harness below
//! - [`repl_offline_tools`] — the offline (no-REPL) lint/search paths,
//!   tracing tools, and `describe/list_packages/package_classes`
//! - [`repl_editing_tools`] — tool handlers that mutate the live image,
//!   plus the `tool_router` and `ServerHandler::get_info` dispatch surface

pub use super::*;

/// True when the test process is running as Unix root. Root bypasses
/// POSIX permission bits, so `chmod 000` fixtures cannot produce the
/// expected `permission_denied` errors. Tests that rely on those
/// fixtures bail early when this returns true (e.g. CI sandboxes).
#[cfg(unix)]
pub(crate) fn running_as_root() -> bool {
    std::process::Command::new("id")
        .arg("-u")
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .is_some_and(|s| s.trim() == "0")
}

// ------------------------------------------------------------------
// MCP tool handlers against a fake REPL (BT-3324)
// ------------------------------------------------------------------
//
// `server.rs`'s tool handlers take a concrete `Arc<ReplClient>`, not a
// trait object — the same shape BT-3325 found in beamtalk-lsp's
// runtime.rs, with no mockable seam to introduce. Rather than add one
// neither the CLI nor the REPL wire protocol needs, this fake stands in
// for a real REPL server: a loopback WebSocket listener that performs the
// ADR 0020 auth handshake and then answers requests through a per-test
// responder closure, so a real `ReplClient` connects to it exactly as it
// would to `beamtalk repl` — and the tool handler methods below run
// as ordinary async fns against it, unignored and BEAM-free.
// BT-3331: the loopback WS server performing the ADR 0020 handshake
// (listener bind, handshake frames, request/response loop) used to be
// hand-rolled here; it's now shared with `beamtalk-lsp`'s equivalent
// fake workspace via `beamtalk_repl_protocol::test_support` (see that
// module's doc comment for the full extraction rationale). `FakeRepl`
// aliases the shared server type under this file's existing name, so
// every call site below (`fake.port`) is unchanged.
pub(crate) use beamtalk_repl_protocol::test_support::{HandshakeMode, spawn as spawn_ws, text};

pub(crate) type FakeRepl = beamtalk_repl_protocol::test_support::FakeWsServer;

/// Frame the fake REPL sends back for one received request.
pub(crate) type FakeReplResponder =
    Box<dyn Fn(&serde_json::Value) -> serde_json::Value + Send + Sync>;

/// Spawn a fake REPL on an ephemeral loopback port. Performs the ADR 0020
/// handshake (`auth-required` -> client `auth` -> `auth_ok` ->
/// `session-started`) unconditionally — handshake robustness itself is
/// `client.rs`'s concern (covered live via `just test-mcp`) — then
/// answers every subsequent request via `responder`, defaulting `id`
/// (echoed from the request) and `status` (`["done"]`) when the
/// responder didn't set them.
pub(crate) async fn spawn_fake_repl(responder: FakeReplResponder) -> FakeRepl {
    let responder: beamtalk_repl_protocol::test_support::Responder = Box::new(move |request| {
        let mut reply = responder(request);
        if reply.get("id").is_none() {
            reply["id"] = request
                .get("id")
                .cloned()
                .unwrap_or(serde_json::Value::Null);
        }
        if reply.get("status").is_none() {
            reply["status"] = serde_json::json!(["done"]);
        }
        vec![text(&reply)]
    });
    spawn_ws(HandshakeMode::Ok, "fake-session", responder).await
}

/// A responder that always answers with `value` in the response's
/// top-level `value` field (the shape every `evaluate`-backed tool reads).
pub(crate) fn respond_value(value: serde_json::Value) -> FakeReplResponder {
    Box::new(move |_req| serde_json::json!({"value": value.clone()}))
}

/// A responder that returns an arbitrary response object verbatim, for
/// shaping fields `respond_value` doesn't cover (`class_list`, `actors`,
/// `completions`, `errors`, …).
pub(crate) fn respond(response: serde_json::Value) -> FakeReplResponder {
    Box::new(move |_req| response.clone())
}

/// A responder that always errors with `message`.
pub(crate) fn respond_error(message: &'static str) -> FakeReplResponder {
    Box::new(move |_req| serde_json::json!({"status": ["done", "error"], "error": message}))
}

/// A responder that dispatches on the request's `code` field (the
/// `evaluate` payload) — for handlers like `list_packages` that issue
/// more than one distinct `evaluate` call per tool invocation.
pub(crate) fn respond_by_code(cases: Vec<(&'static str, serde_json::Value)>) -> FakeReplResponder {
    Box::new(move |req| {
        let code = req.get("code").and_then(|v| v.as_str()).unwrap_or("");
        cases
            .iter()
            .find(|(prefix, _)| code.starts_with(prefix))
            .map_or_else(
                || serde_json::json!({"value": serde_json::Value::Null}),
                |(_, v)| v.clone(),
            )
    })
}

/// Connect a real `ReplClient` to a fake REPL and wrap it in a
/// `BeamtalkMcp`. The `FakeRepl` handle must outlive the returned server
/// — dropping it aborts the listener task, which owns the socket.
pub(crate) async fn fake_mcp(responder: FakeReplResponder) -> (FakeRepl, BeamtalkMcp) {
    let fake = spawn_fake_repl(responder).await;
    let client = ReplClient::connect(fake.port, "test-cookie", None)
        .await
        .expect("fake REPL handshake should succeed");
    (fake, BeamtalkMcp::new(Arc::new(client)))
}

/// Concatenate a `CallToolResult`'s text content blocks for substring
/// assertions, ignoring non-text blocks (none of these tools emit any).
pub(crate) fn call_text(result: &CallToolResult) -> String {
    result
        .content
        .iter()
        .filter_map(ContentBlock::as_text)
        .map(|t| t.text.as_str())
        .collect::<Vec<_>>()
        .join("\n")
}

mod lint_dependency_fixtures;
mod lint_diagnostics;
mod repl_editing_tools;
mod repl_offline_tools;
mod repl_query_tools;
mod search;
mod tool_registration;
mod validation;
