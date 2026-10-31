// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared wire contract for the workspace WebSocket auth handshake (ADR 0020).
//!
//! Every Rust surface that connects directly to a workspace over WebSocket —
//! `beamtalk-lsp`, `beamtalk-mcp`, `beamtalk-cli`, and the parity test
//! harness's REPL driver — performs the same four-message sequence before
//! the connection is usable:
//!
//! 1. Server → client: `{"op": "auth-required"}` (`websocket_init/1`).
//! 2. Client → server: `{"type": "auth", "cookie": ..., "client": ..., "resume": ...}`.
//! 3. Server → client: `{"type": "auth_ok"}` or `{"type": "auth_error", "message": ...}`
//!    (`handle_auth/2` in `beamtalk_ws_handler.erl`).
//! 4. Server → client, on success only: `{"op": "session-started", "session": ...}`
//!    (`create_session/3` / `start_or_resume_session/3`).
//!
//! Each of the four callers hand-transcribing this literal JSON shape
//! independently would leave nothing pinning any of them to the Erlang
//! server's actual frame shapes — a rename on the Erlang side (e.g.
//! `auth_ok` → `authOk`) could break every client at runtime with no
//! test failing first. This module is instead the single Rust-side source of
//! truth: every caller builds/recognises these frames through the functions
//! below instead of re-matching JSON fields itself, and the `tests` module
//! below pins this module's behaviour to
//! `runtime/apps/beamtalk_workspace/test/fixtures/ws_auth_handshake_wire_corpus.json`
//! — the same fixture `beamtalk_ws_handler_tests`'s
//! `handshake_pre_auth_frame_matches_shared_wire_corpus_test/0`,
//! `handshake_auth_error_matches_shared_wire_corpus_test/0`, and
//! `handshake_success_matches_shared_wire_corpus/0` pin the *Erlang*
//! production code to. Neither side hand-derives the other's expected
//! values; both read the same file
//! (`docs/development/architecture-principles.md` §6: a rule crossing the
//! Rust/Erlang boundary needs a shared conformance fixture, not a comment).
//!
//! I/O (reading/writing frames, timeouts, reconnect policy) stays with each
//! caller — this module only builds/recognises the JSON, since the four
//! callers use different transports (async `tokio-tungstenite` vs. sync
//! `tungstenite`) and different retry/timeout policies.

use serde_json::Value;

/// The `op` value on the pre-auth welcome frame the workspace sends
/// immediately on connect (`beamtalk_ws_handler:websocket_init/1`).
pub const OP_AUTH_REQUIRED: &str = "auth-required";

/// The `type` value on the client's auth request.
pub const TYPE_AUTH: &str = "auth";

/// The `type` value on a successful auth response.
pub const TYPE_AUTH_OK: &str = "auth_ok";

/// The `type` value on a failed auth response.
pub const TYPE_AUTH_ERROR: &str = "auth_error";

/// The `op` value on the post-auth session frame.
pub const OP_SESSION_STARTED: &str = "session-started";

/// The REPL/workspace wire protocol version this client speaks.
///
/// Must stay identical to `?PROTOCOL_VERSION` in
/// `runtime/apps/beamtalk_runtime/include/beamtalk.hrl` — the workspace node
/// rejects an `auth` request whose `protocol_version` differs, and the client
/// likewise rejects a `describe` response whose `protocol` differs. Bump both
/// together; the shared wire corpus plus the Erlang `describe` conformance
/// test pin the two sides to each other.
pub const PROTOCOL_VERSION: &str = "2.0";

/// The connecting toolchain's Beamtalk version (`BEAMTALK_VERSION`, verbatim —
/// e.g. `0.4.0`, or `0.4.0-dev+<sha>` off a release tag).
///
/// Baked in at compile time from the workspace `VERSION` file + git state (see
/// this crate's `build.rs`), the same source the workspace node's Erlang app
/// `vsn` comes from (`scripts/version.escript`). The server requires an exact
/// match — including the `-dev+<sha>` suffix — so a stale binary can never
/// attach to a node built from a different commit.
#[must_use]
pub fn beamtalk_version() -> &'static str {
    env!("BEAMTALK_VERSION")
}

/// True when `frame` is the pre-auth `{"op": "auth-required"}` welcome the
/// workspace sends immediately on connect, before any auth message is sent.
#[must_use]
pub fn is_auth_required(frame: &Value) -> bool {
    frame.get("op").and_then(Value::as_str) == Some(OP_AUTH_REQUIRED)
}

/// Build the client's `auth` request frame.
///
/// `client_surface` tags the connecting surface (`"repl"`, `"mcp"`, `"lsp"`)
/// so `Workspace sessions` can show where a session originated
/// (`beamtalk_ws_handler:handle_auth/2`'s `Meta.kind`). `resume`, when
/// present, requests resuming a previous session by id.
///
/// `beamtalk_version` is the connecting toolchain's `BEAMTALK_VERSION`
/// (see [`beamtalk_version`]) and `protocol_version` is [`PROTOCOL_VERSION`];
/// the server rejects the request when either differs from its own, so mixed
/// toolchains fail fast with a rebuild hint instead of silently miscompiling.
/// Pass [`beamtalk_version()`] and [`PROTOCOL_VERSION`] — never a hardcoded
/// version string, which would go stale on the next bump.
#[must_use]
pub fn auth_request(
    cookie: &str,
    client_surface: &str,
    resume: Option<&str>,
    beamtalk_version: &str,
    protocol_version: &str,
) -> Value {
    let mut msg = serde_json::json!({
        "type": TYPE_AUTH,
        "cookie": cookie,
        "client": client_surface,
        "beamtalk_version": beamtalk_version,
        "protocol_version": protocol_version,
    });
    if let Some(session) = resume {
        msg["resume"] = Value::String(session.to_string());
    }
    msg
}

/// Verify a workspace node's `describe` versions against this client's.
///
/// `versions` is the `versions` map from a `describe` response (`protocol`
/// and `beamtalk` keys). This is the client side of the version lock: the
/// server gates new clients at `auth`, but an older node predating the gate
/// ignores the new `auth` fields, so the client must refuse the stale node
/// itself here. Exact match on both, mirroring the server's check.
///
/// # Errors
///
/// Returns a version-mismatch message (naming both sides plus the rebuild
/// hint) when `versions` is absent or either version differs.
pub fn verify_server_versions(
    versions: Option<&Value>,
    beamtalk_version: &str,
    protocol_version: &str,
) -> Result<(), String> {
    let versions = versions.ok_or_else(|| {
        format!(
            "Workspace did not report versions (describe has no versions map) — \
             the node predates the version handshake. Stop it and rebuild from \
             the same checkout: `just build`. Client is beamtalk {beamtalk_version}."
        )
    })?;
    let server_beamtalk = versions.get("beamtalk").and_then(Value::as_str);
    let server_protocol = versions.get("protocol").and_then(Value::as_str);
    match (server_beamtalk, server_protocol) {
        (Some(bt), Some(p)) if bt == beamtalk_version && p == protocol_version => Ok(()),
        _ => Err(format!(
            "Workspace version mismatch: node reports beamtalk {}, protocol {}; \
             this client is beamtalk {beamtalk_version}, protocol {protocol_version}. \
             Stop the stale node and rebuild from the same checkout: `just build`.",
            server_beamtalk.unwrap_or("<missing>"),
            server_protocol.unwrap_or("<missing>"),
        )),
    }
}

/// The server's response to an `auth` request.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthAck {
    /// `{"type": "auth_ok"}`.
    Ok,
    /// `{"type": "auth_error", "message": ...}`. `message` is `None` when the
    /// server omitted it — every real `handle_auth/2` branch sets one, but a
    /// caller should not panic on a hypothetical future server that doesn't.
    Error {
        /// The server's failure reason, when present.
        message: Option<String>,
    },
}

/// Classify `frame` as an auth response, or `None` if it is neither
/// `auth_ok` nor `auth_error` (an unexpected frame the caller should reject).
#[must_use]
pub fn parse_auth_ack(frame: &Value) -> Option<AuthAck> {
    match frame.get("type").and_then(Value::as_str) {
        Some(TYPE_AUTH_OK) => Some(AuthAck::Ok),
        Some(TYPE_AUTH_ERROR) => Some(AuthAck::Error {
            message: frame
                .get("message")
                .and_then(Value::as_str)
                .map(String::from),
        }),
        _ => None,
    }
}

/// True when `frame` is the post-auth `{"op": "session-started", ...}` frame.
#[must_use]
pub fn is_session_started(frame: &Value) -> bool {
    frame.get("op").and_then(Value::as_str) == Some(OP_SESSION_STARTED)
}

/// Extract the `session` id from a `session-started` frame, if present.
#[must_use]
pub fn session_id(frame: &Value) -> Option<String> {
    frame
        .get("session")
        .and_then(Value::as_str)
        .map(String::from)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_auth_required_true_for_matching_op() {
        assert!(is_auth_required(
            &serde_json::json!({"op": "auth-required"})
        ));
    }

    #[test]
    fn is_auth_required_false_for_other_op() {
        assert!(!is_auth_required(
            &serde_json::json!({"op": "session-started"})
        ));
        assert!(!is_auth_required(&serde_json::json!({"type": "auth_ok"})));
        assert!(!is_auth_required(&serde_json::json!({})));
    }

    #[test]
    fn auth_request_without_resume_omits_field() {
        let req = auth_request("cookie-1", "repl", None, "0.4.0", PROTOCOL_VERSION);
        assert_eq!(req["type"], "auth");
        assert_eq!(req["cookie"], "cookie-1");
        assert_eq!(req["client"], "repl");
        assert_eq!(req["beamtalk_version"], "0.4.0");
        assert_eq!(req["protocol_version"], PROTOCOL_VERSION);
        assert!(req.get("resume").is_none());
    }

    #[test]
    fn auth_request_with_resume_includes_field() {
        let req = auth_request("cookie-1", "mcp", Some("sess-42"), "0.4.0", "9.9");
        assert_eq!(req["type"], "auth");
        assert_eq!(req["cookie"], "cookie-1");
        assert_eq!(req["client"], "mcp");
        assert_eq!(req["resume"], "sess-42");
        assert_eq!(req["beamtalk_version"], "0.4.0");
        assert_eq!(req["protocol_version"], "9.9");
    }

    #[test]
    fn protocol_version_matches_erlang_macro() {
        // Documents the current wire value; the real cross-side pinning is
        // the shared wire corpus (Rust `matches_shared_wire_corpus` and the
        // Erlang `handshake_*_matches_shared_wire_corpus` tests) plus the
        // Erlang `describe` conformance test. Bump this constant together
        // with `?PROTOCOL_VERSION` in
        // `runtime/apps/beamtalk_runtime/include/beamtalk.hrl`.
        assert_eq!(PROTOCOL_VERSION, "2.0");
    }

    #[test]
    fn verify_server_versions_accepts_match() {
        let versions = serde_json::json!({"protocol": "2.0", "beamtalk": "0.4.0"});
        assert!(verify_server_versions(Some(&versions), "0.4.0", "2.0").is_ok());
    }

    #[test]
    fn verify_server_versions_rejects_beamtalk_mismatch() {
        let versions = serde_json::json!({"protocol": "2.0", "beamtalk": "0.3.0"});
        let err = verify_server_versions(Some(&versions), "0.4.0", "2.0").unwrap_err();
        assert!(err.contains("0.3.0"), "{err}");
        assert!(err.contains("just build"), "{err}");
    }

    #[test]
    fn verify_server_versions_rejects_protocol_mismatch() {
        let versions = serde_json::json!({"protocol": "1.0", "beamtalk": "0.4.0"});
        let err = verify_server_versions(Some(&versions), "0.4.0", "2.0").unwrap_err();
        assert!(err.contains("1.0"), "{err}");
    }

    #[test]
    fn verify_server_versions_rejects_absent_versions() {
        let err = verify_server_versions(None, "0.4.0", "2.0").unwrap_err();
        assert!(err.contains("predates"), "{err}");
    }

    #[test]
    fn parse_auth_ack_recognises_ok() {
        assert_eq!(
            parse_auth_ack(&serde_json::json!({"type": "auth_ok"})),
            Some(AuthAck::Ok)
        );
    }

    #[test]
    fn parse_auth_ack_recognises_error_with_message() {
        assert_eq!(
            parse_auth_ack(&serde_json::json!({"type": "auth_error", "message": "nope"})),
            Some(AuthAck::Error {
                message: Some("nope".to_string())
            })
        );
    }

    #[test]
    fn parse_auth_ack_recognises_error_without_message() {
        assert_eq!(
            parse_auth_ack(&serde_json::json!({"type": "auth_error"})),
            Some(AuthAck::Error { message: None })
        );
    }

    #[test]
    fn parse_auth_ack_rejects_unrelated_frame() {
        assert_eq!(
            parse_auth_ack(&serde_json::json!({"type": "eval-result"})),
            None
        );
        assert_eq!(parse_auth_ack(&serde_json::json!({})), None);
    }

    #[test]
    fn is_session_started_true_for_matching_op() {
        assert!(is_session_started(
            &serde_json::json!({"op": "session-started", "session": "abc"})
        ));
    }

    #[test]
    fn is_session_started_false_for_other_op() {
        assert!(!is_session_started(
            &serde_json::json!({"op": "auth-required"})
        ));
    }

    #[test]
    fn session_id_extracts_value() {
        assert_eq!(
            session_id(&serde_json::json!({"op": "session-started", "session": "abc"})),
            Some("abc".to_string())
        );
    }

    #[test]
    fn session_id_absent_returns_none() {
        assert_eq!(
            session_id(&serde_json::json!({"op": "session-started"})),
            None
        );
    }

    /// A wire-corpus case: one example frame, plus which of its fields carry
    /// a value that legitimately varies at runtime (a cookie, a session id) —
    /// see `dynamic_fields` in the fixture.
    struct Case {
        id: String,
        frame: Value,
        dynamic_fields: Vec<String>,
    }

    fn load_corpus() -> Vec<Case> {
        let path = beamtalk_workspace::cargo_workspace_root!().join(
            "runtime/apps/beamtalk_workspace/test/fixtures/ws_auth_handshake_wire_corpus.json",
        );
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
        let cases: Vec<serde_json::Value> =
            serde_json::from_str(&raw).expect("corpus is a JSON array");
        assert!(!cases.is_empty(), "corpus must have cases");
        cases
            .into_iter()
            .map(|case| Case {
                id: case["id"]
                    .as_str()
                    .expect("case.id is a string")
                    .to_string(),
                frame: case["frame"].clone(),
                dynamic_fields: case["dynamic_fields"]
                    .as_array()
                    .expect("case.dynamic_fields is an array")
                    .iter()
                    .map(|v| {
                        v.as_str()
                            .expect("dynamic field name is a string")
                            .to_string()
                    })
                    .collect(),
            })
            .collect()
    }

    fn find<'a>(cases: &'a [Case], id: &str) -> &'a Case {
        cases
            .iter()
            .find(|c| c.id == id)
            .unwrap_or_else(|| panic!("corpus is missing case {id:?}"))
    }

    /// Check one `auth_request`-shaped corpus case: rebuild the frame from
    /// its own field values via [`auth_request`] and require an exact match,
    /// plus presence of every dynamic field. `resume` is passed through when
    /// the case carries one.
    fn check_auth_request_case(cases: &[Case], id: &str) {
        let case = find(cases, id);
        let field = |key: &str| {
            case.frame[key]
                .as_str()
                .unwrap_or_else(|| panic!("{id}.frame.{key} is a string"))
        };
        let resume = case.frame.get("resume").and_then(Value::as_str);
        assert_eq!(
            auth_request(
                field("cookie"),
                field("client"),
                resume,
                field("beamtalk_version"),
                field("protocol_version"),
            ),
            case.frame,
            "corpus mismatch for {id:?}",
        );
        for dynamic in ["cookie", "client", "beamtalk_version", "protocol_version"] {
            assert!(
                case.dynamic_fields.contains(&dynamic.to_string()),
                "{id:?} must list {dynamic:?} as dynamic"
            );
        }
    }

    /// Conformance: this module's frame recognition/construction must
    /// agree with the shared corpus the Erlang side (`beamtalk_ws_handler`'s
    /// production code) is pinned to as well — see the module doc comment.
    #[test]
    fn matches_shared_wire_corpus() {
        let corpus = load_corpus();

        let auth_required = find(&corpus, "auth_required");
        assert!(
            is_auth_required(&auth_required.frame),
            "corpus mismatch for {:?}",
            auth_required.id
        );
        assert!(!is_auth_required(&serde_json::json!({"op": "not-it"})));

        check_auth_request_case(&corpus, "auth_request");
        check_auth_request_case(&corpus, "auth_request_with_resume");

        let auth_ok = find(&corpus, "auth_ok");
        assert_eq!(
            parse_auth_ack(&auth_ok.frame),
            Some(AuthAck::Ok),
            "corpus mismatch for {:?}",
            auth_ok.id
        );

        let auth_error = find(&corpus, "auth_error");
        let expected_message = auth_error.frame["message"]
            .as_str()
            .expect("auth_error.frame.message is a string")
            .to_string();
        assert_eq!(
            parse_auth_ack(&auth_error.frame),
            Some(AuthAck::Error {
                message: Some(expected_message)
            }),
            "corpus mismatch for {:?}",
            auth_error.id
        );

        let session_started = find(&corpus, "session_started");
        assert!(
            is_session_started(&session_started.frame),
            "corpus mismatch for {:?}",
            session_started.id
        );
        assert_eq!(
            session_id(&session_started.frame),
            session_started.frame["session"].as_str().map(String::from),
            "corpus mismatch for {:?}",
            session_started.id
        );
        assert!(
            session_started
                .dynamic_fields
                .contains(&"session".to_string())
        );
    }
}
