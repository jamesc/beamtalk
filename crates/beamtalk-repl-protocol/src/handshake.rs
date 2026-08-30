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
//! Before BT-3330 each of the four callers hand-transcribed this literal JSON
//! shape independently, with nothing pinning any of them to the Erlang
//! server's actual frame shapes — a rename on the Erlang side (e.g.
//! `auth_ok` → `authOk`) would have broken every client at runtime with no
//! test failing first. This module is now the single Rust-side source of
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
#[must_use]
pub fn auth_request(cookie: &str, client_surface: &str, resume: Option<&str>) -> Value {
    let mut msg = serde_json::json!({
        "type": TYPE_AUTH,
        "cookie": cookie,
        "client": client_surface,
    });
    if let Some(session) = resume {
        msg["resume"] = Value::String(session.to_string());
    }
    msg
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
        let req = auth_request("cookie-1", "repl", None);
        assert_eq!(req["type"], "auth");
        assert_eq!(req["cookie"], "cookie-1");
        assert_eq!(req["client"], "repl");
        assert!(req.get("resume").is_none());
    }

    #[test]
    fn auth_request_with_resume_includes_field() {
        let req = auth_request("cookie-1", "mcp", Some("sess-42"));
        assert_eq!(req["type"], "auth");
        assert_eq!(req["cookie"], "cookie-1");
        assert_eq!(req["client"], "mcp");
        assert_eq!(req["resume"], "sess-42");
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
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("crates/")
            .parent()
            .expect("repo root")
            .join(
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

    /// BT-3330 conformance: this module's frame recognition/construction must
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

        let auth_request_case = find(&corpus, "auth_request");
        let cookie = auth_request_case.frame["cookie"]
            .as_str()
            .expect("auth_request.frame.cookie is a string");
        let client_surface = auth_request_case.frame["client"]
            .as_str()
            .expect("auth_request.frame.client is a string");
        assert_eq!(
            auth_request(cookie, client_surface, None),
            auth_request_case.frame,
            "corpus mismatch for {:?}",
            auth_request_case.id
        );
        assert!(
            auth_request_case
                .dynamic_fields
                .contains(&"cookie".to_string())
        );
        assert!(
            auth_request_case
                .dynamic_fields
                .contains(&"client".to_string())
        );

        let auth_request_resume_case = find(&corpus, "auth_request_with_resume");
        let resume = auth_request_resume_case.frame["resume"]
            .as_str()
            .expect("auth_request_with_resume.frame.resume is a string");
        let resume_cookie = auth_request_resume_case.frame["cookie"]
            .as_str()
            .expect("auth_request_with_resume.frame.cookie is a string");
        let resume_client = auth_request_resume_case.frame["client"]
            .as_str()
            .expect("auth_request_with_resume.frame.client is a string");
        assert_eq!(
            auth_request(resume_cookie, resume_client, Some(resume)),
            auth_request_resume_case.frame,
            "corpus mismatch for {:?}",
            auth_request_resume_case.id
        );

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
