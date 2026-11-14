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
//! caller, since the callers use different transports (async
//! `tokio-tungstenite` vs. sync `tungstenite`) and different retry/timeout
//! policies. The step order itself is shared as the transport-free
//! [`AuthHandshake`] state machine; [`perform_auth_sequence`] drives it over
//! any async transport (BT-4682).

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
/// likewise rejects a `describe` response whose `protocol` differs. The
/// header is canonical: `protocol_version_matches_erlang_macro` reads it and
/// fails if this constant drifts (as does the VS Code extension's own test
/// for its `WORKSPACE_PROTOCOL_VERSION`).
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

/// The human-facing build identity (`BEAMTALK_BUILD_ID`, BT-4812) — e.g.
/// `0.4.0` for a tagged release, `0.4.0-edge+2ce49a8 (2026-11-14)` for an edge
/// build, `0.4.0-dev+2ce49a8.dirty (2026-11-14)` for a local checkout.
///
/// Display only: it starts with the same `VERSION` as [`beamtalk_version`] but
/// adds the channel, `dirty` marker and commit date, so it must never be
/// compared across the wire. The handshake keeps matching on
/// [`beamtalk_version`], whose `-dev+<sha>` suffix already names the commit.
#[must_use]
pub fn build_id() -> &'static str {
    env!("BEAMTALK_BUILD_ID")
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
/// Returns a version-mismatch message (naming both sides, how to stop the
/// stale node, and how to get matching toolchains — see
/// [`version_mismatch_remedy`]) when `versions` is absent or either version
/// differs.
pub fn verify_server_versions(
    versions: Option<&Value>,
    beamtalk_version: &str,
    protocol_version: &str,
) -> Result<(), String> {
    let versions = versions.ok_or_else(|| {
        format!(
            "Workspace did not report versions (describe has no versions map) — \
             the node predates the version handshake. Client is beamtalk \
             {beamtalk_version}. {}",
            version_mismatch_remedy(beamtalk_version, None)
        )
    })?;
    let server_beamtalk = versions.get("beamtalk").and_then(Value::as_str);
    let server_protocol = versions.get("protocol").and_then(Value::as_str);
    match (server_beamtalk, server_protocol) {
        (Some(bt), Some(p)) if bt == beamtalk_version && p == protocol_version => Ok(()),
        _ => Err(format!(
            "Workspace version mismatch: node reports beamtalk {}, protocol {}; \
             this client is beamtalk {beamtalk_version}, protocol {protocol_version}. {}",
            server_beamtalk.unwrap_or("<missing>"),
            server_protocol.unwrap_or("<missing>"),
            version_mismatch_remedy(beamtalk_version, server_beamtalk),
        )),
    }
}

/// The "what to do" half of a client-side version-mismatch message.
///
/// Always names `beamtalk workspace stop <id>` (the client does not learn
/// the node's workspace id from `describe`; `beamtalk workspace list` shows
/// it). Suggests `just build` only when either side is a `-dev` build — a
/// source checkout, where rebuilding is the fix; between released
/// toolchains the fix is installing the same release on both sides.
fn version_mismatch_remedy(client_beamtalk: &str, server_beamtalk: Option<&str>) -> String {
    let rebuild = if is_dev_version(client_beamtalk) || server_beamtalk.is_some_and(is_dev_version)
    {
        "rebuild from the same checkout (`just build`)"
    } else {
        "install the same beamtalk release for both"
    };
    format!(
        "Stop the stale node with `beamtalk workspace stop <id>` \
         (`beamtalk workspace list` shows the id), then {rebuild}."
    )
}

/// True for a source-checkout build's version (`0.4.0-dev+<sha>`), as
/// opposed to a release (`0.4.0`).
fn is_dev_version(version: &str) -> bool {
    version.contains("-dev")
}

/// Client side of the version lock over any async transport (BT-4327,
/// BT-4382): send a `describe` request, wait for the reply echoing its id,
/// and check its versions with [`verify_server_versions`] against this
/// toolchain's [`beamtalk_version`] and [`PROTOCOL_VERSION`].
///
/// A node predating the server gate ignores the `auth` version fields, so
/// without this check a new client would silently compile against a stale
/// toolchain. The node queues actor-snapshot push frames ahead of the reply,
/// so frames with any other id are skipped. The whole wait is bounded by
/// `timeout`: a node emitting a continuous push stream would otherwise delay
/// connect indefinitely.
///
/// The I/O stays with the caller (this crate has no async runtime in its
/// production build): `send` writes one text frame to `io`, and `recv` reads
/// the next text frame, giving up after the `Duration` it is passed (the
/// time left before the deadline).
///
/// # Errors
///
/// Returns the first transport error from `send`/`recv`, a parse error for
/// an undecodable frame, a timeout message, or the version-mismatch message
/// from [`verify_server_versions`].
pub async fn verify_server_versions_via_describe<T>(
    io: &mut T,
    timeout: std::time::Duration,
    send: impl AsyncFnOnce(&mut T, String) -> Result<(), String>,
    mut recv: impl AsyncFnMut(&mut T, std::time::Duration) -> Result<String, String>,
) -> Result<(), String> {
    let describe = crate::RequestBuilder::describe();
    let want_id = describe
        .get("id")
        .and_then(Value::as_str)
        .map(str::to_string);
    let describe_str = serde_json::to_string(&describe)
        .map_err(|e| format!("failed to serialise describe: {e}"))?;
    send(io, describe_str)
        .await
        .map_err(|e| format!("failed to send describe: {e}"))?;
    let deadline = std::time::Instant::now() + timeout;
    loop {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return Err("timed out waiting for the describe version check".to_string());
        }
        let response = recv(io, remaining).await?;
        let parsed: crate::ReplResponse = serde_json::from_str(&response)
            .map_err(|e| format!("failed to decode describe response: {e}"))?;
        if want_id.is_some() && parsed.id == want_id {
            return verify_server_versions(
                parsed.versions.as_ref(),
                beamtalk_version(),
                PROTOCOL_VERSION,
            );
        }
        // Anything else (actor-snapshot pushes queued ahead of our reply)
        // is not our answer — keep waiting.
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

/// What the caller does after feeding a frame to an [`AuthHandshake`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthStep {
    /// Write this text frame to the server, then read the next frame.
    Send(String),
    /// Nothing to send; read the next frame.
    Continue,
    /// The handshake is complete. Carries the `session-started` session id.
    Done(Option<String>),
}

#[derive(Debug, Clone, Copy)]
enum AuthPhase {
    AuthRequired,
    AuthAck,
    SessionStarted,
}

/// The client side of the auth sequence as a transport-free state machine
/// (BT-4682): feed it each frame the server sends, do what the returned
/// [`AuthStep`] says. Sync (`tungstenite`) and async (`tokio-tungstenite`)
/// clients drive the same machine, so the step order and error wording live
/// in one place; [`perform_auth_sequence`] is the async closure-style driver.
#[derive(Debug)]
pub struct AuthHandshake {
    phase: AuthPhase,
    cookie: String,
    client: String,
    resume: Option<String>,
}

impl AuthHandshake {
    /// A handshake that authenticates with `cookie`, tagging the session
    /// surface as `client` (`"lsp"`, `"mcp"`, `"repl"`, ...) and optionally
    /// resuming session `resume`.
    #[must_use]
    pub fn new(cookie: &str, client: &str, resume: Option<&str>) -> Self {
        Self {
            phase: AuthPhase::AuthRequired,
            cookie: cookie.to_string(),
            client: client.to_string(),
            resume: resume.map(String::from),
        }
    }

    /// Feed the next text frame from the server.
    ///
    /// # Errors
    /// An undecodable or unexpected frame, or an `auth_error` from the server.
    pub fn on_frame(&mut self, text: &str) -> Result<AuthStep, String> {
        let what = match self.phase {
            AuthPhase::AuthRequired => "auth-required",
            AuthPhase::AuthAck => "auth response",
            AuthPhase::SessionStarted => "session-started",
        };
        let frame: Value =
            serde_json::from_str(text).map_err(|e| format!("failed to parse {what}: {e}"))?;
        self.on_value(&frame)
    }

    /// As [`Self::on_frame`], for a caller that has already decoded the JSON.
    ///
    /// # Errors
    /// An unexpected frame, or an `auth_error` from the server.
    pub fn on_value(&mut self, frame: &Value) -> Result<AuthStep, String> {
        match self.phase {
            AuthPhase::AuthRequired => {
                if !is_auth_required(frame) {
                    return Err(format!("unexpected pre-auth message: {frame}"));
                }
                // The version fields let the node reject a mismatched
                // toolchain (BT-4327) before any session exists.
                let auth = auth_request(
                    &self.cookie,
                    &self.client,
                    self.resume.as_deref(),
                    beamtalk_version(),
                    PROTOCOL_VERSION,
                );
                let text = serde_json::to_string(&auth)
                    .map_err(|e| format!("failed to serialise auth: {e}"))?;
                self.phase = AuthPhase::AuthAck;
                Ok(AuthStep::Send(text))
            }
            AuthPhase::AuthAck => match parse_auth_ack(frame) {
                Some(AuthAck::Ok) => {
                    self.phase = AuthPhase::SessionStarted;
                    Ok(AuthStep::Continue)
                }
                Some(AuthAck::Error { message }) => {
                    let msg = message.as_deref().unwrap_or("authentication failed");
                    Err(format!("workspace authentication failed: {msg}"))
                }
                None => Err(format!("unexpected auth response: {frame}")),
            },
            AuthPhase::SessionStarted => {
                if !is_session_started(frame) {
                    return Err(format!("unexpected post-auth message: {frame}"));
                }
                Ok(AuthStep::Done(session_id(frame)))
            }
        }
    }
}

/// Run the whole auth sequence over any transport, closure style (as
/// [`verify_server_versions_via_describe`]): `send` writes one text frame,
/// `recv` reads the next one. Returns the `session-started` session id.
///
/// # Errors
/// The first transport error from `send`/`recv`, or any [`AuthHandshake`] error.
pub async fn perform_auth_sequence<T>(
    io: &mut T,
    cookie: &str,
    client: &str,
    resume: Option<&str>,
    mut send: impl AsyncFnMut(&mut T, String) -> Result<(), String>,
    mut recv: impl AsyncFnMut(&mut T) -> Result<String, String>,
) -> Result<Option<String>, String> {
    let mut handshake = AuthHandshake::new(cookie, client, resume);
    loop {
        let frame = recv(io).await?;
        match handshake.on_frame(&frame)? {
            AuthStep::Send(text) => send(io, text)
                .await
                .map_err(|e| format!("failed to send auth: {e}"))?,
            AuthStep::Continue => {}
            AuthStep::Done(session) => return Ok(session),
        }
    }
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
        // `?PROTOCOL_VERSION` in beamtalk.hrl is the canonical value; read it
        // from the header so bumping only one side fails here. The VS Code
        // extension's `WORKSPACE_PROTOCOL_VERSION` is pinned to the same
        // header by `editors/vscode/src/__tests__/workspaceClient.test.ts`.
        let hrl_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../runtime/apps/beamtalk_runtime/include/beamtalk.hrl");
        let hrl = std::fs::read_to_string(&hrl_path)
            .unwrap_or_else(|e| panic!("read {}: {e}", hrl_path.display()));
        let erlang_value = hrl
            .lines()
            .find_map(|line| {
                line.trim()
                    .strip_prefix("-define(PROTOCOL_VERSION, <<\"")?
                    .strip_suffix("\">>).")
            })
            .expect("beamtalk.hrl defines ?PROTOCOL_VERSION as a binary literal");
        assert_eq!(PROTOCOL_VERSION, erlang_value);
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
        assert!(err.contains("beamtalk workspace stop <id>"), "{err}");
    }

    /// In-memory transport for [`verify_server_versions_via_describe`]: the
    /// frames the "node" will answer with, and the frames the client sent.
    struct ScriptedIo {
        replies: std::collections::VecDeque<String>,
        sent: Vec<Value>,
    }

    /// Run the shared describe check against `reply_for(describe_id)`'s
    /// frames, in order.
    fn run_describe_check(reply_for: impl Fn(&str) -> Vec<Value>) -> Result<(), String> {
        let mut io = ScriptedIo {
            replies: std::collections::VecDeque::new(),
            sent: Vec::new(),
        };
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_time()
            .build()
            .expect("runtime");
        runtime.block_on(verify_server_versions_via_describe(
            &mut io,
            std::time::Duration::from_secs(5),
            async |io: &mut ScriptedIo, text: String| {
                let sent: Value = serde_json::from_str(&text).map_err(|e| e.to_string())?;
                let id = sent["id"].as_str().unwrap_or_default().to_string();
                io.replies
                    .extend(reply_for(&id).iter().map(Value::to_string));
                io.sent.push(sent);
                Ok(())
            },
            async |io: &mut ScriptedIo, _remaining| {
                io.replies
                    .pop_front()
                    .ok_or_else(|| "stream ended".to_string())
            },
        ))
    }

    /// BT-4382: the one shared describe loop (MCP and LSP) skips push frames
    /// queued ahead of the reply and checks the reply echoing its own id.
    #[test]
    fn verify_server_versions_via_describe_skips_pushes_and_checks_the_reply() {
        let matching = serde_json::json!({
            "beamtalk": beamtalk_version(),
            "protocol": PROTOCOL_VERSION,
        });
        let result = run_describe_check(|id| {
            vec![
                serde_json::json!({"push": "actors", "id": "someone-else"}),
                serde_json::json!({"id": id, "status": ["done"], "versions": matching}),
            ]
        });
        assert_eq!(result, Ok(()));

        let result = run_describe_check(|id| {
            vec![serde_json::json!({
                "id": id,
                "status": ["done"],
                "versions": crate::test_versions::stale_versions(),
            })]
        });
        let err = result.unwrap_err();
        assert!(err.contains("0.0.0-stale"), "{err}");

        // No reply ever arrives: the transport's own error surfaces.
        let err = run_describe_check(|_| Vec::new()).unwrap_err();
        assert_eq!(err, "stream ended");
    }

    /// BT-4382: `just build` is a source-checkout remedy — suggested only
    /// when either side is a `-dev` build, never between two releases.
    #[test]
    fn verify_server_versions_suggests_just_build_only_for_dev_versions() {
        let release = serde_json::json!({"protocol": "2.0", "beamtalk": "0.3.0"});
        let err = verify_server_versions(Some(&release), "0.4.0", "2.0").unwrap_err();
        assert!(!err.contains("just build"), "{err}");
        assert!(err.contains("same beamtalk release"), "{err}");

        let err = verify_server_versions(Some(&release), "0.4.0-dev+abc1234", "2.0").unwrap_err();
        assert!(err.contains("just build"), "{err}");

        let dev_node = serde_json::json!({"protocol": "2.0", "beamtalk": "0.4.0-dev+abc1234"});
        let err = verify_server_versions(Some(&dev_node), "0.4.0", "2.0").unwrap_err();
        assert!(err.contains("just build"), "{err}");

        let err = verify_server_versions(None, "0.4.0", "2.0").unwrap_err();
        assert!(!err.contains("just build"), "{err}");
        let err = verify_server_versions(None, "0.4.0-dev+abc1234", "2.0").unwrap_err();
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

    /// Drive an [`AuthHandshake`] with `frames`, returning what it asked to send.
    fn drive(frames: &[Value]) -> (Vec<Value>, Result<Option<String>, String>) {
        let mut handshake = AuthHandshake::new("cookie-1", "test", Some("sess-9"));
        let mut sent = Vec::new();
        for frame in frames {
            match handshake.on_value(frame) {
                Ok(AuthStep::Send(text)) => sent.push(serde_json::from_str(&text).unwrap()),
                Ok(AuthStep::Continue) => {}
                Ok(AuthStep::Done(session)) => return (sent, Ok(session)),
                Err(e) => return (sent, Err(e)),
            }
        }
        (sent, Err("ran out of frames".to_string()))
    }

    #[test]
    fn auth_handshake_runs_the_four_message_sequence() {
        let (sent, result) = drive(&[
            serde_json::json!({"op": OP_AUTH_REQUIRED}),
            serde_json::json!({"type": TYPE_AUTH_OK}),
            serde_json::json!({"op": OP_SESSION_STARTED, "session": "s-1"}),
        ]);
        assert_eq!(result, Ok(Some("s-1".to_string())));
        assert_eq!(
            sent.len(),
            1,
            "exactly one frame (the auth request) is sent"
        );
        assert_eq!(sent[0]["type"], TYPE_AUTH);
        assert_eq!(sent[0]["cookie"], "cookie-1");
        assert_eq!(sent[0]["client"], "test");
        assert_eq!(sent[0]["resume"], "sess-9");
    }

    #[test]
    fn auth_handshake_rejects_each_unexpected_frame() {
        let (_, result) = drive(&[serde_json::json!({"op": "nope"})]);
        assert!(
            result
                .unwrap_err()
                .starts_with("unexpected pre-auth message")
        );

        let (_, result) = drive(&[
            serde_json::json!({"op": OP_AUTH_REQUIRED}),
            serde_json::json!({"type": "weird"}),
        ]);
        assert!(result.unwrap_err().starts_with("unexpected auth response"));

        let (_, result) = drive(&[
            serde_json::json!({"op": OP_AUTH_REQUIRED}),
            serde_json::json!({"type": TYPE_AUTH_OK}),
            serde_json::json!({"op": "nope"}),
        ]);
        assert!(
            result
                .unwrap_err()
                .starts_with("unexpected post-auth message")
        );
    }

    #[test]
    fn auth_handshake_reports_the_servers_auth_error() {
        let (_, result) = drive(&[
            serde_json::json!({"op": OP_AUTH_REQUIRED}),
            serde_json::json!({"type": TYPE_AUTH_ERROR, "message": "invalid cookie"}),
        ]);
        assert_eq!(
            result.unwrap_err(),
            "workspace authentication failed: invalid cookie"
        );
        let (_, result) = drive(&[
            serde_json::json!({"op": OP_AUTH_REQUIRED}),
            serde_json::json!({"type": TYPE_AUTH_ERROR}),
        ]);
        assert_eq!(
            result.unwrap_err(),
            "workspace authentication failed: authentication failed"
        );
    }

    #[test]
    fn auth_handshake_reports_an_undecodable_frame() {
        let mut handshake = AuthHandshake::new("c", "test", None);
        let err = handshake.on_frame("not json").unwrap_err();
        assert!(err.starts_with("failed to parse auth-required"), "{err}");
    }

    #[test]
    fn perform_auth_sequence_drives_the_machine_over_closures() {
        struct Io {
            replies: std::collections::VecDeque<String>,
            sent: Vec<String>,
        }
        let mut io = Io {
            replies: [
                serde_json::json!({"op": OP_AUTH_REQUIRED}),
                serde_json::json!({"type": TYPE_AUTH_OK}),
                serde_json::json!({"op": OP_SESSION_STARTED, "session": "s-2"}),
            ]
            .iter()
            .map(Value::to_string)
            .collect(),
            sent: Vec::new(),
        };
        let runtime = tokio::runtime::Builder::new_current_thread()
            .build()
            .expect("runtime");
        let session = runtime.block_on(perform_auth_sequence(
            &mut io,
            "c",
            "test",
            None,
            async |io: &mut Io, text: String| {
                io.sent.push(text);
                Ok(())
            },
            async |io: &mut Io| io.replies.pop_front().ok_or_else(|| "ended".to_string()),
        ));
        assert_eq!(session, Ok(Some("s-2".to_string())));
        assert_eq!(io.sent.len(), 1);
    }
}
