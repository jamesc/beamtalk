// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared JSON-over-WebSocket protocol client for workspace communication.
//!
//! **DDD Context:** REPL — Protocol Transport
//!
//! Both the REPL and transcript viewer communicate with the workspace
//! BEAM node using JSON messages over WebSocket (ADR 0020). This module
//! provides the shared transport layer with cookie-based authentication.

use std::net::TcpStream;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};
use tungstenite::{Error as WsError, Message, WebSocket};

/// Default connection timeout in milliseconds.
const CONNECT_TIMEOUT_MS: u64 = 5000;

/// Counter for generating unique message IDs across all protocol clients.
static MSG_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Generate a unique message ID.
pub fn next_msg_id() -> String {
    let n = MSG_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("msg-{n:03}")
}

/// Check if a tungstenite error is a transport-level failure worth retrying.
/// Protocol, UTF-8, and capacity errors indicate a message-level problem
/// that reconnecting won't fix.
fn is_transport_error(e: &WsError) -> bool {
    matches!(
        e,
        WsError::ConnectionClosed
            | WsError::AlreadyClosed
            | WsError::Io(_)
            | WsError::WriteBufferFull(_)
    )
}

/// Low-level JSON-over-WebSocket protocol client (ADR 0020).
///
/// Handles connection, cookie authentication, and JSON message framing
/// over WebSocket. Higher-level clients wrap this to add typed response
/// parsing and domain-specific methods.
pub struct ProtocolClient {
    /// The underlying WebSocket connection.
    ws: WebSocket<TcpStream>,
    /// Session ID from the server welcome message (BT-666).
    session_id: Option<String>,
    /// Connection parameters for reconnects.
    host: String,
    port: u16,
    cookie: String,
    /// Read timeout to reapply on reconnect.
    read_timeout: Option<Duration>,
    /// Whether the transcript output cursor is at the beginning of a line.
    /// Used to insert the `│ ` gutter prefix only at line starts.
    transcript_bol: bool,
    /// When `false`, transcript push messages are silently discarded instead of
    /// being printed. Set to `false` for completion-only connections that must
    /// not re-print transcript output already shown on the main connection.
    print_transcript: bool,
}

impl ProtocolClient {
    /// Connect to a workspace backend at `ws://{host}:{port}/ws` and
    /// authenticate with the given cookie.
    ///
    /// `read_timeout` sets the TCP read timeout (None for blocking reads).
    pub fn connect(
        host: &str,
        port: u16,
        cookie: &str,
        read_timeout: Option<Duration>,
    ) -> Result<Self> {
        // Delegate to connect_with_resume with no resume requested.
        Self::connect_with_resume(host, port, cookie, read_timeout, None)
    }

    /// Connect with optional session resume. If `resume` is `Some(session_id)`,
    /// the auth handshake will request session resumption on the server side.
    pub fn connect_with_resume(
        host: &str,
        port: u16,
        cookie: &str,
        read_timeout: Option<Duration>,
        resume: Option<&str>,
    ) -> Result<Self> {
        let addr = format!("{host}:{port}");
        let tcp_stream = TcpStream::connect_timeout(
            &addr.parse().into_diagnostic()?,
            Duration::from_millis(CONNECT_TIMEOUT_MS),
        )
        .map_err(|e| miette!("Failed to connect to workspace at {addr}: {e}"))?;

        if let Some(timeout) = read_timeout {
            tcp_stream
                .set_read_timeout(Some(timeout))
                .into_diagnostic()?;
        }

        let url = format!("ws://{addr}/ws");
        let (ws, _response) = tungstenite::client(url, tcp_stream)
            .map_err(|e| miette!("WebSocket handshake failed: {e}"))?;

        let mut client = Self {
            ws,
            session_id: None,
            host: host.to_string(),
            port,
            cookie: cookie.to_string(),
            read_timeout,
            transcript_bol: true,
            print_transcript: true,
        };

        client.perform_auth_handshake(resume)?;
        Ok(client)
    }

    /// Exchange the WebSocket auth handshake with the server.
    ///
    /// Performs the four-message sequence required before the connection is
    /// ready for use:
    ///
    /// 1. Receive `auth-required` from the server.
    /// 2. Send `auth` with the cookie and an optional session-resume token.
    /// 3. Receive `auth_ok` (or `auth_error` on bad credentials).
    /// 4. Receive `session-started`; populate `self.session_id`.
    ///
    /// Mirrors the pattern established in the MCP client
    /// (`beamtalk-mcp/src/client.rs: perform_auth_handshake`).
    fn perform_auth_handshake(&mut self, resume: Option<&str>) -> Result<()> {
        // Read auth-required message (pre-auth, no session yet)
        let auth_required = self.read_response()?;
        match auth_required.get("op").and_then(|v| v.as_str()) {
            Some("auth-required") => {}
            _ => return Err(miette!("Unexpected pre-auth message: {auth_required}")),
        }

        // Build auth message with optional resume field. `client` records the
        // originating surface so `Workspace sessions` can show where a session
        // came from; every CLI command (repl, transcript, completion probe)
        // connects through this client, so they all report `repl`.
        let mut auth_msg =
            serde_json::json!({"type": "auth", "cookie": self.cookie.as_str(), "client": "repl"});
        if let Some(res) = resume {
            auth_msg["resume"] = serde_json::Value::String(res.to_string());
        }
        self.send_only(&auth_msg)?;

        // Read auth response
        let auth_response = self.read_response()?;
        match auth_response.get("type").and_then(|t| t.as_str()) {
            Some("auth_ok") => {}
            Some("auth_error") => {
                let msg = auth_response
                    .get("message")
                    .and_then(|m| m.as_str())
                    .unwrap_or("Authentication failed");
                return Err(miette!("Workspace authentication failed: {msg}"));
            }
            _ => {
                return Err(miette!("Unexpected auth response: {}", auth_response));
            }
        }

        // Read session-started message (sent after auth_ok)
        let session_msg = self.read_response()?;
        match session_msg.get("op").and_then(|v| v.as_str()) {
            Some("session-started") => {}
            _ => return Err(miette!("Unexpected session message: {session_msg}")),
        }
        self.session_id = session_msg
            .get("session")
            .and_then(|s| s.as_str())
            .map(String::from);

        Ok(())
    }

    /// Attempt to reconnect the underlying WebSocket and resume the session
    /// using the last-known session id if present.
    ///
    /// Returns `true` if the previous session was successfully resumed,
    /// `false` if a fresh session was established instead.
    pub fn reconnect(&mut self) -> Result<bool> {
        let requested_session = self.session_id.clone();
        tracing::debug!(
            host = %self.host,
            port = self.port,
            has_session = requested_session.is_some(),
            "Attempting WebSocket reconnect"
        );
        let new_client = match Self::connect_with_resume(
            &self.host,
            self.port,
            &self.cookie,
            self.read_timeout,
            requested_session.as_deref(),
        ) {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!(error = %e, "Reconnect failed");
                return Err(e);
            }
        };
        // Swap in websocket and session id.
        // Reset transcript_bol: the display context is fresh regardless of
        // whether the session was resumed, so the next chunk always gets a prefix.
        // Preserve print_transcript: the caller's intent doesn't change on reconnect.
        self.ws = new_client.ws;
        self.session_id.clone_from(&new_client.session_id);
        self.transcript_bol = true;
        let resumed = requested_session.is_some()
            && requested_session.as_deref() == new_client.session_id.as_deref();
        tracing::debug!(
            session_id = ?self.session_id,
            resumed,
            "Reconnect successful"
        );
        Ok(resumed)
    }

    /// Get the session ID assigned by the server during connection.
    pub fn session_id(&self) -> Option<&str> {
        self.session_id.as_deref()
    }

    /// Set the read timeout on the underlying TCP stream.
    pub fn set_read_timeout(&self, timeout: Option<Duration>) -> Result<()> {
        self.ws
            .get_ref()
            .set_read_timeout(timeout)
            .into_diagnostic()
    }

    /// Control whether transcript push messages are printed to stdout.
    ///
    /// Set to `false` for connections that must not re-display transcript output
    /// already shown on the main REPL connection (e.g. the completion client).
    pub fn set_print_transcript(&mut self, val: bool) {
        self.print_transcript = val;
    }

    /// Handle a server-initiated push message (ADR 0017).
    ///
    /// Transcript push messages are printed inline to stdout with a `│ `
    /// gutter prefix at the start of each line so they are visually distinct
    /// from eval results. `reload_check`/`completed` pushes (ADR 0105 Phase
    /// 1, BT-2779) render the reload-induced re-check notice the same way
    /// the ADR's demo shows it — asynchronous, interleaved with whatever the
    /// REPL is doing, since the re-check that produced it runs on the
    /// install path of a *different* session's save as easily as this one's.
    /// Other push types (actor lifecycle, etc.) are silently ignored.
    ///
    /// Returns `true` if the message was a push and should be skipped for
    /// response parsing.
    fn handle_push(&mut self, parsed: &serde_json::Value) -> bool {
        let is_push = parsed.get("push").is_some()
            || parsed.get("type").and_then(|v| v.as_str()) == Some("push");
        if !is_push {
            return false;
        }
        if self.print_transcript
            && parsed.get("push").and_then(|v| v.as_str()) == Some("transcript")
        {
            if let Some(text) = parsed.get("text").and_then(|v| v.as_str()) {
                use std::io::Write;
                let formatted = format_transcript_chunk(text, &mut self.transcript_bol);
                let mut out = std::io::stdout().lock();
                if let Err(err) = out
                    .write_all(formatted.as_bytes())
                    .and_then(|()| out.flush())
                {
                    eprintln!("warning: failed to write transcript output: {err}");
                }
            }
        }
        if parsed.get("channel").and_then(|v| v.as_str()) == Some("reload_check")
            && parsed.get("event").and_then(|v| v.as_str()) == Some("completed")
        {
            if let Some(data) = parsed.get("data") {
                if let Some(notice) = format_reload_check_notice(data) {
                    use std::io::Write;
                    let mut out = std::io::stdout().lock();
                    if let Err(err) = writeln!(out, "{notice}").and_then(|()| out.flush()) {
                        eprintln!("warning: failed to write reload check notice: {err}");
                    }
                    // A fresh notice always starts at its own line start; the
                    // next transcript chunk (if any) should get the gutter
                    // prefix rather than gluing onto this notice's last line.
                    self.transcript_bol = true;
                }
            }
        }
        true
    }

    /// Read a single JSON response from the WebSocket, printing any transcript
    /// push messages inline and skipping other push messages.
    fn read_response(&mut self) -> Result<serde_json::Value> {
        loop {
            let msg = self
                .ws
                .read()
                .map_err(|e| miette!("WebSocket read error: {e}"))?;
            match msg {
                Message::Text(text) => {
                    let parsed: serde_json::Value = serde_json::from_str(&text)
                        .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {text}"))?;
                    if self.handle_push(&parsed) {
                        continue;
                    }
                    return Ok(parsed);
                }
                Message::Close(_) => {
                    return Err(miette!("WebSocket connection closed by server"));
                }
                // Skip ping/pong/binary frames
                _ => {}
            }
        }
    }

    /// Read a single response line from the connection (WebSocket text frame).
    /// Prints transcript push messages inline; skips other push messages.
    /// Returns `Ok(line)` on success, or an error.
    /// When a read timeout is set and expires, returns `Err` with `WouldBlock` kind.
    pub fn read_response_line(&mut self) -> std::io::Result<String> {
        loop {
            match self.ws.read() {
                Ok(Message::Text(text)) => {
                    if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&text) {
                        if self.handle_push(&parsed) {
                            continue;
                        }
                    }
                    return Ok(text.to_string());
                }
                Ok(Message::Close(_)) => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::ConnectionAborted,
                        "WebSocket closed",
                    ));
                }
                Ok(_) => {}
                Err(tungstenite::Error::Io(io_err)) => return Err(io_err),
                Err(e) => {
                    return Err(std::io::Error::other(e.to_string()));
                }
            }
        }
    }

    /// Send a JSON request without reading the response.
    pub fn send_only(&mut self, request: &serde_json::Value) -> Result<()> {
        let request_str = serde_json::to_string(request).into_diagnostic()?;
        self.ws
            .send(Message::Text(request_str.into()))
            .map_err(|e| miette!("WebSocket send error: {e}"))
    }

    /// Send a JSON request and receive a raw JSON response.
    pub fn send_raw(&mut self, request: &serde_json::Value) -> Result<serde_json::Value> {
        self.send_only(request)?;
        self.read_response()
    }

    /// Send a JSON request and deserialize the response into a typed struct.
    ///
    /// On communication failure (send or read), attempts to reconnect and retry
    /// the request once. **Caution:** if the server processed the request before
    /// the read failed, retrying will re-execute it. Callers should be aware of
    /// duplicate-execution risk for mutating operations.
    pub fn send_request<T: serde::de::DeserializeOwned>(
        &mut self,
        request: &serde_json::Value,
    ) -> Result<T> {
        // Attempt once, and on transport failure try reconnect + retry once.
        for attempt in 0..2 {
            if let Err(e) = self.send_only(request) {
                if attempt == 0 {
                    tracing::debug!("Send failed, attempting reconnect: {e}");
                    self.reconnect()?;
                    continue;
                }
                return Err(e);
            }
            loop {
                match self.ws.read() {
                    Ok(Message::Text(text)) => {
                        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&text) {
                            if self.handle_push(&parsed) {
                                continue;
                            }
                        }
                        return serde_json::from_str(&text)
                            .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {text}"));
                    }
                    Ok(Message::Close(_)) => {
                        if attempt == 0 {
                            self.reconnect()?;
                            break; // retry outer loop
                        }
                        return Err(miette!("WebSocket connection closed by server"));
                    }
                    Ok(_) => {}
                    Err(e) if is_transport_error(&e) => {
                        if attempt == 0 {
                            self.reconnect()?;
                            break; // retry outer loop
                        }
                        return Err(miette!("WebSocket transport error: {e}"));
                    }
                    Err(e) => {
                        // Non-transport error (protocol, UTF-8, etc.) — no retry
                        return Err(miette!("WebSocket error: {e}"));
                    }
                }
            }
        }
        unreachable!("send_request loop always returns")
    }
}

/// Apply `│ ` gutter prefix to transcript text at each line start.
///
/// `bol` tracks whether the cursor is at the beginning of a line across
/// successive calls (since `show:` and `cr` arrive as separate push chunks).
/// Returns the formatted string ready to write to stdout.
pub fn format_transcript_chunk(text: &str, bol: &mut bool) -> String {
    let mut out = String::with_capacity(text.len() + 3);
    for ch in text.chars() {
        if *bol && ch != '\n' {
            out.push_str("│ ");
        }
        out.push(ch);
        *bol = ch == '\n';
    }
    out
}

/// Render a `reload_check`/`completed` push frame's `data` payload as the
/// REPL notice (ADR 0105 §"The demo", BT-2779). Returns `None` if `data`
/// is missing the fields the notice needs (defensive — a malformed push
/// should never crash the REPL; it just prints nothing).
///
/// Mirrors the ADR's demo shape:
/// ```text
/// ⚠ reload check: Counter>>getCount signature changed; 2 callers re-checked, 1 stale
///    Dashboard>>refresh (line 14): `+` expects a number, `getCount` now returns String
/// ```
/// `removal` uses `ℹ` (informational — Hint severity per ADR 0100 Rule 1);
/// `signature_change` uses `⚠`. A `self_edit` classification means no
/// dependent re-check ran at all — this call only exists because clearing
/// this class's own stale findings (its source just changed) was itself
/// worth telling the user about, so the notice is a single clearing line
/// with no caller list.
pub fn format_reload_check_notice(data: &serde_json::Value) -> Option<String> {
    use std::fmt::Write as _;

    let changed_class = data.get("changedClass")?.as_str()?;
    let changed_selector = data.get("changedSelector")?.as_str()?;
    let classification = data
        .get("classification")
        .and_then(serde_json::Value::as_str)
        .unwrap_or("signature_change");

    if classification == "self_edit" {
        return Some(format!(
            "✓ reload check: {changed_class}>>{changed_selector} — cleared stale reload-induced finding(s)"
        ));
    }

    let checked = data
        .get("checked")
        .and_then(serde_json::Value::as_u64)
        .unwrap_or(0);
    let cap_note = data.get("capNote").and_then(|v| v.as_str());
    let findings: Vec<&serde_json::Value> = data
        .get("findings")
        .and_then(|v| v.as_array())
        .map(|a| a.iter().collect())
        .unwrap_or_default();
    let stale = findings.len();

    let icon = if classification == "removal" {
        "ℹ"
    } else {
        "⚠"
    };
    let verb = if classification == "removal" {
        "was removed"
    } else {
        "signature changed"
    };

    let mut out = format!("{icon} reload check: {changed_class}>>{changed_selector} {verb}");
    if checked > 0 {
        let caller_word = if checked == 1 { "caller" } else { "callers" };
        let _ = write!(out, "; {checked} {caller_word} re-checked, {stale} stale");
    }
    if let Some(note) = cap_note {
        let _ = write!(out, " ({note})");
    }

    for finding in findings {
        let owner = finding.get("owner").and_then(|v| v.as_str()).unwrap_or("?");
        let message = finding
            .get("message")
            .and_then(|v| v.as_str())
            .unwrap_or("");
        let sites: Vec<&serde_json::Value> = finding
            .get("sites")
            .and_then(|v| v.as_array())
            .map(|a| a.iter().collect())
            .unwrap_or_default();
        if sites.is_empty() {
            let _ = write!(out, "\n   {owner}: {message}");
            continue;
        }
        for site in sites {
            let method = site.get("method").and_then(|v| v.as_str()).unwrap_or("?");
            let line = site
                .get("line")
                .and_then(serde_json::Value::as_u64)
                .unwrap_or(0);
            let _ = write!(out, "\n   {owner}>>{method} (line {line}): {message}");
        }
    }

    Some(out)
}

#[cfg(test)]
mod tests {
    use super::format_transcript_chunk;

    #[test]
    fn prefix_at_start_of_line() {
        let mut bol = true;
        assert_eq!(format_transcript_chunk("hello", &mut bol), "│ hello");
        assert!(!bol);
    }

    #[test]
    fn no_prefix_mid_line() {
        let mut bol = false;
        assert_eq!(format_transcript_chunk(" world", &mut bol), " world");
        assert!(!bol);
    }

    #[test]
    fn newline_resets_bol() {
        let mut bol = false;
        assert_eq!(format_transcript_chunk("\n", &mut bol), "\n");
        assert!(bol);
    }

    #[test]
    fn show_then_cr_as_separate_chunks() {
        // Simulates `Transcript show: "hello"` followed by `Transcript cr`
        let mut bol = true;
        let s1 = format_transcript_chunk("hello", &mut bol);
        assert_eq!(s1, "│ hello");
        assert!(!bol);
        let s2 = format_transcript_chunk("\n", &mut bol);
        assert_eq!(s2, "\n");
        assert!(bol);
    }

    #[test]
    fn multiline_in_single_chunk() {
        let mut bol = true;
        assert_eq!(
            format_transcript_chunk("hello\nworld", &mut bol),
            "│ hello\n│ world"
        );
        assert!(!bol);
    }

    #[test]
    fn trailing_newline_leaves_bol_true() {
        let mut bol = true;
        assert_eq!(format_transcript_chunk("hello\n", &mut bol), "│ hello\n");
        assert!(bol);
    }

    #[test]
    fn reconnect_mid_line_state_resets_on_reconnect() {
        // After a reconnect the display context is fresh — even if the old
        // connection dropped mid-line (bol=false), the next chunk must prefix.
        // We simulate this by calling format_transcript_chunk with bol=false
        // (mid-line state), then manually resetting bol as reconnect() does,
        // and verifying the next chunk gets the │ prefix.
        let mut bol = true;
        let _ = format_transcript_chunk("partial", &mut bol);
        assert!(!bol, "mid-line after partial chunk");
        // reconnect() resets to true
        bol = true;
        assert_eq!(
            format_transcript_chunk("next line", &mut bol),
            "│ next line"
        );
    }

    #[test]
    fn empty_chunk_is_noop() {
        let mut bol = true;
        assert_eq!(format_transcript_chunk("", &mut bol), "");
        assert!(bol); // unchanged
    }

    #[test]
    fn newline_only_chunk_does_not_prefix() {
        // A bare `cr` push: starts mid-line, just a newline — no prefix
        let mut bol = false;
        assert_eq!(format_transcript_chunk("\n", &mut bol), "\n");
        assert!(bol);
    }

    mod reload_check_notice {
        use super::super::format_reload_check_notice;
        use serde_json::json;

        #[test]
        fn signature_change_with_one_stale_caller() {
            let data = json!({
                "changedClass": "Counter",
                "changedSelector": "getCount",
                "classification": "signature_change",
                "checked": 2,
                "notChecked": 0,
                "capNote": null,
                "checkedOwners": ["Dashboard", "StatsView"],
                "findings": [{
                    "owner": "Dashboard",
                    "changedClass": "Counter",
                    "selector": "getCount",
                    "classification": "signature_change",
                    "severity": "warning",
                    "category": "Dnu",
                    "message": "String does not understand '+'",
                    "note": null,
                    "sites": [{"method": "refresh", "line": 14}],
                    "start": 0,
                    "end": 10
                }]
            });
            let notice = format_reload_check_notice(&data).expect("notice");
            assert_eq!(
                notice,
                "⚠ reload check: Counter>>getCount signature changed; 2 callers re-checked, 1 stale\n   Dashboard>>refresh (line 14): String does not understand '+'"
            );
        }

        #[test]
        fn removal_uses_info_icon() {
            let data = json!({
                "changedClass": "Counter",
                "changedSelector": "reset",
                "classification": "removal",
                "checked": 1,
                "notChecked": 0,
                "capNote": null,
                "checkedOwners": ["AdminPanel"],
                "findings": [{
                    "owner": "AdminPanel",
                    "changedClass": "Counter",
                    "selector": "reset",
                    "classification": "removal",
                    "severity": "hint",
                    "category": "Dnu",
                    "message": "'Counter' does not understand 'reset'",
                    "note": "removed by the reload of Counter",
                    "sites": [{"method": "onClick", "line": 9}],
                    "start": 0,
                    "end": 5
                }]
            });
            let notice = format_reload_check_notice(&data).expect("notice");
            assert!(notice.starts_with("ℹ reload check: Counter>>reset was removed"));
            assert!(notice.contains("1 caller re-checked, 1 stale"));
            assert!(notice.contains("AdminPanel>>onClick (line 9)"));
        }

        #[test]
        fn clean_recheck_has_no_stale_findings() {
            // reload-fixes-reload: a recheck ran (checked=1) but produced no
            // findings — the caller was clean against the new generation.
            let data = json!({
                "changedClass": "Counter",
                "changedSelector": "getCount",
                "classification": "signature_change",
                "checked": 1,
                "notChecked": 0,
                "capNote": null,
                "checkedOwners": ["Dashboard"],
                "findings": []
            });
            let notice = format_reload_check_notice(&data).expect("notice");
            assert_eq!(
                notice,
                "⚠ reload check: Counter>>getCount signature changed; 1 caller re-checked, 0 stale"
            );
        }

        #[test]
        fn cap_note_is_appended() {
            let data = json!({
                "changedClass": "Counter",
                "changedSelector": "size",
                "classification": "signature_change",
                "checked": 20,
                "notChecked": 5,
                "capNote": "5 more not checked",
                "checkedOwners": [],
                "findings": []
            });
            let notice = format_reload_check_notice(&data).expect("notice");
            assert!(notice.ends_with("(5 more not checked)"));
        }

        #[test]
        fn self_edit_classification_is_a_single_clearing_line() {
            let data = json!({
                "changedClass": "Dashboard",
                "changedSelector": "refresh",
                "classification": "self_edit",
                "checked": 0,
                "notChecked": 0,
                "capNote": null,
                "checkedOwners": ["Dashboard"],
                "findings": []
            });
            let notice = format_reload_check_notice(&data).expect("notice");
            assert_eq!(
                notice,
                "✓ reload check: Dashboard>>refresh — cleared stale reload-induced finding(s)"
            );
        }

        #[test]
        fn missing_required_field_returns_none() {
            let data = json!({"changedClass": "Counter"});
            assert!(format_reload_check_notice(&data).is_none());
        }
    }
}
