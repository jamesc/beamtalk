// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Transcript viewer command for streaming workspace Transcript output.
//!
//! **DDD Context:** REPL — Transcript Viewer
//!
//! Connects to a running workspace node and displays `TranscriptStream` output,
//! similar to `tail -f` on a log file. Uses the existing REPL TCP protocol
//! to poll the `TranscriptStream` actor's ring buffer.

use std::io::{ErrorKind, Write};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};

use crate::commands::protocol::{self, ProtocolClient};
use crate::commands::workspace;

/// Polling interval for transcript updates in milliseconds.
const POLL_INTERVAL_MS: u64 = 500;

/// Read timeout for polling responses in milliseconds.
const READ_TIMEOUT_MS: u64 = 10_000;

/// Run the transcript viewer command.
pub fn run(name_or_id: Option<&str>, recent: Option<usize>) -> Result<()> {
    // Resolve workspace by name/ID or current directory (same as workspace stop/status/attach)
    let workspace_id = workspace::lifecycle::resolve_workspace_id_or_cwd(name_or_id)?;

    // Check workspace exists
    if !workspace::workspace_exists(&workspace_id)? {
        return Err(match name_or_id {
            Some(name) => {
                miette!(
                    "Workspace '{name}' does not exist. \
                     Start one with `beamtalk repl --workspace {name}`."
                )
            }
            None => {
                miette!("No workspace found for current directory. Start one with `beamtalk repl`.")
            }
        });
    }

    // Get node info
    let node_info = workspace::get_node_info(&workspace_id)?
        .ok_or_else(|| miette!("No workspace found. Start one with `beamtalk repl`."))?;

    // Verify node is actually running
    if !workspace::is_node_running(&node_info, Some(&workspace_id)) {
        workspace::cleanup_stale_node_info(&workspace_id)?;
        return Err(miette!(
            "No workspace found. Start one with `beamtalk repl`."
        ));
    }

    // Read workspace cookie for WebSocket authentication (ADR 0020)
    let cookie = workspace::read_workspace_cookie(&workspace_id)?
        .trim()
        .to_string();
    if cookie.is_empty() {
        return Err(miette!(
            "Workspace cookie is empty; restart workspace with `beamtalk repl`"
        ));
    }

    // Connect to workspace REPL backend
    let mut client = TranscriptClient::connect(node_info.connect_host(), node_info.port, &cookie)?;

    // Set up Ctrl-C handler.
    //
    // `ctrlc::set_handler` may only be installed once per process — a second
    // call anywhere in this binary returns `Err`. No unit test may reach this
    // line more than once across the whole `beamtalk-cli` test binary; see
    // this module's `tests::run_early_returns` doc comment for the tests that
    // deliberately stop short of it.
    let running = Arc::new(AtomicBool::new(true));
    let r = Arc::clone(&running);
    ctrlc::set_handler(move || {
        r.store(false, Ordering::SeqCst);
    })
    .into_diagnostic()?;

    // Fetch initial buffer
    let buffer = client.fetch_recent()?;

    if let Some(n) = recent {
        // --recent N: display last N entries and continue streaming
        let start = buffer.len().saturating_sub(n);
        for entry in &buffer[start..] {
            if !print_entry(entry) {
                return Ok(());
            }
        }
    }

    // Stream mode: poll for new entries
    let mut cursor = TranscriptCursor::new(&buffer);

    while running.load(Ordering::SeqCst) {
        std::thread::sleep(Duration::from_millis(POLL_INTERVAL_MS));

        if !running.load(Ordering::SeqCst) {
            break;
        }

        let Ok(buffer) = client.fetch_recent() else {
            // Connection lost
            eprintln!("Connection to workspace lost.");
            break;
        };

        let new_entries = cursor.update(&buffer);
        for entry in &new_entries {
            if !print_entry(entry) {
                return Ok(());
            }
        }
    }

    Ok(())
}

/// Print a transcript entry as plain text.
/// Returns false if stdout is broken (pipe closed), signaling the caller to exit.
fn print_entry(entry: &str) -> bool {
    // Entries may contain newline characters; print as-is for faithful output
    print!("{entry}");
    // Flush to ensure output appears immediately; exit on broken pipe
    if let Err(e) = std::io::stdout().flush() {
        if e.kind() == ErrorKind::BrokenPipe {
            return false;
        }
    }
    true
}

/// Transcript-specific client wrapping the shared protocol transport.
struct TranscriptClient {
    inner: ProtocolClient,
}

impl TranscriptClient {
    /// Connect to the workspace backend at the given host and port.
    fn connect(host: &str, port: u16, cookie: &str) -> Result<Self> {
        let inner = ProtocolClient::connect(
            host,
            port,
            cookie,
            Some(Duration::from_millis(READ_TIMEOUT_MS)),
        )?;
        Ok(Self { inner })
    }

    /// Fetch recent transcript entries via eval.
    fn fetch_recent(&mut self) -> Result<Vec<String>> {
        let request = serde_json::json!({
            "op": "eval",
            "id": protocol::next_msg_id(),
            "code": "Transcript recent"
        });

        let response = self.inner.send_raw(&request)?;

        // Check for error
        if let Some(error) = response.get("error").and_then(|e| e.as_str()) {
            return Err(miette!("Transcript error: {error}"));
        }

        // Parse the value field — the REPL's term_to_json converts a list of
        // binaries into a JSON array of strings: ["hello", "world"]
        let value = response.get("value");
        if let Some(arr) = value.and_then(|v| v.as_array()) {
            Ok(arr
                .iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect())
        } else if let Some(s) = value.and_then(|v| v.as_str()) {
            // Fallback: single string value (e.g., empty list formatted as "[]")
            if s == "[]" || s.is_empty() {
                Ok(vec![])
            } else {
                Ok(vec![s.to_string()])
            }
        } else {
            Ok(vec![])
        }
    }
}

/// Tracks the transcript buffer state to detect new entries.
struct TranscriptCursor {
    /// Snapshot of the last buffer we've seen, used as a fingerprint for overlap detection.
    last_entries: Vec<String>,
}

impl TranscriptCursor {
    /// Create a new cursor initialized with the given buffer snapshot.
    fn new(initial_buffer: &[String]) -> Self {
        Self {
            last_entries: initial_buffer.to_vec(),
        }
    }

    /// Compare new buffer against last known state and return only new entries.
    fn update(&mut self, buffer: &[String]) -> Vec<String> {
        if buffer.is_empty() {
            self.last_entries.clear();
            return vec![];
        }

        if self.last_entries.is_empty() {
            self.last_entries = buffer.to_vec();
            return buffer.to_vec();
        }

        // Find where the old buffer's tail overlaps with the new buffer.
        // The ring buffer appends new entries at the end and drops from the front.
        // Strategy: find the last entry of old buffer in the new buffer,
        // then everything after it is new.
        let new_entries = if let Some(last_old) = self.last_entries.last() {
            // Search backwards in new buffer for our cursor entry
            let mut found_idx = None;
            for i in (0..buffer.len()).rev() {
                if &buffer[i] == last_old {
                    // Verify a few preceding entries match to avoid false positives
                    let match_confirmed = if self.last_entries.len() >= 2 && i > 0 {
                        let second_last_old = &self.last_entries[self.last_entries.len() - 2];
                        &buffer[i - 1] == second_last_old
                    } else {
                        true
                    };

                    if match_confirmed {
                        found_idx = Some(i);
                        break;
                    }
                }
            }

            match found_idx {
                Some(idx) if idx + 1 < buffer.len() => buffer[idx + 1..].to_vec(),
                Some(_) => vec![], // Cursor at end, nothing new
                None => {
                    // Cursor entry not found in new buffer — buffer wrapped completely.
                    // Show all entries in new buffer.
                    buffer.to_vec()
                }
            }
        } else {
            buffer.to_vec()
        };

        self.last_entries = buffer.to_vec();
        new_entries
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cursor_initial_no_new() {
        let buffer = vec!["a".to_string(), "b".to_string()];
        let mut cursor = TranscriptCursor::new(&buffer);

        // Same buffer — nothing new
        let new = cursor.update(&buffer);
        assert!(new.is_empty());
    }

    #[test]
    fn test_cursor_detects_new_entries() {
        let initial = vec!["a".to_string(), "b".to_string()];
        let mut cursor = TranscriptCursor::new(&initial);

        let updated = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let new = cursor.update(&updated);
        assert_eq!(new, vec!["c"]);
    }

    #[test]
    fn test_cursor_buffer_wrapped() {
        let initial = vec!["a".to_string(), "b".to_string()];
        let mut cursor = TranscriptCursor::new(&initial);

        // Completely different buffer (old entries dropped)
        let updated = vec!["x".to_string(), "y".to_string(), "z".to_string()];
        let new = cursor.update(&updated);
        assert_eq!(new, vec!["x", "y", "z"]);
    }

    #[test]
    fn test_cursor_empty_to_entries() {
        let mut cursor = TranscriptCursor::new(&[]);

        let buffer = vec!["a".to_string(), "b".to_string()];
        let new = cursor.update(&buffer);
        assert_eq!(new, vec!["a", "b"]);
    }

    #[test]
    fn test_cursor_entries_to_empty() {
        let initial = vec!["a".to_string()];
        let mut cursor = TranscriptCursor::new(&initial);

        let new = cursor.update(&[]);
        assert!(new.is_empty());
    }

    #[test]
    fn test_cursor_partial_wrap() {
        let initial = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let mut cursor = TranscriptCursor::new(&initial);

        // "a" was dropped, "d" and "e" added
        let updated = vec![
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
            "e".to_string(),
        ];
        let new = cursor.update(&updated);
        assert_eq!(new, vec!["d", "e"]);
    }

    // -- TranscriptClient --------------------------------------------------
    //
    // `spawn_auth_ok_server`/`spawn_auth_error_server` are the shared
    // synchronous ADR 0020 handshake doubles in `crate::commands::test_support`
    // (BT-3349) — see that module's doc comment.

    mod transcript_client {
        use super::super::TranscriptClient;
        use crate::commands::test_support::{spawn_auth_error_server, spawn_auth_ok_server};
        use tungstenite::Message;

        #[test]
        fn connect_fails_against_unbound_port() {
            let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
            let port = listener.local_addr().expect("local_addr").port();
            drop(listener);

            let Err(err) = TranscriptClient::connect("127.0.0.1", port, "cookie") else {
                panic!("expected connect to fail against an unbound port");
            };
            assert!(err.to_string().contains("Failed to connect"));
        }

        #[test]
        fn connect_fails_on_auth_error() {
            let port = spawn_auth_error_server("bad cookie");
            let Err(err) = TranscriptClient::connect("127.0.0.1", port, "wrong") else {
                panic!("expected connect to fail on auth_error");
            };
            assert!(err.to_string().contains("bad cookie"));
        }

        #[test]
        fn fetch_recent_parses_array_of_strings() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("code").and_then(|v| v.as_str()) == Some("Transcript recent") {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"], "value": ["hello\n", "world\n"]})
                            .to_string()
                            .into(),
                    ));
                }
            });
            let mut client =
                TranscriptClient::connect("127.0.0.1", port, "cookie").expect("connect");
            let entries = client.fetch_recent().expect("fetch_recent");
            assert_eq!(entries, vec!["hello\n", "world\n"]);
        }

        #[test]
        fn fetch_recent_empty_array_string_fallback_is_empty() {
            let port = spawn_auth_ok_server(|_req, ws| {
                let _ = ws.send(Message::Text(
                    serde_json::json!({"status": ["done"], "value": "[]"})
                        .to_string()
                        .into(),
                ));
            });
            let mut client =
                TranscriptClient::connect("127.0.0.1", port, "cookie").expect("connect");
            let entries = client.fetch_recent().expect("fetch_recent");
            assert!(entries.is_empty());
        }

        #[test]
        fn fetch_recent_single_string_fallback_wraps_in_vec() {
            let port = spawn_auth_ok_server(|_req, ws| {
                let _ = ws.send(Message::Text(
                    serde_json::json!({"status": ["done"], "value": "solo"})
                        .to_string()
                        .into(),
                ));
            });
            let mut client =
                TranscriptClient::connect("127.0.0.1", port, "cookie").expect("connect");
            let entries = client.fetch_recent().expect("fetch_recent");
            assert_eq!(entries, vec!["solo"]);
        }

        #[test]
        fn fetch_recent_missing_value_is_empty() {
            let port = spawn_auth_ok_server(|_req, ws| {
                let _ = ws.send(Message::Text(
                    serde_json::json!({"status": ["done"]}).to_string().into(),
                ));
            });
            let mut client =
                TranscriptClient::connect("127.0.0.1", port, "cookie").expect("connect");
            let entries = client.fetch_recent().expect("fetch_recent");
            assert!(entries.is_empty());
        }

        #[test]
        fn fetch_recent_server_error_is_propagated() {
            let port = spawn_auth_ok_server(|_req, ws| {
                let _ = ws.send(Message::Text(
                    serde_json::json!({"error": "eval failed"})
                        .to_string()
                        .into(),
                ));
            });
            let mut client =
                TranscriptClient::connect("127.0.0.1", port, "cookie").expect("connect");
            let err = client.fetch_recent().unwrap_err();
            assert!(err.to_string().contains("eval failed"));
        }
    }

    // -- run() early-return paths -------------------------------------------
    //
    // Only the pre-handshake branches are exercised here: `run` registers a
    // process-global `ctrlc::set_handler` right after connecting, and that
    // handler may only be installed once per test process — so no test below
    // may reach a successful `TranscriptClient::connect`, and only one test
    // in the whole crate can ever exercise `run`'s streaming loop. See
    // `crate::commands::test_support` for the `WorkspaceFixture` used here.

    mod run_early_returns {
        use super::super::run;
        use crate::commands::test_support::{WorkspaceFixture, spawn_auth_error_server};
        use crate::commands::workspace::storage::save_workspace_cookie;

        #[test]
        fn missing_workspace_with_name_errors() {
            let err = run(Some("bt3349-transcript-no-such-workspace"), None).unwrap_err();
            assert!(err.to_string().contains("does not exist"), "got: {err}");
        }

        #[test]
        fn node_not_running_errors_and_cleans_up() {
            // Bind then drop so nothing is listening on this port — the TCP
            // liveness probe in `is_node_running` fails.
            let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
            let port = listener.local_addr().expect("local_addr").port();
            drop(listener);
            let fixture = WorkspaceFixture::new("not-running", port, 4_194_304);

            let err = run(Some(&fixture.id), None).unwrap_err();
            assert!(err.to_string().contains("No workspace found"), "got: {err}");
        }

        #[test]
        fn empty_cookie_errors() {
            let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
            let port = listener.local_addr().expect("local_addr").port();
            let fixture = WorkspaceFixture::new("empty-cookie", port, std::process::id());
            // Overwrite the fixture's default "cookie" with an empty one.
            save_workspace_cookie(&fixture.id, "").expect("save empty cookie");

            let err = run(Some(&fixture.id), None).unwrap_err();
            assert!(err.to_string().contains("cookie is empty"), "got: {err}");
            drop(listener);
        }

        #[test]
        fn connect_handshake_rejected_errors() {
            // `spawn_auth_error_server` fails the handshake before `run` would
            // reach `ctrlc::set_handler`, so this stays safe alongside the
            // other tests in this module.
            let port = spawn_auth_error_server("bad cookie");
            let fixture = WorkspaceFixture::new("handshake-rejected", port, std::process::id());

            let err = run(Some(&fixture.id), None).unwrap_err();
            assert!(err.to_string().contains("bad cookie"), "got: {err}");
        }
    }
}
