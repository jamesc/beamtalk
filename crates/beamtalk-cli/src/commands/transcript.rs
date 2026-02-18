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
pub fn run(workspace_name: Option<&str>, recent: Option<usize>) -> Result<()> {
    // Discover workspace
    let current_dir = std::env::current_dir().into_diagnostic()?;
    let project_root = workspace::discovery::discover_project_root(&current_dir);
    let workspace_id = workspace::workspace_id_for(&project_root, workspace_name)?;

    // Check workspace exists
    if !workspace::workspace_exists(&workspace_id)? {
        return Err(miette!(
            "No workspace found. Start one with `beamtalk repl`."
        ));
    }

    // Get node info
    let node_info = workspace::get_node_info(&workspace_id)?
        .ok_or_else(|| miette!("No workspace found. Start one with `beamtalk repl`."))?;

    // Verify node is actually running
    if !workspace::is_node_running(&node_info) {
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

    // Set up Ctrl-C handler
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
}
