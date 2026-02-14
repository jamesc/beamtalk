// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tab completion for the Beamtalk REPL.
//!
//! **DDD Context:** REPL — Interactive Completion
//!
//! Provides tab completion for:
//! - REPL commands (`:load`, `:help`, etc.) — completed client-side
//! - Class names from loaded modules — via backend `complete` op
//! - Keywords (`self`, `true`, `false`, etc.) — via backend `complete` op

use std::cell::RefCell;
use std::time::Duration;

use rustyline::completion::{Completer, Pair};
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::{Context, Helper};

use crate::commands::protocol::{self, ProtocolClient};

use super::ReplResponse;

/// Timeout for completion requests to avoid blocking the REPL.
const COMPLETION_TIMEOUT: Duration = Duration::from_millis(500);

/// REPL commands available for client-side completion.
const REPL_COMMANDS: &[&str] = &[
    ":help",
    ":h",
    ":?",
    ":exit",
    ":quit",
    ":q",
    ":clear",
    ":bindings",
    ":b",
    ":load",
    ":l",
    ":reload",
    ":r",
    ":modules",
    ":m",
    ":unload",
    ":actors",
    ":a",
    ":kill",
    ":inspect",
    ":sessions",
];

/// Tab completion helper for the Beamtalk REPL.
///
/// Uses a separate TCP connection for completion requests so the main
/// REPL connection isn't affected by timeouts or errors.
pub(super) struct ReplCompleter {
    /// Separate protocol client for completion requests (with short timeout).
    completion_client: RefCell<Option<ProtocolClient>>,
    /// Port for reconnection if the completion client disconnects.
    port: u16,
}

impl ReplCompleter {
    /// Create a new completer that connects to the backend on the given port.
    pub(super) fn new(port: u16) -> Self {
        let client = ProtocolClient::connect(port, Some(COMPLETION_TIMEOUT)).ok();
        Self {
            completion_client: RefCell::new(client),
            port,
        }
    }

    /// Query the backend for completions matching a prefix.
    fn backend_complete(&self, prefix: &str) -> Vec<String> {
        if prefix.is_empty() {
            return Vec::new();
        }

        let mut client_ref = self.completion_client.borrow_mut();

        // Try to reconnect if we don't have a client
        if client_ref.is_none() {
            *client_ref = ProtocolClient::connect(self.port, Some(COMPLETION_TIMEOUT)).ok();
        }

        let Some(client) = client_ref.as_mut() else {
            return Vec::new();
        };

        let request = serde_json::json!({
            "op": "complete",
            "id": protocol::next_msg_id(),
            "code": prefix
        });

        if let Ok(response) = client.send_request::<ReplResponse>(&request) {
            response.completions.unwrap_or_default()
        } else {
            // Connection failed — drop client so we reconnect next time
            *client_ref = None;
            Vec::new()
        }
    }
}

impl Completer for ReplCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        let line_to_pos = &line[..pos];

        // REPL command completion (starts with `:`, no arguments yet)
        if line_to_pos.starts_with(':') && !line_to_pos.contains(' ') {
            let candidates: Vec<Pair> = REPL_COMMANDS
                .iter()
                .filter(|cmd| cmd.starts_with(line_to_pos))
                .map(|cmd| Pair {
                    display: cmd.to_string(),
                    replacement: cmd.to_string(),
                })
                .collect();
            return Ok((0, candidates));
        }

        // Find the start of the current word (identifier boundary)
        let word_start = line_to_pos
            .char_indices()
            .rev()
            .find(|&(_, c)| !c.is_ascii_alphanumeric() && c != '_')
            .map_or(0, |(i, c)| i + c.len_utf8());
        let prefix = &line_to_pos[word_start..];

        if prefix.is_empty() {
            return Ok((pos, Vec::new()));
        }

        // Query backend for completions
        let completions = self.backend_complete(prefix);
        let candidates: Vec<Pair> = completions
            .into_iter()
            .map(|c| Pair {
                display: c.clone(),
                replacement: c,
            })
            .collect();

        Ok((word_start, candidates))
    }
}

impl Hinter for ReplCompleter {
    type Hint = String;

    fn hint(&self, _line: &str, _pos: usize, _ctx: &Context<'_>) -> Option<Self::Hint> {
        None
    }
}

impl Highlighter for ReplCompleter {}
impl Validator for ReplCompleter {}
impl Helper for ReplCompleter {}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: extract word start position from a line (same logic as Completer).
    fn find_word_start(line: &str) -> usize {
        line.char_indices()
            .rev()
            .find(|&(_, c)| !c.is_ascii_alphanumeric() && c != '_')
            .map_or(0, |(i, c)| i + c.len_utf8())
    }

    /// Helper: get REPL command completions for a prefix.
    fn command_completions(prefix: &str) -> Vec<String> {
        REPL_COMMANDS
            .iter()
            .filter(|cmd| cmd.starts_with(prefix))
            .map(ToString::to_string)
            .collect()
    }

    #[test]
    fn repl_command_completion_matches_prefix() {
        let candidates = command_completions(":hel");
        assert!(candidates.contains(&":help".to_string()));
        assert!(candidates.iter().all(|c| c.starts_with(":hel")));
    }

    #[test]
    fn repl_command_completion_multiple_matches() {
        let candidates = command_completions(":r");
        assert!(candidates.contains(&":reload".to_string()));
        assert!(candidates.contains(&":r".to_string()));
    }

    #[test]
    fn repl_command_completion_unknown_prefix_is_empty() {
        let candidates = command_completions(":unknown");
        assert!(candidates.is_empty());
    }

    #[test]
    fn repl_command_completion_colon_only() {
        let candidates = command_completions(":");
        assert_eq!(candidates.len(), REPL_COMMANDS.len());
    }

    #[test]
    fn word_boundary_detects_last_identifier() {
        let start = find_word_start("obj message");
        assert_eq!(&"obj message"[start..], "message");
    }

    #[test]
    fn word_boundary_single_word() {
        let start = find_word_start("Counter");
        assert_eq!(&"Counter"[start..], "Counter");
    }

    #[test]
    fn word_boundary_after_space() {
        let start = find_word_start("obj ");
        assert_eq!(&"obj "[start..], "");
    }

    #[test]
    fn word_boundary_after_dot() {
        let start = find_word_start("self.val");
        assert_eq!(&"self.val"[start..], "val");
    }

    #[test]
    fn word_boundary_after_colon() {
        let start = find_word_start("ifTrue:");
        assert_eq!(&"ifTrue:"[start..], "");
    }

    #[test]
    fn word_boundary_unicode_non_ascii_is_boundary() {
        // Unicode alpha chars are treated as boundaries (lexer only allows ASCII)
        let start = find_word_start("über foo");
        assert_eq!(&"über foo"[start..], "foo");
    }

    #[test]
    fn word_boundary_empty_input() {
        let start = find_word_start("");
        assert_eq!(start, 0);
    }

    #[test]
    fn word_boundary_underscore_in_identifier() {
        let start = find_word_start("my_var");
        assert_eq!(&"my_var"[start..], "my_var");
    }
}
