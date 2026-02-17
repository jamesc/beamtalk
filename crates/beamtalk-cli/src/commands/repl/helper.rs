// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL helper: tab completion, syntax highlighting, and input validation.
//!
//! **DDD Context:** REPL — Interactive Assistance
//!
//! Implements rustyline's `Helper` trait, which bundles:
//! - `Completer` — Tab completion for commands, class names, keywords
//! - `Highlighter` — Syntax highlighting using beamtalk's lexer
//! - `Hinter` — Inline hints (currently unused)
//! - `Validator` — Input validation (currently unused)

use std::borrow::Cow;
use std::cell::RefCell;
use std::time::Duration;

use rustyline::completion::{Completer, Pair};
use rustyline::highlight::{CmdKind, Highlighter};
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::{Context, Helper};

use beamtalk_core::source_analysis::{TokenKind, Trivia, lex_with_eof};

use crate::commands::protocol::{self, ProtocolClient};

use super::ReplResponse;
use super::color;

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

/// REPL helper providing tab completion and syntax highlighting.
///
/// Uses a separate TCP connection for completion requests so the main
/// REPL connection isn't affected by timeouts or errors.
pub(super) struct ReplHelper {
    /// Separate protocol client for completion requests (with short timeout).
    completion_client: RefCell<Option<ProtocolClient>>,
    /// Port for reconnection if the completion client disconnects.
    port: u16,
}

impl ReplHelper {
    /// Create a new helper that connects to the backend on the given port.
    pub(super) fn new(port: u16) -> Self {
        let client = ProtocolClient::connect(port, Some(COMPLETION_TIMEOUT))
            .ok()
            .map(|mut c| {
                // BT-666: Consume the session-started welcome message
                let _ = c.read_response_line();
                c
            });
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
            *client_ref = ProtocolClient::connect(self.port, Some(COMPLETION_TIMEOUT))
                .ok()
                .map(|mut c| {
                    // BT-666: Consume the session-started welcome message
                    let _ = c.read_response_line();
                    c
                });
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

impl Completer for ReplHelper {
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

impl Hinter for ReplHelper {
    type Hint = String;

    fn hint(&self, _line: &str, _pos: usize, _ctx: &Context<'_>) -> Option<Self::Hint> {
        None
    }
}

impl Highlighter for ReplHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        if !color::is_enabled() || line.is_empty() {
            return Cow::Borrowed(line);
        }

        // REPL commands get special highlighting
        if line.starts_with(':') {
            return Cow::Owned(format!("{}{}{}", color::BOLD_CYAN, line, color::RESET));
        }

        Cow::Owned(highlight_line(line))
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        if !color::is_enabled() {
            return Cow::Borrowed(prompt);
        }
        if prompt == "..> " {
            Cow::Owned(format!("{}{}{}", color::YELLOW, prompt, color::RESET))
        } else {
            Cow::Owned(format!("{}{}{}", color::GREEN, prompt, color::RESET))
        }
    }

    fn highlight_char(&self, line: &str, pos: usize, kind: CmdKind) -> bool {
        match kind {
            // Always re-highlight when input changes or on forced refresh
            CmdKind::Other | CmdKind::ForcedRefresh => true,
            // On cursor move, only re-highlight near brackets
            CmdKind::MoveCursor => {
                let bytes = line.as_bytes();
                if pos < bytes.len() {
                    matches!(bytes[pos], b'(' | b')' | b'[' | b']' | b'{' | b'}')
                } else if pos > 0 && pos <= bytes.len() {
                    matches!(bytes[pos - 1], b'(' | b')' | b'[' | b']' | b'{' | b'}')
                } else {
                    false
                }
            }
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        if !color::is_enabled() {
            return Cow::Borrowed(hint);
        }
        Cow::Owned(format!("{}{}{}", color::DIM, hint, color::RESET))
    }
}

impl Validator for ReplHelper {}
impl Helper for ReplHelper {}

/// Beamtalk keywords that get special highlighting.
const KEYWORDS: &[&str] = &["self", "super", "true", "false", "nil"];

/// Highlight a line of Beamtalk code using the lexer.
fn highlight_line(line: &str) -> String {
    let mut result = String::with_capacity(line.len() * 2);
    let mut last_pos = 0usize;

    // Use lex_with_eof to include the Eof token, which carries trailing
    // comments/whitespace as leading trivia (e.g., comment-only lines).
    for token in lex_with_eof(line) {
        // Process leading trivia (comments, whitespace)
        for trivia in token.leading_trivia() {
            let trivia_text = trivia.as_str();
            match trivia {
                Trivia::LineComment(_) | Trivia::BlockComment(_) | Trivia::DocComment(_) => {
                    result.push_str(color::GRAY);
                    result.push_str(trivia_text);
                    result.push_str(color::RESET);
                    last_pos += trivia_text.len();
                }
                Trivia::Whitespace(_) => {
                    result.push_str(trivia_text);
                    last_pos += trivia_text.len();
                }
            }
        }

        let span = token.span();
        let start = span.start() as usize;
        let end = span.end() as usize;

        // Fill any gap between last position and token start
        if start > last_pos && start <= line.len() {
            result.push_str(&line[last_pos..start]);
        }

        let text = if end <= line.len() {
            &line[start..end]
        } else {
            &line[start..]
        };

        let col = match token.kind() {
            TokenKind::String(_)
            | TokenKind::StringStart(_)
            | TokenKind::StringSegment(_)
            | TokenKind::StringEnd(_)
            | TokenKind::Character(_) => Some(color::GREEN),
            TokenKind::Integer(_) | TokenKind::Float(_) => Some(color::YELLOW),
            TokenKind::Symbol(_) | TokenKind::Hash => Some(color::CYAN),
            TokenKind::Identifier(name) => {
                let name_str: &str = name;
                if KEYWORDS.contains(&name_str) {
                    Some(color::BOLD_BLUE)
                } else if name_str.starts_with(|c: char| c.is_ascii_uppercase()) {
                    Some(color::MAGENTA)
                } else {
                    None
                }
            }
            TokenKind::Caret => Some(color::BOLD_BLUE), // ^return
            TokenKind::AtPrimitive | TokenKind::AtIntrinsic => Some(color::MAGENTA),
            TokenKind::Error(_) => Some(color::BOLD_RED),
            _ => None,
        };

        if let Some(c) = col {
            result.push_str(c);
            result.push_str(text);
            result.push_str(color::RESET);
        } else {
            result.push_str(text);
        }

        last_pos = end;

        // Process trailing trivia
        for trivia in token.trailing_trivia() {
            let trivia_text = trivia.as_str();
            match trivia {
                Trivia::LineComment(_) | Trivia::BlockComment(_) | Trivia::DocComment(_) => {
                    result.push_str(color::GRAY);
                    result.push_str(trivia_text);
                    result.push_str(color::RESET);
                    last_pos += trivia_text.len();
                }
                Trivia::Whitespace(_) => {
                    result.push_str(trivia_text);
                    last_pos += trivia_text.len();
                }
            }
        }
    }

    // Append any remaining text after the last token
    if last_pos < line.len() {
        result.push_str(&line[last_pos..]);
    }

    result
}

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

    // === Highlighting tests ===

    #[test]
    fn highlight_empty_line() {
        assert_eq!(highlight_line(""), "");
    }

    #[test]
    fn highlight_integer_literal() {
        let result = highlight_line("42");
        assert!(result.contains(color::YELLOW));
        assert!(result.contains("42"));
        assert!(result.contains(color::RESET));
    }

    #[test]
    fn highlight_string_literal() {
        let result = highlight_line("\"hello\"");
        assert!(result.contains(color::GREEN));
        assert!(result.contains("\"hello\""));
    }

    #[test]
    fn highlight_keyword_self() {
        let result = highlight_line("self");
        assert!(result.contains(color::BOLD_BLUE));
        assert!(result.contains("self"));
    }

    #[test]
    fn highlight_keyword_true() {
        let result = highlight_line("true");
        assert!(result.contains(color::BOLD_BLUE));
    }

    #[test]
    fn highlight_class_name() {
        let result = highlight_line("Counter");
        assert!(result.contains(color::MAGENTA));
        assert!(result.contains("Counter"));
    }

    #[test]
    fn highlight_symbol() {
        let result = highlight_line("#foo");
        assert!(result.contains(color::CYAN));
    }

    #[test]
    fn highlight_comment() {
        let result = highlight_line("x + 1 // note");
        assert!(result.contains(color::GRAY));
        assert!(result.contains("// note"));
    }

    #[test]
    fn highlight_comment_only_line() {
        let result = highlight_line("// just a comment");
        assert!(result.contains(color::GRAY));
        assert!(result.contains("// just a comment"));
    }

    #[test]
    fn highlight_regular_identifier_no_color() {
        let result = highlight_line("x");
        // Regular identifiers should not have color codes
        assert!(!result.contains(color::BOLD_BLUE));
        assert!(!result.contains(color::MAGENTA));
    }

    #[test]
    fn highlight_mixed_expression() {
        let result = highlight_line("Counter spawn");
        assert!(result.contains(color::MAGENTA)); // Counter
        // "spawn" is a regular identifier - no special color
    }

    #[test]
    fn highlight_preserves_whitespace() {
        let result = highlight_line("x + 1");
        // Should contain spaces between tokens
        assert!(result.contains(" + ") || result.contains(' '));
    }
}
