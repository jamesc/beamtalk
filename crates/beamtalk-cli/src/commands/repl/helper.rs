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
use std::path::Path;
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
/// Uses a separate WebSocket connection for completion requests so the main
/// REPL connection isn't affected by timeouts or errors.
pub(crate) struct ReplHelper {
    /// Separate protocol client for completion requests (with short timeout).
    completion_client: RefCell<Option<ProtocolClient>>,
    /// Host address for reconnection (BT-694).
    host: String,
    /// Port for reconnection if the completion client disconnects.
    port: u16,
    /// Cookie for reconnection (ADR 0020).
    cookie: String,
}

impl ReplHelper {
    /// Create a new helper that connects to the backend on the given port.
    pub(crate) fn new(host: &str, port: u16, cookie: &str) -> Self {
        let client = ProtocolClient::connect(host, port, cookie, Some(COMPLETION_TIMEOUT)).ok();
        Self {
            completion_client: RefCell::new(client),
            host: host.to_string(),
            port,
            cookie: cookie.to_string(),
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
            *client_ref = ProtocolClient::connect(
                &self.host,
                self.port,
                &self.cookie,
                Some(COMPLETION_TIMEOUT),
            )
            .ok();
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

/// Detect if the line-so-far is a `:load`/`:l`/`:reload`/`:r` command and extract the path.
///
/// Returns `(quoted, partial_path, path_start_byte)` where:
/// - `quoted` — the path was opened with a `"` (add closing `"` to completions)
/// - `partial_path` — the partial path typed so far
/// - `path_start_byte` — byte offset in `line` where the path starts (for rustyline replacement)
fn parse_load_prefix(line: &str) -> Option<(bool, &str, usize)> {
    for prefix in &[":load ", ":l ", ":reload ", ":r "] {
        if let Some(rest) = line.strip_prefix(prefix) {
            let cmd_end = prefix.len();
            if let Some(stripped) = rest.strip_prefix('"') {
                // Quoted: path starts after the opening quote
                return Some((true, stripped, cmd_end + 1));
            }
            return Some((false, rest, cmd_end));
        }
    }
    None
}

/// Build file-path completions for a partial path.
///
/// Lists `.bt` files and directories under the directory portion of `partial`,
/// filtering entries to those whose names start with the file-name prefix.
/// Directories always appear (with trailing `/`); only `.bt` files are shown.
/// When `quoted` is true, completed file paths get a closing `"` appended.
fn complete_path(partial: &str, quoted: bool) -> Vec<Pair> {
    // Split partial into directory prefix and file-name prefix
    let (dir, file_prefix) = match partial.rfind('/') {
        Some(slash) => (&partial[..=slash], &partial[slash + 1..]),
        None => ("", partial),
    };

    let search_dir = if dir.is_empty() { "." } else { dir };

    let Ok(entries) = std::fs::read_dir(search_dir) else {
        return Vec::new();
    };

    let mut completions: Vec<Pair> = entries
        .flatten()
        .filter_map(|entry| {
            let name = entry.file_name();
            let name_str = name.to_string_lossy().into_owned();

            // Skip hidden files unless the user explicitly typed a dot prefix
            if name_str.starts_with('.') && !file_prefix.starts_with('.') {
                return None;
            }

            if !name_str.starts_with(file_prefix) {
                return None;
            }

            let file_type = entry.file_type().ok()?;

            if file_type.is_dir() {
                let full = format!("{dir}{name_str}/");
                Some(Pair {
                    display: format!("{name_str}/"),
                    replacement: full,
                })
            } else if file_type.is_file()
                && Path::new(&name_str).extension().is_some_and(|e| e == "bt")
            {
                let full = if quoted {
                    format!("{dir}{name_str}\"")
                } else {
                    format!("{dir}{name_str}")
                };
                Some(Pair {
                    display: name_str,
                    replacement: full,
                })
            } else {
                None
            }
        })
        .collect();

    completions.sort_by(|a, b| a.display.cmp(&b.display));
    completions
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

        // File path completion for :load/:l/:reload/:r commands
        if let Some((quoted, partial, path_start)) = parse_load_prefix(line_to_pos) {
            let candidates = complete_path(partial, quoted);
            return Ok((path_start, candidates));
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
    use std::fs;
    use tempfile::TempDir;

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

    // === parse_load_prefix tests ===

    #[test]
    fn parse_load_prefix_load_unquoted() {
        let result = parse_load_prefix(":load examples/");
        assert_eq!(result, Some((false, "examples/", 6)));
    }

    #[test]
    fn parse_load_prefix_load_quoted() {
        let result = parse_load_prefix(":load \"examples/");
        assert_eq!(result, Some((true, "examples/", 7)));
    }

    #[test]
    fn parse_load_prefix_l_shorthand() {
        let result = parse_load_prefix(":l foo");
        assert_eq!(result, Some((false, "foo", 3)));
    }

    #[test]
    fn parse_load_prefix_reload_quoted() {
        let result = parse_load_prefix(":reload \"stdlib/");
        assert_eq!(result, Some((true, "stdlib/", 9)));
    }

    #[test]
    fn parse_load_prefix_r_shorthand_quoted() {
        let result = parse_load_prefix(":r \"foo");
        assert_eq!(result, Some((true, "foo", 4)));
    }

    #[test]
    fn parse_load_prefix_non_load_command_is_none() {
        assert_eq!(parse_load_prefix(":help"), None);
        assert_eq!(parse_load_prefix(":bindings"), None);
        assert_eq!(parse_load_prefix("Counter spawn"), None);
    }

    #[test]
    fn parse_load_prefix_command_only_no_space_is_none() {
        // ":load" without trailing space is command completion, not path
        assert_eq!(parse_load_prefix(":load"), None);
    }

    #[test]
    fn parse_load_prefix_empty_path_after_space() {
        let result = parse_load_prefix(":load ");
        assert_eq!(result, Some((false, "", 6)));
    }

    #[test]
    fn parse_load_prefix_empty_quoted_path() {
        let result = parse_load_prefix(":load \"");
        assert_eq!(result, Some((true, "", 7)));
    }

    // === complete_path tests ===

    fn make_test_dir() -> TempDir {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("counter.bt"), "").unwrap();
        fs::write(dir.path().join("fibonacci.bt"), "").unwrap();
        fs::write(dir.path().join("main.rs"), "").unwrap(); // should be excluded
        fs::create_dir(dir.path().join("examples")).unwrap();
        dir
    }

    #[test]
    fn complete_path_lists_bt_files_and_dirs() {
        let dir = make_test_dir();
        let path_str = format!("{}/", dir.path().to_string_lossy());
        let candidates = complete_path(&path_str, false);
        let names: Vec<&str> = candidates.iter().map(|p| p.display.as_str()).collect();
        assert!(
            names.contains(&"counter.bt"),
            "expected counter.bt in {names:?}"
        );
        assert!(names.contains(&"fibonacci.bt"));
        assert!(names.contains(&"examples/"));
        assert!(!names.contains(&"main.rs"), "main.rs should not appear");
    }

    #[test]
    fn complete_path_filters_by_prefix() {
        let dir = make_test_dir();
        let path_str = format!("{}/co", dir.path().to_string_lossy());
        let candidates = complete_path(&path_str, false);
        let names: Vec<&str> = candidates.iter().map(|p| p.display.as_str()).collect();
        assert!(names.contains(&"counter.bt"));
        assert!(!names.contains(&"fibonacci.bt"));
    }

    #[test]
    fn complete_path_quoted_appends_closing_quote() {
        let dir = make_test_dir();
        let path_str = format!("{}/counter.bt", dir.path().to_string_lossy());
        let candidates = complete_path(&path_str, true);
        // Should find counter.bt and its replacement should end with `"`
        let counter = candidates.iter().find(|p| p.display == "counter.bt");
        assert!(counter.is_some(), "expected counter.bt");
        assert!(counter.unwrap().replacement.ends_with('"'));
    }

    #[test]
    fn complete_path_unquoted_no_closing_quote() {
        let dir = make_test_dir();
        let path_str = format!("{}/counter.bt", dir.path().to_string_lossy());
        let candidates = complete_path(&path_str, false);
        let counter = candidates.iter().find(|p| p.display == "counter.bt");
        assert!(counter.is_some());
        assert!(!counter.unwrap().replacement.ends_with('"'));
    }

    #[test]
    fn complete_path_dir_has_trailing_slash() {
        let dir = make_test_dir();
        let path_str = format!("{}/ex", dir.path().to_string_lossy());
        let candidates = complete_path(&path_str, false);
        let example = candidates.iter().find(|p| p.display == "examples/");
        assert!(example.is_some());
        assert!(example.unwrap().replacement.ends_with('/'));
    }

    #[test]
    fn complete_path_dir_quoted_no_closing_quote() {
        let dir = make_test_dir();
        let path_str = format!("{}/ex", dir.path().to_string_lossy());
        let candidates = complete_path(&path_str, true);
        let example = candidates.iter().find(|p| p.display == "examples/");
        assert!(example.is_some());
        assert!(example.unwrap().replacement.ends_with('/'));
        assert!(!example.unwrap().replacement.ends_with('"'));
    }

    #[test]
    fn complete_path_nonexistent_dir_returns_empty() {
        let candidates = complete_path("/this/path/does/not/exist/", false);
        assert!(candidates.is_empty());
    }

    #[test]
    fn complete_path_sorted_alphabetically() {
        let dir = make_test_dir();
        let path_str = format!("{}/", dir.path().to_string_lossy());
        let candidates = complete_path(&path_str, false);
        let names: Vec<&str> = candidates.iter().map(|p| p.display.as_str()).collect();
        let mut sorted = names.clone();
        sorted.sort_unstable();
        assert_eq!(names, sorted, "completions should be sorted alphabetically");
    }
}
