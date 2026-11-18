// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL display formatting, help text, and history.
//!
//! **DDD Context:** REPL — Presentation

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

use beamtalk_repl_protocol::format::{self as fmt, Diagnostic as FmtDiagnostic, OutputMode};
use miette::{IntoDiagnostic, Result};

use crate::paths::beamtalk_dir;

use super::color;
use super::commands::REPL_COMMAND_TABLE;

/// Selects [`OutputMode::Ansi`] when colour is enabled, otherwise plain.
pub(crate) fn output_mode() -> OutputMode {
    if color::is_enabled() {
        OutputMode::Ansi
    } else {
        OutputMode::Plain
    }
}

/// Return the path to the REPL history file, creating the parent directory if needed.
pub(crate) fn history_path() -> Result<PathBuf> {
    let dir = beamtalk_dir()?;
    fs::create_dir_all(&dir).into_diagnostic()?;
    Ok(dir.join("repl_history"))
}

/// Format a value for REPL display with optional coloring.
///
/// Delegates to the shared [`beamtalk_repl_protocol::format::format_value`]
/// helper so CLI / MCP / future surfaces share a single rendering
/// for REPL eval results.
pub(crate) fn format_value(value: &serde_json::Value) -> String {
    fmt::format_value(value, output_mode())
}

/// Format an eval result's value, preferring the server's `value_type` tag
/// (BT-4912) over inspecting the rendered text.
pub(crate) fn format_value_typed(value: &serde_json::Value, value_type: Option<&str>) -> String {
    fmt::format_value_typed(value, value_type, output_mode())
}

/// Format an error message for REPL display.
///
/// Delegates to the shared [`beamtalk_repl_protocol::format::format_diagnostic`]
/// helper.
pub(crate) fn format_error(msg: &str) -> String {
    fmt::format_diagnostic(&FmtDiagnostic::new(msg), output_mode())
}

/// Render the `:help` command list from [`REPL_COMMAND_TABLE`], so a command or
/// alias added to the table can never be missing from the help output.
pub(crate) fn render_command_help() -> String {
    use std::fmt::Write as _;
    let rows: Vec<(String, &str)> = REPL_COMMAND_TABLE
        .iter()
        .map(|c| {
            let forms: Vec<&str> = std::iter::once(c.name)
                .chain(c.aliases.iter().copied())
                .collect();
            (forms.join(", "), c.help)
        })
        .collect();
    let width = rows.iter().map(|(f, _)| f.len()).max().unwrap_or(0);
    let mut out = String::new();
    for (forms, help) in rows {
        let _ = writeln!(out, "  {forms:<width$}  {help}");
    }
    out
}

/// Print help message.
pub(crate) fn print_help() {
    println!("Beamtalk REPL Commands:");
    println!();
    print!("{}", render_command_help());
    println!();
    println!("Documentation lookups (`:help`):");
    println!("  :help <Class>              Show instance-side class docs and methods");
    println!("  :help <Class> <sel>        Show instance-side method documentation");
    println!("  :help <Class> class        Show class-side docs (name, allMethods, reload, ...)");
    println!("  :help <Class> class <sel>  Show class-side method documentation");
    println!("  :help Erlang <module>      Show Erlang module docs and function signatures");
    println!("  :help Erlang <mod> <fn>    Show Erlang function docs and type signature");
    println!();
    println!("Expression examples:");
    println!("  x := 42              # Variable assignment");
    println!("  x + 10               # Arithmetic");
    println!("  Counter spawn        # Spawn an actor");
    println!("  counter increment    # Send a message (auto-awaits result)");
    println!("  counter getValue     # Query actor state (auto-awaits)");
    println!();
    println!("Type information:");
    println!("  :help Result         # Shows class docs and method signatures");
    println!("  :help Result unwrap  # Shows method documentation and return type");
    println!("  Type annotations use :: (e.g., amount :: Integer -> Integer)");
    println!("  Generic types use parentheses: Result(T, E), Array(E)");
    println!();
    println!("Multi-line input:");
    println!("  Expressions with unclosed brackets, strings, trailing operators,");
    println!("  keywords (e.g. at:), or := continue on the next line (..> prompt).");
    println!("  Press Ctrl+C to cancel multi-line input.");
    println!();
    println!("Node introspection:");
    println!("  Node current actors            # List all live actors");
    println!("  Node current actorAt: '<pid>' # Look up actor by PID");
    println!("  Node current actorsOf: Counter # All actors of a class");
    println!();
    println!("Actor message sends return Futures, which are automatically");
    println!("awaited for a synchronous REPL experience.");
}

/// Display generated Core Erlang source.
pub(crate) fn display_codegen(core_erlang: &str) {
    // Try to pretty-print using external `core_pp` if available; fall back to raw output.
    if let Ok(mut child) = Command::new("core_pp")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
    {
        if let Some(mut stdin) = child.stdin.take() {
            let _ = stdin.write_all(core_erlang.as_bytes());
        }

        // Capture stdout in a separate thread so the main thread can enforce a timeout.
        if let Some(mut stdout) = child.stdout.take() {
            let (tx, rx) = std::sync::mpsc::channel();
            std::thread::spawn(move || {
                let mut buf = Vec::new();
                let _ = std::io::Read::read_to_end(&mut stdout, &mut buf);
                let _ = tx.send(buf);
            });

            let timeout = std::time::Duration::from_secs(2);
            let start = std::time::Instant::now();
            let mut exited = false;
            loop {
                match child.try_wait() {
                    Ok(Some(_status)) => {
                        exited = true;
                        break;
                    }
                    Ok(None) => {
                        if start.elapsed() >= timeout {
                            let _ = child.kill();
                            let _ = child.wait();
                            break;
                        }
                        std::thread::sleep(std::time::Duration::from_millis(50));
                    }
                    Err(_) => {
                        let _ = child.kill();
                        let _ = child.wait();
                        break;
                    }
                }
            }

            // Try to receive any captured output (small timeout).
            // Only use formatted output when receive succeeds and bytes are non-empty,
            // to avoid printing a blank line on timeout.
            if exited
                && let Ok(output_bytes) = rx.recv_timeout(std::time::Duration::from_secs(1))
                && !output_bytes.is_empty()
                && let Ok(formatted) = String::from_utf8(output_bytes)
            {
                println!("{}", color::paint(color::CYAN, &formatted));
                return;
            }
        }
    }
    // Fallback: print unmodified Core Erlang
    println!("{}", color::paint(color::CYAN, core_erlang));
}

#[cfg(test)]
mod tests {
    use super::super::color::ColorGuard;
    use super::*;
    use serial_test::serial;

    fn strip_ansi(s: &str) -> String {
        // Remove ESC[...m sequences for comparison
        let mut out = String::new();
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\x1b' {
                // Skip until 'm'
                for ch in chars.by_ref() {
                    if ch == 'm' {
                        break;
                    }
                }
            } else {
                out.push(c);
            }
        }
        out
    }

    #[test]
    fn list_displays_with_beamtalk_syntax() {
        let val = serde_json::json!([1, 2, 3]);
        let rendered = strip_ansi(&format_value(&val));
        assert_eq!(rendered, "#(1, 2, 3)");
    }

    #[test]
    fn empty_list_displays_with_beamtalk_syntax() {
        let val = serde_json::json!([]);
        let rendered = strip_ansi(&format_value(&val));
        assert_eq!(rendered, "#()");
    }

    #[test]
    fn nested_list_displays_with_beamtalk_syntax() {
        let val = serde_json::json!([[1, 2], [3, 4]]);
        let rendered = strip_ansi(&format_value(&val));
        assert_eq!(rendered, "#(#(1, 2), #(3, 4))");
    }

    #[test]
    fn float_string_displays_as_number() {
        // Floats come as strings from the backend to preserve ".0"
        let val = serde_json::json!("6.0");
        let rendered = strip_ansi(&format_value(&val));
        assert_eq!(rendered, "6.0");

        let val = serde_json::json!("3.14");
        let rendered = strip_ansi(&format_value(&val));
        assert_eq!(rendered, "3.14");
    }

    #[test]
    fn regular_string_not_mistaken_for_float() {
        let val = serde_json::json!("hello");
        let rendered = strip_ansi(&format_value(&val));
        assert_eq!(rendered, "hello");
    }

    // --- format_error ---

    #[test]
    fn format_error_prefixes_message_with_error_label() {
        let rendered = strip_ansi(&format_error("connection refused"));
        assert_eq!(rendered, "Error: connection refused");
    }

    #[test]
    fn format_error_empty_message_keeps_prefix() {
        let rendered = strip_ansi(&format_error(""));
        assert_eq!(rendered, "Error: ");
    }

    #[test]
    fn format_error_message_containing_colon() {
        let rendered = strip_ansi(&format_error("undefined variable: x"));
        assert_eq!(rendered, "Error: undefined variable: x");
    }

    // --- history_path ---

    #[test]
    fn history_path_ends_in_repl_history_file() {
        let path = history_path().expect("history_path should succeed");
        assert_eq!(
            path.file_name().and_then(|n| n.to_str()),
            Some("repl_history")
        );
    }

    // --- print_help / display_codegen smoke tests ---
    //
    // These just print to stdout; there's no return value to assert on, so
    // the tests exist to exercise the lines (and confirm no panic) rather
    // than to check specific formatted output.

    #[test]
    fn print_help_does_not_panic() {
        print_help();
    }

    #[test]
    fn display_codegen_falls_back_to_raw_output_when_core_pp_is_absent() {
        // `core_pp` is not installed in the test environment, so this always
        // exercises the "print unmodified Core Erlang" fallback path.
        display_codegen("module test.\n  'foo'/0 = fun () -> ok\nend");
    }

    // --- output_mode ---

    #[test]
    #[serial(color)]
    fn output_mode_is_ansi_when_color_enabled() {
        let _guard = ColorGuard::enabled();
        assert_eq!(output_mode(), OutputMode::Ansi);
    }

    #[test]
    #[serial(color)]
    fn output_mode_is_plain_when_color_disabled() {
        let _guard = ColorGuard::disabled();
        assert_eq!(output_mode(), OutputMode::Plain);
    }
}
