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
/// helper (BT-2086) so CLI / MCP / future surfaces share a single rendering
/// for REPL eval results.
pub(crate) fn format_value(value: &serde_json::Value) -> String {
    fmt::format_value(value, output_mode())
}

/// Format an error message for REPL display.
///
/// Delegates to the shared [`beamtalk_repl_protocol::format::format_diagnostic`]
/// helper (BT-2086).
pub(crate) fn format_error(msg: &str) -> String {
    fmt::format_diagnostic(&FmtDiagnostic::new(msg), output_mode())
}

/// Print help message.
pub(crate) fn print_help() {
    println!("Beamtalk REPL Commands:");
    println!();
    println!("  :help, :h               Show this help message");
    println!("  :help <Class>           Show instance-side class docs and methods");
    println!("  :help <Class> <sel>     Show instance-side method documentation");
    println!("  :help <Class> class     Show class-side docs (name, allMethods, reload, ...)");
    println!("  :help <Class> class <sel>  Show class-side method documentation");
    println!("  :help Erlang <module>   Show Erlang module docs and function signatures");
    println!("  :help Erlang <mod> <fn> Show Erlang function docs and type signature");
    println!("  :exit, :q       Exit the REPL");
    println!("  :clear          Clear all variable bindings");
    println!("  :bindings       Show current variable bindings");
    println!(
        "  :sync, :s       Sync workspace with project in current dir (requires beamtalk.toml)"
    );
    println!("  :unload <Class> Unload a class from the workspace");
    println!("  :test           Run all test classes (→ Workspace test)");
    println!("  :test <Class>   Run a test class (→ Workspace test: ClassName)");
    println!("  :show-codegen <expr>  Show generated Core Erlang for an expression");
    println!("  :sc <expr>      Short alias for :show-codegen");
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
    println!("Workspace introspection:");
    println!("  Workspace actors           # List all live actors");
    println!("  Workspace actorAt: '<pid>' # Look up actor by PID");
    println!("  Workspace actorsOf: Counter # All actors of a class");
    println!();
    println!("Actor message sends return Futures, which are automatically");
    println!("awaited for a synchronous REPL experience.");
}

/// Display generated Core Erlang source (BT-724).
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
            if exited {
                if let Ok(output_bytes) = rx.recv_timeout(std::time::Duration::from_secs(1)) {
                    if !output_bytes.is_empty() {
                        if let Ok(formatted) = String::from_utf8(output_bytes) {
                            println!("{}", color::paint(color::CYAN, &formatted));
                            return;
                        }
                    }
                }
            }
        }
    }
    // Fallback: print unmodified Core Erlang
    println!("{}", color::paint(color::CYAN, core_erlang));
}

/// Display symbol info from the :info command (BT-724).
#[allow(dead_code)]
pub(crate) fn display_info(info: &serde_json::Value) {
    let found = info
        .get("found")
        .and_then(serde_json::Value::as_bool)
        .unwrap_or(false);
    let symbol = info
        .get("symbol")
        .and_then(serde_json::Value::as_str)
        .unwrap_or("?");

    if !found {
        println!("Symbol not found: {symbol}");
        return;
    }

    let kind = info
        .get("kind")
        .and_then(serde_json::Value::as_str)
        .unwrap_or("unknown");

    println!(
        "{} {}",
        color::paint(color::BOLD_CYAN, symbol),
        color::paint(color::DIM, &format!("({kind})"))
    );

    // Display additional info fields if present
    if let Some(obj) = info.as_object() {
        for (key, value) in obj {
            // Skip already-displayed fields
            if matches!(key.as_str(), "found" | "symbol" | "kind") {
                continue;
            }
            if let Some(s) = value.as_str() {
                if !s.is_empty() {
                    println!("  {}: {s}", color::paint(color::DIM, key));
                }
            } else if let Some(arr) = value.as_array() {
                if !arr.is_empty() {
                    println!("  {}:", color::paint(color::DIM, key));
                    for item in arr {
                        if let Some(s) = item.as_str() {
                            println!("    {s}");
                        } else {
                            println!("    {item}");
                        }
                    }
                }
            } else {
                println!(
                    "  {}: {}",
                    color::paint(color::DIM, key),
                    format_value(value)
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        // BT-1336: Floats come as strings from the backend to preserve ".0"
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
}
