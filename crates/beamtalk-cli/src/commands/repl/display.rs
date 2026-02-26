// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL display formatting, help text, and history.
//!
//! **DDD Context:** REPL — Presentation

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

use miette::{IntoDiagnostic, Result};

use crate::paths::beamtalk_dir;

use super::color;

/// Return the path to the REPL history file, creating the parent directory if needed.
pub(crate) fn history_path() -> Result<PathBuf> {
    let dir = beamtalk_dir()?;
    fs::create_dir_all(&dir).into_diagnostic()?;
    Ok(dir.join("repl_history"))
}

/// Format a value for REPL display with optional coloring.
pub(crate) fn format_value(value: &serde_json::Value) -> String {
    match value {
        serde_json::Value::String(s) => {
            // Values are pre-formatted by the backend:
            // - Actors: "#Actor<0.123.0>" or "#ClassName<0.123.0>"
            // - Blocks: "a Block/N"
            // Just return as-is with cyan coloring for actor/block references
            if (s.starts_with('#') && s.contains('<')) || s.starts_with("a Block") {
                color::paint(color::CYAN, s)
            } else {
                color::paint(color::GREEN, s)
            }
        }
        serde_json::Value::Number(n) => color::paint(color::YELLOW, &n.to_string()),
        serde_json::Value::Bool(b) => color::paint(color::BOLD_BLUE, &b.to_string()),
        serde_json::Value::Null => color::paint(color::BOLD_BLUE, "nil"),
        serde_json::Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(format_value).collect();
            format!("[{}]", items.join(", "))
        }
        serde_json::Value::Object(obj) => {
            // Regular object
            let pairs: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!("{k}: {}", format_value(v)))
                .collect();
            format!("{{{}}}", pairs.join(", "))
        }
    }
}

/// Format an error message for REPL display.
pub(crate) fn format_error(msg: &str) -> String {
    if color::is_enabled() {
        format!("{}{}Error:{} {msg}", color::BOLD, color::RED, color::RESET)
    } else {
        format!("Error: {msg}")
    }
}

/// Print help message.
pub(crate) fn print_help() {
    println!("Beamtalk REPL Commands:");
    println!();
    println!("  :help, :h       Show this help message");
    println!("  :help <Class>   Show class documentation and methods");
    println!("  :help <C> <sel> Show method documentation");
    println!("  :exit, :q       Exit the REPL");
    println!("  :clear          Clear all variable bindings");
    println!("  :bindings       Show current variable bindings");
    println!("  :load <path>    Load a .bt file or directory (→ Workspace load: \"path\")");
    println!("  :reload         Reload the last loaded file or directory");
    println!("  :reload <Class> Reload a class by name (→ ClassName reload)");
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
    println!("awaited for synchronous REPL experience. If you store a Future");
    println!("in a binding before it resolves, you'll see #Future<pending>.");
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
