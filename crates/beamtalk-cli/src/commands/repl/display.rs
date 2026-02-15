// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL display formatting, help text, and history.
//!
//! **DDD Context:** REPL — Presentation

use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result};

use crate::paths::beamtalk_dir;

use super::color;

pub(super) fn history_path() -> Result<PathBuf> {
    let dir = beamtalk_dir()?;
    fs::create_dir_all(&dir).into_diagnostic()?;
    Ok(dir.join("repl_history"))
}

/// Format a value for REPL display with optional coloring.
pub(super) fn format_value(value: &serde_json::Value) -> String {
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
pub(super) fn format_error(msg: &str) -> String {
    if color::is_enabled() {
        format!("{}{}Error:{} {msg}", color::BOLD, color::RED, color::RESET)
    } else {
        format!("Error: {msg}")
    }
}

/// Print help message.
pub(super) fn print_help() {
    println!("Beamtalk REPL Commands:");
    println!();
    println!("  :help, :h       Show this help message");
    println!("  :help <Class>   Show class documentation and methods");
    println!("  :help <C> <sel> Show method documentation");
    println!("  :exit, :q       Exit the REPL");
    println!("  :clear          Clear all variable bindings");
    println!("  :bindings       Show current variable bindings");
    println!("  :load <path>    Load a .bt file");
    println!("  :reload         Reload the last loaded file");
    println!("  :reload <name>  Reload a module by name");
    println!("  :modules        List loaded modules");
    println!("  :unload <name>  Unload a module (fails if actors exist)");
    println!("  :actors         List running actors");
    println!("  :kill <pid>     Kill an actor by PID");
    println!("  :inspect <pid>  Inspect an actor's state");
    println!("  :sessions       List active REPL sessions");
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
