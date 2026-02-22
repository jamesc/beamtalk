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
    println!("  :load <path>    Load a .bt file or directory (recursive; paths may be quoted)");
    println!("  :reload         Reload the last loaded file or directory (supports quoted paths)");
    println!("  :reload <name>  Reload a module by name");
    println!("  :modules        List loaded modules");
    println!("  :unload <name>  Unload a module (fails if actors exist)");
    println!("  :actors         List running actors");
    println!("  :kill <pid>     Kill an actor by PID");
    println!("  :inspect <pid>  Inspect an actor's state");
    println!("  :sessions       List active REPL sessions");
    println!("  :test, :t       Run all tests for loaded TestCase classes");
    println!("  :test <Class>   Run tests for a specific TestCase class");
    println!("  :info, :i <sym> Get information about a symbol");
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

/// Display test results from the REPL backend (BT-724).
pub(crate) fn display_test_results(results: &serde_json::Value) {
    let total = results
        .get("total")
        .and_then(serde_json::Value::as_u64)
        .unwrap_or(0);
    let passed = results
        .get("passed")
        .and_then(serde_json::Value::as_u64)
        .unwrap_or(0);
    let failed = results
        .get("failed")
        .and_then(serde_json::Value::as_u64)
        .unwrap_or(0);
    let class = results
        .get("class")
        .and_then(serde_json::Value::as_str)
        .unwrap_or("Tests");

    if let Some(tests) = results.get("tests").and_then(serde_json::Value::as_array) {
        for test in tests {
            let name = test
                .get("name")
                .and_then(serde_json::Value::as_str)
                .unwrap_or("?");
            let status = test
                .get("status")
                .and_then(serde_json::Value::as_str)
                .unwrap_or("?");
            // Include class name for test-all results
            let test_class = test.get("class").and_then(serde_json::Value::as_str);
            let display_name = if let Some(tc) = test_class {
                format!("{tc} >> {name}")
            } else {
                name.to_string()
            };

            match status {
                "pass" => {
                    println!("  {} {display_name}", color::paint(color::GREEN, "✓"));
                }
                "fail" => {
                    let error = test
                        .get("error")
                        .and_then(serde_json::Value::as_str)
                        .unwrap_or("");
                    println!("  {} {display_name}", color::paint(color::RED, "✗"));
                    if !error.is_empty() {
                        println!("    {}", color::paint(color::DIM, &format!("→ {error}")));
                    }
                }
                other => {
                    println!("  ? {display_name} ({other})");
                }
            }
        }
    }

    // Summary line
    println!();
    if failed == 0 {
        println!(
            "{class}: {} ({total} tests)",
            color::paint(color::GREEN, "All tests passed")
        );
    } else {
        println!(
            "{class}: {}, {}",
            color::paint(color::GREEN, &format!("{passed} passed")),
            color::paint(color::RED, &format!("{failed} failed"))
        );
    }
}

/// Display generated Core Erlang source (BT-724).
pub(crate) fn display_codegen(core_erlang: &str) {
    // Use cyan for Core Erlang source to distinguish from regular output
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
