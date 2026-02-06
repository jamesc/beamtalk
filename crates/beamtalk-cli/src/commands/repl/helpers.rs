// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL utility helpers for formatting, history, and runtime discovery.
//!
//! **DDD Context:** REPL — Utilities

use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};
use tracing::info;

use crate::paths::{beamtalk_dir, is_daemon_running};

pub(super) fn history_path() -> Result<PathBuf> {
    let dir = beamtalk_dir()?;
    fs::create_dir_all(&dir).into_diagnostic()?;
    Ok(dir.join("repl_history"))
}

/// Find the runtime directory by checking multiple possible locations.
pub(super) fn find_runtime_dir() -> Result<PathBuf> {
    // Check explicit env var first
    if let Ok(dir) = std::env::var("BEAMTALK_RUNTIME_DIR") {
        let path = PathBuf::from(dir);
        if path.join("rebar.config").exists() {
            return Ok(path);
        }
        return Err(miette!(
            "BEAMTALK_RUNTIME_DIR is set but does not contain a valid runtime (no rebar.config)"
        ));
    }

    // Candidates in order of preference
    let candidates = [
        // 1. CARGO_MANIFEST_DIR (when running via cargo run)
        std::env::var("CARGO_MANIFEST_DIR")
            .ok()
            .map(|d| PathBuf::from(d).join("../../runtime")),
        // 2. Current working directory (running from repo root)
        Some(PathBuf::from("runtime")),
        // 3. Relative to executable (installed location)
        std::env::current_exe()
            .ok()
            .and_then(|exe| exe.parent().map(|p| p.join("../lib/beamtalk/runtime"))),
        // 4. Executable's grandparent (target/debug/beamtalk -> repo root)
        std::env::current_exe().ok().and_then(|exe| {
            exe.parent()
                .and_then(|p| p.parent())
                .and_then(|p| p.parent())
                .map(|p| p.join("runtime"))
        }),
    ];

    for candidate in candidates.into_iter().flatten() {
        if candidate.join("rebar.config").exists() {
            return Ok(candidate);
        }
    }

    Err(miette!(
        "Could not find Beamtalk runtime directory.\n\
        Please run from the repository root or set BEAMTALK_RUNTIME_DIR."
    ))
}

/// Start the compiler daemon in the background.
pub(super) fn start_daemon() -> Result<()> {
    info!("Starting compiler daemon...");

    // Get path to beamtalk binary (ourselves)
    let exe = std::env::current_exe().into_diagnostic()?;

    // Spawn daemon in foreground mode as a background process
    // (background mode in daemon itself is not implemented)
    Command::new(exe)
        .args(["daemon", "start", "--foreground"])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .into_diagnostic()?;

    // Wait a moment for daemon to start
    std::thread::sleep(Duration::from_millis(1000));

    if is_daemon_running()?.is_none() {
        return Err(miette!(
            "Failed to start compiler daemon. Try: beamtalk daemon start --foreground"
        ));
    }

    info!("Compiler daemon started.");
    Ok(())
}

/// Format a value for REPL display.
pub(super) fn format_value(value: &serde_json::Value) -> String {
    match value {
        serde_json::Value::String(s) => {
            // Values are pre-formatted by the backend:
            // - Actors: "#Actor<0.123.0>" or "#ClassName<0.123.0>"
            // - Blocks: "a Block/N"
            // Just return as-is
            s.clone()
        }
        serde_json::Value::Number(n) => n.to_string(),
        serde_json::Value::Bool(b) => b.to_string(),
        serde_json::Value::Null => "nil".to_string(),
        serde_json::Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(format_value).collect();
            format!("[{}]", items.join(", "))
        }
        serde_json::Value::Object(obj) => {
            // Check for tuple marker
            if let Some(serde_json::Value::Array(items)) = obj.get("__tuple__") {
                let formatted: Vec<String> = items.iter().map(format_value).collect();
                return format!("({})", formatted.join(", "));
            }
            // Regular object
            let pairs: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!("{k}: {}", format_value(v)))
                .collect();
            format!("{{{}}}", pairs.join(", "))
        }
    }
}

/// Print help message.
pub(super) fn print_help() {
    println!("Beamtalk REPL Commands:");
    println!();
    println!("  :help, :h       Show this help message");
    println!("  :exit, :q       Exit the REPL");
    println!("  :clear          Clear all variable bindings");
    println!("  :bindings       Show current variable bindings");
    println!("  :load <path>    Load a .bt file");
    println!("  :reload         Reload the last loaded file");
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
    println!("Actor message sends return Futures, which are automatically");
    println!("awaited for synchronous REPL experience. If you store a Future");
    println!("in a binding before it resolves, you'll see #Future<pending>.");
}
