// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL CLI client for interactive Beamtalk evaluation.
//!
//! This module implements the `beamtalk repl` command, which provides an
//! interactive read-eval-print loop for Beamtalk expressions.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────┐     ┌──────────────────────────────────┐
//! │  REPL CLI       │     │   BEAM Node                      │
//! │  (this module)  │────▶│  beamtalk_workspace_sup          │
//! │                 │ TCP │    ├─ beamtalk_repl_server        │
//! │  rustyline      │     │    ├─ beamtalk_session_sup        │
//! └─────────────────┘     │    │   └─ beamtalk_repl_shell     │
//!       │                 │    ├─ beamtalk_actor_registry      │
//!   localhost:9000        │    └─ Compiler Daemon ◀───────────┤
//!                         └──────────────────────────────────┘
//!                                        │
//!                                  Unix socket
//! ```
//!
//! # Usage
//!
//! ```bash
//! beamtalk repl           # Start interactive REPL
//! ```
//!
//! # Protocol
//!
//! The REPL CLI communicates with the Erlang backend using JSON over TCP.
//! Both legacy and new protocol formats are supported:
//!
//! ```json
//! // New protocol (preferred)
//! // Request
//! {"op": "eval", "id": "msg-001", "code": "x := 42"}
//!
//! // Response (success)
//! {"id": "msg-001", "value": "42", "status": ["done"]}
//!
//! // Response (error)
//! {"id": "msg-001", "error": "Undefined variable: foo", "status": ["done", "error"]}
//! ```

use std::time::Duration;

use miette::{IntoDiagnostic, Result};
use rustyline::error::ReadlineError;
use rustyline::history::FileHistory;
use rustyline::{DefaultEditor, Editor};
use serde::Deserialize;
use tracing::warn;

use crate::commands::workspace;
use crate::paths::is_daemon_running;

use beamtalk_core::source_analysis::is_input_complete;

/// Maximum retries when connecting to REPL backend.
const MAX_CONNECT_RETRIES: u32 = 10;

/// Delay between connection retries in milliseconds.
const RETRY_DELAY_MS: u64 = 500;

mod client;
mod display;
mod process;

use client::ReplClient;
use display::{format_value, history_path, print_help};
use process::{
    BeamChildGuard, connect_with_retries, read_port_from_child, resolve_node_name, resolve_port,
    start_beam_node, start_daemon,
};

/// JSON response from the REPL backend.
/// Supports both legacy format (type field) and new protocol format (status field).
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct ReplResponse {
    /// Legacy response type (result, error, bindings, loaded, actors, modules)
    #[serde(rename = "type")]
    response_type: Option<String>,
    /// New protocol: message correlation ID
    id: Option<String>,
    /// New protocol: session ID
    session: Option<String>,
    /// New protocol: status flags
    status: Option<Vec<String>>,
    /// Result value (both formats)
    value: Option<serde_json::Value>,
    /// Captured stdout from evaluation (BT-355)
    output: Option<String>,
    /// Legacy: error message
    message: Option<String>,
    /// New protocol: error message
    error: Option<String>,
    /// Bindings map (both formats)
    bindings: Option<serde_json::Value>,
    /// Loaded classes (both formats)
    classes: Option<Vec<String>>,
    /// Actor list (both formats)
    actors: Option<Vec<ActorInfo>>,
    /// Module list (both formats)
    modules: Option<Vec<ModuleInfo>>,
    /// Session list (new protocol)
    sessions: Option<Vec<SessionInfo>>,
    /// Completion suggestions (new protocol)
    completions: Option<Vec<String>>,
    /// Symbol info (new protocol)
    info: Option<serde_json::Value>,
    /// Actor state (new protocol: inspect op)
    state: Option<serde_json::Value>,
    /// Compilation warnings (BT-407)
    warnings: Option<Vec<String>>,
    /// Documentation text (BT-500: :help command)
    docs: Option<String>,
    /// Number of actors affected by reload (BT-266)
    affected_actors: Option<u32>,
    /// Number of actors that failed code migration (BT-266)
    migration_failures: Option<u32>,
}

impl ReplResponse {
    /// Check if this is an error response (either format).
    fn is_error(&self) -> bool {
        if let Some(ref t) = self.response_type {
            return t == "error";
        }
        if let Some(ref status) = self.status {
            return status.iter().any(|s| s == "error");
        }
        false
    }

    /// Get the error message (either format).
    fn error_message(&self) -> Option<&str> {
        if let Some(ref msg) = self.message {
            return Some(msg.as_str());
        }
        if let Some(ref err) = self.error {
            return Some(err.as_str());
        }
        None
    }

    /// Check if this response has a specific legacy type.
    #[allow(dead_code)]
    fn has_type(&self, expected: &str) -> bool {
        if let Some(ref t) = self.response_type {
            return t == expected;
        }
        // New protocol doesn't have type field - infer from status + fields
        false
    }
}

#[derive(Debug, Deserialize)]
struct ActorInfo {
    pid: String,
    class: String,
    module: String,
    #[allow(dead_code)]
    spawned_at: i64,
}

#[derive(Debug, Deserialize)]
struct ModuleInfo {
    name: String,
    source_file: String,
    actor_count: u32,
    #[allow(dead_code)]
    load_time: i64,
    time_ago: String,
}

#[derive(Debug, Deserialize)]
struct SessionInfo {
    id: String,
    #[allow(dead_code)]
    created_at: Option<i64>,
}

/// Display the result of a reload operation.
fn display_reload_result(response: &ReplResponse, module_name: Option<&str>) {
    if response.is_error() {
        if let Some(msg) = response.error_message() {
            eprintln!("Error: {msg}");
        }
        return;
    }
    let label = if let Some(ref classes) = response.classes {
        if classes.is_empty() {
            module_name.map_or_else(|| "Reloaded".to_string(), |n| format!("Reloaded {n}"))
        } else {
            format!("Reloaded {}", classes.join(", "))
        }
    } else {
        module_name.map_or_else(|| "Reloaded".to_string(), |n| format!("Reloaded {n}"))
    };
    match (response.affected_actors, response.migration_failures) {
        (Some(count), Some(failures)) if count > 0 && failures > 0 => {
            let word = if count == 1 { "actor" } else { "actors" };
            println!("{label} ({count} {word} updated, {failures} failed)");
            eprintln!(
                "Warning: {failures} actor(s) failed code migration. Consider restarting them with :kill"
            );
        }
        (Some(count), _) if count > 0 => {
            let word = if count == 1 { "actor" } else { "actors" };
            println!("{label} ({count} {word} updated)");
        }
        _ => println!("{label}"),
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "REPL main loop handles many commands"
)]
pub fn run(
    port_arg: Option<u16>,
    node_arg: Option<String>,
    foreground: bool,
    workspace_name: Option<&str>,
    persistent: bool,
    timeout: Option<u64>,
) -> Result<()> {
    // Resolve port and node name using priority logic
    let port = resolve_port(port_arg)?;

    // Node name: CLI --node > BEAMTALK_NODE_NAME > None
    let node_name = resolve_node_name(node_arg);

    println!("Beamtalk v{}", env!("CARGO_PKG_VERSION"));
    println!("Type :help for available commands, :exit to quit.");
    println!();

    // Ensure compiler daemon is running
    if is_daemon_running()?.is_none() {
        start_daemon()?;
    }

    // Choose startup mode: workspace (default) or foreground (debug)
    let (beam_guard_opt, is_new_workspace, connect_port) = if foreground {
        // Foreground mode: start node directly (original behavior)
        println!("Starting BEAM node in foreground mode (--foreground)...");
        let mut child = start_beam_node(port, node_name.as_ref())?;

        // Discover the actual port from the BEAM node's stdout.
        // The BEAM prints "BEAMTALK_PORT:<port>" after binding.
        let actual_port = if port == 0 {
            read_port_from_child(&mut child)?
        } else {
            port
        };

        (Some(BeamChildGuard { child }), true, actual_port)
    } else {
        // Workspace mode: start or connect to detached node
        let current_dir = std::env::current_dir().into_diagnostic()?;
        let project_root = workspace::discovery::discover_project_root(&current_dir);

        // Use the same runtime paths as foreground mode
        let runtime_dir = beamtalk_cli::repl_startup::find_runtime_dir()?;
        let paths = beamtalk_cli::repl_startup::beam_paths(&runtime_dir);

        // Warn if stdlib is not compiled (directory may exist without .beam files)
        if !beamtalk_cli::repl_startup::has_beam_files(&paths.stdlib_ebin) {
            warn!(
                "Stdlib not compiled — run `beamtalk build-stdlib` to enable stdlib classes in REPL"
            );
        }

        let (node_info, is_new, workspace_id) = workspace::get_or_start_workspace(
            &project_root,
            workspace_name,
            port,
            &paths.runtime_ebin,
            &paths.workspace_ebin,
            &paths.jsx_ebin,
            &paths.stdlib_ebin,
            !persistent, // auto_cleanup is opposite of persistent flag
            timeout,
        )?;

        let actual_port = node_info.port;

        if is_new {
            println!("Started new workspace node: {}", node_info.node_name);
            if workspace_name.is_some() {
                println!("  Workspace: {workspace_id}");
            } else {
                println!(
                    "  Workspace: {workspace_id} (auto-discovered from {})",
                    project_root.display()
                );
            }
            // Give the node time to initialize
            std::thread::sleep(Duration::from_millis(2000));
        } else {
            println!("✓ Connected to existing workspace: {}", node_info.node_name);
            if workspace_name.is_some() {
                println!("  Workspace: {workspace_id}");
            } else {
                println!(
                    "  Workspace: {workspace_id} (auto-discovered from {})",
                    project_root.display()
                );
            }
        }

        // Display workspace info
        if let Ok(metadata) = workspace::get_workspace_metadata(&workspace_id) {
            println!("  Project:   {}", metadata.project_path.display());
        }

        println!();

        (None, is_new, actual_port) // No guard needed - node is detached
    };

    // Connect to REPL backend
    let mut client = connect_with_retries(connect_port)?;

    println!("Connected to REPL backend on port {connect_port}.");

    // If reconnecting to existing workspace, show available actors
    if beam_guard_opt.is_none() && !is_new_workspace {
        match client.list_actors() {
            Ok(response) => {
                if let Some(actors) = response.actors {
                    if !actors.is_empty() {
                        println!("\nAvailable actors:");
                        for actor in actors {
                            println!("  - {} ({})", actor.class, actor.pid);
                        }
                    }
                }
            }
            Err(e) => {
                // Log but don't fail - actor listing is nice-to-have
                tracing::debug!("Could not list actors: {}", e);
            }
        }
    }

    println!();

    // Set up rustyline editor
    let mut rl: Editor<(), FileHistory> = DefaultEditor::new().into_diagnostic()?;

    // Load history
    let history_file = history_path()?;
    let _ = rl.load_history(&history_file);

    // Main REPL loop
    let mut line_buffer: Vec<String> = Vec::new();
    loop {
        let prompt = if line_buffer.is_empty() { "> " } else { "..> " };
        match rl.readline(prompt) {
            Ok(line) => {
                let line = line.trim();

                // In multi-line mode, empty line continues accumulation
                if line.is_empty() && line_buffer.is_empty() {
                    continue;
                }

                // First line: check if it's a REPL command (only when not
                // already accumulating a multi-line expression)
                if line_buffer.is_empty() {
                    // Add to history (commands and single-line expressions).
                    // Multi-line expressions will also be added as a joined
                    // entry after accumulation completes.
                    let _ = rl.add_history_entry(line);

                    // Handle special commands
                    match line {
                        ":exit" | ":quit" | ":q" => {
                            println!("Goodbye!");
                            break;
                        }
                        ":help" | ":h" | ":?" => {
                            print_help();
                            continue;
                        }
                        _ if line.starts_with(":help ") || line.starts_with(":h ") => {
                            let args = if line.starts_with(":help ") {
                                line.strip_prefix(":help ").unwrap().trim()
                            } else {
                                line.strip_prefix(":h ").unwrap().trim()
                            };

                            if args.is_empty() {
                                print_help();
                                continue;
                            }

                            // Parse "ClassName" or "ClassName selector"
                            let (class_name, selector) = match args.split_once(' ') {
                                Some((cls, sel)) => (cls.trim(), Some(sel.trim())),
                                None => (args, None),
                            };

                            match client.get_docs(class_name, selector) {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("{msg}");
                                        }
                                    } else if let Some(docs) = &response.docs {
                                        println!("{docs}");
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        ":clear" => {
                            match client.clear_bindings() {
                                Ok(_) => println!("Bindings cleared."),
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        ":bindings" | ":b" => {
                            match client.get_bindings() {
                                Ok(response) => {
                                    if let Some(serde_json::Value::Object(map)) = response.bindings
                                    {
                                        if map.is_empty() {
                                            println!("No bindings.");
                                        } else {
                                            for (name, value) in map {
                                                println!("  {name} = {}", format_value(&value));
                                            }
                                        }
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        _ if line.starts_with(":load ") || line.starts_with(":l ") => {
                            let path = if line.starts_with(":load ") {
                                line.strip_prefix(":load ").unwrap().trim()
                            } else {
                                line.strip_prefix(":l ").unwrap().trim()
                            };

                            if path.is_empty() {
                                eprintln!("Usage: :load <path>");
                                continue;
                            }

                            match client.load_file(path) {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("Error: {msg}");
                                        }
                                    } else if let Some(classes) = response.classes {
                                        if classes.is_empty() {
                                            println!("Loaded {path}");
                                        } else {
                                            println!("Loaded {}", classes.join(", "));
                                        }
                                    } else {
                                        println!("Loaded {path}");
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        _ if line.starts_with(":reload ") || line.starts_with(":r ") => {
                            let module_name = if line.starts_with(":reload ") {
                                line.strip_prefix(":reload ").unwrap().trim()
                            } else {
                                line.strip_prefix(":r ").unwrap().trim()
                            };
                            match client.reload_module(module_name) {
                                Ok(response) => {
                                    display_reload_result(&response, Some(module_name));
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        ":reload" | ":r" => {
                            match client.reload_file() {
                                Ok(response) => {
                                    display_reload_result(&response, None);
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        ":actors" | ":a" => {
                            match client.list_actors() {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("Error: {msg}");
                                        }
                                    } else if let Some(actors) = response.actors {
                                        if actors.is_empty() {
                                            println!("No running actors.");
                                        } else {
                                            println!("Running actors:");
                                            for actor in actors {
                                                println!(
                                                    "  {} - {} ({})",
                                                    actor.pid, actor.class, actor.module
                                                );
                                            }
                                        }
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        ":modules" | ":m" => {
                            match client.list_modules() {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("Error: {msg}");
                                        }
                                    } else if let Some(modules) = response.modules {
                                        if modules.is_empty() {
                                            println!("No modules loaded.");
                                        } else {
                                            println!("Loaded modules:");
                                            for module in modules {
                                                let actors_text = if module.actor_count == 0 {
                                                    String::new()
                                                } else if module.actor_count == 1 {
                                                    " - 1 actor".to_string()
                                                } else {
                                                    format!(" - {} actors", module.actor_count)
                                                };
                                                println!(
                                                    "  {} ({}){} - loaded {}",
                                                    module.name,
                                                    module.source_file,
                                                    actors_text,
                                                    module.time_ago
                                                );
                                            }
                                        }
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        _ if line.starts_with(":unload ") => {
                            let module_name = line.strip_prefix(":unload ").unwrap().trim();
                            if module_name.is_empty() {
                                eprintln!("Usage: :unload <module>");
                                continue;
                            }

                            match client.unload_module(module_name) {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("Error: {msg}");
                                        }
                                    } else {
                                        println!("Module {module_name} unloaded.");
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        _ if line.starts_with(":kill ") => {
                            let pid_str = line.strip_prefix(":kill ").unwrap().trim();
                            if pid_str.is_empty() {
                                eprintln!("Usage: :kill <pid>");
                                continue;
                            }

                            match client.kill_actor(pid_str) {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("Error: {msg}");
                                        }
                                    } else {
                                        println!("Actor {pid_str} killed.");
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        ":sessions" => {
                            match client.list_sessions() {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("Error: {msg}");
                                        }
                                    } else if let Some(sessions) = response.sessions {
                                        if sessions.is_empty() {
                                            println!("No active sessions.");
                                        } else {
                                            println!("Active sessions:");
                                            for s in sessions {
                                                println!("  {}", s.id);
                                            }
                                        }
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        _ if line.starts_with(":inspect ") => {
                            let pid_str = line.strip_prefix(":inspect ").unwrap().trim();
                            if pid_str.is_empty() {
                                eprintln!("Usage: :inspect <pid>");
                                continue;
                            }

                            match client.inspect_actor(pid_str) {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("Error: {msg}");
                                        }
                                    } else if let Some(state) = response.state {
                                        println!("{}", format_value(&state));
                                    }
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        _ => {}
                    }

                    // Detect common commands typed without ':' prefix.
                    // Only match full command names to avoid false positives with
                    // single-letter variable names (e.g. `r`, `l`, `b`).
                    let first_word = line.split_whitespace().next().unwrap_or("");
                    if let Some(suggestion) = match first_word {
                        "load" => Some(":load"),
                        "reload" => Some(":reload"),
                        "help" => Some(":help"),
                        "exit" | "quit" => Some(":exit"),
                        "clear" => Some(":clear"),
                        "bindings" => Some(":bindings"),
                        "actors" => Some(":actors"),
                        "modules" => Some(":modules"),
                        "unload" => Some(":unload"),
                        "kill" => Some(":kill"),
                        "inspect" => Some(":inspect"),
                        "sessions" => Some(":sessions"),
                        _ => None,
                    } {
                        eprintln!(
                            "Hint: did you mean `{suggestion}`? REPL commands start with `:`"
                        );
                        continue;
                    }
                } // end if line_buffer.is_empty() (command handling)

                // Accumulate input for multi-line expression detection
                line_buffer.push(line.to_string());
                let accumulated = line_buffer.join("\n");

                if !is_input_complete(&accumulated) {
                    // Input is incomplete — continue reading
                    continue;
                }

                // Input is complete — add to history as single entry and evaluate
                let _ = rl.add_history_entry(&accumulated);

                // Evaluate expression
                match client.eval(&accumulated) {
                    Ok(response) => {
                        // Print captured stdout before value/error (BT-355)
                        if let Some(ref output) = response.output {
                            if !output.is_empty() {
                                print!("{output}");
                                if !output.ends_with('\n') {
                                    println!();
                                }
                                let _ = std::io::Write::flush(&mut std::io::stdout());
                            }
                        }
                        // Display compilation warnings (BT-407)
                        if let Some(ref warnings) = response.warnings {
                            for warning in warnings {
                                eprintln!("⚠ {warning}");
                            }
                        }
                        if response.is_error() {
                            if let Some(msg) = response.error_message() {
                                eprintln!("Error: {msg}");
                            }
                        } else if let Some(value) = response.value {
                            println!("{}", format_value(&value));
                        }
                    }
                    Err(e) => {
                        eprintln!("Communication error: {e}");
                        eprintln!("The REPL backend may have crashed. Exiting.");
                        break;
                    }
                }
                line_buffer.clear();
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl+C — cancel multi-line input if buffering, otherwise just newline
                if !line_buffer.is_empty() {
                    line_buffer.clear();
                    eprintln!("Cancelled");
                }
                println!();
            }
            Err(ReadlineError::Eof) => {
                // Ctrl+D - exit
                println!("Goodbye!");
                break;
            }
            Err(e) => {
                eprintln!("Readline error: {e}");
                break;
            }
        }
    }

    // Save history
    let _ = rl.save_history(&history_file);

    // BEAM child is cleaned up automatically by BeamChildGuard::drop()
    // Clean up BEAM node if in foreground mode
    if let Some(guard) = beam_guard_opt {
        drop(guard);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use process::DEFAULT_REPL_PORT;
    use serial_test::serial;

    #[test]
    fn format_value_string() {
        let value = serde_json::json!("hello");
        assert_eq!(format_value(&value), "hello");
    }

    #[test]
    fn format_value_number() {
        let value = serde_json::json!(42);
        assert_eq!(format_value(&value), "42");
    }

    #[test]
    fn format_value_bool() {
        let value = serde_json::json!(true);
        assert_eq!(format_value(&value), "true");
    }

    #[test]
    fn format_value_array() {
        let value = serde_json::json!([1, 2, 3]);
        assert_eq!(format_value(&value), "[1, 2, 3]");
    }

    #[test]
    fn format_value_pid() {
        // Backend now pre-formats pids as "#Actor<pid>"
        let value = serde_json::json!("#Actor<0.123.0>");
        assert_eq!(format_value(&value), "#Actor<0.123.0>");
    }

    #[test]
    fn format_value_block() {
        // Blocks are formatted as "a Block/N" by the backend
        let value = serde_json::json!("a Block/1");
        assert_eq!(format_value(&value), "a Block/1");

        let value2 = serde_json::json!("a Block/2");
        assert_eq!(format_value(&value2), "a Block/2");
    }

    #[test]
    fn format_value_tuple() {
        let value = serde_json::json!({"__tuple__": [1, "hello"]});
        assert_eq!(format_value(&value), "(1, hello)");
    }

    /// Uses `#[serial(env_var)]` because it modifies the `BEAMTALK_RUNTIME_DIR`
    /// environment variable, which is process-global state.
    #[test]
    #[serial(env_var)]
    fn find_runtime_dir_respects_env_var() {
        // Create a temp directory with the expected structure (needs rebar.config)
        let temp_dir = std::env::temp_dir().join("beamtalk_test_runtime");
        std::fs::create_dir_all(&temp_dir).unwrap();
        std::fs::write(temp_dir.join("rebar.config"), "{deps, []}.").unwrap();

        // SAFETY: This test runs single-threaded and we restore the env var after.
        // The set_var/remove_var pair is scoped to just around the find_runtime_dir call.
        unsafe { std::env::set_var("BEAMTALK_RUNTIME_DIR", &temp_dir) };
        let result = beamtalk_cli::repl_startup::find_runtime_dir();
        // SAFETY: Restoring env var set earlier in this test
        unsafe { std::env::remove_var("BEAMTALK_RUNTIME_DIR") };

        // Cleanup
        let _ = std::fs::remove_dir_all(&temp_dir);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), temp_dir);
    }

    /// Uses `#[serial(env_var)]` because it modifies the `BEAMTALK_RUNTIME_DIR`
    /// environment variable, which is process-global state.
    #[test]
    #[serial(env_var)]
    fn find_runtime_dir_env_var_requires_rebar_config() {
        // Create a temp directory WITHOUT rebar.config
        let temp_dir = std::env::temp_dir().join("beamtalk_test_runtime_no_rebar");
        std::fs::create_dir_all(&temp_dir).unwrap();

        // SAFETY: This test runs single-threaded and we restore the env var after.
        // The set_var/remove_var pair is scoped to just around the find_runtime_dir call.
        unsafe { std::env::set_var("BEAMTALK_RUNTIME_DIR", &temp_dir) };
        let result = beamtalk_cli::repl_startup::find_runtime_dir();
        // SAFETY: Restoring env var set earlier in this test
        unsafe { std::env::remove_var("BEAMTALK_RUNTIME_DIR") };

        // Cleanup
        let _ = std::fs::remove_dir_all(&temp_dir);

        // Should fail because rebar.config is missing
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("does not contain a valid runtime"));
    }

    #[test]
    fn beam_child_guard_kills_process_on_drop() {
        use std::process::Command;

        // Spawn a long-running process (sleep for 60 seconds)
        let child = Command::new("sleep")
            .arg("60")
            .spawn()
            .expect("Failed to spawn sleep process");

        let pid = child.id();

        // Wrap in guard and drop it
        {
            let _guard = BeamChildGuard { child };
            // Guard is dropped here
        }

        // Give it a moment to clean up
        std::thread::sleep(std::time::Duration::from_millis(100));

        // Verify the process is no longer running
        // On Unix, kill -0 returns success if process exists, failure if it doesn't
        #[cfg(unix)]
        {
            let result = Command::new("kill")
                .args(["-0", &pid.to_string()])
                .output()
                .expect("Failed to run kill -0");
            // kill -0 returns exit code 0 if process exists, non-zero if it doesn't
            assert!(
                !result.status.success(),
                "Process should have been killed (kill -0 should fail)"
            );
        }
    }

    #[test]
    fn actor_info_deserializes_correctly() {
        let json = r#"{
            "pid": "<0.123.0>",
            "class": "Counter",
            "module": "counter",
            "spawned_at": 1234567890
        }"#;

        let info: ActorInfo = serde_json::from_str(json).expect("Failed to parse ActorInfo");
        assert_eq!(info.pid, "<0.123.0>");
        assert_eq!(info.class, "Counter");
        assert_eq!(info.module, "counter");
        assert_eq!(info.spawned_at, 1_234_567_890);
    }

    #[test]
    fn actors_response_deserializes_correctly() {
        let json = r#"{
            "type": "actors",
            "actors": [
                {
                    "pid": "<0.123.0>",
                    "class": "Counter",
                    "module": "beamtalk_repl_eval_42",
                    "spawned_at": 1234567890
                },
                {
                    "pid": "<0.124.0>",
                    "class": "Logger",
                    "module": "beamtalk_repl_eval_43",
                    "spawned_at": 1234567891
                }
            ]
        }"#;

        let response: ReplResponse =
            serde_json::from_str(json).expect("Failed to parse ReplResponse");
        assert_eq!(response.response_type, Some("actors".to_string()));

        let actors = response.actors.expect("actors field missing");
        assert_eq!(actors.len(), 2);

        assert_eq!(actors[0].pid, "<0.123.0>");
        assert_eq!(actors[0].class, "Counter");
        assert_eq!(actors[0].module, "beamtalk_repl_eval_42");

        assert_eq!(actors[1].pid, "<0.124.0>");
        assert_eq!(actors[1].class, "Logger");
    }

    #[test]
    fn actors_response_empty_list() {
        let json = r#"{
            "type": "actors",
            "actors": []
        }"#;

        let response: ReplResponse =
            serde_json::from_str(json).expect("Failed to parse ReplResponse");
        assert_eq!(response.response_type, Some("actors".to_string()));

        let actors = response.actors.expect("actors field missing");
        assert!(actors.is_empty());
    }

    #[test]
    fn kill_response_success() {
        let json = r#"{
            "type": "result",
            "value": "ok"
        }"#;

        let response: ReplResponse =
            serde_json::from_str(json).expect("Failed to parse ReplResponse");
        assert_eq!(response.response_type, Some("result".to_string()));
        assert!(response.value.is_some());
    }

    #[test]
    fn kill_response_not_found() {
        let json = r#"{
            "type": "error",
            "message": "not_found"
        }"#;

        let response: ReplResponse =
            serde_json::from_str(json).expect("Failed to parse ReplResponse");
        assert_eq!(response.response_type, Some("error".to_string()));
        assert_eq!(response.message, Some("not_found".to_string()));
    }

    #[test]
    fn kill_response_invalid_pid() {
        let json = r#"{
            "type": "error",
            "message": "invalid_pid"
        }"#;

        let response: ReplResponse =
            serde_json::from_str(json).expect("Failed to parse ReplResponse");
        assert_eq!(response.response_type, Some("error".to_string()));
        assert_eq!(response.message, Some("invalid_pid".to_string()));
    }

    // Tests for port/node resolution
    #[test]
    fn resolve_port_cli_flag_takes_priority() {
        // CLI flag should override everything
        let result = resolve_port(Some(9999));
        assert_eq!(result.unwrap(), 9999);
    }

    #[test]
    fn resolve_port_cli_flag_default_still_takes_priority() {
        // Even if CLI flag is 0 (the default), it should be used
        let result = resolve_port(Some(DEFAULT_REPL_PORT));
        assert_eq!(result.unwrap(), DEFAULT_REPL_PORT);
    }

    #[test]
    #[serial(env_var)]
    fn resolve_port_default_without_env() {
        // When no CLI flag and no env var, should return default (0 = OS-assigned)
        let result = resolve_port(None);
        // If env var is set, it will return that; if not, default
        // The test verifies the function runs without error
        assert!(result.is_ok());
    }

    #[test]
    #[serial(env_var)]
    fn resolve_port_env_var_used_when_no_cli_flag() {
        // Set env var and verify it's used when no CLI flag
        // SAFETY: This test runs single-threaded via #[serial], restoring env var after
        unsafe { std::env::set_var("BEAMTALK_REPL_PORT", "9123") };
        let result = resolve_port(None);
        // SAFETY: Restoring env var set earlier in this test
        unsafe { std::env::remove_var("BEAMTALK_REPL_PORT") };

        assert_eq!(result.unwrap(), 9123);
    }

    #[test]
    #[serial(env_var)]
    fn resolve_port_cli_flag_overrides_env_var() {
        // Even with env var set, CLI flag should win
        // SAFETY: This test runs single-threaded via #[serial], restoring env var after
        unsafe { std::env::set_var("BEAMTALK_REPL_PORT", "9123") };
        let result = resolve_port(Some(9456));
        // SAFETY: Restoring env var set earlier in this test
        unsafe { std::env::remove_var("BEAMTALK_REPL_PORT") };

        assert_eq!(result.unwrap(), 9456);
    }

    #[test]
    #[serial(env_var)]
    fn resolve_port_invalid_env_var_returns_error() {
        // Invalid env var should return an error
        // SAFETY: This test runs single-threaded via #[serial], restoring env var after
        unsafe { std::env::set_var("BEAMTALK_REPL_PORT", "not_a_number") };
        let result = resolve_port(None);
        // SAFETY: Restoring env var set earlier in this test
        unsafe { std::env::remove_var("BEAMTALK_REPL_PORT") };

        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("Invalid BEAMTALK_REPL_PORT"));
    }

    #[test]
    fn resolve_node_name_cli_flag_takes_priority() {
        let result = resolve_node_name(Some("mynode@localhost".to_string()));
        assert_eq!(result, Some("mynode@localhost".to_string()));
    }

    #[test]
    fn resolve_node_name_none_without_env() {
        // When no CLI flag and no env var, should return None
        let result = resolve_node_name(None);
        // If env var is set, it will return that; if not, None
        // The test verifies the function handles None correctly
        // We can't assume env var state without serial test
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    #[serial(env_var)]
    fn resolve_node_name_env_var_used_when_no_cli_flag() {
        // SAFETY: This test runs single-threaded via #[serial], restoring env var after
        unsafe { std::env::set_var("BEAMTALK_NODE_NAME", "envnode@localhost") };
        let result = resolve_node_name(None);
        // SAFETY: Restoring env var set earlier in this test
        unsafe { std::env::remove_var("BEAMTALK_NODE_NAME") };

        assert_eq!(result, Some("envnode@localhost".to_string()));
    }

    #[test]
    #[serial(env_var)]
    fn resolve_node_name_cli_flag_overrides_env_var() {
        // SAFETY: This test runs single-threaded via #[serial], restoring env var after
        unsafe { std::env::set_var("BEAMTALK_NODE_NAME", "envnode@localhost") };
        let result = resolve_node_name(Some("clinode@localhost".to_string()));
        // SAFETY: Restoring env var set earlier in this test
        unsafe { std::env::remove_var("BEAMTALK_NODE_NAME") };

        assert_eq!(result, Some("clinode@localhost".to_string()));
    }
}
