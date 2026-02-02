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
//! ┌─────────────────┐     ┌──────────────────────┐
//! │  REPL CLI       │     │   BEAM Node          │
//! │  (this module)  │────▶│  beamtalk_repl.erl   │
//! │                 │ TCP │                      │
//! │  rustyline      │     │  Compiler Daemon ◀───┤
//! └─────────────────┘     └──────────────────────┘
//!       │                           │
//!   localhost:9000            Unix socket
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
//! The REPL CLI communicates with the Erlang backend using JSON over TCP:
//!
//! ```json
//! // Request
//! {"type": "eval", "expression": "x := 42"}
//!
//! // Response (success)
//! {"type": "result", "value": "42"}
//!
//! // Response (error)
//! {"type": "error", "message": "Undefined variable: foo"}
//! ```

use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::net::TcpStream;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};
use rustyline::error::ReadlineError;
use rustyline::history::FileHistory;
use rustyline::{DefaultEditor, Editor};
use serde::Deserialize;

use crate::paths::{beamtalk_dir, is_daemon_running};

/// Connection timeout in milliseconds.
const CONNECT_TIMEOUT_MS: u64 = 5000;

/// Maximum retries when connecting to REPL backend.
const MAX_CONNECT_RETRIES: u32 = 10;

/// Delay between connection retries in milliseconds.
const RETRY_DELAY_MS: u64 = 500;

/// JSON response from the REPL backend.
#[derive(Debug, Deserialize)]
struct ReplResponse {
    #[serde(rename = "type")]
    response_type: String,
    value: Option<serde_json::Value>,
    message: Option<String>,
    bindings: Option<serde_json::Value>,
    classes: Option<Vec<String>>,
    actors: Option<Vec<ActorInfo>>,
    modules: Option<Vec<ModuleInfo>>,
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

/// REPL client state.
struct ReplClient {
    stream: TcpStream,
    reader: BufReader<TcpStream>,
    last_loaded_file: Option<String>,
}

impl ReplClient {
    /// Connect to the REPL backend.
    fn connect(port: u16) -> Result<Self> {
        let addr = format!("127.0.0.1:{port}");
        let stream = TcpStream::connect_timeout(
            &addr.parse().into_diagnostic()?,
            Duration::from_millis(CONNECT_TIMEOUT_MS),
        )
        .map_err(|e| miette!("Failed to connect to REPL backend at {addr}: {e}"))?;

        // Clone for reader
        let reader_stream = stream.try_clone().into_diagnostic()?;
        let reader = BufReader::new(reader_stream);

        Ok(Self {
            stream,
            reader,
            last_loaded_file: None,
        })
    }

    /// Send an eval request and receive the response.
    fn eval(&mut self, expression: &str) -> Result<ReplResponse> {
        // Build JSON request
        let request = serde_json::json!({
            "type": "eval",
            "expression": expression
        });
        let request_str = serde_json::to_string(&request).into_diagnostic()?;

        // Send request
        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        // Receive response
        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        // Parse response
        serde_json::from_str(&response_line)
            .map_err(|e| miette!("Failed to parse REPL response: {e}\nRaw: {response_line}"))
    }

    /// Send a clear bindings request.
    fn clear_bindings(&mut self) -> Result<ReplResponse> {
        let request = serde_json::json!({ "type": "clear" });
        let request_str = serde_json::to_string(&request).into_diagnostic()?;

        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line).map_err(|e| miette!("Failed to parse response: {e}"))
    }

    /// Get current bindings.
    fn get_bindings(&mut self) -> Result<ReplResponse> {
        let request = serde_json::json!({ "type": "bindings" });
        let request_str = serde_json::to_string(&request).into_diagnostic()?;

        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line).map_err(|e| miette!("Failed to parse response: {e}"))
    }

    /// Load a Beamtalk file.
    fn load_file(&mut self, path: &str) -> Result<ReplResponse> {
        let request = serde_json::json!({
            "type": "load",
            "path": path
        });
        let request_str = serde_json::to_string(&request).into_diagnostic()?;

        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        let response: ReplResponse = serde_json::from_str(&response_line)
            .map_err(|e| miette!("Failed to parse response: {e}"))?;

        // Update last loaded file on success
        if response.response_type == "loaded" {
            self.last_loaded_file = Some(path.to_string());
        }

        Ok(response)
    }

    /// Reload the last loaded file.
    fn reload_file(&mut self) -> Result<ReplResponse> {
        let path = self
            .last_loaded_file
            .clone()
            .ok_or_else(|| miette!("No file has been loaded yet"))?;
        self.load_file(&path)
    }

    /// List running actors.
    fn list_actors(&mut self) -> Result<ReplResponse> {
        let request = serde_json::json!({ "type": "actors" });
        let request_str = serde_json::to_string(&request).into_diagnostic()?;

        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line).map_err(|e| miette!("Failed to parse response: {e}"))
    }

    /// Kill an actor by PID string.
    fn kill_actor(&mut self, pid_str: &str) -> Result<ReplResponse> {
        let request = serde_json::json!({
            "type": "kill",
            "pid": pid_str
        });
        let request_str = serde_json::to_string(&request).into_diagnostic()?;

        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line).map_err(|e| miette!("Failed to parse response: {e}"))
    }

    /// List loaded modules.
    fn list_modules(&mut self) -> Result<ReplResponse> {
        let request = serde_json::json!({ "type": "modules" });
        let request_str = serde_json::to_string(&request).into_diagnostic()?;

        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line).map_err(|e| miette!("Failed to parse response: {e}"))
    }

    /// Unload a module by name.
    fn unload_module(&mut self, module_name: &str) -> Result<ReplResponse> {
        let request = serde_json::json!({
            "type": "unload",
            "module": module_name
        });
        let request_str = serde_json::to_string(&request).into_diagnostic()?;

        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line).map_err(|e| miette!("Failed to parse response: {e}"))
    }
}

/// Path to REPL history file.
fn history_path() -> Result<PathBuf> {
    let dir = beamtalk_dir()?;
    fs::create_dir_all(&dir).into_diagnostic()?;
    Ok(dir.join("repl_history"))
}

/// Find the runtime directory by checking multiple possible locations.
fn find_runtime_dir() -> Result<PathBuf> {
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
fn start_daemon() -> Result<()> {
    eprintln!("Starting compiler daemon...");

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

    eprintln!("Compiler daemon started.");
    Ok(())
}

/// Start the BEAM node with REPL backend.
fn start_beam_node(port: u16, node_name: Option<&String>) -> Result<Child> {
    // Find runtime directory - try multiple locations
    let runtime_dir = find_runtime_dir()?;
    eprintln!("Using runtime at: {}", runtime_dir.display());

    // Build runtime first
    let build_lib_dir = runtime_dir.join("_build/default/lib");
    let runtime_beam_dir = build_lib_dir.join("beamtalk_runtime/ebin");
    let jsx_beam_dir = build_lib_dir.join("jsx/ebin");

    // Check if runtime is built
    if !runtime_beam_dir.exists() {
        eprintln!("Building Beamtalk runtime...");
        let status = Command::new("rebar3")
            .arg("compile")
            .current_dir(&runtime_dir)
            .status()
            .map_err(|e| miette!("Failed to build runtime: {e}"))?;

        if !status.success() {
            return Err(miette!("Failed to build Beamtalk runtime"));
        }
    }

    eprintln!("Starting BEAM node with REPL backend on port {port}...");

    // Build the eval command that configures the runtime via application:set_env
    // This allows runtime to read port from application environment
    let eval_cmd = if let Some(name) = node_name {
        format!(
            "application:set_env(beamtalk_runtime, repl_port, {port}), \
             application:set_env(beamtalk_runtime, node_name, '{name}'), \
             {{ok, _}} = beamtalk_repl:start_link(), \
             io:format(\"REPL backend started on port {port} (node: {name})~n\"), \
             receive stop -> ok end."
        )
    } else {
        format!(
            "application:set_env(beamtalk_runtime, repl_port, {port}), \
             {{ok, _}} = beamtalk_repl:start_link(), \
             io:format(\"REPL backend started on port {port}~n\"), \
             receive stop -> ok end."
        )
    };

    // Start erl with beamtalk_repl running
    // The receive loop keeps the BEAM VM alive while REPL is running
    let child = Command::new("erl")
        .args([
            "-noshell",
            "-pa",
            runtime_beam_dir.to_str().unwrap_or(""),
            "-pa",
            jsx_beam_dir.to_str().unwrap_or(""),
            "-eval",
            &eval_cmd,
        ])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| miette!("Failed to start BEAM node: {e}\nIs Erlang/OTP installed?"))?;

    Ok(child)
}

/// Connect to REPL backend with retries.
fn connect_with_retries(port: u16) -> Result<ReplClient> {
    for attempt in 1..=MAX_CONNECT_RETRIES {
        match ReplClient::connect(port) {
            Ok(client) => return Ok(client),
            Err(e) => {
                if attempt == MAX_CONNECT_RETRIES {
                    return Err(e);
                }
                std::thread::sleep(Duration::from_millis(RETRY_DELAY_MS));
            }
        }
    }
    Err(miette!("Failed to connect to REPL backend"))
}

/// Format a value for display.
fn format_value(value: &serde_json::Value) -> String {
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
fn print_help() {
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

/// Guard to ensure BEAM child process is killed on drop.
/// This prevents orphaned BEAM processes when REPL exits early due to errors.
struct BeamChildGuard {
    child: Child,
}

impl Drop for BeamChildGuard {
    fn drop(&mut self) {
        let _ = self.child.kill();
        // Wait to reap the process and prevent zombies
        let _ = self.child.wait();
    }
}

/// Run the REPL.
#[expect(
    clippy::too_many_lines,
    reason = "REPL main loop handles many commands"
)]
pub fn run(port_arg: u16, node_arg: Option<String>) -> Result<()> {
    // Priority: CLI flag (if non-default) > environment variable > CLI default
    // If user explicitly passes --port, it takes precedence
    // If BEAMTALK_REPL_PORT is set, it overrides the default
    // Note: Cannot distinguish "--port 9000" from omitting --port
    let port = if port_arg != 9000 {
        // Explicitly passed a non-default port
        port_arg
    } else if let Ok(env_port) = std::env::var("BEAMTALK_REPL_PORT") {
        // Use env var if set
        env_port
            .parse()
            .map_err(|_| miette!("Invalid BEAMTALK_REPL_PORT: {env_port}"))?
    } else {
        // Use default
        port_arg
    };

    // Node name: CLI --node > BEAMTALK_NODE_NAME > None
    let node_name = node_arg.or_else(|| std::env::var("BEAMTALK_NODE_NAME").ok());

    println!("Beamtalk v{}", env!("CARGO_PKG_VERSION"));
    println!("Type :help for available commands, :exit to quit.");
    println!();

    // Ensure compiler daemon is running
    if is_daemon_running()?.is_none() {
        start_daemon()?;
    }

    // Start BEAM node with REPL backend
    // Use a guard to ensure cleanup on any exit path
    let beam_guard = BeamChildGuard {
        child: start_beam_node(port, node_name.as_ref())?,
    };

    // Connect to REPL backend
    let mut client = connect_with_retries(port)?;

    println!("Connected to REPL backend.");
    println!();

    // Set up rustyline editor
    let mut rl: Editor<(), FileHistory> = DefaultEditor::new().into_diagnostic()?;

    // Load history
    let history_file = history_path()?;
    let _ = rl.load_history(&history_file);

    // Main REPL loop
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                // Add to history
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
                                if let Some(serde_json::Value::Object(map)) = response.bindings {
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
                                if response.response_type == "loaded" {
                                    if let Some(classes) = response.classes {
                                        if classes.is_empty() {
                                            println!("Loaded {path}");
                                        } else {
                                            println!("Loaded {}", classes.join(", "));
                                        }
                                    } else {
                                        println!("Loaded {path}");
                                    }
                                } else if response.response_type == "error" {
                                    if let Some(msg) = response.message {
                                        eprintln!("Error: {msg}");
                                    }
                                }
                            }
                            Err(e) => eprintln!("Error: {e}"),
                        }
                        continue;
                    }
                    ":reload" | ":r" => {
                        match client.reload_file() {
                            Ok(response) => {
                                if response.response_type == "loaded" {
                                    if let Some(classes) = response.classes {
                                        if classes.is_empty() {
                                            println!("Reloaded");
                                        } else {
                                            println!("Reloaded {}", classes.join(", "));
                                        }
                                    } else {
                                        println!("Reloaded");
                                    }
                                } else if response.response_type == "error" {
                                    if let Some(msg) = response.message {
                                        eprintln!("Error: {msg}");
                                    }
                                }
                            }
                            Err(e) => eprintln!("Error: {e}"),
                        }
                        continue;
                    }
                    ":actors" | ":a" => {
                        match client.list_actors() {
                            Ok(response) => {
                                if response.response_type == "actors" {
                                    if let Some(actors) = response.actors {
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
                                } else if response.response_type == "error" {
                                    if let Some(msg) = response.message {
                                        eprintln!("Error: {msg}");
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
                                if response.response_type == "modules" {
                                    if let Some(modules) = response.modules {
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
                                } else if response.response_type == "error" {
                                    if let Some(msg) = response.message {
                                        eprintln!("Error: {msg}");
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
                                if response.response_type == "result" {
                                    println!("Module {module_name} unloaded.");
                                } else if response.response_type == "error" {
                                    if let Some(msg) = response.message {
                                        eprintln!("Error: {msg}");
                                    }
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
                                if response.response_type == "result" {
                                    println!("Actor {pid_str} killed.");
                                } else if response.response_type == "error" {
                                    if let Some(msg) = response.message {
                                        eprintln!("Error: {msg}");
                                    }
                                }
                            }
                            Err(e) => eprintln!("Error: {e}"),
                        }
                        continue;
                    }
                    _ => {}
                }

                // Evaluate expression
                match client.eval(line) {
                    Ok(response) => {
                        if response.response_type == "result" {
                            if let Some(value) = response.value {
                                println!("{}", format_value(&value));
                            }
                        } else if response.response_type == "error" {
                            if let Some(msg) = response.message {
                                eprintln!("Error: {msg}");
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("Communication error: {e}");
                        eprintln!("The REPL backend may have crashed. Exiting.");
                        break;
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl+C - just print newline and continue
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
    drop(beam_guard);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
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
        let result = find_runtime_dir();
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
        let result = find_runtime_dir();
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
        assert_eq!(response.response_type, "actors");

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
        assert_eq!(response.response_type, "actors");

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
        assert_eq!(response.response_type, "result");
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
        assert_eq!(response.response_type, "error");
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
        assert_eq!(response.response_type, "error");
        assert_eq!(response.message, Some("invalid_pid".to_string()));
    }
}
