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
//!                         │    ├─ beamtalk_actor_registry      │
//!                         │    └─ beamtalk_compiler_sup        │
//!                         │         └─ beamtalk_compiler_server│
//!                         │              └─ OTP Port (Rust)    │
//!                         └──────────────────────────────────┘
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

use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};
use rustyline::error::ReadlineError;
use rustyline::history::FileHistory;
use rustyline::{CompletionType, Config, Editor};
use serde::Deserialize;
use tracing::warn;

use crate::commands::workspace;

use beamtalk_core::source_analysis::is_input_complete;

/// Maximum retries when connecting to REPL backend.
const MAX_CONNECT_RETRIES: u32 = 10;

/// Delay between connection retries in milliseconds.
const RETRY_DELAY_MS: u64 = 500;

pub(crate) mod client;
pub(crate) mod color;
pub(crate) mod display;
pub(crate) mod helper;
pub(crate) mod process;

pub mod bind;

use bind::{resolve_bind_addr, validate_network_binding};
use client::ReplClient;
use display::{format_error, format_value, history_path, print_help};
use helper::ReplHelper;
use process::{
    BeamChildGuard, connect_with_retries, read_port_from_child, resolve_node_name, resolve_port,
    start_beam_node,
};

/// JSON response from the REPL backend.
/// Supports both legacy format (type field) and new protocol format (status field).
#[derive(Debug, Deserialize)]
#[allow(dead_code)] // fields populated by serde deserialization from REPL JSON protocol
pub(crate) struct ReplResponse {
    /// Legacy response type (result, error, bindings, loaded, actors, modules)
    #[serde(rename = "type")]
    pub(crate) response_type: Option<String>,
    /// New protocol: message correlation ID
    pub(crate) id: Option<String>,
    /// New protocol: session ID
    pub(crate) session: Option<String>,
    /// New protocol: status flags
    pub(crate) status: Option<Vec<String>>,
    /// Result value (both formats)
    pub(crate) value: Option<serde_json::Value>,
    /// Captured stdout from evaluation (BT-355)
    pub(crate) output: Option<String>,
    /// Legacy: error message
    pub(crate) message: Option<String>,
    /// New protocol: error message
    pub(crate) error: Option<String>,
    /// Bindings map (both formats)
    pub(crate) bindings: Option<serde_json::Value>,
    /// Loaded classes (both formats)
    pub(crate) classes: Option<Vec<String>>,
    /// Actor list (both formats)
    pub(crate) actors: Option<Vec<ActorInfo>>,
    /// Module list (both formats)
    pub(crate) modules: Option<Vec<ModuleInfo>>,
    /// Session list (new protocol)
    pub(crate) sessions: Option<Vec<SessionInfo>>,
    /// Completion suggestions (new protocol)
    pub(crate) completions: Option<Vec<String>>,
    /// Symbol info (new protocol)
    pub(crate) info: Option<serde_json::Value>,
    /// Actor state (new protocol: inspect op)
    pub(crate) state: Option<serde_json::Value>,
    /// Compilation warnings (BT-407)
    pub(crate) warnings: Option<Vec<String>>,
    /// Documentation text (BT-500: :help command)
    pub(crate) docs: Option<String>,
    /// Generated Core Erlang source (BT-724: :show-codegen command)
    pub(crate) core_erlang: Option<String>,
    /// Number of actors affected by reload (BT-266)
    pub(crate) affected_actors: Option<u32>,
    /// Number of actors that failed code migration (BT-266)
    pub(crate) migration_failures: Option<u32>,
}

impl ReplResponse {
    /// Check if this is an error response (either format).
    pub(crate) fn is_error(&self) -> bool {
        if let Some(ref t) = self.response_type {
            return t == "error";
        }
        if let Some(ref status) = self.status {
            return status.iter().any(|s| s == "error");
        }
        false
    }

    /// Get the error message (either format).
    pub(crate) fn error_message(&self) -> Option<&str> {
        if let Some(ref msg) = self.message {
            return Some(msg.as_str());
        }
        if let Some(ref err) = self.error {
            return Some(err.as_str());
        }
        None
    }
}

/// Information about a running actor, deserialized from REPL JSON.
#[derive(Debug, Deserialize)]
pub(crate) struct ActorInfo {
    /// Erlang process identifier string (e.g., `<0.123.0>`).
    pub(crate) pid: String,
    /// Beamtalk class name of the actor (e.g., `Counter`).
    pub(crate) class: String,
    /// BEAM module backing the actor.
    #[allow(dead_code)] // deserialized from JSON, used in tests
    pub(crate) module: String,
    /// Unix timestamp when the actor was spawned.
    #[allow(dead_code)] // deserialized from JSON, available for future use
    spawned_at: i64,
}

/// Information about a loaded module, deserialized from REPL JSON.
#[derive(Debug, Deserialize)]
pub(crate) struct ModuleInfo {
    /// Module name as registered in the BEAM node.
    pub(crate) name: String,
    /// Path to the source `.bt` file that defined this module.
    pub(crate) source_file: String,
    /// Number of actors currently running from this module.
    pub(crate) actor_count: u32,
    /// Unix timestamp when the module was loaded.
    #[allow(dead_code)] // deserialized from JSON, available for future use
    load_time: i64,
    /// Human-readable relative time since load (e.g., `2 minutes ago`).
    pub(crate) time_ago: String,
}

/// Information about an active REPL session, deserialized from REPL JSON.
#[derive(Debug, Deserialize)]
pub(crate) struct SessionInfo {
    /// Unique session identifier.
    #[allow(dead_code)] // deserialized from JSON, available for future use
    pub(crate) id: String,
    /// Unix timestamp when the session was created.
    #[allow(dead_code)] // deserialized from JSON, available for future use
    created_at: Option<i64>,
}

/// Auto-run the `[run]` entry expression from `beamtalk.toml`, if present.
///
/// Evaluates the configured entry expression after auto-loading compiled classes.
/// Prints the expression and its result to stdout. On failure, prints a warning
/// but does not abort — the REPL remains interactive.
fn auto_run_entry(client: &mut client::ReplClient, project_root: &Path) {
    let Some(project_root_utf8) = camino::Utf8Path::from_path(project_root) else {
        eprintln!("Warning: project path is not valid UTF-8, skipping [run] entry");
        return;
    };

    let run_config = match crate::commands::manifest::find_run_config(project_root_utf8) {
        Ok(Some(cfg)) => cfg,
        Ok(None) => return,
        Err(e) => {
            eprintln!("Warning: failed to read [run] config: {e}");
            return;
        }
    };

    println!("> {}", run_config.entry);
    match client.eval(&run_config.entry) {
        Ok(response) => {
            let is_error = response.is_error();
            display_eval_response(&response);
            if is_error {
                if response.error_message().is_none() {
                    eprintln!("Warning: [run] entry failed (no error details).");
                }
                eprintln!("The REPL is still available.");
            }
        }
        Err(e) => {
            eprintln!("Warning: [run] entry failed: {e}");
            eprintln!("The REPL is still available.");
        }
    }
}

/// Display the result of a reload operation.
pub(crate) fn display_reload_result(response: &ReplResponse, module_name: Option<&str>) {
    if response.is_error() {
        if let Some(msg) = response.error_message() {
            eprintln!("{}", format_error(msg));
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

/// Auto-compile a package if `beamtalk.toml` is present in the project root.
///
/// Returns a list of extra code paths to add to the workspace node.
/// On compile failure, prints errors but returns an empty list so the REPL
/// can still start without the package classes.
fn auto_compile_package(project_root: &Path) -> Vec<PathBuf> {
    // Parse manifest to get package name for summary message
    let Some(project_root_utf8) = camino::Utf8Path::from_path(project_root) else {
        eprintln!("Warning: project path is not valid UTF-8, skipping auto-compile");
        return Vec::new();
    };

    let pkg_manifest = match crate::commands::manifest::find_manifest(project_root_utf8) {
        Ok(Some(pkg)) => pkg,
        Ok(None) => return Vec::new(),
        Err(e) => {
            eprintln!("Warning: failed to parse beamtalk.toml: {e}");
            eprintln!("Starting REPL without package classes.");
            return Vec::new();
        }
    };

    let options = beamtalk_core::CompilerOptions::default();
    match crate::commands::build::build(project_root_utf8.as_str(), &options) {
        Ok(()) => {
            let ebin_path = project_root_utf8.join("_build").join("dev").join("ebin");
            // Count .beam files for summary
            let beam_count = std::fs::read_dir(&ebin_path)
                .map(|entries| {
                    entries
                        .filter_map(std::result::Result::ok)
                        .filter(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
                        .count()
                })
                .unwrap_or(0);
            println!(
                "Compiled {} v{} ({} {})",
                pkg_manifest.name,
                pkg_manifest.version,
                beam_count,
                if beam_count == 1 { "class" } else { "classes" }
            );
            vec![ebin_path.into_std_path_buf()]
        }
        Err(e) => {
            eprintln!("Warning: package compilation failed: {e}");
            eprintln!("Starting REPL without package classes.");
            Vec::new()
        }
    }
}

/// Resolve the TLS `ssl_dist.conf` path for a workspace, if `--tls` is enabled.
///
/// Returns `Some(path)` when TLS is requested and certs are found,
/// `None` when TLS is not requested, or an error if certs are missing.
fn resolve_ssl_dist_conf(
    project_root: &Path,
    workspace_name: Option<&str>,
    tls: bool,
) -> Result<Option<PathBuf>> {
    if !tls {
        return Ok(None);
    }
    let workspace_id = workspace::workspace_id_for_project(project_root, workspace_name)?;
    match super::tls::ssl_dist_conf_path(&workspace_id)? {
        Some(path) => {
            println!("🔒 TLS distribution enabled");
            Ok(Some(path))
        }
        None => Err(miette!(
            "TLS certificates not found. Run `beamtalk tls init` first."
        )),
    }
}

/// Public wrapper for resolving TLS distribution config for workspace startup.
///
/// Used by `workspace create --background` to resolve TLS config without
/// going through the REPL startup flow.
pub fn resolve_ssl_dist_conf_for_workspace(
    project_root: &Path,
    workspace_name: Option<&str>,
) -> Result<Option<PathBuf>> {
    resolve_ssl_dist_conf(project_root, workspace_name, true)
}

/// Read the Erlang default cookie from ~/.erlang.cookie.
/// Used for foreground mode where no workspace cookie exists.
fn read_erlang_cookie() -> Option<String> {
    let home = dirs::home_dir()?;
    let cookie_path = home.join(".erlang.cookie");
    let cookie = std::fs::read_to_string(cookie_path)
        .ok()?
        .trim()
        .to_string();
    if cookie.is_empty() {
        None
    } else {
        Some(cookie)
    }
}

/// Decide whether ephemeral cleanup should stop a workspace node.
/// Returns true when ephemeral flag is set and there is no local BEAM child guard (detached workspace).
pub(crate) fn should_stop_workspace(ephemeral: bool, beam_guard_present: bool) -> bool {
    ephemeral && !beam_guard_present
}

/// Start the interactive REPL session.
///
/// Connects to (or spawns) a workspace BEAM node, then enters the
/// read-eval-print loop using rustyline for line editing and history.
#[expect(
    clippy::too_many_lines,
    reason = "REPL main loop handles many commands"
)]
#[expect(
    clippy::too_many_arguments,
    reason = "CLI flags are passed individually from clap"
)]
#[expect(
    clippy::fn_params_excessive_bools,
    reason = "bools map directly to CLI flags (foreground, persistent, no_color, confirm_network, tls, web)"
)]
pub fn run(
    port_arg: Option<u16>,
    node_arg: Option<String>,
    foreground: bool,
    workspace_name: Option<&str>,
    persistent: bool,
    ephemeral: bool,
    timeout: Option<u64>,
    no_color: bool,
    bind: Option<&str>,
    confirm_network: bool,
    tls: bool,
    web: bool,
    web_port: Option<u16>,
) -> Result<()> {
    // Initialize color support
    color::init(no_color);

    // BT-689: Reject --web-port 0 (ephemeral port cannot be reported to user)
    if web_port == Some(0) {
        return Err(miette!(
            "--web-port must be > 0 (use --web without --web-port to reuse the REPL port)"
        ));
    }

    // Resolve bind address: --bind flag → IP address (ADR 0020)
    let bind_addr = resolve_bind_addr(bind)?;
    validate_network_binding(bind_addr, confirm_network)?;

    // Resolve port and node name using priority logic
    let port = resolve_port(port_arg)?;

    // Node name: CLI --node > BEAMTALK_NODE_NAME > None
    let node_name = resolve_node_name(node_arg);

    println!("Beamtalk v{}", env!("CARGO_PKG_VERSION"));
    println!("Type :help for available commands, :exit to quit.");
    println!();

    // Discover project root for BEAM working directory
    let current_dir = std::env::current_dir().into_diagnostic()?;
    let project_root = workspace::discovery::discover_project_root(&current_dir);

    // Choose startup mode: workspace (default) or foreground (debug)
    let (beam_guard_opt, is_new_workspace, workspace_id_opt, connect_host, connect_port, cookie): (
        Option<BeamChildGuard>,
        bool,
        Option<String>,
        String,
        u16,
        String,
    ) = if foreground {
        // Foreground mode: start node directly (original behavior)
        println!("Starting BEAM node in foreground mode (--foreground)...");

        // Resolve TLS config for foreground mode (ADR 0020 Phase 2)
        let ssl_dist_conf = resolve_ssl_dist_conf(&project_root, workspace_name, tls)?;

        let mut child = start_beam_node(
            port,
            node_name.as_ref(),
            &project_root,
            Some(bind_addr),
            ssl_dist_conf.as_deref(),
            web_port,
        )?;

        // Discover the actual port from the BEAM node's stdout.
        // The BEAM prints "BEAMTALK_PORT:<port>" after binding.
        let actual_port = if port == 0 {
            read_port_from_child(&mut child)?
        } else {
            port
        };

        // Foreground mode: use Erlang cookie, or "nocookie" for nodes without -setcookie
        let fg_cookie = read_erlang_cookie().unwrap_or_else(|| "nocookie".to_string());

        // Foreground: compute connect host from bind_addr (BT-694)
        let fg_host = if bind_addr == std::net::Ipv4Addr::UNSPECIFIED {
            "127.0.0.1".to_string()
        } else {
            bind_addr.to_string()
        };

        (
            Some(BeamChildGuard { child }),
            true,
            None,
            fg_host,
            actual_port,
            fg_cookie,
        )
    } else {
        // Workspace mode: start or connect to detached node

        // Use the same runtime paths as foreground mode
        let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()?;
        let paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);

        // Warn if stdlib is not compiled (directory may exist without .beam files)
        if !beamtalk_cli::repl_startup::has_beam_files(&paths.stdlib_ebin) {
            warn!(
                "Stdlib not compiled — run `beamtalk build-stdlib` to enable stdlib classes in REPL"
            );
        }

        // Auto-compile package if beamtalk.toml is present (BT-606)
        let mut extra_code_paths = auto_compile_package(&project_root);
        // cowboy/cowlib/ranch are needed for the WebSocket transport (ADR 0020)
        extra_code_paths.push(paths.cowboy_ebin.clone());
        extra_code_paths.push(paths.cowlib_ebin.clone());
        extra_code_paths.push(paths.ranch_ebin.clone());

        // Resolve TLS config for workspace mode (ADR 0020 Phase 2)
        let ssl_dist_conf = resolve_ssl_dist_conf(&project_root, workspace_name, tls)?;

        let (node_info, is_new, workspace_id) = workspace::get_or_start_workspace(
            &project_root,
            workspace_name,
            port,
            &paths.runtime_ebin,
            &paths.workspace_ebin,
            &paths.jsx_ebin,
            &paths.compiler_ebin,
            &paths.stdlib_ebin,
            &extra_code_paths,
            !persistent, // auto_cleanup is opposite of persistent flag
            timeout,
            Some(bind_addr),
            ssl_dist_conf.as_deref(),
            web_port,
        )?;

        let actual_port = node_info.port;

        if is_new {
            tracing::debug!("Started new workspace node: {}", node_info.node_name);
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
            if tls {
                eprintln!(
                    "  ⚠️  --tls has no effect on an already-running workspace.\n  \
                     Stop the workspace first with `beamtalk workspace stop` to restart with TLS."
                );
            }
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

        // Read workspace cookie for WebSocket authentication (ADR 0020)
        let ws_cookie = workspace::read_workspace_cookie(&workspace_id)?
            .trim()
            .to_string();

        println!();

        (
            None,
            is_new,
            Some(workspace_id),
            node_info.connect_host().to_string(),
            actual_port,
            ws_cookie,
        ) // No guard needed - node is detached
    };

    // Connect to REPL backend
    let mut client = connect_with_retries(&connect_host, connect_port, &cookie)?;

    println!("Connected to REPL backend on port {connect_port}.");

    // BT-689: Print browser workspace URL when --web flag is used
    if web {
        if !is_new_workspace && beam_guard_opt.is_none() {
            // Reconnecting to existing workspace — warn that --web/--web-port
            // have no effect on already-running nodes
            eprintln!(
                "  ⚠️  --web has no effect on an already-running workspace.\n  \
                 Stop the workspace first with `beamtalk workspace stop` to restart with --web."
            );
        } else {
            let browser_port = web_port.unwrap_or(connect_port);
            let browser_host = if connect_host == "127.0.0.1" || connect_host == "0.0.0.0" {
                "localhost".to_string()
            } else {
                connect_host.clone()
            };
            println!("Browser workspace: http://{browser_host}:{browser_port}/");
        }
    }

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

    // Auto-run entry expression from [run] section of beamtalk.toml (BT-740)
    auto_run_entry(&mut client, &project_root);

    println!();

    // Enter the shared REPL loop (also used by `beamtalk attach`)
    let repl_res = repl_loop(&mut client, &connect_host, connect_port, &cookie);

    // BEAM child is cleaned up automatically by BeamChildGuard::drop()

    // Ephemeral mode: stop workspace node on REPL exit (only in workspace mode)
    if should_stop_workspace(ephemeral, beam_guard_opt.is_some()) {
        if let Some(workspace_id) = workspace_id_opt {
            if let Err(e) = crate::commands::workspace::stop_workspace(Some(&workspace_id), false) {
                eprintln!("Warning: failed to stop workspace {workspace_id}: {e}");
            }
        }
    }

    repl_res?;

    Ok(())
}

#[cfg(test)]
mod ephemeral_tests {
    use super::should_stop_workspace;

    #[test]
    fn stops_when_ephemeral_and_no_guard() {
        assert!(should_stop_workspace(true, false));
    }

    #[test]
    fn not_stop_when_not_ephemeral() {
        assert!(!should_stop_workspace(false, false));
    }

    #[test]
    fn not_stop_when_guard_present() {
        assert!(!should_stop_workspace(true, true));
    }
}

/// Display the output, warnings, errors, and value from an eval response.
fn display_eval_response(response: &ReplResponse) {
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
            if msg == "Interrupted" {
                // BT-666: Clean interrupt message
                eprintln!("{msg}");
            } else {
                eprintln!("{}", format_error(msg));
            }
        }
    } else if let Some(ref value) = response.value {
        println!("{}", format_value(value));
    }
}

/// Shared REPL loop used by both `beamtalk repl` and `beamtalk attach`.
///
/// Sets up rustyline with tab completion and history, registers the SIGINT
/// handler, then enters the read-eval-print loop. Returns when the user
/// types `:exit` / `:quit`, presses Ctrl-D, or a communication error occurs.
#[expect(
    clippy::too_many_lines,
    reason = "REPL main loop handles many commands"
)]
pub(crate) fn repl_loop(
    client: &mut ReplClient,
    host: &str,
    port: u16,
    cookie: &str,
) -> Result<()> {
    // Set up rustyline editor with tab completion and syntax highlighting
    let config = Config::builder()
        .completion_type(CompletionType::List)
        .build();
    let helper = ReplHelper::new(host, port, cookie);
    let mut rl: Editor<ReplHelper, FileHistory> = Editor::with_config(config).into_diagnostic()?;
    rl.set_helper(Some(helper));

    // Load history
    let history_file = history_path()?;
    let _ = rl.load_history(&history_file);

    // BT-666: Register SIGINT handler for interrupt during eval.
    // Uses signal-hook to non-destructively register alongside rustyline's handler.
    let interrupted = Arc::new(AtomicBool::new(false));
    let _ = signal_hook::flag::register(signal_hook::consts::SIGINT, Arc::clone(&interrupted));

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
                    // Add commands to history immediately. Expression history
                    // is deferred until input is complete (avoids duplicates
                    // for multi-line expressions).
                    if line.starts_with(':') {
                        let _ = rl.add_history_entry(line);
                    }

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
                            let args = extract_command_arg(line, ":help ", Some(":h "));

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
                            let raw = extract_command_arg(line, ":load ", Some(":l "));
                            let path = strip_path_quotes(raw);

                            if path.is_empty() {
                                eprintln!("Usage: :load <path>");
                                continue;
                            }

                            if Path::new(path).is_dir() {
                                // Directory load: recursively find and load all .bt files
                                if load_directory(client, path, "Loaded") {
                                    client.set_last_loaded_directory(path);
                                }
                            } else {
                                match client.load_file(path) {
                                    Ok(response) => {
                                        if response.is_error() {
                                            if let Some(msg) = response.error_message() {
                                                eprintln!("Error: {msg}");
                                            }
                                        } else {
                                            client.set_last_loaded_file(path);
                                            if let Some(classes) = &response.classes {
                                                if classes.is_empty() {
                                                    println!("Loaded {path}");
                                                } else {
                                                    println!("Loaded {}", classes.join(", "));
                                                }
                                            } else {
                                                println!("Loaded {path}");
                                            }
                                        }
                                    }
                                    Err(e) => eprintln!("Error: {e}"),
                                }
                            }
                            continue;
                        }
                        _ if line.starts_with(":reload ") || line.starts_with(":r ") => {
                            let raw = extract_command_arg(line, ":reload ", Some(":r "));
                            let module_name = strip_path_quotes(raw);
                            match client.reload_module(module_name) {
                                Ok(response) => {
                                    display_reload_result(&response, Some(module_name));
                                }
                                Err(e) => eprintln!("Error: {e}"),
                            }
                            continue;
                        }
                        ":reload" | ":r" => {
                            match client.last_loaded_path() {
                                Some(client::LastLoadedPath::Directory(dir)) => {
                                    let dir = dir.clone();
                                    load_directory(client, &dir, "Reloaded");
                                }
                                Some(client::LastLoadedPath::File(path)) => {
                                    let path = path.clone();
                                    match client.load_file(&path) {
                                        Ok(response) => {
                                            display_reload_result(&response, None);
                                        }
                                        Err(e) => eprintln!("Error: {e}"),
                                    }
                                }
                                None => {
                                    eprintln!("No file or directory has been loaded yet");
                                }
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
                        // BT-724: :show-codegen / :sc command
                        ":show-codegen" | ":sc" => {
                            eprintln!("Usage: :show-codegen <expression>");
                            continue;
                        }
                        _ if line.starts_with(":show-codegen ") || line.starts_with(":sc ") => {
                            let code = extract_command_arg(line, ":show-codegen ", Some(":sc "));
                            if code.is_empty() {
                                eprintln!("Usage: :show-codegen <expression>");
                                continue;
                            }
                            match client.show_codegen(code) {
                                Ok(response) => {
                                    if response.is_error() {
                                        if let Some(msg) = response.error_message() {
                                            eprintln!("{}", display::format_error(msg));
                                        }
                                    } else if let Some(ref core) = response.core_erlang {
                                        display::display_codegen(core);
                                    }
                                    if !response.is_error()
                                        && response.core_erlang.is_none()
                                        && response.warnings.is_none()
                                    {
                                        println!("No code generated.");
                                    }
                                    if let Some(ref warns) = response.warnings {
                                        for w in warns {
                                            eprintln!(
                                                "{}",
                                                color::paint(
                                                    color::YELLOW,
                                                    &format!("Warning: {w}")
                                                )
                                            );
                                        }
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
                        "modules" => Some(":modules"),
                        "show-codegen" => Some(":show-codegen"),
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

                // Evaluate expression (BT-666: interruptible via Ctrl-C)
                // Clear any stale interrupt flag before starting eval
                interrupted.store(false, Ordering::SeqCst);
                match client.eval_interruptible(&accumulated, &interrupted) {
                    Ok(response) => display_eval_response(&response),
                    Err(e) => {
                        eprintln!("Communication error: {e}");
                        eprintln!("Attempting to reconnect to REPL backend...");
                        match client.reconnect() {
                            Ok(resumed) => {
                                if resumed {
                                    eprintln!(
                                        "Reconnected (session resumed). Retrying evaluation..."
                                    );
                                } else {
                                    eprintln!("Reconnected (new session). Retrying evaluation...");
                                }
                                // Reset interrupt flag for the retry attempt
                                interrupted.store(false, Ordering::SeqCst);
                                match client.eval_interruptible(&accumulated, &interrupted) {
                                    Ok(response) => display_eval_response(&response),
                                    Err(e2) => {
                                        eprintln!("Communication error after reconnect: {e2}");
                                        eprintln!("The REPL backend may have crashed. Exiting.");
                                        break;
                                    }
                                }
                            }
                            Err(re) => {
                                eprintln!("Reconnect failed: {re}");
                                eprintln!("The REPL backend may have crashed. Exiting.");
                                break;
                            }
                        }
                    }
                }
                line_buffer.clear();
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl+C — cancel multi-line input if buffering, otherwise just newline
                // BT-666: Clear the interrupt flag (signal handler also fires)
                interrupted.store(false, Ordering::SeqCst);
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

    Ok(())
}

/// Extracts the argument from a REPL command with long and optional short forms.
///
/// Returns the trimmed argument string, or an empty string if the command
/// doesn't match either prefix.
pub(crate) fn extract_command_arg<'a>(
    line: &'a str,
    long_prefix: &str,
    short_prefix: Option<&str>,
) -> &'a str {
    line.strip_prefix(long_prefix)
        .or_else(|| short_prefix.and_then(|s| line.strip_prefix(s)))
        .unwrap_or("")
        .trim()
}

/// Strip surrounding double-quotes from a path argument.
///
/// Handles both quoted (`"path/to/file.bt"`) and unquoted (`path/to/file.bt`) forms.
/// Quoted paths support spaces in the path; unquoted paths remain unchanged.
pub(crate) fn strip_path_quotes(arg: &str) -> &str {
    if arg.len() >= 2 && arg.starts_with('"') && arg.ends_with('"') {
        &arg[1..arg.len() - 1]
    } else {
        arg
    }
}

/// Recursively discover all `.bt` files under a directory, sorted by full path.
///
/// Returns file paths relative to the current directory (or absolute if the input was absolute).
/// Hidden directories (starting with `.`) are skipped.
fn discover_bt_files(dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    discover_bt_files_recursive(dir, &mut files)?;
    files.sort();
    Ok(files)
}

/// Recursive helper for [`discover_bt_files`].
fn discover_bt_files_recursive(dir: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
    let mut entries: Vec<_> = std::fs::read_dir(dir)?
        .filter_map(std::result::Result::ok)
        .collect();
    entries.sort_by_key(std::fs::DirEntry::file_name);

    for entry in entries {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();

        // Skip hidden entries
        if name_str.starts_with('.') {
            continue;
        }

        let path = entry.path();
        let file_type = entry.file_type()?;

        if file_type.is_dir() {
            discover_bt_files_recursive(&path, files)?;
        } else if file_type.is_file() && path.extension().is_some_and(|ext| ext == "bt") {
            files.push(path);
        }
    }
    Ok(())
}

/// Load all `.bt` files from a directory into the REPL, reporting progress.
///
/// Returns `true` if at least one file was loaded successfully.
fn load_directory(client: &mut ReplClient, dir_path: &str, verb: &str) -> bool {
    let dir = Path::new(dir_path);

    let files = match discover_bt_files(dir) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Error reading directory {dir_path}: {e}");
            return false;
        }
    };

    if files.is_empty() {
        println!("No .bt files found in {dir_path}");
        return false;
    }

    let mut loaded: usize = 0;
    let mut failed: Vec<String> = Vec::new();

    for file in &files {
        let file_str = file.to_string_lossy();
        match client.load_file(&file_str) {
            Ok(response) => {
                if response.is_error() {
                    let file_name = file
                        .file_name()
                        .map_or_else(|| file_str.to_string(), |n| n.to_string_lossy().to_string());
                    if let Some(msg) = response.error_message() {
                        eprintln!("  Error in {file_name}: {msg}");
                    }
                    failed.push(file_name);
                } else {
                    loaded += 1;
                }
            }
            Err(e) => {
                let file_name = file
                    .file_name()
                    .map_or_else(|| file_str.to_string(), |n| n.to_string_lossy().to_string());
                eprintln!("  Error loading {file_name}: {e}");
                failed.push(file_name);
            }
        }
    }

    // Summary
    if failed.is_empty() {
        let word = if loaded == 1 { "file" } else { "files" };
        println!("{verb} {loaded} {word} from {dir_path}");
    } else if loaded > 0 {
        let word = if loaded == 1 { "file" } else { "files" };
        println!("  \u{2713} {loaded} {word} loaded");
        let fail_word = if failed.len() == 1 { "file" } else { "files" };
        println!(
            "  \u{2717} {} {fail_word} failed ({})",
            failed.len(),
            failed.join(", ")
        );
    } else {
        let word = if failed.len() == 1 { "file" } else { "files" };
        println!(
            "  \u{2717} {} {word} failed ({})",
            failed.len(),
            failed.join(", ")
        );
    }

    loaded > 0
}

#[cfg(test)]
mod tests {
    //! Tests for REPL command logic: value formatting, node name resolution, and process management.
    use super::*;
    use color::ColorGuard;
    use process::DEFAULT_REPL_PORT;
    use serial_test::serial;
    use sysinfo::{Pid, ProcessRefreshKind, ProcessesToUpdate, System};

    #[test]
    #[serial(color)]
    fn format_value_string() {
        let _guard = ColorGuard::disabled();
        let value = serde_json::json!("hello");
        assert_eq!(format_value(&value), "hello");
    }

    #[test]
    #[serial(color)]
    fn format_value_number() {
        let _guard = ColorGuard::disabled();
        let value = serde_json::json!(42);
        assert_eq!(format_value(&value), "42");
    }

    #[test]
    #[serial(color)]
    fn format_value_bool() {
        let _guard = ColorGuard::disabled();
        let value = serde_json::json!(true);
        assert_eq!(format_value(&value), "true");
    }

    #[test]
    #[serial(color)]
    fn format_value_array() {
        let _guard = ColorGuard::disabled();
        let value = serde_json::json!([1, 2, 3]);
        assert_eq!(format_value(&value), "[1, 2, 3]");
    }

    #[test]
    #[serial(color)]
    fn format_value_pid() {
        let _guard = ColorGuard::disabled();
        // Backend now pre-formats pids as "#Actor<pid>"
        let value = serde_json::json!("#Actor<0.123.0>");
        assert_eq!(format_value(&value), "#Actor<0.123.0>");
    }

    #[test]
    #[serial(color)]
    fn format_value_block() {
        let _guard = ColorGuard::disabled();
        // Blocks are formatted as "a Block/N" by the backend
        let value = serde_json::json!("a Block/1");
        assert_eq!(format_value(&value), "a Block/1");

        let value2 = serde_json::json!("a Block/2");
        assert_eq!(format_value(&value2), "a Block/2");
    }

    #[test]
    #[serial(color)]
    fn format_value_tuple() {
        let _guard = ColorGuard::disabled();
        // BT-536: Tuples are pre-formatted as strings by the backend
        let value = serde_json::json!("{1, hello}");
        assert_eq!(format_value(&value), "{1, hello}");
    }

    #[test]
    #[serial(color)]
    fn format_error_with_color_disabled() {
        let _guard = ColorGuard::disabled();
        assert_eq!(format_error("test error"), "Error: test error");
    }

    #[test]
    #[serial(color)]
    fn format_error_with_color_enabled() {
        let _guard = ColorGuard::enabled();
        let result = format_error("test error");
        assert!(result.contains("Error:"));
        assert!(result.contains("test error"));
        assert!(result.contains(color::RED));
        assert!(result.contains(color::RESET));
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
        // Spawn a long-running process (cross-platform sleep using a simple program)
        // We'll use a BEAM process since that's what we actually manage
        #[cfg(windows)]
        let child = std::process::Command::new("timeout")
            .args(["/t", "60", "/nobreak"])
            .spawn()
            .expect("Failed to spawn timeout process");

        #[cfg(not(windows))]
        let child = std::process::Command::new("sleep")
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

        // Verify the process is no longer running using sysinfo
        let mut system = System::new();
        system.refresh_processes_specifics(
            ProcessesToUpdate::Some(&[Pid::from_u32(pid)]),
            true,
            ProcessRefreshKind::nothing(),
        );
        assert!(
            system.process(Pid::from_u32(pid)).is_none(),
            "Process should have been killed"
        );
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
    #[serial(env_var)]
    fn resolve_node_name_none_without_env() {
        // When no CLI flag and no env var, should return None
        // SAFETY: This test runs single-threaded via #[serial], restoring env var after
        unsafe { std::env::remove_var("BEAMTALK_NODE_NAME") };
        let result = resolve_node_name(None);
        assert!(result.is_none());
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

    #[test]
    fn extract_command_arg_long_prefix() {
        assert_eq!(
            extract_command_arg(":load foo.bt", ":load ", Some(":l ")),
            "foo.bt"
        );
    }

    #[test]
    fn extract_command_arg_short_prefix() {
        assert_eq!(
            extract_command_arg(":l foo.bt", ":load ", Some(":l ")),
            "foo.bt"
        );
    }

    #[test]
    fn extract_command_arg_trims_whitespace() {
        assert_eq!(
            extract_command_arg(":load   foo.bt  ", ":load ", Some(":l ")),
            "foo.bt"
        );
    }

    #[test]
    fn extract_command_arg_no_short_prefix() {
        assert_eq!(
            extract_command_arg(":unload counter", ":unload ", None),
            "counter"
        );
    }

    #[test]
    fn extract_command_arg_no_match_returns_empty() {
        assert_eq!(extract_command_arg(":other cmd", ":load ", Some(":l ")), "");
    }

    #[test]
    fn extract_command_arg_empty_argument() {
        // Command with trailing space but no argument
        assert_eq!(extract_command_arg(":load ", ":load ", Some(":l ")), "");
    }

    #[test]
    fn extract_command_arg_whitespace_only_argument() {
        assert_eq!(extract_command_arg(":inspect   ", ":inspect ", None), "");
    }

    #[test]
    fn strip_path_quotes_removes_surrounding_quotes() {
        assert_eq!(
            strip_path_quotes("\"examples/counter.bt\""),
            "examples/counter.bt"
        );
    }

    #[test]
    fn strip_path_quotes_removes_quotes_with_spaces() {
        assert_eq!(
            strip_path_quotes("\"path with spaces/file.bt\""),
            "path with spaces/file.bt"
        );
    }

    #[test]
    fn strip_path_quotes_unquoted_unchanged() {
        assert_eq!(
            strip_path_quotes("examples/counter.bt"),
            "examples/counter.bt"
        );
    }

    #[test]
    fn strip_path_quotes_empty_string() {
        assert_eq!(strip_path_quotes(""), "");
    }

    #[test]
    fn strip_path_quotes_only_opening_quote() {
        assert_eq!(strip_path_quotes("\"foo.bt"), "\"foo.bt");
    }

    #[test]
    fn strip_path_quotes_only_closing_quote() {
        assert_eq!(strip_path_quotes("foo.bt\""), "foo.bt\"");
    }

    #[test]
    fn strip_path_quotes_empty_quoted() {
        assert_eq!(strip_path_quotes("\"\""), "");
    }

    // === discover_bt_files tests ===

    #[test]
    fn discover_bt_files_finds_bt_files_sorted() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("beta.bt"), "// beta").unwrap();
        std::fs::write(dir.path().join("alpha.bt"), "// alpha").unwrap();
        std::fs::write(dir.path().join("readme.md"), "# readme").unwrap();

        let files = discover_bt_files(dir.path()).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files[0].ends_with("alpha.bt"));
        assert!(files[1].ends_with("beta.bt"));
    }

    #[test]
    fn discover_bt_files_recursive() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("top.bt"), "// top").unwrap();
        let sub = dir.path().join("sub");
        std::fs::create_dir(&sub).unwrap();
        std::fs::write(sub.join("nested.bt"), "// nested").unwrap();

        let files = discover_bt_files(dir.path()).unwrap();
        assert_eq!(files.len(), 2);
        // sub/nested.bt sorts before top.bt
        assert!(files[0].ends_with("nested.bt"));
        assert!(files[1].ends_with("top.bt"));
    }

    #[test]
    fn discover_bt_files_skips_hidden_dirs() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("visible.bt"), "// vis").unwrap();
        let hidden = dir.path().join(".hidden");
        std::fs::create_dir(&hidden).unwrap();
        std::fs::write(hidden.join("secret.bt"), "// secret").unwrap();

        let files = discover_bt_files(dir.path()).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].ends_with("visible.bt"));
    }

    #[test]
    fn discover_bt_files_empty_dir() {
        let dir = tempfile::tempdir().unwrap();
        let files = discover_bt_files(dir.path()).unwrap();
        assert!(files.is_empty());
    }

    #[test]
    fn discover_bt_files_nonexistent_dir() {
        let result = discover_bt_files(Path::new("/nonexistent/path/abc123"));
        assert!(result.is_err());
    }
}
