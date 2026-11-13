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
use tracing::warn;

pub(crate) use beamtalk_repl_protocol::ReplResponse;

// Re-export response payload types used in tests.
#[cfg(test)]
use beamtalk_repl_protocol::ActorInfo;

use crate::commands::workspace;

use beamtalk_core::source_analysis::{is_input_complete, needs_blank_line_to_complete};

/// Maximum retries when connecting to REPL backend.
const MAX_CONNECT_RETRIES: u32 = 10;

/// Delay between connection retries in milliseconds.
const RETRY_DELAY_MS: u64 = 500;

pub(crate) mod client;
pub(crate) mod color;
pub(crate) mod commands;
pub(crate) mod display;
mod handlers;
use handlers::{CommandResult, handle_repl_command};
pub(crate) mod helper;
pub(crate) mod process;

pub mod bind;

use bind::{resolve_bind_addr, validate_network_binding};
use client::ReplClient;
use display::{format_error, format_value, history_path, output_mode};
use helper::ReplHelper;
use process::{
    BeamChildGuard, connect_with_retries, drain_child_stderr, read_port_from_child,
    resolve_node_name, resolve_port, start_beam_node,
};

// Response types re-exported from beamtalk-repl-protocol above.

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
    match crate::commands::build::build(project_root_utf8.as_str(), &options, false) {
        Ok(()) => {
            let repl_layout = crate::commands::build_layout::BuildLayout::new(project_root_utf8);
            let ebin_path = repl_layout.ebin_dir();
            // Count .beam files for summary
            let beam_count = std::fs::read_dir(&ebin_path).map_or(0, |entries| {
                entries
                    .filter_map(std::result::Result::ok)
                    .filter(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
                    .count()
            });
            println!(
                "Compiled {} v{} ({} {})",
                pkg_manifest.name,
                pkg_manifest.version,
                beam_count,
                if beam_count == 1 { "class" } else { "classes" }
            );

            // Use BeamEnvironment for unified code path collection.
            // from_layout may fail if the lockfile is corrupt; treat as compile failure.
            match crate::commands::beam_environment::BeamEnvironment::from_layout(
                &repl_layout,
                project_root_utf8,
            ) {
                Ok(env) => env.code_paths_std(),
                Err(e) => {
                    eprintln!("Warning: failed to collect BEAM environment: {e}");
                    // Fall back to just the package ebin
                    vec![ebin_path.into_std_path_buf()]
                }
            }
        }
        Err(e) => {
            eprintln!("Warning: package compilation failed: {e}");
            eprintln!("Starting REPL without package classes.");
            Vec::new()
        }
    }
}

/// Collect hex dep names from the lockfile for a project directory.
///
/// Returns an empty vec if the path is not valid UTF-8 or the lockfile
/// doesn't exist. Propagates lockfile parse errors so REPL startup
/// doesn't silently miss required OTP apps.
fn collect_hex_dep_names_for_project(project_root: &std::path::Path) -> Result<Vec<String>> {
    let Some(root) = camino::Utf8Path::from_path(project_root) else {
        return Ok(Vec::new());
    };
    crate::commands::deps::lockfile::Lockfile::collect_hex_dep_names(root)
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
///
/// Returns true only when the ephemeral flag is set, there is no local BEAM
/// child guard (i.e. the workspace is detached rather than `--foreground`),
/// AND this invocation is the one that spawned the workspace
/// (`is_new_workspace`). An ephemeral REPL that merely *attached* to an
/// already-running, persistent workspace must never tear it down on exit —
/// only the invocation that started the node owns its lifecycle.
pub(crate) fn should_stop_workspace(
    ephemeral: bool,
    beam_guard_present: bool,
    is_new_workspace: bool,
) -> bool {
    ephemeral && !beam_guard_present && is_new_workspace
}

/// Decide whether to enter the interactive (rustyline) REPL loop.
///
/// Only when stdin is an interactive terminal. For any non-TTY stdin (a pipe, a
/// file redirect, or the null device) the workspace node is already started and
/// its port printed, so there is nothing for an interactive loop to do — and
/// entering it hangs on Windows, where rustyline does not return EOF for a
/// redirected stdin handle the way it does on Unix.
pub(crate) fn should_enter_interactive_repl(stdin_is_terminal: bool) -> bool {
    stdin_is_terminal
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
    reason = "bools map directly to CLI flags (foreground, persistent, no_color, confirm_network)"
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
    log_level: &str,
) -> Result<()> {
    // Initialize color support
    color::init(no_color);

    // Validate log level against OTP logger levels (also prevents eval injection)
    {
        const VALID: &[&str] = &[
            "emergency",
            "alert",
            "critical",
            "error",
            "warning",
            "notice",
            "info",
            "debug",
        ];
        if !VALID.contains(&log_level) {
            return Err(miette!("--log-level must be one of: {}", VALID.join(", ")));
        }
    }

    // Resolve bind address: --bind flag → IP address (ADR 0020)
    let bind_addr = resolve_bind_addr(bind)?;
    validate_network_binding(bind_addr, confirm_network)?;

    // Resolve port and node name using priority logic
    let port = resolve_port(port_arg)?;

    // Node name: CLI --node > BEAMTALK_NODE_NAME > None
    let node_name = resolve_node_name(node_arg);

    println!("Beamtalk v{}", env!("BEAMTALK_VERSION"));
    println!("Type :help for available commands, :exit to quit.");
    println!();

    // Discover project root for BEAM working directory
    let current_dir = std::env::current_dir().into_diagnostic()?;
    let project_root = beamtalk_project::discover_project_root(&current_dir);

    // Read [application] config from beamtalk.toml so the OTP application
    // supervisor is started after workspace bootstrap.
    // The OTP app atom is the Erlang application name derived from the package name.
    let otp_app_name: Option<String> =
        camino::Utf8Path::from_path(&project_root).and_then(|root| {
            let manifest = match crate::commands::manifest::find_manifest(root) {
                Ok(Some(m)) => m,
                Ok(None) => return None,
                Err(e) => {
                    warn!("Failed to read beamtalk.toml: {e}");
                    return None;
                }
            };
            match crate::commands::manifest::find_application_config(root) {
                Ok(Some(_app)) => Some(manifest.name),
                Ok(None) => None,
                Err(e) => {
                    warn!("Failed to read [application] config: {e}");
                    None
                }
            }
        });

    // The program's root package, from the Rust-parsed manifest (BT-3651).
    // Recorded as the `root_package` runtime fact in the workspace node; a
    // bare `beamtalk repl` outside a project records none.
    let root_package: Option<String> = camino::Utf8Path::from_path(&project_root)
        .and_then(|root| {
            crate::commands::manifest::find_manifest(root)
                .ok()
                .flatten()
        })
        .map(|m| m.name);

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

        // ADR 0072: Collect hex dep names from lockfile (includes transitive deps)
        let hex_dep_names = collect_hex_dep_names_for_project(&project_root)?;

        let fg_config = workspace::WorkspaceConfig {
            port,
            bind_addr: Some(bind_addr),
            auto_cleanup: false,    // not applicable in foreground mode
            max_idle_seconds: None, // not applicable in foreground mode
            log_level,
            otp_app_name: otp_app_name.as_deref(),
            root_package: root_package.as_deref(),
            hex_dep_names: &hex_dep_names,
        };

        let mut child = start_beam_node(node_name.as_ref(), &project_root, &fg_config)?;

        // Discover the actual port from the BEAM node's stdout.
        // The BEAM prints "BEAMTALK_PORT:<port>" after binding.
        let actual_port = if port == 0 {
            read_port_from_child(&mut child)?
        } else {
            port
        };

        // Drain stderr in a background thread to prevent pipe buffer fill-up
        // and capture OTP logger/VM diagnostic output.
        let _stderr_drain = drain_child_stderr(&mut child);

        // Foreground mode: use Erlang cookie, or "nocookie" for nodes without -setcookie
        let fg_cookie = read_erlang_cookie().unwrap_or_else(|| "nocookie".to_string());

        // Foreground: compute connect host from bind_addr
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

        // Auto-compile package if beamtalk.toml is present
        let extra_code_paths = auto_compile_package(&project_root);

        // ADR 0072: Collect hex dep names AFTER auto-compile, which may
        // create/update beamtalk.lock with resolved native packages.
        let hex_dep_names = collect_hex_dep_names_for_project(&project_root)?;

        let ws_config = workspace::WorkspaceConfig {
            port,
            bind_addr: Some(bind_addr),
            auto_cleanup: !persistent, // auto_cleanup is opposite of persistent flag
            max_idle_seconds: timeout,
            log_level,
            otp_app_name: otp_app_name.as_deref(),
            root_package: root_package.as_deref(),
            hex_dep_names: &hex_dep_names,
        };

        let (node_info, is_new, workspace_id) = workspace::get_or_start_workspace(
            &project_root,
            workspace_name,
            &paths,
            &extra_code_paths,
            &ws_config,
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
            std::thread::sleep(Duration::from_secs(2));
        } else {
            println!("✓ Connected to existing workspace: {}", node_info.node_name);
            if timeout.is_some() {
                eprintln!(
                    "  ⚠️  Workspace already running (timeout unchanged). \
                     Use `beamtalk workspace stop` to restart with a new timeout."
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
    if let Some(session_id) = client.session_id() {
        println!("[beamtalk] session: {session_id}");
    } else {
        eprintln!(
            "[beamtalk] warning: REPL backend did not provide a session id; \
             editor integrations (e.g., VS Code) may not function correctly."
        );
        warn!("REPL backend connected without session id; stdout session contract not satisfied");
    }

    // If reconnecting to existing workspace, show available actors
    if beam_guard_opt.is_none() && !is_new_workspace {
        match client.list_actors() {
            Ok(response) => {
                if let Some(actors) = response.actors
                    && !actors.is_empty()
                {
                    println!("\nAvailable actors:");
                    for actor in &actors {
                        println!(
                            "  - {}",
                            beamtalk_repl_protocol::format::format_actor_summary(
                                actor,
                                output_mode(),
                            )
                        );
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

    // Enter the shared interactive REPL loop (also used by `beamtalk workspace
    // attach`) only when stdin is an interactive terminal. For ANY non-TTY stdin
    // — a pipe, a file redirect, or the null device — we skip it: by this point
    // the detached workspace node is already up and its port has been printed,
    // so a non-interactive invocation (e.g. `beamtalk-mcp --start`, which runs
    // `beamtalk repl --port 0` with stdin redirected from null) has nothing more
    // to do here. Non-interactive use is served by the port-based protocol and
    // `beamtalk run`/`exec`, not by piping source into the REPL's stdin.
    //
    // This also avoids a Windows-only hang. On Unix, rustyline returns
    // EOF for a redirected/null stdin, so entering the loop would immediately
    // hit EOF and exit — skipping it here for a piped `echo expr | beamtalk
    // repl` is an intentional, undocumented-path shortcut with no
    // user-visible difference. On Windows rustyline does NOT return EOF for
    // a redirected stdin handle, so `repl_loop` would block forever:
    // `beamtalk repl` would never exit and the parent `beamtalk-mcp
    // --start` (which waits on it) would block until its MCP `initialize` timed out.
    // Gating on a TTY makes the command exit promptly on every platform; the
    // interactive TTY path is unchanged.
    let repl_res =
        if should_enter_interactive_repl(std::io::IsTerminal::is_terminal(&std::io::stdin())) {
            repl_loop(&mut client, &connect_host, connect_port, &cookie)
        } else {
            tracing::debug!(
                "stdin is not a terminal — workspace is started; skipping interactive REPL loop"
            );
            Ok(())
        };

    // BEAM child is cleaned up automatically by BeamChildGuard::drop()

    // Ephemeral mode: stop workspace node on REPL exit (only in workspace mode)
    if should_stop_workspace(ephemeral, beam_guard_opt.is_some(), is_new_workspace)
        && let Some(workspace_id) = workspace_id_opt
        && let Err(e) = crate::commands::workspace::stop_workspace(Some(&workspace_id), false)
    {
        eprintln!("Warning: failed to stop workspace {workspace_id}: {e}");
    }

    repl_res?;

    Ok(())
}

#[cfg(test)]
mod ephemeral_tests {
    use super::should_stop_workspace;

    #[test]
    fn stops_when_ephemeral_new_workspace_and_no_guard() {
        assert!(should_stop_workspace(true, false, true));
    }

    #[test]
    fn not_stop_when_not_ephemeral() {
        assert!(!should_stop_workspace(false, false, true));
    }

    #[test]
    fn not_stop_when_guard_present() {
        assert!(!should_stop_workspace(true, true, true));
    }

    /// `beamtalk repl -e` against a pre-existing/persistent
    /// workspace (this invocation only attached, it didn't spawn the node)
    /// must never send a shutdown request on exit.
    #[test]
    fn not_stop_when_ephemeral_but_attached_to_existing_workspace() {
        assert!(!should_stop_workspace(true, false, false));
    }
}

#[cfg(test)]
mod interactive_repl_tests {
    use super::should_enter_interactive_repl;

    #[test]
    fn enters_loop_on_a_tty() {
        assert!(should_enter_interactive_repl(true));
    }

    #[test]
    fn skips_loop_when_stdin_not_a_terminal() {
        // This guard: a pipe / redirect / null stdin must NOT enter the
        // interactive loop (it hangs on Windows). The workspace is already up.
        assert!(!should_enter_interactive_repl(false));
    }
}

#[cfg(test)]
mod timeout_warning_tests {
    /// Helper: returns true when timeout warning should be shown.
    /// Warning is shown when timeout was explicitly provided AND workspace was already running.
    fn should_warn_timeout(timeout: Option<u64>, is_new: bool) -> bool {
        timeout.is_some() && !is_new
    }

    #[test]
    fn warns_when_timeout_provided_and_workspace_existing() {
        assert!(should_warn_timeout(Some(300), false));
    }

    #[test]
    fn no_warn_when_timeout_not_provided() {
        assert!(!should_warn_timeout(None, false));
    }

    #[test]
    fn no_warn_when_new_workspace_started() {
        assert!(!should_warn_timeout(Some(300), true));
    }

    #[test]
    fn no_warn_when_new_workspace_and_no_timeout() {
        assert!(!should_warn_timeout(None, true));
    }
}

/// Send an expression for evaluation and display the response.
///
/// Convenience wrapper used by several REPL command handlers.
fn eval_and_display(client: &mut ReplClient, expr: &str) {
    match client.eval(expr) {
        Ok(response) => display_eval_response(&response),
        Err(e) => eprintln!("Error: {e}"),
    }
}

/// Evaluate with reconnect-on-failure logic.
///
/// Returns `true` if the REPL loop should break (fatal communication error).
fn eval_with_reconnect(
    client: &mut ReplClient,
    rl: &Editor<ReplHelper, FileHistory>,
    accumulated: &str,
    interrupted: &Arc<AtomicBool>,
) -> bool {
    interrupted.store(false, Ordering::SeqCst);
    match client.eval_interruptible(accumulated, interrupted) {
        Ok(response) => {
            display_eval_response(&response);
            false
        }
        Err(e) => {
            eprintln!("Communication error: {e}");
            eprintln!("Attempting to reconnect to REPL backend...");
            attempt_reconnect_and_retry(client, rl, accumulated, interrupted)
        }
    }
}

/// Try to reconnect and retry evaluation after a communication error.
///
/// Returns `true` if the REPL loop should break (fatal error).
fn attempt_reconnect_and_retry(
    client: &mut ReplClient,
    rl: &Editor<ReplHelper, FileHistory>,
    accumulated: &str,
    interrupted: &Arc<AtomicBool>,
) -> bool {
    match client.reconnect() {
        Ok(resumed) => {
            if resumed {
                eprintln!("Reconnected (session resumed). Retrying evaluation...");
            } else {
                eprintln!("Reconnected (new session). Retrying evaluation...");
                // Emit updated session line so external consumers
                // (e.g. VS Code) can track the new session ID.
                if let Some(session_id) = client.session_id() {
                    println!("[beamtalk] session: {session_id}");
                }
            }
            // Keep the completion helper's session ID in sync
            if let Some(h) = rl.helper() {
                h.update_session_id(client.session_id());
            }
            // Reset interrupt flag for the retry attempt
            interrupted.store(false, Ordering::SeqCst);
            match client.eval_interruptible(accumulated, interrupted) {
                Ok(response) => {
                    display_eval_response(&response);
                    false
                }
                Err(e2) => {
                    eprintln!("Communication error after reconnect: {e2}");
                    eprintln!("The REPL backend may have crashed. Exiting.");
                    true
                }
            }
        }
        Err(re) => {
            eprintln!("Reconnect failed: {re}");
            eprintln!("The REPL backend may have crashed. Exiting.");
            true
        }
    }
}

/// Display the output, warnings, errors, and value from an eval response.
fn display_eval_response(response: &ReplResponse) {
    // Print captured stdout before value/error
    if let Some(ref output) = response.output
        && !output.is_empty()
    {
        print!("{output}");
        if !output.ends_with('\n') {
            println!();
        }
        let _ = std::io::Write::flush(&mut std::io::stdout());
    }
    // Display compilation warnings
    for warning in response.warning_entries() {
        eprintln!(
            "{}",
            beamtalk_repl_protocol::format::format_warning_entry(
                &warning,
                output_mode(),
                beamtalk_repl_protocol::format::WarningStyle::Bullet,
            )
        );
    }
    if response.is_error() {
        if let Some(msg) = response.error_message() {
            if msg == "Interrupted" {
                // Clean interrupt message
                eprintln!("{msg}");
            } else {
                eprintln!("{}", format_error(msg));
            }
        }
    } else if let Some(ref value) = response.value {
        println!("{}", format_value(value));
    }
}

/// Shared REPL loop used by both `beamtalk repl` and `beamtalk workspace attach`.
///
/// Sets up rustyline with tab completion and history, registers the SIGINT
/// handler, then enters the read-eval-print loop. Returns when the user
/// types `:exit` / `:quit`, presses Ctrl-D, or a communication error occurs.
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
    let helper = ReplHelper::new(host, port, cookie, client.session_id());
    let mut rl: Editor<ReplHelper, FileHistory> = Editor::with_config(config).into_diagnostic()?;
    rl.set_helper(Some(helper));

    // Load history
    let history_file = history_path()?;
    let _ = rl.load_history(&history_file);

    // Register SIGINT handler for interrupt during eval.
    // Uses signal-hook to non-destructively register alongside rustyline's handler.
    let interrupted = Arc::new(AtomicBool::new(false));
    let _ = signal_hook::flag::register(signal_hook::consts::SIGINT, Arc::clone(&interrupted));

    // Main REPL loop
    let mut line_buffer: Vec<String> = Vec::new();
    loop {
        let prompt = if line_buffer.is_empty() { "> " } else { "..> " };
        let readline = rl.readline(prompt);

        // Handle readline errors (Ctrl+C, Ctrl+D, I/O errors)
        let line = match readline {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) => {
                // Ctrl+C -- cancel multi-line input if buffering, otherwise just newline
                // Clear the interrupt flag (signal handler also fires)
                interrupted.store(false, Ordering::SeqCst);
                if !line_buffer.is_empty() {
                    line_buffer.clear();
                    eprintln!("Cancelled");
                }
                println!();
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(e) => {
                eprintln!("Readline error: {e}");
                break;
            }
        };
        let line = line.trim();

        // Empty line at the top level — ignore it
        if line.is_empty() && line_buffer.is_empty() {
            continue;
        }

        // Blank line while in multi-line mode: force-submit if the buffer
        // contains a construct that can only be terminated by a blank line
        // (protocol or class definitions).  For other incomplete input
        // (unclosed delimiters, trailing operators) the blank line is pushed
        // onto the buffer below and accumulation continues normally.
        if line.is_empty() && !line_buffer.is_empty() {
            let accumulated = line_buffer.join("\n");
            if needs_blank_line_to_complete(&accumulated) {
                let _ = rl.add_history_entry(&accumulated);
                if eval_with_reconnect(client, &rl, &accumulated, &interrupted) {
                    break;
                }
                line_buffer.clear();
                continue;
            }
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

            match handle_repl_command(line, client) {
                CommandResult::Handled => continue,
                CommandResult::Exit => {
                    println!("Goodbye!");
                    break;
                }
                CommandResult::NotACommand => {}
            }
        }

        // Accumulate input for multi-line expression detection
        line_buffer.push(line.to_string());
        let accumulated = line_buffer.join("\n");

        if !is_input_complete(&accumulated) {
            continue;
        }

        // Input is complete -- add to history as single entry and evaluate
        let _ = rl.add_history_entry(&accumulated);

        // Evaluate expression (interruptible via Ctrl-C)
        if eval_with_reconnect(client, &rl, &accumulated, &interrupted) {
            break;
        }
        line_buffer.clear();
    }

    // Save history
    let _ = rl.save_history(&history_file);

    Ok(())
}

#[cfg(test)]
mod tests {
    //! Tests for REPL command logic: value formatting, node name resolution, and process management.
    use super::*;
    use color::ColorGuard;
    use process::DEFAULT_REPL_PORT;
    use serial_test::serial;

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
        assert_eq!(format_value(&value), "#(1, 2, 3)");
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
        // Blocks are formatted as "Block/N" by the backend
        let value = serde_json::json!("Block/1");
        assert_eq!(format_value(&value), "Block/1");

        let value2 = serde_json::json!("Block/2");
        assert_eq!(format_value(&value2), "Block/2");
    }

    #[test]
    #[serial(color)]
    fn format_value_tuple() {
        let _guard = ColorGuard::disabled();
        // Tuples are pre-formatted as strings by the backend
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

        // Verify the process is no longer running using native OS liveness check
        #[cfg(unix)]
        {
            let pid_i = i32::try_from(pid).expect("pid should fit in i32");
            // SAFETY: kill(2) with signal 0 is a standard existence check.
            let alive = unsafe { libc::kill(pid_i, 0) } == 0;
            assert!(!alive, "Process should have been killed");
        }
        #[cfg(windows)]
        {
            use windows_sys::Win32::Foundation::{CloseHandle, FALSE, STILL_ACTIVE};
            use windows_sys::Win32::System::Threading::{
                GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
            };
            // SAFETY: Windows API call with documented parameters.
            let handle = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid) };
            if !handle.is_null() {
                let mut exit_code: u32 = 0;
                // SAFETY: handle is valid, exit_code is a local variable.
                let ok = unsafe { GetExitCodeProcess(handle, &raw mut exit_code) };
                // Capture the OS error *before* CloseHandle, which may clobber GetLastError.
                let query_err = if ok == FALSE {
                    Some(std::io::Error::last_os_error())
                } else {
                    None
                };
                // SAFETY: handle is valid, obtained from OpenProcess above.
                unsafe { CloseHandle(handle) };
                assert!(
                    query_err.is_none(),
                    "GetExitCodeProcess failed: {}",
                    query_err.unwrap()
                );
                assert_ne!(
                    exit_code, STILL_ACTIVE as u32,
                    "Process should have been killed"
                );
            }
            // handle == 0 means OpenProcess failed → process no longer exists
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
}
