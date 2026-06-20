// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk MCP server — agent interaction with live objects.
//!
//! **DDD Context:** Language Service / Interactive Development
//!
//! This binary starts an MCP server over stdio that connects to
//! a running beamtalk REPL and exposes its operations as MCP tools.
//!
//! ## Usage
//!
//! ```bash
//! # Connect to REPL on a specific port
//! beamtalk-mcp --port 9876
//!
//! # Auto-discover from workspace
//! beamtalk-mcp --workspace-id abc123def456
//!
//! # Auto-discover from current directory
//! beamtalk-mcp
//! ```

/// Async TCP client for communicating with a running beamtalk REPL.
mod client;
/// MCP server implementation that exposes REPL operations as tools.
mod server;
/// Workspace discovery for locating a running REPL server's port.
mod workspace;

use std::sync::Arc;

use clap::{ArgAction, Parser};
use rmcp::{ServiceExt, transport::stdio};
use tokio::io::AsyncBufReadExt;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::{self, EnvFilter, reload};
use workspace::{parse_repl_port, parse_workspace_id};

/// Beamtalk MCP server — interact with live beamtalk objects.
#[derive(Parser, Debug)]
#[command(
    name = "beamtalk-mcp",
    about = "MCP server for beamtalk REPL — agent interaction with live objects"
)]
struct Args {
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,

    /// REPL server port (overrides workspace discovery).
    #[arg(short, long)]
    port: Option<u16>,

    /// Workspace ID for port discovery.
    #[arg(short, long)]
    workspace_id: Option<String>,

    /// Auto-start the workspace if none is running.
    ///
    /// When set, if no running workspace is found for the current directory,
    /// `beamtalk repl` is launched in the background to start one. The workspace
    /// node continues running after `beamtalk repl` exits, preserving REPL state
    /// between sessions. This is the behaviour used by `.mcp.json` entries created
    /// by `beamtalk new`.
    #[arg(long)]
    start: bool,
}

/// Entry point: parse CLI args, connect to the REPL, and start the MCP server.
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // Log to stderr (stdout is the MCP stdio transport).
    // Use a reloadable filter so the MCP debug signal file can upgrade
    // the level at runtime (BT-1441).
    let default_directive = directive_for_verbosity(args.verbose);
    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(default_directive));
    let (filter, reload_handle) = reload::Layer::new(env_filter);

    tracing_subscriber::registry()
        .with(filter)
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(std::io::stderr)
                .with_ansi(false),
        )
        .init();

    // Resolve REPL port, cookie, and workspace ID
    let (port, cookie, workspace_id) = resolve_port_and_cookie(&args).await?;
    tracing::info!(port, workspace_id = ?workspace_id, "Connecting to beamtalk REPL");

    // Keep a copy of the workspace ID for the debug signal watcher (BT-1441).
    let workspace_id_for_signal = workspace_id.clone();

    // Connect to REPL
    let repl_client = client::ReplClient::connect(port, &cookie, workspace_id)
        .await
        .map_err(|e| {
            eprintln!("Error: {e}");
            eprintln!();
            eprintln!("Make sure a beamtalk REPL is running:");
            eprintln!("  beamtalk repl");
            eprintln!();
            eprintln!("Or specify a port directly:");
            eprintln!("  beamtalk-mcp --port 9876");
            e
        })?;

    tracing::info!("Connected to REPL, starting MCP server on stdio");

    // Spawn background task to watch for MCP debug signal file (BT-1441).
    // The signal file is written by `Beamtalk enableDebug: #mcp` on the
    // Erlang side and tells us to switch to debug-level tracing.
    if let Some(ref ws_id) = workspace_id_for_signal {
        let signal_path = mcp_debug_signal_path(ws_id);
        spawn_debug_signal_watcher(signal_path, default_directive, reload_handle);
    }

    // Create MCP server and serve on stdio
    let mcp_server = server::BeamtalkMcp::new(Arc::new(repl_client));
    let service = mcp_server.serve(stdio()).await.inspect_err(|e| {
        tracing::error!(error = ?e, "MCP server error");
    })?;

    service.waiting().await?;
    Ok(())
}

/// Resolve the REPL port, cookie, and workspace ID from CLI args or workspace discovery.
///
/// The workspace ID is returned so the MCP client can re-read the port file
/// on reconnect when the workspace restarts on a different port (BT-1416).
async fn resolve_port_and_cookie(
    args: &Args,
) -> Result<(u16, String, Option<String>), Box<dyn std::error::Error>> {
    // Explicit port takes priority (cookie from env or default Erlang cookie).
    // No workspace_id — port re-discovery is not possible with explicit ports.
    if let Some(port) = args.port {
        let cookie = std::env::var("BEAMTALK_COOKIE").unwrap_or_default();
        if cookie.trim().is_empty() {
            return Err(
                "BEAMTALK_COOKIE is required when using --port (or use --workspace-id).".into(),
            );
        }
        return Ok((port, cookie, None));
    }

    // Try project-specific workspace discovery
    if let Some((port, cookie, ws_id)) =
        workspace::discover_port_cookie_and_id(args.workspace_id.as_deref())
    {
        if cookie.trim().is_empty() {
            return Err(format!(
                "Workspace cookie is empty for workspace {:?}; restart with `beamtalk repl`",
                args.workspace_id
            )
            .into());
        }
        return Ok((port, cookie, Some(ws_id)));
    }

    // If --start, auto-start the workspace for this directory rather than falling
    // back to a workspace from a different project.
    if args.start {
        return start_workspace(args.workspace_id.as_deref()).await;
    }

    // Try finding any running workspace
    if let Some((port, cookie, ws_id)) = workspace::discover_any_port_cookie_and_id() {
        if cookie.trim().is_empty() {
            return Err(
                "Auto-discovered workspace has empty cookie; restart with `beamtalk repl`".into(),
            );
        }
        tracing::info!(port, "Auto-discovered running REPL");
        return Ok((port, cookie, Some(ws_id)));
    }

    Err("Could not find a running beamtalk REPL. \
         Start one with 'beamtalk repl', use --start to auto-start, or specify --port."
        .into())
}

/// Start a beamtalk workspace for the current directory by running `beamtalk repl`.
///
/// Shells out to `beamtalk repl --port 0 --timeout N` with stdin closed so the
/// process exits once the workspace node is up. The node continues running
/// detached. The assigned port is read by **streaming** `beamtalk repl` stdout
/// line-by-line until the port line appears, rather than waiting for the pipe to
/// reach EOF. This is essential on Windows (BT-2568): the detached BEAM
/// grandchild node inherits `beamtalk repl`'s stdout pipe handle (Rust spawns
/// children with `bInheritHandles = TRUE` and Windows has no `CLOEXEC`), so the
/// write end stays open after `beamtalk repl` itself exits. An EOF-based read
/// (`.output()`) therefore blocks forever even though the workspace is fully up,
/// which is what stalled `beamtalk-mcp --start` and timed out the MCP
/// `initialize` handshake. Reading only until the port line decouples us from
/// the leaked pipe; a bounded timeout makes a genuinely stuck boot fail loudly
/// instead of hanging. The cookie is then read from workspace storage.
async fn start_workspace(
    workspace_id: Option<&str>,
) -> Result<(u16, String, Option<String>), Box<dyn std::error::Error>> {
    let idle_timeout = std::env::var("BEAMTALK_WORKSPACE_TIMEOUT")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(14400); // 4 hours

    // Cap the wait for `beamtalk repl` to report its port. A stuck boot now fails
    // with a clear error instead of hanging the MCP server indefinitely (BT-2568).
    let boot_timeout_secs: u64 = std::env::var("BEAMTALK_WORKSPACE_BOOT_TIMEOUT_SECS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(120);

    eprintln!("No running workspace found — starting beamtalk workspace...");

    // stderr is inherited (surfaces `beamtalk repl` diagnostics on the MCP
    // server's own stderr log) rather than piped: a piped stderr would face the
    // same Windows grandchild-handle-inheritance leak as stdout, and draining it
    // would be one more thing that can block boot.
    let mut child = tokio::process::Command::new("beamtalk")
        .args([
            "repl",
            "--port",
            "0",
            "--timeout",
            &idle_timeout.to_string(),
        ])
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit())
        .spawn()
        .map_err(|e| -> Box<dyn std::error::Error> {
            if e.kind() == std::io::ErrorKind::NotFound {
                "beamtalk not found on PATH — install beamtalk or start the workspace manually \
                 with 'beamtalk repl'"
                    .into()
            } else {
                format!("Failed to run beamtalk: {e}").into()
            }
        })?;

    let stdout = child
        .stdout
        .take()
        .ok_or("failed to capture beamtalk repl stdout")?;

    // Stream stdout until the port line appears (or we time out). Note: port and
    // workspace ID parsing depends on `beamtalk repl` stdout format — if REPL
    // output changes, update `parse_repl_port` / `parse_workspace_id` and tests.
    let (port, captured) =
        read_port_from_boot(stdout, std::time::Duration::from_secs(boot_timeout_secs))
            .await
            .map_err(|e| -> Box<dyn std::error::Error> {
                // The launcher is best-effort cleaned up; the detached node (if any) is
                // independent and unaffected.
                let _ = child.start_kill();
                e
            })?;

    // Reap the launcher in the background so it doesn't linger as a zombie. We do
    // NOT block on it: on Windows the leaked stdout pipe means we can't rely on
    // the process being fully drainable, and the workspace node is already up.
    tokio::spawn(async move {
        let _ = child.wait().await;
    });

    // Determine workspace ID: use explicit ID, or parse from stdout, or derive from cwd
    let ws_id = if let Some(id) = workspace_id {
        id.to_string()
    } else {
        parse_workspace_id(&captured).unwrap_or_else(|| {
            std::env::current_dir()
                .ok()
                .and_then(|p| workspace::generate_workspace_id(&p))
                .unwrap_or_default()
        })
    };

    // Read cookie from workspace storage
    let cookie = workspace::read_cookie_file(&ws_id).ok_or_else(|| {
        format!(
            "Workspace started on port {port} but could not read cookie for workspace '{ws_id}' — \
             try running 'beamtalk repl' manually"
        )
    })?;

    // Wait for TCP readiness (up to 60s; configurable)
    let tcp_timeout_secs: u64 = std::env::var("BEAMTALK_WORKSPACE_TCP_TIMEOUT_SECS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(60);
    wait_for_tcp_ready(port, std::time::Duration::from_secs(tcp_timeout_secs)).await?;

    tracing::info!(port, workspace_id = ws_id, "beamtalk workspace ready");
    eprintln!("Workspace ready on port {port}");

    Ok((port, cookie, Some(ws_id)))
}

/// Read `beamtalk repl` stdout until the assigned port is reported.
///
/// Streams the launcher's stdout line-by-line, accumulating it, and returns as
/// soon as `parse_repl_port` matches a line — without waiting for EOF. This
/// avoids the Windows hang where the detached BEAM grandchild keeps the stdout
/// pipe's write end open (BT-2568). The whole read is bounded by `timeout`; on
/// expiry, or if stdout closes before a port is seen, a clear error is returned.
///
/// Output-ordering invariant: `beamtalk repl` MUST print the `Workspace: <id>`
/// line *before* the port line. Reading stops at the port line, so any line
/// emitted after it (the workspace ID included) is not captured and
/// `parse_workspace_id` would fall back to the cwd-derived ID. The fallback keeps
/// the workspace usable, but the ordering is a protocol contract between the REPL
/// output layer and this reader — do not reorder those lines in `beamtalk repl`
/// without updating `read_port_from_boot`. The
/// `read_port_from_boot_captures_workspace_line_before_port` test pins it.
///
/// Returns the parsed port and the captured stdout (for workspace-ID parsing).
async fn read_port_from_boot<R>(
    stdout: R,
    timeout: std::time::Duration,
) -> Result<(u16, String), Box<dyn std::error::Error>>
where
    R: tokio::io::AsyncRead + Unpin,
{
    let mut lines = tokio::io::BufReader::new(stdout).lines();
    let mut captured = String::new();
    let deadline = tokio::time::Instant::now() + timeout;

    loop {
        let remaining = deadline.saturating_duration_since(tokio::time::Instant::now());
        if remaining.is_zero() {
            return Err(format!(
                "beamtalk repl did not report a port within {timeout:?} — workspace boot stalled.\n\
                 Captured output:\n{captured}"
            )
            .into());
        }

        match tokio::time::timeout(remaining, lines.next_line()).await {
            // A line arrived.
            Ok(Ok(Some(line))) => {
                let port = parse_repl_port(&line);
                captured.push_str(&line);
                captured.push('\n');
                if let Some(port) = port {
                    return Ok((port, captured));
                }
            }
            // stdout reached EOF before a port line — the launcher exited early.
            Ok(Ok(None)) => {
                return Err(format!(
                    "beamtalk repl stdout closed before reporting a port — \
                     workspace failed to start.\nCaptured output:\n{captured}"
                )
                .into());
            }
            // I/O error reading the pipe.
            Ok(Err(e)) => {
                return Err(format!("error reading beamtalk repl output: {e}").into());
            }
            // Overall deadline elapsed mid-read.
            Err(_) => {
                return Err(format!(
                    "beamtalk repl did not report a port within {timeout:?} — workspace boot stalled.\n\
                     Captured output:\n{captured}"
                )
                .into());
            }
        }
    }
}

/// Poll TCP until the server accepts connections or the timeout expires.
///
/// Each connection attempt is capped at 2s to prevent a single SYN timeout
/// (which can be 75-120s on Linux) from blowing past the overall deadline.
async fn wait_for_tcp_ready(
    port: u16,
    timeout: std::time::Duration,
) -> Result<(), Box<dyn std::error::Error>> {
    let addr = format!("127.0.0.1:{port}");
    let deadline = tokio::time::Instant::now() + timeout;
    let per_attempt = std::time::Duration::from_secs(2);

    loop {
        // Cap each connect attempt so a SYN-timeout doesn't overshoot the deadline.
        if let Ok(Ok(_)) =
            tokio::time::timeout(per_attempt, tokio::net::TcpStream::connect(&addr)).await
        {
            return Ok(());
        }
        if tokio::time::Instant::now() >= deadline {
            return Err(format!(
                "Workspace port {port} not accepting connections after {}s — \
                 check 'beamtalk workspace status'",
                timeout.as_secs()
            )
            .into());
        }
        tokio::time::sleep(std::time::Duration::from_millis(200)).await;
    }
}

fn directive_for_verbosity(v: u8) -> &'static str {
    // Target must match the crate's Rust module path (`beamtalk_mcp`).
    // `beamtalk=…` only matches `beamtalk::*`, not `beamtalk_mcp`.
    match v {
        0 => "beamtalk_mcp=info",
        1 => "beamtalk_mcp=debug",
        _ => "beamtalk_mcp=trace",
    }
}

/// Return the path to the MCP debug signal file for a given workspace.
///
/// Mirrors the Erlang-side path: `~/.beamtalk/workspaces/{id}/mcp_debug_enabled`.
fn mcp_debug_signal_path(workspace_id: &str) -> std::path::PathBuf {
    // beamtalk_workspace::workspace_dir handles home-dir resolution and
    // path-traversal validation. Falls back to workspaces_base_dir if
    // workspace_dir fails (shouldn't happen for valid workspace IDs).
    beamtalk_workspace::workspace_dir(workspace_id).map_or_else(
        |e| {
            tracing::warn!(
                error = %e,
                workspace_id,
                "Failed to resolve workspace dir for MCP debug signal"
            );
            beamtalk_workspace::workspaces_base_dir().map_or_else(
                |_| {
                    // Non-existent path; signal watcher will simply never trigger
                    std::path::PathBuf::from("__nonexistent_mcp_signal__")
                },
                |base| base.join(workspace_id).join("mcp_debug_enabled"),
            )
        },
        |dir| dir.join("mcp_debug_enabled"),
    )
}

/// Spawn a background task that polls for the MCP debug signal file.
///
/// When the file appears, the tracing filter is reloaded to `beamtalk_mcp=debug`.
/// When it disappears, the filter reverts to the original directive.
fn spawn_debug_signal_watcher(
    signal_path: std::path::PathBuf,
    default_directive: &'static str,
    reload_handle: reload::Handle<EnvFilter, tracing_subscriber::Registry>,
) {
    tokio::spawn(async move {
        let mut debug_enabled = signal_path.exists();
        if debug_enabled {
            // Signal file already present at startup — activate debug immediately
            if let Ok(new_filter) = EnvFilter::try_new("beamtalk_mcp=debug") {
                if reload_handle.reload(new_filter).is_ok() {
                    tracing::info!("MCP debug enabled via signal file at startup");
                }
            }
        }

        let poll_interval = std::time::Duration::from_secs(2);
        loop {
            tokio::time::sleep(poll_interval).await;
            let file_exists = signal_path.exists();

            if file_exists && !debug_enabled {
                // Signal file appeared — switch to debug level
                if let Ok(new_filter) = EnvFilter::try_new("beamtalk_mcp=debug") {
                    match reload_handle.reload(new_filter) {
                        Ok(()) => {
                            tracing::info!("MCP debug logging enabled via signal file");
                            debug_enabled = true;
                        }
                        Err(e) => {
                            tracing::warn!(error = %e, "Failed to reload tracing filter for debug");
                        }
                    }
                }
            } else if !file_exists && debug_enabled {
                // Signal file removed — revert to default level
                if let Ok(new_filter) = EnvFilter::try_new(default_directive) {
                    match reload_handle.reload(new_filter) {
                        Ok(()) => {
                            tracing::info!("MCP debug logging disabled, reverted to default");
                            debug_enabled = false;
                        }
                        Err(e) => {
                            tracing::warn!(
                                error = %e,
                                "Failed to reload tracing filter for default"
                            );
                        }
                    }
                }
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn directive_defaults() {
        assert_eq!(directive_for_verbosity(0), "beamtalk_mcp=info");
        assert_eq!(directive_for_verbosity(1), "beamtalk_mcp=debug");
        assert_eq!(directive_for_verbosity(2), "beamtalk_mcp=trace");
    }

    #[test]
    fn mcp_signal_path_includes_workspace_id() {
        let path = mcp_debug_signal_path("abc123def456");
        assert!(
            path.ends_with("abc123def456/mcp_debug_enabled"),
            "Signal path should end with workspace_id/mcp_debug_enabled, got: {}",
            path.display()
        );
    }

    #[test]
    fn mcp_signal_path_contains_beamtalk_dir() {
        let path = mcp_debug_signal_path("abc123def456");
        let path_str = path.to_string_lossy();
        assert!(
            path_str.contains(".beamtalk") && path_str.contains("workspaces"),
            "Signal path should be under .beamtalk/workspaces/, got: {path_str}"
        );
    }

    #[tokio::test]
    async fn read_port_from_boot_returns_on_port_line() {
        // The port line need not be the last line; reading stops as soon as it
        // appears (we must not depend on EOF — that's the Windows hang, BT-2568).
        let out = b"Welcome to beamtalk REPL\n\
                    Connected to REPL backend on port 9876.\n\
                    extra line that never arrives at EOF\n"
            .as_slice();
        let (port, captured) = read_port_from_boot(out, std::time::Duration::from_secs(5))
            .await
            .expect("should parse port");
        assert_eq!(port, 9876);
        assert!(captured.contains("Connected to REPL backend on port 9876."));
    }

    #[tokio::test]
    async fn read_port_from_boot_captures_workspace_line_before_port() {
        // The workspace line is printed before the port line, so the captured
        // text used for workspace-ID parsing includes it.
        let out = b"  Workspace: abc123def456 (new)\n\
                    Connected to REPL backend on port 5555.\n"
            .as_slice();
        let (port, captured) = read_port_from_boot(out, std::time::Duration::from_secs(5))
            .await
            .expect("should parse port");
        assert_eq!(port, 5555);
        assert_eq!(
            parse_workspace_id(&captured),
            Some("abc123def456".to_string())
        );
    }

    #[tokio::test]
    async fn read_port_from_boot_errors_on_eof_without_port() {
        let out = b"Welcome to beamtalk REPL\nsome other output\n".as_slice();
        let err = read_port_from_boot(out, std::time::Duration::from_secs(5))
            .await
            .expect_err("should error when no port is reported");
        assert!(
            err.to_string().contains("before reporting a port"),
            "unexpected error: {err}"
        );
    }

    #[tokio::test]
    async fn read_port_from_boot_errors_on_timeout() {
        // A stream that never closes and never emits a port line should time out
        // and return the "boot stalled" error — not hang forever. This is the
        // Windows hazard (BT-2568): the detached BEAM grandchild holds the stdout
        // pipe open so EOF never arrives, so the deadline must bound the wait.
        use tokio::io::AsyncWriteExt;
        let (reader, mut writer) = tokio::io::duplex(256);
        writer
            .write_all(b"startup line with no port\n")
            .await
            .unwrap();
        // writer is intentionally kept open so the stream never reaches EOF.
        let err = read_port_from_boot(reader, std::time::Duration::from_millis(50))
            .await
            .expect_err("should time out");
        assert!(
            err.to_string().contains("boot stalled"),
            "unexpected error: {err}"
        );
        drop(writer);
    }
}
