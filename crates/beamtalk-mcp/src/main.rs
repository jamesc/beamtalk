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
use tracing_subscriber::{self, EnvFilter};

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

    // Log to stderr (stdout is the MCP stdio transport)
    let default_directive = directive_for_verbosity(args.verbose);
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(default_directive)),
        )
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    // Resolve REPL port and cookie
    let (port, cookie) = resolve_port_and_cookie(&args).await?;
    tracing::info!(port, "Connecting to beamtalk REPL");

    // Connect to REPL
    let repl_client = client::ReplClient::connect(port, &cookie)
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

    // Create MCP server and serve on stdio
    let mcp_server = server::BeamtalkMcp::new(Arc::new(repl_client));
    let service = mcp_server.serve(stdio()).await.inspect_err(|e| {
        tracing::error!(error = ?e, "MCP server error");
    })?;

    service.waiting().await?;
    Ok(())
}

/// Resolve the REPL port and cookie from CLI args or workspace discovery.
async fn resolve_port_and_cookie(args: &Args) -> Result<(u16, String), Box<dyn std::error::Error>> {
    // Explicit port takes priority (cookie from env or default Erlang cookie)
    if let Some(port) = args.port {
        let cookie = std::env::var("BEAMTALK_COOKIE").unwrap_or_default();
        if cookie.trim().is_empty() {
            return Err(
                "BEAMTALK_COOKIE is required when using --port (or use --workspace-id).".into(),
            );
        }
        return Ok((port, cookie));
    }

    // Try project-specific workspace discovery
    if let Some((port, cookie)) = workspace::discover_port_and_cookie(args.workspace_id.as_deref())
    {
        if cookie.trim().is_empty() {
            return Err(format!(
                "Workspace cookie is empty for workspace {:?}; restart with `beamtalk repl`",
                args.workspace_id
            )
            .into());
        }
        return Ok((port, cookie));
    }

    // If --start, auto-start the workspace for this directory rather than falling
    // back to a workspace from a different project.
    if args.start {
        return start_workspace(args.workspace_id.as_deref()).await;
    }

    // Try finding any running workspace
    if let Some((port, cookie)) = workspace::discover_any_port_and_cookie() {
        if cookie.trim().is_empty() {
            return Err(
                "Auto-discovered workspace has empty cookie; restart with `beamtalk repl`".into(),
            );
        }
        tracing::info!(port, "Auto-discovered running REPL");
        return Ok((port, cookie));
    }

    Err("Could not find a running beamtalk REPL. \
         Start one with 'beamtalk repl', use --start to auto-start, or specify --port."
        .into())
}

/// Start a beamtalk workspace for the current directory by running `beamtalk repl`.
///
/// Shells out to `beamtalk repl --port 0 --timeout N` with stdin closed so the
/// process exits once the workspace node is up. The node continues running
/// detached. Parses the assigned port from stdout and reads the cookie from
/// workspace storage.
async fn start_workspace(
    workspace_id: Option<&str>,
) -> Result<(u16, String), Box<dyn std::error::Error>> {
    let idle_timeout = std::env::var("BEAMTALK_WORKSPACE_TIMEOUT")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(14400); // 4 hours

    eprintln!("No running workspace found — starting beamtalk workspace...");

    let output = tokio::process::Command::new("beamtalk")
        .args([
            "repl",
            "--port",
            "0",
            "--timeout",
            &idle_timeout.to_string(),
        ])
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .await
        .map_err(|e| -> Box<dyn std::error::Error> {
            if e.kind() == std::io::ErrorKind::NotFound {
                "beamtalk not found on PATH — install beamtalk or start the workspace manually \
                 with 'beamtalk repl'"
                    .into()
            } else {
                format!("Failed to run beamtalk: {e}").into()
            }
        })?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Check exit status before parsing stdout
    if !output.status.success() {
        return Err(format!(
            "beamtalk repl exited with {} — workspace failed to start.\n\
             stdout: {stdout}\nstderr: {stderr}",
            output.status
        )
        .into());
    }

    // Note: port and workspace ID parsing depends on `beamtalk repl` stdout format.
    // If REPL output changes, update these parsers and the tests below.
    let port = parse_repl_port(&stdout).ok_or_else(|| {
        format!(
            "beamtalk repl succeeded but did not report a port.\n\
             stdout: {stdout}\nstderr: {stderr}"
        )
    })?;

    // Determine workspace ID: use explicit ID, or parse from stdout, or derive from cwd
    let ws_id = if let Some(id) = workspace_id {
        id.to_string()
    } else {
        parse_workspace_id(&stdout).unwrap_or_else(|| {
            std::env::current_dir()
                .map(|p| workspace::generate_workspace_id(&p))
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

    Ok((port, cookie))
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

/// Parse the REPL port from `beamtalk repl` stdout.
///
/// Expects a line like: `Connected to REPL backend on port 12345.`
fn parse_repl_port(stdout: &str) -> Option<u16> {
    stdout.lines().find_map(|line| {
        line.strip_prefix("Connected to REPL backend on port ")
            .and_then(|rest| rest.trim_end_matches('.').trim().parse().ok())
    })
}

/// Parse the workspace ID from `beamtalk repl` stdout.
///
/// Expects a line like: `  Workspace: abc123def456 (new)`
fn parse_workspace_id(stdout: &str) -> Option<String> {
    stdout.lines().find_map(|line| {
        line.strip_prefix("  Workspace: ")
            .map(|rest| rest.split_whitespace().next().unwrap_or(rest).to_string())
    })
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
    fn parse_repl_port_from_typical_output() {
        let stdout = "Welcome to beamtalk REPL\nConnected to REPL backend on port 9876.\n  Workspace: abc123def456 (new)\n";
        assert_eq!(parse_repl_port(stdout), Some(9876));
    }

    #[test]
    fn parse_repl_port_missing() {
        assert_eq!(parse_repl_port("some other output\n"), None);
        assert_eq!(parse_repl_port(""), None);
    }

    #[test]
    fn parse_repl_port_malformed() {
        assert_eq!(
            parse_repl_port("Connected to REPL backend on port notanumber.\n"),
            None
        );
    }

    #[test]
    fn parse_workspace_id_from_typical_output() {
        let stdout = "Connected to REPL backend on port 9876.\n  Workspace: abc123def456 (new)\n";
        assert_eq!(parse_workspace_id(stdout), Some("abc123def456".to_string()));
    }

    #[test]
    fn parse_workspace_id_missing() {
        assert_eq!(parse_workspace_id("no workspace line\n"), None);
        assert_eq!(parse_workspace_id(""), None);
    }

    #[test]
    fn parse_workspace_id_bare() {
        // Workspace line without extra text after ID
        assert_eq!(
            parse_workspace_id("  Workspace: deadbeef1234\n"),
            Some("deadbeef1234".to_string())
        );
    }
}
