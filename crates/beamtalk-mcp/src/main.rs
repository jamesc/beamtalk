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

use clap::Parser;
use rmcp::{ServiceExt, transport::stdio};
use tracing_subscriber::{self, EnvFilter};

/// Beamtalk MCP server — interact with live beamtalk objects.
#[derive(Parser, Debug)]
#[command(
    name = "beamtalk-mcp",
    about = "MCP server for beamtalk REPL — agent interaction with live objects"
)]
struct Args {
    /// REPL server port (overrides workspace discovery).
    #[arg(short, long)]
    port: Option<u16>,

    /// Workspace ID for port discovery.
    #[arg(short, long)]
    workspace_id: Option<String>,
}

/// Entry point: parse CLI args, connect to the REPL, and start the MCP server.
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Log to stderr (stdout is the MCP stdio transport)
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::from_default_env().add_directive("beamtalk_mcp=info".parse().unwrap()),
        )
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    let args = Args::parse();

    // Resolve REPL port
    let port = resolve_port(&args)?;
    tracing::info!(port, "Connecting to beamtalk REPL");

    // Connect to REPL
    let repl_client = client::ReplClient::connect(port).await.map_err(|e| {
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

/// Resolve the REPL port from CLI args or workspace discovery.
fn resolve_port(args: &Args) -> Result<u16, Box<dyn std::error::Error>> {
    // Explicit port takes priority
    if let Some(port) = args.port {
        return Ok(port);
    }

    // Try workspace discovery
    if let Some(port) = workspace::discover_port(args.workspace_id.as_deref()) {
        return Ok(port);
    }

    // Try finding any running workspace
    if let Some(port) = workspace::discover_any_port() {
        tracing::info!(port, "Auto-discovered running REPL");
        return Ok(port);
    }

    Err("Could not find a running beamtalk REPL. \
         Start one with 'beamtalk repl' or specify --port."
        .into())
}
