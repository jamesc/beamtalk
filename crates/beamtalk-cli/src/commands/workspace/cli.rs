// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI interface for workspace management commands.
//!
//! **DDD Context:** Workspace Management
//!
//! Provides `beamtalk workspace {list,stop,status,create}` subcommands.

use clap::Subcommand;
use miette::Result;

use super::{create_workspace, discovery, get_or_start_workspace, list_workspaces, stop_workspace, workspace_status};

/// Workspace management subcommands.
#[derive(Debug, Subcommand)]
pub enum WorkspaceCommand {
    /// List all workspaces
    List {
        /// Output as JSON for machine-readable consumption
        #[arg(long)]
        json: bool,
    },

    /// Stop a running workspace
    Stop {
        /// Workspace name or ID to stop
        name: String,

        /// Force stop without graceful shutdown
        #[arg(long)]
        force: bool,
    },

    /// Show detailed workspace status
    Status {
        /// Workspace name or ID (default: current project's workspace)
        name: Option<String>,
    },

    /// Create a new named workspace
    Create {
        /// Name for the workspace
        name: String,

        /// Start the workspace node in the background after creation
        #[arg(long)]
        background: bool,

        /// TCP port for the workspace WebSocket server (0 = OS-assigned)
        #[arg(long, default_value = "0")]
        port: u16,

        /// Network bind address (default: 127.0.0.1)
        #[arg(long)]
        bind: Option<String>,

        /// Keep workspace running indefinitely (no idle timeout)
        #[arg(long)]
        persistent: bool,

        /// Max idle seconds before auto-stop (default: 4 hours)
        #[arg(long)]
        idle_timeout: Option<u64>,

        /// Enable TLS for Erlang distribution
        #[arg(long)]
        tls: bool,

        /// Port for the web interface
        #[arg(long)]
        web_port: Option<u16>,

        /// Confirm binding to a non-loopback network address
        #[arg(long)]
        confirm_network: bool,
    },
}

/// Run a workspace subcommand.
pub fn run(command: WorkspaceCommand) -> Result<()> {
    match command {
        WorkspaceCommand::List { json } => run_list(json),
        WorkspaceCommand::Stop { name, force } => stop_workspace(&name, force),
        WorkspaceCommand::Status { name } => run_status(name.as_deref()),
        WorkspaceCommand::Create {
            name,
            background,
            port,
            bind,
            persistent,
            idle_timeout,
            tls,
            web_port,
            confirm_network,
        } => {
            if background {
                run_create_background(
                    &name,
                    port,
                    bind.as_deref(),
                    persistent,
                    idle_timeout,
                    tls,
                    web_port,
                    confirm_network,
                )
            } else {
                run_create(&name)
            }
        }
    }
}

/// List all workspaces with formatted output.
fn run_list(json: bool) -> Result<()> {
    let workspaces = list_workspaces()?;

    if json {
        let output = serde_json::to_string_pretty(&workspaces)
            .map_err(|e| miette::miette!("Failed to serialize workspaces: {e}"))?;
        println!("{output}");
        return Ok(());
    }

    if workspaces.is_empty() {
        println!("No workspaces found.");
        println!("Start a REPL to create one: beamtalk repl");
        return Ok(());
    }

    // Detect current project root to mark the matching workspace
    let current_project_root = std::env::current_dir()
        .ok()
        .map(|cwd| discovery::discover_project_root(&cwd));

    // Table header
    println!(
        "{:<2} {:<14} {:<30} {:<10} {:<6}",
        "", "WORKSPACE", "PROJECT", "STATUS", "PORT"
    );

    for ws in &workspaces {
        let is_current = current_project_root.as_ref().is_some_and(|root| {
            // Compare canonicalized paths to handle symlinks/relative paths
            let root_canon = root.canonicalize().ok();
            let ws_canon = ws.project_path.canonicalize().ok();
            match (root_canon, ws_canon) {
                (Some(a), Some(b)) => a == b,
                _ => root == &ws.project_path,
            }
        });
        let marker = if is_current { "▸" } else { "" };

        let project = ws
            .project_path
            .to_string_lossy()
            .chars()
            .take(30)
            .collect::<String>();

        let port_str = ws.port.map_or_else(|| "-".to_string(), |p| p.to_string());

        println!(
            "{:<2} {:<14} {:<30} {:<10} {:<6}",
            marker, ws.workspace_id, project, ws.status, port_str
        );
    }

    println!("\n{} workspace(s) total", workspaces.len());

    Ok(())
}

/// Show detailed workspace status.
fn run_status(name: Option<&str>) -> Result<()> {
    let detail = workspace_status(name)?;

    println!("Workspace: {}", detail.workspace_id);
    println!("Project:   {}", detail.project_path.display());
    println!("Status:    {}", detail.status);

    if let Some(ref node_name) = detail.node_name {
        println!("Node:      {node_name}");
    }
    if let Some(port) = detail.port {
        println!("Port:      {port}");
    }
    if let Some(pid) = detail.pid {
        println!("PID:       {pid}");
    }

    // Show workspace age (creation time, not node start time)
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let age_secs = now.saturating_sub(detail.created_at);
    let hours = age_secs / 3600;
    let minutes = (age_secs % 3600) / 60;
    println!("Age:       {hours}h {minutes}m");

    Ok(())
}

/// Create a new workspace.
fn run_create(name: &str) -> Result<()> {
    let cwd = std::env::current_dir()
        .map_err(|e| miette::miette!("Could not determine current directory: {e}"))?;
    let project_root = discovery::discover_project_root(&cwd);

    let metadata = create_workspace(&project_root, Some(name))?;

    println!("Workspace '{}' created", metadata.workspace_id);
    println!("Project:   {}", metadata.project_path.display());
    println!(
        "\nStart a REPL session: beamtalk repl --workspace {}",
        metadata.workspace_id
    );

    Ok(())
}

/// Create a workspace and start it in the background.
///
/// Loads runtime beam paths, starts a detached BEAM node, and outputs
/// workspace ID, port, and node name for scripting/CI use.
#[allow(clippy::too_many_arguments)]
fn run_create_background(
    name: &str,
    port: u16,
    bind: Option<&str>,
    persistent: bool,
    idle_timeout: Option<u64>,
    tls: bool,
    web_port: Option<u16>,
    confirm_network: bool,
) -> Result<()> {
    use crate::commands::repl::bind::{resolve_bind_addr, validate_network_binding};

    let cwd = std::env::current_dir()
        .map_err(|e| miette::miette!("Could not determine current directory: {e}"))?;
    let project_root = discovery::discover_project_root(&cwd);

    // Resolve bind address
    let bind_addr = resolve_bind_addr(bind)?;
    validate_network_binding(bind_addr, confirm_network)?;

    // Load runtime beam paths
    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()?;
    let paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    // Cowboy/cowlib/ranch are needed for the WebSocket transport (ADR 0020)
    let extra_code_paths = vec![
        paths.cowboy_ebin.clone(),
        paths.cowlib_ebin.clone(),
        paths.ranch_ebin.clone(),
    ];

    // Resolve TLS config if requested
    let ssl_dist_conf = if tls {
        crate::commands::repl::resolve_ssl_dist_conf_for_workspace(
            &project_root,
            Some(name),
        )?
    } else {
        None
    };

    let (node_info, _is_new, workspace_id) = get_or_start_workspace(
        &project_root,
        Some(name),
        port,
        &paths.runtime_ebin,
        &paths.workspace_ebin,
        &paths.jsx_ebin,
        &paths.compiler_ebin,
        &paths.stdlib_ebin,
        &extra_code_paths,
        !persistent,
        idle_timeout,
        Some(bind_addr),
        ssl_dist_conf.as_deref(),
        web_port,
    )?;

    println!("Workspace '{workspace_id}' started");
    println!("Node:      {}", node_info.node_name);
    println!("Port:      {}", node_info.port);
    println!(
        "\nAttach a REPL: beamtalk repl --workspace {workspace_id}"
    );

    Ok(())
}
