// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI interface for workspace management commands.
//!
//! **DDD Context:** Workspace Management
//!
//! Provides `beamtalk workspace {list,stop,status,create}` subcommands.

use clap::Subcommand;
use miette::Result;

use super::{
    WorkspaceStatus, create_workspace, discovery, generate_workspace_id, list_workspaces,
    stop_workspace, workspace_status,
};

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
    },
}

/// Run a workspace subcommand.
pub fn run(command: WorkspaceCommand) -> Result<()> {
    match command {
        WorkspaceCommand::List { json } => run_list(json),
        WorkspaceCommand::Stop { name, force } => stop_workspace(&name, force),
        WorkspaceCommand::Status { name } => run_status(name.as_deref()),
        WorkspaceCommand::Create { name } => run_create(&name),
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

    // Detect current project workspace to mark it
    let current_ws_id = std::env::current_dir()
        .ok()
        .map(|cwd| discovery::discover_project_root(&cwd))
        .and_then(|root| generate_workspace_id(&root).ok());

    // Table header
    println!(
        "{:<2} {:<14} {:<30} {:<10} {:<6}",
        "", "WORKSPACE", "PROJECT", "STATUS", "PORT"
    );

    for ws in &workspaces {
        let marker = if current_ws_id.as_deref() == Some(&ws.workspace_id) {
            "▸"
        } else {
            ""
        };

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

    // Show uptime if running
    if detail.status == WorkspaceStatus::Running {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        let uptime_secs = now.saturating_sub(detail.created_at);
        let hours = uptime_secs / 3600;
        let minutes = (uptime_secs % 3600) / 60;
        println!("Created:   {hours}h {minutes}m ago");
    }

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
