// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI interface for workspace management commands.
//!
//! **DDD Context:** Workspace Management
//!
//! Provides `beamtalk workspace {list,stop,status,attach,transcript,logs,create}` subcommands.

use clap::Subcommand;
use miette::Result;

use super::{
    create_workspace, discovery, get_or_start_workspace, list_workspaces, stop_workspace,
    workspace_status,
};
use crate::commands::{attach, logs, transcript};

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
        /// Workspace name or ID to stop (default: current project's workspace)
        name: Option<String>,

        /// Force stop without graceful shutdown
        #[arg(long)]
        force: bool,
    },

    /// Show detailed workspace status
    Status {
        /// Workspace name or ID (default: current project's workspace)
        name: Option<String>,
    },

    /// Attach a REPL to a running workspace (without starting one)
    Attach {
        /// Workspace name or ID (default: current project's workspace)
        name: Option<String>,

        /// Connect to a workspace at an explicit localhost port
        #[arg(long, conflicts_with = "name")]
        port: Option<u16>,

        /// Erlang cookie for authentication (required with --port)
        #[arg(long, requires = "port")]
        cookie: Option<String>,

        /// Disable colored output (also respects `NO_COLOR` environment variable)
        #[arg(long)]
        no_color: bool,
    },

    /// Stream Transcript output from a running workspace
    Transcript {
        /// Workspace name or ID (default: current project's workspace)
        name: Option<String>,

        /// Display last N entries from ring buffer on connect
        #[arg(long)]
        recent: Option<usize>,
    },

    /// View workspace log files
    ///
    /// Shows the most recent workspace's log file. Defaults to the last 50 lines.
    /// Use `--follow` to stream new lines, `--level` to filter by severity,
    /// or `--path` to print the log file path.
    Logs {
        /// Select a specific workspace by name or ID
        #[arg(long)]
        workspace: Option<String>,

        /// Stream new log lines as they appear (like `tail -f`)
        #[arg(long, short)]
        follow: bool,

        /// Filter by minimum severity level (debug, info, notice, warning, error)
        #[arg(long)]
        level: Option<String>,

        /// Expected log format for level filtering (text, json)
        #[arg(long)]
        format: Option<String>,

        /// Print the log file path and exit
        #[arg(long)]
        path: bool,
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
        WorkspaceCommand::Stop { name, force } => stop_workspace(name.as_deref(), force),
        WorkspaceCommand::Status { name } => run_status(name.as_deref()),
        WorkspaceCommand::Attach {
            name,
            port,
            cookie,
            no_color,
        } => attach::run(name.as_deref(), port, cookie.as_deref(), no_color),
        WorkspaceCommand::Transcript { name, recent } => transcript::run(name.as_deref(), recent),
        WorkspaceCommand::Logs {
            workspace,
            follow,
            level,
            format,
            path,
        } => logs::run(
            workspace.as_deref(),
            follow,
            level.as_deref(),
            format.as_deref(),
            path,
        ),
        WorkspaceCommand::Create {
            name,
            background,
            port,
            bind,
            persistent,
            idle_timeout,
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
    web_port: Option<u16>,
    confirm_network: bool,
) -> Result<()> {
    use crate::commands::repl::bind::{resolve_bind_addr, validate_network_binding};

    // Reject --web-port 0 (ephemeral web port can't be reported; BT-689)
    if web_port == Some(0) {
        return Err(miette::miette!(
            "--web-port 0 is not supported. Specify an explicit port for the web interface."
        ));
    }

    let cwd = std::env::current_dir()
        .map_err(|e| miette::miette!("Could not determine current directory: {e}"))?;
    let project_root = discovery::discover_project_root(&cwd);

    // Check if workspace already exists and is running before validating
    // startup-only flags. This avoids spurious errors when the user just
    // wants to check that a workspace is running.
    let workspace_id = super::workspace_id_for_project(&project_root, Some(name))?;
    if let Ok(true) = super::storage::workspace_exists(&workspace_id) {
        if let Ok(Some(info)) = super::storage::get_node_info(&workspace_id) {
            if super::is_node_running(&info, Some(&workspace_id)) {
                println!("Workspace '{workspace_id}' already running");
                let has_startup_flags = bind.is_some()
                    || port != 0
                    || web_port.is_some()
                    || persistent
                    || idle_timeout.is_some();
                if has_startup_flags {
                    eprintln!(
                        "  ⚠️  Startup flags (--port, --bind, --web-port, --persistent, \
                         --idle-timeout) have no effect on an already-running workspace.\n  \
                         Stop it first with `beamtalk workspace stop {name}` to restart with new settings."
                    );
                }
                println!("Node:      {}", info.node_name);
                println!("Port:      {}", info.port);
                println!("\nAttach a REPL: beamtalk repl --workspace {workspace_id}");
                return Ok(());
            }
        }
    }

    // Validate startup-only flags (only reached when we're actually starting a new node)
    let bind_addr = resolve_bind_addr(bind)?;
    validate_network_binding(bind_addr, confirm_network)?;

    // Load runtime beam paths
    let (runtime_dir, layout) = beamtalk_cli::repl_startup::find_runtime_dir_with_layout()?;
    let paths = beamtalk_cli::repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    let (node_info, _is_new, workspace_id) = get_or_start_workspace(
        &project_root,
        Some(name),
        port,
        &paths,
        &[],
        !persistent,
        idle_timeout,
        Some(bind_addr),
        web_port,
        None,
    )?;

    println!("Workspace '{workspace_id}' started");
    println!("Node:      {}", node_info.node_name);
    println!("Port:      {}", node_info.port);
    println!("\nAttach a REPL: beamtalk repl --workspace {workspace_id}");

    Ok(())
}
