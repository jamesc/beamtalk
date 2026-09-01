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
            confirm_network,
        } => {
            if background {
                run_create_background(
                    &name,
                    port,
                    bind.as_deref(),
                    persistent,
                    idle_timeout,
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
fn run_create_background(
    name: &str,
    port: u16,
    bind: Option<&str>,
    persistent: bool,
    idle_timeout: Option<u64>,
    confirm_network: bool,
) -> Result<()> {
    use crate::commands::repl::bind::{resolve_bind_addr, validate_network_binding};

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
                let has_startup_flags =
                    bind.is_some() || port != 0 || persistent || idle_timeout.is_some();
                if has_startup_flags {
                    eprintln!(
                        "  ⚠️  Startup flags (--port, --bind, --persistent, \
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

    let config = super::WorkspaceConfig {
        port,
        bind_addr: Some(bind_addr),
        auto_cleanup: !persistent,
        max_idle_seconds: idle_timeout,
        log_level: "info",
        otp_app_name: None,
        hex_dep_names: &[], // No hex deps for standalone workspace CLI
    };

    let (node_info, _is_new, workspace_id) =
        get_or_start_workspace(&project_root, Some(name), &paths, &[], &config)?;

    println!("Workspace '{workspace_id}' started");
    println!("Node:      {}", node_info.node_name);
    println!("Port:      {}", node_info.port);
    println!("\nAttach a REPL: beamtalk repl --workspace {workspace_id}");

    Ok(())
}

/// `run_create`'s and `run_list`/`run_status`'s success paths now run
/// against a `BeamtalkHomeOverride`-pointed hermetic tempdir (BT-3370), now
/// that `test_support` serializes every `~/.beamtalk`-touching test (real or
/// overridden) against a shared `RwLock` so an override can never race a
/// real-directory test in the same test binary.
///
/// `run_create_background`'s "already running" short-circuit is reachable
/// today because `workspace_id_for_project(_, Some(name))` (see
/// `storage::workspace_id_for`) ignores `project_path` entirely when a name
/// is given — it just validates and returns the name — so a uniquely-named
/// on-disk fixture guarantees a workspace ID collision with nothing else.
/// Its "start a new node" path is untested here: unlike the three cases
/// above, it needs a live BEAM node (`get_or_start_workspace` shells out to a
/// real `start_detached_node`), the same live-runtime dependency as
/// `workspace/process.rs`'s live-node paths — tracked as a follow-up rather
/// than solved by this file's `BEAMTALK_HOME` isolation alone.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::test_support::{BeamtalkHomeOverride, WorkspaceFixture};
    use crate::commands::workspace::workspace_exists;

    /// `run_create`'s foreground success path (BT-3370): creates a real
    /// workspace under an overridden `BEAMTALK_HOME` hermetic tempdir and
    /// confirms both the returned success and the on-disk result, exercising
    /// `run_create`'s own wrapper body (project-root discovery, the
    /// `create_workspace` call, and its println! summary) rather than just
    /// `create_workspace` itself (already covered at the `workspace::mod`
    /// level against the real directory).
    #[test]
    fn run_create_creates_workspace_under_overridden_home() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());

        let name = format!("run-create-test-{}", std::process::id());
        run_create(&name).expect("run_create should succeed");

        assert!(
            workspace_exists(&name).unwrap(),
            "run_create should have created the workspace on disk"
        );
    }

    /// `run_list`'s success path (BT-3370), both table and `--json` output:
    /// with at least one real workspace present, exercises the non-empty
    /// table-printing loop (current-project marker detection, path
    /// truncation, port formatting) and the JSON-serialization branch —
    /// previously only the underlying `list_workspaces()` was covered
    /// (`workspace::mod`'s tests), not this wrapper's own body.
    #[test]
    fn run_list_succeeds_with_a_workspace_present() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());

        let name = format!("run-list-test-{}", std::process::id());
        run_create(&name).expect("seed workspace should be created");

        run_list(false).expect("table output should succeed");
        run_list(true).expect("json output should succeed");
    }

    /// `run_status`'s success path (BT-3370): with a real workspace present,
    /// exercises this wrapper's own body (age calculation, conditional
    /// node/port/pid lines) rather than just the underlying
    /// `workspace_status()` (already covered in `workspace::mod`'s tests).
    #[test]
    fn run_status_succeeds_for_an_existing_workspace() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());

        let name = format!("run-status-test-{}", std::process::id());
        run_create(&name).expect("seed workspace should be created");

        run_status(Some(&name)).expect("status should succeed for an existing workspace");
    }

    #[test]
    fn run_create_background_already_running_short_circuits() {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let fixture = WorkspaceFixture::new("cli-already-running", port, std::process::id());

        // `port: 0` / no startup flags: the "already running" branch returns
        // before any of them would matter.
        run_create_background(&fixture.id, 0, None, false, None, false)
            .expect("already-running workspace should short-circuit to Ok");
    }

    #[test]
    fn run_create_background_already_running_warns_on_ignored_startup_flags() {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let fixture = WorkspaceFixture::new("cli-already-running-flags", port, std::process::id());

        // Passing --persistent against an already-running workspace hits the
        // "startup flags have no effect" warning branch, but still succeeds.
        run_create_background(&fixture.id, 0, None, true, None, false)
            .expect("already-running workspace should short-circuit to Ok even with flags set");
    }
}
