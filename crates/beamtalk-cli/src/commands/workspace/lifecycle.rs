// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace lifecycle operations: create, start, stop, list, status.
//!
//! **DDD Context:** CLI

use std::fs;
use std::net::Ipv4Addr;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use miette::{IntoDiagnostic, Result, miette};
use serde::Serialize;

use super::discovery;
use super::process::{
    force_kill_process, is_node_running, start_detached_node, tcp_send_shutdown,
    wait_for_workspace_exit,
};
use super::storage::{
    NodeInfo, WorkspaceMetadata, acquire_workspace_lock, cleanup_stale_node_info, generate_cookie,
    generate_workspace_id, get_node_info, get_workspace_metadata, read_workspace_cookie,
    save_workspace_cookie, save_workspace_metadata, validate_workspace_name, workspace_dir,
    workspace_exists, workspace_id_for, workspaces_base_dir,
};

/// Create a new workspace.
pub fn create_workspace(
    project_path: &Path,
    workspace_name: Option<&str>,
) -> Result<WorkspaceMetadata> {
    let workspace_id = workspace_id_for(project_path, workspace_name)?;

    // Acquire exclusive lock to prevent TOCTOU race on concurrent creation.
    // The lock is released when `_lock` is dropped at end of scope.
    let _lock = acquire_workspace_lock(&workspace_id)?;

    // Re-check under lock — another process may have created the workspace
    // while we were waiting for the lock.
    if workspace_exists(&workspace_id)? {
        return get_workspace_metadata(&workspace_id);
    }

    // Create workspace directory
    let dir = workspace_dir(&workspace_id)?;
    fs::create_dir_all(&dir).into_diagnostic()?;

    // Generate and save cookie
    let cookie = generate_cookie();
    save_workspace_cookie(&workspace_id, &cookie)?;

    // Create metadata
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .into_diagnostic()?
        .as_secs();

    let metadata = WorkspaceMetadata {
        workspace_id: workspace_id.clone(),
        project_path: project_path.to_path_buf(),
        created_at: now,
    };

    save_workspace_metadata(&metadata)?;

    Ok(metadata)
}

/// Get or start a workspace node for the current directory.
/// Returns (`NodeInfo`, bool) where bool indicates if a new node was started.
#[allow(clippy::too_many_arguments)] // delegates to start_detached_node with same params
pub fn get_or_start_workspace(
    project_path: &Path,
    workspace_name: Option<&str>,
    port: u16,
    runtime_beam_dir: &Path,
    repl_beam_dir: &Path,
    jsx_beam_dir: &Path,
    compiler_beam_dir: &Path,
    stdlib_beam_dir: &Path,
    extra_code_paths: &[PathBuf],
    auto_cleanup: bool,
    max_idle_seconds: Option<u64>,
    bind_addr: Option<Ipv4Addr>,
    ssl_dist_optfile: Option<&Path>,
    web_port: Option<u16>,
) -> Result<(NodeInfo, bool, String)> {
    // Create workspace if it doesn't exist
    let metadata = create_workspace(project_path, workspace_name)?;
    let workspace_id = metadata.workspace_id.clone();

    // Check if node is already running
    if let Some(node_info) = get_node_info(&workspace_id)? {
        if is_node_running(&node_info) {
            return Ok((node_info, false, workspace_id)); // Existing node
        }
        // Stale node.info file - orphaned workspace detected
        eprintln!("Cleaning up orphaned workspace: {workspace_id}");
        cleanup_stale_node_info(&workspace_id)?;
    }

    // Start new detached node
    let node_info = start_detached_node(
        &workspace_id,
        port,
        runtime_beam_dir,
        repl_beam_dir,
        jsx_beam_dir,
        compiler_beam_dir,
        stdlib_beam_dir,
        extra_code_paths,
        auto_cleanup,
        max_idle_seconds,
        bind_addr,
        ssl_dist_optfile,
        web_port,
    )?;
    Ok((node_info, true, workspace_id)) // New node started
}

/// Summary of a workspace for listing purposes.
#[derive(Debug, Clone, Serialize)]
pub struct WorkspaceSummary {
    /// Unique workspace identifier.
    pub workspace_id: String,
    /// Absolute path to the project directory.
    pub project_path: PathBuf,
    /// Whether the workspace BEAM node is currently running.
    pub status: WorkspaceStatus,
    /// TCP port of the running node, if any.
    pub port: Option<u16>,
    /// OS process ID of the running node, if any.
    pub pid: Option<u32>,
    /// Unix timestamp (seconds) when the workspace was created.
    pub created_at: u64,
}

/// Running status of a workspace.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum WorkspaceStatus {
    /// The workspace BEAM node is alive and reachable.
    Running,
    /// The workspace has no running BEAM node.
    Stopped,
}

impl std::fmt::Display for WorkspaceStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Running => write!(f, "running"),
            Self::Stopped => write!(f, "stopped"),
        }
    }
}

/// List all workspaces found in `~/.beamtalk/workspaces/`.
pub fn list_workspaces() -> Result<Vec<WorkspaceSummary>> {
    let workspaces_dir = workspaces_base_dir()?;

    if !workspaces_dir.exists() {
        return Ok(Vec::new());
    }

    let mut summaries = Vec::new();

    let entries = fs::read_dir(&workspaces_dir).into_diagnostic()?;
    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();

        if !path.is_dir() {
            continue;
        }

        let metadata_path = path.join("metadata.json");
        if !metadata_path.exists() {
            continue;
        }

        let workspace_id = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("")
            .to_string();

        let Ok(metadata) = get_workspace_metadata(&workspace_id) else {
            continue;
        };

        let (status, port, pid) = match get_node_info(&workspace_id) {
            Ok(Some(info)) => {
                if is_node_running(&info) {
                    (WorkspaceStatus::Running, Some(info.port), Some(info.pid))
                } else {
                    // Stale node.info — clean it up
                    let _ = cleanup_stale_node_info(&workspace_id);
                    (WorkspaceStatus::Stopped, None, None)
                }
            }
            _ => (WorkspaceStatus::Stopped, None, None),
        };

        summaries.push(WorkspaceSummary {
            workspace_id,
            project_path: metadata.project_path,
            status,
            port,
            pid,
            created_at: metadata.created_at,
        });
    }

    // Sort by workspace_id for stable output
    summaries.sort_unstable_by(|a, b| a.workspace_id.cmp(&b.workspace_id));

    Ok(summaries)
}

/// Stop a workspace by name or ID.
///
/// Uses TCP shutdown (graceful OTP teardown via `init:stop()`) as primary
/// mechanism. Falls back to OS-level force-kill if `force` is true or if
/// graceful shutdown times out.
pub fn stop_workspace(name_or_id: &str, force: bool) -> Result<()> {
    // Resolve workspace ID
    let workspace_id = resolve_workspace_id(name_or_id)?;

    if !workspace_exists(&workspace_id)? {
        return Err(miette!("Workspace '{name_or_id}' does not exist"));
    }

    let node_info = get_node_info(&workspace_id)?;

    match node_info {
        Some(info) if is_node_running(&info) => {
            let host = info.connect_host();
            if force {
                // Force-kill: skip graceful shutdown, go straight to OS kill.
                // On Windows PID may be 0 (sentinel) — fall back to graceful.
                if info.pid == 0 {
                    return Err(miette!(
                        "Force-kill is not available (process ID unknown). \
                         Use graceful shutdown instead (omit --force)."
                    ));
                }
                force_kill_process(info.pid)?;
                // Ensure the node has actually released its port before returning.
                wait_for_workspace_exit(host, info.port, 5).map_err(|_| {
                    miette!(
                        "Workspace did not release port {} within 5s after forced stop. \
                         It may still be shutting down; retry shortly.",
                        info.port
                    )
                })?;
            } else {
                eprintln!(
                    "Stopping workspace '{workspace_id}' (port {})...",
                    info.port
                );

                // Try graceful TCP shutdown first
                let cookie = read_workspace_cookie(&workspace_id)?;
                match tcp_send_shutdown(host, info.port, &cookie) {
                    Ok(()) => {
                        // Wait for the workspace to actually exit.
                        // OTP init:stop() does orderly application teardown which
                        // can take 10+ seconds under load.
                        if wait_for_workspace_exit(host, info.port, 15).is_err() {
                            // Graceful shutdown acknowledged but process didn't exit
                            // Fall back to force-kill (if PID available)
                            if info.pid == 0 {
                                return Err(miette!(
                                    "Graceful shutdown timed out. Cannot force-kill \
                                     (process ID unknown). Please manually stop \
                                     the BEAM process or retry."
                                ));
                            }
                            eprintln!("Graceful shutdown timed out, force-killing...");
                            force_kill_process(info.pid)?;
                            wait_for_workspace_exit(host, info.port, 5).map_err(|_| {
                                miette!(
                                    "Workspace did not release port {} within 5s after forced stop. \
                                     It may still be shutting down; retry shortly.",
                                    info.port
                                )
                            })?;
                        }
                    }
                    Err(e) => {
                        // TCP shutdown failed (e.g. connection refused, auth error)
                        // Fall back to force-kill (if PID available)
                        if info.pid == 0 {
                            return Err(miette!(
                                "TCP shutdown failed ({e}). Cannot force-kill \
                                 (process ID unknown). Please manually stop \
                                 the BEAM process or retry."
                            ));
                        }
                        eprintln!("TCP shutdown failed ({e}), force-killing...");
                        force_kill_process(info.pid)?;
                        wait_for_workspace_exit(host, info.port, 5).map_err(|_| {
                            miette!(
                                "Workspace did not release port {} within 5s after forced stop. \
                                 It may still be shutting down; retry shortly.",
                                info.port
                            )
                        })?;
                    }
                }
            }

            // Clean up node.info after process has exited
            cleanup_stale_node_info(&workspace_id)?;

            println!("Workspace '{workspace_id}' stopped");
            Ok(())
        }
        _ => Err(miette!("Workspace '{}' is not running", workspace_id)),
    }
}

/// Detailed status information for a workspace.
#[derive(Debug, Clone, Serialize)]
pub struct WorkspaceDetail {
    /// Unique workspace identifier.
    pub workspace_id: String,
    /// Absolute path to the project directory.
    pub project_path: PathBuf,
    /// Whether the workspace BEAM node is currently running.
    pub status: WorkspaceStatus,
    /// Unix timestamp (seconds) when the workspace was created.
    pub created_at: u64,
    /// Erlang node name, if the workspace is running.
    pub node_name: Option<String>,
    /// TCP port of the running node, if any.
    pub port: Option<u16>,
    /// OS process ID of the running node, if any.
    pub pid: Option<u32>,
}

/// Get detailed status for a workspace.
///
/// If `name_or_id` is `None`, attempts to find the workspace for the current directory.
pub fn workspace_status(name_or_id: Option<&str>) -> Result<WorkspaceDetail> {
    let workspace_id = if let Some(name) = name_or_id {
        resolve_workspace_id(name)?
    } else {
        // Auto-detect: find workspace whose project_path matches current directory
        let cwd = std::env::current_dir().into_diagnostic()?;
        let project_root = discovery::discover_project_root(&cwd);
        find_workspace_by_project_path(&project_root)?
            .unwrap_or(generate_workspace_id(&project_root)?)
    };

    if !workspace_exists(&workspace_id)? {
        return Err(miette!(
            "Workspace '{}' does not exist. Use 'beamtalk workspace list' to see available workspaces.",
            workspace_id
        ));
    }

    let metadata = get_workspace_metadata(&workspace_id)?;

    let (status, node_name, port, pid) = match get_node_info(&workspace_id) {
        Ok(Some(info)) => {
            if is_node_running(&info) {
                (
                    WorkspaceStatus::Running,
                    Some(info.node_name),
                    Some(info.port),
                    Some(info.pid),
                )
            } else {
                let _ = cleanup_stale_node_info(&workspace_id);
                (WorkspaceStatus::Stopped, None, None, None)
            }
        }
        _ => (WorkspaceStatus::Stopped, None, None, None),
    };

    Ok(WorkspaceDetail {
        workspace_id,
        project_path: metadata.project_path,
        status,
        created_at: metadata.created_at,
        node_name,
        port,
        pid,
    })
}

/// Find a workspace by matching its stored `project_path` to the given path.
///
/// Scans all workspaces and compares canonicalized paths. Returns the first
/// matching workspace ID, or `None` if no match is found.
pub(super) fn find_workspace_by_project_path(project_path: &Path) -> Result<Option<String>> {
    let workspaces_dir = workspaces_base_dir()?;

    if !workspaces_dir.exists() {
        return Ok(None);
    }

    let target_canon = project_path.canonicalize().ok();

    let entries = fs::read_dir(&workspaces_dir).into_diagnostic()?;
    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if !path.is_dir() || !path.join("metadata.json").exists() {
            continue;
        }

        let ws_id = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("")
            .to_string();

        let Ok(metadata) = get_workspace_metadata(&ws_id) else {
            continue;
        };

        // Compare canonicalized paths to handle symlinks/relative paths
        let matches = match (&target_canon, metadata.project_path.canonicalize().ok()) {
            (Some(a), Some(b)) => a == &b,
            _ => project_path == metadata.project_path,
        };

        if matches {
            return Ok(Some(ws_id));
        }
    }

    Ok(None)
}

/// Resolve a user-provided name or auto-generated ID to a workspace ID.
///
/// Validates the input using the same rules as workspace creation to ensure
/// consistency and prevent path traversal attacks.
pub fn resolve_workspace_id(name_or_id: &str) -> Result<String> {
    let candidate = name_or_id.trim();

    if candidate.is_empty() {
        return Err(miette!("Workspace name cannot be empty"));
    }

    // Prevent path traversal and malformed filesystem paths
    if candidate.contains('/') || candidate.contains('\\') || candidate.contains('\0') {
        return Err(miette!(
            "Invalid workspace name: must not contain path separators or null bytes"
        ));
    }

    // Reuse the same validation as workspace creation (allowed charset)
    validate_workspace_name(candidate)?;

    Ok(candidate.to_string())
}
