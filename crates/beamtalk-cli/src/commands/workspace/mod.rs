// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace management for persistent BEAM nodes.
//!
//! Workspaces are long-running BEAM nodes that survive REPL disconnects,
//! allowing actors and state to persist across sessions.
//!
//! # Architecture (from ADR 0004)
//!
//! ```text
//! ┌──────────────────────────────────────────────────────────┐
//! │ Workspace: my-feature (detached BEAM node)               │
//! │ Node: beamtalk_workspace_abc123@localhost                │
//! │ Port: OS-assigned  Cookie: ~/.beamtalk/workspaces/abc123/cookie│
//! │                                                           │
//! │   beamtalk_workspace_sup                                 │
//! │     ├─ beamtalk_repl           (TCP server + eval)       │
//! │     ├─ beamtalk_workspace_meta                           │
//! │     ├─ beamtalk_idle_monitor                             │
//! │     ├─ beamtalk_actor_sup                                │
//! │     └─ beamtalk_session_sup                              │
//! └──────────────────────────────────────────────────────────┘
//! ```
//!
//! # Directory Structure
//!
//! ```text
//! ~/.beamtalk/
//! └── workspaces/
//!     └── abc123/              # Workspace ID (hash of project path)
//!         ├── cookie           # Erlang cookie (chmod 600)
//!         ├── node.info        # Node name, port, PID
//!         └── metadata.json    # Project path, created_at
//! ```
//!
//! # Usage
//!
//! ```bash
//! beamtalk workspace create my-feature  # Create new workspace
//! beamtalk workspace list               # List workspaces
//! beamtalk workspace stop my-feature    # Stop workspace
//! beamtalk repl                         # Auto-detect/create workspace
//! ```

pub mod cli;
pub mod discovery;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use fs2::FileExt;
use miette::{IntoDiagnostic, Result, miette};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::paths::socket_path;

/// Default idle timeout in seconds (4 hours)
const DEFAULT_IDLE_TIMEOUT_SECONDS: u64 = 3600 * 4;

/// Workspace metadata stored in ~/.beamtalk/workspaces/{id}/metadata.json
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceMetadata {
    pub workspace_id: String,
    pub project_path: PathBuf,
    pub created_at: u64,
}

/// Node information stored in ~/.beamtalk/workspaces/{id}/node.info
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeInfo {
    pub node_name: String,
    pub port: u16,
    pub pid: u32,
    /// Process start time, representation is platform-dependent.
    /// On Linux, this is clock ticks since boot from `/proc/{pid}/stat` field 22.
    /// `None` for backward compat with old node.info files or non-Linux platforms.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub start_time: Option<u64>,
}

/// Generate a workspace ID from a project path.
/// Uses SHA256 hash of the absolute path.
pub fn generate_workspace_id(project_path: &Path) -> Result<String> {
    let absolute = project_path.canonicalize().into_diagnostic()?;
    let path_str = absolute.to_string_lossy();

    let mut hasher = Sha256::new();
    hasher.update(path_str.as_bytes());
    let result = hasher.finalize();

    // Use first 12 hex chars for readability
    Ok(format!("{result:x}")[..12].to_string())
}

/// Validate a user-provided workspace name.
fn validate_workspace_name(name: &str) -> Result<()> {
    if name.is_empty() {
        return Err(miette!("Workspace name cannot be empty"));
    }

    let valid = name
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_');
    if !valid {
        return Err(miette!(
            "Workspace name must contain only letters, numbers, '-' or '_'"
        ));
    }

    Ok(())
}

/// Determine workspace ID from project path or explicit name.
pub fn workspace_id_for(project_path: &Path, workspace_name: Option<&str>) -> Result<String> {
    match workspace_name {
        Some(name) => {
            let trimmed = name.trim();
            if trimmed.is_empty() {
                return Err(miette!("Workspace name cannot be empty or whitespace-only"));
            }
            validate_workspace_name(trimmed)?;
            Ok(trimmed.to_string())
        }
        None => generate_workspace_id(project_path),
    }
}

/// Get the base directory for all workspaces (`~/.beamtalk/workspaces/`).
fn workspaces_base_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| miette!("Could not determine home directory"))?;
    Ok(home.join(".beamtalk").join("workspaces"))
}

/// Get the workspace directory for a given ID.
pub fn workspace_dir(workspace_id: &str) -> Result<PathBuf> {
    Ok(workspaces_base_dir()?.join(workspace_id))
}

/// Check if a workspace exists.
pub fn workspace_exists(workspace_id: &str) -> Result<bool> {
    let dir = workspace_dir(workspace_id)?;
    Ok(dir.exists() && dir.join("metadata.json").exists())
}

/// Get workspace metadata.
pub fn get_workspace_metadata(workspace_id: &str) -> Result<WorkspaceMetadata> {
    let metadata_path = workspace_dir(workspace_id)?.join("metadata.json");
    let content = fs::read_to_string(&metadata_path).into_diagnostic()?;
    serde_json::from_str(&content).into_diagnostic()
}

/// Save workspace metadata.
pub fn save_workspace_metadata(metadata: &WorkspaceMetadata) -> Result<()> {
    let dir = workspace_dir(&metadata.workspace_id)?;
    fs::create_dir_all(&dir).into_diagnostic()?;

    let metadata_path = dir.join("metadata.json");
    let content = serde_json::to_string_pretty(metadata).into_diagnostic()?;
    fs::write(metadata_path, content).into_diagnostic()?;

    Ok(())
}

/// Generate a unique Erlang cookie for a workspace.
pub fn generate_cookie() -> String {
    use rand::Rng;
    let mut rng = rand::rng();
    let mut bytes = vec![0u8; 24];
    rng.fill_bytes(&mut bytes);
    base64::Engine::encode(&base64::engine::general_purpose::STANDARD, &bytes)
}

/// Save workspace cookie with secure permissions (owner read/write only).
///
/// On Unix, the file is created with mode 0600 to avoid a TOCTOU race where
/// the cookie could briefly be world-readable. Permissions are also enforced
/// via `fchmod` on the open file descriptor so that pre-existing files with
/// overly-permissive modes (e.g. from older versions) are tightened to 0600.
pub fn save_workspace_cookie(workspace_id: &str, cookie: &str) -> Result<()> {
    let cookie_path = workspace_dir(workspace_id)?.join("cookie");

    #[cfg(unix)]
    {
        use std::io::Write;
        use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

        let mut file = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(0o600)
            .open(&cookie_path)
            .into_diagnostic()?;

        // Ensure 0600 even on overwrite of a pre-existing file (uses fchmod).
        file.set_permissions(fs::Permissions::from_mode(0o600))
            .into_diagnostic()?;

        file.write_all(cookie.as_bytes()).into_diagnostic()?;
    }

    #[cfg(not(unix))]
    {
        fs::write(&cookie_path, cookie).into_diagnostic()?;
    }

    Ok(())
}

/// Read workspace cookie.
pub fn read_workspace_cookie(workspace_id: &str) -> Result<String> {
    let cookie_path = workspace_dir(workspace_id)?.join("cookie");
    fs::read_to_string(cookie_path).into_diagnostic()
}

/// Get node info for a workspace.
pub fn get_node_info(workspace_id: &str) -> Result<Option<NodeInfo>> {
    let node_info_path = workspace_dir(workspace_id)?.join("node.info");

    if !node_info_path.exists() {
        return Ok(None);
    }

    let content = fs::read_to_string(&node_info_path).into_diagnostic()?;
    let info: NodeInfo = serde_json::from_str(&content).into_diagnostic()?;
    Ok(Some(info))
}

/// Save node info for a workspace.
pub fn save_node_info(workspace_id: &str, info: &NodeInfo) -> Result<()> {
    let node_info_path = workspace_dir(workspace_id)?.join("node.info");
    let content = serde_json::to_string_pretty(info).into_diagnostic()?;
    fs::write(node_info_path, content).into_diagnostic()?;
    Ok(())
}

/// Read the actual port from the port file written by `beamtalk_repl_server`.
/// Returns `None` if the file doesn't exist or can't be parsed.
fn read_port_file(workspace_id: &str) -> Result<Option<u16>> {
    let port_file_path = workspace_dir(workspace_id)?.join("port");

    if !port_file_path.exists() {
        return Ok(None);
    }

    let content = fs::read_to_string(&port_file_path).into_diagnostic()?;
    if let Ok(port) = content.trim().parse::<u16>() {
        Ok(Some(port))
    } else {
        tracing::warn!("Invalid port file content: {content:?}");
        Ok(None)
    }
}

/// Read process start time from `/proc/{pid}/stat` (field 22 per proc(5)).
/// Returns `None` if the process doesn't exist or the file can't be read.
/// Linux-only: `/proc` filesystem does not exist on macOS/BSD.
#[cfg(target_os = "linux")]
fn read_proc_start_time(pid: u32) -> Option<u64> {
    let stat_path = format!("/proc/{pid}/stat");
    let content = fs::read_to_string(stat_path).ok()?;
    // Fields are space-separated, but comm (field 2) may contain spaces/parens.
    // Find the LAST ')' to handle pathological comm names.
    let after_comm = content.rsplit_once(')')?.1;
    // Fields after comm: state(3), ppid(4), ... starttime is field 22 (1-indexed),
    // which is the 20th field after comm (fields 3..22 = 20 fields).
    let starttime_str = after_comm.split_whitespace().nth(19)?;
    starttime_str.parse::<u64>().ok()
}

/// Check if a BEAM node is actually running (handle stale node.info files).
pub fn is_node_running(info: &NodeInfo) -> bool {
    // Check if PID exists and is a BEAM process
    #[cfg(unix)]
    {
        use std::process::Command;
        let output = Command::new("ps")
            .args(["-p", &info.pid.to_string(), "-o", "comm="])
            .output();

        if let Ok(output) = output {
            if output.status.success() {
                let comm = String::from_utf8_lossy(&output.stdout);
                if !(comm.contains("beam") || comm.contains("erl")) {
                    return false;
                }
                // PID is a BEAM process — now verify start_time if available
                if let Some(expected_start_time) = info.start_time {
                    #[cfg(target_os = "linux")]
                    {
                        if let Some(actual_start_time) = read_proc_start_time(info.pid) {
                            return actual_start_time == expected_start_time;
                        }
                        // Can't read /proc on Linux — conservatively treat as not running
                        return false;
                    }
                    // On non-Linux Unix (macOS/BSD), /proc is unavailable so
                    // skip the start_time check and trust the PID/BEAM check.
                    #[cfg(not(target_os = "linux"))]
                    {
                        let _ = expected_start_time;
                        return true;
                    }
                }
                // No start_time stored (old node.info format) — skip check
                return true;
            }
        }
    }

    #[cfg(not(unix))]
    {
        // On non-Unix, fall back to TCP connection check
        use std::net::TcpStream;
        use std::time::Duration;

        let addr = format!("127.0.0.1:{}", info.port);
        if let Some(Ok(_)) =
            TcpStream::connect_timeout(&addr.parse().ok()?, Duration::from_millis(1000)).ok()
        {
            return true;
        }
    }

    false
}

/// Clean up stale node.info file.
pub fn cleanup_stale_node_info(workspace_id: &str) -> Result<()> {
    let ws_dir = workspace_dir(workspace_id)?;
    let node_info_path = ws_dir.join("node.info");
    if node_info_path.exists() {
        fs::remove_file(node_info_path).into_diagnostic()?;
    }
    // Also clean up port file (written by beamtalk_repl_server for ephemeral port discovery)
    let port_file_path = ws_dir.join("port");
    if port_file_path.exists() {
        let _ = fs::remove_file(port_file_path);
    }
    Ok(())
}

/// Acquire an exclusive advisory lock for workspace creation.
///
/// Returns a locked `File` handle. The lock is released when the file is dropped.
/// The lockfile is created at `~/.beamtalk/workspaces/{workspace_id}.lock`.
fn acquire_workspace_lock(workspace_id: &str) -> Result<fs::File> {
    let base = workspaces_base_dir()?;
    fs::create_dir_all(&base).into_diagnostic()?;

    let lockfile_path = base.join(format!("{workspace_id}.lock"));
    let lockfile = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(false)
        .open(&lockfile_path)
        .into_diagnostic()?;

    lockfile.lock_exclusive().into_diagnostic()?;
    Ok(lockfile)
}

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

/// Start a detached BEAM node for a workspace.
/// Returns the `NodeInfo` for the started node.
#[allow(clippy::too_many_arguments)]
pub fn start_detached_node(
    workspace_id: &str,
    port: u16,
    runtime_beam_dir: &Path,
    repl_beam_dir: &Path,
    jsx_beam_dir: &Path,
    stdlib_beam_dir: &Path,
    auto_cleanup: bool,
    max_idle_seconds: Option<u64>,
) -> Result<NodeInfo> {
    // Generate node name
    let node_name = format!("beamtalk_workspace_{workspace_id}@localhost");

    // Read cookie
    let cookie = read_workspace_cookie(workspace_id)?;

    // Determine idle timeout (explicit arg > environment variable > default)
    let idle_timeout = max_idle_seconds.unwrap_or_else(|| {
        std::env::var("BEAMTALK_WORKSPACE_TIMEOUT")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(DEFAULT_IDLE_TIMEOUT_SECONDS)
    });

    // Build the eval command to start workspace supervisor and keep running
    let project_path = get_workspace_metadata(workspace_id)?.project_path;
    let project_path_str = project_path.to_string_lossy();
    let eval_cmd = format!(
        "application:set_env(beamtalk_runtime, workspace_id, <<\"{workspace_id}\">>), \
         application:set_env(beamtalk_runtime, project_path, <<\"{project_path_str}\">>), \
         application:set_env(beamtalk_runtime, tcp_port, {port}), \
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, _}} = beamtalk_workspace_sup:start_link(#{{workspace_id => <<\"{workspace_id}\">>, \
                                                          project_path => <<\"{project_path_str}\">>, \
                                                          tcp_port => {port}, \
                                                          auto_cleanup => {auto_cleanup}, \
                                                          max_idle_seconds => {idle_timeout}}}), \
         {{ok, ActualPort}} = beamtalk_repl_server:get_port(), \
         io:format(\"Workspace {workspace_id} started on port ~B~n\", [ActualPort]), \
         receive stop -> ok end."
    );

    // Start detached BEAM node
    let mut cmd = build_detached_node_command(
        &node_name,
        &cookie,
        runtime_beam_dir,
        repl_beam_dir,
        jsx_beam_dir,
        stdlib_beam_dir,
        &eval_cmd,
    )?;

    let _child = cmd.spawn().map_err(|e| {
        miette!("Failed to start detached BEAM node: {e}\nIs Erlang/OTP installed?")
    })?;

    // With -detached, the spawn() returns immediately and the real BEAM node
    // runs independently. We need to wait a bit for it to start up.
    std::thread::sleep(Duration::from_millis(1500));

    // Find the BEAM process by node name
    let (pid, start_time) = find_beam_pid_by_node(&node_name)?;

    // Read actual port from port file (written by beamtalk_repl_server after binding).
    // This is essential when port=0 is used (OS assigns ephemeral port).
    // Retry a few times since the BEAM node may still be initializing.
    let actual_port = if port == 0 {
        let mut discovered = None;
        for _ in 0..10 {
            if let Some(p) = read_port_file(workspace_id)? {
                discovered = Some(p);
                break;
            }
            std::thread::sleep(Duration::from_millis(200));
        }
        discovered.ok_or_else(|| {
            miette!(
                "BEAM node did not report its port.\n\
                 The workspace may have failed to start. Check logs."
            )
        })?
    } else {
        read_port_file(workspace_id)?.unwrap_or(port)
    };

    // Create node info
    let node_info = NodeInfo {
        node_name: node_name.clone(),
        port: actual_port,
        pid,
        start_time,
    };

    // Save node info
    save_node_info(workspace_id, &node_info)?;

    Ok(node_info)
}

/// Build a `Command` for starting a detached BEAM workspace node.
///
/// Extracted from `start_detached_node` so the command configuration
/// (args, env vars) can be inspected in tests without spawning a process.
fn build_detached_node_command(
    node_name: &str,
    cookie: &str,
    runtime_beam_dir: &Path,
    repl_beam_dir: &Path,
    jsx_beam_dir: &Path,
    stdlib_beam_dir: &Path,
    eval_cmd: &str,
) -> Result<Command> {
    let (node_flag, node_arg) = if node_name.contains('@') {
        ("-name", node_name.to_string())
    } else {
        ("-sname", node_name.to_string())
    };

    let args = vec![
        "-detached".to_string(),
        "-noshell".to_string(),
        node_flag.to_string(),
        node_arg,
        "-setcookie".to_string(),
        cookie.to_string(),
        "-pa".to_string(),
        runtime_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        repl_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        jsx_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        stdlib_beam_dir.to_str().unwrap_or("").to_string(),
        "-eval".to_string(),
        eval_cmd.to_string(),
    ];

    let mut cmd = Command::new("erl");
    cmd.args(&args)
        .env("BEAMTALK_DAEMON_SOCKET", socket_path()?.as_os_str())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());

    Ok(cmd)
}

/// Find the PID of a BEAM process by its node name.
fn find_beam_pid_by_node(node_name: &str) -> Result<(u32, Option<u64>)> {
    let output = Command::new("ps")
        .args(["-eo", "pid,command"])
        .output()
        .into_diagnostic()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    for line in stdout.lines() {
        if line.contains("beam.smp") && line.contains(node_name) {
            let pid_str = line
                .split_whitespace()
                .next()
                .ok_or_else(|| miette!("Failed to parse PID from ps output"))?;
            let pid: u32 = pid_str.parse().into_diagnostic()?;
            #[cfg(target_os = "linux")]
            let start_time = read_proc_start_time(pid);
            #[cfg(not(target_os = "linux"))]
            let start_time = None;
            return Ok((pid, start_time));
        }
    }

    Err(miette!("Could not find BEAM process for node {node_name}"))
}

/// Get or start a workspace node for the current directory.
/// Returns (`NodeInfo`, bool) where bool indicates if a new node was started.
#[allow(clippy::too_many_arguments)]
pub fn get_or_start_workspace(
    project_path: &Path,
    workspace_name: Option<&str>,
    port: u16,
    runtime_beam_dir: &Path,
    repl_beam_dir: &Path,
    jsx_beam_dir: &Path,
    stdlib_beam_dir: &Path,
    auto_cleanup: bool,
    max_idle_seconds: Option<u64>,
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
        stdlib_beam_dir,
        auto_cleanup,
        max_idle_seconds,
    )?;
    Ok((node_info, true, workspace_id)) // New node started
}

/// Summary of a workspace for listing purposes.
#[derive(Debug, Clone, Serialize)]
pub struct WorkspaceSummary {
    pub workspace_id: String,
    pub project_path: PathBuf,
    pub status: WorkspaceStatus,
    pub port: Option<u16>,
    pub pid: Option<u32>,
    pub created_at: u64,
}

/// Running status of a workspace.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum WorkspaceStatus {
    Running,
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

/// Poll until a process exits or timeout is reached.
///
/// Uses `kill -0` to check process liveness without sending a signal.
/// Returns `Ok(())` if the process exits within `timeout_secs`, or an error
/// suggesting `--force` if it doesn't.
#[cfg(unix)]
fn wait_for_process_exit(pid: u32, timeout_secs: u64) -> Result<()> {
    let interval = Duration::from_millis(100);
    let deadline = std::time::Instant::now() + Duration::from_secs(timeout_secs);

    while std::time::Instant::now() < deadline {
        let status = Command::new("kill")
            .args(["-0", &pid.to_string()])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();

        match status {
            Ok(s) if !s.success() => return Ok(()), // Process no longer exists
            Err(_) => return Ok(()),                // kill command failed = process gone
            _ => std::thread::sleep(interval),
        }
    }

    Err(miette!(
        "Process {} did not exit within {}s. Try --force to send SIGKILL.",
        pid,
        timeout_secs
    ))
}

/// Stop a workspace by name or ID.
///
/// Attempts graceful shutdown by killing the BEAM process PID.
/// Cleans up `node.info` after stopping.
pub fn stop_workspace(name_or_id: &str, force: bool) -> Result<()> {
    // Resolve workspace ID
    let workspace_id = resolve_workspace_id(name_or_id)?;

    if !workspace_exists(&workspace_id)? {
        return Err(miette!("Workspace '{name_or_id}' does not exist"));
    }

    let node_info = get_node_info(&workspace_id)?;

    match node_info {
        Some(info) if is_node_running(&info) => {
            if !force {
                eprintln!(
                    "Stopping workspace '{}' (PID {})...",
                    workspace_id, info.pid
                );
            }

            // Kill the BEAM process
            #[cfg(unix)]
            {
                let signal = if force { "KILL" } else { "TERM" };
                let status = Command::new("kill")
                    .args([&format!("-{signal}"), &info.pid.to_string()])
                    .status()
                    .into_diagnostic()?;

                if !status.success() {
                    return Err(miette!(
                        "Failed to stop workspace '{}' (PID {})",
                        workspace_id,
                        info.pid
                    ));
                }

                // Wait for process to actually exit before cleaning up
                wait_for_process_exit(info.pid, if force { 2 } else { 5 })?;
            }

            #[cfg(not(unix))]
            {
                return Err(miette!(
                    "Stopping workspaces is only supported on Unix systems"
                ));
            }

            // Clean up node.info only after process has exited
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
    pub workspace_id: String,
    pub project_path: PathBuf,
    pub status: WorkspaceStatus,
    pub created_at: u64,
    pub node_name: Option<String>,
    pub port: Option<u16>,
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
fn find_workspace_by_project_path(project_path: &Path) -> Result<Option<String>> {
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
fn resolve_workspace_id(name_or_id: &str) -> Result<String> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    /// Helper to create a unique test workspace ID and clean up after.
    struct TestWorkspace {
        id: String,
    }

    impl TestWorkspace {
        fn new(prefix: &str) -> Self {
            let id = format!("{prefix}_{}", std::process::id());
            Self { id }
        }

        fn dir(&self) -> PathBuf {
            workspace_dir(&self.id).unwrap()
        }
    }

    impl Drop for TestWorkspace {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(self.dir());
            // Clean up lockfile created by acquire_workspace_lock
            if let Ok(base) = workspaces_base_dir() {
                let _ = fs::remove_file(base.join(format!("{}.lock", self.id)));
            }
        }
    }

    #[test]
    fn test_generate_workspace_id_deterministic() {
        let path = std::env::current_dir().unwrap();
        let id1 = generate_workspace_id(&path).unwrap();
        let id2 = generate_workspace_id(&path).unwrap();
        assert_eq!(id1, id2, "Workspace ID should be deterministic");
    }

    #[test]
    fn test_generate_workspace_id_length() {
        let path = std::env::current_dir().unwrap();
        let id = generate_workspace_id(&path).unwrap();
        assert_eq!(id.len(), 12, "Workspace ID should be 12 characters");
    }

    #[test]
    fn test_generate_workspace_id_hex() {
        let path = std::env::current_dir().unwrap();
        let id = generate_workspace_id(&path).unwrap();
        assert!(
            id.chars().all(|c| c.is_ascii_hexdigit()),
            "ID should be hex"
        );
    }

    #[test]
    fn test_workspace_id_for_explicit_name() {
        let path = std::env::current_dir().unwrap();
        let id = workspace_id_for(&path, Some("my_workspace-1")).unwrap();
        assert_eq!(id, "my_workspace-1");
    }

    #[test]
    fn test_workspace_id_for_invalid_name() {
        let path = std::env::current_dir().unwrap();
        assert!(workspace_id_for(&path, Some("bad/name")).is_err());
        assert!(workspace_id_for(&path, Some("")).is_err());
        assert!(workspace_id_for(&path, Some("has space")).is_err());
        assert!(workspace_id_for(&path, Some("has.dot")).is_err());
        // Whitespace-only names should be rejected
        assert!(workspace_id_for(&path, Some("   ")).is_err());
        assert!(workspace_id_for(&path, Some("\t")).is_err());
    }

    #[test]
    fn test_workspace_id_for_none_uses_hash() {
        let path = std::env::current_dir().unwrap();
        let from_none = workspace_id_for(&path, None).unwrap();
        let from_hash = generate_workspace_id(&path).unwrap();
        assert_eq!(from_none, from_hash, "None should fall through to hash");
    }

    #[test]
    fn test_different_paths_produce_different_workspace_ids() {
        // Verifies worktree isolation: different project paths (as with git worktrees)
        // produce different workspace IDs, ensuring separate workspaces per worktree.
        let id1 = generate_workspace_id(Path::new("/")).unwrap();
        let id2 = generate_workspace_id(Path::new("/tmp")).unwrap();
        assert_ne!(
            id1, id2,
            "Different paths must produce different workspace IDs"
        );
    }

    #[test]
    fn test_generate_cookie_length() {
        let cookie = generate_cookie();
        assert_eq!(
            cookie.len(),
            32,
            "Cookie should be 32 chars (24 bytes base64)"
        );
    }

    #[test]
    fn test_generate_cookie_randomness() {
        let c1 = generate_cookie();
        let c2 = generate_cookie();
        assert_ne!(c1, c2, "Cookies should be random");
    }

    #[test]
    fn test_workspace_dir_contains_id() {
        let dir = workspace_dir("test-ws-123").unwrap();
        assert!(dir.ends_with("workspaces/test-ws-123"));
    }

    #[test]
    fn test_save_and_read_workspace_metadata() {
        let ws = TestWorkspace::new("meta_rt");
        let metadata = WorkspaceMetadata {
            workspace_id: ws.id.clone(),
            project_path: PathBuf::from("/tmp/test-project"),
            created_at: 1_000_000,
        };

        save_workspace_metadata(&metadata).unwrap();
        let loaded = get_workspace_metadata(&ws.id).unwrap();

        assert_eq!(loaded.workspace_id, ws.id);
        assert_eq!(loaded.project_path, PathBuf::from("/tmp/test-project"));
        assert_eq!(loaded.created_at, 1_000_000);
    }

    #[test]
    fn test_save_and_read_cookie() {
        let ws = TestWorkspace::new("cookie_rt");
        fs::create_dir_all(ws.dir()).unwrap();

        save_workspace_cookie(&ws.id, "secret-cookie-123").unwrap();
        let cookie = read_workspace_cookie(&ws.id).unwrap();
        assert_eq!(cookie, "secret-cookie-123");

        // Verify permissions (unix only)
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let cookie_path = ws.dir().join("cookie");
            let mode = fs::metadata(&cookie_path).unwrap().permissions().mode();
            assert_eq!(mode & 0o777, 0o600, "Cookie should be owner-only");
        }
    }

    /// Verify cookie file is created with 0600 permissions atomically
    /// (no TOCTOU race where file is briefly world-readable).
    #[cfg(unix)]
    #[test]
    fn test_cookie_permissions_atomic_at_creation() {
        use std::os::unix::fs::PermissionsExt;

        let ws = TestWorkspace::new("cookie_atomic");
        fs::create_dir_all(ws.dir()).unwrap();

        save_workspace_cookie(&ws.id, "atomic-test-cookie").unwrap();

        let cookie_path = ws.dir().join("cookie");
        let mode = fs::metadata(&cookie_path).unwrap().permissions().mode();
        assert_eq!(
            mode & 0o777,
            0o600,
            "Cookie must be created with 0600 permissions (no race window)"
        );

        // Overwriting an existing cookie should also preserve permissions
        save_workspace_cookie(&ws.id, "updated-cookie").unwrap();
        let mode = fs::metadata(&cookie_path).unwrap().permissions().mode();
        assert_eq!(
            mode & 0o777,
            0o600,
            "Overwritten cookie must retain 0600 permissions"
        );

        let content = fs::read_to_string(&cookie_path).unwrap();
        assert_eq!(content, "updated-cookie");
    }

    /// Verify that overwriting a cookie file that already has insecure
    /// permissions (e.g. 0644 from an older version) tightens it to 0600.
    #[cfg(unix)]
    #[test]
    fn test_cookie_tightens_insecure_existing_permissions() {
        use std::os::unix::fs::PermissionsExt;

        let ws = TestWorkspace::new("cookie_tighten");
        fs::create_dir_all(ws.dir()).unwrap();

        // Pre-create cookie with insecure permissions (simulates older version)
        let cookie_path = ws.dir().join("cookie");
        fs::write(&cookie_path, "old-insecure-cookie").unwrap();
        fs::set_permissions(&cookie_path, fs::Permissions::from_mode(0o644)).unwrap();
        let mode = fs::metadata(&cookie_path).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o644, "Precondition: cookie should be 0644");

        // save_workspace_cookie must tighten permissions back to 0600
        save_workspace_cookie(&ws.id, "new-secure-cookie").unwrap();
        let mode = fs::metadata(&cookie_path).unwrap().permissions().mode();
        assert_eq!(
            mode & 0o777,
            0o600,
            "Overwriting insecure cookie must tighten permissions to 0600"
        );

        let content = fs::read_to_string(&cookie_path).unwrap();
        assert_eq!(content, "new-secure-cookie");
    }

    #[test]
    fn test_save_and_read_node_info() {
        let ws = TestWorkspace::new("nodeinfo_rt");
        fs::create_dir_all(ws.dir()).unwrap();

        let info = NodeInfo {
            node_name: "beamtalk_test@localhost".to_string(),
            port: 9999,
            pid: 12345,
            start_time: Some(987_654),
        };

        save_node_info(&ws.id, &info).unwrap();
        let loaded = get_node_info(&ws.id).unwrap().unwrap();

        assert_eq!(loaded.node_name, "beamtalk_test@localhost");
        assert_eq!(loaded.port, 9999);
        assert_eq!(loaded.pid, 12345);
        assert_eq!(loaded.start_time, Some(987_654));
    }

    #[test]
    fn test_get_node_info_returns_none_when_missing() {
        let ws = TestWorkspace::new("nodeinfo_missing");
        fs::create_dir_all(ws.dir()).unwrap();

        let result = get_node_info(&ws.id).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_workspace_exists_true_after_creation() {
        let ws = TestWorkspace::new("exists_true");
        let metadata = WorkspaceMetadata {
            workspace_id: ws.id.clone(),
            project_path: PathBuf::from("/tmp/test"),
            created_at: 1_000_000,
        };
        save_workspace_metadata(&metadata).unwrap();

        assert!(workspace_exists(&ws.id).unwrap());
    }

    #[test]
    fn test_workspace_exists_false_when_absent() {
        assert!(!workspace_exists("nonexistent_ws_12345").unwrap());
    }

    #[test]
    fn test_cleanup_stale_node_info_removes_file() {
        let ws = TestWorkspace::new("cleanup_test");
        fs::create_dir_all(ws.dir()).unwrap();

        let info = NodeInfo {
            node_name: "test@localhost".to_string(),
            port: 8888,
            pid: 99999,
            start_time: None,
        };
        save_node_info(&ws.id, &info).unwrap();
        assert!(ws.dir().join("node.info").exists());

        cleanup_stale_node_info(&ws.id).unwrap();
        assert!(!ws.dir().join("node.info").exists());
    }

    #[test]
    fn test_cleanup_stale_node_info_noop_when_missing() {
        let ws = TestWorkspace::new("cleanup_noop");
        fs::create_dir_all(ws.dir()).unwrap();

        // Should not error when file doesn't exist
        cleanup_stale_node_info(&ws.id).unwrap();
    }

    #[test]
    fn test_create_workspace_creates_files() {
        let ws = TestWorkspace::new("create_test");
        let project_path = std::env::current_dir().unwrap();

        let metadata = create_workspace(&project_path, Some(&ws.id)).unwrap();
        assert_eq!(metadata.workspace_id, ws.id);
        assert!(ws.dir().join("metadata.json").exists());
        assert!(ws.dir().join("cookie").exists());
    }

    #[test]
    fn test_create_workspace_idempotent() {
        let ws = TestWorkspace::new("create_idem");
        let project_path = std::env::current_dir().unwrap();

        let m1 = create_workspace(&project_path, Some(&ws.id)).unwrap();
        let m2 = create_workspace(&project_path, Some(&ws.id)).unwrap();
        assert_eq!(m1.workspace_id, m2.workspace_id);
        assert_eq!(m1.created_at, m2.created_at);
    }

    #[test]
    fn test_is_node_running_false_for_fake_pid() {
        let info = NodeInfo {
            node_name: "fake@localhost".to_string(),
            port: 1,
            pid: u32::MAX, // Very unlikely to be a real PID
            start_time: None,
        };
        assert!(!is_node_running(&info));
    }

    #[test]
    fn test_is_node_running_false_for_non_beam_pid_even_with_start_time() {
        // PID 1 typically belongs to init/systemd and is not a BEAM process.
        // This test verifies that a non-BEAM PID is treated as "not running"
        // even if a start_time is present. Comprehensive PID reuse detection,
        // where a reused PID has a mismatched start_time, requires integration
        // tests with actual BEAM processes.
        let info = NodeInfo {
            node_name: "stale@localhost".to_string(),
            port: 1,
            pid: 1, // PID 1 exists but is init, not BEAM
            start_time: Some(999_999_999),
        };
        assert!(!is_node_running(&info));
    }

    #[test]
    fn test_is_node_running_false_for_fake_pid_with_start_time() {
        let info = NodeInfo {
            node_name: "fake@localhost".to_string(),
            port: 1,
            pid: u32::MAX,
            start_time: Some(12345),
        };
        // PID doesn't exist at all
        assert!(!is_node_running(&info));
    }

    #[test]
    fn test_node_info_backward_compat_without_start_time() {
        // Old node.info format: no start_time field
        let ws = TestWorkspace::new("compat_test");
        fs::create_dir_all(ws.dir()).unwrap();

        let old_json = r#"{"node_name":"old@localhost","port":9999,"pid":12345}"#;
        fs::write(ws.dir().join("node.info"), old_json).unwrap();

        let loaded = get_node_info(&ws.id).unwrap().unwrap();
        assert_eq!(loaded.node_name, "old@localhost");
        assert_eq!(loaded.port, 9999);
        assert_eq!(loaded.pid, 12345);
        assert_eq!(loaded.start_time, None);
    }

    #[test]
    fn test_node_info_with_start_time_round_trips() {
        let ws = TestWorkspace::new("start_time_rt");
        fs::create_dir_all(ws.dir()).unwrap();

        let info = NodeInfo {
            node_name: "test@localhost".to_string(),
            port: 8080,
            pid: 42,
            start_time: Some(1_234_567_890),
        };
        save_node_info(&ws.id, &info).unwrap();

        let loaded = get_node_info(&ws.id).unwrap().unwrap();
        assert_eq!(loaded.start_time, Some(1_234_567_890));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_read_proc_start_time_for_current_process() {
        // Our own PID should have a readable start time
        let pid = std::process::id();
        let start_time = read_proc_start_time(pid);
        assert!(start_time.is_some(), "Should read start time for own PID");
        assert!(start_time.unwrap() > 0, "Start time should be positive");
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_read_proc_start_time_none_for_fake_pid() {
        let start_time = read_proc_start_time(u32::MAX);
        assert!(start_time.is_none(), "Fake PID should return None");
    }

    #[test]
    fn test_list_workspaces_returns_created_workspace() {
        let ws = TestWorkspace::new("list_test");
        let metadata = WorkspaceMetadata {
            workspace_id: ws.id.clone(),
            project_path: PathBuf::from("/tmp/list-test-project"),
            created_at: 2_000_000,
        };
        save_workspace_metadata(&metadata).unwrap();

        let workspaces = list_workspaces().unwrap();
        let found = workspaces.iter().find(|w| w.workspace_id == ws.id);
        assert!(found.is_some(), "Should find the created workspace");

        let ws_summary = found.unwrap();
        assert_eq!(
            ws_summary.project_path,
            PathBuf::from("/tmp/list-test-project")
        );
        assert_eq!(ws_summary.status, WorkspaceStatus::Stopped);
        assert!(ws_summary.port.is_none());
    }

    #[test]
    fn test_list_workspaces_empty_when_no_workspaces_dir() {
        // list_workspaces handles missing ~/.beamtalk/workspaces/ gracefully
        let result = list_workspaces();
        assert!(result.is_ok());
    }

    #[test]
    fn test_list_workspaces_sorted_by_id() {
        let ws_b = TestWorkspace::new("list_sort_b");
        let ws_a = TestWorkspace::new("list_sort_a");

        for ws in [&ws_a, &ws_b] {
            let metadata = WorkspaceMetadata {
                workspace_id: ws.id.clone(),
                project_path: PathBuf::from("/tmp/sort-test"),
                created_at: 1_000_000,
            };
            save_workspace_metadata(&metadata).unwrap();
        }

        let workspaces = list_workspaces().unwrap();
        let ids: Vec<&str> = workspaces
            .iter()
            .filter(|w| w.workspace_id.starts_with("list_sort_"))
            .map(|w| w.workspace_id.as_str())
            .collect();

        let mut sorted_ids = ids.clone();
        sorted_ids.sort_unstable();
        assert_eq!(ids, sorted_ids, "Workspaces should be sorted by ID");
    }

    #[test]
    fn test_stop_workspace_fails_for_nonexistent() {
        let result = stop_workspace("nonexistent_stop_test_ws", false);
        assert!(result.is_err());
    }

    #[test]
    fn test_stop_workspace_fails_when_not_running() {
        let ws = TestWorkspace::new("stop_not_running");
        let metadata = WorkspaceMetadata {
            workspace_id: ws.id.clone(),
            project_path: PathBuf::from("/tmp/stop-test"),
            created_at: 1_000_000,
        };
        save_workspace_metadata(&metadata).unwrap();

        let result = stop_workspace(&ws.id, false);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("is not running"), "Error: {err}");
    }

    #[test]
    fn test_workspace_status_returns_details() {
        let ws = TestWorkspace::new("status_test");
        let metadata = WorkspaceMetadata {
            workspace_id: ws.id.clone(),
            project_path: PathBuf::from("/tmp/status-test"),
            created_at: 3_000_000,
        };
        save_workspace_metadata(&metadata).unwrap();

        let detail = workspace_status(Some(&ws.id)).unwrap();
        assert_eq!(detail.workspace_id, ws.id);
        assert_eq!(detail.project_path, PathBuf::from("/tmp/status-test"));
        assert_eq!(detail.status, WorkspaceStatus::Stopped);
        assert!(detail.node_name.is_none());
        assert!(detail.port.is_none());
        assert!(detail.pid.is_none());
    }

    #[test]
    fn test_workspace_status_fails_for_nonexistent() {
        let result = workspace_status(Some("nonexistent_status_test_ws"));
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("does not exist"), "Error: {err}");
    }

    #[test]
    fn test_workspace_status_display() {
        assert_eq!(WorkspaceStatus::Running.to_string(), "running");
        assert_eq!(WorkspaceStatus::Stopped.to_string(), "stopped");
    }

    #[test]
    fn test_resolve_workspace_id_passthrough() {
        let id = resolve_workspace_id("my-workspace").unwrap();
        assert_eq!(id, "my-workspace");
    }

    #[test]
    fn test_resolve_workspace_id_rejects_path_traversal() {
        assert!(resolve_workspace_id("../../etc").is_err());
        assert!(resolve_workspace_id("../secret").is_err());
        assert!(resolve_workspace_id("foo/bar").is_err());
        assert!(resolve_workspace_id("foo\\bar").is_err());
        assert!(resolve_workspace_id("foo\0bar").is_err());
        // Dots are rejected by validate_workspace_name charset
        assert!(resolve_workspace_id("has.dot").is_err());
        assert!(resolve_workspace_id("..").is_err());
    }

    #[test]
    fn test_resolve_workspace_id_allows_valid_names() {
        assert!(resolve_workspace_id("my-workspace").is_ok());
        assert!(resolve_workspace_id("abc123").is_ok());
        assert!(resolve_workspace_id("test_ws-1").is_ok());
        // Auto-generated hex hash IDs
        assert!(resolve_workspace_id("abcdef012345").is_ok());
    }

    #[test]
    fn test_resolve_workspace_id_trims_whitespace() {
        let id = resolve_workspace_id("  my-workspace  ").unwrap();
        assert_eq!(id, "my-workspace");
    }

    #[test]
    fn test_resolve_workspace_id_rejects_empty_and_whitespace() {
        assert!(resolve_workspace_id("").is_err());
        assert!(resolve_workspace_id("   ").is_err());
    }

    #[test]
    fn test_find_workspace_by_project_path() {
        let ws = TestWorkspace::new("find_by_path");
        // Use a unique project path so this test doesn't collide with other
        // tests that also save metadata pointing at current_dir().
        let project_path = std::env::current_dir().unwrap().join("find_by_path_unique");
        let metadata = WorkspaceMetadata {
            workspace_id: ws.id.clone(),
            project_path: project_path.clone(),
            created_at: 1_000_000,
        };
        save_workspace_metadata(&metadata).unwrap();

        let found = find_workspace_by_project_path(&project_path).unwrap();
        assert!(found.is_some(), "Should find workspace by project path");
        assert_eq!(found.unwrap(), ws.id);
    }

    #[test]
    fn test_find_workspace_by_project_path_returns_none() {
        let result = find_workspace_by_project_path(Path::new("/nonexistent/path/test")).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_concurrent_create_workspace_produces_consistent_cookie() {
        use std::sync::{Arc, Barrier};
        use std::thread;

        let ws = TestWorkspace::new("concurrent_create");
        let project_path = std::env::current_dir().unwrap();
        let ws_id = ws.id.clone();
        let barrier = Arc::new(Barrier::new(10));

        let handles: Vec<_> = (0..10)
            .map(|_| {
                let barrier = Arc::clone(&barrier);
                let ws_id = ws_id.clone();
                let project_path = project_path.clone();
                thread::spawn(move || {
                    // All threads wait here, then race to create the workspace
                    barrier.wait();
                    let metadata = create_workspace(&project_path, Some(&ws_id)).unwrap();
                    // Each worker captures the cookie value it observes immediately
                    let cookie = read_workspace_cookie(&ws_id).unwrap();
                    (metadata, cookie)
                })
            })
            .collect();

        let results: Vec<(WorkspaceMetadata, String)> =
            handles.into_iter().map(|h| h.join().unwrap()).collect();

        // All threads must get the same workspace_id and created_at
        for (metadata, _) in &results {
            assert_eq!(metadata.workspace_id, ws_id);
            assert_eq!(metadata.created_at, results[0].0.created_at);
        }

        // Cookie must be a single consistent value (not corrupted by concurrent writes)
        let first_cookie = &results[0].1;
        assert!(!first_cookie.is_empty(), "Cookie should not be empty");
        assert_eq!(
            first_cookie.len(),
            32,
            "Cookie should be valid (32 chars = 24 bytes base64)"
        );

        for (_, cookie) in &results {
            assert_eq!(
                cookie, first_cookie,
                "All threads must observe the same cookie value"
            );
        }
    }

    #[test]
    fn test_detached_node_command_sets_daemon_socket_env() {
        let cmd = build_detached_node_command(
            "beamtalk_workspace_test@localhost",
            "test-cookie",
            Path::new("/tmp/runtime"),
            Path::new("/tmp/repl"),
            Path::new("/tmp/jsx"),
            Path::new("/tmp/stdlib"),
            "ok.",
        )
        .unwrap();

        let expected_socket = socket_path().unwrap();
        let env_vars: std::collections::HashMap<_, _> = cmd.get_envs().collect();
        assert!(
            env_vars.contains_key(std::ffi::OsStr::new("BEAMTALK_DAEMON_SOCKET")),
            "Command must set BEAMTALK_DAEMON_SOCKET env var"
        );
        assert_eq!(
            env_vars[std::ffi::OsStr::new("BEAMTALK_DAEMON_SOCKET")],
            Some(expected_socket.as_os_str()),
            "BEAMTALK_DAEMON_SOCKET must match socket_path()"
        );
    }

    #[test]
    fn test_read_port_file_returns_none_when_missing() {
        let tw = TestWorkspace::new("port_missing");
        let _ = fs::create_dir_all(tw.dir());
        let result = read_port_file(&tw.id).unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn test_read_port_file_returns_port() {
        let tw = TestWorkspace::new("port_read");
        let _ = fs::create_dir_all(tw.dir());
        let port_file = tw.dir().join("port");
        fs::write(&port_file, "54321").unwrap();
        let result = read_port_file(&tw.id).unwrap();
        assert_eq!(result, Some(54321));
    }

    #[test]
    fn test_read_port_file_handles_invalid_content() {
        let tw = TestWorkspace::new("port_invalid");
        let _ = fs::create_dir_all(tw.dir());
        let port_file = tw.dir().join("port");
        fs::write(&port_file, "not_a_port").unwrap();
        let result = read_port_file(&tw.id).unwrap();
        assert_eq!(result, None);
    }
}
