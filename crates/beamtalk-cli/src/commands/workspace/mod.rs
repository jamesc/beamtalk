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
//! │ Port: 49152  Cookie: ~/.beamtalk/workspaces/abc123/cookie│
//! │                                                           │
//! │   beamtalk_workspace_sup                                 │
//! │     ├─ beamtalk_repl_server    (TCP server)              │
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

use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use miette::{IntoDiagnostic, Result, miette};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

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

/// Get the workspace directory for a given ID.
pub fn workspace_dir(workspace_id: &str) -> Result<PathBuf> {
    let beamtalk_dir = dirs::home_dir()
        .ok_or_else(|| miette!("Could not determine home directory"))?
        .join(".beamtalk");

    Ok(beamtalk_dir.join("workspaces").join(workspace_id))
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
    use rand::RngCore;
    let mut rng = rand::thread_rng();
    let mut bytes = vec![0u8; 24];
    rng.fill_bytes(&mut bytes);
    base64::Engine::encode(&base64::engine::general_purpose::STANDARD, &bytes)
}

/// Save workspace cookie with secure permissions (owner read/write only).
pub fn save_workspace_cookie(workspace_id: &str, cookie: &str) -> Result<()> {
    let cookie_path = workspace_dir(workspace_id)?.join("cookie");
    fs::write(&cookie_path, cookie).into_diagnostic()?;

    // Set file permissions to 0600 (owner read/write only)
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&cookie_path).into_diagnostic()?.permissions();
        perms.set_mode(0o600);
        fs::set_permissions(&cookie_path, perms).into_diagnostic()?;
    }

    Ok(())
}

/// Read workspace cookie.
#[allow(dead_code)] // Used in future phases
pub fn read_workspace_cookie(workspace_id: &str) -> Result<String> {
    let cookie_path = workspace_dir(workspace_id)?.join("cookie");
    fs::read_to_string(cookie_path).into_diagnostic()
}

/// Get node info for a workspace.
#[allow(dead_code)] // Used in future phases
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
#[allow(dead_code)] // Used in future phases
pub fn save_node_info(workspace_id: &str, info: &NodeInfo) -> Result<()> {
    let node_info_path = workspace_dir(workspace_id)?.join("node.info");
    let content = serde_json::to_string_pretty(info).into_diagnostic()?;
    fs::write(node_info_path, content).into_diagnostic()?;
    Ok(())
}

/// Check if a BEAM node is actually running (handle stale node.info files).
#[allow(dead_code)] // Used in future phases
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
                return comm.contains("beam") || comm.contains("erl");
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
#[allow(dead_code)] // Used in future phases
pub fn cleanup_stale_node_info(workspace_id: &str) -> Result<()> {
    let node_info_path = workspace_dir(workspace_id)?.join("node.info");
    if node_info_path.exists() {
        fs::remove_file(node_info_path).into_diagnostic()?;
    }
    Ok(())
}

/// Create a new workspace.
#[allow(dead_code)] // Used in future phases
pub fn create_workspace(project_path: &Path) -> Result<WorkspaceMetadata> {
    let workspace_id = generate_workspace_id(project_path)?;

    // Check if workspace already exists
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
/// Returns the NodeInfo for the started node.
#[allow(dead_code)] // Used in Phase 3
pub fn start_detached_node(
    workspace_id: &str,
    port: u16,
    runtime_beam_dir: &Path,
    jsx_beam_dir: &Path,
) -> Result<NodeInfo> {
    use std::process::{Command, Stdio};

    // Generate node name
    let node_name = format!("beamtalk_workspace_{workspace_id}@localhost");

    // Read cookie
    let cookie = read_workspace_cookie(workspace_id)?;

    // Build the eval command to start workspace supervisor
    let project_path = get_workspace_metadata(workspace_id)?.project_path;
    let project_path_str = project_path.to_string_lossy();
    let eval_cmd = format!(
        "application:set_env(beamtalk_runtime, workspace_id, <<\"{workspace_id}\">>), \
         application:set_env(beamtalk_runtime, project_path, <<\"{project_path_str}\">>), \
         application:set_env(beamtalk_runtime, tcp_port, {port}), \
         {{ok, _}} = beamtalk_workspace_sup:start_link(#{{workspace_id => <<\"{workspace_id}\">>, \
                                                          project_path => <<\"{project_path_str}\">>, \
                                                          tcp_port => {port}, \
                                                          auto_cleanup => true}}), \
         io:format(\"Workspace {workspace_id} started on port {port}~n\")."
    );

    // Start detached BEAM node
    let args = vec![
        "-detached".to_string(),
        "-noshell".to_string(),
        "-sname".to_string(),
        node_name.clone(),
        "-setcookie".to_string(),
        cookie,
        "-pa".to_string(),
        runtime_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        jsx_beam_dir.to_str().unwrap_or("").to_string(),
        "-eval".to_string(),
        eval_cmd,
    ];

    let child = Command::new("erl")
        .args(&args)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|e| {
            miette!("Failed to start detached BEAM node: {e}\nIs Erlang/OTP installed?")
        })?;

    // Get the PID
    let pid = child.id();

    // Create node info
    let node_info = NodeInfo {
        node_name: node_name.clone(),
        port,
        pid,
    };

    // Save node info
    save_node_info(workspace_id, &node_info)?;

    Ok(node_info)
}

/// Get or start a workspace node for the current directory.
/// Returns (NodeInfo, bool) where bool indicates if a new node was started.
#[allow(dead_code)] // Used in Phase 3
pub fn get_or_start_workspace(
    project_path: &Path,
    port: u16,
    runtime_beam_dir: &Path,
    jsx_beam_dir: &Path,
) -> Result<(NodeInfo, bool)> {
    // Create workspace if it doesn't exist
    let metadata = create_workspace(project_path)?;
    let workspace_id = &metadata.workspace_id;

    // Check if node is already running
    if let Some(node_info) = get_node_info(workspace_id)? {
        if is_node_running(&node_info) {
            return Ok((node_info, false)); // Existing node
        }
        // Stale node.info file
        cleanup_stale_node_info(workspace_id)?;
    }

    // Start new detached node
    let node_info = start_detached_node(workspace_id, port, runtime_beam_dir, jsx_beam_dir)?;
    Ok((node_info, true)) // New node started
}
