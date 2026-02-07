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

pub mod discovery;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use miette::{IntoDiagnostic, Result, miette};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

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
pub fn create_workspace(
    project_path: &Path,
    workspace_name: Option<&str>,
) -> Result<WorkspaceMetadata> {
    let workspace_id = workspace_id_for(project_path, workspace_name)?;

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
/// Returns the `NodeInfo` for the started node.
#[allow(dead_code)] // Used in Phase 3
pub fn start_detached_node(
    workspace_id: &str,
    port: u16,
    runtime_beam_dir: &Path,
    jsx_beam_dir: &Path,
    stdlib_beam_dir: &Path,
    auto_cleanup: bool,
    max_idle_seconds: Option<u64>,
) -> Result<NodeInfo> {
    // Generate node name
    let node_name = format!("beamtalk_workspace_{workspace_id}@localhost");

    // Read cookie
    let cookie = read_workspace_cookie(workspace_id)?;
    
    // Determine idle timeout (environment variable > default)
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
         {{ok, _}} = beamtalk_workspace_sup:start_link(#{{workspace_id => <<\"{workspace_id}\">>, \
                                                          project_path => <<\"{project_path_str}\">>, \
                                                          tcp_port => {port}, \
                                                          auto_cleanup => {auto_cleanup}, \
                                                          max_idle_seconds => {idle_timeout}}}), \
         io:format(\"Workspace {workspace_id} started on port {port}~n\"), \
         receive stop -> ok end."
    );

    // Start detached BEAM node
    let (node_flag, node_arg) = if node_name.contains('@') {
        ("-name", node_name.clone())
    } else {
        ("-sname", node_name.clone())
    };

    let args = vec![
        "-detached".to_string(),
        "-noshell".to_string(),
        node_flag.to_string(),
        node_arg,
        "-setcookie".to_string(),
        cookie,
        "-pa".to_string(),
        runtime_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        jsx_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        stdlib_beam_dir.to_str().unwrap_or("").to_string(),
        "-eval".to_string(),
        eval_cmd,
    ];

    let _child = Command::new("erl")
        .args(&args)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|e| {
            miette!("Failed to start detached BEAM node: {e}\nIs Erlang/OTP installed?")
        })?;

    // With -detached, the spawn() returns immediately and the real BEAM node
    // runs independently. We need to wait a bit for it to start up.
    std::thread::sleep(Duration::from_millis(1500));

    // Find the BEAM process by node name
    let pid = find_beam_pid_by_node(&node_name)?;

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

/// Find the PID of a BEAM process by its node name.
fn find_beam_pid_by_node(node_name: &str) -> Result<u32> {
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
            return Ok(pid);
        }
    }

    Err(miette!("Could not find BEAM process for node {node_name}"))
}

/// Get or start a workspace node for the current directory.
/// Returns (`NodeInfo`, bool) where bool indicates if a new node was started.
#[allow(dead_code)] // Used in Phase 3
pub fn get_or_start_workspace(
    project_path: &Path,
    workspace_name: Option<&str>,
    port: u16,
    runtime_beam_dir: &Path,
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
        tracing::info!("Cleaning up orphaned workspace: {}", workspace_id);
        cleanup_stale_node_info(&workspace_id)?;
    }

    // Start new detached node
    let node_info = start_detached_node(
        &workspace_id,
        port,
        runtime_beam_dir,
        jsx_beam_dir,
        stdlib_beam_dir,
        auto_cleanup,
        max_idle_seconds,
    )?;
    Ok((node_info, true, workspace_id)) // New node started
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
        };

        save_node_info(&ws.id, &info).unwrap();
        let loaded = get_node_info(&ws.id).unwrap().unwrap();

        assert_eq!(loaded.node_name, "beamtalk_test@localhost");
        assert_eq!(loaded.port, 9999);
        assert_eq!(loaded.pid, 12345);
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
        };
        assert!(!is_node_running(&info));
    }
}
