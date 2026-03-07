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
//!         ├── metadata.json    # Project path, created_at
//!         └── tls/             # TLS certificates (ADR 0020, optional)
//!             ├── ca.pem
//!             ├── node.pem
//!             ├── node-key.pem # chmod 600
//!             └── ssl_dist.conf
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

/// CLI subcommands for workspace management (`workspace list`, `workspace stop`, etc.).
pub mod cli;
/// Project root discovery and workspace auto-detection.
pub mod discovery;
/// epmd client: TCP NAMES_REQ protocol, deregistration polling, conflict detection.
mod epmd;
/// Workspace lifecycle operations: create, start, list, status.
mod lifecycle;
/// Node liveness queries: OS process checks, port-file nonce validation, health probes.
mod node_state;
/// Workspace node startup sequence.
mod process;
/// Workspace shutdown orchestration: graceful TCP shutdown, force-kill, exit polling.
mod shutdown;
/// BEAM node command construction for workspace startup.
mod startup_command;
/// File I/O operations for workspace metadata, cookies, and node information.
pub mod storage;

pub use lifecycle::{
    WorkspaceStatus, WorkspaceSummary, create_workspace, get_or_start_workspace, list_workspaces,
    resolve_workspace_id, workspace_status,
};
pub use node_state::is_node_running;
pub use shutdown::stop_workspace;
pub use storage::{
    cleanup_stale_node_info, get_node_info, get_workspace_metadata, read_workspace_cookie,
    workspace_exists, workspace_id_for,
};

/// Resolve a workspace ID from a project path and optional name.
///
/// Convenience wrapper around [`workspace_id_for`] for callers
/// that already have a project path.
pub fn workspace_id_for_project(
    project_path: &std::path::Path,
    workspace_name: Option<&str>,
) -> miette::Result<String> {
    storage::workspace_id_for(project_path, workspace_name)
}

#[cfg(test)]
mod tests {
    use super::epmd::wait_for_epmd_deregistration;
    use super::lifecycle::{WorkspaceStatus, find_workspace_by_project_path, resolve_workspace_id};
    use super::process::start_detached_node;
    use super::shutdown::{force_kill_process, wait_for_workspace_exit};
    #[cfg(target_os = "linux")]
    use super::storage::read_proc_start_time;
    use super::storage::{
        NodeInfo, WorkspaceMetadata, generate_cookie, generate_workspace_id, read_port_file,
        read_workspace_cookie, save_node_info, save_workspace_cookie, save_workspace_metadata,
        validate_workspace_name, workspace_dir, workspaces_base_dir,
    };
    use super::*;
    use serial_test::serial;
    use std::fs;
    use std::path::{Path, PathBuf};

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
    fn test_validate_workspace_name_valid_cases() {
        assert!(validate_workspace_name("myws").is_ok());
        assert!(validate_workspace_name("my-ws").is_ok());
        assert!(validate_workspace_name("my_ws").is_ok());
        assert!(validate_workspace_name("my-ws_123").is_ok());
        assert!(validate_workspace_name("a").is_ok());
        assert!(validate_workspace_name("12345").is_ok());
    }

    #[test]
    fn test_validate_workspace_name_invalid_cases() {
        assert!(validate_workspace_name("").is_err());
        assert!(validate_workspace_name("workspäce").is_err());
        assert!(validate_workspace_name("has space").is_err());
        assert!(validate_workspace_name("has.dot").is_err());
        assert!(validate_workspace_name("has/slash").is_err());
        assert!(validate_workspace_name("ws!@#").is_err());
    }

    #[test]
    fn test_validate_workspace_name_boundary_long_name() {
        let long_name = "a".repeat(256);
        // Very long names are valid per the current implementation (only charset is checked)
        assert!(validate_workspace_name(&long_name).is_ok());
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
        let cwd = std::env::current_dir().unwrap();
        let parent = cwd.parent().expect("cwd should have a parent");
        let id1 = generate_workspace_id(&cwd).unwrap();
        let id2 = generate_workspace_id(parent).unwrap();
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
            nonce: None,
            bind_addr: None,
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
            nonce: None,
            bind_addr: None,
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
            nonce: None,
            bind_addr: None,
        };
        assert!(!is_node_running(&info, None));
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
            nonce: None,
            bind_addr: None,
        };
        assert!(!is_node_running(&info, None));
    }

    #[test]
    fn test_is_node_running_false_for_fake_pid_with_start_time() {
        let info = NodeInfo {
            node_name: "fake@localhost".to_string(),
            port: 1,
            pid: u32::MAX,
            start_time: Some(12345),
            nonce: None,
            bind_addr: None,
        };
        // PID doesn't exist at all
        assert!(!is_node_running(&info, None));
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
            nonce: None,
            bind_addr: None,
        };
        save_node_info(&ws.id, &info).unwrap();

        let loaded = get_node_info(&ws.id).unwrap().unwrap();
        assert_eq!(loaded.start_time, Some(1_234_567_890));
    }

    #[test]
    fn test_node_info_backward_compat_without_bind_addr() {
        // Old node.info format: no bind_addr field
        let ws = TestWorkspace::new("compat_bind");
        fs::create_dir_all(ws.dir()).unwrap();

        let old_json = r#"{"node_name":"old@localhost","port":9999,"pid":12345,"start_time":100}"#;
        fs::write(ws.dir().join("node.info"), old_json).unwrap();

        let loaded = get_node_info(&ws.id).unwrap().unwrap();
        assert_eq!(loaded.bind_addr, None);
        assert_eq!(loaded.connect_host(), "127.0.0.1");
    }

    #[test]
    fn test_node_info_bind_addr_round_trips() {
        let ws = TestWorkspace::new("bind_addr_rt");
        fs::create_dir_all(ws.dir()).unwrap();

        let info = NodeInfo {
            node_name: "test@localhost".to_string(),
            port: 8080,
            pid: 42,
            start_time: None,
            nonce: None,
            bind_addr: Some("192.168.1.5".to_string()),
        };
        save_node_info(&ws.id, &info).unwrap();

        let loaded = get_node_info(&ws.id).unwrap().unwrap();
        assert_eq!(loaded.bind_addr, Some("192.168.1.5".to_string()));
        assert_eq!(loaded.connect_host(), "192.168.1.5");
    }

    #[test]
    fn test_connect_host_defaults_to_loopback() {
        let info = NodeInfo {
            node_name: "test@localhost".to_string(),
            port: 8080,
            pid: 1,
            start_time: None,
            nonce: None,
            bind_addr: None,
        };
        assert_eq!(info.connect_host(), "127.0.0.1");
    }

    #[test]
    fn test_connect_host_uses_stored_address() {
        let info = NodeInfo {
            node_name: "test@localhost".to_string(),
            port: 8080,
            pid: 1,
            start_time: None,
            nonce: None,
            bind_addr: Some("192.168.1.5".to_string()),
        };
        assert_eq!(info.connect_host(), "192.168.1.5");
    }

    #[test]
    fn test_connect_host_maps_all_interfaces_to_loopback() {
        let info = NodeInfo {
            node_name: "test@localhost".to_string(),
            port: 8080,
            pid: 1,
            start_time: None,
            nonce: None,
            bind_addr: Some("0.0.0.0".to_string()),
        };
        assert_eq!(info.connect_host(), "127.0.0.1");
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
        let result = stop_workspace(Some("nonexistent_stop_test_ws"), false);
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

        let result = stop_workspace(Some(&ws.id), false);
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
        assert_eq!(result, Some((54321, None)));
    }

    #[test]
    fn test_read_port_file_with_nonce() {
        let tw = TestWorkspace::new("port_nonce");
        let _ = fs::create_dir_all(tw.dir());
        let port_file = tw.dir().join("port");
        fs::write(&port_file, "54321\nabcdef0123456789").unwrap();
        let result = read_port_file(&tw.id).unwrap();
        assert_eq!(result, Some((54321, Some("abcdef0123456789".to_string()))));
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

    // ── Integration Tests (require live Erlang/OTP runtime) ────────────
    //
    // These tests spawn real BEAM nodes and are `#[ignore]` by default.
    // Run them with: `just test-integration` or `cargo test -- --ignored`

    /// Guard that kills a BEAM node when dropped, preventing orphans.
    ///
    /// Performs best-effort cleanup: force-kill → wait for port release and,
    /// on Unix, wait for epmd deregistration. This helps the next `#[serial]`
    /// test start from a clean slate (fewer `TIME_WAIT` ports, fewer stale
    /// epmd entries).
    ///
    /// When not already panicking, cleanup failures cause a panic so that
    /// leaked nodes surface as test failures rather than silent flakiness.
    /// During an unwind (test already failed), errors are printed to stderr
    /// to avoid double-panics.
    struct NodeGuard {
        info: NodeInfo,
    }

    impl NodeGuard {
        fn new(info: &NodeInfo) -> Self {
            Self { info: info.clone() }
        }
    }

    impl Drop for NodeGuard {
        fn drop(&mut self) {
            if let Err(e) = force_kill_process(self.info.pid) {
                if std::thread::panicking() {
                    eprintln!("NodeGuard: failed to force-kill pid {}: {e}", self.info.pid);
                } else {
                    panic!("NodeGuard: failed to force-kill pid {}: {e}", self.info.pid);
                }
            }
            // Wait for port release so the next test doesn't hit TIME_WAIT.
            if let Err(e) = wait_for_workspace_exit(self.info.connect_host(), self.info.port, 15) {
                if std::thread::panicking() {
                    eprintln!(
                        "NodeGuard: workspace did not exit (pid {}, port {}): {e}",
                        self.info.pid, self.info.port
                    );
                } else {
                    panic!(
                        "NodeGuard: workspace did not exit (pid {}, port {}): {e}",
                        self.info.pid, self.info.port
                    );
                }
            }
            // Wait for epmd deregistration so the next test can reuse node names.
            if let Err(e) = wait_for_epmd_deregistration(&self.info.node_name, 5) {
                if std::thread::panicking() {
                    eprintln!(
                        "NodeGuard: epmd did not deregister '{}': {e}",
                        self.info.node_name
                    );
                } else {
                    panic!(
                        "NodeGuard: epmd did not deregister '{}': {e}",
                        self.info.node_name
                    );
                }
            }
        }
    }

    /// Kill a BEAM node without cleanup (simulates a crash).
    ///
    /// Uses native OS calls for cross-platform process kill, then waits for port
    /// release and (on Unix) epmd deregistration with generous timeouts for
    /// loaded CI machines. This ensures the next operation in the same test
    /// (e.g. restarting a node with the same name) doesn't hit stale state.
    ///
    /// # Panics
    ///
    /// Panics if the PID is the sentinel value (0), the process cannot be
    /// found or killed, or post-kill waits time out.
    fn kill_node_raw(info: &NodeInfo) {
        assert!(
            info.pid != 0,
            "kill_node_raw called with sentinel PID 0 — node PID unknown"
        );
        force_kill_process(info.pid)
            .unwrap_or_else(|e| panic!("failed to kill BEAM process with pid {}: {e}", info.pid));
        wait_for_workspace_exit(info.connect_host(), info.port, 15).unwrap_or_else(|e| {
            panic!(
                "workspace did not exit after forced kill (pid {}, port {}): {e}",
                info.pid, info.port
            )
        });
        // Wait for epmd deregistration so a subsequent node with the same name
        // doesn't get rejected by epmd.
        wait_for_epmd_deregistration(&info.node_name, 5).unwrap_or_else(|e| {
            panic!(
                "epmd did not deregister '{}' after kill: {e}",
                info.node_name
            )
        });
    }

    /// Assert that a node is no longer running, with retries.
    ///
    /// After a kill or stop, the OS may keep the port in `TIME_WAIT` briefly,
    /// causing `is_node_running` to see a spurious TCP connect success.
    /// Retries up to 10 × 500ms = 5s before failing.
    #[track_caller]
    fn assert_node_stopped(info: &NodeInfo, msg: &str) {
        for _ in 0..10 {
            if !is_node_running(info, None) {
                return;
            }
            std::thread::sleep(std::time::Duration::from_millis(500));
        }
        panic!("{msg}");
    }

    /// Assert that `list_workspaces` reports the given workspace as `Stopped`,
    /// with retries to handle port `TIME_WAIT` races.
    #[track_caller]
    fn assert_workspace_stopped(workspace_id: &str) {
        for _ in 0..10 {
            let workspaces = list_workspaces().unwrap();
            if let Some(found) = workspaces.iter().find(|w| w.workspace_id == workspace_id) {
                if found.status == WorkspaceStatus::Stopped {
                    return;
                }
            }
            std::thread::sleep(std::time::Duration::from_millis(500));
        }
        panic!("Workspace '{workspace_id}' should be reported as Stopped");
    }

    /// Locate BEAM directories needed to start a workspace node.
    fn beam_dirs_for_tests() -> beamtalk_cli::repl_startup::BeamPaths {
        let runtime_dir = beamtalk_cli::repl_startup::find_runtime_dir()
            .expect("Cannot find runtime dir — run from repo root or set BEAMTALK_RUNTIME_DIR");
        let paths = beamtalk_cli::repl_startup::beam_paths(&runtime_dir);
        assert!(
            beamtalk_cli::repl_startup::has_beam_files(&paths.runtime_ebin),
            "Runtime BEAM files not found at {:?}. Run `cd runtime && rebar3 compile` first.",
            paths.runtime_ebin,
        );
        paths
    }

    /// Start a detached BEAM node for integration tests using default parameters.
    ///
    /// Creates a `TestWorkspace` with the given prefix, creates the workspace
    /// directory, starts a node with standard test defaults (port 0, idle timeout
    /// 60 s, no extra code paths), and wraps it in a `NodeGuard` for automatic
    /// cleanup. Returns all three for use in the test body.
    ///
    /// Retries up to 3 times with exponential backoff to handle transient WebSocket
    /// health check timeouts on slow CI runners (BT-1175). Between retries, any
    /// partially-started node is killed and epmd deregistration is awaited so the
    /// next attempt gets a clean slate.
    fn start_test_node(prefix: &str) -> (TestWorkspace, NodeInfo, NodeGuard) {
        let tw = TestWorkspace::new(prefix);
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();
        let paths = beam_dirs_for_tests();
        // Derive the node name the same way start_detached_node does, for epmd cleanup.
        let node_name = format!("beamtalk_workspace_{}@localhost", tw.id);

        let mut last_err = None;
        for attempt in 0..3_usize {
            if attempt > 0 {
                // Kill any partially-started node before retrying (the node passed the TCP
                // check but timed out on the WS health check, so it is still running).
                let pid_path = workspace_dir(&tw.id).unwrap().join("pid");
                if let Ok(contents) = fs::read_to_string(&pid_path) {
                    if let Ok(pid) = contents.trim().parse::<u32>() {
                        let _ = force_kill_process(pid);
                    }
                }
                // Wait for epmd deregistration so the retry can reuse the same node name.
                let _ = wait_for_epmd_deregistration(&node_name, 5);
                // Linear backoff: 1 s, 2 s.
                std::thread::sleep(std::time::Duration::from_secs(attempt as u64));
            }
            match start_detached_node(&tw.id, 0, &paths, &[], false, Some(60), None, None, None) {
                Ok(node_info) => {
                    let guard = NodeGuard::new(&node_info);
                    return (tw, node_info, guard);
                }
                Err(e) => {
                    eprintln!("start_test_node attempt {}/{} failed: {e}", attempt + 1, 3);
                    last_err = Some(e);
                }
            }
        }
        panic!("start_test_node failed after 3 attempts: {last_err:?}");
    }

    #[test]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_start_detached_node_integration() {
        let (tw, node_info, _guard) = start_test_node("integ_start");

        // Verify node.info was written with correct fields
        let saved = get_node_info(&tw.id)
            .expect("get_node_info should succeed")
            .expect("node.info should exist after start");
        assert_eq!(saved.pid, node_info.pid);
        assert!(
            saved.port > 0,
            "port should be assigned (got {})",
            saved.port
        );
        assert!(
            saved.node_name.contains(&tw.id),
            "node_name should contain workspace ID"
        );

        // Verify the node is actually running
        assert!(
            is_node_running(&node_info, Some(&tw.id)),
            "is_node_running should return true for live node"
        );

        // Verify PID corresponds to a running BEAM process
        #[cfg(unix)]
        {
            let pid_i = i32::try_from(node_info.pid).expect("node PID should fit in i32");
            // SAFETY: kill(2) with signal 0 is a standard existence check.
            let alive = unsafe { libc::kill(pid_i, 0) } == 0;
            assert!(
                alive,
                "PID {} should correspond to a running process",
                node_info.pid
            );
        }
        // On Linux, verify the process is actually a BEAM node via /proc cmdline
        #[cfg(target_os = "linux")]
        {
            let cmdline_path = format!("/proc/{}/cmdline", node_info.pid);
            let cmdline = fs::read_to_string(&cmdline_path)
                .unwrap_or_else(|e| panic!("failed to read {cmdline_path}: {e}"));
            assert!(
                cmdline.contains("beam") || cmdline.contains("erl"),
                "PID {} should be a BEAM process, got cmdline: {:?}",
                node_info.pid,
                cmdline
            );
        }
    }

    #[test]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_is_node_running_true_then_false_integration() {
        let (tw, node_info, _guard) = start_test_node("integ_running");

        // True case: node is running
        assert!(
            is_node_running(&node_info, Some(&tw.id)),
            "is_node_running should be true while node is alive"
        );

        // Kill the node without cleanup (simulates a crash)
        kill_node_raw(&node_info);

        assert_node_stopped(&node_info, "is_node_running should be false after kill");
    }

    #[test]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_get_or_start_workspace_lifecycle_integration() {
        let tw = TestWorkspace::new("integ_lifecycle");
        let project_path = std::env::current_dir().unwrap();

        let paths = beam_dirs_for_tests();

        // Step 1: First call creates workspace and starts node
        let (info1, started1, id1) = get_or_start_workspace(
            &project_path,
            Some(&tw.id),
            0,
            &paths,
            &[],
            false,
            Some(60),
            None,
            None, // ssl_dist_optfile
            None, // web_port
        )
        .expect("first get_or_start should succeed");
        let _guard1 = NodeGuard::new(&info1);
        assert!(started1, "first call should start a new node");
        assert_eq!(id1, tw.id);
        assert!(
            is_node_running(&info1, Some(&tw.id)),
            "node should be running"
        );

        // Step 2: Second call reconnects to existing node
        let (info2, started2, id2) = get_or_start_workspace(
            &project_path,
            Some(&tw.id),
            0,
            &paths,
            &[],
            false,
            Some(60),
            None,
            None, // ssl_dist_optfile
            None, // web_port
        )
        .expect("reconnect should succeed");
        assert!(!started2, "second call should reuse existing node");
        assert_eq!(id2, tw.id);
        assert_eq!(info2.pid, info1.pid, "should return same PID");

        // Step 3: Stop the node gracefully via TCP shutdown
        stop_workspace(Some(&tw.id), false).expect("graceful stop should succeed");
        assert_node_stopped(&info1, "node should not be running after stop");

        // Wait for epmd to deregister the old node name before restarting.
        // After force-kill, epmd may still hold the registration briefly,
        // which prevents a new node with the same name from starting.
        wait_for_epmd_deregistration(&info1.node_name, 5)
            .expect("epmd should deregister node name within timeout");

        // Step 4: Third call starts a new node
        let (info3, started3, id3) = get_or_start_workspace(
            &project_path,
            Some(&tw.id),
            0,
            &paths,
            &[],
            false,
            Some(60),
            None,
            None, // ssl_dist_optfile
            None, // web_port
        )
        .expect("restart should succeed");
        let _guard3 = NodeGuard::new(&info3);
        assert!(started3, "third call should start a new node");
        assert_eq!(id3, tw.id);
        assert_ne!(
            info3.pid, info1.pid,
            "restarted node should have different PID"
        );
        assert!(
            is_node_running(&info3, Some(&tw.id)),
            "restarted node should be running"
        );
    }

    /// Regression test for BT-970: concurrent `get_or_start_workspace` calls on the
    /// same workspace ID must not both attempt to start a node. The workspace lock must
    /// cover the full check-is-running + start sequence so the second caller discovers
    /// the node already running after the first caller starts it.
    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_concurrent_get_or_start_workspace_integration() {
        use std::sync::{Arc, Barrier};
        use std::thread;

        let tw = TestWorkspace::new("integ_concurrent");
        let project_path = std::env::current_dir().unwrap();

        let paths = Arc::new(beam_dirs_for_tests());

        // Use an Arc<Barrier> so both threads enter get_or_start_workspace at the
        // same time, maximising the chance of hitting the race window.
        let barrier = Arc::new(Barrier::new(2));

        let handles: Vec<_> = (0..2)
            .map(|_| {
                let barrier = Arc::clone(&barrier);
                let project_path = project_path.clone();
                let tw_id = tw.id.clone();
                let paths = Arc::clone(&paths);
                thread::spawn(move || {
                    barrier.wait();
                    get_or_start_workspace(
                        &project_path,
                        Some(&tw_id),
                        0,
                        &paths,
                        &[],
                        false,
                        Some(60),
                        None,
                        None, // ssl_dist_optfile
                        None, // web_port
                    )
                })
            })
            .collect();

        let results: Vec<_> = handles.into_iter().map(|h| h.join().unwrap()).collect();

        // Both calls must succeed
        for result in &results {
            assert!(
                result.is_ok(),
                "get_or_start_workspace should succeed, got: {:?}",
                result.as_ref().err()
            );
        }

        let infos: Vec<_> = results.into_iter().map(|r| r.unwrap()).collect();

        // Safety net: ensure the node is killed even if an assertion below panics.
        let _guard = NodeGuard::new(&infos[0].0);

        // Exactly one caller must have started a new node; the other must have joined it
        let started_count = infos.iter().filter(|(_, started, _)| *started).count();
        assert_eq!(
            started_count, 1,
            "exactly one caller should have started the node; got started_count={started_count}"
        );

        // Both callers must return the same node (same PID and port)
        let (info0, _, id0) = &infos[0];
        let (info1, _, id1) = &infos[1];
        assert_eq!(id0, id1, "both callers must return the same workspace ID");
        assert_eq!(
            info0.pid, info1.pid,
            "both callers must return the same node PID"
        );
        assert_eq!(
            info0.port, info1.port,
            "both callers must return the same node port"
        );
        assert!(
            is_node_running(info0, Some(id0)),
            "node should be running after concurrent start"
        );
    }

    /// Regression test for BT-967: stale port file from a previous aborted startup
    /// causes `start_detached_node` to connect to the wrong BEAM node, producing
    /// auth failures in `wait_for_tcp_ready` for 30 seconds before timing out.
    ///
    /// Scenario:
    /// 1. A previous node started and wrote its port to the port file.
    /// 2. The node was force-killed and the workspace directory was partially cleaned
    ///    (node.info deleted, but port file survived — e.g. because the BEAM process
    ///    was still holding workspace.log open when `remove_dir_all` ran on Windows, or
    ///    because a restart was attempted before cleanup completed).
    /// 3. A new node is started for the same workspace.
    ///
    /// Without the fix: port discovery reads the stale port on the first iteration,
    /// then `wait_for_tcp_ready` connects to a dead port → ECONNREFUSED × 150 → timeout.
    /// With the fix: `start_detached_node` deletes the stale port file before spawning,
    /// so port discovery waits for the new node to write its own port file.
    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_start_detached_node_ignores_stale_port_file_integration() {
        let tw = TestWorkspace::new("integ_stale_port");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let paths = beam_dirs_for_tests();

        // Step 1: Start a node to get a real port, then kill it without cleanup.
        let first_info =
            start_detached_node(&tw.id, 0, &paths, &[], false, Some(60), None, None, None)
                .expect("first start should succeed");
        let stale_port = first_info.port;
        kill_node_raw(&first_info);

        // Step 2: Remove node.info but deliberately LEAVE the stale port file.
        // This simulates the scenario where TestWorkspace::Drop's remove_dir_all
        // failed to remove the workspace directory (the BEAM node held workspace.log
        // open), so the directory survived with stale runtime files intact.
        let ws_dir = workspace_dir(&tw.id).unwrap();
        std::fs::remove_file(ws_dir.join("node.info")).expect("node.info should exist after kill");
        // Verify stale port file still exists with the old port
        let port_file_contents = std::fs::read_to_string(ws_dir.join("port"))
            .expect("stale port file should still exist");
        assert!(
            port_file_contents
                .trim_start()
                .starts_with(&stale_port.to_string()),
            "port file should contain stale port {stale_port}: {port_file_contents:?}"
        );

        // Step 3: Start a new node. `get_or_start_workspace` finds no node.info,
        // so it calls `start_detached_node` directly (not via cleanup_stale_node_info).
        // The stale port file must be deleted before discovery, otherwise the loop
        // reads stale_port immediately and wait_for_tcp_ready times out.
        let (new_info, started, _) = get_or_start_workspace(
            &project_path,
            Some(&tw.id),
            0,
            &paths,
            &[],
            false,
            Some(60),
            None,
            None,
            None,
        )
        .expect("restart with stale port file should succeed without timing out");
        let _guard = NodeGuard::new(&new_info);

        assert!(started, "should have started a new node");
        assert_ne!(
            new_info.pid, first_info.pid,
            "new node should have a different PID"
        );
        assert!(
            is_node_running(&new_info, Some(&tw.id)),
            "new node should be running and reachable"
        );
    }

    /// Regression test for BT-969: a `starting` tombstone left by a mid-startup
    /// crash is detected on the next `start_detached_node` call, all stale runtime
    /// files are cleaned up, and the node starts successfully.
    ///
    /// Scenario:
    /// 1. A previous node started successfully (port/pid/node.info written).
    /// 2. After that run, we force-kill the node without cleanup.
    /// 3. We manually write a `starting` tombstone to simulate a partial startup
    ///    (i.e. as if a future startup crashed before completing).
    /// 4. `start_detached_node` is called again — it must detect `starting`,
    ///    clean all stale runtime files, and start successfully.
    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_start_detached_node_cleans_up_tombstone_integration() {
        let tw = TestWorkspace::new("integ_tombstone");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let paths = beam_dirs_for_tests();

        // Step 1: Start a node to produce real runtime files, then kill it.
        let first_info =
            start_detached_node(&tw.id, 0, &paths, &[], false, Some(60), None, None, None)
                .expect("first start should succeed");
        kill_node_raw(&first_info);

        // Step 2: Write a `starting` tombstone to simulate a partial startup that
        // was interrupted before the node finished initialising.
        let ws_dir = workspace_dir(&tw.id).unwrap();
        std::fs::write(ws_dir.join("starting"), b"")
            .expect("should be able to write starting tombstone");

        // Verify stale runtime files exist (port, pid written by first run).
        assert!(
            ws_dir.join("pid").exists(),
            "stale pid file should exist after kill"
        );

        // Step 3: Start a new node.  The tombstone must be detected, all stale
        // files cleaned, and startup must succeed.
        let (new_info, started, _) = get_or_start_workspace(
            &project_path,
            Some(&tw.id),
            0,
            &paths,
            &[],
            false,
            Some(60),
            None,
            None,
            None,
        )
        .expect("restart with tombstone present should succeed");
        let _guard = NodeGuard::new(&new_info);

        assert!(started, "should have started a new node");
        assert_ne!(
            new_info.pid, first_info.pid,
            "new node should have a different PID"
        );
        assert!(
            is_node_running(&new_info, Some(&tw.id)),
            "new node should be running and reachable"
        );
        // Tombstone must be gone after successful startup.
        assert!(
            !ws_dir.join("starting").exists(),
            "starting tombstone should be removed after successful startup"
        );
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_list_workspaces_running_node_integration() {
        let (tw, node_info, _guard) = start_test_node("integ_list_running");

        let workspaces = list_workspaces().unwrap();
        let found = workspaces
            .iter()
            .find(|w| w.workspace_id == tw.id)
            .expect("Should find the workspace in list");

        assert_eq!(
            found.status,
            WorkspaceStatus::Running,
            "Workspace should be Running"
        );
        assert_eq!(
            found.pid,
            Some(node_info.pid),
            "PID should match started node"
        );
        assert!(found.port.is_some(), "Port should be reported");
        assert!(found.port.unwrap() > 0, "Port should be non-zero");
    }

    #[test]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_list_workspaces_stale_cleanup_integration() {
        let tw = TestWorkspace::new("integ_list_stale");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let paths = beam_dirs_for_tests();
        let node_info = start_detached_node(
            &tw.id,
            0,
            &paths,
            &[],
            false,
            Some(60),
            None,
            None, // ssl_dist_optfile
            None, // web_port
        )
        .expect("start_detached_node should succeed");

        // Kill the node without cleanup (simulating crash)
        kill_node_raw(&node_info);

        assert_workspace_stopped(&tw.id);
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_workspace_status_running_integration() {
        let (tw, node_info, _guard) = start_test_node("integ_ws_status");

        let detail = workspace_status(Some(&tw.id)).unwrap();
        assert_eq!(detail.workspace_id, tw.id);
        assert_eq!(detail.status, WorkspaceStatus::Running);
        assert!(
            detail.node_name.is_some(),
            "node_name should be present for running workspace"
        );
        assert!(
            detail.node_name.as_ref().unwrap().contains(&tw.id),
            "node_name should contain workspace ID"
        );
        assert_eq!(detail.port, Some(node_info.port));
        assert_eq!(detail.pid, Some(node_info.pid));
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_stop_workspace_graceful_integration() {
        let (tw, node_info, _guard) = start_test_node("integ_stop_graceful");

        // Graceful stop (force=false) uses TCP shutdown + init:stop(),
        // which should succeed for detached BEAM nodes
        let result = stop_workspace(Some(&tw.id), false);
        assert!(
            result.is_ok(),
            "Graceful TCP shutdown should succeed, got: {:?}",
            result.err()
        );

        assert_node_stopped(&node_info, "Node should not be running after graceful stop");
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_stop_workspace_force_integration() {
        // Safety net: NodeGuard ensures cleanup if test fails before stop_workspace runs.
        // NodeGuard is a no-op for already-dead PIDs, so it won't conflict with stop_workspace.
        let (tw, node_info, _guard) = start_test_node("integ_stop_force");

        // Verify node is running before we stop it
        assert!(
            is_node_running(&node_info, Some(&tw.id)),
            "Node should be running before force stop"
        );

        // Force stop (SIGKILL) should succeed
        let result = stop_workspace(Some(&tw.id), true);
        assert!(
            result.is_ok(),
            "Force stop should succeed, got: {:?}",
            result.err()
        );

        assert_node_stopped(&node_info, "Node should not be running after force stop");
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_stop_workspace_not_running_integration() {
        let tw = TestWorkspace::new("integ_stop_notrun");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        // Workspace exists but no node is running
        let result = stop_workspace(Some(&tw.id), false);
        assert!(
            result.is_err(),
            "Stopping non-running workspace should fail"
        );
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("not running"),
            "Error should indicate workspace is not running, got: {err}"
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_generate_workspace_id_rejects_non_utf8_path() {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;

        // Include PID in name to avoid collisions in parallel test runs
        let tmp = std::env::temp_dir();
        let mut invalid_bytes = b"beamtalk-test-\xff\xfe-".to_vec();
        invalid_bytes.extend_from_slice(std::process::id().to_string().as_bytes());
        let invalid_name = OsStr::from_bytes(&invalid_bytes);
        let non_utf8_path = tmp.join(invalid_name);

        // Create the directory so canonicalize() can succeed.
        // Skip test if the filesystem rejects non-UTF8 names.
        if let Err(e) = fs::create_dir(&non_utf8_path) {
            if e.kind() != std::io::ErrorKind::AlreadyExists {
                eprintln!("skipping test: failed to create non-UTF8 dir {non_utf8_path:?}: {e}");
                return;
            }
        }

        let result = generate_workspace_id(&non_utf8_path);
        // Clean up before asserting
        let _ = fs::remove_dir(&non_utf8_path);

        assert!(result.is_err(), "Non-UTF8 path should produce an error");
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("invalid UTF-8"),
            "Error should mention invalid UTF-8, got: {err}"
        );
    }
}
