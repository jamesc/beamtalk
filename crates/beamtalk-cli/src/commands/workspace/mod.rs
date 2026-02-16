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

/// CLI subcommands for workspace management (`workspace list`, `workspace stop`, etc.).
pub mod cli;
/// Project root discovery and workspace auto-detection.
pub mod discovery;
/// Workspace lifecycle operations: create, start, stop, list, status.
mod lifecycle;
/// Process management for workspace BEAM nodes.
mod process;
/// File I/O operations for workspace metadata, cookies, and node information.
pub mod storage;

pub use lifecycle::{
    create_workspace, get_or_start_workspace, list_workspaces, stop_workspace, workspace_status,
};
pub use process::is_node_running;
pub use storage::{
    cleanup_stale_node_info, get_node_info, get_workspace_metadata,
    workspace_exists, workspace_id_for,
};

#[cfg(test)]
mod tests {
    use super::*;
    use super::lifecycle::{
        find_workspace_by_project_path, resolve_workspace_id, WorkspaceStatus,
    };
    use super::process::wait_for_workspace_exit;
    use super::storage::{
        generate_cookie, generate_workspace_id, read_port_file, read_workspace_cookie,
        save_node_info, save_workspace_cookie, save_workspace_metadata, validate_workspace_name,
        workspace_dir, workspaces_base_dir, NodeInfo, WorkspaceMetadata,
    };
    #[cfg(target_os = "linux")]
    use super::storage::read_proc_start_time;
    #[cfg(unix)]
    use super::process::{force_kill_process, start_detached_node};
    #[cfg(unix)]
    use serial_test::serial;
    use std::fs;
    use std::path::{Path, PathBuf};
    #[cfg(unix)]
    use std::process::Command;

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
            nonce: None,
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
            nonce: None,
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
            nonce: None,
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
    // Unix-only: uses `ps` for PID verification in some tests.

    /// Guard that kills a BEAM node by PID when dropped, preventing orphans.
    #[cfg(unix)]
    struct NodeGuard {
        pid: u32,
    }

    #[cfg(unix)]
    impl Drop for NodeGuard {
        fn drop(&mut self) {
            let _ = force_kill_process(self.pid);
        }
    }

    /// Locate BEAM directories needed to start a workspace node.
    #[cfg(unix)]
    fn beam_dirs_for_tests() -> (PathBuf, PathBuf, PathBuf, PathBuf, PathBuf) {
        let runtime_dir = beamtalk_cli::repl_startup::find_runtime_dir()
            .expect("Cannot find runtime dir — run from repo root or set BEAMTALK_RUNTIME_DIR");
        let paths = beamtalk_cli::repl_startup::beam_paths(&runtime_dir);
        assert!(
            beamtalk_cli::repl_startup::has_beam_files(&paths.runtime_ebin),
            "Runtime BEAM files not found at {:?}. Run `cd runtime && rebar3 compile` first.",
            paths.runtime_ebin,
        );
        (
            paths.runtime_ebin,
            paths.workspace_ebin,
            paths.jsx_ebin,
            paths.compiler_ebin,
            paths.stdlib_ebin,
        )
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_start_detached_node_integration() {
        let tw = TestWorkspace::new("integ_start");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let (runtime, workspace, jsx, compiler, stdlib) = beam_dirs_for_tests();
        let node_info = start_detached_node(
            &tw.id,
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("start_detached_node should succeed");
        let _guard = NodeGuard { pid: node_info.pid };

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
            is_node_running(&node_info),
            "is_node_running should return true for live node"
        );

        // Verify PID corresponds to a real BEAM process
        let ps_output = Command::new("ps")
            .args(["-p", &node_info.pid.to_string(), "-o", "comm="])
            .output()
            .expect("ps should succeed");
        let comm = String::from_utf8_lossy(&ps_output.stdout);
        assert!(
            comm.contains("beam") || comm.contains("erl"),
            "PID {} should be a BEAM process, got: {}",
            node_info.pid,
            comm.trim()
        );
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_is_node_running_true_then_false_integration() {
        let tw = TestWorkspace::new("integ_running");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let (runtime, workspace, jsx, compiler, stdlib) = beam_dirs_for_tests();
        let node_info = start_detached_node(
            &tw.id,
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("start_detached_node should succeed");
        let _guard = NodeGuard { pid: node_info.pid };

        // True case: node is running
        assert!(
            is_node_running(&node_info),
            "is_node_running should be true while node is alive"
        );

        // Kill the node (use SIGKILL since detached BEAM nodes don't exit on SIGTERM)
        let _ = Command::new("kill")
            .args(["-9", &node_info.pid.to_string()])
            .status();
        // Wait for process to exit (TCP probe will fail once process is dead)
        let _ = wait_for_workspace_exit(node_info.port, 5);

        // False case: node has been killed
        assert!(
            !is_node_running(&node_info),
            "is_node_running should be false after kill"
        );
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_get_or_start_workspace_lifecycle_integration() {
        let tw = TestWorkspace::new("integ_lifecycle");
        let project_path = std::env::current_dir().unwrap();

        let (runtime, workspace, jsx, compiler, stdlib) = beam_dirs_for_tests();

        // Step 1: First call creates workspace and starts node
        let (info1, started1, id1) = get_or_start_workspace(
            &project_path,
            Some(&tw.id),
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("first get_or_start should succeed");
        let _guard1 = NodeGuard { pid: info1.pid };
        assert!(started1, "first call should start a new node");
        assert_eq!(id1, tw.id);
        assert!(is_node_running(&info1), "node should be running");

        // Step 2: Second call reconnects to existing node
        let (info2, started2, id2) = get_or_start_workspace(
            &project_path,
            Some(&tw.id),
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("reconnect should succeed");
        assert!(!started2, "second call should reuse existing node");
        assert_eq!(id2, tw.id);
        assert_eq!(info2.pid, info1.pid, "should return same PID");

        // Step 3: Stop the node gracefully via TCP shutdown
        stop_workspace(&tw.id, false).expect("graceful stop should succeed");
        assert!(
            !is_node_running(&info1),
            "node should not be running after stop"
        );

        // Step 4: Third call starts a new node
        let (info3, started3, id3) = get_or_start_workspace(
            &project_path,
            Some(&tw.id),
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("restart should succeed");
        let _guard3 = NodeGuard { pid: info3.pid };
        assert!(started3, "third call should start a new node");
        assert_eq!(id3, tw.id);
        assert_ne!(
            info3.pid, info1.pid,
            "restarted node should have different PID"
        );
        assert!(is_node_running(&info3), "restarted node should be running");
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_list_workspaces_running_node_integration() {
        let tw = TestWorkspace::new("integ_list_running");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let (runtime, workspace, jsx, compiler, stdlib) = beam_dirs_for_tests();
        let node_info = start_detached_node(
            &tw.id,
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("start_detached_node should succeed");
        let _guard = NodeGuard { pid: node_info.pid };

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
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_list_workspaces_stale_cleanup_integration() {
        let tw = TestWorkspace::new("integ_list_stale");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let (runtime, workspace, jsx, compiler, stdlib) = beam_dirs_for_tests();
        let node_info = start_detached_node(
            &tw.id,
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("start_detached_node should succeed");

        // Kill the node without cleanup (simulating crash)
        let _ = Command::new("kill")
            .args(["-9", &node_info.pid.to_string()])
            .status();
        let _ = wait_for_workspace_exit(node_info.port, 5);

        // list_workspaces should detect stale node.info and report Stopped
        let workspaces = list_workspaces().unwrap();
        let found = workspaces
            .iter()
            .find(|w| w.workspace_id == tw.id)
            .expect("Should find the workspace in list");

        assert_eq!(
            found.status,
            WorkspaceStatus::Stopped,
            "Stale workspace should be reported as Stopped"
        );
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_workspace_status_running_integration() {
        let tw = TestWorkspace::new("integ_ws_status");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let (runtime, workspace, jsx, compiler, stdlib) = beam_dirs_for_tests();
        let node_info = start_detached_node(
            &tw.id,
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("start_detached_node should succeed");
        let _guard = NodeGuard { pid: node_info.pid };

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
        let tw = TestWorkspace::new("integ_stop_graceful");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let (runtime, workspace, jsx, compiler, stdlib) = beam_dirs_for_tests();
        let node_info = start_detached_node(
            &tw.id,
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("start_detached_node should succeed");
        let _guard = NodeGuard { pid: node_info.pid };

        // Graceful stop (force=false) uses TCP shutdown + init:stop(),
        // which should succeed for detached BEAM nodes
        let result = stop_workspace(&tw.id, false);
        assert!(
            result.is_ok(),
            "Graceful TCP shutdown should succeed, got: {:?}",
            result.err()
        );

        // Node should no longer be running after graceful stop
        assert!(
            !is_node_running(&node_info),
            "Node should not be running after graceful stop"
        );
    }

    #[test]
    #[cfg(unix)]
    #[ignore = "integration test — requires Erlang/OTP runtime"]
    #[serial(workspace_integration)]
    fn test_stop_workspace_force_integration() {
        let tw = TestWorkspace::new("integ_stop_force");
        let project_path = std::env::current_dir().unwrap();
        let _ = create_workspace(&project_path, Some(&tw.id)).unwrap();

        let (runtime, workspace, jsx, compiler, stdlib) = beam_dirs_for_tests();
        let node_info = start_detached_node(
            &tw.id,
            0,
            &runtime,
            &workspace,
            &jsx,
            &compiler,
            &stdlib,
            &[],
            false,
            Some(60),
        )
        .expect("start_detached_node should succeed");
        // Safety net: NodeGuard ensures cleanup if test fails before stop_workspace runs.
        // NodeGuard is a no-op for already-dead PIDs, so it won't conflict with stop_workspace.
        let _guard = NodeGuard { pid: node_info.pid };

        // Verify node is running before we stop it
        assert!(
            is_node_running(&node_info),
            "Node should be running before force stop"
        );

        // Force stop (SIGKILL) should succeed
        let result = stop_workspace(&tw.id, true);
        assert!(
            result.is_ok(),
            "Force stop should succeed, got: {:?}",
            result.err()
        );

        // Node should no longer be running
        assert!(
            !is_node_running(&node_info),
            "Node should not be running after force stop"
        );
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
        let result = stop_workspace(&tw.id, false);
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
