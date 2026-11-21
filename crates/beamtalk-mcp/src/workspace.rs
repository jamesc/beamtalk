// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace discovery for finding a running REPL server's port.
//!
//! **DDD Context:** Language Service / Interactive Development
//!
//! Delegates workspace ID generation and file I/O to `beamtalk-workspace`.

use std::path::Path;

/// Generate a workspace ID from a project path (SHA256 hex, first 12 chars).
///
/// Returns `None` if the path cannot be canonicalized or contains non-UTF-8 bytes.
pub fn generate_workspace_id(path: &Path) -> Option<String> {
    beamtalk_workspace::generate_workspace_id(path)
        .inspect_err(|err| {
            tracing::warn!(
                "Failed to generate workspace ID for {}: {err}",
                path.display()
            );
        })
        .ok()
}

/// Read the port file for a workspace.
/// Port file format: `PORT\nNONCE` (two lines). Only the port is returned.
pub fn read_port_file(workspace_id: &str) -> Option<u16> {
    beamtalk_workspace::read_port_file(workspace_id)
        .ok()
        .flatten()
        .map(|(port, _nonce)| port)
}

/// Read the cookie file for a workspace.
pub fn read_cookie_file(workspace_id: &str) -> Option<String> {
    beamtalk_workspace::read_cookie_file(workspace_id)
        .ok()
        .flatten()
}

/// Discover the REPL port and cookie for the current directory.
///
/// 1. If a workspace ID is given, use it directly.
/// 2. Otherwise, generate from the current directory.
#[cfg(test)]
pub fn discover_port_and_cookie(workspace_id: Option<&str>) -> Option<(u16, String)> {
    tracing::debug!(workspace_id = ?workspace_id, "discovering REPL port and cookie");
    discover_port_cookie_and_id(workspace_id).map(|(port, cookie, _id)| (port, cookie))
}

/// Discover the REPL port, cookie, and resolved workspace ID
/// so callers can pass it to `ReplClient` for port re-discovery on reconnect.
pub fn discover_port_cookie_and_id(workspace_id: Option<&str>) -> Option<(u16, String, String)> {
    let id = if let Some(id) = workspace_id {
        id.to_string()
    } else {
        let cwd = std::env::current_dir().ok()?;
        generate_workspace_id(&cwd)?
    };
    let port = read_port_file(&id)?;
    let cookie = read_cookie_file(&id)?;
    tracing::info!(port, workspace_id = %id, "discovered REPL port and cookie");
    Some((port, cookie, id))
}

/// Discover the REPL port for the current directory.
///
/// 1. If a workspace ID is given, use it directly.
/// 2. Otherwise, generate from the current directory.
#[cfg(test)]
pub fn discover_port(workspace_id: Option<&str>) -> Option<u16> {
    discover_port_and_cookie(workspace_id).map(|(port, _)| port)
}

/// Parse the REPL port from `beamtalk repl` stdout.
///
/// Expects a line like: `Connected to REPL backend on port 12345.`
pub fn parse_repl_port(stdout: &str) -> Option<u16> {
    beamtalk_workspace::parse_repl_port(stdout)
}

/// Parse the workspace ID from `beamtalk repl` stdout.
///
/// Expects a line like: `  Workspace: abc123def456 (new)`
pub fn parse_workspace_id(stdout: &str) -> Option<String> {
    beamtalk_workspace::parse_repl_workspace_id(stdout)
}

/// How long a candidate workspace has to accept a TCP connection before it
/// is considered dead.
const LIVENESS_PROBE_TIMEOUT: std::time::Duration = std::time::Duration::from_millis(500);

/// Find the live workspace for the current project and return its port,
/// cookie, and workspace ID.
///
/// Only the workspace for the current directory is ever attached to
/// (BT-5080). Other live workspaces belong to other projects, and a session
/// started in project A must not run `flush`, `remove_class` or
/// `rename_class` against project B. When the current project has no live
/// workspace but others are running, this is an error naming them and saying
/// how to attach explicitly. Scans `~/.beamtalk/workspaces/` and keeps only
/// live candidates (see [`candidate_is_live`]).
///
/// Returns the workspace ID for port re-discovery on reconnect.
///
/// # Errors
///
/// Returns a message naming the other running workspaces when the current
/// project has none of its own.
pub fn discover_any_port_cookie_and_id() -> Result<Option<(u16, String, String)>, String> {
    tracing::debug!("scanning for a running workspace for the current project");
    let Ok(root) = beamtalk_workspace::workspaces_base_dir() else {
        return Ok(None);
    };
    let cwd_id = std::env::current_dir()
        .ok()
        .and_then(|cwd| generate_workspace_id(&cwd));
    discover_any_in(&root, cwd_id.as_deref())
}

/// [`discover_any_port_cookie_and_id`] over an explicit workspaces root and
/// current-directory workspace ID.
fn discover_any_in(
    root: &Path,
    cwd_workspace_id: Option<&str>,
) -> Result<Option<(u16, String, String)>, String> {
    let Ok(entries) = std::fs::read_dir(root) else {
        return Ok(None);
    };
    let mut live: Vec<(u16, String, String)> = entries
        .flatten()
        .filter(|entry| entry.file_type().is_ok_and(|ft| ft.is_dir()))
        .filter_map(|entry| {
            let id = entry.file_name().to_string_lossy().into_owned();
            let port = beamtalk_workspace::read_port_file_under(root, &id)
                .ok()
                .flatten()
                .map(|(port, _nonce)| port)?;
            let cookie = beamtalk_workspace::read_cookie_file_under(root, &id)
                .ok()
                .flatten()?;
            candidate_is_live(&entry.path(), port).then_some((port, cookie, id))
        })
        .collect();
    // `read_dir` order is unspecified; sort so errors and tests are stable.
    live.sort_by(|a, b| a.2.cmp(&b.2));

    if let Some(id) = cwd_workspace_id
        && let Some(pos) = live.iter().position(|(_, _, ws)| ws == id)
    {
        let found = live.swap_remove(pos);
        tracing::info!(port = found.0, workspace_id = %found.2, "using workspace for current directory");
        return Ok(Some(found));
    }
    if live.is_empty() {
        tracing::debug!("no running workspace found");
        return Ok(None);
    }
    let ids: Vec<&str> = live.iter().map(|(_, _, id)| id.as_str()).collect();
    let (what, which) = if ids.len() == 1 {
        ("another workspace is running", format!("`{}`", ids[0]))
    } else {
        (
            "other workspaces are running",
            ids.iter()
                .map(|id| format!("`{id}`"))
                .collect::<Vec<_>>()
                .join(", "),
        )
    };
    Err(format!(
        "No running beamtalk workspace for this project, but {what}: {which}. \
         Not attaching to a different project's workspace. Start one for this project \
         with 'beamtalk repl' or --start, or attach explicitly with --workspace-id <id> \
         (e.g. --workspace-id {}) or --port.",
        ids[0]
    ))
}

/// Whether the workspace in `dir` is alive, not a crashed node's leftovers.
///
/// Two checks, both must hold:
/// - when `node.info` records the BEAM node's `pid` (and, where the platform
///   provides one, its `start_time`), that process is still the same one
///   ([`beamtalk_workspace::pid_liveness`]); and
/// - something accepts a TCP connection on the recorded port.
fn candidate_is_live(dir: &Path, port: u16) -> bool {
    if let Ok(content) = std::fs::read_to_string(dir.join("node.info"))
        && let Ok(info) = serde_json::from_str::<serde_json::Value>(&content)
        && let Some(pid) = info
            .get("pid")
            .and_then(serde_json::Value::as_u64)
            .and_then(|p| u32::try_from(p).ok())
    {
        let start_time = info.get("start_time").and_then(serde_json::Value::as_u64);
        if !beamtalk_workspace::pid_liveness::is_process_alive(pid)
            || !beamtalk_workspace::pid_liveness::still_same_process(pid, start_time)
        {
            return false;
        }
    }
    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], port));
    std::net::TcpStream::connect_timeout(&addr, LIVENESS_PROBE_TIMEOUT).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_generate_workspace_id_deterministic() {
        let path = std::env::temp_dir();
        let id1 = generate_workspace_id(&path).unwrap();
        let id2 = generate_workspace_id(&path).unwrap();
        assert_eq!(id1, id2, "Same path should produce same workspace ID");
    }

    #[test]
    fn test_generate_workspace_id_length() {
        let path = std::env::temp_dir();
        let id = generate_workspace_id(&path).unwrap();
        assert_eq!(
            id.len(),
            12,
            "Workspace ID should be 12 hex chars (6 bytes)"
        );
    }

    #[test]
    fn test_generate_workspace_id_hex_format() {
        let path = std::env::temp_dir();
        let id = generate_workspace_id(&path).unwrap();
        assert!(
            id.chars().all(|c| c.is_ascii_hexdigit()),
            "Workspace ID should be all hex digits, got: {id}"
        );
    }

    #[test]
    fn test_different_paths_produce_different_ids() {
        let temp = std::env::temp_dir();
        let cwd = std::env::current_dir().unwrap();
        if temp != cwd {
            let id1 = generate_workspace_id(&temp).unwrap();
            let id2 = generate_workspace_id(&cwd).unwrap();
            assert_ne!(id1, id2, "Different paths should produce different IDs");
        }
    }

    #[test]
    fn test_read_port_file_missing_workspace() {
        let result = read_port_file("nonexistent_workspace_id_abc123");
        assert_eq!(result, None, "Missing workspace should return None");
    }

    #[test]
    fn test_read_port_file_valid() {
        let workspace_id = format!("test_mcp_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "9876\nnonce123").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, Some(9876));

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_two_line_format() {
        let workspace_id = format!("test_mcp_twoln_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        // Port file has PORT\nNONCE format; reader must parse only first line
        fs::write(dir.join("port"), "9876\nabc123nonce\n").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, Some(9876), "Should parse port from first line only");

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_invalid_content() {
        let workspace_id = format!("test_mcp_invalid_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "not_a_number\n").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, None, "Invalid port file content should return None");

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_empty() {
        let workspace_id = format!("test_mcp_empty_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, None, "Empty port file should return None");

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_discover_port_with_explicit_id() {
        let workspace_id = format!("test_mcp_discover_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "5555\nnonce456").unwrap();
        fs::write(dir.join("cookie"), "testcookie").unwrap();

        let result = discover_port(Some(&workspace_id));
        assert_eq!(result, Some(5555));

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_discover_port_missing_id() {
        let result = discover_port(Some("nonexistent_workspace_xyz"));
        assert_eq!(result, None);
    }

    #[test]
    fn test_workspace_dir_rejects_path_traversal() {
        assert!(beamtalk_workspace::workspace_dir("../etc").is_err());
        assert!(beamtalk_workspace::workspace_dir("..").is_err());
        assert!(beamtalk_workspace::workspace_dir("foo/bar").is_err());
        assert!(beamtalk_workspace::workspace_dir("foo\\bar").is_err());
        assert!(beamtalk_workspace::workspace_dir("").is_err());
    }

    #[test]
    fn test_workspace_dir_accepts_valid_ids() {
        let dir = beamtalk_workspace::workspace_dir("abc123def456");
        assert!(dir.is_ok());
        assert!(dir.unwrap().ends_with("abc123def456"));
    }

    #[test]
    fn test_discover_port_cookie_and_id_returns_workspace_id() {
        let workspace_id = format!("test_mcp_pcid_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "7777\nnonce789").unwrap();
        fs::write(dir.join("cookie"), "testcookie").unwrap();

        let result = discover_port_cookie_and_id(Some(&workspace_id));
        assert_eq!(
            result,
            Some((7777, "testcookie".to_string(), workspace_id.clone()))
        );

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    /// Verify that re-reading a port file after the port changes
    /// returns the new port. This is the mechanism that allows reconnect to
    /// find the new port after a workspace restart.
    #[test]
    fn test_port_file_reread_after_change() {
        let workspace_id = format!("test_mcp_reread_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();

        // Write initial port
        fs::write(dir.join("port"), "36109\nnonce1").unwrap();
        assert_eq!(read_port_file(&workspace_id), Some(36109));

        // Simulate workspace restart — port file updated with new port
        fs::write(dir.join("port"), "38901\nnonce2").unwrap();
        assert_eq!(read_port_file(&workspace_id), Some(38901));

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    // --- BT-5029: auto-discovery liveness / ambiguity (hermetic: a private
    // workspaces root, no `~/.beamtalk`, listeners on ephemeral ports) ---

    fn write_workspace(root: &Path, id: &str, port: u16, pid: Option<u32>) {
        let dir = root.join(id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), format!("{port}\nnonce")).unwrap();
        fs::write(dir.join("cookie"), "cookie").unwrap();
        if let Some(pid) = pid {
            fs::write(dir.join("node.info"), format!("{{\"pid\": {pid}}}")).unwrap();
        }
    }

    /// A bound port that nothing listens on: bind, record, drop would race
    /// other tests, so use a listener we keep alive for the live case and a
    /// port from a listener we have *closed* only for the dead case.
    fn dead_port() -> u16 {
        let l = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let port = l.local_addr().unwrap().port();
        drop(l);
        port
    }

    fn live_listener() -> (std::net::TcpListener, u16) {
        let l = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let port = l.local_addr().unwrap().port();
        (l, port)
    }

    #[test]
    fn discovery_skips_dead_workspace_leftovers() {
        let root = tempfile::tempdir().unwrap();
        let (_l, live) = live_listener();
        write_workspace(root.path(), "aaaa_dead", dead_port(), None);
        write_workspace(root.path(), "zzzz_live", live, None);
        let found = discover_any_in(root.path(), Some("zzzz_live"))
            .unwrap()
            .unwrap();
        assert_eq!(found.2, "zzzz_live");
        assert_eq!(found.0, live);
    }

    #[test]
    fn discovery_skips_workspace_whose_recorded_pid_is_gone() {
        let root = tempfile::tempdir().unwrap();
        let (_l, port) = live_listener();
        // Port answers (reused by someone else) but node.info names a pid
        // that cannot exist.
        write_workspace(root.path(), "stale", port, Some(u32::MAX - 1));
        assert_eq!(discover_any_in(root.path(), None).unwrap(), None);
    }

    #[test]
    fn discovery_prefers_workspace_for_current_directory() {
        let root = tempfile::tempdir().unwrap();
        let (_l1, p1) = live_listener();
        let (_l2, p2) = live_listener();
        write_workspace(root.path(), "other", p1, None);
        write_workspace(root.path(), "mine", p2, None);
        let found = discover_any_in(root.path(), Some("mine")).unwrap().unwrap();
        assert_eq!(found.2, "mine");
    }

    #[test]
    fn discovery_with_several_live_workspaces_and_no_match_is_an_error() {
        let root = tempfile::tempdir().unwrap();
        let (_l1, p1) = live_listener();
        let (_l2, p2) = live_listener();
        write_workspace(root.path(), "one", p1, None);
        write_workspace(root.path(), "two", p2, None);
        let err = discover_any_in(root.path(), Some("neither")).unwrap_err();
        assert!(err.contains("one") && err.contains("two"), "got: {err}");
        assert!(err.contains("--workspace-id"), "got: {err}");
    }

    /// BT-5080: one live workspace belonging to another project is NOT
    /// attached to; the error names it and says how to attach explicitly.
    #[test]
    fn discovery_with_one_other_live_workspace_is_an_error_naming_it() {
        let root = tempfile::tempdir().unwrap();
        let (_l, p) = live_listener();
        write_workspace(root.path(), "solo", p, None);
        let err = discover_any_in(root.path(), Some("not-it")).unwrap_err();
        assert!(err.contains("`solo`"), "names the other workspace: {err}");
        assert!(
            err.contains("--workspace-id solo"),
            "says how to attach: {err}"
        );
        assert!(err.contains("Not attaching"), "got: {err}");
    }

    #[test]
    fn discovery_with_no_live_workspace_is_none() {
        let root = tempfile::tempdir().unwrap();
        write_workspace(root.path(), "dead", dead_port(), None);
        assert_eq!(discover_any_in(root.path(), Some("mine")).unwrap(), None);
    }
}
