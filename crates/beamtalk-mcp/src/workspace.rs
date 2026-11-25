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
pub async fn discover_any_port_cookie_and_id() -> Result<Option<(u16, String, String)>, String> {
    tracing::debug!("scanning for a running workspace for the current project");
    let Ok(root) = beamtalk_workspace::workspaces_base_dir() else {
        return Ok(None);
    };
    let cwd_id = std::env::current_dir()
        .ok()
        .and_then(|cwd| generate_workspace_id(&cwd));
    discover_any_in(&root, cwd_id.as_deref()).await
}

/// [`discover_any_port_cookie_and_id`] over an explicit workspaces root and
/// current-directory workspace ID.
async fn discover_any_in(
    root: &Path,
    cwd_workspace_id: Option<&str>,
) -> Result<Option<(u16, String, String)>, String> {
    let Ok(entries) = std::fs::read_dir(root) else {
        return Ok(None);
    };
    let candidates: Vec<(u16, String, String, Option<String>, std::path::PathBuf)> = entries
        .flatten()
        .filter(|entry| entry.file_type().is_ok_and(|ft| ft.is_dir()))
        .filter_map(|entry| {
            let id = entry.file_name().to_string_lossy().into_owned();
            let (port, nonce) = beamtalk_workspace::read_port_file_under(root, &id)
                .ok()
                .flatten()?;
            let cookie = beamtalk_workspace::read_cookie_file_under(root, &id)
                .ok()
                .flatten()?;
            Some((port, cookie, id, nonce, entry.path()))
        })
        .collect();
    // BT-5188: probed concurrently, so one slow or starting candidate does
    // not hold up the others.
    let mut live: Vec<(u16, String, String)> =
        futures_util::future::join_all(candidates.into_iter().map(
            |(port, cookie, id, nonce, dir)| async move {
                candidate_is_live(&dir, port, nonce.as_deref())
                    .await
                    .then_some((port, cookie, id))
            },
        ))
        .await
        .into_iter()
        .flatten()
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

/// How long each step of an identity probe may take (BT-5188): it includes
/// a round trip through the node's REPL server, so it is longer than a bare
/// TCP connect.
const IDENTITY_PROBE_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(1);

/// How long a candidate whose recorded process is alive may take to start
/// accepting connections before it counts as dead (BT-5188).
const STARTING_GRACE: std::time::Duration = std::time::Duration::from_secs(3);

/// Whether `node.info` in `dir` records a process, and if so whether it is
/// still the same one ([`beamtalk_workspace::pid_liveness`]). `None` when no
/// pid is recorded.
fn recorded_process_alive(dir: &Path) -> Option<bool> {
    let content = std::fs::read_to_string(dir.join("node.info")).ok()?;
    let info = serde_json::from_str::<serde_json::Value>(&content).ok()?;
    let pid = info
        .get("pid")
        .and_then(serde_json::Value::as_u64)
        .and_then(|p| u32::try_from(p).ok())?;
    let start_time = info.get("start_time").and_then(serde_json::Value::as_u64);
    Some(
        beamtalk_workspace::pid_liveness::is_process_alive(pid)
            && beamtalk_workspace::pid_liveness::still_same_process(pid, start_time),
    )
}

/// Whether the workspace in `dir` is alive and is really the workspace its
/// port file names — not a crashed node's leftovers, and not some other
/// process that reused the port (BT-5188).
///
/// - When `node.info` records the BEAM node's `pid`, that process must still
///   be the same one.
/// - The process on `port` must prove it knows the port file's `nonce`
///   ([`crate::client::probe_workspace_identity`]); no cookie is sent. A
///   port file without a nonce cannot be verified, so it is not attached to.
/// - A workspace whose recorded process is alive but which does not accept
///   connections yet is still starting: it gets [`STARTING_GRACE`] to come
///   up rather than counting as dead.
async fn candidate_is_live(dir: &Path, port: u16, nonce: Option<&str>) -> bool {
    use crate::client::{ProbeOutcome, probe_workspace_identity};

    let process = recorded_process_alive(dir);
    if process == Some(false) {
        return false;
    }
    let Some(nonce) = nonce else {
        tracing::warn!(port, dir = %dir.display(), "port file has no nonce; cannot verify the workspace");
        return false;
    };
    let deadline = tokio::time::Instant::now()
        + if process == Some(true) {
            STARTING_GRACE
        } else {
            std::time::Duration::ZERO
        };
    loop {
        match probe_workspace_identity(port, nonce, IDENTITY_PROBE_TIMEOUT).await {
            ProbeOutcome::Verified => return true,
            ProbeOutcome::NotTheWorkspace => {
                tracing::warn!(
                    port,
                    dir = %dir.display(),
                    "the process on this workspace's port is not that workspace; skipping it"
                );
                return false;
            }
            ProbeOutcome::Unreachable if tokio::time::Instant::now() >= deadline => return false,
            ProbeOutcome::Unreachable => {
                tokio::time::sleep(std::time::Duration::from_millis(250)).await;
            }
        }
    }
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
    // workspaces root, no `~/.beamtalk`, fake workspaces on ephemeral ports).
    // BT-5188: a live candidate must prove it knows its port file's nonce. ---

    use beamtalk_repl_protocol::test_support::{self, FAKE_NONCE, FakeWsServer, HandshakeMode};

    fn write_workspace_with_nonce(root: &Path, id: &str, port: u16, nonce: &str, pid: Option<u32>) {
        let dir = root.join(id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), format!("{port}\n{nonce}")).unwrap();
        fs::write(dir.join("cookie"), "cookie").unwrap();
        if let Some(pid) = pid {
            fs::write(dir.join("node.info"), format!("{{\"pid\": {pid}}}")).unwrap();
        }
    }

    fn write_workspace(root: &Path, id: &str, port: u16, pid: Option<u32>) {
        write_workspace_with_nonce(root, id, port, FAKE_NONCE, pid);
    }

    /// A port nothing listens on.
    fn dead_port() -> u16 {
        let l = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let port = l.local_addr().unwrap().port();
        drop(l);
        port
    }

    /// A fake workspace that proves it knows [`FAKE_NONCE`].
    async fn live_workspace() -> (FakeWsServer, u16) {
        fake(HandshakeMode::Ok).await
    }

    async fn fake(mode: HandshakeMode) -> (FakeWsServer, u16) {
        let server = test_support::spawn(mode, "sess", Box::new(|_| Vec::new())).await;
        let port = server.port;
        (server, port)
    }

    #[tokio::test]
    async fn discovery_skips_dead_workspace_leftovers() {
        let root = tempfile::tempdir().unwrap();
        let (_l, live) = live_workspace().await;
        write_workspace(root.path(), "aaaa_dead", dead_port(), None);
        write_workspace(root.path(), "zzzz_live", live, None);
        let found = discover_any_in(root.path(), Some("zzzz_live"))
            .await
            .unwrap()
            .unwrap();
        assert_eq!(found.2, "zzzz_live");
        assert_eq!(found.0, live);
    }

    #[tokio::test]
    async fn discovery_skips_workspace_whose_recorded_pid_is_gone() {
        let root = tempfile::tempdir().unwrap();
        let (_l, port) = live_workspace().await;
        // Port answers (reused by someone else) but node.info names a pid
        // that cannot exist.
        write_workspace(root.path(), "stale", port, Some(u32::MAX - 1));
        assert_eq!(discover_any_in(root.path(), None).await.unwrap(), None);
    }

    #[tokio::test]
    async fn discovery_prefers_workspace_for_current_directory() {
        let root = tempfile::tempdir().unwrap();
        let (_l1, p1) = live_workspace().await;
        let (_l2, p2) = live_workspace().await;
        write_workspace(root.path(), "other", p1, None);
        write_workspace(root.path(), "mine", p2, None);
        let found = discover_any_in(root.path(), Some("mine"))
            .await
            .unwrap()
            .unwrap();
        assert_eq!(found.2, "mine");
    }

    #[tokio::test]
    async fn discovery_with_several_live_workspaces_and_no_match_is_an_error() {
        let root = tempfile::tempdir().unwrap();
        let (_l1, p1) = live_workspace().await;
        let (_l2, p2) = live_workspace().await;
        write_workspace(root.path(), "one", p1, None);
        write_workspace(root.path(), "two", p2, None);
        let err = discover_any_in(root.path(), Some("neither"))
            .await
            .unwrap_err();
        assert!(err.contains("one") && err.contains("two"), "got: {err}");
        assert!(err.contains("--workspace-id"), "got: {err}");
    }

    /// BT-5080: one live workspace belonging to another project is NOT
    /// attached to; the error names it and says how to attach explicitly.
    #[tokio::test]
    async fn discovery_with_one_other_live_workspace_is_an_error_naming_it() {
        let root = tempfile::tempdir().unwrap();
        let (_l, p) = live_workspace().await;
        write_workspace(root.path(), "solo", p, None);
        let err = discover_any_in(root.path(), Some("not-it"))
            .await
            .unwrap_err();
        assert!(err.contains("`solo`"), "names the other workspace: {err}");
        assert!(
            err.contains("--workspace-id solo"),
            "says how to attach: {err}"
        );
        assert!(err.contains("Not attaching"), "got: {err}");
    }

    #[tokio::test]
    async fn discovery_with_no_live_workspace_is_none() {
        let root = tempfile::tempdir().unwrap();
        write_workspace(root.path(), "dead", dead_port(), None);
        assert_eq!(
            discover_any_in(root.path(), Some("mine")).await.unwrap(),
            None
        );
    }

    /// BT-5188: a port reused by a process that is not the workspace (here a
    /// bare TCP listener) must not count as live: the cookie would be sent
    /// to it.
    #[tokio::test]
    async fn discovery_skips_a_reused_port_that_is_not_the_workspace() {
        let root = tempfile::tempdir().unwrap();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        // Accept and hold connections, never speaking WebSocket.
        let _impostor = tokio::spawn(async move {
            let mut held = Vec::new();
            while let Ok((sock, _)) = listener.accept().await {
                held.push(sock);
            }
        });
        write_workspace(root.path(), "reused", port, None);
        assert_eq!(
            discover_any_in(root.path(), Some("reused")).await.unwrap(),
            None
        );
    }

    /// BT-5188: a WebSocket server that answers the identity challenge
    /// wrongly (another workspace now on this port) is skipped, and never
    /// receives an `auth` frame.
    #[tokio::test]
    async fn discovery_skips_a_server_that_fails_the_identity_check() {
        let root = tempfile::tempdir().unwrap();
        let (server, port) = fake(HandshakeMode::WrongIdentity).await;
        write_workspace(root.path(), "impostor", port, None);
        assert_eq!(
            discover_any_in(root.path(), Some("impostor"))
                .await
                .unwrap(),
            None
        );
        let seen = server.seen.lock().await.clone();
        assert!(
            seen.iter().all(|f| f["type"] != "auth"),
            "no cookie was sent: {seen:?}"
        );
        assert!(seen.iter().any(|f| f["type"] == "identify"), "{seen:?}");
    }

    /// BT-5188: a stale port file whose nonce no longer matches the node on
    /// that port (it restarted) is skipped.
    #[tokio::test]
    async fn discovery_skips_a_port_file_with_a_stale_nonce() {
        let root = tempfile::tempdir().unwrap();
        let (_l, port) = live_workspace().await;
        write_workspace_with_nonce(root.path(), "restarted", port, "old-nonce", None);
        assert_eq!(
            discover_any_in(root.path(), Some("restarted"))
                .await
                .unwrap(),
            None
        );
    }

    /// BT-5188: a workspace whose process is alive but that is not accepting
    /// connections yet is given time to start, not counted as dead.
    #[tokio::test]
    async fn discovery_waits_for_a_workspace_that_is_still_starting() {
        use futures_util::{SinkExt, StreamExt};
        use tokio_tungstenite::tungstenite::Message;
        let root = tempfile::tempdir().unwrap();
        // Bound now (so no other test can take the port) but not answering
        // until later, like a node that is up but not serving yet.
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        write_workspace(root.path(), "booting", port, Some(std::process::id()));
        let _server = tokio::spawn(async move {
            // sleep: the workspace starts serving after discovery has started.
            tokio::time::sleep(std::time::Duration::from_millis(1500)).await;
            while let Ok((sock, _)) = listener.accept().await {
                tokio::spawn(async move {
                    let Ok(mut ws) = tokio_tungstenite::accept_async(sock).await else {
                        return;
                    };
                    let _ = ws
                        .send(Message::Text(r#"{"op":"auth-required"}"#.into()))
                        .await;
                    if let Some(Ok(Message::Text(body))) = ws.next().await {
                        let req: serde_json::Value =
                            serde_json::from_str(&body).unwrap_or_default();
                        let challenge = req["challenge"].as_str().unwrap_or_default();
                        let proof = beamtalk_repl_protocol::handshake::identity_proof(
                            FAKE_NONCE, challenge,
                        );
                        let reply = serde_json::json!({"type": "identity", "proof": proof});
                        let _ = ws.send(Message::Text(reply.to_string().into())).await;
                    }
                });
            }
        });
        let found = discover_any_in(root.path(), Some("booting")).await.unwrap();
        assert_eq!(found.map(|f| f.2), Some("booting".to_string()));
    }
}
