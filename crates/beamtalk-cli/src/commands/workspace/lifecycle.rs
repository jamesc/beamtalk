// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace lifecycle operations: create, start, stop, list, status.
//!
//! **DDD Context:** CLI

use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use beamtalk_cli::repl_startup::BeamPaths;

use miette::{IntoDiagnostic, Result, miette};
use serde::Serialize;

use super::discovery;
use super::epmd::{
    EPMD_CONFLICT_DEREGISTER_TIMEOUT_SECS, EPMD_CONFLICT_MAX_RETRIES,
    EPMD_CONFLICT_RETRY_INTERVAL_MS, is_epmd_name_conflict, wait_for_epmd_deregistration,
};
use super::node_state::is_node_running;
use super::process::start_detached_node;
use super::storage::{
    NodeInfo, WorkspaceMetadata, acquire_workspace_lock, cleanup_stale_node_info, generate_cookie,
    get_node_info, get_workspace_metadata, save_workspace_cookie, save_workspace_metadata,
    validate_workspace_name, workspace_dir, workspace_exists, workspace_id_for,
    workspaces_base_dir,
};

/// Inner logic for workspace creation, called under an already-held lock.
///
/// Does NOT acquire the lock itself — callers are responsible for holding
/// `acquire_workspace_lock` before calling this function.
fn create_workspace_impl(workspace_id: &str, project_path: &Path) -> Result<WorkspaceMetadata> {
    // Re-check under lock — another process may have created the workspace
    // while we were waiting for the lock.
    if workspace_exists(workspace_id)? {
        // If metadata is valid, return it. If it is corrupted/empty (e.g. from a
        // crashed write), fall through to recreate the workspace metadata.
        if let Ok(metadata) = get_workspace_metadata(workspace_id) {
            // Self-heal a legacy non-absolute `project_path` (BT-3354 case 2): a
            // named workspace created before its project directory existed falls
            // back to storing the raw, relative path (`canonicalize()` fails at
            // creation time, see below) and metadata is never otherwise rewritten.
            // Once the directory exists, this — the next successful lookup by
            // name — can canonicalize it for free using the caller's own
            // `project_path`.
            //
            // This trusts the current call's `project_path` as authoritative for
            // an existing named workspace, which a later `--workspace <name>`
            // invoked from an unrelated directory (typo, wrong cwd, moved
            // project) could abuse to silently repoint the stored path — not
            // just the informational path shown by `workspace list`/`status`,
            // but the value the *next* detached node start for this workspace
            // reads back (see `process.rs`'s `prepare_workspace_paths`) to set
            // the runtime's project root, scoping live-reload file loading and
            // `git` operations. It doesn't affect message routing (that's keyed
            // by `workspace_id`, not this field), and it carries the exact same
            // caller-supplied-path trust the pre-existing creation-time fallback
            // below already has (unchanged since BT-3332) — this just lets that
            // same trust fire again on a later lookup instead of only once at
            // creation. Logged rather than blocked outright; narrowing this
            // further (e.g. requiring the resolved directory to independently
            // look like the same project) is left as a follow-up (BT-3355).
            //
            // Gated on no node currently running for this workspace: healing
            // only ever fires for legacy, not-yet-corrected metadata, so a
            // live, correctly-configured workspace can never have its on-disk
            // `project_path` corrupted invisibly out from under it.
            let node_is_running = get_node_info(workspace_id)
                .ok()
                .flatten()
                .is_some_and(|info| is_node_running(&info, Some(workspace_id)));
            if !metadata.project_path.is_absolute() && !node_is_running {
                if let Ok(canonical_project_path) = project_path.canonicalize() {
                    let healed = WorkspaceMetadata {
                        project_path: canonical_project_path,
                        ..metadata.clone()
                    };
                    // Best-effort: a failed heal write (disk full, permissions)
                    // must not fail workspace creation over a `project_path` that
                    // was already valid and usable — matches this function's
                    // no-error-on-`project_path` contract above.
                    if save_workspace_metadata(&healed).is_ok() {
                        eprintln!(
                            "Updated workspace '{workspace_id}' project_path: '{}' -> '{}' \
                             (run from the wrong directory? re-run from the workspace's \
                             actual project directory to correct it)",
                            metadata.project_path.display(),
                            healed.project_path.display()
                        );
                        return Ok(healed);
                    }
                }
            }
            return Ok(metadata);
        }
    }

    // Create workspace directory
    let dir = workspace_dir(workspace_id)?;
    fs::create_dir_all(&dir).into_diagnostic()?;

    // Only generate a new cookie when one doesn't already exist. Preserving the
    // existing cookie is important when recovering from corrupt metadata: a live
    // node still holds the old cookie in memory, so overwriting it would break
    // reconnection / authentication.
    if !dir.join("cookie").exists() {
        let cookie = generate_cookie();
        save_workspace_cookie(workspace_id, &cookie)?;
    }

    // Create metadata
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .into_diagnostic()?
        .as_secs();

    // Canonicalize before storing: `metadata.json`'s `project_path` must always
    // be an absolute, canonical path, never a bare relative string like "."
    // (BT-3332). A relative stored path later resolves against whichever
    // process happens to read it back, not wherever the workspace was
    // actually created, letting an unrelated caller collide with it. Every
    // caller of `create_workspace_impl` derives `workspace_id` from
    // `workspace_id_for`, which already canonicalizes `project_path` when
    // generating the ID from the path (the `workspace_name` branch skips
    // that, so canonicalize here too rather than assume it happened upstream).
    //
    // Falls back to the raw path rather than erroring when canonicalization
    // fails (e.g. a named workspace created against a not-yet-existing
    // directory) — that matches this function's pre-fix behavior of never
    // rejecting workspace creation on account of `project_path`.
    // `find_workspace_by_project_path`'s defense-in-depth check already
    // treats a non-absolute stored path as unmatchable, so a raw fallback
    // here can't reintroduce the wildcard-match bug.
    let canonical_project_path = project_path
        .canonicalize()
        .unwrap_or_else(|_| project_path.to_path_buf());

    let metadata = WorkspaceMetadata {
        workspace_id: workspace_id.to_string(),
        project_path: canonical_project_path,
        created_at: now,
    };

    save_workspace_metadata(&metadata)?;

    Ok(metadata)
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

    create_workspace_impl(&workspace_id, project_path)
}

/// Get or start a workspace node for the current directory.
/// Returns (`NodeInfo`, bool) where bool indicates if a new node was started.
///
/// `config.otp_app_name` is forwarded to [`start_detached_node`]: when `Some`, the named
/// OTP application is started inside the workspace after class bootstrap completes,
/// ensuring all project classes are registered before the supervisor's `init/1` runs.
pub fn get_or_start_workspace(
    project_path: &Path,
    workspace_name: Option<&str>,
    beam_paths: &BeamPaths,
    extra_code_paths: &[PathBuf],
    config: &super::WorkspaceConfig<'_>,
) -> Result<(NodeInfo, bool, String)> {
    let workspace_id = workspace_id_for(project_path, workspace_name)?;

    // Acquire exclusive lock covering the full check-is-running + start sequence.
    //
    // Without this lock, two concurrent callers (e.g. IDE extension + terminal both
    // starting `beamtalk repl`) can both observe "not running" and both attempt
    // `start_detached_node`, causing the second call to fail when EPMD rejects the
    // duplicate node name.
    //
    // With the lock:
    // - The first caller acquires it, creates the workspace, starts the node, releases the lock.
    // - The second caller blocks until the lock is released, then acquires it, finds the
    //   node already running (written by the first caller), and returns it directly.
    //
    // The lock is released when `_lock` is dropped at end of scope (including error paths).
    let _lock = acquire_workspace_lock(&workspace_id)?;

    // Create workspace if it doesn't exist (under lock)
    create_workspace_impl(&workspace_id, project_path)?;

    // Check if node is already running (under lock)
    if let Some(node_info) = get_node_info(&workspace_id)? {
        if is_node_running(&node_info, Some(&workspace_id)) {
            return Ok((node_info, false, workspace_id)); // Existing node
        }
        // Stale node.info file - orphaned workspace detected
        eprintln!("Cleaning up orphaned workspace: {workspace_id}");
        cleanup_stale_node_info(&workspace_id)?;
    }

    // Start new detached node, retrying if epmd still holds a stale registration
    // from a recently-killed node with the same name.
    //
    // When a BEAM node is force-killed, epmd may retain its name registration
    // for a short period. Erlang rejects the new node startup in that case and
    // exits immediately (the PID file is never written). We detect this via the
    // startup.log, wait for epmd to clear the stale entry using TCP NAMES_REQ,
    // and retry — avoiding a spurious 15-second timeout failure.
    let mut last_err = None;
    for attempt in 0..=EPMD_CONFLICT_MAX_RETRIES {
        if attempt > 0 {
            // Wait for epmd to release the old registration before retrying.
            // Best-effort: proceed even if the wait times out.
            let node_name = format!("beamtalk_workspace_{workspace_id}@localhost");
            let _ = wait_for_epmd_deregistration(&node_name, EPMD_CONFLICT_DEREGISTER_TIMEOUT_SECS);
            std::thread::sleep(Duration::from_millis(EPMD_CONFLICT_RETRY_INTERVAL_MS));
        }

        match start_detached_node(&workspace_id, beam_paths, extra_code_paths, config) {
            Ok(node_info) => return Ok((node_info, true, workspace_id)),
            Err(e) => {
                if attempt < EPMD_CONFLICT_MAX_RETRIES && is_epmd_name_conflict(&workspace_id) {
                    eprintln!(
                        "epmd name conflict for workspace '{workspace_id}' \
                         (attempt {}/{EPMD_CONFLICT_MAX_RETRIES}), retrying...",
                        attempt + 1
                    );
                    last_err = Some(e);
                    continue;
                }
                return Err(e);
            }
        }
    }

    Err(last_err.unwrap_or_else(|| {
        miette!("Failed to start workspace node after {EPMD_CONFLICT_MAX_RETRIES} attempts")
    }))
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

/// Extract a workspace ID string from a workspace directory path.
///
/// Returns `None` for paths that lack a valid UTF-8 filename, have an empty
/// name, or whose name doesn't pass `validate_workspace_name` (e.g. paths
/// ending in `..`, non-UTF-8 characters, or unexpected directory entries).
/// Callers can safely skip `None` entries.
fn workspace_id_from_path(path: &Path) -> Option<String> {
    let id = path
        .file_name()
        .and_then(|n| n.to_str())
        .filter(|s| !s.is_empty())
        .map(str::to_owned)?;
    validate_workspace_name(&id).ok()?;
    Some(id)
}

/// Probe a workspace node's liveness and opportunistically clean up stale files.
///
/// Returns `(Running, Some(NodeInfo))` if the node is alive,
/// or `(Stopped, None)` if the node is not running or has no `node.info`.
/// Stale `node.info` files are removed when the node is detected as stopped.
fn probe_node_status(workspace_id: &str) -> (WorkspaceStatus, Option<NodeInfo>) {
    match get_node_info(workspace_id) {
        Ok(Some(info)) if is_node_running(&info, Some(workspace_id)) => {
            (WorkspaceStatus::Running, Some(info))
        }
        Ok(Some(_)) => {
            let _ = cleanup_stale_node_info(workspace_id);
            (WorkspaceStatus::Stopped, None)
        }
        _ => (WorkspaceStatus::Stopped, None),
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

        let Some(workspace_id) = workspace_id_from_path(&path) else {
            continue;
        };

        let Ok(metadata) = get_workspace_metadata(&workspace_id) else {
            continue;
        };

        let (status, node) = probe_node_status(&workspace_id);
        let port = node.as_ref().map(|i| i.port);
        let pid = node.as_ref().map(|i| i.pid);

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
    let workspace_id = resolve_workspace_id_or_cwd(name_or_id)?;

    if !workspace_exists(&workspace_id)? {
        return Err(miette!(
            "Workspace '{}' does not exist. Use 'beamtalk workspace list' to see available workspaces.",
            workspace_id
        ));
    }

    let metadata = get_workspace_metadata(&workspace_id)?;

    let (status, node) = probe_node_status(&workspace_id);
    let node_name = node.as_ref().map(|i| i.node_name.clone());
    let port = node.as_ref().map(|i| i.port);
    let pid = node.as_ref().map(|i| i.pid);

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

        let Some(ws_id) = workspace_id_from_path(&path) else {
            continue;
        };

        let Ok(metadata) = get_workspace_metadata(&ws_id) else {
            continue;
        };

        // Defense in depth (BT-3332): a stored `project_path` that isn't already
        // absolute can only come from metadata written before workspace creation
        // started canonicalizing it. Canonicalizing it *here* would resolve it
        // against this process's own cwd rather than wherever the workspace was
        // actually created — turning a relative entry like "." into a wildcard
        // that matches every caller. Skip such entries instead of risking a
        // false match.
        if !metadata.project_path.is_absolute() {
            continue;
        }

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
    validate_workspace_name(candidate)?;
    Ok(candidate.to_string())
}

/// Resolve a workspace ID from an optional name/ID, falling back to the current directory.
///
/// If `name_or_id` is `Some`, delegates to [`resolve_workspace_id`].
/// If `None`, discovers the project root from `cwd` and looks up (or generates)
/// the workspace ID for that path.
pub(crate) fn resolve_workspace_id_or_cwd(name_or_id: Option<&str>) -> Result<String> {
    if let Some(name) = name_or_id {
        resolve_workspace_id(name)
    } else {
        let cwd = std::env::current_dir().into_diagnostic()?;
        let project_root = discovery::discover_project_root(&cwd);
        Ok(find_workspace_by_project_path(&project_root)?
            .unwrap_or(beamtalk_workspace::generate_workspace_id(&project_root)?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Regression test for BT-3332.
    ///
    /// Before the fix, `beamtalk run .` wrote workspace metadata with a bare
    /// relative `project_path` of `"."` (see `create_workspace_impl`, which now
    /// canonicalizes before writing). `find_workspace_by_project_path` then
    /// canonicalized that stored `"."` too, but `Path::canonicalize` resolves a
    /// relative path against *the calling process's own cwd* — and
    /// `resolve_workspace_id_or_cwd` always queries with a path derived from
    /// that same cwd. So a stored `"."` acted as a wildcard: it matched
    /// whichever directory the querying process happened to be running from,
    /// regardless of where the workspace was actually created.
    ///
    /// This test reproduces the buggy metadata shape directly via
    /// `save_workspace_metadata` (bypassing `create_workspace_impl`, which no
    /// longer produces it) for two separate workspaces, then confirms
    /// `find_workspace_by_project_path` — queried with this process's own cwd,
    /// exactly as `resolve_workspace_id_or_cwd` does — matches neither.
    /// Removes a workspace's on-disk directory on drop, including when the
    /// test panics (e.g. the very assertion this test guards against) — a
    /// bare cleanup call after the assertion would otherwise never run and
    /// leak `~/.beamtalk/workspaces/*` fixture directories.
    struct CleanupWorkspaceDirs<'a>(&'a [String]);

    impl Drop for CleanupWorkspaceDirs<'_> {
        fn drop(&mut self) {
            for ws_id in self.0 {
                if let Ok(dir) = workspace_dir(ws_id) {
                    let _ = fs::remove_dir_all(dir);
                }
            }
        }
    }

    #[test]
    fn test_find_workspace_by_project_path_rejects_relative_stored_path() {
        let pid = std::process::id();
        let ws_id_a = format!("test_bt3332_relpath_a_{pid}");
        let ws_id_b = format!("test_bt3332_relpath_b_{pid}");
        let _cleanup = CleanupWorkspaceDirs(&[ws_id_a.clone(), ws_id_b.clone()]);

        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();

        for (ws_id, relative_path) in [(&ws_id_a, "."), (&ws_id_b, "./.")] {
            save_workspace_metadata(&WorkspaceMetadata {
                workspace_id: ws_id.clone(),
                project_path: PathBuf::from(relative_path),
                created_at: now,
            })
            .unwrap();
        }

        // The exact query `resolve_workspace_id_or_cwd(None)` performs: this
        // process's own cwd. Under the pre-fix code, both stored "." entries
        // would canonicalize to this same directory and "match" it.
        let cwd = std::env::current_dir().unwrap();

        let result = find_workspace_by_project_path(&cwd).unwrap();
        assert!(
            result.is_none(),
            "a relative stored project_path must never wildcard-match the \
             querying process's own cwd, got {result:?}"
        );
    }

    /// Regression test for BT-3332 review feedback: `create_workspace_impl`'s
    /// fallback to the raw `project_path` when `canonicalize()` fails (e.g. a
    /// named workspace created against a directory that doesn't exist yet)
    /// must not reintroduce the wildcard-match bug this PR fixes.
    #[test]
    fn test_create_workspace_impl_falls_back_to_raw_path_when_canonicalize_fails() {
        let pid = std::process::id();
        let ws_id = format!("test_bt3332_fallback_{pid}");
        let _cleanup = CleanupWorkspaceDirs(std::slice::from_ref(&ws_id));

        // Relative and guaranteed not to exist anywhere on disk, so
        // `canonicalize()` fails inside `create_workspace_impl` and it falls
        // back to storing this raw path.
        let nonexistent_relative = PathBuf::from("bt3332-nonexistent-dir-should-never-exist");

        let metadata = create_workspace_impl(&ws_id, &nonexistent_relative).unwrap();
        assert_eq!(
            metadata.project_path, nonexistent_relative,
            "canonicalize() failure should fall back to storing the raw path unchanged"
        );
        assert!(
            !metadata.project_path.is_absolute(),
            "this case's fallback path is relative — exactly what \
             find_workspace_by_project_path's is_absolute() guard must reject"
        );

        // Query with the identical relative value: even a byte-for-byte match
        // must be rejected, because find_workspace_by_project_path skips any
        // non-absolute stored project_path before it ever reaches path
        // comparison (BT-3332's defense-in-depth check).
        let result = find_workspace_by_project_path(&nonexistent_relative).unwrap();
        assert_ne!(
            result,
            Some(ws_id.clone()),
            "a non-absolute stored project_path must never match, even a \
             byte-identical query"
        );
    }

    /// Removes a directory on drop, including when the test panics — used by
    /// the self-heal test below to clean up the real directory it creates on
    /// disk (separate from `CleanupWorkspaceDirs`, which only removes
    /// `~/.beamtalk/workspaces/*` entries).
    struct CleanupDir(PathBuf);

    impl Drop for CleanupDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    /// Regression test for BT-3354 case 2: a named workspace created before its
    /// project directory exists stores a relative `project_path` (see
    /// `test_create_workspace_impl_falls_back_to_raw_path_when_canonicalize_fails`
    /// above) that, pre-fix, would never be corrected once the directory later
    /// appeared. `create_workspace_impl`'s early-return path (the one every
    /// subsequent by-name lookup takes) now re-attempts `canonicalize()` and
    /// heals the stored metadata in place when it succeeds.
    #[test]
    fn test_create_workspace_impl_self_heals_relative_project_path_once_directory_exists() {
        let pid = std::process::id();
        let ws_id = format!("test_bt3354_selfheal_{pid}");
        let _cleanup = CleanupWorkspaceDirs(std::slice::from_ref(&ws_id));

        let relative_dir = PathBuf::from(format!("bt3354-selfheal-dir-{pid}"));
        let _dir_cleanup = CleanupDir(relative_dir.clone());

        // First lookup: the directory doesn't exist yet, so
        // `create_workspace_impl`'s own `canonicalize()` fails and it falls back
        // to storing the raw relative path — reproducing a named workspace
        // created ahead of its project directory.
        let metadata = create_workspace_impl(&ws_id, &relative_dir).unwrap();
        assert_eq!(metadata.project_path, relative_dir);
        assert!(!metadata.project_path.is_absolute());

        // The directory now appears (e.g. the project is scaffolded afterwards).
        fs::create_dir_all(&relative_dir).unwrap();

        // The next successful by-name lookup should self-heal the stored path
        // rather than carrying the stale relative path forever.
        let healed = create_workspace_impl(&ws_id, &relative_dir).unwrap();
        assert!(
            healed.project_path.is_absolute(),
            "project_path should be canonicalized once the directory exists, got {:?}",
            healed.project_path
        );
        assert_eq!(healed.project_path, relative_dir.canonicalize().unwrap());

        // And the healed path is persisted on disk, not just returned in memory.
        let reloaded = get_workspace_metadata(&ws_id).unwrap();
        assert_eq!(reloaded.project_path, healed.project_path);
    }

    /// BT-3354 case 1 ("orphaned node on upgrade") analysis: not reproducible
    /// in the current code, and this test is the evidence backing that
    /// decision (recorded on the issue) rather than a remediation.
    ///
    /// An unnamed workspace's id is always `generate_workspace_id` — a SHA256
    /// hash of the *canonicalized project directory* — computed fresh by every
    /// caller (every direct `create_workspace_impl` caller such as `run .`,
    /// and `resolve_workspace_id_or_cwd`'s fallback) directly from the
    /// directory in question. It is never read back from the possibly-stale
    /// `metadata.project_path` field the BT-3332 fix hardened, and that was
    /// already true before BT-3332 too. So a legacy workspace whose metadata
    /// still carries a bare relative `project_path` keeps the *same*
    /// `workspace_id` it always had, and a later caller re-derives that
    /// identical id straight from the directory — reusing the existing
    /// workspace (and any running node it has) rather than minting a new,
    /// orphaned one.
    #[test]
    fn test_legacy_relative_metadata_does_not_orphan_workspace_id() {
        // The id a (pre- or post-fix) `generate_workspace_id` call has always
        // produced for this test's own crate directory — standing in for
        // "the project directory", since it's real, absolute, and stable.
        let project_dir = std::env::current_dir().unwrap();
        let ws_id = beamtalk_workspace::generate_workspace_id(&project_dir).unwrap();
        let _cleanup = CleanupWorkspaceDirs(std::slice::from_ref(&ws_id));

        // Seed metadata exactly as a pre-BT-3332 binary would have: correct id,
        // but a bare relative `project_path`.
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        save_workspace_metadata(&WorkspaceMetadata {
            workspace_id: ws_id.clone(),
            project_path: PathBuf::from("."),
            created_at: now,
        })
        .unwrap();

        // find_workspace_by_project_path correctly refuses to match via the
        // legacy relative metadata (BT-3332 defense in depth)...
        assert_eq!(find_workspace_by_project_path(&project_dir).unwrap(), None);

        // ...but replaying exactly what `resolve_workspace_id_or_cwd`'s
        // fallback does — recompute the id straight from the directory — lands
        // on the very same id as the legacy workspace already on disk, not a
        // new one, so no orphan is created.
        let resolved = find_workspace_by_project_path(&project_dir)
            .unwrap()
            .unwrap_or(beamtalk_workspace::generate_workspace_id(&project_dir).unwrap());
        assert_eq!(
            resolved, ws_id,
            "the resolved id must match the legacy workspace's own id for the \
             same directory, or a real running node under that id would be \
             orphaned"
        );
    }
}
