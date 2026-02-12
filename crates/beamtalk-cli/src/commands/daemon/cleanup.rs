// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Session cleanup and management for compiler daemons.
//!
//! This module provides functions to:
//! - List all session directories
//! - Detect orphaned sessions (dead shell process or daemon)
//! - Clean up stale lockfiles and sockets
//! - Age-based cleanup for abandoned sessions

use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use miette::{IntoDiagnostic, Result, miette};

/// Information about a daemon session.
#[derive(Debug, Clone)]
pub struct SessionInfo {
    /// Session name (e.g., "shell-1234" or "my-test")
    pub name: String,
    /// Full path to session directory
    pub path: PathBuf,
    /// Whether the daemon is currently running
    pub is_alive: bool,
    /// Daemon PID if running
    pub pid: Option<u32>,
    /// Age of the session in days
    pub age_days: u64,
}

/// List all daemon sessions.
///
/// Returns information about all session directories found under
/// `~/.beamtalk/sessions/`, including their status and age.
///
/// # Errors
///
/// Returns an error if the sessions directory cannot be read.
pub fn list_sessions() -> Result<Vec<SessionInfo>> {
    let home = dirs::home_dir().ok_or_else(|| miette!("Could not determine home directory"))?;
    let sessions_dir = home.join(".beamtalk").join("sessions");

    if !sessions_dir.exists() {
        return Ok(Vec::new());
    }

    let entries = fs::read_dir(&sessions_dir).into_diagnostic()?;
    let mut sessions = Vec::new();

    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();

        if !path.is_dir() {
            continue;
        }

        let name = entry.file_name().to_string_lossy().to_string();

        // Check if daemon is running for this session
        let lockfile = path.join("daemon.lock");
        let (is_alive, pid) = if lockfile.exists() {
            match fs::read_to_string(&lockfile) {
                Ok(pid_str) => {
                    if let Ok(pid) = pid_str.trim().parse::<u32>() {
                        let alive = is_process_alive(pid);
                        (alive, if alive { Some(pid) } else { None })
                    } else {
                        (false, None)
                    }
                }
                Err(_) => (false, None),
            }
        } else {
            (false, None)
        };

        // Calculate age
        let age_days = get_session_age_days(&path)?;

        sessions.push(SessionInfo {
            name,
            path,
            is_alive,
            pid,
            age_days,
        });
    }

    // Sort by name for consistent output
    sessions.sort_by(|a, b| a.name.cmp(&b.name));

    Ok(sessions)
}

/// Check if a process with the given PID is alive.
#[cfg(unix)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "PID values are always positive and fit in i32"
)]
fn is_process_alive(pid: u32) -> bool {
    // PID 0 has special "process group" semantics for kill(2); treat as invalid.
    if pid == 0 {
        return false;
    }

    // SAFETY: kill with signal 0 only checks if the process exists or if we
    // have permission to signal it; it does not actually send a signal.
    unsafe {
        let res = libc::kill(pid as i32, 0);
        if res == 0 {
            return true;
        }

        // EPERM: process exists but we lack permission -> treat as alive
        // ESRCH: no such process -> treat as dead
        let errno = *libc::__errno_location();
        errno == libc::EPERM
    }
}

/// Check if a process with the given PID is alive (Windows stub).
#[cfg(not(unix))]
fn is_process_alive(_pid: u32) -> bool {
    // Windows: assume alive (cannot check easily)
    true
}

/// Get the age of a session in days based on directory modification time.
fn get_session_age_days(path: &Path) -> Result<u64> {
    let metadata = fs::metadata(path).into_diagnostic()?;
    let modified = metadata.modified().into_diagnostic()?;
    let now = SystemTime::now();

    match now.duration_since(modified) {
        Ok(duration) => Ok(duration.as_secs() / 86400), // Convert seconds to days
        Err(_) => Ok(0),                                // Future timestamp, treat as 0 days old
    }
}

/// Check if a session should be cleaned up.
///
/// A session is considered orphaned if:
/// - For shell sessions (shell-{ppid}): the shell process is dead OR daemon is dead
/// - For named sessions: only if the daemon is dead
pub fn is_session_orphaned(session: &SessionInfo) -> bool {
    // Named sessions: only cleanup if daemon is dead
    if !session.name.starts_with("shell-") {
        return !session.is_alive;
    }

    // Shell sessions: cleanup if shell OR daemon is dead
    if !session.is_alive {
        return true; // Daemon is dead
    }

    // Check if shell process is still alive
    if let Some(ppid) = parse_shell_ppid(&session.name) {
        !is_process_alive(ppid)
    } else {
        false // Cannot parse PPID, don't clean up
    }
}

/// Parse the PPID from a shell session name.
///
/// # Example
///
/// ```text
/// parse_shell_ppid("shell-1234") => Some(1234)
/// parse_shell_ppid("my-test")    => None
/// ```
fn parse_shell_ppid(session_name: &str) -> Option<u32> {
    session_name
        .strip_prefix("shell-")
        .and_then(|s| s.parse().ok())
}

/// Check if a session is older than the given number of days.
pub fn is_session_old(session: &SessionInfo, max_age_days: u64) -> bool {
    session.age_days > max_age_days
}

/// Clean up a session by removing its directory and contents.
///
/// # Errors
///
/// Returns an error if the directory cannot be removed.
pub fn cleanup_session(session: &SessionInfo) -> Result<()> {
    fs::remove_dir_all(&session.path).into_diagnostic()?;
    Ok(())
}

/// Clean up orphaned sessions.
///
/// Returns the number of sessions cleaned up.
///
/// # Errors
///
/// Returns an error if sessions cannot be listed or cleaned.
pub fn cleanup_orphaned_sessions() -> Result<usize> {
    let sessions = list_sessions()?;
    let mut cleaned = 0;

    for session in sessions {
        if is_session_orphaned(&session) {
            cleanup_session(&session)?;
            cleaned += 1;
        }
    }

    Ok(cleaned)
}

/// Clean up sessions older than the given number of days.
///
/// Returns the number of sessions cleaned up.
///
/// # Errors
///
/// Returns an error if sessions cannot be listed or cleaned.
pub fn cleanup_old_sessions(max_age_days: u64) -> Result<usize> {
    let sessions = list_sessions()?;
    let mut cleaned = 0;

    for session in sessions {
        if is_session_old(&session, max_age_days) {
            cleanup_session(&session)?;
            cleaned += 1;
        }
    }

    Ok(cleaned)
}

/// Stop a daemon by sending SIGTERM to its process.
///
/// # Errors
///
/// Returns an error if the signal cannot be sent.
#[cfg(unix)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "PID values are always positive and fit in i32"
)]
pub fn stop_daemon_by_pid(pid: u32) -> Result<()> {
    if pid == 0 {
        return Err(miette!("Invalid PID 0: refusing to signal process group"));
    }
    // SAFETY: kill is safe to call with any valid pid and signal number
    unsafe {
        if libc::kill(pid as i32, libc::SIGTERM) != 0 {
            return Err(miette!("Failed to send SIGTERM to process {pid}"));
        }
    }
    Ok(())
}

/// Stop a daemon by sending SIGTERM to its process (Windows stub).
#[cfg(not(unix))]
pub fn stop_daemon_by_pid(_pid: u32) -> Result<()> {
    Err(miette!(
        "Stopping daemons is not supported on this platform"
    ))
}

/// Stop all daemons and clean up all sessions.
///
/// Returns the number of sessions cleaned up.
///
/// # Errors
///
/// Returns an error if sessions cannot be listed or cleaned.
pub fn cleanup_all_sessions() -> Result<usize> {
    let sessions = list_sessions()?;
    let mut cleaned = 0;

    for session in sessions {
        // Try to stop the daemon if it's running
        if let Some(pid) = session.pid {
            let _ = stop_daemon_by_pid(pid); // Ignore errors
            // Wait briefly for daemon to exit
            std::thread::sleep(std::time::Duration::from_millis(500));

            // Verify the process is actually dead before removing files
            if is_process_alive(pid) {
                eprintln!(
                    "Warning: daemon in session '{}' (PID {}) still alive after SIGTERM, skipping",
                    session.name, pid
                );
                continue;
            }
        }

        // Clean up the session directory
        cleanup_session(&session)?;
        cleaned += 1;
    }

    Ok(cleaned)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn parse_shell_ppid_extracts_pid() {
        assert_eq!(parse_shell_ppid("shell-1234"), Some(1234));
        assert_eq!(parse_shell_ppid("shell-42"), Some(42));
    }

    #[test]
    fn parse_shell_ppid_returns_none_for_named_sessions() {
        assert_eq!(parse_shell_ppid("my-test"), None);
        assert_eq!(parse_shell_ppid("integration-test"), None);
    }

    #[test]
    fn parse_shell_ppid_returns_none_for_invalid_format() {
        assert_eq!(parse_shell_ppid("shell-"), None);
        assert_eq!(parse_shell_ppid("shell-abc"), None);
        assert_eq!(parse_shell_ppid(""), None);
    }

    #[test]
    fn is_session_old_checks_age_correctly() {
        let session = SessionInfo {
            name: "test".to_string(),
            path: PathBuf::from("/tmp/test"),
            is_alive: false,
            pid: None,
            age_days: 10,
        };

        assert!(is_session_old(&session, 7));
        assert!(!is_session_old(&session, 10));
        assert!(!is_session_old(&session, 15));
    }

    #[test]
    fn is_session_orphaned_detects_dead_daemon_for_named_sessions() {
        let session = SessionInfo {
            name: "my-test".to_string(),
            path: PathBuf::from("/tmp/test"),
            is_alive: false,
            pid: None,
            age_days: 0,
        };

        assert!(is_session_orphaned(&session));
    }

    #[test]
    fn is_session_orphaned_keeps_alive_named_sessions() {
        let session = SessionInfo {
            name: "my-test".to_string(),
            path: PathBuf::from("/tmp/test"),
            is_alive: true,
            pid: Some(12345),
            age_days: 0,
        };

        assert!(!is_session_orphaned(&session));
    }

    #[test]
    fn is_session_orphaned_detects_dead_daemon_for_shell_sessions() {
        let session = SessionInfo {
            name: "shell-1234".to_string(),
            path: PathBuf::from("/tmp/test"),
            is_alive: false,
            pid: None,
            age_days: 0,
        };

        assert!(is_session_orphaned(&session));
    }

    #[test]
    fn list_sessions_returns_empty_for_nonexistent_dir() {
        // This test doesn't create a sessions directory, so it should return empty
        let sessions = list_sessions();
        // Should succeed even if directory doesn't exist
        assert!(sessions.is_ok());
    }

    #[test]
    fn cleanup_session_removes_directory() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let session_dir = temp_dir.path().join("test-session");
        fs::create_dir(&session_dir).expect("Failed to create session dir");

        let session = SessionInfo {
            name: "test-session".to_string(),
            path: session_dir.clone(),
            is_alive: false,
            pid: None,
            age_days: 0,
        };

        cleanup_session(&session).expect("Failed to cleanup session");
        assert!(!session_dir.exists());
    }
}
