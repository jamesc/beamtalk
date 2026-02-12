// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared path utilities for the Beamtalk CLI.
//!
//! This module provides common functions for locating Beamtalk state directories,
//! lockfiles, and checking daemon status.

use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, miette};

/// Directory for beamtalk state files (session-based).
///
/// Returns a session-specific directory under `~/.beamtalk/sessions/`.
/// Each terminal/session gets its own isolated daemon.
///
/// Priority order:
/// 1. `BEAMTALK_SESSION` environment variable (explicit named session)
/// 2. Auto-session based on shell PPID: `shell-{ppid}`
///
/// # Examples
///
/// ```text
/// ~/.beamtalk/sessions/shell-1234/     # Auto session (terminal PPID 1234)
/// ~/.beamtalk/sessions/my-test/        # Named session (BEAMTALK_SESSION=my-test)
/// ```
///
/// # Errors
///
/// Returns an error if the home directory cannot be determined.
pub fn beamtalk_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| miette!("Could not determine home directory"))?;
    let base = home.join(".beamtalk");

    // 1. Explicit named session (highest priority)
    if let Ok(session) = std::env::var("BEAMTALK_SESSION") {
        if !session.is_empty() {
            let sanitized = sanitize_session_id(&session);
            return Ok(base.join("sessions").join(sanitized));
        }
    }

    // 2. Auto session based on shell PPID
    let ppid = get_parent_pid();
    Ok(base.join("sessions").join(format!("shell-{ppid}")))
}

/// Get the parent process ID (PPID).
///
/// Returns the PPID of the current process, which typically represents
/// the shell that launched this process.
#[cfg(unix)]
#[expect(
    clippy::cast_sign_loss,
    reason = "PPID is always positive; libc returns signed but semantically unsigned"
)]
fn get_parent_pid() -> u32 {
    // SAFETY: getppid is always safe to call and returns a valid pid_t
    unsafe { libc::getppid() as u32 }
}

/// Get the parent process ID (PPID) (Windows stub).
#[cfg(not(unix))]
fn get_parent_pid() -> u32 {
    // Windows: use process ID as fallback
    std::process::id()
}

/// Sanitize a session ID to be filesystem-safe.
///
/// Replaces any characters that aren't alphanumeric, dash, or underscore
/// with hyphens. Ensures the result is a valid directory name.
fn sanitize_session_id(id: &str) -> String {
    id.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '-' || c == '_' {
                c
            } else {
                '-'
            }
        })
        .collect()
}

/// Path to the lockfile containing the daemon PID.
///
/// Derives the lockfile path from the socket path by setting its extension to `.lock`.
/// This ensures lockfile and socket names stay synchronized when using custom socket paths.
///
/// # Errors
///
/// Returns an error if the beamtalk directory path cannot be determined.
pub fn lockfile_path() -> Result<PathBuf> {
    let socket = socket_path()?;
    let lockfile = socket.with_extension("lock");
    Ok(lockfile)
}

/// Path to the Unix socket for the current session.
///
/// Priority:
/// 1. `BEAMTALK_DAEMON_SOCKET` environment variable (absolute or relative path)
/// 2. Default: `{beamtalk_dir()}/daemon.sock` where `beamtalk_dir()` is
///    session-specific (e.g., `~/.beamtalk/sessions/shell-1234/`).
///
/// # Errors
///
/// Returns an error if the beamtalk directory path cannot be determined.
pub fn socket_path() -> Result<PathBuf> {
    if let Ok(path) = std::env::var("BEAMTALK_DAEMON_SOCKET") {
        if !path.is_empty() {
            return Ok(PathBuf::from(path));
        }
    }
    Ok(beamtalk_dir()?.join("daemon.sock"))
}

/// Check if daemon is currently running.
///
/// Uses POSIX-compliant `kill(pid, 0)` to check process existence,
/// which works on Linux, macOS, and other Unix systems.
///
/// # Returns
///
/// - `Ok(Some(pid))` if the daemon is running with the given PID
/// - `Ok(None)` if the daemon is not running
/// - `Err(_)` if the lockfile exists but cannot be read or parsed
///
/// # Stale Lockfiles
///
/// If a lockfile exists but the process is no longer running, this function
/// automatically cleans up the stale lockfile and socket.
#[cfg(unix)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "PID values are always positive and fit in i32"
)]
pub fn is_daemon_running() -> Result<Option<u32>> {
    let lockfile = lockfile_path()?;
    if !lockfile.exists() {
        return Ok(None);
    }

    let pid_str = fs::read_to_string(&lockfile).into_diagnostic()?;
    let pid: u32 = pid_str.trim().parse().into_diagnostic()?;

    // Check if process exists using POSIX kill(pid, 0)
    // This is portable across Linux, macOS, and BSD systems
    // SAFETY: kill with signal 0 only checks if process exists, doesn't send a signal
    let exists = unsafe { libc::kill(pid as i32, 0) == 0 };
    if exists {
        Ok(Some(pid))
    } else {
        // Stale lockfile, clean up
        let _ = fs::remove_file(&lockfile);
        let _ = fs::remove_file(socket_path()?);
        Ok(None)
    }
}

/// Check if daemon is currently running (Windows stub).
///
/// # Note
///
/// Windows support is not yet implemented. This function always returns `Ok(None)`.
#[cfg(not(unix))]
pub fn is_daemon_running() -> Result<Option<u32>> {
    // Windows support not yet implemented
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;

    /// RAII guard to safely manipulate environment variables in tests.
    /// Automatically restores the previous value (or removes if unset) on drop.
    struct EnvVarGuard {
        key: &'static str,
        prev: Option<String>,
    }

    impl EnvVarGuard {
        /// Set an environment variable and save its previous value.
        /// SAFETY: Caller must ensure tests using this are serialized with `#[serial(env_var)]`.
        unsafe fn set(key: &'static str, value: &str) -> Self {
            let prev = std::env::var(key).ok();
            // SAFETY: Caller ensures tests are serialized
            unsafe {
                std::env::set_var(key, value);
            }
            Self { key, prev }
        }

        /// Clear an environment variable and save its previous value.
        /// SAFETY: Caller must ensure tests using this are serialized with `#[serial(env_var)]`.
        unsafe fn clear(key: &'static str) -> Self {
            let prev = std::env::var(key).ok();
            // SAFETY: Caller ensures tests are serialized
            unsafe {
                std::env::remove_var(key);
            }
            Self { key, prev }
        }
    }

    impl Drop for EnvVarGuard {
        fn drop(&mut self) {
            // SAFETY: Test cleanup - restore previous env var state
            unsafe {
                match &self.prev {
                    Some(val) => std::env::set_var(self.key, val),
                    None => std::env::remove_var(self.key),
                }
            }
        }
    }

    #[test]
    #[serial(env_var)]
    fn beamtalk_dir_returns_session_subdirectory() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::clear("BEAMTALK_SESSION") };
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        // Should be ~/.beamtalk/sessions/shell-{ppid}
        assert!(dir.starts_with(dirs::home_dir().unwrap().join(".beamtalk")));
        assert!(dir.parent().unwrap().ends_with("sessions"));
        let dir_name = dir.file_name().unwrap().to_string_lossy();
        assert!(
            dir_name.starts_with("shell-"),
            "Dir name should start with shell-: {dir_name}"
        );
    }

    #[test]
    #[serial(env_var)]
    fn beamtalk_dir_respects_session_env_var() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_SESSION", "my-test-session") };
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        assert!(dir.ends_with(".beamtalk/sessions/my-test-session"));
    }

    #[test]
    #[serial(env_var)]
    fn beamtalk_dir_sanitizes_session_names() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_SESSION", "my/test\\session:name") };
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        // Slashes and colons should be replaced with hyphens
        assert!(dir.ends_with(".beamtalk/sessions/my-test-session-name"));
    }

    #[test]
    #[serial(env_var)]
    fn beamtalk_dir_ignores_empty_session_env() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_SESSION", "") };
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        // Should fall back to PPID-based session
        assert!(dir.parent().unwrap().ends_with("sessions"));
        let dir_name = dir.file_name().unwrap().to_string_lossy();
        assert!(
            dir_name.starts_with("shell-"),
            "Should fall back to shell-: {dir_name}"
        );
    }

    #[test]
    #[serial(env_var)]
    fn lockfile_path_is_in_session_dir() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::clear("BEAMTALK_SESSION") };
        // SAFETY: Same serialization guarantees as above
        let _guard2 = unsafe { EnvVarGuard::clear("BEAMTALK_DAEMON_SOCKET") };
        let lockfile = lockfile_path().expect("Failed to get lockfile_path");
        // Should be in sessions/shell-{ppid}/daemon.lock
        let parent = lockfile.parent().unwrap();
        let parent_name = parent.file_name().unwrap().to_string_lossy();
        assert!(
            parent_name.starts_with("shell-"),
            "Parent should be shell-*: {parent_name}"
        );
        assert!(parent.parent().unwrap().ends_with("sessions"));
        assert!(lockfile.ends_with("daemon.lock"));
    }

    #[test]
    #[serial(env_var)]
    fn socket_path_is_in_session_dir() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::clear("BEAMTALK_SESSION") };
        // SAFETY: Same serialization guarantees as above
        let _guard2 = unsafe { EnvVarGuard::clear("BEAMTALK_DAEMON_SOCKET") };
        let socket = socket_path().expect("Failed to get socket_path");
        // Should be in sessions/shell-{ppid}/daemon.sock
        let parent = socket.parent().unwrap();
        let parent_name = parent.file_name().unwrap().to_string_lossy();
        assert!(
            parent_name.starts_with("shell-"),
            "Parent should be shell-*: {parent_name}"
        );
        assert!(parent.parent().unwrap().ends_with("sessions"));
        assert!(socket.ends_with("daemon.sock"));
    }

    #[test]
    #[serial(env_var)]
    fn named_session_uses_consistent_paths() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_SESSION", "test-session") };
        // SAFETY: Same serialization guarantees as above
        let _guard2 = unsafe { EnvVarGuard::clear("BEAMTALK_DAEMON_SOCKET") };
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        let socket = socket_path().expect("Failed to get socket_path");
        let lockfile = lockfile_path().expect("Failed to get lockfile_path");

        // All paths should use same session directory
        assert!(dir.ends_with("sessions/test-session"));
        assert!(
            socket.starts_with(&dir),
            "Socket should be under session dir"
        );
        assert!(
            lockfile.starts_with(&dir),
            "Lockfile should be under session dir"
        );
    }

    #[test]
    fn is_daemon_running_returns_result_without_error() {
        // This test verifies the function doesn't panic and returns a valid Result.
        // The actual value (Some/None) depends on whether a daemon is running,
        // which we cannot control in unit tests.
        let result = is_daemon_running();
        assert!(result.is_ok(), "is_daemon_running should return Ok");
    }

    #[test]
    #[serial(env_var)]
    fn socket_path_respects_daemon_socket_env_var() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_DAEMON_SOCKET", "/tmp/test-daemon.sock") };
        let socket = socket_path().expect("Failed to get socket_path");
        assert_eq!(socket, PathBuf::from("/tmp/test-daemon.sock"));
    }

    #[test]
    #[serial(env_var)]
    fn socket_path_ignores_empty_daemon_socket_env() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_DAEMON_SOCKET", "") };
        // SAFETY: Same serialization guarantees as above
        let _guard2 = unsafe { EnvVarGuard::clear("BEAMTALK_SESSION") };
        let socket = socket_path().expect("Failed to get socket_path");
        assert!(
            socket.ends_with("daemon.sock"),
            "Empty env var should use default"
        );
    }

    #[test]
    #[serial(env_var)]
    fn lockfile_path_derives_from_daemon_socket_env() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_DAEMON_SOCKET", "/tmp/test-daemon.sock") };
        let lockfile = lockfile_path().expect("Failed to get lockfile_path");
        assert_eq!(lockfile, PathBuf::from("/tmp/test-daemon.lock"));
    }
}
