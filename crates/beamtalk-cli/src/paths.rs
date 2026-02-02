// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared path utilities for the Beamtalk CLI.
//!
//! This module provides common functions for locating Beamtalk state directories,
//! lockfiles, and checking daemon status.

use std::fs;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, miette};

/// Directory for beamtalk state files.
///
/// Returns the `~/.beamtalk` directory where the CLI stores configuration,
/// history files, and runtime state.
///
/// # Errors
///
/// Returns an error if the home directory cannot be determined.
pub fn beamtalk_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| miette!("Could not determine home directory"))?;
    Ok(home.join(".beamtalk"))
}

/// Path to the lockfile containing the daemon PID.
///
/// Derives the lockfile path from the socket path by replacing `.sock` with `.lock`.
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

/// Path to the Unix socket.
///
/// Priority order:
/// 1. `BEAMTALK_DAEMON_SOCKET` environment variable (if set and non-empty)
/// 2. Default: `~/.beamtalk/daemon.sock`
///
/// This allows per-worktree daemon isolation when working on multiple
/// compiler changes in parallel.
///
/// # Errors
///
/// Returns an error if the beamtalk directory path cannot be determined.
pub fn socket_path() -> Result<PathBuf> {
    if let Ok(socket_path) = std::env::var("BEAMTALK_DAEMON_SOCKET") {
        if !socket_path.is_empty() {
            return Ok(PathBuf::from(socket_path));
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

    #[test]
    fn beamtalk_dir_returns_home_subdirectory() {
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        assert!(dir.ends_with(".beamtalk"));
    }

    #[test]
    fn lockfile_path_is_in_beamtalk_dir() {
        let lockfile = lockfile_path().expect("Failed to get lockfile_path");
        assert!(lockfile.ends_with(".beamtalk/daemon.lock"));
    }

    #[test]
    fn socket_path_is_in_beamtalk_dir() {
        let socket = socket_path().expect("Failed to get socket_path");
        assert!(socket.ends_with(".beamtalk/daemon.sock"));
    }

    #[test]
    fn socket_path_respects_env_var() {
        // SAFETY: This test runs in isolation and only modifies the environment temporarily
        unsafe {
            std::env::set_var("BEAMTALK_DAEMON_SOCKET", "/tmp/test-daemon.sock");
        }
        let socket = socket_path().expect("Failed to get socket_path");
        assert_eq!(socket, PathBuf::from("/tmp/test-daemon.sock"));
        // SAFETY: Cleaning up test state
        unsafe {
            std::env::remove_var("BEAMTALK_DAEMON_SOCKET");
        }
    }

    #[test]
    fn socket_path_ignores_empty_env_var() {
        // SAFETY: This test runs in isolation and only modifies the environment temporarily
        unsafe {
            std::env::set_var("BEAMTALK_DAEMON_SOCKET", "");
        }
        let socket = socket_path().expect("Failed to get socket_path");
        assert!(
            socket.ends_with(".beamtalk/daemon.sock"),
            "Empty env var should use default"
        );
        // SAFETY: Cleaning up test state
        unsafe {
            std::env::remove_var("BEAMTALK_DAEMON_SOCKET");
        }
    }

    #[test]
    fn lockfile_path_derives_from_socket_path() {
        // SAFETY: This test runs in isolation and only modifies the environment temporarily
        unsafe {
            std::env::set_var("BEAMTALK_DAEMON_SOCKET", "/tmp/test-daemon.sock");
        }
        let lockfile = lockfile_path().expect("Failed to get lockfile_path");
        assert_eq!(lockfile, PathBuf::from("/tmp/test-daemon.lock"));
        // SAFETY: Cleaning up test state
        unsafe {
            std::env::remove_var("BEAMTALK_DAEMON_SOCKET");
        }
    }

    #[test]
    fn is_daemon_running_returns_result_without_error() {
        // This test verifies the function doesn't panic and returns a valid Result.
        // The actual value (Some/None) depends on whether a daemon is running,
        // which we cannot control in unit tests.
        let result = is_daemon_running();
        assert!(result.is_ok(), "is_daemon_running should return Ok");
    }
}
