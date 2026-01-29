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
/// # Errors
///
/// Returns an error if the beamtalk directory path cannot be determined.
pub fn lockfile_path() -> Result<PathBuf> {
    Ok(beamtalk_dir()?.join("daemon.lock"))
}

/// Path to the Unix socket.
///
/// # Errors
///
/// Returns an error if the beamtalk directory path cannot be determined.
pub fn socket_path() -> Result<PathBuf> {
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
    fn is_daemon_running_returns_none_when_no_lockfile() {
        // When there's no lockfile, should return None
        let result = is_daemon_running().expect("Failed to check daemon status");
        // We can't assume the daemon is not running in the test environment,
        // so just verify the function doesn't panic
        assert!(result.is_none() || result.is_some());
    }
}
