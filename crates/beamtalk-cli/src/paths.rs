// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared path utilities for the Beamtalk CLI.
//!
//! This module provides common functions for locating Beamtalk state directories.

use std::path::PathBuf;

use miette::{Result, miette};

/// Directory for beamtalk state files (session-based).
///
/// Returns a session-specific directory under `~/.beamtalk/sessions/`.
///
/// Priority order:
/// 1. `BEAMTALK_WORKSPACE` environment variable (explicit named session)
/// 2. Auto-session based on shell PPID: `shell-{ppid}`
///
/// # Examples
///
/// ```text
/// ~/.beamtalk/sessions/shell-1234/     # Auto session (terminal PPID 1234)
/// ~/.beamtalk/sessions/my-test/        # Named session (BEAMTALK_WORKSPACE=my-test)
/// ```
///
/// # Errors
///
/// Returns an error if the home directory cannot be determined.
pub fn beamtalk_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| miette!("Could not determine home directory"))?;
    let base = home.join(".beamtalk");

    // 1. Explicit named session (highest priority)
    if let Ok(session) = std::env::var("BEAMTALK_WORKSPACE") {
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
        let _guard = unsafe { EnvVarGuard::clear("BEAMTALK_WORKSPACE") };
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
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_WORKSPACE", "my-test-session") };
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        assert!(dir.ends_with(".beamtalk/sessions/my-test-session"));
    }

    #[test]
    #[serial(env_var)]
    fn beamtalk_dir_sanitizes_session_names() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_WORKSPACE", "my/test\\session:name") };
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        // Slashes and colons should be replaced with hyphens
        assert!(dir.ends_with(".beamtalk/sessions/my-test-session-name"));
    }

    #[test]
    #[serial(env_var)]
    fn beamtalk_dir_ignores_empty_session_env() {
        // SAFETY: Test is serialized with #[serial(env_var)] and EnvVarGuard restores state
        let _guard = unsafe { EnvVarGuard::set("BEAMTALK_WORKSPACE", "") };
        let dir = beamtalk_dir().expect("Failed to get beamtalk_dir");
        // Should fall back to PPID-based session
        assert!(dir.parent().unwrap().ends_with("sessions"));
        let dir_name = dir.file_name().unwrap().to_string_lossy();
        assert!(
            dir_name.starts_with("shell-"),
            "Should fall back to shell-: {dir_name}"
        );
    }
}
