// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Create/stop workspaces via the installed `beamtalk` CLI (ADR 0097 Broker
//! §5, ADR constraint 1).
//!
//! The desktop broker **never bundles or supervises** the Rust toolchain —
//! workspace lifecycle happens by shelling out to whatever `beamtalk` binary
//! the user already has installed (`workspace create … --background
//! --persistent` daemonizes it; no supervision follows). This module also
//! resolves *where* that binary is: GUI apps launched from a dock/Finder do
//! not inherit the user's shell `PATH` (ADR 0097 Broker §1a), so `PATH`
//! alone is not a reliable resolution strategy — an explicit override env
//! var and a short list of common install locations back it up.
//!
//! **DDD Context:** Desktop Shell

use std::path::{Path, PathBuf};
use std::process::Command;

use crate::error::{BrokerError, Result};

/// Env var that overrides CLI path resolution entirely, for configurations
/// where neither `PATH` nor the standard install locations apply (ADR 0097
/// Broker §1a: "configurable, with sane defaults").
pub const CLI_PATH_OVERRIDE_ENV: &str = "BEAMTALK_CLI_PATH";

/// The CLI's executable name, including the platform's extension.
#[must_use]
pub fn exe_name() -> &'static str {
    if cfg!(windows) {
        "beamtalk.exe"
    } else {
        "beamtalk"
    }
}

/// Expand a `PATH`-style environment variable string into candidate full
/// paths to `exe_name` — one per directory, in order. Pure (no filesystem
/// access) so it's testable with a synthetic `PATH` string.
#[must_use]
pub fn search_path_var(path_var: &str, exe_name: &str) -> Vec<PathBuf> {
    if path_var.is_empty() {
        // `std::env::split_paths("")` yields a single empty-string component
        // (there is one substring between zero separators), which would
        // otherwise produce a bogus `./<exe_name>`-relative candidate.
        return Vec::new();
    }
    std::env::split_paths(path_var)
        .map(|dir| dir.join(exe_name))
        .collect()
}

/// Standard install locations to check after `PATH`, given a home directory
/// (injectable for tests; [`resolve_cli_path`] passes the real one). Order
/// matters: more specific / more likely locations first.
#[must_use]
pub fn candidate_paths(home: Option<&Path>) -> Vec<PathBuf> {
    let name = exe_name();
    let mut candidates = Vec::new();
    if let Some(home) = home {
        // `cargo install`/`rustup`-managed toolchains.
        candidates.push(home.join(".cargo").join("bin").join(name));
        // A user-local install some packaging paths might use.
        candidates.push(home.join(".local").join("bin").join(name));
    }
    // Common system-wide Unix install locations.
    candidates.push(PathBuf::from("/usr/local/bin").join(name));
    // Homebrew on Apple Silicon.
    candidates.push(PathBuf::from("/opt/homebrew/bin").join(name));
    candidates
}

/// Resolve the installed `beamtalk` CLI's path.
///
/// Resolution order: [`CLI_PATH_OVERRIDE_ENV`] (if set, used as-is — an
/// explicit override is trusted without an existence check, so a
/// misconfigured override fails loudly at the point of use rather than
/// silently falling through) → `PATH` → [`candidate_paths`]. Returns
/// [`BrokerError::CliNotFound`] if nothing is found.
///
/// # Errors
///
/// Returns [`BrokerError::CliNotFound`] if the CLI can't be located by any
/// of the resolution steps above.
pub fn resolve_cli_path() -> Result<PathBuf> {
    if let Ok(override_path) = std::env::var(CLI_PATH_OVERRIDE_ENV) {
        if !override_path.is_empty() {
            return Ok(PathBuf::from(override_path));
        }
    }

    if let Ok(path_var) = std::env::var("PATH") {
        for candidate in search_path_var(&path_var, exe_name()) {
            if candidate.is_file() {
                return Ok(candidate);
            }
        }
    }

    for candidate in candidate_paths(dirs::home_dir().as_deref()) {
        if candidate.is_file() {
            return Ok(candidate);
        }
    }

    Err(BrokerError::CliNotFound)
}

/// Args for `beamtalk workspace create <id> --background --persistent`
/// (ADR 0097 Broker §5) — daemonizes the workspace; no broker supervision
/// follows.
#[must_use]
pub fn create_workspace_args(workspace_id: &str) -> Vec<String> {
    vec![
        "workspace".to_string(),
        "create".to_string(),
        workspace_id.to_string(),
        "--background".to_string(),
        "--persistent".to_string(),
    ]
}

/// Args for `beamtalk workspace stop <id>`.
#[must_use]
pub fn stop_workspace_args(workspace_id: &str) -> Vec<String> {
    vec![
        "workspace".to_string(),
        "stop".to_string(),
        workspace_id.to_string(),
    ]
}

/// Run the installed CLI with `args`, waiting for it to exit.
///
/// Returns `Ok(())` on a zero exit status, or [`BrokerError::CliFailed`]
/// with stderr (falling back to stdout if stderr is empty) on a non-zero
/// exit — `workspace create --background` and `workspace stop` are both
/// short-lived, synchronous invocations (the daemonization happens inside
/// the CLI process before it exits), so waiting for completion is correct
/// here, unlike `spawn_front` in [`crate::spawn`], which intentionally does
/// not wait.
fn run_cli(cli_path: &Path, args: &[String]) -> Result<()> {
    let output = Command::new(cli_path).args(args).output()?;
    if output.status.success() {
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    let message = if stderr.trim().is_empty() {
        String::from_utf8_lossy(&output.stdout).into_owned()
    } else {
        stderr.into_owned()
    };
    Err(BrokerError::CliFailed(
        args.join(" "),
        output.status.code().unwrap_or(-1),
        message,
    ))
}

/// `beamtalk workspace create <id> --background --persistent`.
///
/// # Errors
///
/// Returns [`BrokerError::CliFailed`] if the CLI exits non-zero, or
/// [`BrokerError::Io`] if it can't be spawned at all.
pub fn create_workspace(cli_path: &Path, workspace_id: &str) -> Result<()> {
    run_cli(cli_path, &create_workspace_args(workspace_id))
}

/// `beamtalk workspace stop <id>`.
///
/// # Errors
///
/// Returns [`BrokerError::CliFailed`] if the CLI exits non-zero, or
/// [`BrokerError::Io`] if it can't be spawned at all.
pub fn stop_workspace(cli_path: &Path, workspace_id: &str) -> Result<()> {
    run_cli(cli_path, &stop_workspace_args(workspace_id))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::ENV_LOCK;

    #[test]
    fn create_workspace_args_shape() {
        assert_eq!(
            create_workspace_args("abc123"),
            vec![
                "workspace",
                "create",
                "abc123",
                "--background",
                "--persistent"
            ]
        );
    }

    #[test]
    fn stop_workspace_args_shape() {
        assert_eq!(
            stop_workspace_args("abc123"),
            vec!["workspace", "stop", "abc123"]
        );
    }

    #[test]
    fn search_path_var_expands_each_directory() {
        let sep = if cfg!(windows) { ";" } else { ":" };
        let path_var = format!("/usr/bin{sep}/usr/local/bin{sep}/opt/bin");
        let candidates = search_path_var(&path_var, "beamtalk");
        assert_eq!(
            candidates,
            vec![
                PathBuf::from("/usr/bin/beamtalk"),
                PathBuf::from("/usr/local/bin/beamtalk"),
                PathBuf::from("/opt/bin/beamtalk"),
            ]
        );
    }

    #[test]
    fn search_path_var_handles_empty_path() {
        assert_eq!(search_path_var("", "beamtalk"), Vec::<PathBuf>::new());
    }

    #[test]
    fn candidate_paths_includes_cargo_bin_under_home() {
        let home = PathBuf::from("/home/testuser");
        let candidates = candidate_paths(Some(&home));
        assert!(candidates.contains(&home.join(".cargo").join("bin").join(exe_name())));
    }

    #[test]
    fn candidate_paths_includes_system_locations_without_home() {
        let candidates = candidate_paths(None);
        assert!(candidates.contains(&PathBuf::from("/usr/local/bin").join(exe_name())));
        assert!(candidates.contains(&PathBuf::from("/opt/homebrew/bin").join(exe_name())));
    }

    #[test]
    fn exe_name_matches_platform() {
        if cfg!(windows) {
            assert_eq!(exe_name(), "beamtalk.exe");
        } else {
            assert_eq!(exe_name(), "beamtalk");
        }
    }

    #[test]
    fn resolve_cli_path_prefers_the_override_env_var() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by ENV_LOCK.
        unsafe { std::env::set_var(CLI_PATH_OVERRIDE_ENV, "/custom/path/to/beamtalk") };
        let result = resolve_cli_path();
        // SAFETY: guarded by ENV_LOCK.
        unsafe { std::env::remove_var(CLI_PATH_OVERRIDE_ENV) };

        assert_eq!(result.unwrap(), PathBuf::from("/custom/path/to/beamtalk"));
    }

    #[test]
    fn resolve_cli_path_finds_an_executable_on_path() {
        let _guard = ENV_LOCK.lock().unwrap();
        let tmp = tempfile::TempDir::new().unwrap();
        let fake_cli = tmp.path().join(exe_name());
        std::fs::write(&fake_cli, b"#!/bin/sh\n").unwrap();

        // SAFETY: guarded by ENV_LOCK.
        unsafe {
            std::env::remove_var(CLI_PATH_OVERRIDE_ENV);
            std::env::set_var("PATH", tmp.path());
        }
        let result = resolve_cli_path();
        // SAFETY: guarded by ENV_LOCK; restore PATH so later tests in this
        // binary (e.g. spawning real processes) aren't left with a bogus PATH.
        unsafe {
            std::env::remove_var("PATH");
        }

        assert_eq!(result.unwrap(), fake_cli);
    }

    #[test]
    fn resolve_cli_path_errors_when_nothing_found() {
        let _guard = ENV_LOCK.lock().unwrap();
        let tmp = tempfile::TempDir::new().unwrap(); // empty directory
        // SAFETY: guarded by ENV_LOCK.
        unsafe {
            std::env::remove_var(CLI_PATH_OVERRIDE_ENV);
            std::env::set_var("PATH", tmp.path());
        }
        let result = resolve_cli_path();
        // SAFETY: guarded by ENV_LOCK.
        unsafe {
            std::env::remove_var("PATH");
        }

        assert!(matches!(result, Err(BrokerError::CliNotFound)));
    }

    #[test]
    fn run_cli_reports_failure_with_stderr() {
        // `false` (POSIX) always exits 1 with no output — exercise the
        // non-zero-exit path without depending on the real beamtalk binary.
        // /usr/bin/false, not /bin/false: macOS only ships the former (/bin
        // is a much smaller BSD set there); Linux has both (/bin is usually
        // a symlink into /usr/bin on merged-usr distros).
        if cfg!(unix) {
            let result = run_cli(Path::new("/usr/bin/false"), &[]);
            assert!(matches!(result, Err(BrokerError::CliFailed(_, code, _)) if code != 0));
        }
    }

    #[test]
    fn run_cli_succeeds_on_zero_exit() {
        if cfg!(unix) {
            let result = run_cli(Path::new("/usr/bin/true"), &[]);
            assert!(result.is_ok());
        }
    }
}
