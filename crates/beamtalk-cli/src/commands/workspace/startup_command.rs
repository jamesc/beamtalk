// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BEAM node command construction for workspace startup.
//!
//! Owns everything about *building* the detached BEAM node command — separate
//! from *running* it. Isolating command construction here makes the security
//! surface (env allowlist, umask, CREATE_NO_WINDOW flags) auditable and
//! testable without spawning a process.
//!
//! **DDD Context:** CLI

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use beamtalk_cli::repl_startup::{BeamPaths, beam_pa_args};

#[cfg(windows)]
use std::os::windows::process::CommandExt;

#[cfg(unix)]
use std::os::unix::process::CommandExt as _;

use miette::{Result, miette};

/// Write an Erlang args file containing the cookie for secure distribution.
///
/// Uses `-args_file` instead of `-setcookie` on the command line to prevent
/// the cookie from being visible in `ps aux` / `/proc/{pid}/cmdline` (BT-726).
/// The file is created with mode 0600 on Unix for owner-only access.
pub(super) fn write_cookie_args_file(workspace_id: &str, cookie: &str) -> Result<PathBuf> {
    let args_file_path = super::storage::workspace_dir(workspace_id)?.join("vm.args");
    let content = format!("-setcookie {cookie}\n");
    super::storage::write_secure_file(&args_file_path, content.as_bytes())
        .map_err(|e| miette!("Failed to write cookie args file: {e}"))?;
    Ok(args_file_path)
}

/// Convert a Windows path to Unix-style forward slashes for Erlang.
///
/// Erlang on Windows expects forward slashes in `-pa` arguments, not backslashes.
/// See BT-661 for details on this Windows-specific issue.
pub(super) fn path_to_erlang_arg(path: &Path) -> String {
    #[cfg(windows)]
    {
        path.to_string_lossy().replace('\\', "/")
    }
    #[cfg(not(windows))]
    {
        path.to_string_lossy().into_owned()
    }
}

/// Build a `Command` for starting a detached BEAM workspace node.
///
/// Extracted from `start_detached_node` so the command configuration
/// (args, env vars) can be inspected in tests without spawning a process.
///
/// # Security hardening (BT-726)
///
/// - **Environment allowlist**: `env_clear()` prevents leaking sensitive env vars
///   (`AWS_*`, `DATABASE_URL`, `SSH_AUTH_SOCK`, etc.) to the BEAM node.
/// - **Cookie via args file**: Uses `-args_file` instead of `-setcookie` so the
///   cookie is not visible in `ps aux` / `/proc/{pid}/cmdline`.
/// - **Umask**: Set to 0077 on Unix so workspace files are owner-only.
/// - **setsid**: Creates a new session on Unix for proper daemon detachment.
#[allow(clippy::too_many_lines)] // BEAM node command construction is necessarily verbose
pub(super) fn build_detached_node_command(
    node_name: &str,
    cookie_args_file: &Path,
    beam_paths: &BeamPaths,
    extra_code_paths: &[PathBuf],
    eval_cmd: &str,
    project_root: &Path,
) -> Command {
    let (node_flag, node_arg) = if let Some((_local, host)) = node_name.split_once('@') {
        if host.contains('.') {
            // Fully qualified hostname (e.g. host.example.com) → long names
            ("-name", node_name.to_string())
        } else {
            // Short hostname (e.g. localhost) → short names to avoid
            // "Hostname localhost is illegal" errors (BT-1418)
            ("-sname", node_name.to_string())
        }
    } else {
        ("-sname", node_name.to_string())
    };

    // Build the initial (non-path) args: detach flags, node name, cookie file.
    let initial_args = vec![
        // On Windows, don't use -detached - it doesn't work reliably from Rust spawn
        // Instead, we'll use CREATE_NO_WINDOW flag to prevent console popup
        #[cfg(not(windows))]
        "-detached".to_string(),
        "-noshell".to_string(),
        node_flag.to_string(),
        node_arg,
        // Cookie via args file instead of -setcookie (BT-726: not visible in ps)
        "-args_file".to_string(),
        path_to_erlang_arg(cookie_args_file),
    ];

    let mut cmd = Command::new("erl");

    // Add initial args, then BEAM code paths via the canonical beam_pa_args helper
    // (same ordering as the foreground REPL startup).
    cmd.args(&initial_args).args(beam_pa_args(beam_paths));

    // Add extra code paths (e.g. package ebin from auto-compile)
    for path in extra_code_paths {
        cmd.arg("-pa").arg(path_to_erlang_arg(path));
    }

    cmd.arg("-eval").arg(eval_cmd);

    // Security: clear inherited environment, then allowlist only required vars (BT-726).
    // Prevents leaking AWS_*, DATABASE_URL, SSH_AUTH_SOCK, etc.
    cmd.env_clear();

    // Erlang needs PATH to find epmd and other tools
    if let Ok(path) = std::env::var("PATH") {
        cmd.env("PATH", path);
    }
    // HOME is needed for Erlang's ~/.erlang.cookie fallback and epmd
    if let Ok(home) = std::env::var("HOME") {
        cmd.env("HOME", home);
    }
    // Locale settings for proper string handling
    for var in &["LANG", "LC_ALL"] {
        if let Ok(val) = std::env::var(var) {
            cmd.env(var, val);
        }
    }
    // Terminal type (for erl console compatibility)
    if let Ok(term) = std::env::var("TERM") {
        cmd.env("TERM", term);
    }
    // Temp directory (used by Erlang's file module and System.getEnv:)
    for var in &["TMPDIR", "TEMP", "TMP"] {
        if let Ok(val) = std::env::var(var) {
            cmd.env(var, val);
        }
    }
    // Windows-specific system variables required for reliable BEAM startup (BT-727).
    // USERPROFILE is the Windows equivalent of HOME; SystemRoot, COMSPEC, PATHEXT,
    // and APPDATA are needed by Erlang and child processes on Windows.
    #[cfg(windows)]
    for var in &["USERPROFILE", "SystemRoot", "COMSPEC", "PATHEXT", "APPDATA"] {
        if let Ok(val) = std::env::var(var) {
            cmd.env(var, val);
        }
    }

    cmd.current_dir(project_root)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());

    // On Windows, set process creation flags to properly detach
    // CREATE_NO_WINDOW (0x0800_0000) prevents console window popup without -detached
    // CREATE_NEW_PROCESS_GROUP (0x0000_0200) allows the process to run independently
    #[cfg(windows)]
    {
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        const CREATE_NEW_PROCESS_GROUP: u32 = 0x0000_0200;
        cmd.creation_flags(CREATE_NO_WINDOW | CREATE_NEW_PROCESS_GROUP);
    }

    // Security: set umask and create new session for proper daemon behavior (BT-726)
    #[cfg(unix)]
    {
        // SAFETY: setsid() and umask() are async-signal-safe per POSIX.
        // They are safe to call in pre_exec (between fork and exec).
        unsafe {
            cmd.pre_exec(|| {
                // Restrictive umask: workspace files are owner-only (0077)
                libc::umask(0o077);
                // New session: fully detach from controlling terminal.
                // Note: -detached already calls setsid() internally; this is
                // harmless (returns EPERM) but we need pre_exec for umask anyway.
                libc::setsid();
                Ok(())
            });
        }
    }

    // Set compiler port binary path (for runtime compilation support).
    // Preserve any user-provided BEAMTALK_COMPILER_PORT_BIN (e.g. via Nix/Homebrew
    // wrapper), falling back to auto-discovery next to the beamtalk binary.
    if let Ok(user_compiler_port) = std::env::var("BEAMTALK_COMPILER_PORT_BIN") {
        cmd.env("BEAMTALK_COMPILER_PORT_BIN", user_compiler_port);
    } else if let Ok(exe) = std::env::current_exe() {
        if let Some(bin_dir) = exe.parent() {
            let compiler_name = if cfg!(windows) {
                "beamtalk-compiler-port.exe"
            } else {
                "beamtalk-compiler-port"
            };
            let compiler_port = bin_dir.join(compiler_name);
            if compiler_port.exists() {
                cmd.env("BEAMTALK_COMPILER_PORT_BIN", &compiler_port);
            }
        }
    }

    cmd
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsStr;

    /// Helper: build a test command and return its env vars and args.
    fn build_test_command() -> Command {
        let tmp = std::env::temp_dir();
        let cookie_file = tmp.join("test_cookie_args");
        let beam_dir = tmp.join("test_beam");
        let paths = beamtalk_cli::repl_startup::BeamPaths {
            runtime_ebin: beam_dir.clone(),
            workspace_ebin: beam_dir.clone(),
            compiler_ebin: beam_dir.clone(),
            cowboy_ebin: beam_dir.clone(),
            cowlib_ebin: beam_dir.clone(),
            ranch_ebin: beam_dir.clone(),
            telemetry_ebin: beam_dir.clone(),
            telemetry_poller_ebin: beam_dir.clone(),
            stdlib_ebin: beam_dir.clone(),
            stdlib_erlang_ebin: beam_dir.clone(),
        };
        build_detached_node_command(
            "test_node@localhost",
            &cookie_file,
            &paths,
            &[],
            "ok.",
            &tmp,
        )
    }

    #[test]
    fn test_build_command_clears_env_and_restores_path() {
        // After env_clear(), PATH should be restored from current process — but
        // only when PATH is set in the test environment (minimal CI may omit it).
        if std::env::var("PATH").is_err() {
            return;
        }
        let cmd = build_test_command();
        let envs: Vec<_> = cmd.get_envs().collect();
        let has_path = envs.iter().any(|(k, _)| k == &OsStr::new("PATH"));
        assert!(has_path, "PATH must be in the env allowlist");
    }

    #[cfg(windows)]
    #[test]
    fn test_build_command_restores_windows_env_vars() {
        let cmd = build_test_command();
        let env_keys: Vec<_> = cmd
            .get_envs()
            .map(|(k, _)| k.to_string_lossy().to_string())
            .collect();
        for var in &["USERPROFILE", "SystemRoot", "COMSPEC", "PATHEXT", "APPDATA"] {
            // Only assert presence if the var exists in the current env
            if std::env::var(var).is_ok() {
                assert!(
                    env_keys.contains(&var.to_string()),
                    "{var} must be in the env allowlist on Windows"
                );
            }
        }
    }

    #[test]
    fn test_build_command_includes_eval_arg() {
        let cmd = build_test_command();
        let args: Vec<_> = cmd.get_args().collect();
        assert!(
            args.contains(&OsStr::new("-eval")),
            "must include -eval arg"
        );
        assert!(
            args.contains(&OsStr::new("ok.")),
            "must include the eval command"
        );
    }
}
