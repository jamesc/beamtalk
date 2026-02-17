// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared REPL BEAM node startup configuration and runtime discovery.
//!
//! **DDD Context:** REPL — Startup Configuration & Runtime Discovery
//!
//! This module provides the canonical eval command, code-path arguments,
//! and runtime directory discovery for starting a BEAM node with the
//! Beamtalk REPL backend.  Both the production `beamtalk repl` command
//! (`process.rs`) and the E2E test harness (`tests/e2e.rs`) consume
//! these helpers so that any change to the startup sequence is
//! automatically reflected in tests.

use std::ffi::OsString;
use std::path::{Path, PathBuf};

use miette::{Result, miette};

/// Whether the runtime was found in a development checkout or an installed layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeLayout {
    /// Development: `runtime/_build/default/lib/<app>/ebin/`
    Dev,
    /// Installed: `PREFIX/lib/beamtalk/lib/<app>/ebin/`
    Installed,
}

/// Directories that must be on the BEAM code path (`-pa`) for the REPL to work.
#[derive(Debug)]
pub struct BeamPaths {
    /// Path to the `beamtalk_runtime` application's `ebin/` directory.
    pub runtime_ebin: PathBuf,
    /// Path to the `beamtalk_workspace` application's `ebin/` directory.
    pub workspace_ebin: PathBuf,
    /// Path to the `beamtalk_compiler` application's `ebin/` directory.
    pub compiler_ebin: PathBuf,
    /// Path to the `jsx` JSON library's `ebin/` directory.
    pub jsx_ebin: PathBuf,
    /// Path to the `beamtalk_stdlib` application's `ebin/` directory.
    pub stdlib_ebin: PathBuf,
}

/// Compute the standard `-pa` directories from a runtime root.
///
/// For development layout (`runtime/` dir), paths are under `_build/default/lib/`.
/// For installed layout (`PREFIX/lib/beamtalk/`), paths are under `lib/`.
pub fn beam_paths(runtime_dir: &Path) -> BeamPaths {
    beam_paths_for_layout(runtime_dir, RuntimeLayout::Dev)
}

/// Compute `-pa` directories for a specific layout.
pub fn beam_paths_for_layout(runtime_dir: &Path, layout: RuntimeLayout) -> BeamPaths {
    match layout {
        RuntimeLayout::Dev => {
            let build_lib_dir = runtime_dir.join("_build/default/lib");
            BeamPaths {
                runtime_ebin: build_lib_dir.join("beamtalk_runtime/ebin"),
                workspace_ebin: build_lib_dir.join("beamtalk_workspace/ebin"),
                compiler_ebin: build_lib_dir.join("beamtalk_compiler/ebin"),
                jsx_ebin: build_lib_dir.join("jsx/ebin"),
                // Stdlib beams are produced by `beamtalk build-stdlib` under apps/, not _build/
                stdlib_ebin: runtime_dir.join("apps/beamtalk_stdlib/ebin"),
            }
        }
        RuntimeLayout::Installed => {
            let lib_dir = runtime_dir.join("lib");
            BeamPaths {
                runtime_ebin: lib_dir.join("beamtalk_runtime/ebin"),
                workspace_ebin: lib_dir.join("beamtalk_workspace/ebin"),
                compiler_ebin: lib_dir.join("beamtalk_compiler/ebin"),
                jsx_ebin: lib_dir.join("jsx/ebin"),
                stdlib_ebin: lib_dir.join("beamtalk_stdlib/ebin"),
            }
        }
    }
}

/// Build the Erlang `-eval` command that starts the REPL backend.
///
/// The returned string is suitable for passing as `erl -eval <cmd>`.
/// It performs the following steps:
/// 1. Stores the port in the application environment
/// 2. Starts the `beamtalk_workspace` OTP application (and its dependencies)
/// 3. Starts the workspace supervisor (which starts the REPL TCP server,
///    actor registry, session supervisor, and all singletons)
/// 4. Prints a ready message
/// 5. Blocks forever (the BEAM VM stays alive while the REPL runs)
pub fn build_eval_cmd(port: u16) -> String {
    format!(
        "{}, \
         {{ok, ActualPort}} = beamtalk_repl_server:get_port(), \
         io:format(\"BEAMTALK_PORT:~B~n\", [ActualPort]), \
         receive stop -> ok end.",
        startup_prelude(port),
    )
}

/// Build the Erlang `-eval` command with an explicit node name.
///
/// Like [`build_eval_cmd`] but also stores the node name in the application
/// environment before starting the workspace application.
///
/// The `node_name` is escaped for safe interpolation into an Erlang
/// single-quoted atom literal.
pub fn build_eval_cmd_with_node(port: u16, node_name: &str) -> String {
    let safe_name = escape_erlang_atom(node_name);
    format!(
        "application:set_env(beamtalk_runtime, node_name, '{safe_name}'), \
         {}, \
         {{ok, ActualPort}} = beamtalk_repl_server:get_port(), \
         io:format(\"BEAMTALK_PORT:~B~n\", [ActualPort]), \
         receive stop -> ok end.",
        startup_prelude(port),
    )
}

/// The startup prelude shared by all startup modes.
///
/// Starts the workspace OTP application and the workspace supervisor,
/// which brings up the full supervision tree: REPL TCP server, actor
/// registry, session supervisor, transcript stream, system dictionary,
/// workspace actor, and idle monitor.
///
/// The workspace uses a unique per-run ID and resolves the project path
/// to an absolute directory at startup to avoid cross-run metadata reuse.
///
/// Callers append their own shutdown logic (e.g. `receive stop -> ok end`
/// or cover export).
pub fn startup_prelude(port: u16) -> String {
    format!(
        "application:set_env(beamtalk_runtime, repl_port, {port}), \
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, Cwd}} = file:get_cwd(), \
         {{ok, _}} = beamtalk_workspace_sup:start_link(#{{ \
             workspace_id => list_to_binary(\"foreground_\" ++ integer_to_list(erlang:unique_integer([positive]))), \
             project_path => list_to_binary(Cwd), \
             tcp_port => {port}, \
             auto_cleanup => false, \
             max_idle_seconds => 14400}})"
    )
}

/// Escape characters that are special inside Erlang single-quoted atoms.
fn escape_erlang_atom(s: &str) -> String {
    s.replace('\\', "\\\\").replace('\'', "\\'")
}

/// Collect `-pa` paths as `OsString` arguments for passing to `Command::args`.
///
/// Returns alternating `["-pa", "<path>", "-pa", "<path>", ...]`.
/// Uses `OsString` so non-UTF8 filesystem paths are handled without panicking.
///
/// On Windows, converts backslashes to forward slashes since Erlang expects
/// Unix-style paths in `-pa` arguments (see BT-661).
pub fn beam_pa_args(paths: &BeamPaths) -> Vec<OsString> {
    let dirs = [
        &paths.runtime_ebin,
        &paths.workspace_ebin,
        &paths.compiler_ebin,
        &paths.jsx_ebin,
        &paths.stdlib_ebin,
    ];
    let mut args = Vec::with_capacity(dirs.len() * 2);
    for dir in dirs {
        args.push(OsString::from("-pa"));
        #[cfg(windows)]
        {
            // Convert Windows backslashes to forward slashes for Erlang
            let path_str = dir.to_str().unwrap_or("").replace('\\', "/");
            args.push(OsString::from(path_str));
        }
        #[cfg(not(windows))]
        {
            args.push(dir.as_os_str().to_os_string());
        }
    }
    args
}

// ── Runtime Discovery ──────────────────────────────────────────────

/// Find the runtime directory by checking multiple locations.
///
/// Delegates to [`find_runtime_dir_with_layout`]; see that function for
/// layout detection and resolution order.
///
/// # Errors
///
/// Returns an error if no valid runtime directory is found, or if
/// `BEAMTALK_RUNTIME_DIR` is set but doesn't contain a valid runtime.
pub fn find_runtime_dir() -> Result<PathBuf> {
    let (path, _layout) = find_runtime_dir_with_layout()?;
    Ok(path)
}

/// Like [`find_runtime_dir`] but also returns the detected [`RuntimeLayout`].
///
/// # Errors
///
/// Returns an error if no valid runtime directory is found, or if
/// `BEAMTALK_RUNTIME_DIR` is set but doesn't contain a valid runtime.
pub fn find_runtime_dir_with_layout() -> Result<(PathBuf, RuntimeLayout)> {
    // Check explicit env var first
    if let Ok(dir) = std::env::var("BEAMTALK_RUNTIME_DIR") {
        let path = PathBuf::from(dir);
        if path.join("rebar.config").exists() {
            return Ok((path, RuntimeLayout::Dev));
        }
        return Err(miette!(
            "BEAMTALK_RUNTIME_DIR is set but does not contain a valid runtime (no rebar.config)"
        ));
    }

    // Dev-mode candidates (checked via rebar.config)
    let dev_candidates = [
        // 1. CARGO_MANIFEST_DIR (when running via cargo run)
        std::env::var("CARGO_MANIFEST_DIR")
            .ok()
            .map(|d| PathBuf::from(d).join("../../runtime")),
        // 2. Current working directory (running from repo root)
        Some(PathBuf::from("runtime")),
        // 3. Executable's grandparent (target/debug/beamtalk -> repo root)
        std::env::current_exe().ok().and_then(|exe| {
            exe.parent()
                .and_then(|p| p.parent())
                .and_then(|p| p.parent())
                .map(|p| p.join("runtime"))
        }),
    ];

    for candidate in dev_candidates.into_iter().flatten() {
        if candidate.join("rebar.config").exists() {
            return Ok((candidate, RuntimeLayout::Dev));
        }
    }

    // Installed-mode candidate: {exe_dir}/../lib/beamtalk/
    // Validated by checking for .beam files in the runtime ebin dir
    if let Some(installed_root) = std::env::current_exe()
        .ok()
        .and_then(|exe| exe.parent().map(|p| p.join("../lib/beamtalk")))
        .and_then(|p| p.canonicalize().ok())
    {
        if has_beam_files(&installed_root.join("lib/beamtalk_runtime/ebin")) {
            return Ok((installed_root, RuntimeLayout::Installed));
        }
    }

    Err(miette!(
        "Could not find Beamtalk runtime directory.\n\
        Please run from the repository root or set BEAMTALK_RUNTIME_DIR."
    ))
}

/// Check whether a directory contains compiled `.beam` files.
pub fn has_beam_files(dir: &Path) -> bool {
    dir.is_dir()
        && std::fs::read_dir(dir)
            .map(|entries| {
                entries
                    .flatten()
                    .any(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
            })
            .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_cmd_contains_required_steps() {
        let cmd = build_eval_cmd(9000);
        // Must set application env before starting apps
        assert!(cmd.contains("application:set_env(beamtalk_runtime, repl_port, 9000)"));
        // Must start the workspace OTP application
        assert!(cmd.contains("application:ensure_all_started(beamtalk_workspace)"));
        // Must start the workspace supervisor
        assert!(cmd.contains("beamtalk_workspace_sup:start_link"));
        assert!(cmd.contains("tcp_port => 9000"));
        // Must query and print the actual bound port (for ephemeral port discovery)
        assert!(cmd.contains("beamtalk_repl_server:get_port()"));
        assert!(cmd.contains("BEAMTALK_PORT:"));
        // Must block to keep the VM alive
        assert!(cmd.contains("receive stop -> ok end"));
    }

    #[test]
    fn eval_cmd_with_node_includes_node_name() {
        let cmd = build_eval_cmd_with_node(9000, "mynode");
        assert!(cmd.contains("application:set_env(beamtalk_runtime, node_name, 'mynode')"));
        assert!(cmd.contains("beamtalk_workspace_sup:start_link"));
    }

    #[test]
    fn eval_cmd_with_node_escapes_special_chars() {
        let cmd = build_eval_cmd_with_node(9000, "node'inject");
        assert!(cmd.contains("node\\'inject"));
        assert!(!cmd.contains("node'inject"));
    }

    #[test]
    fn startup_prelude_contains_set_env_and_ensure_started() {
        let prelude = startup_prelude(9000);
        assert!(prelude.contains("application:set_env(beamtalk_runtime, repl_port, 9000)"));
        assert!(prelude.contains("application:ensure_all_started(beamtalk_workspace)"));
        // Must start workspace supervisor (which starts all singletons and services)
        assert!(prelude.contains("beamtalk_workspace_sup:start_link"));
        assert!(prelude.contains("tcp_port => 9000"));
        // Uses unique workspace_id and absolute project_path from Cwd
        assert!(prelude.contains("foreground_"));
        assert!(prelude.contains("unique_integer"));
        assert!(prelude.contains("file:get_cwd()"));
        // Prelude should NOT contain the blocking receive
        assert!(!prelude.contains("receive"));
    }

    #[test]
    fn beam_paths_uses_correct_layout() {
        let paths = beam_paths(Path::new("/rt"));
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_runtime/ebin")
        );
        assert_eq!(
            paths.workspace_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_workspace/ebin")
        );
        assert_eq!(
            paths.compiler_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_compiler/ebin")
        );
        assert_eq!(
            paths.jsx_ebin,
            PathBuf::from("/rt/_build/default/lib/jsx/ebin")
        );
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/rt/apps/beamtalk_stdlib/ebin")
        );
    }

    #[test]
    fn beam_pa_args_alternates_flag_and_path() {
        let paths = beam_paths(Path::new("/rt"));
        let args = beam_pa_args(&paths);
        // Should be 10 elements: 5 dirs × 2 (flag + path)
        assert_eq!(args.len(), 10);
        for i in (0..args.len()).step_by(2) {
            assert_eq!(args[i], "-pa");
        }
    }

    #[test]
    fn beam_paths_installed_layout() {
        let paths = beam_paths_for_layout(
            Path::new("/usr/local/lib/beamtalk"),
            RuntimeLayout::Installed,
        );
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_runtime/ebin")
        );
        assert_eq!(
            paths.workspace_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_workspace/ebin")
        );
        assert_eq!(
            paths.compiler_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_compiler/ebin")
        );
        assert_eq!(
            paths.jsx_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/jsx/ebin")
        );
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_stdlib/ebin")
        );
    }

    #[test]
    fn beam_paths_dev_uses_build_dir() {
        let paths = beam_paths_for_layout(Path::new("/rt"), RuntimeLayout::Dev);
        // Dev layout uses _build/default/lib/ for runtime/workspace/jsx
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_runtime/ebin")
        );
        // But stdlib is under apps/
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/rt/apps/beamtalk_stdlib/ebin")
        );
    }
}
