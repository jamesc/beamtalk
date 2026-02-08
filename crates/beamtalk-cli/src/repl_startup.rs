// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared REPL BEAM node startup configuration.
//!
//! **DDD Context:** REPL — Startup Configuration
//!
//! This module provides the canonical eval command and code-path arguments
//! for starting a BEAM node with the Beamtalk REPL backend.  Both the
//! production `beamtalk repl` command (`process.rs`) and the E2E test
//! harness (`tests/e2e.rs`) consume these helpers so that any change to
//! the startup sequence is automatically reflected in tests.

use std::path::{Path, PathBuf};

/// Directories that must be on the BEAM code path (`-pa`) for the REPL to work.
#[derive(Debug)]
pub struct BeamPaths {
    pub runtime_ebin: PathBuf,
    pub workspace_ebin: PathBuf,
    pub jsx_ebin: PathBuf,
    pub stdlib_ebin: PathBuf,
}

/// Compute the standard `-pa` directories from a runtime root.
pub fn beam_paths(runtime_dir: &Path) -> BeamPaths {
    let build_lib_dir = runtime_dir.join("_build/default/lib");
    BeamPaths {
        runtime_ebin: build_lib_dir.join("beamtalk_runtime/ebin"),
        workspace_ebin: build_lib_dir.join("beamtalk_workspace/ebin"),
        jsx_ebin: build_lib_dir.join("jsx/ebin"),
        // Stdlib beams are produced by `beamtalk build-stdlib` under apps/, not _build/
        stdlib_ebin: runtime_dir.join("apps/beamtalk_stdlib/ebin"),
    }
}

/// Build the Erlang `-eval` command that starts the REPL backend.
///
/// The returned string is suitable for passing as `erl -eval <cmd>`.
/// It performs the following steps:
/// 1. Stores the port in the application environment
/// 2. Starts the `beamtalk_workspace` OTP application (and its dependencies)
/// 3. Starts the REPL TCP listener on the given port
/// 4. Prints a ready message
/// 5. Blocks forever (the BEAM VM stays alive while the REPL runs)
pub fn build_eval_cmd(port: u16) -> String {
    format!(
        "application:set_env(beamtalk_runtime, repl_port, {port}), \
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, _}} = beamtalk_repl:start_link({port}), \
         io:format(\"REPL backend started on port {port}~n\"), \
         receive stop -> ok end."
    )
}

/// Build the Erlang `-eval` command with an explicit node name.
///
/// Like [`build_eval_cmd`] but also stores the node name in the application
/// environment before starting the workspace application.
pub fn build_eval_cmd_with_node(port: u16, node_name: &str) -> String {
    format!(
        "application:set_env(beamtalk_runtime, repl_port, {port}), \
         application:set_env(beamtalk_runtime, node_name, '{node_name}'), \
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, _}} = beamtalk_repl:start_link({port}), \
         io:format(\"REPL backend started on port {port} (node: {node_name})~n\"), \
         receive stop -> ok end."
    )
}

/// Collect `-pa` paths as a `Vec<String>` for passing to `Command::args`.
///
/// Returns alternating `["-pa", "<path>", "-pa", "<path>", ...]`.
pub fn beam_pa_args(paths: &BeamPaths) -> Vec<String> {
    let dirs = [
        &paths.runtime_ebin,
        &paths.workspace_ebin,
        &paths.jsx_ebin,
        &paths.stdlib_ebin,
    ];
    let mut args = Vec::with_capacity(dirs.len() * 2);
    for dir in dirs {
        args.push("-pa".to_string());
        args.push(dir.to_str().unwrap_or("").to_string());
    }
    args
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
        // Must start the REPL TCP listener
        assert!(cmd.contains("beamtalk_repl:start_link(9000)"));
        // Must block to keep the VM alive
        assert!(cmd.contains("receive stop -> ok end"));
    }

    #[test]
    fn eval_cmd_with_node_includes_node_name() {
        let cmd = build_eval_cmd_with_node(9000, "mynode");
        assert!(cmd.contains("application:set_env(beamtalk_runtime, node_name, 'mynode')"));
        assert!(cmd.contains("beamtalk_repl:start_link(9000)"));
    }

    #[test]
    fn beam_paths_uses_correct_layout() {
        let paths = beam_paths(Path::new("/rt"));
        assert_eq!(paths.runtime_ebin, PathBuf::from("/rt/_build/default/lib/beamtalk_runtime/ebin"));
        assert_eq!(paths.workspace_ebin, PathBuf::from("/rt/_build/default/lib/beamtalk_workspace/ebin"));
        assert_eq!(paths.jsx_ebin, PathBuf::from("/rt/_build/default/lib/jsx/ebin"));
        assert_eq!(paths.stdlib_ebin, PathBuf::from("/rt/apps/beamtalk_stdlib/ebin"));
    }

    #[test]
    fn beam_pa_args_alternates_flag_and_path() {
        let paths = beam_paths(Path::new("/rt"));
        let args = beam_pa_args(&paths);
        // Should be 8 elements: 4 dirs × 2 (flag + path)
        assert_eq!(args.len(), 8);
        for i in (0..args.len()).step_by(2) {
            assert_eq!(args[i], "-pa");
        }
    }
}
