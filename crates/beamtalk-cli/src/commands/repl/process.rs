// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Process management for BEAM node lifecycle.
//!
//! **DDD Context:** REPL — Process Management

use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::time::Duration;

use miette::{Result, miette};
use tracing::{info, warn};

use super::helpers::find_runtime_dir;
use super::{MAX_CONNECT_RETRIES, RETRY_DELAY_MS, ReplClient};

pub(super) fn has_beam_files(dir: &Path) -> bool {
    dir.is_dir()
        && std::fs::read_dir(dir)
            .map(|entries| {
                entries
                    .flatten()
                    .any(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
            })
            .unwrap_or(false)
}

/// Start the BEAM node with REPL backend.
pub(super) fn start_beam_node(port: u16, node_name: Option<&String>) -> Result<Child> {
    // Find runtime directory - try multiple locations
    let runtime_dir = find_runtime_dir()?;
    info!("Using runtime at: {}", runtime_dir.display());

    // Build runtime first
    let build_lib_dir = runtime_dir.join("_build/default/lib");
    let runtime_beam_dir = build_lib_dir.join("beamtalk_runtime/ebin");
    let jsx_beam_dir = build_lib_dir.join("jsx/ebin");
    // Stdlib beams are produced by `beamtalk build-stdlib` under apps/, not _build/
    let stdlib_beam_dir = runtime_dir.join("apps/beamtalk_stdlib/ebin");

    // Check if runtime is built
    if !runtime_beam_dir.exists() {
        info!("Building Beamtalk runtime...");
        let status = Command::new("rebar3")
            .arg("compile")
            .current_dir(&runtime_dir)
            .status()
            .map_err(|e| miette!("Failed to build runtime: {e}"))?;

        if !status.success() {
            return Err(miette!("Failed to build Beamtalk runtime"));
        }
    }

    info!("Starting BEAM node with REPL backend on port {port}...");

    // Warn if stdlib is not compiled (directory may exist without .beam files)
    if !has_beam_files(&stdlib_beam_dir) {
        warn!("Stdlib not compiled — run `beamtalk build-stdlib` to enable stdlib classes in REPL");
    }

    // Build the eval command that configures the runtime via application:set_env
    // This allows runtime to read port from application environment
    let eval_cmd = if let Some(name) = node_name {
        format!(
            "application:set_env(beamtalk_runtime, repl_port, {port}), \
             application:set_env(beamtalk_runtime, node_name, '{name}'), \
             {{ok, _}} = beamtalk_repl:start_link(), \
             io:format(\"REPL backend started on port {port} (node: {name})~n\"), \
             receive stop -> ok end."
        )
    } else {
        format!(
            "application:set_env(beamtalk_runtime, repl_port, {port}), \
             {{ok, _}} = beamtalk_repl:start_link(), \
             io:format(\"REPL backend started on port {port}~n\"), \
             receive stop -> ok end."
        )
    };

    // Start erl with beamtalk_repl running
    // The receive loop keeps the BEAM VM alive while REPL is running
    let mut args = vec![
        "-noshell".to_string(),
        "-pa".to_string(),
        runtime_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        jsx_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        stdlib_beam_dir.to_str().unwrap_or("").to_string(),
    ];

    // Add node name if specified (use -sname for short names without domain)
    if let Some(name) = node_name {
        // Use -sname for names without dots, -name for FQDN
        if name.contains('.') && !name.ends_with("@localhost") {
            args.push("-name".to_string());
        } else {
            args.push("-sname".to_string());
        }
        args.push(name.clone());
    }

    args.push("-eval".to_string());
    args.push(eval_cmd);

    let child = Command::new("erl")
        .args(&args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| miette!("Failed to start BEAM node: {e}\nIs Erlang/OTP installed?"))?;

    Ok(child)
}

/// Connect to REPL backend with retries.
pub(super) fn connect_with_retries(port: u16) -> Result<ReplClient> {
    for attempt in 1..=MAX_CONNECT_RETRIES {
        match ReplClient::connect(port) {
            Ok(client) => return Ok(client),
            Err(e) => {
                if attempt == MAX_CONNECT_RETRIES {
                    return Err(e);
                }
                std::thread::sleep(Duration::from_millis(RETRY_DELAY_MS));
            }
        }
    }
    Err(miette!("Failed to connect to REPL backend"))
}

/// Guard to ensure BEAM child process is killed on drop.
pub(super) struct BeamChildGuard {
    pub(super) child: Child,
}

impl Drop for BeamChildGuard {
    fn drop(&mut self) {
        let _ = self.child.kill();
        // Wait to reap the process and prevent zombies
        let _ = self.child.wait();
    }
}

/// Default REPL port in the ephemeral range (49152-65535).
/// This avoids conflicts with common services (PHP-FPM on 9000, Prometheus on 9090, etc.)
pub(super) const DEFAULT_REPL_PORT: u16 = 49152;

/// Resolve the REPL port from CLI arg and environment variable.
/// Priority: CLI flag > `BEAMTALK_REPL_PORT` env var > default (49152)
pub(super) fn resolve_port(port_arg: Option<u16>) -> Result<u16> {
    if let Some(p) = port_arg {
        // CLI flag explicitly set
        Ok(p)
    } else if let Ok(env_port) = std::env::var("BEAMTALK_REPL_PORT") {
        // Use env var if set
        env_port
            .parse()
            .map_err(|_| miette!("Invalid BEAMTALK_REPL_PORT: {env_port}"))
    } else {
        // Use default
        Ok(DEFAULT_REPL_PORT)
    }
}

/// Resolve the node name from CLI arg and environment variable.
/// Priority: CLI flag > `BEAMTALK_NODE_NAME` env var > None
pub(super) fn resolve_node_name(node_arg: Option<String>) -> Option<String> {
    node_arg.or_else(|| std::env::var("BEAMTALK_NODE_NAME").ok())
}
