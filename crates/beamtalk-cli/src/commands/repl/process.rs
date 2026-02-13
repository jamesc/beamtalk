// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Process management for BEAM node lifecycle.
//!
//! **DDD Context:** REPL — Process Management

use std::ffi::OsString;
use std::io::{BufRead, BufReader};
use std::process::{Child, Command, Stdio};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};
use tracing::{info, warn};

use beamtalk_cli::repl_startup;

use crate::paths::{is_daemon_running, socket_path};

use super::{MAX_CONNECT_RETRIES, RETRY_DELAY_MS, ReplClient};

/// Start the BEAM node with REPL backend.
pub(super) fn start_beam_node(port: u16, node_name: Option<&String>) -> Result<Child> {
    // Find runtime directory - try multiple locations
    let (runtime_dir, layout) = repl_startup::find_runtime_dir_with_layout()?;
    info!("Using runtime at: {}", runtime_dir.display());

    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, layout);

    // Auto-build runtime if not compiled (dev mode only)
    if layout == repl_startup::RuntimeLayout::Dev && !paths.runtime_ebin.exists() {
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

    // In installed mode, runtime must already be present
    if layout == repl_startup::RuntimeLayout::Installed && !paths.runtime_ebin.exists() {
        return Err(miette!(
            "Installed runtime not found at {}.\n\
            Reinstall Beamtalk using your original installation method, or set BEAMTALK_RUNTIME_DIR.",
            paths.runtime_ebin.display()
        ));
    }

    info!("Starting BEAM node with REPL backend on port {port}...");

    // Warn if stdlib is not compiled (directory may exist without .beam files)
    if !repl_startup::has_beam_files(&paths.stdlib_ebin) {
        warn!("Stdlib not compiled — run `beamtalk build-stdlib` to enable stdlib classes in REPL");
    }

    // Build the eval command using the shared builder (BT-390)
    let eval_cmd = if let Some(name) = node_name {
        // Validate node name to prevent injection into Erlang eval string
        if !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '@' || c == '.')
        {
            return Err(miette!(
                "Invalid node name '{name}': must contain only alphanumeric characters, underscores, hyphens, dots, or @"
            ));
        }
        repl_startup::build_eval_cmd_with_node(port, name)
    } else {
        repl_startup::build_eval_cmd(port)
    };

    // Start erl with beamtalk_workspace running
    // The receive loop keeps the BEAM VM alive while REPL is running
    let mut args = repl_startup::beam_pa_args(&paths);

    // Add node name if specified
    if let Some(name) = node_name {
        if let Some((local, host)) = name.split_once('@') {
            // Full node name with host — always use -name
            if local.is_empty() || host.is_empty() {
                return Err(miette!(
                    "Invalid node name '{name}': expected format 'name@host'"
                ));
            }
            args.push(OsString::from("-name"));
        } else if name.contains('.') {
            // FQDN without @ — use -name
            args.push(OsString::from("-name"));
        } else {
            // Simple short name — use -sname
            args.push(OsString::from("-sname"));
        }
        args.push(OsString::from(name.as_str()));
    }

    args.push(OsString::from("-eval"));
    args.push(OsString::from(eval_cmd));

    let child = Command::new("erl")
        .arg("-noshell")
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

/// Read the actual port from a BEAM child process's stdout.
/// The BEAM node prints `BEAMTALK_PORT:<port>` after binding to the OS-assigned port.
///
/// Uses a background thread for reading so the deadline is enforced even if
/// the child blocks without producing a full line. Takes ownership of stdout
/// via `take()` since it must be moved into the thread; this is fine because
/// all further BEAM communication uses TCP, not stdout.
pub(super) fn read_port_from_child(child: &mut Child) -> Result<u16> {
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| miette!("Cannot read stdout from BEAM child process"))?;

    let (tx, rx) = std::sync::mpsc::channel();

    // Read lines in a background thread so the main thread can enforce a timeout.
    std::thread::spawn(move || {
        let reader = BufReader::new(stdout);
        for line in reader.lines() {
            match line {
                Ok(line) => {
                    if tx.send(line).is_err() {
                        break; // receiver dropped (timeout)
                    }
                }
                Err(_) => break,
            }
        }
    });

    let deadline = std::time::Instant::now() + Duration::from_secs(15);
    loop {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            break;
        }
        match rx.recv_timeout(remaining) {
            Ok(line) => {
                if let Some(port_str) = line.strip_prefix("BEAMTALK_PORT:") {
                    let port = port_str
                        .trim()
                        .parse::<u16>()
                        .map_err(|_| miette!("Invalid port in BEAMTALK_PORT line: {port_str}"))?;
                    info!("BEAM node bound to port {port}");
                    return Ok(port);
                }
            }
            Err(_) => break,
        }
    }

    Err(miette!(
        "Timed out waiting for BEAM node to report its port.\n\
         The BEAM node may have failed to start."
    ))
}

/// Default REPL port: 0 means the OS assigns an available ephemeral port.
/// This eliminates port conflicts between multiple workspaces or other services.
/// Use `BEAMTALK_REPL_PORT` env var or `--port` flag to override.
pub(super) const DEFAULT_REPL_PORT: u16 = 0;

/// Resolve the REPL port from CLI arg and environment variable.
/// Priority: CLI flag > `BEAMTALK_REPL_PORT` env var > default (0 = OS-assigned)
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

/// Start the compiler daemon in the background.
pub(super) fn start_daemon() -> Result<()> {
    info!("Starting compiler daemon...");

    // Get path to beamtalk binary (ourselves)
    let exe = std::env::current_exe().into_diagnostic()?;

    // Propagate our socket path so the child daemon uses the same
    // session directory. Without this the child inherits a different PPID
    // and writes its lockfile/socket to a different session folder.
    let socket = socket_path()?;

    // Validate the socket path is UTF-8 so the daemon child can read it
    // back via std::env::var (which requires UTF-8). Non-UTF-8 home dirs
    // are extremely rare but we surface a clear error rather than silently
    // falling back to a PPID-derived path.
    let socket_str = socket
        .to_str()
        .ok_or_else(|| miette!("Daemon socket path is not valid UTF-8: {:?}", socket))?;

    // Ensure the session directory exists before the daemon tries to
    // create its lockfile and socket inside it.
    if let Some(dir) = socket.parent() {
        std::fs::create_dir_all(dir).into_diagnostic()?;
    }

    // Spawn daemon in foreground mode as a background process
    // (background mode in daemon itself is not implemented)
    Command::new(exe)
        .args(["daemon", "start", "--foreground"])
        .env("BEAMTALK_DAEMON_SOCKET", socket_str)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .into_diagnostic()?;

    // Wait a moment for daemon to start
    std::thread::sleep(Duration::from_millis(1000));

    if is_daemon_running()?.is_none() {
        return Err(miette!(
            "Failed to start compiler daemon. Try: beamtalk daemon start --foreground"
        ));
    }

    info!("Compiler daemon started.");
    Ok(())
}
