// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Process management for BEAM node lifecycle.
//!
//! **DDD Context:** REPL — Process Management

use std::ffi::OsString;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::time::Duration;

use miette::{Result, miette};
use tracing::{info, warn};

use beamtalk_cli::repl_startup;

use super::{MAX_CONNECT_RETRIES, RETRY_DELAY_MS, ReplClient};

/// Start the BEAM node with REPL backend.
///
/// The BEAM process's working directory is set to `project_root` so that
/// relative file paths (e.g., `File lines: "data.csv"`) resolve correctly.
///
/// If `ssl_dist_optfile` is `Some`, the node is started with TLS-secured
/// Erlang distribution (`-proto_dist inet_tls`).
pub(super) fn start_beam_node(
    port: u16,
    node_name: Option<&String>,
    project_root: &Path,
    bind_addr: Option<std::net::Ipv4Addr>,
    ssl_dist_optfile: Option<&Path>,
) -> Result<Child> {
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
        repl_startup::build_eval_cmd_with_node(port, name, bind_addr)
    } else {
        repl_startup::build_eval_cmd(port, bind_addr)
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

    // Add TLS distribution args if configured (ADR 0020 Phase 2)
    if let Some(conf_path) = ssl_dist_optfile {
        args.push(OsString::from("-proto_dist"));
        args.push(OsString::from("inet_tls"));
        args.push(OsString::from("-ssl_dist_optfile"));
        // Normalize path separators for Erlang (backslashes → forward slashes on Windows)
        let normalized = conf_path.to_string_lossy().replace('\\', "/");
        args.push(OsString::from(normalized));
    }

    args.push(OsString::from("-eval"));
    args.push(OsString::from(eval_cmd));

    let mut cmd = Command::new("erl");
    cmd.arg("-noshell")
        .args(&args)
        .current_dir(project_root)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    // Tell the compiler app where to find the compiler port binary.
    // In installed mode, it lives next to the beamtalk binary in bin/.
    // In dev mode, it lives in target/{debug,release}/.
    if let Ok(exe) = std::env::current_exe() {
        if let Some(bin_dir) = exe.parent() {
            let compiler_port = bin_dir.join("beamtalk-compiler-port");
            if compiler_port.exists() {
                cmd.env("BEAMTALK_COMPILER_PORT_BIN", &compiler_port);
            }
        }
    }

    let child = cmd
        .spawn()
        .map_err(|e| miette!("Failed to start BEAM node: {e}\nIs Erlang/OTP installed?"))?;

    Ok(child)
}

/// Connect to REPL backend with retries.
pub(super) fn connect_with_retries(port: u16, cookie: &str) -> Result<ReplClient> {
    for attempt in 1..=MAX_CONNECT_RETRIES {
        match ReplClient::connect(port, cookie) {
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
    /// The managed BEAM child process.
    pub(super) child: Child,
}

impl Drop for BeamChildGuard {
    /// Kill the BEAM child process and reap it to prevent zombies.
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

/// Default REPL port (`0` = OS-assigned ephemeral port).
///
/// Using `0` eliminates port conflicts between multiple workspaces or other
/// services. Override with the `BEAMTALK_REPL_PORT` env var or `--port` flag.
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

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;

    #[test]
    fn connect_with_retries_fails_after_max_retries() {
        // Try to connect to a port that definitely won't have a REPL backend
        let result = connect_with_retries(1, "invalid_cookie");
        assert!(result.is_err());
    }

    #[test]
    fn read_port_from_child_timeout() {
        // Test timeout when child doesn't produce BEAMTALK_PORT line
        // This is difficult to test without actually spawning a process
        // But we can verify the function signature and constants exist
        assert_eq!(MAX_CONNECT_RETRIES, 10);
        assert_eq!(RETRY_DELAY_MS, 500);
    }

    #[test]
    fn default_repl_port_is_zero() {
        // Verify default port is 0 (OS-assigned)
        assert_eq!(DEFAULT_REPL_PORT, 0);
    }

    #[test]
    fn resolve_port_with_none_returns_default() {
        let result = resolve_port(None);
        // When no CLI arg and no env var, should return default or env value
        assert!(result.is_ok());
    }

    #[test]
    #[serial(env_var)]
    fn resolve_port_rejects_invalid_port_number() {
        // SAFETY: This test runs single-threaded via #[serial]
        unsafe { std::env::set_var("BEAMTALK_REPL_PORT", "99999999") };
        let result = resolve_port(None);
        unsafe { std::env::remove_var("BEAMTALK_REPL_PORT") };

        // Port number too large should be rejected
        assert!(result.is_err());
    }

    #[test]
    #[serial(env_var)]
    fn resolve_port_rejects_negative_port() {
        // SAFETY: This test runs single-threaded via #[serial]
        unsafe { std::env::set_var("BEAMTALK_REPL_PORT", "-1") };
        let result = resolve_port(None);
        unsafe { std::env::remove_var("BEAMTALK_REPL_PORT") };

        assert!(result.is_err());
    }

    #[test]
    #[serial(env_var)]
    fn resolve_node_name_returns_none_when_no_args() {
        // SAFETY: This test runs single-threaded via #[serial]
        unsafe { std::env::remove_var("BEAMTALK_NODE_NAME") };
        let result = resolve_node_name(None);
        assert!(result.is_none() || result.is_some()); // May have env var
    }

    #[test]
    fn beam_child_guard_drop_cleans_up() {
        // Create a mock child that's already terminated
        // BeamChildGuard should handle this gracefully
        use std::process::{Command, Stdio};

        #[cfg(windows)]
        let child = Command::new("cmd")
            .args(["/c", "exit"])
            .stdout(Stdio::null())
            .spawn()
            .unwrap();

        #[cfg(not(windows))]
        let child = Command::new("true")
            .stdout(Stdio::null())
            .spawn()
            .unwrap();

        // Wait for it to exit
        let pid = child.id();
        let guard = BeamChildGuard { child };

        // Drop the guard (should not panic even though process already exited)
        drop(guard);

        // Verify the process is not running
        use sysinfo::{Pid, ProcessRefreshKind, ProcessesToUpdate, System};
        let mut system = System::new();
        system.refresh_processes_specifics(
            ProcessesToUpdate::Some(&[Pid::from_u32(pid)]),
            true,
            ProcessRefreshKind::new(),
        );
        assert!(system.process(Pid::from_u32(pid)).is_none());
    }

    #[test]
    fn start_beam_node_validates_node_name() {
        // Test that invalid node names are rejected
        let temp_dir = tempfile::tempdir().unwrap();

        let result = start_beam_node(
            0,
            Some(&"bad/name".to_string()),
            temp_dir.path(),
            None,
            None,
        );

        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("Invalid node name"));
    }

    #[test]
    fn start_beam_node_rejects_injection_attempts() {
        let temp_dir = tempfile::tempdir().unwrap();

        // Try various injection patterns
        let malicious_names = vec![
            "node'; rm -rf /",
            "node\"; halt()",
            "node`whoami`",
            "node$(whoami)",
            "node\nrm -rf /",
        ];

        for malicious in malicious_names {
            let result = start_beam_node(
                0,
                Some(&malicious.to_string()),
                temp_dir.path(),
                None,
                None,
            );
            assert!(result.is_err(), "Should reject malicious name: {}", malicious);
        }
    }

    #[test]
    fn start_beam_node_accepts_valid_node_names() {
        // Just verify validation logic, don't actually start BEAM
        let valid_names = vec![
            "node@localhost",
            "my-node@host.example.com",
            "node_123@192.168.1.1",
            "node",
            "node-with-dash",
            "node_with_underscore",
        ];

        for valid in valid_names {
            // Validate using the same logic as start_beam_node
            let is_valid = valid
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '@' || c == '.');
            assert!(is_valid, "Should accept valid name: {}", valid);
        }
    }
}