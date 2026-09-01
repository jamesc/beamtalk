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
use tracing::{debug, info, warn};

use beamtalk_cli::repl_startup;

use super::{MAX_CONNECT_RETRIES, RETRY_DELAY_MS, ReplClient};

/// Start the BEAM node with REPL backend.
///
/// The BEAM process's working directory is set to `project_root` so that
/// relative file paths (e.g., `File lines: "data.csv"`) resolve correctly.
#[allow(clippy::too_many_lines)] // BEAM node startup involves many sequential setup steps
pub(crate) fn start_beam_node(
    node_name: Option<&String>,
    project_root: &Path,
    config: &crate::commands::workspace::WorkspaceConfig<'_>,
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

    info!(
        "Starting BEAM node with REPL backend on port {}...",
        config.port
    );

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
        repl_startup::build_eval_cmd_with_node(
            config.port,
            name,
            config.bind_addr,
            config.log_level,
            config.otp_app_name,
            config.hex_dep_names,
        )
    } else {
        repl_startup::build_eval_cmd(
            config.port,
            config.bind_addr,
            config.log_level,
            config.otp_app_name,
            config.hex_dep_names,
        )
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

    let mut cmd = Command::new("erl");
    cmd.arg("-noshell")
        .args(&args)
        // Redirect OTP default logger to stderr from boot (BT-1431). The startup
        // prelude removes the default handler, but there's a window between VM
        // start and the -eval code where early events (e.g. application startup
        // failures) would otherwise go to stdout and be parsed as protocol.
        .arg("-kernel")
        .arg("logger")
        .arg(repl_startup::KERNEL_LOGGER_STDERR)
        .current_dir(project_root)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    // Tell the compiler app where to find the compiler port binary.
    // In installed mode, it lives next to the beamtalk binary in bin/.
    // In dev mode, it lives in target/{debug,release}/.
    if let Ok(exe) = std::env::current_exe() {
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

    let child = cmd
        .spawn()
        .map_err(|e| miette!("Failed to start BEAM node: {e}\nIs Erlang/OTP installed?"))?;

    Ok(child)
}

/// Connect to REPL backend with retries.
pub(crate) fn connect_with_retries(host: &str, port: u16, cookie: &str) -> Result<ReplClient> {
    connect_with_retries_and_delay(host, port, cookie, MAX_CONNECT_RETRIES, RETRY_DELAY_MS)
}

/// [`connect_with_retries`]'s retry loop, parameterized on attempt count/delay
/// so tests can exhaust retries against a dead/unreachable port without
/// paying `MAX_CONNECT_RETRIES * RETRY_DELAY_MS` of real wall-clock time
/// (mirrors `client.rs`'s `reconnect_with_retries`, BT-3375).
fn connect_with_retries_and_delay(
    host: &str,
    port: u16,
    cookie: &str,
    max_retries: u32,
    retry_delay_ms: u64,
) -> Result<ReplClient> {
    for attempt in 1..=max_retries {
        match ReplClient::connect(host, port, cookie) {
            Ok(client) => return Ok(client),
            Err(e) => {
                if attempt == max_retries {
                    return Err(e);
                }
                std::thread::sleep(Duration::from_millis(retry_delay_ms));
            }
        }
    }
    Err(miette!("Failed to connect to REPL backend"))
}

/// Guard to ensure BEAM child process is killed on drop.
pub(crate) struct BeamChildGuard {
    /// The managed BEAM child process.
    pub(crate) child: Child,
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
pub(crate) fn read_port_from_child(child: &mut Child) -> Result<u16> {
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

/// Drain stderr from a BEAM child process in a background thread (BT-1431).
///
/// OTP logger output and VM diagnostics go to stderr. Without draining, the
/// pipe buffer fills up and the BEAM node blocks. Lines are logged via
/// `tracing::debug` so they appear in `RUST_LOG=debug` output.
///
/// Takes ownership of the child's stderr handle via `take()`.
/// The returned `JoinHandle` can be ignored — the thread exits when the
/// child process closes stderr (i.e. on exit).
pub(crate) fn drain_child_stderr(child: &mut Child) -> Option<std::thread::JoinHandle<()>> {
    let stderr = child.stderr.take()?;
    Some(std::thread::spawn(move || {
        let reader = BufReader::new(stderr);
        for line in reader.lines() {
            match line {
                Ok(line) if !line.is_empty() => {
                    debug!(target: "beamtalk::beam_stderr", "{}", line);
                }
                Ok(_) => {} // skip empty lines
                Err(_) => break,
            }
        }
    }))
}

/// Default REPL port (`0` = OS-assigned ephemeral port).
///
/// Using `0` eliminates port conflicts between multiple workspaces or other
/// services. Override with the `BEAMTALK_REPL_PORT` env var or `--port` flag.
pub(crate) const DEFAULT_REPL_PORT: u16 = 0;

/// Resolve the REPL port from CLI arg and environment variable.
/// Priority: CLI flag > `BEAMTALK_REPL_PORT` env var > default (0 = OS-assigned)
pub(crate) fn resolve_port(port_arg: Option<u16>) -> Result<u16> {
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
pub(crate) fn resolve_node_name(node_arg: Option<String>) -> Option<String> {
    node_arg.or_else(|| std::env::var("BEAMTALK_NODE_NAME").ok())
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[test]
    fn drain_child_stderr_takes_handle_and_drains() {
        // Spawn a child that writes to stderr then exits.
        let mut child = std::process::Command::new("sh")
            .arg("-c")
            .arg("echo 'test stderr line' >&2")
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to spawn test child");

        let handle = drain_child_stderr(&mut child);
        assert!(handle.is_some(), "should return a join handle");
        assert!(child.stderr.is_none(), "stderr should be taken");

        // Wait for child and drain thread to finish.
        let _ = child.wait();
        handle
            .unwrap()
            .join()
            .expect("drain thread should not panic");
    }

    #[test]
    fn drain_child_stderr_returns_none_without_pipe() {
        // Child with no stderr pipe → drain returns None.
        let mut child = std::process::Command::new("true")
            .stderr(Stdio::null())
            .spawn()
            .expect("Failed to spawn test child");

        assert!(drain_child_stderr(&mut child).is_none());
        let _ = child.wait();
    }

    #[test]
    fn read_port_from_child_parses_happy_path() {
        let mut child = std::process::Command::new("sh")
            .arg("-c")
            .arg("echo 'BEAMTALK_PORT:12345'")
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn failed");

        let port = read_port_from_child(&mut child).expect("should parse port");
        assert_eq!(port, 12345);
        let _ = child.wait();
    }

    #[test]
    fn read_port_from_child_skips_non_matching_lines() {
        // Port line appears after unrelated startup lines.
        let mut child = std::process::Command::new("sh")
            .arg("-c")
            .arg("printf 'some startup line\\nother line\\nBEAMTALK_PORT:8080\\n'")
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn failed");

        let port =
            read_port_from_child(&mut child).expect("should parse port after non-matching lines");
        assert_eq!(port, 8080);
        let _ = child.wait();
    }

    #[test]
    fn read_port_from_child_rejects_invalid_port_number() {
        // The prefix matches but the port is not a valid u16 — must return an error.
        let mut child = std::process::Command::new("sh")
            .arg("-c")
            .arg("echo 'BEAMTALK_PORT:not_a_port'")
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn failed");

        let result = read_port_from_child(&mut child);
        assert!(result.is_err(), "expected error for non-numeric port");
        let _ = child.wait();
    }

    #[test]
    fn read_port_from_child_rejects_out_of_range_port() {
        // A number too large for u16 must also fail.
        let mut child = std::process::Command::new("sh")
            .arg("-c")
            .arg("echo 'BEAMTALK_PORT:99999'")
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn failed");

        let result = read_port_from_child(&mut child);
        assert!(result.is_err(), "expected error for out-of-range port");
        let _ = child.wait();
    }

    #[test]
    fn read_port_from_child_fails_without_stdout_pipe() {
        // When stdout is not piped, take() returns None → function returns an error.
        let mut child = std::process::Command::new("true")
            .stdout(Stdio::null())
            .spawn()
            .expect("spawn failed");

        let result = read_port_from_child(&mut child);
        assert!(result.is_err(), "expected error when stdout is not piped");
        let _ = child.wait();
    }

    #[test]
    fn resolve_port_uses_explicit_arg() {
        // CLI flag wins regardless of environment.
        let result = resolve_port(Some(4242));
        assert_eq!(result.expect("should succeed"), 4242);
    }

    #[test]
    #[serial_test::serial(env_var)]
    fn resolve_port_uses_env_var() {
        // Uses BEAMTALK_REPL_PORT=9090 when no CLI flag is given.
        // SAFETY: Test is serialized with #[serial(env_var)]; env var is removed before returning.
        unsafe { std::env::set_var("BEAMTALK_REPL_PORT", "9090") };
        let result = resolve_port(None);
        // SAFETY: Test is serialized with #[serial(env_var)].
        unsafe { std::env::remove_var("BEAMTALK_REPL_PORT") };
        assert_eq!(result.expect("should succeed"), 9090);
    }

    #[test]
    #[serial_test::serial(env_var)]
    fn resolve_port_rejects_invalid_env_var() {
        // Non-numeric BEAMTALK_REPL_PORT must produce an error.
        // SAFETY: Test is serialized with #[serial(env_var)]; env var is removed before returning.
        unsafe { std::env::set_var("BEAMTALK_REPL_PORT", "not_a_port") };
        let result = resolve_port(None);
        // SAFETY: Test is serialized with #[serial(env_var)].
        unsafe { std::env::remove_var("BEAMTALK_REPL_PORT") };
        assert!(result.is_err(), "expected parse error for invalid env port");
    }

    #[test]
    #[serial_test::serial(env_var)]
    fn resolve_port_returns_default_without_arg_or_env() {
        // With no CLI flag and no env var, must return the OS-assigned sentinel (0).
        // SAFETY: Test is serialized with #[serial(env_var)].
        unsafe { std::env::remove_var("BEAMTALK_REPL_PORT") };
        let result = resolve_port(None);
        assert_eq!(result.expect("should succeed"), DEFAULT_REPL_PORT);
    }

    #[test]
    fn resolve_node_name_returns_explicit_arg() {
        let result = resolve_node_name(Some("mynode".to_string()));
        assert_eq!(result, Some("mynode".to_string()));
    }

    #[test]
    #[serial_test::serial(env_var)]
    fn resolve_node_name_falls_back_to_env_var() {
        // SAFETY: Test is serialized with #[serial(env_var)]; env var is removed before returning.
        unsafe { std::env::set_var("BEAMTALK_NODE_NAME", "envnode") };
        let result = resolve_node_name(None);
        // SAFETY: Test is serialized with #[serial(env_var)].
        unsafe { std::env::remove_var("BEAMTALK_NODE_NAME") };
        assert_eq!(result, Some("envnode".to_string()));
    }

    #[test]
    #[serial_test::serial(env_var)]
    fn resolve_node_name_returns_none_when_nothing_set() {
        // SAFETY: Test is serialized with #[serial(env_var)].
        unsafe { std::env::remove_var("BEAMTALK_NODE_NAME") };
        let result = resolve_node_name(None);
        assert_eq!(result, None);
    }

    #[test]
    #[serial_test::serial(env_var)]
    fn resolve_node_name_prefers_explicit_arg_over_env() {
        // SAFETY: Test is serialized with #[serial(env_var)]; env var is removed before returning.
        unsafe { std::env::set_var("BEAMTALK_NODE_NAME", "envnode") };
        let result = resolve_node_name(Some("clinode".to_string()));
        // SAFETY: Test is serialized with #[serial(env_var)].
        unsafe { std::env::remove_var("BEAMTALK_NODE_NAME") };
        assert_eq!(result, Some("clinode".to_string()));
    }

    // --- connect_with_retries_and_delay ---

    #[test]
    fn connect_with_retries_exhausts_and_returns_last_error() {
        // Bind then immediately drop a listener to get a port nothing is
        // listening on, so every connect attempt fails fast (connection
        // refused) rather than timing out. Small retry count/delay so the
        // exhausted-retries branch is exercised without paying
        // MAX_CONNECT_RETRIES * RETRY_DELAY_MS of real wall-clock time
        // (mirrors client.rs's reconnect_with_retries_exhausts_* tests).
        let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        drop(listener);

        let result = connect_with_retries_and_delay("127.0.0.1", port, "cookie", 2, 5);
        assert!(result.is_err(), "expected connection failure to propagate");
    }
}
