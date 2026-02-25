// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! End-to-end test framework for Beamtalk language features.
//!
//! This module provides a test harness that validates the complete compilation
//! and execution pipeline by connecting to the REPL via TCP socket.
//!
//! # Test Case Format
//!
//! Test cases are `.bt` files with expected output annotations. Each expression
//! is followed by a comment showing the expected result:
//!
//! ```text
//! 3 + 4
//! // => 7
//!
//! [:x | x + 1] value: 5
//! // => 6
//! ```
//!
//! # Running Tests
//!
//! This test is `#[ignore]` by default due to slow startup (~50s).
//!
//! ```bash
//! # Recommended: Use Just
//! just test-e2e
//!
//! # Or run with cargo (must pass --ignored)
//! cargo test --test e2e -- --ignored
//!
//! # Run with verbose output
//! cargo test --test e2e -- --ignored --nocapture
//! ```

use beamtalk_cli::repl_startup;
use beamtalk_core::source_analysis::is_input_complete;
use serial_test::serial;
use std::env;
use std::fs;
use std::net::TcpStream;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::Duration;
use tungstenite::WebSocket;

/// Default port for the REPL TCP server.
const REPL_PORT: u16 = 9000;

/// Explicit cookie for E2E test BEAM nodes.
/// Both the `erl` process and the WebSocket client use this value so that
/// authentication succeeds regardless of whether `~/.erlang.cookie` exists.
/// The value can be overridden at compile time via the `E2E_COOKIE` environment
/// variable to avoid a single, predictable repo-wide cookie.
const E2E_COOKIE: &str = match option_env!("E2E_COOKIE") {
    Some(v) => v,
    None => "beamtalk_e2e_test_cookie",
};

/// Timeout for REPL operations.
/// Cover-instrumented BEAM is slower; `E2E_COVER` bumps this to 120s.
fn repl_timeout() -> Duration {
    let secs = if env::var("E2E_COVER").is_ok() {
        120
    } else {
        30
    };
    Duration::from_secs(secs)
}

/// Maximum retries when connecting to REPL backend.
const MAX_CONNECT_RETRIES: u32 = 20;

/// Delay between connection retries in milliseconds.
const RETRY_DELAY_MS: u64 = 300;

/// Find the workspace root directory.
fn workspace_root() -> PathBuf {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    PathBuf::from(&manifest_dir)
        .parent()
        .and_then(|p| p.parent())
        .expect("Cannot find workspace root")
        .to_path_buf()
}

/// Find the runtime directory.
fn runtime_dir() -> PathBuf {
    workspace_root().join("runtime")
}

/// Find the E2E test cases directory.
fn test_cases_dir() -> PathBuf {
    workspace_root().join("tests/e2e/cases")
}

/// Session name used for E2E test isolation.
const E2E_SESSION_NAME: &str = "e2e-test";

/// A test case parsed from a `.bt` file.
#[derive(Debug)]
struct TestCase {
    /// The expression to evaluate.
    expression: String,
    /// The expected result (from `// =>` comment).
    expected: String,
    /// Line number in the source file.
    line: usize,
}

/// Expected error from loading a file (from `// @load-error` directives).
#[derive(Debug)]
struct LoadErrorCase {
    /// Path to the file to load.
    path: String,
    /// Substring expected in the error message.
    expected_error: String,
    /// Line number in the test file.
    line: usize,
}

/// Parsed test file with metadata.
#[derive(Debug)]
struct ParsedTestFile {
    /// Files to load before running tests (from `// @load` directives).
    load_files: Vec<String>,
    /// Files expected to fail loading (from `// @load-error` directives).
    load_error_cases: Vec<LoadErrorCase>,
    /// Test cases to run.
    cases: Vec<TestCase>,
    /// Warnings about expressions without assertions.
    warnings: Vec<String>,
}

/// Collect continuation lines for a multi-line expression using `is_input_complete()`
/// to detect unclosed delimiters, trailing operators, etc.
fn collect_continuation_lines(lines: &[&str], i: &mut usize, expression_lines: &mut Vec<String>) {
    while *i < lines.len() && !is_input_complete(&expression_lines.join("\n")) {
        let next_line = lines[*i].trim();
        if next_line.starts_with("// =>") || next_line.is_empty() || next_line.starts_with("//") {
            break;
        }
        expression_lines.push(next_line.to_string());
        *i += 1;
    }
}

/// Parse test cases from a `.bt` file.
///
/// Test format:
/// ```text
/// // @load path/to/file.bt
/// // @load-error path/to/bad.bt => expected error substring
///
/// expression
/// // => expected_result
///
/// spawn_expression
/// // => _
/// ```
///
/// Directives:
/// - `// @load <path>` - Load a file before running tests (relative to workspace root)
/// - `// @load-error <path> => <error>` - Load a file and expect compilation to fail with error containing `<error>`
/// - `// => _` - Wildcard: run expression but don't check result (useful for spawn, side effects)
fn parse_test_file(content: &str) -> ParsedTestFile {
    let mut cases = Vec::new();
    let mut load_files = Vec::new();
    let mut load_error_cases = Vec::new();
    let mut warnings = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Check for @load-error directive (must be checked before @load)
        if let Some(rest) = line.strip_prefix("// @load-error") {
            let rest = rest.trim();
            if rest.is_empty() {
                warnings.push(format!(
                    "Line {}: Malformed @load-error directive (expected `// @load-error path.bt => error substring`): {line}",
                    i + 1
                ));
            } else if let Some((path, expected)) = rest.split_once("=>") {
                let path = path.trim();
                let expected = expected.trim();
                if path.is_empty() || expected.is_empty() {
                    warnings.push(format!(
                        "Line {}: Malformed @load-error directive (expected `// @load-error path.bt => error substring`): {line}",
                        i + 1
                    ));
                } else {
                    load_error_cases.push(LoadErrorCase {
                        path: path.to_string(),
                        expected_error: expected.to_string(),
                        line: i + 1,
                    });
                }
            } else {
                warnings.push(format!(
                    "Line {}: Malformed @load-error directive (expected `// @load-error path.bt => error substring`): {line}",
                    i + 1
                ));
            }
            i += 1;
            continue;
        }

        // Check for @load directive
        if let Some(path) = line.strip_prefix("// @load") {
            let path = path.trim();
            if !path.is_empty() {
                load_files.push(path.to_string());
            }
            i += 1;
            continue;
        }

        // Skip empty lines and standalone comments
        if line.is_empty() || (line.starts_with("//") && !line.starts_with("// =>")) {
            i += 1;
            continue;
        }

        // Skip lines that are just "// =>" markers (they belong to previous expression)
        if line.starts_with("// =>") {
            i += 1;
            continue;
        }

        // This should be an expression — possibly spanning multiple lines.
        let mut expression_lines = vec![line.to_string()];
        let expr_line = i + 1;
        i += 1;
        collect_continuation_lines(&lines, &mut i, &mut expression_lines);
        let expression = expression_lines.join("\n");

        // Look for the expected result on the current line
        if i < lines.len() {
            let next_line = lines[i].trim();
            if let Some(expected) = next_line.strip_prefix("// =>") {
                cases.push(TestCase {
                    expression,
                    expected: expected.trim().to_string(),
                    line: expr_line,
                });
                i += 1; // Consume the assertion line
            } else {
                // Warn about expression without assertion
                // Don't increment i - the next line might be another expression
                warnings.push(format!(
                    "Line {expr_line}: Expression will not be executed (missing // => assertion): {expression}"
                ));
            }
        } else {
            // End of file without assertion
            warnings.push(format!(
                "Line {expr_line}: Expression will not be executed (missing // => assertion): {expression}"
            ));
        }
    }

    ParsedTestFile {
        load_files,
        load_error_cases,
        cases,
        warnings,
    }
}

/// Manages the BEAM process for tests.
struct ProcessManager {
    beam_process: Option<Child>,
    #[cfg_attr(not(unix), allow(dead_code))]
    cover_enabled: bool,
}

/// Build the `-eval` command for the BEAM node.
///
/// When `cover` is true, instruments runtime modules with Erlang cover and
/// polls for a signal file to trigger graceful shutdown with cover export.
/// The non-cover path delegates to the shared `repl_startup` module (BT-390)
/// so the E2E startup matches production exactly.
fn beam_eval_cmd(
    cover: bool,
    ebin: &str,
    workspace_ebin: &str,
    signal: &str,
    export: &str,
) -> String {
    if cover {
        // Cover mode wraps instrumentation BEFORE starting the workspace.
        // Cover-compiling must happen before the workspace supervisor spawns
        // the acceptor process, otherwise spawned closures use uninstrumented code.
        let prelude = repl_startup::startup_prelude(REPL_PORT, None, None);
        format!(
            "cover:start(), \
             case cover:compile_beam_directory(\"{ebin}\") of \
                 {{error, R}} -> io:format(standard_error, \"Cover compile failed: ~p~n\", [R]), halt(1); \
                 _ -> ok \
             end, \
             case cover:compile_beam_directory(\"{workspace_ebin}\") of \
                 {{error, R2}} -> io:format(standard_error, \"Cover compile workspace failed: ~p~n\", [R2]), halt(1); \
                 _ -> ok \
             end, \
             {prelude}, \
             WaitFun = fun Wait() -> \
                 case filelib:is_file(\"{signal}\") of \
                     true -> ok; \
                     false -> timer:sleep(200), Wait() \
                 end \
             end, \
             WaitFun(), \
             case cover:export(\"{export}\") of \
                 ok -> ok; \
                 ExpErr -> io:format(standard_error, \"Cover export failed: ~p~n\", [ExpErr]), halt(1) \
             end, \
             cover:stop(), \
             init:stop().",
        )
    } else {
        repl_startup::build_eval_cmd(REPL_PORT, None, None)
    }
}

impl ProcessManager {
    /// Start the BEAM REPL backend if not already running.
    ///
    /// Note: If the test detects an existing REPL on the port, it will use that
    /// and not start its own processes. This is useful for development but means
    /// the test won't manage the lifecycle. If the external REPL fails or stops
    /// mid-test, errors may be confusing. For CI, always start with a clean state.
    #[allow(clippy::too_many_lines)] // REPL startup with build, BEAM node init, and retry loop
    fn start() -> Self {
        // Check if REPL is already running by trying to connect
        if TcpStream::connect(format!("127.0.0.1:{REPL_PORT}")).is_ok() {
            eprintln!("E2E: REPL already running on port {REPL_PORT}");
            return Self {
                beam_process: None,
                cover_enabled: false,
            };
        }

        // Check if debug output is requested via environment variable
        let debug_output = env::var("E2E_DEBUG").is_ok();

        // Start the BEAM node with REPL backend
        eprintln!("E2E: Starting BEAM REPL backend...");
        let runtime = runtime_dir();
        let paths = repl_startup::beam_paths(&runtime);

        // Build runtime if needed
        if !paths.runtime_ebin.exists() {
            eprintln!("E2E: Building runtime...");
            let status = Command::new("rebar3")
                .arg("compile")
                .current_dir(&runtime)
                .status()
                .expect("Failed to run rebar3 compile");

            assert!(status.success(), "Failed to build runtime");
        }

        let (stdout_cfg, stderr_cfg) = if debug_output {
            (Stdio::inherit(), Stdio::inherit())
        } else {
            (Stdio::null(), Stdio::null())
        };

        // When E2E_COVER=1, instrument all runtime modules with Erlang's cover
        // tool and export coverdata on shutdown via a signal file.
        let cover_enabled = env::var("E2E_COVER").is_ok();
        let cover_export_path = runtime.join("_build/test/cover/e2e.coverdata");
        let cover_signal_path = runtime.join("_build/test/cover/.e2e_stop");
        if cover_enabled {
            // Ensure cover directory exists (may not after `just clean`)
            let _ = fs::create_dir_all(runtime.join("_build/test/cover"));
            eprintln!(
                "E2E: Cover mode enabled, will export to {}",
                cover_export_path.display()
            );
        }
        let _ = fs::remove_file(&cover_signal_path);
        let eval_cmd = beam_eval_cmd(
            cover_enabled,
            paths
                .runtime_ebin
                .to_str()
                .expect("ebin path must be UTF-8"),
            paths
                .workspace_ebin
                .to_str()
                .expect("workspace ebin path must be UTF-8"),
            cover_signal_path
                .to_str()
                .expect("signal path must be UTF-8"),
            cover_export_path
                .to_str()
                .expect("export path must be UTF-8"),
        );

        let mut pa_args = repl_startup::beam_pa_args(&paths);
        pa_args.push("-eval".into());
        pa_args.push(eval_cmd.into());

        let beam_child = Command::new("erl")
            .arg("-noshell")
            .arg("-sname")
            .arg("beamtalk_e2e_test")
            .arg("-setcookie")
            .arg(E2E_COOKIE)
            .args(&pa_args)
            .current_dir(workspace_root())
            .env("BEAMTALK_WORKSPACE", E2E_SESSION_NAME)
            .stdout(stdout_cfg)
            .stderr(stderr_cfg)
            .spawn()
            .expect("Failed to start BEAM node");

        // Wait for REPL to be ready
        let mut retries = MAX_CONNECT_RETRIES;
        while retries > 0 {
            std::thread::sleep(Duration::from_millis(RETRY_DELAY_MS));
            if TcpStream::connect(format!("127.0.0.1:{REPL_PORT}")).is_ok() {
                eprintln!("E2E: REPL ready on port {REPL_PORT}");
                return Self {
                    beam_process: Some(beam_child),
                    cover_enabled,
                };
            }
            retries -= 1;
        }

        eprintln!("E2E: Warning - REPL may not be fully started. Port {REPL_PORT} not responding.");

        Self {
            beam_process: Some(beam_child),
            cover_enabled,
        }
    }

    /// Stop the BEAM if we started it.
    ///
    /// Uses SIGTERM first for graceful shutdown, then SIGKILL if needed.
    #[cfg(unix)]
    #[expect(
        clippy::cast_possible_wrap,
        reason = "PIDs are always positive and fit in i32 on Unix"
    )]
    fn stop(&mut self) {
        if let Some(ref mut child) = self.beam_process {
            if self.cover_enabled {
                // Signal the BEAM to export cover data and shut down gracefully
                // by creating the signal file it polls for.
                let runtime = runtime_dir();
                let signal = runtime.join("_build/test/cover/.e2e_stop");
                eprintln!("E2E: Signaling cover export...");
                let _ = fs::write(&signal, "stop");
                // Wait up to 30s for BEAM to export and call init:stop()
                let deadline = std::time::Instant::now() + Duration::from_secs(30);
                let mut clean_exit = false;
                loop {
                    match child.try_wait() {
                        Ok(Some(_)) => {
                            clean_exit = true;
                            break;
                        }
                        Ok(None) if std::time::Instant::now() < deadline => {
                            std::thread::sleep(Duration::from_millis(200));
                        }
                        _ => {
                            eprintln!("E2E: Cover export timed out, force killing BEAM");
                            let _ = child.kill();
                            let _ = child.wait();
                            break;
                        }
                    }
                }
                let _ = fs::remove_file(&signal);
                if clean_exit {
                    eprintln!("E2E: Cover data exported.");
                } else {
                    eprintln!(
                        "E2E: Warning - cover data may be incomplete (BEAM was force-killed)."
                    );
                }
            } else {
                eprintln!("E2E: Stopping BEAM...");
                // SAFETY: libc::kill with SIGTERM is safe to call on any pid.
                // If pid doesn't exist, it returns an error which we ignore.
                unsafe {
                    libc::kill(child.id() as i32, libc::SIGTERM);
                }
                std::thread::sleep(Duration::from_millis(200));
                let _ = child.kill();
                let _ = child.wait();
            }
        }
    }

    #[cfg(not(unix))]
    fn stop(&mut self) {
        // Cover mode signal-file shutdown is handled by the unix path.
        // On non-unix, force-kill is the only option.
        if let Some(ref mut child) = self.beam_process {
            eprintln!("E2E: Stopping BEAM...");
            let _ = child.kill();
            let _ = child.wait();
        }
    }
}

impl Drop for ProcessManager {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Tracks the last loaded path for `:reload` support in e2e tests.
#[derive(Clone, Debug)]
enum LastLoadedPath {
    File(String),
    Directory(String),
}

/// Client for communicating with the REPL via WebSocket.
struct ReplClient {
    ws: WebSocket<TcpStream>,
    /// Last warnings from evaluation (for WARNING: assertions)
    last_warnings: Vec<String>,
    /// Last loaded path for `:reload` without arguments
    last_loaded_path: Option<LastLoadedPath>,
}

impl ReplClient {
    /// Connect to the REPL via WebSocket with cookie auth.
    fn connect() -> Result<Self, std::io::Error> {
        let tcp = TcpStream::connect(format!("127.0.0.1:{REPL_PORT}"))?;
        tcp.set_read_timeout(Some(repl_timeout()))?;
        tcp.set_write_timeout(Some(repl_timeout()))?;

        let url = format!("ws://127.0.0.1:{REPL_PORT}/ws");
        let (mut ws, _response) = tungstenite::client(&url, tcp)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::ConnectionRefused, e))?;

        // Read auth-required message (pre-auth, no session yet)
        let auth_required = ws.read().map_err(std::io::Error::other)?;
        if let tungstenite::Message::Text(text) = auth_required {
            let parsed: serde_json::Value = serde_json::from_str(&text).unwrap_or_default();
            if parsed.get("op").and_then(|v| v.as_str()) != Some("auth-required") {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("Expected auth-required, got: {text}"),
                ));
            }
        }

        // Send cookie auth handshake (uses the same E2E_COOKIE passed via -setcookie)
        let auth_msg = serde_json::json!({
            "type": "auth",
            "cookie": E2E_COOKIE
        });
        ws.send(tungstenite::Message::Text(auth_msg.to_string().into()))
            .map_err(std::io::Error::other)?;

        // Read auth response — must be auth_ok
        let auth_response = ws.read().map_err(std::io::Error::other)?;
        if let tungstenite::Message::Text(text) = auth_response {
            let parsed: serde_json::Value = serde_json::from_str(&text).unwrap_or_default();
            match parsed.get("type").and_then(|t| t.as_str()) {
                Some("auth_ok") => {}
                Some("auth_error") => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::PermissionDenied,
                        "Cookie authentication failed",
                    ));
                }
                _ => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!("Expected auth_ok, got: {text}"),
                    ));
                }
            }
        }

        // Read session-started message (sent after successful auth)
        let session_started = ws.read().map_err(std::io::Error::other)?;
        if let tungstenite::Message::Text(text) = &session_started {
            let parsed: serde_json::Value = serde_json::from_str(text).unwrap_or_default();
            if parsed.get("op").and_then(|v| v.as_str()) != Some("session-started") {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("Expected session-started, got: {text}"),
                ));
            }
        }

        Ok(Self {
            ws,
            last_warnings: Vec::new(),
            last_loaded_path: None,
        })
    }

    /// Read the next text message from the WebSocket, skipping push messages.
    fn read_text(&mut self) -> Result<String, String> {
        loop {
            match self.ws.read() {
                Ok(tungstenite::Message::Text(text)) => {
                    // Skip push messages (e.g. Transcript push, actor lifecycle events)
                    if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&text) {
                        if parsed.get("push").is_some()
                            || parsed
                                .get("type")
                                .and_then(|t| t.as_str())
                                .is_some_and(|t| t == "push")
                        {
                            continue;
                        }
                    }
                    return Ok(text.to_string());
                }
                Ok(tungstenite::Message::Close(_)) => {
                    return Err("WebSocket connection closed".to_string());
                }
                Ok(_) => {} // Skip ping/pong/binary
                Err(e) => return Err(format!("WebSocket read error: {e}")),
            }
        }
    }

    /// Send a JSON message over WebSocket.
    fn write_json(&mut self, value: &serde_json::Value) -> Result<(), String> {
        self.ws
            .send(tungstenite::Message::Text(value.to_string().into()))
            .map_err(|e| format!("Failed to send WebSocket message: {e}"))
    }

    /// Evaluate an expression and return the result.
    fn eval(&mut self, expression: &str) -> Result<String, String> {
        // Send JSON request using new protocol format
        let request = serde_json::json!({
            "op": "eval",
            "id": format!("e2e-{}", std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_micros()),
            "code": expression
        });

        self.write_json(&request)?;

        // Read response
        let response_line = self.read_text()?;

        // Parse JSON response (handles both legacy and new protocol)
        let response: serde_json::Value = serde_json::from_str(&response_line)
            .map_err(|e| format!("Failed to parse response: {e}"))?;

        // Extract warnings if present (BT-407)
        self.last_warnings.clear();
        if let Some(warnings) = response.get("warnings").and_then(|w| w.as_array()) {
            for warning in warnings {
                if let Some(warn_str) = warning.as_str() {
                    self.last_warnings.push(warn_str.to_string());
                }
            }
        }

        // New protocol: check status for errors
        if let Some(status) = response.get("status").and_then(|s| s.as_array()) {
            let is_error = status.iter().any(|s| s.as_str() == Some("error"));
            if is_error {
                let message = response
                    .get("error")
                    .and_then(|m| m.as_str())
                    .unwrap_or("Unknown error");
                return Err(message.to_string());
            }
            // Success - extract value
            let value = response.get("value").map_or_else(
                || "null".to_string(),
                |v| {
                    if v.is_string() {
                        v.as_str().unwrap().to_string()
                    } else {
                        v.to_string()
                    }
                },
            );
            return Ok(value);
        }

        // Legacy protocol fallback
        match response.get("type").and_then(|t| t.as_str()) {
            Some("result") => {
                let value = response.get("value").map_or_else(
                    || "null".to_string(),
                    |v| {
                        if v.is_string() {
                            v.as_str().unwrap().to_string()
                        } else {
                            v.to_string()
                        }
                    },
                );
                Ok(value)
            }
            Some("error") => {
                let message = response
                    .get("message")
                    .and_then(|m| m.as_str())
                    .unwrap_or("Unknown error");
                Err(message.to_string())
            }
            _ => Err(format!("Unexpected response: {response_line}")),
        }
    }

    /// Clear REPL bindings between tests.
    fn clear_bindings(&mut self) -> Result<(), String> {
        self.clear_and_report().map(|_| ())
    }

    /// Load a Beamtalk file (for actor/class definitions).
    ///
    /// This compiles the file and loads its classes into the REPL session,
    /// making them available for spawning and messaging.
    fn load_file(&mut self, path: &str) -> Result<Vec<String>, String> {
        let request = serde_json::json!({
            "op": "load-file",
            "id": "e2e-load",
            "path": path
        });

        self.write_json(&request)?;

        // Read response
        let response_line = self.read_text()?;

        // Parse JSON response (handles both legacy and new protocol)
        let response: serde_json::Value = serde_json::from_str(&response_line)
            .map_err(|e| format!("Failed to parse load response: {e}"))?;

        // Extract warnings from load response (BT-737: class collision warnings)
        self.last_warnings.clear();
        if let Some(warnings) = response.get("warnings").and_then(|w| w.as_array()) {
            for warning in warnings {
                if let Some(warn_str) = warning.as_str() {
                    self.last_warnings.push(warn_str.to_string());
                }
            }
        }

        // New protocol: check status for errors
        if let Some(status) = response.get("status").and_then(|s| s.as_array()) {
            let is_error = status.iter().any(|s| s.as_str() == Some("error"));
            if is_error {
                let message = response
                    .get("error")
                    .and_then(|m| m.as_str())
                    .unwrap_or("Unknown error");
                return Err(message.to_string());
            }
            // Success - extract classes
            let classes = response
                .get("classes")
                .and_then(|c| c.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect()
                })
                .unwrap_or_default();
            return Ok(classes);
        }

        // Legacy protocol fallback
        match response.get("type").and_then(|t| t.as_str()) {
            Some("loaded") => {
                let classes = response
                    .get("classes")
                    .and_then(|c| c.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|v| v.as_str().map(String::from))
                            .collect()
                    })
                    .unwrap_or_default();
                Ok(classes)
            }
            Some("error") => {
                let message = response
                    .get("message")
                    .and_then(|m| m.as_str())
                    .unwrap_or("Unknown error");
                Err(message.to_string())
            }
            _ => Err(format!("Unexpected load response: {response_line}")),
        }
    }

    /// Get documentation for a class or method via the docs op.
    fn get_docs(&mut self, class: &str, selector: Option<&str>) -> Result<String, String> {
        let mut request = serde_json::json!({
            "op": "docs",
            "id": "e2e-docs",
            "class": class
        });
        if let Some(sel) = selector {
            request["selector"] = serde_json::Value::String(sel.to_string());
        }

        self.write_json(&request)?;

        let response_line = self.read_text()?;

        let response: serde_json::Value = serde_json::from_str(&response_line)
            .map_err(|e| format!("Failed to parse docs response: {e}"))?;

        // Check for errors (new protocol)
        if let Some(status) = response.get("status").and_then(|s| s.as_array()) {
            let is_error = status.iter().any(|s| s.as_str() == Some("error"));
            if is_error {
                let message = response
                    .get("error")
                    .and_then(|m| m.as_str())
                    .unwrap_or("Unknown error");
                return Err(message.to_string());
            }
        }

        // Check for errors (legacy protocol)
        if response.get("type").and_then(|t| t.as_str()) == Some("error") {
            let message = response
                .get("message")
                .and_then(|m| m.as_str())
                .unwrap_or("Unknown error");
            return Err(message.to_string());
        }

        // Extract docs text
        response
            .get("docs")
            .and_then(|d| d.as_str())
            .map(String::from)
            .ok_or_else(|| format!("No docs field in response: {response_line}"))
    }

    /// Send a generic op request and return the raw JSON response.
    fn send_op(&mut self, request: &serde_json::Value) -> Result<serde_json::Value, String> {
        self.write_json(request)?;

        let response_line = self.read_text()?;

        let response: serde_json::Value = serde_json::from_str(&response_line)
            .map_err(|e| format!("Failed to parse response: {e}"))?;

        // Check for errors
        if let Some(status) = response.get("status").and_then(|s| s.as_array()) {
            if status.iter().any(|s| s.as_str() == Some("error")) {
                let message = response
                    .get("error")
                    .and_then(|m| m.as_str())
                    .unwrap_or("Unknown error");
                return Err(message.to_string());
            }
        }

        Ok(response)
    }

    /// Clear bindings and return "ok" (for use as a testable expression).
    fn clear_and_report(&mut self) -> Result<String, String> {
        let response = self.send_op(&serde_json::json!({
            "op": "clear",
            "id": "e2e-clear"
        }))?;
        Ok(response
            .get("value")
            .and_then(|v| v.as_str())
            .unwrap_or("ok")
            .to_string())
    }

    /// Get current bindings formatted as a readable string.
    fn get_bindings(&mut self) -> Result<String, String> {
        let response = self.send_op(&serde_json::json!({
            "op": "bindings",
            "id": "e2e-bindings"
        }))?;
        let bindings = response
            .get("bindings")
            .and_then(|b| b.as_object())
            .cloned()
            .unwrap_or_default();
        if bindings.is_empty() {
            return Ok("No bindings".to_string());
        }
        let mut entries: Vec<String> = bindings
            .iter()
            .map(|(k, v)| {
                let val = if v.is_string() {
                    v.as_str().unwrap().to_string()
                } else {
                    v.to_string()
                };
                format!("{k} = {val}")
            })
            .collect();
        entries.sort();
        Ok(entries.join("\n"))
    }

    /// Get list of running actors formatted as a readable string.
    fn get_actors(&mut self) -> Result<String, String> {
        let response = self.send_op(&serde_json::json!({
            "op": "actors",
            "id": "e2e-actors"
        }))?;
        let actors = response
            .get("actors")
            .and_then(|a| a.as_array())
            .cloned()
            .unwrap_or_default();
        if actors.is_empty() {
            return Ok("No actors".to_string());
        }
        let entries: Vec<String> = actors
            .iter()
            .map(|a| {
                let class = a.get("class").and_then(|c| c.as_str()).unwrap_or("?");
                let pid = a.get("pid").and_then(|p| p.as_str()).unwrap_or("?");
                format!("{class} ({pid})")
            })
            .collect();
        Ok(entries.join("\n"))
    }

    /// Get list of loaded modules formatted as a readable string.
    fn get_modules(&mut self) -> Result<String, String> {
        let response = self.send_op(&serde_json::json!({
            "op": "modules",
            "id": "e2e-modules"
        }))?;
        let modules = response
            .get("modules")
            .and_then(|m| m.as_array())
            .cloned()
            .unwrap_or_default();
        if modules.is_empty() {
            return Ok("No modules".to_string());
        }
        let entries: Vec<String> = modules
            .iter()
            .map(|m| {
                let name = m.get("name").and_then(|n| n.as_str()).unwrap_or("?");
                let actors = m
                    .get("actor_count")
                    .and_then(serde_json::Value::as_i64)
                    .unwrap_or(0);
                format!("{name} ({actors} actors)")
            })
            .collect();
        Ok(entries.join("\n"))
    }

    /// Reload a module by name.
    fn reload_module(&mut self, module: &str) -> Result<String, String> {
        let response = self.send_op(&serde_json::json!({
            "op": "reload",
            "id": "e2e-reload",
            "module": module
        }))?;
        let classes = response
            .get("classes")
            .and_then(|c| c.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        if classes.is_empty() {
            Ok("Reloaded".to_string())
        } else {
            Ok(format!("Reloaded: {}", classes.join(", ")))
        }
    }

    /// Load a file or directory and return a formatted string for test assertions.
    fn load_and_report(&mut self, path: &str) -> Result<String, String> {
        let root = workspace_root();
        let path = path.trim();
        let path = path
            .strip_prefix('"')
            .and_then(|p| p.strip_suffix('"'))
            .or_else(|| path.strip_prefix('\'').and_then(|p| p.strip_suffix('\'')))
            .unwrap_or(path);
        let full_path = root.join(path);

        if full_path.is_dir() {
            self.load_directory_and_report(&full_path, path, "Loaded")
        } else {
            // BT-845: Store the original relative path for :reload consistency.
            // Using full_path for reload would break if the CWD changes.
            let classes = self.load_file(path)?;
            self.last_loaded_path = Some(LastLoadedPath::File(path.to_string()));
            if classes.is_empty() {
                Ok("Loaded".to_string())
            } else {
                Ok(format!("Loaded: {}", classes.join(", ")))
            }
        }
    }

    /// Load all `.bt` files from a directory recursively and report results.
    fn load_directory_and_report(
        &mut self,
        full_path: &std::path::Path,
        display_path: &str,
        verb: &str,
    ) -> Result<String, String> {
        let mut files = Vec::new();
        Self::discover_bt_files(full_path, &mut files)
            .map_err(|e| format!("Error reading directory {display_path}: {e}"))?;
        files.sort();

        if files.is_empty() {
            return Ok(format!("No .bt files found in {display_path}"));
        }

        let mut loaded: usize = 0;
        let mut failed: Vec<String> = Vec::new();

        for file in &files {
            let file_str = file.to_string_lossy().to_string();
            match self.load_file(&file_str) {
                Ok(_) => loaded += 1,
                Err(e) => {
                    let file_name = file
                        .file_name()
                        .map_or_else(|| file_str.clone(), |n| n.to_string_lossy().to_string());
                    eprintln!("  Error in {file_name}: {e}");
                    failed.push(file_name);
                }
            }
        }

        self.last_loaded_path = Some(LastLoadedPath::Directory(
            full_path.to_string_lossy().to_string(),
        ));

        if failed.is_empty() {
            let word = if loaded == 1 { "file" } else { "files" };
            Ok(format!("{verb} {loaded} {word} from {display_path}"))
        } else {
            let mut parts = Vec::new();
            if loaded > 0 {
                let word = if loaded == 1 { "file" } else { "files" };
                parts.push(format!("{loaded} {word} loaded"));
            }
            let word = if failed.len() == 1 { "file" } else { "files" };
            parts.push(format!(
                "{} {word} failed ({})",
                failed.len(),
                failed.join(", ")
            ));
            Ok(parts.join(", "))
        }
    }

    /// Recursively discover `.bt` files under a directory.
    fn discover_bt_files(dir: &std::path::Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
        let mut entries: Vec<_> = std::fs::read_dir(dir)?
            .filter_map(std::result::Result::ok)
            .collect();
        entries.sort_by_key(std::fs::DirEntry::file_name);

        for entry in entries {
            let name = entry.file_name();
            if name.to_string_lossy().starts_with('.') {
                continue;
            }
            let path = entry.path();
            let file_type = entry.file_type()?;
            if file_type.is_dir() {
                Self::discover_bt_files(&path, files)?;
            } else if file_type.is_file() && path.extension().is_some_and(|ext| ext == "bt") {
                files.push(path);
            }
        }
        Ok(())
    }

    /// Reload the last loaded file or directory.
    fn reload_last(&mut self) -> Result<String, String> {
        match self.last_loaded_path.clone() {
            Some(LastLoadedPath::Directory(full_path)) => {
                let dir = std::path::Path::new(&full_path);
                let display_path = dir
                    .strip_prefix(workspace_root())
                    .map_or_else(|_| full_path.clone(), |p| p.to_string_lossy().to_string());
                self.load_directory_and_report(dir, &display_path, "Reloaded")
            }
            Some(LastLoadedPath::File(path)) => {
                let classes = self.load_file(&path)?;
                if classes.is_empty() {
                    Ok("Reloaded".to_string())
                } else {
                    Ok(format!("Reloaded: {}", classes.join(", ")))
                }
            }
            None => Err("No file or directory has been loaded yet".to_string()),
        }
    }

    /// Inspect an actor's state by PID string.
    fn inspect_actor(&mut self, pid_str: &str) -> Result<String, String> {
        let response = self.send_op(&serde_json::json!({
            "op": "inspect",
            "id": "e2e-inspect",
            "actor": pid_str
        }))?;
        let state = response
            .get("state")
            .cloned()
            .unwrap_or(serde_json::Value::Null);
        match state {
            serde_json::Value::String(s) => Ok(s),
            other => Ok(other.to_string()),
        }
    }

    /// Kill an actor by PID string.
    fn kill_actor(&mut self, pid_str: &str) -> Result<String, String> {
        self.send_op(&serde_json::json!({
            "op": "kill",
            "id": "e2e-kill",
            "actor": pid_str
        }))?;
        Ok("ok".to_string())
    }

    /// Unload a module by name.
    fn unload_module(&mut self, module: &str) -> Result<String, String> {
        self.send_op(&serde_json::json!({
            "op": "unload",
            "id": "e2e-unload",
            "module": module
        }))?;
        Ok("ok".to_string())
    }

    /// Get the PID of the first actor of a given class from the actors list.
    /// Note: actor ordering from the server is non-deterministic (map-based).
    /// This is acceptable for E2E tests where we need any valid actor of a class.
    fn get_first_actor_pid(&mut self, class_name: &str) -> Result<String, String> {
        let response = self.send_op(&serde_json::json!({
            "op": "actors",
            "id": "e2e-actors-pid"
        }))?;
        let actors = response
            .get("actors")
            .and_then(|a| a.as_array())
            .cloned()
            .unwrap_or_default();
        for actor in &actors {
            let class = actor.get("class").and_then(|c| c.as_str()).unwrap_or("");
            if class == class_name {
                if let Some(pid) = actor.get("pid").and_then(|p| p.as_str()) {
                    return Ok(pid.to_string());
                }
            }
        }
        Err(format!("No actor of class {class_name} found"))
    }
}

/// Pattern matcher for test assertions (BT-502).
///
/// Supports glob-style matching where `_` acts as a wildcard segment:
/// - Bare `_` → matches any result (full wildcard)
/// - `_` within a pattern → matches any substring at that position
/// - `#Actor<Counter,_>` matches `#Actor<Counter,0.173.0>`
/// - `{\: Set, elements: _}` matches the full Set output
/// - `Alice` still exact-matches `Alice`
///
/// A `_` is only treated as a wildcard if it is NOT flanked on both sides by
/// alphanumeric characters. This preserves literal underscores in identifiers
/// like `does_not_understand` or `beamtalk_class`.
fn matches_pattern(pattern: &str, actual: &str) -> bool {
    if pattern == "_" {
        return true;
    }
    if !has_wildcard_underscore(pattern) {
        return actual == pattern;
    }

    // Split pattern on wildcard `_` characters only
    let segments = split_on_wildcard_underscores(pattern);
    let mut pos = 0;

    for (i, segment) in segments.iter().enumerate() {
        if segment.is_empty() {
            // Leading, trailing, or consecutive `_` — skip (matches any chars)
            continue;
        }
        if let Some(found) = actual[pos..].find(segment) {
            // First segment must match at the start
            if i == 0 && found != 0 {
                return false;
            }
            pos += found + segment.len();
        } else {
            return false;
        }
    }

    // Last segment must match at the end (unless pattern ends with `_`)
    if let Some(last) = segments.last() {
        if !last.is_empty() {
            return actual.ends_with(last);
        }
    }

    true
}

/// Check if a string contains `_` that should be treated as a wildcard.
///
/// A `_` is a wildcard if it is NOT flanked on both sides by alphanumeric
/// characters. Examples:
/// - `Counter,_>` → wildcard (`,` before, `>` after)
/// - `does_not` → literal (`s` before, `n` after)
fn has_wildcard_underscore(s: &str) -> bool {
    let bytes = s.as_bytes();
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'_' {
            let before_alnum = i > 0 && bytes[i - 1].is_ascii_alphanumeric();
            let after_alnum = i + 1 < bytes.len() && bytes[i + 1].is_ascii_alphanumeric();
            if !before_alnum || !after_alnum {
                return true;
            }
        }
    }
    false
}

/// Split a pattern on wildcard `_` characters only, preserving literal
/// underscores in identifiers.
fn split_on_wildcard_underscores(pattern: &str) -> Vec<&str> {
    let bytes = pattern.as_bytes();
    let mut segments = Vec::new();
    let mut start = 0;

    for i in 0..bytes.len() {
        if bytes[i] == b'_' {
            let before_alnum = i > 0 && bytes[i - 1].is_ascii_alphanumeric();
            let after_alnum = i + 1 < bytes.len() && bytes[i + 1].is_ascii_alphanumeric();
            if !before_alnum || !after_alnum {
                segments.push(&pattern[start..i]);
                start = i + 1;
            }
        }
    }

    segments.push(&pattern[start..]);
    segments
}

/// Run a single test file.
///
/// Note: Bindings are cleared at the start of each file, but NOT between
/// individual test cases within the same file. This allows testing variable
/// persistence across expressions (stateful tests). If you need isolated tests,
/// put them in separate files.
///
/// If the file contains `// @load <path>` directives, those files are loaded
/// before any test cases run, making their classes available for spawning.
#[expect(
    clippy::too_many_lines,
    reason = "Test runner handles many assertion types"
)]
fn run_test_file(path: &PathBuf, client: &mut ReplClient) -> (usize, Vec<String>) {
    let content = fs::read_to_string(path).expect("Failed to read test file");
    let test_file = parse_test_file(&content);

    let file_name = path.file_name().unwrap().to_string_lossy();
    let mut failures = Vec::new();
    let mut pass_count = 0;

    // Treat warnings as test failures
    for warning in &test_file.warnings {
        eprintln!("⚠️  {file_name}: {warning}");
        failures.push(format!("{file_name}: {warning}"));
    }

    // Clear bindings before running file
    if let Err(e) = client.clear_bindings() {
        failures.push(format!("{file_name}: Failed to clear bindings: {e}"));
        return (0, failures);
    }

    // Load any required files (relative to workspace root)
    let root = workspace_root();
    for load_path in &test_file.load_files {
        let full_path = root.join(load_path);
        let full_path_str = full_path.to_string_lossy();
        match client.load_file(&full_path_str) {
            Ok(classes) => {
                eprintln!(
                    "E2E: Loaded {} (classes: {})",
                    load_path,
                    classes.join(", ")
                );
            }
            Err(e) => {
                failures.push(format!("{file_name}: Failed to load {load_path}: {e}"));
                return (0, failures);
            }
        }
    }

    // Test files expected to fail loading (from @load-error directives)
    for load_error in &test_file.load_error_cases {
        let full_path = root.join(&load_error.path);
        let full_path_str = full_path.to_string_lossy();
        match client.load_file(&full_path_str) {
            Ok(classes) => {
                failures.push(format!(
                    "{file_name}:{}: @load-error `{}` expected error containing `{}`, but load succeeded (classes: {})",
                    load_error.line, load_error.path, load_error.expected_error, classes.join(", ")
                ));
            }
            Err(e) => {
                if e.contains(&load_error.expected_error) {
                    pass_count += 1;
                    eprintln!(
                        "E2E: @load-error {} correctly failed with: {}",
                        load_error.path,
                        e.chars().take(100).collect::<String>()
                    );
                } else {
                    failures.push(format!(
                        "{file_name}:{}: @load-error `{}` expected error containing `{}`, got error `{}`",
                        load_error.line, load_error.path, load_error.expected_error, e
                    ));
                }
            }
        }
    }

    for case in &test_file.cases {
        // Route REPL commands through their proper protocol ops
        let eval_result = if case.expression.starts_with(":help ")
            || case.expression.starts_with(":h ")
        {
            let args = if case.expression.starts_with(":help ") {
                case.expression.strip_prefix(":help ").unwrap().trim()
            } else {
                case.expression.strip_prefix(":h ").unwrap().trim()
            };
            let (class_name, selector) = match args.split_once(' ') {
                Some((cls, sel)) => (cls.trim(), Some(sel.trim())),
                None => (args, None),
            };
            client.get_docs(class_name, selector)
        } else if case.expression == ":clear" {
            client.clear_and_report()
        } else if case.expression == ":bindings" {
            client.get_bindings()
        } else if case.expression == ":actors" {
            client.get_actors()
        } else if case.expression == ":modules" {
            client.get_modules()
        } else if case.expression.starts_with(":load ") || case.expression.starts_with(":l ") {
            let path = case
                .expression
                .strip_prefix(":load ")
                .or_else(|| case.expression.strip_prefix(":l "))
                .unwrap()
                .trim();
            client.load_and_report(path)
        } else if case.expression == ":reload" || case.expression == ":r" {
            client.reload_last()
        } else if case.expression.starts_with(":reload ") || case.expression.starts_with(":r ") {
            let module = case
                .expression
                .strip_prefix(":reload ")
                .or_else(|| case.expression.strip_prefix(":r "))
                .unwrap()
                .trim();
            client.reload_module(module)
        } else if case.expression.starts_with(":inspect ") {
            let arg = case.expression.strip_prefix(":inspect ").unwrap().trim();
            if arg.starts_with('<') {
                // Direct PID string like <0.123.0>
                client.inspect_actor(arg)
            } else {
                // Class name — find first actor of that class
                match client.get_first_actor_pid(arg) {
                    Ok(pid) => client.inspect_actor(&pid),
                    Err(e) => Err(e),
                }
            }
        } else if case.expression.starts_with(":kill ") {
            let arg = case.expression.strip_prefix(":kill ").unwrap().trim();
            if arg.starts_with('<') {
                client.kill_actor(arg)
            } else {
                match client.get_first_actor_pid(arg) {
                    Ok(pid) => client.kill_actor(&pid),
                    Err(e) => Err(e),
                }
            }
        } else if case.expression.starts_with(":unload ") {
            let module = case.expression.strip_prefix(":unload ").unwrap().trim();
            client.unload_module(module)
        } else {
            client.eval(&case.expression)
        };
        match eval_result {
            Ok(result) => {
                // Check if this is a WARNING assertion (BT-407)
                if case.expected.starts_with("WARNING:") {
                    let expected_warning = case.expected.strip_prefix("WARNING:").unwrap().trim();
                    if client
                        .last_warnings
                        .iter()
                        .any(|w| w.contains(expected_warning))
                    {
                        pass_count += 1;
                    } else {
                        failures.push(format!(
                            "{file_name}:{}: `{}` expected warning containing `{}`, got warnings: {:?}",
                            case.line, case.expression, expected_warning, client.last_warnings
                        ));
                    }
                } else if matches_pattern(&case.expected, &result) {
                    pass_count += 1;
                } else {
                    failures.push(format!(
                        "{file_name}:{}: `{}` expected `{}`, got `{}`",
                        case.line, case.expression, case.expected, result
                    ));
                }
            }
            Err(e) => {
                // Check if the error message matches expected (for error tests)
                if case.expected.starts_with("ERROR:") {
                    let expected_error = case.expected.strip_prefix("ERROR:").unwrap().trim();
                    if e.contains(expected_error) {
                        pass_count += 1;
                    } else {
                        failures.push(format!(
                            "{file_name}:{}: `{}` expected error containing `{}`, got error `{}`",
                            case.line, case.expression, expected_error, e
                        ));
                    }
                } else if case.expected == "_" || has_wildcard_underscore(&case.expected) {
                    // Wildcard/pattern means "run but don't check result" - errors are still failures
                    // because we want to know if spawn or other side-effect operations fail
                    failures.push(format!(
                        "{file_name}:{}: `{}` (wildcard) failed with error: {}",
                        case.line, case.expression, e
                    ));
                } else {
                    failures.push(format!(
                        "{file_name}:{}: `{}` failed with error: {}",
                        case.line, case.expression, e
                    ));
                }
            }
        }
    }

    (pass_count, failures)
}

/// Main E2E test entry point.
/// Uses `#[serial(e2e)]` to prevent parallel E2E test runs that compile
/// Beamtalk files and run escript, which can conflict with shared build artifacts.
///
/// Note: Ignored by default due to slow execution (~50s for 316 test cases).
/// BT-885: Class definition + trailing expressions in one eval block.
///
/// When a class definition and method calls are submitted as a single `evaluate` call
/// (as happens with MCP/REPL multi-line submission), the trailing expressions must be
/// evaluated and their result returned — not the class name.
///
/// Run explicitly with: `cargo test --test e2e -- --ignored` or `just test-e2e`
#[test]
#[ignore = "slow test - run with `just test-e2e`"]
#[serial(e2e)]
fn e2e_inline_class_trailing_expressions() {
    let _manager = ProcessManager::start();
    let mut client = ReplClient::connect().expect("Failed to connect to REPL");

    // Class definition followed by an expression in one eval call.
    // Before the fix, this returned the class name ('BT885Counter').
    // After the fix, it should return the result of 'BT885Counter new count' (0).
    let result = client
        .eval("Object subclass: BT885Counter\n  state: count = 0\n  count => self.count\nBT885Counter new count")
        .expect("Eval failed");
    assert_eq!(
        result, "0",
        "Expected trailing expression result (0), got class name or error: {result}"
    );

    // Verify class was also registered (can still use it in subsequent evals).
    let result2 = client
        .eval("BT885Counter new count")
        .expect("Second eval failed");
    assert_eq!(result2, "0", "Class should still be registered: {result2}");
}

/// Run explicitly with: `cargo test --test e2e -- --ignored` or `just test-e2e`
#[test]
#[ignore = "slow test - run with `just test-e2e`"]
#[serial(e2e)]
fn e2e_language_tests() {
    // Check if test cases directory exists
    let cases_dir = test_cases_dir();
    if !cases_dir.exists() {
        eprintln!(
            "E2E test cases directory not found: {}",
            cases_dir.display()
        );
        eprintln!("Skipping E2E tests. Create test cases in tests/e2e/cases/*.bt");
        return;
    }

    // Start or connect to BEAM REPL
    let _manager = ProcessManager::start();

    // Connect to REPL
    let mut client = match ReplClient::connect() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Failed to connect to REPL on port {REPL_PORT}: {e}");
            eprintln!("Make sure the REPL backend is running");
            panic!("Could not connect to REPL");
        }
    };

    // Find all test files (sorted for deterministic execution order)
    let mut test_files: Vec<PathBuf> = fs::read_dir(&cases_dir)
        .expect("Failed to read test cases directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension().is_some_and(|ext| ext == "bt") {
                Some(path)
            } else {
                None
            }
        })
        .collect();
    test_files.sort();

    if test_files.is_empty() {
        eprintln!("No test files found in {}", cases_dir.display());
        eprintln!("Create test cases with .bt extension");
        return;
    }

    eprintln!("E2E: Found {} test file(s)", test_files.len());

    // Run all test files
    let mut total_passed = 0;
    let mut all_failures = Vec::new();

    for test_file in &test_files {
        eprintln!("E2E: Running {}...", test_file.display());
        let (passed, failures) = run_test_file(test_file, &mut client);
        total_passed += passed;
        all_failures.extend(failures);
    }

    // Report results
    let total_tests = total_passed + all_failures.len();
    eprintln!("\nE2E Results: {total_passed}/{total_tests} tests passed");

    if !all_failures.is_empty() {
        eprintln!("\nFailures:");
        for failure in &all_failures {
            eprintln!("  - {failure}");
        }
        panic!(
            "E2E tests failed: {} of {} tests failed",
            all_failures.len(),
            total_tests
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_test_file_basic() {
        let content = r"
3 + 4
// => 7

5 * 2
// => 10
";
        let parsed = parse_test_file(content);
        assert!(parsed.load_files.is_empty());
        assert_eq!(parsed.cases.len(), 2);
        assert_eq!(parsed.cases[0].expression, "3 + 4");
        assert_eq!(parsed.cases[0].expected, "7");
        assert_eq!(parsed.cases[1].expression, "5 * 2");
        assert_eq!(parsed.cases[1].expected, "10");
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_test_file_with_comments() {
        let content = r"
// Test arithmetic operations
3 + 4
// => 7

// Another test
[:x | x + 1] value: 5
// => 6
";
        let parsed = parse_test_file(content);
        assert!(parsed.load_files.is_empty());
        assert_eq!(parsed.cases.len(), 2);
        assert_eq!(parsed.cases[0].expression, "3 + 4");
        assert_eq!(parsed.cases[1].expression, "[:x | x + 1] value: 5");
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_test_file_error_expected() {
        let content = r"
undefined_var
// => ERROR: Undefined variable
";
        let parsed = parse_test_file(content);
        assert!(parsed.load_files.is_empty());
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expected, "ERROR: Undefined variable");
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_test_file_with_load_directive() {
        let content = r"
// @load tests/e2e/fixtures/counter.bt
// @load stdlib/src/stdlib.bt

// Test with loaded classes
Counter spawn
// => <pid>
";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.load_files.len(), 2);
        assert_eq!(parsed.load_files[0], "tests/e2e/fixtures/counter.bt");
        assert_eq!(parsed.load_files[1], "stdlib/src/stdlib.bt");
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expression, "Counter spawn");
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_warns_on_missing_assertion() {
        let content = r"
// Test with proper assertion
3 + 4
// => 7

// This expression has no assertion (should warn)
count := 0

// This expression also has no assertion (should warn)
5 timesRepeat: [count := count + 1]

// This would fail if previous lines actually ran
count
// => 5
";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 2); // Only expressions with assertions
        assert_eq!(parsed.cases[0].expression, "3 + 4");
        assert_eq!(parsed.cases[1].expression, "count");

        // Should have warnings for the two expressions without assertions
        assert_eq!(parsed.warnings.len(), 2);
        assert!(parsed.warnings[0].contains("Line 7"));
        assert!(parsed.warnings[0].contains("count := 0"));
        assert!(parsed.warnings[0].contains("missing // => assertion"));
        assert!(parsed.warnings[1].contains("Line 10"));
        assert!(parsed.warnings[1].contains("5 timesRepeat"));
        assert!(parsed.warnings[1].contains("missing // => assertion"));
    }

    #[test]
    fn test_parse_consecutive_expressions_without_assertions() {
        // Test for bug where consecutive expressions without assertions
        // cause the second expression to be skipped entirely (double increment bug)
        let content = r"
expr1
expr2
expr3
// => result3
";
        let parsed = parse_test_file(content);

        // Should have 1 test case (expr3 with assertion)
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expression, "expr3");
        assert_eq!(parsed.cases[0].expected, "result3");

        // Should have 2 warnings (for expr1 and expr2)
        assert_eq!(
            parsed.warnings.len(),
            2,
            "Expected warnings for expr1 and expr2"
        );
        assert!(parsed.warnings[0].contains("Line 2"));
        assert!(parsed.warnings[0].contains("expr1"));
        assert!(parsed.warnings[1].contains("Line 3"));
        assert!(parsed.warnings[1].contains("expr2"));
    }

    #[test]
    fn test_parse_multiline_expression() {
        // Multi-line expressions: unclosed bracket causes continuation
        let content = r"
[
  :x |
  x * 2
] value: 21
// => 42
";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expression, "[\n:x |\nx * 2\n] value: 21");
        assert_eq!(parsed.cases[0].expected, "42");
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_multiline_map() {
        // Multi-line map literal
        let content = r"
#{
  name => 'Alice',
  age => 30
} at: #name
// => Alice
";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert!(parsed.cases[0].expression.starts_with("#{"));
        assert!(parsed.cases[0].expression.contains("name => 'Alice'"));
        assert_eq!(parsed.cases[0].expected, "Alice");
    }

    #[test]
    fn test_parse_multiline_trailing_operator() {
        // Trailing binary operator causes continuation
        let content = r"
10 +
  20
// => 30
";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expression, "10 +\n20");
        assert_eq!(parsed.cases[0].expected, "30");
    }

    #[test]
    fn test_parse_multiline_trailing_assign() {
        // Trailing := causes continuation
        let content = r"
x :=
  42
// => 42
";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expression, "x :=\n42");
        assert_eq!(parsed.cases[0].expected, "42");
    }

    #[test]
    fn test_parse_multiline_trailing_keyword() {
        // Trailing keyword (without argument) causes continuation
        let content = r#"
#{#a => 1} at: #b put:
  2
// => {"a":1,"b":2}
"#;
        let parsed = parse_test_file(content);
        assert_eq!(parsed.cases.len(), 1);
        assert!(parsed.cases[0].expression.contains("put:\n2"));
        assert_eq!(parsed.cases[0].expected, r#"{"a":1,"b":2}"#);
    }

    #[test]
    fn test_parse_load_error_directive() {
        let content = r"
// @load-error tests/e2e/fixtures/bad_class.bt => cannot assign to field
// @load-error tests/e2e/fixtures/sealed.bt => Cannot subclass sealed class

3 + 4
// => 7
";
        let parsed = parse_test_file(content);
        assert!(parsed.load_files.is_empty());
        assert_eq!(parsed.load_error_cases.len(), 2);
        assert_eq!(
            parsed.load_error_cases[0].path,
            "tests/e2e/fixtures/bad_class.bt"
        );
        assert_eq!(
            parsed.load_error_cases[0].expected_error,
            "cannot assign to field"
        );
        assert_eq!(
            parsed.load_error_cases[1].path,
            "tests/e2e/fixtures/sealed.bt"
        );
        assert_eq!(
            parsed.load_error_cases[1].expected_error,
            "Cannot subclass sealed class"
        );
        assert_eq!(parsed.cases.len(), 1);
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_load_error_not_confused_with_load() {
        let content = r"
// @load tests/e2e/fixtures/counter.bt
// @load-error tests/e2e/fixtures/bad.bt => some error

Counter spawn
// => _
";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.load_files.len(), 1);
        assert_eq!(parsed.load_files[0], "tests/e2e/fixtures/counter.bt");
        assert_eq!(parsed.load_error_cases.len(), 1);
        assert_eq!(parsed.load_error_cases[0].path, "tests/e2e/fixtures/bad.bt");
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn test_parse_load_error_malformed_warns() {
        // Missing => separator
        let content = "// @load-error tests/e2e/fixtures/bad.bt\n";
        let parsed = parse_test_file(content);
        assert!(parsed.load_error_cases.is_empty());
        assert_eq!(parsed.warnings.len(), 1);
        assert!(parsed.warnings[0].contains("Malformed @load-error"));

        // Empty directive
        let content = "// @load-error\n";
        let parsed = parse_test_file(content);
        assert!(parsed.load_error_cases.is_empty());
        assert_eq!(parsed.warnings.len(), 1);
        assert!(parsed.warnings[0].contains("Malformed @load-error"));

        // Empty path with =>
        let content = "// @load-error  => some error\n";
        let parsed = parse_test_file(content);
        assert!(parsed.load_error_cases.is_empty());
        assert_eq!(parsed.warnings.len(), 1);
        assert!(parsed.warnings[0].contains("Malformed @load-error"));

        // Empty expected error
        let content = "// @load-error path.bt =>\n";
        let parsed = parse_test_file(content);
        assert!(parsed.load_error_cases.is_empty());
        assert_eq!(parsed.warnings.len(), 1);
        assert!(parsed.warnings[0].contains("Malformed @load-error"));
    }

    #[test]
    fn test_matches_pattern_bare_wildcard() {
        assert!(matches_pattern("_", "anything"));
        assert!(matches_pattern("_", ""));
        assert!(matches_pattern("_", "#Actor<Counter,0.173.0>"));
    }

    #[test]
    fn test_matches_pattern_exact_match() {
        assert!(matches_pattern("Alice", "Alice"));
        assert!(!matches_pattern("Alice", "Bob"));
        assert!(matches_pattern("42", "42"));
        assert!(!matches_pattern("42", "43"));
    }

    #[test]
    fn test_matches_pattern_literal_underscores() {
        // Underscores between alphanumeric chars are literal, not wildcards
        assert!(matches_pattern(
            "does_not_understand",
            "does_not_understand"
        ));
        assert!(!matches_pattern(
            "does_not_understand",
            "does_XXX_understand"
        ));
        assert!(matches_pattern("runtime_error", "runtime_error"));
        assert!(!matches_pattern("runtime_error", "runtimeXerror"));
        assert!(matches_pattern("a-b_c", "a-b_c"));
        // $beamtalk_class has literal underscore (both sides alphanumeric)
        assert!(matches_pattern(
            r#"{"$beamtalk_class":"Point"}"#,
            r#"{"$beamtalk_class":"Point"}"#
        ));
    }

    #[test]
    fn test_matches_pattern_actor_pid() {
        assert!(matches_pattern(
            "#Actor<Counter,_>",
            "#Actor<Counter,0.173.0>"
        ));
        assert!(matches_pattern(
            "#Actor<Counter,_>",
            "#Actor<Counter,0.999.0>"
        ));
        assert!(!matches_pattern(
            "#Actor<Counter,_>",
            "#Actor<ChatRoom,0.173.0>"
        ));
    }

    #[test]
    fn test_matches_pattern_collection() {
        assert!(matches_pattern(
            r"{\: Set, elements: _}",
            r"{\: Set, elements: [<0.173.0>, <0.174.0>]}"
        ));
        assert!(!matches_pattern(
            r"{\: Set, elements: _}",
            r"{\: List, elements: [1, 2]}"
        ));
    }

    #[test]
    fn test_matches_pattern_multiple_wildcards() {
        assert!(matches_pattern("#Actor<_,_>", "#Actor<Counter,0.173.0>"));
        assert!(!matches_pattern("#Actor<_,_>", "something else"));
    }

    #[test]
    fn test_matches_pattern_leading_wildcard() {
        assert!(matches_pattern("_>", "#Actor<Counter,0.173.0>"));
        assert!(!matches_pattern("_>", "no closing bracket"));
    }

    #[test]
    fn test_matches_pattern_no_false_positives() {
        // Pattern without underscore is exact match
        assert!(!matches_pattern("hello", "hello world"));
        assert!(!matches_pattern("hello world", "hello"));
    }
}
