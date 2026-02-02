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
//! ```bash
//! # Run just E2E tests
//! cargo test --test e2e
//!
//! # Run with verbose output
//! cargo test --test e2e -- --nocapture
//! ```

use serial_test::serial;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::net::TcpStream;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::Duration;

/// Default port for the REPL TCP server.
const REPL_PORT: u16 = 9000;

/// Timeout for REPL operations.
const TIMEOUT_SECS: u64 = 30;

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

/// Find the beamtalk binary (either debug or release).
fn beamtalk_binary() -> PathBuf {
    let root = workspace_root();

    // Try llvm-cov target first (for coverage runs), then debug, then release
    let llvm_cov_debug_path = root.join("target/llvm-cov-target/debug/beamtalk");
    if llvm_cov_debug_path.exists() {
        return llvm_cov_debug_path;
    }

    let debug_path = root.join("target/debug/beamtalk");
    if debug_path.exists() {
        return debug_path;
    }

    let release_path = root.join("target/release/beamtalk");
    if release_path.exists() {
        return release_path;
    }

    panic!(
        "beamtalk binary not found. Run `cargo build` first.\n\
         Checked:\n  - {}\n  - {}\n  - {}",
        llvm_cov_debug_path.display(),
        debug_path.display(),
        release_path.display()
    );
}

/// Find the runtime directory.
fn runtime_dir() -> PathBuf {
    workspace_root().join("runtime")
}

/// Find the E2E test cases directory.
fn test_cases_dir() -> PathBuf {
    workspace_root().join("tests/e2e/cases")
}

/// Get the daemon socket path.
///
/// Respects `BEAMTALK_DAEMON_SOCKET` environment variable for worktree isolation,
/// falling back to the default `~/.beamtalk/daemon.sock`.
fn daemon_socket_path() -> PathBuf {
    if let Ok(socket_path) = env::var("BEAMTALK_DAEMON_SOCKET") {
        if !socket_path.is_empty() {
            return PathBuf::from(socket_path);
        }
    }
    dirs::home_dir()
        .map(|h| h.join(".beamtalk/daemon.sock"))
        .unwrap_or_default()
}

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

/// Parsed test file with metadata.
#[derive(Debug)]
struct ParsedTestFile {
    /// Files to load before running tests (from `// @load` directives).
    load_files: Vec<String>,
    /// Test cases to run.
    cases: Vec<TestCase>,
}

/// Parse test cases from a `.bt` file.
///
/// Test format:
/// ```text
/// // @load path/to/file.bt
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
/// - `// => _` - Wildcard: run expression but don't check result (useful for spawn, side effects)
fn parse_test_file(content: &str) -> ParsedTestFile {
    let mut cases = Vec::new();
    let mut load_files = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

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

        // This should be an expression
        let expression = line.to_string();
        let expr_line = i + 1;

        // Look for the expected result on the next line
        i += 1;
        if i < lines.len() {
            let next_line = lines[i].trim();
            if let Some(expected) = next_line.strip_prefix("// =>") {
                cases.push(TestCase {
                    expression,
                    expected: expected.trim().to_string(),
                    line: expr_line,
                });
            }
        }
        i += 1;
    }

    ParsedTestFile { load_files, cases }
}

/// Manages the daemon and BEAM processes for tests.
struct DaemonManager {
    daemon_process: Option<Child>,
    beam_process: Option<Child>,
}

impl DaemonManager {
    /// Start the daemon and BEAM REPL backend if not already running.
    ///
    /// Note: If the test detects an existing REPL on the port, it will use that
    /// and not start its own processes. This is useful for development but means
    /// the test won't manage the lifecycle. If the external REPL fails or stops
    /// mid-test, errors may be confusing. For CI, always start with a clean state.
    fn start() -> Self {
        // Check if REPL is already running by trying to connect
        if TcpStream::connect(format!("127.0.0.1:{REPL_PORT}")).is_ok() {
            eprintln!("E2E: REPL already running on port {REPL_PORT}");
            return Self {
                daemon_process: None,
                beam_process: None,
            };
        }

        // Start the compiler daemon first
        eprintln!("E2E: Starting compiler daemon...");
        let binary = beamtalk_binary();

        // Check if debug output is requested via environment variable
        let debug_output = env::var("E2E_DEBUG").is_ok();
        let (stdout_cfg, stderr_cfg) = if debug_output {
            (Stdio::inherit(), Stdio::inherit())
        } else {
            (Stdio::null(), Stdio::null())
        };

        let daemon_child = Command::new(&binary)
            .args(["daemon", "start", "--foreground"])
            .stdout(stdout_cfg)
            .stderr(stderr_cfg)
            .spawn()
            .expect("Failed to start daemon");

        // Poll for daemon socket instead of fixed sleep
        let daemon_socket = daemon_socket_path();
        let mut daemon_retries = 20;
        while daemon_retries > 0 && !daemon_socket.exists() {
            std::thread::sleep(Duration::from_millis(100));
            daemon_retries -= 1;
        }

        // Start the BEAM node with REPL backend
        eprintln!("E2E: Starting BEAM REPL backend...");
        let runtime = runtime_dir();
        let ebin_dir = runtime.join("_build/default/lib/beamtalk_runtime/ebin");
        let jsx_dir = runtime.join("_build/default/lib/jsx/ebin");

        // Build runtime if needed
        if !ebin_dir.exists() {
            eprintln!("E2E: Building runtime...");
            let status = Command::new("rebar3")
                .arg("compile")
                .current_dir(&runtime)
                .status()
                .expect("Failed to run rebar3 compile");

            assert!(status.success(), "Failed to build runtime");
        }

        // Reuse debug output settings for BEAM
        let (stdout_cfg, stderr_cfg) = if debug_output {
            (Stdio::inherit(), Stdio::inherit())
        } else {
            (Stdio::null(), Stdio::null())
        };

        let beam_child = Command::new("erl")
            .args([
                "-noshell",
                "-pa",
                ebin_dir.to_str().unwrap_or(""),
                "-pa",
                jsx_dir.to_str().unwrap_or(""),
                "-eval",
                &format!(
                    "{{ok, _}} = beamtalk_repl:start_link({REPL_PORT}), receive stop -> ok end."
                ),
            ])
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
                    daemon_process: Some(daemon_child),
                    beam_process: Some(beam_child),
                };
            }
            retries -= 1;
        }

        eprintln!("E2E: Warning - REPL may not be fully started. Port {REPL_PORT} not responding.");

        Self {
            daemon_process: Some(daemon_child),
            beam_process: Some(beam_child),
        }
    }

    /// Stop the daemon and BEAM if we started them.
    ///
    /// Uses SIGTERM first for graceful shutdown, then SIGKILL if needed.
    #[cfg(unix)]
    #[expect(
        clippy::cast_possible_wrap,
        reason = "PIDs are always positive and fit in i32 on Unix"
    )]
    fn stop(&mut self) {
        if let Some(ref mut child) = self.beam_process {
            eprintln!("E2E: Stopping BEAM...");
            // Try graceful shutdown first (SIGTERM)
            // SAFETY: libc::kill with SIGTERM is safe to call on any pid.
            // If pid doesn't exist, it returns an error which we ignore.
            unsafe {
                libc::kill(child.id() as i32, libc::SIGTERM);
            }
            // Give it a moment to shutdown gracefully
            std::thread::sleep(Duration::from_millis(200));
            // Force kill if still running
            let _ = child.kill();
            let _ = child.wait();
        }

        if let Some(ref mut child) = self.daemon_process {
            eprintln!("E2E: Stopping daemon...");
            // Try graceful shutdown first (SIGTERM)
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

    #[cfg(not(unix))]
    fn stop(&mut self) {
        if let Some(ref mut child) = self.beam_process {
            eprintln!("E2E: Stopping BEAM...");
            let _ = child.kill();
            let _ = child.wait();
        }

        if let Some(ref mut child) = self.daemon_process {
            eprintln!("E2E: Stopping daemon...");
            let _ = child.kill();
            let _ = child.wait();
        }
    }
}

impl Drop for DaemonManager {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Client for communicating with the REPL.
struct ReplClient {
    stream: TcpStream,
    reader: BufReader<TcpStream>,
}

impl ReplClient {
    /// Connect to the REPL.
    fn connect() -> Result<Self, std::io::Error> {
        let stream = TcpStream::connect(format!("127.0.0.1:{REPL_PORT}"))?;
        stream.set_read_timeout(Some(Duration::from_secs(TIMEOUT_SECS)))?;
        stream.set_write_timeout(Some(Duration::from_secs(TIMEOUT_SECS)))?;

        let reader = BufReader::new(stream.try_clone()?);

        Ok(Self { stream, reader })
    }

    /// Evaluate an expression and return the result.
    fn eval(&mut self, expression: &str) -> Result<String, String> {
        // Send JSON request
        let request = serde_json::json!({
            "type": "eval",
            "expression": expression
        });

        writeln!(self.stream, "{request}").map_err(|e| format!("Failed to send request: {e}"))?;

        self.stream
            .flush()
            .map_err(|e| format!("Failed to flush: {e}"))?;

        // Read response
        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .map_err(|e| format!("Failed to read response: {e}"))?;

        // Parse JSON response
        let response: serde_json::Value = serde_json::from_str(&response_line)
            .map_err(|e| format!("Failed to parse response: {e}"))?;

        match response.get("type").and_then(|t| t.as_str()) {
            Some("result") => {
                let value = response.get("value").map_or_else(
                    || "null".to_string(),
                    |v| {
                        // Format the value appropriately
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
        let request = serde_json::json!({
            "type": "clear"
        });

        writeln!(self.stream, "{request}")
            .map_err(|e| format!("Failed to send clear request: {e}"))?;

        self.stream
            .flush()
            .map_err(|e| format!("Failed to flush: {e}"))?;

        // Read and discard response
        let mut response = String::new();
        self.reader
            .read_line(&mut response)
            .map_err(|e| format!("Failed to read clear response: {e}"))?;

        Ok(())
    }

    /// Load a Beamtalk file (for actor/class definitions).
    ///
    /// This compiles the file and loads its classes into the REPL session,
    /// making them available for spawning and messaging.
    fn load_file(&mut self, path: &str) -> Result<Vec<String>, String> {
        let request = serde_json::json!({
            "type": "load",
            "path": path
        });

        writeln!(self.stream, "{request}")
            .map_err(|e| format!("Failed to send load request: {e}"))?;

        self.stream
            .flush()
            .map_err(|e| format!("Failed to flush: {e}"))?;

        // Read response
        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .map_err(|e| format!("Failed to read load response: {e}"))?;

        // Parse JSON response
        let response: serde_json::Value = serde_json::from_str(&response_line)
            .map_err(|e| format!("Failed to parse load response: {e}"))?;

        match response.get("type").and_then(|t| t.as_str()) {
            Some("loaded") => {
                // Extract loaded class names
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
fn run_test_file(path: &PathBuf, client: &mut ReplClient) -> (usize, Vec<String>) {
    let content = fs::read_to_string(path).expect("Failed to read test file");
    let test_file = parse_test_file(&content);

    let file_name = path.file_name().unwrap().to_string_lossy();
    let mut failures = Vec::new();
    let mut pass_count = 0;

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

    for case in &test_file.cases {
        match client.eval(&case.expression) {
            Ok(result) => {
                // Wildcard "_" means run but don't check result
                if case.expected == "_" || result == case.expected {
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
                } else if case.expected == "_" {
                    // Wildcard means "run but don't check result" - errors are still failures
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
#[test]
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

    // Start or connect to daemon
    let _daemon = DaemonManager::start();

    // Connect to REPL
    let mut client = match ReplClient::connect() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Failed to connect to REPL on port {REPL_PORT}: {e}");
            eprintln!("Make sure the daemon is running: beamtalk daemon start --foreground");
            panic!("Could not connect to REPL");
        }
    };

    // Find all test files
    let test_files: Vec<PathBuf> = fs::read_dir(&cases_dir)
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
    }

    #[test]
    fn test_parse_test_file_with_load_directive() {
        let content = r"
// @load tests/e2e/fixtures/counter.bt
// @load lib/stdlib.bt

// Test with loaded classes
Counter spawn
// => <pid>
";
        let parsed = parse_test_file(content);
        assert_eq!(parsed.load_files.len(), 2);
        assert_eq!(parsed.load_files[0], "tests/e2e/fixtures/counter.bt");
        assert_eq!(parsed.load_files[1], "lib/stdlib.bt");
        assert_eq!(parsed.cases.len(), 1);
        assert_eq!(parsed.cases[0].expression, "Counter spawn");
    }
}
