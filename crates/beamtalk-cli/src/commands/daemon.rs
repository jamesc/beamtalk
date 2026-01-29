// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compiler daemon for incremental compilation and IDE support.
//!
//! The daemon runs as a long-lived process that maintains compilation state
//! for fast incremental builds. It communicates via JSON-RPC 2.0 over a
//! Unix socket (or TCP on Windows).
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────┐     ┌─────────────────────────────────┐
//! │   Clients   │     │         Compiler Daemon         │
//! │  LSP/REPL   │────▶│  ┌─────────────────────────┐   │
//! │    CLI      │     │  │    LanguageService      │   │
//! └─────────────┘     │  │  - Parse cache          │   │
//!       │             │  │  - Incremental queries  │   │
//!   JSON-RPC 2.0      │  └─────────────────────────┘   │
//!       │             └─────────────────────────────────┘
//!       ▼
//!  ~/.beamtalk/daemon.sock
//! ```
//!
//! # Usage
//!
//! ```bash
//! beamtalk daemon start           # Start daemon (daemonizes)
//! beamtalk daemon start --foreground  # Run in foreground
//! beamtalk daemon stop            # Stop the daemon
//! beamtalk daemon status          # Check if running
//! ```

use std::fs;
use std::io::{BufRead, BufReader, Write};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
#[cfg(unix)]
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::PathBuf;
use std::process;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use beamtalk_core::language_service::{LanguageService, SimpleLanguageService};
use camino::Utf8PathBuf;
use clap::Subcommand;
use miette::{IntoDiagnostic, Result, miette};
use serde::{Deserialize, Serialize};
use tracing::{debug, error, info};

/// Daemon subcommand actions.
#[derive(Debug, Clone, Copy, Subcommand)]
pub enum DaemonAction {
    /// Start the compiler daemon
    Start {
        /// Run in foreground (don't daemonize)
        #[arg(long)]
        foreground: bool,
    },

    /// Stop the running compiler daemon
    Stop,

    /// Check if the daemon is running
    Status,
}

/// Directory for daemon state files.
fn beamtalk_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| miette!("Could not determine home directory"))?;
    Ok(home.join(".beamtalk"))
}

/// Path to the Unix socket.
fn socket_path() -> Result<PathBuf> {
    Ok(beamtalk_dir()?.join("daemon.sock"))
}

/// Path to the lockfile containing the daemon PID.
fn lockfile_path() -> Result<PathBuf> {
    Ok(beamtalk_dir()?.join("daemon.lock"))
}

/// Check if daemon is currently running.
///
/// Uses POSIX-compliant `kill(pid, 0)` to check process existence,
/// which works on Linux, macOS, and other Unix systems.
#[cfg(unix)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "PID values are always positive and fit in i32"
)]
fn is_daemon_running() -> Result<Option<u32>> {
    let lockfile = lockfile_path()?;
    if !lockfile.exists() {
        return Ok(None);
    }

    let pid_str = fs::read_to_string(&lockfile).into_diagnostic()?;
    let pid: u32 = pid_str.trim().parse().into_diagnostic()?;

    // Check if process exists using POSIX kill(pid, 0)
    // This is portable across Linux, macOS, and BSD systems
    // SAFETY: kill with signal 0 only checks if process exists, doesn't send a signal
    let exists = unsafe { libc::kill(pid as i32, 0) == 0 };
    if exists {
        Ok(Some(pid))
    } else {
        // Stale lockfile, clean up
        let _ = fs::remove_file(&lockfile);
        let _ = fs::remove_file(socket_path()?);
        Ok(None)
    }
}

/// Check if daemon is currently running (Windows stub).
#[cfg(not(unix))]
fn is_daemon_running() -> Result<Option<u32>> {
    // Windows support not yet implemented
    Ok(None)
}

/// Write current PID to lockfile atomically using `O_EXCL`.
///
/// This prevents race conditions where two processes both pass the
/// `is_daemon_running` check and try to create lockfiles simultaneously.
#[cfg(unix)]
fn write_lockfile_atomic() -> Result<()> {
    use std::io::Write;
    use std::os::unix::fs::OpenOptionsExt;

    let lockfile = lockfile_path()?;

    // Use O_CREAT | O_EXCL to atomically create the file only if it doesn't exist
    let mut file = fs::OpenOptions::new()
        .write(true)
        .create_new(true) // This is O_CREAT | O_EXCL
        .mode(0o644)
        .open(&lockfile)
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::AlreadyExists {
                miette!("Daemon lockfile already exists. Another instance may be starting.")
            } else {
                miette!("Failed to create lockfile: {e}")
            }
        })?;

    write!(file, "{}", process::id()).into_diagnostic()?;
    Ok(())
}

/// Remove lockfile and socket on shutdown.
fn cleanup() -> Result<()> {
    let _ = fs::remove_file(lockfile_path()?);
    let _ = fs::remove_file(socket_path()?);
    Ok(())
}

/// Run the daemon command.
pub fn run(action: DaemonAction) -> Result<()> {
    match action {
        DaemonAction::Start { foreground } => start_daemon(foreground),
        DaemonAction::Stop => stop_daemon(),
        DaemonAction::Status => show_status(),
    }
}

/// Start the compiler daemon.
#[cfg(unix)]
fn start_daemon(foreground: bool) -> Result<()> {
    // Check if already running
    if let Some(pid) = is_daemon_running()? {
        return Err(miette!("Daemon already running (PID {pid})"));
    }

    // Ensure .beamtalk directory exists
    let dir = beamtalk_dir()?;
    fs::create_dir_all(&dir).into_diagnostic()?;

    if foreground {
        // Run in foreground
        init_logging();
        info!("Starting compiler daemon in foreground");
        run_daemon_server()
    } else {
        // Background mode is not yet implemented; avoid misleading behavior.
        // When proper daemonization is implemented, this branch should be updated.
        Err(miette!(
            "Background mode is not yet supported. Please rerun with --foreground."
        ))
    }
}

/// Start the compiler daemon (Windows stub).
#[cfg(not(unix))]
fn start_daemon(_foreground: bool) -> Result<()> {
    Err(miette!(
        "Daemon is not yet supported on this platform. Unix socket support requires Unix."
    ))
}

/// Stop the running daemon.
#[cfg(unix)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "PID values are always positive and small"
)]
fn stop_daemon() -> Result<()> {
    if let Some(pid) = is_daemon_running()? {
        // SAFETY: libc::kill is safe to call with any pid and signal number.
        // If the pid doesn't exist, it returns an error which we ignore.
        unsafe {
            libc::kill(pid as i32, libc::SIGTERM);
        }
        println!("Sent stop signal to daemon (PID {pid})");

        // Wait briefly and check if stopped
        std::thread::sleep(std::time::Duration::from_millis(500));
        if is_daemon_running()?.is_none() {
            println!("Daemon stopped successfully");
        } else {
            println!("Daemon may still be shutting down");
        }
    } else {
        println!("Daemon is not running");
    }
    Ok(())
}

/// Stop the running daemon (Windows stub).
#[cfg(not(unix))]
fn stop_daemon() -> Result<()> {
    Err(miette!("Daemon is not yet supported on this platform."))
}

/// Show daemon status.
fn show_status() -> Result<()> {
    match is_daemon_running()? {
        Some(pid) => {
            println!("Daemon is running (PID {pid})");
            println!("Socket: {}", socket_path()?.display());
        }
        None => {
            println!("Daemon is not running");
        }
    }
    Ok(())
}

/// Initialize logging for the daemon.
fn init_logging() {
    use tracing_subscriber::{EnvFilter, fmt, prelude::*};

    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(
            EnvFilter::from_default_env().add_directive(
                "beamtalk=debug"
                    .parse()
                    .expect("Failed to parse tracing directive"),
            ),
        )
        .init();
}

/// Run the main daemon server loop.
///
/// # Limitations
///
/// - **Single connection**: The daemon handles one connection at a time. Subsequent
///   clients will queue at the socket level until the current client disconnects.
///   For concurrent LSP and REPL support, consider spawning threads per connection
///   or using an async runtime.
/// - **Shared state**: The `SimpleLanguageService` is mutably borrowed for the
///   duration of each connection. Concurrent access would require `Arc<Mutex<...>>`.
#[cfg(unix)]
fn run_daemon_server() -> Result<()> {
    // Write lockfile atomically using O_EXCL to prevent race conditions
    write_lockfile_atomic()?;

    // Set up signal handling for graceful shutdown
    let running = Arc::new(AtomicBool::new(true));
    let r = Arc::clone(&running);

    ctrlc::set_handler(move || {
        info!("Received shutdown signal");
        r.store(false, Ordering::SeqCst);
    })
    .into_diagnostic()?;

    // Create Unix socket
    let socket = socket_path()?;
    if socket.exists() {
        fs::remove_file(&socket).into_diagnostic()?;
    }

    let listener = UnixListener::bind(&socket).into_diagnostic()?;

    // Set restrictive permissions (owner only) on the socket
    // This prevents other users on the system from connecting
    fs::set_permissions(&socket, fs::Permissions::from_mode(0o600)).into_diagnostic()?;

    listener.set_nonblocking(true).into_diagnostic()?;

    info!("Daemon listening on {}", socket.display());

    // Create language service
    let mut service = SimpleLanguageService::new();

    // Main loop
    while running.load(Ordering::SeqCst) {
        match listener.accept() {
            Ok((stream, _addr)) => {
                debug!("Accepted connection");
                if let Err(e) = handle_connection(stream, &mut service, &running) {
                    error!("Error handling connection: {e}");
                }
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // No connection, sleep briefly to avoid busy-waiting
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
            Err(e) => {
                error!("Error accepting connection: {e}");
            }
        }
    }

    info!("Daemon shutting down");
    cleanup()?;
    Ok(())
}

/// Handle a single client connection.
#[cfg(unix)]
#[expect(
    clippy::needless_pass_by_value,
    reason = "UnixStream needs to be passed by value for the lifetime of the connection"
)]
fn handle_connection(
    stream: UnixStream,
    service: &mut SimpleLanguageService,
    running: &Arc<AtomicBool>,
) -> Result<()> {
    stream.set_nonblocking(false).into_diagnostic()?;

    let mut reader = BufReader::new(&stream);
    let mut writer = &stream;

    loop {
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => {
                // EOF - client disconnected
                debug!("Client disconnected");
                break;
            }
            Ok(_) => {
                let response = handle_request(&line, service, running);
                writeln!(writer, "{response}").into_diagnostic()?;
                writer.flush().into_diagnostic()?;
            }
            Err(e) => {
                error!("Error reading from client: {e}");
                break;
            }
        }
    }

    Ok(())
}

// ============================================================================
// JSON-RPC 2.0 Protocol
// ============================================================================

/// JSON-RPC 2.0 request.
#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    id: Option<serde_json::Value>,
    method: String,
    #[serde(default)]
    params: serde_json::Value,
}

/// JSON-RPC 2.0 response.
#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

/// JSON-RPC 2.0 error.
#[derive(Debug, Serialize)]
struct JsonRpcError {
    code: i32,
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<serde_json::Value>,
}

// Standard JSON-RPC error codes
const PARSE_ERROR: i32 = -32700;
const INVALID_REQUEST: i32 = -32600;
const METHOD_NOT_FOUND: i32 = -32601;
const INVALID_PARAMS: i32 = -32602;
const INTERNAL_ERROR: i32 = -32603;

impl JsonRpcResponse {
    fn success(id: Option<serde_json::Value>, result: serde_json::Value) -> Self {
        Self {
            jsonrpc: "2.0",
            id,
            result: Some(result),
            error: None,
        }
    }

    fn error(id: Option<serde_json::Value>, code: i32, message: impl Into<String>) -> Self {
        Self {
            jsonrpc: "2.0",
            id,
            result: None,
            error: Some(JsonRpcError {
                code,
                message: message.into(),
                data: None,
            }),
        }
    }
}

/// Handle a JSON-RPC request and return the response as a JSON string.
fn handle_request(
    request_str: &str,
    service: &mut SimpleLanguageService,
    running: &Arc<AtomicBool>,
) -> String {
    let request: JsonRpcRequest = match serde_json::from_str(request_str) {
        Ok(r) => r,
        Err(e) => {
            return serde_json::to_string(&JsonRpcResponse::error(
                None,
                PARSE_ERROR,
                format!("Parse error: {e}"),
            ))
            .unwrap_or_default();
        }
    };

    if request.jsonrpc != "2.0" {
        return serde_json::to_string(&JsonRpcResponse::error(
            request.id,
            INVALID_REQUEST,
            "Invalid JSON-RPC version",
        ))
        .unwrap_or_default();
    }

    let response = dispatch_method(
        &request.method,
        request.params,
        request.id.clone(),
        service,
        running,
    );

    serde_json::to_string(&response).unwrap_or_default()
}

/// Dispatch a method call to the appropriate handler.
fn dispatch_method(
    method: &str,
    params: serde_json::Value,
    id: Option<serde_json::Value>,
    service: &mut SimpleLanguageService,
    running: &Arc<AtomicBool>,
) -> JsonRpcResponse {
    debug!("Dispatching method: {method}");

    match method {
        "compile" => handle_compile(params, id, service),
        "compile_expression" => handle_compile_expression(params, id),
        "diagnostics" => handle_diagnostics(params, id, service),
        "shutdown" => {
            info!("Received shutdown request, stopping daemon");
            running.store(false, Ordering::SeqCst);
            JsonRpcResponse::success(id, serde_json::json!(null))
        }
        "ping" => JsonRpcResponse::success(id, serde_json::json!("pong")),
        _ => JsonRpcResponse::error(id, METHOD_NOT_FOUND, format!("Method not found: {method}")),
    }
}

// ============================================================================
// Method Handlers
// ============================================================================

/// Parameters for the compile method.
#[derive(Debug, Deserialize)]
struct CompileParams {
    path: String,
    #[serde(default)]
    source: Option<String>,
}

/// Result of the compile method.
#[derive(Debug, Serialize)]
struct CompileResult {
    success: bool,
    beam_path: Option<String>,
    core_erlang: Option<String>,
    diagnostics: Vec<DiagnosticInfo>,
    classes: Vec<String>,
}

/// Diagnostic information for responses.
#[derive(Debug, Serialize)]
struct DiagnosticInfo {
    message: String,
    severity: String,
    start: u32,
    end: u32,
}

/// Handle the compile method.
fn handle_compile(
    params: serde_json::Value,
    id: Option<serde_json::Value>,
    service: &mut SimpleLanguageService,
) -> JsonRpcResponse {
    let params: CompileParams = match serde_json::from_value(params) {
        Ok(p) => p,
        Err(e) => {
            return JsonRpcResponse::error(id, INVALID_PARAMS, format!("Invalid params: {e}"));
        }
    };

    let file_path = Utf8PathBuf::from(&params.path);

    // Get source either from params or read from file
    let source = match params.source {
        Some(s) => s,
        None => match std::fs::read_to_string(&params.path) {
            Ok(s) => s,
            Err(e) => {
                return JsonRpcResponse::error(
                    id,
                    INTERNAL_ERROR,
                    format!("Failed to read file: {e}"),
                );
            }
        },
    };

    // Update the language service
    service.update_file(file_path.clone(), source.clone());

    // Get diagnostics
    let diagnostics: Vec<DiagnosticInfo> = service
        .diagnostics(&file_path)
        .into_iter()
        .map(|d| DiagnosticInfo {
            message: d.message.to_string(),
            severity: "error".to_string(),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();

    let has_errors = !diagnostics.is_empty();

    // Generate Core Erlang if no errors
    let (core_erlang, class_names) = if has_errors {
        (None, vec![])
    } else {
        // Parse and generate
        use beamtalk_core::parse::{lex_with_eof, parse};
        let tokens = lex_with_eof(&source);
        let (module, _) = parse(tokens);

        // Derive module name from file path
        let module_name = file_path.file_stem().unwrap_or("module").to_string();

        let core = beamtalk_core::erlang::generate_with_name(&module, &module_name).ok();
        let classes = extract_class_names(&module);
        (core, classes)
    };

    let result = CompileResult {
        success: !has_errors,
        beam_path: None, // TODO: Invoke erlc
        core_erlang,
        diagnostics,
        classes: class_names,
    };

    match serde_json::to_value(result) {
        Ok(value) => JsonRpcResponse::success(id, value),
        Err(e) => {
            error!("Failed to serialize compile result: {e}");
            JsonRpcResponse::error(
                id,
                INTERNAL_ERROR,
                format!("Failed to serialize result: {e}"),
            )
        }
    }
}

/// Parameters for the diagnostics method.
#[derive(Debug, Deserialize)]
struct DiagnosticsParams {
    path: String,
    #[serde(default)]
    source: Option<String>,
}

/// Handle the diagnostics method.
fn handle_diagnostics(
    params: serde_json::Value,
    id: Option<serde_json::Value>,
    service: &mut SimpleLanguageService,
) -> JsonRpcResponse {
    let params: DiagnosticsParams = match serde_json::from_value(params) {
        Ok(p) => p,
        Err(e) => {
            return JsonRpcResponse::error(id, INVALID_PARAMS, format!("Invalid params: {e}"));
        }
    };

    let file_path = Utf8PathBuf::from(&params.path);

    // Get source either from params or read from file
    let source = match params.source {
        Some(s) => s,
        None => match std::fs::read_to_string(&params.path) {
            Ok(s) => s,
            Err(e) => {
                return JsonRpcResponse::error(
                    id,
                    INTERNAL_ERROR,
                    format!("Failed to read file: {e}"),
                );
            }
        },
    };

    // Update the language service
    service.update_file(file_path.clone(), source);

    // Get diagnostics
    let diagnostics: Vec<DiagnosticInfo> = service
        .diagnostics(&file_path)
        .into_iter()
        .map(|d| DiagnosticInfo {
            message: d.message.to_string(),
            severity: "error".to_string(),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();

    match serde_json::to_value(diagnostics) {
        Ok(value) => JsonRpcResponse::success(id, value),
        Err(e) => {
            error!("Failed to serialize diagnostics: {e}");
            JsonRpcResponse::error(
                id,
                INTERNAL_ERROR,
                format!("Failed to serialize diagnostics: {e}"),
            )
        }
    }
}

/// Parameters for the `compile_expression` method.
#[derive(Debug, Deserialize)]
struct CompileExpressionParams {
    /// The expression source code.
    source: String,
    /// Unique module name for this evaluation.
    module_name: String,
}

/// Result of the `compile_expression` method.
#[derive(Debug, Serialize)]
struct CompileExpressionResult {
    success: bool,
    core_erlang: Option<String>,
    diagnostics: Vec<DiagnosticInfo>,
}

/// Handle the `compile_expression` method for REPL evaluation.
///
/// This parses a single expression and generates Core Erlang code
/// that can be compiled and executed by the Erlang runtime.
fn handle_compile_expression(
    params: serde_json::Value,
    id: Option<serde_json::Value>,
) -> JsonRpcResponse {
    let params: CompileExpressionParams = match serde_json::from_value(params) {
        Ok(p) => p,
        Err(e) => {
            return JsonRpcResponse::error(id, INVALID_PARAMS, format!("Invalid params: {e}"));
        }
    };

    // Parse the expression as a module (it will contain one expression)
    let tokens = beamtalk_core::parse::lex_with_eof(&params.source);
    let (module, parse_diagnostics) = beamtalk_core::parse::parse(tokens);

    // Convert diagnostics
    let diagnostics: Vec<DiagnosticInfo> = parse_diagnostics
        .iter()
        .map(|d| DiagnosticInfo {
            message: d.message.to_string(),
            severity: match d.severity {
                beamtalk_core::parse::Severity::Error => "error".to_string(),
                beamtalk_core::parse::Severity::Warning => "warning".to_string(),
            },
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();

    let has_errors = diagnostics.iter().any(|d| d.severity == "error");

    // Generate Core Erlang for the expression
    let core_erlang = if has_errors || module.expressions.is_empty() {
        None
    } else {
        // Get the first (and likely only) expression
        let expression = &module.expressions[0];

        // Generate REPL module for this expression
        match beamtalk_core::erlang::generate_repl_expression(expression, &params.module_name) {
            Ok(code) => Some(code),
            Err(e) => {
                error!("Code generation failed: {e}");
                return JsonRpcResponse::error(
                    id,
                    INTERNAL_ERROR,
                    format!("Code generation failed: {e}"),
                );
            }
        }
    };

    let result = CompileExpressionResult {
        success: !has_errors && core_erlang.is_some(),
        core_erlang,
        diagnostics,
    };

    match serde_json::to_value(result) {
        Ok(value) => JsonRpcResponse::success(id, value),
        Err(e) => {
            error!("Failed to serialize result: {e}");
            JsonRpcResponse::error(
                id,
                INTERNAL_ERROR,
                format!("Failed to serialize result: {e}"),
            )
        }
    }
}

/// Extract class names from a parsed module.
/// Returns empty vec for now - will be populated when class definitions are added to AST.
fn extract_class_names(_module: &beamtalk_core::ast::Module) -> Vec<String> {
    // TODO: When class definitions are added to the AST, extract them here
    // For now, return empty vec
    vec![]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_running() -> Arc<AtomicBool> {
        Arc::new(AtomicBool::new(true))
    }

    #[test]
    fn test_ping_method() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"ping"}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("pong"));
    }

    #[test]
    fn test_method_not_found() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"unknown"}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("Method not found"));
    }

    #[test]
    fn test_compile_with_source() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"compile","params":{"path":"test.bt","source":"x := 42"}}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("success"));
        assert!(response.contains("true"));
    }

    #[test]
    fn test_compile_with_errors() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"compile","params":{"path":"test.bt","source":"x := :="}}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("success"));
        assert!(response.contains("false")); // Should fail
        assert!(response.contains("diagnostics"));
    }

    #[test]
    fn test_diagnostics_method() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"diagnostics","params":{"path":"test.bt","source":"x := 42"}}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("[]")); // No diagnostics
    }

    #[test]
    fn test_invalid_json() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = handle_request("not json", &mut service, &running);
        assert!(response.contains("Parse error"));
    }

    #[test]
    fn test_invalid_jsonrpc_version() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = handle_request(
            r#"{"jsonrpc":"1.0","id":1,"method":"ping"}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("Invalid JSON-RPC version"));
    }

    #[test]
    fn test_shutdown_method() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        assert!(running.load(Ordering::SeqCst)); // Running initially true
        let response = handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"shutdown"}"#,
            &mut service,
            &running,
        );
        // Shutdown should return a success response with null result
        assert!(response.contains(r#""result":null"#));
        assert!(!response.contains("error"));
        // Shutdown should set running to false
        assert!(!running.load(Ordering::SeqCst));
    }
}
