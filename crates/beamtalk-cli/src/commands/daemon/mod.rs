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
//!
//! # JSON-RPC Error Codes
//!
//! The daemon uses standard JSON-RPC 2.0 error codes plus custom codes for
//! Beamtalk-specific errors. Custom codes are in the -32000 to -32099 range,
//! reserved for implementation-defined server errors per the JSON-RPC 2.0 spec.
//!
//! ## Standard Codes
//!
//! | Code | Name | Description |
//! |------|------|-------------|
//! | -32700 | Parse error | Invalid JSON |
//! | -32600 | Invalid request | Invalid JSON-RPC request |
//! | -32601 | Method not found | Unknown method |
//! | -32602 | Invalid params | Invalid method parameters |
//! | -32603 | Internal error | Unexpected server error |
//!
//! ## Custom Codes
//!
//! | Code | Name | Description |
//! |------|------|-------------|
//! | -32001 | File read error | Cannot read source file from disk |

use clap::Subcommand;
use miette::Result;

mod cleanup;
mod lifecycle;
mod protocol;
mod transport;

/// Daemon subcommand actions.
#[derive(Debug, Clone, Subcommand)]
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
    
    /// List all daemon sessions
    List,
    
    /// Clean up orphaned sessions
    Clean {
        /// Clean all sessions (stop all daemons)
        #[arg(long)]
        all: bool,
    },
}

/// Run the daemon command.
pub fn run(action: DaemonAction) -> Result<()> {
    match action {
        DaemonAction::Start { foreground } => lifecycle::start_daemon(foreground),
        DaemonAction::Stop => lifecycle::stop_daemon(),
        DaemonAction::Status => lifecycle::show_status(),
        DaemonAction::List => list_sessions(),
        DaemonAction::Clean { all } => clean_sessions(all),
    }
}

/// List all daemon sessions with their status.
fn list_sessions() -> Result<()> {
    let sessions = cleanup::list_sessions()?;
    
    if sessions.is_empty() {
        println!("No daemon sessions found");
        return Ok(());
    }
    
    println!("Active daemon sessions:");
    println!();
    
    for session in sessions {
        let status = if session.is_alive { "ALIVE" } else { "DEAD " };
        let pid_str = session.pid.map_or("???".to_string(), |p| p.to_string());
        println!(
            "  {} {} (PID: {}, age: {}d)",
            status, session.name, pid_str, session.age_days
        );
    }
    
    Ok(())
}

/// Clean up sessions.
fn clean_sessions(all: bool) -> Result<()> {
    if all {
        println!("Stopping all daemons and cleaning all sessions...");
        let cleaned = cleanup::cleanup_all_sessions()?;
        println!("✓ Cleaned {} session(s)", cleaned);
    } else {
        println!("Cleaning orphaned sessions...");
        let cleaned = cleanup::cleanup_orphaned_sessions()?;
        let sessions = cleanup::list_sessions()?;
        let kept = sessions.len();
        println!("✓ Cleaned {} session(s), kept {} active", cleaned, kept);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::language_service::SimpleLanguageService;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, Ordering};

    fn make_running() -> Arc<AtomicBool> {
        Arc::new(AtomicBool::new(true))
    }

    #[test]
    fn test_ping_method() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = protocol::handle_request(
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
        let response = protocol::handle_request(
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
        let response = protocol::handle_request(
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
        let response = protocol::handle_request(
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
        let response = protocol::handle_request(
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
        let response = protocol::handle_request("not json", &mut service, &running);
        assert!(response.contains("Parse error"));
    }

    #[test]
    fn test_invalid_jsonrpc_version() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = protocol::handle_request(
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
        let response = protocol::handle_request(
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

    // Path validation tests
    #[test]
    fn test_compile_with_empty_path() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = protocol::handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"compile","params":{"path":"","source":"x := 42"}}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("error"));
        assert!(response.contains("Path cannot be empty or whitespace"));
        assert!(response.contains(&protocol::INVALID_PARAMS.to_string()));
    }

    #[test]
    fn test_compile_with_root_path() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = protocol::handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"compile","params":{"path":"/","source":"x := 42"}}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("error"));
        assert!(response.contains("Path cannot be root directory"));
        assert!(response.contains(&protocol::INVALID_PARAMS.to_string()));
    }

    #[test]
    fn test_diagnostics_with_empty_path() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = protocol::handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"diagnostics","params":{"path":"","source":"x := 42"}}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("error"));
        assert!(response.contains("Path cannot be empty or whitespace"));
        assert!(response.contains(&protocol::INVALID_PARAMS.to_string()));
    }

    #[test]
    fn test_diagnostics_with_root_path() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();
        let response = protocol::handle_request(
            r#"{"jsonrpc":"2.0","id":1,"method":"diagnostics","params":{"path":"/","source":"x := 42"}}"#,
            &mut service,
            &running,
        );
        assert!(response.contains("error"));
        assert!(response.contains("Path cannot be root directory"));
        assert!(response.contains(&protocol::INVALID_PARAMS.to_string()));
    }

    #[test]
    fn test_compile_file_read_error_uses_custom_code() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();

        // Use tempdir to construct a guaranteed-missing path (hermetic test)
        let tempdir = tempfile::tempdir().expect("failed to create tempdir");
        let missing_path = tempdir.path().join("missing.bt");
        let request = format!(
            r#"{{"jsonrpc":"2.0","id":1,"method":"compile","params":{{"path":"{}"}}}}"#,
            missing_path.display()
        );

        let response = protocol::handle_request(&request, &mut service, &running);

        // Parse JSON and assert on structured error code
        let parsed: serde_json::Value =
            serde_json::from_str(&response).expect("response should be valid JSON");
        let error = parsed.get("error").expect("response should have error");
        assert_eq!(
            error.get("code").and_then(serde_json::Value::as_i64),
            Some(i64::from(protocol::FILE_READ_ERROR)),
            "error code should be FILE_READ_ERROR"
        );
        assert!(
            error
                .get("message")
                .and_then(serde_json::Value::as_str)
                .is_some_and(|m| m.contains("Failed to read file")),
            "error message should mention file read failure"
        );
    }

    #[test]
    fn test_diagnostics_file_read_error_uses_custom_code() {
        let mut service = SimpleLanguageService::new();
        let running = make_running();

        // Use tempdir to construct a guaranteed-missing path (hermetic test)
        let tempdir = tempfile::tempdir().expect("failed to create tempdir");
        let missing_path = tempdir.path().join("missing.bt");
        let request = format!(
            r#"{{"jsonrpc":"2.0","id":1,"method":"diagnostics","params":{{"path":"{}"}}}}"#,
            missing_path.display()
        );

        let response = protocol::handle_request(&request, &mut service, &running);

        // Parse JSON and assert on structured error code
        let parsed: serde_json::Value =
            serde_json::from_str(&response).expect("response should be valid JSON");
        let error = parsed.get("error").expect("response should have error");
        assert_eq!(
            error.get("code").and_then(serde_json::Value::as_i64),
            Some(i64::from(protocol::FILE_READ_ERROR)),
            "error code should be FILE_READ_ERROR"
        );
        assert!(
            error
                .get("message")
                .and_then(serde_json::Value::as_str)
                .is_some_and(|m| m.contains("Failed to read file")),
            "error message should mention file read failure"
        );
    }
}
