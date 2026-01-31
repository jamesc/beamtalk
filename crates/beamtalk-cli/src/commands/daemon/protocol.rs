// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! JSON-RPC 2.0 protocol handling for the compiler daemon.
//!
//! This module implements the JSON-RPC 2.0 protocol used for client-daemon
//! communication. It handles request parsing, method dispatch, and response
//! serialization.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use beamtalk_core::language_service::{LanguageService, SimpleLanguageService};
use camino::Utf8PathBuf;
use serde::{Deserialize, Serialize};
use tracing::{debug, error};

// ============================================================================
// JSON-RPC 2.0 Protocol Types
// ============================================================================

/// JSON-RPC 2.0 request.
#[derive(Debug, Deserialize)]
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub id: Option<serde_json::Value>,
    pub method: String,
    #[serde(default)]
    pub params: serde_json::Value,
}

/// JSON-RPC 2.0 response.
#[derive(Debug, Serialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

/// JSON-RPC 2.0 error.
#[derive(Debug, Serialize)]
pub struct JsonRpcError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,
}

// Standard JSON-RPC error codes
pub const PARSE_ERROR: i32 = -32700;
pub const INVALID_REQUEST: i32 = -32600;
pub const METHOD_NOT_FOUND: i32 = -32601;
pub const INVALID_PARAMS: i32 = -32602;
pub const INTERNAL_ERROR: i32 = -32603;

// Custom error codes (per JSON-RPC 2.0, -32000 to -32099 are reserved for implementation-defined server errors)
/// File read error - returned when the daemon cannot read a source file from disk.
pub const FILE_READ_ERROR: i32 = -32001;

impl JsonRpcResponse {
    pub fn success(id: Option<serde_json::Value>, result: serde_json::Value) -> Self {
        Self {
            jsonrpc: "2.0",
            id,
            result: Some(result),
            error: None,
        }
    }

    pub fn error(id: Option<serde_json::Value>, code: i32, message: impl Into<String>) -> Self {
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

// ============================================================================
// Request Handling
// ============================================================================

/// Handle a JSON-RPC request and return the response as a JSON string.
pub fn handle_request(
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
            tracing::info!("Received shutdown request, stopping daemon");
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

/// Validate a file path for daemon methods.
///
/// This function provides centralized path validation to prevent common security
/// and usability issues. It should be used by all methods that accept file paths.
///
/// # Validation Rules
///
/// Returns an error message if the path is invalid:
/// - Empty or whitespace-only paths (usability: prevents accidental empty input)
/// - Root path "/" (security: prevents operations on root directory)
///
/// # Future Considerations
///
/// If path validation is needed in other modules (e.g., for file operations outside
/// the daemon), consider extracting this to a shared utilities module such as
/// `crates/beamtalk-cli/src/validation.rs` or `crates/beamtalk-core/src/validation.rs`.
///
/// # Examples
///
/// ```ignore
/// assert!(validate_path("src/main.bt").is_ok());
/// assert!(validate_path("").is_err());
/// assert!(validate_path("/").is_err());
/// ```
fn validate_path(path: &str) -> Result<(), String> {
    let trimmed = path.trim();

    if trimmed.is_empty() {
        return Err("Path cannot be empty or whitespace".to_string());
    }

    if trimmed == "/" {
        return Err("Path cannot be root directory".to_string());
    }

    Ok(())
}

/// Read source code either from provided string or file path.
///
/// This helper function encapsulates the common pattern of:
/// 1. Using the source if provided in params
/// 2. Reading from the file path if no source is provided
/// 3. Returning an appropriate error message on failure
///
/// # Errors
/// Returns an error message string if file reading fails.
fn read_source_from_params_or_file(source: Option<String>, path: &str) -> Result<String, String> {
    match source {
        Some(s) => Ok(s),
        None => std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {e}")),
    }
}

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
pub struct DiagnosticInfo {
    pub message: String,
    pub severity: String,
    pub start: u32,
    pub end: u32,
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

    // Validate path before processing
    if let Err(msg) = validate_path(&params.path) {
        return JsonRpcResponse::error(id, INVALID_PARAMS, msg);
    }

    let file_path = Utf8PathBuf::from(&params.path);

    // Get source either from params or read from file
    let source = match read_source_from_params_or_file(params.source, &params.path) {
        Ok(s) => s,
        Err(e) => {
            return JsonRpcResponse::error(id, FILE_READ_ERROR, e);
        }
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

    // Validate path before processing
    if let Err(msg) = validate_path(&params.path) {
        return JsonRpcResponse::error(id, INVALID_PARAMS, msg);
    }

    let file_path = Utf8PathBuf::from(&params.path);

    // Get source either from params or read from file
    let source = match read_source_from_params_or_file(params.source, &params.path) {
        Ok(s) => s,
        Err(e) => {
            return JsonRpcResponse::error(id, FILE_READ_ERROR, e);
        }
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
