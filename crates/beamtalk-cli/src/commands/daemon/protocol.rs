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
#[derive(Debug, Serialize, Deserialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

/// JSON-RPC 2.0 error.
#[derive(Debug, Serialize, Deserialize)]
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
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(result),
            error: None,
        }
    }

    pub fn error(id: Option<serde_json::Value>, code: i32, message: impl Into<String>) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
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
#[derive(Debug, Serialize, Deserialize)]
struct CompileResult {
    success: bool,
    beam_path: Option<String>,
    core_erlang: Option<String>,
    diagnostics: Vec<DiagnosticInfo>,
    classes: Vec<ClassInfo>,
    /// The module name used for code generation, derived from the class name
    /// in the AST when available, otherwise from the file stem.
    /// The REPL uses this for `code:load_binary` to match the generated code.
    module_name: Option<String>,
    /// Pre-formatted diagnostic messages using miette
    formatted_diagnostics: Vec<String>,
}

/// Class metadata extracted from compiled source.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ClassInfo {
    name: String,
    superclass: String,
}

/// Diagnostic information for responses.
#[derive(Debug, Serialize, Deserialize)]
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
    use beamtalk_core::source_analysis::{lex_with_eof, parse};

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

    // Get diagnostics from language service
    let mut core_diagnostics = service.diagnostics(&file_path);

    // Parse module for additional validation
    let tokens = lex_with_eof(&source);
    let (module, _) = parse(tokens);

    // Run @primitive validation (ADR 0007)
    let options = beamtalk_core::CompilerOptions::default(); // Non-stdlib, primitives disallowed
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    core_diagnostics.extend(primitive_diags);

    let diagnostics: Vec<DiagnosticInfo> = core_diagnostics
        .iter()
        .map(|d| DiagnosticInfo {
            message: d.message.to_string(),
            severity: match d.severity {
                beamtalk_core::source_analysis::Severity::Error => "error".to_string(),
                beamtalk_core::source_analysis::Severity::Warning => "warning".to_string(),
            },
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();

    // Generate formatted diagnostics using miette
    let formatted_diagnostics: Vec<String> = core_diagnostics
        .iter()
        .map(|d| {
            let diag = crate::diagnostic::CompileDiagnostic::from_core_diagnostic(
                d,
                &params.path,
                &source,
            );
            format!("{:?}", miette::Report::new(diag))
        })
        .collect();

    let has_errors = diagnostics.iter().any(|d| d.severity == "error");

    // Generate Core Erlang if no errors
    let (core_erlang, class_names, module_name) = if has_errors {
        (None, vec![], None)
    } else {
        // Module already parsed above for primitive validation
        // Derive module name from class name in AST (the class definition is
        // the source of truth). :load is for loading class definitions; fall
        // back to file stem only for legacy non-class files.
        let classes = extract_class_names(&module);
        let module_name = if let Some(first_class) = module.classes.first() {
            beamtalk_core::erlang::to_module_name(&first_class.name.name)
        } else {
            file_path.file_stem().unwrap_or("module").to_string()
        };

        let core = beamtalk_core::erlang::generate_with_name_and_source(
            &module,
            &module_name,
            Some(&source),
        )
        .ok();
        (core, classes, Some(module_name))
    };

    let result = CompileResult {
        success: !has_errors,
        beam_path: None, // TODO: Invoke erlc
        core_erlang,
        diagnostics,
        classes: class_names,
        module_name,
        formatted_diagnostics,
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
    /// Known variable names from REPL session bindings.
    /// These are treated as pre-defined to avoid "Undefined variable" errors.
    #[serde(default)]
    known_variables: Vec<String>,
}

/// Result of the `compile_expression` method.
#[derive(Debug, Serialize, Deserialize)]
struct CompileExpressionResult {
    success: bool,
    core_erlang: Option<String>,
    diagnostics: Vec<DiagnosticInfo>,
    /// Pre-formatted diagnostic messages using miette
    formatted_diagnostics: Vec<String>,
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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&params.source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Convert known_variables to &str references for the diagnostics function
    let known_vars: Vec<&str> = params.known_variables.iter().map(String::as_str).collect();

    // Run semantic analysis with known REPL variables to avoid false "Undefined variable" errors
    let mut all_diagnostics =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars(
            &module,
            parse_diagnostics,
            &known_vars,
        );

    // Run @primitive validation (ADR 0007) - REPL is never stdlib
    let options = beamtalk_core::CompilerOptions::default();
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    // Convert diagnostics
    let diagnostics: Vec<DiagnosticInfo> = all_diagnostics
        .iter()
        .map(|d| DiagnosticInfo {
            message: d.message.to_string(),
            severity: match d.severity {
                beamtalk_core::source_analysis::Severity::Error => "error".to_string(),
                beamtalk_core::source_analysis::Severity::Warning => "warning".to_string(),
            },
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();

    // Generate formatted diagnostics using miette
    let formatted_diagnostics: Vec<String> = all_diagnostics
        .iter()
        .map(|d| {
            let diag = crate::diagnostic::CompileDiagnostic::from_core_diagnostic(
                d,
                "repl",
                &params.source,
            );
            format!("{:?}", miette::Report::new(diag))
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
        formatted_diagnostics,
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

/// Extract class information from a parsed module.
/// Returns class name and superclass for each class definition.
fn extract_class_names(module: &beamtalk_core::ast::Module) -> Vec<ClassInfo> {
    module
        .classes
        .iter()
        .map(|class| ClassInfo {
            name: class.name.name.to_string(),
            superclass: class.superclass.name.to_string(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    //! Unit tests for the JSON-RPC 2.0 daemon protocol.
    //!
    //! Tests are organized by functionality:
    //! - **JSON-RPC Response Tests**: Protocol conformance (version checks, error codes)
    //! - **Path Validation Tests**: Security validation (empty paths, root directory)
    //! - **Source Reading Tests**: Parameter handling and file I/O fallback
    //! - **Request Handling Tests**: Message dispatch and error handling
    //! - **Compile Expression Tests**: REPL expression compilation
    //! - **Diagnostics Method Tests**: Syntax error detection
    //! - **Compile Method Tests**: Full module compilation
    //! - **Helper Function Tests**: Utility function validation

    use super::*;

    // ========================================================================
    // JSON-RPC Response Tests
    // ========================================================================

    #[test]
    fn jsonrpc_response_success_includes_result() {
        let response =
            JsonRpcResponse::success(Some(serde_json::json!(1)), serde_json::json!("ok"));
        assert_eq!(response.jsonrpc, "2.0");
        assert_eq!(response.id, Some(serde_json::json!(1)));
        assert_eq!(response.result, Some(serde_json::json!("ok")));
        assert!(response.error.is_none());
    }

    #[test]
    fn jsonrpc_response_error_includes_error() {
        let response = JsonRpcResponse::error(
            Some(serde_json::json!(2)),
            METHOD_NOT_FOUND,
            "Unknown method",
        );
        assert_eq!(response.jsonrpc, "2.0");
        assert_eq!(response.id, Some(serde_json::json!(2)));
        assert!(response.result.is_none());
        assert!(response.error.is_some());

        let error = response.error.unwrap();
        assert_eq!(error.code, METHOD_NOT_FOUND);
        assert_eq!(error.message, "Unknown method");
    }

    #[test]
    fn jsonrpc_response_serializes_correctly() {
        let response = JsonRpcResponse::success(Some(serde_json::json!(1)), serde_json::json!(42));
        let json = serde_json::to_string(&response).unwrap();
        assert!(json.contains("\"jsonrpc\":\"2.0\""));
        assert!(json.contains("\"id\":1"));
        assert!(json.contains("\"result\":42"));
    }

    // ========================================================================
    // Path Validation Tests
    // ========================================================================

    #[test]
    fn validate_path_accepts_valid_paths() {
        assert!(validate_path("src/main.bt").is_ok());
        assert!(validate_path("lib/foo.bt").is_ok());
        assert!(validate_path("test.bt").is_ok());
        assert!(validate_path("/home/user/project/file.bt").is_ok());
    }

    #[test]
    fn validate_path_rejects_empty_string() {
        assert!(validate_path("").is_err());
        assert_eq!(
            validate_path("").unwrap_err(),
            "Path cannot be empty or whitespace"
        );
    }

    #[test]
    fn validate_path_rejects_whitespace_only() {
        assert!(validate_path("   ").is_err());
        assert!(validate_path("\t").is_err());
        assert!(validate_path("\n").is_err());
    }

    #[test]
    fn validate_path_rejects_root_directory() {
        assert!(validate_path("/").is_err());
        assert_eq!(
            validate_path("/").unwrap_err(),
            "Path cannot be root directory"
        );
    }

    #[test]
    fn validate_path_trims_whitespace() {
        // Should accept path with leading/trailing whitespace after trim
        assert!(validate_path("  src/main.bt  ").is_ok());
    }

    // ========================================================================
    // Source Reading Tests
    // ========================================================================

    #[test]
    fn read_source_prefers_provided_source() {
        let source = Some("x := 42".to_string());
        let result = read_source_from_params_or_file(source, "nonexistent.bt");
        assert_eq!(result.unwrap(), "x := 42");
    }

    #[test]
    fn read_source_returns_error_for_nonexistent_file() {
        let result = read_source_from_params_or_file(None, "/nonexistent/file.bt");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Failed to read file"));
    }

    // ========================================================================
    // Request Handling Tests
    // ========================================================================

    #[test]
    fn handle_request_rejects_invalid_json() {
        let mut service = SimpleLanguageService::new();
        let running = Arc::new(AtomicBool::new(true));

        let response_str = handle_request("invalid json", &mut service, &running);
        let response: JsonRpcResponse = serde_json::from_str(&response_str).unwrap();

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, PARSE_ERROR);
        assert!(error.message.contains("Parse error"));
    }

    #[test]
    fn handle_request_rejects_wrong_jsonrpc_version() {
        let mut service = SimpleLanguageService::new();
        let running = Arc::new(AtomicBool::new(true));

        let request = r#"{"jsonrpc":"1.0","id":1,"method":"ping"}"#;
        let response_str = handle_request(request, &mut service, &running);
        let response: JsonRpcResponse = serde_json::from_str(&response_str).unwrap();

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, INVALID_REQUEST);
        assert_eq!(error.message, "Invalid JSON-RPC version");
    }

    #[test]
    fn handle_request_ping_returns_pong() {
        let mut service = SimpleLanguageService::new();
        let running = Arc::new(AtomicBool::new(true));

        let request = r#"{"jsonrpc":"2.0","id":1,"method":"ping"}"#;
        let response_str = handle_request(request, &mut service, &running);
        let response: JsonRpcResponse = serde_json::from_str(&response_str).unwrap();

        assert!(response.error.is_none());
        assert_eq!(response.result, Some(serde_json::json!("pong")));
    }

    #[test]
    fn handle_request_unknown_method_returns_error() {
        let mut service = SimpleLanguageService::new();
        let running = Arc::new(AtomicBool::new(true));

        let request = r#"{"jsonrpc":"2.0","id":1,"method":"unknown"}"#;
        let response_str = handle_request(request, &mut service, &running);
        let response: JsonRpcResponse = serde_json::from_str(&response_str).unwrap();

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, METHOD_NOT_FOUND);
        assert!(error.message.contains("Method not found"));
    }

    #[test]
    fn handle_request_shutdown_stops_daemon() {
        let mut service = SimpleLanguageService::new();
        let running = Arc::new(AtomicBool::new(true));

        let request = r#"{"jsonrpc":"2.0","id":1,"method":"shutdown"}"#;
        let response_str = handle_request(request, &mut service, &running);
        let response: JsonRpcResponse = serde_json::from_str(&response_str).unwrap();

        assert!(response.error.is_none());
        // Result may be None or Some(null) depending on serialization
        assert!(!running.load(Ordering::SeqCst));
    }

    // ========================================================================
    // Compile Expression Tests
    // ========================================================================

    #[test]
    fn handle_compile_expression_with_valid_code() {
        let params = serde_json::json!({
            "source": "42",
            "module_name": "test_module"
        });

        let response = handle_compile_expression(params, Some(serde_json::json!(1)));

        assert!(response.error.is_none());
        let result: CompileExpressionResult =
            serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(result.success);
        assert!(result.core_erlang.is_some());
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn handle_compile_expression_with_invalid_code() {
        let params = serde_json::json!({
            "source": "x := :=",
            "module_name": "test_module"
        });

        let response = handle_compile_expression(params, Some(serde_json::json!(1)));

        assert!(response.error.is_none());
        let result: CompileExpressionResult =
            serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(!result.success);
        assert!(!result.diagnostics.is_empty());
    }

    #[test]
    fn handle_compile_expression_with_primitive_fails() {
        // @primitive can only appear in method bodies, so we need a class definition
        let params = serde_json::json!({
            "source": "Object subclass: T\n  m => @primitive '+'",
            "module_name": "test_module"
        });

        let response = handle_compile_expression(params, Some(serde_json::json!(1)));

        assert!(response.error.is_none());
        let result: CompileExpressionResult =
            serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(
            !result.success,
            "Expected REPL expression with primitive to fail"
        );
        assert!(!result.diagnostics.is_empty(), "Expected diagnostic errors");

        // Should contain primitive validation error
        let has_primitive_error = result.diagnostics.iter().any(|d| {
            d.message
                .contains("Primitives can only be declared in the standard library")
        });
        assert!(
            has_primitive_error,
            "Expected primitive validation error, got: {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn handle_compile_expression_with_missing_params() {
        let params = serde_json::json!({
            "source": "42"
            // missing module_name
        });

        let response = handle_compile_expression(params, Some(serde_json::json!(1)));

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, INVALID_PARAMS);
    }

    // ========================================================================
    // Diagnostics Method Tests
    // ========================================================================

    #[test]
    fn handle_diagnostics_with_valid_inline_source() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "test.bt",
            "source": "x := 42"
        });

        let response = handle_diagnostics(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_none());
        let diagnostics: Vec<DiagnosticInfo> =
            serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn handle_diagnostics_with_syntax_errors() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "test.bt",
            "source": "x := :="
        });

        let response = handle_diagnostics(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_none());
        let diagnostics: Vec<DiagnosticInfo> =
            serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn handle_diagnostics_with_empty_path() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "",
            "source": "x := 42"
        });

        let response = handle_diagnostics(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, INVALID_PARAMS);
    }

    #[test]
    fn handle_diagnostics_with_root_path() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "/",
            "source": "x := 42"
        });

        let response = handle_diagnostics(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, INVALID_PARAMS);
    }

    // ========================================================================
    // Compile Method Tests
    // ========================================================================

    #[test]
    fn handle_compile_with_valid_inline_source() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "test.bt",
            "source": "x := 42"
        });

        let response = handle_compile(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_none());
        let result: CompileResult = serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(result.success);
        assert!(result.core_erlang.is_some());
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn handle_compile_with_syntax_errors() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "test.bt",
            "source": "x := :="
        });

        let response = handle_compile(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_none());
        let result: CompileResult = serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(!result.success);
        assert!(result.core_erlang.is_none());
        assert!(!result.diagnostics.is_empty());
    }

    #[test]
    fn handle_compile_with_empty_path() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "",
            "source": "x := 42"
        });

        let response = handle_compile(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, INVALID_PARAMS);
    }

    #[test]
    fn handle_compile_with_invalid_params() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            // missing path
            "source": "x := 42"
        });

        let response = handle_compile(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, INVALID_PARAMS);
    }

    #[test]
    fn handle_compile_returns_module_name_from_class() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "Counter.bt",
            "source": "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1"
        });

        let response = handle_compile(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_none());
        let result: CompileResult = serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(result.success);
        assert_eq!(result.module_name, Some("counter".to_string()));
    }

    #[test]
    fn handle_compile_returns_module_name_from_file_stem_without_class() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "test.bt",
            "source": "x := 42"
        });

        let response = handle_compile(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_none());
        let result: CompileResult = serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(result.success);
        assert_eq!(result.module_name, Some("test".to_string()));
    }

    #[test]
    fn handle_compile_with_primitive_in_non_stdlib_fails() {
        let mut service = SimpleLanguageService::new();
        let params = serde_json::json!({
            "path": "MyClass.bt",
            "source": "Object subclass: MyClass\n  foo => @primitive '+'"
        });

        let response = handle_compile(params, Some(serde_json::json!(1)), &mut service);

        assert!(response.error.is_none());
        let result: CompileResult = serde_json::from_value(response.result.unwrap()).unwrap();
        assert!(
            !result.success,
            "Expected compilation to fail with primitive error"
        );
        assert!(!result.diagnostics.is_empty(), "Expected diagnostic errors");

        // Should contain primitive validation error
        let has_primitive_error = result.diagnostics.iter().any(|d| {
            d.message
                .contains("Primitives can only be declared in the standard library")
        });
        assert!(
            has_primitive_error,
            "Expected primitive validation error, got: {:#?}",
            result.diagnostics
        );
    }

    // ========================================================================
    // Helper Function Tests
    // ========================================================================

    #[test]
    fn extract_class_names_returns_empty_for_module_without_classes() {
        let module = beamtalk_core::ast::Module::new(
            vec![],
            beamtalk_core::source_analysis::Span::new(0, 0),
        );
        let classes = extract_class_names(&module);
        assert!(classes.is_empty());
    }

    #[test]
    fn extract_class_names_returns_class_info_with_superclass() {
        use beamtalk_core::ast::{ClassDefinition, Identifier};
        use beamtalk_core::source_analysis::Span;

        let class = ClassDefinition::new(
            Identifier::new("Counter", Span::new(0, 7)),
            Identifier::new("Actor", Span::new(18, 23)),
            vec![],
            vec![],
            Span::new(0, 50),
        );
        let module = beamtalk_core::ast::Module::with_classes(vec![class], Span::new(0, 50));
        let classes = extract_class_names(&module);

        assert_eq!(classes.len(), 1);
        assert_eq!(classes[0].name, "Counter");
        assert_eq!(classes[0].superclass, "Actor");
    }
}
