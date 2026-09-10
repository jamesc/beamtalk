// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! MCP server exposing beamtalk REPL operations as tools.
//!
//! **DDD Context:** Language Service / Interactive Development
//!
//! Uses the `rmcp` crate to implement an MCP server that wraps the
//! beamtalk REPL's JSON-over-TCP protocol, allowing any MCP-compatible
//! agent to interact with live beamtalk objects.
//!
//! Tool implementations live under [`tools`], one module per tool family,
//! each contributing an `impl BeamtalkMcp` block wired up via
//! `#[tool_router(router = ...)]`; [`BeamtalkMcp::tool_router`] below sums
//! them into the router `#[tool_handler]` dispatches through. Parameter
//! types live in [`params`]; the offline lint/diagnostic-summary analysis
//! pipeline lives in [`lint`].

use std::sync::Arc;

use rmcp::{
    ServerHandler,
    handler::server::router::tool::ToolRouter,
    model::{CallToolResult, ContentBlock, ServerCapabilities, ServerInfo},
    tool_handler,
};

use beamtalk_repl_protocol::format::{self as fmt, Diagnostic as FmtDiagnostic, OutputMode};

use crate::client::ReplClient;

mod lint;
mod params;
mod tools;

#[cfg(test)]
mod tests;

// Re-exported so `server::tests`'s `use super::*;` sees every tool-family
// helper flatly, exactly as it did when this was all one file — tests
// exercise these pure helpers directly, independent of which `tools::*`
// family module or `lint` owns them.
#[cfg(test)]
pub(crate) use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
#[cfg(test)]
pub(crate) use lint::{
    build_native_type_registry, compute_diagnostic_summary, run_lint_structured,
    run_module_analysis,
};
#[cfg(test)]
pub(crate) use params::*;
#[cfg(test)]
pub(crate) use rmcp::handler::server::wrapper::Parameters;
#[cfg(test)]
pub(crate) use tools::docs::compute_doc_method_categories;
#[cfg(test)]
pub(crate) use tools::editing::{save_method_expr, try_method_expr};

/// MCP tool responses are plain text (no terminal escapes).
const MCP_OUTPUT_MODE: OutputMode = OutputMode::Plain;

/// Drop guard that logs MCP tool completion with duration and result status.
///
/// Defaults to `error` — callers must explicitly call [`mark_ok`] on the success
/// path so that early returns via `?`, `check_response!`, or `error_result()`
/// are correctly reported as errors.
struct ToolTimer {
    tool: &'static str,
    start: std::time::Instant,
    is_ok: bool,
}

impl ToolTimer {
    fn new(tool: &'static str) -> Self {
        Self {
            tool,
            start: std::time::Instant::now(),
            is_ok: false,
        }
    }

    fn mark_ok(&mut self) {
        self.is_ok = true;
    }
}

impl Drop for ToolTimer {
    #[allow(clippy::cast_possible_truncation)]
    fn drop(&mut self) {
        let elapsed_ms = self.start.elapsed().as_millis() as u64;
        let status = if self.is_ok { "ok" } else { "error" };
        tracing::debug!(tool = self.tool, elapsed_ms, status, "tool completed");
    }
}

/// MCP server backed by a beamtalk REPL connection.
#[derive(Clone)]
pub struct BeamtalkMcp {
    /// Shared REPL client used by all tool handlers.
    client: Arc<ReplClient>,
    /// Router that dispatches incoming MCP tool calls to handler methods.
    #[allow(dead_code)]
    tool_router: ToolRouter<Self>,
}

impl BeamtalkMcp {
    /// Create a new MCP server backed by the provided REPL client.
    pub fn new(client: Arc<ReplClient>) -> Self {
        Self {
            client,
            tool_router: Self::tool_router(),
        }
    }

    /// The combined tool router: every tool family's router, unioned.
    ///
    /// Each family in [`tools`] registers its own `impl BeamtalkMcp` block
    /// under `#[tool_router(router = <family>_tool_router, vis =
    /// "pub(crate)")]`; `ToolRouter` implements `Add`, so summing them here
    /// gives the one router `#[tool_handler]` below dispatches every
    /// incoming call through.
    pub(crate) fn tool_router() -> ToolRouter<Self> {
        Self::evaluate_tool_router()
            + Self::docs_tool_router()
            + Self::editing_tool_router()
            + Self::traces_tool_router()
            + Self::diagnostics_tool_router()
            + Self::flush_tool_router()
    }
}

/// Create an error `CallToolResult` with `is_error` set to true.
fn error_result(msg: impl Into<String>) -> CallToolResult {
    CallToolResult::error(vec![ContentBlock::text(msg.into())])
}

/// Validate that a string is a valid Beamtalk class name (uppercase-starting identifier).
fn validate_class_name(name: &str) -> Result<(), rmcp::ErrorData> {
    if !beamtalk_core::source_analysis::is_valid_class_name(name) {
        return Err(rmcp::ErrorData::invalid_params(
            format!("Invalid class name: '{name}'. Must be an uppercase-starting identifier."),
            None,
        ));
    }
    Ok(())
}

/// Validate that a string is a valid Erlang module name.
///
/// Erlang module names are lowercase atoms: start with a lowercase letter or underscore,
/// followed by alphanumerics and underscores.
fn validate_erlang_module_name(name: &str) -> Result<(), rmcp::ErrorData> {
    if name.is_empty()
        || !name.starts_with(|c: char| c.is_ascii_lowercase() || c == '_')
        || !name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
    {
        return Err(rmcp::ErrorData::invalid_params(
            format!("Invalid Erlang module name: '{name}'. Must be a lowercase identifier."),
            None,
        ));
    }
    Ok(())
}

/// Validate that a string is a valid Beamtalk selector.
///
/// Accepts keyword/unary selectors (`increment`, `at:put:`) and binary operator
/// selectors (`+`, `>=`, `**`). Delegates to the canonical implementation in
/// `beamtalk_core::source_analysis::validate_selector_input`.
fn validate_selector(sel: &str) -> Result<(), rmcp::ErrorData> {
    beamtalk_core::source_analysis::validate_selector_input(sel)
        .map_err(|e| rmcp::ErrorData::invalid_params(e, None))
}

/// Pretty-print a JSON value, falling back to `Display` on serialization error.
fn pretty_json(value: &serde_json::Value) -> String {
    serde_json::to_string_pretty(value).unwrap_or_else(|_| value.to_string())
}

/// Check a REPL response for errors and return early with a formatted error result.
///
/// The `$fallback` string is used when the response has no error message.
/// Uses the shared `format_diagnostic` helper so MCP error rendering
/// stays in lockstep with CLI output.
macro_rules! check_response {
    ($response:expr, $fallback:expr) => {
        if $response.is_error() {
            let msg = $response.error_message().unwrap_or($fallback);
            return Ok($crate::server::error_result(
                $crate::server::fmt::format_diagnostic(
                    &$crate::server::FmtDiagnostic::new(msg),
                    $crate::server::MCP_OUTPUT_MODE,
                ),
            ));
        }
    };
}
pub(crate) use check_response;

#[tool_handler]
impl ServerHandler for BeamtalkMcp {
    /// Return server metadata and capabilities advertised to MCP clients.
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build())
            .with_instructions(
                "Beamtalk MCP server — interact with live beamtalk objects through the REPL. \
                 Use 'evaluate' to run beamtalk expressions, 'load_project' to load all files \
                 from a project in dependency order, 'load_file' to load a single source file, \
                 'list_actors' to see running actors, 'list_classes' for a class overview with optional superclass/scope filter, \
                 'inspect' to examine actor state, \
                 'reload_class' for hot code reloading, 'test' to run BUnit tests, \
                 'lint' to run style/redundancy checks on .bt source files, \
                 'diagnostic_summary' for aggregated diagnostic counts and type-coverage stats (works offline, no REPL needed), \
                 'search_classes' to discover Beamtalk classes by keyword or concept (works offline, no REPL needed), \
                 'search_examples' to find Beamtalk code examples by keyword (works offline, no REPL needed), \
                 'show_codegen' to inspect generated Core Erlang (use class+selector for loaded classes), 'info' for symbol details, \
                 'list_packages' to see loaded packages with metadata, \
                 'package_classes' to list classes in a package, \
                 'save_method' / 'try_method' to durably or ephemerally patch a class method (ADR 0082), \
                 'remove_method' to remove a class method (ADR 0112), \
                 'save_class' to create a new class file pending flush, \
                 'list_changes' / 'dirty_methods' to inspect pending workspace changes, \
                 'flush' to write durable ChangeLog entries to disk, \
                 'describe' for capability discovery, 'clear' to reset bindings, \
                 'unload' to remove a class, and 'interrupt' to cancel evaluations.",
            )
    }
}
