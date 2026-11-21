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
    model::{CallToolResult, ContentBlock, Implementation, ServerCapabilities, ServerConfig},
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
pub(crate) use beamtalk_core::source_analysis::{lex_with_eof, parse};
#[cfg(test)]
pub(crate) use lint::{compute_diagnostic_summary, run_lint_structured};
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
    /// Start timing a tool call and log `"tool invoked"` with just the tool name.
    fn start(tool: &'static str) -> Self {
        tracing::debug!(tool, "tool invoked");
        Self::start_quiet(tool)
    }

    /// Start timing without logging, for handlers that log `"tool invoked"`
    /// themselves with extra argument fields.
    fn start_quiet(tool: &'static str) -> Self {
        Self {
            tool,
            start: std::time::Instant::now(),
            is_ok: false,
        }
    }

    fn mark_ok(&mut self) {
        self.is_ok = true;
    }

    /// Mark the call successful and return `text` as a single-block result.
    fn ok_text(&mut self, text: impl Into<String>) -> Result<CallToolResult, rmcp::ErrorData> {
        self.ok_parts(vec![ContentBlock::text(text.into())])
    }

    /// Mark the call successful and return `parts` as the result content.
    #[expect(
        clippy::unnecessary_wraps,
        reason = "returned directly from handlers, which return Result"
    )]
    fn ok_parts(&mut self, parts: Vec<ContentBlock>) -> Result<CallToolResult, rmcp::ErrorData> {
        self.mark_ok();
        Ok(CallToolResult::success(parts))
    }
}

/// Map a transport/client error into an MCP internal error.
trait InternalErr<T> {
    fn internal_err(self) -> Result<T, rmcp::ErrorData>;
}

impl<T, E: std::fmt::Display> InternalErr<T> for Result<T, E> {
    fn internal_err(self) -> Result<T, rmcp::ErrorData> {
        self.map_err(|e| rmcp::ErrorData::internal_error(e.to_string(), None))
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
    /// Which filesystem paths the path-taking tools may name (BT-5070).
    path_policy: PathPolicy,
}

/// Which filesystem paths tool arguments may name (BT-5070).
///
/// `lint`, `diagnostic_summary`, `load_file`, `load_project`, `test` and
/// `export_traces` take paths from the MCP client. By default they must lie
/// inside the project root, so a prompt-injected agent cannot point them at
/// `~/.ssh` or write an export over an arbitrary file. The server is still
/// an authenticated local client running code through `evaluate` (ADR 0020);
/// this closes the gap between the typed file tools and that, not the
/// `evaluate` tool itself. `--allow-outside-project` opts out.
#[derive(Debug, Clone)]
pub enum PathPolicy {
    /// Paths must resolve (symlinks and `..` included) inside this root.
    Confined(std::path::PathBuf),
    /// No confinement.
    Unconfined,
}

impl PathPolicy {
    /// Confine to the project root discovered from the current directory.
    pub fn project_root_of_cwd() -> std::io::Result<Self> {
        let cwd = std::env::current_dir()?;
        let root = beamtalk_project::discover_project_root(&cwd);
        Ok(Self::Confined(root.canonicalize()?))
    }

    /// Resolve `path` (relative paths against the current directory) to a
    /// canonical path and check it against the policy. A path that does not
    /// exist yet (an export target) is judged by its canonical parent.
    pub fn check(&self, path: &str) -> Result<std::path::PathBuf, String> {
        use std::path::{Path, PathBuf};
        let raw = Path::new(path);
        let abs: PathBuf = if raw.is_absolute() {
            raw.to_path_buf()
        } else {
            std::env::current_dir()
                .map_err(|e| format!("cannot resolve '{path}': {e}"))?
                .join(raw)
        };
        // Canonicalize the longest existing prefix (resolving symlinks and
        // `..`), then append the not-yet-existing tail, which may not contain
        // `..`. So an export target or a mistyped path is judged by where it
        // would land, not refused for not existing.
        let mut existing = abs.as_path();
        let mut tail: Vec<&std::ffi::OsStr> = Vec::new();
        let mut resolved = loop {
            if let Ok(real) = existing.canonicalize() {
                break real;
            }
            let name = existing
                .file_name()
                .ok_or_else(|| format!("cannot resolve '{path}'"))?;
            tail.push(name);
            existing = existing
                .parent()
                .ok_or_else(|| format!("cannot resolve '{path}'"))?;
        };
        resolved.extend(tail.iter().rev());
        match self {
            Self::Unconfined => Ok(resolved),
            Self::Confined(root) if resolved.starts_with(root) => Ok(resolved),
            Self::Confined(root) => Err(format!(
                "path '{path}' is outside the project root ({}). Start beamtalk-mcp with \
                 --allow-outside-project to allow other locations.",
                root.display()
            )),
        }
    }
}

impl BeamtalkMcp {
    /// Create a new MCP server backed by the provided REPL client.
    pub fn new(client: Arc<ReplClient>, path_policy: PathPolicy) -> Self {
        Self {
            client,
            path_policy,
        }
    }

    /// A one-line note naming the workspace this server is attached to
    /// (BT-5080), appended to state-changing and discovery tool output so an
    /// agent can see which workspace its call touched.
    pub(crate) fn workspace_note(&self) -> String {
        match self.client.workspace_id() {
            Some(id) => format!("Workspace: {id}"),
            None => "Workspace: (explicit --port)".to_string(),
        }
    }

    /// Check a client-supplied path against the path policy, answering the
    /// canonical path or a tool error result to return as-is.
    pub(crate) fn confine_path(&self, path: &str) -> Result<String, CallToolResult> {
        self.path_policy
            .check(path)
            .map(|p| p.to_string_lossy().into_owned())
            .map_err(error_result)
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
/// Erlang module names are unquoted atoms: start with a lowercase letter,
/// followed by alphanumerics and underscores. Delegates to the canonical definition in
/// `beamtalk_core::source_analysis::is_valid_erlang_module_name`.
fn validate_erlang_module_name(name: &str) -> Result<(), rmcp::ErrorData> {
    if !beamtalk_core::source_analysis::is_valid_erlang_module_name(name) {
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

/// Splits `code` at a client-supplied byte `cursor` for `complete`.
///
/// A missing cursor means "end of code" and one past the end is clamped. A
/// cursor inside a multi-byte UTF-8 character is rejected with
/// `invalid_params` rather than slicing there (which panics).
fn code_up_to_cursor(code: &str, cursor: Option<usize>) -> Result<(&str, usize), rmcp::ErrorData> {
    let cursor = cursor.unwrap_or(code.len()).min(code.len());
    code.get(..cursor)
        .map(|prefix| (prefix, cursor))
        .ok_or_else(|| {
            rmcp::ErrorData::invalid_params(
                format!(
                    "Invalid cursor: {cursor} is not on a character boundary. \
                     The cursor is a byte offset into `code`."
                ),
                None,
            )
        })
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
    fn get_info(&self) -> ServerConfig {
        ServerConfig::new(ServerCapabilities::builder().enable_tools().build())
            .with_server_info(Implementation::new(
                "beamtalk-mcp",
                beamtalk_repl_protocol::handshake::build_id(),
            ))
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
                 'unload' to remove a class, and 'interrupt' to cancel evaluations. \
                 'describe', 'flush', 'remove_class' and 'rename_class' name the attached \
                 workspace in their output.",
            )
    }
}
