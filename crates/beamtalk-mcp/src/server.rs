// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! MCP server exposing beamtalk REPL operations as tools.
//!
//! **DDD Context:** Language Service / Interactive Development
//!
//! Uses the `rmcp` crate to implement an MCP server that wraps the
//! beamtalk REPL's JSON-over-TCP protocol, allowing any MCP-compatible
//! agent to interact with live beamtalk objects.

use std::fmt::Write;
use std::sync::Arc;

use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_core::tool_expr::{
    FlushFilter, flush_expr_with_confirm_destructive, precheck_method_expr, remove_class_expr,
    remove_method_expr, remove_method_if_absent_expr, rename_class_expr, rename_method_expr,
    save_class_expr,
};
use beamtalk_core::unparse::escape_string_literal;
use rmcp::{
    ServerHandler,
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{CallToolResult, ContentBlock, ServerCapabilities, ServerInfo},
    schemars, tool, tool_handler, tool_router,
};
use sha2::{Digest, Sha256};

use beamtalk_repl_protocol::format::{self as fmt, Diagnostic as FmtDiagnostic, OutputMode};

use crate::client::ReplClient;

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

/// Discover package corpus files from the working directory's `_build/` tree.
///
/// Looks for `corpus.json` and `class_corpus.json` in:
/// - `_build/dev/` (root package)
/// - `_build/deps/*/` (dependencies, resolved via their `_build/dev/`)
///
/// Returns `(example_corpora, class_corpora)` loaded from disk.
fn discover_package_corpora() -> (
    Vec<beamtalk_examples::Corpus>,
    Vec<beamtalk_examples::ClassCorpus>,
) {
    let mut example_corpora = Vec::new();
    let mut class_corpora = Vec::new();

    let Ok(cwd) = std::env::current_dir() else {
        return (example_corpora, class_corpora);
    };

    let root = beamtalk_project::discover_project_root(&cwd);

    // Root package corpus
    let dev_dir = root.join("_build").join("dev");
    if let Some(corpus) = beamtalk_examples::load_corpus_from_file(&dev_dir.join("corpus.json")) {
        tracing::debug!(path = %dev_dir.display(), "Loaded root package corpus");
        example_corpora.push(corpus);
    }
    if let Some(corpus) =
        beamtalk_examples::load_class_corpus_from_file(&dev_dir.join("class_corpus.json"))
    {
        tracing::debug!(path = %dev_dir.display(), "Loaded root package class corpus");
        class_corpora.push(corpus);
    }

    // Dependency corpora
    let deps_dir = root.join("_build").join("deps");
    if let Ok(entries) = std::fs::read_dir(&deps_dir) {
        for entry in entries.flatten() {
            let dep_path = entry.path();
            if !dep_path.is_dir() {
                continue;
            }
            // Dependencies build their corpus in their own _build/dev/ during compilation,
            // but the corpus is also placed alongside ebin in the dep's checkout.
            // Check both the dep's _build/dev/ and the dep root itself.
            for search_dir in [dep_path.join("_build").join("dev"), dep_path.clone()] {
                if let Some(corpus) =
                    beamtalk_examples::load_corpus_from_file(&search_dir.join("corpus.json"))
                {
                    tracing::debug!(path = %search_dir.display(), "Loaded dependency corpus");
                    example_corpora.push(corpus);
                    break;
                }
            }
            for search_dir in [dep_path.join("_build").join("dev"), dep_path.clone()] {
                if let Some(corpus) = beamtalk_examples::load_class_corpus_from_file(
                    &search_dir.join("class_corpus.json"),
                ) {
                    tracing::debug!(path = %search_dir.display(), "Loaded dependency class corpus");
                    class_corpora.push(corpus);
                    break;
                }
            }
        }
    }

    (example_corpora, class_corpora)
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

/// BT-3239: locate `class`'s on-disk `.bt` source via a `nav-symbols` round
/// trip, then compute its divider-grouped method categories locally.
///
/// The `nav-symbols` op is the same one the LSP's `documentSymbol`
/// runtime-delegate path already sends (`crates/beamtalk-lsp/src/runtime.rs`)
/// — reused here purely to resolve `class` -> source file path; the
/// categorization itself runs in-process against `beamtalk-core`'s
/// `source_analysis::categorize_methods_in_source`, the same function the
/// static AST-walker `documentSymbol` path calls (BT-2601) — no
/// reimplementation, no second port round trip. Returns `None` (never an
/// error) whenever nothing is computable: the class isn't in the live
/// registry, it has no source file, the file can't be read, or the class
/// can't be found/categorized in it.
///
/// Requests `scope: "user"` — only classes with a backing `.bt` file, per
/// `RequestBuilder::nav_symbols`'s doc — rather than `"all"`: a class with
/// no source file could never yield a `source_file` below regardless, so
/// the narrower scope is both the correct filter and a smaller reply to
/// pull over the wire on an image with many loaded classes.
async fn doc_method_categories(client: &ReplClient, class: &str) -> Option<serde_json::Value> {
    let response = client.nav_symbols(Some("user")).await.ok()?;
    if response.is_error() {
        return None;
    }
    let payload: beamtalk_language_service::NavSymbolsResponse =
        serde_json::from_value(response.value?).ok()?;
    let source_file = payload
        .classes
        .into_iter()
        .find(|c| c.name == class)?
        .source_file?;
    // File I/O + parsing is blocking/CPU-bound work — run it off the Tokio
    // worker thread, same as the `lint`/`diagnostic_summary` tools' own
    // `spawn_blocking` wrapping around comparable offline analysis.
    let class_owned = class.to_string();
    tokio::task::spawn_blocking(move || compute_doc_method_categories(&source_file, &class_owned))
        .await
        .ok()
        .flatten()
}

/// Pure helper behind [`doc_method_categories`]: read `source_file` and
/// categorize `class`'s methods by its `// === Name ===` section dividers,
/// returning a JSON value shaped
/// `{"class": ..., "categories": [{"name": ..|null, "methods": [{"selector",
/// "side"}]}]}` — or `None` if the file can't be read or `class` can't be
/// found/categorized in it. Split out from [`doc_method_categories`] so it
/// can be unit-tested against a fixture file with no live REPL connection,
/// matching this module's other offline-testable helpers (e.g.
/// `run_lint_structured`).
fn compute_doc_method_categories(source_file: &str, class: &str) -> Option<serde_json::Value> {
    use beamtalk_core::source_analysis::{MethodSide, categorize_methods_in_source};

    let source = std::fs::read_to_string(source_file).ok()?;
    let (result, _diagnostics) = categorize_methods_in_source(&source, class);
    let categories = result.ok()?;
    let categories_json: Vec<serde_json::Value> = categories
        .iter()
        .map(|category| {
            let methods: Vec<serde_json::Value> = category
                .methods
                .iter()
                .map(|method| {
                    let side = match method.side {
                        MethodSide::Instance => "instance",
                        MethodSide::Class => "class",
                    };
                    serde_json::json!({"selector": method.selector, "side": side})
                })
                .collect();
            serde_json::json!({"name": category.name, "methods": methods})
        })
        .collect();
    Some(serde_json::json!({"class": class, "categories": categories_json}))
}

/// Build the Beamtalk expression for the `save_method` MCP tool — durable
/// patch path (ADR 0082 Phase 3). Selector is the bare form (no leading `#`).
fn save_method_expr(class: &str, selector: &str, body: &str) -> String {
    format!(
        "{} compile: #{} source: \"{}\"",
        class,
        selector,
        escape_string_literal(body),
    )
}

/// Build the Beamtalk expression for the `try_method` MCP tool — ephemeral
/// patch path (ADR 0082 Phase 3). Selector is the bare form (no leading `#`).
fn try_method_expr(class: &str, selector: &str, body: &str) -> String {
    format!(
        "{} tryCompile: #{} source: \"{}\"",
        class,
        selector,
        escape_string_literal(body),
    )
}

/// Check a REPL response for errors and return early with a formatted error result.
///
/// The `$fallback` string is used when the response has no error message.
/// Uses the shared `format_diagnostic` helper (BT-2086) so MCP error rendering
/// stays in lockstep with CLI output.
macro_rules! check_response {
    ($response:expr, $fallback:expr) => {
        if $response.is_error() {
            let msg = $response.error_message().unwrap_or($fallback);
            return Ok(error_result(fmt::format_diagnostic(
                &FmtDiagnostic::new(msg),
                MCP_OUTPUT_MODE,
            )));
        }
    };
}

// --- Tool parameter types ---

/// Parameters for the `evaluate` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct EvaluateParams {
    /// Beamtalk expression to evaluate.
    #[schemars(description = "A beamtalk expression to evaluate in the REPL")]
    pub code: String,
    /// If true, return per-statement step values instead of a single result (BT-1238).
    /// Each step has `src` (the source text) and `value` (the evaluated result).
    #[schemars(
        description = "If true, return per-statement trace steps instead of a single result value. Each step includes the source text and the evaluated value."
    )]
    pub trace: Option<bool>,
}

/// Parameters for the `complete` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct CompleteParams {
    /// Beamtalk expression up to the cursor position to get completions for.
    /// For chain completions (e.g. `"hello" size `) include the full expression
    /// up to where the cursor is placed.
    #[schemars(
        description = "Beamtalk expression up to the cursor position to get completions for"
    )]
    pub code: String,
    /// Cursor position (byte offset into `code`). Defaults to `code.len()` if absent.
    /// The `code` string is truncated to this offset before forwarding to the REPL,
    /// enabling correct completions when the cursor is mid-expression.
    #[schemars(
        description = "Cursor position as byte offset into code. Omit to complete at end of input."
    )]
    pub cursor: Option<usize>,
}

/// Parameters for the `load_file` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct LoadFileParams {
    /// Path to a .bt source file to load.
    #[schemars(description = "Path to a .bt source file to load into the workspace")]
    pub path: String,
}

/// Parameters for the `inspect` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct InspectParams {
    /// Actor PID to inspect (e.g. "<0.123.0>").
    #[schemars(description = "Erlang PID of the actor to inspect, e.g. \"<0.123.0>\"")]
    pub actor: String,
}

/// Parameters for the `supervision_tree` MCP tool (ADR 0092).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct SupervisionTreeParams {
    /// Scope: "default" (the workspace application tree with runtime plumbing
    /// filtered — the safe Read view) or "system" (everything, including
    /// runtime internals and foreign processes — privileged).
    #[serde(default)]
    #[schemars(
        description = "Scope: \"default\" (workspace tree, runtime plumbing filtered — the default) or \"system\" (everything, incl. runtime internals — privileged). Defaults to \"default\"."
    )]
    pub scope: Option<String>,
}

/// Parameters for the `reload_class` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ReloadClassParams {
    /// Class name to reload.
    #[schemars(description = "Name of the beamtalk class to reload (hot code reload)")]
    pub class: String,
}

/// Parameters for the `docs` MCP tool.
///
/// Provide exactly one of `class` (Beamtalk class) or `erlang_module` (Erlang module).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct DocsParams {
    /// Beamtalk class name to get documentation for.
    #[schemars(
        description = "Name of the beamtalk class to get documentation for. Mutually exclusive with erlang_module."
    )]
    pub class: Option<String>,
    /// Erlang module name to get documentation for.
    #[schemars(
        description = "Name of an Erlang module to get FFI documentation for (e.g. \"lists\", \"maps\"). Mutually exclusive with class."
    )]
    pub erlang_module: Option<String>,
    /// Optional selector to get docs for a specific method or function.
    #[schemars(description = "Optional method/function selector to get documentation for")]
    pub selector: Option<String>,
}

/// Parameters for the `show_codegen` MCP tool.
///
/// Provide either `code` (expression snippet) or `class` (loaded class name).
/// When both are provided, `class` takes priority.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ShowCodegenParams {
    /// Beamtalk code snippet to compile and show the generated Core Erlang for.
    /// Used when `class` is not provided.
    #[schemars(
        description = "Beamtalk code snippet to compile and show generated Core Erlang for. Used when 'class' is not provided."
    )]
    pub code: Option<String>,
    /// Name of a loaded Beamtalk class to inspect. Takes priority over `code` when both provided.
    #[schemars(
        description = "Name of a loaded Beamtalk class to show generated Core Erlang for. Takes priority over 'code' when both are provided."
    )]
    pub class: Option<String>,
    /// Optional method selector when using `class`. If omitted, shows the full class.
    #[schemars(
        description = "Optional method selector when inspecting a class. Narrows context but full class Core Erlang is returned."
    )]
    pub selector: Option<String>,
}

/// Parameters for the `test` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct TestParams {
    /// Optional class name to run tests for. Mutually exclusive with `file`.
    #[schemars(
        description = "Optional TestCase class name. Mutually exclusive with 'file'. If omitted, runs all BUnit tests."
    )]
    pub class: Option<String>,
    /// Optional path to a `.bt` test file. Mutually exclusive with `class`.
    #[schemars(
        description = "Optional path to a .bt source file (e.g. 'test/foo_test.bt'). Discovers and runs all TestCase subclasses defined in that file. Mutually exclusive with 'class'."
    )]
    pub file: Option<String>,
}

/// Parameters for the `unload` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct UnloadParams {
    /// Name of the class to unload from the workspace.
    #[schemars(description = "Name of the beamtalk class to unload from the workspace")]
    pub class: String,
}

/// Parameters for the `load_project` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct LoadProjectParams {
    /// Path to a directory containing `beamtalk.toml`.
    #[schemars(
        description = "Path to the project directory containing beamtalk.toml. Use \".\" for the current directory."
    )]
    pub path: String,
    /// If true, also load files from the `test/` directory.
    #[schemars(
        description = "Whether to also load test files from the test/ directory. Defaults to false."
    )]
    pub include_tests: Option<bool>,
    /// If true, bypass incremental detection and force recompilation of all files.
    #[schemars(
        description = "Force full recompilation of all files, bypassing incremental change detection. Defaults to false."
    )]
    pub force: Option<bool>,
}

/// Parameters for the `lint` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct LintParams {
    /// Path to a `.bt` source file or directory to lint. Defaults to `.`.
    #[schemars(
        description = "Path to a .bt source file or directory to lint. Defaults to the current directory."
    )]
    pub path: Option<String>,
}

/// Parameters for the `search_examples` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct SearchExamplesParams {
    /// Search query — keywords or natural language describing what you're looking for.
    #[schemars(
        description = "Keywords or natural language query (e.g. 'closures', 'actor state', 'pattern matching')"
    )]
    pub query: String,
    /// Maximum number of results (default 5, max 20).
    #[schemars(description = "Maximum results to return. Default 5, max 20.")]
    pub limit: Option<usize>,
}

/// Parameters for the `search_classes` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct SearchClassesParams {
    /// Search query — keywords or concept to search for classes.
    #[schemars(
        description = "Keywords or natural language query to search for classes (e.g. 'environment variable', 'subprocess', 'immutable', 'http', 'collection')"
    )]
    pub query: String,
    /// Maximum number of results (default 5, max 20).
    #[schemars(description = "Maximum results to return. Default 5, max 20.")]
    pub limit: Option<usize>,
}

/// Parameters for the `list_classes` MCP tool (BT-1404).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ListClassesParams {
    /// Optional filter: a superclass name to show only subclasses of (e.g. 'Value', 'Actor'),
    /// or 'stdlib' to show only stdlib classes, or 'user' to show only user-defined classes.
    #[schemars(
        description = "Optional filter: a superclass name (e.g. 'Value', 'Actor') to show only subclasses, or 'stdlib' for built-in classes, or 'user' for user-defined classes."
    )]
    pub filter: Option<String>,
}

/// Parameters for the `get_traces` MCP tool (ADR 0069).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct GetTracesParams {
    /// Optional actor PID to filter traces (e.g. "<0.123.0>").
    #[schemars(
        description = "Optional actor PID to filter traces, e.g. \"<0.123.0>\". Omit to get all traces."
    )]
    pub actor: Option<String>,
    /// Optional method selector to filter traces (e.g. "increment").
    #[schemars(description = "Optional method selector to filter traces (e.g. \"increment\").")]
    pub selector: Option<String>,
    /// Optional class name to filter traces (e.g. "`EventStore`").
    #[schemars(description = "Optional actor class name to filter traces (e.g. \"EventStore\").")]
    pub class: Option<String>,
    /// Optional outcome to filter traces (e.g. "error", "ok", "timeout").
    #[schemars(
        description = "Optional outcome to filter traces (e.g. \"error\", \"ok\", \"timeout\")."
    )]
    pub outcome: Option<String>,
    /// Optional minimum duration in nanoseconds — only return traces slower than this.
    #[schemars(
        description = "Optional minimum duration in nanoseconds. Only returns traces with duration >= this value (e.g. 5000000 for 5ms)."
    )]
    pub min_duration_ns: Option<u64>,
    /// Maximum number of trace events to return. Traces are newest-first.
    #[schemars(description = "Maximum number of trace events to return. Traces are newest-first.")]
    pub limit: Option<u32>,
}

/// Parameters for the `actor_stats` MCP tool (ADR 0069).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ActorStatsParams {
    /// Optional actor PID to get stats for (e.g. "<0.123.0>").
    /// Omit to get stats for all actors.
    #[schemars(
        description = "Optional actor PID to get stats for, e.g. \"<0.123.0>\". Omit to get aggregate stats for all actors."
    )]
    pub actor: Option<String>,
}

/// Parameters for the `export_traces` MCP tool (ADR 0069).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ExportTracesParams {
    /// Optional file path for the export. Defaults to a timestamped file in the
    /// workspace (e.g. "traces-2026-03-22T14-30-00.json").
    #[schemars(
        description = "Optional file path for the JSON export. Defaults to a timestamped file in the current directory."
    )]
    pub path: Option<String>,
    /// Optional actor PID to filter traces (e.g. "<0.123.0>").
    #[schemars(
        description = "Optional actor PID to filter exported traces, e.g. \"<0.123.0>\". Omit to export all traces."
    )]
    pub actor: Option<String>,
    /// Optional method selector to filter exported traces (e.g. "increment").
    #[schemars(
        description = "Optional method selector to filter exported traces (e.g. \"increment\")."
    )]
    pub selector: Option<String>,
    /// Optional class name to filter exported traces (e.g. "`EventStore`").
    #[schemars(
        description = "Optional actor class name to filter exported traces (e.g. \"EventStore\")."
    )]
    pub class: Option<String>,
    /// Optional outcome to filter exported traces (e.g. "error", "ok", "timeout").
    #[schemars(
        description = "Optional outcome to filter exported traces (e.g. \"error\", \"ok\", \"timeout\")."
    )]
    pub outcome: Option<String>,
    /// Optional minimum duration in nanoseconds — only export traces slower than this.
    #[schemars(
        description = "Optional minimum duration in nanoseconds. Only exports traces with duration >= this value."
    )]
    pub min_duration_ns: Option<u64>,
    /// Maximum number of trace events to export. Traces are newest-first.
    #[schemars(description = "Maximum number of trace events to export. Traces are newest-first.")]
    pub limit: Option<u32>,
}

/// Parameters for the `package_classes` MCP tool (ADR 0070 Phase 5).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct PackageClassesParams {
    /// Name of the package to list classes for (e.g. "stdlib").
    #[schemars(
        description = "Name of the package to list classes for (e.g. \"stdlib\", \"json\")"
    )]
    pub package: String,
}

/// Parameters for the `diagnostic_summary` MCP tool (BT-2014).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct DiagnosticSummaryParams {
    /// Path to a `.bt` source file or directory. Defaults to the current directory.
    #[schemars(
        description = "Path to a .bt source file or directory. Defaults to the current directory."
    )]
    pub path: Option<String>,
}

/// Parameters for the `precheck_method` MCP tool (ADR 0105 Phase 3, BT-2782).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct PrecheckMethodParams {
    /// Name of the Beamtalk class the pending edit targets.
    #[schemars(
        description = "Name of the Beamtalk class the pending edit targets (e.g. \"Counter\")."
    )]
    pub class: String,
    /// Method selector — accepted with or without a leading `#`.
    #[schemars(
        description = "Method selector the pending edit targets (e.g. \"increment\", \"at:put:\", \"+\"). Accepted with or without a leading '#'."
    )]
    pub selector: String,
    /// Pending method source body as a String value (the right-hand side of `=>`).
    #[schemars(
        description = "The pending method body source as a String value (the right-hand side of '=>'). Nothing installs — this is a read-only pre-save check."
    )]
    pub body: String,
}

/// Parameters for the `save_method` MCP tool (ADR 0082 Phase 3, BT-2288).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct SaveMethodParams {
    /// Name of the Beamtalk class whose method should be patched.
    #[schemars(
        description = "Name of the Beamtalk class to install the method on (e.g. \"Counter\")."
    )]
    pub class: String,
    /// Method selector — accepted with or without a leading `#`.
    #[schemars(
        description = "Method selector to install (e.g. \"increment\", \"at:put:\", \"+\"). Accepted with or without a leading '#'."
    )]
    pub selector: String,
    /// Method source body as a String value (the right-hand side of `=>`).
    #[schemars(
        description = "The method body source as a String value (the right-hand side of '=>'). Passed verbatim to Behaviour>>compile:source: — no escaping required by the caller."
    )]
    pub body: String,
}

/// Parameters for the `try_method` MCP tool (ADR 0082 Phase 3, BT-2288).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct TryMethodParams {
    /// Name of the Beamtalk class whose method should be ephemerally patched.
    #[schemars(
        description = "Name of the Beamtalk class to install the ephemeral method on (e.g. \"Counter\")."
    )]
    pub class: String,
    /// Method selector — accepted with or without a leading `#`.
    #[schemars(
        description = "Method selector to install (e.g. \"increment\", \"at:put:\", \"+\"). Accepted with or without a leading '#'."
    )]
    pub selector: String,
    /// Method source body as a String value (the right-hand side of `=>`).
    #[schemars(
        description = "The method body source as a String value (the right-hand side of '=>'). Passed verbatim to Behaviour>>tryCompile:source: — no escaping required by the caller."
    )]
    pub body: String,
}

/// Parameters for the `save_class` MCP tool (ADR 0082 Phase 3, BT-2288).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct SaveClassParams {
    /// Full Beamtalk class source (e.g. `Object subclass: Greeter ...`).
    #[schemars(
        description = "Full Beamtalk class source — the entire 'Object subclass: ...' declaration including any methods. Passed verbatim to Workspace>>newClass:at:."
    )]
    pub source: String,
    /// Target path for the new class file, relative to the project root
    /// (e.g. `"src/greeter.bt"`).
    #[schemars(
        description = "Target path for the new class file, typically relative to the project root (e.g. \"src/greeter.bt\" or \"test/greeter_test.bt\"). Must lie inside the project source tree and the basename must match the declared class name."
    )]
    pub path: String,
}

/// Parameters for the `remove_method` MCP tool (ADR 0112 Phase 4, BT-3188).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct RemoveMethodParams {
    /// Name of the Beamtalk class to remove the method from.
    #[schemars(
        description = "Name of the Beamtalk class to remove the method from (e.g. \"Counter\")."
    )]
    pub class: String,
    /// Method selector — accepted with or without a leading `#`.
    #[schemars(
        description = "Method selector to remove (e.g. \"increment\", \"at:put:\", \"+\"). Accepted with or without a leading '#'."
    )]
    pub selector: String,
    /// Optional fallback expression evaluated instead of raising when the
    /// selector is not defined locally or as an extension.
    #[schemars(
        description = "Optional fallback: a Beamtalk expression (not a string value) evaluated as the body of an 'ifAbsent:' block when the selector is not found, instead of raising a selector_not_found error. Omit to raise on an absent selector."
    )]
    pub if_absent: Option<String>,
}

/// Parameters for the `remove_class` MCP tool (ADR 0113 Phase 4, BT-3210).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct RemoveClassParams {
    /// Name of the Beamtalk class to remove from the running system.
    #[schemars(
        description = "Name of the Beamtalk class to remove from the running system (e.g. \"Counter\")."
    )]
    pub class: String,
}

/// Parameters for the `rename_class` MCP tool (ADR 0114 Phase 5, BT-3276).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct RenameClassParams {
    /// Name of the Beamtalk class to rename.
    #[schemars(description = "Name of the Beamtalk class to rename (e.g. \"Counter\").")]
    pub class: String,
    /// The new name for the class.
    #[schemars(description = "The new name for the class (e.g. \"Accumulator\").")]
    pub new_name: String,
}

/// Parameters for the `rename_method` MCP tool (ADR 0114 Phase 5, BT-3276).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct RenameMethodParams {
    /// Name of the Beamtalk class whose method should be renamed.
    #[schemars(
        description = "Name of the Beamtalk class whose method should be renamed (e.g. \"Counter\"). Instance-side only — a class-side rename needs a direct 'Counter class renameSelector: #old to: #new' via 'evaluate'."
    )]
    pub class: String,
    /// Current method selector — accepted with or without a leading `#`.
    #[schemars(
        description = "Current method selector to rename (e.g. \"increment\", \"at:put:\"). Accepted with or without a leading '#'."
    )]
    pub selector: String,
    /// New selector — accepted with or without a leading `#`.
    #[schemars(
        description = "The new selector (e.g. \"incrementBy\"). Accepted with or without a leading '#'."
    )]
    pub new_selector: String,
}

/// Parameters for the `flush` MCP tool (ADR 0082 Phase 3, BT-2288).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct FlushParams {
    /// Optional class name to scope the flush to that class's pending entries.
    #[schemars(
        description = "Optional class name to scope the flush (compiles to 'Workspace flush: ClassName'). Mutually exclusive with 'file' and 'kind'. Omit all three to flush every pending durable change."
    )]
    pub class: Option<String>,
    /// Optional file path to scope the flush to entries against that file.
    #[schemars(
        description = "Optional source file path to scope the flush (compiles to 'Workspace flush: #{ #file => \"path\" }'). Mutually exclusive with 'class' and 'kind'."
    )]
    pub file: Option<String>,
    /// Optional change-kind symbol (e.g. `"new-class"`) to scope the flush.
    #[schemars(
        description = "Optional change-kind symbol to scope the flush, e.g. \"new-class\" (compiles to 'Workspace flush: #'new-class'). Mutually exclusive with 'class' and 'file'."
    )]
    pub kind: Option<String>,
    /// Required-when-applicable Tier-2 confirmation (ADR 0113 Phase 2/4,
    /// BT-3207/BT-3210). Must be `true` to also apply pending `remove-class`
    /// (destructive, file-deleting) entries; omitted or `false` flushes only
    /// Tier 1 (patches, new-class, remove-method) exactly as before.
    #[schemars(
        description = "Set to true to ALSO apply pending destructive 'remove-class' entries (which delete a .bt file) within this flush's scope — omitted or false flushes only non-destructive Tier-1 entries (patches, new-class, remove-method), and any pending remove-class entry is reported in the result as 'skipped: destructive'. There is no default: an agent cannot delete a file by omission. With no 'class'/'file'/'kind' filter, true compiles to the unscoped 'Workspace flushIncludingDestructive'; with a filter, it compiles to 'Workspace flush: <filter> confirmDestructive: true'. (ADR 0113 Phase 2/4, BT-3207/BT-3210.)"
    )]
    pub confirm_destructive: Option<bool>,
}

// --- MCP Tool implementations ---

#[tool_router]
impl BeamtalkMcp {
    /// Create a new MCP server backed by the provided REPL client.
    pub fn new(client: Arc<ReplClient>) -> Self {
        Self {
            client,
            tool_router: Self::tool_router(),
        }
    }

    /// Evaluate a beamtalk expression in the live REPL.
    #[tool(
        description = "Evaluate a beamtalk expression in the live REPL. Returns the result value and any stdout output. Use this to interact with beamtalk objects, call methods, spawn actors, and explore the live system. Set trace=true to get per-statement step values instead of a single result."
    )]
    async fn evaluate(
        &self,
        Parameters(params): Parameters<EvaluateParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("evaluate");
        let use_trace = params.trace.unwrap_or(false);
        tracing::debug!(
            tool = "evaluate",
            code_len = params.code.len(),
            trace = use_trace,
            "tool invoked"
        );
        let response = self
            .client
            .evaluate_with_options(&params.code, use_trace)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Unknown error");
            let mut diag = FmtDiagnostic::new(msg);
            if let Some(line) = response.line {
                diag = diag.with_line(line);
            }
            if let Some(ref hint) = response.hint {
                diag = diag.with_hint(hint);
            }
            return Ok(error_result(fmt::format_diagnostic(&diag, MCP_OUTPUT_MODE)));
        }

        let mut parts = Vec::new();

        if let Some(ref output) = response.output {
            if !output.is_empty() {
                parts.push(ContentBlock::text(format!("Output: {output}")));
            }
        }

        if use_trace {
            let steps = response.steps.unwrap_or_default();
            if steps.is_empty() {
                parts.push(ContentBlock::text("(no steps)"));
            } else {
                for step in &steps {
                    parts.push(ContentBlock::text(fmt::format_trace_step(
                        step,
                        MCP_OUTPUT_MODE,
                    )));
                }
            }
        } else {
            let value = response.value_string();
            if !value.is_empty() {
                parts.push(ContentBlock::text(value));
            }
        }

        if parts.is_empty() {
            parts.push(ContentBlock::text("nil"));
        }

        timer.mark_ok();
        Ok(CallToolResult::success(parts))
    }

    /// Get autocompletion suggestions for partial beamtalk input.
    #[tool(
        description = "Get autocompletion suggestions for partial beamtalk input. Returns a list of possible completions."
    )]
    async fn complete(
        &self,
        Parameters(params): Parameters<CompleteParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("complete");
        let code_len = params.code.len();
        tracing::debug!(tool = "complete", code_len, cursor = ?params.cursor, "tool invoked");
        let cursor = params.cursor.unwrap_or(code_len).min(code_len);
        // Truncate code to cursor: the REPL uses the code string as-is for
        // completions, so only the text up to the cursor should be sent.
        let code_up_to_cursor = &params.code[..cursor];
        let response = self
            .client
            .complete(code_up_to_cursor, cursor)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Completion failed");

        let completions = response.completions.unwrap_or_default();
        let text = if completions.is_empty() {
            "No completions available".to_string()
        } else {
            completions.join("\n")
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Load all `.bt` source files from a project in dependency order.
    #[tool(
        description = "Load all .bt source files from a beamtalk project (identified by beamtalk.toml) in dependency order. Reads the src/ directory and loads files so superclasses are loaded before subclasses. Returns the list of loaded classes and any per-file errors."
    )]
    async fn load_project(
        &self,
        Parameters(params): Parameters<LoadProjectParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("load_project");
        let include_tests = params.include_tests.unwrap_or(false);
        let force = params.force.unwrap_or(false);
        tracing::debug!(tool = "load_project", path = %params.path, include_tests, force, "tool invoked");
        let response = self
            .client
            .load_project(&params.path, include_tests, force)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to load project");

        let classes = response.classes.unwrap_or_default();
        let errors = response.errors;

        let mut parts = Vec::new();

        if !errors.is_empty() {
            // BT-1855: Count distinct failed file paths (a single file may
            // produce multiple diagnostics) to give an accurate summary.
            let failed_paths: std::collections::BTreeSet<&str> = errors
                .iter()
                .filter_map(|e| {
                    e.as_object()
                        .and_then(|m| m.get("path"))
                        .and_then(|v| v.as_str())
                })
                .collect();

            // Lead with failure summary so agents detect errors immediately.
            if failed_paths.is_empty() {
                // Errors without structured path info — fall back to error count.
                parts.push(ContentBlock::text(format!(
                    "Load completed with errors: {} classes loaded, {} error(s)",
                    classes.len(),
                    errors.len()
                )));
            } else {
                parts.push(ContentBlock::text(format!(
                    "Load completed with errors: {} classes loaded, {} file(s) failed [{}]",
                    classes.len(),
                    failed_paths.len(),
                    failed_paths.iter().copied().collect::<Vec<_>>().join(", ")
                )));
            }

            // Report each failure with path, line, message, and hint.
            for e in &errors {
                let msg = match e {
                    serde_json::Value::Object(map) => {
                        let path = map.get("path").and_then(|v| v.as_str()).unwrap_or("");
                        let line = map.get("line").and_then(serde_json::Value::as_u64);
                        let message = map
                            .get("message")
                            .and_then(|v| v.as_str())
                            .unwrap_or("unknown error");
                        let hint = map.get("hint").and_then(|v| v.as_str());
                        let mut s = match (path.is_empty(), line) {
                            (true, _) => message.to_string(),
                            (false, Some(l)) => format!("{path}:{l}: {message}"),
                            (false, None) => format!("{path}: {message}"),
                        };
                        if let Some(h) = hint {
                            let _ = write!(s, " (hint: {h})");
                        }
                        s
                    }
                    serde_json::Value::String(s) => s.clone(),
                    _ => e.to_string(),
                };
                parts.push(ContentBlock::text(format!("FAILED: {msg}")));
            }

            if !classes.is_empty() {
                parts.push(ContentBlock::text(format!(
                    "Loaded classes: {}",
                    classes.join(", ")
                )));
            }

            // BT-1855: Include incremental summary even when there are errors,
            // so agents know how many files were processed overall.
            if let Some(summary) = response.summary {
                parts.push(ContentBlock::text(summary));
            }

            return Ok(CallToolResult::error(parts));
        }

        if classes.is_empty() {
            parts.push(ContentBlock::text("No classes loaded"));
        } else {
            parts.push(ContentBlock::text(format!(
                "Loaded classes: {}",
                classes.join(", ")
            )));
        }

        // BT-1685: Include incremental summary if available.
        if let Some(summary) = response.summary {
            parts.push(ContentBlock::text(summary));
        }

        timer.mark_ok();
        Ok(CallToolResult::success(parts))
    }

    /// Load a `.bt` source file into the workspace.
    #[tool(
        description = "Load a .bt source file into the workspace. Compiles the file and makes its classes available. Returns the list of loaded classes."
    )]
    async fn load_file(
        &self,
        Parameters(params): Parameters<LoadFileParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("load_file");
        tracing::debug!(tool = "load_file", path = %params.path, "tool invoked");
        // Use native Beamtalk API: Workspace load: "path"
        let expr = format!(
            "Workspace load: \"{}\"",
            escape_string_literal(&params.path)
        );
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to load file");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "File loaded".to_string()
            } else {
                v
            }
        };

        let mut parts = vec![ContentBlock::text(text)];

        // Include any warnings
        if let Some(warnings) = response.warnings {
            for w in warnings {
                parts.push(ContentBlock::text(format!("Warning: {w}")));
            }
        }

        timer.mark_ok();
        Ok(CallToolResult::success(parts))
    }

    /// Inspect a running actor's state by PID.
    #[tool(
        description = "Inspect a running actor's state. Provide the actor's PID (e.g. \"<0.123.0>\") to see its current state as structured data."
    )]
    async fn inspect(
        &self,
        Parameters(params): Parameters<InspectParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("inspect");
        tracing::debug!(tool = "inspect", actor = %params.actor, "tool invoked");
        let response = self
            .client
            .inspect(&params.actor)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to inspect actor");

        let text = match response.state {
            Some(serde_json::Value::String(s)) => s,
            Some(state) => pretty_json(&state),
            None => "No state available".to_string(),
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// List all running actors in the workspace.
    #[tool(
        description = "List all running actors in the workspace. Returns each actor's PID and class."
    )]
    async fn list_actors(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("list_actors");
        tracing::debug!(tool = "list_actors", "tool invoked");
        let response = self
            .client
            .actors()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to list actors");

        let actors = response.actors.unwrap_or_default();
        let text = fmt::format_actor_list(&actors, MCP_OUTPUT_MODE);

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Snapshot the live supervision tree (ADR 0092).
    #[tool(
        description = "Snapshot the live OTP supervision tree as a flat list of node records (pid, registeredName, kind, class, childCount, isSupervisor, parentPid for adjacency). scope=\"default\" (the safe view: workspace tree, runtime plumbing filtered) or scope=\"system\" (everything, incl. runtime internals — privileged). Defaults to \"default\"."
    )]
    async fn supervision_tree(
        &self,
        Parameters(params): Parameters<SupervisionTreeParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("supervision_tree");
        let scope = params.scope.as_deref().unwrap_or("default");
        tracing::debug!(tool = "supervision_tree", scope, "tool invoked");
        // Surface the snapshot through the same term-returning eval seam every
        // surface shares (so the structured node data is identical across
        // surfaces). `system` is the privileged whole-node view; `default` is
        // the runtime-plumbing-filtered Read view (ADR 0091 / BT-2432).
        let code = if scope == "system" {
            "ProcessNavigation system tree asDictionaries"
        } else {
            "ProcessNavigation default tree asDictionaries"
        };
        let response = self
            .client
            .evaluate_with_options(code, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Unknown error");
            return Ok(error_result(fmt::format_diagnostic(
                &FmtDiagnostic::new(msg),
                MCP_OUTPUT_MODE,
            )));
        }

        // The serialised tree (`asDictionaries`) renders as a Beamtalk list of
        // node records, e.g. `#(#{#pid => "<0.200.0>", #kind => ...}, ...)`, or
        // `#()` for an empty snapshot.
        let value = response.value_string();

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(value)]))
    }

    /// List all available Beamtalk classes with one-line descriptions (BT-1404).
    #[tool(
        description = "List all available Beamtalk classes with one-line descriptions. Optionally filter by superclass (e.g. 'Value', 'Actor') or scope ('stdlib' for built-in classes, 'user' for user-defined)."
    )]
    async fn list_classes(
        &self,
        Parameters(params): Parameters<ListClassesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("list_classes");
        tracing::debug!(tool = "list_classes", filter = ?params.filter, "tool invoked");
        let response = self
            .client
            .list_classes(params.filter.as_deref())
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to list classes");

        let classes = response.class_list.unwrap_or_default();
        let text = fmt::format_class_list(&classes, MCP_OUTPUT_MODE);

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Hot-reload a class, migrating running actors to the new code.
    #[tool(
        description = "Hot-reload a class. Recompiles and reloads the class, migrating any running actors to the new code."
    )]
    async fn reload_class(
        &self,
        Parameters(params): Parameters<ReloadClassParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("reload_class");
        tracing::debug!(tool = "reload_class", class = %params.class, "tool invoked");
        validate_class_name(&params.class)?;
        // Use native Beamtalk API: ClassName reload
        let expr = format!("{} reload", params.class);
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to reload class");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "Class reloaded successfully".to_string()
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Get documentation for a Beamtalk class or Erlang module.
    #[tool(
        description = "Get documentation for a Beamtalk class or Erlang module. Provide either 'class' (Beamtalk) or 'erlang_module' (Erlang FFI), and optionally a method/function selector."
    )]
    async fn docs(
        &self,
        Parameters(params): Parameters<DocsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("docs");
        tracing::debug!(tool = "docs", class = ?params.class, erlang_module = ?params.erlang_module, selector = ?params.selector, "tool invoked");

        let expr = match (&params.class, &params.erlang_module) {
            (Some(class), None) => {
                validate_class_name(class)?;
                match params.selector.as_deref() {
                    Some(sel) => {
                        let sel = sel.strip_prefix('#').unwrap_or(sel);
                        validate_selector(sel)?;
                        format!("Beamtalk help: {class} selector: #{sel}")
                    }
                    None => format!("Beamtalk help: {class}"),
                }
            }
            (None, Some(module)) => {
                validate_erlang_module_name(module)?;
                match params.selector.as_deref() {
                    Some(sel) => {
                        let sel = sel.strip_prefix('#').unwrap_or(sel);
                        validate_selector(sel)?;
                        format!("Beamtalk erlangHelp: \"{module}\" selector: #{sel}")
                    }
                    None => format!("Beamtalk erlangHelp: \"{module}\""),
                }
            }
            (Some(_), Some(_)) => {
                return Err(rmcp::ErrorData::invalid_params(
                    "Provide either 'class' or 'erlang_module', not both.",
                    None,
                ));
            }
            (None, None) => {
                return Err(rmcp::ErrorData::invalid_params(
                    "Provide either 'class' (Beamtalk class) or 'erlang_module' (Erlang module).",
                    None,
                ));
            }
        };

        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "No documentation found");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "No documentation available".to_string()
            } else {
                v
            }
        };

        let mut call_result = CallToolResult::default();
        call_result.content = vec![ContentBlock::text(text)];

        // BT-3239: for a whole-class lookup (no per-selector filter), also
        // attach structured, divider-grouped method-category data —
        // "structured data, not just REPL text formatting" per the surface-
        // parity contract, since `docs`'s text content above is the exact
        // same rendered string the REPL's `:help` prints. Best-effort: a
        // purely runtime-loaded class (no `.bt` source file), a class with
        // no `// === Name ===` dividers, or any lookup failure along the
        // way just leaves `structured_content` unset — never an error.
        if let (Some(class), None) = (&params.class, &params.selector) {
            call_result.structured_content = doc_method_categories(&self.client, class).await;
        }

        timer.mark_ok();
        Ok(call_result)
    }

    /// Unload a class from the workspace.
    #[tool(
        description = "Unload a class from the workspace. Removes the class. Does not affect running actors."
    )]
    async fn unload(
        &self,
        Parameters(params): Parameters<UnloadParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("unload");
        validate_class_name(&params.class)?;
        tracing::debug!(tool = "unload", class = %params.class, "tool invoked");
        let response = self
            .client
            .unload(&params.class)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to unload class");

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(format!(
            "Class '{}' unloaded",
            params.class
        ))]))
    }

    /// Interrupt a running evaluation.
    #[tool(
        description = "Interrupt a running evaluation in the REPL. Use this to cancel long-running or stuck evaluations."
    )]
    async fn interrupt(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("interrupt");
        tracing::debug!(tool = "interrupt", "tool invoked");
        let response = self
            .client
            .interrupt()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to send interrupt");

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(
            "Interrupt sent",
        )]))
    }

    /// Inspect the generated Core Erlang code for a beamtalk expression or loaded class.
    #[tool(
        description = "Show the generated Core Erlang code for a beamtalk expression or loaded class. Use 'code' to compile an expression snippet, or 'class' (+ optional 'selector') to inspect a class already loaded in the session. Useful for debugging codegen and understanding compilation."
    )]
    async fn show_codegen(
        &self,
        Parameters(params): Parameters<ShowCodegenParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("show_codegen");
        tracing::debug!(tool = "show_codegen", class = ?params.class, has_code = params.code.is_some(), selector = ?params.selector, "tool invoked");
        // Normalize empty strings to absent — Some("") is not a valid class or code.
        let class = params.class.filter(|s| !s.is_empty());
        let code = params.code.filter(|s| !s.is_empty());
        let selector = params.selector.filter(|s| !s.is_empty());

        // Reject orphaned selector (selector without class).
        if selector.is_some() && class.is_none() {
            return Ok(error_result(
                "ERROR: 'selector' requires 'class' to be specified.",
            ));
        }

        let response = match (&class, &code) {
            (Some(class_str), _) => {
                self.client
                    .show_codegen_class(class_str, selector.as_deref())
                    .await
            }
            (None, Some(code_str)) => self.client.show_codegen(code_str).await,
            (None, None) => {
                return Ok(error_result(
                    "ERROR: Provide 'code' to compile an expression or 'class' to inspect a loaded class.",
                ));
            }
        }
        .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to generate Core Erlang");

        let mut parts = Vec::new();

        if let Some(core_erlang) = response.core_erlang {
            parts.push(ContentBlock::text(core_erlang));
        } else {
            parts.push(ContentBlock::text("No Core Erlang output"));
        }

        if let Some(warnings) = response.warnings {
            for w in warnings {
                parts.push(ContentBlock::text(format!("Warning: {w}")));
            }
        }

        timer.mark_ok();
        Ok(CallToolResult::success(parts))
    }

    /// Run `BUnit` tests.
    #[tool(
        description = "Run BUnit tests. Provide a class name or a file path to scope the run, or omit both to run all tests. 'class' and 'file' are mutually exclusive. Returns structured results with pass/fail counts."
    )]
    async fn test(
        &self,
        Parameters(params): Parameters<TestParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("test");
        tracing::debug!(tool = "test", class = ?params.class, file = ?params.file, "tool invoked");
        if params.class.is_some() && params.file.is_some() {
            return Ok(error_result(
                "ERROR: 'class' and 'file' parameters are mutually exclusive".to_string(),
            ));
        }
        let response = match (&params.class, &params.file) {
            (Some(class), _) => self.client.test_class(class).await,
            (_, Some(file)) => self.client.test_file(file).await,
            _ => self.client.test_all().await,
        }
        .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Test execution failed");

        let has_failures = response.has_test_error();

        let text = match response.results {
            Some(results) => fmt::format_test_result(&results, MCP_OUTPUT_MODE),
            None => "Tests completed (no structured results)".to_string(),
        };

        if has_failures {
            return Ok(error_result(format!("TEST FAILURES:\n{text}")));
        }

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Run lint checks on a `.bt` source file or directory.
    #[tool(
        description = "Run style and redundancy lint checks on a .bt source file or directory. Returns structured diagnostics with file, line, message, and severity. Use path=. for the current directory."
    )]
    async fn lint(
        &self,
        Parameters(params): Parameters<LintParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("lint");
        let path = params.path.unwrap_or_else(|| ".".to_string());
        tracing::debug!(tool = "lint", path = %path, "tool invoked");
        // Run blocking I/O and CPU-bound parsing off the Tokio worker thread.
        let result = tokio::task::spawn_blocking(move || run_lint_structured(&path))
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e.to_string(), None))?;
        let has_errors = !result.errors.is_empty();
        let text = serde_json::to_string_pretty(&result).unwrap_or_else(|_| format!("{result:?}"));
        let structured = serde_json::to_value(&result).ok();
        let mut call_result = CallToolResult::default();
        call_result.content = vec![ContentBlock::text(text)];
        call_result.structured_content = structured;
        if has_errors {
            call_result.is_error = Some(true);
        } else {
            timer.mark_ok();
        }
        Ok(call_result)
    }

    /// Return aggregated diagnostic counts for a Beamtalk package without per-diagnostic detail.
    #[tool(
        description = "Return a diagnostic summary (counts by category and severity) for a Beamtalk package or file. \
                        Also includes type-coverage statistics. Use this to monitor typing progress \
                        without parsing full lint output. Works offline — no REPL connection needed."
    )]
    async fn diagnostic_summary(
        &self,
        Parameters(params): Parameters<DiagnosticSummaryParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("diagnostic_summary");
        let path = params.path.unwrap_or_else(|| ".".to_string());
        tracing::debug!(tool = "diagnostic_summary", path = %path, "tool invoked");

        let result = tokio::task::spawn_blocking(move || compute_diagnostic_summary(&path))
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e.to_string(), None))?;

        let text = serde_json::to_string_pretty(&result).unwrap_or_else(|_| format!("{result:?}"));
        let structured = serde_json::to_value(&result).ok();
        let mut call_result = CallToolResult::default();
        call_result.content = vec![ContentBlock::text(text)];
        call_result.structured_content = structured;
        timer.mark_ok();
        Ok(call_result)
    }

    /// Search the bundled example corpus for Beamtalk code examples.
    #[tool(
        description = "Search for Beamtalk code examples by keyword or topic. Returns matching examples with source code, explanation, and tags. Use this to find idiomatic patterns, syntax examples, and working code before writing .bt files. Works offline — no REPL connection needed."
    )]
    async fn search_examples(
        &self,
        Parameters(params): Parameters<SearchExamplesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("search_examples");
        tracing::debug!(tool = "search_examples", limit = ?params.limit, "tool invoked");
        let start = std::time::Instant::now();

        // BT-1722: Aggregate package corpora with the bundled corpus.
        let (pkg_corpora, _) = discover_package_corpora();
        let merged;
        let corpus_ref = if pkg_corpora.is_empty() {
            &*beamtalk_examples::corpus::CORPUS
        } else {
            merged =
                beamtalk_examples::merge_corpora(&beamtalk_examples::corpus::CORPUS, &pkg_corpora);
            &merged
        };
        let results = beamtalk_examples::search_in(corpus_ref, &params.query, params.limit);
        let duration_us = start.elapsed().as_micros();

        let result_count = results.len();
        let top_score = results.first().map_or(0, |r| r.score);

        // Telemetry: hash the query for counting unique queries without exposing content.
        let hash_bytes = Sha256::digest(params.query.as_bytes());
        let query_hash = hash_bytes
            .iter()
            .fold(String::with_capacity(64), |mut acc, b| {
                use std::fmt::Write as _;
                let _ = write!(acc, "{b:02x}");
                acc
            });

        tracing::info!(
            query_hash = %query_hash,
            result_count = result_count,
            top_score = top_score,
            duration_us = duration_us,
            "search_examples"
        );
        tracing::debug!(query = %params.query, "search_examples query");

        if results.is_empty() {
            timer.mark_ok();
            return Ok(CallToolResult::success(vec![ContentBlock::text(
                "No examples found for that query. Try different keywords — e.g. 'closures', 'actor state', 'collections'.",
            )]));
        }

        let text = results
            .iter()
            .map(|r| {
                format!(
                    "## {} (score: {})\n**Category:** {} | **Tags:** {}\n\n```beamtalk\n{}\n```\n\n{}\n",
                    r.entry.title,
                    r.score,
                    r.entry.category,
                    r.entry.tags.join(", "),
                    r.entry.source,
                    r.entry.explanation,
                )
            })
            .collect::<Vec<_>>()
            .join("\n---\n\n");

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Search for Beamtalk classes by keyword or concept.
    #[tool(
        description = "Search for Beamtalk classes by keyword, concept, or method name. Returns matching classes with their superclass, description, and key methods. Use this to discover which class provides a capability before using 'docs' for full details. Works offline — no REPL connection needed."
    )]
    async fn search_classes(
        &self,
        Parameters(params): Parameters<SearchClassesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("search_classes");
        tracing::debug!(tool = "search_classes", limit = ?params.limit, "tool invoked");
        let start = std::time::Instant::now();

        // BT-1722: Aggregate package class corpora with the bundled corpus.
        let (_, pkg_class_corpora) = discover_package_corpora();
        let merged;
        let corpus_ref = if pkg_class_corpora.is_empty() {
            &*beamtalk_examples::class_corpus::CLASS_CORPUS
        } else {
            merged = beamtalk_examples::merge_class_corpora(
                &beamtalk_examples::class_corpus::CLASS_CORPUS,
                &pkg_class_corpora,
            );
            &merged
        };
        let results = beamtalk_examples::search_classes_in(corpus_ref, &params.query, params.limit);
        let duration_us = start.elapsed().as_micros();

        let result_count = results.len();
        let top_score = results.first().map_or(0, |r| r.score);

        let hash_bytes = Sha256::digest(params.query.as_bytes());
        let query_hash = hash_bytes
            .iter()
            .fold(String::with_capacity(64), |mut acc, b| {
                use std::fmt::Write as _;
                let _ = write!(acc, "{b:02x}");
                acc
            });

        tracing::info!(
            query_hash = %query_hash,
            result_count = result_count,
            top_score = top_score,
            duration_us = duration_us,
            "search_classes"
        );
        tracing::debug!(
            query_hash = %query_hash,
            query_len = params.query.len(),
            "search_classes query"
        );

        if results.is_empty() {
            timer.mark_ok();
            return Ok(CallToolResult::success(vec![ContentBlock::text(
                "No classes found for that query. Try different keywords — e.g. 'http', 'collection', 'file', 'actor', 'subprocess'.",
            )]));
        }

        let text = results
            .iter()
            .map(|r| {
                let sealed = if r.entry.is_sealed { " (sealed)" } else { "" };
                let abstract_ = if r.entry.is_abstract {
                    " (abstract)"
                } else {
                    ""
                };
                let doc = r
                    .entry
                    .doc
                    .as_deref()
                    .unwrap_or("No description available.");
                let methods_display = if r.entry.methods.is_empty() {
                    "  (no methods)".to_string()
                } else {
                    r.entry
                        .methods
                        .iter()
                        .take(15)
                        .map(|m| format!("  {m}"))
                        .collect::<Vec<_>>()
                        .join("\n")
                };
                let more = if r.entry.methods.len() > 15 {
                    format!("\n  ... and {} more", r.entry.methods.len() - 15)
                } else {
                    String::new()
                };
                format!(
                    "## {}{}{} < {} (score: {})\n{}\n\n**Methods:**\n{}{}\n",
                    r.entry.name,
                    sealed,
                    abstract_,
                    r.entry.superclass,
                    r.score,
                    doc,
                    methods_display,
                    more,
                )
            })
            .collect::<Vec<_>>()
            .join("\n---\n\n");

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Enable actor trace event capture (ADR 0069).
    #[tool(
        description = "Enable actor trace event capture. Aggregate stats (call counts, durations) are always on; this enables detailed per-event traces. Call get-traces and actor-stats after running actor code to inspect results. Disable with evaluate(\"Tracing disable\")."
    )]
    async fn enable_tracing(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("enable_tracing");
        tracing::debug!(tool = "enable_tracing", "tool invoked");
        let response = self
            .client
            .enable_tracing()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to enable tracing");

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(
            "Tracing enabled. Run actor code, then use get-traces or actor-stats to inspect results.",
        )]))
    }

    /// Disable actor trace event capture (ADR 0069).
    #[tool(
        description = "Disable actor trace event capture. Aggregate stats remain available. Use enable-tracing to resume capture."
    )]
    async fn disable_tracing(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("disable_tracing");
        tracing::debug!(tool = "disable_tracing", "tool invoked");
        let response = self
            .client
            .disable_tracing()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to disable tracing");

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(
            "Tracing disabled. Aggregate stats remain available via actor-stats.",
        )]))
    }

    /// Get captured trace events with optional filtering (ADR 0069).
    #[tool(
        description = "Get captured trace events, newest first. Filter by actor PID, method selector, class name, outcome (ok/error/timeout), minimum duration, or limit the number of results. Returns structured JSON with actor, class, selector, duration, outcome, and timestamp for each event."
    )]
    async fn get_traces(
        &self,
        Parameters(params): Parameters<GetTracesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("get_traces");
        tracing::debug!(
            tool = "get_traces",
            actor = ?params.actor,
            selector = ?params.selector,
            class = ?params.class,
            outcome = ?params.outcome,
            min_duration_ns = ?params.min_duration_ns,
            limit = ?params.limit,
            "tool invoked"
        );

        let response = self
            .client
            .get_traces(
                params.actor.as_deref(),
                params.selector.as_deref(),
                params.class.as_deref(),
                params.outcome.as_deref(),
                params.min_duration_ns,
                params.limit,
            )
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to get traces");

        let text = match response.value {
            Some(ref v) => pretty_json(v),
            None => {
                "No traces captured. Enable tracing first with enable-tracing, then run actor code."
                    .to_string()
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Export trace events to a JSON file with optional filtering (ADR 0069).
    #[tool(
        description = "Export captured trace events to a JSON file. Filter by actor PID, method selector, class name, outcome (ok/error/timeout), minimum duration, or limit the number of events. Returns the file path and event count."
    )]
    async fn export_traces(
        &self,
        Parameters(params): Parameters<ExportTracesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("export_traces");
        tracing::debug!(
            tool = "export_traces",
            path = ?params.path,
            actor = ?params.actor,
            selector = ?params.selector,
            class = ?params.class,
            outcome = ?params.outcome,
            min_duration_ns = ?params.min_duration_ns,
            limit = ?params.limit,
            "tool invoked"
        );

        let response = self
            .client
            .export_traces(
                params.path.as_deref(),
                params.actor.as_deref(),
                params.selector.as_deref(),
                params.class.as_deref(),
                params.outcome.as_deref(),
                params.min_duration_ns,
                params.limit,
            )
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to export traces");

        let text = match response.value {
            Some(ref v) => pretty_json(v),
            None => "No traces to export. Enable tracing first with enable-tracing, then run actor code.".to_string(),
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Get aggregate actor statistics (ADR 0069).
    #[tool(
        description = "Get aggregate per-actor, per-method statistics: call count, total/average/min/max duration, error and timeout counts. Stats are always available even without tracing enabled. Optionally filter by actor PID."
    )]
    async fn actor_stats(
        &self,
        Parameters(params): Parameters<ActorStatsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("actor_stats");
        tracing::debug!(tool = "actor_stats", actor = ?params.actor, "tool invoked");

        let response = self
            .client
            .actor_stats(params.actor.as_deref())
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to get actor stats");

        let text = match response.value {
            Some(ref v) => pretty_json(v),
            None => "No stats available.".to_string(),
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Discover supported REPL operations and protocol version.
    #[tool(
        description = "Discover supported REPL operations and protocol version. Returns the list of available ops with their parameters, and version information."
    )]
    async fn describe(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("describe");
        tracing::debug!(tool = "describe", "tool invoked");
        let response = self
            .client
            .describe()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Describe failed");

        let mut parts = Vec::new();

        if let Some(ops) = response.ops {
            parts.push(ContentBlock::text(format!(
                "Supported operations:\n{}",
                pretty_json(&ops)
            )));
        }
        if let Some(versions) = response.versions {
            parts.push(ContentBlock::text(format!(
                "Versions: {}",
                pretty_json(&versions)
            )));
        }

        if parts.is_empty() {
            parts.push(ContentBlock::text("No describe information available"));
        }

        timer.mark_ok();
        Ok(CallToolResult::success(parts))
    }

    /// List all loaded Beamtalk packages with metadata.
    #[tool(
        description = "List all loaded Beamtalk packages with their versions, class counts, and dependencies. Returns package metadata from the runtime."
    )]
    async fn list_packages(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("list_packages");
        tracing::debug!(tool = "list_packages", "tool invoked");

        // Get list of package names via Package all
        let names_response = self
            .client
            .evaluate_with_options("Package all", false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(names_response, "Failed to list packages");

        let value = names_response.value_string();
        if value.is_empty() || value == "nil" {
            timer.mark_ok();
            return Ok(CallToolResult::success(vec![ContentBlock::text(
                "No packages loaded",
            )]));
        }

        // For each package, get detailed info via a single Beamtalk expression
        let detail_response = self
            .client
            .evaluate_with_options(
                "Package all collect: [:name | \
                    pkg := Package named: name. \
                    name ++ \" v\" ++ (pkg at: #version) ++ \
                    \" (\" ++ (pkg at: #classes) size printString ++ \" classes)\"\
                ]",
                false,
            )
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        let text = if detail_response.is_error() {
            // Fall back to just listing names
            value
        } else {
            detail_response.value_string()
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// List all classes belonging to a named package.
    #[tool(
        description = "List all classes belonging to a named Beamtalk package (e.g. 'stdlib'). Returns the class names as a list."
    )]
    async fn package_classes(
        &self,
        Parameters(params): Parameters<PackageClassesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("package_classes");
        let pkg = &params.package;
        tracing::debug!(tool = "package_classes", package = %pkg, "tool invoked");

        // Validate package name — no code injection
        if pkg.is_empty()
            || !pkg
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
        {
            return Err(rmcp::ErrorData::invalid_params(
                format!(
                    "Invalid package name: '{pkg}'. Must contain only alphanumeric characters, hyphens, or underscores."
                ),
                None,
            ));
        }

        let expr = format!("Package classes: \"{pkg}\"");
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to get package classes");

        let text = response.value_string();
        if text.is_empty() || text == "#()" || text == "nil" {
            timer.mark_ok();
            return Ok(CallToolResult::success(vec![ContentBlock::text(format!(
                "No classes found in package '{pkg}' (package may not be loaded)"
            ))]));
        }

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    // --- ADR 0082 Phase 3 (BT-2288): ChangeLog + flush MCP tools ---
    //
    // Each tool below is a typed front-end for an `evaluate` of a Beamtalk
    // expression. Per ADR 0082 there are no new workspace-side REPL ops; the
    // language is the API, MCP is convenience. The Beamtalk expression each
    // tool compiles to is documented next to the implementation and aligned
    // with the surface-parity map.

    /// Durably install a method on a Beamtalk class (ADR 0082 Phase 3).
    ///
    /// Compiles to `aClass compile: #selector source: body`. The patch installs
    /// in memory and appends a durable `ChangeLog` entry that can be written to
    /// disk by a subsequent `flush`.
    #[tool(
        description = "Durably install a method on a Beamtalk class. The method patch installs in memory and appends a durable ChangeLog entry that 'flush' will later write to disk (when the class is backed by an in-project .bt file). The 'body' argument is the source on the right-hand side of '=>' and is passed as a String value — no escaping or quoting required from the caller. Use 'try_method' for ephemeral spikes you may discard. (ADR 0082 Phase 3, BT-2288.)"
    )]
    async fn save_method(
        &self,
        Parameters(params): Parameters<SaveMethodParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("save_method");
        validate_class_name(&params.class)?;
        let selector = params
            .selector
            .strip_prefix('#')
            .unwrap_or(&params.selector);
        validate_selector(selector)?;
        tracing::debug!(
            tool = "save_method",
            class = %params.class,
            selector = %selector,
            body_len = params.body.len(),
            "tool invoked"
        );

        // `aClass compile: #selector source: "body"`. The body is passed as a
        // String value through eval; the runtime primitive (classCompileSource)
        // takes the body as a value, so we never re-parse it as Beamtalk source.
        let expr = save_method_expr(&params.class, selector, &params.body);
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to save method");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                format!("Method {}>>#{} saved", params.class, selector)
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Ephemerally install a method on a Beamtalk class (ADR 0082 Phase 3).
    ///
    /// Compiles to `aClass tryCompile: #selector source: body`. Installs in
    /// memory and logs an ephemeral `ChangeLog` entry that does not flush and
    /// auto-prunes on workspace restart. Promote a successful spike by calling
    /// `save_method` with the same source.
    #[tool(
        description = "Ephemerally install a method on a Beamtalk class for exploration. Installs in memory and logs an ephemeral ChangeLog entry that 'flush' skips and auto-prunes on workspace restart. Use this for spike fixes you may discard; promote a successful spike by calling 'save_method' with the same body to upgrade the intent to durable. The 'body' argument is the source on the right-hand side of '=>' and is passed as a String value — no escaping or quoting required from the caller. (ADR 0082 Phase 3, BT-2288.)"
    )]
    async fn try_method(
        &self,
        Parameters(params): Parameters<TryMethodParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("try_method");
        validate_class_name(&params.class)?;
        let selector = params
            .selector
            .strip_prefix('#')
            .unwrap_or(&params.selector);
        validate_selector(selector)?;
        tracing::debug!(
            tool = "try_method",
            class = %params.class,
            selector = %selector,
            body_len = params.body.len(),
            "tool invoked"
        );

        // `aClass tryCompile: #selector source: "body"`.
        let expr = try_method_expr(&params.class, selector, &params.body);
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to try method");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                format!(
                    "Method {}>>#{} installed (ephemeral)",
                    params.class, selector
                )
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Create a new Beamtalk class at a path (ADR 0082 Phase 3).
    ///
    /// Compiles to `Workspace newClass: source at: path`. Installs the class in
    /// memory and logs a durable `kind: #'new-class'` `ChangeLog` entry. The
    /// file is written to disk by a later `flush`.
    #[tool(
        description = "Create a new Beamtalk class. Compiles 'source' and installs the class in memory, then appends a durable 'kind: new-class' ChangeLog entry; a subsequent 'flush' writes the file to disk at 'path'. The path is typically relative to the project root (e.g. \"src/greeter.bt\") and must lie inside the project source tree; the basename must match the declared class name. Raises a structured error if the target already exists, lies outside the project tree, the class name does not match the basename, or a class with that name is already loaded — use 'save_method' against the existing class in that last case. (ADR 0082 Phase 3, BT-2288.)"
    )]
    async fn save_class(
        &self,
        Parameters(params): Parameters<SaveClassParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("save_class");
        if params.path.is_empty() {
            return Err(rmcp::ErrorData::invalid_params(
                "save_class: 'path' must not be empty.",
                None,
            ));
        }
        if params.source.is_empty() {
            return Err(rmcp::ErrorData::invalid_params(
                "save_class: 'source' must not be empty.",
                None,
            ));
        }
        tracing::debug!(
            tool = "save_class",
            path = %params.path,
            source_len = params.source.len(),
            "tool invoked"
        );

        let expr = save_class_expr(&params.source, &params.path);
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to save class");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                format!("New class queued for {}", params.path)
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Remove a method from a Beamtalk class (ADR 0112 Phase 4, BT-3188).
    ///
    /// Compiles to `aClass removeSelector: #selector` (or `aClass
    /// removeSelector: #selector ifAbsent: [...]` when `if_absent` is
    /// supplied), reusing the existing `evaluate` pathway per ADR 0082's
    /// surface-parity principle — no new workspace-side op. Removal re-exposes
    /// any inherited implementation (or an extension-shadowed local method)
    /// immediately, no restart needed, and installs unconditionally including
    /// on stdlib classes — flushability, not refusal, same as `save_method`.
    #[tool(
        description = "Remove a method from a Beamtalk class. Compiles to 'aClass removeSelector: #selector', which raises a selector_not_found error if the selector is not defined locally or as an extension (check first with includesSelector:, or supply 'if_absent'). Removing a locally-defined override re-exposes the inherited implementation immediately, no restart needed; removing an extension that shadows a same-named local method re-exposes that local method. Installs unconditionally, including on stdlib classes — whether the resulting change is flushable to disk (not whether it takes effect in memory) depends on whether the class is backed by an in-project .bt file. 'if_absent', if supplied, is a Beamtalk expression (not a string value) evaluated as a fallback instead of raising. (ADR 0112 Phase 4, BT-3188.)"
    )]
    async fn remove_method(
        &self,
        Parameters(params): Parameters<RemoveMethodParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("remove_method");
        validate_class_name(&params.class)?;
        let selector = params
            .selector
            .strip_prefix('#')
            .unwrap_or(&params.selector);
        validate_selector(selector)?;
        tracing::debug!(
            tool = "remove_method",
            class = %params.class,
            selector = %selector,
            has_if_absent = params.if_absent.is_some(),
            "tool invoked"
        );

        let expr = match params.if_absent.as_deref() {
            Some(if_absent) => remove_method_if_absent_expr(&params.class, selector, if_absent),
            None => remove_method_expr(&params.class, selector),
        };
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to remove method");

        let text = {
            let v = response.value_string();
            if v.is_empty() && params.if_absent.is_none() {
                format!("Method {}>>#{} removed", params.class, selector)
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Remove a class from the running Beamtalk system (ADR 0113 Phase 4,
    /// BT-3210).
    ///
    /// Compiles to `aClass removeFromSystem`, then looks up and returns the
    /// resulting `remove-class` `ChangeEntry`, reusing the existing
    /// `evaluate` pathway per ADR 0082's surface-parity principle — no new
    /// workspace-side op. Memory-mutating only: this tool never implicitly
    /// flushes. Reaching disk requires a distinct, explicit `flush` call with
    /// `confirm_destructive: true` (or `Workspace flushIncludingDestructive`)
    /// — the same two-step promotion idiom `try_method` → `save_method`
    /// already establishes, applied here to memory-removal vs. disk-deletion
    /// instead of ephemeral-vs-durable intent.
    #[tool(
        description = "Remove a class from the running Beamtalk system. DESTRUCTIVE (eventually): compiles to 'aClass removeFromSystem', which stops any live actors of the class, terminates its gen_server, purges the BEAM module, and appends a durable 'remove-class' ChangeLog entry — but does NOT touch disk. Refuses to remove stdlib/sealed classes or a class with live subclasses (remove those first), raising a structured error. Nothing is written to disk until a separate, later 'flush' tool call with 'confirm_destructive: true' (or 'Workspace flushIncludingDestructive'); until then the pending removal shows as 'skipped: destructive' from 'flush'/'list_changes'. Returns the resulting ChangeEntry, reporting whether it is flushable. (ADR 0113 Phase 4, BT-3210.)"
    )]
    async fn remove_class(
        &self,
        Parameters(params): Parameters<RemoveClassParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("remove_class");
        validate_class_name(&params.class)?;
        tracing::debug!(tool = "remove_class", class = %params.class, "tool invoked");

        let expr = remove_class_expr(&params.class);
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to remove class");

        let text = {
            let v = response.value_string();
            let entry = if v.is_empty() {
                format!("{} (remove-class)", params.class)
            } else {
                v
            };
            format!(
                "{entry} — removed from memory, not yet flushed to disk. Call 'flush' with confirm_destructive: true (or evaluate 'Workspace flushIncludingDestructive') to delete its source file."
            )
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Rename a class in the running Beamtalk system (ADR 0114 Phase 5,
    /// BT-3276).
    ///
    /// Compiles to `aClass renameTo: #NewName` (wraps `Behaviour>>renameTo:`,
    /// ADR 0114 Phase 2, BT-3278), reusing the existing `evaluate` pathway
    /// per ADR 0082's surface-parity principle — no new workspace-side op.
    /// Auto-rewrites every in-project cross-file reference the xref index
    /// (`referencesTo:`/`direct_subclasses:`) can find and re-registers the
    /// class under the new name immediately; refuses a stdlib/dependency
    /// class or a collision with an already-loaded class name. Like
    /// `remove_class`, memory-mutating only: this tool never implicitly
    /// flushes — reaching disk (the file move + rewritten cross-file
    /// references) requires a distinct, explicit `flush` call with
    /// `confirm_destructive: true` (or `Workspace flushIncludingDestructive`)
    /// — `rename-class` joins `remove-class` in the same Tier 2 gate (ADR
    /// 0114 "Flush" reuses ADR 0113's tier verbatim, extended to genuinely
    /// multi-file staging).
    #[tool(
        description = "Rename a class in the running Beamtalk system. Compiles to 'aClass renameTo: #NewName', which rewrites every in-project cross-file reference the cross-reference index can find (constructor/message sends, type annotations, superclass declarations, extension declarations) and re-registers the class under the new name immediately — but does NOT touch disk. Refuses a stdlib/dependency class (the xref index only covers in-project source) or a collision with an already-loaded class name, raising a structured error. Appends a durable 'rename-class' ChangeLog entry. Nothing is written to disk until a separate, later 'flush' tool call with 'confirm_destructive: true' (or 'Workspace flushIncludingDestructive'); until then the pending rename shows as 'skipped: destructive' from 'flush'/'list_changes'. Returns the renamed class. (ADR 0114 Phase 5, BT-3276.)"
    )]
    async fn rename_class(
        &self,
        Parameters(params): Parameters<RenameClassParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("rename_class");
        validate_class_name(&params.class)?;
        validate_class_name(&params.new_name)?;
        tracing::debug!(
            tool = "rename_class",
            class = %params.class,
            new_name = %params.new_name,
            "tool invoked"
        );

        let expr = rename_class_expr(&params.class, &params.new_name);
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to rename class");

        let text = format!(
            "{} — renamed in memory, not yet flushed to disk. Call 'flush' with confirm_destructive: true (or evaluate 'Workspace flushIncludingDestructive') to move its source file and rewrite cross-file references.",
            response.value_string()
        );

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Rename a method on a class in the running Beamtalk system (ADR 0114
    /// Phase 5, BT-3276).
    ///
    /// Compiles to `aClass renameSelector: #old to: #new` (wraps
    /// `Behaviour>>renameSelector:to:`, ADR 0114 Phase 3, BT-3279), reusing
    /// the existing `evaluate` pathway per ADR 0082's surface-parity
    /// principle — no new workspace-side op. Instance-side only — sent to a
    /// bare class name, this always touches the instance-side method table;
    /// a class-side rename needs a direct `Counter class renameSelector:
    /// ... to: ...` eval, the same chokepoint limitation `remove_method`
    /// documents (`docs/development/surface-parity.md`'s `remove-method`
    /// row). Auto-rewrites only the self/super sends the cross-reference
    /// index can prove are structurally safe; everything else is recorded on
    /// the resulting `ChangeLog` entry's `candidate_sites` for human/agent
    /// review, never auto-rewritten. Memory-mutating only, joining
    /// `rename-class`/`remove-class` in the same Tier 2 flush gate.
    #[tool(
        description = "Rename a method on a class in the running Beamtalk system. Compiles to 'aClass renameSelector: #old to: #new', which auto-rewrites only the self/super sends the cross-reference index can prove are structurally safe, and raises a selector_not_found error if the selector is not defined locally. Instance-side only (send 'Counter class renameSelector: #old to: #new' directly via 'evaluate' for a class-side rename). Refuses a collision with an already-defined local selector, raising a structured error. Appends a durable 'rename-method' ChangeLog entry recording confirmed 'sites' (auto-rewritten) separately from 'candidate_sites' (reported for review, never auto-rewritten) — does NOT touch disk. Nothing is written to disk until a separate, later 'flush' tool call with 'confirm_destructive: true' (or 'Workspace flushIncludingDestructive'); until then the pending rename shows as 'skipped: destructive' from 'flush'/'list_changes'. Returns the class. (ADR 0114 Phase 5, BT-3276.)"
    )]
    async fn rename_method(
        &self,
        Parameters(params): Parameters<RenameMethodParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("rename_method");
        validate_class_name(&params.class)?;
        let selector = params
            .selector
            .strip_prefix('#')
            .unwrap_or(&params.selector);
        validate_selector(selector)?;
        let new_selector = params
            .new_selector
            .strip_prefix('#')
            .unwrap_or(&params.new_selector);
        validate_selector(new_selector)?;
        tracing::debug!(
            tool = "rename_method",
            class = %params.class,
            selector = %selector,
            new_selector = %new_selector,
            "tool invoked"
        );

        let expr = rename_method_expr(&params.class, selector, new_selector);
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to rename method");

        let text = format!(
            "{} — renamed in memory, not yet flushed to disk. Call 'flush' with confirm_destructive: true (or evaluate 'Workspace flushIncludingDestructive') to write confirmed sites to disk.",
            response.value_string()
        );

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Flush pending `ChangeLog` entries to disk (ADR 0082 Phase 3;
    /// destructive tier added ADR 0113 Phase 2/4, BT-3207/BT-3210).
    ///
    /// Compiles to `Workspace flush` / `Workspace flush: <selector>` (Tier 1
    /// only), or — when `confirm_destructive: true` — `Workspace
    /// flushIncludingDestructive` / `Workspace flush: <selector>
    /// confirmDestructive: true` (Tier 1 + Tier 2). The optional `class`,
    /// `file`, and `kind` filters are mutually exclusive; at most one may be
    /// supplied.
    #[tool(
        description = "Write pending durable ChangeLog entries to disk via byte-span splice + atomic rename, with external-edit conflict detection. With no arguments, flushes every pending durable Tier-1 change ('Workspace flush'). At most one of 'class', 'file', or 'kind' may be supplied: 'class' scopes to one class ('Workspace flush: ClassName'), 'file' scopes to one source file ('Workspace flush: #{ #file => \"path\" }'), and 'kind' scopes to a ChangeEntry kind such as \"new-class\" ('Workspace flush: #'new-class'). DESTRUCTIVE when 'confirm_destructive' is true: pending 'remove-class' entries (from 'remove_class') delete their .bt file from disk — see 'confirm_destructive's own description for the required-argument gate. Returns a FlushResult summary listing files written and any conflicts; a skipped destructive entry is reported distinctly as 'skipped: destructive', not applied. (ADR 0082 Phase 3 / ADR 0113 Phase 2, BT-2288/BT-3207.)"
    )]
    async fn flush(
        &self,
        Parameters(params): Parameters<FlushParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("flush");
        // Mutual exclusivity: at most one filter.
        let provided = [
            params.class.as_deref(),
            params.file.as_deref(),
            params.kind.as_deref(),
        ]
        .into_iter()
        .filter(|v| v.is_some_and(|s| !s.is_empty()))
        .count();
        if provided > 1 {
            return Err(rmcp::ErrorData::invalid_params(
                "flush: 'class', 'file', and 'kind' are mutually exclusive — pass at most one.",
                None,
            ));
        }
        let confirm_destructive = params.confirm_destructive.unwrap_or(false);
        tracing::debug!(
            tool = "flush",
            class = ?params.class,
            file = ?params.file,
            kind = ?params.kind,
            confirm_destructive,
            "tool invoked"
        );

        let expr = match (
            params.class.as_deref().filter(|s| !s.is_empty()),
            params.file.as_deref().filter(|s| !s.is_empty()),
            params.kind.as_deref().filter(|s| !s.is_empty()),
        ) {
            (Some(class), None, None) => {
                validate_class_name(class)?;
                flush_expr_with_confirm_destructive(FlushFilter::Class(class), confirm_destructive)
            }
            (None, Some(file), None) => {
                flush_expr_with_confirm_destructive(FlushFilter::File(file), confirm_destructive)
            }
            (None, None, Some(kind)) => {
                // Allow either bare `new-class` or `#'new-class'`. We always
                // emit a quoted-symbol literal so hyphenated kinds parse.
                let bare = kind.strip_prefix('#').unwrap_or(kind);
                let bare = bare.trim_matches('\'');
                if bare.is_empty()
                    || !bare
                        .chars()
                        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
                {
                    return Err(rmcp::ErrorData::invalid_params(
                        format!(
                            "flush: 'kind' must be an identifier (letters, digits, '-' or '_'); got '{kind}'."
                        ),
                        None,
                    ));
                }
                flush_expr_with_confirm_destructive(FlushFilter::Kind(bare), confirm_destructive)
            }
            _ => flush_expr_with_confirm_destructive(FlushFilter::None, confirm_destructive),
        };

        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Flush failed");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "Flushed".to_string()
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// List pending `ChangeLog` entries (ADR 0082 Phase 3).
    ///
    /// Compiles to `Workspace changes` — returns the `ChangeLog` object's
    /// display form. Pair with `dirty_methods` for the per-class breakdown.
    #[tool(
        description = "Return the workspace ChangeLog — the navigable view of pending in-memory changes against the on-disk source files. Compiles to 'Workspace changes'. Pair with 'dirty_methods' for the per-class breakdown of dirty selectors, or 'flush' to write durable entries to disk. (ADR 0082 Phase 3, BT-2288.)"
    )]
    async fn list_changes(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("list_changes");
        tracing::debug!(tool = "list_changes", "tool invoked");

        let response = self
            .client
            .evaluate_with_options("Workspace changes", false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to list changes");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "No changes".to_string()
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Per-class dirty selectors (ADR 0082 Phase 3).
    ///
    /// Compiles to `Workspace changes dirtyMethods` — the per-class set of
    /// dirty selectors, the structured "what specifically has changed?" view.
    #[tool(
        description = "Return the per-class set of dirty selectors in the workspace — the structured 'what specifically has changed?' view. Compiles to 'Workspace changes dirtyMethods'. Pair with 'list_changes' for the full summary or 'flush' to write durable entries to disk. (ADR 0082 Phase 3, BT-2288.)"
    )]
    async fn dirty_methods(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("dirty_methods");
        tracing::debug!(tool = "dirty_methods", "tool invoked");

        let response = self
            .client
            .evaluate_with_options("Workspace changes dirtyMethods", false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to list dirty methods");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "No dirty methods".to_string()
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Pre-save advisory precheck (ADR 0105 Phase 3, BT-2782).
    ///
    /// Compiles to `aClass precheckCompile: #selector source: body`. Nothing
    /// installs and nothing is recorded to the `ChangeLog` — this is a
    /// read-only "check before save" report of would-be-stale callers.
    #[tool(
        description = "Compile a pending method edit and report would-be-stale dependents, without installing it. Compiles to 'aClass precheckCompile: #selector source: body' — the editor/LSP pre-save advisory (ADR 0105 Phase 3): non-blocking, the post-reload image check that runs automatically on 'save_method' remains the authority. The 'body' argument is the source on the right-hand side of '=>', passed as a String value — no escaping required by the caller. Returns a report Dictionary (findings/checked/totalCandidates/notChecked/capNote/checkedOwners); an edit with no type-relevant signature change reports empty. (ADR 0105 Phase 3, BT-2782.)"
    )]
    async fn precheck_method(
        &self,
        Parameters(params): Parameters<PrecheckMethodParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("precheck_method");
        validate_class_name(&params.class)?;
        let selector = params
            .selector
            .strip_prefix('#')
            .unwrap_or(&params.selector);
        validate_selector(selector)?;
        tracing::debug!(
            tool = "precheck_method",
            class = %params.class,
            selector = %selector,
            body_len = params.body.len(),
            "tool invoked"
        );

        let expr = precheck_method_expr(&params.class, selector, &params.body);
        let response = self
            .client
            .evaluate_with_options(&expr, false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to precheck method");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                format!("Precheck for {}>>#{}: no findings", params.class, selector)
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }

    /// Whole-image re-check (ADR 0105 Phase 3, BT-2782).
    ///
    /// Compiles to `Workspace recheckImage` — the "complete but unbounded"
    /// path kept out of the automatic per-reload check: re-checks every live
    /// class the workspace has a recorded source for, not just the
    /// xref-filtered dependents of one changed selector.
    #[tool(
        description = "Re-check every live class in the workspace against the current image, not just the dependents of the last reload. Compiles to 'Workspace recheckImage' — the explicit, on-demand, unbounded sweep (ADR 0105 Phase 3), as opposed to the automatic post-reload check which only re-checks xref-filtered dependents of one changed selector, capped per reload. Returns a report Dictionary (checked/stale/findings). (ADR 0105 Phase 3, BT-2782.)"
    )]
    async fn recheck_image(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("recheck_image");
        tracing::debug!(tool = "recheck_image", "tool invoked");

        let response = self
            .client
            .evaluate_with_options("Workspace recheckImage", false)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to recheck image");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "Whole-image re-check: no findings".to_string()
            } else {
                v
            }
        };

        timer.mark_ok();
        Ok(CallToolResult::success(vec![ContentBlock::text(text)]))
    }
}

// --- Lint helpers ---

/// A single lint diagnostic in structured form.
///
/// `line` is `None` for file-level errors (e.g. unreadable path, non-`.bt` file)
/// where there is no specific source location.  For diagnostics derived from
/// source text it is a 1-indexed line number.
#[derive(Debug, serde::Serialize)]
struct LintDiagnostic {
    file: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    line: Option<u32>,
    message: String,
    severity: &'static str,
}

/// Structured result returned by the `lint` MCP tool.
#[derive(Debug, serde::Serialize)]
struct LintResult {
    warnings: Vec<LintDiagnostic>,
    errors: Vec<LintDiagnostic>,
    total: usize,
}

/// BT-2152: Run the shared three-step lint analysis pipeline for a single
/// module. Callers pre-filter `parse_diags` per their severity requirements
/// and pass them in; this helper appends lint-pass results, runs semantic
/// analysis with cross-file class context, filters analysis diagnostics by
/// `category.is_some()`, applies `@expect` directives, and returns the
/// resulting diagnostics together with the `ClassHierarchy` (used by
/// `compute_diagnostic_summary` for type inference).
///
/// `has_package_dependencies` mirrors `beamtalk lint`'s
/// `CompilerOptions::has_package_dependencies` (BT-2794/BT-2823): true when
/// the project's manifest declares `[dependencies]`, regardless of whether
/// any of them could be resolved on disk.
///
/// `native_type_registry` (BT-2858) mirrors `beamtalk lint`'s FFI type
/// registry (BT-2851/BT-2134): when `Some`, `(Erlang m) f:` calls get return
/// type inference and argument-type checks from the registry instead of
/// falling back to `Dynamic(UntypedFfi)` — the same registry `beamtalk
/// build`/`beamtalk lint` use, so MCP `lint`/`diagnostic_summary` never
/// diverge from them on which Erlang calls are seen as typed.
///
/// `current_package` (BT-2921) mirrors `beamtalk lint`'s
/// `CompilerOptions::current_package`: when `Some`, `check_class_visibility`/
/// `check_alias_leaked_visibility` (E0401/E0402/E0403) actually run — they
/// are gated on `current_package: Some(_)` and silently emit zero
/// diagnostics otherwise.
///
/// `source` (BT-3257) is the module's raw source text — mirroring
/// `queries::diagnostic_provider::compute_project_diagnostics_with_analysis`
/// (BT-3240) and `beamtalk lint`'s `collect_diagnostics`: needed so the
/// near-miss `// === Name ===` divider check can scan `source` directly
/// (`beamtalk_core::near_miss_divider::check_near_miss_dividers`) instead of relying on
/// the AST's `Comment::span`, which is actually the *following
/// declaration's* span, not the comment's own.
///
/// `is_stub_file` (BT-3398) mirrors the LSP's `ProjectIndex::is_stub_file`
/// fix (review follow-up on #3679): this crate builds its own
/// `AnalysisContext` directly rather than going through
/// `beamtalk-language-service`'s shared `diagnostic_provider.rs`, so it was
/// never touched by that fix and always analysed every file — including a
/// legitimate `stubs/lists.bt`'s `declare native:` blocks — as if it lived
/// outside `stubs/`. Callers derive this from the file path being analysed
/// (`beamtalk_project::package::is_under_stubs_dir`, called once per
/// top-level `path` argument below rather than by this per-file helper).
///
/// `file_stem` (BT-3431) is the target file's basename without extension,
/// passed to `check_class_file_name_agreement` so MCP `lint`/
/// `diagnostic_summary` report the same file-name/class-name mismatch
/// `beamtalk build`/`beamtalk lint`/the LSP do — `None` for callers with no
/// real file backing the module skips the check.
#[allow(clippy::too_many_arguments)] // BT-3398 added is_stub_file; each param is load-bearing context, same as `beamtalk lint`'s `collect_diagnostics`.
fn run_module_analysis(
    module: &beamtalk_core::ast::Module,
    source: &str,
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    mut diags: Vec<beamtalk_core::source_analysis::Diagnostic>,
    has_package_dependencies: bool,
    native_type_registry: Option<
        std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    >,
    current_package: Option<&str>,
    is_stub_file: bool,
    file_stem: Option<&str>,
) -> (
    Vec<beamtalk_core::source_analysis::Diagnostic>,
    beamtalk_core::semantic_analysis::ClassHierarchy,
) {
    use beamtalk_core::semantic_analysis::ClassHierarchy;

    diags.extend(beamtalk_lint::run_lint_passes(module));

    let cross_file_classes = ClassHierarchy::cross_file_class_infos(all_class_infos, module);
    let options = beamtalk_core::CompilerOptions {
        has_package_dependencies,
        current_package: current_package.map(str::to_string),
        ..Default::default()
    };
    let analysis_ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
        .with_options(&options)
        .with_pre_loaded_classes(cross_file_classes)
        .with_native_type_registry(native_type_registry)
        .with_is_stub_file(is_stub_file);
    let analysis_result = beamtalk_core::semantic_analysis::analyse_full(module, analysis_ctx);
    diags.extend(
        analysis_result
            .diagnostics
            .into_iter()
            .filter(|d| d.category.is_some()),
    );

    // BT-3431: Validate the file name agrees with the class it declares —
    // `analyse_full` doesn't run this check itself (see
    // `check_class_file_name_agreement`'s doc), so it must be called
    // explicitly here, mirroring `compute_project_diagnostics_with_analysis`.
    diags.extend(
        beamtalk_core::semantic_analysis::module_validator::check_class_file_name_agreement(
            module, file_stem,
        ),
    );

    beamtalk_language_service::queries::diagnostic_provider::apply_expect_directives(
        module, &mut diags,
    );

    // BT-3257: mirrors `compute_project_diagnostics_with_analysis`'s
    // placement — appended after `apply_expect_directives` because a
    // near-miss-divider comment's span (the comment's own line) can never
    // be contained in any `@expect`-annotated declaration's target span, so
    // running it through that pass first would be a no-op at best. See that
    // function's BT-3240 comment for the full reasoning.
    beamtalk_core::near_miss_divider::check_near_miss_dividers(source, &mut diags);

    (diags, analysis_result.class_hierarchy)
}

/// BT-2858: Build the Erlang FFI native-type registry for `path`'s package,
/// the same way `beamtalk lint` does (BT-2134/BT-2851's `extract_type_specs`,
/// now shared via `beamtalk_cli::native_type_specs`) — rather than reading a
/// possibly-absent/stale on-disk `_build/type_cache/` written by a *previous*
/// `beamtalk build`. Returns `None` outside a manifest-backed package or when
/// extraction finds no `.beam` files (e.g. runtime not yet compiled).
fn build_native_type_registry(
    path: &str,
) -> Option<std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>> {
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path))?;
    let project_root = camino::Utf8PathBuf::from_path_buf(project_root).ok()?;
    let layout = beamtalk_cli::build_layout::BuildLayout::new(&project_root);
    beamtalk_cli::native_type_specs::extract_project_type_specs(&layout).map(std::sync::Arc::new)
}

/// BT-2014: Compute a diagnostic summary (counts + type coverage) for a path.
///
/// Runs the same two-pass parse + semantic-analysis pipeline as `beamtalk lint`,
/// aggregates all diagnostics via the shared `DiagnosticSummary` type, and
/// additionally computes type-coverage statistics. Returns a JSON-serializable
/// value suitable for direct MCP tool output.
#[allow(clippy::too_many_lines)] // Multi-pass pipeline is inherently sequential.
fn compute_diagnostic_summary(path: &str) -> serde_json::Value {
    use beamtalk_core::semantic_analysis::{ClassHierarchy, CoverageReport, infer_types};
    use beamtalk_core::source_analysis::{DiagnosticSummary, category_name};

    let source_files = match resolve_source_files(path) {
        Ok(files) => files,
        Err(result) => {
            // BT-2031: Surface the actual error (permission, IO, path-not-found)
            // instead of collapsing to a generic "no files found" message. Include
            // the file context from the diagnostic since the message alone may not
            // name the offending path.
            let error_msg = result.errors.first().map_or_else(
                || format!("No .bt source files found in '{path}'"),
                |e| {
                    if e.file.is_empty() {
                        format!("{}: {}", path, e.message)
                    } else {
                        format!("{}: {}", e.file, e.message)
                    }
                },
            );
            return serde_json::json!({
                "error": error_msg,
                "files_checked": 0,
                "total": 0,
            });
        }
    };

    // BT-2052: Determine the full extraction set (package-wide src/ + test/)
    // so cross-file class references resolve correctly.
    let (extraction_files, target_set) = resolve_extraction_files(path, &source_files);

    // BT-2921: Resolve the current package once, mirroring `beamtalk lint`,
    // so E0401/E0402/E0403 visibility checks fire the same way in MCP.
    let current_package = resolve_current_package(path);

    // BT-3398: Resolve the package root once so each file's `is_stub_file`
    // check below (`beamtalk_project::package::is_under_stubs_dir`) doesn't
    // re-walk ancestors per file — mirrors `current_package`/
    // `native_type_registry`'s own one-time-per-call resolution above.
    // `None` outside a manifest-backed package, matching
    // `AnalysisContext::is_stub_file`'s own conservative default (no file is
    // ever treated as a stub without a known project root).
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path));

    // Pass 1: Parse all files and extract class metadata.
    let mut all_class_infos = Vec::new();
    let mut parsed_files = Vec::new();
    let mut unreadable_files: Vec<String> = Vec::new();
    let mut unreadable_target_files: Vec<String> = Vec::new();

    for file in &extraction_files {
        let Ok(source) = std::fs::read_to_string(file) else {
            // BT-2067: Track unreadable target files separately so the caller
            // sees a clear error instead of a deceptively-clean `files_checked=0`
            // summary. BT-2056: Unreadable package-only files produce a softer
            // warning since cross-file class extraction may be incomplete but
            // the targets themselves were still checked.
            let canonical = canonicalize_or_clone(file);
            if target_set.contains(&canonical) {
                unreadable_target_files.push(file.to_string_lossy().into_owned());
            } else {
                unreadable_files.push(file.to_string_lossy().into_owned());
            }
            continue;
        };
        let file_str = file.to_string_lossy().into_owned();
        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);
        let mut class_infos = ClassHierarchy::extract_class_infos(&module);
        // BT-2921: Stamp the package per-file, same as build's/lint's Pass 1 —
        // without this, a same-package class defined in a sibling file is
        // indistinguishable from a builtin/REPL class and never flagged as a
        // leak by `check_class_visibility`.
        if let Some(pkg) = current_package.as_deref() {
            ClassHierarchy::stamp_package_on_infos(&mut class_infos, pkg);
        }
        all_class_infos.extend(class_infos);

        let canonical = canonicalize_or_clone(file);
        if target_set.contains(&canonical) {
            parsed_files.push((file_str, source, module, parse_diags));
        }
    }

    // BT-2823: Merge dependency class metadata so cross-file references to
    // classes defined only in a git/path dependency (declared in
    // beamtalk.toml) resolve the same way `beamtalk build` does.
    let has_package_dependencies = merge_dependency_class_infos(path, &mut all_class_infos);

    // BT-2858: Populate the FFI type registry the same way `beamtalk lint` does.
    let native_type_registry = build_native_type_registry(path);

    // Pass 2: Analyse each file and collect diagnostics + coverage.
    let mut all_diags = Vec::new();
    let mut coverage = CoverageReport {
        classes: Vec::new(),
        dynamic_entries: Vec::new(),
        total_expressions: 0,
        typed_expressions: 0,
    };

    for (file_str, source, module, parse_diags) in &parsed_files {
        // Pre-filter to lint-severity parse diagnostics (compute_diagnostic_summary
        // intentionally drops parse errors/warnings here — they're surfaced via
        // other channels).
        let initial_diags: Vec<_> = parse_diags
            .iter()
            .filter(|d| d.severity == Severity::Lint)
            .cloned()
            .collect();

        let is_stub_file = project_root.as_deref().is_some_and(|root| {
            beamtalk_project::package::is_under_stubs_dir(root, std::path::Path::new(file_str))
        });
        let (file_diags, class_hierarchy) = run_module_analysis(
            module,
            source,
            &all_class_infos,
            initial_diags,
            has_package_dependencies,
            native_type_registry.clone(),
            current_package.as_deref(),
            is_stub_file,
            std::path::Path::new(file_str)
                .file_stem()
                .and_then(std::ffi::OsStr::to_str),
        );

        all_diags.extend(file_diags);

        // Type coverage.
        let type_map = infer_types(module, &class_hierarchy, native_type_registry.as_deref());
        let file_report = CoverageReport::from_module(module, &type_map, file_str, false);
        coverage.merge(file_report);
    }

    // BT-2031: Count only files that were actually read and analysed,
    // not all resolved files (some may have been unreadable).
    let files_checked = parsed_files.len();
    let summary = DiagnosticSummary::from_diagnostics(&all_diags, files_checked);
    let totals = summary.totals_by_severity();

    let mut by_category = serde_json::Map::new();
    for (cat, counts) in &summary.by_category {
        by_category.insert(
            category_name(*cat).to_string(),
            serde_json::json!({
                "error": counts.error,
                "warning": counts.warning,
                "lint": counts.lint,
                "hint": counts.hint,
                "total": counts.total(),
            }),
        );
    }

    let dynamic_pct = if coverage.total_expressions > 0 {
        let typed_pct = coverage.coverage_percent();
        ((100.0 - typed_pct) * 10.0).round() / 10.0
    } else {
        0.0
    };

    // BT-2056: Include unreadable package files in the output so the caller
    // knows cross-file class extraction may be incomplete.
    let mut result = serde_json::json!({
        "files_checked": files_checked,
        "totals_by_severity": {
            "error": totals.error,
            "warning": totals.warning,
            "lint": totals.lint,
            "hint": totals.hint,
        },
        "totals_by_category": by_category,
        "total": summary.total(),
        "type_coverage": {
            "typed": coverage.typed_expressions,
            "total": coverage.total_expressions,
            "dynamic_percent": dynamic_pct,
        },
    });
    if !unreadable_files.is_empty() {
        result["unreadable_package_files"] = serde_json::json!(unreadable_files);
    }
    // BT-2067: Surface unreadable target files as a structured field and a
    // top-level `error` message so callers treating the response as a summary
    // do not mistake zero-checked-files for a clean result.
    if !unreadable_target_files.is_empty() {
        let joined = unreadable_target_files.join(", ");
        result["unreadable_target_files"] = serde_json::json!(unreadable_target_files);
        result["error"] = serde_json::json!(format!("Failed to read target file(s): {joined}"));
    }
    result
}

/// Canonicalize `path`, falling back to a plain clone when the path cannot be
/// resolved (e.g. it does not yet exist or permissions are denied). Used as a
/// normalized key for path-based deduplication in the two-pass lint pipeline.
fn canonicalize_or_clone(path: &std::path::Path) -> std::path::PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

/// Resolve `path` to a list of `.bt` source files, or return a `LintResult`
/// containing a single error diagnostic explaining why no files could be found.
///
/// Returns `Vec<PathBuf>` (not `Utf8PathBuf`) so that files with non-UTF-8
/// names are preserved rather than silently dropped.
fn resolve_source_files(path: &str) -> Result<Vec<std::path::PathBuf>, LintResult> {
    use beamtalk_core::file_walker::FileWalker;

    let source_path = std::path::Path::new(path);
    let files = FileWalker::lint_files()
        .walk_pathbuf(source_path)
        .map_err(|e| lint_error(path, e.to_string()))?;
    if files.is_empty() {
        return Err(lint_error(
            path,
            format!("No .bt source files found in '{path}'"),
        ));
    }
    Ok(files)
}

/// Build a `LintResult` containing a single file-level error diagnostic.
fn lint_error(file: &str, message: String) -> LintResult {
    let diag = LintDiagnostic {
        file: file.to_string(),
        line: None,
        message,
        severity: "error",
    };
    LintResult {
        warnings: vec![],
        errors: vec![diag],
        total: 1,
    }
}

/// BT-2060: Package root / source-file resolution now lives in
/// [`beamtalk_project::package`] so CLI lint and MCP lint share one
/// implementation.
///
/// This thin wrapper exists only to keep the MCP call sites readable — it
/// forwards to
/// [`beamtalk_project::package::resolve_extraction_files`] and adapts
/// the `&str` path argument the MCP layer carries around.
fn resolve_extraction_files(
    path: &str,
    source_files: &[std::path::PathBuf],
) -> (
    Vec<std::path::PathBuf>,
    std::collections::HashSet<std::path::PathBuf>,
) {
    beamtalk_project::package::resolve_extraction_files(std::path::Path::new(path), source_files)
}

/// BT-2921: Resolve the current package name from `path`'s `beamtalk.toml`,
/// mirroring `beamtalk lint`'s `find_manifest_full` resolution
/// (`beamtalk_cli::commands::lint::run_lint`) so `CompilerOptions::current_package`
/// gets set the same way for MCP `lint`/`diagnostic_summary` as it does for
/// the CLI. Without this, `check_class_visibility`/`check_alias_leaked_visibility`
/// (E0401/E0402/E0403) never fire — both are gated on `current_package: Some(_)`.
///
/// Returns `None` outside a manifest-backed package or when the manifest is
/// malformed (visibility checks conservatively disabled, matching CLI lint's
/// error-path behaviour).
fn resolve_current_package(path: &str) -> Option<String> {
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path))?;
    let project_root = camino::Utf8PathBuf::from_path_buf(project_root).ok()?;
    match beamtalk_cli::manifest::find_manifest_full(&project_root) {
        Ok(Some(m)) => Some(m.package.name),
        Ok(None) => None,
        Err(e) => {
            tracing::warn!(
                error = %e,
                "Failed to parse beamtalk.toml for MCP lint/diagnostic_summary; \
                 E0401/E0402/E0403 visibility checks disabled"
            );
            None
        }
    }
}

/// BT-2823: Merge class metadata from `path`'s package dependencies (as
/// declared in `beamtalk.toml`) into `all_class_infos`, so `Unresolved
/// class` diagnostics see the same class hierarchy as `beamtalk
/// build`/`beamtalk lint` for classes defined only in a dependency.
///
/// Delegates to [`beamtalk_cli::dependency_classes::resolve_dependency_class_infos`],
/// which is filesystem-only and best-effort — it never fetches over the
/// network, so this never turns an offline `lint`/`diagnostic_summary` call
/// into one with network side effects. Dependencies that have never been
/// fetched by a prior `beamtalk build` are silently skipped.
///
/// Returns whether the project's manifest declares any dependencies, for use
/// as `CompilerOptions::has_package_dependencies` (BT-2794).
fn merge_dependency_class_infos(
    path: &str,
    all_class_infos: &mut Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
) -> bool {
    let Some(project_root) =
        beamtalk_project::package::find_package_root(std::path::Path::new(path))
    else {
        return false;
    };
    let Ok(project_root) = camino::Utf8PathBuf::from_path_buf(project_root) else {
        return false;
    };

    let (has_package_dependencies, dep_class_infos) =
        beamtalk_cli::dependency_classes::resolve_dependency_class_infos(&project_root);
    all_class_infos.extend(dep_class_infos);
    has_package_dependencies
}

/// Run lint passes on `path` (file or directory) and return structured results.
///
/// BT-2052: Uses a two-pass pipeline mirroring CLI `beamtalk lint`:
/// - Pass 1: Parse all files in the package and extract class metadata
/// - Pass 2: Analyse each target file with cross-file class context
///
/// Without cross-file classes, the MCP lint produces different diagnostics
/// than the CLI — e.g. `@expect type` annotations are falsely reported as
/// stale because the type/DNU diagnostics they suppress require cross-file
/// class resolution to appear.
#[allow(clippy::too_many_lines)] // Two-pass pipeline is inherently sequential.
fn run_lint_structured(path: &str) -> LintResult {
    use beamtalk_core::semantic_analysis::ClassHierarchy;

    let source_files = match resolve_source_files(path) {
        Ok(files) => files,
        Err(result) => return result,
    };

    // BT-2052: Determine the full extraction set (package-wide src/ + test/)
    // so cross-file class references resolve correctly.
    let (extraction_files, target_set) = resolve_extraction_files(path, &source_files);

    // BT-2921: Resolve the current package once, mirroring `beamtalk lint`,
    // so E0401/E0402/E0403 visibility checks fire the same way in MCP.
    let current_package = resolve_current_package(path);

    // BT-3398: Resolve the package root once, mirroring
    // `compute_diagnostic_summary`'s own one-time resolution above, so each
    // target file's `is_stub_file` can be derived via
    // `beamtalk_project::package::is_under_stubs_dir` without re-walking
    // ancestors per file.
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path));

    // Pass 1: Parse files and extract class metadata.
    let mut all_class_infos = Vec::new();
    let mut parsed_targets: Vec<(
        std::path::PathBuf,
        String,
        beamtalk_core::ast::Module,
        Vec<beamtalk_core::source_analysis::Diagnostic>,
    )> = Vec::new();

    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    for file in &extraction_files {
        let Ok(source) = std::fs::read_to_string(file) else {
            let canonical = canonicalize_or_clone(file);
            if target_set.contains(&canonical) {
                errors.push(LintDiagnostic {
                    file: file.to_string_lossy().into_owned(),
                    line: None,
                    message: format!("Failed to read '{}'", file.display()),
                    severity: "error",
                });
            } else {
                // BT-2056: Surface a warning when a package-extraction file
                // (chosen by the resolver but not a direct lint target) cannot
                // be read. Without this, cross-file class extraction silently
                // drops the file, potentially re-introducing diagnostic
                // divergence from CLI lint.
                warnings.push(LintDiagnostic {
                    file: file.to_string_lossy().into_owned(),
                    line: None,
                    message: format!(
                        "Failed to read package file '{}'; cross-file class extraction may be incomplete",
                        file.display()
                    ),
                    severity: "warning",
                });
            }
            continue;
        };

        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);

        let mut class_infos = ClassHierarchy::extract_class_infos(&module);
        // BT-2921: Stamp the package per-file, same as build's/lint's Pass 1 —
        // without this, a same-package class defined in a sibling file is
        // indistinguishable from a builtin/REPL class and never flagged as a
        // leak by `check_class_visibility`.
        if let Some(pkg) = current_package.as_deref() {
            ClassHierarchy::stamp_package_on_infos(&mut class_infos, pkg);
        }
        all_class_infos.extend(class_infos);

        let canonical = canonicalize_or_clone(file);
        if target_set.contains(&canonical) {
            parsed_targets.push((file.clone(), source, module, parse_diags));
        }
    }

    // BT-2823: Merge dependency class metadata so cross-file references to
    // classes defined only in a git/path dependency (declared in
    // beamtalk.toml) resolve the same way `beamtalk build` does.
    let has_package_dependencies = merge_dependency_class_infos(path, &mut all_class_infos);

    // BT-2858: Populate the FFI type registry the same way `beamtalk lint` does.
    let native_type_registry = build_native_type_registry(path);

    // Pass 2: Analyse each target file with cross-file class context.
    for (file, source, module, parse_diags) in parsed_targets {
        // Include parse errors (syntax problems) and warnings so files with
        // broken syntax or parser-emitted warnings don't silently appear clean.
        // Hint-severity diagnostics (DNU hints) are excluded as they are
        // informational and belong to the check/compile workflow.
        let initial_diags: Vec<_> = parse_diags
            .into_iter()
            .filter(|d| {
                matches!(
                    d.severity,
                    Severity::Error | Severity::Warning | Severity::Lint
                )
            })
            .collect();

        // BT-1587 / BT-2052: run_module_analysis runs lint passes, semantic
        // analysis with cross-file class context (mirroring CLI `beamtalk lint`),
        // and applies @expect directives (BT-1476).
        let is_stub_file = project_root
            .as_deref()
            .is_some_and(|root| beamtalk_project::package::is_under_stubs_dir(root, &file));
        let (lint_diags, _) = run_module_analysis(
            &module,
            &source,
            &all_class_infos,
            initial_diags,
            has_package_dependencies,
            native_type_registry.clone(),
            current_package.as_deref(),
            is_stub_file,
            file.file_stem().and_then(std::ffi::OsStr::to_str),
        );

        let file_name = file.to_string_lossy().into_owned();
        for diag in &lint_diags {
            let line = diag.span.line_number(&source);
            let severity = match diag.severity {
                Severity::Error => "error",
                Severity::Warning | Severity::Lint | Severity::Hint => "warning",
            };
            // BT-1588: Include notes in the message for origin tracing
            let message = if diag.notes.is_empty() {
                diag.message.to_string()
            } else {
                use std::fmt::Write;
                let mut msg = diag.message.to_string();
                for note in &diag.notes {
                    let _ = write!(msg, " ({})", note.message);
                }
                msg
            };
            let entry = LintDiagnostic {
                file: file_name.clone(),
                line: Some(line),
                message,
                severity,
            };
            if diag.severity == Severity::Error {
                errors.push(entry);
            } else {
                warnings.push(entry);
            }
        }
    }

    let total = warnings.len() + errors.len();
    LintResult {
        warnings,
        errors,
        total,
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;
    use camino::Utf8PathBuf;

    /// True when the test process is running as Unix root. Root bypasses
    /// POSIX permission bits, so `chmod 000` fixtures cannot produce the
    /// expected `permission_denied` errors. Tests that rely on those
    /// fixtures bail early when this returns true (e.g. CI sandboxes).
    #[cfg(unix)]
    fn running_as_root() -> bool {
        std::process::Command::new("id")
            .arg("-u")
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .is_some_and(|s| s.trim() == "0")
    }

    // --- run_lint_structured ---

    #[test]
    fn run_lint_structured_nonexistent_path() {
        let result = run_lint_structured("/nonexistent/path/that/does/not/exist");
        assert_eq!(result.total, 1);
        assert!(result.errors.len() == 1);
        assert!(result.warnings.is_empty());
        assert!(result.errors[0].message.contains("does not exist"));
    }

    #[test]
    fn run_lint_structured_non_bt_file() {
        // Use a temp file so the test is portable across platforms.
        let temp = tempfile::TempDir::new().unwrap();
        let path = Utf8PathBuf::from_path_buf(temp.path().join("non_bt.txt"))
            .expect("temp dir should be UTF-8");
        std::fs::write(path.as_std_path(), "not beamtalk").unwrap();
        let result = run_lint_structured(path.as_str());
        assert_eq!(result.total, 1);
        assert!(result.errors.len() == 1);
        assert!(result.errors[0].message.contains(".bt file"));
    }

    #[test]
    fn run_lint_structured_includes_dnu_diagnostics() {
        // BT-1587: MCP lint must include DNU diagnostics from semantic analysis,
        // matching CLI `beamtalk lint` behavior.
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("dnu_test.bt");
        std::fs::write(
            &file,
            r#"Object subclass: DnuTest

  class demo =>
    s := "hello"
    val := s sqrt
    val
"#,
        )
        .unwrap();
        let result = run_lint_structured(file.to_str().unwrap());
        let has_dnu = result
            .warnings
            .iter()
            .chain(result.errors.iter())
            .any(|d| d.message.contains("does not understand"));
        assert!(
            has_dnu,
            "MCP lint should report DNU diagnostics from semantic analysis, got: {result:?}",
        );
    }

    #[test]
    fn run_lint_structured_expect_type_suppresses_dnu() {
        // BT-1587: @expect type should suppress DNU diagnostics in MCP lint,
        // just as it does in CLI lint.
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("expect_test.bt");
        std::fs::write(
            &file,
            r#"Object subclass: ExpectTest

  class demo =>
    s := "hello"
    @expect type
    val := s sqrt
    val
"#,
        )
        .unwrap();
        let result = run_lint_structured(file.to_str().unwrap());
        let has_dnu = result
            .warnings
            .iter()
            .chain(result.errors.iter())
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !has_dnu,
            "@expect type should suppress DNU in MCP lint, got: {result:?}",
        );
    }

    // ── near-miss `// === Name ===` divider (BT-3240/BT-3257) ──────────────
    //
    // These exercise the real `lint`/`diagnostic_summary` entry points
    // (`run_lint_structured`, `run_module_analysis`), not
    // `near_miss_divider::scan_source` directly — that's already covered by
    // that module's own `scan_source_locates_the_near_miss_comment_line_precisely`
    // test.

    #[test]
    fn run_lint_structured_near_miss_divider_span_points_at_comment_line() {
        // BT-3240/BT-3257: before `source` was threaded through
        // `run_module_analysis`, MCP `lint` reached this check through the
        // AST-based `NearMissDividerPass`, whose `Comment::span` is actually
        // `bar`'s token span (line 3), not the comment's own line (line 2).
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("near_miss_test.bt");
        std::fs::write(
            &file,
            "Object subclass: Foo\n  // === Section ====\n  bar => 1\n",
        )
        .unwrap();
        let result = run_lint_structured(file.to_str().unwrap());
        let near_misses: Vec<_> = result
            .warnings
            .iter()
            .chain(result.errors.iter())
            .filter(|d| d.message.contains("section divider"))
            .collect();
        assert_eq!(
            near_misses.len(),
            1,
            "expected exactly one near-miss-divider diagnostic: {result:?}"
        );
        assert_eq!(
            near_misses[0].line,
            Some(2),
            "span should point at the comment's own line (2), not `bar`'s line (3): {result:?}"
        );
    }

    #[test]
    fn run_lint_structured_multiple_near_miss_dividers_get_distinct_correctly_attributed_lines() {
        // Two near-misses in one file must not get their lines mixed up.
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("multi_near_miss_test.bt");
        std::fs::write(
            &file,
            "Object subclass: Foo\n  // === First ====\n  bar => 1\n\n  // == Second ==\n  baz => 2\n",
        )
        .unwrap();
        let result = run_lint_structured(file.to_str().unwrap());
        let mut lines: Vec<u32> = result
            .warnings
            .iter()
            .chain(result.errors.iter())
            .filter(|d| d.message.contains("section divider"))
            .filter_map(|d| d.line)
            .collect();
        lines.sort_unstable();
        assert_eq!(
            lines,
            vec![2, 5],
            "each near-miss should be attributed to its own comment line, not the other's: {result:?}"
        );
    }

    /// BT-3257: `compute_diagnostic_summary`'s public JSON output only
    /// exposes aggregate severity/category counts, not individual
    /// diagnostic spans — so span accuracy can't be asserted through its
    /// return value directly. This instead calls `run_module_analysis`,
    /// the exact shared function `compute_diagnostic_summary`'s Pass 2 loop
    /// invokes per file (see the call site a few lines below in this
    /// module), and inspects the diagnostic it produces before that
    /// information is aggregated away — proving MCP `diagnostic_summary`'s
    /// code path carries the same accurate, comment-line span as MCP
    /// `lint` and the LSP, not just that it doesn't crash.
    #[test]
    fn run_module_analysis_near_miss_divider_span_points_at_comment_line() {
        let source = "Object subclass: Foo\n  // === Section ====\n  bar => 1\n";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let initial_diags: Vec<_> = parse_diags
            .into_iter()
            .filter(|d| d.severity == Severity::Lint)
            .collect();
        let (diags, _) = run_module_analysis(
            &module,
            source,
            &[],
            initial_diags,
            false,
            None,
            None,
            false,
            None,
        );
        let near_misses: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("section divider"))
            .collect();
        assert_eq!(
            near_misses.len(),
            1,
            "expected exactly one near-miss-divider diagnostic: {diags:?}"
        );
        assert_eq!(
            near_misses[0].span.line_number(source),
            2,
            "span should point at the comment's own line (2), not `bar`'s line (3): {diags:?}"
        );
    }

    /// BT-3431: MCP `lint`/`diagnostic_summary` must report the same
    /// file-name/class-name mismatch `beamtalk build`/`beamtalk lint`/the
    /// LSP do — before this fix, `run_module_analysis` never called
    /// `check_class_file_name_agreement` at all, so this surface silently
    /// reported clean regardless of `file_stem`.
    #[test]
    fn run_module_analysis_reports_mismatched_file_name() {
        let source = "Value subclass: ExduraEvent";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let (diags, _) = run_module_analysis(
            &module,
            source,
            &[],
            parse_diags,
            false,
            None,
            None,
            false,
            Some("event"),
        );
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("does not match declared class")),
            "mismatched file name should be reported: {diags:?}"
        );
    }

    /// BT-3431 negative control.
    #[test]
    fn run_module_analysis_does_not_report_matching_file_name() {
        let source = "Value subclass: ExduraEvent";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let (diags, _) = run_module_analysis(
            &module,
            source,
            &[],
            parse_diags,
            false,
            None,
            None,
            false,
            Some("exdura_event"),
        );
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("does not match declared class")),
            "matching file name should not be reported: {diags:?}"
        );
    }

    /// BT-3398 regression, analogous to
    /// `beamtalk_language_service::project_index::tests::is_stub_file_true_for_file_under_a_root_stubs_dir`:
    /// `run_module_analysis`'s `is_stub_file` argument must actually reach
    /// `AnalysisContext::is_stub_file` — verified here by calling it directly
    /// with `is_stub_file: false` (the value every call site used
    /// unconditionally before this fix) on a module containing a `declare
    /// native:` block and confirming `check_native_declaration_location`
    /// still runs (would reject it if this test's own module lived in
    /// `src/`), then with `is_stub_file: true` (what a real `stubs/` call
    /// site now derives) and confirming it no longer would.
    ///
    /// At the time this test was written, `check_native_declaration_location`'s
    /// diagnostic had no `DiagnosticCategory` (a separate, pre-existing gap
    /// shared by `beamtalk lint`'s own `collect_diagnostics` — filed and
    /// fixed as BT-3404), so `run_module_analysis`'s `category.is_some()`
    /// filter dropped it from the *returned* diagnostics regardless of
    /// `is_stub_file`. This test therefore asserts on `analyse_full`'s
    /// pre-filter diagnostics — built with the identical `AnalysisContext`
    /// construction `run_module_analysis` uses — rather than
    /// `run_module_analysis`'s own return value, so it actually exercises the
    /// `is_stub_file` wiring instead of vacuously passing either way. Now that
    /// BT-3404 has given the diagnostic a category, the *returned* diagnostics
    /// carry it too — see
    /// `run_module_analysis_reports_native_declaration_location_error` below.
    #[test]
    fn run_module_analysis_is_stub_file_suppresses_native_declaration_location_error() {
        let source = "declare native: lists\n";
        let tokens = lex_with_eof(source);
        let (module, _parse_diags) = parse(tokens);

        let has_location_error = |is_stub_file: bool| {
            let analysis_ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
                .with_is_stub_file(is_stub_file);
            let result = beamtalk_core::semantic_analysis::analyse_full(&module, analysis_ctx);
            result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("only valid in stubs/ directory"))
        };

        assert!(
            has_location_error(false),
            "declare native: outside stubs/ should still be rejected"
        );
        assert!(
            !has_location_error(true),
            "declare native: inside stubs/ should not be rejected"
        );
    }

    /// BT-3404 regression: `check_native_declaration_location`'s diagnostic
    /// now carries a `DiagnosticCategory`
    /// (`NativeDeclarationLocation`), so `run_module_analysis`'s
    /// `category.is_some()` filter no longer silently drops it from the
    /// diagnostics MCP `lint`/`diagnostic_summary` actually return — unlike
    /// before this fix, where the previous test had to reach past
    /// `run_module_analysis` into `analyse_full`'s pre-filter diagnostics to
    /// observe the check running at all.
    #[test]
    fn run_module_analysis_reports_native_declaration_location_error() {
        let source = "declare native: lists\n";
        let tokens = lex_with_eof(source);
        let (module, _parse_diags) = parse(tokens);

        let (diags, _) = run_module_analysis(
            &module,
            source,
            &[],
            Vec::new(),
            false,
            None,
            None,
            false,
            None,
        );

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("only valid in stubs/ directory")),
            "declare native: outside stubs/ should be reported by run_module_analysis, \
             not silently dropped: {diags:?}"
        );
    }

    /// BT-3398 end-to-end (MCP-level) regression, per the issue's acceptance
    /// criteria: opening a legitimate `stubs/lists.bt` via the MCP `lint`
    /// tool must not report a false "only valid in stubs/ directory" error.
    /// The previous test asserts the `is_stub_file` wiring actually
    /// discriminates stub vs. non-stub at the `AnalysisContext` level (the
    /// only level that can observe it, per that test's doc on the
    /// category-filter gap); this one pins the MCP-surface behaviour the
    /// issue is actually about.
    #[test]
    fn run_lint_structured_stub_file_declare_native_no_location_error() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"stub-test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let stubs_dir = dir.join("stubs");
        std::fs::create_dir_all(&stubs_dir).unwrap();
        let stub_file = stubs_dir.join("lists.bt");
        std::fs::write(&stub_file, "declare native: lists\n").unwrap();

        let stub_result = run_lint_structured(stub_file.to_str().unwrap());
        let stub_location_errors: Vec<_> = stub_result
            .errors
            .iter()
            .filter(|d| d.message.contains("only valid in stubs/ directory"))
            .collect();
        assert!(
            stub_location_errors.is_empty(),
            "a legitimate stubs/lists.bt should not report a native-declaration \
             location error via MCP lint, got: {stub_result:?}"
        );
    }

    /// BT-2858: `build_native_type_registry` extracts live from OTP `.beam`
    /// files for a manifest-backed project with no prior `beamtalk build` —
    /// analogous to `commands::lint`'s
    /// `lint_extracts_type_specs_live_on_cold_cache_bt_2851` in the CLI
    /// binary. Before this fix, MCP `lint`/`diagnostic_summary` had no way to
    /// obtain a registry at all (`run_module_analysis` always passed `None`).
    #[test]
    fn build_native_type_registry_extracts_live_on_cold_cache() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        std::fs::create_dir_all(dir.join("src")).unwrap();
        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        // No `_build/` directory exists yet — the cold-cache case.
        assert!(!dir.join("_build").exists());

        let Some(registry) = build_native_type_registry(dir.join("src").to_str().unwrap()) else {
            // OTP `.beam` discovery is environment-dependent (e.g. a sandbox
            // with no OTP install on disk); skip rather than false-fail.
            eprintln!(
                "skipping build_native_type_registry_extracts_live_on_cold_cache: \
                 no OTP .beam files discovered in this environment"
            );
            return;
        };
        assert!(
            registry.lookup("lists", "reverse", 1).is_some(),
            "live extraction with no prior build must still find lists:reverse/1"
        );
        // The extractor writes the same cache a `beamtalk build`/`beamtalk
        // lint` run would, so a subsequent call reads it back instead of
        // re-extracting.
        assert!(dir.join("_build").join("type_cache").exists());
    }

    /// BT-2858: MCP `lint` must see the same FFI argument-type registry
    /// `beamtalk lint`/`beamtalk build` do, so a well-specced `(Erlang m) f:`
    /// call does not fall back to `Dynamic(UntypedFfi)` and trip the BT-1914
    /// "Dynamic in typed class" warning — mirrors
    /// `commands::lint`'s `ffi_call_with_registry_does_not_warn_dynamic_in_typed_class`.
    /// Before this fix, `run_module_analysis` always analysed with `None`,
    /// so this warning fired unconditionally regardless of whether the
    /// runtime's `.beam` files carried a real `-spec`.
    #[test]
    fn run_lint_structured_ffi_call_does_not_warn_dynamic_in_typed_class() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        std::fs::create_dir_all(dir.join("src")).unwrap();
        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let file = dir.join("src").join("ffi_test.bt");
        std::fs::write(
            &file,
            "sealed typed Value subclass: FfiTest\n\n  check -> Dynamic =>\n    Erlang lists reverse: (1 to: 3) asArray\n",
        )
        .unwrap();

        if build_native_type_registry(file.to_str().unwrap()).is_none() {
            eprintln!(
                "skipping run_lint_structured_ffi_call_does_not_warn_dynamic_in_typed_class: \
                 no OTP .beam files discovered in this environment"
            );
            return;
        }

        let result = run_lint_structured(file.to_str().unwrap());
        let untyped_ffi: Vec<_> = result
            .warnings
            .iter()
            .chain(result.errors.iter())
            .filter(|d| d.message.contains("untyped FFI"))
            .collect();
        assert!(
            untyped_ffi.is_empty(),
            "with a live registry, MCP lint must not warn untyped FFI; got: {untyped_ffi:?}"
        );
    }

    /// BT-2052: MCP lint must resolve cross-file classes from the full package
    /// source set (src/ + test/). Without this, `@expect type` annotations that
    /// suppress diagnostics referencing classes from other files in the same
    /// package are falsely reported as stale.
    #[test]
    fn run_lint_structured_cross_file_classes() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        let src_dir = dir.join("src");
        let test_dir = dir.join("test");
        std::fs::create_dir_all(&src_dir).unwrap();
        std::fs::create_dir_all(&test_dir).unwrap();

        // Create a beamtalk.toml so find_package_root works.
        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"cross-test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        // Define an Actor in src/ — Actor subclasses are known types.
        std::fs::write(
            src_dir.join("my_actor.bt"),
            "Actor subclass: MyActor\n  run => 42\n",
        )
        .unwrap();

        // A test file that uses `@expect all` on `MyActor new` — the `new`
        // message on an Actor produces an instantiation_error diagnostic that
        // the @expect suppresses. Without cross-file class info, the @expect
        // would be reported as stale.
        std::fs::write(
            test_dir.join("my_actor_test.bt"),
            "Object subclass: MyActorTest\n\n  class run =>\n    @expect all\n    MyActor new\n",
        )
        .unwrap();

        // Lint only the test file, but cross-file resolution should still see
        // MyActor from src/.
        let test_file = test_dir.join("my_actor_test.bt");
        let result = run_lint_structured(test_file.to_str().unwrap());

        let stale = result
            .warnings
            .iter()
            .chain(result.errors.iter())
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "MCP lint with cross-file classes should not report @expect as stale, got: {result:?}",
        );
    }

    /// Write a fixture project whose `src/` declares an `internal` class in
    /// one file and leaks it through a public method's signature in a
    /// *sibling* file, mirroring `docs/beamtalk-language-features.md`'s
    /// TokenBuffer/Parser example (and the CLI's `cli_build.rs` regression
    /// for the same fixture, BT-2920). Returns the fixture's `TempDir` (keep
    /// it alive for the duration of the test) and the path to
    /// `src/parser.bt`.
    fn write_cross_file_visibility_leak_fixture() -> (tempfile::TempDir, std::path::PathBuf) {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        let src_dir = dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();

        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        std::fs::write(
            src_dir.join("token_buffer.bt"),
            "internal Object subclass: TokenBuffer\n  data => nil\n",
        )
        .unwrap();
        let parser_file = src_dir.join("parser.bt");
        std::fs::write(
            &parser_file,
            "Object subclass: Parser\n  tokenize: input :: String -> TokenBuffer => nil\n",
        )
        .unwrap();

        (temp, parser_file)
    }

    /// Regression for BT-2921: `current_package` was never threaded into
    /// `run_module_analysis`'s `CompilerOptions`, so `check_class_visibility`
    /// (E0401/E0402) silently emitted zero diagnostics for MCP `lint`, unlike
    /// `beamtalk build`/`beamtalk lint` after BT-2920.
    #[test]
    fn run_lint_structured_reports_e0402_for_cross_file_internal_class_leak() {
        let (_temp, parser_file) = write_cross_file_visibility_leak_fixture();
        let result = run_lint_structured(parser_file.to_str().unwrap());

        let leaked = result.warnings.iter().chain(result.errors.iter()).any(|d| {
            d.message
                .contains("Internal class 'TokenBuffer' appears in public signature")
        });
        assert!(
            leaked,
            "MCP lint should report E0402 for the cross-file internal class leak, got: {result:?}",
        );
    }

    /// Same as `run_lint_structured_reports_e0402_for_cross_file_internal_class_leak`
    /// but for the `diagnostic_summary` tool (BT-2921).
    #[test]
    fn compute_diagnostic_summary_reports_e0402_for_cross_file_internal_class_leak() {
        let (_temp, parser_file) = write_cross_file_visibility_leak_fixture();
        let result = compute_diagnostic_summary(parser_file.to_str().unwrap());

        let visibility_total = result["totals_by_category"]["Visibility"]["total"]
            .as_u64()
            .unwrap_or(0);
        assert!(
            visibility_total > 0,
            "diagnostic_summary should report the E0402 visibility leak, got: {result:?}",
        );
    }

    /// Write a fixture project declaring a git dependency `http` in
    /// `beamtalk.toml`, with the dependency's checkout already present under
    /// `_build/deps/http/src/` (simulating the state left by a prior
    /// `beamtalk build`, matching BT-2823's repro). The project's own
    /// `src/app.bt` references the dependency's `HTTPServer` class. Returns
    /// the fixture's `TempDir` (keep it alive for the duration of the test —
    /// it removes the project directory on drop) and the path to
    /// `src/app.bt`.
    fn write_git_dependency_fixture() -> (tempfile::TempDir, std::path::PathBuf) {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        let src_dir = dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();

        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
        )
        .unwrap();

        // Simulate `beamtalk build` having already fetched the dependency.
        let dep_src_dir = dir.join("_build").join("deps").join("http").join("src");
        std::fs::create_dir_all(&dep_src_dir).unwrap();
        std::fs::write(
            dep_src_dir.join("http_server.bt"),
            "Object subclass: HTTPServer\n",
        )
        .unwrap();

        // A second project-local class so `has_cross_file_classes` is true
        // independent of dependency resolution (matching the sentinel
        // pattern `fixture_sourced_protocol_name_is_not_unresolved` uses in
        // beamtalk-core). Without this, `check_unresolved_classes` would be
        // skipped entirely for a single-file project and this test would
        // pass vacuously regardless of whether the fix is in place.
        std::fs::write(src_dir.join("other.bt"), "Object subclass: Other\n").unwrap();

        let app_file = src_dir.join("app.bt");
        std::fs::write(
            &app_file,
            "Object subclass: App\n\n  class run =>\n    HTTPServer new\n",
        )
        .unwrap();

        (temp, app_file)
    }

    /// BT-2823: MCP `lint` must resolve classes from a project's git
    /// dependencies (declared in `beamtalk.toml`) the same way `beamtalk
    /// build`/`beamtalk lint` do, using whatever dependency checkout is
    /// already on disk under `_build/deps/<name>/` — without a false-positive
    /// `Unresolved class` diagnostic.
    #[test]
    fn run_lint_structured_resolves_git_dependency_classes() {
        let (_temp, app_file) = write_git_dependency_fixture();
        let result = run_lint_structured(app_file.to_str().unwrap());

        let unresolved = result
            .warnings
            .iter()
            .chain(result.errors.iter())
            .any(|d| d.message.contains("Unresolved class"));
        assert!(
            !unresolved,
            "MCP lint should resolve HTTPServer from the git dependency checkout, got: {result:?}",
        );
    }

    /// BT-2823: Same as `run_lint_structured_resolves_git_dependency_classes`
    /// but for the `diagnostic_summary` tool.
    #[test]
    fn compute_diagnostic_summary_resolves_git_dependency_classes() {
        let (_temp, app_file) = write_git_dependency_fixture();
        let result = compute_diagnostic_summary(app_file.to_str().unwrap());

        let unresolved_class_total = result["totals_by_category"]["UnresolvedClass"]["total"]
            .as_u64()
            .unwrap_or(0);
        assert_eq!(
            unresolved_class_total, 0,
            "diagnostic_summary should resolve HTTPServer from the git dependency \
             checkout with zero UnresolvedClass diagnostics, got: {result:?}",
        );
    }

    /// Write a fixture project declaring a *direct* git dependency `http` in
    /// `beamtalk.toml`, where `http`'s own checked-out `beamtalk.toml`
    /// declares a *transitive* git dependency `net` (BT-2836) — never
    /// mentioned in the project's own manifest. Both checkouts are already
    /// present under `_build/deps/`, simulating a prior `beamtalk build`. The
    /// project's own `src/app.bt` references `net`'s `NetClient` class
    /// directly, which is only reachable by walking `http`'s manifest.
    /// Returns the fixture's `TempDir` (keep it alive for the duration of the
    /// test) and the path to `src/app.bt`.
    fn write_transitive_git_dependency_fixture() -> (tempfile::TempDir, std::path::PathBuf) {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        let src_dir = dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();

        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
        )
        .unwrap();

        // Simulate `beamtalk build` having already fetched the direct
        // dependency, whose own manifest declares a transitive dependency.
        let http_dir = dir.join("_build").join("deps").join("http");
        std::fs::create_dir_all(http_dir.join("src")).unwrap();
        std::fs::write(
            http_dir.join("beamtalk.toml"),
            "[package]\nname = \"http\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nnet = { git = \"https://example.com/net.git\", tag = \"v1.0.0\" }\n",
        )
        .unwrap();
        std::fs::write(
            http_dir.join("src").join("http_server.bt"),
            "Object subclass: HTTPServer\n",
        )
        .unwrap();

        // The transitive dependency's checkout, never declared in app's own
        // `beamtalk.toml` — only discoverable by walking `http`'s manifest.
        let net_src_dir = dir.join("_build").join("deps").join("net").join("src");
        std::fs::create_dir_all(&net_src_dir).unwrap();
        std::fs::write(
            net_src_dir.join("net_client.bt"),
            "Object subclass: NetClient\n",
        )
        .unwrap();

        // Sentinel class so `has_cross_file_classes` isn't vacuously true/false
        // independent of dependency resolution, matching
        // `write_git_dependency_fixture`'s pattern.
        std::fs::write(src_dir.join("other.bt"), "Object subclass: Other\n").unwrap();

        let app_file = src_dir.join("app.bt");
        std::fs::write(
            &app_file,
            "Object subclass: App\n\n  class run =>\n    NetClient new\n",
        )
        .unwrap();

        (temp, app_file)
    }

    /// BT-2836: MCP `lint` must resolve classes from a *transitive*
    /// dependency (declared only in a direct dependency's own
    /// `beamtalk.toml`, not the project's) the same way `beamtalk
    /// build`/`beamtalk lint` do, using whatever checkout is already on disk
    /// under `_build/deps/<name>/` — without a false-positive `Unresolved
    /// class` diagnostic.
    #[test]
    fn run_lint_structured_resolves_transitive_git_dependency_classes() {
        let (_temp, app_file) = write_transitive_git_dependency_fixture();
        let result = run_lint_structured(app_file.to_str().unwrap());

        let unresolved = result
            .warnings
            .iter()
            .chain(result.errors.iter())
            .any(|d| d.message.contains("Unresolved class"));
        assert!(
            !unresolved,
            "MCP lint should resolve NetClient from the transitive git dependency \
             checkout, got: {result:?}",
        );
    }

    /// BT-2836: Same as
    /// `run_lint_structured_resolves_transitive_git_dependency_classes` but
    /// for the `diagnostic_summary` tool.
    #[test]
    fn compute_diagnostic_summary_resolves_transitive_git_dependency_classes() {
        let (_temp, app_file) = write_transitive_git_dependency_fixture();
        let result = compute_diagnostic_summary(app_file.to_str().unwrap());

        let unresolved_class_total = result["totals_by_category"]["UnresolvedClass"]["total"]
            .as_u64()
            .unwrap_or(0);
        assert_eq!(
            unresolved_class_total, 0,
            "diagnostic_summary should resolve NetClient from the transitive git \
             dependency checkout with zero UnresolvedClass diagnostics, got: {result:?}",
        );
    }

    /// BT-2056: When a package-extraction file in src/ cannot be read, MCP lint
    /// must surface a warning rather than silently dropping it from the
    /// extraction set.
    #[cfg(unix)]
    #[test]
    fn run_lint_structured_unreadable_package_file_warns() {
        use std::os::unix::fs::PermissionsExt;

        if running_as_root() {
            eprintln!("skipped: running as root, chmod 000 doesn't apply");
            return;
        }

        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        let src_dir = dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();

        // Create a beamtalk.toml so find_package_root works.
        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"unreadable-test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        // Create a readable target file.
        let target = src_dir.join("main.bt");
        std::fs::write(&target, "Object subclass: Main\n  class hello => 42\n").unwrap();

        // Create a sibling file, then make it unreadable.
        let sibling = src_dir.join("helper.bt");
        std::fs::write(&sibling, "Object subclass: Helper\n  class help => 1\n").unwrap();
        std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o000)).unwrap();

        let result = run_lint_structured(target.to_str().unwrap());

        // Restore permissions so the TempDir's Drop cleanup can remove the
        // sibling file.
        let _ = std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o644));

        let has_unreadable_warning = result.warnings.iter().any(|d| {
            d.message
                .contains("cross-file class extraction may be incomplete")
        });
        assert!(
            has_unreadable_warning,
            "MCP lint should warn about unreadable package files, got: {result:?}",
        );
    }

    /// BT-2056: `compute_diagnostic_summary` should include unreadable package
    /// files in its output when a sibling file cannot be read.
    #[cfg(unix)]
    #[test]
    fn compute_diagnostic_summary_unreadable_package_file() {
        use std::os::unix::fs::PermissionsExt;

        if running_as_root() {
            eprintln!("skipped: running as root, chmod 000 doesn't apply");
            return;
        }

        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();
        let src_dir = dir.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();

        std::fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"unreadable-test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let target = src_dir.join("main.bt");
        std::fs::write(&target, "Object subclass: Main\n  class hello => 42\n").unwrap();

        let sibling = src_dir.join("helper.bt");
        std::fs::write(&sibling, "Object subclass: Helper\n  class help => 1\n").unwrap();
        std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o000)).unwrap();

        let result = compute_diagnostic_summary(target.to_str().unwrap());

        // Restore permissions so the TempDir's Drop cleanup can remove the
        // sibling file.
        let _ = std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o644));

        assert!(
            result.get("unreadable_package_files").is_some(),
            "diagnostic summary should include unreadable_package_files, got: {result}",
        );
        let unreadable = result["unreadable_package_files"].as_array().unwrap();
        assert_eq!(unreadable.len(), 1);
        assert!(
            unreadable[0].as_str().unwrap().contains("helper.bt"),
            "unreadable file should be helper.bt, got: {unreadable:?}",
        );
    }

    /// BT-2067: `compute_diagnostic_summary` must surface an error when a
    /// direct target file is unreadable, not a clean `files_checked=0` result.
    #[cfg(unix)]
    #[test]
    fn compute_diagnostic_summary_unreadable_direct_target() {
        use std::os::unix::fs::PermissionsExt;

        if running_as_root() {
            eprintln!("skipped: running as root, chmod 000 doesn't apply");
            return;
        }

        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();

        let target = dir.join("locked.bt");
        std::fs::write(&target, "Object subclass: Locked\n  class hello => 1\n").unwrap();
        std::fs::set_permissions(&target, std::fs::Permissions::from_mode(0o000)).unwrap();

        let result = compute_diagnostic_summary(target.to_str().unwrap());

        // Restore permissions so the TempDir's Drop cleanup can remove the
        // target file.
        std::fs::set_permissions(&target, std::fs::Permissions::from_mode(0o644)).unwrap();

        assert_eq!(result["files_checked"], 0);
        assert!(
            result["error"].is_string(),
            "should surface an error for unreadable target, got: {result}",
        );
        let err_msg = result["error"].as_str().unwrap();
        assert!(
            err_msg.contains("locked.bt"),
            "error should name the unreadable file, got: {err_msg}",
        );
        let listed = result["unreadable_target_files"].as_array().unwrap();
        assert_eq!(listed.len(), 1);
        assert!(listed[0].as_str().unwrap().contains("locked.bt"));
    }

    /// BT-2067: Directory-based lint invocations must still surface a specific
    /// unreadable target when some files resolve successfully and others do
    /// not.
    #[cfg(unix)]
    #[test]
    fn compute_diagnostic_summary_directory_with_unreadable_target() {
        use std::os::unix::fs::PermissionsExt;

        if running_as_root() {
            eprintln!("skipped: running as root, chmod 000 doesn't apply");
            return;
        }

        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();

        let readable = dir.join("readable.bt");
        std::fs::write(&readable, "Object subclass: Readable\n  class hello => 1\n").unwrap();

        let locked = dir.join("locked.bt");
        std::fs::write(&locked, "Object subclass: Locked\n  class hello => 2\n").unwrap();
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();

        let result = compute_diagnostic_summary(dir.to_str().unwrap());

        // Restore permissions so the TempDir's Drop cleanup can remove the
        // locked file.
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o644)).unwrap();

        assert_eq!(
            result["files_checked"], 1,
            "readable sibling should still be checked, got: {result}",
        );
        let listed = result["unreadable_target_files"]
            .as_array()
            .unwrap_or_else(|| panic!("unreadable_target_files missing, got: {result}"));
        assert_eq!(listed.len(), 1);
        assert!(
            listed[0].as_str().unwrap().contains("locked.bt"),
            "should name the locked file, got: {listed:?}",
        );
        let err_msg = result["error"].as_str().unwrap();
        assert!(err_msg.contains("locked.bt"));
    }

    /// BT-2067: `run_lint_structured` must emit a file-level error for
    /// unreadable targets surfaced by a directory walk, not drop them silently.
    #[cfg(unix)]
    #[test]
    fn run_lint_structured_directory_with_unreadable_target_errors() {
        use std::os::unix::fs::PermissionsExt;

        if running_as_root() {
            eprintln!("skipped: running as root, chmod 000 doesn't apply");
            return;
        }

        let temp = tempfile::TempDir::new().unwrap();
        let dir = temp.path();

        let readable = dir.join("readable.bt");
        std::fs::write(&readable, "Object subclass: Readable\n  class hello => 1\n").unwrap();

        let locked = dir.join("locked.bt");
        std::fs::write(&locked, "Object subclass: Locked\n  class hello => 2\n").unwrap();
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();

        let result = run_lint_structured(dir.to_str().unwrap());

        // Restore permissions so the TempDir's Drop cleanup can remove the
        // locked file.
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o644)).unwrap();

        let has_locked_error = result
            .errors
            .iter()
            .any(|d| d.file.contains("locked.bt") && d.message.contains("Failed to read"));
        assert!(
            has_locked_error,
            "expected a read-failure error naming locked.bt, got: {result:?}",
        );
    }

    // BT-2060: `find_package_root` tests moved to
    // `beamtalk_project::package` tests — the MCP helper is now a thin
    // wrapper around the shared implementation, so duplicating the
    // ancestor-walk assertions here would only lock in behaviour twice.

    // --- search_examples ---

    #[test]
    fn search_examples_returns_results_for_known_query() {
        let results = beamtalk_examples::search("closures", None);
        assert!(
            !results.is_empty(),
            "searching 'closures' should return results from the bundled corpus"
        );
    }

    #[test]
    fn search_examples_respects_limit() {
        let results = beamtalk_examples::search("a", Some(2));
        assert!(
            results.len() <= 2,
            "limit=2 should return at most 2 results, got {}",
            results.len()
        );
    }

    #[test]
    fn search_examples_empty_query_returns_empty() {
        let results = beamtalk_examples::search("", None);
        assert!(results.is_empty(), "empty query should return no results");
    }

    #[test]
    fn search_examples_tool_registered() {
        // Verify that search_examples appears in the tool router by checking
        // that the tool_router lists it. The #[tool_router] macro generates
        // a tool_router() method that includes all #[tool] handlers.
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            tool_names.contains(&"search_examples"),
            "search_examples should be in tool list, found: {tool_names:?}"
        );
    }

    // --- search_classes ---

    #[test]
    fn search_classes_returns_results_for_known_query() {
        let results = beamtalk_examples::search_classes("http", None);
        assert!(
            !results.is_empty(),
            "searching 'http' should return results from the bundled class corpus"
        );
    }

    #[test]
    fn search_classes_respects_limit() {
        let results = beamtalk_examples::search_classes("a", Some(2));
        assert!(
            results.len() <= 2,
            "limit=2 should return at most 2 results, got {}",
            results.len()
        );
    }

    #[test]
    fn search_classes_empty_query_returns_empty() {
        let results = beamtalk_examples::search_classes("", None);
        assert!(results.is_empty(), "empty query should return no results");
    }

    #[test]
    fn search_classes_tool_registered() {
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            tool_names.contains(&"search_classes"),
            "search_classes should be in tool list, found: {tool_names:?}"
        );
    }

    // --- list_classes param deserialization (BT-1404) ---

    #[test]
    fn list_classes_params_no_filter() {
        let json = serde_json::json!({});
        let params: ListClassesParams = serde_json::from_value(json).unwrap();
        assert!(params.filter.is_none());
    }

    #[test]
    fn list_classes_params_with_filter() {
        let json = serde_json::json!({"filter": "Value"});
        let params: ListClassesParams = serde_json::from_value(json).unwrap();
        assert_eq!(params.filter.as_deref(), Some("Value"));
    }

    #[test]
    fn list_classes_tool_registered() {
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            tool_names.contains(&"list_classes"),
            "list_classes should be in tool list, found: {tool_names:?}"
        );
    }

    // --- validate_class_name ---

    #[test]
    fn validate_class_name_valid_simple() {
        assert!(validate_class_name("Counter").is_ok());
    }

    #[test]
    fn validate_class_name_valid_with_digits() {
        assert!(validate_class_name("Counter2").is_ok());
    }

    #[test]
    fn validate_class_name_valid_with_underscore() {
        assert!(validate_class_name("My_Class").is_ok());
    }

    #[test]
    fn validate_class_name_valid_single_char() {
        assert!(validate_class_name("A").is_ok());
    }

    #[test]
    fn validate_class_name_empty_is_error() {
        assert!(validate_class_name("").is_err());
    }

    #[test]
    fn validate_class_name_lowercase_start_is_error() {
        let err = validate_class_name("counter");
        assert!(err.is_err());
        let msg = err.unwrap_err().message;
        assert!(msg.contains("counter"), "error should mention the name");
    }

    #[test]
    fn validate_class_name_digit_start_is_error() {
        assert!(validate_class_name("1Counter").is_err());
    }

    #[test]
    fn validate_class_name_space_is_error() {
        assert!(validate_class_name("My Class").is_err());
    }

    #[test]
    fn validate_class_name_hyphen_is_error() {
        assert!(validate_class_name("My-Class").is_err());
    }

    #[test]
    fn validate_class_name_colon_is_error() {
        // Colons are not allowed in class names (only selectors)
        assert!(validate_class_name("My:Class").is_err());
    }

    #[test]
    fn validate_class_name_unicode_is_error() {
        // Non-ASCII chars are not valid
        assert!(validate_class_name("Clàss").is_err());
    }

    #[test]
    fn validate_class_name_error_message_contains_name() {
        let err = validate_class_name("bad name").unwrap_err();
        assert!(
            err.message.contains("bad name"),
            "error message should include the invalid name: {}",
            err.message
        );
    }

    // --- validate_erlang_module_name ---

    #[test]
    fn validate_erlang_module_name_valid() {
        assert!(validate_erlang_module_name("lists").is_ok());
        assert!(validate_erlang_module_name("maps").is_ok());
        assert!(validate_erlang_module_name("beamtalk_runtime").is_ok());
        assert!(validate_erlang_module_name("_private").is_ok());
    }

    #[test]
    fn validate_erlang_module_name_empty_is_error() {
        assert!(validate_erlang_module_name("").is_err());
    }

    #[test]
    fn validate_erlang_module_name_uppercase_start_is_error() {
        assert!(validate_erlang_module_name("Lists").is_err());
    }

    #[test]
    fn validate_erlang_module_name_space_is_error() {
        assert!(validate_erlang_module_name("my module").is_err());
    }

    #[test]
    fn validate_erlang_module_name_hyphen_is_error() {
        assert!(validate_erlang_module_name("my-module").is_err());
    }

    #[test]
    fn validate_erlang_module_name_error_contains_name() {
        let err = validate_erlang_module_name("BadModule").unwrap_err();
        assert!(err.message.contains("BadModule"));
    }

    // --- validate_selector ---

    #[test]
    fn validate_selector_empty_is_error() {
        assert!(validate_selector("").is_err());
    }

    #[test]
    fn validate_selector_unary_valid() {
        assert!(validate_selector("increment").is_ok());
        assert!(validate_selector("size").is_ok());
        assert!(validate_selector("isEmpty").is_ok());
    }

    #[test]
    fn validate_selector_keyword_valid() {
        assert!(validate_selector("at:").is_ok());
        assert!(validate_selector("at:put:").is_ok());
        assert!(validate_selector("ifTrue:ifFalse:").is_ok());
    }

    #[test]
    fn validate_selector_binary_operator_valid() {
        assert!(validate_selector("+").is_ok());
        assert!(validate_selector("-").is_ok());
        assert!(validate_selector("*").is_ok());
        assert!(validate_selector("/").is_ok());
        assert!(validate_selector("<").is_ok());
        assert!(validate_selector(">").is_ok());
        assert!(validate_selector("=").is_ok());
        assert!(validate_selector(">=").is_ok());
        assert!(validate_selector("<=").is_ok());
        assert!(validate_selector("**").is_ok());
        assert!(validate_selector("~=").is_ok());
    }

    #[test]
    fn validate_selector_binary_with_alphanumeric_is_error() {
        // Starts with operator char but mixes in alphanumeric — invalid binary
        let err = validate_selector("+foo");
        assert!(err.is_err());
        let msg = err.unwrap_err().message;
        assert!(msg.contains("+foo"), "error should mention the selector");
    }

    #[test]
    fn validate_selector_keyword_with_space_is_error() {
        assert!(validate_selector("at: put:").is_err());
    }

    #[test]
    fn validate_selector_keyword_with_hash_is_error() {
        assert!(validate_selector("#size").is_err());
    }

    #[test]
    fn validate_selector_keyword_starting_with_digit_is_ok() {
        // digits are alphanumeric — "1x:" is technically accepted by current impl
        // (no leading-char restriction on selectors, only on class names)
        assert!(validate_selector("at1:").is_ok());
    }

    #[test]
    fn validate_selector_error_message_contains_selector() {
        let err = validate_selector("bad selector!").unwrap_err();
        assert!(
            err.message.contains("bad selector!"),
            "error message should include the invalid selector: {}",
            err.message
        );
    }

    // --- tool registration: reload_class replaces reload_module ---

    #[test]
    fn reload_class_tool_registered() {
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            tool_names.contains(&"reload_class"),
            "reload_class should be in tool list, found: {tool_names:?}"
        );
        assert!(
            !tool_names.contains(&"reload_module"),
            "reload_module should not be in tool list (replaced by reload_class)"
        );
    }

    #[test]
    fn precheck_method_and_recheck_image_tools_registered() {
        // ADR 0105 Phase 3 (BT-2782): surface-parity entries for MCP.
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            tool_names.contains(&"precheck_method"),
            "precheck_method should be in tool list, found: {tool_names:?}"
        );
        assert!(
            tool_names.contains(&"recheck_image"),
            "recheck_image should be in tool list, found: {tool_names:?}"
        );
    }

    #[test]
    fn list_modules_tool_not_registered() {
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            !tool_names.contains(&"list_modules"),
            "list_modules should not be in tool list (removed in this PR)"
        );
    }

    // BT-2369 (ADR 0081 Phase 6): the get_bindings / clear tools were removed —
    // session state is read and reset via `evaluate` (`Session current bindings
    // keys`, `Session current clear`).
    #[test]
    fn session_state_tools_not_registered() {
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            !tool_names.contains(&"get_bindings"),
            "get_bindings should not be in tool list (removed; use evaluate)"
        );
        assert!(
            !tool_names.contains(&"clear"),
            "clear should not be in tool list (removed; use evaluate)"
        );
    }

    // --- tool registration: package tools (BT-1658, ADR 0070 Phase 5) ---

    #[test]
    fn list_packages_tool_registered() {
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            tool_names.contains(&"list_packages"),
            "list_packages should be in tool list, found: {tool_names:?}"
        );
    }

    #[test]
    fn package_classes_tool_registered() {
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            tool_names.contains(&"package_classes"),
            "package_classes should be in tool list, found: {tool_names:?}"
        );
    }

    // --- diagnostic_summary (BT-2014) ---

    #[test]
    fn diagnostic_summary_tool_registered() {
        let router = BeamtalkMcp::tool_router();
        let tools = router.list_all();
        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_ref()).collect();
        assert!(
            tool_names.contains(&"diagnostic_summary"),
            "diagnostic_summary should be in tool list, found: {tool_names:?}"
        );
    }

    #[test]
    fn compute_diagnostic_summary_nonexistent_path() {
        let result = compute_diagnostic_summary("/nonexistent/path/that/does/not/exist");
        assert_eq!(result["files_checked"], 0);
        assert_eq!(result["total"], 0);
        assert!(result["error"].is_string());
    }

    #[test]
    fn compute_diagnostic_summary_clean_file() {
        // A well-formed source file should produce a summary with files_checked=1.
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("clean.bt");
        std::fs::write(&file, "Object subclass: Clean\n  class hello => 42\n").unwrap();
        let result = compute_diagnostic_summary(file.to_str().unwrap());
        assert_eq!(result["files_checked"], 1);
        // Total may include some diagnostics from semantic analysis depending
        // on the class environment, but files_checked must be correct.
        assert!(result["total"].is_number());
        assert!(result["type_coverage"]["typed"].is_number());
        assert!(result["type_coverage"]["total"].is_number());
        assert!(result["type_coverage"]["dynamic_percent"].is_number());
    }

    // --- ADR 0082 Phase 3 (BT-2288): MCP ChangeLog / flush tool wiring ---
    //
    // The tools below pin the Beamtalk expression each MCP tool dispatches to,
    // matching the REPL meta-command tests in
    // crates/beamtalk-cli/src/commands/repl/mod.rs. Surface drift in the
    // expression mapping fails CI through these tests.

    #[test]
    fn save_method_expr_compiles_durable_patch() {
        // `save_method` → `aClass compile: #selector source: body`.
        assert_eq!(
            save_method_expr("Counter", "increment", "self.value := self.value + 1"),
            "Counter compile: #increment source: \"self.value := self.value + 1\"",
        );
    }

    #[test]
    fn save_method_expr_escapes_body_quotes_and_braces() {
        // Interpolation braces in the body must be neutralised — the body is
        // a String value, not interpolated source.
        assert_eq!(
            save_method_expr("Greeter", "greet", "\"Hello, {name}\""),
            "Greeter compile: #greet source: \"\\\"Hello, \\{name}\\\"\"",
        );
    }

    #[test]
    fn save_method_expr_preserves_keyword_selectors() {
        assert_eq!(
            save_method_expr("Dict", "at:put:", "..."),
            "Dict compile: #at:put: source: \"...\"",
        );
    }

    #[test]
    fn try_method_expr_compiles_ephemeral_patch() {
        // `try_method` → `aClass tryCompile: #selector source: body`.
        assert_eq!(
            try_method_expr("Counter", "doubled", "^ self value * 2"),
            "Counter tryCompile: #doubled source: \"^ self value * 2\"",
        );
    }

    // `save_class_expr`, `precheck_method_expr`, `remove_method_expr`,
    // `remove_method_if_absent_expr`, and `flush_expr`/`FlushFilter` are
    // defined in `beamtalk_core::tool_expr` (BT-3193) and golden-tested
    // there — that suite is the single source of truth both this crate and
    // `beamtalk-lsp` call into, so there is nothing left to re-test here.

    // --- compute_doc_method_categories (BT-3239) ---

    #[test]
    fn compute_doc_method_categories_groups_by_divider() {
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("counter.bt");
        std::fs::write(
            &file,
            "Object subclass: Counter\n\
             \n\
             \x20\x20// === Construction ===\n\
             \x20\x20class new => self basicNew\n\
             \n\
             \x20\x20// === Arithmetic ===\n\
             \x20\x20increment => self.value := self.value + 1\n",
        )
        .unwrap();

        let result = compute_doc_method_categories(file.to_str().unwrap(), "Counter")
            .expect("categorization should succeed");
        assert_eq!(result["class"], "Counter");
        let categories = result["categories"].as_array().unwrap();
        assert_eq!(categories.len(), 2);
        assert_eq!(categories[0]["name"], "Construction");
        assert_eq!(categories[0]["methods"][0]["selector"], "new");
        assert_eq!(categories[0]["methods"][0]["side"], "class");
        assert_eq!(categories[1]["name"], "Arithmetic");
        assert_eq!(categories[1]["methods"][0]["selector"], "increment");
        assert_eq!(categories[1]["methods"][0]["side"], "instance");
    }

    #[test]
    fn compute_doc_method_categories_no_dividers_is_single_unnamed_category() {
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("plain.bt");
        std::fs::write(&file, "Object subclass: Plain\n  foo => 1\n").unwrap();

        let result = compute_doc_method_categories(file.to_str().unwrap(), "Plain")
            .expect("categorization should succeed");
        let categories = result["categories"].as_array().unwrap();
        assert_eq!(categories.len(), 1);
        assert!(categories[0]["name"].is_null());
    }

    #[test]
    fn compute_doc_method_categories_missing_file_is_none() {
        assert!(compute_doc_method_categories("/nonexistent/path/nope.bt", "Counter").is_none());
    }

    #[test]
    fn compute_doc_method_categories_class_not_found_is_none() {
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("counter.bt");
        std::fs::write(&file, "Object subclass: Counter\n  foo => 1\n").unwrap();

        assert!(compute_doc_method_categories(file.to_str().unwrap(), "NoSuchClass").is_none());
    }

    // ------------------------------------------------------------------
    // MCP tool handlers against a fake REPL (BT-3324)
    // ------------------------------------------------------------------
    //
    // `server.rs`'s tool handlers take a concrete `Arc<ReplClient>`, not a
    // trait object — the same shape BT-3325 found in beamtalk-lsp's
    // runtime.rs, with no mockable seam to introduce. Rather than add one
    // neither the CLI nor the REPL wire protocol needs, this fake stands in
    // for a real REPL server: a loopback WebSocket listener that performs the
    // ADR 0020 auth handshake and then answers requests through a per-test
    // responder closure, so a real `ReplClient` connects to it exactly as it
    // would to `beamtalk repl` — and the tool handler methods below run
    // as ordinary async fns against it, unignored and BEAM-free.
    // BT-3331: the loopback WS server performing the ADR 0020 handshake
    // (listener bind, handshake frames, request/response loop) used to be
    // hand-rolled here; it's now shared with `beamtalk-lsp`'s equivalent
    // fake workspace via `beamtalk_repl_protocol::test_support` (see that
    // module's doc comment for the full extraction rationale). `FakeRepl`
    // aliases the shared server type under this file's existing name, so
    // every call site below (`fake.port`) is unchanged.
    use beamtalk_repl_protocol::test_support::{HandshakeMode, spawn as spawn_ws, text};

    type FakeRepl = beamtalk_repl_protocol::test_support::FakeWsServer;

    /// Frame the fake REPL sends back for one received request.
    type FakeReplResponder = Box<dyn Fn(&serde_json::Value) -> serde_json::Value + Send + Sync>;

    /// Spawn a fake REPL on an ephemeral loopback port. Performs the ADR 0020
    /// handshake (`auth-required` -> client `auth` -> `auth_ok` ->
    /// `session-started`) unconditionally — handshake robustness itself is
    /// `client.rs`'s concern (covered live via `just test-mcp`) — then
    /// answers every subsequent request via `responder`, defaulting `id`
    /// (echoed from the request) and `status` (`["done"]`) when the
    /// responder didn't set them.
    async fn spawn_fake_repl(responder: FakeReplResponder) -> FakeRepl {
        let responder: beamtalk_repl_protocol::test_support::Responder = Box::new(move |request| {
            let mut reply = responder(request);
            if reply.get("id").is_none() {
                reply["id"] = request
                    .get("id")
                    .cloned()
                    .unwrap_or(serde_json::Value::Null);
            }
            if reply.get("status").is_none() {
                reply["status"] = serde_json::json!(["done"]);
            }
            vec![text(&reply)]
        });
        spawn_ws(HandshakeMode::Ok, "fake-session", responder).await
    }

    /// A responder that always answers with `value` in the response's
    /// top-level `value` field (the shape every `evaluate`-backed tool reads).
    fn respond_value(value: serde_json::Value) -> FakeReplResponder {
        Box::new(move |_req| serde_json::json!({"value": value.clone()}))
    }

    /// A responder that returns an arbitrary response object verbatim, for
    /// shaping fields `respond_value` doesn't cover (`class_list`, `actors`,
    /// `completions`, `errors`, …).
    fn respond(response: serde_json::Value) -> FakeReplResponder {
        Box::new(move |_req| response.clone())
    }

    /// A responder that always errors with `message`.
    fn respond_error(message: &'static str) -> FakeReplResponder {
        Box::new(move |_req| serde_json::json!({"status": ["done", "error"], "error": message}))
    }

    /// A responder that dispatches on the request's `code` field (the
    /// `evaluate` payload) — for handlers like `list_packages` that issue
    /// more than one distinct `evaluate` call per tool invocation.
    fn respond_by_code(cases: Vec<(&'static str, serde_json::Value)>) -> FakeReplResponder {
        Box::new(move |req| {
            let code = req.get("code").and_then(|v| v.as_str()).unwrap_or("");
            cases
                .iter()
                .find(|(prefix, _)| code.starts_with(prefix))
                .map_or_else(
                    || serde_json::json!({"value": serde_json::Value::Null}),
                    |(_, v)| v.clone(),
                )
        })
    }

    /// Connect a real `ReplClient` to a fake REPL and wrap it in a
    /// `BeamtalkMcp`. The `FakeRepl` handle must outlive the returned server
    /// — dropping it aborts the listener task, which owns the socket.
    async fn fake_mcp(responder: FakeReplResponder) -> (FakeRepl, BeamtalkMcp) {
        let fake = spawn_fake_repl(responder).await;
        let client = ReplClient::connect(fake.port, "test-cookie", None)
            .await
            .expect("fake REPL handshake should succeed");
        (fake, BeamtalkMcp::new(Arc::new(client)))
    }

    /// Concatenate a `CallToolResult`'s text content blocks for substring
    /// assertions, ignoring non-text blocks (none of these tools emit any).
    fn call_text(result: &CallToolResult) -> String {
        result
            .content
            .iter()
            .filter_map(ContentBlock::as_text)
            .map(|t| t.text.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    }

    // --- evaluate ---

    #[tokio::test]
    async fn evaluate_returns_value_and_output() {
        let (_fake, mcp) =
            fake_mcp(respond(serde_json::json!({"value": "3", "output": "hi\n"}))).await;
        let result = mcp
            .evaluate(Parameters(EvaluateParams {
                code: "1 + 2".to_string(),
                trace: None,
            }))
            .await
            .expect("evaluate should not raise a protocol error");
        assert_eq!(result.is_error, Some(false));
        let text = call_text(&result);
        assert!(text.contains('3'), "expected value in {text:?}");
        assert!(text.contains("Output: hi"), "expected output in {text:?}");
    }

    #[tokio::test]
    async fn evaluate_trace_renders_steps() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "steps": [{"src": "1 + 2", "value": "3"}]
        })))
        .await;
        let result = mcp
            .evaluate(Parameters(EvaluateParams {
                code: "1 + 2".to_string(),
                trace: Some(true),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(!call_text(&result).is_empty());
    }

    #[tokio::test]
    async fn evaluate_error_includes_line_and_hint() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "status": ["done", "error"],
            "error": "boom",
            "line": 3,
            "hint": "try again"
        })))
        .await;
        let result = mcp
            .evaluate(Parameters(EvaluateParams {
                code: "bogus".to_string(),
                trace: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
        let text = call_text(&result);
        assert!(text.contains("boom"), "expected error message in {text:?}");
    }

    // --- complete ---

    #[tokio::test]
    async fn complete_returns_joined_completions() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "completions": ["size", "sqrt"]
        })))
        .await;
        let result = mcp
            .complete(Parameters(CompleteParams {
                code: "3 s".to_string(),
                cursor: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        let text = call_text(&result);
        assert!(text.contains("size") && text.contains("sqrt"));
    }

    #[tokio::test]
    async fn complete_empty_reports_no_completions() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"completions": []}))).await;
        let result = mcp
            .complete(Parameters(CompleteParams {
                code: "zzz".to_string(),
                cursor: Some(3),
            }))
            .await
            .unwrap();
        assert_eq!(call_text(&result), "No completions available");
    }

    // --- load_project ---

    #[tokio::test]
    async fn load_project_success_lists_classes_and_summary() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "classes": ["Counter", "Greeter"],
            "summary": "2 files compiled"
        })))
        .await;
        let result = mcp
            .load_project(Parameters(LoadProjectParams {
                path: ".".to_string(),
                include_tests: Some(true),
                force: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        let text = call_text(&result);
        assert!(text.contains("Counter") && text.contains("Greeter"));
        assert!(text.contains("2 files compiled"));
    }

    #[tokio::test]
    async fn load_project_partial_errors_reports_failed_files() {
        // `errors` non-empty with an overall `status: done` (no top-level
        // error flag) is the per-file-partial-failure shape `check_response!`
        // does not catch — a distinct branch from a fully-failed response.
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "classes": ["Counter"],
            "errors": [{"path": "src/bad.bt", "line": 4, "message": "parse error", "hint": "check syntax"}],
            "summary": "1 of 2 files compiled"
        })))
        .await;
        let result = mcp
            .load_project(Parameters(LoadProjectParams {
                path: ".".to_string(),
                include_tests: None,
                force: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
        let text = call_text(&result);
        assert!(text.contains("src/bad.bt"));
        assert!(text.contains("parse error"));
        assert!(text.contains("check syntax"));
        assert!(text.contains("Counter"));
    }

    #[tokio::test]
    async fn load_project_full_failure_is_error() {
        let (_fake, mcp) = fake_mcp(respond_error("path does not exist")).await;
        let result = mcp
            .load_project(Parameters(LoadProjectParams {
                path: "/nope".to_string(),
                include_tests: None,
                force: Some(true),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
        assert!(call_text(&result).contains("path does not exist"));
    }

    // --- load_file ---

    #[tokio::test]
    async fn load_file_success_with_warnings() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "value": "Counter",
            "warnings": ["unused variable x"]
        })))
        .await;
        let result = mcp
            .load_file(Parameters(LoadFileParams {
                path: "src/counter.bt".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        let text = call_text(&result);
        assert!(text.contains("Counter"));
        assert!(text.contains("unused variable x"));
    }

    #[tokio::test]
    async fn load_file_error() {
        let (_fake, mcp) = fake_mcp(respond_error("file not found")).await;
        let result = mcp
            .load_file(Parameters(LoadFileParams {
                path: "src/missing.bt".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- inspect ---

    #[tokio::test]
    async fn inspect_string_state_is_used_verbatim() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"state": "a nice actor"}))).await;
        let result = mcp
            .inspect(Parameters(InspectParams {
                actor: "<0.1.0>".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(call_text(&result), "a nice actor");
    }

    #[tokio::test]
    async fn inspect_object_state_is_pretty_printed() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"state": {"count": 3}}))).await;
        let result = mcp
            .inspect(Parameters(InspectParams {
                actor: "<0.1.0>".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("count"));
    }

    #[tokio::test]
    async fn inspect_no_state_available() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .inspect(Parameters(InspectParams {
                actor: "<0.1.0>".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(call_text(&result), "No state available");
    }

    #[tokio::test]
    async fn inspect_error() {
        let (_fake, mcp) = fake_mcp(respond_error("no such actor")).await;
        let result = mcp
            .inspect(Parameters(InspectParams {
                actor: "<0.999.0>".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- list_actors ---

    #[tokio::test]
    async fn list_actors_success() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "actors": [{"pid": "<0.1.0>", "class": "Counter", "module": "bt@counter", "spawned_at": 0}]
        })))
        .await;
        let result = mcp.list_actors().await.unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(call_text(&result).contains("Counter"));
    }

    #[tokio::test]
    async fn list_actors_error() {
        let (_fake, mcp) = fake_mcp(respond_error("workspace unavailable")).await;
        let result = mcp.list_actors().await.unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- supervision_tree ---

    #[tokio::test]
    async fn supervision_tree_default_scope() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("#()"))).await;
        let result = mcp
            .supervision_tree(Parameters(SupervisionTreeParams { scope: None }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
    }

    #[tokio::test]
    async fn supervision_tree_system_scope() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("#()"))).await;
        let result = mcp
            .supervision_tree(Parameters(SupervisionTreeParams {
                scope: Some("system".to_string()),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
    }

    #[tokio::test]
    async fn supervision_tree_error() {
        let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
        let result = mcp
            .supervision_tree(Parameters(SupervisionTreeParams { scope: None }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- list_classes ---

    #[tokio::test]
    async fn list_classes_success() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "class_list": [{"name": "Counter", "superclass": "Object", "doc": "a counter", "sealed": false, "abstract": false}]
        })))
        .await;
        let result = mcp
            .list_classes(Parameters(ListClassesParams { filter: None }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(call_text(&result).contains("Counter"));
    }

    #[tokio::test]
    async fn list_classes_error() {
        let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
        let result = mcp
            .list_classes(Parameters(ListClassesParams {
                filter: Some("stdlib".to_string()),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- reload_class ---

    #[tokio::test]
    async fn reload_class_success_default_message() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .reload_class(Parameters(ReloadClassParams {
                class: "Counter".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(call_text(&result), "Class reloaded successfully");
    }

    #[tokio::test]
    async fn reload_class_rejects_invalid_class_name() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let err = mcp
            .reload_class(Parameters(ReloadClassParams {
                class: "not_a_class".to_string(),
            }))
            .await
            .expect_err("lowercase class name should be rejected before touching the client");
        assert!(err.message.contains("Invalid class name"));
    }

    #[tokio::test]
    async fn reload_class_error() {
        let (_fake, mcp) = fake_mcp(respond_error("migration failed")).await;
        let result = mcp
            .reload_class(Parameters(ReloadClassParams {
                class: "Counter".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- docs ---

    #[tokio::test]
    async fn docs_class_lookup() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("Counter docs"))).await;
        let result = mcp
            .docs(Parameters(DocsParams {
                class: Some("Counter".to_string()),
                erlang_module: None,
                selector: None,
            }))
            .await
            .unwrap();
        // `docs`' success path builds `CallToolResult` via `::default()` (to
        // also carry `structured_content`), so `is_error` is `None` on
        // success rather than `Some(false)` — unlike the `success()`
        // constructor most other tools use.
        assert_ne!(result.is_error, Some(true));
        assert!(call_text(&result).contains("Counter docs"));
    }

    #[tokio::test]
    async fn docs_erlang_module_with_selector() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("lists:map/2 docs"))).await;
        let result = mcp
            .docs(Parameters(DocsParams {
                class: None,
                erlang_module: Some("lists".to_string()),
                selector: Some("map".to_string()),
            }))
            .await
            .unwrap();
        assert_ne!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn docs_rejects_both_class_and_erlang_module() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let err = mcp
            .docs(Parameters(DocsParams {
                class: Some("Counter".to_string()),
                erlang_module: Some("lists".to_string()),
                selector: None,
            }))
            .await
            .expect_err("both class and erlang_module should be rejected");
        assert!(err.message.contains("either"));
    }

    #[tokio::test]
    async fn docs_rejects_neither_class_nor_erlang_module() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let err = mcp
            .docs(Parameters(DocsParams {
                class: None,
                erlang_module: None,
                selector: None,
            }))
            .await
            .expect_err("neither class nor erlang_module should be rejected");
        assert!(err.message.contains("Provide"));
    }

    #[tokio::test]
    async fn docs_error() {
        let (_fake, mcp) = fake_mcp(respond_error("no docs")).await;
        let result = mcp
            .docs(Parameters(DocsParams {
                class: Some("Counter".to_string()),
                erlang_module: None,
                selector: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- unload ---

    #[tokio::test]
    async fn unload_success() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .unload(Parameters(UnloadParams {
                class: "Counter".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("Counter"));
    }

    #[tokio::test]
    async fn unload_rejects_invalid_class_name() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let err = mcp
            .unload(Parameters(UnloadParams {
                class: "bad".to_string(),
            }))
            .await
            .expect_err("lowercase class name should be rejected");
        assert!(err.message.contains("Invalid class name"));
    }

    #[tokio::test]
    async fn unload_error() {
        let (_fake, mcp) = fake_mcp(respond_error("class in use")).await;
        let result = mcp
            .unload(Parameters(UnloadParams {
                class: "Counter".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- interrupt ---

    #[tokio::test]
    async fn interrupt_success() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp.interrupt().await.unwrap();
        assert_eq!(call_text(&result), "Interrupt sent");
    }

    #[tokio::test]
    async fn interrupt_error() {
        let (_fake, mcp) = fake_mcp(respond_error("nothing running")).await;
        let result = mcp.interrupt().await.unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- show_codegen ---

    #[tokio::test]
    async fn show_codegen_from_code_snippet() {
        let (_fake, mcp) =
            fake_mcp(respond(serde_json::json!({"core_erlang": "'add'/2 = ..."}))).await;
        let result = mcp
            .show_codegen(Parameters(ShowCodegenParams {
                code: Some("1 + 2".to_string()),
                class: None,
                selector: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(call_text(&result).contains("add"));
    }

    #[tokio::test]
    async fn show_codegen_from_class_with_warnings() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "core_erlang": "module 'counter'",
            "warnings": ["deprecated selector"]
        })))
        .await;
        let result = mcp
            .show_codegen(Parameters(ShowCodegenParams {
                code: None,
                class: Some("Counter".to_string()),
                selector: Some("increment".to_string()),
            }))
            .await
            .unwrap();
        let text = call_text(&result);
        assert!(text.contains("module 'counter'"));
        assert!(text.contains("deprecated selector"));
    }

    #[tokio::test]
    async fn show_codegen_rejects_selector_without_class() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .show_codegen(Parameters(ShowCodegenParams {
                code: None,
                class: None,
                selector: Some("increment".to_string()),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
        assert!(call_text(&result).contains("requires"));
    }

    #[tokio::test]
    async fn show_codegen_rejects_neither_code_nor_class() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .show_codegen(Parameters(ShowCodegenParams {
                code: None,
                class: None,
                selector: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
        assert!(call_text(&result).contains("Provide"));
    }

    #[tokio::test]
    async fn show_codegen_error() {
        let (_fake, mcp) = fake_mcp(respond_error("compile failed")).await;
        let result = mcp
            .show_codegen(Parameters(ShowCodegenParams {
                code: Some("bogus".to_string()),
                class: None,
                selector: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- test ---

    #[tokio::test]
    async fn test_by_class_success() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "results": {"passed": 3, "failed": 0}
        })))
        .await;
        let result = mcp
            .test(Parameters(TestParams {
                class: Some("CounterTest".to_string()),
                file: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(call_text(&result).contains("passed"));
    }

    #[tokio::test]
    async fn test_by_file_and_all_default() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "results": {"passed": 1, "failed": 0}
        })))
        .await;
        let by_file = mcp
            .test(Parameters(TestParams {
                class: None,
                file: Some("test/counter_test.bt".to_string()),
            }))
            .await
            .unwrap();
        assert_eq!(by_file.is_error, Some(false));

        let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({
            "results": {"passed": 1, "failed": 0}
        })))
        .await;
        let all = mcp2
            .test(Parameters(TestParams {
                class: None,
                file: None,
            }))
            .await
            .unwrap();
        assert_eq!(all.is_error, Some(false));
    }

    #[tokio::test]
    async fn test_rejects_class_and_file_together() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .test(Parameters(TestParams {
                class: Some("CounterTest".to_string()),
                file: Some("test/counter_test.bt".to_string()),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
        assert!(call_text(&result).contains("mutually exclusive"));
    }

    #[tokio::test]
    async fn test_failures_are_reported_as_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "status": ["done", "test-error"],
            "results": {"passed": 1, "failed": 1}
        })))
        .await;
        let result = mcp
            .test(Parameters(TestParams {
                class: None,
                file: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
        assert!(call_text(&result).contains("TEST FAILURES"));
    }

    #[tokio::test]
    async fn test_execution_error() {
        let (_fake, mcp) = fake_mcp(respond_error("no such class")).await;
        let result = mcp
            .test(Parameters(TestParams {
                class: Some("NoSuch".to_string()),
                file: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- lint / diagnostic_summary (offline — no REPL touched) ---

    #[tokio::test]
    async fn lint_tool_reports_clean_file() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("clean.bt");
        std::fs::write(&file, "Object subclass: Clean\n  foo => 1\n").unwrap();
        let result = mcp
            .lint(Parameters(LintParams {
                path: Some(file.to_str().unwrap().to_string()),
            }))
            .await
            .unwrap();
        assert_ne!(result.is_error, Some(true));
        assert!(result.structured_content.is_some());
    }

    #[tokio::test]
    async fn lint_tool_reports_errors_for_nonexistent_path() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .lint(Parameters(LintParams {
                path: Some("/nonexistent/path/nope".to_string()),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn diagnostic_summary_tool_runs_offline() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let temp = tempfile::TempDir::new().unwrap();
        let file = temp.path().join("clean.bt");
        std::fs::write(&file, "Object subclass: Clean\n  foo => 1\n").unwrap();
        let result = mcp
            .diagnostic_summary(Parameters(DiagnosticSummaryParams {
                path: Some(file.to_str().unwrap().to_string()),
            }))
            .await
            .unwrap();
        // Like `docs`, `diagnostic_summary` builds its `CallToolResult` via
        // `::default()` and never sets `is_error` — it never fails.
        assert_ne!(result.is_error, Some(true));
        assert!(result.structured_content.is_some());
    }

    // --- search_examples / search_classes (offline — no REPL touched) ---

    #[tokio::test]
    async fn search_examples_finds_results_for_a_known_topic() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .search_examples(Parameters(SearchExamplesParams {
                query: "closures".to_string(),
                limit: Some(3),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(!call_text(&result).contains("No examples found"));
    }

    #[tokio::test]
    async fn search_examples_reports_no_results() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .search_examples(Parameters(SearchExamplesParams {
                query: "zzznonexistentqueryxyz999".to_string(),
                limit: None,
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("No examples found"));
    }

    #[tokio::test]
    async fn search_classes_finds_results_for_a_known_topic() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .search_classes(Parameters(SearchClassesParams {
                query: "collection".to_string(),
                limit: Some(3),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(!call_text(&result).contains("No classes found"));
    }

    #[tokio::test]
    async fn search_classes_reports_no_results() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .search_classes(Parameters(SearchClassesParams {
                query: "zzznonexistentqueryxyz999".to_string(),
                limit: None,
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("No classes found"));
    }

    // --- tracing tools ---

    #[tokio::test]
    async fn enable_tracing_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let ok = mcp.enable_tracing().await.unwrap();
        assert_eq!(ok.is_error, Some(false));

        let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
        let error = mcp2.enable_tracing().await.unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    #[tokio::test]
    async fn disable_tracing_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let ok = mcp.disable_tracing().await.unwrap();
        assert_eq!(ok.is_error, Some(false));

        let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
        let error = mcp2.disable_tracing().await.unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    #[tokio::test]
    async fn get_traces_returns_value_or_placeholder() {
        let (_fake, mcp) = fake_mcp(respond(
            serde_json::json!({"value": [{"actor": "<0.1.0>"}]}),
        ))
        .await;
        let result = mcp
            .get_traces(Parameters(GetTracesParams {
                actor: Some("<0.1.0>".to_string()),
                selector: None,
                class: None,
                outcome: None,
                min_duration_ns: Some(1000),
                limit: Some(10),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("0.1.0"));

        let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({}))).await;
        let empty = mcp2
            .get_traces(Parameters(GetTracesParams {
                actor: None,
                selector: None,
                class: None,
                outcome: None,
                min_duration_ns: None,
                limit: None,
            }))
            .await
            .unwrap();
        assert!(call_text(&empty).contains("No traces captured"));
    }

    #[tokio::test]
    async fn get_traces_error() {
        let (_fake, mcp) = fake_mcp(respond_error("tracing disabled")).await;
        let result = mcp
            .get_traces(Parameters(GetTracesParams {
                actor: None,
                selector: None,
                class: None,
                outcome: None,
                min_duration_ns: None,
                limit: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn export_traces_returns_value_or_placeholder() {
        let (_fake, mcp) = fake_mcp(respond(
            serde_json::json!({"value": {"path": "t.json", "count": 2}}),
        ))
        .await;
        let result = mcp
            .export_traces(Parameters(ExportTracesParams {
                path: Some("t.json".to_string()),
                actor: None,
                selector: None,
                class: None,
                outcome: None,
                min_duration_ns: None,
                limit: None,
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("t.json"));

        let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({}))).await;
        let empty = mcp2
            .export_traces(Parameters(ExportTracesParams {
                path: None,
                actor: None,
                selector: None,
                class: None,
                outcome: None,
                min_duration_ns: None,
                limit: None,
            }))
            .await
            .unwrap();
        assert!(call_text(&empty).contains("No traces to export"));
    }

    #[tokio::test]
    async fn export_traces_error() {
        let (_fake, mcp) = fake_mcp(respond_error("disk full")).await;
        let result = mcp
            .export_traces(Parameters(ExportTracesParams {
                path: None,
                actor: None,
                selector: None,
                class: None,
                outcome: None,
                min_duration_ns: None,
                limit: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn actor_stats_returns_value_or_placeholder() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"value": {"calls": 5}}))).await;
        let result = mcp
            .actor_stats(Parameters(ActorStatsParams {
                actor: Some("<0.1.0>".to_string()),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("calls"));

        let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({}))).await;
        let empty = mcp2
            .actor_stats(Parameters(ActorStatsParams { actor: None }))
            .await
            .unwrap();
        assert_eq!(call_text(&empty), "No stats available.");
    }

    #[tokio::test]
    async fn actor_stats_error() {
        let (_fake, mcp) = fake_mcp(respond_error("no such actor")).await;
        let result = mcp
            .actor_stats(Parameters(ActorStatsParams { actor: None }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- describe / list_packages / package_classes ---

    #[tokio::test]
    async fn describe_reports_ops_and_versions() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({
            "ops": ["eval", "complete"],
            "versions": {"protocol": 1}
        })))
        .await;
        let result = mcp.describe().await.unwrap();
        assert_eq!(result.is_error, Some(false));
        let text = call_text(&result);
        assert!(text.contains("eval"));
        assert!(text.contains("protocol"));
    }

    #[tokio::test]
    async fn describe_empty_response_has_fallback_text() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp.describe().await.unwrap();
        assert_eq!(call_text(&result), "No describe information available");
    }

    #[tokio::test]
    async fn describe_error() {
        let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
        let result = mcp.describe().await.unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn list_packages_success_with_details() {
        let (_fake, mcp) = fake_mcp(respond_by_code(vec![
            (
                "Package all collect:",
                serde_json::json!({"value": "stdlib v1 (10 classes)"}),
            ),
            ("Package all", serde_json::json!({"value": "#(\"stdlib\")"})),
        ]))
        .await;
        let result = mcp.list_packages().await.unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(call_text(&result).contains("stdlib v1"));
    }

    #[tokio::test]
    async fn list_packages_detail_failure_falls_back_to_names() {
        let (_fake, mcp) = fake_mcp(respond_by_code(vec![
            (
                "Package all collect:",
                serde_json::json!({"status": ["done", "error"], "error": "boom"}),
            ),
            ("Package all", serde_json::json!({"value": "#(\"stdlib\")"})),
        ]))
        .await;
        let result = mcp.list_packages().await.unwrap();
        assert_eq!(result.is_error, Some(false));
        assert!(call_text(&result).contains("stdlib"));
    }

    #[tokio::test]
    async fn list_packages_none_loaded() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({"value": "nil"}))).await;
        let result = mcp.list_packages().await.unwrap();
        assert_eq!(call_text(&result), "No packages loaded");
    }

    #[tokio::test]
    async fn list_packages_error() {
        let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
        let result = mcp.list_packages().await.unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn package_classes_success() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("#(\"Counter\")"))).await;
        let result = mcp
            .package_classes(Parameters(PackageClassesParams {
                package: "stdlib".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("Counter"));
    }

    #[tokio::test]
    async fn package_classes_rejects_invalid_package_name() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let err = mcp
            .package_classes(Parameters(PackageClassesParams {
                package: "bad name!".to_string(),
            }))
            .await
            .expect_err("package name with spaces/punctuation should be rejected");
        assert!(err.message.contains("Invalid package name"));
    }

    #[tokio::test]
    async fn package_classes_empty_package_reports_not_loaded() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("nil"))).await;
        let result = mcp
            .package_classes(Parameters(PackageClassesParams {
                package: "missing_pkg".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("No classes found"));
    }

    #[tokio::test]
    async fn package_classes_error() {
        let (_fake, mcp) = fake_mcp(respond_error("nope")).await;
        let result = mcp
            .package_classes(Parameters(PackageClassesParams {
                package: "stdlib".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- save_method / try_method / save_class ---

    #[tokio::test]
    async fn save_method_success_default_message() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .save_method(Parameters(SaveMethodParams {
                class: "Counter".to_string(),
                selector: "#increment".to_string(),
                body: "self value: self value + 1".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("Counter"));
        assert!(call_text(&result).contains("increment"));
    }

    #[tokio::test]
    async fn save_method_rejects_invalid_selector() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let err = mcp
            .save_method(Parameters(SaveMethodParams {
                class: "Counter".to_string(),
                selector: "bad selector".to_string(),
                body: "1".to_string(),
            }))
            .await
            .expect_err("selector with a space should be rejected");
        assert!(err.message.contains("selector"));
    }

    #[tokio::test]
    async fn save_method_error() {
        let (_fake, mcp) = fake_mcp(respond_error("compile error")).await;
        let result = mcp
            .save_method(Parameters(SaveMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                body: "bogus".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn try_method_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let ok = mcp
            .try_method(Parameters(TryMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                body: "1".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&ok).contains("ephemeral"));

        let (_fake2, mcp2) = fake_mcp(respond_error("compile error")).await;
        let error = mcp2
            .try_method(Parameters(TryMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                body: "bogus".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    #[tokio::test]
    async fn save_class_success() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .save_class(Parameters(SaveClassParams {
                source: "Object subclass: Greeter\n  greet => \"hi\"".to_string(),
                path: "src/greeter.bt".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("src/greeter.bt"));
    }

    #[tokio::test]
    async fn save_class_rejects_empty_path_and_source() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let empty_path = mcp
            .save_class(Parameters(SaveClassParams {
                source: "Object subclass: Greeter".to_string(),
                path: String::new(),
            }))
            .await
            .expect_err("empty path should be rejected");
        assert!(empty_path.message.contains("path"));

        let (_fake2, mcp2) = fake_mcp(respond(serde_json::json!({}))).await;
        let empty_source = mcp2
            .save_class(Parameters(SaveClassParams {
                source: String::new(),
                path: "src/greeter.bt".to_string(),
            }))
            .await
            .expect_err("empty source should be rejected");
        assert!(empty_source.message.contains("source"));
    }

    #[tokio::test]
    async fn save_class_error() {
        let (_fake, mcp) = fake_mcp(respond_error("already exists")).await;
        let result = mcp
            .save_class(Parameters(SaveClassParams {
                source: "Object subclass: Greeter".to_string(),
                path: "src/greeter.bt".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    // --- remove_method / remove_class / rename_class / rename_method ---

    #[tokio::test]
    async fn remove_method_success_and_with_if_absent() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let plain = mcp
            .remove_method(Parameters(RemoveMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                if_absent: None,
            }))
            .await
            .unwrap();
        assert!(call_text(&plain).contains("removed"));

        let (_fake2, mcp2) = fake_mcp(respond_value(serde_json::json!("nil"))).await;
        let with_fallback = mcp2
            .remove_method(Parameters(RemoveMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                if_absent: Some("nil".to_string()),
            }))
            .await
            .unwrap();
        assert_eq!(with_fallback.is_error, Some(false));
    }

    #[tokio::test]
    async fn remove_method_error() {
        let (_fake, mcp) = fake_mcp(respond_error("selector_not_found")).await;
        let result = mcp
            .remove_method(Parameters(RemoveMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                if_absent: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn remove_class_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let ok = mcp
            .remove_class(Parameters(RemoveClassParams {
                class: "Counter".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&ok).contains("not yet flushed"));

        let (_fake2, mcp2) = fake_mcp(respond_error("sealed class")).await;
        let error = mcp2
            .remove_class(Parameters(RemoveClassParams {
                class: "Object".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    #[tokio::test]
    async fn rename_class_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("Accumulator"))).await;
        let ok = mcp
            .rename_class(Parameters(RenameClassParams {
                class: "Counter".to_string(),
                new_name: "Accumulator".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&ok).contains("Accumulator"));

        let (_fake2, mcp2) = fake_mcp(respond_error("name collision")).await;
        let error = mcp2
            .rename_class(Parameters(RenameClassParams {
                class: "Counter".to_string(),
                new_name: "Object".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    #[tokio::test]
    async fn rename_method_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("Counter"))).await;
        let ok = mcp
            .rename_method(Parameters(RenameMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                new_selector: "incrementBy".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&ok).contains("Counter"));

        let (_fake2, mcp2) = fake_mcp(respond_error("selector collision")).await;
        let error = mcp2
            .rename_method(Parameters(RenameMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                new_selector: "value".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    // --- flush / list_changes / dirty_methods / precheck_method / recheck_image ---

    #[tokio::test]
    async fn flush_success_no_filter() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .flush(Parameters(FlushParams {
                class: None,
                file: None,
                kind: None,
                confirm_destructive: None,
            }))
            .await
            .unwrap();
        assert_eq!(call_text(&result), "Flushed");
    }

    #[tokio::test]
    async fn flush_scoped_by_class_with_confirm_destructive() {
        let (_fake, mcp) = fake_mcp(respond_value(serde_json::json!("1 file written"))).await;
        let result = mcp
            .flush(Parameters(FlushParams {
                class: Some("Counter".to_string()),
                file: None,
                kind: None,
                confirm_destructive: Some(true),
            }))
            .await
            .unwrap();
        assert!(call_text(&result).contains("1 file written"));
    }

    #[tokio::test]
    async fn flush_scoped_by_kind() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let result = mcp
            .flush(Parameters(FlushParams {
                class: None,
                file: None,
                kind: Some("new-class".to_string()),
                confirm_destructive: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(false));
    }

    #[tokio::test]
    async fn flush_rejects_multiple_filters() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let err = mcp
            .flush(Parameters(FlushParams {
                class: Some("Counter".to_string()),
                file: Some("src/counter.bt".to_string()),
                kind: None,
                confirm_destructive: None,
            }))
            .await
            .expect_err("class + file together should be rejected");
        assert!(err.message.contains("mutually exclusive"));
    }

    #[tokio::test]
    async fn flush_rejects_invalid_kind() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let err = mcp
            .flush(Parameters(FlushParams {
                class: None,
                file: None,
                kind: Some("bad kind!".to_string()),
                confirm_destructive: None,
            }))
            .await
            .expect_err("kind with punctuation should be rejected");
        assert!(err.message.contains("identifier"));
    }

    #[tokio::test]
    async fn flush_error() {
        let (_fake, mcp) = fake_mcp(respond_error("conflict")).await;
        let result = mcp
            .flush(Parameters(FlushParams {
                class: None,
                file: None,
                kind: None,
                confirm_destructive: None,
            }))
            .await
            .unwrap();
        assert_eq!(result.is_error, Some(true));
    }

    #[tokio::test]
    async fn list_changes_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let ok = mcp.list_changes().await.unwrap();
        assert_eq!(call_text(&ok), "No changes");

        let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
        let error = mcp2.list_changes().await.unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    #[tokio::test]
    async fn dirty_methods_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let ok = mcp.dirty_methods().await.unwrap();
        assert_eq!(call_text(&ok), "No dirty methods");

        let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
        let error = mcp2.dirty_methods().await.unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    #[tokio::test]
    async fn precheck_method_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let ok = mcp
            .precheck_method(Parameters(PrecheckMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                body: "1".to_string(),
            }))
            .await
            .unwrap();
        assert!(call_text(&ok).contains("no findings"));

        let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
        let error = mcp2
            .precheck_method(Parameters(PrecheckMethodParams {
                class: "Counter".to_string(),
                selector: "increment".to_string(),
                body: "1".to_string(),
            }))
            .await
            .unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    #[tokio::test]
    async fn recheck_image_success_and_error() {
        let (_fake, mcp) = fake_mcp(respond(serde_json::json!({}))).await;
        let ok = mcp.recheck_image().await.unwrap();
        assert!(call_text(&ok).contains("no findings"));

        let (_fake2, mcp2) = fake_mcp(respond_error("nope")).await;
        let error = mcp2.recheck_image().await.unwrap();
        assert_eq!(error.is_error, Some(true));
    }

    // --- tool_router (the "tool listing" dispatch surface) ---
    //
    // `#[tool_router]` generates `call_tool`/`list_tools` on `ServerHandler`
    // itself, but those need a live `RequestContext<RoleServer>` (a `Peer`
    // wired to a real transport) to invoke — plumbing that belongs to rmcp's
    // own test suite, not ours. `ToolRouter::list_all`/`get`/`has_route` give
    // the same registry data without that transport dependency, so the tool
    // list every `#[tool]` method above populates is covered directly.

    #[test]
    fn tool_router_registers_every_tool_exactly_once() {
        let router = BeamtalkMcp::tool_router();
        let names: Vec<String> = router
            .list_all()
            .iter()
            .map(|t| t.name.to_string())
            .collect();

        let expected = [
            "evaluate",
            "complete",
            "load_project",
            "load_file",
            "inspect",
            "list_actors",
            "supervision_tree",
            "list_classes",
            "reload_class",
            "docs",
            "unload",
            "interrupt",
            "show_codegen",
            "test",
            "lint",
            "diagnostic_summary",
            "search_examples",
            "search_classes",
            "enable_tracing",
            "disable_tracing",
            "get_traces",
            "export_traces",
            "actor_stats",
            "describe",
            "list_packages",
            "package_classes",
            "save_method",
            "try_method",
            "save_class",
            "remove_method",
            "remove_class",
            "rename_class",
            "rename_method",
            "flush",
            "list_changes",
            "dirty_methods",
            "precheck_method",
            "recheck_image",
        ];
        for name in expected {
            assert!(
                names.iter().any(|n| n == name),
                "tool_router should register {name:?}, got {names:?}"
            );
            assert!(router.has_route(name), "has_route({name:?}) should be true");
            assert!(
                router.get(name).is_some(),
                "get({name:?}) should find a Tool"
            );
        }
        assert_eq!(
            names.len(),
            expected.len(),
            "tool_router registered an unexpected tool — update this test's `expected` list \
             alongside any new #[tool] handler, got {names:?}"
        );
    }

    #[test]
    fn tool_router_rejects_unknown_tool_name() {
        let router = BeamtalkMcp::tool_router();
        assert!(!router.has_route("no_such_tool"));
        assert!(router.get("no_such_tool").is_none());
    }

    // --- ServerHandler::get_info ---

    #[test]
    fn get_info_advertises_tool_capabilities_and_instructions() {
        // get_info is synchronous and never touches the client, but
        // BeamtalkMcp::new needs one to construct — a disconnected client
        // would work equally well here; reuse fake_mcp's async constructor
        // via a tiny blocking runtime for symmetry with the rest of this
        // module. `fake` is kept alive (unused otherwise) for the same
        // reason every other test holds onto it: dropping it early would
        // abort the listener task that owns the socket.
        let rt = tokio::runtime::Runtime::new().unwrap();
        let (fake, mcp) = rt.block_on(fake_mcp(respond(serde_json::json!({}))));
        let info = mcp.get_info();
        assert!(info.capabilities.tools.is_some());
        assert!(info.instructions.unwrap().contains("evaluate"));
        drop(fake);
    }
}
