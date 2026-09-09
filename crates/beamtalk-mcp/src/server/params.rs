// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Parameter types deserialized from each MCP tool call's JSON arguments.
//!
//! One `pub struct` per tool, named `<Tool>Params`. Field doc comments and
//! `#[schemars(description = ...)]` attributes double as the JSON schema
//! description surfaced to MCP clients — kept in sync by construction since
//! there is only one source. Grouped here (rather than beside each tool's
//! implementation in `tools/`) so the wire-facing shape of every tool is
//! visible in one place.

use rmcp::schemars;

/// Parameters for the `evaluate` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct EvaluateParams {
    /// Beamtalk expression to evaluate.
    #[schemars(description = "A beamtalk expression to evaluate in the REPL")]
    pub code: String,
    /// If true, return per-statement step values instead of a single result.
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

/// Parameters for the `list_classes` MCP tool.
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

/// Parameters for the `diagnostic_summary` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct DiagnosticSummaryParams {
    /// Path to a `.bt` source file or directory. Defaults to the current directory.
    #[schemars(
        description = "Path to a .bt source file or directory. Defaults to the current directory."
    )]
    pub path: Option<String>,
}

/// Parameters for the `precheck_method` MCP tool (ADR 0105 Phase 3).
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

/// Parameters for the `save_method` MCP tool (ADR 0082 Phase 3).
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

/// Parameters for the `try_method` MCP tool (ADR 0082 Phase 3).
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

/// Parameters for the `save_class` MCP tool (ADR 0082 Phase 3).
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

/// Parameters for the `remove_method` MCP tool (ADR 0112 Phase 4).
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

/// Parameters for the `remove_class` MCP tool (ADR 0113 Phase 4).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct RemoveClassParams {
    /// Name of the Beamtalk class to remove from the running system.
    #[schemars(
        description = "Name of the Beamtalk class to remove from the running system (e.g. \"Counter\")."
    )]
    pub class: String,
}

/// Parameters for the `rename_class` MCP tool (ADR 0114 Phase 5).
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct RenameClassParams {
    /// Name of the Beamtalk class to rename.
    #[schemars(description = "Name of the Beamtalk class to rename (e.g. \"Counter\").")]
    pub class: String,
    /// The new name for the class.
    #[schemars(description = "The new name for the class (e.g. \"Accumulator\").")]
    pub new_name: String,
}

/// Parameters for the `rename_method` MCP tool (ADR 0114 Phase 5).
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

/// Parameters for the `flush` MCP tool (ADR 0082 Phase 3).
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
    /// Required-when-applicable Tier-2 confirmation (ADR 0113 Phase 2/4).
    /// Must be `true` to also apply pending `remove-class`
    /// (destructive, file-deleting) entries; omitted or `false` flushes only
    /// Tier 1 (patches, new-class, remove-method) exactly as before.
    #[schemars(
        description = "Set to true to ALSO apply pending destructive 'remove-class' entries (which delete a .bt file) within this flush's scope — omitted or false flushes only non-destructive Tier-1 entries (patches, new-class, remove-method), and any pending remove-class entry is reported in the result as 'skipped: destructive'. There is no default: an agent cannot delete a file by omission. With no 'class'/'file'/'kind' filter, true compiles to the unscoped 'Workspace flushIncludingDestructive'; with a filter, it compiles to 'Workspace flush: <filter> confirmDestructive: true'. (ADR 0113 Phase 2/4, BT-3207/BT-3210.)"
    )]
    pub confirm_destructive: Option<bool>,
}
