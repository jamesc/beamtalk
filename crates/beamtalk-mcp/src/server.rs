// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! MCP server exposing beamtalk REPL operations as tools.
//!
//! **DDD Context:** Language Service / Interactive Development
//!
//! Uses the `rmcp` crate to implement an MCP server that wraps the
//! beamtalk REPL's JSON-over-TCP protocol, allowing any MCP-compatible
//! agent to interact with live beamtalk objects.

use std::sync::Arc;

use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use rmcp::{
    ServerHandler,
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{CallToolResult, Content, ServerCapabilities, ServerInfo},
    schemars, tool, tool_handler, tool_router,
};
use sha2::{Digest, Sha256};

use crate::client::ReplClient;

/// MCP server backed by a beamtalk REPL connection.
#[derive(Clone)]
pub struct BeamtalkMcp {
    /// Shared REPL client used by all tool handlers.
    client: Arc<ReplClient>,
    /// Router that dispatches incoming MCP tool calls to handler methods.
    tool_router: ToolRouter<Self>,
}

/// Create an error `CallToolResult` with `is_error` set to true.
fn error_result(msg: impl Into<String>) -> CallToolResult {
    CallToolResult::error(vec![Content::text(msg.into())])
}

/// Pretty-print a JSON value, falling back to `Display` on serialization error.
fn pretty_json(value: &serde_json::Value) -> String {
    serde_json::to_string_pretty(value).unwrap_or_else(|_| value.to_string())
}

/// Check a REPL response for errors and return early with a formatted error result.
///
/// The `$fallback` string is used when the response has no error message.
macro_rules! check_response {
    ($response:expr, $fallback:expr) => {
        if $response.is_error() {
            let msg = $response.error_message().unwrap_or($fallback);
            return Ok(error_result(format!("ERROR: {msg}")));
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

/// Parameters for the `reload_module` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ReloadModuleParams {
    /// Module name to reload.
    #[schemars(description = "Name of the beamtalk module to reload (hot code reload)")]
    pub module: String,
}

/// Parameters for the `docs` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct DocsParams {
    /// Class name to get documentation for.
    #[schemars(description = "Name of the beamtalk class to get documentation for")]
    pub class: String,
    /// Optional selector to get docs for a specific method.
    #[schemars(description = "Optional method selector to get documentation for")]
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
    /// Optional method selector when using `class`. If omitted, shows the full module.
    #[schemars(
        description = "Optional method selector when inspecting a class. Narrows context but full module Core Erlang is returned."
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
    /// Name of the module to unload from the workspace.
    #[schemars(description = "Name of the beamtalk module to unload from the workspace")]
    pub module: String,
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
        let use_trace = params.trace.unwrap_or(false);
        let response = self
            .client
            .evaluate_with_options(&params.code, use_trace)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        if response.is_error() {
            use std::fmt::Write as _;
            let msg = response.error_message().unwrap_or("Unknown error");
            let mut error_text = format!("ERROR: {msg}");
            if let Some(line) = response.line {
                let _ = write!(error_text, "\nLine: {line}");
            }
            if let Some(ref hint) = response.hint {
                let _ = write!(error_text, "\nHint: {hint}");
            }
            return Ok(error_result(error_text));
        }

        let mut parts = Vec::new();

        if let Some(ref output) = response.output {
            if !output.is_empty() {
                parts.push(Content::text(format!("Output: {output}")));
            }
        }

        if use_trace {
            let steps = response.steps.unwrap_or_default();
            if steps.is_empty() {
                parts.push(Content::text("(no steps)"));
            } else {
                for step in &steps {
                    let src = step.get("src").and_then(|v| v.as_str()).unwrap_or("?");
                    let val = step
                        .get("value")
                        .cloned()
                        .unwrap_or(serde_json::Value::Null);
                    let val_str = match &val {
                        serde_json::Value::String(s) => s.clone(),
                        v => v.to_string(),
                    };
                    parts.push(Content::text(format!("{src} => {val_str}")));
                }
            }
        } else {
            let value = response.value_string();
            if !value.is_empty() {
                parts.push(Content::text(value));
            }
        }

        if parts.is_empty() {
            parts.push(Content::text("nil"));
        }

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
        let code_len = params.code.len();
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

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// Load all `.bt` source files from a project in dependency order.
    #[tool(
        description = "Load all .bt source files from a beamtalk project (identified by beamtalk.toml) in dependency order. Reads the src/ directory and loads files so superclasses are loaded before subclasses. Returns the list of loaded classes and any per-file errors."
    )]
    async fn load_project(
        &self,
        Parameters(params): Parameters<LoadProjectParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let include_tests = params.include_tests.unwrap_or(false);
        let response = self
            .client
            .load_project(&params.path, include_tests)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to load project");

        let classes = response.classes.unwrap_or_default();
        let errors = response.errors.unwrap_or_default();

        let mut parts = Vec::new();

        if classes.is_empty() {
            parts.push(Content::text("No classes loaded"));
        } else {
            parts.push(Content::text(format!(
                "Loaded classes: {}",
                classes.join(", ")
            )));
        }

        for e in &errors {
            // Each error is a structured map with path, kind, message (and optional hint).
            let msg = match e {
                serde_json::Value::Object(map) => {
                    let path = map.get("path").and_then(|v| v.as_str()).unwrap_or("");
                    let message = map
                        .get("message")
                        .and_then(|v| v.as_str())
                        .unwrap_or("unknown error");
                    if path.is_empty() {
                        message.to_string()
                    } else {
                        format!("{path}: {message}")
                    }
                }
                serde_json::Value::String(s) => s.clone(),
                _ => e.to_string(),
            };
            parts.push(Content::text(format!("Error: {msg}")));
        }

        // Partial loads (some files succeeded, some failed) are still reported as
        // errors so MCP clients that inspect is_error see the failure.
        if !errors.is_empty() {
            return Ok(CallToolResult::error(parts));
        }

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
        let response = self
            .client
            .load_file(&params.path)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to load file");

        let classes = response.classes.unwrap_or_default();
        let text = if classes.is_empty() {
            "File loaded (no classes defined)".to_string()
        } else {
            format!("Loaded classes: {}", classes.join(", "))
        };

        let mut parts = vec![Content::text(text)];

        // Include any warnings
        if let Some(warnings) = response.warnings {
            for w in warnings {
                parts.push(Content::text(format!("Warning: {w}")));
            }
        }

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

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// List all running actors in the workspace.
    #[tool(
        description = "List all running actors in the workspace. Returns each actor's PID, class, and module."
    )]
    async fn list_actors(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .actors()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to list actors");

        let actors = response.actors.unwrap_or_default();
        let text = if actors.is_empty() {
            "No actors running".to_string()
        } else {
            actors
                .iter()
                .map(|a| format!("{} ({}) — pid: {}", a.class, a.module, a.pid))
                .collect::<Vec<_>>()
                .join("\n")
        };

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// List all loaded modules in the workspace.
    #[tool(
        description = "List all loaded modules in the workspace. Returns each module's name, source file, actor count, and when it was loaded."
    )]
    async fn list_modules(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .modules()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to list modules");

        let modules = response.modules.unwrap_or_default();
        let text = if modules.is_empty() {
            "No modules loaded".to_string()
        } else {
            modules
                .iter()
                .map(|m| {
                    format!(
                        "{} — {} ({} actors, loaded {})",
                        m.name, m.source_file, m.actor_count, m.time_ago
                    )
                })
                .collect::<Vec<_>>()
                .join("\n")
        };

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// List all available Beamtalk classes with one-line descriptions (BT-1404).
    #[tool(
        description = "List all available Beamtalk classes with one-line descriptions. Optionally filter by superclass (e.g. 'Value', 'Actor') or scope ('stdlib' for built-in classes, 'user' for user-defined)."
    )]
    async fn list_classes(
        &self,
        Parameters(params): Parameters<ListClassesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .list_classes(params.filter.as_deref())
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to list classes");

        let classes = response.class_list.unwrap_or_default();
        let text = if classes.is_empty() {
            "No classes found".to_string()
        } else {
            classes
                .iter()
                .map(|c| {
                    let super_str = c.superclass.as_deref().unwrap_or("(root)");
                    let doc_str = c.doc.as_deref().unwrap_or("");
                    let modifiers = {
                        let mut m = Vec::new();
                        if c.sealed {
                            m.push("sealed");
                        }
                        if c.is_abstract {
                            m.push("abstract");
                        }
                        if m.is_empty() {
                            String::new()
                        } else {
                            format!(" [{}]", m.join(", "))
                        }
                    };
                    if doc_str.is_empty() {
                        format!("{} < {}{}", c.name, super_str, modifiers)
                    } else {
                        format!("{} < {}{} — {}", c.name, super_str, modifiers, doc_str)
                    }
                })
                .collect::<Vec<_>>()
                .join("\n")
        };

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// Get current variable bindings in the REPL session.
    #[tool(
        description = "Get current variable bindings in the REPL session. Shows all variables and their values."
    )]
    async fn get_bindings(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .bindings()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to get bindings");

        let text = match response.bindings {
            Some(bindings) => pretty_json(&bindings),
            None => "No bindings".to_string(),
        };

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// Hot-reload a module, migrating running actors to the new code.
    #[tool(
        description = "Hot-reload a module. Recompiles and reloads the module, migrating any running actors to the new code. Returns the number of affected actors and any migration failures."
    )]
    async fn reload_module(
        &self,
        Parameters(params): Parameters<ReloadModuleParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .reload(&params.module)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to reload module");

        let mut parts = vec![Content::text("Module reloaded successfully")];

        if let Some(affected) = response.affected_actors {
            parts.push(Content::text(format!("Affected actors: {affected}")));
        }
        if let Some(failures) = response.migration_failures {
            if failures > 0 {
                parts.push(Content::text(format!("Migration failures: {failures}")));
            }
        }

        Ok(CallToolResult::success(parts))
    }

    /// Get documentation for a beamtalk class or method.
    #[tool(
        description = "Get documentation for a beamtalk class or method. Provide a class name and optionally a method selector."
    )]
    async fn docs(
        &self,
        Parameters(params): Parameters<DocsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .docs(&params.class, params.selector.as_deref())
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "No documentation found");

        let text = response
            .docs
            .unwrap_or_else(|| "No documentation available".to_string());

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// Clear all variable bindings in the REPL session.
    #[tool(
        description = "Clear all variable bindings in the REPL session. Resets the workspace to a clean state."
    )]
    async fn clear(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .clear()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to clear bindings");

        Ok(CallToolResult::success(vec![Content::text(
            "Bindings cleared",
        )]))
    }

    /// Unload a module from the workspace.
    #[tool(
        description = "Unload a module from the workspace. Removes the module and its classes. Does not affect running actors."
    )]
    async fn unload(
        &self,
        Parameters(params): Parameters<UnloadParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .unload(&params.module)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to unload module");

        Ok(CallToolResult::success(vec![Content::text(format!(
            "Module '{}' unloaded",
            params.module
        ))]))
    }

    /// Interrupt a running evaluation.
    #[tool(
        description = "Interrupt a running evaluation in the REPL. Use this to cancel long-running or stuck evaluations."
    )]
    async fn interrupt(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .interrupt()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Failed to send interrupt");

        Ok(CallToolResult::success(vec![Content::text(
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
            parts.push(Content::text(core_erlang));
        } else {
            parts.push(Content::text("No Core Erlang output"));
        }

        if let Some(warnings) = response.warnings {
            for w in warnings {
                parts.push(Content::text(format!("Warning: {w}")));
            }
        }

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
            Some(results) => pretty_json(&results),
            None => "Tests completed (no structured results)".to_string(),
        };

        if has_failures {
            return Ok(error_result(format!("TEST FAILURES:\n{text}")));
        }

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// Run lint checks on a `.bt` source file or directory.
    #[tool(
        description = "Run style and redundancy lint checks on a .bt source file or directory. Returns structured diagnostics with file, line, message, and severity. Use path=. for the current directory."
    )]
    async fn lint(
        &self,
        Parameters(params): Parameters<LintParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let path = params.path.unwrap_or_else(|| ".".to_string());
        // Run blocking I/O and CPU-bound parsing off the Tokio worker thread.
        let result = tokio::task::spawn_blocking(move || run_lint_structured(&path))
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e.to_string(), None))?;
        let has_errors = !result.errors.is_empty();
        let text = serde_json::to_string_pretty(&result).unwrap_or_else(|_| format!("{result:?}"));
        let structured = serde_json::to_value(&result).ok();
        let mut call_result = CallToolResult::default();
        call_result.content = vec![Content::text(text)];
        call_result.structured_content = structured;
        if has_errors {
            call_result.is_error = Some(true);
        }
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
        let start = std::time::Instant::now();
        let results = beamtalk_examples::search(&params.query, params.limit);
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
            return Ok(CallToolResult::success(vec![Content::text(
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

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// Search for Beamtalk classes by keyword or concept.
    #[tool(
        description = "Search for Beamtalk classes by keyword, concept, or method name. Returns matching classes with their superclass, description, and key methods. Use this to discover which class provides a capability before using 'docs' for full details. Works offline — no REPL connection needed."
    )]
    async fn search_classes(
        &self,
        Parameters(params): Parameters<SearchClassesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let start = std::time::Instant::now();
        let results = beamtalk_examples::search_classes(&params.query, params.limit);
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
        tracing::debug!(query = %params.query, "search_classes query");

        if results.is_empty() {
            return Ok(CallToolResult::success(vec![Content::text(
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

        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    /// Discover supported REPL operations and protocol version.
    #[tool(
        description = "Discover supported REPL operations and protocol version. Returns the list of available ops with their parameters, and version information."
    )]
    async fn describe(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .describe()
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        check_response!(response, "Describe failed");

        let mut parts = Vec::new();

        if let Some(ops) = response.ops {
            parts.push(Content::text(format!(
                "Supported operations:\n{}",
                pretty_json(&ops)
            )));
        }
        if let Some(versions) = response.versions {
            parts.push(Content::text(format!(
                "Versions: {}",
                pretty_json(&versions)
            )));
        }

        if parts.is_empty() {
            parts.push(Content::text("No describe information available"));
        }

        Ok(CallToolResult::success(parts))
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

/// Convert a byte offset into a 1-indexed line number.
fn offset_to_line(source: &str, offset: usize) -> u32 {
    let clamped = offset.min(source.len());
    #[allow(clippy::cast_possible_truncation)]
    let line = source[..clamped].bytes().filter(|&b| b == b'\n').count() as u32 + 1;
    line
}

/// Recursively collect all `.bt` files under `dir`.
///
/// Returns an error if the directory cannot be read (e.g. permission denied),
/// preventing silent false-clean results.
fn collect_bt_files(dir: &std::path::Path) -> std::io::Result<Vec<std::path::PathBuf>> {
    let mut out = Vec::new();
    let entries = std::fs::read_dir(dir)?;
    let mut entries: Vec<_> = entries.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(std::fs::DirEntry::file_name);
    for entry in entries {
        let path = entry.path();
        if path.is_symlink() {
            continue;
        }
        if path.is_dir() {
            out.extend(collect_bt_files(&path)?);
        } else if path.extension().is_some_and(|e| e == "bt") {
            out.push(path);
        }
    }
    Ok(out)
}

/// Resolve `path` to a list of `.bt` source files, or return a `LintResult`
/// containing a single error diagnostic explaining why no files could be found.
///
/// Returns `Vec<PathBuf>` (not `Utf8PathBuf`) so that files with non-UTF-8
/// names are preserved rather than silently dropped.
fn resolve_source_files(path: &str) -> Result<Vec<std::path::PathBuf>, LintResult> {
    let source_path = std::path::Path::new(path);
    if source_path.is_file() {
        if source_path.extension().is_some_and(|e| e == "bt") {
            return Ok(vec![source_path.to_path_buf()]);
        }
        return Err(lint_error(
            path,
            format!("'{path}' is not a .bt source file"),
        ));
    }
    if source_path.is_dir() {
        let files = collect_bt_files(source_path)
            .map_err(|e| lint_error(path, format!("Failed to read directory '{path}': {e}")))?;
        if files.is_empty() {
            return Err(lint_error(
                path,
                format!("No .bt source files found in '{path}'"),
            ));
        }
        return Ok(files);
    }
    Err(lint_error(path, format!("Path '{path}' does not exist")))
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

/// Run lint passes on `path` (file or directory) and return structured results.
fn run_lint_structured(path: &str) -> LintResult {
    let source_files = match resolve_source_files(path) {
        Ok(files) => files,
        Err(result) => return result,
    };

    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    for file in &source_files {
        let Ok(source) = std::fs::read_to_string(file) else {
            errors.push(LintDiagnostic {
                file: file.to_string_lossy().into_owned(),
                line: None,
                message: format!("Failed to read '{}'", file.display()),
                severity: "error",
            });
            continue;
        };

        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);

        // Include parse errors (syntax problems) and warnings so files with
        // broken syntax or parser-emitted warnings don't silently appear clean.
        // Hint-severity diagnostics (DNU hints) are excluded as they are
        // informational and belong to the check/compile workflow.
        let mut lint_diags: Vec<_> = parse_diags
            .into_iter()
            .filter(|d| {
                matches!(
                    d.severity,
                    Severity::Error | Severity::Warning | Severity::Lint
                )
            })
            .collect();
        lint_diags.extend(beamtalk_core::lint::run_lint_passes(&module));

        let file_name = file.to_string_lossy().into_owned();
        for diag in &lint_diags {
            let line = offset_to_line(&source, diag.span.start() as usize);
            let severity = match diag.severity {
                Severity::Error => "error",
                Severity::Warning | Severity::Lint | Severity::Hint => "warning",
            };
            let entry = LintDiagnostic {
                file: file_name.clone(),
                line: Some(line),
                message: diag.message.to_string(),
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
                 'reload_module' for hot code reloading, 'test' to run BUnit tests, \
                 'lint' to run style/redundancy checks on .bt source files, \
                 'search_classes' to discover Beamtalk classes by keyword or concept (works offline, no REPL needed), \
                 'search_examples' to find Beamtalk code examples by keyword (works offline, no REPL needed), \
                 'show_codegen' to inspect generated Core Erlang (use class+selector for loaded classes), 'info' for symbol details, \
                 'describe' for capability discovery, 'clear' to reset bindings, \
                 'unload' to remove a module, and 'interrupt' to cancel evaluations.",
            )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use camino::Utf8PathBuf;

    // --- offset_to_line ---

    #[test]
    fn offset_to_line_empty_string() {
        assert_eq!(offset_to_line("", 0), 1);
    }

    #[test]
    fn offset_to_line_single_line() {
        let src = "hello world";
        assert_eq!(offset_to_line(src, 0), 1);
        assert_eq!(offset_to_line(src, 5), 1);
        assert_eq!(offset_to_line(src, src.len()), 1);
    }

    #[test]
    fn offset_to_line_multi_line() {
        let src = "line1\nline2\nline3";
        assert_eq!(offset_to_line(src, 0), 1); // start of line1
        assert_eq!(offset_to_line(src, 5), 1); // end of line1 (before \n)
        assert_eq!(offset_to_line(src, 6), 2); // start of line2
        assert_eq!(offset_to_line(src, 11), 2); // end of line2 (before \n)
        assert_eq!(offset_to_line(src, 12), 3); // start of line3
    }

    #[test]
    fn offset_to_line_clamps_past_end() {
        let src = "abc";
        // Offset beyond source length should clamp and not panic.
        assert_eq!(offset_to_line(src, 999), 1);
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
        let path = Utf8PathBuf::from_path_buf(std::env::temp_dir())
            .expect("temp dir should be UTF-8")
            .join(format!(
                "beamtalk-mcp-lint-non-bt-{}.txt",
                std::process::id()
            ));
        std::fs::write(path.as_std_path(), "not beamtalk").unwrap();
        let result = run_lint_structured(path.as_str());
        let _ = std::fs::remove_file(path.as_std_path());
        assert_eq!(result.total, 1);
        assert!(result.errors.len() == 1);
        assert!(result.errors[0].message.contains(".bt source file"));
    }

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
}
