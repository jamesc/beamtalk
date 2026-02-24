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

use rmcp::{
    ServerHandler,
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{CallToolResult, Content, ServerCapabilities, ServerInfo},
    schemars, tool, tool_handler, tool_router,
};

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
    CallToolResult {
        content: vec![Content::text(msg.into())],
        is_error: Some(true),
        meta: None,
        structured_content: None,
    }
}

// --- Tool parameter types ---

/// Parameters for the `evaluate` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct EvaluateParams {
    /// Beamtalk expression to evaluate.
    #[schemars(description = "A beamtalk expression to evaluate in the REPL")]
    pub code: String,
}

/// Parameters for the `complete` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct CompleteParams {
    /// Partial input to complete.
    #[schemars(description = "Partial beamtalk input to get completions for")]
    pub code: String,
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
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ShowCodegenParams {
    /// Beamtalk code to compile and show the generated Core Erlang for.
    #[schemars(description = "Beamtalk code to compile and show the generated Core Erlang for")]
    pub code: String,
}

/// Parameters for the `test` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct TestParams {
    /// Optional class name to run tests for. If omitted, runs all tests.
    #[schemars(description = "Optional TestCase class name. If omitted, runs all BUnit tests.")]
    pub class: Option<String>,
}

/// Parameters for the `unload` MCP tool.
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct UnloadParams {
    /// Name of the module to unload from the workspace.
    #[schemars(description = "Name of the beamtalk module to unload from the workspace")]
    pub module: String,
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
        description = "Evaluate a beamtalk expression in the live REPL. Returns the result value and any stdout output. Use this to interact with beamtalk objects, call methods, spawn actors, and explore the live system."
    )]
    async fn evaluate(
        &self,
        Parameters(params): Parameters<EvaluateParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .eval(&params.code)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Unknown error");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

        let mut parts = Vec::new();

        if let Some(ref output) = response.output {
            if !output.is_empty() {
                parts.push(Content::text(format!("Output: {output}")));
            }
        }
        let value = response.value_string();
        if !value.is_empty() {
            parts.push(Content::text(value));
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
        let response = self
            .client
            .complete(&params.code)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Completion failed");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

        let completions = response.completions.unwrap_or_default();
        let text = if completions.is_empty() {
            "No completions available".to_string()
        } else {
            completions.join("\n")
        };

        Ok(CallToolResult::success(vec![Content::text(text)]))
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

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Failed to load file");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

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

        if response.is_error() {
            let msg = response
                .error_message()
                .unwrap_or("Failed to inspect actor");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

        let text = match response.state {
            Some(serde_json::Value::String(s)) => s,
            Some(state) => {
                serde_json::to_string_pretty(&state).unwrap_or_else(|_| state.to_string())
            }
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

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Failed to list actors");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

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

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Failed to list modules");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

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

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Failed to get bindings");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

        let text = match response.bindings {
            Some(bindings) => {
                serde_json::to_string_pretty(&bindings).unwrap_or_else(|_| bindings.to_string())
            }
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

        if response.is_error() {
            let msg = response
                .error_message()
                .unwrap_or("Failed to reload module");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

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

        if response.is_error() {
            let msg = response.error_message().unwrap_or("No documentation found");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

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

        if response.is_error() {
            let msg = response
                .error_message()
                .unwrap_or("Failed to clear bindings");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

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

        if response.is_error() {
            let msg = response
                .error_message()
                .unwrap_or("Failed to unload module");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

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

        if response.is_error() {
            let msg = response
                .error_message()
                .unwrap_or("Failed to send interrupt");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

        Ok(CallToolResult::success(vec![Content::text(
            "Interrupt sent",
        )]))
    }

    /// Inspect the generated Core Erlang code for a beamtalk expression.
    #[tool(
        description = "Show the generated Core Erlang code for a beamtalk expression or class definition. Useful for debugging codegen and understanding compilation."
    )]
    async fn show_codegen(
        &self,
        Parameters(params): Parameters<ShowCodegenParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = self
            .client
            .show_codegen(&params.code)
            .await
            .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        if response.is_error() {
            let msg = response
                .error_message()
                .unwrap_or("Failed to generate Core Erlang");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

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
        description = "Run BUnit tests. Provide a class name to run tests for that class, or omit to run all tests. Returns structured results with pass/fail counts."
    )]
    async fn test(
        &self,
        Parameters(params): Parameters<TestParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let response = match params.class {
            Some(ref class) => self.client.test_class(class).await,
            None => self.client.test_all().await,
        }
        .map_err(|e| rmcp::ErrorData::internal_error(e, None))?;

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Test execution failed");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

        let has_failures = response.has_test_error();

        let text = match response.results {
            Some(results) => {
                serde_json::to_string_pretty(&results).unwrap_or_else(|_| results.to_string())
            }
            None => "Tests completed (no structured results)".to_string(),
        };

        if has_failures {
            return Ok(error_result(format!("TEST FAILURES:\n{text}")));
        }

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

        if response.is_error() {
            let msg = response.error_message().unwrap_or("Describe failed");
            return Ok(error_result(format!("ERROR: {msg}")));
        }

        let mut parts = Vec::new();

        if let Some(ops) = response.ops {
            parts.push(Content::text(format!(
                "Supported operations:\n{}",
                serde_json::to_string_pretty(&ops).unwrap_or_else(|_| ops.to_string())
            )));
        }
        if let Some(versions) = response.versions {
            parts.push(Content::text(format!(
                "Versions: {}",
                serde_json::to_string_pretty(&versions).unwrap_or_else(|_| versions.to_string())
            )));
        }

        if parts.is_empty() {
            parts.push(Content::text("No describe information available"));
        }

        Ok(CallToolResult::success(parts))
    }
}

#[tool_handler]
impl ServerHandler for BeamtalkMcp {
    /// Return server metadata and capabilities advertised to MCP clients.
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(
                "Beamtalk MCP server — interact with live beamtalk objects through the REPL. \
                 Use 'evaluate' to run beamtalk expressions, 'load_file' to load source code, \
                 'list_actors' to see running actors, 'inspect' to examine actor state, \
                 'reload_module' for hot code reloading, 'test' to run BUnit tests, \
                 'show_codegen' to inspect generated Core Erlang, 'info' for symbol details, \
                 'describe' for capability discovery, 'clear' to reset bindings, \
                 'unload' to remove a module, and 'interrupt' to cancel evaluations."
                    .to_string(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}
