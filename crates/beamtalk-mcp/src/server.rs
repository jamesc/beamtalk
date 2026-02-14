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
    client: Arc<ReplClient>,
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

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct EvaluateParams {
    /// Beamtalk expression to evaluate.
    #[schemars(description = "A beamtalk expression to evaluate in the REPL")]
    pub code: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct CompleteParams {
    /// Partial input to complete.
    #[schemars(description = "Partial beamtalk input to get completions for")]
    pub code: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct LoadFileParams {
    /// Path to a .bt source file to load.
    #[schemars(description = "Path to a .bt source file to load into the workspace")]
    pub path: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct InspectParams {
    /// Actor PID to inspect (e.g. "<0.123.0>").
    #[schemars(description = "Erlang PID of the actor to inspect, e.g. \"<0.123.0>\"")]
    pub actor: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct ReloadModuleParams {
    /// Module name to reload.
    #[schemars(description = "Name of the beamtalk module to reload (hot code reload)")]
    pub module: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct DocsParams {
    /// Class name to get documentation for.
    #[schemars(description = "Name of the beamtalk class to get documentation for")]
    pub class: String,
    /// Optional selector to get docs for a specific method.
    #[schemars(description = "Optional method selector to get documentation for")]
    pub selector: Option<String>,
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
}

#[tool_handler]
impl ServerHandler for BeamtalkMcp {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(
                "Beamtalk MCP server — interact with live beamtalk objects through the REPL. \
                 Use 'evaluate' to run beamtalk expressions, 'load_file' to load source code, \
                 'list_actors' to see running actors, 'inspect' to examine actor state, \
                 and 'reload_module' for hot code reloading."
                    .to_string(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}
