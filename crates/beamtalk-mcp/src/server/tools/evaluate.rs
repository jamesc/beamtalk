// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Evaluate/complete/load tool family: `evaluate`, `complete`, `load_project`,
//! `load_file`, `unload`, `interrupt`, `inspect`, `show_codegen`, and `test`
//! — the tools that run code or move source files in and out of the live
//! REPL session.

use std::fmt::Write;

use beamtalk_core::unparse::escape_string_literal;
use beamtalk_repl_protocol::format::{self as fmt, Diagnostic as FmtDiagnostic};
use rmcp::{
    handler::server::wrapper::Parameters,
    model::{CallToolResult, ContentBlock},
    tool, tool_router,
};

use crate::server::params::{
    CompleteParams, EvaluateParams, InspectParams, LoadFileParams, LoadProjectParams,
    ShowCodegenParams, TestParams, UnloadParams,
};
use crate::server::{BeamtalkMcp, MCP_OUTPUT_MODE, ToolTimer, check_response, error_result};
use crate::server::{pretty_json, validate_class_name};

#[tool_router(router = evaluate_tool_router, vis = "pub(crate)")]
impl BeamtalkMcp {
    /// Evaluate a beamtalk expression in the live REPL.
    #[tool(
        description = "Evaluate a beamtalk expression in the live REPL. Returns the result value and any stdout output. Use this to interact with beamtalk objects, call methods, spawn actors, and explore the live system. Set trace=true to get per-statement step values instead of a single result."
    )]
    pub(crate) async fn evaluate(
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
    pub(crate) async fn complete(
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
    pub(crate) async fn load_project(
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
    pub(crate) async fn load_file(
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

    /// Unload a class from the workspace.
    #[tool(
        description = "Unload a class from the workspace. Removes the class. Does not affect running actors."
    )]
    pub(crate) async fn unload(
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
    pub(crate) async fn interrupt(&self) -> Result<CallToolResult, rmcp::ErrorData> {
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

    /// Inspect a running actor's state by PID.
    #[tool(
        description = "Inspect a running actor's state. Provide the actor's PID (e.g. \"<0.123.0>\") to see its current state as structured data."
    )]
    pub(crate) async fn inspect(
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

    /// Inspect the generated Core Erlang code for a beamtalk expression or loaded class.
    #[tool(
        description = "Show the generated Core Erlang code for a beamtalk expression or loaded class. Use 'code' to compile an expression snippet, or 'class' (+ optional 'selector') to inspect a class already loaded in the session. Useful for debugging codegen and understanding compilation."
    )]
    pub(crate) async fn show_codegen(
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
    pub(crate) async fn test(
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
}
