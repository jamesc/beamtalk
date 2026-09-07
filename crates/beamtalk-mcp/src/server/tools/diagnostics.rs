// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint/diagnostics tool family: `lint` and `diagnostic_summary`. Both are
//! thin MCP wrappers around the offline analysis pipeline in
//! `server::lint` — no REPL connection needed, since everything runs
//! directly against source files via `spawn_blocking`.

use rmcp::{
    handler::server::wrapper::Parameters,
    model::{CallToolResult, ContentBlock},
    tool, tool_router,
};

use crate::server::lint::{compute_diagnostic_summary, run_lint_structured};
use crate::server::params::{DiagnosticSummaryParams, LintParams};
use crate::server::{BeamtalkMcp, ToolTimer};

#[tool_router(router = diagnostics_tool_router, vis = "pub(crate)")]
impl BeamtalkMcp {
    /// Run lint checks on a `.bt` source file or directory.
    #[tool(
        description = "Run style and redundancy lint checks on a .bt source file or directory. Returns structured diagnostics with file, line, message, and severity. Use path=. for the current directory."
    )]
    pub(crate) async fn lint(
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
    pub(crate) async fn diagnostic_summary(
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
}
