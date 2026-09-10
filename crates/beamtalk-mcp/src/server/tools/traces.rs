// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Traces/stats tool family: `list_actors`, `supervision_tree`,
//! `enable_tracing`, `disable_tracing`, `get_traces`, `export_traces`, and
//! `actor_stats` (ADR 0069/0092) — process-tree and per-actor call tracing
//! introspection.

use beamtalk_repl_protocol::format::{self as fmt, Diagnostic as FmtDiagnostic};
use rmcp::{
    handler::server::wrapper::Parameters,
    model::{CallToolResult, ContentBlock},
    tool, tool_router,
};

use crate::server::params::{
    ActorStatsParams, ExportTracesParams, GetTracesParams, SupervisionTreeParams,
};
use crate::server::{
    BeamtalkMcp, MCP_OUTPUT_MODE, ToolTimer, check_response, error_result, pretty_json,
};

#[tool_router(router = traces_tool_router, vis = "pub(crate)")]
impl BeamtalkMcp {
    /// List all running actors in the workspace.
    #[tool(
        description = "List all running actors in the workspace. Returns each actor's PID and class."
    )]
    pub(crate) async fn list_actors(&self) -> Result<CallToolResult, rmcp::ErrorData> {
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
    pub(crate) async fn supervision_tree(
        &self,
        Parameters(params): Parameters<SupervisionTreeParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::new("supervision_tree");
        let scope = params.scope.as_deref().unwrap_or("default");
        tracing::debug!(tool = "supervision_tree", scope, "tool invoked");
        // Surface the snapshot through the same term-returning eval seam every
        // surface shares (so the structured node data is identical across
        // surfaces). `system` is the privileged whole-node view; `default` is
        // the runtime-plumbing-filtered Read view (ADR 0091).
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

    /// Enable actor trace event capture (ADR 0069).
    #[tool(
        description = "Enable actor trace event capture. Aggregate stats (call counts, durations) are always on; this enables detailed per-event traces. Call get-traces and actor-stats after running actor code to inspect results. Disable with evaluate(\"Tracing disable\")."
    )]
    pub(crate) async fn enable_tracing(&self) -> Result<CallToolResult, rmcp::ErrorData> {
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
    pub(crate) async fn disable_tracing(&self) -> Result<CallToolResult, rmcp::ErrorData> {
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
    pub(crate) async fn get_traces(
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
    pub(crate) async fn export_traces(
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
    pub(crate) async fn actor_stats(
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
}
