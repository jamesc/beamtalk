// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Flush tool family: `flush`, `list_changes`, `dirty_methods`, and
//! `recheck_image` — writing pending `ChangeLog` entries to disk (ADR 0082
//! Phase 3; the destructive tier is ADR 0113 Phase 2/4) and inspecting or
//! re-validating the workspace's pending-change state.

use beamtalk_core::tool_expr::{FlushFilter, flush_expr_with_confirm_destructive};
use rmcp::{
    handler::server::wrapper::Parameters,
    model::{CallToolResult, ContentBlock},
    tool, tool_router,
};

use crate::server::params::FlushParams;
use crate::server::{BeamtalkMcp, ToolTimer, check_response, validate_class_name};

#[tool_router(router = flush_tool_router, vis = "pub(crate)")]
impl BeamtalkMcp {
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
    pub(crate) async fn flush(
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
    pub(crate) async fn list_changes(&self) -> Result<CallToolResult, rmcp::ErrorData> {
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
    pub(crate) async fn dirty_methods(&self) -> Result<CallToolResult, rmcp::ErrorData> {
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

    /// Whole-image re-check (ADR 0105 Phase 3, BT-2782).
    ///
    /// Compiles to `Workspace recheckImage` — the "complete but unbounded"
    /// path kept out of the automatic per-reload check: re-checks every live
    /// class the workspace has a recorded source for, not just the
    /// xref-filtered dependents of one changed selector.
    #[tool(
        description = "Re-check every live class in the workspace against the current image, not just the dependents of the last reload. Compiles to 'Workspace recheckImage' — the explicit, on-demand, unbounded sweep (ADR 0105 Phase 3), as opposed to the automatic post-reload check which only re-checks xref-filtered dependents of one changed selector, capped per reload. Returns a report Dictionary (checked/stale/findings). (ADR 0105 Phase 3, BT-2782.)"
    )]
    pub(crate) async fn recheck_image(&self) -> Result<CallToolResult, rmcp::ErrorData> {
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
