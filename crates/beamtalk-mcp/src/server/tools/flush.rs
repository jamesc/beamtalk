// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Flush tool family: `flush`, `list_changes`, `dirty_methods`, and
//! `recheck_image` — writing pending `ChangeLog` entries to disk (ADR 0082
//! Phase 3; the destructive tier is ADR 0113 Phase 2/4) and inspecting or
//! re-validating the workspace's pending-change state.

use beamtalk_repl_protocol::FlushScope;
use rmcp::{handler::server::wrapper::Parameters, model::CallToolResult, tool, tool_router};

use crate::server::params::FlushParams;
use crate::server::{BeamtalkMcp, InternalErr, ToolTimer, check_response, validate_class_name};

#[tool_router(router = flush_tool_router, vis = "pub(crate)")]
impl BeamtalkMcp {
    /// Flush pending `ChangeLog` entries to disk (ADR 0082 Phase 3;
    /// destructive tier added ADR 0113 Phase 2/4).
    ///
    /// Compiles to `Workspace flush` / `Workspace flush: <selector>` (Tier 1
    /// only), or — when `confirm_destructive: true` — `Workspace
    /// flushIncludingDestructive` / `Workspace flush: <selector>
    /// confirmDestructive: true` (Tier 1 + Tier 2). The optional `class`,
    /// `file`, and `kind` filters are mutually exclusive; at most one may be
    /// supplied. A `selector` scopes to one method, alone or with a `class`.
    #[tool(
        description = "Write pending durable ChangeLog entries to disk via byte-span splice + atomic rename, with external-edit conflict detection. With no arguments, flushes every pending durable Tier-1 change ('Workspace flush'). At most one of 'class', 'file', or 'kind' may be supplied, except that 'selector' (a method selector such as \"increment\" or \"at:put:\") scopes the flush to that method and may be combined with 'class' to flush one method of one class (not with 'file' or 'kind'): 'class' scopes to one class ('Workspace flush: ClassName'), 'file' scopes to one source file ('Workspace flush: #{ #file => \"path\" }'), and 'kind' scopes to one ChangeEntry kind: \"new-class\", \"remove-method\", \"remove-class\", \"rename-class\" or \"rename-method\" ('Workspace flush: #'new-class'). An empty string for any of these is refused, not read as unscoped. DESTRUCTIVE when 'confirm_destructive' is true: pending 'remove-class' entries (from 'remove_class') delete their .bt file from disk — see 'confirm_destructive's own description for the required-argument gate. Returns a FlushResult summary listing files written and any conflicts; a skipped destructive entry is reported distinctly as 'skipped: destructive', not applied. (ADR 0082 Phase 3 / ADR 0113 Phase 2, BT-2288/BT-3207.)",
        annotations(read_only_hint = false, destructive_hint = true)
    )]
    pub(crate) async fn flush(
        &self,
        Parameters(params): Parameters<FlushParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start_quiet("flush");
        // An empty filter is refused, not read as "no filter": flushing every
        // pending change because a scope came through blank would widen it.
        for (name, value) in [
            ("class", params.class.as_deref()),
            ("file", params.file.as_deref()),
            ("kind", params.kind.as_deref()),
            ("selector", params.selector.as_deref()),
        ] {
            if value.is_some_and(str::is_empty) {
                return Err(rmcp::ErrorData::invalid_params(
                    format!("flush: '{name}' must not be empty — omit it to flush everything."),
                    None,
                ));
            }
        }
        // Mutual exclusivity: at most one of class/file/kind, and a selector
        // combines only with a class.
        let provided = [
            params.class.as_deref(),
            params.file.as_deref(),
            params.kind.as_deref(),
        ]
        .into_iter()
        .flatten()
        .count();
        if params.selector.is_some() {
            if params.file.is_some() || params.kind.is_some() {
                return Err(rmcp::ErrorData::invalid_params(
                    "flush: 'selector' combines only with 'class' — not with 'file' or 'kind'.",
                    None,
                ));
            }
        } else if provided > 1 {
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
            selector = ?params.selector,
            confirm_destructive,
            "tool invoked"
        );

        let selector = params
            .selector
            .as_deref()
            .map(|s| s.strip_prefix('#').unwrap_or(s).trim_matches('\''));
        if selector.is_some_and(str::is_empty) {
            return Err(rmcp::ErrorData::invalid_params(
                "flush: 'selector' must not be empty — omit it to flush everything.",
                None,
            ));
        }

        let scope = match (
            params.class.as_deref(),
            params.file.as_deref(),
            params.kind.as_deref(),
        ) {
            (Some(class), None, None) if selector.is_some() => {
                validate_class_name(class)?;
                FlushScope::ClassSelector(class, selector.unwrap_or_default())
            }
            (None, None, None) if selector.is_some() => {
                FlushScope::Selector(selector.unwrap_or_default())
            }
            (Some(class), None, None) => {
                validate_class_name(class)?;
                FlushScope::Class(class)
            }
            (None, Some(file), None) => FlushScope::File(file),
            (None, None, Some(kind)) => {
                // Allow either bare `new-class` or `#'new-class'`; the operation
                // takes the bare kind name.
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
                FlushScope::Kind(bare)
            }
            _ => FlushScope::All,
        };

        // The `flush` operation: the one implementation LiveView and wire
        // clients share, including the destructive-tier confirmation rule.
        let response = self
            .client
            .flush(scope, confirm_destructive)
            .await
            .internal_err()?;

        check_response!(response, "Flush failed");

        // The `FlushResult` summary (`flushed`, `files`, `skipped`,
        // `conflicts`, ...) as JSON, so a withheld destructive entry shows as
        // `"reason":"destructive"` and a conflict is visible.
        let text = response
            .value
            .as_ref()
            .and_then(|v| serde_json::to_string(v).ok())
            .unwrap_or_else(|| "Flushed".to_string());

        timer.ok_text(format!("{text}\n{}", self.workspace_note()))
    }

    /// List pending `ChangeLog` entries (ADR 0082 Phase 3).
    ///
    /// Compiles to `Workspace changes` — returns the `ChangeLog` object's
    /// display form. Pair with `dirty_methods` for the per-class breakdown.
    #[tool(
        description = "Return the workspace ChangeLog — the navigable view of pending in-memory changes against the on-disk source files. Compiles to 'Workspace changes'. Pair with 'dirty_methods' for the per-class breakdown of dirty selectors, or 'flush' to write durable entries to disk. (ADR 0082 Phase 3, BT-2288.)",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn list_changes(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start("list_changes");

        let response = self
            .client
            .evaluate_with_options("Workspace changes", false)
            .await
            .internal_err()?;

        check_response!(response, "Failed to list changes");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "No changes".to_string()
            } else {
                v
            }
        };

        timer.ok_text(text)
    }

    /// Per-class dirty selectors (ADR 0082 Phase 3).
    ///
    /// Compiles to `Workspace changes dirtyMethods` — the per-class set of
    /// dirty selectors, the structured "what specifically has changed?" view.
    #[tool(
        description = "Return the per-class set of dirty selectors in the workspace — the structured 'what specifically has changed?' view. Compiles to 'Workspace changes dirtyMethods'. Pair with 'list_changes' for the full summary or 'flush' to write durable entries to disk. (ADR 0082 Phase 3, BT-2288.)",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn dirty_methods(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start("dirty_methods");

        let response = self
            .client
            .evaluate_with_options("Workspace changes dirtyMethods", false)
            .await
            .internal_err()?;

        check_response!(response, "Failed to list dirty methods");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "No dirty methods".to_string()
            } else {
                v
            }
        };

        timer.ok_text(text)
    }

    /// Whole-image re-check (ADR 0105 Phase 3).
    ///
    /// Compiles to `Workspace recheckImage` — the "complete but unbounded"
    /// path kept out of the automatic per-reload check: re-checks every live
    /// class the workspace has a recorded source for, not just the
    /// xref-filtered dependents of one changed selector.
    #[tool(
        description = "Re-check every live class in the workspace against the current image, not just the dependents of the last reload. Compiles to 'Workspace recheckImage' — the explicit, on-demand, unbounded sweep (ADR 0105 Phase 3), as opposed to the automatic post-reload check which only re-checks xref-filtered dependents of one changed selector, capped per reload. Returns a report Dictionary (checked/stale/findings). (ADR 0105 Phase 3, BT-2782.)",
        annotations(read_only_hint = true)
    )]
    pub(crate) async fn recheck_image(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut timer = ToolTimer::start("recheck_image");

        let response = self
            .client
            .evaluate_with_options("Workspace recheckImage", false)
            .await
            .internal_err()?;

        check_response!(response, "Failed to recheck image");

        let text = {
            let v = response.value_string();
            if v.is_empty() {
                "Whole-image re-check: no findings".to_string()
            } else {
                v
            }
        };

        timer.ok_text(text)
    }
}
