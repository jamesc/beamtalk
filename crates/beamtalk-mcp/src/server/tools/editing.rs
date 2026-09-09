// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-editing tool family: `reload_class`, `save_method`, `try_method`,
//! `save_class`, `remove_method`, `remove_class`, `rename_class`,
//! `rename_method`, and `precheck_method`.
//!
//! Per ADR 0082 there are no dedicated workspace-side REPL ops for these —
//! each tool is a typed front-end that compiles to a plain Beamtalk
//! expression (`aClass compile: #selector source: body`, `aClass
//! removeSelector: #selector`, …) and runs it through the same `evaluate`
//! pathway every other tool uses. Most of these tools mutate memory only
//! and log a `ChangeLog` entry; writing to disk is a separate, explicit
//! `server::tools::flush` call.

use beamtalk_core::tool_expr::{
    precheck_method_expr, remove_class_expr, remove_method_expr, remove_method_if_absent_expr,
    rename_class_expr, rename_method_expr, save_class_expr,
};
use beamtalk_core::unparse::escape_string_literal;
use rmcp::{
    handler::server::wrapper::Parameters,
    model::{CallToolResult, ContentBlock},
    tool, tool_router,
};

use crate::server::params::{
    PrecheckMethodParams, ReloadClassParams, RemoveClassParams, RemoveMethodParams,
    RenameClassParams, RenameMethodParams, SaveClassParams, SaveMethodParams, TryMethodParams,
};
use crate::server::{
    BeamtalkMcp, ToolTimer, check_response, validate_class_name, validate_selector,
};

/// Build the Beamtalk expression for the `save_method` MCP tool — durable
/// patch path (ADR 0082 Phase 3). Selector is the bare form (no leading `#`).
pub(crate) fn save_method_expr(class: &str, selector: &str, body: &str) -> String {
    format!(
        "{} compile: #{} source: \"{}\"",
        class,
        selector,
        escape_string_literal(body),
    )
}

/// Build the Beamtalk expression for the `try_method` MCP tool — ephemeral
/// patch path (ADR 0082 Phase 3). Selector is the bare form (no leading `#`).
pub(crate) fn try_method_expr(class: &str, selector: &str, body: &str) -> String {
    format!(
        "{} tryCompile: #{} source: \"{}\"",
        class,
        selector,
        escape_string_literal(body),
    )
}

#[tool_router(router = editing_tool_router, vis = "pub(crate)")]
impl BeamtalkMcp {
    /// Hot-reload a class, migrating running actors to the new code.
    #[tool(
        description = "Hot-reload a class. Recompiles and reloads the class, migrating any running actors to the new code."
    )]
    pub(crate) async fn reload_class(
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

    /// Durably install a method on a Beamtalk class (ADR 0082 Phase 3).
    ///
    /// Compiles to `aClass compile: #selector source: body`. The patch installs
    /// in memory and appends a durable `ChangeLog` entry that can be written to
    /// disk by a subsequent `flush`.
    #[tool(
        description = "Durably install a method on a Beamtalk class. The method patch installs in memory and appends a durable ChangeLog entry that 'flush' will later write to disk (when the class is backed by an in-project .bt file). The 'body' argument is the source on the right-hand side of '=>' and is passed as a String value — no escaping or quoting required from the caller. Use 'try_method' for ephemeral spikes you may discard. (ADR 0082 Phase 3, BT-2288.)"
    )]
    pub(crate) async fn save_method(
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
    pub(crate) async fn try_method(
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
    pub(crate) async fn save_class(
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
    pub(crate) async fn remove_method(
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
    pub(crate) async fn remove_class(
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
    pub(crate) async fn rename_class(
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
    pub(crate) async fn rename_method(
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

    /// Pre-save advisory precheck (ADR 0105 Phase 3, BT-2782).
    ///
    /// Compiles to `aClass precheckCompile: #selector source: body`. Nothing
    /// installs and nothing is recorded to the `ChangeLog` — this is a
    /// read-only "check before save" report of would-be-stale callers.
    #[tool(
        description = "Compile a pending method edit and report would-be-stale dependents, without installing it. Compiles to 'aClass precheckCompile: #selector source: body' — the editor/LSP pre-save advisory (ADR 0105 Phase 3): non-blocking, the post-reload image check that runs automatically on 'save_method' remains the authority. The 'body' argument is the source on the right-hand side of '=>', passed as a String value — no escaping required by the caller. Returns a report Dictionary (findings/checked/totalCandidates/notChecked/capNote/checkedOwners); an edit with no type-relevant signature change reports empty. (ADR 0105 Phase 3, BT-2782.)"
    )]
    pub(crate) async fn precheck_method(
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
}
