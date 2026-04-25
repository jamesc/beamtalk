// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! LSP capability parity (BT-2081).
//!
//! The LSP is not a 1:1 overlap with the REPL/MCP surface, but several
//! interactive editor capabilities have natural REPL/MCP analogues:
//!
//! | LSP capability              | REPL / MCP analogue          |
//! |-----------------------------|------------------------------|
//! | `textDocument/hover`        | MCP `docs` (Beamtalk class)  |
//! | `textDocument/completion`   | MCP `complete` / REPL op     |
//! | `textDocument/definition`   | MCP `inspect` / class source |
//! | `workspace/symbol`          | MCP `list_classes`           |
//! | Erlang FFI hover (BT-1903)  | MCP `docs` (Erlang module)   |
//!
//! Each fixture asserts cross-surface equivalence, intentionally lenient
//! about transport-level differences (markdown wrapping, list ordering,
//! pluralisation) but strict about the underlying *content* — i.e. the
//! same class names, the same documentation tokens, the same definition
//! file resolution.
//!
//! All `#[test]`s are gated behind `#[ignore]` because they spawn real
//! workspaces and child binaries. Run via `just test-parity`.

#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::uninlined_format_args,
    clippy::too_many_lines
)]

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use beamtalk_parity_tests::drivers::{lsp::LspDriver, mcp::McpDriver};
use beamtalk_parity_tests::pool::{SharedRepl, beamtalk_binary, shared_repl};

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
#[ignore = "lsp parity — run via `just test-parity`"]
async fn lsp_capability_parity() {
    let staged = stage_widget_project().expect("stage widget project");

    let repl = shared_repl().expect("start shared workspace");
    let mut mcp = McpDriver::spawn(&repl).await.expect("spawn mcp driver");

    // The MCP-side surfaces (docs / list_classes / complete) need the
    // project loaded into the shared workspace. Use `force=true` so a
    // re-run on a hot workspace still surfaces the fresh class list.
    let load_out = mcp
        .load_project(staged.to_string_lossy().as_ref())
        .await
        .expect("mcp load_project");
    let loaded_classes = load_out.classes.as_ref();
    assert!(
        loaded_classes.is_some_and(|cs| cs.contains("Widget") && cs.contains("Gadget")),
        "mcp load_project did not register Widget/Gadget — got classes={:?}, raw={}",
        loaded_classes,
        load_out.raw,
    );

    let mut lsp = LspDriver::spawn(&staged).await.expect("spawn lsp driver");

    let widget = staged.join("src/widget.bt");
    let widget_uri = lsp.open_file(&widget).await.expect("lsp open widget.bt");

    let mut failures: Vec<String> = Vec::new();

    if let Err(e) = check_hover_parity(&mut lsp, &widget_uri, &mut mcp).await {
        failures.push(format!("hover: {e}"));
    }
    if let Err(e) = check_completion_parity(&mut lsp, &widget_uri, &mut mcp).await {
        failures.push(format!("completion: {e}"));
    }
    if let Err(e) = check_definition_parity(&mut lsp, &widget_uri, &staged).await {
        failures.push(format!("definition: {e}"));
    }
    if let Err(e) = check_workspace_symbol_parity(&mut lsp, &mut mcp).await {
        failures.push(format!("workspace/symbol: {e}"));
    }
    if let Err(e) = check_erlang_ffi_hover_parity(&mut mcp).await {
        failures.push(format!("erlang FFI hover: {e}"));
    }

    lsp.close().await;
    mcp.close().await;
    stop_parity_workspace(&repl);

    assert!(
        failures.is_empty(),
        "lsp parity assertions failed:\n  - {}",
        failures.join("\n  - ")
    );
}

/// Hover on the `Widget` class identifier in `Value subclass: Widget`.
///
/// LSP returns a markdown payload; MCP `docs` returns a similar markdown
/// payload from the workspace's `Beamtalk help:` runtime helper. Both must
/// mention the class name and at least one identifying token from the
/// docstring so accidental empty payloads or stale strings get caught.
async fn check_hover_parity(
    lsp: &mut LspDriver,
    widget_uri: &str,
    mcp: &mut McpDriver,
) -> Result<(), String> {
    // Source has `Value subclass: Widget` on line 8 (0-indexed: 7); the
    // identifier `Widget` starts at column 16. Hovering the identifier
    // should produce a class hover.
    let lsp_hover = lsp
        .hover(widget_uri, 7, 18)
        .await
        .map_err(|e| format!("lsp hover: {e}"))?;
    if lsp_hover.trim().is_empty() {
        return Err("LSP hover returned empty payload".to_string());
    }
    if !lsp_hover.contains("Widget") {
        return Err(format!(
            "LSP hover missing class name `Widget`: {}",
            truncate(&lsp_hover)
        ));
    }

    let mcp_docs = mcp
        .docs_class("Widget")
        .await
        .map_err(|e| format!("mcp docs: {e}"))?;
    if mcp_docs.trim().is_empty() {
        return Err("MCP docs returned empty payload".to_string());
    }
    if !mcp_docs.contains("Widget") {
        return Err(format!(
            "MCP docs missing class name `Widget`: {}",
            truncate(&mcp_docs)
        ));
    }

    // Both surfaces expose the runtime help payload, which carries the
    // class superclass declaration. Verify both reference `Value` (the
    // parent) so we know neither is silently returning a stub.
    if !lsp_hover.contains("Value") {
        return Err(format!(
            "LSP hover does not reference superclass `Value`: {}",
            truncate(&lsp_hover)
        ));
    }
    if !mcp_docs.contains("Value") {
        return Err(format!(
            "MCP docs does not reference superclass `Value`: {}",
            truncate(&mcp_docs)
        ));
    }
    Ok(())
}

/// Completion at the start of `Widget` in `widget.bt`.
///
/// LSP `textDocument/completion` returns a list of items with `label`s; MCP
/// `complete` returns newline-delimited candidates. The two surfaces draw
/// from different completion engines: LSP combines class names + bindings +
/// keywords + snippets via the language service, while MCP delegates to the
/// REPL `complete` op which surfaces bindings + class names. A strict
/// item-set match is not feasible, but the loaded `Widget` class must be
/// visible on at least one surface — and LSP must include it because the
/// project was indexed at handshake time.
///
/// Item kinds are intentionally not compared: LSP carries
/// `CompletionItemKind::CLASS`, MCP only carries the label string.
async fn check_completion_parity(
    lsp: &mut LspDriver,
    widget_uri: &str,
    mcp: &mut McpDriver,
) -> Result<(), String> {
    let lsp_items = lsp
        .completion(widget_uri, 0, 0)
        .await
        .map_err(|e| format!("lsp completion: {e}"))?;
    if lsp_items.is_empty() {
        return Err("lsp completion returned no items".to_string());
    }
    if !lsp_items.iter().any(|s| s == "Widget") {
        return Err(format!(
            "lsp completion did not surface Widget class: {lsp_items:?}"
        ));
    }

    // MCP `complete` against the same partial input. The REPL completion
    // op is bindings-focused; an empty result is acceptable when no
    // matching session binding exists, but a non-empty result must not
    // contain transport noise (a leading "No completions" line).
    let mcp_items = mcp
        .complete("Widget")
        .await
        .map_err(|e| format!("mcp complete: {e}"))?;
    for item in &mcp_items {
        if item.contains("No completions") {
            return Err(format!(
                "mcp complete returned a `No completions` marker as a candidate: {mcp_items:?}"
            ));
        }
    }
    Ok(())
}

/// Goto definition for `Widget` referenced inside `widget.bt`.
///
/// The LSP must resolve the location to a `file://` URI pointing at the
/// staged `widget.bt` file. MCP doesn't expose goto-definition directly, so
/// we treat the workspace's view of the class source as the analogue: the
/// class must be loaded (visible via `list_classes`), and the resolved LSP
/// URI must point inside the staged project root.
async fn check_definition_parity(
    lsp: &mut LspDriver,
    widget_uri: &str,
    staged: &Path,
) -> Result<(), String> {
    // `Widget` declaration on line 7 (0-indexed) → the identifier starts at
    // column 16; clicking inside it should return its own range.
    let resolved = lsp
        .definition(widget_uri, 7, 18)
        .await
        .map_err(|e| format!("lsp definition: {e}"))?;
    let (target_uri, _line) =
        resolved.ok_or_else(|| "LSP definition returned None for `Widget`".to_string())?;
    if !target_uri.starts_with("file://") {
        return Err(format!(
            "LSP definition uri is not a file:// uri: {target_uri}"
        ));
    }
    // The resolved URI must point at the staged project tree. We canonicalise
    // both because tempfile may round-trip through `/private/var` on macOS.
    let staged_canonical = staged
        .canonicalize()
        .unwrap_or_else(|_| staged.to_path_buf());
    let staged_str = staged_canonical.to_string_lossy().replace('\\', "/");
    if !target_uri.contains(&*staged_str) {
        return Err(format!(
            "LSP definition uri `{target_uri}` does not point inside staged project `{staged_str}`"
        ));
    }
    Ok(())
}

/// Workspace symbol query parity vs MCP `list_classes`.
///
/// Both surfaces should surface the user-defined classes from the loaded
/// project (`Widget`, `Gadget`). LSP additionally substring-filters by the
/// query string, which the harness exercises with both an empty and a
/// targeted query.
async fn check_workspace_symbol_parity(
    lsp: &mut LspDriver,
    mcp: &mut McpDriver,
) -> Result<(), String> {
    // Empty query → all user classes.
    let lsp_all = lsp
        .workspace_symbol("")
        .await
        .map_err(|e| format!("lsp workspace/symbol(\"\"): {e}"))?;
    let lsp_set: std::collections::BTreeSet<&str> = lsp_all.iter().map(String::as_str).collect();
    if !lsp_set.contains("Widget") || !lsp_set.contains("Gadget") {
        return Err(format!(
            "lsp workspace/symbol missing user classes: got {lsp_all:?}"
        ));
    }

    // MCP `list_classes` with the `user` filter scopes to the same set.
    let mcp_user = mcp
        .list_classes(Some("user"))
        .await
        .map_err(|e| format!("mcp list_classes(user): {e}"))?;
    let mcp_set: std::collections::BTreeSet<&str> = mcp_user.iter().map(String::as_str).collect();
    if !mcp_set.contains("Widget") || !mcp_set.contains("Gadget") {
        return Err(format!(
            "mcp list_classes(user) missing user classes: got {mcp_user:?}"
        ));
    }

    // Targeted query: every surface should narrow to `Widget` only.
    let lsp_widget = lsp
        .workspace_symbol("Widget")
        .await
        .map_err(|e| format!("lsp workspace/symbol(Widget): {e}"))?;
    if !lsp_widget.iter().any(|s| s == "Widget") {
        return Err(format!(
            "lsp workspace/symbol(Widget) did not surface Widget: {lsp_widget:?}"
        ));
    }
    if lsp_widget.iter().any(|s| s == "Gadget") {
        return Err(format!(
            "lsp workspace/symbol(Widget) erroneously surfaced Gadget: {lsp_widget:?}"
        ));
    }
    Ok(())
}

/// Erlang FFI hover parity (BT-1903 + BT-2081).
///
/// MCP `docs` accepts an `erlang_module` parameter; the LSP exposes the same
/// payload via hover when the cursor is on an Erlang FFI call site. We
/// can't easily synthesise a hover-able FFI call inside the parity widget
/// project (the LSP needs an existing import + call). Instead, we assert
/// the MCP side returns content for a well-known module so the parity
/// contract is at least one-sided verifiable; the LSP-side hover for
/// Erlang FFI is covered by `crates/beamtalk-lsp` unit tests.
async fn check_erlang_ffi_hover_parity(mcp: &mut McpDriver) -> Result<(), String> {
    let lists = mcp
        .docs_erlang("lists")
        .await
        .map_err(|e| format!("mcp docs(erlang_module=lists): {e}"))?;
    if lists.trim().is_empty() {
        return Err("MCP docs(erlang_module=lists) returned empty payload".to_string());
    }
    Ok(())
}

/// Stage the LSP parity widget project to a per-process temp directory so
/// the LSP child can index the on-disk tree without touching the
/// repository copy.
fn stage_widget_project() -> Result<PathBuf, String> {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").map_err(|e| format!("manifest: {e}"))?;
    let src = PathBuf::from(manifest)
        .parent()
        .and_then(|p| p.parent())
        .ok_or_else(|| "workspace root".to_string())?
        .join("tests/parity/lsp/widget_project");
    if !src.is_dir() {
        return Err(format!("fixture missing: {}", src.display()));
    }
    let dst =
        std::env::temp_dir().join(format!("beamtalk-parity-lsp-widget-{}", std::process::id()));
    if dst.exists() {
        std::fs::remove_dir_all(&dst).map_err(|e| format!("remove stale staging dir: {e}"))?;
    }
    copy_tree(&src, &dst).map_err(|e| format!("copy_tree: {e}"))?;
    Ok(dst)
}

fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

/// Stop the parity workspace so its loaded classes don't linger in BEAM
/// node memory and bleed into other test runs.
fn stop_parity_workspace(repl: &SharedRepl) {
    let Ok(bin) = beamtalk_binary("beamtalk") else {
        return;
    };
    let _ = Command::new(bin)
        .args(["workspace", "stop", &repl.workspace_id])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
}

fn truncate(s: &str) -> String {
    const LIMIT: usize = 240;
    if s.len() <= LIMIT {
        s.to_string()
    } else {
        let mut end = LIMIT;
        while end > 0 && !s.is_char_boundary(end) {
            end -= 1;
        }
        let mut t = s[..end].to_string();
        t.push_str("...[truncated]");
        t
    }
}
