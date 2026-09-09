// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for the LSP server (`beamtalk-lsp`'s `Backend` and its
//! supporting helpers).
//!
//! Tests are organized into feature-focused sub-modules:
//! - [`hover_and_stdlib`] — hover `SymbolKind` disambiguation, stdlib
//!   source-dir resolution and policy notes, hover class-name extraction
//! - [`content_fetch`] — `load_type_cache` and stdlib `fetch_content`
//! - [`diagnostics_table`] — `beamtalk.toml` `[diagnostics]` table
//! - [`preload_racing`] — sibling-root package stamps and didOpen-vs-preload
//!   races
//! - [`did_close_racing`] — `did_close` reverting to disk vs. racing a
//!   reopen or `did_change`
//! - [`preload_files`] — `collect_preload_files` /
//!   `preload_workspace_source_files`
//! - [`formatting_and_paths`] — `format_source`, `resolve_path_for_uri`,
//!   `position_to_offset`
//! - [`command_builders`] — ADR 0082 Phase 3 executeCommand expression
//!   builders and their argument validation
//! - [`flush_and_rename_edits`] — workspace-edit builders, flush-action
//!   classification, and rename-class/-method support
//! - [`call_hierarchy`] — callHierarchy helpers
//! - [`references`] — `textDocument/references` declaration-merge,
//!   driven through the [`open_test_file`] / [`real_did_open`] harness below
//! - [`type_hierarchy`] — goto-implementation and
//!   `prepare_type_hierarchy/supertypes/subtypes`
//! - [`symbol_unification`] — workspace/document symbol unification

pub use super::*;
pub(crate) use beamtalk_core::test_helpers::unique_temp_dir;
pub(crate) use camino::Utf8PathBuf;
pub(crate) use std::fs;

/// Helper: open a single file in a `Backend` test fixture by directly
/// updating the service and versions maps. Skips the full LSP lifecycle
/// (`publish_diagnostics`, etc.) so handler logic can be exercised in
/// isolation. Returns the URI that `textDocument/references` requests
/// should target.
pub(crate) fn open_test_file(backend: &Backend, path: &Utf8PathBuf, source: &str) -> Url {
    {
        let mut svc = backend.service.lock().expect("service lock");
        svc.update_file(path.clone(), source.to_string());
    }
    {
        let mut versions = backend.versions.lock().expect("versions lock");
        versions.insert(path.clone(), 1);
    }
    Url::from_file_path(path.as_std_path()).expect("path → uri")
}

/// Helper: drives the real `did_open` handler (unlike `open_test_file`
/// above) so its own `publish_diagnostics` call actually runs — used by
/// `did_open_during_preload_defers_to_republish_for_sibling_class`
/// to verify that call's send behavior, not just the resulting state.
pub(crate) async fn real_did_open(backend: &Backend, uri: Url, text: &str) {
    backend
        .did_open(DidOpenTextDocumentParams {
            text_document: tower_lsp::lsp_types::TextDocumentItem {
                uri,
                language_id: "beamtalk".to_string(),
                version: 1,
                text: text.to_string(),
            },
        })
        .await;
}

/// Helper: drives a real `initialize` JSON-RPC request through
/// `LspService`'s `tower::Service` impl so `ServerState` reaches
/// `Initialized` — a bare `LspService::new` never does, and
/// `Client::publish_diagnostics`/`send_notification` silently no-op
/// (never touching the socket) until it does.
pub(crate) async fn initialize_service(
    service: &mut tower_lsp::LspService<Backend>,
    project_root: &Path,
) {
    use tower::{Service, ServiceExt};

    let root_uri = Url::from_directory_path(project_root).expect("root uri");
    let init_request = tower_lsp::jsonrpc::Request::build("initialize")
        .params(serde_json::json!({
            "capabilities": {},
            "workspaceFolders": [{"uri": root_uri.to_string(), "name": "project"}],
        }))
        .id(1)
        .finish();
    let init_response = service
        .ready()
        .await
        .expect("service ready")
        .call(init_request)
        .await
        .expect("initialize call succeeds");
    assert!(
        init_response.is_some_and(|r| r.is_ok()),
        "initialize request must succeed for ServerState to reach Initialized"
    );
}

/// Helper: receives exactly two `textDocument/publishDiagnostics`
/// notifications from `forward_rx` (see
/// `did_open_during_preload_defers_to_republish_for_sibling_class`'s use
/// of a spawned reader task forwarding onto this channel) and returns
/// the diagnostics keyed by URI. `.recv().await` (not `try_recv`)
/// correctly waits for the reader task to actually forward each one,
/// rather than racing it.
pub(crate) async fn recv_two_publish_diagnostics(
    forward_rx: &mut tokio::sync::mpsc::UnboundedReceiver<tower_lsp::jsonrpc::Request>,
) -> HashMap<Url, Vec<tower_lsp::lsp_types::Diagnostic>> {
    let mut diagnostics_by_uri = HashMap::new();
    for _ in 0..2 {
        let notification = forward_rx
            .recv()
            .await
            .expect("expected exactly two publishDiagnostics notifications");
        assert_eq!(notification.method(), "textDocument/publishDiagnostics");
        let params: tower_lsp::lsp_types::PublishDiagnosticsParams =
            serde_json::from_value(notification.params().expect("params").clone())
                .expect("valid PublishDiagnosticsParams");
        diagnostics_by_uri.insert(params.uri, params.diagnostics);
    }
    diagnostics_by_uri
}

pub(crate) fn references_params(
    uri: Url,
    line: u32,
    character: u32,
    include_declaration: bool,
) -> ReferenceParams {
    ReferenceParams {
        text_document_position: tower_lsp::lsp_types::TextDocumentPositionParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri },
            position: tower_lsp::lsp_types::Position::new(line, character),
        },
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
        context: tower_lsp::lsp_types::ReferenceContext {
            include_declaration,
        },
    }
}

mod call_hierarchy;
mod command_builders;
mod content_fetch;
mod diagnostics_table;
mod did_close_racing;
mod flush_and_rename_edits;
mod formatting_and_paths;
mod hover_and_stdlib;
mod preload_files;
mod preload_racing;
mod references;
mod symbol_unification;
mod type_hierarchy;
