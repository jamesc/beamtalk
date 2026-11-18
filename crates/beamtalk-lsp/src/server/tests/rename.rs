// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `textDocument/prepareRename` and `textDocument/rename` (BT-5007): the
//! advertised capability, refusals, and the `WorkspaceEdit` shape for each
//! client capability set (annotations, plain `documentChanges`, `changes`).

use super::*;
use crate::server::handlers::{RenameClientCaps, build_rename_edit};
use tower_lsp::lsp_types::{
    DocumentChangeOperation, DocumentChanges, OneOf, PrepareRenameResponse, RenameParams,
    ResourceOp, TextDocumentPositionParams,
};

const FULL: RenameClientCaps = RenameClientCaps {
    document_changes: true,
    change_annotations: true,
    rename_file: true,
};

fn backend_with(files: &[(&str, &str)]) -> (tower_lsp::LspService<Backend>, Vec<Url>) {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let uris = {
        let backend: &Backend = service.inner();
        files
            .iter()
            .map(|(path, text)| open_test_file(backend, &Utf8PathBuf::from(*path), text))
            .collect()
    };
    (service, uris)
}

fn rename_params(uri: &Url, line: u32, character: u32, new_name: &str) -> RenameParams {
    RenameParams {
        text_document_position: TextDocumentPositionParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            position: tower_lsp::lsp_types::Position::new(line, character),
        },
        new_name: new_name.to_string(),
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
    }
}

const COUNTER: &str = "Object subclass: Counter\n  increment => 1\n  twice => self increment\n";
const USER: &str = "x := Counter new increment\n";

#[tokio::test]
async fn initialize_advertises_rename_with_prepare_provider() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let result = backend
        .initialize(InitializeParams::default())
        .await
        .expect("initialize");
    match result.capabilities.rename_provider {
        Some(OneOf::Right(options)) => assert_eq!(options.prepare_provider, Some(true)),
        other => panic!("expected RenameOptions with prepareProvider, got {other:?}"),
    }
}

#[test]
fn client_caps_are_read_from_initialize_params() {
    let params: InitializeParams = serde_json::from_value(serde_json::json!({
        "capabilities": {"workspace": {"workspaceEdit": {
            "documentChanges": true,
            "resourceOperations": ["rename"],
            "changeAnnotationSupport": {"groupsOnLabel": true}
        }}}
    }))
    .unwrap();
    assert_eq!(RenameClientCaps::from_params(&params), FULL);
    assert_eq!(
        RenameClientCaps::from_params(&InitializeParams::default()),
        RenameClientCaps::default()
    );
}

#[tokio::test]
async fn prepare_rename_returns_range_and_placeholder() {
    let (service, uris) = backend_with(&[("/p/Counter.bt", COUNTER)]);
    let backend: &Backend = service.inner();
    // `increment` on line 1, columns 2..11.
    let response = backend
        .prepare_rename(TextDocumentPositionParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier {
                uri: uris[0].clone(),
            },
            position: tower_lsp::lsp_types::Position::new(1, 4),
        })
        .await
        .expect("ok")
        .expect("some");
    match response {
        PrepareRenameResponse::RangeWithPlaceholder { range, placeholder } => {
            assert_eq!(placeholder, "increment");
            assert_eq!(range.start, tower_lsp::lsp_types::Position::new(1, 2));
            assert_eq!(range.end, tower_lsp::lsp_types::Position::new(1, 11));
        }
        other => panic!("unexpected {other:?}"),
    }
}

#[tokio::test]
async fn prepare_rename_refuses_a_literal_with_a_request_failed_error() {
    let (service, uris) = backend_with(&[("/p/a.bt", "x := 42\n")]);
    let backend: &Backend = service.inner();
    let err = backend
        .prepare_rename(TextDocumentPositionParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier {
                uri: uris[0].clone(),
            },
            position: tower_lsp::lsp_types::Position::new(0, 6),
        })
        .await
        .expect_err("literal is not renameable");
    assert_eq!(err.code, tower_lsp::jsonrpc::ErrorCode::ServerError(-32803));
    assert!(err.message.contains("Only class names"), "{}", err.message);
}

#[tokio::test]
async fn rename_collision_is_refused_with_a_message() {
    let (service, uris) = backend_with(&[
        ("/p/Counter.bt", COUNTER),
        ("/p/Other.bt", "Object subclass: Other\n  go => 1\n"),
    ]);
    let backend: &Backend = service.inner();
    let err = backend
        .rename(rename_params(&uris[0], 0, 20, "Other"))
        .await
        .expect_err("collision");
    assert!(err.message.contains("already exists"), "{}", err.message);
}

#[tokio::test]
async fn class_rename_emits_text_edits_then_the_file_rename() {
    let (service, uris) = backend_with(&[("/p/Counter.bt", COUNTER), ("/p/use.bt", USER)]);
    let backend: &Backend = service.inner();
    *backend.rename_caps.lock().unwrap() = FULL;
    let edit = backend
        .rename(rename_params(&uris[0], 0, 20, "Tally"))
        .await
        .expect("ok")
        .expect("edit");
    let Some(DocumentChanges::Operations(ops)) = edit.document_changes else {
        panic!("expected documentChanges operations");
    };
    assert!(edit.change_annotations.is_none());
    let (edits, moves): (Vec<_>, Vec<_>) = ops
        .iter()
        .partition(|op| matches!(op, DocumentChangeOperation::Edit(_)));
    assert_eq!(edits.len(), 2, "declaration file and use file");
    let [DocumentChangeOperation::Op(ResourceOp::Rename(mv))] = moves.as_slice() else {
        panic!("expected exactly one RenameFile, got {moves:?}");
    };
    assert!(mv.old_uri.path().ends_with("/p/Counter.bt"));
    assert!(mv.new_uri.path().ends_with("/p/Tally.bt"));
    // The move comes after every text edit.
    assert!(matches!(
        ops.last(),
        Some(DocumentChangeOperation::Op(ResourceOp::Rename(_)))
    ));
}

#[tokio::test]
async fn selector_rename_annotates_candidates_when_the_client_supports_it() {
    let (service, uris) = backend_with(&[("/p/Counter.bt", COUNTER), ("/p/use.bt", USER)]);
    let backend: &Backend = service.inner();
    *backend.rename_caps.lock().unwrap() = FULL;
    let edit = backend
        .rename(rename_params(&uris[0], 1, 4, "bump"))
        .await
        .expect("ok")
        .expect("edit");
    let annotations = edit.change_annotations.expect("annotations");
    assert_eq!(annotations.len(), 1);
    let annotation = annotations.values().next().unwrap();
    assert_eq!(annotation.needs_confirmation, Some(true));
    let Some(DocumentChanges::Operations(ops)) = edit.document_changes else {
        panic!("expected documentChanges operations");
    };
    let mut plain = 0;
    let mut annotated = 0;
    for op in &ops {
        let DocumentChangeOperation::Edit(doc) = op else {
            panic!("selector rename has no file operations");
        };
        for e in &doc.edits {
            match e {
                OneOf::Left(t) => {
                    plain += 1;
                    assert_ne!(t.new_text, "");
                }
                OneOf::Right(a) => {
                    annotated += 1;
                    assert!(annotations.contains_key(&a.annotation_id));
                    assert!(doc.text_document.uri.path().ends_with("/p/use.bt"));
                }
            }
        }
    }
    assert_eq!(plain, 2, "definition + `self increment`");
    assert_eq!(annotated, 1, "the external send is a candidate");
}

fn selector_plan() -> (beamtalk_language_service::RenamePlan, SimpleLanguageService) {
    let mut svc = SimpleLanguageService::new();
    svc.update_file(Utf8PathBuf::from("/p/Counter.bt"), COUNTER.to_string());
    svc.update_file(Utf8PathBuf::from("/p/use.bt"), USER.to_string());
    let plan = svc
        .plan_rename(
            &Utf8PathBuf::from("/p/Counter.bt"),
            beamtalk_language_service::Position::new(1, 4),
            "bump",
        )
        .expect("plan");
    (plan, svc)
}

#[test]
fn client_without_change_annotation_support_gets_confirmed_edits_and_a_message() {
    let (plan, svc) = selector_plan();
    let caps = RenameClientCaps {
        document_changes: true,
        change_annotations: false,
        rename_file: true,
    };
    let outcome = build_rename_edit(&plan, caps, &|f| svc.file_source(f), &HashMap::new());
    assert!(outcome.edit.change_annotations.is_none());
    let Some(DocumentChanges::Operations(ops)) = outcome.edit.document_changes else {
        panic!("expected documentChanges");
    };
    let total: usize = ops
        .iter()
        .map(|op| match op {
            DocumentChangeOperation::Edit(doc) => {
                assert!(doc.edits.iter().all(|e| matches!(e, OneOf::Left(_))));
                doc.edits.len()
            }
            DocumentChangeOperation::Op(_) => 0,
        })
        .sum();
    assert_eq!(total, 2, "candidates are left out of the edit");
    let message = outcome.message.expect("candidates listed in a message");
    assert!(message.contains("/p/use.bt:1"), "{message}");
    assert!(message.contains("1 other"), "{message}");
}

#[test]
fn client_without_document_changes_gets_a_plain_changes_map() {
    let (plan, svc) = selector_plan();
    let outcome = build_rename_edit(
        &plan,
        RenameClientCaps::default(),
        &|f| svc.file_source(f),
        &HashMap::new(),
    );
    assert!(outcome.edit.document_changes.is_none());
    let changes = outcome.edit.changes.expect("changes map");
    assert_eq!(changes.len(), 1, "only Counter.bt has confirmed edits");
    assert!(outcome.message.is_some());
}

#[test]
fn class_rename_without_resource_operations_asks_for_a_manual_file_rename() {
    let mut svc = SimpleLanguageService::new();
    svc.update_file(Utf8PathBuf::from("/p/Counter.bt"), COUNTER.to_string());
    let plan = svc
        .plan_rename(
            &Utf8PathBuf::from("/p/Counter.bt"),
            beamtalk_language_service::Position::new(0, 20),
            "Tally",
        )
        .expect("plan");
    let caps = RenameClientCaps {
        document_changes: true,
        change_annotations: true,
        rename_file: false,
    };
    let outcome = build_rename_edit(&plan, caps, &|f| svc.file_source(f), &HashMap::new());
    let Some(DocumentChanges::Operations(ops)) = outcome.edit.document_changes else {
        panic!("expected documentChanges");
    };
    assert!(
        ops.iter()
            .all(|op| matches!(op, DocumentChangeOperation::Edit(_)))
    );
    assert!(outcome.message.expect("message").contains("Tally.bt"));
}
