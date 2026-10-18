// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `textDocument/codeAction`: the ADR 0133 "return the value instead"
//! quick-fix on the escaping-block diagnostic (BT-3914), driven through the
//! real handler so the language service's multi-edit actions reach the client
//! as one `WorkspaceEdit` of LSP ranges.

use super::*;
use tower_lsp::lsp_types::{
    CodeActionContext, CodeActionOrCommand, PartialResultParams, Position, TextDocumentIdentifier,
    WorkDoneProgressParams,
};

const SOURCE: &str = "Object subclass: Twice\n  class twice: b => b value. b value\n\n  \
                      class run =>\n    t := 0\n    Twice twice: [t := t + 1]\n    t\n";

async fn code_actions_at(source: &str, name: &str, at: Position) -> Vec<CodeActionOrCommand> {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(unique_temp_dir(name).with_extension("bt"))
        .expect("temp path is UTF-8");
    let uri = open_test_file(backend, &path, source);
    // The fixture must carry the escaping-block diagnostic, so a missing
    // action is the provider's decision, not a missing diagnostic.
    let escaping = backend
        .service
        .lock()
        .expect("service lock")
        .diagnostics(&path)
        .iter()
        .filter(|d| {
            d.category
                == Some(
                    beamtalk_core::source_analysis::DiagnosticCategory::Tier2BlockNoReturnChannel,
                )
        })
        .count();
    assert_eq!(
        escaping, 1,
        "fixture must have one escaping-block diagnostic"
    );
    backend
        .code_action(CodeActionParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range { start: at, end: at },
            context: CodeActionContext::default(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .await
        .expect("rpc ok")
        .expect("some actions")
}

fn return_value_edits(actions: &[CodeActionOrCommand]) -> Option<Vec<TextEdit>> {
    actions.iter().find_map(|action| match action {
        CodeActionOrCommand::CodeAction(action)
            if action.title.starts_with("Return the new value") =>
        {
            let changes = action.edit.as_ref()?.changes.as_ref()?;
            changes.values().next().cloned()
        }
        _ => None,
    })
}

#[tokio::test]
async fn code_action_offers_return_value_instead_as_two_edits() {
    // Cursor inside `t := t + 1` (line 5, 0-based).
    let actions = code_actions_at(SOURCE, "bt-3914-offered", Position::new(5, 20)).await;
    let edits = return_value_edits(&actions).unwrap_or_else(|| panic!("{actions:?}"));
    assert_eq!(
        edits,
        vec![
            TextEdit {
                range: Range {
                    start: Position::new(5, 4),
                    end: Position::new(5, 4),
                },
                new_text: "t := ".to_string(),
            },
            TextEdit {
                range: Range {
                    start: Position::new(5, 18),
                    end: Position::new(5, 28),
                },
                new_text: "t + 1".to_string(),
            },
        ]
    );
}

#[tokio::test]
async fn code_action_not_offered_for_a_field_write() {
    let source = "Object subclass: Twice\n  field: n = 0\n  twice: b => b value. b value\n\n  \
                  run =>\n    t := 0\n    self twice: [self.n := 1. t := t + 1]\n    t\n";
    let actions = code_actions_at(source, "bt-3914-field", Position::new(6, 33)).await;
    assert!(return_value_edits(&actions).is_none(), "{actions:?}");
}
