// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `resolve_flushed_path`, workspace-edit builders (delete/change/create file), flush-action classification, document-moved params, rename-class URI resolution and notification, rename-method site edits, reload-publish deferral, and the open-paths handle.

use super::*;

#[test]
fn resolve_flushed_path_canonicalises_absolute_paths_against_existing_file() {
    let temp = unique_temp_dir("beamtalk_lsp_resolve_abs");
    fs::create_dir_all(&temp).expect("create temp");
    let bt = temp.join("counter.bt");
    fs::write(&bt, "src").expect("write");
    let raw = bt.to_str().unwrap();
    let (resolved, existed) = resolve_flushed_path(raw, &[]).expect("resolved");
    // canonicalize may resolve symlinks; just verify equality with the
    // canonical form of the same input.
    let canon = bt.canonicalize().expect("canonicalize");
    assert_eq!(resolved, canon);
    assert!(existed);
    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn resolve_flushed_path_joins_against_first_matching_workspace_root() {
    let temp = unique_temp_dir("beamtalk_lsp_resolve_root");
    let root = temp.join("project");
    fs::create_dir_all(root.join("src")).expect("dirs");
    let target = root.join("src/counter.bt");
    fs::write(&target, "src").expect("write");

    let (resolved, existed) =
        resolve_flushed_path("src/counter.bt", std::slice::from_ref(&root)).expect("resolved");
    assert_eq!(resolved, target.canonicalize().expect("canonicalize"));
    assert!(existed);
    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn resolve_flushed_path_returns_none_when_nothing_matches() {
    // A nonexistent relative path with no roots, and no real parent
    // directory either — nothing to find, deleted or otherwise.
    assert!(resolve_flushed_path("does/not/exist.bt", &[]).is_none());
}

// ADR 0113 Phase 4a (BT-3209): a Tier 2 destructive flush (`remove-class`)
// has already unlinked its file by the time `flush_completed` fires —
// `resolve_flushed_path` must still resolve a usable path for it (so the
// listener can build a `DeleteFile` URI), just flagged `existed = false`
// instead of failing the way plain `canonicalize()` would.

#[test]
fn resolve_flushed_path_falls_back_for_deleted_absolute_file() {
    let temp = unique_temp_dir("beamtalk_lsp_resolve_deleted_abs");
    fs::create_dir_all(&temp).expect("create temp");
    let bt = temp.join("counter.bt");
    fs::write(&bt, "src").expect("write");
    fs::remove_file(&bt).expect("delete");

    let raw = bt.to_str().unwrap();
    let (resolved, existed) = resolve_flushed_path(raw, &[]).expect("resolved");
    assert!(!existed);
    // The leaf no longer exists, so we can't canonicalise it — the
    // literal (still-absolute) candidate path is returned as-is.
    assert_eq!(resolved, bt);
    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn resolve_flushed_path_falls_back_for_deleted_relative_file() {
    let temp = unique_temp_dir("beamtalk_lsp_resolve_deleted_rel");
    let root = temp.join("project");
    fs::create_dir_all(root.join("src")).expect("dirs");
    let target = root.join("src/counter.bt");
    fs::write(&target, "src").expect("write");
    fs::remove_file(&target).expect("delete");

    let (resolved, existed) =
        resolve_flushed_path("src/counter.bt", std::slice::from_ref(&root)).expect("resolved");
    assert!(!existed);
    assert_eq!(resolved, target);
    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn resolve_flushed_path_returns_none_when_parent_dir_also_missing() {
    // Not merely a missing leaf — the whole directory tree is fictional,
    // so there is no real parent to fall back to either.
    let temp = unique_temp_dir("beamtalk_lsp_resolve_deleted_no_parent");
    // Note: `temp` itself is never created on disk.
    let raw = temp.join("src/counter.bt");
    let raw = raw.to_str().unwrap();
    assert!(resolve_flushed_path(raw, &[]).is_none());
}

// ADR 0113 Phase 4a (BT-3209): the `WorkspaceEdit` builders the flush
// listener dispatches to, tested directly since driving a full
// `client.apply_edit` round trip needs a live LSP client on the other
// end of the socket.

#[test]
fn delete_file_edit_emits_typed_delete_resource_op() {
    let uri = Url::parse("file:///workspace/Counter.bt").expect("uri");
    let edit = delete_file_edit(uri.clone());

    // No plain-text `changes` map — a delete has no content to send.
    assert!(edit.changes.is_none());
    match edit.document_changes {
        Some(DocumentChanges::Operations(ops)) => {
            assert_eq!(ops.len(), 1);
            match &ops[0] {
                DocumentChangeOperation::Op(ResourceOp::Delete(delete)) => {
                    assert_eq!(delete.uri, uri);
                    let options = delete.options.as_ref().expect("delete options");
                    assert_eq!(options.ignore_if_not_exists, Some(true));
                    assert_eq!(options.recursive, Some(false));
                }
                other => panic!("expected a Delete resource op, got {other:?}"),
            }
        }
        other => panic!("expected Operations document_changes, got {other:?}"),
    }
}

#[test]
fn change_file_edit_emits_whole_document_text_edit() {
    // Regression coverage (AC: "existing non-destructive flush LSP
    // behavior... unaffected") — this must keep the exact shape
    // `flush_event_listener` sent before ADR 0113 Phase 4a: a `changes`
    // map with one whole-document `TextEdit`, no `documentChanges`.
    let uri = Url::parse("file:///workspace/Counter.bt").expect("uri");
    let edit = change_file_edit(uri.clone(), "Object subclass: Counter\n".to_string());

    assert!(edit.document_changes.is_none());
    let changes = edit.changes.expect("changes map");
    let edits = changes.get(&uri).expect("edit for uri");
    assert_eq!(edits.len(), 1);
    assert_eq!(edits[0].new_text, "Object subclass: Counter\n");
    assert_eq!(
        edits[0].range,
        Range {
            start: Position {
                line: 0,
                character: 0
            },
            end: Position {
                line: u32::MAX,
                character: u32::MAX
            },
        }
    );
}

// BT-3212 (ADR 0113 LSP follow-up): the `CreateFile` builder and the
// `classify_flush_action` dispatch, tested the same way the BT-3209
// `DeleteFile`/`Change` builders above are — directly, since driving a
// full `client.apply_edit` round trip needs a live LSP client.

#[test]
fn create_file_edit_emits_typed_create_resource_op_with_content() {
    let uri = Url::parse("file:///workspace/Greeter.bt").expect("uri");
    let edit = create_file_edit(uri.clone(), "Object subclass: Greeter\n".to_string());

    // No plain-text `changes` map — `CreateFile` + its content edit both
    // live in `documentChanges` (the LSP spec forbids mixing the two).
    assert!(edit.changes.is_none());
    match edit.document_changes {
        Some(DocumentChanges::Operations(ops)) => {
            assert_eq!(ops.len(), 2);
            match &ops[0] {
                DocumentChangeOperation::Op(ResourceOp::Create(create)) => {
                    assert_eq!(create.uri, uri);
                    let options = create.options.as_ref().expect("create options");
                    assert_eq!(options.ignore_if_exists, Some(true));
                    assert_eq!(options.overwrite, None);
                }
                other => panic!("expected a Create resource op, got {other:?}"),
            }
            match &ops[1] {
                DocumentChangeOperation::Edit(text_edit) => {
                    assert_eq!(text_edit.text_document.uri, uri);
                    assert_eq!(text_edit.edits.len(), 1);
                    match &text_edit.edits[0] {
                        OneOf::Left(edit) => {
                            assert_eq!(edit.new_text, "Object subclass: Greeter\n");
                            assert_eq!(
                                edit.range,
                                Range {
                                    start: Position {
                                        line: 0,
                                        character: 0
                                    },
                                    end: Position {
                                        line: u32::MAX,
                                        character: u32::MAX
                                    },
                                }
                            );
                        }
                        other @ OneOf::Right(_) => {
                            panic!("expected a plain TextEdit, got {other:?}")
                        }
                    }
                }
                other @ DocumentChangeOperation::Op(_) => {
                    panic!("expected a TextDocumentEdit, got {other:?}")
                }
            }
        }
        other => panic!("expected Operations document_changes, got {other:?}"),
    }
}

#[test]
fn classify_flush_action_uses_wire_kind_when_present() {
    // A present `kind` drives the decision directly, regardless of the
    // (otherwise-irrelevant) existence flag.
    assert_eq!(
        classify_flush_action(Some(FlushFileKind::NewClass), true),
        FlushAction::Create
    );
    assert_eq!(
        classify_flush_action(Some(FlushFileKind::NewClass), false),
        FlushAction::Create
    );
    assert_eq!(
        classify_flush_action(Some(FlushFileKind::RemoveClass), true),
        FlushAction::Delete
    );
    assert_eq!(
        classify_flush_action(Some(FlushFileKind::RemoveClass), false),
        FlushAction::Delete
    );
    assert_eq!(
        classify_flush_action(Some(FlushFileKind::Patch), true),
        FlushAction::Patch
    );
    assert_eq!(
        classify_flush_action(Some(FlushFileKind::Patch), false),
        FlushAction::Patch
    );
}

#[test]
fn classify_flush_action_falls_back_to_existence_when_kind_absent() {
    // BT-3209 backward compat: no wire `kind` (a pre-BT-3212 producer)
    // falls back to the existence heuristic — and can never produce
    // `Create`, since existence alone cannot distinguish "freshly
    // created" from "patched in place".
    assert_eq!(classify_flush_action(None, true), FlushAction::Patch);
    assert_eq!(classify_flush_action(None, false), FlushAction::Delete);
}

#[test]
fn classify_flush_action_buckets_rename_kinds_to_patch_defensively() {
    // ADR 0114 LSP follow-up (BT-3275): `flush_event_listener` never
    // actually calls `classify_flush_action` for these — `RenameClass`
    // with `oldFile` and `RenameMethod` are dispatched to their own
    // branches first. This is only the defensive fallback for the
    // in-practice-unreachable remaining case (`RenameClass` reaching
    // here at all only happens when the wire carried no `oldFile`).
    assert_eq!(
        classify_flush_action(Some(FlushFileKind::RenameClass), true),
        FlushAction::Patch
    );
    assert_eq!(
        classify_flush_action(Some(FlushFileKind::RenameMethod), true),
        FlushAction::Patch
    );
}

// ADR 0114 LSP follow-up (BT-3275): the rename-method-site
// `WorkspaceEdit` builder, tested the same direct way the BT-3209/
// BT-3212 builders above are.

// BT-3285: the rename-class move path no longer builds a `WorkspaceEdit`
// at all — it sends the custom `beamtalk-lsp/documentMoved` notification
// instead (see `DocumentMoved`'s doc for why the old `RenameFile` op was
// dropped). `Client::send_notification` only actually sends once
// `ServerState` has reached `Initialized`, which a bare `LspService::new`
// in a test never reaches on its own — a naive `apply_rename_class_move`
// test against an un-initialized service just hangs `socket.next()`
// forever instead of failing loudly, since `send_notification` silently
// no-ops (never touching the socket) rather than erroring when
// un-initialized. `resolve_rename_class_old_uri` — the one part of
// `apply_rename_class_move` with real path-resolution logic — is tested
// on its own regardless, matching this file's existing preference for
// testing the `Client`-free half directly (the `resolve_flushed_path`
// tests above take the same approach for the `WorkspaceEdit` builders'
// path resolution); `DocumentMovedParams`'s wire shape is tested via
// direct serialization. `apply_rename_class_move_sends_document_moved_notification`
// below additionally exercises the full send path end-to-end, by driving
// a real `initialize` JSON-RPC request through `LspService`'s
// `tower::Service` impl first (mirroring `tower-lsp`'s own
// `initializes_only_once` test) so `ServerState` genuinely reaches
// `Initialized` before `apply_rename_class_move` runs.

#[test]
fn document_moved_params_serializes_camel_case_uri_fields() {
    let old_uri = Url::parse("file:///workspace/counter.bt").expect("uri");
    let new_uri = Url::parse("file:///workspace/accumulator.bt").expect("uri");
    let value = serde_json::to_value(DocumentMovedParams {
        old_uri: old_uri.clone(),
        new_uri: new_uri.clone(),
    })
    .expect("serialize");
    assert_eq!(
        value,
        serde_json::json!({
            "oldUri": old_uri.to_string(),
            "newUri": new_uri.to_string(),
        })
    );
}

#[test]
fn resolve_rename_class_old_uri_builds_uri_for_already_deleted_old_path() {
    // Mirrors `resolve_flushed_path_falls_back_for_deleted_absolute_file`
    // above: by the time `apply_rename_class_move` runs, Phase B has
    // already unlinked the old path from disk, so the old file is
    // created and then deleted rather than left in place.
    let temp = unique_temp_dir("beamtalk_lsp_rename_class_old_uri");
    fs::create_dir_all(&temp).expect("create temp");
    let old_path = temp.join("counter.bt");
    fs::write(&old_path, "Object subclass: Counter\n").expect("write");
    fs::remove_file(&old_path).expect("delete (simulates Phase B's unlink)");

    let old_uri = resolve_rename_class_old_uri(&[], old_path.to_str().unwrap())
        .expect("resolves even though the old file is already gone");
    assert_eq!(old_uri, Url::from_file_path(&old_path).expect("uri"));

    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn resolve_rename_class_old_uri_returns_none_when_path_cannot_be_resolved() {
    // No real parent directory either — nothing to fall back to.
    assert!(resolve_rename_class_old_uri(&[], "does/not/exist/counter.bt").is_none());
}

#[tokio::test]
async fn apply_rename_class_move_sends_document_moved_notification() {
    use futures_util::StreamExt;
    use tower::{Service, ServiceExt};

    // Mirrors `resolve_flushed_path_falls_back_for_deleted_absolute_file`
    // above: by the time `apply_rename_class_move` runs, Phase B has
    // already unlinked the old path from disk, so the old file is
    // created and then deleted rather than left in place.
    let temp = unique_temp_dir("beamtalk_lsp_rename_class_move_e2e");
    fs::create_dir_all(&temp).expect("create temp");
    let old_path = temp.join("counter.bt");
    fs::write(&old_path, "Object subclass: Counter\n").expect("write");
    fs::remove_file(&old_path).expect("delete (simulates Phase B's unlink)");

    let (mut service, mut socket) = tower_lsp::LspService::new(Backend::new);

    // Drive a real `initialize` JSON-RPC request through the service's
    // `tower::Service` impl (the same mechanism `main.rs`/an actual LSP
    // client use) so `ServerState` reaches `Initialized` — otherwise
    // `Client::send_notification` below silently no-ops. Directly
    // calling `Backend::initialize` would not do this: the state
    // transition lives in `tower-lsp`'s own `InitializeLayer`, wrapping
    // `LspService::call`, not in the `LanguageServer::initialize` method.
    let init_request = tower_lsp::jsonrpc::Request::build("initialize")
        .params(serde_json::json!({"capabilities": {}}))
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

    let client = service.inner().client.clone();
    let new_path = temp.join("accumulator.bt");
    let new_uri = Url::from_file_path(&new_path).expect("uri");

    apply_rename_class_move(&client, &[], new_uri.clone(), old_path.to_str().unwrap()).await;

    let notification = socket
        .next()
        .await
        .expect("expected a beamtalk-lsp/documentMoved notification");
    assert_eq!(notification.method(), "beamtalk-lsp/documentMoved");
    let params = notification
        .params()
        .expect("notification carries params")
        .clone();
    let expected_old_uri = Url::from_file_path(&old_path).expect("uri");
    assert_eq!(
        params,
        serde_json::json!({
            "oldUri": expected_old_uri.to_string(),
            "newUri": new_uri.to_string(),
        })
    );

    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn rename_method_site_edit_emits_typed_text_document_edit() {
    let uri = Url::parse("file:///workspace/counter.bt").expect("uri");
    let edit = rename_method_site_edit(
        uri.clone(),
        "Object subclass: Counter\n  incrementBy => self.value := self.value + 1\nend\n"
            .to_string(),
    );

    // No plain-text `changes` map — the ADR calls for a typed
    // `TextDocumentEdit` per confirmed site, not `change_file_edit`'s
    // generic patch shape.
    assert!(edit.changes.is_none());
    match edit.document_changes {
        Some(DocumentChanges::Operations(ops)) => {
            assert_eq!(ops.len(), 1);
            match &ops[0] {
                DocumentChangeOperation::Edit(text_edit) => {
                    assert_eq!(text_edit.text_document.uri, uri);
                    assert_eq!(text_edit.edits.len(), 1);
                    match &text_edit.edits[0] {
                        OneOf::Left(edit) => {
                            assert_eq!(
                                edit.new_text,
                                "Object subclass: Counter\n  incrementBy => self.value := self.value + 1\nend\n"
                            );
                            assert_eq!(
                                edit.range,
                                Range {
                                    start: Position {
                                        line: 0,
                                        character: 0
                                    },
                                    end: Position {
                                        line: u32::MAX,
                                        character: u32::MAX
                                    },
                                }
                            );
                        }
                        other @ OneOf::Right(_) => {
                            panic!("expected a plain TextEdit, got {other:?}")
                        }
                    }
                }
                other @ DocumentChangeOperation::Op(_) => {
                    panic!("expected a TextDocumentEdit, got {other:?}")
                }
            }
        }
        other => panic!("expected Operations document_changes, got {other:?}"),
    }
}

/// BT-3433 (follow-up): `reload_check_listener`/`seed_reload_diagnostics`
/// must defer their `publish_diagnostics_impl` send — same as
/// `Backend::publish_diagnostics` already does for
/// `did_open`/`did_change`/`did_save` — exactly when doing otherwise
/// would risk the startup-preload notification race: preload in-flight
/// *and* the target URI open. Neither condition alone is enough: a
/// closed URI is never resent by `republish_open_diagnostics`, so
/// deferring it would drop the diagnostic forever, and an open URI with
/// preload already settled has no race to avoid.
#[test]
fn should_defer_reload_publish_for_preload_requires_open_and_in_progress() {
    // Real (if nonexistent) platform-native paths, not a hand-written
    // `/workspace/...` literal — `Url::from_file_path` requires an
    // absolute path in the host platform's own form (e.g. a drive
    // letter on Windows), which a Unix-style literal is not.
    let temp = unique_temp_dir("beamtalk_lsp_defer_reload_publish");
    let path = Utf8PathBuf::from_path_buf(temp.join("activity_retry_helper.bt"))
        .expect("temp path is UTF-8");
    let uri = Url::from_file_path(path.as_std_path()).expect("path → uri");
    let versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>> =
        Arc::new(Mutex::new(HashMap::from([(path.clone(), 1)])));
    let open_paths = OpenPathsHandle {
        versions: Arc::clone(&versions),
    };

    let mut service = SimpleLanguageService::new();
    assert!(
        !should_defer_reload_publish_for_preload(&Mutex::new(service.clone()), &open_paths, &uri),
        "preload settled: nothing to race, must not defer an open file's publish"
    );

    service.set_preload_in_progress(true);
    assert!(
        should_defer_reload_publish_for_preload(&Mutex::new(service.clone()), &open_paths, &uri),
        "preload in-flight and URI open: must defer to republish_open_diagnostics"
    );

    let closed_path =
        Utf8PathBuf::from_path_buf(temp.join("activity_outcome.bt")).expect("temp path is UTF-8");
    let closed_uri = Url::from_file_path(closed_path.as_std_path()).expect("path → uri");
    assert!(
        !should_defer_reload_publish_for_preload(&Mutex::new(service), &open_paths, &closed_uri),
        "preload in-flight but URI not open: nothing will ever resend it, must publish now"
    );
}

#[test]
fn open_paths_handle_observes_late_inserts() {
    // ADR 0082 Phase 3 (BT-2289): the listener must see files opened
    // *after* the runtime client attached. Verify the handle reads the
    // live map, not a captured snapshot, by inserting after handle
    // creation and confirming `contains` flips from false to true.
    let versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>> = Arc::new(Mutex::new(HashMap::new()));
    let handle = OpenPathsHandle {
        versions: Arc::clone(&versions),
    };
    let path = Utf8PathBuf::from("/workspace/counter.bt");
    assert!(!handle.contains(&path));
    versions
        .lock()
        .expect("versions lock")
        .insert(path.clone(), 1);
    assert!(handle.contains(&path));
}

#[test]
fn open_paths_handle_observes_late_removes() {
    // Symmetric to the late-inserts case: closing a file in the editor
    // should immediately flip `contains` back to false.
    let versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>> = Arc::new(Mutex::new(HashMap::new()));
    let handle = OpenPathsHandle {
        versions: Arc::clone(&versions),
    };
    let path = Utf8PathBuf::from("/workspace/counter.bt");
    versions
        .lock()
        .expect("versions lock")
        .insert(path.clone(), 1);
    assert!(handle.contains(&path));
    versions.lock().expect("versions lock").remove(&path);
    assert!(!handle.contains(&path));
}
