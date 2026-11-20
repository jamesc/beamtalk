// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Index freshness (BT-5078): on-disk changes reach the index, cross-file
//! diagnostics follow a class rename, and `did_open` / `publish_diagnostics`
//! cannot clobber newer state.

use super::*;
use futures_util::StreamExt;
use std::time::Duration;
use tower_lsp::lsp_types::{
    DidChangeWatchedFilesParams, FileChangeType, FileEvent, PublishDiagnosticsParams,
    TextDocumentContentChangeEvent, VersionedTextDocumentIdentifier,
};

fn watched(uri: &Url, typ: FileChangeType) -> DidChangeWatchedFilesParams {
    DidChangeWatchedFilesParams {
        changes: vec![FileEvent {
            uri: uri.clone(),
            typ,
        }],
    }
}

/// A package root with `src/` and an uninitialised backend rooted there.
fn rooted_backend(name: &str) -> (tower_lsp::LspService<Backend>, Utf8PathBuf) {
    let root = Utf8PathBuf::from_path_buf(unique_temp_dir(name)).expect("utf8");
    fs::create_dir_all(root.join("src")).expect("src dir");
    fs::write(
        root.join("beamtalk.toml"),
        "[package]\nname = \"fresh\"\nversion = \"0.1.0\"\n",
    )
    .expect("manifest");
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    service
        .inner()
        .workspace_roots
        .lock_recover()
        .push(root.clone().into_std_path_buf());
    (service, root)
}

#[tokio::test]
async fn watched_file_change_delete_and_create_update_the_index() {
    let (service, root) = rooted_backend("bt-5078-watch");
    let backend: &Backend = service.inner();
    let path = root.join("src").join("a.bt");
    fs::write(&path, "Object subclass: Foo\n").expect("write");
    backend
        .service
        .lock_recover()
        .update_file(path.clone(), "Object subclass: Foo\n".to_string());
    let uri = Url::from_file_path(&path).expect("uri");

    // Changed on disk (git pull).
    fs::write(&path, "Object subclass: Bar\n").expect("rewrite");
    backend
        .did_change_watched_files(watched(&uri, FileChangeType::CHANGED))
        .await;
    assert_eq!(
        backend.service.lock_recover().file_source(&path).as_deref(),
        Some("Object subclass: Bar\n")
    );

    // Deleted.
    fs::remove_file(&path).expect("delete");
    backend
        .did_change_watched_files(watched(&uri, FileChangeType::DELETED))
        .await;
    assert!(backend.service.lock_recover().file_source(&path).is_none());

    // Created (a rename arrives as delete + create) under a preloaded dir.
    let moved = root.join("src").join("b.bt");
    fs::write(&moved, "Object subclass: Baz\n").expect("create");
    let moved_uri = Url::from_file_path(&moved).expect("uri");
    backend
        .did_change_watched_files(watched(&moved_uri, FileChangeType::CREATED))
        .await;
    assert_eq!(
        backend
            .service
            .lock_recover()
            .file_source(&moved)
            .as_deref(),
        Some("Object subclass: Baz\n")
    );
}

#[tokio::test]
async fn watched_events_never_overwrite_an_open_buffer_or_index_unrelated_files() {
    let (service, root) = rooted_backend("bt-5078-watch-skip");
    let backend: &Backend = service.inner();

    let open = root.join("src").join("open.bt");
    fs::write(&open, "Object subclass: OnDisk\n").expect("write");
    let open_uri = open_test_file(backend, &open, "Object subclass: InBuffer\n");
    backend
        .did_change_watched_files(watched(&open_uri, FileChangeType::CHANGED))
        .await;
    assert_eq!(
        backend.service.lock_recover().file_source(&open).as_deref(),
        Some("Object subclass: InBuffer\n"),
        "the editor buffer wins over disk for an open file"
    );

    let stray = root.join("scratch").join("stray.bt");
    fs::create_dir_all(stray.parent().unwrap()).expect("dir");
    fs::write(&stray, "Object subclass: Stray\n").expect("write");
    let stray_uri = Url::from_file_path(&stray).expect("uri");
    backend
        .did_change_watched_files(watched(&stray_uri, FileChangeType::CREATED))
        .await;
    assert!(
        backend.service.lock_recover().file_source(&stray).is_none(),
        "files the preload would not index stay out of the index"
    );
}

/// Starts an initialised service whose published notifications are forwarded
/// onto a channel (see `did_open_during_preload_defers_to_republish...`).
async fn service_with_publish_feed(
    project_root: &Path,
) -> (
    tower_lsp::LspService<Backend>,
    tokio::sync::mpsc::UnboundedReceiver<tower_lsp::jsonrpc::Request>,
) {
    let (mut service, mut socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, project_root).await;
    let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
    tokio::spawn(async move {
        while let Some(n) = socket.next().await {
            if tx.send(n).is_err() {
                break;
            }
        }
    });
    (service, rx)
}

fn published(n: &tower_lsp::jsonrpc::Request) -> Option<PublishDiagnosticsParams> {
    (n.method() == "textDocument/publishDiagnostics")
        .then(|| serde_json::from_value(n.params()?.clone()).ok())
        .flatten()
}

/// Rename `Foo` to `Bar` in a.bt while b.bt (which uses `Foo`) is open: b.bt
/// must show the new error without being edited.
#[tokio::test]
async fn renaming_a_class_republishes_diagnostics_for_other_open_files() {
    let root = unique_temp_dir("bt-5078-cross-file");
    fs::create_dir_all(root.join("src")).expect("src");
    let a = root.join("src").join("a.bt");
    let b = root.join("src").join("b.bt");
    let b_src = "Object subclass: User\n\n  make => Foo new\n";
    fs::write(&a, "Object subclass: Foo\n").expect("a");
    fs::write(&b, b_src).expect("b");
    let (a_uri, b_uri) = (
        Url::from_file_path(&a).expect("uri"),
        Url::from_file_path(&b).expect("uri"),
    );

    let (service, mut feed) = service_with_publish_feed(&root).await;
    let backend: &Backend = service.inner();
    real_did_open(backend, a_uri.clone(), "Object subclass: Foo\n").await;
    real_did_open(backend, b_uri.clone(), b_src).await;
    // sleep: let the feed task forward the opens' own publishes before draining.
    tokio::time::sleep(Duration::from_millis(100)).await;
    while feed.try_recv().is_ok() {}

    backend
        .did_change(tower_lsp::lsp_types::DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: a_uri.clone(),
                version: 2,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "Object subclass: Bar\n".to_string(),
            }],
        })
        .await;

    let mut b_saw_foo_unresolved = false;
    while let Ok(Some(n)) = tokio::time::timeout(Duration::from_secs(2), feed.recv()).await {
        if let Some(p) = published(&n)
            && p.uri == b_uri
            && p.diagnostics
                .iter()
                .any(|d| d.message.contains("Unresolved class") && d.message.contains("Foo"))
        {
            b_saw_foo_unresolved = true;
            break;
        }
    }
    assert!(
        b_saw_foo_unresolved,
        "b.bt must be re-checked after Foo disappears from a.bt"
    );
}

/// A `didChange` that runs while `didOpen` awaits its registry load must not
/// be overwritten by the older `didOpen` text.
#[tokio::test]
async fn did_open_cannot_overwrite_a_newer_did_change() {
    // Outside any package, so `did_open` really awaits a registry load.
    let dir = Utf8PathBuf::from_path_buf(unique_temp_dir("bt-5078-open-race")).expect("utf8");
    fs::create_dir_all(&dir).expect("dir");
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = dir.join("scratch.bt");
    let uri = Url::from_file_path(&path).expect("uri");

    let open = backend.did_open(DidOpenTextDocumentParams {
        text_document: tower_lsp::lsp_types::TextDocumentItem {
            uri: uri.clone(),
            language_id: "beamtalk".to_string(),
            version: 1,
            text: "Object subclass: Old\n".to_string(),
        },
    });
    let change = backend.did_change(tower_lsp::lsp_types::DidChangeTextDocumentParams {
        text_document: VersionedTextDocumentIdentifier {
            uri: uri.clone(),
            version: 2,
        },
        content_changes: vec![TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            text: "Object subclass: New\n".to_string(),
        }],
    });
    // `join!` polls `open` to its first await (the registry load), then runs
    // `change` while it is parked: the interleaving from the issue.
    tokio::join!(open, change);

    assert_eq!(
        backend.service.lock_recover().file_source(&path).as_deref(),
        Some("Object subclass: New\n")
    );
    assert_eq!(backend.versions.lock_recover().get(&path).copied(), Some(2));
}

/// A publish that lands after `didClose` must not put diagnostics back on the
/// closed tab.
#[tokio::test]
async fn publish_after_close_sends_nothing() {
    let root = unique_temp_dir("bt-5078-ghost");
    fs::create_dir_all(root.join("src")).expect("src");
    let path = root.join("src").join("g.bt");
    let src = "Object subclass: G\n\n  make => Missing new\n";
    fs::write(&path, src).expect("write");
    let uri = Url::from_file_path(&path).expect("uri");

    let (service, mut feed) = service_with_publish_feed(&root).await;
    let backend: &Backend = service.inner();
    real_did_open(backend, uri.clone(), src).await;
    backend
        .did_close(tower_lsp::lsp_types::DidCloseTextDocumentParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri: uri.clone() },
        })
        .await;
    while feed.try_recv().is_ok() {}

    // The stale publish (computed before the close) arrives late.
    backend.publish_diagnostics(&uri).await;
    // sleep: give the feed task time to forward a (wrongly) sent notification.
    tokio::time::sleep(Duration::from_millis(100)).await;
    assert!(
        feed.try_recv().is_err(),
        "no diagnostics may be published for a closed document"
    );
}

/// Edit 1 renames `Foo` to `Bar`; edit 2 lands inside the debounce window and
/// only touches a method body. Edit 1 is superseded, so edit 2 (the debounce
/// winner) must still republish the dependents (review of BT-5078).
#[tokio::test]
async fn superseded_class_rename_still_republishes_dependents() {
    let root = unique_temp_dir("bt-5078-superseded");
    fs::create_dir_all(root.join("src")).expect("src");
    let a = root.join("src").join("a.bt");
    let b = root.join("src").join("b.bt");
    let b_src = "Object subclass: User\n\n  make => Foo new\n";
    fs::write(&a, "Object subclass: Foo\n  go => 1\n").expect("a");
    fs::write(&b, b_src).expect("b");
    let (a_uri, b_uri) = (
        Url::from_file_path(&a).expect("uri"),
        Url::from_file_path(&b).expect("uri"),
    );

    let (service, mut feed) = service_with_publish_feed(&root).await;
    let backend: &Backend = service.inner();
    real_did_open(backend, a_uri.clone(), "Object subclass: Foo\n  go => 1\n").await;
    real_did_open(backend, b_uri.clone(), b_src).await;
    // sleep: let the feed task forward the opens' own publishes before draining.
    tokio::time::sleep(Duration::from_millis(100)).await;
    while feed.try_recv().is_ok() {}

    let change = |version: i32, text: &str| tower_lsp::lsp_types::DidChangeTextDocumentParams {
        text_document: VersionedTextDocumentIdentifier {
            uri: a_uri.clone(),
            version,
        },
        content_changes: vec![TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            text: text.to_string(),
        }],
    };
    // Both run concurrently, so edit 1 is superseded during its debounce.
    tokio::join!(
        backend.did_change(change(2, "Object subclass: Bar\n  go => 1\n")),
        backend.did_change(change(3, "Object subclass: Bar\n  go => 2\n")),
    );

    let mut b_saw_foo_unresolved = false;
    while let Ok(Some(n)) = tokio::time::timeout(Duration::from_secs(2), feed.recv()).await {
        if let Some(p) = published(&n)
            && p.uri == b_uri
            && p.diagnostics
                .iter()
                .any(|d| d.message.contains("Unresolved class") && d.message.contains("Foo"))
        {
            b_saw_foo_unresolved = true;
            break;
        }
    }
    assert!(
        b_saw_foo_unresolved,
        "the superseded rename must still re-check b.bt"
    );
}
