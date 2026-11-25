// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Recovery from failures that used to leave state stale (BT-5188): a panic
//! during preload, a failed watched-files batch, and the create watcher's
//! directory walk.

use super::*;
use futures_util::{SinkExt, StreamExt};
use std::time::Duration;
use tower_lsp::lsp_types::{
    DidChangeWatchedFilesParams, FileChangeType, FileEvent, InitializedParams,
    PublishDiagnosticsParams,
};

/// An initialised service whose client traffic is forwarded onto a channel.
/// Requests the server sends to the client (`client/registerCapability`, ...)
/// are answered with `null` so the server does not wait on them.
async fn service_with_feed(
    project_root: &Path,
) -> (
    tower_lsp::LspService<Backend>,
    tokio::sync::mpsc::UnboundedReceiver<tower_lsp::jsonrpc::Request>,
) {
    let (mut service, socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, project_root).await;
    let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
    let (mut stream, mut sink) = socket.split();
    tokio::spawn(async move {
        while let Some(request) = stream.next().await {
            if let Some(id) = request.id().cloned() {
                let _ = sink
                    .send(tower_lsp::jsonrpc::Response::from_ok(
                        id,
                        serde_json::Value::Null,
                    ))
                    .await;
            }
            if tx.send(request).is_err() {
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

/// Every message the feed delivers within `wait` of the previous one.
async fn collect(
    feed: &mut tokio::sync::mpsc::UnboundedReceiver<tower_lsp::jsonrpc::Request>,
    wait: Duration,
) -> Vec<tower_lsp::jsonrpc::Request> {
    let mut out = Vec::new();
    while let Ok(Some(n)) = tokio::time::timeout(wait, feed.recv()).await {
        out.push(n);
    }
    out
}

fn publishes_for(messages: &[tower_lsp::jsonrpc::Request], uri: &Url) -> usize {
    messages
        .iter()
        .filter(|n| published(n).is_some_and(|p| &p.uri == uri))
        .count()
}

fn is_indexed(backend: &Backend, path: &Path) -> bool {
    let path = Utf8PathBuf::from_path_buf(path.to_path_buf()).expect("utf8");
    backend.service.lock_recover().file_source(&path).is_some()
}

/// A package root with `src/a.bt` (open later) and `src/b.bt` (never opened,
/// so only a preload indexes it).
fn package_with_two_files(name: &str) -> (PathBuf, PathBuf, PathBuf) {
    let root = unique_temp_dir(name);
    fs::create_dir_all(root.join("src")).expect("src");
    fs::write(
        root.join("beamtalk.toml"),
        "[package]\nname = \"recover\"\nversion = \"0.1.0\"\n",
    )
    .expect("manifest");
    let a = root.join("src").join("a.bt");
    let b = root.join("src").join("b.bt");
    fs::write(&a, "Object subclass: User\n\n  make => Helper new\n").expect("a");
    fs::write(&b, "Object subclass: Helper\n").expect("b");
    (root, a, b)
}

/// BT-5188: a panic in the startup preload republishes the open files, still
/// runs the rest of `initialized` (the watcher registrations), and retries
/// the preload on the next `did_open`.
#[tokio::test]
async fn startup_preload_panic_republishes_and_retries() {
    let (root, a, b) = package_with_two_files("bt-5188-startup-panic");
    let a_uri = Url::from_file_path(&a).expect("uri");
    let (service, mut feed) = service_with_feed(&root).await;
    let backend: &Backend = service.inner();

    // Opened before `initialized` runs, like a restored editor tab.
    real_did_open(
        backend,
        a_uri.clone(),
        "Object subclass: User\n\n  make => Helper new\n",
    )
    .await;
    let _ = collect(&mut feed, Duration::from_millis(100)).await;

    backend
        .panic_next_preload
        .store(true, std::sync::atomic::Ordering::SeqCst);
    backend.initialized(InitializedParams {}).await;
    let after = collect(&mut feed, Duration::from_secs(1)).await;
    assert!(
        publishes_for(&after, &a_uri) > 0,
        "open files are republished after a caught preload panic"
    );
    assert!(
        after
            .iter()
            .any(|n| n.method() == "client/registerCapability"),
        "the rest of `initialized` still runs after the panic"
    );
    assert!(
        !backend.service.lock_recover().is_preload_in_progress(),
        "the preload flag is cleared"
    );
    assert!(
        !is_indexed(backend, &b),
        "the panicked preload indexed b.bt"
    );

    // The retry runs on the next open.
    real_did_open(
        backend,
        a_uri,
        "Object subclass: User\n\n  make => Helper new\n",
    )
    .await;
    assert!(is_indexed(backend, &b), "the retried preload indexes b.bt");
}

/// BT-5188: a panic while loading a package root discovered from an open
/// file still publishes diagnostics for the open files, and the next open
/// under that root retries the load.
#[tokio::test]
async fn package_root_preload_panic_republishes_and_retries() {
    let temp = unique_temp_dir("bt-5188-root-panic");
    let workspace = temp.join("source");
    let (pkg, a, b) = package_with_two_files("bt-5188-root-panic-pkg");
    let pkg_in_ws = workspace.join("pkg");
    fs::create_dir_all(&workspace).expect("workspace");
    fs::rename(&pkg, &pkg_in_ws).expect("move package under workspace");
    let a = pkg_in_ws.join(a.strip_prefix(&pkg).expect("a under pkg"));
    let b = pkg_in_ws.join(b.strip_prefix(&pkg).expect("b under pkg"));
    let a_uri = Url::from_file_path(&a).expect("uri");
    let (service, mut feed) = service_with_feed(&workspace).await;
    let backend: &Backend = service.inner();

    backend
        .panic_next_preload
        .store(true, std::sync::atomic::Ordering::SeqCst);
    real_did_open(
        backend,
        a_uri.clone(),
        "Object subclass: User\n\n  make => Helper new\n",
    )
    .await;
    let after = collect(&mut feed, Duration::from_secs(1)).await;
    assert!(
        publishes_for(&after, &a_uri) > 0,
        "the open file is published after a caught package-root panic"
    );
    assert!(
        !backend.service.lock_recover().is_preload_in_progress(),
        "the preload flag is cleared"
    );
    assert!(
        !backend.workspace_roots_snapshot().contains(&pkg_in_ws),
        "the panicked root is unregistered for retry"
    );

    real_did_open(
        backend,
        a_uri,
        "Object subclass: User\n\n  make => Helper new\n",
    )
    .await;
    assert!(is_indexed(backend, &b), "the retry loads the root");
    let _ = fs::remove_dir_all(&temp);
}

/// BT-5188: a failed watched-files batch must not drop a `beamtalk.toml`
/// change reported in the same batch.
#[tokio::test]
async fn failed_watched_batch_still_reloads_the_manifest() {
    use beamtalk_core::source_analysis::DiagnosticCategory;

    let root = unique_temp_dir("bt-5188-batch-fail");
    fs::create_dir_all(root.join("src")).expect("src");
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend.workspace_roots.lock_recover().push(root.clone());

    let manifest = root.join("beamtalk.toml");
    fs::write(
        &manifest,
        "[package]\nname = \"t\"\nversion = \"0.1.0\"\n\n[diagnostics]\ndnu = \"error\"\n",
    )
    .expect("manifest");
    let bt = root.join("src").join("a.bt");
    fs::write(&bt, "Object subclass: A\n").expect("a");

    backend
        .fail_next_watched_batch
        .store(true, std::sync::atomic::Ordering::SeqCst);
    backend
        .did_change_watched_files(DidChangeWatchedFilesParams {
            changes: vec![
                FileEvent {
                    uri: Url::from_file_path(&manifest).expect("uri"),
                    typ: FileChangeType::CHANGED,
                },
                FileEvent {
                    uri: Url::from_file_path(&bt).expect("uri"),
                    typ: FileChangeType::CHANGED,
                },
            ],
        })
        .await;

    let probe = Utf8PathBuf::from_path_buf(root.join("dnu.bt")).expect("utf8");
    let diags = {
        let mut svc = backend.service.lock_recover();
        svc.update_file(probe.clone(), "\"hello\" frobnicate".to_string());
        svc.diagnostics(&probe)
    };
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Dnu) && d.severity == Severity::Error),
        "the manifest change is applied even though the batch task failed: {diags:?}"
    );
    let _ = fs::remove_dir_all(&root);
}

/// BT-5188: a directory renamed into place is walked the way the preload
/// walks: `node_modules`, `_build`, `.git` and `target` are skipped.
#[tokio::test]
async fn created_directory_walk_skips_ignored_directories() {
    let root = unique_temp_dir("bt-5188-create-skip");
    fs::create_dir_all(root.join("src")).expect("src");
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend.workspace_roots.lock_recover().push(root.clone());

    let moved = root.join("src").join("moved");
    let kept = moved.join("one.bt");
    let mut skipped = Vec::new();
    fs::create_dir_all(&moved).expect("moved");
    fs::write(&kept, "Object subclass: One\n").expect("one");
    for dir in ["node_modules", "_build", ".git", "target"] {
        let d = moved.join(dir);
        fs::create_dir_all(&d).expect("skipped dir");
        let f = d.join("x.bt");
        fs::write(&f, "Object subclass: Skipped\n").expect("x");
        skipped.push(f);
    }
    backend
        .did_change_watched_files(DidChangeWatchedFilesParams {
            changes: vec![FileEvent {
                uri: Url::from_file_path(&moved).expect("uri"),
                typ: FileChangeType::CREATED,
            }],
        })
        .await;
    assert!(
        is_indexed(backend, &kept),
        "the moved source file is indexed"
    );
    for f in &skipped {
        assert!(!is_indexed(backend, f), "{} must be skipped", f.display());
    }
    let _ = fs::remove_dir_all(&root);
}

fn replace_text(
    uri: &Url,
    version: i32,
    text: &str,
) -> tower_lsp::lsp_types::DidChangeTextDocumentParams {
    tower_lsp::lsp_types::DidChangeTextDocumentParams {
        text_document: tower_lsp::lsp_types::VersionedTextDocumentIdentifier {
            uri: uri.clone(),
            version,
        },
        content_changes: vec![tower_lsp::lsp_types::TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            text: text.to_string(),
        }],
    }
}

/// BT-5188: a mid-typing parse error whose recovery drops a method does not
/// re-check every other open file at each pause. Clearing it re-checks them
/// once (they may have been checked against the recovered interface), and
/// edits after that are back to normal.
#[tokio::test]
async fn parse_error_edit_does_not_republish_other_files() {
    let root = unique_temp_dir("bt-5188-parse-error");
    fs::create_dir_all(root.join("src")).expect("src");
    let a = root.join("src").join("a.bt");
    let b = root.join("src").join("b.bt");
    let a_src = "Object subclass: Foo\n  go => 1\n  stop => 2\n";
    let b_src = "Object subclass: User\n\n  make => Foo new go\n";
    fs::write(&a, a_src).expect("a");
    fs::write(&b, b_src).expect("b");
    let (a_uri, b_uri) = (
        Url::from_file_path(&a).expect("uri"),
        Url::from_file_path(&b).expect("uri"),
    );
    let (service, mut feed) = service_with_feed(&root).await;
    let backend: &Backend = service.inner();
    real_did_open(backend, a_uri.clone(), a_src).await;
    real_did_open(backend, b_uri.clone(), b_src).await;
    let _ = collect(&mut feed, Duration::from_secs(1)).await;

    // Half-typed: the parser recovers by dropping a method. Two pauses on
    // broken code.
    for (version, broken) in [
        (2, "Object subclass: Foo\n  go => (1 +\n  stop => 2\n"),
        (3, "Object subclass: Foo\n  go => (1 + 2\n  stop => 2\n"),
    ] {
        backend
            .did_change(replace_text(&a_uri, version, broken))
            .await;
        assert!(
            backend
                .service
                .lock_recover()
                .has_parse_errors(&Utf8PathBuf::from_path_buf(a.clone()).expect("utf8")),
            "the edit must really be a parse error"
        );
        let after_broken = collect(&mut feed, Duration::from_secs(1)).await;
        assert_eq!(
            publishes_for(&after_broken, &b_uri),
            0,
            "a parse-error edit must not re-check other open files"
        );
    }

    // Fixed back to the original interface: one re-check of b.bt.
    backend.did_change(replace_text(&a_uri, 4, a_src)).await;
    let after_fix = collect(&mut feed, Duration::from_secs(1)).await;
    assert_eq!(
        publishes_for(&after_fix, &b_uri),
        1,
        "clearing an error whose recovery changed the interface re-checks b.bt once"
    );

    // A body-only edit after that is back to no cross-file work.
    backend
        .did_change(replace_text(
            &a_uri,
            5,
            "Object subclass: Foo\n  go => 7\n  stop => 2\n",
        ))
        .await;
    let after_body = collect(&mut feed, Duration::from_secs(1)).await;
    assert_eq!(publishes_for(&after_body, &b_uri), 0);

    // A real interface change still re-checks.
    backend
        .did_change(replace_text(&a_uri, 6, "Object subclass: Foo\n  go => 1\n"))
        .await;
    let after_change = collect(&mut feed, Duration::from_secs(1)).await;
    assert!(
        publishes_for(&after_change, &b_uri) > 0,
        "removing a method still re-checks b.bt"
    );
}

/// BT-5188: an edit to a file with no recorded baseline (its open lost a
/// race) records one instead of re-checking every other open file.
#[tokio::test]
async fn edit_without_a_baseline_does_not_republish_other_files() {
    let root = unique_temp_dir("bt-5188-no-baseline");
    fs::create_dir_all(root.join("src")).expect("src");
    let a = root.join("src").join("a.bt");
    let b = root.join("src").join("b.bt");
    let a_src = "Object subclass: Foo\n  go => 1\n";
    let b_src = "Object subclass: User\n\n  make => Foo new go\n";
    fs::write(&a, b_src).expect("a");
    let (a_uri, b_uri) = (
        Url::from_file_path(&a).expect("uri"),
        Url::from_file_path(&b).expect("uri"),
    );
    let (service, mut feed) = service_with_feed(&root).await;
    let backend: &Backend = service.inner();
    real_did_open(backend, b_uri.clone(), b_src).await;
    // Indexed and open, but no baseline, as when a change beats the open.
    open_test_file(
        backend,
        &Utf8PathBuf::from_path_buf(a.clone()).expect("utf8"),
        a_src,
    );
    let _ = collect(&mut feed, Duration::from_secs(1)).await;
    backend
        .did_change(replace_text(&a_uri, 2, "Object subclass: Foo\n  go => 2\n"))
        .await;
    let after = collect(&mut feed, Duration::from_secs(1)).await;
    assert_eq!(publishes_for(&after, &b_uri), 0);
}

/// BT-5188: opening N files that each define a new class (a client restoring
/// its tabs) costs O(N) analyses, not O(N²).
#[tokio::test]
async fn opening_many_files_republishes_linearly() {
    const N: usize = 8;
    let root = unique_temp_dir("bt-5188-many-opens");
    let scratch = root.join("scratch");
    fs::create_dir_all(&scratch).expect("scratch");
    let (service, mut feed) = service_with_feed(&root).await;
    let backend: &Backend = service.inner();
    for i in 0..N {
        let path = scratch.join(format!("c{i}.bt"));
        let src = format!("Object subclass: C{i}\n  go => 1\n");
        fs::write(&path, &src).expect("write");
        real_did_open(backend, Url::from_file_path(&path).expect("uri"), &src).await;
    }
    let messages = collect(&mut feed, Duration::from_secs(1)).await;
    let total = messages.iter().filter(|n| published(n).is_some()).count();
    assert!(
        total <= 2 * N,
        "opening {N} files published {total} times; expected at most {}",
        2 * N
    );
}
