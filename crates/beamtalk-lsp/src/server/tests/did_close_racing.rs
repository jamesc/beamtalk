// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `did_close` tests: reverting to on-disk content vs. keeping preloaded/indexed state, including `did_close` racing a reopen or a `did_change`.

use super::*;

/// Startup preload indexes every `src/`/`test/` file, so the
/// `ProjectIndex` is workspace-wide rather than "currently open files".
/// `did_close` used to evict the closed file regardless — correct back
/// when files were only ever indexed while open, but after preload it
/// silently dropped the file's classes from the merged hierarchy, so
/// every other file referencing them reported a false `Unresolved
/// class` until the next LSP restart (and restarting only helped until
/// the file was opened and closed again). Closing must revert the file
/// to its on-disk content instead.
#[tokio::test]
async fn did_close_keeps_preloaded_file_indexed_from_disk() {
    let temp = unique_temp_dir("beamtalk_lsp_close_keeps_preloaded");
    let project_root = temp.join("project");
    let workflow_dir = project_root.join("src").join("workflow");
    fs::create_dir_all(&workflow_dir).expect("create workflow dir");
    fs::write(
        project_root.join("beamtalk.toml"),
        "[package]\nname = \"exdura\"\nversion = \"0.1.0\"\n",
    )
    .expect("write beamtalk.toml");

    let execution_path = workflow_dir.join("workflow_execution.bt");
    let execution_source = "Object subclass: WorkflowExecution";
    fs::write(&execution_path, execution_source).expect("write workflow_execution.bt");

    let engine_path = workflow_dir.join("workflow_engine.bt");
    let engine_source =
        "Object subclass: WorkflowEngine\n\n  makeExecution => WorkflowExecution new\n";
    fs::write(&engine_path, engine_source).expect("write workflow_engine.bt");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    // Direct call (not through the `LspService` tower layer) so
    // `workspace_roots` is recorded for `did_close` while the client
    // stays uninitialized — its `publish_diagnostics` then no-ops
    // instead of blocking on the unread capacity-1 socket.
    backend
        .initialize(InitializeParams {
            workspace_folders: Some(vec![tower_lsp::lsp_types::WorkspaceFolder {
                uri: Url::from_directory_path(&project_root).expect("root uri"),
                name: "project".to_string(),
            }]),
            ..InitializeParams::default()
        })
        .await
        .expect("initialize ok");
    backend
        .preload_workspace_source_files(PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![],
        })
        .await;

    let execution_uri = Url::from_file_path(&execution_path).expect("path -> uri");
    let engine_uri = Url::from_file_path(&engine_path).expect("path -> uri");
    real_did_open(backend, execution_uri.clone(), execution_source).await;
    real_did_open(backend, engine_uri, engine_source).await;

    // The user looks at workflow_execution.bt, then closes the tab.
    backend
        .did_close(DidCloseTextDocumentParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri: execution_uri },
        })
        .await;

    let engine_utf8 = Utf8PathBuf::from_path_buf(engine_path).expect("temp path is UTF-8");
    let svc = backend.service.lock().expect("service lock poisoned");
    assert!(
        svc.project_index()
            .hierarchy()
            .has_class("WorkflowExecution"),
        "closing a preloaded file's tab must not evict its classes from \
             the project index"
    );
    let diags = svc.diagnostics(&engine_utf8);
    assert!(
        !diags.iter().any(|d| d.message.contains("Unresolved class")),
        "workflow_engine.bt must still resolve WorkflowExecution after \
             workflow_execution.bt's tab is closed, got {diags:?}"
    );
    drop(svc);

    let _ = fs::remove_dir_all(&temp);
}

/// Counterpart to `did_close_keeps_preloaded_file_indexed_from_disk`:
/// a file preload never covers (here, one at the workspace root rather
/// than under `src/`/`test/`) was only ever indexed because it was
/// open, so closing it still removes it — otherwise a stray scratch
/// file's classes would linger in the project index and could shadow
/// real ones.
#[tokio::test]
async fn did_close_removes_file_outside_preload_coverage() {
    let temp = unique_temp_dir("beamtalk_lsp_close_removes_uncovered");
    let project_root = temp.join("project");
    fs::create_dir_all(project_root.join("src")).expect("create src dir");
    let scratch_path = project_root.join("scratch.bt");
    let scratch_source = "Object subclass: Scratch";
    fs::write(&scratch_path, scratch_source).expect("write scratch.bt");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .initialize(InitializeParams {
            workspace_folders: Some(vec![tower_lsp::lsp_types::WorkspaceFolder {
                uri: Url::from_directory_path(&project_root).expect("root uri"),
                name: "project".to_string(),
            }]),
            ..InitializeParams::default()
        })
        .await
        .expect("initialize ok");

    let scratch_uri = Url::from_file_path(&scratch_path).expect("path -> uri");
    real_did_open(backend, scratch_uri.clone(), scratch_source).await;
    {
        let svc = backend.service.lock().expect("service lock poisoned");
        assert!(svc.project_index().hierarchy().has_class("Scratch"));
    }

    backend
        .did_close(DidCloseTextDocumentParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri: scratch_uri },
        })
        .await;

    let svc = backend.service.lock().expect("service lock poisoned");
    assert!(
        !svc.project_index().hierarchy().has_class("Scratch"),
        "a file outside preload coverage must still be dropped on close"
    );
    drop(svc);

    let _ = fs::remove_dir_all(&temp);
}

/// Regression for the review Blocker on `did_close_keeps_preloaded_file_indexed_from_disk`'s
/// fix: `did_close`'s disk read is not instantaneous, so a `did_open` for the
/// same path racing in and completing *during* that read must not have its
/// fresher content clobbered by the close's now-stale disk snapshot once the
/// read finally returns.
///
/// Forces the interleaving deterministically (rather than hoping a timing
/// race reproduces) by backing `target.bt` with a FIFO: `did_close`'s
/// `fs::read_to_string` blocks on it until this test explicitly writes and
/// closes the write end, giving a fixed point at which to run the
/// concurrent `did_open` and observe its effect *before* letting the close
/// proceed.
///
/// Unix-only: relies on a POSIX FIFO actually blocking `fs::read_to_string`
/// until written to. On Windows, `mkfifo` (via Git's bundled coreutils)
/// creates the path but native `ReadFile` does not block on it the way a
/// POSIX pipe does, so the read returns immediately and the deterministic
/// interleaving this test depends on never happens.
#[cfg(unix)]
#[tokio::test]
async fn did_close_racing_reopen_does_not_clobber_reopened_content() {
    let temp = unique_temp_dir("beamtalk_lsp_close_reopen_race");
    let project_root = temp.join("project");
    let src_dir = project_root.join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");

    let target_path = src_dir.join("target.bt");
    let mkfifo_status = std::process::Command::new("mkfifo")
        .arg(&target_path)
        .status()
        .expect("run mkfifo");
    assert!(
        mkfifo_status.success(),
        "mkfifo must succeed on this platform"
    );

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .initialize(InitializeParams {
            workspace_folders: Some(vec![tower_lsp::lsp_types::WorkspaceFolder {
                uri: Url::from_directory_path(&project_root).expect("root uri"),
                name: "project".to_string(),
            }]),
            ..InitializeParams::default()
        })
        .await
        .expect("initialize ok");

    let target_uri = Url::from_file_path(&target_path).expect("path -> uri");
    // Open with version 1 — did_close will snapshot this version before
    // blocking on the FIFO read below.
    real_did_open(backend, target_uri.clone(), "Object subclass: Original").await;

    std::thread::scope(|scope| {
        let close_handle = scope.spawn(|| {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("build runtime")
                .block_on(backend.did_close(DidCloseTextDocumentParams {
                    text_document: tower_lsp::lsp_types::TextDocumentIdentifier {
                        uri: target_uri.clone(),
                    },
                }));
        });

        // Give did_close a moment to reach the (blocking) FIFO read before
        // the reopen below — best-effort; the FIFO itself is what makes
        // the interleaving deterministic (the reopen below completing does
        // not depend on this sleep, only on happening before the write
        // further down unblocks the read).
        std::thread::sleep(std::time::Duration::from_millis(50));

        // The reopen: new content, new version, racing did_close's blocked
        // read. Run on a real, separate OS thread (not just a joined
        // future) so this genuinely executes concurrently with did_close's
        // blocking read rather than only after it, matching the real
        // multi-threaded tokio runtime `#[tokio::main]` uses.
        let reopen_handle = scope.spawn(|| {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("build runtime")
                .block_on(backend.did_open(DidOpenTextDocumentParams {
                    text_document: tower_lsp::lsp_types::TextDocumentItem {
                        uri: target_uri.clone(),
                        language_id: "beamtalk".to_string(),
                        version: 2,
                        text: "Object subclass: Reopened".to_string(),
                    },
                }));
        });
        reopen_handle.join().expect("reopen thread panicked");

        // Reopen has now fully applied (versions=2, svc has `Reopened`).
        // Unblock did_close's read: write something to the FIFO — its
        // content must never reach `svc`, since the reopen above is
        // strictly newer.
        fs::write(&target_path, "Object subclass: StaleFromDisk")
            .expect("write unblocks the FIFO reader");

        close_handle.join().expect("close thread panicked");
    });

    let engine_utf8 = Utf8PathBuf::from_path_buf(target_path.clone()).expect("utf8 path");
    let svc = backend.service.lock().expect("service lock poisoned");
    assert!(
        svc.project_index().hierarchy().has_class("Reopened"),
        "the racing reopen's content must survive"
    );
    assert!(
        !svc.project_index().hierarchy().has_class("StaleFromDisk"),
        "did_close's stale disk read must not clobber the racing reopen"
    );
    assert!(
        !svc.project_index().hierarchy().has_class("Original"),
        "the pre-race content must be gone — the reopen replaced it"
    );
    drop(svc);

    let versions = backend.versions.lock().expect("versions lock poisoned");
    assert_eq!(
        versions.get(&engine_utf8).copied(),
        Some(2),
        "did_close must not remove the version entry a racing reopen installed"
    );
    drop(versions);

    let _ = fs::remove_dir_all(&temp);
}

/// Regression for the review Blocker on the `open_generation` design
/// itself: a `did_change` racing the same close (not a reopen) must
/// *not* make `did_close` back off. `did_change` only bumps `versions`,
/// never `open_generation`, so `did_close`'s reopen check stays
/// unaffected and it still completes the disk revert — proving the fix
/// doesn't just trade one permanent-leak race (reopen) for another
/// (any edit racing a close).
///
/// Same FIFO technique as `did_close_racing_reopen_does_not_clobber_reopened_content`
/// for deterministic interleaving — Unix-only for the same reason (see
/// that test's doc comment).
#[cfg(unix)]
#[tokio::test]
async fn did_close_racing_did_change_still_reverts_to_disk() {
    let temp = unique_temp_dir("beamtalk_lsp_close_change_race");
    let project_root = temp.join("project");
    let src_dir = project_root.join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");

    let target_path = src_dir.join("target.bt");
    let mkfifo_status = std::process::Command::new("mkfifo")
        .arg(&target_path)
        .status()
        .expect("run mkfifo");
    assert!(
        mkfifo_status.success(),
        "mkfifo must succeed on this platform"
    );

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .initialize(InitializeParams {
            workspace_folders: Some(vec![tower_lsp::lsp_types::WorkspaceFolder {
                uri: Url::from_directory_path(&project_root).expect("root uri"),
                name: "project".to_string(),
            }]),
            ..InitializeParams::default()
        })
        .await
        .expect("initialize ok");

    let target_uri = Url::from_file_path(&target_path).expect("path -> uri");
    real_did_open(backend, target_uri.clone(), "Object subclass: Original").await;

    std::thread::scope(|scope| {
        let close_handle = scope.spawn(|| {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("build runtime")
                .block_on(backend.did_close(DidCloseTextDocumentParams {
                    text_document: tower_lsp::lsp_types::TextDocumentIdentifier {
                        uri: target_uri.clone(),
                    },
                }));
        });

        std::thread::sleep(std::time::Duration::from_millis(50));

        // A plain edit — not a reopen — racing the close in flight (e.g.
        // a final keystroke, or a format-on-save, right before the tab
        // closes). Must not be mistaken for a reopen.
        let change_handle = scope.spawn(|| {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("build runtime")
                .block_on(backend.did_change(DidChangeTextDocumentParams {
                    text_document: tower_lsp::lsp_types::VersionedTextDocumentIdentifier {
                        uri: target_uri.clone(),
                        version: 2,
                    },
                    content_changes: vec![tower_lsp::lsp_types::TextDocumentContentChangeEvent {
                        range: None,
                        range_length: None,
                        text: "Object subclass: Edited".to_string(),
                    }],
                }));
        });
        change_handle.join().expect("change thread panicked");

        // Unblock did_close's read now that the (moot) edit has fully
        // applied — the disk content below must still win, since we are
        // genuinely closing.
        fs::write(&target_path, "Object subclass: FromDisk")
            .expect("write unblocks the FIFO reader");

        close_handle.join().expect("close thread panicked");
    });

    let target_utf8 = Utf8PathBuf::from_path_buf(target_path.clone()).expect("utf8 path");
    let svc = backend.service.lock().expect("service lock poisoned");
    assert!(
        svc.project_index().hierarchy().has_class("FromDisk"),
        "a racing did_change (not a reopen) must not stop did_close from \
             reverting to on-disk content"
    );
    assert!(
        !svc.project_index().hierarchy().has_class("Edited"),
        "the moot edit must not survive the close"
    );
    drop(svc);

    let versions = backend.versions.lock().expect("versions lock poisoned");
    assert!(
        !versions.contains_key(&target_utf8),
        "a genuinely closing document's version entry must still be removed, \
             even though a racing did_change bumped it"
    );
    drop(versions);

    let _ = fs::remove_dir_all(&temp);
}
