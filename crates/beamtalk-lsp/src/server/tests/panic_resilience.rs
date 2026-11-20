// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! A panic inside one request must not poison the server for later ones (BT-5078).

use super::*;

const SRC: &str = "Object subclass: Foo\n  bar => 1\nx bar\n";

async fn references_ok(backend: &Backend, uri: &Url) -> usize {
    backend
        .references(references_params(uri.clone(), 2, 2, true))
        .await
        .expect("request after a panic must succeed")
        .expect("references found")
        .len()
}

/// A handler that panics while holding the service lock poisons it. The next
/// request must still be answered correctly, not panic on the poisoned lock.
#[tokio::test]
async fn request_after_panic_under_service_lock_still_succeeds() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(unique_temp_dir("bt-5078-poison").with_extension("bt"))
        .expect("temp path is UTF-8");
    let uri = open_test_file(backend, &path, SRC);
    let before = references_ok(backend, &uri).await;

    // Inject a panic inside an analysis call, under the service lock, run
    // through the same guard every `LanguageServer` method uses.
    let result: tower_lsp::jsonrpc::Result<()> = guard_request("test/inject", async {
        let _svc = backend.service.lock_recover();
        panic!("injected analysis panic");
    })
    .await;
    let err = result.expect_err("caught panic surfaces as an LSP error, not a wrong answer");
    assert_eq!(err.code, tower_lsp::jsonrpc::ErrorCode::InternalError);
    assert!(
        err.message.contains("injected analysis panic"),
        "message should carry the panic text: {}",
        err.message
    );
    assert!(
        backend.service.is_poisoned(),
        "the injected panic must really have poisoned the service lock"
    );

    assert_eq!(references_ok(backend, &uri).await, before);
}

/// A panic in the middle of a real handler (not just a synthetic future) is
/// contained per request: the failing call is an error, the next one works.
#[tokio::test]
async fn guarded_request_error_does_not_affect_next_request() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(unique_temp_dir("bt-5078-next").with_extension("bt"))
        .expect("temp path is UTF-8");
    let uri = open_test_file(backend, &path, SRC);

    let ok: tower_lsp::jsonrpc::Result<u8> = guard_request("test/ok", async { Ok(7) }).await;
    assert_eq!(ok.expect("non-panicking handler is untouched"), 7);

    let bad: tower_lsp::jsonrpc::Result<u8> =
        guard_request("test/bad", async { panic!("boom") }).await;
    assert!(bad.is_err());

    assert!(references_ok(backend, &uri).await > 0);
}

/// Notifications have no reply to carry an error; a panic is swallowed and
/// logged, and the server keeps serving.
#[tokio::test]
async fn panicking_notification_is_contained() {
    guard_notification("test/notify", async { panic!("notify boom") }).await;
}
