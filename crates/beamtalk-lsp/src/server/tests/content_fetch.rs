// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `load_type_cache` and `fetch_content` tests: live type-cache extraction on a cold cache, and the stdlib content-fetch error/success paths (unsupported scheme, malformed/authority-bearing/query-bearing URIs, ambiguous and registered filenames).

use super::*;

/// BT-2859: `load_type_cache` extracts live from OTP `.beam` files for a
/// workspace root with no prior `beamtalk build` — analogous to
/// `beamtalk-cli`'s `lint_extracts_type_specs_live_on_cold_cache_bt_2851`
/// and `beamtalk-mcp`'s `build_native_type_registry_extracts_live_on_cold_cache`
/// (BT-2858). Before this fix, the LSP only read `_build/type_cache/`
/// JSON files directly, so a workspace opened before any build got an
/// empty registry for the rest of the session.
#[tokio::test]
async fn load_type_cache_extracts_live_on_cold_cache() {
    let temp = tempfile::TempDir::new().unwrap();
    let root = temp.path().to_path_buf();
    // No `_build/` directory exists yet — the cold-cache case.
    assert!(!root.join("_build").exists());

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend.load_type_cache(std::slice::from_ref(&root)).await;

    let svc = backend.service.lock().expect("service lock poisoned");
    let Some(registry) = svc.native_types() else {
        eprintln!(
            "skipping load_type_cache_extracts_live_on_cold_cache: \
                 no OTP .beam files discovered in this environment"
        );
        return;
    };
    assert!(
        registry.lookup("lists", "reverse", 1).is_some(),
        "live extraction with no prior build must still find lists:reverse/1"
    );
    // The extractor writes the same cache a `beamtalk build`/`beamtalk
    // lint` run would.
    assert!(root.join("_build").join("type_cache").exists());
}

#[tokio::test]
async fn fetch_content_returns_error_for_unsupported_scheme() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
        .fetch_content(FetchContentParams {
            uri: "file:///some/file.bt".to_string(),
        })
        .await;
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("unsupported URI scheme"));
}

#[tokio::test]
async fn fetch_content_returns_error_for_missing_stdlib_file() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
        .fetch_content(FetchContentParams {
            uri: "beamtalk-stdlib:///NonExistent.bt".to_string(),
        })
        .await;
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("stdlib source not available"));
    assert!(err.message.contains("NonExistent.bt"));
}

#[tokio::test]
async fn fetch_content_rejects_malformed_stdlib_uri_path() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    for bad_uri in &[
        "beamtalk-stdlib:///",
        "beamtalk-stdlib:///sub/integer.bt",
        "beamtalk-stdlib:///Integer.erl",
    ] {
        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: bad_uri.to_string(),
            })
            .await;
        assert!(result.is_err(), "expected error for {bad_uri}");
        let err = result.unwrap_err();
        assert!(
            err.message.contains("invalid stdlib URI path"),
            "expected 'invalid stdlib URI path' in error for {bad_uri}, got: {}",
            err.message
        );
    }
}

#[tokio::test]
async fn fetch_content_rejects_authority_bearing_stdlib_uri() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
        .fetch_content(FetchContentParams {
            uri: "beamtalk-stdlib://host/integer.bt".to_string(),
        })
        .await;
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.message.contains("invalid stdlib URI"),
        "expected 'invalid stdlib URI' but got: {}",
        err.message
    );
}

#[tokio::test]
async fn fetch_content_rejects_stdlib_uri_with_query_or_fragment() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    for bad_uri in &[
        "beamtalk-stdlib:///integer.bt?x=1",
        "beamtalk-stdlib:///integer.bt#section",
    ] {
        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: bad_uri.to_string(),
            })
            .await;
        assert!(result.is_err(), "expected error for {bad_uri}");
        let err = result.unwrap_err();
        assert!(
            err.message.contains("invalid stdlib URI"),
            "expected 'invalid stdlib URI' for {bad_uri}, got: {}",
            err.message
        );
    }
}

#[tokio::test]
async fn fetch_content_returns_error_for_ambiguous_stdlib_filename() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let path1 = Utf8PathBuf::from("/fake/stdlib/a/integer.bt");
    let path2 = Utf8PathBuf::from("/fake/stdlib/b/integer.bt");
    let content = "Object subclass: Integer".to_string();

    {
        let mut stdlib_paths = backend
            .stdlib_paths
            .lock()
            .expect("stdlib_paths lock poisoned");
        stdlib_paths.insert(path1.clone());
        stdlib_paths.insert(path2.clone());
    }
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(path1, content.clone());
        svc.update_file(path2, content);
    }

    let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
        .fetch_content(FetchContentParams {
            uri: "beamtalk-stdlib:///integer.bt".to_string(),
        })
        .await;
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.message.contains("ambiguous"),
        "expected 'ambiguous' but got: {}",
        err.message
    );
}

#[tokio::test]
async fn fetch_content_returns_content_for_registered_stdlib_file() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let stdlib_path = Utf8PathBuf::from("/fake/stdlib/integer.bt");
    let content = "Object subclass: Integer\n  + other => 0".to_string();

    {
        let mut stdlib_paths = backend
            .stdlib_paths
            .lock()
            .expect("stdlib_paths lock poisoned");
        stdlib_paths.insert(stdlib_path.clone());
    }
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(stdlib_path, content.clone());
    }

    let result: FetchContentResult = backend
        .fetch_content(FetchContentParams {
            uri: "beamtalk-stdlib:///integer.bt".to_string(),
        })
        .await
        .expect("should return content");
    assert_eq!(result.content, content);
}
