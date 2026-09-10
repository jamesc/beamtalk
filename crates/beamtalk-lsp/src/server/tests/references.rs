// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Declaration-merge in textDocument/references, exercised through the cold-file (AST-fallback) path since the runtime path needs a live `beamtalk_workspace` WebSocket the test harness doesn't spin up -- covers the includeDeclaration round-trip, polymorphic call sites, and type-alias find-references (declaration/annotation sites, and the preload-completeness warning).

use super::*;

// -----------------------------------------------------------------
// Declaration-merge in `textDocument/references`.
//
// These tests exercise the cold-file (AST-fallback) path because the
// runtime path requires a live `beamtalk_workspace` WebSocket and a
// populated `beamtalk_xref` table — which `delegate_nav_query`'s own test
// fixture also avoids. The acceptance criterion
// we cover here is the `includeDeclaration` round-trip semantics: with
// the flag on, the result must include method-definition headers; with
// the flag off, declarations must be filtered out and only call sites
// remain. The runtime path inherits the same merge logic by construction
// (`declaration_sites_for_query` always falls back to the AST helpers
// when `delegate_to_runtime` is off, which is the default in tests).
// -----------------------------------------------------------------

#[tokio::test]
async fn references_with_declaration_merges_method_definition_and_call_sites() {
    // Cold-file path: cursor on a selector at a call site. With
    // `includeDeclaration = true` the result must include the method
    // definition header **and** every call site (one of each here).
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-with-decl").with_extension("bt"))
            .expect("temp path is UTF-8");
    let source = "Object subclass: Foo\n  bar => 1\nx bar\n";
    let uri = open_test_file(backend, &path, source);

    // Cursor on the `bar` call at line 2 (0-based), col 2.
    let result = backend
        .references(references_params(uri.clone(), 2, 2, true))
        .await
        .expect("rpc ok")
        .expect("some locations");

    // Expect at least 2 sites: the method-definition header on line 1
    // and the call on line 2.
    assert!(
        result.len() >= 2,
        "expected def + call (>= 2 sites), got {result:?}"
    );
    let lines: HashSet<u32> = result.iter().map(|l| l.range.start.line).collect();
    assert!(
        lines.contains(&1),
        "expected definition line 1, got {lines:?}"
    );
    assert!(
        lines.contains(&2),
        "expected call site line 2, got {lines:?}"
    );
}

#[tokio::test]
async fn references_without_declaration_strips_method_definition() {
    // Same setup as above, but `includeDeclaration = false`: the
    // method-definition header must be filtered out and only the call
    // site should remain.
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-no-decl").with_extension("bt"))
        .expect("temp path is UTF-8");
    let source = "Object subclass: Foo\n  bar => 1\nx bar\n";
    let uri = open_test_file(backend, &path, source);

    let result = backend
        .references(references_params(uri.clone(), 2, 2, false))
        .await
        .expect("rpc ok")
        .expect("some locations");

    let lines: HashSet<u32> = result.iter().map(|l| l.range.start.line).collect();
    assert!(
        !lines.contains(&1),
        "method-definition header (line 1) must be stripped when \
             includeDeclaration = false, got lines {lines:?}"
    );
    assert!(
        lines.contains(&2),
        "call site line 2 must remain, got {lines:?}"
    );
}

#[tokio::test]
async fn references_with_declaration_includes_polymorphic_definitions() {
    // Two classes both define `ping`. From a call site, the merge
    // path must surface both definition headers (declaration-merge for
    // polymorphic selectors).
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path_foo = Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-foo").with_extension("bt"))
        .expect("temp path is UTF-8");
    let path_bar = Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-bar").with_extension("bt"))
        .expect("temp path is UTF-8");
    let path_call =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-call").with_extension("bt"))
            .expect("temp path is UTF-8");
    open_test_file(backend, &path_foo, "Object subclass: Foo\n  ping => 1\n");
    open_test_file(backend, &path_bar, "Object subclass: Bar\n  ping => 2\n");
    let uri_call = open_test_file(backend, &path_call, "x ping\n");

    let result = backend
        .references(references_params(uri_call, 0, 2, true))
        .await
        .expect("rpc ok")
        .expect("some locations");

    // Expect: Foo definition + Bar definition + the call site.
    let file_uris: HashSet<String> = result.iter().map(|l| l.uri.to_string()).collect();
    let foo_uri = Url::from_file_path(path_foo.as_std_path())
        .unwrap()
        .to_string();
    let bar_uri = Url::from_file_path(path_bar.as_std_path())
        .unwrap()
        .to_string();
    assert!(
        file_uris.contains(&foo_uri),
        "expected Foo definition, got {file_uris:?}"
    );
    assert!(
        file_uris.contains(&bar_uri),
        "expected Bar definition, got {file_uris:?}"
    );
}

// ---- ADR 0108 Phase 8: type alias find-references ----

/// Shared fixture: a `type RestartStrategy = ...` declaration on line 0,
/// used as a parameter type annotation on line 3.
const ALIAS_REFERENCES_SOURCE: &str = "type RestartStrategy = #temporary | #transient | #permanent\n\nObject subclass: Supervisor\n  restart: policy :: RestartStrategy =>\n    policy\n";

#[tokio::test]
async fn references_on_type_alias_enumerates_declaration_and_annotation_site() {
    // Cursor on the alias name at its own declaration (line 0, inside
    // "RestartStrategy"). Find-references must enumerate both the
    // declaration site and the parameter-annotation use site on line 3
    // (ADR 0108: "find-references enumerates annotation sites").
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2901-alias-refs").with_extension("bt"))
            .expect("temp path is UTF-8");
    let uri = open_test_file(backend, &path, ALIAS_REFERENCES_SOURCE);

    let result = backend
        .references(references_params(uri, 0, 10, true))
        .await
        .expect("rpc ok")
        .expect("some locations");

    let lines: HashSet<u32> = result.iter().map(|l| l.range.start.line).collect();
    assert!(
        lines.contains(&0),
        "expected the alias declaration site (line 0), got {lines:?}"
    );
    assert!(
        lines.contains(&3),
        "expected the parameter-annotation use site (line 3), got {lines:?}"
    );
}

#[tokio::test]
async fn references_on_type_alias_excludes_declaration_when_not_requested() {
    // Same cursor, `includeDeclaration = false`: only the annotation use
    // site should remain.
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(
        unique_temp_dir("bt-2901-alias-refs-no-decl").with_extension("bt"),
    )
    .expect("temp path is UTF-8");
    let uri = open_test_file(backend, &path, ALIAS_REFERENCES_SOURCE);

    let result = backend
        .references(references_params(uri, 0, 10, false))
        .await
        .expect("rpc ok")
        .expect("some locations");

    let lines: HashSet<u32> = result.iter().map(|l| l.range.start.line).collect();
    assert!(
        !lines.contains(&0),
        "declaration site (line 0) must be excluded when includeDeclaration = false, \
             got {lines:?}"
    );
    assert!(
        lines.contains(&3),
        "annotation use site (line 3) must remain, got {lines:?}"
    );
}

#[tokio::test]
async fn references_on_type_alias_warns_when_workspace_preload_incomplete() {
    // ADR 0108: find-references coverage for a type alias is scoped to
    // files compiled into the current build graph. `SimpleLanguageService`
    // defaults to `project_complete = false` (no workspace preload has
    // run in this test), so the handler must surface a
    // `window/showMessage` warning rather than let the returned list
    // silently read as exhaustive.
    use futures_util::StreamExt;

    let (service, mut socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(
        unique_temp_dir("bt-2901-alias-incomplete").with_extension("bt"),
    )
    .expect("temp path is UTF-8");
    let uri = open_test_file(backend, &path, ALIAS_REFERENCES_SOURCE);

    backend
        .references(references_params(uri, 0, 10, true))
        .await
        .expect("rpc ok");

    let notification = socket
        .next()
        .await
        .expect("expected a window/showMessage notification");
    assert_eq!(notification.method(), "window/showMessage");
}

#[tokio::test]
async fn references_on_type_alias_is_silent_when_workspace_preload_complete() {
    // Same fixture, but with `project_complete = true` (as the LSP
    // server sets after a full-coverage workspace preload) —
    // find-references coverage is now known-exhaustive, so no warning
    // should fire.
    use futures_util::{FutureExt, StreamExt};

    let (service, mut socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2901-alias-complete").with_extension("bt"))
            .expect("temp path is UTF-8");
    let uri = open_test_file(backend, &path, ALIAS_REFERENCES_SOURCE);
    {
        let mut svc = backend.service.lock().expect("service lock");
        svc.set_project_complete(true);
    }

    backend
        .references(references_params(uri, 0, 10, true))
        .await
        .expect("rpc ok");

    // No notification should have been queued — polling once must not
    // yield a `window/showMessage`. `now_or_never` avoids blocking
    // forever waiting on a notification that (correctly) never comes.
    let pending = socket.next().now_or_never().flatten();
    assert!(
        pending.is_none(),
        "expected no notification when the project is complete, got {pending:?}"
    );
}

#[tokio::test]
async fn references_warns_on_unresolved_type_reference_when_declaring_file_unindexed() {
    // `Foo` is referenced in parameter-annotation position
    // (`policy :: Foo`) but no file declaring `type Foo = ...` (or
    // `Object subclass: Foo`) has ever been opened/indexed in this test.
    // `alias_name_at` alone returns `None` here (it only recognizes
    // already-registered aliases), so the incompleteness
    // warning cannot depend on it alone — even though the cursor
    // position proves `Foo` can only be a class/protocol/alias
    // reference, and the project isn't known-complete. The handler must
    // still surface the `window/showMessage` warning via
    // `unresolved_type_reference_at`.
    use futures_util::StreamExt;

    const UNRESOLVED_TYPE_REF_SOURCE: &str =
        "Object subclass: Supervisor\n  restart: policy :: Foo => policy\n";

    let (service, mut socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(
        unique_temp_dir("bt-2919-unresolved-type-ref").with_extension("bt"),
    )
    .expect("temp path is UTF-8");
    let uri = open_test_file(backend, &path, UNRESOLVED_TYPE_REF_SOURCE);

    // "  restart: policy :: Foo => policy" — `Foo` occupies columns 21-23.
    backend
        .references(references_params(uri, 1, 22, true))
        .await
        .expect("rpc ok");

    let notification = socket
        .next()
        .await
        .expect("expected a window/showMessage notification");
    assert_eq!(notification.method(), "window/showMessage");
}

#[test]
fn merge_locations_dedupes_overlapping_runtime_and_decl_sites() {
    // The runtime path may legitimately return a site that the
    // declaration-overlay path would also produce (e.g. a method that
    // is its own only call site, or a future change that has
    // `senders_of/1` include definition rows). Verify the merge
    // helper does not duplicate them.
    let uri = Url::parse("file:///foo.bt").unwrap();
    let r = Range {
        start: tower_lsp::lsp_types::Position::new(1, 2),
        end: tower_lsp::lsp_types::Position::new(1, 5),
    };
    let mut base = vec![tower_lsp::lsp_types::Location {
        uri: uri.clone(),
        range: r,
    }];
    let extras = vec![tower_lsp::lsp_types::Location {
        uri: uri.clone(),
        range: r,
    }];
    merge_locations(&mut base, extras);
    assert_eq!(base.len(), 1, "duplicate site must collapse, got {base:?}");
}

#[test]
fn merge_locations_appends_disjoint_decl_sites() {
    let uri = Url::parse("file:///foo.bt").unwrap();
    let r1 = Range {
        start: tower_lsp::lsp_types::Position::new(1, 0),
        end: tower_lsp::lsp_types::Position::new(1, 3),
    };
    let r2 = Range {
        start: tower_lsp::lsp_types::Position::new(7, 0),
        end: tower_lsp::lsp_types::Position::new(7, 3),
    };
    let mut base = vec![tower_lsp::lsp_types::Location {
        uri: uri.clone(),
        range: r1,
    }];
    let extras = vec![tower_lsp::lsp_types::Location {
        uri: uri.clone(),
        range: r2,
    }];
    merge_locations(&mut base, extras);
    assert_eq!(base.len(), 2);
    assert!(base.iter().any(|l| l.range == r1));
    assert!(base.iter().any(|l| l.range == r2));
}
