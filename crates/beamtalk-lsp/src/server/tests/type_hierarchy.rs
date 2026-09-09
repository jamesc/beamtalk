// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `goto_implementation` and `prepare_type_hierarchy/supertypes/subtypes` tests, plus the initialize capability-advertisement pins for both (BT-2241).

use super::*;

/// BT-2241: `initialize` must advertise `implementation_provider` so
/// clients enable goto-implementation. Pins the capability registration
/// so a future refactor of the capabilities literal can't silently
/// drop the binding (the surface-parity drift checker would catch
/// the drop via `extract_lsp_caps`, but a dedicated unit test gives a
/// faster signal at the change site).
#[tokio::test]
async fn initialize_advertises_implementation_provider() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let result = backend
        .initialize(InitializeParams::default())
        .await
        .expect("initialize ok");
    assert!(
        matches!(
            result.capabilities.implementation_provider,
            Some(ImplementationProviderCapability::Simple(true))
        ),
        "expected implementation_provider = Simple(true), got {:?}",
        result.capabilities.implementation_provider
    );
}

/// BT-2241: AST-fallback path of `goto_implementation`. The runtime
/// flag defaults to off in tests (no `initialize` with
/// `delegateToRuntime`), so this exercises the cold-file walker via
/// `SimpleLanguageService::find_implementors`. Two classes in the same
/// in-memory file define `bar`; both method-header locations must come
/// back when the cursor is on a call site for `bar`.
#[tokio::test]
async fn goto_implementation_returns_all_implementors_via_ast_fallback() {
    use tower_lsp::lsp_types::{
        PartialResultParams, TextDocumentIdentifier, TextDocumentPositionParams,
        WorkDoneProgressParams,
    };

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    // Seed an in-memory file with two implementors of `bar` plus a call
    // site for `bar`. Use a `file://` URI so `resolve_path_for_uri`
    // returns the path key the service stores.
    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2241-goto-impl-test").with_extension("bt"))
            .expect("temp path is UTF-8");
    let source = "Object subclass: Foo\n  \
                      bar => 1\n\
                      Object subclass: Baz\n  \
                      bar => 2\n\
                      Object subclass: User\n  \
                      go => self bar\n";
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(path.clone(), source.to_string());
    }

    // Cursor on the `bar` call site (line 5, column 12 in the User
    // method body). Line/column are 0-based for LSP.
    let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
    let params = tower_lsp::lsp_types::request::GotoImplementationParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: tower_lsp::lsp_types::Position::new(5, 12),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };

    let response = backend
        .goto_implementation(params)
        .await
        .expect("goto_implementation ok");

    let locations = match response {
        Some(GotoDefinitionResponse::Array(v)) => v,
        other => panic!("expected Array response, got {other:?}"),
    };
    assert_eq!(
        locations.len(),
        2,
        "expected one location per implementor (Foo + Baz), got {locations:?}"
    );
    // Both locations should be in the seeded file (the only one
    // indexed) and both ranges should be zero-width at column 0
    // (matching the runtime path's header-line anchor, BT-2241 review).
    let expected_uri = Url::from_file_path(path.as_std_path()).expect("file URI");
    for loc in &locations {
        assert_eq!(loc.uri, expected_uri, "unexpected URI in result");
        assert_eq!(
            loc.range.start, loc.range.end,
            "AST fallback should emit zero-width ranges to match runtime path"
        );
        assert_eq!(
            loc.range.start.character, 0,
            "AST fallback should anchor at column 0 of the header line"
        );
    }
}

/// BT-2241: cursor on a non-selector token (e.g. a `state:` declaration
/// name) yields `None`. The runtime-attached mode would skip the hop
/// (the classifier returns `None`), and the AST fallback would do the
/// same — `goto_implementation` is selector-scoped by design.
#[tokio::test]
async fn goto_implementation_returns_none_for_non_selector_cursor() {
    use tower_lsp::lsp_types::{
        PartialResultParams, TextDocumentIdentifier, TextDocumentPositionParams,
        WorkDoneProgressParams,
    };

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2241-goto-impl-none").with_extension("bt"))
            .expect("temp path is UTF-8");
    // A class body with a state declaration; cursor will land on the
    // state-variable name, which is neither a selector nor a class.
    let source = "Object subclass: Foo\n  \
                      state: counter = 0\n  \
                      bar => 1\n";
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(path.clone(), source.to_string());
    }

    let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
    let params = tower_lsp::lsp_types::request::GotoImplementationParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            // Line 1 ("  state: counter = 0"), column 11 lands on the
            // `counter` identifier.
            position: tower_lsp::lsp_types::Position::new(1, 11),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };

    let response = backend
        .goto_implementation(params)
        .await
        .expect("goto_implementation ok");
    assert!(
        response.is_none(),
        "expected None for non-selector cursor, got {response:?}"
    );
}

/// BT-2242: `initialize` must advertise type-hierarchy support via the
/// `experimental` channel (lsp-types 0.94.1 does not yet expose a typed
/// `type_hierarchy_provider` field on `ServerCapabilities`).
///
/// Pins the JSON shape so editors that look for
/// `experimental.typeHierarchyProvider == true` continue to detect the
/// capability after refactors of the capabilities literal.
#[tokio::test]
async fn initialize_advertises_type_hierarchy_provider_via_experimental() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let result = backend
        .initialize(InitializeParams::default())
        .await
        .expect("initialize ok");
    let experimental = result
        .capabilities
        .experimental
        .expect("experimental field set");
    let provider = experimental
        .get("typeHierarchyProvider")
        .expect("typeHierarchyProvider key present");
    assert_eq!(
        provider,
        &serde_json::Value::Bool(true),
        "typeHierarchyProvider should be `true`, got {provider:?}"
    );
}

/// BT-2242: `prepare_type_hierarchy` on a known class name returns a
/// single item carrying the class-name span. Exercises the AST-walker
/// classifier (`type_hierarchy_prepare_at`) plus the LSP-side item
/// construction.
#[tokio::test]
async fn prepare_type_hierarchy_resolves_class_under_cursor() {
    use tower_lsp::lsp_types::{
        TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
    };

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2242-prepare-th").with_extension("bt"))
            .expect("temp path is UTF-8");
    // Two user classes — Bar's superclass reference to Foo is the
    // cursor target. Click on the `Foo` token (line 2, col 0).
    let source = "Object subclass: Foo\n\
                      \n\
                      Foo subclass: Bar\n";
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(path.clone(), source.to_string());
    }
    let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
    let params = TypeHierarchyPrepareParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            // Line 2 = "Foo subclass: Bar", column 1 lands on "Foo".
            position: tower_lsp::lsp_types::Position::new(2, 1),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    };

    let items = backend
        .prepare_type_hierarchy(params)
        .await
        .expect("prepare ok")
        .expect("Some(items) for cursor on class name");

    assert_eq!(
        items.len(),
        1,
        "expected exactly one prepared item, got {items:?}"
    );
    assert_eq!(items[0].name, "Foo");
    assert_eq!(items[0].kind, SymbolKind::CLASS);
    // The URI must be the file containing the `Foo` declaration —
    // since the seeded file declares Foo on line 0, the item points
    // back at the seeded path.
    assert_eq!(items[0].uri, uri);
}

/// BT-2242: cursor not on a class name (e.g. inside the `subclass:`
/// selector token) yields `None`. Selector tokens are owned by the
/// implementors / senders queries, not type hierarchy.
#[tokio::test]
async fn prepare_type_hierarchy_returns_none_for_non_class_cursor() {
    use tower_lsp::lsp_types::{
        TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
    };

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2242-prepare-th-none").with_extension("bt"))
            .expect("temp path is UTF-8");
    let source = "Object subclass: Foo\n  \
                      bar => 42\n";
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(path.clone(), source.to_string());
    }

    let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
    // Line 1 ("  bar => 42"), column 4 — squarely on `bar` (a selector,
    // not a class name).
    let params = TypeHierarchyPrepareParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: tower_lsp::lsp_types::Position::new(1, 4),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    };

    let response = backend
        .prepare_type_hierarchy(params)
        .await
        .expect("prepare ok");
    assert!(
        response.is_none(),
        "expected None for selector cursor, got {response:?}"
    );
}

/// BT-2242: `typeHierarchy/supertypes` returns the receiver's ancestor
/// chain, in `Behaviour superclassChain` order. Two-level chain
/// `Bar -> Foo -> Object -> ProtoObject` exercises the BFS-like
/// ordering (direct parent first).
#[tokio::test]
async fn supertypes_returns_chain_in_inheritance_order() {
    use tower_lsp::lsp_types::{PartialResultParams, WorkDoneProgressParams};

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2242-supertypes").with_extension("bt"))
            .expect("temp path is UTF-8");
    let source = "Object subclass: Foo\n\
                      Foo subclass: Bar\n";
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(path.clone(), source.to_string());
    }

    // Build a synthetic TypeHierarchyItem for `Bar` (the editor would
    // get this from `prepare_type_hierarchy`; we hand-roll it here so
    // the test focuses on the supertypes resolver).
    let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
    let zero_range = Range {
        start: tower_lsp::lsp_types::Position::new(0, 0),
        end: tower_lsp::lsp_types::Position::new(0, 0),
    };
    let item = TypeHierarchyItem {
        name: "Bar".to_string(),
        kind: SymbolKind::CLASS,
        tags: None,
        detail: None,
        uri,
        range: zero_range,
        selection_range: zero_range,
        data: None,
    };
    let params = TypeHierarchySupertypesParams {
        item,
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };

    let supers = backend
        .supertypes(params)
        .await
        .expect("supertypes ok")
        .expect("Some(items)");

    // `Bar` => Foo, Object, ProtoObject. The user file only declares
    // Foo; Object / ProtoObject come from the builtin hierarchy and
    // have no indexed declaration site, but the LSP layer still emits
    // a row for them (parent-URI fallback).
    let names: Vec<&str> = supers.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(
        names,
        vec!["Foo", "Object", "ProtoObject"],
        "expected ordered ancestor chain"
    );
}

/// BT-2242: `typeHierarchy/subtypes` returns transitive descendants
/// (BFS order — direct children before grandchildren) via
/// `ClassHierarchy::all_subclasses`. Two-level tree exercises both
/// levels and confirms the receiver itself is excluded.
#[tokio::test]
async fn subtypes_returns_transitive_descendants_in_bfs_order() {
    use tower_lsp::lsp_types::{PartialResultParams, WorkDoneProgressParams};

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    let path = Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2242-subtypes").with_extension("bt"))
        .expect("temp path is UTF-8");
    // Two-level tree: Foo -> {Bar, Baz}; Bar -> Qux.
    let source = "Object subclass: Foo\n\
                      Foo subclass: Bar\n\
                      Foo subclass: Baz\n\
                      Bar subclass: Qux\n";
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(path.clone(), source.to_string());
    }

    let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
    let zero_range = Range {
        start: tower_lsp::lsp_types::Position::new(0, 0),
        end: tower_lsp::lsp_types::Position::new(0, 0),
    };
    let item = TypeHierarchyItem {
        name: "Foo".to_string(),
        kind: SymbolKind::CLASS,
        tags: None,
        detail: None,
        uri,
        range: zero_range,
        selection_range: zero_range,
        data: None,
    };
    let params = TypeHierarchySubtypesParams {
        item,
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };

    let subs = backend
        .subtypes(params)
        .await
        .expect("subtypes ok")
        .expect("Some(items)");

    let names: Vec<&str> = subs.iter().map(|s| s.name.as_str()).collect();
    // `all_subclasses` iteration order depends on hash-map iteration
    // for siblings at the same level — assert as a set for the direct
    // children and pin Qux's position after both direct children.
    assert_eq!(names.len(), 3, "expected 3 descendants, got {names:?}");
    assert!(
        names.contains(&"Bar"),
        "Bar should be in subtypes, got {names:?}"
    );
    assert!(
        names.contains(&"Baz"),
        "Baz should be in subtypes, got {names:?}"
    );
    let qux_pos = names.iter().position(|n| *n == "Qux").expect("Qux present");
    let bar_pos = names.iter().position(|n| *n == "Bar").expect("Bar present");
    assert!(
        qux_pos > bar_pos,
        "Qux (grandchild) must come after Bar (its parent) in BFS order, got {names:?}"
    );
    assert!(
        !names.contains(&"Foo"),
        "receiver Foo must not be in its own subtypes, got {names:?}"
    );
}
