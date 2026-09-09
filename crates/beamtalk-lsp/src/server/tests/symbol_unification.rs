// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2244: workspace/document symbol unification. Covers the pure conversion helpers (`runtime_class_to_document_symbol`, `runtime_class_to_workspace_symbol`, `zero_width_range_for_line`), reload-induced-diagnostic conversion (ADR 0105 Phase 1, BT-2779), and the AST-fallback behaviour of the `document_symbol` / `folding_range` / symbol handlers -- the runtime dispatch itself is exercised end-to-end by the surface-drift and Erlang `EUnit` tests.

use super::*;

// -----------------------------------------------------------------
// BT-2244: workspace/document symbol unification.
//
// The runtime path (`nav-symbols` over WebSocket) needs a live
// workspace, which the LSP test harness doesn't spin up. These
// tests target the pure conversion helpers
// (`runtime_class_to_document_symbol`,
// `runtime_class_to_workspace_symbol`, `zero_width_range_for_line`)
// and the AST-fallback behaviour of the `document_symbol` and
// `symbol` handlers — together they pin the parts of the dispatch
// contract that don't require runtime attachment. (The runtime
// dispatch itself is exercised end-to-end by the surface-drift +
// Erlang EUnit tests in `beamtalk_repl_ops_nav_symbols_tests.erl`.)
// -----------------------------------------------------------------

use beamtalk_language_service::{NavSymbolClass as NSClass, NavSymbolMethod as NSMethod};

#[test]
fn zero_width_range_for_line_clamps_zero_to_row_zero() {
    // Defensive: the runtime should never emit line 0, but if it
    // does we render the symbol at row 0 rather than panicking on
    // a `0 - 1` underflow.
    let r = zero_width_range_for_line(0);
    assert_eq!(r.start, Position::new(0, 0));
    assert_eq!(r.end, Position::new(0, 0));
}

#[test]
fn zero_width_range_for_line_converts_one_based_runtime_lines() {
    let r = zero_width_range_for_line(7);
    assert_eq!(r.start, Position::new(6, 0));
    assert_eq!(r.end, Position::new(6, 0));
}

// -----------------------------------------------------------------
// ADR 0105 Phase 1 (BT-2779): reload-induced diagnostics
// -----------------------------------------------------------------

fn sample_reload_finding() -> crate::runtime::ReloadFinding {
    crate::runtime::ReloadFinding {
        owner: "Dashboard".to_string(),
        changed_class: "Counter".to_string(),
        selector: "getCount".to_string(),
        classification: "signature_change".to_string(),
        severity: "warning".to_string(),
        category: Some("Dnu".to_string()),
        message: "String does not understand '+'".to_string(),
        note: None,
        sites: vec![crate::runtime::ReloadSite {
            method: "refresh".to_string(),
            line: 14,
        }],
        start: 0,
        end: 5,
    }
}

#[test]
fn reload_finding_to_lsp_diagnostics_one_per_site() {
    let mut finding = sample_reload_finding();
    finding.sites.push(crate::runtime::ReloadSite {
        method: "render".to_string(),
        line: 20,
    });
    let diags = reload_finding_to_lsp_diagnostics(&finding);
    assert_eq!(diags.len(), 2);
    assert_eq!(diags[0].range.start, Position::new(13, 0));
    assert_eq!(diags[0].range.end, Position::new(13, u32::MAX));
    assert_eq!(diags[1].range.start, Position::new(19, 0));
    assert!(diags[0].message.contains("getCount"));
    assert!(diags[0].message.contains("refresh"));
    assert!(diags[1].message.contains("render"));
}

#[test]
fn reload_finding_to_lsp_diagnostics_maps_severity() {
    let mut finding = sample_reload_finding();
    finding.severity = "hint".to_string();
    let diags = reload_finding_to_lsp_diagnostics(&finding);
    assert_eq!(diags[0].severity, Some(DiagnosticSeverity::HINT));

    finding.severity = "warning".to_string();
    let diags = reload_finding_to_lsp_diagnostics(&finding);
    assert_eq!(diags[0].severity, Some(DiagnosticSeverity::WARNING));
}

#[test]
fn reload_finding_to_lsp_diagnostics_appends_note() {
    let mut finding = sample_reload_finding();
    finding.classification = "removal".to_string();
    finding.note = Some("removed by the reload of Counter".to_string());
    let diags = reload_finding_to_lsp_diagnostics(&finding);
    assert!(
        diags[0]
            .message
            .contains("removed by the reload of Counter")
    );
}

#[test]
fn reload_finding_to_lsp_diagnostics_carries_category_as_code() {
    let finding = sample_reload_finding();
    let diags = reload_finding_to_lsp_diagnostics(&finding);
    assert_eq!(
        diags[0].code,
        Some(tower_lsp::lsp_types::NumberOrString::String(
            "Dnu".to_string()
        ))
    );
}

#[test]
fn group_findings_by_origin_splits_same_owner_different_changed_class() {
    // BT-2801: `seed_reload_diagnostics` must seed independently-clearing
    // entries — two findings attributed to the same owner but from
    // *different* reloaded classes are two distinct origins, not one
    // merged bucket, exactly mirroring `reload_check_listener`'s
    // per-origin bucketing (`ReloadDiagnosticsByUriAndOrigin`'s doc).
    let mut from_counter = sample_reload_finding();
    from_counter.owner = "Dashboard".to_string();
    from_counter.changed_class = "Counter".to_string();
    let mut from_widget = sample_reload_finding();
    from_widget.owner = "Dashboard".to_string();
    from_widget.changed_class = "Widget".to_string();

    let by_origin = group_findings_by_origin(vec![from_counter.clone(), from_widget.clone()]);

    assert_eq!(by_origin.len(), 2);
    assert_eq!(
        by_origin[&("Dashboard".to_string(), "Counter".to_string())],
        vec![from_counter]
    );
    assert_eq!(
        by_origin[&("Dashboard".to_string(), "Widget".to_string())],
        vec![from_widget]
    );
}

#[test]
fn group_findings_by_origin_merges_same_owner_and_changed_class() {
    // Two findings sharing the exact same origin (e.g. two removed
    // selectors on the same reloaded class) must land in one bucket, so
    // `seed_reload_diagnostics` seeds one origin entry covering both —
    // matching `reload_check_listener`'s per-event grouping, which never
    // splits a single `(owner, changed_class)` origin across entries.
    let mut first = sample_reload_finding();
    first.selector = "getCount".to_string();
    let mut second = sample_reload_finding();
    second.selector = "reset".to_string();

    let by_origin = group_findings_by_origin(vec![first.clone(), second.clone()]);

    assert_eq!(by_origin.len(), 1);
    let bucket = &by_origin[&("Dashboard".to_string(), "Counter".to_string())];
    assert_eq!(bucket.len(), 2);
    assert_eq!(bucket[0].selector, "getCount");
    assert_eq!(bucket[1].selector, "reset");
}

#[test]
fn group_findings_by_origin_empty_input_returns_empty_map() {
    assert!(group_findings_by_origin(vec![]).is_empty());
}

#[test]
fn resolve_class_uri_resolves_absolute_source_file() {
    let temp = unique_temp_dir("bt-2779-resolve-class-uri");
    fs::create_dir_all(&temp).expect("create temp dir");
    let file_path = temp.join("dashboard.bt");
    fs::write(&file_path, "").expect("write file");

    let class = NSClass::new(
        "Dashboard",
        Some(file_path.to_str().expect("utf8").to_string()),
        Some(1),
        vec![],
    );
    let uri = resolve_class_uri(&class, &[]).expect("resolved uri");
    assert_eq!(uri.scheme(), "file");
    assert!(uri.path().ends_with("dashboard.bt"));

    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn resolve_class_uri_returns_none_without_source_file() {
    let class = NSClass::new("Dashboard", None, None, vec![]);
    assert!(resolve_class_uri(&class, &[]).is_none());
}

#[test]
fn runtime_class_to_document_symbol_filters_classes_in_other_files() {
    // A `nav-symbols` reply lists every loaded class. The per-file
    // `documentSymbol` handler must drop classes that belong to a
    // different `source_file` than the requested URI.
    let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-doc-sym-filter");
    std::fs::create_dir_all(&tmp).expect("create temp dir");
    let a_path = tmp.join("a.bt");
    let b_path = tmp.join("b.bt");
    std::fs::write(&a_path, "").expect("write a.bt");
    std::fs::write(&b_path, "").expect("write b.bt");

    let class_in_b = NSClass::new(
        "B",
        Some(b_path.to_str().unwrap().to_string()),
        Some(1),
        vec![],
    );
    let requested = Utf8PathBuf::from_path_buf(a_path).expect("utf8");
    let workspace_roots = vec![tmp.clone()];
    let out = runtime_class_to_document_symbol(class_in_b, &requested, &workspace_roots);
    assert!(out.is_none(), "class from b.bt must be filtered out");
}

#[test]
fn runtime_class_to_document_symbol_drops_classes_without_source_file() {
    // Classes without a backing source file (REPL-loaded,
    // ClassBuilder) belong to `workspace/symbol`, not the per-file
    // outline.
    let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-doc-sym-no-src");
    std::fs::create_dir_all(&tmp).expect("create temp dir");
    let path = tmp.join("counter.bt");
    std::fs::write(&path, "").expect("write counter.bt");

    let class = NSClass::new("ReplOnly", None, None, vec![]);
    let requested = Utf8PathBuf::from_path_buf(path).expect("utf8");
    let out = runtime_class_to_document_symbol(class, &requested, std::slice::from_ref(&tmp));
    assert!(out.is_none(), "source-less class must be filtered out");
}

#[test]
fn runtime_class_to_document_symbol_emits_class_with_methods() {
    let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-doc-sym-emit");
    std::fs::create_dir_all(&tmp).expect("create temp dir");
    let path = tmp.join("counter.bt");
    std::fs::write(&path, "").expect("write counter.bt");
    let path_str = path.to_str().unwrap().to_string();

    let class = NSClass::new(
        "Counter",
        Some(path_str),
        Some(1),
        vec![
            NSMethod::new("increment", false, Some(7)),
            NSMethod::new("withInitial:", true, Some(3)),
        ],
    );
    let requested = Utf8PathBuf::from_path_buf(path).expect("utf8");
    let sym = runtime_class_to_document_symbol(class, &requested, std::slice::from_ref(&tmp))
        .expect("class with matching source file should produce a symbol");

    assert_eq!(sym.name, "Counter (class)");
    assert_eq!(sym.kind, SymbolKind::CLASS);
    let children = sym.children.expect("Counter has methods");
    assert_eq!(children.len(), 2);
    let increment = children.iter().find(|c| c.name == "increment").unwrap();
    assert!(increment.detail.is_none());
    assert_eq!(increment.kind, SymbolKind::METHOD);
    assert_eq!(increment.range.start, Position::new(6, 0));
    let with_initial = children.iter().find(|c| c.name == "withInitial:").unwrap();
    // BT-3442: class-side methods now share the `to_lsp_symbol` path's
    // SymbolKind::FUNCTION + "class method" detail convention, so this
    // path can no longer silently diverge from the AST-fallback one.
    assert_eq!(with_initial.kind, SymbolKind::FUNCTION);
    assert_eq!(with_initial.detail.as_deref(), Some("class method"));
    assert_eq!(with_initial.range.start, Position::new(2, 0));
}

#[test]
fn runtime_class_to_workspace_symbol_applies_query_filter() {
    let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-ws-sym-filter");
    std::fs::create_dir_all(&tmp).expect("create temp dir");
    let path = tmp.join("counter.bt");
    std::fs::write(&path, "").expect("write counter.bt");
    let path_str = path.to_str().unwrap().to_string();

    let class = NSClass::new("Counter", Some(path_str), Some(1), vec![]);

    // Empty query — everything matches.
    let root_uri = Url::from_file_path(&tmp).ok();
    let s = runtime_class_to_workspace_symbol(
        &class,
        "",
        std::slice::from_ref(&tmp),
        root_uri.as_ref(),
    )
    .expect("empty query matches");
    assert_eq!(s.name, "Counter");

    // Case-insensitive substring — `count` matches `Counter`.
    let s2 = runtime_class_to_workspace_symbol(
        &class,
        "count",
        std::slice::from_ref(&tmp),
        root_uri.as_ref(),
    )
    .expect("substring matches");
    assert_eq!(s2.name, "Counter");

    // Non-matching query.
    let s3 = runtime_class_to_workspace_symbol(
        &class,
        "zzzz",
        std::slice::from_ref(&tmp),
        root_uri.as_ref(),
    );
    assert!(s3.is_none(), "non-matching query must drop the row");
}

#[test]
fn runtime_class_to_workspace_symbol_surfaces_source_less_classes() {
    // The headline win of BT-2244: classes with no backing source
    // file (REPL-loaded, ClassBuilder) still appear in
    // `workspace/symbol`, anchored to the workspace-root URI with a
    // `(no source file)` detail string.
    let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-ws-sym-nosrc");
    std::fs::create_dir_all(&tmp).expect("create temp dir");
    let root_uri = Url::from_file_path(&tmp).expect("root → uri");

    let class = NSClass::new("MyRunner", None, None, vec![]);
    let s =
        runtime_class_to_workspace_symbol(&class, "", std::slice::from_ref(&tmp), Some(&root_uri))
            .expect("source-less class still surfaces");
    assert_eq!(s.name, "MyRunner");
    assert_eq!(s.location.range.start, Position::new(0, 0));
    // `container_name` carries the `(no source file)` marker (we
    // overload the field to render visibly distinct rows; the
    // alternative — `detail`, which isn't on `SymbolInformation`
    // — is unavailable in the LSP type).
    let container = s.container_name.as_deref();
    assert_eq!(container, Some("(no source file)"));
}

#[test]
fn class_source_is_stdlib_keeps_source_less_classes() {
    // Source-less REPL/dynamic classes are *not* stdlib — they must
    // pass the filter so the headline win still works.
    let class = NSClass::new("MyRunner", None, None, vec![]);
    let stdlib_paths: HashSet<Utf8PathBuf> = HashSet::new();
    assert!(!class_source_is_stdlib(&class, &[], &stdlib_paths));
}

#[test]
fn class_source_is_stdlib_keeps_user_classes() {
    // A class whose resolved source file is *not* in the stdlib set
    // is kept (this is the typical user-code path).
    let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-stdlib-keep-user");
    std::fs::create_dir_all(&tmp).expect("create temp dir");
    let user_path = tmp.join("user.bt");
    std::fs::write(&user_path, "").expect("write user.bt");
    let class = NSClass::new(
        "UserClass",
        Some(user_path.to_str().unwrap().to_string()),
        Some(1),
        vec![],
    );
    let stdlib_paths: HashSet<Utf8PathBuf> = HashSet::new();
    assert!(!class_source_is_stdlib(
        &class,
        std::slice::from_ref(&tmp),
        &stdlib_paths
    ));
}

#[test]
fn class_source_is_stdlib_filters_stdlib_classes() {
    // A class whose resolved source file *is* in the stdlib set is
    // filtered out — the runtime-attached `workspace/symbol`
    // consumer relies on this to keep its result set matching the
    // cold-file fallback (which only sees user files).
    let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-stdlib-filter");
    std::fs::create_dir_all(&tmp).expect("create temp dir");
    let stdlib_path = tmp.join("integer.bt");
    std::fs::write(&stdlib_path, "").expect("write integer.bt");
    let utf8 = Utf8PathBuf::from_path_buf(stdlib_path.clone()).expect("utf8");
    let stdlib_paths: HashSet<Utf8PathBuf> = std::iter::once(utf8).collect();
    let class = NSClass::new(
        "Integer",
        Some(stdlib_path.to_str().unwrap().to_string()),
        Some(1),
        vec![],
    );
    assert!(class_source_is_stdlib(
        &class,
        std::slice::from_ref(&tmp),
        &stdlib_paths
    ));
}

#[test]
fn runtime_class_to_workspace_symbol_drops_source_less_without_root() {
    // No workspace root configured → nowhere safe to anchor the
    // source-less row, so drop it rather than emit an invalid URI.
    let class = NSClass::new("Orphan", None, None, vec![]);
    let s = runtime_class_to_workspace_symbol(&class, "", &[], None);
    assert!(
        s.is_none(),
        "source-less class without a root URI must drop the row"
    );
}

#[tokio::test]
async fn document_symbol_uses_ast_for_untitled_uri() {
    // BT-2244 review fix: `document_symbol` must bypass the runtime
    // (`delegate_nav_symbols`) path whenever the URI is not a clean
    // `file://` document.  An `untitled:` buffer has no `source_file`
    // in the live class registry, so a `scope = "user"` runtime
    // query would filter every class out and return an empty
    // outline. The handler should take the AST fallback directly
    // and return the buffer's parsed symbols, even when
    // `delegateToRuntime` is on.
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    // Mimic what `did_open` does for an `untitled:` URI: the path
    // key is `__untitled__/<name>`.
    let path = Utf8PathBuf::from("__untitled__/scratch.bt");
    let source = "Object subclass: Counter\n  increment => 1\n  value => 2";
    {
        let mut svc = backend.service.lock().expect("service lock");
        svc.update_file(path.clone(), source.to_string());
    }
    {
        let mut versions = backend.versions.lock().expect("versions lock");
        versions.insert(path.clone(), 1);
    }
    // Flip the flag on so the bypass guard is the *only* reason the
    // runtime path isn't taken. (Without the bypass, a delegate
    // path with no attached runtime would still fall back to AST,
    // so the assertion below couldn't distinguish — but the flag
    // being on makes the guard observable as the early-return.)
    backend.set_delegate_to_runtime(true);

    let untitled_uri = Url::parse("untitled:scratch.bt").expect("untitled uri parses");
    let params = DocumentSymbolParams {
        text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri: untitled_uri },
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
    };
    let response = backend.document_symbol(params).await.expect("rpc ok");
    let DocumentSymbolResponse::Nested(symbols) = response.expect("Some(symbols)") else {
        panic!("expected nested response");
    };
    assert_eq!(
        symbols.len(),
        1,
        "untitled buffer should yield the AST outline"
    );
    assert_eq!(symbols[0].name, "Counter (class)");
    let children = symbols[0].children.as_ref().expect("methods present");
    let names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
    assert!(names.contains(&"increment"), "got {names:?}");
    assert!(names.contains(&"value"), "got {names:?}");
}

#[tokio::test]
async fn document_symbol_falls_back_to_ast_when_flag_off() {
    // With `delegateToRuntime` false (the default), `document_symbol`
    // returns the AST walker's outline byte-for-byte — same shape
    // the pre-BT-2244 implementation produced. This pins the
    // "no behaviour change when off" contract.
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(
        unique_temp_dir("bt-2244-doc-sym-fallback").with_extension("bt"),
    )
    .expect("utf8 path");
    let source = "Object subclass: Counter\n  increment => 1\n  value => 2";
    let uri = open_test_file(backend, &path, source);
    assert!(!backend.delegate_to_runtime());

    let params = DocumentSymbolParams {
        text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri },
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
    };
    let response = backend.document_symbol(params).await.expect("rpc ok");
    let DocumentSymbolResponse::Nested(symbols) = response.expect("Some(symbols)") else {
        panic!("expected nested response");
    };
    assert_eq!(symbols.len(), 1);
    assert_eq!(symbols[0].name, "Counter (class)");
    let children = symbols[0].children.as_ref().expect("methods present");
    let names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
    assert!(names.contains(&"increment"), "got {names:?}");
    assert!(names.contains(&"value"), "got {names:?}");
}

#[tokio::test]
async fn folding_range_returns_one_range_per_divider() {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-3237-folding-range").with_extension("bt"))
            .expect("utf8 path");
    let source = "\
Object subclass: Counter
  // === Alpha ===
  foo => 1
  bar => 2

  // === Beta ===
  baz => 3
";
    let uri = open_test_file(backend, &path, source);

    let params = FoldingRangeParams {
        text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri },
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
    };
    let response = backend.folding_range(params).await.expect("rpc ok");
    let ranges = response.expect("Some(ranges)");
    // BT-3260: divider-category ranges (Alpha, Beta), plus the
    // class-body range — `foo`/`bar`/`baz` are all single-line, so none
    // contributes a method range of its own.
    assert_eq!(ranges.len(), 3, "got {ranges:?}");
    // "Alpha" starts at the divider line (line 1, 0-based) and ends at
    // "bar => 2" (line 3).
    assert_eq!(ranges[0].start_line, 1);
    assert_eq!(ranges[0].end_line, 3);
    // "Beta" starts at its divider line (line 5) and ends at "baz => 3"
    // (line 6).
    assert_eq!(ranges[1].start_line, 5);
    assert_eq!(ranges[1].end_line, 6);
    // The class body spans the whole file: the header (line 0) through
    // "baz => 3" (line 6).
    assert_eq!(ranges[2].start_line, 0);
    assert_eq!(ranges[2].end_line, 6);
}

#[tokio::test]
async fn folding_range_still_covers_a_class_without_dividers() {
    // BT-3260: registering `folding_range_provider` at all opts every
    // `.bt` file out of VS Code's built-in indentation-based folding
    // once *any* provider is registered — so a divider-less class must
    // still get a class-body range here, or its fold arrows regress to
    // nothing. `Ok(None)` is reserved for a file with no foldable
    // content at all (see `folding_range_is_none_for_a_trivial_file`
    // below), not merely "no dividers".
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(
        unique_temp_dir("bt-3260-folding-range-no-dividers").with_extension("bt"),
    )
    .expect("utf8 path");
    let source = "Object subclass: Counter\n  foo => 1\n  bar => 2";
    let uri = open_test_file(backend, &path, source);

    let params = FoldingRangeParams {
        text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri },
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
    };
    let response = backend.folding_range(params).await.expect("rpc ok");
    let ranges = response.expect("a divider-less class still folds by class body");
    assert_eq!(ranges.len(), 1);
    assert_eq!(ranges[0].start_line, 0);
    assert_eq!(ranges[0].end_line, 2);
}

#[tokio::test]
async fn folding_range_is_none_for_a_trivial_single_line_file() {
    // A file with nothing multi-line to fold (no dividers, no class/
    // method body spanning more than one line) still returns `Ok(None)`
    // — the "this provider has nothing to say" signal `Backend::
    // folding_range`'s doc comment calls out, distinct from an empty
    // `Ok(Some(vec![]))`.
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path = Utf8PathBuf::from_path_buf(
        unique_temp_dir("bt-3260-folding-range-trivial").with_extension("bt"),
    )
    .expect("utf8 path");
    let source = "Object subclass: Empty\n";
    let uri = open_test_file(backend, &path, source);

    let params = FoldingRangeParams {
        text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri },
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
    };
    let response = backend.folding_range(params).await.expect("rpc ok");
    assert!(
        response.is_none(),
        "a single-line class has no foldable content"
    );
}

#[tokio::test]
async fn workspace_symbol_falls_back_to_ast_when_flag_off() {
    // Cold-file path matches the BT-2081 behaviour exactly — one
    // SymbolInformation per top-level class whose name matches the
    // (case-insensitive substring) query, drawn from the indexed
    // user files.
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let path =
        Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2244-ws-sym-fallback").with_extension("bt"))
            .expect("utf8 path");
    let source = "Object subclass: Counter\n  increment => 1";
    let _uri = open_test_file(backend, &path, source);
    assert!(!backend.delegate_to_runtime());

    // Empty query — matches every user class.
    let params = WorkspaceSymbolParams {
        query: String::new(),
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
    };
    let response = backend.symbol(params).await.expect("rpc ok");
    let syms = response.expect("Some(symbols)");
    let names: Vec<&str> = syms.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains(&"Counter"),
        "fallback should find Counter, got {names:?}"
    );

    // Case-insensitive substring filter.
    let params2 = WorkspaceSymbolParams {
        query: "count".to_string(),
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
    };
    let response2 = backend.symbol(params2).await.expect("rpc ok");
    let syms2 = response2.expect("Some(symbols)");
    assert_eq!(syms2.len(), 1);
    assert_eq!(syms2[0].name, "Counter");

    // Non-matching query.
    let params3 = WorkspaceSymbolParams {
        query: "zzzz".to_string(),
        work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
        partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
    };
    let response3 = backend.symbol(params3).await.expect("rpc ok");
    assert!(response3.is_none(), "non-matching query → None");
}
