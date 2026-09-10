// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Sibling-root package stamps and didOpen-vs-preload race tests: distinct alias package stamps per root, and `did_open` racing `load_root_packages` / preload for both stdlib stamps and sibling-class resolution.

use super::*;

/// Two sibling workspace roots, each a genuinely different real
/// package (per its own `beamtalk.toml` `[package] name`), each
/// declaring an `internal type Foo = ...` with a different expansion.
/// Without root-derived stamps, every same-project file in every root
/// would share the same fixed `$project` marker, so root B's file would
/// resolve root A's `internal` alias instead of it being excluded.
/// Confirms the two roots' `Foo` aliases carry distinct, root-derived
/// package stamps.
#[tokio::test]
async fn load_root_packages_gives_sibling_roots_distinct_alias_package_stamps() {
    let temp = unique_temp_dir("beamtalk_lsp_root_packages_multi_root");
    let root_a = temp.join("a");
    let root_b = temp.join("b");
    fs::create_dir_all(&root_a).expect("create root a");
    fs::create_dir_all(&root_b).expect("create root b");
    fs::write(
        root_a.join("beamtalk.toml"),
        "[package]\nname = \"pkg_a\"\nversion = \"0.1.0\"\n",
    )
    .expect("write root a manifest");
    fs::write(
        root_b.join("beamtalk.toml"),
        "[package]\nname = \"pkg_b\"\nversion = \"0.1.0\"\n",
    )
    .expect("write root b manifest");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .load_root_packages(&[root_a.clone(), root_b.clone()])
        .await;

    let file_a = Utf8PathBuf::from_path_buf(root_a.join("Foo.bt")).unwrap();
    let file_b = Utf8PathBuf::from_path_buf(root_b.join("Foo.bt")).unwrap();
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(file_a.clone(), "internal type Foo = Integer".to_string());
        svc.update_file(file_b.clone(), "internal type Foo = String".to_string());
    }

    let svc = backend.service.lock().expect("service lock poisoned");
    assert_eq!(
        svc.project_index().alias_package_for_file(&file_a),
        EcoString::from("pkg_a"),
        "root a's file must be stamped with root a's real package name"
    );
    assert_eq!(
        svc.project_index().alias_package_for_file(&file_b),
        EcoString::from("pkg_b"),
        "root b's file must be stamped with root b's real package name, \
             not root a's or the shared same-project marker"
    );

    let _ = fs::remove_dir_all(&temp);
}

/// A `didOpen` racing `initialized()`'s startup sequence indexes
/// its file (via `svc.update_file`, exactly what the `did_open` handler
/// does) *before* `load_root_packages` has registered any root — the
/// file's aliases get the `$project` fallback stamp. `set_root_packages`
/// must re-stamp that file when it runs, so the race resolves to the
/// same correct stamp as the happy-path ordering.
#[tokio::test]
async fn did_open_racing_load_root_packages_still_gets_real_package_stamp() {
    let temp = unique_temp_dir("beamtalk_lsp_root_packages_race");
    let root = temp.join("a");
    fs::create_dir_all(&root).expect("create root");
    fs::write(
        root.join("beamtalk.toml"),
        "[package]\nname = \"pkg_a\"\nversion = \"0.1.0\"\n",
    )
    .expect("write manifest");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    // Simulated didOpen arrives first (race): indexed with no roots known.
    let file = Utf8PathBuf::from_path_buf(root.join("Foo.bt")).unwrap();
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(file.clone(), "internal type Foo = Integer".to_string());
    }

    // Startup sequence completes afterwards.
    backend
        .load_root_packages(std::slice::from_ref(&root))
        .await;

    let svc = backend.service.lock().expect("service lock poisoned");
    assert_eq!(
        svc.project_index().alias_package_for_file(&file),
        EcoString::from("pkg_a"),
    );
    let stamped = svc
        .project_index()
        .cross_file_alias_infos_for(&Utf8PathBuf::from("elsewhere.bt"));
    assert_eq!(
        stamped.iter().find(|i| i.name == "Foo").unwrap().package,
        Some(EcoString::from("pkg_a")),
        "a didOpen racing load_root_packages must be re-stamped with the \
             root's real package name once startup completes, not keep the \
             fallback marker until the next edit"
    );
    drop(svc);

    let _ = fs::remove_dir_all(&temp);
}

/// The stdlib counterpart of the race above — a `didOpen` for a
/// stdlib file indexed *before* `preload_workspace_source_files` marks
/// stdlib membership must still end up stamped with the stdlib package
/// marker after preload runs.
#[tokio::test]
async fn did_open_racing_preload_still_gets_stdlib_stamp() {
    let temp = unique_temp_dir("beamtalk_lsp_stdlib_race");
    let project_root = temp.join("project");
    let stdlib_dir = temp.join("stdlib");
    fs::create_dir_all(project_root.join("src")).expect("create src dir");
    fs::create_dir_all(&stdlib_dir).expect("create stdlib dir");
    let stdlib_file_path = stdlib_dir.join("Direction.bt");
    fs::write(
        &stdlib_file_path,
        "internal type Direction = #north | #south",
    )
    .expect("write stdlib file");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    // Simulated didOpen arrives first (race): the stdlib file is indexed
    // before anything marked it stdlib, so it stamps as same-project.
    let stdlib_file = Utf8PathBuf::from_path_buf(stdlib_file_path).unwrap();
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(
            stdlib_file.clone(),
            "internal type Direction = #north | #south".to_string(),
        );
    }

    // Startup preload completes afterwards.
    backend
        .preload_workspace_source_files(PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![stdlib_dir],
        })
        .await;

    let svc = backend.service.lock().expect("service lock poisoned");
    assert!(svc.project_index().is_stdlib_file(&stdlib_file));
    let stamped = svc
        .project_index()
        .cross_file_alias_infos_for(&Utf8PathBuf::from("elsewhere.bt"));
    assert_eq!(
        stamped
            .iter()
            .find(|i| i.name == "Direction")
            .unwrap()
            .package,
        Some(EcoString::from(
            beamtalk_language_service::STDLIB_PACKAGE_MARKER,
        )),
        "a didOpen racing preload must be re-stamped with the stdlib \
             package marker once preload marks the file, not keep the \
             same-project marker until the next edit"
    );
    drop(svc);

    let _ = fs::remove_dir_all(&temp);
}

/// Rules out a *state* bug as the cause of the false
/// `Unresolved class` warning fixed below. `svc.diagnostics()` always
/// recomputes fresh against the current `ProjectIndex`, so once
/// `preload_workspace_source_files` has indexed a sibling class, every
/// subsequent diagnosis sees it — regardless of whether the referencing
/// file was indexed (via a racing `didOpen`) before or after that
/// preload ran. The actual bug is a *notification send-order* race, not a
/// stale `ProjectIndex`/`pre_loaded_classes` snapshot — see
/// `did_open_during_preload_defers_to_republish_for_sibling_class` for
/// the test that actually exercises the fix.
#[tokio::test]
async fn did_open_racing_preload_resolves_sibling_class() {
    let temp = unique_temp_dir("beamtalk_lsp_sibling_class_race");
    let project_root = temp.join("project");
    let workflow_dir = project_root.join("src").join("workflow");
    fs::create_dir_all(&workflow_dir).expect("create workflow dir");

    let signal_path = workflow_dir.join("signal.bt");
    fs::write(&signal_path, "Object subclass: Signal").expect("write signal.bt");

    let workflow_context_path = workflow_dir.join("workflow_context.bt");
    let workflow_context_source =
        "Object subclass: WorkflowContext\n\n  makeSignal => Signal new\n";
    fs::write(&workflow_context_path, workflow_context_source).expect("write workflow_context.bt");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();

    // Simulated didOpen arrives first (race): workflow_context.bt is
    // indexed before preload has populated Signal anywhere in the
    // ProjectIndex.
    let workflow_context_utf8 =
        Utf8PathBuf::from_path_buf(workflow_context_path).expect("temp path is UTF-8");
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.update_file(
            workflow_context_utf8.clone(),
            workflow_context_source.to_string(),
        );
    }

    // Startup preload completes afterwards, populating signal.bt.
    backend
        .preload_workspace_source_files(PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![],
        })
        .await;

    let svc = backend.service.lock().expect("service lock poisoned");
    let diags = svc.diagnostics(&workflow_context_utf8);
    assert!(
        !diags.iter().any(|d| d.message.contains("Unresolved class")),
        "a didOpen racing preload must not leave a same-directory \
             sibling class unresolved once preload has indexed it, got {diags:?}"
    );
    drop(svc);

    let _ = fs::remove_dir_all(&temp);
}

/// End-to-end counterpart of
/// `did_open_racing_preload_resolves_sibling_class` — drives the real
/// `did_open` handler (not direct `ProjectIndex` state checks) through a
/// real `LspService`/socket pair, with `preload_in_progress` set exactly
/// as `initialized()` sets it before preload starts, and inspects every
/// `textDocument/publishDiagnostics` notification actually sent for the
/// opened file.
///
/// `did_open` must skip its own diagnostics send outright while preload is
/// in-flight, so `republish_open_diagnostics` is the *only* publish for
/// this URI. Without that guard, `did_open` would publish immediately, and
/// a `didOpen` racing preload would send a stale `Unresolved class`
/// notification that `republish_open_diagnostics` would have to race to
/// overwrite — a real send-order race between two concurrent tasks on
/// `tower-lsp`'s capacity-1 notification channel, not guaranteed to
/// resolve in the correct notification's favor (unlike the *strictly
/// sequential* "`did_open` fully completes, then preload starts" case,
/// which self-heals correctly regardless — the race this test guards is
/// the concurrent one, only reproducible by controlling the flag
/// directly).
#[tokio::test]
async fn did_open_during_preload_defers_to_republish_for_sibling_class() {
    use futures_util::StreamExt;

    let temp = unique_temp_dir("beamtalk_lsp_sibling_class_notification_race");
    let project_root = temp.join("project");
    let workflow_dir = project_root.join("src").join("workflow");
    fs::create_dir_all(&workflow_dir).expect("create workflow dir");
    fs::write(
        project_root.join("beamtalk.toml"),
        "[package]\nname = \"exdura\"\nversion = \"0.1.0\"\n",
    )
    .expect("write beamtalk.toml");

    fs::write(workflow_dir.join("signal.bt"), "Object subclass: Signal").expect("write signal.bt");

    let workflow_context_path = workflow_dir.join("workflow_context.bt");
    let workflow_context_source =
        "Object subclass: WorkflowContext\n\n  makeSignal => Signal new\n";
    fs::write(&workflow_context_path, workflow_context_source).expect("write workflow_context.bt");
    let workflow_context_uri = Url::from_file_path(&workflow_context_path).expect("path → uri");

    // The actual trigger: `check_unresolved_classes` only runs at
    // all when `pre_loaded_classes` is non-empty (an open-world
    // assumption — with *zero* other files known, any class reference
    // might legitimately live in one not yet indexed). A single file
    // racing `did_open` before preload sees a completely empty
    // `ProjectIndex` and skips the check entirely, so it alone can't
    // reproduce the false positive. A second, unrelated file already
    // open (as a restored editor tab commonly is) makes
    // `pre_loaded_classes` non-empty for `workflow_context.bt`'s own
    // diagnosis while `Signal` specifically still isn't indexed yet.
    let other_path = project_root.join("src").join("other.bt");
    fs::write(&other_path, "Object subclass: Other").expect("write other.bt");
    let other_uri = Url::from_file_path(&other_path).expect("path → uri");

    let (mut service, mut socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &project_root).await;
    let backend: &Backend = service.inner();

    // `ClientSocket`'s channel has capacity 1 (`tower-lsp`'s
    // `Client::new`), so a second send before anyone reads the first
    // would block forever. Drain it concurrently on a spawned task
    // (forwarding onto an unbounded channel) rather than reading only
    // after every `did_open`/preload/republish call below returns.
    let (forward_tx, mut forward_rx) = tokio::sync::mpsc::unbounded_channel();
    let reader = tokio::spawn(async move {
        while let Some(notification) = socket.next().await {
            if forward_tx.send(notification).is_err() {
                break;
            }
        }
    });

    // Mirrors the flag flip `initialized()` performs before starting
    // preload — see that method's doc for why this must be
    // set before any racing didOpen's own `publish_diagnostics` check.
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.set_preload_in_progress(true);
    }

    // Both didOpens arrive while preload is (per the flag above) still
    // in flight — both must index the file (so `versions`/`ProjectIndex`
    // still reflect them) but skip their own `publish_diagnostics` send.
    // `other.bt` opens first, exactly like a second restored editor tab
    // would.
    real_did_open(backend, other_uri.clone(), "Object subclass: Other").await;
    real_did_open(
        backend,
        workflow_context_uri.clone(),
        workflow_context_source,
    )
    .await;

    // Mirrors the diagnostics-relevant subset of `initialized()`'s
    // sequence (`load_root_packages` → `preload_workspace_source_files`
    // → `republish_open_diagnostics`), skipping `load_type_cache` /
    // `load_diagnostics_table` (unrelated to class resolution) and
    // `resolve_otp_lib_dir` / `register_type_hierarchy_capability`
    // (client *requests* that would hang forever here — nothing is
    // polling the socket to answer them, unlike the notifications this
    // test reads back below).
    backend
        .load_root_packages(std::slice::from_ref(&project_root))
        .await;
    backend
        .preload_workspace_source_files(PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![],
        })
        .await;
    // Mirrors `initialized()`'s own flip: clear the flag *before*
    // republishing, so republish's `publish_diagnostics` calls send.
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        svc.set_preload_in_progress(false);
    }
    backend.republish_open_diagnostics().await;

    // Exactly two `publishDiagnostics` notifications total are expected:
    // one per tracked open path (`other.bt`, `workflow_context.bt`), both
    // from `republish_open_diagnostics` — neither `did_open` sent
    // anything, since preload was in-flight both times.
    let diagnostics_by_uri = recv_two_publish_diagnostics(&mut forward_rx).await;
    reader.abort();

    assert_eq!(
        diagnostics_by_uri.keys().collect::<HashSet<_>>(),
        HashSet::from([&other_uri, &workflow_context_uri]),
        "expected exactly one publishDiagnostics notification per open \
             file (both from republish_open_diagnostics, since did_open \
             skipped its own send while preload was in-flight)"
    );
    let healed = &diagnostics_by_uri[&workflow_context_uri];
    assert!(
        !healed
            .iter()
            .any(|d| d.message.contains("Unresolved class")),
        "the sole notification the client receives for a didOpen-during-\
             preload file must not carry an Unresolved class warning against \
             a sibling class preload has since indexed, got {healed:?}"
    );

    let _ = fs::remove_dir_all(&temp);
}
