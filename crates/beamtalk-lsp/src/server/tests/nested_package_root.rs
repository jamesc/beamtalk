// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-open-file package root discovery (BT-4158): an editor that opens a
//! *parent* of one or more package roots must still load each package's
//! manifest, sources and fetched dependencies when one of its files opens.

use super::super::config::package_root_for_file;
use super::*;

/// Writes a package at `root` named `name`, with one `src/` class
/// referencing a class that only exists in a fetched dependency's sources.
/// Returns the path of the `src/` file.
fn write_package_with_dep(root: &Path, name: &str, app_class: &str, dep_class: &str) -> PathBuf {
    let src = root.join("src");
    let dep_src = root.join("_build").join("deps").join("dep").join("src");
    fs::create_dir_all(&src).expect("create src dir");
    fs::create_dir_all(&dep_src).expect("create dep src dir");
    write_manifest_with_git_deps(root, name, &["dep"]);
    fs::write(
        dep_src.join(format!("{dep_class}.bt")),
        format!("Object subclass: {dep_class}"),
    )
    .expect("write dep class");
    let app_path = src.join(format!("{app_class}.bt"));
    fs::write(&app_path, app_source(app_class, dep_class)).expect("write app class");
    app_path
}

fn app_source(app_class: &str, dep_class: &str) -> String {
    format!("Object subclass: {app_class}\n\n  make => {dep_class} new\n")
}

fn unresolved_class_diagnostics(backend: &Backend, path: &Utf8PathBuf) -> Vec<String> {
    let svc = backend.service.lock().expect("service lock poisoned");
    svc.diagnostics(path)
        .into_iter()
        .map(|d| d.message.to_string())
        .filter(|m| m.contains("Unresolved class"))
        .collect()
}

#[test]
fn package_root_for_file_finds_nearest_manifest_below_workspace_root() {
    let temp = unique_temp_dir("beamtalk_lsp_package_root_for_file");
    let pkg = temp.join("pkg");
    let app = write_package_with_dep(&pkg, "pkg", "App", "Dep");

    assert_eq!(package_root_for_file(&app), Some(pkg.clone()));

    // A fetched dependency checkout with its own manifest still belongs to
    // the package that fetched it.
    let dep_checkout = pkg.join("_build").join("deps").join("dep");
    fs::write(
        dep_checkout.join("beamtalk.toml"),
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    )
    .expect("write dep manifest");
    assert_eq!(
        package_root_for_file(&dep_checkout.join("src").join("Dep.bt")),
        Some(pkg.clone()),
        "a dependency file resolves to the package that fetched it"
    );

    // A file with no `beamtalk.toml` ancestor (up to the first marker or the
    // filesystem root) is not in any package.
    let loose_dir = temp.join("loose");
    fs::create_dir_all(&loose_dir).expect("create loose dir");
    fs::create_dir_all(loose_dir.join(".git")).expect("create .git marker");
    let loose = loose_dir.join("Loose.bt");
    fs::write(&loose, "Object subclass: Loose").expect("write loose file");
    assert_eq!(
        package_root_for_file(&loose),
        None,
        "a root found only via a .git marker is not a package root"
    );

    let _ = fs::remove_dir_all(&temp);
}

/// The BT-4158 repro: the workspace folder is a parent (`~/source`) of two
/// package checkouts. Opening a file in each package must register that
/// package as a root, preload its fetched dependency sources, and load its
/// `[package]` name — so the dependency class resolves.
#[tokio::test]
async fn opening_file_under_parent_workspace_loads_its_package_dependencies() {
    use futures_util::StreamExt;

    let temp = unique_temp_dir("beamtalk_lsp_nested_package_root");
    let workspace = temp.join("source");
    let pkg_a = workspace.join("beamtalk-a");
    let pkg_b = workspace.join("beamtalk-b");
    let app_a = write_package_with_dep(&pkg_a, "pkg_a", "AppA", "ServerConfig");
    let app_b = write_package_with_dep(&pkg_b, "pkg_b", "AppB", "HTTPClient");

    let (mut service, mut socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &workspace).await;
    let backend: &Backend = service.inner();
    // `ClientSocket`'s channel has capacity 1, and each `did_open` below
    // publishes more than once (the root load's republish plus its own), so
    // drain it concurrently or the second send blocks forever.
    let drain = tokio::spawn(async move { while socket.next().await.is_some() {} });
    assert!(
        !backend.workspace_roots_snapshot().contains(&pkg_a),
        "precondition: the package root is not a workspace root at initialize"
    );

    // (The unresolved-class check only runs once the index holds some other
    // class, so without per-file discovery it is the second package's file
    // — not the first, alone in the index — that reports the false
    // `Unresolved class`.)
    for (app, app_class, dep_class) in [
        (&app_a, "AppA", "ServerConfig"),
        (&app_b, "AppB", "HTTPClient"),
    ] {
        let uri = Url::from_file_path(app).expect("path → uri");
        real_did_open(backend, uri, &app_source(app_class, dep_class)).await;
        let utf8 = Utf8PathBuf::from_path_buf(app.clone()).expect("temp path is UTF-8");
        let unresolved = unresolved_class_diagnostics(backend, &utf8);
        assert!(
            unresolved.is_empty(),
            "dependency class {dep_class} must resolve for a file opened under a \
             parent workspace folder, got {unresolved:?}"
        );
    }

    let roots = backend.workspace_roots_snapshot();
    assert!(roots.contains(&pkg_a), "pkg_a registered, got {roots:?}");
    assert!(roots.contains(&pkg_b), "pkg_b registered, got {roots:?}");

    // Each package's real `[package]` name was loaded for its files.
    {
        let mut svc = backend.service.lock().expect("service lock poisoned");
        let alias_a = Utf8PathBuf::from_path_buf(pkg_a.join("src").join("Alias.bt")).unwrap();
        let alias_b = Utf8PathBuf::from_path_buf(pkg_b.join("src").join("Alias.bt")).unwrap();
        svc.update_file(alias_a.clone(), "internal type Foo = Integer".to_string());
        svc.update_file(alias_b.clone(), "internal type Foo = String".to_string());
        assert_eq!(
            svc.project_index().alias_package_for_file(&alias_a),
            EcoString::from("pkg_a")
        );
        assert_eq!(
            svc.project_index().alias_package_for_file(&alias_b),
            EcoString::from("pkg_b")
        );
    }

    // Opening another file of an already-registered package adds nothing.
    let other = pkg_a.join("test").join("AppATest.bt");
    fs::create_dir_all(other.parent().unwrap()).expect("create test dir");
    fs::write(&other, "Object subclass: AppATest").expect("write test file");
    real_did_open(
        backend,
        Url::from_file_path(&other).expect("path → uri"),
        "Object subclass: AppATest",
    )
    .await;
    assert_eq!(backend.workspace_roots_snapshot(), roots);

    drain.abort();
    let _ = fs::remove_dir_all(&temp);
}

/// Opening the package root directly keeps today's behaviour: the root is
/// the workspace root, its files are already covered, and `did_open`
/// registers nothing new.
#[tokio::test]
async fn opening_file_under_package_root_workspace_adds_no_root() {
    let temp = unique_temp_dir("beamtalk_lsp_package_root_workspace");
    let pkg = temp.join("pkg");
    let app = write_package_with_dep(&pkg, "pkg", "App", "Dep");

    let (mut service, _socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &pkg).await;
    let backend: &Backend = service.inner();
    let roots_before = backend.workspace_roots_snapshot();
    assert_eq!(roots_before, vec![pkg.clone()]);

    real_did_open(
        backend,
        Url::from_file_path(&app).expect("path → uri"),
        &app_source("App", "Dep"),
    )
    .await;

    assert_eq!(backend.workspace_roots_snapshot(), roots_before);

    let _ = fs::remove_dir_all(&temp);
}

/// Spawns a task draining `socket` so `publishDiagnostics` sends (capacity-1
/// channel) never block a `did_open`.
fn drain_socket(mut socket: tower_lsp::ClientSocket) -> tokio::task::JoinHandle<()> {
    use futures_util::StreamExt;
    tokio::spawn(async move { while socket.next().await.is_some() {} })
}

fn utf8(path: &Path) -> Utf8PathBuf {
    Utf8PathBuf::from_path_buf(path.to_path_buf()).expect("temp path is UTF-8")
}

fn dep_class_path(pkg: &Path, dep_class: &str) -> Utf8PathBuf {
    utf8(
        &pkg.join("_build")
            .join("deps")
            .join("dep")
            .join("src")
            .join(format!("{dep_class}.bt")),
    )
}

fn is_indexed(backend: &Backend, path: &Utf8PathBuf) -> bool {
    let svc = backend.service.lock().expect("service lock poisoned");
    svc.file_source(path).is_some()
}

fn is_project_complete(backend: &Backend) -> bool {
    let svc = backend.service.lock().expect("service lock poisoned");
    svc.is_project_complete()
}

fn set_preload_budget(backend: &Backend, budget: usize) {
    *backend
        .preload_budget_remaining
        .lock()
        .expect("budget lock poisoned") = budget;
}

fn set_type_cache_outcome(backend: &Backend, outcome: Option<bool>) {
    *backend
        .type_cache_outcome
        .lock()
        .expect("type_cache_outcome lock poisoned") = outcome;
}

fn type_cache_extractions(backend: &Backend) -> Vec<PathBuf> {
    backend
        .type_cache_extractions
        .lock()
        .expect("type_cache_extractions lock poisoned")
        .clone()
}

/// BT-4383 #5: when a package root's preload walk task fails, the root is
/// unregistered again, so the next `did_open` under it retries the load
/// instead of finding a "known" root with nothing indexed.
#[tokio::test]
async fn failed_root_walk_unregisters_root_so_next_open_retries() {
    let temp = unique_temp_dir("beamtalk_lsp_failed_root_walk");
    let workspace = temp.join("source");
    let pkg = workspace.join("pkg");
    let app = write_package_with_dep(&pkg, "pkg", "App", "ServerConfig");

    let (mut service, socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &workspace).await;
    let backend: &Backend = service.inner();
    let drain = drain_socket(socket);
    let uri = Url::from_file_path(&app).expect("path → uri");
    let dep = dep_class_path(&pkg, "ServerConfig");

    backend
        .fail_next_root_walk
        .store(true, std::sync::atomic::Ordering::SeqCst);
    real_did_open(backend, uri.clone(), &app_source("App", "ServerConfig")).await;
    assert!(
        !backend.workspace_roots_snapshot().contains(&pkg),
        "a root whose walk failed must be unregistered"
    );
    assert!(
        !is_indexed(backend, &dep),
        "the failed walk indexed nothing"
    );

    real_did_open(backend, uri, &app_source("App", "ServerConfig")).await;
    assert!(
        backend.workspace_roots_snapshot().contains(&pkg),
        "the retry registers the root"
    );
    assert!(
        is_indexed(backend, &dep),
        "the retry loads the root's dependency sources"
    );

    drain.abort();
    let _ = fs::remove_dir_all(&temp);
}

/// BT-4383 #1: opening a file in the Nth new package root extracts the type
/// cache for that root only — not every known root again.
#[tokio::test]
async fn each_new_package_root_extracts_only_its_own_type_cache() {
    let temp = unique_temp_dir("beamtalk_lsp_incremental_type_cache");
    let workspace = temp.join("source");
    let app_classes = ["AppA", "AppB", "AppC"];
    let pkgs: Vec<PathBuf> = ["a", "b", "c"]
        .iter()
        .map(|n| workspace.join(format!("pkg_{n}")))
        .collect();
    let apps: Vec<PathBuf> = pkgs
        .iter()
        .zip(app_classes)
        .map(|(pkg, app)| write_package_with_dep(pkg, "pkg", app, "Dep"))
        .collect();

    let (mut service, socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &workspace).await;
    let backend: &Backend = service.inner();
    let drain = drain_socket(socket);
    // Extraction succeeds for every root, whatever OTP type data this
    // machine has, so each root counts as loaded after one extraction.
    set_type_cache_outcome(backend, Some(true));

    for (i, (app, app_class)) in apps.iter().zip(app_classes).enumerate() {
        let before = type_cache_extractions(backend).len();
        real_did_open(
            backend,
            Url::from_file_path(app).expect("path → uri"),
            &app_source(app_class, "Dep"),
        )
        .await;
        let log = type_cache_extractions(backend);
        let new_extractions = &log[before..];
        assert!(
            new_extractions.contains(&pkgs[i]),
            "opening {app_class} extracts its root, got {new_extractions:?}"
        );
        for earlier in &pkgs[..i] {
            assert!(
                !new_extractions.contains(earlier),
                "opening {app_class} must not re-extract earlier root {}, got {new_extractions:?}",
                earlier.display()
            );
        }
    }
    let log = type_cache_extractions(backend);
    let unique: HashSet<&PathBuf> = log.iter().collect();
    assert_eq!(
        unique.len(),
        log.len(),
        "every root is extracted exactly once, got {log:?}"
    );

    drain.abort();
    let _ = fs::remove_dir_all(&temp);
}

/// BT-4383: a root whose `extract_type_specs` returned `None` (e.g. opened
/// before the runtime was built) is not marked loaded, so the next root load
/// extracts it again instead of leaving its FFI types empty for the session.
#[tokio::test]
async fn root_with_no_extracted_specs_is_retried_on_next_root_load() {
    let temp = unique_temp_dir("beamtalk_lsp_type_cache_retry");
    let workspace = temp.join("source");
    let pkg_a = workspace.join("pkg_a");
    let pkg_b = workspace.join("pkg_b");
    let app_a = write_package_with_dep(&pkg_a, "pkg", "AppA", "Dep");
    let app_b = write_package_with_dep(&pkg_b, "pkg", "AppB", "Dep");

    let (mut service, socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &workspace).await;
    let backend: &Backend = service.inner();
    let drain = drain_socket(socket);

    set_type_cache_outcome(backend, Some(false));
    real_did_open(
        backend,
        Url::from_file_path(&app_a).expect("path → uri"),
        &app_source("AppA", "Dep"),
    )
    .await;
    let before = type_cache_extractions(backend).len();
    assert!(
        type_cache_extractions(backend).contains(&pkg_a),
        "opening AppA attempts pkg_a's extraction"
    );

    set_type_cache_outcome(backend, Some(true));
    real_did_open(
        backend,
        Url::from_file_path(&app_b).expect("path → uri"),
        &app_source("AppB", "Dep"),
    )
    .await;
    let log = type_cache_extractions(backend);
    let retried = &log[before..];
    assert!(
        retried.contains(&pkg_a),
        "pkg_a, which extracted nothing, is retried on the next root load, got {retried:?}"
    );
    assert!(
        retried.contains(&pkg_b),
        "pkg_b is extracted, got {retried:?}"
    );

    // Both are now loaded: a further load extracts neither again.
    let settled = type_cache_extractions(backend).len();
    backend
        .load_type_cache(&[pkg_a.clone(), pkg_b.clone()])
        .await;
    assert_eq!(
        type_cache_extractions(backend).len(),
        settled,
        "roots with extracted specs are not extracted again"
    );

    drain.abort();
    let _ = fs::remove_dir_all(&temp);
}

/// BT-4383 #2: per-root loads draw from one session-wide preload budget, so
/// the total number of preloaded files stays bounded however many package
/// roots get opened — and running out marks coverage partial.
#[tokio::test]
async fn per_root_loads_share_one_session_preload_budget() {
    let temp = unique_temp_dir("beamtalk_lsp_session_preload_budget");
    let workspace = temp.join("source");
    let app_classes = ["AppA", "AppB", "AppC"];
    let dep_classes = ["DepA", "DepB", "DepC"];
    let pkgs: Vec<PathBuf> = ["a", "b", "c"]
        .iter()
        .map(|n| workspace.join(format!("pkg_{n}")))
        .collect();
    let apps: Vec<PathBuf> = pkgs
        .iter()
        .zip(app_classes)
        .zip(dep_classes)
        .map(|((pkg, app), dep)| write_package_with_dep(pkg, "pkg", app, dep))
        .collect();

    let (mut service, socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &workspace).await;
    let backend: &Backend = service.inner();
    let drain = drain_socket(socket);
    // Each package holds two preloadable files (its `src/` class and one
    // dependency class): a budget of four covers exactly two packages.
    set_preload_budget(backend, 4);

    for ((app, app_class), dep) in apps.iter().zip(app_classes).zip(dep_classes) {
        real_did_open(
            backend,
            Url::from_file_path(app).expect("path → uri"),
            &app_source(app_class, dep),
        )
        .await;
    }

    assert!(is_indexed(backend, &dep_class_path(&pkgs[0], "DepA")));
    assert!(is_indexed(backend, &dep_class_path(&pkgs[1], "DepB")));
    assert!(
        !is_indexed(backend, &dep_class_path(&pkgs[2], "DepC")),
        "the third package's load must not get a fresh budget"
    );
    assert_eq!(backend.preload_budget_remaining(), 0);
    assert!(backend.preload_coverage_is_partial());
    assert!(!is_project_complete(backend));

    drain.abort();
    let _ = fs::remove_dir_all(&temp);
}

/// BT-4383 #3, order 1: a `did_open` root load that exhausts the budget runs
/// before the startup preload (it won `root_load_lock`); the startup preload
/// over `config.roots` alone must not reset `project_complete` to true —
/// even when the startup walk itself has budget to spare.
#[tokio::test]
async fn exhausted_root_load_before_startup_preload_keeps_project_incomplete() {
    let temp = unique_temp_dir("beamtalk_lsp_sticky_partial_root_first");
    let workspace = temp.join("source");
    let pkg = workspace.join("pkg");
    let app = write_package_with_dep(&pkg, "pkg", "App", "Dep");

    let (mut service, socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &workspace).await;
    let backend: &Backend = service.inner();
    let drain = drain_socket(socket);
    set_preload_budget(backend, 1);

    real_did_open(
        backend,
        Url::from_file_path(&app).expect("path → uri"),
        &app_source("App", "Dep"),
    )
    .await;
    assert!(
        !is_project_complete(backend),
        "the per-root load was partial"
    );

    // Give the startup walk room of its own, so only the sticky verdict —
    // not a drained budget — keeps completeness false.
    set_preload_budget(backend, 100);
    backend
        .preload_workspace_source_files(PreloadConfig {
            roots: vec![workspace.clone()],
            stdlib_dirs: vec![],
        })
        .await;
    assert!(
        !is_project_complete(backend),
        "a later startup preload must not reclaim project completeness"
    );

    drain.abort();
    let _ = fs::remove_dir_all(&temp);
}

/// BT-4383 #3, order 2: the startup preload completes first (claiming
/// completeness), then a `did_open` root load exhausts the budget; nothing
/// reclaims completeness afterwards.
#[tokio::test]
async fn exhausted_root_load_after_startup_preload_keeps_project_incomplete() {
    let temp = unique_temp_dir("beamtalk_lsp_sticky_partial_startup_first");
    let workspace = temp.join("source");
    let pkg = workspace.join("pkg");
    let app = write_package_with_dep(&pkg, "pkg", "App", "Dep");

    let (mut service, socket) = tower_lsp::LspService::new(Backend::new);
    initialize_service(&mut service, &workspace).await;
    let backend: &Backend = service.inner();
    let drain = drain_socket(socket);
    set_preload_budget(backend, 1);

    backend
        .preload_workspace_source_files(PreloadConfig {
            roots: vec![workspace.clone()],
            stdlib_dirs: vec![],
        })
        .await;
    assert!(
        is_project_complete(backend),
        "precondition: the startup walk (nothing under the parent folder) is complete"
    );

    real_did_open(
        backend,
        Url::from_file_path(&app).expect("path → uri"),
        &app_source("App", "Dep"),
    )
    .await;
    assert!(
        !is_project_complete(backend),
        "the per-root load was partial"
    );

    // The startup sequence never runs twice, but a second startup-style
    // walk with budget to spare still must not reclaim completeness.
    set_preload_budget(backend, 100);
    backend
        .preload_workspace_source_files(PreloadConfig {
            roots: vec![workspace.clone()],
            stdlib_dirs: vec![],
        })
        .await;
    assert!(!is_project_complete(backend));

    drain.abort();
    let _ = fs::remove_dir_all(&temp);
}
