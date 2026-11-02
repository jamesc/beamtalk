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
    fs::write(
        root.join("beamtalk.toml"),
        format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n"),
    )
    .expect("write manifest");
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
