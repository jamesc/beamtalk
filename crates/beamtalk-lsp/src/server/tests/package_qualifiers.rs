// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `pkg@Class` qualifier validation in LSP diagnostics (ADR 0070 Phase 2,
//! BT-4928): the package set comes from each workspace root's manifest.

use super::*;

const QUALIFIED_REF_SOURCE: &str = "Object subclass: Foo\n  make => xml@Parser new\n";

fn unknown_package_messages(backend: &Backend, file: &Utf8PathBuf) -> Vec<String> {
    let svc = backend.service.lock().expect("service lock poisoned");
    svc.diagnostics(file)
        .into_iter()
        .map(|d| d.message.to_string())
        .filter(|m| m.contains("Unknown package"))
        .collect()
}

/// Loads `root` as a workspace root, opens `src/Foo.bt` with an `xml@`
/// reference and returns the "Unknown package" messages.
async fn diagnose_qualified_ref(root: &Path) -> Vec<String> {
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend.load_root_packages(&[root.to_path_buf()]).await;
    let file = Utf8PathBuf::from_path_buf(root.join("src").join("Foo.bt")).unwrap();
    backend
        .service
        .lock()
        .expect("service lock poisoned")
        .update_file(file.clone(), QUALIFIED_REF_SOURCE.to_string());
    unknown_package_messages(backend, &file)
}

#[tokio::test]
async fn undeclared_qualifier_is_reported_from_the_root_manifest() {
    let temp = unique_temp_dir("beamtalk_lsp_pkg_qualifier_undeclared");
    write_manifest_with_git_deps(&temp, "app", &["json"]);

    let messages = diagnose_qualified_ref(&temp).await;
    assert_eq!(messages.len(), 1, "got: {messages:?}");
    assert!(
        messages[0].contains("Unknown package 'xml' in qualified reference"),
        "got: {messages:?}"
    );
    let _ = fs::remove_dir_all(&temp);
}

#[tokio::test]
async fn declared_qualifier_is_not_reported() {
    let temp = unique_temp_dir("beamtalk_lsp_pkg_qualifier_declared");
    write_manifest_with_git_deps(&temp, "app", &["xml"]);

    assert!(diagnose_qualified_ref(&temp).await.is_empty());
    let _ = fs::remove_dir_all(&temp);
}

#[tokio::test]
async fn missing_manifest_gives_no_false_error() {
    let temp = unique_temp_dir("beamtalk_lsp_pkg_qualifier_no_manifest");
    fs::create_dir_all(&temp).expect("create root");

    assert!(diagnose_qualified_ref(&temp).await.is_empty());
    let _ = fs::remove_dir_all(&temp);
}

#[tokio::test]
async fn unreadable_manifest_gives_no_false_error() {
    let temp = unique_temp_dir("beamtalk_lsp_pkg_qualifier_bad_manifest");
    fs::create_dir_all(&temp).expect("create root");
    fs::write(temp.join("beamtalk.toml"), "[package\nname = ").expect("write bad manifest");

    assert!(diagnose_qualified_ref(&temp).await.is_empty());
    let _ = fs::remove_dir_all(&temp);
}
