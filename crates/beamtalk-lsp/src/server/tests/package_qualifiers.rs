// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `pkg@Class` qualifier validation in LSP diagnostics (ADR 0070 Phase 2,
//! BT-4928): the package set comes from each workspace root's manifest.

use super::*;

const QUALIFIED_REF_SOURCE: &str = "Object subclass: Foo\n  make => xml@Parser new\n";

fn unknown_package_messages(backend: &Backend, file: &Utf8PathBuf) -> Vec<String> {
    let svc = backend.service.lock_recover();
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
        .lock_recover()
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

// BT-4929: the LSP reaches the qualifier check through `analyse_full`, which
// checks the module as written. A qualifier in a protocol's provided method is
// therefore reported once, in the protocol's file, however many classes use the
// protocol, and a `uses: nosuchpkg@Foo` is reported once, at the qualifier.

const GREETS_SOURCE: &str =
    "Protocol define: Greets\n  name -> String\n\n  greet -> Object => nosuchpkg@Widget new\n";
const USER_A_SOURCE: &str = "Object subclass: UserA\n  uses: Greets\n  name -> String => \"a\"\n";
const USER_B_SOURCE: &str = "Object subclass: UserB\n  uses: Greets\n  name -> String => \"b\"\n";
const USES_QUALIFIED_SOURCE: &str =
    "Object subclass: UsesQualified\n  uses: nosuchpkg@Foo\n  name -> String => \"q\"\n";

/// `(message, text under the diagnostic's span)` for each "Unknown package"
/// diagnostic the editor shows for `file`.
fn unknown_package_diagnostics(
    backend: &Backend,
    file: &Utf8PathBuf,
    source: &str,
) -> Vec<(String, String)> {
    let svc = backend.service.lock_recover();
    svc.diagnostics(file)
        .into_iter()
        .filter(|d| d.message.contains("Unknown package"))
        .map(|d| {
            let start = d.span.start() as usize;
            let end = d.span.end() as usize;
            (d.message.to_string(), source[start..end].to_string())
        })
        .collect()
}

#[tokio::test]
async fn protocol_body_and_uses_qualifiers_are_each_reported_once_in_the_right_file() {
    let temp = unique_temp_dir("beamtalk_lsp_pkg_qualifier_protocol_uses");
    write_manifest_with_git_deps(&temp, "app", &["json"]);

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .load_root_packages(std::slice::from_ref(&temp))
        .await;

    let files = [
        ("Greets", GREETS_SOURCE),
        ("UserA", USER_A_SOURCE),
        ("UserB", USER_B_SOURCE),
        ("UsesQualified", USES_QUALIFIED_SOURCE),
    ];
    let paths: Vec<Utf8PathBuf> = files
        .iter()
        .map(|(name, _)| {
            Utf8PathBuf::from_path_buf(temp.join("src").join(format!("{name}.bt"))).unwrap()
        })
        .collect();
    {
        let mut svc = backend.service.lock_recover();
        for (path, (_, source)) in paths.iter().zip(files) {
            svc.update_file(path.clone(), source.to_string());
        }
    }

    let report = |i: usize| unknown_package_diagnostics(backend, &paths[i], files[i].1);

    // The protocol's own file: its body's qualifier, once, at the qualifier.
    let greets = report(0);
    assert_eq!(greets.len(), 1, "Greets.bt: {greets:?}");
    assert!(greets[0].0.contains("'nosuchpkg@Widget'"), "{greets:?}");
    assert_eq!(greets[0].1, "nosuchpkg", "{greets:?}");

    // Users of the protocol: nothing — the body was not written in their file.
    for i in [1, 2] {
        let user = report(i);
        assert!(user.is_empty(), "{}: {user:?}", paths[i]);
    }

    // `uses: nosuchpkg@Foo`: once, at the qualifier, and not alongside the
    // flattener's "no source available" error for the same line.
    let uses = report(3);
    assert_eq!(uses.len(), 1, "UsesQualified.bt: {uses:?}");
    assert!(
        uses[0].0.contains("protocol reference 'nosuchpkg@Foo'"),
        "{uses:?}"
    );
    assert_eq!(uses[0].1, "nosuchpkg", "{uses:?}");
    let all_messages: Vec<String> = {
        let svc = backend.service.lock_recover();
        svc.diagnostics(&paths[3])
            .into_iter()
            .map(|d| d.message.to_string())
            .collect()
    };
    assert!(
        !all_messages
            .iter()
            .any(|m| m.contains("no source available")),
        "{all_messages:?}"
    );

    let _ = fs::remove_dir_all(&temp);
}
