// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! A file under a `stdlib/` tree gets the runtime's FFI registry in LSP
//! diagnostics, by the rule `build`, `test` and `lint` share
//! (`ffi_type_registry::lint_type_registry`, BT-4965), so the editor does not
//! report a needed `@expect type` as stale (BT-4967).
//!
//! The beamtalk repo's `stdlib/` has no `beamtalk.toml`; a user package does,
//! so a user project that merely has a folder called `stdlib` is told apart by
//! its manifest and keeps the registry its own root builds.

use super::*;

/// Needs `@expect type` once the runtime's `-spec` for
/// `beamtalk_character:is_digit/1` is known; with no registry the marker looks
/// stale. Same shape as the CLI twin
/// `lint_does_not_report_needed_expect_type_stale_under_stdlib_test_bt_4965`.
const NEEDED_EXPECT_SOURCE: &str = "Object subclass: FfiExpectTest\n\n  bad =>\n    @expect type\n    Erlang beamtalk_character is_digit: #notAnInteger\n";

/// Opens `file` (with `NEEDED_EXPECT_SOURCE`) through the real `did_open`
/// handler and returns the messages of the diagnostics the editor shows.
async fn open_and_diagnose(file: &std::path::Path) -> Vec<String> {
    std::fs::create_dir_all(file.parent().unwrap()).expect("create dir");
    std::fs::write(file, NEEDED_EXPECT_SOURCE).expect("write file");
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let uri = Url::from_file_path(file).expect("file uri");
    real_did_open(backend, uri, NEEDED_EXPECT_SOURCE).await;
    let path = Utf8PathBuf::from_path_buf(file.to_path_buf()).expect("utf8 path");
    let svc = backend.service.lock().expect("service lock poisoned");
    svc.diagnostics(&path)
        .into_iter()
        .map(|d| d.message.to_string())
        .collect()
}

fn runtime_specs_available() -> bool {
    beamtalk_package::native_type_specs::extract_stdlib_type_specs().is_some()
}

#[tokio::test]
async fn stdlib_test_file_gets_runtime_registry_so_needed_expect_is_not_stale() {
    assert!(
        runtime_specs_available(),
        "build the runtime first (just build-erlang)"
    );
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp
        .path()
        .join("stdlib")
        .join("test")
        .join("ffi_expect_test.bt");

    let messages = open_and_diagnose(&file).await;
    assert!(
        messages.iter().all(|m| !m.contains("stale")),
        "needed @expect type must not be stale under stdlib/test; got: {messages:?}"
    );
}

#[tokio::test]
async fn stdlib_src_file_gets_runtime_registry_too() {
    assert!(
        runtime_specs_available(),
        "build the runtime first (just build-erlang)"
    );
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp
        .path()
        .join("stdlib")
        .join("src")
        .join("FfiExpectTest.bt");

    let messages = open_and_diagnose(&file).await;
    assert!(
        messages.iter().all(|m| !m.contains("stale")),
        "got: {messages:?}"
    );
}

/// Non-vacuous: without the `stdlib/` path the same source IS reported stale,
/// so the two tests above prove the registry made the difference.
#[tokio::test]
async fn outside_stdlib_the_same_marker_is_still_stale() {
    assert!(
        runtime_specs_available(),
        "build the runtime first (just build-erlang)"
    );
    let temp = tempfile::TempDir::new().unwrap();
    let file = temp.path().join("scratch").join("ffi_expect_test.bt");

    let messages = open_and_diagnose(&file).await;
    assert!(
        messages.iter().any(|m| m.contains("stale")),
        "no runtime registry outside stdlib/; got: {messages:?}"
    );
}

/// A user package that happens to have a `stdlib/` folder has a
/// `beamtalk.toml`; it is not the beamtalk repo and gets no runtime registry.
#[tokio::test]
async fn user_package_with_a_stdlib_folder_gets_no_runtime_registry() {
    assert!(
        runtime_specs_available(),
        "build the runtime first (just build-erlang)"
    );
    let temp = tempfile::TempDir::new().unwrap();
    std::fs::write(
        temp.path().join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    let file = temp
        .path()
        .join("stdlib")
        .join("test")
        .join("ffi_expect_test.bt");

    let messages = open_and_diagnose(&file).await;
    assert!(
        messages.iter().any(|m| m.contains("stale")),
        "a package's own stdlib/ folder must not get the runtime registry; got: {messages:?}"
    );
}

/// The real files BT-4965 fixed for `beamtalk lint`: the editor shows no
/// stale `@expect type` or FFI-registry warning for them either. (Their
/// `@expect unresolved_class` markers need the project-wide index a bare
/// service lacks, so they are out of scope here.)
#[tokio::test]
async fn real_stdlib_test_files_show_no_stale_or_ffi_registry_warnings() {
    assert!(
        runtime_specs_available(),
        "build the runtime first (just build-erlang)"
    );
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for name in [
        "bt3911arm_frames_test.bt",
        "class_side_missing_class_test.bt",
    ] {
        let file = repo
            .join("stdlib")
            .join("test")
            .join(name)
            .canonicalize()
            .unwrap();
        let source = std::fs::read_to_string(&file).unwrap();
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        real_did_open(backend, Url::from_file_path(&file).unwrap(), &source).await;
        let path = Utf8PathBuf::from_path_buf(file).unwrap();
        let svc = backend.service.lock().expect("service lock poisoned");
        let bad: Vec<String> = svc
            .diagnostics(&path)
            .into_iter()
            .map(|d| d.message.to_string())
            .filter(|m| {
                m.contains("stale @expect type")
                    || m.contains("untyped FFI")
                    || m.contains("Unknown module")
            })
            .collect();
        assert!(bad.is_empty(), "{name}: {bad:?}");
    }
}
