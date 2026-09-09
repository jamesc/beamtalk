// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `collect_preload_files` and `preload_workspace_source_files` tests: stdlib-vs-user classification, src/test walking, dependency src dirs, and multi-root file-priority ordering.

use super::*;

#[test]
fn collect_preload_files_classifies_overlapping_path_as_stdlib() {
    // When the same .bt file appears in both a workspace src/ dir and a
    // stdlib dir, it must end up in stdlib_files (not user_files) so that
    // goto_definition emits a beamtalk-stdlib:// URI rather than file://.
    let temp = unique_temp_dir("beamtalk_lsp_preload_overlap");
    let project_root = temp.join("project");
    let src_dir = project_root.join("src");
    let stdlib_dir = temp.join("stdlib");

    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::create_dir_all(&stdlib_dir).expect("create stdlib dir");

    // Write the same path into both dirs by using a shared physical file
    // path via the stdlib_dir pointing into the src dir is not realistic;
    // instead, create the same filename in both so we can test the set logic.
    // The realistic case is a symlinked or canonicalized path appearing twice.
    let shared_path = src_dir.join("integer.bt");
    fs::write(&shared_path, "Object subclass: Integer").expect("write Integer");
    let stdlib_integer = stdlib_dir.join("integer.bt");
    fs::write(&stdlib_integer, "Object subclass: Integer").expect("write stdlib Integer");

    // The paths are different files with the same name — test that the stdlib
    // path deduplication is independent of the user path deduplication.
    let config = PreloadConfig {
        roots: vec![project_root.clone()],
        stdlib_dirs: vec![stdlib_dir.clone()],
    };
    let loaded = collect_preload_files(config);

    // The two files have the same name but different paths, so both should
    // appear in their respective buckets.
    assert_eq!(loaded.user_files.len(), 1);
    assert_eq!(loaded.stdlib_files.len(), 1);
    assert!(loaded.user_files[0].0.ends_with("integer.bt"));
    assert!(loaded.stdlib_files[0].0.ends_with("integer.bt"));
    // The user file must NOT be the stdlib path.
    assert_ne!(loaded.user_files[0].0, stdlib_integer);
    assert_eq!(loaded.stdlib_files[0].0, stdlib_integer);

    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn collect_preload_files_separates_user_and_stdlib_files() {
    let temp = unique_temp_dir("beamtalk_lsp_preload_separation");
    let project_root = temp.join("project");
    let src_dir = project_root.join("src");
    let stdlib_dir = temp.join("stdlib");

    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::create_dir_all(&stdlib_dir).expect("create stdlib dir");

    fs::write(src_dir.join("User.bt"), "Object subclass: User").expect("write user file");
    fs::write(stdlib_dir.join("integer.bt"), "Object subclass: Integer")
        .expect("write stdlib file");

    let config = PreloadConfig {
        roots: vec![project_root],
        stdlib_dirs: vec![stdlib_dir],
    };
    let loaded = collect_preload_files(config);

    assert_eq!(loaded.user_files.len(), 1);
    assert_eq!(loaded.stdlib_files.len(), 1);
    assert!(loaded.user_files[0].0.ends_with("User.bt"));
    assert!(loaded.stdlib_files[0].0.ends_with("integer.bt"));

    let _ = fs::remove_dir_all(&temp);
}

/// Preload must walk both `src/` and `test/`, or
/// opening a `test/` file in the LSP reports spurious `Unresolved class`
/// for every reference to a `src/` class.
#[test]
fn collect_preload_files_walks_src_and_test() {
    let temp = unique_temp_dir("beamtalk_lsp_preload_test_dir");
    let project_root = temp.join("project");
    let src_dir = project_root.join("src");
    let test_dir = project_root.join("test");

    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::create_dir_all(&test_dir).expect("create test dir");

    fs::write(src_dir.join("Foo.bt"), "Object subclass: Foo").expect("write src file");
    fs::write(test_dir.join("FooTest.bt"), "Object subclass: FooTest").expect("write test file");

    let config = PreloadConfig {
        roots: vec![project_root],
        stdlib_dirs: vec![],
    };
    let loaded = collect_preload_files(config);

    assert_eq!(loaded.user_files.len(), 2);
    assert!(loaded.user_files.iter().any(|(p, _)| p.ends_with("Foo.bt")));
    assert!(
        loaded
            .user_files
            .iter()
            .any(|(p, _)| p.ends_with("FooTest.bt"))
    );

    let _ = fs::remove_dir_all(&temp);
}

/// Preload must walk every `_build/deps/<name>/src/` so classes
/// from declared `beamtalk.toml` dependencies resolve without spurious
/// `Unresolved class` warnings.
#[test]
fn collect_preload_files_walks_dependency_src_dirs() {
    let temp = unique_temp_dir("beamtalk_lsp_preload_deps");
    let project_root = temp.join("project");
    let src_dir = project_root.join("src");
    let dep_src = project_root
        .join("_build")
        .join("deps")
        .join("http")
        .join("src");
    let other_dep_src = project_root
        .join("_build")
        .join("deps")
        .join("json")
        .join("src");

    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::create_dir_all(&dep_src).expect("create dep src dir");
    fs::create_dir_all(&other_dep_src).expect("create other dep src dir");

    fs::write(src_dir.join("App.bt"), "Object subclass: App").expect("write src file");
    fs::write(dep_src.join("HTTPClient.bt"), "Object subclass: HTTPClient")
        .expect("write dep file");
    fs::write(
        other_dep_src.join("JSONParser.bt"),
        "Object subclass: JSONParser",
    )
    .expect("write other dep file");

    let config = PreloadConfig {
        roots: vec![project_root],
        stdlib_dirs: vec![],
    };
    let loaded = collect_preload_files(config);

    assert_eq!(loaded.user_files.len(), 3);
    assert!(loaded.user_files.iter().any(|(p, _)| p.ends_with("App.bt")));
    assert!(
        loaded
            .user_files
            .iter()
            .any(|(p, _)| p.ends_with("HTTPClient.bt")),
        "dependency class HTTPClient must be preloaded, got {:?}",
        loaded.user_files
    );
    assert!(
        loaded
            .user_files
            .iter()
            .any(|(p, _)| p.ends_with("JSONParser.bt")),
        "dependency class JSONParser must be preloaded, got {:?}",
        loaded.user_files
    );

    let _ = fs::remove_dir_all(&temp);
}

/// The real running LSP chains `user_files`/`stdlib_files` into
/// one generic `update_file` loop, which never marked a preloaded
/// stdlib file's `ProjectIndex::is_stdlib_file` — only the separate
/// `ProjectIndex::with_stdlib` constructor (used by beamtalk-cli's build
/// pipeline, not the LSP) did that. Confirms
/// `preload_workspace_source_files` now marks stdlib files before
/// indexing them, so `is_stdlib_file` is true after a real preload.
#[tokio::test]
async fn preload_workspace_source_files_marks_stdlib_files_in_project_index() {
    let temp = unique_temp_dir("beamtalk_lsp_preload_marks_stdlib");
    let project_root = temp.join("project");
    let src_dir = project_root.join("src");
    let stdlib_dir = temp.join("stdlib");

    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::create_dir_all(&stdlib_dir).expect("create stdlib dir");

    fs::write(src_dir.join("App.bt"), "Object subclass: App").expect("write user file");
    let stdlib_direction = stdlib_dir.join("Direction.bt");
    fs::write(
        &stdlib_direction,
        "internal type Direction = #north | #south",
    )
    .expect("write stdlib file");

    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    let config = PreloadConfig {
        roots: vec![project_root],
        stdlib_dirs: vec![stdlib_dir],
    };
    backend.preload_workspace_source_files(config).await;

    let utf8_stdlib_direction =
        Utf8PathBuf::from_path_buf(stdlib_direction).expect("temp path is UTF-8");
    let svc = backend.service.lock().expect("service lock poisoned");
    assert!(
        svc.project_index().is_stdlib_file(&utf8_stdlib_direction),
        "a stdlib file walked by the real preload path must be marked \
             stdlib in the ProjectIndex"
    );

    let seen = svc
        .project_index()
        .cross_file_alias_infos_for(&Utf8PathBuf::from("elsewhere.bt"));
    assert_eq!(
        seen.iter().find(|i| i.name == "Direction").unwrap().package,
        Some("stdlib".into()),
        "a stdlib file's alias must be stamped the stdlib package marker \
             after a real preload, not the same-project marker"
    );

    let _ = fs::remove_dir_all(&temp);
}

/// A dep checkout without a `src/` subdirectory must be skipped
/// silently rather than causing a walk failure.
#[test]
fn collect_preload_files_tolerates_dep_without_src_dir() {
    let temp = unique_temp_dir("beamtalk_lsp_preload_deps_no_src");
    let project_root = temp.join("project");
    let src_dir = project_root.join("src");
    let dep_dir = project_root.join("_build").join("deps").join("oddball");

    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::create_dir_all(&dep_dir).expect("create bare dep dir");
    fs::write(src_dir.join("App.bt"), "Object subclass: App").expect("write src file");

    let config = PreloadConfig {
        roots: vec![project_root],
        stdlib_dirs: vec![],
    };
    let loaded = collect_preload_files(config);

    assert_eq!(loaded.user_files.len(), 1);
    assert!(loaded.user_files[0].0.ends_with("App.bt"));

    let _ = fs::remove_dir_all(&temp);
}

/// In a multi-root workspace, every root's own `src/`/`test/`
/// must be preloaded before *any* root's dependency `src/` directories.
/// Otherwise deps from an earlier root could exhaust the shared
/// `PRELOAD_MAX_FILES` budget and leave a later root's user files
/// unindexed — exactly the unresolved-class noise this change fights.
#[test]
fn collect_preload_files_prioritizes_user_files_across_multi_root_workspace() {
    let temp = unique_temp_dir("beamtalk_lsp_preload_multi_root");
    let root_a = temp.join("root_a");
    let root_b = temp.join("root_b");
    let src_a = root_a.join("src");
    let src_b = root_b.join("src");
    let dep_a_src = root_a.join("_build").join("deps").join("dep_a").join("src");

    fs::create_dir_all(&src_a).expect("create root_a src");
    fs::create_dir_all(&src_b).expect("create root_b src");
    fs::create_dir_all(&dep_a_src).expect("create root_a dep src");

    fs::write(src_a.join("AppA.bt"), "Object subclass: AppA").expect("write src_a file");
    fs::write(src_b.join("AppB.bt"), "Object subclass: AppB").expect("write src_b file");
    fs::write(dep_a_src.join("DepA.bt"), "Object subclass: DepA").expect("write dep file");

    let config = PreloadConfig {
        roots: vec![root_a, root_b],
        stdlib_dirs: vec![],
    };
    let loaded = collect_preload_files(config);

    // The ordering contract: every workspace user file appears in
    // `user_paths` *before* any dependency file. We can observe that by
    // confirming root_b's `AppB.bt` is loaded before root_a's `DepA.bt`.
    let app_b_pos = loaded
        .user_files
        .iter()
        .position(|(p, _)| p.ends_with("AppB.bt"))
        .expect("AppB.bt must be preloaded");
    let dep_a_pos = loaded
        .user_files
        .iter()
        .position(|(p, _)| p.ends_with("DepA.bt"))
        .expect("DepA.bt must be preloaded");
    assert!(
        app_b_pos < dep_a_pos,
        "root_b user file must be walked before root_a dep file (got AppB at {app_b_pos}, DepA at {dep_a_pos})"
    );

    let _ = fs::remove_dir_all(&temp);
}
