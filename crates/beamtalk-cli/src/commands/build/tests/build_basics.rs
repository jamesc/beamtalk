// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source-discovery and basic build/compile tests: `find_source_files`, single-file compile, and build over empty/single/multiple-file projects and manifests.

use super::*;

#[test]
fn test_find_source_files_single_file() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let test_file = project_path.join("test.bt");
    write_test_file(&test_file, "test := [1].");

    let files = find_source_files(&test_file).unwrap();
    assert_eq!(files.len(), 1);
    assert_eq!(files[0], test_file);
}

#[test]
fn test_find_source_files_in_directory() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");

    write_test_file(&src_path.join("file1.bt"), "test := [1].");
    write_test_file(&src_path.join("file2.bt"), "test := [2].");
    write_test_file(&src_path.join("other.txt"), "not beamtalk");

    let files = find_source_files(&project_path).unwrap();
    assert_eq!(files.len(), 2);
    assert!(files.iter().any(|f| f.file_name() == Some("file1.bt")));
    assert!(files.iter().any(|f| f.file_name() == Some("file2.bt")));
}

#[test]
fn test_find_source_files_no_src_directory() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    write_test_file(&project_path.join("test.bt"), "test := [1].");

    let files = find_source_files(&project_path).unwrap();
    assert_eq!(files.len(), 1);
}

#[test]
fn test_find_source_files_excludes_stubs_dir_with_src() {
    // ADR 0075 regression: stubs/ is a sibling of src/, so
    // when src/ exists it's already excluded by the src/-only scoping —
    // this pins that behavior so a future refactor can't reintroduce it.
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    let stubs_path = project_path.join("stubs");
    std::fs::create_dir_all(&stubs_path).unwrap();

    write_test_file(&src_path.join("main.bt"), "test := [1].");
    write_test_file(
        &stubs_path.join("lists.bt"),
        "declare native: lists\n  reverse: list -> List\n",
    );

    let files = find_source_files(&project_path).unwrap();
    assert_eq!(files.len(), 1);
    assert!(files.iter().any(|f| f.file_name() == Some("main.bt")));
    assert!(!files.iter().any(|f| f.starts_with(&stubs_path)));
}

#[test]
fn test_find_source_files_excludes_stubs_dir_without_src() {
    // ADR 0075 regression: without a src/ directory, find_source_files
    // must not fall back to scanning the whole project root — that would
    // sweep stubs/*.bt into the normal compile pipeline, where
    // `declare native:` is a hard error.
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let stubs_path = project_path.join("stubs");
    std::fs::create_dir_all(&stubs_path).unwrap();

    write_test_file(&project_path.join("main.bt"), "test := [1].");
    write_test_file(
        &stubs_path.join("lists.bt"),
        "declare native: lists\n  reverse: list -> List\n",
    );

    let files = find_source_files(&project_path).unwrap();
    assert_eq!(files.len(), 1);
    assert!(files.iter().any(|f| f.file_name() == Some("main.bt")));
    assert!(!files.iter().any(|f| f.starts_with(&stubs_path)));
}

#[test]
fn test_find_source_files_non_bt_file_error() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let test_file = project_path.join("test.txt");
    write_test_file(&test_file, "not beamtalk");

    let result = find_source_files(&test_file);
    assert!(result.is_err());
}

#[test]
fn test_find_source_files_nonexistent_path() {
    let path = Utf8PathBuf::from("/nonexistent/path");
    let result = find_source_files(&path);
    assert!(result.is_err());
}

#[test]
fn test_compile_valid_file() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let test_file = project_path.join("test.bt");
    let core_file = project_path.join("test.core");
    write_test_file(&test_file, "test := [1 + 2].");

    let result = compile_file(
        &test_file,
        "test",
        &core_file,
        &default_options(),
        &CompileContext::default(),
        None,
    );
    assert!(result.is_ok());
    assert!(core_file.exists());
}

#[test]
fn test_compile_file_with_syntax_error() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let test_file = project_path.join("test.bt");
    let core_file = project_path.join("test.core");
    write_test_file(&test_file, "test := [1 + ]."); // Syntax error

    let result = compile_file(
        &test_file,
        "test",
        &core_file,
        &default_options(),
        &CompileContext::default(),
        None,
    );
    assert!(result.is_err());
}

#[test]
fn test_build_empty_directory() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);

    let result = build(project_path.as_str(), &default_options(), false);
    assert!(result.is_err());
}

#[test]
fn test_build_single_file() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("main.bt"), "main := [42].");

    let result = build(project_path.as_str(), &default_options(), false);

    // If escript is not available, the test should fail at the BEAM compilation stage
    // We allow this in CI environments
    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            println!("Skipping test - escript not installed in CI environment");
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }
}

#[test]
fn test_build_multiple_files() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");

    write_test_file(&src_path.join("file1.bt"), "test1 := [1].");
    write_test_file(&src_path.join("file2.bt"), "test2 := [2].");

    let result = build(project_path.as_str(), &default_options(), false);

    // If escript is not available, the test should fail at the BEAM compilation stage
    // We allow this in CI environments
    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            println!("Skipping test - escript not installed in CI environment");
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }
}

#[test]
fn test_build_with_manifest() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("main.bt"), "main := [42].");
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
    );

    let result = build(project_path.as_str(), &default_options(), false);

    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            eprintln!("Skipping test - escript not installed in CI environment");
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }
}

#[test]
fn test_build_with_malformed_manifest() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("main.bt"), "main := [42].");
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "this is not valid toml {{{{",
    );

    let result = build(project_path.as_str(), &default_options(), false);
    let err = result.expect_err("Expected build to fail due to malformed manifest");
    let error_msg = format!("{err:?}");
    assert!(
        error_msg.contains("Failed to parse manifest"),
        "Expected error to mention manifest parse failure, got: {error_msg}"
    );
}

#[test]
fn test_find_source_files_recursive() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    let util_path = src_path.join("util");
    fs::create_dir_all(&util_path).unwrap();

    write_test_file(&src_path.join("main.bt"), "main := [1].");
    write_test_file(&util_path.join("math.bt"), "math := [2].");

    let files = find_source_files(&project_path).unwrap();
    assert_eq!(files.len(), 2);
    assert!(files.iter().any(|f| f.file_name() == Some("main.bt")));
    assert!(files.iter().any(|f| f.file_name() == Some("math.bt")));
}

#[test]
fn test_find_source_files_deeply_nested() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    let deep_path = src_path.join("a").join("b").join("c");
    fs::create_dir_all(&deep_path).unwrap();

    write_test_file(&deep_path.join("deep.bt"), "deep := [42].");

    let files = find_source_files(&project_path).unwrap();
    assert_eq!(files.len(), 1);
    assert!(files[0].as_str().contains('a'));
}
