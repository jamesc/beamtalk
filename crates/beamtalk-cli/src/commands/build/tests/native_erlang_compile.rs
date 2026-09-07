// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `compile_native_erlang_with_deps` and `collect_erl_files` tests: empty/missing native dirs, non-.erl files ignored, simple modules, includes, and compile errors.

use super::*;

// ---- compile_native_erlang_with_deps tests ----

#[test]
fn test_compile_native_erlang_with_deps_no_native_dir() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
    assert!(
        result.is_none(),
        "Should return None when no native/ directory exists"
    );
}

#[test]
fn test_compile_native_erlang_with_deps_empty_native_dir() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    fs::create_dir(project_root.join("native")).unwrap();

    let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
    assert!(
        result.is_none(),
        "Should return None when native/ has no .erl files"
    );
}

#[test]
fn test_compile_native_erlang_with_deps_ignores_non_erl_files() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let native_dir = project_root.join("native");
    fs::create_dir(&native_dir).unwrap();

    // Only non-.erl files
    fs::write(native_dir.join("readme.md"), "# Native stuff").unwrap();
    fs::write(native_dir.join("notes.txt"), "notes").unwrap();

    let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
    assert!(
        result.is_none(),
        "Should return None when no .erl files found"
    );
}

#[test]
#[ignore = "requires erlc"]
fn test_compile_native_erlang_with_deps_simple_module() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let native_dir = project_root.join("native");
    fs::create_dir(&native_dir).unwrap();

    fs::write(
        native_dir.join("hello_native.erl"),
        "-module(hello_native).\n-export([greet/0]).\ngreet() -> <<\"hello from native\">>.\n",
    )
    .unwrap();

    let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
    assert!(result.is_some(), "Should compile native .erl files");

    let ebin_dir = result.unwrap();
    assert!(
        ebin_dir.ends_with("_build/dev/native/ebin"),
        "ebin_dir should be _build/dev/native/ebin, got: {ebin_dir}"
    );
    assert!(
        ebin_dir.join("hello_native.beam").exists(),
        "BEAM file should exist"
    );
}

#[test]
#[ignore = "requires erlc"]
fn test_compile_native_erlang_with_deps_with_include() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let native_dir = project_root.join("native");
    let include_dir = native_dir.join("include");
    fs::create_dir_all(&include_dir).unwrap();

    fs::write(include_dir.join("my_defs.hrl"), "-define(MY_CONST, 42).\n").unwrap();

    fs::write(
            native_dir.join("with_include.erl"),
            "-module(with_include).\n-include(\"my_defs.hrl\").\n-export([value/0]).\nvalue() -> ?MY_CONST.\n",
        )
        .unwrap();

    let result = compile_native_erlang_with_deps(&project_root, &[]).unwrap();
    assert!(result.is_some(), "Should compile with includes");
    let ebin_dir = result.unwrap();
    assert!(ebin_dir.join("with_include.beam").exists());
}

#[test]
#[ignore = "requires erlc"]
fn test_compile_native_erlang_with_deps_compile_error() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let native_dir = project_root.join("native");
    fs::create_dir(&native_dir).unwrap();

    // Write truly broken syntax (undefined calls just warn, not error)
    fs::write(
        native_dir.join("broken.erl"),
        "-module(broken).\n-export([oops/0]).\noops( -> ok.\n",
    )
    .unwrap();

    let result = compile_native_erlang_with_deps(&project_root, &[]);
    assert!(result.is_err(), "Should fail with syntax error");
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("Native Erlang compilation failed"),
        "Error should mention native compilation: {err_msg}"
    );
}

// ---- collect_erl_files tests ----

#[test]
fn test_collect_erl_files_filters_correctly() {
    let temp = TempDir::new().unwrap();
    let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    fs::write(dir.join("foo.erl"), "-module(foo).").unwrap();
    fs::write(dir.join("bar.erl"), "-module(bar).").unwrap();
    fs::write(dir.join("readme.md"), "# Notes").unwrap();
    fs::write(dir.join("data.json"), "{}").unwrap();

    let mut files = Vec::new();
    collect_erl_files(&dir, &mut files).unwrap();
    files.sort();

    assert_eq!(files.len(), 2);
    assert!(files[0].as_str().ends_with("bar.erl"));
    assert!(files[1].as_str().ends_with("foo.erl"));
}
