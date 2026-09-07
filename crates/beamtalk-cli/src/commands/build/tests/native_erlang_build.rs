// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0072 Phase 1: native Erlang compilation during build, and native-module collision detection across the root package and its dependencies.

use super::*;

#[test]
fn test_rebar3_path_returns_bundled_when_exists() {
    // When running from the repo root (or with BEAMTALK_RUNTIME_DIR set),
    // rebar3_path() should find the bundled copy at runtime/tools/rebar3.
    let result = rebar3_path();
    // In CI or dev, the bundled rebar3 should be present.
    // If neither bundled nor system rebar3 is available, this test will
    // fail — that's intentional, as it means the vendored copy is missing.
    assert!(
        result.is_ok(),
        "rebar3_path() should find rebar3: {result:?}"
    );

    let path = result.unwrap();
    // When running in the dev repo, the path should be the bundled copy
    assert!(
        path.ends_with("tools/rebar3"),
        "Expected bundled rebar3 path ending in 'tools/rebar3', got: {path:?}"
    );
}

// ---- ADR 0072 Phase 1: native Erlang compilation in build ----

#[test]
fn test_build_without_native_dir_is_unaffected() {
    // Packages without native/ should build exactly as before.
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("main.bt"), "main := [42].");
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"no_native\"\nversion = \"0.1.0\"\n",
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

    // native ebin should NOT be created
    let test_layout = BuildLayout::new(&project_path);
    let native_ebin = test_layout.native_ebin_dir();
    assert!(
        !native_ebin.exists(),
        "native ebin should not be created when no native/ directory exists"
    );
}

#[test]
#[ignore = "requires erlc"]
fn test_build_with_native_erlang() {
    // ADR 0072 Phase 1: build should discover and compile native/*.erl files
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");

    // Create beamtalk.toml
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"with_native\"\nversion = \"0.1.0\"\n",
    );

    // Create a .bt source file
    write_test_file(&src_path.join("main.bt"), "main := [42].");

    // Create native/ with an Erlang module
    let native_dir = project_path.join("native");
    fs::create_dir(&native_dir).unwrap();
    write_test_file(
        &native_dir.join("hello_native.erl"),
        "-module(hello_native).\n-export([greet/0]).\ngreet() -> <<\"hello\">>.\n",
    );

    let result = build(project_path.as_str(), &default_options(), false);
    assert!(result.is_ok(), "Build should succeed: {result:?}");

    // Native BEAM file should exist in _build/dev/native/ebin/
    let test_layout = BuildLayout::new(&project_path);
    let native_beam = test_layout.native_ebin_dir().join("hello_native.beam");
    assert!(
        native_beam.exists(),
        "Native BEAM file should be produced at {native_beam}"
    );

    // Regular .bt BEAM should also exist
    let bt_beam = test_layout.ebin_dir().join("bt@with_native@main.beam");
    assert!(
        bt_beam.exists(),
        "Beamtalk BEAM file should be produced at {bt_beam}"
    );

    // ADR 0072: .app file should include native_modules in env
    let app_file = test_layout.ebin_dir().join("with_native.app");
    assert!(app_file.exists(), "Expected .app file at {app_file}");
    let app_content = fs::read_to_string(&app_file).unwrap();
    assert!(
        app_content.contains("{native_modules, [hello_native]}"),
        "Generated .app should contain native_modules. Got: {app_content}"
    );
}

// ---- ADR 0072 Phase 1: native module collision detection ----

#[test]
fn test_no_collision_single_package_no_native() {
    // Packages without native/ should pass collision check trivially.
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);

    let result = check_native_module_collisions(&project_path, "test_pkg", &[]);
    assert!(result.is_ok(), "No native dir should pass: {result:?}");
}

#[test]
fn test_no_collision_single_package_with_native() {
    // A single package with native modules should not trigger collision.
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n",
    );

    let native_dir = project_path.join("native");
    fs::create_dir(&native_dir).unwrap();
    write_test_file(
        &native_dir.join("my_helper.erl"),
        "-module(my_helper).\n-export([hello/0]).\nhello() -> ok.\n",
    );

    let result = check_native_module_collisions(&project_path, "my_pkg", &[]);
    assert!(result.is_ok(), "Single package should pass: {result:?}");
}

#[test]
fn test_collision_between_root_and_dependency() {
    // Root package and a dependency both define the same native module name.
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"root_pkg\"\nversion = \"0.1.0\"\n",
    );

    // Root package has native/utils.erl
    let root_native = project_path.join("native");
    fs::create_dir(&root_native).unwrap();
    write_test_file(
        &root_native.join("utils.erl"),
        "-module(utils).\n-export([]).\n",
    );

    // Dependency also has native/utils.erl
    let dep_dir = temp.path().join("dep_pkg");
    fs::create_dir_all(dep_dir.join("native")).unwrap();
    fs::create_dir_all(dep_dir.join("src")).unwrap();
    fs::write(
        dep_dir.join("native").join("utils.erl"),
        "-module(utils).\n-export([]).\n",
    )
    .unwrap();

    let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();
    let deps = vec![super::super::super::deps::path::ResolvedDependency {
        name: "dep_pkg".to_string(),
        root: dep_root.clone(),
        ebin_path: dep_root.join("ebin"),
        class_module_index: HashMap::new(),
        class_infos: Vec::new(),
        protocol_infos: Vec::new(),
        alias_infos: Vec::new(),
        is_direct: true,
        via_chain: Vec::new(),
        stubs_dir: None,
    }];

    let result = check_native_module_collisions(&project_path, "root_pkg", &deps);
    assert!(result.is_err(), "Should detect collision");

    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("utils"),
        "Error should name the conflicting module: {err_msg}"
    );
    assert!(
        err_msg.contains("root_pkg"),
        "Error should name the root package: {err_msg}"
    );
    assert!(
        err_msg.contains("dep_pkg"),
        "Error should name the dependency: {err_msg}"
    );
}

#[test]
fn test_collision_between_two_dependencies() {
    // Two dependencies define the same native module name (root has none).
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"root_pkg\"\nversion = \"0.1.0\"\n",
    );

    // first_dep has native/shared_mod.erl
    let first_dep_dir = temp.path().join("dep_a");
    fs::create_dir_all(first_dep_dir.join("native")).unwrap();
    fs::create_dir_all(first_dep_dir.join("src")).unwrap();
    fs::write(
        first_dep_dir.join("native").join("shared_mod.erl"),
        "-module(shared_mod).\n-export([]).\n",
    )
    .unwrap();

    // second_dep also has native/shared_mod.erl
    let second_dep_dir = temp.path().join("dep_b");
    fs::create_dir_all(second_dep_dir.join("native")).unwrap();
    fs::create_dir_all(second_dep_dir.join("src")).unwrap();
    fs::write(
        second_dep_dir.join("native").join("shared_mod.erl"),
        "-module(shared_mod).\n-export([]).\n",
    )
    .unwrap();

    let first_root = Utf8PathBuf::from_path_buf(first_dep_dir).unwrap();
    let second_root = Utf8PathBuf::from_path_buf(second_dep_dir).unwrap();
    let deps = vec![
        super::super::super::deps::path::ResolvedDependency {
            name: "dep_a".to_string(),
            root: first_root.clone(),
            ebin_path: first_root.join("ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: None,
        },
        super::super::super::deps::path::ResolvedDependency {
            name: "dep_b".to_string(),
            root: second_root.clone(),
            ebin_path: second_root.join("ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: None,
        },
    ];

    let result = check_native_module_collisions(&project_path, "root_pkg", &deps);
    assert!(result.is_err(), "Should detect collision between deps");

    let err_msg = format!("{:?}", result.unwrap_err());
    assert!(
        err_msg.contains("shared_mod"),
        "Error should name the module: {err_msg}"
    );
    assert!(
        err_msg.contains("dep_a"),
        "Error should name dep_a: {err_msg}"
    );
    assert!(
        err_msg.contains("dep_b"),
        "Error should name dep_b: {err_msg}"
    );
}

#[test]
fn test_no_collision_different_native_modules() {
    // Root and dependency have native modules with different names — no collision.
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"root_pkg\"\nversion = \"0.1.0\"\n",
    );

    // Root has native/root_utils.erl
    let root_native = project_path.join("native");
    fs::create_dir(&root_native).unwrap();
    write_test_file(
        &root_native.join("root_utils.erl"),
        "-module(root_utils).\n-export([]).\n",
    );

    // Dependency has native/dep_utils.erl
    let dep_dir = temp.path().join("dep_pkg");
    fs::create_dir_all(dep_dir.join("native")).unwrap();
    fs::create_dir_all(dep_dir.join("src")).unwrap();
    fs::write(
        dep_dir.join("native").join("dep_utils.erl"),
        "-module(dep_utils).\n-export([]).\n",
    )
    .unwrap();

    let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();
    let deps = vec![super::super::super::deps::path::ResolvedDependency {
        name: "dep_pkg".to_string(),
        root: dep_root.clone(),
        ebin_path: dep_root.join("ebin"),
        class_module_index: HashMap::new(),
        class_infos: Vec::new(),
        protocol_infos: Vec::new(),
        alias_infos: Vec::new(),
        is_direct: true,
        via_chain: Vec::new(),
        stubs_dir: None,
    }];

    let result = check_native_module_collisions(&project_path, "root_pkg", &deps);
    assert!(
        result.is_ok(),
        "Different module names should pass: {result:?}"
    );
}
