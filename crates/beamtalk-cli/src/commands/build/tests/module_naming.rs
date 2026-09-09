// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `compute_relative_module` and manifest-driven package/module naming tests: package module names, subdirectories, `build_dev/ebin` layout, app-file generation, and declared-alias metadata (including forward-slash path normalization).

use super::*;
use crate::commands::util::to_forward_slash;

#[test]
fn test_compute_relative_module_flat() {
    let root = Utf8Path::new("/project/src");
    let file = Utf8Path::new("/project/src/counter.bt");
    assert_eq!(
        compute_relative_module(file, Some(root)).unwrap(),
        "counter"
    );
}

#[test]
fn test_compute_relative_module_subdirectory() {
    let root = Utf8Path::new("/project/src");
    let file = Utf8Path::new("/project/src/util/math.bt");
    assert_eq!(
        compute_relative_module(file, Some(root)).unwrap(),
        "util@math"
    );
}

#[test]
fn test_compute_relative_module_deep_subdirectory() {
    let root = Utf8Path::new("/project/src");
    let file = Utf8Path::new("/project/src/a/b/c.bt");
    assert_eq!(compute_relative_module(file, Some(root)).unwrap(), "a@b@c");
}

#[test]
fn test_compute_relative_module_camel_case() {
    let root = Utf8Path::new("/project/src");
    let file = Utf8Path::new("/project/src/MyCounter.bt");
    assert_eq!(
        compute_relative_module(file, Some(root)).unwrap(),
        "my_counter"
    );
}

#[test]
fn test_compute_relative_module_no_root() {
    let file = Utf8Path::new("/project/counter.bt");
    assert_eq!(compute_relative_module(file, None).unwrap(), "counter");
}

#[test]
fn test_compute_relative_module_invalid_dir_name() {
    let root = Utf8Path::new("/project/src");
    let file = Utf8Path::new("/project/src/my-dir/counter.bt");
    let result = compute_relative_module(file, Some(root));
    assert!(result.is_err());
}

#[test]
fn test_build_with_manifest_generates_package_module_name() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("counter.bt"), "counter := [42].");
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
    );

    let result = build(project_path.as_str(), &default_options(), false);

    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            // Verify the .core file was generated with package-qualified name
            let core_file = project_path.join("_build/dev/ebin/bt@my_app@counter.core");
            assert!(
                core_file.exists(),
                "Expected package-qualified .core file at {core_file}"
            );
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }
}

#[test]
fn test_build_without_manifest_preserves_bt_prefix() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("counter.bt"), "counter := [42].");
    // No beamtalk.toml

    let result = build(project_path.as_str(), &default_options(), false);

    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            let core_file = project_path.join("build/bt@counter.core");
            assert!(
                core_file.exists(),
                "Expected bt@counter.core file at {core_file}"
            );
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }
}

#[test]
fn test_build_with_manifest_subdirectory() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    let util_path = src_path.join("util");
    fs::create_dir_all(&util_path).unwrap();
    write_test_file(&util_path.join("math.bt"), "math := [42].");
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
    );

    let result = build(project_path.as_str(), &default_options(), false);

    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            let core_file = project_path.join("_build/dev/ebin/bt@my_app@util@math.core");
            assert!(
                core_file.exists(),
                "Expected package-qualified .core file at {core_file}"
            );
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }
}

#[test]
fn test_build_with_manifest_creates_build_dev_ebin() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("main.bt"), "main := [42].");
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
    );

    let result = build(project_path.as_str(), &default_options(), false);

    // Verify _build/dev/ebin/ directory structure exists
    let ebin_dir = project_path.join("_build/dev/ebin");
    assert!(ebin_dir.exists(), "Expected _build/dev/ebin/ directory");

    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }
}

#[test]
fn test_build_without_manifest_uses_build_dir() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("main.bt"), "main := [42].");
    // No beamtalk.toml

    let result = build(project_path.as_str(), &default_options(), false);

    // Verify build/ directory is used (not _build/)
    let build_dir = project_path.join("build");
    assert!(
        build_dir.exists(),
        "Expected build/ directory for single-file mode"
    );
    let ebin_dir = project_path.join("_build");
    assert!(
        !ebin_dir.exists(),
        "_build/ should not exist in single-file mode"
    );

    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }
}

#[test]
fn test_build_with_manifest_generates_app_file() {
    let temp = TempDir::new().unwrap();
    let project_path = create_test_project(&temp);
    let src_path = project_path.join("src");
    write_test_file(&src_path.join("counter.bt"), "counter := [42].");
    write_test_file(
        &project_path.join("beamtalk.toml"),
        "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\ndescription = \"Test app\"\n",
    );

    let result = build(project_path.as_str(), &default_options(), false);

    if let Err(e) = result {
        let error_msg = format!("{e:?}");
        if error_msg.contains("escript not found") {
            // .app is generated after BEAM compile — may not exist if escript missing
            // but the .core file should exist
            let core_file = project_path.join("_build/dev/ebin/bt@my_app@counter.core");
            assert!(core_file.exists(), "Expected .core file at {core_file}");
            return;
        }
        panic!("Build failed with unexpected error: {e:?}");
    }

    // If BEAM compilation succeeded, .app file should exist
    let app_file = project_path.join("_build/dev/ebin/my_app.app");
    assert!(app_file.exists(), "Expected .app file at {app_file}");
    let content = fs::read_to_string(&app_file).unwrap();
    assert!(content.contains("{application, my_app, ["));
    assert!(content.contains("{description, \"Test app\"}"));
    assert!(content.contains("{vsn, \"0.1.0\"}"));
    assert!(content.contains("'bt@my_app@counter'"));
}

#[test]
fn test_build_alias_metadata_extracts_declared_aliases() {
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();
    let alias_file = src_path.join("restart_strategy.bt");
    write_test_file(
        &alias_file,
        "/// Restart strategy for a supervised child.\n\
             type RestartStrategy = #temporary | #transient | #permanent\n\n\
             internal type ParserState = Integer | String\n",
    );

    let result = build_alias_metadata(&[alias_file]);

    assert_eq!(result.len(), 2, "expected two aliases, got: {result:?}");
    let public = result
        .iter()
        .find(|a| a.name == "RestartStrategy")
        .expect("RestartStrategy alias missing");
    assert_eq!(public.expansion, "#temporary | #transient | #permanent");
    assert_eq!(
        public.doc.as_deref(),
        Some("Restart strategy for a supervised child.")
    );
    assert!(!public.internal);

    let internal = result
        .iter()
        .find(|a| a.name == "ParserState")
        .expect("ParserState alias missing");
    assert!(internal.internal);
    assert_eq!(internal.doc, None);
}

#[test]
fn test_build_alias_metadata_skips_unreadable_file() {
    let nonexistent = Utf8PathBuf::from("/nonexistent/no_such_file.bt");
    let result = build_alias_metadata(&[nonexistent]);
    assert!(result.is_empty());
}

#[test]
fn test_build_alias_metadata_preserves_declaration_order() {
    // Sorting is `format_type_aliases_entry`'s job (app_file.rs), not
    // this function's — see `test_format_type_aliases_entry_sorted`.
    // `build_alias_metadata` itself just collects in source-file order.
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();
    let alias_file = src_path.join("aliases.bt");
    write_test_file(&alias_file, "type Zebra = Integer\ntype Alpha = Integer\n");

    let result = build_alias_metadata(&[alias_file]);
    let names: Vec<&str> = result.iter().map(|a| a.name.as_str()).collect();
    assert_eq!(names, vec!["Zebra", "Alpha"]);
}

// BT-3067: `source_file` is written verbatim into the checked-in
// `beamtalk_stdlib.app.src`, so it must be identical regardless of the
// host OS that generated it. Uses a `Utf8PathBuf` built directly from a
// backslash-containing string (rather than `Utf8Path::join`, whose
// separator behavior itself varies by host OS) so the test exercises the
// Windows-native-path case deterministically on every platform, including
// Linux CI.
#[test]
fn test_to_forward_slash_normalizes_backslashes() {
    let path = Utf8PathBuf::from("stdlib/src\\ets.bt");
    assert_eq!(to_forward_slash(path.as_str()), "stdlib/src/ets.bt");
}

#[test]
fn test_to_forward_slash_normalizes_all_backslash_components() {
    let path = Utf8PathBuf::from("stdlib\\src\\ets.bt");
    assert_eq!(to_forward_slash(path.as_str()), "stdlib/src/ets.bt");
}

#[test]
fn test_to_forward_slash_leaves_forward_slash_paths_unchanged() {
    let path = Utf8PathBuf::from("stdlib/src/ets.bt");
    assert_eq!(to_forward_slash(path.as_str()), "stdlib/src/ets.bt");
}

#[test]
fn test_build_alias_metadata_source_file_uses_forward_slashes() {
    // Exercises the real join-based path (not a hand-built string) so
    // this also catches the bug as originally reported: on Windows,
    // `src_path.join("aliases.bt")` yields a `Utf8PathBuf` whose
    // `Display`/`to_string()` uses `\`, which used to leak straight into
    // `source_file` before the `to_forward_slash` fix.
    let temp = TempDir::new().unwrap();
    let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_path = project_path.join("src");
    fs::create_dir_all(&src_path).unwrap();
    let alias_file = src_path.join("aliases.bt");
    write_test_file(&alias_file, "type Zebra = Integer\n");

    let result = build_alias_metadata(&[alias_file]);
    assert_eq!(result.len(), 1);
    assert!(
        !result[0].source_file.contains('\\'),
        "source_file should never contain a backslash, got: {:?}",
        result[0].source_file
    );
}
