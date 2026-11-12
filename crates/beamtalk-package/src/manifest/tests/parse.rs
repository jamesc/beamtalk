// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `manifest` tests: parse.

use super::*;

/// BT-4382: one checker-scope rule for every surface, including the
/// malformed-manifest case the four former copies disagreed on.
#[test]
fn package_checker_scope_covers_every_manifest_state() {
    use beamtalk_core::semantic_analysis::KnowledgeScope;

    assert_eq!(
        package_checker_scope(None),
        PackageCheckerScope::module_only()
    );

    let dir = TempDir::new().unwrap();
    let root = write_manifest(&dir, "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n");
    let scope = package_checker_scope(Some(&root));
    assert_eq!(scope.knowledge_scope, KnowledgeScope::ProjectComplete);
    assert!(!scope.has_package_dependencies);
    assert_eq!(scope.package_name.as_deref(), Some("my_pkg"));

    let dir = TempDir::new().unwrap();
    let root = write_manifest(
        &dir,
        "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n\n[dependencies]\nfoo = { path = \"../foo\" }\n",
    );
    assert!(package_checker_scope(Some(&root)).has_package_dependencies);

    let dir = TempDir::new().unwrap();
    let root = write_manifest(&dir, "[package\nnot toml");
    let scope = package_checker_scope(Some(&root));
    assert_eq!(scope.knowledge_scope, KnowledgeScope::ProjectComplete);
    assert!(
        scope.has_package_dependencies,
        "a malformed manifest conservatively assumes dependencies"
    );
    assert_eq!(scope.package_name, None);

    let dir = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
    assert_eq!(
        package_checker_scope(Some(&root)),
        PackageCheckerScope::module_only(),
        "a root with no manifest is not a package"
    );
}

#[test]
fn test_parse_valid_manifest() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
"#,
    );

    let manifest = parse_manifest(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.name, "my_app");
    assert_eq!(manifest.version, "0.1.0");
    assert!(manifest.description.is_none());
    assert!(manifest.licenses.is_none());
}

#[test]
fn test_parse_manifest_with_optional_fields() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "1.2.3"
description = "A cool app"
licenses = ["Apache-2.0", "MIT"]

[dependencies]
"#,
    );

    let manifest = parse_manifest(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.name, "my_app");
    assert_eq!(manifest.version, "1.2.3");
    assert_eq!(manifest.description.as_deref(), Some("A cool app"));
    assert_eq!(
        manifest.licenses.as_deref(),
        Some(["Apache-2.0".to_string(), "MIT".to_string()].as_slice())
    );
}

#[test]
fn test_parse_manifest_missing_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
version = "0.1.0"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("beamtalk.toml"),
        "error should mention file: {err}"
    );
}

#[test]
fn test_parse_manifest_missing_version() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
}

#[test]
fn test_parse_manifest_missing_package_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r"
[dependencies]
",
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
}

#[test]
fn test_parse_manifest_invalid_toml() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, "this is not valid toml {{{{");

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
}

#[test]
fn test_parse_manifest_nonexistent_file() {
    let path = Utf8PathBuf::from("/nonexistent/beamtalk.toml");
    let result = parse_manifest(&path);
    assert!(result.is_err());
}

#[test]
fn test_find_manifest_exists() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "found_app"
version = "0.1.0"
"#,
    );

    let result = find_manifest(&path).unwrap();
    assert!(result.is_some());
    assert_eq!(result.unwrap().name, "found_app");
}

#[test]
fn test_find_manifest_not_found() {
    let temp = TempDir::new().unwrap();
    let path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let result = find_manifest(&path).unwrap();
    assert!(result.is_none());
}

#[test]
fn test_package_name_for_project() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "named_app"
version = "0.1.0"
"#,
    );
    assert_eq!(
        package_name_for_project(path.as_str()),
        Some("named_app".to_string())
    );
}

#[test]
fn test_package_name_for_project_missing_or_malformed_is_none() {
    let temp = TempDir::new().unwrap();
    let missing = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    assert_eq!(package_name_for_project(missing.as_str()), None);

    let bad = TempDir::new().unwrap();
    let bad_path = write_manifest(&bad, "not valid toml {{{{");
    assert_eq!(package_name_for_project(bad_path.as_str()), None);
}

#[test]
fn test_find_manifest_malformed() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, "not valid toml {{{{");

    let result = find_manifest(&path);
    assert!(result.is_err());
}

#[test]
fn test_parse_manifest_extra_fields_ignored() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"
unknown_field = "should be ignored"

[dependencies]
some_dep = "1.0"
"#,
    );

    let manifest = parse_manifest(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.name, "my_app");
    assert_eq!(manifest.version, "0.1.0");
}

#[test]
fn test_parse_manifest_rejects_invalid_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "MyApp"
version = "0.1.0"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(err.contains("my_app"), "should suggest fix: {err}");
}

#[test]
fn test_parse_manifest_rejects_reserved_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "stdlib"
version = "0.1.0"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(err.contains("reserved"), "should mention reserved: {err}");
}

#[test]
fn test_parse_manifest_rejects_erlang_app_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "crypto"
version = "0.1.0"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("Erlang"),
        "should mention Erlang conflict: {err}"
    );
}

#[test]
fn test_find_application_config_present() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[application]
supervisor = "AppSup"
"#,
    );

    let app_config = find_application_config(&path).unwrap();
    assert!(app_config.is_some());
    assert_eq!(app_config.unwrap().supervisor, "AppSup");
}

#[test]
fn test_find_application_config_absent() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"
"#,
    );

    let app_config = find_application_config(&path).unwrap();
    assert!(app_config.is_none());
}

#[test]
fn test_find_application_config_no_manifest() {
    let temp = TempDir::new().unwrap();
    let path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let app_config = find_application_config(&path).unwrap();
    assert!(app_config.is_none());
}

#[test]
fn test_find_application_config_rejects_invalid_package_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "BadName"
version = "0.1.0"

[application]
supervisor = "AppSup"
"#,
    );

    let result = find_application_config(&path);
    assert!(
        result.is_err(),
        "should reject manifest with invalid package name"
    );
}

#[test]
fn test_parse_manifest_with_application_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[application]
supervisor = "RootSup"
"#,
    );

    let manifest = parse_manifest_raw(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.application.is_some());
    assert_eq!(manifest.application.unwrap().supervisor, "RootSup");
}
