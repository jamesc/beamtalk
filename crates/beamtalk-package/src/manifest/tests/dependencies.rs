// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `manifest` tests: dependencies.

use super::*;

// --- Dependency parsing tests ---

#[test]
fn test_parse_path_dependency() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
utils = { path = "../my-utils" }
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.dependencies.len(), 1);
    let dep = &manifest.dependencies["utils"];
    assert_eq!(dep.name, "utils");
    assert_eq!(
        dep.source,
        DependencySource::Path {
            path: PathBuf::from("../my-utils")
        }
    );
}

// --- Registry dependency parsing ---

/// Render an error as a single line.
///
/// miette's `Debug` output hard-wraps to the terminal width, so asserting
/// on a multi-word phrase is otherwise flaky — collapsing whitespace lets
/// tests assert on the message as written.
fn flat_err(err: &miette::Report) -> String {
    format!("{err:?}")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Parse a `[dependencies]` table containing a single entry, returning the
/// result so tests can assert on either success or the error message.
fn parse_single_dep(temp: &TempDir, entry: &str) -> Result<ParsedManifest> {
    let path = write_manifest(
        temp,
        &format!("[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n[dependencies]\n{entry}\n"),
    );
    parse_manifest_full(&path.join("beamtalk.toml"))
}

#[test]
fn test_parse_registry_dependency() {
    let temp = TempDir::new().unwrap();
    let manifest = parse_single_dep(&temp, r#"yaml = "0.2.1""#).unwrap();

    assert_eq!(manifest.dependencies.len(), 1);
    let dep = &manifest.dependencies["yaml"];
    assert_eq!(dep.name, "yaml");
    assert_eq!(
        dep.source,
        DependencySource::Registry {
            version: "0.2.1".to_string()
        }
    );
}

#[test]
fn test_registry_dependency_display() {
    let temp = TempDir::new().unwrap();
    let manifest = parse_single_dep(&temp, r#"yaml = "0.2.1""#).unwrap();
    assert_eq!(
        manifest.dependencies["yaml"].to_string(),
        "yaml (registry: 0.2.1)"
    );
}

#[test]
fn test_registry_and_other_sources_coexist() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
yaml = "0.2.1"
utils = { path = "../my-utils" }
json = { git = "https://example.test/json", tag = "v1.0.0" }
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.dependencies.len(), 3);
    assert!(matches!(
        manifest.dependencies["yaml"].source,
        DependencySource::Registry { .. }
    ));
    assert!(matches!(
        manifest.dependencies["utils"].source,
        DependencySource::Path { .. }
    ));
    assert!(matches!(
        manifest.dependencies["json"].source,
        DependencySource::Git { .. }
    ));
}

#[test]
fn test_registry_dependency_rejects_partial_version() {
    let temp = TempDir::new().unwrap();
    let err = parse_single_dep(&temp, r#"yaml = "0.2""#).unwrap_err();
    let msg = flat_err(&err);
    assert!(msg.contains("expected exactly three segments"), "{msg}");
}

#[test]
fn test_registry_dependency_rejects_range() {
    let temp = TempDir::new().unwrap();
    let err = parse_single_dep(&temp, r#"yaml = "~> 1.0""#).unwrap_err();
    let msg = flat_err(&err);
    assert!(msg.contains("version ranges are not supported"), "{msg}");
}

#[test]
fn test_registry_dependency_rejects_empty_version() {
    let temp = TempDir::new().unwrap();
    let err = parse_single_dep(&temp, r#"yaml = """#).unwrap_err();
    let msg = flat_err(&err);
    assert!(msg.contains("empty"), "{msg}");
}

#[test]
fn test_registry_dependency_rejects_non_numeric_version() {
    let temp = TempDir::new().unwrap();
    let err = parse_single_dep(&temp, r#"yaml = "1.2.beta""#).unwrap_err();
    let msg = flat_err(&err);
    assert!(msg.contains("not a non-negative integer"), "{msg}");
}

#[test]
fn test_registry_dependency_rejects_too_many_segments() {
    let temp = TempDir::new().unwrap();
    let err = parse_single_dep(&temp, r#"yaml = "1.2.3.4""#).unwrap_err();
    let msg = flat_err(&err);
    assert!(msg.contains("expected exactly three segments"), "{msg}");
}

#[test]
fn test_registry_dependency_rejects_leading_zero_version() {
    let temp = TempDir::new().unwrap();
    let err = parse_single_dep(&temp, r#"yaml = "01.0.0""#).unwrap_err();
    let msg = flat_err(&err);
    assert!(msg.contains("leading zero"), "{msg}");
}

#[test]
fn test_registry_dependency_accepts_zero_segments() {
    // A bare `0` segment is canonical and must still be accepted.
    let temp = TempDir::new().unwrap();
    let manifest = parse_single_dep(&temp, r#"yaml = "0.0.0""#).unwrap();
    assert!(matches!(
        manifest.dependencies["yaml"].source,
        DependencySource::Registry { .. }
    ));
}

#[test]
fn test_registry_dependency_rejects_invalid_package_name() {
    let temp = TempDir::new().unwrap();
    let err = parse_single_dep(&temp, r#"BadName = "1.2.3""#).unwrap_err();
    let msg = flat_err(&err);
    assert!(msg.contains("Invalid dependency name"), "{msg}");
}

#[test]
fn test_registry_dependency_error_suggests_git_alternative() {
    let temp = TempDir::new().unwrap();
    let err = parse_single_dep(&temp, r#"yaml = "~> 1.0""#).unwrap_err();
    let msg = flat_err(&err);
    assert!(msg.contains("git ="), "should suggest a git dep: {msg}");
}

// --- [registry] section ---

#[test]
fn test_parse_registry_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[registry]
url = "https://example.test/registry"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(
        manifest.registry.unwrap().url,
        "https://example.test/registry"
    );
}

/// A Windows path in `[registry] url` must be written as a TOML *literal*
/// string — in a basic string its backslashes are escapes, and `\U` in
/// particular is a unicode escape that fails to parse (the same hazard
/// `file://` handling has).
#[test]
fn test_registry_url_accepts_windows_path_as_literal_string() {
    let temp = TempDir::new().unwrap();
    let windows_path = r"C:\Users\RUNNER~1\AppData\Local\Temp\.tmpZ1Oqf7";
    let path = write_manifest(
        &temp,
        &format!(
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n\
             [registry]\nurl = '{windows_path}'\n"
        ),
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.registry.unwrap().url, windows_path);
}

#[test]
fn test_registry_section_absent_is_none() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n");
    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.registry.is_none());
}

#[test]
fn test_registry_section_rejects_unknown_fields() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[registry]
url = "https://example.test/registry"
mirror = "https://example.test/other"
"#,
    );

    assert!(parse_manifest_full(&path.join("beamtalk.toml")).is_err());
}

// --- [stubs] section ---

#[test]
fn test_parse_stubs_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[stubs]
path = "stubs/"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.stubs.unwrap().path, "stubs/");
}

#[test]
fn test_stubs_section_absent_is_none() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n");
    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.stubs.is_none());
}

#[test]
fn test_stubs_section_rejects_unknown_fields() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[stubs]
path = "stubs/"
extra = "not allowed"
"#,
    );

    assert!(parse_manifest_full(&path.join("beamtalk.toml")).is_err());
}

#[test]
fn test_parse_git_dependency_with_tag() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.dependencies.len(), 1);
    let dep = &manifest.dependencies["json"];
    assert_eq!(dep.name, "json");
    assert_eq!(
        dep.source,
        DependencySource::Git {
            url: "https://github.com/jamesc/beamtalk-json".to_string(),
            reference: GitReference::Tag("v1.0.0".to_string()),
        }
    );
}

#[test]
fn test_parse_git_dependency_with_branch() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
http = { git = "https://github.com/someone/beamtalk-http", branch = "main" }
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    let dep = &manifest.dependencies["http"];
    assert_eq!(
        dep.source,
        DependencySource::Git {
            url: "https://github.com/someone/beamtalk-http".to_string(),
            reference: GitReference::Branch("main".to_string()),
        }
    );
}

#[test]
fn test_parse_git_dependency_with_rev() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
my_tools = { git = "https://github.com/someone/beamtalk-tools", rev = "abc1234def" }
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    let dep = &manifest.dependencies["my_tools"];
    assert_eq!(
        dep.source,
        DependencySource::Git {
            url: "https://github.com/someone/beamtalk-tools".to_string(),
            reference: GitReference::Rev("abc1234def".to_string()),
        }
    );
}

#[test]
fn test_parse_multiple_dependencies() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
utils = { path = "../utils" }
json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }
http = { git = "https://github.com/someone/beamtalk-http", branch = "main" }
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.dependencies.len(), 3);
    assert!(manifest.dependencies.contains_key("utils"));
    assert!(manifest.dependencies.contains_key("json"));
    assert!(manifest.dependencies.contains_key("http"));
}

#[test]
fn test_parse_empty_dependencies_section() {
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

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.dependencies.is_empty());
}

#[test]
fn test_parse_no_dependencies_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.dependencies.is_empty());
}

#[test]
fn test_reject_dep_with_both_path_and_git() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
bad = { path = "../bad", git = "https://example.com/bad" }
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(err.contains("both"), "should mention both sources: {err}");
}

#[test]
fn test_reject_dep_with_no_source() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
empty = {}
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("must specify"),
        "should mention missing source: {err}"
    );
}

#[test]
fn test_reject_git_dep_without_ref() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
no_ref = { git = "https://example.com/repo" }
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(err.contains("no ref"), "should mention missing ref: {err}");
}

#[test]
fn test_reject_git_dep_with_multiple_refs() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
multi = { git = "https://example.com/repo", tag = "v1", branch = "main" }
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("multiple"),
        "should mention multiple refs: {err}"
    );
}

#[test]
fn test_reject_path_dep_with_git_ref() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
confused = { path = "../lib", tag = "v1.0" }
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("path dependency"),
        "should mention path dep conflict: {err}"
    );
}

#[test]
fn test_reject_invalid_dependency_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
MyLib = { path = "../my-lib" }
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("Invalid dependency name"),
        "should mention invalid name: {err}"
    );
}

#[test]
fn test_reject_reserved_dependency_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
stdlib = { path = "../stdlib" }
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("reserved"),
        "should mention reserved name: {err}"
    );
}

#[test]
fn test_reject_unknown_dependency_fields() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
oops = { path = "../lib", version = "1.0" }
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(
        result.is_err(),
        "should reject unknown fields in dependency entries"
    );
}

#[test]
fn test_dependency_display() {
    let path_dep = DependencySpec {
        name: "utils".to_string(),
        source: DependencySource::Path {
            path: PathBuf::from("../utils"),
        },
    };
    assert_eq!(format!("{path_dep}"), "utils (path: ../utils)");

    let git_dep = DependencySpec {
        name: "json".to_string(),
        source: DependencySource::Git {
            url: "https://github.com/jamesc/beamtalk-json".to_string(),
            reference: GitReference::Tag("v1.0.0".to_string()),
        },
    };
    assert_eq!(
        format!("{git_dep}"),
        "json (git: https://github.com/jamesc/beamtalk-json (tag: v1.0.0))"
    );
}

#[test]
fn test_parse_manifest_full_preserves_package_and_application() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"
description = "Test app"

[application]
supervisor = "AppSup"

[dependencies]
utils = { path = "../utils" }
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.package.name, "my_app");
    assert_eq!(manifest.package.version, "0.1.0");
    assert_eq!(manifest.package.description.as_deref(), Some("Test app"));
    assert!(manifest.application.is_some());
    assert_eq!(manifest.application.unwrap().supervisor, "AppSup");
    assert_eq!(manifest.dependencies.len(), 1);
}
