// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Transitive native-dependency detection (ADR 0072), class-header generation, and `validate_native_class_references` checks.

use super::*;
use crate::commands::manifest::{self, NativeDependencyMap};

// ---- Transitive native dependency detection tests (ADR 0072) ----

/// Helper: create a minimal beamtalk.toml with native deps in a temp dir.
fn write_dep_manifest(dir: &std::path::Path, name: &str, native_deps: &[(&str, &str)]) {
    let manifest_dir = dir.join(name);
    fs::create_dir_all(&manifest_dir).unwrap();
    let mut toml =
        format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n\n[native.dependencies]\n");
    for (dep_name, constraint) in native_deps {
        use std::fmt::Write;
        let _ = writeln!(toml, "{dep_name} = \"{constraint}\"");
    }
    fs::write(manifest_dir.join("beamtalk.toml"), toml).unwrap();
}

/// Helper: create a `ResolvedDependency` pointing at a temp dir.
fn make_resolved_dep(
    root: &std::path::Path,
    name: &str,
) -> super::super::super::deps::path::ResolvedDependency {
    super::super::super::deps::path::ResolvedDependency {
        name: name.to_string(),
        root: Utf8PathBuf::from_path_buf(root.join(name)).unwrap(),
        ebin_path: Utf8PathBuf::from_path_buf(root.join(name).join("ebin")).unwrap(),
        class_module_index: std::collections::HashMap::new(),
        class_infos: Vec::new(),
        protocol_infos: Vec::new(),
        alias_infos: Vec::new(),
        is_direct: true,
        via_chain: Vec::new(),
        stubs_dir: None,
    }
}

#[test]
fn test_aggregate_native_deps_from_transitive_dep() {
    let temp = TempDir::new().unwrap();
    write_dep_manifest(
        temp.path(),
        "http",
        &[("cowboy", "~> 2.12"), ("gun", "~> 2.1")],
    );

    let root_deps = NativeDependencyMap::new(); // root has NO native deps
    let resolved = [make_resolved_dep(temp.path(), "http")];

    let aggregated = aggregate_native_dependencies(&root_deps, &resolved);

    assert_eq!(
        aggregated.len(),
        2,
        "Should aggregate 2 native deps from transitive dep"
    );
    assert_eq!(aggregated["cowboy"].constraint, "~> 2.12");
    assert_eq!(aggregated["gun"].constraint, "~> 2.1");
}

#[test]
fn test_aggregate_native_deps_root_takes_precedence() {
    let temp = TempDir::new().unwrap();
    write_dep_manifest(temp.path(), "http", &[("cowboy", "~> 2.10")]);

    let mut root_deps = NativeDependencyMap::new();
    root_deps.insert(
        "cowboy".to_string(),
        manifest::NativeDependency {
            name: "cowboy".to_string(),
            constraint: "~> 2.12".to_string(),
        },
    );

    let resolved = [make_resolved_dep(temp.path(), "http")];
    let aggregated = aggregate_native_dependencies(&root_deps, &resolved);

    assert_eq!(aggregated.len(), 1);
    assert_eq!(
        aggregated["cowboy"].constraint, "~> 2.12",
        "Root constraint should take precedence over transitive dep"
    );
}

#[test]
fn test_aggregate_native_deps_dep_without_manifest() {
    let temp = TempDir::new().unwrap();
    // Create dep dir with NO beamtalk.toml
    fs::create_dir(temp.path().join("orphan")).unwrap();

    let root_deps = NativeDependencyMap::new();
    let resolved = [make_resolved_dep(temp.path(), "orphan")];
    let aggregated = aggregate_native_dependencies(&root_deps, &resolved);

    assert!(aggregated.is_empty(), "Should skip deps without manifests");
}

#[test]
fn test_has_native_deps_detects_transitive() {
    // Simulates the has_native_deps check from build() — verifies that
    // transitive deps with [native.dependencies] are detected even when
    // the root project has none.
    let temp = TempDir::new().unwrap();
    write_dep_manifest(temp.path(), "http", &[("gun", "~> 2.1")]);

    let resolved = [make_resolved_dep(temp.path(), "http")];

    let has_native_deps = resolved.iter().any(|dep| {
        let manifest_path = dep.root.join("beamtalk.toml");
        manifest_path
            .exists()
            .then(|| manifest::parse_manifest_full(&manifest_path).ok())
            .flatten()
            .is_some_and(|m| !m.native_dependencies.is_empty())
    });

    assert!(
        has_native_deps,
        "Should detect native deps from transitive dependency"
    );
}

#[test]
fn test_has_native_deps_false_when_no_native_anywhere() {
    let temp = TempDir::new().unwrap();
    // Dep with NO native.dependencies section
    let dep_dir = temp.path().join("utils");
    fs::create_dir(&dep_dir).unwrap();
    fs::write(
        dep_dir.join("beamtalk.toml"),
        "[package]\nname = \"utils\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    let resolved = [make_resolved_dep(temp.path(), "utils")];

    let has_native_deps = resolved.iter().any(|dep| {
        let manifest_path = dep.root.join("beamtalk.toml");
        manifest_path
            .exists()
            .then(|| manifest::parse_manifest_full(&manifest_path).ok())
            .flatten()
            .is_some_and(|m| !m.native_dependencies.is_empty())
    });

    assert!(
        !has_native_deps,
        "Should be false when no native deps anywhere"
    );
}

// ── Class module header tests ──────────────────────────────────

#[test]
fn test_generate_class_header_creates_hrl_with_macros() {
    let temp = TempDir::new().unwrap();
    let include_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let mut index = HashMap::new();
    index.insert(
        "HTTPResponse".to_string(),
        "bt@http@httpresponse".to_string(),
    );
    index.insert("HTTPRequest".to_string(), "bt@http@httprequest".to_string());

    generate_class_header(&include_dir, &index).unwrap();

    let hrl_path = include_dir.join("beamtalk_classes.hrl");
    assert!(hrl_path.exists(), "Header file should be created");

    let content = fs::read_to_string(&hrl_path).unwrap();
    assert!(
        content.contains("-define(BT_CLASS_MODULE_HTTPRequest, 'bt@http@httprequest')."),
        "Should contain HTTPRequest macro"
    );
    assert!(
        content.contains("-define(BT_CLASS_MODULE_HTTPResponse, 'bt@http@httpresponse')."),
        "Should contain HTTPResponse macro"
    );
    assert!(
        content.contains("-ifndef(BEAMTALK_CLASSES_HRL)."),
        "Should have include guard"
    );
}

#[test]
fn test_generate_class_header_sorted_deterministic() {
    let temp = TempDir::new().unwrap();
    let include_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let mut index = HashMap::new();
    index.insert("Zeta".to_string(), "bt@pkg@zeta".to_string());
    index.insert("Alpha".to_string(), "bt@pkg@alpha".to_string());
    index.insert("Middle".to_string(), "bt@pkg@middle".to_string());

    generate_class_header(&include_dir, &index).unwrap();

    let content = fs::read_to_string(include_dir.join("beamtalk_classes.hrl")).unwrap();
    let alpha_pos = content.find("BT_CLASS_MODULE_Alpha").unwrap();
    let middle_pos = content.find("BT_CLASS_MODULE_Middle").unwrap();
    let zeta_pos = content.find("BT_CLASS_MODULE_Zeta").unwrap();
    assert!(
        alpha_pos < middle_pos && middle_pos < zeta_pos,
        "Macros should be sorted alphabetically"
    );
}

#[test]
fn test_generate_class_header_empty_index() {
    let temp = TempDir::new().unwrap();
    let include_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let index = HashMap::new();
    generate_class_header(&include_dir, &index).unwrap();

    let content = fs::read_to_string(include_dir.join("beamtalk_classes.hrl")).unwrap();
    assert!(
        content.contains("-ifndef(BEAMTALK_CLASSES_HRL)."),
        "Should still have include guard even with empty index"
    );
    assert!(
        !content.contains("-define(BT_CLASS_MODULE_"),
        "Should have no macro definitions with empty index"
    );
}

#[test]
fn test_validate_native_class_references_warns_on_hardcoded() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let native_dir = project_root.join("native");
    fs::create_dir_all(&native_dir).unwrap();

    // Write a .erl file with a hardcoded reference
    fs::write(
        native_dir.join("test_mod.erl"),
        "-module(test_mod).\n\
             -export([f/0]).\n\
             f() -> 'bt@mypkg@myclass':hello().\n",
    )
    .unwrap();

    let mut index = HashMap::new();
    index.insert("MyClass".to_string(), "bt@mypkg@myclass".to_string());

    // Should not error (just warns to stderr)
    validate_native_class_references(&project_root, "mypkg", &index).unwrap();
}

#[test]
fn test_validate_native_class_references_skips_comments() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let native_dir = project_root.join("native");
    fs::create_dir_all(&native_dir).unwrap();

    // Write a .erl file with reference only in a comment
    fs::write(
        native_dir.join("test_mod.erl"),
        "-module(test_mod).\n\
             %% This references 'bt@mypkg@myclass' but only in a comment.\n",
    )
    .unwrap();

    let index = HashMap::new();
    // Should not warn (comment-only references are fine)
    validate_native_class_references(&project_root, "mypkg", &index).unwrap();
}

#[test]
fn test_validate_native_class_references_no_native_dir() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    // No native/ directory — should be a no-op
    let index = HashMap::new();
    validate_native_class_references(&project_root, "mypkg", &index).unwrap();
}
