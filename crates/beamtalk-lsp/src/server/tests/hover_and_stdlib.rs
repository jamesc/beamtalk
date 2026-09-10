// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Hover-content tests: class/instance-method `SymbolKind` disambiguation, stdlib source-dir resolution and policy notes, hover class-name extraction, and the beamtalk-stdlib URI helpers.

use super::*;
use beamtalk_language_service::HoverInfo;

/// A class-side method sharing a selector with an instance
/// method must map to a different LSP `SymbolKind` (and carry a
/// disambiguating `detail`) so VS Code's Outline, breadcrumbs, and Go
/// to Symbol can tell the two apart — otherwise both sides
/// map to `SymbolKind::METHOD` with no `detail`, and the two entries
/// are visually identical.
#[test]
fn to_lsp_symbol_distinguishes_class_and_instance_method_sharing_a_selector() {
    use beamtalk_core::source_analysis::Span;
    use beamtalk_language_service::DocumentSymbol;

    let source = "x".repeat(20);
    let instance_symbol = DocumentSymbol {
        name: "value".into(),
        kind: DocumentSymbolKind::Method,
        span: Span::new(0, 5),
        name_span: None,
        children: vec![],
    };
    let class_symbol = DocumentSymbol {
        name: "value".into(),
        kind: DocumentSymbolKind::ClassMethod,
        span: Span::new(6, 11),
        name_span: None,
        children: vec![],
    };

    let instance_lsp = to_lsp_symbol(instance_symbol, &source);
    let class_lsp = to_lsp_symbol(class_symbol, &source);

    assert_eq!(instance_lsp.name, "value");
    assert_eq!(class_lsp.name, "value");
    assert_eq!(instance_lsp.kind, SymbolKind::METHOD);
    assert_eq!(class_lsp.kind, SymbolKind::FUNCTION);
    assert_ne!(
        instance_lsp.kind, class_lsp.kind,
        "same-selector instance/class methods must map to distinct SymbolKinds"
    );
    assert_eq!(instance_lsp.detail, None);
    assert_eq!(class_lsp.detail.as_deref(), Some("class method"));
}

#[test]
fn configured_stdlib_source_dirs_rejects_relative_traversal_outside_root() {
    let temp = unique_temp_dir("beamtalk_lsp_stdlib_traversal");
    let project_root = temp.join("project");
    let outside = temp.join("outside");
    fs::create_dir_all(project_root.join("src")).expect("create project dirs");
    fs::create_dir_all(outside.join("lib")).expect("create outside dirs");

    let dirs = configured_stdlib_source_dirs(Some("../outside/lib"), &[project_root]);
    assert!(dirs.is_empty());

    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn configured_stdlib_source_dirs_accepts_in_root_absolute_path() {
    let temp = unique_temp_dir("beamtalk_lsp_stdlib_in_root");
    let project_root = temp.join("project");
    let stdlib = project_root.join("stdlib/src");
    fs::create_dir_all(&stdlib).expect("create stdlib dir");

    let dirs = configured_stdlib_source_dirs(
        Some(stdlib.to_str().expect("utf8 path")),
        std::slice::from_ref(&project_root),
    );
    assert_eq!(dirs.len(), 1);

    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn configured_stdlib_source_dirs_accepts_outside_root_absolute_path() {
    let temp = unique_temp_dir("beamtalk_lsp_stdlib_outside_root");
    let project_root = temp.join("project");
    let stdlib = temp.join("shared-stdlib");
    fs::create_dir_all(project_root.join("src")).expect("create project dir");
    fs::create_dir_all(&stdlib).expect("create stdlib dir");

    let dirs = configured_stdlib_source_dirs(
        Some(stdlib.to_str().expect("utf8 path")),
        std::slice::from_ref(&project_root),
    );
    assert_eq!(
        dirs,
        vec![fs::canonicalize(&stdlib).expect("canonical stdlib")]
    );

    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn configured_stdlib_source_dir_reads_initialize_option() {
    let params = InitializeParams {
        initialization_options: Some(serde_json::json!({
            "stdlibSourceDir": "stdlib/src"
        })),
        ..Default::default()
    };

    let configured = configured_stdlib_source_dir(&params);
    assert_eq!(configured.as_deref(), Some("stdlib/src"));
}

#[test]
fn extract_hover_class_name_from_class_hover() {
    let text = "Class: `Integer`";
    assert_eq!(extract_hover_class_name(text), Some("Integer"));
}

#[test]
fn extract_hover_class_name_from_package_qualified_hover() {
    // Class name extraction must work with package provenance suffix
    let text = "Class: `Parser` (from package `json`)";
    assert_eq!(extract_hover_class_name(text), Some("Parser"));
}

#[test]
fn extract_hover_class_name_from_resolved_method_hover() {
    let text = "```beamtalk\n+\n```\n\nResolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_";
    assert_eq!(extract_hover_class_name(text), Some("Integer"));
}

#[test]
fn stdlib_policy_note_for_sealed_class() {
    let service = SimpleLanguageService::new();
    let note = stdlib_hover_policy_note(&service, "Class: `Integer`");
    assert!(note.is_some());
    let note = note.unwrap();
    assert!(note.contains("Stdlib Profile"));
    assert!(note.contains("sealed"));
}

#[test]
fn stdlib_policy_note_ignores_non_builtin_classes() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("user.bt");
    service.update_file(
        file,
        "Object subclass: Counter\n  increment => 1".to_string(),
    );

    let note = stdlib_hover_policy_note(&service, "Class: `Counter`");
    assert!(note.is_none());
}

#[test]
fn stdlib_policy_note_for_abstract_class() {
    let service = SimpleLanguageService::new();
    let note = stdlib_hover_policy_note(&service, "Class: `Boolean`");
    assert!(note.is_some());
    let note = note.unwrap();
    assert!(note.contains("abstract"));
}

#[test]
fn stdlib_policy_note_marks_high_confidence_for_sealed_method_hover() {
    let service = SimpleLanguageService::new();
    let hover = "```beamtalk\n+\n```\n\nResolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_";
    let note = stdlib_hover_policy_note(&service, hover);
    assert!(note.is_some());
    let note = note.unwrap();
    assert!(note.contains("Confidence: high"));
    assert!(note.contains("sealed stdlib dispatch"));
}

#[test]
fn stdlib_policy_note_does_not_mark_high_confidence_for_class_hover() {
    let service = SimpleLanguageService::new();
    let note = stdlib_hover_policy_note(&service, "Class: `Integer`");
    assert!(note.is_some());
    assert!(!note.unwrap().contains("Confidence: high"));
}

#[test]
fn policy_note_works_for_resolved_method_hover_markdown() {
    let service = SimpleLanguageService::new();
    let hover = HoverInfo::new("```beamtalk\n+\n```", Span::new(0, 1)).with_documentation(
        "Resolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_",
    );
    let markdown = format!(
        "{}\n\n{}",
        hover.contents,
        format_hover_documentation(hover.documentation.as_deref().unwrap_or_default())
    );

    let note = stdlib_hover_policy_note(&service, &markdown);
    assert!(note.is_some());
    assert!(note.unwrap().contains("Integer"));
}

#[test]
fn configured_stdlib_falls_back_to_sysroot_when_none() {
    // When no explicit stdlibSourceDir is configured, configured_stdlib_source_dirs
    // falls back to sysroot auto-discovery. Assert against the actual sysroot
    // result so the test is deterministic whether or not the binary is installed.
    let expected: Vec<PathBuf> = sysroot_stdlib_source_dir().into_iter().collect();
    let dirs = configured_stdlib_source_dirs(None, &[PathBuf::from("/workspace/project")]);
    assert_eq!(dirs, expected);
}

#[test]
fn sysroot_stdlib_source_dir_smoke_test() {
    // Verify sysroot_stdlib_source_dir doesn't panic regardless of
    // the environment — the result depends on the test runner's
    // installation layout and may be Some or None.
    let _ = sysroot_stdlib_source_dir();
}

#[test]
fn path_to_stdlib_uri_produces_beamtalk_stdlib_scheme() {
    let path = Utf8PathBuf::from("/usr/share/beamtalk/stdlib/src/integer.bt");
    let uri = path_to_stdlib_uri(&path).expect("should produce URI");
    assert_eq!(uri.scheme(), "beamtalk-stdlib");
    assert_eq!(uri.path(), "/integer.bt");
}

#[test]
fn path_to_stdlib_uri_handles_nested_path() {
    let path = Utf8PathBuf::from("/some/deep/path/to/collection.bt");
    let uri = path_to_stdlib_uri(&path).expect("should produce URI");
    assert_eq!(uri.scheme(), "beamtalk-stdlib");
    assert_eq!(uri.path(), "/collection.bt");
}
