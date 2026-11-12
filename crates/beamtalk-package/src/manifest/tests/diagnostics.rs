// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `manifest` tests: diagnostics.

use super::*;

// --- ADR 0100 Rule 3 `[diagnostics]` table tests ---

#[test]
fn test_parse_no_diagnostics_section() {
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
    assert!(
        manifest.diagnostics.is_empty(),
        "absent [diagnostics] must preserve today's Rule 1 defaults (empty table)"
    );
}

#[test]
fn test_parse_empty_diagnostics_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[diagnostics]
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.diagnostics.is_empty());
}

#[test]
fn test_parse_diagnostics_table_all_categories() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[diagnostics]
dnu = "hint"
type = "hint"
unused = "warn"
empty-body = "error"
lint = "off"
dead-assignment = "warn"
extension-conflict = "error"
deprecation = "warn"
actor-new = "error"
visibility = "error"
unresolved-class = "warn"
unresolved-ffi = "warn"
arity-mismatch = "warn"
shadowed-class = "warn"
type-annotation = "hint"
inheritance = "error"
sendability = "hint"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.diagnostics.len(), 17);
    assert_eq!(
        manifest.diagnostics[&DiagnosticCategory::Dnu],
        DiagnosticSeverityOverride::Hint
    );
    assert_eq!(
        manifest.diagnostics[&DiagnosticCategory::UnresolvedClass],
        DiagnosticSeverityOverride::Warn
    );
    assert_eq!(
        manifest.diagnostics[&DiagnosticCategory::Lint],
        DiagnosticSeverityOverride::Off
    );
    assert_eq!(
        manifest.diagnostics[&DiagnosticCategory::Inheritance],
        DiagnosticSeverityOverride::Error
    );
}

#[test]
fn test_reject_diagnostics_unknown_category() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[diagnostics]
not-a-category = "warn"
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("unknown category"),
        "should mention unknown category: {err}"
    );
}

#[test]
fn test_reject_diagnostics_invalid_severity() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[diagnostics]
dnu = "critical"
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("invalid diagnostic severity"),
        "should mention invalid severity: {err}"
    );
}

#[test]
fn test_reject_diagnostics_non_string_value() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[diagnostics]
dnu = 42
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("severity string"),
        "should mention expected string type: {err}"
    );
}

#[test]
fn test_diagnostics_table_does_not_affect_other_sections() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[diagnostics]
dnu = "error"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.dependencies.is_empty());
    assert!(manifest.native_dependencies.is_empty());
    assert_eq!(manifest.diagnostics.len(), 1);
}
