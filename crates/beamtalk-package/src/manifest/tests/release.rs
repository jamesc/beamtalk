// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `manifest` tests: release.

use super::*;

// --- [release] parsing (ADR 0125 §1.2) ---

#[test]
fn test_parse_release_config_absent_returns_defaults() {
    let config = parse_release_config(None).unwrap();
    assert_eq!(config, ReleaseConfig::default());
    assert!(config.name.is_none());
    assert!(config.apps.is_empty());
    assert!(config.include_erts);
    assert!(!config.console);
    assert_eq!(config.bind, "127.0.0.1");
    assert_eq!(config.sys_config, "config/sys.config");
    assert_eq!(config.vm_args, "config/vm.args");
    assert!(!config.strip_beams);
    assert!(!config.include_compiler);
}

#[test]
fn test_parse_release_config_every_key() {
    let toml_str = r#"
name = "orders"
apps = ["gproc"]
include-erts = false
console = true
bind = "0.0.0.0"
sys-config = "config/prod.sys.config"
vm-args = "config/prod.vm.args"
strip-beams = true
include-compiler = false
"#;
    let value: toml::Value = toml::from_str(toml_str).unwrap();
    let config = parse_release_config(Some(&value)).unwrap();
    assert_eq!(config.name.as_deref(), Some("orders"));
    assert_eq!(config.apps, vec!["gproc".to_string()]);
    assert!(!config.include_erts);
    assert!(config.console);
    assert_eq!(config.bind, "0.0.0.0");
    assert_eq!(config.sys_config, "config/prod.sys.config");
    assert_eq!(config.vm_args, "config/prod.vm.args");
    assert!(config.strip_beams);
    assert!(!config.include_compiler);
}

#[test]
fn test_parse_release_config_rejects_version_key() {
    let value: toml::Value = toml::from_str(r#"version = "1.4.0""#).unwrap();
    let err = parse_release_config(Some(&value)).unwrap_err();
    assert_eq!(err, ReleaseConfigError::VersionKeyNotAllowed);
    assert!(err.to_string().contains("must not declare 'version'"));
}

#[test]
fn test_parse_release_config_rejects_strip_beams_with_include_compiler() {
    let value: toml::Value =
        toml::from_str("strip-beams = true\ninclude-compiler = true\n").unwrap();
    let err = parse_release_config(Some(&value)).unwrap_err();
    assert_eq!(err, ReleaseConfigError::StripBeamsWithIncludeCompiler);
}

#[test]
fn test_parse_release_config_rejects_unknown_key() {
    let value: toml::Value = toml::from_str(r#"typo-key = "x""#).unwrap();
    let err = parse_release_config(Some(&value)).unwrap_err();
    assert_eq!(
        err,
        ReleaseConfigError::UnknownKey {
            key: "typo-key".to_string()
        }
    );
}

#[test]
fn test_parse_release_config_rejects_wrong_type() {
    let value: toml::Value = toml::from_str("console = \"yes\"").unwrap();
    let err = parse_release_config(Some(&value)).unwrap_err();
    assert_eq!(
        err,
        ReleaseConfigError::WrongType {
            key: "console",
            expected: "boolean",
            found: "string"
        }
    );
}

#[test]
fn test_parse_release_config_rejects_non_string_apps_entry() {
    let value: toml::Value = toml::from_str("apps = [1, 2]").unwrap();
    let err = parse_release_config(Some(&value)).unwrap_err();
    assert_eq!(
        err,
        ReleaseConfigError::AppsEntryWrongType { found: "integer" }
    );
}

#[test]
fn test_parse_release_config_rejects_non_table() {
    let value = toml::Value::String("nope".to_string());
    let err = parse_release_config(Some(&value)).unwrap_err();
    assert_eq!(err, ReleaseConfigError::NotATable { found: "string" });
}

#[test]
fn test_parse_manifest_full_surfaces_release_error_with_hint() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[release]
version = "1.4.0"
"#,
    );

    let result = parse_manifest_full(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(err.contains("must not declare 'version'"), "got: {err}");
    assert!(
        err.contains("[package] version"),
        "hint should point at [package] version: {err}"
    );
}

#[test]
fn test_parse_manifest_full_defaults_release_config_when_absent() {
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
    assert_eq!(manifest.release, ReleaseConfig::default());
}
