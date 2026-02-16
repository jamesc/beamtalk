// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package manifest parsing and validation.
//!
//! **DDD Context:** Build System
//!
//! Parses `beamtalk.toml` manifests that define a package's identity and metadata.
//! See ADR 0026 for the package definition and manifest format.

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use serde::Deserialize;
use std::fmt;
use std::fs;

/// The top-level manifest structure parsed from `beamtalk.toml`.
#[derive(Debug, Deserialize)]
pub struct Manifest {
    pub package: PackageManifest,
}

/// Package metadata from `beamtalk.toml`.
///
/// Required fields: `name`, `version`.
/// Optional fields: `description`, `licenses`.
#[derive(Debug, Deserialize)]
pub struct PackageManifest {
    pub name: String,
    pub version: String,
    #[serde(default)]
    #[allow(dead_code)] // Read by future phases (module naming, .app generation)
    pub description: Option<String>,
    #[serde(default)]
    #[allow(dead_code)] // Read by future phases (.app generation, Hex publishing)
    pub licenses: Option<Vec<String>>,
}

/// Parse a `beamtalk.toml` manifest file.
///
/// Returns the parsed manifest, or an error if the file cannot be read or
/// contains invalid TOML / missing required fields.
pub fn parse_manifest(path: &Utf8Path) -> Result<PackageManifest> {
    let content = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read manifest '{path}'"))?;

    let manifest: Manifest = toml::from_str(&content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to parse manifest '{path}'"))?;

    if let Err(e) = validate_package_name(&manifest.package.name) {
        miette::bail!("{}", format_name_error(&manifest.package.name, &e));
    }

    Ok(manifest.package)
}

/// Look for `beamtalk.toml` in the given directory and parse it if found.
///
/// Returns `None` if no manifest file exists. Returns an error if the file
/// exists but is malformed.
pub fn find_manifest(project_root: &Utf8Path) -> Result<Option<PackageManifest>> {
    let manifest_path = project_root.join("beamtalk.toml");
    if manifest_path
        .try_exists()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat manifest '{manifest_path}'"))?
    {
        parse_manifest(&manifest_path).map(Some)
    } else {
        Ok(None)
    }
}

/// Format a package name validation error with an optional suggestion.
pub fn format_name_error(name: &str, error: &PackageNameError) -> String {
    let msg = format!("Invalid package name '{name}': {error}");
    match suggest_package_name(name) {
        Some(s) => format!("{msg} (try '{s}')"),
        None => msg,
    }
}

/// Reserved beamtalk package names that conflict with core components.
const RESERVED_NAMES: &[&str] = &[
    "beamtalk",
    "stdlib",
    "kernel",
    "runtime",
    "workspace",
    "compiler",
];

/// Erlang standard application names that would conflict with OTP.
const ERLANG_APPS: &[&str] = &[
    "common_test",
    "crypto",
    "dialyzer",
    "eunit",
    "inets",
    "mnesia",
    "observer",
    "public_key",
    "sasl",
    "ssh",
    "ssl",
    "tools",
    "xmerl",
];

/// Error returned when a package name is invalid.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageNameError {
    Empty,
    TooLong(usize),
    StartsWithNonLetter(char),
    InvalidCharacter(char),
    ReservedName,
    ErlangAppConflict,
}

impl fmt::Display for PackageNameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "package name must not be empty"),
            Self::TooLong(len) => {
                write!(f, "package name is too long ({len} chars, max 64)")
            }
            Self::StartsWithNonLetter(c) => {
                write!(
                    f,
                    "package name must start with a lowercase letter, found '{c}'"
                )
            }
            Self::InvalidCharacter(c) => {
                write!(
                    f,
                    "package name must contain only lowercase letters, digits, and underscores — found '{c}'"
                )
            }
            Self::ReservedName => {
                write!(
                    f,
                    "is a reserved package name (conflicts with Beamtalk core)"
                )
            }
            Self::ErlangAppConflict => {
                write!(f, "conflicts with an Erlang/OTP standard application")
            }
        }
    }
}

/// Validate a package name according to ADR 0026 §8.
///
/// Package names must be:
/// - Lowercase ASCII letters, digits, and underscores only
/// - Start with a letter
/// - 1–64 characters
/// - Not a reserved Beamtalk name
/// - Not an Erlang standard application name
pub fn validate_package_name(name: &str) -> Result<(), PackageNameError> {
    if name.is_empty() {
        return Err(PackageNameError::Empty);
    }

    if name.len() > 64 {
        return Err(PackageNameError::TooLong(name.len()));
    }

    let first = name.chars().next().unwrap();
    if !first.is_ascii_lowercase() {
        return Err(PackageNameError::StartsWithNonLetter(first));
    }

    for c in name.chars() {
        if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_') {
            return Err(PackageNameError::InvalidCharacter(c));
        }
    }

    if RESERVED_NAMES.contains(&name) {
        return Err(PackageNameError::ReservedName);
    }

    if ERLANG_APPS.contains(&name) {
        return Err(PackageNameError::ErlangAppConflict);
    }

    Ok(())
}

/// Suggest a corrected package name from an invalid one.
///
/// Converts CamelCase to `snake_case`, replaces non-alphanumeric chars with
/// underscores, strips leading non-letter chars, and collapses consecutive
/// underscores. Returns `None` if no reasonable suggestion can be made.
pub fn suggest_package_name(name: &str) -> Option<String> {
    // Insert underscores before uppercase letters (CamelCase → snake_case)
    let mut expanded = String::new();
    let mut prev_char: Option<char> = None;
    for c in name.chars() {
        if c.is_ascii_uppercase() {
            if let Some(p) = prev_char {
                if p.is_ascii_lowercase() || p.is_ascii_digit() {
                    expanded.push('_');
                }
            }
        }
        expanded.push(c);
        prev_char = Some(c);
    }

    let suggested: String = expanded
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '_'
            }
        })
        .collect();

    // Strip leading non-letter characters
    let suggested = suggested.trim_start_matches(|c: char| !c.is_ascii_lowercase());

    // Collapse consecutive underscores and trim trailing ones
    let mut result = String::new();
    let mut prev_underscore = false;
    for c in suggested.chars() {
        if c == '_' {
            if !prev_underscore {
                result.push(c);
            }
            prev_underscore = true;
        } else {
            result.push(c);
            prev_underscore = false;
        }
    }
    let result = result.trim_end_matches('_').to_string();

    if result.is_empty() || result == name {
        return None;
    }

    // Only suggest if the suggestion is actually valid
    if validate_package_name(&result).is_ok() {
        Some(result)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use camino::Utf8PathBuf;
    use tempfile::TempDir;

    fn write_manifest(dir: &TempDir, content: &str) -> Utf8PathBuf {
        let path = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        fs::write(path.join("beamtalk.toml"), content).unwrap();
        path
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

    // --- Package name validation tests ---

    #[test]
    fn test_valid_package_names() {
        let valid = [
            "a",
            "my_app",
            "json_parser",
            "web_utils",
            "counter",
            "x123",
            "app_v2",
            "a_b_c_d",
        ];
        for name in valid {
            assert!(
                validate_package_name(name).is_ok(),
                "expected '{name}' to be valid"
            );
        }
    }

    #[test]
    fn test_empty_name() {
        assert_eq!(validate_package_name(""), Err(PackageNameError::Empty));
    }

    #[test]
    fn test_too_long_name() {
        let long_name = "a".repeat(65);
        assert_eq!(
            validate_package_name(&long_name),
            Err(PackageNameError::TooLong(65))
        );
        // Exactly 64 is fine
        let max_name = "a".repeat(64);
        assert!(validate_package_name(&max_name).is_ok());
    }

    #[test]
    fn test_starts_with_digit() {
        assert_eq!(
            validate_package_name("123app"),
            Err(PackageNameError::StartsWithNonLetter('1'))
        );
    }

    #[test]
    fn test_starts_with_underscore() {
        assert_eq!(
            validate_package_name("_private"),
            Err(PackageNameError::StartsWithNonLetter('_'))
        );
    }

    #[test]
    fn test_uppercase_letters() {
        assert_eq!(
            validate_package_name("MyApp"),
            Err(PackageNameError::StartsWithNonLetter('M'))
        );
    }

    #[test]
    fn test_mixed_case() {
        assert_eq!(
            validate_package_name("myApp"),
            Err(PackageNameError::InvalidCharacter('A'))
        );
    }

    #[test]
    fn test_dashes() {
        assert_eq!(
            validate_package_name("my-app"),
            Err(PackageNameError::InvalidCharacter('-'))
        );
    }

    #[test]
    fn test_dots() {
        assert_eq!(
            validate_package_name("my.app"),
            Err(PackageNameError::InvalidCharacter('.'))
        );
    }

    #[test]
    fn test_spaces() {
        assert_eq!(
            validate_package_name("my app"),
            Err(PackageNameError::InvalidCharacter(' '))
        );
    }

    #[test]
    fn test_reserved_names() {
        for name in RESERVED_NAMES {
            assert_eq!(
                validate_package_name(name),
                Err(PackageNameError::ReservedName),
                "expected '{name}' to be rejected as reserved"
            );
        }
    }

    #[test]
    fn test_erlang_app_names() {
        for name in ERLANG_APPS {
            assert_eq!(
                validate_package_name(name),
                Err(PackageNameError::ErlangAppConflict),
                "expected '{name}' to be rejected as Erlang app"
            );
        }
    }

    // --- Suggestion tests ---

    #[test]
    fn test_suggest_camel_case() {
        assert_eq!(suggest_package_name("MyApp"), Some("my_app".to_string()));
    }

    #[test]
    fn test_suggest_dashes() {
        assert_eq!(
            suggest_package_name("my-cool-app"),
            Some("my_cool_app".to_string())
        );
    }

    #[test]
    fn test_suggest_leading_digits() {
        assert_eq!(suggest_package_name("123app"), Some("app".to_string()));
    }

    #[test]
    fn test_suggest_mixed() {
        assert_eq!(
            suggest_package_name("My-Cool_App123"),
            Some("my_cool_app123".to_string())
        );
    }

    #[test]
    fn test_suggest_reserved_returns_none() {
        // Can't suggest a fix for "stdlib" → "stdlib"
        assert_eq!(suggest_package_name("stdlib"), None);
    }

    #[test]
    fn test_suggest_no_letters_returns_none() {
        assert_eq!(suggest_package_name("123"), None);
    }

    #[test]
    fn test_suggest_already_valid_returns_none() {
        assert_eq!(suggest_package_name("my_app"), None);
    }

    #[test]
    fn test_suggest_unicode_does_not_panic() {
        // Non-ASCII input should not panic despite char/byte index mismatch
        let _ = suggest_package_name("café");
        let _ = suggest_package_name("über_app");
        let _ = suggest_package_name("日本語");
    }
}
