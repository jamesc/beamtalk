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
use std::fs;

/// The top-level manifest structure parsed from `beamtalk.toml`.
#[derive(Debug, Deserialize)]
pub struct Manifest {
    /// The `[package]` section of the manifest.
    pub package: PackageManifest,
}

/// Package metadata from `beamtalk.toml`.
///
/// Required fields: `name`, `version`.
/// Optional fields: `description`, `licenses`.
#[derive(Debug, Deserialize)]
pub struct PackageManifest {
    /// Package name (used for module naming and OTP application ID).
    pub name: String,
    /// Package version string (e.g. `"0.1.0"`).
    pub version: String,
    /// Optional human-readable package description.
    #[serde(default)]
    #[allow(dead_code)] // Read by future phases (module naming, .app generation)
    pub description: Option<String>,
    /// Optional SPDX license identifiers (e.g. `["Apache-2.0"]`).
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
}
