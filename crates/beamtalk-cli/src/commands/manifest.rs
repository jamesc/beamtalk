// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package manifest parsing and validation.
//!
//! **DDD Context:** Build System
//!
//! Parses `beamtalk.toml` manifests that define a package's identity and metadata.
//! See ADR 0026 for the package definition and manifest format.

use beamtalk_core::compilation::{DependencyMap, DependencySource, DependencySpec, GitReference};
use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use serde::Deserialize;
use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::path::PathBuf;

/// A map of native (hex.pm) dependency names to their version constraints.
pub type NativeDependencyMap = BTreeMap<String, NativeDependency>;

/// A native (hex.pm) dependency with a version constraint.
///
/// Declared in the `[native.dependencies]` section of `beamtalk.toml`.
/// Version constraints follow hex.pm conventions:
/// - `"~> 2.1"` — `>= 2.1.0 and < 3.0.0`
/// - `"~> 2.1.0"` — `>= 2.1.0 and < 2.2.0`
/// - `">= 1.0.0 and < 2.0.0"` — explicit range
/// - `"2.12.0"` — exact version
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeDependency {
    /// The hex package name.
    pub name: String,
    /// The version constraint string (passed through to rebar.config verbatim).
    pub constraint: String,
}

/// The top-level manifest structure parsed from `beamtalk.toml`.
#[derive(Debug, Deserialize)]
pub struct Manifest {
    /// The `[package]` section of the manifest.
    pub package: PackageManifest,
    /// The optional `[application]` section of the manifest.
    #[serde(default)]
    pub application: Option<ApplicationConfig>,
    /// The optional `[dependencies]` section — stored as raw TOML for lazy parsing.
    ///
    /// Use [`parse_manifest_full`] to get validated [`DependencySpec`] values.
    #[serde(default)]
    dependencies: Option<toml::Value>,
    /// The optional `[native]` section containing `dependencies` for hex packages.
    #[serde(default)]
    native: Option<NativeSection>,
}

/// The `[native]` section of `beamtalk.toml`.
///
/// Contains an optional `dependencies` sub-table declaring hex.pm package
/// dependencies for native Erlang code.
#[derive(Debug, Deserialize, Default)]
struct NativeSection {
    /// The `[native.dependencies]` table — hex package name to version constraint.
    #[serde(default)]
    dependencies: Option<toml::Value>,
}

/// A dependency entry as it appears in `beamtalk.toml`.
///
/// Supports two forms:
/// - `name = { path = "..." }` — local path dependency
/// - `name = { git = "...", tag/branch/rev = "..." }` — git dependency
#[derive(Debug, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
#[allow(dead_code)] // Constructed by serde deserialization
pub struct TomlDependency {
    /// Local filesystem path to the dependency.
    pub path: Option<String>,
    /// Git repository URL.
    pub git: Option<String>,
    /// Git tag to check out.
    pub tag: Option<String>,
    /// Git branch to check out.
    pub branch: Option<String>,
    /// Git commit SHA to check out.
    pub rev: Option<String>,
}

/// OTP application configuration from the `[application]` section of `beamtalk.toml`.
///
/// When present, `beamtalk build` generates an OTP application callback module
/// (`beamtalk_{appname}_app.erl`) and includes `{mod, ...}` in the `.app` file.
/// `beamtalk run` starts the package as an OTP application rather than calling
/// an imperative start method.
#[derive(Debug, Deserialize, Clone)]
pub struct ApplicationConfig {
    /// Beamtalk class name of the root `Supervisor subclass:` for this OTP application
    /// (e.g. `"AppSup"`). The generated `start/2` callback calls its `start_link`.
    pub supervisor: String,
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
    pub description: Option<String>,
    /// Optional SPDX license identifiers (e.g. `["Apache-2.0"]`).
    #[serde(default)]
    #[allow(dead_code)] // Read by future phases (Hex publishing)
    pub licenses: Option<Vec<String>>,
    /// When `true`, using a class from a transitive dependency is a compile
    /// error instead of a warning (ADR 0070 Phase 3).
    /// Defaults to `false` (warning only).
    #[serde(default, rename = "strict-deps")]
    pub strict_deps: bool,
}

/// Validate and convert a single TOML dependency entry into a [`DependencySpec`].
///
/// Returns a helpful error message if the entry is malformed.
fn validate_dependency(name: &str, dep: &TomlDependency) -> Result<DependencySpec> {
    // Validate the dependency name uses package naming rules
    if let Err(e) = validate_package_name(name) {
        miette::bail!(
            "Invalid dependency name '{name}': {}",
            format_name_error(name, &e)
        );
    }

    let has_path = dep.path.is_some();
    let has_git = dep.git.is_some();
    let has_git_ref = dep.tag.is_some() || dep.branch.is_some() || dep.rev.is_some();

    // Must have exactly one source type
    if has_path && has_git {
        miette::bail!("Dependency '{name}' specifies both 'path' and 'git' — use one or the other");
    }

    if !has_path && !has_git {
        miette::bail!("Dependency '{name}' must specify either 'path' or 'git' as a source");
    }

    // Path deps must not have git ref fields
    if has_path && has_git_ref {
        miette::bail!(
            "Dependency '{name}' is a path dependency — 'tag', 'branch', and 'rev' are only valid for git dependencies"
        );
    }

    let source = if let Some(path) = &dep.path {
        DependencySource::Path {
            path: PathBuf::from(path),
        }
    } else {
        let url = dep.git.as_ref().unwrap().clone();

        // Count git ref specifiers — at most one allowed
        let ref_count = u8::from(dep.tag.is_some())
            + u8::from(dep.branch.is_some())
            + u8::from(dep.rev.is_some());
        if ref_count > 1 {
            miette::bail!(
                "Dependency '{name}' specifies multiple git references — use exactly one of 'tag', 'branch', or 'rev'"
            );
        }

        let reference = if let Some(tag) = &dep.tag {
            GitReference::Tag(tag.clone())
        } else if let Some(branch) = &dep.branch {
            GitReference::Branch(branch.clone())
        } else if let Some(rev) = &dep.rev {
            GitReference::Rev(rev.clone())
        } else {
            miette::bail!(
                "Dependency '{name}' is a git dependency but has no ref — specify 'tag', 'branch', or 'rev'"
            );
        };

        DependencySource::Git { url, reference }
    };

    Ok(DependencySpec {
        name: name.to_string(),
        source,
    })
}

/// Parse and validate the `[dependencies]` section of a manifest.
///
/// Returns an empty map if the section is missing or empty.
fn parse_dependencies(deps: Option<&toml::Value>) -> Result<DependencyMap> {
    let Some(raw_value) = deps else {
        return Ok(BTreeMap::new());
    };

    // The [dependencies] section must be a TOML table
    let table = raw_value.as_table().ok_or_else(|| {
        miette::miette!(
            "[dependencies] must be a table, not a {}",
            value_type_name(raw_value)
        )
    })?;

    if table.is_empty() {
        return Ok(BTreeMap::new());
    }

    let mut result = DependencyMap::new();
    for (name, value) in table {
        // Each dependency entry must be an inline table like { path = "..." }
        let dep: TomlDependency = value
            .clone()
            .try_into()
            .into_diagnostic()
            .wrap_err_with(|| format!("Dependency '{name}' has an invalid format — expected a table with 'path' or 'git' keys"))?;

        let spec = validate_dependency(name, &dep)?;
        result.insert(name.clone(), spec);
    }
    Ok(result)
}

/// Validate a hex.pm version constraint string.
///
/// Accepted forms:
/// - Exact version: `"2.12.0"`, `"1.0"`, `"3"`
/// - Pessimistic (tilde-arrow): `"~> 2.1"`, `"~> 2.1.0"`
/// - Comparison operators: `">= 1.0.0"`, `"> 1.0"`, `"< 2.0.0"`, `"<= 3.0"`
/// - Compound constraints: `">= 1.0.0 and < 2.0.0"`
///
/// Version segments must be non-negative integers separated by dots (max 3 segments).
fn validate_hex_constraint(constraint: &str) -> Result<(), String> {
    let trimmed = constraint.trim();
    if trimmed.is_empty() {
        return Err("version constraint must not be empty".to_string());
    }

    // Check for compound constraints with "and"
    if trimmed.contains(" and ") {
        let parts: Vec<&str> = trimmed.split(" and ").collect();
        if parts.len() != 2 {
            return Err(
                "compound constraints must have exactly two parts separated by 'and'".to_string(),
            );
        }
        for part in &parts {
            validate_single_constraint(part.trim())?;
            // Compound parts must use comparison operators, not ~> or exact versions
            let p = part.trim();
            if p.starts_with("~>") {
                return Err(
                    "'~>' (pessimistic) constraints cannot be used in compound expressions"
                        .to_string(),
                );
            }
            if !p.starts_with(">=")
                && !p.starts_with('>')
                && !p.starts_with("<=")
                && !p.starts_with('<')
            {
                return Err(
                    "compound constraints must use comparison operators (>=, >, <, <=)".to_string(),
                );
            }
        }
        return Ok(());
    }

    validate_single_constraint(trimmed)
}

/// Validate a single (non-compound) version constraint.
fn validate_single_constraint(s: &str) -> Result<(), String> {
    if let Some(rest) = s.strip_prefix("~>") {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'~>' must be followed by a version number".to_string());
        }
        validate_version_string(version)?;
        // ~> requires at least 2 segments (e.g. ~> 2.1, not ~> 2)
        let segments: Vec<&str> = version.split('.').collect();
        if segments.len() < 2 {
            return Err("'~>' requires at least a major.minor version (e.g. '~> 2.1')".to_string());
        }
        Ok(())
    } else if let Some(rest) = s.strip_prefix(">=") {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'>=' must be followed by a version number".to_string());
        }
        validate_version_string(version)
    } else if let Some(rest) = s.strip_prefix("<=") {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'<=' must be followed by a version number".to_string());
        }
        validate_version_string(version)
    } else if let Some(rest) = s.strip_prefix('>') {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'>' must be followed by a version number".to_string());
        }
        validate_version_string(version)
    } else if let Some(rest) = s.strip_prefix('<') {
        let version = rest.trim();
        if version.is_empty() {
            return Err("'<' must be followed by a version number".to_string());
        }
        validate_version_string(version)
    } else {
        // Must be an exact version
        validate_version_string(s)
    }
}

/// Validate a bare version string like `"2.1.0"`, `"2.1"`, or `"2"`.
///
/// Segments must be non-negative integers, 1–3 segments allowed.
fn validate_version_string(version: &str) -> Result<(), String> {
    let trimmed = version.trim();
    if trimmed.is_empty() {
        return Err("version must not be empty".to_string());
    }

    let segments: Vec<&str> = trimmed.split('.').collect();
    if segments.len() > 3 {
        return Err(format!(
            "version '{trimmed}' has too many segments (max 3: major.minor.patch)"
        ));
    }

    for (i, segment) in segments.iter().enumerate() {
        if segment.is_empty() {
            return Err(format!(
                "version '{trimmed}' has an empty segment at position {i}"
            ));
        }
        if segment.parse::<u64>().is_err() {
            return Err(format!(
                "version '{trimmed}' has non-numeric segment '{segment}'"
            ));
        }
    }

    Ok(())
}

/// Parse and validate the `[native.dependencies]` section of a manifest.
///
/// Returns an empty map if the section is missing or empty.
fn parse_native_dependencies(native: Option<&NativeSection>) -> Result<NativeDependencyMap> {
    let Some(section) = native else {
        return Ok(BTreeMap::new());
    };

    let Some(raw_value) = &section.dependencies else {
        return Ok(BTreeMap::new());
    };

    let table = raw_value.as_table().ok_or_else(|| {
        miette::miette!(
            "[native.dependencies] must be a table, not a {}",
            value_type_name(raw_value)
        )
    })?;

    if table.is_empty() {
        return Ok(BTreeMap::new());
    }

    let mut result = NativeDependencyMap::new();
    for (name, value) in table {
        // Validate the package name: hex package names use lowercase + underscores
        if name.is_empty() {
            miette::bail!("Native dependency name must not be empty");
        }
        if !name.starts_with(|c: char| c.is_ascii_lowercase()) {
            miette::bail!("Native dependency '{name}' must start with a lowercase letter");
        }
        for c in name.chars() {
            if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_') {
                miette::bail!(
                    "Native dependency '{name}' contains invalid character '{c}' — \
                     hex package names must contain only lowercase letters, digits, and underscores"
                );
            }
        }

        // Value must be a string (the version constraint)
        let constraint = value.as_str().ok_or_else(|| {
            miette::miette!(
                "Native dependency '{name}' must be a version constraint string, not a {}",
                value_type_name(value)
            )
        })?;

        // Validate the constraint syntax (use trimmed value)
        let normalized = constraint.trim();
        if let Err(msg) = validate_hex_constraint(normalized) {
            miette::bail!(
                "Native dependency '{name}' has an invalid version constraint \
                 '{constraint}': {msg}"
            );
        }

        result.insert(
            name.clone(),
            NativeDependency {
                name: name.clone(),
                constraint: normalized.to_string(),
            },
        );
    }

    Ok(result)
}

/// Return a human-readable TOML type name for error messages.
fn value_type_name(value: &toml::Value) -> &'static str {
    match value {
        toml::Value::String(_) => "string",
        toml::Value::Integer(_) => "integer",
        toml::Value::Float(_) => "float",
        toml::Value::Boolean(_) => "boolean",
        toml::Value::Datetime(_) => "datetime",
        toml::Value::Array(_) => "array",
        toml::Value::Table(_) => "table",
    }
}

/// Read and deserialize a `beamtalk.toml` file into a raw [`Manifest`].
///
/// Does **not** validate the package name — use [`parse_manifest`] for that.
fn parse_manifest_raw(path: &Utf8Path) -> Result<Manifest> {
    let content = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read manifest '{path}'"))?;

    toml::from_str(&content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to parse manifest '{path}'"))
}

/// A fully parsed and validated manifest with package metadata and dependencies.
#[derive(Debug)]
pub struct ParsedManifest {
    /// The package metadata.
    pub package: PackageManifest,
    /// The optional application configuration.
    #[allow(dead_code)] // Used by tests; will be used for dep application support
    pub application: Option<ApplicationConfig>,
    /// The parsed and validated dependencies (empty if `[dependencies]` is absent).
    pub dependencies: DependencyMap,
    /// The parsed and validated native (hex) dependencies (empty if
    /// `[native.dependencies]` is absent).
    #[allow(dead_code)] // Used by tests; will be used for rebar.config generation
    pub native_dependencies: NativeDependencyMap,
}

/// Parse a `beamtalk.toml` manifest file.
///
/// Returns the parsed manifest, or an error if the file cannot be read or
/// contains invalid TOML / missing required fields.
pub fn parse_manifest(path: &Utf8Path) -> Result<PackageManifest> {
    let manifest = parse_manifest_raw(path)?;

    if let Err(e) = validate_package_name(&manifest.package.name) {
        miette::bail!("{}", format_name_error(&manifest.package.name, &e));
    }

    Ok(manifest.package)
}

/// Parse a `beamtalk.toml` manifest file including dependencies.
///
/// Returns the fully parsed manifest with validated package metadata and
/// dependency specifications. Returns an error if the file cannot be read,
/// contains invalid TOML, or has malformed dependency entries.
pub fn parse_manifest_full(path: &Utf8Path) -> Result<ParsedManifest> {
    let manifest = parse_manifest_raw(path)?;

    if let Err(e) = validate_package_name(&manifest.package.name) {
        miette::bail!("{}", format_name_error(&manifest.package.name, &e));
    }

    let dependencies = parse_dependencies(manifest.dependencies.as_ref())
        .wrap_err_with(|| format!("Failed to parse [dependencies] in '{path}'"))?;

    let native_dependencies = parse_native_dependencies(manifest.native.as_ref())
        .wrap_err_with(|| format!("Failed to parse [native.dependencies] in '{path}'"))?;

    Ok(ParsedManifest {
        package: manifest.package,
        application: manifest.application,
        dependencies,
        native_dependencies,
    })
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

/// Look for `beamtalk.toml` in the given directory and parse it fully
/// (including dependencies) if found.
///
/// Returns `None` if no manifest file exists. Returns an error if the file
/// exists but is malformed.
pub fn find_manifest_full(project_root: &Utf8Path) -> Result<Option<ParsedManifest>> {
    let manifest_path = project_root.join("beamtalk.toml");
    if manifest_path
        .try_exists()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat manifest '{manifest_path}'"))?
    {
        parse_manifest_full(&manifest_path).map(Some)
    } else {
        Ok(None)
    }
}

/// Look for `beamtalk.toml` in the given directory and return the `[application]` config if present.
///
/// Returns `None` if no manifest file exists or no `[application]` section is present.
/// Returns an error if the file exists but is malformed.
pub fn find_application_config(project_root: &Utf8Path) -> Result<Option<ApplicationConfig>> {
    let manifest_path = project_root.join("beamtalk.toml");
    if !manifest_path
        .try_exists()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat manifest '{manifest_path}'"))?
    {
        return Ok(None);
    }
    let manifest = parse_manifest_raw(&manifest_path)?;
    if let Err(e) = validate_package_name(&manifest.package.name) {
        miette::bail!("{}", format_name_error(&manifest.package.name, &e));
    }
    Ok(manifest.application)
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

    // --- Native dependency parsing tests ---

    #[test]
    fn test_parse_native_deps_pessimistic_major_minor() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
gun = "~> 2.1"
cowboy = "~> 2.12"
"#,
        );

        let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
        assert_eq!(manifest.native_dependencies.len(), 2);
        let gun = &manifest.native_dependencies["gun"];
        assert_eq!(gun.name, "gun");
        assert_eq!(gun.constraint, "~> 2.1");
        let cowboy = &manifest.native_dependencies["cowboy"];
        assert_eq!(cowboy.name, "cowboy");
        assert_eq!(cowboy.constraint, "~> 2.12");
    }

    #[test]
    fn test_parse_native_deps_pessimistic_patch() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
jason = "~> 1.4.0"
"#,
        );

        let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
        assert_eq!(manifest.native_dependencies["jason"].constraint, "~> 1.4.0");
    }

    #[test]
    fn test_parse_native_deps_exact_version() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
cowboy = "2.12.0"
"#,
        );

        let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
        assert_eq!(manifest.native_dependencies["cowboy"].constraint, "2.12.0");
    }

    #[test]
    fn test_parse_native_deps_compound_constraint() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
ranch = ">= 1.0.0 and < 2.0.0"
"#,
        );

        let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
        assert_eq!(
            manifest.native_dependencies["ranch"].constraint,
            ">= 1.0.0 and < 2.0.0"
        );
    }

    #[test]
    fn test_parse_native_deps_comparison_operators() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
dep_ge = ">= 1.0"
dep_gt = "> 1.0"
dep_le = "<= 3.0"
dep_lt = "< 2.0.0"
"#,
        );

        let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
        assert_eq!(manifest.native_dependencies.len(), 4);
        assert_eq!(manifest.native_dependencies["dep_ge"].constraint, ">= 1.0");
        assert_eq!(manifest.native_dependencies["dep_gt"].constraint, "> 1.0");
        assert_eq!(manifest.native_dependencies["dep_le"].constraint, "<= 3.0");
        assert_eq!(manifest.native_dependencies["dep_lt"].constraint, "< 2.0.0");
    }

    #[test]
    fn test_parse_no_native_deps_section() {
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
        assert!(manifest.native_dependencies.is_empty());
    }

    #[test]
    fn test_parse_empty_native_deps_section() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
"#,
        );

        let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
        assert!(manifest.native_dependencies.is_empty());
    }

    #[test]
    fn test_reject_native_dep_non_string_value() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
gun = 42
"#,
        );

        let result = parse_manifest_full(&path.join("beamtalk.toml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("version constraint string"),
            "should mention string type: {err}"
        );
    }

    #[test]
    fn test_reject_native_dep_empty_constraint() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
gun = ""
"#,
        );

        let result = parse_manifest_full(&path.join("beamtalk.toml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("must not be empty"),
            "should mention empty constraint: {err}"
        );
    }

    #[test]
    fn test_reject_native_dep_invalid_version() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
gun = "abc"
"#,
        );

        let result = parse_manifest_full(&path.join("beamtalk.toml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("non-numeric"),
            "should mention non-numeric segment: {err}"
        );
    }

    #[test]
    fn test_reject_native_dep_too_many_segments() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
gun = "1.2.3.4"
"#,
        );

        let result = parse_manifest_full(&path.join("beamtalk.toml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("too many segments"),
            "should mention too many segments: {err}"
        );
    }

    #[test]
    fn test_reject_native_dep_tilde_without_minor() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
gun = "~> 2"
"#,
        );

        let result = parse_manifest_full(&path.join("beamtalk.toml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("major.minor"),
            "should mention ~> needs minor: {err}"
        );
    }

    #[test]
    fn test_reject_native_dep_invalid_name_uppercase() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
Gun = "~> 2.1"
"#,
        );

        let result = parse_manifest_full(&path.join("beamtalk.toml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("lowercase"),
            "should mention lowercase requirement: {err}"
        );
    }

    #[test]
    fn test_reject_native_dep_invalid_name_dash() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
my-dep = "~> 1.0"
"#,
        );

        let result = parse_manifest_full(&path.join("beamtalk.toml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("invalid character"),
            "should mention invalid character: {err}"
        );
    }

    #[test]
    fn test_reject_native_dep_tilde_in_compound() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
gun = "~> 2.1 and < 3.0"
"#,
        );

        let result = parse_manifest_full(&path.join("beamtalk.toml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("pessimistic"),
            "should mention ~> in compound not allowed: {err}"
        );
    }

    #[test]
    fn test_native_deps_do_not_affect_regular_deps() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
utils = { path = "../utils" }

[native.dependencies]
gun = "~> 2.1"
"#,
        );

        let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
        assert_eq!(manifest.dependencies.len(), 1);
        assert!(manifest.dependencies.contains_key("utils"));
        assert_eq!(manifest.native_dependencies.len(), 1);
        assert!(manifest.native_dependencies.contains_key("gun"));
    }

    #[test]
    fn test_packages_without_native_deps_unaffected() {
        // A manifest with only regular dependencies should still parse correctly
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "my_app"
version = "0.1.0"
description = "Regular app without native deps"

[dependencies]
json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }
utils = { path = "../utils" }
"#,
        );

        let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
        assert_eq!(manifest.dependencies.len(), 2);
        assert!(manifest.native_dependencies.is_empty());
    }

    // --- Hex constraint validation unit tests ---

    #[test]
    fn test_validate_hex_constraint_valid() {
        let valid = [
            "2.12.0",
            "1.0",
            "3",
            "~> 2.1",
            "~> 2.1.0",
            ">= 1.0.0",
            "> 1.0",
            "< 2.0.0",
            "<= 3.0",
            ">= 1.0.0 and < 2.0.0",
            ">= 0.1 and <= 1.0",
        ];
        for c in valid {
            assert!(
                validate_hex_constraint(c).is_ok(),
                "expected '{c}' to be valid"
            );
        }
    }

    #[test]
    fn test_validate_hex_constraint_invalid() {
        let invalid = [
            ("", "empty"),
            ("abc", "non-numeric"),
            ("1.2.3.4", "too many segments"),
            ("~> 2", "major.minor"),
            ("~>", "must be followed"),
            (">=", "must be followed"),
            (">= 1.0.0 and ~> 2.0", "pessimistic"),
            (">= 1.0 and 2.0", "comparison operators"),
        ];
        for (constraint, expected_msg) in invalid {
            let result = validate_hex_constraint(constraint);
            assert!(result.is_err(), "expected '{constraint}' to be invalid");
            let err = result.unwrap_err();
            assert!(
                err.contains(expected_msg),
                "error for '{constraint}' should contain '{expected_msg}', got: {err}"
            );
        }
    }
}
