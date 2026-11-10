// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Parsing a manifest's `[dependencies]` table into a
//! [`DependencyMap`] (ADR 0026, ADR 0070).
//!
//! **DDD Context:** Build System
//!
//! Lives here rather than in `beamtalk-cli` so the LSP, which must not
//! depend on `beamtalk-cli`, reads `[dependencies]` with the exact same
//! validation `beamtalk build` applies (BT-4611). `beamtalk-cli`'s
//! `manifest` module re-exports these items.

use crate::package_name::{format_name_error, validate_package_name};
use beamtalk_core::compilation::{DependencyMap, DependencySource, DependencySpec, GitReference};
use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::PathBuf;

/// A dependency entry as it appears in `beamtalk.toml`.
///
/// Supports three forms:
/// - `name = "1.2.3"` — registry dependency at an exact version
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

        let reference = git_reference_from_fields(
            &format!("Dependency '{name}'"),
            dep.tag.as_ref(),
            dep.branch.as_ref(),
            dep.rev.as_ref(),
        )?;

        DependencySource::Git { url, reference }
    };

    Ok(DependencySpec {
        name: name.to_string(),
        source,
    })
}

/// Build a [`GitReference`] from a git dependency's `tag`/`branch`/`rev`
/// fields, requiring exactly one of them. Shared by `[dependencies]` and
/// `[native.dependencies]`; `subject` names the entry in error messages
/// (e.g. `"Dependency 'json'"`).
///
/// # Errors
///
/// Returns an error unless exactly one of `tag`, `branch` or `rev` is set.
pub fn git_reference_from_fields(
    subject: &str,
    tag: Option<&String>,
    branch: Option<&String>,
    rev: Option<&String>,
) -> Result<GitReference> {
    match (tag, branch, rev) {
        (Some(tag), None, None) => Ok(GitReference::Tag(tag.clone())),
        (None, Some(branch), None) => Ok(GitReference::Branch(branch.clone())),
        (None, None, Some(rev)) => Ok(GitReference::Rev(rev.clone())),
        (None, None, None) => miette::bail!(
            "{subject} is a git dependency but has no ref — specify 'tag', 'branch', or 'rev'"
        ),
        _ => miette::bail!(
            "{subject} specifies multiple git references — use exactly one of 'tag', 'branch', or 'rev'"
        ),
    }
}

/// Validate a bare-string registry dependency (`name = "1.2.3"`).
///
/// Registry dependencies pin an exact `major.minor.patch` version — version
/// ranges are deliberately not supported, so the resolved graph is always
/// reproducible without a constraint solver.
///
/// `pub` (rather than crate-private) so `deps add --version` (in the
/// `beamtalk` binary crate that depends on this lib crate) can validate a
/// user-supplied version up front, with the identical error a manual
/// `beamtalk.toml` edit would get, before touching the registry index.
///
/// # Errors
///
/// Returns an error if `name` is not a valid package name, or `version` is
/// not an exact `major.minor.patch` version.
pub fn validate_registry_dependency(name: &str, version: &str) -> Result<DependencySpec> {
    if let Err(e) = validate_package_name(name) {
        miette::bail!(
            "Invalid dependency name '{name}': {}",
            format_name_error(name, &e)
        );
    }

    if let Err(reason) = validate_exact_version(version) {
        miette::bail!(
            "Dependency '{name}' has an invalid version '{version}' — {reason}\n\n  \
             Registry dependencies must pin an exact version, e.g.\n    \
             {name} = \"1.2.3\"\n\n  \
             For version ranges or a specific branch, use a git dependency instead:\n    \
             {name} = {{ git = \"https://...\", tag = \"v1.2.3\" }}"
        );
    }

    Ok(DependencySpec {
        name: name.to_string(),
        source: DependencySource::Registry {
            version: version.to_string(),
        },
    })
}

/// Validate an exact `major.minor.patch` version string.
///
/// Every segment must be a non-empty run of ASCII digits. Returns a short
/// human-readable reason on failure (the caller adds the surrounding context).
///
/// `pub` so `commands::version` (`beamtalk version X.Y.Z`) can apply
/// the same exact-version rule a registry dependency's version is held to,
/// rather than duplicating it.
///
/// # Errors
///
/// Returns `Err` with a short human-readable reason if `version` is not a
/// valid `major.minor.patch` string (empty, has surrounding whitespace, uses
/// a range operator, has the wrong number of segments, a non-numeric
/// segment, or a segment with a leading zero).
pub fn validate_exact_version(version: &str) -> Result<(), String> {
    if version.is_empty() {
        return Err("the version is empty".to_string());
    }

    if version.trim() != version {
        return Err("the version has leading or trailing whitespace".to_string());
    }

    // Reject range/constraint operators with a targeted message rather than
    // the generic "not major.minor.patch" one.
    for op in ["~>", ">=", "<=", ">", "<", "=", "^", "*"] {
        if version.starts_with(op) {
            return Err(format!(
                "version ranges are not supported (found '{op}'), only exact versions"
            ));
        }
    }

    let segments: Vec<&str> = version.split('.').collect();
    if segments.len() != 3 {
        return Err(format!(
            "expected exactly three segments (major.minor.patch), found {}",
            segments.len()
        ));
    }

    for segment in &segments {
        if segment.is_empty() {
            return Err("a version segment is empty".to_string());
        }
        if !segment.bytes().all(|b| b.is_ascii_digit()) {
            return Err(format!(
                "version segment '{segment}' is not a non-negative integer"
            ));
        }
        // `01.0.0` and `1.0.0` name the same release but would be pinned as two
        // different strings in the lockfile, so only the canonical spelling is
        // accepted.
        if segment.len() > 1 && segment.starts_with('0') {
            return Err(format!(
                "version segment '{segment}' has a leading zero — write it as '{}'",
                segment.trim_start_matches('0')
            ));
        }
    }

    Ok(())
}

/// Parse and validate the `[dependencies]` section of a manifest.
///
/// Returns an empty map if the section is missing or empty.
///
/// # Errors
///
/// Returns an error if the section is not a table or any entry is malformed.
pub fn parse_dependencies(deps: Option<&toml::Value>) -> Result<DependencyMap> {
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
        // A bare string is a registry dependency: `name = "1.2.3"`.
        let spec = if let Some(version) = value.as_str() {
            validate_registry_dependency(name, version)?
        } else {
            // Otherwise the entry must be an inline table like { path = "..." }
            let dep: TomlDependency =
                value
                    .clone()
                    .try_into()
                    .into_diagnostic()
                    .wrap_err_with(|| {
                        format!(
                            "Dependency '{name}' has an invalid format — expected a version string \
                         like \"1.2.3\", or a table with 'path' or 'git' keys"
                        )
                    })?;

            validate_dependency(name, &dep)?
        };
        result.insert(name.clone(), spec);
    }
    Ok(result)
}

/// Return a human-readable TOML type name for error messages.
#[must_use]
pub fn value_type_name(value: &toml::Value) -> &'static str {
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

/// Read only the `[dependencies]` table of the manifest at `manifest_path`.
///
/// The other sections are not validated: a tool that only needs the
/// dependency graph (the LSP, the offline dependency scan) should not lose
/// it to an unrelated malformed section.
///
/// # Errors
///
/// Returns an error if the file cannot be read, is not valid TOML, or has a
/// malformed `[dependencies]` section.
pub fn read_manifest_dependencies(manifest_path: &Utf8Path) -> Result<DependencyMap> {
    let content = std::fs::read_to_string(manifest_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read manifest '{manifest_path}'"))?;
    let document: toml::Table = toml::from_str(&content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to parse manifest '{manifest_path}'"))?;
    parse_dependencies(document.get("dependencies"))
        .wrap_err_with(|| format!("Failed to parse [dependencies] in '{manifest_path}'"))
}
