// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `[native.dependencies]` / `[native.overrides]` types and parsing.
//!
//! **DDD Context:** Build System
//!
//! Declarations of native (Erlang) dependencies and rebar3 overrides, and the
//! parser that validates them. Moved out of `manifest.rs` (BT-4700); the public
//! types are re-exported from `manifest` so existing paths are unchanged.

use super::hex::validate_hex_constraint;
use super::{git_reference_from_fields, value_type_name};
use beamtalk_core::compilation::GitReference;
use miette::{Context, IntoDiagnostic, Result};
use serde::Deserialize;
use std::collections::BTreeMap;
use std::fmt;

/// A map of native dependency names to their declarations.
pub type NativeDependencyMap = BTreeMap<String, NativeDependency>;

/// A map of rebar3 override targets (dependency names) to their overrides.
pub type NativeOverrideMap = BTreeMap<String, NativeOverride>;

/// A native (Erlang) dependency declared in `[native.dependencies]`.
///
/// Either a hex.pm version constraint (`gun = "~> 2.1"`) or a git source
/// (`oidcc = { git = "...", branch = "..." }`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeDependency {
    /// The Erlang application / hex package name.
    pub name: String,
    /// Where rebar3 fetches the dependency from.
    pub source: NativeSource,
}

impl NativeDependency {
    /// The hex version constraint, or `None` for a git dependency.
    pub fn hex_constraint(&self) -> Option<&str> {
        match &self.source {
            NativeSource::Hex { constraint } => Some(constraint),
            NativeSource::Git { .. } => None,
        }
    }
}

/// The source of a native dependency.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeSource {
    /// A hex.pm package with a version constraint.
    ///
    /// Version constraints follow hex.pm conventions:
    /// - `"~> 2.1"` — `>= 2.1.0 and < 3.0.0`
    /// - `"~> 2.1.0"` — `>= 2.1.0 and < 2.2.0`
    /// - `">= 1.0.0 and < 2.0.0"` — explicit range
    /// - `"2.12.0"` — exact version
    Hex {
        /// The version constraint string (passed through to rebar.config verbatim).
        constraint: String,
    },
    /// A git repository checked out at a tag, branch or commit.
    Git {
        /// The git repository URL.
        url: String,
        /// The tag, branch or commit to check out.
        reference: GitReference,
    },
}

impl fmt::Display for NativeSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Hex { constraint } => write!(f, "hex \"{constraint}\""),
            Self::Git { url, reference } => {
                write!(f, "git {url}")?;
                match reference {
                    GitReference::Tag(tag) => write!(f, " (tag: {tag})"),
                    GitReference::Branch(branch) => write!(f, " (branch: {branch})"),
                    GitReference::Rev(rev) => write!(f, " (rev: {rev})"),
                }
            }
        }
    }
}

/// A rebar3 override declared in `[native.overrides]`.
///
/// Emitted as `{override, Dep, [{erl_opts, [...]}]}`, which replaces the
/// dependency's own `erl_opts` — e.g. to drop a `warnings_as_errors` that
/// breaks the build on a newer OTP.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeOverride {
    /// Replacement `erl_opts` for the dependency, as Erlang atom names
    /// (e.g. `debug_info`).
    pub erl_opts: Vec<String>,
}

/// The `[native]` section of `beamtalk.toml`.
///
/// Contains an optional `dependencies` sub-table declaring hex.pm or git
/// dependencies for native Erlang code, and an optional `overrides`
/// sub-table of rebar3 overrides.
#[derive(Debug, Deserialize, Default)]
pub(super) struct NativeSection {
    /// The `[native.dependencies]` table — dependency name to a hex version
    /// constraint string or a git source table.
    #[serde(default)]
    pub(super) dependencies: Option<toml::Value>,
    /// The `[native.overrides]` table — dependency name to rebar3 overrides.
    #[serde(default)]
    pub(super) overrides: Option<toml::Value>,
}

/// A git source in `[native.dependencies]`:
/// `name = { git = "...", tag/branch/rev = "..." }`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TomlNativeGitDependency {
    git: String,
    tag: Option<String>,
    branch: Option<String>,
    rev: Option<String>,
}

/// An entry in `[native.overrides]`: `name = { erl_opts = [...] }`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TomlNativeOverride {
    erl_opts: Vec<String>,
}

/// Validate a native dependency (or override target) name.
///
/// Hex package / OTP application names use lowercase letters, digits and
/// underscores, so they are also valid unquoted Erlang atoms in the
/// generated `rebar.config`.
fn validate_native_name(name: &str) -> Result<()> {
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
    Ok(())
}

/// Return the `[native]` sub-table `key` as a TOML table, or `None` when the
/// section or sub-table is absent.
fn native_subtable<'a>(
    raw_value: Option<&'a toml::Value>,
    key: &str,
) -> Result<Option<&'a toml::Table>> {
    let Some(raw_value) = raw_value else {
        return Ok(None);
    };
    raw_value.as_table().map(Some).ok_or_else(|| {
        miette::miette!(
            "[native.{key}] must be a table, not a {}",
            value_type_name(raw_value)
        )
    })
}

/// Parse and validate the `[native.dependencies]` section of a manifest.
///
/// Each entry is either a hex version-constraint string or a git source
/// table with exactly one of `tag`, `branch` or `rev`. Returns an empty map
/// if the section is missing or empty.
pub(super) fn parse_native_dependencies(
    native: Option<&NativeSection>,
) -> Result<NativeDependencyMap> {
    let raw = native.and_then(|section| section.dependencies.as_ref());
    let Some(table) = native_subtable(raw, "dependencies")? else {
        return Ok(BTreeMap::new());
    };

    let mut result = NativeDependencyMap::new();
    for (name, value) in table {
        validate_native_name(name)?;

        let source = match value {
            toml::Value::String(constraint) => {
                // Validate the constraint syntax (use trimmed value)
                let normalized = constraint.trim();
                if let Err(msg) = validate_hex_constraint(normalized) {
                    miette::bail!(
                        "Native dependency '{name}' has an invalid version constraint \
                         '{constraint}': {msg}"
                    );
                }
                NativeSource::Hex {
                    constraint: normalized.to_string(),
                }
            }
            toml::Value::Table(fields) => parse_native_git_source(name, fields)?,
            other => miette::bail!(
                "Native dependency '{name}' must be a version constraint string or a git \
                 table like {{ git = \"https://...\", tag = \"v1.0.0\" }}, not a {}",
                value_type_name(other)
            ),
        };

        result.insert(
            name.clone(),
            NativeDependency {
                name: name.clone(),
                source,
            },
        );
    }

    Ok(result)
}

/// Parse a native git source table (`{ git = "...", branch = "..." }`).
fn parse_native_git_source(name: &str, fields: &toml::Table) -> Result<NativeSource> {
    // rebar3 spells a commit pin `{ref, Sha}`; the manifest uses `rev` for
    // both [dependencies] and [native.dependencies], so point at it.
    if fields.contains_key("ref") {
        miette::bail!(
            "Native dependency '{name}' uses 'ref' — pin a commit with 'rev' instead \
             (the same key [dependencies] uses)"
        );
    }
    if !fields.contains_key("git") {
        miette::bail!(
            "Native dependency '{name}' is a table but has no 'git' URL — hex dependencies \
             are written as a version string, e.g. {name} = \"~> 1.0\""
        );
    }

    let dep: TomlNativeGitDependency = toml::Value::Table(fields.clone())
        .try_into()
        .into_diagnostic()
        .wrap_err_with(|| {
            format!(
                "Native dependency '{name}' has an invalid git source — expected \
                 {{ git = \"https://...\", tag/branch/rev = \"...\" }}"
            )
        })?;

    let url = dep.git.trim();
    if url.is_empty() {
        miette::bail!("Native dependency '{name}' has an empty 'git' URL");
    }

    let reference = git_reference_from_fields(
        &format!("Native dependency '{name}'"),
        dep.tag.as_ref(),
        dep.branch.as_ref(),
        dep.rev.as_ref(),
    )?;

    Ok(NativeSource::Git {
        url: url.to_string(),
        reference,
    })
}

/// Parse and validate the `[native.overrides]` section of a manifest.
///
/// Each entry names a native dependency (direct or transitive) and gives the
/// rebar3 overrides to apply to it. Only `erl_opts` is supported, as a list
/// of Erlang atom names. Returns an empty map if the section is missing.
pub(super) fn parse_native_overrides(native: Option<&NativeSection>) -> Result<NativeOverrideMap> {
    let raw = native.and_then(|section| section.overrides.as_ref());
    let Some(table) = native_subtable(raw, "overrides")? else {
        return Ok(BTreeMap::new());
    };

    let mut result = NativeOverrideMap::new();
    for (name, value) in table {
        validate_native_name(name)?;

        let entry: TomlNativeOverride =
            value
                .clone()
                .try_into()
                .into_diagnostic()
                .wrap_err_with(|| {
                    format!(
                        "Native override '{name}' has an invalid format — expected a table \
                     like {{ erl_opts = [\"debug_info\"] }}"
                    )
                })?;

        for opt in &entry.erl_opts {
            if !is_plain_erlang_atom(opt) {
                miette::bail!(
                    "Native override '{name}' has an invalid erl_opts entry '{opt}' — \
                     only plain atoms such as \"debug_info\" or \"nowarn_export_all\" \
                     are supported"
                );
            }
        }

        result.insert(
            name.clone(),
            NativeOverride {
                erl_opts: entry.erl_opts,
            },
        );
    }

    Ok(result)
}

/// Whether `s` is an Erlang atom that needs no quoting
/// (`[a-z][A-Za-z0-9_@]*`).
fn is_plain_erlang_atom(s: &str) -> bool {
    s.starts_with(|c: char| c.is_ascii_lowercase())
        && s.chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '@')
}
