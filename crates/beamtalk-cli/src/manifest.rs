// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package manifest parsing and validation.
//!
//! **DDD Context:** Build System
//!
//! Parses `beamtalk.toml` manifests that define a package's identity and metadata.
//! See ADR 0026 for the package definition and manifest format.
//!
//! This module lives in the `beamtalk_cli` **library** crate (not
//! the `beamtalk` binary) so `beamtalk-mcp` can depend on it too. The
//! `beamtalk` binary's `crate::commands::manifest` is a `pub use
//! beamtalk_cli::manifest;` re-export (see `commands/mod.rs`) that keeps
//! existing `crate::commands::manifest`/`super::manifest` call sites
//! throughout `commands/` compiling unchanged. Every item this module
//! exposes to those call sites must therefore be `pub`, not `pub(crate)` —
//! `pub(crate)` here would scope to the *library* crate only and become
//! invisible to the binary crate, producing a confusing "private item"
//! error far from this file.

use beamtalk_core::compilation::{
    DependencyMap, DependencySource, DependencySpec, GitReference, parse_diagnostics_table,
};
#[cfg(test)]
use beamtalk_core::source_analysis::DiagnosticCategory;
use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use serde::Deserialize;
use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::path::PathBuf;

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
    /// The optional `[registry]` section overriding the package registry the
    /// project's registry dependencies resolve through.
    #[serde(default)]
    registry: Option<RegistryConfig>,
    /// The optional `[stubs]` section declaring where this package's own
    /// FFI type stub files live (ADR 0075 layer 2).
    #[serde(default)]
    stubs: Option<StubsConfig>,
    /// The optional `[diagnostics]` section — per-category severity overrides
    /// (ADR 0100 Rule 3). Stored as raw TOML for lazy parsing.
    ///
    /// Use [`parse_manifest_full`] to get a validated [`DiagnosticsTable`].
    #[serde(default)]
    diagnostics: Option<toml::Value>,
    /// The optional `[release]` section — `beamtalk release` configuration
    /// (ADR 0125 §1.2). Stored as raw TOML for lazy parsing so an invalid
    /// key (e.g. `version`) gets [`ReleaseConfigError`]'s structured
    /// diagnostic rather than a generic serde error.
    ///
    /// Use [`parse_manifest_full`] to get a validated [`ReleaseConfig`].
    #[serde(default)]
    release: Option<toml::Value>,
}

/// The `[native]` section of `beamtalk.toml`.
///
/// Contains an optional `dependencies` sub-table declaring hex.pm or git
/// dependencies for native Erlang code, and an optional `overrides`
/// sub-table of rebar3 overrides.
#[derive(Debug, Deserialize, Default)]
struct NativeSection {
    /// The `[native.dependencies]` table — dependency name to a hex version
    /// constraint string or a git source table.
    #[serde(default)]
    dependencies: Option<toml::Value>,
    /// The `[native.overrides]` table — dependency name to rebar3 overrides.
    #[serde(default)]
    overrides: Option<toml::Value>,
}

/// The `[registry]` section of `beamtalk.toml`.
///
/// Overrides which package registry this project's registry dependencies
/// resolve through. The value may be a git URL (cloned into
/// `_build/registry/index/`) or a path to a local directory containing a
/// `packages/` subdirectory (used in place, with no git and no network).
///
/// ```toml
/// [registry]
/// url = "https://github.com/jamesc/beamtalk-registry"
/// ```
#[derive(Debug, Deserialize, Clone, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct RegistryConfig {
    /// The registry index location — a git URL or a local directory path.
    pub url: String,
}

/// The `[stubs]` section of `beamtalk.toml`.
///
/// Declares where a package's own FFI type stub files (`declare native:`
/// `.bt` files, ADR 0075) live, relative to the package root. A package
/// declares stubs only for its own native Erlang code (its `native/`
/// directory, ADR 0072) — not for its dependencies, which are typed via
/// auto-extraction instead.
///
/// ```toml
/// [stubs]
/// path = "stubs/"
/// ```
#[derive(Debug, Deserialize, Clone, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct StubsConfig {
    /// Path to the package's stub directory, relative to the package root.
    pub path: String,
}

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

/// `beamtalk release` configuration from the `[release]` section of
/// `beamtalk.toml` (ADR 0125 §1.2). Every key is optional; a missing
/// `[release]` section is [`ReleaseConfig::default`].
///
/// There is deliberately no `version` field — the release version is always
/// `[package] version` ([`ReleaseConfigError::VersionKeyNotAllowed`]).
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::struct_excessive_bools)] // orthogonal [release] flags, each independently toggled
pub struct ReleaseConfig {
    /// Release name; defaults to `[package] name` when `None`.
    pub name: Option<String>,
    /// Extra OTP applications to add to the computed closure, beyond the
    /// project app, its ADR 0070 dependency closure, and the runtime
    /// closure (`beamtalk_runtime`/`beamtalk_stdlib`/`beamtalk_workspace`
    /// and their declared deps).
    pub apps: Vec<String>,
    /// Bundle this machine's ERTS into the release (ADR 0125 §1.3, BT-3571):
    /// `release::assembly::copy_erts` copies it on disk into
    /// `erts-<vsn>/`, and `release::assembly::make_tarball` embeds it in the
    /// tarball. `beamtalk release --no-include-erts` overrides this to
    /// `false` for one build without editing the manifest.
    pub include_erts: bool,
    /// Start the REPL/remote-console WebSocket listener.
    pub console: bool,
    /// Bind address for the console listener; only meaningful when
    /// `console = true`.
    pub bind: String,
    /// Path (relative to the project root) to a user `sys.config` fragment
    /// merged into the generated one.
    pub sys_config: String,
    /// Path (relative to the project root) to a user `vm.args` fragment
    /// merged into the generated one.
    pub vm_args: String,
    /// Drop `debug_info` chunks from staged beams (ADR 0125 §1.3/§3.3,
    /// BT-3571) via `beam_lib:strip_release/1`
    /// (`release::assembly::strip_release_beams`); refused in combination
    /// with `include_compiler` (`ReleaseConfigError::StripBeamsWithIncludeCompiler`,
    /// above).
    pub strip_beams: bool,
    /// Ship `beamtalk_compiler` in the release (a live, patchable image,
    /// ADR 0125 §1.3/§1.5): joins the app closure (`closure.rs`), and
    /// BT-3571 additionally stages the compiler port binary itself
    /// (`release::assembly::stage_compiler_port_binary`) and sets
    /// `include_compiler => true` in the generated `sys.config`.
    pub include_compiler: bool,
}

impl Default for ReleaseConfig {
    fn default() -> Self {
        Self {
            name: None,
            apps: Vec::new(),
            include_erts: true,
            console: false,
            bind: "127.0.0.1".to_string(),
            sys_config: "config/sys.config".to_string(),
            vm_args: "config/vm.args".to_string(),
            strip_beams: false,
            include_compiler: false,
        }
    }
}

/// Errors parsing a `beamtalk.toml` `[release]` table (ADR 0125 §1.2).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum ReleaseConfigError {
    /// `[release]` is present but is not a table.
    #[error("[release] must be a table, not a {found}")]
    NotATable {
        /// The TOML value kind actually found (e.g. `"string"`).
        found: &'static str,
    },
    /// `[release]` declares a `version` key.
    #[error("[release] must not declare 'version'")]
    #[diagnostic(help(
        "The release version is always [package] version, the same single \
         source of truth every other Beamtalk artifact uses — see \
         docs/development/releasing.md. Remove the 'version' key from \
         [release]."
    ))]
    VersionKeyNotAllowed,
    /// A `[release]` key is not one of the recognised keys.
    #[error("[release] has unknown key '{key}'")]
    #[diagnostic(help(
        "Expected one of: name, apps, include-erts, console, bind, \
         sys-config, vm-args, strip-beams, include-compiler (ADR 0125 §1.2)."
    ))]
    UnknownKey {
        /// The offending key as written in the manifest.
        key: String,
    },
    /// A `[release]` value has the wrong TOML type for its key.
    #[error("[release] '{key}' must be a {expected}, not a {found}")]
    WrongType {
        /// The key whose value has the wrong type.
        key: &'static str,
        /// The TOML type this key expects.
        expected: &'static str,
        /// The TOML value kind actually found.
        found: &'static str,
    },
    /// `[release] apps` contains a non-string element.
    #[error("[release] apps' entries must all be strings, found a {found}")]
    AppsEntryWrongType {
        /// The TOML value kind actually found.
        found: &'static str,
    },
    /// `strip-beams = true` and `include-compiler = true` together.
    #[error("[release] strip-beams = true together with include-compiler = true is refused")]
    #[diagnostic(help(
        "include-compiler ships a live compiler port, which needs \
         debug_info-bearing beams to recompile against (ADR 0075); \
         strip-beams drops that chunk. Set at most one of the two to true."
    ))]
    StripBeamsWithIncludeCompiler,
}

/// Validate that `value` is a string, for a `[release]` key expecting one.
fn release_config_expect_string(
    key: &'static str,
    value: &toml::Value,
) -> std::result::Result<String, ReleaseConfigError> {
    value
        .as_str()
        .map(str::to_string)
        .ok_or_else(|| ReleaseConfigError::WrongType {
            key,
            expected: "string",
            found: value_type_name(value),
        })
}

/// Validate that `value` is a boolean, for a `[release]` key expecting one.
fn release_config_expect_bool(
    key: &'static str,
    value: &toml::Value,
) -> std::result::Result<bool, ReleaseConfigError> {
    value
        .as_bool()
        .ok_or_else(|| ReleaseConfigError::WrongType {
            key,
            expected: "boolean",
            found: value_type_name(value),
        })
}

/// Parse and validate the `[release]` section of a manifest (ADR 0125 §1.2).
///
/// `release` is the raw TOML value of the `[release]` key, already extracted
/// from a deserialized [`Manifest`]. Returns [`ReleaseConfig::default`] when
/// the section is absent.
///
/// # Errors
///
/// Returns [`ReleaseConfigError`] if `[release]` is present but is not a
/// table, declares `version`, has an unrecognised key, has a value of the
/// wrong type for its key, or sets both `strip-beams` and
/// `include-compiler` to `true`.
pub fn parse_release_config(
    release: Option<&toml::Value>,
) -> std::result::Result<ReleaseConfig, ReleaseConfigError> {
    let mut config = ReleaseConfig::default();
    let Some(raw_value) = release else {
        return Ok(config);
    };

    let table = raw_value
        .as_table()
        .ok_or_else(|| ReleaseConfigError::NotATable {
            found: value_type_name(raw_value),
        })?;

    for (key, value) in table {
        match key.as_str() {
            "version" => return Err(ReleaseConfigError::VersionKeyNotAllowed),
            "name" => config.name = Some(release_config_expect_string("name", value)?),
            "apps" => {
                let array = value
                    .as_array()
                    .ok_or_else(|| ReleaseConfigError::WrongType {
                        key: "apps",
                        expected: "array",
                        found: value_type_name(value),
                    })?;
                let mut apps = Vec::with_capacity(array.len());
                for entry in array {
                    apps.push(entry.as_str().map(str::to_string).ok_or_else(|| {
                        ReleaseConfigError::AppsEntryWrongType {
                            found: value_type_name(entry),
                        }
                    })?);
                }
                config.apps = apps;
            }
            "include-erts" => {
                config.include_erts = release_config_expect_bool("include-erts", value)?;
            }
            "console" => config.console = release_config_expect_bool("console", value)?,
            "bind" => config.bind = release_config_expect_string("bind", value)?,
            "sys-config" => config.sys_config = release_config_expect_string("sys-config", value)?,
            "vm-args" => config.vm_args = release_config_expect_string("vm-args", value)?,
            "strip-beams" => config.strip_beams = release_config_expect_bool("strip-beams", value)?,
            "include-compiler" => {
                config.include_compiler = release_config_expect_bool("include-compiler", value)?;
            }
            other => {
                return Err(ReleaseConfigError::UnknownKey {
                    key: other.to_string(),
                });
            }
        }
    }

    if config.strip_beams && config.include_compiler {
        return Err(ReleaseConfigError::StripBeamsWithIncludeCompiler);
    }

    Ok(config)
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
fn git_reference_from_fields(
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

/// Whether a resolved hex `version` satisfies a `constraint` already
/// accepted by [`validate_hex_constraint`] (hex.pm semantics: `~> 2.1` is
/// `>= 2.1.0 and < 3.0.0`, `~> 2.1.0` is `>= 2.1.0 and < 2.2.0`).
///
/// A pre-release version (`2.1.0-rc.1`) sorts before its release and never
/// matches a bare `~>` or exact constraint, so a lock holding one is simply
/// re-resolved. Returns `false` for anything unparseable.
pub fn hex_constraint_matches(constraint: &str, version: &str) -> bool {
    let Some(version) = HexVersion::parse(version) else {
        return false;
    };
    constraint
        .trim()
        .split(" and ")
        .all(|part| single_constraint_matches(part.trim(), &version))
}

/// A hex version for comparison: numeric `major.minor.patch` (missing
/// segments are 0) plus whether it is a release (pre-releases sort first).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct HexVersion {
    numbers: [u64; 3],
    is_release: bool,
}

impl HexVersion {
    fn parse(version: &str) -> Option<Self> {
        let version = version.trim();
        // Build metadata never affects precedence.
        let version = version.split('+').next()?;
        let (core, pre) = match version.split_once('-') {
            Some((core, pre)) => (core, Some(pre)),
            None => (version, None),
        };
        let mut numbers = [0; 3];
        let mut count = 0;
        for (slot, segment) in numbers.iter_mut().zip(core.split('.')) {
            *slot = segment.parse().ok()?;
            count += 1;
        }
        if count == 0 || core.split('.').count() > 3 {
            return None;
        }
        Some(Self {
            numbers,
            is_release: pre.is_none(),
        })
    }

    fn release(numbers: [u64; 3]) -> Self {
        Self {
            numbers,
            is_release: true,
        }
    }
}

fn single_constraint_matches(s: &str, version: &HexVersion) -> bool {
    let bound = |rest: &str| HexVersion::parse(rest);
    if let Some(rest) = s.strip_prefix("~>") {
        let rest = rest.trim();
        let Some(lower) = bound(rest) else {
            return false;
        };
        let [major, minor, _] = lower.numbers;
        // `~> X.Y` allows any X.*; `~> X.Y.Z` allows any X.Y.*.
        let upper = if rest.split('.').count() >= 3 {
            [major, minor + 1, 0]
        } else {
            [major + 1, 0, 0]
        };
        version.is_release && *version >= lower && *version < HexVersion::release(upper)
    } else if let Some(rest) = s.strip_prefix(">=") {
        bound(rest).is_some_and(|b| *version >= b)
    } else if let Some(rest) = s.strip_prefix("<=") {
        bound(rest).is_some_and(|b| *version <= b)
    } else if let Some(rest) = s.strip_prefix('>') {
        bound(rest).is_some_and(|b| *version > b)
    } else if let Some(rest) = s.strip_prefix('<') {
        bound(rest).is_some_and(|b| *version < b)
    } else {
        bound(s).is_some_and(|b| *version == b)
    }
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
fn parse_native_dependencies(native: Option<&NativeSection>) -> Result<NativeDependencyMap> {
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
fn parse_native_overrides(native: Option<&NativeSection>) -> Result<NativeOverrideMap> {
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

// ADR 0100 Rule 3: `DiagnosticSeverityOverride` and
// `DiagnosticsTable` — plus the `[diagnostics]`-table parser
// (`parse_diagnostics_table`) — moved to `beamtalk-core`
// (`beamtalk_core::compilation::diagnostics_policy`) so that `beamtalk-lsp`
// can apply the same table `beamtalk build` does without depending on
// `beamtalk-cli` (dependencies flow downward only — see
// `docs/development/architecture-principles.md`). Re-exported here so
// existing `crate::commands::manifest::{DiagnosticsTable,
// DiagnosticSeverityOverride}` references throughout this crate keep working
// unchanged. `DiagnosticSeverityOverride` is referenced from test code in
// both this lib crate and the `beamtalk` bin crate's own `#[cfg(test)]`
// modules (e.g. `beam_compiler.rs`); the bin crate depends on this lib built
// *without* `cfg(test)`, so the re-export must be unconditional.
pub use beamtalk_core::compilation::DiagnosticSeverityOverride;
pub use beamtalk_core::compilation::DiagnosticsTable;

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
    /// The parsed and validated native (hex or git) dependencies (empty if
    /// `[native.dependencies]` is absent).
    pub native_dependencies: NativeDependencyMap,
    /// The parsed and validated rebar3 overrides (empty if
    /// `[native.overrides]` is absent).
    pub native_overrides: NativeOverrideMap,
    /// The parsed and validated `[diagnostics]` severity overrides (ADR 0100
    /// Rule 3; empty if the section is absent).
    pub diagnostics: DiagnosticsTable,
    /// The optional `[registry]` section — overrides where registry
    /// dependencies are resolved from (`None` uses the env var / default).
    pub registry: Option<RegistryConfig>,
    /// The optional `[stubs]` section — where this package's own FFI type
    /// stubs live (`None` if the package ships no stubs).
    pub stubs: Option<StubsConfig>,
    /// The `[release]` section (ADR 0125 §1.2), defaulted when absent.
    pub release: ReleaseConfig,
}

/// Parse a `beamtalk.toml` manifest file.
///
/// Returns the parsed manifest, or an error if the file cannot be read or
/// contains invalid TOML / missing required fields.
///
/// # Errors
///
/// Returns an error if the file cannot be read, contains invalid TOML, is
/// missing required `[package]` fields, or has an invalid package name.
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
///
/// # Errors
///
/// Returns an error if the file cannot be read, contains invalid TOML, has
/// an invalid package name, or has a malformed `[dependencies]`,
/// `[native.dependencies]`, or `[diagnostics]` section.
pub fn parse_manifest_full(path: &Utf8Path) -> Result<ParsedManifest> {
    let manifest = parse_manifest_raw(path)?;

    if let Err(e) = validate_package_name(&manifest.package.name) {
        miette::bail!("{}", format_name_error(&manifest.package.name, &e));
    }

    let dependencies = parse_dependencies(manifest.dependencies.as_ref())
        .wrap_err_with(|| format!("Failed to parse [dependencies] in '{path}'"))?;

    let native_dependencies = parse_native_dependencies(manifest.native.as_ref())
        .wrap_err_with(|| format!("Failed to parse [native.dependencies] in '{path}'"))?;

    let native_overrides = parse_native_overrides(manifest.native.as_ref())
        .wrap_err_with(|| format!("Failed to parse [native.overrides] in '{path}'"))?;

    let diagnostics = parse_diagnostics_table(manifest.diagnostics.as_ref())
        .wrap_err_with(|| format!("Failed to parse [diagnostics] in '{path}'"))?;

    let release = parse_release_config(manifest.release.as_ref())
        .wrap_err_with(|| format!("Failed to parse [release] in '{path}'"))?;

    Ok(ParsedManifest {
        package: manifest.package,
        application: manifest.application,
        dependencies,
        native_dependencies,
        native_overrides,
        diagnostics,
        registry: manifest.registry,
        stubs: manifest.stubs,
        release,
    })
}

/// Look for `beamtalk.toml` in the given directory and parse it if found.
///
/// Returns `None` if no manifest file exists. Returns an error if the file
/// exists but is malformed.
///
/// # Errors
///
/// Returns an error if the manifest path cannot be stat'd, or the file
/// exists but is malformed (see [`parse_manifest`]).
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

/// The package name declared by the `beamtalk.toml` in `project_root`, if any.
///
/// Used by `load-project` clients to tell the workspace which package a loaded
/// project root belongs to (BT-3661), so the Erlang side never parses TOML.
/// A missing or malformed manifest yields `None`: the load itself reports
/// manifest problems, and the workspace falls back to the registry-only path.
#[must_use]
pub fn package_name_for_project(project_root: &str) -> Option<String> {
    find_manifest(Utf8Path::new(project_root))
        .ok()
        .flatten()
        .map(|manifest| manifest.name)
}

/// Look for `beamtalk.toml` in the given directory and parse it fully
/// (including dependencies) if found.
///
/// Returns `None` if no manifest file exists. Returns an error if the file
/// exists but is malformed.
///
/// # Errors
///
/// Returns an error if the manifest path cannot be stat'd, or the file
/// exists but is malformed (see [`parse_manifest_full`]).
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

/// How the semantic checker treats one compilation unit's package
/// (BT-4334, BT-4382) — the one rule `build`, `lint`, `test` and the
/// offline dependency-class resolver (MCP/LSP) share.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageCheckerScope {
    /// `ProjectComplete` inside a package (every project source file is
    /// walked before per-file analysis), `ModuleOnly` otherwise.
    pub knowledge_scope: beamtalk_core::semantic_analysis::KnowledgeScope,
    /// The pre-WS3 guard: dependency extension contributions are not loaded
    /// yet, so a package that declares dependencies has no provably complete
    /// receiver surface.
    pub has_package_dependencies: bool,
    /// The package name, for E0401/E0402 visibility checks; `None` outside a
    /// package or when the manifest does not parse.
    pub package_name: Option<String>,
}

impl PackageCheckerScope {
    /// The scope for a file outside any package.
    #[must_use]
    pub fn module_only() -> Self {
        Self {
            knowledge_scope: beamtalk_core::semantic_analysis::KnowledgeScope::ModuleOnly,
            has_package_dependencies: false,
            package_name: None,
        }
    }

    /// The scope for an already-parsed manifest (`None`: no manifest, so
    /// not a package).
    #[must_use]
    pub fn for_manifest(manifest: Option<&ParsedManifest>) -> Self {
        manifest.map_or_else(Self::module_only, |manifest| Self {
            knowledge_scope: beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete,
            has_package_dependencies: !manifest.dependencies.is_empty(),
            package_name: Some(manifest.package.name.clone()),
        })
    }

    /// The scope implied by one read of a package root's manifest
    /// ([`find_manifest_full`]).
    ///
    /// A manifest that fails to parse still marks a package (the root was
    /// found by its `beamtalk.toml`), so it stays `ProjectComplete`, with
    /// dependencies conservatively assumed present (fewer diagnostics, never
    /// spurious ones) and no package name (visibility checks off). A warning
    /// is logged; the surface that reads the manifest for real (`build`)
    /// reports the parse error itself.
    #[must_use]
    pub fn from_manifest_read(read: &Result<Option<ParsedManifest>>) -> Self {
        match read {
            Ok(manifest) => Self::for_manifest(manifest.as_ref()),
            Err(e) => {
                tracing::warn!(
                    error = %e,
                    "Failed to parse beamtalk.toml; E0401/E0402 visibility checks disabled \
                     and dependencies conservatively assumed present"
                );
                Self {
                    knowledge_scope:
                        beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete,
                    has_package_dependencies: true,
                    package_name: None,
                }
            }
        }
    }
}

/// The checker scope for a compilation unit whose package root is
/// `package_root` (`None` outside any package); see [`PackageCheckerScope`].
#[must_use]
pub fn package_checker_scope(package_root: Option<&Utf8Path>) -> PackageCheckerScope {
    package_root.map_or_else(PackageCheckerScope::module_only, |root| {
        PackageCheckerScope::from_manifest_read(&find_manifest_full(root))
    })
}

/// Look for `beamtalk.toml` in the given directory and return the `[application]` config if present.
///
/// Returns `None` if no manifest file exists or no `[application]` section is present.
/// Returns an error if the file exists but is malformed.
///
/// # Errors
///
/// Returns an error if the manifest path cannot be stat'd, or the file
/// exists but is malformed (invalid TOML, missing required fields, or an
/// invalid package name).
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
///
/// # Errors
///
/// Returns a [`PackageNameError`] describing which rule `name` violates.
///
/// # Panics
///
/// Never panics: the internal `.unwrap()` on the first character is only
/// reached after the preceding empty-name check has already returned.
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

    /// BT-4382: one checker-scope rule for every surface, including the
    /// malformed-manifest case the four former copies disagreed on.
    #[test]
    fn package_checker_scope_covers_every_manifest_state() {
        use beamtalk_core::semantic_analysis::KnowledgeScope;

        assert_eq!(
            package_checker_scope(None),
            PackageCheckerScope::module_only()
        );

        let dir = TempDir::new().unwrap();
        let root = write_manifest(&dir, "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n");
        let scope = package_checker_scope(Some(&root));
        assert_eq!(scope.knowledge_scope, KnowledgeScope::ProjectComplete);
        assert!(!scope.has_package_dependencies);
        assert_eq!(scope.package_name.as_deref(), Some("my_pkg"));

        let dir = TempDir::new().unwrap();
        let root = write_manifest(
            &dir,
            "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n\n[dependencies]\nfoo = { path = \"../foo\" }\n",
        );
        assert!(package_checker_scope(Some(&root)).has_package_dependencies);

        let dir = TempDir::new().unwrap();
        let root = write_manifest(&dir, "[package\nnot toml");
        let scope = package_checker_scope(Some(&root));
        assert_eq!(scope.knowledge_scope, KnowledgeScope::ProjectComplete);
        assert!(
            scope.has_package_dependencies,
            "a malformed manifest conservatively assumes dependencies"
        );
        assert_eq!(scope.package_name, None);

        let dir = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        assert_eq!(
            package_checker_scope(Some(&root)),
            PackageCheckerScope::module_only(),
            "a root with no manifest is not a package"
        );
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
    fn test_package_name_for_project() {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            r#"
[package]
name = "named_app"
version = "0.1.0"
"#,
        );
        assert_eq!(
            package_name_for_project(path.as_str()),
            Some("named_app".to_string())
        );
    }

    #[test]
    fn test_package_name_for_project_missing_or_malformed_is_none() {
        let temp = TempDir::new().unwrap();
        let missing = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        assert_eq!(package_name_for_project(missing.as_str()), None);

        let bad = TempDir::new().unwrap();
        let bad_path = write_manifest(&bad, "not valid toml {{{{");
        assert_eq!(package_name_for_project(bad_path.as_str()), None);
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
            &format!(
                "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n[dependencies]\n{entry}\n"
            ),
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
        assert_eq!(gun.hex_constraint(), Some("~> 2.1"));
        let cowboy = &manifest.native_dependencies["cowboy"];
        assert_eq!(cowboy.name, "cowboy");
        assert_eq!(cowboy.hex_constraint(), Some("~> 2.12"));
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
        assert_eq!(
            manifest.native_dependencies["jason"].hex_constraint(),
            Some("~> 1.4.0")
        );
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
        assert_eq!(
            manifest.native_dependencies["cowboy"].hex_constraint(),
            Some("2.12.0")
        );
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
            manifest.native_dependencies["ranch"].hex_constraint(),
            Some(">= 1.0.0 and < 2.0.0")
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
        assert_eq!(
            manifest.native_dependencies["dep_ge"].hex_constraint(),
            Some(">= 1.0")
        );
        assert_eq!(
            manifest.native_dependencies["dep_gt"].hex_constraint(),
            Some("> 1.0")
        );
        assert_eq!(
            manifest.native_dependencies["dep_le"].hex_constraint(),
            Some("<= 3.0")
        );
        assert_eq!(
            manifest.native_dependencies["dep_lt"].hex_constraint(),
            Some("< 2.0.0")
        );
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
            err.contains("version constraint string or a git"),
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

    // --- BT-4555: native git sources and [native.overrides] ---

    fn parse_native_toml(native: &str) -> Result<ParsedManifest> {
        let temp = TempDir::new().unwrap();
        let path = write_manifest(
            &temp,
            &format!("[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n{native}"),
        );
        parse_manifest_full(&path.join("beamtalk.toml"))
    }

    fn native_err(native: &str) -> String {
        format!("{:?}", parse_native_toml(native).unwrap_err())
    }

    #[test]
    fn test_parse_native_git_dep_branch_tag_rev() {
        let manifest = parse_native_toml(
            r#"
[native.dependencies]
oidcc = { git = "https://github.com/jamesc/oidcc", branch = "rfc7009-revocation" }
jose = { git = "https://github.com/potatosalad/erlang-jose", tag = "1.11.10" }
yamerl = { git = "https://github.com/yakaz/yamerl", rev = "abc1234" }
gun = "~> 2.1"
"#,
        )
        .unwrap();

        let deps = &manifest.native_dependencies;
        assert_eq!(
            deps["oidcc"].source,
            NativeSource::Git {
                url: "https://github.com/jamesc/oidcc".to_string(),
                reference: GitReference::Branch("rfc7009-revocation".to_string()),
            }
        );
        assert_eq!(
            deps["jose"].source,
            NativeSource::Git {
                url: "https://github.com/potatosalad/erlang-jose".to_string(),
                reference: GitReference::Tag("1.11.10".to_string()),
            }
        );
        assert_eq!(
            deps["yamerl"].source,
            NativeSource::Git {
                url: "https://github.com/yakaz/yamerl".to_string(),
                reference: GitReference::Rev("abc1234".to_string()),
            }
        );
        assert_eq!(deps["oidcc"].hex_constraint(), None);
        assert_eq!(deps["gun"].hex_constraint(), Some("~> 2.1"));
    }

    #[test]
    fn test_reject_native_git_dep_without_ref() {
        let err = native_err(
            "[native.dependencies]\noidcc = { git = \"https://github.com/jamesc/oidcc\" }\n",
        );
        assert!(
            err.contains("Native dependency 'oidcc' is a git dependency but has no ref"),
            "{err}"
        );
    }

    #[test]
    fn test_reject_native_git_dep_multiple_refs() {
        let err = native_err(
            "[native.dependencies]\noidcc = { git = \"https://x/oidcc\", branch = \"main\", tag = \"v1\" }\n",
        );
        assert!(err.contains("specifies multiple git references"), "{err}");
    }

    #[test]
    fn test_reject_native_git_dep_ref_key_points_at_rev() {
        let err = native_err(
            "[native.dependencies]\noidcc = { git = \"https://x/oidcc\", ref = \"abc\" }\n",
        );
        assert!(err.contains("pin a commit with 'rev' instead"), "{err}");
    }

    #[test]
    fn test_reject_native_table_without_git() {
        let err = native_err("[native.dependencies]\noidcc = { version = \"3.0.0\" }\n");
        assert!(err.contains("has no 'git' URL"), "{err}");
    }

    #[test]
    fn test_reject_native_git_dep_unknown_key() {
        let err = native_err(
            "[native.dependencies]\noidcc = { git = \"https://x/oidcc\", branch = \"main\", path = \"../x\" }\n",
        );
        assert!(err.contains("invalid git source"), "{err}");
    }

    #[test]
    fn test_reject_native_git_dep_empty_url() {
        let err = native_err("[native.dependencies]\noidcc = { git = \" \", branch = \"main\" }\n");
        assert!(err.contains("empty 'git' URL"), "{err}");
    }

    #[test]
    fn test_parse_native_overrides() {
        let manifest = parse_native_toml(
            r#"
[native.dependencies]
oidcc = "~> 3.0"

[native.overrides]
jose = { erl_opts = [] }
cowlib = { erl_opts = ["debug_info", "nowarn_export_all"] }
"#,
        )
        .unwrap();

        assert_eq!(manifest.native_overrides.len(), 2);
        assert_eq!(
            manifest.native_overrides["jose"].erl_opts,
            Vec::<String>::new()
        );
        assert_eq!(
            manifest.native_overrides["cowlib"].erl_opts,
            vec!["debug_info".to_string(), "nowarn_export_all".to_string()]
        );
    }

    #[test]
    fn test_no_native_overrides_section() {
        let manifest = parse_native_toml("[native.dependencies]\ngun = \"~> 2.1\"\n").unwrap();
        assert!(manifest.native_overrides.is_empty());
    }

    #[test]
    fn test_reject_native_override_non_atom_erl_opt() {
        let err = native_err("[native.overrides]\njose = { erl_opts = [\"{d, 'X'}\"] }\n");
        assert!(err.contains("invalid erl_opts entry"), "{err}");
    }

    #[test]
    fn test_reject_native_override_unknown_key() {
        let err = native_err("[native.overrides]\njose = { deps = [] }\n");
        assert!(
            err.contains("Native override 'jose' has an invalid format"),
            "{err}"
        );
    }

    #[test]
    fn test_reject_native_override_without_erl_opts() {
        let err = native_err("[native.overrides]\njose = {}\n");
        assert!(
            err.contains("Native override 'jose' has an invalid format"),
            "{err}"
        );
    }

    #[test]
    fn test_parse_native_git_dep_trims_url() {
        let manifest = parse_native_toml(
            "[native.dependencies]\noidcc = { git = \" https://x/oidcc \", branch = \"main\" }\n",
        )
        .unwrap();
        assert_eq!(
            manifest.native_dependencies["oidcc"].source,
            NativeSource::Git {
                url: "https://x/oidcc".to_string(),
                reference: GitReference::Branch("main".to_string()),
            }
        );
    }

    #[test]
    fn test_reject_native_override_invalid_name() {
        let err = native_err("[native.overrides]\nJose = { erl_opts = [] }\n");
        assert!(err.contains("must start with a lowercase letter"), "{err}");
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

    #[test]
    fn test_hex_constraint_matches() {
        let cases = [
            ("~> 2.1", "2.1.0", true),
            ("~> 2.1", "2.9.3", true),
            ("~> 2.1", "3.0.0", false),
            ("~> 2.1", "2.0.9", false),
            ("~> 2.1.0", "2.1.7", true),
            ("~> 2.1.0", "2.2.0", false),
            ("~> 2.1", "2.2.0-rc.1", false),
            ("2.12.0", "2.12.0", true),
            ("2.12", "2.12.0", true),
            ("2.12.0", "2.12.1", false),
            (">= 1.0.0 and < 2.0.0", "1.5.0", true),
            (">= 1.0.0 and < 2.0.0", "2.0.0", false),
            ("> 1.0", "1.0.0", false),
            ("<= 3.0", "3.0.0", true),
            ("~> 2.1", "not-a-version", false),
        ];
        for (constraint, version, expected) in cases {
            assert_eq!(
                hex_constraint_matches(constraint, version),
                expected,
                "{constraint} vs {version}"
            );
        }
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
}
