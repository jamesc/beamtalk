// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package manifest parsing and validation.
//!
//! **DDD Context:** Build System
//!
//! Parses `beamtalk.toml` manifests that define a package's identity and metadata.
//! See ADR 0026 for the package definition and manifest format.
//!
//! This module lives in `beamtalk-package` (not the `beamtalk` binary) so
//! `beamtalk-mcp` can depend on it too; the CLI imports
//! `beamtalk_package::manifest` directly. Every item the CLI uses must be
//! `pub`, not `pub(crate)`.

mod git_url;
mod hex;
mod native;

pub use git_url::{
    ALLOW_INSECURE_GIT_ENV, check_git_transport, insecure_git_allowed, is_plaintext_git_url,
    redact_url_credentials, validate_git_url, validate_registry_release_url,
};
pub use hex::hex_constraint_matches;
pub use native::{
    NativeDependency, NativeDependencyMap, NativeOverride, NativeOverrideMap, NativeSource,
};
use native::{NativeSection, parse_native_dependencies, parse_native_overrides};

use beamtalk_core::compilation::{DependencyMap, parse_diagnostics_table};
#[cfg(test)]
use beamtalk_core::compilation::{DependencySource, DependencySpec};
#[cfg(test)]
use beamtalk_core::source_analysis::DiagnosticCategory;
#[cfg(test)]
use std::path::PathBuf;
// `[dependencies]` parsing and package-name validation live in
// `beamtalk-project` so the LSP shares them (BT-4611); re-exported so
// existing `manifest::…` call sites are unchanged.
pub use beamtalk_project::dependencies::{
    TomlDependency, git_reference_from_fields, parse_dependencies, read_manifest_dependencies,
    validate_exact_version, validate_registry_dependency, value_type_name,
};
#[cfg(test)]
use beamtalk_project::package_name::{ERLANG_APPS, RESERVED_NAMES};
pub use beamtalk_project::package_name::{
    PackageNameError, format_name_error, suggest_package_name, validate_package_name,
};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use serde::Deserialize;
use std::fs;

/// The top-level manifest structure parsed from `beamtalk.toml`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
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

/// OTP application configuration from the `[application]` section of `beamtalk.toml`.
///
/// When present, `beamtalk build` generates an OTP application callback module
/// (`beamtalk_{appname}_app.erl`) and includes `{mod, ...}` in the `.app` file.
/// `beamtalk run` starts the package as an OTP application rather than calling
/// an imperative start method.
#[derive(Debug, Deserialize, Clone)]
#[serde(deny_unknown_fields)]
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
#[serde(deny_unknown_fields)]
pub struct PackageManifest {
    /// Package name (used for module naming and OTP application ID).
    pub name: String,
    /// Package version string (e.g. `"0.1.0"`).
    pub version: String,
    /// Optional human-readable package description.
    #[serde(default)]
    pub description: Option<String>,
    /// Optional SPDX license identifiers (e.g. `["Apache-2.0"]`). Written to
    /// the generated `.app` file and the registry index entry.
    #[serde(default)]
    pub licenses: Option<Vec<String>>,
    /// When `true`, using a class from a transitive dependency is a compile
    /// error instead of a warning (ADR 0070 Phase 3).
    /// Defaults to `false` (warning only).
    #[serde(default, rename = "strict-deps")]
    pub strict_deps: bool,
}

// ADR 0100 Rule 3: `DiagnosticSeverityOverride` and
// `DiagnosticsTable` — plus the `[diagnostics]`-table parser
// (`parse_diagnostics_table`) — moved to `beamtalk-core`
// (`beamtalk_core::compilation::diagnostics_policy`) so that `beamtalk-lsp`
// can apply the same table `beamtalk build` does without depending on
// `beamtalk-cli` (dependencies flow downward only — see
// `docs/development/architecture-principles.md`). `DiagnosticSeverityOverride`
// stays re-exported here because `manifest::DiagnosticsTable` /
// `manifest::DiagnosticSeverityOverride` are the paths manifest consumers
// (including the CLI's tests) use.
pub use beamtalk_core::compilation::DiagnosticSeverityOverride;
pub use beamtalk_core::compilation::DiagnosticsTable;

/// Validate a `[package] version`.
///
/// The version becomes the `.app` `vsn`, the release directory name and the
/// `.rel` entry, so it must be a `SemVer` 2.0 version: `major.minor.patch`
/// (non-negative integers, no leading zeros), optionally followed by
/// `-prerelease` and/or `+build` (dot-separated `[0-9A-Za-z-]` identifiers).
///
/// # Errors
///
/// Returns a message naming the accepted form when `version` is not valid.
pub fn validate_package_version(version: &str) -> std::result::Result<(), String> {
    const FORM: &str = "expected MAJOR.MINOR.PATCH with an optional -prerelease and +build suffix (e.g. 1.2.3 or 1.2.3-rc.1)";
    let (rest, build) = match version.split_once('+') {
        Some((rest, build)) => (rest, Some(build)),
        None => (version, None),
    };
    let (core, pre) = match rest.split_once('-') {
        Some((core, pre)) => (core, Some(pre)),
        None => (rest, None),
    };
    let segments: Vec<&str> = core.split('.').collect();
    let core_ok = segments.len() == 3
        && segments.iter().all(|seg| {
            !seg.is_empty()
                && seg.bytes().all(|b| b.is_ascii_digit())
                && (seg.len() == 1 || !seg.starts_with('0'))
        });
    let idents_ok = |part: Option<&str>, numeric_no_leading_zero: bool| {
        part.is_none_or(|p| {
            p.split('.').all(|id| {
                !id.is_empty()
                    && id.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'-')
                    // SemVer: a numeric prerelease identifier has no leading zero.
                    && !(numeric_no_leading_zero
                        && id.len() > 1
                        && id.starts_with('0')
                        && id.bytes().all(|b| b.is_ascii_digit()))
            })
        })
    };
    if core_ok && idents_ok(pre, true) && idents_ok(build, false) {
        Ok(())
    } else {
        Err(format!("'{version}' is not a valid version: {FORM}"))
    }
}

/// Validate the `[package]` identity fields shared by every parse entry point.
fn validate_package_identity(package: &PackageManifest) -> Result<()> {
    if let Err(e) = validate_package_name(&package.name) {
        miette::bail!("{}", format_name_error(&package.name, &e));
    }
    if let Err(msg) = validate_package_version(&package.version) {
        miette::bail!("Invalid [package] version: {msg}");
    }
    Ok(())
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

impl ParsedManifest {
    /// This package's own package-bundled FFI type stubs directory (ADR 0075
    /// layer 2), its `[stubs] path` resolved against `package_root` — `None`
    /// if it declares no `[stubs]` section. Shared by dependency resolution
    /// and the offline dependency scan MCP `lint` uses (BT-4643).
    #[must_use]
    pub fn stubs_dir(&self, package_root: &Utf8Path) -> Option<Utf8PathBuf> {
        self.stubs
            .as_ref()
            .map(|stubs| package_root.join(&stubs.path))
    }
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

    validate_package_identity(&manifest.package)?;

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
    parsed_from_raw(manifest, path)
}

/// Parse a *dependency's* `beamtalk.toml` leniently (BT-5133).
///
/// A dependency may be published with a newer toolchain that knows keys this
/// one does not. Unknown keys are dropped with a warning (once per file and
/// key per process) instead of failing the dependent's build; every other
/// problem (invalid TOML, bad package name, malformed dependency tables) is
/// still an error, reported with the path. Every read of a dependency's
/// manifest goes through here — never [`parse_manifest`] /
/// [`parse_manifest_full`], which are for the project's own manifest.
///
/// **Leniency stops at dependency specs (BT-5186).** Unknown top-level keys,
/// tables, and keys in `[package]`, `[application]`, `[stubs]`, `[registry]`
/// and `[native]` are dropped. An unknown key *inside one entry* of
/// `[dependencies]`, `[native.dependencies]` or `[native.overrides]` is still
/// an error: those keys decide what code is fetched and how it is compiled
/// (a new source kind, a version pin, compiler options), so ignoring one would
/// silently build something other than what the dependency declared.
///
/// # Errors
///
/// As [`parse_manifest_full`], except that unknown keys are not an error.
pub fn parse_dependency_manifest_full(path: &Utf8Path) -> Result<ParsedManifest> {
    let content = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read manifest '{path}'"))?;
    let (manifest, ignored) = deserialize_ignoring_unknown_keys(&content)
        .wrap_err_with(|| format!("Failed to parse manifest '{path}'"))?;
    for key in ignored {
        if first_unknown_key_warning(path, &key) {
            tracing::warn!("Ignoring unknown key `{key}` in dependency manifest '{path}'");
        }
    }
    parsed_from_raw(manifest, path)
}

/// Whether the unknown-key warning for `key` in the dependency manifest at
/// `path` has not been emitted yet by this process (BT-5186). A build reads
/// each dependency's manifest several times (discovery, stubs, `.app`
/// generation, native-dependency collection), so the warning is deduplicated
/// per manifest file and key: once per dependency per build. A long-lived
/// process (REPL, LSP) warns once for its lifetime.
fn first_unknown_key_warning(path: &Utf8Path, key: &str) -> bool {
    static WARNED: std::sync::OnceLock<
        std::sync::Mutex<std::collections::HashSet<(std::path::PathBuf, String)>>,
    > = std::sync::OnceLock::new();
    // Canonical, so `dep/./beamtalk.toml` and `dep/beamtalk.toml` are one file.
    let file = std::fs::canonicalize(path.as_std_path())
        .unwrap_or_else(|_| path.as_std_path().to_path_buf());
    WARNED
        .get_or_init(Default::default)
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert((file, key.to_string()))
}

/// Deserialize `content`, removing each unknown key the strict parse rejects
/// and retrying, so only the unknown keys are tolerated. Returns the manifest
/// and the names of the dropped keys. A rejection that is not an unknown key
/// (or whose key cannot be located on a single line) is returned as is.
fn deserialize_ignoring_unknown_keys(content: &str) -> Result<(Manifest, Vec<String>)> {
    const MAX_DROPPED: usize = 32;
    let mut text = content.to_string();
    let mut dropped = Vec::new();
    loop {
        let err = match toml::from_str::<Manifest>(&text) {
            Ok(manifest) => return Ok((manifest, dropped)),
            Err(err) => err,
        };
        let Some(span) = err
            .span()
            .filter(|_| err.message().starts_with("unknown field") && dropped.len() < MAX_DROPPED)
        else {
            return Err(miette::miette!("{err}"));
        };
        let line_start = text[..span.start].rfind('\n').map_or(0, |i| i + 1);
        let line_end = text[span.start..]
            .find('\n')
            .map_or(text.len(), |i| span.start + i + 1);
        let line = &text[line_start..line_end];
        let remove_end = if line.trim_start().starts_with('[') {
            // An unknown table: drop its header and body, up to the next header.
            let mut end = line_end;
            while end < text.len() {
                let next = text[end..].find('\n').map_or(text.len(), |i| end + i + 1);
                if text[end..next].trim_start().starts_with('[') {
                    break;
                }
                end = next;
            }
            end
        } else {
            // Only a plain single-line `key = value` can be dropped safely.
            let single_line_value = line.split_once('=').is_some_and(|(_, v)| {
                let v = v.trim();
                !v.is_empty() && !v.ends_with('[') && !v.ends_with('{') && !v.contains("\"\"\"")
            });
            if !single_line_value {
                return Err(miette::miette!("{err}"));
            }
            line_end
        };
        dropped.push(text[span.start..span.end].to_string());
        text.replace_range(line_start..remove_end, "");
    }
}

/// Validate a deserialized [`Manifest`] into a [`ParsedManifest`].
fn parsed_from_raw(manifest: Manifest, path: &Utf8Path) -> Result<ParsedManifest> {
    validate_package_identity(&manifest.package)?;

    let dependencies = parse_dependencies(manifest.dependencies.as_ref())
        .wrap_err_with(|| format!("Failed to parse [dependencies] in '{path}'"))?;

    let native_dependencies = parse_native_dependencies(manifest.native.as_ref())
        .wrap_err_with(|| format!("Failed to parse [native.dependencies] in '{path}'"))?;
    if native_dependencies.contains_key(&manifest.package.name) {
        miette::bail!(
            "Native dependency '{}' has the same name as this package; the two would \
             generate colliding .app files",
            manifest.package.name
        );
    }

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
    beamtalk_project::package::read_package_name(std::path::Path::new(project_root))
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
    /// The package name, for E0401/E0402 visibility checks; `None` outside a
    /// package or when the manifest does not parse.
    pub package_name: Option<String>,
    /// The package names a `pkg@Class` qualifier may name (ADR 0070 Phase 2):
    /// see [`beamtalk_core::semantic_analysis::known_qualifier_packages`].
    /// `None` outside a package or when the manifest does not parse, which
    /// skips the check.
    pub known_packages: Option<std::collections::HashSet<String>>,
}

impl PackageCheckerScope {
    /// The scope for a file outside any package.
    #[must_use]
    pub fn module_only() -> Self {
        Self {
            knowledge_scope: beamtalk_core::semantic_analysis::KnowledgeScope::ModuleOnly,
            package_name: None,
            known_packages: None,
        }
    }

    /// The scope for an already-parsed manifest (`None`: no manifest, so
    /// not a package).
    #[must_use]
    pub fn for_manifest(manifest: Option<&ParsedManifest>) -> Self {
        manifest.map_or_else(Self::module_only, |manifest| Self {
            knowledge_scope: beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete,
            package_name: Some(manifest.package.name.clone()),
            known_packages: Some(beamtalk_core::semantic_analysis::known_qualifier_packages(
                Some(&manifest.package.name),
                manifest.dependencies.keys().map(String::as_str),
            )),
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
                    package_name: None,
                    known_packages: None,
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
    validate_package_identity(&manifest.package)?;
    Ok(manifest.application)
}

#[cfg(test)]
mod tests;
