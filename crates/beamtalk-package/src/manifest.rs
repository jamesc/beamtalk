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

mod hex;
mod native;

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

#[cfg(test)]
mod tests;
