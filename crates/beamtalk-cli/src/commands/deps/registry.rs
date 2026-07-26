// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package registry index resolution (BT-2978).
//!
//! **DDD Context:** Build System
//!
//! A registry dependency (`yaml = "0.2.1"` in `beamtalk.toml`) is resolved
//! through a *registry index* into a `(git url, tag)` pair, which then flows
//! through the existing git-dependency machinery unchanged.
//!
//! ## Index layout
//!
//! The index is a directory (typically a git repository) containing one TOML
//! file per package under `packages/`:
//!
//! ```toml
//! # packages/yaml.toml
//! name = "yaml"
//! description = "YAML parsing for Beamtalk"
//!
//! [[versions]]
//! version = "0.2.1"
//! git = "https://github.com/jamesc/beamtalk-yaml"
//! tag = "v0.2.1"   # optional — defaults to "v{version}"
//! ```
//!
//! ## Index location
//!
//! Resolved in priority order:
//! 1. the `BEAMTALK_REGISTRY` environment variable
//! 2. `[registry] url` in the project's `beamtalk.toml`
//! 3. [`DEFAULT_REGISTRY_URL`]
//!
//! A value naming an existing local directory is read in place — no git, no
//! network. Anything else is treated as a git URL and cloned into
//! `_build/registry/index/`. The clone is refreshed only on a lookup miss
//! (retried once) and by `beamtalk deps update`.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use serde::Deserialize;
use std::cmp::Ordering;
use std::process::Command;
use tracing::{debug, info};

use crate::commands::build_layout::BuildLayout;
use crate::commands::manifest::RegistryConfig;

/// The environment variable overriding the registry index location.
pub const REGISTRY_ENV_VAR: &str = "BEAMTALK_REGISTRY";

/// The registry index used when neither the environment nor the manifest
/// selects one.
pub const DEFAULT_REGISTRY_URL: &str = "https://github.com/jamesc/beamtalk-registry";

/// Maximum number of names listed in a "not found" error before truncating.
const MAX_LISTED: usize = 20;

/// Where the registry index lives.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegistryLocation {
    /// An existing local directory, read in place (no git, no network).
    LocalDir(Utf8PathBuf),
    /// A git URL, cloned/refreshed into `_build/registry/index/`.
    Git(String),
}

impl std::fmt::Display for RegistryLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LocalDir(path) => write!(f, "{path}"),
            Self::Git(url) => write!(f, "{url}"),
        }
    }
}

/// A single published release of a package, as recorded in the index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryRelease {
    /// The exact version (`major.minor.patch`).
    pub version: String,
    /// The git repository URL hosting this release.
    pub git: String,
    /// The git tag for this release (defaults to `v{version}` in the index).
    pub tag: String,
}

/// A parsed index entry — one package and all of its published releases.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryEntry {
    /// The package name.
    pub name: String,
    /// The package's human-readable description, if the index records one.
    pub description: Option<String>,
    /// All published releases, in the order the index lists them.
    pub versions: Vec<RegistryRelease>,
}

impl RegistryEntry {
    /// Find the release matching an exact version.
    pub fn find_version(&self, version: &str) -> Option<&RegistryRelease> {
        self.versions.iter().find(|r| r.version == version)
    }

    /// The highest published version, by numeric segment comparison.
    ///
    /// Returns `None` when the entry lists no releases.
    pub fn latest_version(&self) -> Option<&RegistryRelease> {
        self.versions
            .iter()
            .max_by(|a, b| compare_versions(&a.version, &b.version))
    }
}

// ── Index entry TOML ─────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TomlIndexEntry {
    name: String,
    #[serde(default)]
    description: Option<String>,
    #[serde(default)]
    versions: Vec<TomlIndexRelease>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TomlIndexRelease {
    version: String,
    git: String,
    #[serde(default)]
    tag: Option<String>,
}

/// Parse an index entry from TOML text.
///
/// `expected_name` is the package name the file was looked up under; a
/// mismatch against the entry's own `name` is an error, since it would
/// silently resolve the wrong package.
///
/// # Errors
///
/// Returns an error if the TOML is malformed, carries unknown fields, or
/// declares a name other than `expected_name`.
pub fn parse_index_entry(expected_name: &str, content: &str) -> Result<RegistryEntry> {
    let parsed: TomlIndexEntry = toml::from_str(content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Invalid registry index entry for package '{expected_name}'"))?;

    if parsed.name != expected_name {
        miette::bail!(
            "Registry index entry for '{expected_name}' declares a different package name '{}'.\n  \
             The index is inconsistent — report this to the registry maintainer.",
            parsed.name
        );
    }

    let versions = parsed
        .versions
        .into_iter()
        .map(|r| RegistryRelease {
            tag: r.tag.unwrap_or_else(|| format!("v{}", r.version)),
            version: r.version,
            git: r.git,
        })
        .collect();

    Ok(RegistryEntry {
        name: parsed.name,
        description: parsed.description,
        versions,
    })
}

// ── Location resolution ──────────────────────────────────────────────

/// Determine where the registry index lives.
///
/// Priority: `BEAMTALK_REGISTRY` → `[registry] url` → [`DEFAULT_REGISTRY_URL`].
/// A value naming an existing directory becomes [`RegistryLocation::LocalDir`];
/// anything else is treated as a git URL.
pub fn resolve_registry_location(registry: Option<&RegistryConfig>) -> RegistryLocation {
    let env_value = std::env::var(REGISTRY_ENV_VAR).ok();
    resolve_registry_location_from(env_value.as_deref(), registry)
}

/// The pure core of [`resolve_registry_location`], with the environment
/// supplied by the caller so it can be exercised without mutating the
/// process-wide environment.
fn resolve_registry_location_from(
    env_value: Option<&str>,
    registry: Option<&RegistryConfig>,
) -> RegistryLocation {
    let raw = env_value
        .filter(|v| !v.trim().is_empty())
        .map(str::to_string)
        .or_else(|| registry.map(|r| r.url.clone()))
        .unwrap_or_else(|| DEFAULT_REGISTRY_URL.to_string());

    classify_location(&raw)
}

/// Classify a raw registry location string as a local directory or a git URL.
fn classify_location(raw: &str) -> RegistryLocation {
    let candidate = Utf8Path::new(raw);
    if candidate.is_dir() {
        RegistryLocation::LocalDir(candidate.to_path_buf())
    } else {
        RegistryLocation::Git(raw.to_string())
    }
}

// ── Index materialisation ────────────────────────────────────────────

/// Ensure the registry index is available on disk and return its root
/// directory (the one containing `packages/`).
///
/// A local-directory registry is returned as-is. A git registry is cloned into
/// `_build/registry/index/` on first use; when `refresh` is set an existing
/// clone is fast-forwarded (falling back to a fresh clone if that fails).
///
/// # Errors
///
/// Returns an error if the local directory has no `packages/` subdirectory, or
/// if cloning the git registry fails.
pub fn ensure_index(
    location: &RegistryLocation,
    project_root: &Utf8Path,
    refresh: bool,
) -> Result<Utf8PathBuf> {
    match location {
        RegistryLocation::LocalDir(dir) => {
            let packages = dir.join("packages");
            if !packages.is_dir() {
                miette::bail!(
                    "Registry '{dir}' is not a valid registry index — expected a 'packages' \
                     directory at '{packages}'.\n\n  \
                     Set {REGISTRY_ENV_VAR} or [registry] url in beamtalk.toml to a registry \
                     index directory or git URL."
                );
            }
            Ok(dir.clone())
        }
        RegistryLocation::Git(url) => ensure_git_index(url, project_root, refresh),
    }
}

/// Clone or refresh the git registry index into `_build/registry/index/`.
fn ensure_git_index(url: &str, project_root: &Utf8Path, refresh: bool) -> Result<Utf8PathBuf> {
    let index_dir = BuildLayout::new(project_root).registry_index_dir();

    if index_dir.join("packages").is_dir() {
        if !refresh {
            debug!(%index_dir, "Using existing registry index");
            return Ok(index_dir);
        }

        info!(url, "Refreshing registry index");
        if fast_forward_index(&index_dir) {
            return Ok(index_dir);
        }
        debug!(%index_dir, "Fast-forward failed, re-cloning registry index");
    }

    clone_index(url, &index_dir)?;
    Ok(index_dir)
}

/// Try to fast-forward an existing index clone. Returns `false` when the
/// caller should fall back to a fresh clone.
fn fast_forward_index(index_dir: &Utf8Path) -> bool {
    Command::new("git")
        .args(["pull", "--quiet", "--ff-only"])
        .current_dir(index_dir)
        .output()
        .is_ok_and(|out| out.status.success())
}

/// Clone the registry index, replacing any existing directory.
fn clone_index(url: &str, index_dir: &Utf8Path) -> Result<()> {
    if index_dir.exists() {
        std::fs::remove_dir_all(index_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove stale registry index '{index_dir}'"))?;
    }

    if let Some(parent) = index_dir.parent() {
        std::fs::create_dir_all(parent)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create registry directory '{parent}'"))?;
    }

    info!(url, %index_dir, "Cloning registry index");
    let output = Command::new("git")
        .args(["clone", "--quiet", "--depth", "1", url, index_dir.as_str()])
        .output()
        .into_diagnostic()
        .wrap_err("Failed to execute 'git clone' for the registry index")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!(
            "Failed to clone the package registry from '{url}'\n\n\
             git clone failed:\n{stderr}\n\n  \
             Check the URL and your network connection, or point {REGISTRY_ENV_VAR} at a \
             local registry index directory."
        );
    }

    Ok(())
}

// ── Lookup ───────────────────────────────────────────────────────────

/// Read a package's index entry.
///
/// Returns `Ok(None)` when the package has no entry in this index — a lookup
/// miss the caller may retry after refreshing.
///
/// # Errors
///
/// Returns an error if the entry file exists but cannot be read or parsed.
pub fn read_entry(index_root: &Utf8Path, name: &str) -> Result<Option<RegistryEntry>> {
    // Package names are validated at manifest-parse time, but the name reaches
    // the filesystem here — refuse anything that could escape `packages/`.
    if name.is_empty() || name.contains(['/', '\\']) || name.contains("..") {
        miette::bail!("Invalid registry package name '{name}'");
    }

    let entry_path = index_root.join("packages").join(format!("{name}.toml"));
    if !entry_path.is_file() {
        return Ok(None);
    }

    let content = std::fs::read_to_string(&entry_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read registry index entry '{entry_path}'"))?;

    parse_index_entry(name, &content)
        .wrap_err_with(|| format!("Failed to parse registry index entry '{entry_path}'"))
        .map(Some)
}

/// Resolve a `(package, exact version)` pair to a `(git url, tag)` release.
///
/// Consults the index without touching the network first; on a miss (unknown
/// package *or* unknown version) the index is refreshed once and the lookup
/// retried, so a newly published version is picked up without a manual
/// `deps update`.
///
/// # Errors
///
/// Returns an error if the index is unavailable, the package has no entry, or
/// the requested version is not published. The error lists what *is*
/// available.
pub fn resolve_release(
    project_root: &Utf8Path,
    location: &RegistryLocation,
    name: &str,
    version: &str,
) -> Result<RegistryRelease> {
    // First attempt: whatever index is already on disk.
    let index_root = ensure_index(location, project_root, false)?;
    if let Some(entry) = read_entry(&index_root, name)? {
        if let Some(release) = entry.find_version(version) {
            return Ok(release.clone());
        }
    }

    // Miss — refresh the index once and retry before failing.
    debug!(
        name,
        version, "Registry lookup miss, refreshing index and retrying"
    );
    let index_root = ensure_index(location, project_root, true)?;
    let entry = read_entry(&index_root, name)?;

    let Some(entry) = entry else {
        miette::bail!(
            "Package '{name}' was not found in the registry ({location}).\n\n  \
             {}\n\n  \
             Check the spelling, or declare it as a git dependency:\n    \
             {name} = {{ git = \"https://...\", tag = \"v{version}\" }}",
            describe_available_packages(&index_root)
        );
    };

    if let Some(release) = entry.find_version(version) {
        return Ok(release.clone());
    }

    let available = if entry.versions.is_empty() {
        "The registry lists no published versions for this package.".to_string()
    } else {
        let mut versions: Vec<&str> = entry.versions.iter().map(|r| r.version.as_str()).collect();
        versions.sort_by(|a, b| compare_versions(b, a));
        format!("Available versions: {}", truncated_list(&versions))
    };

    let latest_hint = entry.latest_version().map_or_else(String::new, |latest| {
        format!(
            "\n\n  The latest version is {}:\n    {name} = \"{}\"",
            latest.version, latest.version
        )
    });

    miette::bail!(
        "Version '{version}' of package '{name}' was not found in the registry ({location}).\n\n  \
         {available}{latest_hint}"
    );
}

/// Describe which packages the index does contain, for a not-found error.
fn describe_available_packages(index_root: &Utf8Path) -> String {
    let mut names = Vec::new();
    if let Ok(entries) = std::fs::read_dir(index_root.join("packages")) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().is_some_and(|e| e == "toml") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    names.push(stem.to_string());
                }
            }
        }
    }

    if names.is_empty() {
        return "The registry index contains no packages.".to_string();
    }

    names.sort();
    let refs: Vec<&str> = names.iter().map(String::as_str).collect();
    format!("Packages in the registry: {}", truncated_list(&refs))
}

/// Join a list for display, truncating past [`MAX_LISTED`] entries.
fn truncated_list(items: &[&str]) -> String {
    if items.len() <= MAX_LISTED {
        return items.join(", ");
    }
    format!(
        "{}, ... ({} more)",
        items[..MAX_LISTED].join(", "),
        items.len() - MAX_LISTED
    )
}

// ── Version ordering ─────────────────────────────────────────────────

/// Compare two version strings by numeric segments.
///
/// Segments are compared as integers, so `0.10.0` sorts above `0.9.1` (which a
/// lexicographic comparison would get wrong). Missing trailing segments count
/// as zero, and any non-numeric segment compares as zero — the manifest parser
/// already rejects non-numeric versions, so this only affects malformed index
/// entries, which sort low rather than panicking.
pub fn compare_versions(a: &str, b: &str) -> Ordering {
    let mut left = a.split('.');
    let mut right = b.split('.');

    loop {
        match (left.next(), right.next()) {
            (None, None) => return Ordering::Equal,
            (l, r) => {
                let lv = l.and_then(|s| s.parse::<u64>().ok()).unwrap_or(0);
                let rv = r.and_then(|s| s.parse::<u64>().ok()).unwrap_or(0);
                match lv.cmp(&rv) {
                    Ordering::Equal => {}
                    other => return other,
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn utf8(dir: &TempDir) -> Utf8PathBuf {
        Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap()
    }

    /// Render an error as a single line — miette's `Debug` output hard-wraps
    /// to the terminal width, which would otherwise break phrase assertions.
    fn flat_err(err: &miette::Report) -> String {
        format!("{err:?}")
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Build a local registry index directory with the given package files.
    fn make_index(entries: &[(&str, &str)]) -> (TempDir, Utf8PathBuf) {
        let dir = TempDir::new().unwrap();
        let root = utf8(&dir);
        std::fs::create_dir_all(root.join("packages")).unwrap();
        for (name, content) in entries {
            std::fs::write(root.join("packages").join(format!("{name}.toml")), content).unwrap();
        }
        (dir, root)
    }

    const YAML_ENTRY: &str = r#"
name = "yaml"
description = "YAML parsing for Beamtalk"

[[versions]]
version = "0.1.0"
git = "https://example.test/yaml"

[[versions]]
version = "0.2.1"
git = "https://example.test/yaml"
tag = "release-0.2.1"
"#;

    // ── Entry parsing ────────────────────────────────────────────────

    #[test]
    fn test_parse_index_entry_with_explicit_tag() {
        let entry = parse_index_entry("yaml", YAML_ENTRY).unwrap();
        assert_eq!(entry.name, "yaml");
        assert_eq!(
            entry.description.as_deref(),
            Some("YAML parsing for Beamtalk")
        );
        assert_eq!(entry.versions.len(), 2);
        assert_eq!(entry.find_version("0.2.1").unwrap().tag, "release-0.2.1");
    }

    #[test]
    fn test_parse_index_entry_tag_defaults_to_v_prefix() {
        let entry = parse_index_entry("yaml", YAML_ENTRY).unwrap();
        assert_eq!(entry.find_version("0.1.0").unwrap().tag, "v0.1.0");
    }

    #[test]
    fn test_parse_index_entry_rejects_name_mismatch() {
        let err = parse_index_entry("json", YAML_ENTRY).unwrap_err();
        let msg = flat_err(&err);
        assert!(msg.contains("different package name"), "{msg}");
    }

    #[test]
    fn test_parse_index_entry_rejects_unknown_fields() {
        let content = "name = \"yaml\"\nbogus = true\n";
        assert!(parse_index_entry("yaml", content).is_err());
    }

    #[test]
    fn test_parse_index_entry_with_no_versions() {
        let entry = parse_index_entry("yaml", "name = \"yaml\"\n").unwrap();
        assert!(entry.versions.is_empty());
        assert!(entry.latest_version().is_none());
    }

    // ── Version ordering ─────────────────────────────────────────────

    #[test]
    fn test_compare_versions_numeric_not_lexicographic() {
        assert_eq!(compare_versions("0.10.0", "0.9.1"), Ordering::Greater);
        assert_eq!(compare_versions("0.9.1", "0.10.0"), Ordering::Less);
        assert_eq!(compare_versions("1.0.0", "1.0.0"), Ordering::Equal);
        assert_eq!(compare_versions("2.0.0", "1.99.99"), Ordering::Greater);
    }

    #[test]
    fn test_compare_versions_missing_segments_are_zero() {
        assert_eq!(compare_versions("1.0", "1.0.0"), Ordering::Equal);
        assert_eq!(compare_versions("1.0", "1.0.1"), Ordering::Less);
    }

    #[test]
    fn test_latest_version_picks_highest_numeric() {
        let content = r#"
name = "p"
[[versions]]
version = "0.9.1"
git = "g"
[[versions]]
version = "0.10.0"
git = "g"
[[versions]]
version = "0.2.0"
git = "g"
"#;
        let entry = parse_index_entry("p", content).unwrap();
        assert_eq!(entry.latest_version().unwrap().version, "0.10.0");
    }

    // ── Location resolution ──────────────────────────────────────────

    #[test]
    fn test_classify_location_existing_dir_is_local() {
        let dir = TempDir::new().unwrap();
        let root = utf8(&dir);
        assert_eq!(
            classify_location(root.as_str()),
            RegistryLocation::LocalDir(root)
        );
    }

    #[test]
    fn test_classify_location_url_is_git() {
        assert_eq!(
            classify_location("https://example.test/registry"),
            RegistryLocation::Git("https://example.test/registry".to_string())
        );
    }

    #[test]
    fn test_location_falls_back_to_default() {
        assert_eq!(
            resolve_registry_location_from(None, None),
            RegistryLocation::Git(DEFAULT_REGISTRY_URL.to_string())
        );
    }

    #[test]
    fn test_location_prefers_manifest_over_default() {
        let cfg = RegistryConfig {
            url: "https://example.test/custom".to_string(),
        };
        assert_eq!(
            resolve_registry_location_from(None, Some(&cfg)),
            RegistryLocation::Git("https://example.test/custom".to_string())
        );
    }

    #[test]
    fn test_location_prefers_env_over_manifest() {
        let cfg = RegistryConfig {
            url: "https://example.test/from-manifest".to_string(),
        };
        assert_eq!(
            resolve_registry_location_from(Some("https://example.test/from-env"), Some(&cfg)),
            RegistryLocation::Git("https://example.test/from-env".to_string())
        );
    }

    #[test]
    fn test_location_ignores_blank_env() {
        let cfg = RegistryConfig {
            url: "https://example.test/from-manifest".to_string(),
        };
        assert_eq!(
            resolve_registry_location_from(Some("   "), Some(&cfg)),
            RegistryLocation::Git("https://example.test/from-manifest".to_string())
        );
    }

    // ── ensure_index ─────────────────────────────────────────────────

    #[test]
    fn test_ensure_index_local_dir_passthrough() {
        let (_dir, root) = make_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let resolved = ensure_index(
            &RegistryLocation::LocalDir(root.clone()),
            &utf8(&project),
            false,
        )
        .unwrap();
        assert_eq!(resolved, root);
    }

    #[test]
    fn test_ensure_index_local_dir_without_packages_errors() {
        let dir = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();
        let err = ensure_index(
            &RegistryLocation::LocalDir(utf8(&dir)),
            &utf8(&project),
            false,
        )
        .unwrap_err();
        let msg = flat_err(&err);
        assert!(msg.contains("packages"), "{msg}");
    }

    // ── Lookup ───────────────────────────────────────────────────────

    #[test]
    fn test_read_entry_hit() {
        let (_dir, root) = make_index(&[("yaml", YAML_ENTRY)]);
        let entry = read_entry(&root, "yaml").unwrap().unwrap();
        assert_eq!(entry.name, "yaml");
    }

    #[test]
    fn test_read_entry_miss_returns_none() {
        let (_dir, root) = make_index(&[("yaml", YAML_ENTRY)]);
        assert!(read_entry(&root, "nope").unwrap().is_none());
    }

    #[test]
    fn test_read_entry_rejects_path_traversal() {
        let (_dir, root) = make_index(&[("yaml", YAML_ENTRY)]);
        assert!(read_entry(&root, "../secrets").is_err());
        assert!(read_entry(&root, "a/b").is_err());
    }

    #[test]
    fn test_resolve_release_hit() {
        let (_dir, root) = make_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let location = RegistryLocation::LocalDir(root);

        let release = resolve_release(&utf8(&project), &location, "yaml", "0.2.1").unwrap();
        assert_eq!(release.version, "0.2.1");
        assert_eq!(release.git, "https://example.test/yaml");
        assert_eq!(release.tag, "release-0.2.1");
    }

    #[test]
    fn test_resolve_release_defaults_tag_from_version() {
        let (_dir, root) = make_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let location = RegistryLocation::LocalDir(root);

        let release = resolve_release(&utf8(&project), &location, "yaml", "0.1.0").unwrap();
        assert_eq!(release.tag, "v0.1.0");
    }

    #[test]
    fn test_resolve_release_unknown_package_lists_available() {
        let (_dir, root) = make_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let location = RegistryLocation::LocalDir(root);

        let err = resolve_release(&utf8(&project), &location, "json", "1.0.0").unwrap_err();
        let msg = flat_err(&err);
        assert!(msg.contains("not found in the registry"), "{msg}");
        assert!(
            msg.contains("yaml"),
            "should list available packages: {msg}"
        );
    }

    #[test]
    fn test_resolve_release_unknown_version_lists_versions() {
        let (_dir, root) = make_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let location = RegistryLocation::LocalDir(root);

        let err = resolve_release(&utf8(&project), &location, "yaml", "9.9.9").unwrap_err();
        let msg = flat_err(&err);
        assert!(msg.contains("Available versions"), "{msg}");
        assert!(msg.contains("0.2.1"), "{msg}");
        assert!(msg.contains("0.1.0"), "{msg}");
        assert!(
            msg.contains("The latest version is 0.2.1"),
            "should suggest the latest: {msg}"
        );
    }
}
