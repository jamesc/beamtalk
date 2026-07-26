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
//! network; a relative path is taken relative to the project root. Anything
//! else is treated as a git URL and cloned into `_build/registry/index/`. The
//! clone is refreshed only on a lookup miss, retried once. (Refreshing it from
//! `beamtalk deps update` arrives with registry support for that command.)

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

// Unknown fields are deliberately *accepted* here, unlike in `beamtalk.toml`.
// The index is a separately versioned artifact served to clients this binary
// does not control: the day it starts carrying `checksum`, `yanked`, or
// `licenses`, every already-released beamtalk must keep reading it rather than
// hard-failing on every package that adopts the new key.
#[derive(Debug, Deserialize)]
struct TomlIndexEntry {
    name: String,
    #[serde(default)]
    description: Option<String>,
    #[serde(default)]
    versions: Vec<TomlIndexRelease>,
}

#[derive(Debug, Deserialize)]
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
/// Returns an error if the TOML is malformed, declares a name other than
/// `expected_name`, or lists the same version more than once. Unknown fields
/// are accepted so a newer index format stays readable.
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

    let versions: Vec<RegistryRelease> = parsed
        .versions
        .into_iter()
        .map(|r| RegistryRelease {
            tag: r.tag.unwrap_or_else(|| format!("v{}", r.version)),
            version: r.version,
            git: r.git,
        })
        .collect();

    // A duplicated version would otherwise resolve to whichever block happens
    // to come first — possibly a different repository than intended.
    for (i, release) in versions.iter().enumerate() {
        if versions[..i].iter().any(|r| r.version == release.version) {
            miette::bail!(
                "Registry index entry for '{expected_name}' lists version '{}' more than once.\n  \
                 The index is inconsistent — report this to the registry maintainer.",
                release.version
            );
        }
    }

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
pub fn resolve_registry_location(
    project_root: &Utf8Path,
    registry: Option<&RegistryConfig>,
) -> RegistryLocation {
    let env_value = std::env::var(REGISTRY_ENV_VAR).ok();
    resolve_registry_location_from(project_root, env_value.as_deref(), registry)
}

/// The pure core of [`resolve_registry_location`], with the environment
/// supplied by the caller so it can be exercised without mutating the
/// process-wide environment.
fn resolve_registry_location_from(
    project_root: &Utf8Path,
    env_value: Option<&str>,
    registry: Option<&RegistryConfig>,
) -> RegistryLocation {
    let raw = env_value
        .filter(|v| !v.trim().is_empty())
        .map(str::to_string)
        .or_else(|| registry.map(|r| r.url.clone()))
        .unwrap_or_else(|| DEFAULT_REGISTRY_URL.to_string());

    classify_location(project_root, &raw)
}

/// Classify a raw registry location string as a local directory or a git URL.
///
/// A relative path is resolved against `project_root`, not the process working
/// directory: the CLI, LSP and MCP servers all run with different cwds, and the
/// same `beamtalk.toml` must name the same registry from each of them.
fn classify_location(project_root: &Utf8Path, raw: &str) -> RegistryLocation {
    let candidate = Utf8Path::new(raw);
    if candidate.is_absolute() {
        if candidate.is_dir() {
            return RegistryLocation::LocalDir(candidate.to_path_buf());
        }
    } else {
        let joined = project_root.join(candidate);
        if joined.is_dir() {
            return RegistryLocation::LocalDir(joined);
        }
    }

    RegistryLocation::Git(raw.to_string())
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

    // `swap_in_index` moves the old index aside before renaming the new one
    // into place. If a previous run was killed between those two renames, the
    // only good copy is sitting at `index.previous` — put it back before
    // deciding whether an index exists at all.
    recover_orphaned_index(&index_dir);

    let has_index = index_dir.join("packages").is_dir();

    if has_index {
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

    // Clone into a scratch directory and swap it in only once it is complete.
    // A refresh that fails (offline, VPN down, transient DNS) must leave the
    // usable index that is already on disk exactly where it was, rather than
    // deleting it up front and then failing to replace it.
    let staging_dir = BuildLayout::new(project_root)
        .registry_dir()
        .join("index.staging");

    match clone_index(url, &staging_dir) {
        Ok(()) => {
            swap_in_index(&staging_dir, &index_dir)?;
            Ok(index_dir)
        }
        Err(e) => {
            let _ = std::fs::remove_dir_all(&staging_dir);
            if has_index {
                // Keep serving the stale index — a refresh is best-effort, and
                // the lookup that triggered it reports its own error if the
                // package really is absent.
                debug!(%index_dir, error = %e, "Registry refresh failed, keeping existing index");
                Ok(index_dir)
            } else {
                Err(e)
            }
        }
    }
}

/// Restore an index stranded at `<index_dir>.previous` by an interrupted swap.
///
/// Only acts when `index_dir` itself has no usable index, so it can never
/// clobber a good checkout.
fn recover_orphaned_index(index_dir: &Utf8Path) {
    if index_dir.join("packages").is_dir() {
        return;
    }

    let previous = index_dir.with_extension("previous");
    if previous.join("packages").is_dir() {
        debug!(%previous, %index_dir, "Restoring registry index left by an interrupted swap");
        if let Err(e) = std::fs::rename(&previous, index_dir) {
            debug!(%e, "Failed to restore orphaned registry index — will re-clone");
        }
    }
}

/// Replace `index_dir` with the freshly cloned `staging_dir`.
fn swap_in_index(staging_dir: &Utf8Path, index_dir: &Utf8Path) -> Result<()> {
    if index_dir.exists() {
        let previous = index_dir.with_extension("previous");
        let _ = std::fs::remove_dir_all(&previous);
        std::fs::rename(index_dir, &previous)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to move aside registry index '{index_dir}'"))?;
        let result = std::fs::rename(staging_dir, index_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to install registry index at '{index_dir}'"));
        if result.is_err() {
            // Put the working index back rather than leaving nothing behind.
            let _ = std::fs::rename(&previous, index_dir);
            return result;
        }
        let _ = std::fs::remove_dir_all(&previous);
        return Ok(());
    }

    std::fs::rename(staging_dir, index_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to install registry index at '{index_dir}'"))
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

/// Clone the registry index into `target`, replacing anything already there.
fn clone_index(url: &str, target: &Utf8Path) -> Result<()> {
    // The registry URL can come from the environment or the manifest; screen it
    // with the same rules as a dependency URL so `ext::` and option-lookalikes
    // can't reach `git clone`.
    super::git::validate_git_url("<registry>", url)?;

    if target.exists() {
        std::fs::remove_dir_all(target)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove stale registry clone '{target}'"))?;
    }

    if let Some(parent) = target.parent() {
        std::fs::create_dir_all(parent)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create registry directory '{parent}'"))?;
    }

    info!(url, %target, "Cloning registry index");
    let output = Command::new("git")
        .args([
            "-c",
            "protocol.ext.allow=never",
            "clone",
            "--quiet",
            "--depth",
            "1",
            "--",
            url,
            target.as_str(),
        ])
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
    fn test_parse_index_entry_accepts_unknown_fields() {
        // The index is versioned independently of this binary: a future key
        // must not break clients that predate it.
        let content = r#"
name = "yaml"
yanked = false
licenses = ["Apache-2.0"]

[[versions]]
version = "0.1.0"
git = "https://example.test/yaml"
checksum = "sha256:deadbeef"
"#;
        let entry = parse_index_entry("yaml", content).unwrap();
        assert_eq!(entry.versions.len(), 1);
        assert_eq!(entry.find_version("0.1.0").unwrap().tag, "v0.1.0");
    }

    #[test]
    fn test_parse_index_entry_rejects_duplicate_versions() {
        let content = r#"
name = "yaml"

[[versions]]
version = "1.0.0"
git = "https://example.test/yaml"

[[versions]]
version = "1.0.0"
git = "https://evil.test/yaml"
"#;
        let err = parse_index_entry("yaml", content).unwrap_err();
        assert!(flat_err(&err).contains("more than once"), "{err:?}");
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

    /// A project root that exists but contains no registry directory.
    fn empty_root(dir: &TempDir) -> Utf8PathBuf {
        utf8(dir)
    }

    #[test]
    fn test_classify_location_existing_dir_is_local() {
        let dir = TempDir::new().unwrap();
        let root = utf8(&dir);
        let project = TempDir::new().unwrap();
        assert_eq!(
            classify_location(&empty_root(&project), root.as_str()),
            RegistryLocation::LocalDir(root)
        );
    }

    #[test]
    fn test_classify_location_url_is_git() {
        let project = TempDir::new().unwrap();
        assert_eq!(
            classify_location(&empty_root(&project), "https://example.test/registry"),
            RegistryLocation::Git("https://example.test/registry".to_string())
        );
    }

    #[test]
    fn test_classify_location_relative_path_resolves_against_project_root() {
        // Not the process cwd — the CLI, LSP and MCP servers all run with
        // different working directories.
        let project = TempDir::new().unwrap();
        let root = utf8(&project);
        std::fs::create_dir_all(root.join("vendor/registry/packages")).unwrap();

        assert_eq!(
            classify_location(&root, "vendor/registry"),
            RegistryLocation::LocalDir(root.join("vendor/registry"))
        );
    }

    #[test]
    fn test_location_falls_back_to_default() {
        let project = TempDir::new().unwrap();
        assert_eq!(
            resolve_registry_location_from(&empty_root(&project), None, None),
            RegistryLocation::Git(DEFAULT_REGISTRY_URL.to_string())
        );
    }

    #[test]
    fn test_location_prefers_manifest_over_default() {
        let project = TempDir::new().unwrap();
        let cfg = RegistryConfig {
            url: "https://example.test/custom".to_string(),
        };
        assert_eq!(
            resolve_registry_location_from(&empty_root(&project), None, Some(&cfg)),
            RegistryLocation::Git("https://example.test/custom".to_string())
        );
    }

    #[test]
    fn test_location_prefers_env_over_manifest() {
        let project = TempDir::new().unwrap();
        let cfg = RegistryConfig {
            url: "https://example.test/from-manifest".to_string(),
        };
        assert_eq!(
            resolve_registry_location_from(
                &empty_root(&project),
                Some("https://example.test/from-env"),
                Some(&cfg)
            ),
            RegistryLocation::Git("https://example.test/from-env".to_string())
        );
    }

    #[test]
    fn test_location_ignores_blank_env() {
        let project = TempDir::new().unwrap();
        let cfg = RegistryConfig {
            url: "https://example.test/from-manifest".to_string(),
        };
        assert_eq!(
            resolve_registry_location_from(&empty_root(&project), Some("   "), Some(&cfg)),
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

    // ── Git-backed index ─────────────────────────────────────────────
    //
    // Exercised through a local `file://` repository, so still network-free.

    fn run_git(dir: &std::path::Path, args: &[&str]) {
        let out = Command::new("git")
            .args(args)
            .current_dir(dir)
            .output()
            .unwrap();
        assert!(
            out.status.success(),
            "git {args:?} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }

    /// A git repository serving as a registry index. Returns the repo dir and
    /// a `file://` URL for it.
    fn make_git_index(entries: &[(&str, &str)]) -> (TempDir, String) {
        let (dir, root) = make_index(entries);
        let path = dir.path();
        run_git(path, &["init"]);
        run_git(path, &["config", "user.email", "t@t.com"]);
        run_git(path, &["config", "user.name", "T"]);
        run_git(path, &["config", "commit.gpgsign", "false"]);
        run_git(path, &["add", "."]);
        run_git(path, &["commit", "-m", "index"]);

        let mut s = root.as_str().replace('\\', "/");
        if !s.starts_with('/') {
            s.insert(0, '/');
        }
        (dir, format!("file://{s}"))
    }

    #[test]
    fn test_ensure_index_clones_git_registry() {
        let (_repo, url) = make_git_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let project_root = utf8(&project);

        let index_root = ensure_index(&RegistryLocation::Git(url), &project_root, false).unwrap();

        assert_eq!(
            index_root,
            BuildLayout::new(&project_root).registry_index_dir()
        );
        assert!(index_root.join("packages/yaml.toml").is_file());
        assert_eq!(
            read_entry(&index_root, "yaml").unwrap().unwrap().name,
            "yaml"
        );
    }

    #[test]
    fn test_resolve_release_through_git_registry() {
        let (_repo, url) = make_git_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let location = RegistryLocation::Git(url);

        let release = resolve_release(&utf8(&project), &location, "yaml", "0.2.1").unwrap();
        assert_eq!(release.tag, "release-0.2.1");
    }

    /// A newly published version is picked up by the miss-then-refresh retry.
    #[test]
    fn test_resolve_release_refresh_picks_up_new_version() {
        let (repo, url) = make_git_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let project_root = utf8(&project);
        let location = RegistryLocation::Git(url);

        // Populate the local clone at the current index state.
        resolve_release(&project_root, &location, "yaml", "0.2.1").unwrap();

        // Publish 0.3.0 upstream.
        let mut updated = YAML_ENTRY.to_string();
        updated
            .push_str("\n[[versions]]\nversion = \"0.3.0\"\ngit = \"https://example.test/yaml\"\n");
        std::fs::write(repo.path().join("packages/yaml.toml"), updated).unwrap();
        run_git(repo.path(), &["add", "."]);
        run_git(repo.path(), &["commit", "-m", "publish 0.3.0"]);

        let release = resolve_release(&project_root, &location, "yaml", "0.3.0").unwrap();
        assert_eq!(release.version, "0.3.0");
        assert_eq!(release.tag, "v0.3.0");
    }

    /// A refresh that cannot reach the remote must leave the working index
    /// intact rather than deleting it and failing.
    #[test]
    fn test_failed_refresh_keeps_existing_index() {
        let (repo, url) = make_git_index(&[("yaml", YAML_ENTRY)]);
        let project = TempDir::new().unwrap();
        let project_root = utf8(&project);
        let location = RegistryLocation::Git(url);

        resolve_release(&project_root, &location, "yaml", "0.2.1").unwrap();
        let index_dir = BuildLayout::new(&project_root).registry_index_dir();
        assert!(index_dir.join("packages/yaml.toml").is_file());

        // Destroy the remote, then force a refresh via a lookup miss.
        repo.close().unwrap();
        let err = resolve_release(&project_root, &location, "yaml", "9.9.9").unwrap_err();

        // The failure is about the missing version, not a broken registry, and
        // the cached index survives for the next build.
        assert!(flat_err(&err).contains("Available versions"), "{err:?}");
        assert!(
            index_dir.join("packages/yaml.toml").is_file(),
            "a failed refresh must not delete the usable index"
        );
    }

    #[test]
    fn test_ensure_index_rejects_dangerous_registry_url() {
        let project = TempDir::new().unwrap();
        let err = ensure_index(
            &RegistryLocation::Git("ext::sh -c 'touch /tmp/pwned'".to_string()),
            &utf8(&project),
            false,
        )
        .unwrap_err();
        assert!(flat_err(&err).contains("unsupported git URL"), "{err:?}");
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
