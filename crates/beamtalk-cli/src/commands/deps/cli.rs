// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI interface for dependency management commands (ADR 0070 Phase 1).
//!
//! **DDD Context:** Build System
//!
//! Provides `beamtalk deps {add, list, update}` subcommands for managing
//! package dependencies declared in `beamtalk.toml`.

use beamtalk_core::compilation::{DependencySource, GitReference};
use camino::{Utf8Path, Utf8PathBuf};
use clap::Subcommand;
use miette::{Context, IntoDiagnostic, Result};
use std::collections::BTreeMap;
use tracing::info;

use crate::commands::manifest::{self, format_name_error, validate_package_name};

use super::git;
use super::lockfile::{LockEntry, Lockfile};
use super::registry::{self, RegistryLocation};

/// Dependency management subcommands.
#[derive(Debug, Subcommand)]
pub enum DepsCommand {
    /// Add a dependency to beamtalk.toml
    ///
    /// Writes the dependency entry to `beamtalk.toml` and resolves it
    /// (cloning git repos, validating path deps, or resolving a registry
    /// version). With none of `--path`/`--git` given, NAME is resolved
    /// through the package registry — pinned to `--version` if given, else
    /// the latest published version.
    Add {
        /// Dependency package name (e.g. `json`, `utils`)
        name: String,

        /// Local filesystem path to the dependency
        #[arg(long, group = "source")]
        path: Option<String>,

        /// Git repository URL
        #[arg(long, group = "source", requires = "git_ref")]
        git: Option<String>,

        /// Exact registry version to pin (default: latest in the index).
        /// Only valid without `--path`/`--git`.
        #[arg(long, conflicts_with_all = ["path", "git", "tag", "branch", "rev"])]
        version: Option<String>,

        /// Git tag to check out (e.g. `v1.0.0`)
        #[arg(long, group = "git_ref", requires = "git")]
        tag: Option<String>,

        /// Git branch to track (e.g. `main`)
        #[arg(long, group = "git_ref", requires = "git")]
        branch: Option<String>,

        /// Exact git commit SHA
        #[arg(long, group = "git_ref", requires = "git")]
        rev: Option<String>,
    },

    /// List resolved dependencies with sources and pinned versions
    List,

    /// Update lockfile — advance git deps to latest matching their spec, and
    /// re-resolve registry deps to a fresh SHA
    ///
    /// Refreshing a registry dep picks up a moved tag or a corrected index
    /// entry; the pinned version itself never changes here — bumping it is a
    /// manifest edit.
    ///
    /// With no arguments, updates all git and registry dependencies.
    /// With a name, updates only that dependency.
    Update {
        /// Name of a single dependency to update (default: all)
        name: Option<String>,
    },
}

/// Run the given deps subcommand.
pub fn run(command: DepsCommand) -> Result<()> {
    let project_root = find_project_root()?;
    match command {
        DepsCommand::Add {
            name,
            path,
            git,
            version,
            tag,
            branch,
            rev,
        } => run_add(
            &project_root,
            &name,
            path.as_deref(),
            git.as_deref(),
            version.as_deref(),
            tag,
            branch,
            rev,
        ),
        DepsCommand::List => run_list(&project_root),
        DepsCommand::Update { name } => run_update(&project_root, name.as_deref()),
    }
}

// ---------------------------------------------------------------------------
// deps add
// ---------------------------------------------------------------------------

/// Add a dependency to `beamtalk.toml` and resolve it.
#[allow(clippy::too_many_arguments)] // one field per clap flag on `DepsCommand::Add`
fn run_add(
    project_root: &Utf8Path,
    name: &str,
    path: Option<&str>,
    git_url: Option<&str>,
    version: Option<&str>,
    tag: Option<String>,
    branch: Option<String>,
    rev: Option<String>,
) -> Result<()> {
    // Validate the dependency name
    if let Err(e) = validate_package_name(name) {
        miette::bail!("Invalid dependency name: {}", format_name_error(name, &e));
    }

    let manifest_path = project_root.join("beamtalk.toml");

    // Parse the existing manifest to check for self-dependency and duplicates
    let manifest = manifest::parse_manifest_full(&manifest_path)?;
    if manifest.package.name == name {
        miette::bail!("Cannot add '{name}' as a dependency — it is this package itself");
    }
    if manifest.dependencies.contains_key(name) {
        miette::bail!(
            "Dependency '{name}' already exists in beamtalk.toml. \
             Remove it first or edit beamtalk.toml directly."
        );
    }

    // Build the TOML fragment and validate
    let (toml_fragment, source_display) = if let Some(path_str) = path {
        let fragment = format!("{name} = {{ path = \"{path_str}\" }}");

        // Validate the path exists
        let resolved = project_root.join(path_str);
        if !resolved.exists() {
            miette::bail!(
                "Path dependency '{name}' directory does not exist: {resolved}\n  \
                 (resolved from '{path_str}' relative to '{project_root}')"
            );
        }

        // Validate it contains a beamtalk.toml
        let dep_manifest = resolved.join("beamtalk.toml");
        if !dep_manifest.exists() {
            miette::bail!(
                "Path dependency '{name}' directory does not contain a beamtalk.toml: {resolved}"
            );
        }

        (fragment, format!("path: {path_str}"))
    } else if let Some(url) = git_url {
        let (ref_key, ref_value, ref_display) = if let Some(t) = &tag {
            ("tag", t.as_str(), format!("tag: {t}"))
        } else if let Some(b) = &branch {
            ("branch", b.as_str(), format!("branch: {b}"))
        } else if let Some(r) = &rev {
            ("rev", r.as_str(), format!("rev: {r}"))
        } else {
            miette::bail!(
                "Git dependency '{name}' requires a ref — specify --tag, --branch, or --rev"
            );
        };

        let fragment = format!("{name} = {{ git = \"{url}\", {ref_key} = \"{ref_value}\" }}");

        // Resolve the git dep to validate URL and ref, and get the SHA
        let reference = build_git_reference(tag, branch, rev)?;
        let resolved = git::resolve_git_dep(name, url, &reference, project_root, None)?;
        info!(
            name,
            sha = %resolved.resolved_sha,
            "Resolved git dependency"
        );

        // Update the lockfile
        let mut lockfile = Lockfile::read(project_root)?.unwrap_or_default();
        lockfile.insert(LockEntry {
            name: name.to_string(),
            url: url.to_string(),
            reference,
            resolved_sha: resolved.resolved_sha.clone(),
            registry_version: None,
        });
        lockfile.write(project_root)?;

        let display = format!(
            "git: {url} ({ref_display} @ {})",
            &resolved.resolved_sha[..7.min(resolved.resolved_sha.len())]
        );
        (fragment, display)
    } else {
        // No --path or --git — resolve the dependency through the package
        // registry: pin to --version if given, else the latest published
        // release.
        add_registry_dependency(project_root, name, version, manifest.registry.as_ref())?
    };

    // Append to beamtalk.toml
    append_dependency_to_manifest(&manifest_path, &toml_fragment)?;

    println!("Added dependency '{name}' ({source_display})");
    Ok(())
}

/// Resolve `name` (optionally pinned to `version`, else the latest published
/// release) through the package registry, fetch it via the ordinary git
/// machinery, and record it in the lockfile.
///
/// Returns the `beamtalk.toml` fragment (`name = "X.Y.Z"`) and a
/// human-readable display string for the "Added dependency" message.
fn add_registry_dependency(
    project_root: &Utf8Path,
    name: &str,
    version: Option<&str>,
    registry_config: Option<&manifest::RegistryConfig>,
) -> Result<(String, String)> {
    let registry_location = registry::resolve_registry_location(project_root, registry_config);

    let release = if let Some(v) = version {
        // Validate the exact-version format up front, with the same message a
        // manual beamtalk.toml edit would get, before touching the registry
        // index. Only the error side of `validate_registry_dependency` is
        // wanted here — the `DependencySpec` it also builds is unused.
        manifest::validate_registry_dependency(name, v).map(|_| ())?;
        registry::resolve_release(project_root, &registry_location, name, v)?
    } else {
        registry::resolve_latest_release(project_root, &registry_location, name)?
    };

    // The index is third-party data (same threat model as the git URLs it
    // hands out — see BT-2978's `validate_git_url`), so validate whatever
    // version it reports before writing it into the user's manifest. On the
    // `Some(v)` path above this re-validates the same already-valid `v`
    // (`resolve_release` can only return a release whose version equals it)
    // — redundant but harmless. It's load-bearing on the `--version`-omitted
    // path: there, `release.version` comes straight from untrusted index
    // TOML and this is the only check it gets before being written out.
    manifest::validate_registry_dependency(name, &release.version).map(|_| ())?;

    let fragment = format!("{name} = \"{}\"", release.version);

    // Resolve the release's (git url, tag) through the ordinary git machinery
    // to validate it and pin an exact SHA.
    let reference = GitReference::Tag(release.tag.clone());
    let resolved = git::resolve_git_dep(name, &release.git, &reference, project_root, None)?;

    // Catch a registry index that points this version at the wrong tag (or a
    // package whose tag and beamtalk.toml have drifted apart) right now,
    // rather than leaving it to surface on the next `beamtalk build` — the
    // same check `graph.rs` runs on every resolve.
    super::graph::validate_checkout_version(
        name,
        &release.version,
        &resolved.checkout_path,
        &release.git,
        &reference,
    )?;

    info!(
        name,
        version = %release.version,
        sha = %resolved.resolved_sha,
        "Resolved registry dependency"
    );

    // Update the lockfile
    let mut lockfile = Lockfile::read(project_root)?.unwrap_or_default();
    lockfile.insert(LockEntry {
        name: name.to_string(),
        url: release.git.clone(),
        reference,
        resolved_sha: resolved.resolved_sha.clone(),
        registry_version: Some(release.version.clone()),
    });
    lockfile.write(project_root)?;

    let display = format!(
        "registry: {} (tag: {} @ {})",
        shorten_git_url(&release.git),
        release.tag,
        &resolved.resolved_sha[..7.min(resolved.resolved_sha.len())]
    );
    Ok((fragment, display))
}

/// Build a `GitReference` from optional tag/branch/rev flags.
fn build_git_reference(
    tag: Option<String>,
    branch: Option<String>,
    rev: Option<String>,
) -> Result<GitReference> {
    if let Some(t) = tag {
        Ok(GitReference::Tag(t))
    } else if let Some(b) = branch {
        Ok(GitReference::Branch(b))
    } else if let Some(r) = rev {
        Ok(GitReference::Rev(r))
    } else {
        miette::bail!("Git dependency requires --tag, --branch, or --rev")
    }
}

/// Append a dependency line to the `[dependencies]` section of `beamtalk.toml`.
///
/// If the `[dependencies]` section doesn't exist, it is created.
fn append_dependency_to_manifest(manifest_path: &Utf8Path, toml_fragment: &str) -> Result<()> {
    let content = std::fs::read_to_string(manifest_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{manifest_path}'"))?;

    let new_content = if let Some(deps_idx) = content
        .lines()
        .collect::<Vec<&str>>()
        .iter()
        .position(|l| l.trim() == "[dependencies]")
    {
        // Found the [dependencies] section — append after it
        let mut lines: Vec<&str> = content.lines().collect();

        // Find the end of the dependencies section (next section header or end of file)
        let insert_idx = lines
            .iter()
            .enumerate()
            .skip(deps_idx + 1)
            .find(|(_, l)| l.starts_with('[') && !l.starts_with("[["))
            .map_or(lines.len(), |(i, _)| {
                // Back up past any trailing blank lines before the next section
                let mut idx = i;
                while idx > deps_idx + 1 && lines[idx - 1].trim().is_empty() {
                    idx -= 1;
                }
                idx
            });

        lines.insert(insert_idx, toml_fragment);

        let mut result = lines.join("\n");
        if !result.ends_with('\n') {
            result.push('\n');
        }
        result
    } else {
        // No [dependencies] section — append one
        let mut result = content.clone();
        if !result.ends_with('\n') {
            result.push('\n');
        }
        result.push('\n');
        result.push_str("[dependencies]\n");
        result.push_str(toml_fragment);
        result.push('\n');
        result
    };

    // Validate the new TOML parses correctly before writing
    let _: toml::Value = toml::from_str(&new_content)
        .into_diagnostic()
        .wrap_err("Generated beamtalk.toml is invalid TOML — this is a bug")?;

    std::fs::write(manifest_path, new_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{manifest_path}'"))?;

    Ok(())
}

// ---------------------------------------------------------------------------
// deps list
// ---------------------------------------------------------------------------

/// List all resolved dependencies with sources and pinned versions.
fn run_list(project_root: &Utf8Path) -> Result<()> {
    let manifest_path = project_root.join("beamtalk.toml");
    let manifest = manifest::parse_manifest_full(&manifest_path)?;

    if manifest.dependencies.is_empty() {
        println!("No dependencies declared in beamtalk.toml");
        return Ok(());
    }

    // Read the lockfile to get pinned SHAs for git deps
    let lockfile = Lockfile::read(project_root)?.unwrap_or_default();

    // Collect dep info for aligned output
    let mut entries: Vec<(String, String, String)> = Vec::new();
    let mut max_name = 0;
    let mut max_version = 0;

    for (dep_name, spec) in &manifest.dependencies {
        let version = dep_version(project_root, dep_name, &spec.source);
        let source_info = match &spec.source {
            DependencySource::Path { path } => {
                format!("(path: {})", path.display())
            }
            DependencySource::Git { url, reference } => {
                let short_url = shorten_git_url(url);
                let ref_str = match reference {
                    GitReference::Tag(t) => format!("tag: {t}"),
                    GitReference::Branch(b) => format!("branch: {b}"),
                    GitReference::Rev(r) => format!("rev: {}", &r[..7.min(r.len())]),
                };
                if let Some(entry) = lockfile.get(dep_name) {
                    let short_sha = &entry.resolved_sha[..7.min(entry.resolved_sha.len())];
                    format!("(git: {short_url} {ref_str} @ {short_sha})")
                } else {
                    format!("(git: {short_url} {ref_str})")
                }
            }
            DependencySource::Registry { version } => {
                format_registry_source_info(version, lockfile.get(dep_name))
            }
        };

        max_name = max_name.max(dep_name.len());
        max_version = max_version.max(version.len());
        entries.push((dep_name.clone(), version, source_info));
    }

    for (name, version, source) in &entries {
        println!("  {name:<max_name$}  {version:<max_version$}  {source}");
    }

    Ok(())
}

/// Try to read the version of a dependency from its `beamtalk.toml`.
fn dep_version(project_root: &Utf8Path, name: &str, source: &DependencySource) -> String {
    let dep_root = match source {
        DependencySource::Path { path } => {
            let utf8 = Utf8PathBuf::from(path.to_string_lossy().to_string());
            project_root.join(&utf8)
        }
        DependencySource::Git { .. } | DependencySource::Registry { .. } => {
            crate::commands::build_layout::BuildLayout::new(project_root).dep_checkout_dir(name)
        }
    };

    let manifest_path = dep_root.join("beamtalk.toml");
    if let Ok(m) = manifest::parse_manifest(&manifest_path) {
        m.version
    } else {
        "?".to_string()
    }
}

/// Format a registry dependency's provenance for `deps list` display, e.g.
/// `(registry: github.com/jamesc/beamtalk-yaml tag: v0.2.1 @ abc1234)`.
///
/// Falls back to just the requested version when there is no lockfile entry
/// yet (e.g. a manifest edited by hand before the next resolve).
fn format_registry_source_info(version: &str, lock_entry: Option<&LockEntry>) -> String {
    let Some(entry) = lock_entry else {
        return format!("(registry: {version})");
    };

    let short_url = shorten_git_url(&entry.url);
    let ref_str = match &entry.reference {
        GitReference::Tag(t) => format!("tag: {t}"),
        GitReference::Branch(b) => format!("branch: {b}"),
        GitReference::Rev(r) => format!("rev: {}", &r[..7.min(r.len())]),
    };
    let short_sha = &entry.resolved_sha[..7.min(entry.resolved_sha.len())];
    format!("(registry: {short_url} {ref_str} @ {short_sha})")
}

/// Shorten a git URL for display (strip scheme and trailing `.git`).
fn shorten_git_url(url: &str) -> String {
    let s = url
        .strip_prefix("https://")
        .or_else(|| url.strip_prefix("http://"))
        .or_else(|| url.strip_prefix("git://"))
        .unwrap_or(url);
    s.strip_suffix(".git").unwrap_or(s).to_string()
}

// ---------------------------------------------------------------------------
// deps update
// ---------------------------------------------------------------------------

/// Update dependencies to the latest matching their spec.
///
/// For git deps: re-fetches and resolves to the latest SHA.
/// For registry deps: refreshes the registry index and re-resolves the
/// pinned version to a fresh SHA (picks up a moved tag or a corrected index
/// entry — the pinned version itself never changes; bumping it is a manifest
/// edit).
/// For native (hex) deps: clears locked versions so the next build
/// re-resolves against hex.pm via rebar3 (ADR 0072 Phase 2).
fn run_update(project_root: &Utf8Path, name: Option<&str>) -> Result<()> {
    let manifest_path = project_root.join("beamtalk.toml");
    let manifest = manifest::parse_manifest_full(&manifest_path)?;

    // Determine which deps to update
    let git_deps: BTreeMap<&str, (&str, &GitReference)> = manifest
        .dependencies
        .iter()
        .filter_map(|(dep_name, spec)| {
            if let DependencySource::Git { url, reference } = &spec.source {
                Some((dep_name.as_str(), (url.as_str(), reference)))
            } else {
                None
            }
        })
        .collect();

    let registry_deps: BTreeMap<&str, &str> = manifest
        .dependencies
        .iter()
        .filter_map(|(dep_name, spec)| {
            if let DependencySource::Registry { version } = &spec.source {
                Some((dep_name.as_str(), version.as_str()))
            } else {
                None
            }
        })
        .collect();

    let has_native_deps = !manifest.native_dependencies.is_empty();

    if let Some(target_name) = name {
        // Update a single dep — check existence before emptiness
        if !manifest.dependencies.contains_key(target_name) {
            miette::bail!("Dependency '{target_name}' not found in beamtalk.toml");
        }

        if registry_deps.contains_key(target_name) {
            let version = registry_deps[target_name];
            let registry_location =
                registry::resolve_registry_location(project_root, manifest.registry.as_ref());
            // Refresh once up front so a moved tag or corrected index entry is
            // picked up before resolving.
            registry::ensure_index(&registry_location, project_root, true)?;
            update_single_registry_dep(target_name, version, &registry_location, project_root)?;
        } else if git_deps.contains_key(target_name) {
            let (url, reference) = git_deps[target_name];
            update_single_git_dep(target_name, url, reference, project_root)?;
        } else {
            miette::bail!(
                "Dependency '{target_name}' is a path dependency — \
                 only git and registry dependencies can be updated"
            );
        }
    } else {
        // Update all git and registry deps
        let has_git_deps = !git_deps.is_empty();
        let has_registry_deps = !registry_deps.is_empty();
        if !has_git_deps && !has_registry_deps && !has_native_deps {
            println!("No git, registry, or native dependencies to update");
            return Ok(());
        }

        for (dep_name, (url, reference)) in &git_deps {
            update_single_git_dep(dep_name, url, reference, project_root)?;
        }

        if has_registry_deps {
            let registry_location =
                registry::resolve_registry_location(project_root, manifest.registry.as_ref());
            // Refresh once for the whole batch rather than once per
            // dependency — same effect, without N redundant index
            // clones/pulls when a git-backed registry has several deps.
            registry::ensure_index(&registry_location, project_root, true)?;
            for (dep_name, version) in &registry_deps {
                update_single_registry_dep(dep_name, version, &registry_location, project_root)?;
            }
        }

        // Clear native package locks so the next build re-resolves via rebar3
        if has_native_deps {
            let mut lockfile = Lockfile::read(project_root)?.unwrap_or_default();
            if lockfile.has_native_packages() {
                lockfile.clear_native_packages();
                lockfile.write(project_root)?;
                println!(
                    "Cleared native package locks — next build will re-resolve against hex.pm"
                );
            } else {
                println!("No native packages locked — next build will resolve against hex.pm");
            }
        }
    }

    Ok(())
}

/// Update a single git dependency: re-fetch, resolve SHA, update lockfile.
fn update_single_git_dep(
    name: &str,
    url: &str,
    reference: &GitReference,
    project_root: &Utf8Path,
) -> Result<()> {
    // Force a fresh resolve (ignore lockfile)
    let resolved = git::resolve_git_dep(name, url, reference, project_root, None)?;

    // Read existing lockfile
    let mut lockfile = Lockfile::read(project_root)?.unwrap_or_default();
    let old_sha = lockfile.get(name).map(|e| e.resolved_sha.clone());

    // Update the lockfile entry
    lockfile.insert(LockEntry {
        name: name.to_string(),
        url: url.to_string(),
        reference: reference.clone(),
        resolved_sha: resolved.resolved_sha.clone(),
        registry_version: None,
    });
    lockfile.write(project_root)?;

    let short_sha = &resolved.resolved_sha[..7.min(resolved.resolved_sha.len())];
    if let Some(old) = old_sha {
        let old_short = &old[..7.min(old.len())];
        if old == resolved.resolved_sha {
            println!("  {name}: already up-to-date ({short_sha})");
        } else {
            println!("  {name}: {old_short} -> {short_sha}");
        }
    } else {
        println!("  {name}: resolved to {short_sha}");
    }

    Ok(())
}

/// Re-resolve a single registry dependency's pinned version to a (possibly
/// new) git SHA, and update the lockfile.
///
/// Callers are expected to have already refreshed `location`'s index (once
/// per `deps update` invocation, not once per dependency — see call sites).
/// The pinned version itself never changes here — bumping it is a manifest
/// edit. This only picks up a moved tag or a corrected index entry.
fn update_single_registry_dep(
    name: &str,
    version: &str,
    location: &RegistryLocation,
    project_root: &Utf8Path,
) -> Result<()> {
    let release = registry::resolve_release(project_root, location, name, version)?;

    // Force a fresh resolve (ignore any existing checkout's lockfile pin)
    let reference = GitReference::Tag(release.tag.clone());
    let resolved = git::resolve_git_dep(name, &release.git, &reference, project_root, None)?;

    // Same guard `deps add` and every `beamtalk build` apply: catch a moved
    // tag whose package now declares a different version than the one this
    // manifest still pins.
    super::graph::validate_checkout_version(
        name,
        version,
        &resolved.checkout_path,
        &release.git,
        &reference,
    )?;

    let mut lockfile = Lockfile::read(project_root)?.unwrap_or_default();
    let old_sha = lockfile.get(name).map(|e| e.resolved_sha.clone());

    lockfile.insert(LockEntry {
        name: name.to_string(),
        url: release.git.clone(),
        reference,
        resolved_sha: resolved.resolved_sha.clone(),
        registry_version: Some(version.to_string()),
    });
    lockfile.write(project_root)?;

    let short_sha = &resolved.resolved_sha[..7.min(resolved.resolved_sha.len())];
    if let Some(old) = old_sha {
        let old_short = &old[..7.min(old.len())];
        if old == resolved.resolved_sha {
            println!("  {name}: already up-to-date ({short_sha})");
        } else {
            println!("  {name}: {old_short} -> {short_sha}");
        }
    } else {
        println!("  {name}: resolved to {short_sha}");
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Find the project root by looking for `beamtalk.toml` in the current directory.
fn find_project_root() -> Result<Utf8PathBuf> {
    let cwd = std::env::current_dir()
        .into_diagnostic()
        .wrap_err("Failed to determine current directory")?;

    let project_root = Utf8PathBuf::from_path_buf(cwd).map_err(|p| {
        miette::miette!("Current directory path is not valid UTF-8: {}", p.display())
    })?;

    let manifest_path = project_root.join("beamtalk.toml");
    if !manifest_path.exists() {
        miette::bail!(
            "No beamtalk.toml found in current directory.\n  \
             Run this command from a Beamtalk project root, or create one with `beamtalk new`."
        );
    }

    Ok(project_root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::deps::lockfile::LOCKFILE_NAME;
    use clap::Parser;
    use std::process::Command;
    use tempfile::TempDir;

    /// Create a minimal beamtalk.toml in a temp dir.
    fn create_project(name: &str) -> (TempDir, Utf8PathBuf) {
        let dir = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        let content = format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n");
        std::fs::write(root.join("beamtalk.toml"), content).unwrap();
        (dir, root)
    }

    /// Create a path dependency directory with its own beamtalk.toml.
    fn create_path_dep(parent: &Utf8Path, name: &str) -> Utf8PathBuf {
        let dep_dir = parent.join(name);
        std::fs::create_dir_all(&dep_dir).unwrap();
        let content = format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n");
        std::fs::write(dep_dir.join("beamtalk.toml"), content).unwrap();
        dep_dir
    }

    /// Create a local git repo with beamtalk.toml, a tag, and a branch.
    /// Returns (`TempDir`, url, `commit_sha`).
    fn create_git_dep(pkg_name: &str, version: &str) -> (TempDir, String, String) {
        let dir = TempDir::new().unwrap();
        let path = dir.path();

        run_git_cmd(path, &["init"]);
        run_git_cmd(path, &["config", "user.email", "test@test.com"]);
        run_git_cmd(path, &["config", "user.name", "Test"]);
        run_git_cmd(path, &["config", "commit.gpgsign", "false"]);

        let content = format!("[package]\nname = \"{pkg_name}\"\nversion = \"{version}\"\n");
        std::fs::write(path.join("beamtalk.toml"), content).unwrap();

        run_git_cmd(path, &["add", "."]);
        run_git_cmd(path, &["commit", "-m", "initial"]);
        run_git_cmd(path, &["tag", "-m", "v1.0.0", "v1.0.0"]);
        run_git_cmd(path, &["branch", "develop"]);

        let sha = get_git_sha(path);
        // file:// URLs need forward slashes. On Windows, paths don't start with
        // `/`, so we prepend one to get `file:///C:/...`. On Unix, `display()`
        // already starts with `/`, giving `file:///tmp/...` (BT-1737).
        let mut path_str = path.display().to_string().replace('\\', "/");
        if !path_str.starts_with('/') {
            path_str.insert(0, '/');
        }
        let url = format!("file://{path_str}");
        (dir, url, sha)
    }

    fn run_git_cmd(dir: &std::path::Path, args: &[&str]) {
        let output = Command::new("git")
            .args(args)
            .current_dir(dir)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "git {:?} failed: {}",
            args,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn get_git_sha(dir: &std::path::Path) -> String {
        let output = Command::new("git")
            .args(["rev-parse", "HEAD"])
            .current_dir(dir)
            .output()
            .unwrap();
        String::from_utf8_lossy(&output.stdout).trim().to_string()
    }

    // -----------------------------------------------------------------------
    // append_dependency_to_manifest tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_append_dependency_to_manifest_new_section() {
        let (_dir, root) = create_project("my_app");
        let manifest_path = root.join("beamtalk.toml");

        append_dependency_to_manifest(&manifest_path, "utils = { path = \"../utils\" }").unwrap();

        let content = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(content.contains("[dependencies]"));
        assert!(content.contains("utils = { path = \"../utils\" }"));
        let _: toml::Value = toml::from_str(&content).unwrap();
    }

    #[test]
    fn test_append_dependency_to_existing_section() {
        let (_dir, root) = create_project("my_app");
        let manifest_path = root.join("beamtalk.toml");

        let content = "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
utils = { path = \"../utils\" }
";
        std::fs::write(&manifest_path, content).unwrap();

        append_dependency_to_manifest(
            &manifest_path,
            "json = { git = \"https://example.com/json\", tag = \"v1.0\" }",
        )
        .unwrap();

        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(new_content.contains("utils = { path = \"../utils\" }"));
        assert!(new_content.contains("json = { git = \"https://example.com/json\""));
        let _: toml::Value = toml::from_str(&new_content).unwrap();
    }

    #[test]
    fn test_append_dependency_before_next_section() {
        let (_dir, root) = create_project("my_app");
        let manifest_path = root.join("beamtalk.toml");

        let content = "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
utils = { path = \"../utils\" }

[application]
supervisor = \"AppSup\"
";
        std::fs::write(&manifest_path, content).unwrap();

        append_dependency_to_manifest(&manifest_path, "http = { path = \"../http\" }").unwrap();

        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(new_content.contains("utils = { path = \"../utils\" }"));
        assert!(new_content.contains("http = { path = \"../http\" }"));
        assert!(new_content.contains("[application]"));
        let _: toml::Value = toml::from_str(&new_content).unwrap();
    }

    // -----------------------------------------------------------------------
    // Helper function tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_shorten_git_url() {
        assert_eq!(
            shorten_git_url("https://github.com/jamesc/beamtalk-json.git"),
            "github.com/jamesc/beamtalk-json"
        );
        assert_eq!(
            shorten_git_url("https://github.com/jamesc/beamtalk-json"),
            "github.com/jamesc/beamtalk-json"
        );
        assert_eq!(
            shorten_git_url("git://example.com/repo.git"),
            "example.com/repo"
        );
    }

    #[test]
    fn test_build_git_reference_tag() {
        let r = build_git_reference(Some("v1.0".to_string()), None, None).unwrap();
        assert_eq!(r, GitReference::Tag("v1.0".to_string()));
    }

    #[test]
    fn test_build_git_reference_branch() {
        let r = build_git_reference(None, Some("main".to_string()), None).unwrap();
        assert_eq!(r, GitReference::Branch("main".to_string()));
    }

    #[test]
    fn test_build_git_reference_rev() {
        let r = build_git_reference(None, None, Some("abc123".to_string())).unwrap();
        assert_eq!(r, GitReference::Rev("abc123".to_string()));
    }

    #[test]
    fn test_build_git_reference_none_errors() {
        assert!(build_git_reference(None, None, None).is_err());
    }

    // -----------------------------------------------------------------------
    // deps add tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_add_path_dep_validates_existence() {
        let (_dir, root) = create_project("my_app");
        let result = run_add(
            &root,
            "missing_dep",
            Some("../nonexistent"),
            None,
            None,
            None,
            None,
            None,
        );
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("does not exist"), "Error: {err}");
    }

    #[test]
    fn test_add_path_dep_validates_manifest() {
        let (_dir, root) = create_project("my_app");

        // Create a dep directory without beamtalk.toml
        std::fs::create_dir_all(root.join("nodeps")).unwrap();

        let result = run_add(
            &root,
            "nodeps",
            Some("nodeps"),
            None,
            None,
            None,
            None,
            None,
        );
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("beamtalk.toml"),
            "Should mention missing manifest: {err}"
        );
    }

    #[test]
    fn test_add_path_dep_success() {
        let (_dir, root) = create_project("my_app");
        create_path_dep(&root, "utils");

        let result = run_add(&root, "utils", Some("utils"), None, None, None, None, None);
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());

        let content = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
        assert!(content.contains("[dependencies]"));
        assert!(content.contains("utils = { path = \"utils\" }"));
    }

    #[test]
    fn test_add_rejects_duplicate() {
        let (_dir, root) = create_project("my_app");
        create_path_dep(&root, "utils");

        let content = "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
utils = { path = \"utils\" }
";
        std::fs::write(root.join("beamtalk.toml"), content).unwrap();

        let result = run_add(&root, "utils", Some("utils"), None, None, None, None, None);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("already exists"), "Error: {err}");
    }

    #[test]
    fn test_add_rejects_invalid_name() {
        let (_dir, root) = create_project("my_app");
        let result = run_add(
            &root,
            "BadName",
            Some("whatever"),
            None,
            None,
            None,
            None,
            None,
        );
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("Invalid dependency name"), "Error: {err}");
    }

    #[test]
    fn test_add_rejects_self_dependency() {
        let (_dir, root) = create_project("my_app");
        let result = run_add(
            &root,
            "my_app",
            Some("somewhere"),
            None,
            None,
            None,
            None,
            None,
        );
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("this package itself"), "Error: {err}");
    }

    #[test]
    fn test_add_git_dep_with_tag() {
        let (_dir, root) = create_project("my_app");
        let (_git_dir, url, _sha) = create_git_dep("json_lib", "1.0.0");

        let result = run_add(
            &root,
            "json_lib",
            None,
            Some(&url),
            None,
            Some("v1.0.0".to_string()),
            None,
            None,
        );
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());

        // Verify manifest was updated
        let content = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
        assert!(content.contains("json_lib"));
        assert!(content.contains(&url));
        assert!(content.contains("v1.0.0"));

        // Verify lockfile was created
        let lock_content = std::fs::read_to_string(root.join(LOCKFILE_NAME)).unwrap();
        assert!(lock_content.contains("json_lib"));
        assert!(lock_content.contains("tag:v1.0.0"));
    }

    // -----------------------------------------------------------------------
    // deps add — registry dependencies (BT-2979)
    // -----------------------------------------------------------------------
    //
    // These use a local-directory registry index and `file://` git repos —
    // no network access, mirroring `deps/graph.rs`'s registry test pattern.

    /// Create a local registry index directory holding one package entry.
    fn create_registry_index(pkg_name: &str, entry_toml: &str) -> (TempDir, Utf8PathBuf) {
        let dir = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        std::fs::create_dir_all(root.join("packages")).unwrap();
        std::fs::write(
            root.join("packages").join(format!("{pkg_name}.toml")),
            entry_toml,
        )
        .unwrap();
        (dir, root)
    }

    /// Point a project's `beamtalk.toml` at a local registry index directory.
    fn set_registry_url(root: &Utf8Path, index_root: &Utf8Path) {
        let manifest_path = root.join("beamtalk.toml");
        let content = std::fs::read_to_string(&manifest_path).unwrap();
        let updated = format!("{content}\n[registry]\nurl = '{index_root}'\n");
        std::fs::write(&manifest_path, updated).unwrap();
    }

    /// Build a single-release registry index entry pointing at a real
    /// `file://` git repo tagged `v1.0.0` (matching `create_git_dep`'s fixed
    /// tag), and point `root`'s manifest at that index.
    fn setup_registry_dep(
        root: &Utf8Path,
        pkg_name: &str,
    ) -> (TempDir, Utf8PathBuf, TempDir, String) {
        let (git_dir, url, _sha) = create_git_dep(pkg_name, "1.0.0");
        let entry = format!(
            "name = \"{pkg_name}\"\n\n[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n"
        );
        let (index_dir, index_root) = create_registry_index(pkg_name, &entry);
        set_registry_url(root, &index_root);
        (index_dir, index_root, git_dir, url)
    }

    #[test]
    fn test_add_registry_dep_with_version() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, _index_root, _git_dir, url) = setup_registry_dep(&root, "yaml");

        let result = run_add(&root, "yaml", None, None, Some("1.0.0"), None, None, None);
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());

        let content = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
        assert!(
            content.contains("yaml = \"1.0.0\""),
            "manifest should have the bare-string fragment: {content}"
        );

        let lock_content = std::fs::read_to_string(root.join(LOCKFILE_NAME)).unwrap();
        assert!(lock_content.contains("yaml"));
        assert!(lock_content.contains(&url));
        assert!(
            lock_content.contains("version = \"1.0.0\""),
            "lock entry should record the registry version: {lock_content}"
        );
    }

    #[test]
    fn test_add_registry_dep_without_version_uses_latest() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, _index_root, _git_dir, _url) = setup_registry_dep(&root, "yaml");

        let result = run_add(&root, "yaml", None, None, None, None, None, None);
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());

        let content = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
        assert!(
            content.contains("yaml = \"1.0.0\""),
            "should pin the latest (only) published version: {content}"
        );
    }

    /// Minimal parser wrapping `DepsCommand`, for exercising clap's own
    /// argument-group constraints (`requires`/`conflicts_with`) directly,
    /// independent of `run_add`'s runtime logic.
    #[derive(Debug, clap::Parser)]
    struct DepsCli {
        #[command(subcommand)]
        cmd: DepsCommand,
    }

    #[test]
    fn test_add_tag_without_git_is_rejected_by_clap() {
        // `--tag`/`--branch`/`--rev` only make sense alongside `--git`; without
        // it, `run_add` falls into the registry-resolution arm and would
        // otherwise silently discard the ref instead of erroring.
        let err = DepsCli::try_parse_from(["deps", "add", "yaml", "--tag", "v1.0.0"])
            .expect_err("clap should reject --tag without --git");
        let msg = err.to_string();
        assert!(
            msg.contains("--git") || msg.contains("required arguments"),
            "expected a missing-requirement error: {msg}"
        );
    }

    #[test]
    fn test_add_registry_dep_latest_rejects_malformed_index_version() {
        // The index is third-party data (BT-2978's threat model for the git
        // URLs it hands out applies equally to the version string): a
        // malformed `version` in the index must not reach the manifest
        // unvalidated just because `--version` was omitted. The bad version
        // must be caught before any git clone is attempted, so a bogus `git`
        // field here is fine — it's never touched.
        let (_dir, root) = create_project("my_app");
        let entry =
            "name = \"yaml\"\n\n[[versions]]\nversion = \"1.0\"\ngit = \"file:///nonexistent\"\n";
        let (_index_dir, index_root) = create_registry_index("yaml", entry);
        set_registry_url(&root, &index_root);

        let result = run_add(&root, "yaml", None, None, None, None, None, None);
        assert!(result.is_err(), "malformed index version must be rejected");
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("exact version"), "Error: {err}");

        let content = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
        assert!(
            !content.contains("yaml"),
            "manifest must not gain a fragment for a rejected version: {content}"
        );
    }

    #[test]
    fn test_add_registry_dep_unknown_package() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, index_root) = create_registry_index("yaml", "name = \"yaml\"\n");
        set_registry_url(&root, &index_root);

        let result = run_add(&root, "does_not_exist", None, None, None, None, None, None);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("not found in the registry"), "Error: {err}");
    }

    #[test]
    fn test_add_registry_dep_unknown_version() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, _index_root, _git_dir, _url) = setup_registry_dep(&root, "yaml");

        let result = run_add(&root, "yaml", None, None, Some("9.9.9"), None, None, None);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("Available versions"), "Error: {err}");
    }

    #[test]
    fn test_add_registry_dep_rejects_malformed_version() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, _index_root, _git_dir, _url) = setup_registry_dep(&root, "yaml");

        let result = run_add(&root, "yaml", None, None, Some("1.0"), None, None, None);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("exact version"),
            "should reject a non major.minor.patch version: {err}"
        );
    }

    #[test]
    fn test_add_registry_dep_version_mismatch_is_an_error() {
        let (_dir, root) = create_project("my_app");
        // The index claims "1.0.0", but the tag it points at (defaulting to
        // "v1.0.0", matching `create_git_dep`'s fixed tag) actually declares
        // "2.0.0" — a packaging error the index alone can't catch.
        let (_git_dir, url, _sha) = create_git_dep("yaml", "2.0.0");
        let entry =
            format!("name = \"yaml\"\n\n[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n");
        let (_index_dir, index_root) = create_registry_index("yaml", &entry);
        set_registry_url(&root, &index_root);

        let result = run_add(&root, "yaml", None, None, Some("1.0.0"), None, None, None);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("version mismatch"), "Error: {err}");

        // A failed add must not leave a dangling manifest entry.
        let content = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
        assert!(!content.contains("yaml"), "manifest: {content}");
    }

    // -----------------------------------------------------------------------
    // deps list tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_list_no_deps() {
        let (_dir, root) = create_project("my_app");
        let result = run_list(&root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_list_with_path_dep() {
        let (_dir, root) = create_project("my_app");
        create_path_dep(&root, "utils");

        let content = "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
utils = { path = \"utils\" }
";
        std::fs::write(root.join("beamtalk.toml"), content).unwrap();

        let result = run_list(&root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_list_with_registry_dep() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, _index_root, _git_dir, _url) = setup_registry_dep(&root, "yaml");

        run_add(&root, "yaml", None, None, Some("1.0.0"), None, None, None).unwrap();

        let result = run_list(&root);
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());
    }

    // -----------------------------------------------------------------------
    // format_registry_source_info tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_format_registry_source_info_with_lock_entry() {
        let entry = LockEntry {
            name: "yaml".to_string(),
            url: "https://github.com/jamesc/beamtalk-yaml".to_string(),
            reference: GitReference::Tag("v0.2.1".to_string()),
            resolved_sha: "abc1234def5678".to_string(),
            registry_version: Some("0.2.1".to_string()),
        };
        assert_eq!(
            format_registry_source_info("0.2.1", Some(&entry)),
            "(registry: github.com/jamesc/beamtalk-yaml tag: v0.2.1 @ abc1234)"
        );
    }

    #[test]
    fn test_format_registry_source_info_without_lock_entry() {
        assert_eq!(
            format_registry_source_info("0.2.1", None),
            "(registry: 0.2.1)"
        );
    }

    // -----------------------------------------------------------------------
    // deps update tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_update_no_git_deps() {
        let (_dir, root) = create_project("my_app");
        create_path_dep(&root, "utils");

        let content = "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
utils = { path = \"utils\" }
";
        std::fs::write(root.join("beamtalk.toml"), content).unwrap();

        let result = run_update(&root, None);
        assert!(result.is_ok());
    }

    #[test]
    fn test_update_rejects_unknown_dep() {
        let (_dir, root) = create_project("my_app");
        let result = run_update(&root, Some("nonexistent"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("not found"), "Error: {err}");
    }

    #[test]
    fn test_update_rejects_path_dep() {
        let (_dir, root) = create_project("my_app");
        create_path_dep(&root, "utils");

        let content = "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
utils = { path = \"utils\" }
";
        std::fs::write(root.join("beamtalk.toml"), content).unwrap();

        let result = run_update(&root, Some("utils"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("path dependency"), "Error: {err}");
    }

    #[test]
    fn test_update_git_dep() {
        let (_dir, root) = create_project("my_app");
        let (_git_dir, url, _sha) = create_git_dep("json_lib", "1.0.0");

        let content = format!(
            "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
json_lib = {{ git = \"{url}\", branch = \"develop\" }}
"
        );
        std::fs::write(root.join("beamtalk.toml"), content).unwrap();

        let result = run_update(&root, Some("json_lib"));
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());

        // Verify lockfile was created/updated
        let lock_content = std::fs::read_to_string(root.join(LOCKFILE_NAME)).unwrap();
        assert!(lock_content.contains("json_lib"));
    }

    #[test]
    fn test_update_all_git_deps() {
        let (_dir, root) = create_project("my_app");
        let (_git_dir1, url1, _sha1) = create_git_dep("dep_a", "1.0.0");
        let (_git_dir2, url2, _sha2) = create_git_dep("dep_b", "2.0.0");

        let content = format!(
            "\
[package]
name = \"my_app\"
version = \"0.1.0\"

[dependencies]
dep_a = {{ git = \"{url1}\", branch = \"develop\" }}
dep_b = {{ git = \"{url2}\", tag = \"v1.0.0\" }}
"
        );
        std::fs::write(root.join("beamtalk.toml"), content).unwrap();

        let result = run_update(&root, None);
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());

        let lock_content = std::fs::read_to_string(root.join(LOCKFILE_NAME)).unwrap();
        assert!(lock_content.contains("dep_a"));
        assert!(lock_content.contains("dep_b"));
    }

    // -----------------------------------------------------------------------
    // deps update — registry dependencies (BT-2979)
    // -----------------------------------------------------------------------

    #[test]
    fn test_update_registry_dep_reresolves_sha() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, _index_root, _git_dir, url) = setup_registry_dep(&root, "yaml");

        run_add(&root, "yaml", None, None, Some("1.0.0"), None, None, None).unwrap();

        let result = run_update(&root, Some("yaml"));
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());

        // The pinned version is untouched — only the resolved SHA may move.
        let lock_content = std::fs::read_to_string(root.join(LOCKFILE_NAME)).unwrap();
        assert!(lock_content.contains("yaml"));
        assert!(lock_content.contains(&url));
        assert!(lock_content.contains("version = \"1.0.0\""));
    }

    #[test]
    fn test_update_all_includes_registry_deps() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, _index_root, _git_dir, _url) = setup_registry_dep(&root, "yaml");
        let (_git_dir2, url2, _sha2) = create_git_dep("dep_b", "2.0.0");

        run_add(&root, "yaml", None, None, Some("1.0.0"), None, None, None).unwrap();
        run_add(
            &root,
            "dep_b",
            None,
            Some(&url2),
            None,
            Some("v1.0.0".to_string()),
            None,
            None,
        )
        .unwrap();

        let result = run_update(&root, None);
        assert!(result.is_ok(), "Error: {:?}", result.unwrap_err());

        let lock_content = std::fs::read_to_string(root.join(LOCKFILE_NAME)).unwrap();
        assert!(lock_content.contains("yaml"));
        assert!(lock_content.contains("dep_b"));
    }

    #[test]
    fn test_update_registry_dep_unknown_version_is_an_error() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, _index_root, _git_dir, _url) = setup_registry_dep(&root, "yaml");

        run_add(&root, "yaml", None, None, Some("1.0.0"), None, None, None).unwrap();

        // Simulate the version having been removed from the index after it
        // was pinned — a real scenario `deps update` should surface clearly.
        let content = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
        let content = content.replace("yaml = \"1.0.0\"", "yaml = \"9.9.9\"");
        std::fs::write(root.join("beamtalk.toml"), content).unwrap();

        let result = run_update(&root, Some("yaml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("Available versions"), "Error: {err}");
    }

    #[test]
    fn test_update_registry_dep_version_mismatch_is_an_error() {
        let (_dir, root) = create_project("my_app");
        let (_index_dir, index_root, _git_dir, _url) = setup_registry_dep(&root, "yaml");

        run_add(&root, "yaml", None, None, Some("1.0.0"), None, None, None).unwrap();

        // Simulate the index later being repointed at a repo that still
        // tags "v1.0.0" (the default `deps update` re-resolves to) but whose
        // beamtalk.toml declares a different version — a packaging error
        // `deps update`'s re-resolve should catch, not silently relock.
        let (_bad_git_dir, bad_url, _sha) = create_git_dep("yaml", "2.0.0");
        let bad_entry =
            format!("name = \"yaml\"\n\n[[versions]]\nversion = \"1.0.0\"\ngit = \"{bad_url}\"\n");
        std::fs::write(index_root.join("packages").join("yaml.toml"), bad_entry).unwrap();

        let result = run_update(&root, Some("yaml"));
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("version mismatch"), "Error: {err}");
    }
}
