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

use crate::commands::toml_utils::{
    escape_toml_string, is_any_header, line_ending, table_header_name,
};
use crate::commands::util::find_project_root;
use beamtalk_package::manifest::{self, format_name_error, validate_package_name};

use super::git;
use super::graph;
use super::lockfile::{LockEntry, Lockfile};
use super::registry;

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

    // Build the TOML fragment and validate. Every user-supplied string is
    // escaped: a Windows path (`..\lib\foo`) or a ref with a quote must not
    // produce invalid TOML or inject keys.
    let (toml_fragment, source_display, lock_entry) = if let Some(path_str) = path {
        let fragment = format!("{name} = {{ path = \"{}\" }}", escape_toml_string(path_str));

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

        (fragment, format!("path: {path_str}"), None)
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

        let fragment = format!(
            "{name} = {{ git = \"{}\", {ref_key} = \"{}\" }}",
            escape_toml_string(url),
            escape_toml_string(ref_value)
        );

        // Resolve the git dep to validate URL and ref, and get the SHA
        let reference = build_git_reference(tag, branch, rev)?;
        let resolved = git::resolve_git_dep(name, url, &reference, project_root, None)?;
        info!(
            name,
            sha = %resolved.resolved_sha,
            "Resolved git dependency"
        );

        let lock_entry = LockEntry {
            name: name.to_string(),
            url: url.to_string(),
            reference,
            resolved_sha: resolved.resolved_sha.clone(),
            registry_version: None,
        };

        let display = format!(
            "git: {url} ({ref_display} @ {})",
            &resolved.resolved_sha[..7.min(resolved.resolved_sha.len())]
        );
        (fragment, display, Some(lock_entry))
    } else {
        // No --path or --git — resolve the dependency through the package
        // registry: pin to --version if given, else the latest published
        // release.
        let (fragment, display, entry) =
            add_registry_dependency(project_root, name, version, manifest.registry.as_ref())?;
        (fragment, display, Some(entry))
    };

    // Edit the manifest first, then record the lock entry. The other order
    // left an orphan lock entry (and a checkout) whenever the manifest edit
    // failed; if the lock write fails now, the manifest edit is undone.
    let original_manifest = std::fs::read_to_string(&manifest_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{manifest_path}'"))?;
    append_dependency_to_manifest(&manifest_path, &toml_fragment)?;
    if let Some(entry) = lock_entry {
        let write_lock = || -> Result<()> {
            let mut lockfile = Lockfile::read(project_root)?.unwrap_or_default();
            lockfile.insert(entry);
            lockfile.write(project_root)
        };
        if let Err(err) = write_lock() {
            let _ = crate::commands::util::write_atomic(
                &manifest_path,
                &original_manifest,
                ".beamtalk-manifest.",
            );
            return Err(
                err.wrap_err("Failed to update beamtalk.lock; beamtalk.toml was left unchanged")
            );
        }
    }

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
) -> Result<(String, String, LockEntry)> {
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
    // hands out — see `validate_git_url`), so validate whatever
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

    let lock_entry = LockEntry {
        name: name.to_string(),
        url: release.git.clone(),
        reference,
        resolved_sha: resolved.resolved_sha.clone(),
        registry_version: Some(super::lockfile::RegistryVersion {
            version: release.version.clone(),
            registry: Some(registry::registry_identity(registry_config)),
        }),
    };

    let display = format!(
        "registry: {} (tag: {} @ {})",
        shorten_git_url(&release.git),
        release.tag,
        &resolved.resolved_sha[..7.min(resolved.resolved_sha.len())]
    );
    Ok((fragment, display, lock_entry))
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

    let eol = line_ending(&content);
    // `split_inclusive` keeps each line's own terminator, so untouched lines
    // are written back byte-for-byte (CRLF stays CRLF).
    let mut lines: Vec<String> = content.split_inclusive('\n').map(str::to_string).collect();
    let with_eol = |text: &str| format!("{text}{eol}");

    let new_content = if let Some(deps_idx) = lines
        .iter()
        .position(|l| table_header_name(l) == Some("dependencies"))
    {
        // Insert before the next section header, backing up past blank lines.
        let mut insert_idx = lines
            .iter()
            .enumerate()
            .skip(deps_idx + 1)
            .find(|(_, l)| is_any_header(l) && !l.trim_start().starts_with("[["))
            .map_or(lines.len(), |(i, _)| i);
        while insert_idx > deps_idx + 1 && lines[insert_idx - 1].trim().is_empty() {
            insert_idx -= 1;
        }
        // The line before the insertion point may be an unterminated last line.
        if insert_idx > 0 && !lines[insert_idx - 1].ends_with('\n') {
            lines[insert_idx - 1].push_str(eol);
        }
        lines.insert(insert_idx, with_eol(toml_fragment));
        lines.concat()
    } else {
        // No [dependencies] section — append one
        let mut result = content.clone();
        if !result.ends_with('\n') {
            result.push_str(eol);
        }
        result.push_str(eol);
        result.push_str(&with_eol("[dependencies]"));
        result.push_str(&with_eol(toml_fragment));
        result
    };

    crate::commands::toml_utils::write_validated_manifest(manifest_path, &new_content)
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
            beamtalk_core::build_layout::BuildLayout::new(project_root).dep_checkout_dir(name)
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
/// For git and registry deps: re-resolves the dependency **and everything it
/// pulls in** (BT-4920) — `deps update <name>` re-resolves `<name>` and its
/// whole subtree, a bare `deps update` re-resolves every lock entry. A
/// registry dep refreshes the index and re-resolves its pinned version to a
/// fresh SHA (picks up a moved tag or a corrected index entry); the pinned
/// version itself never changes here — bumping it is a manifest edit, and
/// `deps update` then brings in the transitive versions the new one declares.
/// Lock entries the new versions no longer require are removed; a transitive
/// another direct dependency also needs stays pinned on `deps update <name>`.
/// For native deps (hex.pm and git, ADR 0072 Phase 2): clears the locked
/// versions so the next build re-resolves via rebar3 — all of them, or just
/// the named one (direct or transitive). Native locks are cleared whenever
/// `beamtalk.lock` has any, including ones only a dependency declares.
pub(super) fn run_update(project_root: &Utf8Path, name: Option<&str>) -> Result<()> {
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

    let old_lock = Lockfile::read(project_root)?;
    let has_native_locks = old_lock.as_ref().is_some_and(Lockfile::has_native_packages);

    if let Some(target_name) = name {
        // Update a single dep — check existence before emptiness
        if !manifest.dependencies.contains_key(target_name) {
            if unlock_native_package(project_root, target_name)? {
                return Ok(());
            }
            miette::bail!("Dependency '{target_name}' not found in beamtalk.toml");
        }
        if !registry_deps.contains_key(target_name) && !git_deps.contains_key(target_name) {
            miette::bail!(
                "Dependency '{target_name}' is a path dependency — \
                 only git and registry dependencies can be updated"
            );
        }
        relock(
            project_root,
            &manifest,
            old_lock.as_ref(),
            &graph::Unlock::Subtree(target_name),
            false,
        )?;
    } else {
        // Update all git and registry deps
        if git_deps.is_empty() && registry_deps.is_empty() && !has_native_locks {
            println!("No git, registry, or native dependencies to update");
            return Ok(());
        }
        // Native locks are cleared in the same write, so the next build
        // re-resolves them via rebar3.
        relock(
            project_root,
            &manifest,
            old_lock.as_ref(),
            &graph::Unlock::All,
            has_native_locks,
        )?;
        if has_native_locks {
            println!("Cleared native package locks — next build will re-resolve via rebar3");
        }
    }

    Ok(())
}

/// Re-resolve the part of the dependency graph `unlock` selects (BT-4920),
/// write `beamtalk.lock`, and report what moved.
///
/// The lock is written once, after the whole graph resolved: a conflict or
/// fetch failure anywhere leaves it exactly as it was.
fn relock(
    project_root: &Utf8Path,
    manifest: &manifest::ParsedManifest,
    old_lock: Option<&Lockfile>,
    unlock: &graph::Unlock<'_>,
    clear_native_locks: bool,
) -> Result<()> {
    let has_registry_locks =
        old_lock.is_some_and(|l| l.packages.values().any(|e| e.registry_version.is_some()));
    let has_registry_deps = manifest
        .dependencies
        .values()
        .any(|spec| matches!(spec.source, DependencySource::Registry { .. }));
    if has_registry_deps || has_registry_locks {
        let registry_location =
            registry::resolve_registry_location(project_root, manifest.registry.as_ref());
        // Refresh once for the whole batch rather than once per dependency —
        // a moved tag or corrected index entry must be picked up before
        // resolving, without N redundant index clones/pulls.
        registry::ensure_index(&registry_location, project_root, true)?;
    }

    let mut relocked = graph::relock_dependency_graph(project_root, unlock)?;
    if clear_native_locks {
        relocked.lockfile.clear_native_packages();
    }
    relocked.lockfile.write(project_root)?;

    let old_packages = old_lock.map(|l| &l.packages);
    for (dep_name, entry) in &relocked.lockfile.packages {
        if !relocked.reachable.contains(dep_name) {
            continue;
        }
        let short_sha = &entry.resolved_sha[..7.min(entry.resolved_sha.len())];
        match old_packages.and_then(|p| p.get(dep_name)) {
            Some(old) if old.resolved_sha == entry.resolved_sha && old == entry => {
                println!("  {dep_name}: already up-to-date ({short_sha})");
            }
            Some(old) => {
                let old_short = &old.resolved_sha[..7.min(old.resolved_sha.len())];
                println!("  {dep_name}: {old_short} -> {short_sha}");
            }
            None => println!("  {dep_name}: resolved to {short_sha}"),
        }
    }
    if let Some(old) = old_packages {
        for dep_name in old.keys() {
            if !relocked.lockfile.packages.contains_key(dep_name) {
                println!("  {dep_name}: removed (no longer required)");
            }
        }
    }
    Ok(())
}

/// Unlock one native package (direct or transitive) so the next build
/// re-resolves it: drop its `beamtalk.lock` entry and its rebar3 checkout —
/// rebar3 keeps an existing checkout that no `rebar.lock` entry moves.
/// Returns `false` if no native package of that name is locked.
fn unlock_native_package(project_root: &Utf8Path, name: &str) -> Result<bool> {
    let is_locked = |lockfile: &Option<Lockfile>| {
        lockfile
            .as_ref()
            .is_some_and(|l| l.get_native(name).is_some())
    };
    // Cheap check first, so an unknown name never creates `_build/`.
    if !is_locked(&Lockfile::read(project_root)?) {
        return Ok(false);
    }

    let layout = beamtalk_core::build_layout::BuildLayout::new(project_root);
    // Same lock a build's native step holds while it rewrites `beamtalk.lock`
    // and touches the checkouts; read the lockfile again under it.
    let _native_build_lock =
        crate::commands::dir_lock::lock_dir(&layout.native_dir(), "native build")?;
    let Some(mut lockfile) = Lockfile::read(project_root)? else {
        return Ok(false);
    };
    if lockfile.native_packages.remove(name).is_none() {
        return Ok(false);
    }

    let checkout = layout.rebar_lib_dir().join(name);
    if checkout.exists() {
        std::fs::remove_dir_all(&checkout)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove native checkout '{checkout}'"))?;
    }
    lockfile.write(project_root)?;
    println!("Unlocked native package '{name}' — next build will re-resolve it via rebar3");
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::deps::lockfile::LOCKFILE_NAME;
    use crate::commands::util::to_forward_slash;
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
        // already starts with `/`, giving `file:///tmp/...`.
        let mut path_str = to_forward_slash(&path.display().to_string());
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

    /// BT-5076: a backslash in the path (a Windows `..\\lib\\foo`) used to produce
    /// an invalid TOML escape and fail with "this is a bug".
    #[cfg(unix)] // a literal backslash is a legal directory-name character on Unix
    #[test]
    fn test_add_path_dep_with_backslash_is_escaped() {
        let (_dir, root) = create_project("my_app");
        create_path_dep(&root, "lib\\foo");

        run_add(&root, "foo", Some("lib\\foo"), None, None, None, None, None).unwrap();

        let content = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();
        assert!(
            content.contains(r#"foo = { path = "lib\\foo" }"#),
            "{content}"
        );
        let parsed: toml::Value = toml::from_str(&content).unwrap();
        assert_eq!(
            parsed["dependencies"]["foo"]["path"].as_str(),
            Some("lib\\foo")
        );
    }

    #[test]
    fn test_append_dependency_preserves_crlf() {
        let (_dir, root) = create_project("my_app");
        let manifest_path = root.join("beamtalk.toml");
        let content = "[package]\r\nname = \"my_app\"\r\nversion = \"0.1.0\"\r\n\r\n[dependencies]\r\nutils = { path = \"../utils\" }\r\n\r\n[stubs]\r\npath = \"stubs/\"\r\n";
        std::fs::write(&manifest_path, content).unwrap();

        append_dependency_to_manifest(&manifest_path, "json = { path = \"../json\" }").unwrap();

        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(
            !new_content.replace("\r\n", "").contains('\n'),
            "bare LF: {new_content:?}"
        );
        assert!(new_content.contains("json = { path = \"../json\" }\r\n\r\n[stubs]"));
        let _: toml::Value = toml::from_str(&new_content).unwrap();

        // No [dependencies] section, no trailing newline.
        std::fs::write(
            &manifest_path,
            "[package]\r\nname = \"my_app\"\r\nversion = \"0.1.0\"",
        )
        .unwrap();
        append_dependency_to_manifest(&manifest_path, "json = { path = \"../json\" }").unwrap();
        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(
            !new_content.replace("\r\n", "").contains('\n'),
            "bare LF: {new_content:?}"
        );
    }

    #[test]
    fn test_append_dependency_to_header_with_trailing_comment() {
        let (_dir, root) = create_project("my_app");
        let manifest_path = root.join("beamtalk.toml");
        let content = "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n[dependencies] # runtime deps\nutils = { path = \"../utils\" }\n";
        std::fs::write(&manifest_path, content).unwrap();

        append_dependency_to_manifest(&manifest_path, "json = { path = \"../json\" }").unwrap();

        let new_content = std::fs::read_to_string(&manifest_path).unwrap();
        assert_eq!(
            new_content.matches("[dependencies]").count(),
            1,
            "{new_content}"
        );
        let parsed: toml::Value = toml::from_str(&new_content).unwrap();
        assert!(parsed["dependencies"]["json"].is_table());
        assert!(parsed["dependencies"]["utils"].is_table());
    }

    /// BT-5076: the lock entry is written after the manifest edit, so a failed
    /// manifest edit leaves no orphan lock entry.
    #[test]
    fn test_add_git_dep_failed_manifest_edit_leaves_no_lock_entry() {
        let (_dir, root) = create_project("my_app");
        let (_git_dir, url, _sha) = create_git_dep("jsonlib", "1.0.0");
        // `[dependencies]` as a non-table makes the edited manifest invalid.
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n[dependencies]\n[dependencies.jsonlib]\nbroken = true\n",
        )
        .unwrap();
        let before = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap();

        let result = run_add(
            &root,
            "jsonlib",
            None,
            Some(&url),
            None,
            Some("v1.0.0".into()),
            None,
            None,
        );

        assert!(result.is_err());
        assert!(!root.join("beamtalk.lock").exists(), "orphan lock entry");
        assert_eq!(
            std::fs::read_to_string(root.join("beamtalk.toml")).unwrap(),
            before
        );
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
    // deps add — registry dependencies
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
        // The index is third-party data (the same threat model for the git
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
            registry_version: Some(super::super::lockfile::RegistryVersion {
                version: "0.2.1".to_string(),
                registry: Some("https://github.com/jamesc/beamtalk-registry".to_string()),
            }),
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
    fn test_update_unlocks_single_native_package() {
        use super::super::lockfile::NativePackageLock;

        let (_dir, root) = create_project("my_app");
        let mut lockfile = Lockfile::new();
        lockfile.insert_native(NativePackageLock::hex("gun", "2.1.0", "A").at_level(0));
        lockfile.insert_native(NativePackageLock::hex("cowlib", "2.13.0", "B").at_level(1));
        lockfile.write(&root).unwrap();
        let checkout = beamtalk_core::build_layout::BuildLayout::new(&root)
            .rebar_lib_dir()
            .join("cowlib");
        std::fs::create_dir_all(&checkout).unwrap();

        // `cowlib` is transitive — not in beamtalk.toml — but can be unlocked.
        run_update(&root, Some("cowlib")).unwrap();

        let lockfile = Lockfile::read(&root).unwrap().unwrap();
        assert!(lockfile.get_native("cowlib").is_none());
        assert!(lockfile.get_native("gun").is_some());
        assert!(!checkout.exists(), "the stale checkout must be removed");
    }

    /// Native locks are cleared even when only a dependency (not the root
    /// manifest) declares native dependencies.
    #[test]
    fn test_update_clears_native_locks_without_root_native_deps() {
        use super::super::lockfile::NativePackageLock;

        let (_dir, root) = create_project("my_app");
        let mut lockfile = Lockfile::new();
        lockfile.insert_native(NativePackageLock::hex("gun", "2.1.0", "A").at_level(0));
        lockfile.write(&root).unwrap();

        run_update(&root, None).unwrap();

        assert!(
            Lockfile::read(&root)
                .unwrap()
                .is_none_or(|l| !l.has_native_packages())
        );
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
    // deps update — registry dependencies
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
