// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk publish` — tag and publish a release to the package registry (BT-2980).
//!
//! **DDD Context:** Build System
//!
//! ```bash
//! beamtalk version bump minor
//! git commit -am "release 0.3.0"
//! beamtalk publish
//! ```
//!
//! Publishing a version `X.Y.Z` of package `name`:
//! 1. Preflights: clean working tree, an `origin` remote, tag `vX.Y.Z`
//!    absent both locally and on `origin`, and the version not already
//!    recorded in the registry index (the index is refreshed first).
//! 2. Creates an annotated tag `vX.Y.Z` and pushes it to `origin`.
//! 3. Updates the registry index — creating `packages/{name}.toml` for a
//!    first release, or appending a `[[versions]]` entry for a subsequent
//!    one — and commits/pushes the index (when it is git-backed).
//!
//! The registry the release publishes to is resolved through the same
//! priority chain dependency resolution uses (`BEAMTALK_REGISTRY` →
//! `[registry] url` in the library's own manifest → the default registry —
//! see `deps::registry::resolve_registry_location`), so publishing and
//! consuming a package always agree on which index is authoritative.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, Result, WrapErr};
use std::process::Command;

use crate::commands::deps::registry::{self, RegistryEntry, RegistryLocation};
use crate::commands::manifest;
use crate::commands::toml_utils::escape_toml_string;

/// Run `beamtalk publish`.
///
/// # Errors
///
/// Returns an error if any preflight fails (dirty working tree, missing
/// `origin` remote, the tag or version already exists), or if creating the
/// tag, pushing it, or updating the registry index fails.
pub fn run(dry_run: bool) -> Result<()> {
    let project_root = find_project_root()?;
    let manifest_path = project_root.join("beamtalk.toml");
    let manifest = manifest::parse_manifest_full(&manifest_path)?;

    let name = &manifest.package.name;
    let version = &manifest.package.version;
    let tag = format!("v{version}");

    // Preflight 1: clean working tree.
    ensure_clean_working_tree(&project_root)?;

    // Preflight 2: an `origin` remote exists — its URL becomes the index
    // entry's `git` field, so it must be resolved before the tag-absence
    // check below (which queries `origin` over `git ls-remote`).
    let origin_url = get_origin_remote_url(&project_root)?;

    // Preflight 3: resolve the registry index and check whether `version` is
    // already recorded there. Refresh the index first so this always sees
    // the latest state, not a stale local clone from an earlier `deps`
    // resolution.
    //
    // This must run *before* the tag-absence check below: a tag that already
    // exists on `origin` is ambiguous by itself — it means either "this
    // version is already published" (it's in the index — bump the version)
    // or "a previous `publish` pushed the tag but died before updating the
    // index" (it's *not* in the index — that's a partial failure, recover it
    // manually, see docs/beamtalk-packages.md, "If step 3 fails"). Knowing
    // the index state up front is what lets the tag-absence check tell those
    // two cases apart instead of always blaming the version.
    let location = registry::resolve_registry_location(&project_root, manifest.registry.as_ref());
    let index_root = registry::ensure_index(&location, &project_root, true)?;
    let existing_entry = registry::read_entry(&index_root, name)?;
    let version_recorded = existing_entry
        .as_ref()
        .is_some_and(|entry| entry.find_version(version).is_some());
    // A version recorded in the on-disk entry only really counts as
    // *published* once it's committed to the index repository — otherwise
    // this is reading back a previous `publish`'s partial failure (BT-3000
    // sibling): `write_index_entry` succeeded but `git add`/`git commit`
    // (`commit_and_push_index`) never landed, so the file sits in the index
    // clone staged or untracked. Without this check, that leftover file
    // would make a retry falsely report "already published" and tell the
    // user to bump the version — permanently orphaning the tagged release.
    let version_published = version_recorded && is_entry_committed(&index_root, name, &location)?;

    // Preflight 4: the release tag doesn't already exist, locally or on
    // origin — unless the tag is on origin and `version_published` is
    // false, which is the partial-failure case described above.
    ensure_tag_absent(
        &project_root,
        &tag,
        name,
        version,
        version_published,
        &location,
        &index_root,
    )?;

    // Preflight 5: guards the (unusual) case where the tag doesn't exist at
    // all — locally or on `origin` — yet the version is already recorded in
    // the index, e.g. because the tag was deleted by hand after a successful
    // publish.
    if version_published {
        miette::bail!(
            "Version '{version}' of package '{name}' is already published in the \
             registry ({location}).\n\n  \
             Bump the version first: beamtalk version bump patch"
        );
    }

    // Preflight 6: for a git-backed registry, resolve the identity that will
    // author the index commit *before* anything is mutated. The index lives
    // in a separate clone from the project (typically
    // `_build/registry/index/`), so it does not inherit an identity the user
    // configured only locally in their project repo — checking this only at
    // commit time would mean failing after the release tag is already live
    // on `origin` and `beamtalk publish` refuses to re-run (the tag-absence
    // preflight above would then block a retry).
    let git_identity = match &location {
        RegistryLocation::Git(_) => Some(get_git_identity(&project_root)?),
        RegistryLocation::LocalDir(_) => None,
    };

    let entry_path = index_root.join("packages").join(format!("{name}.toml"));
    let new_entry_content = render_index_entry_content(
        name,
        manifest.package.description.as_deref(),
        version,
        &origin_url,
        &tag,
        existing_entry.as_ref(),
        &entry_path,
    )?;

    if dry_run {
        println!("Would create tag: {tag}");
        println!("Would push tag '{tag}' to origin ({origin_url})");
        println!();
        if existing_entry.is_some() {
            println!("Would append to registry index entry '{entry_path}':");
        } else {
            println!("Would create registry index entry '{entry_path}':");
        }
        println!("---");
        print!("{new_entry_content}");
        println!("---");
        return Ok(());
    }

    create_and_push_tag(&project_root, &tag, version)?;

    write_index_entry(&entry_path, &new_entry_content)?;
    match &location {
        RegistryLocation::Git(_) => {
            let (author_name, author_email) =
                git_identity.expect("resolved for RegistryLocation::Git in preflight above");
            commit_and_push_index(&index_root, name, version, &author_name, &author_email)?;
        }
        RegistryLocation::LocalDir(dir) => {
            println!(
                "Updated local registry index file at '{entry_path}' (no git commit — \
                 '{dir}' is a plain directory, not a git checkout)"
            );
        }
    }

    println!("Published {name} v{version}");
    Ok(())
}

// ---------------------------------------------------------------------------
// Preflight checks
// ---------------------------------------------------------------------------

/// Ensure the git working tree has no uncommitted changes (staged or unstaged).
fn ensure_clean_working_tree(project_root: &Utf8Path) -> Result<()> {
    let output = Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(project_root)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run 'git status'")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!(
            "Failed to check git status in '{project_root}':\n{stderr}\n\n  \
             Is this a git repository?"
        );
    }

    if !output.stdout.is_empty() {
        miette::bail!(
            "The git working tree has uncommitted changes — commit or stash them before \
             publishing:\n\n{}",
            String::from_utf8_lossy(&output.stdout)
        );
    }

    Ok(())
}

/// Read the `origin` remote's URL — it becomes the registry index entry's
/// `git` field.
fn get_origin_remote_url(project_root: &Utf8Path) -> Result<String> {
    let output = Command::new("git")
        .args(["remote", "get-url", "origin"])
        .current_dir(project_root)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run 'git remote get-url origin'")?;

    if !output.status.success() {
        miette::bail!(
            "No 'origin' remote is configured for this repository.\n\n  \
             'beamtalk publish' needs a git remote named 'origin' — its URL becomes the \
             registry index entry. Add one with:\n    git remote add origin <url>"
        );
    }

    let url = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if url.is_empty() {
        miette::bail!("The 'origin' remote has an empty URL");
    }
    Ok(url)
}

/// Resolve the git identity (`user.name`, `user.email`) that authored the
/// release, from the project repo's own config (local, falling back to
/// global — the normal git resolution order).
///
/// The registry index is a *separate* clone (typically
/// `_build/registry/index/`), so it does not inherit an identity the user
/// configured only locally in their project repo. That identity is resolved
/// here and passed through explicitly when committing to the index, rather
/// than depending on whatever the index clone's own local/global config
/// happens to be.
fn get_git_identity(project_root: &Utf8Path) -> Result<(String, String)> {
    let read = |key: &str| -> Result<String> {
        let output = Command::new("git")
            .args(["config", key])
            .current_dir(project_root)
            .output()
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to run 'git config {key}'"))?;
        if !output.status.success() {
            miette::bail!(
                "git has no '{key}' configured for this repository.\n\n  \
                 'beamtalk publish' commits the registry index update using your git \
                 identity. Set it with:\n    git config user.name \"Your Name\"\n    \
                 git config user.email you@example.com"
            );
        }
        let value = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if value.is_empty() {
            miette::bail!(
                "git's '{key}' is configured but empty.\n\n  \
                 'beamtalk publish' commits the registry index update using your git \
                 identity. Set it with:\n    git config user.name \"Your Name\"\n    \
                 git config user.email you@example.com"
            );
        }
        Ok(value)
    };

    Ok((read("user.name")?, read("user.email")?))
}

/// Whether the registry index's on-disk entry for `name` is actually
/// committed at `HEAD` in the index repository — as opposed to merely
/// present on disk (staged or untracked) from a previous `publish` that
/// wrote the file but never got as far as `commit_and_push_index`.
///
/// Only meaningful for a git-backed index: `RegistryLocation::LocalDir`
/// registries aren't necessarily git checkouts at all (a CI fixture, a
/// hand-maintained directory), so "committed" has no meaning there and
/// filesystem presence — what [`registry::read_entry`] already reports — is
/// authoritative, the same as it is for normal dependency resolution
/// (`resolve_release`/`resolve_latest_release`) against such a registry.
///
/// Deliberately checks membership in the `HEAD` tree (`git cat-file -e
/// HEAD:<path>`) rather than `git ls-files` or `git status`: the git index
/// (staging area) is not the commit history, so a file that's been `git
/// add`-ed but not yet committed would still satisfy `git ls-files` — this
/// must return `false` for that case too, not just for a fully untracked
/// file.
///
/// # Errors
///
/// Returns an error if the underlying `git` command cannot be run at all
/// (e.g. `git` itself is missing). A `HEAD` that doesn't exist yet, or a path
/// absent from it, is reported as `Ok(false)` — both are "not committed",
/// not failures.
fn is_entry_committed(
    index_root: &Utf8Path,
    name: &str,
    location: &RegistryLocation,
) -> Result<bool> {
    match location {
        RegistryLocation::LocalDir(_) => Ok(true),
        RegistryLocation::Git(_) => {
            let rel_path = format!("packages/{name}.toml");
            let output = Command::new("git")
                .args(["cat-file", "-e", &format!("HEAD:{rel_path}")])
                .current_dir(index_root)
                .output()
                .into_diagnostic()
                .wrap_err_with(|| {
                    format!(
                        "Failed to check whether '{rel_path}' is committed in the registry \
                         index at '{index_root}'"
                    )
                })?;
            Ok(output.status.success())
        }
    }
}

/// Ensure the release tag doesn't already exist locally or on `origin` —
/// unless it exists on `origin` but `version` is genuinely not yet published
/// (`version_published` is `false`), which means a *previous* `publish` run
/// pushed the tag (step 2) and then died before updating the registry index
/// (step 3), rather than this being a real republish attempt.
///
/// The remote check's result takes priority over the local one when both the
/// tag and its remote counterpart exist: a partial failure of the kind above
/// leaves the tag both on `origin` *and* locally (it was created before
/// being pushed), so treating the local tag as the controlling signal would
/// always report the true-republish message even when the index disagrees.
/// `version_published` is resolved by the caller from the same index read
/// used for the "already published" preflight, so both checks agree on the
/// index's state.
///
/// The remote check needs network access to `origin`; the local one doesn't.
/// If it fails (offline, VPN down) and a local tag already gives an
/// actionable answer, this falls back to the local-only verdict rather than
/// turning what used to be an instant, network-free local conflict into a
/// network error.
///
/// # Errors
///
/// Returns an error if the tag already exists (locally, or on `origin` when
/// `version_published` is `true`), if `origin` has the tag but the version
/// isn't published yet (the partial-failure case — the error names the
/// manual recovery steps instead of suggesting a version bump), or if the
/// underlying `git` commands fail and there's no local tag to fall back to
/// reporting instead.
fn ensure_tag_absent(
    project_root: &Utf8Path,
    tag: &str,
    name: &str,
    version: &str,
    version_published: bool,
    location: &RegistryLocation,
    index_root: &Utf8Path,
) -> Result<()> {
    let local = Command::new("git")
        .args(["tag", "--list", tag])
        .current_dir(project_root)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to list local git tags")?;
    if !local.status.success() {
        let stderr = String::from_utf8_lossy(&local.stderr);
        miette::bail!("Failed to check local tags:\n{stderr}");
    }
    let local_exists = !String::from_utf8_lossy(&local.stdout).trim().is_empty();

    // Checking `origin` needs network access; a tag already known to exist
    // locally does not. When the remote check itself fails (offline, VPN
    // down) and there's a local tag to report instead, fall back to the
    // local-only verdict rather than turning what used to be an instant,
    // network-free local conflict into a network error.
    let remote_exists = match Command::new("git")
        .args(["ls-remote", "--tags", "origin", tag])
        .current_dir(project_root)
        .output()
    {
        Ok(output) if output.status.success() => {
            !String::from_utf8_lossy(&output.stdout).trim().is_empty()
        }
        Ok(output) if !local_exists => {
            let stderr = String::from_utf8_lossy(&output.stderr);
            miette::bail!("Failed to check remote tags on 'origin':\n{stderr}");
        }
        Err(e) if !local_exists => {
            return Err(e)
                .into_diagnostic()
                .wrap_err("Failed to list remote git tags on 'origin'");
        }
        Ok(_) | Err(_) => false,
    };

    if remote_exists {
        if version_published {
            miette::bail!(
                "Tag '{tag}' already exists on 'origin'.\n\n  \
                 This version has already been published — bump the version first."
            );
        }

        let recovery = match location {
            RegistryLocation::Git(_) => format!(
                "1. Check '{index_root}' for an uncommitted or unpushed change.\n  \
                 2. If it's uncommitted: (cd {index_root} && git add . && git commit -m \
                 \"registry: {name} v{version}\")\n  \
                 3. Push it: (cd {index_root} && git push)\n  \
                 If the push fails (another author pushed to the registry concurrently): \
                 (cd {index_root} && git pull --rebase && git push)"
            ),
            RegistryLocation::LocalDir(_) => format!(
                "Check '{index_root}/packages/{name}.toml' for a '{version}' entry and add it \
                 by hand if it's missing."
            ),
        };

        miette::bail!(
            "Tag '{tag}' already exists on 'origin', but version '{version}' of package \
             '{name}' is not yet in the registry index ({location}).\n\n  \
             A previous 'beamtalk publish' pushed the release tag but failed before the \
             registry index was updated — this is a partial failure, not a republish. Do \
             NOT bump the version and do NOT re-run 'beamtalk publish': the tag already \
             exists, so a retry only reports this same error, and bumping the version would \
             permanently abandon the release you already tagged.\n\n  \
             Finish the index update manually instead:\n  {recovery}\n\n  \
             See docs/beamtalk-packages.md, \"If step 3 fails\", for details."
        );
    }

    if local_exists {
        miette::bail!(
            "Tag '{tag}' already exists locally.\n\n  \
             This version may already have been published — bump the version first, or \
             delete the stale tag with `git tag -d {tag}` if the previous publish failed \
             partway through."
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Tag creation
// ---------------------------------------------------------------------------

/// Create an annotated tag `tag` and push it to `origin`.
fn create_and_push_tag(project_root: &Utf8Path, tag: &str, version: &str) -> Result<()> {
    let create = Command::new("git")
        .args(["tag", "-a", tag, "-m", &format!("Release {version}")])
        .current_dir(project_root)
        .output()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create tag '{tag}'"))?;
    if !create.status.success() {
        let stderr = String::from_utf8_lossy(&create.stderr);
        miette::bail!("Failed to create annotated tag '{tag}':\n{stderr}");
    }

    let push = Command::new("git")
        .args(["push", "origin", tag])
        .current_dir(project_root)
        .output()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to push tag '{tag}'"))?;
    if !push.status.success() {
        let stderr = String::from_utf8_lossy(&push.stderr);
        miette::bail!(
            "Failed to push tag '{tag}' to origin:\n{stderr}\n\n  \
             The tag was created locally — push it manually with `git push origin {tag}` \
             once the issue is resolved, or delete it with `git tag -d {tag}` to retry \
             `beamtalk publish` from scratch."
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Registry index update
// ---------------------------------------------------------------------------

/// Render the full new content of a package's `packages/{name}.toml` index
/// entry — either a brand-new file, or the existing file with a
/// `[[versions]]` block appended.
#[allow(clippy::too_many_arguments)]
fn render_index_entry_content(
    name: &str,
    description: Option<&str>,
    version: &str,
    git_url: &str,
    tag: &str,
    existing_entry: Option<&RegistryEntry>,
    entry_path: &Utf8Path,
) -> Result<String> {
    let content = if existing_entry.is_some() {
        let existing_content = std::fs::read_to_string(entry_path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read existing index entry '{entry_path}'"))?;
        append_version_block(&existing_content, version, git_url, tag)
    } else {
        render_new_index_entry(name, description, version, git_url, tag)
    };

    // Validate it round-trips through the same parser dependency resolution
    // uses, so a malformed write is caught here rather than by the next
    // person who tries to depend on this package.
    registry::parse_index_entry(name, &content)
        .wrap_err("Generated registry index entry is invalid — this is a bug")?;

    Ok(content)
}

fn render_new_index_entry(
    name: &str,
    description: Option<&str>,
    version: &str,
    git_url: &str,
    tag: &str,
) -> String {
    use std::fmt::Write as _;

    let mut out = format!("name = \"{}\"\n", escape_toml_string(name));
    if let Some(desc) = description {
        let _ = writeln!(out, "description = \"{}\"", escape_toml_string(desc));
    }
    out.push('\n');
    out.push_str(&render_version_block(version, git_url, tag));
    out
}

fn append_version_block(existing_content: &str, version: &str, git_url: &str, tag: &str) -> String {
    let mut out = existing_content.trim_end().to_string();
    out.push_str("\n\n");
    out.push_str(&render_version_block(version, git_url, tag));
    out
}

fn render_version_block(version: &str, git_url: &str, tag: &str) -> String {
    format!(
        "[[versions]]\nversion = \"{}\"\ngit = \"{}\"\ntag = \"{}\"\n",
        escape_toml_string(version),
        escape_toml_string(git_url),
        escape_toml_string(tag)
    )
}

fn write_index_entry(entry_path: &Utf8Path, content: &str) -> Result<()> {
    if let Some(parent) = entry_path.parent() {
        std::fs::create_dir_all(parent)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create directory '{parent}'"))?;
    }

    std::fs::write(entry_path, content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write registry index entry '{entry_path}'"))
}

/// Commit the working index tree and push it upstream.
fn commit_and_push_index(
    index_root: &Utf8Path,
    name: &str,
    version: &str,
    author_name: &str,
    author_email: &str,
) -> Result<()> {
    let add = Command::new("git")
        .args(["add", "."])
        .current_dir(index_root)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to stage registry index changes")?;
    if !add.status.success() {
        let stderr = String::from_utf8_lossy(&add.stderr);
        miette::bail!("Failed to stage registry index changes:\n{stderr}");
    }

    let commit_message = format!("Publish {name} v{version}");
    let commit = Command::new("git")
        .args([
            "-c",
            &format!("user.name={author_name}"),
            "-c",
            &format!("user.email={author_email}"),
            "commit",
            "-m",
            &commit_message,
        ])
        .current_dir(index_root)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to commit registry index changes")?;
    if !commit.status.success() {
        let stderr = String::from_utf8_lossy(&commit.stderr);
        miette::bail!(
            "Failed to commit registry index changes:\n{stderr}\n\n  \
             The release tag was already pushed, but no commit was created — 'git commit' \
             itself failed, so there is nothing to push yet. Resolve the issue and stage and \
             commit the change at '{index_root}' manually (`cd {index_root} && git add . && \
             git commit -m \"registry: {name} v{version}\"`), then push it, or contact the \
             registry maintainer."
        );
    }

    let push = Command::new("git")
        .args(["push"])
        .current_dir(index_root)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to push registry index changes")?;
    if !push.status.success() {
        let stderr = String::from_utf8_lossy(&push.stderr);
        miette::bail!(
            "Failed to push registry index changes:\n{stderr}\n\n  \
             The release tag was already pushed and the index commit exists locally at \
             '{index_root}' — push it manually with `git push` once the issue is resolved."
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Find the project root by requiring a `beamtalk.toml` in the current
/// directory.
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
    use serial_test::serial;
    use tempfile::TempDir;

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

    /// Flatten a diagnostic's message to single-spaced text for substring
    /// assertions.
    ///
    /// Uses `Display` (the raw message), not `Debug` (miette's graphical
    /// renderer): the graphical renderer word-wraps at a fixed column width
    /// and injects a `│` continuation marker at each wrap point, which
    /// silently splits any substring — e.g. a temp-dir path — that happens
    /// to straddle the wrap column. Whether a given path straddles it
    /// depends on the OS's temp-dir prefix length (`/tmp/...` on Linux vs.
    /// the much longer `/var/folders/.../T/...` on macOS or
    /// `C:\Users\RUNNER~1\AppData\Local\Temp\...` on Windows), so a
    /// `Debug`-based flatten can pass on Linux CI and fail on macOS/Windows
    /// CI for the exact same diagnostic.
    fn flat_err(err: &miette::Report) -> String {
        format!("{err}")
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn utf8(dir: &TempDir) -> Utf8PathBuf {
        Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap()
    }

    /// Restores the process cwd to the directory captured at construction
    /// when dropped — including on an unwinding panic, so one failing test
    /// can't strand the process cwd inside a tempdir that a later
    /// `#[serial(cwd)]` test then deletes out from under it.
    struct CwdGuard(std::path::PathBuf);

    impl Drop for CwdGuard {
        fn drop(&mut self) {
            let _ = std::env::set_current_dir(&self.0);
        }
    }

    /// Run `f` with the process cwd set to `dir`, always restoring the
    /// original cwd afterward, even if `f` panics. Callers must serialize
    /// with `#[serial(cwd)]`.
    fn with_cwd<T>(dir: &Utf8Path, f: impl FnOnce() -> T) -> T {
        let _guard = CwdGuard(std::env::current_dir().unwrap());
        std::env::set_current_dir(dir).unwrap();
        f()
    }

    /// Set up a "publishable" library: a bare repo standing in for `origin`,
    /// a working clone of it with `beamtalk.toml` committed, and a second
    /// bare repo standing in for the registry index, referenced from the
    /// library's manifest via `[registry] url` (so tests never touch process
    /// environment variables).
    struct TestFixture {
        // Kept alive for the duration of the test — dropping deletes the tempdir.
        _origin_bare: TempDir,
        // Read back by `index_dir()` (BT-2996) — not just kept alive, hence no
        // leading underscore.
        index_bare: TempDir,
        library: TempDir,
    }

    impl TestFixture {
        /// Where `run()` cached this fixture's registry index clone.
        ///
        /// BT-2996 moved the default cache out of the project (previously a
        /// fixed `_build/registry/index/`) into a shared, user-level
        /// directory keyed by the registry URL, so tests resolve it the same
        /// way production code does — via `ensure_index` — rather than
        /// hardcoding a path. `refresh: false` is safe here: every caller
        /// only inspects this after a `run()` that already cloned it.
        fn index_dir(&self) -> Utf8PathBuf {
            let location = RegistryLocation::Git(file_url(self.index_bare.path()));
            registry::ensure_index(&location, &utf8(&self.library), false).unwrap()
        }
    }

    fn setup(pkg_name: &str, version: &str, description: Option<&str>) -> TestFixture {
        // Bare repo standing in for the library's `origin`.
        let origin_bare = TempDir::new().unwrap();
        run_git(origin_bare.path(), &["init", "--bare"]);

        // Bare repo standing in for the registry index's `origin`. Seed it
        // with an empty `packages/` directory via a throwaway clone.
        let index_bare = TempDir::new().unwrap();
        run_git(index_bare.path(), &["init", "--bare"]);
        let index_seed = TempDir::new().unwrap();
        run_git(index_seed.path(), &["init"]);
        configure_git_identity(index_seed.path());
        std::fs::create_dir_all(index_seed.path().join("packages")).unwrap();
        std::fs::write(index_seed.path().join("packages/.gitkeep"), "").unwrap();
        run_git(index_seed.path(), &["add", "."]);
        run_git(index_seed.path(), &["commit", "-m", "seed index"]);
        run_git(
            index_seed.path(),
            &["remote", "add", "origin", &file_url(index_bare.path())],
        );
        run_git(index_seed.path(), &["push", "origin", "HEAD:main"]);
        run_git(
            index_bare.path(),
            &["symbolic-ref", "HEAD", "refs/heads/main"],
        );

        // The library's working clone.
        let library = TempDir::new().unwrap();
        run_git(library.path(), &["init"]);
        configure_git_identity(library.path());

        let description_line = description
            .map(|d| format!("description = \"{d}\"\n"))
            .unwrap_or_default();
        let manifest = format!(
            "[package]\nname = \"{pkg_name}\"\nversion = \"{version}\"\n{description_line}\n\
             [registry]\nurl = \"{}\"\n",
            file_url(index_bare.path())
        );
        std::fs::write(library.path().join("beamtalk.toml"), manifest).unwrap();
        // Real projects scaffolded by `beamtalk new` ignore `_build/` — keep
        // that convention here too so the working tree never looks dirty on
        // a re-publish, regardless of where the registry index is cached
        // (BT-2996: normally a shared, user-level cache outside the project
        // entirely, but `BEAMTALK_REGISTRY_CACHE_DIR` can still point it at
        // `_build/registry`).
        std::fs::write(library.path().join(".gitignore"), "/_build/\n").unwrap();
        run_git(library.path(), &["add", "."]);
        run_git(library.path(), &["commit", "-m", "initial"]);
        run_git(
            library.path(),
            &["remote", "add", "origin", &file_url(origin_bare.path())],
        );
        run_git(library.path(), &["push", "origin", "HEAD:main"]);

        TestFixture {
            _origin_bare: origin_bare,
            index_bare,
            library,
        }
    }

    fn configure_git_identity(dir: &std::path::Path) {
        run_git(dir, &["config", "user.email", "test@test.com"]);
        run_git(dir, &["config", "user.name", "Test"]);
        run_git(dir, &["config", "commit.gpgsign", "false"]);
        run_git(dir, &["config", "tag.gpgsign", "false"]);
    }

    fn file_url(path: &std::path::Path) -> String {
        let mut s = path.display().to_string().replace('\\', "/");
        if !s.starts_with('/') {
            s.insert(0, '/');
        }
        format!("file://{s}")
    }

    // -----------------------------------------------------------------------
    // Preflight checks
    // -----------------------------------------------------------------------

    #[test]
    fn test_ensure_clean_working_tree_rejects_dirty_tree() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());
        std::fs::write(dir.path().join("dirty.txt"), "uncommitted").unwrap();

        let err = ensure_clean_working_tree(&utf8(&dir)).unwrap_err();
        assert!(flat_err(&err).contains("uncommitted changes"), "{err:?}");
    }

    #[test]
    fn test_ensure_clean_working_tree_accepts_clean_tree() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());
        std::fs::write(dir.path().join("a.txt"), "content").unwrap();
        run_git(dir.path(), &["add", "."]);
        run_git(dir.path(), &["commit", "-m", "initial"]);

        assert!(ensure_clean_working_tree(&utf8(&dir)).is_ok());
    }

    #[test]
    fn test_get_origin_remote_url_errors_when_missing() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());

        let err = get_origin_remote_url(&utf8(&dir)).unwrap_err();
        assert!(flat_err(&err).contains("No 'origin' remote"), "{err:?}");
    }

    #[test]
    fn test_get_origin_remote_url_returns_url() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());
        run_git(
            dir.path(),
            &["remote", "add", "origin", "https://example.test/repo"],
        );

        let url = get_origin_remote_url(&utf8(&dir)).unwrap();
        assert_eq!(url, "https://example.test/repo");
    }

    #[test]
    fn test_ensure_tag_absent_rejects_local_tag() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());
        std::fs::write(dir.path().join("a.txt"), "x").unwrap();
        run_git(dir.path(), &["add", "."]);
        run_git(dir.path(), &["commit", "-m", "initial"]);
        run_git(dir.path(), &["tag", "-a", "v1.0.0", "-m", "v1.0.0"]);

        // A real `origin` that does *not* have the tag — a self-referencing
        // "remote" would see the just-created tag too (it's the same repo),
        // which would exercise the remote branch below instead of this one.
        let origin_bare = TempDir::new().unwrap();
        run_git(origin_bare.path(), &["init", "--bare"]);
        run_git(
            dir.path(),
            &["remote", "add", "origin", &file_url(origin_bare.path())],
        );

        let err = ensure_tag_absent(
            &utf8(&dir),
            "v1.0.0",
            "pkg",
            "1.0.0",
            false,
            &RegistryLocation::Git("https://example.test/registry".to_string()),
            Utf8Path::new("unused-index-root"),
        )
        .unwrap_err();
        assert!(flat_err(&err).contains("already exists locally"), "{err:?}");
    }

    /// A local-only tag conflict must stay reportable without network
    /// access: if `origin` can't be reached (offline, VPN down), fall back
    /// to the local verdict instead of surfacing the remote check's own
    /// network failure — the local answer is already actionable on its own.
    #[test]
    fn test_ensure_tag_absent_local_tag_survives_unreachable_origin() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());
        std::fs::write(dir.path().join("a.txt"), "x").unwrap();
        run_git(dir.path(), &["add", "."]);
        run_git(dir.path(), &["commit", "-m", "initial"]);
        run_git(dir.path(), &["tag", "-a", "v1.0.0", "-m", "v1.0.0"]);

        // A directory that isn't a git repository at all — `git ls-remote`
        // against it fails the way it would offline against a real host.
        let unreachable = TempDir::new().unwrap();
        run_git(
            dir.path(),
            &[
                "remote",
                "add",
                "origin",
                unreachable.path().to_str().unwrap(),
            ],
        );

        let err = ensure_tag_absent(
            &utf8(&dir),
            "v1.0.0",
            "pkg",
            "1.0.0",
            false,
            &RegistryLocation::Git("https://example.test/registry".to_string()),
            Utf8Path::new("unused-index-root"),
        )
        .unwrap_err();
        assert!(flat_err(&err).contains("already exists locally"), "{err:?}");
    }

    #[test]
    fn test_ensure_tag_absent_rejects_remote_tag_when_version_published() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());
        std::fs::write(dir.path().join("a.txt"), "x").unwrap();
        run_git(dir.path(), &["add", "."]);
        run_git(dir.path(), &["commit", "-m", "initial"]);

        let origin_bare = TempDir::new().unwrap();
        run_git(origin_bare.path(), &["init", "--bare"]);
        run_git(
            dir.path(),
            &["remote", "add", "origin", &file_url(origin_bare.path())],
        );
        run_git(dir.path(), &["tag", "-a", "v1.0.0", "-m", "v1.0.0"]);
        run_git(dir.path(), &["push", "origin", "v1.0.0"]);

        // A true republish attempt: the tag is on origin *and* the version
        // is already recorded in the index — the original message is
        // correct here.
        let err = ensure_tag_absent(
            &utf8(&dir),
            "v1.0.0",
            "pkg",
            "1.0.0",
            true,
            &RegistryLocation::Git("https://example.test/registry".to_string()),
            Utf8Path::new("unused-index-root"),
        )
        .unwrap_err();
        let msg = flat_err(&err);
        assert!(msg.contains("already exists on 'origin'"), "{msg}");
        assert!(msg.contains("bump the version first"), "{msg}");
    }

    #[test]
    fn test_ensure_tag_absent_reports_partial_failure_when_version_not_published() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());
        std::fs::write(dir.path().join("a.txt"), "x").unwrap();
        run_git(dir.path(), &["add", "."]);
        run_git(dir.path(), &["commit", "-m", "initial"]);

        let origin_bare = TempDir::new().unwrap();
        run_git(origin_bare.path(), &["init", "--bare"]);
        run_git(
            dir.path(),
            &["remote", "add", "origin", &file_url(origin_bare.path())],
        );
        run_git(dir.path(), &["tag", "-a", "v1.0.0", "-m", "v1.0.0"]);
        run_git(dir.path(), &["push", "origin", "v1.0.0"]);

        // The tag is on origin (step 2 succeeded) but the version is *not*
        // in the index (step 3 never happened) — a partial failure, not a
        // republish. The old unconditional "bump the version" message would
        // be actively harmful here: it abandons the release just tagged.
        let index_root = utf8(&dir).join("_build/registry/index");
        let err = ensure_tag_absent(
            &utf8(&dir),
            "v1.0.0",
            "pkg",
            "1.0.0",
            false,
            &RegistryLocation::Git("https://example.test/registry".to_string()),
            &index_root,
        )
        .unwrap_err();
        let msg = flat_err(&err);
        assert!(
            !msg.contains("bump the version first"),
            "must not suggest bumping the version on a partial failure: {msg}"
        );
        assert!(msg.contains("Do NOT bump the version"), "{msg}");
        assert!(msg.contains("partial failure"), "{msg}");
        assert!(msg.contains(index_root.as_str()), "{msg}");
        assert!(msg.contains("git push"), "{msg}");
    }

    // -----------------------------------------------------------------------
    // Index rendering
    // -----------------------------------------------------------------------

    #[test]
    fn test_render_new_index_entry_includes_description() {
        let content = render_new_index_entry(
            "yaml",
            Some("YAML parsing"),
            "0.1.0",
            "https://example.test/yaml",
            "v0.1.0",
        );
        assert!(content.contains("name = \"yaml\""));
        assert!(content.contains("description = \"YAML parsing\""));
        assert!(content.contains("[[versions]]"));
        assert!(content.contains("version = \"0.1.0\""));
        assert!(content.contains("git = \"https://example.test/yaml\""));
        assert!(content.contains("tag = \"v0.1.0\""));

        let parsed = registry::parse_index_entry("yaml", &content).unwrap();
        assert_eq!(parsed.versions.len(), 1);
    }

    #[test]
    fn test_render_new_index_entry_omits_absent_description() {
        let content =
            render_new_index_entry("yaml", None, "0.1.0", "https://example.test/yaml", "v0.1.0");
        assert!(!content.contains("description"));
    }

    #[test]
    fn test_append_version_block_preserves_existing_content() {
        let existing = "name = \"yaml\"\ndescription = \"YAML parsing\"\n\n[[versions]]\nversion = \"0.1.0\"\ngit = \"https://example.test/yaml\"\ntag = \"v0.1.0\"\n";
        let updated =
            append_version_block(existing, "0.2.0", "https://example.test/yaml", "v0.2.0");

        assert!(updated.contains("description = \"YAML parsing\""));
        assert!(updated.contains("version = \"0.1.0\""));
        assert!(updated.contains("version = \"0.2.0\""));

        let parsed = registry::parse_index_entry("yaml", &updated).unwrap();
        assert_eq!(parsed.versions.len(), 2);
        assert!(parsed.find_version("0.1.0").is_some());
        assert!(parsed.find_version("0.2.0").is_some());
    }

    #[test]
    fn test_render_new_index_entry_with_multiline_description_produces_valid_toml() {
        // A `description` containing a raw newline (reachable via a TOML
        // multi-line basic string in beamtalk.toml) must not corrupt the
        // generated single-line index entry. Also covers a `\uXXXX`-escaped
        // control character (VT, U+000B) — the named escapes (`\n`, `\t`)
        // above don't exercise that fallback arm of `escape_toml_string`.
        let content = render_new_index_entry(
            "yaml",
            Some("Line one\nLine two\twith a tab\u{b}VT"),
            "0.1.0",
            "https://example.test/yaml",
            "v0.1.0",
        );

        let parsed = registry::parse_index_entry("yaml", &content).unwrap_or_else(|e| {
            panic!("generated index entry failed to re-parse: {e}\n\ncontent:\n{content}")
        });
        assert_eq!(
            parsed.description.as_deref(),
            Some("Line one\nLine two\twith a tab\u{b}VT")
        );
    }

    // -----------------------------------------------------------------------
    // End-to-end `run()` against local bare repos
    // -----------------------------------------------------------------------

    #[test]
    #[serial(cwd)]
    fn test_publish_pushes_tag_and_index_entry() {
        let fixture = setup("yaml", "0.1.0", Some("YAML parsing for Beamtalk"));

        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_ok(), "{:?}", result.err());

        // The tag was pushed to origin.
        let tags = Command::new("git")
            .args(["tag", "--list"])
            .current_dir(fixture.library.path())
            .output()
            .unwrap();
        assert!(String::from_utf8_lossy(&tags.stdout).contains("v0.1.0"));

        let remote_tags = Command::new("git")
            .args(["ls-remote", "--tags", "origin"])
            .current_dir(fixture.library.path())
            .output()
            .unwrap();
        assert!(String::from_utf8_lossy(&remote_tags.stdout).contains("refs/tags/v0.1.0"));

        // The index entry landed in the registry index cache and was pushed.
        let index_dir = fixture.index_dir();
        let entry_path = index_dir.join("packages/yaml.toml");
        assert!(entry_path.is_file());
        let entry_content = std::fs::read_to_string(&entry_path).unwrap();
        assert!(entry_content.contains("version = \"0.1.0\""));

        let log = Command::new("git")
            .args(["log", "--oneline", "-1"])
            .current_dir(&index_dir)
            .output()
            .unwrap();
        assert!(String::from_utf8_lossy(&log.stdout).contains("Publish yaml v0.1.0"));
    }

    #[test]
    #[serial(cwd)]
    fn test_republish_same_version_errors() {
        let fixture = setup("yaml", "0.1.0", None);
        with_cwd(&utf8(&fixture.library), || run(false)).unwrap();

        // Re-running against the same (already-tagged, already-published)
        // version must fail — a real republish attempt, so the original
        // "bump the version" message is correct. The tag exists on `origin`
        // (a successful first publish pushes it there), which the
        // remote-tag check reports in preference to the local one — it's
        // the stronger signal that this version really was published, not
        // just tagged locally and abandoned mid-publish (BT-3000).
        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_err());
        let msg = flat_err(&result.unwrap_err());
        assert!(
            msg.contains("already exists on 'origin'"),
            "expected a tag-exists error: {msg}"
        );
        assert!(msg.contains("bump the version first"), "{msg}");
    }

    #[test]
    #[serial(cwd)]
    fn test_publish_rejects_empty_git_identity() {
        // A local config entry set to an empty string (as opposed to unset,
        // which would fall back to global config) shadows global config
        // regardless of what identity the environment running this test
        // happens to have configured — so this stays hermetic.
        let fixture = setup("yaml", "0.1.0", None);
        run_git(fixture.library.path(), &["config", "user.name", ""]);

        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_err());
        assert!(flat_err(&result.unwrap_err()).contains("configured but empty"));

        // The identity preflight runs before create_and_push_tag, not after
        // — a missing/empty identity must not leave a tag already pushed to
        // origin with no way to retry (beamtalk publish would then refuse to
        // re-run: "Tag already exists").
        let tags = Command::new("git")
            .args(["tag", "--list"])
            .current_dir(fixture.library.path())
            .output()
            .unwrap();
        assert!(
            String::from_utf8_lossy(&tags.stdout).trim().is_empty(),
            "no tag should have been created when the identity preflight fails"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_publish_dirty_tree_errors() {
        let fixture = setup("yaml", "0.1.0", None);
        std::fs::write(fixture.library.path().join("dirty.txt"), "oops").unwrap();

        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_err());
        assert!(flat_err(&result.unwrap_err()).contains("uncommitted changes"));
    }

    #[test]
    #[serial(cwd)]
    fn test_publish_dry_run_changes_nothing() {
        let fixture = setup("yaml", "0.1.0", None);

        let result = with_cwd(&utf8(&fixture.library), || run(true));
        assert!(result.is_ok(), "{:?}", result.err());

        // No local tag was created.
        let tags = Command::new("git")
            .args(["tag", "--list"])
            .current_dir(fixture.library.path())
            .output()
            .unwrap();
        assert!(String::from_utf8_lossy(&tags.stdout).trim().is_empty());

        // No index clone was pushed to / left with new content beyond the
        // read-only clone `ensure_index` makes for the preflight check.
        let index_dir = fixture.index_dir();
        assert!(!index_dir.join("packages/yaml.toml").is_file());
    }

    #[test]
    #[serial(cwd)]
    fn test_publish_second_release_appends_to_existing_entry() {
        let fixture = setup("yaml", "0.1.0", Some("YAML parsing"));
        with_cwd(&utf8(&fixture.library), || run(false)).unwrap();

        // Bump and republish.
        let manifest_path = fixture.library.path().join("beamtalk.toml");
        let content = std::fs::read_to_string(&manifest_path).unwrap();
        let bumped = content.replace("version = \"0.1.0\"", "version = \"0.2.0\"");
        std::fs::write(&manifest_path, bumped).unwrap();
        run_git(fixture.library.path(), &["commit", "-am", "release 0.2.0"]);

        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_ok(), "{:?}", result.err());

        let index_dir = fixture.index_dir();
        let entry = registry::read_entry(&index_dir, "yaml").unwrap().unwrap();
        assert_eq!(entry.versions.len(), 2);
        assert!(entry.find_version("0.1.0").is_some());
        assert!(entry.find_version("0.2.0").is_some());
        // The description from the first publish survives the second.
        assert_eq!(entry.description.as_deref(), Some("YAML parsing"));
    }

    /// BT-3000: a `publish` that pushes the release tag (step 2) and then
    /// dies before the registry index is updated (step 3) — e.g. the index
    /// remote went unreachable — must not tell a retrying user to bump the
    /// version. That advice, correct for a *true* republish, is actively
    /// harmful for a partial failure: it abandons the release already live
    /// on `origin` with no way to add it to the index later. See
    /// `docs/beamtalk-packages.md`, "If step 3 fails".
    #[test]
    #[serial(cwd)]
    fn test_publish_after_tag_pushed_but_index_not_updated_reports_manual_recovery() {
        let fixture = setup("yaml", "0.1.0", None);

        // Simulate the partial failure directly: push the release tag
        // exactly as `create_and_push_tag` would, without ever touching the
        // registry index (as if the process died right after step 2).
        run_git(
            fixture.library.path(),
            &["tag", "-a", "v0.1.0", "-m", "Release 0.1.0"],
        );
        run_git(fixture.library.path(), &["push", "origin", "v0.1.0"]);

        // A retry (`beamtalk publish` run again) must recognize the version
        // is not actually published yet and report the partial-failure
        // recovery, not the "already published" message.
        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_err());
        let msg = flat_err(&result.unwrap_err());

        assert!(
            !msg.contains("bump the version first"),
            "must not tell the user to bump the version — that would abandon \
             the release already tagged on origin: {msg}"
        );
        assert!(
            msg.contains("partial failure"),
            "should explain this is a partial failure, not a republish: {msg}"
        );
        assert!(
            msg.contains("re-run 'beamtalk publish'"),
            "should tell the user not to just retry publish: {msg}"
        );

        let index_dir = fixture.index_dir();
        assert!(
            msg.contains(index_dir.as_str()),
            "should point at the registry index checkout for manual recovery: {msg}"
        );
        assert!(msg.contains("git push"), "{msg}");

        // And indeed: the index was never updated with the 0.1.0 entry.
        assert!(registry::read_entry(&index_dir, "yaml").unwrap().is_none());
    }

    #[test]
    #[serial(cwd)]
    fn test_publish_no_origin_errors() {
        let temp = TempDir::new().unwrap();
        run_git(temp.path(), &["init"]);
        configure_git_identity(temp.path());
        std::fs::write(
            temp.path().join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        run_git(temp.path(), &["add", "."]);
        run_git(temp.path(), &["commit", "-m", "initial"]);

        let result = with_cwd(&utf8(&temp), || run(false));
        assert!(result.is_err());
        assert!(flat_err(&result.unwrap_err()).contains("No 'origin' remote"));
    }

    // -----------------------------------------------------------------------
    // Uncommitted index entry (BT-3002)
    // -----------------------------------------------------------------------

    /// A previous `publish` can push the release tag (step 2), have
    /// `write_index_entry` write `packages/{name}.toml` into the index
    /// clone, `git add` it (step 3)... and then have `git commit` itself
    /// fail or never run. The entry file is then present in the index
    /// clone's working tree but not committed. A retry must not read that
    /// filesystem presence as "already published" — `read_entry` alone
    /// can't tell staged-but-uncommitted from truly published, so
    /// `is_entry_committed` must gate `version_published` on `HEAD`, not
    /// just the working tree.
    #[test]
    #[serial(cwd)]
    fn test_republish_after_staged_uncommitted_index_entry_is_not_already_published() {
        let fixture = setup("yaml", "0.1.0", None);

        // Simulate the tag half of a partial failure: push the release tag
        // exactly as `create_and_push_tag` would (step 2 succeeded).
        run_git(
            fixture.library.path(),
            &["tag", "-a", "v0.1.0", "-m", "Release 0.1.0"],
        );
        run_git(fixture.library.path(), &["push", "origin", "v0.1.0"]);

        // Simulate the index half: `write_index_entry` wrote the file and it
        // was `git add`-ed, but `git commit` never landed.
        let index_dir = fixture.index_dir();
        let entry_path = index_dir.join("packages/yaml.toml");
        let origin_url = "https://example.test/yaml";
        std::fs::write(
            &entry_path,
            format!(
                "name = \"yaml\"\n\n[[versions]]\nversion = \"0.1.0\"\ngit = \"{origin_url}\"\n\
                 tag = \"v0.1.0\"\n"
            ),
        )
        .unwrap();
        run_git(index_dir.as_std_path(), &["add", "."]);

        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_err());
        let msg = flat_err(&result.unwrap_err());

        assert!(
            !msg.contains("bump the version first"),
            "a staged-but-uncommitted index entry must not read as 'already published': {msg}"
        );
        assert!(
            msg.contains("partial failure"),
            "should report the partial-failure recovery, not a republish: {msg}"
        );
    }

    /// As above, but the entry file was written and never staged at all
    /// (fully untracked) — e.g. `write_index_entry` succeeded but `git add`
    /// itself never ran before the process died.
    #[test]
    #[serial(cwd)]
    fn test_republish_after_untracked_index_entry_is_not_already_published() {
        let fixture = setup("yaml", "0.1.0", None);

        run_git(
            fixture.library.path(),
            &["tag", "-a", "v0.1.0", "-m", "Release 0.1.0"],
        );
        run_git(fixture.library.path(), &["push", "origin", "v0.1.0"]);

        let index_dir = fixture.index_dir();
        let entry_path = index_dir.join("packages/yaml.toml");
        let origin_url = "https://example.test/yaml";
        std::fs::write(
            &entry_path,
            format!(
                "name = \"yaml\"\n\n[[versions]]\nversion = \"0.1.0\"\ngit = \"{origin_url}\"\n\
                 tag = \"v0.1.0\"\n"
            ),
        )
        .unwrap();
        // Deliberately no `git add` — the file is fully untracked.

        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_err());
        let msg = flat_err(&result.unwrap_err());

        assert!(
            !msg.contains("bump the version first"),
            "an untracked index entry must not read as 'already published': {msg}"
        );
        assert!(
            msg.contains("partial failure"),
            "should report the partial-failure recovery, not a republish: {msg}"
        );
    }

    /// A package's `description` containing a newline (reachable via a TOML
    /// multi-line basic string in `beamtalk.toml`) must round-trip through
    /// a real `publish` run, not just through `render_new_index_entry`
    /// directly — covering the full path including the round-trip
    /// validation in `render_index_entry_content`.
    #[test]
    #[serial(cwd)]
    fn test_publish_description_with_newline_roundtrips() {
        let fixture = setup("yaml", "0.1.0", None);

        // `setup()`'s naive `description = "{d}"` interpolation can't carry a
        // raw newline (that's invalid TOML), so write a multi-line basic
        // string directly, the way a real `beamtalk.toml` author would.
        let manifest_path = fixture.library.path().join("beamtalk.toml");
        let manifest = format!(
            "[package]\nname = \"yaml\"\nversion = \"0.1.0\"\ndescription = \"\"\"\nLine one\n\
             Line two\twith a tab\"\"\"\n\n[registry]\nurl = \"{}\"\n",
            file_url(fixture.index_bare.path())
        );
        std::fs::write(&manifest_path, manifest).unwrap();
        run_git(
            fixture.library.path(),
            &["commit", "-am", "add description"],
        );

        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_ok(), "{:?}", result.err());

        let index_dir = fixture.index_dir();
        let entry = registry::read_entry(&index_dir, "yaml").unwrap().unwrap();
        assert_eq!(
            entry.description.as_deref(),
            Some("Line one\nLine two\twith a tab")
        );
    }

    // -----------------------------------------------------------------------
    // Commit-failure error message (BT-3002)
    // -----------------------------------------------------------------------

    /// If `git commit` itself fails, no local commit exists yet — the user
    /// needs to stage and commit first, not push. An empty repository
    /// reproduces this deterministically: `git add .` is a no-op (nothing to
    /// add) and the subsequent `git commit` fails with "nothing to commit",
    /// exercising the commit-failure branch without a contrived hook
    /// rejection.
    #[test]
    fn test_commit_and_push_index_commit_failure_says_stage_and_commit_not_push() {
        let dir = TempDir::new().unwrap();
        run_git(dir.path(), &["init"]);
        configure_git_identity(dir.path());

        let index_root = utf8(&dir);
        let err = commit_and_push_index(&index_root, "yaml", "0.1.0", "Test", "test@test.com")
            .unwrap_err();
        let msg = flat_err(&err);

        assert!(msg.contains("git add"), "{msg}");
        assert!(msg.contains("git commit"), "{msg}");
        assert!(
            !msg.contains("push the commit"),
            "must not tell the user to push a commit that was never created: {msg}"
        );
        assert!(
            !msg.contains("commit exists locally"),
            "must not claim a commit already exists when 'git commit' itself failed: {msg}"
        );
    }
}
