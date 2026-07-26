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

    // Preflight 3: the release tag doesn't already exist, locally or on origin.
    ensure_tag_absent(&project_root, &tag)?;

    // Preflight 4: the version isn't already published. Refresh the index
    // first so a publish run always sees the latest state, not a stale local
    // clone from an earlier `deps` resolution.
    let location = registry::resolve_registry_location(&project_root, manifest.registry.as_ref());
    let index_root = registry::ensure_index(&location, &project_root, true)?;
    let existing_entry = registry::read_entry(&index_root, name)?;
    if let Some(entry) = &existing_entry {
        if entry.find_version(version).is_some() {
            miette::bail!(
                "Version '{version}' of package '{name}' is already published in the \
                 registry ({location}).\n\n  \
                 Bump the version first: beamtalk version bump patch"
            );
        }
    }

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
            // The index lives in a separate clone from the project (typically
            // `_build/registry/index/`), so it does not inherit a git identity
            // the user has configured only locally in their project repo.
            // Resolve identity there and pass it through explicitly, rather
            // than relying on whatever the index clone's own local/global
            // config happens to be.
            let (author_name, author_email) = get_git_identity(&project_root)?;
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
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    };

    Ok((read("user.name")?, read("user.email")?))
}

/// Ensure the release tag doesn't already exist locally or on `origin`.
fn ensure_tag_absent(project_root: &Utf8Path, tag: &str) -> Result<()> {
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
    if !String::from_utf8_lossy(&local.stdout).trim().is_empty() {
        miette::bail!(
            "Tag '{tag}' already exists locally.\n\n  \
             This version may already have been published — bump the version first, or \
             delete the stale tag with `git tag -d {tag}` if the previous publish failed \
             partway through."
        );
    }

    let remote = Command::new("git")
        .args(["ls-remote", "--tags", "origin", tag])
        .current_dir(project_root)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to list remote git tags on 'origin'")?;
    if !remote.status.success() {
        let stderr = String::from_utf8_lossy(&remote.stderr);
        miette::bail!("Failed to check remote tags on 'origin':\n{stderr}");
    }
    if !String::from_utf8_lossy(&remote.stdout).trim().is_empty() {
        miette::bail!(
            "Tag '{tag}' already exists on 'origin'.\n\n  \
             This version has already been published — bump the version first."
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

/// Escape a value for embedding in a TOML basic string.
fn escape_toml_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
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
             The release tag was already pushed. Resolve the index issue and push the \
             commit at '{index_root}' manually, or contact the registry maintainer."
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

    fn flat_err(err: &miette::Report) -> String {
        format!("{err:?}")
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn utf8(dir: &TempDir) -> Utf8PathBuf {
        Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap()
    }

    /// Run `f` with the process cwd set to `dir`, always restoring the
    /// original cwd afterward. Callers must serialize with `#[serial(cwd)]`.
    fn with_cwd<T>(dir: &Utf8Path, f: impl FnOnce() -> T) -> T {
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir).unwrap();
        let result = f();
        std::env::set_current_dir(original_dir).unwrap();
        result
    }

    /// Set up a "publishable" library: a bare repo standing in for `origin`,
    /// a working clone of it with `beamtalk.toml` committed, and a second
    /// bare repo standing in for the registry index, referenced from the
    /// library's manifest via `[registry] url` (so tests never touch process
    /// environment variables).
    struct TestFixture {
        // Kept alive for the duration of the test — dropping deletes the tempdirs.
        _origin_bare: TempDir,
        _index_bare: TempDir,
        library: TempDir,
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
        // Real projects scaffolded by `beamtalk new` ignore `_build/`; publish
        // itself clones the registry index into `_build/registry/index/`, so
        // without this the working tree would look dirty on a re-publish.
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
            _index_bare: index_bare,
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
        run_git(
            dir.path(),
            &["remote", "add", "origin", dir.path().to_str().unwrap()],
        );

        let err = ensure_tag_absent(&utf8(&dir), "v1.0.0").unwrap_err();
        assert!(flat_err(&err).contains("already exists locally"), "{err:?}");
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
    fn test_escape_toml_string_escapes_quotes_and_backslashes() {
        assert_eq!(escape_toml_string(r#"say "hi""#), r#"say \"hi\""#);
        assert_eq!(escape_toml_string(r"a\b"), r"a\\b");
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

        // The index entry landed in `_build/registry/index/` and was pushed.
        let index_dir = crate::commands::build_layout::BuildLayout::new(utf8(&fixture.library))
            .registry_index_dir();
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

        // Re-running against the same (already-tagged) version must fail —
        // the local tag preflight catches it before any network operation.
        let result = with_cwd(&utf8(&fixture.library), || run(false));
        assert!(result.is_err());
        assert!(
            flat_err(&result.unwrap_err()).contains("already exists locally"),
            "expected a tag-exists error"
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
        let index_dir = crate::commands::build_layout::BuildLayout::new(utf8(&fixture.library))
            .registry_index_dir();
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

        let index_dir = crate::commands::build_layout::BuildLayout::new(utf8(&fixture.library))
            .registry_index_dir();
        let entry = registry::read_entry(&index_dir, "yaml").unwrap().unwrap();
        assert_eq!(entry.versions.len(), 2);
        assert!(entry.find_version("0.1.0").is_some());
        assert!(entry.find_version("0.2.0").is_some());
        // The description from the first publish survives the second.
        assert_eq!(entry.description.as_deref(), Some("YAML parsing"));
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
}
