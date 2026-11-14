// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared build-script helpers for beamtalk crates.
//!
//! Provides `emit_beamtalk_version` which reads the workspace `VERSION` file
//! and git state, then exposes the `BEAMTALK_VERSION` compile-time env var
//! (exact-match identity) and `BEAMTALK_BUILD_ID` (human-facing, BT-4812).
//! Also provides `emit_spec_mapping_stamp`, which hashes
//! `beamtalk_spec_reader.erl` and exposes the result as
//! `BEAMTALK_SPEC_MAPPING_STAMP`.
//!
//! # Usage
//!
//! In a crate's `build.rs`:
//!
//! ```rust,ignore
//! fn main() {
//!     let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
//!     let workspace_root = std::path::Path::new(&manifest_dir)
//!         .parent()
//!         .and_then(std::path::Path::parent)
//!         .expect("Cannot find workspace root");
//!     beamtalk_build::emit_beamtalk_version(workspace_root);
//! }
//! ```

use beamtalk_cerl_doc::hash::fnv1a_64;
use std::fs;
use std::path::Path;
use std::process::Command;

/// Read VERSION file and git state, then expose `BEAMTALK_VERSION` at compile time.
///
/// On a release tag: `0.2.0`
/// Off a tag:        `0.2.0-dev+abc1234`
///
/// # Panics
///
/// Panics if the `VERSION` file cannot be read, is empty, or contains
/// characters outside `[A-Za-z0-9.+-]`.
pub fn emit_beamtalk_version(workspace_root: &Path) {
    let version_file = workspace_root.join("VERSION");
    println!("cargo:rerun-if-changed={}", version_file.display());

    // Track git state so version is recomputed on new commits/tags.
    // Uses `git rev-parse --git-path` to resolve each path correctly in both
    // regular repos and worktrees (where HEAD is worktree-private but refs
    // and packed-refs live in the shared git-common-dir).
    // Only paths that exist: Cargo reruns a build script on every build while
    // one of its rerun-if-changed paths is missing, and a fresh CI checkout has
    // no `packed-refs`, so beamtalk-cli and beamtalk-compiler-port (and every
    // crate above them) recompiled on every cargo command (BT-3879). Refs
    // packed into a later `packed-refs` leave `refs/heads`/`refs/tags`, so
    // that change still reruns the script.
    for p in ["HEAD", "refs/heads", "refs/tags", "packed-refs"] {
        if let Some(path) = git_path(p).filter(|path| Path::new(path).exists()) {
            println!("cargo:rerun-if-changed={path}");
        }
    }

    let base = fs::read_to_string(&version_file)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", version_file.display()));
    let base = base.trim();
    assert!(
        !base.is_empty(),
        "VERSION file is empty: {}",
        version_file.display()
    );
    assert!(
        base.chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '+')),
        "VERSION contains invalid characters (allowed: [A-Za-z0-9.+-]): {}",
        version_file.display()
    );

    let version = if git_on_tag() {
        base.to_string()
    } else if let Some(sha) = git_short_sha() {
        format!("{base}-dev+{sha}")
    } else {
        base.to_string()
    };

    println!("cargo:rustc-env=BEAMTALK_VERSION={version}");

    // Human-facing build identity (BT-4812). Kept separate from
    // `BEAMTALK_VERSION`, which is the exact-match identity compared by the
    // workspace handshake, the build stamp and the Erlang app `vsn`; this one
    // adds the channel, a `dirty` marker and the commit date for `--version`.
    println!("cargo:rerun-if-env-changed={ENV_BUILD_CHANNEL}");
    println!("cargo:rerun-if-env-changed={ENV_BUILD_COMMIT}");
    let index = git_path("index").filter(|path| Path::new(path).exists());
    if let Some(path) = index {
        // `git add`/`commit`/`status` rewrite the index, so a dirty marker is
        // refreshed on the next build after the tree is staged or committed.
        println!("cargo:rerun-if-changed={path}");
    }
    let channel = env_nonempty(ENV_BUILD_CHANNEL);
    let sha = env_nonempty(ENV_BUILD_COMMIT)
        .map(|c| c.chars().take(SHORT_SHA_LEN).collect::<String>())
        .or_else(git_short_sha);
    let date = sha.as_deref().and_then(git_commit_date);
    let id = compose_build_id(&BuildInputs {
        base,
        on_tag: git_on_tag(),
        sha: sha.as_deref(),
        dirty: git_dirty(),
        channel: channel.as_deref(),
        date: date.as_deref(),
    });
    println!("cargo:rustc-env=BEAMTALK_BUILD_ID={id}");
}

/// Env var naming the distribution channel (`edge`, `nightly`) of a CI build.
/// Set by `edge.yml` / `nightly.yml`; unset for local and release builds.
const ENV_BUILD_CHANNEL: &str = "BEAMTALK_BUILD_CHANNEL";
/// Env var carrying the commit being built, for builds without a `.git`
/// directory (source tarballs). Falls back to `git rev-parse` when unset.
const ENV_BUILD_COMMIT: &str = "BEAMTALK_BUILD_COMMIT";
const SHORT_SHA_LEN: usize = 7;

/// Everything [`compose_build_id`] needs, gathered by [`emit_beamtalk_version`].
#[derive(Debug, Clone, Copy)]
pub struct BuildInputs<'a> {
    /// Trimmed contents of the workspace `VERSION` file.
    pub base: &'a str,
    /// `HEAD` is exactly a git tag.
    pub on_tag: bool,
    /// Short commit, from `BEAMTALK_BUILD_COMMIT` or `git rev-parse`.
    pub sha: Option<&'a str>,
    /// The working tree has uncommitted changes to tracked files.
    pub dirty: bool,
    /// `edge` / `nightly` from `BEAMTALK_BUILD_CHANNEL`; `None` otherwise.
    pub channel: Option<&'a str>,
    /// Commit date (`YYYY-MM-DD`) of `sha`, when git can tell.
    pub date: Option<&'a str>,
}

/// Compose the human-facing build identity shown by `--version` and the REPL
/// banner. Always begins with the plain `VERSION` so anything parsing
/// `beamtalk <semver>` keeps working.
///
/// | Build                    | Result                                  |
/// |--------------------------|-----------------------------------------|
/// | Tagged release, clean    | `0.4.0`                                 |
/// | Edge / nightly           | `0.4.0-edge+2ce49a8 (2026-11-14)`       |
/// | Local checkout           | `0.4.0-dev+2ce49a8 (2026-11-14)`        |
/// | Local, uncommitted edits | `0.4.0-dev+2ce49a8.dirty (2026-11-14)`  |
/// | No git metadata          | `0.4.0`                                 |
///
/// An explicit channel beats the tag check, so a rolling `edge` tag that
/// happens to point at `HEAD` cannot masquerade as a release.
///
/// # Panics
///
/// Panics if `channel` contains characters outside `[A-Za-z0-9-]` (it ends up
/// in a semver pre-release identifier).
#[must_use]
pub fn compose_build_id(inputs: &BuildInputs<'_>) -> String {
    let BuildInputs {
        base,
        on_tag,
        sha,
        dirty,
        channel,
        date,
    } = *inputs;
    if let Some(c) = channel {
        assert!(
            !c.is_empty() && c.chars().all(|ch| ch.is_ascii_alphanumeric() || ch == '-'),
            "{ENV_BUILD_CHANNEL} must match [A-Za-z0-9-]+: {c:?}"
        );
    }
    if on_tag && !dirty && channel.is_none() {
        return base.to_string();
    }
    let Some(sha) = sha else {
        return base.to_string();
    };
    let channel = channel.unwrap_or("dev");
    let dirty = if dirty { ".dirty" } else { "" };
    let date = date.map(|d| format!(" ({d})")).unwrap_or_default();
    format!("{base}-{channel}+{sha}{dirty}{date}")
}

fn env_nonempty(name: &str) -> Option<String> {
    std::env::var(name)
        .ok()
        .map(|v| v.trim().to_string())
        .filter(|v| !v.is_empty())
}

fn git_dirty() -> bool {
    Command::new("git")
        .args(["status", "--porcelain", "--untracked-files=no"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .is_some_and(|o| !o.stdout.iter().all(u8::is_ascii_whitespace))
}

fn git_commit_date(sha: &str) -> Option<String> {
    Command::new("git")
        .args(["show", "-s", "--format=%cs", sha])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .filter(|d| !d.is_empty())
}

fn git_on_tag() -> bool {
    Command::new("git")
        .args(["describe", "--exact-match", "--tags", "HEAD"])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}

fn git_path(path: &str) -> Option<String> {
    Command::new("git")
        .args(["rev-parse", "--git-path", path])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
}

fn git_short_sha() -> Option<String> {
    Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
}

/// Hash `runtime/apps/beamtalk_compiler/src/beamtalk_spec_reader.erl` and
/// expose the result as the `BEAMTALK_SPEC_MAPPING_STAMP` compile-time env var.
///
/// The FFI type-spec cache (`_build/type_cache/`, and the shared
/// OTP-version-keyed tier) keys cached specs by module name, `.beam` mtime,
/// and OTP version only — none of which change just because the *compiler's*
/// Erlang→Beamtalk type-mapping logic (`beamtalk_spec_reader.erl`'s
/// `map_type/1` and friends) changed. Baking a content hash of that file into
/// the binary at compile time gives cache entries a stamp to compare against:
/// a build whose mapping logic differs produces a different stamp, so stale
/// entries are detected as a miss rather than silently reused forever.
///
/// Deriving the stamp from a hash (rather than a hand-maintained constant)
/// means there is nothing to remember to bump — any change to the reader's
/// source, including comment-only edits, rotates the stamp automatically.
/// The hash itself runs once per `cargo build` (via `rerun-if-changed`), not
/// per `beamtalk` invocation, so comparing it at runtime is a cheap string
/// comparison against a `&'static str` baked in via `env!()`.
///
/// # Panics
///
/// Panics if `beamtalk_spec_reader.erl` cannot be read — this file is
/// committed source, so its absence indicates a broken checkout.
pub fn emit_spec_mapping_stamp(workspace_root: &Path) {
    let reader_path = workspace_root
        .join("runtime")
        .join("apps")
        .join("beamtalk_compiler")
        .join("src")
        .join("beamtalk_spec_reader.erl");
    println!("cargo:rerun-if-changed={}", reader_path.display());

    let content = fs::read(&reader_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", reader_path.display()));

    let hash = fnv1a_64(&content);
    println!("cargo:rustc-env=BEAMTALK_SPEC_MAPPING_STAMP={hash:016x}");
}

#[cfg(test)]
mod tests {
    use super::*;

    fn inputs() -> BuildInputs<'static> {
        BuildInputs {
            base: "0.4.0",
            on_tag: false,
            sha: Some("2ce49a8"),
            dirty: false,
            channel: None,
            date: Some("2026-11-14"),
        }
    }

    #[test]
    fn release_build_prints_plain_version() {
        let id = compose_build_id(&BuildInputs {
            on_tag: true,
            ..inputs()
        });
        assert_eq!(id, "0.4.0");
    }

    #[test]
    fn edge_build_names_channel_commit_and_date() {
        let id = compose_build_id(&BuildInputs {
            channel: Some("edge"),
            ..inputs()
        });
        assert_eq!(id, "0.4.0-edge+2ce49a8 (2026-11-14)");
    }

    #[test]
    fn nightly_channel_beats_a_tag_pointing_at_head() {
        let id = compose_build_id(&BuildInputs {
            on_tag: true,
            channel: Some("nightly"),
            ..inputs()
        });
        assert_eq!(id, "0.4.0-nightly+2ce49a8 (2026-11-14)");
    }

    #[test]
    fn no_git_metadata_prints_plain_version() {
        let id = compose_build_id(&BuildInputs {
            sha: None,
            date: None,
            ..inputs()
        });
        assert_eq!(id, "0.4.0");
        // A channel with nothing to name still degrades to the plain version.
        let id = compose_build_id(&BuildInputs {
            sha: None,
            date: None,
            channel: Some("edge"),
            ..inputs()
        });
        assert_eq!(id, "0.4.0");
    }

    #[test]
    fn local_checkout_is_dev_and_marks_dirty_trees() {
        assert_eq!(
            compose_build_id(&inputs()),
            "0.4.0-dev+2ce49a8 (2026-11-14)"
        );
        let id = compose_build_id(&BuildInputs {
            dirty: true,
            ..inputs()
        });
        assert_eq!(id, "0.4.0-dev+2ce49a8.dirty (2026-11-14)");
    }

    #[test]
    fn dirty_tagged_checkout_is_not_a_release() {
        let id = compose_build_id(&BuildInputs {
            on_tag: true,
            dirty: true,
            date: None,
            ..inputs()
        });
        assert_eq!(id, "0.4.0-dev+2ce49a8.dirty");
    }

    #[test]
    fn missing_date_is_omitted() {
        let id = compose_build_id(&BuildInputs {
            date: None,
            ..inputs()
        });
        assert_eq!(id, "0.4.0-dev+2ce49a8");
    }

    #[test]
    #[should_panic(expected = "BEAMTALK_BUILD_CHANNEL")]
    fn rejects_channel_with_invalid_characters() {
        let _ = compose_build_id(&BuildInputs {
            channel: Some("ed ge"),
            ..inputs()
        });
    }
}
