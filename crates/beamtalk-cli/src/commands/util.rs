// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for CLI commands.
//!
//! **DDD Context:** CLI

use beamtalk_core::file_walker::FileWalker;
use beamtalk_workspace::hex_encode;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use sha2::{Digest, Sha256};
use std::fs;
use std::time::SystemTime;

pub(crate) use beamtalk_cli::path_util::to_forward_slash;

/// Escape HTML special characters.
///
/// Shared by `commands/doc` and `commands/registry` — both render third-party
/// or user-supplied text into HTML and must escape the same four characters.
/// Centralised here (the shared-helpers leaf) so the two renderer modules
/// import a single definition rather than each carrying a private copy.
pub(crate) fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Build the BEAM module name for a user-code file stem (`bt@<normalised-stem>`).
///
/// ADR 0016: all user-code modules use the `bt@` prefix. The stem is normalised
/// via [`beamtalk_codegen::core_erlang::to_module_name`] so that e.g.
/// `my-class.bt` → `bt@my_class`.
pub(crate) fn bt_module_name_from_stem(stem: &str) -> String {
    format!("bt@{}", beamtalk_codegen::core_erlang::to_module_name(stem))
}

/// Build the package-qualified BEAM module name (`bt@<pkg>@<rel>`).
///
/// ADR 0026: package-mode modules use the `bt@<package>@<relative-path>` naming
/// convention, where `rel` is the `snake_case` module path relative to the package
/// source root (e.g. `"http@server"` for `src/http/server.bt`). This helper
/// centralises the format string so all three build-phase callers agree on the
/// exact shape — change one, change all.
pub(crate) fn bt_qualified_module_name(pkg: &str, rel: &str) -> String {
    format!("bt@{pkg}@{rel}")
}

/// What a test assertion expects: a value or an error.
///
/// Shared between stdlib tests (`test_stdlib`) and doc tests (`doc_tests`).
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expected {
    /// Match formatted result string (`_` for wildcard).
    Value(String),
    /// Match `#beamtalk_error{kind = Kind}` on error.
    Error { kind: String },
}

/// Read the modification time of a file, returning `None` on any error.
pub(super) fn mtime_of(path: &Utf8Path) -> Option<SystemTime> {
    fs::metadata(path).ok()?.modified().ok()
}

/// SHA-256 content hash of a file, as a lowercase hex string, or `None` if
/// the file cannot be read.
///
/// The source of truth for batch-build staleness checks
/// (`Pass1Cache` and `detect_changes`'s `.bt`-vs-`.beam` decision). Unlike
/// mtime, a content hash cannot be fooled by git branch switches (which
/// restore old content under a fresh mtime) or by tools that preserve or
/// backdate mtimes on write — both of which make mtime-only staleness checks
/// either spuriously rebuild (benign) or, worse, incorrectly skip a rebuild
/// (a stale `.beam`). Reuses `sha2`, already a dependency of this crate and
/// of `beamtalk-workspace`, and that crate's [`beamtalk_workspace::hex_encode`]
/// leaf for the digest-to-hex-string step — no new hash crate, and no
/// second copy of the hex-encoding loop, is introduced.
pub(super) fn content_hash_of(path: &Utf8Path) -> Option<String> {
    let bytes = fs::read(path).ok()?;
    Some(sha256_hex(&bytes))
}

/// SHA-256 hash of arbitrary bytes, as a lowercase hex string — the shared
/// leaf [`content_hash_of`] and [`protocol_content_hash`] both hash through,
/// so there is exactly one digest-to-hex-string routine in this crate.
pub(crate) fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    hex_encode(&digest)
}

/// Content hash of a protocol's *own* AST, independent of which file it came
/// from (ADR 0127 §10a; BT-3591) — used to build a class's build-graph edge
/// to each protocol its `uses:` lines name (`detect_changes`'s combined
/// cache key) and, eventually, the `uses => [{Name, Hash}]` `__beamtalk_meta`
/// entry (BT-3590/BT-3625's codegen work; not produced by this crate).
///
/// Re-unparses `protocol` into canonical source text via
/// [`beamtalk_core::unparse::unparse_module`] (wrapped in a throwaway,
/// otherwise-empty `Module`) rather than hashing the *defining file's* raw
/// content: a protocol's hash must depend only on that protocol's own
/// definition, not on unrelated content sharing its file (a doc comment
/// edit elsewhere in the file, a second protocol in the same file changing)
/// — hashing the whole file would force every one of its users to rebuild
/// on any edit to that file, not just an edit to the protocol they actually
/// use. Canonical unparse output (rather than the original source slice)
/// also means two byte-identical protocols defined with different
/// formatting/whitespace hash the same, matching this hash's only real job:
/// detecting a *semantic* change to what `uses:` would flatten in.
#[must_use]
pub(crate) fn protocol_content_hash(protocol: &beamtalk_core::ast::ProtocolDefinition) -> String {
    let wrapper = beamtalk_core::ast::Module {
        protocols: vec![protocol.clone()],
        ..beamtalk_core::ast::Module::new(Vec::new(), protocol.span)
    };
    sha256_hex(beamtalk_core::unparse::unparse_module(&wrapper).as_bytes())
}

/// [`content_hash_of`] for every file in `paths`, keyed by path string.
///
/// A single build hashes each source file's content in more than
/// one place — Pass 1's staleness check and cache-entry rebuild
/// (`build_cache::partition_files` / `build_cache::build_cache_entries`),
/// and Pass 2's `.beam` staleness check (`detect_changes`). Computing every
/// file's hash once here and threading the map through those call sites
/// (rather than each calling [`content_hash_of`] independently) keeps a
/// build to one content-hash pass per file instead of two or three — the
/// difference between a `stat()` and hashing full file contents is real, and
/// The acceptance criteria specifically call out "no measurable
/// regression in warm no-op build time". Files that can't be read are
/// omitted, matching [`content_hash_of`]'s `None`-on-error behaviour —
/// callers already treat a missing hash as "must recompute".
pub(super) fn content_hashes_of(
    paths: &[Utf8PathBuf],
) -> std::collections::HashMap<String, String> {
    paths
        .iter()
        .filter_map(|p| content_hash_of(p).map(|h| (p.as_str().to_string(), h)))
        .collect()
}

/// Find the project root by requiring a `beamtalk.toml` in the current directory.
///
/// Returns the current working directory as a [`Utf8PathBuf`] if a manifest is
/// present, or an error telling the user to run from a Beamtalk project root.
pub(crate) fn find_project_root() -> Result<Utf8PathBuf> {
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

/// Write `contents` to `path` atomically: stage in a sibling temp file keyed
/// by `tmp_prefix` and the current pid, then rename into place (atomic on the
/// same filesystem). The pid suffix ensures concurrent builders don't clobber
/// each other's staging file; the final rename is the accepted last-write-wins
/// race (ADR 0098 §1).
///
/// Shared by `commands/build_stamp` and `commands/deps/snapshot` — both use
/// the same two-step stage-then-rename pattern and differ only in the prefix
/// string they pass (`.beamtalk-stamp.` vs `.beamtalk-dep-graph.`), which
/// keeps temp files identifiable by domain when debugging a partial build.
pub(crate) fn write_atomic(
    path: &Utf8Path,
    contents: &str,
    tmp_prefix: &str,
) -> std::io::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!("{}{}.tmp", tmp_prefix, std::process::id()));
    fs::write(&tmp, contents)?;
    fs::rename(&tmp, path)
}

/// Where a corpus-compiling command (`build-stdlib`, `test-stdlib`, `test`)
/// should write its `.core` files, and how to clean that directory up
/// afterwards.
///
/// Normally these commands only need `.core` files as a throwaway
/// intermediate on the way to `.beam`, so the default is an auto-cleaned
/// temp directory. BT-3509's corpus `.core` diff harness (`just core-diff`;
/// see `docs/development/testing-strategy.md` § Corpus `.core` diff
/// harness) is the only consumer that needs them to outlive the compiling
/// process, and it asks for that via `BEAMTALK_CORE_SNAPSHOT_DIR` — sharing
/// this one resolver rather than each of the three call sites growing its
/// own copy of the "check the env var, else make a tempdir" branch
/// (CLAUDE.md's no-duplicate-implementations rule).
pub(crate) enum CoreOutputDir {
    /// Deleted when dropped. Never read — held only for its `Drop` impl, so
    /// every caller binds it (`let (dir, _guard) = ...`) rather than letting
    /// it drop immediately.
    Temp(#[allow(dead_code)] tempfile::TempDir),
    /// Left on disk for the harness to read after this process exits.
    Persistent,
}

/// Resolves a [`CoreOutputDir`] and its path.
///
/// Bind the returned guard even where its value is never read directly
/// (`let (dir, _guard) = core_output_dir()?;`) — dropping it early deletes a
/// temp directory a caller is still writing into.
pub(crate) fn core_output_dir() -> Result<(Utf8PathBuf, CoreOutputDir)> {
    if let Some(dir) = std::env::var_os("BEAMTALK_CORE_SNAPSHOT_DIR") {
        // `var_os`, checked for presence the same way `build_stdlib.rs`'s
        // `want_core_snapshot` does — and, unlike `env::var`, one that
        // fails loudly (not a silent fall-through to the temp-dir branch
        // below) if the value isn't valid UTF-8, so the two checks can
        // never disagree about whether a snapshot was requested.
        let path = Utf8PathBuf::from_path_buf(std::path::PathBuf::from(dir)).map_err(|p| {
            miette::miette!(
                "BEAMTALK_CORE_SNAPSHOT_DIR is not valid UTF-8: {}",
                p.display()
            )
        })?;
        fs::create_dir_all(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to create '{path}'"))?;
        return Ok((path, CoreOutputDir::Persistent));
    }

    let temp_dir = tempfile::tempdir()
        .into_diagnostic()
        .wrap_err("Failed to create temporary directory")?;
    let path = Utf8PathBuf::from_path_buf(temp_dir.path().to_path_buf())
        .map_err(|_| miette::miette!("Non-UTF-8 temp directory path"))?;
    Ok((path, CoreOutputDir::Temp(temp_dir)))
}

/// Find files matching the given extensions in a path.
///
/// - If `path` is a file, validates it has one of the given extensions and returns it.
/// - If `path` is a directory, returns all matching files directly inside it (sorted).
/// - If `path` does not exist, returns an error.
///
/// This is a **non-recursive** scan. For recursive directory walking, use
/// [`FileWalker`](beamtalk_core::file_walker::FileWalker) directly.
pub fn find_files(path: &Utf8Path, extensions: &[&str]) -> Result<Vec<Utf8PathBuf>> {
    FileWalker::new()
        .extensions(extensions)
        .recursive(false)
        .walk(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_content_hash_of_stable_for_same_content() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("a.bt");
        fs::write(&file, "counter := [0].").unwrap();

        let hash1 = content_hash_of(&file).unwrap();
        let hash2 = content_hash_of(&file).unwrap();
        assert_eq!(
            hash1, hash2,
            "hashing the same content must be deterministic"
        );
        assert_eq!(hash1.len(), 64, "SHA-256 hex digest is 64 chars");
    }

    #[test]
    fn test_content_hash_of_changes_with_content() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("a.bt");

        fs::write(&file, "counter := [0].").unwrap();
        let hash_a = content_hash_of(&file).unwrap();

        fs::write(&file, "counter := [1].").unwrap();
        let hash_b = content_hash_of(&file).unwrap();

        assert_ne!(hash_a, hash_b, "different content must hash differently");
    }

    #[test]
    fn test_content_hash_of_same_content_different_mtime() {
        // The hash must depend only on bytes, not on filesystem
        // metadata — rewriting identical content (which bumps mtime) must
        // produce the same hash.
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("a.bt");

        fs::write(&file, "counter := [0].").unwrap();
        let hash_a = content_hash_of(&file).unwrap();

        std::thread::sleep(std::time::Duration::from_millis(10));
        fs::write(&file, "counter := [0].").unwrap();
        let hash_b = content_hash_of(&file).unwrap();

        assert_eq!(
            hash_a, hash_b,
            "identical content must hash the same regardless of mtime"
        );
    }

    #[test]
    fn test_content_hash_of_missing_file_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let missing = dir_path.join("does-not-exist.bt");
        assert!(content_hash_of(&missing).is_none());
    }

    /// ADR 0127 §10a / BT-3591: parses `source` and returns its single
    /// protocol definition, for exercising [`protocol_content_hash`] without
    /// a file on disk.
    fn parse_one_protocol(source: &str) -> beamtalk_core::ast::ProtocolDefinition {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        assert_eq!(module.protocols.len(), 1);
        module.protocols.into_iter().next().unwrap()
    }

    #[test]
    fn test_protocol_content_hash_stable_for_same_definition() {
        let protocol = parse_one_protocol(
            "Protocol define: Comparable\n  \
             < other :: Self -> Boolean\n\n  \
             max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]\n",
        );
        let hash1 = protocol_content_hash(&protocol);
        let hash2 = protocol_content_hash(&protocol);
        assert_eq!(hash1, hash2);
        assert_eq!(hash1.len(), 64, "SHA-256 hex digest is 64 chars");
    }

    #[test]
    fn test_protocol_content_hash_changes_with_provision_body() {
        let a = parse_one_protocol(
            "Protocol define: Comparable\n  \
             < other :: Self -> Boolean\n\n  \
             max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]\n",
        );
        let b = parse_one_protocol(
            "Protocol define: Comparable\n  \
             < other :: Self -> Boolean\n\n  \
             max: other :: Self -> Self => (self < other) ifTrue: [self] ifFalse: [other]\n",
        );
        assert_ne!(
            protocol_content_hash(&a),
            protocol_content_hash(&b),
            "a changed provision body must change the hash"
        );
    }

    #[test]
    fn test_protocol_content_hash_independent_of_defining_file() {
        // Two byte-identical protocol definitions, one parsed alone and one
        // parsed alongside an unrelated class in the same file, must hash
        // identically — the hash is a property of the protocol's own AST,
        // not of whatever else happens to share its file.
        let source = "Protocol define: Printable\n  asString -> String\n";
        let tokens_a = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module_a, diags_a) = beamtalk_core::source_analysis::parse(tokens_a);
        assert!(diags_a.is_empty());
        let a = module_a.protocols.into_iter().next().unwrap();

        let combined = format!("Object subclass: Unrelated\n  m => 1\n\n{source}");
        let tokens_b = beamtalk_core::source_analysis::lex_with_eof(&combined);
        let (module_b, diags_b) = beamtalk_core::source_analysis::parse(tokens_b);
        assert!(diags_b.is_empty());
        let b = module_b.protocols.into_iter().next().unwrap();

        assert_eq!(
            protocol_content_hash(&a),
            protocol_content_hash(&b),
            "the same protocol definition must hash the same regardless of \
             unrelated content in its file"
        );
    }

    #[test]
    fn test_find_files_single_file() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("test.bt");
        fs::write(&file, "").unwrap();

        let files = find_files(&file, &["bt"]).unwrap();
        assert_eq!(files, vec![file]);
    }

    #[test]
    fn test_find_files_wrong_extension() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("test.txt");
        fs::write(&file, "").unwrap();

        let result = find_files(&file, &["bt"]);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_files_directory() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(dir_path.join("b.bt"), "").unwrap();
        fs::write(dir_path.join("c.txt"), "").unwrap();

        let files = find_files(dir_path, &["bt"]).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files[0].file_name() == Some("a.bt"));
        assert!(files[1].file_name() == Some("b.bt"));
    }

    #[test]
    fn test_find_files_nonexistent() {
        let result = find_files(Utf8Path::new("/nonexistent"), &["bt"]);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_files_multiple_extensions() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(dir_path.join("b.btscript"), "").unwrap();
        fs::write(dir_path.join("c.txt"), "").unwrap();

        let files = find_files(dir_path, &["bt", "btscript"]).unwrap();
        assert_eq!(files.len(), 2);
    }
}
