// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Incremental-build tests: stale-artifact cleanup and `detect_changes` across new/unchanged/modified/orphaned/branch-switch scenarios, including the `seed_beam_hash` / `make_pairs` fixtures they share.

use super::*;
use crate::commands::util::content_hash_of;

#[test]
fn test_clean_stale_artifacts_removes_orphaned_beam() {
    let temp = TempDir::new().unwrap();
    let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    // Create a stale .beam and .core from a previous build
    write_test_file(&build_dir.join("bt@old_pkg@counter.beam"), "stale");
    write_test_file(&build_dir.join("bt@old_pkg@counter.core"), "stale");
    // Create the current build's .beam
    let current = build_dir.join("bt@new_pkg@counter.beam");
    write_test_file(&current, "current");
    write_test_file(&build_dir.join("bt@new_pkg@counter.core"), "current");

    clean_stale_artifacts(&build_dir, std::slice::from_ref(&current), "new_pkg").unwrap();

    // Stale files should be removed
    assert!(!build_dir.join("bt@old_pkg@counter.beam").exists());
    assert!(!build_dir.join("bt@old_pkg@counter.core").exists());
    // Current files should remain
    assert!(current.exists());
    assert!(build_dir.join("bt@new_pkg@counter.core").exists());
}

#[test]
fn test_clean_stale_artifacts_removes_old_app_file() {
    let temp = TempDir::new().unwrap();
    let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    write_test_file(&build_dir.join("old_name.app"), "stale");
    write_test_file(&build_dir.join("new_name.app"), "current");
    let beam = build_dir.join("bt@new_name@main.beam");
    write_test_file(&beam, "current");

    clean_stale_artifacts(&build_dir, &[beam], "new_name").unwrap();

    assert!(!build_dir.join("old_name.app").exists());
    assert!(build_dir.join("new_name.app").exists());
}

#[test]
fn test_clean_stale_artifacts_preserves_non_bt_files() {
    let temp = TempDir::new().unwrap();
    let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    // Non-bt@ beam files (like OTP callback modules) should be preserved
    write_test_file(&build_dir.join("beamtalk_myapp_app.beam"), "callback");
    write_test_file(&build_dir.join("beamtalk_myapp_app.erl"), "source");
    let beam = build_dir.join("bt@myapp@main.beam");
    write_test_file(&beam, "current");

    clean_stale_artifacts(&build_dir, &[beam], "myapp").unwrap();

    assert!(build_dir.join("beamtalk_myapp_app.beam").exists());
    assert!(build_dir.join("beamtalk_myapp_app.erl").exists());
}

// ── BT-1682 / BT-3120: Change detection tests ────────────────────

/// Helper: seed the beam-hash sidecar as if `file` was last successfully
/// compiled at its *current* on-disk content — i.e. record today's hash
/// as "the content that produced the `.beam`". Simulates a prior
/// successful `detect_changes` + compile + `save_beam_hash_cache` cycle
/// without needing a real compiler.
fn seed_beam_hash(build_dir: &Utf8Path, file: &Utf8Path) {
    let hash = content_hash_of(file).expect("test file must be readable");
    let mut hashes = super::super::super::build_cache::load_beam_hash_cache(build_dir);
    hashes.insert(file.as_str().to_string(), hash);
    super::super::super::build_cache::save_beam_hash_cache(build_dir, &hashes);
}

/// Helper: create `file_module_pairs` matching the format used by the build pipeline.
fn make_pairs(
    source_files: &[Utf8PathBuf],
    build_dir: &Utf8Path,
) -> Vec<(Utf8PathBuf, String, Utf8PathBuf)> {
    source_files
        .iter()
        .map(|f| {
            let stem = f.file_stem().unwrap_or("unknown");
            let module_name = format!("bt@{stem}");
            let core_file = build_dir.join(format!("{module_name}.core"));
            (f.clone(), module_name, core_file)
        })
        .collect()
}

#[test]
fn test_detect_changes_new_files_no_build_dir() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    write_test_file(&src_dir.join("counter.bt"), "counter := [0].");

    // Build dir doesn't exist yet — all files should be changed
    let build_dir = project.join("build");
    let source_files = vec![src_dir.join("counter.bt")];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert_eq!(result.changed_files.len(), 1);
    assert!(result.unchanged_files.is_empty());
    assert!(result.orphaned_beam_files.is_empty());
}

#[test]
fn test_detect_changes_no_beam_exists() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    write_test_file(&src_dir.join("counter.bt"), "counter := [0].");

    let source_files = vec![src_dir.join("counter.bt")];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert_eq!(
        result.changed_files.len(),
        1,
        "New file with no .beam should be changed"
    );
    assert!(result.unchanged_files.is_empty());
}

#[test]
fn test_detect_changes_up_to_date() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    let source_file = src_dir.join("counter.bt");
    write_test_file(&source_file, "counter := [0].");
    write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
    // Record the current content hash as "what produced this .beam"
    // (BT-3120) — without this the sidecar has no prior hash to compare
    // against, and the file must be recompiled once regardless of mtime.
    seed_beam_hash(&build_dir, &source_file);

    let source_files = vec![source_file.clone()];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert!(
        result.changed_files.is_empty(),
        "Up-to-date file should not be changed"
    );
    assert_eq!(result.unchanged_files.len(), 1);
}

#[test]
fn test_detect_changes_source_modified() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    let source_file = src_dir.join("counter.bt");
    write_test_file(&source_file, "counter := [0].");
    write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
    seed_beam_hash(&build_dir, &source_file);

    // Modify the source after the .beam was built from it.
    write_test_file(&source_file, "counter := [1].");

    let source_files = vec![source_file];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert_eq!(
        result.changed_files.len(),
        1,
        "Modified source should be changed"
    );
    assert!(result.unchanged_files.is_empty());
}

/// BT-3120: a source rewritten with identical content but a backdated (or
/// preserved) mtime must still be treated as up-to-date — the hash, not
/// the mtime, is authoritative.
#[test]
fn test_detect_changes_touch_without_change_not_recompiled() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    let source_file = src_dir.join("counter.bt");
    write_test_file(&source_file, "counter := [0].");
    write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
    seed_beam_hash(&build_dir, &source_file);

    // Rewrite the exact same content (a `touch`, or an editor
    // save-without-edit) — bumps mtime, leaves bytes unchanged.
    std::thread::sleep(std::time::Duration::from_millis(10));
    write_test_file(&source_file, "counter := [0].");

    let source_files = vec![source_file];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert!(
        result.changed_files.is_empty(),
        "touch-without-change must not trigger recompilation"
    );
    assert_eq!(result.unchanged_files.len(), 1);
}

/// BT-3120: `known_hashes` (Pass 1's already-computed content hashes) must
/// actually be consulted instead of always re-hashing from disk — proven
/// here by seeding a deliberately wrong hash for the file and observing
/// `detect_changes` trust it (report the file changed) even though its
/// real on-disk content is unchanged from what produced the `.beam`. A
/// no-op `known_hashes` (the `HashMap::new()` used by every other test in
/// this module) would instead fall back to hashing the file directly and
/// correctly report it unchanged — so this test also pins that the
/// fallback and fast paths are actually two different code paths, not
/// one being silently skipped.
#[test]
fn test_detect_changes_trusts_known_hashes_over_rereading() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    let source_file = src_dir.join("counter.bt");
    write_test_file(&source_file, "counter := [0].");
    write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
    seed_beam_hash(&build_dir, &source_file);

    let source_files = vec![source_file.clone()];
    let pairs = make_pairs(&source_files, &build_dir);

    // A "known" hash that does NOT match either the file's real content
    // or the beam-hash sidecar's recorded hash — if `detect_changes`
    // trusts it, the file is reported changed; if it were silently
    // ignored in favour of re-reading the file, it would be unchanged.
    let mut known_hashes = HashMap::new();
    known_hashes.insert(
        source_file.as_str().to_string(),
        "not-the-real-hash".to_string(),
    );

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &known_hashes);
    assert_eq!(
        result.changed_files,
        vec![source_file],
        "a precomputed hash from known_hashes must be trusted, not silently discarded"
    );
    assert!(result.unchanged_files.is_empty());
}

/// BT-3120 acceptance criterion: `git checkout` restoring older content
/// under a newer mtime must still be recompiled — mtime ordering must
/// not be trusted.
#[test]
fn test_detect_changes_branch_switch_scenario() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    let source_file = src_dir.join("counter.bt");
    write_test_file(&source_file, "counter := [0]. \"main branch\"");
    write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
    seed_beam_hash(&build_dir, &source_file);

    // `git checkout old-branch`: older content, but git always stamps
    // the checkout with a fresh (newer) mtime — content is what changed,
    // not recency.
    std::thread::sleep(std::time::Duration::from_millis(10));
    write_test_file(
        &source_file,
        "counter := [-1]. \"old branch, checked out later\"",
    );

    let source_files = vec![source_file.clone()];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert_eq!(
        result.changed_files.len(),
        1,
        "checked-out content differs from what produced the .beam — must rebuild"
    );
    assert!(result.unchanged_files.is_empty());
}

#[test]
fn test_detect_changes_force_flag() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    let source_file = src_dir.join("counter.bt");
    write_test_file(&source_file, "counter := [0].");
    write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
    // Even though the recorded hash matches (normally unchanged), --force
    // must still mark it changed.
    seed_beam_hash(&build_dir, &source_file);

    let source_files = vec![source_file];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, true, &HashMap::new());
    assert_eq!(
        result.changed_files.len(),
        1,
        "--force should mark all files as changed"
    );
    assert!(result.unchanged_files.is_empty());
}

#[test]
fn test_detect_changes_orphaned_beam() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    // Source file exists for counter but not for deleted_module
    write_test_file(&src_dir.join("counter.bt"), "counter := [0].");
    write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
    // Orphaned beam — no corresponding .bt
    write_test_file(&build_dir.join("bt@deleted_module.beam"), "BEAM");

    let source_files = vec![src_dir.join("counter.bt")];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert_eq!(
        result.orphaned_beam_files.len(),
        1,
        "Should detect orphaned .beam"
    );
    assert!(
        result.orphaned_beam_files[0]
            .as_str()
            .contains("deleted_module"),
        "Orphaned file should be the deleted_module beam"
    );
}

#[test]
fn test_detect_changes_mixed_states() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    // File 1: up-to-date (hash matches what produced the .beam)
    let stable_file = src_dir.join("stable.bt");
    write_test_file(&stable_file, "stable := [1].");
    write_test_file(&build_dir.join("bt@stable.beam"), "BEAM");
    seed_beam_hash(&build_dir, &stable_file);

    // File 2: modified after its .beam was recorded
    let changed_file = src_dir.join("changed.bt");
    write_test_file(&changed_file, "changed := [1].");
    write_test_file(&build_dir.join("bt@changed.beam"), "BEAM");
    seed_beam_hash(&build_dir, &changed_file);
    write_test_file(&changed_file, "changed := [2].");

    // File 3: new (no beam exists)
    write_test_file(&src_dir.join("new_file.bt"), "new_file := [3].");

    let source_files = vec![changed_file, src_dir.join("new_file.bt"), stable_file];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert_eq!(
        result.changed_files.len(),
        2,
        "changed + new_file should need compilation"
    );
    assert_eq!(
        result.unchanged_files.len(),
        1,
        "stable should be unchanged"
    );
}

#[test]
fn test_detect_changes_non_bt_beams_ignored() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let build_dir = project.join("build");
    fs::create_dir_all(&build_dir).unwrap();

    // Non-bt@ beam files should NOT appear as orphaned
    write_test_file(&build_dir.join("beamtalk_app.beam"), "callback");
    write_test_file(&build_dir.join("some_otp_module.beam"), "otp");

    let source_files: Vec<Utf8PathBuf> = Vec::new();
    let pairs: Vec<(Utf8PathBuf, String, Utf8PathBuf)> = Vec::new();

    let result = detect_changes(&source_files, &build_dir, &pairs, false, &HashMap::new());
    assert!(
        result.orphaned_beam_files.is_empty(),
        "Non-bt@ beam files should not be flagged as orphaned"
    );
}
