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

// ── Change detection tests ────────────────────

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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
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
    // Record the current content hash as "what produced this .beam" —
    // without this the sidecar has no prior hash to compare
    // against, and the file must be recompiled once regardless of mtime.
    seed_beam_hash(&build_dir, &source_file);

    let source_files = vec![source_file.clone()];
    let pairs = make_pairs(&source_files, &build_dir);

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
    assert!(
        result.changed_files.is_empty(),
        "Up-to-date file should not be changed"
    );
    assert_eq!(result.unchanged_files.len(), 1);
}

/// ADR 0127 §10a / BT-3591: a class's `uses:` line makes its file's cache
/// key depend on the protocol's content too — editing a protocol (its
/// content hash changing) must rebuild its users even when their own source
/// is completely untouched.
#[test]
fn test_detect_changes_protocol_hash_change_forces_rebuild_of_user() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    // The user's own source never changes across this test — only the
    // protocol it `uses:` does.
    let source_file = src_dir.join("greeter.bt");
    write_test_file(
        &source_file,
        "Object subclass: Greeter\n  uses: Greetable\n",
    );
    write_test_file(&build_dir.join("bt@greeter.beam"), "BEAM");

    let source_files = vec![source_file.clone()];
    let pairs = make_pairs(&source_files, &build_dir);

    let mut file_protocol_uses = HashMap::new();
    file_protocol_uses.insert(
        source_file.clone(),
        vec![ecow::EcoString::from("Greetable")],
    );

    // First build (forced, as a real `beamtalk build` would be on a clean
    // checkout): establishes the baseline combined cache key under
    // `Greetable`'s v1 hash, then persists it — mirroring
    // `build/mod.rs`'s own detect-then-`save_beam_hash_cache` sequence.
    let mut protocol_hashes_v1 = HashMap::new();
    protocol_hashes_v1.insert(ecow::EcoString::from("Greetable"), "hash-v1".to_string());
    let first = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        true,
        &HashMap::new(),
        &file_protocol_uses,
        &protocol_hashes_v1,
    );
    super::super::super::build_cache::save_beam_hash_cache(&build_dir, &first.source_hashes);

    // Second build: the user's source is byte-identical, but `Greetable`
    // now hashes differently (as if its file had just been edited) — the
    // user's combined cache key must differ, forcing a rebuild.
    let mut protocol_hashes_v2 = HashMap::new();
    protocol_hashes_v2.insert(ecow::EcoString::from("Greetable"), "hash-v2".to_string());
    let second = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &file_protocol_uses,
        &protocol_hashes_v2,
    );
    assert_eq!(
        second.changed_files,
        vec![source_file.clone()],
        "editing a used protocol must rebuild its user even though the \
         user's own source is unchanged"
    );
    assert!(second.unchanged_files.is_empty());

    // Third build: same protocol hash as the second — now up-to-date, since
    // both the source and every protocol it depends on are unchanged from
    // the last (v2) build. Guards against the combined-key mechanism
    // never converging (e.g. an unstable hash order) once a protocol's
    // hash stops changing.
    super::super::super::build_cache::save_beam_hash_cache(&build_dir, &second.source_hashes);
    let third = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &file_protocol_uses,
        &protocol_hashes_v2,
    );
    assert!(
        third.changed_files.is_empty(),
        "must settle to up-to-date once the protocol stops changing: {third:?}"
    );
    assert_eq!(third.unchanged_files, vec![source_file]);
}

/// ADR 0127 §10a / BT-3591 Blocker fix: a file's `uses:` protocol names
/// must survive a Pass 1 cache hit, not just be known for files this build
/// actually re-scanned. Before this fix, `file_protocol_uses` was derived
/// only from `cached_asts` (Pass 1's freshly-parsed ASTs), which is empty
/// for a cache-fresh file — silently breaking protocol-driven incremental
/// rebuilds after the first build (the exact scenario this test drives:
/// build once, then rebuild with an *unrelated* file changed so the
/// `uses:` file itself is skipped as fresh).
#[test]
fn test_incremental_pass1_persists_protocol_uses_for_a_cache_fresh_file() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();

    let greeter_file = src_dir.join("greeter.bt");
    write_test_file(
        &greeter_file,
        "Object subclass: Greeter\n  uses: Greetable\n",
    );
    let other_file = src_dir.join("other.bt");
    write_test_file(&other_file, "Object subclass: Other\n  m => 1\n");

    let source_files = vec![greeter_file.clone(), other_file.clone()];

    // First build: cache miss — both files scanned; greeter.bt's `uses:`
    // is recorded from its fresh parse.
    let first = super::super::super::build_cache::incremental_build_class_module_index(
        &source_files,
        Some(&src_dir),
        "pkg",
        &build_dir,
        None,
        false,
    )
    .unwrap();
    assert_eq!(
        first.file_protocol_uses.get(&greeter_file),
        Some(&vec![ecow::EcoString::from("Greetable")]),
        "first build must record greeter.bt's uses: from its fresh parse"
    );

    // Second build: only other.bt changed — greeter.bt is a cache hit
    // (fresh, not re-scanned this build). Its recorded protocol uses must
    // still be present, sourced from the persisted cache entry rather than
    // a re-parse.
    write_test_file(&other_file, "Object subclass: Other\n  m => 2\n");
    let second = super::super::super::build_cache::incremental_build_class_module_index(
        &source_files,
        Some(&src_dir),
        "pkg",
        &build_dir,
        None,
        false,
    )
    .unwrap();
    assert!(
        !second.cached_asts.contains_key(&greeter_file),
        "greeter.bt must be a cache hit (not re-scanned) for this test to be meaningful"
    );
    assert_eq!(
        second.file_protocol_uses.get(&greeter_file),
        Some(&vec![ecow::EcoString::from("Greetable")]),
        "a cache-fresh file's uses: must survive from the persisted Pass 1 \
         cache entry, not just files re-scanned this build"
    );
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
    assert_eq!(
        result.changed_files.len(),
        1,
        "Modified source should be changed"
    );
    assert!(result.unchanged_files.is_empty());
}

/// A source rewritten with identical content but a backdated (or
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
    assert!(
        result.changed_files.is_empty(),
        "touch-without-change must not trigger recompilation"
    );
    assert_eq!(result.unchanged_files.len(), 1);
}

/// `known_hashes` (Pass 1's already-computed content hashes) must
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &known_hashes,
        &HashMap::new(),
        &HashMap::new(),
    );
    assert_eq!(
        result.changed_files,
        vec![source_file],
        "a precomputed hash from known_hashes must be trusted, not silently discarded"
    );
    assert!(result.unchanged_files.is_empty());
}

/// Acceptance criterion: `git checkout` restoring older content
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        true,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
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

    let result = detect_changes(
        &source_files,
        &build_dir,
        &pairs,
        false,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
    );
    assert!(
        result.orphaned_beam_files.is_empty(),
        "Non-bt@ beam files should not be flagged as orphaned"
    );
}
