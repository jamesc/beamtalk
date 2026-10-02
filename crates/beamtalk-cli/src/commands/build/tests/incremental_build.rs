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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges {
            file_protocol_uses: file_protocol_uses.clone(),
            protocol_hashes: protocol_hashes_v1.clone(),
            ..BuildGraphEdges::default()
        },
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
        &BuildGraphEdges {
            file_protocol_uses: file_protocol_uses.clone(),
            protocol_hashes: protocol_hashes_v2.clone(),
            ..BuildGraphEdges::default()
        },
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
        &BuildGraphEdges {
            file_protocol_uses: file_protocol_uses.clone(),
            protocol_hashes: protocol_hashes_v2.clone(),
            ..BuildGraphEdges::default()
        },
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

/// BT-3684: a package-qualified `uses: pkg_b@Retryable` is keyed by `pkg_b`'s
/// protocol: editing it rebuilds the user, editing an unrelated dependency's
/// same-named protocol does not — a bare `uses: Retryable` follows the first
/// definition, and a qualifier naming the *current* package (`uses: my_app@Greetable`,
/// whose project protocol carries no package stamp) follows the project's own
/// protocol, all as `trait_expansion` resolves them.
#[test]
fn test_detect_changes_qualified_uses_follows_the_named_packages_protocol() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src_dir = project.join("src");
    let build_dir = project.join("build");
    fs::create_dir_all(&src_dir).unwrap();
    fs::create_dir_all(&build_dir).unwrap();
    let qualified = src_dir.join("qualified.bt");
    write_test_file(
        &qualified,
        "Object subclass: Qualified\n  uses: pkg_b@Retryable\n",
    );
    let bare = src_dir.join("bare.bt");
    write_test_file(&bare, "Object subclass: Bare\n  uses: Retryable\n");
    let own = src_dir.join("own.bt");
    write_test_file(&own, "Object subclass: Own\n  uses: my_app@Greetable\n");
    write_test_file(&build_dir.join("bt@qualified.beam"), "BEAM");
    write_test_file(&build_dir.join("bt@bare.beam"), "BEAM");
    write_test_file(&build_dir.join("bt@own.beam"), "BEAM");
    let source_files = vec![qualified.clone(), bare.clone(), own.clone()];
    let pairs = make_pairs(&source_files, &build_dir);

    // The `uses:` keys come from Pass 1, exactly as a build records them.
    let pass1 = super::super::super::build_cache::incremental_build_class_module_index(
        &source_files,
        Some(&src_dir),
        "pkg",
        &build_dir,
        None,
        false,
    )
    .unwrap();
    assert_eq!(
        pass1.file_protocol_uses.get(&qualified),
        Some(&vec![ecow::EcoString::from("pkg_b@Retryable")])
    );
    assert_eq!(
        pass1.file_protocol_uses.get(&bare),
        Some(&vec![ecow::EcoString::from("Retryable")])
    );
    assert_eq!(
        pass1.file_protocol_uses.get(&own),
        Some(&vec![ecow::EcoString::from("my_app@Greetable")])
    );
    let protocol = |package: Option<&str>, name: &str, selector: &str, body: &str| {
        let source = format!(
            "Protocol define: {name}\n  name -> String\n\n  {selector} -> String => {body}\n"
        );
        let (module, _) = beamtalk_core::source_analysis::parse(
            beamtalk_core::source_analysis::lex_with_eof(&source),
        );
        let mut def = module.protocols[0].clone();
        def.package = package.map(Into::into);
        def
    };
    let build = |force: bool, a_selector: &str, b_selector: &str, greet_body: &str| {
        // The project's own protocol is unstamped; dependencies' are stamped.
        let defs = [
            protocol(None, "Greetable", "greet", greet_body),
            protocol(Some("pkg_a"), "Retryable", a_selector, "self name"),
            protocol(Some("pkg_b"), "Retryable", b_selector, "self name"),
        ];
        let hashes = crate::commands::util::protocol_hashes(
            &defs,
            pass1.file_protocol_uses.values().flatten(),
            Some("my_app"),
        );
        let changes = detect_changes(
            &source_files,
            &build_dir,
            &pairs,
            force,
            &HashMap::new(),
            &BuildGraphEdges {
                file_protocol_uses: pass1.file_protocol_uses.clone(),
                protocol_hashes: hashes,
                ..BuildGraphEdges::default()
            },
        );
        super::super::super::build_cache::save_beam_hash_cache(&build_dir, &changes.source_hashes);
        changes
    };

    build(true, "aTag", "bTag", "\"hi\"");

    let unrelated_edit = build(false, "aTag2", "bTag", "\"hi\"");
    assert_eq!(
        unrelated_edit.changed_files,
        vec![bare.clone()],
        "editing pkg_a's Retryable rebuilds only the bare user (first definition), \
         not the file that names pkg_b's"
    );

    let named_edit = build(false, "aTag2", "bTag2", "\"hi\"");
    assert_eq!(
        named_edit.changed_files,
        vec![qualified],
        "editing pkg_b's Retryable must rebuild the file that names it"
    );

    // Only the body of the project's own protocol changes: selectors and types,
    // hence `trait_surface_hash`, stay put, so the self-qualified user is
    // rebuilt through its protocol hash alone.
    let own_edit = build(false, "aTag2", "bTag2", "\"hello\"");
    assert_eq!(
        own_edit.changed_files,
        vec![own],
        "editing the project's own protocol must rebuild a file that names it as `my_app@Greetable`"
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
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
        &BuildGraphEdges::default(),
    );
    assert!(
        result.orphaned_beam_files.is_empty(),
        "Non-bt@ beam files should not be flagged as orphaned"
    );
}

/// BT-3674: runs Pass 1 + `detect_changes` exactly as `execute_build_passes`
/// does, then records the hashes (and dummy `.beam`s) as a real successful
/// build would. Returns the (changed, unchanged) file names, sorted.
fn detect_and_record(project: &Utf8Path) -> (Vec<String>, Vec<String>) {
    let env = setup_build_environment(project.as_str()).unwrap();
    let dep_ctx = DependencyContext {
        resolved_deps: Vec::new(),
        has_native_deps: false,
    };
    let index = build_class_index(&env, &dep_ctx, &default_options(), false).unwrap();
    let protocol_hashes = crate::commands::util::protocol_hashes(
        &index.all_protocol_defs,
        index.file_protocol_uses.values().flatten(),
        None,
    );
    let pairs = compute_file_module_pairs(&env).unwrap();
    fs::create_dir_all(&env.build_dir).unwrap();
    let changes = detect_changes(
        &env.source_files,
        &env.build_dir,
        &pairs,
        false,
        &index.source_hashes,
        &BuildGraphEdges {
            file_protocol_uses: index.file_protocol_uses.clone(),
            protocol_hashes,
            trait_surface_hash: index.trait_surface_hash.clone(),
        },
    );
    for (_, module, _) in &pairs {
        write_test_file(&env.build_dir.join(format!("{module}.beam")), "BEAM");
    }
    super::super::super::build_cache::save_beam_hash_cache(&env.build_dir, &changes.source_hashes);
    let names = |files: &[Utf8PathBuf]| {
        let mut v: Vec<String> = files
            .iter()
            .map(|f| f.file_name().unwrap().to_string())
            .collect();
        v.sort();
        v
    };
    (
        names(&changes.changed_files),
        names(&changes.unchanged_files),
    )
}

/// BT-3674: an unchanged `caller.bt` that calls a trait-provided method must
/// be rebuilt (and so re-type-checked) when a provision is renamed in the
/// trait's file, even though neither it nor anything it names changed. A
/// trait edit that leaves the flattened surface equal (a body-only change)
/// must not rebuild it.
#[test]
fn test_incremental_rebuilds_unchanged_caller_when_provision_renamed() {
    let temp = TempDir::new().unwrap();
    let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let src = project.join("src");
    fs::create_dir_all(&src).unwrap();
    write_test_file(
        &project.join("beamtalk.toml"),
        "[package]\nname = \"test_pkg\"\nversion = \"0.1.0\"\n",
    );
    let trait_src = |provision: &str, body: &str| {
        format!("Protocol define: Tagged\n  name -> String\n\n  {provision} -> String => {body}\n")
    };
    write_test_file(&src.join("tagged.bt"), &trait_src("tag", "self name"));
    write_test_file(
        &src.join("widget.bt"),
        "Object subclass: Widget\n  uses: Tagged\n  name -> String => \"w\"\n",
    );
    write_test_file(
        &src.join("caller.bt"),
        "Object subclass: Caller\n  describe: w :: Widget -> String => w tag\n",
    );

    let (changed, _) = detect_and_record(&project);
    assert_eq!(changed, ["caller.bt", "tagged.bt", "widget.bt"]);
    let (changed, _) = detect_and_record(&project);
    assert!(
        changed.is_empty(),
        "no-op rebuild must be clean: {changed:?}"
    );

    // Body-only trait edit: the flattened surface is equal.
    write_test_file(&src.join("tagged.bt"), &trait_src("tag", "\"fixed\""));
    let (changed, unchanged) = detect_and_record(&project);
    assert!(changed.contains(&"widget.bt".to_string()), "{changed:?}");
    assert!(
        unchanged.contains(&"caller.bt".to_string()),
        "a body-only trait edit must not rebuild the caller: {changed:?}"
    );

    // Rename the provision: caller.bt is byte-identical but now calls a
    // method that no longer exists.
    write_test_file(&src.join("tagged.bt"), &trait_src("label", "\"fixed\""));
    let (changed, _) = detect_and_record(&project);
    assert!(
        changed.contains(&"caller.bt".to_string()),
        "renaming a provision must rebuild the unchanged caller: {changed:?}"
    );
    let (changed, _) = detect_and_record(&project);
    assert!(
        changed.is_empty(),
        "must settle once surfaces stop changing: {changed:?}"
    );
}
