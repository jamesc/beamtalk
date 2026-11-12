// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for the `orchestrate` side of the stdlib build.
//!
//! **DDD Context:** CLI / Build System

use super::super::orchestrate::*;
use super::{set_mtime, temp_utf8_dir};
use camino::{Utf8Path, Utf8PathBuf};
use std::fs;
use std::time::Duration;
use std::time::SystemTime;
use tempfile::TempDir;

#[test]
fn test_find_stdlib_files() {
    let (_temp, lib_dir) = temp_utf8_dir();

    fs::write(lib_dir.join("integer.bt"), "// stub").unwrap();
    fs::write(lib_dir.join("string.bt"), "// stub").unwrap();
    fs::write(lib_dir.join("README.md"), "not a bt file").unwrap();

    let files = find_stdlib_files(&lib_dir).unwrap();
    assert_eq!(files.len(), 2);
    assert!(files.iter().any(|f| f.file_name() == Some("integer.bt")));
    assert!(files.iter().any(|f| f.file_name() == Some("string.bt")));
}

#[test]
fn test_find_stdlib_files_sorted() {
    let (_temp, lib_dir) = temp_utf8_dir();

    fs::write(lib_dir.join("Zebra.bt"), "// stub").unwrap();
    fs::write(lib_dir.join("Alpha.bt"), "// stub").unwrap();

    let files = find_stdlib_files(&lib_dir).unwrap();
    assert_eq!(files.len(), 2);
    assert!(files[0].as_str() < files[1].as_str());
}

#[test]
fn test_find_stdlib_files_empty_dir() {
    let (_temp, lib_dir) = temp_utf8_dir();

    let files = find_stdlib_files(&lib_dir).unwrap();
    assert!(files.is_empty());
}

#[test]
fn test_module_name_from_path() {
    // ADR 0016: All stdlib modules use bt@stdlib@ prefix
    let path = Utf8PathBuf::from("lib/integer.bt");
    assert_eq!(module_name_from_path(&path).unwrap(), "bt@stdlib@integer");
}

#[test]
fn test_module_name_from_path_non_primitive() {
    let path = Utf8PathBuf::from("lib/beamtalk.bt");
    assert_eq!(module_name_from_path(&path).unwrap(), "bt@stdlib@beamtalk");
}

#[test]
fn test_module_name_from_path_multi_word() {
    let path = Utf8PathBuf::from("lib/proto_object.bt");
    assert_eq!(
        module_name_from_path(&path).unwrap(),
        "bt@stdlib@proto_object"
    );
}

#[test]
fn test_module_name_from_path_invalid() {
    let path = Utf8PathBuf::from("lib/my-module.bt");
    assert!(module_name_from_path(&path).is_err());
}

#[test]
fn test_clean_ebin_dir() {
    let (_temp, ebin_dir) = temp_utf8_dir();

    fs::write(ebin_dir.join("beamtalk_integer.beam"), "fake").unwrap();
    fs::write(ebin_dir.join("beamtalk_stdlib.app"), "fake").unwrap();
    fs::write(ebin_dir.join("keep_me.txt"), "keep").unwrap();

    clean_ebin_dir(&ebin_dir).unwrap();

    assert!(!ebin_dir.join("beamtalk_integer.beam").exists());
    assert!(!ebin_dir.join("beamtalk_stdlib.app").exists());
    assert!(ebin_dir.join("keep_me.txt").exists());
}

// --- stdlib/src/ subdirectory support ---

#[test]
fn find_stdlib_files_recurses_into_subdirectories() {
    let (_temp, dir) = temp_utf8_dir();
    fs::create_dir_all(dir.join("collections")).unwrap();
    fs::create_dir_all(dir.join("actors/supervision")).unwrap();
    fs::write(dir.join("object.bt"), "Object subclass: Object\n").unwrap();
    fs::write(dir.join("collections/array.bt"), "Object subclass: Array\n").unwrap();
    fs::write(
        dir.join("actors/supervision/supervisor.bt"),
        "Object subclass: Supervisor\n",
    )
    .unwrap();

    let files = find_stdlib_files(&dir).unwrap();

    let mut stems: Vec<&str> = files.iter().filter_map(|f| f.file_stem()).collect();
    stems.sort_unstable();
    assert_eq!(
        stems,
        vec!["array", "object", "supervisor"],
        "Nested classes must be found at any depth. Got: {files:?}"
    );
}

#[test]
fn module_name_ignores_subdirectory() {
    // Subdirectories are editorial only — `bt@stdlib@array` regardless of
    // where the file sits. Deliberately unlike user packages, where
    // `src/util/math.bt` becomes `util@math`.
    let flat = module_name_from_path(Utf8Path::new("stdlib/src/array.bt")).unwrap();
    let nested = module_name_from_path(Utf8Path::new("stdlib/src/collections/array.bt")).unwrap();

    assert_eq!(flat, "bt@stdlib@array");
    assert_eq!(nested, flat, "Subdirectory must not change the module name");
}

#[test]
fn check_duplicate_module_names_accepts_unique_names_across_subdirectories() {
    let files = vec![
        Utf8PathBuf::from("stdlib/src/object.bt"),
        Utf8PathBuf::from("stdlib/src/collections/array.bt"),
        Utf8PathBuf::from("stdlib/src/numeric/integer.bt"),
    ];
    assert!(check_duplicate_module_names(&files).is_ok());
}

#[test]
fn check_duplicate_module_names_rejects_same_stem_in_two_subdirectories() {
    // Both compile to `bt@stdlib@array`, silently clobbering in ebin/.
    let files = vec![
        Utf8PathBuf::from("stdlib/src/collections/array.bt"),
        Utf8PathBuf::from("stdlib/src/legacy/array.bt"),
    ];

    let err = check_duplicate_module_names(&files)
        .unwrap_err()
        .to_string();
    assert!(
        err.contains("stdlib/src/collections/array.bt")
            && err.contains("stdlib/src/legacy/array.bt"),
        "Error must name both colliding paths in full, so the user knows \
         which two files to rename. Got: {err}"
    );
    assert!(
        err.contains("bt@stdlib@array"),
        "Error should name the module they collide on. Got: {err}"
    );
}

#[test]
fn check_duplicate_module_names_catches_case_folded_collision() {
    // `to_module_name` lowercases, so these distinct filenames both yield
    // `bt@stdlib@beamerror`. A raw-stem comparison would miss it — and
    // `beamerror.bt` is a real stdlib class, so the shape is not academic.
    let files = vec![
        Utf8PathBuf::from("stdlib/src/beamerror.bt"),
        Utf8PathBuf::from("stdlib/src/errors/Beamerror.bt"),
    ];

    let err = check_duplicate_module_names(&files)
        .unwrap_err()
        .to_string();
    assert!(
        err.contains("bt@stdlib@beamerror"),
        "Case-folded collision must be rejected. Got: {err}"
    );
}

#[test]
fn test_is_stdlib_up_to_date_missing_ebin_dir() {
    let (_temp, base) = temp_utf8_dir();
    let ebin = base.join("does_not_exist");
    assert!(!is_stdlib_up_to_date(&ebin, &[], Some(&[])));
}

#[test]
fn test_is_stdlib_up_to_date_no_beam_files() {
    let (_temp, ebin) = temp_utf8_dir();
    fs::write(ebin.join("readme.txt"), "not a beam file").unwrap();
    assert!(!is_stdlib_up_to_date(&ebin, &[], Some(&[])));
}

#[test]
fn test_is_stdlib_up_to_date_beam_source_count_mismatch() {
    let (_temp, ebin) = temp_utf8_dir();
    fs::write(ebin.join("bt@stdlib@a.beam"), "").unwrap();

    let (_temp2, src_dir) = temp_utf8_dir();
    let src1 = src_dir.join("A.bt");
    let src2 = src_dir.join("B.bt");
    fs::write(&src1, "").unwrap();
    fs::write(&src2, "").unwrap();

    // One .beam but two sources: a renamed/removed source must force a
    // rebuild rather than silently leaving a stale .beam in place.
    assert!(!is_stdlib_up_to_date(&ebin, &[src1, src2], Some(&[])));
}

#[test]
fn test_is_stdlib_up_to_date_source_newer_than_output() {
    let (_temp, ebin) = temp_utf8_dir();
    let beam = ebin.join("bt@stdlib@a.beam");
    fs::write(ebin.join(STDLIB_APP_FILE), "").unwrap();
    fs::write(&beam, "").unwrap();
    set_mtime(
        &beam,
        SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000_000),
    );

    let (_temp2, src_dir) = temp_utf8_dir();
    let src = src_dir.join("A.bt");
    fs::write(&src, "").unwrap(); // freshly written: newer than the beam output above

    assert!(!is_stdlib_up_to_date(&ebin, &[src], Some(&[])));
}

#[test]
fn test_is_stdlib_up_to_date_missing_source_file_metadata() {
    let (_temp, ebin) = temp_utf8_dir();
    fs::write(ebin.join("bt@stdlib@gone.beam"), "").unwrap();
    fs::write(ebin.join(STDLIB_APP_FILE), "").unwrap();

    let (_temp2, src_dir) = temp_utf8_dir();
    let gone = src_dir.join("Gone.bt"); // listed but never created

    assert!(!is_stdlib_up_to_date(&ebin, &[gone], Some(&[])));
}

#[test]
fn test_is_stdlib_up_to_date_compiler_binary_newer_forces_rebuild() {
    let (_temp, ebin) = temp_utf8_dir();
    let beam = ebin.join("bt@stdlib@a.beam");
    fs::write(ebin.join(STDLIB_APP_FILE), "").unwrap();
    fs::write(&beam, "").unwrap();
    // Far enough in the past (1970) that the real test binary — built
    // moments before this test ran — is deterministically newer.
    let ancient = SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000);
    set_mtime(&beam, ancient);

    let (_temp2, src_dir) = temp_utf8_dir();
    let src = src_dir.join("A.bt");
    fs::write(&src, "").unwrap();
    set_mtime(&src, ancient); // same age as the beam output: not "source newer"

    assert!(!is_stdlib_up_to_date(&ebin, &[src], Some(&[])));
}

/// Shared setup for the `runtime_ebin_dirs`-branch tests below: a
/// same-age beam/source pair far enough in the future that the real
/// compiler binary running this test is never "newer than output" —
/// isolating each test to just the runtime-ebin-dirs check that follows
/// it.
fn up_to_date_beam_and_source() -> (TempDir, Utf8PathBuf, TempDir, Utf8PathBuf, SystemTime) {
    let (ebin_temp, ebin) = temp_utf8_dir();
    let beam = ebin.join("bt@stdlib@a.beam");
    fs::write(&beam, "").unwrap();
    fs::write(ebin.join(STDLIB_APP_FILE), "").unwrap();
    let output_time = SystemTime::UNIX_EPOCH + Duration::from_secs(4_000_000_000);
    set_mtime(&beam, output_time);

    let (src_temp, src_dir) = temp_utf8_dir();
    let src = src_dir.join("A.bt");
    fs::write(&src, "").unwrap();
    set_mtime(&src, output_time - Duration::from_secs(1));

    (ebin_temp, ebin, src_temp, src, output_time)
}

#[test]
fn test_is_stdlib_up_to_date_runtime_beam_newer_forces_rebuild() {
    let (_ebin_temp, ebin, _src_temp, src, output_time) = up_to_date_beam_and_source();

    let (_temp3, runtime_ebin) = temp_utf8_dir();
    let runtime_beam = runtime_ebin.join("beamtalk_runtime.beam");
    fs::write(&runtime_beam, "").unwrap();
    set_mtime(&runtime_beam, output_time + Duration::from_secs(1));

    assert!(!is_stdlib_up_to_date(
        &ebin,
        &[src],
        Some(&[runtime_ebin.into_std_path_buf()]),
    ));
}

#[test]
fn test_is_stdlib_up_to_date_runtime_beam_older_no_rebuild() {
    let (_ebin_temp, ebin, _src_temp, src, output_time) = up_to_date_beam_and_source();

    let (_temp3, runtime_ebin) = temp_utf8_dir();
    let runtime_beam = runtime_ebin.join("beamtalk_runtime.beam");
    fs::write(&runtime_beam, "").unwrap();
    set_mtime(&runtime_beam, output_time - Duration::from_secs(1));

    assert!(is_stdlib_up_to_date(
        &ebin,
        &[src],
        Some(&[runtime_ebin.into_std_path_buf()]),
    ));
}

#[test]
fn test_is_stdlib_up_to_date_runtime_dirs_undiscoverable_forces_rebuild() {
    let (_ebin_temp, ebin, _src_temp, src, _output_time) = up_to_date_beam_and_source();

    // `None` stands in for `discover_runtime_ebin_dirs()` failing to
    // locate the runtime layout at all — same "force rebuild to be
    // safe" contract as every other missing-input branch above.
    assert!(!is_stdlib_up_to_date(&ebin, &[src], None));
}

#[test]
fn test_is_stdlib_up_to_date_missing_runtime_ebin_dir_is_skipped() {
    // A runtime ebin directory that doesn't exist in this layout (e.g.
    // an app not built yet) is skipped, not treated as an error.
    let (_ebin_temp, ebin, _src_temp, src, _output_time) = up_to_date_beam_and_source();

    let (_temp3, base) = temp_utf8_dir();
    let missing_runtime_ebin = base.join("does_not_exist");

    assert!(is_stdlib_up_to_date(
        &ebin,
        &[src],
        Some(&[missing_runtime_ebin.into_std_path_buf()]),
    ));
}

#[test]
fn test_is_stdlib_up_to_date_renamed_source_forces_rebuild() {
    // BT-4384: `A.bt` renamed to `B.bt` with its mtime kept (as `mv`
    // does) — same beam count, nothing newer than the output. The old
    // `bt@stdlib@a` beam must not survive as "up to date".
    let (_ebin_temp, ebin, src_temp, old_src, _output_time) = up_to_date_beam_and_source();
    let renamed = src_temp.path().join("B.bt");
    fs::rename(old_src.as_std_path(), &renamed).unwrap();
    let renamed = Utf8PathBuf::from_path_buf(renamed).unwrap();

    assert!(!is_stdlib_up_to_date(&ebin, &[renamed], Some(&[])));
}

#[test]
fn test_is_stdlib_up_to_date_missing_app_file_forces_rebuild() {
    // The generated `.app` is what gets installed (BT-4384): beams
    // without it must not count as an up-to-date build.
    let (_ebin_temp, ebin, _src_temp, src, _output_time) = up_to_date_beam_and_source();
    fs::remove_file(ebin.join(STDLIB_APP_FILE)).unwrap();

    assert!(!is_stdlib_up_to_date(&ebin, &[src], Some(&[])));
}

#[test]
fn test_is_stdlib_up_to_date_ignores_newer_class_beam_copies() {
    // `rebar3 compile` copies this build's own `bt@*` class beams into
    // the stdlib Erlang ebin with fresh mtimes; they are not a runtime
    // (native-spec) input and must not force a rebuild.
    let (_ebin_temp, ebin, _src_temp, src, output_time) = up_to_date_beam_and_source();

    let (_temp3, rebar3_ebin) = temp_utf8_dir();
    let copy = rebar3_ebin.join("bt@stdlib@a.beam");
    fs::write(&copy, "").unwrap();
    set_mtime(&copy, output_time + Duration::from_secs(1));

    assert!(is_stdlib_up_to_date(
        &ebin,
        &[src],
        Some(&[rebar3_ebin.into_std_path_buf()]),
    ));
}

#[test]
fn test_sync_rebar3_stdlib_ebin_replaces_app_and_drops_class_beams() {
    let (_temp, ebin) = temp_utf8_dir();
    fs::write(ebin.join(STDLIB_APP_FILE), "current").unwrap();
    fs::write(ebin.join("bt@stdlib@a.beam"), "current").unwrap();

    let (_temp2, rebar3_ebin) = temp_utf8_dir();
    fs::write(rebar3_ebin.join(STDLIB_APP_FILE), "stale").unwrap();
    fs::write(rebar3_ebin.join("bt@stdlib@a.beam"), "stale").unwrap();
    fs::write(rebar3_ebin.join("bt@stdlib@removed.beam"), "stale").unwrap();
    fs::write(rebar3_ebin.join("beamtalk_list.beam"), "erlang").unwrap();

    sync_rebar3_stdlib_ebin(&ebin, rebar3_ebin.as_std_path()).unwrap();

    assert_eq!(
        fs::read_to_string(rebar3_ebin.join(STDLIB_APP_FILE)).unwrap(),
        "current"
    );
    assert!(!rebar3_ebin.join("bt@stdlib@a.beam").exists());
    assert!(!rebar3_ebin.join("bt@stdlib@removed.beam").exists());
    assert!(rebar3_ebin.join("beamtalk_list.beam").exists());
    // The build's own output is untouched.
    assert!(ebin.join("bt@stdlib@a.beam").exists());
}

#[test]
fn test_sync_rebar3_stdlib_ebin_without_rebar3_dir_is_noop() {
    let (_temp, ebin) = temp_utf8_dir();
    fs::write(ebin.join(STDLIB_APP_FILE), "current").unwrap();
    let missing = ebin.join("no_build_dir");

    sync_rebar3_stdlib_ebin(&ebin, missing.as_std_path()).unwrap();

    assert!(!missing.exists());
}

#[test]
fn test_stdlib_erlang_modules_lists_erl_stems_only() {
    let (_temp, src_dir) = temp_utf8_dir();
    fs::write(src_dir.join("beamtalk_list.erl"), "").unwrap();
    fs::write(src_dir.join("beamtalk_set.erl"), "").unwrap();
    fs::write(src_dir.join("beamtalk_stdlib.app.src"), "").unwrap();
    fs::write(src_dir.join("beamtalk_internal.hrl"), "").unwrap();

    let mut modules = stdlib_erlang_modules(&src_dir).unwrap();
    modules.sort();
    assert_eq!(modules, vec!["beamtalk_list", "beamtalk_set"]);
    assert!(
        stdlib_erlang_modules(&src_dir.join("missing"))
            .unwrap()
            .is_empty()
    );
}

#[test]
fn test_oldest_mtime_in_dir_returns_none_when_empty() {
    let (_temp, dir) = temp_utf8_dir();
    assert!(oldest_mtime_in_dir(&dir, "beam").is_none());
}

#[test]
fn test_oldest_mtime_in_dir_ignores_other_extensions() {
    let (_temp, dir) = temp_utf8_dir();
    let unrelated = dir.join("skip.txt");
    fs::write(&unrelated, "").unwrap();
    assert!(oldest_mtime_in_dir(&dir, "beam").is_none());
}

#[test]
fn test_oldest_mtime_in_dir_picks_oldest_matching_extension() {
    let (_temp, dir) = temp_utf8_dir();
    let old = dir.join("old.beam");
    let newer = dir.join("newer.beam");
    let wrong_ext = dir.join("oldest_but_wrong_ext.txt");
    fs::write(&old, "").unwrap();
    fs::write(&newer, "").unwrap();
    fs::write(&wrong_ext, "").unwrap();

    let old_time = SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000);
    let newer_time = SystemTime::UNIX_EPOCH + Duration::from_secs(2_000_000);
    set_mtime(&old, old_time);
    set_mtime(&newer, newer_time);
    // Older than either .beam file, but the wrong extension — must be ignored.
    set_mtime(&wrong_ext, SystemTime::UNIX_EPOCH);

    assert_eq!(oldest_mtime_in_dir(&dir, "beam"), Some(old_time));
}
