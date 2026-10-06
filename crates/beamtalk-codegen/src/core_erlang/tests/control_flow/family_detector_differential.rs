// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3510 (ADR 0122 Phase 1): differential test comparing the OLD,
//! top-level-only `SelfVt` loop-body detector
//! (`CoreErlangGenerator::loop_body_threads_value_self`, still the live
//! formula behind `ThreadingPlan::threads_value_self`) against the NEW,
//! recursive `CoreErlangGenerator::body_threaded_families` detector, over
//! every loop and `Foldl*` body `ThreadingPlan::new_impl` builds while
//! compiling the whole `stdlib/src` + `stdlib/test` + `stdlib/bootstrap-test`
//! corpus.
//!
//! Scope: `on:do:`/`ensure:` and `match:` are the other two detector families
//! ADR 0122 names, and both have since finished their own migrations —
//! `exception_construct_families` now CALLS `body_threaded_families` directly
//! (BT-3522), and `match_needs_state_threading` moved to its own
//! `MATCH_ARM_FAMILIES` capability declaration (BT-3517). Neither needs
//! differential coverage here any more: one has no second detector left to
//! differ from, the other has no body walk at all. This test covers the
//! loop/`Foldl*` sites `ThreadingPlan::new_impl` already builds.
//!
//! Class variables are not a threaded family any more (ADR 0130 §3): they
//! are written in place in the class process, so a class method's loop and
//! fold bodies thread no family, and the `ClassVars` half of this harness —
//! with the reviewed exception list it used to carry (a class-variable
//! mutation nested in a construct the loop could not carry out) — was
//! deleted with the family.
//!
//! `expected_mismatches` below is the reviewed exception list for the
//! remaining (`SelfVt`) family — anything the corpus produces that is not on
//! it fails the test.

use super::*;

/// One instance of the old/new detectors disagreeing on a real corpus
/// construct — human-reviewable via `file:line (shape)`.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Mismatch {
    file: String,
    line: u32,
    shape: &'static str,
}

impl std::fmt::Display for Mismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{} ({})", self.file, self.line, self.shape)
    }
}

/// The reviewed, expected set of old/new mismatches over the whole corpus —
/// see this file's own doc comment.
///
/// Empty. A corpus change that adds a mismatch for the `SelfVt` family fails
/// the test until it is looked at (ADR 0130 §3 deleted the `ClassVars` half,
/// whose entries only existed because a class variable was a threaded
/// family).
fn expected_mismatches() -> Vec<Mismatch> {
    Vec::new()
}

/// Recursively collects every `.bt`/`.btscript` file under `dir`.
fn collect_source_files(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut entries: Vec<std::fs::DirEntry> = entries.filter_map(std::result::Result::ok).collect();
    entries.sort_by_key(std::fs::DirEntry::path);
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_source_files(&path, out);
        } else if matches!(
            path.extension().and_then(std::ffi::OsStr::to_str),
            Some("bt" | "btscript")
        ) {
            out.push(path);
        }
    }
}

/// Repo root, derived from this crate's own manifest directory rather than
/// the test process's current directory (which `cargo test` does not
/// guarantee is the workspace root).
fn repo_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("beamtalk-codegen must be two directories below the repo root")
}

/// The 1-based source line containing byte offset `at`.
#[allow(
    clippy::naive_bytecount,
    reason = "one-off test diagnostic over a single small file — not worth a bytecount crate dependency"
)]
fn line_at(source: &str, at: u32) -> u32 {
    let at = (at as usize).min(source.len());
    let newlines = source.as_bytes()[..at]
        .iter()
        .filter(|&&b| b == b'\n')
        .count();
    1 + u32::try_from(newlines).unwrap_or(u32::MAX)
}

#[test]
fn family_detector_agrees_with_old_loop_detectors_over_the_corpus() {
    let root = repo_root();
    let mut files = Vec::new();
    for corpus_dir in ["stdlib/src", "stdlib/test", "stdlib/bootstrap-test"] {
        collect_source_files(&root.join(corpus_dir), &mut files);
    }
    assert!(
        files.len() > 100,
        "expected hundreds of corpus files under stdlib/{{src,test,bootstrap-test}}, \
         found {} — repo_root() likely resolved wrong: {}",
        files.len(),
        root.display()
    );

    let mut mismatches: Vec<Mismatch> = Vec::new();
    let mut total_records = 0usize;

    for (i, path) in files.iter().enumerate() {
        let Ok(source) = std::fs::read_to_string(path) else {
            continue;
        };
        let rel = path
            .strip_prefix(&root)
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/");

        crate::core_erlang::control_flow::analysis::FAMILY_DETECTOR_DIFF_LOG
            .with(|log| log.borrow_mut().clear());

        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
        let is_stdlib_src = rel.starts_with("stdlib/src/");
        let module_name = format!("bt@family_diff@{i}");
        let options = CodegenOptions::new(&module_name).with_stdlib_mode(is_stdlib_src);
        // Ignore the Result — a corpus fixture that intentionally fails to
        // compile (a rejection test) still runs `ThreadingPlan::new_impl`
        // for every construct generated before the failure, and that partial
        // coverage is still worth comparing.
        let _ = generate_module(&module, options);

        let records = crate::core_erlang::control_flow::analysis::FAMILY_DETECTOR_DIFF_LOG
            .with(std::cell::RefCell::take);
        for record in records {
            total_records += 1;
            let new_self_vt = record
                .new_families
                .contains(&crate::core_erlang::threaded_ir::VersionPrefix::SelfVt);
            let agrees = record.old_threads_value_self == new_self_vt;
            if !agrees {
                mismatches.push(Mismatch {
                    file: rel.clone(),
                    line: line_at(&source, record.span.start()),
                    shape: record.shape,
                });
            }
        }
    }

    assert!(
        total_records > 50,
        "expected the corpus to exercise well over 50 loop/fold `ThreadingPlan`s, \
         only saw {total_records} — is the differential recording hook still wired \
         into `ThreadingPlan::new_impl`?"
    );

    mismatches.sort_by(|a, b| (a.file.as_str(), a.line).cmp(&(b.file.as_str(), b.line)));
    mismatches.dedup();

    println!(
        "family_detector_differential: {total_records} records, {} old/new mismatches:",
        mismatches.len()
    );
    for m in &mismatches {
        println!("  {m}");
    }

    let mut expected = expected_mismatches();
    expected.sort_by(|a, b| (a.file.as_str(), a.line).cmp(&(b.file.as_str(), b.line)));

    assert_eq!(
        mismatches, expected,
        "the old (top-level-only) and new (recursive) storage-family \
         detectors disagree on a corpus construct that isn't in this file's \
         reviewed `expected_mismatches()` list (or a previously-reviewed \
         mismatch is now missing). Every disagreement must be a nested \
         mutation the loop genuinely cannot carry — confirm the rejection \
         path (`reject_unthreadable_value_self_field_write`) still rejects it, \
         then update the exception list to match."
    );
}
