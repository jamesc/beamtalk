// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3510 (ADR 0122 Phase 1): differential test comparing the OLD,
//! top-level-only `ClassVars`/`SelfVt` loop-body detectors
//! (`CoreErlangGenerator::loop_body_threads_class_vars`/
//! `loop_body_threads_value_self`, still the live formula behind
//! `ThreadingPlan::threads_class_vars`/`threads_value_self` — this issue
//! changes no site's emission) against the NEW, recursive
//! `CoreErlangGenerator::body_threaded_families` detector, over every loop
//! and `Foldl*` body `ThreadingPlan::new_impl` builds while compiling the
//! whole `stdlib/src` + `stdlib/test` + `stdlib/bootstrap-test` corpus.
//!
//! Scope: `on:do:`/`ensure:` (BT-3506, migrated to its own top-level-only
//! `exception_construct_families` — see `exception_handling.rs`) and `match:`
//! (`match_needs_state_threading`, migrated to its own `MATCH_ARM_FAMILIES`
//! capability declaration by BT-3517) are the other two detector families ADR
//! 0122 names — this test covers the loop/
//! `Foldl*` sites `ThreadingPlan::new_impl` already builds, which is where
//! `find_class_var_mutating_stmt`/`find_value_self_mutating_stmt` (the
//! detector pair this issue's `body_threaded_families` replaces) actually
//! live.
//!
//! Per ADR 0122 §Implementation step 1: "the only expected differences are
//! nested mutations the old walks did not report, which the rejection
//! function turns into the same errors as today" — for `ClassVars`
//! specifically, `class_var_sub_expr.bt`'s `tickInLoopConditional` is the
//! one corpus construct on record (`analysis.rs`'s own
//! `find_class_var_mutating_stmt` doc comment) as deliberately, silently
//! non-threading: a same-class self-send buried in a loop's own `ifTrue:`
//! CONDITION, one level too deep for the old top-level walk, previously
//! accepted as out-of-scope.
//!
//! `bt3055actor.bt`'s `runWithSelfSend` is a second, structurally different
//! exception: `^self foo` — a same-class self-send one level inside a `^`
//! (non-local return) statement, also invisible to the old top-level-only
//! shape match. Unlike the class-var-mutation gap this ADR exists to close,
//! this one is genuinely benign rather than merely accepted: a `^` throws
//! immediately (CLAUDE.md — a non-local return never falls through to the
//! loop's own tail-recursive continuation), so whether the loop's `ClassVars`
//! carry-out is `true` or `false` for THIS statement can never be observed —
//! there is no next iteration to carry it into. `bt3055class_method_counted_loop_test.bt`
//! pins `runWithSelfSend` as compiling and returning `#FromAncestor`, so this
//! is confirmed passing, correct behavior today, not a latent bug the
//! recursive detector merely failed to also miss.
//!
//! `expected_mismatches` below is that reviewed exception list — anything
//! else the corpus produces fails the test.

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
/// see this file's own doc comment. A corpus change that removes one of
/// these (e.g. the fixture is rewritten to avoid the nested self-send) is
/// welcome; the test just needs its entry deleted here too. A corpus change
/// that ADDS a new, unreviewed mismatch fails the test until it is looked at
/// and either fixed (the nested mutation should really be rejected — see
/// `reject_class_var_field_assignment`/`reject_unthreadable_value_self_field_write`)
/// or added here as a reviewed, out-of-scope shape.
fn expected_mismatches() -> Vec<Mismatch> {
    vec![
        Mismatch {
            file: "stdlib/test/fixtures/class_var_sub_expr.bt".to_string(),
            line: 117,
            shape: "letrec",
        },
        Mismatch {
            file: "stdlib/test/fixtures/bt3055actor.bt".to_string(),
            line: 38,
            shape: "letrec",
        },
    ]
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
        let options = CodegenOptions::new(&module_name)
            .with_workspace_mode(true)
            .with_stdlib_mode(is_stdlib_src);
        // Ignore the Result — a corpus fixture that intentionally fails to
        // compile (a rejection test) still runs `ThreadingPlan::new_impl`
        // for every construct generated before the failure, and that partial
        // coverage is still worth comparing.
        let _ = generate_module(&module, options);

        let records = crate::core_erlang::control_flow::analysis::FAMILY_DETECTOR_DIFF_LOG
            .with(std::cell::RefCell::take);
        for record in records {
            total_records += 1;
            let new_class_vars = record
                .new_families
                .contains(&crate::core_erlang::threaded_ir::VersionPrefix::ClassVars);
            let new_self_vt = record
                .new_families
                .contains(&crate::core_erlang::threaded_ir::VersionPrefix::SelfVt);
            let agrees = record.old_threads_class_vars == new_class_vars
                && record.old_threads_value_self == new_self_vt;
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
         path (`reject_class_var_field_assignment`/\
         `reject_unthreadable_value_self_field_write`) still rejects it, \
         then update the exception list to match."
    );
}
