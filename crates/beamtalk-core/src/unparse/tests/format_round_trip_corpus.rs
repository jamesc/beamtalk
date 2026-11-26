// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Corpus-wide round-trip invariant for `format_source` (BT-5183).
//!
//! Formats every `.bt` file under `stdlib/`, `examples/`, `tests/` and the
//! unparser's own fixtures that parses without errors, and asserts for each:
//!
//! - `format_source` accepts it: its built-in safety net found the same
//!   comments and the same AST (modulo spans) after formatting;
//! - the comment multiset and the AST fingerprint agree with the input
//!   (checked again here, independently of `format_source`'s own gate, so
//!   that weakening the gate cannot make this test pass vacuously);
//! - formatting is idempotent.
//!
//! Files with deliberate parse errors (negative fixtures) are skipped; the
//! test asserts that most of the corpus is exercised so a broken walk cannot
//! go green by skipping everything.

use std::path::PathBuf;

use super::super::round_trip::{ast_fingerprint, comment_multiset};
use super::super::{FormatError, format_source};
use crate::file_walker::FileWalker;
use crate::source_analysis::corpus_test_support::{read_corpus_file, repo_root};
use crate::source_analysis::{lex_with_eof, parse};

/// Every `.bt` file the round-trip invariant covers.
fn round_trip_corpus() -> Vec<PathBuf> {
    let root = repo_root();
    let dirs = [
        root.join("stdlib"),
        root.join("examples"),
        root.join("tests"),
        root.join("crates/beamtalk-core/src/unparse/fixtures"),
    ];
    let walker = FileWalker::new()
        .extensions(&["bt"])
        .skip_symlinks(false)
        .strict_errors(false);
    let mut files: Vec<PathBuf> = dirs
        .iter()
        .filter(|dir| dir.is_dir())
        .flat_map(|dir| walker.walk_pathbuf(dir).unwrap_or_default())
        .collect();
    files.sort();
    files
}

#[test]
fn format_source_round_trips_every_corpus_file() {
    let files = round_trip_corpus();
    if files.is_empty() {
        // Built outside the workspace (no stdlib/examples checkout).
        return;
    }

    let mut formatted_count = 0usize;
    let mut failures: Vec<String> = Vec::new();
    for path in &files {
        let source = read_corpus_file(path);
        let shown = path.strip_prefix(repo_root()).unwrap_or(path).display();
        let pass1 = match format_source(&source) {
            Ok(out) => out,
            Err(FormatError::ParseErrors) => continue,
            Err(e) => {
                failures.push(format!("{shown}: {e}"));
                continue;
            }
        };
        formatted_count += 1;

        if comment_multiset(&source) != comment_multiset(&pass1) {
            failures.push(format!("{shown}: comment set changed"));
        }
        let (before, _) = parse(lex_with_eof(&source));
        let (after, _) = parse(lex_with_eof(&pass1));
        if ast_fingerprint(&before) != ast_fingerprint(&after) {
            failures.push(format!("{shown}: AST changed"));
        }
        match format_source(&pass1) {
            Ok(pass2) if pass2 == pass1 => {}
            Ok(_) => failures.push(format!("{shown}: formatting is not idempotent")),
            Err(e) => failures.push(format!("{shown}: second pass failed: {e}")),
        }
    }

    assert!(
        formatted_count * 2 > files.len(),
        "only {formatted_count} of {} corpus files parsed cleanly; the corpus walk is broken",
        files.len()
    );
    assert!(
        failures.is_empty(),
        "{} of {formatted_count} corpus files do not round-trip through `format_source`:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
