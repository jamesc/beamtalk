// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Corpus-wide validation for `// === Name ===` method-category dividers
//! (BT-2601's recognizer, exercised here at BT-2626's stdlib-wide curation
//! scale).
//!
//! [`method_category`](super::method_category) documents two silent-failure
//! modes that a hand-written (or AI-curated) divider can fall into with no
//! diagnostic anywhere in the pipeline:
//!
//! 1. **A near-miss divider is not diagnosed.** `=== Name ====` (mismatched
//!    run lengths), `== Name ==` (too short), or a `///`/`/* */` divider-
//!    shaped comment all silently fail to parse and fall back to an ordinary
//!    leading comment — the methods below are silently absorbed into
//!    whichever category was already open. `fmt-check-beamtalk` and the
//!    byte-span round-trip test ([`super::method_span_corpus_tests`]) both
//!    treat comments as opaque trivia, so neither one would catch this.
//! 2. **A divider with nothing but non-method declarations after it** would
//!    register as a named category with zero methods — a heading with
//!    nothing under it.
//!
//! This suite proves neither happens anywhere in the corpus, checked against
//! the real recognizer ([`parse_divider_name`], [`categorize_methods`]) —
//! not a reimplementation of the rule — so it can't silently drift from what
//! the LSP outline/cockpit/REPL actually do. It complements (does not
//! replace) BT-3240's proposed editor-time lint: this is the regression gate
//! that keeps the corpus itself honest.

use crate::source_analysis::corpus_test_support::{corpus_files, corpus_present, read_corpus_file};
use crate::source_analysis::method_category::{categorize_methods, parse_divider_name};
use crate::source_analysis::{lex_with_eof, parse};

/// Returns the divider-candidate text (the comment body after `//`) if `line`
/// is shaped like someone attempting a `// === Name ===` divider: a plain
/// `//` line comment (never `///`/`//!`) whose trimmed body has a non-empty
/// run of `=` at both the start and the end, with *some* non-`=` content
/// strictly between them.
///
/// That last clause is what excludes the older, unrelated pure-border banner
/// line (`// ===...===`, all `=`, no name) some example files use as part of
/// a 3-line `border`/`HEADING`/`border` convention — [`parse_divider_name`]
/// deliberately doesn't recognize that shape either, so it must not count as
/// a "near miss" here. Anything this function *does* flag is exactly the
/// shape a `// === Name ===` divider takes, valid or not — so every hit must
/// parse via [`parse_divider_name`], or it's a silently-broken divider.
fn divider_attempt(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    let rest = trimmed.strip_prefix("//")?;
    // Exclude `///` doc comments and `//!` module docs — never dividers, and
    // doc-comment example snippets (fenced ```beamtalk blocks showing a
    // divider) are themselves `///`-prefixed line-by-line, so this also
    // keeps embedded examples out of the raw scan for free.
    if rest.starts_with('/') || rest.starts_with('!') {
        return None;
    }
    let body = rest.trim_end_matches(['\r', '\n']);
    let content = body.trim();
    let chars: Vec<char> = content.chars().collect();
    let left_len = chars.iter().take_while(|&&c| c == '=').count();
    if left_len == 0 {
        return None;
    }
    let right_len = chars.iter().rev().take_while(|&&c| c == '=').count();
    if right_len == 0 {
        return None;
    }
    if left_len + right_len >= chars.len() {
        // Pure border line (entirely `=`, or the two runs overlap/touch) —
        // the unrelated 3-line banner convention, not a divider attempt.
        return None;
    }
    Some(body)
}

/// No divider-shaped comment anywhere in the corpus fails to parse as a
/// valid divider. A hit here means a `// === Name ===` attempt has a typo
/// (mismatched `=` run lengths, too short) that would silently vanish from
/// the LSP outline instead of erroring.
#[test]
fn corpus_has_no_near_miss_dividers() {
    if !corpus_present() {
        return;
    }
    let files = corpus_files();
    let mut near_misses: Vec<String> = Vec::new();
    let mut valid_dividers = 0usize;

    for path in &files {
        let source = read_corpus_file(path);
        for (idx, line) in source.lines().enumerate() {
            let Some(body) = divider_attempt(line) else {
                continue;
            };
            if parse_divider_name(body).is_some() {
                valid_dividers += 1;
            } else {
                near_misses.push(format!(
                    "{}:{}: divider-shaped comment does not parse as a valid \
                     divider: {:?}",
                    path.display(),
                    idx + 1,
                    line.trim()
                ));
            }
        }
    }

    assert!(
        near_misses.is_empty(),
        "found {} near-miss divider(s) in the corpus (valid `=` run on both \
         sides, but not recognized by parse_divider_name — see \
         method_category.rs's near-miss documentation):\n{}",
        near_misses.len(),
        near_misses.join("\n")
    );

    // Sanity: BT-2626 put several hundred real dividers into the stdlib. If
    // this drops to near zero, the raw scan (or the corpus walk) silently
    // broke rather than the corpus actually losing its dividers.
    assert!(
        valid_dividers > 100,
        "expected >100 valid dividers across the corpus, found \
         {valid_dividers} — the scan may have silently failed"
    );
}

/// Every named category [`categorize_methods`] produces across the corpus
/// contains at least one method. A named category with zero methods is a
/// divider that precedes only non-method declarations (or nothing) before
/// the next divider/EOF — a heading rendered with nothing under it.
#[test]
fn corpus_has_no_empty_named_categories() {
    if !corpus_present() {
        return;
    }
    let files = corpus_files();
    let mut empty: Vec<String> = Vec::new();
    let mut named_categories = 0usize;

    for path in &files {
        let source = read_corpus_file(path);
        let tokens = lex_with_eof(&source);
        let (module, _diags) = parse(tokens);

        for class_def in &module.classes {
            for category in categorize_methods(class_def, &source) {
                let Some(name) = &category.name else {
                    continue;
                };
                named_categories += 1;
                if category.methods.is_empty() {
                    empty.push(format!(
                        "{}: {}::{name:?} has no methods",
                        path.display(),
                        class_def.name.name
                    ));
                }
            }
        }
    }

    assert!(
        empty.is_empty(),
        "found {} named categor{} with zero methods:\n{}",
        empty.len(),
        if empty.len() == 1 { "y" } else { "ies" },
        empty.join("\n")
    );

    assert!(
        named_categories > 100,
        "expected >100 named categories across the corpus, found \
         {named_categories} — the walk may have silently failed"
    );
}

/// BT-3240: the near-miss-divider lint's source-text scan
/// (`crate::near_miss_divider::check_near_miss_dividers`) agrees with this suite's own
/// raw scan (`corpus_has_no_near_miss_dividers`, above) — every
/// divider-shaped comment anywhere in the corpus parses cleanly, so the
/// lint (which runs live in the LSP) has nothing to flag today. A hit here
/// means either a real near-miss divider slipped into the corpus, or the
/// lint's shape detection has drifted from `parse_divider_name`.
#[test]
fn corpus_triggers_no_near_miss_divider_lint() {
    if !corpus_present() {
        return;
    }
    let files = corpus_files();
    let mut hits: Vec<String> = Vec::new();

    for path in &files {
        let source = read_corpus_file(path);
        let mut diagnostics = Vec::new();
        crate::near_miss_divider::check_near_miss_dividers(&source, &mut diagnostics);
        for diag in diagnostics {
            hits.push(format!("{}: {}", path.display(), diag.message));
        }
    }

    assert!(
        hits.is_empty(),
        "found {} near-miss-divider lint hit(s) in the corpus:\n{}",
        hits.len(),
        hits.join("\n")
    );
}
