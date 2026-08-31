// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Corpus round-trip validation for the byte-span resolver (ADR 0082, Phase 0).
//!
//! This is the **load-bearing validation spike** for ADR 0082. The entire
//! method-level edit-and-save design rests on one assumption: that the parser
//! can resolve *any* method's exact byte span against arbitrary `.bt` files, so
//! that flush can splice a patched method body in by byte replacement (no AST
//! reprint).
//!
//! The proof: for **every method in the stdlib + `examples/` corpus**, resolve
//! its span, splice the span's own verbatim bytes back in (a no-op edit), and
//! assert the resulting file is **byte-identical** to the original. If this
//! holds across the whole corpus — doc comments, multi-line bodies, cascades,
//! class-side methods, binary selectors, trailing-comment lines — the splice
//! strategy is sound and the rest of ADR 0082 can be built on it.
//!
//! In addition to the no-op identity check (which is tautological for a correct
//! slice), this suite asserts the *structural* properties that make the span
//! meaningful: every method resolves to exactly one span, spans are non-empty,
//! cover the method's selector text, lie within the class definition, are
//! ordered, and never overlap a sibling. Those properties are what would break
//! if the resolver returned a wrong or sloppy span.
//!
//! The corpus tests that also exercise `unparse` (reshape round-trip, full
//! save/flush pipeline round-trip, and the receiver-span/syntactic-send-walk
//! conformance check) live in `crate::unparse`'s own test tree instead
//! (BT-3346) — this module has no test-time dependency on `unparse`.

use std::collections::BTreeSet;

use crate::source_analysis::corpus_test_support::{
    MethodTarget, corpus_files, corpus_present, enumerate_methods, read_corpus_file,
};
use crate::source_analysis::method_span::resolve_in_module;
use crate::source_analysis::{Span, lex_with_eof, parse};

/// Splices `replacement` into `source` over `span`.
fn splice(source: &str, span: Span, replacement: &str) -> String {
    let mut out = String::with_capacity(source.len());
    out.push_str(&source[..span.start() as usize]);
    out.push_str(replacement);
    out.push_str(&source[span.end() as usize..]);
    out
}

/// The headline Phase 0 proof: no-op byte-span splice is byte-identical across
/// the entire corpus.
#[test]
fn corpus_round_trip_is_byte_identical() {
    if !corpus_present() {
        // Partial checkout / source distribution without the workspace corpus
        // dirs: nothing to validate here.
        return;
    }
    let files = corpus_files();
    assert!(
        !files.is_empty(),
        "corpus walk found no .bt files — check repo layout"
    );

    let mut total_methods = 0usize;
    let mut failures: Vec<String> = Vec::new();

    for path in &files {
        let source = read_corpus_file(path);
        let tokens = lex_with_eof(&source);
        let (module, _diags) = parse(tokens);

        for target in enumerate_methods(&module) {
            total_methods += 1;
            let span = match resolve_in_module(
                &module,
                &source,
                &target.class,
                &target.selector,
                target.side,
            ) {
                Ok(span) => span,
                Err(e) => {
                    failures.push(format!(
                        "{}: {}.{} ({}): resolve failed: {e}",
                        path.display(),
                        target.class,
                        target.selector,
                        target.side.as_str()
                    ));
                    continue;
                }
            };

            // The core no-op identity: splicing the span's own bytes back is a
            // byte-for-byte no-op.
            let own_bytes = &source[span.as_range()];
            let round_tripped = splice(&source, span, own_bytes);
            if round_tripped != source {
                failures.push(format!(
                    "{}: {}.{} ({}): no-op splice changed the file",
                    path.display(),
                    target.class,
                    target.selector,
                    target.side.as_str()
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "corpus round-trip failed for {} method(s) (of {} across {} files):\n{}",
        failures.len(),
        total_methods,
        files.len(),
        failures.join("\n")
    );

    // Sanity: the corpus is non-trivial (>1300 methods across the stdlib +
    // examples at time of writing). If this ever drops to a handful, the walk
    // silently broke.
    assert!(
        total_methods > 100,
        "expected the corpus to contain >100 methods, found {total_methods} — \
         the walk may have silently failed"
    );
}

/// Structural invariants that make the no-op identity meaningful: every method
/// resolves to exactly one non-empty span that covers its selector, lies within
/// its class, and never overlaps a sibling in the same file.
#[test]
fn corpus_spans_are_well_formed_and_non_overlapping() {
    if !corpus_present() {
        return;
    }
    let files = corpus_files();
    let mut failures: Vec<String> = Vec::new();

    for path in &files {
        let source = read_corpus_file(path);
        let tokens = lex_with_eof(&source);
        let (module, _diags) = parse(tokens);

        let mut resolved: Vec<(MethodTarget, Span)> = Vec::new();
        for target in enumerate_methods(&module) {
            let Ok(span) = resolve_in_module(
                &module,
                &source,
                &target.class,
                &target.selector,
                target.side,
            ) else {
                // Resolution failures are reported by the round-trip test; skip
                // here to keep this test focused on geometry.
                continue;
            };

            // Span must be non-empty and within the file.
            if span.is_empty() {
                failures.push(format!(
                    "{}: {}.{} ({}): empty span",
                    path.display(),
                    target.class,
                    target.selector,
                    target.side.as_str()
                ));
            }
            if span.end() as usize > source.len() {
                failures.push(format!(
                    "{}: {}.{} ({}): span end {} exceeds file length {}",
                    path.display(),
                    target.class,
                    target.selector,
                    target.side.as_str(),
                    span.end(),
                    source.len()
                ));
            }

            // The span must contain the selector's textual start (the AST span's
            // start lies inside the resolved span — the resolver only extends
            // the end, never moves the start).
            if !(span.start() <= target.ast_span.start() && target.ast_span.start() < span.end()) {
                failures.push(format!(
                    "{}: {}.{} ({}): span {:?} does not contain selector start {}",
                    path.display(),
                    target.class,
                    target.selector,
                    target.side.as_str(),
                    span.as_range(),
                    target.ast_span.start()
                ));
            }

            resolved.push((target, span));
        }

        // No two distinct method spans in the same file may overlap.
        resolved.sort_by_key(|(_, span)| (span.start(), span.end()));
        for window in resolved.windows(2) {
            let (a_t, a) = &window[0];
            let (b_t, b) = &window[1];
            if a.end() > b.start() {
                failures.push(format!(
                    "{}: spans overlap: {}.{} ({}) {:?} vs {}.{} ({}) {:?}",
                    path.display(),
                    a_t.class,
                    a_t.selector,
                    a_t.side.as_str(),
                    a.as_range(),
                    b_t.class,
                    b_t.selector,
                    b_t.side.as_str(),
                    b.as_range()
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "span geometry violations:\n{}",
        failures.join("\n")
    );
}

/// Every method in the corpus must resolve — none may report a structured
/// error. This is the "no false negatives" half of the proof: the resolver
/// finds a span for *every* method the parser knows about.
#[test]
fn corpus_every_method_resolves() {
    if !corpus_present() {
        return;
    }
    let files = corpus_files();
    let mut unresolved: BTreeSet<String> = BTreeSet::new();
    let mut total = 0usize;

    for path in &files {
        let source = read_corpus_file(path);
        let tokens = lex_with_eof(&source);
        let (module, _diags) = parse(tokens);

        for target in enumerate_methods(&module) {
            total += 1;
            if resolve_in_module(
                &module,
                &source,
                &target.class,
                &target.selector,
                target.side,
            )
            .is_err()
            {
                unresolved.insert(format!(
                    "{}: {}.{} ({})",
                    path.display(),
                    target.class,
                    target.selector,
                    target.side.as_str()
                ));
            }
        }
    }

    assert!(
        unresolved.is_empty(),
        "{} of {} methods did not resolve:\n{}",
        unresolved.len(),
        total,
        unresolved.iter().cloned().collect::<Vec<_>>().join("\n")
    );
}
