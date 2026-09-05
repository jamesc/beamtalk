// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Corpus-wide conformance tests for the unparser against the byte-span
//! resolver and the method-source send walker (ADR 0082 / BT-2584 / BT-2594 /
//! BT-3217).
//!
//! These moved here from `source_analysis::method_span_corpus_tests` (BT-3346,
//! ADR 0117 Phase 4): each test below round-trips through `unparse_method` or
//! `reindent_method_source`, so — unlike their `source_analysis`-only
//! siblings that stayed behind (span geometry, resolvability) — they belong in
//! `unparse`'s own test tree, not `source_analysis`'s.
//!
//! All three walk the same **stdlib + `examples/` corpus** as the tests that
//! stayed in `source_analysis`, via the shared
//! [`crate::source_analysis::corpus_test_support`] leaf module.

use std::collections::BTreeSet;

use super::{reindent_method_source, unparse_method};
use crate::ast::Module;
use crate::source_analysis::corpus_test_support::{
    corpus_files, corpus_present, enumerate_methods, read_corpus_file,
};
use crate::source_analysis::method_span::resolve_in_module;
use crate::source_analysis::{lex_with_eof, parse, parse_method};

/// The leading run of spaces/tabs of the first line of `body` — the base
/// indentation of the on-disk method definition the span covers. Mirrors the
/// install hook's `leading_ws/1` (Erlang) and the port's `leading_whitespace`.
fn base_indent(body: &str) -> &str {
    let first_line = body.split('\n').next().unwrap_or("");
    let len = first_line
        .bytes()
        .take_while(|&b| b == b' ' || b == b'\t')
        .count();
    &first_line[..len]
}

/// Strip up to `n` leading whitespace bytes from every line of `s` (the lines'
/// shared base indent), preserving relative indentation. Recovers the column-0
/// bare-method body the live editor sends from the file-indented disk slice.
fn dedent(s: &str, n: usize) -> String {
    let mut out = String::with_capacity(s.len());
    let mut first = true;
    for line in s.split('\n') {
        if !first {
            out.push('\n');
        }
        first = false;
        // ASCII whitespace is single-byte, so the byte count is a char boundary.
        let strip = line
            .bytes()
            .take(n)
            .take_while(|&b| b == b' ' || b == b'\t')
            .count();
        out.push_str(&line[strip..]);
    }
    out
}

/// Make `s`'s trailing-newline state match `reference`'s. Mirrors the install
/// hook's `match_trailing_newline/2` (Erlang).
fn match_trailing_newline(s: &str, reference: &str) -> String {
    let trimmed = s.trim_end_matches('\n');
    if reference.ends_with('\n') {
        format!("{trimmed}\n")
    } else {
        trimmed.to_string()
    }
}

/// BT-2584: the install-hook reshape round-trips every method's disk slice
/// byte-for-byte.
///
/// This is the "`source_ref == disk[span]` by construction" proof for the part
/// BT-2584 owns: the *reshape* the install hook applies to turn a column-0
/// canonical body into the on-disk byte-span shape, and that flush then splices
/// verbatim. The reshape is two mutually-inverse transforms:
///
/// 1. [`reindent_method_source`] (with the span's base indent), which strips a
///    column-0 body's shared indent and re-prepends the base; and
/// 2. trailing-newline matching to the disk slice (the install hook's
///    `match_trailing_newline/2`).
///
/// Their composition with the inverse [`dedent`] must be the identity on every
/// method's verbatim disk slice across the stdlib + `examples/` corpus — doc
/// comments, multi-line bodies/signatures, blank lines between doc and method,
/// class-side methods, binary selectors, last-method-without-trailing-newline.
/// That is exactly the invariant that lets flush drop the old `reindent/2`
/// reconciliation: whatever the install hook stores re-indents back to the slice
/// it replaces.
///
/// Since BT-2594 `reindent_method_source` re-lays-out at the target indent (it
/// re-parses and re-renders, rather than only shifting whitespace), so this holds
/// for the whole corpus only because the corpus is `bt fmt`-clean — i.e. every
/// method's disk shape already *is* its canonical layout at the span's indent.
/// That cleanliness is enforced in CI by `just fmt-check-beamtalk` (Justfile:
/// `fmt-check-beamtalk`, covering `stdlib/` and `examples/`).
/// `corpus_methods_round_trip_byte_identical` proves the complementary direction:
/// the full `unparse_method` → reshape pipeline reproduces the disk slice.
#[test]
fn corpus_reshape_round_trip_is_byte_identical() {
    if !corpus_present() {
        return;
    }
    let files = corpus_files();
    let mut total_methods = 0usize;
    let mut failures: Vec<String> = Vec::new();

    for path in &files {
        let source = read_corpus_file(path);
        let tokens = lex_with_eof(&source);
        let (module, _diags) = parse(tokens);

        for target in enumerate_methods(&module) {
            total_methods += 1;
            let Ok(span) = resolve_in_module(
                &module,
                &source,
                &target.class,
                &target.selector,
                target.side,
            ) else {
                // Resolution failures are reported by the round-trip test.
                continue;
            };
            let disk_slice = &source[span.as_range()];
            let base = base_indent(disk_slice);

            // The column-0 "stored canonical" body the editor/compiler hands the
            // install hook for an unchanged method: the slice dedented to its
            // base. Re-indenting it back to the base and matching the slice's
            // trailing newline must reproduce the slice exactly.
            let column0 = dedent(disk_slice, base.len());
            let reindented = reindent_method_source(base, &column0);
            let stored = match_trailing_newline(&reindented, disk_slice);

            if stored != disk_slice {
                failures.push(format!(
                    "{}: {}.{} ({}):\n  disk[span] = {disk_slice:?}\n  stored     = {stored:?}",
                    path.display(),
                    target.class,
                    target.selector,
                    target.side.as_str(),
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "reshape round-trip changed {} of {} method(s) — \
         the re-indented body is not byte-identical to disk[span]:\n{}",
        failures.len(),
        total_methods,
        failures.join("\n\n")
    );
    assert!(
        total_methods > 100,
        "expected the corpus to contain >100 methods, found {total_methods}"
    );
}

/// BT-2594: the full production save/flush pipeline reproduces `disk[span]`
/// byte-for-byte for **every** method in the corpus — no skipped subset.
///
/// The live save pipeline is: the editor's bare body is re-parsed + re-emitted
/// (`compile_method` → `unparse_method`), then the install hook re-indents that
/// canonical body to the span's base (`reindent_method_source`) and matches the
/// slice's trailing newline. For a *no-op* save of an unchanged method, the
/// result must equal `disk[span]` exactly — otherwise saving silently reformats
/// the file.
///
/// This previously held only for the subset whose on-disk body already matched
/// the unparser's *column-0* layout (~80%); the rest diverged because
/// `unparse_method` decides line breaks at column 0 while the method lives
/// indented on disk (BT-2594, bucket 3), and because the per-method source
/// dropped the `class ` prefix for class-side methods (bucket 2). With
/// `reindent_method_source` re-laying-out at the target indent and
/// `MethodDefinition::is_class_method` carrying the prefix, the pipeline is now
/// byte-identical for the **whole** corpus — provided the corpus is `bt fmt`-clean
/// (enforced for stdlib and examples by `fmt-check-beamtalk`). So this asserts
/// 100%, not a majority subset.
#[test]
fn corpus_methods_round_trip_byte_identical() {
    if !corpus_present() {
        return;
    }
    let files = corpus_files();
    let mut checked = 0usize;
    let mut failures: Vec<String> = Vec::new();

    for path in &files {
        let source = read_corpus_file(path);
        let tokens = lex_with_eof(&source);
        let (module, _diags) = parse(tokens);

        for target in enumerate_methods(&module) {
            let Ok(span) = resolve_in_module(
                &module,
                &source,
                &target.class,
                &target.selector,
                target.side,
            ) else {
                continue;
            };
            let disk_slice = &source[span.as_range()];
            let base = base_indent(disk_slice);

            let column0 = dedent(disk_slice, base.len());
            let method_tokens = lex_with_eof(&column0);
            let (parsed, _diags) = parse_method(method_tokens);
            let Some(method) = parsed else {
                continue;
            };
            // The full production pipeline: compiler-canonical body → install-hook
            // reshape (re-layout at the span's indent + trailing-newline match).
            let canonical = unparse_method(&method);
            checked += 1;

            let reindented = reindent_method_source(base, &canonical);
            let stored = match_trailing_newline(&reindented, disk_slice);
            if stored != disk_slice {
                failures.push(format!(
                    "{}: {}.{} ({}):\n  disk[span] = {disk_slice:?}\n  stored     = {stored:?}",
                    path.display(),
                    target.class,
                    target.selector,
                    target.side.as_str(),
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "{} method(s) did not round-trip byte-identical through the save/flush \
         pipeline — a no-op save would reformat them:\n{}",
        failures.len(),
        failures.join("\n\n")
    );
    assert!(
        checked > 100,
        "expected >100 methods to validate the pipeline against, only found {checked}"
    );
}

/// Enumerates every `&MethodDefinition` in `module` alongside a
/// human-readable label (`Class.selector (side)`) — unlike
/// [`crate::source_analysis::corpus_test_support::enumerate_methods`], which
/// returns identity-only targets for `resolve_in_module` round-tripping, this
/// hands back the actual method AST the BT-3217 conformance test below needs
/// to run both walkers against.
fn corpus_method_definitions(module: &Module) -> Vec<(String, &crate::ast::MethodDefinition)> {
    let mut out = Vec::new();
    for class_def in &module.classes {
        for m in &class_def.methods {
            out.push((
                format!("{}.{} (instance)", class_def.name.name, m.selector.name()),
                m,
            ));
        }
        for m in &class_def.class_methods {
            out.push((
                format!("{}.{} (class)", class_def.name.name, m.selector.name()),
                m,
            ));
        }
    }
    for standalone in &module.method_definitions {
        let side = if standalone.is_class_method {
            "class"
        } else {
            "instance"
        };
        out.push((
            format!(
                "{}.{} ({side})",
                standalone.class_name.name,
                standalone.method.selector.name()
            ),
            &standalone.method,
        ));
    }
    out
}

/// BT-3217 (ADR 0115 Phase 2): `build_method_xref_entry` joins
/// `method_source_walker::collect_receiver_spans`'s span-carrying walk of the
/// *original* AST to `find_all_sends_in_source`'s syntactic walk of a
/// re-unparsed/re-parsed copy **by pre-order ordinal** — the two walks must
/// find the same number of hits, in the same order, sending the same
/// selectors, or the join silently misaligns a send with the wrong
/// receiver's inferred type.
///
/// This is the corpus conformance test the ADR 0115 Phase 1 spike
/// (`docs/internal/adr-0115-phase1-spike-findings.md` §1d) called for: proof
/// that the ordinal-join assumption holds for every method in the
/// stdlib + `examples/` corpus, not just an asserted invariant in a comment
/// (this project's no-"keep-in-sync"-comment-without-a-test rule).
///
/// Was non-empty for two `system_navigation.bt` methods until BT-3223 fixed
/// the underlying parser bug (`is_at_declaration_level_expect` misclassified
/// a body-level `@expect` as declaration-level when the enclosing method was
/// rendered at column 0 — exactly `find_all_sends_in_source`'s synthetic-wrap
/// shape). Kept empty-but-present rather than removed: any *new* divergence
/// still fails the test outright, and a future one gets the same narrow,
/// documented allowlist entry this one did rather than silently masking it.
const KNOWN_DIVERGENT_METHODS: &[(&str, &str)] = &[];

#[test]
fn corpus_receiver_span_walk_matches_syntactic_send_walk() {
    use crate::method_source_walker::{collect_receiver_spans, find_all_sends_in_source};

    if !corpus_present() {
        return;
    }
    let files = corpus_files();
    let mut total_methods = 0usize;
    let mut total_hits = 0usize;
    let mut failures: Vec<String> = Vec::new();
    let mut known_divergences_seen: BTreeSet<(&str, &str)> = BTreeSet::new();

    for path in &files {
        let source = read_corpus_file(path);
        let tokens = lex_with_eof(&source);
        let (module, _diags) = parse(tokens);
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

        for (label, method) in corpus_method_definitions(&module) {
            total_methods += 1;

            // The exact same source channel `build_method_xref_entry` feeds
            // `find_all_sends_in_source` (a direct `unparse_method(method)` call,
            // deliberately bypassing `extract_method_source`'s BT-3249
            // inferred-return-type stripping — xref/`referencesTo:` still needs to
            // see writeback-inferred type references, only the human-facing
            // browsable source should hide them).
            let bare_source = unparse_method(method);
            let syntactic_hits = find_all_sends_in_source(&bare_source);
            let span_hits = collect_receiver_spans(method);

            let known_entry = KNOWN_DIVERGENT_METHODS
                .iter()
                .find(|&&(f, l)| f == file_name && l == label);

            let mut case_failures: Vec<String> = Vec::new();

            if syntactic_hits.len() == span_hits.len() {
                for (i, (syn_hit, span_hit)) in
                    syntactic_hits.iter().zip(span_hits.iter()).enumerate()
                {
                    if syn_hit.selector != span_hit.selector {
                        case_failures.push(format!(
                            "{}: {label}: selector mismatch at pre-order ordinal {i} — \
                             syntactic walk: {:?}, span walk: {:?}",
                            path.display(),
                            syn_hit.selector,
                            span_hit.selector,
                        ));
                    }
                }
            } else {
                case_failures.push(format!(
                    "{}: {label}: hit count mismatch — syntactic walk found {}, \
                     span walk found {}",
                    path.display(),
                    syntactic_hits.len(),
                    span_hits.len(),
                ));
            }

            if !case_failures.is_empty() {
                if let Some(&entry) = known_entry {
                    known_divergences_seen.insert(entry);
                } else {
                    failures.extend(case_failures);
                }
            }

            total_hits += syntactic_hits.len();
        }
    }

    assert!(
        failures.is_empty(),
        "receiver-span walk diverged from the syntactic send walk for {} case(s) \
         (of {} methods, {} total hits, across {} files):\n{}",
        failures.len(),
        total_methods,
        total_hits,
        files.len(),
        failures.join("\n")
    );
    assert!(
        total_methods > 100,
        "expected the corpus to contain >100 methods, found {total_methods} — \
         the walk may have silently failed"
    );
    assert!(
        total_hits > 100,
        "expected the corpus to contain >100 message sends, found {total_hits} — \
         the walk may have silently failed"
    );
    // Every `KNOWN_DIVERGENT_METHODS` entry must still actually diverge — if
    // BT-3223's parser fix lands, this fails loudly rather than letting a
    // stale exception silently mask a *new*, different divergence at the
    // same (file, label) key.
    assert_eq!(
        known_divergences_seen.len(),
        KNOWN_DIVERGENT_METHODS.len(),
        "KNOWN_DIVERGENT_METHODS lists {} entries but only {} were observed to \
         actually diverge this run — if BT-3223 is fixed, remove the \
         now-stale entries from KNOWN_DIVERGENT_METHODS",
        KNOWN_DIVERGENT_METHODS.len(),
        known_divergences_seen.len(),
    );
}
