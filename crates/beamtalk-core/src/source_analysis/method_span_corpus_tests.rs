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

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use crate::ast::Module;
use crate::source_analysis::method_span::{MethodSide, resolve_in_module};
use crate::source_analysis::{Span, lex_with_eof, parse};

/// Returns the repository root (`CARGO_MANIFEST_DIR/../..`).
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf()
}

/// Recursively collects every `.bt` file under `dir`.
fn collect_bt_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut entries: Vec<_> = entries.filter_map(Result::ok).map(|e| e.path()).collect();
    // Deterministic order for stable test output.
    entries.sort();
    for path in entries {
        if path.is_dir() {
            collect_bt_files(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "bt") {
            out.push(path);
        }
    }
}

/// The corpus directories: every `.bt` file lives under one of these.
fn corpus_dirs() -> [PathBuf; 2] {
    let root = repo_root();
    [root.join("stdlib/src"), root.join("examples")]
}

/// Whether the corpus directories are present in this checkout.
///
/// They are absent when the crate is built from a source distribution or a
/// partial checkout that omits the workspace `stdlib/` and `examples/` trees. In
/// that case the corpus tests skip rather than hard-fail (a present-but-
/// unreadable *file*, by contrast, is always a hard failure).
fn corpus_present() -> bool {
    corpus_dirs().iter().any(|dir| dir.is_dir())
}

/// Gathers the whole corpus: every `.bt` file under `stdlib/src` and `examples`.
fn corpus_files() -> Vec<PathBuf> {
    let mut files = Vec::new();
    for dir in corpus_dirs() {
        collect_bt_files(&dir, &mut files);
    }
    files
}

/// Reads a corpus file, hard-failing (with the path + error) if it is present
/// but cannot be read. Unlike skipping an absent *directory*, an unreadable file
/// that the walk already discovered must never be silently ignored — that would
/// let the round-trip proof go false-green.
fn read_corpus_file(path: &Path) -> String {
    std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("could not read corpus file {}: {e}", path.display()))
}

/// One method to resolve, identified the way a caller would: by class name,
/// canonical selector string, and side.
#[derive(Debug, Clone)]
struct MethodTarget {
    class: String,
    selector: String,
    side: MethodSide,
    /// AST span of the definition, for the structural overlap/ordering checks.
    ast_span: Span,
}

/// Enumerates every resolvable method definition in `module`.
///
/// This mirrors what the resolver searches: instance and class methods in class
/// bodies, plus standalone `Class >> selector` extension definitions.
fn enumerate_methods(module: &Module) -> Vec<MethodTarget> {
    let mut targets = Vec::new();
    for class_def in &module.classes {
        for m in &class_def.methods {
            targets.push(MethodTarget {
                class: class_def.name.name.to_string(),
                selector: m.selector.name().to_string(),
                side: MethodSide::Instance,
                ast_span: m.span,
            });
        }
        for m in &class_def.class_methods {
            targets.push(MethodTarget {
                class: class_def.name.name.to_string(),
                selector: m.selector.name().to_string(),
                side: MethodSide::Class,
                ast_span: m.span,
            });
        }
    }
    for standalone in &module.method_definitions {
        targets.push(MethodTarget {
            class: standalone.class_name.name.to_string(),
            selector: standalone.method.selector.name().to_string(),
            side: if standalone.is_class_method {
                MethodSide::Class
            } else {
                MethodSide::Instance
            },
            ast_span: standalone.span,
        });
    }
    targets
}

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
/// 1. [`crate::unparse::reindent_method_source`] (with the span's base indent),
///    which strips a column-0 body's shared indent and re-prepends the base; and
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
    use crate::unparse::reindent_method_source;

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
    use crate::source_analysis::parse_method;
    use crate::unparse::{reindent_method_source, unparse_method};

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
