// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test fixtures for `unparse` per-feature tests.
//!
//! These helpers were previously inlined at the top (and middle) of the
//! monolithic `mod.rs` test module. They are re-exported into every child
//! test module via `use super::common::*;`.

pub(super) use super::super::*;
pub(super) use crate::source_analysis::Span;

pub(super) fn span() -> Span {
    Span::new(0, 0)
}

// Helper: lex + parse a source string into a Module.
pub(super) fn parse_source(source: &str) -> crate::ast::Module {
    use crate::source_analysis::{lex_with_eof, parse};
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);
    module
}

/// Assert that `source` round-trips through parse→unparse and that the
/// second pass is idempotent: `unparse(parse(unparse(parse(source)))) ==
/// unparse(parse(source))`.
///
/// Use this for sources that are already in canonical form so the first
/// unparse pass should not change them.
#[track_caller]
pub(super) fn assert_idempotent(source: &str) {
    let pass1 = unparse_module(&parse_source(source));
    let pass2 = unparse_module(&parse_source(&pass1));
    assert_eq!(
        pass1, pass2,
        "unparser is not idempotent for source:\n{source}\n\npass1:\n{pass1}\n\npass2:\n{pass2}"
    );
}

/// Assert that `source` is already in canonical format: the first format
/// pass must be a no-op (`format_source(source) == source`).
///
/// Use this for fixture files that are pre-formatted so regressions are
/// caught immediately — any formatter change that silently alters
/// already-canonical code will fail here.
#[track_caller]
pub(super) fn assert_identity(source: &str) {
    let formatted = format_source(source)
        .expect("format_source must succeed for canonical source (no parse errors)");
    assert_eq!(
        formatted, source,
        "formatter changed already-canonical source.\n\noriginal:\n{source}\n\nformatted:\n{formatted}"
    );
}

/// Like [`assert_identity`], but for syntax that parses to a diagnostic
/// `Severity::Error` on purpose (e.g. a "not yet supported" placeholder for
/// a still-unimplemented form) — `format_source` refuses to format anything
/// with an error diagnostic, so this bypasses that gate and unparses
/// directly via [`parse_source`]/`unparse_module`. Still checks canonical
/// round-tripping, just without requiring a clean parse.
#[track_caller]
pub(super) fn assert_identity_despite_errors(source: &str) {
    let formatted = unparse_module(&parse_source(source));
    assert_eq!(
        formatted, source,
        "formatter changed already-canonical source.\n\noriginal:\n{source}\n\nformatted:\n{formatted}"
    );
}
