// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test helpers for `semantic_analysis` tests.

use crate::ast::Module;
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::{Severity, Span};

pub fn test_span() -> Span {
    Span::new(0, 0)
}

/// Lex and parse `src`, asserting it produces no parse errors.
///
/// Use this in tests whose fixture is expected to be syntactically valid; the
/// assertion surfaces parse problems immediately rather than letting them
/// silently propagate as empty/malformed ASTs.
///
/// # Panics
///
/// Panics if `src` contains any parse errors, with a message that lists the
/// diagnostics — indicating a broken test fixture, not a bug under test.
pub fn parse_module(src: &str) -> Module {
    let (module, diags) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
    assert!(
        diags.iter().all(|d| d.severity != Severity::Error),
        "fixture must parse: {diags:?}"
    );
    module
}

/// Parses `src` (no Error or Warning diagnostics allowed, via
/// [`parse_ok`](crate::test_helpers::test_support::parse_ok)) and builds its
/// [`ClassHierarchy`], unwrapping the build.
///
/// # Panics
///
/// Panics if `src` does not parse cleanly or its hierarchy cannot be built —
/// a broken test fixture, not a bug under test.
pub fn parse_and_build(src: &str) -> (Module, ClassHierarchy) {
    let module = crate::test_helpers::test_support::parse_ok(src);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    (module, hierarchy)
}
