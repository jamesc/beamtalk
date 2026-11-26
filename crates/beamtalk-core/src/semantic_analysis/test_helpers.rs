// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test helpers for `semantic_analysis` tests.

use crate::ast::Module;
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::Span;

pub fn test_span() -> Span {
    Span::new(0, 0)
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
    let hierarchy = ClassHierarchy::build(&module).0;
    (module, hierarchy)
}
