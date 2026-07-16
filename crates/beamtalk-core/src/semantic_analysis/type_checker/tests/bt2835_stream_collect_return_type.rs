// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Stream>>collect:` propagates the transform's return type to the
//! resulting stream's element type (BT-2835, follow-up to BT-2826).
//!
//! Before this fix, `Stream>>collect:` declared `Block(E, Object) -> Stream`
//! — the block param inferred correctly (BT-2826), but the transform's
//! *return* type was erased rather than propagated, leaving the resulting
//! stream unparameterized. Chaining a further typed operation onto the
//! result left its block param `Dynamic(UnannotatedParam)`. With
//! `Block(E, R) -> Stream(R)`, matching the free-type-variable convention
//! already used by `List>>collect:` and `Stream>>inject:into:`, the
//! transform's return type `R` now propagates to the resulting stream's
//! element type.

use super::common::*;

/// `(Stream on: #(1, 2, 3)) collect: [:n | n printString]` produces a
/// `Stream(String)`. Chaining `select: [:s | s isEmpty not]` onto it should
/// infer `s` as `String` (propagated from the transform's return type), so
/// `s isEmpty` resolves with no DNU and no "unannotated parameter" Dynamic
/// warning.
#[test]
fn bt2835_stream_collect_propagates_return_type_to_chained_operation() {
    let source = r"
typed Object subclass: Repro
  shout -> List(String) =>
    ^(((Stream on: #(1, 2, 3)) collect: [:n | n printString]) select: [:s | s isEmpty not]) asList
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "s isEmpty should resolve — s inferred as String, propagated from \
         collect:'s transform return type; got: {:?}",
        dnu_warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );

    let unannotated_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("unannotated parameter"))
        .collect();
    assert!(
        unannotated_warnings.is_empty(),
        "block param `s` should be typed String (propagated from \
         collect:'s Block(E, R) -> Stream(R)), not Dynamic; got: {:?}",
        unannotated_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}
