// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dynamic-iterable block-param inference in typed classes (BT-2042).

use super::common::*;

// BT-2042: Block param inferred Dynamic in `typed` class when iterable's
// element type is Dynamic.
//
// When a block argument is passed to a method on a Dynamic receiver (e.g.,
// `Dictionary at:ifAbsent:` returns Dynamic, and we call `anySatisfy:` on
// it), the block's parameters previously defaulted to
// `Dynamic(UnannotatedParam)`. In a `typed` class, every use of the block
// param then re-fired the "expression inferred as Dynamic in typed class"
// warning — forcing users to annotate iterators whose upstream iterable was
// Dynamic, even though the root cause lives at the receiver, not the block.
//
// The fix propagates `Dynamic(DynamicReceiver)` into block params in the
// fallback path (non-Known receiver / unresolved selector), matching how the
// send's result itself is classified. `DynamicReceiver` is filtered from the
// BT-1914 warning, so usages of the block param no longer double-warn.
// =========================================================================

/// Repro from the issue: iterating the result of `Dictionary at:ifAbsent:`
/// (Dynamic) inside a `typed` class. The block param `b` is unannotated and
/// its usage `b at: "state" ifAbsent: [nil]` previously emitted a spurious
/// "Dynamic in typed class (unannotated parameter)" warning.
#[test]
fn bt2042_dynamic_iterable_does_not_warn_on_block_param_usage() {
    let source = r#"
typed Value subclass: Filter
  hasMissingState: issue :: Dictionary -> Boolean =>
    blockers := issue at: "blocked_by" ifAbsent: [#()]
    blockers anySatisfy: [:b |
      bState := b at: "state" ifAbsent: [nil]
      bState isNil
    ]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "block param use should not fire the Dynamic-in-typed-class warning when \
         the receiver is itself Dynamic (the root cause is the iterable, not the \
         block); got: {:?}",
        dynamic_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Workaround: annotating the local that feeds the iterator
/// (`blockers :: List(Dictionary) := ...`) still lints clean.
#[test]
fn bt2042_workaround_annotated_local_still_works() {
    let source = r#"
typed Value subclass: Filter
  hasMissingState: issue :: Dictionary -> Boolean =>
    blockers :: List(Dictionary) := issue at: "blocked_by" ifAbsent: [#()]
    blockers anySatisfy: [:b |
      bState := b at: "state" ifAbsent: [nil]
      bState isNil
    ]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "annotated local should lint clean (block params resolve via List(Dictionary)); got: {:?}",
        dynamic_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Regression guard: when the receiver's element type *is* known (e.g.,
/// iterating a typed `List(Dictionary)` field), the existing block-param
/// inference path continues to resolve `b` to `Dictionary` — no regression.
#[test]
fn bt2042_known_receiver_still_resolves_block_param_type() {
    let source = r#"
typed Value subclass: Filter
  field: items :: List(Dictionary) = #()

  hasMissingState -> Boolean =>
    self.items anySatisfy: [:b |
      bState := b at: "state" ifAbsent: [nil]
      bState isNil
    ]
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();

    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "Known List(Dictionary) receiver should resolve block param to Dictionary; got: {:?}",
        dynamic_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}
