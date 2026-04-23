// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `self error:` Never propagation in class-side and block contexts (BT-2037).

use super::common::*;

// =====================================================================
// BT-2037 — `self error:` should infer as Never inside typed classes,
// both class-side and inside block arguments.
// =====================================================================

/// BT-2037: a class-side method that calls `self error: "..."` should
/// not warn about Dynamic-in-typed-class. `error:` is declared
/// `-> Never` on `Object`; the type checker must propagate that through
/// the Class→Behaviour→Object→ProtoObject chain when no class-side
/// override exists.
#[test]
fn bt2037_class_side_self_error_infers_as_never() {
    let source = "
typed Object subclass: WidgetFactory
  class new: arg :: Object -> Nil =>
    self error: \"use named constructors\"
";
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
        .filter(|d| d.message.contains("inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "class-side `self error:` should resolve to Never, not Dynamic; got: {:?}",
        dynamic_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// BT-2037: an instance-side method that wraps `self error: "..."` in a
/// block argument (e.g. as a fallback for `valueOrDo:`) should not warn
/// about Dynamic-in-typed-class. `self` inside the block is the same
/// instance type as in the enclosing method, and `error:` is `-> Never`.
#[test]
fn bt2037_block_self_error_infers_as_never() {
    // Mirrors the exdura reproduction: a typed instance method whose body
    // sends `valueOrDo:` to `registry` (an Object-typed field), with a
    // fallback block containing `self error:`. The block body's `self
    // error:` must still resolve to Never via the enclosing instance-method
    // `self`.
    let source = "
typed Object subclass: ExduraWorker
  state: registry :: Object = nil
  doWork -> Nil =>
    registry valueOrDo: [:e | self error: \"failed\"]
";
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
        .filter(|d| d.message.contains("inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "block-embedded `self error:` should resolve to Never, not Dynamic; got: {:?}",
        dynamic_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}
