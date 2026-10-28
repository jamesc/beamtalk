// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4110: a class-side send substitutes method-local type params (`A`)
//! into its declared return type, as an instance-side send does.

use super::common::*;

fn type_diagnostics(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

fn mentions(diags: &[Diagnostic], needle: &str) -> bool {
    diags.iter().any(|d| d.message.contains(needle))
}

/// `inject:into:`-shaped class method: `A` binds from `initial` and the
/// block's result, so the send answers `Integer` and an unknown send on it
/// is reported against `Integer`.
#[test]
fn class_side_inject_into_result_substitutes_method_local_param() {
    let diags = type_diagnostics(
        r"
typed Object subclass: Folder
  class sealed fold: xs :: List(Integer) inject: initial :: A into: aBlock :: Block(A, Integer, A) -> A =>
    xs inject: initial into: aBlock
  class sealed run -> Integer =>
    total := self fold: #(1, 2) inject: 0 into: [:acc :x | acc + x]
    total fooBarBaz
    total
",
    );
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("Integer") && d.message.contains("fooBarBaz")),
        "the class-side send should answer Integer: {diags:#?}"
    );
}

/// The substituted `Integer` result is checked against a typed local:
/// assigning it to `:: String` reports a mismatch (before BT-4110 the result
/// was untyped).
#[test]
fn class_side_method_local_result_mismatches_typed_local() {
    let diags = type_diagnostics(
        r"
typed Object subclass: Folder
  class sealed fold: xs :: List(Integer) inject: initial :: A into: aBlock :: Block(A, Integer, A) -> A =>
    xs inject: initial into: aBlock
  class sealed run -> String =>
    total :: String := self fold: #(1, 2) inject: 0 into: [:acc :x | acc + x]
    total
",
    );
    assert!(
        mentions(&diags, "Type mismatch"),
        "an Integer result assigned to `:: String` should mismatch: {diags:#?}"
    );
}

/// When `A` cannot be bound (a `Dynamic` initial value and an opaque block
/// result), the send stays `Dynamic`: no false DNU on the result.
#[test]
fn class_side_unbound_method_local_result_stays_dynamic() {
    let diags = type_diagnostics(
        r"
Object subclass: Folder
  class sealed fold: xs :: List(Integer) inject: initial :: A into: aBlock :: Block(A, Integer, A) -> A =>
    xs inject: initial into: aBlock
  class sealed run: seed =>
    total := self fold: #(1, 2) inject: seed into: [:acc :x | acc]
    total fooBarBaz
",
    );
    assert!(
        !mentions(&diags, "fooBarBaz"),
        "an unbound `A` must not produce a DNU: {diags:#?}"
    );
}
