// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4044: the reflective sends (`perform:`, `perform:withArguments:`,
//! `perform:withArguments:timeout:`, `performLocally:withArguments:`)
//! declare `-> Dynamic` explicitly, so a reflective send in a `typed`
//! class method is silent — the result of a reflective send really is
//! unknown, so "inferred as Dynamic (unannotated return)" has nothing
//! actionable for the caller to fix.
//!
//! An explicit `Dynamic` resolves to
//! [`DynamicReason::ExplicitDynamic`](super::super::DynamicReason),
//! which both the "expression inferred as Dynamic in typed class"
//! warning (`detect_dynamic_in_typed_class`) and the whole-method
//! "declares return type …, but its body inferred as Dynamic"
//! hint (`check_dynamic_body_return_hint`) deliberately exclude.

use super::common::*;

fn dynamic_diags(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| {
            d.message.contains("inferred as Dynamic") || d.message.contains("declares return type")
        })
        .collect()
}

fn check(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

/// Acceptance: `x perform: #s withArguments: #()` in a typed class
/// method declared `-> Object` produces no warning.
#[test]
fn perform_with_arguments_in_typed_method_is_silent() {
    let diags = check(
        "typed Object subclass: PerformClient\n\
         \x20 performIt: x :: Object -> Object =>\n\
         \x20\x20\x20 x perform: #name withArguments: #()\n",
    );
    let dynamic = dynamic_diags(&diags);
    assert!(
        dynamic.is_empty(),
        "perform:withArguments: is declared -> Dynamic, so no Dynamic/return-type \
         diagnostic is expected, got: {dynamic:?} (all: {diags:?})"
    );
}

/// The issue's exact repro shape: a metaclass-typed receiver
/// (`c :: Latetest class`) performing into a `-> Object` method.
#[test]
fn perform_through_metaclass_receiver_is_silent() {
    let diags = check(
        "typed Actor subclass: Latetest\n\
         \x20 performLocal: c :: Latetest class -> Object =>\n\
         \x20\x20\x20 c perform: #name withArguments: #()\n",
    );
    let dynamic = dynamic_diags(&diags);
    assert!(
        dynamic.is_empty(),
        "reflective send through a metaclass receiver should be silent, \
         got: {dynamic:?} (all: {diags:?})"
    );
}

/// Assigning the reflective result to a typed local is silent too.
#[test]
fn perform_result_assigned_to_typed_local_is_silent() {
    let diags = check(
        "typed Object subclass: PerformAssign\n\
         \x20 performIt: x :: Object -> Object =>\n\
         \x20\x20\x20 r :: Object := x perform: #name withArguments: #()\n\
         \x20\x20\x20 r\n",
    );
    let dynamic = dynamic_diags(&diags);
    assert!(
        dynamic.is_empty(),
        "typed-local assignment of a perform: result should be silent, \
         got: {dynamic:?} (all: {diags:?})"
    );
}

/// All reflective siblings share the explicit `-> Dynamic` declaration.
#[test]
fn perform_siblings_in_typed_method_are_silent() {
    let diags = check(
        "typed Object subclass: PerformSiblings\n\
         \x20 unary: x :: Object -> Object => x perform: #name\n\
         \x20 withTimeout: x :: Object -> Object =>\n\
         \x20\x20\x20 x perform: #name withArguments: #() timeout: 5000\n\
         \x20 locally: x :: Object -> Object =>\n\
         \x20\x20\x20 x performLocally: #name withArguments: #()\n",
    );
    let dynamic = dynamic_diags(&diags);
    assert!(
        dynamic.is_empty(),
        "perform: / perform:withArguments:timeout: / performLocally: should all \
         be silent, got: {dynamic:?} (all: {diags:?})"
    );
}

/// Negative control: a genuinely unannotated user method must still warn
/// in this same harness — the silence above comes from the stdlib fix,
/// not a deaf test setup. (That an `UnannotatedReturn` *reason* warns is
/// covered by `bt2868_unannotated_return_method_reports_honest_reason`.)
#[test]
fn unannotated_user_method_still_warns() {
    let diags = check(
        "typed Object subclass: StillWarns\n\
         \x20 helper: x => x\n\
         \x20 callIt: x :: Object -> Object => self helper: x\n",
    );
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("unannotated parameter")),
        "an unannotated user method must still warn, got: {diags:?}"
    );
}
