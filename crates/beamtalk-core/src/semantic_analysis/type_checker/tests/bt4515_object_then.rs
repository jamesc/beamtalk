// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4515 (ADR 0145 § 2): `Object >> then:`.
//!
//! A literal block's parameter is typed as the receiver's type, and the send
//! answers the block's result type, so a `|>` pipeline that continues after
//! `then:` keeps its types. The rule only describes `Object`'s method: a
//! class that defines its own `then:` keeps its declared signature.

use super::common::*;
use std::collections::HashMap;

/// The inferred method return types and the checker's diagnostics for
/// `source`.
fn check_source(source: &str) -> (HashMap<MethodReturnKey, InferredType>, Vec<Diagnostic>) {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let diagnostics = checker.take_diagnostics();
    (checker.take_method_return_types(), diagnostics)
}

/// The inferred return type of instance method `selector` of `Probe`.
fn probe_return(source: &str, selector: &str) -> (Option<InferredType>, Vec<Diagnostic>) {
    let (types, diags) = check_source(source);
    (
        types
            .get(&("Probe".into(), selector.into(), false))
            .cloned(),
        diags,
    )
}

fn known_name(ty: Option<&InferredType>) -> Option<&str> {
    ty.and_then(InferredType::as_known)
        .map(ecow::EcoString::as_str)
}

fn dnu_messages(diags: &[Diagnostic]) -> Vec<&str> {
    diags
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .map(|d| d.message.as_str())
        .collect()
}

#[test]
fn then_answers_the_block_result_type() {
    let (ty, diags) = probe_return(
        "Object subclass: Probe\n  run => 3 then: [:x | x printString]\n",
        "run",
    );
    assert_eq!(known_name(ty.as_ref()), Some("String"), "{ty:?} {diags:?}");
}

#[test]
fn then_types_the_block_parameter_as_the_receiver() {
    // `x` is `Integer`, so `x size` is a DNU the checker can see, and
    // `x + 1` answers `Integer`.
    let (_, diags) = probe_return(
        "Object subclass: Probe\n  run => 3 then: [:x | x size]\n",
        "run",
    );
    let dnus = dnu_messages(&diags);
    assert_eq!(dnus.len(), 1, "{diags:?}");
    assert!(dnus[0].contains("Integer"), "{dnus:?}");

    let (ty, _) = probe_return(
        "Object subclass: Probe\n  run => 3 then: [:x | x + 1]\n",
        "run",
    );
    assert_eq!(known_name(ty.as_ref()), Some("Integer"), "{ty:?}");
}

#[test]
fn a_pipeline_after_then_keeps_types() {
    // ADR 0145: `|>` after `then:` sends to the block's result, here a
    // `String`, so the next stage resolves on `String`.
    let (ty, diags) = probe_return(
        "Object subclass: Probe\n  run => #(1, 2, 3) |> then: [:xs | xs size] |> printString\n",
        "run",
    );
    assert_eq!(known_name(ty.as_ref()), Some("String"), "{ty:?} {diags:?}");

    // A stage the block's result does not understand is a static DNU on it.
    let (_, diags) = probe_return(
        "Object subclass: Probe\n  run => #(1, 2, 3) |> then: [:xs | xs size] |> notAMessage\n",
        "run",
    );
    let dnus = dnu_messages(&diags);
    assert_eq!(dnus.len(), 1, "{diags:?}");
    assert!(dnus[0].contains("Integer"), "{dnus:?}");
}

#[test]
fn a_pipeline_inside_the_block_sees_the_receiver_type() {
    let (ty, diags) = probe_return(
        "Object subclass: Probe\n  run => \"abc\" then: [:s | s |> size]\n",
        "run",
    );
    assert_eq!(known_name(ty.as_ref()), Some("Integer"), "{ty:?} {diags:?}");
}

#[test]
fn a_nilable_receiver_is_not_narrowed() {
    // `then:` evaluates its block for `nil` too, so the parameter keeps the
    // whole `String | Nil` (unlike `ifNotNil:`, which narrows it).
    let source = "Object subclass: Probe\n  field: name :: String | Nil = nil\n  run => self.name then: [:s | s]\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // The block body's `s`: the last `s` in the source.
    let at = u32::try_from(source.rfind("s]").unwrap()).unwrap();
    let ty = checker
        .type_map()
        .iter()
        .find(|(sp, _)| sp.start() == at && sp.end() == at + 1)
        .map(|(_, ty)| ty.clone());
    let Some(InferredType::Union { members, .. }) = &ty else {
        panic!("expected a union, got: {ty:?}");
    };
    let names: Vec<_> = members.iter().filter_map(InferredType::as_known).collect();
    assert!(names.iter().any(|n| n.as_str() == "String"), "{names:?}");
    assert!(
        names
            .iter()
            .any(|n| matches!(n.as_str(), "UndefinedObject" | "Nil")),
        "{names:?}"
    );
}

#[test]
fn a_non_local_return_in_the_block_is_never() {
    let (ty, diags) = probe_return(
        "Object subclass: Probe\n  run => 3 then: [:x | ^x]\n",
        "run",
    );
    // The send itself never completes: the block's `^` exits `run`.
    assert_eq!(ty, Some(InferredType::Never), "{diags:?}");
}

#[test]
fn a_class_with_its_own_then_keeps_its_signature() {
    // A promise-like class whose block takes a resolved `String`, not the
    // receiver, and which answers a `Promise`.
    let source = "\
Object subclass: Promise
  then: aBlock :: Block(String, Object) -> Promise => self

Object subclass: Probe
  run => Promise new then: [:v | v size]
";
    let (ty, diags) = probe_return(source, "run");
    assert_eq!(known_name(ty.as_ref()), Some("Promise"), "{ty:?} {diags:?}");
    assert!(dnu_messages(&diags).is_empty(), "{diags:?}");
}

#[test]
fn an_own_then_with_a_non_block_parameter_draws_no_override_warning() {
    // A parser combinator's `then:` takes a `Parser`, not a `Block`.
    // `Object >> then:` leaves its parameter untyped, so this is not an
    // incompatible override.
    let source = "\
Object subclass: Parser
  then: next :: Parser -> Parser => next
";
    let (_, diags) = check_source(source);
    assert!(
        !diags
            .iter()
            .any(|d| d.message.contains("incompatible with parent")),
        "{diags:?}"
    );
}

#[test]
fn a_supertyped_receiver_with_an_overriding_subclass_keeps_the_generic_path() {
    // `b :: Base` may hold a `Sub`, whose own `then:` answers a `Sub`, not
    // the block's result. So `Object`'s rule must not type this send.
    let source = "\
Object subclass: Base

Base subclass: Sub
  then: aBlock => self

Object subclass: Probe
  run: b :: Base => b then: [:x | 42]
";
    let (ty, diags) = probe_return(source, "run:");
    assert_ne!(known_name(ty.as_ref()), Some("Integer"), "{ty:?} {diags:?}");
}

#[test]
fn a_dynamic_receiver_keeps_the_generic_path() {
    // An untyped parameter may be a class with its own `then:`, so neither
    // the block parameter nor the result is typed from `Object`'s method.
    let (ty, diags) = probe_return(
        "Object subclass: Probe\n  run: p => p then: [:x | x printString]\n",
        "run:",
    );
    assert_ne!(known_name(ty.as_ref()), Some("String"), "{ty:?} {diags:?}");
}

#[test]
fn a_block_variable_answers_its_declared_result() {
    let source = "\
Object subclass: Probe
  run: b :: Block(Integer, String) => 3 then: b
";
    let (ty, diags) = probe_return(source, "run:");
    assert_eq!(known_name(ty.as_ref()), Some("String"), "{ty:?} {diags:?}");
}

#[test]
fn a_class_receiver_is_understood() {
    // `then:` is an `Object` instance method, which a class object answers
    // too: no DNU on the class side.
    let (_, diags) = probe_return(
        "Object subclass: Probe\n  run => Integer then: [:c | c name]\n",
        "run",
    );
    assert!(dnu_messages(&diags).is_empty(), "{diags:?}");
}
