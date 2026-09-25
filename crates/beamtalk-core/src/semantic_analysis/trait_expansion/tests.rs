// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Unit tests for `trait_expansion` (ADR 0127 §3, BT-3588).

use super::*;
use crate::source_analysis::Severity;

/// Lex + parse a source string into a `Module` (mirrors the same helper in
/// every other `semantic_analysis` test module, e.g.
/// `type_checker::tests::common::parse_source`).
fn parse_source(source: &str) -> Module {
    use crate::source_analysis::{lex_with_eof, parse};
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "fixture source failed to parse cleanly: {diagnostics:?}\nsource:\n{source}"
    );
    module
}

fn find_class<'a>(module: &'a Module, name: &str) -> &'a ClassDefinition {
    module
        .classes
        .iter()
        .find(|c| c.name.name == name)
        .unwrap_or_else(|| panic!("no class named {name} in module"))
}

fn find_method<'a>(class: &'a ClassDefinition, selector: &str) -> Option<&'a MethodDefinition> {
    class.methods.iter().find(|m| m.selector.name() == selector)
}

// ── class-wins (ADR 0127 §3 step 4) ─────────────────────────────────────

#[test]
fn class_body_wins_over_a_single_provision() {
    let mut module = parse_source(
        "Protocol define: Describable
  printString -> String => \"a describable thing\"

Value subclass: Report
  uses: Describable

  printString -> String => \"my own printString\"",
    );

    let (diagnostics, origins) = expand_module(&mut module);
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:?}"
    );
    assert!(
        origins.is_empty(),
        "class's own method must not get an origin"
    );

    let report = find_class(&module, "Report");
    let print_string_methods: Vec<_> = report
        .methods
        .iter()
        .filter(|m| m.selector.name() == "printString")
        .collect();
    assert_eq!(
        print_string_methods.len(),
        1,
        "the class's own printString must not be duplicated by the provision"
    );
}

#[test]
fn synthesised_value_accessor_ranks_as_class_body_too() {
    // ADR 0127 §3, "Synthesised methods rank as class body": a `field:`
    // slot's auto-generated getter isn't written in `class.methods` at all
    // (it's synthesised later, at `ClassHierarchy::build` time) — this pass
    // must still treat `size` as owned by the class because of the slot,
    // not just because of a hand-written method.
    let mut module = parse_source(
        "Protocol define: Sized
  size -> Integer => 0

Value subclass: Bucket
  uses: Sized
  field: size :: Integer = 0",
    );

    let (diagnostics, origins) = expand_module(&mut module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert!(
        origins.is_empty(),
        "the field's synthesised accessor wins, not the provision"
    );

    let bucket = find_class(&module, "Bucket");
    assert!(
        find_method(bucket, "size").is_none(),
        "size must not be flattened in — the class's own field beats the provision"
    );
}

#[test]
fn class_wins_suppresses_conflict_between_two_providers_too() {
    // ADR 0127 §4: the class's own definition beats *both* provisions, so
    // no exclusion — and no conflict error — is needed (the ADR's own
    // `Report` example).
    let mut module = parse_source(
        "Protocol define: Labelled
  printString -> String => \"labelled\"

Protocol define: Describable
  printString -> String => \"describable\"

Value subclass: Report
  uses: Labelled
  uses: Describable

  printString -> String => \"mine\"",
    );

    let (diagnostics, _origins) = expand_module(&mut module);
    assert!(
        diagnostics.is_empty(),
        "class-wins must suppress the conflict entirely: {diagnostics:?}"
    );
}

// ── conflict detection (ADR 0127 §4) ────────────────────────────────────

#[test]
fn two_protocols_providing_the_same_selector_conflict() {
    let mut module = parse_source(
        "Protocol define: Labelled
  printString -> String => \"labelled\"

Protocol define: Describable
  printString -> String => \"describable\"

Value subclass: Report
  uses: Labelled
  uses: Describable",
    );

    let (diagnostics, origins) = expand_module(&mut module);
    assert_eq!(
        diagnostics.len(),
        1,
        "expected exactly one conflict error: {diagnostics:?}"
    );
    let diag = &diagnostics[0];
    assert_eq!(diag.severity, Severity::Error);
    assert!(diag.message.contains("printString"));
    assert!(diag.message.contains("Labelled"));
    assert!(diag.message.contains("Describable"));
    assert!(diag.message.contains("Report"));

    let report = find_class(&module, "Report");
    assert!(
        find_method(report, "printString").is_none(),
        "a conflicting selector must not be flattened from either side"
    );
    assert!(origins.is_empty());
}

// ── `excluding:` (ADR 0127 §3 step 2) ───────────────────────────────────

#[test]
fn excluding_drops_just_that_selector() {
    let mut module = parse_source(
        "Protocol define: Describable
  printString -> String => \"a thing\"
  summary -> String => \"a summary\"

Value subclass: Report
  uses: Describable excluding: #(#printString)",
    );

    let (diagnostics, origins) = expand_module(&mut module);
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:?}"
    );

    let report = find_class(&module, "Report");
    assert!(
        find_method(report, "printString").is_none(),
        "printString was excluded"
    );
    assert!(
        find_method(report, "summary").is_some(),
        "summary was not excluded"
    );
    assert_eq!(
        origins.get(&(EcoString::from("Report"), EcoString::from("summary"))),
        Some(&EcoString::from("Describable"))
    );
    assert!(!origins.contains_key(&(EcoString::from("Report"), EcoString::from("printString"))));
}

#[test]
fn excluding_resolves_a_conflict_between_two_providers() {
    let mut module = parse_source(
        "Protocol define: Labelled
  printString -> String => \"labelled\"

Protocol define: Describable
  printString -> String => \"describable\"

Value subclass: Report
  uses: Labelled
  uses: Describable excluding: #(#printString)",
    );

    let (diagnostics, origins) = expand_module(&mut module);
    assert!(
        diagnostics.is_empty(),
        "excluding one side must resolve the conflict: {diagnostics:?}"
    );

    let report = find_class(&module, "Report");
    assert!(find_method(report, "printString").is_some());
    assert_eq!(
        origins.get(&(EcoString::from("Report"), EcoString::from("printString"))),
        Some(&EcoString::from("Labelled"))
    );
}

// ── the body-less-protocol hint (ADR 0127 §1, §Status 6) ────────────────

#[test]
fn uses_of_a_protocol_with_no_provisions_is_a_hint_not_an_error() {
    let mut module = parse_source(
        "Protocol define: Printable
  asString -> String

Value subclass: Report
  uses: Printable",
    );

    let (diagnostics, origins) = expand_module(&mut module);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].severity, Severity::Hint);
    assert!(diagnostics[0].message.contains("Printable"));
    assert!(
        diagnostics[0]
            .message
            .contains("only checks its requirements")
    );
    assert!(origins.is_empty());

    let report = find_class(&module, "Report");
    assert!(report.methods.is_empty());
}

// ── unknown protocol ─────────────────────────────────────────────────────

#[test]
fn uses_of_an_unknown_protocol_is_an_error() {
    let mut module = parse_source(
        "Value subclass: Report
  uses: NoSuchProtocol",
    );

    let (diagnostics, _origins) = expand_module(&mut module);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].severity, Severity::Error);
    assert!(diagnostics[0].message.contains("NoSuchProtocol"));
}

// ── `Self` substitution (ADR 0127 §1) ───────────────────────────────────

#[test]
fn self_in_a_provision_becomes_the_using_class_non_generic() {
    let mut module = parse_source(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]

Value subclass: Version
  uses: Comparable
  field: major :: Integer = 0

  < other :: Version -> Boolean => self.major < other major",
    );

    let (diagnostics, origins) = expand_module(&mut module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let version = find_class(&module, "Version");
    let max = find_method(version, "max:").expect("max: was flattened in");
    assert_eq!(max.parameters.len(), 1);
    assert_eq!(
        max.parameters[0]
            .type_annotation
            .as_ref()
            .map(TypeAnnotation::type_name),
        Some(EcoString::from("Version"))
    );
    assert_eq!(
        max.return_type.as_ref().map(TypeAnnotation::type_name),
        Some(EcoString::from("Version"))
    );
    assert_eq!(
        origins.get(&(EcoString::from("Version"), EcoString::from("max:"))),
        Some(&EcoString::from("Comparable"))
    );
}

#[test]
fn self_in_a_provision_becomes_the_generic_user_applied_to_its_own_params() {
    let mut module = parse_source(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]

Value subclass: Pair(A, B)
  uses: Comparable
  field: first :: A
  field: second :: B

  < other :: Pair(A, B) -> Boolean => self.first < other first",
    );

    let (diagnostics, _origins) = expand_module(&mut module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let pair = find_class(&module, "Pair");
    let max = find_method(pair, "max:").expect("max: was flattened in");
    let Some(TypeAnnotation::Generic {
        base, parameters, ..
    }) = max.parameters[0].type_annotation.as_ref()
    else {
        panic!(
            "expected max:'s parameter to be a generic Pair(A, B), got {:?}",
            max.parameters[0].type_annotation
        );
    };
    assert_eq!(base.name, "Pair");
    assert_eq!(parameters.len(), 2);
    assert_eq!(parameters[0].type_name(), "A");
    assert_eq!(parameters[1].type_name(), "B");

    let Some(TypeAnnotation::Generic { base: rt_base, .. }) = max.return_type.as_ref() else {
        panic!(
            "expected max:'s return type to be a generic Pair(A, B), got {:?}",
            max.return_type
        );
    };
    assert_eq!(rt_base.name, "Pair");
}

// ── type-arg substitution + method-local hygiene (ADR 0127 §3) ─────────

#[test]
fn uses_type_arg_substitutes_the_protocols_type_param() {
    let mut module = parse_source(
        "Protocol define: Enumerable(E)
  elements -> List(E)

  size -> Integer => self elements size

Actor subclass: WorkerPool
  uses: Enumerable(Worker)

  elements -> List(Worker) => self.workers",
    );

    let (diagnostics, _origins) = expand_module(&mut module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    // `size` carries no E-typed signature, so this mainly proves the
    // provision made it across; the real substitution check is the
    // hygiene test below (`inject:into:`'s `Block(A, E, A)`).
    let pool = find_class(&module, "WorkerPool");
    assert!(find_method(pool, "size").is_some());
}

#[test]
fn method_local_type_var_colliding_with_the_class_type_param_is_alpha_renamed() {
    // ADR 0127 §3, "Type-parameter substitution and hygiene":
    // `Pair(A, B) uses: Enumerable(A)` must not let `inject:into:`'s own
    // method-local `A` collapse into the class's `A`.
    let mut module = parse_source(
        "Protocol define: Enumerable(E)
  elements -> List(E)

  inject: initial :: A into: block :: Block(A, E, A) -> A => self elements inject: initial into: block

Value subclass: Pair(A, B)
  uses: Enumerable(A)
  field: first :: A
  field: second :: B

  elements -> List(A) => List with: self.first",
    );

    let (diagnostics, _origins) = expand_module(&mut module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let pair = find_class(&module, "Pair");
    let inject = find_method(pair, "inject:into:").expect("inject:into: was flattened in");
    assert_eq!(inject.parameters.len(), 2);

    // `initial`'s local `A` was renamed (it collided with class param `A`);
    // the renamed letter must not be `A` or `B` (both taken by the class).
    let initial_ty = inject.parameters[0]
        .type_annotation
        .as_ref()
        .expect("initial has a type annotation");
    let TypeAnnotation::Simple(initial_id) = initial_ty else {
        panic!("expected initial's type to be a Simple identifier, got {initial_ty:?}");
    };
    assert_ne!(initial_id.name, "A");
    assert_ne!(initial_id.name, "B");
    let fresh = initial_id.name.clone();

    // `block`'s `Block(A, E, A)` becomes `Block(<fresh>, A, <fresh>)` — `E`
    // substituted to the class's own `A` (the `uses: Enumerable(A)` type
    // arg), and both local-`A` occurrences renamed identically.
    let block_ty = inject.parameters[1]
        .type_annotation
        .as_ref()
        .expect("block has a type annotation");
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = block_ty
    else {
        panic!("expected block's type to be Generic Block(...), got {block_ty:?}");
    };
    assert_eq!(base.name, "Block");
    assert_eq!(parameters.len(), 3);
    assert_eq!(parameters[0].type_name(), fresh);
    assert_eq!(
        parameters[1].type_name(),
        "A",
        "E must substitute to the uses: type arg A"
    );
    assert_eq!(parameters[2].type_name(), fresh);

    // Return type `A` (method-local) also renamed to the same fresh letter.
    assert_eq!(
        inject.return_type.as_ref().map(TypeAnnotation::type_name),
        Some(fresh)
    );
}

// ── `extending:` a protocol with provisions (ADR 0127 §5, §8) ──────────

#[test]
fn extending_a_protocol_with_provisions_does_not_flatten_the_parents_provisions() {
    // `uses: Q` only flattens Q's *own* provisions — a protocol using
    // another protocol (Q composing P's bodies) is post-v1 (§Status 8).
    // `extending:` only widens Q's *type* (checked in protocol_registry.rs),
    // never brings P's bodies along.
    let mut module = parse_source(
        "Protocol define: Describable
  printString -> String => \"described\"

Protocol define: Labelled
  extending: Describable
  label -> String => \"labelled\"

Value subclass: Report
  uses: Labelled",
    );

    let (diagnostics, _origins) = expand_module(&mut module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let report = find_class(&module, "Report");
    assert!(
        find_method(report, "label").is_some(),
        "Labelled's own provision flattens in"
    );
    assert!(
        find_method(report, "printString").is_none(),
        "Describable's provision must NOT flatten in just because Labelled extends it"
    );
}

// ── deterministic output order ──────────────────────────────────────────

#[test]
fn flattened_methods_are_appended_in_deterministic_selector_order() {
    // `merged` (the per-class collection of surviving provisions) is a
    // `HashMap`, whose iteration order is randomised per process
    // (`RandomState`) — the splice into `class.methods` must not depend on
    // it, or codegen's emitted function order (and anything else that reads
    // `class.methods` positionally) would vary across compiler runs of the
    // exact same source.
    let mut module = parse_source(
        "Protocol define: Multi
  zebra -> String => \"z\"
  apple -> String => \"a\"
  mango -> String => \"m\"

Value subclass: Fruit
  uses: Multi",
    );

    let (diagnostics, _origins) = expand_module(&mut module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let fruit = find_class(&module, "Fruit");
    let actual: Vec<String> = fruit
        .methods
        .iter()
        .map(|m| m.selector.name().to_string())
        .collect();
    assert_eq!(
        actual,
        vec!["apple", "mango", "zebra"],
        "must be selector-sorted, not hash order"
    );
}

// ── no-op on a class with no `uses:` ─────────────────────────────────────

#[test]
fn class_with_no_uses_is_untouched() {
    let mut module = parse_source(
        "Value subclass: Plain
  field: x :: Integer = 0",
    );
    let before = module.clone();
    let (diagnostics, origins) = expand_module(&mut module);
    assert!(diagnostics.is_empty());
    assert!(origins.is_empty());
    assert_eq!(module, before);
}
