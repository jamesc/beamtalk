// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2949: `List(E)`/`Dictionary(K, V)` element-type checking on
//! construction/mutation sends (`at:put:`, `++`, ...).
//!
//! Before this fix, `check_argument_types` never substituted a class-level
//! generic type-param name (`K`/`V`/`E`) in a declared parameter type with
//! the receiver's concrete type args — `at:put:`'s declared param type `V`
//! is never a registered hierarchy class, so `is_type_compatible`'s
//! "unknown declared class -> conservatively compatible" escape hatch fired
//! unconditionally, and every element/value argument to a generic
//! collection mutator passed regardless of its real type.

use super::common::*;

use crate::semantic_analysis::alias_registry::{AliasInfo, AliasRegistry};

fn assert_one_expects_diagnostic(diags: &[Diagnostic], expected: &str, actual: &str) {
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type) && d.message.contains("expects"))
        .collect();
    assert_eq!(
        hits.len(),
        1,
        "expected exactly one argument-type diagnostic, got: {diags:#?}"
    );
    assert!(
        hits[0].message.contains(expected) && hits[0].message.contains(actual),
        "diagnostic should mention expected `{expected}` and actual `{actual}`, got: {}",
        hits[0].message
    );
}

fn assert_no_expects_diagnostic(diags: &[Diagnostic]) {
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type) && d.message.contains("expects"))
        .collect();
    assert!(
        hits.is_empty(),
        "expected no argument-type diagnostic, got: {hits:#?}"
    );
}

#[test]
fn dictionary_at_put_incompatible_value_warns() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &eco_string("Dictionary"),
        "at:put:",
        &[InferredType::known("Symbol"), InferredType::known("String")],
        span(),
        &hierarchy,
        false,
        None,
        None,
        &[
            InferredType::known("Symbol"),
            InferredType::known("Integer"),
        ],
    );
    assert_one_expects_diagnostic(checker.diagnostics(), "Integer", "String");
}

#[test]
fn dictionary_at_put_compatible_value_no_warning() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &eco_string("Dictionary"),
        "at:put:",
        &[
            InferredType::known("Symbol"),
            InferredType::known("Integer"),
        ],
        span(),
        &hierarchy,
        false,
        None,
        None,
        &[
            InferredType::known("Symbol"),
            InferredType::known("Integer"),
        ],
    );
    assert_no_expects_diagnostic(checker.diagnostics());
}

#[test]
fn dictionary_at_put_with_no_receiver_type_args_stays_conservative() {
    // A bare (unparameterized) `Dictionary` receiver — e.g. one that erased
    // its type args, or one the checker never inferred args for — must keep
    // today's pre-BT-2949 conservative behavior: no type args to substitute
    // with means no basis for a diagnostic, exactly like every other
    // "unknown declared type" case in this checker.
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &eco_string("Dictionary"),
        "at:put:",
        &[InferredType::known("Symbol"), InferredType::known("String")],
        span(),
        &hierarchy,
        false,
        None,
        None,
        &[],
    );
    assert_no_expects_diagnostic(checker.diagnostics());
}

/// BT-2949 investigation: a `Dictionary` literal's key(s) infer as narrow
/// *singleton* types (`#{#a => 1}` gives `Dictionary(#a, Integer)`, not
/// `Dictionary(Symbol, Integer)`). Substituting `K` (like `V`) would make
/// `at:ifAbsent:` with any differently-keyed lookup — extremely common,
/// correct code — look like a type-arg mismatch. `check_argument_types`
/// must never substitute `K`, only `V`.
#[test]
fn dictionary_at_ifabsent_different_singleton_key_no_warning() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &eco_string("Dictionary"),
        "at:ifAbsent:",
        &[
            InferredType::known("#b"),
            InferredType::known_with_args("Block", vec![InferredType::known("Integer")]),
        ],
        span(),
        &hierarchy,
        false,
        None,
        None,
        &[InferredType::known("#a"), InferredType::known("Integer")],
    );
    assert_no_expects_diagnostic(checker.diagnostics());
}

/// BT-2949 investigation: same false positive, but through
/// `check_variance_in_expr` (`merge:`'s param is the nested
/// `Dictionary(K, V)`) — `d1 merge: d2` where `d1`/`d2` were built from
/// literals with different keys must not warn.
#[test]
fn dictionary_merge_different_singleton_keys_no_warning() {
    let diags = run_with_protocols(
        r"Object subclass: Probe
  check -> Nil =>
    d1 :: Dictionary(Symbol, Integer) := #{#a => 1, #b => 2}
    d2 :: Dictionary(Symbol, Integer) := #{#b => 20, #c => 3}
    d3 := d1 merge: d2
    _u := d3
    nil
",
    );
    assert_no_expects_diagnostic(&diags);
}

// `List(E)`'s `++` is checked by a *different* mechanism than
// `Dictionary(K, V)`'s `at:put:` above: `check_argument_types`'s `Known`-arm
// only ever compares the argument's bare `class_name` (never its own
// `type_args`), so a `List(String)` argument's inner mismatch is invisible
// to it regardless of this fix — that's `check_variance_in_expr`'s job
// (ADR 0068 Phase 2f, `protocol.rs`), which renders the argument's full
// nested type string. These two tests exercise that function directly,
// through `check_module_with_protocols` (the only entry point that runs
// it — plain `check_module` does not, see `check_protocol_conformance_in_module`).

fn list_concat_source(arg_literal: &str) -> String {
    format!(
        r"Object subclass: Probe
  check -> Nil =>
    l :: List(Integer) := #()
    l2 := l ++ {arg_literal}
    _u := l2
    nil
"
    )
}

fn run_with_protocols(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let mut hierarchy = ClassHierarchy::with_builtins();
    let user_hierarchy = ClassHierarchy::build(&module).0.unwrap();
    hierarchy.merge(&user_hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(
        &module,
        &hierarchy,
        &crate::semantic_analysis::protocol_registry::ProtocolRegistry::new(),
    );
    checker.take_diagnostics()
}

#[test]
fn list_concat_incompatible_element_warns() {
    let diags = run_with_protocols(&list_concat_source("#(\"a\", \"b\")"));
    assert_one_expects_diagnostic(&diags, "List(Integer)", "List(String)");
}

#[test]
fn list_concat_compatible_element_no_warning() {
    let diags = run_with_protocols(&list_concat_source("#(1, 2, 3)"));
    assert_no_expects_diagnostic(&diags);
}

/// BT-2949's motivating repro used `Dictionary(Symbol, JsonValue)`, an
/// alias-typed element. Mirrors `type_alias_display_provenance.rs`'s own
/// alias setup to confirm the substituted element type still flows through
/// the existing (BT-2953) alias-expansion machinery unchanged — the
/// substitution produces a plain `Known("RestartStrategy")`, exactly what a
/// *directly* alias-typed parameter would already look like, so the
/// existing "resolve to structural expansion" step downstream applies with
/// no special-casing.
#[test]
fn dictionary_at_put_alias_element_type_expands_and_warns() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut alias_registry = AliasRegistry::new();
    alias_registry.register_test_alias(AliasInfo {
        name: "RestartStrategy".into(),
        annotation: crate::ast::TypeAnnotation::Union {
            types: vec![
                crate::ast::TypeAnnotation::Singleton {
                    name: "temporary".into(),
                    span: span(),
                },
                crate::ast::TypeAnnotation::Singleton {
                    name: "transient".into(),
                    span: span(),
                },
                crate::ast::TypeAnnotation::Singleton {
                    name: "permanent".into(),
                    span: span(),
                },
            ],
            span: span(),
        },
        is_internal: false,
        package: None,
        span: span(),
    });

    let mut checker = TypeChecker::new();
    checker.set_alias_registry(alias_registry);
    checker.check_argument_types(
        &eco_string("Dictionary"),
        "at:put:",
        &[
            InferredType::known("Symbol"),
            InferredType::known("Integer"),
        ],
        span(),
        &hierarchy,
        false,
        None,
        None,
        &[
            InferredType::known("Symbol"),
            InferredType::known("RestartStrategy"),
        ],
    );
    let diags = checker.diagnostics();
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    assert!(
        hits[0].message.contains(
            "expects RestartStrategy (#temporary | #transient | #permanent), got Integer"
        ),
        "should name the alias with its expansion, got: {}",
        hits[0].message
    );
}

/// End-to-end: real source, real `Dictionary`/`List` builtins, through the
/// full `check_module` inference pipeline (not just a direct
/// `check_argument_types` call) — exercises the `inference.rs` call-site
/// wiring that threads the receiver's `type_args` through, not just
/// `check_argument_types` itself. Mirrors the issue's own repro shape
/// (a custom class instance passed where a narrower element type is
/// declared) with plain `Integer` in place of the stdlib `JsonValue` alias,
/// since alias-registry wiring through the full pipeline is a separate,
/// pre-existing concern orthogonal to this fix.
#[test]
fn end_to_end_dictionary_and_list_element_mismatches_are_flagged() {
    let source = r"
Object subclass: Probe
  bad -> Nil =>
    p :: Dictionary(Symbol, Integer) := #{}
    p2 := p at: #a put: self
    _u := p2
    nil

  badList -> Nil =>
    l :: List(Integer) := #()
    l2 := l ++ #(self)
    _u := l2
    nil

  good -> Nil =>
    p :: Dictionary(Symbol, Integer) := #{}
    p2 := p at: #a put: 42
    _u := p2
    nil

  goodList -> Nil =>
    l :: List(Integer) := #()
    l2 := l ++ #(1, 2, 3)
    _u := l2
    nil
";
    let diags = run_with_protocols(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type) && d.message.contains("expects"))
        .collect();
    assert_eq!(
        hits.len(),
        2,
        "expected exactly two argument-type diagnostics (Dictionary at:put: and List ++), got: {diags:#?}"
    );
    assert!(
        hits.iter()
            .any(|d| d.message.contains("at:put:") && d.message.contains("Probe")),
        "should flag the Dictionary at:put: mismatch, got: {hits:#?}"
    );
    assert!(
        hits.iter()
            .any(|d| d.message.contains("'++'") && d.message.contains("List(Integer)")),
        "should flag the List ++ mismatch, got: {hits:#?}"
    );
}

/// BT-2949 investigation: `is_type_compatible`'s singleton branch must treat
/// an unresolved `Dynamic` actual as compatible with any expected singleton,
/// same as every other branch in this function already does via the
/// "unknown class -> conservatively compatible" fallback (`Dynamic` is never
/// a registered hierarchy class). Before this fix, `actual == expected ||
/// is_generic_type_param(actual)` had no such case, so `Dynamic` compared
/// against a singleton like `#x` wrongly failed.
#[test]
fn is_type_compatible_treats_dynamic_actual_as_compatible_with_singleton() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(TypeChecker::is_type_compatible(
        &eco_string("Dynamic"),
        &eco_string("#x"),
        &hierarchy
    ));
}

/// End-to-end repro of the `Result>>andThen:` false positive this fix
/// closes: `andThen:`'s declared `Block(T, Result(R, E))` substitutes `E`
/// (Result's own class type param) with the receiver's concrete `#x`, but
/// `R` is a *method-local* type param the substitution can't resolve and
/// collapses to `Dynamic` — comparing that nested `Dynamic` against the
/// substituted `#x` must not warn.
#[test]
fn result_and_then_block_return_type_arg_with_unresolved_method_local_param_no_warning() {
    let diags = run_with_protocols(
        r"Object subclass: Probe
  check -> Nil =>
    r := (Result error: #x) andThen: [:v | Result ok: v * 2]
    _u := r
    nil
",
    );
    assert_no_expects_diagnostic(&diags);
}
