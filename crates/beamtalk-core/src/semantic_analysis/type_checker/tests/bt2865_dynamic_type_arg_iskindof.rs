// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `isKindOf:` (and other dead-code "can never be true" comparison analysis)
//! must never fire when the receiver's static type is genuinely `Dynamic`,
//! including when `Dynamic` is nested as one type-arg of an otherwise-
//! parameterized generic like `Result(Dynamic, Error)` (BT-2865).
//!
//! Root cause (two compounding bugs, both fixed here):
//!
//! 1. `TypeAnnotation::Simple("Dynamic")` resolved to a *fake*
//!    `Known{class_name: "Dynamic"}` pseudo-class instead of the real
//!    `InferredType::Dynamic` variant — so every check written against
//!    `matches!(ty, InferredType::Dynamic(_))` (dead-code analysis, the
//!    "Known survives Dynamic" merge rule, etc.) silently failed to
//!    recognise it. Invisible for a *bare* `Result` (T/E never resolved, so
//!    this path never ran) — only surfaced once a param was partially
//!    parameterized (`Result(Dynamic, Error)`).
//! 2. Even with (1) fixed, `merge_method_local_binding` — which unifies a
//!    method-local type param (e.g. `R` in `ifOk:ifError:`) across multiple
//!    block-argument positions — only protected one arrival order: an
//!    existing Known binding survived an incoming *uninformative* Dynamic
//!    (untyped FFI), but an existing *meaningful* Dynamic binding (from
//!    `ifOk:`'s block, genuinely `Dynamic` because `T` is `Dynamic`) was
//!    silently overwritten by a later Known binding from `ifError:`'s block.

use super::common::*;
use std::collections::HashMap;

/// AC / exact repro shape: `Result(Dynamic, Error)>>ifOk:ifError:` — the ok
/// branch's block param is genuinely `Dynamic` (`T` = `Dynamic`), the error
/// branch returns a concrete `Nil`. `config`'s inferred type must stay
/// `Dynamic`, so `(config isKindOf: Dictionary)` must not be flagged as
/// "can never be true".
#[test]
fn iskindof_on_dynamic_type_arg_from_partially_parameterized_generic_stays_silent() {
    let source = "\
typed Value subclass: MyResult(T, E)\n\
  ifOk: okBlock :: Block(T, R) ifError: errorBlock :: Block(E, R) -> R => okBlock value: nil\n\
typed Object subclass: Repro\n\
  check: r :: MyResult(Dynamic, String) -> Object =>\n\
    config := r ifOk: [:v | v] ifError: [:e | nil]\n\
    (config isKindOf: Dictionary) ifFalse: [ 1 ]\n\
    2\n";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let impossible: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("can never be true"))
        .collect();
    assert!(
        impossible.is_empty(),
        "config's static type is genuinely Dynamic (from MyResult(Dynamic, E)'s T) — \
         isKindOf: must stay silent, got: {impossible:?}"
    );
}

/// `resolve_type_annotation` (AST-based) must resolve a bare `Dynamic`
/// annotation to the real `InferredType::Dynamic` variant, not a
/// `Known{class_name: "Dynamic"}` pseudo-class.
#[test]
fn resolve_type_annotation_dynamic_keyword_resolves_to_dynamic_variant() {
    let ann = TypeAnnotation::Simple(ident("Dynamic"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert!(
        matches!(result, InferredType::Dynamic(_)),
        "expected the real Dynamic variant, got: {result:?}"
    );
}

/// `resolve_type_annotation` must resolve `Dynamic` correctly even nested
/// inside a generic's type args (the exact repro shape).
#[test]
fn resolve_type_annotation_dynamic_nested_in_generic_resolves_to_dynamic_variant() {
    let ann = TypeAnnotation::Generic {
        base: ident("MyResult"),
        parameters: vec![
            TypeAnnotation::Simple(ident("Dynamic")),
            TypeAnnotation::Simple(ident("Error")),
        ],
        span: span(),
    };
    let result = TypeChecker::resolve_type_annotation(&ann);
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = result
    else {
        panic!("expected a Known generic type, got: {result:?}");
    };
    assert_eq!(class_name.as_str(), "MyResult");
    assert!(
        matches!(type_args.first(), Some(InferredType::Dynamic(_))),
        "T should resolve to the real Dynamic variant, got: {type_args:?}"
    );
    assert_eq!(
        type_args
            .get(1)
            .and_then(InferredType::as_known)
            .map(ecow::EcoString::as_str),
        Some("Error")
    );
}

/// `resolve_type_name_string` (the state-field-type string path) must also
/// resolve a bare `Dynamic` to the real `InferredType::Dynamic` variant.
#[test]
fn resolve_type_name_string_dynamic_keyword_resolves_to_dynamic_variant() {
    let result = TypeChecker::resolve_type_name_string(&eco_string("Dynamic"));
    assert!(
        matches!(result, InferredType::Dynamic(_)),
        "expected the real Dynamic variant, got: {result:?}"
    );
}

/// `resolve_type_name_string` must resolve `Dynamic` correctly nested inside
/// a generic's type args too.
#[test]
fn resolve_type_name_string_dynamic_nested_in_generic_resolves_to_dynamic_variant() {
    let result = TypeChecker::resolve_type_name_string(&eco_string("MyResult(Dynamic, Error)"));
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = result
    else {
        panic!("expected a Known generic type, got: {result:?}");
    };
    assert_eq!(class_name.as_str(), "MyResult");
    assert!(
        matches!(type_args.first(), Some(InferredType::Dynamic(_))),
        "T should resolve to the real Dynamic variant, got: {type_args:?}"
    );
}

/// `merge_method_local_binding`: an existing *explicitly declared* Dynamic
/// binding must survive an incoming Known binding — it's authoritative, not
/// last-wins overwrite. This is the exact ordering that broke before the
/// fix: `ifOk:`'s Dynamic binding (from `T = Dynamic`) lands first,
/// `ifError:`'s concrete `Nil` binding lands second.
#[test]
fn merge_method_local_binding_explicit_dynamic_then_known_stays_dynamic() {
    let mut subst: HashMap<ecow::EcoString, InferredType> = HashMap::new();
    let key: ecow::EcoString = "R".into();
    TypeChecker::merge_method_local_binding(
        &mut subst,
        key.clone(),
        InferredType::Dynamic(DynamicReason::ExplicitDynamic),
    );
    TypeChecker::merge_method_local_binding(&mut subst, key.clone(), InferredType::known("Nil"));
    assert!(
        matches!(
            subst.get(&key),
            Some(InferredType::Dynamic(DynamicReason::ExplicitDynamic))
        ),
        "a meaningful Dynamic binding must not be overwritten by a later Known one, got: {:?}",
        subst.get(&key)
    );
}

/// Regression guard: the broad BT-2039 rule (any non-`ExplicitDynamic`
/// reason loses to Known) must cover ordinary `UnannotatedParam`, not just
/// the narrower `UntypedFfi`/`DynamicSpec` — this is the exact shape that
/// regressed `stdlib/src/SystemNavigation.bt`'s `inject:into:` block during
/// this fix's development: an accumulator's `ifTrue:`/`ifFalse:` branches
/// where one arm reassigns to an unannotated value and the other returns the
/// existing (Known) accumulator.
#[test]
fn merge_method_local_binding_known_then_unannotated_param_dynamic_keeps_known() {
    let mut subst: HashMap<ecow::EcoString, InferredType> = HashMap::new();
    let key: ecow::EcoString = "A".into();
    TypeChecker::merge_method_local_binding(&mut subst, key.clone(), InferredType::known("List"));
    TypeChecker::merge_method_local_binding(
        &mut subst,
        key.clone(),
        InferredType::Dynamic(DynamicReason::UnannotatedParam),
    );
    assert_eq!(
        subst
            .get(&key)
            .and_then(InferredType::as_known)
            .map(ecow::EcoString::as_str),
        Some("List"),
        "Known must survive an ordinary UnannotatedParam Dynamic too, not just \
         UntypedFfi/DynamicSpec, got: {:?}",
        subst.get(&key)
    );
}

/// Regression guard (BT-2039, unchanged): an existing Known binding must
/// still survive an incoming *uninformative* (untyped-FFI) Dynamic.
#[test]
fn merge_method_local_binding_known_then_uninformative_dynamic_keeps_known() {
    let mut subst: HashMap<ecow::EcoString, InferredType> = HashMap::new();
    let key: ecow::EcoString = "R".into();
    TypeChecker::merge_method_local_binding(
        &mut subst,
        key.clone(),
        InferredType::known("Integer"),
    );
    TypeChecker::merge_method_local_binding(
        &mut subst,
        key.clone(),
        InferredType::Dynamic(DynamicReason::UntypedFfi),
    );
    assert_eq!(
        subst
            .get(&key)
            .and_then(InferredType::as_known)
            .map(ecow::EcoString::as_str),
        Some("Integer"),
        "BT-2039: Known must survive an uninformative untyped-FFI Dynamic, got: {:?}",
        subst.get(&key)
    );
}

/// Regression guard (BT-2039, unchanged): the reverse order — uninformative
/// Dynamic first, Known second — must also keep the Known binding.
#[test]
fn merge_method_local_binding_uninformative_dynamic_then_known_keeps_known() {
    let mut subst: HashMap<ecow::EcoString, InferredType> = HashMap::new();
    let key: ecow::EcoString = "R".into();
    TypeChecker::merge_method_local_binding(
        &mut subst,
        key.clone(),
        InferredType::Dynamic(DynamicReason::UntypedFfi),
    );
    TypeChecker::merge_method_local_binding(
        &mut subst,
        key.clone(),
        InferredType::known("Integer"),
    );
    assert_eq!(
        subst
            .get(&key)
            .and_then(InferredType::as_known)
            .map(ecow::EcoString::as_str),
        Some("Integer"),
        "BT-2039 (reverse order): Known must survive an uninformative untyped-FFI Dynamic, got: {:?}",
        subst.get(&key)
    );
}

/// Two different concrete bindings for the same type param unify via union,
/// rather than last-wins silently discarding one.
#[test]
fn merge_method_local_binding_two_known_types_unify() {
    let mut subst: HashMap<ecow::EcoString, InferredType> = HashMap::new();
    let key: ecow::EcoString = "R".into();
    TypeChecker::merge_method_local_binding(
        &mut subst,
        key.clone(),
        InferredType::known("Integer"),
    );
    TypeChecker::merge_method_local_binding(&mut subst, key.clone(), InferredType::known("String"));
    let InferredType::Union { members, .. } = subst.get(&key).unwrap() else {
        panic!(
            "expected a Union of Integer and String, got: {:?}",
            subst.get(&key)
        );
    };
    let names: Vec<_> = members
        .iter()
        .filter_map(InferredType::as_known)
        .map(ecow::EcoString::as_str)
        .collect();
    assert!(
        names.contains(&"Integer") && names.contains(&"String"),
        "got: {names:?}"
    );
}
