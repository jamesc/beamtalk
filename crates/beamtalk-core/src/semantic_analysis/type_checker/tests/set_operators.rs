// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Normalisation tests for the set-theoretic type operators `intersect` and
//! `difference`, plus the `Negation` variant and `union_of`'s absorption law.
//!
//! Covers ADR 0102 §1 exactly: dedup, the full absorption-law set (restoration,
//! partial, same-base complement union), LHS/RHS distribution and fold,
//! same-base flattening, exact-generics matching, symbol-singleton membership,
//! the nominal-class base case (via the class hierarchy), canonical `excluded`
//! ordering, intersect-through-a-complement, the Dynamic asymmetry / rule-priority
//! regression, commutativity, and termination on arbitrary inputs.

use super::super::*;
use super::common::*;

use crate::semantic_analysis::ClassHierarchy;
use proptest::prelude::*;

// ── Fixtures ────────────────────────────────────────────────────────────

fn prov() -> TypeProvenance {
    TypeProvenance::Inferred(Span::default())
}

fn symbol() -> InferredType {
    InferredType::known("Symbol")
}

fn object() -> InferredType {
    InferredType::known("Object")
}

fn dynamic() -> InferredType {
    InferredType::Dynamic(DynamicReason::Unknown)
}

/// A symbol singleton `#name` (the type checker's singleton convention).
fn singleton(name: &str) -> InferredType {
    InferredType::known(name)
}

/// `List(arg)` — a single-argument generic, used for exact-generics tests.
fn list_of(arg: InferredType) -> InferredType {
    InferredType::known_with_args("List", vec![arg])
}

/// Builds the canonical `Symbol \ excluded` negation directly.
fn negation(excluded: InferredType) -> InferredType {
    InferredType::Negation {
        base: Box::new(symbol()),
        excluded: Box::new(excluded),
        provenance: prov(),
    }
}

/// The built-in class hierarchy — carries `Integer <: Number`, `String`, etc.,
/// so the nominal-class base case of `intersect` can be exercised.
fn hierarchy() -> ClassHierarchy {
    ClassHierarchy::with_builtins()
}

// ── intersect: base rules ───────────────────────────────────────────────

#[test]
fn intersect_same_type_is_identity() {
    let t = InferredType::known("Integer");
    assert_eq!(InferredType::intersect(&t, &t, prov(), None, None), t);
}

#[test]
fn intersect_with_never_is_never() {
    let t = InferredType::known("Integer");
    assert_eq!(
        InferredType::intersect(&t, &InferredType::Never, prov(), None, None),
        InferredType::Never
    );
    assert_eq!(
        InferredType::intersect(&InferredType::Never, &t, prov(), None, None),
        InferredType::Never
    );
}

#[test]
fn intersect_with_object_is_identity() {
    let t = InferredType::known("Integer");
    assert_eq!(
        InferredType::intersect(&t, &object(), prov(), None, None),
        t
    );
    assert_eq!(
        InferredType::intersect(&object(), &t, prov(), None, None),
        t
    );
}

#[test]
fn intersect_distinct_concrete_types_are_disjoint() {
    // Structural disjointness (no hierarchy here): distinct classes → Never.
    assert_eq!(
        InferredType::intersect(
            &InferredType::known("Integer"),
            &singleton("#infinity"),
            prov(),
            None,
            None
        ),
        InferredType::Never
    );
}

#[test]
fn intersect_lhs_union_distributes_to_bare_singleton() {
    // ADR 0102 §1: `intersect(Integer | #infinity, #infinity)` normalises to
    // the *bare* singleton `#infinity`, never a stored wrapper.
    let lhs = InferredType::union_of(&[InferredType::known("Integer"), singleton("#infinity")]);
    let result = InferredType::intersect(&lhs, &singleton("#infinity"), prov(), None, None);
    assert_eq!(result, singleton("#infinity"));
    assert!(
        matches!(result, InferredType::Known { .. }),
        "expected a bare Known singleton, got {result:?}"
    );
}

#[test]
fn intersect_rhs_union_folds() {
    // `intersect(T, A | B) = union_of(intersect(T,A), intersect(T,B))`.
    let t = InferredType::union_of(&[InferredType::known("Integer"), singleton("#a")]);
    let rhs = InferredType::union_of(&[singleton("#a"), singleton("#b")]);
    let result = InferredType::intersect(&t, &rhs, prov(), None, None);
    // `#a` survives (present on both sides); `#b`/`Integer` are disjoint → gone.
    assert_eq!(result, singleton("#a"));
}

// ── intersect: nominal-class base case (GAP 1, ADR 0102 §1) ──────────────

#[test]
fn intersect_nominal_subclass_reduces_to_subclass() {
    // `Number ∩ Integer = Integer` (B <: A ⇒ B), symmetrically A <: B ⇒ A.
    // Requires a hierarchy — `Integer <: Number` in the builtins.
    let h = hierarchy();
    let number = InferredType::known("Number");
    let integer = InferredType::known("Integer");
    assert_eq!(
        InferredType::intersect(&number, &integer, prov(), Some(&h), None),
        integer
    );
    assert_eq!(
        InferredType::intersect(&integer, &number, prov(), Some(&h), None),
        integer
    );
}

#[test]
fn intersect_nominal_unrelated_is_never() {
    // Hierarchy-unrelated classes are disjoint (single inheritance):
    // `Integer ∩ String = Never`, `Integer ∩ #foo = Never`.
    let h = hierarchy();
    let integer = InferredType::known("Integer");
    let string = InferredType::known("String");
    assert_eq!(
        InferredType::intersect(&integer, &string, prov(), Some(&h), None),
        InferredType::Never
    );
    assert_eq!(
        InferredType::intersect(&integer, &singleton("#foo"), prov(), Some(&h), None),
        InferredType::Never
    );
}

#[test]
fn intersect_symbol_singleton_preserved_with_hierarchy() {
    // The `Symbol ∩ #foo = #foo` behaviour survives the nominal path —
    // singletons are not hierarchy entries, so the explicit symbol arms win.
    let h = hierarchy();
    assert_eq!(
        InferredType::intersect(&symbol(), &singleton("#foo"), prov(), Some(&h), None),
        singleton("#foo")
    );
    assert_eq!(
        InferredType::intersect(&singleton("#foo"), &symbol(), prov(), Some(&h), None),
        singleton("#foo")
    );
}

#[test]
fn intersect_without_hierarchy_keeps_distinct_classes_disjoint() {
    // GAP 1: `None` preserves the old structural-only behaviour — even a real
    // subclass relation is invisible without a hierarchy.
    let number = InferredType::known("Number");
    let integer = InferredType::known("Integer");
    assert_eq!(
        InferredType::intersect(&number, &integer, prov(), None, None),
        InferredType::Never
    );
}

// ── intersect: rule priority (Dynamic first) ────────────────────────────

#[test]
fn intersect_dynamic_is_checked_before_object() {
    // Gap 1: the `Dynamic` arm must win over the generic `T ∩ Object = T`
    // identity — `intersect(Dynamic, Object)` refines to `Object`, not `Dynamic`.
    assert_eq!(
        InferredType::intersect(&dynamic(), &object(), prov(), None, None),
        object()
    );
    assert_eq!(
        InferredType::intersect(&object(), &dynamic(), prov(), None, None),
        object()
    );
}

// ── intersect: symbol-singleton membership (#foo <: Symbol) ─────────────

#[test]
fn intersect_symbol_with_singleton_keeps_singleton() {
    // A symbol singleton is a subtype of `Symbol`, so the narrower singleton
    // is kept in both orders.
    assert_eq!(
        InferredType::intersect(&symbol(), &singleton("#foo"), prov(), None, None),
        singleton("#foo")
    );
    assert_eq!(
        InferredType::intersect(&singleton("#foo"), &symbol(), prov(), None, None),
        singleton("#foo")
    );
}

#[test]
fn intersect_distinct_singletons_are_disjoint() {
    // Distinct singleton disjointness is preserved.
    assert_eq!(
        InferredType::intersect(&singleton("#foo"), &singleton("#bar"), prov(), None, None),
        InferredType::Never
    );
}

/// BT-2764: with a hierarchy, a nominal *supertype* of `Symbol` other than
/// `Object` (in the builtin hierarchy: `ProtoObject`) also admits singletons —
/// `ProtoObject ∩ #foo = #foo` in both orders, matching the pre-ADR-0102
/// hierarchy walk. Without this, the nominal arm would answer `Never`
/// (`is_nominal_subtype(h, "#foo", "ProtoObject")` is false — singletons are
/// never hierarchy entries).
#[test]
fn intersect_symbol_supertype_with_singleton_keeps_singleton() {
    let h = hierarchy();
    let proto = InferredType::known("ProtoObject");
    assert_eq!(
        InferredType::intersect(&proto, &singleton("#foo"), prov(), Some(&h), None),
        singleton("#foo")
    );
    assert_eq!(
        InferredType::intersect(&singleton("#foo"), &proto, prov(), Some(&h), None),
        singleton("#foo")
    );
}

/// BT-2764: without a hierarchy there is no way to know `ProtoObject` sits
/// above `Symbol`, so the pre-existing conservative fallback (`Never`) is
/// pinned — only bare `Symbol` (and top `Object`) admit singletons
/// hierarchy-free.
#[test]
fn intersect_symbol_supertype_with_singleton_without_hierarchy_is_never() {
    let proto = InferredType::known("ProtoObject");
    assert_eq!(
        InferredType::intersect(&proto, &singleton("#foo"), prov(), None, None),
        InferredType::Never
    );
}

/// BT-2764: the hierarchy-aware singleton arm must not leak to classes that
/// are *not* supertypes of `Symbol` — `Integer ∩ #foo` stays `Never` with the
/// hierarchy threaded.
#[test]
fn intersect_non_symbol_supertype_with_singleton_stays_never() {
    let h = hierarchy();
    assert_eq!(
        InferredType::intersect(
            &InferredType::known("Integer"),
            &singleton("#foo"),
            prov(),
            Some(&h),
            None
        ),
        InferredType::Never
    );
}

// ── intersect: through a complement ─────────────────────────────────────

#[test]
fn intersect_complement_keeps_unexcluded_singleton() {
    // `intersect(Symbol \ #foo, #bar) = #bar` (#bar is a symbol, not excluded).
    let neg = negation(singleton("#foo"));
    assert_eq!(
        InferredType::intersect(&neg, &singleton("#bar"), prov(), None, None),
        singleton("#bar")
    );
}

#[test]
fn intersect_complement_excludes_matching_singleton() {
    // `intersect(Symbol \ #foo, #foo) = Never` (#foo is excluded).
    let neg = negation(singleton("#foo"));
    assert_eq!(
        InferredType::intersect(&neg, &singleton("#foo"), prov(), None, None),
        InferredType::Never
    );
}

#[test]
fn intersect_complement_with_union() {
    // `intersect(Symbol \ #foo, #foo | #bar | #baz) = #bar | #baz`.
    let neg = negation(singleton("#foo"));
    let rhs = InferredType::union_of(&[singleton("#foo"), singleton("#bar"), singleton("#baz")]);
    let result = InferredType::intersect(&neg, &rhs, prov(), None, None);
    assert_eq!(
        result,
        InferredType::union_of(&[singleton("#bar"), singleton("#baz")])
    );
}

#[test]
fn intersect_complement_lhs_is_symmetric() {
    // `intersect(#bar, Symbol \ #foo) = #bar` — Negation on the RHS too.
    let neg = negation(singleton("#foo"));
    assert_eq!(
        InferredType::intersect(&singleton("#bar"), &neg, prov(), None, None),
        singleton("#bar")
    );
}

#[test]
fn intersect_two_complements_same_base() {
    // `intersect(Symbol \ #a, Symbol \ #b) = Symbol \ (#a | #b)`.
    let result = InferredType::intersect(
        &negation(singleton("#a")),
        &negation(singleton("#b")),
        prov(),
        None,
        None,
    );
    let expected = negation(InferredType::union_of(&[singleton("#a"), singleton("#b")]));
    assert_eq!(result, expected);
}

// ── difference: boundary + Dynamic asymmetry ────────────────────────────

#[test]
fn difference_from_never_is_never() {
    assert_eq!(
        InferredType::difference(&InferredType::Never, &singleton("#a"), prov()),
        InferredType::Never
    );
}

#[test]
fn difference_by_never_is_identity() {
    let t = InferredType::known("Integer");
    assert_eq!(
        InferredType::difference(&t, &InferredType::Never, prov()),
        t
    );
}

#[test]
fn dynamic_asymmetry_regression() {
    // ADR 0102 §1, explicitly NOT union_of's Dynamic-absorbs rule:
    //   intersect(Dynamic, P) = P     (both orders)
    //   difference(Dynamic, P) = Dynamic
    //   difference(T, Dynamic) = T
    let p = InferredType::known("Integer");
    assert_eq!(
        InferredType::intersect(&dynamic(), &p, prov(), None, None),
        p
    );
    assert_eq!(
        InferredType::intersect(&p, &dynamic(), prov(), None, None),
        p
    );
    assert_eq!(
        InferredType::difference(&dynamic(), &p, prov()),
        dynamic(),
        "Dynamic never narrows under difference"
    );
    assert_eq!(InferredType::difference(&p, &dynamic(), prov()), p);
}

#[test]
fn intersect_dynamic_identity_both_sides_with_hierarchy() {
    // GAP 4: `intersect(P, Dynamic) = P` AND `intersect(Dynamic, P) = P`, and
    // the Dynamic arm still wins even when a hierarchy is present.
    let h = hierarchy();
    let p = InferredType::known("Integer");
    assert_eq!(
        InferredType::intersect(&dynamic(), &p, prov(), Some(&h), None),
        p
    );
    assert_eq!(
        InferredType::intersect(&p, &dynamic(), prov(), Some(&h), None),
        p
    );
}

// ── difference: union member drop + generics ────────────────────────────

#[test]
fn difference_drops_union_member() {
    let t = InferredType::union_of(&[InferredType::known("Integer"), singleton("#infinity")]);
    let result = InferredType::difference(&t, &singleton("#infinity"), prov());
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn difference_generics_compare_exactly() {
    // `difference(List(Integer) | List(Symbol), List(Integer))` removes exactly
    // one member, matched under full equality (including type_args).
    let t = InferredType::union_of(&[
        list_of(InferredType::known("Integer")),
        list_of(InferredType::known("Symbol")),
    ]);
    let result = InferredType::difference(&t, &list_of(InferredType::known("Integer")), prov());
    assert_eq!(result, list_of(InferredType::known("Symbol")));
}

#[test]
fn difference_generics_different_args_is_noop() {
    // Same class name, different args → not equal → no member removed.
    let t = InferredType::union_of(&[
        list_of(InferredType::known("Integer")),
        list_of(InferredType::known("Symbol")),
    ]);
    let result = InferredType::difference(&t, &list_of(InferredType::known("String")), prov());
    assert_eq!(result, t);
}

// ── difference: Negation construction + flattening ──────────────────────

#[test]
fn difference_symbol_minus_singleton_makes_negation() {
    let result = InferredType::difference(&symbol(), &singleton("#foo"), prov());
    assert_eq!(result, negation(singleton("#foo")));
}

#[test]
fn difference_symbol_minus_singleton_union_negates_the_union() {
    // `excluded` is an InferredType: `difference(Symbol, #a | #b)` =
    // `Negation{Symbol, #a | #b}` (via the RHS-union fold + same-base flatten).
    let removed = InferredType::union_of(&[singleton("#a"), singleton("#b")]);
    let result = InferredType::difference(&symbol(), &removed, prov());
    assert_eq!(result, negation(removed));
}

#[test]
fn difference_same_base_flattens_nested_negation() {
    // `difference(Negation{Symbol, #a}, #b) = Negation{Symbol, #a | #b}` —
    // nested negation never escapes normal form.
    let neg_a = InferredType::difference(&symbol(), &singleton("#a"), prov());
    let result = InferredType::difference(&neg_a, &singleton("#b"), prov());
    let expected = negation(InferredType::union_of(&[singleton("#a"), singleton("#b")]));
    assert_eq!(result, expected);

    // The excluded set must contain no nested Negation.
    if let InferredType::Negation { excluded, .. } = &result {
        let has_nested = match excluded.as_ref() {
            InferredType::Union { members, .. } => members
                .iter()
                .any(|m| matches!(m, InferredType::Negation { .. })),
            other => matches!(other, InferredType::Negation { .. }),
        };
        assert!(!has_nested, "excluded must not nest a Negation: {result:?}");
    } else {
        panic!("expected a Negation, got {result:?}");
    }
}

#[test]
fn difference_meta_is_opaque_to_negation() {
    // `Meta` never becomes a Negation base; subtracting a singleton is a no-op.
    let meta = InferredType::meta("Symbol");
    let result = InferredType::difference(&meta, &singleton("#foo"), prov());
    assert_eq!(result, meta);
}

#[test]
fn difference_rhs_union_folds_left() {
    // `difference(T, A | B) = difference(difference(T, A), B)`.
    let removed = InferredType::union_of(&[singleton("#a"), singleton("#b")]);
    let folded = InferredType::difference(
        &InferredType::difference(&symbol(), &singleton("#a"), prov()),
        &singleton("#b"),
        prov(),
    );
    let direct = InferredType::difference(&symbol(), &removed, prov());
    assert_eq!(direct, folded);
}

// ── canonical `excluded` ordering (GAP 2, ADR 0102 §1) ──────────────────

#[test]
fn negation_excluded_is_canonically_sorted_ascending() {
    // GAP 2: however the exclusions are built up, the stored `excluded` union
    // is sorted ascending by `class_name` — deterministic serialisation.
    // Build in reverse order (#b then #a) and assert the stored order is #a,#b.
    let neg = InferredType::difference(
        &InferredType::difference(&symbol(), &singleton("#b"), prov()),
        &singleton("#a"),
        prov(),
    );
    let InferredType::Negation { excluded, .. } = &neg else {
        panic!("expected a Negation, got {neg:?}");
    };
    let InferredType::Union { members, .. } = excluded.as_ref() else {
        panic!("expected a union of exclusions, got {excluded:?}");
    };
    let names: Vec<String> = members
        .iter()
        .filter_map(|m| m.as_known().map(ToString::to_string))
        .collect();
    assert_eq!(
        names,
        vec!["#a".to_string(), "#b".to_string()],
        "excluded members must be sorted ascending by class_name"
    );
}

// ── union_of: absorption law ────────────────────────────────────────────

#[test]
fn union_absorbs_negation_and_singleton() {
    // ADR 0102 §1: `(Symbol \ #foo) | #foo ⇒ Symbol`.
    let neg = InferredType::difference(&symbol(), &singleton("#foo"), prov());
    assert_eq!(
        InferredType::union_of(&[neg.clone(), singleton("#foo")]),
        symbol()
    );
    // Order-independent.
    assert_eq!(InferredType::union_of(&[singleton("#foo"), neg]), symbol());
}

#[test]
fn union_partially_absorbs_negation() {
    // `(Symbol \ (#a | #b)) | #a = Symbol \ #b`.
    let neg = negation(InferredType::union_of(&[singleton("#a"), singleton("#b")]));
    let result = InferredType::union_of(&[neg, singleton("#a")]);
    assert_eq!(result, negation(singleton("#b")));
}

#[test]
fn union_bare_symbol_subsumes_negation() {
    // `(Symbol \ #a) | Symbol = Symbol`.
    let neg = negation(singleton("#a"));
    assert_eq!(InferredType::union_of(&[neg, symbol()]), symbol());
}

#[test]
fn union_merges_disjoint_complements_to_base() {
    // Same-base complement union: `(Symbol \ #a) | (Symbol \ #b) = Symbol`
    // (excludeds intersect to nothing, so nothing is removed).
    let result = InferredType::union_of(&[negation(singleton("#a")), negation(singleton("#b"))]);
    assert_eq!(result, symbol());
}

#[test]
fn union_merges_overlapping_complements() {
    // `(Symbol \ (#a | #b)) | (Symbol \ (#b | #c)) = Symbol \ #b`
    // (only `#b` is excluded from *both* sides).
    let neg_ab = negation(InferredType::union_of(&[singleton("#a"), singleton("#b")]));
    let neg_bc = negation(InferredType::union_of(&[singleton("#b"), singleton("#c")]));
    let result = InferredType::union_of(&[neg_ab, neg_bc]);
    assert_eq!(result, negation(singleton("#b")));
}

#[test]
fn union_dedups_equal_complements() {
    // Logically-equal negations collapse to one (dedup), never grow the union.
    let neg = negation(singleton("#a"));
    assert_eq!(
        InferredType::union_of(&[neg.clone(), neg.clone()]),
        negation(singleton("#a"))
    );
}

// ── display + provenance ────────────────────────────────────────────────

#[test]
fn negation_display_renders_backslash() {
    let neg = negation(singleton("#foo"));
    assert_eq!(neg.display_for_diagnostic().unwrap(), "Symbol \\ #foo");
    assert_eq!(neg.display_name().unwrap(), "Symbol \\ #foo");
}

#[test]
fn negation_display_parenthesises_excluded_union() {
    let neg = negation(InferredType::union_of(&[singleton("#a"), singleton("#b")]));
    let rendered = neg.display_for_diagnostic().unwrap();
    assert!(
        rendered == "Symbol \\ (#a | #b)" || rendered == "Symbol \\ (#b | #a)",
        "unexpected render: {rendered}"
    );
}

#[test]
fn difference_threads_provenance_into_negation() {
    let declared = TypeProvenance::Declared(Span::new(3, 9));
    let result = InferredType::difference(&symbol(), &singleton("#foo"), declared);
    assert_eq!(result.provenance(), Some(declared));
}

#[test]
fn provenance_accessor_covers_all_variants() {
    assert!(InferredType::known("Integer").provenance().is_some());
    assert!(negation(singleton("#a")).provenance().is_some());
    assert!(InferredType::Never.provenance().is_none());
    assert!(dynamic().provenance().is_none());
}

// ── as_known: Negation is not a bare class ──────────────────────────────

#[test]
fn negation_is_not_known() {
    assert!(negation(singleton("#a")).as_known().is_none());
}

// ── Intersection (ADR 0102 §1/§3, BT-2743) ──────────────────────────────

/// Builds a minimal `ProtocolRegistry` containing empty (no required method)
/// protocols under the given names — enough for `has_protocol` checks.
fn protocol_registry_with(
    names: &[&str],
) -> crate::semantic_analysis::protocol_registry::ProtocolRegistry {
    use crate::ast::{CommentAttachment, Module, ProtocolDefinition};
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let h = hierarchy();
    let proto_module = Module {
        protocols: names
            .iter()
            .map(|name| ProtocolDefinition {
                name: crate::ast::Identifier::new(*name, Span::default()),
                type_params: vec![],
                extending: None,
                method_signatures: vec![],
                class_method_signatures: vec![],
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::default(),
            })
            .collect(),
        ..Module::new(vec![], Span::default())
    };
    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&proto_module, &h);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    registry
}

#[test]
fn intersect_class_and_protocol_produces_stored_intersection() {
    // `Collection & Comparable` (class ∩ protocol) is the irreducible case —
    // ADR 0068's flagship `&` example. Requires the protocol registry so
    // `Comparable` is recognised as a protocol rather than an unrelated class.
    let registry = protocol_registry_with(&["Comparable"]);
    let result = InferredType::intersect(
        &InferredType::known("Collection"),
        &InferredType::known("Comparable"),
        prov(),
        None,
        Some(&registry),
    );
    let InferredType::Intersection { members, .. } = result else {
        panic!("expected Intersection, got {result:?}");
    };
    assert_eq!(members.len(), 2);
    assert!(members.contains(&InferredType::known("Collection")));
    assert!(members.contains(&InferredType::known("Comparable")));
}

#[test]
fn intersect_protocol_and_protocol_produces_stored_intersection() {
    // Protocol ∩ protocol is likewise irreducible.
    let registry = protocol_registry_with(&["Printable", "Comparable"]);
    let result = InferredType::intersect(
        &InferredType::known("Printable"),
        &InferredType::known("Comparable"),
        prov(),
        None,
        Some(&registry),
    );
    assert!(matches!(result, InferredType::Intersection { .. }));
}

#[test]
fn intersect_class_and_protocol_without_registry_falls_back_to_never() {
    // Without a registry, `intersect` cannot distinguish a protocol name from
    // an unrelated class — conservatively `Never` (documented `None` fallback).
    let result = InferredType::intersect(
        &InferredType::known("Collection"),
        &InferredType::known("Comparable"),
        prov(),
        None,
        None,
    );
    assert_eq!(result, InferredType::Never);
}

#[test]
fn intersect_class_and_class_still_reduces_with_hierarchy_even_with_registry() {
    // GAP 1 must not regress (BT-2743 task instruction): class ∩ class still
    // reduces via the nominal hierarchy — `Number ∩ Integer = Integer` — even
    // when a (empty, unrelated) protocol registry is also supplied.
    let registry = protocol_registry_with(&["Comparable"]);
    let h = hierarchy();
    let number = InferredType::known("Number");
    let integer = InferredType::known("Integer");
    assert_eq!(
        InferredType::intersect(&number, &integer, prov(), Some(&h), Some(&registry)),
        integer
    );
}

#[test]
fn intersect_unrelated_classes_is_never_the_adr_headline_example() {
    // ADR 0102 §1: "`Integer & String` *is* defined: it is `Never`" — holds
    // even with a protocol registry present (neither name is a protocol).
    let registry = protocol_registry_with(&["Comparable"]);
    let result = InferredType::intersect(
        &InferredType::known("Integer"),
        &InferredType::known("String"),
        prov(),
        None,
        Some(&registry),
    );
    assert_eq!(result, InferredType::Never);
}

#[test]
fn intersect_equality_is_order_independent() {
    let registry = protocol_registry_with(&["Printable", "Comparable"]);
    let ab = InferredType::intersect(
        &InferredType::known("Printable"),
        &InferredType::known("Comparable"),
        prov(),
        None,
        Some(&registry),
    );
    let ba = InferredType::intersect(
        &InferredType::known("Comparable"),
        &InferredType::known("Printable"),
        prov(),
        None,
        Some(&registry),
    );
    assert_eq!(ab, ba);
}

#[test]
fn intersect_flattens_three_way_chain() {
    // `A & B & C` parses left-associatively as `(A & B) & C` (ADR 0102 §3),
    // and resolving that through repeated pairwise `intersect` calls must
    // flatten to a single 3-member `Intersection{A, B, C}` — never a nested
    // `Intersection{Intersection{A,B}, C}` (which would break exhaustive
    // matches / display / equality elsewhere expecting a flat `members`).
    let registry = protocol_registry_with(&["A", "B", "C"]);
    let ab = InferredType::intersect(
        &InferredType::known("A"),
        &InferredType::known("B"),
        prov(),
        None,
        Some(&registry),
    );
    let abc = InferredType::intersect(
        &ab,
        &InferredType::known("C"),
        prov(),
        None,
        Some(&registry),
    );
    let InferredType::Intersection { members, .. } = &abc else {
        panic!("expected Intersection, got {abc:?}");
    };
    assert_eq!(
        members.len(),
        3,
        "expected a flat 3-member Intersection, got {members:?}"
    );
    assert!(
        !members
            .iter()
            .any(|m| matches!(m, InferredType::Intersection { .. })),
        "Intersection must not nest another Intersection, got {members:?}"
    );
    assert!(members.contains(&InferredType::known("A")));
    assert!(members.contains(&InferredType::known("B")));
    assert!(members.contains(&InferredType::known("C")));
}

#[test]
fn intersect_flattening_dedups_repeated_member() {
    // `(A & B) & B` must flatten and dedup to the 2-member `{A, B}`, not a
    // 3-member `{A, B, B}`.
    let registry = protocol_registry_with(&["A", "B"]);
    let ab = InferredType::intersect(
        &InferredType::known("A"),
        &InferredType::known("B"),
        prov(),
        None,
        Some(&registry),
    );
    let result = InferredType::intersect(
        &ab,
        &InferredType::known("B"),
        prov(),
        None,
        Some(&registry),
    );
    let InferredType::Intersection { members, .. } = &result else {
        panic!("expected Intersection, got {result:?}");
    };
    assert_eq!(
        members.len(),
        2,
        "expected dedup to 2 members, got {members:?}"
    );
}

#[test]
fn intersect_display_renders_ampersand() {
    let registry = protocol_registry_with(&["Printable", "Comparable"]);
    let result = InferredType::intersect(
        &InferredType::known("Printable"),
        &InferredType::known("Comparable"),
        prov(),
        None,
        Some(&registry),
    );
    let rendered = result.display_for_diagnostic().unwrap();
    assert!(
        rendered == "Printable & Comparable" || rendered == "Comparable & Printable",
        "unexpected render: {rendered}"
    );
}

#[test]
fn intersection_is_not_known() {
    let registry = protocol_registry_with(&["Printable", "Comparable"]);
    let result = InferredType::intersect(
        &InferredType::known("Printable"),
        &InferredType::known("Comparable"),
        prov(),
        None,
        Some(&registry),
    );
    assert!(result.as_known().is_none());
}

#[test]
fn intersection_provenance_accessor() {
    let registry = protocol_registry_with(&["Printable", "Comparable"]);
    let declared = TypeProvenance::Declared(Span::new(3, 9));
    let result = InferredType::intersect(
        &InferredType::known("Printable"),
        &InferredType::known("Comparable"),
        declared,
        None,
        Some(&registry),
    );
    assert_eq!(result.provenance(), Some(declared));
}

#[test]
fn union_of_treats_intersection_as_opaque_member() {
    // `union_of` dedups an `Intersection` by structural equality but applies
    // no absorption law to it (unlike `Negation`) — ADR 0102 §1, BT-2743.
    let registry = protocol_registry_with(&["Printable", "Comparable"]);
    let inter = InferredType::intersect(
        &InferredType::known("Printable"),
        &InferredType::known("Comparable"),
        prov(),
        None,
        Some(&registry),
    );
    // Dedup: unioning with itself yields the same Intersection, not a Union.
    assert_eq!(
        InferredType::union_of(&[inter.clone(), inter.clone()]),
        inter
    );
    // Distinct from an unrelated member: produces a genuine Union.
    let union = InferredType::union_of(&[inter.clone(), InferredType::known("Integer")]);
    assert!(matches!(union, InferredType::Union { .. }));
}

#[test]
fn union_of_singleton_with_bare_symbol_collapses_to_symbol() {
    // BT-2741 (Windows CI): bare-singleton-under-bare-`Symbol` subsumption
    // must apply in `union_of` regardless of whether a `Negation` is present —
    // `#a | Symbol = Symbol` always (a singleton is a subtype of `Symbol`;
    // the collapsed form admits exactly the same values). Previously this
    // subsumption only ran inside the negation-gated absorption pass, so the
    // same set had two normal forms depending on evaluation order.
    assert_eq!(
        InferredType::union_of(&[singleton("#a"), symbol()]),
        symbol()
    );
    assert_eq!(
        InferredType::union_of(&[symbol(), singleton("#a")]),
        symbol()
    );
    // Non-symbol members are untouched.
    assert_eq!(
        InferredType::union_of(&[singleton("#a"), symbol(), InferredType::known("Integer")]),
        InferredType::union_of(&[symbol(), InferredType::known("Integer")])
    );
    // A union of singletons alone (no bare Symbol) must NOT collapse.
    let sing_union = InferredType::union_of(&[singleton("#a"), singleton("#b")]);
    assert!(matches!(&sing_union, InferredType::Union { members, .. } if members.len() == 2));
}

#[test]
fn intersect_commutes_on_windows_ci_counterexample() {
    // BT-2741: deterministic pin of the Windows CI proptest counterexample
    // (seed cc cb64d47a…). Before the `union_of` subsumption fix:
    //   intersect(#a | Symbol, (Symbol \ #a) | Object) = #a | Symbol
    //   intersect((Symbol \ #a) | Object, #a | Symbol) = Symbol
    // — two normal forms for the same set. Both orders must agree, and equal
    // the collapsed form `Symbol`.
    let h = hierarchy();
    let a = InferredType::union_of(&[singleton("#a"), symbol()]);
    let b = InferredType::union_of(&[negation(singleton("#a")), object()]);
    let ab = InferredType::intersect(&a, &b, prov(), Some(&h), None);
    let ba = InferredType::intersect(&b, &a, prov(), Some(&h), None);
    assert_eq!(ab, ba, "intersect must commute: {ab:?} vs {ba:?}");
    assert_eq!(ab, symbol());
}

// ── Property tests ──────────────────────────────────────────────────────

/// Concrete, non-`Dynamic`/`Never` leaf types (safe for the algebraic laws).
fn concrete_leaf() -> impl Strategy<Value = InferredType> {
    prop_oneof![
        Just(symbol()),
        Just(object()),
        Just(InferredType::known("Integer")),
        Just(InferredType::known("String")),
        Just(singleton("#a")),
        Just(singleton("#b")),
        Just(singleton("#c")),
    ]
}

/// A symbol singleton.
fn singleton_strat() -> impl Strategy<Value = InferredType> {
    prop_oneof![
        Just(singleton("#a")),
        Just(singleton("#b")),
        Just(singleton("#c")),
        Just(singleton("#d")),
    ]
}

/// Any type, including `Dynamic`, `Never`, unions, generics and negations —
/// used to exercise termination / no-panic on arbitrary inputs.
fn any_type() -> impl Strategy<Value = InferredType> {
    let leaf = prop_oneof![concrete_leaf(), Just(InferredType::Never), Just(dynamic()),];
    leaf.prop_recursive(3, 16, 4, |inner| {
        prop_oneof![
            prop::collection::vec(inner.clone(), 2..4).prop_map(|ms| InferredType::union_of(&ms)),
            inner.clone().prop_map(list_of),
            singleton_strat().prop_map(negation),
        ]
    })
}

proptest! {
    /// Dedup: unioning a type with itself yields that type.
    #[test]
    fn prop_union_dedup(t in concrete_leaf()) {
        prop_assert_eq!(InferredType::union_of(&[t.clone(), t.clone()]), t);
    }

    /// Absorption: `(Symbol \ s) | s = Symbol` for any singleton `s`, order-independent.
    #[test]
    fn prop_absorption(s in singleton_strat()) {
        let neg = InferredType::difference(&symbol(), &s, prov());
        prop_assert_eq!(InferredType::union_of(&[neg.clone(), s.clone()]), symbol());
        prop_assert_eq!(InferredType::union_of(&[s, neg]), symbol());
    }

    /// LHS distribution: `intersect(A | B, P) = intersect(A,P) | intersect(B,P)`.
    #[test]
    fn prop_intersect_lhs_distribution(
        a in concrete_leaf(),
        b in concrete_leaf(),
        p in concrete_leaf(),
    ) {
        let lhs = InferredType::intersect(&InferredType::union_of(&[a.clone(), b.clone()]), &p, prov(), None, None);
        let rhs = InferredType::union_of(&[
            InferredType::intersect(&a, &p, prov(), None, None),
            InferredType::intersect(&b, &p, prov(), None, None),
        ]);
        prop_assert_eq!(lhs, rhs);
    }

    /// RHS fold: `difference(T, A | B) = difference(difference(T, A), B)`,
    /// where the removed side is a union of singletons.
    #[test]
    fn prop_difference_rhs_fold(
        t in concrete_leaf(),
        a in singleton_strat(),
        b in singleton_strat(),
    ) {
        let removed = InferredType::union_of(&[a.clone(), b.clone()]);
        let direct = InferredType::difference(&t, &removed, prov());
        let folded = InferredType::difference(
            &InferredType::difference(&t, &a, prov()),
            &b,
            prov(),
        );
        prop_assert_eq!(direct, folded);
    }

    /// Same-base flattening: subtracting two singletons from `Symbol` yields a
    /// single `Negation` whose `excluded` never nests another `Negation`.
    #[test]
    fn prop_same_base_flatten(a in singleton_strat(), b in singleton_strat()) {
        let step = InferredType::difference(
            &InferredType::difference(&symbol(), &a, prov()),
            &b,
            prov(),
        );
        let direct = InferredType::difference(
            &symbol(),
            &InferredType::union_of(&[a, b]),
            prov(),
        );
        prop_assert_eq!(&step, &direct);
        if let InferredType::Negation { excluded, .. } = &step {
            let nested = match excluded.as_ref() {
                InferredType::Union { members, .. } => members
                    .iter()
                    .any(|m| matches!(m, InferredType::Negation { .. })),
                other => matches!(other, InferredType::Negation { .. }),
            };
            prop_assert!(!nested, "excluded nests a Negation: {:?}", step);
        }
    }

    /// Termination / no-panic: both operators return on arbitrary inputs
    /// (with and without a hierarchy).
    #[test]
    fn prop_operators_terminate(a in any_type(), b in any_type()) {
        let h = hierarchy();
        let _ = InferredType::intersect(&a, &b, prov(), Some(&h), None);
        let _ = InferredType::intersect(&a, &b, prov(), None, None);
        let _ = InferredType::difference(&a, &b, prov());
    }

    /// GAP 3: commutativity — `intersect(a, b) == intersect(b, a)` across the
    /// Phase-1 type fragment (Known classes, singletons, Symbol, unions,
    /// negations, Dynamic, Never, Object), with the nominal hierarchy engaged.
    #[test]
    fn prop_intersect_commutes(a in any_type(), b in any_type()) {
        let h = hierarchy();
        prop_assert_eq!(
            InferredType::intersect(&a, &b, prov(), Some(&h), None),
            InferredType::intersect(&b, &a, prov(), Some(&h), None),
        );
    }

    /// Dynamic asymmetry holds for any concrete `P`.
    #[test]
    fn prop_dynamic_asymmetry(p in concrete_leaf()) {
        prop_assert_eq!(InferredType::intersect(&dynamic(), &p, prov(), None, None), p.clone());
        prop_assert_eq!(InferredType::difference(&dynamic(), &p, prov()), dynamic());
        prop_assert_eq!(InferredType::difference(&p, &dynamic(), prov()), p);
    }

    /// Intersect-through-a-complement singleton case:
    /// `intersect(Symbol \ s, p) = Never` when `p == s`, else the probed
    /// singleton `p`.
    #[test]
    fn prop_intersect_complement(s in singleton_strat(), p in singleton_strat()) {
        let neg = negation(s.clone());
        let result = InferredType::intersect(&neg, &p, prov(), None, None);
        if p == s {
            prop_assert_eq!(result, InferredType::Never);
        } else {
            prop_assert_eq!(result, p);
        }
    }
}

#[test]
fn intersect_intersection_with_disjoint_class_reduces_to_never() {
    // `Printable & Integer & String`: the flatten arms must re-check pairwise
    // disjointness — `Integer ∩ String = Never` under single inheritance, so
    // the chain reduces to `Never` rather than storing a three-member
    // Intersection that violates its own invariant. Both operand orders.
    let registry = protocol_registry_with(&["Printable"]);
    let h = hierarchy();
    let step1 = InferredType::intersect(
        &InferredType::known("Printable"),
        &InferredType::known("Integer"),
        prov(),
        Some(&h),
        Some(&registry),
    );
    assert!(
        matches!(step1, InferredType::Intersection { .. }),
        "expected stored Intersection, got {step1:?}"
    );
    let left = InferredType::intersect(
        &step1,
        &InferredType::known("String"),
        prov(),
        Some(&h),
        Some(&registry),
    );
    assert_eq!(left, InferredType::Never);
    let right = InferredType::intersect(
        &InferredType::known("String"),
        &step1,
        prov(),
        Some(&h),
        Some(&registry),
    );
    assert_eq!(right, InferredType::Never);
}
