// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Normalisation tests for the set-theoretic type operators `intersect` and
//! `difference`, plus the `Negation` variant and `union_of`'s absorption law.
//!
//! Covers ADR 0102 §1 exactly: dedup, absorption, LHS/RHS distribution and
//! fold, same-base flattening, exact-generics matching, the Dynamic asymmetry
//! regression, and termination on arbitrary inputs.

use super::super::*;
use super::common::*;

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

// ── intersect: base rules ───────────────────────────────────────────────

#[test]
fn intersect_same_type_is_identity() {
    let t = InferredType::known("Integer");
    assert_eq!(InferredType::intersect(&t, &t, prov()), t);
}

#[test]
fn intersect_with_never_is_never() {
    let t = InferredType::known("Integer");
    assert_eq!(
        InferredType::intersect(&t, &InferredType::Never, prov()),
        InferredType::Never
    );
    assert_eq!(
        InferredType::intersect(&InferredType::Never, &t, prov()),
        InferredType::Never
    );
}

#[test]
fn intersect_with_object_is_identity() {
    let t = InferredType::known("Integer");
    assert_eq!(InferredType::intersect(&t, &object(), prov()), t);
    assert_eq!(InferredType::intersect(&object(), &t, prov()), t);
}

#[test]
fn intersect_distinct_concrete_types_are_disjoint() {
    // Structural disjointness (no hierarchy here): distinct classes → Never.
    assert_eq!(
        InferredType::intersect(
            &InferredType::known("Integer"),
            &singleton("#infinity"),
            prov()
        ),
        InferredType::Never
    );
}

#[test]
fn intersect_lhs_union_distributes_to_bare_singleton() {
    // ADR 0102 §1: `intersect(Integer | #infinity, #infinity)` normalises to
    // the *bare* singleton `#infinity`, never a stored wrapper.
    let lhs = InferredType::union_of(&[InferredType::known("Integer"), singleton("#infinity")]);
    let result = InferredType::intersect(&lhs, &singleton("#infinity"), prov());
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
    let result = InferredType::intersect(&t, &rhs, prov());
    // `#a` survives (present on both sides); `#b`/`Integer` are disjoint → gone.
    assert_eq!(result, singleton("#a"));
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
    //   intersect(Dynamic, P) = P
    //   difference(Dynamic, P) = Dynamic
    //   difference(T, Dynamic) = T
    let p = InferredType::known("Integer");
    assert_eq!(InferredType::intersect(&dynamic(), &p, prov()), p);
    assert_eq!(InferredType::intersect(&p, &dynamic(), prov()), p);
    assert_eq!(
        InferredType::difference(&dynamic(), &p, prov()),
        dynamic(),
        "Dynamic never narrows under difference"
    );
    assert_eq!(InferredType::difference(&p, &dynamic(), prov()), p);
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
        let lhs = InferredType::intersect(&InferredType::union_of(&[a.clone(), b.clone()]), &p, prov());
        let rhs = InferredType::union_of(&[
            InferredType::intersect(&a, &p, prov()),
            InferredType::intersect(&b, &p, prov()),
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

    /// Termination / no-panic: both operators return on arbitrary inputs.
    #[test]
    fn prop_operators_terminate(a in any_type(), b in any_type()) {
        let _ = InferredType::intersect(&a, &b, prov());
        let _ = InferredType::difference(&a, &b, prov());
    }

    /// Dynamic asymmetry holds for any concrete `P`.
    #[test]
    fn prop_dynamic_asymmetry(p in concrete_leaf()) {
        prop_assert_eq!(InferredType::intersect(&dynamic(), &p, prov()), p.clone());
        prop_assert_eq!(InferredType::difference(&dynamic(), &p, prov()), dynamic());
        prop_assert_eq!(InferredType::difference(&p, &dynamic(), prov()), p);
    }
}
