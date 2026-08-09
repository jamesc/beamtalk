// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Centralised parametric type resolution — `TypeAnnotation` → `InferredType`.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module is the single source of truth for converting a parsed
//! [`TypeAnnotation`] (from the AST) into an [`InferredType`] while preserving
//! any generic type arguments end-to-end.
//!
//! Previously several sites in `inference.rs` and `validation.rs` flattened
//! annotations to an `EcoString` base name via ad-hoc `.find('(')` slicing (or
//! even dropped `type_args` entirely). That was the shared root cause of the
//! six type_args-preservation bugs tracked under BT-2024 (BT-2018..BT-2023,
//! BT-2016 partial). This module lays down the shared framework once; the
//! follow-up PRs then reduce to "add a failing test, swap the call site".
//!
//! **References:**
//! - Parent epic: BT-2024
//! - This change: BT-2025
//! - ADRs: 0025 (gradual typing), 0068 (parametric types and protocols)

use std::collections::HashMap;

use ecow::{EcoString, eco_format};

use crate::ast::TypeAnnotation;
use crate::semantic_analysis::alias_registry::AliasRegistry;
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::source_analysis::Span;

use super::is_generic_type_param;
use super::well_known::WellKnownClass;
use super::{DynamicReason, InferredType, TypeProvenance, TypeStringContext};

/// Substitution map from a generic type parameter name to its concrete
/// [`InferredType`].
///
/// Used to thread method-local / class-level type-parameter bindings through
/// [`resolve_type_annotation`] so that e.g. `T` resolves to `Integer` inside a
/// call to `Result(T, E) >> value` when the receiver is `Result(Integer, _)`.
///
/// An empty map (the common case, via [`SubstitutionMap::empty`]) means no
/// substitutions apply — all type-parameter identifiers pass through unchanged.
pub(in crate::semantic_analysis) type SubstitutionMap = HashMap<EcoString, InferredType>;

/// Merge a class-level and a method-local substitution map into the single
/// [`SubstitutionMap`] [`resolve_declared_type`] takes, with method-local
/// bindings winning on key collision — the same two-map priority order the
/// deleted `TypeChecker::resolve_type_string` (BT-3075) used to check
/// separately (BT-3080: that function's three-parameter shape — class subst,
/// method subst, `self_type` — collapses to this single map plus the
/// resolver's existing `subst.get("Self")` convention below).
///
/// When `self_type` is `Some`, additionally binds the reserved `"Self"` key
/// to it. Both spellings of a self type route through this binding:
/// [`DeclaredType::SelfType`] (built from [`TypeAnnotation::SelfType`], or
/// parsed from a bare `"Self"` string — `DeclaredType::parse` recognises the
/// flat rendering) reads it in [`resolve_declared_type_inner`]'s dedicated
/// `SelfType` arm, and [`DeclaredType::SelfClass`] derives its metatype from
/// the same key. Binding `"Self"` here reproduces `resolve_type_string`'s
/// dedicated `type_name == "Self"` special case through one shared binding
/// rather than a second lookup path. `"Self"` is reserved (ADR 0108 forbids
/// it as a generic type-param or alias name), so this can never collide with
/// a real substitution entry.
#[must_use]
pub(in crate::semantic_analysis) fn merge_substitutions(
    class_subst: &SubstitutionMap,
    method_subst: &SubstitutionMap,
    self_type: Option<&InferredType>,
) -> SubstitutionMap {
    let mut merged = class_subst.clone();
    merged.extend(method_subst.iter().map(|(k, v)| (k.clone(), v.clone())));
    if let Some(ty) = self_type {
        merged.insert(EcoString::from("Self"), ty.clone());
    }
    merged
}

/// Resolve a [`TypeAnnotation`] into a fully-parameterised [`InferredType`],
/// preserving generic arguments end-to-end.
///
/// Behaviour per variant:
/// * [`TypeAnnotation::Simple`] — plain class names. The type keywords `nil`,
///   `true`, `false` are mapped to `UndefinedObject`, `True`, `False`. The
///   keyword `Never` becomes [`InferredType::Never`]. If the name appears in
///   `subst`, the substituted value wins (method-local / class-level type
///   parameters resolve to their concrete bindings). Otherwise, if the name
///   is registered in `alias_registry` (ADR 0108, BT-2895), the reference
///   expands *eagerly* to the alias's declared annotation, recursively
///   resolved through this same function — so a `RestartStrategy` annotation
///   resolves to the exact `InferredType::Union` its expansion would. This
///   makes the resolution order `subst` → alias table → nominal class,
///   disjoint by construction: ADR 0108 reserves every bare single-letter
///   name for `subst`/generic type parameters and forbids it as an alias
///   name, so the two lookups never contend for the same identifier.
/// * [`TypeAnnotation::Generic`] — builds `Known { class_name: base, type_args }`
///   with every argument recursively resolved through the same rules. Provenance
///   is marked `Declared(span)` since the annotation is user-written.
/// * [`TypeAnnotation::Union`] — resolves each member, then folds through
///   [`InferredType::union_of`] which handles flattening and deduplication.
/// * [`TypeAnnotation::FalseOr`] — `inner | False`, using the same union
///   machinery.
/// * [`TypeAnnotation::Difference`] — `base \ excluded`, resolved through
///   [`InferredType::difference`] (ADR 0102 §1) with `hierarchy = None` (this
///   resolver never consults the class hierarchy — see below), so only the
///   singleton flavour (`Symbol \ #foo`) and structurally-reducible cases
///   normalise; the nominal-class flavour (ADR 0102 §5, BT-2744, `Object \
///   Number`) requires a hierarchy and is only produced by narrowing
///   (`refine_class_narrowing` in `inference.rs`), not by annotation
///   resolution.
/// * [`TypeAnnotation::Intersection`] — `left & right`, resolved through
///   [`InferredType::intersect`] (ADR 0102 §1/§3, BT-2743) with `hierarchy =
///   None` (this resolver does not consult the class hierarchy — see below)
///   and the `protocol_registry` parameter passed through unchanged. Class ∩
///   protocol resolves correctly whenever a registry is supplied (protocol
///   names are recognised without needing the hierarchy — protocols and
///   classes share one namespace). Class ∩ class only reduces via nominal
///   subtyping when a *future* caller threads a hierarchy in; without one, two
///   distinct, non-protocol class names conservatively fall to `Never` (same
///   structural-only default `intersect` already uses elsewhere).
/// * [`TypeAnnotation::SelfType`] — returns `Dynamic(Unknown)`. Bare `Self`
///   is resolved at the *call site* where the static receiver class is known;
///   the annotation-resolution pass does not have that context.
/// * [`TypeAnnotation::SelfClass`] — returns `Dynamic(Unknown)`. `Self class`
///   is a metatype that historically resolves to Dynamic here (see BT-1952) so
///   that existing patterns like `x class = Foo` keep working; the call-site
///   metatype wrapping lives in the caller.
/// * [`TypeAnnotation::ClassOf`] — returns `Dynamic(Unknown)`. The
///   `<ClassName> class` metatype (BT-2034) names the metaclass of a specific
///   class hierarchy; like `Self class` it resolves to Dynamic so that
///   class-side methods on the named class (e.g. `Actor class >> isSupervisor`)
///   flow through without false DNU warnings.
/// * [`TypeAnnotation::Singleton`] — `#name` becomes `Known("#name", [])`,
///   matching the existing singleton-as-Symbol-subtype convention.
///
/// The class hierarchy is *not* consulted during annotation resolution —
/// "does this class exist?" is a call-site decision that happens *after* the
/// annotation resolves, not during.
///
/// `protocol_registry` (ADR 0102 §1/§3, BT-2743) is threaded through purely
/// for [`TypeAnnotation::Intersection`] resolution — every other variant
/// ignores it. Pass `None` when no registry is available (the annotation
/// still resolves; `&`-typed protocol intersections just won't recognise a
/// protocol name and will conservatively fall to `Never`, matching
/// [`InferredType::intersect`]'s documented `None` behaviour).
///
/// `alias_registry` (ADR 0108, BT-2895) is consulted only by
/// [`TypeAnnotation::Simple`] — see that variant's doc above. Pass `None`
/// when no registry is available (a `Simple` name that would otherwise be an
/// alias reference just resolves as an ordinary nominal class, matching
/// pre-ADR-0108 behaviour).
pub(in crate::semantic_analysis) fn resolve_type_annotation(
    ann: &TypeAnnotation,
    subst: &SubstitutionMap,
    protocol_registry: Option<&ProtocolRegistry>,
    alias_registry: Option<&AliasRegistry>,
) -> InferredType {
    let declared = DeclaredType::from(ann);
    resolve_declared_type(
        &declared,
        subst,
        protocol_registry,
        alias_registry,
        TypeStringContext::Declared,
    )
}

/// [`resolve_type_annotation`], additionally returning the full transitive
/// set of alias names touched while resolving `ann` (ADR 0108 hot-reload
/// re-check trigger, BT-2899).
///
/// `memo` (see [`resolve_type_annotation_inner`]'s doc) already accumulates
/// exactly one entry per alias name *fully* resolved during this one
/// top-level call — including alias-of-alias chains, since expanding `B` in
/// `type B = A | #z` recurses into resolving `A` through the same `memo`
/// before `B` itself is memoized. So `memo`'s keys *are* the transitive
/// dependency set this function needs to report: resolving `p :: B` records
/// both `B` and `A`, exactly the "full transitive expansion walk" ADR 0108's
/// Implementation section requires — a live redefinition of `A` alone must
/// still invalidate `p`'s site even though `A` is never the annotation's own
/// written name.
///
/// Returned as a sorted `Vec` (not the `HashSet` `memo`'s keys would give)
/// so callers get a deterministic order for free — useful for tests and for
/// building a stable compiler-port response field without an extra sort at
/// the call site.
///
/// A plain, non-alias annotation (or one resolved with `alias_registry =
/// None`) returns an empty `Vec`, matching [`resolve_type_annotation`]'s
/// existing "no registry, no alias behaviour" contract.
pub(in crate::semantic_analysis) fn resolve_type_annotation_with_alias_deps(
    ann: &TypeAnnotation,
    subst: &SubstitutionMap,
    protocol_registry: Option<&ProtocolRegistry>,
    alias_registry: Option<&AliasRegistry>,
) -> (InferredType, Vec<EcoString>) {
    let declared = DeclaredType::from(ann);
    resolve_declared_type_with_alias_deps(
        &declared,
        subst,
        protocol_registry,
        alias_registry,
        TypeStringContext::Declared,
    )
}

/// [`resolve_declared_type`] additionally returning the full transitive set
/// of alias names touched while resolving `dt` — the [`DeclaredType`]
/// counterpart of [`resolve_type_annotation_with_alias_deps`] (ADR 0108
/// hot-reload re-check trigger, BT-2899). See that function's doc for the
/// full rationale; identical here, just walking `DeclaredType` instead of
/// `TypeAnnotation`.
pub(in crate::semantic_analysis) fn resolve_declared_type_with_alias_deps(
    dt: &DeclaredType,
    subst: &SubstitutionMap,
    protocol_registry: Option<&ProtocolRegistry>,
    alias_registry: Option<&AliasRegistry>,
    ctx: TypeStringContext,
) -> (InferredType, Vec<EcoString>) {
    let mut expanding = Vec::new();
    let mut memo = HashMap::new();
    let resolved = resolve_declared_type_inner(
        dt,
        subst,
        protocol_registry,
        alias_registry,
        ctx,
        &mut expanding,
        &mut memo,
    );
    let mut touched: Vec<EcoString> = memo.into_keys().collect();
    touched.sort();
    (resolved, touched)
}

/// Resolve a [`DeclaredType`] into a fully-parameterised [`InferredType`]
/// (BT-3076) — the canonical, span-free recursion that
/// [`resolve_type_annotation`] now delegates to (via `DeclaredType::from`)
/// and that a future caller may drive directly from a stored
/// `MethodInfo::return_type` / `param_types` string (via
/// [`DeclaredType::parse`](crate::semantic_analysis::class_hierarchy::DeclaredType::parse))
/// without re-parsing through the AST at all.
///
/// Behaviour is the [`TypeAnnotation`]-based resolver's, verbatim, per
/// variant — see [`resolve_type_annotation`]'s doc for the full per-variant
/// rundown (`Simple`, `Generic`, `Union`, `FalseOr`, `Difference`,
/// `Intersection`, `SelfType`, `SelfClass`, `ClassOf`, `Singleton`) — plus
/// the [`TypeStringContext`] provenance rules the now-deleted
/// `TypeChecker::resolve_type_string` established (BT-3075, removed by
/// BT-3076 stage 3c once every call site resolved a `DeclaredType`
/// directly): in [`TypeStringContext::Substitution`], an
/// unresolved bare single-letter type-param name collapses to `Dynamic`
/// (BT-1834, checked only for a bare `Simple` node — a `Generic` base is
/// never a type param) and constructed types are stamped `Substituted`
/// provenance instead of `Declared`.
///
/// **Span degradation.** `DeclaredType` carries no source location, so every
/// provenance this resolver stamps uses [`Span::default()`] rather than a
/// real span — unlike the annotation-based recursion this replaces, which
/// stamped each node's own span. This mirrors the deleted
/// `resolve_type_string`'s own convention (it only ever had
/// `Span::default()` to work with, being string-based) and is behaviourally
/// inert: [`InferredType`]'s `PartialEq`
/// ignores provenance entirely, and the one production reader of a
/// `TypeProvenance`'s payload (`check_impossible_class_comparison`'s
/// `Inferred`-only gate) matches on the *variant*, never the span value.
///
/// [`resolve_type_annotation`]: self::resolve_type_annotation
pub(in crate::semantic_analysis) fn resolve_declared_type(
    dt: &DeclaredType,
    subst: &SubstitutionMap,
    protocol_registry: Option<&ProtocolRegistry>,
    alias_registry: Option<&AliasRegistry>,
    ctx: TypeStringContext,
) -> InferredType {
    let mut expanding = Vec::new();
    let mut memo = HashMap::new();
    resolve_declared_type_inner(
        dt,
        subst,
        protocol_registry,
        alias_registry,
        ctx,
        &mut expanding,
        &mut memo,
    )
}

/// The actual resolution recursion, guarded by `expanding` — the stack of
/// alias names currently being expanded on the current path — and cached by
/// `memo` — every alias name already *fully* resolved earlier in this same
/// top-level call.
///
/// Cycle detection *itself* (rejecting a cyclic alias at declaration time,
/// e.g. `type A = B`, `type B = A`) lives in
/// [`AliasRegistry::register_module`](crate::semantic_analysis::alias_registry::AliasRegistry::register_module)
/// and
/// [`AliasRegistry::redefine_alias`](crate::semantic_analysis::alias_registry::AliasRegistry::redefine_alias)
/// (BT-2896). But eager expansion means a cycle that slips past
/// declaration-time checking (a live redefinition landing outside the
/// `redefine_alias` path, or any other gap) would otherwise recurse forever
/// the first time it's *referenced* — this guard is the "expansion itself
/// carries a visited-set guard so a cycle that slips through is a
/// diagnostic, never a hang" promise from that same ADR section, satisfied
/// here defensively: a name already being expanded on this path resolves to
/// `Dynamic` instead of recursing again.
///
/// `memo` exists for a different reason: without it, a *legal, acyclic*
/// chain of aliases that each reference the previous one twice —
/// `type L1 = L0 | L0`, `type L2 = L1 | L1`, …, `type Ln = L(n-1) | L(n-1)`
/// — re-expands `L(n-1)` from scratch on both sides of every `|`, doubling
/// the work at each level (`O(2ⁿ)` total). `expanding` (a path stack) does
/// not prevent this — it only rejects a name that is an *ancestor* of
/// itself, and `L(n-1)`'s two references here are siblings, not nested.
/// Since resolution is a pure function of `(declared_type, subst,
/// alias_registry, ctx)` and neither changes across a single top-level
/// [`resolve_declared_type`] call (every recursive call below threads them
/// unchanged), caching by alias name alone is sound *within* one call: the
/// first full resolution of a name is reused verbatim for every subsequent
/// reference, turning the exponential re-walk into one resolution per
/// distinct alias name.
///
/// Alias expansion threads `subst`, `protocol_registry`, `alias_registry`,
/// and `ctx` through unchanged into the recursive call on the alias's own
/// (converted) declared body — the same threading
/// `resolve_type_annotation_inner` used. The now-deleted
/// `TypeChecker::resolve_type_string`'s alias branch used to diverge from
/// this (it reset to an empty `SubstitutionMap`, dropped `protocol_registry`,
/// and always resolved through the `Declared`-context AST path); that
/// divergence no longer exists now that every call site resolves through
/// this one recursion (BT-3076 stage 3c / BT-3080).
#[allow(clippy::too_many_lines)] // one match arm per DeclaredType variant — see resolve_declared_type's doc
fn resolve_declared_type_inner(
    dt: &DeclaredType,
    subst: &SubstitutionMap,
    protocol_registry: Option<&ProtocolRegistry>,
    alias_registry: Option<&AliasRegistry>,
    ctx: TypeStringContext,
    expanding: &mut Vec<EcoString>,
    memo: &mut HashMap<EcoString, InferredType>,
) -> InferredType {
    match dt {
        DeclaredType::Simple(name) => {
            if WellKnownClass::from_str(name) == Some(WellKnownClass::Never) {
                return InferredType::Never;
            }
            // BT-2865: `Dynamic` must resolve to the real `InferredType::
            // Dynamic` variant, not a `Known{class_name: "Dynamic"}` pseudo-
            // class — otherwise every check written against the `Dynamic`
            // variant (dead-code analysis like `isKindOf:`'s "can never be
            // true" hint, `merge_method_local_binding`'s "Known survives
            // Dynamic" rule, etc.) silently fails to recognise it, since
            // `matches!(ty, InferredType::Dynamic(_))` never matches a
            // disguised `Known`. This matters most as a *nested* type arg
            // (e.g. `Result(Dynamic, Error)`) — a bare, unparameterized
            // `Result` never hits this path at all (no type args to resolve),
            // which is why the bug was invisible until a param was partially
            // parameterized.
            //
            // BT-3080: an FFI spec's `any()`/`term()` return type ("Dynamic"
            // in the wire vocabulary) is not the same *reason* as a
            // user-written `:: Dynamic` — `DynamicSpec` "loses" to a concrete
            // binding observed elsewhere during merge, where `ExplicitDynamic`
            // is authoritative (see `DynamicReason::ExplicitDynamic`'s doc).
            // Folding `native_types::map_type_name` onto this resolver must
            // not collapse that distinction, so `Extracted` context keeps the
            // FFI-specific reason; every other context keeps the pre-existing
            // `ExplicitDynamic`.
            if WellKnownClass::from_str(name) == Some(WellKnownClass::Dynamic) {
                let reason = if ctx == TypeStringContext::Extracted {
                    DynamicReason::DynamicSpec
                } else {
                    DynamicReason::ExplicitDynamic
                };
                return InferredType::Dynamic(reason);
            }
            // Method-local / class-level type-parameter substitution wins over
            // keyword resolution, because `T`, `E`, `R` would otherwise flow
            // through as ordinary class names.
            if let Some(resolved) = subst.get(name) {
                return resolved.clone();
            }
            // BT-1834 / BT-3075: an unresolved bare type param (single
            // uppercase letter) collapses to Dynamic in substitution contexts
            // only, so downstream sends don't get false DNU warnings. The
            // `Declared`-context callers (`resolve_type_annotation`'s
            // wrapper) never hit this — an unmapped bare param there passes
            // through as a nominal class name, matching pre-BT-3076 behaviour.
            if ctx == TypeStringContext::Substitution && is_generic_type_param(name) {
                return InferredType::Dynamic(DynamicReason::Unknown);
            }
            // ADR 0108 / BT-2895: alias table comes next in the resolution
            // order (`subst` → alias table → nominal class). A reference to
            // an alias name expands eagerly to its declared annotation,
            // recursively resolved through this same function so a chain of
            // aliases (`type B = A | #z`, `type A = #x | #y`) resolves all
            // the way down to a plain structural `InferredType`.
            if let Some(registry) = alias_registry {
                if let Some(alias_info) = registry.get(name) {
                    // See `resolve_declared_type_inner`'s doc: a name
                    // already fully resolved earlier in this call is reused
                    // verbatim, avoiding the exponential re-walk a chain of
                    // doubling aliases would otherwise trigger.
                    if let Some(cached) = memo.get(name) {
                        return cached.clone();
                    }
                    if expanding.contains(name) {
                        // A cycle slipped through declaration-time checking
                        // (e.g. a live redefinition that bypassed
                        // `AliasRegistry::redefine_alias`) — never hang. See
                        // `resolve_declared_type_inner`'s doc.
                        return InferredType::Dynamic(DynamicReason::Unknown);
                    }
                    expanding.push(name.clone());
                    let alias_declared = DeclaredType::from(&alias_info.annotation);
                    let resolved = resolve_declared_type_inner(
                        &alias_declared,
                        subst,
                        protocol_registry,
                        alias_registry,
                        ctx,
                        expanding,
                        memo,
                    );
                    expanding.pop();
                    // BT-2897 / ADR 0108: tag the *exact* value just produced
                    // with the alias name for display — see `InferredType::
                    // tag_alias_expansion`'s doc. This is the single
                    // construction point for `TypeProvenance::Aliased`, so
                    // every hover/diagnostic use of this resolved type can
                    // render `RestartStrategy (#temporary | …)` instead of
                    // the bare expansion. Tagged *before* memoizing so a
                    // later reference to the same alias reuses the tagged
                    // value verbatim (see the `memo.get` early-return above).
                    // Span degradation: `Span::default()` — see
                    // `resolve_declared_type`'s doc.
                    let resolved = resolved.tag_alias_expansion(name.clone(), Span::default());
                    memo.insert(name.clone(), resolved.clone());
                    return resolved;
                }
            }
            let resolved_name = resolve_type_keyword(name);
            InferredType::Known {
                class_name: resolved_name,
                type_args: vec![],
                provenance: declared_type_provenance(ctx),
            }
        }
        DeclaredType::Generic { base, parameters } => {
            let type_args: Vec<InferredType> = parameters
                .iter()
                .map(|p| {
                    resolve_declared_type_inner(
                        p,
                        subst,
                        protocol_registry,
                        alias_registry,
                        ctx,
                        expanding,
                        memo,
                    )
                })
                .collect();
            InferredType::Known {
                class_name: base.clone(),
                type_args,
                provenance: declared_type_provenance(ctx),
            }
        }
        DeclaredType::Union(types) => {
            let members: Vec<InferredType> = types
                .iter()
                .map(|t| {
                    resolve_declared_type_inner(
                        t,
                        subst,
                        protocol_registry,
                        alias_registry,
                        ctx,
                        expanding,
                        memo,
                    )
                })
                .collect();
            let unioned = InferredType::union_of(&members);
            if ctx == TypeStringContext::Substitution {
                stamp_substituted_provenance(unioned)
            } else {
                unioned
            }
        }
        DeclaredType::FalseOr(inner) => {
            let inner_ty = resolve_declared_type_inner(
                inner,
                subst,
                protocol_registry,
                alias_registry,
                ctx,
                expanding,
                memo,
            );
            let unioned = InferredType::union_of(&[inner_ty, InferredType::known("False")]);
            if ctx == TypeStringContext::Substitution {
                stamp_substituted_provenance(unioned)
            } else {
                unioned
            }
        }
        DeclaredType::Difference { base, excluded } => {
            // ADR 0102 §1: `base \ excluded` resolves through the shared
            // `difference` set operation (BT-2739). Reducible cases normalise —
            // e.g. an excluded member that drops a union member, or `T \ T`
            // collapsing to `Never`.
            let base_ty = resolve_declared_type_inner(
                base,
                subst,
                protocol_registry,
                alias_registry,
                ctx,
                expanding,
                memo,
            );
            let excluded_ty = resolve_declared_type_inner(
                excluded,
                subst,
                protocol_registry,
                alias_registry,
                ctx,
                expanding,
                memo,
            );
            InferredType::difference(&base_ty, &excluded_ty, declared_type_provenance(ctx), None)
        }
        DeclaredType::Intersection { left, right } => {
            // ADR 0102 §1/§3, BT-2743: `left & right` resolves through the
            // shared `intersect` set operation. `hierarchy` is `None` (this
            // resolver never consults it); `protocol_registry` is passed
            // through so class ∩ protocol resolves to the stored
            // `Intersection` instead of falling through to `Never`.
            let left_ty = resolve_declared_type_inner(
                left,
                subst,
                protocol_registry,
                alias_registry,
                ctx,
                expanding,
                memo,
            );
            let right_ty = resolve_declared_type_inner(
                right,
                subst,
                protocol_registry,
                alias_registry,
                ctx,
                expanding,
                memo,
            );
            InferredType::intersect(
                &left_ty,
                &right_ty,
                declared_type_provenance(ctx),
                None,
                protocol_registry,
            )
        }
        DeclaredType::ClassOf(class_name) => {
            // ADR 0083: `<ClassName> class` is the metatype of the named class.
            // The name is carried directly in the annotation, so resolve it to a
            // dedicated `Meta` here (no enclosing-class context needed). A
            // metatype value still satisfies `:: Class` / `:: Behaviour`
            // parameters via the tower subtyping in `validation.rs`.
            InferredType::Meta {
                class_name: class_name.clone(),
                provenance: declared_type_provenance(ctx),
            }
        }
        DeclaredType::SelfClass => {
            // ADR 0083: `Self class` is the metatype of the *enclosing* class.
            // That class isn't known to the free-function resolver, so callers
            // that have the receiver class thread it through `subst` under the
            // reserved `Self` key (see `build_self_subst`). Without it, fall
            // back to `Dynamic` so existing patterns keep working (BT-1952).
            if let Some(InferredType::Known { class_name, .. }) = subst.get("Self") {
                return InferredType::Meta {
                    class_name: class_name.clone(),
                    provenance: declared_type_provenance(ctx),
                };
            }
            InferredType::Dynamic(DynamicReason::Unknown)
        }
        DeclaredType::SelfType => {
            // `Self` needs the static receiver class. A caller that knows it
            // threads it under the reserved `Self` subst key — mirroring
            // `SelfClass` above (BT-3076) — so a *nested* `Self` inside a
            // generic/union return type (`Result(Self, Error)`, BT-1986 /
            // BT-1992) resolves to the full receiver type (with type args)
            // instead of losing it. A bare top-level `Self` is still handled
            // at the call site directly (see `resolve_type_annotation`'s
            // doc), so this fallback only matters for the nested case.
            // Without a threaded binding, falls back to `Dynamic` — the
            // pre-BT-3076 behaviour for every caller that never threads one.
            if let Some(self_ty) = subst.get("Self") {
                return self_ty.clone();
            }
            InferredType::Dynamic(DynamicReason::Unknown)
        }
        DeclaredType::Singleton(name) => InferredType::known(eco_format!("#{name}")),
    }
}

/// `Declared`, `Substituted`, or `Extracted` provenance, chosen by `ctx` — the
/// `DeclaredType` recursion's single construction point for "this was a
/// constructed nominal / generic / metatype / set-op result" (BT-3075,
/// extended BT-3080 with the `Extracted` arm for the folded-in FFI path).
fn declared_type_provenance(ctx: TypeStringContext) -> TypeProvenance {
    match ctx {
        TypeStringContext::Substitution => TypeProvenance::Substituted(Span::default()),
        TypeStringContext::Extracted => TypeProvenance::Extracted,
        TypeStringContext::Declared => TypeProvenance::Declared(Span::default()),
    }
}

/// Re-stamp `Substituted` provenance on a substitution-context union/false-or
/// result. `union_of` derives provenance from the members, and a plain
/// nominal member never wins, so a substituted `V | Nil` would otherwise
/// read as `Inferred`; `Aliased` results keep their tag (re-stamping would
/// lose the alias display name); `Dynamic` and `Never` results carry no
/// provenance and pass through unchanged.
fn stamp_substituted_provenance(mut ty: InferredType) -> InferredType {
    match &mut ty {
        InferredType::Known { provenance, .. }
        | InferredType::Union { provenance, .. }
        | InferredType::Meta { provenance, .. }
        | InferredType::Negation { provenance, .. }
        | InferredType::Intersection { provenance, .. }
            if !matches!(provenance, TypeProvenance::Aliased { .. }) =>
        {
            *provenance = TypeProvenance::Substituted(Span::default());
        }
        _ => {}
    }
    ty
}

/// Build an [`InferredType`] for a method receiver from a class definition.
///
/// For a generic class `Logger(T, E)`, returns
/// `Known("Logger", [Known("T", []), Known("E", [])])` — symbolic placeholders
/// that thread the class's type parameters so that substitution can later map
/// them to concrete bindings.
///
/// For a non-generic class `Counter`, returns `Known("Counter", [])`.
///
/// For an unknown class (not in the hierarchy), falls back to
/// `Known(class_name, [])` so callers still see a `Known` type and existing
/// receiver checks do not regress.
///
/// Used by the `self`/`super`/`Self class` receiver-synthesis sites so that
/// `T` / `E` bindings flow correctly through [`build_substitution_map`] when a
/// method on a generic class calls another method on the same receiver.
///
/// **References:** BT-2025 acceptance criteria for generic receiver handling.
///
/// [`build_substitution_map`]: super::TypeChecker::build_substitution_map
pub(in crate::semantic_analysis) fn receiver_type_for_class(
    class_name: &EcoString,
    hierarchy: &ClassHierarchy,
) -> InferredType {
    let type_args: Vec<InferredType> = hierarchy
        .get_class(class_name)
        .map(|info| {
            info.type_params
                .iter()
                .map(|param| InferredType::Known {
                    class_name: param.clone(),
                    type_args: vec![],
                    provenance: TypeProvenance::Inferred(crate::source_analysis::Span::default()),
                })
                .collect()
        })
        .unwrap_or_default();

    InferredType::Known {
        class_name: class_name.clone(),
        type_args,
        provenance: TypeProvenance::Inferred(crate::source_analysis::Span::default()),
    }
}

/// Build the [`InferredType`] for a `super` expression's receiver, threading
/// the child class's type-arg bindings into the parent class's type-param
/// positions.
///
/// Walks the child's `superclass_type_args` (the `Sub(R) extends Base(R)` /
/// `IntBase extends Base(Integer)` mapping) and resolves each entry against
/// the child's symbolic / concrete type args:
///
/// * [`SuperclassTypeArg::ParamRef`] — looks up the child's type arg at the
///   given index, so `Sub(R) extends Base(R)` makes `super`'s receiver
///   `Known("Base", [Known("R")])` (Sub's R, not Base's symbolic E).
/// * [`SuperclassTypeArg::Concrete`] — substitutes a fixed type, so
///   `IntBase extends Base(Integer)` makes `super`'s receiver
///   `Known("Base", [Known("Integer")])`.
///
/// When the child has no `superclass_type_args` (no `extends Base(...)`
/// annotation), falls back to [`receiver_type_for_class`] which emits the
/// parent's symbolic type-param placeholders.
///
/// **References:** BT-2021 sub-bug B — `super` sends used to lose receiver
/// `type_args` in generic classes.
///
/// [`SuperclassTypeArg::ParamRef`]: crate::semantic_analysis::class_hierarchy::SuperclassTypeArg::ParamRef
/// [`SuperclassTypeArg::Concrete`]: crate::semantic_analysis::class_hierarchy::SuperclassTypeArg::Concrete
///
/// `alias_registry` (BT-2936, ADR 0108 follow-up to BT-2928) is threaded
/// through to the `Concrete` arm below so an alias-typed `extends
/// Base(SomeAlias)` type argument expands to its declared type instead of
/// staying an opaque nominal class. Pass `None` when no registry is
/// available, matching this function's pre-BT-2936 behaviour.
pub(in crate::semantic_analysis) fn super_receiver_type(
    child_class: &EcoString,
    child_type_args: &[InferredType],
    parent_class: &EcoString,
    hierarchy: &ClassHierarchy,
    alias_registry: Option<&AliasRegistry>,
) -> InferredType {
    use crate::semantic_analysis::class_hierarchy::SuperclassTypeArg;

    let Some(child_info) = hierarchy.get_class(child_class) else {
        return receiver_type_for_class(parent_class, hierarchy);
    };

    if child_info.superclass_type_args.is_empty() {
        return receiver_type_for_class(parent_class, hierarchy);
    }

    let parent_type_args: Vec<InferredType> = child_info
        .superclass_type_args
        .iter()
        .map(|sta| match sta {
            SuperclassTypeArg::ParamRef { param_index } => child_type_args
                .get(*param_index)
                .cloned()
                .unwrap_or(InferredType::Dynamic(DynamicReason::Unknown)),
            // Copilot on PR #2064: resolve the structured declared type so
            // concrete generics (`List(Integer)`), unions (`Integer | Nil`),
            // and `Nil` keyword aliases canonicalise correctly instead of
            // becoming an opaque `Known("List(Integer)")`.
            SuperclassTypeArg::Concrete { declared } => resolve_declared_type(
                declared,
                &SubstitutionMap::new(),
                None,
                alias_registry,
                TypeStringContext::Declared,
            ),
        })
        .collect();

    InferredType::Known {
        class_name: parent_class.clone(),
        type_args: parent_type_args,
        provenance: TypeProvenance::Inferred(crate::source_analysis::Span::default()),
    }
}

/// Resolve type-position keywords to their class names.
///
/// - `nil` / `Nil` → `UndefinedObject`
/// - `false` → `False`
/// - `true` → `True`
/// - Everything else passes through unchanged.
///
/// Both lowercase (`nil`) and capitalised (`Nil`) spellings map to
/// `UndefinedObject` so that `Integer | Nil` type annotations narrow
/// consistently under `isNil` guards (BT-2016).
fn resolve_type_keyword(name: &EcoString) -> EcoString {
    match name.as_str() {
        "nil" | "Nil" => WellKnownClass::UndefinedObject.as_str().into(),
        "false" => "False".into(),
        "true" => "True".into(),
        _ => name.clone(),
    }
}

/// Split a stored-string type name into `(base, args_slice)`.
///
/// Given `"Array(Integer)"` returns `("Array", Some("Integer"))`.
/// Given `"Result(Integer, Error)"` returns `("Result", Some("Integer, Error"))`.
/// Given `"Integer"` (no parentheses) returns `("Integer", None)`.
/// Given `"Array(Integer)extra"` (not terminated by `)`) falls back to
/// `("Array(Integer)extra", None)` — the caller should treat the string as an
/// opaque class name.
///
/// Used by the remaining string-form parsers (`parse_generic_type_string`,
/// `infer_method_local_params`'s param-type matching, etc.) in place of
/// manual `.find('(')` slicing. Centralising this keeps the
/// parenthesis-parsing logic in one place and lets the
/// `no .find('(')` grep check stay clean.
///
/// Re-exported from [`string_utils`](crate::semantic_analysis::string_utils)
/// — the shared implementation, below both `type_checker` and
/// `class_hierarchy` (BT-3089) — so existing call sites that spell this as
/// `type_resolver::split_generic_base` keep working unchanged.
///
/// **References:** BT-2025, BT-3089.
pub(in crate::semantic_analysis) use crate::semantic_analysis::string_utils::split_generic_base;

/// Extract the base class-name portion of a stored-string type name.
///
/// `"Array(Integer)"` → `"Array"`. `"Integer"` → `"Integer"`.
///
/// Convenience wrapper around [`split_generic_base`] for callers that only
/// need the base name and discard the args slice. Used in `validation.rs`
/// where only the base name participates in the comparison
/// (`is_assignable_to`, protocol-conformance checks). Callers that also need
/// the args should use [`split_generic_base`] directly to avoid re-parsing.
#[must_use]
pub(in crate::semantic_analysis) fn base_name_of_string(type_name: &str) -> &str {
    split_generic_base(type_name).0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Identifier, TypeAnnotation};
    use crate::semantic_analysis::alias_registry::{AliasInfo, AliasRegistry};
    use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: span(),
        }
    }

    fn empty_subst() -> SubstitutionMap {
        SubstitutionMap::new()
    }

    /// Hierarchy containing all built-in classes but no user classes.
    ///
    /// `Array(E)` and `Dictionary(K, V)` are generic built-ins, suitable for
    /// exercising `receiver_type_for_class` without the test having to wire up
    /// a `Module` + `ClassDefinition` chain.
    fn builtin_hierarchy() -> ClassHierarchy {
        ClassHierarchy::with_builtins()
    }

    // ---- resolve_type_annotation: simple ----

    #[test]
    fn simple_class_name_resolves_to_known() {
        let ann = TypeAnnotation::Simple(ident("Integer"));
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn simple_nil_keyword_resolves_to_undefined_object() {
        let ann = TypeAnnotation::Simple(ident("nil"));
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::known("UndefinedObject"));
    }

    #[test]
    fn simple_capital_nil_keyword_resolves_to_undefined_object() {
        // `Integer | Nil` annotations (BT-2016) — the capital-N spelling must
        // canonicalize to the same class as lowercase `nil` so narrowing under
        // `isNil` works regardless of which the user typed.
        let ann = TypeAnnotation::Simple(ident("Nil"));
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::known("UndefinedObject"));
    }

    #[test]
    fn simple_true_false_keywords_resolve() {
        let true_ann = TypeAnnotation::Simple(ident("true"));
        let false_ann = TypeAnnotation::Simple(ident("false"));
        assert_eq!(
            resolve_type_annotation(&true_ann, &empty_subst(), None, None),
            InferredType::known("True")
        );
        assert_eq!(
            resolve_type_annotation(&false_ann, &empty_subst(), None, None),
            InferredType::known("False")
        );
    }

    #[test]
    fn simple_never_keyword_resolves_to_never() {
        let ann = TypeAnnotation::Simple(ident("Never"));
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::Never);
    }

    // ---- resolve_type_annotation: substitution ----

    #[test]
    fn simple_type_param_substitutes_from_map() {
        let ann = TypeAnnotation::Simple(ident("T"));
        let mut subst = SubstitutionMap::new();
        subst.insert("T".into(), InferredType::known("Integer"));
        let result = resolve_type_annotation(&ann, &subst, None, None);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn simple_unmapped_type_param_passes_through() {
        // Type params without a substitution entry pass through as-is so that
        // downstream code (e.g. `is_generic_type_param`-aware call sites) can
        // decide whether to fall back to Dynamic.
        let ann = TypeAnnotation::Simple(ident("T"));
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::known("T"));
    }

    // ---- resolve_type_annotation: generic ----

    #[test]
    fn generic_two_args_preserves_type_args() {
        let ann = TypeAnnotation::Generic {
            base: ident("Result"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Simple(ident("String")),
            ],
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], InferredType::known("Integer"));
                assert_eq!(type_args[1], InferredType::known("String"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    #[test]
    fn nested_generics_preserved() {
        // `List(Dictionary(String, Integer))`
        let inner = TypeAnnotation::Generic {
            base: ident("Dictionary"),
            parameters: vec![
                TypeAnnotation::Simple(ident("String")),
                TypeAnnotation::Simple(ident("Integer")),
            ],
            span: span(),
        };
        let ann = TypeAnnotation::Generic {
            base: ident("List"),
            parameters: vec![inner],
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "List");
        assert_eq!(type_args.len(), 1);
        let InferredType::Known {
            class_name: inner_name,
            type_args: inner_args,
            ..
        } = &type_args[0]
        else {
            panic!("expected nested Known");
        };
        assert_eq!(inner_name.as_str(), "Dictionary");
        assert_eq!(inner_args.len(), 2);
        assert_eq!(inner_args[0], InferredType::known("String"));
        assert_eq!(inner_args[1], InferredType::known("Integer"));
    }

    #[test]
    fn generic_with_substitution_applies_to_args() {
        // `Result(T, E)` with subst `{T => Integer, E => String}`
        let ann = TypeAnnotation::Generic {
            base: ident("Result"),
            parameters: vec![
                TypeAnnotation::Simple(ident("T")),
                TypeAnnotation::Simple(ident("E")),
            ],
            span: span(),
        };
        let mut subst = SubstitutionMap::new();
        subst.insert("T".into(), InferredType::known("Integer"));
        subst.insert("E".into(), InferredType::known("String"));
        let result = resolve_type_annotation(&ann, &subst, None, None);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Result");
        assert_eq!(type_args.len(), 2);
        assert_eq!(type_args[0], InferredType::known("Integer"));
        assert_eq!(type_args[1], InferredType::known("String"));
    }

    // ---- resolve_type_annotation: union ----

    #[test]
    fn union_members_resolved_and_deduplicated() {
        let ann = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("String")),
                TypeAnnotation::Simple(ident("nil")),
            ],
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        let InferredType::Union { members, .. } = result else {
            panic!("expected Union");
        };
        assert_eq!(members.len(), 2);
        assert!(members.contains(&InferredType::known("String")));
        assert!(members.contains(&InferredType::known("UndefinedObject")));
    }

    #[test]
    fn union_with_generic_member_preserves_type_args() {
        // `Result(Integer, String) | nil`
        let ann = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Generic {
                    base: ident("Result"),
                    parameters: vec![
                        TypeAnnotation::Simple(ident("Integer")),
                        TypeAnnotation::Simple(ident("String")),
                    ],
                    span: span(),
                },
                TypeAnnotation::Simple(ident("nil")),
            ],
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        let InferredType::Union { members, .. } = result else {
            panic!("expected Union");
        };
        assert_eq!(members.len(), 2);
        let has_result_with_args = members.iter().any(|m| {
            matches!(
                m,
                InferredType::Known { class_name, type_args, .. }
                if class_name.as_str() == "Result" && type_args.len() == 2
            )
        });
        assert!(
            has_result_with_args,
            "Result member must preserve its two type args: {members:?}"
        );
    }

    // ---- resolve_type_annotation: false_or ----

    #[test]
    fn false_or_produces_union_with_false() {
        let ann = TypeAnnotation::FalseOr {
            inner: Box::new(TypeAnnotation::Simple(ident("Integer"))),
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        let InferredType::Union { members, .. } = result else {
            panic!("expected Union, got {result:?}");
        };
        assert_eq!(members.len(), 2);
        assert!(members.contains(&InferredType::known("Integer")));
        assert!(members.contains(&InferredType::known("False")));
    }

    // ---- resolve_type_annotation: difference ----

    #[test]
    fn difference_symbol_minus_singleton_produces_negation() {
        // `Symbol \ #foo` → Negation { base: Symbol, excluded: #foo }.
        let ann = TypeAnnotation::difference(
            TypeAnnotation::Simple(ident("Symbol")),
            TypeAnnotation::Singleton {
                name: "foo".into(),
                span: span(),
            },
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        let InferredType::Negation { base, excluded, .. } = result else {
            panic!("expected Negation, got {result:?}");
        };
        assert_eq!(*base, InferredType::known("Symbol"));
        assert_eq!(*excluded, InferredType::known("#foo"));
    }

    #[test]
    fn difference_of_identical_types_collapses_to_never() {
        // `Integer \ Integer` = Never (reducible case, normalised by `difference`).
        let ann = TypeAnnotation::difference(
            TypeAnnotation::Simple(ident("Integer")),
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::Never);
    }

    #[test]
    fn difference_dropping_union_member_normalises() {
        // `(Integer | String) \ String` drops the `String` member, leaving
        // `Integer` (LHS-union distribution in `difference`).
        let ann = TypeAnnotation::difference(
            TypeAnnotation::Union {
                types: vec![
                    TypeAnnotation::Simple(ident("Integer")),
                    TypeAnnotation::Simple(ident("String")),
                ],
                span: span(),
            },
            TypeAnnotation::Simple(ident("String")),
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::known("Integer"));
    }

    // ---- resolve_type_annotation: intersection ----

    /// Builds a minimal `ProtocolRegistry` containing empty (no required
    /// method) protocols under the given names — enough for `has_protocol`
    /// checks without needing a full conformance fixture.
    fn protocol_registry_with(names: &[&str]) -> ProtocolRegistry {
        use crate::ast::{CommentAttachment, Module, ProtocolDefinition};

        let hierarchy = builtin_hierarchy();
        let proto_module = Module {
            protocols: names
                .iter()
                .map(|name| ProtocolDefinition {
                    name: ident(name),
                    type_params: vec![],
                    extending: None,
                    method_signatures: vec![],
                    class_method_signatures: vec![],
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: span(),
                })
                .collect(),
            ..Module::new(vec![], span())
        };
        let mut registry = ProtocolRegistry::new();
        let diags = registry.register_module(&proto_module, &hierarchy);
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        registry
    }

    #[test]
    fn intersection_of_identical_types_collapses_to_known() {
        // `Integer & Integer` = Integer (identity, reducible).
        let ann = TypeAnnotation::intersection(
            TypeAnnotation::Simple(ident("Integer")),
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn intersection_unrelated_classes_is_never() {
        // `Integer & String` = Never — ADR 0102 §1's example: unrelated
        // classes are disjoint under single inheritance. No protocol registry
        // needed since neither side is a protocol.
        let ann = TypeAnnotation::intersection(
            TypeAnnotation::Simple(ident("Integer")),
            TypeAnnotation::Simple(ident("String")),
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::Never);
    }

    #[test]
    fn intersection_class_and_protocol_produces_stored_intersection() {
        // `Collection & Comparable` (class ∩ protocol) is the irreducible case
        // ADR 0102 §1/§3 stores — the flagship `&` example from ADR 0068's
        // Protocol Composition section. Requires the protocol registry to
        // recognise `Comparable` as a protocol rather than an unrelated class.
        let registry = protocol_registry_with(&["Comparable"]);
        let ann = TypeAnnotation::intersection(
            TypeAnnotation::Simple(ident("Collection")),
            TypeAnnotation::Simple(ident("Comparable")),
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), Some(&registry), None);
        let InferredType::Intersection { members, .. } = result else {
            panic!("expected Intersection, got {result:?}");
        };
        assert_eq!(members.len(), 2);
        assert!(members.contains(&InferredType::known("Collection")));
        assert!(members.contains(&InferredType::known("Comparable")));
    }

    #[test]
    fn intersection_without_protocol_registry_falls_back_to_never() {
        // Without a registry, `Collection & Comparable` cannot be distinguished
        // from two unrelated classes and conservatively resolves to `Never` —
        // a documented limitation (see `resolve_type_annotation`'s doc
        // comment): callers that need class ∩ protocol reduction must
        // thread a `ProtocolRegistry` through.
        let ann = TypeAnnotation::intersection(
            TypeAnnotation::Simple(ident("Collection")),
            TypeAnnotation::Simple(ident("Comparable")),
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::Never);
    }

    #[test]
    fn intersection_of_two_protocols_produces_stored_intersection() {
        // Protocol ∩ protocol is likewise irreducible.
        let registry = protocol_registry_with(&["Printable", "Comparable"]);
        let ann = TypeAnnotation::intersection(
            TypeAnnotation::Simple(ident("Printable")),
            TypeAnnotation::Simple(ident("Comparable")),
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), Some(&registry), None);
        assert!(
            matches!(result, InferredType::Intersection { .. }),
            "expected Intersection, got {result:?}"
        );
    }

    #[test]
    fn grouped_mixed_intersection_then_difference_resolves_transparently() {
        // `(Symbol & Symbol) \ #foo` — writable since grouping parens in
        // type-annotation position (BT-2760). Grouping is transparent in the
        // AST, so the resolver just sees Difference { base: Intersection },
        // no new `InferredType`: the intersection reduces (identity) to
        // `Symbol`, then the difference produces the usual `Negation`.
        let ann = TypeAnnotation::difference(
            TypeAnnotation::intersection(
                TypeAnnotation::Simple(ident("Symbol")),
                TypeAnnotation::Simple(ident("Symbol")),
                span(),
            ),
            TypeAnnotation::Singleton {
                name: "foo".into(),
                span: span(),
            },
            span(),
        );
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        let InferredType::Negation { base, excluded, .. } = result else {
            panic!("expected Negation, got {result:?}");
        };
        assert_eq!(*base, InferredType::known("Symbol"));
        assert_eq!(*excluded, InferredType::known("#foo"));
    }

    // ---- resolve_type_annotation: Self / Self class ----

    #[test]
    fn self_type_resolves_to_dynamic() {
        let ann = TypeAnnotation::SelfType { span: span() };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert!(matches!(result, InferredType::Dynamic(_)));
    }

    #[test]
    fn self_class_without_self_binding_resolves_to_dynamic() {
        // ADR 0083: `Self class` needs the enclosing class threaded under the
        // reserved `Self` subst key. Without it (the free-function default),
        // it still falls back to Dynamic so existing patterns keep working.
        let ann = TypeAnnotation::SelfClass { span: span() };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert!(matches!(result, InferredType::Dynamic(_)));
    }

    #[test]
    fn self_class_with_self_binding_resolves_to_metatype() {
        // ADR 0083: when callers thread the enclosing class under `Self`,
        // `Self class` resolves to the metatype of that class.
        let ann = TypeAnnotation::SelfClass { span: span() };
        let mut subst = SubstitutionMap::new();
        subst.insert("Self".into(), InferredType::known("Counter"));
        let result = resolve_type_annotation(&ann, &subst, None, None);
        assert_eq!(result.as_meta().map(EcoString::as_str), Some("Counter"));
    }

    #[test]
    fn class_of_resolves_to_metatype() {
        // ADR 0083: `<Name> class` resolves to the metatype of the named class
        // (was Dynamic pre-0083, BT-2034). The metatype subtypes into the
        // tower so class-side sends route through `find_class_method`.
        let ann = TypeAnnotation::ClassOf {
            class_name: ident("Actor"),
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result.as_meta().map(EcoString::as_str), Some("Actor"));
    }

    // ---- resolve_type_annotation: singleton ----

    #[test]
    fn singleton_resolves_with_hash_prefix() {
        let ann = TypeAnnotation::Singleton {
            name: "ok".into(),
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::known("#ok"));
    }

    // ---- resolve_type_annotation: type alias expansion (ADR 0108, BT-2895) ----

    /// Builds an `AliasRegistry` containing a single alias `name = annotation`.
    fn alias_registry_with(name: &str, annotation: TypeAnnotation) -> AliasRegistry {
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: name.into(),
            annotation,
            is_internal: false,
            package: None,
            span: span(),
        });
        registry
    }

    #[test]
    fn alias_reference_expands_to_singleton_union() {
        // `type RestartStrategy = #temporary | #transient | #permanent` —
        // referencing `RestartStrategy` must resolve to the exact same
        // `InferredType::Union` the spelled-out union would.
        let expansion = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Singleton {
                    name: "temporary".into(),
                    span: span(),
                },
                TypeAnnotation::Singleton {
                    name: "transient".into(),
                    span: span(),
                },
                TypeAnnotation::Singleton {
                    name: "permanent".into(),
                    span: span(),
                },
            ],
            span: span(),
        };
        let registry = alias_registry_with("RestartStrategy", expansion.clone());

        let ann = TypeAnnotation::Simple(ident("RestartStrategy"));
        let aliased_result = resolve_type_annotation(&ann, &empty_subst(), None, Some(&registry));
        let spelled_out_result = resolve_type_annotation(&expansion, &empty_subst(), None, None);

        assert_eq!(
            aliased_result, spelled_out_result,
            "an alias reference must resolve identically to its spelled-out expansion"
        );
        let InferredType::Union { members, .. } = aliased_result else {
            panic!("expected Union, got {aliased_result:?}");
        };
        assert_eq!(members.len(), 3);
        assert!(members.contains(&InferredType::known("#temporary")));
    }

    #[test]
    fn alias_reference_without_registry_resolves_as_nominal_class() {
        // Pre-ADR-0108 behaviour is preserved when no alias registry is
        // supplied: a `Simple` name just resolves as an ordinary (unknown)
        // nominal class.
        let ann = TypeAnnotation::Simple(ident("RestartStrategy"));
        let result = resolve_type_annotation(&ann, &empty_subst(), None, None);
        assert_eq!(result, InferredType::known("RestartStrategy"));
    }

    #[test]
    fn alias_reference_inside_union_member_expands() {
        // `type Timeout = Integer | #infinity`, referenced as one member of
        // a larger annotation (`Timeout | Nil`) — nested `Simple` positions
        // must also consult the alias table.
        let timeout_expansion = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Singleton {
                    name: "infinity".into(),
                    span: span(),
                },
            ],
            span: span(),
        };
        let registry = alias_registry_with("Timeout", timeout_expansion);

        let ann = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("Timeout")),
                TypeAnnotation::Simple(ident("nil")),
            ],
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, Some(&registry));
        let InferredType::Union { members, .. } = result else {
            panic!("expected Union, got {result:?}");
        };
        assert_eq!(members.len(), 3, "Integer, #infinity, UndefinedObject");
        assert!(members.contains(&InferredType::known("Integer")));
        assert!(members.contains(&InferredType::known("#infinity")));
        assert!(members.contains(&InferredType::known("UndefinedObject")));
    }

    #[test]
    fn chained_alias_reference_expands_through_both_levels() {
        // `type B = A | #z`, `type A = #x | #y` — referencing `B` must
        // expand all the way down to `#x | #y | #z`, not stop at `A`.
        let a_expansion = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Singleton {
                    name: "x".into(),
                    span: span(),
                },
                TypeAnnotation::Singleton {
                    name: "y".into(),
                    span: span(),
                },
            ],
            span: span(),
        };
        let b_expansion = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("A")),
                TypeAnnotation::Singleton {
                    name: "z".into(),
                    span: span(),
                },
            ],
            span: span(),
        };
        let mut registry = alias_registry_with("A", a_expansion);
        registry.register_test_alias(AliasInfo {
            name: "B".into(),
            annotation: b_expansion,
            is_internal: false,
            package: None,
            span: span(),
        });

        let ann = TypeAnnotation::Simple(ident("B"));
        let result = resolve_type_annotation(&ann, &empty_subst(), None, Some(&registry));
        let InferredType::Union { members, .. } = result else {
            panic!("expected Union, got {result:?}");
        };
        assert_eq!(members.len(), 3);
        assert!(members.contains(&InferredType::known("#x")));
        assert!(members.contains(&InferredType::known("#y")));
        assert!(members.contains(&InferredType::known("#z")));
    }

    // ---- resolve_type_annotation_with_alias_deps (ADR 0108 hot-reload
    // re-check trigger, BT-2899) ----

    #[test]
    fn alias_deps_records_both_levels_of_a_chained_alias() {
        // `type B = A | #z`, `type A = #x | #y` — resolving `p :: B` must
        // record dependency edges for *both* `B` and `A` (ADR 0108
        // Implementation: "recording only the outermost written name would
        // leave a live redefinition of `A` alone unable to trigger a
        // re-check of `p`'s site").
        let a_expansion = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Singleton {
                    name: "x".into(),
                    span: span(),
                },
                TypeAnnotation::Singleton {
                    name: "y".into(),
                    span: span(),
                },
            ],
            span: span(),
        };
        let b_expansion = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("A")),
                TypeAnnotation::Singleton {
                    name: "z".into(),
                    span: span(),
                },
            ],
            span: span(),
        };
        let mut registry = alias_registry_with("A", a_expansion);
        registry.register_test_alias(AliasInfo {
            name: "B".into(),
            annotation: b_expansion,
            is_internal: false,
            package: None,
            span: span(),
        });

        let ann = TypeAnnotation::Simple(ident("B"));
        let (_resolved, deps) =
            resolve_type_annotation_with_alias_deps(&ann, &empty_subst(), None, Some(&registry));

        assert_eq!(
            deps,
            vec![EcoString::from("A"), EcoString::from("B")],
            "expected both transitively-referenced alias names, sorted: {deps:?}"
        );
    }

    #[test]
    fn alias_deps_is_empty_for_a_plain_nominal_annotation() {
        // No alias registry consulted, or a name that isn't an alias at
        // all — no dependency edges to record.
        let ann = TypeAnnotation::Simple(ident("Integer"));
        let (_resolved, deps) =
            resolve_type_annotation_with_alias_deps(&ann, &empty_subst(), None, None);
        assert!(deps.is_empty(), "expected no deps, got: {deps:?}");

        let registry = alias_registry_with("A", TypeAnnotation::Simple(ident("Integer")));
        let (_resolved, deps) =
            resolve_type_annotation_with_alias_deps(&ann, &empty_subst(), None, Some(&registry));
        assert!(
            deps.is_empty(),
            "a plain nominal name must not be recorded as an alias dep: {deps:?}"
        );
    }

    #[test]
    fn alias_deps_records_every_member_of_a_union_of_aliases() {
        // `type Combined = X | Y` where both `X` and `Y` are themselves
        // aliases — every alias name touched anywhere in the annotation
        // must be recorded, not just the first.
        let mut registry = alias_registry_with("X", TypeAnnotation::Simple(ident("Integer")));
        registry.register_test_alias(AliasInfo {
            name: "Y".into(),
            annotation: TypeAnnotation::Simple(ident("String")),
            is_internal: false,
            package: None,
            span: span(),
        });
        let ann = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("X")),
                TypeAnnotation::Simple(ident("Y")),
            ],
            span: span(),
        };
        let (_resolved, deps) =
            resolve_type_annotation_with_alias_deps(&ann, &empty_subst(), None, Some(&registry));
        assert_eq!(deps, vec![EcoString::from("X"), EcoString::from("Y")]);
    }

    #[test]
    fn repeated_alias_reference_in_one_annotation_both_display_the_alias_name() {
        // `A | A` — the second reference hits `resolve_type_annotation_inner`'s
        // per-call `memo` cache (keyed by alias name), reusing the *first*
        // reference's already-tagged value rather than re-expanding. This
        // pins that the reused value still displays correctly (adversarial
        // review, BT-2897): both members carry the same `Aliased("A", …)`
        // tag, so the union simplifies to the single tagged member, not two
        // untagged copies.
        let registry = alias_registry_with("A", TypeAnnotation::Simple(ident("Integer")));
        let ann = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("A")),
                TypeAnnotation::Simple(ident("A")),
            ],
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst(), None, Some(&registry));
        // `A | A` structurally collapses to a single `Integer` (dedup).
        assert_eq!(result, InferredType::known("Integer"));
        assert_eq!(
            result.display_for_diagnostic().unwrap(),
            "A (Integer)",
            "the reused, memoized value must still carry the alias tag"
        );
    }

    #[test]
    fn subst_wins_over_alias_table_for_same_name() {
        // ADR 0108 resolution order: `subst` (type params) → alias table →
        // nominal class. Disjoint by construction (single-letter alias names
        // are a declaration error), but the resolver itself must still
        // honour the documented order if a caller ever supplies both for
        // the same key.
        let registry = alias_registry_with("Shadowed", TypeAnnotation::Simple(ident("String")));
        let mut subst = SubstitutionMap::new();
        subst.insert("Shadowed".into(), InferredType::known("Integer"));

        let ann = TypeAnnotation::Simple(ident("Shadowed"));
        let result = resolve_type_annotation(&ann, &subst, None, Some(&registry));
        assert_eq!(
            result,
            InferredType::known("Integer"),
            "subst must win over the alias table"
        );
    }

    #[test]
    fn cyclic_alias_reference_resolves_to_dynamic_instead_of_hanging() {
        // ADR 0108 "No recursion": real declaration-time cycle *detection*
        // (BT-2896) lives in `AliasRegistry::register_module` /
        // `redefine_alias`, which this test bypasses via the test-only
        // `register_test_alias` — so this exercises the defensive
        // `expanding` guard in isolation, standing in for a cycle that
        // somehow slipped past declaration-time checking. `type A = B`,
        // `type B = A`.
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "A".into(),
            annotation: TypeAnnotation::Simple(ident("B")),
            is_internal: false,
            package: None,
            span: span(),
        });
        registry.register_test_alias(AliasInfo {
            name: "B".into(),
            annotation: TypeAnnotation::Simple(ident("A")),
            is_internal: false,
            package: None,
            span: span(),
        });

        let ann = TypeAnnotation::Simple(ident("A"));
        let result = resolve_type_annotation(&ann, &empty_subst(), None, Some(&registry));
        assert!(
            matches!(result, InferredType::Dynamic(_)),
            "a cyclic alias must resolve to Dynamic, not hang: {result:?}"
        );
    }

    #[test]
    fn doubling_alias_chain_does_not_blow_up_exponentially() {
        // A legal, acyclic chain where each level references the previous
        // one *twice* — `type L1 = L0 | L0`, `type L2 = L1 | L1`, … — has no
        // cycle for `expanding` to catch, but re-expanding `L(n-1)` from
        // scratch on both sides of every `|` is `O(2ⁿ)` work without
        // memoization. 40 levels finishes near-instantly with the `memo`
        // cache; without it, this test would not complete in this suite's
        // lifetime. Asserts both a bounded runtime and a correct result
        // (the union always dedups down to the same two singleton members).
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "L0".into(),
            annotation: TypeAnnotation::Union {
                types: vec![
                    TypeAnnotation::Singleton {
                        name: "a".into(),
                        span: span(),
                    },
                    TypeAnnotation::Singleton {
                        name: "b".into(),
                        span: span(),
                    },
                ],
                span: span(),
            },
            is_internal: false,
            package: None,
            span: span(),
        });
        for level in 1..=40 {
            let prev = format!("L{}", level - 1);
            registry.register_test_alias(AliasInfo {
                name: format!("L{level}").into(),
                annotation: TypeAnnotation::Union {
                    types: vec![
                        TypeAnnotation::Simple(ident(&prev)),
                        TypeAnnotation::Simple(ident(&prev)),
                    ],
                    span: span(),
                },
                is_internal: false,
                package: None,
                span: span(),
            });
        }

        let ann = TypeAnnotation::Simple(ident("L40"));
        let start = std::time::Instant::now();
        let result = resolve_type_annotation(&ann, &empty_subst(), None, Some(&registry));
        assert!(
            start.elapsed() < std::time::Duration::from_secs(5),
            "resolving a 40-level doubling alias chain must stay near-instant with memoization"
        );
        let InferredType::Union { members, .. } = result else {
            panic!("expected Union, got {result:?}");
        };
        assert_eq!(members.len(), 2);
        assert!(members.contains(&InferredType::known("#a")));
        assert!(members.contains(&InferredType::known("#b")));
    }

    // ---- receiver_type_for_class ----
    //
    // The tests below exercise built-in classes only, so they do not depend
    // on AST wiring. The built-in hierarchy contains:
    //   * `Integer`       — non-generic (smoke test for the no-type-args path)
    //   * `Array`         — generic with one type param `E`
    //   * `Dictionary`    — generic with two type params `K`, `V`
    //   * `Collection`    — abstract generic with one type param `E`
    // Anything not in the hierarchy falls through the "unknown class" branch.

    #[test]
    fn receiver_for_non_generic_class_has_no_type_args() {
        let hierarchy = builtin_hierarchy();
        let result = receiver_type_for_class(&"Integer".into(), &hierarchy);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Integer");
        assert!(
            type_args.is_empty(),
            "non-generic class must have no type args"
        );
    }

    #[test]
    fn receiver_for_generic_class_synthesises_symbolic_type_args() {
        // `Dictionary(K, V)` should receive two symbolic type_args
        // `Known("K", [])` / `Known("V", [])` so substitution can later map
        // them to concrete bindings.
        let hierarchy = builtin_hierarchy();
        let result = receiver_type_for_class(&"Dictionary".into(), &hierarchy);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Dictionary");
        assert_eq!(type_args.len(), 2);
        assert_eq!(type_args[0], InferredType::known("K"));
        assert_eq!(type_args[1], InferredType::known("V"));
    }

    #[test]
    fn receiver_for_single_param_generic_class() {
        // `Array(E)` — covers the single-param generic case.
        let hierarchy = builtin_hierarchy();
        let result = receiver_type_for_class(&"Array".into(), &hierarchy);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Array");
        assert_eq!(type_args, vec![InferredType::known("E")]);
    }

    #[test]
    fn receiver_for_unknown_class_falls_back_to_bare_known() {
        // Class not in the hierarchy — resolver must not panic, and returns a
        // bare Known with no type args so call-site receiver checks keep
        // working.
        let hierarchy = builtin_hierarchy();
        let result = receiver_type_for_class(&"ClassThatDoesNotExist".into(), &hierarchy);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "ClassThatDoesNotExist");
        assert!(type_args.is_empty());
    }

    #[test]
    fn receiver_for_abstract_class_still_threads_type_params() {
        // `Collection(E)` is abstract — the helper must still thread the
        // declared type param through.
        let hierarchy = builtin_hierarchy();
        let result = receiver_type_for_class(&"Collection".into(), &hierarchy);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Collection");
        // The builtin `Collection` class declares type params via the generated
        // hierarchy; we just assert the arity matches the class's declaration.
        let expected_arity = hierarchy
            .get_class("Collection")
            .map_or(0, |info| info.type_params.len());
        assert_eq!(type_args.len(), expected_arity);
        // Every synthesised arg is itself a `Known { type_args: [] }` with the
        // param's literal name.
        for (arg, param) in type_args.iter().zip(
            hierarchy
                .get_class("Collection")
                .expect("Collection must be in builtin hierarchy")
                .type_params
                .iter(),
        ) {
            let InferredType::Known {
                class_name: arg_name,
                type_args: inner_args,
                ..
            } = arg
            else {
                panic!("expected symbolic Known for type param");
            };
            assert_eq!(arg_name, param);
            assert!(inner_args.is_empty());
        }
    }

    // ---- super_receiver_type ----
    //
    // The tests below exercise BT-2021 sub-bug B's fix path: `super` sends
    // must thread the *child's* type-arg bindings into the parent's
    // type-param positions via `superclass_type_args` (`ParamRef` for
    // forwarded type params, `Concrete` for fixed types).

    fn parse_module(source: &str) -> (crate::ast::Module, ClassHierarchy) {
        let tokens = crate::source_analysis::lex_with_eof(source);
        let (module, diags) = crate::source_analysis::parse(tokens);
        assert!(diags.is_empty(), "Parse failed: {diags:?}");
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        (module, hierarchy)
    }

    #[test]
    fn super_receiver_threads_child_param_through_param_ref() {
        // `Sub(R) extends Base(R)` — Sub's R forwards to Base's E.
        // Calling super from inside Sub(R) with self typed as
        // Known("Sub", [Known("R")]) should produce Known("Base", [Known("R")]).
        let (_, hierarchy) = parse_module(
            "Object subclass: Base(E)\n  state: x :: E = nil\n\nBase(R) subclass: Sub(R)\n",
        );
        let child_type_args = vec![InferredType::known("R")];
        let result = super_receiver_type(
            &"Sub".into(),
            &child_type_args,
            &"Base".into(),
            &hierarchy,
            None,
        );
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Base");
        assert_eq!(type_args, vec![InferredType::known("R")]);
    }

    #[test]
    fn super_receiver_substitutes_concrete_superclass_arg() {
        // `IntBase extends Base(Integer)` — Integer is fixed; Sub has no type
        // params of its own. super's receiver must carry the concrete Integer.
        let (_, hierarchy) = parse_module(
            "Object subclass: Base(E)\n  state: x :: E = nil\n\nBase(Integer) subclass: IntBase\n",
        );
        let result = super_receiver_type(&"IntBase".into(), &[], &"Base".into(), &hierarchy, None);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Base");
        assert_eq!(type_args, vec![InferredType::known("Integer")]);
    }

    /// BT-2936: `IntBase extends Base(SomeAlias)` where `type SomeAlias =
    /// Integer` — the `Concrete` superclass type arg expands the alias
    /// through the threaded `alias_registry` instead of staying an opaque
    /// nominal class named `SomeAlias`.
    #[test]
    fn super_receiver_expands_alias_typed_concrete_superclass_arg() {
        let (_, hierarchy) = parse_module(
            "Object subclass: Base(E)\n  state: x :: E = nil\n\nBase(SomeAlias) subclass: IntBase\n",
        );
        let registry = alias_registry_with("SomeAlias", TypeAnnotation::Simple(ident("Integer")));
        let result = super_receiver_type(
            &"IntBase".into(),
            &[],
            &"Base".into(),
            &hierarchy,
            Some(&registry),
        );
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Base");
        assert_eq!(type_args, vec![InferredType::known("Integer")]);

        // Without the registry, the alias name stays opaque — the
        // pre-BT-2936 fallback this test guards against regressing back to.
        let unresolved =
            super_receiver_type(&"IntBase".into(), &[], &"Base".into(), &hierarchy, None);
        let InferredType::Known { type_args, .. } = unresolved else {
            panic!("expected Known");
        };
        assert_eq!(type_args, vec![InferredType::known("SomeAlias")]);
    }

    #[test]
    fn super_receiver_falls_back_when_no_extends_annotation() {
        // Non-generic child of non-generic parent — no superclass_type_args.
        // super's receiver should match `receiver_type_for_class(parent)`.
        let (_, hierarchy) = parse_module("Object subclass: Parent\n\nParent subclass: Child\n");
        let result = super_receiver_type(&"Child".into(), &[], &"Parent".into(), &hierarchy, None);
        let expected = receiver_type_for_class(&"Parent".into(), &hierarchy);
        assert_eq!(result, expected);
    }

    #[test]
    fn super_receiver_unknown_child_falls_back_safely() {
        // Child class not in hierarchy — must not panic; falls back to the
        // parent's symbolic placeholders.
        let (_, hierarchy) = parse_module("Object subclass: Base(E)\n");
        let result = super_receiver_type(
            &"NonExistentChild".into(),
            &[InferredType::known("R")],
            &"Base".into(),
            &hierarchy,
            None,
        );
        let expected = receiver_type_for_class(&"Base".into(), &hierarchy);
        assert_eq!(result, expected);
    }

    // ---- base_name_of_string ----
    //
    // `split_generic_base` itself is tested once, at its definition site in
    // `string_utils` (BT-3089) — no need to re-test it by proxy here.

    #[test]
    fn base_name_of_string_discards_args() {
        assert_eq!(base_name_of_string("Dictionary(K, V)"), "Dictionary");
        assert_eq!(base_name_of_string("Integer"), "Integer");
    }

    // ---- resolve_declared_type (BT-3076 stage 2) ----
    //
    // `resolve_type_annotation` is now a thin `DeclaredType::from` + this
    // function wrapper (always `TypeStringContext::Declared`), so every test
    // above already exercises this recursion indirectly. These tests target
    // `resolve_declared_type` directly, including the `Substitution` context
    // no existing `TypeAnnotation`-based caller reaches yet.

    #[test]
    fn declared_type_simple_matches_annotation_resolution() {
        let dt = DeclaredType::Simple("Integer".into());
        let result =
            resolve_declared_type(&dt, &empty_subst(), None, None, TypeStringContext::Declared);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn declared_type_never_and_dynamic_keywords() {
        let never = resolve_declared_type(
            &DeclaredType::Simple("Never".into()),
            &empty_subst(),
            None,
            None,
            TypeStringContext::Declared,
        );
        assert_eq!(never, InferredType::Never);

        let dynamic = resolve_declared_type(
            &DeclaredType::Simple("Dynamic".into()),
            &empty_subst(),
            None,
            None,
            TypeStringContext::Declared,
        );
        assert!(matches!(dynamic, InferredType::Dynamic(_)));
    }

    #[test]
    fn declared_type_generic_preserves_type_args() {
        let dt = DeclaredType::Generic {
            base: "Result".into(),
            parameters: vec![
                DeclaredType::Simple("Integer".into()),
                DeclaredType::Simple("String".into()),
            ],
        };
        let result =
            resolve_declared_type(&dt, &empty_subst(), None, None, TypeStringContext::Declared);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = result
        else {
            panic!("expected Known");
        };
        assert_eq!(class_name.as_str(), "Result");
        assert_eq!(type_args.len(), 2);
    }

    #[test]
    fn declared_type_singleton_resolves_with_hash_prefix() {
        let dt = DeclaredType::Singleton("ok".into());
        let result =
            resolve_declared_type(&dt, &empty_subst(), None, None, TypeStringContext::Declared);
        assert_eq!(result, InferredType::known("#ok"));
    }

    #[test]
    fn declared_type_substitution_context_collapses_bare_type_param_to_dynamic() {
        // BT-1834: an unmapped bare single-letter param collapses to Dynamic
        // only in `Substitution` context — `Declared` context (what
        // `resolve_type_annotation` always uses) passes it through unchanged
        // as a nominal class name.
        let dt = DeclaredType::Simple("T".into());
        let declared =
            resolve_declared_type(&dt, &empty_subst(), None, None, TypeStringContext::Declared);
        assert_eq!(declared, InferredType::known("T"));

        let substituted = resolve_declared_type(
            &dt,
            &empty_subst(),
            None,
            None,
            TypeStringContext::Substitution,
        );
        assert!(matches!(substituted, InferredType::Dynamic(_)));
    }

    #[test]
    fn declared_type_substitution_context_still_honours_subst_map() {
        // Substitution wins over the bare-param collapse when the map does
        // have an entry for the name.
        let dt = DeclaredType::Simple("T".into());
        let mut subst = SubstitutionMap::new();
        subst.insert("T".into(), InferredType::known("Integer"));
        let result =
            resolve_declared_type(&dt, &subst, None, None, TypeStringContext::Substitution);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn declared_type_alias_expansion_matches_annotation_based_resolution() {
        let expansion = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Singleton {
                    name: "temporary".into(),
                    span: span(),
                },
                TypeAnnotation::Singleton {
                    name: "permanent".into(),
                    span: span(),
                },
            ],
            span: span(),
        };
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "RestartStrategy".into(),
            annotation: expansion,
            is_internal: false,
            package: None,
            span: span(),
        });

        let dt = DeclaredType::Simple("RestartStrategy".into());
        let via_declared_type = resolve_declared_type(
            &dt,
            &empty_subst(),
            None,
            Some(&registry),
            TypeStringContext::Declared,
        );

        let ann = TypeAnnotation::Simple(ident("RestartStrategy"));
        let via_annotation = resolve_type_annotation(&ann, &empty_subst(), None, Some(&registry));

        assert_eq!(via_declared_type, via_annotation);
        let InferredType::Union { members, .. } = via_declared_type else {
            panic!("expected Union");
        };
        assert_eq!(members.len(), 2);
    }

    #[test]
    fn declared_type_with_alias_deps_records_transitive_names() {
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "A".into(),
            annotation: TypeAnnotation::Simple(ident("Integer")),
            is_internal: false,
            package: None,
            span: span(),
        });
        registry.register_test_alias(AliasInfo {
            name: "B".into(),
            annotation: TypeAnnotation::Simple(ident("A")),
            is_internal: false,
            package: None,
            span: span(),
        });

        let dt = DeclaredType::Simple("B".into());
        let (resolved, deps) = resolve_declared_type_with_alias_deps(
            &dt,
            &empty_subst(),
            None,
            Some(&registry),
            TypeStringContext::Declared,
        );
        assert_eq!(resolved, InferredType::known("Integer"));
        assert_eq!(deps, vec![EcoString::from("A"), EcoString::from("B")]);
    }

    #[test]
    fn declared_type_union_in_substitution_context_stamps_substituted_provenance() {
        // Mirrors the deleted `resolve_type_string`'s own `Substitution`-
        // context re-stamping (BT-3075): a substituted `V | Nil` reads as
        // `Substituted`, not `Inferred`, so `check_impossible_class_comparison`'s
        // provenance gate stays silent for it.
        let dt = DeclaredType::Union(vec![
            DeclaredType::Simple("Integer".into()),
            DeclaredType::Simple("String".into()),
        ]);
        let result = resolve_declared_type(
            &dt,
            &empty_subst(),
            None,
            None,
            TypeStringContext::Substitution,
        );
        assert_eq!(
            result.provenance(),
            Some(TypeProvenance::Substituted(Span::default()))
        );
    }

    #[test]
    fn declared_type_difference_and_intersection_match_annotation_resolution() {
        let dt_diff = DeclaredType::Difference {
            base: Box::new(DeclaredType::Simple("Symbol".into())),
            excluded: Box::new(DeclaredType::Singleton("foo".into())),
        };
        let ann_diff = TypeAnnotation::difference(
            TypeAnnotation::Simple(ident("Symbol")),
            TypeAnnotation::Singleton {
                name: "foo".into(),
                span: span(),
            },
            span(),
        );
        assert_eq!(
            resolve_declared_type(
                &dt_diff,
                &empty_subst(),
                None,
                None,
                TypeStringContext::Declared
            ),
            resolve_type_annotation(&ann_diff, &empty_subst(), None, None)
        );

        let dt_inter = DeclaredType::Intersection {
            left: Box::new(DeclaredType::Simple("Integer".into())),
            right: Box::new(DeclaredType::Simple("Integer".into())),
        };
        let ann_inter = TypeAnnotation::intersection(
            TypeAnnotation::Simple(ident("Integer")),
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        );
        assert_eq!(
            resolve_declared_type(
                &dt_inter,
                &empty_subst(),
                None,
                None,
                TypeStringContext::Declared
            ),
            resolve_type_annotation(&ann_inter, &empty_subst(), None, None)
        );
    }
}
