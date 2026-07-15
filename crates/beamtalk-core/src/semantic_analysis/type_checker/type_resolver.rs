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
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

use super::well_known::WellKnownClass;
use super::{DynamicReason, InferredType, TypeProvenance};

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
    let mut expanding = Vec::new();
    let mut memo = HashMap::new();
    resolve_type_annotation_inner(
        ann,
        subst,
        protocol_registry,
        alias_registry,
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
/// e.g. `type A = B`, `type B = A`) is BT-2896, deliberately out of scope
/// here (ADR 0108 "No recursion" explicitly assigns it to a later issue).
/// But eager expansion means a cycle that slips past declaration-time
/// checking (or simply hasn't been checked yet, since BT-2896 hasn't landed)
/// would otherwise recurse forever the first time it's *referenced* — this
/// guard is the "expansion itself carries a visited-set guard so a cycle
/// that slips through is a diagnostic, never a hang" promise from that same
/// ADR section, satisfied here defensively: a name already being expanded on
/// this path resolves to `Dynamic` instead of recursing again.
///
/// `memo` exists for a different reason: without it, a *legal, acyclic*
/// chain of aliases that each reference the previous one twice —
/// `type L1 = L0 | L0`, `type L2 = L1 | L1`, …, `type Ln = L(n-1) | L(n-1)`
/// — re-expands `L(n-1)` from scratch on both sides of every `|`, doubling
/// the work at each level (`O(2ⁿ)` total). `expanding` (a path stack) does
/// not prevent this — it only rejects a name that is an *ancestor* of
/// itself, and `L(n-1)`'s two references here are siblings, not nested.
/// Since resolution is a pure function of `(annotation, subst,
/// alias_registry)` and `subst` never changes across a single top-level
/// [`resolve_type_annotation`] call (every recursive call above threads the
/// same `subst` unchanged), caching by alias name alone is sound *within*
/// one call: the first full resolution of a name is reused verbatim for
/// every subsequent reference, turning the exponential re-walk into one
/// resolution per distinct alias name.
#[allow(clippy::too_many_lines)] // one match arm per TypeAnnotation variant — see resolve_type_annotation's doc
fn resolve_type_annotation_inner(
    ann: &TypeAnnotation,
    subst: &SubstitutionMap,
    protocol_registry: Option<&ProtocolRegistry>,
    alias_registry: Option<&AliasRegistry>,
    expanding: &mut Vec<EcoString>,
    memo: &mut HashMap<EcoString, InferredType>,
) -> InferredType {
    match ann {
        TypeAnnotation::Simple(type_id) => {
            let name = &type_id.name;
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
            if WellKnownClass::from_str(name) == Some(WellKnownClass::Dynamic) {
                return InferredType::Dynamic(DynamicReason::ExplicitDynamic);
            }
            // Method-local / class-level type-parameter substitution wins over
            // keyword resolution, because `T`, `E`, `R` would otherwise flow
            // through as ordinary class names.
            if let Some(resolved) = subst.get(name) {
                return resolved.clone();
            }
            // ADR 0108 / BT-2895: alias table comes next in the resolution
            // order (`subst` → alias table → nominal class). A reference to
            // an alias name expands eagerly to its declared annotation,
            // recursively resolved through this same function so a chain of
            // aliases (`type B = A | #z`, `type A = #x | #y`) resolves all
            // the way down to a plain structural `InferredType`.
            if let Some(registry) = alias_registry {
                if let Some(alias_info) = registry.get(name) {
                    // See `resolve_type_annotation_inner`'s doc: a name
                    // already fully resolved earlier in this call is reused
                    // verbatim, avoiding the exponential re-walk a chain of
                    // doubling aliases would otherwise trigger.
                    if let Some(cached) = memo.get(name) {
                        return cached.clone();
                    }
                    if expanding.contains(name) {
                        // A cycle slipped through (BT-2896 not yet landed, or
                        // not yet re-checked on a live redefinition) — never
                        // hang. See `resolve_type_annotation_inner`'s doc.
                        return InferredType::Dynamic(DynamicReason::Unknown);
                    }
                    expanding.push(name.clone());
                    let resolved = resolve_type_annotation_inner(
                        &alias_info.annotation,
                        subst,
                        protocol_registry,
                        alias_registry,
                        expanding,
                        memo,
                    );
                    expanding.pop();
                    memo.insert(name.clone(), resolved.clone());
                    return resolved;
                }
            }
            let resolved_name = resolve_type_keyword(name);
            InferredType::Known {
                class_name: resolved_name,
                type_args: vec![],
                provenance: TypeProvenance::Declared(ann.span()),
            }
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            let type_args: Vec<InferredType> = parameters
                .iter()
                .map(|p| {
                    resolve_type_annotation_inner(
                        p,
                        subst,
                        protocol_registry,
                        alias_registry,
                        expanding,
                        memo,
                    )
                })
                .collect();
            InferredType::Known {
                class_name: base.name.clone(),
                type_args,
                provenance: TypeProvenance::Declared(ann.span()),
            }
        }
        TypeAnnotation::Union { types, .. } => {
            let members: Vec<InferredType> = types
                .iter()
                .map(|t| {
                    resolve_type_annotation_inner(
                        t,
                        subst,
                        protocol_registry,
                        alias_registry,
                        expanding,
                        memo,
                    )
                })
                .collect();
            InferredType::union_of(&members)
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            let inner_ty = resolve_type_annotation_inner(
                inner,
                subst,
                protocol_registry,
                alias_registry,
                expanding,
                memo,
            );
            InferredType::union_of(&[inner_ty, InferredType::known("False")])
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            // ADR 0102 §1: `base \ excluded` resolves through the shared
            // `difference` set operation (BT-2739). Reducible cases normalise —
            // e.g. an excluded member that drops a union member, or `T \ T`
            // collapsing to `Never`.
            let base_ty = resolve_type_annotation_inner(
                base,
                subst,
                protocol_registry,
                alias_registry,
                expanding,
                memo,
            );
            let excluded_ty = resolve_type_annotation_inner(
                excluded,
                subst,
                protocol_registry,
                alias_registry,
                expanding,
                memo,
            );
            InferredType::difference(
                &base_ty,
                &excluded_ty,
                TypeProvenance::Declared(ann.span()),
                None,
            )
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            // ADR 0102 §1/§3, BT-2743: `left & right` resolves through the
            // shared `intersect` set operation. `hierarchy` is `None` (this
            // resolver never consults it); `protocol_registry` is passed
            // through so class ∩ protocol resolves to the stored
            // `Intersection` instead of falling through to `Never`.
            let left_ty = resolve_type_annotation_inner(
                left,
                subst,
                protocol_registry,
                alias_registry,
                expanding,
                memo,
            );
            let right_ty = resolve_type_annotation_inner(
                right,
                subst,
                protocol_registry,
                alias_registry,
                expanding,
                memo,
            );
            InferredType::intersect(
                &left_ty,
                &right_ty,
                TypeProvenance::Declared(ann.span()),
                None,
                protocol_registry,
            )
        }
        TypeAnnotation::ClassOf { class_name, .. } => {
            // ADR 0083: `<ClassName> class` is the metatype of the named class.
            // The name is carried directly in the annotation, so resolve it to a
            // dedicated `Meta` here (no enclosing-class context needed). A
            // metatype value still satisfies `:: Class` / `:: Behaviour`
            // parameters via the tower subtyping in `validation.rs`.
            InferredType::Meta {
                class_name: class_name.name.clone(),
                provenance: TypeProvenance::Declared(ann.span()),
            }
        }
        TypeAnnotation::SelfClass { .. } => {
            // ADR 0083: `Self class` is the metatype of the *enclosing* class.
            // That class isn't known to the free-function resolver, so callers
            // that have the receiver class thread it through `subst` under the
            // reserved `Self` key (see `build_self_subst`). Without it, fall
            // back to `Dynamic` so existing patterns keep working (BT-1952).
            if let Some(InferredType::Known { class_name, .. }) = subst.get("Self") {
                return InferredType::Meta {
                    class_name: class_name.clone(),
                    provenance: TypeProvenance::Declared(ann.span()),
                };
            }
            InferredType::Dynamic(DynamicReason::Unknown)
        }
        TypeAnnotation::SelfType { .. } => {
            // `Self` needs the static receiver class, which the annotation
            // resolver does not have. Call sites that do know it (e.g.
            // return-type validation, nested-Self substitution) handle it
            // directly.
            InferredType::Dynamic(DynamicReason::Unknown)
        }
        TypeAnnotation::Singleton { name, .. } => InferredType::known(eco_format!("#{name}")),
    }
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
pub(in crate::semantic_analysis) fn super_receiver_type(
    child_class: &EcoString,
    child_type_args: &[InferredType],
    parent_class: &EcoString,
    hierarchy: &ClassHierarchy,
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
            // Copilot on PR #2064: parse the type-name string so concrete
            // generics (`List(Integer)`), unions (`Integer | Nil`), and `Nil`
            // keyword aliases canonicalise correctly instead of becoming an
            // opaque `Known("List(Integer)")`.
            SuperclassTypeArg::Concrete { type_name } => {
                super::TypeChecker::resolve_type_name_string(type_name)
            }
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
/// Used by the string-form parsers (`resolve_type_name_string`,
/// `substitute_return_type_with_self`, `parse_generic_type_string`, etc.) in
/// place of manual `.find('(')` slicing. Centralising this keeps the
/// parenthesis-parsing logic in one place and lets the
/// `no .find('(')` grep check stay clean.
///
/// **References:** BT-2025.
#[must_use]
pub(in crate::semantic_analysis) fn split_generic_base(type_name: &str) -> (&str, Option<&str>) {
    match type_name.split_once('(') {
        Some((base, rest)) if rest.ends_with(')') => {
            let args = &rest[..rest.len() - 1];
            (base, Some(args))
        }
        _ => (type_name, None),
    }
}

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
        // ADR 0108 "No recursion": full cycle *detection* at declaration
        // time is BT-2896, out of scope here — but expansion itself must
        // never hang if a cycle slips through. `type A = B`, `type B = A`.
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "A".into(),
            annotation: TypeAnnotation::Simple(ident("B")),
            span: span(),
        });
        registry.register_test_alias(AliasInfo {
            name: "B".into(),
            annotation: TypeAnnotation::Simple(ident("A")),
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
        let result =
            super_receiver_type(&"Sub".into(), &child_type_args, &"Base".into(), &hierarchy);
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
        let result = super_receiver_type(&"IntBase".into(), &[], &"Base".into(), &hierarchy);
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

    #[test]
    fn super_receiver_falls_back_when_no_extends_annotation() {
        // Non-generic child of non-generic parent — no superclass_type_args.
        // super's receiver should match `receiver_type_for_class(parent)`.
        let (_, hierarchy) = parse_module("Object subclass: Parent\n\nParent subclass: Child\n");
        let result = super_receiver_type(&"Child".into(), &[], &"Parent".into(), &hierarchy);
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
        );
        let expected = receiver_type_for_class(&"Base".into(), &hierarchy);
        assert_eq!(result, expected);
    }

    // ---- split_generic_base / base_name_of_string ----

    #[test]
    fn split_generic_base_plain_name() {
        assert_eq!(split_generic_base("Integer"), ("Integer", None));
    }

    #[test]
    fn split_generic_base_single_arg() {
        assert_eq!(
            split_generic_base("Array(Integer)"),
            ("Array", Some("Integer"))
        );
    }

    #[test]
    fn split_generic_base_multiple_args() {
        assert_eq!(
            split_generic_base("Result(Integer, Error)"),
            ("Result", Some("Integer, Error"))
        );
    }

    #[test]
    fn split_generic_base_unterminated_treats_as_opaque() {
        // No closing `)` — treat the whole string as an opaque class name.
        assert_eq!(split_generic_base("Array(Integer"), ("Array(Integer", None));
    }

    #[test]
    fn base_name_of_string_discards_args() {
        assert_eq!(base_name_of_string("Dictionary(K, V)"), "Dictionary");
        assert_eq!(base_name_of_string("Integer"), "Integer");
    }
}
