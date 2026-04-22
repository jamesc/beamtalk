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
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;

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
///   parameters resolve to their concrete bindings).
/// * [`TypeAnnotation::Generic`] — builds `Known { class_name: base, type_args }`
///   with every argument recursively resolved through the same rules. Provenance
///   is marked `Declared(span)` since the annotation is user-written.
/// * [`TypeAnnotation::Union`] — resolves each member, then folds through
///   [`InferredType::union_of`] which handles flattening and deduplication.
/// * [`TypeAnnotation::FalseOr`] — `inner | False`, using the same union
///   machinery.
/// * [`TypeAnnotation::SelfType`] — returns `Dynamic(Unknown)`. Bare `Self`
///   is resolved at the *call site* where the static receiver class is known;
///   the annotation-resolution pass does not have that context.
/// * [`TypeAnnotation::SelfClass`] — returns `Dynamic(Unknown)`. `Self class`
///   is a metatype that historically resolves to Dynamic here (see BT-1952) so
///   that existing patterns like `x class = Foo` keep working; the call-site
///   metatype wrapping lives in the caller.
/// * [`TypeAnnotation::Singleton`] — `#name` becomes `Known("#name", [])`,
///   matching the existing singleton-as-Symbol-subtype convention.
///
/// The class hierarchy is *not* consulted during annotation resolution —
/// "does this class exist?" is a call-site decision that happens *after* the
/// annotation resolves, not during. A future evolution may thread one in for
/// type-alias resolution; no current caller needs it.
pub(in crate::semantic_analysis) fn resolve_type_annotation(
    ann: &TypeAnnotation,
    subst: &SubstitutionMap,
) -> InferredType {
    match ann {
        TypeAnnotation::Simple(type_id) => {
            let name = &type_id.name;
            if name.as_str() == "Never" {
                return InferredType::Never;
            }
            // Method-local / class-level type-parameter substitution wins over
            // keyword resolution, because `T`, `E`, `R` would otherwise flow
            // through as ordinary class names.
            if let Some(resolved) = subst.get(name) {
                return resolved.clone();
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
                .map(|p| resolve_type_annotation(p, subst))
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
                .map(|t| resolve_type_annotation(t, subst))
                .collect();
            InferredType::union_of(&members)
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            let inner_ty = resolve_type_annotation(inner, subst);
            InferredType::union_of(&[inner_ty, InferredType::known("False")])
        }
        TypeAnnotation::SelfType { .. } | TypeAnnotation::SelfClass { .. } => {
            // `Self` / `Self class` need the static receiver class, which the
            // annotation-resolver does not have. Call sites that do know it
            // (e.g. return-type validation, nested-Self substitution) handle
            // these cases directly.
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
        "nil" | "Nil" => "UndefinedObject".into(),
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
        let result = resolve_type_annotation(&ann, &empty_subst());
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn simple_nil_keyword_resolves_to_undefined_object() {
        let ann = TypeAnnotation::Simple(ident("nil"));
        let result = resolve_type_annotation(&ann, &empty_subst());
        assert_eq!(result, InferredType::known("UndefinedObject"));
    }

    #[test]
    fn simple_capital_nil_keyword_resolves_to_undefined_object() {
        // `Integer | Nil` annotations (BT-2016) — the capital-N spelling must
        // canonicalize to the same class as lowercase `nil` so narrowing under
        // `isNil` works regardless of which the user typed.
        let ann = TypeAnnotation::Simple(ident("Nil"));
        let result = resolve_type_annotation(&ann, &empty_subst());
        assert_eq!(result, InferredType::known("UndefinedObject"));
    }

    #[test]
    fn simple_true_false_keywords_resolve() {
        let true_ann = TypeAnnotation::Simple(ident("true"));
        let false_ann = TypeAnnotation::Simple(ident("false"));
        assert_eq!(
            resolve_type_annotation(&true_ann, &empty_subst()),
            InferredType::known("True")
        );
        assert_eq!(
            resolve_type_annotation(&false_ann, &empty_subst()),
            InferredType::known("False")
        );
    }

    #[test]
    fn simple_never_keyword_resolves_to_never() {
        let ann = TypeAnnotation::Simple(ident("Never"));
        let result = resolve_type_annotation(&ann, &empty_subst());
        assert_eq!(result, InferredType::Never);
    }

    // ---- resolve_type_annotation: substitution ----

    #[test]
    fn simple_type_param_substitutes_from_map() {
        let ann = TypeAnnotation::Simple(ident("T"));
        let mut subst = SubstitutionMap::new();
        subst.insert("T".into(), InferredType::known("Integer"));
        let result = resolve_type_annotation(&ann, &subst);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn simple_unmapped_type_param_passes_through() {
        // Type params without a substitution entry pass through as-is so that
        // downstream code (e.g. `is_generic_type_param`-aware call sites) can
        // decide whether to fall back to Dynamic.
        let ann = TypeAnnotation::Simple(ident("T"));
        let result = resolve_type_annotation(&ann, &empty_subst());
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
        let result = resolve_type_annotation(&ann, &empty_subst());
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
        let result = resolve_type_annotation(&ann, &empty_subst());
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
        let result = resolve_type_annotation(&ann, &subst);
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
        let result = resolve_type_annotation(&ann, &empty_subst());
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
        let result = resolve_type_annotation(&ann, &empty_subst());
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
        let result = resolve_type_annotation(&ann, &empty_subst());
        let InferredType::Union { members, .. } = result else {
            panic!("expected Union, got {result:?}");
        };
        assert_eq!(members.len(), 2);
        assert!(members.contains(&InferredType::known("Integer")));
        assert!(members.contains(&InferredType::known("False")));
    }

    // ---- resolve_type_annotation: Self / Self class ----

    #[test]
    fn self_type_resolves_to_dynamic() {
        let ann = TypeAnnotation::SelfType { span: span() };
        let result = resolve_type_annotation(&ann, &empty_subst());
        assert!(matches!(result, InferredType::Dynamic(_)));
    }

    #[test]
    fn self_class_resolves_to_dynamic() {
        let ann = TypeAnnotation::SelfClass { span: span() };
        let result = resolve_type_annotation(&ann, &empty_subst());
        assert!(matches!(result, InferredType::Dynamic(_)));
    }

    // ---- resolve_type_annotation: singleton ----

    #[test]
    fn singleton_resolves_with_hash_prefix() {
        let ann = TypeAnnotation::Singleton {
            name: "ok".into(),
            span: span(),
        };
        let result = resolve_type_annotation(&ann, &empty_subst());
        assert_eq!(result, InferredType::known("#ok"));
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
