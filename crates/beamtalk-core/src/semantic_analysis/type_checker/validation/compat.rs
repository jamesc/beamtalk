// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type compatibility and generics: `is_type_compatible`, assignability, metatype assignability, alias resolution and generic type-string handling.
//!
//! **DDD Context:** Semantic Analysis

//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

use std::collections::HashMap;

use crate::ast::TypeAnnotation;
use crate::semantic_analysis::alias_registry::AliasRegistry;
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::semantic_analysis::string_utils::{is_symbol_singleton_name, split_top_level};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

use super::super::shape_assignability::declared_type_has_shape;
use super::super::type_resolver;
use super::super::types::{ClassObjectType, MetatypeNonconformance};
use super::super::well_known::WellKnownClass;
use super::super::{InferredType, TypeChecker};

impl TypeChecker {
    /// Check if `actual` type is compatible with `expected` type.
    ///
    /// Compatibility rules:
    /// - Same type → compatible
    /// - `expected` appears in `actual`'s superclass chain → compatible (e.g., Integer for Number)
    /// - Either type is unknown to the hierarchy → compatible (conservative)
    ///
    /// `pub(crate)`, not `pub(super)`: `semantic_analysis::trait_expansion`'s
    /// override-compatibility check (ADR 0127 §8, BT-3589) reuses this same
    /// nominal-chain rule for a class-body method replacing a dropped trait
    /// provision — "the same rule used for overriding an inherited method" —
    /// rather than re-deriving type compatibility a second time.
    pub(crate) fn is_type_compatible(
        actual: &EcoString,
        expected: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        // Resolve `Nil` to `UndefinedObject` so hierarchy lookups succeed.
        let actual_resolved = Self::resolve_type_alias(actual);
        let expected_resolved = Self::resolve_type_alias(expected);
        let actual = actual_resolved.as_ref().unwrap_or(actual);
        let expected = expected_resolved.as_ref().unwrap_or(expected);

        if actual == expected {
            return true;
        }
        // Never is the bottom type — compatible with everything as actual.
        // As expected, only Never itself is compatible (a non-divergent method
        // returning Integer does not satisfy -> Never).
        if WellKnownClass::from_str(actual) == Some(WellKnownClass::Never) {
            return true;
        }
        if WellKnownClass::from_str(expected) == Some(WellKnownClass::Never) {
            return false;
        }
        // Union syntax in expected type (e.g., "Integer | Symbol" from builtins).
        // If expected contains `|`, split into members and check if actual matches any.
        // Split at the top level only, so a map shape's internal `|` is not a
        // member separator; a single piece falls through to the arms below.
        if expected.contains('|') {
            let members = split_top_level(expected, '|');
            if members.len() > 1 {
                return members.into_iter().any(|member| {
                    let member_eco: EcoString = member.into();
                    Self::is_type_compatible(actual, &member_eco, hierarchy)
                });
            }
        }
        // Singleton types: #foo is a subtype of Symbol, not the reverse.
        // Symbol is NOT compatible with a singleton like #ok.
        // Generic type params (K, V, T, etc.) are always compatible (conservative).
        //
        // `actual == "Dynamic"` is also always compatible here, same
        // as it already is everywhere else `is_type_compatible` reasons about
        // an unresolved argument — every other branch in this function falls
        // through to the "unknown declared/actual class -> conservatively
        // compatible" escape hatch below, since `Dynamic` is never a
        // registered hierarchy class, but this singleton branch returns
        // before ever reaching it. This matters for class-type-param
        // substitution: a method-local type param the substitution can't
        // resolve (e.g. `andThen:`'s `R` in `Block(T, Result(R, E))`, not
        // bound by the receiver's own type args) becomes literal `Dynamic`,
        // and comparing it against a substituted singleton E (e.g. `#x`)
        // must not fail just because it's unresolved.
        if is_symbol_singleton_name(expected) && !expected.contains('|') {
            return actual == expected
                || super::super::is_type_var_placeholder(actual, hierarchy)
                || WellKnownClass::from_str(actual) == Some(WellKnownClass::Dynamic);
        }
        if is_symbol_singleton_name(actual) && !actual.contains('|') {
            // Singletons are subtypes of Symbol — check if expected is Symbol
            // or an ancestor of Symbol in the class hierarchy.
            if super::super::is_type_var_placeholder(expected, hierarchy) {
                return true;
            }
            let symbol: EcoString = "Symbol".into();
            return Self::is_type_compatible(&symbol, expected, hierarchy);
        }
        // `Class` is a meta-type — any class reference is an instance of
        // Class.  Since class references are currently inferred as their concrete
        // class name (e.g. `Supervisor`), we treat `Class` as compatible with any
        // known class to avoid false positives on `Class | Nil` parameters.
        if expected.as_str() == "Class" && hierarchy.has_class(actual) {
            return true;
        }
        // Strip type arguments before the hierarchy lookup so generic
        // annotations like `Block(T, R)` or `Array(Integer)` resolve to their
        // base class. Without stripping, `hierarchy.has_class("Block(T, R)")`
        // is false and the conservative "unknown → compatible" escape hatch
        // below suppresses warnings for non-Block arguments to `Block(T, R)`
        // parameters. Both sides must be normalized so override checks
        // (`check_override_param_compatibility`), which pass annotation
        // strings as `actual`, also resolve to the base class.
        let actual_base = actual
            .split_once('(')
            .map_or(actual.as_str(), |(base, _)| base);
        let expected_base = expected
            .split_once('(')
            .map_or(expected.as_str(), |(base, _)| base);

        if actual_base == expected_base {
            // When *both* sides carry type arguments and the bases match
            // (e.g. `Array(Integer)` vs `Array(String)`), don't blindly pass on
            // the base alone — compare the type args invariantly so a union
            // member like `Array(String)` is rejected against a declared
            // `Array(Integer)`. If only one side is parameterized we stay
            // conservative (bare `Array` may be any `Array(_)`).
            return Self::type_args_match(actual, expected, hierarchy);
        }
        // If either type isn't known to the hierarchy, don't warn (conservative)
        if !hierarchy.has_class(actual_base) || !hierarchy.has_class(expected_base) {
            return true;
        }
        // Walk superclass chain: if expected is an ancestor of actual, it's compatible
        let chain = hierarchy.superclass_chain(actual_base);
        chain
            .iter()
            .any(|ancestor| ancestor.as_str() == expected_base)
    }

    /// Is a *declared* return type `actual` an acceptable refinement of a
    /// *declared* return type `expected` (BT-4175)?
    ///
    /// The one covariance rule shared by every "a method stands in for another
    /// signature" check: an instance method overriding an inherited one
    /// (`check_override_return_compatibility`) and a class satisfying a
    /// protocol requirement
    /// ([`ProtocolRegistry::conformance_return_type_mismatches`]). Return
    /// types are covariant, so `actual` must be assignable to `expected`:
    ///
    /// - a union `actual` is compatible iff *every* member is (so
    ///   `String | Result(String, Error)` does not refine `String`);
    /// - a union `expected` accepts `actual` iff *some* member does;
    /// - everything else uses [`Self::is_type_compatible`]'s nominal-chain
    ///   rule (invariant type args, `Nil`/`Never` handling, unknown class ⇒
    ///   compatible).
    ///
    /// Deliberately conservative (answers `true`, i.e. "no warning") for
    /// shapes the nominal walk cannot judge without the substitution context
    /// it does not have: anything mentioning a generic type parameter
    /// (`E`, `T`, `K`, `V` — the parent's binding is not substituted here),
    /// `Self`/`Self class`/`X class`, `Dynamic`, a difference or
    /// intersection type, or a protocol name (structural, not nominal).
    pub(crate) fn is_return_type_refinement(
        actual: &DeclaredType,
        expected: &DeclaredType,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if Self::declared_type_is_opaque(actual, hierarchy)
            || Self::declared_type_is_opaque(expected, hierarchy)
        {
            return true;
        }
        let false_ty = || DeclaredType::simple("False");
        match (actual, expected) {
            (DeclaredType::Union(members), _) => members
                .iter()
                .all(|m| Self::is_return_type_refinement(m, expected, hierarchy)),
            (DeclaredType::FalseOr(inner), _) => {
                Self::is_return_type_refinement(inner, expected, hierarchy)
                    && Self::is_return_type_refinement(&false_ty(), expected, hierarchy)
            }
            (_, DeclaredType::Union(members)) => members
                .iter()
                .any(|m| Self::is_return_type_refinement(actual, m, hierarchy)),
            (_, DeclaredType::FalseOr(inner)) => {
                Self::is_return_type_refinement(actual, inner, hierarchy)
                    || Self::is_return_type_refinement(actual, &false_ty(), hierarchy)
            }
            _ => {
                // ADR 0147: the string helper knows no `#{...}` class (and
                // would call it compatible), so a shape on either side is
                // judged structurally.
                if declared_type_has_shape(actual) || declared_type_has_shape(expected) {
                    let subst = type_resolver::SubstitutionMap::new();
                    let resolve = |t: &DeclaredType| {
                        type_resolver::resolve_declared_type_in(
                            t,
                            &subst,
                            None,
                            None,
                            super::super::TypeStringContext::Declared,
                            Some(hierarchy),
                        )
                    };
                    return Self::shape_assignable(&resolve(actual), &resolve(expected), hierarchy)
                        .is_ok();
                }
                let actual_str: EcoString = actual.to_string().into();
                let expected_str: EcoString = expected.to_string().into();
                Self::is_type_compatible(&actual_str, &expected_str, hierarchy)
            }
        }
    }

    /// `true` when `ty` (anywhere inside it) has a shape
    /// [`Self::is_return_type_refinement`] declines to judge — see that
    /// function's doc for the list.
    fn declared_type_is_opaque(ty: &DeclaredType, hierarchy: &ClassHierarchy) -> bool {
        match ty {
            DeclaredType::Simple(name) => {
                super::super::is_type_var_placeholder(name, hierarchy)
                    || WellKnownClass::from_str(name) == Some(WellKnownClass::Dynamic)
                    || hierarchy.is_protocol_class(name)
            }
            DeclaredType::Singleton(_) => false,
            DeclaredType::Generic { base, parameters } => {
                hierarchy.is_protocol_class(base)
                    || parameters
                        .iter()
                        .any(|p| Self::declared_type_is_opaque(p, hierarchy))
            }
            DeclaredType::Union(members) => members
                .iter()
                .any(|m| Self::declared_type_is_opaque(m, hierarchy)),
            DeclaredType::FalseOr(inner) => Self::declared_type_is_opaque(inner, hierarchy),
            DeclaredType::Shape { fields, .. } => fields
                .iter()
                .any(|f| Self::declared_type_is_opaque(&f.value, hierarchy)),
            DeclaredType::Difference { .. }
            | DeclaredType::Intersection { .. }
            | DeclaredType::SelfType
            | DeclaredType::SelfClass
            | DeclaredType::ClassOf(_) => true,
        }
    }

    /// `true` when two same-base, *both*-parameterized positional types
    /// disagree on arity — `Tuple(A, B)` vs `Tuple(A, B, C)` (BT-4220).
    ///
    /// For a class with a fixed type-param list an arity difference is a
    /// malformed annotation (reported elsewhere), so the arg comparisons stay
    /// conservative about it. A positional tuple's arity *is* its shape, though:
    /// a 2-tuple is never a 3-tuple, so the mismatch is a definite type error.
    /// A bare (unparameterized) side has length 0 and never mismatches — it
    /// flows either way per the gradual rules.
    fn is_positional_arity_mismatch(base: &str, actual_len: usize, expected_len: usize) -> bool {
        WellKnownClass::from_str(base) == Some(WellKnownClass::Tuple)
            && actual_len != 0
            && expected_len != 0
            && actual_len != expected_len
    }

    /// Compare the *type arguments* of two same-base generic type
    /// strings (e.g. `"Array(Integer)"` vs `"Array(String)"`).
    ///
    /// Returns `true` (compatible) unless both sides are parameterized with the
    /// same arity and at least one arg pair is incompatible. When either side is
    /// unparameterized (bare `Array`, which may stand for any `Array(_)`) or the
    /// arities differ, we stay conservative and return `true` so this never
    /// introduces a false positive on its own — except for a positional tuple,
    /// where an arity difference is a definite mismatch
    /// ([`Self::is_positional_arity_mismatch`]).
    ///
    /// Args are compared invariantly via [`is_type_compatible`], recursing into
    /// nested generics (so `Array(Array(Integer))` vs `Array(Array(String))`
    /// is also caught).
    fn type_args_match(actual: &str, expected: &str, hierarchy: &ClassHierarchy) -> bool {
        let (base, actual_args) = Self::parse_generic_type_string(actual);
        let (_, expected_args) = Self::parse_generic_type_string(expected);
        if Self::is_positional_arity_mismatch(&base, actual_args.len(), expected_args.len()) {
            return false;
        }
        if actual_args.is_empty()
            || expected_args.is_empty()
            || actual_args.len() != expected_args.len()
        {
            return true; // Unparameterized or arity mismatch — conservative.
        }
        actual_args.iter().zip(expected_args.iter()).all(|(a, e)| {
            // Generic placeholders (T, E, K, V) are symbolic — treat as
            // compatible, with the same predicate `type_args_compatible` uses.
            if super::super::is_type_var_placeholder(a, hierarchy)
                || super::super::is_type_var_placeholder(e, hierarchy)
            {
                return true;
            }
            Self::is_type_compatible(&a.as_str().into(), &e.as_str().into(), hierarchy)
        })
    }

    /// Recursive structural compatibility for one type-arg slot.
    ///
    /// Used when comparing a declared generic return type's inner args against
    /// the body's inferred inner args. Recurses into nested `Known` so that
    /// shapes like `Result(Array(Integer), Error)` vs `Result(Array(String),
    /// Error)` are detected as mismatches at the inner level.
    ///
    /// A nested `Union` on the `actual` (body-inferred) side is not an
    /// automatic pass — every member must be compatible with
    /// `expected`, recursing through this same function (mirroring the
    /// `classify_union_members`-style check used at the top level).
    /// So a declared `List(String)` against a body inferring `List(String |
    /// Nil)` warns, since `Nil` isn't assignable to `String`. Like
    /// `classify_union_members`, the whole union is skipped (permissive) if
    /// any member is itself non-`Known` (nested `Dynamic`/`Union`/`Never`/
    /// etc.) — we can't reason about those precisely enough to warn without
    /// risking a false positive.
    ///
    /// Nested `Dynamic`/`Never` on either side (outside of a `Union`) still
    /// short-circuit to `true` — that stays permissive by design:
    /// `Dynamic` is the checker's universal escape hatch and
    /// Beamtalk generics are erased at runtime, so there's no runtime-safety
    /// payoff to warning on nested `Dynamic` the way there is for a concrete
    /// incompatible `Union` member like `Nil`. Generic type-param
    /// placeholders (T, E, K, V) are treated as compatible since they're
    /// symbolic.
    pub(in super::super) fn type_args_compatible(
        expected: &InferredType,
        actual: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if let InferredType::Union { members, .. } = actual {
            // Conservative skip (classify_union_members-style): a non-Known
            // member (nested Dynamic/Union/Never/etc.) means we can't reason
            // about the union precisely, so stay permissive for the whole
            // thing rather than risk a false positive.
            if members.iter().any(|m| m.as_known().is_none()) {
                return true;
            }
            return members
                .iter()
                .all(|member| Self::type_args_compatible(expected, member, hierarchy));
        }
        let (
            InferredType::Known {
                class_name: exp_name,
                type_args: exp_inner,
                ..
            },
            InferredType::Known {
                class_name: act_name,
                type_args: act_inner,
                ..
            },
        ) = (expected, actual)
        else {
            return true; // Dynamic / Never on either side — skip
        };
        if super::super::is_type_var_placeholder(exp_name, hierarchy)
            || super::super::is_type_var_placeholder(act_name, hierarchy)
        {
            return true;
        }
        if !Self::is_type_compatible(act_name, exp_name, hierarchy) {
            return false;
        }
        // Base classes match — recurse into inner type args.
        Self::same_base_type_args_compatible(exp_name, exp_inner, act_inner, hierarchy)
    }

    /// Compare the type-arg lists of two compatible-base `Known` types
    /// (`expected_base` is the expected side's class name), slot by slot via
    /// [`Self::type_args_compatible`].
    ///
    /// Conservative (`true`) when either side is unparameterized (bare `Array`
    /// may stand for any `Array(_)`) or the arities differ — except for a
    /// positional tuple, whose arity is its shape, so `Tuple(A, B)` vs
    /// `Tuple(A, B, C)` is a mismatch ([`Self::is_positional_arity_mismatch`],
    /// BT-4220). The single home of this rule for every `Known`-vs-`Known`
    /// return/arg comparison in this file.
    pub(super) fn same_base_type_args_compatible(
        expected_base: &str,
        expected_args: &[InferredType],
        actual_args: &[InferredType],
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if Self::is_positional_arity_mismatch(expected_base, actual_args.len(), expected_args.len())
        {
            return false;
        }
        if expected_args.is_empty()
            || actual_args.is_empty()
            || expected_args.len() != actual_args.len()
        {
            return true;
        }
        expected_args
            .iter()
            .zip(actual_args.iter())
            .all(|(e, a)| Self::type_args_compatible(e, a, hierarchy))
    }

    /// Whether a class object — `C class` (`Meta{C}`) or a protocol metatype
    /// `P class` (`ProtocolMeta{P}`, ADR 0144) — may flow into a slot or
    /// parameter declared `declared_type` (BT-4199).
    ///
    /// A union declared type accepts it iff some top-level member does
    /// (BT-4206: `:: Integer | Foo class` does not accept `Integer class`);
    /// an intersection member (`P class & Q class`, ADR 0102) iff every part
    /// does. A declared `X class` part is covariant: for a class `X`,
    /// `C class` is accepted iff `C` is `X` or a subclass of it; for a
    /// protocol `X` (ADR 0144), `C class` is accepted iff
    /// [`ProtocolRegistry::check_conformance`]`(C, X)` is `Ok` (the one
    /// structural conformance rule, open-world gate included) and `C` is not
    /// itself a protocol, and `P class` iff
    /// [`ProtocolInfo::subsumes_conformance_to`](crate::semantic_analysis::protocol_registry::ProtocolInfo::subsumes_conformance_to)`(P, X)`. Any other part models the class object as
    /// `Class` for the metaclass-tower walk (ADR 0083) — `Class` /
    /// `Behaviour` / `Object` accept it, `Integer` does not; a protocol
    /// literal (`Meta{P}` for a registered protocol) is modelled as
    /// `Protocol` instead — judged by
    /// `tower`: [`Self::is_assignable_to`] for a slot (a field or a state
    /// default, which has no unknown-class escape), [`Self::is_type_compatible`]
    /// for an argument, as for every other argument type. A type-parameter
    /// part accepts it.
    pub(in super::super) fn metatype_assignable_to(
        value: ClassObjectType<'_>,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
        tower: fn(&EcoString, &EcoString, &ClassHierarchy) -> bool,
    ) -> bool {
        let members = crate::semantic_analysis::string_utils::split_top_level(declared_type, '|');
        let union = members.len() > 1;
        members.iter().any(|member| {
            crate::semantic_analysis::string_utils::split_top_level(member, '&')
                .iter()
                .all(|part| {
                    Self::metatype_assignable_to_part(
                        value,
                        part.trim(),
                        union,
                        hierarchy,
                        protocol_registry,
                        tower,
                    )
                })
        })
    }

    /// One `|`/`&`-free part of [`Self::metatype_assignable_to`]'s declared
    /// type.
    fn metatype_assignable_to_part(
        value: ClassObjectType<'_>,
        part: &str,
        union: bool,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
        tower: fn(&EcoString, &EcoString, &ClassHierarchy) -> bool,
    ) -> bool {
        if let Some(declared_base) = part.strip_suffix(" class") {
            let declared_base = declared_base.trim();
            if super::super::is_type_var_placeholder(declared_base, hierarchy) {
                return true;
            }
            if let Some(registry) =
                protocol_registry.filter(|r| Self::is_protocol_type(declared_base, hierarchy, r))
            {
                return match value {
                    // A protocol literal is the protocol object, not the
                    // class object of a conformer; `check_conformance` would
                    // wave it through the open-world gate (no hierarchy entry).
                    // Return types count too (BT-4490), as in
                    // `subsumes_conformance_to` below: one conformance rule.
                    ClassObjectType::Class(class_name) => {
                        !registry.has_protocol(class_name)
                            && registry
                                .check_conformance(class_name, declared_base, hierarchy)
                                .is_ok()
                            && registry
                                .conformance_return_type_mismatches(
                                    class_name,
                                    declared_base,
                                    hierarchy,
                                )
                                .is_empty()
                    }
                    ClassObjectType::Protocol(protocol_name) => {
                        match (registry.get(protocol_name), registry.get(declared_base)) {
                            (Some(value_protocol), Some(declared_protocol)) => value_protocol
                                .subsumes_conformance_to(declared_protocol, registry, hierarchy),
                            _ => false,
                        }
                    }
                };
            }
            return match value {
                ClassObjectType::Class(class_name) => {
                    Self::is_assignable_to(class_name, &declared_base.into(), hierarchy)
                }
                // The class object of *some* conformer is not provably a
                // subclass of any one named class.
                ClassObjectType::Protocol(_) => false,
            };
        }
        // A type-parameter part (`T | Nil`, `T class`) cannot be judged until the
        // class is instantiated: accept, as `is_type_compatible` does.
        if super::super::is_type_var_placeholder(part, hierarchy) {
            return true;
        }
        let part: EcoString = if union {
            Self::resolve_type_keyword_static(part)
        } else {
            part.into()
        };
        // A protocol literal (`Meta{P}` for a registered protocol `P`) is a
        // `Protocol` object, not a class (BT-4440): `:: Protocol` accepts it.
        let runtime_class: EcoString = match value {
            ClassObjectType::Class(name)
                if protocol_registry.is_some_and(|r| r.has_protocol(name)) =>
            {
                "Protocol".into()
            }
            _ => "Class".into(),
        };
        tower(&runtime_class, &part, hierarchy)
    }

    /// ADR 0144: when a class object `C class` fails
    /// [`Self::metatype_assignable_to`] because of a declared protocol
    /// metatype part `P class`, the protocol, the selectors `C` is missing
    /// (as [`ProtocolRegistry::check_conformance`] reports them — class-side
    /// ones prefixed `class `) and the requirements it answers with an
    /// incompatible return type (BT-4490,
    /// [`ProtocolRegistry::conformance_return_type_mismatches`]). `None`
    /// when no declared `P class` part is the cause, so the caller keeps its
    /// plain type-mismatch wording.
    pub(in super::super) fn protocol_metatype_nonconformance(
        class_name: &EcoString,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> Option<MetatypeNonconformance> {
        let registry = protocol_registry?;
        crate::semantic_analysis::string_utils::split_top_level(declared_type, '|')
            .iter()
            .flat_map(|member| crate::semantic_analysis::string_utils::split_top_level(member, '&'))
            .filter_map(|part| part.trim().strip_suffix(" class").map(str::trim))
            .filter(|base| Self::is_protocol_type(base, hierarchy, registry))
            .find_map(|base| {
                let missing = registry
                    .check_conformance(class_name, base, hierarchy)
                    .err()
                    .unwrap_or_default();
                let return_mismatches =
                    registry.conformance_return_type_mismatches(class_name, base, hierarchy);
                (!missing.is_empty() || !return_mismatches.is_empty()).then(|| {
                    MetatypeNonconformance {
                        protocol: EcoString::from(base),
                        missing,
                        return_mismatches,
                    }
                })
            })
    }

    /// The diagnostic for a class object that does not fit a declared slot,
    /// field default or parameter type: the ADR 0144 conformance wording
    /// (`<C> class does not conform to <P> class`, listing the missing
    /// selectors) when a declared `P class` part is the cause, otherwise the
    /// caller's own type-mismatch `message` and `hint`.
    pub(in super::super) fn metatype_mismatch_diagnostic(
        value: ClassObjectType<'_>,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
        message: String,
        hint: String,
        span: Span,
    ) -> Diagnostic {
        let nonconformance = match value {
            ClassObjectType::Class(class_name) => Self::protocol_metatype_nonconformance(
                class_name,
                declared_type,
                hierarchy,
                protocol_registry,
            ),
            ClassObjectType::Protocol(_) => None,
        };
        match nonconformance {
            Some(nonconformance) => Self::protocol_metatype_nonconformance_diagnostic(
                &value.to_string(),
                &nonconformance,
                span,
            ),
            None => Diagnostic::warning(message, span)
                .with_hint(hint)
                .with_category(DiagnosticCategory::Type),
        }
    }

    /// The ADR 0144 conformance warning: `<C> class does not conform to <P>
    /// class`, with the missing selectors (class-side ones prefixed `class `)
    /// and, after them, the requirements answered with an incompatible
    /// return type (BT-4490).
    pub(in super::super) fn protocol_metatype_nonconformance_diagnostic(
        value_display: &str,
        nonconformance: &MetatypeNonconformance,
        span: Span,
    ) -> Diagnostic {
        let MetatypeNonconformance {
            protocol,
            missing,
            return_mismatches,
        } = nonconformance;
        let mut hint_parts = Vec::new();
        if !missing.is_empty() {
            hint_parts.push(format!("missing: {}", missing.join(", ")));
        }
        if !return_mismatches.is_empty() {
            let mismatches: Vec<String> = return_mismatches
                .iter()
                .map(|m| {
                    let side = if m.class_side { "class " } else { "" };
                    format!(
                        "{side}{} returns {}, expected {}",
                        m.selector, m.actual, m.expected
                    )
                })
                .collect();
            hint_parts.push(format!("return type: {}", mismatches.join(", ")));
        }
        Diagnostic::warning(
            format!("{value_display} does not conform to {protocol} class"),
            span,
        )
        .with_category(DiagnosticCategory::Type)
        .with_hint(hint_parts.join("; "))
    }

    /// Returns true if `value_type` is assignable to `declared_type`.
    ///
    /// A type is assignable if it is the same type or a subclass of the declared type.
    /// For example, Integer is assignable to Number because Integer's superclass
    /// chain includes Number.
    ///
    /// For union declared types (containing `|`), the value is assignable if it's
    /// compatible with any member. For union *value* types, every member must
    /// be assignable (`A | B <: T` iff `A <: T` and `B <: T`) — the direction
    /// the local-annotation narrowing check needs. For generic types
    /// (`Result(Integer, Error)`), compares the base type name. Singleton
    /// (`#`) types require an exact match or `Symbol`.
    pub(in super::super) fn is_assignable_to(
        value_type: &EcoString,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if value_type == declared_type {
            return true;
        }
        // Union value types (BT-4042): a union is assignable to the declared
        // type iff *every* member is — the mirror of the union-declared arm
        // below, and the same all-members rule `classify_union_members`
        // applies to structured unions. Split at the top level only (shared
        // `split_top_level` scanner) so a union nested in a generic member's
        // type args (`Array(Integer | String) | Nil`) is not mistaken for a
        // member separator; a single piece means no top-level `|` and falls
        // through to the arms below. Each member is strictly shorter than the
        // whole, so the recursion terminates.
        if value_type.contains('|') {
            let members = crate::semantic_analysis::string_utils::split_top_level(value_type, '|');
            if members.len() > 1 {
                return members.iter().all(|member| {
                    let member_eco: EcoString = Self::resolve_type_keyword_static(member.trim());
                    Self::is_assignable_to(&member_eco, declared_type, hierarchy)
                });
            }
        }
        // Singleton declared types: value must be the exact same singleton.
        // Symbol is NOT assignable to a singleton like #ok (wrong subtyping direction).
        if is_symbol_singleton_name(declared_type) && !declared_type.contains('|') {
            return value_type == declared_type;
        }
        // Singleton value types: check union membership first, then fall back to
        // Symbol hierarchy (e.g., #ok is assignable to Object, Symbol | nil, etc.)
        if is_symbol_singleton_name(value_type) && !value_type.contains('|') {
            // For union declared types, check if the singleton is a member
            if declared_type.contains('|') {
                return split_top_level(declared_type, '|')
                    .into_iter()
                    .any(|member| {
                        let member_eco: EcoString = Self::resolve_type_keyword_static(member);
                        value_type.as_str() == member_eco.as_str()
                            || Self::is_type_compatible(value_type, &member_eco, hierarchy)
                    });
            }
            let symbol: EcoString = "Symbol".into();
            return Self::is_assignable_to(&symbol, declared_type, hierarchy);
        }
        // Union declared types: value must be compatible with at least one member
        if declared_type.contains('|') {
            return split_top_level(declared_type, '|')
                .into_iter()
                .any(|member| {
                    let member_eco: EcoString = Self::resolve_type_keyword_static(member);
                    Self::is_type_compatible(value_type, &member_eco, hierarchy)
                });
        }
        // For generic declared types like "Result(Integer, Error)", extract
        // the base type name and compare structurally, going through
        // the centralised `base_name_of_string` helper so the grep for
        // ad-hoc `.find('(')` slicing stays clean.
        let declared_base = type_resolver::base_name_of_string(declared_type);
        let value_base = type_resolver::base_name_of_string(value_type);
        if value_base == declared_base {
            // Same base — compare type args so `Array(String)` is not
            // silently assignable to a declared `Array(Integer)`. Conservative
            // (returns true) when either side is unparameterized.
            return Self::type_args_match(value_type, declared_type, hierarchy);
        }
        // Check if value_type's base is a subclass of declared_type's base
        let value_eco: EcoString = value_base.into();
        let declared_eco: EcoString = declared_base.into();
        hierarchy
            .superclass_chain(&value_eco)
            .iter()
            .any(|ancestor| ancestor == &declared_eco)
    }

    /// Returns true if `value_type` is assignable to `declared_type` with
    /// variance-aware generic type argument checking (ADR 0068 Phase 2f).
    ///
    /// Extends [`is_assignable_to`] with covariance support for sealed Value classes:
    /// when the declared type is generic (e.g., `Array(Printable)`) and the value type
    /// is the same generic class with different type args (e.g., `Array(Integer)`),
    /// checks each type argument covariantly — the value's type arg must be assignable
    /// to the declared type arg. This is sound because sealed Value classes are immutable.
    ///
    /// Actor classes remain invariant: type args must match exactly.
    ///
    /// Falls back to [`is_assignable_to`] when no generic type args are present or
    /// when the protocol registry is not needed.
    pub(in super::super) fn is_assignable_to_with_variance(
        value_type: &EcoString,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) -> bool {
        // Delegate to the base check first for non-generic cases
        if !declared_type.contains('(') || !value_type.contains('(') {
            return Self::is_assignable_to(value_type, declared_type, hierarchy);
        }

        // Both are generic — extract base names and type arg strings
        let (declared_base, declared_args) = Self::parse_generic_type_string(declared_type);
        let (value_base, value_args) = Self::parse_generic_type_string(value_type);

        // Different base types — check normal assignability
        if value_base != declared_base {
            return Self::is_assignable_to(value_type, declared_type, hierarchy);
        }

        // Same base, same args — trivially compatible
        if value_args == declared_args {
            return true;
        }

        if Self::is_positional_arity_mismatch(&declared_base, value_args.len(), declared_args.len())
        {
            return false;
        }

        // Same base, different args — check variance
        if hierarchy.is_covariant_class(&declared_base) {
            // Covariant: each value type arg must be compatible with declared type arg.
            // A type arg is compatible if:
            // 1. It's the same type or a subclass, OR
            // 2. The declared type arg is a protocol and the value type conforms to it
            for (val_arg, decl_arg) in value_args.iter().zip(declared_args.iter()) {
                let val_eco: EcoString = val_arg.as_str().into();
                let decl_eco: EcoString = decl_arg.as_str().into();
                if val_eco == decl_eco {
                    continue;
                }
                // Check protocol conformance first — the declared type arg might be a protocol.
                // Protocol types are NOT classes in the hierarchy, so is_type_compatible
                // would return true (conservative unknown-type fallback), masking real errors.
                if Self::is_protocol_type(decl_arg, hierarchy, protocol_registry) {
                    let base_protocol = type_resolver::base_name_of_string(decl_arg);
                    if protocol_registry
                        .check_conformance(&val_eco, base_protocol, hierarchy)
                        .is_ok()
                    {
                        continue;
                    }
                    // Value type does not conform to the protocol
                    return false;
                }
                // Check class hierarchy compatibility (subclass check)
                if Self::is_type_compatible(&val_eco, &decl_eco, hierarchy) {
                    continue;
                }
                // Type arg is not compatible
                return false;
            }
            true
        } else {
            // Invariant: type args must match exactly (already checked they differ above)
            // Fall back to base-name-only compatibility (current behaviour)
            Self::is_assignable_to(value_type, declared_type, hierarchy)
        }
    }

    /// Parse a generic type string like `"Array(Integer)"` into base name and type arg strings.
    ///
    /// Returns `("Array", ["Integer"])` for `"Array(Integer)"`.
    /// Returns `("Result", ["Integer", "Error"])` for `"Result(Integer, Error)"`.
    /// Returns `("Map", ["Result(A, B)", "C"])` for `"Map(Result(A, B), C)"` —
    /// nested multi-parameter generics split at the top level only, matching the
    /// balanced splitter used elsewhere in the type checker.
    /// For non-generic types, returns the full name and an empty vec.
    pub(in super::super) fn parse_generic_type_string(type_str: &str) -> (String, Vec<String>) {
        let (base, args_slice) = type_resolver::split_generic_base(type_str);
        let args: Vec<String> = args_slice
            .map(|s| {
                super::super::TypeChecker::split_type_params(s)
                    .into_iter()
                    .map(|p| p.trim().to_string())
                    .collect()
            })
            .unwrap_or_default();
        (base.to_string(), args)
    }

    /// Does `type_str` reference one of `params`'s keys — a class's
    /// own generic type-param names — either bare (`"V"`) or nested inside a
    /// generic (`"List(E)"`, `"Dictionary(K, V)"`, recursing through deeper
    /// nesting like `"Pair(K, List(V))"`)?
    ///
    /// Deliberately conservative: top-level union syntax (`"V | Nil"`) is
    /// left unrecognized (returns `false`) rather than guessing which member
    /// substitution should apply to — no builtin generic collection method
    /// declares a union-shaped parameter naming a class type param today, so
    /// this only means such a signature keeps the
    /// unsubstituted behavior rather than risking a wrong substitution.
    pub(in super::super) fn type_string_references_class_param(
        type_str: &str,
        params: &HashMap<EcoString, InferredType>,
    ) -> bool {
        if params.contains_key(type_str) {
            return true;
        }
        if type_str.contains(" | ") {
            return false;
        }
        let (_, args) = Self::parse_generic_type_string(type_str);
        args.iter()
            .any(|arg| Self::type_string_references_class_param(arg, params))
    }

    /// Widen a singleton (or union-of-singletons) type arg to
    /// `Symbol` before using it as a class-type-param substitution target
    /// for argument checking.
    ///
    /// A container/value class's type args, when *inferred* from literal
    /// instances rather than declared, narrow to the literal's singleton
    /// type — `#{#a => 1}` infers `Dictionary(#a, Integer)`, `#(#a, #b)`
    /// infers `List(#a | #b)`, `Result ok: #active` infers
    /// `Result(#active, Dynamic)`. Using that narrow substitution target
    /// invariantly against a *different* literal's singleton (a different
    /// dict key, list element, or Result value — all extremely common,
    /// entirely correct code) would flag a type-arg mismatch that isn't
    /// one. Widening to `Symbol` matches this checker's own existing
    /// singleton/`Symbol` subtyping rule (see [`Self::is_type_compatible`]'s
    /// singleton branch: singletons are subtypes of `Symbol`, so any other
    /// singleton — or any symbol at all — is compatible with a `Symbol`
    /// substitution target).
    ///
    /// This is deliberately scoped to argument-checking substitution only
    /// (called from [`Self::check_argument_types`] and
    /// `check_variance_in_expr`, not from inside
    /// `build_substitution_map`/`build_inherited_substitution_map`
    /// themselves), since those are also used for *return*-type inference
    /// (e.g. `class_method_substitution`), which wants the precise
    /// singleton preserved for narrowing/exhaustiveness — widening there
    /// would lose real information a caller might depend on.
    pub(in super::super) fn widen_singleton_type_arg(ty: &InferredType) -> InferredType {
        let is_singleton = |t: &InferredType| matches!(t, InferredType::Known { class_name, .. } if is_symbol_singleton_name(class_name));
        let widens = match ty {
            InferredType::Known { .. } => is_singleton(ty),
            InferredType::Union { members, .. } => {
                !members.is_empty() && members.iter().all(is_singleton)
            }
            _ => false,
        };
        if widens {
            InferredType::known("Symbol")
        } else {
            ty.clone()
        }
    }

    /// Resolves type-position keywords to their class names (static version).
    ///
    /// Same as `resolve_type_keyword` but takes `&str` for use in validation contexts.
    fn resolve_type_keyword_static(name: &str) -> EcoString {
        match name {
            // Match both `nil` and `Nil`, consistent with resolve_type_keyword.
            "nil" | "Nil" => WellKnownClass::UndefinedObject.as_str().into(),
            "false" => "False".into(),
            "true" => "True".into(),
            _ => EcoString::from(name),
        }
    }

    /// Resolves type aliases that don't exist in the class hierarchy to their
    /// canonical class names. Returns `Some` with the resolved name if an alias
    /// was applied, `None` if the name is already canonical.
    ///
    /// `Nil` in type annotations maps to `UndefinedObject` in the
    /// hierarchy. Without this resolution, `Nil` is treated as an unknown type
    /// and the conservative fallback disables validation.
    fn resolve_type_alias(name: &EcoString) -> Option<EcoString> {
        match name.as_str() {
            "Nil" => Some(WellKnownClass::UndefinedObject.as_str().into()),
            _ => None,
        }
    }

    /// ADR 0108: resolve a declared type name that refers to a
    /// registered type alias (e.g. `"RestartStrategy"`) to its structural
    /// expansion (e.g. `"#temporary | #transient | #permanent"`), so
    /// [`Self::is_assignable_to`]/[`Self::is_type_compatible`]'s nominal
    /// class-hierarchy comparison sees the alias's union members instead of
    /// an opaque, never-registered class name.
    ///
    /// `ClassHierarchy::state_field_type`/`MethodInfo::param_types` store an
    /// alias-typed declaration as the bare alias name —
    /// `TypeAnnotation::type_name()` is purely syntactic and never resolves
    /// through `AliasRegistry`. Re-resolving the *display* string at
    /// these same call sites (`declared_display`/`expected_display`) is not enough
    /// on its own — the string handed to the structural
    /// comparison functions must also be resolved, not left as the bare alias
    /// name ("display-only" would leave a gap): `is_assignable_to` has no "unknown class" escape
    /// hatch, so an unresolved alias name would produce false-positive "Type
    /// mismatch" warnings even for valid union members; `is_type_compatible`
    /// has one, so an unresolved alias name would make argument-type checking a
    /// silent no-op instead.
    ///
    /// Reconstructs a `TypeAnnotation::Simple` reference to `name` (mirroring
    /// [`AliasRegistry::resolve_display_name`]'s own approach) and routes it
    /// through the shared [`type_resolver::resolve_type_annotation_with_alias_deps`]
    /// resolver, so an alias-of-alias chain expands all the way down to a
    /// plain structural type exactly like every other alias reference in the
    /// checker. The second element of the returned pair is the (possibly
    /// empty) set of alias names touched while resolving — callers fold this
    /// into `self.referenced_aliases` (ADR 0108 hot-reload re-check trigger)
    /// the same way every other alias-aware resolution site does.
    ///
    /// A name that is not a registered alias — an ordinary class name, a
    /// singleton, or an annotation already spelled out as a union (e.g.
    /// `"Integer | Symbol"` from a builtin) — is returned unchanged with no
    /// alias deps, so non-alias assignability/compatibility behavior is
    /// untouched.
    ///
    /// Deliberately not the more general
    /// [`type_resolver::resolve_declared_type`] (also
    /// alias-registry-aware): that resolver additionally re-parses keywords
    /// (`nil`/`true`/`false`), generics, and pre-spelled unions, which would
    /// change what a *non-alias* declared type resolves to here (e.g.
    /// `"Nil"` -> `"UndefinedObject"`) — a broader behavior change than this
    /// function's scope, and it doesn't report alias deps for hot-reload
    /// tracking. This function only ever touches a name that
    /// `AliasRegistry::get` recognises, then reuses the same
    /// `resolve_type_annotation_with_alias_deps`/`referenced_aliases`
    /// convention `check_return_type`/`check_state_defaults` already use in
    /// this file for their (AST-`TypeAnnotation`-sourced) alias resolution.
    pub(super) fn resolve_alias_structural(
        name: &EcoString,
        alias_registry: Option<&AliasRegistry>,
    ) -> (EcoString, Vec<EcoString>) {
        let Some(registry) = alias_registry else {
            return (name.clone(), Vec::new());
        };
        let Some(alias_info) = registry.get(name) else {
            return (name.clone(), Vec::new());
        };
        let reference = TypeAnnotation::Simple(crate::ast::Identifier {
            name: alias_info.name.clone(),
            span: alias_info.span,
        });
        let (resolved, deps) = type_resolver::resolve_type_annotation_with_alias_deps(
            &reference,
            &type_resolver::SubstitutionMap::new(),
            None,
            Some(registry),
        );
        (resolved.display_annotation(), deps)
    }

    // ── Renderers for facts `inference/` detects ──────────────
    //
    // Each of these four takes the plain data record `inference/` built
    // (see `types.rs`'s "diagnostic facts" section) and turns it into the
    // `Diagnostic` this file already owns constructing every other
    // diagnostic with — `inference/` no longer builds a `Diagnostic`
    // directly at these four sites.

    /// Returns true if the given type name refers to a protocol (not a class).
    ///
    /// Used to determine whether a type annotation should trigger structural
    /// conformance checking (protocol) or nominal subtyping (class).
    pub(in super::super) fn is_protocol_type(
        type_name: &str,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) -> bool {
        // Extract base name for generic types
        let base_name = type_resolver::base_name_of_string(type_name);

        // A protocol type is one that's in the registry and either not a class name,
        // or only present as a synthetic protocol class entry (added by register_protocol_classes)
        protocol_registry.has_protocol(base_name)
            && (!hierarchy.has_class(base_name) || hierarchy.is_protocol_class(base_name))
    }

    /// Splits a `type_name()`-rendered intersection string (`"P1 & P2 & …"`,
    /// ADR 0068 §Protocol Composition / ADR 0102 §1/§3) into its
    /// top-level `&`-joined parts.
    ///
    /// Respects parenthesis nesting so a generic type argument's own
    /// annotation (which may itself contain `(`/`)`) is never mistaken for a
    /// top-level split point. Returns `None` when the string has no
    /// top-level `&` — i.e. it isn't an intersection annotation, and callers
    /// should fall back to single-type handling (`is_protocol_type` /
    /// `check_protocol_argument_conformance`).
    ///
    /// Used by [`check_protocol_conformance_in_expr`](super::super::protocol::TypeChecker::check_protocol_conformance_in_expr)
    /// so a parameter declared `:: P1 & P2` is checked for conformance to
    /// **both** protocol parts, per ADR 0068's protocol-composition use case.
    ///
    /// Thin wrapper over the shared nesting-aware scanner
    /// (`string_utils::split_top_level`), layering on "no top-level
    /// `&` found ⇒ `None`" — the one behavioural difference this consumer
    /// actually needs (see doc comment above): callers use `None` to detect
    /// "not an intersection annotation at all" and fall back to single-type
    /// handling, which a bare 1-element `Vec` can't distinguish from "an
    /// intersection of one part".
    pub(in super::super) fn split_intersection_type_string(type_name: &str) -> Option<Vec<&str>> {
        let parts = crate::semantic_analysis::string_utils::split_top_level(type_name, '&');
        if parts.len() < 2 { None } else { Some(parts) }
    }

    /// Check type parameter bounds for a generic type application (ADR 0068 Phase 2d).
    ///
    /// When a generic class has bounded type parameters (e.g., `Logger(T :: Printable)`),
    /// this checks that the concrete type arguments conform to those bounds.
    ///
    /// For example, `Logger(Integer)` is valid because `Integer` conforms to `Printable`,
    /// but `Logger(SomeOpaqueType)` would warn if `SomeOpaqueType` does not.
    ///
    /// Only emits warnings (not errors) per ADR 0025 gradual typing philosophy.
    #[allow(clippy::too_many_lines)] // ADR 0102 added a `Negation` skip arm
    pub(in super::super) fn check_type_param_bounds(
        &mut self,
        class_name: &EcoString,
        type_args: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        // Resolve type param names and bounds from either ClassInfo or ProtocolInfo
        let (param_names, param_bounds): (Vec<&EcoString>, Vec<&Option<EcoString>>) =
            if let Some(class_info) = hierarchy.get_class(class_name) {
                (
                    class_info.type_params.iter().collect(),
                    class_info.type_param_bounds.iter().collect(),
                )
            } else if let Some(proto_info) = protocol_registry.get(class_name) {
                (
                    proto_info.type_params.iter().collect(),
                    proto_info.type_param_bounds.iter().collect(),
                )
            } else {
                return; // Unknown type — can't check bounds
            };

        if param_bounds.is_empty() || param_bounds.iter().all(|b| b.is_none()) {
            return;
        }

        for (i, (arg, bound_opt)) in type_args.iter().zip(param_bounds.iter()).enumerate() {
            let Some(bound_protocol) = bound_opt else {
                continue; // Unbounded — any type is accepted
            };

            match arg {
                InferredType::Known {
                    class_name: arg_class,
                    ..
                } => {
                    // Check if the concrete type arg conforms to the bound protocol
                    match protocol_registry.check_conformance(arg_class, bound_protocol, hierarchy)
                    {
                        Ok(()) => {} // Conforms — no warning
                        Err(missing) => {
                            let param_name = param_names.get(i).map_or("?", |p| p.as_str());
                            let missing_list = Self::format_selector_list(&missing);
                            self.diagnostics.push(
                                Diagnostic::warning(
                                    format!(
                                        "Type argument {arg_class} for {param_name} in {class_name} does not conform to {bound_protocol}"
                                    ),
                                    span,
                                )
                                .with_category(DiagnosticCategory::Type)
                                .with_hint(format!(
                                    "{arg_class} is missing required method(s): {missing_list}"
                                )),
                            );
                        }
                    }
                }
                InferredType::Union { members, .. } => {
                    // Check all union members against the bound protocol.
                    let Some((compat, total, non_conforming)) =
                        Self::classify_union_members(members, |m| {
                            // Strip type args (`Array(Integer)` → `Array`);
                            // conformance is checked on the base class.
                            let base = type_resolver::base_name_of_string(m);
                            protocol_registry
                                .check_conformance(base, bound_protocol, hierarchy)
                                .is_ok()
                        })
                    else {
                        continue; // Contains Dynamic — skip
                    };
                    if compat == total {
                        continue; // All conform → pass
                    }
                    let param_name = param_names.get(i).map_or("?", |p| p.as_str());
                    // Render `UndefinedObject` as `Nil` in user-facing messages.
                    let union_display = arg
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let diag = if compat == 0 {
                        let missing = Self::collect_missing_protocol_methods(
                            members,
                            bound_protocol,
                            hierarchy,
                            protocol_registry,
                        );
                        Diagnostic::warning(
                            format!("Type argument {union_display} for {param_name} in {class_name} does not conform to {bound_protocol}"),
                            span,
                        )
                        .with_hint(format!("No member of {union_display} conforms — missing required method(s): {missing}"))
                    } else {
                        let list = non_conforming
                            .iter()
                            .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        Diagnostic::hint(
                            format!("Type argument {union_display} for {param_name} in {class_name}: not all members conform to {bound_protocol}"),
                            span,
                        )
                        .with_hint(format!("Non-conforming member(s): {list}"))
                    };
                    self.diagnostics
                        .push(diag.with_category(DiagnosticCategory::Type));
                }
                // ADR 0083: a metatype as a type argument is unparameterized and
                // its class-side bound conformance is out of scope here — skip.
                // Dynamic/Never values: can't verify bounds (skip silently).
                // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip
                // like `Meta`/`Dynamic`/`Never` (ADR 0102). An `Intersection`
                // (`P1 & P2`, ADR 0102) as a type argument is likewise
                // skipped — a bound is a single protocol name, and checking
                // it against one member of the intersection risks a false
                // positive when a *different* member is the one that conforms.
                InferredType::Meta { .. }
                | InferredType::ProtocolMeta { .. }
                | InferredType::Dynamic(_)
                | InferredType::Never
                | InferredType::Negation { .. }
                | InferredType::Intersection { .. }
                // A shape type argument (`Box(#{...})`) conforms to no
                // protocol bound structurally; skip like `Meta`.
                | InferredType::Shape { .. } => {}
            }
        }
    }
}
