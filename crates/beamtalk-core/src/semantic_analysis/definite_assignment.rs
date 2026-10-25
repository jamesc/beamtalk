// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The "requires definite assignment" predicate (ADR 0124 §6, §7, Implementation A1).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A slot *requires definite assignment* when it is annotated, has no
//! default, its declared type does not admit `nil`, and it is not `late`
//! (ADR 0124 §1 — `late` is exactly the declaration that opts a slot out of
//! this requirement). [`requires_definite_assignment`] is the single,
//! alias-aware, `beamtalk-core`-owned implementation of that predicate —
//! previously duplicated (and wrong) in `beamtalk-codegen`
//! (`gen_server/callbacks.rs`'s `is_nilable_type` / `is_nilable_type_name`,
//! which matched only `TypeAnnotation::Simple("Nil")` inside a `Union` and
//! string-split on `" | "`, missing `UndefinedObject`, ADR 0108 aliases, and
//! ADR 0102 intersection/negation types).
//!
//! **Nilability** is decided by resolving the declared type all the way down
//! to an [`InferredType`] — through [`resolve_declared_type`], which expands
//! ADR 0108 aliases eagerly via the supplied [`AliasRegistry`] — and then
//! asking whether that resolved type admits [`WellKnownClass::is_nil_class`]
//! (`UndefinedObject`, or its legacy `Nil` spelling; both normalise to
//! `UndefinedObject` during resolution). [`is_nilable_inferred`] applies the
//! ADR 0102 set-operator rules:
//! - **Union** — nilable if *any* member is nilable (the ordinary "one of
//!   these could be nil" reading).
//! - **Intersection** (`A & B`) — nilable only if *every* member is nilable:
//!   a value in the intersection must be a member of each operand, so it can
//!   only be `nil` if `nil` itself is a value of every operand.
//! - **Negation** (`base \\ excluded`, ADR 0102 §1) — nilable if `base` is
//!   nilable *and* `excluded` is not: subtracting a nil-admitting type
//!   (`T \\ Nil`) removes `nil` from the result, so a negation whose
//!   `excluded` operand admits `nil` is never nilable regardless of `base`.
//! - Every other resolved shape (`Known` naming a non-nil class, `Meta`,
//!   `Dynamic`, `Never`) is non-nilable — conservative by construction, since
//!   an unresolvable/dynamic type has never been treated as an implicit
//!   escape hatch from the definite-assignment check.
//!
//! [`admits_nil_type_annotation`] (ADR 0124 B1, BT-4061) answers the same
//! question a second way — subtype-style rather than explicit-member-style:
//! `nil` (a value of `UndefinedObject`) is assignable to the type, so a
//! `late` slot declaring it already has a value for the unassigned state.
//! It resolves through the same [`resolve_declared_type`] + ADR 0102
//! set-operator machinery ([`inferred_admits_nil`], shared with
//! [`is_nilable_inferred`]), but answers the leaves differently: a `Known`
//! class admits when it is the nil class, a superclass of it (`Object`,
//! `ProtoObject` — the nominal chain `is_assignable_to` walks to accept
//! `nil` into an `:: Object` slot), or a protocol `nil` provably satisfies;
//! `Dynamic` admits (it accepts every value unchecked); `Meta`/`Never`
//! admit nothing. The two readings differ deliberately: Part A's narrow
//! reading keeps the Warning (and the runtime post-`initialize` backstop)
//! firing on `:: Object`/`:: Dynamic` slots, while B1's wide reading rejects
//! `late` on exactly the types `nil` can flow into. Unknown class names
//! admit nothing — an Error must not fire on what the checker cannot see.
//!
//! `&`/`\` operands resolve without a hierarchy (this module passes none,
//! as Part A always has), so a nominal `Base \ Nil` still reads as its base
//! and a nominal `A & B` collapses to `Never`: the same erasure Part A
//! applies. `&`/`\` in a slot annotation is exotic, and matching the
//! checker's own resolution everywhere beats a second, B1-only reading.
//!
//! This module has no dependency on `beamtalk-codegen` — it lives below both
//! `beamtalk-codegen` and `beamtalk-lsp`/`beamtalk-lint` (the shared-leaf-
//! module pattern, `docs/development/architecture-principles.md` §6), so
//! every surface reaches the same predicate `beamtalk-core` already owns
//! every input for (`AliasRegistry`, `WellKnownClass`).

use crate::ast::{SlotKind, StateDeclaration, TypeAnnotation};
use crate::semantic_analysis::alias_registry::AliasRegistry;
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{
    InferredType, TypeStringContext, resolve_declared_type,
};
use std::collections::HashMap;

/// `true` if `decl` must be definitely assigned before `initialize` returns
/// (ADR 0124 §6): it carries a type annotation, has no default value, its
/// type does not admit `nil`, and it is not declared `late`.
///
/// `aliases` resolves any ADR 0108 alias reference in the declared type
/// (e.g. `state: v :: JsonValue` where `type JsonValue = Nil | …`) before
/// deciding nilability — see the module doc for the full resolution and
/// set-operator rules.
#[must_use]
pub fn requires_definite_assignment(decl: &StateDeclaration, aliases: &AliasRegistry) -> bool {
    if decl.slot_kind == SlotKind::Late {
        return false;
    }
    if decl.default_value.is_some() {
        return false;
    }
    match decl.type_annotation.as_ref() {
        None => false,
        Some(annotation) => !is_nilable_type_annotation(annotation, aliases),
    }
}

/// [`requires_definite_assignment`] for a caller that only has
/// `ClassHierarchy`/`ClassInfo`-level data — a resolved
/// [`crate::semantic_analysis::class_hierarchy::DeclaredType`], an explicit
/// has-default flag, and a [`SlotKind`] — rather than an AST
/// [`StateDeclaration`]. This is the shape `ClassHierarchy::all_state`
/// walks across cross-file ancestors expose (`state_field_type`,
/// `state_field_has_default`, `state_field_kind`), which have no AST to hand
/// for a superclass compiled in another file.
///
/// Used by ADR 0124 §6/§7's Implementation A3/A4 (the Value
/// construction-site definite-assignment check, `TypeChecker::check_value_construction_definite_assignment`)
/// so the construction-site check and the declaration-site
/// [`requires_definite_assignment`] share one predicate rather than
/// re-deriving the same "no default, not late, not nilable" rule twice.
#[must_use]
pub fn requires_definite_assignment_for_declared_type(
    ty: &DeclaredType,
    has_default: bool,
    slot_kind: SlotKind,
    aliases: &AliasRegistry,
) -> bool {
    if slot_kind == SlotKind::Late || has_default {
        return false;
    }
    !is_nilable_declared_type(ty, aliases)
}

/// `true` if `annotation` admits `nil` once ADR 0108 aliases and ADR 0102
/// intersection/negation operators are resolved — see the module doc.
#[must_use]
pub fn is_nilable_type_annotation(annotation: &TypeAnnotation, aliases: &AliasRegistry) -> bool {
    is_nilable_declared_type(&DeclaredType::from(annotation), aliases)
}

/// [`is_nilable_type_annotation`] for a flattened type-name string (e.g.
/// `ClassInfo::state_types`'s cross-file ancestor metadata, which has no
/// structured `TypeAnnotation` to hand — only the string
/// `TypeAnnotation::type_name` rendered at the declaring file's compile
/// time). Parses `name` via [`DeclaredType::parse`] before resolving, so it
/// shares the exact same alias/set-operator handling as the AST-sourced
/// path; only bare union/generic/singleton/self shapes parse back
/// structurally (`DeclaredType::parse`'s documented grammar — no `&`/`\`
/// support), so a stored `&`/`\` string degrades to an opaque non-nilable
/// class name, matching that parser's existing "unparsed string becomes a
/// nominal class name" fallback.
#[must_use]
pub fn is_nilable_type_name(name: &str, aliases: &AliasRegistry) -> bool {
    is_nilable_declared_type(&DeclaredType::parse(name), aliases)
}

/// Shared resolution step for [`is_nilable_type_annotation`],
/// [`is_nilable_type_name`] and [`admits_nil_type_annotation`]: resolve `dt`
/// to an [`InferredType`], expanding aliases through `aliases` (and, when
/// given, keeping class ∩ protocol intersections irreducible instead of
/// collapsing them to `Never`).
fn resolve_for_nil_admission(
    dt: &DeclaredType,
    aliases: &AliasRegistry,
    protocols: Option<&ProtocolRegistry>,
) -> InferredType {
    resolve_declared_type(
        dt,
        &HashMap::new(),
        protocols,
        Some(aliases),
        TypeStringContext::Declared,
    )
}

/// Shared resolution step for both [`is_nilable_type_annotation`] and
/// [`is_nilable_type_name`]: resolve `dt` to an [`InferredType`] (expanding
/// aliases through `aliases`) and check the result for nil-admission.
fn is_nilable_declared_type(dt: &DeclaredType, aliases: &AliasRegistry) -> bool {
    let resolved = resolve_for_nil_admission(dt, aliases, None);
    is_nilable_inferred(&resolved)
}

/// `true` if `annotation` admits `nil` subtype-style (ADR 0124 §1, B1,
/// BT-4061): `nil` is assignable to it, so a `late` slot declaring it
/// already has a value for the unassigned state. See the module doc for how
/// this wide reading differs from [`is_nilable_type_annotation`]'s narrow
/// one, and why.
#[must_use]
pub fn admits_nil_type_annotation(
    annotation: &TypeAnnotation,
    aliases: &AliasRegistry,
    hierarchy: &ClassHierarchy,
    protocols: &ProtocolRegistry,
) -> bool {
    let resolved =
        resolve_for_nil_admission(&DeclaredType::from(annotation), aliases, Some(protocols));
    inferred_admits_nil(
        &resolved,
        &|class_name| admits_nil_known_class(class_name, hierarchy, protocols),
        true,
    )
}

/// The nominal leaf of [`admits_nil_type_annotation`]: `true` when `nil` is
/// a value of the named class-or-protocol — the nil class itself, a nominal
/// superclass of it, or a protocol it provably satisfies. Anything else
/// (ordinary classes, unknown names) admits nothing.
fn admits_nil_known_class(
    class_name: &str,
    hierarchy: &ClassHierarchy,
    protocols: &ProtocolRegistry,
) -> bool {
    if WellKnownClass::from_str(class_name).is_some_and(WellKnownClass::is_nil_class) {
        return true;
    }
    // `nil`'s nominal supertypes (`Object`, `ProtoObject`): the same chain
    // `is_assignable_to`/`is_type_compatible` walk to accept `nil` into an
    // `:: Object` slot.
    if hierarchy
        .superclass_chain(WellKnownClass::UndefinedObject.as_str())
        .iter()
        .any(|ancestor| ancestor == class_name)
    {
        return true;
    }
    // A protocol `nil` provably satisfies admits `nil` as a value. A plain
    // unknown name is neither a protocol nor satisfied, so this answers
    // `false` for it too — see `admits_nil_type_annotation`'s doc.
    protocols.conforms_by_known_surface(
        WellKnownClass::UndefinedObject.as_str(),
        class_name,
        hierarchy,
    )
}

/// `true` if a resolved [`InferredType`] admits `nil` — see the module doc
/// for the per-shape rules (Union/Intersection/Negation).
fn is_nilable_inferred(ty: &InferredType) -> bool {
    inferred_admits_nil(
        ty,
        &|class_name| {
            WellKnownClass::from_str(class_name).is_some_and(WellKnownClass::is_nil_class)
        },
        false,
    )
}

/// Shared ADR 0102 set-operator recursion for "does this resolved type admit
/// `nil`" — Union(any)/Intersection(all)/Negation(base ∧ ¬excluded) —
/// parameterized by the two leaf answers that differ per query: how a nominal
/// class name answers, and how `Dynamic` answers. `Meta`/`Never` never admit
/// under either query.
fn inferred_admits_nil(
    ty: &InferredType,
    known_admits_nil: &impl Fn(&str) -> bool,
    dynamic_admits_nil: bool,
) -> bool {
    match ty {
        InferredType::Known { class_name, .. } => known_admits_nil(class_name),
        InferredType::Union { members, .. } => members
            .iter()
            .any(|m| inferred_admits_nil(m, known_admits_nil, dynamic_admits_nil)),
        InferredType::Intersection { members, .. } => members
            .iter()
            .all(|m| inferred_admits_nil(m, known_admits_nil, dynamic_admits_nil)),
        InferredType::Negation { base, excluded, .. } => {
            inferred_admits_nil(base, known_admits_nil, dynamic_admits_nil)
                && !inferred_admits_nil(excluded, known_admits_nil, dynamic_admits_nil)
        }
        InferredType::Dynamic(_) => dynamic_admits_nil,
        InferredType::Meta { .. } | InferredType::Never => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{CommentAttachment, DeclaredKeyword, Identifier};
    use crate::semantic_analysis::alias_registry::AliasInfo;
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn decl(
        type_annotation: Option<TypeAnnotation>,
        default_value: Option<crate::ast::Expression>,
        slot_kind: SlotKind,
    ) -> StateDeclaration {
        StateDeclaration {
            name: Identifier::new("x", span()),
            type_annotation,
            default_value,
            declared_keyword: DeclaredKeyword::default(),
            slot_kind,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }
    }

    fn simple(name: &str) -> TypeAnnotation {
        TypeAnnotation::simple(name, span())
    }

    fn union(names: &[&str]) -> TypeAnnotation {
        TypeAnnotation::union(names.iter().map(|n| simple(n)).collect(), span())
    }

    fn registry_with_alias(name: &str, annotation: TypeAnnotation) -> AliasRegistry {
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

    // ── requires_definite_assignment ──────────────────────────────────────

    #[test]
    fn plain_non_nilable_requires_assignment() {
        let registry = AliasRegistry::new();
        assert!(requires_definite_assignment(
            &decl(Some(simple("Integer")), None, SlotKind::Eager),
            &registry
        ));
    }

    #[test]
    fn untyped_does_not_require_assignment() {
        let registry = AliasRegistry::new();
        assert!(!requires_definite_assignment(
            &decl(None, None, SlotKind::Eager),
            &registry
        ));
    }

    #[test]
    fn defaulted_does_not_require_assignment() {
        let registry = AliasRegistry::new();
        let default = crate::ast::Expression::Literal(crate::ast::Literal::Integer(0), span());
        assert!(!requires_definite_assignment(
            &decl(Some(simple("Integer")), Some(default), SlotKind::Eager),
            &registry
        ));
    }

    #[test]
    fn late_does_not_require_assignment() {
        let registry = AliasRegistry::new();
        assert!(!requires_definite_assignment(
            &decl(Some(simple("Integer")), None, SlotKind::Late),
            &registry
        ));
    }

    // ── is_nilable_type_annotation ────────────────────────────────────────

    #[test]
    fn plain_class_is_not_nilable() {
        let registry = AliasRegistry::new();
        assert!(!is_nilable_type_annotation(&simple("Integer"), &registry));
    }

    #[test]
    fn union_with_nil_is_nilable() {
        let registry = AliasRegistry::new();
        assert!(is_nilable_type_annotation(
            &union(&["Integer", "Nil"]),
            &registry
        ));
    }

    #[test]
    fn union_with_undefined_object_is_nilable() {
        let registry = AliasRegistry::new();
        assert!(is_nilable_type_annotation(
            &union(&["Integer", "UndefinedObject"]),
            &registry
        ));
    }

    #[test]
    fn union_without_nil_is_not_nilable() {
        let registry = AliasRegistry::new();
        assert!(!is_nilable_type_annotation(
            &union(&["Integer", "String"]),
            &registry
        ));
    }

    #[test]
    fn alias_to_nilable_union_is_nilable() {
        let registry = registry_with_alias("JsonValue", union(&["Nil", "Boolean", "Integer"]));
        assert!(is_nilable_type_annotation(&simple("JsonValue"), &registry));
    }

    #[test]
    fn alias_to_non_nilable_type_is_not_nilable() {
        let registry = registry_with_alias("Positive", simple("Integer"));
        assert!(!is_nilable_type_annotation(&simple("Positive"), &registry));
    }

    #[test]
    fn intersection_nilable_only_if_every_operand_admits_nil() {
        let registry = AliasRegistry::new();
        let both_nilable = TypeAnnotation::Intersection {
            left: Box::new(union(&["Nil", "String"])),
            right: Box::new(union(&["Nil", "Integer"])),
            span: span(),
        };
        assert!(
            is_nilable_type_annotation(&both_nilable, &registry),
            "an intersection is nilable only when every operand admits nil"
        );

        let one_non_nilable = TypeAnnotation::Intersection {
            left: Box::new(union(&["Nil", "String"])),
            right: Box::new(simple("Integer")),
            span: span(),
        };
        assert!(
            !is_nilable_type_annotation(&one_non_nilable, &registry),
            "an intersection with one non-nilable operand is not nilable"
        );
    }

    #[test]
    fn negation_of_nil_is_not_nilable() {
        let registry = AliasRegistry::new();
        let negation = TypeAnnotation::Difference {
            base: Box::new(union(&["Nil", "String"])),
            excluded: Box::new(simple("Nil")),
            span: span(),
        };
        assert!(
            !is_nilable_type_annotation(&negation, &registry),
            "subtracting Nil from a nilable base is never nilable"
        );
    }

    #[test]
    fn negation_of_non_nil_stays_nilable_when_base_is() {
        let registry = AliasRegistry::new();
        let negation = TypeAnnotation::Difference {
            base: Box::new(union(&["Nil", "String"])),
            excluded: Box::new(simple("String")),
            span: span(),
        };
        assert!(
            is_nilable_type_annotation(&negation, &registry),
            "excluding a non-nilable member leaves a nilable base nilable"
        );
    }

    #[test]
    fn late_slot_kind_excluded_by_requires_definite_assignment_even_when_non_nilable() {
        let registry = AliasRegistry::new();
        let d = decl(Some(union(&["Integer", "String"])), None, SlotKind::Late);
        assert!(!is_nilable_type_annotation(
            d.type_annotation.as_ref().unwrap(),
            &registry
        ));
        assert!(!requires_definite_assignment(&d, &registry));
    }

    // ── requires_definite_assignment_for_declared_type ────────────────────

    #[test]
    fn declared_type_plain_non_nilable_no_default_eager_requires_assignment() {
        let registry = AliasRegistry::new();
        assert!(requires_definite_assignment_for_declared_type(
            &DeclaredType::simple("Integer"),
            false,
            SlotKind::Eager,
            &registry
        ));
    }

    #[test]
    fn declared_type_with_default_does_not_require_assignment() {
        let registry = AliasRegistry::new();
        assert!(!requires_definite_assignment_for_declared_type(
            &DeclaredType::simple("Integer"),
            true,
            SlotKind::Eager,
            &registry
        ));
    }

    #[test]
    fn declared_type_late_does_not_require_assignment_even_without_default() {
        let registry = AliasRegistry::new();
        assert!(!requires_definite_assignment_for_declared_type(
            &DeclaredType::simple("Integer"),
            false,
            SlotKind::Late,
            &registry
        ));
    }

    #[test]
    fn declared_type_nilable_union_does_not_require_assignment() {
        let registry = AliasRegistry::new();
        let ty = DeclaredType::union(vec![
            DeclaredType::simple("Integer"),
            DeclaredType::simple("Nil"),
        ]);
        assert!(!requires_definite_assignment_for_declared_type(
            &ty,
            false,
            SlotKind::Eager,
            &registry
        ));
    }

    #[test]
    fn declared_type_alias_to_nilable_does_not_require_assignment() {
        let registry = registry_with_alias("JsonValue", union(&["Nil", "Boolean", "Integer"]));
        assert!(!requires_definite_assignment_for_declared_type(
            &DeclaredType::simple("JsonValue"),
            false,
            SlotKind::Eager,
            &registry
        ));
    }

    // ── admits_nil_type_annotation (BT-4061: wide, subtype-style) ────────────

    #[test]
    fn admits_nil_object_dynamic_proto_object_but_part_a_does_not() {
        // The deliberate asymmetry between the two readings: B1 rejects
        // `late` on exactly the types `nil` can flow into, while Part A
        // keeps its narrow reading so the Warning (and the runtime
        // backstop) still fire on `:: Object`/`:: Dynamic` slots.
        let aliases = AliasRegistry::new();
        let hierarchy = ClassHierarchy::with_builtins();
        let protocols = ProtocolRegistry::new();
        for name in ["Object", "ProtoObject", "Dynamic"] {
            let annotation = simple(name);
            assert!(
                admits_nil_type_annotation(&annotation, &aliases, &hierarchy, &protocols),
                "{name} admits nil subtype-style (BT-4061)"
            );
            assert!(
                !is_nilable_type_annotation(&annotation, &aliases),
                "Part A keeps its narrow explicit-Nil reading for {name}"
            );
        }
    }

    #[test]
    fn admits_nil_plain_and_unknown_classes_admit_nothing() {
        let aliases = AliasRegistry::new();
        let hierarchy = ClassHierarchy::with_builtins();
        let protocols = ProtocolRegistry::new();
        for name in ["Integer", "Subprocess", "SomeUnknownClass"] {
            assert!(
                !admits_nil_type_annotation(&simple(name), &aliases, &hierarchy, &protocols),
                "{name} admits nothing"
            );
        }
    }

    #[test]
    fn admits_nil_unions_and_aliases_follow_members() {
        let hierarchy = ClassHierarchy::with_builtins();
        let protocols = ProtocolRegistry::new();
        let plain = AliasRegistry::new();
        assert!(admits_nil_type_annotation(
            &union(&["Integer", "Nil"]),
            &plain,
            &hierarchy,
            &protocols
        ));
        assert!(!admits_nil_type_annotation(
            &union(&["Integer", "String"]),
            &plain,
            &hierarchy,
            &protocols
        ));
        let aliased = registry_with_alias("JsonValue", union(&["Nil", "Boolean", "Integer"]));
        assert!(admits_nil_type_annotation(
            &simple("JsonValue"),
            &aliased,
            &hierarchy,
            &protocols
        ));
    }
}
