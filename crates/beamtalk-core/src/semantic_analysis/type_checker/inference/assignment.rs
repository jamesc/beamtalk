// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type inference for assignment expressions.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Covers `Expression::Assignment` — annotation-vs-inferred-type reconciliation,
//! the generic-origin tracking that feeds it, and the two assignment targets
//! (`Identifier`, `FieldAccess`) with their distinct validation rules.

use crate::ast::{Expression, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::semantic_analysis::type_checker::binary_string_compare;
use crate::semantic_analysis::type_checker::known_remote;
use crate::semantic_analysis::type_checker::narrowing::facts::AliasFacts;
use crate::semantic_analysis::type_checker::shape_assignability::ShapeMismatch;
use crate::semantic_analysis::type_checker::type_resolver;
use crate::semantic_analysis::type_checker::types::{
    AssignmentTypeMismatch, ClassObjectType, CrossObjectFieldMutation,
};
use crate::semantic_analysis::type_checker::{EnvKey, InferredType, TypeChecker, TypeEnv};
use crate::source_analysis::Span;
use ecow::EcoString;

impl TypeChecker {
    /// Describes where an expression's type originated.
    ///
    /// Returns `Some((description, span))` when the value expression is a
    /// message send that returns a generic type parameter (e.g., `V` from
    /// `Dictionary at:ifAbsent:`). Returns `None` when origin tracking
    /// isn't useful (concrete types, Dynamic, etc.).
    pub(in crate::semantic_analysis::type_checker) fn describe_type_origin(
        value: &Expression,
        ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &TypeEnv,
    ) -> Option<(EcoString, crate::source_analysis::Span)> {
        // Only track origin for generic type params (single uppercase letter)
        let type_name = ty.as_known()?;
        if !crate::semantic_analysis::type_checker::is_generic_type_param(type_name) {
            return None;
        }

        // Extract the message send details
        if let Expression::MessageSend {
            receiver,
            selector,
            span,
            ..
        } = value
        {
            let selector_name = selector.name();
            // Try to get the receiver's type name for context
            let receiver_type = match receiver.as_ref() {
                Expression::Identifier(ident) => env
                    .get_local(&ident.name)
                    .and_then(|t| t.as_known().cloned())
                    .unwrap_or_else(|| ident.name.clone()),
                Expression::ClassReference { name, .. } => name.name.clone(),
                _ => {
                    // For other expressions, try to find a class name from hierarchy
                    EcoString::from("receiver")
                }
            };

            let desc = if hierarchy.has_class(&receiver_type) {
                EcoString::from(format!(
                    "`{receiver_type} {selector_name}` returns generic type `{type_name}`"
                ))
            } else {
                EcoString::from(format!(
                    "`{selector_name}` returns generic type `{type_name}`"
                ))
            };

            return Some((desc, *span));
        }

        None
    }

    /// Detect a declared-vs-inferred type mismatch on an assignment's type
    /// annotation — pure data, no diagnostic construction; see
    /// `validation/::emit_assignment_type_mismatch` for the rendering.
    ///
    /// Returns `None` when the RHS is `Dynamic` (the primary use case for
    /// annotations — accepted silently) or `Never` (a diverging expression
    /// like `self error:`, compatible with any declared type as the bottom
    /// of the type lattice), when the declared type is `Dynamic` (it accepts
    /// anything), or when either type is assignable to the
    /// other — including the narrowing-assignment escape hatch (declared
    /// assignable to inferred, e.g. `Dictionary := <Object>`, the
    /// type-erasure pattern the annotation exists for).
    ///
    /// Class objects — the whole RHS or members of a union RHS — never take
    /// part in that name comparison (BT-4445): each is judged by
    /// [`Self::metatype_assignable_to`] alone, since a protocol literal
    /// (`Meta{P}`) and a protocol metatype (`ProtocolMeta{P}`) share the
    /// spelling `P class`, and the name check accepts any `X class` it cannot
    /// find in the hierarchy.
    ///
    /// Also `None` when either side is a bare type-variable placeholder
    /// (`E`, `T` — see `is_type_var_placeholder`). Inside a method body a
    /// method-local or class-level type variable is an opaque placeholder
    /// with no binding (BT-4210), so it is symbolic and compatible with
    /// anything; `type_args_compatible` (validation/) treats generic type
    /// args with the same predicate.
    pub(in crate::semantic_analysis::type_checker) fn detect_assignment_type_mismatch(
        inferred_ty: &InferredType,
        declared: &InferredType,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> Option<AssignmentTypeMismatch> {
        if matches!(inferred_ty, InferredType::Dynamic(_) | InferredType::Never)
            || matches!(declared, InferredType::Dynamic(_))
        {
            return None;
        }
        let is_type_var = |ty: &InferredType| {
            matches!(
                ty,
                InferredType::Known { class_name, type_args, .. }
                    if type_args.is_empty()
                        && crate::semantic_analysis::type_checker::is_type_var_placeholder(
                            class_name, hierarchy,
                        )
            )
        };
        if is_type_var(inferred_ty) || is_type_var(declared) {
            return None;
        }
        // ADR 0083 / ADR 0144: class objects — bare, or members of a union
        // value — are judged by the one slot rule (`C class` into `P class`
        // iff `C` conforms to `P`), never by name: the protocol literal
        // `Meta{P}` and the protocol metatype `ProtocolMeta{P}` both spell
        // `P class`, and the name check waves an `X class` it cannot find in
        // the hierarchy through.
        let declared_name = declared.display_name();
        let inferred_members = Self::union_members(inferred_ty);
        let declared_members = Self::union_members(declared);
        let has_class_object =
            |members: &[InferredType]| members.iter().any(|m| ClassObjectType::of(m).is_some());
        let refused_class_object = Self::refused_class_object(
            inferred_members,
            &declared_name,
            hierarchy,
            protocol_registry,
            Self::is_assignable_to,
        );
        let assignable = if has_class_object(inferred_members) || has_class_object(declared_members)
        {
            let inferred_instances: Vec<InferredType> = inferred_members
                .iter()
                .filter(|m| ClassObjectType::of(m).is_none())
                .cloned()
                .collect();
            let inferred_rest = (!inferred_instances.is_empty())
                .then(|| InferredType::union_of(&inferred_instances).display_name());
            // Widening: every member of the value fits the declared type.
            let widening = refused_class_object.is_none()
                && Self::instances_widen(
                    &inferred_instances,
                    declared,
                    hierarchy,
                    protocol_registry,
                );
            // Narrowing escape hatch: every member of the declared type fits
            // some member of the value.
            let narrowing =
                declared_members
                    .iter()
                    .all(|member| match ClassObjectType::of(member) {
                        Some(class_object) => inferred_members.iter().any(|value| {
                            Self::class_object_narrows_into(
                                class_object,
                                value,
                                hierarchy,
                                protocol_registry,
                            )
                        }),
                        None => inferred_rest.as_ref().is_some_and(|rest| {
                            Self::is_assignable_to(&member.display_name(), rest, hierarchy)
                        }),
                    });
            widening || narrowing
        } else {
            Self::instance_assignable(inferred_ty, declared, hierarchy, protocol_registry)
        };
        if assignable {
            return None;
        }
        // ADR 0144: a class object missing a declared `P class` is reported
        // by name (`C class does not conform to P class`), even as one member
        // of a union value.
        let nonconformance = match refused_class_object {
            Some(ClassObjectType::Class(class_name)) => Self::protocol_metatype_nonconformance(
                class_name,
                &declared_name,
                hierarchy,
                protocol_registry,
            ),
            _ => None,
        };
        let inferred_display = match (&nonconformance, refused_class_object) {
            (Some(_), Some(class_object)) => {
                InferredType::class_name_for_diagnostic(&class_object.to_string())
            }
            // Use source-sympathetic spelling (`Nil`) for user-facing messages.
            _ => inferred_ty
                .display_for_diagnostic()
                .unwrap_or_else(|| inferred_ty.display_name()),
        };
        // ADR 0147: when a map shape is involved, say which key is at fault.
        let shape_detail = Self::shape_mismatch_detail(inferred_ty, declared, hierarchy);
        Some(AssignmentTypeMismatch {
            shape_detail,
            declared_display: declared
                .display_for_diagnostic()
                .unwrap_or_else(|| declared_name.clone()),
            inferred_display,
            nonconformance,
        })
    }

    /// Whether the non-class-object members of a value (`instances`) all
    /// fit `declared` — the widening half of
    /// [`Self::detect_assignment_type_mismatch`]'s class-object arm. Vacuously
    /// true when there are none; a protocol part is judged structurally
    /// (BT-4482).
    fn instances_widen(
        instances: &[InferredType],
        declared: &InferredType,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> bool {
        let value = InferredType::union_of(instances);
        // As in `instance_assignable`: where a protocol is involved, the
        // nominal rule's unknown-class leniency must not decide.
        let protocol_involved =
            Self::protocol_involved(&value, declared, hierarchy, protocol_registry);
        instances.is_empty()
            || (!protocol_involved
                && Self::is_assignable_to(
                    &value.display_name(),
                    &declared.display_name(),
                    hierarchy,
                ))
            || Self::members_conform_structurally(
                instances,
                Self::union_members(declared),
                hierarchy,
                protocol_registry,
            )
    }

    /// ADR 0147: when a map shape is involved, why the value is not
    /// assignable, naming the offending key — from the first member of the
    /// value that [`Self::shape_member_ok`] itself refused.
    fn shape_mismatch_detail(
        inferred_ty: &InferredType,
        declared: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> Option<EcoString> {
        if !(inferred_ty.contains_shape() || declared.contains_shape()) {
            return None;
        }
        Self::union_members(inferred_ty)
            .iter()
            .filter(|member| !Self::shape_member_ok(member, declared, hierarchy))
            .find_map(|member| {
                Self::shape_assignable(member, declared, hierarchy)
                    .err()
                    .filter(|m| !matches!(m, ShapeMismatch::Incompatible))
            })
            .map(|m| m.describe())
    }

    /// ADR 0147: whether one member of an assigned value fits `declared` when
    /// a shape is involved. Forward (`member` into `declared`) always; a
    /// member that is not itself a shape may also be the narrowing cast, i.e.
    /// some member of `declared` accepts it the other way (so a nullable
    /// `#{...} | Nil` annotation still casts a typed `Dictionary`).
    fn shape_member_ok(
        member: &InferredType,
        declared: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        Self::shape_assignable(member, declared, hierarchy).is_ok()
            || (!member.contains_shape()
                && Self::union_members(declared)
                    .iter()
                    .any(|d| Self::shape_assignable(d, member, hierarchy).is_ok()))
    }

    /// The instance-type arm of [`Self::detect_assignment_type_mismatch`]
    /// (no class object on either side): either type assignable to the
    /// other. BT-4482: where a protocol is involved, structural conformance
    /// ([`Self::members_conform_structurally`]) alone decides — the nominal
    /// rule's unknown-class leniency would wave a protocol name through a
    /// union (`String` into `P | Nil`).
    fn instance_assignable(
        inferred_ty: &InferredType,
        declared: &InferredType,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> bool {
        // ADR 0147: a map shape is judged structurally, never by display
        // name (a shape has no class name to compare). Each member of the
        // value is judged on its own: a member that is, or contains, a shape
        // goes forward only (so a misspelt key in one branch of a union of
        // literals is caught, and an open shape or a container of shapes is
        // not silently narrowed); any other member (a typed `Dictionary`,
        // `Object`) may also go the other way, because an annotated local is
        // the narrowing cast (ADR 0147 § `Dictionary` to shape).
        if inferred_ty.contains_shape() || declared.contains_shape() {
            return Self::union_members(inferred_ty)
                .iter()
                .all(|member| Self::shape_member_ok(member, declared, hierarchy));
        }
        let inferred_members = Self::union_members(inferred_ty);
        let declared_members = Self::union_members(declared);
        if !Self::protocol_involved(inferred_ty, declared, hierarchy, protocol_registry) {
            let inferred_name = inferred_ty.display_name();
            let declared_name = declared.display_name();
            return Self::is_assignable_to(&inferred_name, &declared_name, hierarchy)
                || Self::is_assignable_to(&declared_name, &inferred_name, hierarchy);
        }
        Self::members_conform_structurally(
            inferred_members,
            declared_members,
            hierarchy,
            protocol_registry,
        ) || Self::members_conform_structurally(
            declared_members,
            inferred_members,
            hierarchy,
            protocol_registry,
        )
    }

    /// Whether either side of an assignment names a protocol type (as a
    /// union member or intersection part) — where the nominal rule's
    /// unknown-class leniency must not decide and structural conformance
    /// ([`Self::members_conform_structurally`]) does instead. `false`
    /// without a registry.
    fn protocol_involved(
        value: &InferredType,
        declared: &InferredType,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> bool {
        protocol_registry.is_some_and(|registry| {
            Self::mentions_protocol(value, hierarchy, registry)
                || Self::mentions_protocol(declared, hierarchy, registry)
        })
    }

    /// BT-4607: whether a state slot of type `declared` accepts the value
    /// member `value` — the rule `self.f := v` and `state: f :: T = v`
    /// share. Where a protocol is involved on either side, the
    /// annotated-local rule ([`Self::instances_widen`], BT-4482) decides by
    /// structural conformance, so a conformer of a protocol-typed slot (`P`,
    /// `P | Nil`, `P1 & P2`) is accepted and a non-conformer is not.
    /// Otherwise the nominal superclass walk ([`Self::is_assignable_to`])
    /// compares `value_name` against `declared_name`, the slot's flattened,
    /// alias-expanded spelling.
    pub(in crate::semantic_analysis::type_checker) fn slot_accepts(
        value: &InferredType,
        value_name: &EcoString,
        declared: &InferredType,
        declared_name: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> bool {
        if Self::protocol_involved(value, declared, hierarchy, protocol_registry) {
            Self::instances_widen(
                std::slice::from_ref(value),
                declared,
                hierarchy,
                protocol_registry,
            )
        } else {
            Self::is_assignable_to(value_name, declared_name, hierarchy)
        }
    }

    /// BT-4482: whether every one of the `values` (union members of a value
    /// type) fits some member of `declared`, judging a protocol-typed part by
    /// structural conformance ([`ProtocolRegistry::check_conformance`], the
    /// rule protocol-typed method arguments use, open-world gate included)
    /// rather than by the superclass chain [`Self::is_assignable_to`] walks.
    /// A declared intersection (`P1 & P2`) needs every part; a value
    /// intersection fits when any of its parts does; a value typed as a
    /// protocol fits a declared protocol it subsumes. Every other part is
    /// [`Self::is_assignable_to`], so this only ever accepts more than the
    /// nominal rule where a protocol is involved. `false` without a registry.
    fn members_conform_structurally(
        values: &[InferredType],
        declared: &[InferredType],
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> bool {
        let Some(registry) = protocol_registry else {
            return false;
        };
        let union = declared.len() > 1;
        !values.is_empty()
            && values.iter().all(|value| {
                matches!(value, InferredType::Dynamic(_) | InferredType::Never)
                    || declared.iter().any(|member| {
                        Self::conforms_structurally(value, member, union, hierarchy, registry)
                    })
            })
    }

    /// Whether `ty` — a union member or intersection part of it — is a
    /// protocol type ([`Self::is_protocol_type`]).
    fn mentions_protocol(
        ty: &InferredType,
        hierarchy: &ClassHierarchy,
        registry: &ProtocolRegistry,
    ) -> bool {
        match ty {
            InferredType::Known { class_name, .. } => {
                Self::is_protocol_type(class_name, hierarchy, registry)
            }
            InferredType::Union { members, .. } | InferredType::Intersection { members, .. } => {
                members
                    .iter()
                    .any(|member| Self::mentions_protocol(member, hierarchy, registry))
            }
            _ => false,
        }
    }

    /// One value member against one declared member of
    /// [`Self::members_conform_structurally`]. `union`: the declared member
    /// is one of several, where [`Self::is_assignable_to`] judges a
    /// non-protocol member by [`Self::is_type_compatible`] (unknown classes
    /// accepted) — kept so a protocol in the union changes nothing else.
    fn conforms_structurally(
        value: &InferredType,
        declared: &InferredType,
        union: bool,
        hierarchy: &ClassHierarchy,
        registry: &ProtocolRegistry,
    ) -> bool {
        if let InferredType::Intersection { members, .. } = declared {
            return members
                .iter()
                .all(|part| Self::conforms_structurally(value, part, false, hierarchy, registry));
        }
        if let InferredType::Intersection { members, .. } = value {
            return members.iter().any(|part| {
                Self::conforms_structurally(part, declared, union, hierarchy, registry)
            });
        }
        match (value, declared) {
            (
                InferredType::Known {
                    class_name: value_class,
                    ..
                },
                InferredType::Known {
                    class_name: protocol,
                    ..
                },
            ) if Self::is_protocol_type(protocol, hierarchy, registry) => {
                let protocol = type_resolver::base_name_of_string(protocol);
                if Self::is_protocol_type(value_class, hierarchy, registry) {
                    // A protocol-typed value is *some* conformer of its own
                    // protocol: it fits a protocol that one subsumes.
                    return match (registry.get(value_class), registry.get(protocol)) {
                        (Some(value_protocol), Some(declared_protocol)) => value_protocol
                            .subsumes_conformance_to(declared_protocol, registry, hierarchy),
                        _ => false,
                    };
                }
                // A singleton (`#foo`) is a `Symbol`, not an unknown class
                // the open-world gate would wave through.
                let conformer = super::method_resolution_class(value_class);
                registry
                    .check_conformance(conformer, protocol, hierarchy)
                    .is_ok()
            }
            // A class object is judged by the slot rule, never here.
            _ if ClassObjectType::of(value).is_some()
                || ClassObjectType::of(declared).is_some() =>
            {
                false
            }
            // A protocol-typed value is *some* conformer: it fits `Object`
            // (and its ancestors), but no conformer is provably any narrower
            // class (nor a `Protocol`) — whether or not the protocol has a
            // synthetic hierarchy entry (same-file) or not (cross-file).
            (InferredType::Known { class_name, .. }, _)
                if Self::is_protocol_type(class_name, hierarchy, registry) =>
            {
                Self::is_assignable_to(&"Object".into(), &declared.display_name(), hierarchy)
            }
            _ => {
                let (value, declared) = (value.display_name(), declared.display_name());
                Self::is_assignable_to(&value, &declared, hierarchy)
                    || (union && Self::is_type_compatible(&value, &declared, hierarchy))
            }
        }
    }

    /// The top-level members of a union type, or the type itself.
    fn union_members(ty: &InferredType) -> &[InferredType] {
        match ty {
            InferredType::Union { members, .. } => members,
            other => std::slice::from_ref(other),
        }
    }

    /// Whether the declared class object `declared` narrows the value member
    /// `value` — the narrowing-assignment escape hatch of
    /// [`Self::detect_assignment_type_mismatch`] (`Tcp class | Nil` into
    /// `Tcp class`), judged by the slot rule. Only a class-object member is
    /// narrowed: a `Class` or `Object` value is not provably the class object
    /// of a conformer. A protocol literal (`Meta{P}`, the protocol object) is
    /// narrowed by no class object: its `P class` spelling names the protocol
    /// metatype.
    fn class_object_narrows_into(
        declared: ClassObjectType<'_>,
        value: &InferredType,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> bool {
        match ClassObjectType::of(value) {
            Some(ClassObjectType::Class(name))
                if protocol_registry.is_some_and(|r| r.has_protocol(name)) =>
            {
                false
            }
            Some(_) => Self::metatype_assignable_to(
                declared,
                &value.display_name(),
                hierarchy,
                protocol_registry,
                Self::is_assignable_to,
            ),
            None => false,
        }
    }

    /// BT-4714: the alias facts for `local := value`; `None` when a block may
    /// rewrite a tested variable behind our back.
    fn alias_condition_facts(
        &mut self,
        value: &Expression,
        hierarchy: &ClassHierarchy,
        env: &TypeEnv,
    ) -> Option<AliasFacts> {
        let facts = self.alias_facts(value, env, hierarchy);
        let blocked = facts
            .keys()
            .any(|k| matches!(k, EnvKey::Local(n) if self.block_written_locals.contains(n)));
        (!blocked).then_some(facts)
    }

    /// Infer an assignment `target := value` (optionally `target :: Type :=
    /// value`) — the `Expression::Assignment` arm of `infer_expr`'s dispatch.
    ///
    /// When a type annotation is present, the declared type wins over the
    /// inferred RHS type (with a mismatch warning when the two are
    /// unrelated). The target then determines how the resulting type is
    /// recorded: an `Identifier` binds it (with origin tracking) in
    /// `env`; a `self.field` `FieldAccess` validates it against the
    /// declared state type and invalidates any stale narrowing; any other
    /// `FieldAccess` (mutating another object's state) is rejected.
    #[allow(clippy::too_many_arguments)] // split from infer_expr's dispatch, mirrors its arity
    pub(in crate::semantic_analysis::type_checker) fn infer_assignment(
        &mut self,
        target: &Expression,
        value: &Expression,
        type_annotation: Option<&TypeAnnotation>,
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        // Resolve the annotation first so a Block annotation can push its
        // parameter types into a block-literal RHS (BT-4102 Decision C).
        let declared_ann = type_annotation.map(|ann| {
            // ADR 0102 §1/§3: thread the protocol registry
            // through so a local `x :: P1 & P2` annotation resolves
            // class ∩ protocol correctly rather than falling to `Never`.
            // ADR 0108: thread the alias registry through so
            // a local `heading :: Direction := ...` annotation
            // expands to its declared union, feeding the exact same
            // `InferredType` a spelled-out union would into
            // downstream narrowing/exhaustiveness.
            //
            // ADR 0108 hot-reload re-check trigger: this is
            // exactly the `heading :: Direction := ...` site the ADR's
            // REPL example builds on, so recording its alias deps here
            // is load-bearing, not incidental.
            // Constructed inline rather than via `self.resolution_context()`:
            // this call site also needs `self.protocol_registry` at the same
            // time, and a disjoint field borrow (this) composes with that
            // sibling borrow in one expression where a method call would not
            // — see `TypeChecker::resolution_context`'s doc.
            type_resolver::ResolutionContext::new(
                self.alias_registry.as_deref(),
                &mut self.referenced_aliases,
            )
            .resolve_type_annotation(
                ann,
                &type_resolver::SubstitutionMap::new(),
                self.protocol_registry.as_deref(),
            )
        });
        if let Some(declared) = &declared_ann {
            self.stash_block_pushdown(value, declared);
        }
        let inferred_ty = self.infer_expr(value, hierarchy, env, in_abstract_method);

        // If there's a type annotation, use the declared type instead
        // of the inferred type. Emit a warning if the RHS has a known
        // (non-Dynamic) type that is incompatible with the annotation.
        let ty = if let Some(declared) = declared_ann {
            // Detection (is this RHS type actually incompatible
            // with the declared annotation?) stays here; rendering the
            // resulting fact as a diagnostic is `validation/`'s job.
            if let Some(mismatch) = Self::detect_assignment_type_mismatch(
                &inferred_ty,
                &declared,
                hierarchy,
                self.protocol_registry.as_deref(),
            ) {
                self.emit_assignment_type_mismatch(&mismatch, span);
            }
            declared
        } else {
            inferred_ty
        };

        // BT-4257: classify before the write below clears the target's own
        // raw-Binary fact (`x := x concat: y` reads it).
        let value_is_raw_binary =
            binary_string_compare::is_raw_binary(value, &|name| env.is_raw_binary_local(name));
        // A local some block writes may change behind the flow fact's back.
        let target_written_by_block = matches!(target, Expression::Identifier(ident)
            if self.block_written_locals.contains(&ident.name));

        let alias_facts = if matches!(target, Expression::Identifier(_)) && !target_written_by_block
        {
            self.alias_condition_facts(value, hierarchy, env)
        } else {
            None
        };

        match target {
            Expression::Identifier(ident) => {
                // Track type origin for generic type params
                if let Some(origin) = Self::describe_type_origin(value, &ty, hierarchy, env) {
                    env.set_with_origin(
                        EnvKey::local(ident.name.clone()),
                        ty.clone(),
                        origin.0,
                        origin.1,
                    );
                } else {
                    env.set_local(ident.name.clone(), ty.clone());
                }
                // ADR 0126 §6: known-remote flow fact. Re-derived on every
                // assignment (including a re-binding of an already-remote
                // name to a non-remote value, which clears it) — flow
                // fact, not sticky metadata. `value`, not `target`: the
                // classification is purely a function of the RHS shape.
                let key = EnvKey::local(ident.name.clone());
                match known_remote::classify(value, &|id| {
                    env.known_remote(&EnvKey::local(id.name.clone()))
                }) {
                    Some(remote) => env.mark_known_remote(key.clone(), remote),
                    None => env.clear_known_remote(&key),
                }
                // BT-4714: remember what a Boolean condition proves (computed
                // before the write, against the pre-assignment env).
                if let Some(facts) = alias_facts {
                    if !facts.mentions(&key) {
                        env.set_cond_alias(key.clone(), facts);
                    }
                }
                // BT-4257: the write above cleared any raw-Binary fact.
                if value_is_raw_binary && !target_written_by_block {
                    env.mark_raw_binary(key);
                }
            }
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                let is_self_receiver = matches!(
                    receiver.as_ref(),
                    Expression::Identifier(recv_id) if recv_id.name == "self"
                );
                if is_self_receiver {
                    // `self.field := value` — validate against declared state type
                    self.check_field_assignment(field, &ty, span, hierarchy, env);
                    // Invalidate any stale narrowing on
                    // `self.<field>`. After a write, the narrowed type is
                    // no longer guaranteed.
                    env.remove(&EnvKey::self_field(field.name.clone()));
                } else {
                    // `other.field := value` or `(expr).field := value` —
                    // objects cannot mutate another object's state. The
                    // fact (which receiver/field) is detected here; the
                    // message (including the `withField:` suggestion text)
                    // is rendered by `validation/`.
                    let recv_name = match receiver.as_ref() {
                        Expression::Identifier(recv_id) => recv_id.name.clone(),
                        _ => EcoString::from("receiver"),
                    };
                    let mutation = CrossObjectFieldMutation {
                        receiver_name: recv_name,
                        field_name: field.name.clone(),
                    };
                    self.emit_cross_object_field_mutation(&mutation, span);
                }
            }
            _ => {}
        }
        ty
    }
}
