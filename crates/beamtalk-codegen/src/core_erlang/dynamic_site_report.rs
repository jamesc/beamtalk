// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The dynamic-site report (BT-4292): with `BEAMTALK_CODEGEN_DIAGNOSTICS=1`,
//! every send that compiles to `beamtalk_message_dispatch:send/3` gets a hint
//! naming the receiver's static type and the reason it stayed dynamic.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! The reason is not re-derived here: it is the [`StaysDynamic`] gate that
//! [`CoreErlangGenerator::typed_sealed_send_target`] — the same decision the
//! `typed_sealed` handler emits from — rejected the send at. This module
//! only describes it, adding detail the gate does not need (the routing row
//! of a built-in, whether an open class is abstract, an actor or in the
//! supervisor family). Epic BT-4291 measures its baseline and progress with
//! it (`benchmarks/vs-elixir/results/bench-bt4292-baseline.md`).
//!
//! Each hint reads
//!
//! ```text
//! Send 'at:put:' at line 12: dynamic dispatch (receiver: Dictionary [Object, declared]; reason: builtin — representation-routed: untagged_map guard -> bt@stdlib@dictionary)
//! ```
//!
//! The word after `reason: ` is a stable token (`untyped`, `builtin`,
//! `supervisor`, `abstract`, `actor`, `open`, `exception`, …) for counting
//! sites by reason.

use super::CoreErlangGenerator;
use super::dispatch_codegen::{StaysDynamic, is_exception_family};
use super::routing::{self, Guard, Representation};
use beamtalk_core::ast::{ClassKind, Expression, MessageSelector};
use beamtalk_core::semantic_analysis::{InferredType, TypeProvenance};

/// How the receiver's static type reads in the report.
fn describe_type(ty: Option<&InferredType>, kind: Option<ClassKind>) -> String {
    let Some(ty) = ty else {
        return "untyped (no recorded type)".to_string();
    };
    let shape = match ty {
        InferredType::Known { provenance, .. } => {
            let kind = kind.map_or("unknown class", |kind| match kind {
                ClassKind::Object => "Object",
                ClassKind::Actor => "Actor",
                ClassKind::Value => "Value",
            });
            format!("{kind}, {}", describe_provenance(provenance))
        }
        InferredType::Union { .. } => "union".to_string(),
        InferredType::Negation { .. } => "negation".to_string(),
        InferredType::Intersection { .. } => "intersection".to_string(),
        InferredType::Meta { .. } => "metatype".to_string(),
        InferredType::Dynamic(_) => "dynamic".to_string(),
        InferredType::Never => "never".to_string(),
    };
    format!("{} [{shape}]", ty.display_name())
}

fn describe_provenance(provenance: &TypeProvenance) -> &'static str {
    match provenance {
        TypeProvenance::Declared(_) => "declared",
        TypeProvenance::Inferred(_) => "inferred",
        TypeProvenance::Substituted(_) => "substituted",
        TypeProvenance::Extracted => "extracted",
        TypeProvenance::Aliased { .. } => "aliased",
    }
}

fn describe_guard(guard: Guard) -> String {
    match guard {
        Guard::Bif(bif) => bif.to_string(),
        Guard::StringTag => "string_tag".to_string(),
        Guard::BinaryOrStringTag => "binary_or_string_tag".to_string(),
        Guard::UnalignedBitstring => "unaligned_bitstring".to_string(),
        Guard::UnreservedTuple => "unreserved_tuple".to_string(),
        Guard::Symbol => "symbol".to_string(),
        Guard::ExactAtom(atom) => format!("=:= {atom}"),
        Guard::UntaggedMap => "untagged_map".to_string(),
    }
}

/// The routing row's account of a representation-routed class.
fn describe_routed(class: &str, selector: &str) -> String {
    let Some(row) = routing::row(class) else {
        return format!("builtin — {class} is representation-routed");
    };
    if let Some(why) = row.dynamic_only {
        return format!("builtin — {class}: {why}");
    }
    if row.special_selectors.contains(&selector) {
        return format!(
            "builtin — {class}: '{selector}' is answered outside {}",
            row.target_module.unwrap_or("its module")
        );
    }
    match (row.guard, row.target_module) {
        (Some(guard), Some(module)) => format!(
            "builtin — representation-routed: {} guard -> {module}",
            describe_guard(guard)
        ),
        _ => format!("builtin — {class} has no single representation route"),
    }
}

impl CoreErlangGenerator {
    /// The `(receiver: …; reason: …)` part of a dynamic-dispatch hint for
    /// `receiver selector`.
    pub(in crate::core_erlang) fn dynamic_site_description(
        &self,
        receiver: &Expression,
        selector: &MessageSelector,
    ) -> String {
        let ty = self.type_map.get(receiver.span());
        let class_name = match ty {
            Some(InferredType::Known { class_name, .. }) => Some(class_name.as_str()),
            _ => None,
        };
        let hierarchy = self.class_hierarchy.as_ref();
        let kind = class_name.and_then(|class| {
            hierarchy
                .filter(|h| h.get_class(class).is_some())
                .map(|h| h.resolve_class_kind(class))
        });
        let reason = match self.typed_sealed_send_target(receiver, selector) {
            // The `typed_sealed` handler would have claimed this send had it
            // reached `HANDLERS`; a dynamic send here came another way.
            Ok(_) => "eligible — a guarded direct call applies, but this send site bypassed the typed_sealed handler".to_string(),
            Err(gate) => self.describe_gate(&gate, class_name, kind, selector.name().as_str()),
        };
        format!("receiver: {}; reason: {reason}", describe_type(ty, kind))
    }

    fn describe_gate(
        &self,
        gate: &StaysDynamic,
        class_name: Option<&str>,
        kind: Option<ClassKind>,
        selector: &str,
    ) -> String {
        let class = class_name.unwrap_or("?");
        match gate {
            StaysDynamic::Untyped => "untyped".to_string(),
            StaysDynamic::BinarySelector => "binary-selector".to_string(),
            StaysDynamic::FieldAtPut => {
                "fieldAt:put: — the dynamic path raises its own hint".to_string()
            }
            StaysDynamic::RepresentationRouted => describe_routed(class, selector),
            StaysDynamic::RuntimeOnlyBuiltin => {
                format!("runtime-only — {class} has no compiled module")
            }
            StaysDynamic::UnknownClass => format!("unknown-class — {class} is not in scope"),
            StaysDynamic::NotSealed => self.describe_open_class(class, kind),
            StaysDynamic::AmbiguousPackage => {
                format!("ambiguous — {class} is declared by more than one package (BT-4151)")
            }
            StaysDynamic::ModuleLess => format!("module-less — {class} has no compiled module"),
            StaysDynamic::NotValueKind => format!("actor — {class} instances are actor references"),
            StaysDynamic::ExceptionFamily => {
                format!("exception — {class} dispatches through bt@stdlib@exception")
            }
            StaysDynamic::CrossFileParent => {
                format!("cross-file-parent — {class}'s ancestry is not fully known")
            }
        }
    }

    /// Detail for an open (unsealed) class: which exclusion a guarded call
    /// to it would still hit once the sealed gate is lifted (BT-4293).
    fn describe_open_class(&self, class: &str, kind: Option<ClassKind>) -> String {
        let Some(hierarchy) = self.class_hierarchy.as_ref() else {
            return format!("open — {class} is not sealed");
        };
        if let Some(supervisor) = self.describe_supervisor_family(class) {
            return supervisor;
        }
        if is_exception_family(hierarchy, class) {
            return format!("exception — {class} dispatches through bt@stdlib@exception");
        }
        if kind == Some(ClassKind::Actor) {
            return format!("actor — {class} instances are actor references");
        }
        if hierarchy
            .get_class(class)
            .is_some_and(|info| info.is_abstract)
        {
            return format!("abstract — {class} has no instances of its own");
        }
        format!("open — {class} is not sealed")
    }

    /// The `supervisor` reason when `class` is a `Supervisor` /
    /// `DynamicSupervisor` (or a subclass) behind the `NotSealed` gate: its
    /// instances are `beamtalk_supervisor` tuples.
    fn describe_supervisor_family(&self, class: &str) -> Option<String> {
        let hierarchy = self.class_hierarchy.as_ref()?;
        let root = std::iter::once(class.into())
            .chain(hierarchy.superclass_chain(class))
            .find(|ancestor: &ecow::EcoString| {
                routing::row(ancestor.as_str())
                    .is_some_and(|row| row.representation == Representation::SupervisorTuple)
            })?;
        Some(format!(
            "supervisor — {class} instances are {root} handles (beamtalk_supervisor tuples)"
        ))
    }
}
