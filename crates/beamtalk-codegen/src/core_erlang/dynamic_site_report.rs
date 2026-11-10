// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The dynamic-site report (BT-4292): with `BEAMTALK_CODEGEN_DIAGNOSTICS=1`,
//! every send that compiles to `beamtalk_message_dispatch:send/3` gets a hint
//! naming the receiver's static type and the reason it stayed dynamic.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! The reason is not re-derived here: it is the [`StaysDynamic`] gate that
//! [`CoreErlangGenerator::typed_send_target`] — the same decision the
//! `typed_guarded` handler emits from — rejected the send at. This module
//! only describes it, adding detail the gate does not need (the routing row
//! of a built-in, the supervisor-family root of a supervisor subclass). Epic BT-4291 measures its baseline and progress with
//! it (`benchmarks/vs-elixir/results/bench-bt4292-baseline.md`).
//!
//! Each hint reads
//!
//! ```text
//! Send 'whenResolved:' at line 12: dynamic dispatch (receiver: Pid [Object, declared]; reason: builtin — Pid: the dynamic path answers 'whenResolved:' outside bt@stdlib@pid)
//! ```
//!
//! The word after `reason: ` is a stable token (`untyped`, `builtin`,
//! `supervisor`, `abstract`, `root`, `actor`, `exception`, …) for counting
//! sites by reason.

use super::CoreErlangGenerator;
use super::dispatch_codegen::{StaysDynamic, supervisor_family_root};
use super::routing::{self, NoBuiltinRoute};
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
        InferredType::ProtocolMeta { .. } => "protocol metatype".to_string(),
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

/// The routing fixture's account of why a representation-routed class has
/// no guarded direct route (BT-4294).
fn describe_routed(class: &str, why: NoBuiltinRoute) -> String {
    let row = routing::row(class);
    let target = row
        .and_then(|row| row.target_module)
        .unwrap_or("its module");
    match why {
        NoBuiltinRoute::DynamicOnly(why) => format!("builtin — {class}: {why}"),
        NoBuiltinRoute::NoSingleRoute => {
            format!("builtin — {class} has no single representation route")
        }
        NoBuiltinRoute::SpecialSelector(selector) => {
            format!("builtin — {class}: the dynamic path answers '{selector}' outside {target}")
        }
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
        let reason = match self.typed_send_target(receiver, selector) {
            // The `typed_guarded` handler would have claimed this send had it
            // reached `HANDLERS`; a dynamic send here came another way.
            Ok(_) => "eligible — a guarded direct call applies, but this send site bypassed the typed_guarded handler".to_string(),
            Err(gate) => self.describe_gate(&gate, class_name),
        };
        format!("receiver: {}; reason: {reason}", describe_type(ty, kind))
    }

    fn describe_gate(&self, gate: &StaysDynamic, class_name: Option<&str>) -> String {
        let class = class_name.unwrap_or("?");
        match gate {
            StaysDynamic::Untyped => "untyped".to_string(),
            StaysDynamic::BinarySelector => "binary-selector".to_string(),
            StaysDynamic::FieldAtPut => {
                "fieldAt:put: — the dynamic path raises its own hint".to_string()
            }
            StaysDynamic::PerformFamily => {
                "perform — the dynamic path does not dispatch this selector as sent".to_string()
            }
            StaysDynamic::RepresentationRouted(why) => describe_routed(class, *why),
            StaysDynamic::RuntimeOnlyBuiltin => {
                format!("runtime-only — {class} has no compiled module")
            }
            StaysDynamic::UnknownClass => format!("unknown-class — {class} is not in scope"),
            StaysDynamic::Abstract => format!("abstract — {class} has no instances of its own"),
            StaysDynamic::RepresentationAncestor => format!(
                "root — {class} is an ancestor of representation-routed built-ins, so its instances are rarely tagged {class}"
            ),
            StaysDynamic::SupervisorFamily => self.describe_supervisor_family(class),
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

    /// The `supervisor` reason: `class`'s instances are the `beamtalk_supervisor`
    /// tuples of its supervisor-family root.
    fn describe_supervisor_family(&self, class: &str) -> String {
        let root = self
            .class_hierarchy
            .as_ref()
            .and_then(|hierarchy| supervisor_family_root(hierarchy, class))
            .unwrap_or_else(|| class.into());
        format!("supervisor — {class} instances are {root} handles (beamtalk_supervisor tuples)")
    }
}
