// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Plain data records of detected diagnostic sites (split from `types.rs`).
//!
//! **DDD Context:** Semantic Analysis

use super::types::InferredType;
use crate::semantic_analysis::class_hierarchy::DeclaredType;
use crate::semantic_analysis::protocol_registry::ReturnTypeMismatch;
use ecow::EcoString;

// ── Diagnostic facts ──────────────────────────────────────────────────────
//
// A shared leaf between `inference/` and `validation/` (this module is a
// dependency of both already, via `InferredType`/`DynamicReason`): each
// struct below is a plain data record of something an `inference/` site
// *detected*, with no `Diagnostic`/message-formatting logic of its own.
// `inference/` constructs one and hands it to the matching `validation/`
// `emit_*` method, which does the actual rendering (message text, hint,
// category, severity) and pushes it. Splitting detection from rendering
// this way means inference never
// constructs `Diagnostic`s itself for these four sites. (The cons-tail site
// already had this split, via `InferredType::improper_cons_tail_display`'s
// existing `Option<EcoString>` — that one carries no new struct here, only
// its `validation/` renderer moved.)

/// A class-object type as the slot/argument assignability check sees it
/// (ADR 0083, ADR 0144): the class object of one named class (`Meta{C}`) or
/// of some class conforming to a protocol (`ProtocolMeta{P}`). Both render
/// as `<name> class`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::semantic_analysis::type_checker) enum ClassObjectType<'a> {
    /// `C class` — `Meta{C}`.
    Class(&'a EcoString),
    /// `P class` — `ProtocolMeta{P}`.
    Protocol(&'a EcoString),
}

impl<'a> ClassObjectType<'a> {
    /// The class-object view of `ty`, or `None` when it is not a class object.
    pub(in crate::semantic_analysis::type_checker) fn of(ty: &'a InferredType) -> Option<Self> {
        match ty {
            InferredType::Meta { class_name, .. } => Some(Self::Class(class_name)),
            InferredType::ProtocolMeta { protocol_name, .. } => Some(Self::Protocol(protocol_name)),
            _ => None,
        }
    }
}

impl std::fmt::Display for ClassObjectType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Class(name) | Self::Protocol(name) => write!(f, "{name} class"),
        }
    }
}

/// An instance-method override whose declared return type does not refine
/// the one it inherits. Found by `validation/overrides.rs::override_return_widening`,
/// rendered by `check_override_return_compatibility`, and consulted by
/// `ProtocolRegistry::conformance_return_type_mismatches` (BT-4175).
pub(crate) struct OverrideReturnWidening {
    pub(crate) child_rt: DeclaredType,
    pub(crate) parent_rt: DeclaredType,
    pub(crate) parent_class: EcoString,
}

/// A declared-vs-inferred type mismatch on an assignment's type annotation
/// (`x :: Declared := value`). Detected in `inference/assignment.rs`,
/// rendered by `validation/emit.rs::emit_assignment_type_mismatch`.
pub(in crate::semantic_analysis::type_checker) struct AssignmentTypeMismatch {
    pub(in crate::semantic_analysis::type_checker) declared_display: EcoString,
    pub(in crate::semantic_analysis::type_checker) inferred_display: EcoString,
    /// ADR 0147: when a map shape is involved, why the value is not
    /// assignable, naming the offending key.
    pub(in crate::semantic_analysis::type_checker) shape_mismatch:
        Option<super::shape_assignability::ShapeMismatch>,
    /// ADR 0144: when a class object `C class` misses a declared `P class`,
    /// the protocol `P` and the selectors `C` is missing — rendered as
    /// `C class does not conform to P class` instead of the plain mismatch.
    pub(in crate::semantic_analysis::type_checker) nonconformance: Option<MetatypeNonconformance>,
}

/// ADR 0144: why a class object `C class` is not a declared `P class` — the
/// selectors `C` is missing, and (BT-4490) the requirements it answers with
/// an incompatible declared return type, the same two findings
/// `ProtocolInfo::subsumes_conformance_to` judges for `P class <: Q class`.
pub(in crate::semantic_analysis::type_checker) struct MetatypeNonconformance {
    pub(in crate::semantic_analysis::type_checker) protocol: EcoString,
    /// As `ProtocolRegistry::check_conformance` reports them; class-side
    /// ones are prefixed `class `.
    pub(in crate::semantic_analysis::type_checker) missing: Vec<EcoString>,
    pub(in crate::semantic_analysis::type_checker) return_mismatches: Vec<ReturnTypeMismatch>,
}

/// An attempt to mutate another object's field (`other.field := value`,
/// rather than `self.field := value`) — objects cannot mutate another
/// object's state (Value types are immutable; actors only mutate their own
/// state via `self.x :=`). Detected in `inference/assignment.rs`, rendered
/// by `validation/emit.rs::emit_cross_object_field_mutation`.
pub(in crate::semantic_analysis::type_checker) struct CrossObjectFieldMutation {
    pub(in crate::semantic_analysis::type_checker) receiver_name: EcoString,
    pub(in crate::semantic_analysis::type_checker) field_name: EcoString,
}

/// An expression inside a `typed` class's method body inferred as
/// `Dynamic` for a root-cause reason (not a propagated `DynamicReceiver`, an
/// inert `Unknown`, or an author-acknowledged `ExplicitDynamic`). Detected
/// in `inference/mod.rs`, rendered by
/// `validation/emit.rs::emit_dynamic_in_typed_class`.
pub(in crate::semantic_analysis::type_checker) struct DynamicInTypedClass {
    pub(in crate::semantic_analysis::type_checker) class_name: EcoString,
    pub(in crate::semantic_analysis::type_checker) description: &'static str,
    /// For [`DynamicReason::UnresolvedSelector`], the unresolved selector, so
    /// the diagnostic names the real problem rather than suggesting a type
    /// annotation (BT-4526). `None` for every other reason.
    pub(in crate::semantic_analysis::type_checker) unresolved_selector: Option<EcoString>,
}
