// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Sendability tiers (ADR 0103).
//!
//! Derives a *sendability tier* for every inferred type from its class kind
//! (ADR 0067). The tier answers "will this value still mean what the sender
//! thinks after it is copied across a process boundary?" — actor message
//! arguments, `spawnWith:` maps, blocks handed to actors, and Announcement
//! payloads all copy the term on the BEAM.
//!
//! Advisory only (ADR 0100): findings are warnings/hints, never compile
//! blockers, and are silent whenever the checker lacks the knowledge to grade
//! them (`Dynamic`, unclassified `Object` kinds, `#node`-scoped sends in v1).
//!
//! This module is the **single source of truth** for tier derivation — the
//! Phase 2 message-argument / `spawnWith:` / Announcement checks and the
//! Phase 3 block-capture validator all call [`tier_of`], so the tiers cannot
//! drift between boundaries (ADR 0103 Negative consequences).
//!
//! ## Phasing
//!
//! Phase 0 (this file's first cut) ships the builtin hazard table, the
//! primitive fallback, and the coarse class-kind fallback. The `Known` arm is
//! structured so that later phases slot in without touching the boundary
//! checks:
//!
//! * **Phase 1 (BT-2755):** `Value` structural composition (weakest field
//!   tier), generic `type_args` (`List(Port)` → `HandleScoped`), and user
//!   `handleScope:` declarations.

use ecow::EcoString;

use crate::ast::ClassKind;
use crate::semantic_analysis::ClassHierarchy;

use super::InferredType;

/// The scope over which a `HandleScoped` value's backing handle stays valid.
///
/// The symbol set is deliberately **open** (ADR 0103): `#process` and `#node`
/// ship first, and any other user-declared symbol is carried through as
/// [`HandleScope::Other`]. Only `#process` produces a diagnostic in v1;
/// `#node` and other scopes are silent (no static remoteness knowledge — see
/// ADR 0079, deferred cluster-registration ADR).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum HandleScope {
    /// Bound to its owning process (ports, file handles). Sends warn.
    Process,
    /// Node-local (e.g. an ETS-backed row). Silent in v1.
    Node,
    /// Any other user-declared scope symbol. Silent in v1.
    Other(EcoString),
}

impl HandleScope {
    /// Parse a declared `handleScope:` symbol into a scope. The stored symbol
    /// text has no leading `#` (symbols are interned as their bare name).
    ///
    /// Consumed by the Phase 1 tier derivation (BT-2755) once user
    /// `handleScope:` declarations are read from `ClassInfo`.
    #[must_use]
    #[allow(dead_code)] // Consumed by Phase 1 tier derivation (BT-2755).
    pub(crate) fn from_symbol(sym: &str) -> Self {
        match sym {
            "process" => HandleScope::Process,
            "node" => HandleScope::Node,
            other => HandleScope::Other(EcoString::from(other)),
        }
    }

    /// How hazardous this scope is when composing two `HandleScoped` fields.
    /// `#process` is the tightest bound, so it wins a tie.
    fn hazard_rank(&self) -> u8 {
        match self {
            HandleScope::Process => 2,
            HandleScope::Node => 1,
            HandleScope::Other(_) => 0,
        }
    }
}

/// A value's sendability tier (ADR 0103 §Tiers).
///
/// Ordering for structural composition: `Sendable < SendableRef <
/// HandleScoped`, with `Unknown` outranking all three (an unclassified field
/// makes the whole thing unclassified, and therefore silent).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Tier {
    /// Value kinds, primitives, symbols, opaque tokens with no owner
    /// (`Reference`). A copy is semantically free.
    Sendable,
    /// Pid-backed references: Actor kinds and the builtin `Pid`. The copy
    /// preserves reference identity; validity is tied to the process lifetime.
    /// No diagnostic rule consumes this tier in v1 — its sole consumer is
    /// hover (ADR 0103 §Tiers, "honest scoping note").
    SendableRef,
    /// Object kinds wrapping runtime-backed state whose validity is scoped.
    HandleScoped(HandleScope),
    /// Dynamic, untyped FFI results, or unclassified Object kinds. Silent.
    Unknown,
}

impl Tier {
    /// Structural rank used by [`join`](Self::join). Higher = weaker/less
    /// sendable; `Unknown` is highest so it dominates a composite.
    fn rank(&self) -> u8 {
        match self {
            Tier::Sendable => 0,
            Tier::SendableRef => 1,
            Tier::HandleScoped(_) => 2,
            Tier::Unknown => 3,
        }
    }

    /// Combine two tiers, taking the **weakest** (ADR 0103 §Tiers composition).
    ///
    /// Used for unions (a value may be any member) and — from Phase 1 — for
    /// `Value` field composition and generic `type_args`. When both operands
    /// are `HandleScoped` the more hazardous scope wins (`#process` over
    /// `#node`) so a composite carrying a process-bound handle still warns.
    #[must_use]
    pub(crate) fn join(self, other: Tier) -> Tier {
        use std::cmp::Ordering;
        match self.rank().cmp(&other.rank()) {
            Ordering::Less => other,
            Ordering::Greater => self,
            Ordering::Equal => match (&self, &other) {
                (Tier::HandleScoped(a), Tier::HandleScoped(b)) => {
                    if b.hazard_rank() > a.hazard_rank() {
                        other
                    } else {
                        self
                    }
                }
                _ => self,
            },
        }
    }

    /// Combine two tiers taking the **strongest** (most-known) — used for
    /// intersections, where the value satisfies every member at once, so the
    /// least hazardous member describes it best and avoids false warnings.
    fn meet(self, other: Tier) -> Tier {
        if other.rank() < self.rank() {
            other
        } else {
            self
        }
    }
}

/// Builtin tier table (ADR 0103) — classified directly in the checker, not via
/// source annotations, because these are the canonical hazards and one of them
/// (`Pid`) breaks the naive kind→tier rule.
// Each arm documents a distinct builtin even where the resulting tier coincides.
#[allow(clippy::match_same_arms)]
fn builtin_tier(class_name: &str) -> Option<Tier> {
    let tier = match class_name {
        // Live process reference; valid across nodes.
        "Pid" => Tier::SendableRef,
        // Port dies with / is bound to its owner.
        "Port" => Tier::HandleScoped(HandleScope::Process),
        // Opaque comparison token, no owner, no hazard.
        "Reference" => Tier::Sendable,
        // Wraps a node-local ETS row.
        "Subscription" => Tier::HandleScoped(HandleScope::Node),
        // Approximation: true scope is dynamic extent; `#process` is the
        // conservative expressible bound (ADR 0103 Consequences).
        "FileHandle" => Tier::HandleScoped(HandleScope::Process),
        _ => return None,
    };
    Some(tier)
}

/// Scalar primitives that are always `Sendable` regardless of where they sit
/// in the class hierarchy. `Integer`/`Float`/`Boolean` also reach `Sendable`
/// via their `Value`-kind ancestry, but `String` (→ `Binary` → `Collection`)
/// and `Symbol` (→ `Object`) do not, so they are listed explicitly.
fn is_primitive_sendable(class_name: &str) -> bool {
    matches!(
        class_name,
        "Integer"
            | "Float"
            | "Number"
            | "String"
            | "Symbol"
            | "Boolean"
            | "Character"
            | "Nil"
            | "UndefinedObject"
            | "Binary"
            | "ByteArray"
    )
}

/// Derive the sendability tier of an inferred type. **The single source of
/// truth** — every boundary check calls this.
// `Never` and `Meta` coincide at `Sendable` but describe distinct cases.
#[allow(clippy::match_same_arms)]
pub(crate) fn tier_of(ty: &InferredType, hierarchy: &ClassHierarchy) -> Tier {
    match ty {
        // Untyped FFI / Dynamic: no knowledge to grade on (ADR 0075, 0100).
        InferredType::Dynamic(_) => Tier::Unknown,
        // Uninhabited — no value ever crosses a boundary. Silent.
        InferredType::Never => Tier::Sendable,
        // A class object is an atom-like reference (module name); free to copy.
        InferredType::Meta { .. } => Tier::Sendable,
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => tier_of_known(class_name, type_args, hierarchy),
        // A union value may be *any* member — take the weakest so a hazardous
        // member is not masked by a sendable one.
        InferredType::Union { members, .. } => members
            .iter()
            .map(|m| tier_of(m, hierarchy))
            .reduce(Tier::join)
            .unwrap_or(Tier::Unknown),
        // An intersection value satisfies *every* member — the most-known
        // member describes it best.
        InferredType::Intersection { members, .. } => members
            .iter()
            .map(|m| tier_of(m, hierarchy))
            .reduce(Tier::meet)
            .unwrap_or(Tier::Unknown),
        // `A \ B` is still an `A`.
        InferredType::Negation { base, .. } => tier_of(base, hierarchy),
    }
}

fn tier_of_known(
    class_name: &EcoString,
    type_args: &[InferredType],
    hierarchy: &ClassHierarchy,
) -> Tier {
    let name = class_name.as_str();
    // Symbol singletons (`#foo`) are plain atoms.
    if name.starts_with('#') {
        return Tier::Sendable;
    }
    // The builtin hazard table takes precedence over class kind: `Pid` is
    // Object-kind but behaves as `SendableRef`, and `Port`/`Subscription`/…
    // carry scoped handles that the coarse kind fallback would miss.
    if let Some(tier) = builtin_tier(name) {
        return tier;
    }
    if is_primitive_sendable(name) {
        return Tier::Sendable;
    }
    // Phase 1 (BT-2755) extends this arm with `Value` structural composition
    // over fields, generic `type_args` (`List(Port)` → `HandleScoped`), and
    // user `handleScope:` declarations. Phase 0 uses the coarse kind fallback.
    let _ = type_args;
    match hierarchy.resolve_class_kind(name) {
        ClassKind::Value => Tier::Sendable,
        ClassKind::Actor => Tier::SendableRef,
        ClassKind::Object => Tier::Unknown,
    }
}

/// Hover label for a value's tier, or `None` when nothing should be shown.
///
/// Phase 0's sole tier consumer is hover, and its v1 contract is fixed:
/// display `SendableRef` for `Pid`- and Actor-typed values (ADR 0103 §Tiers).
/// Other tiers render nothing yet — Phase 1 (BT-2755) broadens this to all
/// tiers.
#[must_use]
pub(crate) fn hover_tier_label(
    ty: &InferredType,
    hierarchy: &ClassHierarchy,
) -> Option<&'static str> {
    match tier_of(ty, hierarchy) {
        Tier::SendableRef => Some("SendableRef"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn known(name: &str) -> InferredType {
        InferredType::known(EcoString::from(name))
    }

    fn hierarchy() -> ClassHierarchy {
        ClassHierarchy::with_builtins()
    }

    #[test]
    fn builtin_table_tiers() {
        let h = hierarchy();
        assert_eq!(tier_of(&known("Pid"), &h), Tier::SendableRef);
        assert_eq!(
            tier_of(&known("Port"), &h),
            Tier::HandleScoped(HandleScope::Process)
        );
        assert_eq!(tier_of(&known("Reference"), &h), Tier::Sendable);
        assert_eq!(
            tier_of(&known("Subscription"), &h),
            Tier::HandleScoped(HandleScope::Node)
        );
        assert_eq!(
            tier_of(&known("FileHandle"), &h),
            Tier::HandleScoped(HandleScope::Process)
        );
    }

    #[test]
    fn primitives_are_sendable() {
        let h = hierarchy();
        for p in ["Integer", "String", "Symbol", "Boolean", "Float"] {
            assert_eq!(tier_of(&known(p), &h), Tier::Sendable, "{p}");
        }
    }

    #[test]
    fn symbol_singleton_is_sendable() {
        let h = hierarchy();
        assert_eq!(tier_of(&known("#west"), &h), Tier::Sendable);
    }

    #[test]
    fn dynamic_is_unknown() {
        let h = hierarchy();
        assert_eq!(
            tier_of(
                &InferredType::Dynamic(super::super::DynamicReason::UntypedFfi),
                &h
            ),
            Tier::Unknown
        );
    }

    #[test]
    fn unclassified_object_is_unknown() {
        let h = hierarchy();
        // A bare user Object subclass with no classification is silent.
        assert_eq!(tier_of(&known("SomeRandomClass"), &h), Tier::Unknown);
    }

    #[test]
    fn hover_shows_sendableref_only() {
        let h = hierarchy();
        assert_eq!(hover_tier_label(&known("Pid"), &h), Some("SendableRef"));
        assert_eq!(hover_tier_label(&known("Port"), &h), None);
        assert_eq!(hover_tier_label(&known("Integer"), &h), None);
    }

    #[test]
    fn join_takes_weakest() {
        assert_eq!(Tier::Sendable.join(Tier::SendableRef), Tier::SendableRef);
        assert_eq!(
            Tier::Sendable.join(Tier::HandleScoped(HandleScope::Process)),
            Tier::HandleScoped(HandleScope::Process)
        );
        assert_eq!(Tier::SendableRef.join(Tier::Unknown), Tier::Unknown);
        // process outranks node on a tie
        assert_eq!(
            Tier::HandleScoped(HandleScope::Node).join(Tier::HandleScoped(HandleScope::Process)),
            Tier::HandleScoped(HandleScope::Process)
        );
    }
}
