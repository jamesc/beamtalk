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
use crate::semantic_analysis::alias_registry::AliasRegistry;

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
    #[must_use]
    pub(crate) fn from_symbol(sym: &str) -> Self {
        match sym {
            "process" => HandleScope::Process,
            "node" => HandleScope::Node,
            other => HandleScope::Other(EcoString::from(other)),
        }
    }

    /// The bare scope symbol text, for display (`process` / `node` / …).
    fn as_atom(&self) -> &str {
        match self {
            HandleScope::Process => "process",
            HandleScope::Node => "node",
            HandleScope::Other(sym) => sym.as_str(),
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

/// Recursion cap for structural composition — guards against Value types with
/// self-referential fields (`Value subclass: Tree` with a `Tree` field). At the
/// cap, composition contributes nothing (`Sendable`), biasing toward the
/// already-determined shallow tier rather than over-warning.
const MAX_COMPOSE_DEPTH: u8 = 12;

/// Derive the sendability tier of an inferred type. **The single source of
/// truth** — every boundary check calls this.
///
/// `alias_registry` (BT-2936, ADR 0108 follow-up to BT-2928) is threaded
/// through to the `Value` structural composition below so an alias-typed
/// field expands to its declared type's tier instead of falling back to
/// `Tier::Unknown` for the opaque alias name. Pass `None` when no registry
/// is available, matching this function's pre-BT-2936 behaviour.
#[must_use]
pub(crate) fn tier_of(
    ty: &InferredType,
    hierarchy: &ClassHierarchy,
    alias_registry: Option<&AliasRegistry>,
) -> Tier {
    tier_of_depth(ty, hierarchy, 0, alias_registry)
}

// `Never` and `Meta` coincide at `Sendable` but describe distinct cases.
#[allow(clippy::match_same_arms)]
fn tier_of_depth(
    ty: &InferredType,
    hierarchy: &ClassHierarchy,
    depth: u8,
    alias_registry: Option<&AliasRegistry>,
) -> Tier {
    if depth >= MAX_COMPOSE_DEPTH {
        return Tier::Sendable;
    }
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
        } => tier_of_known(class_name, type_args, hierarchy, depth, alias_registry),
        // A union value may be *any* member — take the weakest so a hazardous
        // member is not masked by a sendable one.
        InferredType::Union { members, .. } => members
            .iter()
            .map(|m| tier_of_depth(m, hierarchy, depth + 1, alias_registry))
            .reduce(Tier::join)
            .unwrap_or(Tier::Unknown),
        // An intersection value satisfies *every* member at once, so its
        // runtime representation embeds every member's hazard — one
        // `HandleScoped` facet must pull the whole type up, not be masked by a
        // safer co-member. Same weakest-wins rule as a union.
        InferredType::Intersection { members, .. } => members
            .iter()
            .map(|m| tier_of_depth(m, hierarchy, depth + 1, alias_registry))
            .reduce(Tier::join)
            .unwrap_or(Tier::Unknown),
        // `A \ B` is still an `A`.
        InferredType::Negation { base, .. } => {
            tier_of_depth(base, hierarchy, depth, alias_registry)
        }
    }
}

fn tier_of_known(
    class_name: &EcoString,
    type_args: &[InferredType],
    hierarchy: &ClassHierarchy,
    depth: u8,
    alias_registry: Option<&AliasRegistry>,
) -> Tier {
    let name = class_name.as_str();
    // Symbol singletons (`#foo`) are plain atoms.
    if name.starts_with('#') {
        return Tier::Sendable;
    }
    // The builtin hazard table takes precedence over class kind: `Pid` is
    // Object-kind but behaves as `SendableRef`, and `Port`/`Subscription`/…
    // carry scoped handles that the coarse kind fallback would miss. Builtins
    // are non-generic, so no composition applies.
    if let Some(tier) = builtin_tier(name) {
        return tier;
    }
    if is_primitive_sendable(name) {
        return Tier::Sendable;
    }

    // Base tier from the class kind, then compose structurally. A user
    // `handleScope:` declaration classifies an otherwise-`Unknown` Object
    // (BT-2754); it is a no-op on Value/Actor kinds (see
    // `check_handle_scope_on_object`), which keep their structural tier — so
    // the scope is consulted only inside the `Object` arm, and an `Actor` with
    // a stray `handleScope:` stays `SendableRef`, not `HandleScoped`.
    let kind = hierarchy.resolve_class_kind(name);
    let mut tier = match kind {
        ClassKind::Value => Tier::Sendable,
        ClassKind::Actor => Tier::SendableRef,
        ClassKind::Object => match hierarchy.handle_scope(name) {
            Some(scope) => Tier::HandleScoped(HandleScope::from_symbol(scope.as_str())),
            // Unclassified Object with no declaration stays silent.
            None => Tier::Unknown,
        },
    };

    // Generic composition (ADR 0102 `type_args`): a container is as weak as its
    // weakest element — `List(Port)` (a `Collection` → `Value`) → `HandleScoped`.
    for arg in type_args {
        tier = tier.join(tier_of_depth(arg, hierarchy, depth + 1, alias_registry));
    }

    // `Value` structural composition: a Value inherits the weakest tier of its
    // declared fields — a `SendableRef` field makes it `SendableRef`, a
    // `HandleScoped` field makes it `HandleScoped`, an untyped/`Unknown` field
    // makes the composite `Unknown` (ADR 0103 §Tiers).
    if kind == ClassKind::Value {
        for field in hierarchy.all_state(name) {
            let field_tier = match hierarchy.state_field_type(name, &field) {
                // Parse the stored type-name string through the shared resolver
                // so generic annotations keep their `type_args`: `"List(Port)"`
                // becomes `Known("List", [Known("Port")])`, not an opaque
                // `Known("List(Port)")`. Without this the whole annotation would
                // be treated as the class name, `resolve_class_kind` would find
                // no such class, and a generic field would silently drop to
                // `Unknown` instead of composing its element tier (BT-2770).
                // BT-2936: `alias_registry` (threaded from the top-level
                // `tier_of` call) additionally expands an alias-typed field
                // to its declared type before tiering, instead of treating
                // the alias name as an unresolved nominal class.
                Some(field_ty) => {
                    let field_type =
                        super::TypeChecker::resolve_type_name_string(&field_ty, alias_registry);
                    tier_of_depth(&field_type, hierarchy, depth + 1, alias_registry)
                }
                // An untyped field carries no static tier — treat as Unknown.
                None => Tier::Unknown,
            };
            tier = tier.join(field_tier);
        }
    }

    tier
}

/// Hover label for a value's tier, or `None` when nothing should be shown.
///
/// Hover is the sole v1 consumer of the tier lattice (ADR 0103 §Tiers). It
/// renders the meaningful tiers — `SendableRef` (Pid/Actor references) and
/// `HandleScoped(#scope)` (scoped handles, including tiers a `Value` inherited
/// structurally) — so a developer sees the send semantics truthfully. The two
/// silent tiers are deliberately not shown: `Sendable` is the unremarkable
/// default (rendering it on every `Integer` would be noise) and `Unknown`
/// means the checker has nothing to say.
#[must_use]
pub(crate) fn hover_tier_label(
    ty: &InferredType,
    hierarchy: &ClassHierarchy,
    alias_registry: Option<&AliasRegistry>,
) -> Option<String> {
    match tier_of(ty, hierarchy, alias_registry) {
        Tier::SendableRef => Some("SendableRef".to_string()),
        Tier::HandleScoped(scope) => Some(format!("HandleScoped(#{})", scope.as_atom())),
        Tier::Sendable | Tier::Unknown => None,
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
        assert_eq!(tier_of(&known("Pid"), &h, None), Tier::SendableRef);
        assert_eq!(
            tier_of(&known("Port"), &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
        assert_eq!(tier_of(&known("Reference"), &h, None), Tier::Sendable);
        assert_eq!(
            tier_of(&known("Subscription"), &h, None),
            Tier::HandleScoped(HandleScope::Node)
        );
        assert_eq!(
            tier_of(&known("FileHandle"), &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
    }

    #[test]
    fn primitives_are_sendable() {
        let h = hierarchy();
        for p in ["Integer", "String", "Symbol", "Boolean", "Float"] {
            assert_eq!(tier_of(&known(p), &h, None), Tier::Sendable, "{p}");
        }
    }

    #[test]
    fn symbol_singleton_is_sendable() {
        let h = hierarchy();
        assert_eq!(tier_of(&known("#west"), &h, None), Tier::Sendable);
    }

    #[test]
    fn dynamic_is_unknown() {
        let h = hierarchy();
        assert_eq!(
            tier_of(
                &InferredType::Dynamic(super::super::DynamicReason::UntypedFfi),
                &h,
                None
            ),
            Tier::Unknown
        );
    }

    #[test]
    fn unclassified_object_is_unknown() {
        let h = hierarchy();
        // A bare user Object subclass with no classification is silent.
        assert_eq!(tier_of(&known("SomeRandomClass"), &h, None), Tier::Unknown);
    }

    #[test]
    fn hover_renders_meaningful_tiers() {
        let h = hierarchy();
        assert_eq!(
            hover_tier_label(&known("Pid"), &h, None).as_deref(),
            Some("SendableRef")
        );
        assert_eq!(
            hover_tier_label(&known("Port"), &h, None).as_deref(),
            Some("HandleScoped(#process)")
        );
        assert_eq!(
            hover_tier_label(&known("Subscription"), &h, None).as_deref(),
            Some("HandleScoped(#node)")
        );
        // Sendable and Unknown are the silent tiers — no hover label.
        assert_eq!(hover_tier_label(&known("Integer"), &h, None), None);
        assert_eq!(hover_tier_label(&known("SomeRandomClass"), &h, None), None);
    }

    #[test]
    fn generic_collection_composes_element_tier() {
        let h = hierarchy();
        // List(Port): List is a Collection → Value (base Sendable); the element
        // Port makes the whole collection HandleScoped(#process).
        let list_of_port = InferredType::known_with_args("List", vec![known("Port")]);
        assert_eq!(
            tier_of(&list_of_port, &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
        // List(Integer) stays Sendable.
        let list_of_int = InferredType::known_with_args("List", vec![known("Integer")]);
        assert_eq!(tier_of(&list_of_int, &h, None), Tier::Sendable);
    }

    #[test]
    fn dictionary_composes_value_tier() {
        let h = hierarchy();
        // Dictionary(String, Port) → HandleScoped via its value type arg.
        let dict =
            InferredType::known_with_args("Dictionary", vec![known("String"), known("Port")]);
        assert_eq!(
            tier_of(&dict, &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
    }

    #[test]
    fn user_handle_scope_classifies_object() {
        // A user Object subclass declaring handleScope: #process is HandleScoped.
        let tokens = crate::source_analysis::lex_with_eof(
            "typed Object subclass: PortBox native: pb\n  handleScope: #process",
        );
        let (module, _) = crate::source_analysis::parse(tokens);
        let h = ClassHierarchy::build(&module).0.unwrap();
        assert_eq!(
            tier_of(&known("PortBox"), &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
    }

    #[test]
    fn actor_ignores_stray_handle_scope() {
        // A `handleScope:` on an Actor class is a no-op (advisory elsewhere);
        // the tier stays SendableRef, not HandleScoped.
        let tokens = crate::source_analysis::lex_with_eof(
            "Actor subclass: MyActor\n  handleScope: #process",
        );
        let (module, _) = crate::source_analysis::parse(tokens);
        let h = ClassHierarchy::build(&module).0.unwrap();
        assert_eq!(tier_of(&known("MyActor"), &h, None), Tier::SendableRef);
    }

    #[test]
    fn intersection_takes_most_hazardous_member() {
        let h = hierarchy();
        // Pid & Port: the value embeds a process-bound handle, so the whole
        // intersection is HandleScoped(#process), not masked to SendableRef.
        let intersection = InferredType::Intersection {
            members: vec![known("Pid"), known("Port")],
            provenance: super::super::TypeProvenance::Extracted,
        };
        assert_eq!(
            tier_of(&intersection, &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
    }

    #[test]
    fn value_composition_inherits_weakest_field() {
        // A Value with a Port field inherits HandleScoped(#process).
        let tokens = crate::source_analysis::lex_with_eof(
            "typed Value subclass: Wrapper\n  field: p :: Port = nil",
        );
        let (module, _) = crate::source_analysis::parse(tokens);
        let h = ClassHierarchy::build(&module).0.unwrap();
        assert_eq!(
            tier_of(&known("Wrapper"), &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
    }

    #[test]
    fn value_composition_inherits_generic_field_element_tier() {
        // BT-2770: a Value with a `List(Port)` field must compose to
        // HandleScoped(#process), matching the bare-`Port` field case. The
        // stored field type is the string "List(Port)"; the fix parses it back
        // into `Known("List", [Known("Port")])` so the element tier survives.
        let tokens = crate::source_analysis::lex_with_eof(
            "typed Value subclass: Wrapper\n  field: ports :: List(Port) = nil",
        );
        let (module, _) = crate::source_analysis::parse(tokens);
        let h = ClassHierarchy::build(&module).0.unwrap();
        assert_eq!(
            tier_of(&known("Wrapper"), &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
    }

    #[test]
    fn value_composition_generic_sendable_field_stays_sendable() {
        // A `List(Integer)` field composes to Sendable — the generic parse must
        // not spuriously weaken a container of sendable elements.
        let tokens = crate::source_analysis::lex_with_eof(
            "typed Value subclass: IntBox\n  field: nums :: List(Integer) = nil",
        );
        let (module, _) = crate::source_analysis::parse(tokens);
        let h = ClassHierarchy::build(&module).0.unwrap();
        assert_eq!(tier_of(&known("IntBox"), &h, None), Tier::Sendable);
    }

    #[test]
    fn value_composition_dictionary_value_field_is_handle_scoped() {
        // `Dictionary(String, Port)` field → HandleScoped(#process) via its
        // value type arg, confirming multi-arg generics survive the parse too.
        let tokens = crate::source_analysis::lex_with_eof(
            "typed Value subclass: PortMap\n  field: byName :: Dictionary(String, Port) = nil",
        );
        let (module, _) = crate::source_analysis::parse(tokens);
        let h = ClassHierarchy::build(&module).0.unwrap();
        assert_eq!(
            tier_of(&known("PortMap"), &h, None),
            Tier::HandleScoped(HandleScope::Process)
        );
    }

    /// BT-2936: a `Value`'s alias-typed field composes the tier of the
    /// alias's *expansion* (here, `Port` → `HandleScoped(#process)`) when a
    /// registry is threaded through, instead of falling back to
    /// `Tier::Unknown` for the opaque alias name — the deferred half of
    /// BT-2928's `resolve_type_name_string` fix for sendability tiering.
    #[test]
    fn value_composition_inherits_alias_typed_field_tier() {
        use crate::semantic_analysis::alias_registry::AliasRegistry;
        use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

        let tokens = crate::source_analysis::lex_with_eof(
            "type PortAlias = Port\ntyped Value subclass: Wrapper\n  field: p :: PortAlias = nil",
        );
        let (module, parse_diags) = crate::source_analysis::parse(tokens);
        assert!(parse_diags.is_empty(), "parse failed: {parse_diags:?}");
        let h = ClassHierarchy::build(&module).0.unwrap();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &h, &protocol_registry);
        assert!(diags.is_empty(), "unexpected alias diagnostics: {diags:?}");

        assert_eq!(
            tier_of(&known("Wrapper"), &h, Some(&registry)),
            Tier::HandleScoped(HandleScope::Process)
        );
        // Without the registry, the alias name stays opaque — the
        // pre-BT-2936 fallback this test guards against regressing back to.
        assert_eq!(tier_of(&known("Wrapper"), &h, None), Tier::Unknown);
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
