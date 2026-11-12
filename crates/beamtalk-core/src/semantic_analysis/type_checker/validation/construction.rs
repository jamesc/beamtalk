// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Construction and field checks: field assignment, `spawn`/`new` definite assignment, spawn map keys and state-default validation.
//!
//! **DDD Context:** Semantic Analysis

//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

use crate::ast::{Expression, Literal, SlotKind};
use crate::semantic_analysis::alias_registry::AliasRegistry;
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::string_utils::edit_distance;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

use super::super::type_resolver;
use super::super::types::ClassObjectType;
use super::super::well_known::WellKnownClass;
use super::super::{InferredType, TypeChecker, TypeEnv};

impl TypeChecker {
    /// Check a field assignment `self.field := value` against the declared state type.
    ///
    /// If the field has a type annotation in the class hierarchy and the inferred
    /// value type is known and incompatible, emits a warning.
    #[allow(clippy::too_many_lines)] // includes alias-structural resolution
    pub(in super::super) fn check_field_assignment(
        &mut self,
        field: &crate::ast::Identifier,
        value_ty: &InferredType,
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &TypeEnv,
    ) {
        let Some(InferredType::Known { class_name, .. }) = env.get_local("self") else {
            return;
        };
        let Some(declared_type) = hierarchy.state_field_type(&class_name, &field.name) else {
            return; // No type annotation on this field
        };
        // `resolve_alias_structural` / `resolve_display_name` below
        // key an alias lookup by exact bare name — only a `DeclaredType::
        // Simple` could ever be a registered alias reference to begin with,
        // matching `state_field_type`'s bare-`EcoString` return —
        // so render the structured type once at this boundary.
        // BT-4607: the structured slot type, so a protocol-typed slot
        // (`P`, `P | Nil`, `P1 & P2`) is judged by structural conformance
        // (`slot_accepts`) rather than by name.
        let (declared_resolved, _) = type_resolver::resolve_declared_type_with_alias_deps(
            &declared_type,
            &type_resolver::SubstitutionMap::new(),
            self.protocol_registry.as_deref(),
            self.alias_registry.as_deref(),
            super::super::TypeStringContext::Declared,
        );
        // ADR 0147: a shape on either side is judged structurally;
        // `is_assignable_to` below knows no `#{...}` class.
        if self.check_shape_slot(
            value_ty,
            &declared_resolved,
            hierarchy,
            span,
            None,
            |value, declared| {
                format!(
                    "Type mismatch: field `{}` declared as {declared}, got {value}",
                    field.name
                )
            },
        ) {
            return;
        }
        let declared_type: EcoString = declared_type.to_string().into();
        // `ClassHierarchy::state_field_type` predates ADR 0108 and
        // returns a bare `EcoString` with no alias provenance — unlike an
        // `InferredType`, which tags via
        // `TypeProvenance::Aliased` when it flows through
        // `resolve_type_annotation`. Re-resolve `declared_type` through
        // `AliasRegistry::resolve_display_name` when it names a registered
        // alias so this diagnostic renders `AliasName (expansion)` just like
        // a mismatched alias-typed parameter already does; fall back to the
        // existing `UndefinedObject` -> `Nil` display otherwise.
        let declared_display = self
            .alias_registry
            .as_ref()
            .and_then(|registry| registry.resolve_display_name(&declared_type))
            .unwrap_or_else(|| InferredType::class_name_for_diagnostic(declared_type.as_str()));
        // `declared_type` above is still the bare, unresolved name —
        // `declared_display` only fixes the diagnostic text (display-only
        // scope). Resolve it to its structural expansion
        // (e.g. `RestartStrategy` -> `#temporary | #transient | #permanent`)
        // before it reaches `is_assignable_to`'s structural comparison below,
        // or an alias-typed field falsely rejects every one of its own union
        // members (no "unknown class" escape hatch in `is_assignable_to`).
        let (declared_type, alias_deps) =
            Self::resolve_alias_structural(&declared_type, self.alias_registry.as_deref());
        self.referenced_aliases.extend(alias_deps);
        // BT-4199: an explicit `:: Dynamic` annotation opts the slot out of
        // checking (ADR 0077), same as `check_state_defaults`;
        // `is_assignable_to` has no Dynamic escape of its own.
        if declared_type.as_str() == "Dynamic" {
            return;
        }
        match value_ty {
            InferredType::Known {
                class_name: value_type,
                ..
            } => {
                if !Self::slot_accepts(
                    value_ty,
                    value_type,
                    &declared_resolved,
                    &declared_type,
                    hierarchy,
                    self.protocol_registry.as_deref(),
                ) {
                    // Render `UndefinedObject` as `Nil` in user-facing messages.
                    let declared_display = declared_display.clone();
                    let value_display =
                        InferredType::class_name_for_diagnostic(value_type.as_str());
                    // ADR 0124 §3/§4f: assigning `nil` to a non-nilable
                    // `late` field is still this same type mismatch (a
                    // `late` field's type is never nilable — enforced by
                    // `late_slot_validators`) — but the hint should point at
                    // `clearField:`, the actual way to return the slot to
                    // "unassigned", rather than suggesting a differently
                    // *typed* value as the fix.
                    let hint = if WellKnownClass::from_str(value_type.as_str())
                        .is_some_and(WellKnownClass::is_nil_class)
                        && hierarchy.state_field_kind(&class_name, &field.name) == SlotKind::Late
                    {
                        format!(
                            "`{}` is `late` — use `clearField: #{}` to return it to the \
                             unassigned state instead of assigning nil",
                            field.name, field.name
                        )
                    } else {
                        format!("Expected {declared_display} but assigning {value_display}")
                    };
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Type mismatch: field `{}` declared as {declared_display}, got {value_display}",
                                field.name
                            ),
                            span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(hint),
                    );
                }
            }
            InferredType::Union { members, .. } => {
                // BT-4454: class-object members are judged by the slot rule,
                // as a bare class object is below.
                let declared_display_for_members = declared_display.clone();
                if let Some(diag) = self.union_class_object_diagnostic(
                    members,
                    &declared_type,
                    hierarchy,
                    Self::is_assignable_to,
                    span,
                    |class_object| {
                        (
                            format!(
                                "Type mismatch: field `{}` declared as {declared_display_for_members}, got {class_object}",
                                field.name
                            ),
                            format!(
                                "Expected {declared_display_for_members} but assigning {class_object}"
                            ),
                        )
                    },
                ) {
                    self.diagnostics.push(diag);
                    return;
                }
                // Check the other union members against the declared field type.
                let instance_members = Self::non_class_object_members(members);
                let Some((compat, total, incompatible)) =
                    Self::classify_union_members_with(&instance_members, |member, name| {
                        Self::slot_accepts(
                            member,
                            name,
                            &declared_resolved,
                            &declared_type,
                            hierarchy,
                            self.protocol_registry.as_deref(),
                        )
                    })
                else {
                    return; // Contains Dynamic — skip
                };
                if compat == total {
                    return; // All match → pass
                }
                // Accepted class objects are compatible members too.
                let compat = compat + (members.len() - instance_members.len());
                let union_display = value_ty
                    .display_for_diagnostic()
                    .unwrap_or_else(|| EcoString::from("Dynamic"));
                let declared_display = declared_display.clone();
                let diag = if compat == 0 {
                    Diagnostic::warning(
                        format!("Type mismatch: field `{}` declared as {declared_display}, got {union_display}", field.name),
                        span,
                    )
                    .with_hint(format!("No member of {union_display} is compatible with {declared_display}"))
                } else {
                    let list = incompatible
                        .iter()
                        .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    Diagnostic::hint(
                        format!("Type mismatch: field `{}` declared as {declared_display}, got {union_display}", field.name),
                        span,
                    )
                    .with_hint(format!("Some members of the union are not compatible with {declared_display}: {list}"))
                };
                self.diagnostics
                    .push(diag.with_category(DiagnosticCategory::Type));
            }
            InferredType::Meta { .. } | InferredType::ProtocolMeta { .. } => {
                // ADR 0083: a class object `C class` is assignable to a field
                // declared `:: Class` / `:: Behaviour` / `:: Object` (the
                // metaclass tower). Model the metatype as `Class` for the
                // assignability walk. ADR 0144: `:: P class` accepts it iff
                // `C` conforms to `P`.
                let Some(class_object) = ClassObjectType::of(value_ty) else {
                    return;
                };
                if !Self::metatype_assignable_to(
                    class_object,
                    &declared_type,
                    hierarchy,
                    self.protocol_registry.as_deref(),
                    Self::is_assignable_to,
                ) {
                    let declared_display = declared_display.clone();
                    let diag = Self::metatype_mismatch_diagnostic(
                        class_object,
                        &declared_type,
                        hierarchy,
                        self.protocol_registry.as_deref(),
                        format!(
                            "Type mismatch: field `{}` declared as {declared_display}, got {class_object}",
                            field.name
                        ),
                        format!("Expected {declared_display} but assigning {class_object}"),
                        span,
                    );
                    self.diagnostics.push(diag);
                }
            }
            // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip
            // conservatively like `Dynamic`/`Never` (ADR 0102). Likewise an
            // `Intersection` (`P1 & P2`, ADR 0102) as the value's own
            // inferred type — skip conservatively.
            // A shape is judged before this match (ADR 0147).
            InferredType::Dynamic(_)
            | InferredType::Never
            | InferredType::Negation { .. }
            | InferredType::Intersection { .. }
            | InferredType::Shape { .. } => {}
        }
    }

    /// Shared call-site boundary for `spawnWith:` map-literal inspection
    /// (ADR 0104 Phase 2).
    ///
    /// Returns the `MapPair`s of a `C spawnWith: #{...}` send when the argument
    /// is a *literal* map — the only point at which per-key inspection is
    /// possible, because map-literal inference otherwise collapses `#{...}` to a
    /// single joined `Dictionary(K, V)` and discards the individual literal
    /// keys. Returns `None` for any non-`spawnWith:` selector or a non-literal
    /// first argument (e.g. a `Dictionary`-typed variable), so callers never
    /// inspect a non-literal map — the key check produces no false positives.
    ///
    /// This is the reusable walk boundary: ADR 0103's value-sendability
    /// check iterates the *same* `(key, value)` pairs at the same call site and
    /// hooks in here.
    pub(in super::super) fn spawn_with_map_pairs<'a>(
        selector: &str,
        arguments: &'a [Expression],
    ) -> Option<&'a [crate::ast::MapPair]> {
        if selector != "spawnWith:" {
            return None;
        }
        match arguments.first() {
            Some(Expression::MapLiteral { pairs, .. }) => Some(pairs),
            _ => None,
        }
    }

    /// Check a `C spawnWith: #{...}` call site's map keys against `C`'s declared
    /// `state:` slots (ADR 0104 Phase 2).
    ///
    /// For each symbol key in the literal map:
    ///   * a key matching a declared slot (including inherited slots) passes;
    ///     when that slot carries a type annotation, the literal value's inferred
    ///     type is checked against it (see [`Self::check_spawn_with_value`]).
    ///   * an unknown key emits a **Warning** — the provably-failing tier per
    ///     ADR 0100, since a class's `state:` slot set is closed and declared
    ///     in-program — with a typo suggestion naming the nearest slot when one
    ///     is close (edit distance).
    ///
    /// No-ops when the argument is not a literal map (via
    /// [`Self::spawn_with_map_pairs`]) or when the class declares no state slots.
    pub(in super::super) fn check_spawn_with_map_keys(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arguments: &[Expression],
        hierarchy: &ClassHierarchy,
    ) {
        let Some(pairs) = Self::spawn_with_map_pairs(selector, arguments) else {
            return;
        };
        // `spawnWith:` is an Actor protocol (ADR 0104). A non-Actor class with
        // `state:` slots that receives a `spawnWith:` send is already a terminal
        // DNU — key-checking on top of that is noise. Guard on the class kind so
        // the diagnostic only fires where `spawnWith:` is a real constructor.
        // Supervisor / DynamicSupervisor receivers (ADR 0135 §1) are not Actor
        // subclasses, so they are skipped here too: a supervisor's
        // `spawnWith:` args are free-form start args (`self args`), not
        // `state:` slots.
        //
        // Scoping note: `is_actor_subclass` returns `false` when any link in the
        // superclass chain is absent from the hierarchy, so key-checking is
        // silently skipped for a partially-unresolved chain — a false negative,
        // never a false positive. This is consistent with `all_state` /
        // `state_field_type` / `superclass_chain`, which all break at the same
        // unknown ancestor.
        if !hierarchy.is_actor_subclass(class_name) {
            return;
        }
        // Declared state slot names, including inherited slots. An empty set
        // means the class declares no state (e.g. a builtin, or one whose slots
        // are not in the hierarchy) — bail rather than flag every key.
        let slots = hierarchy.all_state(class_name);
        if slots.is_empty() {
            return;
        }
        for pair in pairs {
            // Keys are symbol literals (`#count`); a non-symbol key is already a
            // parse error — skip it here.
            let Expression::Literal(Literal::Symbol(key_name), key_span) = &pair.key else {
                continue;
            };
            if slots.iter().any(|s| s.as_str() == key_name.as_str()) {
                // Known slot — value-check against the declared type when typed.
                if let Some(declared_ty) = hierarchy.state_field_type(class_name, key_name) {
                    self.check_spawn_with_value(
                        class_name,
                        key_name,
                        &declared_ty,
                        &pair.value,
                        hierarchy,
                    );
                }
                continue;
            }
            // Unknown key → Warning + typo suggestion.
            let message = match Self::closest_state_slot(key_name, &slots) {
                Some(suggestion) => format!(
                    "unknown state key `{key_name}` for `{class_name}` — did you mean `{suggestion}`?"
                ),
                None => format!("unknown state key `{key_name}` for `{class_name}`"),
            };
            self.diagnostics.push(
                Diagnostic::warning(message, *key_span)
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!(
                        "`spawnWith:` keys must be declared `state:` slots of `{class_name}`"
                    )),
            );
        }
    }

    /// Definite-assignment check at a Value construction site (ADR 0124 §6,
    /// §7, Implementation A3+A4): `Cls new` or a literal-map `Cls new:
    /// #{...}` that leaves a declared, no-default, non-nilable, non-`late`
    /// field unassigned.
    ///
    /// Values have no runtime post-`initialize` check at all —
    /// `generate_post_initialize_check` is wired only from
    /// `generate_handle_continue` (Actor-only) — so this static check is the
    /// *only* one a Value's typed-no-default fields ever get. Reuses the A1
    /// predicate via
    /// [`crate::semantic_analysis::requires_definite_assignment_for_declared_type`]
    /// (the `ClassHierarchy`-level sibling of
    /// [`crate::semantic_analysis::requires_definite_assignment`]) over
    /// [`ClassHierarchy::all_state`], so an inherited unassigned field is
    /// reported with the same rules as a directly-declared one.
    ///
    /// Called from the same three class-side-send call sites as
    /// [`Self::check_spawn_with_map_keys`] (`inference/send/receiver.rs`) —
    /// a syntactic `Cls new`, a `Meta`-typed receiver (`aClassVar new`), and
    /// a class method's own `self new: #{...}` factory call — so the
    /// canonical "class-method factory is the construction site" pattern
    /// (ADR 0124 §6, e.g. `WorkflowHandle class for:client:`) is covered the
    /// same way the `spawnWith:` key check already covers it.
    ///
    /// Fires only when:
    /// - `selector` is `new` (arity 0) or `new:` (arity 1) and `class_name`
    ///   is a Value subclass ([`ClassHierarchy::is_value_subclass`]) known to
    ///   the hierarchy.
    /// - Neither `class_name` nor any ancestor defines its own class-side
    ///   `new`/`new:` (checked per selector) — when it does, the send
    ///   doesn't resolve to the auto-generated constructor this check
    ///   assumes (ADR 0124 §6's "only when `new` resolves to the
    ///   auto-generated default" carve-out; applied to `new:` too since an
    ///   overridden `class new:` may do anything with its argument —
    ///   `stdlib/src/duration.bt:31`/`date_time.bt:41` ignore it entirely).
    /// - For `new:`, the argument is a *literal* map (a non-literal `new:
    ///   someMap` is no evidence, reported nothing — same boundary as
    ///   [`Self::spawn_with_map_pairs`]).
    ///
    /// Never fires for the auto-generated keyword constructor (a different
    /// selector entirely — it supplies every field by construction, ADR
    /// 0042) or for an untyped/defaulted/`late` field.
    ///
    /// Severity is Warning when the ancestor chain is fully known (no
    /// cross-file-unresolved or parse-error-degraded ancestor) and no
    /// ancestor is `native:`; Hint otherwise — ADR 0124 §6's severity table
    /// ("chain incomplete ... => Hint").
    pub(in super::super) fn check_value_construction_definite_assignment(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arguments: &[Expression],
        span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        if selector != "new" && selector != "new:" {
            return;
        }
        if class_name == "self" || class_name == "super" {
            return;
        }
        if !hierarchy.has_class(class_name) || !hierarchy.is_value_subclass(class_name) {
            return;
        }
        // Only when this selector resolves to the auto-generated
        // constructor — see this function's doc comment.
        if Self::value_new_selector_is_overridden(hierarchy, class_name, selector) {
            return;
        }

        // For `new:`, only a literal map is evidence; anything else
        // (including no argument at all, which can't parse as `new:`) is
        // silently skipped.
        let Some(pairs) = Self::construction_evidence_pairs(selector, "new:", arguments) else {
            return;
        };

        // Confidence: a fully-known, locally-visible ancestor chain with no
        // `native:` ancestor is a Warning; anything less certain is a Hint.
        let chain_known = !hierarchy.has_cross_file_parent(class_name)
            && !hierarchy.has_incomplete_surface_in_chain(class_name);
        let has_native_ancestor = hierarchy.is_native(class_name)
            || hierarchy
                .ancestors(class_name)
                .any(|ancestor| hierarchy.is_native(&ancestor.name));
        let confident = chain_known && !has_native_ancestor;

        self.report_unsupplied_slots(
            hierarchy,
            class_name,
            span,
            pairs,
            hierarchy.all_state(class_name),
            |_| false,
            confident,
            |field_name, type_display| {
                (
                    format!(
                        "`{class_name}` declares `{field_name} :: {type_display}` with no default, and `{class_name} {selector}` supplies no `{field_name}` — the constructed instance would hold `nil` there despite its non-nilable declared type"
                    ),
                    format!(
                        "Supply `{field_name}` at the construction site, give the field a default value, or widen its type to `{type_display} | Nil`"
                    ),
                )
            },
        );
    }

    /// Definite-assignment check at an Actor construction site (ADR 0124 §6
    /// Implementation A2b, BT-1948): `C spawn` or a literal-map `C
    /// spawnWith: #{...}` that leaves a declared, no-default, non-nilable,
    /// non-`late` state slot unassigned.
    ///
    /// Sibling to [`Self::check_spawn_with_map_keys`] (beside it, per
    /// BT-1948's acceptance criteria, rather than extending it in place —
    /// this predicate answers a different question over the same literal
    /// map and needs its own bare-`spawn` call-site shape
    /// `check_spawn_with_map_keys` never sees) and structurally mirrors
    /// [`Self::check_value_construction_definite_assignment`], reading two
    /// facts a Value's construction site has no equivalent of: A2a's
    /// composed [`ClassHierarchy::all_initialize_assigns`] summary
    /// (flattened parent-first over the ADR 0078 auto-chained `initialize`
    /// sequence — union, not intersection, is correct here because every
    /// ancestor's `initialize` is guaranteed to have run by the time the
    /// leaf's own body starts, so an ancestor's assignment is as good as
    /// the leaf's own) and the same summary's
    /// [`crate::semantic_analysis::class_hierarchy::InitializeAssignsSummary::has_dynamic_writer`]
    /// (this module's confidence signal for a `fieldAt:put:`/`perform:`
    /// write the must-analysis can't attribute to a slot at all, ADR 0124
    /// §6's Hint trigger — chain-wide, not leaf-only, since an ancestor's
    /// dynamic write is just as invisible to the must-analysis as the
    /// leaf's own). A slot the composed summary says is definitely assigned
    /// is treated exactly like a slot the literal map supplies — neither
    /// produces a diagnostic.
    ///
    /// Called from the same three class-side-send call sites as
    /// [`Self::check_spawn_with_map_keys`] and
    /// [`Self::check_value_construction_definite_assignment`]
    /// (`inference/send/receiver.rs`): a syntactic `C spawn`/`spawnWith:`, a
    /// `Meta`-typed receiver (`aClassVar spawn`), and a class method's own
    /// `self spawn` (spawning a sibling actor from a factory).
    ///
    /// Fires only when:
    /// - `selector` is `spawn` (arity 0) or `spawnWith:` (arity 1) and
    ///   `class_name` is an Actor subclass ([`ClassHierarchy::is_actor_subclass`])
    ///   known to the hierarchy with at least one declared `state:` slot —
    ///   the same guard [`Self::check_spawn_with_map_keys`] already applies,
    ///   inheriting its deliberate false-negative choice for an unresolved
    ///   receiver or empty `all_state` (`validation/:2382-2396`'s comment):
    ///   report nothing rather than risk a false positive.
    /// - For `spawnWith:`, the argument is a *literal* map — a non-literal
    ///   `spawnWith: someMap` is no evidence, reported nothing (same
    ///   boundary as [`Self::spawn_with_map_pairs`]). A `late` slot
    ///   supplied by key in that literal map counts as assigned exactly
    ///   like any other slot — `init/1` merges with the caller winning
    ///   (ADR 0124 §6), so no separate carve-out is needed here: `late`
    ///   slots are already excluded from `requires_definite_assignment_for_declared_type`
    ///   regardless of whether they're supplied.
    ///
    /// `spawn`/`spawnWith:` are Actor protocol constants (ADR 0104), never
    /// user-overridable the way a Value's `new`/`new:` is, so there is no
    /// override guard here analogous to
    /// [`Self::value_new_selector_is_overridden`].
    ///
    /// Severity mirrors [`Self::check_value_construction_definite_assignment`]:
    /// Warning when the composed summary's chain is fully known
    /// ([`crate::semantic_analysis::class_hierarchy::InitializeAssignsSummary::incomplete`]
    /// is `false`, which already covers both a `native:` ancestor and a
    /// missing/cross-file chain link), the surface is complete
    /// ([`ClassHierarchy::has_incomplete_surface_in_chain`]), and no
    /// dynamic-writer selector was found anywhere in the class; Hint
    /// otherwise (ADR 0124 §6's severity table).
    pub(in super::super) fn check_actor_construction_definite_assignment(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arguments: &[Expression],
        span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        if selector != "spawn" && selector != "spawnWith:" {
            return;
        }
        if class_name == "self" || class_name == "super" {
            return;
        }
        if !hierarchy.is_actor_subclass(class_name) {
            return;
        }
        let slots = hierarchy.all_state(class_name);
        if slots.is_empty() {
            return;
        }

        // For `spawnWith:`, only a literal map is evidence; bare `spawn`
        // supplies nothing. Mirrors `spawn_with_map_pairs`'s boundary.
        let Some(pairs) = Self::construction_evidence_pairs(selector, "spawnWith:", arguments)
        else {
            return;
        };

        let summary = hierarchy.all_initialize_assigns(class_name);
        let confident = !summary.incomplete
            && !hierarchy.has_incomplete_surface_in_chain(class_name)
            && !summary.has_dynamic_writer;

        self.report_unsupplied_slots(
            hierarchy,
            class_name,
            span,
            pairs,
            slots,
            |field_name| summary.assigned.contains(field_name),
            confident,
            |field_name, type_display| {
                (
                    format!(
                        "`{class_name}` declares `{field_name} :: {type_display}` with no default, and no `initialize` in its chain assigns it. `{class_name} {selector}` supplies no `{field_name}`, so it may raise `UninitializedStateError`"
                    ),
                    format!(
                        "Supply `{field_name}` at the construction site, give the field a default value, widen its type to `{type_display} | Nil`, or declare it `late`"
                    ),
                )
            },
        );
    }

    /// The map pairs that count as evidence at a construction site.
    ///
    /// For `keyed_selector` (`new:` / `spawnWith:`) only a *literal* map is
    /// evidence, so any other argument shape answers `None` and the caller
    /// reports nothing; for the bare selector (`new` / `spawn`) nothing is
    /// supplied, so the pairs are empty.
    fn construction_evidence_pairs<'a>(
        selector: &str,
        keyed_selector: &str,
        arguments: &'a [Expression],
    ) -> Option<&'a [crate::ast::MapPair]> {
        if selector != keyed_selector {
            return Some(&[]);
        }
        match arguments.first() {
            Some(Expression::MapLiteral { pairs, .. }) => Some(pairs),
            _ => None,
        }
    }

    /// The shared body of [`Self::check_value_construction_definite_assignment`]
    /// and [`Self::check_actor_construction_definite_assignment`]: emit a
    /// definite-assignment diagnostic for each of `slots` that the construction
    /// site neither supplies (a symbol key of `pairs`) nor `already_assigned`
    /// covers, and whose declared type is non-nilable with no default.
    ///
    /// `describe` builds the `(message, hint)` text from the field name and the
    /// declared type's display; `confident` picks Warning over Hint.
    #[allow(clippy::too_many_arguments)] // the two callers differ in exactly these inputs
    fn report_unsupplied_slots(
        &mut self,
        hierarchy: &ClassHierarchy,
        class_name: &EcoString,
        span: Span,
        pairs: &[crate::ast::MapPair],
        slots: Vec<EcoString>,
        already_assigned: impl Fn(&EcoString) -> bool,
        confident: bool,
        describe: impl Fn(&EcoString, &str) -> (String, String),
    ) {
        let supplied: std::collections::HashSet<&str> = pairs
            .iter()
            .filter_map(|pair| match &pair.key {
                Expression::Literal(Literal::Symbol(sym), _) => Some(sym.as_str()),
                _ => None,
            })
            .collect();

        let empty_registry = AliasRegistry::new();
        let aliases = self.alias_registry.as_deref().unwrap_or(&empty_registry);

        for field_name in slots {
            if supplied.contains(field_name.as_str()) || already_assigned(&field_name) {
                continue;
            }
            let Some(ty) = hierarchy.state_field_type(class_name, &field_name) else {
                continue; // Untyped — defaults to `nil`, no check (ADR 0124 §6).
            };
            let has_default = hierarchy.state_field_has_default(class_name, &field_name);
            let slot_kind = hierarchy.state_field_kind(class_name, &field_name);
            if !crate::semantic_analysis::requires_definite_assignment_for_declared_type(
                &ty,
                has_default,
                slot_kind,
                aliases,
            ) {
                continue;
            }
            let (message, hint) = describe(&field_name, &ty.to_string());
            let diagnostic = if confident {
                Diagnostic::warning(message, span)
            } else {
                Diagnostic::hint(message, span)
            }
            .with_hint(hint)
            .with_category(DiagnosticCategory::DefiniteAssignment);
            self.diagnostics.push(diagnostic);
        }
    }

    /// `true` if `class_name` or an ancestor **strictly between it and
    /// `Value`** defines its own class-side method named `selector` — i.e.
    /// `selector` does not resolve to the auto-generated Value constructor
    /// [`Self::check_value_construction_definite_assignment`] assumes.
    ///
    /// `Value` itself carries the real, sealed, source-backed `new`/`new:`
    /// methods every Value subclass inherits (`stdlib/src/value.bt`) — *that*
    /// definition **is** the auto-generated default this check assumes, not
    /// an override of it, so the ancestor walk stops there rather than
    /// treating every Value subclass as having an "overridden" constructor.
    /// A native Value subclass genuinely overriding `new:` (e.g.
    /// `stdlib/src/duration.bt:31`, `date_time.bt:41`) is caught by the walk
    /// stopping *before* `Value`, not after.
    fn value_new_selector_is_overridden(
        hierarchy: &ClassHierarchy,
        class_name: &str,
        selector: &str,
    ) -> bool {
        if hierarchy.has_own_class_method(class_name, selector) {
            return true;
        }
        // Walk the class's *strict* ancestors; each `info.superclass` is the
        // next name up the chain (it may name a class the map doesn't hold).
        hierarchy
            .ancestors(class_name)
            .map_while(|info| info.superclass.as_deref())
            .take_while(|ancestor| *ancestor != "Value")
            .any(|ancestor| hierarchy.has_own_class_method(ancestor, selector))
    }

    /// Check a `spawnWith:` literal value against a declared slot type
    /// (ADR 0104 Phase 2).
    ///
    /// The value's inferred type is read back from the type map (populated when
    /// the argument map literal was inferred), so the value expression is not
    /// re-inferred and its own diagnostics are not double-emitted. Union slot
    /// types (e.g. `Integer | #infinity`) are handled by [`Self::is_assignable_to`],
    /// which already splits the declared union string and checks membership.
    fn check_spawn_with_value(
        &mut self,
        class_name: &EcoString,
        slot_name: &EcoString,
        declared: &DeclaredType,
        value_expr: &Expression,
        hierarchy: &ClassHierarchy,
    ) {
        let Some(value_ty) = self.type_map.get(value_expr.span()).cloned() else {
            return; // Dynamic / unknown value type — skip conservatively.
        };
        // ADR 0147: a shape on either side is judged structurally.
        let declared_resolved = self
            .resolve_declared_recording_aliases(declared, &type_resolver::SubstitutionMap::new());
        if self.check_shape_slot(
            &value_ty,
            &declared_resolved,
            hierarchy,
            value_expr.span(),
            Some(value_expr),
            |value, declared_display| {
                format!(
                    "type mismatch for state key `{slot_name}` on `{class_name}`: declared {declared_display}, got {value}"
                )
            },
        ) {
            return;
        }
        let declared_ty: &EcoString = &declared.to_string().into();
        // `declared_ty` is a bare `EcoString` pulled from
        // `ClassHierarchy::state_field_type` — the same pre-ADR-0108
        // storage `check_field_assignment` sits on. Re-resolve through
        // `AliasRegistry::resolve_display_name` when it names a registered
        // alias, mirroring that sibling check.
        let declared_display = self
            .alias_registry
            .as_ref()
            .and_then(|registry| registry.resolve_display_name(declared_ty))
            .unwrap_or_else(|| InferredType::class_name_for_diagnostic(declared_ty.as_str()));
        // Mirror `check_field_assignment`'s structural resolution —
        // `declared_display` above only fixes the diagnostic text (display-only
        // scope); `is_assignable_to` below still needs the
        // alias's structural expansion, not the opaque unresolved name, or a
        // valid `spawnWith:` slot value falsely fails the check.
        let (declared_structural, alias_deps) =
            Self::resolve_alias_structural(declared_ty, self.alias_registry.as_deref());
        self.referenced_aliases.extend(alias_deps);
        match &value_ty {
            InferredType::Known {
                class_name: value_type,
                ..
            } if !Self::is_assignable_to(value_type, &declared_structural, hierarchy) => {
                let value_display = InferredType::class_name_for_diagnostic(value_type.as_str());
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "type mismatch for state key `{slot_name}` on `{class_name}`: declared {declared_display}, got {value_display}"
                        ),
                        value_expr.span(),
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!(
                        "Expected {declared_display} for `{slot_name}`, got {value_display}"
                    )),
                );
            }
            InferredType::Union { members, .. } => {
                let Some((compat, total, incompatible)) =
                    Self::classify_union_members(members, |m| {
                        Self::is_assignable_to(m, &declared_structural, hierarchy)
                    })
                else {
                    return; // Contains Dynamic — skip.
                };
                if compat == total {
                    return;
                }
                let union_display = value_ty
                    .display_for_diagnostic()
                    .unwrap_or_else(|| EcoString::from("Dynamic"));
                let list = incompatible
                    .iter()
                    .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                    .collect::<Vec<_>>()
                    .join(", ");
                // Mirror `check_field_assignment_type`: when no member is
                // compatible, say so plainly rather than "some members".
                let hint = if compat == 0 {
                    format!("No member of {union_display} is compatible with {declared_display}")
                } else {
                    format!(
                        "Some members of {union_display} are not compatible with {declared_display}: {list}"
                    )
                };
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "type mismatch for state key `{slot_name}` on `{class_name}`: declared {declared_display}, got {union_display}"
                        ),
                        value_expr.span(),
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(hint),
                );
            }
            // Dynamic / Never / Meta / Negation / Intersection value — skip.
            _ => {}
        }
    }

    /// Find the declared state slot closest (by edit distance) to `target`, for
    /// a "did you mean" typo suggestion. Mirrors [`Self::find_similar_selector`]'s
    /// thresholds so suggestions stay conservative.
    fn closest_state_slot(target: &str, slots: &[EcoString]) -> Option<EcoString> {
        let mut best: Option<(EcoString, usize)> = None;
        for slot in slots {
            let dist = edit_distance(target, slot.as_str());
            // `dist > 0` documents the invariant that exact matches are already
            // filtered by the caller (`continue`d before this is called) — and
            // keeps the function correct if ever reused without that pre-filter.
            if dist > 0
                && dist <= 3
                && dist < target.len() / 2 + 1
                && best.as_ref().is_none_or(|(_, d)| dist < *d)
            {
                best = Some((slot.clone(), dist));
            }
        }
        best.map(|(slot, _)| slot)
    }

    /// Check state default values match declared types at class definition time.
    #[allow(clippy::too_many_lines)] // ADR 0108 alias-aware resolution of the declared type
    pub(in super::super) fn check_state_defaults(
        &mut self,
        class: &crate::ast::ClassDefinition,
        hierarchy: &ClassHierarchy,
    ) {
        // Collect type parameter names once — we can't validate concrete
        // assignability for generic fields at class definition time.
        let type_param_names: Vec<&str> = class
            .type_params
            .iter()
            .map(|tp| tp.name.name.as_str())
            .collect();
        for decl in &class.state {
            let Some(ref type_annotation) = decl.type_annotation else {
                continue;
            };
            if decl.default_value.is_none() {
                continue;
            }
            // ADR 0108: resolve the declared type through the alias
            // table before comparing against the default value's inferred
            // type — mirroring `check_method_return_type`'s existing
            // alias-aware resolution — so `field: restart :: RestartStrategy
            // = #temporary` checks the default against the alias's
            // expansion, not its opaque unresolved name. A plain nominal
            // name (including a generic type-parameter name like `T`, which
            // never resolves via `subst`/the alias table since this call
            // site passes an empty `SubstitutionMap`) round-trips unchanged,
            // so the `type_param_names` check below is unaffected.
            let (resolved_declared, alias_deps) =
                type_resolver::resolve_type_annotation_with_alias_deps(
                    type_annotation,
                    &type_resolver::SubstitutionMap::new(),
                    self.protocol_registry.as_deref(),
                    self.alias_registry.as_deref(),
                );
            self.referenced_aliases.extend(alias_deps);
            // BT-4032: an explicit `:: Dynamic` annotation opts the slot
            // out of checking (ADR 0077) — accept any default.
            // `is_assignable_to` below has no unknown-class escape hatch
            // (unlike `is_type_compatible`), so without this skip a
            // declared `Dynamic` rejects every `Known` literal default.
            // Mirrors the `InferredType::Dynamic(_) => true`
            // expected-type arm in `known_type_compatible_with_expected`
            // (return-type checking), which treats a declared `Dynamic`
            // the same way.
            if matches!(resolved_declared, InferredType::Dynamic(_)) {
                continue;
            }
            let declared_type = resolved_declared.display_annotation();
            if type_param_names.contains(&declared_type.as_str()) {
                continue;
            }
            // `resolved_declared` already carries a
            // `TypeProvenance::Aliased` tag when `type_annotation` names a
            // registered alias (see the doc above) — `display_for_diagnostic`
            // renders it as `AliasName (expansion)` directly. Computed once
            // here, before the tag is discarded by `display_annotation`'s
            // flattening above (needed for `is_assignable_to`'s structural
            // comparison), and reused by every arm below instead of
            // re-deriving a plain display from the flattened `declared_type`.
            let declared_display = resolved_declared
                .display_for_diagnostic()
                .unwrap_or_else(|| EcoString::from("Dynamic"));
            // Read the pre-computed type instead of re-inferring: `inference/
            // mod.rs::check_module` already inferred every state field's
            // default-value expression (BT-3481) and cached it in
            // `state_default_types` keyed by (class, field) — looking it up
            // here keeps the module dependency direction one-way
            // (`inference` → `validation`, never back).
            let Some(inferred) = self
                .state_default_types
                .get(&(class.name.name.clone(), decl.name.name.clone()))
                .cloned()
            else {
                continue;
            };
            // ADR 0147: shapes are judged structurally.
            if self.check_shape_slot(
                &inferred,
                &resolved_declared,
                hierarchy,
                decl.span,
                decl.default_value.as_ref(),
                |value, declared| {
                    format!(
                        "Type mismatch: state `{}` declared as {declared}, default is {value}",
                        decl.name.name
                    )
                },
            ) {
                continue;
            }
            match &inferred {
                InferredType::Known {
                    class_name: value_type,
                    ..
                } if !Self::slot_accepts(
                    &inferred,
                    value_type,
                    &resolved_declared,
                    &declared_type,
                    hierarchy,
                    self.protocol_registry.as_deref(),
                ) =>
                {
                    let declared_display = declared_display.clone();
                    // Render `UndefinedObject` as `Nil` in user-facing messages.
                    let value_display =
                        InferredType::class_name_for_diagnostic(value_type.as_str());
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Type mismatch: state `{}` declared as {declared_display}, default is {value_display}",
                                decl.name.name
                            ),
                            decl.span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!(
                            "Default value type {value_display} is not compatible with {declared_display}"
                        )),
                    );
                }
                InferredType::Union { members, .. } => {
                    // BT-4454: class-object members are judged by the slot
                    // rule, as a bare class object is below.
                    if let Some(diag) = self.union_class_object_diagnostic(
                        members,
                        &declared_type,
                        hierarchy,
                        Self::is_assignable_to,
                        decl.span,
                        |class_object| {
                            (
                                format!(
                                    "Type mismatch: state `{}` declared as {declared_display}, default is {class_object}",
                                    decl.name.name
                                ),
                                format!(
                                    "Default value type {class_object} is not compatible with {declared_display}"
                                ),
                            )
                        },
                    ) {
                        self.diagnostics.push(diag);
                        continue;
                    }
                    // Check the other union members against the declared field
                    // type, mirroring `check_field_assignment`'s handling.
                    let instance_members = Self::non_class_object_members(members);
                    let Some((compat, total, incompatible)) =
                        Self::classify_union_members_with(&instance_members, |member, name| {
                            Self::slot_accepts(
                                member,
                                name,
                                &resolved_declared,
                                &declared_type,
                                hierarchy,
                                self.protocol_registry.as_deref(),
                            )
                        })
                    else {
                        continue; // Contains Dynamic — skip
                    };
                    if compat == total {
                        continue; // All members compatible
                    }
                    // Accepted class objects are compatible members too.
                    let compat = compat + (members.len() - instance_members.len());
                    let union_display = inferred
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let declared_display = declared_display.clone();
                    let diag = if compat == 0 {
                        Diagnostic::warning(
                            format!(
                                "Type mismatch: state `{}` declared as {declared_display}, default is {union_display}",
                                decl.name.name
                            ),
                            decl.span,
                        )
                        .with_hint(format!(
                            "No member of {union_display} is compatible with {declared_display}"
                        ))
                    } else {
                        let list = incompatible
                            .iter()
                            .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        Diagnostic::hint(
                            format!(
                                "Type mismatch: state `{}` declared as {declared_display}, default is {union_display}",
                                decl.name.name
                            ),
                            decl.span,
                        )
                        .with_hint(format!(
                            "Some members of the union are not compatible with {declared_display}: {list}"
                        ))
                    };
                    self.diagnostics
                        .push(diag.with_category(DiagnosticCategory::Type));
                }
                // A class object as the default (`state: k :: Foo class =
                // Bar`): the slot rule `check_field_assignment` applies
                // (BT-4206), including ADR 0144 protocol metatypes.
                InferredType::Meta { .. } | InferredType::ProtocolMeta { .. } => {
                    let Some(class_object) = ClassObjectType::of(&inferred) else {
                        continue;
                    };
                    if Self::metatype_assignable_to(
                        class_object,
                        &declared_type,
                        hierarchy,
                        self.protocol_registry.as_deref(),
                        Self::is_assignable_to,
                    ) {
                        continue;
                    }
                    let diag = Self::metatype_mismatch_diagnostic(
                        class_object,
                        &declared_type,
                        hierarchy,
                        self.protocol_registry.as_deref(),
                        format!(
                            "Type mismatch: state `{}` declared as {declared_display}, default is {class_object}",
                            decl.name.name
                        ),
                        format!(
                            "Default value type {class_object} is not compatible with {declared_display}"
                        ),
                        decl.span,
                    );
                    self.diagnostics.push(diag);
                }
                // Dynamic / Never / Negation / Intersection defaults — skip
                // conservatively, matching `check_field_assignment`.
                _ => {}
            }
        }
    }
}
