// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Protocol, union and metatype argument conformance, and the union-member classification they share.
//!
//! **DDD Context:** Semantic Analysis

//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::{ProtocolRegistry, ReturnTypeMismatch};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

use super::super::type_resolver;
use super::super::types::ClassObjectType;
use super::super::{InferredType, TypeChecker};

/// Outcome of walking a `Union`-typed argument's members against one
/// expected simple type, via [`TypeChecker::classify_union_members`] +
/// [`TypeChecker::is_type_compatible`].
///
/// Shared by [`TypeChecker::check_argument_types`] and
/// [`TypeChecker::check_ffi_argument_types`] (`inference.rs`), which both
/// need the same "skip if unclassifiable, skip if every member is
/// compatible, otherwise report the incompatible members" decision from a
/// raw `classify_union_members` call, kept as one implementation rather
/// than two independently re-derived copies (AGENTS.md § No duplicate
/// implementations). Diagnostic
/// *message* text still differs per call site (an FFI signature vs. a
/// declared Beamtalk method), so only the classification — not the
/// diagnostic construction — is shared here.
pub(in super::super) enum UnionArgCompat {
    /// Every member is compatible with the expected type — no diagnostic.
    AllCompatible,
    /// The union contains a member `classify_union_members` can't reason
    /// about conservatively (Dynamic/Union/Meta/Never) — skip.
    Unclassifiable,
    /// `compat` members were compatible; `incompatible` names the rest, for
    /// the diagnostic hint text. `compat == 0` distinguishes "no member
    /// compatible" (a `warning`) from "some members compatible" (a `hint`)
    /// at both call sites.
    SomeIncompatible {
        compat: usize,
        incompatible: Vec<EcoString>,
    },
}

impl TypeChecker {
    /// Classify how well a union's known members match a predicate.
    ///
    /// Returns `None` if any member is Dynamic (conservative skip).
    /// Otherwise returns the count of members that satisfy `pred` and the total,
    /// plus the rendered names of the members that did *not* satisfy it.
    ///
    /// The predicate receives each member's *full* type string
    /// (via [`InferredType::display_annotation`], e.g. `"Array(Integer)"`), not the bare
    /// `as_known()` class name: collection literals carry element types, so a
    /// union like `Array(Integer) | Array(String)` can arise, and dropping the type
    /// args would let `Array(String)` silently pass a check against a declared
    /// `Array(Integer)`. The `as_known()` guard is still used solely to skip the
    /// whole union when any member is `Dynamic`/`Union`/`Meta`/`Never`.
    pub(in super::super) fn classify_union_members<F>(
        members: &[InferredType],
        pred: F,
    ) -> Option<(usize, usize, Vec<EcoString>)>
    where
        F: Fn(&EcoString) -> bool,
    {
        Self::classify_union_members_with(members, |_, name| pred(name))
    }

    /// [`Self::classify_union_members`] with a predicate that also receives
    /// each member's own type (BT-4607: a protocol-typed state slot judges a
    /// member by structural conformance, which needs the type, not its name).
    pub(in super::super) fn classify_union_members_with<F>(
        members: &[InferredType],
        pred: F,
    ) -> Option<(usize, usize, Vec<EcoString>)>
    where
        F: Fn(&InferredType, &EcoString) -> bool,
    {
        // Skip the whole union if any member is non-`Known` (Dynamic/Union/Meta/
        // Never) — we can't reason about those conservatively.
        if members.iter().any(|m| m.as_known().is_none()) {
            return None;
        }
        let mut compatible = 0;
        let mut incompatible = Vec::new();
        for member in members {
            let name = member.display_annotation();
            if pred(member, &name) {
                compatible += 1;
            } else {
                incompatible.push(name);
            }
        }
        Some((compatible, members.len(), incompatible))
    }

    /// Classifies a `Union` argument's members against `expected`, per
    /// [`UnionArgCompat`].
    pub(in super::super) fn classify_union_arg_compat(
        members: &[InferredType],
        expected: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> UnionArgCompat {
        let Some((compat, total, incompatible)) = Self::classify_union_members(members, |m| {
            Self::is_type_compatible(m, expected, hierarchy)
        }) else {
            return UnionArgCompat::Unclassifiable;
        };
        if compat == total {
            UnionArgCompat::AllCompatible
        } else {
            UnionArgCompat::SomeIncompatible {
                compat,
                incompatible,
            }
        }
    }

    /// BT-4454: the first class-object member (`C class` / `P class`) of a
    /// union value that [`Self::metatype_assignable_to`] refuses for
    /// `declared_type`. The one rule for judging class objects inside a
    /// union, shared by [`Self::detect_assignment_type_mismatch`] (annotated
    /// locals) and [`Self::union_class_object_diagnostic`] (arguments, fields,
    /// state defaults, return bodies); `tower` is the caller's own tower
    /// predicate, as for a bare class object at that site.
    pub(in super::super) fn refused_class_object<'a>(
        members: &'a [InferredType],
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&ProtocolRegistry>,
        tower: fn(&EcoString, &EcoString, &ClassHierarchy) -> bool,
    ) -> Option<ClassObjectType<'a>> {
        members
            .iter()
            .filter_map(ClassObjectType::of)
            .find(|class_object| {
                !Self::metatype_assignable_to(
                    *class_object,
                    declared_type,
                    hierarchy,
                    protocol_registry,
                    tower,
                )
            })
    }

    /// BT-4454: a union value's class-object members are judged by
    /// [`Self::refused_class_object`]; when one is refused, the diagnostic
    /// for it (ADR 0144 conformance wording where it applies, else the
    /// site's own `message`/`hint`, which receive the refused class object).
    /// Like an annotated local, a refused class object warns even when other
    /// members fit. `None` when every class-object member is accepted (or
    /// there is none); the caller then checks the remaining members with
    /// [`Self::non_class_object_members`] as before.
    pub(in super::super) fn union_class_object_diagnostic(
        &self,
        members: &[InferredType],
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
        tower: fn(&EcoString, &EcoString, &ClassHierarchy) -> bool,
        span: Span,
        describe: impl FnOnce(ClassObjectType<'_>) -> (String, String),
    ) -> Option<Diagnostic> {
        let refused = Self::refused_class_object(
            members,
            declared_type,
            hierarchy,
            self.protocol_registry.as_deref(),
            tower,
        )?;
        let (message, hint) = describe(refused);
        Some(Self::metatype_mismatch_diagnostic(
            refused,
            declared_type,
            hierarchy,
            self.protocol_registry.as_deref(),
            message,
            hint,
            span,
        ))
    }

    /// The members of a union value that are not class objects — what the
    /// pre-BT-4454 checks (`classify_union_members`) still judge once the
    /// class objects have been accepted by [`Self::refused_class_object`].
    pub(in super::super) fn non_class_object_members(
        members: &[InferredType],
    ) -> Vec<InferredType> {
        members
            .iter()
            .filter(|m| ClassObjectType::of(m).is_none())
            .cloned()
            .collect()
    }

    /// Collect deduplicated missing protocol methods across union members.
    pub(super) fn collect_missing_protocol_methods(
        members: &[InferredType],
        protocol: &str,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) -> String {
        let mut all_missing: Vec<EcoString> = Vec::new();
        for member in members {
            if let Some(name) = member.as_known() {
                if let Err(missing) = protocol_registry.check_conformance(name, protocol, hierarchy)
                {
                    for m in &missing {
                        if !all_missing.iter().any(|existing| existing == m) {
                            all_missing.push(m.clone());
                        }
                    }
                }
            }
        }
        Self::format_selector_list(&all_missing)
    }

    /// Check protocol conformance for a call-site argument.
    ///
    /// When a parameter type is a protocol name and the argument type is a known
    /// class, checks structural conformance. Emits a warning if the class does
    /// not conform to the protocol.
    ///
    /// **Metatype arguments:** a class-object argument `Meta{C}`
    /// (ADR 0083) satisfies a protocol-typed parameter `:: P` iff the *class
    /// side* of `C` conforms to `P` — see
    /// [`ProtocolRegistry::check_class_side_conformance`] for the full rule.
    /// This composes with the metaclass-tower subtyping
    /// `Meta{C} <: Class <: Behaviour <: Object`: parameters declared
    /// `:: Class` / `:: Behaviour` / `:: Object` are *classes*, not protocols,
    /// so they never reach this function — `check_argument_types` accepts any
    /// metatype for them via nominal subtyping. Only genuine protocol
    /// annotations trigger the structural class-side check. Unknown classes
    /// stay silent (open-world gating inside the registry check).
    ///
    /// **References:** ADR 0068 Phase 2b — "Type checker: protocol name in type
    /// annotation → structural conformance check"
    pub(in super::super) fn check_protocol_argument_conformance(
        &mut self,
        arg_type: &InferredType,
        expected_protocol: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        // Extract base protocol name for generic protocols (e.g., "Enumerable(T)" → "Enumerable")
        let base_protocol = type_resolver::base_name_of_string(expected_protocol);

        match arg_type {
            InferredType::Known { class_name, .. } => {
                let Some(_protocol) = protocol_registry.get(base_protocol) else {
                    return; // Not a protocol — handled by normal type checking
                };

                match protocol_registry.check_conformance(class_name, base_protocol, hierarchy) {
                    // Conforms by selector — still check the declared
                    // return types refine the protocol's (BT-4175).
                    Ok(()) => {
                        for mismatch in protocol_registry.conformance_return_type_mismatches(
                            class_name,
                            base_protocol,
                            hierarchy,
                        ) {
                            let ReturnTypeMismatch {
                                selector,
                                class_side,
                                defined_in,
                                actual,
                                expected,
                            } = mismatch;
                            let side = if class_side { " class" } else { "" };
                            self.diagnostics.push(
                                Diagnostic::warning(
                                    format!(
                                        "{class_name}{side}>>{selector} returns {actual}, incompatible with protocol {base_protocol}'s {expected}"
                                    ),
                                    span,
                                )
                                .with_category(DiagnosticCategory::Type)
                                .with_hint(format!(
                                    "{class_name} responds to every {base_protocol} selector, but `{selector}` (defined in {defined_in}{side}) is declared -> {actual}; code typed `:: {base_protocol}` expects {expected}"
                                )),
                            );
                        }
                    }
                    Err(missing) => {
                        let missing_list = Self::format_selector_list(&missing);
                        self.diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "{class_name} does not conform to protocol {base_protocol}"
                                ),
                                span,
                            )
                            .with_category(DiagnosticCategory::Type)
                            .with_hint(format!("Missing required method(s): {missing_list}")),
                        );
                    }
                }
            }
            InferredType::Dynamic(_) => {
                if protocol_registry.has_protocol(base_protocol) {
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!("Cannot verify {base_protocol} conformance for Dynamic value"),
                            span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint("Add a type annotation to enable protocol conformance checking"),
                    );
                }
            }
            InferredType::Union { members, .. } => {
                self.check_union_argument_conformance(
                    arg_type,
                    members,
                    base_protocol,
                    span,
                    hierarchy,
                    protocol_registry,
                );
            }
            // ADR 0083: a class-object argument `Meta{C}` satisfies
            // `:: P` iff the *class side* of `C` conforms to `P` (the callee
            // will send `P`'s required selectors to the class object, which
            // dispatch to `C`'s class methods or the metaclass tower — see
            // `check_class_side_conformance`). Unknown classes / cross-file
            // parents / class-side DNU overrides stay silent inside the
            // registry check (open-world gating).
            InferredType::Meta { class_name, .. } => {
                self.check_meta_argument_conformance(
                    class_name,
                    base_protocol,
                    span,
                    hierarchy,
                    protocol_registry,
                );
            }
            // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip like
            // `Never` (ADR 0102). An `Intersection` (`P1 & P2`, ADR
            // 0102) as the *argument's own* inferred type is a rarer,
            // deeper case (the value's declared type is itself a stored
            // intersection) — skip conservatively rather than guess which
            // member to check; the flagship "parameter declared `P1 & P2`"
            // case is handled by splitting `expected_protocol` in
            // `check_protocol_conformance_in_expr`, not here.
            //
            // A `P class` argument (ADR 0144) is the class object of *some*
            // conformer of `P`; which class it is — and so whether its class
            // side answers `Q`'s instance-side selectors — is unknown here,
            // so skip conservatively (open world, ADR 0100).
            //
            // A map shape (ADR 0147) is a dictionary; no user protocol is
            // satisfied structurally by it, so there is nothing to check.
            InferredType::Never
            | InferredType::ProtocolMeta { .. }
            | InferredType::Negation { .. }
            | InferredType::Intersection { .. }
            | InferredType::Shape { .. } => {}
        }
    }

    /// Check protocol conformance for a union-typed call-site argument —
    /// the `Union` arm of
    /// [`check_protocol_argument_conformance`](Self::check_protocol_argument_conformance).
    ///
    /// All members conforming → pass; none → warning listing missing methods
    /// across members; some → hint listing the non-conforming members.
    /// Unions containing `Dynamic` are skipped.
    fn check_union_argument_conformance(
        &mut self,
        arg_type: &InferredType,
        members: &[InferredType],
        base_protocol: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        if protocol_registry.get(base_protocol).is_none() {
            return; // Not a protocol — handled by normal type checking
        }
        let Some((compat, total, non_conforming)) = Self::classify_union_members(members, |m| {
            // Members carry type args (e.g. `Array(Integer)`);
            // conformance is a property of the base class, so strip them.
            let base = type_resolver::base_name_of_string(m);
            protocol_registry
                .check_conformance(base, base_protocol, hierarchy)
                .is_ok()
        }) else {
            return; // Contains Dynamic — skip
        };
        if compat == total {
            return; // All conform → pass
        }
        // Render `UndefinedObject` as `Nil` in user-facing messages.
        let union_display = arg_type
            .display_for_diagnostic()
            .unwrap_or_else(|| EcoString::from("Dynamic"));
        let diag = if compat == 0 {
            // Collect missing methods across all members
            let all_missing = Self::collect_missing_protocol_methods(
                members,
                base_protocol,
                hierarchy,
                protocol_registry,
            );
            Diagnostic::warning(
                format!("{union_display} does not conform to protocol {base_protocol}"),
                span,
            )
            .with_hint(format!(
                "No member conforms — missing required method(s): {all_missing}"
            ))
        } else {
            let list = non_conforming
                .iter()
                .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                .collect::<Vec<_>>()
                .join(", ");
            Diagnostic::hint(
                format!("Not all members of {union_display} conform to protocol {base_protocol}"),
                span,
            )
            .with_hint(format!("Non-conforming member(s): {list}"))
        };
        self.diagnostics
            .push(diag.with_category(DiagnosticCategory::Type));
    }

    /// Check class-side protocol conformance for a metatype (`Meta{C}`)
    /// call-site argument — the `Meta` arm of
    /// [`check_protocol_argument_conformance`](Self::check_protocol_argument_conformance).
    ///
    /// Emits a warning naming the *class side* (`C class`) when it does not
    /// conform to `base_protocol` per
    /// [`ProtocolRegistry::check_class_side_conformance`].
    fn check_meta_argument_conformance(
        &mut self,
        class_name: &EcoString,
        base_protocol: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        if protocol_registry.get(base_protocol).is_none() {
            return; // Not a protocol — handled by normal type checking
        }
        match protocol_registry.check_class_side_conformance(class_name, base_protocol, hierarchy) {
            Ok(()) => {} // Class side conforms — no warning
            Err(missing) => {
                let missing_list = Self::format_selector_list(&missing);
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!("{class_name} class does not conform to protocol {base_protocol}"),
                        span,
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!(
                        "Missing required class-side method(s): {missing_list}"
                    )),
                );
            }
        }
    }

    /// Format a slice of selector names as a comma-separated list of
    /// single-quoted atoms for use in diagnostic hint strings.
    pub(super) fn format_selector_list(selectors: &[EcoString]) -> String {
        selectors
            .iter()
            .map(|s| format!("'{s}'"))
            .collect::<Vec<_>>()
            .join(", ")
    }
}
