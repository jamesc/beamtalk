// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Return-type checking of method bodies against the declared return type.
//!
//! **DDD Context:** Semantic Analysis

//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

use crate::ast::{Expression, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use ecow::EcoString;

use super::super::type_resolver;
use super::super::types::ClassObjectType;
use super::super::{DynamicReason, InferredType, TypeChecker};

impl TypeChecker {
    /// Check that a method body's inferred return type matches its declared return type.
    #[allow(clippy::too_many_lines)] // includes the type_args comparison arm
    pub(in super::super) fn check_return_type(
        &mut self,
        method: &crate::ast::MethodDefinition,
        body_type: &InferredType,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
    ) {
        // Skip primitive methods
        if method
            .body
            .iter()
            .any(|s| matches!(s.expression, Expression::Primitive { .. }))
        {
            return;
        }

        // BT-4090: a method that defers its reply (`self deferReply`) returns
        // a filler that is never delivered — the caller is answered later by
        // `ReplyTo>>reply:` — so its declared return type describes that later
        // reply, not the body. Skip syntactically; a conditional deferral also
        // skips the check for the non-deferred path (documented caveat).
        if method
            .body
            .iter()
            .any(|s| Self::contains_defer_reply_send(&s.expression))
        {
            return;
        }

        let Some(ref declared) = method.return_type else {
            return;
        };

        // Resolve the declared return type up front — every `body_type` arm
        // below (`Known`, `Union` [Part A], `Dynamic` [Part
        // B]) needs it. The `-> Self` case needs the static receiver class
        // (which the plain annotation resolver does not thread), so handle it
        // specially via `receiver_type_for_class`. `-> Self class` returns a
        // class object whose class depends on the receiver — skip. A
        // `-> C class` / `-> P class` return resolves like any annotation;
        // a class-object body is then judged by `metatype_assignable_to`
        // below (BT-4440), and any other body against it is skipped.
        //
        // All other arms — including `Generic` — go through the
        // central `resolve_type_annotation` resolver. ADR 0108:
        // thread the alias registry (but not the protocol registry — this
        // mirrors `Self::resolve_type_annotation`'s existing, unchanged
        // scope) so a declared `-> RestartStrategy` return type expands to
        // its structural union instead of an opaque unknown class.
        let expected = match declared {
            TypeAnnotation::SelfType { .. } => {
                type_resolver::receiver_type_for_class(class_name, hierarchy)
            }
            TypeAnnotation::SelfClass { .. } => return,
            _ => {
                // ADR 0108 hot-reload re-check trigger: a declared
                // `-> RestartStrategy` return type is an annotation site too
                // — record its (transitive) alias deps the same as every
                // other resolved annotation.
                let (expected, deps) = type_resolver::resolve_type_annotation_with_alias_deps(
                    declared,
                    &type_resolver::SubstitutionMap::new(),
                    None,
                    self.alias_registry.as_deref(),
                );
                self.referenced_aliases.extend(deps);
                expected
            }
        };

        // BT-4440: a class-object body (`C class` / `P class`) meets the
        // declared return type under the same `metatype_assignable_to` rule
        // arguments, slots and locals use (ADR 0083 tower, ADR 0144
        // conformance). `-> Self` depends on the receiver side, so skip it.
        if let Some(class_object) = ClassObjectType::of(body_type) {
            if !matches!(declared, TypeAnnotation::SelfType { .. }) {
                self.check_class_object_return(
                    method,
                    class_object,
                    &expected,
                    class_name,
                    hierarchy,
                );
            }
            return;
        }
        // Any other body against a declared class-object return has no
        // reliable comparison here — skip, as before BT-4440.
        if matches!(declared, TypeAnnotation::ClassOf { .. }) {
            return;
        }

        // ADR 0147: a shape in the body or the declared return type is judged
        // structurally. A `Dynamic` body keeps its dedicated hint path.
        if !matches!(body_type, InferredType::Dynamic(_))
            && (expected.contains_shape() || body_type.contains_shape())
        {
            if let Err(mismatch) = Self::shape_assignable(body_type, &expected, hierarchy) {
                let show = |t: &InferredType| {
                    t.display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"))
                };
                let selector = method.selector.name();
                let diag = Self::shape_mismatch_diagnostic(
                    format!(
                        "Method '{selector}' in {class_name} declares return type {}, but body returns {}",
                        show(&expected),
                        show(body_type)
                    ),
                    &mismatch,
                    &expected,
                    method.span,
                );
                self.diagnostics.push(diag);
            }
            return;
        }

        let InferredType::Known {
            class_name: actual_ty,
            type_args: actual_args,
            ..
        } = body_type
        else {
            // `Union` and `Dynamic` bodies must not bail out of return-type
            // checking entirely here, even against a concrete declared return
            // type — silently skipping them would hide real return-type
            // mismatches. Route those through dedicated checks (Parts A and B);
            // `Never`/`Meta`/`Negation`/`Intersection` bodies still can't be
            // reliably compared against `expected` here, so they keep bailing
            // silently as before.
            match body_type {
                InferredType::Union { members, .. } => {
                    self.check_union_body_return_type(
                        method, body_type, members, &expected, class_name, hierarchy,
                    );
                }
                InferredType::Dynamic(reason) => {
                    self.check_dynamic_body_return_hint(
                        method, *reason, &expected, class_name, hierarchy,
                    );
                }
                // `Known` is unreachable here at runtime (the `let`-`else`
                // above already matched it out) — listed only because the
                // compiler can't narrow `body_type`'s type in this arm.
                InferredType::Never
                | InferredType::Meta { .. }
                | InferredType::ProtocolMeta { .. }
                | InferredType::Negation { .. }
                | InferredType::Intersection { .. }
                // A shape body is judged before this match (ADR 0147).
                | InferredType::Shape { .. }
                | InferredType::Known { .. } => {}
            }
            return;
        };

        // The resolver preserves type_args, and the comparison
        // below checks them when both sides carry generic arguments, so
        // `-> Result(Integer, Error)` with body `Result(String, Error)`
        // produces a warning.
        match &expected {
            InferredType::Known {
                class_name: expected_ty,
                type_args: expected_args,
                ..
            } => {
                // Check base class compatibility, then inner type args.
                let base_mismatch = !Self::is_type_compatible(actual_ty, expected_ty, hierarchy);
                // When the base class matches, also verify inner type args
                // match. `same_base_type_args_compatible` recurses into nested generics so
                // mismatches like `Result(Array(Integer), Error)` vs
                // `Result(Array(String), Error)` are caught (Copilot review on
                // PR #2059), and rejects a positional-tuple arity mismatch
                // (`-> Tuple(A, B)` with body `Tuple(A, B, C)`, BT-4220).
                let args_mismatch = !base_mismatch
                    && !Self::same_base_type_args_compatible(
                        expected_ty,
                        expected_args,
                        actual_args,
                        hierarchy,
                    );
                if base_mismatch || args_mismatch {
                    let expected_display = expected.display_for_diagnostic().unwrap_or_else(|| {
                        InferredType::class_name_for_diagnostic(expected_ty.as_str())
                    });
                    let actual_display = body_type.display_for_diagnostic().unwrap_or_else(|| {
                        InferredType::class_name_for_diagnostic(actual_ty.as_str())
                    });
                    let selector = method.selector.name();
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Method '{selector}' in {class_name} declares return type {expected_display}, but body returns {actual_display}"
                            ),
                            method.span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!("Declared -> {expected_display}, inferred body type is {actual_display}")),
                    );
                }
            }
            InferredType::Union { members, .. } => {
                // Body type must be compatible with at least one union member.
                // Delegates to `known_type_compatible_with_union_member`,
                // which also compares generic type args — otherwise a body like
                // `Result(Integer, Error)` is wrongly treated as compatible with
                // a declared `Result(String, Error) | Nil` just because the
                // `Result` base class matches.
                let compatible = members.iter().any(|member| {
                    Self::known_type_compatible_with_union_member(
                        actual_ty,
                        actual_args,
                        member,
                        hierarchy,
                    )
                });
                if !compatible {
                    let selector = method.selector.name();
                    let union_display = expected
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let actual_display =
                        InferredType::class_name_for_diagnostic(actual_ty.as_str());
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Method '{selector}' in {class_name} declares return type {union_display}, but body returns {actual_display}"
                            ),
                            method.span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!("Declared -> {union_display}, inferred body type is {actual_display}")),
                    );
                }
            }
            InferredType::Never => {
                // Declared `-> Never` means the method is divergent
                // (never returns normally). The body must also be Never for the
                // declaration to be honest. Any `Known` body reaching this arm
                // means the method *does* return a value — warn.
                //
                // Note: `Never` body-types cause the early return on line ~753
                // before this match, so we only reach this arm when the body
                // has a non-Never `Known` type (the destructure above extracted
                // `actual_ty`).
                let selector = method.selector.name();
                let actual_display = body_type
                    .display_for_diagnostic()
                    .unwrap_or_else(|| InferredType::class_name_for_diagnostic(actual_ty.as_str()));
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Method '{selector}' in {class_name} declares return type Never, but body returns {actual_display}"
                        ),
                        method.span,
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!(
                        "`Never` means the method is divergent (throws or loops). Either remove `-> Never` or ensure the body always diverges (e.g. ends with `self error: ...`). Inferred body type is {actual_display}"
                    )),
                );
            }
            // ADR 0083: a declared metatype return (`-> Self class` / `-> X class`)
            // already returned above (a class-object body is judged by
            // `check_class_object_return`), so `Meta` is unreachable in
            // practice; skip checking (like `Dynamic`) to stay safe should a
            // future annotation resolve to a metatype.
            // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip like
            // `Meta`/`Dynamic` (ADR 0102). An `Intersection` (`P1 & P2`, ADR
            // 0102) declared return type has no single `Known` body
            // shape to compare against here; skip conservatively too.
            InferredType::Meta { .. }
            | InferredType::ProtocolMeta { .. }
            | InferredType::Dynamic(_)
            | InferredType::Negation { .. }
            | InferredType::Intersection { .. }
            // A shape is judged before this match (ADR 0147).
            | InferredType::Shape { .. } => {}
        }
    }

    /// BT-4440: warn when a method whose body is a class object (`C class` /
    /// `P class`) declares a return type that class object does not fit,
    /// judged by [`Self::metatype_assignable_to`] with the
    /// [`Self::is_type_compatible`] tower (the same rule as an argument,
    /// which keeps return checking's unknown-class escape). A declared
    /// `P class` the class misses gets the ADR 0144 conformance wording with
    /// the missing selectors. A `Dynamic` declared return accepts anything.
    fn check_class_object_return(
        &mut self,
        method: &crate::ast::MethodDefinition,
        class_object: ClassObjectType<'_>,
        expected: &InferredType,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
    ) {
        if matches!(expected, InferredType::Dynamic(_)) {
            return;
        }
        let declared_type = expected.display_annotation();
        if Self::metatype_assignable_to(
            class_object,
            &declared_type,
            hierarchy,
            self.protocol_registry.as_deref(),
            Self::is_type_compatible,
        ) {
            return;
        }
        let expected_display = expected
            .display_for_diagnostic()
            .unwrap_or_else(|| InferredType::class_name_for_diagnostic(declared_type.as_str()));
        let selector = method.selector.name();
        let diag = Self::metatype_mismatch_diagnostic(
            class_object,
            &declared_type,
            hierarchy,
            self.protocol_registry.as_deref(),
            format!(
                "Method '{selector}' in {class_name} declares return type {expected_display}, but body returns {class_object}"
            ),
            format!("Declared -> {expected_display}, inferred body type is {class_object}"),
            method.span,
        );
        self.diagnostics.push(diag);
    }

    /// Returns whether a `Known` type — either a method body's whole inferred
    /// type or one member of a `Union` body (Part A) — is compatible
    /// with an `expected` (resolved declared) return type.
    ///
    /// Mirrors the comparison rules `check_return_type`'s `Known` body arm
    /// applies against `expected`: base-class compatibility via
    /// [`Self::is_type_compatible`], plus a recursive type-args check
    /// when both sides carry generic arguments and the arities
    /// match. `expected == Never` is always a mismatch for a concrete `Known`
    /// value (a divergent declaration requires a divergent body). `Meta` /
    /// `Dynamic` / `Negation` / `Intersection` expected types have no
    /// reliable `Known`-vs-`expected` comparison, so — like the `Known` body
    /// arm's own fallthrough for those — they're treated as compatible
    /// (conservative skip, not a false positive).
    fn known_type_compatible_with_expected(
        actual_ty: &EcoString,
        actual_args: &[InferredType],
        expected: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        match expected {
            InferredType::Known {
                class_name: expected_ty,
                type_args: expected_args,
                ..
            } => {
                Self::is_type_compatible(actual_ty, expected_ty, hierarchy)
                    && Self::same_base_type_args_compatible(
                        expected_ty,
                        expected_args,
                        actual_args,
                        hierarchy,
                    )
            }
            // Delegate to `known_type_compatible_with_union_member`,
            // which also compares generic type args instead of stopping at
            // base-class compatibility — otherwise a union body member like
            // `Result(Integer, Error)` is wrongly treated as compatible with
            // a declared `Result(String, Error) | Nil` just because the
            // `Result` base class matches.
            InferredType::Union { members, .. } => members.iter().any(|member| {
                Self::known_type_compatible_with_union_member(
                    actual_ty,
                    actual_args,
                    member,
                    hierarchy,
                )
            }),
            InferredType::Never => false,
            InferredType::Meta { .. }
            | InferredType::ProtocolMeta { .. }
            | InferredType::Dynamic(_)
            | InferredType::Negation { .. }
            | InferredType::Intersection { .. }
            // A shape is judged by `shape_assignable` (ADR 0147).
            | InferredType::Shape { .. } => true,
        }
    }

    /// Compare a body's `Known` type against one member of a
    /// `Union` expected/declared return type.
    ///
    /// Shared by both `Union`-arm sites in this file — `check_return_type`'s
    /// own dispatch (declared type is directly a `Union`) and
    /// [`Self::known_type_compatible_with_expected`] (declared type is a
    /// `Union` reached via `check_union_body_return_type`'s per-member
    /// check) — so this comparison lives in one place.
    ///
    /// Mirrors the `Known`-vs-`Known` comparison in
    /// [`Self::known_type_compatible_with_expected`]: base-class
    /// compatibility via [`Self::is_type_compatible`], plus a recursive
    /// type-args check via [`Self::type_args_compatible`] when both the
    /// actual type and the member carry generic arguments of matching
    /// arity — not just the base class (via `member.as_known()`), so a
    /// body like `Result(Integer, Error)` is not
    /// wrongly treated as compatible with a declared `Result(String,
    /// Error) | Nil` merely because the `Result` base class matches.
    ///
    /// Stays conservative (returns `true`) for any non-`Known` member
    /// (`Dynamic`/`Union`/`Meta`/`Never`) — those can't be reasoned about
    /// precisely enough to warn without risking a false positive, matching
    /// the previous `member.as_known().is_none_or(...)` behavior.
    fn known_type_compatible_with_union_member(
        actual_ty: &EcoString,
        actual_args: &[InferredType],
        member: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        let InferredType::Known {
            class_name: member_ty,
            type_args: member_args,
            ..
        } = member
        else {
            return true;
        };
        Self::is_type_compatible(actual_ty, member_ty, hierarchy)
            && Self::same_base_type_args_compatible(member_ty, member_args, actual_args, hierarchy)
    }

    /// Part A: validate a `Union` method body against the declared
    /// return type.
    ///
    /// `check_return_type` must not bail out of validation entirely for
    /// `Union` bodies — so `foo -> String => self bar ifTrue: ["yes"]
    /// ifFalse: [42]` (body infers `Union(String, Integer)`) must not compile with
    /// zero diagnostics despite `Integer` never satisfying `-> String`.
    ///
    /// Every member is checked with
    /// [`Self::known_type_compatible_with_expected`] — the same rules the
    /// `Known` body arm applies — and a single incompatible member raises the
    /// same `DiagnosticCategory::Type` mismatch diagnostic the `Known` arm
    /// produces (same severity: this is a real bug, not a new lint class). A
    /// union whose members are all compatible with `expected` — including
    /// a legitimate branch-union case, e.g. `ifNil:ifNotNil:`
    /// producing exactly the declared `T | R` — stays silent.
    ///
    /// Conservative like `classify_union_members`: if any member is
    /// itself not `Known` (nested `Dynamic`/`Union`/`Meta`/`Never`/etc.), skip
    /// — we can't reason about those precisely enough to warn without risking
    /// false positives.
    ///
    /// Scoped to `typed` classes only, matching
    /// Part B's gating even though the underlying comparison itself is
    /// typed-status-agnostic — this keeps both diagnostics equally
    /// narrow and avoids a noise spike in untyped/dynamic classes.
    fn check_union_body_return_type(
        &mut self,
        method: &crate::ast::MethodDefinition,
        body_type: &InferredType,
        members: &[InferredType],
        expected: &InferredType,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
    ) {
        if !hierarchy.is_typed(class_name) {
            return;
        }

        // BT-4454: class-object members are judged by the return slot rule
        // (the same one `check_class_object_return` applies to a bare class
        // object); the remaining members keep the check below.
        if !matches!(expected, InferredType::Dynamic(_)) {
            let declared_type = expected.display_annotation();
            let expected_display = expected
                .display_for_diagnostic()
                .unwrap_or_else(|| InferredType::class_name_for_diagnostic(declared_type.as_str()));
            let selector = method.selector.name();
            if let Some(diag) = self.union_class_object_diagnostic(
                members,
                &declared_type,
                hierarchy,
                Self::is_type_compatible,
                method.span,
                |class_object| {
                    (
                        format!(
                            "Method '{selector}' in {class_name} declares return type {expected_display}, but body returns {class_object}"
                        ),
                        format!("Declared -> {expected_display}, inferred body type is {class_object}"),
                    )
                },
            ) {
                self.diagnostics.push(diag);
                return;
            }
        }
        let instance_members = Self::non_class_object_members(members);
        let members = instance_members.as_slice();

        let mut known_members = Vec::with_capacity(members.len());
        for member in members {
            let InferredType::Known {
                class_name: member_ty,
                type_args: member_args,
                ..
            } = member
            else {
                return; // Non-Known member — can't reliably check, skip conservatively.
            };
            known_members.push((member_ty, member_args));
        }

        let all_compatible = known_members.iter().all(|(member_ty, member_args)| {
            Self::known_type_compatible_with_expected(member_ty, member_args, expected, hierarchy)
        });
        if all_compatible {
            return;
        }

        let selector = method.selector.name();
        let expected_display = expected
            .display_for_diagnostic()
            .unwrap_or_else(|| EcoString::from("Dynamic"));
        let actual_display = body_type
            .display_for_diagnostic()
            .unwrap_or_else(|| EcoString::from("Dynamic"));
        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Method '{selector}' in {class_name} declares return type {expected_display}, but body returns {actual_display}"
                ),
                method.span,
            )
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// Part B: hint when a `Dynamic` body swallows type information
    /// the checker could plausibly have recovered.
    ///
    /// Fires only when all of the following hold:
    /// - the method is declared in a `typed` class (untyped/dynamic classes
    ///   intentionally don't get this rigor),
    /// - the method has a declared concrete return type (guaranteed by the
    ///   caller — `check_return_type` already returned early otherwise), and
    /// - the `Dynamic` reason is `AmbiguousControlFlow` or `UnannotatedReturn`
    ///   specifically — the two reasons that indicate the checker *could*
    ///   have inferred something better.
    ///
    /// Deliberately excludes `DynamicReceiver`, `UntypedFfi`, `DynamicSpec`,
    /// and `Unknown` — those are legitimately dynamic (FFI boundaries,
    /// dynamic receivers, no-spec externals) and not actionable in `typed`
    /// classes. They stay invisible except via `diagnostic_summary`'s
    /// type-coverage percentage, same as before this change, keeping this new
    /// diagnostic narrowly scoped.
    fn check_dynamic_body_return_hint(
        &mut self,
        method: &crate::ast::MethodDefinition,
        reason: DynamicReason,
        expected: &InferredType,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
    ) {
        if !matches!(
            reason,
            DynamicReason::AmbiguousControlFlow | DynamicReason::UnannotatedReturn
        ) {
            return;
        }
        if !hierarchy.is_typed(class_name) {
            return;
        }
        let description = reason
            .description()
            .expect("AmbiguousControlFlow/UnannotatedReturn always have a description");

        let selector = method.selector.name();
        let expected_display = expected
            .display_for_diagnostic()
            .unwrap_or_else(|| EcoString::from("Dynamic"));
        self.diagnostics.push(
            Diagnostic::hint(
                format!(
                    "Method '{selector}' in {class_name} declares return type {expected_display}, but its body inferred as Dynamic ({description})"
                ),
                method.span,
            )
            .with_category(DiagnosticCategory::Type)
            .with_hint(
                "Add explicit type annotations or restructure control flow so the checker can infer a concrete return type",
            ),
        );
    }
}
