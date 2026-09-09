// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Match exhaustiveness and impossible-comparison diagnostics (BT-3461).
//!
//! Pure validation per ADR 0106/0107: these methods consult an already
//! *inferred* type (never perform inference themselves) and emit
//! `Diagnostic::error()`/`warning()`/`hint()` when a `class = C` /
//! `isKindOf: C` / singleton-equality comparison is statically decidable,
//! or when a `match:`/`matchExhaustive:` over a closed union leaves a
//! residual uncovered.
//!
//! Split out of `inference.rs` (BT-3461): narrowing *refinement* (computing
//! branch types once a shape is detected) stays in
//! [`super::narrowing::refine`] — this module is the diagnostic-only half.

use crate::ast::{Literal, MatchArm, Pattern};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::string_utils::edit_distance;
use crate::semantic_analysis::validators::is_concrete_leaf_class;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::{EcoString, eco_format};

use super::well_known::WellKnownClass;
use super::{InferredType, TypeChecker, narrowing};

impl TypeChecker {
    /// ADR 0102 §2 group 2 / BT-2741: emits the "comparison can never be
    /// true" hint when a `class = C` / `isKindOf: C` test is statically
    /// decidable impossible — `C` is hierarchy-unrelated to `current_ty`, so
    /// `intersect(current_ty, C)` is `Never`.
    ///
    /// Parallels `check_impossible_singleton_comparison`'s gating (silent on
    /// `Dynamic` and `Never` receivers, silent whenever the intersect result
    /// is not `Never`) with one deliberate extra gate: **provenance**. The
    /// hint fires only when the receiver's type was *inferred* from actual
    /// value flow (`x := 42. x isKindOf: String`). A *declared* annotation
    /// (`aClass :: Behaviour`) is an unverified promise under gradual typing,
    /// and `isKindOf:` is precisely how code verifies it at runtime — stdlib
    /// defensive guards like `SystemNavigation referencesTo:`'s
    /// `(aClass isKindOf: Symbol)` check are legitimate and must stay silent.
    /// Conservative rule: only provably-`Inferred` provenance fires;
    /// `Declared`, `Substituted`, `Extracted`, or absent provenance is
    /// silent. (The true-branch narrowing to `Never` is *not* gated — sends
    /// in the unreachable branch are silent per the `Never`-receiver policy,
    /// so it stays harmless.) Unlike the singleton case, there is no "always
    /// true" counterpart — `isKindOf:`/`class =` have no negated form to
    /// swap the message for.
    pub(super) fn check_impossible_class_comparison(
        &mut self,
        current_ty: &InferredType,
        class_name: &EcoString,
        refined_ty: &InferredType,
        test_span: Span,
    ) {
        if matches!(current_ty, InferredType::Dynamic(_) | InferredType::Never)
            || !matches!(refined_ty, InferredType::Never)
        {
            return;
        }
        // Provenance gate (see doc comment): declared annotations are
        // runtime-unverified promises — a defensive `isKindOf:` guard against
        // them is legitimate, so only value-flow-inferred types fire.
        if !matches!(
            current_ty.provenance(),
            Some(super::TypeProvenance::Inferred(_))
        ) {
            return;
        }
        let ty_display = current_ty.display_for_diagnostic().unwrap_or_default();
        let message =
            format!("comparison can never be true: `{ty_display}` is never a `{class_name}`");
        self.diagnostics.push(
            Diagnostic::hint(message, test_span)
                .with_category(crate::source_analysis::DiagnosticCategory::Type),
        );
    }

    /// BT-2624 / BT-2631: emits the "comparison can never be true" / "always
    /// true" hint when a singleton (in)equality test (`var =:= #foo`) is
    /// statically decidable — the singleton can never be a value of `current_ty`.
    ///
    /// Called from `infer_message_send_with_receiver_ty`, which is the single
    /// emitter for both the guarded path (`(unionVar = #foo) ifTrue: [...]`,
    /// where the inner `=` send is inferred through that method) and the bare
    /// standalone path (`unionVar = #foo` outside a guard), so the membership
    /// rule (`type_admits_singleton`) and the diagnostic wording live in one
    /// place. Stays conservative: silent on `Dynamic` (unknown) and `Never`
    /// (already-unreachable) receivers, and silent when any member admits the
    /// singleton (it *is* the singleton, or `Symbol`/one of its supertypes).
    pub(super) fn check_impossible_singleton_comparison(
        &mut self,
        current_ty: &InferredType,
        singleton: &narrowing::SingletonName,
        negated: bool,
        test_span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        if matches!(current_ty, InferredType::Dynamic(_) | InferredType::Never)
            || Self::type_admits_singleton(current_ty, singleton, hierarchy)
        {
            return;
        }
        // BT-2897 / ADR 0108: `display_for_diagnostic` already prefixes an
        // alias name here when `current_ty` is the eager expansion of a
        // registered alias (`TypeProvenance::Aliased`) — see that method's
        // doc — so this membership diagnostic names the alias with no
        // bespoke code beyond the display layer, e.g. `RestartStrategy
        // (#temporary | #transient | #permanent)`.
        let ty_display = current_ty.display_for_diagnostic().unwrap_or_default();
        let mut message = if negated {
            format!("comparison is always true: `{singleton}` is never a value of `{ty_display}`")
        } else {
            format!("comparison can never be true: `{singleton}` is not a value of `{ty_display}`")
        };
        // "Did you mean" suggestion (ADR 0108 Error examples): only for the
        // non-negated ("can never be true") case — a mistyped singleton
        // literal is the concrete membership-violation shape the ADR gives
        // (`#premanent` vs `#permanent`), and only when the receiver is a
        // *closed* singleton union, so the suggestion pool is the finite,
        // known member set rather than an open-ended guess.
        if !negated {
            if let Some(suggestion) =
                Self::closest_singleton_member(singleton.as_type_name(), current_ty)
            {
                use std::fmt::Write;
                let _ = write!(message, " — did you mean `{suggestion}`?");
            }
        }
        self.diagnostics.push(
            Diagnostic::hint(message, test_span)
                .with_category(crate::source_analysis::DiagnosticCategory::Type),
        );
    }

    /// Finds the closest singleton member of `ty` (a closed singleton union)
    /// to `target` by edit distance, for the "did you mean" suggestion in
    /// [`check_impossible_singleton_comparison`]. Mirrors
    /// `validation.rs`'s `closest_state_slot` thresholding (distance > 0,
    /// distance ≤ 3, distance less than half the target's length) so typo
    /// suggestions read consistently across the checker. Returns `None` for
    /// anything other than a closed singleton union (including `Symbol`
    /// itself, `Negation`, and every other shape `type_admits_singleton`
    /// already lets through unchanged).
    fn closest_singleton_member(target: &EcoString, ty: &InferredType) -> Option<EcoString> {
        let InferredType::Union { members, .. } = ty else {
            return None;
        };
        let mut best: Option<(EcoString, usize)> = None;
        for member in members {
            let InferredType::Known { class_name, .. } = member else {
                continue;
            };
            if !class_name.starts_with('#') {
                continue;
            }
            let dist = edit_distance(target.as_str(), class_name.as_str());
            if dist > 0
                && dist <= 3
                && dist < target.len() / 2 + 1
                && best.as_ref().is_none_or(|(_, d)| dist < *d)
            {
                best = Some((class_name.clone(), dist));
            }
        }
        best.map(|(name, _)| name)
    }

    /// BT-2624 / ADR 0102 §2: whether the singleton `singleton` (`#foo`) could
    /// be a runtime value of `ty` — defined as
    /// `intersect(ty, #foo, hierarchy) != Never`.
    ///
    /// Intersection already models singleton membership (`Symbol ∩ #foo = #foo`,
    /// `Object ∩ #foo = #foo`, `Integer ∩ #foo = Never`, and it distributes over
    /// unions), so this is the single source of truth for "the test could hold".
    /// `Dynamic` intersects to the singleton (non-`Never`), so callers stay
    /// conservative when the type is unknown.
    ///
    /// The [`narrowing::SingletonName`] parameter guarantees at the type level
    /// (BT-2764) that the pattern is a bare `#foo` singleton, never a nominal
    /// class name — singletons are not hierarchy entries, so a nominal name
    /// here would silently mis-answer membership.
    ///
    /// The hierarchy is threaded through so *supertypes* of `Symbol` other
    /// than `Object` (e.g. an abstract `ProtoObject`-typed receiver) also
    /// admit singletons (BT-2764): `intersect`'s symbol-singleton arms consult
    /// the hierarchy to reduce `ProtoObject ∩ #foo` to `#foo` rather than
    /// falling through to `Never`, matching the pre-ADR-0102 hierarchy walk.
    fn type_admits_singleton(
        ty: &InferredType,
        singleton: &narrowing::SingletonName,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        let matched = InferredType::known(singleton.as_type_name().clone());
        let provenance = super::TypeProvenance::Inferred(Span::default());
        !matches!(
            // No protocol registry needed — a bare singleton is never a
            // protocol name.
            InferredType::intersect(ty, &matched, provenance, Some(hierarchy), None),
            InferredType::Never
        )
    }

    /// BT-2745 / ADR 0102 §4: `true` when `ty` is a *known-closed* singleton
    /// union — an `InferredType::Union` whose every member is a bare
    /// `#symbol` singleton (`Known` with a `#`-prefixed name and no type
    /// args).
    ///
    /// This single structural condition is the entire gate for
    /// [`check_singleton_match_exhaustiveness`](Self::check_singleton_match_exhaustiveness),
    /// and it is what keeps the check silent under every open-world case
    /// ADR 0100 requires (mirroring `check_impossible_singleton_comparison`'s
    /// conservatism):
    /// - `Dynamic` is not a `Union` at all — silent.
    /// - A bare/open `Symbol` (`Known("Symbol")`) is not a `Union` — silent.
    /// - `Negation` (`Symbol \ #foo`, a co-finite set) is not a `Union` — silent.
    /// - A `perform:`-typed or DNU-overriding receiver types as `Dynamic`
    ///   upstream, so it never reaches here as a singleton union — silent.
    /// - A union with *any* non-singleton member — including a single
    ///   `Dynamic` arm — fails the `all()` check, so the *whole* union stays
    ///   silent, matching ADR 0100 Rule 1's "any `Dynamic` in a union
    ///   downgrades the whole union to `Open`".
    fn is_closed_singleton_union(ty: &InferredType) -> bool {
        matches!(ty, InferredType::Union { members, .. }
        if !members.is_empty() && members.iter().all(|m| matches!(
            m,
            InferredType::Known { class_name, type_args, .. }
                if type_args.is_empty() && class_name.starts_with('#')
        )))
    }

    /// BT-2856 / ADR 0107 Phase A: `true` when `ty` is a closed union eligible
    /// for `Nil`/`Type`-pattern exhaustiveness — a closed `Known | Nil` union,
    /// or (more generally) a small closed union whose every member is either
    /// the `nil` class (`UndefinedObject`) or a concrete leaf class in the
    /// exact sense a `Type` pattern arm is already restricted to
    /// ([`is_concrete_leaf_class`](crate::semantic_analysis::validators::is_concrete_leaf_class) —
    /// the same check `validate_type_pattern_class`'s "has subclasses"
    /// compile error enforces, factored out so the two mechanisms can never
    /// disagree about which classes are "closed").
    ///
    /// Disjoint from [`is_closed_singleton_union`](Self::is_closed_singleton_union)
    /// by construction: `is_concrete_leaf_class` never accepts a `#`-prefixed
    /// name, so a bare-symbol union is never also recognised here (and vice
    /// versa) — call sites check the singleton gate first, this one second.
    fn is_closed_leaf_type_union(ty: &InferredType, hierarchy: &ClassHierarchy) -> bool {
        matches!(ty, InferredType::Union { members, .. }
        if !members.is_empty() && members.iter().all(|m| matches!(
            m,
            InferredType::Known { class_name, type_args, .. }
                if type_args.is_empty()
                    && (class_name.as_str() == WellKnownClass::UndefinedObject.as_str()
                        || is_concrete_leaf_class(class_name, hierarchy))
        )))
    }

    /// BT-2856 / ADR 0107 Phase A: `true` when at least one arm is an
    /// (guarded or unguarded) `nil` or `Type` pattern.
    ///
    /// This is the second half of the gate alongside
    /// [`is_closed_leaf_type_union`](Self::is_closed_leaf_type_union) —
    /// required so a `match:`/`matchExhaustive:` that merely *happens* to
    /// have a closed-leaf-class-union scrutinee, but whose arms are ordinary
    /// symbol/literal patterns unrelated to that union (a match that could
    /// never be exhaustive in the first place, and was never gated by this
    /// mechanism before it existed), does not newly start warning/erroring.
    /// Without this second gate, e.g. `x :: Integer | String; x match:
    /// [#north -> ...]` would flip from silent (nothing here has ever been
    /// closed-union-checkable) to "non-exhaustive: `Integer`, `String` are
    /// not handled" — technically true, but out of this feature's scope
    /// (BT-2745/ADR-0106's existing symbol-union/`Result` behaviour must stay
    /// unaffected) and not something the programmer asked this `match:` to
    /// prove.
    ///
    /// The gate is arm-*shape*-based, not arm-*relevance*-based (PR #2965
    /// review): a `nil` arm on a scrutinee whose union does not include
    /// `Nil` (e.g. `x :: Integer | String; x match: [nil -> 0]`) still
    /// engages the mechanism, and the advisory warning names the uncovered
    /// residual (`Integer`, `String`) rather than the impossible `nil` arm
    /// itself. Deliberate: the arm asserts a nil-ness the annotation says
    /// cannot happen, so the match is genuinely suspect, and the
    /// advisory-only warning is the cheapest honest signal — requiring the
    /// covered arms to intersect the scrutinee before engaging would
    /// silence a technically-correct warning exactly where the code most
    /// likely has a bug.
    fn arms_use_nil_or_type_pattern(arms: &[MatchArm]) -> bool {
        arms.iter()
            .any(|arm| matches!(arm.pattern, Pattern::Nil(_) | Pattern::Type { .. }))
    }

    /// BT-2745 / ADR 0102 §4: advisory `match:` exhaustiveness for
    /// singleton-union scrutinees.
    ///
    /// **Distinct from BT-1299.** `validators::match_validators::check_match_exhaustiveness`
    /// is *pattern-based* — it keys on `Result ok:`/`Result error:` constructor
    /// patterns in the arms and emits a hard `Diagnostic::error()`, because
    /// (per its own doc comment) "there is no resolved scrutinee type available
    /// at compile time" for sealed constructor types. This check is the
    /// opposite: it is *type-based*, consulting the scrutinee's **inferred**
    /// type and computing the residual via `difference` (ADR 0102 §1) —
    /// `difference(scrutinee_type, covered)`. The two checks run independently,
    /// side by side, and neither suppresses the other.
    ///
    /// **Severity is `Warning`, never `Error`** (ADR 0102 §4 / ADR 0100):
    /// gradual-typing annotations are not runtime-enforced, so a "provably
    /// exhaustive" static claim can never be a soundness guarantee — an FFI
    /// call typed `#north | #south` can still return `#west` at runtime.
    ///
    /// **Gating:** see [`is_closed_singleton_union`](Self::is_closed_singleton_union).
    ///
    /// **Known discoverability cliff** (ADR 0102 §4, documented here rather
    /// than fixed): this warning silently disappears the moment inference
    /// *widens* the scrutinee — one `Dynamic`-returning arm upstream, a
    /// reassignment to a wider type, or annotating the variable as bare
    /// `Symbol` — and the user has no way to distinguish "provably exhaustive"
    /// from "the checker gave up". An opt-in `match:` assertion (analogous to
    /// TypeScript's `satisfies never` idiom) is the natural follow-up; it is
    /// deliberately out of scope for this ADR so the check stays advisory-only.
    pub(super) fn check_singleton_match_exhaustiveness(
        &mut self,
        scrutinee_ty: &InferredType,
        arms: &[MatchArm],
        match_span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        let residual = if Self::is_closed_singleton_union(scrutinee_ty) {
            Self::singleton_match_residual(scrutinee_ty, arms)
        } else if Self::is_closed_leaf_type_union(scrutinee_ty, hierarchy)
            && Self::arms_use_nil_or_type_pattern(arms)
        {
            Self::nil_or_type_match_residual(scrutinee_ty, arms, hierarchy)
        } else {
            return;
        };
        let Some((residual_display, missing)) = residual else {
            return;
        };
        let missing_str = Self::format_missing_members(&missing);
        let verb = if missing.len() == 1 { "is" } else { "are" };

        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "non-exhaustive match: {missing_str} {verb} not handled \
                     (residual type: `{residual_display}`)"
                ),
                match_span,
            )
            .with_hint(
                "Add an arm for the remaining case(s), or a `_ ->` wildcard \
                 to handle them."
                    .to_string(),
            )
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// BT-2763 / ADR 0106: `matchExhaustive:` — an opt-in **assertion** that a
    /// `match:` is provably exhaustive, at asserted `Error` severity (the user
    /// opted in by writing `matchExhaustive:` instead of `match:`, so ADR
    /// 0100's "escalation to a build-failing error is always opt-in" rule is
    /// satisfied here, not violated).
    ///
    /// **Distinct from, and does not replace,**
    /// [`check_singleton_match_exhaustiveness`](Self::check_singleton_match_exhaustiveness)
    /// (BT-2745's advisory `Warning` path for plain `match:`), which is
    /// unchanged and still runs whenever `exhaustive` is `false` — see the
    /// call site in `infer_expr`'s `Expression::Match` arm.
    ///
    /// Two failure modes, both `Error`:
    /// - **Residual non-empty on a closed singleton union**: same residual
    ///   computation as the advisory check, naming the uncovered members.
    /// - **Scrutinee is not a *known-closed* singleton union at all**
    ///   (`Dynamic`, an open/bare `Symbol`, a `Negation` co-finite set, a
    ///   union with any non-singleton member, or an ordinary nominal type) —
    ///   the assertion cannot be verified, so it fails loudly rather than
    ///   silently downgrading to advisory. This is the behaviour BT-2745 /
    ///   ADR 0102 §4 left as a "known discoverability cliff": once the
    ///   scrutinee widens, `matchExhaustive:` stops being provable and must
    ///   say so, not go quiet.
    pub(super) fn check_asserted_match_exhaustiveness(
        &mut self,
        scrutinee_ty: &InferredType,
        arms: &[MatchArm],
        match_span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        let residual = if Self::is_closed_singleton_union(scrutinee_ty) {
            Self::singleton_match_residual(scrutinee_ty, arms)
        } else if Self::is_closed_leaf_type_union(scrutinee_ty, hierarchy)
            && Self::arms_use_nil_or_type_pattern(arms)
        {
            Self::nil_or_type_match_residual(scrutinee_ty, arms, hierarchy)
        } else {
            let ty_display = scrutinee_ty.display_for_diagnostic().unwrap_or_default();
            self.diagnostics.push(
                Diagnostic::error(
                    format!(
                        "cannot verify `matchExhaustive:` is exhaustive — scrutinee type \
                         `{ty_display}` is not a closed union of symbol singletons, \
                         `nil`, or concrete leaf classes"
                    ),
                    match_span,
                )
                .with_hint(
                    "matchExhaustive: only proves exhaustiveness over a closed union of \
                     `#symbol` singletons (e.g. `x :: #north | #south`), or a closed \
                     `Known | Nil` union covered by `nil`/`Type` patterns (e.g. \
                     `x :: String | Nil`). Annotate the scrutinee with such a type, or use \
                     `match:` if exhaustiveness cannot be guaranteed statically."
                        .to_string(),
                )
                .with_category(DiagnosticCategory::Type),
            );
            return;
        };

        let Some((residual_display, missing)) = residual else {
            return;
        };
        let missing_str = Self::format_missing_members(&missing);
        let verb = if missing.len() == 1 { "is" } else { "are" };

        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "non-exhaustive matchExhaustive: {missing_str} {verb} not handled \
                     (residual type: `{residual_display}`)"
                ),
                match_span,
            )
            .with_hint(
                "Add an arm for the remaining case(s), or a `_ ->` wildcard \
                 to handle them."
                    .to_string(),
            )
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// Shared residual computation for both the advisory (BT-2745) and
    /// asserted (BT-2763) singleton-union `match:` exhaustiveness checks.
    ///
    /// Callers must already have checked
    /// [`is_closed_singleton_union`](Self::is_closed_singleton_union) —
    /// this function assumes `scrutinee_ty` is one.
    ///
    /// Returns `None` when the match is exhaustive (an unguarded
    /// wildcard/variable-binding arm, or a `Never` residual after subtracting
    /// covered singleton arms). Otherwise returns `(residual_display,
    /// missing_members)`.
    fn singleton_match_residual(
        scrutinee_ty: &InferredType,
        arms: &[MatchArm],
    ) -> Option<(EcoString, Vec<EcoString>)> {
        // An unguarded wildcard arm is full coverage — mirrors BT-1299's
        // suppression rule. An unguarded variable-binding arm (`x -> ...`)
        // always matches too, so it counts the same. A *guarded* catch-all
        // (`_ when: [cond] -> ...`) does NOT guarantee coverage of the
        // remaining cases.
        if arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(arm.pattern, Pattern::Wildcard(_) | Pattern::Variable(_))
        }) {
            return None;
        }

        // Collect covered singletons from unguarded symbol-literal arms only —
        // a guarded arm (`#north when: [cond] -> ...`) does not guarantee
        // coverage of that variant, same rule as BT-1299's constructor-arm
        // coverage.
        let mut covered: Vec<InferredType> = Vec::new();
        for arm in arms {
            if arm.guard.is_some() {
                continue;
            }
            if let Pattern::Literal(Literal::Symbol(name), _) = &arm.pattern {
                covered.push(InferredType::known(eco_format!("#{name}")));
            }
        }

        let provenance = super::TypeProvenance::Inferred(Span::default());
        let covered_ty = InferredType::union_of(&covered);
        // Singleton scrutinees only (guaranteed by `is_closed_singleton_union`
        // callers) — singletons are never hierarchy entries, so no hierarchy
        // is needed here.
        let residual = InferredType::difference(scrutinee_ty, &covered_ty, provenance, None);

        // `Never` residual ⇒ every member is covered ⇒ exhaustive.
        if matches!(residual, InferredType::Never) {
            return None;
        }
        Some(Self::residual_missing(&residual))
    }

    /// BT-2856 / ADR 0107 Phase A: shared residual computation for `nil`/
    /// `Type`-pattern coverage over a closed `Known | Nil` union or a small
    /// closed union of concrete leaf classes (see
    /// [`is_closed_leaf_type_union`](Self::is_closed_leaf_type_union), which
    /// callers must already have checked). Structurally mirrors
    /// [`singleton_match_residual`](Self::singleton_match_residual) — same
    /// unguarded-wildcard/variable-binding full-coverage rule, same
    /// guarded-arm-does-not-count rule — but collects covered members from
    /// unguarded `Pattern::Nil` and `Pattern::Type` arms instead of `#symbol`
    /// literal arms, and passes `hierarchy` into `difference` since the
    /// covered members are nominal classes (subclass relationships matter),
    /// unlike bare singletons.
    ///
    /// Returns `None` when the match is exhaustive, otherwise
    /// `(residual_display, missing_members)` — identical shape to
    /// `singleton_match_residual`'s result, so both share one caller-side
    /// diagnostic-emission path.
    fn nil_or_type_match_residual(
        scrutinee_ty: &InferredType,
        arms: &[MatchArm],
        hierarchy: &ClassHierarchy,
    ) -> Option<(EcoString, Vec<EcoString>)> {
        // Same full-coverage rule as `singleton_match_residual`: an unguarded
        // wildcard or variable-binding arm always matches.
        if arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(arm.pattern, Pattern::Wildcard(_) | Pattern::Variable(_))
        }) {
            return None;
        }

        // Collect covered members from unguarded `nil`/`Type` arms only — a
        // guarded arm (`nil when: [cond] -> ...`, `s :: String when: [...] ->
        // ...`) does not guarantee coverage, same rule as every other
        // exhaustiveness check in this file.
        let mut covered: Vec<InferredType> = Vec::new();
        for arm in arms {
            if arm.guard.is_some() {
                continue;
            }
            match &arm.pattern {
                Pattern::Nil(_) => {
                    covered.push(InferredType::known(
                        WellKnownClass::UndefinedObject.as_str(),
                    ));
                }
                Pattern::Type { class, .. } => {
                    covered.push(InferredType::known(class.name.clone()));
                }
                _ => {}
            }
        }

        let provenance = super::TypeProvenance::Inferred(Span::default());
        let covered_ty = InferredType::union_of(&covered);
        // Unlike bare singletons, covered members here are nominal classes —
        // pass `hierarchy` so `difference` can reason about subclass
        // relationships (even though Phase A's leaf-only restriction means
        // there is none to reason about yet; consistent with every other
        // nominal-class `difference` call in this file).
        let residual =
            InferredType::difference(scrutinee_ty, &covered_ty, provenance, Some(hierarchy));

        // `Never` residual ⇒ every member is covered ⇒ exhaustive.
        if matches!(residual, InferredType::Never) {
            return None;
        }
        Some(Self::residual_missing(&residual))
    }

    /// Shared "what's left over" extraction for both
    /// [`singleton_match_residual`](Self::singleton_match_residual) and
    /// [`nil_or_type_match_residual`](Self::nil_or_type_match_residual):
    /// renders `residual` for the diagnostic message, and lists its member
    /// class names — through
    /// [`InferredType::class_name_for_diagnostic`], so a residual `Nil`
    /// member (internally `UndefinedObject`) always renders as `Nil` in the
    /// missing-members list, matching `residual_display`'s own rendering.
    fn residual_missing(residual: &InferredType) -> (EcoString, Vec<EcoString>) {
        let residual_display = residual.display_for_diagnostic().unwrap_or_default();
        let missing: Vec<EcoString> = match residual {
            InferredType::Union { members, .. } => members
                .iter()
                .filter_map(InferredType::as_known)
                .map(|name| InferredType::class_name_for_diagnostic(name))
                .collect(),
            InferredType::Known { class_name, .. } => {
                vec![InferredType::class_name_for_diagnostic(class_name)]
            }
            // Not reachable in practice: `difference` over a union of bare
            // singletons or concrete classes only ever normalises to `Never`,
            // a single `Known`, or a `Union` of `Known`s — but stay
            // conservative rather than panicking if the algebra's normal form
            // ever changes.
            _ => vec![],
        };
        (residual_display, missing)
    }

    /// Formats a list of missing singleton member names as a
    /// backtick-quoted, comma-separated list for a diagnostic message.
    fn format_missing_members(missing: &[EcoString]) -> String {
        missing
            .iter()
            .map(|m| format!("`{m}`"))
            .collect::<Vec<_>>()
            .join(", ")
    }
}
