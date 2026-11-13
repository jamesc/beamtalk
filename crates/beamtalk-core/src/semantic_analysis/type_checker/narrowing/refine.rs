// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Narrowing refinement.
//!
//! Detection of a control-flow narrowing shape (`x isNil`, `x class = Foo`,
//! …) lives in [`super::rules`] via [`super::detect`] — pure AST pattern
//! matching, no type information needed. This module is the other half:
//! once a [`NarrowingInfo`] shape is detected, these methods resolve the
//! tested variable's *current* type (from `TypeEnv`/`ClassHierarchy`) and
//! compute the concrete true/false branch types, including the
//! hierarchy-and-protocol-aware set algebra (`intersect`/`difference`) for
//! class tests.
//!
//! Split out of `inference.rs` (ADR 0106/0107): these methods are
//! narrowing-specific refinement, not general expression type inference.

use crate::ast::Expression;
use crate::semantic_analysis::alias_registry::AliasRegistry;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::source_analysis::Span;
use ecow::EcoString;

use super::super::well_known::WellKnownClass;
use super::super::{InferredType, TypeChecker, TypeEnv, TypeProvenance};
use super::{ClassTestInfo, ClassTestKind, NarrowingInfo};

impl TypeChecker {
    /// Detect control-flow narrowing from the receiver of `ifTrue:`/`ifFalse:`.
    ///
    /// Dispatches through the [`super::rules::RULES`] table.
    /// Kept as a thin wrapper so existing test helpers that call
    /// `TypeChecker::detect_narrowing` stay working without import churn.
    pub(in crate::semantic_analysis::type_checker) fn detect_narrowing(
        receiver: &Expression,
    ) -> Option<NarrowingInfo> {
        super::detect(receiver)
    }

    /// Detect a single-variable type test and run every refinement stage
    /// over it — the leaf of [`Self::condition_facts`], so a stage added here
    /// reaches every consumer (BT-4635, BT-4710). `hint_span` is where a class
    /// test that can never be true is reported; `None` suppresses the hint.
    pub(in crate::semantic_analysis::type_checker) fn detect_refined_narrowing(
        &mut self,
        receiver: &Expression,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        hint_span: Option<Span>,
    ) -> Option<NarrowingInfo> {
        Self::detect_narrowing(receiver)
            .map(|info| self.refine_responds_to_narrowing(info))
            .map(|info| {
                Self::refine_result_narrowing(
                    info,
                    env,
                    hierarchy,
                    self.alias_registry.as_deref(),
                    self.protocol_registry.as_deref(),
                )
            })
            .map(|info| {
                Self::refine_singleton_narrowing(
                    info,
                    env,
                    hierarchy,
                    self.alias_registry.as_deref(),
                    self.protocol_registry.as_deref(),
                )
            })
            .map(|info| self.refine_nil_test(info, env, hierarchy))
            .map(|info| match hint_span {
                Some(span) => self.refine_class_narrowing(info, env, hierarchy, span),
                // Already type-checked (post-guard) or nested in a chain: the
                // same math, without re-reporting the "can never be true" hint.
                None if info.class_test.is_some() => {
                    let current_ty = self.narrowing_variable_type(&info.variable, env, hierarchy);
                    Self::compute_class_narrowing(
                        info,
                        &current_ty,
                        hierarchy,
                        self.protocol_registry.as_deref(),
                    )
                }
                None => info,
            })
    }

    /// Resolves an inequality nil test (`x =/= nil`, `x /= nil`, BT-4712).
    ///
    /// `detect` produces the `isNil` shape; for a negated test the true
    /// branch is the non-nil type (resolved against the variable's live type,
    /// like the `isNil` false branch) and the false branch is `UndefinedObject`.
    /// Idempotent and a no-op for every other shape, so each pipeline that
    /// refines singleton/class tests can call it unconditionally.
    pub(in crate::semantic_analysis::type_checker) fn refine_nil_negation(
        mut info: NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        alias_registry: Option<&AliasRegistry>,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> NarrowingInfo {
        if !(info.is_nil_check && info.nil_test_negated) || info.false_type.is_some() {
            return info;
        }
        let current_ty = Self::resolve_narrowing_variable_type(
            &info.variable,
            env,
            hierarchy,
            alias_registry,
            protocol_registry,
        );
        info.false_type = Some(std::mem::replace(
            &mut info.true_type,
            Self::non_nil_type(&current_ty),
        ));
        info
    }

    /// [`Self::refine_nil_negation`] with this checker's registries, so
    /// pipeline call sites stay one line.
    pub(in crate::semantic_analysis::type_checker) fn refine_nil_test(
        &self,
        info: NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
    ) -> NarrowingInfo {
        Self::refine_nil_negation(
            info,
            env,
            hierarchy,
            self.alias_registry.as_deref(),
            self.protocol_registry.as_deref(),
        )
    }

    /// Refines a `respondsTo:` narrowing from `Dynamic` to a protocol type
    /// when the protocol registry is available and exactly one protocol
    /// requires the tested selector (ADR 0068 Phase 2e).
    ///
    /// If no protocol registry is set, or zero/multiple protocols match the
    /// selector, the narrowing is returned unchanged (stays `Dynamic`).
    pub(in crate::semantic_analysis::type_checker) fn refine_responds_to_narrowing(
        &self,
        mut info: NarrowingInfo,
    ) -> NarrowingInfo {
        // Only refine to a protocol type when the variable is currently Dynamic.
        // If the variable already has a concrete type (e.g., Integer), narrowing
        // to a protocol (e.g., Printable) would lose type-specific APIs.
        if matches!(info.true_type, InferredType::Dynamic(_))
            && let Some(ref selector) = info.responded_selector
            && let Some(ref registry) = self.protocol_registry
            && let Some(protocol_name) = registry.find_unique_protocol_for_selector(selector)
        {
            info.true_type = InferredType::known(protocol_name.clone());
        }
        info
    }

    /// Refines a Result `isOk` / `ok` / `isError` narrowing.
    ///
    /// When the variable has type `Result(T, E)`, both true and false branches
    /// keep the full `Result(T, E)` type — generic substitution already resolves
    /// `value -> T` and `error -> E`.  The narrowing ensures the branches get
    /// typed environments (via `infer_block_with_narrowing`) rather than falling
    /// through to the no-narrowing path.
    ///
    /// If the variable is not typed as `Result`, the result-specific flags are
    /// cleared and `true_type` is set to the variable's current type so the
    /// narrowing is effectively a no-op (preserves the existing type in blocks).
    pub(in crate::semantic_analysis::type_checker) fn refine_result_narrowing(
        mut info: NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        alias_registry: Option<&AliasRegistry>,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> NarrowingInfo {
        if !info.is_result_ok_check && !info.is_result_error_check {
            return info;
        }
        let current_ty = Self::resolve_narrowing_variable_type(
            &info.variable,
            env,
            hierarchy,
            alias_registry,
            protocol_registry,
        );
        let is_result = matches!(
            &current_ty,
            InferredType::Known { class_name, .. }
                if WellKnownClass::from_str(class_name) == Some(WellKnownClass::Result)
        );
        if is_result {
            // Both branches keep the full Result(T, E) type so generic
            // substitution continues to resolve value->T / error->E.
            info.true_type = current_ty.clone();
            info.false_type = Some(current_ty);
        } else {
            // Not a Result — clear the result flags so downstream code doesn't
            // treat this as a Result narrowing.  Set true_type to the variable's
            // current type to preserve type info in the block env.
            info.is_result_ok_check = false;
            info.is_result_error_check = false;
            info.true_type = current_ty;
            info.false_type = None;
        }
        info
    }

    /// Refines a singleton (in)equality narrowing `x = #foo` / `#foo = x`.
    ///
    /// `detect` only sees the AST, so it leaves the branch types provisional
    /// and records the tested singleton in `singleton_eq`. Here we resolve the
    /// variable's current type and split it: the branch where the test holds
    /// narrows to the singleton, and the complementary branch narrows to the
    /// variable's type with that singleton removed (`Integer | #infinity` minus
    /// `#infinity` ⇒ `Integer`). For an inequality (`/=`, `=/=`) the two
    /// branches are swapped.
    ///
    /// The "comparison can never be true / always
    /// true" hint for a statically decidable singleton test is *not* emitted
    /// here. A guarded comparison (`unionVar = #foo ifTrue: …`) has its receiver
    /// — the `=` send — inferred through `infer_message_send_with_receiver_ty`,
    /// whose `check_impossible_singleton_comparison` is the single emitter for
    /// both guarded and bare comparisons. Emitting here too would duplicate the
    /// hint. This method only refines the branch types.
    pub(in crate::semantic_analysis::type_checker) fn refine_singleton_narrowing(
        mut info: NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        alias_registry: Option<&AliasRegistry>,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> NarrowingInfo {
        let Some(eq) = info.singleton_eq.clone() else {
            return info;
        };
        let current_ty = Self::resolve_narrowing_variable_type(
            &info.variable,
            env,
            hierarchy,
            alias_registry,
            protocol_registry,
        );

        let matched = InferredType::known(eq.singleton.as_type_name().clone());
        let provenance = TypeProvenance::Inferred(Span::default());
        // ADR 0102 §2: the equality branches are the set-theoretic intersection
        // and difference of the variable's current type with the tested
        // singleton. `intersect(T, #foo)` is the "test holds" type (the values of
        // `T` that could be `#foo`); `difference(T, #foo)` is the complementary
        // type with `#foo` removed. An impossible test (`x :: Integer; x = #foo`)
        // yields `intersect = Never` for the unreachable branch — the diagnostic
        // for that case is emitted separately by
        // `check_impossible_singleton_comparison`.
        // Out of scope for narrowing (ADR 0102 §2 group 3): singleton
        // narrowing never intersects with a protocol name, so `None` here.
        let holds = InferredType::intersect(
            &current_ty,
            &matched,
            provenance.clone(),
            Some(hierarchy),
            None,
        );
        let removed = InferredType::difference(&current_ty, &matched, provenance, Some(hierarchy));
        if eq.negated {
            // `x /= #foo`: the true branch removes the singleton; the false
            // branch is the singleton.
            info.true_type = removed;
            info.false_type = Some(holds);
        } else {
            // `x = #foo`: the true branch is the singleton; the false branch
            // removes it.
            info.true_type = holds;
            info.false_type = Some(removed);
        }
        info
    }

    /// Refines a `class = C` / `isKindOf: C` narrowing (ADR 0102 §2 group 2,
    /// §5).
    ///
    /// `detect` only sees the AST, so it records the tested class name in
    /// `class_test` and leaves `true_type` provisional. Here we resolve the
    /// variable's current type and delegate to the diagnostic-free
    /// [`Self::compute_class_narrowing`] (factored out so
    /// `Self::apply_early_return_narrowing` can reuse the same math without
    /// re-emitting the "comparison can never be true" hint below for a guard
    /// that was already fully type-checked), then reports that hint using
    /// the resolved true branch.
    pub(in crate::semantic_analysis::type_checker) fn refine_class_narrowing(
        &mut self,
        info: NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        test_span: Span,
    ) -> NarrowingInfo {
        let Some(ClassTestInfo { class_name, .. }) = info.class_test.clone() else {
            return info;
        };
        let current_ty = self.narrowing_variable_type(&info.variable, env, hierarchy);
        let refined_info = Self::compute_class_narrowing(
            info,
            &current_ty,
            hierarchy,
            self.protocol_registry.as_deref(),
        );
        self.check_impossible_class_comparison(
            &current_ty,
            &class_name,
            &refined_info.true_type,
            test_span,
        );
        refined_info
    }

    /// Pure narrowing math for a `class = C` / `isKindOf: C` test — no
    /// diagnostics. Shared by [`Self::refine_class_narrowing`] (the
    /// primary `ifTrue:`/`ifFalse:` dispatch site, which additionally emits
    /// the "comparison can never be true" hint) and
    /// `Self::apply_early_return_narrowing` (the guard-and-early-return
    /// post-guard case, which must *not* re-emit that hint since the guard
    /// expression was already fully type-checked by the time post-guard
    /// narrowing runs).
    ///
    /// Routes the **true** branch through the hierarchy-and-protocol-aware
    /// `intersect(current, C)` for *both* idioms, and the **false** branch
    /// through `difference(current, C)` for `isKindOf:` *only* — see
    /// `ClassTestKind` for why `class =:=`'s false branch must stay
    /// unnarrowed.
    ///
    /// The true branch narrows precisely (`x :: Number; x isKindOf: Integer`
    /// true branch is `Integer`, not `Number`), and a test against a
    /// hierarchy-unrelated class types the (unreachable) true branch `Never`
    /// (reported by the caller via `check_impossible_class_comparison`).
    ///
    /// **Protocol collapse:** when `current` is a protocol (or a
    /// union containing one) and `C` is an unrelated concrete class,
    /// `intersect` conservatively returns the irreducible `current & C`
    /// (ADR 0102 §1/§3) — sound for a *declared* `P1 & P2` annotation, but
    /// `isKindOf: C` is a positive **runtime** proof that the value literally
    /// is a `C` (or subclass), which is strictly stronger. The true branch
    /// collapses that `Intersection` down to the bare `C` so downstream
    /// assignability (`is_assignable_to`, which has no notion of `&`) and DNU
    /// checks see a plain nominal type instead of a compound one they don't
    /// otherwise understand — this is what lets a `Printable`-declared local
    /// satisfy a `List`-typed assignment after `(x isKindOf: List) ifTrue:
    /// [...]` / `ifFalse: [^...]` without an `@expect type` escape hatch.
    ///
    /// The `isKindOf:` false branch closes the group-2 gap ADR 0102 §1
    /// deliberately left open (nominal-class difference needed its own
    /// design, §5): `x :: Number; x isKindOf: Integer` false branch narrows
    /// to `Number \ Integer` (previously untouched — `false_type` stayed
    /// `None`, and `ifFalse:`/the else-arm of `ifTrue:ifFalse:` fell back to
    /// no narrowing). `class =:=`'s false branch stays `None`.
    pub(in crate::semantic_analysis::type_checker) fn compute_class_narrowing(
        mut info: NarrowingInfo,
        current_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&crate::semantic_analysis::protocol_registry::ProtocolRegistry>,
    ) -> NarrowingInfo {
        let Some(ClassTestInfo { class_name, kind }) = info.class_test.clone() else {
            return info;
        };
        let pattern = InferredType::known(class_name.clone());
        info.true_type =
            Self::intersect_with_class(current_ty, &class_name, hierarchy, protocol_registry);
        // Only `isKindOf:`'s false branch can be narrowed via
        // nominal-class `difference` — `Negation{base, excluded}` always
        // excludes `excluded`'s *entire* subtree, which matches `isKindOf:`'s
        // negation ("not C and not any subclass of C") but not `class =:=`'s
        // ("not exactly C" — C's subclasses are still live possibilities;
        // narrowing them away would produce a false "comparison can never be
        // true" hint on a later, satisfiable `isKindOf:` test). See
        // `ClassTestKind`.
        if kind == ClassTestKind::KindOf {
            info.false_type = Some(InferredType::difference(
                current_ty,
                &pattern,
                TypeProvenance::Inferred(Span::default()),
                Some(hierarchy),
            ));
        }
        info
    }

    /// `intersect(current, Known(class_name))`, collapsing a compound
    /// `Intersection` result down to the bare nominal `class_name`
    /// — shared by `isKindOf:`/`class =` guard narrowing (above) and
    /// `Pattern::Type` match-arm binding narrowing (ADR 0107), both
    /// of which need the same "this value literally is `class_name`"
    /// true-branch collapse so downstream `is_assignable_to`/DNU checks see
    /// a plain nominal type rather than an `Intersection` they don't
    /// otherwise understand.
    pub(in crate::semantic_analysis::type_checker) fn intersect_with_class(
        current_ty: &InferredType,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&crate::semantic_analysis::protocol_registry::ProtocolRegistry>,
    ) -> InferredType {
        let pattern = InferredType::known(class_name.clone());
        let provenance = TypeProvenance::Inferred(Span::default());
        let refined = InferredType::intersect(
            current_ty,
            &pattern,
            provenance,
            Some(hierarchy),
            protocol_registry,
        );
        if matches!(refined, InferredType::Intersection { .. }) {
            pattern
        } else {
            refined
        }
    }
}
