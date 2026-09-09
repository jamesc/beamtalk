// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Control-flow narrowing and divergence type inference.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Covers `Expression::Match` (arm-type union with nil/class-pattern
//! residual narrowing, ADR 0107 Phase A), pattern-variable binding shared
//! with `DestructureAssignment`, early-return narrowing after a diverging
//! `ifTrue:`/`ifFalse:` guard (ADR 0068 Phase 1g), and the `Never`/divergence
//! detection ([`Self::block_diverges`]) that narrowing depends on.

use crate::ast::{Expression, MatchArm, MessageSelector, Pattern, WellKnownSelector};
use crate::semantic_analysis::alias_registry::AliasRegistry;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::narrowing::NarrowingInfo;
use crate::semantic_analysis::type_checker::narrowing::extract::extract_variable_name;
use crate::semantic_analysis::type_checker::narrowing::refinement::RefinementLayer;
use crate::semantic_analysis::type_checker::narrowing::visitors::{
    block_has_return, block_may_reassign,
};
use crate::semantic_analysis::type_checker::type_resolver;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{
    DynamicReason, EnvKey, InferredType, TypeChecker, TypeEnv, TypeStringContext,
};
use crate::source_analysis::Span;

impl TypeChecker {
    /// Bind all named variables in a destructuring `pattern` into `env` as `Dynamic`.
    ///
    /// Delegates to [`crate::semantic_analysis::extract_pattern_bindings`] to walk
    /// all pattern variants (including `Binary` segments) consistently. Wildcards
    /// and literals are skipped; duplicates are silently ignored (the name resolver
    /// already reports them as errors earlier in the pipeline).
    pub(in crate::semantic_analysis::type_checker) fn bind_pattern_vars(
        pattern: &Pattern,
        env: &mut TypeEnv,
    ) {
        let (bindings, _diagnostics) = crate::semantic_analysis::extract_pattern_bindings(pattern);
        for id in bindings {
            env.set_local(
                id.name.clone(),
                InferredType::Dynamic(DynamicReason::Unknown),
            );
        }
    }

    /// Check whether a block's execution cannot fall through to the enclosing
    /// method's next statement (BT-2049, extended in BT-2051).
    ///
    /// A block "diverges" when any of the following hold:
    /// - It contains a non-local return `^expr` (already handled by
    ///   [`block_has_return`]).
    /// - Its body's inferred return type is [`InferredType::Never`] — i.e. the
    ///   last expression is a call to a `-> Never`-returning method such as
    ///   `Object>>error:` or `Object>>notImplemented`.
    /// - Any statement inside the body has — or *contains* as a descendant —
    ///   an expression of inferred type `Never`. This covers both
    ///   `[self error: "…". ^nil]` (trailing `^nil` unreachable) and
    ///   `[logger info: (self error: "…")]` (the diverging call is buried
    ///   in a method-send argument). BT-2051 walks descendants via
    ///   [`Self::expr_contains_never`], symmetric with the `^`-walker
    ///   [`Self::expr_contains_return`].
    ///
    /// The block's inferred type is read from [`TypeChecker::type_map`] as the
    /// final type-arg of its `Block(...)` representation (populated by
    /// [`Self::infer_block_with_narrowing`]). The block must therefore have
    /// been type-checked already; callers run this after the enclosing
    /// expression has been inferred.
    pub(in crate::semantic_analysis::type_checker) fn block_diverges(
        &self,
        block: &crate::ast::Block,
    ) -> bool {
        if block_has_return(block) {
            return true;
        }
        // Look up the block's type from the type_map; the last type-arg of the
        // `Block(P1, ..., Pn, R)` representation is the body's return type.
        if let Some(InferredType::Known {
            class_name,
            type_args,
            ..
        }) = self.type_map.get(block.span)
        {
            if WellKnownClass::from_str(class_name) == Some(WellKnownClass::Block) {
                if let Some(body_ty) = type_args.last() {
                    if matches!(body_ty, InferredType::Never) {
                        return true;
                    }
                }
            }
        }
        block
            .body
            .iter()
            .any(|stmt| self.expr_contains_never(&stmt.expression))
    }

    /// Recursively check whether `expr` — or any sub-expression — has
    /// inferred type [`InferredType::Never`] in the type map (BT-2051).
    ///
    /// This is the `Never`-typed companion to
    /// [`crate::semantic_analysis::type_checker::narrowing::visitors::expr_contains_return`]: both share the
    /// exhaustive [`crate::ast::visitor`] walker so every sub-expression
    /// variant gets covered (BT-2063). A diverging call such as
    /// `self error: "…"` is detected whether it appears as the whole
    /// statement (`[self error: "…"]`), as a receiver, buried in a message
    /// send argument (`[logger info: (self error: "…")]`), inside a
    /// `DestructureAssignment`, `Match`, `MapLiteral`, `ListLiteral`, etc.
    ///
    /// **Nested block literals are opaque**: a block value is *constructed*,
    /// not *executed*, at this position. Guards like
    /// `[callbacks add: [self error: "later"]]` would otherwise be
    /// mis-classified as diverging even though the outer block still falls
    /// through. This is the [`crate::ast::visitor::Visitor`] default.
    pub(in crate::semantic_analysis::type_checker) fn expr_contains_never(
        &self,
        expr: &Expression,
    ) -> bool {
        use crate::ast::visitor::{Visitor, walk_expr};

        struct Finder<'a> {
            type_map: &'a crate::semantic_analysis::type_checker::TypeMap,
            found: bool,
        }
        impl<'ast> Visitor<'ast> for Finder<'_> {
            fn visit_expr(&mut self, e: &'ast Expression) {
                if self.found {
                    return;
                }
                if matches!(self.type_map.get(e.span()), Some(InferredType::Never)) {
                    self.found = true;
                    return;
                }
                walk_expr(self, e);
            }
            // `visit_block` default (opaque) preserves the BT-2051 rule:
            // inert block literals must NOT count toward divergence.
        }

        let mut finder = Finder {
            type_map: &self.type_map,
            found: false,
        };
        finder.visit_expr(expr);
        finder.found
    }

    /// Remove `UndefinedObject` (nil) from a union type or convert a known type
    /// to itself if it is non-nil — the `isNil ifFalse:` branch type.
    ///
    /// ADR 0102 §2: for a union receiver this routes through the set-theoretic
    /// `difference` operator with `P = UndefinedObject` (nil's type). `"Nil"` is
    /// subtracted alongside the canonical `UndefinedObject` as a defensive alias
    /// (BT-2016) — `resolve_type_keyword` should canonicalize to
    /// `"UndefinedObject"`, but downstream callers may encounter `"Nil"` from
    /// BEAM metadata or return-type strings.
    ///
    /// Two behaviours are preserved from the pre-operator implementation:
    /// - **Nil-only-union → `Dynamic` softening.** A union whose members are all
    ///   nil would collapse to `Never` under `difference`; instead it softens to
    ///   `Dynamic(Unknown)` so an unreachable `ifNotNil:` body still compiles.
    /// - **Non-union pass-through.** A non-union type is returned unchanged (we
    ///   cannot make a non-union "more non-nil"), rather than subtracting nil —
    ///   which would turn a bare `UndefinedObject` receiver into `Never`.
    pub(in crate::semantic_analysis::type_checker) fn non_nil_type(
        ty: &InferredType,
    ) -> InferredType {
        match ty {
            InferredType::Union { .. } => {
                let provenance = crate::semantic_analysis::type_checker::TypeProvenance::Inferred(
                    Span::default(),
                );
                // `P = UndefinedObject | Nil` (both nil spellings, BT-2016).
                let nil = InferredType::simple_union(&["nil", "Nil"]);
                // `nil`/`Nil` are matched by exact equality, not subclassing,
                // so no hierarchy is needed here.
                let stripped = InferredType::difference(ty, &nil, provenance, None);
                // Nil-only union collapses to `Never`; soften to `Dynamic`.
                if matches!(stripped, InferredType::Never) {
                    InferredType::Dynamic(DynamicReason::Unknown)
                } else {
                    stripped
                }
            }
            // If the variable is not a union, narrowing away nil for a non-union
            // type means it stays the same (we can't make it "more non-nil").
            _ => ty.clone(),
        }
    }

    /// Resolve the current type of a narrowing variable from the environment.
    ///
    /// For locals, this is a simple env lookup. For
    /// [`EnvKey::SelfField`] (BT-2048 / BT-2062) we prefer a previously
    /// pushed narrowing, falling back to the declared state type resolved
    /// through the class hierarchy when none is present.
    ///
    /// `alias_registry` (BT-2936, ADR 0108 follow-up to BT-2928) is threaded
    /// through so a `self.field isNil ifFalse: [...]` narrowing check on a
    /// cross-file alias-typed field expands the alias instead of falling
    /// back to an opaque nominal class.
    pub(in crate::semantic_analysis::type_checker) fn resolve_narrowing_variable_type(
        var_key: &EnvKey,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        alias_registry: Option<&AliasRegistry>,
    ) -> InferredType {
        // Regular env lookup first (handles locals and previously-narrowed fields).
        if let Some(ty) = env.get(var_key) {
            return ty;
        }
        // BT-2048 / BT-2062: for `self.<field>` keys, resolve via the class
        // hierarchy using the `self` binding in the env.
        if let EnvKey::SelfField(field_name) = var_key {
            if let Some(InferredType::Known { class_name, .. }) = env.get_local("self") {
                if let Some(field_type) = hierarchy.state_field_type(&class_name, field_name) {
                    return type_resolver::resolve_declared_type(
                        &field_type,
                        &type_resolver::SubstitutionMap::new(),
                        None,
                        alias_registry,
                        TypeStringContext::Declared,
                    );
                }
            }
        }
        InferredType::Dynamic(DynamicReason::Unknown)
    }

    /// Apply early-return narrowing to the environment after a statement.
    ///
    /// Detects `x isNil ifTrue: [<diverge>]` and, since BT-2825, `x isKindOf:
    /// C ifTrue: [<diverge>]` / `ifFalse: [<diverge>]` / `ifTrue: [<diverge>]
    /// ifFalse: [...]` — if the branch whose test is *not* the one we fall
    /// through cannot fall through (either a non-local return `^` or a
    /// diverging call such as `self error: "..."` whose inferred type is
    /// `Never`), the variable is narrowed for subsequent statements. Covers
    /// both local variables and synthetic `self.field` keys via
    /// [`Self::resolve_narrowing_variable_type`] (BT-2049).
    pub(in crate::semantic_analysis::type_checker) fn apply_early_return_narrowing(
        &mut self,
        expr: &Expression,
        env: &mut TypeEnv,
        hierarchy: &ClassHierarchy,
    ) {
        // Match: `<receiver> ifTrue: [diverging]`, `<receiver> ifFalse:
        // [diverging]` (BT-2825), or `<receiver> ifTrue: [diverging]
        // ifFalse: [...]` — whichever block diverges, any execution reaching
        // the next statement came through the *other* path, narrowing the
        // variable accordingly.
        let Expression::MessageSend {
            receiver,
            selector: selector @ MessageSelector::Keyword(_),
            arguments,
            ..
        } = expr
        else {
            return;
        };
        let is_if_true = selector.well_known() == Some(WellKnownSelector::IfTrue);
        let is_if_false = selector.well_known() == Some(WellKnownSelector::IfFalse);
        let is_if_true_if_false = selector.well_known() == Some(WellKnownSelector::IfTrueIfFalse);
        if !(is_if_true || is_if_false || is_if_true_if_false) {
            return;
        }
        let Some(mut info) = Self::detect_narrowing(receiver) else {
            return;
        };
        if !info.is_nil_check
            && info.class_test.is_none()
            && info.singleton_eq.is_none()
            && info.responded_selector.is_none()
        {
            return;
        }
        if info.class_test.is_some() {
            // BT-2825: resolve `true_type`/`false_type` the same way the
            // primary `ifTrue:`/`ifFalse:` dispatch does
            // (`refine_class_narrowing`), but through the diagnostic-free
            // `compute_class_narrowing` — `expr` (this exact guard) was
            // already fully type-checked by `infer_stmts` above, so the
            // "comparison can never be true" hint already fired once.
            let current_ty = Self::resolve_narrowing_variable_type(
                &info.variable,
                env,
                hierarchy,
                self.alias_registry.as_ref(),
            );
            info = Self::compute_class_narrowing(
                info,
                &current_ty,
                hierarchy,
                self.protocol_registry.as_ref(),
            );
        }
        if info.singleton_eq.is_some() {
            // BT-3369: resolve `true_type`/`false_type` the same way the
            // primary `ifTrue:`/`ifFalse:` dispatch does, via the existing
            // (diagnostic-free) `refine_singleton_narrowing` — the "comparison
            // can never be true" hint for this guard already fired once
            // during `infer_stmts` above.
            info = Self::refine_singleton_narrowing(
                info,
                env,
                hierarchy,
                self.alias_registry.as_ref(),
            );
        }
        if info.responded_selector.is_some() {
            // BT-3369: resolve `true_type` (upgrading `Dynamic` to a concrete
            // `Protocol` type when exactly one protocol requires the tested
            // selector) via the existing `refine_responds_to_narrowing`.
            // `false_type` stays `None` — there is no sound type for "doesn't
            // respond to X" — so only the `ifFalse: [<diverge>]` guard shape
            // below can narrow post-guard for this kind.
            info = self.refine_responds_to_narrowing(info);
        }

        if is_if_true || is_if_true_if_false {
            // In both shapes, the true block is argument 0.
            let Some(Expression::Block(true_block)) = arguments.first() else {
                return;
            };
            if !self.block_diverges(true_block) {
                return;
            }
            // For `ifTrue:ifFalse:`, execution may reach the next
            // statement through the `ifFalse:` block. If that block
            // reassigns the tested variable (e.g. `[self.user := nil]`
            // or `[x := nil]`), the post-statement narrowing would be
            // unsound — skip it.
            if is_if_true_if_false {
                if let Some(Expression::Block(false_block)) = arguments.get(1) {
                    if block_may_reassign(false_block, &info.variable) {
                        return;
                    }
                }
            }
            let Some(narrowed) = Self::early_return_false_branch_type(
                &info,
                env,
                hierarchy,
                self.alias_registry.as_ref(),
            ) else {
                return;
            };
            // BT-2050: after this statement, the variable is narrowed.
            // Use method-remainder scope: the refinement outlives the
            // guard send and applies to the rest of the enclosing method
            // body (unlike the block-scoped narrowings pushed inside
            // `infer_block_with_narrowing`).
            env.push_refinement(RefinementLayer::method_remainder(
                info.variable.clone(),
                narrowed,
            ));
        } else if is_if_false {
            // BT-2825: `<receiver> ifFalse: [diverging]` — the sole
            // argument is the false block. If it diverges, execution
            // reaching the next statement proves the guard's test held,
            // so the variable narrows to the *true*-branch type.
            //
            // For `isKindOf:` this is `compute_class_narrowing`'s resolved
            // `true_type` (set above). For plain `isNil` checks, `info` was
            // never routed through `compute_class_narrowing` — `true_type`
            // instead comes straight from `detect_narrowing`'s `isNil` rule,
            // which sets it to `InferredType::known("UndefinedObject")`
            // (see `narrowing/rules/is_nil.rs`).
            let Some(Expression::Block(false_block)) = arguments.first() else {
                return;
            };
            if !self.block_diverges(false_block) {
                return;
            }
            env.push_refinement(RefinementLayer::method_remainder(
                info.variable.clone(),
                info.true_type.clone(),
            ));
        }
    }

    /// The type the tested variable takes in the "complementary" (false)
    /// branch of a narrowing, mirroring `infer_args_with_narrowing`'s
    /// `ifFalse:` arm (BT-2825): an explicit `false_type` (class test /
    /// Result / singleton) if set, else non-nil for `isNil` checks, else
    /// `None` (no useful narrowing — e.g. `class =:=`'s false branch, which
    /// deliberately stays unnarrowed per `ClassTestKind`).
    pub(in crate::semantic_analysis::type_checker) fn early_return_false_branch_type(
        info: &NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        alias_registry: Option<&AliasRegistry>,
    ) -> Option<InferredType> {
        if let Some(ref false_ty) = info.false_type {
            Some(false_ty.clone())
        } else if info.is_nil_check {
            let current_ty = Self::resolve_narrowing_variable_type(
                &info.variable,
                env,
                hierarchy,
                alias_registry,
            );
            Some(Self::non_nil_type(&current_ty))
        } else {
            None
        }
    }

    /// Infer a `value match: [...]` / `value matchExhaustive: [...]`
    /// expression as the union of its arm body types — the
    /// `Expression::Match` arm of `infer_expr`'s dispatch.
    ///
    /// `Never`-typed arms are eliminated from the union. A `nil` arm and an
    /// unguarded `binding :: ClassName` arm each narrow the scrutinee's
    /// residual type for subsequent arms (ADR 0107 Phase A, BT-2854/BT-2855).
    #[allow(clippy::too_many_arguments)] // split from infer_expr's dispatch, mirrors its arity
    pub(in crate::semantic_analysis::type_checker) fn infer_match(
        &mut self,
        value: &Expression,
        arms: &[MatchArm],
        exhaustive: bool,
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let scrutinee_ty = self.infer_expr(value, hierarchy, env, in_abstract_method);
        // BT-2745 / ADR 0102 §4 (advisory `Warning`) vs. BT-2763 /
        // ADR 0106 (opt-in asserted `Error`, `matchExhaustive:`).
        // Distinct from (and does not replace) BT-1299's
        // pattern-based sealed-constructor check, which still runs
        // separately in `validators::match_validators`.
        if exhaustive {
            self.check_asserted_match_exhaustiveness(&scrutinee_ty, arms, span, hierarchy);
        } else {
            self.check_singleton_match_exhaustiveness(&scrutinee_ty, arms, span, hierarchy);
        }

        // BT-2854 / ADR 0107 Phase A: a `nil` arm narrows the
        // scrutinee to `UndefinedObject` inside its own body (mirrors
        // `x isNil ifTrue:`), and — when unguarded — removes `Nil`
        // from what subsequent arms see (mirrors `x isNil ifFalse:`'s
        // `non_nil_type`), reusing the exact narrowing algebra
        // already established by `is_nil.rs`. A guarded `nil when:
        // [...] ->` arm does not guarantee coverage, so it does not
        // narrow the residual for later arms (same rule
        // `singleton_match_residual` uses for guarded arms). Only
        // applies when the scrutinee has a stable narrowable key
        // (`extract_variable_name`) — an arbitrary expression
        // scrutinee has nothing to narrow.
        let scrutinee_key = extract_variable_name(value.unwrap_parens());
        let mut residual_scrutinee_ty = scrutinee_ty.clone();

        let arm_types: Vec<InferredType> = arms
            .iter()
            .map(|arm| {
                let mut arm_env = env.child();
                Self::bind_pattern_vars(&arm.pattern, &mut arm_env);

                // BT-2855 / ADR 0107 Phase A: a `binding :: ClassName`
                // arm narrows `binding` to `ClassName`, computed the
                // same way `isKindOf:`'s true branch does
                // (`intersect_with_class`, shared with
                // `compute_class_narrowing`) — from the scrutinee's
                // *current* residual type, not the original
                // `scrutinee_ty`, so a `Type` arm after an unguarded
                // `nil ->`/`Type` arm sees the narrower residual.
                // Overwrites the `Dynamic` `bind_pattern_vars` (above)
                // just set for `binding`.
                let type_pattern_narrowed =
                    if let Pattern::Type { binding, class, .. } = &arm.pattern {
                        let narrowed = Self::intersect_with_class(
                            &residual_scrutinee_ty,
                            &class.name,
                            hierarchy,
                            self.protocol_registry.as_ref(),
                        );
                        arm_env.set_local(binding.name.clone(), narrowed.clone());
                        Some(narrowed)
                    } else {
                        None
                    };

                if let Some(key) = &scrutinee_key {
                    if matches!(arm.pattern, Pattern::Nil(_)) {
                        arm_env.set(
                            key.clone(),
                            InferredType::known(WellKnownClass::UndefinedObject.as_str()),
                        );
                    } else if let Some(narrowed) = &type_pattern_narrowed {
                        // The scrutinee variable (if it has a stable
                        // name) denotes the same value as `binding` —
                        // give it the identical narrowed type inside
                        // this arm, so `raw match: [path :: String ->
                        // raw ...]` sees `raw` narrowed too, not just
                        // `path`.
                        arm_env.set(key.clone(), narrowed.clone());
                    } else {
                        // Deliberately overwrites whatever
                        // `bind_pattern_vars` (above) just set for
                        // this key: today that's always `Dynamic` for
                        // a pattern-bound variable, so the residual
                        // here is strictly more precise.
                        arm_env.set(key.clone(), residual_scrutinee_ty.clone());
                    }
                }
                // Guard sees `binding`/scrutinee already narrowed
                // above (ADR 0107: "scope includes the arm's `when:`
                // guard, not just its body").
                if let Some(guard) = &arm.guard {
                    self.infer_expr(guard, hierarchy, &mut arm_env, in_abstract_method);
                }
                let body_ty =
                    self.infer_expr(&arm.body, hierarchy, &mut arm_env, in_abstract_method);
                if arm.guard.is_none() {
                    if matches!(arm.pattern, Pattern::Nil(_)) {
                        residual_scrutinee_ty = Self::non_nil_type(&residual_scrutinee_ty);
                    } else if let Pattern::Type { class, .. } = &arm.pattern {
                        // Unguarded `Type` arm guarantees coverage of
                        // `ClassName` — subsequent arms see the
                        // scrutinee narrowed by `\ ClassName` (ADR
                        // 0102 §5 nominal-class difference), mirroring
                        // `isKindOf:`'s false-branch narrowing.
                        residual_scrutinee_ty = InferredType::difference(
                            &residual_scrutinee_ty,
                            &InferredType::known(class.name.clone()),
                            crate::semantic_analysis::type_checker::TypeProvenance::Inferred(
                                Span::default(),
                            ),
                            Some(hierarchy),
                        );
                    }
                }
                body_ty
            })
            .collect();
        if arm_types.is_empty() {
            InferredType::Dynamic(DynamicReason::AmbiguousControlFlow)
        } else {
            InferredType::union_of(&arm_types)
        }
    }
}
