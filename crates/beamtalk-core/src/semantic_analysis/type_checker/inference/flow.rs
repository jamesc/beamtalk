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
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::semantic_analysis::type_checker::narrowing::NarrowingInfo;
use crate::semantic_analysis::type_checker::narrowing::extract::extract_variable_name;
use crate::semantic_analysis::type_checker::narrowing::facts::{Facts, FactsCtx};
use crate::semantic_analysis::type_checker::narrowing::refinement::RefinementLayer;
use crate::semantic_analysis::type_checker::narrowing::visitors::{
    block_has_return, block_may_reassign, expr_contains_returning_self_send,
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

    /// Bind the variables of a destructuring assignment, typing flat tuple
    /// patterns from the right-hand side's positional type (BT-4487).
    ///
    /// A plain-variable element of a `{a, b}` pattern is typed like
    /// `t at: i` would be (see `infer_literal_index_tuple_at`), except that an
    /// `Object`, `Dynamic` or unresolved-generic position binds as
    /// `Dynamic(Unknown)`: `Object` is never a useful typed-send target.
    ///
    /// `Nil` is dropped from the right-hand side (a nil value raises
    /// `#pattern_mismatch`). The result must be a `Tuple(..)` whose arity
    /// equals the pattern's, or a union whose every member is one; element
    /// types are then the per-position union. Anything else (bare `Tuple`,
    /// arity mismatch, mixed union, non-tuple patterns) falls back to
    /// [`Self::bind_pattern_vars`]. Wildcard and literal elements bind nothing.
    pub(in crate::semantic_analysis::type_checker) fn bind_destructure_vars(
        pattern: &Pattern,
        value_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
    ) {
        // Variables are bound by the fallback first so every name is
        // defined; typed positions then overwrite their entry.
        Self::bind_pattern_vars(pattern, env);
        let Pattern::Tuple { elements, .. } = pattern else {
            return;
        };
        let Some(positions) = Self::tuple_positions(&Self::non_nil_type(value_ty), elements.len())
        else {
            return;
        };
        for (element, ty) in elements.iter().zip(positions) {
            if let Pattern::Variable(id) = element {
                let bound = if Self::is_dynamic_position(&ty, hierarchy) {
                    InferredType::Dynamic(DynamicReason::Unknown)
                } else {
                    ty
                };
                env.set_local(id.name.clone(), bound);
            }
        }
    }

    /// Whether a destructured tuple position must bind as `Dynamic`: it is
    /// `Dynamic`, bare `Object`, or an unresolved type variable — or, for a
    /// union column, any member is (BT-4628).
    fn is_dynamic_position(ty: &InferredType, hierarchy: &ClassHierarchy) -> bool {
        match ty {
            InferredType::Dynamic(_) => true,
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                (class_name == "Object" && type_args.is_empty())
                    || super::super::types::is_type_var_placeholder(class_name, hierarchy)
            }
            InferredType::Union { members, .. } => members
                .iter()
                .any(|m| Self::is_dynamic_position(m, hierarchy)),
            _ => false,
        }
    }

    /// Per-position element types of `ty` if it is a `Tuple(..)` of exactly
    /// `arity` positions, or a union of only such tuples (element-wise union).
    fn tuple_positions(ty: &InferredType, arity: usize) -> Option<Vec<InferredType>> {
        match ty {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } if class_name == "Tuple" && type_args.len() == arity && arity > 0 => {
                Some(type_args.clone())
            }
            InferredType::Union { members, .. } => {
                let rows: Vec<Vec<InferredType>> = members
                    .iter()
                    .map(|m| Self::tuple_positions(m, arity))
                    .collect::<Option<_>>()?;
                Some(
                    (0..arity)
                        .map(|i| {
                            let col: Vec<InferredType> =
                                rows.iter().map(|r| r[i].clone()).collect();
                            InferredType::union_of(&col)
                        })
                        .collect(),
                )
            }
            _ => None,
        }
    }

    /// Check whether a block's execution cannot fall through to the enclosing
    /// method's next statement.
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
    ///   in a method-send argument). This walks descendants via
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
    /// inferred type [`InferredType::Never`] in the type map.
    ///
    /// This is the `Never`-typed companion to
    /// [`crate::semantic_analysis::type_checker::narrowing::visitors::expr_contains_return`]: both share the
    /// exhaustive [`crate::ast::visitor`] walker so every sub-expression
    /// variant gets covered. A diverging call such as
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
            // `visit_block` default (opaque) preserves the rule that
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
    /// — `resolve_type_keyword` should canonicalize to
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
                // `P = UndefinedObject | Nil` (both nil spellings).
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

    /// [`Self::resolve_narrowing_variable_type`] with this checker's alias and
    /// protocol registries.
    pub(in crate::semantic_analysis::type_checker) fn narrowing_variable_type(
        &self,
        var_key: &EnvKey,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        Self::resolve_narrowing_variable_type(
            var_key,
            env,
            hierarchy,
            self.alias_registry.as_deref(),
            self.protocol_registry.as_deref(),
        )
    }

    /// Resolve the current type of a narrowing variable from the environment.
    ///
    /// For locals, this is a simple env lookup. For
    /// [`EnvKey::SelfField`] we prefer a previously
    /// pushed narrowing, falling back to the declared state type resolved
    /// through the class hierarchy when none is present.
    ///
    /// `alias_registry` (ADR 0108) is threaded
    /// through so a `self.field isNil ifFalse: [...]` narrowing check on a
    /// cross-file alias-typed field expands the alias instead of falling
    /// back to an opaque nominal class; `protocol_registry` so a
    /// `P class | Nil` field resolves to a protocol metatype (ADR 0144).
    pub(in crate::semantic_analysis::type_checker) fn resolve_narrowing_variable_type(
        var_key: &EnvKey,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        alias_registry: Option<&AliasRegistry>,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> InferredType {
        // Regular env lookup first (handles locals and previously-narrowed fields).
        if let Some(ty) = env.get(var_key) {
            return ty;
        }
        // For `self.<field>` keys, resolve via the class
        // hierarchy using the `self` binding in the env.
        if let EnvKey::SelfField(field_name) = var_key {
            if let Some(InferredType::Known { class_name, .. }) = env.get_local("self") {
                if let Some(field_type) = hierarchy.state_field_type(&class_name, field_name) {
                    return type_resolver::resolve_declared_type(
                        &field_type,
                        &type_resolver::SubstitutionMap::new(),
                        protocol_registry,
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
    /// Detects `<cond> ifTrue: [<diverge>]` / `ifFalse: [<diverge>]` /
    /// `ifTrue: [<diverge>] ifFalse: [...]` (and the dual) — if the branch
    /// whose test is *not* the one we fall through cannot fall through (either
    /// a non-local return `^` or a diverging call such as `self error: "..."`
    /// whose inferred type is `Never`), the facts the condition proves on the
    /// other path (BT-4710: [`Self::condition_facts`], so `isNil`, `notNil`,
    /// `isKindOf:`, … and `and:` / `or:` chains of them) narrow the rest of
    /// the method. Covers both local variables and synthetic `self.field`
    /// keys via [`Self::resolve_narrowing_variable_type`].
    pub(in crate::semantic_analysis::type_checker) fn apply_early_return_narrowing(
        &mut self,
        expr: &Expression,
        env: &mut TypeEnv,
        hierarchy: &ClassHierarchy,
    ) {
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
        let mut facts = self.condition_facts(receiver, env, hierarchy, FactsCtx::post_guard());
        if facts.when_true.is_empty() && facts.when_false.is_empty() {
            return;
        }
        // BT-4715: don't re-add a field narrowing a fall-through self-send undid.
        if self.fallthrough_may_self_send(receiver, arguments) {
            facts.drop_fields();
        }

        if is_if_true || is_if_true_if_false {
            // In both shapes, the true block is argument 0.
            let Some(Expression::Block(true_block)) = arguments.first() else {
                return;
            };
            if !self.block_diverges(true_block) {
                // BT-4199: the dual `ifTrue: [...] ifFalse: [diverging]` shape —
                // reaching the next statement proves the condition held.
                if is_if_true_if_false {
                    self.narrow_after_diverging_false_block(&facts.when_true, arguments, env);
                }
                return;
            }
            // For `ifTrue:ifFalse:`, execution may reach the next
            // statement through the `ifFalse:` block. If that block
            // reassigns a tested variable (e.g. `[self.user := nil]`
            // or `[x := nil]`), the post-statement narrowing would be
            // unsound — skip it.
            if is_if_true_if_false {
                if let Some(Expression::Block(false_block)) = arguments.get(1) {
                    if facts
                        .when_false
                        .keys()
                        .any(|key| block_may_reassign(false_block, key))
                    {
                        return;
                    }
                }
            }
            // The true block diverges, so any execution reaching the next
            // statement came through the false path.
            Self::push_method_remainder(&facts.when_false, env);
        } else if is_if_false {
            // `<receiver> ifFalse: [diverging]` — reaching the next statement
            // proves the condition held.
            let Some(Expression::Block(false_block)) = arguments.first() else {
                return;
            };
            if self.block_diverges(false_block) {
                Self::push_method_remainder(&facts.when_true, env);
            }
        }
    }

    /// Narrow the rest of the enclosing method by `facts`. Unlike the
    /// block-scoped narrowings pushed inside `infer_block_with_narrowings`,
    /// the refinement outlives the guard send.
    fn push_method_remainder(facts: &Facts, env: &mut TypeEnv) {
        for (key, ty) in facts.iter() {
            env.push_refinement(RefinementLayer::method_remainder(key.clone(), ty.clone()));
        }
    }

    /// BT-4715: can a send to `self`/`super` run on a path that falls through
    /// to the statement after the guard `<receiver> if…: arguments`? That is
    /// the tested expression itself (`(self.f isNil or: [self isStale])`) or
    /// any branch block that does not diverge. A diverging branch never
    /// reaches the next statement, so a self-send in it (`[self log. ^0]`)
    /// leaves the post-guard narrowing intact.
    fn fallthrough_may_self_send(&self, receiver: &Expression, arguments: &[Expression]) -> bool {
        expr_contains_returning_self_send(receiver, &self.type_map)
            || arguments.iter().any(|arg| match arg {
                Expression::Block(block) => {
                    !self.block_diverges(block)
                        && expr_contains_returning_self_send(arg, &self.type_map)
                }
                other => expr_contains_returning_self_send(other, &self.type_map),
            })
    }

    /// BT-4199: `<test> ifTrue: [...] ifFalse: [diverging]` — reaching the
    /// next statement proves the test held, so narrow by the facts true of it
    /// (unless the true block reassigns a tested variable).
    fn narrow_after_diverging_false_block(
        &self,
        when_true: &Facts,
        arguments: &[Expression],
        env: &mut TypeEnv,
    ) {
        let (Some(Expression::Block(true_block)), Some(Expression::Block(false_block))) =
            (arguments.first(), arguments.get(1))
        else {
            return;
        };
        if self.block_diverges(false_block)
            && !when_true
                .keys()
                .any(|key| block_may_reassign(true_block, key))
        {
            Self::push_method_remainder(when_true, env);
        }
    }

    /// The type the tested variable takes in the "complementary" (false)
    /// branch of a narrowing, mirroring `infer_args_with_narrowing`'s
    /// `ifFalse:` arm: an explicit `false_type` (class test /
    /// Result / singleton) if set, else non-nil for `isNil` checks, else
    /// `None` (no useful narrowing — e.g. `class =:=`'s false branch, which
    /// deliberately stays unnarrowed per `ClassTestKind`).
    pub(in crate::semantic_analysis::type_checker) fn early_return_false_branch_type(
        info: &NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        alias_registry: Option<&AliasRegistry>,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> Option<InferredType> {
        if let Some(ref false_ty) = info.false_type {
            Some(false_ty.clone())
        } else if info.is_nil_check {
            let current_ty = Self::resolve_narrowing_variable_type(
                &info.variable,
                env,
                hierarchy,
                alias_registry,
                protocol_registry,
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
    /// residual type for subsequent arms (ADR 0107 Phase A).
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
        // ADR 0102 §4 (advisory `Warning`) vs.
        // ADR 0106 (opt-in asserted `Error`, `matchExhaustive:`).
        // Distinct from (and does not replace) the
        // pattern-based sealed-constructor check, which still runs
        // separately in `validators::match_validators`.
        if exhaustive {
            self.check_asserted_match_exhaustiveness(&scrutinee_ty, arms, span, hierarchy);
        } else {
            self.check_singleton_match_exhaustiveness(&scrutinee_ty, arms, span, hierarchy);
        }

        // ADR 0107 Phase A: a `nil` arm narrows the
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

                // ADR 0107 Phase A: a `binding :: ClassName`
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
                            self.protocol_registry.as_deref(),
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
                // BT-4650: widenings queued by the arm guard and body name a local in
                // `arm_env`, where a pattern variable may shadow an outer
                // local of the same name. Collect them in a frame of their
                // own and forward only those that name the outer binding.
                let outer_deferred = std::mem::take(&mut self.deferred_block_widenings);
                if let Some(guard) = &arm.guard {
                    self.infer_expr(guard, hierarchy, &mut arm_env, in_abstract_method);
                }
                let body_ty =
                    self.infer_expr(&arm.body, hierarchy, &mut arm_env, in_abstract_method);
                let arm_deferred =
                    std::mem::replace(&mut self.deferred_block_widenings, outer_deferred);
                let (bound, _) = crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
                self.deferred_block_widenings.extend(
                    arm_deferred
                        .into_iter()
                        .filter(|w| !bound.iter().any(|id| id.name == w.name)),
                );
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
                // BT-4458: an arm whose body is a bare `^expr` never produces a
                // value for the match — it leaves the enclosing method — so it
                // contributes `Never`, not `expr`'s type. A method whose tail is
                // this match gets those `^` types back in its return type: see
                // [`Self::tail_match_return_types`].
                if Self::is_return_arm(arm) {
                    InferredType::Never
                } else {
                    body_ty
                }
            })
            .collect();
        if arm_types.is_empty() {
            InferredType::Dynamic(DynamicReason::AmbiguousControlFlow)
        } else {
            InferredType::union_of(&arm_types)
        }
    }

    /// BT-4458: is `arm`'s body a bare `^expr` (possibly parenthesised)?
    fn is_return_arm(arm: &MatchArm) -> bool {
        matches!(arm.body.unwrap_parens(), Expression::Return { .. })
    }

    /// BT-4458: the types of the `^` arms of a `match:` that is a method
    /// body's last statement.
    ///
    /// Method return inference reads only the last statement's type, and
    /// [`Self::infer_match`] gives `^` arms `Never`, so without these the
    /// `^` values would drop out of the method's return type: a mismatch
    /// against a declared `-> T` would go unreported, and an all-`^` tail
    /// would infer `-> Never` and make sends to the method count as diverging.
    pub(in crate::semantic_analysis::type_checker) fn tail_match_return_types(
        &self,
        body: &[crate::ast::ExpressionStatement],
    ) -> Vec<InferredType> {
        let Some(Expression::Match { arms, .. }) = body.last().map(|s| &s.expression) else {
            return Vec::new();
        };
        arms.iter()
            .filter(|arm| Self::is_return_arm(arm))
            // `type_map` never records `Dynamic(Unknown)`; an arm with no entry is
            // that, not an arm to drop (dropping it would leave an all-`^` tail
            // as `Never`).
            .map(|arm| {
                self.type_map
                    .get(arm.body.span())
                    .cloned()
                    .unwrap_or(InferredType::Dynamic(DynamicReason::Unknown))
            })
            .collect()
    }
}
