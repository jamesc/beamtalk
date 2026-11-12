// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Condition facts: what a Boolean condition proves about variables (BT-4710).
//!
//! [`TypeChecker::condition_facts`] is the one recursive function behind every
//! consumer of control-flow narrowing: the arms of `ifTrue:` / `ifFalse:` /
//! `ifTrue:ifFalse:`, the lazy block of `and:` / `or:`, and post-guard
//! (early-return) narrowing. For a condition it answers a [`CondFacts`]: the
//! [`Facts`] (variable → narrowed type) that hold when the condition is true,
//! and those that hold when it is false.
//!
//! * A single test (`isNil`, `notNil`, `isKindOf:`, …) is one entry of the
//!   [`super::rules::RULES`] table, refined against the live type.
//! * `a and: [b]` — true: both sides' true facts, with `b` checked under
//!   `a`'s true facts. False: the *join* of the two sides' false facts (one of
//!   them held, so only a variable both sides narrow stays narrowed, to the
//!   union of its types).
//! * `a or: [b]` — the dual.
//! * `c not` — `c`'s two fact sets, swapped.
//! * Anything else contributes no facts but never stops the rest of a chain
//!   from narrowing.
//!
//! New connectives (aliases, `whileTrue:` conditions) extend
//! `condition_facts` with one more case that builds a [`CondFacts`].

use crate::ast::Expression;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::source_analysis::Span;

use super::super::{EnvKey, InferredType, TypeChecker, TypeEnv};
use super::refinement::RefinementLayer;
use super::visitors::{Writes, expr_may_write_fields, expr_writes};

/// A set of `variable -> narrowed type` facts, one entry per variable.
#[derive(Debug, Clone, Default)]
pub(crate) struct Facts(Vec<(EnvKey, InferredType)>);

impl Facts {
    /// Whether no variable is narrowed.
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// The facts as `(key, type)` references, in insertion order.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&EnvKey, &InferredType)> {
        self.0.iter().map(|(key, ty)| (key, ty))
    }

    /// The narrowed variables.
    pub(crate) fn keys(&self) -> impl Iterator<Item = &EnvKey> {
        self.0.iter().map(|(key, _)| key)
    }

    /// Both sides hold: every fact of `self` and of `other`; `other` wins on a
    /// shared key (it was computed under `self`'s facts). A rule whose branch
    /// type ignores the live type (`isNil`, an unupgraded `respondsTo:`) can
    /// make that less precise than `self`'s, never unsound.
    fn merge(mut self, other: Self) -> Self {
        for (key, ty) in other.0 {
            if let Some(slot) = self.0.iter_mut().find(|(k, _)| *k == key) {
                slot.1 = ty;
            } else {
                self.0.push((key, ty));
            }
        }
        self
    }

    /// One side holds: only a variable narrowed by *both* stays narrowed, to
    /// the union of its two types. A variable missing on either side keeps its
    /// un-narrowed type, so it drops out.
    fn join(&self, other: &Self) -> Self {
        Self(
            self.0
                .iter()
                .filter_map(|(key, ty)| {
                    let (_, other_ty) = other.0.iter().find(|(k, _)| k == key)?;
                    Some((
                        key.clone(),
                        InferredType::union_of(&[ty.clone(), other_ty.clone()]),
                    ))
                })
                .collect(),
        )
    }

    /// Forget the facts about `keys` (`Writes::Unknown`: all of them).
    fn forget_written(&mut self, writes: &Writes) {
        match writes {
            Writes::Keys(keys) => self.0.retain(|(key, _)| !keys.contains(key)),
            Writes::Unknown => self.0.clear(),
        }
    }

    /// Forget every `self.<field>` fact (BT-4715: a self-send may reassign
    /// any field, so a fact about one cannot outlive it).
    pub(crate) fn drop_fields(&mut self) {
        self.0
            .retain(|(key, _)| !matches!(key, EnvKey::SelfField(_)));
    }
}

/// What a condition proves when it is true and when it is false.
#[derive(Debug, Clone, Default)]
pub(crate) struct CondFacts {
    /// Facts that hold when the condition answered `true`.
    pub(crate) when_true: Facts,
    /// Facts that hold when the condition answered `false`.
    pub(crate) when_false: Facts,
}

impl CondFacts {
    /// Drop `self.<field>` facts from both sides (see [`Facts::drop_fields`]).
    pub(crate) fn drop_fields(&mut self) {
        self.when_true.drop_fields();
        self.when_false.drop_fields();
    }
}

/// Which consumer is asking for facts.
#[derive(Debug, Clone, Copy)]
pub(crate) struct FactsCtx {
    /// Post-guard narrowing (`x isNil ifTrue: [^…]` narrows the *rest of the
    /// method*): Result `isOk` / `isError` tests are skipped, since their
    /// "narrowing" only keeps the variable's type for the arm dispatch.
    post_guard: bool,
    /// Where to report "comparison can never be true" for a class test.
    /// `Some` only at the root of a receiver being type-checked for the first
    /// time; `None` when the condition was already checked (post-guard) or is
    /// nested inside a chain. A chain's left operand is reported by its own
    /// `and:` / `or:` send; a test inside a right-hand block is not reported
    /// at all (that shape was never narrowed before BT-4710).
    hint_span: Option<Span>,
}

impl FactsCtx {
    /// Facts for the arms of the send whose receiver is the condition.
    pub(crate) fn arm(hint_span: Span) -> Self {
        Self {
            post_guard: false,
            hint_span: Some(hint_span),
        }
    }

    /// Facts for the statements after a diverging guard.
    pub(crate) fn post_guard() -> Self {
        Self {
            post_guard: true,
            hint_span: None,
        }
    }

    /// The context for a sub-condition of a chain.
    fn nested(self) -> Self {
        Self {
            hint_span: None,
            ..self
        }
    }
}

/// Whether a send is a lazy Boolean connective: `Some(true)` for `and:`,
/// `Some(false)` for `or:`. `and:` / `or:` stay string comparisons rather than
/// `WellKnownSelector` variants — they are ordinary self-hosted `Boolean`
/// methods (`boolean.bt`), not type-checker intrinsics.
fn connective_kind(selector_name: &str) -> Option<bool> {
    match selector_name {
        "and:" => Some(true),
        "or:" => Some(false),
        _ => None,
    }
}

/// The sole expression of a parameterless block literal; `None` for anything
/// else (a multi-statement block, a block with parameters, a non-block).
fn single_expression_block_body(arg: &Expression) -> Option<&Expression> {
    let Expression::Block(block) = arg else {
        return None;
    };
    match (block.parameters.as_slice(), block.body.as_slice()) {
        ([], [stmt]) => Some(stmt.expression.unwrap_parens()),
        _ => None,
    }
}

impl TypeChecker {
    /// The facts `cond` proves when true and when false — see the module docs.
    pub(in crate::semantic_analysis::type_checker) fn condition_facts(
        &mut self,
        cond: &Expression,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        ctx: FactsCtx,
    ) -> CondFacts {
        // A send's receiver is already peeled by `ast::normalize`; a block's
        // statement (below) is not.
        if let Some(info) = self.detect_refined_narrowing(cond, env, hierarchy, ctx.hint_span) {
            // Post-guard narrowing has always been limited to nil, class,
            // singleton and `respondsTo:` tests.
            if ctx.post_guard
                && !info.is_nil_check
                && info.class_test.is_none()
                && info.singleton_eq.is_none()
                && info.responded_selector.is_none()
            {
                return CondFacts::default();
            }
            let when_false = Self::early_return_false_branch_type(
                &info,
                env,
                hierarchy,
                self.alias_registry.as_deref(),
                self.protocol_registry.as_deref(),
            )
            .map(|ty| Facts(vec![(info.variable.clone(), ty)]))
            .unwrap_or_default();
            return CondFacts {
                when_true: Facts(vec![(info.variable, info.true_type)]),
                when_false,
            };
        }

        let Expression::MessageSend {
            receiver: lhs,
            selector,
            arguments,
            ..
        } = cond
        else {
            return CondFacts::default();
        };
        // `c not`: the same facts, swapped. The receiver is already peeled.
        if arguments.is_empty() && selector.name() == "not" {
            let CondFacts {
                when_true,
                when_false,
            } = self.condition_facts(lhs, env, hierarchy, ctx.nested());
            return CondFacts {
                when_true: when_false,
                when_false: when_true,
            };
        }
        let (Some(is_and), [rhs]) = (connective_kind(&selector.name()), arguments.as_slice())
        else {
            return CondFacts::default();
        };

        let mut left = self.condition_facts(lhs, env, hierarchy, ctx.nested());
        // The block runs only when `lhs` answered true (`and:`) / false
        // (`or:`): check it under those facts.
        let assumed = if is_and {
            &left.when_true
        } else {
            &left.when_false
        };
        // Only a block holding exactly one expression is understood: any
        // other statement (notably an assignment to a tested variable) would
        // invalidate the proof.
        let mut right = match single_expression_block_body(rhs) {
            Some(body) => {
                let mut rhs_env = env.child();
                for (key, ty) in assumed.iter() {
                    rhs_env.push_refinement(RefinementLayer::block_scope(key.clone(), ty.clone()));
                }
                self.condition_facts(body, &rhs_env, hierarchy, ctx.nested())
            }
            None => CondFacts::default(),
        };

        // Whatever the right-hand argument may assign, or a field a self-send
        // in it may reassign, is no longer what either side proved — whether
        // or not the argument was understood. Conservatively applied to both
        // sides' facts (an `or:` skips the argument when the left side held,
        // but we cannot tell which path reached the result).
        let writes = expr_writes(rhs);
        // A block *value* (a local holding a block, a parameter, a send's
        // answer) can write actor fields live wherever it runs (ADR 0137), so
        // only a block literal's own body can be ruled out by reading it.
        let self_send = !matches!(rhs, Expression::Block(_))
            || expr_may_write_fields(rhs, &self.type_map, self.protocol_registry.as_deref());
        for side in [&mut left, &mut right] {
            for facts in [&mut side.when_true, &mut side.when_false] {
                facts.forget_written(&writes);
                if self_send {
                    facts.drop_fields();
                }
            }
        }

        if is_and {
            CondFacts {
                when_true: left.when_true.merge(right.when_true),
                when_false: left.when_false.join(&right.when_false),
            }
        } else {
            CondFacts {
                when_true: left.when_true.join(&right.when_true),
                when_false: left.when_false.merge(right.when_false),
            }
        }
    }

    /// BT-4713: the facts the body of `[cond] whileTrue: [body]` (or
    /// `whileFalse:`) may assume on entry — `cond`'s last expression's true
    /// (false) facts. `None` when the send is not such a loop or nothing is
    /// proved.
    ///
    /// The condition is re-evaluated before every iteration, so the facts hold
    /// at the start of each pass; they are kept only for bindings the body
    /// never assigns. `self.<field>` facts are not applied (see below).
    pub(in crate::semantic_analysis::type_checker) fn loop_body_facts(
        &mut self,
        receiver: &Expression,
        selector: &crate::ast::MessageSelector,
        arguments: &[Expression],
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        span: Span,
    ) -> Option<Facts> {
        use crate::ast::WellKnownSelector;
        let on_true = match selector.well_known() {
            Some(WellKnownSelector::WhileTrue) => true,
            Some(WellKnownSelector::WhileFalse) => false,
            _ => return None,
        };
        let [body] = arguments else {
            return None;
        };
        let Expression::Block(cond_block) = receiver else {
            return None;
        };
        if !cond_block.parameters.is_empty() {
            return None;
        }
        let last = cond_block.body.last()?.expression.unwrap_parens();
        let facts = self.condition_facts(last, env, hierarchy, FactsCtx::arm(span).nested());
        let mut facts = if on_true {
            facts.when_true
        } else {
            facts.when_false
        };
        facts.forget_written(&expr_writes(body));
        // Field facts never reach a loop body: deciding whether the body can
        // write a field needs its types (`expr_may_write_fields` reads the
        // type map), and the body has not been inferred yet. Dropping them is
        // the safe direction.
        facts.drop_fields();
        (!facts.is_empty()).then_some(facts)
    }
}
