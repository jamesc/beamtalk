// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Guard-style early returns as direct returns (BT-4415).
//!
//! **DDD Context:** Code Generation
//!
//! A `^` whose enclosing block is an *inlined* conditional at the method's
//! top level is a plain early return of the method: the block is lowered
//! into a `case` arm of the method body, never a closure. The NLR recognizer
//! ([`body_has_block_nlr`]) nonetheless counts it as a `^` inside a block,
//! so the method pays for a non-local-return frame — `make_ref/0`, a `try`
//! around the whole body and a `throw` for the `^` — on every call, taken
//! or not. The guard idiom the language guide teaches,
//!
//! ```beamtalk
//! canon: bytes =>
//!   bytes isEmpty ifTrue: [^bytes]
//!   bytes reversed
//! ```
//!
//! is the expression form
//!
//! ```beamtalk
//! canon: bytes =>
//!   bytes isEmpty ifTrue: [bytes] ifFalse: [bytes reversed]
//! ```
//!
//! which compiles to a `case` and needs no frame. [`rewrite_guard_returns`]
//! performs that rewrite on codegen's own clone of the module, so every
//! method kind (Actor, Value, Object, class-side)
//! reaches the existing inlined-conditional lowering — and with it
//! `ThreadedIr` and its `verify()` for state threading — rather than a
//! second, per-method-kind early-return lowering.
//!
//! # The rewrite
//!
//! A method body is a *tail body*: the value of its last statement is the
//! method's result. Over a tail body:
//!
//! - **Guard.** A non-last statement that is an inlined conditional with
//!   literal block arguments ([`is_inlined_conditional_send`], less
//!   `and:`/`or:`), where exactly one branch slot *falls through* (is absent,
//!   or a block whose last statement is not a `^`) and every other branch
//!   ends in a `^`, absorbs the statements after it: they become the tail of
//!   the falling branch (a fresh block when the slot was absent), and the
//!   conditional becomes the body's last statement.
//! - **Tail.** In tail position a `^ v` is `v`; an inlined conditional's
//!   branch bodies are tail bodies; a `match:` arm body `^ v` is `v`.
//!
//! A falling branch with block parameters (`ifNotNil: [:x | …]`) never
//! absorbs the statements after it, since its parameter would shadow a name
//! they use.
//!
//! # Non-Boolean receivers
//!
//! The nil family (`ifNil:`, `ifNotNil:` and their two-branch forms) lowers
//! to a total `case` on `nil` with no send, so the two forms always agree.
//!
//! The Boolean family does not. On a receiver that is neither `true` nor
//! `false` the inlined conditional sends its own selector, with its branches
//! as closures (BT-4244): the guard as written sends `ifTrue:` with a block
//! that returns from the method when called, and its rewrite would send
//! `ifTrue:ifFalse:`. A Boolean-family conditional is therefore rewritten
//! only when:
//!
//! - its receiver is a Boolean by construction, so the send never happens:
//!   `true`, `false`, a structural BIF comparison such as `=:=`
//!   ([`is_boolean_by_construction`]), or an ordering comparison
//!   (`< > <= >=`) codegen lowers to a bare BIF because its left operand is
//!   a number/string/character literal ([`Rewriter::typed_comparison`],
//!   BT-4452). The rewrite records each such comparison, and the compile
//!   fails unless codegen did
//!   lower it bare ([`CoreErlangGenerator::unconfirmed_boolean_comparisons`]);
//!   or an `and:`/`or:` of Booleans by construction with a literal
//!   one-statement block, which inlines to a `case` that sends only for a
//!   non-Boolean receiver (BT-4496);
//!   or
//! - it is a one-branch guard, `c ifTrue: [^v]` or `c ifFalse: [^v]`, whose
//!   `v` has no effects ([`is_effect_free`]), or whose receiver `c` is a
//!   comparison on a *declared* parameter ([`Rewriter::is_declared_boolean`],
//!   BT-4823): its kind guard answers a Boolean while the declaration holds,
//!   and only a receiver that broke it can reach the helper, so any block
//!   ending in `^` is accepted. A declared comparison is not a Boolean by
//!   construction, so the other shapes keep their NLR frame, except a tail
//!   two-branch conditional with plain values (see below). Its receiver is then recorded
//!   in [`GuardRewrite::conditions`], and codegen
//!   ([`CoreErlangGenerator::guard_condition_doc`]) decides the branch with
//!   `beamtalk_guard_return`, which sends a non-Boolean the guard's own
//!   selector with a block whose call it catches. A taken `^` returns `v` as
//!   before; `v` having no effects, it does not matter that it is evaluated
//!   after the receiver's method has unwound rather than inside it.
//!
//! # Tail two-branch conditionals (BT-4998)
//!
//! `c ifTrue: [^a] ifFalse: [^b]` (or `ifFalse:ifTrue:`) as the last statement
//! of a tail body, with `a` and `b` literals or variables, is rewritten to
//! `c ifTrue: [a] ifFalse: [b]` on **any** receiver. A Boolean takes a plain
//! `case` arm. A non-Boolean receiver is still sent the selector as written,
//! by `beamtalk_guard_return:two_branch/3`, whose blocks return the values
//! from the method through a frame the helper owns, so the method has none.
//! Values with a send or a field read keep the method's frame: a receiver's
//! own `ifTrue:ifFalse:` would evaluate them inside the block it calls.
//!
//! # When it applies
//!
//! Only to a method that holds a block NLR as written and none once
//! rewritten, so the frame is gone entirely: a method that keeps a `^`
//! inside a real closure (`do:`, `collect:`, a stored block) keeps its frame
//! and is compiled exactly as written. Facts derived from the tree
//! ([`SemanticFacts`](beamtalk_core::semantic_analysis::SemanticFacts)) are
//! recomputed by the caller over the rewritten module; the original bodies
//! are returned so a method's reflected source and xref stay as written.

use std::collections::{HashMap, HashSet};

use beamtalk_core::ast::{
    Block, Expression, ExpressionStatement, KeywordPart, MessageSelector, MethodDefinition, Module,
};
use beamtalk_core::ast_walker::walk_expression;
use beamtalk_core::semantic_analysis::facts::body_has_block_nlr;
use beamtalk_core::source_analysis::Span;
use beamtalk_core::state_threading_selectors::{
    StructuralBinary, is_inlined_conditional_send, structural_binary,
};
use ecow::EcoString;

use crate::core_erlang::field_types::{ComparisonKinds, trusted_param_types};
use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::operators::{
    ProvenKind, comparison_is_bare, is_ordering_comparison, proven_comparison_kind,
};
use beamtalk_cerl_doc::{Document, docvec, leaf};

/// A method's identity within one generation: its span, side and selector. A
/// protocol provision flattened in from another file keeps that file's spans,
/// so the span alone could coincide with one of this file's methods.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::core_erlang) struct MethodKey {
    span: Span,
    is_class_method: bool,
    selector: EcoString,
}

impl MethodKey {
    pub(in crate::core_erlang) fn of(method: &MethodDefinition) -> Self {
        Self {
            span: method.span,
            is_class_method: method.is_class_method,
            selector: method.selector.name(),
        }
    }
}

/// Which branch a recorded guard's condition picks
/// (`beamtalk_guard_return:if_true/1` or `if_false/1`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum GuardSense {
    /// `c ifTrue: [^v]. rest`, rewritten `c ifTrue: [v] ifFalse: [rest]`.
    IfTrue,
    /// `c ifFalse: [^v]. rest`, rewritten `c ifTrue: [rest] ifFalse: [v]`.
    IfFalse,
}

impl GuardSense {
    /// Every sense, for the runtime export contract
    /// (`runtime_export_contract_tests`). The exhaustive `match` below makes
    /// a new sense a compile error here until it is listed.
    #[cfg(test)]
    pub(in crate::core_erlang) const ALL: [Self; 2] = {
        match Self::IfTrue {
            Self::IfTrue | Self::IfFalse => {}
        }
        [Self::IfTrue, Self::IfFalse]
    };

    /// The `beamtalk_guard_return` function this sense calls.
    pub(in crate::core_erlang) fn helper(self) -> &'static str {
        match self {
            Self::IfTrue => "if_true",
            Self::IfFalse => "if_false",
        }
    }
}

/// A recorded guard condition: its method and its receiver's span.
pub(in crate::core_erlang) type GuardConditionKey = (MethodKey, Span);

/// What [`rewrite_guard_returns`] changed.
#[derive(Debug, Default)]
pub(in crate::core_erlang) struct GuardRewrite {
    /// The bodies, as written, of the methods it rewrote.
    pub(in crate::core_erlang) bodies: HashMap<MethodKey, Vec<ExpressionStatement>>,
    /// The receivers of the guards whose condition codegen must decide
    /// through `beamtalk_guard_return`.
    pub(in crate::core_erlang) conditions: HashMap<GuardConditionKey, GuardSense>,
    /// The left operands of the typed comparisons the rewrite took for
    /// Booleans by construction ([`Rewriter::typed_comparison`]), which
    /// codegen must lower to a bare comparison BIF
    /// ([`CoreErlangGenerator::record_comparison_lowering`]).
    pub(in crate::core_erlang) boolean_comparisons: HashSet<GuardConditionKey>,
    /// The tail two-branch conditionals (BT-4998) whose non-Boolean fallback
    /// codegen must send through `beamtalk_guard_return:two_branch/3`, keyed
    /// by the span of the earlier of the two branch blocks.
    pub(in crate::core_erlang) two_branch: HashSet<GuardConditionKey>,
}

/// The `beamtalk_guard_return` function a tail two-branch conditional's
/// non-Boolean fallback calls (BT-4998).
pub(in crate::core_erlang) const TWO_BRANCH_HELPER: &str = "two_branch";

impl GuardRewrite {
    #[cfg(test)]
    pub(in crate::core_erlang) fn is_empty(&self) -> bool {
        self.bodies.is_empty()
    }
}

/// Rewrites, in place, every method of `module` whose guard-style `^`s are
/// its only block NLRs (see the module doc).
pub(in crate::core_erlang) fn rewrite_guard_returns(module: &mut Module) -> GuardRewrite {
    let mut rewrite = GuardRewrite::default();
    // The driver folds a same-file `X >> m` into X before this runs (BT-4721)
    // and rejects any other standalone method (ADR 0152), so the methods of
    // the module's classes are all there is to rewrite.
    for method in methods_mut(module) {
        let mut rw = Rewriter::of(method);
        if let Some(body) = rewritten_body(&method.body, &mut rw) {
            let original = std::mem::replace(&mut method.body, body);
            let key = MethodKey::of(method);
            rewrite.conditions.extend(
                rw.conditions
                    .into_iter()
                    .map(|(receiver, sense)| ((key.clone(), receiver), sense)),
            );
            rewrite.boolean_comparisons.extend(
                rw.boolean_comparisons
                    .into_iter()
                    .map(|left| (key.clone(), left)),
            );
            rewrite
                .two_branch
                .extend(rw.two_branch.into_iter().map(|span| (key.clone(), span)));
            rewrite.bodies.insert(key, original);
        }
    }
    rewrite
}

/// Whether `module` has a method [`rewrite_guard_returns`] would change, so
/// a caller can skip cloning the module when it has none.
pub(in crate::core_erlang) fn has_guard_returns(module: &Module) -> bool {
    module
        .classes
        .iter()
        .flat_map(|class| class.methods.iter().chain(class.class_methods.iter()))
        .any(|method| rewritten_body(&method.body, &mut Rewriter::of(method)).is_some())
}

fn methods_mut(module: &mut Module) -> impl Iterator<Item = &mut MethodDefinition> {
    module.classes.iter_mut().flat_map(|class| {
        class
            .methods
            .iter_mut()
            .chain(class.class_methods.iter_mut())
    })
}

/// One method's rewrite: what it records, and what it needs to know about
/// the method's locals.
struct Rewriter {
    /// The receiver spans of the method's helper-decided guards.
    conditions: HashMap<Span, GuardSense>,
    /// The left-operand spans of the typed comparisons the rewrite relied on
    /// as Booleans by construction.
    boolean_comparisons: HashSet<Span>,
    /// The first-block spans of the tail two-branch conditionals the rewrite
    /// left to `beamtalk_guard_return:two_branch/3` (BT-4998).
    two_branch: HashSet<Span>,
    /// The method's parameters whose comparison kind is only *declared*
    /// ([`trusted_param_types`] with a [`ComparisonKinds`] kind). A comparison
    /// on one carries a kind guard that dispatches a wrong-kind receiver
    /// (BT-4823), so it is a Boolean only while the declaration holds.
    declared_params: HashSet<EcoString>,
    /// How many times the method assigns each local, nested blocks included.
    assignments: HashMap<EcoString, usize>,
    /// The method's and its blocks' parameter names.
    parameters: HashSet<EcoString>,
    /// Whether the method has a destructuring assignment, whose bound names
    /// [`Self::assignments`] does not count.
    destructures: bool,
}

impl Rewriter {
    fn of(method: &MethodDefinition) -> Self {
        let mut rw = Self {
            conditions: HashMap::new(),
            boolean_comparisons: HashSet::new(),
            two_branch: HashSet::new(),
            declared_params: trusted_param_types(method)
                .into_iter()
                .filter(|(_, ty)| ComparisonKinds::of_type_name(ty).is_some())
                .map(|(name, _)| EcoString::from(name))
                .collect(),
            assignments: HashMap::new(),
            parameters: method
                .parameters
                .iter()
                .map(|p| p.name.name.clone())
                .collect(),
            destructures: false,
        };
        for stmt in &method.body {
            walk_expression(&stmt.expression, &mut |e| match e {
                Expression::Assignment { target, .. } => {
                    if let Expression::Identifier(id) = target.as_ref() {
                        *rw.assignments.entry(id.name.clone()).or_default() += 1;
                    }
                }
                Expression::DestructureAssignment { .. } => rw.destructures = true,
                Expression::Block(block) => rw
                    .parameters
                    .extend(block.parameters.iter().map(|p| p.name.clone())),
                _ => {}
            });
        }
        rw
    }

    /// Whether `expr` is `true` or `false` whatever it evaluates: a structural
    /// Boolean ([`is_boolean_by_construction`]) or a [`Self::typed_comparison`].
    fn is_boolean(&self, expr: &Expression) -> bool {
        is_boolean_by_construction(expr)
            || Self::typed_comparison(expr).is_some()
            || self.short_circuit_operands(expr).is_some()
    }

    /// Whether `expr` is a comparison on a *declared* parameter
    /// (`x > 0`, `x :: Integer`), or an `and:`/`or:` of Booleans and such
    /// comparisons (BT-4823). It answers a Boolean while the declaration
    /// holds; a receiver that breaks it is dispatched and may answer anything,
    /// which [`helper_guard_sense`] decides through `beamtalk_guard_return`.
    ///
    /// A comparison on a proven receiver whose right operand is a declared
    /// parameter (`0 < x`, `x :: Integer`) is the same: a Boolean while the
    /// declaration holds; a non-number right operand broke it and is sent a
    /// reflected hook that may answer anything (ADR 0116 § Addendum,
    /// BT-5176). With an untyped right operand a hook answer is ordinary
    /// code, not a broken declaration, so it is not counted here, whether
    /// the receiver is a literal or a declared parameter.
    fn is_declared_boolean(&self, expr: &Expression) -> bool {
        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            is_cast: false,
            ..
        } = expr
            && is_ordering_comparison(op)
        {
            let declared = |e: &Expression| matches!(e, Expression::Identifier(id) if self.declared_params.contains(&id.name));
            // The right operand must be proven numeric or itself declared:
            // an untyped one makes a hook answer ordinary code (ADR 0116
            // § Addendum, BT-5176), not a broken declaration.
            let right_ok = arguments.first().is_some_and(|right| {
                declared(right) || proven_comparison_kind(right, None) == Some(ProvenKind::Number)
            });
            return right_ok
                && (declared(receiver)
                    || proven_comparison_kind(receiver, None) == Some(ProvenKind::Number));
        }
        self.short_circuit_declared_operands(expr).is_some()
    }

    /// The operands of `a and: [b]` / `a or: [b]` (literal one-statement
    /// block) when each is a Boolean by construction or a declared
    /// comparison, and at least one is the latter.
    fn short_circuit_declared_operands<'e>(
        &self,
        expr: &'e Expression,
    ) -> Option<(&'e Expression, &'e Expression)> {
        let (first, second) = and_or_operands(expr)?;
        let ok = |e: &Expression| self.is_boolean(e) || self.is_declared_boolean(e);
        (ok(first)
            && ok(second)
            && (self.is_declared_boolean(first) || self.is_declared_boolean(second)))
        .then_some((first, second))
    }

    /// The receiver and block value of `expr` when it is `a and: [b]` /
    /// `a or: [b]` with a literal one-statement block, both of which are
    /// Booleans ([`Self::is_boolean`]). Codegen inlines such a send as a
    /// `case` on `a` that only sends for a non-Boolean `a`, and answers
    /// `a` or `b` otherwise, so it is `true` or `false` and never sends
    /// (BT-4496).
    fn short_circuit_operands<'e>(
        &self,
        expr: &'e Expression,
    ) -> Option<(&'e Expression, &'e Expression)> {
        let (first, second) = and_or_operands(expr)?;
        (self.is_boolean(first) && self.is_boolean(second)).then_some((first, second))
    }

    /// The left operand of `expr` when it is a comparison (`< > <= >=`)
    /// codegen lowers to a bare Erlang BIF, which answers a Boolean and never
    /// sends ([`comparison_is_bare`], the one predicate codegen lowers by):
    /// a `String` literal receiver, or a number or character literal receiver
    /// with a proven numeric right operand (`0 < 5`, `x`-free). A typed
    /// parameter is only declared, so its comparison carries a kind guard
    /// that dispatches a wrong-kind receiver (BT-4823), and an unproven right
    /// operand is tested and may be sent a reflected hook (ADR 0116
    /// § Addendum, BT-5176): neither is a Boolean by construction. The
    /// rewrite proves nothing about `self` (no class name), so a comparison
    /// on `self` is never taken for one here.
    fn typed_comparison(expr: &Expression) -> Option<&Expression> {
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            is_cast: false,
            ..
        } = expr
        else {
            return None;
        };
        if !is_ordering_comparison(op) {
            return None;
        }
        let [right] = arguments.as_slice() else {
            return None;
        };
        let bare = if fault!(LiteralComparisonTrustsRightOperand) {
            proven_comparison_kind(receiver, None).is_some()
        } else {
            comparison_is_bare(receiver, right, None)
        };
        bare.then_some(receiver.as_ref())
    }

    /// Records `receiver`, a conditional's receiver the rewrite treats as a
    /// Boolean by construction, when that rests on a typed comparison.
    fn note_boolean(&mut self, receiver: &Expression) {
        if let Some(left) = Self::typed_comparison(receiver) {
            self.boolean_comparisons.insert(left.span());
        } else if let Some((first, second)) = self.short_circuit_operands(receiver) {
            self.note_boolean(first);
            self.note_boolean(second);
        }
    }

    /// Whether `rest`, the statements after a guard, may move into the
    /// guard's falling branch without changing any construct's threaded
    /// locals (ADR 0131): every local `rest` assigns is one only `rest`
    /// assigns. A local bound outside it (a parameter, or one assigned
    /// earlier) would become an outer local of the branch, which a
    /// construct in `rest` then threads where the method as written does
    /// not, and the core recognizer (`block_facts`) would disagree.
    fn can_absorb(&self, rest: &[ExpressionStatement]) -> bool {
        if self.destructures {
            return false;
        }
        let mut in_rest: HashMap<EcoString, usize> = HashMap::new();
        for stmt in rest {
            walk_expression(&stmt.expression, &mut |e| {
                if let Expression::Assignment { target, .. } = e
                    && let Expression::Identifier(id) = target.as_ref()
                {
                    *in_rest.entry(id.name.clone()).or_default() += 1;
                }
            });
        }
        in_rest.iter().all(|(name, n)| {
            !self.parameters.contains(name) && self.assignments.get(name) == Some(n)
        })
    }
}

/// `body` rewritten, when it holds a block NLR and the rewrite leaves none.
fn rewritten_body(
    body: &[ExpressionStatement],
    rw: &mut Rewriter,
) -> Option<Vec<ExpressionStatement>> {
    if !body_has_block_nlr(body) {
        return None;
    }
    let rewritten = rewrite_tail_body(body.to_vec(), rw);
    (!body_has_block_nlr(&rewritten)).then_some(rewritten)
}

/// Rewrites a tail body: absorbs the statements after the first guard into
/// it, then rewrites the (new) last statement in tail position.
fn rewrite_tail_body(
    mut body: Vec<ExpressionStatement>,
    rw: &mut Rewriter,
) -> Vec<ExpressionStatement> {
    let guard_at = body.iter().enumerate().position(|(i, stmt)| {
        i + 1 < body.len()
            && guard_shape(&stmt.expression, rw).is_some()
            && rw.can_absorb(&body[i + 1..])
    });
    if let Some(i) = guard_at {
        let rest = body.split_off(i + 1);
        let guard = body.pop().expect("guard statement at index i");
        // The guard's own check made it exact on any receiver, so its
        // branches are tail bodies whatever the receiver.
        let absorbed = absorb_rest(guard.expression, rest, rw);
        body.push(ExpressionStatement {
            expression: rewrite_conditional_branches(absorbed, rw),
            ..guard
        });
    } else if let Some(last) = body.pop() {
        let expression = rewrite_tail_expression(last.expression, rw);
        body.push(ExpressionStatement { expression, ..last });
    }
    body
}

/// Rewrites `expr` in tail position: `^ v` is `v`, and an inlined
/// conditional's branches (a `match:`'s arm bodies) are in tail position too.
fn rewrite_tail_expression(expr: Expression, rw: &mut Rewriter) -> Expression {
    match expr {
        Expression::Return { value, .. } => *value,
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            is_cast: false,
            span,
        } if !rw.is_boolean(&receiver)
            && let Some(first) = two_branch_returns(&parts, &arguments) =>
        {
            // BT-4998: `c ifTrue: [^a] ifFalse: [^b]` on a receiver that may
            // not be a Boolean. Its fallback sends the selector as written
            // through the helper, so the arms are the plain values.
            rw.two_branch.insert(first);
            rewrite_conditional_branches(
                Expression::MessageSend {
                    receiver,
                    selector: MessageSelector::Keyword(parts),
                    arguments,
                    is_cast: false,
                    span,
                },
                rw,
            )
        }
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            is_cast: false,
            span,
        } if is_inlined_conditional(&parts, &arguments)
            && (family_of(&parts) == Family::Nil || rw.is_boolean(&receiver)) =>
        {
            if family_of(&parts) == Family::Boolean {
                rw.note_boolean(&receiver);
            }
            rewrite_conditional_branches(
                Expression::MessageSend {
                    receiver,
                    selector: MessageSelector::Keyword(parts),
                    arguments,
                    is_cast: false,
                    span,
                },
                rw,
            )
        }
        Expression::Match {
            value,
            arms,
            exhaustive,
            span,
        } => Expression::Match {
            value,
            arms: arms
                .into_iter()
                .map(|mut arm| {
                    if let Expression::Return { value, .. } = arm.body {
                        arm.body = *value;
                    }
                    arm
                })
                .collect(),
            exhaustive,
            span,
        },
        other => other,
    }
}

/// The span of the first branch block when `parts`/`arguments` are a
/// two-branch Boolean conditional (`ifTrue:ifFalse:` / `ifFalse:ifTrue:`)
/// whose two literal blocks are each exactly `^ v` with a `v` that
/// [`is_plain_value`] (BT-4998).
fn two_branch_returns(parts: &[KeywordPart], arguments: &[Expression]) -> Option<Span> {
    if parts.len() != 2
        || !is_inlined_conditional(parts, arguments)
        || family_of(parts) != Family::Boolean
    {
        return None;
    }
    let mut first = None;
    for arg in arguments {
        let Expression::Block(block) = arg else {
            return None;
        };
        let [stmt] = block.body.as_slice() else {
            return None;
        };
        let Expression::Return { value, .. } = &stmt.expression else {
            return None;
        };
        if !block.parameters.is_empty() || !is_plain_value(value) {
            return None;
        }
        // Written order differs from the true/false order codegen sees for
        // `ifFalse:ifTrue:`, so key on the earlier block, which both agree on.
        first = Some(first.map_or(block.span, |f: Span| {
            if block.span.start() < f.start() {
                block.span
            } else {
                f
            }
        }));
    }
    first
}

/// Whether `expr` is a literal or a variable: it has no effect, cannot raise,
/// and answers the same wherever it is evaluated (unlike a field, which a
/// receiver's method could change, or a class reference, which is a registry
/// lookup that raises `class_not_found`).
fn is_plain_value(expr: &Expression) -> bool {
    matches!(expr, Expression::Literal(..) | Expression::Identifier(..))
}

/// The inlined conditional `expr` with each literal block's body rewritten
/// as a tail body.
fn rewrite_conditional_branches(expr: Expression, rw: &mut Rewriter) -> Expression {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast,
        span,
    } = expr
    else {
        return expr;
    };
    Expression::MessageSend {
        receiver,
        selector,
        arguments: arguments
            .into_iter()
            .map(|arg| match arg {
                Expression::Block(mut block) => {
                    let body = std::mem::take(&mut block.body);
                    Expression::Block(with_body(block, rewrite_tail_body(body, rw)))
                }
                other => other,
            })
            .collect(),
        is_cast,
        span,
    }
}

/// `block` with `body` in place of its own, keeping its identity.
fn with_body(mut block: Block, body: Vec<ExpressionStatement>) -> Block {
    block.body = body;
    block
}

fn selector_of(parts: &[KeywordPart]) -> String {
    parts.iter().map(|p| p.keyword.as_str()).collect()
}

/// The receiver and block value of `expr` when it is `a and: [b]` /
/// `a or: [b]` with a literal one-statement block: the one shape both
/// [`Rewriter::short_circuit_operands`] and
/// [`Rewriter::short_circuit_declared_operands`] decide a condition on.
fn and_or_operands(expr: &Expression) -> Option<(&Expression, &Expression)> {
    let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        is_cast: false,
        ..
    } = expr
    else {
        return None;
    };
    let ([part], [Expression::Block(block)]) = (parts.as_slice(), arguments.as_slice()) else {
        return None;
    };
    if !matches!(part.keyword.as_str(), "and:" | "or:") || !block.parameters.is_empty() {
        return None;
    }
    let [stmt] = block.body.as_slice() else {
        return None;
    };
    Some((receiver.as_ref(), &stmt.expression))
}

/// Whether `parts`/`arguments` name an inlined `ifTrue:`/`ifNil:`-family
/// conditional with literal block arguments (the shared
/// [`is_inlined_conditional_send`], less `and:`/`or:`).
fn is_inlined_conditional(parts: &[KeywordPart], arguments: &[Expression]) -> bool {
    let selector = selector_of(parts);
    !matches!(selector.as_str(), "and:" | "or:")
        && is_inlined_conditional_send(&selector, arguments)
}

/// Whether `expr` is `true` or `false` whatever it evaluates: a Boolean
/// literal, or a comparison codegen lowers to a bare Erlang BIF
/// ([`structural_binary`]: `==`, `=:=`, `/=`, `=/=`), never to a send.
pub(in crate::core_erlang) fn is_boolean_by_construction(expr: &Expression) -> bool {
    match expr {
        Expression::Identifier(id) => matches!(id.name.as_str(), "true" | "false"),
        Expression::MessageSend {
            selector: MessageSelector::Binary(op),
            ..
        } => matches!(
            structural_binary(op),
            Some(StructuralBinary::Bif("==" | "=:=" | "/=" | "=/="))
        ),
        _ => false,
    }
}

/// Whether evaluating `expr` can have no effect and cannot raise: a literal,
/// a variable or a field of `self`.
fn is_effect_free(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. } => true,
        Expression::FieldAccess { receiver, .. } => receiver.is_self(),
        _ => false,
    }
}

/// The two branch slots of a conditional, by the selector of its two-branch
/// form: (`ifTrue:`, `ifFalse:`) or (`ifNil:`, `ifNotNil:`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Family {
    Boolean,
    Nil,
}

impl Family {
    fn keywords(self) -> [&'static str; 2] {
        match self {
            Self::Boolean => ["ifTrue:", "ifFalse:"],
            Self::Nil => ["ifNil:", "ifNotNil:"],
        }
    }
}

/// The family of the conditional `parts` names.
fn family_of(parts: &[KeywordPart]) -> Family {
    if parts[0].keyword.starts_with("ifTrue") || parts[0].keyword.starts_with("ifFalse") {
        Family::Boolean
    } else {
        Family::Nil
    }
}

/// A guard's shape: its family, which slot (0 or 1, in [`Family::keywords`]
/// order) falls through, and whether codegen decides its condition through
/// `beamtalk_guard_return`.
struct GuardShape {
    family: Family,
    falling_slot: usize,
    helper: Option<GuardSense>,
}

/// `expr`'s [`GuardShape`] when it is a guard statement (see the module
/// doc), else `None`.
fn guard_shape(expr: &Expression, rw: &Rewriter) -> Option<GuardShape> {
    let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        is_cast: false,
        ..
    } = expr
    else {
        return None;
    };
    if !is_inlined_conditional(parts, arguments) {
        return None;
    }
    let (family, slots) = branch_slots(parts, arguments)?;
    let returns = |b: Option<&Block>| {
        b.is_some_and(|b| {
            b.body
                .last()
                .is_some_and(|s| matches!(s.expression, Expression::Return { .. }))
        })
    };
    let falling: Vec<usize> = (0..2).filter(|&i| !returns(slots[i])).collect();
    let [falling_slot] = falling.as_slice() else {
        return None;
    };
    let falling_slot = *falling_slot;
    // A falling branch's parameters would shadow names the absorbed
    // statements use.
    if slots[falling_slot].is_some_and(|b| !b.parameters.is_empty()) {
        return None;
    }
    let helper = match family {
        Family::Nil => None,
        Family::Boolean if rw.is_boolean(receiver) => None,
        Family::Boolean => Some(helper_guard_sense(
            parts,
            slots[1 - falling_slot]?,
            rw.is_declared_boolean(receiver),
        )?),
    };
    Some(GuardShape {
        family,
        falling_slot,
        helper,
    })
}

/// The [`GuardSense`] of a one-branch Boolean guard `c ifTrue: [^v]` /
/// `c ifFalse: [^v]` decided by `beamtalk_guard_return`: with an effect-free
/// `v` on any receiver, or any block ending in `^` when `declared` (below).
/// These are the only Boolean guards on a receiver that is not a Boolean by
/// construction the rewrite keeps exact, except that `declared` accepts a
/// small, documented difference for a receiver that broke its declaration
/// (see `beamtalk_guard_return`'s moduledoc).
///
/// `declared` is a receiver that answers a Boolean while a parameter's
/// declared type holds ([`Rewriter::is_declared_boolean`], BT-4823). Then the
/// helper only ever decides a receiver that broke its declaration, so the
/// returning block may be any block that ends in `^`: a `v` with effects, or
/// statements before the `^`, run after that receiver's own `ifTrue:` has
/// unwound instead of inside it. This keeps `x > 0 ifTrue: [^self one]` on an
/// `:: Integer` parameter the direct return BT-4452 compiled it to.
fn helper_guard_sense(
    parts: &[KeywordPart],
    returning: &Block,
    declared: bool,
) -> Option<GuardSense> {
    let [part] = parts else {
        return None;
    };
    if !declared {
        let [stmt] = returning.body.as_slice() else {
            return None;
        };
        let Expression::Return { value, .. } = &stmt.expression else {
            return None;
        };
        if !is_effect_free(value) {
            return None;
        }
    }
    match part.keyword.as_str() {
        "ifTrue:" => Some(GuardSense::IfTrue),
        "ifFalse:" => Some(GuardSense::IfFalse),
        _ => None,
    }
}

/// The conditional's family and its two branch blocks in
/// [`Family::keywords`] order (`None` for an absent slot).
fn branch_slots<'a>(
    parts: &[KeywordPart],
    arguments: &'a [Expression],
) -> Option<(Family, [Option<&'a Block>; 2])> {
    let family = family_of(parts);
    let keywords = family.keywords();
    let mut slots = [None, None];
    for (part, arg) in parts.iter().zip(arguments) {
        let Expression::Block(block) = arg else {
            return None;
        };
        let slot = keywords.iter().position(|k| *k == part.keyword)?;
        slots[slot] = Some(block);
    }
    Some((family, slots))
}

/// The guard `expr` (a [`guard_shape`]) with `rest` appended to its falling
/// branch, as the two-branch conditional (`ifTrue:ifFalse:` /
/// `ifNil:ifNotNil:`), recording its receiver in `conditions` when codegen
/// decides it through `beamtalk_guard_return`.
fn absorb_rest(expr: Expression, rest: Vec<ExpressionStatement>, rw: &mut Rewriter) -> Expression {
    let shape = guard_shape(&expr, rw).expect("absorb_rest is only called on a guard");
    let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        is_cast,
        span,
    } = expr
    else {
        unreachable!("guard_shape matched a keyword send");
    };
    match shape.helper {
        Some(sense) => {
            rw.conditions.insert(receiver.span(), sense);
        }
        None if shape.family == Family::Boolean => rw.note_boolean(&receiver),
        None => {}
    }
    let keywords = shape.family.keywords();
    let mut slots: [Option<Block>; 2] = [None, None];
    for (part, arg) in parts.iter().zip(arguments) {
        if let Expression::Block(block) = arg {
            let slot = keywords
                .iter()
                .position(|k| *k == part.keyword)
                .expect("guard_shape resolved every keyword");
            slots[slot] = Some(block);
        }
    }
    // The absent falling slot becomes a fresh block. Its span is the empty
    // span at the guard's end: no parsed node has it, so nothing keyed by
    // span (facts, sequencing registrations) can collide with it.
    let mut falling = slots[shape.falling_slot]
        .take()
        .unwrap_or_else(|| Block::new(Vec::new(), Vec::new(), Span::new(span.end(), span.end())));
    falling.body.extend(rest);
    slots[shape.falling_slot] = Some(falling);
    let [first, second] = slots;
    Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(
            keywords
                .iter()
                .map(|k| KeywordPart::new(*k, span))
                .collect(),
        ),
        arguments: vec![
            Expression::Block(first.expect("both slots are filled")),
            Expression::Block(second.expect("both slots are filled")),
        ],
        is_cast,
        span,
    }
}

impl CoreErlangGenerator {
    /// `cond_doc`, the compiled `receiver` of an inlined Boolean
    /// conditional, as the condition the conditional's `case` reads: when
    /// `receiver` is a recorded guard's ([`GuardRewrite::conditions`]), a
    /// non-Boolean value is decided by `beamtalk_guard_return`, so the
    /// `case` sees `true` or `false` and its own fallback send never runs.
    pub(in crate::core_erlang) fn guard_condition_doc(
        &mut self,
        receiver: &Expression,
        cond_doc: Document<'static>,
    ) -> Document<'static> {
        let Some(method) = self.current_guard_method.clone() else {
            return cond_doc;
        };
        let key = (method, receiver.span());
        let Some(sense) = self.guard_rewrite.conditions.get(&key).copied() else {
            return cond_doc;
        };
        self.guard_conditions_used.insert(key);
        let other = self.fresh_temp_var("GuardCond");
        docvec![
            "case ",
            cond_doc,
            " of <'true'> when 'true' -> 'true' <'false'> when 'true' -> 'false' <",
            leaf::var(other.clone()),
            "> when 'true' -> call ",
            leaf::atom("beamtalk_guard_return"),
            ":",
            leaf::atom(sense.helper()),
            "(",
            leaf::var(other),
            ") end",
        ]
    }

    /// BT-4998: whether the conditional whose first branch block is `first`
    /// is a recorded tail two-branch conditional. Marks it consumed.
    pub(in crate::core_erlang) fn take_two_branch(&mut self, first: Span) -> bool {
        let Some(method) = self.current_guard_method.clone() else {
            return false;
        };
        let key = (method, first);
        if self.guard_rewrite.two_branch.contains(&key) {
            self.guard_conditions_used.insert(key);
            true
        } else {
            false
        }
    }

    /// Whether `receiver` is a recorded guard's, in the method being
    /// generated.
    pub(in crate::core_erlang) fn is_guard_condition(&self, receiver: &Expression) -> bool {
        self.current_guard_method.as_ref().is_some_and(|method| {
            self.guard_rewrite
                .conditions
                .contains_key(&(method.clone(), receiver.span()))
        })
    }

    /// Records how a comparison whose left operand is `left` was lowered
    /// (`bare`: to the comparison BIF, else through a dispatching guard),
    /// when it is a recorded typed comparison of the method being generated
    /// ([`GuardRewrite::boolean_comparisons`]). One dispatched lowering
    /// sticks, whatever other lowerings of the method did.
    pub(in crate::core_erlang) fn record_comparison_lowering(
        &mut self,
        left: &Expression,
        bare: bool,
    ) {
        let Some(method) = self.current_guard_method.clone() else {
            return;
        };
        let key = (method, left.span());
        if self.guard_rewrite.boolean_comparisons.contains(&key) {
            *self.boolean_comparison_lowerings.entry(key).or_insert(true) &= bare;
        }
    }

    /// The recorded typed comparisons codegen did not always lower to a bare
    /// BIF (never lowered, or dispatched at least once): each is a guard the
    /// rewrite made exact only for a Boolean receiver, whose receiver could
    /// then be a non-Boolean answer of a dispatched comparison (BT-4452).
    pub(in crate::core_erlang) fn unconfirmed_boolean_comparisons(&self) -> Vec<GuardConditionKey> {
        let mut missed: Vec<_> = self
            .guard_rewrite
            .boolean_comparisons
            .iter()
            .filter(|key| self.boolean_comparison_lowerings.get(*key) != Some(&true))
            .cloned()
            .collect();
        missed.sort_by_key(|(m, r)| (m.span.start(), r.start()));
        missed
    }

    /// The recorded guard conditions no conditional consumed: each is a
    /// lowering path that compiled a rewritten guard's receiver without
    /// [`Self::guard_condition_doc`], so a non-Boolean receiver would be
    /// sent `ifTrue:ifFalse:` in place of the guard's own selector.
    pub(in crate::core_erlang) fn unconsumed_guard_conditions(&self) -> Vec<GuardConditionKey> {
        let used: &HashSet<GuardConditionKey> = &self.guard_conditions_used;
        let mut missed: Vec<_> = self
            .guard_rewrite
            .conditions
            .keys()
            .chain(self.guard_rewrite.two_branch.iter())
            .filter(|key| !used.contains(key))
            .cloned()
            .collect();
        missed.sort_by_key(|(m, r)| (m.span.start(), r.start()));
        missed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::unparse::unparse_method;

    fn parse(source: &str) -> Module {
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (mut module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        // Codegen runs the rewrite over a module with send-operand
        // parentheses peeled.
        beamtalk_core::ast::normalize::peel_operand_parens(&mut module);
        module
    }

    /// The first method of `class_source`'s class, rewritten and unparsed,
    /// with the number of helper-decided guards (`None` when the rewrite
    /// does not apply).
    fn rewritten_with_conditions(class_source: &str) -> Option<(String, usize)> {
        let mut module = parse(class_source);
        assert_eq!(
            has_guard_returns(&module),
            !rewrite_guard_returns(&mut module.clone()).is_empty(),
            "has_guard_returns agrees with rewrite_guard_returns"
        );
        let rewrite = rewrite_guard_returns(&mut module);
        let method = &module.classes[0].methods[0];
        rewrite
            .bodies
            .contains_key(&MethodKey::of(method))
            .then(|| (unparse_method(method), rewrite.conditions.len()))
    }

    fn rewritten(class_source: &str) -> Option<String> {
        rewritten_with_conditions(class_source).map(|(out, _)| out)
    }

    fn class(body: &str) -> String {
        format!("Object subclass: C\n  m: x =>\n{body}\n")
    }

    #[test]
    fn if_true_guard_becomes_if_true_if_false() {
        let (out, helpers) =
            rewritten_with_conditions(&class("    x > 0 ifTrue: [^1]\n    2")).unwrap();
        assert!(out.contains("ifTrue: [1] ifFalse: [2]"), "{out}");
        assert_eq!(helpers, 1, "`x > 0` is not a Boolean by construction");
    }

    #[test]
    fn structural_comparison_needs_no_helper() {
        let (out, helpers) =
            rewritten_with_conditions(&class("    x =:= 0 ifTrue: [^1]\n    2")).unwrap();
        assert!(out.contains("ifTrue: [1] ifFalse: [2]"), "{out}");
        assert_eq!(helpers, 0);
    }

    #[test]
    fn if_false_guard_falls_through_on_true() {
        let out = rewritten(&class("    x > 0 ifFalse: [^1]\n    2")).unwrap();
        assert!(out.contains("ifTrue: [2] ifFalse: [1]"), "{out}");
    }

    #[test]
    fn consecutive_guards_nest() {
        let (out, helpers) = rewritten_with_conditions(&class(
            "    x > 0 ifTrue: [^1]\n    x < 0 ifTrue: [^2]\n    3",
        ))
        .unwrap();
        assert!(!out.contains('^'), "{out}");
        assert!(out.contains("ifTrue: [2] ifFalse: [3]"), "{out}");
        assert_eq!(helpers, 2);
    }

    #[test]
    fn if_nil_guard_absorbs_rest_into_if_not_nil() {
        let out = rewritten(&class("    x ifNil: [^0]\n    x + 1")).unwrap();
        assert!(out.contains("ifNil: [0] ifNotNil: [x + 1]"), "{out}");
    }

    #[test]
    fn two_branch_guard_appends_rest_to_falling_branch() {
        let out = rewritten(&class(
            "    y := 0\n    x =:= 0 ifTrue: [^1] ifFalse: [y := 5]\n    y",
        ))
        .unwrap();
        assert!(!out.contains('^'), "{out}");
        let falling = out.split("ifFalse:").nth(1).expect("ifFalse: branch");
        assert!(falling.contains("y := 5"), "{out}");
    }

    #[test]
    fn tail_match_arm_returns_are_stripped() {
        let out = rewritten(&class("    x match: [0 -> ^#zero; _ -> #other]")).unwrap();
        assert!(!out.contains('^'), "{out}");
    }

    #[test]
    fn closure_nlr_keeps_the_method_as_written() {
        assert_eq!(
            rewritten(&class(
                "    x > 0 ifTrue: [^1]\n    #[1, 2] do: [:e | e =:= x ifTrue: [^e]]\n    2"
            )),
            None
        );
    }

    #[test]
    fn method_without_block_nlr_is_untouched() {
        assert_eq!(
            rewritten(&class("    x > 0 ifTrue: [1] ifFalse: [2]")),
            None
        );
    }

    #[test]
    fn rest_assigning_an_outer_local_is_not_absorbed() {
        // `t` would become an outer local of the falling branch, threaded
        // where the method as written threads nothing (ADR 0131 agreement).
        assert_eq!(
            rewritten(&class(
                "    t := 0\n    x =:= 0 ifTrue: [^1]\n    t := t + 1\n    t"
            )),
            None
        );
        assert_eq!(
            rewritten(&class("    x =:= 0 ifTrue: [^1]\n    x := 2\n    x")),
            None
        );
        // A local only the absorbed statements assign moves with them.
        assert!(rewritten(&class("    x =:= 0 ifTrue: [^1]\n    u := 2\n    u + x")).is_some());
    }

    #[test]
    fn falling_branch_with_parameters_does_not_absorb() {
        assert_eq!(
            rewritten(&class("    x ifNil: [^0] ifNotNil: [:v | v]\n    x")),
            None
        );
    }

    #[test]
    fn non_boolean_receiver_shapes_the_helper_cannot_keep_exact() {
        // A `^` value with effects runs inside the receiver's `ifTrue:` as
        // written, and outside it once rewritten.
        assert_eq!(rewritten(&class("    x ifTrue: [^x foo]\n    2")), None);
        // Statements before the `^`, likewise.
        assert_eq!(rewritten(&class("    x ifTrue: [x foo. ^1]\n    2")), None);
        // A two-branch guard's falling block runs inside the receiver's
        // `ifTrue:ifFalse:` as written.
        assert_eq!(
            rewritten(&class("    x ifTrue: [^1] ifFalse: [x foo]\n    2")),
            None
        );
        // In tail position the receiver's own answer is the method's.
        assert_eq!(rewritten(&class("    x ifTrue: [^1]")), None);
    }

    fn typed_class(body: &str) -> String {
        format!("Object subclass: C\n  m: x :: Integer =>\n{body}\n")
    }

    /// The typed comparisons the rewrite of `class_source` relied on.
    fn boolean_comparisons(class_source: &str) -> usize {
        let mut module = parse(class_source);
        rewrite_guard_returns(&mut module).boolean_comparisons.len()
    }

    #[test]
    fn typed_comparison_allows_every_shape() {
        // BT-4452: a literal left operand and a proven numeric right one
        // (ADR 0116 § Addendum, BT-5176) make `3 > 4` a bare BIF, so every
        // shape the helper cannot keep exact on a non-Boolean is exact here.
        // A `String` literal receiver is bare whatever its argument.
        for body in [
            "    3 > 4 ifTrue: [^self withN: 0]\n    2",
            "    3 > 4.5 ifTrue: [self foo. ^1]\n    2",
            "    3 <= $a ifTrue: [^1] ifFalse: [self foo]\n    2",
            "    3 >= 1 ifTrue: [^1]",
            "    0 < 1 ifFalse: [^x foo]\n    2",
            "    \"m\" < x ifFalse: [^x foo]\n    2",
        ] {
            let src = typed_class(body);
            let (out, helpers) =
                rewritten_with_conditions(&src).unwrap_or_else(|| panic!("{body}"));
            assert!(!out.contains('^'), "{out}");
            assert_eq!(helpers, 0, "{body}");
            assert_eq!(boolean_comparisons(&src), 1, "{body}");
        }
    }

    #[test]
    fn literal_receiver_comparison_with_an_unproven_right_operand_is_decided_by_the_helper() {
        // ADR 0116 § Addendum (BT-5176): `3 > x` tests `x` and sends a
        // non-number its reflected hook, which may answer a non-Boolean, so
        // like a declared comparison it is decided by the helper and nothing
        // is recorded as a Boolean by construction. A declared right operand
        // (`x :: Integer`) is tested, not trusted.
        for body in [
            "    3 > x ifTrue: [^self withN: 0]\n    2",
            "    3 > x ifTrue: [self foo. ^1]\n    2",
            "    0 < x ifFalse: [^x foo]\n    2",
            "    $a <= x ifTrue: [^1]\n    2",
            "    (0 < x or: [x > 9]) ifTrue: [^self withN: 0]\n    2",
        ] {
            let src = typed_class(body);
            let (out, helpers) =
                rewritten_with_conditions(&src).unwrap_or_else(|| panic!("{body}"));
            assert!(!out.contains('^'), "{out}");
            assert_eq!(helpers, 1, "{body}");
            assert_eq!(boolean_comparisons(&src), 0, "{body}");
        }
        // With an untyped right operand a non-Boolean hook answer is ordinary
        // code, not a broken declaration: only the strict effect-free
        // `[^v]` guard goes to the helper; an effectful one keeps its frame.
        let untyped = |body: &str| format!("Object subclass: C\n  m: x =>\n{body}\n");
        assert_eq!(
            rewritten(&untyped("    0 < x ifTrue: [self foo. ^1]\n    2")),
            None
        );
        let (_, helpers) = rewritten_with_conditions(&untyped("    0 < x ifTrue: [^1]\n    2"))
            .expect("an effect-free guard is rewritten");
        assert_eq!(helpers, 1);
        // A declared receiver with an untyped right operand is not lenient
        // either: `x < y` sends a non-number `y` its hook in ordinary code.
        let mixed =
            |body: &str| format!("Object subclass: C\n  m: x :: Integer with: y =>\n{body}\n");
        assert_eq!(
            rewritten(&mixed("    x < y ifTrue: [self foo. ^1]\n    2")),
            None
        );
        let (_, helpers) = rewritten_with_conditions(&mixed("    x < y ifTrue: [^1]\n    2"))
            .expect("an effect-free guard is rewritten");
        assert_eq!(helpers, 1);
        // A two-branch guard on it keeps its frame, as on a declared one.
        assert_eq!(
            rewritten(&typed_class(
                "    3 <= x ifTrue: [^1] ifFalse: [self foo]\n    2"
            )),
            None
        );
    }

    #[test]
    fn declared_param_comparison_guard_is_decided_by_the_helper() {
        // BT-4823: `x :: Integer` is only declared, so `x > 0` carries a kind
        // guard that dispatches a wrong-kind receiver and may answer a
        // non-Boolean. A one-branch guard on it is rewritten whatever its
        // returned block does, with the condition decided by the helper and
        // nothing recorded as a Boolean by construction.
        for body in [
            "    x > 0 ifTrue: [^self withN: 0]\n    2",
            "    x > 0 ifTrue: [self foo. ^1]\n    2",
            "    x > 0 ifFalse: [^x foo]\n    2",
            "    (x < 0 or: [x > 9]) ifTrue: [^self withN: 0]\n    2",
            "    (x =:= 0 and: [x > 9]) ifTrue: [^self withN: 0]\n    2",
        ] {
            let src = typed_class(body);
            let (out, helpers) =
                rewritten_with_conditions(&src).unwrap_or_else(|| panic!("{body}"));
            assert!(!out.contains('^'), "{out}");
            assert_eq!(helpers, 1, "{body}");
            assert_eq!(boolean_comparisons(&src), 0, "{body}");
        }
    }

    #[test]
    fn declared_param_comparison_in_other_shapes_is_not_rewritten() {
        // A two-branch guard and a tail conditional need a Boolean, which a
        // declared comparison is not by construction: the frame stays.
        let body = "    x <= 0 ifTrue: [^1] ifFalse: [self foo]\n    2";
        assert_eq!(rewritten(&typed_class(body)), None, "{body}");
    }

    #[test]
    fn declared_param_comparison_two_branch_tail_returns_go_through_the_helper() {
        // BT-4998: plain values in both branches of a tail conditional need
        // no Boolean: a non-Boolean receiver is sent through
        // `beamtalk_guard_return:two_branch/3`.
        let body = "    x > 0 ifTrue: [^1] ifFalse: [^2]";
        assert!(rewritten(&typed_class(body)).is_some(), "{body}");
    }

    #[test]
    fn untrusted_comparisons_are_not_boolean() {
        // Untyped, non-comparable, or rebound parameters dispatch `>`.
        assert_eq!(
            rewritten(&class("    x > 0 ifTrue: [^self withN: 0]\n    2")),
            None
        );
        assert_eq!(
            rewritten(
                "Object subclass: C\n  m: x :: Point =>\n    x > 0 ifTrue: [^x foo]\n    2\n"
            ),
            None
        );
        assert_eq!(
            rewritten(&typed_class(
                "    #[1] do: [:x | x]\n    x > 0 ifTrue: [^x foo]\n    2"
            )),
            None
        );
        // A comparison on a send's answer dispatches `>`.
        assert_eq!(
            rewritten(&typed_class("    x abs > 0 ifTrue: [^x foo]\n    2")),
            None
        );
    }

    #[test]
    fn and_or_of_booleans_allows_every_shape() {
        // BT-4496: `a and: [b]` of Booleans by construction is a Boolean.
        for body in [
            "    (x =:= 0 and: [x =/= 1]) ifTrue: [x foo. ^x foo]\n    2",
            "    (x == 0 or: [x == 1]) ifTrue: [^1] ifFalse: [x foo]\n    2",
            "    (x == 0 and: [x == 1 or: [x == 2]]) ifFalse: [^x foo]\n    2",
            "    (x == 0 and: [true]) ifTrue: [^1]",
        ] {
            let (out, helpers) =
                rewritten_with_conditions(&class(body)).unwrap_or_else(|| panic!("{body}"));
            assert!(!out.contains('^'), "{out}");
            assert_eq!(helpers, 0, "{body}");
        }
        // Typed comparisons inside are recorded for the lowering check.
        let src = typed_class("    (3 > 2 and: [3 < 4.0]) ifTrue: [^x foo]\n    2");
        assert!(rewritten(&src).is_some());
        assert_eq!(boolean_comparisons(&src), 2);
    }

    #[test]
    fn and_or_with_a_non_boolean_operand_is_not_boolean() {
        for body in [
            "    (x and: [x == 0]) ifTrue: [^x foo]\n    2",
            "    (x == 0 and: [x foo]) ifTrue: [^x foo]\n    2",
            "    (x == 0 and: [x == 1. x foo]) ifTrue: [^x foo]\n    2",
            "    (x == 0 and: [:y | x == 1]) ifTrue: [^x foo]\n    2",
            "    (x == 0 and: x) ifTrue: [^x foo]\n    2",
            "    (x == 0 xor: [x == 1]) ifTrue: [^x foo]\n    2",
        ] {
            assert_eq!(rewritten(&class(body)), None, "{body}");
        }
    }

    #[test]
    fn boolean_by_construction_allows_every_shape() {
        assert!(rewritten(&class("    x == 1 ifTrue: [x foo. ^x foo]\n    2")).is_some());
        assert!(rewritten(&class("    x =/= 1 ifTrue: [^1] ifFalse: [x foo]\n    2")).is_some());
        assert!(rewritten(&class("    x == 1 ifTrue: [^1]")).is_some());
    }
}
