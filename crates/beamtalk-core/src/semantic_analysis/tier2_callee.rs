// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 §6 (BT-3755): which actor self-send callees thread a Tier 2 block.
//!
//! **DDD Context:** Semantic Analysis
//!
//! `self ap: [t := t + 1]` in an actor instance method is compiled by
//! codegen's `detect_tier2_self_send`: the block becomes a Tier 2 closure
//! (`fun(Args, StateAcc) -> {Result, StateAcc}`), its captured-local writes
//! are packed into the actor state around the dispatch, and the callee `ap:`
//! is compiled with the matching parameter as a Tier 2 block parameter.
//! That only answers right when the callee actually hands the `StateAcc` of
//! the block back. This module is the one place that decides it; the §6
//! diagnostic (`validators::local_threading`) and codegen's class pre-scan
//! (`scan_class_for_tier2_blocks`) both ask [`callee_threads_block_param`] and
//! [`passed_on_targets`], so a site the diagnostic exempts is exactly a site
//! codegen promotes.
//!
//! A callee threads parameter `i` of selector `sel` in class `C` when:
//!
//! 1. `C` itself defines `sel` as an instance method (a callee inherited from
//!    another class is compiled with its parameter as a plain Tier 1 block and
//!    raises on a Tier 2 closure);
//! 2. no subclass can replace it: `C` or the method is `sealed` (an actor
//!    self-send late-binds through the receiver's own class module, and an
//!    override is compiled without a call site, so its parameter is Tier 1);
//! 3. every use of the parameter in the method body is one of the shapes
//!    codegen unpacks (probed on the debug build, BT-3755; the runtime pins are
//!    the fixtures named below), and no other use exists (a use inside
//!    another block, a conditional or loop outside these shapes, an operand,
//!    a store, a return of the block itself, or an argument of anything else
//!    drops the write or raises):
//!    - a **direct invocation** (`b value`, `b value: x`, a `value`-family
//!      cascade) written as a method-body statement, the right-hand side of a
//!      body-level `x := ...` or the value of a body-level `^...`;
//!    - the same direct invocation as the right-hand side of `x := ...` in the
//!      body of a statement-level `do:` loop, where `x` is a local bound
//!      before the loop (the loop's own local write is what routes its body
//!      through the stateful `StateAcc` path: `hom_composability_actor.bt`);
//!    - the **callable argument of a collection HOM** that codegen lowers
//!      through the ADR 0128 opaque-callable fold
//!      ([`is_opaque_callable_hom_send`]), in operand position of a
//!      method-body statement or inside the branches of a conditional, the
//!      body of `to:do:` or the condition of `whileTrue:`/`whileFalse:`
//!      (`mutation_corpus_actor.bt`, `self_send_hom_fold_pool.bt`); a fold
//!      nested in the receiver or a non-block argument of another send is
//!      accepted, pinned (runtime) at the argument of a self-send, the
//!      argument and the receiver of a keyword send, and two folds in one
//!      statement (`adr0131section6exemptions_actor.bt`, `calleeNest*`);
//!    - passed as a bare argument of a method-body self-send to another
//!      callee of the same class that itself threads that parameter.
//!
//! # ADR 0132 Phase 1b: the unit-wide threading index
//!
//! [`ThreadingIndex`] is the generalization ADR 0132 §1 asks for, restricted to
//! the Phase 1 subset of uses, and it is what populates
//! `MethodInfo::threading_params` (for a compilation unit in
//! `ClassInfo::from_class_definition`, for the stdlib through the
//! `generated_builtins.rs` generator in `build_stdlib`). Parameter `index` of a
//! method (class side or instance side, separately) is a *threading parameter*
//! when it is used at least once and every use is one of, at the **method
//! root** (a statement, an assignment right-hand side, a `^` value, or an
//! operand of such a statement):
//!
//! - a direct invocation (`b value`, `b value: x`, `b valueWithArguments: xs`,
//!   a `value`-family cascade);
//! - the callable argument of a collection message
//!   ([`is_opaque_callable_hom_send`]: `items do: b`, `items inject: 0 into: b`);
//! - a bare argument of a same-module send (receiver `self`, or a literal class
//!   defined in the same unit) whose callee has a threading parameter at that
//!   position.
//!
//! Anything else makes the parameter non-threading in this Epic: a store (field,
//! class variable, literal), a return of the block itself, an FFI argument, an
//! asynchronous send, a `spawns_block` callee, `arity`/`respondsTo:`, a forward
//! to a receiver outside the unit or to an unknown receiver (Phase 2), and the
//! two cases ADR 0132 §1 and §2 add later (Phase 3, the `Acc` capability):
//! **an invocation inside a construct** (loop or fold body, conditional arm,
//! `on:do:`/`ensure:` or `tryDo:` block) and **a use inside a nested block
//! literal**.
//!
//! The answer is a **greatest fixpoint** over the unit: every candidate starts
//! as threading and is removed when one of its uses fails against the current
//! assumption, until nothing changes. A cycle of forwards (self-recursion,
//! mutual recursion) therefore threads unless some member of the cycle has a
//! disqualifying use, and the result does not depend on which method is asked
//! first. The actor-channel predicate above ([`callee_threads_block_param`])
//! is a separate, narrower rule (its accepted set does not grow in Phase 1b, so
//! it keeps answering `false` on a cycle) until the actor exemption moves onto
//! the index.

use crate::ast::{
    Block, ClassDefinition, Expression, ExpressionStatement, MessageSelector, MethodDefinition,
};
use crate::semantic_analysis::block_facts::{
    is_safe_value_family_selector, normalize_cascade, stored_block_var_uses,
};
use crate::state_threading_selectors::{is_opaque_callable_hom_send, opaque_fold_callable_arg};
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

/// The instance method of `class` that a self-send of `selector` resolves to,
/// when `class` itself defines it.
#[must_use]
pub fn same_class_callee<'a>(
    class: &'a ClassDefinition,
    selector: &str,
) -> Option<&'a MethodDefinition> {
    class
        .methods
        .iter()
        .find(|m| !m.is_class_method && m.selector.name() == selector)
}

/// Whether `class`'s instance method `selector` threads the `StateAcc` of the
/// Tier 2 block passed as its parameter number `index` (see the module docs).
#[must_use]
pub fn callee_threads_block_param(class: &ClassDefinition, selector: &str, index: usize) -> bool {
    threads(class, selector, index, &mut Vec::new())
}

/// The `(selector, parameter index)` pairs of `class` that the Tier 2 block
/// passed as parameter `index` of `selector` is passed on to as a bare
/// argument of a method-body self-send. Codegen compiles each of them with a
/// Tier 2 block parameter too (BT-3755). Empty if `selector` is not a
/// threading callee.
#[must_use]
pub fn passed_on_targets(
    class: &ClassDefinition,
    selector: &str,
    index: usize,
) -> Vec<(EcoString, usize)> {
    let mut targets = Vec::new();
    let mut seen = HashSet::new();
    collect_targets(class, selector, index, &mut seen, &mut targets);
    targets
}

fn collect_targets(
    class: &ClassDefinition,
    selector: &str,
    index: usize,
    seen: &mut HashSet<(EcoString, usize)>,
    targets: &mut Vec<(EcoString, usize)>,
) {
    if !callee_threads_block_param(class, selector, index) || !seen.insert((selector.into(), index))
    {
        return;
    }
    let Some(method) = same_class_callee(class, selector) else {
        return;
    };
    let Some(param) = method.parameters.get(index) else {
        return;
    };
    for stmt in &method.body {
        if let Some((target, position)) =
            pass_on(peel(&stmt.expression, &param.name.name), &param.name.name)
        {
            targets.push((target.clone(), position));
            collect_targets(class, &target, position, seen, targets);
        }
    }
}

fn threads(
    class: &ClassDefinition,
    selector: &str,
    index: usize,
    visiting: &mut Vec<(EcoString, usize)>,
) -> bool {
    let Some(method) = same_class_callee(class, selector) else {
        return false;
    };
    if !(class.is_sealed || method.is_sealed) {
        return false;
    }
    let Some(param) = method.parameters.get(index) else {
        return false;
    };
    let key = (EcoString::from(selector), index);
    if visiting.contains(&key) {
        return false;
    }
    visiting.push(key);
    let ctx = Ctx {
        class,
        method,
        name: param.name.name.as_str(),
    };
    let ok = method
        .body
        .iter()
        .enumerate()
        .all(|(i, stmt)| ctx.statement_ok(i, &stmt.expression, visiting));
    visiting.pop();
    ok
}

struct Ctx<'a> {
    class: &'a ClassDefinition,
    method: &'a MethodDefinition,
    name: &'a str,
}

impl Ctx<'_> {
    fn mentions(&self, expr: &Expression) -> bool {
        stored_block_var_uses(expr, self.name) != (false, false)
    }

    /// Whether method-body statement number `index` uses the parameter only
    /// in a shape the module docs list (or not at all).
    fn statement_ok(
        &self,
        index: usize,
        statement: &Expression,
        visiting: &mut Vec<(EcoString, usize)>,
    ) -> bool {
        if !self.mentions(statement) {
            return true;
        }
        let inner = peel(statement, self.name);
        if self.is_direct_invocation(inner) {
            return true;
        }
        if let Some((target, position)) = pass_on(inner, self.name) {
            return threads(self.class, &target, position, visiting);
        }
        if self.is_stateful_do_loop(index, statement) {
            return true;
        }
        self.opaque_ok(statement)
    }

    /// `name value...` or a `value`-family cascade on `name`, whose arguments
    /// do not mention `name`.
    fn is_direct_invocation(&self, expr: &Expression) -> bool {
        direct_invocation(expr, self.name, is_safe_value_family_selector)
    }

    /// `items do: [:e | ... x := name value: e ...]` as a method-body
    /// statement, where `x` is bound before the loop. Each loop-body statement
    /// that mentions `name` must be such an `x := name value: ...` assignment
    /// (`x` neither `name` nor a loop parameter); statements that do not
    /// mention `name` are unconstrained. The caller ([`Self::statement_ok`])
    /// checks the method's other statements separately.
    fn is_stateful_do_loop(&self, index: usize, statement: &Expression) -> bool {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast: false,
            ..
        } = statement
        else {
            return false;
        };
        let [Expression::Block(body)] = arguments.as_slice() else {
            return false;
        };
        if selector.name() != "do:"
            || self.mentions(receiver)
            || !crate::semantic_analysis::block_facts::is_threaded_frame_body(statement, body)
            || body.parameters.iter().any(|p| p.name.as_str() == self.name)
        {
            return false;
        }
        let bound_before = |x: &str| {
            self.method.parameters.iter().any(|p| p.name.name == x)
                || self.method.body[..index].iter().any(|s| {
                    matches!(&s.expression, Expression::Assignment { target, .. }
                        if matches!(target.as_ref(), Expression::Identifier(id) if id.name == x))
                })
        };
        body.body.iter().all(|stmt| {
            if !self.mentions(&stmt.expression) {
                return true;
            }
            let Expression::Assignment { target, value, .. } = &stmt.expression else {
                return false;
            };
            let Expression::Identifier(x) = target.as_ref() else {
                return false;
            };
            x.name != self.name
                && !body.parameters.iter().any(|p| p.name == x.name)
                && bound_before(&x.name)
                && self.is_direct_invocation(value)
        })
    }

    /// Whether every mention of the parameter in `expr` is the callable
    /// argument of an opaque-callable collection HOM ([`is_opaque_callable_hom_send`])
    /// reached through the positions the module docs list.
    fn opaque_ok(&self, expr: &Expression) -> bool {
        if !self.mentions(expr) {
            return true;
        }
        match expr {
            Expression::Parenthesized { expression, .. } => self.opaque_ok(expression),
            Expression::Return { value, .. } => self.opaque_ok(value),
            Expression::Assignment { target, value, .. } => {
                !self.mentions(target) && self.opaque_ok(value)
            }
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                is_cast: false,
                ..
            } => {
                let sel = selector.name();
                if is_opaque_callable_hom_send(expr) {
                    let callable = opaque_fold_callable_arg(&sel, arguments);
                    if callable.is_some_and(|c| is_name(c, self.name)) {
                        return !self.mentions(receiver)
                            && arguments
                                .iter()
                                .filter(|a| !is_name(a, self.name))
                                .all(|a| !self.mentions(a));
                    }
                }
                match (sel.as_str(), receiver.as_ref()) {
                    ("ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifFalse:ifTrue:", _) => {
                        self.opaque_ok(receiver) && arguments.iter().all(|a| self.branch_ok(a))
                    }
                    ("to:do:", _) => {
                        !self.mentions(receiver)
                            && match arguments.as_slice() {
                                [limit, body] => !self.mentions(limit) && self.branch_ok(body),
                                _ => false,
                            }
                    }
                    ("whileTrue:" | "whileFalse:", Expression::Block(condition)) => {
                        self.block_ok(condition) && arguments.iter().all(|a| !self.mentions(a))
                    }
                    _ => {
                        self.opaque_ok(receiver)
                            && arguments
                                .iter()
                                .all(|a| !matches!(a, Expression::Block(_)) && self.opaque_ok(a))
                    }
                }
            }
            _ => false,
        }
    }

    fn branch_ok(&self, arg: &Expression) -> bool {
        match arg {
            Expression::Block(block) => self.block_ok(block),
            other => !self.mentions(other),
        }
    }

    fn block_ok(&self, block: &Block) -> bool {
        !block
            .parameters
            .iter()
            .any(|p| p.name.as_str() == self.name)
            && block
                .body
                .iter()
                .all(|s: &ExpressionStatement| self.opaque_ok(&s.expression))
    }
}

/// Key of one candidate: `(class, class side, selector, parameter index)`.
type ParamKey = (EcoString, bool, EcoString, usize);
/// Key of one method: `(class, class side, selector)`.
type MethodKey = (EcoString, bool, EcoString);
type MethodTable<'a> = HashMap<MethodKey, (&'a ClassDefinition, &'a MethodDefinition)>;

/// The ADR 0132 §1 (Phase 1 subset) threading parameters of every method in a
/// compilation unit; see the module docs. The one source for
/// `MethodInfo::threading_params`.
#[derive(Debug, Default, Clone)]
pub struct ThreadingIndex {
    params: HashMap<MethodKey, Vec<usize>>,
}

impl ThreadingIndex {
    /// Compute the index for `classes` (one module's classes): forwards resolve
    /// within this set only.
    #[must_use]
    pub fn build(classes: &[ClassDefinition]) -> Self {
        let mut methods: MethodTable<'_> = HashMap::new();
        for class in classes {
            for (class_side, list) in [(false, &class.methods), (true, &class.class_methods)] {
                for method in list {
                    methods
                        .entry((class.name.name.clone(), class_side, method.selector.name()))
                        .or_insert((class, method));
                }
            }
        }
        // Candidates: every parameter the body mentions at all (an unused
        // parameter has nothing to thread).
        let mut candidates: HashSet<ParamKey> = HashSet::new();
        for ((class, class_side, selector), (_, method)) in &methods {
            for (index, param) in method.parameters.iter().enumerate() {
                let name = param.name.name.as_str();
                if method
                    .body
                    .iter()
                    .any(|s| stored_block_var_uses(&s.expression, name) != (false, false))
                {
                    candidates.insert((class.clone(), *class_side, selector.clone(), index));
                }
            }
        }
        // Greatest fixpoint: start from "everything threads" and drop every
        // candidate with a use that fails against the current assumption.
        loop {
            let failing: Vec<ParamKey> = candidates
                .iter()
                .filter(|key| !Self::holds(&methods, &candidates, key))
                .cloned()
                .collect();
            if failing.is_empty() {
                break;
            }
            for key in &failing {
                candidates.remove(key);
            }
        }
        let mut params: HashMap<MethodKey, Vec<usize>> = HashMap::new();
        for (class, class_side, selector, index) in candidates {
            params
                .entry((class, class_side, selector))
                .or_default()
                .push(index);
        }
        for indices in params.values_mut() {
            indices.sort_unstable();
        }
        Self { params }
    }

    fn holds(
        methods: &MethodTable<'_>,
        assumed: &HashSet<ParamKey>,
        (class, class_side, selector, index): &ParamKey,
    ) -> bool {
        let Some((class_def, method)) =
            methods.get(&(class.clone(), *class_side, selector.clone()))
        else {
            return false;
        };
        let Some(param) = method.parameters.get(*index) else {
            return false;
        };
        let ctx = RootCtx {
            assumed,
            class: class_def,
            class_side: *class_side,
            name: param.name.name.as_str(),
        };
        method.body.iter().all(|s| ctx.expr_ok(&s.expression))
    }

    /// The threading parameter positions (0-based, ascending) of `class`'s
    /// `selector`, instance side or class side; empty when it has none or is
    /// not in the unit.
    #[must_use]
    pub fn params(&self, class: &str, class_side: bool, selector: &str) -> Vec<usize> {
        self.params
            .get(&(class.into(), class_side, selector.into()))
            .cloned()
            .unwrap_or_default()
    }
}

/// Method-root use checker for [`ThreadingIndex`].
struct RootCtx<'a> {
    assumed: &'a HashSet<ParamKey>,
    class: &'a ClassDefinition,
    class_side: bool,
    name: &'a str,
}

impl RootCtx<'_> {
    fn mentions(&self, expr: &Expression) -> bool {
        stored_block_var_uses(expr, self.name) != (false, false)
    }

    fn is_direct_invocation(&self, expr: &Expression) -> bool {
        direct_invocation(expr, self.name, MessageSelector::is_block_invocation)
    }

    /// The `(class, class side)` a send to `receiver` resolves to inside the
    /// unit: `self`, or a literal class.
    fn forward_target(&self, receiver: &Expression) -> Option<(EcoString, bool)> {
        match receiver {
            Expression::Identifier(id) if id.name == "self" => {
                Some((self.class.name.name.clone(), self.class_side))
            }
            Expression::ClassReference {
                name,
                package: None,
                ..
            } => Some((name.name.clone(), true)),
            _ => None,
        }
    }

    /// Whether every mention of the parameter in `expr` is one of the
    /// method-root uses the module docs list.
    fn expr_ok(&self, expr: &Expression) -> bool {
        if !self.mentions(expr) {
            return true;
        }
        match expr {
            Expression::Parenthesized { expression, .. } => self.expr_ok(expression),
            Expression::Return { value, .. } => self.expr_ok(value),
            Expression::Assignment { target, value, .. } => {
                !self.mentions(target) && self.expr_ok(value)
            }
            Expression::Cascade { .. } => self.is_direct_invocation(expr),
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                is_cast: false,
                ..
            } => {
                if self.is_direct_invocation(expr) {
                    return true;
                }
                let sel = selector.name();
                if is_opaque_callable_hom_send(expr)
                    && opaque_fold_callable_arg(&sel, arguments)
                        .is_some_and(|c| is_name(c, self.name))
                {
                    return !self.mentions(receiver)
                        && arguments
                            .iter()
                            .filter(|a| !is_name(a, self.name))
                            .all(|a| !self.mentions(a));
                }
                if let Some((class, class_side)) = self.forward_target(receiver) {
                    let mut bare = arguments
                        .iter()
                        .enumerate()
                        .filter(|(_, a)| is_name(a, self.name))
                        .map(|(i, _)| i);
                    if let Some(position) = bare.next() {
                        return bare.next().is_none()
                            && arguments
                                .iter()
                                .enumerate()
                                .all(|(i, a)| i == position || !self.mentions(a))
                            && self.assumed.contains(&(class, class_side, sel, position));
                    }
                }
                // Any other send: the parameter may only appear inside an
                // operand that is itself an allowed use (`(b value) + 1`,
                // `self foo: (b value: 2)`). A bare occurrence, or one inside
                // a block literal, falls through to `false` below.
                self.expr_ok(receiver) && arguments.iter().all(|a| self.expr_ok(a))
            }
            _ => false,
        }
    }
}

/// `name <invocation>...` or a cascade of invocations on `name`, where
/// `accepts` says which selectors count as an invocation and no argument
/// mentions `name`. The one shape check shared by the actor-channel predicate
/// (the `value` family) and [`ThreadingIndex`] (adds `valueWithArguments:`);
/// a caller that must exclude a cast (`!`) send does so before calling.
fn direct_invocation(expr: &Expression, name: &str, accepts: fn(&MessageSelector) -> bool) -> bool {
    let mentions = |e: &Expression| stored_block_var_uses(e, name) != (false, false);
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => is_name(receiver, name) && accepts(selector) && !arguments.iter().any(mentions),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            let (underlying, all) = normalize_cascade(receiver, messages);
            is_name(underlying, name)
                && !all.is_empty()
                && all
                    .iter()
                    .all(|(sel, args)| accepts(sel) && !args.iter().any(mentions))
        }
        _ => false,
    }
}

fn is_name(expr: &Expression, name: &str) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == name)
}

/// `expr` without a body-level `x := ...` (for an `x` other than `name`) or
/// `^...` wrapper.
fn peel<'a>(expr: &'a Expression, name: &str) -> &'a Expression {
    match expr {
        Expression::Assignment { target, value, .. } if matches!(target.as_ref(), Expression::Identifier(id) if id.name != name) => {
            value.as_ref()
        }
        Expression::Return { value, .. } => value.as_ref(),
        other => other,
    }
}

/// `self target: ... name ...` where `name` is a bare argument (at position
/// `j`) and no other argument mentions it.
fn pass_on(expr: &Expression, name: &str) -> Option<(EcoString, usize)> {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: false,
        ..
    } = expr
    else {
        return None;
    };
    if !is_name(receiver, "self") {
        return None;
    }
    let mut positions = arguments
        .iter()
        .enumerate()
        .filter(|(_, a)| is_name(a, name))
        .map(|(i, _)| i);
    let position = positions.next()?;
    if positions.next().is_some()
        || arguments
            .iter()
            .enumerate()
            .any(|(i, a)| i != position && stored_block_var_uses(a, name) != (false, false))
    {
        return None;
    }
    Some((selector.name(), position))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Module;

    fn parse(src: &str) -> Module {
        let (module, diags) =
            crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
        assert!(
            diags
                .iter()
                .all(|d| d.severity != crate::source_analysis::Severity::Error),
            "fixture must parse: {diags:?}"
        );
        module
    }

    fn index(src: &str) -> ThreadingIndex {
        ThreadingIndex::build(&parse(src).classes)
    }

    fn inst(idx: &ThreadingIndex, class: &str, sel: &str) -> Vec<usize> {
        idx.params(class, false, sel)
    }

    #[test]
    fn direct_invocations_at_the_method_root_thread() {
        let idx = index(
            "Object subclass: C\n  stmt: b => b value\n  withArg: b => b value: 1\n  \
             assigned: b =>\n    x := b value: 1\n    x\n  returned: b => ^b value\n  \
             operand: b => (b value: 1) + 2\n  two: a with: b => a value: (b value)\n  \
             viaArgs: b => b valueWithArguments: #(1)\n  cascaded: b => b value; value\n",
        );
        for sel in [
            "stmt:",
            "withArg:",
            "assigned:",
            "returned:",
            "operand:",
            "viaArgs:",
            "cascaded:",
        ] {
            assert_eq!(inst(&idx, "C", sel), vec![0], "{sel}");
        }
        assert_eq!(inst(&idx, "C", "two:with:"), vec![0, 1]);
    }

    #[test]
    fn collection_callable_arguments_thread() {
        let idx = index(
            "Object subclass: C\n  each: b => self items do: b\n  \
             fold: b => self items inject: 0 into: b\n  \
             pick: b => ^self items detect: b ifNone: [nil]\n",
        );
        assert_eq!(inst(&idx, "C", "each:"), vec![0]);
        assert_eq!(inst(&idx, "C", "fold:"), vec![0]);
        assert_eq!(inst(&idx, "C", "pick:"), vec![0]);
    }

    #[test]
    fn disqualifying_uses_do_not_thread() {
        let idx = index(
            "Object subclass: C\n  state: kept = nil\n  stored: b => self.kept := b\n  \
             returnedBare: b => ^b\n  ffi: b => (Erlang lists) foreach: b list: #()\n  \
             asyncSend: b => other foo: b!\n  \
             arity: b => b arity\n  respond: b => b respondsTo: #value\n  \
             inLoop: b => #(1) do: [:e | b value: e]\n  \
             inArm: b => true ifTrue: [b value]\n  \
             nested: b =>\n    f := [b value]\n    f value\n  \
             unused: b => 1\n  \
             outside: b => other foo: b\n  literal: b => #(b)\n",
        );
        for sel in [
            "stored:",
            "returnedBare:",
            "ffi:",
            "asyncSend:",
            "arity:",
            "respond:",
            "inLoop:",
            "inArm:",
            "nested:",
            "unused:",
            "outside:",
            "literal:",
        ] {
            assert!(inst(&idx, "C", sel).is_empty(), "{sel} must not thread");
        }
    }

    #[test]
    fn forward_follows_the_callee() {
        let idx = index(
            "Object subclass: C\n  state: kept = nil\n  fwd: b => self leaf: b\n  \
             leaf: b => b value\n  dead: b => self sink: b\n  sink: b => self.kept := b\n  \
             classFwd: b => C viaClass: b\n  class viaClass: b => b value\n  \
             class selfFwd: b => self viaClass: b\n",
        );
        assert_eq!(inst(&idx, "C", "fwd:"), vec![0]);
        assert!(inst(&idx, "C", "dead:").is_empty());
        assert_eq!(inst(&idx, "C", "classFwd:"), vec![0]);
        // Class side and instance side are separate.
        assert_eq!(idx.params("C", true, "selfFwd:"), vec![0]);
        assert!(idx.params("C", false, "selfFwd:").is_empty());
        assert!(idx.params("C", true, "fwd:").is_empty());
    }

    #[test]
    fn self_recursive_hom_threads() {
        let idx = index("Object subclass: C\n  walk: b =>\n    b value: self\n    self walk: b\n");
        assert_eq!(inst(&idx, "C", "walk:"), vec![0]);
        let only_forward = index("Object subclass: C\n  loop: b => self loop: b\n");
        assert_eq!(inst(&only_forward, "C", "loop:"), vec![0]);
    }

    #[test]
    fn mutually_recursive_pair_threads() {
        let idx = index(
            "Object subclass: C\n  ping: b =>\n    b value\n    self pong: b\n  \
             pong: b =>\n    b value\n    self ping: b\n",
        );
        assert_eq!(inst(&idx, "C", "ping:"), vec![0]);
        assert_eq!(inst(&idx, "C", "pong:"), vec![0]);
    }

    #[test]
    fn mutual_recursion_with_a_storing_member_is_non_threading() {
        let idx = index(
            "Object subclass: C\n  state: kept = nil\n  ping: b =>\n    b value\n    \
             self pong: b\n  pong: b =>\n    self.kept := b\n    self ping: b\n",
        );
        assert!(inst(&idx, "C", "ping:").is_empty());
        assert!(inst(&idx, "C", "pong:").is_empty());
    }

    #[test]
    fn a_cycle_member_outside_the_pair_does_not_break_it() {
        // `entry:` forwards into a sound cycle; `bad:` forwards into the same
        // cycle but also stores the block itself.
        let idx = index(
            "Object subclass: C\n  state: kept = nil\n  \
             entry: b => self ping: b\n  \
             bad: b =>\n    self.kept := b\n    self ping: b\n  \
             ping: b =>\n    b value\n    self pong: b\n  \
             pong: b =>\n    b value\n    self ping: b\n",
        );
        assert_eq!(inst(&idx, "C", "entry:"), vec![0]);
        assert_eq!(inst(&idx, "C", "ping:"), vec![0]);
        assert!(inst(&idx, "C", "bad:").is_empty());
    }

    #[test]
    fn answer_does_not_depend_on_method_order() {
        let a = "  ping: b =>\n    b value\n    self pong: b\n";
        let b = "  pong: b =>\n    self.kept := b\n    self ping: b\n";
        let c = "  entry: b => self ping: b\n";
        let orders = [
            [a, b, c],
            [a, c, b],
            [b, a, c],
            [b, c, a],
            [c, a, b],
            [c, b, a],
        ];
        let mut answers = Vec::new();
        for order in orders {
            let src = format!(
                "Object subclass: C\n  state: kept = nil\n{}",
                order.concat()
            );
            let idx = index(&src);
            answers.push(["ping:", "pong:", "entry:"].map(|sel| inst(&idx, "C", sel)));
        }
        assert!(answers.windows(2).all(|w| w[0] == w[1]), "{answers:?}");
        assert!(answers[0].iter().all(Vec::is_empty), "{answers:?}");
    }

    #[test]
    fn forward_to_a_same_unit_class_resolves() {
        let idx = index(
            "Object subclass: A\n  go: b => B run: b\n\nObject subclass: B\n  class run: b => b value\n",
        );
        assert_eq!(idx.params("A", false, "go:"), vec![0]);
        assert_eq!(idx.params("B", true, "run:"), vec![0]);
    }

    #[test]
    fn actor_channel_predicate_is_unchanged_on_a_cycle() {
        let module = parse("Actor subclass: Ac\n  sealed loop: b => self loop: b\n");
        assert!(!callee_threads_block_param(&module.classes[0], "loop:", 0));
    }
}
