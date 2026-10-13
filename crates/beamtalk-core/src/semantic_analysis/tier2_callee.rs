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

use crate::ast::{Block, ClassDefinition, Expression, ExpressionStatement, MethodDefinition};
use crate::semantic_analysis::block_facts::{
    is_safe_value_family_selector, normalize_cascade, stored_block_var_uses,
};
use crate::state_threading_selectors::{is_opaque_callable_hom_send, opaque_fold_callable_arg};
use ecow::EcoString;
use std::collections::HashSet;

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
        match expr {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                is_name(receiver, self.name)
                    && is_safe_value_family_selector(selector)
                    && !arguments.iter().any(|a| self.mentions(a))
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                let (underlying, all) = normalize_cascade(receiver, messages);
                is_name(underlying, self.name)
                    && !all.is_empty()
                    && all.iter().all(|(sel, args)| {
                        is_safe_value_family_selector(sel) && !args.iter().any(|a| self.mentions(a))
                    })
            }
            _ => false,
        }
    }

    /// `items do: [:e | ... x := name value: e ...]` as a method-body
    /// statement, where `x` is bound before the loop and no other statement of
    /// the body mentions `name`.
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
