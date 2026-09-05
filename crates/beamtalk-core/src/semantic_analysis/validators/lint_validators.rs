// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code quality lint validators.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators that check for code quality issues:
//! - Redundant assignment `x := x` (BT-950)
//! - Literal boolean conditions (BT-955)
//! - Empty method bodies (BT-859)
//! - Effect-free statements (BT-951)

use crate::ast::{
    Expression, Identifier, MessageSelector, Module, TypeAnnotation, WellKnownSelector,
};
use crate::ast_walker::{walk_expression, walk_module};
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::type_checker::{InferredType, TypeMap};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use std::collections::HashMap;

// ── BT-950: Redundant assignment ─────────────────────────────────────────────

/// BT-950: Warn when the RHS of an assignment is the same identifier as the LHS.
///
/// Detects `x := x` where both sides are the same plain identifier binding.
/// This has no effect at runtime and usually indicates a copy-paste error or
/// leftover code.
pub(crate) fn check_redundant_assignment(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        if let Expression::Assignment {
            target,
            value,
            span,
            ..
        } = expr
        {
            if let (
                Expression::Identifier(Identifier { name: lhs, .. }),
                Expression::Identifier(Identifier { name: rhs, .. }),
            ) = (target.as_ref(), value.as_ref())
            {
                if lhs == rhs {
                    diagnostics.push(
                        Diagnostic::warning(
                            format!("Redundant assignment: `{lhs} := {lhs}` has no effect"),
                            *span,
                        )
                        .with_hint("Remove this assignment or assign a different value.")
                        .with_category(DiagnosticCategory::Lint),
                    );
                }
            }
        }
    });
}

// ── BT-955: Literal boolean condition ────────────────────────────────────────

/// BT-955: Warn when a boolean conditional message is sent to a literal boolean receiver.
///
/// When `ifTrue:`, `ifFalse:`, or `ifTrue:ifFalse:` is sent to a literal `true`
/// or `false`, one branch is statically unreachable or the conditional is
/// entirely redundant.
///
/// Examples:
/// - `true ifTrue: [42]`           → condition always true (branch always taken)
/// - `false ifFalse: [42]`         → condition always false (branch always taken)
/// - `true ifFalse: [42]`          → condition always true (`ifFalse:` unreachable)
/// - `false ifTrue: [42]`          → condition always false (`ifTrue:` unreachable)
/// - `true ifTrue: [1] ifFalse: [2]`  → `ifFalse:` branch is dead code
/// - `false ifTrue: [1] ifFalse: [2]` → `ifTrue:` branch is dead code
pub(crate) fn check_literal_boolean_condition(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        check_literal_boolean_condition_at(expr, diagnostics);
    });
}

/// Classifies a selector as a boolean conditional message, returning the
/// matching [`WellKnownSelector`] variant or `None` if it isn't one.
fn boolean_conditional(selector: &MessageSelector) -> Option<WellKnownSelector> {
    match selector.well_known()? {
        sel @ (WellKnownSelector::IfTrue
        | WellKnownSelector::IfFalse
        | WellKnownSelector::IfTrueIfFalse) => Some(sel),
        _ => None,
    }
}

/// Returns a hint string describing the unreachable or redundant branch.
fn dead_branch_hint(is_true: bool, selector: WellKnownSelector) -> &'static str {
    use WellKnownSelector::{IfFalse, IfTrue, IfTrueIfFalse};
    match (is_true, selector) {
        (true, IfTrue) | (false, IfFalse) => {
            "The branch is always taken. Remove the conditional and use the branch body directly."
        }
        (true, IfFalse) | (false, IfTrue) => "This branch is never executed. Remove it.",
        (true, IfTrueIfFalse) => {
            "The `ifFalse:` branch is never executed. Simplify to the `ifTrue:` block."
        }
        (false, IfTrueIfFalse) => {
            "The `ifTrue:` branch is never executed. Simplify to the `ifFalse:` block."
        }
        _ => "Remove the unreachable branch.",
    }
}

/// Returns `Some(true)` if `receiver` is the identifier `true`, `Some(false)` if `false`,
/// or `None` for any other expression.
fn literal_bool_receiver(receiver: &Expression) -> Option<bool> {
    match receiver {
        Expression::Identifier(Identifier { name, .. }) if name == "true" => Some(true),
        Expression::Identifier(Identifier { name, .. }) if name == "false" => Some(false),
        _ => None,
    }
}

/// Checks a single expression node for literal boolean conditional patterns.
///
/// Called by [`walk_module`] via [`check_literal_boolean_condition`]; the walker
/// handles recursive traversal, so this function only inspects the current node.
fn check_literal_boolean_condition_at(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    if let Expression::MessageSend {
        receiver,
        selector,
        span,
        ..
    } = expr
    {
        if let Some(is_true) = literal_bool_receiver(receiver) {
            if let Some(well_known) = boolean_conditional(selector) {
                let literal_name = if is_true { "true" } else { "false" };
                diagnostics.push(
                    Diagnostic::warning(format!("Condition is always `{literal_name}`"), *span)
                        .with_hint(dead_branch_hint(is_true, well_known))
                        .with_category(DiagnosticCategory::Lint),
                );
            }
        }
    }
    // Also check cascade messages — `true ifTrue: [1]; ifFalse: [2]` has a literal
    // boolean receiver with each cascade message going to the same receiver.
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(is_true) = literal_bool_receiver(receiver) {
            let literal_name = if is_true { "true" } else { "false" };
            for msg in messages {
                if let Some(well_known) = boolean_conditional(&msg.selector) {
                    diagnostics.push(
                        Diagnostic::warning(
                            format!("Condition is always `{literal_name}`"),
                            msg.span,
                        )
                        .with_hint(dead_branch_hint(is_true, well_known))
                        .with_category(DiagnosticCategory::Lint),
                    );
                }
            }
        }
    }
}

/// BT-859: Error on empty method bodies.
///
/// Methods declared with `=>` but no body expressions are a compile error.
/// Use `self notImplemented` for work-in-progress stubs, or
/// `self subclassResponsibility` for abstract interface contracts.
/// Creates a diagnostic for a method with an empty body.
fn empty_body_error(selector: &str, span: Span) -> Diagnostic {
    Diagnostic::error(format!("Method `{selector}` has an empty body"), span)
        .with_category(DiagnosticCategory::EmptyBody)
        .with_hint(
            "Use `self notImplemented` for stubs, or `self subclassResponsibility` for abstract methods",
        )
}

pub(crate) fn check_empty_method_bodies(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for method in module.all_methods() {
        if method.body.is_empty() {
            let selector = method.selector.name();
            diagnostics.push(empty_body_error(&selector, method.span));
        }
    }
}

// ── BT-951: Effect-free statement detection ───────────────────────────────────

/// Returns `true` if the expression is pure (no observable side effects).
///
/// Only a conservative subset is classified as pure: literals, variable reads,
/// class references, parenthesized pure expressions, and binary sends with a
/// known arithmetic or comparison operator applied to pure operands.
fn is_effect_free(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(_, _)
        | Expression::Identifier(_)
        | Expression::ClassReference { .. } => true,
        Expression::MapLiteral { pairs, .. } => pairs
            .iter()
            .all(|p| is_effect_free(&p.key) && is_effect_free(&p.value)),
        Expression::ArrayLiteral { elements, .. } => elements.iter().all(is_effect_free),
        Expression::ListLiteral { elements, tail, .. } => {
            elements.iter().all(is_effect_free) && tail.as_ref().is_none_or(|t| is_effect_free(t))
        }
        Expression::Parenthesized { expression, .. } => is_effect_free(expression),
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast,
            ..
        } => {
            if *is_cast {
                return false;
            }
            match selector {
                crate::ast::MessageSelector::Binary(op) => {
                    is_pure_binary_op(op)
                        && is_effect_free(receiver)
                        && arguments.iter().all(is_effect_free)
                }
                _ => false,
            }
        }
        _ => false,
    }
}

/// Returns `true` if a binary operator is a known pure arithmetic/comparison op.
fn is_pure_binary_op(op: &str) -> bool {
    matches!(
        op,
        "+" | "-"
            | "*"
            | "/"
            | "//"
            | "\\\\"
            | "**"
            | "<"
            | ">"
            | "<="
            | ">="
            | "~~"
            | "&"
            | "|"
            | "^"
            | ">>"
            | "<<"
    )
}

/// Returns a short description of the expression kind for diagnostic messages.
fn effect_free_label(expr: &Expression) -> &'static str {
    match expr {
        Expression::Literal(_, _) => "literal",
        Expression::Identifier(_) => "variable reference",
        Expression::ClassReference { .. } => "class reference",
        Expression::MapLiteral { .. } => "map literal",
        Expression::ArrayLiteral { .. } => "array literal",
        Expression::ListLiteral { .. } => "list literal",
        _ => "pure expression",
    }
}

/// Walk an expression to find nested sequences (blocks) that may contain
/// effect-free non-last statements.
fn walk_expr_for_effect_free(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    match expr {
        Expression::Block(block) => {
            check_seq_for_effect_free(&block.body, diagnostics);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            walk_expr_for_effect_free(receiver, diagnostics);
            for arg in arguments {
                walk_expr_for_effect_free(arg, diagnostics);
            }
        }
        Expression::Assignment { target, value, .. } => {
            walk_expr_for_effect_free(target, diagnostics);
            walk_expr_for_effect_free(value, diagnostics);
        }
        Expression::Return { value, .. } => {
            walk_expr_for_effect_free(value, diagnostics);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            walk_expr_for_effect_free(receiver, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    walk_expr_for_effect_free(arg, diagnostics);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => {
            walk_expr_for_effect_free(expression, diagnostics);
        }
        Expression::FieldAccess { receiver, .. } => {
            walk_expr_for_effect_free(receiver, diagnostics);
        }
        Expression::Match { value, arms, .. } => {
            walk_expr_for_effect_free(value, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_expr_for_effect_free(guard, diagnostics);
                }
                walk_expr_for_effect_free(&arm.body, diagnostics);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_expr_for_effect_free(&pair.key, diagnostics);
                walk_expr_for_effect_free(&pair.value, diagnostics);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                walk_expr_for_effect_free(elem, diagnostics);
            }
            if let Some(t) = tail {
                walk_expr_for_effect_free(t, diagnostics);
            }
        }
        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                walk_expr_for_effect_free(elem, diagnostics);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    walk_expr_for_effect_free(e, diagnostics);
                }
            }
        }
        _ => {}
    }
}

/// Check a sequence of expressions: warn on any non-last expression that is
/// effect-free, then recurse into all expressions for nested sequences.
fn check_seq_for_effect_free(
    exprs: &[crate::ast::ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let len = exprs.len();
    for (i, stmt) in exprs.iter().enumerate() {
        let expr = &stmt.expression;
        let is_last = i == len - 1;
        if !is_last && is_effect_free(expr) {
            let label = effect_free_label(expr);
            diagnostics.push(
                Diagnostic::lint(format!("this {label} has no effect"), expr.span()).with_hint(
                    "Remove this expression, or assign its value to a variable if needed.",
                ),
            );
        }
        walk_expr_for_effect_free(expr, diagnostics);
    }
}

/// BT-951: Warn (as a lint) when a statement is an effect-free expression
/// whose value is silently discarded.
///
/// Checks method bodies, standalone method bodies, and (by default)
/// module-level expressions (`module.expressions`). Pass
/// `skip_module_expression_lint = true` to suppress the module-level check —
/// bootstrap-test compilation uses this because those files intentionally use
/// top-level expressions as test assertions paired with `// =>` comments.
///
/// Effect-free expressions include literals, variable references, and pure
/// binary arithmetic / comparison expressions composed from pure sub-expressions.
///
/// Uses `Severity::Lint` so the warning is suppressed during normal compilation
/// and only surfaces when running `beamtalk lint` or in the REPL.
// BT-3340: widened from `pub(crate)` — the standalone `beamtalk-lint`
// crate's `effect_free_statement` pass calls this directly.
pub fn check_effect_free_statements(
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
    skip_module_expression_lint: bool,
) {
    // BT-979: Check module-level expressions unless the caller opts out.
    if !skip_module_expression_lint {
        check_seq_for_effect_free(&module.expressions, diagnostics);
    }
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            check_seq_for_effect_free(&method.body, diagnostics);
        }
    }
    for standalone in &module.method_definitions {
        check_seq_for_effect_free(&standalone.method.body, diagnostics);
    }
}

// BT-1476 validator removed: all control-flow selectors and Tier 2 blocks
// now have state threading. The dead_block_assignment lint (lint/) remains
// for `beamtalk lint` usage with @expect dead_assignment suppression.

// ── BT-1955: Redundant `super initialize` in Actor initialize methods ─────────

/// BT-1955: Warn when an `initialize` method on an Actor subclass contains
/// an explicit `super initialize` send.
///
/// ADR 0078 Phase 2: with auto-chained `initialize` (BT-1951), parent
/// `initialize` methods run automatically before the child's. An explicit
/// `super initialize` in the body causes the parent's `initialize` to run
/// twice — once from the auto-chain, once from the explicit send.
///
/// The warning fires only for:
/// - Methods whose selector is the unary `initialize`
/// - That are instance methods on an Actor subclass (auto-chain only happens
///   in `handle_continue` for actors; non-actor classes still need explicit
///   `super` calls if they want parent behavior).
///
/// The warning does NOT fire for:
/// - `super` sends to other selectors (e.g. `super foo`)
/// - `super initialize` outside of an `initialize` method
/// - Class-side `class initialize: ...` keyword methods (different selector)
pub(crate) fn check_redundant_super_initialize(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        if !hierarchy.is_actor_subclass(class_name) {
            continue;
        }
        for method in &class.methods {
            if !is_unary_initialize(&method.selector) {
                continue;
            }
            check_method_body_for_super_initialize(&method.body, diagnostics);
        }
    }
    // Also check standalone (Tonel-style) method definitions — but only
    // instance-side. Class-side `initialize` is not auto-chained, so an
    // explicit `super initialize` there is not redundant.
    for standalone in &module.method_definitions {
        if standalone.is_class_method {
            continue;
        }
        let class_name = standalone.class_name.name.as_str();
        if !hierarchy.is_actor_subclass(class_name) {
            continue;
        }
        if !is_unary_initialize(&standalone.method.selector) {
            continue;
        }
        check_method_body_for_super_initialize(&standalone.method.body, diagnostics);
    }
}

/// Returns `true` if the selector is the unary `initialize` message.
fn is_unary_initialize(selector: &MessageSelector) -> bool {
    matches!(selector, MessageSelector::Unary(name) if name.as_str() == "initialize")
}

/// Walks every expression in the body of an `initialize` method and emits a
/// warning for each `super initialize` send found (including those nested
/// inside blocks, conditionals, etc.).
fn check_method_body_for_super_initialize(
    body: &[crate::ast::ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    for stmt in body {
        walk_expression(&stmt.expression, &mut |expr| {
            match expr {
                Expression::MessageSend {
                    receiver,
                    selector,
                    span,
                    ..
                } if matches!(receiver.as_ref(), Expression::Super(_))
                    && is_unary_initialize(selector) =>
                {
                    diagnostics.push(redundant_super_initialize_diagnostic(*span));
                }
                // Also detect `super initialize` as a cascade message, e.g.
                // `super initialize; foo` — the cascade's receiver is Super,
                // and each message in the cascade dispatches to that receiver.
                Expression::Cascade {
                    receiver, messages, ..
                } if matches!(receiver.as_ref(), Expression::Super(_)) => {
                    for msg in messages {
                        if is_unary_initialize(&msg.selector) {
                            diagnostics.push(redundant_super_initialize_diagnostic(msg.span));
                        }
                    }
                }
                _ => {}
            }
        });
    }
}

/// Builds the BT-1955 diagnostic for a redundant `super initialize` send.
fn redundant_super_initialize_diagnostic(span: Span) -> Diagnostic {
    Diagnostic::warning(
        "explicit `super initialize` is unnecessary — \
         parent initializers run automatically"
            .to_string(),
        span,
    )
    .with_hint("Remove this line — Beamtalk auto-chains initialize up the hierarchy")
    .with_category(DiagnosticCategory::Lint)
}

// ── BT-3391/BT-3395: setUp drops field mutations unless it ends in self ──────

/// BT-3391/BT-3395: Warn when a `TestCase` subclass's `setUp` method mutates
/// a field — via `self.field := value` or a `with<Field>:` send — but its
/// last statement isn't itself self-producing.
///
/// `TestCase` is a `Value subclass:` (BT-1533 exempts `self.field :=` there
/// from the general value-immutability error, pending the `with*:` migration
/// tracked by BT-1534). Value-type method bodies return the value of their
/// *last* expression, with two special cases: a `self.field := value`
/// assignment in last position evaluates to the updated `self` rather than
/// the assigned value (BT-833/BT-900), and a `with<Field>:` send — the
/// auto-generated copy-setter naming convention for every `Value` field
/// (`crate::synthetic_selectors::with_star_selector`) — always returns a new,
/// fully updated self by construction. Either shape only carries the
/// mutation forward when it IS the last statement (a cascade of `with*:`
/// sends included, since a cascade's value is its last message's result).
/// Any other trailing statement (an unrelated local assignment, a `super
/// setUp` call, a log line, …) makes the method return *that* statement's own
/// value instead, silently discarding every field mutation made earlier in
/// the body — the test runner then threads the wrong `self` into every test
/// method, with no compile or runtime error.
///
/// Fires only for `setUp`: it is the one `TestCase` lifecycle method whose
/// return value the runner threads as the receiver of each test method (see
/// `stdlib/src/test_case.bt`'s class doc comment). `tearDown` returns `Nil`
/// and `setUpOnce`'s return is stored as a suite fixture, not threaded as
/// `self`, so neither is affected by this trap.
///
/// Does NOT fire when:
/// - `setUp` contains no `self.field := ...` assignment and no `self
///   with<Field>: ...` send at all — there's nothing to drop.
/// - The last statement is a bare `self`, another `self.field := value`
///   assignment, a `self with<Field>: value` send, or a cascade on `self`
///   ending in a `with<Field>:` message — all reliably yield the fully
///   updated self already.
pub(crate) fn check_testcase_setup_drops_field_assignments(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = class.name.name.as_str();
        if !hierarchy.is_testcase_subclass(class_name) {
            continue;
        }
        for method in &class.methods {
            if is_unary_setup(&method.selector) {
                check_setup_body_returns_self(&method.body, diagnostics);
            }
        }
    }
    // Also check standalone (Tonel-style) method definitions.
    for standalone in &module.method_definitions {
        if standalone.is_class_method {
            continue;
        }
        let class_name = standalone.class_name.name.as_str();
        if !hierarchy.is_testcase_subclass(class_name) {
            continue;
        }
        if is_unary_setup(&standalone.method.selector) {
            check_setup_body_returns_self(&standalone.method.body, diagnostics);
        }
    }
}

/// Returns `true` if the selector is the unary `setUp` message.
fn is_unary_setup(selector: &MessageSelector) -> bool {
    matches!(selector, MessageSelector::Unary(name) if name.as_str() == "setUp")
}

/// Returns `true` if `expr` is `self.field := value` (any field name).
fn is_self_field_assignment(expr: &Expression) -> bool {
    let Expression::Assignment { target, .. } = expr.unwrap_parens() else {
        return false;
    };
    let Expression::FieldAccess { receiver, .. } = target.as_ref() else {
        return false;
    };
    matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
}

/// Returns `true` if `expr` is the bare identifier `self`.
fn is_bare_self(expr: &Expression) -> bool {
    matches!(expr.unwrap_parens(), Expression::Identifier(id) if id.name == "self")
}

/// Returns `true` if `selector` is a single-keyword-part send following the
/// `with<Field>:` copy-setter naming convention recognized by
/// [`crate::synthetic_selectors::is_with_star_selector`] — the shared
/// recognition counterpart to `with_star_selector`'s generation, also used
/// by `beamtalk-lint`'s `ValueLikeObjectPass` (`value_like_object.rs`) to
/// spot hand-written `withX:` setter methods. Structural, like
/// `is_self_field_assignment`: it recognizes the *shape* of the convention
/// rather than looking up whether `Field` is a real field on some class.
fn is_with_star_selector(selector: &MessageSelector) -> bool {
    let MessageSelector::Keyword(parts) = selector else {
        return false;
    };
    if parts.len() != 1 {
        return false;
    }
    crate::synthetic_selectors::is_with_star_selector(&selector.name())
}

/// Returns `true` if `expr` is a `with<Field>:` send whose receiver is
/// itself self-producing — either literal `self` (`self withA: x`) or
/// another self-producing expression, recursively, so an arbitrarily deep
/// `with*:` chain (`((self withA: x) withB: y) withC: z`) is recognized
/// throughout, not just its outermost send. See [`is_self_producing`].
fn is_self_with_field_send(expr: &Expression) -> bool {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = expr.unwrap_parens()
    else {
        return false;
    };
    arguments.len() == 1 && is_with_star_selector(selector) && is_self_producing(receiver)
}

/// Returns `true` if `expr` reliably evaluates to the fully updated `self`
/// — a bare `self`, a `self.field := value` assignment, a `with<Field>:`
/// send (recursively, so a chain of them also qualifies), or a `with*:`
/// cascade whose *last* message is a `with<Field>:` send. This is the
/// shared "self-producing" predicate: a `with<Field>:` send's receiver must
/// satisfy it (chaining), and so must the trailing statement
/// `check_setup_body_returns_self` requires — both need to know what the
/// expression actually *evaluates to*, so a cascade is checked by its last
/// message only (ADR 0067), unlike the presence-only
/// [`is_self_with_field_cascade`] used for detecting *that* a mutation
/// happened anywhere in the body.
fn is_self_producing(expr: &Expression) -> bool {
    is_bare_self(expr)
        || is_self_field_assignment(expr)
        || is_self_with_field_send(expr)
        || cascade_value_is_self_producing(expr)
}

/// Returns the cascade's common receiver — the receiver of the message
/// embedded in the `Cascade` node's own `receiver` field, which the parser
/// always fills with the cascade's first message send (see
/// `ast_walker::tests::walk_cascade_visits_receiver_and_message_arguments`).
/// `None` for the (grammatically unreachable in practice) case where that
/// field isn't itself a message send.
fn cascade_common_receiver(receiver: &Expression) -> Option<&Expression> {
    match receiver.unwrap_parens() {
        Expression::MessageSend { receiver, .. } => Some(receiver.as_ref()),
        _ => None,
    }
}

/// Returns `true` if `expr` is a cascade whose common receiver is
/// self-producing (literal `self`, or itself a `with<Field>:` send/chain —
/// recursively, via [`is_self_producing`]) and *at least one* of its
/// messages is a `with<Field>:` send — e.g. `self withCounter: 1; withDb:
/// 2`, or a cascade with an unrelated message mixed in. Presence-only: this
/// answers "did a field mutation happen anywhere in this cascade", which is
/// what [`contains_self_reconstructing_send`] needs to decide whether
/// `setUp` has anything to drop at all. It does NOT tell you whether the
/// cascade's own *value* is self — for that (whether this expression, in
/// trailing position, actually carries the mutation forward), use
/// [`cascade_value_is_self_producing`], which requires the *last* message to
/// be the `with<Field>:` send (ADR 0067: a cascade evaluates to its last
/// message's result) — the distinction a 3+-message cascade like `self
/// withCounter: 1; withDb: 2; log: "ready"` depends on: this function
/// answers "true" (a mutation happened), but the cascade's value is `log:`'s
/// result, not self, so `cascade_value_is_self_producing` must answer
/// "false" for the same expression.
fn is_self_with_field_cascade(expr: &Expression) -> bool {
    let Expression::Cascade {
        receiver, messages, ..
    } = expr.unwrap_parens()
    else {
        return false;
    };
    cascade_common_receiver(receiver).is_some_and(is_self_producing)
        && messages
            .iter()
            .any(|msg| msg.arguments.len() == 1 && is_with_star_selector(&msg.selector))
}

/// Returns `true` if `expr` is a cascade whose common receiver is
/// self-producing AND whose *last* message is a `with<Field>:` send — the
/// only shape of cascade that actually evaluates to the fully updated self
/// (ADR 0067: a cascade's value is its last message's result). Used by
/// [`is_self_producing`] wherever the code needs to know what an expression
/// evaluates to (a chained receiver, or `setUp`'s trailing statement) —
/// never for mere presence detection, where [`is_self_with_field_cascade`]
/// applies instead.
fn cascade_value_is_self_producing(expr: &Expression) -> bool {
    let Expression::Cascade {
        receiver, messages, ..
    } = expr.unwrap_parens()
    else {
        return false;
    };
    cascade_common_receiver(receiver).is_some_and(is_self_producing)
        && messages
            .last()
            .is_some_and(|msg| msg.arguments.len() == 1 && is_with_star_selector(&msg.selector))
}

/// Returns `true` if `expr` contains a self-reconstructing send anywhere in
/// its subtree (including itself) — a `self.field := value` assignment, a
/// `with<Field>:` send/chain, or a `with*:` cascade (see
/// [`is_self_producing`], minus the bare-`self` case, which mutates
/// nothing). Each of these shapes returns the fully updated `self`; when the
/// containing `setUp` doesn't return that value as its very last statement,
/// the mutation is silently dropped (BT-3391, BT-3395).
fn contains_self_reconstructing_send(expr: &Expression) -> bool {
    let mut found = false;
    walk_expression(expr, &mut |e| {
        if is_self_field_assignment(e)
            || is_self_with_field_send(e)
            || is_self_with_field_cascade(e)
        {
            found = true;
        }
    });
    found
}

/// Checks one `setUp` method body: warns when it mutates a field via
/// `self.field := value` or a `with<Field>:` send but the last statement
/// won't carry those mutations forward as the returned `self`.
fn check_setup_body_returns_self(
    body: &[crate::ast::ExpressionStatement],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(last_stmt) = body.last() else {
        return;
    };
    let has_field_mutation = body
        .iter()
        .any(|stmt| contains_self_reconstructing_send(&stmt.expression));
    if !has_field_mutation {
        return;
    }
    let last_expr = last_stmt.expression.unwrap_parens();
    if is_self_producing(last_expr) {
        return;
    }
    diagnostics.push(setup_drops_field_assignments_diagnostic(last_expr.span()));
}

/// Builds the BT-3391 diagnostic for a `setUp` whose trailing statement
/// drops earlier `self.field :=` mutations.
fn setup_drops_field_assignments_diagnostic(span: Span) -> Diagnostic {
    Diagnostic::warning(
        "`setUp` mutates a field earlier in the method (via `self.field := \
         ...` or a `with<Field>:` send), but its last statement doesn't \
         return the updated self — every field mutation is silently dropped \
         for the test method"
            .to_string(),
        span,
    )
    .with_hint(
        "End `setUp` with a bare `self` — or make the last statement a \
         `self.field := value` assignment, a `self with<Field>: value` \
         send, or a `with<Field>:` cascade — so the updated fields carry \
         forward",
    )
    .with_category(DiagnosticCategory::Lint)
}

// ── BT-2140: Redundant local-variable type annotation ────────────────────────

/// BT-2140: Lint when a local-variable assignment carries a `:: T` annotation
/// whose resolved type exactly matches the inferred type of the right-hand
/// side.
///
/// Triggers on `name :: T := <expr>` when the static type of `<expr>` is
/// exactly `T` (not a subtype, not narrowed via union). Skips:
///
/// - Union and false-or annotations (`T | nil`, `T?`) — load-bearing for widening.
/// - RHS whose inferred type is `Dynamic`, `Never`, or `Union` — the annotation
///   is supplying real information the inferer could not derive.
/// - Field assignments (`self.x := ...`), destructuring assignments, and any
///   non-`Identifier` target — those don't carry user-written type annotations
///   in this lint's scope.
/// - Block parameters / state declarations / method params — those use their
///   own AST nodes, not [`Expression::Assignment`].
///
/// Suppressed by `@expect type_annotation` on the enclosing statement.
pub(crate) fn check_redundant_local_type_annotation(
    module: &Module,
    type_map: &TypeMap,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // A future refinement could thread the surrounding method's type-param
    // subst, but for the common cases (`Counter`, `List(Foo)`) the resolved
    // form matches the inferer's symbolic placeholders without a subst.
    // Hoisted out of the walker so we don't re-allocate per assignment.
    let empty_subst: HashMap<ecow::EcoString, InferredType> = HashMap::new();
    walk_module(module, &mut |expr| {
        let Expression::Assignment {
            target,
            value,
            type_annotation: Some(annotation),
            span,
        } = expr
        else {
            return;
        };
        // Only flag plain local-variable targets. `self.x :: T := ...` is not
        // valid Beamtalk syntax for fields, but guard defensively.
        if !matches!(target.as_ref(), Expression::Identifier(_)) {
            return;
        }
        // Widening annotations are load-bearing: keep them. A difference
        // (`Symbol \ #foo`) or intersection (`P1 & P2`) narrows/composes the
        // RHS type, so both are load-bearing too.
        if matches!(
            annotation,
            TypeAnnotation::Union { .. }
                | TypeAnnotation::FalseOr { .. }
                | TypeAnnotation::Difference { .. }
                | TypeAnnotation::Intersection { .. }
        ) {
            return;
        }

        let Some(rhs_ty) = type_map.get(value.span()) else {
            return;
        };

        // Only flag when the RHS resolves to a single Known type. Dynamic,
        // Never, and Union RHS types mean the annotation is doing real work.
        let InferredType::Known {
            class_name: rhs_class,
            type_args: rhs_args,
            ..
        } = rhs_ty
        else {
            return;
        };

        // ADR 0108 (BT-2895): no alias registry threaded here — this lint
        // only fires on an exact `Known` match between annotation and RHS,
        // so an alias-typed local (e.g. `heading :: Direction := ...`)
        // simply resolves as an opaque unknown class, never spuriously
        // matches the RHS, and the lint stays silent (no false positive).
        let resolved = crate::semantic_analysis::type_checker::resolve_type_annotation(
            annotation,
            &empty_subst,
            None,
            None,
        );
        let InferredType::Known {
            class_name: ann_class,
            type_args: ann_args,
            ..
        } = resolved
        else {
            return;
        };

        // Exact match — annotation adds no information.
        // Type-arg equality also catches narrowing cases like
        // `List(Foo) := List new`, where the RHS's `Dynamic` element type
        // doesn't equal the annotation's concrete `Foo`.
        if ann_class != *rhs_class || ann_args != *rhs_args {
            return;
        }

        let ann_str = InferredType::Known {
            class_name: ann_class.clone(),
            type_args: ann_args.clone(),
            provenance: crate::semantic_analysis::type_checker::TypeProvenance::Declared(*span),
        }
        .display_for_diagnostic()
        .unwrap_or_else(|| ann_class.clone());

        diagnostics.push(
            Diagnostic::lint(
                format!(
                    "Redundant type annotation: `:: {ann_str}` matches the inferred type of the right-hand side",
                ),
                annotation.span(),
            )
            .with_hint(format!(
                "Drop the `:: {ann_str}` annotation — the right-hand side already has this type. \
                 Suppress with `@expect type_annotation` if the explicit annotation is intentional.",
            ))
            .with_category(DiagnosticCategory::TypeAnnotation),
        );
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Severity;
    use crate::source_analysis::lex_with_eof;
    use crate::source_analysis::parse;

    // ── BT-951: Effect-free statement tests ──────────────────────────────────

    /// A lone literal in a method body is its return value — no lint warning.
    #[test]
    fn single_literal_in_method_no_lint() {
        let src = "Object subclass: Foo\n  bar => 42";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert!(
            diagnostics.is_empty(),
            "Expected no lint for single literal return value, got: {diagnostics:?}"
        );
    }

    /// A literal appearing as a non-last statement should produce a lint.
    #[test]
    fn literal_non_last_statement_emits_lint() {
        let src = "Object subclass: Foo\n  bar =>\n    42\n    self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded literal, got: {diagnostics:?}"
        );
        assert_eq!(
            diagnostics[0].severity,
            Severity::Lint,
            "Expected Lint severity"
        );
        assert!(
            diagnostics[0].message.contains("literal"),
            "Expected 'literal' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// A string literal as a non-last statement should produce a lint.
    #[test]
    fn string_literal_non_last_emits_lint() {
        let src = "Object subclass: Foo\n  bar =>\n    \"hello\"\n    self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded string literal, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Lint);
    }

    /// `x + y` as a non-last statement (pure arithmetic) should produce a lint.
    #[test]
    fn pure_binary_expr_non_last_emits_lint() {
        let src = "Object subclass: Foo\n  bar: x and: y =>\n    x + y\n    self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for pure binary expression, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Lint);
        assert!(
            diagnostics[0].message.contains("pure expression"),
            "Got: {}",
            diagnostics[0].message
        );
    }

    /// A message send with side effects (keyword send) should NOT produce a lint.
    #[test]
    fn effectful_send_no_lint() {
        let src = "Object subclass: Foo\n  bar =>\n    self doSomething\n    self doOtherThing";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert!(
            diagnostics.is_empty(),
            "Expected no lint for effectful sends, got: {diagnostics:?}"
        );
    }

    /// A literal inside a block in non-last position should produce a lint.
    #[test]
    fn literal_in_block_non_last_emits_lint() {
        let src = "Object subclass: Foo\n  bar => [42. self doSomething] value";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded literal in block, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Lint);
    }

    /// Multiple effect-free statements produce one lint each.
    #[test]
    fn multiple_effect_free_stmts_emit_multiple_lints() {
        let src = "Object subclass: Foo\n  bar =>\n    42\n    \"hello\"\n    self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            2,
            "Expected 2 lints for two discarded literals, got: {diagnostics:?}"
        );
    }

    /// BT-979: Module-level expressions ARE linted by default.
    #[test]
    fn module_level_effect_free_linted_by_default() {
        let src = "42.\nself doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded module-level literal, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Lint);
    }

    /// BT-979: Module-level expressions NOT linted when opt-out flag is set.
    #[test]
    fn module_level_effect_free_skipped_with_flag() {
        let src = "42.\nself doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, true);
        assert!(
            diagnostics.is_empty(),
            "Expected no lint when skip_module_expression_lint is true, got: {diagnostics:?}"
        );
    }

    /// Standalone method definition: non-last literal triggers lint.
    #[test]
    fn standalone_method_effect_free_emits_lint() {
        let src = "Object subclass: Foo\nFoo >> bar =>\n  42\n  self doSomething";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        assert_eq!(
            module.method_definitions.len(),
            1,
            "Expected 1 standalone method"
        );
        let mut diagnostics = Vec::new();
        check_effect_free_statements(&module, &mut diagnostics, false);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 lint for discarded literal in standalone method, got: {diagnostics:?}"
        );
    }

    // ── BT-950: Redundant assignment tests ───────────────────────────────────

    /// `x := x` at the top level emits a warning.
    #[test]
    fn redundant_assignment_top_level_warns() {
        let src = "x := 1.\nx := x";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for redundant assignment, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("Redundant assignment"),
            "Expected 'Redundant assignment' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0].message.contains("x := x"),
            "Expected 'x := x' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `x := y` (different names) does NOT warn.
    #[test]
    fn non_redundant_assignment_no_warn() {
        let src = "x := 1.\ny := 2.\nx := y";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for non-redundant assignment, got: {diagnostics:?}"
        );
    }

    /// `x := x` inside a method body (using a parameter) emits a warning.
    #[test]
    fn redundant_assignment_in_method_warns() {
        let src = "Object subclass: Foo\n  withX: x => x := x";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for redundant assignment in method, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// `x := x` nested inside a block emits a warning.
    #[test]
    fn redundant_assignment_in_block_warns() {
        let src = "Object subclass: Foo\n  withX: x => [x := x] value";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for redundant assignment inside block, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// `self.x := self.x` (field access, not plain identifiers) does NOT trigger
    /// the redundant-assignment check (that's a separate BT-914 concern).
    #[test]
    fn field_access_assignment_not_flagged_as_redundant() {
        let src = "Object subclass: Foo\n  state: x = 0\n  noOp => self.x := self.x";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no redundant-assignment warning for field access, got: {diagnostics:?}"
        );
    }

    /// `x := x` inside a standalone method definition warns.
    #[test]
    fn redundant_assignment_in_standalone_method_warns() {
        let src = "Object subclass: Foo\n  value => 1\nFoo >> withX: x => x := x";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_redundant_assignment(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for redundant assignment in standalone method, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    // ── BT-955: Literal boolean condition tests ───────────────────────────────

    /// `true ifTrue: [42]` — condition always true, branch always taken.
    #[test]
    fn literal_true_if_true_warns() {
        let src = "true ifTrue: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for true ifTrue:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `true`"),
            "Expected 'always `true`' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `false ifFalse: [42]` — condition always false, branch always taken.
    #[test]
    fn literal_false_if_false_warns() {
        let src = "false ifFalse: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for false ifFalse:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `false`"),
            "Expected 'always `false`' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `true ifFalse: [42]` — condition always true, `ifFalse:` branch unreachable.
    #[test]
    fn literal_true_if_false_warns() {
        let src = "true ifFalse: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for true ifFalse:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `true`"),
            "Expected 'always `true`' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `false ifTrue: [42]` — condition always false, `ifTrue:` branch unreachable.
    #[test]
    fn literal_false_if_true_warns() {
        let src = "false ifTrue: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for false ifTrue:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `false`"),
            "Expected 'always `false`' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `true ifTrue: [1] ifFalse: [2]` — `ifFalse:` branch is dead code.
    #[test]
    fn literal_true_if_true_if_false_warns() {
        let src = "true ifTrue: [1] ifFalse: [2]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for true ifTrue:ifFalse:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `true`"),
            "Expected 'always `true`' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .is_some_and(|h| h.contains(WellKnownSelector::IfFalse.as_str())),
            "Expected hint to mention `ifFalse:`, got: {:?}",
            diagnostics[0].hint
        );
    }

    /// `false ifTrue: [1] ifFalse: [2]` — `ifTrue:` branch is dead code.
    #[test]
    fn literal_false_if_true_if_false_warns() {
        let src = "false ifTrue: [1] ifFalse: [2]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for false ifTrue:ifFalse:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("always `false`"),
            "Expected 'always `false`' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .is_some_and(|h| h.contains(WellKnownSelector::IfTrue.as_str())),
            "Expected hint to mention `ifTrue:`, got: {:?}",
            diagnostics[0].hint
        );
    }

    /// A non-literal receiver does NOT trigger the warning.
    #[test]
    fn non_literal_receiver_no_warn() {
        let src = "x := true.\nx ifTrue: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for non-literal receiver, got: {diagnostics:?}"
        );
    }

    /// A non-boolean conditional selector does NOT trigger the warning.
    #[test]
    fn non_conditional_selector_no_warn() {
        let src = "true printString";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for non-conditional selector, got: {diagnostics:?}"
        );
    }

    /// Warning fires inside a method body.
    #[test]
    fn literal_bool_condition_in_method_warns() {
        let src = "Object subclass: Foo\n  run => true ifFalse: [42]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning inside method body, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// Warning fires inside a nested block.
    #[test]
    fn literal_bool_condition_in_block_warns() {
        let src = "Object subclass: Foo\n  run => [false ifTrue: [1]] value";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning inside nested block, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// Message with literal boolean receiver should warn (receiver of first message before cascade).
    /// `true ifTrue: [1]; ifFalse: [2]` — warns for the first message's receiver.
    #[test]
    fn literal_bool_cascade_warns_for_each_message() {
        let src = "true ifTrue: [1]; ifFalse: [2]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_literal_boolean_condition(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for cascade starting with literal true, got: {diagnostics:?}"
        );
        assert!(diagnostics.iter().all(|d| d.severity == Severity::Warning));
        assert!(
            diagnostics
                .iter()
                .all(|d| d.message.contains("always `true`")),
            "Expected all messages to say 'always `true`', got: {diagnostics:?}"
        );
    }

    // ── BT-1955: Redundant `super initialize` tests ───────────────────────────

    /// Helper: build module + class hierarchy from source for BT-1955 tests.
    fn build_module_and_hierarchy(src: &str) -> (crate::ast::Module, ClassHierarchy) {
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        // Only treat parser Errors as hard failures — Warnings and Lint
        // diagnostics (e.g. redundant `.`, unattached doc comments) don't
        // invalidate the AST we need for the test.
        let hard_errs: Vec<_> = parse_diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(hard_errs.is_empty(), "Parse failed: {hard_errs:?}");
        let (hierarchy, _diags) = ClassHierarchy::build(&module);
        (module, hierarchy.expect("hierarchy build failed"))
    }

    /// `super initialize` inside an Actor subclass `initialize` method warns.
    #[test]
    fn super_initialize_in_actor_initialize_warns() {
        let src = "Actor subclass: MyActor\n  initialize =>\n    super initialize.\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_redundant_super_initialize(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("super initialize"),
            "Expected 'super initialize' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .is_some_and(|h| h.contains("auto-chains")),
            "Expected hint about auto-chaining, got: {:?}",
            diagnostics[0].hint
        );
    }

    /// `super foo` (different selector) inside `initialize` does NOT warn.
    #[test]
    fn super_other_selector_in_initialize_no_warn() {
        let src = "Actor subclass: MyActor\n  initialize =>\n    super foo.\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_redundant_super_initialize(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for `super foo`, got: {diagnostics:?}"
        );
    }

    /// `super initialize` outside an `initialize` method does NOT warn.
    #[test]
    fn super_initialize_outside_initialize_no_warn() {
        let src = "Actor subclass: MyActor\n  start =>\n    super initialize.\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_redundant_super_initialize(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for `super initialize` in non-initialize method, got: {diagnostics:?}"
        );
    }

    /// `super initialize` inside a non-actor class does NOT warn (no auto-chain).
    #[test]
    fn super_initialize_in_non_actor_no_warn() {
        let src = "Object subclass: MyObj\n  initialize =>\n    super initialize.\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_redundant_super_initialize(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for non-actor class, got: {diagnostics:?}"
        );
    }

    /// `super initialize` nested inside a block (e.g. `[super initialize] value`)
    /// inside an Actor `initialize` method also warns.
    #[test]
    fn super_initialize_nested_in_block_warns() {
        let src =
            "Actor subclass: MyActor\n  initialize =>\n    [super initialize] value.\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_redundant_super_initialize(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for nested super initialize, got: {diagnostics:?}"
        );
    }

    /// Standalone (Tonel-style) method definitions are also checked.
    #[test]
    fn super_initialize_in_standalone_method_warns() {
        let src = "Actor subclass: MyActor\n  value => 1\nMyActor >> initialize =>\n    super initialize.\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        assert_eq!(module.method_definitions.len(), 1);
        let mut diagnostics = Vec::new();
        check_redundant_super_initialize(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning in standalone method, got: {diagnostics:?}"
        );
    }

    /// `super initialize; foo` (cascade) — the `initialize` cascade message
    /// also warns.
    #[test]
    fn super_initialize_in_cascade_warns() {
        let src = "Actor subclass: MyActor\n  initialize =>\n    super initialize; foo";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_redundant_super_initialize(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for cascade starting with super initialize, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    /// Class-side `class initialize: ...` (keyword) does NOT trigger the warning,
    /// because the unary `initialize` selector is required.
    #[test]
    fn class_keyword_initialize_no_warn() {
        let src =
            "Actor subclass: MyActor\n  class initialize: x =>\n    super initialize.\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_redundant_super_initialize(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for class-side keyword initialize, got: {diagnostics:?}"
        );
    }

    // ── BT-3391: setUp drops field assignments tests ──────────────────────────

    /// A single `self.field := value` statement, with nothing after it — the
    /// documented-as-working idiom (BT-833/BT-900 special-cases the last
    /// position). No warning.
    #[test]
    fn setup_single_field_assignment_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: dashboard = nil\n  setUp =>\n    self.dashboard := 1";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for setUp ending in self.field :=, got: {diagnostics:?}"
        );
    }

    /// The exact BT-3391 repro: a field assignment followed by an unrelated
    /// trailing statement — warns.
    #[test]
    fn setup_field_assignment_then_unrelated_statement_warns() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: dashboard = nil\n  setUp =>\n    self.dashboard := 1.\n    extra := 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("silently dropped"),
            "Expected 'silently dropped' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// A trailing `super setUp` call after a field assignment also warns —
    /// explicitly called out as a breaking case in BT-3391.
    #[test]
    fn setup_field_assignment_then_super_setup_warns() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: dashboard = nil\n  setUp =>\n    self.dashboard := 1.\n    super setUp";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for trailing super setUp, got: {diagnostics:?}"
        );
    }

    /// Ending with an explicit trailing `self` after the extra statement —
    /// the documented fix. No warning.
    #[test]
    fn setup_field_assignment_then_extra_then_self_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: dashboard = nil\n  setUp =>\n    self.dashboard := 1.\n    extra := 2.\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings when setUp ends in explicit self, got: {diagnostics:?}"
        );
    }

    /// Two field assignments in a row, ending in the second — still safe
    /// (each field-assignment-in-last-position threads the full self).
    #[test]
    fn setup_two_field_assignments_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: a = nil\n  field: b = nil\n  setUp =>\n    self.a := 1.\n    self.b := 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for two field assignments ending in the last, got: {diagnostics:?}"
        );
    }

    /// The documented `self withCounter: ...` idiom as `setUp`'s sole/trailing
    /// statement — its own return value already threads the updated self.
    /// No warning.
    #[test]
    fn setup_with_selector_idiom_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: CounterTest\n  field: counter = nil\n  setUp =>\n    self withCounter: 1";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for with*: idiom as the trailing statement, got: {diagnostics:?}"
        );
    }

    // ── BT-3395: with*: chain variant of the same trap ────────────────────────

    /// A single `with<Field>:` send followed by an unrelated trailing
    /// statement — the exact BT-3395 repro. Warns, same as the
    /// `self.field :=` form.
    #[test]
    fn setup_with_field_send_then_unrelated_statement_warns() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: DbTest\n  field: db = nil\n  setUp =>\n    self withDb: 1.\n    Transcript show: \"ready\"";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for with<Field>: send followed by an unrelated \
             statement, got: {diagnostics:?}"
        );
        assert!(
            diagnostics[0].message.contains("silently dropped"),
            "Expected 'silently dropped' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Ending with an explicit trailing `self` after the extra statement
    /// fixes the `with<Field>:` variant too. No warning.
    #[test]
    fn setup_with_field_send_then_extra_then_self_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: DbTest\n  field: db = nil\n  setUp =>\n    self withDb: 1.\n    Transcript show: \"ready\".\n    self";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings when setUp ends in explicit self, got: {diagnostics:?}"
        );
    }

    /// A `with*:` cascade (`self withCounter: 1; withDb: 2`) as the sole
    /// statement — ADR 0067's documented chained-setter idiom. The cascade's
    /// own value is its last message's result, so this threads the full
    /// self forward. No warning.
    #[test]
    fn setup_with_field_cascade_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: CounterDbTest\n  field: counter = nil\n  field: db = nil\n\
                   \n  setUp =>\n    self withCounter: 1; withDb: 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for a with*: cascade as the trailing statement, got: {diagnostics:?}"
        );
    }

    /// The same `with*:` cascade, but followed by an unrelated trailing
    /// statement — warns, since the cascade's self-preserving value is
    /// discarded once something follows it.
    #[test]
    fn setup_with_field_cascade_then_unrelated_statement_warns() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: CounterDbTest\n  field: counter = nil\n  field: db = nil\n\
                   \n  setUp =>\n    self withCounter: 1; withDb: 2.\n    Transcript show: \"ready\"";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for a with*: cascade followed by an unrelated \
             statement, got: {diagnostics:?}"
        );
    }

    /// Mixed `self.field := value` and `self with<Field>: value` sends in the
    /// same body, ending in an unrelated statement — warns; either form
    /// mutating a field earlier is enough to trigger the check.
    #[test]
    fn setup_mixed_field_assignment_and_with_field_send_warns() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: MixedTest\n  field: counter = nil\n  field: db = nil\n\
                   \n  setUp =>\n    self.counter := 1.\n    self withDb: 2.\n    Transcript show: \"ready\"";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for mixed self.field := and with<Field>: sends, got: {diagnostics:?}"
        );
    }

    /// Mixed forms ending in a `with<Field>:` send — no warning, since the
    /// trailing `with<Field>:` send itself carries every earlier mutation
    /// (both the `self.field :=` and the earlier `with*:`) forward as self.
    #[test]
    fn setup_mixed_field_assignment_and_with_field_send_ending_in_with_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: MixedTest\n  field: counter = nil\n  field: db = nil\n\
                   \n  setUp =>\n    self.counter := 1.\n    Transcript show: \"ready\".\n    self withDb: 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings when setUp ends in a with<Field>: send, got: {diagnostics:?}"
        );
    }

    /// A `with:` send whose keyword part isn't capitalized after `with`
    /// (e.g. a hypothetical `self with: x`) does not match the `with*:`
    /// naming convention — no false positive from an unrelated `with:`
    /// method.
    #[test]
    fn setup_bare_with_colon_send_not_recognized_as_with_star() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: dashboard = nil\n\
                   \n  setUp =>\n    self.dashboard := 1.\n    self with: 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning: trailing `self with: 2` isn't a with<Field>: \
             send, so it doesn't carry the earlier self.dashboard := forward, \
             got: {diagnostics:?}"
        );
    }

    /// A chained `with*:` send — `(self withA: x) withB: y` — where the
    /// outer send's receiver is itself a `with<Field>:` send rather than
    /// literal `self`. The CI regression (BT-3395 fix-forward): the chain's
    /// overall value is still the fully updated self (each `with*:` send
    /// returns self with its own field set), so this must NOT warn as
    /// `setUp`'s trailing statement.
    #[test]
    fn setup_with_field_chain_two_deep_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ChainedTest\n  field: a = nil\n  field: b = nil\n\
                   \n  setUp =>\n    (self withA: 1) withB: 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for a 2-deep with*: chain as the trailing statement, got: {diagnostics:?}"
        );
    }

    /// The same chain, 3 levels deep — `((self withA: x) withB: y) withC:
    /// z` — confirming the recursive receiver check isn't hardcoded to a
    /// single level of nesting.
    #[test]
    fn setup_with_field_chain_three_deep_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ChainedTest\n  field: a = nil\n  field: b = nil\n  field: c = nil\n\
                   \n  setUp =>\n    ((self withA: 1) withB: 2) withC: 3";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for a 3-deep with*: chain as the trailing statement, got: {diagnostics:?}"
        );
    }

    /// A 2-deep `with*:` chain followed by an unrelated trailing statement
    /// still warns — the chain's self-preserving value is discarded once
    /// something follows it, same trap as the single-send and cascade
    /// forms.
    #[test]
    fn setup_with_field_chain_then_unrelated_statement_warns() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ChainedTest\n  field: a = nil\n  field: b = nil\n\
                   \n  setUp =>\n    (self withA: 1) withB: 2.\n    Transcript show: \"ready\"";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning for a with*: chain followed by an unrelated statement, got: {diagnostics:?}"
        );
    }

    /// A 3+-message cascade whose `with<Field>:` sends are NOT last —
    /// `self withCounter: 1; withDb: 2; log: "ready"`. The cascade's actual
    /// value is `log:`'s result, not self (ADR 0067: a cascade evaluates to
    /// its *last* message), so both `counter` and `db` are genuinely
    /// dropped. Must warn — a naive `.any()` check over the cascade's
    /// messages would wrongly see the `with<Field>:` sends and call this
    /// self-preserving.
    #[test]
    fn setup_with_field_cascade_with_trailing_non_with_message_warns() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: CounterDbTest\n  field: counter = nil\n  field: db = nil\n\
                   \n  setUp =>\n    self withCounter: 1; withDb: 2; log: \"ready\"";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning: the cascade's last message is `log:`, not a \
             with<Field>: send, so counter/db are genuinely dropped, got: {diagnostics:?}"
        );
    }

    /// A non-TestCase Value subclass with the identical shape does NOT warn
    /// — this lint is scoped to `TestCase`'s documented setUp/threading
    /// contract, not a general value-type rule.
    #[test]
    fn non_testcase_setup_no_warn() {
        let src =
            "Value subclass: Point\n  field: x = 0\n  setUp =>\n    self.x := 1.\n    extra := 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for non-TestCase class, got: {diagnostics:?}"
        );
    }

    /// A method named `setUp` with a keyword selector part (e.g.
    /// `setUp:` — not the case in practice, but guards the unary-only match)
    /// does not trigger the check on unrelated methods.
    #[test]
    fn other_method_named_differently_no_warn() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: dashboard = nil\n  helper =>\n    self.dashboard := 1.\n    extra := 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings for a non-setUp method, got: {diagnostics:?}"
        );
    }

    /// Standalone (Tonel-style) `setUp` method definitions are also checked.
    #[test]
    fn setup_in_standalone_method_warns() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: dashboard = nil\n\
                   \nThingTest >> setUp =>\n    self.dashboard := 1.\n    extra := 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        assert_eq!(module.method_definitions.len(), 1);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 warning in standalone setUp method, got: {diagnostics:?}"
        );
    }

    /// The diagnostic has the `Lint` category and a hint pointing at the fix.
    #[test]
    fn setup_drops_field_diagnostic_has_hint_and_category() {
        let src = "Value subclass: TestCase\n  field: name = \"\"\n\n\
                   TestCase subclass: ThingTest\n  field: dashboard = nil\n  setUp =>\n    self.dashboard := 1.\n    extra := 2";
        let (module, hierarchy) = build_module_and_hierarchy(src);
        let mut diagnostics = Vec::new();
        check_testcase_setup_drops_field_assignments(&module, &hierarchy, &mut diagnostics);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].category, Some(DiagnosticCategory::Lint));
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .is_some_and(|h| h.contains("self")),
            "Expected hint mentioning self, got: {:?}",
            diagnostics[0].hint
        );
    }

    // ── BT-2140: Redundant local-variable type annotation tests ───────────────

    /// Helper: parse, type-check, and run the redundant-annotation lint.
    fn redundant_local_type_lints(src: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        let hard_errs: Vec<_> = parse_diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(hard_errs.is_empty(), "Parse failed: {hard_errs:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.expect("hierarchy build failed");
        let type_map = crate::semantic_analysis::infer_types(&module, &hierarchy, None);
        let mut diagnostics = Vec::new();
        check_redundant_local_type_annotation(&module, &type_map, &mut diagnostics);
        diagnostics
    }

    /// Top-level: `x :: Counter := Counter new` — annotation is redundant.
    #[test]
    fn redundant_simple_annotation_with_new_flagged() {
        let src = "Object subclass: Counter\n  count => 0\n\nx :: Counter := Counter new";
        let diags = redundant_local_type_lints(src);
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 redundant-annotation lint, got: {diags:?}"
        );
        assert_eq!(diags[0].severity, Severity::Lint);
        assert_eq!(diags[0].category, Some(DiagnosticCategory::TypeAnnotation));
        assert!(
            diags[0].message.contains("Redundant type annotation"),
            "Expected 'Redundant type annotation' in message, got: {}",
            diags[0].message
        );
        assert!(
            diags[0].message.contains("Counter"),
            "Expected 'Counter' in message, got: {}",
            diags[0].message
        );
    }

    /// Inside a method: `c :: Counter := Counter new` — flagged.
    #[test]
    fn redundant_annotation_inside_method_flagged() {
        let src = "Object subclass: Counter\n  count => 0\n\nObject subclass: Foo\n  build =>\n    c :: Counter := Counter new.\n    c";
        let diags = redundant_local_type_lints(src);
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 redundant-annotation lint inside method, got: {diags:?}"
        );
    }

    /// `x :: Counter | Nil := nil` — Union annotation is load-bearing, NOT flagged.
    #[test]
    fn union_annotation_with_nil_rhs_not_flagged() {
        let src = "Object subclass: Counter\n  count => 0\n\nx :: Counter | Nil := nil";
        let diags = redundant_local_type_lints(src);
        assert!(
            diags.is_empty(),
            "Union annotation (T | Nil) must never be flagged, got: {diags:?}"
        );
    }

    /// `x :: SuperType := SubType new` — annotation widens, NOT flagged.
    #[test]
    fn widening_annotation_not_flagged() {
        let src = "Object subclass: Animal\n  name => \"animal\"\n\nAnimal subclass: Dog\n  bark => \"woof\"\n\nx :: Animal := Dog new";
        let diags = redundant_local_type_lints(src);
        assert!(
            diags.is_empty(),
            "Widening annotation (Super := Sub) must not be flagged, got: {diags:?}"
        );
    }

    /// `self.x := Counter new` — field assignment has no user-facing annotation
    /// in this lint's scope and must not be flagged.
    #[test]
    fn field_assignment_not_flagged() {
        let src = "Object subclass: Counter\n  count => 0\n\nObject subclass: Foo\n  state: c = nil\n  build =>\n    self.c := Counter new.\n    self";
        let diags = redundant_local_type_lints(src);
        assert!(
            diags.is_empty(),
            "Field assignment must not be flagged, got: {diags:?}"
        );
    }

    /// `x := Counter new` (no annotation) — nothing to flag.
    #[test]
    fn no_annotation_not_flagged() {
        let src = "Object subclass: Counter\n  count => 0\n\nx := Counter new";
        let diags = redundant_local_type_lints(src);
        assert!(
            diags.is_empty(),
            "Bare `x := ...` (no annotation) must never be flagged, got: {diags:?}"
        );
    }

    /// State declarations carry their own annotation form (`state: x :: T = ...`)
    /// and are not flagged by this local-variable lint.
    #[test]
    fn state_declaration_not_flagged() {
        let src = "Object subclass: Counter\n  count => 0\n\nObject subclass: Foo\n  state: c :: Counter = nil\n  build => self";
        let diags = redundant_local_type_lints(src);
        assert!(
            diags.is_empty(),
            "State declarations are not local-variable assignments, got: {diags:?}"
        );
    }

    /// Method/block parameters with annotations are not local-variable
    /// assignments — never flagged.
    #[test]
    fn method_parameter_annotation_not_flagged() {
        let src = "Object subclass: Counter\n  count => 0\n\nObject subclass: Foo\n  with: c :: Counter => c";
        let diags = redundant_local_type_lints(src);
        assert!(
            diags.is_empty(),
            "Method parameter annotation must not be flagged, got: {diags:?}"
        );
    }

    /// Hint should be present and mention `@expect type_annotation` for suppression.
    #[test]
    fn redundant_annotation_has_suppressible_hint() {
        let src = "Object subclass: Counter\n  count => 0\n\nx :: Counter := Counter new";
        let diags = redundant_local_type_lints(src);
        assert_eq!(diags.len(), 1);
        let hint = diags[0]
            .hint
            .as_ref()
            .expect("expected hint on redundant-annotation lint");
        assert!(
            hint.contains("@expect type_annotation"),
            "hint should mention @expect type_annotation, got: {hint}"
        );
    }

    /// `x :: Integer := 42` — literal RHS already has the matching type, flagged.
    #[test]
    fn redundant_annotation_with_literal_rhs_flagged() {
        let src = "x :: Integer := 42";
        let diags = redundant_local_type_lints(src);
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 lint for `x :: Integer := 42`, got: {diags:?}"
        );
    }

    /// Multiple redundant assignments in the same method each trigger their own lint.
    #[test]
    fn multiple_redundant_annotations_each_flagged() {
        let src = "Object subclass: Counter\n  count => 0\n\nObject subclass: Foo\n  build =>\n    a :: Counter := Counter new.\n    b :: Counter := Counter new.\n    a";
        let diags = redundant_local_type_lints(src);
        assert_eq!(
            diags.len(),
            2,
            "Expected one lint per redundant annotation, got: {diags:?}"
        );
    }
}
