// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code quality lint validators.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validators that check for code quality issues:
//! - Redundant assignment `x := x` (BT-950)
//! - Self capture in actor collection HOF blocks (BT-953)
//! - Literal boolean conditions (BT-955)
//! - Empty method bodies (BT-859)
//! - Effect-free statements (BT-951)

use super::class_validators::child_expressions;
use crate::ast::{Block, Expression, Identifier, Module};
use crate::ast_walker::{walk_expression, walk_module};
use crate::semantic_analysis::block_context::{classify_block, is_collection_hof_selector};
use crate::semantic_analysis::{BlockContext, ClassHierarchy};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;
use std::collections::HashSet;

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
                        .with_hint("Remove this assignment or assign a different value."),
                    );
                }
            }
        }
    });
}

// ── BT-953: Self capture in collection HOF blocks ─────────────────────────────

/// BT-953: Warn when `self` is referenced inside a literal block passed to a
/// collection higher-order method (collect:, do:, select:, reject:, inject:into:,
/// detect:, detect:ifNone:) inside an **Actor** class method.
///
/// Only `Actor subclass:` uses the `calling_self` / `gen_server` dispatch mechanism.
/// `Object subclass:` and `Value subclass:` use direct synchronous Erlang function
/// calls, so self-sends inside collection blocks are safe for those class kinds.
///
/// Example: `items collect: [:x | self process: x]`  ← deadlock risk (Actor only)
pub(crate) fn check_self_capture_in_actor_block(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Precompute the set of selectors whose block arguments run in a separate
    // BEAM process, so lookups are O(1) per expression node (BT-1312).
    let spawn_selectors = hierarchy.spawns_block_selectors();

    for class in &module.classes {
        if !hierarchy.is_actor_subclass(class.name.name.as_str()) {
            continue;
        }
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |e| {
                    check_self_capture_at(e, &spawn_selectors, diagnostics);
                });
            }
        }
    }

    // Also cover Tonel-style standalone method definitions for Actor classes.
    for standalone in &module.method_definitions {
        if !hierarchy.is_actor_subclass(standalone.class_name.name.as_str()) {
            continue;
        }
        for stmt in &standalone.method.body {
            walk_expression(&stmt.expression, &mut |e| {
                check_self_capture_at(e, &spawn_selectors, diagnostics);
            });
        }
    }
}

/// Returns `true` if `expr` is a bare `self` identifier, unwrapping any
/// surrounding parentheses.
fn is_self_receiver(expr: &Expression) -> bool {
    match expr {
        Expression::Identifier(Identifier { name, .. }) => name == "self",
        Expression::Parenthesized { expression, .. } => is_self_receiver(expression),
        _ => false,
    }
}

/// Searches an expression tree for a `MessageSend` or `Cascade` whose
/// immediate receiver is `self`.
///
/// Only these forms go through the `calling_self` / `gen_server` dispatch and
/// can deadlock when called from inside a collection HOF block.
/// `FieldAccess { receiver: self }` (`self.field`) compiles to `maps:get` /
/// `maps:put` — a direct in-process map operation — and is safe.
///
/// Returns `true` if the expression is a block literal, possibly wrapped in
/// parentheses (e.g. `([self work])`). Used to filter block arguments of
/// `spawns_block` methods so we don't traverse into separate-process code.
fn is_block_literal_like(expr: &Expression) -> bool {
    match expr {
        Expression::Block(_) => true,
        Expression::Parenthesized { expression, .. } => is_block_literal_like(expression),
        _ => false,
    }
}

/// Block arguments of methods whose selectors are in `spawn_selectors` are
/// skipped: they run in a separate BEAM process and self-sends there are
/// always safe (BT-1312).
fn find_self_message_send(expr: &Expression, spawn_selectors: &HashSet<EcoString>) -> Option<Span> {
    match expr {
        Expression::MessageSend { receiver, span, .. }
        | Expression::Cascade { receiver, span, .. } => {
            if is_self_receiver(receiver) {
                return Some(*span);
            }
        }
        _ => {}
    }
    // Methods with `spawns_block: true` run their block in a separate process —
    // skip the block argument so we don't flag safe self-sends (BT-1301/BT-1312).
    // The receiver is NOT skipped: `(self getTimer) after:do:` still warns
    // because `self getTimer` is a real calling_self dispatch in the HOF.
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        ..
    } = expr
    {
        if spawn_selectors.contains(selector.name().as_str()) {
            return std::iter::once(receiver.as_ref())
                .chain(arguments.iter().filter(|a| !is_block_literal_like(a)))
                .find_map(|e| find_self_message_send(e, spawn_selectors));
        }
    }
    // Cascade: apply the same spawns_block skipping per-message.
    if let Expression::Cascade {
        receiver, messages, ..
    } = expr
    {
        if let Some(span) = find_self_message_send(receiver, spawn_selectors) {
            return Some(span);
        }
        for msg in messages {
            let found = if spawn_selectors.contains(msg.selector.name().as_str()) {
                msg.arguments
                    .iter()
                    .filter(|a| !is_block_literal_like(a))
                    .find_map(|e| find_self_message_send(e, spawn_selectors))
            } else {
                msg.arguments
                    .iter()
                    .find_map(|e| find_self_message_send(e, spawn_selectors))
            };
            if found.is_some() {
                return found;
            }
        }
        return None;
    }
    child_expressions(expr)
        .into_iter()
        .find_map(|e| find_self_message_send(e, spawn_selectors))
}

/// Searches a block's body for a message send or cascade directly to `self`.
fn find_self_reference_in_block(
    block: &Block,
    spawn_selectors: &HashSet<EcoString>,
) -> Option<Span> {
    block
        .body
        .iter()
        .find_map(|s| find_self_message_send(&s.expression, spawn_selectors))
}

/// Checks a single expression node for the self-capture pattern.
///
/// Called by [`check_self_capture_in_actor_block`]; the
/// walker handles recursive traversal, so this function only inspects the
/// current node.
fn check_self_capture_at(
    expr: &Expression,
    spawn_selectors: &HashSet<EcoString>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Expression::MessageSend {
        selector,
        arguments,
        span,
        ..
    } = expr
    {
        let selector_str = selector.name();
        for (i, arg) in arguments.iter().enumerate() {
            if !is_collection_hof_selector(&selector_str, i) {
                continue;
            }
            // Use classify_block to confirm this is a literal block in a control-flow
            // position (not a block variable). This is the production wiring of the
            // block_context infrastructure (BT-953).
            let ctx = classify_block(arg.span(), expr, false);
            if !matches!(ctx, BlockContext::ControlFlow) {
                continue;
            }
            if let Expression::Block(block) = arg {
                if find_self_reference_in_block(block, spawn_selectors).is_some() {
                    diagnostics.push(
                        Diagnostic::hint(
                            format!(
                                "`self` capture in block passed to `{selector_str}` may deadlock"
                            ),
                            *span,
                        )
                        .with_hint(
                            "Sending `self` from within a collection block re-enters the \
                             `calling_self` dispatch and can deadlock. \
                             Inline the logic or bind the result to a local variable before \
                             entering the block.",
                        )
                        .with_category(DiagnosticCategory::SelfCapture),
                    );
                }
            }
        }
    }
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

/// Returns `true` if the selector is a boolean conditional message.
fn is_boolean_conditional_selector(sel: &str) -> bool {
    matches!(sel, "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:")
}

/// Returns a hint string describing the unreachable or redundant branch.
fn dead_branch_hint(is_true: bool, selector: &str) -> &'static str {
    match (is_true, selector) {
        (true, "ifTrue:") | (false, "ifFalse:") => {
            "The branch is always taken. Remove the conditional and use the branch body directly."
        }
        (true, "ifFalse:") | (false, "ifTrue:") => "This branch is never executed. Remove it.",
        (true, "ifTrue:ifFalse:") => {
            "The `ifFalse:` branch is never executed. Simplify to the `ifTrue:` block."
        }
        (false, "ifTrue:ifFalse:") => {
            "The `ifTrue:` branch is never executed. Simplify to the `ifFalse:` block."
        }
        _ => "Remove the unreachable branch.",
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
        let literal_val = match receiver.as_ref() {
            Expression::Identifier(Identifier { name, .. }) if name == "true" => Some(true),
            Expression::Identifier(Identifier { name, .. }) if name == "false" => Some(false),
            _ => None,
        };
        if let Some(is_true) = literal_val {
            let selector_str = selector.name();
            if is_boolean_conditional_selector(&selector_str) {
                let literal_name = if is_true { "true" } else { "false" };
                diagnostics.push(
                    Diagnostic::warning(format!("Condition is always `{literal_name}`"), *span)
                        .with_hint(dead_branch_hint(is_true, &selector_str)),
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
        let literal_val = match receiver.as_ref() {
            Expression::Identifier(Identifier { name, .. }) if name == "true" => Some(true),
            Expression::Identifier(Identifier { name, .. }) if name == "false" => Some(false),
            _ => None,
        };
        if let Some(is_true) = literal_val {
            let literal_name = if is_true { "true" } else { "false" };
            for msg in messages {
                let selector_str = msg.selector.name();
                if is_boolean_conditional_selector(&selector_str) {
                    diagnostics.push(
                        Diagnostic::warning(
                            format!("Condition is always `{literal_name}`"),
                            msg.span,
                        )
                        .with_hint(dead_branch_hint(is_true, &selector_str)),
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
pub(crate) fn check_empty_method_bodies(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if method.body.is_empty() {
                let selector = method.selector.name();
                diagnostics.push(
                    Diagnostic::error(format!("Method `{selector}` has an empty body"), method.span)
                        .with_category(DiagnosticCategory::EmptyBody)
                        .with_hint("Use `self notImplemented` for stubs, or `self subclassResponsibility` for abstract methods"),
                );
            }
        }
    }
    for standalone in &module.method_definitions {
        if standalone.method.body.is_empty() {
            let selector = standalone.method.selector.name();
            diagnostics.push(
                Diagnostic::error(
                    format!("Method `{selector}` has an empty body"),
                    standalone.method.span,
                )
                .with_category(DiagnosticCategory::EmptyBody)
                .with_hint("Use `self notImplemented` for stubs, or `self subclassResponsibility` for abstract methods"),
            );
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
            | "="
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
pub(crate) fn check_effect_free_statements(
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
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

    // ── BT-953: Self capture in collection HOF blocks ─────────────────────────

    /// `self` inside a `collect:` block emits a hint.
    #[test]
    fn self_capture_in_collect_block_hints() {
        let src =
            "Actor subclass: Processor\n  process: items => items collect: [:x | self handle: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self capture in collect:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
        assert!(
            diagnostics[0].message.contains("collect:"),
            "Expected 'collect:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// `self` inside an `inject:into:` block emits a hint.
    #[test]
    fn self_capture_in_inject_into_block_hints() {
        let src = "Actor subclass: Processor\n  run: items => items inject: 0 into: [:acc :x | self transform: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self capture in inject:into:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
        assert!(diagnostics[0].message.contains("inject:into:"));
    }

    /// No `self` in the block — no hint.
    #[test]
    fn no_self_in_collect_block_no_hint() {
        let src = "Actor subclass: Processor\n  process: items => items collect: [:x | x * 2]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints when self is not in block, got: {diagnostics:?}"
        );
    }

    /// `self` in an `ifTrue:` block is safe — no hint.
    #[test]
    fn self_in_if_true_block_no_hint() {
        let src = "Actor subclass: Worker\n  run => (x > 0) ifTrue: [self doWork]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for self in ifTrue: block, got: {diagnostics:?}"
        );
    }

    /// `Object subclass:` does NOT use `calling_self` dispatch — no hint emitted.
    #[test]
    fn self_capture_in_object_subclass_no_hint() {
        let src = "Object subclass: Formatter\n  format: rows => rows collect: [:row | self formatRow: row]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for Object subclass: (no calling_self dispatch), got: {diagnostics:?}"
        );
    }

    /// `Value subclass:` uses synchronous pure-function dispatch — no hint emitted.
    #[test]
    fn self_capture_in_value_subclass_no_hint() {
        let src =
            "Value subclass: Point\n  scale: factor => #(1, 2) collect: [:c | self transform: c]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for Value subclass: (synchronous dispatch), got: {diagnostics:?}"
        );
    }

    /// `self` inside a `do:` block in an Actor class emits a hint.
    #[test]
    fn self_capture_in_do_block_hints() {
        let src = "Actor subclass: Runner\n  run: items => items do: [:x | self process: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self capture in do:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    /// `self` inside a map literal within a collect: block in an Actor class still triggers.
    #[test]
    fn self_capture_nested_in_map_literal_hints() {
        let src =
            "Actor subclass: Foo\n  run: items => items collect: [:x | #{#key => self value}]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self nested in map literal, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    /// A block variable (not a literal block) passed to collect: does NOT trigger
    /// the hint — we only flag literal blocks.
    #[test]
    fn block_variable_in_collect_no_hint() {
        let src = "Object subclass: Foo\n  run: items with: blk => items collect: blk";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for block variable (not literal), got: {diagnostics:?}"
        );
    }

    /// Standalone (Tonel-style) Actor method with self in collect: emits a hint.
    #[test]
    fn self_capture_in_standalone_actor_method_hints() {
        let src = "Actor subclass: Processor\nProcessor >> process: items => items collect: [:x | self handle: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self capture in standalone Actor method, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    /// Standalone (Tonel-style) Object method with self in collect: — no hint.
    #[test]
    fn self_capture_in_standalone_object_method_no_hint() {
        let src = "Object subclass: Formatter\nFormatter >> format: rows => rows collect: [:row | self formatRow: row]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hints for standalone Object subclass: method, got: {diagnostics:?}"
        );
    }

    /// Transitive Actor subclass: class inheriting from an Actor also gets the hint.
    #[test]
    fn self_capture_in_transitive_actor_subclass_hints() {
        let src = "Actor subclass: BaseActor\nBaseActor subclass: SpecificActor\n  process: items => items collect: [:x | self handle: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for transitive Actor subclass, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    // ── Field access / assignment in collection blocks (false-positive tests) ──

    /// `self.field` read inside collect: is a maps:get, not a `gen_server` call — no hint.
    #[test]
    fn self_field_read_in_collect_no_hint() {
        let src =
            "Actor subclass: Counter\n  sumItems: items => items collect: [:x | x + self.total]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self.field read (maps:get, not gen_server), got: {diagnostics:?}"
        );
    }

    /// `self.field :=` write inside inject:into: is a maps:put, not a `gen_server` call — no hint.
    #[test]
    fn self_field_write_in_inject_no_hint() {
        let src = "Actor subclass: Counter\n  accumulate: items =>\n    items inject: 0 into: [:acc :val |\n      self.total := self.total + val\n      acc + val]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self.field := (maps:put, not gen_server), got: {diagnostics:?}"
        );
    }

    /// Sending a message to a field value (`self.cache at: x`) inside a block is safe —
    /// the receiver of the send is `self.cache` (a maps:get result), not `self`.
    #[test]
    fn message_send_to_self_field_no_hint() {
        let src =
            "Actor subclass: Repo\n  process: items => items collect: [:x | self.cache at: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for message sent to self.field value (not self), got: {diagnostics:?}"
        );
    }

    /// `self someMessage` (actual message send to self) inside collect: still fires.
    #[test]
    fn self_message_send_in_collect_still_hints() {
        let src =
            "Actor subclass: Counter\n  process: items => items collect: [:x | self transform: x]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for actual self message send in collect:, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    // ── BT-1301: Timer block self-capture false-positive tests ───────────────

    /// `Timer after:do:` with `self` in the block — no hint (separate process).
    #[test]
    fn self_in_timer_after_do_block_no_hint() {
        let src = "Actor subclass: Worker\n  run => Timer after: 0 do: [self doWork]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self in Timer after:do: block (separate process), got: {diagnostics:?}"
        );
    }

    /// `Timer every:do:` with `self` in the block — no hint (separate process).
    #[test]
    fn self_in_timer_every_do_block_no_hint() {
        let src = "Actor subclass: Worker\n  run => Timer every: 100 do: [self tick]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self in Timer every:do: block (separate process), got: {diagnostics:?}"
        );
    }

    /// `Timer after:do:` block nested inside a collection HOF — no hint (Timer
    /// runs in a separate process even when scheduled from a HOF iteration).
    #[test]
    fn self_in_timer_block_nested_in_hof_no_hint() {
        let src = "Actor subclass: Worker\n  process: items => items do: [:x | Timer after: 0 do: [self handle: x]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self in Timer block inside do: (Timer runs separately), got: {diagnostics:?}"
        );
    }

    /// Cast (`!`) variant: `Timer after:do:` with `self someMethod!` — no hint.
    #[test]
    fn self_cast_in_timer_block_no_hint() {
        let src = "Actor subclass: Worker\n  run => Timer after: 0 do: [self doWork!]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no hint for self cast in Timer after:do: block, got: {diagnostics:?}"
        );
    }

    /// A direct `self` call inside a collection HOF still fires even when a Timer
    /// is also present — only the Timer block is safe, not sibling direct self-sends.
    #[test]
    fn direct_self_in_hof_with_timer_still_hints() {
        let src = "Actor subclass: Worker\n  process: items => items do: [:x | self log: x. Timer after: 0 do: [self handle: x]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for direct self send in do: block alongside Timer, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
    }

    /// `self` in the *receiver* of a timer call inside a HOF still warns —
    /// e.g. `(self getTimer) after: 0 do: [block]` — the receiver goes through
    /// `calling_self` dispatch in the HOF and is NOT safe.
    #[test]
    fn self_in_timer_receiver_inside_hof_still_hints() {
        let src = "Actor subclass: Worker\n  process: items => items do: [:x | (self getTimer) after: 0 do: [x doSomething]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let mut diagnostics = Vec::new();
        check_self_capture_in_actor_block(&module, &hierarchy, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 hint for self in Timer receiver inside do: block, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Hint);
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
                .is_some_and(|h| h.contains("ifFalse:")),
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
                .is_some_and(|h| h.contains("ifTrue:")),
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
}
