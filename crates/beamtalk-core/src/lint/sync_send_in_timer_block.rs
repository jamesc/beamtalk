// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when a synchronous `self` send appears inside a `Timer every:do:`
//! or `Timer after:do:` block (BT-1458).
//!
//! **DDD Context:** Compilation
//!
//! Timer blocks execute in a separate process. A synchronous `self method.`
//! inside such a block sends a `gen_server:call` back to the actor that
//! scheduled the timer, which is likely waiting for the timer callback to
//! return — causing a deadlock.
//!
//! ```text
//! // Bad — deadlock: sync send to self in timer block
//! Timer every: 1000 do: [self refresh]
//!
//! // Good — async cast avoids deadlock
//! Timer every: 1000 do: [self refresh!]
//! ```

use crate::ast::{Block, Expression, Identifier, MessageSelector, Module};
use crate::lint::LintPass;
use crate::source_analysis::Diagnostic;

/// Lint pass that warns on synchronous `self` sends inside `Timer every:do:`
/// and `Timer after:do:` blocks.
pub(crate) struct SyncSendInTimerBlockPass;

impl LintPass for SyncSendInTimerBlockPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for stmt in &method.body {
                    walk_for_timer_sends(&stmt.expression, false, diagnostics);
                }
            }
        }
        for standalone in &module.method_definitions {
            for stmt in &standalone.method.body {
                walk_for_timer_sends(&stmt.expression, false, diagnostics);
            }
        }
    }
}

/// Returns `true` if `expr` is a `Timer` class reference.
fn is_timer_class_ref(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::ClassReference {
            name: Identifier { name, .. },
            ..
        } if name.as_str() == "Timer"
    )
}

/// Returns `true` if `selector` is `every:do:` or `after:do:`.
fn is_timer_block_selector(selector: &MessageSelector) -> bool {
    matches!(selector, MessageSelector::Keyword(parts)
        if parts.len() == 2
            && parts[0].keyword.as_str() == "every:"
            && parts[1].keyword.as_str() == "do:"
    ) || matches!(selector, MessageSelector::Keyword(parts)
        if parts.len() == 2
            && parts[0].keyword.as_str() == "after:"
            && parts[1].keyword.as_str() == "do:"
    )
}

/// Returns `true` if `expr` is the identifier `self`.
fn is_self(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Identifier(Identifier { name, .. }) if name.as_str() == "self"
    )
}

/// Handles a `MessageSend` node: detects Timer block boundaries and flags
/// sync self-sends inside timer blocks. Returns `true` if a Timer send was
/// detected (and fully handled), `false` otherwise.
fn check_message_send(
    receiver: &Expression,
    selector: &MessageSelector,
    arguments: &[Expression],
    is_cast: bool,
    span: crate::source_analysis::Span,
    in_timer_block: bool,
    diagnostics: &mut Vec<Diagnostic>,
) -> bool {
    // Check if this is a Timer every:do: or Timer after:do: send
    if is_timer_class_ref(receiver) && is_timer_block_selector(selector) {
        // The block is the last argument (the `do:` argument)
        if let Some(block_arg) = arguments.last() {
            walk_for_timer_sends(block_arg, true, diagnostics);
        }
        // Walk the non-block arguments normally (e.g. the interval)
        for arg in arguments.iter().take(arguments.len().saturating_sub(1)) {
            walk_for_timer_sends(arg, in_timer_block, diagnostics);
        }
        return true;
    }

    // If we're inside a timer block and this is a sync self-send, flag it
    if in_timer_block && !is_cast && is_self(receiver) {
        let selector_name = selector.name();
        diagnostics.push(
            Diagnostic::lint(
                format!(
                    "synchronous send `self {selector_name}` inside a Timer block will deadlock"
                ),
                span,
            )
            .with_hint(format!(
                "Timer blocks run in a separate process; \
                 use `self {selector_name}!` (async cast) instead"
            )),
        );
    }
    false
}

/// Recursively walks an expression tree. When `in_timer_block` is true, we are
/// inside a `Timer every:do:` or `Timer after:do:` block and should flag sync
/// self-sends.
#[allow(clippy::too_many_lines)]
fn walk_for_timer_sends(
    expr: &Expression,
    in_timer_block: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast,
            span,
        } => {
            if check_message_send(
                receiver,
                selector,
                arguments,
                *is_cast,
                *span,
                in_timer_block,
                diagnostics,
            ) {
                return;
            }
            walk_for_timer_sends(receiver, in_timer_block, diagnostics);
            for arg in arguments {
                walk_for_timer_sends(arg, in_timer_block, diagnostics);
            }
        }

        Expression::Block(Block { body, .. }) => {
            for stmt in body {
                walk_for_timer_sends(&stmt.expression, in_timer_block, diagnostics);
            }
        }

        Expression::Assignment { target, value, .. } => {
            walk_for_timer_sends(target, in_timer_block, diagnostics);
            walk_for_timer_sends(value, in_timer_block, diagnostics);
        }

        Expression::Return { value, .. } | Expression::DestructureAssignment { value, .. } => {
            walk_for_timer_sends(value, in_timer_block, diagnostics);
        }

        Expression::FieldAccess { receiver, .. } => {
            walk_for_timer_sends(receiver, in_timer_block, diagnostics);
        }

        Expression::Cascade {
            receiver,
            messages,
            span,
        } => {
            walk_for_timer_sends(receiver, in_timer_block, diagnostics);
            // Cascades send all messages to the same receiver. If the receiver
            // is `self` and we're in a timer block, each cascade message is an
            // inherently synchronous send that will deadlock.
            if in_timer_block && is_self(receiver) {
                for msg in messages {
                    let selector_name = msg.selector.name();
                    diagnostics.push(
                        Diagnostic::lint(
                            format!(
                                "synchronous send `self {selector_name}` inside a Timer block will deadlock"
                            ),
                            *span,
                        )
                        .with_hint(format!(
                            "Timer blocks run in a separate process; \
                             use `self {selector_name}!` (async cast) instead"
                        )),
                    );
                }
            }
            for msg in messages {
                for arg in &msg.arguments {
                    walk_for_timer_sends(arg, in_timer_block, diagnostics);
                }
            }
        }

        Expression::Parenthesized { expression, .. } => {
            walk_for_timer_sends(expression, in_timer_block, diagnostics);
        }

        Expression::Match { value, arms, .. } => {
            walk_for_timer_sends(value, in_timer_block, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_for_timer_sends(guard, in_timer_block, diagnostics);
                }
                walk_for_timer_sends(&arm.body, in_timer_block, diagnostics);
            }
        }

        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_for_timer_sends(&pair.key, in_timer_block, diagnostics);
                walk_for_timer_sends(&pair.value, in_timer_block, diagnostics);
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                walk_for_timer_sends(elem, in_timer_block, diagnostics);
            }
            if let Some(t) = tail {
                walk_for_timer_sends(t, in_timer_block, diagnostics);
            }
        }

        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                walk_for_timer_sends(elem, in_timer_block, diagnostics);
            }
        }

        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    walk_for_timer_sends(e, in_timer_block, diagnostics);
                }
            }
        }

        // Leaf nodes — nothing to recurse into.
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Error { .. }
        | Expression::Spread { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::lint::run_lint_passes;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        assert!(
            parse_diags.is_empty(),
            "Parse failed for lint fixture: {parse_diags:?}"
        );
        run_lint_passes(&module)
    }

    fn timer_lints(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
        lint(source)
            .into_iter()
            .filter(|d| d.message.contains("Timer block"))
            .collect()
    }

    #[test]
    fn sync_self_send_in_timer_every_do_flagged() {
        let diags = timer_lints(
            "Actor subclass: Foo\n  start =>\n    Timer every: 1000 do: [self refresh]\n",
        );
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("self refresh"),
            "message: {}",
            diags[0].message
        );
        assert!(diags[0].hint.is_some(), "expected a hint");
        assert!(
            diags[0].hint.as_ref().unwrap().contains("self refresh!"),
            "hint should suggest async cast: {:?}",
            diags[0].hint
        );
    }

    #[test]
    fn sync_self_send_in_timer_after_do_flagged() {
        let diags = timer_lints(
            "Actor subclass: Foo\n  start =>\n    Timer after: 500 do: [self cleanup]\n",
        );
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
        assert!(diags[0].message.contains("self cleanup"));
    }

    #[test]
    fn async_self_send_in_timer_block_not_flagged() {
        let diags = timer_lints(
            "Actor subclass: Foo\n  start =>\n    Timer every: 1000 do: [self refresh!]\n",
        );
        assert!(
            diags.is_empty(),
            "async cast should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn non_self_send_in_timer_block_not_flagged() {
        let diags = timer_lints(
            "Actor subclass: Foo\n  start: logger =>\n    Timer every: 1000 do: [logger info: \"tick\"]\n",
        );
        assert!(
            diags.is_empty(),
            "non-self send should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn self_send_outside_timer_block_not_flagged() {
        let diags = timer_lints("Actor subclass: Foo\n  start =>\n    self refresh\n");
        assert!(
            diags.is_empty(),
            "self send outside timer block should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn multiple_sync_sends_in_timer_block_all_flagged() {
        let diags = timer_lints(
            "Actor subclass: Foo\n  start =>\n    Timer every: 1000 do: [self refresh. self save]\n",
        );
        assert_eq!(diags.len(), 2, "expected two lints, got: {diags:?}");
    }

    #[test]
    fn nested_block_in_timer_block_flagged() {
        let diags = timer_lints(
            "Actor subclass: Foo\n  start =>\n    Timer every: 1000 do: [items do: [:i | self process: i]]\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "expected one lint for nested block, got: {diags:?}"
        );
    }

    #[test]
    fn sync_keyword_send_in_timer_block_flagged() {
        let diags = timer_lints(
            "Actor subclass: Foo\n  start =>\n    Timer after: 0 do: [self update: 42]\n",
        );
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
        assert!(
            diags[0].message.contains("self update:"),
            "message: {}",
            diags[0].message
        );
    }
}
