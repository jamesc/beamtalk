// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `handleInfo:` written as a `match:` with no catch-all arm
//! (ADR 0134 § Decision, Implementation step 2; BT-3959).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Under ADR 0134 an error raised in `handleInfo:` crashes the server (it
//! is no longer logged and swallowed). A `match:` with no matching arm
//! raises `#pattern_mismatch` (BT-3943), so a `handleInfo:` whose body is a
//! `match:` without a catch-all arm crashes on every message its author did
//! not anticipate — exactly the stray-message footgun ADR 0065's
//! log-and-continue was guarding against. ADR 0134 keeps that tolerance by
//! lint rather than by swallowing errors: this check warns at compile time
//! and names the one-line fix (`_ -> nil`).
//!
//! The existing advisory `match:` exhaustiveness warning only fires over
//! typed unions; `msg` in `handleInfo: msg` is untyped, so it never fires
//! here — hence a dedicated check.
//!
//! **What counts.** For every instance-side `handleInfo:` method, each
//! **top-level** statement of the body (stripped of wrapping parentheses
//! and a leading `^`) that is a `match:`/`matchExhaustive:` send is checked.
//! A `match:` nested inside another expression (an `ifTrue:` block, an arm
//! body, a send argument) is not reported: the lint is purely syntactic and
//! only looks at the shape ADR 0134 names.
//!
//! **What is a catch-all.** An *unguarded* arm whose pattern is the
//! wildcard `_` or a bare variable binding (`other -> ...`). A guarded arm
//! (`_ when: [...] -> ...`) can still fall through, and a tuple pattern
//! (`{a, b}`, BT-3969) only matches tuples of that arity even when every
//! element is a variable, so neither counts — matching
//! `match_validators.rs`'s reading of "wildcard arm".
//!
//! **Category.** [`DiagnosticCategory::HandleInfoNoCatchAll`], configurable
//! under `[diagnostics]` as `handleinfo-no-catch-all`. Like
//! `UnguardedLateRead` it has no dedicated `@expect` category (ADR 0134 /
//! BT-3959 scope): the catch-all arm is the fix, not a suppression;
//! `@expect all` still acknowledges it where a fixture triggers it on
//! purpose.

use crate::ast::{Expression, MatchArm, Module, Pattern};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use crate::unparse::unparse_expression;

/// The selector this lint applies to.
const HANDLE_INFO_SELECTOR: &str = "handleInfo:";

/// Runs the ADR 0134 `handleinfo-no-catch-all` check over every class in
/// `module`.
pub(crate) fn check_handle_info_catch_all(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        for method in &class.methods {
            if method.is_class_method || method.selector.name() != HANDLE_INFO_SELECTOR {
                continue;
            }
            for stmt in &method.body {
                let Expression::Match {
                    value, arms, span, ..
                } = strip_parens_and_return(&stmt.expression)
                else {
                    continue;
                };
                if !arms.iter().any(is_catch_all_arm) {
                    diagnostics.push(build_diagnostic(value, *span));
                }
            }
        }
    }
}

/// An arm that matches every value: unguarded, with a wildcard or a bare
/// variable pattern.
fn is_catch_all_arm(arm: &MatchArm) -> bool {
    arm.guard.is_none() && matches!(arm.pattern, Pattern::Wildcard(_) | Pattern::Variable(_))
}

/// Unwraps any number of `Parenthesized` wrappers and `^` returns, so
/// `^msg match: [...]` and `(msg match: [...])` count as top-level.
fn strip_parens_and_return(expr: &Expression) -> &Expression {
    match expr {
        Expression::Parenthesized { expression, .. } => strip_parens_and_return(expression),
        Expression::Return { value, .. } => strip_parens_and_return(value),
        other => other,
    }
}

/// How the message names the matched value: the source of a single-line
/// scrutinee (usually just the parameter name), else a generic phrase.
fn scrutinee_text(value: &Expression) -> String {
    let text = unparse_expression(value).to_pretty_string();
    if text.is_empty() || text.contains('\n') {
        "its argument".to_string()
    } else {
        format!("`{text}`")
    }
}

fn build_diagnostic(value: &Expression, span: Span) -> Diagnostic {
    let message = format!(
        "`handleInfo:` matches on {} with no catch-all arm; any other message raises \
         `#pattern_mismatch` and crashes this server",
        scrutinee_text(value)
    );
    Diagnostic::warning(message, span)
        .with_hint("Add `_ -> nil` as the last arm to ignore unexpected messages")
        .with_category(DiagnosticCategory::HandleInfoNoCatchAll)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    fn findings(src: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_handle_info_catch_all(&module, &mut diagnostics);
        diagnostics
    }

    fn server(handle_info_body: &str) -> String {
        format!(
            "Server subclass: Watcher\n  state: n = 0\n\n  handleInfo: msg =>\n{handle_info_body}"
        )
    }

    // ── fires ────────────────────────────────────────────────────────────

    #[test]
    fn match_without_catch_all_warns() {
        let src = server(
            "    msg match: [\n      #tick -> self.n := self.n + 1;\n      #reset -> self.n := 0\n    ]",
        );
        let diags = findings(&src);
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        let d = &diags[0];
        assert_eq!(d.category, Some(DiagnosticCategory::HandleInfoNoCatchAll));
        assert_eq!(
            d.message.as_str(),
            "`handleInfo:` matches on `msg` with no catch-all arm; any other message raises \
             `#pattern_mismatch` and crashes this server"
        );
        assert_eq!(
            d.hint.as_deref(),
            Some("Add `_ -> nil` as the last arm to ignore unexpected messages")
        );
        assert_eq!(
            &src[d.span.start() as usize..d.span.start() as usize + 10],
            "msg match:"
        );
    }

    #[test]
    fn match_exhaustive_without_catch_all_warns() {
        let src = server("    msg matchExhaustive: [#tick -> nil]");
        assert_eq!(findings(&src).len(), 1);
    }

    #[test]
    fn tuple_patterns_are_not_catch_alls() {
        // BT-3969: `{a, b}` only matches 2-tuples, even when every element
        // is a variable binding.
        let src = server(
            "    msg match: [\n      {#ok, value} -> self.n := value;\n      {tag, payload} -> nil\n    ]",
        );
        assert_eq!(findings(&src).len(), 1);
    }

    #[test]
    fn guarded_wildcard_is_not_a_catch_all() {
        let src = server(
            "    msg match: [\n      #tick -> nil;\n      _ when: [self.n > 0] -> nil\n    ]",
        );
        assert_eq!(findings(&src).len(), 1);
    }

    #[test]
    fn returned_match_without_catch_all_warns() {
        let src = server("    ^msg match: [#tick -> nil]");
        assert_eq!(findings(&src).len(), 1);
    }

    #[test]
    fn match_after_other_statements_warns() {
        let src = server("    self.n := self.n + 1\n    msg match: [#tick -> nil]");
        assert_eq!(findings(&src).len(), 1);
    }

    // ── silent ───────────────────────────────────────────────────────────

    #[test]
    fn wildcard_arm_is_silent() {
        let src =
            server("    msg match: [\n      #tick -> self.n := self.n + 1;\n      _ -> nil\n    ]");
        assert!(findings(&src).is_empty());
    }

    #[test]
    fn variable_arm_is_silent() {
        let src =
            server("    msg match: [\n      #tick -> nil;\n      other -> self.n := 0\n    ]");
        assert!(findings(&src).is_empty());
    }

    #[test]
    fn tuple_arms_with_wildcard_are_silent() {
        let src = server(
            "    msg match: [\n      {#ok, value} -> self.n := value;\n      _ -> nil\n    ]",
        );
        assert!(findings(&src).is_empty());
    }

    #[test]
    fn non_match_body_is_silent() {
        let src = server("    msg == #tick ifTrue: [self.n := self.n + 1]");
        assert!(findings(&src).is_empty());
    }

    #[test]
    fn match_nested_in_if_true_is_silent() {
        let src = server("    msg isSymbol ifTrue: [\n      msg match: [#tick -> nil]\n    ]");
        assert!(findings(&src).is_empty());
    }

    #[test]
    fn other_selectors_are_silent() {
        let src = "Server subclass: Watcher\n  handle: msg => msg match: [#tick -> nil]";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn class_side_handle_info_is_silent() {
        let src = "Object subclass: Watcher\n  class handleInfo: msg => msg match: [#tick -> nil]";
        assert!(findings(src).is_empty());
    }
}
