// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when `inspect` is sent in string position — directly as an operand
//! of `++` (string concatenation) or as a string-interpolation segment
//! (ADR 0095 Phase 3).
//!
//! **DDD Context:** Compilation
//!
//! ADR 0095 Phase 3 repurposed `inspect` from `-> String` to `-> Inspector`:
//! `anObject inspect` now opens a navigable [`Inspector`] cursor, not a string.
//! Because typing is gradual and code hot-reloads, a leftover `… ++ x inspect`
//! compiles but produces an `Inspector` where a `String` is expected — a runtime
//! crash (ADR 0094 Risk #2). This transitional lint flags those sites so they can
//! migrate to `printString` (the structural Debug string, ADR 0094).
//!
//! ```text
//! // Flagged — inspect no longer returns a String
//! "point = " ++ p inspect
//! "result: {x inspect}"
//!
//! // Good — printString is the structural Debug string
//! "point = " ++ p printString
//! "result: {x printString}"
//! ```

use crate::{LintContext, LintPass};
use beamtalk_core::ast::{Expression, MessageSelector, Module, StringSegment};
use beamtalk_core::ast_walker::walk_expression;
use beamtalk_core::source_analysis::{Diagnostic, Span};

/// Lint pass that warns on `inspect` sends used directly in string position
/// (`++` operands and string-interpolation segments).
pub(crate) struct InspectInStringPositionPass;

impl LintPass for InspectInStringPositionPass {
    fn check(&self, module: &Module, _ctx: &LintContext<'_>, diagnostics: &mut Vec<Diagnostic>) {
        for method in module.all_methods() {
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |e| check_node(e, diagnostics));
            }
        }
    }
}

/// Returns the span of `expr` if it is a unary `inspect` send (`receiver inspect`),
/// else `None`.
fn inspect_send_span(expr: &Expression) -> Option<Span> {
    match expr {
        Expression::MessageSend {
            selector: MessageSelector::Unary(name),
            span,
            ..
        } if name.as_str() == "inspect" => Some(*span),
        // See through parentheses: `(x inspect)` in `++` position.
        Expression::Parenthesized { expression, .. } => inspect_send_span(expression),
        _ => None,
    }
}

/// Push the lint diagnostic for an `inspect` send used in string position.
fn flag(span: Span, diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.push(
        Diagnostic::lint(
            "`inspect` returns an Inspector, not a String — using it in string \
             position will crash at runtime (ADR 0095)"
                .to_string(),
            span,
        )
        .with_hint(
            "use `printString` for the structural Debug string; `inspect` opens a \
             navigable Inspector cursor"
                .to_string(),
        ),
    );
}

/// Flag `inspect` sends in `++` operand and string-interpolation positions of
/// `expr` itself (the caller visits every node via `walk_expression`).
fn check_node(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    let is_concat =
        |sel: &MessageSelector| matches!(sel, MessageSelector::Binary(op) if op.as_str() == "++");
    match expr {
        // `a ++ b`: either operand being a bare `inspect` send is a string-
        // position use.
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } if is_concat(selector) => {
            for operand in std::iter::once(&**receiver).chain(arguments) {
                if let Some(span) = inspect_send_span(operand) {
                    flag(span, diagnostics);
                }
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let StringSegment::Interpolation(e) = seg
                    && let Some(span) = inspect_send_span(e)
                {
                    flag(span, diagnostics);
                }
            }
        }
        // A `++` cascade message (`x foo; ++ y`) puts its argument in
        // string-concat position, same as a binary `++` send.
        Expression::Cascade { messages, .. } => {
            for msg in messages.iter().filter(|m| is_concat(&m.selector)) {
                for arg in &msg.arguments {
                    if let Some(span) = inspect_send_span(arg) {
                        flag(span, diagnostics);
                    }
                }
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::run_lint_passes;
    use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        assert!(
            parse_diags.is_empty(),
            "Parse failed for lint fixture: {parse_diags:?}"
        );
        run_lint_passes(&module)
    }

    fn inspect_lints(source: &str) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
        lint(source)
            .into_iter()
            .filter(|d| d.message.contains("`inspect` returns an Inspector"))
            .collect()
    }

    #[test]
    fn inspect_on_right_of_concat_flagged() {
        let diags =
            inspect_lints("Object subclass: Foo\n  bar: p =>\n    \"point = \" ++ p inspect\n");
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(diags[0].hint.is_some());
        assert!(
            diags[0].hint.as_ref().unwrap().contains("printString"),
            "hint should suggest printString: {:?}",
            diags[0].hint
        );
    }

    #[test]
    fn inspect_on_left_of_concat_flagged() {
        let diags =
            inspect_lints("Object subclass: Foo\n  bar: p =>\n    p inspect ++ \" done\"\n");
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
    }

    #[test]
    fn parenthesized_inspect_in_concat_flagged() {
        let diags =
            inspect_lints("Object subclass: Foo\n  bar: p =>\n    \"x = \" ++ (p inspect)\n");
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
    }

    #[test]
    fn inspect_in_string_interpolation_flagged() {
        let diags =
            inspect_lints("Object subclass: Foo\n  bar: p =>\n    \"result: {p inspect}\"\n");
        assert_eq!(diags.len(), 1, "expected one lint, got: {diags:?}");
    }

    #[test]
    fn printstring_in_concat_not_flagged() {
        let diags =
            inspect_lints("Object subclass: Foo\n  bar: p =>\n    \"point = \" ++ p printString\n");
        assert!(
            diags.is_empty(),
            "printString should not be flagged: {diags:?}"
        );
    }

    #[test]
    fn bare_inspect_not_in_string_position_not_flagged() {
        // `i := p inspect` — a normal Inspector-cursor use, not string position.
        let diags = inspect_lints("Object subclass: Foo\n  bar: p =>\n    i := p inspect\n");
        assert!(
            diags.is_empty(),
            "bare inspect (cursor use) should not be flagged: {diags:?}"
        );
    }

    #[test]
    fn inspect_with_drill_not_flagged() {
        // `(p inspect) fields` — drilling the cursor, not string position.
        let diags = inspect_lints("Object subclass: Foo\n  bar: p =>\n    (p inspect) fields\n");
        assert!(
            diags.is_empty(),
            "drilling the cursor should not be flagged: {diags:?}"
        );
    }
}
