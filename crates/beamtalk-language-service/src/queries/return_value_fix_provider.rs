// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The ADR 0133 "return the value instead" quick-fix (BT-3914).
//!
//! **DDD Context:** Language Service
//!
//! The escaping-block diagnostic
//! ([`DiagnosticCategory::Tier2BlockNoReturnChannel`]) reports an assignment
//! to a captured local inside a block the compiler cannot thread it out of,
//! and its fix is the ADR's idiom: have the block answer the new value and
//! assign it where the block is called. This provider applies that fix for
//! the one shape where it is mechanical:
//!
//! ```beamtalk
//! t := 0. Twice twice: [t := t + 1]      // before
//! t := 0. t := Twice twice: [t + 1]      // after
//! ```
//!
//! The action is offered only when
//! - the block is the receiver or an argument of a (synchronous) message send
//!   that is itself a statement,
//! - the block's last statement is the reported assignment `t := e`, to a
//!   plain local, and
//! - that is the block's only assignment (none to another local, a field or a
//!   class variable, at any depth).
//!
//! Otherwise no action is offered and the diagnostic keeps its message.
//!
//! **Precondition the action cannot check: the callee answers the block's
//! value.** The rewrite assigns the *send's* result to `t`, which equals the
//! value the block computed only if the method answers what its block
//! answers (`twice: b => b value. b value` does; a method that answers
//! `self`, or folds the block's values, does not). The action only ever
//! appears on a user-defined or otherwise unthreaded callee: the threading
//! constructs (`do:`, `inject:into:`, `ifTrue:`, ...) carry the assignment
//! back themselves, so the diagnostic never fires on them. This is ADR
//! 0133's own fix ("return the new value from the block and assign it where
//! it is called", § REPL session, § User Impact), which leaves the callee's
//! answer to the user. So the action is never marked preferred (the LSP
//! sends `isPreferred: false`; no auto-apply), and its title names the
//! selector and the check: "Assign the result of `twice:` to `t` (check
//! `twice:` answers the block's value)".
//!
//! The escape analysis is not re-derived here: the diagnostic names the
//! assignment (its span) and the block (the note spanning it), and this
//! provider only checks the AST shape around them. Every edit is delimited by
//! AST spans.

use beamtalk_core::ast::{Block, Expression, ExpressionStatement, Module};
use beamtalk_core::ast_walker::{for_each_expr_seq, walk_expression};
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

use crate::value_objects::{CodeAction, SourceEdit};

/// The "return the value instead" actions for the escaping-block diagnostics
/// whose assignment overlaps the requested range (`start == end` is a
/// cursor).
///
/// `diagnostics` runs the file's diagnostic pipeline; it is called only when
/// the range touches an assignment that ends a block, so a code-action
/// request elsewhere (the common case, on every cursor move) stays cheap.
#[must_use]
pub fn compute_return_value_actions(
    module: &Module,
    source: &str,
    diagnostics: impl FnOnce() -> Vec<Diagnostic>,
    start: u32,
    end: u32,
) -> Vec<CodeAction> {
    if !touches_block_final_assignment(module, start, end) {
        return Vec::new();
    }
    diagnostics()
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Tier2BlockNoReturnChannel))
        .filter(|d| overlaps(d.span, start, end))
        .filter_map(|d| return_value_action(module, source, d))
        .collect()
}

/// Whether the range overlaps an assignment that is the last statement of a
/// block literal: the only place the action can apply.
fn touches_block_final_assignment(module: &Module, start: u32, end: u32) -> bool {
    let mut touched = false;
    for_each_expr_seq(module, |seq| {
        for statement in seq {
            walk_expression(&statement.expression, &mut |e| {
                if let Expression::Block(block) = e
                    && let Some(last) = block.body.last()
                    && let Expression::Assignment { span, .. } = &last.expression
                    && overlaps(*span, start, end)
                {
                    touched = true;
                }
            });
        }
    });
    touched
}

/// Whether the diagnostic at `span` is under the cursor or selection. A
/// cursor just past the assignment (`t := t + 1|`) still counts.
fn overlaps(span: Span, start: u32, end: u32) -> bool {
    if start == end {
        span.start() <= start && start <= span.end()
    } else {
        span.start() < end && start < span.end()
    }
}

fn return_value_action(
    module: &Module,
    source: &str,
    diagnostic: &Diagnostic,
) -> Option<CodeAction> {
    let write = diagnostic.span;
    // The diagnostic's "the block" note: the escaping block literal, which
    // contains the assignment.
    let block_span = diagnostic
        .notes
        .iter()
        .filter_map(|note| note.span)
        .find(|span| span.contains(write))?;
    let (statement, selector, block) = statement_sending_block(module, block_span)?;
    let block = &block;

    if assignment_count(block) != 1 {
        return None;
    }
    let Expression::Assignment {
        target,
        value,
        span,
        ..
    } = &block.body.last()?.expression
    else {
        return None;
    };
    if *span != write {
        return None;
    }
    let Expression::Identifier(local) = target.as_ref() else {
        return None;
    };
    let value_text = source.get(value.span().as_range())?;
    let name = &local.name;
    Some(CodeAction::with_edits(
        format!(
            "Assign the result of `{selector}` to `{name}` (check `{selector}` answers the \
             block's value)"
        ),
        vec![
            SourceEdit::insert(statement.start(), format!("{name} := ")),
            SourceEdit::replace(write, value_text),
        ],
    ))
}

/// The number of assignments (plain or destructuring) anywhere in `block`,
/// nested blocks included.
fn assignment_count(block: &Block) -> usize {
    let mut count = 0;
    for statement in &block.body {
        walk_expression(&statement.expression, &mut |e| {
            if matches!(
                e,
                Expression::Assignment { .. } | Expression::DestructureAssignment { .. }
            ) {
                count += 1;
            }
        });
    }
    count
}

/// The span of the statement whose expression is a synchronous message send
/// with the block literal at `block_span` as its receiver or one of its
/// arguments, with the send's selector and (a copy of) that block. Searches
/// every statement sequence of the module: the top-level expressions, method
/// bodies and block bodies.
fn statement_sending_block(module: &Module, block_span: Span) -> Option<(Span, EcoString, Block)> {
    let mut found = None;
    for_each_expr_seq(module, |seq| {
        if found.is_none() {
            found = in_sequence(seq, block_span);
        }
    });
    found
}

fn in_sequence(seq: &[ExpressionStatement], block_span: Span) -> Option<(Span, EcoString, Block)> {
    for statement in seq {
        let expression = &statement.expression;
        if !expression.span().contains(block_span) {
            continue;
        }
        if let Some((selector, block)) = sent_block(expression, block_span) {
            return Some((expression.span(), selector, block.clone()));
        }
        // The block is deeper: look in the bodies of the block literals of
        // this statement.
        let mut found = None;
        walk_expression(expression, &mut |e| {
            if found.is_none()
                && let Expression::Block(inner) = e
                && inner.span != block_span
                && inner.span.contains(block_span)
            {
                found = Some(inner.clone());
            }
        });
        // `walk_expression` is pre-order, so the first enclosing block is the
        // outermost; recursing into its body finds the rest.
        return found.and_then(|inner| in_sequence(&inner.body, block_span));
    }
    None
}

/// The selector of `expression`, a synchronous message send, and the block
/// literal at `block_span` when it is the send's receiver or an argument.
fn sent_block(expression: &Expression, block_span: Span) -> Option<(EcoString, &Block)> {
    let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: false,
        ..
    } = expression
    else {
        return None;
    };
    std::iter::once(receiver.as_ref())
        .chain(arguments)
        .find_map(|operand| match operand {
            Expression::Block(block) if block.span == block_span => Some((selector.name(), block)),
            _ => None,
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::source_analysis::{lex_with_eof, parse};

    fn apply(source: &str, action: &CodeAction) -> String {
        let mut out = source.to_string();
        for edit in action.edits.iter().rev() {
            out.replace_range(edit.span.as_range(), &edit.new_text);
        }
        out
    }

    fn diagnostic(write: Span, block: Span) -> Diagnostic {
        Diagnostic::error("escaping", write)
            .with_note("the block", Some(block))
            .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel)
    }

    fn span_of(source: &str, text: &str) -> Span {
        let start = u32::try_from(source.find(text).expect("text in source")).unwrap();
        Span::new(start, start + u32::try_from(text.len()).unwrap())
    }

    fn actions(source: &str, write: &str, block: &str) -> Vec<CodeAction> {
        let (module, _) = parse(lex_with_eof(source));
        let d = diagnostic(span_of(source, write), span_of(source, block));
        let at = d.span.start();
        compute_return_value_actions(&module, source, || vec![d], at, at)
    }

    #[test]
    fn rewrites_the_one_assignment_shape() {
        let source = "t := 0\nTwice twice: [t := t + 1]\nt";
        let found = actions(source, "t := t + 1", "[t := t + 1]");
        assert_eq!(found.len(), 1);
        assert_eq!(
            apply(source, &found[0]),
            "t := 0\nt := Twice twice: [t + 1]\nt"
        );
    }

    #[test]
    fn finds_the_statement_inside_an_enclosing_block() {
        let source = "t := 0\n#(1) do: [:x | Twice twice: [t := t + x]]\nt";
        let found = actions(source, "t := t + x", "[t := t + x]");
        assert_eq!(found.len(), 1);
        assert_eq!(
            apply(source, &found[0]),
            "t := 0\n#(1) do: [:x | t := Twice twice: [t + x]]\nt"
        );
    }

    #[test]
    fn not_offered_when_the_send_is_not_a_statement() {
        let source = "t := 0\nr := Twice twice: [t := t + 1]\nr";
        assert!(actions(source, "t := t + 1", "[t := t + 1]").is_empty());
    }

    #[test]
    fn not_offered_when_the_block_is_stored() {
        let source = "t := 0\nf := [t := t + 1]\nf";
        assert!(actions(source, "t := t + 1", "[t := t + 1]").is_empty());
    }

    #[test]
    fn not_offered_when_the_assignment_is_not_last() {
        let source = "t := 0\nTwice twice: [t := t + 1. 7]\nt";
        assert!(actions(source, "t := t + 1", "[t := t + 1. 7]").is_empty());
    }

    #[test]
    fn not_offered_for_a_second_assignment() {
        let source = "t := 0\nu := 0\nTwice twice: [u := 1. t := t + 1]\nt";
        assert!(actions(source, "t := t + 1", "[u := 1. t := t + 1]").is_empty());
    }

    /// A field write; a class variable is written the same way (`self.x :=`
    /// in a class method), so this covers both.
    #[test]
    fn not_offered_for_a_field_write_alongside() {
        let source = "t := 0\nTwice twice: [self.n := 1. t := t + 1]\nt";
        assert!(actions(source, "t := t + 1", "[self.n := 1. t := t + 1]").is_empty());
    }

    #[test]
    fn not_offered_away_from_the_diagnostic() {
        let source = "t := 0\nTwice twice: [t := t + 1]\nt";
        let (module, _) = parse(lex_with_eof(source));
        let d = diagnostic(
            span_of(source, "t := t + 1"),
            span_of(source, "[t := t + 1]"),
        );
        assert!(compute_return_value_actions(&module, source, || vec![d], 0, 0).is_empty());
    }
}
