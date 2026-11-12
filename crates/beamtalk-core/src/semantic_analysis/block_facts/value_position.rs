// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value-position local assignments (ADR 0131 §1).
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! Every local assignment that sits in value position, lowered as a
//! `LocalRebind` of the enclosing frame.

use super::{RootMethodKind, conditional_arm_blocks};
use crate::ast::{Block, Expression, ExpressionStatement};
use crate::source_analysis::Span;
use ecow::EcoString;
use std::collections::HashSet;

/// BT-3910 (ADR 0131 §1): every local (identifier-target) assignment in `body`
/// that sits in *value position*: a producer, lowered as a `LocalRebind` of the
/// enclosing frame. `root` is the kind of method `body` is the body of, or
/// `None` for a top-level expression list (a REPL input or a script).
///
/// A local assignment is in statement position when it is, bare, a statement
/// of a body: a method body, a block body or a top-level expression list.
/// Every body lowering binds the local there. Parentheses around a statement
/// that is not the body's last are unwrapped only at the root of a value-type
/// or actor method (BT-3830, BT-3838: `(t := t + 1)` followed by `t`) and in
/// an inlined arm ([`conditional_arm_blocks`]: a conditional's arm, a lookup
/// handler, a `[...] value` body); a method body's own last statement is
/// unwrapped too (its value is the method's, and nothing reads a local after
/// it). The class-method root, the REPL root, a last statement of a block and
/// every other block body (a loop or fold body, an exception body, a closure)
/// compile a parenthesised statement as an expression. Anywhere else the
/// assignment is in value position: an operand (`x := (t := t + 1) + 1`), a
/// collection-literal element (`#[t, (t := 7), t]`), the value of another
/// assignment (`x := t := 7`) or of `^`, a conditional's or a cascade's
/// receiver (`(t := t + 1) > 0 ifTrue: [...]`), an interpolated segment, a
/// `match:` subject or arm. A field assignment (`self.x := ...`) is never one:
/// it threads `State` (BT-3803's producer).
///
/// The one rule codegen's producer gates read (as
/// [`value_position_assignment_spans`]) and the agreement tests
/// (`beamtalk-codegen`'s and `beamtalk-repl`'s
/// `value_position_assignment_agreement`) prove: codegen drops none of them.
#[must_use]
pub fn value_position_local_assignments(
    body: &[ExpressionStatement],
    root: Option<RootMethodKind>,
) -> Vec<ValuePositionAssignment> {
    let exprs: Vec<&Expression> = body.iter().map(|stmt| &stmt.expression).collect();
    value_position_assignments_in(&exprs, root)
}

/// [`value_position_local_assignments`] over a body given as its statements'
/// expressions, so a caller holding `&Expression`s need not clone them into
/// `ExpressionStatement`s.
fn value_position_assignments_in(
    body: &[&Expression],
    root: Option<RootMethodKind>,
) -> Vec<ValuePositionAssignment> {
    let mut finder = ValuePositionAssignments::default();
    let unwraps_parens = matches!(
        root,
        Some(RootMethodKind::ValueType | RootMethodKind::Actor)
    );
    finder.body(body, unwraps_parens, root.is_some());
    finder.found
}

/// BT-3910 (ADR 0131 §1): the spans of the value-position local assignments
/// ([`value_position_local_assignments`]) of one body: a method body of kind
/// `root`, or a top-level expression list (`None`). Codegen installs the set per
/// method body (`with_local_producer_sites`), never module-wide: a `Span` carries
/// no file id, and a `uses:` provision's body keeps the spans of the protocol's
/// file, so only spans of one body are guaranteed distinct from each other.
/// Codegen's producer gates read this set: one rule decides which assignments
/// are producers, so a gate cannot disagree with the lowering.
#[must_use]
pub fn value_position_assignment_spans<'a>(
    body: impl IntoIterator<Item = &'a Expression>,
    root: Option<RootMethodKind>,
) -> HashSet<Span> {
    let exprs: Vec<&Expression> = body.into_iter().collect();
    value_position_assignments_in(&exprs, root)
        .into_iter()
        .map(|found| found.span)
        .collect()
}

/// BT-3910: every identifier-target assignment inside `expr` (itself
/// included) that is not inside a block literal, as its target and span, in
/// source order. A block literal is a closure or an inlined arm whose
/// statements are lowered by their own statement lowering, so its assignments
/// are not `expr`'s. Codegen filters the result by
/// [`value_position_assignment_spans`] to find the value-position assignments
/// a sequencing pass or a snapshot rule must see in `expr`.
#[must_use]
pub fn assignments_outside_blocks(expr: &Expression) -> Vec<(EcoString, Span)> {
    use crate::ast::visitor::Visitor;
    #[derive(Default)]
    struct Found(Vec<(EcoString, Span)>);
    impl<'ast> Visitor<'ast> for Found {
        fn visit_expr(&mut self, expr: &'ast Expression) {
            if let Expression::Assignment { target, .. } = expr
                && let Expression::Identifier(id) = target.as_ref()
            {
                self.0.push((id.name.clone(), expr.span()));
            }
            crate::ast::visitor::walk_expr(self, expr);
        }
    }
    let mut found = Found::default();
    found.visit_expr(expr);
    found.0
}

/// One local assignment [`value_position_local_assignments`] names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValuePositionAssignment {
    /// The local it assigns.
    pub name: ecow::EcoString,
    /// The whole assignment (`t := t + 1`, without enclosing parentheses).
    pub span: Span,
    /// Whether the assignment is the whole value of a `^` (`^t := e`), whose
    /// usual rewrite is `^e` (an `ensure:` cleanup can still read `t`, so it
    /// is rejected all the same).
    pub returned: bool,
}

/// The walk behind [`value_position_local_assignments`].
#[derive(Default)]
struct ValuePositionAssignments {
    found: Vec<ValuePositionAssignment>,
}

impl ValuePositionAssignments {
    /// The statements of a body; `unwraps_parens` is whether its lowering
    /// binds a parenthesised local assignment that is not its last statement.
    ///
    /// `method_root` is whether `body` is a method's own body: its last
    /// statement is the method's value and nothing reads a local after it, so
    /// a parenthesised assignment there writes nothing observable and is
    /// judged like a bare one (the REPL's last statement is not: its
    /// bindings persist).
    fn statements(
        &mut self,
        body: &[ExpressionStatement],
        unwraps_parens: bool,
        method_root: bool,
    ) {
        let exprs: Vec<&Expression> = body.iter().map(|stmt| &stmt.expression).collect();
        self.body(&exprs, unwraps_parens, method_root);
    }

    fn body(&mut self, body: &[&Expression], unwraps_parens: bool, method_root: bool) {
        use crate::ast::visitor::Visitor;
        for (i, stmt_expr) in body.iter().enumerate() {
            let last = i + 1 == body.len();
            let expr = if (unwraps_parens && !last) || (method_root && last) {
                stmt_expr.unwrap_parens()
            } else {
                *stmt_expr
            };
            match expr {
                // Binds its local: only its value is judged.
                Expression::Assignment { target, value, .. }
                    if matches!(target.as_ref(), Expression::Identifier(_)) =>
                {
                    self.visit_expr(value);
                }
                other => self.visit_expr(other),
            }
        }
    }
}

impl<'ast> crate::ast::visitor::Visitor<'ast> for ValuePositionAssignments {
    fn visit_expr(&mut self, expr: &'ast Expression) {
        // `^t := e`: the same value position, told apart only for the hint.
        if let Expression::Return {
            value: returned, ..
        } = expr
            && let assignment @ Expression::Assignment {
                target,
                value: assigned,
                ..
            } = returned.unwrap_parens()
            && let Expression::Identifier(id) = target.as_ref()
        {
            self.found.push(ValuePositionAssignment {
                name: id.name.clone(),
                span: assignment.span(),
                returned: true,
            });
            self.visit_expr(assigned);
            return;
        }
        if let Expression::Assignment { target, .. } = expr
            && let Expression::Identifier(id) = target.as_ref()
        {
            self.found.push(ValuePositionAssignment {
                name: id.name.clone(),
                span: expr.span(),
                returned: false,
            });
        }
        // An inlined arm (a conditional's, a lookup handler, `[...] value`)
        // is lowered like a method root: a parenthesised statement that is
        // not the arm's value binds its local.
        if let Some(arms) = conditional_arm_blocks(expr)
            && let Expression::MessageSend {
                receiver,
                arguments,
                ..
            } = expr.unwrap_parens()
        {
            for operand in std::iter::once(receiver.as_ref()).chain(arguments) {
                match operand {
                    Expression::Block(block)
                        if arms.iter().any(|arm| std::ptr::eq(*arm, block)) =>
                    {
                        self.statements(&block.body, true, false);
                    }
                    other => self.visit_expr(other),
                }
            }
            return;
        }
        crate::ast::visitor::walk_expr(self, expr);
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.statements(&block.body, false, false);
    }
}
