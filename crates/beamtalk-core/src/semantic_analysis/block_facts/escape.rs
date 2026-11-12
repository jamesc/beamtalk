// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Escape analysis for block literals in class methods.
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! How a block literal leaves the class-method invocation that created it
//! (ADR 0130 §5): returned, stored, or handed to another send.

use super::{
    LocalThreadingFamily, conditional_arm_blocks, exception_arm_blocks, local_threading_construct,
};
use crate::ast::{Block, Expression, ExpressionStatement};
use crate::source_analysis::Span;

/// How a block literal leaves the class-method invocation that created it
/// (ADR 0130 §5): the shapes the Phase 0 census (BT-3703) counts and the
/// `class-state-abroad` lint (BT-3712) warns on. One definition, shared by
/// both, so they cannot disagree about what "escaping" means.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EscapeShape {
    /// Returned: the method's last statement, or the operand of `^`.
    Returned,
    /// Assigned to a local variable (`b := [...]`).
    StoredLocal,
    /// Assigned to a class variable (`self.cb := [...]`).
    StoredClassVar,
    /// An element of a list, array or map literal.
    StoredInLiteral,
}

/// The block literal an escape operand is ([`for_each_escape_operand`] hands
/// every operand over with its parentheses already peeled).
fn as_block(expr: &Expression) -> Option<&Block> {
    match expr {
        Expression::Block(block) => Some(block),
        _ => None,
    }
}

/// How far [`for_each_escape_operand`] follows a returned operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ReturnedOperands {
    /// Only the method's last statement and the operand of each `^`, as
    /// written: what the class-state-abroad lint and the Phase 0 census have
    /// counted since BT-3703 ([`escaping_blocks`]).
    Direct,
    /// As [`Self::Direct`], and also through what makes up such an operand's
    /// value (BT-3806): the value of a local assignment (`b := [..]` as the
    /// last statement, `^(b := [..])`), the last statement of each arm of an
    /// inlined conditional, `[..] value`, `on:do:` or `ensure:` body
    /// ([`value_arm_blocks`]) and the body of each `match:` arm, repeatedly.
    /// What the ADR 0131 §6 check reads ([`escape_sites`]).
    ThroughValue,
}

/// The literal blocks whose last statement's value is the value of `expr`
/// when it runs: the arms of a conditional, a lookup handler or an evaluated
/// literal block ([`conditional_arm_blocks`]), and the protected body and
/// handler of an `on:do:` or the protected body of an `ensure:`
/// ([`exception_arm_blocks`]; a `tryDo:` answers a `Result`, not its block's
/// value). Empty for any other expression.
fn value_arm_blocks(expr: &Expression) -> Vec<&Block> {
    if let Some(arms) = conditional_arm_blocks(expr) {
        return arms;
    }
    let Some(construct) = local_threading_construct(expr) else {
        return Vec::new();
    };
    if construct.family != LocalThreadingFamily::Exception {
        return Vec::new();
    }
    let mut arms = exception_arm_blocks(expr).unwrap_or_default();
    if construct.selector == "ensure:" {
        // `ensure:` answers its protected body's value, not the cleanup's.
        arms.truncate(1);
    }
    arms
}

/// Calls `visit` with `operand`, a returned operand, and (for
/// [`ReturnedOperands::ThroughValue`]) with each expression whose value it
/// answers, all as [`EscapeShape::Returned`].
fn visit_returned<'a>(
    operand: &'a Expression,
    returns: ReturnedOperands,
    visit: &mut dyn FnMut(&'a Expression, EscapeShape),
) {
    let operand = operand.unwrap_parens();
    visit(operand, EscapeShape::Returned);
    if returns == ReturnedOperands::Direct {
        return;
    }
    match operand {
        Expression::Assignment { target, value, .. }
            if matches!(**target, Expression::Identifier(_)) =>
        {
            visit_returned(value, returns, visit);
        }
        Expression::Match { arms, .. } => {
            for arm in arms {
                visit_returned(&arm.body, returns, visit);
            }
        }
        _ => {
            for block in value_arm_blocks(operand) {
                if let Some(last) = block.body.last() {
                    visit_returned(&last.expression, returns, visit);
                }
            }
        }
    }
}

/// The block literals `expr` may evaluate to, through what makes up its value
/// as [`ReturnedOperands::ThroughValue`] follows it (`[..]`,
/// `c ifTrue: [[..]] ifFalse: [[..]]`, `b := [..]`, `[[..]] value`): the
/// blocks [`escape_sites`] would report for a returned `expr`. ADR 0131 §6
/// reads it for a local bound to such a value (BT-3806).
#[must_use]
pub fn value_blocks(expr: &Expression) -> Vec<&Block> {
    let mut found = Vec::new();
    visit_returned(expr, ReturnedOperands::ThroughValue, &mut |operand, _| {
        if let Expression::Block(block) = operand {
            found.push(block);
        }
    });
    found
}

/// Calls `visit` with every operand of a method body (nested blocks included)
/// that is returned or stored, and how it escapes: the one classification
/// behind [`escaping_blocks`] and [`escape_sites`], so they cannot disagree
/// about what an escape site is. Purely syntactic: an operand passed as a
/// message argument is not an escape here (whether it escapes depends on the
/// callee). The body's last statement is a returned operand. `returns` says
/// how far a returned operand is followed ([`ReturnedOperands`]). An operand
/// may be visited more than once, with different shapes (`^(b := [..])` is
/// both returned and stored in a local under
/// [`ReturnedOperands::ThroughValue`]).
fn for_each_escape_operand(
    body: &[ExpressionStatement],
    returns: ReturnedOperands,
    visit: &mut dyn FnMut(&Expression, EscapeShape),
) {
    if let Some(last) = body.last() {
        visit_returned(&last.expression, returns, visit);
    }
    for stmt in body {
        crate::ast_walker::walk_expression(&stmt.expression, &mut |expr| match expr {
            Expression::Return { value, .. } => visit_returned(value, returns, visit),
            Expression::Assignment { target, value, .. } => {
                let shape = if matches!(**target, Expression::FieldAccess { .. }) {
                    EscapeShape::StoredClassVar
                } else {
                    EscapeShape::StoredLocal
                };
                visit(value.unwrap_parens(), shape);
            }
            Expression::ListLiteral { elements, .. }
            | Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for element in elements {
                    visit(element.unwrap_parens(), EscapeShape::StoredInLiteral);
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    visit(pair.value.unwrap_parens(), EscapeShape::StoredInLiteral);
                }
            }
            _ => {}
        });
    }
}

/// Every block literal in a method body (nested blocks included) that is
/// returned or stored, with how it escapes (cloned: the walker's borrows do
/// not outlive its callback). See [`for_each_escape_operand`] for what counts;
/// a returned operand is taken as written ([`ReturnedOperands::Direct`]), so a
/// block that is only the value of a conditional arm or of a last-statement
/// `b := [..]` is not one here (the census and the class-state-abroad lint
/// keep the counts they were measured with; the ADR 0131 §6 check follows
/// both, [`escape_sites`]).
pub fn escaping_blocks(body: &[ExpressionStatement]) -> Vec<(Block, EscapeShape)> {
    let mut found = Vec::new();
    for_each_escape_operand(body, ReturnedOperands::Direct, &mut |operand, shape| {
        if let Some(block) = as_block(operand) {
            found.push((block.clone(), shape));
        }
    });
    found
}

/// The spans of the block literals and bare local-variable references in a
/// method body that are themselves a returned or stored operand (`^[..]`,
/// `self.f := b`, `#[b]`), with how they escape: the same classification as
/// [`escaping_blocks`], the body's last statement included, with a returned
/// operand also followed through what makes up its value
/// ([`ReturnedOperands::ThroughValue`], BT-3806: `c ifTrue: [[..]]`,
/// `b := [..]` as the last statement, `^(b := [..])`). A local stored into
/// another local is reported as [`EscapeShape::StoredLocal`]. A span may
/// appear more than once with different shapes (`^(b := [..])`: the block is
/// both returned and stored in a local).
#[must_use]
pub fn escape_sites(body: &[ExpressionStatement]) -> Vec<(Span, EscapeShape)> {
    let mut found = Vec::new();
    for_each_escape_operand(
        body,
        ReturnedOperands::ThroughValue,
        &mut |operand, shape| match operand {
            Expression::Block(block) => found.push((block.span, shape)),
            Expression::Identifier(id) => found.push((id.span, shape)),
            _ => {}
        },
    );
    found
}
