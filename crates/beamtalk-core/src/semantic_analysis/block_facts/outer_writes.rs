// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Outer-local write analysis.
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! Which locals of an enclosing scope a block (or statement) writes, found by
//! [`OuterWriteWalker`]; the scope-tracking walk behind [`outer_local_writes`].

use super::{LocalThreadingConstruct, local_threading_construct};
use crate::ast::{BinarySegment, Block, Expression};
use crate::source_analysis::Span;
use std::collections::HashSet;

/// What an [`OuterLocalWrite`] writes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriteKind {
    /// A local of an enclosing scope ([`outer_local_writes`]).
    Local,
    /// Object state: `self.field :=`, an instance field or, in a class-side
    /// method, a class variable ([`outer_state_writes`], BT-3922).
    ObjectState,
}

/// One outer write found by [`outer_local_writes`] or [`outer_state_writes`]:
/// the name written and the span of the assignment that writes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OuterLocalWrite {
    /// What is written: a captured local, or object state.
    pub kind: WriteKind,
    /// The outer local, or the field, written.
    pub name: ecow::EcoString,
    /// The assignment (or destructuring pattern) that writes it.
    pub span: Span,
}

/// ADR 0131 (BT-3745): the outer locals `block` writes, anywhere in its body
/// including nested blocks, in source order (first write of each name only).
///
/// A name is an outer local when `bound_outside(name)` says it is bound in
/// the scope enclosing `block` and nothing between that scope and the write
/// (the block's own parameters, a parameter of a nested block, a match-arm or
/// destructuring binding, or an earlier first assignment that introduced it
/// as a block local) rebinds it. Write-only writes count: a block that
/// assigns an outer local without reading it still writes the caller's local
/// in Smalltalk, so it is a Tier 2 block value (ADR 0041) and its write must
/// be threaded back or it is lost.
///
/// This is the single "does this block write an outer local" fact that both
/// ADR 0131 diagnostics (§6 and the Phase 0 allow-set, in
/// `validators/local_threading.rs`) are computed from.
pub fn outer_local_writes(
    block: &Block,
    bound_outside: &dyn Fn(&str) -> bool,
) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 0,
        nested: NestedBlocks::All,
    };
    walker.block(block);
    walker.writes
}

/// ADR 0133 (BT-3922): the `self.field` assignments `block` makes anywhere in
/// its body, nested blocks included, first write of each field. The
/// object-state half of the escaping-block rule: in an instance method these
/// are instance fields, in a class-side method class variables (ADR 0130).
/// Computed by the same walk as [`outer_local_writes`].
pub fn outer_state_writes(block: &Block) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside: &|_| false,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 0,
        nested: NestedBlocks::All,
    };
    walker.block(block);
    walker.state_writes
}

/// The names `stmt` itself binds by assignment or destructuring: those not
/// inside a block nested in it, and not a match-arm binding. A nested block's
/// own first assignment binds the name in that block
/// (`_b := [i := 0. [i < 3] whileTrue: [i := i + 1]]` stores a closure whose
/// `i` is not the statement's). The block-scope rule is [`OuterWriteWalker`]'s,
/// the one encoding of "every nested `Block` opens its own binding scope".
/// A name assigned inside a `match:` arm body is not counted either: the
/// walker gives each arm its own frame and pops it after the arm, matching
/// the name resolver's per-arm scope.
#[must_use]
pub fn names_assigned_outside_nested_blocks(stmt: &Expression) -> HashSet<ecow::EcoString> {
    let mut walker = OuterWriteWalker {
        bound_outside: &|_| false,
        frames: vec![HashSet::new()],
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 1,
        nested: NestedBlocks::None,
    };
    walker.expr(stmt);
    walker.frames.pop().unwrap_or_default()
}

/// Scope-tracking walk behind [`outer_local_writes`].
pub(super) struct OuterWriteWalker<'a> {
    pub(super) bound_outside: &'a dyn Fn(&str) -> bool,
    /// Names bound inside the block being analysed, innermost last.
    pub(super) frames: Vec<HashSet<ecow::EcoString>>,
    pub(super) writes: Vec<OuterLocalWrite>,
    /// Assignments to `self.field` (an instance field, or a class variable in a
    /// class-side method) inside the walked block, first write of each field.
    pub(super) state_writes: Vec<OuterLocalWrite>,
    /// Which nested blocks the walk descends into.
    pub(super) nested: NestedBlocks,
    /// How many walked blocks enclose the current expression.
    pub(super) blocks_entered: usize,
}

/// Which nested blocks an [`OuterWriteWalker`] descends into.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum NestedBlocks {
    /// No block at all ([`names_assigned_outside_nested_blocks`]): every
    /// nested `Block` opens its own binding scope, so none is entered.
    None,
    /// Every block ([`outer_local_writes`]).
    All,
    /// Only the blocks of nested local-threading constructs
    /// ([`construct_outer_local_writes`](super::construct_outer_local_writes)).
    Producers,
    /// Only the blocks of nested constructs whose family is threaded today
    /// ([`threaded_today_block_writes`](super::threaded_today_block_writes)).
    ProducersThreadedToday,
}

impl OuterWriteWalker<'_> {
    fn bound_inside(&self, name: &str) -> bool {
        self.frames.iter().any(|f| f.contains(name))
    }

    fn define(&mut self, name: &ecow::EcoString) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name.clone());
        }
    }

    /// An assignment to `name` at `span`: an outer write, or the first
    /// assignment of a block local (which then shadows nothing outside).
    fn assign(&mut self, name: &ecow::EcoString, span: Span) {
        // Outside every walked block (only [`expression_threaded_writes`](super::expression_threaded_writes)
        // walks there): the expression's own write, not a construct's.
        if self.blocks_entered == 0 || self.bound_inside(name) {
            return;
        }
        if (self.bound_outside)(name) {
            if !self.writes.iter().any(|w| &w.name == name) {
                self.writes.push(OuterLocalWrite {
                    kind: WriteKind::Local,
                    name: name.clone(),
                    span,
                });
            }
        } else {
            self.define(name);
        }
    }

    /// Whether an assignment to a field of `receiver` is a write to the state
    /// of a captured `self`: `self.field :=` inside a walked block. A block
    /// that takes `self` as a parameter (`[:self | self.n := ...]`, a
    /// class-builder method body) writes its own receiver's state instead.
    fn is_captured_self_field_write(&self, receiver: &Expression) -> bool {
        self.blocks_entered > 0
            && !self.bound_inside("self")
            && matches!(receiver.unwrap_parens(), Expression::Identifier(r) if r.name == "self")
    }

    /// The target of an assignment at `span`: a local, a captured `self`'s
    /// field, or an expression walked for what it contains.
    fn assignment_target(&mut self, target: &Expression, span: Span) {
        match target {
            Expression::Identifier(id) => self.assign(&id.name, span),
            Expression::FieldAccess {
                receiver, field, ..
            } if self.is_captured_self_field_write(receiver) => {
                self.state_write(&field.name, span);
            }
            other => self.expr(other),
        }
    }

    /// Records the first `self.field :=` write of each field.
    fn state_write(&mut self, field: &ecow::EcoString, span: Span) {
        if !self.state_writes.iter().any(|w| &w.name == field) {
            self.state_writes.push(OuterLocalWrite {
                kind: WriteKind::ObjectState,
                name: field.clone(),
                span,
            });
        }
    }

    pub(super) fn block(&mut self, block: &Block) {
        self.frames
            .push(block.parameters.iter().map(|p| p.name.clone()).collect());
        self.blocks_entered += 1;
        for stmt in &block.body {
            self.expr(&stmt.expression);
        }
        self.blocks_entered -= 1;
        self.frames.pop();
    }

    /// A message send. Unless every block is walked, only the blocks of a
    /// nested construct are (its writes are the enclosing construct's too);
    /// any other block operand is a closure.
    fn send(&mut self, send: &Expression, receiver: &Expression, arguments: &[Expression]) {
        let nested: Vec<&Block> = match self.nested {
            NestedBlocks::None | NestedBlocks::All => Vec::new(),
            NestedBlocks::Producers => local_threading_construct(send)
                .map(|c| c.blocks)
                .unwrap_or_default(),
            NestedBlocks::ProducersThreadedToday => local_threading_construct(send)
                .filter(LocalThreadingConstruct::is_threaded_today)
                .map(|c| c.blocks)
                .unwrap_or_default(),
        };
        for operand in std::iter::once(receiver).chain(arguments) {
            match operand {
                Expression::Block(b) if nested.iter().any(|n| std::ptr::eq(*n, b)) => {
                    self.block(b);
                }
                other => self.expr(other),
            }
        }
    }

    pub(super) fn expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Assignment {
                target,
                value,
                span,
                ..
            } => {
                self.expr(value);
                self.assignment_target(target, *span);
            }
            Expression::DestructureAssignment {
                pattern,
                value,
                span,
            } => {
                self.expr(value);
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                for id in ids {
                    self.assign(&id.name, *span);
                }
            }
            Expression::Block(block) => {
                if self.nested == NestedBlocks::All {
                    self.block(block);
                }
            }
            Expression::Match { value, arms, .. } => {
                self.expr(value);
                for arm in arms {
                    let (ids, _) =
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
                    self.frames
                        .push(ids.into_iter().map(|id| id.name).collect());
                    if let Some(guard) = &arm.guard {
                        self.expr(guard);
                    }
                    self.expr(&arm.body);
                    self.frames.pop();
                }
            }
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => self.send(expr, receiver, arguments),
            Expression::Cascade {
                receiver, messages, ..
            } => {
                self.expr(receiver);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.expr(arg);
                    }
                }
            }
            Expression::FieldAccess { receiver, .. } => self.expr(receiver),
            Expression::Return { value, .. } => self.expr(value),
            Expression::Parenthesized { expression, .. } => self.expr(expression),
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.expr(&pair.key);
                    self.expr(&pair.value);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements {
                    self.expr(e);
                }
                if let Some(t) = tail {
                    self.expr(t);
                }
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    self.expr(e);
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(e) = segment {
                        self.expr(e);
                    }
                }
            }
            Expression::BinaryConstruction { segments, .. } => {
                for e in segments.iter().flat_map(BinarySegment::sub_expressions) {
                    self.expr(e);
                }
            }
            Expression::Literal(..)
            | Expression::Identifier(_)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => {}
        }
    }
}

pub(super) fn walk_construct_blocks(
    blocks: &[&Block],
    bound_outside: &dyn Fn(&str) -> bool,
    nested: NestedBlocks,
) -> Vec<OuterLocalWrite> {
    let mut walker = OuterWriteWalker {
        bound_outside,
        frames: Vec::new(),
        writes: Vec::new(),
        state_writes: Vec::new(),
        blocks_entered: 0,
        nested,
    };
    for block in blocks {
        walker.block(block);
    }
    walker.writes
}
