// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The ADR 0131 / 0133 walk behind [`super::check_local_threading`]: scope
//! and position tracking, the escaping-block check and the retired Phase 0
//! position check.
//!
//! **DDD Context:** Semantic Analysis

use crate::ast::{
    BinarySegment, Block, Expression, ExpressionStatement, MatchArm, MessageSelector,
    WellKnownSelector,
};
use crate::semantic_analysis::block_facts::{
    EscapeShape, LocalThreadingConstruct, LocalThreadingFamily, LoopBodyContext, OuterLocalWrite,
    RootMethodKind, WriteKind, block_literal_of_other_arity, conditional_arm_blocks,
    conditional_producer_writes, construct_outer_local_writes, escape_sites, exception_arm_blocks,
    exception_producer_writes, is_actor_fold_value_site, is_fold_frame_body,
    is_loop_body_producer_site, is_threaded_frame_body, literal_block_value_arm,
    local_threading_construct, lookup_arm_blocks, loop_body_producer_sites, match_arm_block,
    match_arm_body_writes, match_arms, match_guard_writes, match_producer_writes,
    operand_producer_writes, outer_local_writes, outer_state_writes, producer_body_block,
    receiver_statically_not_dictionary, repl_arm_producer_sites, repl_evaluated_block,
    repl_operand_sites, repl_producer_writes, root_producer_writes, sequenced_operand_sites,
    value_blocks,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

use super::escapes::{Escape, threading_constructs_note};
use super::types::{ConstructKind, Container, MethodContext, Position};
use super::{PLAIN_ARM_SELECTORS, ReceiverFacts};

use super::types::ConstructKind::{
    AndOr, DetectIfNone, DetectIfNoneSearch, Do, IfNil, IfTrue, KeyedDo, ListOp, TryDo,
};
use super::types::MethodContext::{Actor, Class, Repl, ValueType};

/// BT-3823: whether `expr` is an evaluated block in the REPL sense
/// ([`repl_evaluated_block`]: a `value`-family send of the block's own arity to
/// a bare block literal), whose block the REPL inlines over its bindings map
/// when it writes a binding: a REPL arm construct.
fn is_repl_evaluated_block(expr: &Expression) -> bool {
    repl_evaluated_block(expr).is_some()
}

/// The walk behind [`check_local_threading`] for one method body: tracks
/// scope (so a write can be told apart from a block-local definition) and the
/// block nesting depth (a construct inside a block is in a nested frame).
/// The current statement of a threaded frame body (ADR 0131 Phase 2b).
#[derive(Default)]
struct FrameSites {
    /// The statement's [`loop_body_producer_sites`].
    sites: HashSet<Span>,
    /// The statement's [`LoopBodyContext`] with the frame facts that do not
    /// change per statement filled in at the statement.
    statement: LoopBodyContext,
    /// Whether the frame is a fold's ([`is_fold_frame_body`]).
    fold: bool,
    /// Whether the frame is an arm of a conditional that is a local-threading
    /// producer (ADR 0131 Phase 3): a `BranchArm`, not a loop or fold body.
    arm: bool,
    /// Whether the arm is a *plain* arm: an arm of a conditional that is not
    /// a producer (it writes no outer local), a `BranchArm` frame that codegen
    /// registers every arm-frame producer site in (`generate_plain_arm`,
    /// BT-3949, BT-4460): a loop or fold, and a nested conditional, `on:do:`
    /// or `match:` that writes a local of the arm
    /// (`arm_frame_conditional_producer`). It differs from a
    /// producer's arm only in the positions a loop or fold is accepted at
    /// (`check_construct`'s plain-arm position check).
    plain: bool,
}

/// What kind of threaded frame body a block operand is about to be walked as
/// ([`Walker::pending_frame_body`]).
#[derive(Debug, Clone, Copy)]
struct FrameBody {
    /// A fold's body ([`is_fold_frame_body`]).
    fold: bool,
    /// An arm of a conditional producer (ADR 0131 Phase 3).
    arm: bool,
    /// An arm of a conditional that is not a producer ([`FrameSites::plain`]).
    plain: bool,
}

#[allow(clippy::struct_excessive_bools)] // independent one-shot walk flags, each documented
pub(super) struct Walker<'d> {
    context: MethodContext,
    /// Names bound in the method so far, innermost frame last.
    pub(super) scope: Vec<HashSet<EcoString>>,
    /// Block nesting depth of the expression being walked.
    depth: usize,
    /// For each enclosing block, innermost last: the index in `scope` of
    /// its first frame.
    block_frames: Vec<usize>,
    /// For each enclosing block, innermost last: its role.
    containers: Vec<Container>,
    /// For the method body and each enclosing block, innermost last:
    /// whether the statement being walked in that body is its last.
    last_statements: Vec<bool>,
    /// Whether the outermost enclosing block touches the method's state
    /// (see [`Position::NestedStatement`]).
    outer_stateful: bool,
    /// For each enclosing block, innermost last: whether it rebinds a local
    /// bound outside it (see [`Position::NestedStatement`]).
    direct_blocks: Vec<bool>,
    /// The spans of the block literals that make up the value of some
    /// `x := ...` in the method ([`value_blocks`]: `b := [..]`, `b := c
    /// ifTrue: [[..]] ifFalse: [..]`): the blocks
    /// [`Walker::note_block_binding`] reports as stored (BT-3859).
    value_operands: HashSet<Span>,
    /// For each block in [`Walker::value_operands`], its first outer-local
    /// write, taken while the walk is at the block, with the scope it is
    /// written in (BT-3859): by the time the assignment's binding is noted,
    /// the frames of the conditional arms the value passes through have been
    /// popped, so a local first bound in an arm (`b := c ifTrue: [u := 0.
    /// [u := u + 1]] ...`) would look like the block's own.
    value_block_writes: HashMap<Span, OuterLocalWrite>,
    /// The block literals already reported as escaping (one diagnostic per
    /// block, whichever way it is first seen to escape).
    reported_blocks: HashSet<Span>,
    /// The escaping-block diagnostics, keyed by the assignment they report
    /// and the block that escapes, in walk order ([`Walker::report_escape`]).
    escaping_writes: Vec<(Span, Span, Diagnostic)>,
    /// The enclosing method's selector, for [`Escape::Returned`]; `None` at
    /// the REPL root.
    pub(super) method_selector: Option<EcoString>,
    /// The spans of the current method-body statement's
    /// [`sequenced_operand_sites`]: where a loop or fold is lowered as an
    /// ADR 0131 local-threading producer (BT-3749).
    operand_sites: HashSet<Span>,
    /// ADR 0131 Phase 2b (BT-3793): for each enclosing block, innermost last,
    /// `Some(sites)` when it is the body of a loop or fold that codegen lowers
    /// as a threaded frame ([`is_threaded_frame_body`]) — `sites` being the
    /// current statement's [`loop_body_producer_sites`] — and `None` for any
    /// other block (an arm, a handler, a protected body, a closure).
    frame_sites: Vec<Option<FrameSites>>,
    /// Set by a send's walk just before it descends into a block operand that
    /// is a threaded frame body; consumed by [`Walker::block`].
    pending_frame_body: Option<FrameBody>,
    /// ADR 0131 Phase 2d (BT-3798): the spans of the lookup sends
    /// ([`LocalThreadingFamily::Lookup`]) accepted as local-threading
    /// producers at their position by [`Walker::check_construct`], whose
    /// handler blocks the §6 check then exempts ([`Walker::check_send`]).
    lookup_producers: HashSet<Span>,
    /// What the lookup-receiver rule reads; `None` only before [`check_body`]
    /// has set it.
    pub(super) receivers: Option<ReceiverFacts<'d>>,
    /// ADR 0131 §6 (BT-3756): the block literals of the method body that
    /// escape the method without a send — returned, stored in a field or
    /// class variable, or an element of a collection literal
    /// ([`escape_sites`], the classification behind `escaping_blocks`). Keyed
    /// by the operand's span.
    escapes: HashMap<Span, EscapeShape>,
    diagnostics: &'d mut Vec<Diagnostic>,
}

impl<'d> Walker<'d> {
    pub(super) fn new(
        context: MethodContext,
        body: &[ExpressionStatement],
        diagnostics: &'d mut Vec<Diagnostic>,
    ) -> Self {
        let mut value_operands = HashSet::new();
        for stmt in body {
            crate::ast_walker::walk_expression(&stmt.expression, &mut |e| {
                if let Expression::Assignment { target, value, .. } = e {
                    if matches!(target.as_ref(), Expression::Identifier(_)) {
                        value_operands.extend(value_blocks(value).into_iter().map(|b| b.span));
                    }
                }
            });
        }
        Self {
            context,
            scope: Vec::new(),
            depth: 0,
            block_frames: Vec::new(),
            containers: Vec::new(),
            last_statements: Vec::new(),
            outer_stateful: false,
            direct_blocks: Vec::new(),
            value_operands,
            value_block_writes: HashMap::new(),
            reported_blocks: HashSet::new(),
            escaping_writes: Vec::new(),
            method_selector: None,
            operand_sites: HashSet::new(),
            frame_sites: Vec::new(),
            pending_frame_body: None,
            lookup_producers: HashSet::new(),
            receivers: None,
            escapes: escape_map(body),
            diagnostics,
        }
    }

    fn is_bound(&self, name: &str) -> bool {
        self.scope.iter().any(|f| f.contains(name))
    }

    /// The [`LoopBodyContext`] of the loop or fold body statement being walked:
    /// the innermost frame's statement, with whether that frame and the ones
    /// around it are folds.
    fn loop_body_context(&self) -> LoopBodyContext {
        let frames: Vec<&FrameSites> = self.frame_sites.iter().flatten().collect();
        let Some((innermost, outer)) = frames.split_last() else {
            return LoopBodyContext::default();
        };
        LoopBodyContext {
            innermost_fold: innermost.fold,
            outer_fold: outer.iter().any(|frame| frame.fold),
            in_actor: self.context == Actor,
            ..innermost.statement
        }
    }

    /// Whether `name` is bound in the innermost enclosing block (or, at
    /// method level, anywhere in the method).
    fn is_frame_local(&self, name: &str) -> bool {
        let start = self.block_frames.last().copied().unwrap_or(0);
        self.scope[start..].iter().any(|f| f.contains(name))
    }

    fn define(&mut self, name: &EcoString) {
        if !self.is_bound(name) {
            if let Some(frame) = self.scope.last_mut() {
                frame.insert(name.clone());
            }
        }
    }

    fn writes_of(&self, block: &Block) -> Vec<OuterLocalWrite> {
        outer_local_writes(block, &|name| self.is_bound(name))
    }

    /// The first write of `block` that makes it a state-carrying block when it
    /// escapes (ADR 0133): a captured local, else a `self.field` assignment of
    /// a value type, named `self.field`. An actor field or a class variable is
    /// not one (ADR 0137): the block writes the cell, and a write from another
    /// process raises `actor_state_unreachable` at run time.
    fn escape_write_of(&self, block: &Block) -> Option<OuterLocalWrite> {
        self.writes_of(block).into_iter().next().or_else(|| {
            // A module-level expression has no `self`: "self can only be used
            // inside a method body" is already the error there.
            if matches!(self.context, Repl | Actor | Class) {
                return None;
            }
            // Where "Cannot assign to slot" fires (a `Value` subclass, not a
            // `TestCase`; `rejects_slot_assignment`, the validator's own
            // predicate) that error already covers the write, with the right
            // fix. A `TestCase` or plain `Object` subclass has no such error,
            // so the write would be dropped silently.
            if self.context == ValueType
                && self
                    .receivers
                    .as_ref()
                    .is_some_and(|r| r.hierarchy.rejects_slot_assignment(&r.class_name))
            {
                return None;
            }
            outer_state_writes(block).into_iter().next()
        })
    }

    pub(super) fn body(&mut self, body: &[ExpressionStatement]) {
        self.last_statements.push(false);
        for (i, stmt) in body.iter().enumerate() {
            if let Some(last) = self.last_statements.last_mut() {
                *last = i + 1 == body.len();
            }
            if self.depth == 0 {
                // ADR 0131 Phase 2d (BT-3795): the REPL root's sites are its
                // own subset of the method-body rule.
                let sites = if self.context == Repl {
                    repl_operand_sites(&stmt.expression)
                } else {
                    sequenced_operand_sites(&stmt.expression)
                };
                self.operand_sites = sites.into_iter().map(Expression::span).collect();
            } else if let (Repl, Some(Some(frame))) = (self.context, self.frame_sites.last_mut()) {
                // BT-3823: a REPL arm's sites (codegen's
                // `with_repl_arm_producer_sites` reads the same rule).
                frame.sites = repl_arm_producer_sites(&stmt.expression)
                    .into_iter()
                    .map(Expression::span)
                    .collect();
            } else if let Some(Some(frame)) = self.frame_sites.last_mut() {
                frame.statement = LoopBodyContext::new(&stmt.expression, false, false, false);
                if frame.arm {
                    frame.statement = frame.statement.in_arm();
                }
                frame.sites = loop_body_producer_sites(&stmt.expression)
                    .into_iter()
                    .map(Expression::span)
                    .collect();
            }
            // ADR 0133: a block literal that is the value of a collection
            // operation's block (`collect: [:x | [t := t + x]]`) escapes into
            // the operation's result.
            if i + 1 == body.len() && self.containers.last() == Some(&Container::ListOpBody) {
                if let Expression::Block(block) = stmt.expression.unwrap_parens() {
                    self.report_literal(block, &Escape::CollectionResult);
                }
            }
            self.expr(&stmt.expression, Position::Statement);
        }
        self.last_statements.pop();
    }

    fn block(&mut self, block: &Block, container: Container) {
        if let Some(shape) = self.escapes.get(&block.span).copied() {
            self.report_escaping_literal(block, shape);
        }
        if self.value_operands.contains(&block.span) {
            if let Some(write) = self.escape_write_of(block) {
                self.value_block_writes.insert(block.span, write);
            }
        }
        let frame_body = self.pending_frame_body.take();
        self.frame_sites.push(frame_body.map(|body| FrameSites {
            fold: body.fold,
            arm: body.arm,
            plain: body.plain,
            ..FrameSites::default()
        }));
        self.block_in_frame(block, container);
        self.frame_sites.pop();
    }

    fn block_in_frame(&mut self, block: &Block, container: Container) {
        if self.depth == 0 {
            self.outer_stateful =
                self.context != Repl && touches_state(block, self.context == Class);
        }
        let direct = rebinds_outer_local(block, &|name| self.is_bound(name));
        self.direct_blocks.push(direct);
        self.block_frames.push(self.scope.len());
        self.containers.push(container);
        self.scope
            .push(block.parameters.iter().map(|p| p.name.clone()).collect());
        self.depth += 1;
        self.body(&block.body);
        self.depth -= 1;
        self.pop_frame();
        self.containers.pop();
        self.direct_blocks.pop();
        self.block_frames.pop();
    }

    /// Leaves the innermost scope frame.
    fn pop_frame(&mut self) {
        self.scope.pop();
    }

    /// Walks `expr` as an operand: a block literal is walked as a block (of
    /// role [`Container::Other`]), any other expression at `position`.
    fn operand(&mut self, expr: &Expression, position: Position) {
        self.operand_in(expr, position, Container::Other);
    }

    /// As [`Walker::operand`], walking a block literal as a block of role
    /// `container`.
    fn operand_in(&mut self, expr: &Expression, position: Position, container: Container) {
        match expr {
            Expression::Block(block) => self.block(block, container),
            other => self.expr(other, position),
        }
    }

    fn expr(&mut self, expr: &Expression, position: Position) {
        self.expr_inner(expr, position);
    }

    #[allow(clippy::too_many_lines)] // one arm per expression kind
    fn expr_inner(&mut self, expr: &Expression, position: Position) {
        let nested = self.depth > 0;
        match expr {
            Expression::Parenthesized { expression, .. } => {
                self.expr(expression, position);
            }
            Expression::Assignment { target, value, .. } => match target.as_ref() {
                Expression::Identifier(id) => {
                    self.operand(value, Position::AssignValue { nested });
                    self.define(&id.name);
                    self.note_block_binding(&id.name, value);
                }
                Expression::FieldAccess { receiver, .. } => {
                    self.expr(receiver, Position::Receiver);
                    self.operand(value, Position::FieldAssignValue { nested });
                }
                other => {
                    self.expr(other, Position::Operand);
                    self.operand(value, Position::Operand);
                }
            },
            Expression::DestructureAssignment { pattern, value, .. } => {
                self.operand(value, Position::DestructureValue);
                let (ids, _) = crate::semantic_analysis::extract_pattern_bindings(pattern);
                for id in ids {
                    self.define(&id.name);
                }
            }
            Expression::Return { value, .. } => {
                self.operand(value, Position::ReturnValue { nested });
            }
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                self.check_construct(expr, position);
                let lookup_producer = self.lookup_producers.contains(&expr.unwrap_parens().span());
                let dispatched = self.is_actor_self_send_fold(expr);
                self.check_send(
                    receiver,
                    selector,
                    arguments,
                    true,
                    lookup_producer,
                    dispatched,
                );
                // The role of each block literal the construct inlines (the
                // same recognizer as `check_construct`); any other block is a
                // block value.
                let construct = local_threading_construct(expr)
                    .filter(LocalThreadingConstruct::is_threaded_today);
                let role = |e: &Expression, is_receiver: bool| match (e, &construct) {
                    (Expression::Block(b), Some(c))
                        if c.blocks.iter().any(|blk| std::ptr::eq(*blk, b)) =>
                    {
                        Container::of(&c.selector, is_receiver)
                    }
                    _ => Container::Other,
                };
                let receiver_role = role(receiver, true);
                // ADR 0131 Phase 3 (BT-3738): the arms of a conditional that is
                // a local-threading producer are `BranchArm` frames, so a loop
                // or fold in an arm is a producer there (codegen's
                // `generate_conditional_arm`). Phase 3b (BT-3817): so are the
                // protected body (the receiver block), handler and cleanup of an
                // `on:do:`/`ensure:` producer.
                let arm_frames = self.is_arm_frame_conditional(expr, position);
                let plain_arm_frames = !arm_frames && self.is_plain_arm_conditional(expr);
                self.pending_frame_body = match receiver.as_ref() {
                    // The receiver block of an `on:do:`/`ensure:` (the protected
                    // body) or of an evaluated literal block (`[...] value`, the
                    // arm that always runs) is an arm frame.
                    Expression::Block(_)
                        if arm_frames
                            && (exception_arm_blocks(expr).is_some()
                                || literal_block_value_arm(expr).is_some()
                                || (self.context == Repl && is_repl_evaluated_block(expr))) =>
                    {
                        Some(FrameBody {
                            fold: false,
                            arm: true,
                            plain: false,
                        })
                    }
                    _ => None,
                };
                self.operand_in(receiver, Position::Receiver, receiver_role);
                self.pending_frame_body = None;
                for arg in arguments {
                    let arg_role = role(arg, false);
                    self.pending_frame_body = match arg {
                        Expression::Block(b) if is_threaded_frame_body(expr, b) => {
                            Some(FrameBody {
                                fold: is_fold_frame_body(expr, b),
                                arm: false,
                                plain: false,
                            })
                        }
                        Expression::Block(_) if arm_frames => Some(FrameBody {
                            fold: false,
                            arm: true,
                            plain: false,
                        }),
                        // The arms of a conditional that is not a producer are
                        // plain `BranchArm` frames in codegen (BT-3911), where a
                        // loop or fold is a producer (BT-3949).
                        Expression::Block(_) if plain_arm_frames => Some(FrameBody {
                            fold: false,
                            arm: true,
                            plain: true,
                        }),
                        _ => None,
                    };
                    self.operand_in(arg, Position::Argument, arg_role);
                    self.pending_frame_body = None;
                }
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // The parser folds the first message into `receiver`; the
                // later messages go to that send's own receiver.
                if let Expression::MessageSend {
                    receiver: shared,
                    selector,
                    arguments,
                    ..
                } = receiver.as_ref()
                {
                    // Codegen sends every cascade message through ordinary
                    // dispatch (`generate_cascade`) and inlines none of
                    // their blocks, so a block literal in any of them is a
                    // block value with no return channel (§6), whatever its
                    // selector.
                    // The shared receiver is evaluated once: check it once.
                    if let Expression::Block(block) = shared.unwrap_parens() {
                        self.report_literal(
                            block,
                            &Escape::CascadeReceiver(crate::ast::normalize::selector_as_written(
                                shared, selector, arguments,
                            )),
                        );
                    }
                    self.check_send(shared, selector, arguments, false, false, false);
                    for msg in messages {
                        self.check_send(shared, &msg.selector, &msg.arguments, false, false, false);
                    }
                    self.operand(shared, Position::Cascade);
                    for arg in arguments {
                        self.operand(arg, Position::Argument);
                    }
                } else {
                    self.operand(receiver, Position::Cascade);
                }
                for msg in messages {
                    for arg in &msg.arguments {
                        self.operand(arg, Position::Argument);
                    }
                }
            }
            Expression::Block(block) => self.block(block, Container::Other),
            Expression::FieldAccess { receiver, .. } => self.expr(receiver, Position::Receiver),
            Expression::Match { value, arms, .. } => {
                self.operand(value, Position::MatchSubject);
                // BT-3947 (closes BT-3875): a `match:` that is a local-threading
                // producer here has an arm that is a `BranchArm` frame, like a
                // conditional's block (codegen's `generate_match_producer`): its
                // writes are threaded back and a loop or fold in an arm is a
                // producer there.
                if self.is_match_producer(expr, position) {
                    for arm in arms {
                        let block = match_arm_block(arm);
                        self.pending_frame_body = Some(FrameBody {
                            fold: false,
                            arm: true,
                            plain: false,
                        });
                        if let Some(guard) = &arm.guard {
                            self.operand(guard, Position::MatchArm);
                        }
                        self.block(&block, Container::Arm);
                        self.pending_frame_body = None;
                    }
                    return;
                }
                for arm in arms {
                    let (ids, _) =
                        crate::semantic_analysis::extract_match_arm_bindings(&arm.pattern);
                    self.scope.push(ids.into_iter().map(|id| id.name).collect());
                    // BT-3913: a `match:` that is not a producer here keeps
                    // the arm error (a guard write is never threaded, and a
                    // `match:` at no producer site cannot return one either).
                    self.report_match_arm_writes(arm);
                    if let Some(guard) = &arm.guard {
                        self.operand(guard, Position::MatchArm);
                    }
                    self.operand(&arm.body, Position::MatchArm);
                    self.pop_frame();
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.operand(&pair.key, Position::LiteralElement);
                    self.operand(&pair.value, Position::LiteralElement);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for e in elements {
                    self.operand(e, Position::LiteralElement);
                }
                if let Some(t) = tail {
                    self.operand(t, Position::LiteralElement);
                }
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    self.operand(e, Position::LiteralElement);
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(e) = segment {
                        self.operand(e, Position::Interpolation);
                    }
                }
            }
            Expression::BinaryConstruction { segments, .. } => {
                for e in segments.iter().flat_map(BinarySegment::sub_expressions) {
                    self.operand(e, Position::LiteralElement);
                }
            }
            // A local bound to a writing block was reported where it was bound
            // (`note_block_binding`), so a later escape of the local is not.
            Expression::Identifier(_)
            | Expression::Literal(..)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. }
            | Expression::Error { .. } => {}
        }
    }

    /// `name := value` (ADR 0133): each block literal that makes up `value`
    /// ([`value_blocks`]: `b := [..]`, `b := c ifTrue: [[..]] ifFalse: [..]`)
    /// and assigns a captured local escapes by being stored in `name`, however
    /// `name` is used later. Its write is the one seen where the block was
    /// walked (`value_block_writes`), else as seen here (`b := [b := b + 1]`
    /// writes the `b` this assignment binds). A later use of `name` (`^b`,
    /// `self foo: b`) is not reported again.
    fn note_block_binding(&mut self, name: &EcoString, value: &Expression) {
        for block in value_blocks(value) {
            let write = self
                .value_block_writes
                .get(&block.span)
                .cloned()
                .or_else(|| self.escape_write_of(block));
            if let Some(write) = write {
                self.report_escape(block, &write, &Escape::Stored(name.clone()), None);
            }
        }
    }

    /// Whether `expr` is a conditional or an `on:do:`/`ensure:` that is a
    /// local-threading producer at the method root (an operand site, an
    /// assignment's whole value or a method-body statement), whose arms are
    /// therefore `BranchArm` frames (ADR 0131 §2, Phase 3 and 3b). The REPL
    /// keeps its own lowering.
    fn is_arm_frame_conditional(&self, expr: &Expression, position: Position) -> bool {
        if self.context == Repl {
            return self.is_repl_arm_construct(expr, position);
        }
        let Some(kind) = self.root_method_kind() else {
            return false;
        };
        let bound = |name: &str| self.is_bound(name);
        let producer = conditional_producer_writes(expr, &bound, kind)
            .or_else(|| exception_producer_writes(expr, &bound, kind));
        if self.depth == 0 {
            return (self.operand_sites.contains(&expr.span()) || position == Position::Statement)
                && producer.is_some();
        }
        // A conditional nested in an arm frame is a producer there, so its own
        // arms are frames too.
        producer.is_some() && self.arm_frame_conditional_producer(expr)
    }

    /// Whether `expr` is a conditional with block-literal arms that is not a
    /// local-threading producer here, so its arms are *plain* `BranchArm`
    /// frames in which a loop or fold, and a nested conditional, `on:do:` or
    /// `match:` writing an arm-local, is a producer (`generate_plain_arm`,
    /// BT-3911, BT-3949, BT-4460; `bt4460plain_arm_nested_grid_test.bt`). Restricted to what a runtime differential proves
    /// (`stdlib/test/bt4367arm_local_grid_test.bt`: the arm against the same
    /// body at the method root, over every selector in
    /// [`PLAIN_ARM_SELECTORS`] x five positions x five body shapes x actor and
    /// value-type methods, all equal):
    ///
    /// - actor and value-type methods only: a class method's plain arm does
    ///   not lower the loop (`Tuple does not understand 'plusFromNumber:'`);
    /// - a conditional at the method root only (`depth == 0`);
    /// - the [`LocalThreadingFamily::Conditional`] family only: a `Dictionary`
    ///   lookup handler (`at:ifAbsent:`) is a real closure, since only a
    ///   producer lookup is inlined, and crashes the same way in an actor;
    /// - the selectors in [`PLAIN_ARM_SELECTORS`]: `and:`/`or:` are not proven.
    ///
    /// The REPL keeps its own arm lowering.
    fn is_plain_arm_conditional(&self, expr: &Expression) -> bool {
        self.depth == 0
            && matches!(self.context, Actor | ValueType)
            && local_threading_construct(expr).is_some_and(|c| {
                c.family == LocalThreadingFamily::Conditional
                    && WellKnownSelector::from_name(&c.selector)
                        .is_some_and(|w| PLAIN_ARM_SELECTORS.contains(&w))
            })
            && conditional_arm_blocks(expr).is_some()
    }

    /// BT-3947: whether `expr`, a `match:`, is a local-threading producer at
    /// `position` (the `match:` counterpart of [`Self::is_arm_frame_conditional`]):
    /// at the method root (an operand site or a statement) or at a site of the
    /// frame it is nested in. The REPL keeps its own lowering.
    fn is_match_producer(&self, expr: &Expression, position: Position) -> bool {
        if self.context == Repl {
            return self.is_repl_arm_construct(expr, position);
        }
        let Some(kind) = self.root_method_kind() else {
            return false;
        };
        let bound = |name: &str| self.is_bound(name);
        if match_producer_writes(expr, &bound, kind).is_none() {
            return false;
        }
        if self.depth == 0 {
            return self.operand_sites.contains(&expr.span()) || position == Position::Statement;
        }
        self.arm_frame_conditional_producer(expr)
    }

    /// The [`RootMethodKind`] of the method being walked, `None` in the REPL.
    fn root_method_kind(&self) -> Option<RootMethodKind> {
        self.context.root_method_kind()
    }

    /// BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): whether `expr` is a
    /// REPL construct whose blocks are *REPL arms*: a conditional (not a
    /// lookup), an `on:do:`/`ensure:`/`Result tryDo:` or a `value`-family send
    /// to a block literal ([`is_repl_evaluated_block`]) that writes a binding,
    /// so
    /// codegen inlines its blocks over the bindings map
    /// (`generate_repl_arm_inline`, `with_repl_arm_producer_sites`), at a
    /// position whose lowering compiles it that way: a statement, an
    /// assignment's whole value or an operand site of the input
    /// ([`repl_operand_sites`]), or a site of a REPL arm it is nested in
    /// ([`repl_arm_producer_sites`]).
    fn is_repl_arm_construct(&self, expr: &Expression, position: Position) -> bool {
        let inner = expr.unwrap_parens();
        if lookup_arm_blocks(inner).is_some() {
            return false;
        }
        // BT-3947: a `match:` whose arm bodies write a binding (no guard does)
        // is a REPL construct too, one arm per `match:` arm.
        let match_writes = match_arms(inner).map(|arms| {
            match_guard_writes(arms, &|name| self.is_bound(name)).is_empty()
                && !match_arm_body_writes(arms, &|name| self.is_bound(name)).is_empty()
        });
        if conditional_arm_blocks(inner).is_none()
            && exception_arm_blocks(inner).is_none()
            && !is_repl_evaluated_block(inner)
            && match_writes.is_none()
        {
            return false;
        }
        let writes = match_writes.unwrap_or_else(|| {
            local_threading_construct(inner).is_some_and(|c| {
                !construct_outer_local_writes(&c, &|name| self.is_bound(name)).is_empty()
            })
        });
        if !writes {
            return false;
        }
        if self.depth == 0 {
            return matches!(
                position,
                Position::Statement | Position::AssignValue { nested: false }
            ) || self.operand_sites.contains(&inner.span());
        }
        self.at_repl_arm_site(inner)
    }

    /// Whether the innermost frame is a plain arm ([`FrameSites::plain`]).
    fn in_plain_arm(&self) -> bool {
        self.frame_sites
            .last()
            .and_then(Option::as_ref)
            .is_some_and(|frame| frame.plain)
    }

    /// BT-3823: whether every enclosing block is a REPL arm and `expr` is one
    /// of the current arm statement's [`repl_arm_producer_sites`].
    fn at_repl_arm_site(&self, expr: &Expression) -> bool {
        self.context == Repl
            && !self.frame_sites.is_empty()
            && self
                .frame_sites
                .iter()
                .all(|frame| frame.as_ref().is_some_and(|f| f.arm))
            && self
                .frame_sites
                .last()
                .and_then(Option::as_ref)
                .is_some_and(|frame| frame.sites.contains(&expr.unwrap_parens().span()))
    }

    /// Whether `expr`, a conditional or `on:do:`/`ensure:` at a site of the
    /// innermost frame, is a local-threading producer in that frame: a
    /// conditional ([`conditional_producer_writes`]) or an `on:do:`/`ensure:`
    /// ([`exception_producer_writes`]) in any threaded frame, a loop or fold
    /// body included (ADR 0131 Phase 3b and 3c, BT-3817/BT-3822), at one of the
    /// statement's sites.
    fn arm_frame_conditional_producer(&self, expr: &Expression) -> bool {
        // BT-4460: a plain arm registers every arm-frame site in codegen
        // (`generate_plain_arm`), as a producer's arm does, so a
        // conditional, `on:do:` or `match:` nested in one is a producer there
        // by this same rule, and its own arms are frames in which a nested
        // loop keeps its write (the BT-4367 drop came from the two sides
        // disagreeing about exactly this).
        let Some(kind) = self.root_method_kind() else {
            return false;
        };
        let bound = |name: &str| self.is_bound(name);
        self.frame_sites
            .last()
            .and_then(Option::as_ref)
            .is_some_and(|frame| frame.sites.contains(&expr.span()))
            && is_loop_body_producer_site(expr, &self.loop_body_context())
            && (conditional_producer_writes(expr, &bound, kind).is_some()
                || exception_producer_writes(expr, &bound, kind).is_some()
                || match_producer_writes(expr, &bound, kind).is_some())
    }

    /// The Phase 0 allow-set check for a construct at `position`.
    #[allow(clippy::too_many_lines)] // one rule per accepted position, each documented
    fn check_construct(&mut self, expr: &Expression, position: Position) {
        let Some(construct) =
            local_threading_construct(expr).filter(LocalThreadingConstruct::is_threaded_today)
        else {
            return;
        };
        let is_lookup = construct.family == LocalThreadingFamily::Lookup;
        let writes = construct_outer_local_writes(&construct, &|name| self.is_bound(name));
        // BT-3922: in a class method, a lookup handler that writes object
        // state (a class variable) and no outer local writes it in place
        // (ADR 0130), so the §6 check must not take it for an escaping block
        // (`lookup_field_write_test.bt` proves the write survives). In an
        // actor or value-type method codegen runs the handler as a closure and
        // the field write is dropped, so it stays an escape.
        let state_only_lookup = is_lookup
            && self.context == Class
            && writes.is_empty()
            && construct
                .blocks
                .iter()
                .any(|block| !outer_state_writes(block).is_empty());
        let selector = construct.selector;
        // BT-3841: a keyed iteration on a statically non-`Dictionary` receiver
        // keeps the permanent §6 error whether its block writes an outer local
        // or, in an actor instance method, object state (a field write or a
        // self-send): the receiver's own method would run the block as a closure,
        // which can return neither. Only an actor method inlines a state-only
        // keyed block (`needs_mutation_threading` threads the actor `State`);
        // a class or value-type method dispatches it normally, so there the
        // block is accepted unless it writes an outer local.
        if matches!(selector.as_str(), "keysAndValuesDo:" | "doWithKey:")
            && self.receiver_is_not_dictionary(expr)
        {
            if let Some((_, body)) = producer_body_block(expr) {
                let hint = format!(
                    "`{selector}` runs the block inline only for a `Dictionary` receiver; this \
                     receiver is not one, so it would call its own `{selector}` with a closure. \
                     Compute the new value without writing it from the block, or iterate a \
                     `Dictionary` (`asDictionary`)"
                );
                if let Some(write) = writes.first() {
                    let escape = Escape::Passed(EcoString::from(selector.as_str()));
                    self.report_escape(body, write, &escape, Some(hint));
                } else if self.context == Actor && touches_state(body, false) {
                    self.report_state_literal(body, &selector, hint);
                }
            }
            return;
        }
        let Some(first) = writes.first().cloned() else {
            if state_only_lookup && !self.receiver_is_not_dictionary(expr) {
                self.lookup_producers.insert(expr.unwrap_parens().span());
            }
            return;
        };
        // ADR 0131 Phase 2d (BT-3798 review): a lookup whose receiver is
        // statically known not to be a `Dictionary` would only ever raise at
        // run time (the guard), so it keeps its §6 compile error.
        if is_lookup && self.receiver_is_not_dictionary(expr) {
            return;
        }
        // ADR 0131 Phase 2 (BT-3749): a loop or fold at a sequenced operand
        // site of a method-body statement is a local-threading producer,
        // wherever it sits there. Phase 2d (BT-3795): and at an operand site
        // of a REPL input (`repl_operand_sites`), whose root frame threads the
        // bindings map.
        //
        // ADR 0131 Phase 3 (BT-3738): a conditional with a block-literal arm
        // that writes an outer local is one too (`root_producer_writes`), at an
        // operand site, as an assignment's whole value, and as a method-body
        // statement (the statement lowering of a class or value-type method
        // routes it through the producer as well). The REPL keeps its own
        // conditional lowering.
        let bound = |name: &str| self.is_bound(name);
        let root_producer = match self.context {
            // ADR 0131 Phase 2d (BT-3798): a lookup is a producer in a method
            // only; the REPL keeps the §6 rejection (codegen's
            // `conditional_producer_locals` agrees).
            // The REPL's constructs at an operand site (`repl_operand_sites`)
            // or a REPL arm site (`repl_arm_producer_sites`) are producers that
            // adopt the bindings map whole (BT-3823: `repl_producer_writes`,
            // the rule codegen's REPL producer reads, a `value` send to a block
            // literal included).
            Repl => repl_producer_writes(expr, &bound),
            Actor => root_producer_writes(expr, &bound, RootMethodKind::Actor),
            Class => root_producer_writes(expr, &bound, RootMethodKind::Class),
            ValueType => root_producer_writes(expr, &bound, RootMethodKind::ValueType),
        };
        let root_site = self.operand_sites.contains(&expr.span())
            || (position == Position::Statement
                && self.context != Repl
                && (conditional_arm_blocks(expr).is_some()
                    || exception_arm_blocks(expr).is_some()));
        if self.depth == 0 && root_site && root_producer.is_some() {
            if is_lookup {
                self.lookup_producers.insert(expr.unwrap_parens().span());
            }
            return;
        }
        // ADR 0131 Phase 2b (BT-3793): in the body of a loop or fold that
        // codegen lowers as a threaded frame — every enclosing block a
        // frame body, none an arm, handler or closure — a loop or fold at
        // one of the statement's `loop_body_producer_sites` is a
        // local-threading producer there: its prelude rebinds the locals it
        // writes, lowered by the enclosing frame's mode.
        //
        // `is_loop_body_producer_site` is the one narrowing of those sites,
        // shared with codegen's `with_body_producer_sites`: `detect:`/
        // `detect:ifNone:` are producers there in class and value-type methods
        // but stay out in an actor method (they keep their statement rows), and an
        // actor fold frame's value-bearing sites are not lowered by the
        // producer at all, so no other row may accept them either
        // (`r := xs inject: 0 into: [...]` in a `do:` body compiled and then
        // crashed, BT-3797).
        let frame_site = self.depth > 0
            && self.context != Repl
            && !self.frame_sites.is_empty()
            && self.frame_sites.iter().all(Option::is_some)
            && self
                .frame_sites
                .last()
                .and_then(Option::as_ref)
                .is_some_and(|frame| frame.sites.contains(&expr.span()))
            && (operand_producer_writes(expr, &|name| self.is_bound(name)).is_some()
                || self.arm_frame_conditional_producer(expr))
            // A plain arm lowers a loop or fold that is the statement or the
            // whole value of an assignment (BT-4367). One at an operand or
            // array-literal site of an actor `ifTrue:ifFalse:` arm compiles to
            // Core Erlang with an unbound arm-local, so it keeps the position
            // error.
            // `AssignValue { .. }` is any assignment in the arm (`nested` is
            // always true inside a block). That the loop is the *whole* value,
            // not an inner assignment of a chain (`a := s := xs inject: ...`),
            // comes from site registration, not from this `matches!`: codegen's
            // `loop_body_producer_sites` does not register the inner value, so
            // the shared recognizer rejects the chain (pinned in
            // `local_threading.rs`).
            && (!self.in_plain_arm()
                || matches!(
                    position,
                    Position::Statement | Position::AssignValue { .. }
                ));
        // BT-3823 (ADR 0131 §2, `MethodBody (REPL)` row): in a REPL arm — every
        // enclosing block an arm of a REPL construct inlined over the bindings
        // map (`is_repl_arm_construct`) — a construct at one of the statement's
        // `repl_arm_producer_sites` is a producer that adopts that map, as one
        // at an operand site of the input is.
        if self.depth > 0 && root_producer.is_some() && self.at_repl_arm_site(expr) {
            return;
        }
        let ctx = self.loop_body_context();
        let actor_value_site = frame_site && is_actor_fold_value_site(expr, &ctx);
        if frame_site && is_loop_body_producer_site(expr, &ctx) {
            if is_lookup {
                self.lookup_producers.insert(expr.unwrap_parens().span());
            }
            return;
        }
        // ADR 0131 Phase 2d (BT-3798): a lookup that is not a producer here
        // is not a local-threading construct at this position: the §6 check
        // reports its handler's write.
        if is_lookup {
            return;
        }
        let handler_writes = selector == "detect:ifNone:"
            && matches!(
                expr.unwrap_parens(),
                Expression::MessageSend { arguments, .. }
                    if matches!(arguments.get(1), Some(Expression::Block(h)) if !self.writes_of(h).is_empty())
            );
        let kind = ConstructKind::of(&selector, handler_writes);
        // BT-3813: a `^` of a `do:`-family loop in a block in a class or
        // value-type method is lowered by codegen's producer with the
        // Discard tail (`nlr_return_construct_value`): the `^` ends the
        // method, so the carrier is thrown, never leaked. An actor method
        // steps `State` through the throw instead, and the REPL threads its
        // own bindings map; both keep their own rules below.
        //
        // BT-3913: this and the next two rules are the sound shapes the
        // retired allow-set rows listed, stated structurally against the
        // lowering that threads them.
        if matches!(position, Position::ReturnValue { nested: true })
            && matches!(self.context, Class | ValueType)
            && matches!(kind, Do | KeyedDo)
        {
            return;
        }
        // An actor method's `^` of a conditional in a block: its `^` value
        // goes through `threaded_expression`, which steps the `State`
        // family the throw carries, so the carrier is never leaked. (A
        // `do:`-family loop there keeps the error: its fold frame does not
        // lower a value-bearing site, BT-3797.)
        if matches!(position, Position::ReturnValue { nested: true })
            && matches!(self.context, Actor)
            && matches!(kind, IfTrue | IfNil | AndOr)
            && !actor_value_site
        {
            return;
        }
        // The REPL assembler threads a loop or fold that is the whole value
        // of an assignment (it unpacks the `{Value, Bindings}` tuple and
        // adopts the bindings map, ADR 0041), and a destructured list-op
        // value (`destructuring.btscript`); a `Result tryDo:` there is one
        // when the core rule names it a producer (`repl_try_do_tier2_send`).
        // A lookup is none of these: it keeps the §6 rejection above.
        if self.context == Repl
            && self.depth == 0
            && (matches!(position, Position::AssignValue { nested: false })
                || matches!(
                    position,
                    Position::DestructureValue if matches!(kind, ListOp | DetectIfNoneSearch)
                ))
            && (kind != TryDo || root_producer.is_some())
        {
            return;
        }
        // An assignment inside a block whose construct only writes locals of
        // that same block is threaded in that block's own frame, exactly as
        // in a method body; only a write that crosses the block boundary
        // needs the enclosing construct to thread it on (ADR 0131 §1's
        // transitive closure, which is not there yet).
        let crosses_block = writes.iter().any(|w| !self.is_frame_local(&w.name));
        let position = match position {
            // Codegen lowers a nested statement with the construct that owns
            // the *outermost* enclosing block (the one directly under the
            // method body): an arm inside a `do:` body threads as the `do:`
            // body does, a `do:` inside an arm as the arm does (probed,
            // BT-3753).
            Position::Statement if self.depth > 0 => Position::NestedStatement {
                container: self.containers.first().copied().unwrap_or(Container::Other),
                crosses: crosses_block,
                last: self.last_statements.get(1).copied().unwrap_or(false),
                deep: self.depth > 1,
                stateful: self.outer_stateful,
                direct: self.direct_blocks.iter().any(|d| *d),
                inner: self.containers.last().copied().unwrap_or(Container::Other),
                depth: self.depth,
            },
            Position::AssignValue { nested: true } if !crosses_block => {
                Position::AssignValue { nested: false }
            }
            Position::FieldAssignValue { nested: true } if !crosses_block => {
                Position::FieldAssignValue { nested: false }
            }
            other => other,
        };
        // ADR 0133 Phase 3 close-out (BT-3913): every statement shape is
        // lowered — the 733-shape oracle reports zero silent drops and zero
        // `erlc` failures — so a statement is accepted wherever it sits. (A
        // statement in a `whileTrue:`/`whileFalse:` condition the loop does
        // not thread is still rejected by that check, like an escaping block
        // is by its own.)
        //
        // In an actor fold body a value-bearing site is not lowered by the
        // producer at all (BT-3797), so it keeps the error even as a
        // statement.
        //
        // ADR 0131 Phase 2c (BT-3794): a handler-writing `detect:ifNone:`
        // keeps its producer-shape gate — only a literal search block
        // (`producer_body_block`), the one rule codegen's router and fold
        // plan read, packs the handler's writes. A stored search block
        // routes to the fold but is not packed, so the write would be
        // dropped. The REPL threads its bindings map and is not shaped by
        // it.
        let stored_search =
            kind == DetectIfNone && self.context != Repl && producer_body_block(expr).is_none();
        if !actor_value_site
            && !stored_search
            && matches!(
                position,
                Position::Statement | Position::NestedStatement { .. }
            )
        {
            return;
        }
        // A canonicalised loop (`repeat`, a unary `whileTrue`) is named as
        // written (BT-4589).
        let written = construct.written_selector.clone();
        let hint = Self::construct_hint(&written, position, &first.name);
        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "`{written}` writes outer local `{name}`, but as {position} in {context} \
                     the write is not threaded back (BT-3913)",
                    name = first.name,
                    context = self.context,
                ),
                expr.span(),
            )
            .with_hint(hint)
            .with_note(
                format!("`{}` is written here", first.name),
                Some(first.span),
            )
            .with_note(
                format!(
                    "a {kind} threads its writes only where a producer frame lowers it: as a \
                     statement, or as the value of an assignment, a `^` value or an operand a \
                     frame sequences"
                ),
                None,
            )
            .with_category(DiagnosticCategory::UnthreadedLocalWrite),
        );
    }

    /// Whether the receiver of the lookup send `expr` is statically known not
    /// to be a `Dictionary` ([`receiver_statically_not_dictionary`]).
    fn receiver_is_not_dictionary(&self, expr: &Expression) -> bool {
        let (Some(facts), Expression::MessageSend { receiver, .. }) =
            (&self.receivers, expr.unwrap_parens())
        else {
            return false;
        };
        let ty = facts.type_map.and_then(|map| map.get(receiver.span()));
        receiver_statically_not_dictionary(receiver, ty, facts.self_is_dictionary, facts.hierarchy)
    }

    /// The help text for a construct the position check rejects at
    /// `position`: the nearest position that threads `name`.
    fn construct_hint(selector: &str, position: Position, name: &str) -> String {
        match position {
            Position::NestedStatement { container, .. } => format!(
                "evaluate the `{selector}` as a statement of the method body, not inside \
                 {container}, and read the locals it writes afterwards"
            ),
            Position::Statement => format!(
                "compute the new value of `{name}` without writing it from the block, and \
                 assign it in a statement of the method body (`{name} := coll inject: {name} \
                 into: [:acc :x | ...]`)"
            ),
            Position::AssignValue { .. } => format!(
                "evaluate the `{selector}` as a statement of its own, at the level of the \
                 method body, and read the locals it writes afterwards"
            ),
            _ => format!(
                "assign the `{selector}` result to a local in a statement of its own \
                 (`v := ...`) and use `v` here"
            ),
        }
    }

    /// Whether `expr` is a collection-message send (a `Fold`-family
    /// [`local_threading_construct`]: `do:`, `collect:`, `inject:into:`, ...)
    /// to `self` or `super` in an actor instance method or a class-side method
    /// (BT-3935: `self` is the class, whose own class-side `do:` runs in the
    /// class process and is reached only by a dispatched send). Codegen never lowers
    /// it in place: it is a dispatched self-send, so its block literal is
    /// passed to the actor's own method and escapes. Its writes were carried
    /// only by the callee's ADR 0128 opaque-callable fold, which ADR 0133
    /// removed (BT-3908).
    fn is_actor_self_send_fold(&self, expr: &Expression) -> bool {
        let Expression::MessageSend { receiver, .. } = expr.unwrap_parens() else {
            return false;
        };
        matches!(self.context, Actor | Class)
            && receiver.is_self_or_super()
            && local_threading_construct(expr)
                .is_some_and(|c| c.family == LocalThreadingFamily::Fold)
    }

    /// The ADR 0133 check for one send: its literal block arguments and
    /// receiver. `inlined` is false for a cascade message, which codegen never
    /// inlines; `dispatched` is true for an actor self-send of a collection
    /// message ([`Self::is_actor_self_send_fold`]), whose block escapes. A
    /// block passed to any other method escapes, an actor self-send to a
    /// `sealed` method of the same class included (ADR 0133 retires the
    /// BT-912/BT-3755 exemption).
    fn check_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        inlined: bool,
        lookup_producer: bool,
        dispatched: bool,
    ) {
        let sel = selector.name();
        if let Some(module) = crate::ffi_receiver::erlang_module_of_receiver(receiver) {
            // BT-3917: a block literal in an FFI argument position is handed to
            // Erlang, which calls it as a plain fun, so a write to a captured
            // local is dropped: it escapes (ADR 0133, ADR 0041).
            let escape = Escape::Ffi(EcoString::from(module), sel.clone());
            for arg in arguments {
                for block in value_blocks(arg) {
                    self.report_literal(block, &escape);
                }
            }
            return;
        }
        // EXEMPTION (§1 construct): a bare `[...]` argument of an inlined
        // send of a §1 selector (`local_threading_construct`).
        // (`tryDo:` is a §1 construct only sent to the `Result` class: any other
        // receiver takes the ordinary path.)
        // Not for an actor self-send of a collection message, which is
        // dispatched (`is_actor_self_send_fold`).
        let construct_send = inlined
            && !dispatched
            && (is_section1_selector(&sel)
                || crate::semantic_analysis::block_facts::is_result_try_do(receiver, &sel));
        // BT-4430: an `on:do:`/`ensure:` whose protected body or handler is not
        // a block literal is not inlined: its literal block is a closure.
        let exception_not_inlined = construct_send
            && crate::state_threading_selectors::is_exception_selector(&sel)
            && !crate::semantic_analysis::block_facts::exception_send_inlined(
                &sel, receiver, arguments,
            );
        let construct_send = construct_send && !exception_not_inlined;
        // EXEMPTION (lookup producer, BT-3798): the handler blocks of a lookup
        // accepted as a producer at this position (`check_construct`); a
        // block in any other argument position is a key that is a block value.
        let handler_indices: &[usize] = if lookup_producer {
            crate::state_threading_selectors::lookup_block_arg_indices(&sel)
        } else {
            &[]
        };

        // A block literal receiver. EXEMPTION: a bare `[...]` receiver of an
        // inlined `value`/loop/protected-body send (codegen's
        // `inline_block_captured_mutations` and the loop and exception
        // generators match only a bare block). A cascade checks its shared
        // receiver once itself, so `inlined: false` skips it here.
        if inlined {
            match receiver {
                // BT-4062: a `whileTrue:`/`whileFalse:` whose body is not a
                // block literal takes codegen's simple lowering in every
                // context (the REPL included), whose condition is a plain
                // closure: its write is lost (the loop never ends) or, as a
                // Tier 2 block, applied with the wrong arity.
                Expression::Block(block)
                    if crate::semantic_analysis::block_facts::while_condition_threads(
                        &sel, arguments,
                    ) == Some(false) =>
                {
                    self.report_literal(block, &Escape::ConditionBodyNotLiteral(sel.clone()));
                }
                // BT-4602: with a literal body a `whileTrue:`/`whileFalse:`
                // CONDITION block threads its outer-local writes in every
                // context, the unary `[...] whileTrue` (canonicalised to
                // `whileTrue: []`, BT-4313) included: codegen carries them in
                // the loop's `StateAcc` (BT-4015,
                // `while_condition_has_carried_effects`), and the verifier's
                // `ConditionFamilyNotCarried` fails a loop that drops one
                // (`while_condition_binding_corpus.rs`).
                // BT-3884: a block literal sent a `value`-family selector of
                // another arity (`[t := t + 1] value: 1`) is applied, never
                // inlined ([`block_literal_of_other_arity`]): the send raises
                // `arity_mismatch` and the body never runs, so the block
                // neither threads its writes nor escapes with them. A
                // parenthesized literal is the same literal (BT-3942).
                _ if block_literal_of_other_arity(receiver, selector, arguments.len())
                    .is_some() => {}
                Expression::Block(block) if !receiver_has_channel(&sel) => {
                    self.report_literal(block, &Escape::Receiver(sel.clone()));
                }
                Expression::Block(block) if exception_not_inlined => {
                    self.report_literal(block, &Escape::ExceptionNotInlined(sel.clone()));
                }
                Expression::Block(_) => {}
                // Any other receiver that evaluates to a block literal, a
                // parenthesized one or one answered by a conditional arm, an
                // evaluated block or a protected body ([`value_blocks`]), is a
                // block value sent `sel`.
                _ => {
                    for block in value_blocks(receiver) {
                        self.report_literal(block, &Escape::Receiver(sel.clone()));
                    }
                }
            }
        }
        // Block arguments. Only a bare `[...]` argument of a construct send (or a
        // lookup producer's handler) is lowered in place; any other argument that
        // evaluates to a block literal ([`value_blocks`]: the literal itself,
        // parenthesized, or answered by a conditional arm, an evaluated block or
        // a protected body) is a block value passed to `sel`, in every send.
        let escape = if exception_not_inlined {
            Escape::ExceptionNotInlined(sel.clone())
        } else if dispatched {
            let word = if matches!(receiver.unwrap_parens(), Expression::Super(_)) {
                "super"
            } else {
                "self"
            };
            let owner = if self.context == Class {
                "class's"
            } else {
                "actor's"
            };
            Escape::ActorSelfSend(word, owner, sel.clone())
        } else if inlined {
            self.passed_escape(receiver, &sel)
        } else {
            // A canonicalised loop is named as written (BT-4589).
            Escape::Cascade(crate::ast::normalize::selector_as_written(
                receiver, selector, arguments,
            ))
        };
        for (index, arg) in arguments.iter().enumerate() {
            if matches!(arg, Expression::Block(_))
                && (construct_send || handler_indices.contains(&index))
            {
                continue;
            }
            for block in value_blocks(arg) {
                self.report_literal(block, &escape);
            }
        }
    }

    /// How a block argument of an ordinary send of `selector` to `receiver`
    /// escapes: to a class method that runs in the class's process (a class
    /// reference receiver, or `self` in a class method, unless the method runs
    /// in the caller, ADR 0142: `ClassHierarchy::class_reference_send_runs_in_caller`
    /// for a class reference, which a shadowed name never does (BT-4283), and
    /// `ClassHierarchy::runs_class_method_in_caller` for `self`, the running
    /// class itself), or to any other method.
    fn passed_escape(&self, receiver: &Expression, selector: &EcoString) -> Escape {
        let receivers = self.receivers.as_ref();
        let runs_in_caller = match receiver {
            Expression::ClassReference { name, .. } => Some(receivers.is_some_and(|r| {
                r.hierarchy
                    .class_reference_send_runs_in_caller(&name.name, selector)
            })),
            Expression::Identifier(id) if id.name == "self" && self.context == Class => receivers
                .map(|r| {
                    r.hierarchy
                        .runs_class_method_in_caller(&r.class_name, selector)
                }),
            _ => None,
        };
        match runs_in_caller {
            Some(false) => Escape::ClassMethod(selector.clone()),
            _ if crate::state_threading_selectors::is_dictionary_inline_lookup(selector) => {
                Escape::Lookup(selector.clone())
            }
            _ => Escape::Passed(selector.clone()),
        }
    }

    /// A `match:` arm that assigns an outer local, in a `match:` that is not a
    /// local-threading producer where it sits ([`Walker::is_match_producer`]:
    /// BT-3947 lowers one that is, each arm a `BranchArm` frame). No construct
    /// threads such a write back to the method (BT-3817): compiling it would
    /// drop the write silently. This is a permanent error (BT-3913 retired the
    /// residual row with the rest of the position check): a guard write is
    /// never threaded, and neither is a body write of a `match:` at no
    /// producer site. Pinned in `adr0131_local_rebind.btscript`. Walks the whole
    /// arm (guard and body) without entering a nested block literal, whose
    /// writes are the constructs' own business; the arm's pattern variables
    /// (the innermost scope frame, already pushed) are the arm's own.
    fn report_match_arm_writes(&mut self, arm: &MatchArm) {
        let Some((own, outer)) = self.scope.split_last() else {
            return;
        };
        let roots: Vec<&Expression> = arm.guard.iter().chain([&arm.body]).collect();
        let mut block_spans: Vec<Span> = Vec::new();
        for root in &roots {
            crate::ast_walker::walk_expression(root, &mut |e| {
                match e {
                    Expression::Block(b) => block_spans.push(b.span),
                    // A nested `match:` is judged by its own arms (its pattern
                    // variables may shadow an outer local); only its subject
                    // belongs to this arm.
                    Expression::Match { arms, .. } => {
                        for nested in arms {
                            block_spans.push(nested.body.span());
                            if let Some(guard) = &nested.guard {
                                block_spans.push(guard.span());
                            }
                        }
                    }
                    _ => {}
                }
            });
        }
        let mut found: Vec<(EcoString, Span)> = Vec::new();
        for root in &roots {
            crate::ast_walker::walk_expression(root, &mut |e| {
                let Expression::Assignment { target, .. } = e else {
                    return;
                };
                let Expression::Identifier(id) = target.as_ref() else {
                    return;
                };
                let span = e.span();
                let in_block = block_spans
                    .iter()
                    .any(|b| b.start() <= span.start() && span.end() <= b.end());
                if !in_block
                    && !own.contains(&id.name)
                    && outer.iter().any(|f| f.contains(&id.name))
                {
                    found.push((id.name.clone(), span));
                }
            });
        }
        for (name, span) in found {
            self.diagnostics.push(
                Diagnostic::error(
                    format!(
                        "a `match:` arm writes outer local `{name}`, but `match:` cannot return the write"
                    ),
                    span,
                )
                .with_hint(format!(
                    "bind the arm's value instead: `{name} := subject match: [pattern -> newValue; ...]`, \
                     or use `ifTrue:`/`ifNil:` for a branch that writes `{name}` (BT-3875)"
                ))
                .with_category(DiagnosticCategory::UnthreadedLocalWrite),
            );
        }
    }

    /// Reports `block` as escaping (`escape`) if it assigns a captured local.
    fn report_literal(&mut self, block: &Block, escape: &Escape) {
        let Some(write) = self.escape_write_of(block) else {
            return;
        };
        self.report_escape(block, &write, escape, None);
    }

    /// The ADR 0133 diagnostic: `write` (the block's first assignment to a
    /// captured local) is reported at the assignment, with the shape, the fix
    /// (`hint`, else the shape's own) and the note listing the constructs that
    /// do thread assignments. One diagnostic per block literal.
    fn report_escape(
        &mut self,
        block: &Block,
        write: &OuterLocalWrite,
        escape: &Escape,
        hint: Option<String>,
    ) {
        if !self.reported_blocks.insert(block.span) {
            return;
        }
        // A `self.field` write (BT-3922) is named as written and has its own
        // fix: the field is object state, which a block that has left this
        // scope cannot hand back.
        let name: &EcoString = &match write.kind {
            WriteKind::Local => write.name.clone(),
            WriteKind::ObjectState => format!("self.{}", write.name).into(),
        };
        let hint = hint.or_else(|| {
            (write.kind == WriteKind::ObjectState).then(|| {
                format!(
                    "`{name}` is object state: once the block has left this scope no later call \
                     of it can hand the write back. Return the new value from the block and \
                     assign it where the block is called (`{name} := f value`), or move the \
                     write into a method that runs it directly"
                )
            })
        });
        let diagnostic = Diagnostic::error(
            format!(
                "`{name}` is assigned in a block {}",
                escape.fragment(self.method_selector.as_deref())
            ),
            write.span,
        )
        .with_hint(hint.unwrap_or_else(|| escape.help(name)))
        .with_note("the block", Some(block.span))
        .with_note(threading_constructs_note(), None)
        .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel);
        // One diagnostic per assignment: a write inside nested escaping blocks
        // (`f := [:d | d at: k ifAbsent: [t := t + 1]]`) is reported for the
        // outermost one, which is the escape to fix.
        self.push_escaping(write.span, block.span, diagnostic);
    }

    /// Records the escaping-block `diagnostic` found at `at` for `block`,
    /// keeping the outermost block's when nested blocks report the same site.
    fn push_escaping(&mut self, at: Span, block: Span, diagnostic: Diagnostic) {
        let within =
            |outer: Span, inner: Span| outer.start() <= inner.start() && inner.end() <= outer.end();
        match self
            .escaping_writes
            .iter_mut()
            .find(|(found, ..)| *found == at)
        {
            Some(entry) if within(block, entry.1) => {
                *entry = (at, block, diagnostic);
            }
            Some(_) => {}
            None => self.escaping_writes.push((at, block, diagnostic)),
        }
    }

    /// Hands the escaping-block diagnostics found by the walk
    /// ([`Walker::report_escape`]) to the caller.
    pub(super) fn finish(self) {
        self.diagnostics
            .extend(self.escaping_writes.into_iter().map(|(.., d)| d));
    }

    /// A keyed iteration's block that writes object state (a field or a class
    /// variable) rather than a captured local, on a receiver that would run it
    /// as a closure (BT-3841).
    fn report_state_literal(&mut self, block: &Block, selector: &str, hint: String) {
        self.diagnostics.push(
            Diagnostic::error(
                format!("block writes object state, but `{selector}` cannot return the write"),
                block.span,
            )
            .with_hint(hint)
            .with_category(DiagnosticCategory::Tier2BlockNoReturnChannel),
        );
    }

    /// ADR 0131 §6 (BT-3756): a block literal that assigns a captured local
    /// and escapes the method without a send (returned, stored in a field or
    /// class variable, or placed in a literal). A block stored in a local is
    /// reported by [`Walker::note_block_binding`].
    fn report_escaping_literal(&mut self, block: &Block, shape: EscapeShape) {
        // A block literal assigned straight to a field of an instance is
        // already a compile error from the block analyzer (BT-2809, "cannot
        // mutate captured variable ... inside a closure stored in a field"),
        // which reports it once; `section6_field_stored_literal_is_an_error_in_every_context`
        // pins that some error survives. A class variable store has no such
        // error, so the class-method context reports it here.
        // A block that is (part of) the value of a local assignment is
        // reported as stored in that local, whatever else it does.
        if self.value_operands.contains(&block.span) {
            return;
        }
        let escape = match shape {
            EscapeShape::StoredLocal => return,
            // A literal that assigns a captured local is the block analyzer's
            // (BT-2809); one that only assigns `self.field` (BT-3922) is
            // reported here.
            EscapeShape::StoredClassVar
                if self.context != Class && !self.writes_of(block).is_empty() =>
            {
                return;
            }
            EscapeShape::StoredClassVar => Escape::StoredState,
            EscapeShape::Returned => Escape::Returned,
            EscapeShape::StoredInLiteral => Escape::InLiteral,
        };
        self.report_literal(block, &escape);
    }
}

/// The [`escape_sites`] of a method body (or REPL expression list, BT-3858:
/// the value of its last expression is the input's value) that ADR 0133
/// inspects (BT-3756).
///
/// An operand listed with several shapes (`^(b := [..])` is returned and
/// stored in a local, BT-3806) keeps the one that leaves the method.
fn escape_map(body: &[ExpressionStatement]) -> HashMap<Span, EscapeShape> {
    let mut escapes = HashMap::new();
    for (span, shape) in escape_sites(body) {
        let entry = escapes.entry(span).or_insert(shape);
        if *entry == EscapeShape::StoredLocal {
            *entry = shape;
        }
    }
    escapes
}

/// Whether a statement of `block` (not of a nested block) is an assignment
/// `x := ...` to a local bound outside `block` (`is_outer`) whose value reads
/// `x`.
fn rebinds_outer_local(block: &Block, is_outer: &dyn Fn(&str) -> bool) -> bool {
    let params: HashSet<&str> = block.parameters.iter().map(|p| p.name.as_str()).collect();
    block.body.iter().any(|stmt| {
        let Expression::Assignment { target, value, .. } = &stmt.expression else {
            return false;
        };
        let Expression::Identifier(id) = target.as_ref() else {
            return false;
        };
        if params.contains(id.name.as_str()) || !is_outer(&id.name) {
            return false;
        }
        let mut reads = false;
        crate::ast_walker::walk_expression(value, &mut |e| {
            if matches!(e, Expression::Identifier(r) if r.name == id.name) {
                reads = true;
            }
        });
        reads
    })
}

/// Whether `block`'s statements touch the method's state: a `self` send or a
/// `self.x := ...` write, through the blocks codegen inlines. This is the
/// shared [`crate::semantic_analysis::facts::StateEffects`] fact codegen's
/// state threading reads.
fn touches_state(block: &Block, in_class_method: bool) -> bool {
    block.body.iter().any(|stmt| {
        crate::semantic_analysis::facts::compute_state_effects_for_expr(
            &stmt.expression,
            in_class_method,
        )
        .any()
    })
}

/// ADR 0131 §6: a §1 construct selector, whose literal block arguments are
/// inlined (so their outer-local writes have a way back): a state-threading
/// selector, the conditional family or `on:do:`/`ensure:`. (`tryDo:` is a §1
/// construct only when sent to the `Result` class, `is_result_try_do`, which
/// the call site checks with the receiver; ADR 0131 §5.)
fn is_section1_selector(selector: &str) -> bool {
    use crate::state_threading_selectors::{
        is_conditional_selector, is_exception_selector, is_state_threading_keyword_selector,
    };
    is_state_threading_keyword_selector(selector)
        || is_conditional_selector(selector)
        || is_exception_selector(selector)
}

/// Whether a block literal *receiver* of `selector` is inlined: a block
/// `value` send, a loop's condition, or a protected body. A keyword
/// `whileTrue:`/`whileFalse:` condition is narrowed first to a loop whose body
/// is a block literal (BT-4062, `Escape::ConditionBodyNotLiteral`); with one,
/// its writes thread in every context (BT-4602). A unary `[...] whileTrue` or
/// `[...] repeat` is that keyword loop once normalised; a unary loop sent to a
/// literal block with parameters stays an ordinary send (BT-4603).
fn receiver_has_channel(selector: &str) -> bool {
    crate::state_threading_selectors::is_state_threaded_block_receiver(selector)
        || matches!(selector, "whileTrue:" | "whileFalse:")
}
