// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Statement-kind lowerers shared by the two `BodyExprKind` statement walkers:
//! the actor-method body (`gen_server/methods.rs`, tail `{'reply', V, State}`)
//! and the conditional / `match:` / handler arm (`control_flow/conditionals.rs`,
//! tail: the arm's result value).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A kind lowers here once, parameterised by [`StmtTail`] (what a last
//! statement's value becomes) and the [`StmtCx::frame`] it is lowered in (the
//! method root, or the arm's branch frame). The kinds the two walkers lower
//! differently, and why, are listed in the comments on the walkers' `match`
//! arms.

use super::control_flow::LocalWrite;
use super::threaded_ir::{ThreadedStmt, ValueRef};
use super::{CoreErlangGenerator, Result};
use crate::core_erlang::threaded_ir::FrameId;
use beamtalk_cerl_doc::{docvec, leaf};
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

/// What the value of a body's LAST statement becomes.
pub(super) enum StmtTail<'a> {
    /// An actor-method body: the value is answered as `{'reply', V, State}`.
    Reply,
    /// A conditional arm: the value is recorded as the arm's result, closed
    /// later by the arm's `Return` node.
    Arm {
        last_result: &'a mut Option<ValueRef>,
    },
}

/// The statement a shared lowerer is lowering, and where.
#[derive(Clone, Copy)]
pub(super) struct StmtCx<'a> {
    pub expr: &'a Expression,
    pub span: Span,
    pub is_last: bool,
    /// The method root for a [`StmtTail::Reply`] body, the arm's branch frame
    /// for a [`StmtTail::Arm`] one.
    pub frame: FrameId,
}

impl<'a> StmtCx<'a> {
    /// A statement of an actor-method body, lowered in the method root frame.
    pub(super) fn root(expr: &'a Expression, span: Span, is_last: bool) -> Self {
        Self {
            expr,
            span,
            is_last,
            frame: FrameId::ROOT,
        }
    }
}

impl CoreErlangGenerator {
    /// Deliver `value`, an already-lowered pure value, as the last value of
    /// the body: a reply for a method, a temp-bound result for an arm.
    /// `arm_temp` is the temp-name prefix the arm form binds it to.
    fn deliver_last_value(
        &mut self,
        tail: &mut StmtTail<'_>,
        value: &ValueRef,
        arm_temp: &str,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        match tail {
            StmtTail::Reply => {
                let reply = self.threaded_value_reply_doc(value);
                stmts.push(ThreadedStmt::Statement(reply, span));
            }
            StmtTail::Arm { last_result } => {
                let result_var = self.fresh_temp_var(arm_temp);
                let value_doc = self.threaded_value_doc(value);
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        value_doc,
                        " in "
                    ],
                    span,
                ));
                **last_result = Some(ValueRef::Var(result_var));
            }
        }
    }

    /// `BodyExprKind::DispatchingSelfSend`: `self log: (self nextId)`.
    ///
    /// ADR 0118 phase 1a/2a: `threaded_expression`'s producer path
    /// (`is_prelude_producer`, matching `is_dispatching_actor_self_send`
    /// exactly) sequences the arguments' own self-sends ahead of this dispatch
    /// and builds the `Statement` + `Bind` pair; the last value is the
    /// dispatch's pure result.
    pub(super) fn lower_stmt_dispatching_self_send(
        &mut self,
        cx: StmtCx<'_>,
        tail: &mut StmtTail<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let tv = self.threaded_expression(cx.expr, cx.frame)?;
        stmts.extend(tv.prelude);
        if cx.is_last {
            self.deliver_last_value(tail, &tv.value, "SDResultVal", cx.span, stmts);
        }
        Ok(())
    }

    /// Deliver the variable `var`, bound by the statement itself (a local
    /// assignment's or destructuring's right-hand side), as the last value of
    /// the body: `{'reply', Var, State}` for a method, the arm's result for an
    /// arm.
    fn deliver_last_var(
        &mut self,
        tail: &mut StmtTail<'_>,
        var: String,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        match tail {
            StmtTail::Reply => {
                let reply = self.destructure_reply_doc(Some(var))?;
                stmts.push(ThreadedStmt::Statement(reply, span));
            }
            StmtTail::Arm { last_result } => **last_result = Some(ValueRef::Var(var)),
        }
        Ok(())
    }

    /// `BodyExprKind::DestructureAssignment`: `{a, b} := expr`.
    ///
    /// `local_write` is how the body writes the names it binds: the method
    /// root binds them with the extraction's `let`s alone
    /// ([`LocalWrite::Plain`]); an arm also writes each to its frame's slot
    /// ([`LocalWrite::Slot`], BT-4439). A destructuring whose right-hand side
    /// is itself a field write (`{a, b} := (self.x := ...)`, BT-3495) lowers,
    /// in an arm, the write as its own real `Bind`, then destructures the
    /// assigned value; a method body leaves it to `thread_ahead`.
    pub(super) fn lower_stmt_destructure_assignment(
        &mut self,
        cx: StmtCx<'_>,
        local_write: LocalWrite,
        tail: &mut StmtTail<'_>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
        } = cx;
        let Expression::DestructureAssignment { pattern, value, .. } = expr else {
            return Ok(());
        };
        let write_of = |_: &str| local_write;
        // Only an arm lowers the field write as its own `Bind` here. A method
        // body already sequences it: `thread_ahead` threads the assignment
        // as a producer and the extraction reads the sequenced value
        // (`Bt4919DestructureFieldWriteTest` pins both bodies).
        let field_write = match tail {
            StmtTail::Arm { .. } => self.local_assign_field_write(value),
            StmtTail::Reply => None,
        };
        let rhs_var = if let Some(field_write) = field_write {
            let field_val_var =
                self.lower_field_assignment_bind(field_write, frame, span, stmts)?;
            let _ = self.lower_destructure_bind_from_var(
                pattern,
                &field_val_var,
                frame,
                span,
                stmts,
                &write_of,
            )?;
            field_val_var
        } else {
            self.lower_destructure_bind(pattern, value, frame, span, stmts, &write_of)?
                .0
        };
        if is_last {
            self.deliver_last_var(tail, rhs_var, span, stmts)?;
        }
        Ok(())
    }
}
