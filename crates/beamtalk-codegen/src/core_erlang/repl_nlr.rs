// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4288 / BT-4312: the REPL eval's non-local-return boundary.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A REPL input is a method body: a `^` run in place (`beamtalk-core`'s
//! `block_facts::InPlaceReturns::in_place`: at the root, in a conditional
//! arm, an evaluated or protected block, a loop or fold body) ends the input
//! with its value, and the rest of the input does not run. Such a `^` is
//! compiled inside a construct's `fun` or before the input's later
//! statements, so it can end the input only as a throw to its boundary
//! (ADR 0041). A method mints its boundary whenever a block holds a `^`. A
//! REPL input mints its own for exactly these returns
//! (`block_facts::repl_eval_returns`): the eval body is wrapped in a
//! [`ThreadedStmt::NlrCatch`] with [`NlrBoundary::Repl`], whose catch arm
//! answers the eval's `{Value, Bindings}` from the thrown value and the
//! workspace bindings map at the `^`. So `#(1, 2) do: [:x | ^x]` and
//! `x := 1. ^7. x := 2` answer 1 and 7, as in a method, and the bindings
//! written before the `^` keep their values.
//!
//! A `^` that is the input's whole last statement already ends it where it
//! stands, so it keeps its bare-value lowering. A `^` held in a closure
//! (`b := [:x | ^x]`) is never one of the eval's: the closure may run in a
//! later input, after this eval has ended, where no catch is left. It answers
//! its value where it stands, and the REPL check rejects one in a loop body,
//! which the loop would ignore (`InPlaceReturns::in_closure`). A REPL `^` of
//! the eval's that finds no boundary is
//! [`VerifyError::ReplReturnWithoutBoundary`], never silent.

use super::NlrBoundary;
use super::generator::CoreErlangGenerator;
use super::threaded_ir::{self, FrameId, ThreadedStmt, TokenId, VerifyError};
use beamtalk_cerl_doc::Document;
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::block_facts::repl_eval_returns;
use beamtalk_core::source_analysis::Span;
use std::collections::HashSet;

/// The boundary of the REPL input being compiled (on `ReplContext`).
#[derive(Debug, Clone, Default)]
pub(crate) struct ReplEvalBoundary {
    /// The eval's NLR token variable: `Some` when the input holds a `^` that
    /// throws to it.
    token: Option<String>,
    /// The spans of the input's `^`s that throw to it
    /// (`block_facts::repl_eval_returns`).
    returns: HashSet<Span>,
}

impl CoreErlangGenerator {
    /// Opens the eval boundary of the REPL input `expressions`: records the
    /// `^`s that throw to it and, when there are any, mints their NLR token,
    /// before any of the input is compiled (the method mint order).
    /// Returns the token, which [`Self::wrap_repl_eval_body`] wraps the eval
    /// body with. Closed by [`Self::exit_repl_eval_boundary`].
    pub fn enter_repl_eval_boundary(&mut self, expressions: &[Expression]) -> Option<String> {
        let returns: HashSet<Span> = repl_eval_returns(expressions).into_iter().collect();
        let token = (!returns.is_empty() && !fault!(ReplBoundaryOff))
            .then(|| self.fresh_temp_var("NlrToken"));
        self.repl_context_mut().eval_boundary = ReplEvalBoundary {
            token: token.clone(),
            returns,
        };
        token
    }

    /// Closes the boundary [`Self::enter_repl_eval_boundary`] opened.
    pub fn exit_repl_eval_boundary(&mut self) {
        self.repl_context_mut().eval_boundary = ReplEvalBoundary::default();
    }

    /// Wraps the REPL eval body `body` (everything after `let State =
    /// Bindings in`, ending in the eval's own result) in the boundary
    /// `token` names: a [`ThreadedStmt::NlrCatch`] with
    /// [`NlrBoundary::Repl`], verified and rendered like a method's. In trace
    /// mode `trace_source` is the input's source text, the one step the catch
    /// arm answers.
    pub fn wrap_repl_eval_body(
        &mut self,
        body: Document<'static>,
        token: &str,
        trace_source: Option<&str>,
    ) -> Document<'static> {
        let stmts = [
            ThreadedStmt::NlrCatch {
                boundary: NlrBoundary::Repl {
                    trace_source: trace_source.map(str::to_string),
                },
                token: TokenId::new(token),
                frame: FrameId::ROOT,
                span: Span::default(),
            },
            ThreadedStmt::Statement(body, Span::default()),
        ];
        let errors = self.verify_threaded_ir(&stmts);
        self.report_threaded_ir_verify_errors(
            &errors,
            "REPL eval NLR boundary must be well-formed",
            Span::default(),
        );
        let mut ctx = threaded_ir::RenderCtx::new(self);
        threaded_ir::render(&stmts, &mut ctx)
    }

    /// The eval's NLR token, when the input holds a `^` that throws to it.
    pub(in crate::core_erlang) fn repl_eval_token(&self) -> Option<String> {
        if !self.is_repl_mode() {
            return None;
        }
        self.repl_context.as_ref()?.eval_boundary.token.clone()
    }

    /// The token the REPL `^` at `span` throws to: the eval's, when `span` is
    /// one of the `^`s that throw to it. `None` for every other `^`, which
    /// answers its value where it stands. Such a `^` with no token is
    /// reported as [`VerifyError::ReplReturnWithoutBoundary`] (and lowered to
    /// its value, the pre-BT-4288 shape).
    pub(in crate::core_erlang) fn repl_return_token(&mut self, span: Span) -> Option<String> {
        if !self.is_repl_mode() {
            return None;
        }
        let boundary = &self.repl_context.as_ref()?.eval_boundary;
        if !boundary.returns.contains(&span) {
            return None;
        }
        if let Some(token) = boundary.token.clone() {
            return Some(token);
        }
        self.report_threaded_ir_verify_errors(
            &[VerifyError::ReplReturnWithoutBoundary { at: span }],
            "a REPL `^` run in place must throw to the eval's NLR boundary",
            span,
        );
        None
    }
}
