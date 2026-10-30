// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4288: the REPL eval's non-local-return boundary.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A `^` in the body of a loop or fold run in place (`beamtalk-core`'s
//! `block_facts::loop_body_returns`) is compiled inside the loop's `fun`, so it
//! can end the enclosing scope only as a throw to that scope's boundary
//! (ADR 0041). A method mints its boundary whenever a block holds a `^`. A
//! REPL input mints its own for exactly these returns: the eval body is
//! wrapped in a [`ThreadedStmt::NlrCatch`] with [`NlrBoundary::Repl`], whose
//! catch arm answers the eval's `{Value, Bindings}` from the thrown value and
//! the workspace bindings map at the `^`. So `#(1, 2) do: [:x | ^x]` answers 1,
//! as in a method, and the bindings written before the `^` keep their values.
//!
//! Every other REPL `^` (at the root, in a conditional arm, an evaluated or
//! protected block outside a loop, a stored closure) keeps its lowering: it
//! answers its value where it stands. A REPL loop-body `^` that finds no
//! boundary is [`VerifyError::LoopReturnWithoutBoundary`], never silent.

use super::NlrBoundary;
use super::generator::CoreErlangGenerator;
use super::threaded_ir::{self, FrameId, ThreadedStmt, TokenId, VerifyError};
use beamtalk_cerl_doc::Document;
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::block_facts::loop_body_returns;
use beamtalk_core::source_analysis::Span;
use std::collections::HashSet;

/// The boundary of the REPL input being compiled (on `ReplContext`).
#[derive(Debug, Clone, Default)]
pub(crate) struct ReplEvalBoundary {
    /// The eval's NLR token variable: `Some` when the input holds a
    /// loop-body `^`.
    token: Option<String>,
    /// The spans of the input's loop-body `^`s, the ones that throw to it.
    returns: HashSet<Span>,
}

impl CoreErlangGenerator {
    /// Opens the eval boundary of the REPL input `expressions`: records its
    /// loop-body `^`s and, when there are any, mints the NLR token they throw
    /// to, before any of the input is compiled (the method mint order).
    /// Returns the token, which [`Self::wrap_repl_eval_body`] wraps the eval
    /// body with. Closed by [`Self::exit_repl_eval_boundary`].
    pub fn enter_repl_eval_boundary(&mut self, expressions: &[Expression]) -> Option<String> {
        let returns: HashSet<Span> = loop_body_returns(expressions).into_iter().collect();
        let token = (!returns.is_empty() && !boundary_fault::active())
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

    /// The token the REPL `^` at `span` throws to: the eval's, when `span` is
    /// one of the input's loop-body `^`s. `None` for every other `^`, which
    /// answers its value where it stands. A loop-body `^` with no token is
    /// reported as [`VerifyError::LoopReturnWithoutBoundary`] (and lowered to
    /// its value, the pre-BT-4288 shape).
    pub(in crate::core_erlang) fn repl_loop_return_token(&mut self, span: Span) -> Option<String> {
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
            &[VerifyError::LoopReturnWithoutBoundary { at: span }],
            "a loop-body `^` must throw to an NLR boundary",
            span,
        );
        None
    }
}

/// Test-only: compiles REPL inputs without their eval boundary, as before
/// BT-4288, so [`VerifyError::LoopReturnWithoutBoundary`] can be shown to fire
/// on a real compile (`beamtalk-repl/tests/repl_loop_body_return.rs`).
/// Compiled for this crate's tests and for dependents that enable the `test`
/// feature; otherwise [`active`](boundary_fault::active) is always `false`.
pub mod boundary_fault {
    #[cfg(any(test, feature = "test"))]
    use std::cell::Cell;

    #[cfg(any(test, feature = "test"))]
    thread_local! {
        static OFF: Cell<bool> = const { Cell::new(false) };
    }

    /// Whether the REPL eval boundary is switched off on this thread.
    #[cfg(any(test, feature = "test"))]
    pub(crate) fn active() -> bool {
        OFF.with(Cell::get)
    }

    /// Whether the REPL eval boundary is switched off on this thread.
    #[cfg(not(any(test, feature = "test")))]
    pub(crate) fn active() -> bool {
        false
    }

    /// Runs `body` with the REPL eval boundary switched off, restoring it
    /// afterwards even if `body` panics.
    #[cfg(any(test, feature = "test"))]
    pub fn without_boundary<T>(body: impl FnOnce() -> T) -> T {
        struct Reset;
        impl Drop for Reset {
            fn drop(&mut self) {
                OFF.with(|off| off.set(false));
            }
        }
        OFF.with(|off| off.set(true));
        let _reset = Reset;
        body()
    }
}
