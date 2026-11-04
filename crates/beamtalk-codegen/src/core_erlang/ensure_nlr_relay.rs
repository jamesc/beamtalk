// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4312: an `ensure:` cleanup run by a `^` keeps its writes.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! An `ensure:` is not a boundary: its cleanup runs while an error or a `^`
//! unwinds through it, and its writes go with whichever boundary that
//! reaches. The invariant, for every scope kind:
//!
//! - **Error.** The cleanup runs and the error propagates; the boundary that
//!   catches it (`on:do:`, `Result tryDo:`, a class-method invocation) decides
//!   what is kept, as for any other write made inside it.
//! - **A `^` of the scope.** Its throw carries the scope's state to the
//!   boundary that answers it ([`NlrSlot`](super::nlr::NlrSlot)): the REPL's
//!   bindings map, an actor method's `State`, a value-type method's `Self`. The
//!   catch runs the cleanup from that carried state and resumes the throw with
//!   the cleanup's result, so the boundary answers the cleanup's writes too.
//!   Where the state is the actor-state cell (an actor closure, a merged Tier 2
//!   fun) the cleanup writes the cell and the throw resumes with the cell as it
//!   is afterwards. A class method's throw carries nothing: its class variables
//!   are written in place (ADR 0130), so nothing is relayed.
//! - **A `^` of another scope** (a block created by another method) carries
//!   that scope's state, not this one's: it takes the error path and is
//!   re-raised as it came.
//!
//! The same holds at every nesting: a statement, a conditional arm, a loop or
//! fold body, an `on:do:` arm or another `ensure:` cleanup (whose relay hands
//! the next one out the state it threads), a stored closure (the cell), and up
//! to the method's or the REPL eval's NLR boundary.
//!
//! Before BT-4312 the catch re-raised the original throw after the cleanup,
//! so the boundary answered the state at the `^` and dropped the cleanup's
//! writes: `w := 0. [^5] ensure: [w := 9]` left `w` at 0 at the REPL once that
//! `^` became a throw (and BT-4288's loop-body `^` before it), and an actor
//! method's `[^1] ensure: [self.n := 9]` never kept `n`. The catch is one
//! [`ThreadedStmt::EnsureCatch`] node, built here for every compiled `ensure:`
//! and verified: a cleanup that writes the carried state with no relay is
//! [`VerifyError::EnsureCleanupLostOnNlr`](super::threaded_ir::VerifyError::EnsureCleanupLostOnNlr).

use super::generator::CoreErlangGenerator;
use super::threaded_ir::{
    self, EnsureCatchVars, EnsureNlrRelay, NlrCarry, RenderCtx, ThreadedStmt, TokenId,
};
use beamtalk_cerl_doc::Document;
use beamtalk_core::source_analysis::Span;

/// The catch triple of an `ensure:` (`catch <Type, Error, Stack>`), minted by
/// its lowering before the protected body, and the variable a relay's seed
/// reads the in-flight throw's state from
/// ([`CoreErlangGenerator::ensure_relay_carried_var`]).
#[allow(clippy::struct_field_names)]
pub(in crate::core_erlang) struct EnsureCatchTriple {
    pub type_var: String,
    pub error_var: String,
    pub stack_var: String,
    pub carried_var: Option<String>,
}

impl CoreErlangGenerator {
    /// Builds, verifies and renders the catch of a compiled `ensure:`
    /// ([`ThreadedStmt::EnsureCatch`]): `other` is the error path (the cleanup
    /// and the re-raise), `relay` the `$bt_nlr` clause for the scope's own
    /// throw, `carry` what that throw carries and `cleanup_final` the carried
    /// family's version the cleanup ends at. The single owner of every
    /// compiled `ensure:` catch.
    pub(in crate::core_erlang) fn ensure_catch(
        &mut self,
        triple: EnsureCatchTriple,
        carry: Option<NlrCarry>,
        cleanup_final: Option<usize>,
        relay: Option<EnsureNlrRelay>,
        other: Document<'static>,
        span: Span,
    ) -> Document<'static> {
        let relay = relay.filter(|_| !fault::active());
        let token = self.scope_nlr_token().map(TokenId::new);
        // The relay clause's pattern variables, minted only when it is
        // rendered so an `ensure:` that relays nothing keeps its numbering.
        let nlr_var = |generator: &mut Self, base: &str| {
            if relay.is_some() {
                generator.fresh_temp_var(base)
            } else {
                String::new()
            }
        };
        let vars = EnsureCatchVars {
            nlr_tok_var: nlr_var(self, "EnsureNlrTok"),
            nlr_val_var: nlr_var(self, "EnsureNlrVal"),
            nlr_carried_var: match triple.carried_var {
                Some(var) => var,
                None => nlr_var(self, "EnsureCarried"),
            },
            other_pair_var: nlr_var(self, "EnsureOther"),
            type_var: triple.type_var,
            error_var: triple.error_var,
            stack_var: triple.stack_var,
        };
        let ir = [ThreadedStmt::EnsureCatch {
            vars: Box::new(vars),
            token,
            carry,
            cleanup_final,
            relay,
            other,
            span,
        }];
        let errors = self.verify_threaded_ir(&ir);
        self.report_threaded_ir_verify_errors(
            &errors,
            "an `ensure:` cleanup run by a `^` must reach the `^`'s boundary",
            span,
        );
        threaded_ir::render(&ir, &mut RenderCtx::new(self))
    }

    /// The pattern variable the relay binds to the in-flight throw's state,
    /// for a seed that reads it. Minted ahead of [`Self::ensure_catch`]'s
    /// own, so the seed and the clause agree.
    pub(in crate::core_erlang) fn ensure_relay_carried_var(&mut self) -> String {
        self.fresh_temp_var("EnsureCarried")
    }
}

/// Test-only: compiles `ensure:` without its `$bt_nlr` relay, as before
/// BT-4312, so [`VerifyError::EnsureCleanupLostOnNlr`](super::threaded_ir::VerifyError::EnsureCleanupLostOnNlr)
/// can be shown to fire on a real compile
/// (`beamtalk-repl/tests/repl_loop_body_return.rs`). Compiled for this crate's
/// tests and for dependents that enable the `test` feature; otherwise
/// [`active`](fault::active) is always `false`.
pub mod fault {
    #[cfg(any(test, feature = "test"))]
    use std::cell::Cell;

    #[cfg(any(test, feature = "test"))]
    thread_local! {
        static OFF: Cell<bool> = const { Cell::new(false) };
    }

    /// Whether the `ensure:` relay is switched off on this thread.
    #[cfg(any(test, feature = "test"))]
    pub(crate) fn active() -> bool {
        OFF.with(Cell::get)
    }

    /// Whether the `ensure:` relay is switched off on this thread.
    #[cfg(not(any(test, feature = "test")))]
    pub(crate) fn active() -> bool {
        false
    }

    /// Runs `body` with the `ensure:` relay switched off, restoring it
    /// afterwards even if `body` panics.
    #[cfg(any(test, feature = "test"))]
    pub fn without_relay<T>(body: impl FnOnce() -> T) -> T {
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
