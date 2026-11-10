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
//!   fun) the cleanup writes the cell and the throw resumes with the carried
//!   state under the cell as it is afterwards (`refresh(Self, Carried)`): the
//!   fields are the cell's, and the `__local__` keys a merged fun's `^`
//!   flushed keep the values they had at the `^`, never the ones bound at the
//!   `ensure:`. A class method's throw carries nothing: its class variables
//!   are written in place (ADR 0130), so nothing is relayed.
//! - **However the cleanup runs.** When neither block needs threading, or an
//!   operand is not a block literal, the cleanup is applied as a closure,
//!   which threads nothing and writes the actor-state cell in place. An actor
//!   method's `^` stored its `State` to the cell before it threw, so the
//!   throw resumes with the carried state under the cell, as in a closure.
//!   Such a closure never writes the REPL's bindings (a block that assigns a
//!   binding cannot be stored, ADR 0133) or a value type's `Self` (a closure
//!   cannot assign its fields), so there nothing is relayed.
//! - **A `^` of another scope** (a block created by another method) carries
//!   that scope's state, not this one's, and is re-raised as it came. Where
//!   the actor-state cell is live (an actor instance method's threaded body)
//!   it takes the foreign clause (ADR 0146 R1, [`EnsureForeignNlr`]): the
//!   cleanup runs from the cell and its final `StateAcc` is stored to the
//!   cell before the re-raise, so a scope that unwinds by a `^` it does not
//!   own leaves the cell holding its final version. The error path cannot:
//!   it runs with write-through off, so its writes die with the re-raise.
//!   Elsewhere (a class method running in its caller, ADR 0142, a value
//!   type, the REPL) there is no actor `State` to store, and in a closure the
//!   state already is the cell; there it takes the error path.
//! - **`Block>>ensure:` reached dynamically** (`perform:`, BT-4429). Its
//!   structural-fallback body runs in the Block method, where every `^` is of
//!   another scope and its state kind is known only at run time
//!   ([`NlrCarry::Foreign`]). The cleanup is a closure, so it writes only the
//!   cell: the relay matches every `$bt_nlr` throw, snapshots the cell before
//!   the cleanup, and resumes the throw with the cell over the carried state
//!   when the carried state was the cell's
//!   (`beamtalk_actor_state:refresh_carried/2`,
//!   [`RethrowSlot::CellIfCarried`]); any other state (the REPL's bindings, a
//!   value type's `Self`, another actor's state) a closure cannot write, and
//!   resumes as it came. "Was the cell's" is decided by identity, not by
//!   equal fields (BT-4448): a closure of another actor of the same class,
//!   run here away from home, carries its own actor's capture stamped with
//!   that actor (`beamtalk_actor_state:nlr_state/2`), never this cell.
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
    self, EnsureCatchVars, EnsureCleanup, EnsureForeignNlr, EnsureNlrRelay, NlrCarry, RenderCtx,
    RethrowSlot, ThreadedStmt, TokenId,
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
    /// throw, `foreign` (ADR 0146 R1) the one every other `$bt_nlr` throw
    /// takes where the actor-state cell is live (`cell_live`), `carry` what
    /// the scope's throw carries (with whether the construct's scratch map is
    /// private, which the verifier reads to exempt it) and `cleanup` how the
    /// cleanup runs (inlined, ending at a version of the carried family, or
    /// as a closure). The single owner of every compiled `ensure:` catch.
    pub(in crate::core_erlang) fn ensure_catch(
        &mut self,
        triple: EnsureCatchTriple,
        (carry, private_scratch, cell_live): (Option<NlrCarry>, bool, bool),
        cleanup: EnsureCleanup,
        (relay, foreign): (Option<EnsureNlrRelay>, Option<EnsureForeignNlr>),
        other: Document<'static>,
        span: Span,
    ) -> Document<'static> {
        let foreign = match fault::active() {
            // The catch before ADR 0146 R1: a foreign `^` takes the error
            // path, whose cleanup's writes die with the re-raise.
            Some(fault::RelayFault::ForeignOnErrorPath) => None,
            _ => foreign,
        };
        let relay = match fault::active() {
            None | Some(fault::RelayFault::ForeignOnErrorPath) => relay,
            Some(fault::RelayFault::NoRelay) => None,
            // The relay before review: a cell-carried throw resumed with what
            // a `^` at this site would carry, rebuilt from the site's bindings.
            #[cfg(any(test, feature = "test"))]
            Some(fault::RelayFault::RebuildAtSite) => relay.map(|relay| {
                if relay.rethrow == RethrowSlot::CellOverCarried && carry == Some(NlrCarry::Live) {
                    EnsureNlrRelay {
                        rethrow: RethrowSlot::SiteState(self.nlr_throw_state_doc(span)),
                        ..relay
                    }
                } else {
                    relay
                }
            }),
        };
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
            other_pair_var: if relay.is_some() || foreign.is_some() {
                self.fresh_temp_var("EnsureOther")
            } else {
                String::new()
            },
            cell_snapshot_var: match &relay {
                Some(relay) if relay.rethrow == RethrowSlot::CellIfCarried => {
                    self.fresh_temp_var("EnsureCellSnap")
                }
                _ => String::new(),
            },
            foreign_nlr_var: if foreign.is_some() {
                self.fresh_temp_var("EnsureForeignNlr")
            } else {
                String::new()
            },
            type_var: triple.type_var,
            error_var: triple.error_var,
            stack_var: triple.stack_var,
        };
        let ir = [ThreadedStmt::EnsureCatch {
            vars: Box::new(vars),
            token,
            carry,
            private_scratch,
            cleanup,
            relay,
            cell_live,
            foreign,
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

/// Test-only: compiles `ensure:` with its `$bt_nlr` relay as it was before a
/// fix, so [`VerifyError::EnsureCleanupLostOnNlr`](super::threaded_ir::VerifyError::EnsureCleanupLostOnNlr)
/// (and, for the foreign clause,
/// [`VerifyError::EnsureForeignCleanupNotStored`](super::threaded_ir::VerifyError::EnsureForeignCleanupNotStored))
/// can be shown to fire on a real compile
/// (`beamtalk-repl/tests/repl_loop_body_return.rs`,
/// `tests/ensure_relay_corpus.rs`). Compiled for this crate's tests and for
/// dependents that enable the `test` feature; otherwise
/// [`active`](fault::active) is always `None`.
pub mod fault {
    #[cfg(any(test, feature = "test"))]
    use std::cell::Cell;

    /// The relay as it was before a fix.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[cfg_attr(not(any(test, feature = "test")), allow(dead_code))]
    pub enum RelayFault {
        /// No relay: the catch re-raises the scope's own throw after the
        /// cleanup, as every `ensure:` did before BT-4312.
        NoRelay,
        /// A cell-carried throw resumes with the state a `^` at the
        /// `ensure:` site would carry, rebuilt from the site's bindings, as
        /// the first BT-4312 relay did. Test builds only, with the
        /// [`RethrowSlot::SiteState`](crate::core_erlang::threaded_ir::RethrowSlot::SiteState)
        /// it builds.
        #[cfg(any(test, feature = "test"))]
        RebuildAtSite,
        /// ADR 0146 R1: no foreign clause. A `^` of another method of the
        /// same actor takes the error path, which runs the cleanup with
        /// write-through off and re-raises, so the cleanup's writes never
        /// reach the cell (`VerifyError::EnsureForeignCleanupNotStored`).
        ForeignOnErrorPath,
    }

    #[cfg(any(test, feature = "test"))]
    thread_local! {
        static FAULT: Cell<Option<RelayFault>> = const { Cell::new(None) };
    }

    /// The relay fault switched on on this thread, if any.
    #[cfg(any(test, feature = "test"))]
    pub(crate) fn active() -> Option<RelayFault> {
        FAULT.with(Cell::get)
    }

    /// The relay fault switched on on this thread, if any.
    #[cfg(not(any(test, feature = "test")))]
    pub(crate) fn active() -> Option<RelayFault> {
        None
    }

    /// Runs `body` with `fault` switched on, restoring the relay afterwards
    /// even if `body` panics.
    #[cfg(any(test, feature = "test"))]
    pub fn with_fault<T>(fault: RelayFault, body: impl FnOnce() -> T) -> T {
        struct Reset;
        impl Drop for Reset {
            fn drop(&mut self) {
                FAULT.with(|f| f.set(None));
            }
        }
        FAULT.with(|f| f.set(Some(fault)));
        let _reset = Reset;
        body()
    }

    /// Runs `body` with the `ensure:` relay switched off
    /// ([`RelayFault::NoRelay`]).
    #[cfg(any(test, feature = "test"))]
    pub fn without_relay<T>(body: impl FnOnce() -> T) -> T {
        with_fault(RelayFault::NoRelay, body)
    }
}
