// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Non-local-return (NLR) try/catch scaffolding shared by the Actor,
//! class-method, and value-type calling conventions.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! The NLR value objects ([`NlrCatchVars`], [`NlrBoundary`]) and the
//! `wrap_*_with_nlr_catch` family of [`CoreErlangGenerator`] methods that
//! build the try/catch scaffolding around a 4-tuple
//! `{'$bt_nlr', Token, Value, State}` throw. See ADR 0041.

use crate::core_erlang::CodeGenContext;
use crate::core_erlang::actor_state_access::{actor_state_call, actor_state_helper};
use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::synced_call::ActorStateSource;
use crate::core_erlang::threaded_ir::{self, NlrCarry};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_cerl_doc::{Document, INDENT, line, nest};
use beamtalk_core::source_analysis::Span;

/// Fresh temporary variable names shared by all three NLR try/catch wrappers
/// (Actor, class-method, and value-type).
///
/// Allocated by [`CoreErlangGenerator::alloc_nlr_catch_vars`] and consumed by
/// its `format_try_prefix`/`format_catch_suffix` methods — the single place
/// the try/catch scaffolding is built, by
/// [`CoreErlangGenerator::wrap_body_with_nlr_catch`] for every boundary.
#[allow(clippy::struct_field_names)]
pub(in crate::core_erlang) struct NlrCatchVars {
    pub token_var: String,
    pub result_var: String,
    pub cls_var: String,
    pub err_var: String,
    pub stk_var: String,
    pub ctk_var: String,
    pub val_var: String,
    /// State variable captured from the 4-tuple NLR throw.
    pub state_var: String,
    pub ot_pair_var: String,
}

/// The per-context NLR boundary — the *only* thing that differs between the
/// Actor, class-method and value-type non-local-return catch wrappers once the catch
/// vars are shared.
///
/// All three contexts catch the same 4-tuple throw `{'$bt_nlr', Token, Value, State}`
/// (ADR 0041's state-carrying NLR convention); they disagree only about the Document
/// the matching catch arm yields. This enum captures that single axis so the catch
/// scaffolding can be written once (see [`nlr_arm_result`]) instead of being
/// copy-evolved per context.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum NlrBoundary {
    /// Actor (`gen_server`) methods: the catch arm yields
    /// `{'reply', Value, beamtalk_actor_state:commit(Self, State)}`. ADR 0146:
    /// the method commits the actor's cell, the one place its fields live, not
    /// the copy the throw carries; away from home (an away `super_send/5`)
    /// `commit/2` answers the carried `State` unchanged.
    ActorReply,
    /// Class methods: the catch arm yields the bare `Value`. Class variables
    /// live in the class process's dictionary (ADR 0130 §3), so nothing is
    /// returned alongside the result.
    ClassMethod,
    /// Value-type methods (and value-type extension funs): the catch arm
    /// yields the bare `Value`, as the body's normal path does (BT-4503). A
    /// value-type method function answers exactly what a send of it answers,
    /// so `dispatch/3`, the own-module typed direct call (BT-4473) and
    /// the runtime's extension apply all take its return as the send's
    /// result. A value type's `Self` is immutable to its callers, so nothing
    /// is returned beside the result.
    ValueType,
    /// BT-4288: a REPL eval (`repl_nlr`). The throw's state is the workspace
    /// bindings map at the `^`, so the catch arm yields the eval's own
    /// `{Value, Bindings}`; in trace mode (`trace_source` is the input's
    /// source text) `{[{<<Source>>, Value}], Bindings}`, one step for the
    /// input the `^` ended.
    Repl { trace_source: Option<String> },
}

/// BT-4503: what the matching NLR catch arm of an [`NlrBoundary`] yields, as
/// the function the boundary closes answers it to its caller. The one fact
/// `ThreadedIr`'s verifier checks against the scope's calling convention
/// (`VerifyError::MethodNlrAnswersPair`); [`nlr_arm_result`] renders it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum NlrArmShape {
    /// The bare `Value`: what a send answers.
    Value,
    /// ADR 0146: the `gen_server` reply committing the actor's cell,
    /// `{'reply', Value, beamtalk_actor_state:commit(Self, State)}`.
    Reply,
    /// The `gen_server` reply committing the carried copy, `{'reply', Value,
    /// State}`: every actor method before ADR 0146, which lost a cleanup's
    /// writes and, for a block run by another actor, every write since the
    /// block's creation (BT-4459). Only the test-only
    /// [`fault::NlrFault::CommitCarried`] builds it, to show
    /// `VerifyError::ActorNlrCommitBypassesCell` rejects it.
    CarriedReply,
    /// A `{Result, State}` pair: the REPL eval's `{Value, Bindings}` (or its
    /// trace-mode `{[{Source, Value}], Bindings}`).
    ValueStatePair,
}

impl NlrBoundary {
    /// What this boundary's matching catch arm yields ([`NlrArmShape`]).
    pub(in crate::core_erlang) fn arm_shape(&self) -> NlrArmShape {
        match self {
            NlrBoundary::ActorReply => match fault::active() {
                Some(fault::NlrFault::CommitCarried) => NlrArmShape::CarriedReply,
                _ => NlrArmShape::Reply,
            },
            NlrBoundary::ClassMethod | NlrBoundary::ValueType => NlrArmShape::Value,
            NlrBoundary::Repl { .. } => NlrArmShape::ValueStatePair,
        }
    }
}

/// Builds the Document the matching NLR catch arm yields for `boundary`.
///
/// This is the single place the per-context divergence between the three former
/// `wrap_*_body_with_nlr_catch` wrappers lives. `val_var`/`state_var` are the
/// catch-bound `Value`/`State` extracted from the 4-tuple throw. Shared by the
/// gen-server wrapper ([`CoreErlangGenerator::wrap_body_with_nlr_catch`]) and
/// [`NlrCatchVars::format_catch_suffix`].
///
/// Use Document/docvec! — never format!() for Core Erlang fragments.
fn nlr_arm_result(val_var: &str, state_var: &str, boundary: &NlrBoundary) -> Document<'static> {
    match boundary {
        NlrBoundary::ActorReply => {
            let state = match boundary.arm_shape() {
                NlrArmShape::CarriedReply => leaf::var(state_var.to_string()),
                _ => actor_state_call(
                    actor_state_helper::COMMIT,
                    vec![leaf::var("Self"), leaf::var(state_var.to_string())],
                ),
            };
            docvec![
                "{'reply', ",
                leaf::var(val_var.to_string()),
                ", ",
                state,
                "}"
            ]
        }
        NlrBoundary::ClassMethod | NlrBoundary::ValueType => leaf::var(val_var.to_string()),
        NlrBoundary::Repl {
            trace_source: Some(source),
        } => docvec![
            "{[{",
            leaf::binary_lit(source),
            ", ",
            leaf::var(val_var.to_string()),
            "}], ",
            leaf::var(state_var.to_string()),
            "}",
        ],
        NlrBoundary::Repl { trace_source: None } => docvec![
            "{",
            leaf::var(val_var.to_string()),
            ", ",
            leaf::var(state_var.to_string()),
            "}",
        ],
    }
}

impl NlrCatchVars {
    /// Formats the try prefix for the NLR wrapper.
    ///
    /// Returns `Document` instead of `String` for composable codegen.
    ///
    /// ```text
    /// let TokenVar = call 'erlang':'make_ref'() in
    /// try
    /// ```
    pub fn format_try_prefix(&self) -> Document<'static> {
        docvec![
            "    let ",
            leaf::var(self.token_var.clone()),
            " = call 'erlang':'make_ref'() in",
            nest(INDENT, line()),
            "try",
            line(),
        ]
    }

    /// Formats the catch suffix for the NLR wrapper.
    ///
    /// Returns `Document` instead of `String` for composable codegen.
    /// Catches 4-tuple NLR throws and yields `boundary`'s matching-arm
    /// result (see [`nlr_arm_result`]) — the single place this scaffolding
    /// is built for the Actor, class-method, and value-type boundaries
    /// alike.
    ///
    /// ```text
    /// of Result -> Result
    /// catch <Cls, Err, Stk> ->
    ///   case {Cls, Err} of
    ///     <{'throw', {'$bt_nlr', CatchTok, Val, State}}> when ... -> <boundary's arm>
    ///     <Other> when 'true' -> primop 'raw_raise'(Cls, Err, Stk)
    ///   end
    /// ```
    pub fn format_catch_suffix(&self, boundary: &NlrBoundary) -> Document<'static> {
        docvec![
            nest(INDENT, line()),
            "of ",
            leaf::var(self.result_var.clone()),
            " -> ",
            leaf::var(self.result_var.clone()),
            nest(INDENT, line()),
            "catch <",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            ", ",
            leaf::var(self.stk_var.clone()),
            "> ->",
            nest(INDENT + 2, line()),
            "case {",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            "} of",
            nest(INDENT + 4, line()),
            "<{'throw', {'$bt_nlr', ",
            leaf::var(self.ctk_var.clone()),
            ", ",
            leaf::var(self.val_var.clone()),
            ", ",
            leaf::var(self.state_var.clone()),
            "}}> ",
            "when call 'erlang':'=:='(",
            leaf::var(self.ctk_var.clone()),
            ", ",
            leaf::var(self.token_var.clone()),
            ") -> ",
            nlr_arm_result(&self.val_var, &self.state_var, boundary),
            nest(INDENT + 4, line()),
            "<",
            leaf::var(self.ot_pair_var.clone()),
            "> when 'true' -> ",
            "primop 'raw_raise'(",
            leaf::var(self.cls_var.clone()),
            ", ",
            leaf::var(self.err_var.clone()),
            ", ",
            leaf::var(self.stk_var.clone()),
            ")",
            nest(INDENT + 2, line()),
            "end",
            line(),
        ]
    }
}

/// ADR 0041: where the state a `^` thrown from the code being lowered comes
/// from, which is what its boundary answers. The one place the rule is
/// written: the throw ([`CoreErlangGenerator::nlr_throw_state_doc`]) and an
/// `ensure:` cleanup that the throw runs while it unwinds (BT-4312,
/// [`NlrSlot::carry`]) both read it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum NlrSlot {
    /// BT-4122: a merged Tier 2 fun: the cell merged with the fun's current
    /// locals (the throw bypasses the fun's exit flush).
    Tier2Merged,
    /// A class method: `nil`. Class variables are written in place (ADR 0130
    /// §3), so a `^` unwinding out of one has nothing to hand back.
    ClassMethod,
    /// BT-4288: the REPL: the workspace bindings map at the `^` (the
    /// threaded `StateAcc`), which the eval's catch answers as its bindings.
    Repl,
    /// ADR 0137 §3: an actor closure holds no threaded `State`; the actor's
    /// current one, the cell at home (the creation-time capture away).
    CellClosure,
    /// An actor instance method: its threaded `State` (or the live cell where
    /// the read choke point decides so, BT-4121/BT-4180).
    Actor,
    /// A value-type method: the latest `Self{N}`, so the field writes made
    /// before the `^` are kept.
    ValueType,
}

impl NlrSlot {
    /// BT-4312: what an `ensure:` cleanup run by such a throw must thread its
    /// writes through, so they reach the boundary with it. `None` for a class
    /// method, whose throw carries no state.
    pub(in crate::core_erlang) fn carry(self) -> Option<NlrCarry> {
        match self {
            Self::Tier2Merged | Self::CellClosure => Some(NlrCarry::Live),
            Self::ClassMethod => None,
            Self::Repl => Some(NlrCarry::Bindings),
            Self::Actor => Some(NlrCarry::ActorState),
            Self::ValueType => Some(NlrCarry::SelfVt),
        }
    }
}

impl CoreErlangGenerator {
    /// The [`NlrSlot`] of the code being lowered.
    pub(in crate::core_erlang) fn nlr_slot(&self) -> NlrSlot {
        if self.tier2_merged_fun.is_some() {
            NlrSlot::Tier2Merged
        } else if self.in_class_method() {
            NlrSlot::ClassMethod
        } else if self.is_repl_mode() {
            NlrSlot::Repl
        } else if self.in_cell_closure() {
            NlrSlot::CellClosure
        } else if self.context == CodeGenContext::Actor {
            NlrSlot::Actor
        } else {
            NlrSlot::ValueType
        }
    }

    /// The state a `^` thrown here carries in its 4th element
    /// (`{'$bt_nlr', Token, Value, State}`), from the current versions
    /// ([`NlrSlot`]).
    pub(in crate::core_erlang) fn nlr_throw_state_doc(&mut self, span: Span) -> Document<'static> {
        match self.nlr_slot() {
            NlrSlot::Tier2Merged => {
                let captured = self
                    .tier2_merged_fun
                    .as_ref()
                    .map(|merged| merged.captured.clone())
                    .unwrap_or_default();
                let take_var = self.fresh_temp_var("T2Nlr");
                let (flush_doc, final_acc) = self.tier2_flush_locals_doc(&take_var, &captured);
                docvec![
                    "let ",
                    leaf::var(take_var),
                    " = ",
                    threaded_ir::cell_take_call(),
                    " in ",
                    flush_doc,
                    leaf::var(final_acc),
                ]
            }
            NlrSlot::ClassMethod => leaf::atom("nil"),
            NlrSlot::Repl => leaf::var(self.current_state_var()),
            NlrSlot::CellClosure => self.cell_nlr_state_doc(),
            // A direct-params loop binds no `StateAcc`: its body cannot write
            // `State`, so the throw carries the state the loop captured on entry
            // (BT-3562's `current_field_read_state_var`). BT-4121: unless the
            // loop brackets guarded operators, whose slow paths wrote the cell
            // — then the throw carries the live cell, like the loop's own exit
            // pack, and the pre-throw store keeps those writes instead of
            // clobbering them with the entry state. (BT-4180: decided and
            // recorded by the read choke point.)
            NlrSlot::Actor => {
                match self.actor_state_source(threaded_ir::StateReadSite::NlrThrowState, span) {
                    ActorStateSource::Cell => threaded_ir::cell_take_call(),
                    ActorStateSource::Var(var) => leaf::var(var),
                }
            }
            NlrSlot::ValueType => leaf::var(self.current_self_var()),
        }
    }

    /// ADR 0137 §5 / ADR 0146: whether a `^` thrown here stores its state to
    /// the actor-state cell before the throw: an actor instance method's
    /// threaded body (the cell protocol's scope outside a closure, whose
    /// state already is the cell), and a merged Tier 2 fun (BT-4122, which
    /// stores its merged map). The cell is then current whenever a `^`
    /// starts: a cleanup the throw runs reads it, and the method's NLR
    /// boundary commits it (`beamtalk_actor_state:commit/2`). The one
    /// decision behind every actor `^` throw: the generic `Return` lowering
    /// and [`Self::nlr_throw_stmt`].
    pub(in crate::core_erlang) fn nlr_throw_stores_cell(&self) -> bool {
        self.synced_calls_active() || self.tier2_merged_fun.is_some()
    }

    /// A `^` a conditional arm lowers to `ThreadedIr`
    /// ([`ThreadedStmt::NlrThrow`](threaded_ir::ThreadedStmt::NlrThrow)):
    /// throws `value` with the arm's current `State` (`state_var`, in the
    /// arm's threaded scope) to the method's boundary, storing it to the cell
    /// first where the cell is live ([`Self::nlr_throw_stores_cell`]).
    /// Answers the node and the variable its (never-produced) result binds.
    pub(in crate::core_erlang) fn nlr_throw_stmt(
        &mut self,
        token: String,
        value: threaded_ir::ValueRef,
        state_var: String,
        span: Span,
    ) -> (threaded_ir::ThreadedStmt, String) {
        let result_var = self.fresh_temp_var("NlrThrow");
        let cell_live = self.nlr_throw_stores_cell();
        let cell_store =
            cell_live && fault::active() != Some(fault::NlrFault::ThrowWithoutCellStore);
        // A gensym slot renders the arm's state variable verbatim; the arm's
        // own `Bind`s already verified the version it names.
        let slot = threaded_ir::VersionedVar::new(
            threaded_ir::VersionPrefix::Gensym(state_var),
            0,
            threaded_ir::FrameId::ROOT,
        );
        let stmt = threaded_ir::ThreadedStmt::NlrThrow {
            token: threaded_ir::TokenId::new(token),
            value,
            slot: vec![slot],
            result_var: result_var.clone(),
            cell_live,
            cell_store,
            span,
        };
        (stmt, result_var)
    }

    /// Allocates fresh temporary variable names for an NLR try/catch wrapper.
    fn alloc_nlr_catch_vars(&mut self, token_var: &str) -> NlrCatchVars {
        NlrCatchVars {
            token_var: token_var.to_string(),
            result_var: self.fresh_temp_var("NlrResult"),
            cls_var: self.fresh_temp_var("NlrCls"),
            err_var: self.fresh_temp_var("NlrErr"),
            stk_var: self.fresh_temp_var("NlrStk"),
            ctk_var: self.fresh_temp_var("CatchTok"),
            val_var: self.fresh_temp_var("NlrVal"),
            state_var: self.fresh_temp_var("NlrState"),
            ot_pair_var: self.fresh_temp_var("OtherPair"),
        }
    }

    /// The single boundary-parameterised NLR try/catch wrapper.
    ///
    /// Builds its try/catch scaffolding with [`NlrCatchVars::format_try_prefix`]
    /// and [`NlrCatchVars::format_catch_suffix`], so the make-token/`try`/`of`/`catch` the 4-tuple
    /// `{'$bt_nlr', Token, Value, State}` throw template is written exactly
    /// once; only the matching arm's result Document varies, which
    /// `boundary` selects via [`nlr_arm_result`].
    ///
    /// Use Document/docvec! — never format!() for Core Erlang fragments.
    pub(in crate::core_erlang) fn wrap_body_with_nlr_catch(
        &mut self,
        body_doc: Document<'static>,
        token_var: &str,
        boundary: &NlrBoundary,
    ) -> Document<'static> {
        // ADR 0111 Phase D: this is the true call site
        // `ThreadedStmt::NlrCatch` faithfully models (module docs on
        // `threaded_ir::ThreadedStmt::NlrCatch`) — every NLR try/catch this
        // generator ever emits (Actor/ClassMethod/ValueType alike) is built
        // here. No standalone `verify()` call here: a
        // lone `NlrCatch` with no `Bind` can never trigger any
        // `VerifyError` (`walk_stmt` treats it as a no-op), so constructing
        // one on every NLR-catch wrap — a hot path — would pay a real
        // allocation for a check that can't fire (caught in review).

        let vars = self.alloc_nlr_catch_vars(token_var);
        docvec![
            vars.format_try_prefix(),
            body_doc,
            vars.format_catch_suffix(boundary),
        ]
    }

    /// BT-4312: the token of the scope being lowered: a method's (bound when a
    /// block of it holds a `^`), or the REPL eval's. Only a throw of this
    /// token carries this scope's state; a throw of any other token belongs to
    /// another scope.
    pub(in crate::core_erlang) fn scope_nlr_token(&self) -> Option<String> {
        self.current_nlr_token()
            .cloned()
            .or_else(|| self.repl_eval_token())
    }

    /// Returns the current NLR token variable name, if any.
    pub(in crate::core_erlang) fn current_nlr_token(&self) -> Option<&String> {
        self.value_type_context
            .as_ref()
            .and_then(|ctx| ctx.current_nlr_token.as_ref())
    }

    /// Sets the current NLR token variable name.
    pub(in crate::core_erlang) fn set_current_nlr_token(&mut self, token: Option<String>) {
        self.value_type_context_mut().current_nlr_token = token;
    }
}

/// Test-only: compiles an actor method's NLR boundary, or a `^` throw, as it
/// was before ADR 0146, so [`VerifyError::ActorNlrCommitBypassesCell`](super::threaded_ir::VerifyError::ActorNlrCommitBypassesCell)
/// and [`VerifyError::NlrThrowWithoutCellStore`](super::threaded_ir::VerifyError::NlrThrowWithoutCellStore)
/// can be shown to fire on a real compile (`tests/nlr.rs`). Compiled for this
/// crate's tests only; otherwise [`active`](fault::active) is always `None`.
pub mod fault {
    #[cfg(test)]
    use std::cell::Cell;

    /// An actor `^` as it was before a fix.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[cfg_attr(not(test), allow(dead_code))]
    pub enum NlrFault {
        /// The catch arm commits the state the throw carries,
        /// `{'reply', Value, State}`, instead of the actor's cell.
        CommitCarried,
        /// A `^` a conditional arm lowers to a
        /// [`ThreadedStmt::NlrThrow`](super::super::threaded_ir::ThreadedStmt::NlrThrow)
        /// throws without storing its state to the cell, as BT-3374's and
        /// BT-3495's arm throws did before BT-4535
        /// (`VerifyError::NlrThrowWithoutCellStore`).
        ThrowWithoutCellStore,
    }

    #[cfg(test)]
    thread_local! {
        static FAULT: Cell<Option<NlrFault>> = const { Cell::new(None) };
    }

    /// The commit fault switched on on this thread, if any.
    #[cfg(test)]
    pub(crate) fn active() -> Option<NlrFault> {
        FAULT.with(Cell::get)
    }

    /// The commit fault switched on on this thread, if any.
    #[cfg(not(test))]
    pub(crate) fn active() -> Option<NlrFault> {
        None
    }

    /// Runs `body` with `fault` switched on, restoring the boundary
    /// afterwards even if `body` panics.
    #[cfg(test)]
    pub fn with_fault<T>(fault: NlrFault, body: impl FnOnce() -> T) -> T {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nlr_arm_result_actor_reply_commits_the_cell() {
        let doc = nlr_arm_result("Val", "St", &NlrBoundary::ActorReply);
        assert_eq!(
            doc.to_pretty_string(),
            "{'reply', Val, call 'beamtalk_actor_state':'commit'(Self, St)}"
        );
    }

    #[test]
    fn nlr_arm_result_actor_reply_under_the_fault_commits_the_carried_state() {
        let doc = fault::with_fault(fault::NlrFault::CommitCarried, || {
            nlr_arm_result("Val", "St", &NlrBoundary::ActorReply)
        });
        assert_eq!(doc.to_pretty_string(), "{'reply', Val, St}");
    }

    #[test]
    fn nlr_arm_result_class_method_yields_bare_value() {
        let doc = nlr_arm_result("V", "S", &NlrBoundary::ClassMethod);
        assert_eq!(doc.to_pretty_string(), "V");
    }

    #[test]
    fn nlr_arm_result_repl_yields_value_bindings_tuple() {
        let doc = nlr_arm_result("V", "S", &NlrBoundary::Repl { trace_source: None });
        assert_eq!(doc.to_pretty_string(), "{V, S}");
    }

    #[test]
    fn nlr_arm_result_repl_trace_yields_one_step() {
        let doc = nlr_arm_result(
            "V",
            "S",
            &NlrBoundary::Repl {
                trace_source: Some("x".to_string()),
            },
        );
        let expected = docvec!["{[{", leaf::binary_lit("x"), ", V}], S}"].to_pretty_string();
        assert_eq!(doc.to_pretty_string(), expected);
    }

    #[test]
    fn nlr_arm_result_value_type_yields_bare_value() {
        let doc = nlr_arm_result("V", "S", &NlrBoundary::ValueType);
        assert_eq!(doc.to_pretty_string(), "V");
    }

    /// BT-4503: every boundary's rendered arm has the shape `arm_shape`
    /// declares — the fact the verifier checks is the one rendered.
    #[test]
    fn nlr_arm_result_matches_arm_shape() {
        for boundary in [
            NlrBoundary::ActorReply,
            NlrBoundary::ClassMethod,
            NlrBoundary::ValueType,
            NlrBoundary::Repl { trace_source: None },
            NlrBoundary::Repl {
                trace_source: Some("x".to_string()),
            },
        ] {
            let doc = nlr_arm_result("V", "S", &boundary).to_pretty_string();
            let shape = match doc.as_str() {
                "V" => NlrArmShape::Value,
                "{'reply', V, S}" => NlrArmShape::CarriedReply,
                d if d.starts_with("{'reply', ") && d.contains("'commit'(Self, S)") => {
                    NlrArmShape::Reply
                }
                d if d.starts_with('{') && d.ends_with(", S}") => NlrArmShape::ValueStatePair,
                other => panic!("unexpected arm {other} for {boundary:?}"),
            };
            assert_eq!(shape, boundary.arm_shape(), "{boundary:?}");
        }
    }

    fn make_test_vars() -> NlrCatchVars {
        NlrCatchVars {
            token_var: "Tok0".to_string(),
            result_var: "Res0".to_string(),
            cls_var: "Cls0".to_string(),
            err_var: "Err0".to_string(),
            stk_var: "Stk0".to_string(),
            ctk_var: "CTok0".to_string(),
            val_var: "Val0".to_string(),
            state_var: "St0".to_string(),
            ot_pair_var: "OtP0".to_string(),
        }
    }

    #[test]
    fn format_try_prefix_binds_token_with_make_ref() {
        let doc = make_test_vars().format_try_prefix().to_pretty_string();
        assert!(
            doc.contains("Tok0"),
            "prefix should bind token var. Got:\n{doc}"
        );
        assert!(
            doc.contains("call 'erlang':'make_ref'()"),
            "prefix should call erlang:make_ref/0 to mint the NLR token. Got:\n{doc}"
        );
        assert!(
            doc.contains("try"),
            "prefix should open a try block. Got:\n{doc}"
        );
    }

    #[test]
    fn format_catch_suffix_passthrough_arm_uses_result_var() {
        let doc = make_test_vars()
            .format_catch_suffix(&NlrBoundary::ValueType)
            .to_pretty_string();
        // The `of` arm passes through non-throw results unchanged: `of Res0 -> Res0`.
        assert!(
            doc.contains("of Res0 -> Res0"),
            "catch suffix `of` arm must pass through the result_var unchanged. Got:\n{doc}"
        );
    }

    #[test]
    fn format_catch_suffix_value_type_nlr_arm_yields_bare_value() {
        let doc = make_test_vars()
            .format_catch_suffix(&NlrBoundary::ValueType)
            .to_pretty_string();
        // Matching NLR throw: pattern must name the '$bt_nlr' tag.
        assert!(
            doc.contains("'$bt_nlr'"),
            "NLR pattern must match the '$bt_nlr' tag. Got:\n{doc}"
        );
        // Token guard: catch-token must equal the method's own token.
        assert!(
            doc.contains("'=:='(CTok0, Tok0)"),
            "catch suffix should guard with CTok0 =:= Tok0. Got:\n{doc}"
        );
        // ValueType boundary (BT-4503): the NLR arm answers the bare value.
        assert!(
            doc.contains(") -> Val0") && !doc.contains("{Val0, St0}"),
            "ValueType NLR arm must yield the bare Value. Got:\n{doc}"
        );
    }

    #[test]
    fn format_catch_suffix_reraises_non_nlr_exceptions() {
        let doc = make_test_vars()
            .format_catch_suffix(&NlrBoundary::ValueType)
            .to_pretty_string();
        // Non-NLR exceptions fall through to the wildcard arm and are re-raised.
        assert!(
            doc.contains("primop 'raw_raise'(Cls0, Err0, Stk0)"),
            "catch suffix must re-raise non-NLR exceptions via primop raw_raise. Got:\n{doc}"
        );
    }
}
