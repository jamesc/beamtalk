// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0137 Phase 2a (BT-4020): the cell bracket around calls that may run
//! user code, and the check that none is missing.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! An actor instance method brackets every call that may run one of this actor's
//! blocks or methods in this process with a [`ThreadedStmt::SyncedCall`]:
//! store the threaded `State` to the actor-state cell, make the call, reload
//! the cell as the next `State` version. A call that is not bracketed is a
//! [`VerifyError::UnbracketedUserCodeSend`], reported through
//! [`CoreErlangGenerator::report_threaded_ir_verify_errors`].
//!
//! ## One place
//!
//! The bracket is built in [`CoreErlangGenerator::threaded_expression`], the
//! one entry every expression-position state effect already goes through (it
//! owns the `State` version a bracket steps), around the call the send
//! emitters produce. The send emitter ([`CoreErlangGenerator::generate_message_send`])
//! notes each call it compiles; a call noted outside a bracket window is a
//! gap. Nothing else emits a bracket.
//!
//! ## What may run user code
//!
//! [`beamtalk_core::state_threading_selectors::send_user_code`] classifies
//! the selector from the existing intrinsic and inlined-construct tables.
//! This module adds the two judgements that need generator context: a
//! self-send whose target is a compiled method of this actor keeps explicit
//! `State` threading and is not a call into the cell (the compiled callee
//! brackets what it runs itself), and the dispatching branch of an operator
//! guard.
//!
//! ## Verification
//!
//! A call outside every bracket is a [`VerifyError::UnbracketedUserCodeSend`]
//! (a hard failure in debug builds). The invariant probe
//! (`tests/synced_call_sites_corpus.rs`) compiles generated programs in a
//! count-only test mode that records each gap's codegen site instead.

use super::threaded_ir::{
    FrameId, StaleReadDefect, StateReadSite, StateRestoreDefect, ThreadedStmt, ThreadedValue,
    UserCodeRegion, ValueRef, VerifyError, VersionPrefix, VersionedVar,
};
use super::{CodeGenContext, CoreErlangGenerator};
use crate::core_erlang::docvec;
use crate::core_erlang::leaf;
use beamtalk_cerl_doc::Document;
use beamtalk_core::ast::{Expression, MessageSelector};
use beamtalk_core::source_analysis::Span;
use beamtalk_core::state_threading_selectors::{SendUserCode, send_user_code};

/// A call that may run user code, compiled outside every bracket.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct UnbracketedSend {
    pub(in crate::core_erlang) selector: String,
    pub(in crate::core_erlang) region: UserCodeRegion,
    pub(in crate::core_erlang) at: Span,
    /// Probe only: the codegen site (`file:function`) that compiled the
    /// call outside a `ThreadedIr` frame, and the codegen frames above it.
    pub(in crate::core_erlang) site: GapSite,
}

/// A bracketed conditional fallback send
/// ([`CoreErlangGenerator::bracket_conditional_fallback`]).
pub(in crate::core_erlang) struct FallbackBracket {
    /// The open `let` chain: store, send, reload.
    pub(in crate::core_erlang) prelude: Document<'static>,
    /// The variable bound to the send's answer.
    pub(in crate::core_erlang) result: String,
    /// The variable bound to the reloaded cell.
    pub(in crate::core_erlang) state: String,
}

/// Where in the code generator a gap was compiled: [`capture_site`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(in crate::core_erlang) struct GapSite {
    /// `file:function` of the nearest lowering frame that is not generic plumbing.
    pub(in crate::core_erlang) site: String,
    /// The codegen frames, innermost first, for choosing the plumbing list.
    pub(in crate::core_erlang) chain: String,
}

/// One gap as the invariant probe sees it (`tests/synced_call_sites.rs`).
#[cfg(test)]
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct SiteRecord {
    pub(in crate::core_erlang) site: String,
    pub(in crate::core_erlang) chain: String,
    pub(in crate::core_erlang) selector: String,
    pub(in crate::core_erlang) region: UserCodeRegion,
}

#[cfg(test)]
thread_local! {
    /// Test-only fault: no call is bracketed (every user-code call is a gap), so
    /// the invariant probe can be shown to see a missing bracket.
    pub(in crate::core_erlang) static FAULT_NO_BRACKETS: std::cell::Cell<bool> =
        const { std::cell::Cell::new(false) };
    /// Test-only fault (BT-4122): Tier 2 threads the legacy way (fields
    /// through `StateAcc`, callers rebinding `State` from its return), so
    /// the `FieldReadFromStateAcc` rule can be shown to fire on it.
    pub(in crate::core_erlang) static FAULT_LEGACY_TIER2: std::cell::Cell<bool> =
        const { std::cell::Cell::new(false) };
    /// Test-only fault (BT-4179): inside a loop that brackets guarded
    /// operators, an `on:do:` stores the threaded `State` (`Snapshot` only in
    /// an actor closure) and a nested `StateAcc` producer seeds from it, as
    /// before the fix, so the loop checkout can be shown to report both.
    pub(in crate::core_erlang) static FAULT_STALE_STATE_IN_BRACKETED_LOOP: std::cell::Cell<bool> =
        const { std::cell::Cell::new(false) };
    /// Test-only fault (BT-4180): the read choke point
    /// ([`CoreErlangGenerator::actor_state_read_source`]) ignores the
    /// take-reads decision while the loop still brackets, so every read site
    /// emits the threaded `State` (or a pre-extracted parameter) and the
    /// checkout can be shown to report `StaleReadInBracketedLoop`.
    pub(in crate::core_erlang) static FAULT_ENTRY_STATE_READS: std::cell::Cell<bool> =
        const { std::cell::Cell::new(false) };
}

#[cfg(test)]
thread_local! {
    /// Count-only mode (the invariant probe): gaps and other verifier findings
    /// are recorded, not failed on.
    static COUNT_ONLY: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Restores count-only mode on drop.
#[cfg(test)]
#[derive(Debug)]
pub(in crate::core_erlang) struct CountOnly(bool);

#[cfg(test)]
impl Drop for CountOnly {
    fn drop(&mut self) {
        COUNT_ONLY.with(|c| c.set(self.0));
    }
}

/// Puts the current thread in count-only mode until the guard drops.
#[cfg(test)]
pub(in crate::core_erlang) fn count_only() -> CountOnly {
    CountOnly(COUNT_ONLY.with(|c| c.replace(true)))
}

#[cfg(test)]
thread_local! {
    /// Every gap a count-only compile on this thread recorded.
    pub(in crate::core_erlang) static SITE_LOG: std::cell::RefCell<Vec<SiteRecord>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

#[cfg(test)]
/// Frames that are plumbing every expression passes through, never the owning
/// lowering: skipped when naming a gap's site.
const PLUMBING_FRAMES: &[&str] = &[
    "capture_site",
    "record_gap",
    "note_send",
    "note_self_dispatch",
    "generate_message_send",
    "generate_expression",
    "generate_expression_inner",
    "expression_doc",
    "generate_runtime_dispatch",
    "generate_binary_op",
    "guarded_op_doc",
];

/// The `file:function` site of the code generator frame that compiled the
/// current call (count-only mode; a backtrace per gap is too slow otherwise).
#[cfg(test)]
pub(in crate::core_erlang) fn capture_site() -> GapSite {
    let text = std::backtrace::Backtrace::force_capture().to_string();
    let mut frames: Vec<(String, String)> = Vec::new();
    let mut lines = text.lines().peekable();
    while let Some(line) = lines.next() {
        let trimmed = line.trim_start();
        let Some((index, symbol)) = trimmed.split_once(": ") else {
            continue;
        };
        if !index.chars().all(|c| c.is_ascii_digit()) {
            continue;
        }
        // The frame's source line follows its symbol; only code generator frames count.
        let Some(at) = lines
            .peek()
            .and_then(|next| next.trim_start().strip_prefix("at "))
        else {
            continue;
        };
        let path = at.rsplitn(3, ':').last().unwrap_or(at).replace('\\', "/");
        let Some((_, file)) = path.split_once("src/core_erlang/") else {
            continue;
        };
        let function = symbol
            .split('<')
            .next()
            .unwrap_or(symbol)
            .trim()
            .to_string();
        frames.push((file.to_string(), function));
    }
    let chain = frames
        .iter()
        .take(14)
        .map(|(file, function)| format!("{file}:{function}"))
        .collect::<Vec<_>>()
        .join(" < ");
    let site = frames
        .iter()
        .find(|(_, function)| {
            !PLUMBING_FRAMES.contains(&function.as_str())
                && !function.starts_with('{')
                && !function.starts_with("with_")
        })
        .map(|(file, function)| format!("{file}:{function}"))
        .unwrap_or_default();
    GapSite { site, chain }
}

/// The generator's bracket bookkeeping.
#[derive(Debug)]
pub(in crate::core_erlang) struct SyncedCallState {
    /// How many bracket windows enclose the code being compiled. A call
    /// compiled inside one runs between a store and a reload.
    pub(in crate::core_erlang) window_depth: usize,
    /// Calls that may run user code compiled outside every window.
    pub(in crate::core_erlang) gaps: Vec<UnbracketedSend>,
    /// Guarded operators compiled in a stateless loop with no loop-level
    /// bracket (BT-4121): reported as
    /// [`VerifyError::UnbracketedGuardedOperator`], separately from
    /// [`Self::gaps`] so the two rules stay cleanly separable.
    pub(in crate::core_erlang) guarded_op_gaps: Vec<UnbracketedSend>,
    /// Tier 2 `StateAcc` field reads (BT-4122): reported as
    /// [`VerifyError::FieldReadFromStateAcc`], separately from both lists
    /// above so the three rules stay cleanly separable.
    pub(in crate::core_erlang) tier2_stateacc_gaps: Vec<UnbracketedSend>,
    /// Uses of a bracketing loop's entry `State` inside the loop (BT-4179),
    /// reported as [`VerifyError::CatchWithoutStateRestore`]
    /// ([`StateRestoreDefect::PreStateOverLiveCell`]) for an `on:do:`
    /// boundary and [`VerifyError::StaleStateSeedInBracketedLoop`] for a
    /// `StateAcc` seed; and actor-state reads not sourced from the cell
    /// inside one (BT-4180), reported as
    /// [`VerifyError::StaleReadInBracketedLoop`].
    pub(in crate::core_erlang) stale_state_gaps: Vec<(StaleStateUseKind, UnbracketedSend)>,
    /// One marker per loop lowering in progress (BT-4121), innermost last.
    /// A guarded operator compiled in a stateless loop body flags the
    /// innermost marker instead of bracketing (there is no `State` in scope
    /// to store); the loop lowering pops its marker after the body and
    /// brackets the loop when the marker asked for it. Behind a [`RefCell`]
    /// like [`Self::inlined_classes`] so a checkout can pop it while
    /// holding `&mut self`.
    pub(in crate::core_erlang) guarded_op_loops: std::cell::RefCell<Vec<GuardedOpLoopMarker>>,
    /// The class of a conditional with a `super` send in a branch
    /// ([`CoreErlangGenerator::classify_inlined`]), decided once per send.
    pub(in crate::core_erlang) inlined_classes:
        std::cell::RefCell<std::collections::HashMap<Span, UserCodeCall>>,
    /// BT-4269: names the loop being planned will bind (its block parameters
    /// and the temps its body assigns that are not fields), seen as locals by
    /// [`CoreErlangGenerator::analysis_touches_actor_state`] while
    /// [`CoreErlangGenerator::loop_holds_bracketed_call`] probes the loop
    /// before those names are in scope. Empty outside a probe.
    pub(in crate::core_erlang) probe_locals: std::cell::RefCell<Vec<String>>,
}

impl SyncedCallState {
    pub(in crate::core_erlang) fn new() -> Self {
        Self {
            window_depth: 0,
            gaps: Vec::new(),
            guarded_op_gaps: Vec::new(),
            tier2_stateacc_gaps: Vec::new(),
            stale_state_gaps: Vec::new(),
            guarded_op_loops: std::cell::RefCell::default(),
            inlined_classes: std::cell::RefCell::default(),
            probe_locals: std::cell::RefCell::default(),
        }
    }
}

/// BT-4121: one loop lowering's guarded-operator bracket state, pushed by
/// [`CoreErlangGenerator::push_guarded_op_loop`] before the loop's body (and
/// condition) is lowered and popped after.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct GuardedOpLoopMarker {
    /// Whether the loop will bracket: the body or condition holds a guarded
    /// operator (decided at push time by the same classifier the send
    /// emitter uses, so the two cannot disagree) and the loop runs where
    /// the cell protocol applies (an actor instance method).
    pub(in crate::core_erlang) will_bracket: bool,
    /// The first guarded operator that asked for the bracket while the body
    /// was lowered (its operator and span, for the report when no enclosing
    /// loop brackets). `None` until one fires.
    pub(in crate::core_erlang) need: Option<GuardedOpNeed>,
    /// BT-4179: the `State` variable in scope when the loop began (the one a
    /// bracket stores before the loop). Once the loop brackets, the slow
    /// paths in its body write the cell past it, so a use of it inside the
    /// loop is stale.
    pub(in crate::core_erlang) entry_state: String,
    /// BT-4179: the threaded-`State` uses built while this marker was
    /// innermost (an `on:do:` boundary storing a `State`, a `StateAcc`
    /// producer seeding from one, and — BT-4180 — every actor-state read
    /// that did not take the cell). Checked at checkout against the loop's
    /// own bracket decision and entry `State`, neither of which comes from
    /// the gates that chose those shapes.
    pub(in crate::core_erlang) state_uses: Vec<ThreadedStateUse>,
}

/// BT-4179: one use of a threaded `State` variable in a loop body, noted on
/// the innermost loop marker for its checkout
/// ([`CoreErlangGenerator::checkout_guarded_op_loop`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct ThreadedStateUse {
    pub(in crate::core_erlang) kind: StaleStateUseKind,
    /// The `State` variable used.
    pub(in crate::core_erlang) var: String,
    pub(in crate::core_erlang) at: Span,
}

/// BT-4179: what a [`ThreadedStateUse`] does with the `State` variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum StaleStateUseKind {
    /// An `on:do:` boundary stores it on entry and on catch
    /// (`CellBoundary::PreState`).
    OnDoStore,
    /// A `StateAcc` loop or fold packs its accumulator from it
    /// (`ThreadingPlan::generate_pack_prefix`).
    StateAccSeed,
    /// BT-4180: an actor-state read at `site` takes the state map from it
    /// (or, for [`StaleReadDefect::PreExtractedParam`], reads a hybrid
    /// loop's pre-extracted field parameter), in a frame that holds no
    /// whole `State` ([`CoreErlangGenerator::actor_state_read_source`]).
    /// Stale under any bracket, whatever the variable: inside the region
    /// only the cell is current.
    Read {
        site: StateReadSite,
        defect: StaleReadDefect,
        /// Where the read was lowered (the checkout's own region is the
        /// loop's enclosing one).
        region: UserCodeRegion,
    },
}

impl StaleStateUseKind {
    /// Whether this use, under a loop that brackets with entry `State`
    /// `entry_state`, reads or stores a version the bracket's slow paths have
    /// overtaken in the cell.
    fn is_stale_under_bracket(self, var: &str, entry_state: &str) -> bool {
        match self {
            Self::OnDoStore | Self::StateAccSeed => var == entry_state,
            Self::Read { .. } => true,
        }
    }

    /// The finding a stale use of this kind at `at` reports.
    fn verify_error(self, at: Span) -> VerifyError {
        match self {
            Self::OnDoStore => VerifyError::CatchWithoutStateRestore {
                defect: StateRestoreDefect::PreStateOverLiveCell,
                at,
            },
            Self::StateAccSeed => VerifyError::StaleStateSeedInBracketedLoop { at },
            Self::Read {
                site,
                defect,
                region,
            } => VerifyError::StaleReadInBracketedLoop {
                site,
                defect,
                region,
                at,
            },
        }
    }

    /// The invariant probe's selector prefix for this use.
    #[cfg(test)]
    fn probe_prefix(self) -> &'static str {
        match self {
            Self::OnDoStore | Self::StateAccSeed => "stale-state",
            Self::Read { .. } => "stale-read",
        }
    }

    /// The gap label naming this use in a report.
    fn label(self) -> &'static str {
        match self {
            Self::OnDoStore => "on:do:",
            Self::StateAccSeed => "StateAcc seed",
            Self::Read { site, .. } => site.label(),
        }
    }
}

/// BT-4180: where an actor-state read takes the actor's state map from
/// ([`CoreErlangGenerator::actor_state_read_source`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum ActorStateSource {
    /// The live cell (`take()`): inside a bracketed region.
    Cell,
    /// A variable: a threaded `State` or a hybrid loop's pre-extracted field
    /// parameter.
    Var(String),
}

/// BT-4121: one guarded operator's request for its loop's bracket.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct GuardedOpNeed {
    pub(in crate::core_erlang) selector: String,
    pub(in crate::core_erlang) at: Span,
}

/// How a send relates to the cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum UserCodeCall {
    /// Not a call into user code (an intrinsic, an inlined construct, a cast).
    No,
    /// A self-send to a compiled method of this actor: explicit `State`
    /// threading, no bracket (the amended ADR 0137 §1).
    CompiledSelfSend,
    /// A binary operator on an operand of unknown type: the bare BIF when
    /// the operands are numbers, a message send otherwise. The bracket goes
    /// in the dispatching branch only.
    GuardedOperator,
    /// Bracket the call.
    Yes,
    /// A self-send that may reach `dispatch/4`'s default arm (an inherited
    /// `Object`/`ProtoObject` method, `does_not_understand`, a reflective
    /// selector): that code is compiled without actor `State` and may run a
    /// block, so it is bracketed like [`Self::Yes`]. Counted separately.
    SelfViaDefaultArm,
}

impl CoreErlangGenerator {
    /// Whether the bracket is being built and checked for the code being
    /// compiled: this is an actor instance method (a
    /// class method and a value type thread no actor `State`; the REPL has
    /// none).
    pub(in crate::core_erlang) fn synced_calls_active(&self) -> bool {
        self.synced_calls_scope() && !self.in_cell_closure()
    }

    /// Whether the code being compiled is in the scope the cell protocol covers
    /// (an actor instance method body, closures included): the context test
    /// behind both [`Self::synced_calls_active`] (a threaded body, where calls
    /// are bracketed) and [`Self::cell_closures_apply`] (a closure, which reads
    /// the cell directly and brackets nothing).
    pub(in crate::core_erlang) fn synced_calls_scope(&self) -> bool {
        self.context == CodeGenContext::Actor
            && !self.in_class_method()
            && !self.is_repl_mode()
            // A compiled class (not a legacy workspace block body).
            && self.class_context.is_some()
            // Outside a method body (a `state:` default, built while the state
            // map itself is being made) there is no `State` to store.
            && self.current_method_selector.is_some()
    }

    /// The kind of region being compiled, for the report.
    pub(in crate::core_erlang) fn user_code_region(&self) -> UserCodeRegion {
        use super::threaded_ir::{RebindFrameKind, ThreadingMode};
        if self.block_depth > 0 {
            return UserCodeRegion::Tier2Body;
        }
        match self.rebind_frames.get(&self.current_frame()) {
            Some(frame) => match &frame.kind {
                RebindFrameKind::BranchArm => UserCodeRegion::BranchArm,
                RebindFrameKind::Loop(ThreadingMode::TupleAcc(_)) => UserCodeRegion::FoldBody,
                RebindFrameKind::Loop(_) if self.in_fold_frame => UserCodeRegion::FoldBody,
                RebindFrameKind::Loop(_) => UserCodeRegion::LetrecLoop,
                RebindFrameKind::MethodBody => UserCodeRegion::MethodRoot,
            },
            None if self.in_loop_body => {
                if self.in_fold_frame {
                    UserCodeRegion::FoldBody
                } else {
                    UserCodeRegion::LetrecLoop
                }
            }
            None if self.current_frame() == super::threaded_ir::FrameId::ROOT => {
                UserCodeRegion::MethodRoot
            }
            None => UserCodeRegion::Other,
        }
    }

    /// Whether a self-send of `selector` statically resolves to a compiled
    /// Beamtalk method of the class being compiled or of a compiled actor
    /// superclass. Anything else may reach `dispatch/4`'s default arm — an
    /// inherited `Object`/`ProtoObject`/`Actor` method (compiled without
    /// actor `State`), a native method, an ADR 0066 extension fun, or
    /// `does_not_understand` — and is bracketed.
    fn self_send_target_is_compiled(&self, selector: &MessageSelector) -> bool {
        let Some(hierarchy) = &self.class_hierarchy else {
            return false;
        };
        let class = self.class_name();
        hierarchy
            .find_method(&class, selector.name().as_str())
            .is_some_and(|m| {
                let defined_in = m.defined_in.as_str();
                hierarchy
                    .get_class(defined_in)
                    .is_some_and(|c| !c.is_native)
                    && defined_in != "Actor"
                    && hierarchy
                        .superclass_chain(defined_in)
                        .iter()
                        .any(|c| c.as_str() == "Actor")
            })
    }

    /// Whether a `super` send compiled now is in a position that threads no
    /// `State` (a block or loop body of an actor instance method): it runs the
    /// superclass method against the cell, bracketed like any call that may run
    /// user code (ADR 0137 §3). At the method root it threads `State` itself.
    pub(in crate::core_erlang) fn super_send_uses_cell(&self) -> bool {
        self.synced_calls_active() && (self.block_depth > 0 || self.in_loop_body)
    }

    /// A `super` send resolves from the superclass: a compiled actor method
    /// there is explicit threading, anything else (an `Object` method, a
    /// native one, DNU) may reach `dispatch/4`'s default arm.
    fn classify_super_send(&self, selector: &MessageSelector) -> UserCodeCall {
        if self.super_send_uses_cell() {
            return UserCodeCall::Yes;
        }
        let compiled = self.class_hierarchy.as_ref().is_some_and(|hierarchy| {
            let Some(superclass) = hierarchy
                .get_class(&self.class_name())
                .and_then(|c| c.superclass.clone())
            else {
                return false;
            };
            hierarchy
                .find_method(superclass.as_str(), selector.name().as_str())
                .is_some_and(|m| {
                    let defined_in = m.defined_in.as_str();
                    hierarchy
                        .get_class(defined_in)
                        .is_some_and(|c| !c.is_native)
                        && defined_in != "Actor"
                        && hierarchy
                            .superclass_chain(defined_in)
                            .iter()
                            .any(|c| c.as_str() == "Actor")
                })
        });
        if compiled {
            UserCodeCall::CompiledSelfSend
        } else {
            UserCodeCall::SelfViaDefaultArm
        }
    }

    /// The classification of a self-send (`self` receiver, or a
    /// binary selector sent to `self`): explicit threading when it resolves
    /// to a compiled actor method, the default-arm bracket otherwise.
    fn classify_self_send(&self, selector: &MessageSelector) -> UserCodeCall {
        if self.self_send_target_is_compiled(selector) {
            UserCodeCall::CompiledSelfSend
        } else {
            UserCodeCall::SelfViaDefaultArm
        }
    }

    /// Classifies the send `receiver selector arguments` (ADR 0137 §1–2).
    pub(in crate::core_erlang) fn classify_send(
        &self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        is_cast: bool,
    ) -> UserCodeCall {
        if is_cast {
            // A cast posts to the receiver's mailbox; nothing runs here.
            return UserCodeCall::No;
        }
        let receiver = receiver.unwrap_parens();
        if matches!(receiver, Expression::Super(_)) {
            return self.classify_super_send(selector);
        }
        if let MessageSelector::Binary(op) = selector {
            return self.classify_binary(receiver, op, arguments);
        }
        if super::expr_shape::is_self_identifier(receiver)
            && super::expr_shape::selector_dispatches_via_self(selector)
        {
            return self.classify_self_send(selector);
        }
        if self.is_no_user_code_send(receiver, selector) {
            return UserCodeCall::No;
        }
        match send_user_code(receiver, selector, arguments) {
            SendUserCode::Never => UserCodeCall::No,
            SendUserCode::Inlined => self.classify_inlined(selector, arguments),
            SendUserCode::MayRun => UserCodeCall::Yes,
            // A fold the block analysis threads is lowered in place (its
            // statements are bracketed where they are compiled); one it does
            // not is `lists:foreach(Closure, List)` with a send fallback: a call.
            SendUserCode::InlinedOrClosureCall => {
                if arguments.iter().any(|a| match a {
                    Expression::Block(block) => self.loop_body_has_carried_mutations(block),
                    _ => false,
                }) {
                    UserCodeCall::No
                } else {
                    UserCodeCall::Yes
                }
            }
            // A loop is lowered in place when it carries something, else its
            // blocks are closures handed to the runtime (cell closures): a
            // call, bracketed when a block touches the actor's state.
            SendUserCode::LoopOrClosureCall => self.classify_loop(receiver, selector, arguments),
        }
    }

    /// Whether `receiver selector` is statically a `no_user_code` primitive
    /// (ADR 0137 §4): the receiver's inferred class has a method for `selector`
    /// that is a primitive the stdlib marked `no_user_code`. `build-stdlib` rejects
    /// the mark unless the declaring class is `sealed` (no override can run user
    /// code) and every parameter is a primitive value type.
    fn is_no_user_code_send(&self, receiver: &Expression, selector: &MessageSelector) -> bool {
        use beamtalk_core::semantic_analysis::InferredType;
        // Only a type the compiler derived (a literal, a constructor): a declared
        // annotation is not checked at run time, so the value may be any class.
        let Some(InferredType::Known {
            class_name,
            provenance: beamtalk_core::semantic_analysis::TypeProvenance::Inferred(_),
            ..
        }) = self.type_map.get(receiver.span())
        else {
            return false;
        };
        self.primitive_bindings
            .is_no_user_code(class_name.as_str(), selector.name().as_str())
    }

    /// A construct the compiler lowers in place is not a call. The exception is
    /// a conditional that does not thread `State` (a `super` send is not a
    /// state effect, BT-3995) with a `super` send in a branch: it is a real send
    /// with closure arguments, and the `super` send in the closure reads and
    /// writes the cell, so the send is bracketed.
    fn classify_inlined(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> UserCodeCall {
        if !beamtalk_core::state_threading_selectors::is_conditional_selector(
            selector.name().as_str(),
        ) {
            return UserCodeCall::No;
        }
        let blocks: Vec<&beamtalk_core::ast::Block> = arguments
            .iter()
            .filter_map(|a| match a {
                Expression::Block(block) => Some(block),
                _ => None,
            })
            .collect();
        if !blocks.iter().any(|block| Self::block_sends_to_super(block)) {
            return UserCodeCall::No;
        }
        // Decided once per send: whether the conditional threads can change as
        // its receiver is sequenced ahead, and the window, the gap note and the
        // lowering must agree.
        let key = blocks[0].span;
        if let Some(class) = self.synced.inlined_classes.borrow().get(&key) {
            return *class;
        }
        let class = if self.conditional_blocks_need_threading(&blocks) {
            UserCodeCall::No
        } else {
            UserCodeCall::Yes
        };
        self.synced.inlined_classes.borrow_mut().insert(key, class);
        class
    }

    /// The class of a loop with literal blocks ([`SendUserCode::LoopOrClosureCall`]).
    fn classify_loop(
        &self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> UserCodeCall {
        // An evaluated literal block (`[...] value`): in place when it threads,
        // else a closure applied by the runtime.
        if selector
            .well_known()
            .is_some_and(beamtalk_core::ast::well_known::WellKnownSelector::is_block_value)
        {
            if let Expression::Block(block) = receiver {
                if self.value_block_needs_threading(&self.block_profile_or_analyze(block)) {
                    return UserCodeCall::No;
                }
            }
        }
        let blocks: Vec<&beamtalk_core::ast::Block> = std::iter::once(receiver)
            .chain(arguments.iter())
            .filter_map(|e| match e {
                Expression::Block(block) => Some(block),
                _ => None,
            })
            .collect();
        let carried = blocks
            .iter()
            .any(|block| self.loop_body_has_carried_mutations(block))
            || matches!(receiver, Expression::Block(_))
                && arguments.iter().any(|a| match a {
                    Expression::Block(body) => {
                        self.while_condition_has_carried_effects(receiver, Some(body))
                    }
                    _ => false,
                });
        if carried {
            UserCodeCall::No
        } else if blocks.iter().any(|block| {
            // BT-4121: a guarded operator's slow path can reach user code
            // that touches this actor's fields, so a closure holding one is
            // bracketed like a closure that touches the state syntactically.
            // Read off the same classifier the send emitter exempts.
            self.block_touches_actor_state(block) || self.block_contains_guarded_operator(block)
        }) {
            UserCodeCall::Yes
        } else {
            UserCodeCall::No
        }
    }

    fn classify_binary(
        &self,
        left: &Expression,
        op: &str,
        arguments: &[Expression],
    ) -> UserCodeCall {
        use super::operators::{StructuralBinary, structural_binary};
        match structural_binary(op) {
            // `>>` looks a method up; `**`, `++`, `%` and the equality
            // family are bare BIFs (ADR 0002): none dispatches to user code.
            Some(
                StructuralBinary::MethodLookup
                | StructuralBinary::Power
                | StructuralBinary::Concat
                | StructuralBinary::Bif(_),
            ) => return UserCodeCall::No,
            None => {}
        }
        if super::expr_shape::is_self_identifier(left) {
            let selector = MessageSelector::Binary(op.into());
            return self.classify_self_send(&selector);
        }
        let arithmetic = matches!(op, "+" | "-" | "*" | "/");
        let comparison = matches!(op, "<" | ">" | "<=" | ">=");
        let Some(right) = arguments.first() else {
            return UserCodeCall::Yes;
        };
        if !arithmetic && !comparison {
            return UserCodeCall::Yes;
        }
        // The guard rule is `operators.rs`'s (`operator_guard`); an arithmetic
        // operator with a bare-BIF receiver still reaches the number-on-the-left
        // coercion (a user `plusFromNumber:`) when its right operand is not
        // statically numeric (ADR 0116).
        let coercion = arithmetic && !self.receiver_is_statically_numeric(right);
        if self.operator_guard(op, left).is_some() || coercion {
            UserCodeCall::GuardedOperator
        } else {
            UserCodeCall::No
        }
    }

    /// The send emitter's note of a call it is compiling: a call that may run
    /// user code outside every bracket window is a gap (reported when the
    /// module is finished, [`Self::finish_synced_calls`]).
    pub(in crate::core_erlang) fn note_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        is_cast: bool,
        at: Span,
    ) {
        if !self.synced_calls_active() || self.in_cell_closure() {
            return;
        }
        // A send to `self` is noted where its dispatch is built
        // ([`Self::note_self_dispatch`]), once, whichever path reaches it.
        if super::expr_shape::is_self_identifier(receiver)
            || matches!(receiver.unwrap_parens(), Expression::Super(_))
        {
            return;
        }
        let class = self.classify_send(receiver, selector, arguments, is_cast);
        if matches!(class, UserCodeCall::No | UserCodeCall::CompiledSelfSend) {
            return;
        }
        // A guarded operator in a loop that holds no whole `State` flags its
        // loop's marker instead of bracketing (there is no `State` in the
        // loop's fun to store); the loop brackets around it (BT-4121). Inside
        // a window [`Self::synced_call_value`]'s arm below handles it — a
        // sync site always lowers through `threaded_expression`, so exactly
        // one of the two arms fires per send.
        if class == UserCodeCall::GuardedOperator && self.loop_holds_no_whole_state() {
            if self.synced.window_depth == 0 {
                self.note_stateless_guarded_op(selector.name().to_string(), at);
            }
            return;
        }
        if self.synced.window_depth > 0 {
            return;
        }
        self.record_gap(selector.name().to_string(), at);
    }

    /// In count-only test mode, accepts a verifier finding instead of failing
    /// on it; answers whether it did.
    #[cfg(test)]
    #[allow(clippy::unused_self)]
    pub(in crate::core_erlang) fn note_counted_verify_errors(&self) -> bool {
        COUNT_ONLY.with(std::cell::Cell::get)
    }

    #[cfg(not(test))]
    #[allow(clippy::unused_self)]
    pub(in crate::core_erlang) fn note_counted_verify_errors(&self) -> bool {
        false
    }

    /// The statement of a self-dispatch prelude: the dispatch `call` itself, or
    /// (store-only [`ThreadedStmt::SyncedCall`]) with the cell stored first
    /// when the send may reach `dispatch/4`'s default arm.
    pub(in crate::core_erlang) fn self_dispatch_statement(
        &mut self,
        selector: &MessageSelector,
        call: Document<'static>,
        store: VersionedVar,
        span: Span,
    ) -> ThreadedStmt {
        if !self.synced_calls_active()
            || self.in_cell_closure()
            || !super::expr_shape::selector_dispatches_via_self(selector)
            || self.classify_self_send(selector) != UserCodeCall::SelfViaDefaultArm
        {
            return ThreadedStmt::Statement(call, span);
        }
        ThreadedStmt::SyncedCall {
            store,
            call,
            result: None,
            load: None,
            span,
            tier2_local_rebinds: Vec::new(),
            on_private_scratch: false,
        }
    }

    /// The self-dispatch builders' note of a dispatching self-send: a send
    /// that may reach `dispatch/4`'s default arm outside every bracket window
    /// is a gap, and every such send is counted for the report. A send to a
    /// compiled actor method keeps explicit `State` threading and is neither.
    pub(in crate::core_erlang) fn note_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) {
        if !self.synced_calls_active() || self.in_cell_closure() {
            return;
        }
        // A selector an intrinsic claims (`error:`, `class`, ...) is not a dispatch.
        if !super::expr_shape::selector_dispatches_via_self(selector)
            || self.classify_self_send(selector) != UserCodeCall::SelfViaDefaultArm
        {
            return;
        }
        if self.synced.window_depth == 0 {
            let at = arguments.first().map(Expression::span).unwrap_or_default();
            self.record_gap(format!("{} (self, default arm)", selector.name()), at);
        }
    }

    /// The store-only bracket around the call document of a `super` send that
    /// may reach `dispatch/4`'s default arm, built here because
    /// `generate_super_send` returns a bare expression document (its caller
    /// unpacks the `{reply, R, State}` pair): the store of the current `State`
    /// to the cell, then the call, rendered through the one `SyncedCall`
    /// node. A compiled `super` target keeps explicit threading unchanged.
    pub(in crate::core_erlang) fn bracket_super_send(
        &mut self,
        selector: &MessageSelector,
        call: Document<'static>,
        span: Span,
    ) -> Document<'static> {
        if !self.synced_calls_active()
            || self.in_cell_closure()
            || self.classify_super_send(selector) != UserCodeCall::SelfViaDefaultArm
        {
            return call;
        }
        let store = VersionedVar::new(
            VersionPrefix::State,
            self.state_version(),
            self.current_frame(),
        );
        let node = ThreadedStmt::SyncedCall {
            store,
            call,
            result: None,
            load: None,
            span,
            tier2_local_rebinds: Vec::new(),
            on_private_scratch: false,
        };
        let mut ctx = super::threaded_ir::RenderCtx::new(self);
        super::threaded_ir::render(&[node], &mut ctx)
    }

    /// BT-4244: the bracket around a boolean conditional's non-Boolean fallback
    /// send (`CoreErlangGenerator::mutation_boolean_fallback_clause`), built
    /// here because the mutation-path `case` is a `Document` whose arms each
    /// answer `{Result, NewState}`: the store of `base_state` (the scratch
    /// map the arms continue) to the cell, the send, and the reload of the
    /// cell as the arm's state, rendered through the one `SyncedCall` node.
    ///
    /// Called only where [`Self::conditional_fallback_brackets`] holds.
    pub(in crate::core_erlang) fn bracket_conditional_fallback(
        &mut self,
        call: Document<'static>,
        base_state: &str,
        span: Span,
    ) -> FallbackBracket {
        let result = self.fresh_temp_var("Synced");
        let state = self.fresh_temp_var("FallbackState");
        // The node is a self-contained slice (verified and rendered on its
        // own, like `conditional_baseline_tuple`'s slot): its gensym names
        // render verbatim, so they sit in the root frame of that slice.
        let frame = FrameId::ROOT;
        let node = ThreadedStmt::SyncedCall {
            store: VersionedVar::new(VersionPrefix::Gensym(base_state.to_string()), 0, frame),
            call,
            result: Some(result.clone()),
            load: Some(VersionedVar::new(
                VersionPrefix::Gensym(state.clone()),
                0,
                frame,
            )),
            span,
            tier2_local_rebinds: Vec::new(),
            on_private_scratch: self.in_private_scratch,
        };
        // Today's only caller checks `conditional_fallback_brackets()` first,
        // which excludes a private scratch map. The verify guards a future
        // caller that skips that check: a bracket on a private scratch map is
        // reported (`SyncedCallOnPrivateScratch`).
        let errors = self.verify_threaded_ir(std::slice::from_ref(&node));
        self.report_threaded_ir_verify_errors(&errors, "conditional fallback bracket", span);
        let mut ctx = super::threaded_ir::RenderCtx::new(self);
        let prelude = super::threaded_ir::render(&[node], &mut ctx);
        FallbackBracket {
            prelude,
            result,
            state,
        }
    }

    /// BT-4244: whether a boolean conditional's fallback send is bracketed
    /// ([`Self::bracket_conditional_fallback`]): in an actor instance method
    /// body whose frame holds a whole `State` to store. Where the cell
    /// protocol applies but the frame holds none (a direct-params, hybrid or
    /// tuple-accumulator loop, [`Self::loop_holds_no_whole_state`]), the
    /// fallback cannot bracket, so a block that touches the actor's state is
    /// not passed as a closure at all
    /// (`CoreErlangGenerator::conditional_fallback_stubs_state`).
    pub(in crate::core_erlang) fn conditional_fallback_brackets(&self) -> bool {
        self.synced_calls_active() && !self.loop_holds_no_whole_state()
    }

    /// BT-4244: whether a boolean conditional's fallback passes a block that
    /// touches the actor's state as a raising stub rather than a closure: the
    /// cell protocol applies, but the send cannot be bracketed
    /// ([`Self::conditional_fallback_brackets`]), so a closure would read a
    /// stale cell and its writes would never reach the threaded `State`.
    pub(in crate::core_erlang) fn conditional_fallback_stubs_state(&self) -> bool {
        self.synced_calls_active() && self.loop_holds_no_whole_state()
    }

    /// Records one gap.
    pub(in crate::core_erlang) fn record_gap(&mut self, selector: String, at: Span) {
        let gap = self.make_unbracketed_send(selector, at);
        self.synced.gaps.push(gap);
    }

    /// BT-4121: records one guarded-operator gap, reported as
    /// [`VerifyError::UnbracketedGuardedOperator`] separately from
    /// [`Self::record_gap`].
    pub(in crate::core_erlang) fn record_guarded_op_gap(&mut self, selector: String, at: Span) {
        let gap = self.make_unbracketed_send(selector, at);
        self.synced.guarded_op_gaps.push(gap);
    }

    /// BT-4122: records one Tier 2 `StateAcc` field access, reported as
    /// [`VerifyError::FieldReadFromStateAcc`] separately from both lists
    /// above. `selector` names the send or access, suffixed by the caller
    /// with which fun-body arm fired (read or write).
    pub(in crate::core_erlang) fn record_tier2_stateacc_gap(&mut self, selector: String, at: Span) {
        let gap = self.make_unbracketed_send(selector, at);
        self.synced.tier2_stateacc_gaps.push(gap);
    }

    /// One gap record for `selector` at `at`, shared by [`Self::record_gap`],
    /// [`Self::record_guarded_op_gap`] and [`Self::record_tier2_stateacc_gap`].
    fn make_unbracketed_send(&self, selector: String, at: Span) -> UnbracketedSend {
        let region = self.user_code_region();
        #[cfg(test)]
        let site = if COUNT_ONLY.with(std::cell::Cell::get) {
            capture_site()
        } else {
            GapSite::default()
        };
        #[cfg(not(test))]
        let site = GapSite::default();
        UnbracketedSend {
            selector,
            region,
            at,
            site,
        }
    }

    /// Reports the gaps recorded while compiling the module as verifier
    /// findings (or, in count-only test mode, logs them for the probe).
    pub(in crate::core_erlang) fn finish_synced_calls(&mut self, module_span: Span) {
        let gaps = std::mem::take(&mut self.synced.gaps);
        // BT-4121: the guarded-operator gaps, reported as their own
        // `VerifyError` variant (own arms, no shared shaping with the gaps
        // above).
        let guarded_op_gaps = std::mem::take(&mut self.synced.guarded_op_gaps);
        let tier2_stateacc_gaps = std::mem::take(&mut self.synced.tier2_stateacc_gaps);
        let stale_state_gaps = std::mem::take(&mut self.synced.stale_state_gaps);
        #[cfg(test)]
        if COUNT_ONLY.with(std::cell::Cell::get) {
            SITE_LOG.with(|log| {
                log.borrow_mut().extend(gaps.iter().map(|g| SiteRecord {
                    site: g.site.site.clone(),
                    chain: g.site.chain.clone(),
                    selector: g.selector.clone(),
                    region: g.region,
                }));
                log.borrow_mut()
                    .extend(guarded_op_gaps.iter().map(|g| SiteRecord {
                        site: g.site.site.clone(),
                        chain: g.site.chain.clone(),
                        selector: format!("guarded-op {}", g.selector),
                        region: g.region,
                    }));
                log.borrow_mut()
                    .extend(tier2_stateacc_gaps.iter().map(|g| SiteRecord {
                        site: g.site.site.clone(),
                        chain: g.site.chain.clone(),
                        selector: format!("tier2 {}", g.selector),
                        region: g.region,
                    }));
                log.borrow_mut()
                    .extend(stale_state_gaps.iter().map(|(kind, g)| SiteRecord {
                        site: g.site.site.clone(),
                        chain: g.site.chain.clone(),
                        selector: format!("{} {}", kind.probe_prefix(), g.selector),
                        region: g.region,
                    }));
            });
            return;
        }
        let errors: Vec<VerifyError> = gaps
            .iter()
            .map(|g| VerifyError::UnbracketedUserCodeSend {
                selector: g.selector.clone(),
                region: g.region,
                at: g.at,
            })
            .collect();
        self.report_threaded_ir_verify_errors(
            &errors,
            "call that may run user code outside a SyncedCall",
            module_span,
        );
        let guarded_op_errors: Vec<VerifyError> = guarded_op_gaps
            .iter()
            .map(|g| VerifyError::UnbracketedGuardedOperator {
                selector: g.selector.clone(),
                region: g.region,
                at: g.at,
            })
            .collect();
        self.report_threaded_ir_verify_errors(
            &guarded_op_errors,
            "guarded operator in a stateless loop outside a loop-level bracket",
            module_span,
        );
        let tier2_errors: Vec<VerifyError> = tier2_stateacc_gaps
            .iter()
            .map(|g| VerifyError::FieldReadFromStateAcc {
                selector: g.selector.clone(),
                region: g.region,
                at: g.at,
            })
            .collect();
        self.report_threaded_ir_verify_errors(
            &tier2_errors,
            "field read from a Tier 2 block's StateAcc",
            module_span,
        );
        let stale_state_errors: Vec<VerifyError> = stale_state_gaps
            .iter()
            .map(|(kind, g)| kind.verify_error(g.at))
            .collect();
        self.report_threaded_ir_verify_errors(
            &stale_state_errors,
            "use of a bracketing loop's entry State, or an actor-state read in a bracketed loop not sourced from take()",
            module_span,
        );
    }
}

impl CoreErlangGenerator {
    /// Wraps a rendered method body: when the method stored to the cell, the cell holds
    /// the version stored last (older than the `State` the method answers once it
    /// threads on; every store goes through `cell_put_call`), so the reply stores its final `State` back. Every compiled
    /// callee then hands `call_foreign/3` an answer equal to the cell, which is what
    /// lets it tell a block's write from a compiled method's own history.
    pub(in crate::core_erlang) fn with_exit_store(
        &mut self,
        body: Document<'static>,
    ) -> Document<'static> {
        if !super::threaded_ir::take_cell_store_emitted() {
            return body;
        }
        let reply = self.fresh_temp_var("ExitReply");
        let state = self.fresh_temp_var("ExitState");
        let doc = docvec![
            "let ",
            leaf::var(reply.clone()),
            " = ",
            body,
            " in case ",
            leaf::var(reply.clone()),
            " of <{'reply', _, ",
            leaf::var(state.clone()),
            "}> when 'true' -> let _ = ",
            super::threaded_ir::cell_put_call(leaf::var(state)),
            " in ",
            leaf::var(reply.clone()),
            " <_> when 'true' -> ",
            leaf::var(reply),
            " end",
        ];
        // The exit store is itself a cell store; it must not mark the next method.
        super::threaded_ir::take_cell_store_emitted();
        doc
    }
}

/// Proof that a bracket window was opened for an expression
/// ([`CoreErlangGenerator::open_sync_window`]).
#[derive(Debug, Clone, Copy)]
#[must_use]
pub(in crate::core_erlang) struct SyncWindow(());

impl CoreErlangGenerator {
    /// Opens a bracket window around `expr` when it is a call that may run
    /// user code and nothing already brackets it. Calls compiled inside the
    /// window (its operands) run between the store and the reload, so they
    /// are not gaps.
    pub(in crate::core_erlang) fn open_sync_window(
        &mut self,
        expr: &Expression,
    ) -> Option<SyncWindow> {
        if !self.is_sync_site(expr.unwrap_parens()) {
            return None;
        }
        self.synced.window_depth += 1;
        Some(SyncWindow(()))
    }

    /// The [`UserCodeCall`] class of `inner` (paren-unwrapped).
    pub(in crate::core_erlang) fn classify_expr(&self, inner: &Expression) -> UserCodeCall {
        match inner {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                is_cast,
                ..
            } => self.classify_send(receiver, selector, arguments, *is_cast),
            Expression::Cascade { receiver, .. } => {
                // The cascade's receiver is its folded first send; the shared
                // receiver is that send's own receiver.
                if super::expr_shape::is_self_identifier(
                    beamtalk_core::ast_walker::cascade_shared_receiver(receiver),
                ) {
                    UserCodeCall::No
                } else {
                    UserCodeCall::Yes
                }
            }
            _ => UserCodeCall::No,
        }
    }

    /// Whether `inner` (paren-unwrapped) is a call [`Self::threaded_expression`]
    /// brackets: the flag is on, it is not already sequenced ahead, and it
    /// may run user code. The pure predicate behind the window and behind
    /// `subexpr_needs_prelude`'s mirror of it.
    pub(in crate::core_erlang) fn is_sync_site(&self, inner: &Expression) -> bool {
        if !self.synced_calls_active() || self.in_cell_closure() {
            return false;
        }
        #[cfg(test)]
        if FAULT_NO_BRACKETS.with(std::cell::Cell::get) {
            return false;
        }
        if self.precompiled_subexprs_contains(inner) {
            return false;
        }
        // A `super` send threads its own `State` (`generate_super_send` stores
        // for itself when it may reach the default arm).
        if matches!(inner, Expression::MessageSend { receiver, .. }
            if matches!(receiver.unwrap_parens(), Expression::Super(_)))
            && !self.super_send_uses_cell()
        {
            return false;
        }
        let class = self.classify_expr(inner);
        // A self-send's own prelude carries its store (`self_dispatch_statement`).
        !matches!(
            class,
            UserCodeCall::No | UserCodeCall::CompiledSelfSend | UserCodeCall::SelfViaDefaultArm
        )
    }

    /// Closes a window opened by [`Self::open_sync_window`].
    pub(in crate::core_erlang) fn close_sync_window(&mut self, window: SyncWindow) {
        let SyncWindow(()) = window;
        self.synced.window_depth -= 1;
    }

    /// Core-var rebinds for a merged Tier 2 call's captured locals
    /// (BT-4122): `(core var, state key)` pairs, resolved against the
    /// current bindings like the statement unpacker's rebinds (a shadowed
    /// local rebinds its live Core name, not the deterministic one). The
    /// bracket renders them after its reload, reading each fresh value
    /// out of the reloaded state. Empty when the prescan recorded no
    /// mutations (defensive: the merge predicate already proved this is
    /// a may-write Tier 2 call, so the merge still threads its state).
    pub(in crate::core_erlang) fn tier2_merge_rebinds(
        &mut self,
        expr: &Expression,
    ) -> Vec<(String, String)> {
        // Unwrap: the operand may be paren-wrapped (`(b value) + ...`),
        // and the prescan lookup matches sends, not parens.
        let Some(mutations) = self.get_inline_block_captured_mutations(expr.unwrap_parens()) else {
            return Vec::new();
        };
        mutations
            .iter()
            .map(|var| {
                let core_var = self
                    .lookup_var(var)
                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                (core_var, Self::local_state_key(var))
            })
            .collect()
    }

    /// Builds the [`ThreadedStmt::SyncedCall`] around the call `tv` ends in
    /// (`expr` compiled in a window): store the current `State`, make the
    /// call, reload the next version. A value that is not a call document
    /// (its call already ran in the prelude, outside any node this function
    /// can wrap) is a gap.
    pub(in crate::core_erlang) fn synced_call_value(
        &mut self,
        tv: ThreadedValue,
        expr: &Expression,
        frame: FrameId,
        version_before: usize,
    ) -> ThreadedValue {
        let span = expr.unwrap_parens().span();
        let ValueRef::Doc(call) = tv.value else {
            self.record_gap(Self::send_label(expr), span);
            return tv;
        };
        if self.loop_holds_no_whole_state() {
            // The loop's `fun` binds no whole `State` to store (direct
            // params or a tuple accumulator), or holds fields in
            // pre-extracted parameters (hybrid): a call that may run user
            // code cannot bracket here, so the loop has to thread one (its
            // owning node's job). Except for a guarded operator, whose slow
            // path is not worth a threaded loop (the block analysis does not
            // count it, see `send_may_run_user_code`): it flags its loop's
            // marker and the loop brackets around it (BT-4121).
            if self.classify_expr(expr.unwrap_parens()) == UserCodeCall::GuardedOperator {
                self.note_stateless_guarded_op(Self::send_label(expr), span);
            } else {
                self.record_gap(Self::send_label(expr), span);
            }
            return ThreadedValue {
                prelude: tv.prelude,
                value: ValueRef::Doc(call),
            };
        }
        // BT-4122: a may-write Tier 2 call merges here — the same
        // predicate its `close_tier2_value_subexpr_doc` read, so the wrap
        // unpacks exactly the calls the close handed a raw tuple.
        let tier2_merged =
            Self::tier2_merge_call_name(expr).is_some_and(|name| self.tier2_operand_merges(name));
        let mut call = call;
        let tier2_local_rebinds = if tier2_merged {
            // The apply answered `{Result, NewState}`: the bracket's value
            // is element 1 (element 2 went to the cell at the fun's exit,
            // and the reload below reads it back).
            let tuple_var = self.fresh_temp_var("T2Merge");
            call = docvec![
                "let ",
                leaf::var(tuple_var.clone()),
                " = ",
                call,
                " in call 'erlang':'element'(1, ",
                leaf::var(tuple_var),
                ")",
            ];
            self.tier2_merge_rebinds(expr)
        } else {
            Vec::new()
        };
        let mut prelude = tv.prelude;
        // The `State` the call runs against is the one the prelude left, not the
        // counter: the call's own operands may have minted versions inside a
        // closed document (a generic dispatch closes its operands' brackets into
        // the call), and those are not in scope at the store.
        let stored_version = prelude
            .iter()
            .rev()
            .find_map(|stmt| {
                // A `Bind`, a bracket's reload, or a local rebind that steps the
                // frame's `State` map (`MapPut`).
                let target = match stmt {
                    ThreadedStmt::LocalRebind { lowering, .. } => {
                        lowering.version_step().map(|(_, target)| target)
                    }
                    other => other.version_step().map(|(target, _, _)| target),
                };
                target
                    .filter(|t| t.prefix == VersionPrefix::State && t.frame == frame)
                    .map(|t| t.version)
            })
            .unwrap_or(version_before);
        let store = VersionedVar::new(VersionPrefix::State, stored_version, frame);
        self.next_state_var();
        let load = VersionedVar::new(VersionPrefix::State, self.state_version(), frame);
        let result = self.fresh_temp_var("Synced");
        prelude.push(ThreadedStmt::SyncedCall {
            store,
            call,
            result: Some(result.clone()),
            load: Some(load),
            span,
            tier2_local_rebinds,
            on_private_scratch: self.in_private_scratch,
        });
        ThreadedValue {
            prelude,
            value: ValueRef::Var(result),
        }
    }

    /// Whether the loop being compiled has no whole `State` to store: a
    /// direct-params loop binds none, a hybrid loop holds fields in
    /// pre-extracted parameters and a tuple-accumulator fold carries only locals.
    pub(in crate::core_erlang) fn loop_holds_no_whole_state(&self) -> bool {
        use super::threaded_ir::{RebindFrameKind, ThreadingMode};
        self.in_private_scratch
            || (self.loop_mode.in_direct_params_loop && !self.loop_mode.in_hybrid_loop)
            || !self.loop_mode.hybrid_readonly_field_params.is_empty()
            || !self.loop_mode.hybrid_mutated_fields.is_empty()
            // A loop frame whose parameters or accumulator carry only locals.
            || matches!(
                self.rebind_frames.get(&self.current_frame()).map(|f| &f.kind),
                Some(RebindFrameKind::Loop(
                    ThreadingMode::TupleAcc(_) | ThreadingMode::DirectParams | ThreadingMode::Hybrid
                ))
            )
    }

    /// The selector (or `cascade`) naming `expr` in a gap report.
    fn send_label(expr: &Expression) -> String {
        match expr.unwrap_parens() {
            Expression::MessageSend { selector, .. } => selector.name().to_string(),
            Expression::Cascade { .. } => "cascade".to_string(),
            _ => "expression".to_string(),
        }
    }
}

/// BT-4121: which exit a stateless loop lowering takes for its popped
/// [`GuardedOpLoopMarker`] (decided by
/// [`CoreErlangGenerator::checkout_guarded_op_loop`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum GuardedOpCheckout {
    /// Nothing to do: the loop holds no guarded operator (or the operator
    /// compiled elsewhere). Emit the loop exactly as before.
    Nothing,
    /// Bracket the loop: seed its exit packs from the cell (`take()`) and
    /// store the loop-entry `State` before the loop. The fast path is
    /// untouched; the slow path runs against a current cell and the exit
    /// pack loads its writes back.
    Bracket,
}

impl CoreErlangGenerator {
    /// BT-4121: pushes a guarded-operator marker for the loop lowering
    /// starting now. Call after the loop's method-level setup (receiver,
    /// bounds) is lowered and before its condition and body are: the walk
    /// below must see the same codegen facts the send emitter will, and the
    /// pre-store name the caller captures alongside must be the
    /// loop-entry `State`.
    ///
    /// `will_bracket` is decided here, from the same [`Self::classify_binary`]
    /// the send emitter exempts, so the two cannot disagree; the `need` half
    /// is set while the body lowers. Under [`FAULT_NO_BRACKETS`] no marker
    /// is pushed, so the invariant probe sees every guarded-operator gap.
    pub(in crate::core_erlang) fn push_guarded_op_loop(
        &mut self,
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
    ) {
        #[cfg(test)]
        if FAULT_NO_BRACKETS.with(std::cell::Cell::get) {
            return;
        }
        let will_bracket = self.synced_calls_active()
            && (self.block_contains_guarded_operator(body)
                || condition.is_some_and(|c| self.expr_contains_guarded_operator(c)));
        let entry_state = self.current_field_read_state_var();
        self.synced
            .guarded_op_loops
            .borrow_mut()
            .push(GuardedOpLoopMarker {
                will_bracket,
                need: None,
                entry_state,
                state_uses: Vec::new(),
            });
    }

    /// BT-4121: pops the marker [`Self::push_guarded_op_loop`] pushed, after
    /// the loop's body (and condition) is lowered. The caller passes it to
    /// [`Self::checkout_guarded_op_loop`] with the loop's mode. Takes `&self`
    /// (the stack is a [`RefCell`]) so a checkout nests the pop in its own
    /// call.
    pub(in crate::core_erlang) fn pop_guarded_op_loop(&self) -> GuardedOpLoopMarker {
        #[cfg(test)]
        if FAULT_NO_BRACKETS.with(std::cell::Cell::get) {
            return GuardedOpLoopMarker {
                will_bracket: false,
                need: None,
                entry_state: String::new(),
                state_uses: Vec::new(),
            };
        }
        self.synced
            .guarded_op_loops
            .borrow_mut()
            .pop()
            .unwrap_or(GuardedOpLoopMarker {
                will_bracket: false,
                need: None,
                entry_state: String::new(),
                state_uses: Vec::new(),
            })
    }

    /// BT-4121: checks out `marker` for a loop whose parameters hold no
    /// whole `State` (tuple-accumulator, direct-params, or a hybrid loop,
    /// which cannot bracket and reports loud instead — `hybrid`).
    ///
    /// A loop nested in another stateless loop's fun has no `State` to
    /// store, so it hands its `need` to the enclosing loop, whose own walk
    /// saw the same operators and whose bracket keeps the cell current for
    /// both. Anything else that cannot bracket —
    /// [`GuardedOpCheckout::Nothing`] is not returned for it — reports a
    /// [`VerifyError::UnbracketedGuardedOperator`], never silently.
    ///
    /// BT-4179: the marker's `state_uses` are checked against the decision.
    /// Where the loop brackets, a use of its entry `State` reads a version
    /// the bracket's slow paths have overtaken in the cell (an `on:do:` that
    /// stores it, a `StateAcc` seeded from it, both drop those writes), and is
    /// reported; a use of any other `State` was bound inside the loop, from
    /// the cell. Where the loop does not bracket, the uses pass to the
    /// enclosing marker, whose bracket (if any) covers them too.
    ///
    /// BT-4180: the same uses carry every actor-state read that did not take
    /// the cell ([`StaleStateUseKind::Read`], recorded by
    /// [`Self::actor_state_read_source`]); under a bracket each one is
    /// reported, whatever variable it read, as
    /// [`VerifyError::StaleReadInBracketedLoop`]. The outcome is this
    /// checkout's, so a drift between the push-time prediction
    /// ([`Self::guarded_op_take_reads`]) and the decision is caught too.
    pub(in crate::core_erlang) fn checkout_guarded_op_loop(
        &mut self,
        mut marker: GuardedOpLoopMarker,
        hybrid: bool,
        at: Span,
    ) -> GuardedOpCheckout {
        let state_uses = std::mem::take(&mut marker.state_uses);
        let entry_state = std::mem::take(&mut marker.entry_state);
        let checkout = self.checkout_guarded_op_loop_decision(marker, hybrid, at);
        if checkout == GuardedOpCheckout::Bracket {
            for state_use in state_uses
                .into_iter()
                .filter(|u| u.kind.is_stale_under_bracket(&u.var, &entry_state))
            {
                let gap =
                    self.make_unbracketed_send(state_use.kind.label().to_string(), state_use.at);
                self.synced.stale_state_gaps.push((state_use.kind, gap));
            }
        } else if let Some(outer) = self.synced.guarded_op_loops.borrow_mut().last_mut() {
            outer.state_uses.extend(state_uses);
        }
        checkout
    }

    /// BT-4179: notes a use of the threaded `State` variable `var` at `at` on
    /// the innermost loop marker, for its checkout to check
    /// ([`Self::checkout_guarded_op_loop`]). Outside every loop marker there
    /// is no bracket to overtake that `State`.
    pub(in crate::core_erlang) fn note_threaded_state_use(
        &self,
        kind: StaleStateUseKind,
        var: &str,
        at: Span,
    ) {
        if let Some(marker) = self.synced.guarded_op_loops.borrow_mut().last_mut() {
            marker.state_uses.push(ThreadedStateUse {
                kind,
                var: var.to_string(),
                at,
            });
        }
    }

    /// BT-4180: the one choke point every actor-state read site asks for its
    /// source (ADR 0137 §4b). Answers [`ActorStateSource::Cell`] inside a
    /// stateless loop that brackets guarded operators, or nested in one
    /// ([`Self::guarded_op_take_reads`], the emitter's decision); else, for
    /// a read of `field` in a hybrid loop that pre-extracted it, that
    /// parameter. `None` leaves the source to the site's own context rule,
    /// which for an actor instance method is [`Self::actor_state_read_var`].
    ///
    /// A source other than the cell is recorded on the innermost loop
    /// marker (when the frame holds no whole `State`), so the checkout of a
    /// loop that brackets reports it from its own decision
    /// ([`VerifyError::StaleReadInBracketedLoop`]). A site must not call
    /// `current_field_read_state_var`, `guarded_op_take_reads` or the
    /// hybrid parameter map itself (`tests/stale_read_sites.rs` checks it).
    pub(in crate::core_erlang) fn actor_state_read_source(
        &self,
        site: StateReadSite,
        field: Option<&str>,
        at: Span,
    ) -> Option<ActorStateSource> {
        #[cfg(test)]
        let faulted = FAULT_ENTRY_STATE_READS.with(std::cell::Cell::get);
        #[cfg(not(test))]
        let faulted = false;
        if self.guarded_op_take_reads() && !faulted {
            return Some(ActorStateSource::Cell);
        }
        if let Some(field) = field
            && self.loop_mode.in_hybrid_loop
            && let Some(param) = self.loop_mode.hybrid_readonly_field_params.get(field)
        {
            self.note_state_read(site, StaleReadDefect::PreExtractedParam, param, at);
            return Some(ActorStateSource::Var(param.clone()));
        }
        None
    }

    /// BT-4180: the threaded `State` variable an actor-state read at `site`
    /// reads when [`Self::actor_state_read_source`] left it to the context
    /// (`current_field_read_state_var`), recorded like every other source
    /// that is not the cell.
    pub(in crate::core_erlang) fn actor_state_read_var(
        &self,
        site: StateReadSite,
        at: Span,
    ) -> String {
        let var = self.current_field_read_state_var();
        self.note_state_read(site, StaleReadDefect::ThreadedState, &var, at);
        var
    }

    /// BT-4180: [`Self::actor_state_read_source`] for a read of no
    /// particular field, falling back to [`Self::actor_state_read_var`].
    pub(in crate::core_erlang) fn actor_state_source(
        &self,
        site: StateReadSite,
        at: Span,
    ) -> ActorStateSource {
        self.actor_state_read_source(site, None, at)
            .unwrap_or_else(|| ActorStateSource::Var(self.actor_state_read_var(site, at)))
    }

    /// BT-4180: the `State` in scope for a site that stores it rather than
    /// reads from it, recording nothing: a loop bracket's pre-store (it runs
    /// at region entry, before any slow path), an `on:do:` boundary's
    /// pre-region `State` (noted where the boundary picks `PreState`,
    /// [`Self::on_do_cell_boundary`]) and a loop plan's initial state (noted
    /// where a lowering emits it as a source). Its callers are counted per
    /// file by `tests/stale_read_sites.rs` (`STORE_SITES`, BT-4256), so a
    /// read site cannot quietly use it to skip the record.
    pub(in crate::core_erlang) fn state_var_to_store(&self) -> String {
        self.current_field_read_state_var()
    }

    /// BT-4180: records a read source that is not the cell on the innermost
    /// loop marker. A read in a frame that holds a whole `State` (a
    /// `StateAcc` loop or fold nested in the region) records nothing: that
    /// frame's state was seeded from the cell, which the seed's own note
    /// ([`StaleStateUseKind::StateAccSeed`]) checks.
    pub(in crate::core_erlang) fn note_state_read(
        &self,
        site: StateReadSite,
        defect: StaleReadDefect,
        var: &str,
        at: Span,
    ) {
        if self.loop_holds_no_whole_state() {
            let region = self.user_code_region();
            self.note_threaded_state_use(
                StaleStateUseKind::Read {
                    site,
                    defect,
                    region,
                },
                var,
                at,
            );
        }
    }

    /// BT-4179: whether a construct lowered right now must take the actor's
    /// state from the cell rather than the threaded `State` in scope: inside
    /// a stateless loop that brackets guarded operators, whose slow paths
    /// write the cell past that `State` ([`Self::guarded_op_take_reads`]).
    pub(in crate::core_erlang) fn state_seeds_from_cell(&self) -> bool {
        #[cfg(test)]
        if FAULT_STALE_STATE_IN_BRACKETED_LOOP.with(std::cell::Cell::get) {
            return false;
        }
        self.guarded_op_take_reads()
    }

    /// The bracket decision of [`Self::checkout_guarded_op_loop`].
    fn checkout_guarded_op_loop_decision(
        &mut self,
        marker: GuardedOpLoopMarker,
        hybrid: bool,
        at: Span,
    ) -> GuardedOpCheckout {
        let GuardedOpLoopMarker {
            will_bracket, need, ..
        } = marker;
        if !will_bracket && need.is_none() {
            return GuardedOpCheckout::Nothing;
        }
        if hybrid {
            // A hybrid loop holds fields in pre-extracted parameters that
            // advance past the entry `State` every iteration, so a store of
            // the entry would be stale the moment a param advances. The mode
            // selection routes a hybrid body with a guarded operator to
            // `StateAcc`; reaching here means it missed, which is loud.
            let (selector, at) = need.map_or_else(
                || ("guarded operator".to_string(), at),
                |n| (n.selector, n.at),
            );
            self.record_guarded_op_gap(selector, at);
            return GuardedOpCheckout::Nothing;
        }
        if !will_bracket {
            // An operator fired that the push-time walk missed (`need` is
            // set: the early return above covered the unset case). The walk
            // reads the same classifier over a superset of the positions
            // the lowering inlines, so this is unreachable unless the two
            // drift; field reads already lowered against the entry `State`
            // cannot be covered after the fact, so it is loud.
            if let Some(need) = need {
                self.record_guarded_op_gap(need.selector, need.at);
            }
            return GuardedOpCheckout::Nothing;
        }
        if self.loop_holds_no_whole_state() {
            // Nested in another stateless loop's fun: no `State` in scope
            // to store. Hand the need outward (the outer loop's own walk saw
            // the same operators); with no enclosing marker the shape is
            // loud. The exit packs still seed from the cell, which the
            // outer bracket keeps current.
            if self.synced.guarded_op_loops.borrow().is_empty() {
                let (selector, at) = need.map_or_else(
                    || ("guarded operator".to_string(), at),
                    |n| (n.selector, n.at),
                );
                self.record_guarded_op_gap(selector, at);
                return GuardedOpCheckout::Nothing;
            }
            if let Some(need) = need {
                self.propagate_guarded_op_need(need);
            }
            return GuardedOpCheckout::Bracket;
        }
        GuardedOpCheckout::Bracket
    }

    /// BT-4121: checks out `marker` for a loop lowering that assembles
    /// documents (every in-place loop): the [`Self::checkout_guarded_op_loop`]
    /// decision plus the pre-store document to splice before the loop.
    /// Answers `(seed_from_cell, prestore)`: `seed_from_cell` seeds the
    /// loop's exit packs from the cell, and `prestore` is the
    /// `let _ = store(<loop-entry State>) in ` prefix — [`Document::Nil`]
    /// when the loop needs no store (identical code, hybrid (loud), or
    /// nested stateless (the need propagated outward)), so the caller
    /// splices it unconditionally.
    ///
    /// BT-4180: a loop that does not bracket itself but sits in an enclosing
    /// bracketed region (the read choke point answers the cell,
    /// [`Self::actor_state_read_source`]) seeds its exit packs
    /// from the cell too: its plan's initial state is the region's entry
    /// `State`, which the region's slow paths have overtaken.
    pub(in crate::core_erlang) fn checkout_guarded_op_loop_docs(
        &mut self,
        marker: GuardedOpLoopMarker,
        hybrid: bool,
        at: Span,
        prestore_state: &str,
    ) -> (bool, Document<'static>) {
        let bracket =
            self.checkout_guarded_op_loop(marker, hybrid, at) == GuardedOpCheckout::Bracket;
        if bracket && !self.loop_holds_no_whole_state() {
            let prestore = docvec![
                "let _ = ",
                super::threaded_ir::cell_put_call(leaf::var(prestore_state.to_string())),
                " in ",
            ];
            (true, prestore)
        } else {
            let in_region = matches!(
                self.actor_state_read_source(StateReadSite::LoopPlanInitialState, None, at),
                Some(ActorStateSource::Cell)
            );
            (bracket || in_region, Document::Nil)
        }
    }

    /// BT-4121: flags the innermost loop marker for the guarded operator
    /// `selector` at `at`, compiled in a stateless loop body. With no
    /// marker the operator is covered by an enclosing bracket window (a
    /// closure or evaluated-block call the classifier wrapped), and loud
    /// otherwise.
    pub(in crate::core_erlang) fn note_stateless_guarded_op(&mut self, selector: String, at: Span) {
        let mut stack = self.synced.guarded_op_loops.borrow_mut();
        if let Some(marker) = stack.last_mut() {
            if marker.need.is_none() {
                marker.need = Some(GuardedOpNeed { selector, at });
            }
            return;
        }
        drop(stack);
        if self.synced.window_depth == 0 {
            self.record_guarded_op_gap(selector, at);
        }
    }

    /// BT-4121: hands `need` to the next enclosing loop marker (a nested
    /// stateless loop's checkout, which has no `State` to store itself).
    /// With no enclosing marker the shape is loud.
    fn propagate_guarded_op_need(&mut self, need: GuardedOpNeed) {
        let mut stack = self.synced.guarded_op_loops.borrow_mut();
        if let Some(marker) = stack.last_mut() {
            if marker.need.is_none() {
                marker.need = Some(need);
            }
            return;
        }
        drop(stack);
        self.record_guarded_op_gap(need.selector, need.at);
    }

    /// BT-4121: whether a field read lowered right now must read the live
    /// cell (`maps:get` over `take()`) instead of the loop-entry `State`.
    /// True inside a stateless loop that will bracket, or nested in one
    /// that will: any bracketing loop's slow paths write the cell, so the
    /// entry `State` (or a parameter extracted from it) is stale for every
    /// read in the iteration — including reads in a nested loop with no
    /// guarded operator of its own. Never inside a cell closure nested in
    /// the loop: its reads are already live-at-home through the cell view
    /// (and must stay capture-aware away from home).
    pub(in crate::core_erlang) fn guarded_op_take_reads(&self) -> bool {
        self.synced
            .guarded_op_loops
            .borrow()
            .iter()
            .any(|m| m.will_bracket)
            && self.loop_holds_no_whole_state()
            && !self.in_cell_closure()
    }

    /// BT-4121: whether `expr` holds a binary send that compiles to a
    /// guarded operator — read off the same [`Self::classify_binary`] the
    /// send emitter exempts, over every syntactic position (nested blocks
    /// included, whether the lowering inlines them or not), so a position
    /// the lowering inlines is never missed. A send the lowering does not
    /// inline only brackets a loop that needs none, which is harmless.
    pub(in crate::core_erlang) fn expr_contains_guarded_operator(&self, expr: &Expression) -> bool {
        let mut found = false;
        beamtalk_core::ast_walker::walk_expression(expr, &mut |e| {
            if found {
                return;
            }
            if let Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } = e
                && let MessageSelector::Binary(op) = selector
            {
                found |=
                    self.classify_binary(receiver, op, arguments) == UserCodeCall::GuardedOperator;
            }
        });
        found
    }

    /// BT-4121: whether any statement of `block` holds a guarded operator
    /// ([`Self::expr_contains_guarded_operator`]).
    pub(in crate::core_erlang) fn block_contains_guarded_operator(
        &self,
        block: &beamtalk_core::ast::Block,
    ) -> bool {
        block
            .body
            .iter()
            .any(|s| self.expr_contains_guarded_operator(&s.expression))
    }

    /// BT-4255: whether `expr` holds a send this classifier brackets
    /// ([`Self::classify_send`] answers [`UserCodeCall::Yes`] or
    /// [`UserCodeCall::SelfViaDefaultArm`]), cascades and nested blocks
    /// included.
    ///
    /// Loop mode selection asks this, with the block analysis, before it
    /// lets a loop drop its `State`: such a send compiled in a loop that holds
    /// no whole `State` has nothing to bracket it with
    /// ([`VerifyError::UnbracketedUserCodeSend`]). The block analysis alone
    /// misses the sends whose class only codegen decides, such as a loop or
    /// evaluated literal block that carries nothing and so becomes a runtime
    /// call with its blocks as closures ([`SendUserCode::LoopOrClosureCall`]).
    ///
    /// A probe, not a decision: it runs before the loop's body context is set
    /// up, so the per-send class [`Self::classify_inlined`] caches (for the
    /// window, the gap note and the lowering to agree on) is left as it was.
    pub(in crate::core_erlang) fn expr_holds_bracketed_call(&self, expr: &Expression) -> bool {
        let cached = self.synced.inlined_classes.borrow().clone();
        let mut found = false;
        beamtalk_core::ast_walker::walk_sends(expr, &mut |send| {
            found = found
                || matches!(
                    self.classify_send(send.receiver, send.selector, send.arguments, send.is_cast),
                    UserCodeCall::Yes | UserCodeCall::SelfViaDefaultArm
                );
        });
        *self.synced.inlined_classes.borrow_mut() = cached;
        found
    }

    /// BT-4255: whether the loop with `body` (and `condition`, for a `while`
    /// loop) holds a bracketed call ([`Self::expr_holds_bracketed_call`]), the
    /// question loop mode selection asks before it lets the loop drop its
    /// `State`.
    ///
    /// BT-4269: the loop has not bound its own names yet, so an inner closure
    /// reading the loop's block parameter or a temp its body assigns would
    /// look like a bare field read. Those names are probe locals for the
    /// duration ([`Self::loop_bound_names`]).
    pub(in crate::core_erlang) fn loop_holds_bracketed_call(
        &self,
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
    ) -> bool {
        let names = self.loop_bound_names(body, condition);
        let saved = std::mem::replace(&mut *self.synced.probe_locals.borrow_mut(), names);
        let found = body
            .body
            .iter()
            .any(|s| self.expr_holds_bracketed_call(&s.expression))
            || condition.is_some_and(|c| self.expr_holds_bracketed_call(c));
        *self.synced.probe_locals.borrow_mut() = saved;
        found
    }

    /// BT-4269: the names the loop with `body` and `condition` binds itself:
    /// its block parameters, and the temps its body (or condition block)
    /// assigns that are not fields of the class (a bare field name is a field
    /// read, so it stays one). Without the class hierarchy no temp can be
    /// told from a field, so only the parameters count.
    fn loop_bound_names(
        &self,
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
    ) -> Vec<String> {
        use beamtalk_core::semantic_analysis::block_facts as block_analysis;
        let mut names: Vec<String> = body.parameters.iter().map(|p| p.name.to_string()).collect();
        if let Some(hierarchy) = self.class_hierarchy.as_ref() {
            let fields = hierarchy.all_state(&self.class_name());
            let blocks = std::iter::once(body).chain(condition.and_then(|c| match c {
                Expression::Block(block) => Some(block),
                _ => None,
            }));
            for block in blocks {
                names.extend(
                    block_analysis::analyze_block(block)
                        .local_writes
                        .into_iter()
                        .filter(|name| !fields.iter().any(|f| f.as_str() == name)),
                );
            }
        }
        names
    }
}
