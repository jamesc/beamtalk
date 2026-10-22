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
    FrameId, ThreadedStmt, ThreadedValue, UserCodeRegion, ValueRef, VerifyError, VersionPrefix,
    VersionedVar,
};
use super::{CodeGenContext, CoreErlangGenerator};
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
        let path = at.rsplitn(3, ':').last().unwrap_or(at);
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
    /// The class of a conditional with a `super` send in a branch
    /// ([`CoreErlangGenerator::classify_inlined`]), decided once per send.
    pub(in crate::core_erlang) inlined_classes:
        std::cell::RefCell<std::collections::HashMap<Span, UserCodeCall>>,
}

impl SyncedCallState {
    pub(in crate::core_erlang) fn new() -> Self {
        Self {
            window_depth: 0,
            gaps: Vec::new(),
            inlined_classes: std::cell::RefCell::default(),
        }
    }
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

    /// Whether the code being compiled is inside a Tier 1 closure body. Such
    /// a body holds no threaded `State`: it reads the cell directly (ADR 0137
    /// §3), so a call in it has nothing to store or reload.
    fn in_cell_resident_closure(&self) -> bool {
        self.in_cell_closure()
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
                if arguments.iter().any(|a| match a.unwrap_parens() {
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
    /// (ADR 0137 §4): the receiver's inferred class is a sealed stdlib class whose
    /// method for `selector` is a primitive the stdlib marked `no_user_code`.
    /// Sealed classes cannot be subclassed, so no override can run user code; the
    /// build-stdlib check guarantees every argument is a primitive type.
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
            .filter_map(|a| match a.unwrap_parens() {
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
            .filter_map(|e| match e.unwrap_parens() {
                Expression::Block(block) => Some(block),
                _ => None,
            })
            .collect();
        let carried = blocks
            .iter()
            .any(|block| self.loop_body_has_carried_mutations(block))
            || matches!(receiver, Expression::Block(_))
                && arguments.iter().any(|a| match a.unwrap_parens() {
                    Expression::Block(body) => {
                        self.while_condition_has_carried_effects(receiver, Some(body))
                    }
                    _ => false,
                });
        if carried {
            UserCodeCall::No
        } else if blocks
            .iter()
            .any(|block| self.block_touches_actor_state(block))
        {
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
        if !self.synced_calls_active() || self.in_cell_resident_closure() {
            return;
        }
        // A send to `self` is noted where its dispatch is built
        // ([`Self::note_self_dispatch`]), once, whichever path reaches it.
        if super::expr_shape::is_self_identifier(receiver.unwrap_parens())
            || matches!(receiver.unwrap_parens(), Expression::Super(_))
        {
            return;
        }
        let class = self.classify_send(receiver, selector, arguments, is_cast);
        if matches!(class, UserCodeCall::No | UserCodeCall::CompiledSelfSend) {
            return;
        }
        // A guarded operator's slow path runs unbracketed in a loop that holds
        // no whole `State` ([`Self::synced_call_value`]).
        if class == UserCodeCall::GuardedOperator && self.loop_holds_no_whole_state() {
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
            || self.in_cell_resident_closure()
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
        if !self.synced_calls_active() || self.in_cell_resident_closure() {
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
            || self.in_cell_resident_closure()
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
        };
        let mut ctx = super::threaded_ir::RenderCtx::new(self);
        super::threaded_ir::render(&[node], &mut ctx)
    }

    /// Records one gap.
    pub(in crate::core_erlang) fn record_gap(&mut self, selector: String, at: Span) {
        let region = self.user_code_region();
        #[cfg(test)]
        let site = if COUNT_ONLY.with(std::cell::Cell::get) {
            capture_site()
        } else {
            GapSite::default()
        };
        #[cfg(not(test))]
        let site = GapSite::default();
        self.synced.gaps.push(UnbracketedSend {
            selector,
            region,
            at,
            site,
        });
    }

    /// Reports the gaps recorded while compiling the module as verifier
    /// findings (or, in count-only test mode, logs them for the probe).
    pub(in crate::core_erlang) fn finish_synced_calls(&mut self, module_span: Span) {
        let gaps = std::mem::take(&mut self.synced.gaps);
        #[cfg(test)]
        if COUNT_ONLY.with(std::cell::Cell::get) {
            SITE_LOG.with(|log| {
                log.borrow_mut().extend(gaps.iter().map(|g| SiteRecord {
                    site: g.site.site.clone(),
                    chain: g.site.chain.clone(),
                    selector: g.selector.clone(),
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
        if !self.synced_calls_active() || self.in_cell_resident_closure() {
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
            // params), or holds fields in pre-extracted parameters (hybrid):
            // the loop has to thread one (its owning node's job). Except for
            // a guarded operator, whose slow path is not worth a threaded
            // loop (the block analysis does not count it, see
            // `send_may_run_user_code`): it runs unbracketed here.
            if self.classify_expr(expr.unwrap_parens()) != UserCodeCall::GuardedOperator {
                self.record_gap(Self::send_label(expr), span);
            }
            return ThreadedValue {
                prelude: tv.prelude,
                value: ValueRef::Doc(call),
            };
        }
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
        });
        ThreadedValue {
            prelude,
            value: ValueRef::Var(result),
        }
    }

    /// Whether the loop being compiled has no whole `State` to store: a
    /// direct-params loop binds none, a hybrid loop holds fields in
    /// pre-extracted parameters and a tuple-accumulator fold carries only locals.
    fn loop_holds_no_whole_state(&self) -> bool {
        use super::threaded_ir::{RebindFrameKind, ThreadingMode};
        (self.loop_mode.in_direct_params_loop && !self.loop_mode.in_hybrid_loop)
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
