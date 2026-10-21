// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0137 Phase 2a (BT-4020): the cell bracket around calls that may run
//! user code, and the check that none is missing.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Everything here is behind a codegen flag that is **off by default**; with
//! it off no state is recorded and no output changes. With it on, an actor
//! instance method brackets every call that may run one of this actor's
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
//! ## Modes
//!
//! `BEAMTALK_SYNCED_CALLS` selects the mode (`beamtalk_core::synced_calls`):
//! unset or `0` is [`SyncMode::Off`]; `count` records gaps without failing,
//! appending one line per gap to the file `BEAMTALK_SYNCED_CALLS_REPORT`
//! names (the count-by-region tool for BT-4020); anything else is
//! [`SyncMode::Enforce`], where a gap is a verifier finding (a hard failure
//! in debug builds).

use super::threaded_ir::{
    FrameId, ThreadedStmt, ThreadedValue, UserCodeRegion, ValueRef, VerifyError, VersionPrefix,
    VersionedVar,
};
use super::{CodeGenContext, CoreErlangGenerator};
use beamtalk_cerl_doc::Document;
use beamtalk_core::ast::{Expression, MessageSelector};
use beamtalk_core::source_analysis::Span;
use beamtalk_core::state_threading_selectors::{SendUserCode, send_user_code};

/// Environment variable naming the file `count` mode appends its report to.
pub const SYNCED_CALLS_REPORT_ENV: &str = "BEAMTALK_SYNCED_CALLS_REPORT";

/// Whether and how the bracket is built (`beamtalk_core::synced_calls`).
pub(in crate::core_erlang) use beamtalk_core::synced_calls::SyncedCallsMode as SyncMode;

/// A call that may run user code, compiled outside every bracket.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct UnbracketedSend {
    pub(in crate::core_erlang) selector: String,
    pub(in crate::core_erlang) region: UserCodeRegion,
    pub(in crate::core_erlang) at: Span,
}

/// The generator's bracket bookkeeping.
#[derive(Debug)]
pub(in crate::core_erlang) struct SyncedCallState {
    pub(in crate::core_erlang) mode: SyncMode,
    /// How many bracket windows enclose the code being compiled. A call
    /// compiled inside one runs between a store and a reload.
    pub(in crate::core_erlang) window_depth: usize,
    /// Calls that may run user code compiled outside every window.
    pub(in crate::core_erlang) gaps: Vec<UnbracketedSend>,
    /// Brackets built (for the report's denominator).
    pub(in crate::core_erlang) brackets: usize,
    /// Count mode: other verifier findings the flag-on lowering tripped,
    /// as `label\tkind\tposition` report lines.
    pub(in crate::core_erlang) other_findings: Vec<String>,
    /// Self-sends that may reach `dispatch/4`'s default arm (bracketed or not).
    pub(in crate::core_erlang) default_arm_sends: usize,
}

impl SyncedCallState {
    pub(in crate::core_erlang) fn new(mode: SyncMode) -> Self {
        Self {
            mode,
            window_depth: 0,
            gaps: Vec::new(),
            brackets: 0,
            other_findings: Vec::new(),
            default_arm_sends: 0,
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
    /// compiled: the flag is on and this is an actor instance method (a
    /// class method and a value type thread no actor `State`; the REPL has
    /// none).
    pub(in crate::core_erlang) fn synced_calls_active(&self) -> bool {
        self.synced.mode != SyncMode::Off
            && self.context == CodeGenContext::Actor
            && !self.in_class_method()
            && !self.is_repl_mode()
            // Outside a method body (a `state:` default, built while the state
            // map itself is being made) there is no `State` to store.
            && self.current_method_selector.is_some()
    }

    /// Whether the code being compiled is inside a Tier 1 closure body. Such
    /// a body holds no threaded `State`: it reads the cell directly (ADR 0137
    /// §3), so a call in it has nothing to store or reload.
    fn in_cell_resident_closure(&self) -> bool {
        self.block_depth > 0 && self.closure_drops_state
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

    /// A `super` send resolves from the superclass: a compiled actor method
    /// there is explicit threading, anything else (an `Object` method, a
    /// native one, DNU) may reach `dispatch/4`'s default arm.
    fn classify_super_send(&self, selector: &MessageSelector) -> UserCodeCall {
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
                    hierarchy.get_class(defined_in).is_some_and(|c| !c.is_native)
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
        match send_user_code(receiver, selector, arguments) {
            SendUserCode::Never | SendUserCode::Inlined => UserCodeCall::No,
            SendUserCode::MayRun => UserCodeCall::Yes,
        }
    }

    fn classify_binary(&self, left: &Expression, op: &str, arguments: &[Expression]) -> UserCodeCall {
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
        if arithmetic {
            if self.receiver_is_statically_numeric(left)
                && self.receiver_is_statically_numeric(right)
            {
                UserCodeCall::No
            } else {
                UserCodeCall::GuardedOperator
            }
        } else if comparison {
            if self.receiver_is_statically_comparable(left) {
                UserCodeCall::No
            } else {
                UserCodeCall::GuardedOperator
            }
        } else {
            UserCodeCall::Yes
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
        if self.synced.window_depth > 0 {
            return;
        }
        self.record_gap(selector.name().to_string(), at);
    }

    /// In [`SyncMode::Count`], tallies a verifier finding for the report
    /// instead of failing on it; answers whether it did.
    pub(in crate::core_erlang) fn note_counted_verify_errors(
        &mut self,
        errors: &[VerifyError],
        label: &str,
        at: Span,
    ) -> bool {
        if self.synced.mode != SyncMode::Count {
            return false;
        }
        let kind = errors
            .iter()
            .map(|e| format!("{e:?}").split([' ', '{']).next().unwrap_or("").to_string())
            .collect::<Vec<_>>()
            .join("+");
        self.synced
            .other_findings
            .push(format!("{label}\t{kind}\t{}", at.start()));
        true
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
        self.synced.default_arm_sends += 1;
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
        self.synced.default_arm_sends += 1;
        let store = VersionedVar::new(
            VersionPrefix::State,
            self.state_version(),
            self.current_frame(),
        );
        self.synced.brackets += 1;
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
        self.synced.gaps.push(UnbracketedSend {
            selector,
            region,
            at,
        });
    }

    /// Reports the gaps recorded while compiling the module: a verifier
    /// finding in [`SyncMode::Enforce`], a report line in [`SyncMode::Count`].
    pub(in crate::core_erlang) fn finish_synced_calls(&mut self, module_span: Span) {
        if self.synced.mode == SyncMode::Off {
            return;
        }
        let gaps = std::mem::take(&mut self.synced.gaps);
        match self.synced.mode {
            SyncMode::Off => {}
            SyncMode::Count => self.write_synced_calls_report(&gaps),
            SyncMode::Enforce => {
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
    }

    fn write_synced_calls_report(&self, gaps: &[UnbracketedSend]) {
        use std::io::Write as _;
        let Some(path) = std::env::var_os(SYNCED_CALLS_REPORT_ENV) else {
            return;
        };
        let Ok(mut file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
        else {
            return;
        };
        let module = &self.module_name;
        let _ = writeln!(
            file,
            "MODULE\t{module}\tbrackets={}\tgaps={}\tdefault_arm_self_sends={}",
            self.synced.brackets,
            gaps.len(),
            self.synced.default_arm_sends
        );
        for finding in &self.synced.other_findings {
            let _ = writeln!(file, "VERIFY\t{module}\t{finding}");
        }
        for gap in gaps {
            let _ = writeln!(
                file,
                "GAP\t{module}\t{}\t{}\t{}",
                gap.region.label(),
                gap.selector,
                gap.at.start()
            );
        }
    }
}

/// Proof that a bracket window was opened for an expression
/// ([`CoreErlangGenerator::open_sync_window`]).
#[derive(Debug)]
#[must_use]
pub(in crate::core_erlang) struct SyncWindow(());

impl CoreErlangGenerator {
    /// Opens a bracket window around `expr` when it is a call that may run
    /// user code and nothing already brackets it. Calls compiled inside the
    /// window (its operands) run between the store and the reload, so they
    /// are not gaps.
    pub(in crate::core_erlang) fn open_sync_window(&mut self, expr: &Expression) -> Option<SyncWindow> {
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
                if super::expr_shape::is_self_identifier(receiver) {
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
        if self.precompiled_subexprs_contains(inner) {
            return false;
        }
        // A `super` send threads its own `State` (`generate_super_send` stores
        // for itself when it may reach the default arm).
        if matches!(inner, Expression::MessageSend { receiver, .. }
            if matches!(receiver.unwrap_parens(), Expression::Super(_)))
        {
            return false;
        }
        let class = self.classify_expr(inner);
        !matches!(class, UserCodeCall::No | UserCodeCall::CompiledSelfSend)
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
    ) -> super::Result<ThreadedValue> {
        let span = expr.unwrap_parens().span();
        if self.classify_expr(expr.unwrap_parens()) == UserCodeCall::SelfViaDefaultArm {
            return Ok(self.store_before_self_dispatch(tv, expr, span));
        }
        let ValueRef::Doc(call) = tv.value else {
            self.record_gap(Self::send_label(expr), span);
            return Ok(tv);
        };
        let state_not_authoritative = (self.loop_mode.in_direct_params_loop
            && !self.loop_mode.in_hybrid_loop)
            || !self.loop_mode.hybrid_readonly_field_params.is_empty()
            || !self.loop_mode.hybrid_mutated_fields.is_empty();
        if state_not_authoritative {
            // The loop's `fun` binds no whole `State` to store (direct
            // params), or holds fields in pre-extracted parameters (hybrid):
            // the loop has to thread one (its owning node's job).
            self.record_gap(Self::send_label(expr), span);
            return Ok(ThreadedValue {
                prelude: tv.prelude,
                value: ValueRef::Doc(call),
            });
        }
        let mut prelude = tv.prelude;
        let store = VersionedVar::new(VersionPrefix::State, self.state_version(), frame);
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
        self.synced.brackets += 1;
        Ok(ThreadedValue {
            prelude,
            value: ValueRef::Var(result),
        })
    }

    /// The store-only form for a self-send that may reach `dispatch/4`'s
    /// default arm: the send keeps its explicit `{reply, R, State}` threading
    /// (the `Bind` that follows its dispatch statement), and the dispatch
    /// statement is wrapped so the cell holds the current `State` while it
    /// runs. Anything but the producer's own `[.., dispatch, Bind]` shape is a
    /// gap.
    fn store_before_self_dispatch(
        &mut self,
        tv: ThreadedValue,
        expr: &Expression,
        span: Span,
    ) -> ThreadedValue {
        let mut prelude = tv.prelude;
        let n = prelude.len();
        let store = match prelude.get(n.wrapping_sub(1)) {
            Some(ThreadedStmt::Bind { source, target, .. })
                if n >= 2
                    && target.prefix == VersionPrefix::State
                    && matches!(prelude[n - 2], ThreadedStmt::Statement(..)) =>
            {
                Some(source.clone())
            }
            _ => None,
        };
        let Some(store) = store else {
            self.record_gap(Self::send_label(expr), span);
            return ThreadedValue {
                prelude,
                value: tv.value,
            };
        };
        if let ThreadedStmt::Statement(call, stmt_span) = prelude.remove(n - 2) {
            prelude.insert(
                n - 2,
                ThreadedStmt::SyncedCall {
                    store,
                    call,
                    result: None,
                    load: None,
                    span: stmt_span,
                },
            );
        }
        self.synced.brackets += 1;
        ThreadedValue {
            prelude,
            value: tv.value,
        }
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
