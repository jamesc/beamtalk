// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! [`BodyState`]: the generator state that belongs to one method body, owned
//! by the stack of method frames.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! ADR 0133 Phase 3 (BT-3911). A body is lowered against a handful of
//! generator-wide fields that only mean something while that body is being
//! lowered: the `State`/`Self` version counters, whether it is inside a loop
//! body, the active branch frame, the
//! loop mode, the sub-expressions already sequenced
//! (`precompiled_subexprs`), the producer sites and frame registry of the
//! local-threading rules, the fold the body unpacks, the block depth and the
//! class-variable capture. They used to be single slots on the generator, so a
//! body lowered *inside* another one (a `ClassBuilder` `classMethods:` fun
//! whose cascade sits in a loop body, the REPL's per-expression body)
//! inherited the enclosing body's values and, unless every field was reset by
//! hand, saw state that was never its own. A body now enters a
//! [`BodyScope`]: the enclosing body's state is parked in the scope, the body
//! starts from the default, and the enclosing state is back on exit, on every
//! path. [`MethodFrame`](crate::core_erlang::method_frame::MethodFrame) holds
//! one for each method body; a nested body that is not a method (the builder
//! fun) enters one directly. The frames nest as a stack on the Rust stack.
//!
//! Not part of it: counters whose identity must outlive a body
//! (`branch_frame_counter`: frame ids are unique across a module), and the
//! variable scopes (`var_context`), which have their own push/pop discipline.
//! The `State`/`Self` version counters and the non-local-return token are
//! part of it.

use super::CoreErlangGenerator;
use crate::core_erlang::control_flow::LoopMode;
use crate::core_erlang::sequencing::PrecompiledSubexpr;
use crate::core_erlang::threaded_ir::{FrameId, RebindFrame};
use beamtalk_core::source_analysis::Span;
use std::collections::{HashMap, HashSet};

/// The per-body generator state, parked while a nested body is lowered. The
/// default is what a body starts from: nothing inherited.
#[derive(Default)]
pub(in crate::core_erlang) struct BodyState {
    state_version: usize,
    self_version: usize,
    in_loop_body: bool,
    active_branch_frame: u32,
    closure_frame: Option<FrameId>,
    loop_mode: LoopMode,
    precompiled_subexprs: HashMap<Span, PrecompiledSubexpr>,
    local_producer_sites: HashSet<Span>,
    fold_self_slot_consumer: Option<Span>,
    in_fold_frame: bool,
    rebind_frames: HashMap<FrameId, RebindFrame>,
    block_depth: usize,
    class_var_capture: Option<String>,
    actor_closure: Option<crate::core_erlang::actor_state_access::ActorClosure>,
    /// The enclosing body's non-local-return token variable
    /// (`value_type_context.current_nlr_token`): a nested body mints and
    /// clears its own, and the enclosing body's `^` in a later block must
    /// still throw to its own catch.
    nlr_token: Option<String>,
}

impl CoreErlangGenerator {
    /// Takes the current body's state out of the generator, leaving the
    /// default (a body that has inherited nothing) in its place.
    fn take_body_state(&mut self) -> BodyState {
        let state_version = self.state_version();
        let self_version = self.self_version();
        self.set_state_version(0);
        self.set_self_version(0);
        BodyState {
            state_version,
            self_version,
            in_loop_body: std::mem::take(&mut self.in_loop_body),
            active_branch_frame: std::mem::take(&mut self.active_branch_frame),
            closure_frame: self.closure_frame.take(),
            loop_mode: std::mem::take(&mut self.loop_mode),
            precompiled_subexprs: std::mem::take(&mut self.precompiled_subexprs),
            local_producer_sites: std::mem::take(&mut self.local_producer_sites),
            fold_self_slot_consumer: self.fold_self_slot_consumer.take(),
            in_fold_frame: std::mem::take(&mut self.in_fold_frame),
            rebind_frames: std::mem::take(&mut self.rebind_frames),
            block_depth: std::mem::take(&mut self.block_depth),
            class_var_capture: self.class_var_capture.take(),
            actor_closure: self.actor_closure.take(),
            nlr_token: {
                let token = self.current_nlr_token().cloned();
                self.set_current_nlr_token(None);
                token
            },
        }
    }

    /// Puts `state` back as the current body's, discarding whatever the nested
    /// body left behind.
    fn restore_body_state(&mut self, state: BodyState) {
        let BodyState {
            state_version,
            self_version,
            in_loop_body,
            active_branch_frame,
            closure_frame,
            loop_mode,
            precompiled_subexprs,
            local_producer_sites,
            fold_self_slot_consumer,
            in_fold_frame,
            rebind_frames,
            block_depth,
            class_var_capture,
            actor_closure,
            nlr_token,
        } = state;
        self.set_state_version(state_version);
        self.set_self_version(self_version);
        self.in_loop_body = in_loop_body;
        self.active_branch_frame = active_branch_frame;
        self.closure_frame = closure_frame;
        self.loop_mode = loop_mode;
        self.precompiled_subexprs = precompiled_subexprs;
        self.local_producer_sites = local_producer_sites;
        self.fold_self_slot_consumer = fold_self_slot_consumer;
        self.in_fold_frame = in_fold_frame;
        self.rebind_frames = rebind_frames;
        self.block_depth = block_depth;
        self.class_var_capture = class_var_capture;
        self.actor_closure = actor_closure;
        self.set_current_nlr_token(nlr_token);
    }

    /// Enters a nested body: the enclosing body's [`BodyState`] is parked in
    /// the returned scope and the nested body starts from the default. The
    /// scope restores the enclosing state when dropped, on every path.
    pub(in crate::core_erlang) fn enter_body_scope(&mut self) -> BodyScope<'_> {
        let outer = self.take_body_state();
        BodyScope {
            generator: self,
            outer: Some(outer),
        }
    }

    /// Parks the current body's state for a [`MethodFrame`](crate::core_erlang::method_frame::MethodFrame)
    /// that does not own a [`BodyScope`] (it borrows the generator itself).
    pub(in crate::core_erlang) fn park_body_state(&mut self) -> BodyState {
        self.take_body_state()
    }

    /// The counterpart of [`Self::park_body_state`].
    pub(in crate::core_erlang) fn unpark_body_state(&mut self, state: BodyState) {
        self.restore_body_state(state);
    }
}

/// RAII scope of a nested body: see the module docs. Reaches the generator
/// through `Deref`/`DerefMut`, like [`MethodFrame`](crate::core_erlang::method_frame::MethodFrame).
pub(in crate::core_erlang) struct BodyScope<'a> {
    generator: &'a mut CoreErlangGenerator,
    outer: Option<BodyState>,
}

impl std::ops::Deref for BodyScope<'_> {
    type Target = CoreErlangGenerator;

    fn deref(&self) -> &Self::Target {
        self.generator
    }
}

impl std::ops::DerefMut for BodyScope<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.generator
    }
}

impl Drop for BodyScope<'_> {
    fn drop(&mut self) {
        if let Some(outer) = self.outer.take() {
            self.generator.restore_body_state(outer);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Gives `generator` a body state that is not the default in any field.
    fn dirty(generator: &mut CoreErlangGenerator) {
        generator.set_state_version(4);
        generator.set_self_version(5);
        generator.in_loop_body = true;
        generator.active_branch_frame = 7;
        generator.loop_mode.in_direct_params_loop = true;
        generator.local_producer_sites.insert(Span::new(1, 2));
        generator.fold_self_slot_consumer = Some(Span::new(3, 4));
        generator.in_fold_frame = true;
        generator
            .rebind_frames
            .insert(FrameId::new(7), RebindFrame::implicit_root());
        generator.block_depth = 2;
        generator.class_var_capture = Some("CVCap".to_string());
        generator.set_current_nlr_token(Some("NlrTok".to_string()));
    }

    #[test]
    fn a_nested_body_inherits_nothing_and_the_enclosing_body_gets_its_state_back() {
        let mut generator = CoreErlangGenerator::new("test");
        dirty(&mut generator);
        {
            let scope = generator.enter_body_scope();
            assert_eq!(scope.state_version(), 0);
            assert_eq!(scope.self_version(), 0);
            assert!(!scope.in_loop_body);
            assert_eq!(scope.active_branch_frame, 0);
            assert!(!scope.loop_mode.in_direct_params_loop);
            assert!(scope.local_producer_sites.is_empty());
            assert!(scope.fold_self_slot_consumer.is_none());
            assert!(!scope.in_fold_frame);
            assert!(scope.rebind_frames.is_empty());
            assert_eq!(scope.block_depth, 0);
            assert!(scope.class_var_capture.is_none());
            assert!(scope.current_nlr_token().is_none());
        }
        assert_eq!(generator.state_version(), 4);
        assert_eq!(generator.self_version(), 5);
        assert!(generator.in_loop_body);
        assert_eq!(generator.active_branch_frame, 7);
        assert!(generator.loop_mode.in_direct_params_loop);
        assert!(generator.local_producer_sites.contains(&Span::new(1, 2)));
        assert_eq!(generator.fold_self_slot_consumer, Some(Span::new(3, 4)));
        assert!(generator.in_fold_frame);
        assert!(generator.rebind_frames.contains_key(&FrameId::new(7)));
        assert_eq!(generator.block_depth, 2);
        assert_eq!(generator.class_var_capture.as_deref(), Some("CVCap"));
        assert_eq!(
            generator.current_nlr_token().map(String::as_str),
            Some("NlrTok")
        );
    }

    #[test]
    fn what_a_nested_body_leaves_behind_does_not_reach_the_enclosing_body() {
        let mut generator = CoreErlangGenerator::new("test");
        {
            let mut scope = generator.enter_body_scope();
            dirty(&mut scope);
        }
        assert_eq!(generator.state_version(), 0);
        assert!(!generator.in_loop_body);
        assert_eq!(generator.active_branch_frame, 0);
        assert!(generator.local_producer_sites.is_empty());
        assert!(generator.rebind_frames.is_empty());
        assert_eq!(generator.block_depth, 0);
        assert!(generator.class_var_capture.is_none());
        assert!(generator.current_nlr_token().is_none());
    }

    #[test]
    fn body_scopes_nest_as_a_stack() {
        let mut generator = CoreErlangGenerator::new("test");
        generator.block_depth = 1;
        {
            let mut outer = generator.enter_body_scope();
            outer.block_depth = 2;
            {
                let mut inner = outer.enter_body_scope();
                assert_eq!(inner.block_depth, 0);
                inner.block_depth = 3;
            }
            assert_eq!(outer.block_depth, 2);
        }
        assert_eq!(generator.block_depth, 1);
    }
}
