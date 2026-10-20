// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Live actor state for closures that run at home (BT-3952, ADR 0133
//! amendment; the instance-side twin of ADR 0130 section 5).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! An actor method threads `State` as a value, so a closure created early in
//! the method captures the `State` of that moment. Run at home — in the same
//! actor process, inside one of its own invocations — the closure must read
//! what a direct `self.field` read at that point would: the *live* state. Two
//! halves, both here so neither can drift from the other:
//!
//! - **Publish** (writes): every actor field write also publishes its result to
//!   the runtime (`beamtalk_actor:publish_state/2`, or `publish_field/3` where
//!   the write rebinds a loop parameter and there is no `State` to publish).
//!   `threaded_ir::render`'s `Put` arm covers every method-body, branch-arm,
//!   loop, fold, `on:do:` and Tier 2 body write, so no scope kind needs its own
//!   call; the other write lowerings (a hybrid-loop rebind, the reflective
//!   `fieldAt:put:`/`clearField:`) call the helpers below where they mint the
//!   new value.
//! - **Read** (closures): a `self.field` read in a closure body asks the
//!   runtime for the live state (`beamtalk_actor:live_state/2`), which answers
//!   the closure's captured `State` anywhere that is not home.
//!
//! The runtime keeps the live state in the invocation slot `'$bt_actor_state'`
//! that `handle_call`/`handle_cast` already stash and restore, so there is no
//! entry/exit bookkeeping here.

use super::threaded_ir::ThreadedStmt;
use super::{CodeGenContext, CoreErlangGenerator};
use beamtalk_cerl_doc::{Document, docvec, leaf};
use beamtalk_core::source_analysis::Span;

/// `do call 'beamtalk_actor':'publish_state'(Self, StateN) ` — publishes a
/// whole `State` version as the invocation's live state. A prefix: the
/// statements that follow are its body.
pub(super) fn publish_state_doc(state_var: &str) -> Document<'static> {
    docvec![
        "do call 'beamtalk_actor':'publish_state'(Self, ",
        leaf::var(state_var.to_string()),
        ") ",
    ]
}

/// `do call 'beamtalk_actor':'publish_field'(Self, 'field', Value) ` —
/// publishes one field's new value. For a write that rebinds a hybrid loop's
/// field parameter, where no `State` map exists to publish.
pub(super) fn publish_field_doc(field: &str, value_var: &str) -> Document<'static> {
    docvec![
        "do call 'beamtalk_actor':'publish_field'(Self, ",
        leaf::atom(field.to_string()),
        ", ",
        leaf::var(value_var.to_string()),
        ") ",
    ]
}

/// `call 'beamtalk_actor':'live_state'(Self, State)` — the state a closure's
/// `self.field` read consults.
pub(super) fn live_state_doc(captured_state_var: &str) -> Document<'static> {
    docvec![
        "call 'beamtalk_actor':'live_state'(Self, ",
        leaf::var(captured_state_var.to_string()),
        ")",
    ]
}

impl CoreErlangGenerator {
    /// `true` where an actor field write must publish its result: an Actor
    /// *instance* method. A class method has no `State` (ADR 0130), a value
    /// type is immutable, and the REPL has no actor invocation.
    pub(super) fn publishes_actor_field_writes(&self) -> bool {
        self.context == CodeGenContext::Actor && !self.in_class_method() && !self.is_repl_mode()
    }

    /// The publish statement for the `State` version just minted by a write
    /// that is lowered as an opaque `maps:put`/`maps:remove` with a computed
    /// field name (`fieldAt:put:`, `clearField:`), or `None` where no actor
    /// field write publishes.
    pub(super) fn publish_current_state_stmt(&self, span: Span) -> Option<ThreadedStmt> {
        self.publishes_actor_field_writes()
            .then(|| ThreadedStmt::Statement(publish_state_doc(&self.current_state_var()), span))
    }

    /// [`publish_state_doc`] for an actor instance method, empty elsewhere.
    pub(super) fn publish_state_doc_if_actor(&self, state_var: &str) -> Document<'static> {
        if self.publishes_actor_field_writes() {
            publish_state_doc(state_var)
        } else {
            Document::Nil
        }
    }

    /// `true` while generating the body of a closure (`block_depth > 0`) in an
    /// Actor instance method: a `self.field` read there asks the runtime for
    /// the live state instead of reading the captured `State`. Literal blocks
    /// the compiler inlines (loops, conditionals, folds, `on:do:`) are not
    /// closures and keep reading the threaded `State`.
    pub(super) fn reads_live_actor_state(&self) -> bool {
        self.publishes_actor_field_writes() && self.block_depth > 0
    }
}
