// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared state-threading utilities used across loop/fold/conditional codegen:
//! the `__local__` state-map key convention and the typed marker for a
//! construct compiled in place (`construct_locals_in_place`). Which locals a loop threads is
//! `threading_analysis.rs`'s `threaded_locals_of` (ADR 0131, BT-3746).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! split out of `control_flow/mod.rs`, no logic changes.

use super::super::CoreErlangGenerator;
use super::super::threaded_ir::LOCAL_STATE_KEY_PREFIX;
use super::super::threaded_ir::ThreadedStmt;
use super::super::threading_analysis::ThreadedLocals;
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// Returns the state map key for a local variable.
    /// Uses the `__local__` prefix ([`LOCAL_STATE_KEY_PREFIX`]) to prevent
    /// collision with actor field names.
    pub(in crate::core_erlang) fn local_state_key(var_name: &str) -> String {
        let mut key = String::with_capacity(LOCAL_STATE_KEY_PREFIX.len() + var_name.len());
        key.push_str(LOCAL_STATE_KEY_PREFIX);
        key.push_str(var_name);
        key
    }

    /// The key a local's value is held under in the frame's scratch map: the
    /// bare name in a REPL input, whose scratch map is the bindings map itself
    /// (BT-4424), and [`Self::local_state_key`] everywhere else. Every seed,
    /// write and read back of a scratch slot keys by it (the seed,
    /// `seed_scratch_locals`; a write, `lower_local_var_assignment_bind`; a
    /// read, `construct_rebind_prelude`), so a REPL read never asks
    /// for a key nothing wrote (`badkey`) or for a seed the arm's writes left
    /// stale.
    pub(in crate::core_erlang) fn local_scratch_key(&self, var_name: &str) -> String {
        if self.is_repl_mode() {
            var_name.to_string()
        } else {
            Self::local_state_key(var_name)
        }
    }

    /// ADR 0131 (BT-4462): the typed marker for a nested construct `expr`
    /// that a frame compiled in place (unpacking its `{Value, State}`
    /// carrier with an opaque `element(2, ...)` `Bind`) rather than lowering
    /// as a producer: a [`ThreadedStmt::ConstructLocalsInPlace`] naming the
    /// locals it threads through the frame (`threaded_locals_of`, lowered:
    /// a REPL loop's bindings ride the REPL map, not the frame), or `None`
    /// when it threads none, the only
    /// in-place shape an accepted program reaches (an actor list-op that only
    /// sends to `self`, a conditional writing only fields). With a local the
    /// marker is a `VerifyError::ConstructLocalsNotRebound`, reported through
    /// the frame's `report_threaded_ir_verify_errors`: the construct should
    /// have been a producer at its frame's sites, and nothing rereads its
    /// locals out of the carrier any more (BT-4462 deleted that legacy
    /// write-back).
    pub(super) fn construct_locals_in_place(
        &mut self,
        expr: &Expression,
        span: Span,
    ) -> Option<ThreadedStmt> {
        let locals = self
            .threaded_locals_of(expr)
            .and_then(ThreadedLocals::into_lowered)?;
        (!locals.is_empty()).then_some(ThreadedStmt::ConstructLocalsInPlace { locals, span })
    }
}
