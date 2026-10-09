// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared state-threading utilities used across loop/fold/conditional codegen:
//! the `__local__` state-map key convention and rebinding threaded locals from
//! a `StateAcc` map. Which locals a loop threads is
//! `threading_analysis.rs`'s `threaded_locals_of` (ADR 0131, BT-3746).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! split out of `control_flow/mod.rs`, no logic changes.

use super::super::CoreErlangGenerator;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};

impl CoreErlangGenerator {
    /// Returns the state map key for a local variable.
    /// Uses a `__local__` prefix to prevent collision with actor field names.
    pub(in crate::core_erlang) fn local_state_key(var_name: &str) -> String {
        format!("__local__{var_name}")
    }

    /// ADR 0111 Addendum 5 (rebind idiom): rebinds
    /// each of `vars` — a nested control-flow construct's threaded
    /// `__local__` captured vars — from `state_var`, returning one `let V =
    /// maps:get(...) in` `Document` per var and updating each var's own
    /// Core Erlang binding via `bind_var` so later code (in whatever form
    /// the caller assembles) sees the rebound value rather than the stale
    /// pre-statement one.
    ///
    /// Shared leaf helper for two call sites that both need this same
    /// rebind after unpacking a nested construct's `{Result, NewState}`
    /// result, differing only in how they wrap the returned `Document`s:
    /// `conditionals.rs`'s `push_control_flow_threaded_var_rereads` (the
    /// `ThreadedIr`-rendered conditional-arm path, wraps as one
    /// `ThreadedStmt::Statement`) and `body.rs`'s
    /// `lower_foldl_body` (the foldl loop-body path,
    /// pushes directly onto its `stmts` vec).
    pub(super) fn rebind_threaded_vars_from_state(
        &mut self,
        vars: &[String],
        state_var: &str,
    ) -> Vec<Document<'static>> {
        let mut docs = Vec::new();
        for var in vars {
            let core_var = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            docs.push(docvec![
                "let ",
                leaf::var(core_var.clone()),
                " = call 'maps':'get'(",
                leaf::atom(Self::local_state_key(var)),
                ", ",
                leaf::var(state_var.to_string()),
                ") in ",
            ]);
            self.bind_var(var, &core_var);
        }
        docs
    }
}
