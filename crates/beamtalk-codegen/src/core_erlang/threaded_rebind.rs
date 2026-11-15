// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The one "extract a threaded local from a state map and rebind it" lowering
//! (BT-4888).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Every construct that threads locals through a `StateAcc` map (a loop, a
//! fold, an inlined conditional arm, a Tier 2 block, a method-body control
//! flow) re-reads each local after the construct with
//! `let <Var> = call 'maps':'get'(<key>, <State>) in`. The sites differ in
//! exactly three things, each an explicit parameter here instead of a copy:
//!
//! * the **state-map key** ([`CoreErlangGenerator::local_state_key`],
//!   `local_scratch_key`, or a `ThreadingPlan::state_key`);
//! * the **state variable** being read;
//! * the **[`RebindNaming`] policy** — which Core Erlang variable the local is
//!   (re)bound to, and whether the scope binding is updated.
//!
//! The template-level difference between a method body (re-extracts with
//! `maps:get` and advances the `State` version) and an arm (rebinds in place)
//! stays at the walkers; this module is only the per-local read.

use super::CoreErlangGenerator;
use beamtalk_cerl_doc::{Document, docvec, leaf};

/// Which Core Erlang variable a rebound threaded local gets (BT-4888).
///
/// Each variant is one of the pre-existing site policies, kept as-is:
/// the choice affects the minted-name sequence (`fresh_temp_var` /
/// `fresh_var` counters) and so the generated `.core`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum RebindNaming<'a> {
    /// Reuse the name the local is already bound to (falling back to the
    /// canonical spelling) and leave the scope untouched. The rebind
    /// *shadows* the old Core variable with a new `let` of the same name.
    ReuseBound,
    /// The canonical `to_core_erlang_var` name, bound in the current scope.
    Canonical,
    /// A `fresh_var` name (counter-suffixed), bound in the current scope.
    FreshBound,
    /// A `fresh_temp_var(base)` name (unbindable temp marker), bound in the
    /// current scope.
    FreshTemp(&'a str),
}

/// How one rebind `let` is laid out around [`CoreErlangGenerator::state_get_let_doc`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum RebindLayout {
    /// `let V = call 'maps':'get'(K, S) in ` — spliced into a larger line.
    Inline,
    /// `    let V = call 'maps':'get'(K, S) in\n` — one indented line per local.
    Line,
}

impl RebindLayout {
    fn wrap(self, let_doc: Document<'static>) -> Document<'static> {
        match self {
            Self::Inline => docvec![let_doc, " in "],
            Self::Line => docvec!["    ", let_doc, " in\n"],
        }
    }
}

impl CoreErlangGenerator {
    /// Picks (and, for the binding policies, binds) the Core Erlang variable
    /// that `local` is rebound to under `naming`.
    pub(super) fn rebind_local_name(&mut self, local: &str, naming: RebindNaming<'_>) -> String {
        match naming {
            RebindNaming::ReuseBound => self.resolve_local_core_var(local),
            RebindNaming::Canonical => {
                let core_var = Self::to_core_erlang_var(local);
                self.bind_var(local, &core_var);
                core_var
            }
            RebindNaming::FreshBound => self.fresh_var(local),
            RebindNaming::FreshTemp(base) => {
                let core_var = self.fresh_temp_var(base);
                self.bind_var(local, &core_var);
                core_var
            }
        }
    }

    /// `let <core_var> = call 'maps':'get'(<key>, <state_var>)` — no `in`
    /// terminator (call sites differ in layout around it). With
    /// `default`, the 3-arity `maps:get(Key, State, Default)` form.
    pub(super) fn state_get_let_doc(
        core_var: String,
        key: String,
        state_var: &str,
        default: Option<String>,
    ) -> Document<'static> {
        let tail = match default {
            Some(d) => docvec![
                ", ",
                leaf::var(state_var.to_string()),
                ", ",
                leaf::var(d),
                ")"
            ],
            None => docvec![", ", leaf::var(state_var.to_string()), ")"],
        };
        docvec![
            "let ",
            leaf::var(core_var),
            " = call 'maps':'get'(",
            leaf::atom(key),
            tail,
        ]
    }

    /// Rebinds every local in `locals` from `state_var` (see the module docs):
    /// one [`Self::state_get_let_doc`] per local, in order, each named per
    /// `naming`, keyed by `key_of` and laid out per `layout`.
    pub(super) fn rebind_threaded_locals<'l>(
        &mut self,
        locals: impl IntoIterator<Item = &'l String>,
        key_of: impl Fn(&Self, &str) -> String,
        state_var: &str,
        naming: RebindNaming<'_>,
        layout: RebindLayout,
    ) -> Vec<Document<'static>> {
        locals
            .into_iter()
            .map(|local| {
                let core_var = self.rebind_local_name(local, naming);
                layout.wrap(Self::state_get_let_doc(
                    core_var,
                    key_of(self, local),
                    state_var,
                    None,
                ))
            })
            .collect()
    }
}
