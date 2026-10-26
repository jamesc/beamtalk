// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor-closure access to the actor's state (ADR 0137 §3, Phase 2b).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! A method body threads `State` in locals. A *closure* written in an actor
//! instance method (a block that is not lowered in place: a `fun` value) has
//! no threaded `State`: it runs inside the actor, between a bracketed call's
//! store and reload ([`super::synced_call`]), or in some other process. Its
//! field reads and writes therefore go through `beamtalk_actor_state`, the one
//! owner of the cell, exactly as a class method's class-variable access goes
//! through `beamtalk_class_vars` (ADR 0130): in place, with nothing rebound.
//!
//! Inside a closure the generator is in *cell mode*
//! ([`CoreErlangGenerator::in_cell_closure`]): [`CoreErlangGenerator::in_actor_instance_context`]
//! and the `State` family's eligibility are false there, so every lowering that
//! would thread `State` (a conditional arm, a loop body, a self-send) treats the
//! body like a class method's: a field write is an expression with a side
//! effect, and a self-send is an ordinary message to `Self`.
//!
//! ## Capture
//!
//! A closure created in a method body captures the method's current threaded
//! `State` version (the variable in scope where the `fun` is created: it costs
//! nothing). A closure created inside another closure binds
//! `beamtalk_actor_state:capture(Self, OuterCapture)` around its `fun`: the cell
//! at home (current, the enclosing closure writes it in place), the enclosing
//! capture otherwise. The capture is consulted only away from home, where a
//! read answers it and a write raises `actor_state_unreachable`.

use super::CoreErlangGenerator;
use super::erlang_types::ErlangVar;
use super::threaded_ir::CellBoundary;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::Block;

/// The actor closure being lowered: the Core Erlang variable holding the
/// creation-time capture its field reads fall back to away from home.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct ActorClosure {
    /// The capture variable: the enclosing method's `State` version at the
    /// closure's creation, or the `capture/2` binding of a nested closure.
    capture: String,
    /// Whether anything lowered inside the closure read [`Self::capture`], so a
    /// nested closure binds `capture/2` only when it is used.
    used: std::rc::Rc<std::cell::Cell<bool>>,
}

impl ActorClosure {
    fn new(capture: String) -> Self {
        Self {
            capture,
            used: std::rc::Rc::default(),
        }
    }

    /// The capture variable, noting that it was read.
    fn capture(&self) -> &str {
        self.used.set(true);
        &self.capture
    }
}

/// Every `beamtalk_actor_state` helper codegen emits, as `(name, arity)`.
///
/// [`actor_state_call`] takes one of these entries and checks the emitted
/// argument count against its arity, and `cell_helper_conformance` checks
/// every entry against the runtime's `-export` blocks — renaming,
/// unexporting, or mis-aritying a helper fails the build instead of failing
/// with `undef` at runtime.
pub(in crate::core_erlang) mod actor_state_helper {
    pub const INSTALL: (&str, u32) = ("install", 1);
    pub const UNINSTALL: (&str, u32) = ("uninstall", 1);
    pub const TAKE: (&str, u32) = ("take", 0);
    pub const STORE: (&str, u32) = ("store", 1);
    pub const SNAPSHOT: (&str, u32) = ("snapshot", 0);
    pub const RESTORE: (&str, u32) = ("restore", 2);
    pub const CALL_FOREIGN: (&str, u32) = ("call_foreign", 3);
    pub const REFRESH: (&str, u32) = ("refresh", 2);
    pub const CAPTURE: (&str, u32) = ("capture", 2);
    pub const VIEW: (&str, u32) = ("view", 2);
    pub const GET: (&str, u32) = ("get", 3);
    pub const PUT: (&str, u32) = ("put", 3);
    pub const SUPER_SEND: (&str, u32) = ("super_send", 5);

    /// Every helper above, for the export conformance check.
    #[cfg(test)]
    pub const ALL: &[(&str, u32)] = &[
        INSTALL,
        UNINSTALL,
        TAKE,
        STORE,
        SNAPSHOT,
        RESTORE,
        CALL_FOREIGN,
        REFRESH,
        CAPTURE,
        VIEW,
        GET,
        PUT,
        SUPER_SEND,
    ];
}

/// The one emitter of a `call 'beamtalk_actor_state':'<helper>'(<args>...)`.
///
/// Every codegen site that reaches the actor-state cell goes through here —
/// bracket stores and loads, closure reads/writes/captures/`super` sends,
/// `on:do:` and `ensure:` boundaries, callback install/uninstall, and the
/// `dispatch/4` foreign exits — so the module and function atoms are built
/// with [`leaf::atom`] in one place (ADR 0089). `helper` is one of
/// [`actor_state_helper`]'s `(name, arity)` entries and `args` the call's
/// full argument list; the count is checked against the arity here, so
/// emitting the wrong number of arguments fails the build instead of failing
/// with `undef` at runtime.
pub(in crate::core_erlang) fn actor_state_call(
    helper: (&str, u32),
    args: Vec<Document<'static>>,
) -> Document<'static> {
    let (name, arity) = helper;
    assert_eq!(
        args.len(),
        arity as usize,
        "beamtalk_actor_state:{name} takes {arity} arguments"
    );
    let mut parts: Vec<Document<'static>> = vec![
        Document::Str("call "),
        leaf::atom("beamtalk_actor_state"),
        Document::Str(":"),
        leaf::atom(name),
        Document::Str("("),
    ];
    for (i, arg) in args.into_iter().enumerate() {
        if i > 0 {
            parts.push(Document::Str(", "));
        }
        parts.push(arg);
    }
    parts.push(Document::Str(")"));
    Document::Vec(parts)
}

impl CoreErlangGenerator {
    /// Whether the code being compiled is inside an actor closure in cell mode.
    pub(in crate::core_erlang) fn in_cell_closure(&self) -> bool {
        self.actor_closure.is_some()
    }

    /// Whether the body being compiled threads no actor `State`: a class method
    /// (class variables live in the class process, ADR 0130) or an actor
    /// closure (the cell, ADR 0137 §3). Locals thread through a fresh scratch
    /// map and fields are accessed in place.
    pub(in crate::core_erlang) fn threads_no_actor_state(&self) -> bool {
        self.in_class_method() || self.in_cell_closure()
    }

    /// Whether running `block` can read or write the actor's state or run user
    /// code in this process: a `self.field` access, a bare field name, a send to
    /// `self` or `super`, or a send that may run user code, nested blocks
    /// included. A closure that does none of these never looks at the cell, so
    /// the call that runs it needs no bracket.
    pub(in crate::core_erlang) fn block_touches_actor_state(&self, block: &Block) -> bool {
        self.analysis_touches_actor_state(&self.block_profile_or_analyze(block))
    }

    /// Whether `block` sends a message to `super`, nested blocks included.
    pub(in crate::core_erlang) fn block_sends_to_super(block: &Block) -> bool {
        let mut found = false;
        for stmt in &block.body {
            beamtalk_core::ast_walker::walk_expression(&stmt.expression, &mut |e| {
                if let beamtalk_core::ast::Expression::MessageSend { receiver, .. } = e {
                    found |= matches!(
                        receiver.unwrap_parens(),
                        beamtalk_core::ast::Expression::Super(_)
                    );
                }
            });
        }
        found
    }

    /// Whether a block literal compiled now becomes an actor closure in cell
    /// mode: an actor instance method body (the same scope the cell bracket
    /// is built for, [`Self::synced_calls_active`]).
    pub(in crate::core_erlang) fn cell_closures_apply(&self) -> bool {
        self.synced_calls_scope()
    }

    /// The capture variable of the closure being lowered.
    pub(in crate::core_erlang) fn actor_closure_capture(&self) -> Option<&str> {
        self.actor_closure.as_ref().map(ActorClosure::capture)
    }

    /// Runs `build_fun` (which generates the `fun` of a block literal) in cell
    /// mode and binds the closure's capture around the result when it is nested
    /// in another closure (see the module doc). Outside an actor instance
    /// method nothing changes.
    pub(in crate::core_erlang) fn with_actor_closure(
        &mut self,
        build_fun: impl FnOnce(&mut Self) -> super::Result<Document<'static>>,
    ) -> super::Result<Document<'static>> {
        if !self.cell_closures_apply() {
            return build_fun(self);
        }
        if let Some(outer) = self.actor_closure.clone() {
            let capture = self.fresh_temp_var("ASCapture");
            let inner = ActorClosure::new(capture.clone());
            let used = std::rc::Rc::clone(&inner.used);
            let saved = self.actor_closure.replace(inner);
            let built = build_fun(self);
            self.actor_closure = saved;
            let fun = built?;
            if !used.get() {
                return Ok(fun);
            }
            return Ok(docvec![
                "let ",
                leaf::var(capture),
                " = ",
                actor_state_call(
                    actor_state_helper::CAPTURE,
                    vec![leaf::var("Self"), leaf::var(outer.capture().to_string()),],
                ),
                " in ",
                fun
            ]);
        }
        // The method's own current `State` version, where the `fun` is created.
        let capture = self.current_field_read_state_var();
        self.actor_closure = Some(ActorClosure::new(capture));
        let built = build_fun(self);
        self.actor_closure = None;
        built
    }

    /// `beamtalk_actor_state:view(Self, Capture)`: the state map a read sees,
    /// the cell at home and the creation-time capture away.
    pub(in crate::core_erlang) fn cell_view_doc(&self) -> Document<'static> {
        let capture = self
            .actor_closure_capture()
            .map_or_else(|| Document::Str("'none'"), |c| leaf::var(c.to_string()));
        actor_state_call(actor_state_helper::VIEW, vec![leaf::var("Self"), capture])
    }

    /// A read of field `name` inside an actor closure.
    pub(in crate::core_erlang) fn cell_field_read_doc(&self, name: &str) -> Document<'static> {
        let capture = self
            .actor_closure_capture()
            .map_or_else(|| Document::Str("'none'"), |c| leaf::var(c.to_string()));
        actor_state_call(
            actor_state_helper::GET,
            vec![leaf::var("Self"), leaf::atom(name.to_string()), capture],
        )
    }

    /// A `super` send inside an actor closure:
    /// `beamtalk_actor_state:super_send(Self, Capture, Class, Selector, Args)`.
    pub(in crate::core_erlang) fn cell_super_send_doc(
        &self,
        selector: &str,
        class_name: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        let capture = self
            .actor_closure_capture()
            .map_or_else(|| Document::Str("'none'"), |c| leaf::var(c.to_string()));
        actor_state_call(
            actor_state_helper::SUPER_SEND,
            vec![
                leaf::var("Self"),
                capture,
                leaf::atom(class_name.to_string()),
                leaf::atom(selector.to_string()),
                docvec!["[", args_doc, "]"],
            ],
        )
    }

    /// The entry half of an `on:do:` catch boundary's cell obligation
    /// ([`CellBoundary`]): store the method's pre-region `State` so the region's
    /// closures read it, or snapshot the cell inside a closure.
    pub(in crate::core_erlang) fn cell_boundary_entry_doc(
        boundary: &CellBoundary,
    ) -> Document<'static> {
        match boundary {
            CellBoundary::PreState(state) => docvec![
                "let _ = ",
                super::threaded_ir::cell_put_call(leaf::var(state.name().to_string())),
                " in "
            ],
            CellBoundary::Snapshot(snapshot) => docvec![
                "let ",
                leaf::var(snapshot.name().to_string()),
                " = ",
                actor_state_call(actor_state_helper::SNAPSHOT, vec![]),
                " in "
            ],
        }
    }

    /// The catch half of the cell obligation: put the cell back to the region's
    /// pre-region state before the handler runs. The snapshot arm passes
    /// `Self` so `restore/2` only rolls back at home (BT-4122): away from
    /// home the cell holds another actor's state and is left alone.
    pub(in crate::core_erlang) fn cell_boundary_restore_doc(
        boundary: &CellBoundary,
    ) -> Document<'static> {
        match boundary {
            CellBoundary::PreState(state) => {
                docvec![
                    "do ",
                    super::threaded_ir::cell_put_call(leaf::var(state.name().to_string())),
                    " "
                ]
            }
            CellBoundary::Snapshot(snapshot) => docvec![
                "do ",
                actor_state_call(
                    actor_state_helper::RESTORE,
                    vec![leaf::var("Self"), leaf::var(snapshot.name().to_string())]
                ),
                " "
            ],
        }
    }

    /// The cell obligation an `on:do:` built here carries: `PreState` (the
    /// version in scope before the region) in an actor instance method,
    /// `Snapshot` inside an actor closure, none anywhere else.
    pub(in crate::core_erlang) fn on_do_cell_boundary(
        &self,
        ex_class_var: &str,
        pre_region_state: &str,
    ) -> Option<CellBoundary> {
        if self.in_cell_closure() {
            return Some(CellBoundary::Snapshot(ErlangVar::new(
                [ex_class_var, "CellSnap"].concat(),
            )));
        }
        self.synced_calls_active()
            .then(|| CellBoundary::PreState(ErlangVar::new(pre_region_state.to_string())))
    }

    /// A write of field `name` inside an actor closure to the value `value_doc`
    /// evaluates to; the expression's own value is the assigned value.
    pub(in crate::core_erlang) fn cell_field_write_doc(
        &mut self,
        name: &str,
        value_doc: Document<'static>,
    ) -> Document<'static> {
        let val_var = self.fresh_temp_var("ASVal");
        let ok_var = self.fresh_temp_var("ASOk");
        docvec![
            "let ",
            leaf::var(val_var.clone()),
            " = ",
            value_doc,
            " in let ",
            leaf::var(ok_var),
            " = ",
            actor_state_call(
                actor_state_helper::PUT,
                vec![
                    leaf::var("Self"),
                    leaf::atom(name.to_string()),
                    leaf::var(val_var.clone())
                ]
            ),
            " in ",
            leaf::var(val_var)
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::{actor_state_call, actor_state_helper};

    /// The arity half of a helper entry is checked where the call is built:
    /// emitting the wrong argument count fails codegen instead of failing
    /// with `undef` when the generated module runs.
    #[test]
    #[should_panic(expected = "beamtalk_actor_state:get takes 3 arguments")]
    fn wrong_argument_count_fails_codegen() {
        let _ = actor_state_call(
            actor_state_helper::GET,
            vec![beamtalk_cerl_doc::leaf::var("Self")],
        );
    }
}
