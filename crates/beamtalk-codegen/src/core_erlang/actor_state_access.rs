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
use super::synced_call::ActorStateSource;
use super::threaded_ir::{CellBoundary, StateReadSite};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::Block;
use beamtalk_core::source_analysis::Span;

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

/// A `beamtalk_actor_state` helper: its name and, in the type, its arity.
///
/// [`actor_state_call`] takes `[Document; N]` for a `Helper<N>`, so emitting
/// the wrong number of arguments is a compile error at the call site, not a
/// failure at codegen time or an `undef` when the generated module runs.
/// `cell_helper_conformance` checks every entry against the runtime's
/// `-export` blocks, so renaming or unexporting a helper fails the build too.
#[derive(Debug, Clone, Copy)]
pub(in crate::core_erlang) struct Helper<const N: usize> {
    name: &'static str,
}

impl<const N: usize> Helper<N> {
    const fn new(name: &'static str) -> Self {
        Self { name }
    }

    /// The helper's `(name, arity)`, for the export conformance check.
    #[cfg(test)]
    pub(in crate::core_erlang) const fn entry(self) -> (&'static str, usize) {
        (self.name, N)
    }
}

/// Every `beamtalk_actor_state` helper codegen emits.
pub(in crate::core_erlang) mod actor_state_helper {
    use super::Helper;

    pub const INSTALL: Helper<1> = Helper::new("install");
    pub const UNINSTALL: Helper<1> = Helper::new("uninstall");
    pub const TAKE: Helper<0> = Helper::new("take");
    pub const STORE: Helper<1> = Helper::new("store");
    pub const SNAPSHOT: Helper<0> = Helper::new("snapshot");
    pub const RESTORE: Helper<2> = Helper::new("restore");
    pub const CALL_FOREIGN: Helper<3> = Helper::new("call_foreign");
    pub const REFRESH: Helper<2> = Helper::new("refresh");
    pub const CAPTURE: Helper<2> = Helper::new("capture");
    pub const VIEW: Helper<2> = Helper::new("view");
    pub const COMMIT: Helper<2> = Helper::new("commit");
    pub const STORE_UNWOUND: Helper<2> = Helper::new("store_unwound");
    pub const NOTE_UNWOUND: Helper<1> = Helper::new("note_unwound");
    pub const GET: Helper<3> = Helper::new("get");
    pub const PUT: Helper<3> = Helper::new("put");
    pub const SUPER_SEND: Helper<5> = Helper::new("super_send");

    /// Every helper above, for the export conformance check.
    #[cfg(test)]
    pub const ALL: &[(&str, usize)] = &[
        INSTALL.entry(),
        UNINSTALL.entry(),
        TAKE.entry(),
        STORE.entry(),
        SNAPSHOT.entry(),
        RESTORE.entry(),
        CALL_FOREIGN.entry(),
        REFRESH.entry(),
        CAPTURE.entry(),
        VIEW.entry(),
        COMMIT.entry(),
        STORE_UNWOUND.entry(),
        NOTE_UNWOUND.entry(),
        GET.entry(),
        PUT.entry(),
        SUPER_SEND.entry(),
    ];
}

/// The one emitter of a `call 'beamtalk_actor_state':'<helper>'(<args>...)`.
///
/// Every codegen site that reaches the actor-state cell goes through here —
/// bracket stores and loads, closure reads/writes/captures/`super` sends,
/// `on:do:` and `ensure:` boundaries, callback install/uninstall, and the
/// `dispatch/4` foreign exits — so the module and function atoms are built
/// with [`leaf::atom`] in one place (ADR 0089). `helper` is one of
/// [`actor_state_helper`]'s entries and `args` the call's full argument list;
/// its length is the helper's arity by type, so a wrong count does not compile.
pub(in crate::core_erlang) fn actor_state_call<const N: usize>(
    helper: Helper<N>,
    args: [Document<'static>; N],
) -> Document<'static> {
    let mut parts: Vec<Document<'static>> = vec![
        Document::Str("call "),
        leaf::atom("beamtalk_actor_state"),
        Document::Str(":"),
        leaf::atom(helper.name),
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

    /// Runs `build_fun` (which generates the `fun` of a block literal at
    /// `at`) in cell mode and binds the closure's capture around the result
    /// when it is nested in another closure (see the module doc). Outside an
    /// actor instance method nothing changes.
    ///
    /// BT-4180: a closure created inside a stateless loop that brackets
    /// guarded operators captures the cell (`take()`) at creation, bound
    /// around the `fun`: the slow paths of earlier iterations wrote the cell
    /// past the loop-entry `State`, and a capture of that `State` would show
    /// the closure's away-from-home reads the actor as of loop entry. The
    /// source is the read choke point's ([`Self::actor_state_source`]).
    pub(in crate::core_erlang) fn with_actor_closure(
        &mut self,
        at: Span,
        build_fun: impl FnOnce(&mut Self) -> super::Result<Document<'static>>,
    ) -> super::Result<Document<'static>> {
        if !self.cell_closures_apply() {
            return build_fun(self);
        }
        if self.actor_closure.is_none() {
            // BT-4524: a method-level closure holding a `^` must be run by a
            // call that stored the method's state first.
            self.check_nlr_closure(at);
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
                    [leaf::var("Self"), leaf::var(outer.capture().to_string()),],
                ),
                " in ",
                fun
            ]);
        }
        // The method's own current `State` version, where the `fun` is
        // created — or, in a bracketed region, the cell at creation.
        match self.actor_state_source(StateReadSite::ClosureCapture, at) {
            ActorStateSource::Var(capture) => {
                self.actor_closure = Some(ActorClosure::new(capture));
                let built = build_fun(self);
                self.actor_closure = None;
                built
            }
            ActorStateSource::Cell => {
                let capture = self.fresh_temp_var("ASCapture");
                let inner = ActorClosure::new(capture.clone());
                let used = std::rc::Rc::clone(&inner.used);
                self.actor_closure = Some(inner);
                let built = build_fun(self);
                self.actor_closure = None;
                let fun = built?;
                if !used.get() {
                    return Ok(fun);
                }
                Ok(docvec![
                    "let ",
                    leaf::var(capture),
                    " = ",
                    super::threaded_ir::cell_take_call(),
                    " in ",
                    fun
                ])
            }
        }
    }

    /// `beamtalk_actor_state:view(Self, Capture)`: the state map a read sees,
    /// the cell at home and the creation-time capture away.
    pub(in crate::core_erlang) fn cell_view_doc(&self) -> Document<'static> {
        actor_state_call(
            actor_state_helper::VIEW,
            [leaf::var("Self"), self.closure_capture_doc()],
        )
    }

    /// The closure's creation-time capture variable, or `'none'`.
    fn closure_capture_doc(&self) -> Document<'static> {
        self.actor_closure_capture()
            .map_or_else(|| Document::Str("'none'"), |c| leaf::var(c.to_string()))
    }

    /// A read of field `name` inside an actor closure.
    pub(in crate::core_erlang) fn cell_field_read_doc(&self, name: &str) -> Document<'static> {
        let capture = self.closure_capture_doc();
        actor_state_call(
            actor_state_helper::GET,
            [leaf::var("Self"), leaf::atom(name.to_string()), capture],
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
        let capture = self.closure_capture_doc();
        actor_state_call(
            actor_state_helper::SUPER_SEND,
            [
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
                actor_state_call(actor_state_helper::SNAPSHOT, []),
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
                    [leaf::var("Self"), leaf::var(snapshot.name().to_string())]
                ),
                " "
            ],
        }
    }

    /// Whether code compiled right now runs against the live cell rather than
    /// a threaded `State`: inside an actor closure (no threaded `State`), or
    /// inside a stateless loop that brackets guarded operators (BT-4121),
    /// whose slow paths write the cell past the loop-entry `State` — the same
    /// fact that routes the loop's field reads and `^` state through `take()`
    /// ([`Self::guarded_op_take_reads`]).
    pub(in crate::core_erlang) fn cell_is_live(&self) -> bool {
        self.in_cell_closure() || self.state_seeds_from_cell()
    }

    /// The cell obligation an `on:do:` built here carries: `Snapshot` where
    /// the cell is live ([`Self::cell_is_live`]: an actor closure, or a
    /// bracketed stateless loop — BT-4179: the loop-entry `State` is stale
    /// there, so storing it would drop the slow paths' writes), `PreState`
    /// (the version in scope before the region) elsewhere in an actor
    /// instance method, none anywhere else.
    ///
    /// A `PreState` boundary is noted on the innermost loop marker
    /// ([`Self::note_threaded_state_use`]): the checkout of a loop that
    /// brackets reports it when it stores that loop's entry `State`, a
    /// decision reached from the loop's mode and operators rather than from
    /// this gate.
    pub(in crate::core_erlang) fn on_do_cell_boundary(
        &self,
        ex_class_var: &str,
        pre_region_state: &str,
        at: Span,
    ) -> Option<CellBoundary> {
        if self.cell_is_live() {
            return Some(CellBoundary::Snapshot(ErlangVar::new(
                [ex_class_var, "CellSnap"].concat(),
            )));
        }
        if !self.synced_calls_active() {
            return None;
        }
        self.note_threaded_state_use(
            super::synced_call::StaleStateUseKind::OnDoStore,
            pre_region_state,
            at,
        );
        Some(CellBoundary::PreState(ErlangVar::new(
            pre_region_state.to_string(),
        )))
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
                [
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
