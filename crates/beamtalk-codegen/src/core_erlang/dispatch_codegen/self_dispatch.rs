// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Self-dispatch and sealed dispatch.
//!
//! Lowers a send to `self` inside an instance method: the normal
//! `safe_dispatch`/`dispatch` call, the statically-bound sealed-class form, and
//! the shared error clauses and result threading.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix, VersionedVar,
};
use crate::core_erlang::{CodeGenContext, CoreErlangGenerator, Result};

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MessageSelector};
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// Generates the `<{'error',..., _}>` case clauses shared by all
    /// self-dispatch call sites (`safe_dispatch`/`dispatch` error branches).
    ///
    /// The dispatched call's error branch has two distinct shapes, mirroring the
    /// two `<{'error', ...}>` clauses already used at the `handle_cast`/
    /// `handle_info` boundary (see `gen_server/callbacks.rs`):
    ///
    /// 1. **Caught exception**: `safe_dispatch/3` packs a caught exception as a
    ///    3-tuple `{Type, Reason, Stacktrace}` in the middle element of its
    ///    `{'error', ..., State}` return (see `generate_safe_dispatch`). Passing
    ///    that whole triple straight to `beamtalk_error:'raise'/1` — which only
    ///    accepts a raw `#beamtalk_error{}` record — crashes with
    ///    `function_clause` instead of propagating the real error.
    ///    Destructuring the triple and routing it through
    ///    `beamtalk_exception_handler:'reraise'/4` mirrors the cross-actor call
    ///    boundary (`beamtalk_actor:sync_send_remote/3`), which correctly
    ///    classifies raw Erlang errors and preserves already-wrapped
    ///    `#beamtalk_error{}` values.
    /// 2. **Plain returned error**: `dispatch/4`'s DNU fallback (and other
    ///    non-exception error paths) *returns* `{'error', Error, State}` where
    ///    `Error` is a bare `#beamtalk_error{}` record — not a 3-tuple, so it
    ///    never reaches `safe_dispatch`'s try/catch at all. This fallback clause
    ///    must stay, or a self-send that resolves to DNU crashes with
    ///    `case_clause` instead of raising the DNU error.
    ///
    /// Passes a `selector`/`class` breadcrumb `Context` map to
    /// `reraise/4` — mirroring `sync_send_remote/3`'s
    /// `#{selector => Selector, class => Class}` construction — so a raw
    /// Erlang error escaping a self-send forwarding hop gets the same
    /// `ClassName>>selector: ...` location prefix (via `wrap_raw/2` /
    /// `located/3`) as the cross-actor equivalent.
    ///
    /// `class` is resolved via a *runtime* `beamtalk_actor:lookup_class/1`
    /// call on `self()`, not a compile-time literal atom. For a self-send inside
    /// a method a subclass inherits without overriding, the inherited method's
    /// code lives in the superclass module, so a literal `class_name()` atom
    /// would yield the superclass instead of the actor's actual runtime class —
    /// unlike `sync_send_remote/3`, which resolves the runtime class via the
    /// same `lookup_class/1` ETS reverse-lookup on `beamtalk_instance_registry`.
    /// Emitting the same runtime lookup here keeps the self-send breadcrumb's
    /// `class` value in parity with the cross-actor path for inherited methods.
    /// `selector_atom` is still known at compile time and stays a literal atom.
    ///
    /// Known caveat: `lookup_class/1` reads `beamtalk_instance_registry`,
    /// which the *spawner* populates only after `beamtalk_actor:safe_spawn/2`'s
    /// `await_initialize/3` confirms `initialize` has returned (see
    /// `gen_server/spawn.rs`'s `instance_registration_doc`). A self-send that
    /// raises *during* `initialize` — or in a continuation `initialize`
    /// requested, which since BT-3992 runs concurrently with the spawner's
    /// registration — can therefore run before this actor's own registry
    /// entry exists, so `lookup_class(self())` falls back to `'unknown'` for
    /// that narrow window — trading a guaranteed-
    /// correct compile-time atom (for non-inherited classes only) for a
    /// safe-but-less-specific fallback, in exchange for a correct answer in
    /// every other case (including all inherited-method self-sends, the
    /// actual bug this fixes). `lookup_class/1` never crashes either way.
    ///
    /// Core Erlang map literals (`~{...}~`) cannot contain `call` expressions
    /// (see `lifecycle_start_telemetry_doc` in `gen_server/callbacks.rs` for the
    /// same constraint), so the lookup is hoisted into a `let` binding inside
    /// the clause body before the map is constructed. This only runs on the
    /// error path — no overhead on the success path.
    ///
    /// ## NLR relay (BT-3580)
    ///
    /// A `^` inside a block passed through this self-send to a user HOM
    /// (e.g. `self do: [:each | ... ifTrue: [^each]]`) throws the 4-tuple
    /// `{'$bt_nlr', Token, Value, State}` (ADR 0041). `generate_safe_dispatch`
    /// (`gen_server/dispatch.rs`) already special-cases that exact throw
    /// shape ahead of its own catch-all (BT-3580/BT-3582) and returns it as
    /// a *bare* value — `{'error', {'$bt_nlr', Token, Value, State}, State}`
    /// — instead of packing it into the ordinary `{Type, Reason,
    /// Stacktrace}` triple every other caught exception gets. This call
    /// site must recognize that same bare shape and re-throw it (a plain
    /// `erlang:throw/1` — there is no stacktrace left to preserve; `^` is
    /// intentional control flow, not a reported error, matching the
    /// existing relays in `beamtalk_actor.erl` and
    /// `beamtalk_class_dispatch.erl`, neither of which preserves one
    /// either) so the enclosing method's own NLR catch (which only matches
    /// `throw:{'$bt_nlr', ...}` — see `nlr.rs`'s `wrap_body_with_nlr_catch`)
    /// sees it, whether it owns this token or is itself just another
    /// self-dispatch hop relaying it further up — the same relay
    /// `beamtalk_class_dispatch.erl` already performs for class-method
    /// self-dispatch hops (ADR 0110), now applied to instance actor
    /// self-sends. A leading clause here must come before the "plain
    /// returned error" catch-all below, which would otherwise pass the raw
    /// NLR tuple to `beamtalk_error:'raise'/1` — a `function_clause` crash,
    /// since that function only accepts a `#beamtalk_error{}` record.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// <{'error', {'$bt_nlr', NlrTok, NlrVal, NlrSt}, _}> when 'true' ->
    ///     call 'erlang':'throw'({'$bt_nlr', NlrTok, NlrVal, NlrSt})
    /// <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///     let Class = call 'beamtalk_actor':'lookup_class'(call 'erlang':'self'()) in
    ///     call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace,
    ///         ~{'selector' => 'selector:', 'class' => Class}~)
    /// <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// <NoMatch> when 'true' -> call 'erlang':'error'({'case_clause', NoMatch})
    /// ```
    ///
    /// the trailing wildcard clause is not reachable at runtime
    /// (`safe_dispatch/3` only ever returns `{'reply', _, _}` or one of the
    /// two `{'error', _, _}` shapes matched above) but is required to make
    /// the `case` *statically* exhaustive — see `case_clause_fallback`'s
    /// doc comment for why an implicit fallback isn't good enough here.
    fn generate_self_dispatch_error_clause(
        &mut self,
        var_prefix: &str,
        selector_atom: &str,
    ) -> Document<'static> {
        let nlr_token_var = self.fresh_var(&format!("{var_prefix}NlrTok"));
        let nlr_value_var = self.fresh_var(&format!("{var_prefix}NlrVal"));
        let nlr_state_var = self.fresh_var(&format!("{var_prefix}NlrSt"));
        let type_var = self.fresh_var(&format!("{var_prefix}Type"));
        let reason_var = self.fresh_var(&format!("{var_prefix}Reason"));
        let stack_var = self.fresh_var(&format!("{var_prefix}Stack"));
        let plain_error_var = self.fresh_var(&format!("{var_prefix}Plain"));
        let class_var = self.fresh_var(&format!("{var_prefix}Class"));
        let no_match_fallback = self.case_clause_fallback(&format!("{var_prefix}NoMatch"));
        let nlr_tuple_doc = || {
            docvec![
                "{'$bt_nlr', ",
                leaf::var(nlr_token_var.clone()),
                ", ",
                leaf::var(nlr_value_var.clone()),
                ", ",
                leaf::var(nlr_state_var.clone()),
                "}",
            ]
        };
        docvec![
            "<{'error', ",
            nlr_tuple_doc(),
            ", _}> when 'true' -> call 'erlang':'throw'(",
            nlr_tuple_doc(),
            ") ",
            "<{'error', {",
            leaf::var(type_var.clone()),
            ", ",
            leaf::var(reason_var.clone()),
            ", ",
            leaf::var(stack_var.clone()),
            "}, _}> when 'true' -> let ",
            leaf::var(class_var.clone()),
            " = call 'beamtalk_actor':'lookup_class'(call 'erlang':'self'()) in ",
            "call 'beamtalk_exception_handler':'reraise'(",
            leaf::var(type_var),
            ", ",
            leaf::var(reason_var),
            ", ",
            leaf::var(stack_var),
            ", ~{'selector' => ",
            leaf::atom(selector_atom.to_string()),
            ", 'class' => ",
            leaf::var(class_var),
            "}~) ",
            "<{'error', ",
            leaf::var(plain_error_var.clone()),
            ", _}> when 'true' -> call 'beamtalk_error':'raise'(",
            leaf::var(plain_error_var),
            ")",
            no_match_fallback,
            " ",
        ]
    }

    /// Handles self-sends inside actor methods.
    ///
    /// Returns `Some(())` if the receiver is `self` in an Actor context, `None` otherwise.
    ///
    /// ADR 0118 phase 2b: every position that threads a
    /// dispatching self-send's `NewState` now compiles it through
    /// [`Self::generate_self_dispatch`]'s producer directly (via
    /// `threaded_expression`/`thread_ahead`), substituting the already-
    /// sequenced value via `precompiled_subexprs` before `generate_message_send`
    /// ever reaches this function — so a self-send that gets here always
    /// falls through to [`Self::generate_discarding_self_dispatch`], the
    /// same *discarding* dispatch every un-migrated position always used.
    pub(super) fn try_handle_self_dispatch(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // ADR 0137 §3: a self-send in an actor closure is an ordinary message
        // to `Self` (the cell is current at home; a message to the actor away),
        // handled by the generic dispatch below.
        if self.context == CodeGenContext::Actor
            && !self.in_cell_closure()
            && let Expression::Identifier(id) = receiver
            && id.name == "self"
        {
            let doc = self.generate_discarding_self_dispatch(selector, arguments)?;
            return Ok(Some(doc));
        }
        Ok(None)
    }

    /// Generates synchronous self-dispatch for actor self-sends.
    ///
    /// When an actor method sends a message to `self`, we bypass the async
    /// `gen_server:cast` path and call `safe_dispatch/3` directly. This ensures
    /// the result is a value (not a Future), enabling recursive algorithms like
    /// factorial and fibonacci to work correctly.
    ///
    /// # Sealed Class Optimization
    ///
    /// ADR 0118 §Decision 2: the state-effecting *producer* for
    /// an Actor self-send — the one place a dispatching self-send is
    /// compiled in a state-threading context. Returns a [`ThreadedValue`]
    /// whose prelude is
    ///
    /// ```text
    /// Statement(let _SDn = case call 'm':'safe_dispatch'('sel', [Args], StateK) of … end in)
    /// Bind(State{K+1} <- call 'erlang':'element'(2, _SDn))
    /// ```
    ///
    /// and whose value is `call 'erlang':'element'(1, _SDn)` — exactly the
    /// `Statement` + real `Bind` pair `dispatch_self_send_as_bind`
    /// (`control_flow/conditionals.rs`) built for the planner, now owned
    /// here; that function is a thin adapter over
    /// [`Self::generate_self_dispatch_parts`].
    ///
    /// `arguments` are compiled by
    /// [`Self::generate_self_dispatch_call_doc_for`] exactly as before; a
    /// caller that must sequence state-effecting *arguments* ahead of the
    /// dispatch (the ADR §Decision 3 rule) does so before calling this —
    /// see `threaded_expression` (`util.rs`).
    ///
    /// `frame` is the [`FrameId`] the `Bind` belongs to — `FrameId::ROOT`
    /// for the flat Actor method body, a branch arm's own frame otherwise;
    /// `span` is the source span both prelude nodes are attributed to.
    pub(in crate::core_erlang) fn generate_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
        frame: FrameId,
        span: Span,
    ) -> Result<ThreadedValue> {
        let (prelude, dispatch_var) =
            self.generate_self_dispatch_parts(selector, arguments, frame, span)?;
        Ok(ThreadedValue {
            prelude,
            value: Self::self_dispatch_result_value(&dispatch_var),
        })
    }

    /// The pure `element(1, _SDn)` reference to a self-dispatch's reply —
    /// the single spelling shared by [`Self::generate_self_dispatch`]'s
    /// value and [`Self::try_handle_self_dispatch`]'s planner substitution.
    pub(in crate::core_erlang) fn self_dispatch_result_value(dispatch_var: &str) -> ValueRef {
        ValueRef::Doc(docvec![
            "call 'erlang':'element'(1, ",
            leaf::var(dispatch_var.to_string()),
            ")",
        ])
    }

    /// The `Statement` + `Bind` pair behind [`Self::generate_self_dispatch`],
    /// plus the dispatch tuple's variable name for callers that register
    /// it (the planner's `hoisted_self_send_results`). Mint order is
    /// unchanged from `dispatch_self_send_as_bind`: the dispatch temps and
    /// the `next_state_var` bump all happen inside
    /// [`Self::generate_self_dispatch_call_doc_for`], and the `Bind`'s
    /// source/target versions are read off the live counter either side
    /// of it.
    pub(in crate::core_erlang) fn generate_self_dispatch_parts(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
        frame: FrameId,
        span: Span,
    ) -> Result<(Vec<ThreadedStmt>, String)> {
        let source_version = self.state_version();
        let (call_doc, dispatch_var) =
            self.generate_self_dispatch_call_doc_for(selector, arguments)?;
        let target_version = self.state_version();
        // ADR 0137: a self-send that may reach `dispatch/4`'s default arm keeps its
        // explicit `{reply, R, State}` threading and has the cell stored before it
        // runs; built here, the one place every self-dispatch prelude is made.
        let dispatch = self.self_dispatch_statement(
            selector,
            call_doc,
            VersionedVar::new(VersionPrefix::State, source_version, frame),
            span,
        );
        let prelude = vec![
            dispatch,
            ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'erlang':'element'(2, ",
                    leaf::var(dispatch_var.clone()),
                    ")",
                ])),
                span,
            },
        ];
        Ok((prelude, dispatch_var))
    }

    /// For sealed classes, we skip the `safe_dispatch/3` try/catch overhead and
    /// call `dispatch/4` directly. Since sealed classes have all methods known at
    /// compile time, the error isolation overhead is unnecessary.
    ///
    /// # Generated Code (normal)
    ///
    /// ```erlang
    /// case call 'module':'safe_dispatch'('selector', [Args], State) of
    ///   <{'reply', Result, _NewState}> when 'true' -> Result
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end
    /// ```
    ///
    /// # Generated Code (sealed class)
    ///
    /// ```erlang
    /// let Self = call 'beamtalk_actor':'make_self'(State) in
    /// case call 'module':'dispatch'('selector', [Args], Self, State) of
    ///   <{'reply', Result, _NewState}> when 'true' -> Result
    ///   <{'error', {Type, Reason, Stacktrace}, _}> when 'true' ->
    ///       call 'beamtalk_exception_handler':'reraise'(Type, Reason, Stacktrace)
    ///   <{'error', Error, _}> when 'true' -> call 'beamtalk_error':'raise'(Error)
    /// end
    /// ```
    ///
    /// ADR 0118: this is the *discarding* form — the `NewState`
    /// the dispatch returns is dropped. It is reached only from
    /// [`Self::try_handle_self_dispatch`]'s fallback, i.e. for a self-send
    /// in a position no consumer has yet migrated to
    /// [`Self::generate_self_dispatch`]'s prelude form (a cascade message,
    /// a `match:` scrutinee, a `sort:` comparator, an interpolation
    /// segment after an effectful one, …) and that no planner pre-hoisted.
    /// Every such position is a row of
    /// `stdlib/test/actor_self_send_position_matrix_test.bt` gated on the
    /// ADR 0118 phase that migrates it; once phase 2b removes the last
    /// `Document`-only consumer this fallback becomes
    /// `generate_self_dispatch(..).close(.., CloseContext::Opaque)` and the
    /// drop a [`crate::core_erlang::threaded_ir::VerifyError::StateEffectEscapesExpression`]
    /// (§Decision 5). Byte-identical to the pre-ADR-0118 output by
    /// construction, so un-migrated positions are untouched.
    fn generate_discarding_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        self.note_self_dispatch(selector, arguments);
        // Sealed class optimization — skip safe_dispatch try/catch
        if self.is_class_sealed() {
            return self.generate_sealed_self_dispatch(selector, arguments);
        }

        let selector_atom = selector.name().to_string();
        let result_var = self.fresh_var("SelfResult");
        let state_var = self.fresh_var("SelfState");
        let current_state = self.current_state_var();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let error_clause = self.generate_self_dispatch_error_clause("SelfError", &selector_atom);

        let reply_body = leaf::var(result_var.clone());
        let doc = docvec![
            "case ",
            self.safe_dispatch_call_doc(selector_atom, args_doc, current_state),
            " of ",
            "<{'reply', ",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}> when 'true' -> ",
            reply_body,
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// ADR 0111 Addendum 5 (shape E2): the dispatch-call/
    /// case-clause portion of a self-dispatch, WITHOUT the trailing
    /// state-extraction `let` (the former `generate_self_dispatch_open` text;
    /// BT-3809 removed that wrapper once every caller took the `Bind` form) —
    /// factored out so
    /// `exception_handling.rs`'s per-arm `ThreadedIr` lowering can model the
    /// state-version bump as a real `Bind` (`BindOp::Direct`) instead of
    /// baking it into an opaque `Statement`'s text. Still mints the version
    /// bump itself (`next_state_var()`, right before
    /// `generate_self_dispatch_error_clause`, exactly where the un-split
    /// function always minted it) so every mint *after* this call keeps its
    /// original position — callers that need the bumped state's rendered
    /// name read it back via `current_state_var()` rather than consuming a
    /// return value, since a real `Bind`'s `render_bind` re-derives the same
    /// name from the version number, not from a string this function hands
    /// back.
    ///
    /// Takes the decomposed selector/arguments so callers that only hold those
    /// (not an owning `MessageSend` node) share one dispatch-call shape.
    #[allow(clippy::too_many_lines)] // Document-based sealed/normal dispatch branches
    pub(in crate::core_erlang) fn generate_self_dispatch_call_doc_for(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<(Document<'static>, String)> {
        {
            let selector_atom = selector.name().to_string();
            // `selector_atom` is moved into `call_doc` below (some
            // branches consume it via `leaf::atom`), so clone the value
            // needed for the error-clause breadcrumb before that happens.
            let selector_atom_for_error = selector_atom.clone();
            let dispatch_var = self.fresh_temp_var("SD");
            let result_var = self.fresh_var("SDResult");
            let state_var = self.fresh_var("SDState");
            let current_state = self.current_state_var();

            // Capture arguments via bridge (ADR 0018 Phase 0)
            let args_doc = self.capture_argument_list_doc(arguments)?;

            // Build the dispatch call (varies by sealed optimization level)
            let call_doc = if self.is_class_sealed() {
                let selector_name = selector.name().to_string();
                if self.sealed_method_selectors().contains(&selector_name) {
                    // Level 1: Direct __sealed_ call
                    let self_var = self.fresh_temp_var("SealedSelf");
                    let module = self.module_name.clone();
                    let comma = if arguments.is_empty() { "" } else { ", " };
                    docvec![
                        "let ",
                        leaf::var(self_var.clone()),
                        " = call 'beamtalk_actor':'make_self'(",
                        leaf::var(current_state.clone()),
                        ") in ",
                        "let ",
                        leaf::var(dispatch_var.clone()),
                        " = case call ",
                        leaf::atom(module),
                        ":",
                        leaf::atom(crate::core_erlang::selector_mangler::sealed_fn_name(
                            &selector_name
                        )),
                        "(",
                        args_doc,
                        Document::Str(comma),
                        leaf::var(self_var),
                        ", ",
                        leaf::var(current_state),
                        ") of ",
                    ]
                } else {
                    // Level 2: Direct dispatch/4 call
                    let self_var = self.fresh_temp_var("SealedSelf");
                    let module = self.module_name.clone();
                    docvec![
                        "let ",
                        leaf::var(self_var.clone()),
                        " = call 'beamtalk_actor':'make_self'(",
                        leaf::var(current_state.clone()),
                        ") in ",
                        "let ",
                        leaf::var(dispatch_var.clone()),
                        " = case call ",
                        leaf::atom(module),
                        ":'dispatch'(",
                        leaf::atom(selector_atom),
                        ", [",
                        args_doc,
                        "], ",
                        leaf::var(self_var),
                        ", ",
                        leaf::var(current_state),
                        ") of ",
                    ]
                }
            } else {
                // Normal: safe_dispatch/3
                docvec![
                    "let ",
                    leaf::var(dispatch_var.clone()),
                    " = case ",
                    self.safe_dispatch_call_doc(selector_atom, args_doc, current_state),
                    " of ",
                ]
            };

            // Result/error clauses. The state-version bump
            // (`next_state_var()`) stays exactly here — mint-order fidelity
            // — but its returned name is no longer consumed for rendering;
            // the `Bind`-based callers re-derive it from the version number
            // via `render_bind`.
            let _ = self.next_state_var();
            let ok_doc = docvec![
                "{",
                leaf::var(result_var.clone()),
                ", ",
                leaf::var(state_var.clone()),
                "}"
            ];
            let reply_body = ok_doc;
            let error_clause =
                self.generate_self_dispatch_error_clause("SDError", &selector_atom_for_error);
            let doc = docvec![
                call_doc,
                "<{'reply', ",
                leaf::var(result_var.clone()),
                ", ",
                leaf::var(state_var.clone()),
                "}> when 'true' -> ",
                reply_body,
                " ",
                error_clause,
                "end in ",
            ];

            Ok((doc, dispatch_var))
        }
    }

    /// Sealed-class self-dispatch (value-discarding — see
    /// `generate_self_dispatch`'s call site).
    ///
    /// Two levels of optimization:
    /// 1. **Known sealed method**: Direct function call to `__sealed_{selector}`,
    ///    bypassing both `safe_dispatch/3` and `dispatch/4` case matching.
    /// 2. **Unknown method** (inherited): Direct `dispatch/4` call, skipping
    ///    only the `safe_dispatch/3` try/catch overhead.
    fn generate_sealed_self_dispatch(
        &mut self,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let selector_name = selector.name().to_string();

        // Level 1: Direct call to standalone sealed method function
        if self.sealed_method_selectors().contains(&selector_name) {
            let selector_atom = selector.name().to_string();
            return self.generate_direct_sealed_call(&selector_name, &selector_atom, arguments);
        }

        // Level 2: Direct dispatch/4 call (skip safe_dispatch try/catch)
        let selector_atom = selector.name().to_string();
        let result_var = self.fresh_var("SealedResult");
        let self_var = self.fresh_temp_var("SealedSelf");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let error_clause = self.generate_self_dispatch_error_clause("SealedError", &selector_atom);
        let state_var = self.fresh_var("SealedState");
        let reply_body = leaf::var(result_var.clone());

        let doc = docvec![
            "let ",
            leaf::var(self_var.clone()),
            " = call 'beamtalk_actor':'make_self'(",
            leaf::var(current_state.clone()),
            ") in ",
            "case call ",
            leaf::atom(module),
            ":'dispatch'(",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "], ",
            leaf::var(self_var),
            ", ",
            leaf::var(current_state),
            ") of ",
            "<{'reply', ",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}> when 'true' -> ",
            reply_body,
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// Generates a direct call to a sealed method's standalone function.
    ///
    /// This is the most optimized self-dispatch path: calls `__sealed_{selector}`
    /// directly, bypassing `safe_dispatch`, dispatch, and case selector matching.
    fn generate_direct_sealed_call(
        &mut self,
        selector_name: &str,
        selector_atom: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        let result_var = self.fresh_var("SealedResult");
        let self_var = self.fresh_temp_var("SealedSelf");
        let current_state = self.current_state_var();
        let module = self.module_name.clone();

        let args_doc = self.capture_argument_list_doc(arguments)?;
        let comma = if arguments.is_empty() { "" } else { ", " };
        // `selector_atom` (from `MessageSelector::name`) is
        // the breadcrumb value — kept independent of `selector_name` (used
        // below for `sealed_fn_name` mangling) so a future change to either
        // mangling scheme can't silently desync the breadcrumb from the
        // dispatch atom used at the other call sites.
        let error_clause =
            self.generate_self_dispatch_error_clause("SealedDirectError", selector_atom);
        let state_var = self.fresh_var("SealedState");
        let reply_body = leaf::var(result_var.clone());

        let doc = docvec![
            "let ",
            leaf::var(self_var.clone()),
            " = call 'beamtalk_actor':'make_self'(",
            leaf::var(current_state.clone()),
            ") in ",
            "case call ",
            leaf::atom(module),
            ":",
            leaf::atom(crate::core_erlang::selector_mangler::sealed_fn_name(
                selector_name
            )),
            "(",
            args_doc,
            Document::Str(comma),
            leaf::var(self_var),
            ", ",
            leaf::var(current_state),
            ") of ",
            "<{'reply', ",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}> when 'true' -> ",
            reply_body,
            " ",
            error_clause,
            "end"
        ];

        Ok(doc)
    }

    /// Builds the shared `safe_dispatch/4` self-send fragment used by every
    /// non-sealed self-dispatch call site (self-cast, discarding
    /// self-dispatch and open self-dispatch).
    ///
    /// BT-3666: the callee module is looked up at run time from the actor's
    /// own `'__class_mod__'` state key — the module of the *instance's* class
    /// (each compiled actor's `init/1` stamps its own module there) — rather
    /// than bound statically to the module the send was compiled in. A
    /// method inherited from a superclass (or flattened in from a trait)
    /// that does `self foo` therefore reaches a subclass override of `foo`
    /// (the template-method pattern), exactly as `Actor>>self_dispatch/2`
    /// (aliased self-sends) already does. The `'maps':'get'` default is the
    /// compiling module, so a state without the key keeps the old binding.
    ///
    /// ```erlang
    /// let _Args = [Args] in
    /// let _CM = call 'maps':'get'('__class_mod__', State, 'module') in
    ///   case call 'erlang':'function_exported'(_CM, 'safe_dispatch', 4) of
    ///     <'true'>  -> call _CM:'safe_dispatch'('selector', _Args, Self, State)
    ///     <_>       -> call _CM:'safe_dispatch'('selector', _Args, State)
    ///   end
    /// ```
    ///
    /// BT-3692: the `function_exported` arm keeps a receiver module compiled by an
    /// older compiler (no `safe_dispatch/4`, e.g. a stale package beam subclassing a
    /// recompiled class) working through `/3` instead of raising `undef`.
    ///
    /// Sealed classes never reach this helper: they cannot be subclassed, so
    /// their self-sends stay statically bound (see the sealed branches).
    fn safe_dispatch_call_doc(
        &mut self,
        selector_atom: impl Into<String>,
        args_doc: Document<'static>,
        state_var: String,
    ) -> Document<'static> {
        let class_mod_var = self.fresh_temp_var("ClassMod");
        let args_var = self.fresh_temp_var("SDArgs");
        let selector = selector_atom.into();
        // The args are bound once so the two arms below do not duplicate (and
        // re-evaluate) the argument expressions.
        docvec![
            "let ",
            leaf::var(args_var.clone()),
            " = [",
            args_doc,
            "] in let ",
            leaf::var(class_mod_var.clone()),
            " = call 'maps':'get'('__class_mod__', ",
            leaf::var(state_var.clone()),
            ", ",
            leaf::atom(self.module_name.clone()),
            ") in case call 'erlang':'function_exported'(",
            leaf::var(class_mod_var.clone()),
            ", 'safe_dispatch', 4) of <'true'> when 'true' -> call ",
            leaf::var(class_mod_var.clone()),
            ":'safe_dispatch'(",
            leaf::atom(selector.clone()),
            ", ",
            leaf::var(args_var.clone()),
            ", ",
            // BT-3692: pass the in-scope `Self` instead of making the callee
            // rebuild it with `beamtalk_actor:make_self/1`.
            leaf::var("Self"),
            ", ",
            leaf::var(state_var.clone()),
            ") <_> when 'true' -> call ",
            leaf::var(class_mod_var),
            ":'safe_dispatch'(",
            leaf::atom(selector),
            ", ",
            leaf::var(args_var),
            ", ",
            leaf::var(state_var),
            ") end"
        ]
    }
}
