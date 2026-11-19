// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor `gen_server` callbacks: `handle_cast/2`, `handle_call/3` and `handle_info/2` (split from `callbacks`, BT-5041).
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::actor_state_access::{actor_state_call, actor_state_helper};

use crate::core_erlang::CoreErlangGenerator;
use crate::core_erlang::exception_forms::{
    CatchVars, build_caught_triple_doc, wrap_and_reraise_doc,
};
use crate::core_erlang::sentinel_tags;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::{ClassDefinition, Module};

use super::{
    CommittedStateKeys, PdictVars, arm, arm_inline, bind, case_block, clean_state_let, noreply,
};

impl CoreErlangGenerator {
    /// BT-3967 / BT-3968: wraps a `gen_server` callback body so its return
    /// value passes through `beamtalk_actor:apply_pending_actions/1`.
    ///
    /// `self stopWith: reason` inside a method cannot call
    /// `gen_server:stop/3` on its own process (`calling_self`), and
    /// `self continueWith: #sel` must run only after the current callback
    /// returns, so the runtime records both requests in the process
    /// dictionary instead; this wrapper is where they take effect — after
    /// the current dispatch has returned. A pending stop turns
    /// `{reply, R, S}` / `{noreply, S}` into `{stop, Reason, R, S}` /
    /// `{stop, Reason, S}`; otherwise pending continuations add a
    /// `{continue, {'$bt_continue', Queue}}` action, handled by the
    /// `'$bt_continue'` clause of the generated `handle_continue/2`. Any other
    /// return value, and every callback with no pending request, passes
    /// through unchanged. `clear_pending_actions/0` runs first, so only a
    /// request made by this callback's own dispatch can take effect.
    pub(super) fn pending_actions_wrap(body: Document<'static>) -> Document<'static> {
        bind(
            leaf::var("_PendingActionsCleared"),
            form::call("beamtalk_actor", "clear_pending_actions", []),
            // `in` sits on its own line here, a layout no `form::let_in`
            // produces.
            docvec![
                "let _CallbackResult =",
                nest(INDENT, docvec![line(), body]),
                line(),
                "in ",
                form::call(
                    "beamtalk_actor",
                    "apply_pending_actions",
                    [leaf::var("_CallbackResult")],
                ),
            ],
        )
    }

    /// BT-3968: `handle_call/3`'s dispatch behind the mailbox-boundary guard
    /// (`beamtalk_actor:mailbox_continuation_check/2`). `self continueWith:`
    /// is a direct in-process send and never arrives through the mailbox, so
    /// a call message requesting a continuation — directly or via a
    /// `perform:` — came from another process and is answered with
    /// `invalid_continuation` instead of being dispatched inside the actor,
    /// where the runtime's `self()` check could not tell.
    fn mailbox_call_guard(
        module_name: &ecow::EcoString,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        Self::mailbox_guard(
            "Selector",
            "Args",
            "ContErr",
            Self::handle_call_dispatch_case(module_name, state_keys),
            form::tuple([
                leaf::atom("reply"),
                form::tuple([leaf::atom("error"), leaf::var("ContErr")]),
                leaf::var("State"),
            ]),
        )
    }

    /// BT-3968: `handle_cast/2`'s fire-and-forget dispatch behind the same
    /// guard as [`Self::mailbox_call_guard`]; a rejected cast (logged by the
    /// runtime check) is dropped.
    fn mailbox_cast_guard(
        module_name: &ecow::EcoString,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        Self::mailbox_guard(
            "CastSelector",
            "CastArgs",
            "_ContErr",
            Self::cast_dispatch_case(module_name, state_keys),
            noreply(leaf::var("State")),
        )
    }

    fn mailbox_guard(
        selector_var: &'static str,
        args_var: &'static str,
        error_var: &'static str,
        accepted: Document<'static>,
        rejected: Document<'static>,
    ) -> Document<'static> {
        case_block(
            form::call(
                "beamtalk_actor",
                "mailbox_continuation_check",
                [leaf::var(selector_var), leaf::var(args_var)],
            ),
            [
                arm([leaf::atom("ok")], accepted),
                arm(
                    [form::tuple([leaf::atom("error"), leaf::var(error_var)])],
                    rejected,
                ),
            ],
        )
    }

    /// BT-3968: the `let` binding that drops the continuation queue in
    /// `handle_call/3`'s dispatch-error arm — the follow-up steps a failed
    /// dispatch requested are void. (The no-reply error arms crash the actor
    /// instead, which drops the whole queue; ADR 0134.)
    fn discard_continuations_let(body: Document<'static>) -> Document<'static> {
        bind(
            leaf::var("_ContDiscarded"),
            form::call("beamtalk_actor", "discard_pending_continuations", []),
            body,
        )
    }

    /// `let <old> = <install state_var> in body`: stashes `state_var` for
    /// re-entrant self-sends around `body`, which must close the bracket with
    /// [`Self::pdict_restore`] using the same `vars`.
    pub(super) fn pdict_stash(
        vars: &PdictVars,
        state_var: &str,
        body: Document<'static>,
    ) -> Document<'static> {
        bind(
            leaf::var(vars.old.clone()),
            actor_state_call(
                actor_state_helper::INSTALL,
                [leaf::var(state_var.to_string())],
            ),
            body,
        )
    }

    /// Wraps `body` in the uninstall of the stashed state and the erase of the
    /// call stack. `body` follows the dispatch result binding (for example
    /// `let _DispatchResult = ... in`).
    pub(super) fn pdict_restore(vars: &PdictVars, body: Document<'static>) -> Document<'static> {
        bind(
            leaf::var(vars.restore.clone()),
            actor_state_call(actor_state_helper::UNINSTALL, [leaf::var(vars.old.clone())]),
            bind(
                leaf::var(vars.clear.clone()),
                form::call("erlang", "erase", [leaf::atom(sentinel_tags::CALL_STACK)]),
                body,
            ),
        )
    }

    /// The inner `case safe_dispatch ... end` for a fire-and-forget cast (and
    /// a `continueWith:` step, which reuses it). A successful dispatch commits
    /// its new state and discards the result; a raising one crashes the actor
    /// through the shared no-reply error arms (ADR 0134, BT-3958).
    pub(super) fn cast_dispatch_case(
        module_name: &ecow::EcoString,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        let vars = PdictVars::reentrant();
        // Stash State for re-entrant self-sends
        Self::pdict_stash(
            &vars,
            "State",
            // safe_dispatch turns a raise into an {error, ...} value; the
            // error arms below re-raise it after the pdict is restored
            bind(
                leaf::var("_CastDispatchResult"),
                form::call(
                    module_name.clone(),
                    "safe_dispatch",
                    [
                        leaf::var("CastSelector"),
                        leaf::var("CastArgs"),
                        leaf::var("State"),
                    ],
                ),
                // Restore pdict
                Self::pdict_restore(
                    &vars,
                    case_block(leaf::var("_CastDispatchResult"), {
                        let [triple_arm, plain_arm] = noreply_error_arms("Cast");
                        [
                            arm(
                                [form::tuple([
                                    leaf::atom("reply"),
                                    leaf::var("_CastResult"),
                                    leaf::var("CastNewState"),
                                ])],
                                // strip codegen-internal `__local__` threading
                                // temporaries before persist + notify (see handle_call).
                                clean_state_let(
                                    "CleanCastNewState",
                                    "CastNewState",
                                    state_keys,
                                    // same per-object change push as handle_call, for
                                    // a state write committed via a fire-and-forget cast.
                                    bind(
                                        leaf::var("_CastStateChanged"),
                                        form::call(
                                            "beamtalk_actor",
                                            "notify_state_change",
                                            [leaf::var("State"), leaf::var("CleanCastNewState")],
                                        ),
                                        noreply(leaf::var("CleanCastNewState")),
                                    ),
                                ),
                            ),
                            // ADR 0134: re-raise, crashing the actor — shared with handle_info
                            triple_arm,
                            plain_arm,
                        ]
                    }),
                ),
            ),
        )
    }

    /// Generates the `handle_cast/2` callback for fire-and-forget (`!`) sends,
    /// `{cast, Selector, Args}` from `beamtalk_actor:cast_send/3`. The result
    /// is discarded; an error raised by the method crashes the actor (ADR
    /// 0134, BT-3958) through the error arms shared with `handle_info/2` —
    /// there is no caller to hand it to.
    ///
    /// ADR 0069 Phase 2b: Matches 4-tuple messages with `PropCtx`
    /// for both fire-and-forget casts and async sends. Falls back to 3-tuple
    /// for backward compatibility.
    ///
    /// ADR 0126 §5.1 (BT-3613): a leading clause recognises a
    /// `'$beamtalk_wire'`-tagged remote fire-and-forget cast by shape
    /// (6-tuple headed by that atom, `cast` kind) and hands it to
    /// `beamtalk_actor:decode_wire_cast/1` — the single place the tag's
    /// decode/version-check logic lives, shared with
    /// `beamtalk_actor.erl`'s own `handle_cast/2` (AGENTS.md "No duplicate
    /// implementations"). A decoded envelope re-dispatches, by a recursive
    /// self-call, through the `{'cast', ...}` clause below exactly as if it
    /// had arrived locally; a decode/version failure has already been
    /// logged and dropped by the shared helper. There is no `async`-kind
    /// wire shape: remote async sends are unsupported and raise at the
    /// sender (ADR 0126 Amendment BT-4989).
    pub(in crate::core_erlang) fn generate_handle_cast(
        &mut self,
        module: &Module,
    ) -> Document<'static> {
        let module_name = self.module_name.clone();
        let state_keys_owned = self.committed_state_keys(module);
        let state_keys = state_keys_owned.as_ref();
        let doc = docvec![
            form::fun_def(
                "handle_cast",
                [leaf::var("Msg"), leaf::var("State")],
                Self::pending_actions_wrap(case_block(
                    leaf::var("Msg"),
                    [
                        // Wire-tagged remote fire-and-forget cast (ADR 0126
                        // §5.1) — decode via the shared helper, then
                        // redispatch or drop.
                        arm(
                            [form::tuple([
                                leaf::atom(sentinel_tags::WIRE),
                                leaf::var("_WireVersion"),
                                leaf::atom("cast"),
                                leaf::var("_WireSelector"),
                                leaf::var("_WireArgs"),
                                leaf::var("_WirePropCtx"),
                            ])],
                            case_block(
                                form::call(
                                    "beamtalk_actor",
                                    "decode_wire_cast",
                                    [leaf::var("Msg")],
                                ),
                                [
                                    arm(
                                        [form::tuple([
                                            leaf::atom("redispatch"),
                                            leaf::var("WireMsg2"),
                                        ])],
                                        form::call(
                                            module_name.clone(),
                                            "handle_cast",
                                            [leaf::var("WireMsg2"), leaf::var("State")],
                                        ),
                                    ),
                                    arm([leaf::atom("noreply")], noreply(leaf::var("State"))),
                                ],
                            ),
                        ),
                        // Fire-and-forget cast with propagated context
                        arm(
                            [form::tuple([
                                leaf::atom("cast"),
                                leaf::var("CastSelector"),
                                leaf::var("CastArgs"),
                                leaf::var("CastPropCtx"),
                            ])],
                            bind(
                                leaf::var("_CastCtxOk"),
                                form::call(
                                    "beamtalk_actor",
                                    "restore_propagated_ctx",
                                    [leaf::var("CastPropCtx")],
                                ),
                                Self::mailbox_cast_guard(&module_name, state_keys),
                            ),
                        ),
                        // Fire-and-forget cast without context (backward compat)
                        arm(
                            [form::tuple([
                                leaf::atom("cast"),
                                leaf::var("CastSelector"),
                                leaf::var("CastArgs"),
                            ])],
                            Self::mailbox_cast_guard(&module_name, state_keys),
                        ),
                        arm_inline([leaf::var("_")], noreply(leaf::var("State"))),
                    ],
                )),
                Layout::Block,
            ),
            "\n",
            "\n",
        ];
        doc
    }

    /// Generates the `handle_call/3` callback for sync message sends.
    ///
    /// Uses `safe_dispatch/3` for error isolation and
    /// returns `{ok, Result}` or `{error, Error}` tuples.
    ///
    /// ADR 0069 Phase 2b: Matches 3-tuple `{Selector, Args, PropCtx}`
    /// to restore propagated context (`OTel` trace context) before dispatch.
    /// Falls back to 2-tuple `{Selector, Args}` for backward compatibility.
    ///
    /// ADR 0126 §5.1 (BT-3613): a leading clause recognises a
    /// `'$beamtalk_wire'`-tagged remote call by shape (6-tuple headed by
    /// that atom) and hands it to `beamtalk_actor:decode_wire_call/1` —
    /// the single place the tag's decode/version-check logic lives
    /// (`beamtalk_actor.erl`'s own `handle_call/3` calls the very same
    /// function for its hand-written `__methods__` path, AGENTS.md "No
    /// duplicate implementations"). A decoded envelope re-dispatches, by a
    /// recursive self-call, through the 3-tuple clause below exactly as if
    /// it had arrived locally; a decode/version failure's ready-made reply
    /// is returned with `State` untouched, without ever reaching
    /// `safe_dispatch/3`.
    pub(in crate::core_erlang) fn generate_handle_call(
        &mut self,
        module: &Module,
    ) -> Document<'static> {
        let module_name = self.module_name.clone();
        let dispatch_case =
            Self::mailbox_call_guard(&module_name, self.committed_state_keys(module).as_ref());
        let doc = docvec![
            form::fun_def(
                "handle_call",
                [leaf::var("Msg"), leaf::var("From"), leaf::var("State")],
                Self::pending_actions_wrap(case_block(
                    leaf::var("Msg"),
                    [
                        // Wire-tagged remote call (ADR 0126 §5.1) — decode via
                        // the shared helper, then redispatch or reply directly.
                        arm(
                            [form::tuple([
                                leaf::atom(sentinel_tags::WIRE),
                                leaf::var("_WireVersion"),
                                leaf::atom("call"),
                                leaf::var("_WireSelector"),
                                leaf::var("_WireArgs"),
                                leaf::var("_WirePropCtx"),
                            ])],
                            case_block(
                                form::call(
                                    "beamtalk_actor",
                                    "decode_wire_call",
                                    [leaf::var("Msg")],
                                ),
                                [
                                    arm(
                                        [form::tuple([
                                            leaf::atom("redispatch"),
                                            leaf::var("WireMsg2"),
                                        ])],
                                        form::call(
                                            module_name.clone(),
                                            "handle_call",
                                            [
                                                leaf::var("WireMsg2"),
                                                leaf::var("From"),
                                                leaf::var("State"),
                                            ],
                                        ),
                                    ),
                                    arm(
                                        [form::tuple([
                                            leaf::atom("reply"),
                                            leaf::var("WireReplyVal"),
                                        ])],
                                        form::tuple([
                                            leaf::atom("reply"),
                                            leaf::var("WireReplyVal"),
                                            leaf::var("State"),
                                        ]),
                                    ),
                                ],
                            ),
                        ),
                        // 3-tuple with propagated context — restore before dispatch
                        arm(
                            [form::tuple([
                                leaf::var("Selector"),
                                leaf::var("Args"),
                                leaf::var("PropCtx"),
                            ])],
                            bind(
                                leaf::var("_CtxOk"),
                                form::call(
                                    "beamtalk_actor",
                                    "restore_propagated_ctx",
                                    [leaf::var("PropCtx")],
                                ),
                                dispatch_case.clone(),
                            ),
                        ),
                        // Backward compat: 2-tuple without context
                        arm(
                            [form::tuple([leaf::var("Selector"), leaf::var("Args")])],
                            dispatch_case,
                        ),
                    ],
                )),
                Layout::Block,
            ),
            "\n",
            "\n",
        ];
        doc
    }

    /// BT-4090: the success arm's reply. A deferred call (`self deferReply`)
    /// is answered `{noreply, _}` *before* encoding, so the method's ignored
    /// return value is never encoded; otherwise the reply is wire-encoded and
    /// tagged exactly as described on [`Self::handle_call_dispatch_case`].
    fn success_reply_case() -> Document<'static> {
        case_block(
            leaf::var("_CallDeferred"),
            [
                arm([leaf::atom("true")], noreply(leaf::var("CleanNewState"))),
                arm(
                    [leaf::var("_NotDeferred")],
                    case_block(
                        form::call(
                            "beamtalk_actor",
                            "encode_reply_for_tagged",
                            [
                                leaf::var("From"),
                                leaf::var("Selector"),
                                leaf::var("Result"),
                            ],
                        ),
                        [
                            arm(
                                [form::tuple([leaf::atom("error"), leaf::var("EncErr")])],
                                form::tuple([
                                    leaf::atom("reply"),
                                    form::tuple([leaf::atom("error"), leaf::var("EncErr")]),
                                    leaf::var("CleanNewState"),
                                ]),
                            ),
                            arm(
                                [form::tuple([leaf::atom("ok"), leaf::var("EncodedResult")])],
                                form::tuple([
                                    leaf::atom("reply"),
                                    form::tuple([leaf::atom("ok"), leaf::var("EncodedResult")]),
                                    leaf::var("CleanNewState"),
                                ]),
                            ),
                        ],
                    ),
                ),
            ],
        )
    }

    /// Generates the inner `case safe_dispatch ... end` block for `handle_call`.
    /// Shared between 3-tuple (with `PropCtx`) and 2-tuple (backward compat) patterns.
    ///
    /// ADR 0126 §5.1/§5.5 (BT-3613): both arms route their payload through
    /// `beamtalk_actor` helpers shared with the hand-written dispatch path
    /// rather than re-deriving the same logic in codegen (AGENTS.md "No
    /// duplicate implementations") — `encode_reply_for/3` wire-encodes the
    /// reply when `From` is on another node (a no-op for a local caller,
    /// so this is byte-identical to before for every existing local-call
    /// test), and the error arm's `maybe_reclassify_compiled_dispatch_error/2`
    /// reclassifies a stale-block badfun/undef as `remote_code_mismatch`
    /// before that same encode step, leaving every other error shape
    /// untouched.
    ///
    /// The success arm branches on `encode_reply_for/3`'s own result rather
    /// than unconditionally wrapping it as `{'ok', EncodedResult}`: a
    /// wire-encode failure (e.g. the method returned a `HandleScoped` value
    /// to a remote caller) makes `encode_reply_for/3` return `{'error',
    /// EncErr}` instead of the encoded payload, per its own doc ("An encode
    /// failure never crashes the callee: it replaces the reply with
    /// `{error, #beamtalk_error{}}` instead"). Wrapping that in `{'ok', ...}`
    /// would hand the caller `{'ok', {'error', EncErr}}` — which decodes and
    /// unwraps back to the plain tuple `{'error', EncErr}` as a
    /// *successful* method return value instead of raising, silently
    /// breaking ADR 0126 §5.1's documented `not_serialisable` contract.
    ///
    /// Branching on `encode_reply_for/3`'s own (untagged) result is
    /// unsound here, though: that function is a no-op passthrough for a
    /// local caller, so a method that legitimately returns a raw `Tuple`
    /// shaped like `{error, X}` (a normal, first-class `BeamTalk` value —
    /// `stdlib/src/tuple.bt`) would be indistinguishable from a genuine
    /// encode failure by shape alone. `encode_reply_for_tagged/3` exists
    /// precisely for this call site: it always outer-tags its result
    /// `{ok, _}`/`{error, _}` — `ok` for both the remote-encoded-success
    /// case and the local passthrough case, `error` only for a genuine
    /// remote encode failure — so matching the outer tag never depends on
    /// what the payload itself looks like. (The hand-written
    /// `beamtalk_actor:handle_call/3` keeps using the untagged
    /// `encode_reply_for/3` unchanged — it never branches on the result,
    /// it builds the reply's own outer shape itself at each call site, so
    /// it was never at risk of this ambiguity.)
    fn handle_call_dispatch_case(
        module_name: &ecow::EcoString,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        let vars = PdictVars::reentrant();
        // Stash State for re-entrant self-sends
        Self::pdict_stash(
            &vars,
            "State",
            // BT-4090: expose the caller's `From` to `self deferReply`
            bind(
                leaf::var("_CallFromInstalled"),
                form::call("beamtalk_actor", "install_call_from", [leaf::var("From")]),
                // Use safe_dispatch for error isolation
                bind(
                    leaf::var("_DispatchResult"),
                    form::call(
                        module_name.clone(),
                        "safe_dispatch",
                        [leaf::var("Selector"), leaf::var("Args"), leaf::var("State")],
                    ),
                    // Restore pdict
                    Self::pdict_restore(
                        &vars,
                        // BT-4090: remove the `From` cell; `true` when the method deferred
                        // its reply. Only the success arm acts on it — error arms reply
                        // the error, so a raise after deferring never hangs the caller.
                        bind(
                            leaf::var("_CallDeferred"),
                            form::call("beamtalk_actor", "uninstall_call_from", []),
                            case_block(
                                leaf::var("_DispatchResult"),
                                [Self::call_success_arm(state_keys), Self::call_error_arm()],
                            ),
                        ),
                    ),
                ),
            ),
        )
    }

    /// `handle_call/3`'s success arm: `<{'reply', Result, NewState}>`.
    fn call_success_arm(state_keys: Option<&CommittedStateKeys>) -> Document<'static> {
        // Success case: return {ok, Result}
        arm(
            [form::tuple([
                leaf::atom("reply"),
                leaf::var("Result"),
                leaf::var("NewState"),
            ])],
            // strip codegen-internal `__local__` threading
            // temporaries before persist + notify, so an outer local
            // threaded through a control-flow desugar never leaks into
            // the committed actor state or a watch notification.
            clean_state_let(
                "CleanNewState",
                "NewState",
                state_keys,
                // notify the per-object change substrate so a
                // watched compiled actor's committed state write pushes
                // {object_changed, …} to the live Inspector — the runtime
                // beamtalk_actor path does this via log_dispatch_complete.
                bind(
                    leaf::var("_StateChanged"),
                    form::call(
                        "beamtalk_actor",
                        "notify_state_change",
                        [leaf::var("State"), leaf::var("CleanNewState")],
                    ),
                    // wire-encode the reply when the caller is remote
                    // (no-op locally — see `handle_call_dispatch_case`'s doc), then
                    // branch on the outer {ok,_}/{error,_} tag
                    // encode_reply_for_tagged/3 itself always adds —
                    // never on the payload's own shape.
                    //
                    // BT-4090: a deferred reply (`self deferReply`) is
                    // answered `{noreply, _}` *before* encoding — the
                    // method's ignored return value must not be encoded.
                    Self::success_reply_case(),
                ),
            ),
        )
    }

    /// `handle_call/3`'s error arm, `<{'error', Error, ErrState}>`.
    //
    // Error case: reclassify a stale-block badfun/undef, then
    // pass the (possibly reclassified) error opaquely to the
    // caller, wire-encoded when remote. Same branch-not-wrap
    // reasoning as the success arm: an encode failure
    // here would otherwise double-wrap as
    // {'error', {'error', EncErr}} instead of the flat
    // {'error', EncErr} shape decode_call_result expects.
    // Unlike the success arm, this one stays on the
    // untagged encode_reply_for/3, not the _tagged variant
    // — both of its branches already converge on the same
    // {'reply', {'error', _}, ErrState} outer shape, so
    // there's no {'ok', _} vs {'error', _} choice for a
    // payload shape collision to corrupt.
    fn call_error_arm() -> Document<'static> {
        let error_reply = |error: Document<'static>| {
            form::tuple([
                leaf::atom("reply"),
                form::tuple([leaf::atom("error"), error]),
                leaf::var("ErrState"),
            ])
        };
        arm(
            [form::tuple([
                leaf::atom("error"),
                leaf::var("Error"),
                leaf::var("ErrState"),
            ])],
            Self::discard_continuations_let(
                // BT-4850: a caught exception's raw trace is built here,
                // in the actor that caught it, so the caller's
                // `ensure_wrapped` keeps the raise-site frames.
                bind(
                    leaf::var("BuiltError"),
                    build_caught_triple_doc(leaf::var("Error"), "Reply"),
                    bind(
                        leaf::var("ClassifiedError"),
                        form::call(
                            "beamtalk_actor",
                            "maybe_reclassify_compiled_dispatch_error",
                            [leaf::var("Args"), leaf::var("BuiltError")],
                        ),
                        case_block(
                            form::call(
                                "beamtalk_actor",
                                "encode_reply_for",
                                [
                                    leaf::var("From"),
                                    leaf::var("Selector"),
                                    leaf::var("ClassifiedError"),
                                ],
                            ),
                            [
                                arm(
                                    [form::tuple([leaf::atom("error"), leaf::var("EncErr")])],
                                    error_reply(leaf::var("EncErr")),
                                ),
                                arm(
                                    [leaf::var("EncodedError")],
                                    error_reply(leaf::var("EncodedError")),
                                ),
                            ],
                        ),
                    ),
                ),
            ),
        )
    }

    /// Generates the `handle_info/2` callback (ADR 0065).
    ///
    /// BT-3596: In both branches, a non-parent `'EXIT'` message is
    /// intercepted first via `beamtalk_actor:handle_linked_exit/2`, before
    /// the Server-subclass `handleInfo:` dispatch and before the default
    /// ignore-all delegate — mirroring the crash-propagation semantics an
    /// *untrapped* link already has (this only ever fires for an actor that
    /// traps exits; see `generate_init_function`'s `trap_exit` gating). The
    /// `gen_server` loop's own parent-EXIT handling already calls
    /// `terminate/2` and exits before `handle_info/2` runs at all, so any
    /// `'EXIT'` reaching here is from some other link (e.g. a child the
    /// actor itself spawned and linked) — without this, that crash would be
    /// silently swallowed by the ignore-all default.
    ///
    /// For **Server subclasses**, dispatches to the user-defined `handleInfo:`
    /// method. If `handleInfo:` raises, the error is re-raised and the server
    /// crashes (ADR 0134, BT-3958): `gen_server` calls `terminate/2` with the
    /// error as its reason, emits a crash report, and the supervisor applies
    /// its restart policy. The error arms are shared with `handle_cast/2`
    /// (`noreply_error_arms`).
    ///
    /// For **plain Actor subclasses**, generates the default ignore-all stub that
    /// delegates to `beamtalk_actor:handle_info/2` — which itself checks for a
    /// linked `'EXIT'` first (see that function's doc comment).
    ///
    /// # Generated Code (Server subclass)
    ///
    /// ```erlang
    /// 'handle_info'/2 = fun (Msg, State) ->
    ///     case call 'beamtalk_actor':'handle_linked_exit'(Msg, State) of
    ///         <'pass'> when 'true' ->
    ///             case call 'Module':'safe_dispatch'('handleInfo:', [Msg], State) of
    ///                 <{'reply', _Result, NewState}> when 'true' -> {'noreply', NewState}
    ///                 <{'error', {InfoType, InfoReason, InfoStacktrace}, _InfoState}> when 'true' ->
    ///                     let InfoBuiltStack = primop 'build_stacktrace'(InfoStacktrace) in
    ///                     let InfoException = call 'beamtalk_exception_handler':'ensure_wrapped'(InfoType, InfoReason, InfoBuiltStack) in
    ///                     primop 'raw_raise'('error', InfoException, InfoStacktrace)
    ///                 <{'error', InfoError, _InfoState2}> when 'true' ->
    ///                     call 'beamtalk_exception_handler':'reraise'(InfoError)
    ///                 <_Other> when 'true' -> {'noreply', State}
    ///             end
    ///         <_ExitResult> when 'true' -> _ExitResult
    ///     end
    /// ```
    ///
    /// # Generated Code (plain Actor)
    ///
    /// ```erlang
    /// 'handle_info'/2 = fun (Msg, State) ->
    ///     call 'beamtalk_actor':'handle_info'(Msg, State)
    /// ```
    pub(in crate::core_erlang) fn generate_handle_info(
        &self,
        module: &Module,
    ) -> Document<'static> {
        if self.is_server_subclass {
            // BT-4052: baked flag - true when the class (or an ancestor other
            // than `Server`) defines `onLinkedExit:reason:`.
            let has_hook = self.current_class(module).is_some_and(|c| {
                self.class_or_ancestor_defines_linked_exit_hook(module, &c.name.name)
            });
            let state_keys_owned = self.committed_state_keys(module);
            let state_keys = state_keys_owned.as_ref();
            let dispatch_body = self.info_dispatch_doc("handleInfo:", "[Msg]", state_keys);
            let hook_arm = has_hook.then(|| {
                arm(
                    [form::tuple([
                        leaf::atom("hook"),
                        leaf::var("HookPid"),
                        leaf::var("HookReason"),
                    ])],
                    // The leading `line()` is the blank line this arm has
                    // always had (see `linked_exit_intercept_doc`).
                    docvec![
                        line(),
                        self.info_dispatch_doc(
                            "onLinkedExit:reason:",
                            "[HookPid, HookReason]",
                            state_keys,
                        ),
                    ],
                )
            });
            let doc = docvec![
                form::fun_def(
                    "handle_info",
                    [leaf::var("Msg"), leaf::var("State")],
                    Self::pending_actions_wrap(Self::linked_exit_intercept_doc(
                        dispatch_body,
                        hook_arm,
                        has_hook,
                    )),
                    Layout::Block,
                ),
                "\n",
                "\n",
            ];
            doc
        } else {
            let doc = docvec![
                form::fun_def(
                    "handle_info",
                    [leaf::var("Msg"), leaf::var("State")],
                    form::call(
                        "beamtalk_actor",
                        "handle_info",
                        [leaf::var("Msg"), leaf::var("State")],
                    ),
                    Layout::Block,
                ),
                "\n",
                "\n",
            ];
            doc
        }
    }

    /// The `safe_dispatch` + state-commit + error-arms block shared by the
    /// `handleInfo:` dispatch and (BT-4052) the `onLinkedExit:reason:` hook
    /// dispatch in `handle_info/2`. `args` is a Core Erlang list literal.
    fn info_dispatch_doc(
        &self,
        selector: &str,
        args: &'static str,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        let vars = PdictVars::reentrant();
        // Stash State for re-entrant self-sends
        Self::pdict_stash(
            &vars,
            "State",
            bind(
                leaf::var("_InfoDispatchResult"),
                form::call(
                    self.module_name.clone(),
                    "safe_dispatch",
                    [
                        leaf::atom(selector.to_string()),
                        Document::Str(args),
                        leaf::var("State"),
                    ],
                ),
                Self::pdict_restore(&vars, {
                    let [triple_arm, plain_arm] = noreply_error_arms("Info");
                    case_block(
                        leaf::var("_InfoDispatchResult"),
                        [
                            arm(
                                [form::tuple([
                                    leaf::atom("reply"),
                                    leaf::var("_Result"),
                                    leaf::var("NewState"),
                                ])],
                                // handle_info is an outermost state-commit
                                // boundary too - a Server `handleInfo:` that threads an
                                // outer local through a control-flow desugar must not
                                // persist `__local__` temps into the committed state.
                                clean_state_let(
                                    "CleanInfoNewState",
                                    "NewState",
                                    state_keys,
                                    noreply(leaf::var("CleanInfoNewState")),
                                ),
                            ),
                            // ADR 0134: re-raise, crashing the actor - shared with handle_cast
                            triple_arm,
                            plain_arm,
                            arm_inline([leaf::var("_Other")], noreply(leaf::var("State"))),
                        ],
                    )
                }),
            ),
        )
    }

    /// BT-3596: Wraps a `handle_info/2` body in a `beamtalk_actor:handle_linked_exit/3`
    /// interception `case`. That runtime helper only ever produces a non-`'pass'`
    /// result for a trapping actor's non-parent `'EXIT'` message (see its own doc
    /// comment) - for every other message, and for any actor not trapping exits at
    /// all, it returns `'pass'` and `inner_body` runs exactly as before.
    ///
    /// BT-4052: the third argument is the baked hook flag; when true the helper
    /// returns `{'hook', Pid, Reason}` for every non-parent `'EXIT'`, which
    /// `hook_arm` dispatches to `onLinkedExit:reason:`.
    fn linked_exit_intercept_doc(
        inner_body: Document<'static>,
        hook_arm: Option<Document<'static>>,
        has_hook: bool,
    ) -> Document<'static> {
        // The leading `line()` in both arm bodies is the blank line the
        // generated `handle_info/2` has always had after each arm head
        // (the body, which starts with its own line break, sits under the
        // arm's); kept so the generated Core Erlang stays byte-identical.
        let clauses = std::iter::once(arm([leaf::atom("pass")], docvec![line(), inner_body]))
            .chain(hook_arm)
            .chain(std::iter::once(arm_inline(
                [leaf::var("_ExitResult")],
                leaf::var("_ExitResult"),
            )));
        case_block(
            form::call(
                "beamtalk_actor",
                "handle_linked_exit",
                [
                    leaf::var("Msg"),
                    leaf::var("State"),
                    leaf::atom(if has_hook { "true" } else { "false" }.to_string()),
                ],
            ),
            clauses,
        )
    }

    /// Whether `class`'s superclass is itself a compiled actor module whose
    /// generated callbacks (`init/1`'s inherited state, BT-4567's
    /// `'__beamtalk_secrets'/0`) this class chains to: any superclass other
    /// than the `Actor`/`Object` roots.
    pub(super) fn has_parent_actor_module(class: &ClassDefinition) -> bool {
        class.superclass.as_ref().is_some_and(|s| {
            !s.name.eq_ignore_ascii_case("Actor") && !s.name.eq_ignore_ascii_case("Object")
        })
    }
}

/// Raise tail of the triple-format noreply arm (BT-4057).
///
/// `exit` with `normal`/`shutdown` is a control signal, not a crash: keep the
/// raw reason so the actor stops normally. Everything else is raised as the
/// wrapped error.
fn noreply_raise_tail(
    type_v: Document<'static>,
    reason_v: Document<'static>,
    stack_v: Document<'static>,
    wrapped_v: Document<'static>,
) -> Document<'static> {
    case_block(
        type_v,
        [
            form::clause(
                [leaf::atom("exit")],
                Some(form::call(
                    "erlang",
                    "or",
                    [
                        form::call("erlang", "=:=", [reason_v.clone(), leaf::atom("normal")]),
                        form::call("erlang", "=:=", [reason_v.clone(), leaf::atom("shutdown")]),
                    ],
                )),
                form::primop("raw_raise", [leaf::atom("exit"), reason_v, stack_v.clone()]),
                Layout::Block,
            ),
            arm(
                [leaf::var("_")],
                form::primop("raw_raise", [leaf::atom("error"), wrapped_v, stack_v]),
            ),
        ],
    )
}

/// Generates the two error case arms shared by every `gen_server` callback
/// that dispatches a message with no reply channel: `handle_cast/2` (`!`
/// sends), a Server's `handle_info/2` (`handleInfo:`) and the
/// `'$bt_continue'` step of `handle_continue/2` (`continueWith:`, which
/// reuses the cast dispatch case).
///
/// ADR 0134 (BT-3958): there is no caller to hand the error to, so the actor
/// crashes the OTP way: both arms raise, `gen_server` calls `terminate/2`
/// with `{Exception, Stacktrace}` as its reason, emits a crash report naming
/// the last message, and the supervisor applies its restart policy.
///
/// Both arms raise the error as a wrapped Exception object (an `error`-class
/// raise of `beamtalk_exception_handler:ensure_wrapped`'s result), so
/// `terminate:` and `onExit:` see the same kind of reason whatever was
/// raised — a Beamtalk `Error signal:`, a raw Erlang `badarith`, an abnormal
/// `exit` or a `throw` — and the same as a runtime-path actor
/// (`beamtalk_actor:dispatch_cast/3`). Raising as `error` also matters for a
/// `throw`: `gen_server` takes a throw escaping a callback as the callback's
/// *return value*, so re-raising it as a throw would install the thrown term
/// as the next state.
///
/// The one exception (BT-4057, ADR 0134 amendment): an `exit` whose reason is
/// `normal` or `shutdown` is a control signal, not a crash, and the triple
/// arm re-raises it as `exit` with the original raw reason so the actor stops
/// normally (see [`noreply_raise_tail`]).
///
/// 1. **Triple-format error** `{'error', {Type, Reason, RawStack}, _State}`:
///    the three-tuple produced by `safe_dispatch`'s `catch`, whose third
///    element is the raw trace the catch clause binds (see
///    `generate_safe_dispatch`). The trace is built for the Exception's
///    `stacktrace` field and the raise keeps the original trace
///    (`primop 'raw_raise'`, since `erlang:raise/3` rejects a raw trace).
/// 2. **Plain-format fallback** `{'error', Error, _State2}`: DNU,
///    `#beamtalk_error{}`, a relayed non-local return, or any non-triple
///    error from dispatch. Raised by `beamtalk_exception_handler:reraise/1`,
///    which the runtime-path actor's `dispatch_cast/3` uses too.
///
/// `var_prefix` is the capitalised variable-name prefix (e.g. `"Cast"` →
/// `CastType`, `CastReason`, …; `"Info"` → `InfoType`, …).  Variable names are
/// produced via [`leaf::var`] to satisfy the typed-leaf rule (ADR 0089).
fn noreply_error_arms(var_prefix: &str) -> [Document<'static>; 2] {
    // Compute variable name strings from the prefix, then pass to leaf::var.
    // format! here derives the *input* to the typed-leaf API, not a raw Core
    // Erlang fragment — this is the sanctioned pattern per ADR 0089.
    let type_v = leaf::var(format!("{var_prefix}Type"));
    let reason_v = leaf::var(format!("{var_prefix}Reason"));
    let stack_v = leaf::var(format!("{var_prefix}Stacktrace"));
    let built_v = leaf::var(format!("{var_prefix}BuiltStack"));
    let wrapped_v = leaf::var(format!("{var_prefix}Exception"));
    let state1_v = leaf::var(format!("_{var_prefix}State"));
    let error_v = leaf::var(format!("{var_prefix}Error"));
    let state2_v = leaf::var(format!("_{var_prefix}State2"));

    [
        // Triple-format arm — wrap, then raise keeping the original trace
        form::clause(
            [form::tuple([
                leaf::atom("error"),
                form::tuple([type_v.clone(), reason_v.clone(), stack_v.clone()]),
                state1_v,
            ])],
            Some(leaf::atom("true")),
            wrap_and_reraise_doc(
                &CatchVars {
                    class: type_v.clone(),
                    reason: reason_v.clone(),
                    stack: stack_v.clone(),
                },
                built_v,
                wrapped_v.clone(),
                None,
                noreply_raise_tail(type_v, reason_v, stack_v, wrapped_v),
                Layout::Block,
            ),
            Layout::Block,
        ),
        // Plain-format fallback — {error, Error, State} (DNU, #beamtalk_error{}, etc.)
        arm(
            [form::tuple([
                leaf::atom("error"),
                error_v.clone(),
                state2_v,
            ])],
            form::call("beamtalk_exception_handler", "reraise", [error_v]),
        ),
    ]
}
