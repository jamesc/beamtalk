// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor `gen_server` callbacks: `handle_continue/2` (queued actions and the `initialize` chain) (split from `callbacks`, BT-5041).
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::CoreErlangGenerator;
use crate::core_erlang::sentinel_tags;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::Module;

use super::{LogAndStop, PdictVars, arm, arm_inline, bind, case_block, log_and_stop_arm, noreply};

impl CoreErlangGenerator {
    /// Generates the `handle_continue/2` callback.
    ///
    /// ADR 0078: Auto-chains `initialize` methods parent-first across
    /// the full inheritance hierarchy. For each ancestor class that defines its
    /// own `initialize`, we emit a `safe_dispatch` call, threading the state
    /// from one initializer to the next. After the chain completes, the
    /// post-initialize check validates that every typed-no-default
    /// field (from any class in the chain) was set.
    ///
    /// Dispatches run via `beamtalk_dispatch:lookup/5` with each class's own
    /// compiled module name so that dispatch starts *at* that class and walks
    /// upward — which finds the class's own `initialize` if present, bypassing
    /// any overriding subclass method (this is `super`-like semantics).
    ///
    /// This runs after `init/1` returns, so the `gen_server` message loop is
    /// active and self-sends do not deadlock. OTP guarantees no other messages
    /// are processed before `handle_continue`.
    ///
    /// # Generated Code (single-class — no user ancestors)
    ///
    /// ```erlang
    /// 'handle_continue'/2 = fun (Continue, State) ->
    ///     case Continue of
    ///         <'initialize'> when 'true' ->
    ///             <stash State> in
    ///             let _InitResult0 = call 'module':'safe_dispatch'('initialize', [], State) in
    ///             <restore State> in
    ///             case _InitResult0 of
    ///                 <{'reply', _InitRet0, InitNewState}> when 'true' ->
    ///                     <post-init check on InitNewState>
    ///                 ...errors...
    ///             end
    ///         <_> when 'true' -> {'noreply', State}
    ///     end
    /// ```
    ///
    /// # Generated Code (with ancestor chain `Grandparent -> Parent -> Child`)
    ///
    /// Grandparent runs first, its output state feeds Parent, whose output
    /// feeds Child, whose output is validated.
    #[allow(clippy::too_many_lines)] // chain dispatch + error arms grow with hierarchy depth
    pub(in crate::core_erlang) fn generate_handle_continue(
        &self,
        module: &Module,
    ) -> Document<'static> {
        let module_name = self.module_name.clone();

        // Find typed-no-default fields (leaf + ancestors) for
        // post-initialize validation.
        let current_class = self.current_class(module);
        let leaf_class_name =
            current_class.map_or_else(|| module_name.clone(), |c| c.name.name.clone());
        let state_keys_owned = self.committed_state_keys(module);
        let state_keys = state_keys_owned.as_ref();
        let success_body = self.generate_post_initialize_check(
            current_class,
            module,
            state_keys,
            &leaf_class_name,
        );

        // ADR 0078: Walk the user-defined initialize chain (parent-first).
        // Each entry is the (class_name, compiled_module_name) for a class that
        // defines its own `initialize`. Root classes (Actor/Object/ProtoObject)
        // are filtered out since their `initialize` is a no-op default.
        let chain = current_class
            .map(|c| self.user_defined_initialize_chain(module, &c.name.name))
            .unwrap_or_default();

        // Body that runs after the dispatch chain: the post-initialize check.
        // If the chain is empty (e.g. only typed-no-default fields exist with no
        // initialize method), the post-init check runs directly against the
        // inbound `State` by binding it to `InitNewState`.
        let inner = if chain.is_empty() {
            bind(leaf::var("InitNewState"), leaf::var("State"), success_body)
        } else {
            // Build the dispatch chain from last to first so each step can nest
            // inside the previous one's success arm.
            let mut inner: Document<'static> = success_body;
            let n = chain.len();
            for (i, class) in chain.iter().enumerate().rev() {
                let idx = i.to_string();
                let is_last = i + 1 == n;
                let in_state_var = if i == 0 {
                    "State".to_string()
                } else {
                    format!("InitState{i}")
                };
                let result_var = format!("_InitResult{idx}");
                let reply_ret_var = format!("_InitRet{idx}");
                let next_state_var = if is_last {
                    "InitNewState".to_string()
                } else {
                    let next = i + 1;
                    format!("InitState{next}")
                };
                let err_state_var = format!("_InitErrState{idx}");
                let err_state_var2 = format!("_InitErrState{idx}b");
                let err_triple_type = format!("_InitType{idx}");
                let err_triple_reason = format!("_InitReason{idx}");
                let err_triple_stack = format!("_InitStack{idx}");
                let err_plain = format!("_InitError{idx}");
                let err_msg = format!("_InitErrMsg{idx}");
                let err_msg2 = format!("_InitErrMsg{idx}b");
                let err_class_name_var = format!("_InitErrClass{idx}");
                let err_class_name_var2 = format!("_InitErrClass{idx}b");

                let pdict = PdictVars::chain_step(&idx);

                let dispatch = form::call(
                    class.module_name.clone(),
                    "safe_dispatch",
                    [
                        leaf::atom("initialize"),
                        Document::Str("[]"),
                        leaf::var(in_state_var.clone()),
                    ],
                );
                let success_arm = arm(
                    [form::tuple([
                        leaf::atom("reply"),
                        leaf::var(reply_ret_var),
                        leaf::var(next_state_var),
                    ])],
                    inner,
                );
                // Destructure error triple to capture stacktrace
                let triple_arm = arm(
                    [form::tuple([
                        leaf::atom("error"),
                        form::tuple([
                            leaf::var(err_triple_type.clone()),
                            leaf::var(err_triple_reason.clone()),
                            leaf::var(err_triple_stack.clone()),
                        ]),
                        leaf::var(err_state_var.clone()),
                    ])],
                    log_and_stop_arm(LogAndStop {
                        msg_var: err_msg,
                        class_name_var: err_class_name_var,
                        module_name: &class.module_name,
                        format_safe_args: docvec![
                            form::tuple([
                                leaf::var(err_triple_type.clone()),
                                leaf::var(err_triple_reason.clone()),
                            ]),
                            ", ",
                            leaf::var(err_triple_stack.clone()),
                        ],
                        formatted_var: None,
                        reason: form::tuple([
                            leaf::var(err_triple_type.clone()),
                            leaf::var(err_triple_reason.clone()),
                        ]),
                        stacktrace: Some(leaf::var(err_triple_stack.clone())),
                        stop_reason: form::tuple([
                            leaf::var(err_triple_type),
                            leaf::var(err_triple_reason),
                            leaf::var(err_triple_stack),
                        ]),
                        stop_state: leaf::var(err_state_var),
                    }),
                );
                // Fallback: plain {error, Error, State} from dispatch (DNU, #beamtalk_error{}, etc.)
                let plain_arm = arm(
                    [form::tuple([
                        leaf::atom("error"),
                        leaf::var(err_plain.clone()),
                        leaf::var(err_state_var2.clone()),
                    ])],
                    log_and_stop_arm(LogAndStop {
                        msg_var: err_msg2,
                        class_name_var: err_class_name_var2,
                        module_name: &class.module_name,
                        format_safe_args: leaf::var(err_plain.clone()),
                        formatted_var: None,
                        reason: leaf::var(err_plain.clone()),
                        stacktrace: None,
                        stop_reason: leaf::var(err_plain),
                        stop_state: leaf::var(err_state_var2),
                    }),
                );

                inner = Self::pdict_stash(
                    &pdict,
                    &in_state_var,
                    bind(
                        leaf::var(result_var.clone()),
                        dispatch,
                        Self::pdict_restore(
                            &pdict,
                            case_block(leaf::var(result_var), [success_arm, triple_arm, plain_arm]),
                        ),
                    ),
                );
            }
            inner
        };

        let wrapped = Self::pending_actions_wrap(case_block(
            leaf::var("Continue"),
            [
                // The auto-chained dispatch body. Each class's initialize is
                // bracketed by its own pdict stash/restore pair to preserve
                // re-entrant self-send semantics.
                arm([leaf::atom("initialize")], inner),
                // BT-3968: one queued `self continueWith:`
                // step. The rest of the queue is seeded back
                // first (FIFO: a request made by this step
                // runs after them), then the step dispatches
                // through the very same case as a
                // fire-and-forget cast — pdict stash/restore,
                // strip_local_temps, notify_state_change and
                // the shared no-reply error arms, so a raising
                // step crashes the actor (ADR 0134) — by binding
                // CastSelector / CastArgs in the pattern.
                arm(
                    [form::tuple([
                        leaf::atom(sentinel_tags::CONTINUE),
                        docvec!["[{CastSelector, CastArgs}|ContRest]"],
                    ])],
                    bind(
                        leaf::var("_ContSeeded"),
                        form::call(
                            "beamtalk_actor",
                            "seed_pending_continuations",
                            [leaf::var("ContRest")],
                        ),
                        Self::cast_dispatch_case(&module_name, state_keys),
                    ),
                ),
                arm_inline([leaf::var("_")], noreply(leaf::var("State"))),
            ],
        ));

        let doc = docvec![
            form::fun_def(
                "handle_continue",
                [leaf::var("Continue"), leaf::var("State")],
                docvec![
                    // BT-3992: once the `initialize` step (with its pending
                    // actions applied) has its final result, tell the starter
                    // startup is over — before OTP runs any continuation
                    // `initialize` requested. A no-op for every other step.
                    //
                    // `in` sits on its own line here, a layout no
                    // `form::let_in` produces.
                    "let _ContinueResult =",
                    nest(INDENT, docvec![line(), wrapped]),
                    line(),
                    "in ",
                    form::call(
                        "beamtalk_actor",
                        "ack_initialized",
                        [leaf::var("Continue"), leaf::var("_ContinueResult")],
                    ),
                ],
                Layout::Block,
            ),
            "\n",
            "\n",
        ];
        doc
    }
}
