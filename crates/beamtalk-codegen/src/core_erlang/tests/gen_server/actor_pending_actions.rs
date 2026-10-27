// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3967 / BT-3968: every generated `gen_server` callback that runs a
//! Beamtalk dispatch passes its return value through
//! `beamtalk_actor:apply_pending_actions/1`, so a `self stopWith: reason` or
//! `self continueWith: #sel` issued during the dispatch takes effect once the
//! callback returns — see `gen_server::callbacks::pending_actions_wrap`. The
//! generated `handle_continue/2` runs a queued continuation through the same
//! dispatch case as a fire-and-forget cast.

use super::*;

const APPLY_PENDING_ACTIONS: &str =
    "call 'beamtalk_actor':'apply_pending_actions'(_CallbackResult)";
const CLEAR_PENDING_ACTIONS: &str = "call 'beamtalk_actor':'clear_pending_actions'()";
const DISCARD_CONTINUATIONS: &str = "call 'beamtalk_actor':'discard_pending_continuations'()";
const CONTINUE_CLAUSE: &str =
    "<{'$bt_continue', [{CastSelector, CastArgs}|ContRest]}> when 'true' ->";
const SEED_CONTINUATIONS: &str = "call 'beamtalk_actor':'seed_pending_continuations'(ContRest)";

/// Every `gen_server` callback the actor codegen can emit.
const CALLBACK_HEADERS: [&str; 4] = [
    "'handle_continue'/2 = fun",
    "'handle_cast'/2 = fun",
    "'handle_call'/3 = fun",
    "'handle_info'/2 = fun",
];

/// Any callback that runs a Beamtalk dispatch must clear a stale request on
/// entry and apply its own on exit — so a callback added later that
/// dispatches but forgets the wrap fails here.
fn assert_dispatching_callbacks_wrapped(code: &str) -> usize {
    let mut dispatching = 0;
    for header in CALLBACK_HEADERS {
        let body = extract_core_fn(code, header)
            .unwrap_or_else(|| panic!("{header} must be present in generated code"));
        if body.contains("'safe_dispatch'(") {
            dispatching += 1;
            assert!(
                body.contains(CLEAR_PENDING_ACTIONS) && body.contains(APPLY_PENDING_ACTIONS),
                "{header} dispatches, so it must clear and apply the pending actions. Got:\n{body}"
            );
        }
    }
    dispatching
}

fn generate(src: &str, module_name: &str) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    generate_module(&module, CodegenOptions::new(module_name)).expect("codegen should succeed")
}

#[test]
fn test_dispatching_callbacks_apply_pending_actions() {
    let code = generate(
        concat!(
            "Actor subclass: Stopper\n",
            "  state: value = 0\n\n",
            "  initialize => self.value := 1\n\n",
            "  bye => self stopWith: #shutdown\n",
        ),
        "stopper",
    );
    // handle_continue (initialize + continuations), handle_cast and
    // handle_call dispatch.
    assert_eq!(assert_dispatching_callbacks_wrapped(&code), 3);
}

#[test]
fn test_server_handle_info_applies_pending_actions() {
    let code = generate(
        concat!(
            "Server subclass: InfoStopper\n\n",
            "  handleInfo: msg => self stopWith: msg\n",
        ),
        "info_stopper",
    );
    // A Server's handle_info dispatches handleInfo: alongside
    // handle_continue (continuations), handle_cast and handle_call.
    assert_eq!(assert_dispatching_callbacks_wrapped(&code), 4);
    let info = extract_core_fn(&code, "'handle_info'/2 = fun").expect("handle_info/2");
    assert!(info.contains(APPLY_PENDING_ACTIONS), "Got:\n{info}");
}

/// BT-3968: `handle_continue/2` has a `'$bt_continue'` clause — ahead of the
/// catch-all — that seeds the rest of the queue and dispatches the step with
/// the same `safe_dispatch(CastSelector, CastArgs, State)` case a cast uses.
#[test]
fn test_handle_continue_runs_queued_continuation_like_a_cast() {
    let code = generate(
        concat!(
            "Actor subclass: Continuer\n",
            "  state: value = 0\n\n",
            "  poke => self continueWith: #bump\n\n",
            "  bump => self.value := self.value + 1\n",
        ),
        "continuer",
    );
    let cont = extract_core_fn(&code, "'handle_continue'/2 = fun").expect("handle_continue/2");
    let clause = cont.find(CONTINUE_CLAUSE).unwrap_or_else(|| {
        panic!("handle_continue must have the '$bt_continue' clause. Got:\n{cont}")
    });
    let catch_all = cont
        .find("<_> when 'true' -> {'noreply', State}")
        .expect("handle_continue catch-all");
    assert!(
        clause < catch_all,
        "the continue clause must precede the catch-all"
    );
    let step = &cont[clause..catch_all];
    assert!(step.contains(SEED_CONTINUATIONS), "Got:\n{step}");
    assert!(
        step.contains("'safe_dispatch'(CastSelector, CastArgs, State)"),
        "Got:\n{step}"
    );
    // Shares the cast error arms, which crash the actor on a dispatch error
    // (BT-3958, ADR 0134).
    assert!(
        step.contains("call 'beamtalk_exception_handler':'reraise'(CastError)"),
        "Got:\n{step}"
    );
}

/// BT-3992: startup ends when `initialize` returns. `init/1` hands the
/// starter's ack target to `take_init_ack/2` (`'initializing'`, since an
/// `initialize` step follows), and `handle_continue/2` passes its final
/// result — after the pending actions were applied, so a `self stopWith:` in
/// `initialize` still fails the start — through `ack_initialized/2`.
#[test]
fn test_initialize_step_acks_starter_after_pending_actions() {
    let code = generate(
        concat!(
            "Actor subclass: Acker\n",
            "  state: value = 0\n\n",
            "  initialize => self continueWith: #bump\n\n",
            "  bump => self.value := self.value + 1\n",
        ),
        "acker",
    );
    let init = extract_core_fn(&code, "'init'/1 = fun").expect("init/1");
    assert!(
        init.contains("call 'beamtalk_actor':'take_init_ack'(CleanState1, 'initializing')"),
        "Got:\n{init}"
    );
    assert!(
        init.contains("{'ok', AckState, {'continue', 'initialize'}}"),
        "Got:\n{init}"
    );
    let cont = extract_core_fn(&code, "'handle_continue'/2 = fun").expect("handle_continue/2");
    let apply = cont
        .find(APPLY_PENDING_ACTIONS)
        .expect("apply_pending_actions");
    let ack = cont
        .find("call 'beamtalk_actor':'ack_initialized'(Continue, _ContinueResult)")
        .unwrap_or_else(|| panic!("handle_continue must ack initialize. Got:\n{cont}"));
    assert!(
        apply < ack,
        "the ack must see the applied result. Got:\n{cont}"
    );
}

/// BT-3968: `handle_call`'s dispatch-error arm drops the continuation queue —
/// the failed dispatch's state rolls back, so its follow-ups are void. The
/// no-reply error arms (`handle_cast`, `handle_info`, and the continuation
/// step that reuses the cast case) re-raise instead, crashing the actor
/// (BT-3958, ADR 0134), which drops the whole queue.
#[test]
fn test_dispatch_error_arms_discard_continuations_or_crash() {
    let code = generate(
        concat!(
            "Server subclass: Discarder\n\n",
            "  handleInfo: msg => self continueWith: #noop\n\n",
            "  noop => nil\n",
        ),
        "discarder",
    );
    let call = extract_core_fn(&code, "'handle_call'/3 = fun").expect("handle_call/3");
    let err_arm = call
        .find("<{'error', Error, ErrState}> when 'true' ->")
        .expect("handle_call error arm");
    assert!(
        call[err_arm..].contains(DISCARD_CONTINUATIONS),
        "handle_call's error arm must discard. Got:\n{call}"
    );
    for (header, prefix) in [
        ("'handle_cast'/2 = fun", "Cast"),
        ("'handle_info'/2 = fun", "Info"),
        ("'handle_continue'/2 = fun", "Cast"),
    ] {
        let body = extract_core_fn(&code, header).expect(header);
        // Both no-reply error arms (triple + plain) raise the error as a
        // wrapped Exception object (an `error`-class raise, so gen_server
        // never takes a throw as the callback's return value); none logs
        // and carries on.
        let wrap = format!(
            "call 'beamtalk_exception_handler':'ensure_wrapped'({prefix}Type, {prefix}Reason, {prefix}BuiltStack)"
        );
        let triple = format!("primop 'raw_raise'('error', {prefix}Exception, {prefix}Stacktrace)");
        let plain = format!("call 'beamtalk_exception_handler':'reraise'({prefix}Error)");
        assert!(body.contains(&wrap), "{header}: {wrap}. Got:\n{body}");
        assert!(body.contains(&triple), "{header}: {triple}. Got:\n{body}");
        assert!(body.contains(&plain), "{header}: {plain}. Got:\n{body}");
        assert!(
            !body.contains(DISCARD_CONTINUATIONS),
            "{header} must not discard and continue. Got:\n{body}"
        );
        assert!(
            !body.contains("'logger':'warning'"),
            "{header} must not log and continue. Got:\n{body}"
        );
    }
}

/// BT-3968: `handle_call`/`handle_cast` reject a mailbox message requesting
/// a continuation (`beamtalk_actor:mailbox_continuation_check/2`) before
/// dispatching it; `handle_continue`'s own step is not guarded.
#[test]
fn test_mailbox_entries_guard_continuation_requests() {
    let code = generate(
        concat!(
            "Actor subclass: Guarded\n",
            "  state: value = 0\n\n",
            "  poke => self continueWith: #bump\n\n",
            "  bump => self.value := self.value + 1\n",
        ),
        "guarded",
    );
    let call = extract_core_fn(&code, "'handle_call'/3 = fun").expect("handle_call/3");
    assert_eq!(
        call.matches("call 'beamtalk_actor':'mailbox_continuation_check'(Selector, Args)")
            .count(),
        2,
        "both handle_call dispatch clauses are guarded. Got:\n{call}"
    );
    let cast = extract_core_fn(&code, "'handle_cast'/2 = fun").expect("handle_cast/2");
    assert_eq!(
        cast.matches("call 'beamtalk_actor':'mailbox_continuation_check'(CastSelector, CastArgs)")
            .count(),
        2,
        "both handle_cast dispatch clauses are guarded. Got:\n{cast}"
    );
    let cont = extract_core_fn(&code, "'handle_continue'/2 = fun").expect("handle_continue/2");
    assert!(!cont.contains("mailbox_continuation_check"), "Got:\n{cont}");
}

/// BT-3968: an actor's own `self perform:withArguments:timeout:` lowers to
/// `beamtalk_message_dispatch:send/4` like the other `perform:` forms,
/// instead of self-dispatching into `ProtoObject`'s method body.
#[test]
fn test_self_perform_with_timeout_lowers_to_send4() {
    let code = generate(
        concat!(
            "Actor subclass: Performer\n\n",
            "  later => self perform: #continueWith: withArguments: #(#bump) timeout: 1000\n\n",
            "  bump => nil\n",
        ),
        "performer",
    );
    assert!(
        code.contains("call 'beamtalk_message_dispatch':'send'(")
            && !code.contains("'safe_dispatch'('perform:withArguments:timeout:'"),
        "Got:\n{code}"
    );
}

const INSTALL_CALL_FROM: &str = "call 'beamtalk_actor':'install_call_from'(From)";
const UNINSTALL_CALL_FROM: &str = "call 'beamtalk_actor':'uninstall_call_from'()";
const ENCODE_TAGGED: &str =
    "call 'beamtalk_actor':'encode_reply_for_tagged'(From, Selector, Result)";

/// BT-4090: `handle_call` exposes the caller's `From` to `self deferReply`
/// around dispatch: installed before `safe_dispatch`, removed right after it
/// (on the success and error paths alike, since the result is bound first).
#[test]
fn test_handle_call_installs_and_uninstalls_call_from_around_dispatch() {
    let code = generate(
        concat!(
            "Server subclass: Parker\n",
            "  state: value = 0\n\n",
            "  wait => self deferReply\n",
        ),
        "parker",
    );
    let call = extract_core_fn(&code, "'handle_call'/3 = fun").expect("handle_call/3");
    let install = call
        .find(INSTALL_CALL_FROM)
        .unwrap_or_else(|| panic!("handle_call must install From. Got:\n{call}"));
    let dispatch = call
        .find("'safe_dispatch'(Selector, Args, State)")
        .expect("dispatch");
    let uninstall = call
        .find(UNINSTALL_CALL_FROM)
        .unwrap_or_else(|| panic!("handle_call must uninstall From. Got:\n{call}"));
    let result_case = call.find("case _DispatchResult of").expect("result case");
    assert!(
        install < dispatch && dispatch < uninstall && uninstall < result_case,
        "install < dispatch < uninstall < result case. Got:\n{call}"
    );
}

/// BT-4090: the deferred check runs before the wire-encode, so a deferred
/// call is answered `{noreply, CleanNewState}` without encoding the ignored
/// return value; only the success arm consults it.
#[test]
fn test_handle_call_success_arm_checks_deferred_before_encode() {
    let code = generate(
        concat!(
            "Server subclass: Parker\n",
            "  state: value = 0\n\n",
            "  wait => self deferReply\n",
        ),
        "parker",
    );
    let call = extract_core_fn(&code, "'handle_call'/3 = fun").expect("handle_call/3");
    let deferred = call
        .find("case _CallDeferred of")
        .unwrap_or_else(|| panic!("success arm must branch on _CallDeferred. Got:\n{call}"));
    let noreply = call
        .find("{'noreply', CleanNewState}")
        .expect("deferred arm must answer noreply with the clean state");
    let encode = call.find(ENCODE_TAGGED).expect("encode_reply_for_tagged");
    assert!(
        deferred < noreply && noreply < encode,
        "deferred check and noreply must precede the encode. Got:\n{call}"
    );
}

/// BT-4090: the error arm ignores the deferred flag, so a method that raises
/// after deferring replies the error to its caller instead of hanging it.
#[test]
fn test_handle_call_error_arm_discards_deferred_flag() {
    let code = generate(
        concat!(
            "Server subclass: Parker\n",
            "  state: value = 0\n\n",
            "  wait => self deferReply\n",
        ),
        "parker",
    );
    let call = extract_core_fn(&code, "'handle_call'/3 = fun").expect("handle_call/3");
    let err_arm = call
        .find("<{'error', Error, ErrState}> when 'true' ->")
        .expect("handle_call error arm");
    // The 3-tuple and 2-tuple clauses each carry a full dispatch case; look
    // only at this clause's error arm.
    let tail = call[err_arm..]
        .split("<{Selector, Args}>")
        .next()
        .expect("error arm tail");
    assert!(
        !tail.contains("_CallDeferred"),
        "the error arm must not consult the deferred flag. Got:\n{tail}"
    );
    assert!(
        tail.contains("{'reply', {'error', "),
        "the error arm must reply. Got:\n{tail}"
    );
}

/// BT-4090: only `handle_call` installs the call cell — cast, info and
/// continuation dispatches have no caller to defer.
#[test]
fn test_only_handle_call_installs_call_from() {
    let code = generate(
        concat!(
            "Server subclass: Parker\n",
            "  state: value = 0\n\n",
            "  handleInfo: msg => self.value := 1\n",
            "  bump => self continueWith: #bump\n",
        ),
        "parker",
    );
    for header in [
        "'handle_cast'/2 = fun",
        "'handle_info'/2 = fun",
        "'handle_continue'/2 = fun",
    ] {
        let body = extract_core_fn(&code, header).expect(header);
        assert!(
            !body.contains(INSTALL_CALL_FROM),
            "{header} must not install From"
        );
    }
}
