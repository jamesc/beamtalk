// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3967: every generated `gen_server` callback that runs a Beamtalk
//! dispatch passes its return value through
//! `beamtalk_actor:apply_pending_stop/1`, so a `self stopWith: reason`
//! issued during the dispatch takes effect once the callback returns —
//! see `gen_server::callbacks::pending_stop_wrap`.

use super::*;

const APPLY_PENDING_STOP: &str = "call 'beamtalk_actor':'apply_pending_stop'(_CallbackResult)";
const CLEAR_PENDING_STOP: &str = "call 'beamtalk_actor':'clear_pending_stop'()";

/// Every gen_server callback the actor codegen can emit.
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
                body.contains(CLEAR_PENDING_STOP) && body.contains(APPLY_PENDING_STOP),
                "{header} dispatches, so it must clear and apply the pending stop. Got:\n{body}"
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
fn test_dispatching_callbacks_apply_pending_stop() {
    let code = generate(
        concat!(
            "Actor subclass: Stopper\n",
            "  state: value = 0\n\n",
            "  initialize => self.value := 1\n\n",
            "  bye => self stopWith: #shutdown\n",
        ),
        "stopper",
    );
    // handle_continue (initialize), handle_cast and handle_call dispatch.
    assert_eq!(assert_dispatching_callbacks_wrapped(&code), 3);
}

#[test]
fn test_server_handle_info_applies_pending_stop() {
    let code = generate(
        concat!(
            "Server subclass: InfoStopper\n\n",
            "  handleInfo: msg => self stopWith: msg\n",
        ),
        "info_stopper",
    );
    // No initialize here, so handle_continue does not dispatch; a Server's
    // handle_info dispatches handleInfo: alongside handle_cast/handle_call.
    assert_eq!(assert_dispatching_callbacks_wrapped(&code), 3);
    let info = extract_core_fn(&code, "'handle_info'/2 = fun").expect("handle_info/2");
    assert!(info.contains(APPLY_PENDING_STOP), "Got:\n{info}");
}
