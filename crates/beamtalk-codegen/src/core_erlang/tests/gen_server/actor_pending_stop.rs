// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3967: every generated gen_server callback that runs a Beamtalk
//! dispatch passes its return value through
//! `beamtalk_actor:apply_pending_stop/1`, so a `self stopWith: reason`
//! issued during the dispatch takes effect once the callback returns —
//! see `gen_server::callbacks::pending_stop_wrap`.

use super::*;

const APPLY_PENDING_STOP: &str = "call 'beamtalk_actor':'apply_pending_stop'(_CallbackResult)";

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
    for header in [
        "'handle_continue'/2 = fun",
        "'handle_cast'/2 = fun",
        "'handle_call'/3 = fun",
    ] {
        let body = extract_core_fn(&code, header)
            .unwrap_or_else(|| panic!("{header} must be present in generated code"));
        assert!(
            body.contains(APPLY_PENDING_STOP),
            "{header} must pass its result through apply_pending_stop/1. Got:\n{body}"
        );
    }
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
    let body = extract_core_fn(&code, "'handle_info'/2 = fun")
        .expect("handle_info/2 must be present in generated code");
    assert!(
        body.contains(APPLY_PENDING_STOP),
        "Server-subclass handle_info/2 must pass its result through \
         apply_pending_stop/1. Got:\n{body}"
    );
}
