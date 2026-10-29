// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4115: a class-literal send whose class-side method is a caller-safe
//! class primitive (`Error signal: m`) raises in the caller, with no class
//! process hop; dynamic receivers and overriding subclasses keep the send.

use super::codegen;

/// Whether `out` calls `beamtalk_exception_handler:<fun>` with `tail` in its
/// argument list (argument variables are generated, so only the fixed part is
/// matched).
fn has_handler_call(out: &str, fun: &str, tail: &str) -> bool {
    let head = format!("call 'beamtalk_exception_handler':'{fun}'(");
    out.match_indices(&head).any(|(at, _)| {
        let call = &out[at..];
        let call = &call[..call.find('\n').unwrap_or(call.len())];
        call.contains(tail)
    })
}

const CALLER: &str = "\
Object subclass: Raiser
  boom: m => Error signal: m
  boomBare => Error signal
  boomReason: m => Error signal: m reason: #why
  boomKind => Exception signalKind: #not_found class: #Raiser selector: #boomKind hint: \"h\"
  boomCustom: m => MyError signal: m
  boomLoud: m => LoudError signal: m

Error subclass: MyError

Error subclass: LoudError
  class signal: msg => super signal: msg
";

#[test]
fn class_literal_signal_compiles_to_direct_handler_call() {
    let out = codegen(CALLER);
    for (fun, tail) in [
        ("class_signal_message", ", 'Error')"),
        ("class_signal", "('Error')"),
        ("class_signal_message_reason", ", 'why', 'Error')"),
        ("class_signal_message", ", 'MyError')"),
        ("class_signal_kind", "('not_found', 'Raiser', 'boomKind', "),
    ] {
        assert!(
            has_handler_call(&out, fun, tail),
            "expected a direct `{fun}` raise containing `{tail}`. Got:\n{out}"
        );
    }
    for class in ["Error", "MyError", "Exception"] {
        let lookup = format!("'beamtalk_class_registry':'whereis_class'('{class}')");
        assert!(
            !out.contains(&lookup),
            "a caller-safe class primitive must not reach the {class} class process. Got:\n{out}"
        );
    }
}

#[test]
fn overriding_subclass_keeps_class_send() {
    let out = codegen(CALLER);
    assert!(
        out.contains("'beamtalk_class_registry':'whereis_class'('LoudError')"),
        "a subclass overriding `class signal:` must keep the class send. Got:\n{out}"
    );
    assert!(
        !has_handler_call(&out, "class_signal_message", ", 'LoudError')"),
        "the override must run, not be inlined away. Got:\n{out}"
    );
}

#[test]
fn dynamic_receiver_keeps_send() {
    let out = codegen("Object subclass: Raiser\n  boom: cls msg: m => cls signal: m\n");
    assert!(
        !has_handler_call(&out, "class_signal_message", ""),
        "a dynamic receiver is out of scope and must not be inlined. Got:\n{out}"
    );
}
