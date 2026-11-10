// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4270: a class-literal send whose class-side method is one of
//! `Exception`'s caller-safe signal primitives (`Error signal: m`) raises in
//! the caller (BT-4115), but only behind a run-time key,
//! `{caller_safe_raise, Primitive, DefiningModule, Fun, ReceiverTag}`: the runtime
//! installs it only while the send still resolves to that compiled primitive,
//! else the call site takes the dynamic `class_send`. So a later
//! `class signal:` override, extension, runtime fun or reload is honoured.

use super::codegen;

const CALLER: &str = "\
Object subclass: Raiser
  boom: m => Error signal: m
  boomBare => Error signal
  boomReason: m => Error signal: m reason: #why
  boomOwn: m => Exception signal: m
  boomKind => Exception signalKind: #not_found class: #Raiser selector: #boomKind hint: \"h\"
  boomCustom: m => MyError signal: m
  boomLoud: m => LoudError signal: m

Error subclass: MyError

Error subclass: LoudError
  class signal: msg => super signal: msg
";

/// The body of the generated function `name/N`, up to the next function.
fn function_body<'a>(out: &'a str, name: &str) -> &'a str {
    let head = format!("'{name}'/");
    let start = out
        .match_indices(&head)
        .map(|(at, _)| at)
        .find(|&at| {
            let after = &out[at + head.len()..];
            let digits = after.chars().take_while(char::is_ascii_digit).count();
            digits > 0 && after[digits..].starts_with(" = fun")
        })
        .unwrap_or_else(|| panic!("function {name} defined. Got:\n{out}"));
    let rest = &out[start..];
    let end = rest[1..].find("\n'").map_or(rest.len(), |e| e + 1);
    &rest[..end]
}

/// The module `Exception`'s class methods compile to, as the keys name it.
fn exception_module(out: &str) -> String {
    let body = function_body(out, "boomOwn:");
    let head = "{'caller_safe_raise', 'classSignal:', ";
    let at = body
        .find(head)
        .unwrap_or_else(|| panic!("`Exception signal:` reads a raise key. Got:\n{body}"))
        + head.len();
    let end = at + body[at..].find(", ").expect("the key names a function");
    body[at..end].to_string()
}

/// The `beamtalk_exception_handler` call in the hit arm of a guarded send.
fn hit_arm(body: &str) -> &str {
    let at = body
        .rfind("<_> when 'true' -> ")
        .unwrap_or_else(|| panic!("the guard has a hit arm. Got:\n{body}"));
    &body[at..]
}

#[test]
fn signal_sends_raise_through_the_handler_only_in_the_hit_arm() {
    let out = codegen(CALLER);
    let module = exception_module(&out);
    for (method, class, selector, fun, arity, raise) in [
        (
            "boom:",
            "Error",
            "signal:",
            "class_signal:",
            2,
            "'class_signal_message'(_Arg",
        ),
        (
            "boomBare",
            "Error",
            "signal",
            "class_signal",
            1,
            "'class_signal'('Error')",
        ),
        (
            "boomReason:",
            "Error",
            "signal:reason:",
            "class_signal:reason:",
            3,
            "'class_signal_message_reason'(_Arg",
        ),
        (
            "boomOwn:",
            "Exception",
            "signal:",
            "class_signal:",
            2,
            "'class_signal_message'(_Arg",
        ),
        (
            "boomCustom:",
            "MyError",
            "signal:",
            "class_signal:",
            2,
            "'class_signal_message'(_Arg",
        ),
    ] {
        let body = function_body(&out, method);
        // `Exception`'s method bodies: `class signal: msg => @primitive "classSignal:"`.
        let primitive = format!("class{}{}", &selector[..1].to_uppercase(), &selector[1..]);
        let key =
            format!("{{'caller_safe_raise', '{primitive}', {module}, '{fun}', '{class} class'}}");
        assert!(
            body.contains(&format!(
                "call 'persistent_term':'get'('{class} class static calls', ~{{}}~) of <~{{{key} := "
            ))
                && body.contains(&format!(
                    "'beamtalk_class_shadow_flags':'static_call_target'({key}, '{class}', '{selector}', {arity})"
                )),
            "`{class} {selector}` reads the raise key naming Exception's method and its own tag. Got:\n{body}"
        );
        assert!(
            body.contains(&format!("'erlang':'whereis'('beamtalk_class_{class}')"))
                && body.contains("'beamtalk_object_class':'class_send'("),
            "a miss takes the dynamic class send to `{class}`. Got:\n{body}"
        );
        let hit = hit_arm(body);
        assert!(
            hit.contains(&format!("call 'beamtalk_exception_handler':{raise}"))
                && (selector != "signal:" || hit.contains(&format!(", '{class}')"))),
            "a hit raises in the caller with the receiving class's name. Got:\n{hit}"
        );
        assert_eq!(
            body.matches("'beamtalk_exception_handler'").count(),
            1,
            "the handler is only reached in the hit arm. Got:\n{body}"
        );
    }
}

#[test]
fn signal_kind_raises_behind_its_key() {
    let out = codegen(CALLER);
    let body = function_body(&out, "boomKind");
    assert!(
        body.contains("'class_signalKind:class:selector:hint:', 'Exception class'}")
            && hit_arm(body).contains("'beamtalk_exception_handler':'class_signal_kind'("),
        "`Exception signalKind:...` raises behind its key. Got:\n{body}"
    );
}

#[test]
fn overriding_subclass_runs_its_own_override() {
    let out = codegen(CALLER);
    let body = function_body(&out, "boomLoud:");
    assert!(
        !body.contains("'beamtalk_exception_handler'") && !body.contains("'caller_safe_raise'"),
        "a subclass overriding `class signal:` is never raised past. Got:\n{body}"
    );
    assert!(
        body.contains("'class_signal:', 'LoudError class'}"),
        "it is the guarded direct call of LoudError's own override. Got:\n{body}"
    );
}

#[test]
fn dynamic_receiver_keeps_send() {
    let out = codegen("Object subclass: Raiser\n  boom: cls msg: m => cls signal: m\n");
    let body = function_body(&out, "boom:msg:");
    assert!(
        !body.contains("'beamtalk_exception_handler'") && !body.contains("'static_call_target'"),
        "a dynamic receiver is an ordinary send. Got:\n{body}"
    );
}
