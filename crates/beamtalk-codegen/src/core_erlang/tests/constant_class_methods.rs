// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4363: a class-literal send of a class-side method whose whole body is
//! one integer literal answers the literal in place, but only behind a
//! run-time key, `{constant_value, Value, Module, Fun, Tag}`: the runtime
//! installs it only while a direct call would run that compiled method and
//! the defining module's `constant_class_methods` metadata still maps the
//! selector to `Value`, else the call site takes the dynamic `class_send`. So
//! a later extension, runtime fun, `removeSelector:` or reload is honoured.

use super::codegen;

const SRC: &str = "\
Object subclass: Caller
  constant => Errs protocolError
  negative => Errs negative
  keyword: y => Errs code: y
  computed => Errs computed
  symbolic => Errs symbolic
  inherited => SubErrs protocolError
  stateful => Stateful answer

Object subclass: Errs
  class protocolError => 1
  class negative => -7
  class code: x => 9
  class computed => 40 + 2
  class symbolic => #one
  class withState => 3

Errs subclass: SubErrs

Object subclass: Stateful
  classState: count :: Integer = 0
  class answer => 5
";

/// The class whose metadata certifies the constants, compiled on its own.
const ERRS: &str = "\
Object subclass: Errs
  class protocolError => 1
  class negative => -7
  class code: x => 9
  class computed => 40 + 2
  class symbolic => #one
  class withState => 3
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

/// The hit arm of a guarded send: everything after its last wildcard clause.
fn hit_arm(body: &str) -> &str {
    let at = body
        .rfind("<_> when 'true' -> ")
        .unwrap_or_else(|| panic!("the guard has a hit arm. Got:\n{body}"));
    &body[at..]
}

#[test]
fn constant_sends_answer_the_literal_only_behind_a_constant_value_key() {
    let out = codegen(SRC);
    for (method, value, fun, arity) in [
        ("constant", "1", "class_protocolError", 1),
        ("negative", "-7", "class_negative", 1),
        ("keyword:", "9", "class_code:", 2),
    ] {
        let body = function_body(&out, method);
        let key = format!("{{'constant_value', {value}, 'bt@errs', '{fun}', 'Errs class'}}");
        assert!(
            body.contains(&format!("call 'persistent_term':'get'({key}, 'false')")),
            "`{method}` reads its constant-value key. Got:\n{body}"
        );
        let miss =
            format!("call 'beamtalk_class_shadow_flags':'static_call_target'({key}, 'Errs', ");
        assert!(
            body.contains(&miss) && body.contains(&format!(", {arity})")),
            "`{method}` installs the key on a miss. Got:\n{body}"
        );
        assert!(
            body.contains("call 'beamtalk_object_class':'class_send'("),
            "`{method}` falls back to the dynamic send. Got:\n{body}"
        );
        let hit = hit_arm(body);
        assert!(
            hit.starts_with(&format!("<_> when 'true' -> {value}")),
            "`{method}`'s hit arm is the literal. Got:\n{hit}"
        );
        assert!(
            !body.contains("call 'bt@errs':"),
            "`{method}` never calls the method it folds. Got:\n{body}"
        );
    }
}

#[test]
fn a_keyword_constant_still_evaluates_its_argument_once() {
    let out = codegen(SRC);
    let body = function_body(&out, "keyword:");
    // The argument is bound once, before the guard, and the fallback sends it.
    assert!(
        body.contains("class_send'(") && body.contains("'code:', [_"),
        "the fallback sends the bound argument. Got:\n{body}"
    );
}

#[test]
fn non_literal_bodies_keep_the_guarded_direct_call() {
    let out = codegen(SRC);
    for (method, fun) in [
        ("computed", "class_computed"),
        ("symbolic", "class_symbolic"),
    ] {
        let body = function_body(&out, method);
        assert!(
            !body.contains("'constant_value'"),
            "`{method}` is not folded. Got:\n{body}"
        );
        assert!(
            body.contains(&format!("{{'bt@errs', '{fun}', 'Errs class'}}"))
                && hit_arm(body).contains(&format!("call 'bt@errs':'{fun}'(")),
            "`{method}` keeps the guarded direct call. Got:\n{body}"
        );
    }
}

#[test]
fn inherited_or_class_process_constants_are_not_folded() {
    let out = codegen(SRC);
    // An inherited class method takes the dynamic path (ADR 0142 §2): no
    // call site names a superclass module.
    let inherited = function_body(&out, "inherited");
    assert!(
        !inherited.contains("'constant_value'") && !inherited.contains("persistent_term"),
        "an inherited constant takes the dynamic send. Got:\n{inherited}"
    );
    // A class with class variables keeps its class methods in its process.
    let stateful = function_body(&out, "stateful");
    assert!(
        !stateful.contains("'constant_value'") && !stateful.contains("persistent_term"),
        "a class-variable class's constant takes the class process. Got:\n{stateful}"
    );
}

#[test]
fn the_defining_module_certifies_only_integer_literal_bodies() {
    let out = codegen(ERRS);
    assert!(
        out.contains(
            "'constant_class_methods' => ~{'protocolError' => 1, 'negative' => -7, 'code:' => 9, 'withState' => 3}~"
        ),
        "only integer-literal class-side bodies are certified. Got:\n{out}"
    );
    let plain = codegen("Object subclass: Plain\n  class answer => 40 + 2\n");
    assert!(
        !plain.contains("'constant_class_methods'"),
        "omitted for a class with none. Got:\n{plain}"
    );
}
