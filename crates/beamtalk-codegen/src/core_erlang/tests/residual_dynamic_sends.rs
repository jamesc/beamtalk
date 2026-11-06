// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4473: residual dynamic sends on a request path.
//!
//! - A typed send to an instance of the unit's own class (sealed or open)
//!   calls the own `dispatch/3` arm's function locally behind the class-tag
//!   guard, instead of `Module:dispatch/3`. A send to another module's class
//!   keeps `Module:dispatch/3`.
//! - `x isKindOf: C`, where `C` is a sealed built-in class literal with a
//!   routing-fixture type test, applies that test and hands it to
//!   `beamtalk_behaviour_intrinsics:isKindOfBuiltin/3`.
//! - `x not` is an inline `case` on `true`/`false`, with the ordinary send in
//!   its wildcard arm.

use super::codegen;

// ---------------------------------------------------------------------------
// Own-class direct arms
// ---------------------------------------------------------------------------

#[test]
fn sealed_own_class_typed_send_calls_the_arm_locally() {
    let code = codegen(concat!(
        "sealed Value subclass: Pt\n",
        "  field: x :: Integer = 0\n",
        "  sumWith: other :: Pt -> Integer => self x + other x\n",
    ));
    assert!(
        code.contains("<'Pt'> when 'true' -> apply 'x'/1("),
        "an own-class typed send should call the getter arm locally. Got:\n{code}"
    );
    assert!(
        !code.contains("call 'test':'dispatch'('x'"),
        "no own-class send should go through dispatch/3. Got:\n{code}"
    );
    // The guard's miss arms keep the dynamic send.
    assert!(code.contains("call 'beamtalk_message_dispatch':'send'("));
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn open_own_class_self_send_calls_the_arm_locally() {
    // Open class: a subclass instance misses the tag guard and sends.
    let code = codegen(concat!(
        "Value subclass: Box\n",
        "  field: v :: Integer = 0\n",
        "  scaled: k :: Integer -> Integer => v * k\n",
        "  twice -> Integer => self scaled: 2\n",
    ));
    assert!(
        code.contains("<'Box'> when 'true' -> apply 'scaled:'/2("),
        "an open own-class self-send should call its arm locally. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn own_arm_with_non_local_return_answers_its_result() {
    // BT-4503: a method whose block holds `^` answers its result itself (the
    // NLR boundary inside the function answers the bare value), so its arm
    // and the direct call both take the return as is.
    let code = codegen(concat!(
        "sealed Value subclass: Finder\n",
        "  field: items :: List(Integer) = #()\n",
        "  firstBig -> Integer | Nil =>\n",
        "    self items do: [:e | e > 10 ifTrue: [^e]]\n",
        "    nil\n",
        "  firstBigOrZero -> Integer => self firstBig ifNil: [0]\n",
    ));
    let call = code
        .find("apply 'firstBig'/1(")
        .unwrap_or_else(|| panic!("the own arm should be called locally. Got:\n{code}"));
    assert!(
        !code[..call].ends_with("case "),
        "the direct call answers the send's result, no unwrap. Got:\n{code}"
    );
    assert!(
        code.contains("<'firstBig'> when 'true' ->\n            call 'test':'firstBig'(Self)\n"),
        "the dispatch/3 arm answers the function's return. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn other_module_sealed_class_keeps_dispatch() {
    // Only the unit's own class: whether another module's selector is a
    // method or a slot read is not known for the version that is loaded.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "a send to another module's class keeps dispatch/3. Got:\n{code}"
    );
    assert!(!code.contains("apply 'x'/1("));
}

#[test]
fn dispatch_prefix_selectors_match_emitted_arms() {
    // The selectors `dispatch/3` answers before its method arms, in order,
    // must be exactly DISPATCH_PREFIX_SELECTORS (plus the conditional
    // `asString`), or an own-class direct call could reach a method the
    // dynamic send never would.
    let code = codegen(concat!(
        "sealed Value subclass: Probe\n",
        "  field: x :: Integer = 0\n",
        "  asString -> String => \"probe\"\n",
    ));
    let start = code
        .find("'dispatch'/3 = fun (Selector, Args, Self) ->")
        .expect("dispatch/3 should be emitted");
    let body = &code[start..];
    let first_method = body
        .find("<'asString'> when 'true' ->")
        .expect("the class's own asString arm");
    let prefix: Vec<&str> = body[..first_method]
        .lines()
        .filter_map(|line| {
            let line = line.trim_start();
            line.strip_prefix("<'")
                .and_then(|rest| rest.split_once("'> when 'true' ->"))
                .map(|(sel, _)| sel)
        })
        .filter(|sel| !matches!(*sel, "true" | "false"))
        .collect();
    assert_eq!(
        prefix,
        crate::core_erlang::value_type_codegen::DISPATCH_PREFIX_SELECTORS,
        "dispatch/3's prefix arms drifted from DISPATCH_PREFIX_SELECTORS. Got:\n{code}"
    );
}

// ---------------------------------------------------------------------------
// isKindOf: with a sealed built-in class literal
// ---------------------------------------------------------------------------

#[test]
fn is_kind_of_builtin_literal_applies_the_type_test() {
    let code = codegen(concat!(
        "Object subclass: Probe\n",
        "  check: v :: Object -> Boolean => v isKindOf: Integer\n",
    ));
    assert!(
        code.contains("call 'beamtalk_behaviour_intrinsics':'isKindOfBuiltin'("),
        "isKindOf: with a sealed built-in literal should use isKindOfBuiltin. Got:\n{code}"
    );
    assert!(code.contains("call 'erlang':'is_integer'("));
    assert!(
        !code.contains("'isKindOf:', ["),
        "no dynamic isKindOf: send should remain. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn is_kind_of_binary_literal_admits_strings() {
    let code = codegen(concat!(
        "Object subclass: Probe\n",
        "  check: v :: Object -> Boolean => v isKindOf: Binary\n",
    ));
    assert!(code.contains("'isKindOfBuiltin'("));
    assert!(code.contains("call 'erlang':'is_binary'("));
    assert!(
        code.contains("<'beamtalk_string'> when 'true'"),
        "Binary's test must admit a String. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn is_kind_of_stays_dynamic_without_a_sealed_builtin_literal() {
    for (arg, why) in [
        ("Number", "abstract built-in with no exact test"),
        ("Probe", "user class"),
        ("k", "not a class literal"),
    ] {
        let src = format!(
            "Object subclass: Probe\n  check: v :: Object with: k :: Behaviour -> Boolean => v isKindOf: {arg}\n"
        );
        let code = codegen(&src);
        assert!(
            !code.contains("'isKindOfBuiltin'("),
            "isKindOf: {arg} ({why}) must stay dynamic. Got:\n{code}"
        );
    }
}

#[test]
fn is_kind_of_abstract_sealed_boolean_uses_is_boolean() {
    // `Boolean` is abstract and sealed: its instances are exactly `true` and
    // `false` (its stdlib subclasses), which `is_boolean` tests.
    let code = codegen(concat!(
        "Object subclass: Probe\n",
        "  check: v :: Object -> Boolean => v isKindOf: Boolean\n",
    ));
    assert!(code.contains("'isKindOfBuiltin'("), "Got:\n{code}");
    assert!(code.contains("call 'erlang':'is_boolean'("), "Got:\n{code}");
}

#[test]
fn is_kind_of_on_a_class_literal_receiver_keeps_the_class_reference_lowering() {
    // In a class method of a class with class variables, a send to its own
    // class literal runs in the class process; the fallback send would call
    // back into it.
    let code = codegen(concat!(
        "Object subclass: Probe\n",
        "  classState: count :: Integer = 0\n",
        "  class check -> Boolean => Probe isKindOf: Integer\n",
    ));
    assert!(!code.contains("'isKindOfBuiltin'("), "Got:\n{code}");
}

#[test]
fn is_kind_of_on_self_stays_dynamic() {
    // An actor's `self` would make the fallback send a call to itself.
    let code = codegen(concat!(
        "Actor subclass: Probe\n",
        "  check -> Boolean => self isKindOf: Integer\n",
    ));
    assert!(!code.contains("'isKindOfBuiltin'("), "Got:\n{code}");
}

// ---------------------------------------------------------------------------
// not
// ---------------------------------------------------------------------------

#[test]
fn not_is_an_inline_boolean_case() {
    let code = codegen(concat!(
        "Object subclass: Probe\n",
        "  flip: b :: Object -> Object => b not\n",
    ));
    assert!(
        code.contains("of <'true'> when 'true' -> 'false' <'false'> when 'true' -> 'true'"),
        "`not` should be an inline case. Got:\n{code}"
    );
    assert!(
        code.contains(", 'not', [])"),
        "a non-Boolean receiver keeps the ordinary send. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}
