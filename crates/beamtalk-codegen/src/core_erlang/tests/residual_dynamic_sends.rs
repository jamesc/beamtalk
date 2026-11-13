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
    assert!(code.contains("call 'erlang':'is_integer'("));
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
fn is_kind_of_user_class_literal_is_name_only() {
    let code = codegen(concat!(
        "Object subclass: Probe\n",
        "  check: v :: Object -> Boolean => v isKindOf: Probe\n",
    ));
    assert!(
        code.contains("call 'beamtalk_behaviour_intrinsics':'isKindOfNamed'("),
        "user class literal should use isKindOfNamed. Got:\n{code}"
    );
    assert!(code.contains("'Probe')"), "Got:\n{code}");
    assert!(!code.contains("'isKindOfBuiltin'("), "Got:\n{code}");
    // BT-4768: the shared class-liveness probe guards the fast path; the one
    // `isKindOf:` send sits in the dead-class branch only, after the probe
    // and after the fast call, so the live path makes no dynamic send.
    let probe = code
        .find("call 'erlang':'is_pid'(_KindOfPid")
        .unwrap_or_else(|| panic!("liveness probe missing. Got:\n{code}"));
    // BT-4754: one `whereis` per call, bound once and shared with the runtime.
    assert_eq!(
        code.matches("call 'erlang':'whereis'('beamtalk_class_Probe')")
            .count(),
        1,
        "Got:\n{code}"
    );
    let fast = code.find("'isKindOfNamed'(").expect("fast call");
    let send = code.find("'isKindOf:', [").expect("dead-class send");
    assert!(probe < fast && fast < send, "Got:\n{code}");
    assert_eq!(code.matches("'isKindOf:', [").count(), 1, "Got:\n{code}");
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn is_kind_of_user_class_literal_builds_the_class_reference_only_on_the_send_exit() {
    // BT-4769: the fast path reads the override flag in line (no
    // `overridden_anywhere/1` call, hence no `lists:member/2` scan) and never
    // builds the class reference; the one reference sits on the send exit.
    let code = codegen(concat!(
        "Object subclass: Probe\n",
        "  check: v :: Object -> Boolean => v isKindOf: Probe\n",
    ));
    assert!(
        code.contains(
            "call 'persistent_term':'get'({'beamtalk_default_method', 'overridden', 'isKindOf:'}, 'false')"
        ),
        "override flag must be read in line. Got:\n{code}"
    );
    assert!(!code.contains("'overridden_anywhere'"), "Got:\n{code}");
    assert_eq!(
        code.matches("'Probe class static calls'").count(),
        1,
        "class reference built once, on the send exit. Got:\n{code}"
    );
    assert!(
        code.contains("'isKindOfNamed'(_KindOfRecv") && code.contains(", 'Probe', _KindOfPid"),
        "isKindOfNamed/3 takes the receiver, the name and the probe's pid. Got:\n{code}"
    );
    let reference = code.find("'Probe class static calls'").expect("reference");
    let fast = code.find("'isKindOfNamed'(").expect("fast call");
    assert!(
        fast < reference,
        "reference after the fast call. Got:\n{code}"
    );
}

#[test]
fn is_kind_of_fast_paths_share_one_guard() {
    // BT-4768 / BT-4769: the built-in lowering (BT-4764) and the name-only
    // lowering (BT-4757) emit the same guard (class liveness probe, then the
    // in-line override-flag read), from one helper.
    let guard = |pid: &str| {
        format!(
            "call 'erlang':'and'(call 'erlang':'is_pid'({pid}), call 'erlang':'not'(call 'persistent_term':'get'({{'beamtalk_default_method', 'overridden', 'isKindOf:'}}, 'false')))"
        )
    };
    let builtin = codegen(concat!(
        "Object subclass: Probe\n",
        "  check: v :: Object -> Boolean => v isKindOf: String\n",
    ));
    let named = codegen(concat!(
        "Object subclass: Probe\n",
        "  check: v :: Object -> Boolean => v isKindOf: Probe\n",
    ));
    assert!(
        builtin.contains(&guard("call 'erlang':'whereis'('beamtalk_class_String')")),
        "Got:\n{builtin}"
    );
    // The name-only lowering binds the `whereis` once (BT-4754) and guards on it.
    assert!(
        named.contains("call 'erlang':'whereis'('beamtalk_class_Probe') in case ")
            && named.contains(&guard("_KindOfPid")),
        "Got:\n{named}"
    );
    assert!(
        !builtin.contains("'overridden_anywhere'"),
        "Got:\n{builtin}"
    );
}

#[test]
fn override_flag_key_matches_shared_fixture() {
    // The Erlang side asserts the identical cases in
    // `beamtalk_behaviour_intrinsics_tests:override_flag_key_matches_shared_fixture_test/0`.
    let path = super::repo_root()
        .join("runtime/apps/beamtalk_runtime/test/fixtures/override_flag_key_conformance.json");
    let raw = std::fs::read_to_string(&path).expect("read fixture");
    let cases: Vec<serde_json::Value> = serde_json::from_str(&raw).expect("fixture is an array");
    assert!(!cases.is_empty());
    for case in &cases {
        let selector = case["selector"].as_str().expect("selector");
        let key: Vec<&str> = case["key"]
            .as_array()
            .expect("key")
            .iter()
            .map(|k| k.as_str().expect("key atom"))
            .collect();
        let doc = crate::core_erlang::CoreErlangGenerator::override_flag_read_doc(selector)
            .to_pretty_string();
        let expected = format!(
            "call 'persistent_term':'get'({{{}}}, 'false')",
            key.iter()
                .map(|a| format!("'{a}'"))
                .collect::<Vec<_>>()
                .join(", ")
        );
        assert_eq!(doc, expected);
    }
}

#[test]
fn is_kind_of_user_class_literal_stays_a_send_in_the_repl() {
    // A REPL name resolves through the session map before the class registry
    // (ADR 0129 §7), so the literal's spelling does not name the class.
    use crate::core_erlang::{CodeGenContext, CoreErlangGenerator};
    let (module, _) = super::parse_normalised("dog isKindOf: IkAnimal");
    let stmt = module.expressions[0].expression.clone();
    let mut generator = CoreErlangGenerator::new("bt4757_repl");
    generator.context = CodeGenContext::Repl;
    generator.set_is_repl_mode(true);
    generator.push_scope();
    generator.bind_var("__bindings__", "Bindings");
    generator.bind_var("dog", "Dog");
    let code = generator
        .expression_doc_with_repl_mutation_tracking(&stmt)
        .expect("codegen")
        .0
        .to_pretty_string();
    assert!(!code.contains("'isKindOfNamed'("), "Got:\n{code}");
    assert!(code.contains("'isKindOf:'"), "Got:\n{code}");
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

/// The classes `x isKindOf: C` lowers inline for: every routing-fixture class
/// (and alias) with a representation test that the lowering accepts. Derived
/// from the fixture, never listed here.
fn inline_kind_of_classes() -> Vec<&'static str> {
    let mut found = Vec::new();
    for row in crate::core_erlang::routing::rows() {
        for class in std::iter::once(&row.class).chain(row.aliases.iter()) {
            let code = codegen(&format!(
                "Object subclass: Probe\n  check: v :: Object -> Boolean => v isKindOf: {class}\n"
            ));
            if code.contains("'overridden_anywhere'('isKindOf:')") {
                found.push(*class);
            }
        }
    }
    found
}

/// The text of the inline fast path of an `isKindOf:` lowering: from the
/// guard's `true` clause (class alive, no override) up to the first class-reference
/// construction (`let _KindOfClass`), which only the cold paths contain.
fn kind_of_fast_path(code: &str) -> &str {
    let check = "'overridden_anywhere'('isKindOf:'))) of <'true'> when 'true' -> ";
    let start = code.find(check).expect("override check") + check.len();
    let rest = &code[start..];
    &rest[..rest.find("let _KindOfClass").expect("cold path")]
}

#[test]
fn is_kind_of_builtin_fast_path_builds_no_class_reference_and_sends_nothing() {
    for class in inline_kind_of_classes() {
        let code = codegen(&format!(
            "Object subclass: Probe\n  check: v :: Object -> Boolean => v isKindOf: {class}\n"
        ));
        let fast = kind_of_fast_path(&code);
        // `Tuple`'s test itself names the reserved tuple tags.
        let reserved = if class == "Tuple" {
            None
        } else {
            Some("beamtalk_object")
        };
        for forbidden in reserved.into_iter().chain([
            "beamtalk_message_dispatch",
            "persistent_term",
            "whereis",
            "class_ref",
            "beamtalk_behaviour_intrinsics",
        ]) {
            assert!(
                !fast.contains(forbidden),
                "{class}: fast path must not contain {forbidden}. Got:\n{fast}"
            );
        }
        // The class process is probed once, before the override check, so a
        // dead class still raises `class_not_found` as the literal always did.
        let before_check = &code[..code.find("'overridden_anywhere'").unwrap()];
        assert!(
            before_check.contains("call 'erlang':'is_pid'(call 'erlang':'whereis'"),
            "{class}: Got:\n{code}"
        );
        assert!(
            before_check.contains(&format!("'whereis'('beamtalk_class_{class}')")),
            "{class}: liveness probe missing. Got:\n{code}"
        );
        // A dead class process (probe fails) takes the cold path, whose class
        // literal raises `class_not_found` before the send, as it always did.
        let cold = &code[code
            .find("<'false'> when 'true' -> let _KindOfClass")
            .unwrap()..];
        let raise = cold.find("'class_not_found'").expect("cold path raises");
        assert!(
            raise < cold.find("'send'(").unwrap(),
            "{class}: class_not_found must precede the send. Got:\n{cold}"
        );
        // The override is honoured with the plain send.
        assert!(
            code.contains("<'false'> when 'true' -> let _KindOfClass")
                && code.contains("'send'(_KindOfRecv"),
            "{class}: override fallback missing. Got:\n{code}"
        );
    }
}

#[test]
fn is_kind_of_hit_answers_true_inline_unless_the_strategy_cannot_prove_a_plain_value() {
    // `Tuple`'s test rejects a reserved tag only at the tag's own tuple size,
    // so a hit is not proof of a plain value and keeps the runtime check.
    for class in inline_kind_of_classes() {
        let code = codegen(&format!(
            "Object subclass: Probe\n  check: v :: Object -> Boolean => v isKindOf: {class}\n"
        ));
        let fast = kind_of_fast_path(&code);
        if class == "Tuple" {
            assert!(
                code.contains("'isKindOfBuiltin'(") && code.contains(", 'true')"),
                "Tuple's hit must keep the runtime plain-value check. Got:\n{code}"
            );
        } else {
            assert!(fast.contains("-> 'true'"), "{class}: Got:\n{fast}");
        }
    }
}

#[test]
fn is_kind_of_inline_classes_are_covered_by_the_runtime_matrix() {
    // The runtime conformance matrix names every class the lowering handles,
    // so a new fixture strategy cannot ship without a row in it.
    let matrix = std::fs::read_to_string(
        super::repo_root().join("stdlib/test/is_kind_of_inline_matrix_test.bt"),
    )
    .expect("matrix test");
    for class in inline_kind_of_classes() {
        assert!(
            matrix.contains(&format!("isKindOf: {class}")),
            "stdlib/test/is_kind_of_inline_matrix_test.bt must test isKindOf: {class}"
        );
    }
}
