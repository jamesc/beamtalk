// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5213 (ADR 0116 § Addendum → `String` and `Symbol` receivers): a
//! `String` or `Symbol` on the left of an ordering comparison
//! (`< <= > >=`) orders only against its own kind. The inlined path tests the
//! right operand of a proven `String` receiver with the `String` routing
//! guard and raises the frame-free `beamtalk_ordering_type_error` term for
//! anything else; the `@primitive` bodies of `String` and `Symbol` do the
//! same (`primitives::tests`). The test adds no call, so the one "bare and
//! Boolean" predicate (`operators::comparison_is_bare`) still holds for a
//! proven `String` receiver: the comparison runs no user code and answers a
//! Boolean or raises, so it needs no cell bracket and stays a Boolean for the
//! guard-return rewrite. The generated corpus is in
//! `number_comparison_right_operand_corpus.rs` (its `String`-literal and
//! `self`-in-`String` receivers).

use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::CoreErlangGenerator;
use crate::core_erlang::patterns::guard_boolean_test;
use crate::core_erlang::routing::{self, Guard};
use beamtalk_cerl_doc::Document;

/// The Core Erlang of the method `selector`: from its function head to the
/// next one.
fn method<'a>(code: &'a str, selector: &str) -> &'a str {
    let head = format!("\n'{selector}'/");
    let start = code
        .match_indices(&head)
        .map(|(i, _)| i + head.len())
        .find(|&i| {
            code[i..]
                .trim_start_matches(|c: char| c.is_ascii_digit())
                .starts_with(" =")
        })
        .unwrap_or_else(|| panic!("no method {selector}:\n{code}"));
    let rest = &code[start..];
    let end = rest.find("\n'").unwrap_or(rest.len());
    &rest[..end]
}

const STRING_TEST: &str = "call 'erlang':'is_record'(_BinRight";
const RAISE: &str = "call 'erlang':'error'({'beamtalk_ordering_type_error', _BinLeft";

#[test]
fn a_string_literal_receiver_tests_an_unproven_right_operand() {
    let code = codegen(
        "Object subclass: P\n\
         \x20 lt: x => \"m\" < x\n\
         \x20 le: x :: String => \"m\" <= x\n\
         \x20 gt: x => \"m\" > 5\n\
         \x20 ge: x => \"m\" >= #m\n",
    );
    for (selector, op) in [("lt:", "<"), ("le:", "<="), ("gt:", ">"), ("ge:", ">=")] {
        let body = method(&code, selector);
        assert!(
            body.contains(STRING_TEST) && body.contains(RAISE),
            "{selector}: an unproven right operand (even a declared String) is tested:\n{body}"
        );
        assert!(
            body.contains("'beamtalk_ordering_type_error', _BinLeft")
                && body.contains(&format!("'{op}', _BinRight")),
            "{selector}: the raise carries the operator:\n{body}"
        );
        // Frame-free: the wrong-kind arm is `erlang:error/1`, never a call
        // into the runtime (ADR 0116 § The failing arm).
        assert!(
            !body.contains("beamtalk_message_dispatch"),
            "{selector}: the wrong-kind arm must not call the runtime:\n{body}"
        );
    }
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn two_proven_strings_stay_the_bare_bif() {
    let code = codegen(
        "Object subclass: P\n\
         \x20 lit => \"a\" < \"b\"\n",
    );
    let body = method(&code, "lit");
    assert!(
        !body.contains("is_record") && !body.contains("beamtalk_ordering_type_error"),
        "both operands proven Strings: no test:\n{body}"
    );
}

#[test]
fn self_in_string_is_a_proven_string_operand() {
    let code = codegen(
        "Object subclass: String\n\
         \x20 below: x => self < x\n\
         \x20 above => \"m\" < self\n",
    );
    let below = method(&code, "below:");
    assert!(
        below.contains(STRING_TEST) && below.contains(RAISE),
        "self in String with an unproven right operand is tested:\n{below}"
    );
    let above = method(&code, "above");
    assert!(
        !above.contains("is_record") && !above.contains("beamtalk_ordering_type_error"),
        "self in String on the right is a proven String:\n{above}"
    );
}

#[test]
fn a_tested_string_comparison_in_an_actor_needs_no_bracket() {
    // The wrong-kind arm raises and runs no user code, so the comparison
    // between two field writes is not bracketed (no store before it), unlike
    // a number receiver's hook (`a_proven_receiver_with_a_tested_right_operand_is_bracketed`).
    let code = codegen(
        "Actor subclass: A\n\
         \x20 state: n :: Integer = 0\n\
         \x20 bump: x =>\n\
         \x20   self.n := self.n + 1\n\
         \x20   r := \"m\" < x\n\
         \x20   self.n := self.n + 10\n\
         \x20   r\n",
    );
    assert!(code.contains(STRING_TEST), "{code}");
    assert!(code.contains(RAISE), "{code}");
    assert!(
        !code.contains("'beamtalk_actor_state':'store'"),
        "no user code runs, so no bracket:\n{code}"
    );
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn symbol_and_unknown_receivers_still_dispatch() {
    // A Symbol literal is not a proven comparison kind: it dispatches to
    // `Symbol>><`, whose primitive body tests the argument.
    let code = codegen(
        "Object subclass: P\n\
         \x20 sym: x => #a < x\n\
         \x20 any: a with: x => a < x\n",
    );
    for selector in ["sym:", "any:with:"] {
        let body = method(&code, selector);
        assert!(
            body.contains("'beamtalk_message_dispatch':'send'("),
            "{selector} dispatches:\n{body}"
        );
        assert!(
            !body.contains("beamtalk_ordering_type_error"),
            "{selector}: the test lives in the primitive body:\n{body}"
        );
    }
}

/// The guard kinds the expression renderer covers, run against the `case`
/// renderers of record over the same terms: the two must agree on every one.
#[test]
fn boolean_guard_tests_agree_with_the_case_renderers() {
    let guards: Vec<(&str, Guard)> = vec![
        (
            "string",
            routing::type_test("String").expect("String guard"),
        ),
        (
            "symbol",
            routing::type_test("Symbol").expect("Symbol guard"),
        ),
        (
            "nil",
            routing::type_test("UndefinedObject").expect("nil guard"),
        ),
        ("true", routing::type_test("True").expect("True guard")),
        (
            "integer",
            routing::type_test("Integer").expect("Integer guard"),
        ),
        ("list", routing::type_test("List").expect("List guard")),
    ];
    let mut generator = CoreErlangGenerator::new("bt_guard_agree");
    let mut functions = Vec::new();
    let mut exports = Vec::new();
    for (name, guard) in &guards {
        let as_case = generator.render_type_test(
            *guard,
            "X",
            Document::Str("'true'"),
            &Document::Str("'false'"),
        );
        let as_bool =
            guard_boolean_test(*guard, "X").unwrap_or_else(|| panic!("{name}: no expression form"));
        for (kind, body) in [("case", as_case), ("bool", as_bool)] {
            let fname = format!("{kind}_{name}");
            exports.push(format!("'{fname}'/1"));
            functions.push(format!(
                "'{fname}'/1 = fun (X) -> {}",
                body.to_pretty_string()
            ));
        }
    }
    let core = format!(
        "module 'bt_guard_agree' [{}]\n attributes []\n{}\nend\n",
        exports.join(", "),
        functions.join("\n")
    );
    let terms = [
        "{beamtalk_string, <<\"a\">>}",
        "{beamtalk_string, <<\"a\">>, x}",
        "{other, <<\"a\">>}",
        "<<\"a\">>",
        "a",
        "nil",
        "true",
        "false",
        "5",
        "2.5",
        "[1]",
        "[]",
        "#{}",
        "{}",
        "self()",
    ];
    let mut calls = Vec::new();
    for (name, _) in &guards {
        for (i, term) in terms.iter().enumerate() {
            calls.push((
                format!("{name}|{i}"),
                format!(
                    "{{bt_guard_agree:'case_{name}'({term}), bt_guard_agree:'bool_{name}'({term})}}"
                ),
            ));
        }
    }
    let Some(observed) =
        super::value_type_fold_family_corpus::run_erlang_probes(&[&core], &[], "ok", &calls)
    else {
        return;
    };
    assert_eq!(observed.len(), calls.len(), "{observed:?}");
    for (name, result) in &observed {
        assert!(
            result == "{true,true}" || result == "{false,false}",
            "{name}: the case and expression forms disagree: {result}"
        );
    }
    // Both outcomes are exercised for every guard.
    for (name, _) in &guards {
        for outcome in ["{true,true}", "{false,false}"] {
            assert!(
                observed
                    .iter()
                    .any(|(probe, result)| probe.starts_with(&format!("{name}|"))
                        && result == outcome),
                "{name} never answered {outcome}"
            );
        }
    }
}

/// The raw error tag crosses the Rust/Erlang boundary: codegen raises
/// `{ORDERING_TYPE_ERROR_TAG, …}` and the runtime matches
/// `?BT_ORDERING_TYPE_ERROR_TAG` in `wrap_raw/2`. The header must define the
/// macro as exactly this atom.
#[test]
fn the_ordering_type_error_tag_matches_the_runtime_header() {
    use crate::core_erlang::operators::ORDERING_TYPE_ERROR_TAG;
    let header = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../runtime/apps/beamtalk_runtime/include/beamtalk.hrl");
    let text = std::fs::read_to_string(&header).expect("read beamtalk.hrl");
    let expected = format!("-define(BT_ORDERING_TYPE_ERROR_TAG, {ORDERING_TYPE_ERROR_TAG}).");
    assert!(
        text.lines().any(|line| line.trim() == expected),
        "beamtalk.hrl must contain `{expected}`"
    );
}
