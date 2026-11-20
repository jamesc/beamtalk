// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Argument-validation tests for `validate_class_name`, `validate_erlang_module_name`, and `validate_selector`.
//!
//! BT-4908: one table per validator. Rejections assert the exact error message
//! (which also names the offending input) rather than only `is_err()`.

use super::*;

/// Asserts `f` returns `Ok` for every accepted input and an exact-message
/// `invalid_params` error for every rejected one.
fn check(
    validator: &str,
    f: fn(&str) -> Result<(), rmcp::ErrorData>,
    accepted: &[&str],
    rejected: &[(&str, String)],
) {
    for input in accepted {
        assert!(
            f(input).is_ok(),
            "{validator}({input:?}) should be accepted"
        );
    }
    for (input, expected) in rejected {
        let err = f(input).expect_err(&format!("{validator}({input:?}) should be rejected"));
        assert_eq!(err.message, *expected, "{validator}({input:?})");
        assert_eq!(
            err.code,
            rmcp::model::ErrorCode::INVALID_PARAMS,
            "{validator}({input:?})"
        );
    }
}

fn bad_class(name: &str) -> String {
    format!("Invalid class name: '{name}'. Must be an uppercase-starting identifier.")
}

fn bad_module(name: &str) -> String {
    format!("Invalid Erlang module name: '{name}'. Must be a lowercase identifier.")
}

#[test]
fn validate_class_name_accepts_and_rejects() {
    let rejected: Vec<(&str, String)> = [
        "", "counter", "1Counter", "My Class", "My-Class",
        "My:Class", // colons are for selectors only
        "Clàss",    // non-ASCII
        "bad name",
    ]
    .into_iter()
    .map(|n| (n, bad_class(n)))
    .collect();
    check(
        "validate_class_name",
        validate_class_name,
        &["Counter", "Counter2", "My_Class", "A"],
        &rejected,
    );
}

#[test]
fn validate_erlang_module_name_accepts_and_rejects() {
    let rejected: Vec<(&str, String)> = [
        "_private", // underscore-leading identifiers are variables, not atoms
        "",
        "Lists",
        "my module",
        "my-module",
        "BadModule",
    ]
    .into_iter()
    .map(|n| (n, bad_module(n)))
    .collect();
    check(
        "validate_erlang_module_name",
        validate_erlang_module_name,
        &["lists", "maps", "beamtalk_runtime"],
        &rejected,
    );
}

#[test]
fn validate_selector_accepts_and_rejects() {
    let invalid = |s: &str| format!("invalid selector: '{s}'");
    let rejected_inputs = [
        "+foo",     // operator char mixed with alphanumerics
        "at: put:", // space
        "#size",    // hash
        "bad selector!",
    ];
    let mut rejected: Vec<(&str, String)> =
        rejected_inputs.iter().map(|s| (*s, invalid(s))).collect();
    rejected.push(("", "selector must not be empty".to_string()));
    check(
        "validate_selector",
        validate_selector,
        &[
            // unary
            "increment",
            "size",
            "isEmpty",
            // keyword
            "at:",
            "at:put:",
            "ifTrue:ifFalse:",
            // digits are alphanumeric: selectors have no leading-char restriction
            "at1:",
            // binary
            "+",
            "-",
            "*",
            "/",
            "<",
            ">",
            "=",
            ">=",
            "<=",
            "**",
            "~=",
        ],
        &rejected,
    );
}

#[test]
fn complete_cursor_must_be_on_a_char_boundary() {
    // BT-5078: `{"code": "é", "cursor": 1}` used to slice inside the 2-byte
    // `é` and panic the handler.
    let err = code_up_to_cursor("é", Some(1)).expect_err("mid-character cursor is rejected");
    assert_eq!(err.code, rmcp::model::ErrorCode::INVALID_PARAMS);
    assert!(
        err.message.contains("character boundary"),
        "{}",
        err.message
    );

    assert_eq!(code_up_to_cursor("é", Some(2)).unwrap(), ("é", 2));
    assert_eq!(code_up_to_cursor("é", Some(0)).unwrap(), ("", 0));
    // Default is end of code; a cursor past the end is clamped.
    assert_eq!(code_up_to_cursor("aé", None).unwrap(), ("aé", 3));
    assert_eq!(code_up_to_cursor("abc", Some(99)).unwrap(), ("abc", 3));
}
