// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4449: a warning's diagnostic hint reaches the REPL as its own field.
//! Every warning in a response's `warnings` list is a `#{message => Bin}`
//! map, with `hint => Bin` only when the diagnostic carried a hint — on the
//! expression path and the file-compile path alike.

use super::*;

/// A DNU near a real selector gets a "Did you mean" hint.
const HINTED_EXPR: &str = "\"abc\" sizee";

/// The warning whose message mentions `needle`, or a panic listing them all.
fn warning_containing(response: &Term, needle: &str) -> crate::diagnostics::Warning {
    let warnings = response_warnings(response);
    warnings
        .iter()
        .find(|w| w.message.contains(needle))
        .cloned()
        .unwrap_or_else(|| panic!("no warning containing {needle:?}: {warnings:?}"))
}

#[test]
fn expression_warning_carries_its_hint() {
    let response = handle_compile_expression(&compile_expression_request(HINTED_EXPR));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "{response:?}"
    );
    let w = warning_containing(&response, "does not understand 'sizee'");
    assert_eq!(w.hint.as_deref(), Some("Did you mean 'size'?"));
    assert!(
        !w.message.contains("Did you mean"),
        "the hint is not folded into the message: {w:?}"
    );
}

#[test]
fn expression_warning_without_hint_has_no_hint_key() {
    // An inline class with a standalone method targeting an unknown class:
    // the port synthesises a hint-less warning for it.
    let source = "Object subclass: Bt4449A\n  foo => 1\n\nBt4449Missing >> bar => 2\n";
    let response = handle_compile_expression(&compile_expression_request(source));
    let w = warning_containing(&response, "Standalone method targets unknown class");
    assert_eq!(w.hint, None);
}

#[test]
fn file_compile_warning_carries_its_hint() {
    let source = "Object subclass: Bt4449B\n  run => \"abc\" sizee\n";
    let response = handle_compile(&compile_request(source));
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
    let w = warning_containing(&response, "does not understand 'sizee'");
    assert_eq!(w.hint.as_deref(), Some("Did you mean 'size'?"));
}

#[test]
fn file_compile_warning_without_hint_has_no_hint_key() {
    let source = "Object subclass: Bt4449C\n  run => 1\n\nBt4449Missing >> bar => 2\n";
    let response = handle_compile(&compile_request(source));
    let w = warning_containing(&response, "Standalone method targets unknown class");
    assert_eq!(w.hint, None);
}

#[test]
fn warning_term_omits_hint_key_when_absent() {
    let terms = crate::respond::build_warning_terms(&[
        crate::diagnostics::Warning::message_only("plain"),
        crate::diagnostics::Warning {
            message: "hinted".to_string(),
            hint: Some("try this".to_string()),
        },
    ]);
    let Term::Map(plain) = &terms[0] else {
        panic!("expected map: {:?}", terms[0]);
    };
    assert_eq!(
        map_get(plain, "message")
            .and_then(term_to_string)
            .as_deref(),
        Some("plain")
    );
    assert!(map_get(plain, "hint").is_none());
    let Term::Map(hinted) = &terms[1] else {
        panic!("expected map: {:?}", terms[1]);
    };
    assert_eq!(
        map_get(hinted, "hint").and_then(term_to_string).as_deref(),
        Some("try this")
    );
}
