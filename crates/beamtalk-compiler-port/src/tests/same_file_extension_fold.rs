// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4721: a file is one compilation unit for its class. The REPL compiler
//! port folds same-file extensions into the host class (the same
//! `beamtalk_core::compilation::fold_same_file_extensions` pass the build
//! uses), reports a duplicate selector as an error and rejects an
//! instance-side extension on a `native:` actor.

use super::*;

fn error_messages(response: &Term) -> Vec<String> {
    let Some(list) = response_diagnostics(response) else {
        return Vec::new();
    };
    list.elements
        .iter()
        .filter_map(|t| match t {
            Term::Map(m) => map_get(m, "message").and_then(term_to_string),
            _ => None,
        })
        .collect()
}

fn assert_error_containing(response: &Term, needle: &str) {
    assert_eq!(
        response_status(response).as_deref(),
        Some("error"),
        "{response:?}"
    );
    let msgs = error_messages(response);
    assert!(
        msgs.iter().any(|m| m.contains(needle)),
        "expected an error containing {needle:?}, got {msgs:?}"
    );
}

/// Per class kind, both compile entry points (`compile`, `compile_expression`)
/// fold the extension into the class: it is a dispatch arm and nothing
/// registers through `beamtalk_extensions`.
#[test]
fn same_file_extensions_fold_for_every_class_kind() {
    let cases = [
        (
            "plain",
            "Object subclass: Bt4721Plain\n  base => 1\n\nBt4721Plain >> extra => 2\nBt4721Plain class >> make => 3\n",
        ),
        (
            "value",
            "Value subclass: Bt4721Val\n  field: x :: Integer = 0\n\nBt4721Val >> extra: n :: Integer => self.x + n\nBt4721Val class >> make => 3\n",
        ),
        (
            "actor",
            "Actor subclass: Bt4721Act\n  state: n :: Integer = 5\n\nBt4721Act >> plus: k :: Integer => self.n + k\nBt4721Act class >> make => 3\n",
        ),
    ];
    for (kind, source) in cases {
        let file = handle_compile(&compile_request(source));
        assert_eq!(
            response_status(&file).as_deref(),
            Some("ok"),
            "{kind} file: {file:?}"
        );
        let core = response_field_str(&file, "core_erlang").expect("core_erlang");
        assert!(
            !core.contains("call 'beamtalk_extensions':'register'"),
            "{kind}: same-file extension must not register. Got:\n{core}"
        );
        assert!(
            core.contains("'make'"),
            "{kind}: class-side arm missing:\n{core}"
        );

        let inline = handle_compile_expression(&compile_expression_request(source));
        assert_eq!(
            response_status(&inline).as_deref(),
            Some("ok"),
            "{kind} inline: {inline:?}"
        );
    }
}

#[test]
fn duplicate_instance_selector_between_body_and_extension_is_an_error() {
    let source = "Object subclass: Bt4721Dup\n  foo => 1\n\nBt4721Dup >> foo => 2\n";
    assert_error_containing(
        &handle_compile(&compile_request(source)),
        "Duplicate method `foo`",
    );
    assert_error_containing(
        &handle_compile_expression(&compile_expression_request(source)),
        "Duplicate method `foo`",
    );
}

#[test]
fn duplicate_class_selector_between_body_and_extension_is_an_error() {
    let source = "Object subclass: Bt4721DupC\n  class foo => 1\n\nBt4721DupC class >> foo => 2\n";
    assert_error_containing(
        &handle_compile(&compile_request(source)),
        "Duplicate class method `foo`",
    );
    assert_error_containing(
        &handle_compile_expression(&compile_expression_request(source)),
        "Duplicate class method `foo`",
    );
}

#[test]
fn instance_extension_on_native_actor_is_an_error() {
    let source = "Actor subclass: Bt4721Nat native: beamtalk_test_native_counter\n  getValue => self delegate\n\nBt4721Nat >> extra => 1\n";
    assert_error_containing(&handle_compile(&compile_request(source)), "native actor");
}

#[test]
fn class_extension_on_native_actor_folds() {
    let source = "Actor subclass: Bt4721Nat2 native: beamtalk_test_native_counter\n  getValue => self delegate\n\nBt4721Nat2 class >> extra => 1\n";
    let response = handle_compile(&compile_request(source));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "{response:?}"
    );
    let core = response_field_str(&response, "core_erlang").expect("core_erlang");
    assert!(!core.contains("call 'beamtalk_extensions':'register'"), "Got:\n{core}");
}
