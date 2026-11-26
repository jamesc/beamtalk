// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5332: an `x :: T` nested inside a tuple, map, array or
//! `Result ok:` / `error:` pattern narrows `x` to `T`, like a top-level
//! `x :: T` arm. Before, the nested binding was `Dynamic` and every send to it
//! went unchecked.

use super::common::*;

fn type_messages(source: &str) -> Vec<String> {
    let (_, parse_diags) =
        crate::source_analysis::parse(crate::source_analysis::lex_with_eof(source));
    assert!(
        parse_diags.is_empty(),
        "fixture must parse: {parse_diags:?}"
    );
    type_diags(source)
        .iter()
        .map(|d| d.message.to_string())
        .collect()
}

/// A method matching `v` with `pattern -> body`, where `v :: Object`.
fn fixture(pattern: &str, body: &str) -> String {
    format!(
        "typed Object subclass: ZzRepro\n  class run: v :: Object -> Object =>\n    v match: [\n      {pattern} -> {body};\n      _ -> 0\n    ]\n"
    )
}

fn assert_narrowed(pattern: &str, bad_send: &str, good_send: &str) {
    let bad = type_messages(&fixture(pattern, bad_send));
    assert!(
        bad.iter().any(|m| m.contains("does not understand")),
        "`{pattern}`: `{bad_send}` must be flagged: {bad:?}"
    );
    let good = type_messages(&fixture(pattern, good_send));
    assert!(
        good.is_empty(),
        "`{pattern}`: `{good_send}` must not be flagged: {good:?}"
    );
}

#[test]
fn nested_in_map_value_narrows() {
    assert_narrowed(
        "#{#errors => errors :: List}",
        "errors fooBar",
        "errors size",
    );
}

#[test]
fn nested_in_tuple_narrows() {
    assert_narrowed("{#ok, n :: Integer}", "n fooBar", "n + 1");
}

#[test]
fn nested_in_array_narrows() {
    assert_narrowed("#[s :: String, _]", "s fooBar", "s size");
}

#[test]
fn nested_in_constructor_narrows() {
    assert_narrowed("Result ok: s :: String", "s fooBar", "s size");
}

#[test]
fn nested_wildcard_type_binds_nothing() {
    let msgs = type_messages(&fixture("{_ :: Integer, n}", "n"));
    assert!(msgs.is_empty(), "unexpected diagnostics: {msgs:?}");
}
