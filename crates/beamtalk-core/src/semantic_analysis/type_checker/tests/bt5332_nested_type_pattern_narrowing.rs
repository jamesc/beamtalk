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
        "typed Object subclass: ZzRepro\n  class run: v :: Object -> Object =>\n    v match: [\n      {pattern} -> {body};\n      _ -> 0\n    ]\n\n  class needsInt: i :: Integer -> Integer => i\n  class needsStr: s :: String -> Integer => s size\n  class needsList: l :: List -> Integer => l size\n"
    )
}

fn assert_narrowed(pattern: &str, bad_send: &str, good_send: &str) {
    let bad = type_messages(&fixture(pattern, bad_send));
    assert!(
        bad.iter().any(|m| m.contains("expects")),
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
        "self needsInt: errors",
        "self needsList: errors",
    );
}

#[test]
fn nested_in_tuple_narrows() {
    assert_narrowed(
        "{#ok, n :: Integer}",
        "self needsStr: n",
        "self needsInt: n",
    );
}

#[test]
fn nested_in_array_narrows() {
    assert_narrowed("#[s :: String, _]", "self needsInt: s", "self needsStr: s");
}

#[test]
fn nested_in_constructor_narrows() {
    assert_narrowed(
        "Result ok: s :: String",
        "self needsInt: s",
        "self needsStr: s",
    );
}

#[test]
fn nested_wildcard_type_binds_nothing() {
    let msgs = type_messages(&fixture("{_ :: Integer, n}", "n"));
    assert!(msgs.is_empty(), "unexpected diagnostics: {msgs:?}");
}

#[test]
fn nested_binding_shadowing_the_scrutinee_variable_keeps_its_narrowed_type() {
    // The scrutinee-key rewrite in `infer_match` must not clobber a nested
    // binding that reuses the scrutinee variable's name.
    assert_narrowed(
        "{#ok, v :: Integer}",
        "self needsStr: v",
        "self needsInt: v",
    );
}

#[test]
fn nested_object_type_stays_dynamic() {
    let msgs = type_messages(&fixture("{#ok, o :: Object}", "self needsInt: o"));
    assert!(msgs.is_empty(), "unexpected diagnostics: {msgs:?}");
}
