// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5336: a nested `x :: T` binds `intersect_with_class(position, T)` when
//! the scrutinee's type at that position is known (tuple element, map-shape
//! value, `Result ok:` / `error:` payload), like a top-level `x :: T` arm.
//! `l :: List` over a `List(Integer)` position binds `List(Integer)`, so
//! `l first` is `Integer`, not unchecked. Unknown / `Object` positions keep
//! the bare `T`.

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

/// `v :: scrutinee` matched with `pattern -> body`.
fn fixture(scrutinee: &str, pattern: &str, body: &str) -> String {
    format!(
        "typed Object subclass: ZzRepro\n  class run: v :: {scrutinee} -> Object =>\n    v match: [\n      {pattern} -> {body};\n      _ -> 0\n    ]\n\n  class needsInt: i :: Integer -> Integer => i\n  class needsStr: s :: String -> Integer => s size\n"
    )
}

/// `l first` is `Integer` iff `l` bound `List(Integer)`: feeding it to a
/// `String` parameter is flagged, feeding it to an `Integer` one is not.
fn assert_binds_list_of_integer(scrutinee: &str, pattern: &str) {
    let bad = type_messages(&fixture(scrutinee, pattern, "self needsStr: l first"));
    assert!(
        bad.iter().any(|m| m.contains("expects")),
        "`{pattern}` over `{scrutinee}`: `l first` must be Integer: {bad:?}"
    );
    let good = type_messages(&fixture(scrutinee, pattern, "self needsInt: l first"));
    assert!(
        good.is_empty(),
        "`{pattern}` over `{scrutinee}`: unexpected diagnostics: {good:?}"
    );
}

#[test]
fn tuple_position_intersects() {
    assert_binds_list_of_integer("Tuple(Symbol, List(Integer))", "{#ok, l :: List}");
}

#[test]
fn tuple_union_column_intersects() {
    assert_binds_list_of_integer(
        "Tuple(Symbol, List(Integer)) | Tuple(Symbol, List(Integer))",
        "{#ok, l :: List}",
    );
}

#[test]
fn nested_tuple_position_intersects() {
    assert_binds_list_of_integer(
        "Tuple(Symbol, Tuple(Symbol, List(Integer)))",
        "{#ok, {#inner, l :: List}}",
    );
}

#[test]
fn map_shape_value_intersects() {
    assert_binds_list_of_integer("#{#items => List(Integer)}", "#{#items => l :: List}");
}

#[test]
fn result_ok_payload_intersects() {
    assert_binds_list_of_integer("Result(List(Integer), String)", "Result ok: l :: List");
}

#[test]
fn result_error_payload_intersects() {
    assert_binds_list_of_integer("Result(String, List(Integer))", "Result error: l :: List");
}

#[test]
fn unknown_position_stays_bare() {
    // `Tuple` without arguments has no position types: `l` is a bare `List`,
    // whose `first` is unchecked.
    let msgs = type_messages(&fixture(
        "Tuple",
        "{#ok, l :: List}",
        "self needsStr: l first",
    ));
    assert!(msgs.is_empty(), "unexpected diagnostics: {msgs:?}");
}

#[test]
fn object_position_stays_bare() {
    let msgs = type_messages(&fixture(
        "Tuple(Symbol, Object)",
        "{#ok, l :: List}",
        "self needsStr: l first",
    ));
    assert!(msgs.is_empty(), "unexpected diagnostics: {msgs:?}");
}

#[test]
fn object_type_pattern_over_known_position_stays_dynamic() {
    let msgs = type_messages(&fixture(
        "Tuple(Symbol, Integer)",
        "{#ok, o :: Object}",
        "self needsStr: o",
    ));
    assert!(msgs.is_empty(), "unexpected diagnostics: {msgs:?}");
}
