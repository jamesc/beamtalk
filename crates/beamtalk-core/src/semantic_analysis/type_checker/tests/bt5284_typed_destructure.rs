// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5284: `x :: T` elements in a destructuring pattern declare the local's
//! type (trusted, like `x :: T := expr`). A known position type that clashes
//! with the annotation warns like an annotated assignment does.

use super::common::analyse_diags;
use crate::source_analysis::{Diagnostic, Severity};

fn problems(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| matches!(d.severity, Severity::Error | Severity::Warning))
        .collect()
}

fn mismatches(diags: &[Diagnostic]) -> usize {
    diags
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .count()
}

#[test]
fn annotated_tuple_element_types_the_local() {
    // Untyped `Tuple` RHS binds Dynamic; the annotation makes `sid` an
    // Integer, so returning it as String is a mismatch.
    let diags = analyse_diags(
        "typed Object subclass: Demo\n  class a: t :: Tuple -> String =>\n    {_, sid :: Integer} := t\n    sid\n",
    );
    assert_eq!(mismatches(&diags), 1, "{diags:?}");
}

#[test]
fn annotated_tuple_element_is_clean_for_matching_use() {
    let diags = analyse_diags(
        "typed Object subclass: Demo\n  class a: t :: Tuple -> Integer =>\n    {_, sid :: Integer, es :: Boolean} := t\n    es ifTrue: [sid + 1] ifFalse: [sid]\n",
    );
    assert!(problems(&diags).is_empty(), "{diags:?}");
}

#[test]
fn annotated_array_and_map_elements_type_the_local() {
    let arr = analyse_diags(
        "typed Object subclass: Demo\n  run: xs :: Array -> String =>\n    #[a :: Integer, _] := xs\n    a\n",
    );
    assert_eq!(mismatches(&arr), 1, "{arr:?}");
    let map = analyse_diags(
        "typed Object subclass: Demo\n  run: m :: Dictionary -> String =>\n    #{#id => i :: Integer} := m\n    i\n",
    );
    assert_eq!(mismatches(&map), 1, "{map:?}");
}

#[test]
fn annotation_clashing_with_known_position_warns() {
    let diags = analyse_diags(
        "typed Object subclass: Demo\n  run: p :: Tuple(Integer, String) -> Integer =>\n    {a :: String, _} := p\n    0\n",
    );
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("String") && d.message.contains("Integer")),
        "{diags:?}"
    );
}
