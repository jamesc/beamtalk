// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source-span resolution commands (ADR 0082 Phase 1): `resolve_method_span`, `find_selector_send_spans`, `find_definition_selector_spans`, and `resolve_class_span`.

use super::*;

// --- resolve_method_span tests (ADR 0082 Phase 1) ---

#[test]
fn resolve_method_span_instance_method() {
    let request = Map::from([
        (atom("command"), atom("resolve_method_span")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("Counter")),
        (atom("selector"), binary("increment")),
    ]);
    let response = handle_resolve_method_span(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    // prev_source must be exactly the bytes occupying the span — splicing it
    // back is a no-op, the load-bearing property of the splice strategy.
    let prev = map_get(m, "prev_source")
        .and_then(term_to_string)
        .expect("prev_source present");
    // The span is a verbatim full-line slice, so it carries the method's
    // leading indentation (and its doc comment, when present).
    assert!(prev.starts_with("  increment =>"), "got: {prev:?}");
    assert!(
        prev.ends_with('\n'),
        "span includes trailing newline: {prev:?}"
    );
    let Some(Term::Map(span)) = map_get(m, "span") else {
        panic!("span should be a map: {response:?}");
    };
    let start = map_get(span, "start").and_then(term_to_usize).unwrap();
    let end = map_get(span, "end").and_then(term_to_usize).unwrap();
    assert_eq!(
        &SPAN_FIXTURE[start..end],
        prev,
        "span must bound prev_source"
    );
}

#[test]
fn resolve_method_span_class_side() {
    let request = Map::from([
        (atom("command"), atom("resolve_method_span")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("Counter")),
        (atom("selector"), binary("new")),
        (atom("side"), atom("class")),
    ]);
    let response = handle_resolve_method_span(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let prev = map_get(m, "prev_source")
        .and_then(term_to_string)
        .expect("prev_source present");
    assert!(prev.starts_with("  class new =>"), "got: {prev:?}");
}

#[test]
fn resolve_method_span_selector_not_found_is_structured_error() {
    let request = Map::from([
        (atom("command"), atom("resolve_method_span")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("Counter")),
        (atom("selector"), binary("nope")),
    ]);
    let response = handle_resolve_method_span(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
    assert_eq!(map_get(m, "reason"), Some(&atom("selector_not_found")));
}

#[test]
fn resolve_method_span_class_not_found_is_structured_error() {
    let request = Map::from([
        (atom("command"), atom("resolve_method_span")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("NoSuchClass")),
        (atom("selector"), binary("increment")),
    ]);
    let response = handle_resolve_method_span(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
    assert_eq!(map_get(m, "reason"), Some(&atom("class_not_found")));
}

// --- find_selector_send_spans tests (ADR 0114) ---

#[test]
fn find_selector_send_spans_finds_unary_self_send() {
    let request = Map::from([
        (atom("command"), atom("find_selector_send_spans")),
        (atom("method_source"), binary("self basicNew")),
        (atom("old_selector"), binary("basicNew")),
        (atom("new_selector"), binary("newBasic")),
    ]);
    let response = handle_find_selector_send_spans(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let Some(Term::List(occurrences)) = map_get(m, "occurrences") else {
        panic!("occurrences should be a list: {response:?}");
    };
    assert_eq!(occurrences.elements.len(), 1, "got {occurrences:?}");
    let Term::List(ref spans) = occurrences.elements[0] else {
        panic!("occurrence should be a list: {occurrences:?}");
    };
    assert_eq!(spans.elements.len(), 1);
    let Term::Map(ref span) = spans.elements[0] else {
        panic!("span should be a map: {spans:?}");
    };
    assert_eq!(
        map_get(span, "new_text"),
        Some(&binary("newBasic")),
        "{span:?}"
    );
    let start = map_get(span, "start").and_then(term_to_usize).unwrap();
    let end = map_get(span, "end").and_then(term_to_usize).unwrap();
    assert_eq!(&"self basicNew"[start..end], "basicNew");
}

#[test]
fn find_selector_send_spans_no_match_is_empty_not_error() {
    let request = Map::from([
        (atom("command"), atom("find_selector_send_spans")),
        (atom("method_source"), binary("anObject increment")),
        (atom("old_selector"), binary("increment")),
        (atom("new_selector"), binary("bump")),
    ]);
    let response = handle_find_selector_send_spans(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let Some(Term::List(occurrences)) = map_get(m, "occurrences") else {
        panic!("occurrences should be a list: {response:?}");
    };
    assert!(occurrences.elements.is_empty(), "got {occurrences:?}");
}

#[test]
fn find_selector_send_spans_keyword_selector_produces_one_span_per_part() {
    let request = Map::from([
        (atom("command"), atom("find_selector_send_spans")),
        (atom("method_source"), binary("self at: k put: v")),
        (atom("old_selector"), binary("at:put:")),
        (atom("new_selector"), binary("setAt:to:")),
    ]);
    let response = handle_find_selector_send_spans(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    let Some(Term::List(occurrences)) = map_get(m, "occurrences") else {
        panic!("occurrences should be a list: {response:?}");
    };
    assert_eq!(occurrences.elements.len(), 1, "got {occurrences:?}");
    let Term::List(ref spans) = occurrences.elements[0] else {
        panic!("occurrence should be a list: {occurrences:?}");
    };
    assert_eq!(spans.elements.len(), 2, "got {spans:?}");
}

// --- find_definition_selector_spans tests (ADR 0114) ---

#[test]
fn find_definition_selector_spans_finds_unary_selector() {
    let request = Map::from([
        (atom("command"), atom("find_definition_selector_spans")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("Counter")),
        (atom("old_selector"), binary("increment")),
        (atom("new_selector"), binary("bump")),
    ]);
    let response = handle_find_definition_selector_spans(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let Some(Term::List(spans)) = map_get(m, "spans") else {
        panic!("spans should be a list: {response:?}");
    };
    assert_eq!(spans.elements.len(), 1, "got {spans:?}");
    let Term::Map(ref span) = spans.elements[0] else {
        panic!("span should be a map: {spans:?}");
    };
    let start = map_get(span, "start").and_then(term_to_usize).unwrap();
    let end = map_get(span, "end").and_then(term_to_usize).unwrap();
    assert_eq!(&SPAN_FIXTURE[start..end], "increment");
    assert_eq!(map_get(span, "new_text"), Some(&binary("bump")));
}

#[test]
fn find_definition_selector_spans_class_side() {
    let request = Map::from([
        (atom("command"), atom("find_definition_selector_spans")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("Counter")),
        (atom("old_selector"), binary("new")),
        (atom("new_selector"), binary("make")),
        (atom("side"), atom("class")),
    ]);
    let response = handle_find_definition_selector_spans(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let Some(Term::List(spans)) = map_get(m, "spans") else {
        panic!("spans should be a list: {response:?}");
    };
    assert_eq!(spans.elements.len(), 1, "got {spans:?}");
    let Term::Map(ref span) = spans.elements[0] else {
        panic!("span should be a map: {spans:?}");
    };
    let start = map_get(span, "start").and_then(term_to_usize).unwrap();
    let end = map_get(span, "end").and_then(term_to_usize).unwrap();
    assert_eq!(&SPAN_FIXTURE[start..end], "new");
}

#[test]
fn find_definition_selector_spans_selector_not_found_is_structured_error() {
    let request = Map::from([
        (atom("command"), atom("find_definition_selector_spans")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("Counter")),
        (atom("old_selector"), binary("nope")),
        (atom("new_selector"), binary("stillNope")),
    ]);
    let response = handle_find_definition_selector_spans(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
    assert_eq!(map_get(m, "reason"), Some(&atom("selector_not_found")));
}

// --- resolve_class_span tests (ADR 0082 extension) ---

#[test]
fn resolve_class_span_header_only_excludes_methods() {
    let request = Map::from([
        (atom("command"), atom("resolve_class_span")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("Counter")),
    ]);
    let response = handle_resolve_class_span(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    let prev = map_get(m, "prev_source")
        .and_then(term_to_string)
        .expect("prev_source present");
    // SPAN_FIXTURE's `Counter` has no state declarations, so the span is
    // just its (single-line) header — deliberately excluding every
    // method (a class-span flush must never be able to delete a
    // method's source — see class_span.rs's module doc).
    assert_eq!(prev, "Object subclass: Counter\n", "got: {prev:?}");
    assert!(!prev.contains("increment"), "got: {prev:?}");
    assert!(!prev.contains("class new"), "got: {prev:?}");
    let Some(Term::Map(span)) = map_get(m, "span") else {
        panic!("span should be a map: {response:?}");
    };
    let start = map_get(span, "start").and_then(term_to_usize).unwrap();
    let end = map_get(span, "end").and_then(term_to_usize).unwrap();
    assert_eq!(
        &SPAN_FIXTURE[start..end],
        prev,
        "span must bound prev_source"
    );
}

#[test]
fn resolve_class_span_class_not_found_is_structured_error() {
    let request = Map::from([
        (atom("command"), atom("resolve_class_span")),
        (atom("source"), binary(SPAN_FIXTURE)),
        (atom("class_name"), binary("NoSuchClass")),
    ]);
    let response = handle_resolve_class_span(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
    assert_eq!(map_get(m, "reason"), Some(&atom("class_not_found")));
}

#[test]
fn resolve_class_span_missing_class_name_field_is_error() {
    let request = Map::from([
        (atom("command"), atom("resolve_class_span")),
        (atom("source"), binary(SPAN_FIXTURE)),
    ]);
    let response = handle_resolve_class_span(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
}
