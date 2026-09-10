// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Unit tests for the seven source-navigation query handlers
//! (`find_senders_in_source`, `find_all_sends_in_source`,
//! `find_announce_sites_in_source`, `find_references_to_in_source`,
//! `find_field_readers_in_source`, `find_field_writers_in_source`,
//! `find_ffi_sites_in_source`).
//!
//! These handlers are thin ETF wrappers over language-service query
//! functions.  The tests below verify the wire protocol: that each handler
//! parses its request Map correctly, calls the underlying query, and returns
//! the expected response shape — including `{status => error}` for missing
//! required fields.

use super::*;
use eetf::FixInteger;

// ─── helpers ────────────────────────────────────────────────────────────────

/// Assert the response has `status => ok` and return the inner Map.
fn assert_ok(response: &Term) -> &Map {
    let Term::Map(m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    m
}

/// Assert the response has `status => error`.
fn assert_error(response: &Term) {
    let Term::Map(m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
}

/// Extract the `lines` list from a `#{status => ok, lines => [...]}` response.
fn ok_lines(response: &Term) -> Vec<u32> {
    let m = assert_ok(response);
    let Some(Term::List(list)) = map_get(m, "lines") else {
        panic!("Expected 'lines' list in: {response:?}");
    };
    list.elements
        .iter()
        .filter_map(|t| term_to_usize(t).and_then(|n| u32::try_from(n).ok()))
        .collect()
}

// ─── handle_find_senders_in_source ──────────────────────────────────────────

#[test]
fn find_senders_finds_matching_selector() {
    let request = Map::from([
        (atom("source"), binary("greet => self name asUppercase")),
        (atom("selector"), binary("asUppercase")),
    ]);
    let lines = ok_lines(&handle_find_senders_in_source(&request));
    assert_eq!(lines, vec![1]);
}

#[test]
fn find_senders_empty_when_selector_absent() {
    let request = Map::from([
        (atom("source"), binary("greet => self name")),
        (atom("selector"), binary("absentSelector")),
    ]);
    let lines = ok_lines(&handle_find_senders_in_source(&request));
    assert!(lines.is_empty(), "got {lines:?}");
}

#[test]
fn find_senders_finds_multiple_occurrences() {
    let request = Map::from([
        (
            atom("source"),
            binary("report =>\n  a printString\n  b printString"),
        ),
        (atom("selector"), binary("printString")),
    ]);
    let lines = ok_lines(&handle_find_senders_in_source(&request));
    assert_eq!(lines, vec![2, 3]);
}

#[test]
fn find_senders_missing_source_is_error() {
    let request = Map::from([(atom("selector"), binary("increment"))]);
    assert_error(&handle_find_senders_in_source(&request));
}

#[test]
fn find_senders_missing_selector_is_error() {
    let request = Map::from([(atom("source"), binary("greet => self name"))]);
    assert_error(&handle_find_senders_in_source(&request));
}

// ─── handle_find_all_sends_in_source ────────────────────────────────────────

#[test]
fn find_all_sends_returns_sends_list() {
    let request = Map::from([(atom("source"), binary("greet => self name asUppercase"))]);
    let response = handle_find_all_sends_in_source(&request);
    let m = assert_ok(&response);
    let Some(Term::List(sends)) = map_get(m, "sends") else {
        panic!("Expected 'sends' list in: {response:?}");
    };
    // `name` and `asUppercase` — at least two sends
    assert!(sends.elements.len() >= 2, "got {sends:?}");
}

#[test]
fn find_all_sends_each_entry_has_selector_line_recv_target_module() {
    let request = Map::from([(atom("source"), binary("greet => self name"))]);
    let response = handle_find_all_sends_in_source(&request);
    let m = assert_ok(&response);
    let Some(Term::List(sends)) = map_get(m, "sends") else {
        panic!("Expected 'sends' list in: {response:?}");
    };
    assert!(!sends.elements.is_empty(), "expected at least one send");
    // Every entry must be a map with required keys.
    for entry in &sends.elements {
        let Term::Map(em) = entry else {
            panic!("send entry should be a map: {entry:?}");
        };
        assert!(map_get(em, "selector").is_some(), "missing 'selector'");
        assert!(map_get(em, "line").is_some(), "missing 'line'");
        assert!(map_get(em, "recv").is_some(), "missing 'recv'");
        assert!(
            map_get(em, "target_module").is_some(),
            "missing 'target_module'"
        );
    }
}

#[test]
fn find_all_sends_empty_source_returns_empty_list() {
    // A syntactically empty method has no sends.
    let request = Map::from([(atom("source"), binary("empty => nil"))]);
    let response = handle_find_all_sends_in_source(&request);
    let m = assert_ok(&response);
    let Some(Term::List(sends)) = map_get(m, "sends") else {
        panic!("Expected 'sends' list in: {response:?}");
    };
    // `nil` is a literal, not a send — zero sends expected.
    assert!(
        sends.elements.is_empty(),
        "expected no sends, got {sends:?}"
    );
}

#[test]
fn find_all_sends_self_recv_is_self_atom() {
    let request = Map::from([(atom("source"), binary("test => self increment"))]);
    let response = handle_find_all_sends_in_source(&request);
    let m = assert_ok(&response);
    let Some(Term::List(sends)) = map_get(m, "sends") else {
        panic!("Expected 'sends' list");
    };
    let found_self = sends.elements.iter().any(|e| {
        if let Term::Map(em) = e {
            map_get(em, "recv") == Some(&atom("self"))
        } else {
            false
        }
    });
    assert!(found_self, "expected a self-receiver send, got {sends:?}");
}

#[test]
fn find_all_sends_missing_source_is_error() {
    let request = Map::from([]);
    assert_error(&handle_find_all_sends_in_source(&request));
}

// ─── handle_find_announce_sites_in_source ────────────────────────────────────

#[test]
fn find_announce_sites_finds_announce_send() {
    let request = Map::from([(
        atom("source"),
        binary("emit => announcer announce: MyEvent new"),
    )]);
    let response = handle_find_announce_sites_in_source(&request);
    let m = assert_ok(&response);
    let Some(Term::List(sites)) = map_get(m, "sites") else {
        panic!("Expected 'sites' list in: {response:?}");
    };
    assert_eq!(sites.elements.len(), 1, "got {sites:?}");
    let Term::Map(ref site) = sites.elements[0] else {
        panic!("site should be a map");
    };
    // selector, line, and announcement_class keys must be present.
    assert!(map_get(site, "selector").is_some(), "missing 'selector'");
    assert!(map_get(site, "line").is_some(), "missing 'line'");
    assert!(
        map_get(site, "announcement_class").is_some(),
        "missing 'announcement_class'"
    );
}

#[test]
fn find_announce_sites_resolves_class_name() {
    let request = Map::from([(
        atom("source"),
        binary("emit => self announce: OrderPlaced new"),
    )]);
    let response = handle_find_announce_sites_in_source(&request);
    let m = assert_ok(&response);
    let Some(Term::List(sites)) = map_get(m, "sites") else {
        panic!("Expected 'sites' list");
    };
    assert!(!sites.elements.is_empty(), "expected a site");
    if let Term::Map(ref site) = sites.elements[0] {
        let class_bin = map_get(site, "announcement_class")
            .and_then(term_to_string)
            .unwrap_or_default();
        assert_eq!(class_bin, "OrderPlaced", "got {class_bin:?}");
    }
}

#[test]
fn find_announce_sites_empty_when_no_announces() {
    let request = Map::from([(atom("source"), binary("greet => self name"))]);
    let response = handle_find_announce_sites_in_source(&request);
    let m = assert_ok(&response);
    let Some(Term::List(sites)) = map_get(m, "sites") else {
        panic!("Expected 'sites' list");
    };
    assert!(sites.elements.is_empty(), "got {sites:?}");
}

#[test]
fn find_announce_sites_missing_source_is_error() {
    let request = Map::from([]);
    assert_error(&handle_find_announce_sites_in_source(&request));
}

// ─── handle_find_references_to_in_source ────────────────────────────────────

#[test]
fn find_references_to_finds_class_reference() {
    let request = Map::from([
        (atom("source"), binary("build => Counter new")),
        (atom("class_name"), binary("Counter")),
    ]);
    let lines = ok_lines(&handle_find_references_to_in_source(&request));
    assert_eq!(lines, vec![1]);
}

#[test]
fn find_references_to_empty_when_class_absent() {
    let request = Map::from([
        (atom("source"), binary("build => Foo new")),
        (atom("class_name"), binary("Counter")),
    ]);
    let lines = ok_lines(&handle_find_references_to_in_source(&request));
    assert!(lines.is_empty(), "got {lines:?}");
}

#[test]
fn find_references_to_missing_source_is_error() {
    let request = Map::from([(atom("class_name"), binary("Counter"))]);
    assert_error(&handle_find_references_to_in_source(&request));
}

#[test]
fn find_references_to_missing_class_name_is_error() {
    let request = Map::from([(atom("source"), binary("build => Counter new"))]);
    assert_error(&handle_find_references_to_in_source(&request));
}

// ─── handle_find_field_readers_in_source ────────────────────────────────────

#[test]
fn find_field_readers_finds_self_dot_access() {
    let request = Map::from([
        (atom("source"), binary("getValue => self.value")),
        (atom("field"), binary("value")),
    ]);
    let lines = ok_lines(&handle_find_field_readers_in_source(&request));
    assert_eq!(lines, vec![1]);
}

#[test]
fn find_field_readers_empty_when_field_absent() {
    let request = Map::from([
        (atom("source"), binary("getValue => self.count")),
        (atom("field"), binary("value")),
    ]);
    let lines = ok_lines(&handle_find_field_readers_in_source(&request));
    assert!(lines.is_empty(), "got {lines:?}");
}

#[test]
fn find_field_readers_does_not_report_write_target() {
    // `self.value := 0` — the LHS is a WRITE, not a read.
    let request = Map::from([
        (atom("source"), binary("reset => self.value := 0")),
        (atom("field"), binary("value")),
    ]);
    let lines = ok_lines(&handle_find_field_readers_in_source(&request));
    assert!(
        lines.is_empty(),
        "write target should not appear as read, got {lines:?}"
    );
}

#[test]
fn find_field_readers_missing_source_is_error() {
    let request = Map::from([(atom("field"), binary("value"))]);
    assert_error(&handle_find_field_readers_in_source(&request));
}

#[test]
fn find_field_readers_missing_field_is_error() {
    let request = Map::from([(atom("source"), binary("getValue => self.value"))]);
    assert_error(&handle_find_field_readers_in_source(&request));
}

// ─── handle_find_field_writers_in_source ────────────────────────────────────

#[test]
fn find_field_writers_finds_assignment_target() {
    let request = Map::from([
        (atom("source"), binary("reset => self.value := 0")),
        (atom("field"), binary("value")),
    ]);
    let lines = ok_lines(&handle_find_field_writers_in_source(&request));
    assert_eq!(lines, vec![1]);
}

#[test]
fn find_field_writers_empty_when_field_only_read() {
    let request = Map::from([
        (atom("source"), binary("getValue => self.value")),
        (atom("field"), binary("value")),
    ]);
    let lines = ok_lines(&handle_find_field_writers_in_source(&request));
    assert!(lines.is_empty(), "got {lines:?}");
}

#[test]
fn find_field_writers_finds_rhs_write() {
    // `self.count := self.count + 1` — count is written on the LHS.
    let request = Map::from([
        (
            atom("source"),
            binary("increment => self.count := self.count + 1"),
        ),
        (atom("field"), binary("count")),
    ]);
    let lines = ok_lines(&handle_find_field_writers_in_source(&request));
    assert_eq!(lines, vec![1]);
}

#[test]
fn find_field_writers_missing_source_is_error() {
    let request = Map::from([(atom("field"), binary("value"))]);
    assert_error(&handle_find_field_writers_in_source(&request));
}

#[test]
fn find_field_writers_missing_field_is_error() {
    let request = Map::from([(atom("source"), binary("reset => self.value := 0"))]);
    assert_error(&handle_find_field_writers_in_source(&request));
}

// ─── handle_find_ffi_sites_in_source ────────────────────────────────────────

#[test]
fn find_ffi_sites_finds_keyword_ffi_call() {
    let request = Map::from([
        (
            atom("source"),
            binary("rev: xs => Erlang lists reverse: xs"),
        ),
        (atom("module"), binary("lists")),
        (atom("function"), binary("reverse")),
    ]);
    let lines = ok_lines(&handle_find_ffi_sites_in_source(&request));
    assert_eq!(lines, vec![1]);
}

#[test]
fn find_ffi_sites_finds_unary_ffi_call() {
    let request = Map::from([
        (atom("source"), binary("ref => Erlang erlang make_ref")),
        (atom("module"), binary("erlang")),
        (atom("function"), binary("make_ref")),
    ]);
    let lines = ok_lines(&handle_find_ffi_sites_in_source(&request));
    assert_eq!(lines, vec![1]);
}

#[test]
fn find_ffi_sites_arity_filter_matches() {
    let request = Map::from([
        (
            atom("source"),
            binary("rev: xs => Erlang lists reverse: xs"),
        ),
        (atom("module"), binary("lists")),
        (atom("function"), binary("reverse")),
        (atom("arity"), Term::from(FixInteger::from(1))),
    ]);
    let lines = ok_lines(&handle_find_ffi_sites_in_source(&request));
    assert_eq!(lines, vec![1]);
}

#[test]
fn find_ffi_sites_arity_filter_no_match() {
    // `reverse: xs` has arity 1; querying for arity 2 must return nothing.
    let request = Map::from([
        (
            atom("source"),
            binary("rev: xs => Erlang lists reverse: xs"),
        ),
        (atom("module"), binary("lists")),
        (atom("function"), binary("reverse")),
        (atom("arity"), Term::from(FixInteger::from(2))),
    ]);
    let lines = ok_lines(&handle_find_ffi_sites_in_source(&request));
    assert!(lines.is_empty(), "got {lines:?}");
}

#[test]
fn find_ffi_sites_empty_when_no_ffi_calls() {
    let request = Map::from([
        (atom("source"), binary("greet => self name")),
        (atom("module"), binary("lists")),
        (atom("function"), binary("reverse")),
    ]);
    let lines = ok_lines(&handle_find_ffi_sites_in_source(&request));
    assert!(lines.is_empty(), "got {lines:?}");
}

#[test]
fn find_ffi_sites_missing_source_is_error() {
    let request = Map::from([
        (atom("module"), binary("lists")),
        (atom("function"), binary("reverse")),
    ]);
    assert_error(&handle_find_ffi_sites_in_source(&request));
}

#[test]
fn find_ffi_sites_missing_module_is_error() {
    let request = Map::from([
        (
            atom("source"),
            binary("rev: xs => Erlang lists reverse: xs"),
        ),
        (atom("function"), binary("reverse")),
    ]);
    assert_error(&handle_find_ffi_sites_in_source(&request));
}

#[test]
fn find_ffi_sites_missing_function_is_error() {
    let request = Map::from([
        (
            atom("source"),
            binary("rev: xs => Erlang lists reverse: xs"),
        ),
        (atom("module"), binary("lists")),
    ]);
    assert_error(&handle_find_ffi_sites_in_source(&request));
}
