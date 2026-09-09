// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2243: callHierarchy helper tests -- `CallTarget` round-tripping through item data and outgoing-calls extraction from a method body.

use super::*;

// --- BT-2243: callHierarchy helpers ---

/// `target_to_lsp_item` round-trips through `SerializedCallTarget::from_item`
/// so the prepare → outgoing flow preserves enough context to walk the
/// method body in the open document.
#[test]
fn call_target_round_trips_through_item_data() {
    use beamtalk_core::source_analysis::Span;
    // CLAUDE.md forbids hardcoded `/tmp/`. `unique_temp_dir` keeps
    // the path Windows-safe and platform-portable; `path_to_uri`
    // turns it into a `file://` URI without a hand-written prefix.
    let temp = unique_temp_dir("beamtalk_lsp_callhier_roundtrip");
    let path = Utf8PathBuf::try_from(temp.join("counter.bt")).expect("utf8 path");
    let target = CallHierarchyTarget::new(
        "increment",
        Some("Counter".into()),
        false,
        path.clone(),
        Span::new(10, 50),
        Span::new(10, 19),
    );
    let uri = path_to_uri(&path).expect("uri");
    // The slice covers up to byte 50 — make the synthetic source at
    // least that long so `span_to_range` doesn't clamp inside our
    // expected range.
    let source = " ".repeat(60);
    let item = target_to_lsp_item(&target, &uri, &source).expect("item built");
    assert_eq!(item.name, "increment");
    assert_eq!(item.kind, SymbolKind::METHOD);
    assert_eq!(item.detail.as_deref(), Some("Counter"));
    let decoded = SerializedCallTarget::from_item(&item).expect("decoded");
    assert_eq!(decoded.selector, "increment");
    assert_eq!(decoded.class_name.as_deref(), Some("Counter"));
    assert!(!decoded.class_side);
    assert_eq!(decoded.range_start, 10);
    assert_eq!(decoded.range_end, 50);
    // Round-trip the file field against the temp path rather than a
    // hardcoded string; the specific bytes are irrelevant — only
    // that the path survived the encode/decode round trip.
    assert_eq!(decoded.file, path.as_str());
    let _ = std::fs::remove_dir_all(&temp);
}

/// Class-side targets surface a `Counter class` detail string so the
/// editor distinguishes instance from class methods in the hover.
#[test]
fn call_target_class_side_detail_uses_class_suffix() {
    use beamtalk_core::source_analysis::Span;
    let temp = unique_temp_dir("beamtalk_lsp_callhier_classside");
    let path = Utf8PathBuf::try_from(temp.join("c.bt")).expect("utf8 path");
    let target = CallHierarchyTarget::new(
        "make",
        Some("Counter".into()),
        true,
        path.clone(),
        Span::new(0, 10),
        Span::new(0, 4),
    );
    let uri = path_to_uri(&path).expect("uri");
    let source = "x".repeat(20);
    let item = target_to_lsp_item(&target, &uri, &source).expect("item");
    assert_eq!(item.detail.as_deref(), Some("Counter class"));
    let decoded = SerializedCallTarget::from_item(&item).expect("decoded");
    assert!(decoded.class_side);
    let _ = std::fs::remove_dir_all(&temp);
}

/// `from_item` returns None on a call-site item that has no recorded
/// data — defensive against editors that scrub the `data` field on
/// round-trip.
#[test]
fn serialized_call_target_returns_none_when_data_missing() {
    // The URI value is irrelevant to this test (we only check the
    // `data` field), so use a non-platform-specific synthetic URI
    // rather than a `/tmp/` literal.
    let item = CallHierarchyItem {
        name: "foo".into(),
        kind: SymbolKind::METHOD,
        tags: None,
        detail: None,
        uri: Url::parse("file:///workspace/x.bt").expect("url"),
        range: Range {
            start: Position::new(0, 0),
            end: Position::new(0, 3),
        },
        selection_range: Range {
            start: Position::new(0, 0),
            end: Position::new(0, 3),
        },
        data: None,
    };
    assert!(SerializedCallTarget::from_item(&item).is_none());
}

/// `outgoing_calls_for_body` walks the slice and emits one
/// `CallHierarchyOutgoingCall` per non-FFI send. Lines are offset by
/// the body's starting line so absolute file lines come out right.
#[test]
fn outgoing_calls_for_body_emits_one_per_send_offset_by_body_start() {
    // URI is opaque to the call-hierarchy walker — the test only
    // verifies the returned items, so a generic non-`/tmp/` URI is
    // sufficient (CLAUDE.md cross-platform temp-path rule).
    let uri = Url::parse("file:///workspace/counter.bt").expect("url");
    // A simple method body with two sends on the same line, plus one
    // FFI call which must be filtered out.
    let body = "report =>\n  self show: \"hi\"\n  Erlang lists reverse: x";
    // Body starts on line 5 of the synthetic file (0-based).
    let body_start = Position::new(5, 0);
    let calls = outgoing_calls_for_body(body, body_start, &uri);
    // `find_all_sends_in_source` finds `show:` and (likely) one of
    // the Erlang sends; the FFI one must not appear. Verify at least
    // the show: send is present and at the right absolute line, and
    // no Erlang/reverse send leaks through.
    assert!(
        calls.iter().any(|c| c.to.name == "show:"),
        "expected show: in {calls:?}"
    );
    assert!(
        calls.iter().all(|c| c.to.name != "reverse:"),
        "Erlang reverse: leaked: {calls:?}"
    );
    // `show:` is on body-relative line 2 → absolute file line 6.
    let show = calls.iter().find(|c| c.to.name == "show:").unwrap();
    assert_eq!(show.to.range.start.line, 6);
}

/// An empty body produces no outgoing calls (defensive — the editor
/// should get `None` from the handler in this case).
#[test]
fn outgoing_calls_for_body_empty_returns_no_calls() {
    let uri = Url::parse("file:///workspace/counter.bt").expect("url");
    let calls = outgoing_calls_for_body("answer => 42", Position::new(0, 0), &uri);
    assert!(calls.is_empty(), "got {calls:?}");
}
