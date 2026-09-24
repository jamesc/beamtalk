// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Round-trip tests for ADR 0127 (Traits) syntax: provided methods in
//! `Protocol define:` bodies, and `uses:` lines in class bodies (BT-3587).

use super::common::*;

// --- Provided methods (ADR 0127 §1) ---
//
// A protocol body with provided methods parses cleanly (no diagnostics), so
// these use the ordinary `assert_identity` (which requires a clean parse).

#[test]
fn protocol_provided_method_round_trip() {
    let source = concat!(
        "Protocol define: Greeter\n",
        "  greeting -> String => \"hello\"\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_required_and_provided_mixed_round_trip() {
    let source = concat!(
        "Protocol define: Comparable\n",
        "  < other :: Self -> Boolean\n",
        "\n",
        "  > other :: Self -> Boolean => other < self\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_provided_method_keyword_selector_multiline_body_round_trip() {
    let source = concat!(
        "Protocol define: Comparable\n",
        "  < other :: Self -> Boolean\n",
        "\n",
        "  between: min :: Self and: max :: Self -> Boolean =>\n",
        "    (self >= min) and: [self <= max]\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_provided_method_doc_comment_round_trip() {
    let source = concat!(
        "Protocol define: Comparable\n",
        "  /// Strict ordering. Required.\n",
        "  < other :: Self -> Boolean\n",
        "\n",
        "  /// True if the receiver is strictly greater.\n",
        "  > other :: Self -> Boolean => other < self\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_class_side_requirement_and_provided_instance_method_round_trip() {
    let source = concat!(
        "Protocol define: Creatable\n",
        "  class create -> Self\n",
        "\n",
        "  describe -> String => \"a Creatable\"\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_no_blank_line_between_consecutive_provided_methods_not_inserted() {
    // Matches the ADR's own `comparable.bt` example: consecutive short
    // provisions (no blank line in source) stay without one.
    let source = concat!(
        "Protocol define: Comparable\n",
        "  < other :: Self -> Boolean\n",
        "\n",
        "  > other :: Self -> Boolean => other < self\n",
        "  <= other :: Self -> Boolean => (other < self) not\n",
    );
    assert_identity(source);
}

// --- `uses:` lines (ADR 0127 §2) ---
//
// A class with any `uses:` line always carries the BT-3587 placeholder
// error ("protocol composition is not yet supported" — flattening is
// BT-3588), so `format_source`/`assert_identity` — which refuse to format
// anything with an error diagnostic — cannot be used here.
// `assert_identity_despite_errors` unparses directly instead.

#[test]
fn class_uses_line_round_trip() {
    let source = concat!(
        "sealed typed Value subclass: DateTime\n",
        "  uses: Comparable\n",
        "  field: seconds :: Integer = 0\n",
    );
    assert_identity_despite_errors(source);
}

#[test]
fn class_uses_line_package_qualified_round_trip() {
    let source = concat!("Value subclass: LenientParser\n", "  uses: json@Parser\n",);
    assert_identity_despite_errors(source);
}

#[test]
fn class_uses_line_with_type_args_round_trip() {
    let source = concat!(
        "Actor subclass: WorkerPool\n",
        "  uses: Enumerable(Worker)\n",
    );
    assert_identity_despite_errors(source);
}

#[test]
fn class_uses_line_with_excluding_and_overriding_round_trip() {
    let source = concat!(
        "Value subclass: Report\n",
        "  uses: Labelled excluding: #(#printString, #summary) overriding: #(#hash)\n",
        "  field: title :: String = \"\"\n",
    );
    assert_identity_despite_errors(source);
}

#[test]
fn class_uses_line_excluding_keyword_selector_round_trip() {
    let source = concat!(
        "Value subclass: Pair\n",
        "  uses: Enumerable excluding: #(#at:put:)\n",
    );
    assert_identity_despite_errors(source);
}

#[test]
fn class_multiple_uses_lines_round_trip() {
    let source = concat!(
        "Value subclass: Report\n",
        "  uses: Labelled\n",
        "  uses: Describable\n",
        "\n",
        "  printString -> String => self title\n",
    );
    assert_identity_despite_errors(source);
}

#[test]
fn class_uses_line_idempotent() {
    // `assert_idempotent` doesn't gate on diagnostics at all, so it works
    // unchanged here — included for belt-and-braces coverage alongside the
    // exact-text checks above.
    assert_idempotent(
        "Value subclass: Report\n  uses: Labelled excluding: #(#printString)\n  field: title :: String = \"\"\n",
    );
}
