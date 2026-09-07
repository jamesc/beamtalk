// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Keyword-message block-argument formatting (BT-1064), 3+/4-keyword
//! always-break rules (BT-1294), protocol definition round-trips (BT-1618),
//! and type-alias round-trips (ADR 0108 Phase 1, BT-2894) (moved from
//! `unparse/mod.rs` per BT-3450).

use super::common::*;

// --- Keyword message formatting with block arguments (BT-1064) ---

#[test]
fn keyword_single_stmt_block_stays_inline() {
    // Single-statement block: keyword stays on the same line as receiver.
    let source = "Object subclass: A\n  m => x ifTrue: [^42]\n";
    let module = parse_source(source);
    let out = unparse_module(&module);
    assert!(
        out.contains("x ifTrue: [^42]"),
        "single-stmt block should stay inline: {out}"
    );
}

#[test]
fn keyword_multi_stmt_block_breaks_keywords() {
    // Multi-statement block: all keywords break to their own indented lines.
    let source = "Object subclass: A\n  m =>\n    flag\n      ifTrue: [\n        a showCr: \"x\"\n        b showCr: \"y\"\n      ]\n      ifFalse: [nil]\n";
    let module = parse_source(source);
    let out = unparse_module(&module);
    assert!(
        out.contains("flag\n      ifTrue: ["),
        "receiver should be on its own line before ifTrue: {out}"
    );
    assert!(
        out.contains("      ifFalse: [nil]"),
        "ifFalse: should be on its own indented line: {out}"
    );
}

#[test]
fn keyword_multi_stmt_block_idempotent() {
    assert_idempotent(
        "Object subclass: A\n  m =>\n    flag\n      ifTrue: [\n        self showCr: \"yes\"\n        self showCr: \"done\"\n      ]\n      ifFalse: [nil]\n",
    );
}

#[test]
fn keyword_single_keyword_multi_stmt_breaks() {
    // Single keyword with a multi-statement block: keyword breaks to own line.
    let source = "Object subclass: A\n  m =>\n    coll\n      do: [\n        self showCr: \"a\"\n        self showCr: \"b\"\n      ]\n";
    let module = parse_source(source);
    let out = unparse_module(&module);
    assert!(
        out.contains("coll\n      do: ["),
        "receiver should be on its own line before do: {out}"
    );
}

// --- BT-1294: 3+ keyword always-break, block body multiline, no ]] stacking ---

#[test]
fn three_keywords_always_break() {
    // 3 keyword parts → always break, regardless of total length.
    let source = "Object subclass: A\n  m => CandidateFilter filterEligible: c config: self.config running: self.running\n";
    let module = parse_source(source);
    let out = unparse_module(&module);
    assert!(
        out.contains("CandidateFilter\n"),
        "receiver must be on its own line for 3+ keywords: {out}"
    );
    assert!(
        out.contains("      filterEligible:"),
        "first keyword must be indented: {out}"
    );
    assert!(
        out.contains("      config:"),
        "second keyword must be indented: {out}"
    );
    assert!(
        out.contains("      running:"),
        "third keyword must be indented: {out}"
    );
}

#[test]
fn three_keywords_always_break_idempotent() {
    let source = "Object subclass: A\n  m =>\n    CandidateFilter\n      filterEligible: c\n      config: self.config\n      running: self.running\n";
    assert_idempotent(source);
}

#[test]
fn four_keywords_always_break() {
    // 4 keyword parts — real symphony pattern (orchestrator.bt:96).
    let source = "Object subclass: A\n  m => CandidateFilter terminatedEntries: self.running stateMap: stateMap activeStates: self.config trackerActiveStates terminalStates: self.config trackerTerminalStates\n";
    let module = parse_source(source);
    let out = unparse_module(&module);
    // No line should exceed 120 chars (measured by raw byte length).
    for line in out.lines() {
        assert!(
            line.len() <= 120,
            "line exceeds 120 chars after formatting: {line:?}"
        );
    }
    assert!(
        out.contains("      terminatedEntries:"),
        "first keyword must break: {out}"
    );
    assert!(
        out.contains("      terminalStates:"),
        "last keyword must break: {out}"
    );
}

#[test]
fn two_keywords_short_stays_inline() {
    // 2 keyword parts that are short → stays inline (no change to current behaviour).
    let source = "Object subclass: A\n  m => dict at: #key ifAbsent: [nil]\n";
    let module = parse_source(source);
    let out = unparse_module(&module);
    assert!(
        out.contains("dict at: #key ifAbsent: [nil]"),
        "short 2-keyword message should stay inline: {out}"
    );
}

#[test]
fn block_with_multiline_body_breaks_outer_close() {
    // Block whose body renders multiline (keyword send with multiline block args)
    // must use always-break form so `]` gets its own line (no `]]` stacking).
    let source = "Object subclass: A\n  m =>\n    resp\n      andThen: [:r |\n        r ok\n          ifTrue: [\n            x := r body\n            Result ok: x\n          ]\n          ifFalse: [Result error: #http_error]\n      ]\n";
    let module = parse_source(source);
    let out = unparse_module(&module);
    // `]]` must not appear — every `]` must be on its own line.
    assert!(!out.contains("]]"), "no `]]` stacking allowed: {out}");
    assert_idempotent(source);
}

#[test]
fn block_with_single_short_keyword_stays_inline() {
    // Single-statement block whose body is a short 1-keyword send must stay inline.
    let source = "Object subclass: A\n  m => flag ifTrue: [self showCr: \"yes\"]\n";
    let module = parse_source(source);
    let out = unparse_module(&module);
    assert!(
        out.contains("ifTrue: [self showCr: \"yes\"]"),
        "short single-keyword block body should stay inline: {out}"
    );
}

// --- BT-1294: class-side method with 3+ keyword body (non-idempotency regression) ---

#[test]
fn class_side_four_keyword_body_idempotent() {
    // Actor subclass with a `class` method whose body is a 4-keyword send followed
    // by an instance method.  The formatter must not merge the broken keyword lines
    // with the next method's selector on the second pass.
    let source = concat!(
        "Actor subclass: Subprocess\n",
        "\n",
        "  class open: command args: args env: env dir: dir -> Result =>\n",
        "    (Erlang beamtalk_subprocess)\n",
        "      open: command\n",
        "      args: args\n",
        "      env: env\n",
        "      dir: dir\n",
        "\n",
        "  writeLine: data -> Nil =>\n",
        "    (Erlang beamtalk_subprocess) writeLine: self data: data\n",
    );
    assert_idempotent(source);
}

#[test]
fn class_side_four_keyword_body_identity() {
    // Same source must be already in canonical form (no change on first pass).
    let source = concat!(
        "Actor subclass: Subprocess\n",
        "\n",
        "  class open: command args: args env: env dir: dir -> Result =>\n",
        "    (Erlang beamtalk_subprocess)\n",
        "      open: command\n",
        "      args: args\n",
        "      env: env\n",
        "      dir: dir\n",
        "\n",
        "  writeLine: data -> Nil =>\n",
        "    (Erlang beamtalk_subprocess) writeLine: self data: data\n",
    );
    assert_identity(source);
}

// --- Protocol round-trip (BT-1618) ---

#[test]
fn protocol_class_method_doc_comment_round_trip() {
    // BT-1618: `class` prefix must appear on the signature line, after doc comments.
    let source = concat!(
        "Protocol define: Parseable\n",
        "  /// Reconstruct from string.\n",
        "  class fromString: aString :: String -> Self\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_class_method_no_doc_comment_round_trip() {
    let source = concat!("Protocol define: Creatable\n", "  class create -> Self\n",);
    assert_identity(source);
}

#[test]
fn bare_primitive_round_trips_without_selector_string() {
    // BT-2724: a bare `@primitive` (selector inferred from the method) must
    // not be rewritten with an explicit selector string by the formatter.
    let source = concat!("Object subclass: Foo\n", "  size => @primitive\n");
    let formatted = format_source(source).expect("format_source must succeed");
    assert!(
        formatted.contains("@primitive\n"),
        "expected bare @primitive, got:\n{formatted}"
    );
    assert!(
        !formatted.contains("@primitive \""),
        "bare @primitive should not gain a selector string:\n{formatted}"
    );
    assert_idempotent(source);
}

#[test]
fn explicit_primitive_selector_string_preserved() {
    // BT-2724: an explicit selector string (genuine rename) is preserved.
    let source = concat!(
        "Object subclass: Foo\n",
        "  signal => @primitive \"classSignal\"\n"
    );
    let formatted = format_source(source).expect("format_source must succeed");
    assert!(
        formatted.contains("@primitive \"classSignal\""),
        "explicit selector string should be preserved, got:\n{formatted}"
    );
}

#[test]
fn protocol_instance_method_doc_comment_round_trip() {
    let source = concat!(
        "Protocol define: Displayable\n",
        "  /// Convert to display string.\n",
        "  asString -> String\n",
    );
    assert_identity(source);
}

// --- BT-2930 regression: leading `//`/`/* */` comments on protocol
// method signatures must survive a `beamtalk fmt` round-trip. ---

#[test]
fn protocol_instance_method_leading_comment_round_trip() {
    // Exact repro from BT-2930: an ordinary leading `//` comment (no doc
    // comment) directly above an instance-side signature was silently
    // dropped because `unparse_protocol_method_signature` never called
    // `unparse_comment_attachment_leading`.
    let source = concat!(
        "Protocol define: Displayable\n",
        "  // A leading comment.\n",
        "  asString -> String\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_instance_method_leading_block_comment_round_trip() {
    let source = concat!(
        "Protocol define: Displayable\n",
        "  /* A leading block comment. */\n",
        "  asString -> String\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_instance_method_leading_comment_and_doc_comment_round_trip() {
    // Leading comments and doc comments must both render, in the right
    // order (leading comment first, then doc comment, then signature) —
    // matching every other declaration kind.
    let source = concat!(
        "Protocol define: Displayable\n",
        "  // A leading comment.\n",
        "  /// Convert to display string.\n",
        "  asString -> String\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_class_method_leading_comment_round_trip() {
    let source = concat!(
        "Protocol define: Creatable\n",
        "  // A leading comment.\n",
        "  class create -> Self\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_class_method_leading_comment_and_doc_comment_round_trip() {
    // Verifies interaction with the existing doc-comment-before-`class`-
    // keyword handling: leading comment, then doc comment, then `class`
    // on the signature line.
    let source = concat!(
        "Protocol define: Parseable\n",
        "  // A leading comment.\n",
        "  /// Reconstruct from string.\n",
        "  class fromString: aString :: String -> Self\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_trailing_comment_round_trip() {
    // BT-2906: a trailing end-of-line comment on the `Protocol define:`
    // declaration line must round-trip losslessly, mirroring the
    // identical fix for `type` declarations below.
    let source = concat!(
        "Protocol define: Sortable  // comment\n",
        "  sortKey -> Object\n",
    );
    assert_identity(source);
}

// --- BT-2946 regression: blank lines between protocol method
// signatures must survive a `beamtalk fmt` round-trip, without being
// force-inserted when absent (mirrors BT-2929 for top-level
// declarations). ---

#[test]
fn protocol_blank_line_between_instance_signatures_round_trip() {
    let source = concat!(
        "Protocol define: Displayable\n",
        "  asString -> String\n",
        "\n",
        "  displayString -> String\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_no_blank_line_between_instance_signatures_not_inserted() {
    let source = concat!(
        "Protocol define: Displayable\n",
        "  asString -> String\n",
        "  displayString -> String\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_blank_line_between_class_signatures_round_trip() {
    let source = concat!(
        "Protocol define: Creatable\n",
        "  class create -> Self\n",
        "\n",
        "  class createEmpty -> Self\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_blank_line_at_instance_class_boundary_round_trip() {
    // Mix: a blank line between the last instance signature and the
    // first class signature must also be preserved.
    let source = concat!(
        "Protocol define: Creatable\n",
        "  asString -> String\n",
        "\n",
        "  class create -> Self\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_no_blank_line_at_instance_class_boundary_not_inserted() {
    let source = concat!(
        "Protocol define: Creatable\n",
        "  asString -> String\n",
        "  class create -> Self\n",
    );
    assert_identity(source);
}

#[test]
fn protocol_blank_line_before_first_signature_not_preserved() {
    // A blank line between the header and the very first signature is
    // dropped, same as pre-existing behaviour for top-level
    // declarations (BT-2929) — only inter-signature gaps are
    // preserved.
    let source = concat!(
        "Protocol define: Displayable\n",
        "\n",
        "  asString -> String\n",
    );
    let formatted = format_source(source).expect("format_source must succeed");
    assert_eq!(
        formatted,
        "Protocol define: Displayable\n  asString -> String\n"
    );
}

#[test]
fn protocol_blank_line_with_leading_comment_round_trip() {
    // Interaction with BT-2930: a blank line before a signature that
    // also has a leading `//` comment must place the blank line before
    // the comment, not between the comment and the signature.
    let source = concat!(
        "Protocol define: Displayable\n",
        "  asString -> String\n",
        "\n",
        "  // Convert to display string.\n",
        "  displayString -> String\n",
    );
    assert_identity(source);
}

// --- Type alias round-trip (ADR 0108, Phase 1, BT-2894) ---

#[test]
fn type_alias_simple_round_trip() {
    let source = "type Port = Integer\n";
    assert_identity(source);
}

#[test]
fn type_alias_singleton_union_round_trip() {
    let source = "type RestartStrategy = #temporary | #transient | #permanent\n";
    assert_identity(source);
}

#[test]
fn type_alias_doc_comment_round_trip() {
    let source = concat!(
        "/// How a supervised child restarts after exit.\n",
        "type RestartStrategy = #temporary | #transient | #permanent\n",
    );
    assert_identity(source);
}

#[test]
fn type_alias_generic_round_trip() {
    let source = "type IntList = List(Integer)\n";
    assert_identity(source);
}

#[test]
fn type_alias_difference_round_trip() {
    let source = "type PublicTag = Symbol \\ (#reserved | #internal)\n";
    assert_identity(source);
}

#[test]
fn internal_type_alias_round_trip() {
    // ADR 0071, ADR 0108 Phase 5, BT-2898.
    let source = "internal type ParserState = Integer | String\n";
    assert_identity(source);
}

#[test]
fn type_alias_trailing_comment_round_trip() {
    // BT-2906: a trailing end-of-line comment on the declaration line
    // must round-trip losslessly instead of being dropped.
    let source = "type Port = Integer  // comment\n";
    assert_identity(source);
}
