// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary construction (ADR 0143, BT-4346) unparse round-trips: a
//! construction reprints with its own spelling, and a segment reads the
//! same whether it builds (`<<...>>` as an expression) or matches (`<<...>>`
//! in a `match:` arm).

use super::common::*;

/// One construction per segment kind the parser accepts, each already in
/// canonical spelling.
const CANONICAL_CONSTRUCTIONS: &[&str] = &[
    "<<>>",
    "<<16:8>>",
    "<<v:4>>",
    "<<payload:len/binary>>",
    "<<p/binary>>",
    "<<v:(n * 8)>>",
    "<<(payload size):16>>",
    "<<(a + b)>>",
    "<<h:16/integer-unsigned-big>>",
    "<<v:32/signed-little>>",
    "<<s:8/unit:2>>",
    "<<s:2/integer-unit:8>>",
    "<<b/bitstring>>",
    "<<c/utf8>>",
    "<<c/utf16>>",
    "<<c/utf32>>",
    "<<1.5/float>>",
    "<<-1.5:32/float>>",
    "<<-1:8/signed>>",
    "<<$A>>",
    "<<\"hi\">>",
    "<<\"hi\"/utf16>>",
    "<<16:8, 3:8, len:24, payload/binary>>",
    "<<(<<1:4, 2:4>>)/binary>>",
];

#[test]
fn construction_round_trips_with_identical_spelling() {
    for source in CANONICAL_CONSTRUCTIONS {
        assert_identity(&format!("{source}\n"));
    }
}

#[test]
fn construction_in_context_round_trips() {
    for source in [
        "frame := <<16:8, 3:8, len:24, payload/binary>>\n",
        "<<16:8, payload/binary>> size\n",
        "x at: <<1:8>> put: y ++ <<2:8>>\n",
        "^<<v:8>>\n",
    ] {
        assert_identity(source);
    }
}

#[test]
fn construction_spelling_matches_pattern_spelling() {
    // ADR 0143: one grammar, two directions. A segment list that parses as
    // both a construction and a pattern reprints identically in both
    // positions, including the canonicalised specifier spellings
    // (`bits` → `bitstring`, `bytes` → `binary`) and chain order.
    for segments in [
        "16:8, len:24, payload/binary",
        "h:16/big-unsigned-integer",
        "v:32/little-signed",
        "s:8/unit:2, rest/bits",
        "c/utf16, rest/bytes",
        "1.5/float, $A, \"hi\"",
        "-1:8/signed, v:(n * 8)",
    ] {
        let source = format!("<<{segments}>> match: [<<{segments}>> -> 1]");
        let unparsed = unparse_module(&parse_source(&source));
        let line = unparsed.trim_end();
        let (built, rest) = line
            .split_once(" match: [")
            .unwrap_or_else(|| panic!("unexpected unparse of {source}: {unparsed}"));
        let matched = rest
            .strip_suffix(" -> 1]")
            .unwrap_or_else(|| panic!("unexpected unparse of {source}: {unparsed}"));
        assert_eq!(
            built, matched,
            "construction and pattern spell `{segments}` differently"
        );
        assert_idempotent(&source);
    }
}

#[test]
fn construction_specifier_spellings_canonicalise_like_patterns() {
    // Aliases canonicalise the same way they do on the match side, so a
    // second pass is a no-op.
    for (source, canonical) in [
        ("<<b/bits>>", "<<b/bitstring>>\n"),
        ("<<b/bytes>>", "<<b/binary>>\n"),
        (
            "<<h:16/big-unsigned-integer>>",
            "<<h:16/integer-unsigned-big>>\n",
        ),
        ("<<v :4>>", "<<v:4>>\n"),
    ] {
        assert_eq!(
            format_source(source).as_deref(),
            Some(canonical),
            "{source}"
        );
        assert_idempotent(source);
    }
}
