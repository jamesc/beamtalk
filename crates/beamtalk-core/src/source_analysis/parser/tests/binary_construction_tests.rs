// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary construction parsing (ADR 0143, BT-4346): a leading `<<` in
//! operand position builds a binary with the match side's segment grammar.

use super::*;
use crate::ast::{BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Literal};

/// Parses `source` (one expression statement, no errors) and answers the
/// segments of the binary construction it must be.
fn construction_segments(source: &str) -> Vec<BinarySegment<Expression>> {
    let module = parse_ok(source);
    assert_eq!(module.expressions.len(), 1, "one statement in {source}");
    match &module.expressions[0].expression {
        Expression::BinaryConstruction { segments, .. } => segments.clone(),
        other => panic!("expected a binary construction for {source}, got {other:?}"),
    }
}

fn single_segment(source: &str) -> BinarySegment<Expression> {
    let segments = construction_segments(source);
    assert_eq!(segments.len(), 1, "one segment in {source}");
    segments.into_iter().next().unwrap()
}

fn assert_identifier(expr: &Expression, name: &str) {
    assert!(
        matches!(expr, Expression::Identifier(id) if id.name == name),
        "expected identifier {name}, got {expr:?}"
    );
}

fn assert_int(expr: Option<&Expression>, n: i64) {
    assert!(
        matches!(expr, Some(Expression::Literal(Literal::Integer(v), _)) if *v == n),
        "expected integer {n}, got {expr:?}"
    );
}

#[test]
fn empty_construction() {
    assert!(construction_segments("<<>>").is_empty());
}

#[test]
fn integer_literal_with_integer_size() {
    let seg = single_segment("<<16:8>>");
    assert_int(Some(&seg.value), 16);
    assert_int(seg.size.as_deref(), 8);
    assert_eq!(seg.segment_type, None);
    assert_eq!(seg.unit, None);
}

#[test]
fn variable_with_integer_size() {
    // `v:4` lexes as Keyword("v:") + Integer, exactly as in patterns.
    let seg = single_segment("<<v:4>>");
    assert_identifier(&seg.value, "v");
    assert_int(seg.size.as_deref(), 4);
    // The value's span excludes the `:`.
    assert_eq!(seg.value.span(), Span::new(2, 3));
}

#[test]
fn variable_with_spaced_colon_size() {
    let seg = single_segment("<<v :4>>");
    assert_identifier(&seg.value, "v");
    assert_int(seg.size.as_deref(), 4);
}

#[test]
fn variable_size() {
    let seg = single_segment("<<payload:len/binary>>");
    assert_identifier(&seg.value, "payload");
    assert_identifier(seg.size.as_deref().unwrap(), "len");
    assert_eq!(seg.segment_type, Some(BinarySegmentType::Binary));
}

#[test]
fn parenthesized_size() {
    let seg = single_segment("<<v:(n * 8)>>");
    assert!(matches!(
        seg.size.as_deref(),
        Some(Expression::Parenthesized { .. })
    ));
}

#[test]
fn parenthesized_value() {
    let seg = single_segment("<<(payload size):16>>");
    let Expression::Parenthesized { expression, .. } = &seg.value else {
        panic!("expected a parenthesized value, got {:?}", seg.value);
    };
    assert!(matches!(
        expression.as_ref(),
        Expression::MessageSend {
            selector: MessageSelector::Unary(s),
            ..
        } if s == "size"
    ));
    assert_int(seg.size.as_deref(), 16);
}

#[test]
fn parenthesized_value_without_size() {
    let seg = single_segment("<<(a + b)>>");
    assert!(matches!(seg.value, Expression::Parenthesized { .. }));
    assert!(seg.size.is_none());
}

#[test]
fn whole_binary_variable() {
    let seg = single_segment("<<p/binary>>");
    assert_identifier(&seg.value, "p");
    assert!(seg.size.is_none());
    assert_eq!(seg.segment_type, Some(BinarySegmentType::Binary));
}

#[test]
fn typed_specifier_chain() {
    let seg = single_segment("<<h:16/big-unsigned-integer>>");
    assert_identifier(&seg.value, "h");
    assert_int(seg.size.as_deref(), 16);
    assert_eq!(seg.segment_type, Some(BinarySegmentType::Integer));
    assert_eq!(seg.signedness, Some(BinarySignedness::Unsigned));
    assert_eq!(seg.endianness, Some(BinaryEndianness::Big));
}

#[test]
fn signed_little_chain() {
    let seg = single_segment("<<v:32/signed-little>>");
    assert_eq!(seg.signedness, Some(BinarySignedness::Signed));
    assert_eq!(seg.endianness, Some(BinaryEndianness::Little));
    assert_eq!(seg.segment_type, None);
}

#[test]
fn unit_specifier() {
    let seg = single_segment("<<s:8/unit:2>>");
    assert_identifier(&seg.value, "s");
    assert_int(seg.size.as_deref(), 8);
    assert_eq!(seg.unit, Some(2));
}

#[test]
fn unit_in_chain() {
    let seg = single_segment("<<s:2/integer-unit:8>>");
    assert_eq!(seg.segment_type, Some(BinarySegmentType::Integer));
    assert_eq!(seg.unit, Some(8));
}

#[test]
fn utf_types() {
    for (spec, ty) in [
        ("utf8", BinarySegmentType::Utf8),
        ("utf16", BinarySegmentType::Utf16),
        ("utf32", BinarySegmentType::Utf32),
    ] {
        let seg = single_segment(&format!("<<c/{spec}>>"));
        assert_eq!(seg.segment_type, Some(ty), "{spec}");
    }
}

#[test]
fn bits_and_bitstring_types() {
    for spec in ["bits", "bitstring"] {
        let seg = single_segment(&format!("<<b/{spec}>>"));
        assert_eq!(
            seg.segment_type,
            Some(BinarySegmentType::Bitstring),
            "{spec}"
        );
    }
}

#[test]
fn float_literal() {
    let seg = single_segment("<<1.5/float>>");
    assert!(
        matches!(seg.value, Expression::Literal(Literal::Float(f), _) if (f - 1.5).abs() < f64::EPSILON)
    );
    assert_eq!(seg.segment_type, Some(BinarySegmentType::Float));
}

#[test]
fn negative_float_literal() {
    let seg = single_segment("<<- 1.5:32/float>>");
    assert!(
        matches!(seg.value, Expression::Literal(Literal::Float(f), _) if (f + 1.5).abs() < f64::EPSILON)
    );
    assert_int(seg.size.as_deref(), 32);
}

#[test]
fn negative_integer_literal() {
    let seg = single_segment("<<-1:8/signed>>");
    assert_int(Some(&seg.value), -1);
    assert_eq!(seg.signedness, Some(BinarySignedness::Signed));
}

#[test]
fn character_literal() {
    let seg = single_segment("<<$A>>");
    assert!(matches!(
        seg.value,
        Expression::Literal(Literal::Character('A'), _)
    ));
}

#[test]
fn string_literal() {
    let seg = single_segment("<<\"hi\">>");
    assert!(matches!(&seg.value, Expression::Literal(Literal::String(s), _) if s == "hi"));
    assert_eq!(seg.segment_type, None);
}

#[test]
fn string_literal_utf16() {
    let seg = single_segment("<<\"hi\"/utf16>>");
    assert_eq!(seg.segment_type, Some(BinarySegmentType::Utf16));
}

#[test]
fn frame_layout() {
    let segments = construction_segments("<<16:8, 3:8, len:24, payload/binary>>");
    assert_eq!(segments.len(), 4);
    assert_identifier(&segments[2].value, "len");
    assert_int(segments[2].size.as_deref(), 24);
    assert_identifier(&segments[3].value, "payload");
}

#[test]
fn construction_span_covers_delimiters() {
    let module = parse_ok("<<v:4>>");
    assert_eq!(module.expressions[0].expression.span(), Span::new(0, 7));
}

#[test]
fn construction_as_assignment_value() {
    let module = parse_ok("frame := <<16:8, payload/binary>>");
    let Expression::Assignment { value, .. } = &module.expressions[0].expression else {
        panic!("expected an assignment");
    };
    assert!(matches!(
        value.as_ref(),
        Expression::BinaryConstruction { .. }
    ));
}

#[test]
fn construction_as_receiver() {
    let module = parse_ok("<<16:8, payload/binary>> size");
    let Expression::MessageSend { receiver, .. } = &module.expressions[0].expression else {
        panic!("expected a unary send");
    };
    assert!(matches!(
        receiver.as_ref(),
        Expression::BinaryConstruction { .. }
    ));
}

#[test]
fn construction_as_keyword_and_binary_argument() {
    let module = parse_ok("x at: <<1:8>> put: (y ++ <<2:8>>)");
    let Expression::MessageSend { arguments, .. } = &module.expressions[0].expression else {
        panic!("expected a keyword send");
    };
    assert!(matches!(
        arguments[0],
        Expression::BinaryConstruction { .. }
    ));
    let Expression::Parenthesized { expression, .. } = &arguments[1] else {
        panic!("expected a parenthesized argument");
    };
    let Expression::MessageSend { arguments, .. } = expression.as_ref() else {
        panic!("expected `++`");
    };
    assert!(matches!(
        arguments[0],
        Expression::BinaryConstruction { .. }
    ));
}

#[test]
fn nested_construction_in_parenthesized_value() {
    let segments = construction_segments("<<(<<1:4, 2:4>>)/binary>>");
    let Expression::Parenthesized { expression, .. } = &segments[0].value else {
        panic!("expected a parenthesized value");
    };
    assert!(matches!(
        expression.as_ref(),
        Expression::BinaryConstruction { .. }
    ));
}

#[test]
fn match_arm_still_parses_a_pattern() {
    // The same text in a match arm is a pattern, not a construction.
    use crate::ast::Pattern;
    let module = parse_ok("<<16:8, len:8>> match: [<<16:8, n:8>> -> n]");
    let Expression::Match { value, arms, .. } = &module.expressions[0].expression else {
        panic!("expected a match");
    };
    assert!(matches!(
        value.as_ref(),
        Expression::BinaryConstruction { .. }
    ));
    assert!(matches!(arms[0].pattern, Pattern::Binary { .. }));
}

#[test]
fn infix_ltlt_is_reported_not_read_as_a_construction() {
    // Only a leading `<<` opens a construction. `<<` is not a binary
    // operator (it lexes as `LtLt`, never `BinarySelector`), so one right
    // after an operand gets a targeted diagnostic instead of silently
    // starting a second statement.
    let tokens = lex_with_eof("a << b");
    let (module, diagnostics) = parse(tokens);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].message, "'<<' is not a binary operator");
    assert!(
        diagnostics[0]
            .hint
            .as_deref()
            .is_some_and(|h| h.contains("leading '<<' opens a binary construction")),
        "{diagnostics:?}"
    );
    assert_eq!(module.expressions.len(), 1);
    assert!(module.expressions[0].expression.is_error());
    assert!(
        !module
            .expressions
            .iter()
            .any(|s| matches!(s.expression, Expression::BinaryConstruction { .. }))
    );
}

#[test]
fn infix_ltlt_inside_a_keyword_argument_is_reported() {
    let diagnostics = parse_err("x foo: a << b");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message == "'<<' is not a binary operator"),
        "{diagnostics:?}"
    );
}

#[test]
fn ltlt_on_a_new_line_starts_a_construction_statement() {
    let module = parse_ok("x := 1\n<<x:8>> size");
    assert_eq!(module.expressions.len(), 2);
    let Expression::MessageSend { receiver, .. } = &module.expressions[1].expression else {
        panic!("expected a unary send");
    };
    assert!(matches!(
        receiver.as_ref(),
        Expression::BinaryConstruction { .. }
    ));
}

// --- Diagnostics ---

#[test]
fn wildcard_value_is_an_error() {
    let diagnostics = parse_err("<<_:8>>");
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("'_' cannot be a value in a binary construction")),
        "{diagnostics:?}"
    );
}

#[test]
fn bare_wildcard_value_is_an_error() {
    let diagnostics = parse_err("<<_>>");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("'_' cannot be a value")),
        "{diagnostics:?}"
    );
}

#[test]
fn unparenthesized_send_value_is_an_error_with_a_hint() {
    // `x size:8` reads as segment `x` followed by the keyword `size:`, so
    // the segment value must be parenthesised. Recovery skips to the `>>`,
    // so nothing else is reported and the next statement parses.
    let (module, diagnostics) = parse(lex_with_eof("y := <<x size:8>>. z := 1"));
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(
        diagnostics[0].message,
        "Expected '>>' to close binary construction"
    );
    assert!(
        diagnostics[0]
            .hint
            .as_deref()
            .is_some_and(|h| h.contains("<<(x size):8>>")),
        "{diagnostics:?}"
    );
    assert_eq!(module.expressions.len(), 2);
}

#[test]
fn non_operand_value_is_an_error_with_a_hint() {
    let diagnostics = parse_err("<<#foo>>");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    let diag = diagnostics
        .iter()
        .find(|d| d.message.contains("binary construction segment"))
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert!(
        diag.hint
            .as_deref()
            .is_some_and(|h| h.contains("parentheses")),
        "{diag:?}"
    );
}

#[test]
fn missing_close_is_an_error() {
    let diagnostics = parse_err("<<v:8");
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("Expected '>>' to close binary construction")),
        "{diagnostics:?}"
    );
}

#[test]
fn trailing_comma_is_an_error() {
    let diagnostics = parse_err("<<v:8, >>");
    assert!(
        diagnostics.iter().any(
            |d| d.message == "Trailing comma is not allowed before '>>' in binary construction"
        ),
        "{diagnostics:?}"
    );
}

#[test]
fn shared_specifier_errors_apply() {
    // The specifier parser is the match side's: unknown specifiers and unit
    // conflicts are reported identically.
    let diagnostics = parse_err("<<v:8/bogus>>");
    assert!(
        diagnostics.iter().any(|d| d.message.contains("bogus")),
        "{diagnostics:?}"
    );
    let diagnostics = parse_err("<<v:2/bits-unit:8>>");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("conflict in unit specification")),
        "{diagnostics:?}"
    );
}

#[test]
fn invalid_size_is_an_error() {
    let diagnostics = parse_err("<<v:#x>>");
    assert!(
        diagnostics.iter().any(|d| d.message.contains("size")),
        "{diagnostics:?}"
    );
}
