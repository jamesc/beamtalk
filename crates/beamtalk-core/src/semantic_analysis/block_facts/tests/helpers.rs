// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::*;
use crate::ast::{CascadeMessage, KeywordPart, Literal};
use crate::source_analysis::Span;

// ─── captured_local_mutations_from_analysis ────────────────────────────────

#[test]
fn clma_returns_sorted_intersection_of_local_writes_and_captured_reads() {
    let analysis = BlockMutationAnalysis {
        local_writes: ["x", "y", "z"].iter().map(ToString::to_string).collect(),
        captured_reads: ["y", "z", "w"].iter().map(ToString::to_string).collect(),
        ..Default::default()
    };
    assert_eq!(
        captured_local_mutations_from_analysis(&analysis),
        vec!["y", "z"]
    );
}

#[test]
fn clma_returns_empty_when_no_intersection() {
    let analysis = BlockMutationAnalysis {
        local_writes: ["x"].iter().map(ToString::to_string).collect(),
        captured_reads: ["y"].iter().map(ToString::to_string).collect(),
        ..Default::default()
    };
    assert!(captured_local_mutations_from_analysis(&analysis).is_empty());
}

#[test]
fn clma_returns_empty_for_default_analysis() {
    assert!(captured_local_mutations_from_analysis(&BlockMutationAnalysis::default()).is_empty());
}

// ─── normalize_cascade ─────────────────────────────────────────────────────

#[test]
fn normalize_cascade_message_send_receiver_unfolds_first_message() {
    // The parser folds the first cascade message into the receiver as a MessageSend.
    // normalize_cascade must unwrap it and prepend to the messages list.
    let expr = parse_first_expr("blk value: 1; value: 2");
    let Expression::Cascade {
        receiver, messages, ..
    } = &expr
    else {
        panic!("expected Cascade, got {expr:?}");
    };
    let (underlying, all_msgs) = normalize_cascade(receiver, messages);
    assert!(
        matches!(underlying, Expression::Identifier(id) if id.name == "blk"),
        "expected blk identifier as underlying receiver"
    );
    assert_eq!(all_msgs.len(), 2, "both value: messages should be present");
}

#[test]
fn normalize_cascade_non_message_send_receiver_returns_it_directly() {
    // This branch is not reachable from normally-parsed code (the parser
    // always folds the first cascade message into the receiver). We construct
    // the AST directly to exercise the else branch.
    let receiver = make_expr_id("blk");
    let msg = CascadeMessage::new(
        MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(4, 10))]),
        vec![Expression::Literal(Literal::Integer(1), Span::new(11, 12))],
        Span::new(4, 12),
    );
    let msgs = [msg];
    let (underlying, all_msgs) = normalize_cascade(&receiver, &msgs);
    assert!(
        matches!(underlying, Expression::Identifier(id) if id.name == "blk"),
        "receiver passed through unchanged"
    );
    assert_eq!(all_msgs.len(), 1, "one message in the list");
}

#[test]
fn normalize_cascade_non_message_send_receiver_empty_messages() {
    // Edge: receiver is not a MessageSend and messages is empty.
    let receiver = make_expr_id("x");
    let (underlying, all_msgs) = normalize_cascade(&receiver, &[]);
    assert!(matches!(underlying, Expression::Identifier(id) if id.name == "x"));
    assert!(all_msgs.is_empty());
}

// ─── is_safe_value_family_selector ─────────────────────────────────────────

#[test]
fn safe_value_selectors_are_recognised() {
    for src in [
        "blk value",
        "blk value: 1",
        "blk value: 1 value: 2",
        "blk value: 1 value: 2 value: 3",
    ] {
        let expr = parse_first_expr(src);
        let Expression::MessageSend { selector, .. } = &expr else {
            panic!("expected MessageSend for {src:?}");
        };
        assert!(is_safe_value_family_selector(selector), "{src}");
    }
}

#[test]
fn non_value_selectors_are_not_safe() {
    for src in ["blk printString", "blk + 1", "blk do: [:e | e]"] {
        let expr = parse_first_expr(src);
        let Expression::MessageSend { selector, .. } = &expr else {
            panic!("expected MessageSend for {src:?}");
        };
        assert!(!is_safe_value_family_selector(selector), "{src}");
    }
}

// ─── stored_block_var_uses ─────────────────────────────────────────────────

// --- Spread ---

#[test]
fn sbu_spread_matches_name() {
    let found = Expression::Spread {
        name: make_id("blk"),
        span: Span::new(0, 6),
    };
    let miss = Expression::Spread {
        name: make_id("other"),
        span: Span::new(0, 8),
    };
    assert_eq!(stored_block_var_uses(&found, "blk"), (true, false));
    assert_eq!(stored_block_var_uses(&miss, "blk"), (false, false));
}

// --- FieldAccess ---

#[test]
fn sbu_field_access_delegates_to_receiver() {
    let expr = parse_first_expr("blk.someField");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
    assert_eq!(stored_block_var_uses(&expr, "other"), (false, false));
}

// --- Block literal ---

#[test]
fn sbu_block_literal_any_ref_is_unsafe() {
    // Even a "safe" value send inside a nested block is flagged as unsafe
    // because the nested block compiles through a different path with no
    // Tier-2 state threading.
    let with_safe_send = parse_first_expr("[blk value]");
    assert_eq!(stored_block_var_uses(&with_safe_send, "blk"), (true, false));
}

#[test]
fn sbu_block_literal_no_ref_is_clean() {
    let expr = parse_first_expr("[x value]");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, false));
}

#[test]
fn sbu_block_literal_ref_in_body_becomes_unsafe_regardless_of_shape() {
    // A bare identifier reference inside a nested block is unsafe.
    let expr = parse_first_expr("[blk]");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

// --- Assignment ---

#[test]
fn sbu_assignment_scans_value_side() {
    let expr = parse_first_expr("x := blk");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_assignment_scans_target_side() {
    // Target is usually an identifier or field access; identifier matches are unsafe.
    let expr = parse_first_expr("blk := 1");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_assignment_unrelated_is_clean() {
    let expr = parse_first_expr("x := 1");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, false));
}

// --- DestructureAssignment / Return ---

#[test]
fn sbu_destructure_assignment_scans_value() {
    let expr = parse_first_expr("{a, b} := blk");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_return_delegates_to_value() {
    let expr = parse_first_expr("^blk");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_return_no_match_is_clean() {
    let expr = parse_first_expr("^x");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, false));
}

// --- Cascade ---

#[test]
fn sbu_cascade_all_safe_value_sends_is_safe() {
    let expr = parse_first_expr("blk value: 1; value: 2");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, true));
}

#[test]
fn sbu_cascade_unary_value_send_is_safe() {
    let expr = parse_first_expr("blk value; value: 1");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, true));
}

#[test]
fn sbu_cascade_non_value_send_makes_it_unsafe() {
    let expr = parse_first_expr("blk foo; value: 1");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_cascade_no_ref_is_clean() {
    let expr = parse_first_expr("x foo; bar");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, false));
}

// --- Parenthesized ---

#[test]
fn sbu_parenthesized_safe_send_is_safe() {
    let expr = parse_first_expr("(blk value)");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, true));
}

#[test]
fn sbu_parenthesized_bare_ref_is_unsafe() {
    let expr = parse_first_expr("(blk)");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

// --- Match ---

#[test]
fn sbu_match_scans_matched_value() {
    let expr = parse_first_expr("blk match: [1 -> 1; _ -> 2]");
    // blk is the matched value: bare identifier → unsafe
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_match_scans_arm_bodies() {
    let expr = parse_first_expr("x match: [1 -> blk value; _ -> 2]");
    // blk referenced in arm body as a safe value send
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, true));
}

#[test]
fn sbu_match_scans_arm_guards() {
    // The parser unwraps `when: [expr]` and stores the inner expression directly
    // as the guard, so `blk value` in the guard is a safe value send.
    let expr = parse_first_expr("x match: [n when: [blk value] -> n; _ -> 0]");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, true));
}

// --- TupleLiteral ---

#[test]
fn sbu_tuple_literal_scans_elements() {
    let with_var = parse_first_expr("{blk, 1}");
    assert_eq!(stored_block_var_uses(&with_var, "blk"), (true, false));

    let without_var = parse_first_expr("{x, 1}");
    assert_eq!(stored_block_var_uses(&without_var, "blk"), (false, false));
}

// --- ListLiteral ---

#[test]
fn sbu_list_literal_scans_elements() {
    let expr = parse_first_expr("#(blk, 1)");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_list_literal_scans_tail() {
    // `#(1 | blk)` — blk is the tail expression
    let expr = parse_first_expr("#(1 | blk)");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_list_literal_no_ref_is_clean() {
    let expr = parse_first_expr("#(1, 2)");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, false));
}

// --- ArrayLiteral ---

#[test]
fn sbu_array_literal_scans_elements() {
    let with_var = parse_first_expr("#[blk, 1]");
    assert_eq!(stored_block_var_uses(&with_var, "blk"), (true, false));

    let without_var = parse_first_expr("#[x, 1]");
    assert_eq!(stored_block_var_uses(&without_var, "blk"), (false, false));
}

// --- MapLiteral ---

#[test]
fn sbu_map_literal_scans_keys() {
    // Map keys must be symbols or complex expressions; use a message send as the key.
    let expr = parse_first_expr("#{blk printString => 1}");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_map_literal_scans_values() {
    let expr = parse_first_expr("#{#key => blk}");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_map_literal_no_ref_is_clean() {
    let expr = parse_first_expr("#{#key => 1}");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, false));
}

// --- StringInterpolation ---

#[test]
fn sbu_string_interpolation_scans_expressions() {
    // blk used as receiver inside the interpolation — bare ref is unsafe
    let expr = parse_first_expr("\"count: {blk printString}\"");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_string_interpolation_safe_send_is_safe() {
    let expr = parse_first_expr("\"val: {blk value}\"");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, true));
}

// --- BinaryConstruction ---

#[test]
fn sbu_binary_construction_scans_segment_values() {
    // `<<blk/binary>>` — blk is the segment value
    let expr = parse_first_expr("<<blk/binary>>");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (true, false));
}

#[test]
fn sbu_binary_construction_no_ref_is_clean() {
    let expr = parse_first_expr("<<16:8>>");
    assert_eq!(stored_block_var_uses(&expr, "blk"), (false, false));
}
