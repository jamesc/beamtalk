// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for match-arm patterns and `match:` parsing (moved out of
//! `expressions.rs`; the guard-bypass tests stay beside the private methods
//! they call).

use super::*;
use crate::ast::Pattern;

// ========================================================================
// Bare-word map key error tests
// ========================================================================

fn parse_source(src: &str) -> (crate::ast::Module, Vec<crate::source_analysis::Diagnostic>) {
    use crate::source_analysis::{lex_with_eof, parse};
    parse(lex_with_eof(src))
}

#[test]
fn bare_word_map_key_produces_error() {
    use crate::source_analysis::Severity;
    let (_module, diags) = parse_source("#{foo => 1}");
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
    assert!(
        errors[0].message.contains("bare word 'foo'"),
        "message: {}",
        errors[0].message
    );
    assert!(
        errors[0].message.contains("'#foo'"),
        "message should suggest #foo: {}",
        errors[0].message
    );
}

#[test]
fn explicit_symbol_map_key_no_error() {
    use crate::source_analysis::Severity;
    let (_module, diags) = parse_source("#{#foo => 1}");
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "explicit #foo key should produce no errors, got: {errors:?}"
    );
}

#[test]
fn parenthesized_dynamic_map_key_no_error() {
    use crate::source_analysis::Severity;
    // `#{(k) => 1}` is the escape hatch for variable keys — must not error
    let (_module, diags) = parse_source("#{(k) => 1}");
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "parenthesized dynamic key should produce no errors, got: {errors:?}"
    );
}

#[test]
fn bare_word_map_pattern_key_produces_error() {
    use crate::source_analysis::Severity;
    let (_module, diags) =
        parse_source("Object subclass: Foo\n  m: x => x match: [#{foo => v} -> v]");
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
    assert!(
        errors[0].message.contains("bare word 'foo'"),
        "message: {}",
        errors[0].message
    );
}

// ── ADR 0106: `matchExhaustive:` parsing ─────────────────────

#[test]
fn plain_match_sets_exhaustive_false() {
    let (module, diags) = parse_source("x match: [#a -> 1; _ -> 0]");
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    let expr = &module.expressions[0].expression;
    match expr {
        crate::ast::Expression::Match { exhaustive, .. } => {
            assert!(!exhaustive, "plain `match:` must set exhaustive: false");
        }
        other => panic!("expected Expression::Match, got: {other:?}"),
    }
}

#[test]
fn match_exhaustive_keyword_sets_exhaustive_true() {
    let (module, diags) = parse_source("x matchExhaustive: [#a -> 1; #b -> 0]");
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    let expr = &module.expressions[0].expression;
    match expr {
        crate::ast::Expression::Match {
            exhaustive, arms, ..
        } => {
            assert!(exhaustive, "`matchExhaustive:` must set exhaustive: true");
            assert_eq!(arms.len(), 2);
        }
        other => panic!("expected Expression::Match, got: {other:?}"),
    }
}

#[test]
fn match_exhaustive_parses_same_arm_syntax_as_match() {
    // Guards, wildcards, and destructuring all parse identically under
    // `matchExhaustive:` — only the keyword and the resulting flag differ.
    let (module, diags) =
        parse_source("x matchExhaustive: [#a when: [true] -> 1; #b -> 2; _ -> 0]");
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    let expr = &module.expressions[0].expression;
    match expr {
        crate::ast::Expression::Match { arms, .. } => {
            assert_eq!(arms.len(), 3);
        }
        other => panic!("expected Expression::Match, got: {other:?}"),
    }
}

// `match_exhaustive_unparse_round_trips_keyword` and
// `plain_match_unparse_does_not_gain_exhaustive_keyword` moved to
// `crate::unparse`'s own test tree (ADR 0117 Phase 4) — both
// exercise `unparse::format_source`, which `source_analysis`'s test tree
// no longer references.

// ── ADR 0107 Phase A: `Pattern::Nil` and `Pattern::Type` ──────

#[test]
fn parse_nil_pattern() {
    let (module, diags) = parse_source("x match: [nil -> 0; _ -> 1]");
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    assert!(
        matches!(arms[0].pattern, Pattern::Nil(_)),
        "expected Pattern::Nil, got: {:?}",
        arms[0].pattern
    );
}

#[test]
fn parse_type_pattern() {
    let (module, diags) = parse_source("x match: [path :: String -> path; _ -> 1]");
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    match &arms[0].pattern {
        Pattern::Type { binding, class, .. } => {
            assert_eq!(binding.name, "path");
            assert_eq!(class.name, "String");
        }
        other => panic!("expected Pattern::Type, got: {other:?}"),
    }
}

#[test]
fn parse_type_pattern_missing_class_name_recovers() {
    let (_module, diags) = parse_source("x match: [path :: -> path; _ -> 1]");
    assert!(
        diags.iter().any(|d| d.message.contains("class name")),
        "expected a diagnostic about the missing class name, got: {diags:?}"
    );
}

#[test]
fn parse_nil_variable_and_type_patterns_together() {
    // Combining all three pattern kinds in one match: (ADR 0107 example shape).
    let (module, diags) = parse_source("x match: [nil -> 0; path :: String -> 1; other -> 2]");
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    assert!(matches!(arms[0].pattern, Pattern::Nil(_)));
    assert!(matches!(arms[1].pattern, Pattern::Type { .. }));
    assert!(matches!(arms[2].pattern, Pattern::Variable(_)));
}

// ── Bare `true`/`false` pattern rejection ──────────────────────

#[test]
fn parse_bare_true_pattern_rejected_with_single_diagnostic() {
    let (_module, diags) = parse_source("x match: [true -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'true'") && diags[0].message.contains("x :: True"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_bare_false_pattern_rejected_with_single_diagnostic() {
    let (_module, diags) = parse_source("x match: [false -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'false'") && diags[0].message.contains("x :: False"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_bare_true_false_pattern_does_not_bind_as_variable() {
    let (module, diags) = parse_source("x match: [true -> 0; false -> 1]");
    assert_eq!(diags.len(), 2, "expected one diagnostic per arm: {diags:?}");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    assert!(
        matches!(arms[0].pattern, Pattern::Wildcard(_)),
        "expected bare 'true' to recover as Pattern::Wildcard, not Pattern::Variable, got: {:?}",
        arms[0].pattern
    );
    assert!(
        matches!(arms[1].pattern, Pattern::Wildcard(_)),
        "expected bare 'false' to recover as Pattern::Wildcard, not Pattern::Variable, got: {:?}",
        arms[1].pattern
    );
}

#[test]
fn parse_true_false_type_pattern_still_works() {
    // `x :: True` / `x :: False` (ADR 0107 Phase A `Pattern::Type`) is
    // the correct, already-working idiom this diagnostic points users
    // at — confirm it's unaffected by the bare-identifier rejection.
    let (module, diags) = parse_source("x match: [b :: True -> 0; b :: False -> 1; _ -> 2]");
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    match &arms[0].pattern {
        Pattern::Type { class, .. } => assert_eq!(class.name, "True"),
        other => panic!("expected Pattern::Type, got: {other:?}"),
    }
    match &arms[1].pattern {
        Pattern::Type { class, .. } => assert_eq!(class.name, "False"),
        other => panic!("expected Pattern::Type, got: {other:?}"),
    }
}

#[test]
fn parse_bare_true_pattern_nested_in_tuple_rejected() {
    // parse_pattern recurses into tuple/array/map elements, so the
    // bare true/false rejection must apply there too, not just at the
    // top level of a match arm.
    let (_module, diags) = parse_source("x match: [{true, y} -> y; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'true'"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_bare_true_with_type_annotation_rejected_with_single_diagnostic() {
    // A plausible typo before discovering the `b :: Boolean` idiom —
    // the trailing `:: ClassName` must be consumed along with `true`,
    // not left dangling to cascade into a second "Expected '->'" error
    // (mirroring the `nil :: ClassName` case above).
    let (_module, diags) = parse_source("x match: [true :: Boolean -> 1; _ -> 0]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'true'"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_bare_false_with_type_annotation_rejected_with_single_diagnostic() {
    // Symmetric coverage of `parse_bare_true_with_type_annotation_rejected_with_single_diagnostic`
    // for the `false` arm of the branch.
    let (_module, diags) = parse_source("x match: [false :: Boolean -> 1; _ -> 0]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'false'"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

// ── Constructor pattern keyword bindings — nil/true/false ──────

#[test]
fn parse_constructor_binding_nil_is_nil_pattern() {
    let (module, diags) = parse_source("x match: [Result ok: nil -> 0; _ -> 1]");
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Constructor { keywords, .. } = &arms[0].pattern else {
        panic!("expected Pattern::Constructor, got: {:?}", arms[0].pattern);
    };
    assert!(
        matches!(keywords[0].1, Pattern::Nil(_)),
        "expected 'nil' binding to parse as Pattern::Nil, not Pattern::Variable, got: {:?}",
        keywords[0].1
    );
}

#[test]
fn parse_constructor_binding_nil_type_annotation_rejected_with_single_diagnostic() {
    // The `nil` arm has the
    // same "trailing `:: ClassName` cascades" bug as the `true`/`false`
    // and generic-identifier arms — `nil :: SomeClass` must consume the
    // tail rather than leaving it to cascade into a second diagnostic.
    let (_module, diags) = parse_source("x match: [Result ok: nil :: SomeClass -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0]
            .message
            .contains("a 'nil' pattern cannot have a type annotation"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_constructor_binding_nil_bare_double_colon_rejected_with_single_diagnostic() {
    // Edge case: `consume_type_pattern_tail` returns `None` when `::`
    // isn't followed by a class name, so the recovered span must fall
    // back to the `::` token itself rather than panicking or leaving
    // `->` unconsumed.
    let src = "x match: [Result ok: nil :: -> 0; _ -> 1]";
    let (_module, diags) = parse_source(src);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0]
            .message
            .contains("a 'nil' pattern cannot have a type annotation"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
    let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
    assert!(
        diags[0].span.end() >= double_colon_end,
        "expected diagnostic span to cover '::' (end >= {double_colon_end}), got: {:?}",
        diags[0].span
    );
}

#[test]
fn parse_constructor_binding_bare_true_bare_double_colon_rejected_with_single_diagnostic() {
    // Same bare-`::` edge case as the `nil` arm above, for the
    // `true`/`false` arm.
    let src = "x match: [Result ok: true :: -> 0; _ -> 1]";
    let (_module, diags) = parse_source(src);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'true'"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
    let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
    assert!(
        diags[0].span.end() >= double_colon_end,
        "expected diagnostic span to cover '::' (end >= {double_colon_end}), got: {:?}",
        diags[0].span
    );
}

#[test]
fn parse_constructor_binding_type_annotation_bare_double_colon_rejected_with_single_diagnostic() {
    // Same bare-`::` edge case as the `nil` arm above, for the generic
    // variable-binding arm.
    let src = "r match: [Result ok: x :: -> x; Result error: _ -> 0]";
    let (_module, diags) = parse_source(src);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains(
            "type annotations are not supported in a constructor pattern binding position"
        ),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
    let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
    assert!(
        diags[0].span.end() >= double_colon_end,
        "expected diagnostic span to cover '::' (end >= {double_colon_end}), got: {:?}",
        diags[0].span
    );
}

#[test]
fn parse_constructor_binding_bare_true_rejected_with_single_diagnostic() {
    let (_module, diags) = parse_source("x match: [Result ok: true -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'true'") && diags[0].message.contains("when:"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_constructor_binding_bare_false_rejected_with_single_diagnostic() {
    let (_module, diags) = parse_source("x match: [Result ok: false -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'false'") && diags[0].message.contains("when:"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_constructor_binding_bare_true_false_recovers_as_wildcard() {
    let (module, diags) =
        parse_source("x match: [Result ok: true -> 0; Result error: false -> 1; _ -> 2]");
    assert_eq!(diags.len(), 2, "expected one diagnostic per arm: {diags:?}");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Constructor {
        keywords: ok_kw, ..
    } = &arms[0].pattern
    else {
        panic!("expected Pattern::Constructor, got: {:?}", arms[0].pattern);
    };
    assert!(
        matches!(ok_kw[0].1, Pattern::Wildcard(_)),
        "expected bare 'true' binding to recover as Pattern::Wildcard, got: {:?}",
        ok_kw[0].1
    );
    let Pattern::Constructor {
        keywords: error_kw, ..
    } = &arms[1].pattern
    else {
        panic!("expected Pattern::Constructor, got: {:?}", arms[1].pattern);
    };
    assert!(
        matches!(error_kw[0].1, Pattern::Wildcard(_)),
        "expected bare 'false' binding to recover as Pattern::Wildcard, got: {:?}",
        error_kw[0].1
    );
}

// ── Constructor binding `x :: ClassName` tail ──────────────────

#[test]
fn parse_constructor_binding_type_annotation_rejected_with_single_diagnostic() {
    let (module, diags) =
        parse_source("r match: [Result ok: x :: Integer -> x; Result error: _ -> 0]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains(
            "type annotations are not supported in a constructor pattern binding position"
        ),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Constructor {
        keywords: ok_kw, ..
    } = &arms[0].pattern
    else {
        panic!("expected Pattern::Constructor, got: {:?}", arms[0].pattern);
    };
    assert!(
        matches!(ok_kw[0].1, Pattern::Wildcard(_)),
        "expected 'x :: Integer' binding to recover as Pattern::Wildcard, got: {:?}",
        ok_kw[0].1
    );
}

#[test]
fn parse_constructor_binding_bare_true_type_annotation_rejected_with_single_diagnostic() {
    // The `true`/`false` arm
    // has the same "trailing `:: ClassName` cascades" bug as the plain
    // identifier arm above — `true :: Boolean` must consume the tail
    // rather than leaving it to cascade into a second diagnostic.
    let (_module, diags) = parse_source("x match: [Result ok: true :: Boolean -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'true'"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_constructor_binding_bare_false_type_annotation_rejected_with_single_diagnostic() {
    // Symmetric coverage for the `false` path through the same branch as
    // `parse_constructor_binding_bare_true_type_annotation_rejected_with_single_diagnostic`.
    let (_module, diags) = parse_source("x match: [Result ok: false :: Boolean -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("bare 'false'"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

// ── Pattern::Type parser polish ────────────────────────────────

#[test]
fn parse_type_pattern_generic_args_rejected_with_single_diagnostic() {
    let (_module, diags) = parse_source("x match: [items :: List(Printable) -> items; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0]
            .message
            .contains("generic type arguments are not supported in a type pattern yet"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_type_pattern_generic_args_multi_param_rejected() {
    // Multi-parameter and nested generics should still be consumed as a
    // single balanced group rather than desyncing the parser.
    let (_module, diags) =
        parse_source("x match: [items :: Result(Integer, List(String)) -> items; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0]
            .message
            .contains("generic type arguments are not supported in a type pattern yet"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_nil_pattern_with_type_annotation_rejected_with_single_diagnostic() {
    let (_module, diags) = parse_source("x match: [nil :: String -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("'nil' pattern"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_nil_pattern_double_colon_with_no_consumable_tail_span_covers_double_colon() {
    // When nothing follows `::` that `consume_type_pattern_tail` can
    // consume (e.g. the arrow comes right after), the diagnostic span
    // must still cover the `::` token itself, not just `nil`.
    let src = "x match: [nil :: -> 0; _ -> 1]";
    let (_module, diags) = parse_source(src);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
    assert!(
        diags[0].span.end() >= double_colon_end,
        "expected diagnostic span to cover '::' (end >= {double_colon_end}), got span {:?}",
        diags[0].span
    );
}

#[test]
fn parse_bare_true_double_colon_with_no_consumable_tail_span_covers_double_colon() {
    let src = "x match: [true :: -> 0; _ -> 1]";
    let (_module, diags) = parse_source(src);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
    assert!(
        diags[0].span.end() >= double_colon_end,
        "expected diagnostic span to cover '::' (end >= {double_colon_end}), got span {:?}",
        diags[0].span
    );
}

#[test]
fn parse_type_pattern_non_identifier_class_name_rejected_with_single_diagnostic() {
    let (_module, diags) = parse_source("x match: [y :: 5 -> y; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("class name"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_type_pattern_missing_class_name_nested_in_tuple_does_not_corrupt_recovery() {
    // Regression: a malformed type pattern's error-recovery must
    // not consume a token that the *enclosing* pattern parser needs —
    // specifically, when a `::` with no class name sits directly before
    // the `,` element separator of an enclosing tuple pattern, that `,`
    // must be left alone so the tuple (and the rest of the match arm)
    // still parses. A blacklist-based recovery (a few "boundary" tokens —
    // arrow, `]`, `;`, `when:` — with everything else consumed) would
    // eat the `,` here and cascade into 7 diagnostics instead of 1.
    let (module, diags) = parse_source("x match: [{y ::, z} -> y; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("class name"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
    // The tuple, comma, and second element must still have parsed.
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    match &arms[0].pattern {
        Pattern::Tuple { elements, .. } => {
            assert_eq!(elements.len(), 2, "expected both tuple elements to parse");
            assert!(matches!(elements[1], Pattern::Variable(_)));
        }
        other => panic!("expected Pattern::Tuple, got: {other:?}"),
    }
}

#[test]
fn parse_nil_pattern_with_generic_type_annotation_rejected_with_single_diagnostic() {
    // `nil :: List(Printable)` combines both malformed shapes (a type
    // annotation on `nil`, and one with generic args) — must still
    // collapse to exactly one diagnostic, exercising the
    // `consume_type_pattern_tail` -> `skip_parenthesized_for_recovery`
    // path (previously untested).
    let (_module, diags) = parse_source("x match: [nil :: List(Printable) -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("'nil' pattern"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_nil_pattern_with_literal_type_annotation_rejected_with_single_diagnostic() {
    // Exercises `consume_type_pattern_tail`'s literal-whitelist branch
    // (previously untested) — `nil :: 5` rather than `nil :: String`.
    let (_module, diags) = parse_source("x match: [nil :: 5 -> 0; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    assert!(
        diags[0].message.contains("'nil' pattern"),
        "unexpected diagnostic message: {:?}",
        diags[0].message
    );
}

#[test]
fn parse_type_pattern_literal_class_name_in_tuple_consumes_literal_not_comma() {
    // Positive counterpart to
    // `parse_type_pattern_missing_class_name_nested_in_tuple_does_not_corrupt_recovery`:
    // proves the literal (`5`) *is* consumed by the whitelist while the
    // following `,` is correctly left for the tuple parser.
    let (module, diags) = parse_source("x match: [{y :: 5, z} -> y; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    match &arms[0].pattern {
        Pattern::Tuple { elements, .. } => {
            assert_eq!(elements.len(), 2, "expected both tuple elements to parse");
            assert!(matches!(elements[1], Pattern::Variable(_)));
        }
        other => panic!("expected Pattern::Tuple, got: {other:?}"),
    }
}

#[test]
fn parse_type_pattern_missing_class_name_nested_in_array_does_not_corrupt_recovery() {
    // Array-pattern counterpart of the tuple-nesting regression test —
    // the array path was previously unexercised.
    let (module, diags) = parse_source("x match: [#[y ::, z] -> y; _ -> 1]");
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic (no cascade), got: {diags:?}"
    );
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    match &arms[0].pattern {
        Pattern::Array { elements, .. } => {
            assert_eq!(elements.len(), 2, "expected both array elements to parse");
            assert!(matches!(elements[1], Pattern::Variable(_)));
        }
        other => panic!("expected Pattern::Array, got: {other:?}"),
    }
}

#[test]
fn parse_type_pattern_unterminated_generic_args_does_not_swallow_rest_of_match() {
    // Regression: a single missing `)` in a
    // generic-args group must not consume every remaining match arm as
    // "part of the generic args" — `skip_parenthesized_for_recovery`
    // stops at `;`/`]` even with parens still unbalanced, since a real
    // generic-args group can never legitimately contain either.
    let source = "x match: [a :: List(Foo -> 1; b -> 2; _ -> 3]";
    let (_module, diags) = parse_source(source);
    let generics_diag = diags
        .iter()
        .find(|d| {
            d.message
                .contains("generic type arguments are not supported in a type pattern yet")
        })
        .unwrap_or_else(|| panic!("expected the generic-args diagnostic, got: {diags:?}"));
    // The diagnostic's span must be bounded well short of EOF — proving
    // `skip_parenthesized_for_recovery` stopped at the `;` instead of
    // treating the rest of the file (including the `b -> 2` and
    // `_ -> 3` arms) as "part of the generic args" under one span.
    assert!(
        usize::try_from(generics_diag.span.end()).unwrap() < source.len() - 10,
        "diagnostic span {:?} swallowed too much of the input (len {}), got: {diags:?}",
        generics_diag.span,
        source.len()
    );
}
