// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pattern binding extraction for semantic analysis.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module extracts variable bindings from match patterns, detecting
//! duplicate variables. Used by both `NameResolver` and the block `Analyser`.

use crate::ast::{Identifier, Pattern};
use crate::source_analysis::Span;
use ecow::EcoString;

/// Extract variable bindings from a pattern (strict mode).
///
/// Recursively traverses the pattern and collects all variable identifiers
/// that will be bound when the pattern matches. Returns diagnostics for
/// duplicate pattern variables.
///
/// All duplicate variable names produce an error diagnostic regardless of
/// pattern kind. Use [`extract_match_arm_bindings`] for match-arm patterns,
/// where `Pattern::Array` additionally supports equality-constraint duplicates.
///
/// # Examples
///
/// ```
/// # use beamtalk_core::semantic_analysis::extract_pattern_bindings;
/// # use beamtalk_core::ast::{Pattern, Identifier};
/// # use beamtalk_core::source_analysis::Span;
/// # use ecow::EcoString;
/// let pattern = Pattern::Variable(Identifier::new("x", Span::default()));
/// let (bindings, diagnostics) = extract_pattern_bindings(&pattern);
/// assert_eq!(bindings.len(), 1);
/// assert_eq!(bindings[0].name, EcoString::from("x"));
/// assert!(diagnostics.is_empty());
/// ```
pub fn extract_pattern_bindings(
    pattern: &Pattern,
) -> (Vec<Identifier>, Vec<crate::source_analysis::Diagnostic>) {
    let mut bindings = Vec::new();
    let mut diagnostics = Vec::new();
    let mut seen = std::collections::HashMap::new();
    extract_pattern_bindings_impl(pattern, &mut bindings, &mut seen, &mut diagnostics, false);
    (bindings, diagnostics)
}

/// Extract variable bindings from a **match-arm** pattern.
///
/// Like [`extract_pattern_bindings`], but `Pattern::Array` arms additionally
/// allow duplicate variable names — the codegen for array match arms emits
/// `erlang:=:=` equality guards for subsequent occurrences. This relaxation
/// is intentionally limited to match arms: destructuring assignments and other
/// pattern contexts use the strict variant.
pub fn extract_match_arm_bindings(
    pattern: &Pattern,
) -> (Vec<Identifier>, Vec<crate::source_analysis::Diagnostic>) {
    let mut bindings = Vec::new();
    let mut diagnostics = Vec::new();
    let mut seen = std::collections::HashMap::new();
    // For match arms, top-level array patterns allow duplicate variables.
    let allow_duplicates = matches!(pattern, Pattern::Array { .. });
    extract_pattern_bindings_impl(
        pattern,
        &mut bindings,
        &mut seen,
        &mut diagnostics,
        allow_duplicates,
    );
    (bindings, diagnostics)
}

/// Records a single identifier binding, emitting a duplicate-variable
/// diagnostic if it collides with an earlier binding in the same pattern
/// (unless `allow_duplicates` is set — see `extract_match_arm_bindings`).
/// Shared by `Pattern::Variable` and `Pattern::Type`'s `binding`.
fn record_binding(
    id: &Identifier,
    bindings: &mut Vec<Identifier>,
    seen: &mut std::collections::HashMap<EcoString, Span>,
    diagnostics: &mut Vec<crate::source_analysis::Diagnostic>,
    allow_duplicates: bool,
) {
    use std::collections::hash_map::Entry;

    match seen.entry(id.name.clone()) {
        Entry::Occupied(entry) => {
            if !allow_duplicates {
                // Duplicate variable - emit diagnostic
                let first_span = *entry.get();
                diagnostics.push(crate::source_analysis::Diagnostic::error(
                    format!(
                        "Variable '{}' is bound multiple times in pattern (first bound at byte offset {})",
                        id.name,
                        first_span.start()
                    ),
                    id.span,
                ).with_hint(format!("Rename one of the `{}` bindings — each variable can only appear once in a pattern", id.name)));
            }
        }
        Entry::Vacant(entry) => {
            entry.insert(id.span);
        }
    }
    bindings.push(id.clone());
}

/// Internal implementation of pattern binding extraction.
///
/// `allow_duplicates` suppresses the duplicate-variable diagnostic. It is set
/// to `true` for `Pattern::Array` arms, where codegen emits `erlang:=:=`
/// equality guards for repeated names.
fn extract_pattern_bindings_impl(
    pattern: &Pattern,
    bindings: &mut Vec<Identifier>,
    seen: &mut std::collections::HashMap<EcoString, Span>,
    diagnostics: &mut Vec<crate::source_analysis::Diagnostic>,
    allow_duplicates: bool,
) {
    match pattern {
        // Variable patterns bind the identifier
        Pattern::Variable(id) => {
            record_binding(id, bindings, seen, diagnostics, allow_duplicates);
        }

        // Type patterns bind `binding` (ADR 0107 Phase A) — registering it
        // here is what makes the name resolvable in the arm body/guard;
        // narrowing its *type* to `class` is a separate concern (bindings/scope,
        // narrowing, and codegen for `Pattern::Type` are handled separately).
        // Without this, `path :: String -> path` would raise a spurious
        // "Undefined variable: path" error even though the pattern is
        // otherwise valid Phase A syntax.
        Pattern::Type { binding, .. } => {
            // `_ :: T` (BT-5294) tests the class but binds nothing.
            if binding.name != "_" {
                record_binding(binding, bindings, seen, diagnostics, allow_duplicates);
            }
        }

        // Tuple patterns: recursively extract; duplicates are not allowed (native Core Erlang patterns)
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                extract_pattern_bindings_impl(
                    element,
                    bindings,
                    seen,
                    diagnostics,
                    allow_duplicates,
                );
            }
        }

        // Array patterns: repeated element names are allowed only in match arms
        // (`allow_duplicates`), where codegen emits equality checks. Destructuring
        // emits no guard, so a repeat would silently rebind (BT-5052).
        Pattern::Array { elements, rest, .. } => {
            for element in elements {
                extract_pattern_bindings_impl(
                    element,
                    bindings,
                    seen,
                    diagnostics,
                    allow_duplicates,
                );
            }
            if let Some(rest_pat) = rest {
                // Elements may repeat (array match arms add an equality guard), but
                // the rest is destructuring-only and gets no guard: a repeated name
                // would silently rebind to the sub-array. Strict mode here makes a
                // rest that repeats a name an error, like a tuple/list duplicate (BT-5050).
                extract_pattern_bindings_impl(rest_pat, bindings, seen, diagnostics, false);
            }
        }

        // List patterns: recursively extract from elements and tail
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                extract_pattern_bindings_impl(
                    element,
                    bindings,
                    seen,
                    diagnostics,
                    allow_duplicates,
                );
            }
            if let Some(tail_pattern) = tail {
                extract_pattern_bindings_impl(
                    tail_pattern,
                    bindings,
                    seen,
                    diagnostics,
                    allow_duplicates,
                );
            }
        }

        // Binary patterns: extract from segment value patterns
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                // Binary segments may have value patterns that bind variables
                extract_pattern_bindings_impl(
                    &segment.value,
                    bindings,
                    seen,
                    diagnostics,
                    allow_duplicates,
                );
            }
        }

        // Map patterns: extract variable bindings from value patterns
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                extract_pattern_bindings_impl(
                    &pair.value,
                    bindings,
                    seen,
                    diagnostics,
                    allow_duplicates,
                );
            }
        }

        // Constructor patterns: extract bindings from each keyword argument
        Pattern::Constructor { keywords, .. } => {
            for (_, binding) in keywords {
                extract_pattern_bindings_impl(
                    binding,
                    bindings,
                    seen,
                    diagnostics,
                    allow_duplicates,
                );
            }
        }

        // Wildcards and literals don't bind variables. `nil` likewise binds
        // nothing (ADR 0107 Phase A).
        Pattern::Wildcard(_) | Pattern::Literal(_, _) | Pattern::Nil(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinarySegment, Literal};
    use crate::semantic_analysis::test_helpers::test_span;

    #[test]
    fn test_extract_pattern_bindings_variable() {
        let pattern = Pattern::Variable(Identifier::new("x", test_span()));
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].name, "x");
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_wildcard() {
        let pattern = Pattern::Wildcard(test_span());
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 0);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_literal() {
        let pattern = Pattern::Literal(Literal::Integer(42), test_span());
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 0);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_tuple() {
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Variable(Identifier::new("x", test_span())),
                Pattern::Variable(Identifier::new("y", test_span())),
            ],
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "x");
        assert_eq!(bindings[1].name, "y");
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_nested_tuple() {
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Variable(Identifier::new("status", test_span())),
                Pattern::Tuple {
                    elements: vec![
                        Pattern::Variable(Identifier::new("x", test_span())),
                        Pattern::Variable(Identifier::new("y", test_span())),
                    ],
                    span: test_span(),
                },
            ],
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 3);
        assert_eq!(bindings[0].name, "status");
        assert_eq!(bindings[1].name, "x");
        assert_eq!(bindings[2].name, "y");
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_list() {
        let pattern = Pattern::List {
            elements: vec![
                Pattern::Variable(Identifier::new("head", test_span())),
                Pattern::Variable(Identifier::new("second", test_span())),
            ],
            tail: Some(Box::new(Pattern::Variable(Identifier::new(
                "tail",
                test_span(),
            )))),
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 3);
        assert_eq!(bindings[0].name, "head");
        assert_eq!(bindings[1].name, "second");
        assert_eq!(bindings[2].name, "tail");
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_list_no_tail() {
        let pattern = Pattern::List {
            elements: vec![
                Pattern::Variable(Identifier::new("a", test_span())),
                Pattern::Variable(Identifier::new("b", test_span())),
            ],
            tail: None,
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "a");
        assert_eq!(bindings[1].name, "b");
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_binary() {
        let pattern = Pattern::Binary {
            segments: vec![
                BinarySegment {
                    value: Pattern::Variable(Identifier::new("version", test_span())),
                    size: None,
                    segment_type: None,
                    signedness: None,
                    endianness: None,
                    unit: None,
                    span: test_span(),
                },
                BinarySegment {
                    value: Pattern::Variable(Identifier::new("data", test_span())),
                    size: None,
                    segment_type: None,
                    signedness: None,
                    endianness: None,
                    unit: None,
                    span: test_span(),
                },
            ],
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "version");
        assert_eq!(bindings[1].name, "data");
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_duplicate_in_tuple() {
        // Pattern {x, x} should error
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Variable(Identifier::new("x", test_span())),
                Pattern::Variable(Identifier::new("x", test_span())),
            ],
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        // Both bindings collected
        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "x");
        assert_eq!(bindings[1].name, "x");

        // Diagnostic emitted for duplicate
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("bound multiple times"));
        assert!(diagnostics[0].message.contains("'x'"));
    }

    #[test]
    fn test_extract_pattern_bindings_duplicate_nested() {
        // Pattern {x, {x, y}} should error on second x
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Variable(Identifier::new("x", test_span())),
                Pattern::Tuple {
                    elements: vec![
                        Pattern::Variable(Identifier::new("x", test_span())),
                        Pattern::Variable(Identifier::new("y", test_span())),
                    ],
                    span: test_span(),
                },
            ],
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 3);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("'x'"));
    }

    #[test]
    fn test_extract_pattern_bindings_duplicate_in_list() {
        // Pattern [x, x | tail] should error
        let pattern = Pattern::List {
            elements: vec![
                Pattern::Variable(Identifier::new("x", test_span())),
                Pattern::Variable(Identifier::new("x", test_span())),
            ],
            tail: Some(Box::new(Pattern::Variable(Identifier::new(
                "tail",
                test_span(),
            )))),
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 3);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("'x'"));
    }

    #[test]
    fn test_extract_pattern_bindings_array_rest_repeating_element_is_error() {
        // Pattern #[a, ...a]: no equality guard for a rest, so `a` would silently rebind (BT-5050)
        let pattern = Pattern::Array {
            elements: vec![Pattern::Variable(Identifier::new("a", test_span()))],
            list_syntax: false,
            rest: Some(Box::new(Pattern::Variable(Identifier::new(
                "a",
                test_span(),
            )))),
            span: test_span(),
        };
        for (_, diagnostics) in [
            extract_pattern_bindings(&pattern),
            extract_match_arm_bindings(&pattern),
        ] {
            assert_eq!(diagnostics.len(), 1, "got: {diagnostics:?}");
            assert!(diagnostics[0].message.contains("'a'"));
        }
    }

    fn var(name: &str) -> Pattern {
        Pattern::Variable(Identifier::new(name, test_span()))
    }

    fn array(elements: Vec<Pattern>) -> Pattern {
        Pattern::Array {
            elements,
            list_syntax: false,
            rest: None,
            span: test_span(),
        }
    }

    #[test]
    fn test_destructure_array_repeated_element_is_error() {
        // `#[a, a] := ...` has no equality guard, so the repeat would silently rebind (BT-5052)
        let (_, diagnostics) = extract_pattern_bindings(&array(vec![var("a"), var("a")]));
        assert_eq!(diagnostics.len(), 1, "got: {diagnostics:?}");
        assert!(
            diagnostics[0]
                .message
                .contains("Variable 'a' is bound multiple times in pattern")
        );
    }

    #[test]
    fn test_destructure_nested_array_repeated_element_is_error() {
        // `#[a, #[b, a]]`: the repeat is in a nested array
        let pattern = array(vec![var("a"), array(vec![var("b"), var("a")])]);
        let (_, diagnostics) = extract_pattern_bindings(&pattern);
        assert_eq!(diagnostics.len(), 1, "got: {diagnostics:?}");
    }

    #[test]
    fn test_destructure_array_distinct_elements_ok() {
        let pattern = array(vec![var("a"), array(vec![var("b"), var("c")])]);
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);
        assert_eq!(bindings.len(), 3);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_match_arm_array_repeated_element_still_allowed() {
        // Match arms keep the equality guard, including in nested arrays
        for pattern in [
            array(vec![var("a"), var("a")]),
            array(vec![var("a"), array(vec![var("b"), var("a")])]),
        ] {
            let (_, diagnostics) = extract_match_arm_bindings(&pattern);
            assert!(diagnostics.is_empty(), "got: {diagnostics:?}");
        }
    }

    #[test]
    fn test_extract_pattern_bindings_array_rest_distinct_name_ok() {
        let pattern = Pattern::Array {
            elements: vec![Pattern::Variable(Identifier::new("a", test_span()))],
            list_syntax: false,
            rest: Some(Box::new(Pattern::Variable(Identifier::new(
                "rest",
                test_span(),
            )))),
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_match_arm_bindings(&pattern);
        assert_eq!(bindings.len(), 2);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_no_duplicate_different_names() {
        // Pattern {x, y} should be fine
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Variable(Identifier::new("x", test_span())),
                Pattern::Variable(Identifier::new("y", test_span())),
            ],
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_extract_pattern_bindings_mixed() {
        // Pattern like: {#ok, [first | _], value}
        let pattern = Pattern::Tuple {
            elements: vec![
                Pattern::Literal(Literal::Symbol("ok".into()), test_span()),
                Pattern::List {
                    elements: vec![Pattern::Variable(Identifier::new("first", test_span()))],
                    tail: Some(Box::new(Pattern::Wildcard(test_span()))),
                    span: test_span(),
                },
                Pattern::Variable(Identifier::new("value", test_span())),
            ],
            span: test_span(),
        };
        let (bindings, diagnostics) = extract_pattern_bindings(&pattern);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[0].name, "first");
        assert_eq!(bindings[1].name, "value");
        assert!(diagnostics.is_empty());
    }
}
