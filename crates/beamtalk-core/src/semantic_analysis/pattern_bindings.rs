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

/// Extract variable bindings from a pattern.
///
/// Recursively traverses the pattern and collects all variable identifiers
/// that will be bound when the pattern matches. Returns diagnostics for
/// duplicate pattern variables.
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
    extract_pattern_bindings_impl(pattern, &mut bindings, &mut seen, &mut diagnostics);
    (bindings, diagnostics)
}

/// Internal implementation of pattern binding extraction.
///
/// Detects duplicate pattern variables and emits diagnostics. Beamtalk follows
/// Rust-style semantics: duplicate variables in patterns are an error.
///
/// # Note
///
/// Erlang allows duplicates as equality constraints ({X, X} means both must be equal),
/// but for MVP we disallow this for simplicity. Can be relaxed in future with codegen
/// for equality checks.
fn extract_pattern_bindings_impl(
    pattern: &Pattern,
    bindings: &mut Vec<Identifier>,
    seen: &mut std::collections::HashMap<EcoString, Span>,
    diagnostics: &mut Vec<crate::source_analysis::Diagnostic>,
) {
    match pattern {
        // Variable patterns bind the identifier
        Pattern::Variable(id) => {
            // Use Entry API to avoid double lookup
            use std::collections::hash_map::Entry;

            match seen.entry(id.name.clone()) {
                Entry::Occupied(entry) => {
                    // Duplicate variable - emit diagnostic
                    let first_span = *entry.get();
                    diagnostics.push(crate::source_analysis::Diagnostic::error(
                        format!(
                            "Variable '{}' is bound multiple times in pattern (first bound at byte offset {})",
                            id.name,
                            first_span.start()
                        ),
                        id.span,
                    ));
                }
                Entry::Vacant(entry) => {
                    entry.insert(id.span);
                }
            }
            bindings.push(id.clone());
        }

        // Tuple patterns: recursively extract from all elements
        Pattern::Tuple { elements, .. } => {
            for element in elements {
                extract_pattern_bindings_impl(element, bindings, seen, diagnostics);
            }
        }

        // List patterns: recursively extract from elements and tail
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                extract_pattern_bindings_impl(element, bindings, seen, diagnostics);
            }
            if let Some(tail_pattern) = tail {
                extract_pattern_bindings_impl(tail_pattern, bindings, seen, diagnostics);
            }
        }

        // Binary patterns: extract from segment value patterns
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                // Binary segments may have value patterns that bind variables
                extract_pattern_bindings_impl(&segment.value, bindings, seen, diagnostics);
            }
        }

        // Wildcards and literals don't bind variables
        Pattern::Wildcard(_) | Pattern::Literal(_, _) => {}
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
