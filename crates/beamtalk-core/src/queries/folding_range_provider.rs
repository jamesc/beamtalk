// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Folding-range provider for `// === Name ===` section dividers (BT-3237).
//!
//! **DDD Context:** Language Service
//!
//! Follow-up from BT-2601 (which shipped the shared divider-recognition core,
//! [`source_analysis::method_category::categorize_methods`], plus
//! `textDocument/documentSymbol` nesting only). This module implements
//! `textDocument/foldingRange`: one folding range per named
//! [`source_analysis::MethodCategory`] in a class, spanning from the
//! divider's own banner line through the end of the category's last method —
//! computed via [`source_analysis::MethodCategory::span`], the exact same
//! call [`crate::queries::document_symbols_provider`] uses for its
//! `DocumentSymbolKind::Category` container span, so folding and outline
//! always agree.
//!
//! Classes with **no** dividers contribute no ranges — the implicit unnamed
//! leading category (methods before the first divider, or every method when
//! there are no dividers at all) is never surfaced as a folding range, only
//! ever as flat/uncategorized document symbols.
//!
//! # AST-only scope
//!
//! Like `document_symbols_provider`, this is computed purely from the parsed
//! module and source text — there is no runtime-delegation path (see
//! `docs/development/surface-parity.md`'s `nav-symbols` row, which documents
//! the same AST-only scope for BT-2601's outline nesting).

use crate::ast::Module;
use crate::source_analysis::{self, Span};

/// Computes one folding-range span per named `// === Name ===` divider
/// category, across every class in `module`.
///
/// Returns an empty vector for a module with no dividers anywhere (or no
/// classes at all).
#[must_use]
pub fn compute_folding_ranges(module: &Module, source: &str) -> Vec<Span> {
    let mut ranges = Vec::new();

    for class in &module.classes {
        for category in source_analysis::categorize_methods(class, source) {
            if category.name.is_none() {
                continue;
            }
            // A named category always has at least one method (it is only
            // created while processing one that starts a new category), so
            // `span()` should always resolve here. Defensively skip rather
            // than emit a bogus range if it doesn't (e.g. a stale AST that
            // no longer matches `source`).
            if let Some(span) = category.span() {
                ranges.push(span);
            }
        }
    }

    ranges
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    fn parse_module(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        let (module, diags) = parse(tokens);
        assert!(diags.is_empty(), "fixture should parse cleanly: {diags:?}");
        module
    }

    #[test]
    fn class_with_no_dividers_returns_no_ranges() {
        let source = "Object subclass: Counter\n  foo => 1\n  bar => 2\n";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        assert!(ranges.is_empty());
    }

    #[test]
    fn class_with_dividers_returns_one_range_per_divider() {
        let source = "\
Object subclass: Counter
  // === Alpha ===
  foo => 1
  bar => 2

  // === Beta ===
  baz => 3
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        assert_eq!(ranges.len(), 2);

        let alpha = &source[ranges[0].as_range()];
        assert!(alpha.starts_with("  // === Alpha ==="));
        assert!(alpha.trim_end().ends_with("bar => 2"));

        let beta = &source[ranges[1].as_range()];
        assert!(beta.starts_with("  // === Beta ==="));
        assert!(beta.trim_end().ends_with("baz => 3"));
    }

    #[test]
    fn methods_before_the_first_divider_contribute_no_range() {
        // The implicit unnamed leading category (methods before the first
        // divider) is not a folding range — only named categories are.
        let source = "\
Object subclass: Counter
  foo => 1

  // === Section ===
  bar => 2
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        assert_eq!(ranges.len(), 1);
        let text = &source[ranges[0].as_range()];
        assert!(text.starts_with("  // === Section ==="));
        assert!(!text.contains("foo => 1"));
    }

    #[test]
    fn range_matches_the_document_symbol_category_container_span() {
        // Folding and outline must agree exactly — both call
        // `MethodCategory::span`.
        let source = "\
Object subclass: Counter
  // === Section ===
  bar => 2
  baz => 3
";
        let module = parse_module(source);
        let folding_ranges = compute_folding_ranges(&module, source);
        let symbols =
            crate::queries::document_symbols_provider::compute_document_symbols(&module, source);
        let category = symbols[0]
            .children
            .iter()
            .find(|c| c.kind == crate::language_service::DocumentSymbolKind::Category)
            .expect("one category symbol");
        assert_eq!(folding_ranges.len(), 1);
        assert_eq!(folding_ranges[0], category.span);
    }

    #[test]
    fn multiple_classes_each_contribute_their_own_dividers() {
        let source = "\
Object subclass: A
  // === Section ===
  foo => 1

Object subclass: B
  // === Other ===
  bar => 2
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        assert_eq!(ranges.len(), 2);
    }
}
