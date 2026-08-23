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
            // Skip the implicit unnamed leading category, and — review
            // finding (BT-3237) — skip a *named* category with zero methods
            // too: a divider can precede any class member (BT-2601), so
            // `// === Beta ===\nstate: x = 0` (divider directly above a
            // state/classState declaration with no method before the next
            // divider or end of class) produces a named category whose
            // `methods` is empty. `MethodCategory::span()` still resolves
            // for that case — it degenerates to just `divider_span`, the
            // banner line plus its trailing newline — which would emit a
            // folding range with nothing but the divider itself as "body"
            // (start_line == the divider's line, end_line == the next
            // line), a misleading fold marker with no real content to
            // collapse. Requiring a non-empty `methods` list keeps this
            // aligned with what a category container actually represents.
            if category.name.is_none() || category.methods.is_empty() {
                continue;
            }
            // A non-empty named category always has a resolvable span (its
            // own methods' spans alone are enough to merge, even if
            // `divider_span` couldn't be re-found in `source`). Defensively
            // skip rather than emit a bogus range if it doesn't.
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

    #[test]
    fn named_category_with_no_methods_contributes_no_range() {
        // Review finding (BT-3237): a divider can precede any class member
        // (BT-2601), not only a method. A divider directly above a
        // `state:`/`classState:` declaration with no method before the next
        // divider or the end of the class produces a named category whose
        // `methods` list is empty. Without the `methods.is_empty()` guard,
        // `MethodCategory::span()` still resolves for this case (it
        // degenerates to just `divider_span`), which would emit a folding
        // range spanning nothing but the divider's own banner line — a
        // misleading fold marker with no real content to collapse.
        let source = "\
Object subclass: Counter
  foo => 1

  // === Beta ===
  state: x = 0
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        assert!(
            ranges.is_empty(),
            "an empty-methods named category must not become a folding range, got {ranges:?}"
        );
    }

    #[test]
    fn crlf_line_endings_produce_the_same_ranges_as_lf() {
        let lf_source = "\
Object subclass: Counter
  // === Section ===
  bar => 2
  baz => 3
";
        let crlf_source = lf_source.replace('\n', "\r\n");
        let lf_module = parse_module(lf_source);
        let crlf_module = parse_module(&crlf_source);

        let lf_ranges = compute_folding_ranges(&lf_module, lf_source);
        let crlf_ranges = compute_folding_ranges(&crlf_module, &crlf_source);

        assert_eq!(lf_ranges.len(), 1);
        assert_eq!(crlf_ranges.len(), 1);
        let crlf_text = &crlf_source[crlf_ranges[0].as_range()];
        assert!(crlf_text.starts_with("  // === Section ==="));
        assert!(crlf_text.trim_end().ends_with("baz => 3"));
    }
}
