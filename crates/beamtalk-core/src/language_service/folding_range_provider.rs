// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Folding-range provider for `// === Name ===` section dividers (BT-3237)
//! and class/method bodies (BT-3260).
//!
//! **DDD Context:** Language Service
//!
//! Follow-up from BT-2601 (which shipped the shared divider-recognition core,
//! [`source_analysis::method_category::categorize_methods`], plus
//! `textDocument/documentSymbol` nesting only). This module implements
//! `textDocument/foldingRange`, emitting two kinds of range per class in
//! `module`:
//!
//! - One range per named [`source_analysis::MethodCategory`], spanning from
//!   the divider's own banner line through the end of the category's last
//!   method — computed via [`source_analysis::MethodCategory::span`], the
//!   exact same call [`crate::language_service::document_symbols_provider`] uses for
//!   its `DocumentSymbolKind::Category` container span, so folding and
//!   outline always agree. Classes with **no** dividers contribute none of
//!   these — the implicit unnamed leading category (methods before the
//!   first divider, or every method when there are no dividers at all) is
//!   never surfaced as a folding range, only ever as flat/uncategorized
//!   document symbols.
//! - One range per class body (`ClassDefinition::span`) and one per method
//!   body (`MethodDefinition::span`, instance- and class-side), i.e.
//!   indentation-equivalent folding (BT-3260). VS Code stops using its own
//!   built-in indentation-based folding the moment *any*
//!   `FoldingRangeProvider` is registered for a language (there is no
//!   per-region merge/fallback — see the LSP folding spec and
//!   microsoft/vscode#265661), so a class/method-body range is emitted for
//!   **every** class and method regardless of whether that class uses
//!   dividers — otherwise registering this provider at all would silently
//!   regress every divider-less file's fold arrows from per-method down to
//!   nothing. A single-line class or method (body on the same line as its
//!   header, nothing to collapse) is skipped, matching indentation folding's
//!   own behavior of only offering a fold for a multi-line block.
//!
//! # AST-only scope
//!
//! Like `document_symbols_provider`, this is computed purely from the parsed
//! module and source text — there is no runtime-delegation path (see
//! `docs/development/surface-parity.md`'s `nav-symbols` row, which documents
//! the same AST-only scope for BT-2601's outline nesting).

use crate::ast::Module;
use crate::source_analysis::{self, Span};

/// Computes folding ranges for every class in `module`: divider-category
/// ranges (BT-3237) plus class-body and method-body ranges (BT-3260, the
/// indentation-equivalent fold points VS Code's built-in strategy would
/// otherwise have provided).
///
/// Returns an empty vector only for a module with no classes at all — a
/// class with no dividers and no multi-line members still returns nothing,
/// but any class with at least one multi-line class/method body returns at
/// least the class-body range.
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
                push_multiline_range(&mut ranges, span, source);
            }
        }

        // BT-3260: the class body itself, so the outermost fold point
        // (collapse the whole class down to its header line) survives
        // registering this provider, exactly like indentation folding would
        // offer at column 0.
        push_multiline_range(&mut ranges, class.span, source);

        // BT-3260: every method body, instance- and class-side, independent
        // of whether the class uses dividers at all — the per-method fold
        // arrow indentation folding used to provide everywhere, not just
        // inside divider categories.
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            push_multiline_range(&mut ranges, method.span, source);
        }
    }

    ranges
}

/// Pushes `span` onto `ranges` only if it covers more than one source line —
/// a single-line class/method has no body to collapse, so emitting a range
/// for it would be a no-op fold marker (mirrors indentation folding, which
/// never offers a fold for a one-line block) — and only if it isn't already
/// present. A duplicate is normally impossible (a category's merged span
/// always starts at least one line before any single method it contains,
/// thanks to its divider banner line), but a category whose `divider_span`
/// couldn't be located degenerates to exactly its one method's own span —
/// see `MethodCategory::span`'s doc — so this guards against emitting the
/// identical range twice in that defensive-only corner case.
fn push_multiline_range(ranges: &mut Vec<Span>, span: Span, source: &str) {
    if let Some(text) = source.get(span.as_range())
        && text.contains('\n')
        && !ranges.contains(&span)
    {
        ranges.push(span);
    }
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
    fn class_with_no_dividers_and_single_line_members_gets_only_the_class_body_range() {
        // BT-3260: no dividers, and every method fits on its own single
        // line — nothing to fold *inside* the class, but the class body
        // itself (header through last method) is still a multi-line range,
        // so it must be the one range this returns (indentation folding
        // would offer exactly this fold at column 0).
        let source = "Object subclass: Counter\n  foo => 1\n  bar => 2\n";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0], module.classes[0].span);
    }

    #[test]
    fn class_with_no_dividers_and_multiline_methods_gets_class_and_method_ranges() {
        // BT-3260: a divider-less file must still get per-method fold
        // arrows — the regression this issue fixes. Both `foo` and `bar`
        // have multi-line bodies, so each contributes its own range
        // alongside the class-body range.
        let source = "\
Object subclass: Counter
  foo =>
    1

  bar =>
    2
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        let class = &module.classes[0];
        assert_eq!(
            ranges.len(),
            3,
            "expected class + foo + bar, got {ranges:?}"
        );
        assert!(ranges.contains(&class.span));
        assert!(ranges.contains(&class.methods[0].span));
        assert!(ranges.contains(&class.methods[1].span));

        let foo_text = &source[class.methods[0].span.as_range()];
        assert!(foo_text.starts_with("foo =>"));
        assert!(foo_text.trim_end().ends_with('1'));
    }

    #[test]
    fn class_with_dividers_returns_divider_class_and_method_ranges() {
        // BT-3260: a class that uses dividers must get *both* the
        // divider-category ranges (BT-3237) and the class/method-body
        // ranges — the combination this issue calls for, so nesting
        // (fold class -> fold category -> fold method) still works.
        let source = "\
Object subclass: Counter
  // === Alpha ===
  foo =>
    1
  bar => 2

  // === Beta ===
  baz => 3
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        let class = &module.classes[0];

        // Divider-category ranges are pushed first, in divider order.
        let alpha = &source[ranges[0].as_range()];
        assert!(alpha.starts_with("  // === Alpha ==="));
        assert!(alpha.trim_end().ends_with("bar => 2"));
        let beta = &source[ranges[1].as_range()];
        assert!(beta.starts_with("  // === Beta ==="));
        assert!(beta.trim_end().ends_with("baz => 3"));

        // Plus the class body and every multi-line method (`foo`; `bar` and
        // `baz` are single-line and contribute no range of their own).
        assert!(ranges.contains(&class.span));
        assert!(ranges.contains(&class.methods[0].span)); // foo (multi-line)
        assert_eq!(ranges.len(), 4, "alpha, beta, class, foo — got {ranges:?}");
    }

    #[test]
    fn methods_before_the_first_divider_contribute_no_divider_range() {
        // The implicit unnamed leading category (methods before the first
        // divider) is not a folding range — only named categories are. The
        // class-body range (BT-3260) is still present alongside it.
        let source = "\
Object subclass: Counter
  foo => 1

  // === Section ===
  bar => 2
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        assert_eq!(ranges.len(), 2);
        let divider_range = ranges
            .iter()
            .find(|r| source[r.as_range()].starts_with("  // === Section ==="))
            .expect("divider range present");
        assert!(!source[divider_range.as_range()].contains("foo => 1"));
        assert!(ranges.contains(&module.classes[0].span));
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
        let symbols = crate::language_service::document_symbols_provider::compute_document_symbols(
            &module, source,
        );
        let category = symbols[0]
            .children
            .iter()
            .find(|c| c.kind == crate::language_service::DocumentSymbolKind::Category)
            .expect("one category symbol");
        // The category range is pushed first, ahead of the class-body range
        // (`bar`/`baz` are single-line, so neither contributes its own).
        assert_eq!(folding_ranges.len(), 2);
        assert_eq!(folding_ranges[0], category.span);
        assert_eq!(folding_ranges[1], module.classes[0].span);
    }

    #[test]
    fn multiple_classes_each_contribute_their_own_dividers_and_class_bodies() {
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
        // One divider range + one class-body range per class.
        assert_eq!(ranges.len(), 4);
        assert!(ranges.contains(&module.classes[0].span));
        assert!(ranges.contains(&module.classes[1].span));
    }

    #[test]
    fn named_category_with_no_methods_contributes_no_divider_range() {
        // Review finding (BT-3237): a divider can precede any class member
        // (BT-2601), not only a method. A divider directly above a
        // `state:`/`classState:` declaration with no method before the next
        // divider or the end of the class produces a named category whose
        // `methods` list is empty. Without the `methods.is_empty()` guard,
        // `MethodCategory::span()` still resolves for this case (it
        // degenerates to just `divider_span`), which would emit a folding
        // range spanning nothing but the divider's own banner line — a
        // misleading fold marker with no real content to collapse. The
        // class-body range (BT-3260) is still emitted alongside this.
        let source = "\
Object subclass: Counter
  foo => 1

  // === Beta ===
  state: x = 0
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        assert_eq!(
            ranges,
            vec![module.classes[0].span],
            "only the class-body range should survive, got {ranges:?}"
        );
    }

    #[test]
    fn single_line_method_contributes_no_range_of_its_own() {
        // A one-line method (selector + body on the same line) has nothing
        // to collapse — mirrors indentation folding, which never offers a
        // fold for a single-line block.
        let source = "Object subclass: Counter\n  foo => 1\n";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        // The class body ("Object subclass: Counter" through "foo => 1") is
        // still two lines, so it alone survives.
        assert_eq!(ranges, vec![module.classes[0].span]);
        assert!(!ranges.contains(&module.classes[0].methods[0].span));
    }

    #[test]
    fn class_method_bodies_get_ranges_too() {
        let source = "\
Object subclass: Counter
  class new =>
    Counter new
";
        let module = parse_module(source);
        let ranges = compute_folding_ranges(&module, source);
        let class = &module.classes[0];
        assert!(ranges.contains(&class.class_methods[0].span));
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

        assert_eq!(lf_ranges.len(), 2);
        assert_eq!(crlf_ranges.len(), 2);
        let crlf_text = &crlf_source[crlf_ranges[0].as_range()];
        assert!(crlf_text.starts_with("  // === Section ==="));
        assert!(crlf_text.trim_end().ends_with("baz => 3"));
    }
}
