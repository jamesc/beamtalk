// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Document symbols provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `DocumentSymbolProvider` from the DDD model.
//! It returns an outline of symbols (classes, methods, fields) for a module,
//! following LSP document symbol conventions.
//!
//! Per ADR 0013, class definitions use `(class)` kind suffix in their display name.
//!
//! # Method categories (BT-2601)
//!
//! When a class's source contains at least one `// === Name ===` section
//! divider, its methods are nested one level deeper under a
//! [`DocumentSymbolKind::Category`] container per divider (Outline nesting +
//! breadcrumbs + sticky-scroll fall out of this for free in VS Code) — see
//! [`beamtalk_core::source_analysis::method_category`] for the divider-recognition
//! and method-association rules. Classes with **no** dividers are unaffected:
//! their methods stay flat children of the class symbol, instance methods
//! before class-side methods, exactly as before this feature — a divider-free
//! class's outline never changes shape.

use crate::{DocumentSymbol, DocumentSymbolKind};
use beamtalk_core::ast::Module;
use beamtalk_core::source_analysis::{self, CategorizedMethod, MethodSide};

/// Computes document symbols for a module.
///
/// Returns a hierarchical list of symbols: classes contain methods and
/// fields (and, when the class's source uses section dividers, a further
/// level of category containers — see the module docs).
#[must_use]
pub fn compute_document_symbols(module: &Module, source: &str) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    for class in &module.classes {
        let mut children = Vec::new();

        // Add state variables as field symbols
        for state_var in &class.state {
            children.push(DocumentSymbol {
                name: state_var.name.name.clone(),
                kind: DocumentSymbolKind::Field,
                span: state_var.span,
                name_span: None,
                children: vec![],
            });
        }

        let categories = source_analysis::categorize_methods(class, source);
        let has_dividers = categories.iter().any(|c| c.name.is_some());

        if has_dividers {
            for category in categories {
                // The container's range is the divider banner line merged
                // with every method in the category — computed by
                // `MethodCategory::span` so it stays identical to the span
                // `folding_range_provider` emits for the same category
                // (BT-3237). Computed before the match below since matching
                // `category.name` by value partially moves `category`,
                // which would leave `category.span()` unable to borrow the
                // whole struct.
                let span = category.span().unwrap_or(class.span);
                match category.name {
                    Some(name) => {
                        let method_children: Vec<DocumentSymbol> =
                            category.methods.iter().map(method_symbol).collect();
                        children.push(DocumentSymbol {
                            name: name.into(),
                            kind: DocumentSymbolKind::Category,
                            span,
                            name_span: category.divider_span,
                            children: method_children,
                        });
                    }
                    None => {
                        children.extend(category.methods.iter().map(method_symbol));
                    }
                }
            }
        } else {
            // No dividers: preserve the pre-BT-2601 flat shape exactly
            // (instance methods, then class-side methods).
            for method in &class.methods {
                children.push(DocumentSymbol {
                    name: method.selector.name(),
                    kind: DocumentSymbolKind::Method,
                    span: method.span,
                    name_span: None,
                    children: vec![],
                });
            }
            for method in &class.class_methods {
                children.push(DocumentSymbol {
                    name: method.selector.name(),
                    kind: DocumentSymbolKind::ClassMethod,
                    span: method.span,
                    name_span: None,
                    children: vec![],
                });
            }
        }

        // ADR 0013: class definitions use `(class)` suffix
        symbols.push(DocumentSymbol {
            name: format!("{} (class)", class.name.name).into(),
            kind: DocumentSymbolKind::Class,
            span: class.span,
            name_span: Some(class.name.span),
            children,
        });
    }

    symbols
}

/// Converts one [`CategorizedMethod`] to a leaf [`DocumentSymbol`] (`Method`
/// or `ClassMethod`, matching the pre-BT-2601 flat-shape mapping).
fn method_symbol(method: &CategorizedMethod) -> DocumentSymbol {
    DocumentSymbol {
        name: method.selector.as_str().into(),
        kind: match method.side {
            MethodSide::Instance => DocumentSymbolKind::Method,
            MethodSide::Class => DocumentSymbolKind::ClassMethod,
        },
        span: method.span,
        name_span: None,
        children: vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::source_analysis::{lex_with_eof, parse};

    #[test]
    fn empty_module_returns_no_symbols() {
        let tokens = lex_with_eof("");
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module, "");
        assert!(symbols.is_empty());
    }

    #[test]
    fn class_definition_returns_class_symbol() {
        let source = "Object subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module, source);

        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name.as_str(), "Counter (class)");
        assert_eq!(symbols[0].kind, DocumentSymbolKind::Class);
    }

    #[test]
    fn class_children_include_state_and_methods() {
        let source = "Object subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n  value => self.count";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module, source);

        assert_eq!(symbols.len(), 1);
        let children = &symbols[0].children;

        // Should have state var + 2 methods = 3 children
        assert_eq!(children.len(), 3);

        // First child: state variable
        assert_eq!(children[0].name.as_str(), "count");
        assert_eq!(children[0].kind, DocumentSymbolKind::Field);

        // Second child: increment method
        assert_eq!(children[1].name.as_str(), "increment");
        assert_eq!(children[1].kind, DocumentSymbolKind::Method);

        // Third child: value method
        assert_eq!(children[2].name.as_str(), "value");
        assert_eq!(children[2].kind, DocumentSymbolKind::Method);
    }

    #[test]
    fn class_methods_have_class_method_kind() {
        let source = "Actor subclass: Counter\n  state: count = 0\n\n  class withInitial: n => self new: #{count => n}\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module, source);

        assert_eq!(symbols.len(), 1);
        let children = &symbols[0].children;

        // Find the class method
        let class_methods: Vec<_> = children
            .iter()
            .filter(|c| c.kind == DocumentSymbolKind::ClassMethod)
            .collect();
        assert_eq!(class_methods.len(), 1);
        assert_eq!(class_methods[0].name.as_str(), "withInitial:");
    }

    #[test]
    fn no_dividers_keeps_flat_pre_bt_2601_shape() {
        // A class with no `// === Name ===` divider must produce the exact
        // same flat children list as before this feature — zero behavior
        // change for the overwhelming majority of classes.
        let source = "Object subclass: Counter\n  foo => 1\n  bar => 2";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module, source);
        let children = &symbols[0].children;
        assert_eq!(children.len(), 2);
        assert!(
            children
                .iter()
                .all(|c| c.kind == DocumentSymbolKind::Method)
        );
    }

    #[test]
    fn divider_nests_methods_under_a_category_symbol() {
        let source = "\
Object subclass: Counter
  foo => 1

  // === Section ===

  bar => 2
  baz => 3
";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module, source);
        let children = &symbols[0].children;

        // Uncategorized `foo`, then one `Category` container for `Section`.
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].name.as_str(), "foo");
        assert_eq!(children[0].kind, DocumentSymbolKind::Method);

        let category = &children[1];
        assert_eq!(category.name.as_str(), "Section");
        assert_eq!(category.kind, DocumentSymbolKind::Category);
        assert_eq!(category.children.len(), 2);
        assert_eq!(category.children[0].name.as_str(), "bar");
        assert_eq!(category.children[1].name.as_str(), "baz");
        // The container's range starts at the divider banner line.
        assert!(source[category.span.as_range()].contains("// === Section ==="));
    }

    #[test]
    fn divider_category_nests_both_instance_and_class_methods() {
        let source = "\
Object subclass: Counter
  // === Construction ===
  class new => self new: 0
  foo => 1
";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module, source);
        let children = &symbols[0].children;

        assert_eq!(children.len(), 1);
        let category = &children[0];
        assert_eq!(category.name.as_str(), "Construction");
        assert_eq!(category.children.len(), 2);
        assert_eq!(category.children[0].kind, DocumentSymbolKind::ClassMethod);
        assert_eq!(category.children[1].kind, DocumentSymbolKind::Method);
    }
}
