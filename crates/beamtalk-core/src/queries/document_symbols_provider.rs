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

use crate::ast::Module;
use crate::language_service::{DocumentSymbol, DocumentSymbolKind};

/// Computes document symbols for a module.
///
/// Returns a hierarchical list of symbols: classes contain methods and fields.
#[must_use]
pub fn compute_document_symbols(module: &Module) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    for class in &module.classes {
        let mut children = Vec::new();

        // Add state variables as field symbols
        for state_var in &class.state {
            children.push(DocumentSymbol {
                name: state_var.name.name.clone(),
                kind: DocumentSymbolKind::Field,
                span: state_var.span,
                children: vec![],
            });
        }

        // Add instance methods
        for method in &class.methods {
            children.push(DocumentSymbol {
                name: method.selector.name(),
                kind: DocumentSymbolKind::Method,
                span: method.span,
                children: vec![],
            });
        }

        // Add class-side methods
        for method in &class.class_methods {
            children.push(DocumentSymbol {
                name: method.selector.name(),
                kind: DocumentSymbolKind::ClassMethod,
                span: method.span,
                children: vec![],
            });
        }

        // ADR 0013: class definitions use `(class)` suffix
        symbols.push(DocumentSymbol {
            name: format!("{} (class)", class.name.name).into(),
            kind: DocumentSymbolKind::Class,
            span: class.span,
            children,
        });
    }

    symbols
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    #[test]
    fn empty_module_returns_no_symbols() {
        let tokens = lex_with_eof("");
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module);
        assert!(symbols.is_empty());
    }

    #[test]
    fn class_definition_returns_class_symbol() {
        let source = "Object subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module);

        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name.as_str(), "Counter (class)");
        assert_eq!(symbols[0].kind, DocumentSymbolKind::Class);
    }

    #[test]
    fn class_children_include_state_and_methods() {
        let source = "Object subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n  value => self.count";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let symbols = compute_document_symbols(&module);

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
        let symbols = compute_document_symbols(&module);

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
}
