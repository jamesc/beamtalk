// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! References provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `ReferencesProvider` from the DDD model.
//! It finds all references to a symbol (class name or method selector) across
//! all indexed files.
//!
//! # Design
//!
//! References are collected by walking the AST of each file:
//! - **Class references**: All `Identifier` and `ClassReference` nodes matching the class name,
//!   plus class definitions referencing it as a superclass
//! - **Method selector references**: All `MessageSend` and `Cascade` nodes with matching selector,
//!   plus method definitions with matching selector
//!
//! # Performance
//!
//! Must respond in <500ms for typical project sizes (project-wide search).
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - ADR 0024: Static-First, Live-Augmented IDE Tooling
//! - LSP specification: Language Server Protocol textDocument/references

use crate::ast::{Expression, Module};
use crate::language_service::Location;
use crate::source_analysis::Span;
use camino::Utf8PathBuf;

/// Find all references to a class name across files.
///
/// Collects all locations where the class name appears as:
/// - An identifier in expressions (variable references to class)
/// - A class reference expression
/// - A superclass in class definitions
/// - A class definition name
pub fn find_class_references<'a>(
    class_name: &str,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Vec<Location> {
    let mut results = Vec::new();

    for (file_path, module) in files {
        // Class definitions: name and superclass references
        for class in &module.classes {
            if class.name.name == class_name {
                results.push(Location::new(file_path.clone(), class.name.span));
            }
            if let Some(ref superclass) = class.superclass {
                if superclass.name == class_name {
                    results.push(Location::new(file_path.clone(), superclass.span));
                }
            }
        }

        // Standalone method definitions: class name references
        for smd in &module.method_definitions {
            if smd.class_name.name == class_name {
                results.push(Location::new(file_path.clone(), smd.class_name.span));
            }
        }

        // Expressions: identifiers and class references
        for expr in &module.expressions {
            collect_class_refs(expr, class_name, file_path, &mut results);
        }

        // Class method bodies
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for expr in &method.body {
                    collect_class_refs(expr, class_name, file_path, &mut results);
                }
            }
        }

        // Standalone method bodies
        for smd in &module.method_definitions {
            for expr in &smd.method.body {
                collect_class_refs(expr, class_name, file_path, &mut results);
            }
        }
    }

    results
}

/// Recursively collect class name references in expressions.
fn collect_class_refs(
    expr: &Expression,
    class_name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    match expr {
        Expression::Identifier(ident) if ident.name == class_name => {
            results.push(Location::new(file_path.clone(), ident.span));
        }
        Expression::ClassReference { name, .. } if name.name == class_name => {
            results.push(Location::new(file_path.clone(), name.span));
        }
        Expression::Assignment { target, value, .. } => {
            collect_class_refs(target, class_name, file_path, results);
            collect_class_refs(value, class_name, file_path, results);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            collect_class_refs(receiver, class_name, file_path, results);
            for arg in arguments {
                collect_class_refs(arg, class_name, file_path, results);
            }
        }
        Expression::Block(block) => {
            for expr in &block.body {
                collect_class_refs(expr, class_name, file_path, results);
            }
        }
        Expression::Return { value, .. } => {
            collect_class_refs(value, class_name, file_path, results);
        }
        Expression::Parenthesized { expression, .. } => {
            collect_class_refs(expression, class_name, file_path, results);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_class_refs(receiver, class_name, file_path, results);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_class_refs(receiver, class_name, file_path, results);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_class_refs(arg, class_name, file_path, results);
                }
            }
        }
        Expression::Match { value, arms, .. } => {
            collect_class_refs(value, class_name, file_path, results);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_class_refs(guard, class_name, file_path, results);
                }
                collect_class_refs(&arm.body, class_name, file_path, results);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(expr) = segment {
                    collect_class_refs(expr, class_name, file_path, results);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_class_refs(element, class_name, file_path, results);
            }
            if let Some(tail_expr) = tail {
                collect_class_refs(tail_expr, class_name, file_path, results);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_class_refs(&pair.key, class_name, file_path, results);
                collect_class_refs(&pair.value, class_name, file_path, results);
            }
        }
        _ => {}
    }
}

/// Find all references to a method selector across files.
///
/// Collects all locations where the selector appears as:
/// - A message send with matching selector
/// - A cascade message with matching selector
/// - A method definition with matching selector
pub fn find_selector_references<'a>(
    selector_name: &str,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Vec<Location> {
    let mut results = Vec::new();

    for (file_path, module) in files {
        // Method definitions in classes
        for class in &module.classes {
            collect_method_def_refs(&class.methods, selector_name, file_path, &mut results);
            collect_method_def_refs(&class.class_methods, selector_name, file_path, &mut results);
        }

        // Standalone method definitions
        for smd in &module.method_definitions {
            if smd.method.selector.name() == selector_name {
                results.push(Location::new(file_path.clone(), smd.method.span));
            }
        }

        // Message sends and cascades in expressions
        for expr in &module.expressions {
            collect_selector_refs(expr, selector_name, file_path, &mut results);
        }

        // Message sends and cascades in class method bodies
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for expr in &method.body {
                    collect_selector_refs(expr, selector_name, file_path, &mut results);
                }
            }
        }

        // Message sends and cascades in standalone method bodies
        for smd in &module.method_definitions {
            for expr in &smd.method.body {
                collect_selector_refs(expr, selector_name, file_path, &mut results);
            }
        }
    }

    results
}

/// Collect method definition references with matching selector.
fn collect_method_def_refs(
    methods: &[crate::ast::MethodDefinition],
    selector_name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    for method in methods {
        if method.selector.name() == selector_name {
            results.push(Location::new(file_path.clone(), method.span));
        }
    }
}

/// Recursively collect selector references in expressions.
fn collect_selector_refs(
    expr: &Expression,
    selector_name: &str,
    file_path: &Utf8PathBuf,
    results: &mut Vec<Location>,
) {
    match expr {
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
        } => {
            if selector.name() == selector_name {
                // Use the span of the selector keywords if available, otherwise the send span
                let ref_span = selector_reference_span(selector).unwrap_or(*span);
                results.push(Location::new(file_path.clone(), ref_span));
            }
            collect_selector_refs(receiver, selector_name, file_path, results);
            for arg in arguments {
                collect_selector_refs(arg, selector_name, file_path, results);
            }
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_selector_refs(receiver, selector_name, file_path, results);
            for msg in messages {
                if msg.selector.name() == selector_name {
                    let ref_span = selector_reference_span(&msg.selector).unwrap_or(msg.span);
                    results.push(Location::new(file_path.clone(), ref_span));
                }
                for arg in &msg.arguments {
                    collect_selector_refs(arg, selector_name, file_path, results);
                }
            }
        }
        Expression::Assignment { target, value, .. } => {
            collect_selector_refs(target, selector_name, file_path, results);
            collect_selector_refs(value, selector_name, file_path, results);
        }
        Expression::Block(block) => {
            for expr in &block.body {
                collect_selector_refs(expr, selector_name, file_path, results);
            }
        }
        Expression::Return { value, .. } => {
            collect_selector_refs(value, selector_name, file_path, results);
        }
        Expression::Parenthesized { expression, .. } => {
            collect_selector_refs(expression, selector_name, file_path, results);
        }
        Expression::FieldAccess { receiver, .. } => {
            collect_selector_refs(receiver, selector_name, file_path, results);
        }
        Expression::Match { value, arms, .. } => {
            collect_selector_refs(value, selector_name, file_path, results);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_selector_refs(guard, selector_name, file_path, results);
                }
                collect_selector_refs(&arm.body, selector_name, file_path, results);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(expr) = segment {
                    collect_selector_refs(expr, selector_name, file_path, results);
                }
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for element in elements {
                collect_selector_refs(element, selector_name, file_path, results);
            }
            if let Some(tail_expr) = tail {
                collect_selector_refs(tail_expr, selector_name, file_path, results);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_selector_refs(&pair.key, selector_name, file_path, results);
                collect_selector_refs(&pair.value, selector_name, file_path, results);
            }
        }
        _ => {}
    }
}

/// Get the span covering a selector's keywords (for keyword messages)
/// or None for unary/binary selectors (whose span is part of the message send).
fn selector_reference_span(selector: &crate::ast::MessageSelector) -> Option<Span> {
    match selector {
        crate::ast::MessageSelector::Keyword(parts) if !parts.is_empty() => {
            let first = parts.first().unwrap().span;
            let last = parts.last().unwrap().span;
            Some(first.merge(last))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    fn parse_source(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        parse(tokens).0
    }

    #[test]
    fn find_class_references_in_expressions() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x := Foo new\ny := Foo bar");

        let refs = find_class_references("Foo", [(&file, &module)]);
        assert_eq!(refs.len(), 2);
        assert!(refs.iter().all(|r| r.file == file));
    }

    #[test]
    fn find_class_references_in_class_definition() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("Object subclass: Foo\n  bar => 1");

        let refs = find_class_references("Foo", [(&file, &module)]);
        // Should find the class name in the definition
        assert!(!refs.is_empty());
    }

    #[test]
    fn find_class_references_as_superclass() {
        let file = Utf8PathBuf::from("test.bt");
        let module =
            parse_source("Object subclass: Base\n  foo => 1\nBase subclass: Child\n  bar => 2");

        let refs = find_class_references("Base", [(&file, &module)]);
        // Should find: class definition name + superclass reference
        assert_eq!(refs.len(), 2);
    }

    #[test]
    fn find_class_references_cross_file() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");

        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("x := Foo new");

        let refs = find_class_references("Foo", [(&file_a, &module_a), (&file_b, &module_b)]);
        // file_a: class definition name, file_b: Foo identifier
        assert_eq!(refs.len(), 2);
        assert!(refs.iter().any(|r| r.file == file_a));
        assert!(refs.iter().any(|r| r.file == file_b));
    }

    #[test]
    fn find_selector_references_in_message_sends() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x bar\ny bar");

        let refs = find_selector_references("bar", [(&file, &module)]);
        assert_eq!(refs.len(), 2);
    }

    #[test]
    fn find_selector_references_in_method_definition() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("Object subclass: Foo\n  bar => 1");

        let refs = find_selector_references("bar", [(&file, &module)]);
        // Should find method definition
        assert!(!refs.is_empty());
    }

    #[test]
    fn find_selector_references_cross_file() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");

        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("x := Foo new\nx bar");

        let refs = find_selector_references("bar", [(&file_a, &module_a), (&file_b, &module_b)]);
        // file_a: method definition, file_b: message send
        assert_eq!(refs.len(), 2);
        assert!(refs.iter().any(|r| r.file == file_a));
        assert!(refs.iter().any(|r| r.file == file_b));
    }

    #[test]
    fn find_selector_references_keyword_message() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x at: 1 put: 2\ny at: 3 put: 4");

        let refs = find_selector_references("at:put:", [(&file, &module)]);
        assert_eq!(refs.len(), 2);
    }

    #[test]
    fn find_selector_references_no_match() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x bar");

        let refs = find_selector_references("baz", [(&file, &module)]);
        assert!(refs.is_empty());
    }

    #[test]
    fn find_class_references_in_method_bodies() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("Object subclass: Widget\n  make => Foo new");

        // Foo appears inside method body — should be found
        let refs = find_class_references("Foo", [(&file, &module)]);
        assert!(!refs.is_empty(), "Should find Foo in method body");
    }

    #[test]
    fn find_selector_references_in_method_bodies() {
        let file = Utf8PathBuf::from("test.bt");
        let module =
            parse_source("Object subclass: Widget\n  doWork: x => x bar\n  go => self bar");

        // "bar" appears as method send inside method bodies
        let refs = find_selector_references("bar", [(&file, &module)]);
        // Should find: 2 message sends in method bodies
        assert!(
            refs.len() >= 2,
            "Should find bar in method bodies, found {}",
            refs.len()
        );
    }
}
