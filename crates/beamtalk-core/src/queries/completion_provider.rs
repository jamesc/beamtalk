// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Completion provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `CompletionProvider` from the DDD model.
//! It suggests completions at the cursor position based on the current editing
//! context. The provider follows LSP terminology and aligns with the ubiquitous
//! language defined in `docs/beamtalk-ddd-model.md`.
//!
//! # Design
//!
//! Completions are context-sensitive and consider:
//! - Position in the source (is it after a dot? after whitespace?)
//! - Surrounding AST nodes (in a block? in a message send?)
//! - Available identifiers in scope
//! - Language keywords and built-in types
//!
//! # Performance
//!
//! Must respond in <50ms for typical file sizes.
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - LSP specification: Language Server Protocol completion requests

use crate::ast::{Expression, Module};
use crate::language_service::{Completion, CompletionKind, Position};
use crate::semantic_analysis::ClassHierarchy;
use ecow::EcoString;
use std::collections::HashSet;

/// Computes code completions at a given position in a module.
///
/// # Arguments
///
/// * `module` - The parsed AST
/// * `source` - The source text
/// * `position` - The cursor position
///
/// # Returns
///
/// A list of completion suggestions appropriate for the context.
///
/// # Examples
///
/// ```
/// use beamtalk_core::queries::completion_provider::compute_completions;
/// use beamtalk_core::language_service::Position;
/// use beamtalk_core::semantic_analysis::ClassHierarchy;
/// use beamtalk_core::source_analysis::{lex_with_eof, parse};
///
/// let source = "x := 42";
/// let tokens = lex_with_eof(source);
/// let (module, _) = parse(tokens);
/// let hierarchy = ClassHierarchy::build(&module).0;
///
/// let completions = compute_completions(&module, source, Position::new(0, 0), &hierarchy);
/// assert!(!completions.is_empty());
/// // Should include keywords like "self", "true", "false"
/// assert!(completions.iter().any(|c| c.label == "self"));
/// ```
#[must_use]
pub fn compute_completions(
    module: &Module,
    source: &str,
    position: Position,
    hierarchy: &ClassHierarchy,
) -> Vec<Completion> {
    // Validate position is within bounds
    if position.to_offset(source).is_none() {
        return Vec::new();
    }

    let mut completions = Vec::new();

    // Add keyword completions
    add_keyword_completions(&mut completions);

    // Add identifiers from the current scope
    add_identifier_completions(module, &mut completions);

    // Add message completions from class hierarchy (includes inherited methods)
    add_hierarchy_completions(module, hierarchy, &mut completions);

    // Remove duplicates
    deduplicate_completions(&mut completions);

    completions
}

/// Adds keyword completions.
fn add_keyword_completions(completions: &mut Vec<Completion>) {
    let keywords = [
        ("self", "Reference to the current object"),
        ("super", "Reference to the superclass"),
        ("true", "Boolean true value"),
        ("false", "Boolean false value"),
        ("nil", "Null/absent value"),
        ("match:", "Pattern matching expression"),
        ("if:then:else:", "Conditional expression"),
    ];

    for (keyword, doc) in &keywords {
        completions
            .push(Completion::new(*keyword, CompletionKind::Keyword).with_documentation(*doc));
    }
}

/// Adds identifier completions from the module.
fn add_identifier_completions(module: &Module, completions: &mut Vec<Completion>) {
    let mut identifiers = HashSet::new();

    // Collect all identifiers from the module
    for expr in &module.expressions {
        collect_identifiers_from_expr(expr, &mut identifiers);
    }

    // Add them as completions
    for ident in identifiers {
        completions.push(Completion::new(ident, CompletionKind::Variable));
    }
}

/// Recursively collects identifiers from an expression.
fn collect_identifiers_from_expr(expr: &Expression, identifiers: &mut HashSet<EcoString>) {
    match expr {
        Expression::Identifier(ident) => {
            identifiers.insert(ident.name.clone());
        }
        Expression::Assignment { target, value, .. } => {
            collect_identifiers_from_expr(target, identifiers);
            collect_identifiers_from_expr(value, identifiers);
        }
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            collect_identifiers_from_expr(receiver, identifiers);
            for arg in arguments {
                collect_identifiers_from_expr(arg, identifiers);
            }
        }
        Expression::Block(block) => {
            for param in &block.parameters {
                identifiers.insert(param.name.clone());
            }
            for expr in &block.body {
                collect_identifiers_from_expr(expr, identifiers);
            }
        }
        Expression::Return { value, .. } => {
            collect_identifiers_from_expr(value, identifiers);
        }
        Expression::Parenthesized { expression, .. } => {
            collect_identifiers_from_expr(expression, identifiers);
        }
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            identifiers.insert(field.name.clone());
            collect_identifiers_from_expr(receiver, identifiers);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_identifiers_from_expr(receiver, identifiers);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_identifiers_from_expr(arg, identifiers);
                }
            }
        }
        Expression::Pipe { value, target, .. } => {
            collect_identifiers_from_expr(value, identifiers);
            collect_identifiers_from_expr(target, identifiers);
        }
        Expression::Match { value, arms, .. } => {
            collect_identifiers_from_expr(value, identifiers);
            for arm in arms {
                collect_identifiers_from_expr(&arm.body, identifiers);
            }
        }
        _ => {}
    }
}

/// Adds method completions from the class hierarchy.
///
/// Uses the provided `ClassHierarchy` to include all available methods
/// (local + inherited) for classes defined in the module, plus common
/// Object/ProtoObject methods for general use.
fn add_hierarchy_completions(
    module: &Module,
    hierarchy: &ClassHierarchy,
    completions: &mut Vec<Completion>,
) {
    let mut seen = HashSet::new();

    // Add methods from classes defined in this module (local + inherited)
    for class in &module.classes {
        for method in hierarchy.all_methods(class.name.name.as_str()) {
            if seen.insert(method.selector.clone()) {
                let doc = format!("{}#{}", method.defined_in, method.selector);
                completions.push(
                    Completion::new(method.selector.as_str(), CompletionKind::Function)
                        .with_documentation(doc),
                );
            }
        }
    }

    // Always include common Object/ProtoObject methods for general completions
    for method in hierarchy.all_methods("Object") {
        if seen.insert(method.selector.clone()) {
            let doc = format!("{}#{}", method.defined_in, method.selector);
            completions.push(
                Completion::new(method.selector.as_str(), CompletionKind::Function)
                    .with_documentation(doc),
            );
        }
    }
}

/// Removes duplicate completions based on label.
///
/// Keeps the first occurrence of each unique label.
fn deduplicate_completions(completions: &mut Vec<Completion>) {
    let mut seen = HashSet::new();
    completions.retain(|c| {
        // Check for existence before cloning
        if seen.contains(&c.label) {
            false
        } else {
            seen.insert(c.label.clone());
            true
        }
    });
}

#[cfg(test)]
mod tests {
    //! Unit tests for code completion functionality.
    //!
    //! Tests verify that completions:
    //! - Include language keywords (self, super, true, false, nil)
    //! - Include identifiers from the current scope
    //! - Include common message selectors (at:, do:, size)
    //! - Deduplicate repeated identifiers
    //! - Handle edge cases (invalid positions, empty source, block parameters)
    //! - Provide appropriate documentation and completion kinds

    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    /// Parse source and compute completions with a fresh hierarchy.
    fn completions_at(source: &str, position: Position) -> Vec<Completion> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0;
        compute_completions(&module, source, position, &hierarchy)
    }

    #[test]
    fn compute_completions_includes_keywords() {
        let completions = completions_at("x := 42", Position::new(0, 0));

        assert!(completions.iter().any(|c| c.label == "self"));
        assert!(completions.iter().any(|c| c.label == "true"));
        assert!(completions.iter().any(|c| c.label == "false"));
    }

    #[test]
    fn compute_completions_includes_identifiers() {
        let completions = completions_at("x := 42.\ny := x", Position::new(1, 0));

        assert!(completions.iter().any(|c| c.label == "x"));
        assert!(completions.iter().any(|c| c.label == "y"));
    }

    #[test]
    fn compute_completions_includes_messages() {
        let completions = completions_at("", Position::new(0, 0));

        // Should include Object/ProtoObject methods
        assert!(completions.iter().any(|c| c.label == "isNil"));
        assert!(completions.iter().any(|c| c.label == "class"));
        assert!(completions.iter().any(|c| c.label == "respondsTo:"));
    }

    #[test]
    fn compute_completions_no_duplicates() {
        let completions = completions_at("x := 1.\nx := 2", Position::new(0, 0));

        let x_count = completions.iter().filter(|c| c.label == "x").count();
        assert_eq!(x_count, 1);
    }

    #[test]
    fn compute_completions_with_invalid_position() {
        // Position beyond end of file
        let completions = completions_at("x := 42", Position::new(100, 100));

        // Should return empty vec for out-of-bounds position
        assert!(completions.is_empty());
    }

    #[test]
    fn compute_completions_with_block_expressions() {
        let completions = completions_at("block := [:x | x + 1]", Position::new(0, 0));

        // Should include both the block variable and parameter
        assert!(completions.iter().any(|c| c.label == "block"));
        assert!(completions.iter().any(|c| c.label == "x"));
    }

    #[test]
    fn compute_completions_with_message_sends() {
        let completions = completions_at("obj doSomething", Position::new(0, 0));

        // Should include the identifier
        assert!(completions.iter().any(|c| c.label == "obj"));
    }

    #[test]
    fn compute_completions_empty_source() {
        let completions = completions_at("", Position::new(0, 0));

        // Should still return keywords and common messages
        assert!(!completions.is_empty());
        assert!(completions.iter().any(|c| c.label == "self"));
    }

    #[test]
    fn keyword_completions_have_documentation() {
        let mut completions = Vec::new();
        add_keyword_completions(&mut completions);

        // All keywords should have documentation
        for completion in completions {
            if let CompletionKind::Keyword = completion.kind {
                assert!(completion.documentation.is_some());
            }
        }
    }

    #[test]
    fn hierarchy_completions_include_object_methods() {
        let completions = completions_at("", Position::new(0, 0));

        // Should include Object methods like isNil, class, respondsTo:
        assert!(completions.iter().any(|c| c.label == "isNil"));
        assert!(completions.iter().any(|c| c.label == "class"));
        assert!(completions.iter().any(|c| c.label == "respondsTo:"));

        // All method completions should have Function kind
        let method_completions: Vec<_> = completions
            .iter()
            .filter(|c| matches!(c.kind, CompletionKind::Function))
            .collect();
        assert!(!method_completions.is_empty());
        for completion in method_completions {
            assert!(completion.documentation.is_some());
        }
    }
}
