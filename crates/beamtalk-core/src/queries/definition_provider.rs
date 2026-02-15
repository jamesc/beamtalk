// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Definition provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `DefinitionProvider` from the DDD model.
//! It locates symbol definitions for go-to-definition requests, supporting
//! both single-file and cross-file lookups via `ProjectIndex`.
//!
//! # Design
//!
//! Definition resolution follows a priority order:
//! 1. Local variable assignment in the current file (first assignment heuristic)
//! 2. Class definition in any indexed file (via `ProjectIndex`)
//!
//! # Performance
//!
//! Must respond in <100ms for typical project sizes.
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - ADR 0024: Static-First, Live-Augmented IDE Tooling
//! - LSP specification: Language Server Protocol textDocument/definition

use crate::ast::{Expression, Module};
use crate::language_service::{Location, ProjectIndex};
use crate::source_analysis::Span;
use camino::Utf8PathBuf;

/// Find the definition of a variable by name within a module (first assignment heuristic).
///
/// Walks the AST looking for the first `Assignment` where the target identifier
/// matches `name`. Returns the span of the target identifier if found.
#[must_use]
pub fn find_definition_in_module(module: &Module, name: &str) -> Option<Span> {
    for expr in &module.expressions {
        if let Some(span) = find_definition_in_expr(expr, name) {
            return Some(span);
        }
    }
    None
}

/// Find the definition of a symbol, searching across all indexed files.
///
/// Resolution order:
/// 1. Local variable assignment in `current_file`
/// 2. Class definition matching the identifier name in any indexed file
///
/// The `files` parameter provides access to parsed modules for all indexed files.
#[must_use]
pub fn find_definition_cross_file<'a>(
    name: &str,
    current_file: &Utf8PathBuf,
    current_module: &Module,
    project_index: &ProjectIndex,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Option<Location> {
    // 1. Local variable definition in current file
    if let Some(span) = find_definition_in_module(current_module, name) {
        return Some(Location::new(current_file.clone(), span));
    }

    // 2. Class definition in the project index (skip builtins — they have no file location)
    if !crate::semantic_analysis::ClassHierarchy::is_builtin_class(name)
        && project_index.hierarchy().has_class(name)
    {
        // Find which file defines this class
        for (file_path, module) in files {
            for class in &module.classes {
                if class.name.name.as_str() == name {
                    return Some(Location::new(file_path.clone(), class.name.span));
                }
            }
        }
    }

    None
}

/// Recursively search for a variable definition (first assignment) in an expression.
fn find_definition_in_expr(expr: &Expression, name: &str) -> Option<Span> {
    match expr {
        Expression::Assignment { target, .. } => {
            if let Expression::Identifier(ident) = target.as_ref() {
                if ident.name == name {
                    return Some(ident.span);
                }
            }
            None
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|e| find_definition_in_expr(e, name)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::ClassHierarchy;
    use crate::source_analysis::{lex_with_eof, parse};

    fn parse_source(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        parse(tokens).0
    }

    #[test]
    fn find_definition_variable_assignment() {
        let module = parse_source("x := 42\ny := x");
        let span = find_definition_in_module(&module, "x");
        assert!(span.is_some());
        assert_eq!(span.unwrap().start(), 0);
    }

    #[test]
    fn find_definition_not_found() {
        let module = parse_source("x := 42");
        let span = find_definition_in_module(&module, "y");
        assert!(span.is_none());
    }

    #[test]
    fn find_definition_in_block() {
        let module = parse_source("[x := 42]");
        let span = find_definition_in_module(&module, "x");
        assert!(span.is_some());
    }

    #[test]
    fn cross_file_local_variable_preferred() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("x := 42\ny := x");
        let index = ProjectIndex::new();

        let loc =
            find_definition_cross_file("x", &file_a, &module_a, &index, [(&file_a, &module_a)]);
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn cross_file_class_definition() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");
        let hierarchy_a = ClassHierarchy::build(&module_a).0;

        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("x := Foo new");

        let mut index = ProjectIndex::new();
        index.update_file(file_a.clone(), &hierarchy_a);

        let loc = find_definition_cross_file(
            "Foo",
            &file_b,
            &module_b,
            &index,
            [(&file_a, &module_a), (&file_b, &module_b)],
        );
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn cross_file_stdlib_class() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("x := Integer new");
        let index = ProjectIndex::new();

        // Integer is a built-in, not defined in any file — so no location
        let loc = find_definition_cross_file("Integer", &file, &module, &index, [(&file, &module)]);
        assert!(loc.is_none());
    }
}
