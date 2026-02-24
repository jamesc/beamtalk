// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Module-level validation for Beamtalk.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validates module structure constraints:
//! - Enforces single-class-per-file rule (multi-class files are not supported)

use crate::ast::Module;
use crate::source_analysis::Diagnostic;

/// Validates that a module contains at most one class definition.
///
/// Multi-class files are not supported. If more than one class is found,
/// a single error diagnostic is emitted pointing at the second class definition.
#[must_use]
pub fn validate_single_class(module: &Module) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    if module.classes.len() > 1 {
        let class_names: Vec<&str> = module
            .classes
            .iter()
            .map(|c| c.name.name.as_str())
            .collect();
        let names_list = class_names.join(", ");

        // Emit a single diagnostic pointing at the second class definition
        let mut diag = Diagnostic::error(
            format!(
                "Multiple classes in one file is not supported. Found classes: {names_list}. \
                 Each class should be in its own .bt file."
            ),
            module.classes[1].name.span,
        );
        diag.hint = Some("Move this class to a separate .bt file.".into());
        diagnostics.push(diag);
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    #[test]
    fn test_single_class_passes() {
        let source = "Object subclass: Foo";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = validate_single_class(&module);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_multiple_classes_errors() {
        let source = "Object subclass: Foo\n\nObject subclass: Bar";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        assert_eq!(module.classes.len(), 2);

        let diagnostics = validate_single_class(&module);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("Multiple classes"));
        assert!(diagnostics[0].message.contains("Foo"));
        assert!(diagnostics[0].message.contains("Bar"));
        assert!(diagnostics[0].hint.is_some());
    }

    #[test]
    fn test_three_classes_emits_single_error() {
        let source = "Object subclass: A\n\nObject subclass: B\n\nObject subclass: C";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        assert_eq!(module.classes.len(), 3);

        let diagnostics = validate_single_class(&module);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("A, B, C"));
    }

    #[test]
    fn test_no_classes_passes() {
        let source = "42 + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = validate_single_class(&module);
        assert!(diagnostics.is_empty());
    }
}
