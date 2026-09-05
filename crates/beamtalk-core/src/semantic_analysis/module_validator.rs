// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Module-level validation for Beamtalk.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validates module structure constraints:
//! - Enforces one class or protocol per file (BT-1666)

use crate::ast::Module;
use crate::source_analysis::Diagnostic;

/// Validates that a module contains at most one top-level definition
/// (class or protocol).
///
/// Each `.bt` file may contain either a single class **or** a single protocol,
/// but not both, and not multiples of either. Diagnostics point at the
/// offending definition with a hint to move it to its own file.
#[must_use]
pub fn validate_single_definition(module: &Module) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    let num_classes = module.classes.len();
    let num_protocols = module.protocols.len();

    // Case 1: Multiple classes
    if num_classes > 1 {
        let class_names: Vec<&str> = module
            .classes
            .iter()
            .map(|c| c.name.name.as_str())
            .collect();
        let names_list = class_names.join(", ");

        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Multiple classes in one file is not supported. Found classes: {names_list}. \
                     Each class should be in its own .bt file."
                ),
                module.classes[1].name.span,
            )
            .with_hint("Move this class to a separate .bt file."),
        );
    }

    // Case 2: Multiple protocols
    if num_protocols > 1 {
        let protocol_names: Vec<&str> = module
            .protocols
            .iter()
            .map(|p| p.name.name.as_str())
            .collect();
        let names_list = protocol_names.join(", ");

        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Multiple protocols in one file is not supported. Found protocols: \
                     {names_list}. Each protocol should be in its own .bt file."
                ),
                module.protocols[1].name.span,
            )
            .with_hint("Move this protocol to a separate .bt file."),
        );
    }

    // Case 3: Mixed class and protocol in the same file
    if num_classes >= 1 && num_protocols >= 1 {
        // Point at whichever comes second in source order
        let first_class_start = module.classes[0].name.span.start();
        let first_protocol_start = module.protocols[0].name.span.start();

        if first_class_start < first_protocol_start {
            // Class came first, point at the protocol
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "A class and a protocol cannot be in the same file. \
                         Found class '{}' and protocol '{}'. \
                         Each should be in its own .bt file.",
                        module.classes[0].name.name, module.protocols[0].name.name,
                    ),
                    module.protocols[0].name.span,
                )
                .with_hint("Move this protocol to a separate .bt file."),
            );
        } else {
            // Protocol came first, point at the class
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "A class and a protocol cannot be in the same file. \
                         Found protocol '{}' and class '{}'. \
                         Each should be in its own .bt file.",
                        module.protocols[0].name.name, module.classes[0].name.name,
                    ),
                    module.classes[0].name.span,
                )
                .with_hint("Move this class to a separate .bt file."),
            );
        }
    }

    diagnostics
}

/// Validates that a file's basename agrees with the class it declares,
/// under Erlang module-name case-folding (BT-3431).
///
/// A `.bt` file's Erlang module name is derived from its own file path
/// (`to_module_name(file_stem)`), independently of the class name declared
/// inside it (ADR 0016/0026). Self-dispatch/state-routing codegen (`gen_server`
/// callbacks, class-var shadow-writes) separately derives the *expected*
/// module name from the declared class name and only treats a class as
/// belonging to its own module when the two agree — see
/// `module_matches_class` (`beamtalk-codegen::core_erlang::util`). When they
/// disagree, that codegen silently fails to recognize self, breaking
/// self-message routing with no diagnostic. This check catches the mismatch
/// early, before it reaches codegen.
///
/// Only applies to a file declaring exactly one class (ADR 0040's "one class
/// per file"); a file with zero or multiple classes is either fine (protocol
/// file) or already flagged by [`validate_single_definition`], which would
/// make "the" file/class comparison ambiguous here. `file_stem` is the
/// caller-supplied basename (without extension) of the file being analysed,
/// or `None` when there is no real file backing the module (REPL sessions,
/// in-memory snippets) — the check is skipped in that case.
#[must_use]
pub fn check_class_file_name_agreement(
    module: &Module,
    file_stem: Option<&str>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    let Some(stem) = file_stem else {
        return diagnostics;
    };
    if module.classes.len() != 1 {
        return diagnostics;
    }

    let class = &module.classes[0];
    let class_name = class.name.name.as_str();
    let expected = crate::ast::to_module_name(class_name);
    let actual = crate::ast::to_module_name(stem);

    if expected != actual {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "File name '{stem}' does not match declared class '{class_name}'. \
                     Erlang module names are derived from the file name, so this class \
                     cannot be recognized as belonging to its own module."
                ),
                class.name.span,
            )
            .with_hint(format!(
                "Rename this file to '{expected}.bt' (or '{class_name}.bt') so the file \
                 name agrees with the class it declares."
            ))
            .with_category(crate::source_analysis::DiagnosticCategory::FileClassNameMismatch),
        );
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
        let diagnostics = validate_single_definition(&module);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_single_protocol_passes() {
        let source = "Protocol define: Printable\n  asString -> String";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        assert_eq!(module.protocols.len(), 1);
        let diagnostics = validate_single_definition(&module);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_multiple_classes_errors() {
        let source = "Object subclass: Foo\n\nObject subclass: Bar";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        assert_eq!(module.classes.len(), 2);

        let diagnostics = validate_single_definition(&module);
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

        let diagnostics = validate_single_definition(&module);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("A, B, C"));
    }

    #[test]
    fn test_no_classes_passes() {
        let source = "42 + 1";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = validate_single_definition(&module);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_multiple_protocols_errors() {
        let source = "Protocol define: Printable\n  asString -> String\n\n\
                       Protocol define: Comparable\n  < other :: Self -> Boolean";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        assert_eq!(module.protocols.len(), 2);

        let diagnostics = validate_single_definition(&module);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("Multiple protocols"));
        assert!(diagnostics[0].message.contains("Printable"));
        assert!(diagnostics[0].message.contains("Comparable"));
        assert!(diagnostics[0].hint.is_some());
    }

    #[test]
    fn test_class_and_protocol_errors() {
        let source = "Object subclass: Foo\n  toString => \"hello\"\n\n\
                       Protocol define: Printable\n  asString -> String";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        assert_eq!(module.classes.len(), 1);
        assert_eq!(module.protocols.len(), 1);

        let diagnostics = validate_single_definition(&module);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("class"));
        assert!(diagnostics[0].message.contains("protocol"));
        assert!(diagnostics[0].message.contains("Foo"));
        assert!(diagnostics[0].message.contains("Printable"));
    }

    #[test]
    fn test_protocol_then_class_errors() {
        let source = "Protocol define: Printable\n  asString -> String\n\n\
                       Object subclass: Foo\n  toString => \"hello\"";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        assert_eq!(module.classes.len(), 1);
        assert_eq!(module.protocols.len(), 1);

        let diagnostics = validate_single_definition(&module);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        // Should point at the class (second definition)
        assert!(diagnostics[0].message.contains("Foo"));
        assert!(diagnostics[0].message.contains("Printable"));
        assert!(diagnostics[0].hint.as_ref().unwrap().contains("class"));
    }

    #[test]
    fn test_two_classes_and_protocol_emits_two_errors() {
        let source = "Object subclass: A\n\nObject subclass: B\n\n\
                       Protocol define: P\n  foo -> Integer";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        assert_eq!(module.classes.len(), 2);
        assert_eq!(module.protocols.len(), 1);

        let diagnostics = validate_single_definition(&module);
        // One error for multiple classes, one for class+protocol mix
        assert_eq!(diagnostics.len(), 2);
    }

    #[test]
    fn test_class_file_name_agreement_matching_snake_case() {
        let source = "Value subclass: ExduraEvent";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = check_class_file_name_agreement(&module, Some("exdura_event"));
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_class_file_name_agreement_matching_pascal_case() {
        // stdlib convention: file stem is the class name verbatim.
        let source = "Object subclass: StackFrame";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = check_class_file_name_agreement(&module, Some("StackFrame"));
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_class_file_name_agreement_mismatch_errors() {
        let source = "Value subclass: ExduraEvent";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = check_class_file_name_agreement(&module, Some("event"));
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("event"));
        assert!(diagnostics[0].message.contains("ExduraEvent"));
        assert_eq!(
            diagnostics[0].category,
            Some(crate::source_analysis::DiagnosticCategory::FileClassNameMismatch)
        );
        assert!(
            diagnostics[0]
                .hint
                .as_ref()
                .unwrap()
                .contains("exdura_event")
        );
    }

    #[test]
    fn test_class_file_name_agreement_skips_without_file_stem() {
        let source = "Value subclass: ExduraEvent";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = check_class_file_name_agreement(&module, None);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_class_file_name_agreement_skips_multiple_classes() {
        // Ambiguous which class "the" file name should agree with —
        // `validate_single_definition` already flags this file separately.
        let source = "Object subclass: Foo\n\nObject subclass: Bar";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = check_class_file_name_agreement(&module, Some("foo"));
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_class_file_name_agreement_skips_protocol_only_file() {
        let source = "Protocol define: Awaitable\n  result -> Object";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let diagnostics = check_class_file_name_agreement(&module, Some("awaitable"));
        assert!(diagnostics.is_empty());
    }
}
