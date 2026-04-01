// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test helpers for use in beamtalk-core and dependent crate tests.

use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

/// Creates a unique temporary directory path (does not create it on disk).
/// Uses PID + nanosecond timestamp to avoid collisions between parallel tests.
///
/// # Panics
///
/// Panics if the system clock is set before the Unix epoch.
pub fn unique_temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time")
        .as_nanos();
    std::env::temp_dir().join(format!("{prefix}_{}_{}", std::process::id(), nanos))
}

/// Test-only helpers: parsing, codegen assertions, and AST builders.
///
/// Gated on `#[cfg(any(test, feature = "test"))]` to avoid prod binary
/// bloat while allowing dependent crates to opt in via the `test` Cargo
/// feature in their `[dev-dependencies]`.
#[cfg(any(test, feature = "test"))]
pub mod test_support {
    use crate::ast::{
        ClassDefinition, ClassModifiers, Expression, ExpressionStatement, Identifier,
        MessageSelector, MethodDefinition, Module,
    };
    use crate::source_analysis::{Severity, Span, lex_with_eof, parse};

    /// Parses a Beamtalk source string and returns the [`Module`] AST.
    ///
    /// Panics with a helpful message if any parse diagnostics are emitted,
    /// including the source and all diagnostic messages.
    ///
    /// # Panics
    ///
    /// Panics if the source contains parse errors.
    pub fn parse_bt(source: &str) -> Module {
        let (module, diagnostics) = parse(lex_with_eof(source));
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        if !errors.is_empty() {
            let msgs: Vec<_> = diagnostics.iter().map(|d| format!("{d:?}")).collect();
            panic!(
                "parse_bt: source produced {} parse error(s):\n  {}\n\nSource:\n{}",
                errors.len(),
                msgs.join("\n  "),
                source
            );
        }
        module
    }

    /// Asserts that `output` contains `fragment`, panicking with a clear diff
    /// message on failure.
    ///
    /// # Panics
    ///
    /// Panics if `output` does not contain `fragment`.
    pub fn assert_codegen_contains(output: &str, fragment: &str) {
        assert!(
            output.contains(fragment),
            "assert_codegen_contains failed.\n\nExpected fragment:\n  {fragment}\n\nActual output:\n{output}"
        );
    }

    /// Returns a zero-length [`Span`] suitable for test AST nodes.
    #[must_use]
    pub fn test_span() -> Span {
        Span::new(0, 0)
    }

    /// Builds a minimal [`ClassDefinition`] with the given name, no superclass,
    /// no state, and no methods.
    #[must_use]
    pub fn make_class(name: &str) -> ClassDefinition {
        let span = test_span();
        ClassDefinition::with_modifiers(
            Identifier::new(name, span),
            None,
            ClassModifiers::default(),
            Vec::new(),
            Vec::new(),
            span,
        )
    }

    /// Builds a minimal [`MethodDefinition`] with a unary selector and empty body.
    #[must_use]
    pub fn make_method(selector: &str) -> MethodDefinition {
        let span = test_span();
        MethodDefinition::new(
            MessageSelector::Unary(selector.into()),
            Vec::new(),
            Vec::new(),
            span,
        )
    }

    /// Builds an [`Expression::MessageSend`] representing a unary message send.
    ///
    /// `receiver` is an identifier expression; `selector` is the unary method name.
    #[must_use]
    pub fn make_unary_send(receiver: &str, selector: &str) -> Expression {
        let span = test_span();
        Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(receiver, span))),
            selector: MessageSelector::Unary(selector.into()),
            arguments: Vec::new(),
            is_cast: false,
            span,
        }
    }

    /// Wraps an [`Expression`] in a bare [`ExpressionStatement`].
    #[must_use]
    pub fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn parse_bt_parses_class() {
            let module = parse_bt("Object subclass: Foo\n  greet => 42\n");
            assert_eq!(module.classes.len(), 1);
            assert_eq!(module.classes[0].name.name.as_str(), "Foo");
        }

        #[test]
        #[should_panic(expected = "parse_bt: source produced")]
        fn parse_bt_panics_on_invalid_source() {
            parse_bt("Object subclass:");
        }

        #[test]
        fn assert_codegen_contains_passes_on_match() {
            assert_codegen_contains("call 'erlang':'+'(1, 2)", "erlang");
        }

        #[test]
        #[should_panic(expected = "assert_codegen_contains failed")]
        fn assert_codegen_contains_fails_on_mismatch() {
            assert_codegen_contains("hello world", "missing");
        }

        #[test]
        fn make_class_builds_minimal_class() {
            let class = make_class("Counter");
            assert_eq!(class.name.name.as_str(), "Counter");
            assert!(class.superclass.is_none());
            assert!(class.methods.is_empty());
            assert!(class.state.is_empty());
        }

        #[test]
        fn make_method_builds_unary_method() {
            let method = make_method("increment");
            assert_eq!(method.selector, MessageSelector::Unary("increment".into()));
            assert!(method.parameters.is_empty());
            assert!(method.body.is_empty());
        }

        #[test]
        fn make_unary_send_builds_message_send() {
            let expr = make_unary_send("obj", "size");
            match &expr {
                Expression::MessageSend {
                    selector,
                    arguments,
                    is_cast,
                    ..
                } => {
                    assert_eq!(*selector, MessageSelector::Unary("size".into()));
                    assert!(arguments.is_empty());
                    assert!(!is_cast);
                }
                other => panic!("expected MessageSend, got {other:?}"),
            }
        }
    }
}
