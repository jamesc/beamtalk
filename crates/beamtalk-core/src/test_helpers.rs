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

/// Asserts that the given Core Erlang source compiles successfully through `erlc`.
///
/// Uses `tempfile::tempdir()` for an isolated temp directory per invocation,
/// avoiding filename collisions when tests run in parallel. The temp dir and
/// all its contents are automatically cleaned up on drop.
///
/// If `erlc` is not found in PATH, prints a skip notice and returns without
/// failing.
///
/// # Panics
///
/// Panics if `erlc` exits with a non-zero status, including the full Core
/// Erlang source in the panic message to aid debugging.
#[cfg(test)]
pub fn assert_compiles_through_erlc(module_name: &str, core_erlang: &str) {
    use std::fs;
    use std::process::Command;

    let tmp_dir = tempfile::tempdir().expect("failed to create temp dir for erlc test");
    let core_file = tmp_dir.path().join(format!("{module_name}.core"));
    fs::write(&core_file, core_erlang).expect("should write core erlang file");

    let output = Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(tmp_dir.path())
        .arg(&core_file)
        .output();

    match output {
        Ok(output) => {
            assert!(
                output.status.success(),
                "erlc compilation failed for module '{module_name}':\nstdout: {}\nstderr: {}\n\nGenerated Core Erlang:\n{core_erlang}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            );
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            println!("Skipping erlc compilation test for '{module_name}' - erlc not in PATH");
        }
        Err(e) => {
            panic!("failed to invoke erlc for module '{module_name}': {e}");
        }
    }
    // tmp_dir is dropped here, auto-cleaning .core and .beam files
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
    use crate::semantic_analysis::class_hierarchy::DeclaredType;
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

    /// Builds a minimal Actor [`ClassDefinition`] with the given name, no state, and no methods.
    ///
    /// Equivalent to `Actor subclass: <name>` with no declarations — the standard fixture for
    /// codegen tests that need an actor class but don't care about its shape.
    #[must_use]
    pub fn make_actor_class(name: &str) -> ClassDefinition {
        let span = test_span();
        ClassDefinition::new(
            Identifier::new(name, span),
            Identifier::new("Actor", span),
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

    /// Arbitrary [`DeclaredType`] values, recursively covering every
    /// grouping shape BT-3100's type-string fidelity properties exercise:
    /// union, generic, `FalseOr`/optional, difference, intersection, and
    /// the `Self`-family leaves (`Self`, `Self class`, `<Name> class`).
    ///
    /// Shared by `declared_type.rs`'s own `parse`/`Display` fixed-point
    /// property tests and `beamtalk-compiler-port`'s wire-fidelity property
    /// test (ETF encode/decode round trip) — both boundaries need the exact
    /// same generated shape space, so it lives here once rather than being
    /// copied into both crates (this repo's "No duplicate implementations"
    /// rule).
    pub fn arb_declared_type() -> impl proptest::strategy::Strategy<Value = DeclaredType> {
        use proptest::prelude::*;

        let leaf = prop_oneof![
            "[A-Za-z][A-Za-z0-9]{0,4}".prop_map(DeclaredType::simple),
            "[a-z][a-z0-9]{0,4}".prop_map(DeclaredType::singleton),
            Just(DeclaredType::SelfType),
            Just(DeclaredType::SelfClass),
            "[A-Za-z][A-Za-z0-9]{0,4}".prop_map(|n| DeclaredType::ClassOf(n.into())),
        ];

        leaf.prop_recursive(4, 32, 4, |inner| {
            prop_oneof![
                prop::collection::vec(inner.clone(), 2..4).prop_map(DeclaredType::Union),
                (
                    "[A-Za-z][A-Za-z0-9]{0,4}",
                    prop::collection::vec(inner.clone(), 1..3),
                )
                    .prop_map(|(base, params)| DeclaredType::generic(base, params)),
                inner
                    .clone()
                    .prop_map(|t| DeclaredType::FalseOr(Box::new(t))),
                (inner.clone(), inner.clone()).prop_map(|(base, excluded)| {
                    DeclaredType::Difference {
                        base: Box::new(base),
                        excluded: Box::new(excluded),
                    }
                }),
                (inner.clone(), inner.clone()).prop_map(|(left, right)| {
                    DeclaredType::Intersection {
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                }),
            ]
        })
    }

    /// Patterns that should never appear in valid Core Erlang output — Rust
    /// Debug/Display leaks (BT-875).
    ///
    /// Shared by `core_erlang_validity_tests.rs`'s proptest suite and the
    /// `compile_pipeline` fuzz target (BT-3124) so the two never drift.
    pub const CORE_ERLANG_FORMAT_ARTIFACT_PATTERNS: &[&str] = &[
        "{:?}",
        "Document::",
        "BinaryOp(",
        "Expression::",
        "Literal::",
        "MessageSelector::",
        "Pattern::",
        "TokenKind::",
    ];

    /// Checks that parentheses, brackets, and braces are balanced in Core
    /// Erlang output, skipping the contents of single- and double-quoted
    /// atom/string literals (which may themselves contain unbalanced
    /// delimiter characters).
    ///
    /// Shared by `core_erlang_validity_tests.rs`'s proptest suite and the
    /// `compile_pipeline` fuzz target (BT-3124) so the two never drift.
    #[must_use]
    pub fn core_erlang_has_balanced_delimiters(s: &str) -> bool {
        let mut stack = Vec::new();
        let mut chars = s.chars().peekable();
        let mut in_single_quote = false;
        let mut in_double_quote = false;

        while let Some(ch) = chars.next() {
            match ch {
                '\'' if !in_double_quote => in_single_quote = !in_single_quote,
                '"' if !in_single_quote => in_double_quote = !in_double_quote,
                '\\' if in_single_quote || in_double_quote => {
                    // Skip escaped character
                    chars.next();
                }
                _ if in_single_quote || in_double_quote => {}
                '(' => stack.push(')'),
                '[' => stack.push(']'),
                '{' => stack.push('}'),
                ')' | ']' | '}' => {
                    if stack.pop() != Some(ch) {
                        return false;
                    }
                }
                _ => {}
            }
        }
        stack.is_empty() && !in_single_quote && !in_double_quote
    }

    /// Checks the structural-validity properties `generate_module`'s output
    /// must satisfy whenever it returns `Ok`: starts with `module`, ends
    /// with `end`, has balanced delimiters, and contains no Rust format
    /// artifacts. Returns a list of human-readable violation descriptions —
    /// empty means the output is structurally valid.
    ///
    /// Shared by `core_erlang_validity_tests.rs`'s proptest suite and the
    /// `compile_pipeline` fuzz target (BT-3124) so the two never drift.
    #[must_use]
    pub fn core_erlang_structural_issues(output: &str) -> Vec<String> {
        let mut issues = Vec::new();
        let trimmed = output.trim();

        if !trimmed.starts_with("module") {
            issues.push(format!(
                "does not start with 'module':\n{}",
                char_prefix(trimmed, 200)
            ));
        }
        if !trimmed.ends_with("end") {
            issues.push(format!(
                "does not end with 'end':\n...{}",
                char_suffix(trimmed, 200)
            ));
        }
        if !core_erlang_has_balanced_delimiters(output) {
            issues.push("has unbalanced delimiters".to_string());
        }
        for pattern in CORE_ERLANG_FORMAT_ARTIFACT_PATTERNS {
            if output.contains(pattern) {
                issues.push(format!("contains format artifact {pattern:?}"));
            }
        }

        issues
    }

    /// Returns the first `max_chars` characters of `s` (fewer if shorter),
    /// respecting UTF-8 boundaries. A hand-rolled, MSRV-1.85-compatible
    /// stand-in for the standard library's `floor_char_boundary` (stable
    /// since 1.91, past this crate's pinned MSRV).
    fn char_prefix(s: &str, max_chars: usize) -> &str {
        match s.char_indices().nth(max_chars) {
            Some((byte_idx, _)) => &s[..byte_idx],
            None => s,
        }
    }

    /// Returns the last `max_chars` characters of `s` (fewer if shorter),
    /// respecting UTF-8 boundaries. A hand-rolled, MSRV-1.85-compatible
    /// stand-in for the standard library's `ceil_char_boundary` (stable
    /// since 1.91, past this crate's pinned MSRV).
    fn char_suffix(s: &str, max_chars: usize) -> &str {
        let char_count = s.chars().count();
        let skip = char_count.saturating_sub(max_chars);
        match s.char_indices().nth(skip) {
            Some((byte_idx, _)) => &s[byte_idx..],
            None => "",
        }
    }

    /// Returns the standard proptest configuration used across beamtalk-core property tests.
    ///
    /// Sets `cases` to at least 512 (overridable via `PROPTEST_CASES` env var).
    ///
    /// Gated on `#[cfg(test)]` because `proptest` is a dev-dependency; it is not
    /// available to dependent crates that enable the `"test"` feature.
    #[cfg(test)]
    #[must_use]
    pub fn proptest_config_default() -> proptest::prelude::ProptestConfig {
        let default = proptest::prelude::ProptestConfig::default();
        proptest::prelude::ProptestConfig {
            cases: default.cases.max(512),
            ..default
        }
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
        fn make_actor_class_builds_minimal_actor_class() {
            let class = make_actor_class("CounterActor");
            assert_eq!(class.name.name.as_str(), "CounterActor");
            assert_eq!(
                class
                    .superclass
                    .as_ref()
                    .map(|superclass| superclass.name.as_str()),
                Some("Actor")
            );
            assert_eq!(class.class_kind, crate::ast::ClassKind::Actor);
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
