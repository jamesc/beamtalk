// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Byte-span resolver for method definitions (ADR 0082, Phase 0).
//!
//! **DDD Context:** Source Analysis (Compilation context per ADR 0082).
//!
//! Given the source text of a `.bt` file and a target `(class, selector, kind)`,
//! [`resolve_method_span`] returns the exact byte span of that method's
//! definition — from the first significant token of the definition through the
//! trailing newline that terminates its last body line.
//!
//! # Why this exists
//!
//! ADR 0082 ("Method-Level Edit and Save in the Live Workspace") chose
//! **byte-span replacement**, not AST round-trip, as its flush splice strategy.
//! At flush time a new file body is produced by copying bytes verbatim outside
//! the target span and substituting the patched method source inside it. No
//! reformat, no AST reprint of unchanged content. The load-bearing assumption
//! is that the parser can resolve *any* method's exact byte span against
//! arbitrary `.bt` files. This module — and its corpus round-trip test — exist
//! to validate that assumption before any flush code is written (Phase 0).
//!
//! This is internal scaffolding, not a user-facing feature: the resolver is the
//! proof of concept that the splice approach is sound.
//!
//! # Span boundaries
//!
//! The returned span covers:
//! - **start**: the first significant token of the definition. For a class-body
//!   method that is the first modifier or the selector (whichever comes first);
//!   leading doc comments and indentation are *excluded* (they are not part of
//!   the method body the patcher replaces). For a standalone `Class >> selector`
//!   extension the start is the class-name token.
//! - **end**: the byte immediately after the trailing newline that terminates
//!   the last source line of the body (so the span includes the body and its
//!   trailing newline, per ADR 0082's "start..end including body and trailing
//!   newline"). If the body's last line is the final line of the file with no
//!   trailing newline, the end is clamped to the file length.
//!
//! Splicing `source[span]` back into `source` at `span` is therefore an exact
//! no-op: `source[..start] ++ source[span] ++ source[end..] == source`.

use crate::ast::{ClassDefinition, MessageSelector, Module};
use crate::source_analysis::{Diagnostic, Span, lex_with_eof, parse};

/// Which side of a class a method lives on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodSide {
    /// An instance method (the default; e.g. `increment => ...`).
    Instance,
    /// A class-side method (`class new: name => ...` or `Class class >> ...`).
    Class,
}

impl MethodSide {
    /// Human-readable name for diagnostics.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            MethodSide::Instance => "instance",
            MethodSide::Class => "class",
        }
    }
}

/// Why a method span could not be resolved.
///
/// Resolution never panics on well-formed *or* malformed input — every failure
/// is one of these structured variants (ADR 0082 acceptance criterion: "Failures
/// (selector not found, ambiguous) return a structured error, never a panic").
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanResolveError {
    /// No class with the requested name was found in the source.
    ClassNotFound {
        /// The class name that was searched for.
        class: String,
    },
    /// The class exists but has no method matching `(selector, side)`.
    SelectorNotFound {
        /// The class that was searched.
        class: String,
        /// The selector that was searched for.
        selector: String,
        /// Which side (instance/class) was searched.
        side: MethodSide,
    },
    /// More than one definition matched `(class, selector, side)` — the source
    /// is malformed (duplicate methods) or the resolver cannot disambiguate.
    Ambiguous {
        /// The class that was searched.
        class: String,
        /// The selector that matched more than once.
        selector: String,
        /// Which side (instance/class) was searched.
        side: MethodSide,
        /// How many definitions matched.
        count: usize,
    },
}

impl std::fmt::Display for SpanResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpanResolveError::ClassNotFound { class } => {
                write!(f, "class `{class}` not found in source")
            }
            SpanResolveError::SelectorNotFound {
                class,
                selector,
                side,
            } => write!(
                f,
                "{side} method `{selector}` not found in class `{class}`",
                side = side.as_str()
            ),
            SpanResolveError::Ambiguous {
                class,
                selector,
                side,
                count,
            } => write!(
                f,
                "{side} method `{selector}` in class `{class}` is ambiguous ({count} definitions)",
                side = side.as_str()
            ),
        }
    }
}

impl std::error::Error for SpanResolveError {}

/// Resolves the byte span of a method definition in `source`.
///
/// `class` is the class name (e.g. `"AtomicCounter"`), `selector` is the
/// canonical selector string (e.g. `"increment"`, `"incrementBy:"`, `"at:put:"`,
/// `"+"`), and `side` selects the instance vs class side.
///
/// The returned [`Span`] covers the method definition including its trailing
/// newline; see the module docs for exact boundary semantics. Splicing the
/// span's own bytes back is a guaranteed no-op.
///
/// Parser [`Diagnostic`]s produced while parsing `source` are returned alongside
/// the result so callers can surface parse problems (per the project convention
/// that user-facing operations return `(Result, Vec<Diagnostic>)`). The span
/// `Result` is the resolver's own outcome; an empty diagnostics vector means the
/// source parsed cleanly.
///
/// The `Result` is [`SpanResolveError`] if the class is absent, the selector is
/// absent on the requested side, or the match is ambiguous. Never panics.
pub fn resolve_method_span(
    source: &str,
    class: &str,
    selector: &str,
    side: MethodSide,
) -> (Result<Span, SpanResolveError>, Vec<Diagnostic>) {
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    let result = resolve_in_module(&module, source, class, selector, side);
    (result, diagnostics)
}

/// Resolves against an already-parsed module, avoiding repeated lexing/parsing.
///
/// `pub(crate)` so the corpus round-trip test can parse each file once and
/// resolve many methods against the same [`Module`].
pub(crate) fn resolve_in_module(
    module: &Module,
    source: &str,
    class: &str,
    selector: &str,
    side: MethodSide,
) -> Result<Span, SpanResolveError> {
    // Each match contributes the *AST span* of its definition (whose end is the
    // last body token; the trailing newline is added later by `definition_span`).
    let mut matches: Vec<Span> = Vec::new();
    let mut class_seen = false;

    for class_def in &module.classes {
        if class_def.name.name.as_str() != class {
            continue;
        }
        class_seen = true;
        collect_matches(class_def, selector, side, &mut matches);
    }

    // Standalone `Class >> selector` extension definitions (ADR 0066). These
    // live at module level rather than inside a class body. They count as the
    // same class for span-resolution purposes. Their span starts at the
    // class-name token (not the selector), so use `standalone.span`.
    for standalone in &module.method_definitions {
        if standalone.class_name.name.as_str() != class {
            continue;
        }
        class_seen = true;
        let standalone_side = if standalone.is_class_method {
            MethodSide::Class
        } else {
            MethodSide::Instance
        };
        if standalone_side == side && standalone.method.selector.matches(selector) {
            matches.push(standalone.span);
        }
    }

    match matches.len() {
        0 if !class_seen => Err(SpanResolveError::ClassNotFound {
            class: class.to_string(),
        }),
        0 => Err(SpanResolveError::SelectorNotFound {
            class: class.to_string(),
            selector: selector.to_string(),
            side,
        }),
        1 => Ok(definition_span(source, matches[0])),
        count => Err(SpanResolveError::Ambiguous {
            class: class.to_string(),
            selector: selector.to_string(),
            side,
            count,
        }),
    }
}

/// Collects the spans of instance/class method definitions in `class_def`
/// matching the selector and side.
fn collect_matches(
    class_def: &ClassDefinition,
    selector: &str,
    side: MethodSide,
    out: &mut Vec<Span>,
) {
    let methods = match side {
        MethodSide::Instance => &class_def.methods,
        MethodSide::Class => &class_def.class_methods,
    };
    for method in methods {
        if method.selector.matches(selector) {
            out.push(method.span);
        }
    }
}

/// Computes the definition span for a method whose AST span is `method_span`.
///
/// The AST span ends at the last body expression's last token. The definition
/// span extends that end forward across the remainder of the body's final line,
/// up to and including the terminating newline, so the splice covers the body
/// and its trailing newline (ADR 0082). The start is unchanged.
fn definition_span(source: &str, method_span: Span) -> Span {
    let start = method_span.start();
    let end = extend_to_line_end(source, method_span.end());
    Span::new(start, end)
}

/// Returns the byte offset just past the next newline at or after `offset`,
/// clamped to the source length. If there is no newline before EOF (the final
/// line lacks a trailing newline), returns the source length.
#[expect(
    clippy::cast_possible_truncation,
    reason = "source files over 4GB are not supported (Span uses u32)"
)]
fn extend_to_line_end(source: &str, offset: u32) -> u32 {
    let bytes = source.as_bytes();
    let len = bytes.len();
    let mut i = (offset as usize).min(len);
    while i < len {
        let b = bytes[i];
        i += 1;
        if b == b'\n' {
            break;
        }
    }
    i as u32
}

/// Selector matching helper. Compares the canonical selector string.
trait SelectorMatch {
    fn matches(&self, selector: &str) -> bool;
}

impl SelectorMatch for MessageSelector {
    fn matches(&self, selector: &str) -> bool {
        self.name().as_str() == selector
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// In-crate fixture mirroring the shapes the resolver must handle: a
    /// class-side keyword method, an instance method, a keyword method with a
    /// multi-line body, and an instance method preceded by a doc comment.
    ///
    /// An inline string (rather than `include_str!` of `stdlib/src/*.bt`) keeps
    /// these unit tests self-contained so the crate still compiles from a source
    /// distribution that omits the workspace `stdlib/` directory. The full
    /// stdlib + `examples/` corpus is exercised by the corpus round-trip tests,
    /// which read those files at runtime and skip when absent.
    const ATOMIC: &str = "\
typed Object subclass: AtomicCounter

  /// Create a new named counter starting at 0 (class method).
  class sealed new: name :: Symbol -> AtomicCounter =>
    (Erlang beamtalk_atomic_counter) new: name

  /// Atomically add 1. Returns the new value.
  increment -> Integer => (Erlang beamtalk_atomic_counter) increment: self

  /// Atomically add N. Returns the new value.
  incrementBy: n :: Integer -> Integer =>
    (Erlang beamtalk_atomic_counter) incrementBy: self by: n

  /// Read the current value.
  value -> Integer => (Erlang beamtalk_atomic_counter) readValue: self
";

    /// Test helper: resolve and assert there were no parse diagnostics, then
    /// return the span result.
    fn resolve(
        source: &str,
        class: &str,
        selector: &str,
        side: MethodSide,
    ) -> Result<Span, SpanResolveError> {
        let (result, diagnostics) = resolve_method_span(source, class, selector, side);
        assert!(
            diagnostics.is_empty(),
            "fixture should parse cleanly, got diagnostics: {diagnostics:?}"
        );
        result
    }

    #[test]
    fn resolves_instance_method() {
        let span = resolve(ATOMIC, "AtomicCounter", "increment", MethodSide::Instance)
            .expect("increment should resolve");
        let text = &ATOMIC[span.as_range()];
        assert!(text.starts_with("increment -> Integer =>"), "got: {text:?}");
        assert!(
            text.ends_with('\n'),
            "span should include trailing newline: {text:?}"
        );
        // No-op splice is identity.
        assert_eq!(splice(ATOMIC, span, text), ATOMIC);
    }

    #[test]
    fn resolves_keyword_method() {
        let span = resolve(
            ATOMIC,
            "AtomicCounter",
            "incrementBy:",
            MethodSide::Instance,
        )
        .expect("incrementBy: should resolve");
        let text = &ATOMIC[span.as_range()];
        assert!(
            text.starts_with("incrementBy: n :: Integer"),
            "got: {text:?}"
        );
        // Multi-line body: the `=>` is on one line, the call on the next.
        assert!(
            text.contains("by: n"),
            "should cover full multi-line body: {text:?}"
        );
        assert!(text.ends_with('\n'));
    }

    #[test]
    fn resolves_class_method() {
        let span = resolve(ATOMIC, "AtomicCounter", "new:", MethodSide::Class)
            .expect("class new: should resolve");
        let text = &ATOMIC[span.as_range()];
        assert!(text.starts_with("class sealed new: name"), "got: {text:?}");
        assert!(text.ends_with('\n'));
    }

    #[test]
    fn instance_and_class_sides_are_distinct() {
        // `new:` only exists on the class side; asking for it as an instance
        // method must miss.
        let err = resolve(ATOMIC, "AtomicCounter", "new:", MethodSide::Instance)
            .expect_err("new: is class-side only");
        assert!(matches!(err, SpanResolveError::SelectorNotFound { .. }));
    }

    #[test]
    fn class_not_found_is_structured_error() {
        let err = resolve(ATOMIC, "NoSuchClass", "increment", MethodSide::Instance)
            .expect_err("missing class");
        assert_eq!(
            err,
            SpanResolveError::ClassNotFound {
                class: "NoSuchClass".to_string()
            }
        );
    }

    #[test]
    fn selector_not_found_is_structured_error() {
        let err = resolve(
            ATOMIC,
            "AtomicCounter",
            "noSuchSelector",
            MethodSide::Instance,
        )
        .expect_err("missing selector");
        assert_eq!(
            err,
            SpanResolveError::SelectorNotFound {
                class: "AtomicCounter".to_string(),
                selector: "noSuchSelector".to_string(),
                side: MethodSide::Instance,
            }
        );
    }

    #[test]
    fn ambiguous_duplicate_method_is_structured_error() {
        // Two methods with the same selector on the same side.
        let src = "Object subclass: Dup\n  foo => 1\n  foo => 2\n";
        let err = resolve(src, "Dup", "foo", MethodSide::Instance).expect_err("duplicate foo");
        assert_eq!(
            err,
            SpanResolveError::Ambiguous {
                class: "Dup".to_string(),
                selector: "foo".to_string(),
                side: MethodSide::Instance,
                count: 2,
            }
        );
    }

    #[test]
    fn binary_selector_method() {
        let src = "Object subclass: Vec\n  + other => self\n";
        let span = resolve(src, "Vec", "+", MethodSide::Instance).expect("+ should resolve");
        let text = &src[span.as_range()];
        assert!(text.starts_with("+ other =>"), "got: {text:?}");
        assert_eq!(splice(src, span, text), src);
    }

    #[test]
    fn last_method_without_trailing_newline_clamps_to_eof() {
        let src = "Object subclass: Tail\n  done => 42";
        let span = resolve(src, "Tail", "done", MethodSide::Instance).expect("done resolves");
        assert_eq!(span.end() as usize, src.len());
        let text = &src[span.as_range()];
        assert_eq!(splice(src, span, text), src);
    }

    #[test]
    fn doc_comment_is_excluded_from_span() {
        // The doc comment precedes the method but is not part of the span.
        let span = resolve(ATOMIC, "AtomicCounter", "value", MethodSide::Instance)
            .expect("value resolves");
        let text = &ATOMIC[span.as_range()];
        assert!(
            !text.contains("///"),
            "doc comment must be excluded: {text:?}"
        );
        assert!(text.starts_with("value -> Integer =>"));
    }

    #[test]
    fn returns_parser_diagnostics_alongside_result() {
        // A user-facing operation surfaces parse problems rather than swallowing
        // them. The class still resolves; the diagnostics carry the parse issue.
        let src = "Object subclass: Broken\n  ok => 1\n  bad => @@@\n";
        let (result, diagnostics) = resolve_method_span(src, "Broken", "ok", MethodSide::Instance);
        assert!(result.is_ok(), "ok method should still resolve: {result:?}");
        assert!(
            !diagnostics.is_empty(),
            "malformed body should produce parse diagnostics"
        );
    }

    /// Test helper: replace `span` in `source` with `replacement`.
    fn splice(source: &str, span: Span, replacement: &str) -> String {
        let mut out = String::with_capacity(source.len());
        out.push_str(&source[..span.start() as usize]);
        out.push_str(replacement);
        out.push_str(&source[span.end() as usize..]);
        out
    }
}
