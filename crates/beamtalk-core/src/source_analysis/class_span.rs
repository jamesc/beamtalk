// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Byte-span resolver for whole class definitions (ADR 0082 extension, BT-3248).
//!
//! **DDD Context:** Source Analysis (Compilation context per ADR 0082).
//!
//! Given the source text of a `.bt` file and a target class name,
//! [`resolve_class_span`] returns the exact byte span of that class's own
//! declaration and body — from the first token of the class's declaration
//! line through the trailing newline that terminates its last body line.
//!
//! # Why this exists
//!
//! [`crate::source_analysis::method_span::resolve_method_span`] resolves a
//! single *method*'s byte span so `Workspace flush` can splice a patched
//! method body into a `.bt` file (ADR 0082 Phase 0). Redefining an *existing*
//! class's shape (the cockpit `:def` tab's "Compile" action, BT-3248) needs
//! the same byte-span-replacement strategy applied to the *class's own
//! header + state declaration* — the exact same "verbatim splice, no AST
//! reprint" argument applies, and reusing a second whole-class-install
//! mechanism was already ruled out for `Workspace changes revert:` on a
//! `'remove-class'` entry (see `beamtalk_repl_loader:revert_remove_class/2`'s
//! doc) for the same reason: don't duplicate a chokepoint that already exists.
//!
//! # Span boundaries — deliberately EXCLUDES the doc comment
//!
//! Unlike [`resolve_method_span`], which pulls a method's leading `///` doc
//! comment into the span (BT-2577 — the method editor round-trips the whole
//! definition, doc comment included), this resolver's span starts at the
//! class's own declaration line and never backs up across `///` lines. The
//! cockpit `:def` tab edits only the class's header + state declarations —
//! its doc comment is a separate, independently-fetched/edited surface
//! (`beamtalk_repl_ops_browse:browse_class_definition/1`'s `comment` field,
//! rendered as "Class comment" — see `workspace_live.ex`'s
//! `doc_summary_label/1`), never part of what `:def` submits back to compile.
//! Including the doc comment in the span, as a method's does, would make a
//! flush of an ordinary `:def` edit silently delete the class's doc comment
//! from disk (the spliced-in replacement text has none) — caught by this
//! module's own `preserves_leading_doc_comment` test.
//!
//! - **start**: the beginning of the class's own declaration line (including
//!   indentation and any leading modifier keywords like `sealed`/`typed`,
//!   which are part of `ClassDefinition::span`). A file-level license header,
//!   a plain `//` comment, or the class's own `///` doc comment above it are
//!   never pulled in.
//! - **end**: the byte immediately after the trailing newline terminating the
//!   last source line of the class body (clamped to EOF if the file has no
//!   trailing newline).
//!
//! Splicing `source[span]` back into `source` at `span` is therefore an exact
//! no-op, same guarantee as the method-span resolver.

use crate::source_analysis::method_span::{extend_to_line_end, line_start};
use crate::source_analysis::{Diagnostic, Span, lex_with_eof, parse};

/// Why a class span could not be resolved. Never panics on well-formed or
/// malformed input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassSpanResolveError {
    /// No class with the requested name was found in the source.
    ClassNotFound {
        /// The class name that was searched for.
        class: String,
    },
    /// More than one class definition matched the requested name — the
    /// source is malformed (duplicate class declarations) or the resolver
    /// cannot disambiguate.
    Ambiguous {
        /// The class name that matched more than once.
        class: String,
        /// How many definitions matched.
        count: usize,
    },
}

impl std::fmt::Display for ClassSpanResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClassSpanResolveError::ClassNotFound { class } => {
                write!(f, "class `{class}` not found in source")
            }
            ClassSpanResolveError::Ambiguous { class, count } => {
                write!(f, "class `{class}` is ambiguous ({count} definitions)")
            }
        }
    }
}

impl std::error::Error for ClassSpanResolveError {}

/// Resolve the exact byte span of `class`'s whole definition in `source`
/// (ADR 0082 extension, BT-3248).
///
/// Backs the `:def` tab's live-patch install hook for an *existing* class:
/// given the current on-disk source of a `.bt` file and a target class name,
/// resolve the byte span of that class's definition (header, state
/// declarations, and body) so the install hook can record it — and a later
/// `Workspace flush` can splice the redefined class back in by byte-span
/// replacement — mirroring [`resolve_method_span`]'s contract exactly.
///
/// Parser [`Diagnostic`]s produced while parsing `source` are returned
/// alongside the result, same convention as `resolve_method_span`.
///
/// [`resolve_method_span`]: crate::source_analysis::method_span::resolve_method_span
#[must_use]
pub fn resolve_class_span(
    source: &str,
    class: &str,
) -> (Result<Span, ClassSpanResolveError>, Vec<Diagnostic>) {
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    let result = resolve_class_in_module(&module, source, class);
    (result, diagnostics)
}

fn resolve_class_in_module(
    module: &crate::ast::Module,
    source: &str,
    class: &str,
) -> Result<Span, ClassSpanResolveError> {
    let matches: Vec<Span> = module
        .classes
        .iter()
        .filter(|class_def| class_def.name.name.as_str() == class)
        .map(|class_def| class_def.span)
        .collect();

    match matches.len() {
        0 => Err(ClassSpanResolveError::ClassNotFound {
            class: class.to_string(),
        }),
        1 => Ok(class_definition_span(source, matches[0])),
        count => Err(ClassSpanResolveError::Ambiguous {
            class: class.to_string(),
            count,
        }),
    }
}

/// Computes the definition span for a class whose AST span is `class_span`.
///
/// Deliberately does NOT back up across a leading `///` doc-comment block the
/// way [`crate::source_analysis::method_span`]'s `definition_span` does for a
/// method — see the module doc's "Span boundaries" section for why. The
/// start is simply the beginning of the class's own declaration line; the end
/// extends past the trailing newline of the last body line (ADR 0082).
fn class_definition_span(source: &str, class_span: Span) -> Span {
    let start = line_start(source, class_span.start());
    let end = extend_to_line_end(source, class_span.end());
    Span::new(start, end)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn splice(source: &str, span: Span, replacement: &str) -> String {
        let mut out = String::new();
        out.push_str(&source[..span.start() as usize]);
        out.push_str(replacement);
        out.push_str(&source[span.end() as usize..]);
        out
    }

    #[test]
    fn resolves_simple_class() {
        let source = "Object subclass: Counter\n  state: count = 0\n";
        let (result, _diags) = resolve_class_span(source, "Counter");
        let span = result.expect("class should resolve");
        assert_eq!(&source[span.start() as usize..span.end() as usize], source);
    }

    #[test]
    fn excludes_license_header_and_doc_comment() {
        // Unlike a method's span, the class span must NOT pull in the leading
        // `///` doc comment — see the module doc's "Span boundaries" section:
        // the cockpit `:def` tab never resubmits the doc comment, so
        // including it here would make a flush delete it from disk.
        let source = "// Copyright 2026\n// SPDX-License-Identifier: Apache-2.0\n\n/// A counter.\nObject subclass: Counter\n  state: count = 0\n";
        let (result, _diags) = resolve_class_span(source, "Counter");
        let span = result.expect("class should resolve");
        let slice = &source[span.start() as usize..span.end() as usize];
        assert!(
            slice.starts_with("Object subclass: Counter"),
            "got: {slice:?}"
        );
        assert!(!slice.contains("Copyright"));
        assert!(!slice.contains("A counter."));
    }

    #[test]
    fn splicing_a_new_definition_preserves_leading_doc_comment_on_disk() {
        // The end-to-end guarantee the exclusion above exists for: splicing a
        // `:def`-tab-style replacement (no doc comment) into the resolved
        // span must leave the file's doc comment untouched.
        let source = "/// Original doc.\nObject subclass: Counter\n  state: count = 0\n";
        let (result, _diags) = resolve_class_span(source, "Counter");
        let span = result.expect("class should resolve");
        let spliced = splice(
            source,
            span,
            "Object subclass: Counter\n  state: count = 1\n",
        );
        assert_eq!(
            spliced,
            "/// Original doc.\nObject subclass: Counter\n  state: count = 1\n"
        );
    }

    #[test]
    fn preserves_trailing_content_outside_span() {
        let source = "Object subclass: Counter\n  state: count = 0\n";
        let (result, _diags) = resolve_class_span(source, "Counter");
        let span = result.expect("class should resolve");
        let spliced = splice(
            source,
            span,
            "Object subclass: Counter\n  field: count = 0\n",
        );
        assert_eq!(spliced, "Object subclass: Counter\n  field: count = 0\n");
    }

    #[test]
    fn class_not_found_is_structured_error() {
        let source = "Object subclass: Counter\n  state: count = 0\n";
        let (result, _diags) = resolve_class_span(source, "Missing");
        assert_eq!(
            result,
            Err(ClassSpanResolveError::ClassNotFound {
                class: "Missing".to_string()
            })
        );
    }
}
