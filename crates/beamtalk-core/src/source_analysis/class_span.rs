// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Byte-span resolver for a class's header + state declarations (ADR 0082
//! extension, BT-3248).
//!
//! **DDD Context:** Source Analysis (Compilation context per ADR 0082).
//!
//! Given the source text of a `.bt` file and a target class name,
//! [`resolve_class_span`] returns the byte span of that class's declaration
//! line through its last `state:`/`field:` declaration — **never** its
//! methods.
//!
//! # Why this exists, and why it stops before the methods
//!
//! The cockpit `:def` tab's "Compile" action (BT-3248) recompiles a small,
//! *synthesized* skeleton of an already-loaded class:
//! `beamtalk_repl_ops_browse:class_definition_text/3` builds it purely from
//! runtime reflection — `{Superclass} subclass: {Name}` plus one
//! `state: {field} = {default}` line per instance variable — and explicitly
//! carries **no method bodies** ("Pharo convention", per that function's own
//! doc). This resolver exists to compute the byte range in the on-disk file
//! that a `Workspace flush` of that skeleton is allowed to touch: this
//! module's own [`resolve_class_in_module`] doc and test suite are the
//! record of *why* the span must never reach past the last state declaration.
//!
//! Naively using the whole `ClassDefinition::span` (header through the last
//! *method*, which is what a first draft of this resolver did) would make a
//! flush splice the header+state skeleton over the header+state+**all
//! methods** region — permanently erasing every method's source from the
//! file on disk. That bug was caught in review before it shipped (BT-3248);
//! this module's span deliberately ends at the last `state:`/`field:`
//! declaration (or the header line itself, when there is none) specifically
//! so a flush can never reach a method.
//!
//! This span is currently used only for the CHANGES-dock diff
//! (`beamtalk_workspace_changelog:disk_class_body/2`) — a read-only
//! disk-vs-memory comparison — not for an actual `Workspace flush` splice.
//! `beamtalk_repl_loader:add_class_def_flushability/2` marks every
//! `'class-def'` `ChangeEntry` `flushable: false` unconditionally: even with
//! this corrected span, the synthesized skeleton also drops modifier
//! keywords (`sealed`/`typed`/`abstract`), the `field:`/`state:` keyword
//! choice, and `::` type annotations (see that function's doc and
//! `class_definition_text/3`'s own construction) — so splicing it into disk
//! today would still silently downgrade a class's declaration even with a
//! byte-perfect span. Safe flush support needs the skeleton itself fixed
//! first (tracked as a follow-up); this resolver is the piece of that future
//! work that is safe to land now, because it is read-only until then.
//!
//! # Span boundaries — also EXCLUDES the doc comment
//!
//! Unlike [`crate::source_analysis::method_span::resolve_method_span`], which
//! pulls a method's leading `///` doc comment into its span (BT-2577 — the
//! method editor round-trips the whole definition, doc comment included),
//! this resolver's span starts at the class's own declaration line and never
//! backs up across `///` lines. The `:def` tab's skeleton never carries the
//! doc comment either (it is a separate, independently-fetched surface —
//! `browse_class_definition/1`'s `comment` field, rendered as "Class
//! comment" per `workspace_live.ex`'s `doc_summary_label/1`).
//!
//! - **start**: the beginning of the class's own declaration line (including
//!   indentation and any leading modifier keywords, which are part of
//!   `ClassDefinition::span`). A file-level license header, a plain `//`
//!   comment, or the class's own `///` doc comment above it are never pulled
//!   in.
//! - **end**: when the class declares at least one `state:`/`field:`, the
//!   byte immediately after the trailing newline of the *last* declaration's
//!   own line. When it declares none, the end of the declaration line itself
//!   (the header is always a single line by this codebase's convention —
//!   every example in `stdlib/src/*.bt` is one line, however many modifier
//!   keywords or type parameters it carries).
//!
//! Splicing `source[span]` back into `source` at `span` is an exact no-op,
//! same guarantee as the method-span resolver — and, unlike a first version
//! of this module, is also guaranteed to never include a method's bytes.

use crate::ast::ClassDefinition;
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

/// Resolve the byte span of `class`'s declaration line through its last
/// `state:`/`field:` declaration in `source` (ADR 0082 extension, BT-3248).
///
/// See the module doc for why this stops before any method — it is the
/// load-bearing property of this resolver. Parser [`Diagnostic`]s produced
/// while parsing `source` are returned alongside the result, same convention
/// as `resolve_method_span`.
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
    let matches: Vec<&ClassDefinition> = module
        .classes
        .iter()
        .filter(|class_def| class_def.name.name.as_str() == class)
        .collect();

    match matches.len() {
        0 => Err(ClassSpanResolveError::ClassNotFound {
            class: class.to_string(),
        }),
        1 => Ok(class_header_and_state_span(source, matches[0])),
        count => Err(ClassSpanResolveError::Ambiguous {
            class: class.to_string(),
            count,
        }),
    }
}

/// Computes the header+state span for `class_def`. See the module doc's
/// "Span boundaries" section — this is the one function responsible for the
/// "never reaches a method" guarantee the whole module exists for.
fn class_header_and_state_span(source: &str, class_def: &ClassDefinition) -> Span {
    let header_line_start = line_start(source, class_def.span.start());
    let end = match class_def.state.last() {
        Some(last_state) => extend_to_line_end(source, last_state.span.end()),
        // No state declarations: the span is just the (single-line, by
        // convention) header line itself.
        None => extend_to_line_end(source, header_line_start),
    };
    Span::new(header_line_start, end)
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
    fn resolves_class_with_no_state_or_methods() {
        let source = "Object subclass: Counter\n";
        let (result, _diags) = resolve_class_span(source, "Counter");
        let span = result.expect("class should resolve");
        assert_eq!(&source[span.start() as usize..span.end() as usize], source);
    }

    #[test]
    fn resolves_header_and_state_only() {
        let source = "Object subclass: Counter\n  state: count = 0\n  state: step = 1\n";
        let (result, _diags) = resolve_class_span(source, "Counter");
        let span = result.expect("class should resolve");
        assert_eq!(&source[span.start() as usize..span.end() as usize], source);
    }

    /// The load-bearing guarantee this whole module exists for: a class with
    /// methods must have a span that stops at the last state declaration, so
    /// splicing a header+state-only replacement into it can never touch —
    /// let alone delete — a method's source.
    #[test]
    fn excludes_methods_even_though_they_share_the_class_ast_span() {
        let source = "Object subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n\n  class new => self basicNew\n";
        let (result, _diags) = resolve_class_span(source, "Counter");
        let span = result.expect("class should resolve");
        let slice = &source[span.start() as usize..span.end() as usize];
        assert_eq!(slice, "Object subclass: Counter\n  state: count = 0\n");
        assert!(!slice.contains("increment"));
        assert!(!slice.contains("class new"));
    }

    #[test]
    fn excludes_methods_when_class_has_no_state_at_all() {
        let source = "Object subclass: Greeter\n  greet => 'hello'\n";
        let (result, _diags) = resolve_class_span(source, "Greeter");
        let span = result.expect("class should resolve");
        let slice = &source[span.start() as usize..span.end() as usize];
        assert_eq!(slice, "Object subclass: Greeter\n");
        assert!(!slice.contains("greet"));
    }

    #[test]
    fn excludes_license_header_and_doc_comment() {
        // Unlike a method's span, the class span must NOT pull in the leading
        // `///` doc comment — see the module doc: the cockpit `:def` tab
        // never resubmits the doc comment either.
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
    fn splicing_a_new_definition_preserves_doc_comment_and_methods_on_disk() {
        // The end-to-end guarantee the exclusions above exist for: splicing a
        // `:def`-tab-style replacement (no doc comment, no methods) into the
        // resolved span must leave both untouched.
        let source = "/// Original doc.\nObject subclass: Counter\n  state: count = 0\n\n  increment => self.count := self.count + 1\n";
        let (result, _diags) = resolve_class_span(source, "Counter");
        let span = result.expect("class should resolve");
        let spliced = splice(
            source,
            span,
            "Object subclass: Counter\n  state: count = 1\n",
        );
        assert_eq!(
            spliced,
            "/// Original doc.\nObject subclass: Counter\n  state: count = 1\n\n  increment => self.count := self.count + 1\n"
        );
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
