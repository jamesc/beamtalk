// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Section-divider method categories (BT-2601).
//!
//! **DDD Context:** Source Analysis (shared leaf below Language Service,
//! Cockpit, REPL, and MCP — see `docs/development/architecture-principles.md`
//! § Duplication & the Shared-Leaf-Module Pattern).
//!
//! Beamtalk classes already group methods in source with a `// === Name ===`
//! divider comment (e.g. `Actor.bt`'s `// === Timeout proxy ===`). This module
//! is the single, canonical recognizer for that convention: it parses divider
//! comments and associates the methods that follow each one — until the next
//! divider or the end of the class — into a [`MethodCategory`].
//!
//! # Why a comment, not new syntax
//!
//! The category lives in the source text as the existing `// === Name ===`
//! divider — no new keyword, no per-method annotation, no sidecar / metadata
//! store. Categories are *inter-method file structure*, not a property of the
//! method object (consistent with BT-2594, which stopped the per-method
//! editor from including a divider in a method's own byte span). So they are
//! surfaced by whoever reads the **source** — this module — not by in-memory
//! reflection on a loaded class. A class object built from a running system
//! (no source in hand) carries no category; that is intentional, not a gap.
//!
//! # Divider format (locked down here for every consumer)
//!
//! A leading `//` line comment (not `///`, not `/* */`) is a divider when its
//! text, trimmed, has the shape:
//!
//! ```text
//! =+ <name> =+
//! ```
//!
//! where the leading and trailing runs of `=` are the same length (3 or more)
//! and `<name>` (trimmed) is non-empty. See [`parse_divider_name`] for the
//! exact rule, including why a pure `====...====` border line (no name) does
//! *not* count — that shape is the older, unrelated 3-line banner convention
//! seen in some test files (`// ===...===` / `// HEADING` / `// ===...===`),
//! which this module leaves alone.
//!
//! # Association rule
//!
//! Every divider comment attaches to the class member that immediately
//! follows it (an ordinary consequence of how the parser collects leading
//! comment trivia — see [`crate::source_analysis::parser`]'s
//! `collect_comment_attachment`). Walking a class's instance and class-side
//! methods in true source order (spans interleaved, not grouped by side),
//! a divider on a method starts a new category that the method — and every
//! method after it — belongs to, until the next divider or the end of the
//! class. Methods before the first divider form an implicit, unnamed
//! (`name: None`) leading category.
//!
//! Standalone `Class >> selector` extension methods (defined at module level,
//! ADR 0066) are out of scope: this module only categorizes methods declared
//! inside a class body.

use crate::ast::{ClassDefinition, Comment, CommentKind};
use crate::source_analysis::method_span;
use crate::source_analysis::{MethodSide, Span};

/// One method, categorized by its position relative to `// === Name ===`
/// dividers in the class body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CategorizedMethod {
    /// The method selector (e.g. `increment`, `at:put:`).
    pub selector: String,
    /// Which side of the class the method is defined on.
    pub side: MethodSide,
    /// The method definition's AST span.
    pub span: Span,
}

/// A run of methods sharing one source divider — or the implicit leading
/// group of methods that appear before the class's first divider.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodCategory {
    /// The divider's name, or `None` for the implicit leading group.
    pub name: Option<String>,
    /// The divider comment's own line span (including its trailing newline),
    /// when it could be located in `source`. `None` for the implicit leading
    /// group (which has no divider) and, defensively, if the divider line
    /// could not be re-found in `source` (should not happen when `source` is
    /// the same text the class was parsed from).
    pub divider_span: Option<Span>,
    /// Methods in this category, in source order.
    pub methods: Vec<CategorizedMethod>,
}

/// Parses `content` — the already-`//`-stripped text of a leading line
/// comment — as a `// === Name ===` section divider, the canonical format
/// this module locks down for every surface (LSP outline, Cockpit, REPL,
/// MCP) and that BT-2626's stdlib-wide divider curation must match exactly.
///
/// Recognized shape (`trimmed` = `content.trim()`):
/// - `trimmed` starts with a run of 3+ `=` characters and ends with a run of
///   `=` characters of the *same* length.
/// - Between the two runs (trimmed of surrounding whitespace) is a non-empty
///   name — any text, including punctuation, backticks, or parens.
///
/// Returns `None` for a plain comment or a border-only line made entirely of
/// `=` characters with no name in the middle (the pre-existing, unrelated
/// 3-line `====...====` / heading / `====...====` banner convention used in
/// some test files — deliberately not treated as a divider here).
#[must_use]
pub fn parse_divider_name(content: &str) -> Option<&str> {
    let trimmed = content.trim();
    let left_len = trimmed.chars().take_while(|&c| c == '=').count();
    if left_len < 3 {
        return None;
    }
    let right_len = trimmed.chars().rev().take_while(|&c| c == '=').count();
    if right_len != left_len {
        return None;
    }
    let total = trimmed.chars().count();
    if left_len + right_len >= total {
        // Pure border line (entirely `=`, or the two runs overlap) — no room
        // for a name.
        return None;
    }
    let name = trimmed[left_len..trimmed.len() - right_len].trim();
    if name.is_empty() { None } else { Some(name) }
}

/// Returns the divider name from a method's leading comments, if any.
///
/// Only [`CommentKind::Line`] comments are considered (never `///` doc
/// comments or `/* */` block comments). When more than one divider-shaped
/// line comment precedes the same method — unusual, but not prevented by the
/// grammar — the one nearest the method (last in source order) wins, matching
/// the natural reading that the closest heading is the current one.
fn divider_name_from_leading(leading: &[Comment]) -> Option<&str> {
    leading
        .iter()
        .filter(|c| c.kind == CommentKind::Line)
        .filter_map(|c| parse_divider_name(&c.content))
        .next_back()
}

/// Walks backward from `method_line_start` (the start of a method's own
/// header line) across intervening blank lines, `///` doc-comment lines, and
/// non-divider `//` comment lines, looking for the source line that is the
/// `expected_name` divider already identified via the method's
/// `comments.leading` (mirrors [`method_span`]'s `doc_block_start` walk, but
/// hunts for a divider line instead of a doc-comment block).
///
/// Returns the divider line's full span (including its trailing newline).
/// Returns `None` if no such line is found before reaching the start of the
/// file or a non-comment, non-blank line — defensive: `expected_name` came
/// from the same `source`'s AST, so this should always succeed, but a source
/// callers pass that doesn't match the AST it came from (e.g. a stale AST) is
/// handled without panicking.
fn find_divider_span(source: &str, method_line_start: u32, expected_name: &str) -> Option<Span> {
    let mut pos = method_line_start;
    while pos > 0 {
        let prev_line_start = method_span::line_start(source, pos - 1);
        let line = &source[prev_line_start as usize..pos as usize];
        let trimmed = line.trim_end_matches(['\n', '\r']).trim_start();

        if trimmed.is_empty() {
            pos = prev_line_start;
            continue;
        }
        if trimmed.starts_with("///") {
            pos = prev_line_start;
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("//") {
            let content = rest.strip_prefix(' ').unwrap_or(rest);
            if parse_divider_name(content) == Some(expected_name) {
                let end = method_span::extend_to_line_end(source, prev_line_start);
                return Some(Span::new(prev_line_start, end));
            }
            pos = prev_line_start;
            continue;
        }
        // Anything else (code, or the previous declaration) ends the walk.
        return None;
    }
    None
}

/// Groups a class's methods into [`MethodCategory`] runs by the
/// `// === Name ===` dividers in `source` — see the module docs for the
/// exact recognition and association rules.
///
/// `class` must have been parsed from `source` (the divider-span lookup
/// re-scans `source` around each method's header line). Instance and
/// class-side methods are merged and walked in true source order (by AST
/// span), not grouped by side, so a divider spanning both sides of a class
/// groups correctly.
#[must_use]
pub fn categorize_methods(class: &ClassDefinition, source: &str) -> Vec<MethodCategory> {
    let mut all: Vec<(&crate::ast::MethodDefinition, MethodSide)> = class
        .methods
        .iter()
        .map(|m| (m, MethodSide::Instance))
        .chain(class.class_methods.iter().map(|m| (m, MethodSide::Class)))
        .collect();
    all.sort_by_key(|(m, _)| m.span.start());

    let mut categories: Vec<MethodCategory> = Vec::new();

    for (method, side) in all {
        let categorized = CategorizedMethod {
            selector: method.selector.name().to_string(),
            side,
            span: method.span,
        };
        match divider_name_from_leading(&method.comments.leading) {
            Some(name) => {
                let method_line_start = method_span::line_start(source, method.span.start());
                let divider_span = find_divider_span(source, method_line_start, name);
                categories.push(MethodCategory {
                    name: Some(name.to_string()),
                    divider_span,
                    methods: vec![categorized],
                });
            }
            None => match categories.last_mut() {
                Some(current) => current.methods.push(categorized),
                None => categories.push(MethodCategory {
                    name: None,
                    divider_span: None,
                    methods: vec![categorized],
                }),
            },
        }
    }

    categories
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    // --- parse_divider_name ---

    #[test]
    fn recognizes_three_equals() {
        assert_eq!(
            parse_divider_name("=== Timeout proxy ==="),
            Some("Timeout proxy")
        );
    }

    #[test]
    fn recognizes_five_equals() {
        assert_eq!(
            parse_divider_name("===== Interval Math (pure, no sleeping) ====="),
            Some("Interval Math (pure, no sleeping)")
        );
    }

    #[test]
    fn rejects_mismatched_run_lengths() {
        assert_eq!(parse_divider_name("=== Name ====="), None);
        assert_eq!(parse_divider_name("===== Name ==="), None);
    }

    #[test]
    fn rejects_pure_border_line() {
        assert_eq!(parse_divider_name("========================"), None);
    }

    #[test]
    fn rejects_run_shorter_than_three() {
        assert_eq!(parse_divider_name("== Name =="), None);
        assert_eq!(parse_divider_name("= Name ="), None);
    }

    #[test]
    fn rejects_plain_comment() {
        assert_eq!(parse_divider_name("a plain comment"), None);
        assert_eq!(parse_divider_name("TODO: fix this"), None);
    }

    #[test]
    fn rejects_empty_name() {
        assert_eq!(parse_divider_name("======"), None);
    }

    #[test]
    fn name_may_contain_punctuation_and_equals() {
        assert_eq!(
            parse_divider_name("===== Element identity is `=:=`, not term order (BT-2997) ====="),
            Some("Element identity is `=:=`, not term order (BT-2997)")
        );
    }

    #[test]
    fn tolerates_extra_whitespace() {
        assert_eq!(parse_divider_name("  ===  Name  ===  "), Some("Name"));
    }

    // --- categorize_methods ---

    fn parse_class(source: &str) -> crate::ast::ClassDefinition {
        let tokens = lex_with_eof(source);
        let (module, diags) = parse(tokens);
        assert!(diags.is_empty(), "fixture should parse cleanly: {diags:?}");
        module.classes.into_iter().next().expect("one class")
    }

    #[test]
    fn no_dividers_is_single_unnamed_category() {
        let src = "Object subclass: Counter\n  foo => 1\n  bar => 2\n";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 1);
        assert_eq!(categories[0].name, None);
        assert_eq!(categories[0].methods.len(), 2);
        assert_eq!(categories[0].methods[0].selector, "foo");
        assert_eq!(categories[0].methods[1].selector, "bar");
    }

    #[test]
    fn methods_before_first_divider_are_unnamed() {
        let src = "\
Object subclass: Counter
  foo => 1

  // === Section ===

  bar => 2
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 2);
        assert_eq!(categories[0].name, None);
        assert_eq!(categories[0].methods.len(), 1);
        assert_eq!(categories[0].methods[0].selector, "foo");
        assert_eq!(categories[1].name.as_deref(), Some("Section"));
        assert_eq!(categories[1].methods.len(), 1);
        assert_eq!(categories[1].methods[0].selector, "bar");
    }

    #[test]
    fn divider_applies_to_itself_and_following_methods() {
        let src = "\
Object subclass: Counter
  // === Alpha ===
  foo => 1
  bar => 2

  // === Beta ===
  baz => 3
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 2);
        assert_eq!(categories[0].name.as_deref(), Some("Alpha"));
        assert_eq!(
            categories[0]
                .methods
                .iter()
                .map(|m| m.selector.as_str())
                .collect::<Vec<_>>(),
            vec!["foo", "bar"]
        );
        assert_eq!(categories[1].name.as_deref(), Some("Beta"));
        assert_eq!(categories[1].methods[0].selector, "baz");
    }

    #[test]
    fn divider_span_locates_the_banner_line() {
        let src = "\
Object subclass: Counter
  // === Section ===
  foo => 1
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        let span = categories[0].divider_span.expect("divider span found");
        assert_eq!(&src[span.as_range()], "  // === Section ===\n");
    }

    #[test]
    fn doc_comment_between_divider_and_method_is_skipped_over() {
        let src = "\
Object subclass: Counter
  // === Section ===

  /// Doc comment.
  foo => 1
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories[0].name.as_deref(), Some("Section"));
        let span = categories[0].divider_span.expect("divider span found");
        assert_eq!(&src[span.as_range()], "  // === Section ===\n");
    }

    #[test]
    fn interleaves_instance_and_class_side_methods_by_source_order() {
        let src = "\
Object subclass: Counter
  // === Construction ===
  class new => self new: 0

  foo => 1

  // === Instance ops ===
  bar => 2
  class other => 0
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 2);
        assert_eq!(categories[0].name.as_deref(), Some("Construction"));
        assert_eq!(
            categories[0]
                .methods
                .iter()
                .map(|m| (m.selector.as_str(), m.side))
                .collect::<Vec<_>>(),
            vec![("new", MethodSide::Class), ("foo", MethodSide::Instance)]
        );
        assert_eq!(categories[1].name.as_deref(), Some("Instance ops"));
        assert_eq!(
            categories[1]
                .methods
                .iter()
                .map(|m| (m.selector.as_str(), m.side))
                .collect::<Vec<_>>(),
            vec![("bar", MethodSide::Instance), ("other", MethodSide::Class)]
        );
    }

    #[test]
    fn three_line_banner_convention_is_not_a_divider() {
        // The pre-existing, unrelated `====...` / heading / `====...`
        // 3-line banner some test files use is deliberately not recognized
        // (BT-2601 locks down only the single-line `// === Name ===` shape).
        let src = "\
Object subclass: Counter
  // =========================================================================
  // CONSTRUCTORS
  // =========================================================================
  foo => 1
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 1);
        assert_eq!(categories[0].name, None);
    }

    #[test]
    fn trailing_divider_with_no_following_method_is_dropped() {
        // A divider is only surfaced when it precedes at least one method —
        // it attaches to the class member that follows it, and there is none
        // here, so it stays an ordinary trailing comment.
        let src = "\
Object subclass: Counter
  foo => 1
  // === Nothing follows ===
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 1);
        assert_eq!(categories[0].name, None);
        assert_eq!(categories[0].methods.len(), 1);
    }
}
