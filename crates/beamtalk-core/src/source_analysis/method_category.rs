// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Section-divider method categories (BT-2601).
//!
//! **DDD Context:** Source Analysis (shared leaf below Language Service,
//! Cockpit, REPL, and MCP — see `docs/development/architecture-principles.md`
//! § Duplication & the Shared-Leaf-Module Pattern).
//!
//! Beamtalk classes already group methods in source with a `// === Name ===`
//! divider comment (e.g. `actor.bt`'s `// === Timeout proxy ===`). This module
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
//!
//! A divider can precede *any* class member — not only a method. A
//! `state:`/`classState:` declaration between a divider and the next method
//! still lets the divider start a new category (its own leading comments are
//! checked too), but the declaration itself never becomes a
//! [`CategorizedMethod`] — matching this module's method-only scope.
//!
//! # Known limitations (BT-2601 code review)
//!
//! - **Fields are not nested into categories.** `beamtalk-language-service`'s
//!   `queries::document_symbols_provider` still renders a class's
//!   `state:`/`classState:` fields as flat, top-level
//!   children of the class symbol regardless of where dividers place them.
//!   A divider written directly above a field (rather than a method) is
//!   still recognized (see above) and its category's `span` — the divider
//!   merged with its methods' spans — can therefore numerically *contain*
//!   that field's byte range even though the field renders as a sibling, not
//!   a nested child. Cosmetic only (no data loss), tracked for the
//!   fields-in-categories follow-up alongside Cockpit/REPL/MCP grouped
//!   listing (BT-2601's PR description).
//! - **Two dividers with nothing between them:** only the nearer one is kept
//!   (see [`divider_name_from_leading`]'s doc) — the earlier one silently
//!   disappears from the outline with no diagnostic. Deliberate, not a bug:
//!   this is almost certainly a source mistake the author would notice by
//!   reading their own file.
//! - **A near-miss divider is not diagnosed.** `=== Name ====` (mismatched
//!   run lengths), `== Name ==` (too short), or a `///`/`/* */` divider-
//!   shaped comment all silently fail to parse as a divider and fall back to
//!   an ordinary leading comment — so the run of methods below it is
//!   silently absorbed into whatever category was already open, with no
//!   warning. A lint that flags a comment with `=`-runs at both ends that
//!   *isn't* a valid divider would give BT-2626's stdlib-wide curation (and
//!   any hand-written divider) a safety net; tracked as a follow-up.
//! - **A comment on the last line of a method body, with nothing but blank
//!   lines before the next declaration, is captured as the *next*
//!   declaration's leading trivia** (an inherent property of how this
//!   parser recovers comment trivia — its `collect_comment_attachment` has
//!   no way to know the comment was meant to be inside the previous method's
//!   body). If that comment happens to be divider-shaped, it starts a real
//!   category on the next member. Rare in practice (nothing else in this
//!   codebase's trivia model can distinguish the two cases either — the same
//!   ambiguity already exists for doc comments).
//! - **A declaration's own `comments.leading` can be dropped when it carries
//!   both a preceding comment block *and* an `@expect` directive** — a
//!   pre-existing parser quirk in `PendingDeclarationExpect::apply_to`
//!   (`declarations.rs`), which only transfers the `@expect` token's own
//!   comments when the declaration doesn't already have some. Predates this
//!   module and is orthogonal to divider recognition specifically, but can
//!   manifest as a divider silently vanishing in that narrow combination;
//!   tracked as a parser follow-up rather than fixed here (fixing comment-
//!   attachment merge order has a wider blast radius than this module).

use crate::ast::{ClassDefinition, Comment, CommentKind};
use crate::source_analysis::method_span;
use crate::source_analysis::{Diagnostic, MethodSide, Span, lex_with_eof, parse};

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

impl MethodCategory {
    /// The category's container span: the divider's own banner line merged
    /// with every method in the category, in source order.
    ///
    /// Shared by every consumer that needs "the range this category covers"
    /// — `beamtalk-language-service`'s `queries::document_symbols_provider`
    /// (the `DocumentSymbolKind::Category` container) and
    /// `queries::folding_range_provider` (BT-3237) both call this rather than
    /// re-deriving the merge, so outline and foldingRange always agree
    /// exactly.
    ///
    /// Returns `None` when there is neither a located `divider_span` nor any
    /// methods to merge. For a *named* category this should not happen in
    /// practice — [`categorize_methods`] only ever creates one while
    /// processing a member that starts a new category, and a divider is only
    /// surfaced when at least one method follows it — but a category can be
    /// constructed with neither in principle, so this stays `Option` rather
    /// than panicking or silently falling back to a caller-supplied default.
    #[must_use]
    pub fn span(&self) -> Option<Span> {
        self.methods
            .iter()
            .map(|m| m.span)
            .fold(self.divider_span, |acc, s| {
                Some(acc.map_or(s, |a| a.merge(s)))
            })
    }
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

/// Walks backward from `member_line_start` (the start of a method's, or a
/// state/class-variable declaration's, own header line) across intervening
/// blank lines, `///` doc-comment lines, `@expect`-style directive lines
/// (BT-2601 review: a divider directly above an `@expect`-annotated
/// declaration must still be located), and non-divider `//` comment lines,
/// looking for the source line that is the `expected_name` divider already
/// identified via the member's `comments.leading` (mirrors [`method_span`]'s
/// `doc_block_start` walk, but hunts for a divider line instead of a
/// doc-comment block).
///
/// Returns the divider line's full span (including its trailing newline).
/// Returns `None` if no such line is found before reaching the start of the
/// file or a non-comment, non-blank, non-directive line — defensive:
/// `expected_name` came from the same `source`'s AST, so this should always
/// succeed, but a source callers pass that doesn't match the AST it came
/// from (e.g. a stale AST) is handled without panicking.
fn find_divider_span(source: &str, member_line_start: u32, expected_name: &str) -> Option<Span> {
    let mut pos = member_line_start;
    while pos > 0 {
        let prev_line_start = method_span::line_start(source, pos - 1);
        let line = &source[prev_line_start as usize..pos as usize];
        let trimmed = line.trim_end_matches(['\n', '\r']).trim_start();

        if trimmed.is_empty() {
            pos = prev_line_start;
            continue;
        }
        if trimmed.starts_with("///") || trimmed.starts_with('@') {
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

/// One member of a class body in source order, for the merged walk
/// [`categorize_methods`] performs. A divider can precede *any* class member
/// — not only a method — so `state:`/`classState:` declarations must
/// still be able to start a new category (review finding, BT-2601): without
/// this, `// === Beta ===\nstate: x = 0\nbar => 2` would silently swallow
/// "Beta" and mis-attribute `bar` to whatever category preceded `state: x`.
/// A non-method member never contributes a [`CategorizedMethod`] — it only
/// gets to *start* a category boundary via its own leading comments.
enum ClassMember<'a> {
    Method(&'a crate::ast::MethodDefinition, MethodSide),
    NonMethod,
}

/// Groups a class's methods into [`MethodCategory`] runs by the
/// `// === Name ===` dividers in `source` — see the module docs for the
/// exact recognition and association rules.
///
/// `class` must have been parsed from `source` (the divider-span lookup
/// re-scans `source` around each member's header line). Instance methods,
/// class-side methods, state declarations (`state:`), and class variables
/// (`classState:`) are all merged and walked in true source order (by AST
/// span), not grouped by kind, so a divider spanning both sides of a class —
/// or one written directly above a field rather than a method — still groups
/// correctly. Only methods are ever collected into a category's `methods`;
/// state/class-variable declarations merely establish where a new category
/// begins, matching BT-2601's scope (categories group *methods*).
#[must_use]
pub fn categorize_methods(class: &ClassDefinition, source: &str) -> Vec<MethodCategory> {
    let mut all: Vec<(Span, &[Comment], ClassMember)> = class
        .state
        .iter()
        .map(|s| {
            (
                s.span,
                s.comments.leading.as_slice(),
                ClassMember::NonMethod,
            )
        })
        .chain(class.class_variables.iter().map(|s| {
            (
                s.span,
                s.comments.leading.as_slice(),
                ClassMember::NonMethod,
            )
        }))
        .chain(class.methods.iter().map(|m| {
            (
                m.span,
                m.comments.leading.as_slice(),
                ClassMember::Method(m, MethodSide::Instance),
            )
        }))
        .chain(class.class_methods.iter().map(|m| {
            (
                m.span,
                m.comments.leading.as_slice(),
                ClassMember::Method(m, MethodSide::Class),
            )
        }))
        .collect();
    all.sort_by_key(|(span, ..)| span.start());

    let mut categories: Vec<MethodCategory> = Vec::new();

    for (span, leading, member) in all {
        if let Some(name) = divider_name_from_leading(leading) {
            let member_line_start = method_span::line_start(source, span.start());
            let divider_span = find_divider_span(source, member_line_start, name);
            categories.push(MethodCategory {
                name: Some(name.to_string()),
                divider_span,
                methods: Vec::new(),
            });
        }
        let ClassMember::Method(method, side) = member else {
            continue;
        };
        let categorized = CategorizedMethod {
            selector: method.selector.name().to_string(),
            side,
            span,
        };
        match categories.last_mut() {
            Some(current) => current.methods.push(categorized),
            None => categories.push(MethodCategory {
                name: None,
                divider_span: None,
                methods: vec![categorized],
            }),
        }
    }

    categories
}

/// Why [`categorize_methods_in_source`] could not locate `class` in `source`.
/// Mirrors [`crate::source_analysis::ClassSpanResolveError`]'s shape (same
/// "find the one class definition named `class`" lookup, reused here rather
/// than shared directly since the two resolvers return different success
/// payloads and `ClassSpanResolveError` is not `Copy`-free to repurpose
/// across crate-internal call sites without adding a dependency edge).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CategorizeMethodsError {
    /// No class named `class` was found in `source`.
    ClassNotFound {
        /// The class name that was searched for.
        class: String,
    },
    /// More than one class definition matched `class`.
    Ambiguous {
        /// The class name that matched more than once.
        class: String,
        /// How many definitions matched.
        count: usize,
    },
}

impl std::fmt::Display for CategorizeMethodsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CategorizeMethodsError::ClassNotFound { class } => {
                write!(f, "class `{class}` not found in source")
            }
            CategorizeMethodsError::Ambiguous { class, count } => {
                write!(f, "class `{class}` is ambiguous ({count} definitions)")
            }
        }
    }
}

impl std::error::Error for CategorizeMethodsError {}

/// Parses `source` and categorizes the named `class`'s methods by its
/// `// === Name ===` section dividers (BT-3239) — the entry point for a
/// caller that has only source text and a class name in hand, not an
/// already-parsed [`ClassDefinition`] (e.g. the compiler-port bridge that
/// lets the Erlang REPL/workspace reach this module without reimplementing
/// its recognition grammar — see this module's doc for why that
/// reimplementation must never happen).
///
/// Mirrors [`crate::source_analysis::resolve_class_span`]'s own
/// parse-then-find-the-one-matching-class shape. Parser [`Diagnostic`]s are
/// returned alongside the result, same convention as that resolver — a
/// caller that only needs the categories, not the parse health, may ignore
/// them.
pub fn categorize_methods_in_source(
    source: &str,
    class: &str,
) -> (
    Result<Vec<MethodCategory>, CategorizeMethodsError>,
    Vec<Diagnostic>,
) {
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    let matches: Vec<&ClassDefinition> = module
        .classes
        .iter()
        .filter(|class_def| class_def.name.name.as_str() == class)
        .collect();
    let result = match matches.len() {
        0 => Err(CategorizeMethodsError::ClassNotFound {
            class: class.to_string(),
        }),
        1 => Ok(categorize_methods(matches[0], source)),
        count => Err(CategorizeMethodsError::Ambiguous {
            class: class.to_string(),
            count,
        }),
    };
    (result, diagnostics)
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn tolerates_no_whitespace_around_name() {
        // BT-3261: no whitespace requirement here — only that the trimmed name
        // between the two `=` runs be non-empty. The TextMate grammar in
        // editors/vscode/syntaxes/beamtalk.tmLanguage.json used to require
        // `\s+` on both sides and so missed this shape (a false negative,
        // safe direction — see that file's `comment.line.double-slash.
        // section-divider.beamtalk` pattern comment and the shared
        // conformance fixture at
        // tests/fixtures/section_divider_grammar_cases.json, also checked by
        // `tests/section_divider_grammar_conformance.rs`).
        assert_eq!(parse_divider_name("===Name==="), Some("Name"));
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

    #[test]
    fn divider_above_a_state_declaration_still_starts_a_new_category() {
        // Review finding: a divider can precede *any* class member, not only
        // a method. Before this was handled, the divider comment attached to
        // `state: x = 0` (a non-method declaration `categorize_methods`
        // never inspected) was silently dropped, and `bar` was
        // mis-attributed to the *previous* category ("Alpha") instead of
        // starting a fresh, correctly-named one ("Beta").
        let src = "\
Object subclass: Counter
  // === Alpha ===
  foo => 1

  // === Beta ===
  state: x = 0

  bar => 2
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 2);
        assert_eq!(categories[0].name.as_deref(), Some("Alpha"));
        assert_eq!(categories[0].methods.len(), 1);
        assert_eq!(categories[0].methods[0].selector, "foo");
        assert_eq!(categories[1].name.as_deref(), Some("Beta"));
        assert_eq!(categories[1].methods.len(), 1);
        assert_eq!(categories[1].methods[0].selector, "bar");
    }

    #[test]
    fn divider_above_a_class_variable_still_starts_a_new_category() {
        let src = "\
Object subclass: Counter
  // === Shared state ===
  classState: total :: Integer = 0

  foo => 1
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 1);
        assert_eq!(categories[0].name.as_deref(), Some("Shared state"));
        assert_eq!(categories[0].methods[0].selector, "foo");
    }

    #[test]
    fn a_state_declaration_with_no_divider_does_not_break_the_current_category() {
        // The common case (most classes declare state with no divider
        // directly above it) must keep working exactly as before: the state
        // declaration is invisible to categorization and the surrounding
        // methods stay in whichever category was already open.
        let src = "\
Object subclass: Counter
  // === Section ===
  foo => 1
  state: x = 0
  bar => 2
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories.len(), 1);
        assert_eq!(categories[0].name.as_deref(), Some("Section"));
        assert_eq!(
            categories[0]
                .methods
                .iter()
                .map(|m| m.selector.as_str())
                .collect::<Vec<_>>(),
            vec!["foo", "bar"]
        );
    }

    #[test]
    fn expect_directive_between_divider_and_method_does_not_break_span_lookup() {
        // Review finding: `find_divider_span`'s backward walk must skip over
        // an `@expect` directive line the same way it skips blank lines and
        // `///` doc comments — otherwise the divider's own line can't be
        // relocated and `divider_span` silently degrades to `None`.
        let src = "\
Object subclass: Counter
  // === Section ===
  @expect type
  foo -> Integer => 1
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories[0].name.as_deref(), Some("Section"));
        let span = categories[0]
            .divider_span
            .expect("divider span located past the @expect line");
        assert_eq!(&src[span.as_range()], "  // === Section ===\n");
    }

    // --- MethodCategory::span ---

    #[test]
    fn span_merges_divider_and_every_method_in_the_category() {
        let src = "\
Object subclass: Counter
  // === Section ===
  bar => 2
  baz => 3
";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        let span = categories[0].span().expect("named category has a span");
        // Starts at the divider's own banner line, ends at the last method.
        assert!(&src[span.as_range()].starts_with("  // === Section ==="));
        assert!(&src[span.as_range()].trim_end().ends_with("baz => 3"));
    }

    #[test]
    fn span_of_unnamed_leading_category_merges_only_its_methods() {
        // The implicit leading category has no divider line, so its span is
        // just the merge of its methods' own spans.
        let src = "Object subclass: Counter\n  foo => 1\n  bar => 2\n";
        let class = parse_class(src);
        let categories = categorize_methods(&class, src);
        assert_eq!(categories[0].name, None);
        let span = categories[0]
            .span()
            .expect("unnamed category still has methods to merge");
        let text = &src[span.as_range()];
        assert!(text.starts_with("foo => 1"));
        assert!(text.ends_with("bar => 2"));
    }

    // --- categorize_methods_in_source (BT-3239) ---

    #[test]
    fn categorize_methods_in_source_finds_the_named_class() {
        let src = "\
Object subclass: Counter
  // === Alpha ===
  foo => 1

  // === Beta ===
  bar => 2
";
        let (result, diagnostics) = categorize_methods_in_source(src, "Counter");
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let categories = result.expect("Counter is present");
        assert_eq!(categories.len(), 2);
        assert_eq!(categories[0].name.as_deref(), Some("Alpha"));
        assert_eq!(categories[1].name.as_deref(), Some("Beta"));
    }

    #[test]
    fn categorize_methods_in_source_no_dividers_is_single_unnamed_category() {
        let src = "Object subclass: Counter\n  foo => 1\n  bar => 2\n";
        let (result, _diagnostics) = categorize_methods_in_source(src, "Counter");
        let categories = result.expect("Counter is present");
        assert_eq!(categories.len(), 1);
        assert_eq!(categories[0].name, None);
    }

    #[test]
    fn categorize_methods_in_source_class_not_found_is_structured_error() {
        let src = "Object subclass: Counter\n  foo => 1\n";
        let (result, _diagnostics) = categorize_methods_in_source(src, "NoSuchClass");
        assert_eq!(
            result,
            Err(CategorizeMethodsError::ClassNotFound {
                class: "NoSuchClass".to_string()
            })
        );
    }

    #[test]
    fn categorize_methods_in_source_ambiguous_class_is_structured_error() {
        let src = "\
Object subclass: Counter
  foo => 1

Object subclass: Counter
  bar => 2
";
        let (result, _diagnostics) = categorize_methods_in_source(src, "Counter");
        assert_eq!(
            result,
            Err(CategorizeMethodsError::Ambiguous {
                class: "Counter".to_string(),
                count: 2
            })
        );
    }
}
