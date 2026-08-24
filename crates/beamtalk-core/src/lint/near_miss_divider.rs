// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: flag a `=`-bordered comment that looks like it's trying to be a
//! `// === Name ===` section divider but doesn't actually parse as one
//! (BT-3240).
//!
//! `source_analysis::method_category::parse_divider_name` recognizes exactly
//! `// === Name ===`: a plain `//` line comment with symmetric `=`-runs of 3
//! or more around a non-empty name. A **near-miss** — mismatched run lengths
//! (`// === Name ====`), too-short runs (`// == Name ==`), or using `///` /
//! `/* */` instead of a plain `//` line comment — silently fails to parse as
//! a divider and falls back to an ordinary leading comment, with no
//! diagnostic anywhere in the pipeline (see that function's module doc,
//! "Known limitations", BT-2601). The methods that follow are then silently
//! absorbed into whichever category was already open, rather than starting
//! the section the author intended — this whole module is BT-3240, the
//! fix for that gap.
//!
//! This check calls [`parse_divider_name`] directly as the single source of
//! truth for "is this a valid divider" — it never reimplements that
//! decision. It only adds the extra (purely structural) bookkeeping needed
//! to: build a useful diagnostic message explaining *why* the parse failed,
//! and to leave the unrelated pure-border `// ===...===` banner convention
//! (233+ occurrences across the stdlib/test corpus, see
//! [`parse_divider_name`]'s own doc) alone — that shape has no name at all,
//! so it was never attempting to be a `// === Name ===` divider in the first
//! place.
//!
//! # Source-text scan, not an AST-walking [`LintPass`]
//!
//! Unlike every sibling pass in this directory, this check is not a
//! [`LintPass`] implementor walking the `Module` — it's [`scan_source`], a
//! source-text scan re-deriving comments directly from raw lines. That's
//! deliberate: [`Comment::span`] in the AST is **not** the comment's own
//! source location. `source_analysis::parser`'s `collect_comment_attachment`
//! stamps every leading comment with the *following declaration's* token
//! span — there is no per-comment span anywhere in the lexer's `Trivia`
//! representation to draw from — so a diagnostic built from `comment.span`
//! would point at the member the comment precedes (e.g. the method name),
//! not the comment's own line. `method_category::find_divider_span` solves
//! the identical "which line is this really" problem for a *valid* divider
//! by re-scanning `source`, but it matches by re-parsing for an
//! already-confirmed name — a near-miss, by definition, never parses, so
//! that helper can't be reused here.
//!
//! [`scan_source`] solves this by re-deriving comments directly from raw
//! source lines, so every diagnostic gets an accurate, comment-sized span —
//! and, as a side effect, also catches a `///`-shaped near-miss written
//! immediately above a method with no blank line, which the AST swallows
//! whole into that method's plain-`String` `doc_comment` field (no
//! `Comment`, no span, invisible to an AST walk). [`crate::lint::check_near_miss_dividers`]
//! is the thin, `pub` wrapper every caller actually reaches: BT-3240 wired
//! it into `queries::diagnostic_provider` (the LSP-facing pipeline); BT-3257
//! threaded `source: &str` through `beamtalk lint`'s `collect_diagnostics`
//! and MCP's `run_module_analysis` so those two surfaces call it too,
//! instead of a since-removed AST-based `NearMissDividerPass` that shared
//! this file's accept/reject decision ([`check_comment_text`]) but inherited
//! the AST's span imprecision. All three callers now agree, by construction,
//! on both *whether* something is a near-miss and *where* it is.

use crate::ast::CommentKind;
use crate::source_analysis::{Diagnostic, Span, parse_divider_name};

/// Scans `source` for a single-line `//`, `///`, or `/* ... */` comment that
/// looks like it's trying to be a `// === Name ===` divider but doesn't
/// parse as one, returning one [`Diagnostic`] per near-miss with an
/// accurate, comment-sized span.
///
/// Scope is deliberately not limited to class-body member comments — a
/// plain text scan has no notion of "inside a class"; a near-miss anywhere
/// in the file (top-level, inside a method body, before a protocol
/// signature) is still worth flagging, and the corpus proves this produces
/// no false positives today (zero near-misses anywhere in
/// `stdlib/`/`examples/`). Multi-line `/* ... */` block comments are out of
/// scope — a divider is inherently a one-line construct, so a genuine
/// attempt is always on one line; a block comment whose opening line has no
/// closing `*/` is skipped rather than misidentified, and (BT-3240 review)
/// every line up to its matching `*/` is skipped too — otherwise a `// ...`
/// line of ordinary commentary *inside* a real block comment would be
/// misread as a genuine `//` line comment the lexer never sees as one.
#[must_use]
pub(crate) fn scan_source(source: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut offset: u32 = 0;
    // Tracks whether the line just processed opened a `/* ...` with no
    // matching `*/` on the same line — every line until (and including) the
    // one that closes it belongs to that block comment's body, not to
    // top-level source, and must never be classified on its own.
    let mut in_block_comment = false;
    for line in source.split_inclusive('\n') {
        let line_len = u32::try_from(line.len()).unwrap_or(u32::MAX);
        let line_span = Span::new(offset, offset + line_len);
        offset += line_len;

        // Full `.trim()` (not just leading whitespace / the trailing
        // newline): a `/* ... */` block comment's closing `*/` must be
        // matched by `strip_suffix` below, which would silently fail to
        // recognize the line if trailing spaces before the newline were
        // left in place.
        let trimmed = line.trim();

        if in_block_comment {
            if trimmed.contains("*/") {
                in_block_comment = false;
            }
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("/*") {
            if !rest.contains("*/") {
                in_block_comment = true;
                continue;
            }
        }

        let Some((kind, content)) = classify_comment_line(trimmed) else {
            continue;
        };
        if let Some(diagnostic) = check_comment_text(content, kind, line_span) {
            diagnostics.push(diagnostic);
        }
    }
    diagnostics
}

/// Classifies a trimmed source line as a single-line `//`, `///`, or
/// `/* ... */` comment, returning its kind and inner content with
/// delimiters stripped — mirroring exactly how `source_analysis::parser`'s
/// `collect_comment_attachment` strips `// ` / `///` / `/* ... */` when
/// building [`Comment::content`], so the extracted text matches what the
/// AST path would see for the same line. Returns `None` for a non-comment
/// line, `//!`, a `////...` run (four-or-more slashes is a plain line
/// comment per the lexer's `lex_doc_comment` doc — "exactly three slashes"
/// counts as a doc comment), and a block comment with no closing `*/` on
/// the same line.
fn classify_comment_line(trimmed: &str) -> Option<(CommentKind, &str)> {
    if trimmed.starts_with("//!") {
        return None;
    }
    if let Some(rest) = trimmed.strip_prefix("///") {
        if rest.starts_with('/') {
            let rest = trimmed.strip_prefix("//")?;
            return Some((CommentKind::Line, strip_one_leading_space(rest)));
        }
        return Some((CommentKind::Doc, strip_one_leading_space(rest)));
    }
    if let Some(rest) = trimmed.strip_prefix("//") {
        return Some((CommentKind::Line, strip_one_leading_space(rest)));
    }
    if let Some(rest) = trimmed.strip_prefix("/*") {
        let inner = rest.strip_suffix("*/")?;
        let inner = strip_one_leading_space(inner);
        let inner = inner.strip_suffix(' ').unwrap_or(inner);
        return Some((CommentKind::Block, inner));
    }
    None
}

fn strip_one_leading_space(s: &str) -> &str {
    s.strip_prefix(' ').unwrap_or(s)
}

/// A comment's trimmed text, split into `(left =-run length, right =-run
/// length, name)` when it "looks divider-shaped": a non-empty run of `=` at
/// *both* ends with some non-blank text strictly between them.
///
/// Returns `None` for a plain comment (no `=` at either end — e.g. `x = y`,
/// the false-positive case this lint must never flag) and, just as
/// importantly, for the unrelated pure border-line banner (`===...===`, all
/// `=`, no name — the older 3-line `border`/`HEADING`/`border` convention
/// used throughout the corpus): that shape was never attempting to be a
/// `// === Name ===` divider, so it is not a near-miss.
fn divider_shape(trimmed: &str) -> Option<(usize, usize, &str)> {
    let left_len = trimmed.chars().take_while(|&c| c == '=').count();
    if left_len == 0 {
        return None;
    }
    let right_len = trimmed.chars().rev().take_while(|&c| c == '=').count();
    if right_len == 0 {
        return None;
    }
    let total = trimmed.chars().count();
    if left_len + right_len >= total {
        // Pure border line: no room left for a name between the two runs.
        return None;
    }
    let name = trimmed[left_len..trimmed.len() - right_len].trim();
    if name.is_empty() {
        return None;
    }
    Some((left_len, right_len, name))
}

fn comment_kind_label(kind: CommentKind) -> &'static str {
    match kind {
        CommentKind::Line => "a `//` line comment",
        CommentKind::Block => "a `/* */` block comment",
        CommentKind::Doc => "a `///` doc comment",
    }
}

/// Shared "is this near-miss-shaped text worth a diagnostic" decision,
/// parametrized over an already-classified `(content, kind, span)` so the
/// actual accept/reject logic and message wording live in exactly one
/// place, called from [`scan_source`] once per classified comment line.
fn check_comment_text(content: &str, kind: CommentKind, span: Span) -> Option<Diagnostic> {
    let trimmed = content.trim();
    let (left_len, right_len, name) = divider_shape(trimmed)?;

    // The single source of truth for "is this actually a valid divider" —
    // never reimplemented here. A pass here means it's a genuine divider;
    // nothing to warn about.
    if kind == CommentKind::Line && parse_divider_name(content).is_some() {
        return None;
    }

    let reason = if kind != CommentKind::Line {
        format!(
            "it's {} — a divider must be a plain `//` line comment",
            comment_kind_label(kind)
        )
    } else if left_len < 3 {
        format!(
            "its `=` border is only {left_len} character(s) long — dividers need a run of 3 or more"
        )
    } else if left_len != right_len {
        format!(
            "its `=` borders don't match ({left_len} on the left, {right_len} on the right) \
             — dividers need equal-length runs"
        )
    } else {
        // Defensive: `divider_shape` plus the `kind == Line` check above
        // already mirrors every rejection `parse_divider_name` can produce,
        // so this arm should be unreachable in practice — kept so the match
        // stays total instead of silently mislabeling an unforeseen case.
        "it doesn't parse as a section divider".to_string()
    };

    Some(
        Diagnostic::lint(
            format!("comment looks like a section divider but doesn't parse as one — {reason}"),
            span,
        )
        .with_hint(format!(
            "Use the canonical divider form: `// === {name} ===` — a `//` line comment with \
             matching `=` runs of 3 or more"
        )),
    )
}

#[cfg(test)]
mod tests {
    use crate::source_analysis::Severity;

    #[test]
    fn scan_source_mismatched_run_lengths_are_flagged() {
        let source = "Object subclass: Foo\n  // === Section ====\n  bar => 1\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("don't match"),
            "message: {}",
            diags[0].message
        );
        assert!(diags[0].hint.as_ref().unwrap().contains("=== Section ==="));
    }

    #[test]
    fn scan_source_too_short_runs_are_flagged() {
        let source = "Object subclass: Foo\n  // == Section ==\n  bar => 1\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
        assert!(
            diags[0].message.contains("3 or more"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn scan_source_near_miss_before_state_declaration_is_flagged() {
        let source = "Object subclass: Foo\n  // === Section ====\n  state: x = 0\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
    }

    #[test]
    fn scan_source_near_miss_before_class_method_is_flagged() {
        let source =
            "Object subclass: Foo\n  // === Section ====\n  Foo class >> create => ^Foo new\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
    }

    #[test]
    fn scan_source_diagnostic_has_lint_category() {
        let source = "Object subclass: Foo\n  // === Section ====\n  bar => 1\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].category,
            Some(crate::source_analysis::DiagnosticCategory::Lint)
        );
    }

    #[test]
    fn scan_source_locates_the_near_miss_comment_line_precisely() {
        // BT-3240/BT-3257: a diagnostic built from the AST's `Comment::span`
        // would actually point at `bar` (the *following* declaration's
        // token span — see module doc), not the comment. `scan_source` must
        // point at the comment's own line instead.
        let source = "Object subclass: Foo\n  // === Section ====\n  bar => 1\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1, "expected one near-miss: {diags:?}");
        assert_eq!(
            &source[diags[0].span.as_range()],
            "  // === Section ====\n",
            "span should cover exactly the comment's own line, not the method below it"
        );
    }

    #[test]
    fn scan_source_gives_distinct_spans_to_adjacent_near_misses() {
        // BT-3240 review: two near-miss comments leading the same member
        // must not collapse onto one identical (wrong) span.
        let source = "Object subclass: Foo\n  // == A ==\n  // == B ==\n  bar => 1\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 2, "expected two near-misses: {diags:?}");
        assert_ne!(
            diags[0].span, diags[1].span,
            "each near-miss comment should get its own span"
        );
        assert_eq!(&source[diags[0].span.as_range()], "  // == A ==\n");
        assert_eq!(&source[diags[1].span.as_range()], "  // == B ==\n");
    }

    #[test]
    fn scan_source_catches_a_doc_comment_immediately_above_a_method() {
        // BT-3240 review: without a blank line, `/// === Section ===` is
        // consumed whole into the method's plain-`String` `doc_comment`
        // field (no `Comment`, no span) — invisible to any AST walk.
        // `scan_source` doesn't depend on AST comment attachment at all, so
        // it still catches this.
        let source = "Object subclass: Foo\n  /// === Section ===\n  bar => 1\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1, "expected one near-miss: {diags:?}");
        assert!(
            diags[0].message.contains("doc comment"),
            "message: {}",
            diags[0].message
        );
        assert_eq!(&source[diags[0].span.as_range()], "  /// === Section ===\n");
    }

    #[test]
    fn scan_source_valid_divider_triggers_no_warning() {
        let source = "Object subclass: Foo\n  // === Section ===\n  bar => 1\n";
        assert!(super::scan_source(source).is_empty());
    }

    #[test]
    fn scan_source_plain_comment_with_equals_is_not_flagged() {
        let source = "Object subclass: Foo\n  // x = y\n  bar => 1\n";
        assert!(super::scan_source(source).is_empty());
    }

    #[test]
    fn scan_source_pure_border_banner_is_not_flagged() {
        let source =
            "// =========================================================================\n";
        assert!(super::scan_source(source).is_empty());
    }

    #[test]
    fn scan_source_multiline_block_comment_is_not_flagged() {
        // Multi-line `/* ... */` block comments are out of scope (see
        // `scan_source`'s doc) — a divider is inherently one line, so no
        // line of a multi-line block comment (including its opener, with no
        // `*/` on the same line) is misidentified as a near-miss.
        let source = "/*\n === Section ===\n*/\n";
        assert!(super::scan_source(source).is_empty());
    }

    #[test]
    fn scan_source_ignores_a_near_miss_shaped_line_inside_a_block_comment() {
        // BT-3240 review: a `//`-shaped line *inside* a real multi-line
        // `/* ... */` block comment is never a `//` line comment to the
        // lexer/AST at all — it's ordinary text the block comment swallows.
        // Without block-comment state tracking, the naive per-line scan
        // would misclassify it as a genuine `//` near-miss.
        let source = "/*\n// === Old Behavior ====\n*/\n";
        assert!(
            super::scan_source(source).is_empty(),
            "a commented-out-looking line inside a real block comment must not be flagged"
        );
    }

    #[test]
    fn scan_source_single_line_block_comment_near_miss_is_flagged() {
        let source = "/* === Section === */\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1, "expected one near-miss: {diags:?}");
        assert!(diags[0].message.contains("block comment"));
        assert_eq!(&source[diags[0].span.as_range()], "/* === Section === */\n");
    }

    #[test]
    fn scan_source_finds_near_miss_anywhere_not_just_class_members() {
        // Deliberately broader than the AST path (see `scan_source`'s doc):
        // a near-miss inside a method body is still worth flagging, since a
        // plain text scan has no notion of "inside a class".
        let source = "Object subclass: Foo\n  bar =>\n    // === Oops ====\n    1\n";
        let diags = super::scan_source(source);
        assert_eq!(diags.len(), 1, "expected one near-miss: {diags:?}");
    }
}
