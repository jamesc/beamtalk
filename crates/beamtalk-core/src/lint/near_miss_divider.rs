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
//! "Known limitations"). The methods that follow are then silently absorbed
//! into whichever category was already open, rather than starting the
//! section the author intended.
//!
//! This pass calls [`parse_divider_name`] directly as the single source of
//! truth for "is this a valid divider" — it never reimplements that
//! decision. It only adds the extra (purely structural) bookkeeping needed
//! to: build a useful diagnostic message explaining *why* the parse failed,
//! and to leave the unrelated pure-border `// ===...===` banner convention
//! (233+ occurrences across the stdlib/test corpus, see
//! [`parse_divider_name`]'s own doc) alone — that shape has no name at all,
//! so it was never attempting to be a `// === Name ===` divider in the first
//! place.
//!
//! Scope mirrors [`categorize_methods`]'s own: only comments leading a
//! class's `state:`/`classState:` declarations or instance/class-side
//! methods are inspected — the same member population that divider
//! recognition actually affects.

use crate::ast::{ClassDefinition, Comment, CommentKind, Module};
use crate::lint::LintPass;
use crate::source_analysis::{Diagnostic, parse_divider_name};

/// Lint pass that flags `=`-bordered, named comments that don't parse as a
/// valid `// === Name ===` section divider.
pub(crate) struct NearMissDividerPass;

impl LintPass for NearMissDividerPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        for class in &module.classes {
            check_class(class, diagnostics);
        }
    }
}

fn check_class(class: &ClassDefinition, diagnostics: &mut Vec<Diagnostic>) {
    for state in class.state.iter().chain(class.class_variables.iter()) {
        check_comments(&state.comments.leading, diagnostics);
    }
    for method in class.methods.iter().chain(class.class_methods.iter()) {
        check_comments(&method.comments.leading, diagnostics);
    }
}

fn check_comments(leading: &[Comment], diagnostics: &mut Vec<Diagnostic>) {
    for comment in leading {
        if let Some(diagnostic) = check_comment(comment) {
            diagnostics.push(diagnostic);
        }
    }
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

fn check_comment(comment: &Comment) -> Option<Diagnostic> {
    let trimmed = comment.content.trim();
    let (left_len, right_len, name) = divider_shape(trimmed)?;

    // The single source of truth for "is this actually a valid divider" —
    // never reimplemented here. A pass here means it's a genuine divider;
    // nothing to warn about.
    if comment.kind == CommentKind::Line && parse_divider_name(&comment.content).is_some() {
        return None;
    }

    let reason = if comment.kind != CommentKind::Line {
        format!(
            "it's {} — a divider must be a plain `//` line comment",
            comment_kind_label(comment.kind)
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
            comment.span,
        )
        .with_hint(format!(
            "Use the canonical divider form: `// === {name} ===` — a `//` line comment with \
             matching `=` runs of 3 or more"
        )),
    )
}

#[cfg(test)]
mod tests {
    use crate::lint::run_lint_passes;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, diags) = parse(tokens);
        assert!(
            diags.iter().all(|d| d.severity != Severity::Error),
            "fixture should parse without errors: {diags:?}"
        );
        run_lint_passes(&module)
    }

    #[test]
    fn valid_divider_triggers_no_warning() {
        let diags = lint("Object subclass: Foo\n  // === Section ===\n  bar => 1\n");
        assert!(
            diags.is_empty(),
            "valid divider should not be flagged: {diags:?}"
        );
    }

    #[test]
    fn mismatched_run_lengths_are_flagged() {
        let diags = lint("Object subclass: Foo\n  // === Section ====\n  bar => 1\n");
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
    fn too_short_runs_are_flagged() {
        let diags = lint("Object subclass: Foo\n  // == Section ==\n  bar => 1\n");
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
        assert!(
            diags[0].message.contains("3 or more"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn block_comment_near_miss_is_flagged() {
        let diags = lint("Object subclass: Foo\n  /* === Section === */\n  bar => 1\n");
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
        assert!(
            diags[0].message.contains("block comment"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn orphaned_doc_comment_near_miss_is_flagged() {
        // A blank line between the `///` block and the method keeps it from
        // being consumed as the method's own `doc_comment` (BT-2924) — it
        // stays a `Comment::doc` leading comment this pass can inspect.
        let diags = lint("Object subclass: Foo\n  /// === Section ===\n\n  bar => 1\n");
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
        assert!(
            diags[0].message.contains("doc comment"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn plain_comment_with_equals_is_not_flagged() {
        // False-positive guard: `=` appears, but not as a run at both ends.
        let diags = lint("Object subclass: Foo\n  // x = y\n  bar => 1\n");
        assert!(
            diags.is_empty(),
            "plain comment should not be flagged: {diags:?}"
        );
    }

    #[test]
    fn pure_border_banner_is_not_flagged() {
        // The pre-existing, unrelated `====...` / heading / `====...`
        // 3-line banner convention (BT-2601 leaves it alone) must not be
        // treated as a near-miss divider either.
        let diags = lint(
            "Object subclass: Foo\n  // =========================================================================\n  bar => 1\n",
        );
        assert!(
            diags.is_empty(),
            "pure border banner should not be flagged: {diags:?}"
        );
    }

    #[test]
    fn near_miss_before_state_declaration_is_flagged() {
        let diags = lint("Object subclass: Foo\n  // === Section ====\n  state: x = 0\n");
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
    }

    #[test]
    fn near_miss_before_class_method_is_flagged() {
        let diags = lint(
            "Object subclass: Foo\n  // === Section ====\n  Foo class >> create => ^Foo new\n",
        );
        assert_eq!(diags.len(), 1, "expected one lint diagnostic: {diags:?}");
    }

    #[test]
    fn lint_diagnostic_has_lint_category() {
        let diags = lint("Object subclass: Foo\n  // === Section ====\n  bar => 1\n");
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].category,
            Some(crate::source_analysis::DiagnosticCategory::Lint)
        );
    }
}
