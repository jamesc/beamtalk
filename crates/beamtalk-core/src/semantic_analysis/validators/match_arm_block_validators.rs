// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! A `match:` arm whose body is a bare block literal (BT-4028).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A `match:` arm body is an expression, so `pattern -> [ ... ]` answers the
//! Block as a value and never runs its statements. Found while fixing BT-4024:
//! the main `handleInfo:` examples in the language-features doc, ADR 0065,
//! ADR 0134 and `Server`'s doc comment all used this form.
//!
//! The compiler used to catch this only by accident, and only in actors: a
//! returned block that writes a field or sends to `self` fails with a
//! state-threading error that does not name the real mistake, while a block
//! that does neither compiled silently and never ran. This check warns at
//! build time and names the fix.
//!
//! **What counts.** Every `Expression::Match` in the module (via
//! `walk_module`, so script/REPL top level, methods, standalone defs and
//! nested matches; `matchExhaustive:` is the same AST shape, so one check
//! covers both). Each arm body is stripped of `Parenthesized`/`Return`
//! wrappers (the same strip-helper shape as the sibling
//! `handleinfo-no-catch-all` check, BT-3959); then:
//!
//! - if **every** arm body is a block literal, the whole match is skipped
//!   (an intentional dispatch table / block-valued match, e.g. a lone
//!   `_ -> [0]`);
//! - otherwise each arm whose stripped body is a **zero-parameter** block
//!   literal warns once. Blocks with parameters never warn (a block value
//!   that takes arguments is read as intentional — a documented limitation).
//!
//! **Tier 2 interaction (decided: no change).** When both apply, this warning
//! and the existing `Tier2BlockNoReturnChannel` error coexist: the warning
//! names the real mistake, the error still blocks the build.
//!
//! **Category.** [`DiagnosticCategory::MatchArmBlock`], configurable under
//! `[diagnostics]` as `match-arm-block`, suppressible with
//! `@expect match_arm_block`.

use crate::ast::{Block, Expression, MatchArm, Module};
use crate::ast_walker::walk_module;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

/// Runs the BT-4028 `match-arm-block` check over every `match:` in `module`.
pub(crate) fn check_match_arm_block(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        if let Expression::Match { arms, .. } = expr {
            check_match(arms, diagnostics);
        }
    });
}

/// Warns once per zero-parameter block-literal arm body, unless every arm of
/// the match is a block literal (an intentional block-valued match).
fn check_match(arms: &[MatchArm], diagnostics: &mut Vec<Diagnostic>) {
    let stripped: Vec<&Expression> = arms
        .iter()
        .map(|arm| strip_to_arm_value(&arm.body))
        .collect();
    if stripped
        .iter()
        .all(|body| matches!(body, Expression::Block(_)))
    {
        return;
    }
    for body in stripped {
        if let Expression::Block(block) = body
            && block.parameters.is_empty()
        {
            diagnostics.push(build_diagnostic(block));
        }
    }
}

/// Unwraps any number of `Parenthesized` wrappers and `^` returns, so
/// `#tick -> ([ ... ])` and `#tick -> ^[ ... ]` warn just like the bare form.
fn strip_to_arm_value(expr: &Expression) -> &Expression {
    match expr {
        Expression::Parenthesized { expression, .. }
        | Expression::Return {
            value: expression, ..
        } => strip_to_arm_value(expression),
        other => other,
    }
}

fn build_diagnostic(block: &Block) -> Diagnostic {
    Diagnostic::warning(
        "this `match:` arm answers a Block without running it; the block's statements never execute",
        block.span,
    )
    .with_hint(
        "call a method from the arm (`#tick -> self tick`), or evaluate the block in place \
         (`#tick -> [ ... ] value`)",
    )
    .with_category(DiagnosticCategory::MatchArmBlock)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    fn findings(src: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_arm_block(&module, &mut diagnostics);
        diagnostics
    }

    // ── fires ────────────────────────────────────────────────────────────

    #[test]
    fn issue_repro_in_server_handle_info_warns() {
        let src = "Server subclass: Probe\n  handleInfo: msg =>\n    msg match: [\n      #tick -> [Erlang erlang display: msg];\n      _ -> nil\n    ]";
        let diags = findings(src);
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        let d = &diags[0];
        assert_eq!(d.category, Some(DiagnosticCategory::MatchArmBlock));
        assert_eq!(d.severity, crate::source_analysis::Severity::Warning);
        assert_eq!(
            d.message.as_str(),
            "this `match:` arm answers a Block without running it; the block's statements never \
             execute"
        );
        assert!(
            d.hint.as_deref().unwrap_or("").contains("[ ... ] value"),
            "hint: {:?}",
            d.hint
        );
        assert_eq!(src.as_bytes()[d.span.start() as usize], b'[');
    }

    #[test]
    fn object_class_method_warns() {
        let src = "Object subclass: Shapes\n  class describe: x => x match: [#circle -> [self draw]; _ -> nil]";
        assert_eq!(findings(src).len(), 1);
    }

    #[test]
    fn value_method_warns() {
        let src = "Value subclass: Point\n  field: x\n  describe => self.x match: [0 -> [self origin]; _ -> nil]";
        assert_eq!(findings(src).len(), 1);
    }

    #[test]
    fn standalone_definition_warns() {
        let src = "Object subclass: Shapes\n\nShapes >> describe: x => x match: [#circle -> [x]; _ -> nil]";
        assert_eq!(findings(src).len(), 1);
    }

    #[test]
    fn script_top_level_warns() {
        let src = "x := 1\nx match: [1 -> [2 + 3]; _ -> 0]";
        assert_eq!(findings(src).len(), 1);
    }

    #[test]
    fn nested_match_warns() {
        let src = "Object subclass: Shapes\n  describe: x => x match: [1 -> (x match: [2 -> [3]; _ -> 0]); _ -> 0]";
        assert_eq!(findings(src).len(), 1);
    }

    #[test]
    fn match_exhaustive_warns() {
        let src = "Object subclass: Shapes\n  describe: x => x matchExhaustive: [1 -> [2]; _ -> 0]";
        assert_eq!(findings(src).len(), 1);
    }

    #[test]
    fn parenthesized_block_body_warns() {
        let src = "Object subclass: Shapes\n  describe: x => x match: [1 -> ([2 + 3]); _ -> 0]";
        assert_eq!(findings(src).len(), 1);
    }

    #[test]
    fn multiple_block_arms_warn_once_each() {
        let src = "Object subclass: Shapes\n  describe: x => x match: [1 -> [1]; 2 -> [2]; _ -> 0]";
        assert_eq!(findings(src).len(), 2);
    }

    // ── silent ───────────────────────────────────────────────────────────

    #[test]
    fn all_block_arms_are_silent_dispatch_table() {
        let src = "Object subclass: Shapes\n  describe: x => x match: [1 -> [2 + 3]; _ -> [0]]";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn single_block_arm_is_silent() {
        let src = "Object subclass: Shapes\n  describe: x => x match: [_ -> [0]]";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn block_with_parameters_is_silent() {
        let src = "Object subclass: Shapes\n  describe: x => x match: [1 -> [:y | y + 1]; _ -> 0]";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn evaluated_block_is_silent() {
        let src = "Object subclass: Shapes\n  describe: x => x match: [1 -> [2 + 3] value; _ -> 0]";
        assert!(findings(src).is_empty());
    }

    #[test]
    fn non_block_arms_are_silent() {
        let src = "Object subclass: Shapes\n  describe: x => x match: [1 -> self one; _ -> nil]";
        assert!(findings(src).is_empty());
    }
}
