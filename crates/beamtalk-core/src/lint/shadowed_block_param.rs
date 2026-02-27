// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when a block parameter name shadows a variable in an enclosing scope.
//!
//! **DDD Context:** Compilation
//!
//! When a block parameter has the same name as a variable already defined in
//! an outer scope (method parameter, local variable, or outer block parameter),
//! the inner binding silently shadows the outer one. This is often unintentional
//! and can lead to confusing behaviour inside the block.
//!
//! ```text
//! // Bad — block parameter shadows outer variable
//! x := 10.
//! [:x | x + 1] value        // Warning: block parameter x shadows outer x
//!
//! // OK — no shadowing
//! [:x | x + 1] value        // x is not defined in any outer scope
//!
//! // Method parameter shadowing
//! withX: x =>
//!   items do: [:x | x + 1]  // Warning: block parameter x shadows method param x
//! ```

use std::collections::HashSet;

use crate::ast::{Block, Expression, MethodDefinition, Module, StringSegment};
use crate::lint::LintPass;
use crate::source_analysis::Diagnostic;

/// Lint pass that warns when a block parameter name shadows an outer variable.
pub(crate) struct ShadowedBlockParamPass;

impl LintPass for ShadowedBlockParamPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        let mut scope = LintScope::new();

        // Top-level expressions (module scope, depth 0)
        check_expr_seq(&module.expressions, &mut scope, diagnostics);

        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                check_method(method, &mut scope, diagnostics);
            }
        }
        for standalone in &module.method_definitions {
            check_method(&standalone.method, &mut scope, diagnostics);
        }
    }
}

// ── Scope tracking ────────────────────────────────────────────────────────────

/// Lightweight scope stack used only for shadowing detection.
///
/// Each level holds the set of variable names defined at that depth.
struct LintScope {
    levels: Vec<HashSet<String>>,
}

impl LintScope {
    fn new() -> Self {
        Self {
            levels: vec![HashSet::new()],
        }
    }

    fn push(&mut self) {
        self.levels.push(HashSet::new());
    }

    fn pop(&mut self) {
        debug_assert!(
            self.levels.len() > 1,
            "LintScope::pop called with only root scope"
        );
        if self.levels.len() > 1 {
            self.levels.pop();
        }
    }

    /// Define `name` in the current (innermost) scope level.
    fn define(&mut self, name: &str) {
        if let Some(top) = self.levels.last_mut() {
            top.insert(name.to_string());
        }
    }

    /// Returns `true` if `name` is defined in any currently visible scope level.
    ///
    /// This is called *before* pushing the block's own scope, so every level
    /// in the stack is an outer scope relative to the block being entered.
    fn is_defined_in_any_scope(&self, name: &str) -> bool {
        self.levels.iter().any(|level| level.contains(name))
    }
}

// ── Traversal helpers ─────────────────────────────────────────────────────────

/// Check a method: push a new scope, define method parameters, traverse body.
fn check_method(
    method: &MethodDefinition,
    scope: &mut LintScope,
    diagnostics: &mut Vec<Diagnostic>,
) {
    scope.push();
    for param in &method.parameters {
        scope.define(param.name.name.as_str());
    }
    check_expr_seq(&method.body, scope, diagnostics);
    scope.pop();
}

/// Walk a sequence of expressions in order, collecting assignment targets into
/// the current scope as they are encountered (so later blocks can see them).
fn check_expr_seq(exprs: &[Expression], scope: &mut LintScope, diagnostics: &mut Vec<Diagnostic>) {
    for expr in exprs {
        check_expr(expr, scope, diagnostics);
    }
}

/// Recursively check a single expression.
fn check_expr(expr: &Expression, scope: &mut LintScope, diagnostics: &mut Vec<Diagnostic>) {
    #[allow(clippy::enum_glob_use)]
    use Expression::*;

    match expr {
        // ── Assignment: define the target before recursing into value ────────
        Assignment { target, value, .. } => {
            if let Identifier(id) = target.as_ref() {
                scope.define(id.name.as_str());
            }
            check_expr(value, scope, diagnostics);
        }

        // ── Block: check params for shadowing, then recurse with new scope ──
        Block(block) => {
            check_block(block, scope, diagnostics);
        }

        // ── Structural forms — recurse into children ─────────────────────────
        MessageSend {
            receiver,
            arguments,
            ..
        } => {
            check_expr(receiver, scope, diagnostics);
            for arg in arguments {
                check_expr(arg, scope, diagnostics);
            }
        }

        Cascade {
            receiver, messages, ..
        } => {
            check_expr(receiver, scope, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    check_expr(arg, scope, diagnostics);
                }
            }
        }

        FieldAccess { receiver, .. } => {
            check_expr(receiver, scope, diagnostics);
        }

        Return { value, .. } => {
            check_expr(value, scope, diagnostics);
        }

        Parenthesized { expression, .. } => {
            check_expr(expression, scope, diagnostics);
        }

        Match { value, arms, .. } => {
            check_expr(value, scope, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    check_expr(guard, scope, diagnostics);
                }
                check_expr(&arm.body, scope, diagnostics);
            }
        }

        MapLiteral { pairs, .. } => {
            for pair in pairs {
                check_expr(&pair.key, scope, diagnostics);
                check_expr(&pair.value, scope, diagnostics);
            }
        }

        ListLiteral { elements, tail, .. } => {
            for elem in elements {
                check_expr(elem, scope, diagnostics);
            }
            if let Some(t) = tail {
                check_expr(t, scope, diagnostics);
            }
        }

        ArrayLiteral { elements, .. } => {
            for elem in elements {
                check_expr(elem, scope, diagnostics);
            }
        }

        StringInterpolation { segments, .. } => {
            for seg in segments {
                if let StringSegment::Interpolation(e) = seg {
                    check_expr(e, scope, diagnostics);
                }
            }
        }

        // ── Leaf nodes — no children to recurse into ─────────────────────────
        Literal(..)
        | Identifier(..)
        | Super(..)
        | Error { .. }
        | ClassReference { .. }
        | Primitive { .. }
        | ExpectDirective { .. } => {}
    }
}

/// Check a `Block` node: warn on any parameter that shadows an outer variable,
/// then recurse into the body with the block parameters in scope.
fn check_block(block: &Block, scope: &mut LintScope, diagnostics: &mut Vec<Diagnostic>) {
    // Check BEFORE pushing the block scope — we want to see outer names only.
    for param in &block.parameters {
        let name = param.name.as_str();
        if scope.is_defined_in_any_scope(name) {
            let mut diag = Diagnostic::lint(
                format!("block parameter `{name}` shadows an outer variable"),
                param.span,
            );
            diag.hint =
                Some(format!("Rename `{name}` to avoid shadowing the outer binding.").into());
            diagnostics.push(diag);
        }
    }

    // Now enter the block scope and define block parameters.
    scope.push();
    for param in &block.parameters {
        scope.define(param.name.as_str());
    }
    check_expr_seq(&block.body, scope, diagnostics);
    scope.pop();
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lint::LintPass;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn lint(src: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(src);
        let (module, _) = parse(tokens);
        let mut diags = Vec::new();
        ShadowedBlockParamPass.check(&module, &mut diags);
        diags
    }

    // ── Basic shadowing cases ─────────────────────────────────────────────────

    /// Block parameter shadows a top-level assignment.
    #[test]
    fn block_param_shadows_outer_local() {
        let diags = lint("x := 10.\n[:x | x + 1] value");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("shadows"),
            "Expected 'shadows' in message, got: {}",
            diags[0].message
        );
        assert!(
            diags[0].message.contains('`'),
            "Expected backtick-quoted name, got: {}",
            diags[0].message
        );
    }

    /// Block parameter shadows a method parameter.
    #[test]
    fn block_param_shadows_method_param() {
        let src = "Object subclass: Foo\n  withX: x => items do: [:x | x + 1]";
        let diags = lint(src);
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(diags[0].message.contains("`x`"));
    }

    // ── Non-shadowing cases (must NOT warn) ───────────────────────────────────

    /// Block parameter not in any outer scope — no warning.
    #[test]
    fn block_param_no_outer_binding_no_warn() {
        let diags = lint("[:x | x + 1] value");
        assert!(diags.is_empty(), "Expected no lints, got: {diags:?}");
    }

    /// Different name in outer scope — no warning.
    #[test]
    fn different_outer_name_no_warn() {
        let diags = lint("y := 10.\n[:x | x + 1] value");
        assert!(diags.is_empty(), "Expected no lints, got: {diags:?}");
    }

    /// Block with no parameters — no warning.
    #[test]
    fn block_without_params_no_warn() {
        let diags = lint("x := 10.\n[x + 1] value");
        assert!(diags.is_empty(), "Expected no lints, got: {diags:?}");
    }

    // ── Multiple parameters ───────────────────────────────────────────────────

    /// Two block parameters, only one shadows — only one warning.
    #[test]
    fn only_shadowing_param_warns() {
        let diags = lint("x := 10.\n[:x :y | x + y] value");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`x`"));
    }

    /// Both block parameters shadow outer variables — two warnings.
    #[test]
    fn both_params_shadow_outer_two_warns() {
        let diags = lint("x := 1.\ny := 2.\n[:x :y | x + y] value");
        assert_eq!(diags.len(), 2, "Expected 2 lints, got: {diags:?}");
    }

    // ── Nested blocks ─────────────────────────────────────────────────────────

    /// Inner block parameter shadows outer block parameter.
    #[test]
    fn inner_block_param_shadows_outer_block_param() {
        let diags = lint("[:x | [:x | x + 1] value] value");
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 lint for inner block, got: {diags:?}"
        );
        assert!(diags[0].message.contains("`x`"));
    }

    /// Inner block parameter shadows method parameter via outer block.
    #[test]
    fn inner_block_shadows_method_param_through_outer_block() {
        let src = "Object subclass: Foo\n  withX: x => items do: [:y | items2 do: [:x | x + 1]]";
        let diags = lint(src);
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 lint for innermost block, got: {diags:?}"
        );
        assert!(diags[0].message.contains("`x`"));
    }

    // ── Hint text ─────────────────────────────────────────────────────────────

    /// Lint diagnostic includes a hint for renaming.
    #[test]
    fn lint_includes_rename_hint() {
        let diags = lint("x := 10.\n[:x | x + 1] value");
        assert!(
            diags[0].hint.is_some(),
            "Expected a hint on lint diagnostic"
        );
        let hint = diags[0].hint.as_ref().unwrap();
        assert!(
            hint.contains("Rename"),
            "Expected 'Rename' in hint, got: {hint}"
        );
    }

    // ── Method parameter in standalone method ─────────────────────────────────

    /// Standalone method definition with shadowing block.
    #[test]
    fn standalone_method_block_shadows_param() {
        let src = "Object subclass: Foo\n  value => 1\nFoo >> withX: x => items do: [:x | x + 1]";
        let diags = lint(src);
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`x`"));
    }
}
