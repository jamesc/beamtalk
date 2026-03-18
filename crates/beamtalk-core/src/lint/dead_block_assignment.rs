// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when a local variable is reassigned inside a block on a value type
//! but the new value cannot escape the block scope.
//!
//! **DDD Context:** Compilation
//!
//! In value types (Object/Value subclasses), blocks capture variables by value.
//! Reassigning a captured variable inside a block has no effect on the outer
//! scope — the mutation is silently lost. This is technically correct but a
//! massive usability trap, especially for developers coming from imperative
//! languages.
//!
//! ```text
//! // Bad — x stays 1 after the block
//! x := 1
//! true ifTrue: [x := 2]
//! x  // => 1  (expected 2!)
//!
//! // Bad — count stays 0
//! count := 0
//! #(1, 2, 3) do: [:item | count := count + 1]
//! count  // => 0  (expected 3!)
//!
//! // OK — inject:into: accumulator IS the return value
//! count := #(1, 2, 3) inject: 0 into: [:acc :item | acc + 1]
//! ```
//!
//! This lint warns about dead assignments in blocks on value types. It does NOT
//! warn for:
//! - Actor subclasses (where state mutations in blocks DO propagate)
//! - `inject:into:` accumulator parameter assignments (the accumulator is
//!   returned as the block's result)
//! - Variables defined locally within the block (not captured from outer scope)

use std::collections::HashSet;

use crate::ast::{
    Block, ClassKind, Expression, ExpressionStatement, MethodDefinition, Module, StringSegment,
};
use crate::lint::LintPass;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

/// Lint pass that warns about dead variable assignments inside blocks on value types.
pub(crate) struct DeadBlockAssignmentPass;

impl LintPass for DeadBlockAssignmentPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        // Top-level expressions (script context — always value type semantics)
        let mut scope = LintScope::new();
        walk_expr_seq(&module.expressions, &mut scope, None, diagnostics);

        for class in &module.classes {
            // Skip Actor subclasses — block mutations DO propagate for actors
            if class.class_kind == ClassKind::Actor {
                continue;
            }
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                check_method(method, diagnostics);
            }
        }

        // Standalone method definitions: need to determine class kind from module
        for standalone in &module.method_definitions {
            let class_kind = find_class_kind(module, &standalone.class_name.name);
            if class_kind == ClassKind::Actor {
                continue;
            }
            check_method(&standalone.method, diagnostics);
        }
    }
}

/// Find the `ClassKind` for a standalone method's class name.
fn find_class_kind(module: &Module, class_name: &str) -> ClassKind {
    module
        .classes
        .iter()
        .find(|c| c.name.name.as_str() == class_name)
        .map_or(ClassKind::Object, |c| c.class_kind)
}

// ── Scope tracking ────────────────────────────────────────────────────────────

/// Lightweight scope stack tracking which variables are defined at each depth.
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

    /// Returns `true` if `name` is defined in an OUTER scope (not the current one).
    fn is_defined_in_outer_scope(&self, name: &str) -> bool {
        // Check all levels except the innermost
        self.levels
            .iter()
            .rev()
            .skip(1)
            .any(|level| level.contains(name))
    }

    /// Returns `true` if `name` is defined in the current (innermost) scope.
    fn is_defined_in_current_scope(&self, name: &str) -> bool {
        self.levels.last().is_some_and(|level| level.contains(name))
    }
}

// ── Traversal helpers ─────────────────────────────────────────────────────────

/// Check a method: push a new scope, define method parameters, traverse body.
fn check_method(method: &MethodDefinition, diagnostics: &mut Vec<Diagnostic>) {
    let mut scope = LintScope::new();
    scope.push();
    for param in &method.parameters {
        scope.define(param.name.name.as_str());
    }
    walk_expr_seq(&method.body, &mut scope, None, diagnostics);
    scope.pop();
}

/// Context about the enclosing message send for a block argument.
#[derive(Debug, Clone)]
struct BlockMessageContext {
    /// The full selector name (e.g., "inject:into:", "do:", "ifTrue:")
    selector: String,
    /// The index of this block argument in the message send's argument list
    arg_index: usize,
}

/// Walk a sequence of expressions in order.
fn walk_expr_seq(
    exprs: &[ExpressionStatement],
    scope: &mut LintScope,
    safe_params: Option<&HashSet<String>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for stmt in exprs {
        walk_expr(&stmt.expression, scope, safe_params, diagnostics);
    }
}

/// Recursively walk a single expression, optionally checking for dead block assignments.
///
/// When `safe_params` is `Some`, we are inside a block and should check assignments
/// for dead captured-variable mutations. When `None`, we are at method/script level
/// and only need to track definitions + recurse into blocks.
#[allow(clippy::too_many_lines)]
fn walk_expr(
    expr: &Expression,
    scope: &mut LintScope,
    safe_params: Option<&HashSet<String>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    #[allow(clippy::enum_glob_use)]
    use Expression::*;

    match expr {
        Assignment {
            target,
            value,
            span,
            ..
        } => {
            if let Identifier(id) = target.as_ref() {
                let name = id.name.as_str();
                // Inside a block: check for dead assignment to outer-scope variable
                if let Some(safe) = safe_params {
                    if scope.is_defined_in_outer_scope(name)
                        && !scope.is_defined_in_current_scope(name)
                        && !safe.contains(name)
                    {
                        emit_dead_assignment_warning(name, *span, diagnostics);
                    }
                }
                scope.define(name);
            }
            walk_expr(value, scope, safe_params, diagnostics);
        }

        Block(block) => {
            enter_block(block, scope, None, diagnostics);
        }

        MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            walk_expr(receiver, scope, safe_params, diagnostics);
            walk_msg_args(&selector.name(), arguments, scope, safe_params, diagnostics);
        }

        Cascade {
            receiver, messages, ..
        } => {
            walk_expr(receiver, scope, safe_params, diagnostics);
            for msg in messages {
                walk_msg_args(
                    &msg.selector.name(),
                    &msg.arguments,
                    scope,
                    safe_params,
                    diagnostics,
                );
            }
        }

        FieldAccess { receiver, .. } => walk_expr(receiver, scope, safe_params, diagnostics),
        Return { value, .. } => walk_expr(value, scope, safe_params, diagnostics),
        Parenthesized { expression, .. } => walk_expr(expression, scope, safe_params, diagnostics),

        DestructureAssignment {
            pattern,
            value,
            span,
            ..
        } => {
            walk_expr(value, scope, safe_params, diagnostics);
            check_destructure_for_dead_assignments(pattern, *span, scope, safe_params, diagnostics);
            define_pattern_vars_in_scope(pattern, scope);
        }

        Match { value, arms, .. } => {
            walk_expr(value, scope, safe_params, diagnostics);
            walk_match_arms(arms, scope, safe_params, diagnostics);
        }

        MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_expr(&pair.key, scope, safe_params, diagnostics);
                walk_expr(&pair.value, scope, safe_params, diagnostics);
            }
        }

        ListLiteral { elements, tail, .. } => {
            for elem in elements {
                walk_expr(elem, scope, safe_params, diagnostics);
            }
            if let Some(t) = tail {
                walk_expr(t, scope, safe_params, diagnostics);
            }
        }

        ArrayLiteral { elements, .. } => {
            for elem in elements {
                walk_expr(elem, scope, safe_params, diagnostics);
            }
        }

        StringInterpolation { segments, .. } => {
            for seg in segments {
                if let StringSegment::Interpolation(e) = seg {
                    walk_expr(e, scope, safe_params, diagnostics);
                }
            }
        }

        Literal(..)
        | Identifier(..)
        | Super(..)
        | Error { .. }
        | ClassReference { .. }
        | Primitive { .. }
        | ExpectDirective { .. }
        | Spread { .. } => {}
    }
}

/// Check destructure pattern names for dead assignments before defining them.
fn check_destructure_for_dead_assignments(
    pattern: &crate::ast::Pattern,
    span: crate::source_analysis::Span,
    scope: &LintScope,
    safe_params: Option<&HashSet<String>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(safe) = safe_params {
        for name in collect_pattern_var_names(pattern) {
            if scope.is_defined_in_outer_scope(&name)
                && !scope.is_defined_in_current_scope(&name)
                && !safe.contains(&name)
            {
                emit_dead_assignment_warning(&name, span, diagnostics);
            }
        }
    }
}

/// Walk match arms, scoping pattern-bound variables to each arm.
fn walk_match_arms(
    arms: &[crate::ast::MatchArm],
    scope: &mut LintScope,
    safe_params: Option<&HashSet<String>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for arm in arms {
        // Pattern-bound variables are local to the arm — push a scope
        // so they don't trigger false positives for outer-scope names.
        scope.push();
        define_pattern_vars_in_scope(&arm.pattern, scope);
        if let Some(guard) = &arm.guard {
            walk_expr(guard, scope, safe_params, diagnostics);
        }
        walk_expr(&arm.body, scope, safe_params, diagnostics);
        scope.pop();
    }
}

/// Walk message arguments, entering blocks with appropriate context.
///
/// `safe_params` is forwarded to non-block arguments so that assignments inside
/// parenthesised expressions (e.g. `foo bar: (x := 1)`) are still checked when
/// the message send itself is inside a block.
fn walk_msg_args(
    selector: &str,
    arguments: &[Expression],
    scope: &mut LintScope,
    safe_params: Option<&HashSet<String>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (i, arg) in arguments.iter().enumerate() {
        if let Expression::Block(block) = arg {
            let ctx = BlockMessageContext {
                selector: selector.to_string(),
                arg_index: i,
            };
            enter_block(block, scope, Some(&ctx), diagnostics);
        } else {
            walk_expr(arg, scope, safe_params, diagnostics);
        }
    }
}

/// Enter a block: push scope, define params, walk body with dead-assignment checking.
fn enter_block(
    block: &Block,
    scope: &mut LintScope,
    msg_ctx: Option<&BlockMessageContext>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let safe_params = accumulator_params(block, msg_ctx);
    scope.push();
    for param in &block.parameters {
        scope.define(param.name.as_str());
    }
    walk_expr_seq(&block.body, scope, Some(&safe_params), diagnostics);
    scope.pop();
}

/// Returns the set of block parameter names that are "accumulators" and safe to
/// reassign (their new value escapes the block as the return value).
fn accumulator_params(block: &Block, msg_ctx: Option<&BlockMessageContext>) -> HashSet<String> {
    let mut safe = HashSet::new();
    if let Some(ctx) = msg_ctx {
        // inject:into: — the first block parameter is the accumulator
        if ctx.selector == "inject:into:" && ctx.arg_index == 1 && !block.parameters.is_empty() {
            safe.insert(block.parameters[0].name.to_string());
        }
    }
    safe
}

/// Emit a dead-assignment warning diagnostic.
fn emit_dead_assignment_warning(
    name: &str,
    span: crate::source_analysis::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    diagnostics.push(
        Diagnostic::lint(
            format!(
                "assignment to `{name}` inside a block has no effect — \
                 value types capture variables by value"
            ),
            span,
        )
        .with_hint(
            "Use `inject:into:` to accumulate values, or \
             `ifTrue:ifFalse:` to return alternative values. \
             See: block-scoped variable semantics."
                .to_string(),
        )
        .with_category(DiagnosticCategory::DeadAssignment),
    );
}

/// Collect all variable names bound by a pattern.
fn collect_pattern_var_names(pattern: &crate::ast::Pattern) -> Vec<String> {
    let mut names = Vec::new();
    collect_pattern_var_names_inner(pattern, &mut names);
    names
}

fn collect_pattern_var_names_inner(pattern: &crate::ast::Pattern, names: &mut Vec<String>) {
    use crate::ast::Pattern;
    match pattern {
        Pattern::Variable(id) => names.push(id.name.to_string()),
        Pattern::Tuple { elements, .. } => {
            for elem in elements {
                collect_pattern_var_names_inner(elem, names);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for elem in elements {
                collect_pattern_var_names_inner(elem, names);
            }
            if let Some(rest_pat) = rest {
                collect_pattern_var_names_inner(rest_pat, names);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for elem in elements {
                collect_pattern_var_names_inner(elem, names);
            }
            if let Some(t) = tail {
                collect_pattern_var_names_inner(t, names);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                collect_pattern_var_names_inner(&pair.value, names);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_, binding) in keywords {
                collect_pattern_var_names_inner(binding, names);
            }
        }
        Pattern::Binary { .. } | Pattern::Wildcard(_) | Pattern::Literal(_, _) => {}
    }
}

/// Define pattern-bound variable names in the lint scope.
fn define_pattern_vars_in_scope(pattern: &crate::ast::Pattern, scope: &mut LintScope) {
    use crate::ast::Pattern;
    match pattern {
        Pattern::Variable(id) => scope.define(id.name.as_str()),
        Pattern::Tuple { elements, .. } => {
            for elem in elements {
                define_pattern_vars_in_scope(elem, scope);
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for elem in elements {
                define_pattern_vars_in_scope(elem, scope);
            }
            if let Some(rest_pat) = rest {
                define_pattern_vars_in_scope(rest_pat, scope);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for elem in elements {
                define_pattern_vars_in_scope(elem, scope);
            }
            if let Some(t) = tail {
                define_pattern_vars_in_scope(t, scope);
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                define_pattern_vars_in_scope(&pair.value, scope);
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_, binding) in keywords {
                define_pattern_vars_in_scope(binding, scope);
            }
        }
        Pattern::Binary { .. } | Pattern::Wildcard(_) | Pattern::Literal(_, _) => {}
    }
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
        DeadBlockAssignmentPass.check(&module, &mut diags);
        diags
    }

    // ── Basic detection ───────────────────────────────────────────────────────

    /// Assignment inside ifTrue: block on a top-level script (value-type context).
    #[test]
    fn assignment_in_iftrue_block_warns() {
        let diags = lint("x := 1.\ntrue ifTrue: [x := 2]");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("`x`"),
            "Expected variable name in message, got: {}",
            diags[0].message
        );
        assert!(
            diags[0].message.contains("no effect"),
            "Expected 'no effect' in message, got: {}",
            diags[0].message
        );
    }

    /// Accumulation in a do: block — classic broken pattern.
    #[test]
    fn accumulation_in_do_block_warns() {
        let diags = lint("count := 0.\n#(1, 2, 3) do: [:item | count := count + 1]");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`count`"));
    }

    /// Multiple dead assignments in the same block.
    #[test]
    fn multiple_dead_assignments_warn() {
        let diags = lint("x := 0.\ny := 0.\ntrue ifTrue: [x := 1. y := 2]");
        assert_eq!(diags.len(), 2, "Expected 2 lints, got: {diags:?}");
    }

    // ── No false positives ────────────────────────────────────────────────────

    /// Assignment to a variable defined WITHIN the block — no warning.
    #[test]
    fn local_block_variable_no_warn() {
        let diags = lint("true ifTrue: [x := 1. x := 2]");
        assert!(diags.is_empty(), "Expected no lints, got: {diags:?}");
    }

    /// inject:into: accumulator parameter — no warning.
    #[test]
    fn inject_into_accumulator_no_warn() {
        let diags = lint("#(1, 2, 3) inject: 0 into: [:acc :item | acc := acc + item]");
        assert!(
            diags.is_empty(),
            "Expected no lints for inject:into: accumulator, got: {diags:?}"
        );
    }

    /// Actor subclass — block mutations propagate, no warning.
    #[test]
    fn actor_class_no_warn() {
        let src = "\
Actor subclass: Counter
  state: count = 0
  increment =>
    x := 0
    true ifTrue: [x := 1]";
        let diags = lint(src);
        assert!(
            diags.is_empty(),
            "Expected no lints for Actor class, got: {diags:?}"
        );
    }

    /// Object subclass — block mutations DON'T propagate, should warn.
    #[test]
    fn object_class_warns() {
        let src = "\
Object subclass: Foo
  bar =>
    x := 0
    true ifTrue: [x := 1]";
        let diags = lint(src);
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 lint for Object class, got: {diags:?}"
        );
    }

    /// Value subclass — block mutations DON'T propagate, should warn.
    #[test]
    fn value_class_warns() {
        let src = "\
Value subclass: Point
  state: x = 0
  state: y = 0
  broken =>
    z := 0
    true ifTrue: [z := 1]";
        let diags = lint(src);
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 lint for Value class, got: {diags:?}"
        );
    }

    /// No outer variable — assignment is purely local to the block.
    #[test]
    fn no_outer_variable_no_warn() {
        let src = "\
Object subclass: Foo
  bar =>
    true ifTrue: [x := 1]";
        let diags = lint(src);
        assert!(diags.is_empty(), "Expected no lints, got: {diags:?}");
    }

    // ── Nested blocks ─────────────────────────────────────────────────────────

    /// Dead assignment in a nested block.
    #[test]
    fn nested_block_dead_assignment_warns() {
        let diags = lint("x := 0.\ntrue ifTrue: [true ifTrue: [x := 1]]");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`x`"));
    }

    // ── Hint text ─────────────────────────────────────────────────────────────

    /// Lint diagnostic includes a hint suggesting alternatives.
    #[test]
    fn lint_includes_hint() {
        let diags = lint("x := 1.\ntrue ifTrue: [x := 2]");
        assert!(
            diags[0].hint.is_some(),
            "Expected a hint on lint diagnostic"
        );
        let hint = diags[0].hint.as_ref().unwrap();
        assert!(
            hint.contains("inject:into:"),
            "Expected hint to mention inject:into:, got: {hint}"
        );
    }

    // ── Standalone method definitions ─────────────────────────────────────────

    /// Standalone method on an Object class — should warn.
    #[test]
    fn standalone_method_object_warns() {
        let src = "\
Object subclass: Foo
  value => 1
Foo >> bar =>
  x := 0
  true ifTrue: [x := 1]";
        let diags = lint(src);
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
    }

    /// collect: block that mutates an outer variable — should warn.
    #[test]
    fn collect_block_dead_assignment_warns() {
        let diags =
            lint("total := 0.\n#(1, 2, 3) collect: [:item | total := total + item. item * 2]");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`total`"));
    }

    /// inject:into: — mutating a NON-accumulator captured variable should warn.
    #[test]
    fn inject_into_non_accumulator_mutation_warns() {
        let diags = lint(
            "count := 0.\n#(1, 2, 3) inject: 0 into: [:acc :item | count := count + 1. acc + item]",
        );
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 lint for non-accumulator mutation in inject:into:, got: {diags:?}"
        );
        assert!(diags[0].message.contains("`count`"));
    }

    /// Method parameter captured in block — should warn.
    #[test]
    fn method_param_captured_in_block_warns() {
        let src = "\
Object subclass: Foo
  withX: x =>
    true ifTrue: [x := 99]";
        let diags = lint(src);
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`x`"));
    }

    /// Dead assignment inside a non-block message argument within a block.
    #[test]
    fn assignment_in_msg_arg_inside_block_warns() {
        let diags = lint("x := 0.\ntrue ifTrue: [foo bar: (x := 1)]");
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 lint for assignment in msg arg inside block, got: {diags:?}"
        );
        assert!(diags[0].message.contains("`x`"));
    }

    /// Match arm pattern variables are scoped to the arm — verify parsing and
    /// that the lint correctly tracks them. Currently the parser doesn't allow
    /// assignments in match arm bodies, but this test ensures the scope is
    /// correct for future parser changes.
    #[test]
    fn match_arm_pattern_variable_scoped() {
        // Verify match: parses correctly with a simple arm
        let src = "y := 0.\n1 match: [y -> y + 1]";
        let tokens = lex_with_eof(src);
        let (module, _) = parse(tokens);
        assert!(
            matches!(&module.expressions[1].expression, Expression::Match { .. }),
            "Expected Match expression, got: {:?}",
            module.expressions[1].expression
        );
        let diags = lint(src);
        assert!(
            diags.is_empty(),
            "Expected no lints for match arm pattern variable, got: {diags:?}"
        );
    }

    /// Destructure assignment rebinding an outer variable inside a block — should warn.
    #[test]
    fn destructure_rebinds_outer_var_warns() {
        let diags = lint("x := 0.\ny := 0.\ntrue ifTrue: [{x, y} := {1, 2}]");
        assert_eq!(
            diags.len(),
            2,
            "Expected 2 lints for destructure rebinding outer vars, got: {diags:?}"
        );
    }

    /// Destructure assignment with only local variables — no warning.
    #[test]
    fn destructure_local_vars_no_warn() {
        let diags = lint("true ifTrue: [{x, y} := {1, 2}]");
        assert!(
            diags.is_empty(),
            "Expected no lints for destructure of local vars, got: {diags:?}"
        );
    }

    // ── BT-1476: @expect dead_assignment suppression ─────────────────────────

    /// `@expect dead_assignment` suppresses the dead block assignment lint.
    #[test]
    fn expect_dead_assignment_suppresses_lint() {
        let src = "x := 1.\n@expect dead_assignment\ntrue ifTrue: [x := 2]";
        let tokens = lex_with_eof(src);
        let (module, _) = parse(tokens);
        let mut diags = Vec::new();
        DeadBlockAssignmentPass.check(&module, &mut diags);
        // Apply @expect directives
        crate::queries::diagnostic_provider::apply_expect_directives(&module, &mut diags);
        let lint_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == crate::source_analysis::Severity::Lint)
            .collect();
        assert!(
            lint_diags.is_empty(),
            "@expect dead_assignment should suppress lint, got: {lint_diags:?}"
        );
    }

    /// Lint has `DeadAssignment` category for `@expect` matching.
    #[test]
    fn lint_has_dead_assignment_category() {
        let diags = lint("x := 1.\ntrue ifTrue: [x := 2]");
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].category,
            Some(crate::source_analysis::DiagnosticCategory::DeadAssignment),
            "Expected DeadAssignment category on lint diagnostic"
        );
    }
}
