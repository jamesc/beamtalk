// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when a local variable is reassigned inside a block whose call
//! site the compiler does NOT recognize for captured-local state-threading,
//! on a value type.
//!
//! **DDD Context:** Compilation
//!
//! **BT-3385 correction:** an earlier version of this lint fired for *any*
//! block literal passed directly to a message send, on the theory that
//! value types capture variables by value and any reassignment inside such
//! a block is silently lost. That is no longer true (and empirically was
//! not true at the time either — see BT-3385's root-cause notes) for a
//! block literal passed directly to a loop, conditional, or list-op
//! selector the compiler's codegen recognizes: ADR 0041's state-threading
//! ("known inline call sites", plus BT-1392/BT-2359's `ifTrue:`/`ifFalse:`
//! threading) packs captured-and-mutated outer locals into a `StateAcc` and
//! rebinds them in the caller's scope after the call returns — the
//! reassignment DOES escape the block. `is_state_threaded_block_arg` below
//! is the exemption list for exactly these shapes; see its doc comment for
//! the codegen cross-reference and how BT-3385 verified it (`BUnit` runtime
//! tests, `stdlib/test/bt_3385_dead_assignment_test.bt`).
//!
//! ```text
//! // Fine — do: is a recognized selector, the mutation is threaded through
//! count := 0
//! #(1, 2, 3) do: [:item | count := count + 1]
//! count  // => 3
//!
//! // Still worth a warning — blk escapes and is invoked indirectly; BT-3385
//! // found this currently raises a runtime error ("captures mutable state
//! // and must be invoked directly") rather than silently dropping the
//! // mutation, but it is still a trap worth flagging before it crashes
//! blk := [count := count + 1]
//! blk value
//! ```
//!
//! This lint warns about local reassignments inside a block literal that is
//! either stored/returned or passed to a selector the compiler does not
//! recognize for state-threading, on a value type. It does NOT warn for:
//! - Actor subclasses (where state mutations in blocks DO propagate)
//! - A block literal passed directly to a selector `is_state_threaded_block_arg`
//!   recognizes (loops, conditionals, `do:`/`collect:`/`inject:into:`-style
//!   iteration — the mutation IS threaded back to the caller)
//! - Variables defined locally within the block (not captured from outer scope)

use std::collections::HashSet;

use crate::{LintPass, hierarchy_for_lint};
use beamtalk_core::ast::{
    Block, ClassKind, Expression, ExpressionStatement, MethodDefinition, Module, StringSegment,
};
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory};

/// Lint pass that warns about dead variable assignments inside blocks on value types.
pub(crate) struct DeadBlockAssignmentPass;

impl LintPass for DeadBlockAssignmentPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        // Top-level expressions (script context — always value type semantics)
        let mut scope = LintScope::new();
        walk_expr_seq(&module.expressions, &mut scope, None, diagnostics);

        // BT-3092: see `hierarchy_for_lint` doc comment for why this is needed
        // instead of `class.class_kind`.
        let hierarchy = hierarchy_for_lint(module);

        for class in &module.classes {
            // Skip Actor subclasses (including indirect ones) — block
            // mutations DO propagate for actors.
            if hierarchy.resolve_class_kind(&class.name.name) == ClassKind::Actor {
                continue;
            }
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                check_method(method, diagnostics);
            }
        }

        // Standalone method definitions: need to determine class kind from the hierarchy
        for standalone in &module.method_definitions {
            // Resolves the full ancestor chain (BT-3092) rather than only the
            // direct superclass.
            if hierarchy.resolve_class_kind(&standalone.class_name.name) == ClassKind::Actor {
                continue;
            }
            check_method(&standalone.method, diagnostics);
        }
    }
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
    /// The full selector name (e.g., `inject:into:`, `do:`, `ifTrue:`)
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
    pattern: &beamtalk_core::ast::Pattern,
    span: beamtalk_core::source_analysis::Span,
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
    arms: &[beamtalk_core::ast::MatchArm],
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
    scope.push();
    for param in &block.parameters {
        scope.define(param.name.as_str());
    }
    if is_state_threaded_block_arg(msg_ctx) {
        // BT-3385: this block literal sits at a (selector, argument position)
        // that the compiler's Value-type / class-method state-threading
        // codegen (ADR 0041; see `is_state_threaded_block_arg`'s doc comment
        // for the exact codegen cross-reference) recognizes and threads
        // captured-and-mutated outer locals through. A reassignment here
        // DOES escape the block — it is not dead — so skip the check
        // entirely for this block's body (`None` disables it, same as
        // method/script-level code outside any block).
        walk_expr_seq(&block.body, scope, None, diagnostics);
    } else {
        walk_expr_seq(&block.body, scope, Some(&HashSet::new()), diagnostics);
    }
    scope.pop();
}

/// Returns `true` if `msg_ctx` identifies a block literal at a (selector,
/// argument position) that codegen recognizes for Value-type / class-method
/// captured-local state-threading, meaning a reassignment to an outer local
/// inside the block is threaded back out and visible after the call
/// returns — contradicting this lint's general "capture by value, mutation
/// lost" assumption.
///
/// Delegates to `beamtalk_core::state_threading_selectors::is_state_threaded_block_arg`
/// — the single canonical "which selectors thread which block-argument
/// positions" table (BT-3423 / ADR 0118 §7), shared with `beamtalk-codegen`'s
/// `get_control_flow_threaded_vars`, so the two can never silently drift
/// (CLAUDE.md's "No duplicate implementations" rule; see that table's doc
/// comment for the full selector list and index mapping).
///
/// BT-3385 confirmed empirically (`BUnit` runtime tests, see
/// `stdlib/test/bt_3385_dead_assignment_test.bt`) that mutating ANY captured
/// outer local inside these shapes persists after the call returns — not
/// just an `inject:into:` accumulator parameter, which is why this replaces
/// (rather than extends) the old accumulator-only exemption.
///
/// Deliberately NOT included, so the lint keeps firing there: a block
/// stored in a variable or passed to a user-defined (non-intrinsic) method
/// and invoked indirectly via `value`/`value:` — BT-3385 confirmed the
/// compiler does not silently drop such a mutation, but currently refuses
/// the indirect invocation outright at runtime (a separate, more confusing
/// failure mode outside this lint's scope) rather than threading it through;
/// `eachWithIndex:`/`do:separatedBy:`, whose threading is context-dependent
/// (see the shared table's doc comment) and so conservatively excluded from
/// it entirely.
fn is_state_threaded_block_arg(msg_ctx: Option<&BlockMessageContext>) -> bool {
    let Some(ctx) = msg_ctx else {
        return false;
    };
    beamtalk_core::state_threading_selectors::is_state_threaded_block_arg(
        &ctx.selector,
        ctx.arg_index,
    )
}

/// Emit a dead-assignment warning diagnostic.
fn emit_dead_assignment_warning(
    name: &str,
    span: beamtalk_core::source_analysis::Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    diagnostics.push(
        Diagnostic::lint(
            format!(
                "assignment to `{name}` inside a block relies on the block being \
                 invoked directly at this call site — storing it, returning it, or \
                 passing it to a method that calls it indirectly will not see this \
                 update (BT-3385: an escaped block that captures and mutates `{name}` \
                 currently raises a runtime error rather than silently dropping the \
                 mutation, so this is worth fixing before it crashes, not just before \
                 it surprises)"
            ),
            span,
        )
        .with_hint(
            "If this block is passed directly to a loop, conditional, or \
             iteration method (do:, collect:, inject:into:, ifTrue:, ...), \
             the reassignment IS safe and this warning does not apply there. \
             Otherwise, invoke the block inline instead of storing or passing \
             it for indirect invocation, or use `inject:into:` to accumulate \
             a value as the block's own return value."
                .to_string(),
        )
        .with_category(DiagnosticCategory::DeadAssignment),
    );
}

/// Collect all variable names bound by a pattern.
fn collect_pattern_var_names(pattern: &beamtalk_core::ast::Pattern) -> Vec<String> {
    let mut names = Vec::new();
    collect_pattern_var_names_inner(pattern, &mut names);
    names
}

fn collect_pattern_var_names_inner(pattern: &beamtalk_core::ast::Pattern, names: &mut Vec<String>) {
    use beamtalk_core::ast::Pattern;
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
        // Pattern::Type's binding is not wired into lint scope tracking yet
        // (BT-2855 — bindings/scope land with narrowing and codegen).
        Pattern::Binary { .. }
        | Pattern::Wildcard(_)
        | Pattern::Literal(_, _)
        | Pattern::Nil(_)
        | Pattern::Type { .. } => {}
    }
}

/// Define pattern-bound variable names in the lint scope.
fn define_pattern_vars_in_scope(pattern: &beamtalk_core::ast::Pattern, scope: &mut LintScope) {
    use beamtalk_core::ast::Pattern;
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
        // Pattern::Type's binding is not wired into lint scope tracking yet
        // (BT-2855 — bindings/scope land with narrowing and codegen).
        Pattern::Binary { .. }
        | Pattern::Wildcard(_)
        | Pattern::Literal(_, _)
        | Pattern::Nil(_)
        | Pattern::Type { .. } => {}
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::LintPass;
    use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};

    fn lint(src: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(src);
        let (module, _) = parse(tokens);
        let mut diags = Vec::new();
        DeadBlockAssignmentPass.check(&module, &mut diags);
        diags
    }

    // ── Basic detection (escaped block — stored, not a recognized call site) ───

    /// Assignment inside a block stored in a variable, on a top-level script
    /// (value-type context) — the block escapes its origin call site, so the
    /// compiler cannot thread the mutation back out (BT-3385: invoking such a
    /// block indirectly currently raises a runtime error rather than
    /// silently dropping the mutation, but the lint still flags it early).
    #[test]
    fn assignment_in_stored_block_warns() {
        let diags = lint("x := 1.\nblk := [x := 2]");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("`x`"),
            "Expected variable name in message, got: {}",
            diags[0].message
        );
    }

    /// Accumulation in a block passed to an unrecognized (user-defined-shaped)
    /// selector — not one of `is_state_threaded_block_arg`'s known loop/
    /// conditional/list-op shapes, so still flagged.
    #[test]
    fn accumulation_in_unrecognized_selector_block_warns() {
        let diags = lint("count := 0.\nfoo customLoop: [:item | count := count + 1]");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`count`"));
    }

    /// Multiple dead assignments in the same stored block.
    #[test]
    fn multiple_dead_assignments_warn() {
        let diags = lint("x := 0.\ny := 0.\nblk := [x := 1. y := 2]");
        assert_eq!(diags.len(), 2, "Expected 2 lints, got: {diags:?}");
    }

    // ── No false positives ────────────────────────────────────────────────────

    /// Assignment to a variable defined WITHIN the block — no warning.
    #[test]
    fn local_block_variable_no_warn() {
        let diags = lint("blk := [x := 1. x := 2]");
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
    blk := [x := 1]";
        let diags = lint(src);
        assert!(
            diags.is_empty(),
            "Expected no lints for Actor class, got: {diags:?}"
        );
    }

    /// Indirect Actor subclass (two hops: `Actor <- BaseActor <- Counter`) —
    /// block mutations still propagate for actors, no warning (BT-3092).
    ///
    /// `Counter`'s direct superclass is `BaseActor`, not `Actor` literally,
    /// so `class.class_kind` (the pre-writeback `ClassKind::from_superclass_name`
    /// placeholder) would be `ClassKind::Object` here. The lint must resolve
    /// the full ancestor chain instead to correctly skip this class.
    #[test]
    fn indirect_actor_subclass_no_warn() {
        let src = "\
Actor subclass: BaseActor
  noop => nil
BaseActor subclass: Counter
  state: count = 0
  increment =>
    x := 0
    blk := [x := 1]";
        let diags = lint(src);
        assert!(
            diags.is_empty(),
            "Expected no lints for indirect Actor subclass, got: {diags:?}"
        );
    }

    /// Object subclass — an escaped (stored) block's mutation is still
    /// flagged, since the class kind doesn't change whether an escaped
    /// block's mutation is threaded back (only its selector/position does).
    #[test]
    fn object_class_warns() {
        let src = "\
Object subclass: Foo
  bar =>
    x := 0
    blk := [x := 1]";
        let diags = lint(src);
        assert_eq!(
            diags.len(),
            1,
            "Expected 1 lint for Object class, got: {diags:?}"
        );
    }

    /// Value subclass — an escaped (stored) block's mutation is still flagged.
    #[test]
    fn value_class_warns() {
        let src = "\
Value subclass: Point
  state: x = 0
  state: y = 0
  broken =>
    z := 0
    blk := [z := 1]";
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
    blk := [x := 1]";
        let diags = lint(src);
        assert!(diags.is_empty(), "Expected no lints, got: {diags:?}");
    }

    // ── Nested blocks ─────────────────────────────────────────────────────────

    /// Dead assignment in a nested stored block.
    #[test]
    fn nested_block_dead_assignment_warns() {
        let diags = lint("x := 0.\nouter := [inner := [x := 1]]");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`x`"));
    }

    // ── Hint text ─────────────────────────────────────────────────────────────

    /// Lint diagnostic includes a hint suggesting alternatives.
    #[test]
    fn lint_includes_hint() {
        let diags = lint("x := 1.\nblk := [x := 2]");
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

    /// Standalone method (`Counter >> increment`) on an indirect Actor
    /// subclass — the standalone-method path must also resolve the full
    /// ancestor chain, not just the direct superclass (BT-3092).
    #[test]
    fn standalone_method_indirect_actor_subclass_no_warn() {
        let src = "\
Actor subclass: BaseActor
  noop => nil
BaseActor subclass: Counter
  state: count = 0
Counter >> increment =>
  x := 0
  blk := [x := 1]";
        let diags = lint(src);
        assert!(
            diags.is_empty(),
            "Expected no lints for standalone method on indirect Actor subclass, got: {diags:?}"
        );
    }

    /// Standalone method on an Object class — should warn.
    #[test]
    fn standalone_method_object_warns() {
        let src = "\
Object subclass: Foo
  value => 1
Foo >> bar =>
  x := 0
  blk := [x := 1]";
        let diags = lint(src);
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
    }

    /// Method parameter captured in a stored block — should warn.
    #[test]
    fn method_param_captured_in_block_warns() {
        let src = "\
Object subclass: Foo
  withX: x =>
    blk := [x := 99]";
        let diags = lint(src);
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`x`"));
    }

    /// Dead assignment inside a non-block message argument within a stored block.
    #[test]
    fn assignment_in_msg_arg_inside_block_warns() {
        let diags = lint("x := 0.\nblk := [foo bar: (x := 1)]");
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

    /// Destructure assignment rebinding an outer variable inside a stored
    /// block — should warn.
    #[test]
    fn destructure_rebinds_outer_var_warns() {
        let diags = lint("x := 0.\ny := 0.\nblk := [{x, y} := {1, 2}]");
        assert_eq!(
            diags.len(),
            2,
            "Expected 2 lints for destructure rebinding outer vars, got: {diags:?}"
        );
    }

    /// Destructure assignment with only local variables — no warning.
    #[test]
    fn destructure_local_vars_no_warn() {
        let diags = lint("blk := [{x, y} := {1, 2}]");
        assert!(
            diags.is_empty(),
            "Expected no lints for destructure of local vars, got: {diags:?}"
        );
    }

    // ── BT-3385: state-threaded call sites no longer warn ──────────────────────
    //
    // The compiler's Value-type / class-method state-threading (ADR 0041;
    // BT-1392/BT-2359 for conditionals) threads a captured-and-mutated outer
    // local back out for a block literal passed directly to any of these
    // selectors — confirmed at runtime by the existing `mutation_corpus_value.bt`
    // / `mutation_corpus_class_method.bt` / `counted_loop_mutation_test.bt` BUnit
    // corpora (BT-1053/BT-2308/BT-2360) and, for this issue's own reported
    // shape, by `stdlib/test/bt_3385_dead_assignment_test.bt`.

    /// The issue's exact reproduction: a `sealed typed Value subclass` class
    /// method accumulating into a `Dictionary` via a `to:do:` loop. No longer
    /// flagged as `DeadAssignment` — the reassignment does escape the block.
    #[test]
    fn bt3385_issue_repro_class_method_no_longer_warns() {
        let src = "\
sealed typed Value subclass: Foo
  class buildDict -> Dictionary(String, Integer) =>
    dict := #{}
    97 to: 122 do: [:code | dict := dict at: (String fromCodePoint: code) put: code]
    dict";
        let diags = lint(src);
        assert!(
            diags.is_empty(),
            "Expected no lints for BT-3385's do:-loop repro on a Value class method, got: {diags:?}"
        );
    }

    /// The issue's own open question ("haven't confirmed... instance methods")
    /// — same shape, instance-side. Also no longer flagged.
    #[test]
    fn bt3385_issue_repro_instance_method_no_longer_warns() {
        let src = "\
sealed typed Value subclass: Foo
  buildDict -> Dictionary(String, Integer) =>
    dict := #{}
    97 to: 122 do: [:code | dict := dict at: (String fromCodePoint: code) put: code]
    dict";
        let diags = lint(src);
        assert!(
            diags.is_empty(),
            "Expected no lints for BT-3385's do:-loop repro on a Value instance method, got: {diags:?}"
        );
    }

    /// `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` thread captured-local mutations
    /// too (BT-1392/BT-2359) — no longer flagged.
    #[test]
    fn iftrue_iffalse_no_longer_warn() {
        for src in [
            "x := 1.\ntrue ifTrue: [x := 2]",
            "x := 1.\nfalse ifFalse: [x := 2]",
            "x := 1.\ntrue ifTrue: [x := 2] ifFalse: [x := 3]",
        ] {
            let diags = lint(src);
            assert!(
                diags.is_empty(),
                "Expected no lints for {src:?}, got: {diags:?}"
            );
        }
    }

    /// `and:`/`or:` thread captured-local mutations too (BT-3402's codegen
    /// fix; BT-3423 closes the gap where this lint's own table hadn't
    /// caught up) — no longer flagged.
    #[test]
    fn and_or_no_longer_warn() {
        for src in [
            "x := 1.\nflag and: [x := 2. true]",
            "x := 1.\nflag or: [x := 2. false]",
        ] {
            let diags = lint(src);
            assert!(
                diags.is_empty(),
                "Expected no lints for {src:?}, got: {diags:?}"
            );
        }
    }

    /// The whole family of loop / list-op selectors that codegen's
    /// `block_arg_for_selector` (`crates/beamtalk-codegen/src/core_erlang/
    /// control_flow/mod.rs`) recognizes for captured-local threading — none
    /// of these should warn on a mutation of an outer local at the
    /// recognized block-argument position, whatever the accumulator's name.
    #[test]
    fn loop_and_list_op_family_no_longer_warns() {
        let cases = [
            "x := 0.\n[x < 3] whileTrue: [x := x + 1]",
            "x := 0.\n[x >= 3] whileFalse: [x := x + 1]",
            "x := 0.\n#(1, 2, 3) do: [:item | x := x + item]",
            "x := 0.\n#(1, 2, 3) collect: [:item | x := x + item. item]",
            "x := 0.\n#(1, 2, 3) select: [:item | x := x + item. true]",
            "x := 0.\n#(1, 2, 3) reject: [:item | x := x + item. false]",
            "x := 0.\n#(1, 2, 3) detect: [:item | x := x + item. true]",
            "x := 0.\n3 timesRepeat: [x := x + 1]",
            "x := 0.\n1 to: 3 do: [:i | x := x + i]",
            "x := 0.\n1 to: 3 by: 1 do: [:i | x := x + i]",
            // inject:into: — a NON-accumulator captured var, not just the
            // accumulator parameter (BT-3385 found this threads too).
            "count := 0.\n#(1, 2, 3) inject: 0 into: [:acc :item | count := count + 1. acc + item]",
        ];
        for src in cases {
            let diags = lint(src);
            assert!(
                diags.is_empty(),
                "Expected no lints for {src:?}, got: {diags:?}"
            );
        }
    }

    // ── BT-1476: @expect dead_assignment suppression ─────────────────────────

    /// `@expect dead_assignment` suppresses the dead block assignment lint.
    #[test]
    fn expect_dead_assignment_suppresses_lint() {
        let src = "x := 1.\n@expect dead_assignment\nblk := [x := 2]";
        let tokens = lex_with_eof(src);
        let (module, _) = parse(tokens);
        let mut diags = Vec::new();
        DeadBlockAssignmentPass.check(&module, &mut diags);
        // Apply @expect directives
        beamtalk_core::compilation::diagnostics_policy::apply_expect_directives(
            &module, &mut diags,
        );
        let lint_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Lint)
            .collect();
        assert!(
            lint_diags.is_empty(),
            "@expect dead_assignment should suppress lint, got: {lint_diags:?}"
        );
    }

    /// Lint has `DeadAssignment` category for `@expect` matching.
    #[test]
    fn lint_has_dead_assignment_category() {
        let diags = lint("x := 1.\nblk := [x := 2]");
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].category,
            Some(beamtalk_core::source_analysis::DiagnosticCategory::DeadAssignment),
            "Expected DeadAssignment category on lint diagnostic"
        );
    }
}
